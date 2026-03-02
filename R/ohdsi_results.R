# Module: OHDSI Results Consumer
# Generic adapter for reading pre-computed results from OHDSI tools
# (DQD, CohortDiagnostics, CohortIncidence, Characterization).
# Same pattern as Achilles: we consume existing result tables, not run the tools.

# --- Tool Registry ---

#' Static registry of known OHDSI result tools
#'
#' Each entry declares the tool's result table names, count columns that need
#' disclosure control, and sensitive columns that must never be returned.
#'
#' @return Named list keyed by tool_id
#' @keywords internal
.ohdsi_tool_registry <- function() {
  list(
    dqd = list(
      tool_name = "Data Quality Dashboard",
      table_names = c("dqdashboard_results"),
      prefix_patterns = character(0),
      count_columns = c("num_violated_rows", "num_denominator_rows"),
      sensitive_columns = c("query_text")
    ),

    cohort_diagnostics = list(
      tool_name = "CohortDiagnostics",
      table_names = c("cohort_count", "incidence_rate", "index_event_breakdown",
                       "visit_context", "temporal_covariate_value",
                       "temporal_covariate_value_dist", "time_series",
                       "included_source_concept", "orphan_concept",
                       "concept_sets", "resolved_concepts"),
      prefix_patterns = c("^cd_"),
      count_columns = c("cohort_entries", "cohort_subjects", "person_count",
                         "concept_count", "concept_subjects", "subject_count",
                         "subjects", "records", "sum_value", "count_value"),
      sensitive_columns = c("json", "sql", "concept_set_sql")
    ),

    cohort_incidence = list(
      tool_name = "CohortIncidence",
      table_names = c("incidence_summary", "target_def", "outcome_def", "tar_def"),
      prefix_patterns = c("^ci_"),
      count_columns = c("persons_at_risk_pe", "persons_at_risk",
                         "person_outcomes_pe", "person_outcomes",
                         "outcomes_pe", "outcomes"),
      sensitive_columns = character(0)
    ),

    characterization = list(
      tool_name = "Characterization",
      table_names = c("c_cohort_counts", "c_covariates", "c_covariates_continuous",
                       "c_time_to_event", "c_dechallenge_rechallenge",
                       "c_analysis_ref", "c_covariate_ref", "c_settings"),
      prefix_patterns = c("^c_"),
      count_columns = c("num_persons", "sum_value", "count_value",
                         "num_events", "num_cases", "num_persons_exposed",
                         "num_dechallenge_attempt", "num_dechallenge_success",
                         "num_rechallenge_attempt", "num_rechallenge_success"),
      sensitive_columns = character(0)
    ),

    cohort_method = list(
      tool_name = "CohortMethod",
      table_names = c("cm_result", "cm_diagnostics_summary", "cm_attrition",
                       "cm_covariate_balance", "cm_follow_up_dist",
                       "cm_kaplan_meier_dist", "cm_interaction_result",
                       "cm_shared_covariate_balance"),
      prefix_patterns = c("^cm_"),
      count_columns = c("target_subjects", "comparator_subjects",
                         "target_outcomes", "comparator_outcomes",
                         "target_days", "comparator_days"),
      sensitive_columns = character(0)
    ),

    sccs = list(
      tool_name = "Self-Controlled Case Series",
      table_names = c("sccs_result", "sccs_diagnostics_summary",
                       "sccs_attrition", "sccs_covariate_result"),
      prefix_patterns = c("^sccs_"),
      count_columns = c("outcome_subjects", "outcome_events",
                         "outcome_observation_periods", "observed_days"),
      sensitive_columns = character(0)
    ),

    plp = list(
      tool_name = "Patient-Level Prediction",
      table_names = c("plp_performances", "plp_covariate_summary",
                       "plp_diagnostic_summary", "plp_calibration_summary",
                       "plp_threshold_summary", "plp_model_design",
                       "plp_attrition"),
      prefix_patterns = c("^plp_"),
      count_columns = c("population_size", "outcome_count", "test_size",
                         "train_size", "n_total"),
      sensitive_columns = character(0)
    ),

    evidence_synthesis = list(
      tool_name = "Evidence Synthesis",
      table_names = c("es_cm_result", "es_sccs_result",
                       "es_cm_diagnostics_summary", "es_sccs_diagnostics_summary"),
      prefix_patterns = c("^es_"),
      count_columns = c("n_databases"),
      sensitive_columns = character(0)
    )
  )
}

#' Map a table name to its tool_id
#'
#' Checks exact name match first, then prefix patterns.
#' @param table_name Character; the table name to look up
#' @return Character tool_id, or NULL if no match
#' @keywords internal
.ohdsi_table_to_tool <- function(table_name) {
  registry <- .ohdsi_tool_registry()
  tbl_lower <- tolower(table_name)

  for (tid in names(registry)) {
    tool <- registry[[tid]]
    # Exact match
    if (tbl_lower %in% tool$table_names) return(tid)
    # Strip prefix and check
    for (pat in tool$prefix_patterns) {
      stripped <- sub(pat, "", tbl_lower)
      if (stripped != tbl_lower && stripped %in% tool$table_names) return(tid)
    }
  }
  NULL
}

# --- Discovery ---

#' Find OHDSI result tables in the database
#'
#' Scans results_schema (or cdm_schema for SQLite) for tables matching
#' known OHDSI tool signatures from the registry.
#'
#' @param handle CDM handle
#' @return data.frame with table_name, tool_id, tool_name, qualified_name, n_rows
#' @keywords internal
.ohdsiFindResultTables <- function(handle) {
  empty <- data.frame(
    table_name = character(0), tool_id = character(0),
    tool_name = character(0), qualified_name = character(0),
    n_rows = integer(0), stringsAsFactors = FALSE
  )

  bp <- .buildBlueprint(handle)
  db_tables <- bp$tables$table_name[bp$tables$present_in_db]
  db_tables_lower <- tolower(db_tables)

  registry <- .ohdsi_tool_registry()
  rows <- list()

  for (tid in names(registry)) {
    tool <- registry[[tid]]

    for (known_name in tool$table_names) {
      # Strategy 1: Exact match
      idx <- match(known_name, db_tables_lower)
      if (!is.na(idx)) {
        actual_name <- db_tables[idx]
        qualified <- bp$tables$qualified_name[bp$tables$table_name == actual_name]
        if (length(qualified) == 0) {
          schema <- handle$results_schema %||% handle$cdm_schema
          qualified <- .qualifyTable(handle, actual_name, schema)
        }
        n <- tryCatch({
          sql <- paste0("SELECT COUNT(*) AS n FROM ", qualified)
          as.integer(.executeQuery(handle, sql)$n[1])
        }, error = function(e) 0L)
        rows[[length(rows) + 1]] <- data.frame(
          table_name = actual_name, tool_id = tid,
          tool_name = tool$tool_name, qualified_name = qualified,
          n_rows = n, stringsAsFactors = FALSE
        )
        next
      }

      # Strategy 2: Prefix-pattern match
      for (pat in tool$prefix_patterns) {
        prefixed <- sub("^", pat, known_name)
        prefixed <- sub("\\^", "", prefixed)  # clean regex anchor
        pidx <- match(prefixed, db_tables_lower)
        if (!is.na(pidx)) {
          actual_name <- db_tables[pidx]
          qualified <- bp$tables$qualified_name[bp$tables$table_name == actual_name]
          if (length(qualified) == 0) {
            schema <- handle$results_schema %||% handle$cdm_schema
            qualified <- .qualifyTable(handle, actual_name, schema)
          }
          n <- tryCatch({
            sql <- paste0("SELECT COUNT(*) AS n FROM ", qualified)
            as.integer(.executeQuery(handle, sql)$n[1])
          }, error = function(e) 0L)
          rows[[length(rows) + 1]] <- data.frame(
            table_name = actual_name, tool_id = tid,
            tool_name = tool$tool_name, qualified_name = qualified,
            n_rows = n, stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(rows) == 0) return(empty)
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

# --- Count Column Detection ---

#' Detect count columns needing disclosure control
#'
#' Primary: registry lookup. Fallback: heuristic pattern match on column names.
#'
#' @param handle CDM handle
#' @param table_name Character; table name
#' @param tool_id Character; optional tool_id (auto-detected if NULL)
#' @return Character vector of column names containing counts
#' @keywords internal
.ohdsiDetectCountColumns <- function(handle, table_name, tool_id = NULL) {
  if (is.null(tool_id)) {
    tool_id <- .ohdsi_table_to_tool(table_name)
  }

  registry <- .ohdsi_tool_registry()

  # Primary: registry lookup
  if (!is.null(tool_id) && tool_id %in% names(registry)) {
    registered <- registry[[tool_id]]$count_columns
    # Get actual columns in the table
    bp <- .buildBlueprint(handle)
    actual_cols <- tryCatch({
      qualified <- bp$tables$qualified_name[
        tolower(bp$tables$table_name) == tolower(table_name)
      ]
      if (length(qualified) == 0) return(registered)
      sql <- paste0("SELECT * FROM ", qualified[1], " LIMIT 0")
      names(.executeQuery(handle, sql))
    }, error = function(e) character(0))

    matched <- intersect(tolower(registered), tolower(actual_cols))
    if (length(matched) > 0) return(matched)
  }

  # Fallback: heuristic pattern matching
  bp <- .buildBlueprint(handle)
  actual_cols <- tryCatch({
    qualified <- bp$tables$qualified_name[
      tolower(bp$tables$table_name) == tolower(table_name)
    ]
    if (length(qualified) == 0) return(character(0))
    sql <- paste0("SELECT * FROM ", qualified[1], " LIMIT 0")
    names(.executeQuery(handle, sql))
  }, error = function(e) character(0))

  pattern <- "^n_|^num_|_count$|_subjects$|_persons$|_records$"
  grep(pattern, actual_cols, value = TRUE, ignore.case = TRUE)
}

# --- Generic Query ---

#' Query an OHDSI result table
#'
#' @param handle CDM handle
#' @param table_name Character; table to query (validated via .validateIdentifier)
#' @param columns Character vector; columns to SELECT (NULL = all except sensitive)
#' @param filters Named list; WHERE conditions (name = column, value = filter value)
#' @param order_by Character; ORDER BY clause column name(s)
#' @param limit Integer; max rows (capped at 5000)
#' @param tool_id Character; optional tool_id for registry lookup
#' @return data.frame with disclosure control applied
#' @keywords internal
.ohdsiGetResults <- function(handle, table_name, columns = NULL,
                              filters = NULL, order_by = NULL,
                              limit = 5000L, tool_id = NULL) {
  # Validate table name
  .validateIdentifier(table_name, "table")
  table_name_lower <- tolower(table_name)

  # Verify table exists
  bp <- .buildBlueprint(handle)
  bp_match <- bp$tables[tolower(bp$tables$table_name) == table_name_lower &
                          bp$tables$present_in_db, , drop = FALSE]
  if (nrow(bp_match) == 0) {
    stop("Table '", table_name, "' not found in database.", call. = FALSE)
  }
  qualified <- bp_match$qualified_name[1]

  # Detect tool
  if (is.null(tool_id)) {
    tool_id <- .ohdsi_table_to_tool(table_name)
  }
  registry <- .ohdsi_tool_registry()

  # Get sensitive columns to exclude
  sensitive <- character(0)
  if (!is.null(tool_id) && tool_id %in% names(registry)) {
    sensitive <- registry[[tool_id]]$sensitive_columns
  }

  # Get actual columns
  actual_cols <- tryCatch({
    sql <- paste0("SELECT * FROM ", qualified, " LIMIT 0")
    names(.executeQuery(handle, sql))
  }, error = function(e) character(0))

  # Build SELECT columns
  if (is.null(columns)) {
    select_cols <- setdiff(actual_cols, tolower(sensitive))
  } else {
    # Validate each requested column
    columns <- tolower(columns)
    for (col in columns) .validateIdentifier(col, "column")
    # Remove sensitive even if explicitly requested
    select_cols <- setdiff(columns, tolower(sensitive))
    select_cols <- intersect(select_cols, actual_cols)
  }

  if (length(select_cols) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  # Build SQL
  sql <- paste0("SELECT ", paste(select_cols, collapse = ", "),
                " FROM ", qualified)

  # WHERE clause
  where_parts <- character(0)
  if (!is.null(filters) && length(filters) > 0) {
    for (col_name in names(filters)) {
      .validateIdentifier(col_name, "filter column")
      val <- filters[[col_name]]
      if (is.numeric(val)) {
        where_parts <- c(where_parts,
          paste0(tolower(col_name), " = ", val))
      } else {
        escaped <- gsub("'", "''", as.character(val))
        where_parts <- c(where_parts,
          paste0(tolower(col_name), " = '", escaped, "'"))
      }
    }
    sql <- paste0(sql, " WHERE ", paste(where_parts, collapse = " AND "))
  }

  # ORDER BY
  if (!is.null(order_by) && nchar(order_by) > 0) {
    .validateIdentifier(gsub(" (ASC|DESC)$", "", order_by, ignore.case = TRUE),
                         "order column")
    sql <- paste0(sql, " ORDER BY ", tolower(order_by))
  }

  # LIMIT (cap at 5000)
  limit <- min(as.integer(limit), 5000L)
  sql <- paste0(sql, " LIMIT ", limit)

  # Execute
  result <- tryCatch(
    .executeQuery(handle, sql),
    error = function(e) data.frame(stringsAsFactors = FALSE)
  )
  if (nrow(result) == 0) return(result)

  # Disclosure control on count columns
  count_cols <- .ohdsiDetectCountColumns(handle, table_name, tool_id)
  count_cols <- intersect(count_cols, names(result))
  if (length(count_cols) > 0) {
    result <- .suppressSmallCounts(result, count_cols)
  }

  result
}

# --- Status ---

#' Check OHDSI result tool availability
#'
#' @param handle CDM handle
#' @return Named list with per-tool availability
#' @keywords internal
.ohdsiStatus <- function(handle) {
  found <- .ohdsiFindResultTables(handle)

  registry <- .ohdsi_tool_registry()
  status <- list()

  for (tid in names(registry)) {
    tool_tables <- found[found$tool_id == tid, , drop = FALSE]
    status[[tid]] <- list(
      tool_name = registry[[tid]]$tool_name,
      available = nrow(tool_tables) > 0,
      tables = tool_tables$table_name,
      n_tables = nrow(tool_tables),
      total_rows = sum(tool_tables$n_rows)
    )
  }

  status
}

# --- Summary ---

#' Get a summary of results for a specific OHDSI tool
#'
#' @param handle CDM handle
#' @param tool_id Character; which tool to summarize
#' @return Named list with tool-specific summary info
#' @keywords internal
.ohdsiGetSummary <- function(handle, tool_id) {
  registry <- .ohdsi_tool_registry()
  if (!tool_id %in% names(registry)) {
    stop("Unknown tool_id: '", tool_id, "'. Available: ",
         paste(names(registry), collapse = ", "), call. = FALSE)
  }

  found <- .ohdsiFindResultTables(handle)
  tool_tables <- found[found$tool_id == tool_id, , drop = FALSE]

  if (nrow(tool_tables) == 0) {
    return(list(
      tool_id = tool_id,
      tool_name = registry[[tool_id]]$tool_name,
      available = FALSE,
      tables = data.frame(table_name = character(0), n_rows = integer(0),
                           stringsAsFactors = FALSE)
    ))
  }

  list(
    tool_id = tool_id,
    tool_name = registry[[tool_id]]$tool_name,
    available = TRUE,
    tables = tool_tables[, c("table_name", "n_rows"), drop = FALSE]
  )
}
