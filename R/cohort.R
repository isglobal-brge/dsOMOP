# ==============================================================================
# dsOMOP v2 - Cohort Operations
# ==============================================================================
# Uses blueprint instead of heuristics for column discovery.
# ==============================================================================

#' List available cohort definitions
#'
#' @param handle CDM handle
#' @return Data frame with cohort definitions
#' @keywords internal
.cohortList <- function(handle) {
  bp <- .buildBlueprint(handle)

  empty_df <- data.frame(
    cohort_definition_id = integer(0),
    cohort_definition_name = character(0),
    stringsAsFactors = FALSE
  )

  # Check results schema first, then CDM
  results_schema <- handle$results_schema
  tables_to_check <- character(0)

  if (!is.null(results_schema)) {
    tables_to_check <- .listTablesRaw(handle, results_schema)
  }

  if (!"cohort_definition" %in% tables_to_check) {
    # Try CDM schema
    if ("cohort_definition" %in% bp$tables$table_name[bp$tables$present_in_db]) {
      results_schema <- handle$cdm_schema
    } else {
      return(empty_df)
    }
  }

  qualified <- .qualifyTable(handle, "cohort_definition", results_schema)
  tryCatch(
    .executeQuery(handle, paste0("SELECT * FROM ", qualified)),
    error = function(e) empty_df
  )
}

#' Get a specific cohort definition
#'
#' @param handle CDM handle
#' @param cohort_definition_id Integer
#' @return Named list with definition details
#' @keywords internal
.cohortGetDefinition <- function(handle, cohort_definition_id) {
  results_schema <- handle$results_schema %||% handle$cdm_schema

  qualified <- .qualifyTable(handle, "cohort_definition", results_schema)
  sql <- paste0(
    "SELECT * FROM ", qualified,
    " WHERE cohort_definition_id = ", as.integer(cohort_definition_id)
  )

  result <- .executeQuery(handle, sql)
  if (nrow(result) == 0) {
    stop("Cohort definition ", cohort_definition_id, " not found.", call. = FALSE)
  }
  as.list(result[1, ])
}

#' Create a cohort from a structured specification
#'
#' Uses blueprint for column discovery instead of heuristics.
#'
#' @param handle CDM handle
#' @param spec Named list defining the cohort
#' @param mode Character; "temporary" or "persistent"
#' @param cohort_id Integer; cohort_definition_id
#' @param name Character; cohort name
#' @param overwrite Logical; overwrite existing cohort
#' @return Character; temp table name or confirmation message
#' @keywords internal
.cohortCreate <- function(handle, spec, mode = "temporary",
                          cohort_id = NULL, name = NULL,
                          overwrite = FALSE) {
  bp <- .buildBlueprint(handle)

  spec_type <- tolower(spec$type %||% "condition")

  source_table <- switch(spec_type,
    "condition"   = "condition_occurrence",
    "drug"        = "drug_exposure",
    "measurement" = "measurement",
    "observation" = "observation",
    "procedure"   = "procedure_occurrence",
    stop("Unknown cohort spec type: '", spec_type, "'", call. = FALSE)
  )

  tbl_row <- bp$tables[bp$tables$table_name == source_table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) {
    stop("Source table '", source_table, "' not found in CDM.", call. = FALSE)
  }

  # Use blueprint for column discovery
  concept_col <- .getDomainConceptColumn(bp, source_table)
  col_df <- bp$columns[[source_table]]
  src_cols <- col_df$column_name

  qualified_source <- tbl_row$qualified_name[1]

  # Find date columns via blueprint
  date_col <- .getDateColumn(bp, source_table)
  end_date_cols <- grep("_end_date$", src_cols, value = TRUE)

  select_parts <- c("person_id AS subject_id")
  if (!is.null(date_col)) {
    select_parts <- c(select_parts, paste0(date_col, " AS cohort_start_date"))
  }
  if (length(end_date_cols) > 0) {
    select_parts <- c(select_parts, paste0(end_date_cols[1], " AS cohort_end_date"))
  }

  sql <- paste0(
    "SELECT DISTINCT ", paste(select_parts, collapse = ", "),
    " FROM ", qualified_source
  )

  where <- character(0)

  # Concept set filter
  if (!is.null(spec$concept_set) && !is.null(concept_col) && concept_col %in% src_cols) {
    ids <- paste(as.integer(spec$concept_set), collapse = ", ")
    where <- c(where, paste0(concept_col, " IN (", ids, ")"))
  }

  # Value threshold (for measurements)
  if (!is.null(spec$value_threshold) && "value_as_number" %in% src_cols) {
    op <- spec$value_threshold$op %||% ">="
    val <- as.numeric(spec$value_threshold$value)
    if (op %in% c(">=", "<=", ">", "<", "==", "!=")) {
      if (op == "==") op <- "="
      where <- c(where, paste0("value_as_number ", op, " ", val))
    }
  }

  if (length(where) > 0) {
    sql <- paste0(sql, " WHERE ", paste(where, collapse = " AND "))
  }

  # Check disclosure
  person_count_sql <- paste0(
    "SELECT COUNT(DISTINCT sub.subject_id) AS n ",
    "FROM (", sql, ") sub"
  )
  .assertMinPersons(conn = handle$conn, sql = person_count_sql)

  if (mode == "temporary") {
    temp_name <- paste0("dsomop_cohort_", as.integer(cohort_id %||% 0))
    .createTempTable(handle, temp_name, sql)

    # Apply inclusion criteria if specified
    if (!is.null(spec$inclusion_criteria)) {
      temp_name <- .applyInclusionCriteria(
        handle, temp_name, spec$inclusion_criteria
      )
    }
    return(temp_name)

  } else if (mode == "persistent") {
    results_schema <- handle$results_schema
    if (is.null(results_schema)) {
      stop("Cannot persist cohort: no results_schema configured.", call. = FALSE)
    }

    cohort_table <- .qualifyTable(handle, "cohort", results_schema)

    if (overwrite && !is.null(cohort_id)) {
      del_sql <- paste0(
        "DELETE FROM ", cohort_table,
        " WHERE cohort_definition_id = ", as.integer(cohort_id)
      )
      tryCatch(.executeStatement(handle, del_sql), error = function(e) NULL)
    }

    insert_sql <- paste0(
      "INSERT INTO ", cohort_table, " ",
      "(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) ",
      "SELECT ", as.integer(cohort_id %||% 0), " AS cohort_definition_id, ",
      "sub.* FROM (", sql, ") AS sub"
    )
    .executeStatement(handle, insert_sql)

    return(paste0("Cohort ", cohort_id, " persisted to ", results_schema))
  }
}

#' Combine two cohorts using set operations
#'
#' @param handle CDM handle
#' @param op Character; "intersect", "union", or "setdiff"
#' @param cohort_table_a Character; first cohort temp table
#' @param cohort_table_b Character; second cohort temp table
#' @param new_name Character; name for result temp table
#' @return Character; result temp table name
#' @keywords internal
.cohortCombine <- function(handle, op, cohort_table_a, cohort_table_b,
                           new_name = NULL) {
  sql <- switch(tolower(op),
    "intersect" = paste0(
      "SELECT a.subject_id, a.cohort_start_date, a.cohort_end_date ",
      "FROM ", cohort_table_a, " AS a ",
      "INNER JOIN ", cohort_table_b, " AS b ",
      "ON a.subject_id = b.subject_id"
    ),
    "union" = paste0(
      "SELECT subject_id, cohort_start_date, cohort_end_date FROM ",
      cohort_table_a,
      " UNION ",
      "SELECT subject_id, cohort_start_date, cohort_end_date FROM ",
      cohort_table_b
    ),
    "setdiff" =, "difference" = paste0(
      "SELECT a.subject_id, a.cohort_start_date, a.cohort_end_date ",
      "FROM ", cohort_table_a, " AS a ",
      "WHERE NOT EXISTS (",
      "SELECT 1 FROM ", cohort_table_b, " AS b ",
      "WHERE a.subject_id = b.subject_id)"
    ),
    stop("Unknown cohort operation: '", op, "'", call. = FALSE)
  )

  person_count_sql <- paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM (", sql, ") AS combined"
  )
  .assertMinPersons(conn = handle$conn, sql = person_count_sql)

  result_name <- new_name %||% paste0("dsomop_cohort_combined_",
                                       sample(1000:9999, 1))
  .createTempTable(handle, result_name, sql)
  result_name
}

#' Apply inclusion criteria to filter a cohort
#'
#' Each criterion references a table with temporal constraints and
#' occurrence requirements. Persons must satisfy all criteria.
#'
#' @param handle CDM handle
#' @param cohort_temp Character; cohort temp table name
#' @param criteria List of inclusion criterion specs
#' @return Character; filtered cohort temp table name
#' @keywords internal
.applyInclusionCriteria <- function(handle, cohort_temp, criteria) {
  if (is.null(criteria) || length(criteria) == 0) return(cohort_temp)
  bp <- .buildBlueprint(handle)

  for (i in seq_along(criteria)) {
    crit <- criteria[[i]]
    crit_table <- tolower(crit$table %||% "")
    tbl_row <- bp$tables[bp$tables$table_name == crit_table &
                           bp$tables$present_in_db, , drop = FALSE]
    if (nrow(tbl_row) == 0) next

    concept_col <- .getDomainConceptColumn(bp, crit_table)
    date_col <- .getDateColumn(bp, crit_table)
    qualified <- tbl_row$qualified_name[1]

    # Build subquery for this criterion
    sub_where <- character(0)

    # Concept set filter
    if (!is.null(crit$concept_set) && !is.null(concept_col)) {
      ids <- paste(as.integer(crit$concept_set), collapse = ", ")
      sub_where <- c(sub_where, paste0("e.", concept_col, " IN (", ids, ")"))
    }

    # Temporal constraints (index-relative window)
    if (!is.null(crit$temporal$index_window) && !is.null(date_col)) {
      iw <- crit$temporal$index_window
      if (!is.null(iw$start)) {
        sub_where <- c(sub_where, paste0(
          "e.", date_col, " >= ",
          .renderSql(handle, "DATEADD(day, @days, c.cohort_start_date)",
                     days = as.integer(iw$start))
        ))
      }
      if (!is.null(iw$end)) {
        sub_where <- c(sub_where, paste0(
          "e.", date_col, " <= ",
          .renderSql(handle, "DATEADD(day, @days, c.cohort_start_date)",
                     days = as.integer(iw$end))
        ))
      }
    }

    # Calendar time constraints
    if (!is.null(crit$temporal$calendar) && !is.null(date_col)) {
      cal <- crit$temporal$calendar
      if (!is.null(cal$start)) {
        sub_where <- c(sub_where, paste0(
          "e.", date_col, " >= '", cal$start, "'"
        ))
      }
      if (!is.null(cal$end)) {
        sub_where <- c(sub_where, paste0(
          "e.", date_col, " <= '", cal$end, "'"
        ))
      }
    }

    where_clause <- ""
    if (length(sub_where) > 0) {
      where_clause <- paste0(" AND ", paste(sub_where, collapse = " AND "))
    }

    # Occurrence check
    occ <- crit$occurrence
    occ_type <- occ$type %||% "at_least"
    occ_count <- as.integer(occ$count %||% 1L)

    if (occ_type == "at_least") {
      having_clause <- paste0(" HAVING COUNT(*) >= ", occ_count)
    } else if (occ_type == "at_most") {
      having_clause <- paste0(" HAVING COUNT(*) <= ", occ_count)
    } else if (occ_type == "exactly") {
      having_clause <- paste0(" HAVING COUNT(*) = ", occ_count)
    } else {
      having_clause <- paste0(" HAVING COUNT(*) >= ", occ_count)
    }

    # Filter cohort: keep only persons with qualifying events
    filter_sql <- paste0(
      "SELECT c.subject_id, c.cohort_start_date, c.cohort_end_date ",
      "FROM ", cohort_temp, " AS c ",
      "WHERE EXISTS (SELECT 1 FROM (",
      "SELECT e.person_id",
      " FROM ", qualified, " AS e ",
      "WHERE e.person_id = c.subject_id", where_clause,
      " GROUP BY e.person_id",
      having_clause,
      ") AS crit_sub)"
    )

    new_temp <- paste0(cohort_temp, "_ic", i)
    .createTempTable(handle, new_temp, filter_sql)
    .dropTempTable(handle, cohort_temp)
    cohort_temp <- new_temp
  }

  cohort_temp
}
