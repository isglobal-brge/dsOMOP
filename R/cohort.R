# Module: Cohort Operations
# Server-side cohort creation, combination, and management.

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

  # Concept set filter. Accepts a flat vector of IDs or a concept-set spec
  # (list with $concepts plus optional include_descendants/include_mapped/
  # exclude), resolved the same way as the plan path via .resolveConceptSet.
  if (!is.null(spec$concept_set) && !is.null(concept_col) && concept_col %in% src_cols) {
    concept_ids <- .resolveConceptSet(handle, spec$concept_set)
    if (length(concept_ids) > 0) {
      ids <- paste(concept_ids, collapse = ", ")
      where <- c(where, paste0(concept_col, " IN (", ids, ")"))
    }
  }

  # Value threshold (for measurements). Disclosure policy: threshold operators
  # define a population (low fingerprinting risk, still size-checked by
  # .assertMinPersons below) and are allowed; exact-value operators (==, !=)
  # only serve to single out individuals with a precise value and are blocked.
  if (!is.null(spec$value_threshold) && "value_as_number" %in% src_cols) {
    op <- spec$value_threshold$op %||% ">="
    val <- as.numeric(spec$value_threshold$value)
    if (op %in% c("==", "=", "!=")) {
      stop("Disclosive: exact-value cohort criteria ('", op, "') are not ",
           "permitted; use a threshold operator (>=, <=, >, <).",
           call. = FALSE)
    }
    if (!op %in% c(">=", "<=", ">", "<")) {
      stop("Unknown value_threshold operator: '", op, "'.", call. = FALSE)
    }
    where <- c(where, paste0("value_as_number ", op, " ", val))
  }

  if (length(where) > 0) {
    sql <- paste0(sql, " WHERE ", paste(where, collapse = " AND "))
  }

  # Check disclosure
  person_count_sql <- paste0(
    "SELECT COUNT(DISTINCT sub.subject_id) AS n ",
    "FROM (", sql, ") sub"
  )
  .assertMinPersons(handle = handle, sql = person_count_sql)

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
  .assertMinPersons(handle = handle, sql = person_count_sql)

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

    # Concept set filter (flat vector or concept-set spec; see .resolveConceptSet)
    if (!is.null(crit$concept_set) && !is.null(concept_col)) {
      concept_ids <- .resolveConceptSet(handle, crit$concept_set)
      if (length(concept_ids) > 0) {
        ids <- paste(concept_ids, collapse = ", ")
        sub_where <- c(sub_where, paste0("e.", concept_col, " IN (", ids, ")"))
      }
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

  # Re-gate the FINAL inclusion-filtered cohort on distinct persons (fail-closed).
  # The pre-inclusion cohort was already gated in .cohortCreate, but inclusion
  # criteria narrow the population further and could drop it below threshold;
  # without this check a tightly-specified set of criteria could isolate a
  # handful of individuals.
  final_count_sql <- paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort_temp
  )
  .assertMinPersons(handle = handle, sql = final_count_sql)

  cohort_temp
}

#' Resolve a cohort reference to a server-side cohort temp table (population scope)
#'
#' Single entry point that turns the various ways a caller may name a cohort into
#' a materialized temp table whose \code{subject_id} column can be INNER-JOINed by
#' the exploration aggregates to scope a population. Accepts:
#' \itemize{
#'   \item \code{NULL} -> returns \code{NULL} (no scoping).
#'   \item a character string already naming a server-side cohort temp table
#'     (e.g. one returned by \code{\link{.cohortCreate}}, \code{.cohortCombine},
#'     or \code{\link{omopCohortFromTableDS}}) -> validated and returned as-is.
#'   \item a numeric \code{cohort_definition_id} -> the matching rows of the
#'     \code{cohort} results table are materialized into a temp table (mirrors the
#'     \code{cohort_table} branch of \code{\link{.planExecute}}).
#' }
#' Every path that materializes (or is handed) a cohort is gated on its DISTINCT
#' subject count via \code{\link{.assertMinPersons}} (fail-closed): a cohort with
#' fewer than \code{nfilter_subset} persons can never be used to scope a query.
#'
#' @param handle CDM handle.
#' @param cohort A cohort temp-table name (character), a cohort_definition_id
#'   (numeric), or \code{NULL}.
#' @return Character cohort temp table name, or \code{NULL} when \code{cohort} is
#'   \code{NULL}.
#' @keywords internal
.resolveCohortTable <- function(handle, cohort) {
  if (is.null(cohort)) return(NULL)

  # A bare cohort_definition_id: materialize from the cohort results table, then
  # gate on distinct subjects (same shape as .planExecute's cohort_table branch).
  if (is.numeric(cohort) ||
      (is.character(cohort) && length(cohort) == 1L &&
       grepl("^[0-9]+$", cohort))) {
    cid <- as.integer(cohort)
    results_schema <- handle$results_schema %||% handle$cdm_schema
    qualified <- .qualifyTable(handle, "cohort", results_schema)

    cohort_sql <- paste0(
      "SELECT DISTINCT subject_id, cohort_start_date, cohort_end_date",
      " FROM ", qualified,
      " WHERE cohort_definition_id = ", cid
    )
    temp_name <- paste0("dsomop_cohort_def_", cid)
    .dropTempTable(handle, temp_name)
    cohort_table <- .createTempTable(handle, temp_name, cohort_sql)

    count_sql <- paste0(
      "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort_table)
    .assertMinPersons(handle = handle, sql = count_sql)
    return(cohort_table)
  }

  # An explicit server-side cohort temp table name. Validate as an identifier
  # (defends the INNER JOIN splice) and re-gate on its distinct subjects so a
  # too-small cohort table can never scope a query, regardless of how it was
  # produced.
  cohort_table <- .validateIdentifier(as.character(cohort), "cohort")
  count_sql <- paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort_table)
  .assertMinPersons(handle = handle, sql = count_sql)
  cohort_table
}

#' Materialize a cohort from the DISTINCT person tokens of a workspace omop.table
#'
#' Server engine for \code{\link{omopCohortFromTableDS}}. Given a token-keyed
#' \code{omop.table} data.frame already living in the DataSHIELD session (e.g. a
#' plan output, or a merge/filter result), it derives a reusable cohort temp
#' table WITHOUT the client ever sending any identifier:
#' \enumerate{
#'   \item read the frame's DISTINCT non-NA person/subject TOKENS;
#'   \item reverse them to the ORIGINAL CDM ids with the per-resource key via
#'     \code{\link{.unhashPersonKey}} (server-only; the client cannot invert a
#'     token);
#'   \item gate the distinct ORIGINAL-id count with \code{\link{.assertMinPersons}}
#'     (fail-closed) BEFORE materializing anything;
#'   \item materialize a temp table of \code{subject_id} (original ids) joined to
#'     \code{observation_period} for cohort start/end dates, mirroring the
#'     filter-cohort branch of \code{\link{.planExecute}};
#'   \item re-gate the materialized table on its distinct subjects.
#' }
#'
#' @param handle CDM handle (provides \code{handle$person_key} and the
#'   connection).
#' @param x A token-keyed \code{omop.table} data.frame (resolved from a session
#'   symbol by DataSHIELD).
#' @param new_name Character; the cohort temp table name to create. When NULL a
#'   random name is generated. The client passes a deterministic name so the
#'   returned handle points at a table it can name in later \code{cohort=} calls
#'   (mirrors \code{.cohortCombine}).
#' @return Character; the cohort temp table name.
#' @keywords internal
.cohortFromTokenFrame <- function(handle, x, new_name = NULL) {
  if (!.is_omop.table(x)) {
    stop("omopCohortFromTableDS: input must be a dsOMOP table (omop.table).",
         call. = FALSE)
  }
  keys <- intersect(.PERSON_KEY_COLS(), names(x))
  if (length(keys) == 0L) {
    stop("omopCohortFromTableDS: object has no person key; cannot build a cohort.",
         call. = FALSE)
  }
  key <- if ("person_id" %in% keys) "person_id" else keys[[1]]

  tokens <- x[[key]]
  tokens <- unique(tokens[!is.na(tokens)])
  if (length(tokens) == 0L) {
    stop("omopCohortFromTableDS: no person tokens to build a cohort from.",
         call. = FALSE)
  }

  if (is.null(handle$person_key)) {
    stop("omopCohortFromTableDS: no person key available on this handle; ",
         "cannot reverse tokens.", call. = FALSE)
  }
  # Reverse tokens -> ORIGINAL ids (server-only). Distinct tokens map 1:1 to
  # distinct ids (the encryption is injective), so the distinct count is the
  # number of real persons; gate on it BEFORE writing any table (fail-closed).
  original_ids <- unique(.unhashPersonKey(tokens, handle$person_key))
  original_ids <- original_ids[!is.na(original_ids)]
  .assertMinPersons(n_persons = length(original_ids))

  bp <- .buildBlueprint(handle)
  ids_str <- .sqlIdList(original_ids)
  temp_name <- if (!is.null(new_name)) {
    .validateIdentifier(as.character(new_name), "cohort name")
  } else {
    paste0("dsomop_cohort_fromtbl_", sample(100000:999999, 1))
  }
  .dropTempTable(handle, temp_name)

  # Prefer observation_period for cohort start/end dates (as .planExecute does);
  # fall back to a bare subject_id cohort when the table is absent.
  obs_table <- bp$tables[bp$tables$table_name == "observation_period" &
                           bp$tables$present_in_db, , drop = FALSE]
  if (nrow(obs_table) > 0) {
    obs_qualified <- obs_table$qualified_name[1]
    cohort_sql <- paste0(
      "SELECT DISTINCT o.person_id AS subject_id, ",
      "o.observation_period_start_date AS cohort_start_date, ",
      "o.observation_period_end_date AS cohort_end_date ",
      "FROM ", obs_qualified, " o ",
      "WHERE o.person_id IN (", ids_str, ")"
    )
  } else {
    person_table <- bp$tables[bp$tables$table_name == "person" &
                                bp$tables$present_in_db, , drop = FALSE]
    if (nrow(person_table) == 0) {
      stop("omopCohortFromTableDS: neither observation_period nor person ",
           "table is available to anchor the cohort.", call. = FALSE)
    }
    person_qualified <- person_table$qualified_name[1]
    cohort_sql <- paste0(
      "SELECT DISTINCT p.person_id AS subject_id ",
      "FROM ", person_qualified, " p ",
      "WHERE p.person_id IN (", ids_str, ")"
    )
  }

  cohort_table <- .createTempTable(handle, temp_name, cohort_sql)

  # Re-gate the materialized cohort on distinct subjects: if a token did not map
  # to any in-DB person (or the join dropped rows) the producible cohort must
  # still clear the threshold, else it is unusable and unproducible.
  count_sql <- paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort_table)
  .assertMinPersons(handle = handle, sql = count_sql)

  cohort_table
}
