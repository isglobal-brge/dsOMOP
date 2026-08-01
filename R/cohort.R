# Module: Cohort Operations
# Server-side cohort creation, combination, and management.

#' List available cohort definitions
#'
#' @param handle CDM handle
#' @return Data frame with cohort definitions
#' @keywords internal
.cohortList <- function(handle) {
  settings <- .omopDisclosureSettings()
  threshold <- settings$nfilter_subset
  band <- settings$nfilter_band %||% 5

  empty_df <- data.frame(
    cohort_definition_id = integer(0),
    cohort_definition_name = character(0),
    size = numeric(0),
    stringsAsFactors = FALSE
  )

  # Resolve the schema holding cohort_definition (results first, then CDM).
  bp <- .buildBlueprint(handle)
  results_schema <- handle$results_schema
  tables_to_check <- character(0)
  if (!is.null(results_schema)) {
    tables_to_check <- .listTablesRaw(handle, results_schema)
  }
  if (!"cohort_definition" %in% tables_to_check) {
    if ("cohort_definition" %in% bp$tables$table_name[bp$tables$present_in_db]) {
      results_schema <- handle$cdm_schema
    } else {
      return(empty_df)
    }
  }

  def_tbl    <- .qualifyTable(handle, "cohort_definition", results_schema)
  cohort_tbl <- .qualifyTable(handle, "cohort", results_schema)

  # DISCLOSURE: a cohort with < nfilter_subset DISTINCT subjects is INVISIBLE —
  # omitted entirely (a sub-threshold cohort is treated as if it does not exist).
  # Cohorts with no rows in the cohort table (0 subjects) never reach the count
  # frame, so they are omitted too. Survivors carry a BANDED LOWER BOUND (a
  # multiple of nfilter_band, possibly zero when the disclosure threshold is
  # narrower than one band), NEVER the exact subject count.
  counts <- tryCatch(
    .executeQuery(handle, paste0(
      "SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS n FROM ",
      cohort_tbl, " GROUP BY cohort_definition_id")),
    error = function(e) NULL)
  if (is.null(counts) || nrow(counts) == 0) return(empty_df)
  counts <- counts[!is.na(counts$n) & counts$n >= threshold, , drop = FALSE]
  if (nrow(counts) == 0) return(empty_df)

  defs <- tryCatch(.executeQuery(handle, paste0("SELECT * FROM ", def_tbl)),
                   error = function(e) NULL)
  if (is.null(defs) || nrow(defs) == 0) return(empty_df)

  merged <- merge(defs, counts, by = "cohort_definition_id")
  if (nrow(merged) == 0) return(empty_df)
  merged$size <- vapply(
    merged$n, .bandCount, numeric(1), band_width = band
  )
  merged$n <- NULL
  rownames(merged) <- NULL
  merged
}

#' Get a specific cohort definition
#'
#' @param handle CDM handle
#' @param cohort_definition_id Integer
#' @return Named list with definition details
#' @keywords internal
.cohortGetDefinition <- function(handle, cohort_definition_id) {
  settings <- .omopDisclosureSettings()
  threshold <- settings$nfilter_subset
  results_schema <- handle$results_schema %||% handle$cdm_schema
  cid <- as.integer(cohort_definition_id)

  # DISCLOSURE: a cohort with < nfilter_subset distinct subjects is treated as
  # NONEXISTENT. An absent id and a sub-threshold id return the IDENTICAL
  # "not found" — a caller can never confirm a small cohort exists, nor read its
  # name/description/syntax. Only at/above-threshold cohorts are readable.
  cohort_tbl <- .qualifyTable(handle, "cohort", results_schema)
  n <- tryCatch(
    .executeQuery(handle, paste0(
      "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort_tbl,
      " WHERE cohort_definition_id = ", cid))$n[1],
    error = function(e) 0)
  n <- if (length(n) == 0L || is.na(n)) 0 else n
  not_found <- function()
    stop("Cohort definition ", cid, " not found.", call. = FALSE)
  if (n < threshold) not_found()

  def_tbl <- .qualifyTable(handle, "cohort_definition", results_schema)
  result <- tryCatch(.executeQuery(handle, paste0(
    "SELECT * FROM ", def_tbl, " WHERE cohort_definition_id = ", cid)),
    error = function(e) NULL)
  if (is.null(result) || nrow(result) == 0) not_found()
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
#' @param name Reserved for a future \code{cohort_definition} metadata writer.
#'   This function currently writes cohort rows only and does not create or
#'   update a \code{cohort_definition} record.
#' @param overwrite Logical; overwrite existing cohort
#' @return Character; temp table name or confirmation message
#' @keywords internal
.cohortCreate <- function(handle, spec, mode = "temporary",
                          cohort_id = NULL, name = NULL,
                          overwrite = FALSE) {
  mode <- match.arg(mode, c("temporary", "persistent"))
  if (!is.list(spec)) {
    stop("Cohort spec must be a named list.", call. = FALSE)
  }
  if (length(spec) > 0L &&
      (is.null(names(spec)) || any(!nzchar(names(spec))) ||
       anyDuplicated(names(spec)))) {
    stop("Cohort spec must be a uniquely named list.", call. = FALSE)
  }
  unknown_spec <- setdiff(names(spec) %||% character(0),
                          c("type", "concept_set", "value_threshold",
                            "value_bin", "inclusion_criteria"))
  if (length(unknown_spec) > 0L) {
    stop("Unknown cohort spec field(s): ",
         paste(unknown_spec, collapse = ", "), ".", call. = FALSE)
  }
  if (length(overwrite) != 1L || is.na(overwrite) || !is.logical(overwrite)) {
    stop("overwrite must be TRUE or FALSE.", call. = FALSE)
  }
  if (identical(mode, "persistent")) {
    cohort_id_num <- suppressWarnings(as.numeric(cohort_id))
    if (length(cohort_id_num) != 1L || is.na(cohort_id_num) ||
        !is.finite(cohort_id_num) || cohort_id_num < 0 ||
        cohort_id_num != floor(cohort_id_num) ||
        cohort_id_num > .Machine$integer.max) {
      stop("cohort_id must be one non-negative integer for a persistent cohort.",
           call. = FALSE)
    }
    cohort_id <- as.integer(cohort_id_num)
  }
  bp <- .buildBlueprint(handle)

  spec_type <- spec$type %||% "condition"
  if (!is.character(spec_type) || length(spec_type) != 1L ||
      is.na(spec_type) || !nzchar(spec_type)) {
    stop("Cohort spec type must be one non-empty string.", call. = FALSE)
  }
  spec_type <- tolower(spec_type)

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

  if (is.null(date_col) || !date_col %in% src_cols) {
    stop("Source table '", source_table,
         "' has no usable event date for a cohort index.", call. = FALSE)
  }

  select_parts <- c("person_id AS subject_id",
                    paste0(date_col, " AS cohort_start_date"))
  if (length(end_date_cols) > 0) {
    select_parts <- c(select_parts, paste0(
      "COALESCE(", end_date_cols[1], ", ", date_col,
      ") AS cohort_end_date"))
  } else {
    # OHDSI cohort rows require a closed era. A point event is a one-day era.
    select_parts <- c(select_parts, paste0(date_col, " AS cohort_end_date"))
  }

  sql <- paste0(
    "SELECT DISTINCT ", paste(select_parts, collapse = ", "),
    " FROM ", qualified_source
  )

  where <- character(0)

  # Concept set filter. Accepts a flat vector of IDs or a concept-set spec
  # (list with $concepts plus optional include_descendants/include_mapped/
  # exclude), resolved the same way as the plan path via .resolveConceptSet.
  concept_ids <- integer(0)
  if (!is.null(spec$concept_set)) {
    if (is.null(concept_col) || !concept_col %in% src_cols) {
      stop("Source table '", source_table,
           "' cannot apply the requested concept_set.", call. = FALSE)
    }
    concept_ids <- .resolveConceptSet(handle, spec$concept_set)
    if (length(concept_ids) == 0) {
      stop("concept_set resolved to no concepts; refusing an unfiltered cohort.",
           call. = FALSE)
    }
    where <- c(where, .sqlIdInPredicate(concept_col, concept_ids))
  }

  # Legacy client-authored thresholds are not session-authenticated and permit
  # adaptive probing. Preserve a clear migration error instead of silently
  # accepting them through this older cohort API.
  if (!is.null(spec$value_threshold)) {
    stop("Disclosive: value_threshold is no longer executable. Use a ",
         "server-issued value_bin from ds.omop.safe.filter.value().",
         call. = FALSE)
  }

  if (!is.null(spec$value_bin)) {
    value_bin <- spec$value_bin
    if (!is.list(value_bin) || is.null(names(value_bin)) ||
        any(!nzchar(names(value_bin))) || anyDuplicated(names(value_bin)) ||
        !setequal(names(value_bin), c("lower", "upper", "safe_scope")) ||
        length(names(value_bin)) != 3L) {
      stop("value_bin must contain exactly lower, upper, and safe_scope.",
           call. = FALSE)
    }
    if (!"value_as_number" %in% src_cols) {
      stop("Source table '", source_table,
           "' cannot apply value_bin.", call. = FALSE)
    }
    lower <- suppressWarnings(as.numeric(value_bin$lower))
    upper <- suppressWarnings(as.numeric(value_bin$upper))
    .assertSafeNumericBinContract(
      handle, table = source_table, column = "value_as_number",
      value = list(lower = lower, upper = upper),
      scope = value_bin$safe_scope
    )
    scope_concept <- value_bin$safe_scope$concept_id %||% NULL
    scope_concept_col <- value_bin$safe_scope$concept_col %||% NULL
    if (length(concept_ids) == 0L) {
      if (!is.null(scope_concept)) {
        stop("value_bin concept scope does not match the cohort concept_set.",
             call. = FALSE)
      }
    } else if (length(concept_ids) != 1L ||
               length(scope_concept) != 1L ||
               is.na(suppressWarnings(as.integer(scope_concept))) ||
               as.integer(scope_concept) != concept_ids[[1]] ||
               !identical(tolower(scope_concept_col %||% concept_col),
                          tolower(concept_col))) {
      stop("value_bin requires one concept and a matching server-issued ",
           "concept scope.", call. = FALSE)
    }
    where <- c(where, paste0(
      "value_as_number >= ", lower,
      " AND value_as_number < ", upper
    ))
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

    persistent_source_sql <- sql
    build_temp <- NULL
    if (!is.null(spec$inclusion_criteria) &&
        length(spec$inclusion_criteria) > 0L) {
      build_temp <- paste0("dsomop_cohort_build_", as.integer(cohort_id))
      .createTempTable(handle, build_temp, sql)
      build_temp <- .applyInclusionCriteria(
        handle, build_temp, spec$inclusion_criteria
      )
      on.exit(if (!is.null(build_temp)) .dropTempTable(handle, build_temp),
              add = TRUE)
      persistent_source_sql <- paste0(
        "SELECT subject_id, cohort_start_date, cohort_end_date FROM ", build_temp
      )
    }

    insert_sql <- paste0(
      "INSERT INTO ", cohort_table, " ",
      "(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) ",
      "SELECT ", as.integer(cohort_id %||% 0), " AS cohort_definition_id, ",
      "sub.subject_id, sub.cohort_start_date, sub.cohort_end_date FROM (",
      persistent_source_sql, ") AS sub"
    )
    del_sql <- paste0(
      "DELETE FROM ", cohort_table,
      " WHERE cohort_definition_id = ", as.integer(cohort_id)
    )
    exists_sql <- paste0(
      "SELECT COUNT(*) AS n FROM ", cohort_table,
      " WHERE cohort_definition_id = ", as.integer(cohort_id)
    )

    # Persistent replacement must be all-or-nothing.  In particular, never
    # reconnect between DELETE and INSERT: a reconnect would leave the old
    # cohort deleted outside the transaction that protects its replacement.
    conn <- .conn(handle)
    tryCatch(
      DBI::dbWithTransaction(conn, {
        existing <- DBI::dbGetQuery(conn, exists_sql)$n[1]
        if (length(existing) != 1L || is.na(existing) ||
            !is.finite(as.numeric(existing)) || as.numeric(existing) < 0) {
          stop("Could not verify existing rows for cohort ", cohort_id, ".",
               call. = FALSE)
        }
        if (!isTRUE(overwrite) && !is.na(existing) && existing > 0L) {
          stop("Cohort ", cohort_id, " already has persisted rows; set ",
               "overwrite=TRUE to replace it atomically.", call. = FALSE)
        }
        if (isTRUE(overwrite) && !is.na(existing) && existing > 0L) {
          DBI::dbExecute(conn, del_sql)
        }
        DBI::dbExecute(conn, insert_sql)
      }),
      error = function(e) {
        stop("Persistent cohort write requires a successful database ",
             "transaction and was not committed: ", conditionMessage(e),
             call. = FALSE)
      }
    )

    return(paste0(
      "Cohort rows for ", cohort_id, " persisted to ", results_schema,
      "; cohort_definition metadata was not modified"
    ))
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
  cohort_table_a <- .validateIdentifier(cohort_table_a, "first cohort")
  cohort_table_b <- .validateIdentifier(cohort_table_b, "second cohort")
  owned <- unique(handle$temp_tables %||% character(0))
  if (!cohort_table_a %in% owned || !cohort_table_b %in% owned) {
    stop("Cohort set operations accept only temporary cohorts created by this handle.",
         call. = FALSE)
  }
  if (!is.null(new_name)) {
    new_name <- .validateIdentifier(new_name, "new cohort")
  }
  sql <- switch(tolower(op),
    "intersect" = paste0(
      "SELECT a.subject_id, a.cohort_start_date, a.cohort_end_date ",
      "FROM ", cohort_table_a, " AS a ",
      "WHERE EXISTS (SELECT 1 FROM ", cohort_table_b, " AS b ",
      "WHERE b.subject_id = a.subject_id)"
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
  if (!is.list(criteria)) {
    stop("inclusion_criteria must be a list of named criteria.",
         call. = FALSE)
  }
  bp <- .buildBlueprint(handle)
  base_temp <- cohort_temp

  for (i in seq_along(criteria)) {
    crit <- criteria[[i]]
    if (!is.list(crit) || is.null(names(crit)) ||
        any(!nzchar(names(crit))) || anyDuplicated(names(crit))) {
      stop("Each inclusion criterion must be a uniquely named list.",
           call. = FALSE)
    }
    unknown_crit <- setdiff(names(crit),
                            c("table", "concept_set", "temporal", "occurrence"))
    if (length(unknown_crit) > 0L) {
      stop("Unknown inclusion criterion field(s): ",
           paste(unknown_crit, collapse = ", "), ".", call. = FALSE)
    }
    if (!is.null(crit$temporal)) {
      if (!is.list(crit$temporal) || is.null(names(crit$temporal)) ||
          any(!nzchar(names(crit$temporal))) ||
          anyDuplicated(names(crit$temporal))) {
        stop("Inclusion criterion temporal must be a uniquely named list.",
             call. = FALSE)
      }
      unknown_temporal <- setdiff(names(crit$temporal),
                                  c("index_window", "calendar"))
      if (length(unknown_temporal) > 0L) {
        stop("Unknown inclusion temporal field(s): ",
             paste(unknown_temporal, collapse = ", "), ".", call. = FALSE)
      }
    }
    crit_table <- tolower(crit$table %||% "")
    tbl_row <- bp$tables[bp$tables$table_name == crit_table &
                           bp$tables$present_in_db, , drop = FALSE]
    if (nrow(tbl_row) == 0) {
      stop("Inclusion criterion table '", crit_table,
           "' is not present in the CDM.", call. = FALSE)
    }

    concept_col <- .getDomainConceptColumn(bp, crit_table)
    date_col <- .getDateColumn(bp, crit_table)
    qualified <- tbl_row$qualified_name[1]

    # Build subquery for this criterion
    sub_where <- character(0)

    # Concept set filter (flat vector or concept-set spec; see .resolveConceptSet)
    if (!is.null(crit$concept_set)) {
      if (is.null(concept_col)) {
        stop("Inclusion criterion table '", crit_table,
             "' has no concept column.", call. = FALSE)
      }
      concept_ids <- .resolveConceptSet(handle, crit$concept_set)
      if (length(concept_ids) == 0) {
        stop("Inclusion concept_set resolved to no concepts.", call. = FALSE)
      }
      sub_where <- c(sub_where, .sqlIdInPredicate(
        paste0("e.", concept_col), concept_ids
      ))
    }

    # Temporal constraints (index-relative window)
    if (!is.null(crit$temporal) && is.null(date_col)) {
      stop("Inclusion criterion table '", crit_table,
           "' has no date column for its temporal constraint.", call. = FALSE)
    }
    if (!is.null(crit$temporal$index_window)) {
      iw <- crit$temporal$index_window
      if (!is.list(iw) || is.null(names(iw)) || any(!nzchar(names(iw))) ||
          anyDuplicated(names(iw)) ||
          length(setdiff(names(iw), c("start", "end"))) > 0L ||
          (is.null(iw$start) && is.null(iw$end))) {
        stop("Inclusion index_window must contain only start and/or end.",
             call. = FALSE)
      }
      normalize_offset <- function(value, label) {
        if (is.null(value)) return(NULL)
        numeric_value <- suppressWarnings(as.numeric(value))
        integer_value <- suppressWarnings(as.integer(value))
        if (length(value) != 1L || length(numeric_value) != 1L ||
            !is.finite(numeric_value) || length(integer_value) != 1L ||
            is.na(integer_value) || numeric_value != integer_value) {
          stop(label, " must be one finite integer day offset.",
               call. = FALSE)
        }
        integer_value
      }
      iw_start <- normalize_offset(iw$start, "index_window$start")
      iw_end <- normalize_offset(iw$end, "index_window$end")
      if (!is.null(iw_start) && !is.null(iw_end) && iw_start > iw_end) {
        stop("index_window$start must not be after index_window$end.",
             call. = FALSE)
      }
      if (!is.null(iw$start)) {
        sub_where <- c(sub_where, paste0(
          "e.", date_col, " >= ",
          .renderSql(handle, "DATEADD(day, @days, c.cohort_start_date)",
                     days = iw_start)
        ))
      }
      if (!is.null(iw$end)) {
        sub_where <- c(sub_where, paste0(
          "e.", date_col, " < ",
          .renderSql(handle, "DATEADD(day, @days, c.cohort_start_date)",
                     days = as.double(iw_end) + 1)
        ))
      }
    }

    # Calendar time constraints
    if (!is.null(crit$temporal$calendar)) {
      cal <- crit$temporal$calendar
      if (!is.list(cal) || is.null(names(cal)) || any(!nzchar(names(cal))) ||
          anyDuplicated(names(cal)) ||
          length(setdiff(names(cal), c("start", "end"))) > 0L ||
          (is.null(cal$start) && is.null(cal$end))) {
        stop("Inclusion calendar must contain only start and/or end.",
             call. = FALSE)
      }
      bounds <- .validateDateBounds(cal$start, cal$end,
                                    "inclusion calendar")
      if (!is.null(cal$start)) {
        sub_where <- c(sub_where, paste0(
          "e.", date_col, " >= ",
          .quoteLiteral(as.character(bounds$start), handle)
        ))
      }
      if (!is.null(cal$end)) {
        sub_where <- c(sub_where, paste0(
          "e.", date_col, " < ",
          .quoteLiteral(as.character(bounds$end + 1L), handle)
        ))
      }
    }

    where_clause <- ""
    if (length(sub_where) > 0) {
      where_clause <- paste0(" AND ", paste(sub_where, collapse = " AND "))
    }

    # Occurrence check
    occ <- crit$occurrence %||% list()
    if (!is.list(occ) ||
        (length(occ) > 0L && (is.null(names(occ)) || any(!nzchar(names(occ))) ||
                              anyDuplicated(names(occ))))) {
      stop("occurrence must be a named list.", call. = FALSE)
    }
    unknown_occ <- setdiff(names(occ) %||% character(0), c("type", "count"))
    if (length(unknown_occ) > 0L) {
      stop("Unknown occurrence field(s): ", paste(unknown_occ, collapse = ", "),
           ".", call. = FALSE)
    }
    occ_type <- occ$type %||% "at_least"
    occ_count_raw <- occ$count %||% 1L
    occ_count_num <- suppressWarnings(as.numeric(occ_count_raw))
    occ_count <- suppressWarnings(as.integer(occ_count_raw))
    if (length(occ_count_raw) != 1L || length(occ_count_num) != 1L ||
        !is.finite(occ_count_num) || length(occ_count) != 1L ||
        is.na(occ_count) || occ_count_num != occ_count || occ_count < 0L) {
      stop("occurrence$count must be one non-negative integer.", call. = FALSE)
    }
    count_op <- switch(occ_type,
      "at_least" = ">=", "at_most" = "<=", "exactly" = "=",
      stop("Unknown occurrence type: '", occ_type, "'.", call. = FALSE))

    # A scalar correlated count correctly handles zero-event persons for
    # at_most/exactly, unlike EXISTS(GROUP BY ... HAVING ...).
    filter_sql <- paste0(
      "SELECT c.subject_id, c.cohort_start_date, c.cohort_end_date ",
      "FROM ", cohort_temp, " AS c ",
      "WHERE (SELECT COUNT(*) FROM ", qualified, " AS e ",
      "WHERE e.person_id = c.subject_id", where_clause, ") ",
      count_op, " ", occ_count
    )

    # Keep every stage name derivable from the original base.  The public return
    # value is therefore exactly `<base>_icN`, which lets the client precompute
    # rollback targets without inspecting server data.
    new_temp <- paste0(base_temp, "_ic", i)
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
    temp_name <- .reserveTempTableName(handle, temp_name)
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
  if (!cohort_table %in% (handle$temp_tables %||% character(0))) {
    stop("Named cohort scopes must be temporary cohorts created by this handle; ",
         "use a numeric cohort_definition_id for a persistent cohort.",
         call. = FALSE)
  }
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
#' @param handle CDM handle (provides the server-side person-key provider and the
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
  frame_contract <- .omopTablePseudonymization(
    x, caller = "omopCohortFromTableDS"
  )
  current_contract <- .canonicalPseudonymizationContract(
    .personKeyPublicContract(handle)
  )
  if (!identical(frame_contract, current_contract)) {
    stop("omopCohortFromTableDS: input uses an incompatible ",
         "pseudonymization contract (key, epoch, protocol, or scope); ",
         "recreate it from the current OMOP handle.", call. = FALSE)
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

  person_key <- .personKey(handle)
  # Reverse tokens -> ORIGINAL ids (server-only). Distinct tokens map 1:1 to
  # distinct ids (the encryption is injective), so the distinct count is the
  # number of real persons; gate on it BEFORE writing any table (fail-closed).
  original_ids <- unique(.unhashPersonKey(tokens, person_key))
  original_ids <- original_ids[!is.na(original_ids)]
  .assertMinPersons(n_persons = length(original_ids))

  bp <- .buildBlueprint(handle)
  temp_name <- if (!is.null(new_name)) {
    candidate <- .validateIdentifier(as.character(new_name), "cohort name")
    if (!grepl("^dsomop_cohort_fromtbl_[A-Za-z0-9_]{4,64}$", candidate)) {
      stop("omopCohortFromTableDS: new_name must use the reserved ",
           "dsomop_cohort_fromtbl_ namespace.", call. = FALSE)
    }
    candidate
  } else {
    paste0("dsomop_cohort_fromtbl_",
           paste0(format(openssl::rand_bytes(8L)), collapse = ""))
  }
  if (temp_name %in% (handle$temp_tables %||% character(0))) {
    stop("omopCohortFromTableDS: new_name is already in use.", call. = FALSE)
  }

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
      "FROM ", obs_qualified, " o WHERE ",
      .sqlIdInPredicate("o.person_id", original_ids)
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
      "FROM ", person_qualified, " p WHERE ",
      .sqlIdInPredicate("p.person_id", original_ids)
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
