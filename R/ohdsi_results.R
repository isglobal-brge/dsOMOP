# Module: OHDSI Results Consumer
# Generic adapter for reading pre-computed results from OHDSI tools
# (CohortDiagnostics, CohortIncidence, Characterization, and others).
# Same pattern as Achilles: we consume existing result tables, not run the tools.

# --- Tool Registry ---

#' Construct one reviewed OHDSI result-table release contract
#'
#' The generic OHDSI consumer is intentionally schema-closed. A table contract
#' names every column that may leave the server, the dimensions that may be used
#' for filtering/ordering, and the row-level person basis required before any
#' statistic is released. Tables without a reviewed public contract remain
#' available only when the server administrator disables strict query mode.
#'
#' @keywords internal
.ohdsiResultContract <- function(public_columns = character(0),
                                  filter_columns = character(0),
                                  count_columns = character(0),
                                  person_columns = character(0),
                                  statistic_columns = character(0),
                                  unit = "admin",
                                  release = "public") {
  normalise <- function(x) unique(tolower(as.character(x %||% character(0))))
  list(
    release = release,
    unit = unit,
    public_columns = normalise(public_columns),
    filter_columns = normalise(filter_columns),
    order_columns = normalise(filter_columns),
    count_columns = normalise(count_columns),
    person_columns = normalise(person_columns),
    statistic_columns = normalise(statistic_columns)
  )
}

#' Reviewed public contracts for pre-computed OHDSI result tables
#'
#' A deliberately small set is public. In particular, a table containing a
#' distribution, effect estimate, rate, or arbitrary metadata is not made safe
#' merely because its name is registered: it also needs a same-row person basis.
#' Unlisted registered tables are filled as \code{admin_only} by
#' \code{\link{.ohdsi_tool_registry}} and fail closed in strict mode.
#'
#' @keywords internal
.ohdsiReviewedContracts <- function() {
  list(
    cohort_diagnostics = list(
      cohort_count = .ohdsiResultContract(
        public_columns = c("cohort_id", "cohort_entries", "cohort_subjects",
                           "database_id"),
        filter_columns = c("cohort_id", "database_id"),
        count_columns = c("cohort_entries", "cohort_subjects"),
        person_columns = "cohort_subjects", unit = "record"
      ),
      index_event_breakdown = .ohdsiResultContract(
        public_columns = c("cohort_id", "concept_id", "concept_name",
                           "domain_field", "concept_count", "subject_count",
                           "database_id"),
        filter_columns = c("cohort_id", "concept_id", "domain_field",
                           "database_id"),
        count_columns = c("concept_count", "subject_count"),
        person_columns = "subject_count", unit = "record"
      ),
      visit_context = .ohdsiResultContract(
        public_columns = c("cohort_id", "visit_concept_id", "visit_context",
                           "subjects", "database_id"),
        filter_columns = c("cohort_id", "visit_concept_id", "database_id"),
        count_columns = "subjects", person_columns = "subjects",
        unit = "person"
      ),
      time_series = .ohdsiResultContract(
        public_columns = c("cohort_id", "calendar_year", "calendar_month",
                           "series_type", "records", "subjects", "database_id"),
        filter_columns = c("cohort_id", "calendar_year", "calendar_month",
                           "series_type", "database_id"),
        count_columns = c("records", "subjects"),
        person_columns = "subjects", unit = "record"
      )
    ),
    cohort_incidence = list(
      incidence_summary = .ohdsiResultContract(
        public_columns = c("target_cohort_definition_id", "outcome_id",
                           "persons_at_risk", "person_outcomes", "outcomes"),
        filter_columns = c("target_cohort_definition_id", "outcome_id"),
        count_columns = c("persons_at_risk", "person_outcomes", "outcomes"),
        person_columns = c("persons_at_risk", "person_outcomes"),
        statistic_columns = character(0),
        unit = "record"
      )
    ),
    characterization = list(
      c_cohort_counts = .ohdsiResultContract(
        public_columns = c("cohort_id", "setting_id", "num_persons",
                           "database_id"),
        filter_columns = c("cohort_id", "setting_id", "database_id"),
        count_columns = "num_persons", person_columns = "num_persons",
        unit = "person"
      ),
      c_dechallenge_rechallenge = .ohdsiResultContract(
        public_columns = c("cohort_id", "dechallenge_stop_interval",
                           "dechallenge_evaluation_window", "num_cases",
                           "num_dechallenge_attempt", "num_dechallenge_success",
                           "num_rechallenge_attempt", "num_rechallenge_success",
                           "database_id"),
        filter_columns = c("cohort_id", "dechallenge_stop_interval",
                           "dechallenge_evaluation_window", "database_id"),
        count_columns = c("num_cases", "num_dechallenge_attempt",
                          "num_dechallenge_success", "num_rechallenge_attempt",
                          "num_rechallenge_success"),
        person_columns = "num_cases", unit = "record"
      )
    ),
    cohort_method = list(
      cm_result = .ohdsiResultContract(
        public_columns = c("analysis_id", "target_id", "comparator_id",
                           "outcome_id", "rr", "ci_95_lb", "ci_95_ub", "p",
                           "log_rr", "se_log_rr", "target_subjects",
                           "comparator_subjects", "target_outcomes",
                           "comparator_outcomes"),
        filter_columns = c("analysis_id", "target_id", "comparator_id",
                           "outcome_id"),
        count_columns = c("target_subjects", "comparator_subjects",
                          "target_outcomes", "comparator_outcomes"),
        person_columns = c("target_subjects", "comparator_subjects"),
        statistic_columns = c("rr", "ci_95_lb", "ci_95_ub", "p",
                               "log_rr", "se_log_rr"),
        unit = "record"
      )
    ),
    sccs = list(
      sccs_result = .ohdsiResultContract(
        public_columns = c("analysis_id", "exposures_outcome_set_id",
                           "covariate_id", "rr", "ci_95_lb", "ci_95_ub", "p",
                           "log_rr", "se_log_rr", "outcome_subjects",
                           "outcome_events"),
        filter_columns = c("analysis_id", "exposures_outcome_set_id",
                           "covariate_id"),
        count_columns = c("outcome_subjects", "outcome_events"),
        person_columns = "outcome_subjects",
        statistic_columns = c("rr", "ci_95_lb", "ci_95_ub", "p",
                               "log_rr", "se_log_rr"),
        unit = "record"
      )
    ),
    plp = list(
      plp_performances = .ohdsiResultContract(
        public_columns = c("model_design_id", "development_database_id",
                           "validation_database_id", "auc", "auprc",
                           "population_size", "outcome_count",
                           "calibration_in_large", "calibration_intercept",
                           "calibration_slope"),
        filter_columns = c("model_design_id", "development_database_id",
                           "validation_database_id"),
        count_columns = c("population_size", "outcome_count"),
        person_columns = "population_size",
        statistic_columns = c("auc", "auprc", "calibration_in_large",
                               "calibration_intercept", "calibration_slope"),
        unit = "record"
      )
    )
  )
}

#' Static registry of known OHDSI result tools
#'
#' Each entry declares the tool's result table names, count columns that need
#' disclosure control, sensitive columns that must never be returned, and the
#' definition/reference metadata tables, and a reviewed release contract for
#' each public table. Metadata classification is descriptive only: an
#' allowlisted table with neither a count nor a person-basis column is never
#' assumed safe.
#'
#' @return Named list keyed by tool_id
#' @keywords internal
.ohdsi_tool_registry <- function() {
  registry <- list(
    cohort_diagnostics = list(
      tool_name = "CohortDiagnostics",
      table_names = c("cohort_count", "incidence_rate", "index_event_breakdown",
                       "visit_context", "temporal_covariate_value",
                       "temporal_covariate_value_dist", "time_series",
                       "included_source_concept", "orphan_concept",
                       "concept_sets", "resolved_concepts"),
      prefix_patterns = c("^cd_"),
      count_columns = c("cohort_entries", "cohort_subjects", "cohort_count",
                         "person_count", "concept_count", "concept_subjects",
                         "subject_count", "subjects", "records",
                         "sum_value", "count_value"),
      person_columns = c("cohort_subjects", "person_count", "concept_subjects",
                          "subject_count", "subjects"),
      sensitive_columns = c("json", "sql", "concept_set_sql"),
      metadata_tables = c("concept_sets", "resolved_concepts")
    ),

    cohort_incidence = list(
      tool_name = "CohortIncidence",
      table_names = c("incidence_summary", "target_def", "outcome_def", "tar_def"),
      prefix_patterns = c("^ci_"),
      count_columns = c("persons_at_risk_pe", "persons_at_risk",
                         "person_outcomes_pe", "person_outcomes",
                         "outcomes_pe", "outcomes"),
      person_columns = c("persons_at_risk_pe", "persons_at_risk",
                          "person_outcomes_pe", "person_outcomes"),
      sensitive_columns = character(0),
      metadata_tables = c("target_def", "outcome_def", "tar_def")
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
      person_columns = c("num_persons", "num_cases", "num_persons_exposed"),
      sensitive_columns = character(0),
      metadata_tables = c("c_analysis_ref", "c_covariate_ref", "c_settings")
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
                         "target_days", "comparator_days",
                         "subjects", "count_value", "exposure_subjects"),
      person_columns = c("target_subjects", "comparator_subjects",
                          "exposure_subjects", "subjects"),
      sensitive_columns = character(0),
      metadata_tables = character(0)
    ),

    sccs = list(
      tool_name = "Self-Controlled Case Series",
      table_names = c("sccs_result", "sccs_diagnostics_summary",
                       "sccs_attrition", "sccs_covariate_result"),
      prefix_patterns = c("^sccs_"),
      count_columns = c("outcome_subjects", "outcome_events",
                         "outcome_observation_periods", "observed_days",
                         "subjects", "count_value"),
      person_columns = c("outcome_subjects", "subjects"),
      sensitive_columns = character(0),
      metadata_tables = character(0)
    ),

    plp = list(
      tool_name = "Patient-Level Prediction",
      table_names = c("plp_performances", "plp_covariate_summary",
                       "plp_diagnostic_summary", "plp_calibration_summary",
                       "plp_threshold_summary", "plp_model_design",
                       "plp_attrition"),
      prefix_patterns = c("^plp_"),
      count_columns = c("population_size", "outcome_count", "test_size",
                         "train_size", "n_total", "subjects", "count_value"),
      person_columns = c("population_size", "test_size", "train_size",
                          "subjects"),
      sensitive_columns = character(0),
      metadata_tables = c("plp_model_design")
    ),

    evidence_synthesis = list(
      tool_name = "Evidence Synthesis",
      table_names = c("es_cm_result", "es_sccs_result",
                       "es_cm_diagnostics_summary", "es_sccs_diagnostics_summary"),
      prefix_patterns = c("^es_"),
      count_columns = c("n_databases"),
      # Cross-database meta-analysis counts; no per-person basis in the result.
      person_columns = character(0),
      sensitive_columns = character(0),
      metadata_tables = character(0)
    )
  )

  reviewed <- .ohdsiReviewedContracts()
  for (tool_id in names(registry)) {
    contracts <- reviewed[[tool_id]] %||% list()
    for (table_name in registry[[tool_id]]$table_names) {
      if (is.null(contracts[[table_name]])) {
        contracts[[table_name]] <- .ohdsiResultContract(
          unit = "admin", release = "admin_only"
        )
      }
    }
    registry[[tool_id]]$contracts <-
      contracts[registry[[tool_id]]$table_names]
  }
  registry
}

#' Match a table against one tool's explicit table allowlist
#'
#' A prefix is accepted only when it is itself registered for the tool and,
#' after removing it, the remainder is an exact registered table name.  This
#' keeps prefix support without turning a tool id into a blanket authorization.
#'
#' @param table_name Character; candidate table name.
#' @param tool Registry entry.
#' @return Character vector containing the canonical registered table name, or
#'   an empty vector when the table does not belong to the tool.
#' @keywords internal
.ohdsi_table_matches_tool <- function(table_name, tool) {
  tbl_lower <- tolower(table_name)
  allowed <- tolower(tool$table_names %||% character(0))
  matched <- allowed[allowed == tbl_lower]

  for (pat in tool$prefix_patterns %||% character(0)) {
    stripped <- sub(pat, "", tbl_lower)
    if (!identical(stripped, tbl_lower) && stripped %in% allowed) {
      matched <- c(matched, stripped)
    }
  }

  unique(matched)
}

#' Map a table name to its tool_id
#'
#' Checks exact name match first, then prefix patterns.
#' @param table_name Character; the table name to look up
#' @return Character tool_id, or NULL if no match
#' @keywords internal
.ohdsi_table_to_tool <- function(table_name) {
  registry <- .ohdsi_tool_registry()
  owners <- names(registry)[vapply(registry, function(tool) {
    length(.ohdsi_table_matches_tool(table_name, tool)) == 1L
  }, logical(1))]

  # Never resolve an ambiguous table by registry order.
  if (length(owners) == 1L) owners else NULL
}

#' Remove unreviewed result tables from strict public inventories
#'
#' Discovery itself remains complete for server administration and blueprint
#' construction. Public status/listing consumers call this helper so a table
#' that cannot be queried under a public contract is not advertised as a
#' disclosure-safe analysis surface.
#'
#' @keywords internal
.ohdsiFilterPublicInventory <- function(found) {
  if (!is.data.frame(found) || nrow(found) == 0L ||
      !isTRUE(.omopDisclosureSettings()$query_strict)) {
    return(found)
  }
  registry <- .ohdsi_tool_registry()
  keep <- vapply(seq_len(nrow(found)), function(i) {
    tool_id <- found$tool_id[[i]]
    if (!tool_id %in% names(registry)) return(FALSE)
    canonical <- .ohdsi_table_matches_tool(
      found$table_name[[i]], registry[[tool_id]]
    )
    length(canonical) == 1L &&
      identical(registry[[tool_id]]$contracts[[canonical]]$release, "public")
  }, logical(1))
  found[keep, , drop = FALSE]
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

  schema <- .effectiveResultsSchema(handle)
  db_tables <- .listTablesRaw(handle, schema)
  registry <- .ohdsi_tool_registry()
  rows <- list()

  for (actual_name in db_tables) {
    tid <- .ohdsi_table_to_tool(actual_name)
    if (is.null(tid)) next
    tool <- registry[[tid]]
    qualified <- .qualifyTable(handle, actual_name, schema)
    n <- tryCatch({
      sql <- paste0("SELECT COUNT(*) AS n FROM ", qualified)
      as.integer(.executeQuery(handle, sql)$n[1])
    }, error = function(e) 0L)
    rows[[length(rows) + 1L]] <- data.frame(
      table_name = actual_name, tool_id = tid,
      tool_name = tool$tool_name, qualified_name = qualified,
      n_rows = n, stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) return(empty)
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

#' Protect inventory row counts returned by OHDSI discovery endpoints
#'
#' Result-table row counts are operational metadata rather than patient counts,
#' but exact values still provide a differencing oracle. Suppress small values
#' and floor all other counts to the configured disclosure band.
#'
#' @param x Numeric vector of raw row counts.
#' @return Numeric vector containing only suppressed or banded counts.
#' @keywords internal
.ohdsiBandInventoryCounts <- function(x) {
  settings <- .omopDisclosureSettings()
  x <- suppressWarnings(as.numeric(x))
  vapply(x, function(n) {
    if (is.na(n) || n < settings$nfilter_tab) return(NA_real_)
    .bandCount(n, settings$nfilter_band)
  }, numeric(1))
}

# --- Count Column Detection ---

#' Return lower-case columns already discovered by the CDM blueprint
#'
#' This avoids issuing a dialect-specific \code{SELECT * ... LIMIT 0} merely to
#' inspect a result-table schema. The blueprint uses DBI metadata introspection
#' and is already restricted to the handle's authorised schemas.
#'
#' @param bp Blueprint environment.
#' @param table_name Result-table name.
#' @param handle Optional CDM handle for DBI metadata fallback when a result
#'   table was discovered after the blueprint's CDM column pass.
#' @keywords internal
.ohdsiBlueprintColumns <- function(bp, table_name, handle = NULL) {
  keys <- names(bp$columns %||% list())
  idx <- match(tolower(table_name), tolower(keys))
  if (!is.na(idx)) {
    info <- bp$columns[[keys[[idx]]]]
    cols <- unique(tolower(as.character(
      info$column_name %||% character(0)
    )))
    if (length(cols) > 0L) return(cols)
  }
  if (is.null(handle)) return(character(0))
  schema <- .effectiveResultsSchema(handle)
  info <- tryCatch(
    .listColumnsRaw(handle, table_name, schema),
    error = function(e) NULL
  )
  unique(tolower(as.character(info$column_name %||% character(0))))
}

#' Detect count columns needing disclosure control
#'
#' Detection is the UNION of a registry lookup and a heuristic pattern match
#' on column names. A registry match never short-circuits the heuristic, so a
#' table with one registered count column is still scanned for other count-like
#' columns (e.g. subject_tally, denom) that would otherwise leak raw small
#' counts.
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

  # Get actual columns in the table (shared by both detection paths)
  bp <- .buildBlueprint(handle)
  actual_cols <- .ohdsiBlueprintColumns(bp, table_name, handle)

  # Registry lookup
  registry_matched <- character(0)
  if (!is.null(tool_id) && tool_id %in% names(registry)) {
    registered <- registry[[tool_id]]$count_columns
    registry_matched <- intersect(tolower(registered), tolower(actual_cols))
  }

  # Heuristic pattern matching (always runs, never short-circuited)
  pattern <- "^n_|^num_|_count$|^count$|_subjects$|^subjects$|_persons$|_records$|_entries$|_outcomes$|^outcomes$|^persons_at_risk|^sum_value$|^count_value$"
  heuristic_matched <- grep(pattern, actual_cols, value = TRUE, ignore.case = TRUE)

  union(registry_matched, tolower(heuristic_matched))
}

# --- Person Gate ---

#' Fail-closed distinct-person gate for a pre-computed OHDSI result
#'
#' Pre-computed OHDSI result tables hold no \code{person_id}, so the
#' distinct-person gate that protects raw-CDM aggregates (\code{.assertMinPersons}
#' / \code{.achillesPersonGate}) cannot count individuals directly. The per-row
#' person basis is instead the tool's PERSON-count column(s) — \code{num_persons},
#' \code{cohort_subjects}, \code{persons_at_risk}, \code{target_subjects}, etc.
#' (declared as \code{person_columns} in the registry). A small such value means
#' the row describes too few individuals, even when its record/event/outcome
#' counts are large, exactly the gap \code{.suppressSmallCounts} (a small-CELL
#' control) leaves open.
#'
#' Behaviour, mirroring the "no hints" fail-closed policy:
#' \itemize{
#'   \item rows whose person count is below \code{nfilter.subset} (or NA) are
#'     DROPPED — the same threshold and row-drop semantics as the raw-CDM gate;
#'   \item if the result carries count columns but NONE of the tool's person
#'     columns (a person-less count basis, e.g. record/event/outcome counts with
#'     no person denominator), it is rejected (emptied) when
#'     \code{query_strict} is TRUE, and passed through (cell-suppressed only)
#'     when strict mode is off;
#'   \item a result with no count/person basis at all — including definition or
#'     configuration metadata — is rejected in strict mode. Arbitrary names,
#'     JSON, SQL, and fitted-model configuration are not assumed public merely
#'     because they contain no patient count.
#' }
#'
#' @param result Data frame already returned + cell-suppressed by
#'   \code{.ohdsiGetResults}.
#' @param tool_id Character; resolved tool id (registry key).
#' @param count_cols Character; the count columns declared for this result.
#' @param table_name Optional canonical table name used to resolve its reviewed
#'   person-basis contract.
#' @param contract Optional already-resolved table contract.
#' @return \code{result} with disclosive rows removed (or emptied, fail-closed).
#' @keywords internal
.ohdsiPersonGate <- function(result, tool_id, count_cols, table_name = NULL,
                             contract = NULL) {
  if (nrow(result) == 0) return(result)

  registry <- .ohdsi_tool_registry()
  person_cols <- character(0)
  if (!is.null(contract)) {
    person_cols <- contract$person_columns %||% character(0)
  } else if (!is.null(table_name) && !is.null(tool_id) &&
             tool_id %in% names(registry)) {
    canonical <- .ohdsi_table_matches_tool(table_name, registry[[tool_id]])
    if (length(canonical) == 1L) {
      person_cols <-
        registry[[tool_id]]$contracts[[canonical]]$person_columns %||%
        character(0)
    }
  } else if (!is.null(tool_id) && tool_id %in% names(registry)) {
    person_cols <- registry[[tool_id]]$person_columns %||% character(0)
  }
  present_person <- intersect(tolower(person_cols), names(result))

  # No count/person basis is not a public contract. Preserve the legacy frame
  # only in explicitly non-strict administrator/development mode.
  if (length(intersect(count_cols, names(result))) == 0) {
    if (isTRUE(.omopDisclosureSettings()$query_strict)) {
      return(result[0, , drop = FALSE])
    }
    return(result)
  }

  # Count basis present but NO person basis: reject fail-closed in strict mode.
  if (length(present_person) == 0) {
    if (isTRUE(.omopDisclosureSettings()$query_strict)) {
      return(result[0, , drop = FALSE])
    }
    return(result)
  }

  # Drop every row whose person basis is below nfilter.subset (NA -> dropped),
  # the same threshold + row-drop the raw-CDM distinct-person gate enforces.
  threshold <- .omopDisclosureSettings()$nfilter_subset
  keep <- rep(TRUE, nrow(result))
  for (col in present_person) {
    vals <- suppressWarnings(as.numeric(result[[col]]))
    keep <- keep & (!is.na(vals) & vals >= threshold)
  }
  gated <- result[keep, , drop = FALSE]
  band_cols <- intersect(unique(c(count_cols, present_person)), names(gated))
  band_width <- .omopDisclosureSettings()$nfilter_band
  for (col in band_cols) {
    gated[[col]] <- vapply(
      gated[[col]], .bandCount, numeric(1), band_width = band_width
    )
  }
  rownames(gated) <- NULL
  gated
}

# --- Generic Query ---

#' Resolve one reviewed physical OHDSI result-table contract
#'
#' Keeps physical result-table ownership separate from analysis-catalog live
#' overlays. The returned canonical name and disclosure contract are inert
#' metadata; no query is executed.
#'
#' @param table_name Character; requested physical OHDSI result-table name.
#' @param tool_id Optional character tool identifier.
#' @return Named resolution metadata and the reviewed disclosure contract.
#' @keywords internal
.ohdsiResolveResultContract <- function(table_name, tool_id = NULL) {
  if (!is.character(table_name) || length(table_name) != 1L ||
      is.na(table_name) || !nzchar(table_name)) {
    stop("table_name must be one non-empty table identifier.", call. = FALSE)
  }
  table_name <- .validateIdentifier(table_name, "table")
  registry <- .ohdsi_tool_registry()
  owners <- names(registry)[vapply(registry, function(tool) {
    length(.ohdsi_table_matches_tool(table_name, tool)) == 1L
  }, logical(1L))]

  if (is.null(tool_id)) {
    if (length(owners) == 0L) {
      stop("Table '", table_name,
           "' is not a registered OHDSI result table. Only allowlisted OHDSI ",
           "result tables may be queried.", call. = FALSE)
    }
    if (length(owners) != 1L) {
      stop("Table '", table_name,
           "' has ambiguous OHDSI tool ownership and cannot be queried.",
           call. = FALSE)
    }
    tool_id <- owners[[1L]]
  } else {
    if (!is.character(tool_id) || length(tool_id) != 1L || is.na(tool_id) ||
        !nzchar(tool_id)) {
      stop("tool_id must be one registered OHDSI tool identifier.",
           call. = FALSE)
    }
    tool_id <- tolower(tool_id)
    if (!tool_id %in% names(registry)) {
      stop("Unknown OHDSI tool_id: '", tool_id, "'.", call. = FALSE)
    }
    if (!tool_id %in% owners) {
      stop("Table '", table_name, "' is not registered for OHDSI tool '",
           tool_id, "'.", call. = FALSE)
    }
  }

  canonical <- .ohdsi_table_matches_tool(table_name, registry[[tool_id]])
  if (length(canonical) != 1L) {
    stop("Table '", table_name,
         "' is not a uniquely registered OHDSI result table.", call. = FALSE)
  }
  contract <- registry[[tool_id]]$contracts[[canonical[[1L]]]]
  strict <- isTRUE(.omopDisclosureSettings()$query_strict)
  if (strict && (!is.list(contract) ||
                 !identical(contract$release, "public"))) {
    stop("Table '", table_name,
         "' has no reviewed public disclosure contract. It is available only ",
         "to administrators in non-strict development mode.", call. = FALSE)
  }
  list(
    requested_table = tolower(table_name), canonical_table = canonical[[1L]],
    tool_id = tool_id, tool = registry[[tool_id]], contract = contract,
    strict = strict
  )
}

#' Resolve the requested physical table in the authorised results namespace
#'
#' Registry resolution above establishes the canonical OHDSI contract. This
#' helper separately proves that the exact requested physical table exists in
#' the controller-authorised results namespace. The distinction matters for
#' standard tool prefixes such as \code{cd_cohort_count}: the canonical contract
#' is \code{cohort_count}, while SQL must address the prefixed physical table.
#'
#' @param handle CDM handle.
#' @param resolved Result from \code{\link{.ohdsiResolveResultContract}}.
#' @return Named physical table, qualified name, and cached blueprint.
#' @keywords internal
.ohdsiResolvePhysicalResultTable <- function(handle, resolved) {
  bp <- .buildBlueprint(handle)
  table_name <- resolved$requested_table
  authorised_schema <- .effectiveResultsSchema(handle)

  # Blueprint rows carry the authoritative schema classification for tables it
  # knows. Preserve that fail-closed check, including for co-located results.
  bp_match <- bp$tables[
    tolower(bp$tables$table_name) == table_name & bp$tables$present_in_db,
    , drop = FALSE
  ]
  if (nrow(bp_match) > 0L) {
    if (nrow(bp_match) != 1L ||
        !tolower(bp_match$schema_category[[1L]]) %in% c("result", "results")) {
      stop("Table '", table_name,
           "' is not available from an authorized OHDSI results schema.",
           call. = FALSE)
    }
    actual_table <- tolower(bp_match$table_name[[1L]])
    qualified <- bp_match$qualified_name[[1L]]
    expected <- .qualifyTable(handle, actual_table, authorised_schema)
    if (!identical(tolower(qualified), tolower(expected))) {
      stop("Table '", table_name,
           "' is not available from an authorized OHDSI results schema.",
           call. = FALSE)
    }
    return(list(table_name = actual_table, qualified_name = qualified,
                blueprint = bp))
  }

  # Prefixed OHDSI result tables are intentionally not part of the CDM
  # blueprint specification. Resolve them by exact name, but only inside the
  # already-authorised results namespace; never scan or fall back to another
  # schema.
  available <- tryCatch(
    tolower(.listTablesRaw(handle, authorised_schema)),
    error = function(e) character(0)
  )
  matches <- available[available == table_name]
  if (length(matches) == 0L) {
    stop("Table '", table_name, "' not found in database.", call. = FALSE)
  }
  if (length(matches) != 1L) {
    stop("Table '", table_name,
         "' is not uniquely available from the authorized OHDSI results ",
         "schema.", call. = FALSE)
  }
  actual_table <- matches[[1L]]
  list(
    table_name = actual_table,
    qualified_name = .qualifyTable(handle, actual_table, authorised_schema),
    blueprint = bp
  )
}

#' Return the typed pooling contract for a physical OHDSI result table
#'
#' @param handle CDM handle.
#' @param table_name Character; requested physical OHDSI result-table name.
#' @param tool_id Optional character tool identifier.
#' @return Closed versioned pooling-contract metadata.
#' @keywords internal
.ohdsiResultPoolingContract <- function(handle, table_name, tool_id = NULL) {
  resolved <- .ohdsiResolveResultContract(table_name, tool_id)
  if (!isTRUE(resolved$strict)) {
    stop("Physical OHDSI pooling is available only for reviewed public ",
         "contracts in strict mode.", call. = FALSE)
  }
  physical <- .ohdsiResolvePhysicalResultTable(handle, resolved)
  actual_cols <- .ohdsiBlueprintColumns(
    physical$blueprint, physical$table_name, handle
  )
  required_basis <- unique(c(resolved$contract$count_columns,
                             resolved$contract$person_columns))
  missing_basis <- setdiff(required_basis, actual_cols)
  if (length(resolved$contract$person_columns) == 0L ||
      length(missing_basis) > 0L) {
    stop("Table '", resolved$requested_table,
         "' does not provide the complete contracted person/count basis",
         if (length(missing_basis) > 0L) paste0(": ",
           paste(missing_basis, collapse = ", ")) else "",
         ".", call. = FALSE)
  }
  entry <- list(meta = list(
    tool_id = resolved$tool_id,
    table_name = resolved$canonical_table,
    public_vocabulary_metadata = FALSE
  ))
  pooling_contract <- .omopAnalysisValidatePoolingContract(
    .omopOhdsiPrecomputedPoolingContract(entry),
    paste0("physical OHDSI result ", resolved$tool_id, ".",
           resolved$canonical_table)
  )
  roles <- vapply(pooling_contract$columns, `[[`, character(1L), "role")
  required_pooling_columns <- names(roles)[
    !roles %in% c("label", "nonpoolable", "ratio")
  ]
  if (identical(pooling_contract$strategy, "effect_estimate")) {
    required_pooling_columns <- union(
      required_pooling_columns,
      c(pooling_contract$log_estimate, pooling_contract$standard_error)
    )
  }
  missing_pooling <- setdiff(required_pooling_columns, actual_cols)
  if (length(missing_pooling) > 0L) {
    stop("Table '", resolved$requested_table,
         "' does not provide the complete contracted pooling schema: ",
         paste(missing_pooling, collapse = ", "), ".", call. = FALSE)
  }
  list(
    contract_version = 1L,
    tool_id = resolved$tool_id,
    table_name = resolved$canonical_table,
    pooling_contract = pooling_contract
  )
}

#' Query an OHDSI result table
#'
#' @param handle CDM handle
#' @param table_name Character; table to query (validated via .validateIdentifier)
#' @param columns Character vector; reviewed columns to SELECT (NULL = every
#'   public column in the table contract).
#' @param filters Named list; WHERE conditions (name = column, value = filter value)
#' @param order_by Character; ORDER BY clause column name(s)
#' @param limit Integer; max rows (capped at 5000)
#' @param tool_id Character; optional tool_id for registry lookup
#' @return A disclosure-controlled data frame in strict mode. In non-strict
#'   administrator/development mode the frame is marked with
#'   \code{dsomop.disclosure_safe = FALSE}.
#' @keywords internal
.ohdsiGetResults <- function(handle, table_name, columns = NULL,
                              filters = NULL, order_by = NULL,
                              limit = 5000L, tool_id = NULL) {
  resolved <- .ohdsiResolveResultContract(table_name, tool_id)
  table_name <- resolved$requested_table
  canonical_table <- resolved$canonical_table
  tool_id <- resolved$tool_id
  registry <- .ohdsi_tool_registry()
  contract <- resolved$contract
  strict <- resolved$strict
  finish <- function(x) {
    if (strict && is.null(columns) && is.data.frame(x)) {
      for (column in setdiff(contract$public_columns, names(x))) {
        x[[column]] <- rep(NA, nrow(x))
      }
      x <- x[, contract$public_columns, drop = FALSE]
    }
    if (!strict) {
      attr(x, "dsomop.disclosure_safe") <- FALSE
      attr(x, "dsomop.release_mode") <- "admin_development"
    }
    x
  }

  # Resolve only the exact requested physical table in the authorised results
  # namespace. Canonical and prefixed tool tables share the same reviewed
  # contract but remain distinct physical objects.
  physical <- .ohdsiResolvePhysicalResultTable(handle, resolved)
  bp <- physical$blueprint
  qualified <- physical$qualified_name
  actual_table <- physical$table_name

  # Get sensitive columns to exclude
  sensitive <- character(0)
  if (!is.null(tool_id) && tool_id %in% names(registry)) {
    sensitive <- registry[[tool_id]]$sensitive_columns
  }

  # DBI metadata captured by the blueprint is the sole source of schema
  # introspection here: never SELECT * from a result table.
  actual_cols <- .ohdsiBlueprintColumns(bp, actual_table, handle)
  if (length(actual_cols) == 0L) {
    stop("Could not inspect columns for OHDSI result table '", table_name,
         "'.", call. = FALSE)
  }

  if (strict) {
    table_count_cols <- contract$count_columns
    table_person_cols <- contract$person_columns
    required_basis <- unique(c(table_count_cols, table_person_cols))
    missing_basis <- setdiff(required_basis, actual_cols)
    if (length(table_person_cols) == 0L || length(missing_basis) > 0L) {
      stop("Table '", table_name,
           "' does not provide the complete contracted person/count basis",
           if (length(missing_basis) > 0L) paste0(": ",
             paste(missing_basis, collapse = ", ")) else "",
           ".", call. = FALSE)
    }
    if (length(contract$statistic_columns) > 0L &&
        length(table_person_cols) == 0L) {
      stop("Table '", table_name,
           "' contains statistics without a contracted person basis.",
           call. = FALSE)
    }
  } else {
    # Explicitly unsafe administrator/development compatibility path. The
    # heuristic is retained only here for inspecting legacy OHDSI schemas.
    table_count_cols <- .ohdsiDetectCountColumns(handle, table_name, tool_id)
    table_person_cols <- intersect(
      tolower(registry[[tool_id]]$person_columns %||% character(0)),
      actual_cols
    )
    table_count_cols <- union(
      intersect(tolower(table_count_cols), actual_cols),
      table_person_cols
    )
  }

  # Build SELECT columns
  if (is.null(columns)) {
    select_cols <- if (strict) {
      intersect(contract$public_columns, actual_cols)
    } else {
      setdiff(actual_cols, tolower(sensitive))
    }
  } else {
    if (!is.character(columns) || length(columns) == 0L ||
        anyNA(columns) || any(!nzchar(columns))) {
      stop("columns must be a non-empty character vector.", call. = FALSE)
    }
    columns <- tolower(columns)
    for (col in columns) .validateIdentifier(col, "column")
    if (any(columns %in% tolower(sensitive))) {
      stop("Sensitive OHDSI result columns cannot be selected.", call. = FALSE)
    }
    unavailable <- setdiff(columns, actual_cols)
    if (length(unavailable) > 0L) {
      stop("Unknown OHDSI result column(s): ",
           paste(unavailable, collapse = ", "), ".", call. = FALSE)
    }
    if (strict) {
      uncontracted <- setdiff(columns, contract$public_columns)
      if (length(uncontracted) > 0L) {
        stop("OHDSI result column(s) have no public disclosure contract: ",
             paste(uncontracted, collapse = ", "), ".", call. = FALSE)
      }
    }
    select_cols <- unique(columns)
  }

  if (length(select_cols) == 0) {
    if (strict) {
      stop("Table '", table_name,
           "' has no contracted columns present in this schema.",
           call. = FALSE)
    }
    return(finish(data.frame(stringsAsFactors = FALSE)))
  }

  # Always fetch the disclosure-basis columns internally.  Otherwise a caller
  # could project them away and bypass row suppression, then infer small cells
  # from the remaining dimensions.  Hidden basis columns are removed below.
  output_cols <- select_cols
  query_cols <- unique(c(select_cols, table_count_cols, table_person_cols))

  # Express the row cap in OHDSI/SQL Server form, then translate it through the
  # package's dialect layer (TOP for SQL Server, FETCH FIRST for Oracle, LIMIT
  # for the remaining supported dialects).
  limit_num <- suppressWarnings(as.numeric(limit))
  if (length(limit_num) != 1L || is.na(limit_num) || !is.finite(limit_num) ||
      limit_num != floor(limit_num) || limit_num < 1L) {
    stop("limit must be one positive integer.", call. = FALSE)
  }
  limit <- min(as.integer(limit_num), 5000L)
  sql <- paste0("SELECT TOP ", limit, " ",
                paste(query_cols, collapse = ", "), " FROM ", qualified)

  # WHERE clause
  where_parts <- character(0)
  if (!is.null(filters) && length(filters) > 0) {
    if (!is.list(filters) || is.null(names(filters)) ||
        any(!nzchar(names(filters))) || anyDuplicated(names(filters))) {
      stop("filters must be a uniquely named list.", call. = FALSE)
    }
    for (col_name in names(filters)) {
      .validateIdentifier(col_name, "filter column")
      val <- filters[[col_name]]
      col_lower <- tolower(col_name)
      if (!col_lower %in% actual_cols || col_lower %in% tolower(sensitive)) {
        stop("Filter column '", col_name,
             "' is unavailable or sensitive.", call. = FALSE)
      }
      if (strict && !col_lower %in% contract$filter_columns) {
        stop("Filter column '", col_name,
             "' has no public disclosure contract.", call. = FALSE)
      }
      if (length(val) != 1L || is.na(val)) {
        stop("Each result filter must contain one non-missing scalar.",
             call. = FALSE)
      }
      where_parts <- c(where_parts, paste0(
        col_lower, " = ", .quoteLiteral(val, handle)
      ))
    }
    sql <- paste0(sql, " WHERE ", paste(where_parts, collapse = " AND "))
  }

  # ORDER BY
  if (!is.null(order_by)) {
    if (!is.character(order_by) || length(order_by) != 1L ||
        is.na(order_by) || !nzchar(trimws(order_by))) {
      stop("order_by must be one column with optional ASC or DESC.",
           call. = FALSE)
    }
    order_by <- trimws(order_by)
    if (!grepl("^[A-Za-z_][A-Za-z0-9_]*( (ASC|DESC))?$", order_by,
               ignore.case = TRUE)) {
      stop("order_by must be one column with optional ASC or DESC.",
           call. = FALSE)
    }
    order_col <- sub(" (ASC|DESC)$", "", order_by, ignore.case = TRUE)
    .validateIdentifier(order_col, "order column")
    if (!tolower(order_col) %in% actual_cols) {
      stop("Unknown order column.", call. = FALSE)
    }
    if (strict && !tolower(order_col) %in% contract$order_columns) {
      stop("Order column '", order_col,
           "' has no public disclosure contract.", call. = FALSE)
    }
    sql <- paste0(sql, " ORDER BY ", tolower(order_by))
  }
  sql <- .sql_translate(sql, handle$target_dialect)

  # Execute
  result <- tryCatch(
    .executeQuery(handle, sql),
    error = function(e) {
      stop("OHDSI result query failed for the contracted table.",
           call. = FALSE)
    }
  )
  if (nrow(result) == 0) {
    return(finish(
      result[, intersect(output_cols, names(result)), drop = FALSE]
    ))
  }

  # Disclosure control on count columns (small-CELL suppression).
  count_cols <- intersect(table_count_cols, names(result))
  if (length(count_cols) > 0) {
    result <- .suppressSmallCounts(result, count_cols)
  }

  # Fail-closed distinct-person gate (orthogonal to cell suppression): drop rows
  # backed by too few PERSONS, and reject a person-less count basis in strict
  # mode. Mirrors .achillesPersonGate / .assertMinPersons for pre-computed tables.
  result <- .ohdsiPersonGate(result, tool_id, count_cols,
                             table_name = canonical_table,
                             contract = if (strict) contract else NULL)

  finish(result[, intersect(output_cols, names(result)), drop = FALSE])
}

# --- Status ---

#' Check OHDSI result tool availability
#'
#' @param handle CDM handle
#' @return Named list with per-tool availability
#' @keywords internal
.ohdsiStatus <- function(handle) {
  found <- .ohdsiFilterPublicInventory(.ohdsiFindResultTables(handle))

  registry <- .ohdsi_tool_registry()
  status <- list()

  for (tid in names(registry)) {
    tool_tables <- found[found$tool_id == tid, , drop = FALSE]
    status[[tid]] <- list(
      tool_name = registry[[tid]]$tool_name,
      available = nrow(tool_tables) > 0,
      tables = tool_tables$table_name,
      n_tables = nrow(tool_tables),
      total_rows = if (nrow(tool_tables) == 0L) 0 else
        .ohdsiBandInventoryCounts(sum(tool_tables$n_rows))[[1]]
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

  found <- .ohdsiFilterPublicInventory(.ohdsiFindResultTables(handle))
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
    tables = within(
      tool_tables[, c("table_name", "n_rows"), drop = FALSE],
      n_rows <- .ohdsiBandInventoryCounts(n_rows)
    )
  )
}
