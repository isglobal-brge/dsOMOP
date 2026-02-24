# ==============================================================================
# dsOMOP v2 - DataSHIELD Exposed Methods
# ==============================================================================
# All DataSHIELD assign/aggregate methods. Thin wrappers around internal
# functions. Session handle management.
# ==============================================================================

# --- Handle management ---

.getHandle <- function(symbol) {
  key <- paste0("handle_", symbol)
  if (!exists(key, envir = .dsomop_env)) {
    stop("No OMOP handle for symbol '", symbol,
         "'. Call omopInitDS first.", call. = FALSE)
  }
  get(key, envir = .dsomop_env)
}

.setHandle <- function(symbol, handle) {
  key <- paste0("handle_", symbol)
  assign(key, handle, envir = .dsomop_env)
}

.removeHandle <- function(symbol) {
  key <- paste0("handle_", symbol)
  if (exists(key, envir = .dsomop_env)) {
    handle <- get(key, envir = .dsomop_env)
    .closeHandle(handle)
    rm(list = key, envir = .dsomop_env)
  }
}

# ==============================================================================
# ASSIGN METHODS (5)
# ==============================================================================

#' Initialize an OMOP CDM handle (Assign)
#'
#' @param resource_symbol Character; the resource symbol name
#' @param cdm_schema Character; override CDM schema
#' @param results_schema Character; results schema name
#' @param vocab_schema Character; vocabulary schema name
#' @param temp_schema Character; temp schema name
#' @param config Named list; additional configuration
#' @return The handle symbol (assigned server-side)
#' @export
omopInitDS <- function(resource_symbol,
                       cdm_schema = NULL,
                       results_schema = NULL,
                       vocab_schema = NULL,
                       temp_schema = NULL,
                       config = list()) {
  resolved <- eval(parse(text = resource_symbol),
                   envir = parent.frame())

  # DSLite resolves resources to ResourceClient objects during assign.resource;
  # Opal passes raw resource objects. Handle both cases.
  if (inherits(resolved, "ResourceClient")) {
    resource_client <- resolved
  } else {
    resource_client <- resourcer::newResourceClient(resolved)
  }

  handle <- .createHandle(
    resource_client,
    cdm_schema = cdm_schema,
    vocab_schema = vocab_schema,
    results_schema = results_schema,
    temp_schema = temp_schema,
    config = config
  )

  .buildBlueprint(handle)
  .setHandle(resource_symbol, handle)

  invisible(TRUE)
}

#' Execute an extraction plan (Assign)
#'
#' Runs the extraction plan and assigns each output directly into the
#' DataSHIELD session as a named symbol. Sparse outputs are split into
#' two symbols: \code{<name>.covariates} and \code{<name>.covariateRef}.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param plan List; the extraction plan
#' @param out Named character vector; output_name -> symbol_name mapping
#' @return Invisible TRUE (outputs are assigned into caller's environment)
#' @export
omopPlanExecuteDS <- function(omop_symbol, plan, out) {
  handle <- .getHandle(omop_symbol)
  outputs <- .planExecute(handle, plan, out)

  # Validate that requested outputs were produced
  missing <- setdiff(names(out), names(outputs))
  if (length(missing) > 0) {
    warning("Plan did not produce outputs: ",
            paste(missing, collapse = ", "), call. = FALSE)
  }

  assign_env <- parent.frame()

  for (nm in names(out)) {
    sym <- out[[nm]]
    result <- outputs[[nm]]
    if (is.null(result)) next

    # Temporal covariates: split into 3 symbols
    if (is.list(result) && !is.data.frame(result) &&
        "temporalCovariates" %in% names(result)) {
      assign(paste0(sym, ".temporalCovariates"),
             result$temporalCovariates, envir = assign_env)
      assign(paste0(sym, ".covariateRef"),
             result$covariateRef, envir = assign_env)
      assign(paste0(sym, ".timeRef"),
             result$timeRef, envir = assign_env)
    # Sparse outputs: split list into two data.frame symbols
    } else if (is.list(result) && !is.data.frame(result) &&
        all(c("covariates", "covariateRef") %in% names(result))) {
      assign(paste0(sym, ".covariates"),
             result$covariates, envir = assign_env)
      assign(paste0(sym, ".covariateRef"),
             result$covariateRef, envir = assign_env)
    } else {
      assign(sym, result, envir = assign_env)
    }
  }

  invisible(TRUE)
}

#' Create a cohort (Assign)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param cohort_spec List; cohort specification DSL
#' @param mode Character; "temporary" or "persistent"
#' @param cohort_id Integer; cohort definition ID
#' @param name Character; cohort name
#' @param overwrite Logical
#' @return Character; temp table name or confirmation
#' @export
omopCohortCreateDS <- function(omop_symbol, cohort_spec,
                               mode = "temporary",
                               cohort_id = NULL,
                               name = NULL,
                               overwrite = FALSE) {
  handle <- .getHandle(omop_symbol)
  .cohortCreate(handle, cohort_spec, mode, cohort_id, name, overwrite)
}

#' Combine cohorts (Assign)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param op Character; "intersect", "union", or "setdiff"
#' @param cohort_a Character; first cohort temp table
#' @param cohort_b Character; second cohort temp table
#' @param new_name Character; result temp table name
#' @return Character; result temp table name
#' @export
omopCohortCombineDS <- function(omop_symbol, op,
                                cohort_a, cohort_b,
                                new_name = NULL) {
  handle <- .getHandle(omop_symbol)
  .cohortCombine(handle, op, cohort_a, cohort_b, new_name)
}

#' Clean up temp artifacts (Assign)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param prefix Character; temp table prefix to clean
#' @return Invisible TRUE
#' @export
omopCleanupDS <- function(omop_symbol, prefix = "dsomop_") {
  handle <- .getHandle(omop_symbol)
  to_drop <- grep(paste0("^", prefix), handle$temp_tables,
                  value = TRUE)
  for (tbl in to_drop) {
    .dropTempTable(handle, tbl)
  }
  invisible(TRUE)
}

# ==============================================================================
# AGGREGATE METHODS (17)
# ==============================================================================

#' Ping / health check (Aggregate)
#'
#' @return Named list with alive status
#' @export
omopPingDS <- function() {
  list(
    alive = TRUE,
    version = as.character(utils::packageVersion("dsOMOP")),
    timestamp = Sys.time()
  )
}

#' Get capabilities snapshot (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Named list with schema summary and hash
#' @export
omopGetCapabilitiesDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .getCapabilities(handle)
}

#' List tables (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with table metadata
#' @export
omopListTablesDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  bp <- .buildBlueprint(handle)
  present <- bp$tables[bp$tables$present_in_db, , drop = FALSE]
  present[, c("table_name", "schema_category", "has_person_id",
              "concept_prefix")]
}

#' List columns (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @return Data frame with column metadata
#' @export
omopListColumnsDS <- function(omop_symbol, table) {
  handle <- .getHandle(omop_symbol)
  table <- tolower(.validateIdentifier(table, "table"))
  bp <- .buildBlueprint(handle)

  if (is.null(bp$columns[[table]])) {
    stop("Table '", table, "' not found.", call. = FALSE)
  }
  bp$columns[[table]]
}

#' Get relationship graph (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with join relationships
#' @export
omopRelationshipGraphDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  bp <- .buildBlueprint(handle)
  bp$join_graph
}

#' Get table statistics (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param stats Character vector; which stats to return
#' @return Named list with safe statistics
#' @export
omopTableStatsDS <- function(omop_symbol, table,
                             stats = c("rows", "persons")) {
  handle <- .getHandle(omop_symbol)
  .profileTableStats(handle, table, stats)
}

#' Get column statistics (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param column Character; column name
#' @return Named list with column statistics
#' @export
omopColumnStatsDS <- function(omop_symbol, table, column) {
  handle <- .getHandle(omop_symbol)
  .profileColumnStats(handle, table, column)
}

#' Get cross-table domain coverage (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with per-table person counts
#' @export
omopDomainCoverageDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .profileDomainCoverage(handle)
}

#' Get missingness rates (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param columns Character vector; columns to check
#' @return Data frame with missingness rates
#' @export
omopMissingnessDS <- function(omop_symbol, table,
                              columns = NULL) {
  handle <- .getHandle(omop_symbol)
  .profileMissingness(handle, table, columns)
}

#' Get value counts (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param column Character; column name
#' @param top_n Integer
#' @param suppress_small Logical
#' @return Data frame with value counts
#' @export
omopValueCountsDS <- function(omop_symbol, table, column,
                              top_n = 20,
                              suppress_small = TRUE) {
  handle <- .getHandle(omop_symbol)
  .profileValueCounts(handle, table, column, top_n, suppress_small)
}

#' Search concepts (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param pattern Character; search pattern
#' @param domain Character; domain filter
#' @param vocabulary Character; vocabulary filter
#' @param standard_only Logical; only standard concepts
#' @param limit Integer; max results
#' @return Data frame with concept results
#' @export
omopSearchConceptsDS <- function(omop_symbol, pattern,
                                 domain = NULL,
                                 vocabulary = NULL,
                                 standard_only = TRUE,
                                 limit = 50) {
  handle <- .getHandle(omop_symbol)
  .vocabSearchConcepts(handle, pattern, domain,
                       vocabulary, standard_only, limit)
}

#' Lookup concepts by ID (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param concept_ids Numeric vector
#' @return Data frame with concept details
#' @export
omopLookupConceptsDS <- function(omop_symbol, concept_ids) {
  handle <- .getHandle(omop_symbol)
  .vocabLookupConcepts(handle, concept_ids)
}

#' Get descendant concepts (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param ancestor_ids Numeric vector
#' @param include_self Logical
#' @return Data frame with descendant concepts
#' @export
omopGetDescendantsDS <- function(omop_symbol, ancestor_ids,
                                 include_self = TRUE) {
  handle <- .getHandle(omop_symbol)
  .vocabGetDescendants(handle, ancestor_ids, include_self)
}

#' Expand a concept set (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param concept_set List; concept set specification
#' @return Integer vector of expanded concept IDs
#' @export
omopExpandConceptSetDS <- function(omop_symbol, concept_set) {
  handle <- .getHandle(omop_symbol)
  .vocabExpandConceptSet(handle, concept_set)
}

#' Preview a plan (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param plan List; the extraction plan
#' @return List with preview information
#' @export
omopPlanPreviewDS <- function(omop_symbol, plan) {
  handle <- .getHandle(omop_symbol)
  .planPreview(handle, plan)
}

#' List cohort definitions (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with cohort definitions
#' @export
omopCohortListDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .cohortList(handle)
}

#' Get a cohort definition (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param cohort_definition_id Integer
#' @return Named list with definition details
#' @export
omopCohortGetDefinitionDS <- function(omop_symbol,
                                      cohort_definition_id) {
  handle <- .getHandle(omop_symbol)
  .cohortGetDefinition(handle, cohort_definition_id)
}

# ==============================================================================
# EXPLORATION AGGREGATE METHODS (OMOP Studio)
# ==============================================================================

#' Get concept prevalence (Aggregate)
#'
#' Returns the top concepts in a table ranked by person count or record count,
#' with disclosure control applied.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param concept_col Character; concept column name (NULL = auto-detect)
#' @param metric Character; "persons" or "records"
#' @param top_n Integer; number of top concepts to return
#' @param cohort_table Character; cohort temp table for filtering (NULL)
#' @param window List with start/end dates for filtering (NULL)
#' @return Data frame with concept_id, concept_name, n_persons, n_records
#' @export
omopConceptPrevalenceDS <- function(omop_symbol, table, concept_col = NULL,
                                     metric = "persons", top_n = 50,
                                     cohort_table = NULL, window = NULL) {
  handle <- .getHandle(omop_symbol)
  .profileConceptPrevalence(handle, table, concept_col, metric, top_n,
                            cohort_table, window)
}

#' Get numeric histogram (Aggregate)
#'
#' Computes a safe histogram for a numeric column with suppressed low-count bins.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param bins Integer; number of bins
#' @param cohort_table Character; cohort temp table for filtering (NULL)
#' @param window List with start/end dates for filtering (NULL)
#' @return Data frame with bin_start, bin_end, count, suppressed
#' @export
omopNumericHistogramDS <- function(omop_symbol, table, value_col,
                                    bins = 20L, cohort_table = NULL,
                                    window = NULL) {
  handle <- .getHandle(omop_symbol)
  .profileNumericHistogram(handle, table, value_col, bins,
                           cohort_table, window)
}

#' Get numeric quantiles (Aggregate)
#'
#' Computes quantiles at specified probabilities using SQL approximation.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param probs Numeric vector; probabilities
#' @param cohort_table Character; cohort temp table for filtering (NULL)
#' @param window List with start/end dates for filtering (NULL)
#' @param rounding Integer; decimal places for rounding
#' @return Data frame with probability and value
#' @export
omopNumericQuantilesDS <- function(omop_symbol, table, value_col,
                                    probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                    cohort_table = NULL, window = NULL,
                                    rounding = 2L) {
  handle <- .getHandle(omop_symbol)
  .profileNumericQuantiles(handle, table, value_col, probs,
                           cohort_table, window, rounding)
}

#' Get date counts (Aggregate)
#'
#' Counts records by time bin (year, quarter, or month) with disclosure control.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param date_col Character; date column (NULL = auto-detect)
#' @param granularity Character; "year", "quarter", or "month"
#' @param cohort_table Character; cohort temp table for filtering (NULL)
#' @param window List with start/end dates for filtering (NULL)
#' @return Data frame with period, n_records, suppressed
#' @export
omopDateCountsDS <- function(omop_symbol, table, date_col = NULL,
                              granularity = "year", cohort_table = NULL,
                              window = NULL) {
  handle <- .getHandle(omop_symbol)
  .profileDateCounts(handle, table, date_col, granularity,
                     cohort_table, window)
}

#' Get concept drilldown (Aggregate)
#'
#' Returns a full drilldown profile for a single concept within a table,
#' including summary stats, numeric distribution, categorical values,
#' date coverage, and missingness.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param concept_id Integer; concept ID to drill into
#' @return Named list with drilldown results
#' @export
omopConceptDrilldownDS <- function(omop_symbol, table, concept_id) {
  handle <- .getHandle(omop_symbol)
  .profileConceptDrilldown(handle, table, as.integer(concept_id))
}

#' Locate concept across tables (Aggregate)
#'
#' Searches all CDM tables with concept columns and returns a presence
#' matrix showing where the given concept IDs appear.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param concept_ids Integer vector; concept IDs to locate
#' @return Data frame with table_name, concept_column, concept_id, n_records,
#'   n_persons
#' @export
omopLocateConceptDS <- function(omop_symbol, concept_ids) {
  handle <- .getHandle(omop_symbol)
  .profileLocateConcept(handle, as.integer(concept_ids))
}

# ==============================================================================
# ACHILLES AGGREGATE METHODS (Data Sources)
# ==============================================================================

#' Check Achilles availability (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Named list with availability status
#' @export
omopAchillesStatusDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .achillesStatus(handle)
}

#' List Achilles analyses (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param domain Character; optional domain filter
#' @return Data frame with analysis catalog
#' @export
omopAchillesAnalysesDS <- function(omop_symbol, domain = NULL) {
  handle <- .getHandle(omop_symbol)
  .achillesListAnalyses(handle, domain)
}

#' Get Achilles count results (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param analysis_ids Integer vector; analysis IDs
#' @param stratum_filters Named list; stratum filters
#' @param min_cell_count Integer; minimum cell count
#' @return Data frame with analysis results
#' @export
omopAchillesResultsDS <- function(omop_symbol, analysis_ids,
                                   stratum_filters = NULL,
                                   min_cell_count = NULL) {
  handle <- .getHandle(omop_symbol)
  .achillesGetResults(handle, analysis_ids, stratum_filters, min_cell_count)
}

#' Get Achilles distribution results (Aggregate)
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param analysis_ids Integer vector; analysis IDs
#' @param stratum_filters Named list; stratum filters
#' @return Data frame with distribution statistics
#' @export
omopAchillesDistributionDS <- function(omop_symbol, analysis_ids,
                                        stratum_filters = NULL) {
  handle <- .getHandle(omop_symbol)
  .achillesGetDistributions(handle, analysis_ids, stratum_filters)
}
