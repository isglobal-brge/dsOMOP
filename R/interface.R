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
  resource_client <- resourcer::newResourceClient(
    eval(parse(text = resource_symbol),
         envir = parent.frame()))

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
#' @param omop_symbol Character; the OMOP handle symbol
#' @param plan List; the extraction plan
#' @param out_symbols Named list; output name -> R symbol mapping
#' @return Named list of data frames
#' @export
omopPlanExecuteDS <- function(omop_symbol, plan, out_symbols) {
  handle <- .getHandle(omop_symbol)
  .planExecute(handle, plan, out_symbols)
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
