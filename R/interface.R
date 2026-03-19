# Module: DataSHIELD Exposed Methods
# All DataSHIELD assign/aggregate methods. Thin wrappers around internal functions.

# --- JSON transport for Opal compatibility ---

#' Deserialize a possibly-JSON argument
#'
#' When complex R objects (lists, named vectors) are passed through
#' \code{datashield.assign.expr()} or \code{datashield.aggregate()}, Opal
#' serializes them via \code{deparse()}, which generates \code{structure()} or
#' \code{c()} calls. These base R functions are not in Opal's DataSHIELD method
#' whitelist, causing \code{400 Bad Request} errors.
#'
#' The solution: the client wraps complex arguments in
#' \code{jsonlite::toJSON(auto_unbox = TRUE)}, and the server calls this helper
#' to transparently deserialize them. DSLite passes native R objects directly,
#' so this function is a no-op when the argument is already a list.
#'
#' @param x An argument that may be a JSON string or an already-parsed R object.
#' @return The deserialized R object.
#' @keywords internal
.ds_arg <- function(x) {
  if (is.character(x) && length(x) == 1) {
    if (startsWith(x, "B64:")) {
      # URL-safe base64 → standard base64
      b64 <- substring(x, 5)
      b64 <- gsub("-", "+", b64)
      b64 <- gsub("_", "/", b64)
      # Restore padding
      pad <- (4 - nchar(b64) %% 4) %% 4
      if (pad > 0) b64 <- paste0(b64, strrep("=", pad))
      json <- rawToChar(jsonlite::base64_dec(b64))
      return(jsonlite::fromJSON(json, simplifyVector = FALSE))
    }
    if (nchar(x) > 0 && substr(x, 1, 1) %in% c("{", "[")) {
      return(jsonlite::fromJSON(x, simplifyVector = FALSE))
    }
  }
  x
}

# --- Handle management ---

#' Retrieve a stored OMOP CDM handle
#'
#' Looks up the server-side OMOP CDM handle object associated with a given
#' resource symbol name. In DSLite (multi-server, single R process), each
#' server's session environment holds its own handle, avoiding collisions.
#' Falls back to the global \code{.dsomop_env} for real DataSHIELD (Opal)
#' deployments where each server runs in its own R process.
#'
#' @param symbol Character; the resource symbol name identifying the handle.
#' @return The OMOP CDM handle object.
#' @keywords internal
.getHandle <- function(symbol) {
  local_key <- paste0(".dsomop_handle_", symbol)

  # DSLite-safe: check the calling session environment (2 frames up:
  # .getHandle <- DS_function <- DSLite session env)
  session_env <- tryCatch(parent.frame(2), error = function(e) NULL)
  if (!is.null(session_env) &&
      exists(local_key, envir = session_env, inherits = FALSE)) {
    return(get(local_key, envir = session_env, inherits = FALSE))
  }

  # Fallback: global package environment (works for Opal single-process)
  key <- paste0("handle_", symbol)
  if (!exists(key, envir = .dsomop_env)) {
    stop("No OMOP handle for symbol '", symbol,
         "'. Call omopInitDS first.", call. = FALSE)
  }
  get(key, envir = .dsomop_env)
}

#' Store an OMOP CDM handle in the session environment
#'
#' Saves the given handle object into the dsOMOP session environment under a
#' key derived from the resource symbol name. Overwrites any existing handle
#' for the same symbol.
#'
#' @param symbol Character; the resource symbol name.
#' @param handle The OMOP CDM handle object to store.
#' @return Invisible NULL (called for side effect).
#' @keywords internal
.setHandle <- function(symbol, handle) {
  key <- paste0("handle_", symbol)
  assign(key, handle, envir = .dsomop_env)
}

#' Remove an OMOP CDM handle from the session environment
#'
#' Closes the database connection associated with the handle and removes it
#' from the dsOMOP session environment. No-op if no handle exists for the
#' given symbol.
#'
#' @param symbol Character; the resource symbol name.
#' @return Invisible NULL (called for side effect).
#' @keywords internal
.removeHandle <- function(symbol) {
  key <- paste0("handle_", symbol)
  if (exists(key, envir = .dsomop_env)) {
    handle <- get(key, envir = .dsomop_env)
    .closeHandle(handle)
    rm(list = key, envir = .dsomop_env)
  }
}

#' Strip row-level identifiers from data before DataSHIELD assignment
#'
#' Removes all OMOP CDM row-level identifiers (primary keys and entity
#' foreign keys) from data frames before they are assigned into the
#' DataSHIELD session environment.
#'
#' @section Security Rationale:
#' DataSHIELD analysis functions like \code{ds.summary()}, \code{ds.table()},
#' and \code{ds.quantile()} operate on assigned server-side data frames.
#' If \code{person_id} or other row-level identifiers are present, an
#' attacker can perform group-by re-identification attacks:
#' \itemize{
#'   \item \code{ds.summary(data$value, data$person_id)} reveals per-person
#'     statistics
#'   \item \code{ds.table(data$condition, data$person_id)} produces a
#'     person-level contingency table
#'   \item Even event-level IDs (\code{visit_occurrence_id}) can be used to
#'     count rows per person via frequency analysis
#' }
#' This stripping is \strong{mandatory} and cannot be disabled. It runs on
#' every ASSIGN output before \code{base::assign()} stores the data in the
#' DataSHIELD environment.
#'
#' @param x Data frame or list to sanitize. Operates recursively on lists.
#' @return Sanitized object with identifier columns removed
#' @keywords internal
.stripIdentifiers <- function(x) {
  # Full OMOP CDM identifier column list: primary keys (person_id,
  # *_occurrence_id) and entity foreign keys (provider_id, care_site_id,
  # location_id). These uniquely or quasi-uniquely identify rows and
  # could enable re-identification if exposed to DS analysis functions.
  strip_cols <- c(
    # Person / subject identifiers
    "person_id", "subject_id",
    # Clinical event row IDs
    "visit_occurrence_id", "visit_detail_id",
    "condition_occurrence_id", "drug_exposure_id",
    "procedure_occurrence_id", "measurement_id",
    "observation_id", "device_exposure_id",
    "specimen_id", "note_id",
    # Provider / location entity keys (indirect identifiers)
    "provider_id", "care_site_id", "location_id"
  )
  if (is.data.frame(x)) {
    drop <- intersect(strip_cols, names(x))
    if (length(drop) > 0) {
      x[drop] <- NULL
    }
  } else if (is.list(x)) {
    x <- lapply(x, .stripIdentifiers)
  }
  x
}

# --- Assign methods ---

#' Initialize an OMOP CDM handle (Assign)
#'
#' @description
#' Creates a server-side connection to an OMOP CDM database from a DataSHIELD
#' resource. The handle is stored in the dsOMOP session environment and used
#' by all subsequent OMOP operations.
#'
#' @param resource_symbol Character; the resource symbol name
#' @param cdm_schema Character; override CDM schema
#' @param results_schema Character; results schema name
#' @param vocab_schema Character; vocabulary schema name
#' @param temp_schema Character; temp schema name
#' @param config Named list; additional configuration
#' @return The handle symbol (assigned server-side)
#' @examples
#' \dontrun{
#' omopInitDS("omop_resource")
#' }
#' @export
omopInitDS <- function(resource_symbol,
                       cdm_schema = NULL,
                       results_schema = NULL,
                       vocab_schema = NULL,
                       temp_schema = NULL,
                       config = list()) {
  # SECURITY: resource_symbol comes from the client and is used to look up
  # a variable in the DataSHIELD session environment. Without validation,
  # a malicious client could pass arbitrary strings (e.g., "system('rm -rf')")
  # that, if passed to eval(parse()), would execute arbitrary code.
  # We validate it as a strict R identifier, then use get() (not eval/parse).
  .validateIdentifier(resource_symbol, "resource symbol")
  resolved <- get(resource_symbol, envir = parent.frame(), inherits = FALSE)

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

  # DSLite session isolation: in DSLite (multi-server, single R process),
  # each DataSHIELD session has its own environment. We store the handle
  # in parent.frame() (the session env) under a unique key so that
  # server_a's handle is not overwritten by server_b. Without this,
  # .dsomop_env (global) would be shared, causing cross-server data leaks.
  local_key <- paste0(".dsomop_handle_", resource_symbol)
  tryCatch(
    assign(local_key, handle, envir = parent.frame()),
    error = function(e) NULL  # silently skip if env is locked
  )

  invisible(TRUE)
}

#' Execute an extraction plan (Assign)
#'
#' @description
#' Runs the extraction plan and assigns each output directly into the
#' DataSHIELD session as a named symbol. Sparse outputs are split into
#' two symbols: \code{<name>.covariates} and \code{<name>.covariateRef}.
#' Temporal covariates are split into three symbols.
#'
#' When \code{output_mode = "staged"}, outputs are written to Parquet files
#' on disk and assigned as \code{FlowerDatasetDescriptor} objects instead of
#' data.frames. This avoids loading large datasets into R memory.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param plan List; the extraction plan
#' @param out Named character vector; output_name -> symbol_name mapping
#' @param output_mode Character; "memory" (default) or "staged"
#' @return Invisible TRUE (outputs are assigned into caller's environment)
#' @examples
#' \dontrun{
#' omopPlanExecuteDS("omop", plan, out = c(cohort = "my_cohort"))
#' omopPlanExecuteDS("omop", plan, out = c(features = "D"),
#'                   output_mode = "staged")
#' }
#' @export
omopPlanExecuteDS <- function(omop_symbol, plan, out,
                               output_mode = "memory") {
  handle <- .getHandle(omop_symbol)
  plan <- .ds_arg(plan)
  out <- .ds_arg(out)
  output_mode <- .ds_arg(output_mode)
  if (!is.character(output_mode) || length(output_mode) != 1L) {
    output_mode <- "memory"
  }
  if (!output_mode %in% c("memory", "staged")) {
    output_mode <- "memory"
  }
  outputs <- .planExecute(handle, plan, out, output_mode = output_mode)

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

    # Staged descriptors: assign directly (no data to strip)
    if (inherits(result, "FlowerDatasetDescriptor")) {
      assign(sym, result, envir = assign_env)
      next
    }

    # MANDATORY: Strip all row-level identifiers before assignment.
    # Without this, ds.summary(data$value, data$person_id) would reveal
    # per-person statistics, enabling group-by re-identification attacks.
    # This stripping cannot be disabled — it is the last line of defense
    # before data enters the DataSHIELD analysis environment.
    result <- .stripIdentifiers(result)

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
#' @description
#' Creates a cohort on the server side from a cohort specification DSL object.
#' The cohort can be stored as a temporary table or persisted to the results
#' schema depending on the mode parameter.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param cohort_spec List; cohort specification DSL
#' @param mode Character; "temporary" or "persistent"
#' @param cohort_id Integer; cohort definition ID
#' @param name Character; cohort name
#' @param overwrite Logical
#' @return Character; temp table name or confirmation
#' @examples
#' \dontrun{
#' result <- omopCohortCreateDS("omop", cohort_spec, mode = "temporary")
#' }
#' @export
omopCohortCreateDS <- function(omop_symbol, cohort_spec,
                               mode = "temporary",
                               cohort_id = NULL,
                               name = NULL,
                               overwrite = FALSE) {
  handle <- .getHandle(omop_symbol)
  cohort_spec <- .ds_arg(cohort_spec)
  .cohortCreate(handle, cohort_spec, mode, cohort_id, name, overwrite)
}

#' Combine cohorts (Assign)
#'
#' @description
#' Combines two existing server-side cohorts using a set operation (intersect,
#' union, or set difference) and stores the result as a new temporary table.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param op Character; "intersect", "union", or "setdiff"
#' @param cohort_a Character; first cohort temp table
#' @param cohort_b Character; second cohort temp table
#' @param new_name Character; result temp table name
#' @return Character; result temp table name
#' @examples
#' \dontrun{
#' combined <- omopCohortCombineDS("omop", "union", "cohort_a", "cohort_b")
#' }
#' @export
omopCohortCombineDS <- function(omop_symbol, op,
                                cohort_a, cohort_b,
                                new_name = NULL) {
  handle <- .getHandle(omop_symbol)
  .cohortCombine(handle, op, cohort_a, cohort_b, new_name)
}

#' Clean up temp artifacts (Assign)
#'
#' @description
#' Drops all server-side temporary tables whose names match the given prefix.
#' Called during session teardown or when resetting state between analyses.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param prefix Character; temp table prefix to clean
#' @return Invisible TRUE
#' @examples
#' \dontrun{
#' omopCleanupDS("omop", prefix = "dsomop_")
#' }
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

# --- Aggregate methods ---

#' Ping / health check (Aggregate)
#'
#' @description
#' Returns basic status information indicating the dsOMOP server package is
#' loaded and responsive. Used by the client to verify connectivity before
#' issuing further commands.
#'
#' @return Named list with alive status, package version, and timestamp.
#' @examples
#' \dontrun{
#' omopPingDS()
#' }
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
#' @description
#' Returns a snapshot of the server-side OMOP CDM schema, including available
#' tables, DBMS type, CDM version, and a hash for cache invalidation. Used by
#' the client to adapt the UI to the server's data model.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Named list with schema summary and hash
#' @examples
#' \dontrun{
#' caps <- omopGetCapabilitiesDS("omop")
#' }
#' @export
omopGetCapabilitiesDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .getCapabilities(handle)
}

#' List tables (Aggregate)
#'
#' @description
#' Returns metadata for all OMOP CDM tables present in the database, including
#' schema category, person ID availability, and concept column prefix.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with table metadata
#' @examples
#' \dontrun{
#' tables <- omopListTablesDS("omop")
#' }
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
#' @description
#' Returns column-level metadata for a specific OMOP CDM table, including
#' column names, data types, and whether each column is present in the database.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @return Data frame with column metadata
#' @examples
#' \dontrun{
#' cols <- omopListColumnsDS("omop", "person")
#' }
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
#' @description
#' Returns the join graph describing foreign-key relationships between OMOP CDM
#' tables. Used by the client to understand how tables can be linked for
#' multi-table queries.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with join relationships
#' @examples
#' \dontrun{
#' graph <- omopRelationshipGraphDS("omop")
#' }
#' @export
omopRelationshipGraphDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  bp <- .buildBlueprint(handle)
  bp$join_graph
}

#' Get table statistics (Aggregate)
#'
#' @description
#' Returns disclosure-controlled summary statistics for an OMOP CDM table,
#' such as total row count and distinct person count. Values below the
#' disclosure threshold are suppressed.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param stats Character vector; which stats to return
#' @return Named list with safe statistics
#' @examples
#' \dontrun{
#' stats <- omopTableStatsDS("omop", "condition_occurrence")
#' }
#' @export
omopTableStatsDS <- function(omop_symbol, table,
                             stats = c("rows", "persons")) {
  handle <- .getHandle(omop_symbol)
  stats <- .ds_arg(stats)
  if (is.list(stats)) stats <- unlist(stats)
  .profileTableStats(handle, table, stats)
}

#' Get column statistics (Aggregate)
#'
#' @description
#' Returns disclosure-controlled summary statistics for a single column in an
#' OMOP CDM table, including data type, completeness, distinct values, and
#' numeric summary when applicable.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param column Character; column name
#' @return Named list with column statistics
#' @examples
#' \dontrun{
#' stats <- omopColumnStatsDS("omop", "person", "year_of_birth")
#' }
#' @export
omopColumnStatsDS <- function(omop_symbol, table, column) {
  handle <- .getHandle(omop_symbol)
  .profileColumnStats(handle, table, column)
}

#' Get cross-table domain coverage (Aggregate)
#'
#' @description
#' Returns the number of distinct persons represented in each clinical domain
#' table (e.g., condition_occurrence, drug_exposure). Provides a quick overview
#' of data completeness across the CDM.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with per-table person counts
#' @examples
#' \dontrun{
#' coverage <- omopDomainCoverageDS("omop")
#' }
#' @export
omopDomainCoverageDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .profileDomainCoverage(handle)
}

#' Get missingness rates (Aggregate)
#'
#' @description
#' Computes the proportion of NULL values for each specified column (or all
#' columns) in an OMOP CDM table. Useful for data quality assessment before
#' running analyses.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param columns Character vector; columns to check
#' @return Data frame with missingness rates
#' @examples
#' \dontrun{
#' missing <- omopMissingnessDS("omop", "condition_occurrence")
#' }
#' @export
omopMissingnessDS <- function(omop_symbol, table,
                              columns = NULL) {
  handle <- .getHandle(omop_symbol)
  columns <- .ds_arg(columns)
  if (is.list(columns)) columns <- as.character(unlist(columns))
  .profileMissingness(handle, table, columns)
}

#' Get value counts (Aggregate)
#'
#' @description
#' Returns the frequency distribution of distinct values in a column, limited
#' to the top N most frequent values. Small counts are suppressed according to
#' the server's disclosure threshold.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param column Character; column name
#' @param top_n Integer; maximum number of distinct values to return
#' @param suppress_small Logical; whether to suppress counts below threshold
#' @return Data frame with value counts
#' @examples
#' \dontrun{
#' counts <- omopValueCountsDS("omop", "person", "gender_concept_id")
#' }
#' @export
omopValueCountsDS <- function(omop_symbol, table, column,
                              top_n = 20,
                              suppress_small = TRUE) {
  handle <- .getHandle(omop_symbol)
  .profileValueCounts(handle, table, column, top_n, suppress_small)
}

#' Search concepts (Aggregate)
#'
#' @description
#' Searches the OMOP vocabulary tables for concepts matching a text pattern,
#' optionally filtered by domain and vocabulary. Returns concept metadata
#' including concept ID, name, domain, vocabulary, and standard status.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param pattern Character; search pattern (case-insensitive substring match)
#' @param domain Character; domain filter (e.g., "Condition", "Drug")
#' @param vocabulary Character; vocabulary filter (e.g., "SNOMED", "RxNorm")
#' @param standard_only Logical; only standard concepts
#' @param limit Integer; max results
#' @return Data frame with concept results
#' @examples
#' \dontrun{
#' concepts <- omopSearchConceptsDS("omop", "diabetes", domain = "Condition")
#' }
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
#' @description
#' Retrieves full concept metadata for one or more concept IDs from the OMOP
#' vocabulary tables. Returns concept name, domain, vocabulary, class, and
#' standard concept flag.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param concept_ids Numeric vector; one or more concept IDs to look up
#' @return Data frame with concept details
#' @examples
#' \dontrun{
#' details <- omopLookupConceptsDS("omop", c(201826, 4329847))
#' }
#' @export
omopLookupConceptsDS <- function(omop_symbol, concept_ids) {
  handle <- .getHandle(omop_symbol)
  concept_ids <- .ds_arg(concept_ids)
  if (is.list(concept_ids)) concept_ids <- as.integer(unlist(concept_ids))
  .vocabLookupConcepts(handle, concept_ids)
}

#' Get descendant concepts (Aggregate)
#'
#' @description
#' Traverses the OMOP concept_ancestor table to find all descendant concepts
#' of one or more ancestor concept IDs. Optionally includes the ancestor
#' concepts themselves in the result.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param ancestor_ids Numeric vector; ancestor concept IDs
#' @param include_self Logical; whether to include the ancestor IDs in results
#' @return Data frame with descendant concepts
#' @examples
#' \dontrun{
#' descendants <- omopGetDescendantsDS("omop", c(201826), include_self = TRUE)
#' }
#' @export
omopGetDescendantsDS <- function(omop_symbol, ancestor_ids,
                                 include_self = TRUE) {
  handle <- .getHandle(omop_symbol)
  ancestor_ids <- .ds_arg(ancestor_ids)
  if (is.list(ancestor_ids)) ancestor_ids <- as.integer(unlist(ancestor_ids))
  .vocabGetDescendants(handle, ancestor_ids, include_self)
}

#' Expand a concept set (Aggregate)
#'
#' @description
#' Expands a concept set specification into a flat vector of concept IDs by
#' applying inclusion/exclusion rules and descendant traversal. Mirrors the
#' ATLAS concept set expression logic.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param concept_set List; concept set specification with inclusion rules
#' @return Integer vector of expanded concept IDs
#' @examples
#' \dontrun{
#' ids <- omopExpandConceptSetDS("omop", concept_set)
#' }
#' @export
omopExpandConceptSetDS <- function(omop_symbol, concept_set) {
  handle <- .getHandle(omop_symbol)
  concept_set <- .ds_arg(concept_set)
  .vocabExpandConceptSet(handle, concept_set)
}

#' Preview a plan (Aggregate)
#'
#' @description
#' Validates and previews an extraction plan without executing it. Returns
#' expected output schemas, estimated row counts, and any validation warnings
#' or errors.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param plan List; the extraction plan to preview
#' @return List with preview information including output schemas and warnings
#' @examples
#' \dontrun{
#' preview <- omopPlanPreviewDS("omop", plan)
#' }
#' @export
omopPlanPreviewDS <- function(omop_symbol, plan) {
  handle <- .getHandle(omop_symbol)
  plan <- .ds_arg(plan)
  .planPreview(handle, plan)
}

#' List cohort definitions (Aggregate)
#'
#' @description
#' Returns metadata for all cohort definitions available in the results schema,
#' including cohort definition IDs, names, and subject counts.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with cohort definitions
#' @examples
#' \dontrun{
#' cohorts <- omopCohortListDS("omop")
#' }
#' @export
omopCohortListDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .cohortList(handle)
}

#' Get a cohort definition (Aggregate)
#'
#' @description
#' Retrieves the full definition for a specific cohort, including the cohort
#' specification DSL, name, description, and subject count.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param cohort_definition_id Integer; the cohort definition ID to retrieve
#' @return Named list with definition details
#' @examples
#' \dontrun{
#' defn <- omopCohortGetDefinitionDS("omop", cohort_definition_id = 1L)
#' }
#' @export
omopCohortGetDefinitionDS <- function(omop_symbol,
                                      cohort_definition_id) {
  handle <- .getHandle(omop_symbol)
  .cohortGetDefinition(handle, cohort_definition_id)
}

# --- Exploration aggregate methods ---

#' Get concept prevalence (Aggregate)
#'
#' @description
#' Returns the top concepts in a table ranked by person count or record count,
#' with disclosure control applied. Concepts with counts below the server
#' threshold are suppressed.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param concept_col Character; concept column name (NULL = auto-detect)
#' @param metric Character; "persons" or "records"
#' @param top_n Integer; number of top concepts to return
#' @param cohort_table Character; cohort temp table for filtering (NULL)
#' @param window List with start/end dates for filtering (NULL)
#' @return Data frame with concept_id, concept_name, n_persons, n_records
#' @examples
#' \dontrun{
#' prevalence <- omopConceptPrevalenceDS("omop", "condition_occurrence")
#' }
#' @export
omopConceptPrevalenceDS <- function(omop_symbol, table, concept_col = NULL,
                                     metric = "persons", top_n = 50,
                                     cohort_table = NULL, window = NULL) {
  handle <- .getHandle(omop_symbol)
  .profileConceptPrevalence(handle, table, concept_col, metric, top_n,
                            cohort_table, window)
}

#' Get numeric range (p05/p95) for two-pass histogram pooling (Aggregate)
#'
#' @description
#' Returns the 5th and 95th percentile approximations and total count for a
#' numeric column. Used as pass 1 of two-pass histogram pooling to compute
#' shared bin edges across federated sites.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param cohort_table Character; cohort temp table for filtering (NULL)
#' @param window List with start/end dates for filtering (NULL)
#' @return List with p05, p95, n_total
#' @examples
#' \dontrun{
#' range_info <- omopNumericRangeDS("omop", "measurement", "value_as_number")
#' }
#' @export
omopNumericRangeDS <- function(omop_symbol, table, value_col,
                                cohort_table = NULL, window = NULL) {
  handle <- .getHandle(omop_symbol)
  .profileNumericRange(handle, table, value_col, cohort_table, window)
}

#' Get numeric histogram (Aggregate)
#'
#' @description
#' Computes a disclosure-controlled histogram for a numeric column. Bins with
#' counts below the server threshold are suppressed. Supports shared bin edges
#' from two-pass pooling for consistent cross-site comparisons.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param bins Integer; number of bins
#' @param cohort_table Character; cohort temp table for filtering (NULL)
#' @param window List with start/end dates for filtering (NULL)
#' @param breaks Numeric vector; shared bin edges from two-pass pooling (NULL)
#' @return Data frame with bin_start, bin_end, count, suppressed
#' @examples
#' \dontrun{
#' hist_data <- omopNumericHistogramDS("omop", "measurement", "value_as_number")
#' }
#' @export
omopNumericHistogramDS <- function(omop_symbol, table, value_col,
                                    bins = 20L, cohort_table = NULL,
                                    window = NULL, breaks = NULL) {
  handle <- .getHandle(omop_symbol)
  breaks <- .ds_arg(breaks)
  .profileNumericHistogram(handle, table, value_col, bins,
                           cohort_table, window, breaks)
}

#' Get numeric quantiles (Aggregate)
#'
#' @description
#' Computes quantiles at specified probabilities using SQL-based approximation.
#' Results are rounded to the specified number of decimal places to limit
#' precision and reduce re-identification risk.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param probs Numeric vector; probabilities (e.g., c(0.05, 0.25, 0.5, 0.75, 0.95))
#' @param cohort_table Character; cohort temp table for filtering (NULL)
#' @param window List with start/end dates for filtering (NULL)
#' @param rounding Integer; decimal places for rounding
#' @return Data frame with probability and value
#' @examples
#' \dontrun{
#' quantiles <- omopNumericQuantilesDS("omop", "measurement", "value_as_number")
#' }
#' @export
omopNumericQuantilesDS <- function(omop_symbol, table, value_col,
                                    probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                    cohort_table = NULL, window = NULL,
                                    rounding = 2L) {
  handle <- .getHandle(omop_symbol)
  probs <- .ds_arg(probs)
  .profileNumericQuantiles(handle, table, value_col, probs,
                           cohort_table, window, rounding)
}

#' Get date counts (Aggregate)
#'
#' @description
#' Counts records by time bin (year, quarter, or month) with disclosure control
#' applied. Time periods with counts below the threshold are suppressed. Useful
#' for visualizing temporal trends in clinical data.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param date_col Character; date column (NULL = auto-detect)
#' @param granularity Character; "year", "quarter", or "month"
#' @param cohort_table Character; cohort temp table for filtering (NULL)
#' @param window List with start/end dates for filtering (NULL)
#' @return Data frame with period, n_records, suppressed
#' @examples
#' \dontrun{
#' trends <- omopDateCountsDS("omop", "condition_occurrence", granularity = "year")
#' }
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
#' @description
#' Returns a full drilldown profile for a single concept within a table,
#' including summary stats, numeric distribution, categorical values,
#' date coverage, and missingness. All count-based outputs are subject to
#' disclosure control.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param concept_id Integer; concept ID to drill into
#' @param concept_col Character; concept column name to drill into, or NULL
#'   for automatic detection based on the table's primary concept column.
#' @return Named list with drilldown results
#' @examples
#' \dontrun{
#' drilldown <- omopConceptDrilldownDS("omop", "condition_occurrence", 201826L)
#' }
#' @export
omopConceptDrilldownDS <- function(omop_symbol, table, concept_id,
                                   concept_col = NULL) {
  handle <- .getHandle(omop_symbol)
  .profileConceptDrilldown(handle, table, as.integer(concept_id),
                           concept_col = concept_col)
}

#' Locate concept across tables (Aggregate)
#'
#' @description
#' Searches all CDM tables with concept columns and returns a presence matrix
#' showing where the given concept IDs appear. Useful for understanding which
#' clinical domains contain data for a concept of interest.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param concept_ids Integer vector; concept IDs to locate
#' @return Data frame with table_name, concept_column, concept_id, n_records,
#'   n_persons
#' @examples
#' \dontrun{
#' locations <- omopLocateConceptDS("omop", c(201826L, 4329847L))
#' }
#' @export
omopLocateConceptDS <- function(omop_symbol, concept_ids) {
  handle <- .getHandle(omop_symbol)
  concept_ids <- .ds_arg(concept_ids)
  if (is.list(concept_ids)) concept_ids <- as.integer(unlist(concept_ids))
  .profileLocateConcept(handle, as.integer(concept_ids))
}

#' Get safe numeric cutpoints (Aggregate)
#'
#' @description
#' Returns bin edges that are safe to use as filter thresholds. Each bin is
#' guaranteed to contain enough persons to pass the disclosure threshold,
#' preventing indirect identification via precise numeric filtering.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param column Character; numeric column name
#' @param concept_id Integer or NULL; concept filter
#' @param n_bins Integer; target number of bins (default 10)
#' @return List with breaks (numeric vector) and counts (integer vector)
#' @examples
#' \dontrun{
#' cuts <- omopSafeCutpointsDS("omop", "measurement", "value_as_number")
#' }
#' @export
omopSafeCutpointsDS <- function(omop_symbol, table, column,
                                 concept_id = NULL, n_bins = 10L) {
  handle <- .getHandle(omop_symbol)
  .profileSafeCutpoints(handle, table, column, concept_id, as.integer(n_bins))
}

# --- Achilles aggregate methods ---

#' Check Achilles availability (Aggregate)
#'
#' @description
#' Checks whether pre-computed Achilles results are available in the results
#' schema. Returns the availability status and the number of analyses found.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Named list with availability status
#' @examples
#' \dontrun{
#' status <- omopAchillesStatusDS("omop")
#' }
#' @export
omopAchillesStatusDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .achillesStatus(handle)
}

#' List Achilles analyses (Aggregate)
#'
#' @description
#' Returns the catalog of available Achilles analyses, optionally filtered by
#' clinical domain. Each entry includes analysis ID, name, and description.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param domain Character; optional domain filter (e.g., "Person", "Condition")
#' @return Data frame with analysis catalog
#' @examples
#' \dontrun{
#' analyses <- omopAchillesAnalysesDS("omop", domain = "Person")
#' }
#' @export
omopAchillesAnalysesDS <- function(omop_symbol, domain = NULL) {
  handle <- .getHandle(omop_symbol)
  .achillesListAnalyses(handle, domain)
}

#' Get Achilles count results (Aggregate)
#'
#' @description
#' Returns count-based Achilles results for the given analysis IDs with
#' server-controlled disclosure thresholds. Arbitrary stratum filtering and
#' client-supplied min_cell_count are intentionally not supported to prevent
#' probing attacks and threshold weakening.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param analysis_ids Integer vector; analysis IDs to retrieve
#' @return Data frame with analysis results
#' @examples
#' \dontrun{
#' results <- omopAchillesResultsDS("omop", c(1L, 2L, 3L))
#' }
#' @export
omopAchillesResultsDS <- function(omop_symbol, analysis_ids) {
  handle <- .getHandle(omop_symbol)
  analysis_ids <- .ds_arg(analysis_ids)
  if (is.list(analysis_ids)) analysis_ids <- as.integer(unlist(analysis_ids))
  .achillesGetResults(handle, analysis_ids)
}

#' Get Achilles distribution results (Aggregate)
#'
#' @description
#' Returns distribution statistics (average, standard deviation, median,
#' percentiles) for the given Achilles analysis IDs. Extreme values (min/max)
#' are never returned to prevent identification of outlier individuals.
#' Arbitrary stratum filtering is not supported.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param analysis_ids Integer vector; analysis IDs to retrieve
#' @return Data frame with distribution statistics (no min/max)
#' @examples
#' \dontrun{
#' dists <- omopAchillesDistributionDS("omop", c(103L, 105L))
#' }
#' @export
omopAchillesDistributionDS <- function(omop_symbol, analysis_ids) {
  handle <- .getHandle(omop_symbol)
  analysis_ids <- .ds_arg(analysis_ids)
  if (is.list(analysis_ids)) analysis_ids <- as.integer(unlist(analysis_ids))
  .achillesGetDistributions(handle, analysis_ids)
}

#' Get Achilles analysis catalog (Aggregate)
#'
#' @description
#' Returns the full catalog of available Achilles analyses, either from the
#' achilles_analysis table or dynamically discovered from the results tables.
#' Includes analysis ID, name, description, and result type for each entry.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with analysis catalog
#' @examples
#' \dontrun{
#' catalog <- omopAchillesCatalogDS("omop")
#' }
#' @export
omopAchillesCatalogDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .achillesDiscoverCatalog(handle)
}

# --- OHDSI Results aggregate methods ---

#' Check OHDSI result tool availability (Aggregate)
#'
#' @description
#' Scans the database for pre-computed result tables from OHDSI tools
#' (DQD, CohortDiagnostics, CohortIncidence, Characterization) and
#' returns per-tool availability status.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Named list with per-tool availability
#' @examples
#' \dontrun{
#' status <- omopOhdsiStatusDS("omop")
#' }
#' @export
omopOhdsiStatusDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .ohdsiStatus(handle)
}

#' List discovered OHDSI result tables (Aggregate)
#'
#' @description
#' Returns a data frame of all OHDSI result tables found in the database,
#' including tool identification, qualified names, and row counts.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with table_name, tool_id, tool_name, qualified_name, n_rows
#' @examples
#' \dontrun{
#' tables <- omopOhdsiTablesDS("omop")
#' }
#' @export
omopOhdsiTablesDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .ohdsiFindResultTables(handle)
}

#' Query an OHDSI result table (Aggregate)
#'
#' @description
#' Reads rows from a pre-computed OHDSI result table with server-controlled
#' disclosure thresholds. Sensitive columns (SQL, JSON definitions) are
#' automatically excluded. Count columns are subject to small-cell suppression.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table_name Character; which result table to query
#' @param columns Character vector; columns to select (NULL = all safe columns)
#' @param filters Named list; WHERE conditions
#' @param order_by Character; ORDER BY column
#' @param limit Integer; max rows (capped at 5000)
#' @param tool_id Character; optional tool identifier for registry lookup
#' @return Data frame with disclosure control applied
#' @examples
#' \dontrun{
#' results <- omopOhdsiResultsDS("omop", "dqdashboard_results")
#' }
#' @export
omopOhdsiResultsDS <- function(omop_symbol, table_name, columns = NULL,
                                filters = NULL, order_by = NULL,
                                limit = 5000L, tool_id = NULL) {
  handle <- .getHandle(omop_symbol)
  filters <- .ds_arg(filters)
  .ohdsiGetResults(handle, table_name, columns, filters, order_by, limit,
                    tool_id)
}

#' Get OHDSI tool summary (Aggregate)
#'
#' @description
#' Returns a summary of results for a specific OHDSI tool, including
#' which tables are present and their row counts.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param tool_id Character; which tool to summarize
#' @return Named list with tool-specific summary
#' @examples
#' \dontrun{
#' summary <- omopOhdsiSummaryDS("omop", "dqd")
#' }
#' @export
omopOhdsiSummaryDS <- function(omop_symbol, tool_id) {
  handle <- .getHandle(omop_symbol)
  .ohdsiGetSummary(handle, tool_id)
}

# --- Query library methods ---

#' List query library templates (Aggregate)
#'
#' @description
#' Returns metadata for all available query templates that pass safety
#' classification. Queries are sourced from the curated allowlist and
#' Markdown templates in \code{inst/queries/queries/}.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param domain Character; optional domain/group filter (e.g., "Condition")
#' @param provider Character; query provider ("native" or "all")
#' @return Data frame with query ID, name, group, description, mode, class,
#'   poolable flag, CDM version, and number of input parameters
#' @examples
#' \dontrun{
#' queries <- omopQueryListDS("omop", domain = "Condition")
#' }
#' @export
omopQueryListDS <- function(omop_symbol, domain = NULL,
                               provider = "native") {
  handle <- .getHandle(omop_symbol)
  .query_list(handle, domain, provider)
}

#' Get query template details (Aggregate)
#'
#' @description
#' Returns full metadata for a specific query template, including input
#' parameters, output schema, and sensitive field annotations. Used by the
#' client to render parameter forms and validate inputs before execution.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param query_id Character; the query ID from the query library
#' @return Named list with query metadata (id, name, description, inputs,
#'   outputs, sensitive_fields, class, poolable)
#' @examples
#' \dontrun{
#' template <- omopQueryGetDS("omop", "condition_prevalence")
#' }
#' @export
omopQueryGetDS <- function(omop_symbol, query_id) {
  handle <- .getHandle(omop_symbol)
  .validateIdentifier(query_id, "query_id")
  .query_get(handle, query_id)
}

#' Execute a query template (Aggregate)
#'
#' @description
#' Executes a query template against the database with DataSHIELD-aligned
#' disclosure controls. Only queries classified as SAFE_AGGREGATE can be
#' executed in aggregate mode. Schema placeholders are automatically resolved
#' from the OMOP handle.
#'
#' Disclosure controls applied:
#' \itemize{
#'   \item Sensitive count columns suppressed below \code{nfilter.tab}
#'   \item Output rows capped at 5000 to prevent long-tail disclosure
#'   \item BLOCKED queries cannot be executed
#'   \item SAFE_ASSIGN queries cannot be executed in aggregate mode
#' }
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param query_id Character; the query ID from the query library
#' @param inputs Named list; parameter values for the query template
#' @param mode Character; "aggregate" (default, returns data to client) or
#'   "assign" (keeps data server-side)
#' @return For aggregate mode: disclosure-controlled data frame.
#'   For assign mode: invisible TRUE.
#' @examples
#' \dontrun{
#' result <- omopQueryExecDS("omop", "condition_prevalence", inputs = list())
#' }
#' @export
omopQueryExecDS <- function(omop_symbol, query_id,
                               inputs = list(),
                               mode = "aggregate") {
  handle <- .getHandle(omop_symbol)
  .validateIdentifier(query_id, "query_id")
  inputs <- .ds_arg(inputs)
  mode <- match.arg(mode, c("aggregate", "assign"))
  .query_exec(handle, query_id, inputs, mode)
}
