# Module: Achilles Query Functions
# Internal functions for querying pre-computed Achilles aggregate statistics.

# Analysis Catalog

#' Static catalog of known Achilles analysis IDs (fallback)
#' @return data.frame with analysis_id, analysis_name, domain, stratum_1_name,
#'   stratum_2_name, result_table
#' @keywords internal
.achilles_catalog_static <- function() {
  data.frame(
    analysis_id = c(
      0L, 1L, 2L, 3L, 5L, 8L,
      101L, 108L, 113L, 116L, 117L,
      200L, 201L,
      400L, 401L, 403L, 404L,
      700L, 701L,
      800L,
      900L,
      1800L, 1801L,
      600L, 601L,
      2100L
    ),
    analysis_name = c(
      "Number of persons", "Number of persons by gender",
      "Number of persons by year of birth",
      "Year of birth distribution",
      "Number of persons by ethnicity",
      "Number of persons by race",
      "Number of persons by year of observation",
      "Observation period length distribution",
      "Age at first observation distribution",
      "Number of persons with observation period start by month",
      "Number of persons with observation period end by month",
      "Number of visits by visit concept",
      "Number of visits by visit concept and month",
      "Number of persons with condition by concept",
      "Number of condition records by concept and month",
      "Number of persons with condition by concept and gender",
      "Condition age at first diagnosis distribution",
      "Number of persons with drug by concept",
      "Number of drug records by concept and month",
      "Number of persons with observation by concept",
      "Number of persons by location",
      "Number of persons with measurement by concept",
      "Number of measurement records by concept and month",
      "Number of persons with procedure by concept",
      "Number of procedure records by concept and month",
      "Number of persons with device by concept"
    ),
    domain = c(
      "person", "person", "person", "person", "person", "person",
      "observation_period", "observation_period", "observation_period",
      "observation_period", "observation_period",
      "visit", "visit",
      "condition", "condition", "condition", "condition",
      "drug", "drug",
      "observation",
      "person",
      "measurement", "measurement",
      "procedure", "procedure",
      "device"
    ),
    stratum_1_name = c(
      NA, "gender_concept_id", "year_of_birth", NA,
      "ethnicity_concept_id", "race_concept_id",
      "calendar_year", NA, NA, "calendar_month", "calendar_month",
      "visit_concept_id", "visit_concept_id",
      "concept_id", "concept_id", "concept_id", "concept_id",
      "concept_id", "concept_id",
      "concept_id",
      "location_id",
      "concept_id", "concept_id",
      "concept_id", "concept_id",
      "concept_id"
    ),
    stratum_2_name = c(
      NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA,
      NA, "calendar_month",
      NA, "calendar_month", "gender_concept_id", NA,
      NA, "calendar_month",
      NA,
      NA,
      NA, "calendar_month",
      NA, "calendar_month",
      NA
    ),
    result_table = c(
      "achilles_results", "achilles_results", "achilles_results",
      "achilles_results_dist",
      "achilles_results", "achilles_results",
      "achilles_results", "achilles_results_dist", "achilles_results_dist",
      "achilles_results", "achilles_results",
      "achilles_results", "achilles_results",
      "achilles_results", "achilles_results", "achilles_results",
      "achilles_results_dist",
      "achilles_results", "achilles_results",
      "achilles_results",
      "achilles_results",
      "achilles_results", "achilles_results",
      "achilles_results", "achilles_results",
      "achilles_results"
    ),
    stringsAsFactors = FALSE
  )
}

# Domain-to-analysis mapping

#' Mapping of OMOP CDM domains to their Achilles analysis IDs
#'
#' Named list where each element is an integer vector of analysis IDs
#' associated with the domain.
#'
#' @keywords internal
.achilles_domain_analyses <- list(
  person             = c(0L, 1L, 2L, 3L, 5L, 8L, 900L),
  observation_period = c(101L, 108L, 113L, 116L, 117L),
  condition          = c(400L, 401L, 403L, 404L),
  drug               = c(700L, 701L),
  measurement        = c(1800L, 1801L),
  procedure          = c(600L, 601L),
  observation        = c(800L),
  visit              = c(200L, 201L),
  device             = c(2100L)
)

# Core query functions

#' Check Achilles availability
#'
#' @param handle CDM handle
#' @return list(available, tables, n_analyses, n_heel_warnings)
#' @keywords internal
.achillesStatus <- function(handle) {
  achilles_tables <- c("achilles_analysis", "achilles_results",
                        "achilles_results_dist", "achilles_heel_results")

  # Check which tables exist
  bp <- .buildBlueprint(handle)
  found <- intersect(
    achilles_tables,
    bp$tables$table_name[bp$tables$present_in_db]
  )

  # At minimum need achilles_results to be considered available
  if (!"achilles_results" %in% found && !"achilles_results_dist" %in% found) {
    return(list(available = FALSE, tables = character(0),
                n_analyses = 0L, n_heel_warnings = 0L,
                has_analysis_table = FALSE))
  }

  n_analyses <- 0L
  n_heel <- 0L

  if ("achilles_results" %in% found) {
    qualified <- .qualifyTable(handle, "achilles_results",
                                handle$results_schema)
    sql <- paste0("SELECT COUNT(DISTINCT analysis_id) AS n FROM ", qualified)
    n_analyses <- tryCatch(
      as.integer(.executeQuery(handle, sql)$n[1]),
      error = function(e) 0L
    )
  }

  if ("achilles_heel_results" %in% found) {
    qualified <- .qualifyTable(handle, "achilles_heel_results",
                                handle$results_schema)
    sql <- paste0("SELECT COUNT(*) AS n FROM ", qualified)
    n_heel <- tryCatch(
      as.integer(.executeQuery(handle, sql)$n[1]),
      error = function(e) 0L
    )
  }

  list(available = TRUE, tables = found,
       n_analyses = n_analyses, n_heel_warnings = n_heel,
       has_analysis_table = "achilles_analysis" %in% found)
}

#' List available Achilles analyses
#'
#' @param handle CDM handle
#' @param domain Character; optional domain filter
#' @return data.frame from the analysis catalog, filtered by domain if given
#' @keywords internal
.achillesListAnalyses <- function(handle, domain = NULL) {
  catalog <- .achilles_catalog_static()

  if (!is.null(domain)) {
    domain <- tolower(trimws(domain))
    if (domain %in% names(.achilles_domain_analyses)) {
      ids <- .achilles_domain_analyses[[domain]]
      catalog <- catalog[catalog$analysis_id %in% ids, , drop = FALSE]
    }
  }

  rownames(catalog) <- NULL
  catalog
}

#' Get Achilles results (count-based analyses)
#'
#' Returns count-based Achilles results for the requested analysis IDs.
#' Arbitrary stratum filtering is not supported to prevent targeted probing
#' of rare strata (Fix C). Rows with count_value below nfilter.tab are
#' dropped entirely ("no hints" policy).
#'
#' @param handle CDM handle
#' @param analysis_ids Integer vector; which analysis IDs to retrieve
#' @return data.frame with analysis_id, stratum_1..5, count_value
#' @keywords internal
.achillesGetResults <- function(handle, analysis_ids) {
  bp <- .buildBlueprint(handle)
  empty <- data.frame(analysis_id = integer(0), stratum_1 = character(0),
                      stratum_2 = character(0), stratum_3 = character(0),
                      stratum_4 = character(0), stratum_5 = character(0),
                      count_value = numeric(0), stringsAsFactors = FALSE)

  if (!"achilles_results" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    return(empty)
  }

  qualified <- .qualifyTable(handle, "achilles_results",
                              handle$results_schema)
  analysis_ids <- as.integer(analysis_ids)

  id_list <- paste(analysis_ids, collapse = ", ")

  sql <- paste0(
    "SELECT analysis_id, stratum_1, stratum_2, stratum_3, stratum_4, ",
    "stratum_5, count_value FROM ", qualified,
    " WHERE analysis_id IN (", id_list, ")",
    " ORDER BY analysis_id, stratum_1, stratum_2"
  )

  result <- tryCatch(.executeQuery(handle, sql), error = function(e) empty)
  if (nrow(result) == 0) return(result)

  # Disclosure control: suppress small counts (drop rows, no hints)
  result <- .suppressSmallCounts(result, "count_value")

  result
}

#' Get Achilles distribution results
#'
#' Returns distribution statistics from achilles_results_dist. Extreme values
#' (min_value, max_value) are never returned to prevent identification of
#' outlier individuals. Rows with count_value below the disclosure threshold
#' are dropped entirely (no NA skeleton rows / "no hints" policy).
#'
#' @param handle CDM handle
#' @param analysis_ids Integer vector
#' @return data.frame with analysis_id, stratum_1..5, count_value,
#'   avg/stdev/median/p10/p25/p75/p90 (no min/max)
#' @keywords internal
.achillesGetDistributions <- function(handle, analysis_ids) {
  bp <- .buildBlueprint(handle)
  empty <- data.frame(
    analysis_id = integer(0), stratum_1 = character(0),
    stratum_2 = character(0), stratum_3 = character(0),
    stratum_4 = character(0), stratum_5 = character(0),
    count_value = numeric(0), avg_value = numeric(0),
    stdev_value = numeric(0), median_value = numeric(0),
    p10_value = numeric(0), p25_value = numeric(0),
    p75_value = numeric(0), p90_value = numeric(0),
    stringsAsFactors = FALSE
  )

  if (!"achilles_results_dist" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    return(empty)
  }

  qualified <- .qualifyTable(handle, "achilles_results_dist",
                              handle$results_schema)
  analysis_ids <- as.integer(analysis_ids)

  id_list <- paste(analysis_ids, collapse = ", ")

  # Fix B: min_value and max_value are never selected — extreme values can
  # identify individuals (e.g. unique age, unique lab result).
  sql <- paste0(
    "SELECT analysis_id, stratum_1, stratum_2, stratum_3, stratum_4, ",
    "stratum_5, count_value, avg_value, stdev_value, ",
    "median_value, p10_value, p25_value, p75_value, p90_value FROM ",
    qualified,
    " WHERE analysis_id IN (", id_list, ")",
    " ORDER BY analysis_id, stratum_1"
  )

  result <- tryCatch(.executeQuery(handle, sql), error = function(e) empty)
  if (nrow(result) == 0) return(result)

  # Fix A: use the same row-dropping suppression as count-results
  # (no NA skeleton rows — "no hints" policy)
  result <- .suppressSmallCounts(result, "count_value")

  result
}

#' Get Achilles Heel warnings
#'
#' @param handle CDM handle
#' @return data.frame with analysis_id, achilles_heel_warning, rule_id,
#'   record_count
#' @keywords internal
.achillesGetHeelResults <- function(handle) {
  bp <- .buildBlueprint(handle)
  empty <- data.frame(
    analysis_id = integer(0),
    achilles_heel_warning = character(0),
    rule_id = integer(0),
    record_count = numeric(0),
    stringsAsFactors = FALSE
  )

  if (!"achilles_heel_results" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    return(empty)
  }

  qualified <- .qualifyTable(handle, "achilles_heel_results",
                              handle$results_schema)
  sql <- paste0("SELECT analysis_id, achilles_heel_warning, rule_id, ",
                "record_count FROM ", qualified,
                " ORDER BY rule_id")

  tryCatch(.executeQuery(handle, sql), error = function(e) empty)
}

# Dynamic Catalog Discovery

#' Get Achilles analysis catalog from database
#'
#' Queries the achilles_analysis table if available. Falls back to
#' querying DISTINCT analysis_id from achilles_results with the
#' hardcoded catalog as metadata enrichment.
#'
#' @param handle CDM handle
#' @return data.frame with analysis_id, analysis_name, domain, stratum_*_name, result_table
#' @keywords internal
.achillesDiscoverCatalog <- function(handle) {
  bp <- .buildBlueprint(handle)

  # Try achilles_analysis table first
  if ("achilles_analysis" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    qualified <- .qualifyTable(handle, "achilles_analysis",
                                handle$results_schema %||% handle$cdm_schema)
    sql <- paste0("SELECT analysis_id, analysis_name, stratum_1_name, ",
                  "stratum_2_name, stratum_3_name, stratum_4_name, stratum_5_name ",
                  "FROM ", qualified, " ORDER BY analysis_id")
    catalog <- tryCatch(.executeQuery(handle, sql), error = function(e) NULL)
    if (!is.null(catalog) && nrow(catalog) > 0) {
      names(catalog) <- tolower(names(catalog))
      # Add domain classification + result_table from analysis_id ranges
      catalog$domain <- .achillesAnalysisDomain(catalog$analysis_id)
      catalog$result_table <- .achillesAnalysisResultTable(catalog$analysis_id)
      return(catalog)
    }
  }

  # Fallback: query distinct analysis_ids from achilles_results
  avail_ids <- .achillesDiscoverIds(handle)
  if (length(avail_ids) == 0) return(.achilles_catalog_static())  # ultimate fallback

  # Filter static catalog to only available IDs
  static <- .achilles_catalog_static()
  found <- static[static$analysis_id %in% avail_ids, , drop = FALSE]

  # Add any unknown IDs with minimal metadata
  unknown_ids <- setdiff(avail_ids, static$analysis_id)
  if (length(unknown_ids) > 0) {
    unknown_rows <- data.frame(
      analysis_id = unknown_ids,
      analysis_name = paste("Analysis", unknown_ids),
      domain = .achillesAnalysisDomain(unknown_ids),
      stratum_1_name = NA_character_,
      stratum_2_name = NA_character_,
      result_table = .achillesAnalysisResultTable(unknown_ids),
      stringsAsFactors = FALSE
    )
    found <- rbind(found, unknown_rows)
  }
  found[order(found$analysis_id), , drop = FALSE]
}

#' Query distinct analysis_ids from achilles_results
#'
#' Scans both achilles_results and achilles_results_dist tables to discover
#' which analysis IDs have been populated.
#'
#' @param handle CDM handle object.
#' @return Sorted integer vector of unique analysis IDs found in the database.
#' @keywords internal
.achillesDiscoverIds <- function(handle) {
  bp <- .buildBlueprint(handle)
  ids <- integer(0)
  for (tbl in c("achilles_results", "achilles_results_dist")) {
    if (tbl %in% bp$tables$table_name[bp$tables$present_in_db]) {
      qualified <- .qualifyTable(handle, tbl,
                                  handle$results_schema %||% handle$cdm_schema)
      sql <- paste0("SELECT DISTINCT analysis_id FROM ", qualified)
      res <- tryCatch(.executeQuery(handle, sql), error = function(e) NULL)
      if (!is.null(res)) ids <- c(ids, as.integer(res$analysis_id))
    }
  }
  sort(unique(ids))
}

#' Classify analysis domain from analysis_id range
#'
#' Maps Achilles analysis IDs to their OMOP CDM domain based on the standard
#' numbering convention (e.g., 0-99 = person, 100-199 = observation_period).
#'
#' @param analysis_ids Integer vector of Achilles analysis IDs to classify.
#' @return Character vector of domain names, one per input ID.
#' @keywords internal
.achillesAnalysisDomain <- function(analysis_ids) {
  vapply(analysis_ids, function(id) {
    if (id < 100) return("person")
    if (id < 200) return("observation_period")
    if (id < 300) return("visit")
    if (id < 400) return("provider")
    if (id < 500) return("condition")
    if (id < 600) return("death")
    if (id < 700) return("procedure")
    if (id < 800) return("drug")
    if (id < 900) return("observation")
    if (id < 1000) return("location")
    if (id < 1100) return("care_site")
    if (id < 1800) return("other")
    if (id < 1900) return("measurement")
    if (id < 2000) return("cohort")
    if (id < 2200) return("device")
    "other"
  }, character(1))
}

#' Determine result table from analysis_id
#'
#' Identifies whether each analysis ID stores results in achilles_results
#' or achilles_results_dist based on the Achilles suffix convention.
#'
#' @param analysis_ids Integer vector of Achilles analysis IDs.
#' @return Character vector of table names ("achilles_results" or
#'   "achilles_results_dist").
#' @keywords internal
.achillesAnalysisResultTable <- function(analysis_ids) {
  # Distribution analyses (by Achilles convention): xx03, xx04, xx07, xx08
  dist_suffix <- analysis_ids %% 100
  ifelse(dist_suffix %in% c(3L, 4L, 7L, 8L), "achilles_results_dist", "achilles_results")
}
