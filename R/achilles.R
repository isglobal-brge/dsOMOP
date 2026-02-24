# ==============================================================================
# dsOMOP v2 - Achilles Query Functions
# ==============================================================================
# Internal functions for querying pre-computed Achilles aggregate statistics.
# All results are disclosure-controlled via .suppressSmallCounts().
# ==============================================================================

# --- Analysis Catalog --------------------------------------------------------

#' Hardcoded catalog of supported Achilles analysis IDs
#' @return data.frame with analysis_id, analysis_name, domain, stratum_1_name,
#'   stratum_2_name, result_table
#' @keywords internal
.achilles_catalog <- function() {
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

# --- Domain-to-analysis mapping -----------------------------------------------

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

# --- Core query functions -----------------------------------------------------

#' Check Achilles availability
#'
#' @param handle CDM handle
#' @return list(available, tables, n_analyses, n_heel_warnings)
#' @keywords internal
.achillesStatus <- function(handle) {
  achilles_tables <- c("achilles_results", "achilles_results_dist",
                        "achilles_heel_results")

  # Check which tables exist
  bp <- .buildBlueprint(handle)
  found <- intersect(
    achilles_tables,
    bp$tables$table_name[bp$tables$present_in_db]
  )

  if (length(found) == 0) {
    return(list(available = FALSE, tables = character(0),
                n_analyses = 0L, n_heel_warnings = 0L))
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
       n_analyses = n_analyses, n_heel_warnings = n_heel)
}

#' List available Achilles analyses
#'
#' @param handle CDM handle
#' @param domain Character; optional domain filter
#' @return data.frame from the analysis catalog, filtered by domain if given
#' @keywords internal
.achillesListAnalyses <- function(handle, domain = NULL) {
  catalog <- .achilles_catalog()

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
#' @param handle CDM handle
#' @param analysis_ids Integer vector; which analysis IDs to retrieve
#' @param stratum_filters Named list; e.g. list(stratum_1 = "201820")
#' @param min_cell_count Integer; minimum count for disclosure (NULL = use default)
#' @return data.frame with analysis_id, stratum_1..5, count_value
#' @keywords internal
.achillesGetResults <- function(handle, analysis_ids,
                                 stratum_filters = NULL,
                                 min_cell_count = NULL) {
  bp <- .buildBlueprint(handle)
  if (!"achilles_results" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    return(data.frame(analysis_id = integer(0), stratum_1 = character(0),
                      stratum_2 = character(0), stratum_3 = character(0),
                      stratum_4 = character(0), stratum_5 = character(0),
                      count_value = numeric(0), stringsAsFactors = FALSE))
  }

  qualified <- .qualifyTable(handle, "achilles_results",
                              handle$results_schema)
  analysis_ids <- as.integer(analysis_ids)

  # Build WHERE clause
  id_list <- paste(analysis_ids, collapse = ", ")
  where_parts <- paste0("analysis_id IN (", id_list, ")")

  if (!is.null(stratum_filters) && is.list(stratum_filters)) {
    for (nm in names(stratum_filters)) {
      if (nm %in% c("stratum_1", "stratum_2", "stratum_3",
                     "stratum_4", "stratum_5")) {
        val <- as.character(stratum_filters[[nm]])
        where_parts <- c(where_parts,
          paste0(nm, " = '", gsub("'", "''", val), "'"))
      }
    }
  }

  sql <- paste0(
    "SELECT analysis_id, stratum_1, stratum_2, stratum_3, stratum_4, ",
    "stratum_5, count_value FROM ", qualified,
    " WHERE ", paste(where_parts, collapse = " AND "),
    " ORDER BY analysis_id, stratum_1, stratum_2"
  )

  result <- tryCatch(.executeQuery(handle, sql),
                     error = function(e) {
    data.frame(analysis_id = integer(0), stratum_1 = character(0),
               stratum_2 = character(0), stratum_3 = character(0),
               stratum_4 = character(0), stratum_5 = character(0),
               count_value = numeric(0), stringsAsFactors = FALSE)
  })

  if (nrow(result) == 0) return(result)

  # Disclosure control: suppress small counts
  result <- .suppressSmallCounts(result, "count_value")

  result
}

#' Get Achilles distribution results
#'
#' @param handle CDM handle
#' @param analysis_ids Integer vector
#' @param stratum_filters Named list; optional stratum filters
#' @return data.frame with analysis_id, stratum_1..5, count_value,
#'   min/max/avg/stdev/median/p10/p25/p75/p90
#' @keywords internal
.achillesGetDistributions <- function(handle, analysis_ids,
                                       stratum_filters = NULL) {
  bp <- .buildBlueprint(handle)
  empty <- data.frame(
    analysis_id = integer(0), stratum_1 = character(0),
    stratum_2 = character(0), stratum_3 = character(0),
    stratum_4 = character(0), stratum_5 = character(0),
    count_value = numeric(0), min_value = numeric(0),
    max_value = numeric(0), avg_value = numeric(0),
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
  where_parts <- paste0("analysis_id IN (", id_list, ")")

  if (!is.null(stratum_filters) && is.list(stratum_filters)) {
    for (nm in names(stratum_filters)) {
      if (nm %in% c("stratum_1", "stratum_2", "stratum_3",
                     "stratum_4", "stratum_5")) {
        val <- as.character(stratum_filters[[nm]])
        where_parts <- c(where_parts,
          paste0(nm, " = '", gsub("'", "''", val), "'"))
      }
    }
  }

  sql <- paste0(
    "SELECT analysis_id, stratum_1, stratum_2, stratum_3, stratum_4, ",
    "stratum_5, count_value, min_value, max_value, avg_value, stdev_value, ",
    "median_value, p10_value, p25_value, p75_value, p90_value FROM ",
    qualified,
    " WHERE ", paste(where_parts, collapse = " AND "),
    " ORDER BY analysis_id, stratum_1"
  )

  result <- tryCatch(.executeQuery(handle, sql), error = function(e) empty)
  if (nrow(result) == 0) return(result)

  # Disclosure: suppress entire row stats if count_value is too small
  settings <- .omopDisclosureSettings()
  threshold <- settings$nfilter_tab
  small <- !is.na(result$count_value) & result$count_value < threshold
  if (any(small)) {
    result$count_value[small] <- NA_real_
    stat_cols <- c("min_value", "max_value", "avg_value", "stdev_value",
                   "median_value", "p10_value", "p25_value", "p75_value",
                   "p90_value")
    for (col in stat_cols) {
      if (col %in% names(result)) result[[col]][small] <- NA_real_
    }
  }

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
