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
      1L, 2L, 3L, 4L, 5L, 10L, 12L, 101L, 102L, 103L, 104L, 105L, 106L, 107L,
      108L, 109L, 113L, 200L, 201L, 202L, 203L, 206L, 220L, 221L, 400L, 401L,
      402L, 403L, 406L, 420L, 500L, 502L, 505L, 506L, 600L, 601L, 602L, 603L,
      606L, 620L, 700L, 701L, 702L, 703L, 706L, 715L, 716L, 717L, 720L, 800L,
      801L, 802L, 803L, 806L, 820L, 1800L, 1801L, 1802L, 1803L, 1806L, 1818L,
      1820L, 2000L, 2001L, 2002L, 2003L, 2100L, 2101L, 2102L, 2106L
    ),
    analysis_name = c(
      "Number of persons", "Number of persons by gender",
      "Number of persons by year of birth", "Number of persons by race",
      "Number of persons by ethnicity",
      "Number of persons by year of birth by gender",
      "Number of persons by race and ethnicity",
      "Number of persons by age at first observation period",
      "Number of persons by gender by age at first observation period",
      "Distribution of age at first observation period",
      "Distribution of age at first observation period by gender",
      "Length of observation (days) of first observation period",
      "Length of observation (days) of first observation period by gender",
      "Length of observation (days) of first observation period by age decile",
      "Number of persons by observation period length, 30d increments",
      "Number of persons with continuous observation in each year",
      "Number of persons by number of observation periods",
      "Number of persons with at least one visit, by visit_concept_id",
      "Number of visit records, by visit_concept_id",
      "Number of persons by visit start month, by visit_concept_id",
      "Number of distinct visit concepts per person",
      "Distribution of age by visit_concept_id",
      "Number of visit records by visit start month",
      "Number of persons by visit start year",
      "Number of persons with at least one condition, by condition_concept_id",
      "Number of condition records, by condition_concept_id",
      "Number of persons by condition start month, by condition_concept_id",
      "Number of distinct condition concepts per person",
      "Distribution of age by condition_concept_id",
      "Number of condition records by condition start month",
      "Number of persons with death, by cause_concept_id",
      "Number of persons by death month",
      "Number of death records, by death_type_concept_id",
      "Distribution of age at death by gender",
      "Number of persons with at least one procedure, by procedure_concept_id",
      "Number of procedure records, by procedure_concept_id",
      "Number of persons by procedure start month, by procedure_concept_id",
      "Number of distinct procedure concepts per person",
      "Distribution of age by procedure_concept_id",
      "Number of procedure records by procedure start month",
      "Number of persons with at least one drug, by drug_concept_id",
      "Number of drug records, by drug_concept_id",
      "Number of persons by drug start month, by drug_concept_id",
      "Number of distinct drug concepts per person",
      "Distribution of age by drug_concept_id",
      "Distribution of days_supply by drug_concept_id",
      "Distribution of refills by drug_concept_id",
      "Distribution of quantity by drug_concept_id",
      "Number of drug records by drug start month",
      "Number of persons with at least one observation, by observation_concept_id",
      "Number of observation records, by observation_concept_id",
      "Number of persons by observation start month, by observation_concept_id",
      "Number of distinct observation concepts per person",
      "Distribution of age by observation_concept_id",
      "Number of observation records by observation start month",
      "Number of persons with at least one measurement, by measurement_concept_id",
      "Number of measurement records, by measurement_concept_id",
      "Number of persons by measurement start month, by measurement_concept_id",
      "Number of distinct measurement concepts per person",
      "Distribution of age by measurement_concept_id",
      "Number of measurement records below/within/above normal range, by measurement_concept_id and unit_concept_id",
      "Number of measurement records by measurement start month",
      "Number of patients with at least 1 Dx and 1 Rx",
      "Number of patients with at least 1 Dx and 1 Proc",
      "Number of patients with at least 1 Meas, 1 Dx and 1 Rx",
      "Number of patients with at least 1 Visit",
      "Number of persons with at least one device, by device_concept_id",
      "Number of device records, by device_concept_id",
      "Number of device records by device start month",
      "Distribution of age by device_concept_id"
    ),
    domain = c(
      "person", "person", "person", "person", "person", "person", "person",
      "observation_period", "observation_period", "observation_period",
      "observation_period", "observation_period", "observation_period",
      "observation_period", "observation_period", "observation_period",
      "observation_period", "visit", "visit", "visit", "visit", "visit",
      "visit", "visit", "condition", "condition", "condition", "condition",
      "condition", "condition", "death", "death", "death", "death",
      "procedure", "procedure", "procedure", "procedure", "procedure",
      "procedure", "drug", "drug", "drug", "drug", "drug", "drug", "drug",
      "drug", "drug", "observation", "observation", "observation",
      "observation", "observation", "observation", "measurement",
      "measurement", "measurement", "measurement", "measurement",
      "measurement", "measurement", "general", "general", "general",
      "general", "device", "device", "device", "device"
    ),
    stratum_1_name = c(
      NA, "gender_concept_id", "year_of_birth", "race_concept_id",
      "ethnicity_concept_id", "year_of_birth", "race_concept_id", "age",
      "gender_concept_id", NA, "gender_concept_id", NA, "gender_concept_id",
      "age decile", "obs period length 30d", "calendar year",
      "number of observation periods", "visit_concept_id", "visit_concept_id",
      "visit_concept_id", NA, "visit_concept_id", "calendar month",
      "calendar year", "condition_concept_id", "condition_concept_id",
      "condition_concept_id", NA, "condition_concept_id", "calendar month",
      "cause_concept_id", "calendar month", "death_type_concept_id",
      "gender_concept_id", "procedure_concept_id", "procedure_concept_id",
      "procedure_concept_id", NA, "procedure_concept_id", "calendar month",
      "drug_concept_id", "drug_concept_id", "drug_concept_id", NA,
      "drug_concept_id", "drug_concept_id", "drug_concept_id",
      "drug_concept_id", "calendar month", "observation_concept_id",
      "observation_concept_id", "observation_concept_id", NA,
      "observation_concept_id", "calendar month", "measurement_concept_id",
      "measurement_concept_id", "measurement_concept_id", NA,
      "measurement_concept_id", "measurement_concept_id", "calendar month",
      NA, NA, NA, NA, "device_concept_id", "device_concept_id",
      "calendar month", "device_concept_id"
    ),
    stratum_2_name = c(
      NA, NA, NA, NA, NA, "gender_concept_id", "ethnicity_concept_id", NA,
      "age", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "calendar month", NA,
      "gender_concept_id", NA, NA, NA, NA, "calendar month", NA,
      "gender_concept_id", NA, NA, NA, NA, NA, NA, NA, "calendar month", NA,
      "gender_concept_id", NA, NA, NA, "calendar month", NA,
      "gender_concept_id", NA, NA, NA, NA, NA, NA, "calendar month", NA,
      "gender_concept_id", NA, NA, NA, "calendar month", NA,
      "gender_concept_id", "unit_concept_id", NA, NA, NA, NA, NA, NA, NA, NA,
      "gender_concept_id"
    ),
    result_table = c(
      "achilles_results", "achilles_results", "achilles_results",
      "achilles_results", "achilles_results", "achilles_results",
      "achilles_results", "achilles_results", "achilles_results",
      "achilles_results_dist", "achilles_results_dist", "achilles_results_dist",
      "achilles_results_dist", "achilles_results_dist", "achilles_results",
      "achilles_results", "achilles_results", "achilles_results",
      "achilles_results", "achilles_results", "achilles_results_dist",
      "achilles_results_dist", "achilles_results", "achilles_results",
      "achilles_results", "achilles_results", "achilles_results",
      "achilles_results_dist", "achilles_results_dist", "achilles_results",
      "achilles_results", "achilles_results", "achilles_results",
      "achilles_results_dist", "achilles_results", "achilles_results",
      "achilles_results", "achilles_results_dist", "achilles_results_dist",
      "achilles_results", "achilles_results", "achilles_results",
      "achilles_results", "achilles_results_dist", "achilles_results_dist",
      "achilles_results_dist", "achilles_results_dist", "achilles_results_dist",
      "achilles_results", "achilles_results", "achilles_results",
      "achilles_results", "achilles_results_dist", "achilles_results_dist",
      "achilles_results", "achilles_results", "achilles_results",
      "achilles_results", "achilles_results_dist", "achilles_results_dist",
      "achilles_results", "achilles_results", "achilles_results",
      "achilles_results", "achilles_results", "achilles_results",
      "achilles_results", "achilles_results", "achilles_results",
      "achilles_results_dist"
    ),
    unit = c(
      "person", "person", "person", "person", "person", "person", "person",
      "person", "person", "dist", "dist", "dist", "dist", "dist", "person",
      "person", "person", "person", "record", "person", "dist", "dist",
      "record", "person", "person", "record", "person", "dist", "dist",
      "record", "person", "person", "record", "dist", "person", "record",
      "person", "dist", "dist", "record", "person", "record", "person",
      "dist", "dist", "dist", "dist", "dist", "record", "person", "record",
      "person", "dist", "dist", "record", "person", "record", "person",
      "dist", "dist", "record", "record", "person", "person", "person",
      "person", "person", "record", "record", "dist"
    ),
    stringsAsFactors = FALSE
  )
}

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
                                .resolveResultsSchema(handle))
    sql <- paste0("SELECT COUNT(DISTINCT analysis_id) AS n FROM ", qualified)
    n_analyses <- tryCatch(
      as.integer(.executeQuery(handle, sql)$n[1]),
      error = function(e) 0L
    )
  }

  if ("achilles_heel_results" %in% found) {
    qualified <- .qualifyTable(handle, "achilles_heel_results",
                                .resolveResultsSchema(handle))
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
    catalog <- catalog[tolower(catalog$domain) == domain, , drop = FALSE]
  }

  rownames(catalog) <- NULL
  catalog
}

#' Get Achilles results (count-based analyses)
#'
#' Returns count-based Achilles results for the requested analysis IDs.
#'
#' @section Probing Prevention:
#' Achilles results are pre-computed aggregates (e.g., "n_persons by condition").
#' A probing attack would repeatedly call this function with different analysis
#' IDs and stratum values to reverse-engineer individual-level data. To prevent
#' this:
#' \itemize{
#'   \item \strong{No stratum filtering}: the function accepts only analysis IDs,
#'     not arbitrary stratum conditions. All strata for requested analyses are
#'     returned at once.
#'   \item \strong{Row-dropping suppression}: rows with
#'     \code{count_value < nfilter.tab} are dropped entirely (not returned as
#'     NA). This prevents the attacker from inferring that a rare subgroup
#'     exists but was suppressed.
#' }
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
                              .resolveResultsSchema(handle))
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

  # Disclosure control, in two orthogonal passes:
  # (1) Cell-value suppression: drop any row whose own count_value < nfilter.tab.
  #     For person-unit analyses the count IS the person count, so this fully
  #     gates them; it also removes record cells with trivially small counts.
  result <- .suppressSmallCounts(result, "count_value")

  # (2) Person-gating: record-unit analyses (e.g. 401) count RECORDS, so a large
  #     count_value can still hide a sub-threshold PERSON count. Drop record
  #     cells whose underlying distinct-person count (a precomputed sibling x00
  #     row, or a targeted companion CDM query) is < nfilter.tab. Person- and
  #     distribution-unit rows are untouched. Runs after (1) so a record cell
  #     survives only if BOTH its record count and its person count clear the
  #     threshold. Guarded so the common path (no record analyses) is free.
  if (nrow(result) > 0 && any(result$analysis_id %in% .achillesRecordGatedIds())) {
    result <- .achillesPersonGate(handle, result)
  }

  result
}

#' Get Achilles distribution results
#'
#' Returns distribution statistics from achilles_results_dist with three
#' layers of disclosure control applied:
#'
#' @section Security Controls:
#' \enumerate{
#'   \item \strong{Extreme value exclusion}: \code{min_value} and
#'     \code{max_value} are never selected from the database. These values
#'     identify outlier individuals (e.g., "maximum age = 115 years").
#'   \item \strong{Row-dropping suppression}: Rows with
#'     \code{count_value < nfilter.tab} are dropped entirely. The "no hints"
#'     policy means suppressed rows are absent (not NA), preventing
#'     subtraction attacks.
#'   \item \strong{Small-sample summary-stat suppression}: When
#'     \code{count_value < nfilter_dist}, ALL summary-statistic columns
#'     (\code{avg_value}, \code{stdev_value} and every percentile) are set to
#'     NA. With very few observations, even p25/p75 — or a mean plus its SD —
#'     approximate individual values.
#' }
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
                              .resolveResultsSchema(handle))
  analysis_ids <- as.integer(analysis_ids)

  id_list <- paste(analysis_ids, collapse = ", ")

  # EXTREME VALUE SUPPRESSION: min_value and max_value are deliberately
  # excluded from the SELECT. Extreme values (e.g., "oldest person is 115",
  # "highest lab result is 2500") can identify individuals at the tails of
  # a distribution when combined with external data. This exclusion is
  # unconditional — even for large populations, extreme values are never
  # returned to the client.
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

  # ROW-DROPPING SUPPRESSION: Rows with count_value < nfilter.tab are
  # dropped entirely (not replaced with NA). The "no hints" policy ensures
  # that the absence of a row cannot be distinguished from non-existence,
  # preventing subtraction attacks (total - visible = hidden group size).
  result <- .suppressSmallCounts(result, "count_value")

  # SMALL-SAMPLE SUMMARY-STAT SUPPRESSION: When count_value < nfilter_dist,
  # even median/p25/p75 can approximate individual values (e.g., with n=3,
  # the median IS one person's value). The SAME applies to avg_value and
  # stdev_value: a mean over n=2 plus its SD algebraically pins both
  # individual values, and a mean over n=3 is near-determined. Set ALL
  # summary-statistic columns (mean, SD, and every percentile) to NA for
  # these small-sample rows while keeping count_value visible.
  settings <- .omopDisclosureSettings()
  nfilter_dist <- settings$nfilter_dist %||% 10L
  mask_cols <- c("avg_value", "stdev_value", "p10_value", "p25_value",
                 "median_value", "p75_value", "p90_value")
  mask_cols <- intersect(mask_cols, names(result))
  if (length(mask_cols) > 0 && "count_value" %in% names(result)) {
    small_rows <- !is.na(result$count_value) & result$count_value < nfilter_dist
    result[small_rows, mask_cols] <- NA_real_
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
                              .resolveResultsSchema(handle))
  sql <- paste0("SELECT analysis_id, achilles_heel_warning, rule_id, ",
                "record_count FROM ", qualified,
                " ORDER BY rule_id")

  result <- tryCatch(.executeQuery(handle, sql), error = function(e) empty)
  if (nrow(result) == 0) return(result)

  # DISCLOSURE CONTROL: Heel rows are data-quality rule indicators, not
  # population cells. Keep the rows (the data controller needs to see WHICH
  # rules fired) but strip the disclosive specifics:
  # (1) NA-mask record_count below nfilter.tab — the rule-fired indicator stays
  #     visible, but the exact (often tiny) failing-record count is removed.
  # (2) Achilles Heel messages routinely interpolate raw counts/values into the
  #     free text (e.g. "5 persons have a birth year after their death year"),
  #     which bypasses the column-level filter. Scrub numeric runs, replacing
  #     each with "N".
  settings <- .omopDisclosureSettings()
  threshold <- settings$nfilter_tab
  if ("record_count" %in% names(result)) {
    rc <- suppressWarnings(as.numeric(result$record_count))
    result$record_count[!is.na(rc) & rc < threshold] <- NA
  }
  if ("achilles_heel_warning" %in% names(result)) {
    result$achilles_heel_warning <- gsub(
      "[0-9][0-9.,]*", "N", as.character(result$achilles_heel_warning))
  }
  result
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
                                .resolveResultsSchema(handle))
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
                                  .resolveResultsSchema(handle))
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
