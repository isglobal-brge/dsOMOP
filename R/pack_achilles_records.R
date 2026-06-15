# Module: Achilles record-count catalog entries (native live compute)
#
# Group ACH-E. The record-unit Achilles count analyses, ported from the
# precomputed-results adapter (.achillesGetResults + .achillesPersonGate +
# .achillesCompanionPersonCounts) to native, live-computing catalog entries.
#
# Each entry computes its record volume DIRECTLY from the CDM domain table
# (COUNT(*) AS count_value) AND, in the SAME query over the SAME GROUP BY,
# the distinct-person companion (COUNT(DISTINCT person_id) AS n_persons) that
# makes the record cell disclosure-safe. This single query replaces the old
# two-step machinery: the precomputed record table read PLUS the separate
# sibling/companion person-count lookup. The entry declares
# count_cols = c("count_value", "n_persons") with person_id_col = "n_persons"
# and meta$adapter = "achilles_live", so the SINGLE .omopAnalysisGate routes it
# through its GENERIC record branch (analysis_catalog.R L3278-3291): suppress on
# the distinct-person companion n_persons, then suppress + band count_value.
# Nothing here reads achilles_results / achilles_results_dist.
#
# Entry ids are unchanged ("dsomop:achilles.<id>") so existing references keep
# resolving; only the compute changes (read-results -> live-compute). Concept
# strata are translated to names by default (LEFT JOIN concept); no
# *_source_value / free-text is ever selected. Population is restricted to
# ctx$scoped_cohort (INNER JOIN on subject_id) or computed cohort-wide when
# un-scoped (whole-DB / database-characterization semantics).

#' Domain-spec table for the record-count Achilles analyses (ACH-E)
#'
#' One row per ported analysis_id, mirroring the same domain-spec shape ACH-D
#' uses: the CDM \code{table}, its \code{concept_col} (the domain concept the
#' x01 analyses group by) and \code{date_col} (the event date the xx20 monthly
#' analyses group by), plus a \code{kind} selecting the stratification:
#' \code{"concept"} (x01, group by the domain concept id), \code{"month"} (xx20,
#' group by the YYYYMM start month), \code{"death_type"} (505, group by
#' \code{death_type_concept_id}) and \code{"range"} (1818, group by
#' measurement_concept_id x unit_concept_id x normal-range bucket). All values
#' are static metadata — no DB I/O — so it is safe to evaluate at build time.
#'
#' @return Named list keyed by character analysis_id.
#' @keywords internal
.omopAchillesRecordSpec <- function() {
  list(
    # x01: record count by domain concept id.
    "201"  = list(kind = "concept", table = "visit_occurrence",
                  concept_col = "visit_concept_id",
                  name = "Number of visit records, by visit_concept_id",
                  domain = "visit"),
    "401"  = list(kind = "concept", table = "condition_occurrence",
                  concept_col = "condition_concept_id",
                  name = "Number of condition records, by condition_concept_id",
                  domain = "condition"),
    "601"  = list(kind = "concept", table = "procedure_occurrence",
                  concept_col = "procedure_concept_id",
                  name = "Number of procedure records, by procedure_concept_id",
                  domain = "procedure"),
    "701"  = list(kind = "concept", table = "drug_exposure",
                  concept_col = "drug_concept_id",
                  name = "Number of drug records, by drug_concept_id",
                  domain = "drug"),
    "801"  = list(kind = "concept", table = "observation",
                  concept_col = "observation_concept_id",
                  name = "Number of observation records, by observation_concept_id",
                  domain = "observation"),
    "1801" = list(kind = "concept", table = "measurement",
                  concept_col = "measurement_concept_id",
                  name = "Number of measurement records, by measurement_concept_id",
                  domain = "measurement"),
    "2101" = list(kind = "concept", table = "device_exposure",
                  concept_col = "device_concept_id",
                  name = "Number of device records, by device_concept_id",
                  domain = "device"),
    # xx20: record count by event start month (YYYYMM).
    "220"  = list(kind = "month", table = "visit_occurrence",
                  date_col = "visit_start_date",
                  name = "Number of visit records by visit start month",
                  domain = "visit"),
    "420"  = list(kind = "month", table = "condition_occurrence",
                  date_col = "condition_start_date",
                  name = "Number of condition records by condition start month",
                  domain = "condition"),
    "620"  = list(kind = "month", table = "procedure_occurrence",
                  date_col = "procedure_date",
                  name = "Number of procedure records by procedure start month",
                  domain = "procedure"),
    "720"  = list(kind = "month", table = "drug_exposure",
                  date_col = "drug_exposure_start_date",
                  name = "Number of drug records by drug start month",
                  domain = "drug"),
    "820"  = list(kind = "month", table = "observation",
                  date_col = "observation_date",
                  name = "Number of observation records by observation start month",
                  domain = "observation"),
    "1820" = list(kind = "month", table = "measurement",
                  date_col = "measurement_date",
                  name = "Number of measurement records by measurement start month",
                  domain = "measurement"),
    "2102" = list(kind = "month", table = "device_exposure",
                  date_col = "device_exposure_start_date",
                  name = "Number of device records by device start month",
                  domain = "device"),
    # 505: death record count by death_type_concept_id (concept-translated).
    "505"  = list(kind = "death_type", table = "death",
                  concept_col = "death_type_concept_id",
                  name = "Number of death records, by death_type_concept_id",
                  domain = "death"),
    # 1818: measurement record count by concept x unit x normal-range bucket.
    "1818" = list(kind = "range", table = "measurement",
                  concept_col = "measurement_concept_id",
                  name = paste0("Number of measurement records below/within/above",
                                " normal range, by measurement_concept_id and",
                                " unit_concept_id"),
                  domain = "measurement")
  )
}

#' Dialect-aware YYYYMM month-key expression (shared with the Achilles companion)
#'
#' Same month key \code{.achillesCompanionPersonCounts} (achilles_gating.R:134)
#' built for its precomputed-companion counts, so a live monthly record cell is
#' keyed identically to the historical stratum_1 storage. PostgreSQL/duckdb use
#' \code{EXTRACT(YEAR)*100 + EXTRACT(MONTH)}; SQLite uses \code{strftime('%Y%m')}.
#'
#' @param handle CDM handle (selects the dialect).
#' @param date_expr Character; SQL date expression for the event date.
#' @return Character; SQL scalar expression evaluating to an integer YYYYMM key.
#' @keywords internal
.omopAchillesRecordMonthKey <- function(handle, date_expr) {
  if (identical(handle$target_dialect, "sqlite")) {
    paste0("CAST(strftime('%Y%m', ", date_expr, ") AS INTEGER)")
  } else {
    paste0("EXTRACT(YEAR FROM ", date_expr, ") * 100 + ",
           "EXTRACT(MONTH FROM ", date_expr, ")")
  }
}

#' Build one record-count entry's live compute function
#'
#' Returns the \code{function(handle, ctx, params)} that, for one ACH-E
#' analysis, computes \code{count_value} = COUNT(*) and the distinct-person
#' companion \code{n_persons} = COUNT(DISTINCT person_id) in a SINGLE query over
#' the SAME GROUP BY, restricted to \code{ctx$scoped_cohort} (INNER JOIN on
#' subject_id) or cohort-wide when un-scoped. The population is self-gated with
#' \code{.omopDiagAssertPersons} over the domain table BEFORE any rows are
#' materialised. The frame is returned UN-GATED (declared, not pre-gated): the
#' single \code{.omopAnalysisGate} suppresses+bands from the disclosure spec.
#'
#' @param spec One \code{.omopAchillesRecordSpec()} entry.
#' @return A \code{function(handle, ctx, params)} returning an aggregate frame.
#' @keywords internal
.omopAchillesRecordFn <- function(spec) {
  force(spec)
  function(handle, ctx, params) {
    # Fail closed (empty, not error) when the domain table is absent from this
    # CDM, mirroring the original .achillesCompanionPersonCounts guard
    # (achilles_gating.R:127-130). An empty frame passes through the gate as a
    # no-result (disclosure-safe), whereas letting the SELECT hit a missing table
    # would surface a raw "no such table: <name>" SQL error that needlessly leaks
    # which CDM tables are absent. (e.g. device_exposure absent -> 2101/2102.)
    bp <- .buildBlueprint(handle)
    if (!spec$table %in% bp$tables$table_name[bp$tables$present_in_db]) {
      return(data.frame())
    }

    tbl <- .qualifyTable(handle, spec$table)
    sj  <- .omopScopeJoin(ctx, "e", "person_id")

    # Self-gate the scoped + filtered population over the domain table BEFORE
    # materialising. Un-scoped -> cohort-wide (sj$join == "").
    .omopDiagAssertPersons(handle, ctx, tbl, "e", "person_id")

    if (identical(spec$kind, "month")) {
      # Record count by YYYYMM start month + distinct-person companion.
      mkey <- .omopAchillesRecordMonthKey(handle, paste0("e.", spec$date_col))
      sql <- .sql_translate(paste0(
        "SELECT ", mkey, " AS calendar_month, ",
        "COUNT(*) AS count_value, ",
        "COUNT(DISTINCT e.person_id) AS n_persons ",
        "FROM ", tbl, " e", sj$join,
        " WHERE e.", spec$date_col, " IS NOT NULL",
        " GROUP BY ", mkey,
        " ORDER BY calendar_month"),
        handle$target_dialect)
      return(.executeQuery(handle, sql))
    }

    if (identical(spec$kind, "range")) {
      # Verbatim normal-range bucket from achilles_gating.R:159-163 — the labels
      # 'Below Range Low' / 'Within Range' / 'Above Range High' are load-bearing
      # (they ARE the stratum the historical analysis 1818 reports).
      bucket <- paste0(
        "CASE WHEN e.value_as_number < e.range_low THEN 'Below Range Low' ",
        "WHEN e.value_as_number > e.range_high THEN 'Above Range High' ",
        "ELSE 'Within Range' END")
      concept <- .qualifyTable(handle, "concept",
                               handle$vocab_schema %||% handle$cdm_schema)
      ucept <- .qualifyTable(handle, "concept",
                             handle$vocab_schema %||% handle$cdm_schema)
      sql <- .sql_translate(paste0(
        "SELECT e.measurement_concept_id AS measurement_concept_id, ",
        "mc.concept_name AS measurement_concept_name, ",
        "e.unit_concept_id AS unit_concept_id, ",
        "uc.concept_name AS unit_concept_name, ",
        bucket, " AS range_bucket, ",
        "COUNT(*) AS count_value, ",
        "COUNT(DISTINCT e.person_id) AS n_persons ",
        "FROM ", tbl, " e",
        " LEFT JOIN ", concept,
        " mc ON mc.concept_id = e.measurement_concept_id",
        " LEFT JOIN ", ucept,
        " uc ON uc.concept_id = e.unit_concept_id", sj$join,
        " WHERE e.value_as_number IS NOT NULL",
        " AND e.range_low IS NOT NULL AND e.range_high IS NOT NULL",
        " GROUP BY e.measurement_concept_id, mc.concept_name, ",
        "e.unit_concept_id, uc.concept_name, ", bucket,
        " ORDER BY count_value DESC"),
        handle$target_dialect)
      return(.executeQuery(handle, sql))
    }

    # kind %in% c("concept", "death_type"): record count by a single concept id,
    # translated to its name. (death_type uses death_type_concept_id; the SQL is
    # identical once the concept column is resolved from the spec.)
    concept <- .qualifyTable(handle, "concept",
                             handle$vocab_schema %||% handle$cdm_schema)
    sql <- .sql_translate(paste0(
      "SELECT e.", spec$concept_col, " AS concept_id, ",
      "cc.concept_name, ",
      "COUNT(*) AS count_value, ",
      "COUNT(DISTINCT e.person_id) AS n_persons ",
      "FROM ", tbl, " e",
      " LEFT JOIN ", concept,
      " cc ON cc.concept_id = e.", spec$concept_col, sj$join,
      " WHERE e.", spec$concept_col, " IS NOT NULL",
      " GROUP BY e.", spec$concept_col, ", cc.concept_name",
      " ORDER BY count_value DESC"),
      handle$target_dialect)
    .executeQuery(handle, sql)
  }
}

#' Build one record-count catalog entry from its domain spec
#'
#' Assembles the \code{omop_analysis_entry} for one ACH-E analysis: the live
#' compute fn (\code{\link{.omopAchillesRecordFn}}), the disclosure spec
#' (\code{unit="record"}, \code{count_cols=c("count_value","n_persons")},
#' \code{person_id_col="n_persons"}) that drives the gate's GENERIC record branch,
#' and the LIVE scope (cohort + single-table, \code{requires_cohort=FALSE} to
#' preserve whole-DB semantics when un-scoped). \code{meta$adapter} is
#' \code{"achilles_live"} (NOT \code{"achilles"}) precisely so the gate does NOT
#' dispatch to the precomputed-sibling \code{.achillesPersonGate}; the companion
#' \code{n_persons} travels in the frame and is gated there directly.
#'
#' @param aid Integer analysis id.
#' @param spec Its \code{.omopAchillesRecordSpec()} entry.
#' @return A named-by-id \code{omop_analysis_entry}.
#' @keywords internal
.omopAchillesRecordEntry <- function(aid, spec) {
  name <- paste0("dsomop:achilles.", aid)
  .omopAnalysisEntry(
    name        = name,
    description = spec$name,
    domain      = spec$domain %||% "general",
    params      = list(),
    compute     = list(kind = "r", sql = NULL, fn = .omopAchillesRecordFn(spec)),
    dependencies = list(tables = unique(c(spec$table, "concept")),
                        packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(
      unit          = "record",
      count_cols    = c("count_value", "n_persons"),
      person_id_col = "n_persons"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "achilles_live", analysis_id = as.integer(aid))
  )
}

#' Emit the ACH-E record-count Achilles catalog entries (native live compute)
#'
#' The group registrar for \code{pack_achilles_records.R}: one
#' \code{omop_analysis_entry} per record-count analysis in
#' \code{\link{.omopAchillesRecordSpec}}, each COMPUTING its record volume +
#' distinct-person companion LIVE from the CDM (no \code{achilles_results} read).
#' Returned as a named list keyed by entry id, exactly as
#' \code{\link{.omopAnalysisAchillesEntries}} expects to concatenate. \code{handle}
#' is taken for signature parity ONLY and is never queried at build time (catalog
#' build is cached on \code{handle$analysis_catalog}; all DB I/O happens later
#' inside each entry's \code{compute$fn}).
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by entry id.
#' @keywords internal
.omopAchillesRecordCountEntries <- function(handle) {
  spec_all <- .omopAchillesRecordSpec()
  entries <- lapply(names(spec_all), function(aid) {
    .omopAchillesRecordEntry(as.integer(aid), spec_all[[aid]])
  })
  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}
