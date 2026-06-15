# Module: Native Achilles count pack — persons-with->=1 / by-month / by-year
#
# Group ACH-D. Live-computing replacements for the count-based, PERSON-unit
# Achilles analyses. Each entry was previously served by reading a precomputed
# achilles_results row (.achillesGetResults); here every entry COMPUTES its
# metric directly from the CDM as a COUNT(DISTINCT person_id), scoped to
# ctx$scoped_cohort (cohort-wide when un-scoped, preserving database-
# characterization semantics), with concept ids translated to names. Nothing in
# this file reads achilles_results / achilles_results_dist.
#
# Every metric here is a distinct-person count, so each cell already rests on its
# own person basis: unit="person", count_cols="count_value". The SINGLE
# .omopAnalysisGate small-cell-suppresses then bands count_value — no companion
# person column and no second gate are needed. *_source_value columns are never
# selected (the gate is only a backstop).
#
# The 16 entries (stable ids preserved so existing references keep resolving):
#   persons-with->=1 by concept   : dsomop:achilles.{200,400,600,700,800,1800,2100}
#   persons by start month/concept: dsomop:achilles.{202,402,602,702,802,1802}
#   persons by visit start year   : dsomop:achilles.221
#   persons with death by cause   : dsomop:achilles.500
#   persons by death month        : dsomop:achilles.502
# All share ONE domain-spec-driven SQL template assembled in R (kind="r").

#' Domain spec table for the native Achilles count pack (ACH-D)
#'
#' Maps each Achilles analysis id in this group to the CDM source table, its
#' domain concept column, and its event start-date column, plus a \code{kind}
#' selecting the GROUP BY. \code{.omopCovariateSource} does not cover the visit
#' or death domains used here, so this group keeps its own minimal spec (the same
#' table/concept/date triples Achilles itself uses). All values are static
#' metadata — no DB I/O — so it is safe to evaluate at build time.
#'
#' \code{kind} is one of:
#' \itemize{
#'   \item \code{"concept"} — persons with >= 1 record, GROUP BY concept id;
#'   \item \code{"month"}   — persons by start month (YYYYMM) x concept id;
#'   \item \code{"year"}    — persons by start year;
#'   \item \code{"death_concept"} — persons with death, GROUP BY cause concept;
#'   \item \code{"death_month"}   — persons by death month (YYYYMM).
#' }
#'
#' @return Named list keyed by character analysis id; each value is
#'   \code{list(table, concept_col, date_col, kind, domain)}.
#' @keywords internal
.omopAchillesCountSpec <- function() {
  list(
    # persons with >= 1 <event>, by <domain>_concept_id.
    "200"  = list(kind = "concept",       table = "visit_occurrence",     concept_col = "visit_concept_id",       date_col = "visit_start_date",           domain = "visit"),
    "400"  = list(kind = "concept",       table = "condition_occurrence", concept_col = "condition_concept_id",   date_col = "condition_start_date",       domain = "condition"),
    "600"  = list(kind = "concept",       table = "procedure_occurrence", concept_col = "procedure_concept_id",   date_col = "procedure_date",             domain = "procedure"),
    "700"  = list(kind = "concept",       table = "drug_exposure",        concept_col = "drug_concept_id",        date_col = "drug_exposure_start_date",   domain = "drug"),
    "800"  = list(kind = "concept",       table = "observation",          concept_col = "observation_concept_id", date_col = "observation_date",           domain = "observation"),
    "1800" = list(kind = "concept",       table = "measurement",          concept_col = "measurement_concept_id", date_col = "measurement_date",           domain = "measurement"),
    "2100" = list(kind = "concept",       table = "device_exposure",      concept_col = "device_concept_id",      date_col = "device_exposure_start_date", domain = "device"),
    # persons by <event> start month, by <domain>_concept_id.
    "202"  = list(kind = "month",         table = "visit_occurrence",     concept_col = "visit_concept_id",       date_col = "visit_start_date",           domain = "visit"),
    "402"  = list(kind = "month",         table = "condition_occurrence", concept_col = "condition_concept_id",   date_col = "condition_start_date",       domain = "condition"),
    "602"  = list(kind = "month",         table = "procedure_occurrence", concept_col = "procedure_concept_id",   date_col = "procedure_date",             domain = "procedure"),
    "702"  = list(kind = "month",         table = "drug_exposure",        concept_col = "drug_concept_id",        date_col = "drug_exposure_start_date",   domain = "drug"),
    "802"  = list(kind = "month",         table = "observation",          concept_col = "observation_concept_id", date_col = "observation_date",           domain = "observation"),
    "1802" = list(kind = "month",         table = "measurement",          concept_col = "measurement_concept_id", date_col = "measurement_date",           domain = "measurement"),
    # persons by visit start year.
    "221"  = list(kind = "year",          table = "visit_occurrence",     concept_col = "visit_concept_id",       date_col = "visit_start_date",           domain = "visit"),
    # death analyses.
    "500"  = list(kind = "death_concept", table = "death",                concept_col = "cause_concept_id",       date_col = "death_date",                 domain = "death"),
    "502"  = list(kind = "death_month",   table = "death",                concept_col = "cause_concept_id",       date_col = "death_date",                 domain = "death")
  )
}

#' Dialect-aware YYYYMM month-key expression (Achilles stratum-compatible)
#'
#' Same month-key form \code{.achillesCompanionPersonCounts} (achilles_gating.R:134)
#' uses, so the by-month stratum is identical to what Achilles stores
#' (PostgreSQL/duckdb: \code{YEAR*100 + MONTH}; SQLite: \code{strftime('%Y%m')}).
#'
#' @param handle CDM handle (selects the dialect).
#' @param date_expr Character; a SQL date expression.
#' @return Character; an integer YYYYMM SQL expression.
#' @keywords internal
.omopAchillesCountMonthKey <- function(handle, date_expr) {
  if (identical(handle$target_dialect, "sqlite")) {
    paste0("CAST(strftime('%Y%m', ", date_expr, ") AS INTEGER)")
  } else {
    paste0("EXTRACT(YEAR FROM ", date_expr, ") * 100 + ",
           "EXTRACT(MONTH FROM ", date_expr, ")")
  }
}

#' Build the live person-count fn for one Achilles count analysis
#'
#' Returns the \code{compute$fn} for the analysis described by \code{spec}. The
#' fn self-gates the scoped (or cohort-wide) population over the domain table
#' BEFORE materialising, then runs ONE domain-spec-driven query:
#' \code{COUNT(DISTINCT person_id) AS count_value} grouped by the stratum the
#' \code{kind} selects (concept id; concept id + start month; start year; death
#' cause; or death month). Concept-keyed strata LEFT JOIN \code{concept} and
#' return \code{concept_name} (translation default ON). It NEVER selects a
#' \code{*_source_value} column and returns an AGGREGATE-ONLY frame (no person
#' key) UN-GATED — the single gate suppresses + bands \code{count_value}.
#'
#' @param spec One entry from \code{\link{.omopAchillesCountSpec}}.
#' @return A \code{function(handle, ctx, params)} returning an un-gated frame.
#' @keywords internal
.omopAchillesCountFn <- function(spec) {
  force(spec)
  function(handle, ctx, params) {
    # Fail closed (empty, not error) when the domain table is absent from this
    # CDM, mirroring the .omopAchillesRecordFn / .achillesCompanionPersonCounts
    # guard. An empty frame passes through the gate as a no-result (disclosure-
    # safe), whereas letting the SELECT hit a missing table would surface a raw
    # "no such table: <name>" SQL error that needlessly leaks which CDM tables are
    # absent (e.g. device_exposure absent -> achilles.2100).
    if (!.omopAchillesTablePresent(handle, spec$table)) return(data.frame())

    tbl     <- .qualifyTable(handle, spec$table)
    concept <- .qualifyTable(handle, "concept",
                             handle$vocab_schema %||% handle$cdm_schema)
    sj      <- .omopScopeJoin(ctx, "e", "person_id")

    # SELF-GATE before pulling rows: assert the scoped (or cohort-wide)
    # population over the domain table rests on >= nfilter.subset persons.
    # Un-scoped -> cohort-wide (sj$join == "").
    .omopDiagAssertPersons(handle, ctx, tbl, "e", "person_id")

    # The distinct-person count is the metric for every cell.
    person_count <- "COUNT(DISTINCT e.person_id) AS count_value"

    if (identical(spec$kind, "concept") ||
        identical(spec$kind, "death_concept")) {
      # persons with >= 1 record (or death), by concept id.
      sql <- paste0(
        "SELECT e.", spec$concept_col, " AS concept_id, ",
        "cc.concept_name, ", person_count,
        " FROM ", tbl, " e", sj$join,
        " LEFT JOIN ", concept, " cc ON cc.concept_id = e.", spec$concept_col,
        " GROUP BY e.", spec$concept_col, ", cc.concept_name",
        " ORDER BY count_value DESC")

    } else if (identical(spec$kind, "month")) {
      # persons by start month, by concept id.
      mkey <- .omopAchillesCountMonthKey(handle, paste0("e.", spec$date_col))
      sql <- paste0(
        "SELECT ", mkey, " AS calendar_month, ",
        "e.", spec$concept_col, " AS concept_id, cc.concept_name, ", person_count,
        " FROM ", tbl, " e", sj$join,
        " LEFT JOIN ", concept, " cc ON cc.concept_id = e.", spec$concept_col,
        " WHERE e.", spec$date_col, " IS NOT NULL",
        " GROUP BY ", mkey, ", e.", spec$concept_col, ", cc.concept_name",
        " ORDER BY count_value DESC")

    } else if (identical(spec$kind, "death_month")) {
      # persons by death month (no concept stratum).
      mkey <- .omopAchillesCountMonthKey(handle, paste0("e.", spec$date_col))
      sql <- paste0(
        "SELECT ", mkey, " AS calendar_month, ", person_count,
        " FROM ", tbl, " e", sj$join,
        " WHERE e.", spec$date_col, " IS NOT NULL",
        " GROUP BY ", mkey,
        " ORDER BY calendar_month")

    } else {  # "year": persons by start year.
      ykey <- .omopYearExpr(handle, paste0("e.", spec$date_col))
      sql <- paste0(
        "SELECT ", ykey, " AS calendar_year, ", person_count,
        " FROM ", tbl, " e", sj$join,
        " WHERE e.", spec$date_col, " IS NOT NULL",
        " GROUP BY ", ykey,
        " ORDER BY calendar_year")
    }

    .executeQuery(handle, .sql_translate(sql, handle$target_dialect))
  }
}

#' Build one native Achilles count entry from its domain spec
#'
#' Assembles the \code{omop_analysis_entry} for one ACH-D analysis: the live
#' compute fn (\code{\link{.omopAchillesCountFn}}), the disclosure spec
#' (\code{unit="person"}, \code{count_cols="count_value"}) so the gate's person
#' branch small-cell-suppresses + bands the distinct-person count with no
#' companion, and the LIVE scope (cohort + single-table, \code{requires_cohort=FALSE}
#' to preserve whole-DB characterization when un-scoped). \code{meta$adapter} is
#' \code{"achilles"} with the integer \code{analysis_id} carried as a label only.
#'
#' @param aid Integer analysis id (a key of \code{\link{.omopAchillesCountSpec}}).
#' @param spec Its \code{\link{.omopAchillesCountSpec}} entry.
#' @return A single \code{omop_analysis_entry}.
#' @keywords internal
.omopAchillesCountEntry <- function(aid, spec) {
  name <- paste0("dsomop:achilles.", aid)
  descriptions <- list(
    "200"  = "Number of persons with at least one visit, by visit_concept_id",
    "400"  = "Number of persons with at least one condition, by condition_concept_id",
    "600"  = "Number of persons with at least one procedure, by procedure_concept_id",
    "700"  = "Number of persons with at least one drug, by drug_concept_id",
    "800"  = "Number of persons with at least one observation, by observation_concept_id",
    "1800" = "Number of persons with at least one measurement, by measurement_concept_id",
    "2100" = "Number of persons with at least one device, by device_concept_id",
    "202"  = "Number of persons by visit start month, by visit_concept_id",
    "402"  = "Number of persons by condition start month, by condition_concept_id",
    "602"  = "Number of persons by procedure start month, by procedure_concept_id",
    "702"  = "Number of persons by drug start month, by drug_concept_id",
    "802"  = "Number of persons by observation start month, by observation_concept_id",
    "1802" = "Number of persons by measurement start month, by measurement_concept_id",
    "221"  = "Number of persons by visit start year",
    "500"  = "Number of persons with death, by cause_concept_id",
    "502"  = "Number of persons by death month"
  )

  .omopAnalysisEntry(
    name        = name,
    description = descriptions[[as.character(aid)]] %||%
                  paste0("Number of persons (Achilles ", aid, ")"),
    domain      = spec$domain %||% "general",
    params      = list(),
    compute     = list(kind = "r", sql = NULL, fn = .omopAchillesCountFn(spec)),
    dependencies = list(tables = unique(c(spec$table, "concept")),
                        packages = character(0)),
    # unit="person": count_value IS a distinct-person count, so the gate's
    # person branch small-cell-suppresses + bands it with no companion.
    disclosure = .omopAnalysisDisclosure(
      unit = "person", count_cols = "count_value", min_max = FALSE),
    # LIVE port: scopable by cohort id/handle/table AND omop.table symbol.
    # requires_cohort=FALSE preserves whole-DB characterization when un-scoped.
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "achilles", analysis_id = as.integer(aid))
  )
}

#' Native Achilles count entries (Group ACH-D, person-unit)
#'
#' Registrar for the count-based, person-unit Achilles analyses. Returns a named
#' list keyed by entry id (== \code{entry$name}), one live-computing
#' \code{omop_analysis_entry} per analysis in \code{\link{.omopAchillesCountSpec}}.
#' Returned exactly as \code{\link{.omopAnalysisAchillesEntries}} expects to
#' overlay it by id. \code{handle} is taken for signature parity ONLY and is NOT
#' queried at build time (all DB I/O happens later inside \code{compute$fn}).
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by entry name.
#' @keywords internal
.omopAchillesConceptCountEntries <- function(handle) {
  spec_all <- .omopAchillesCountSpec()
  entries <- lapply(names(spec_all), function(aid) {
    .omopAchillesCountEntry(as.integer(aid), spec_all[[aid]])
  })
  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}
