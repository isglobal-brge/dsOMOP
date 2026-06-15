# Module: Native Achilles observation-period analyses (live-computing pack)
#
# Live re-implementations of the Achilles observation-period analyses, replacing
# the precomputed-results adapter (.achillesGetResults / .achillesGetDistributions
# reading achilles_results / achilles_results_dist) with kind="r" catalog entries
# that COMPUTE each metric directly from the CDM observation_period table over the
# SCOPED cohort (cohort-wide when un-scoped, preserving Achilles whole-database
# semantics). Every entry keeps its stable id (dsomop:achilles.<id>) so existing
# references survive; only the compute changes (read-results -> live-compute).
#
# This file owns .omopAchillesObsPeriodEntries(handle), the single register_fn the
# Achilles adapter (.omopAnalysisAchillesEntries) concatenates for the
# observation-period group. Two sub-groups live here and compose without sharing a
# function body: the distribution analyses (103-107) via
# .omopAchillesObsPeriodDistEntries (unit="dist"), and the count-based analyses
# (101/102/108/109/113) via .omopAchillesObsPeriodCountEntries (unit="person").
#
# All disclosure is delegated to the SINGLE gate (.omopAnalysisGate): each entry
# returns a RAW aggregate-only dist frame (count_value + summary stats, plus
# min/max which the gate strips) and declares unit="dist" / count_cols so the gate
# drops small-count rows, strips min/max, masks summary stats below nfilter_dist,
# and bands count_value. No *_source_value is ever selected; concept ids (gender)
# are translated to names by default.

# --- Per-person value extraction (live, scoped) ------------------------------

#' Resolve the qualified "first observation period per person" sub-select
#'
#' The Achilles observation-period distributions (103-107) summarise ONE value
#' per person derived from that person's FIRST observation period (earliest
#' \code{observation_period_start_date}). This returns a parenthesised sub-select
#' aliased as \code{fop} exposing \code{person_id}, \code{op_start}, \code{op_end}
#' for the first period only (ties on the earliest start collapsed by also taking
#' the latest end, so each person contributes exactly one (start,end) pair).
#'
#' @param handle CDM handle (for table qualification / dialect).
#' @return Character; a SQL sub-select (no trailing alias) over observation_period.
#' @keywords internal
.omopAchillesFirstObsPeriodSql <- function(handle) {
  obs <- .qualifyTable(handle, "observation_period")
  # Per person: earliest start, and the latest end among rows sharing that start
  # (so duplicate same-start periods collapse to a single deterministic pair).
  paste0(
    "(SELECT op.person_id AS person_id, ",
    "MIN(op.observation_period_start_date) AS op_start, ",
    "MAX(op.observation_period_end_date) AS op_end ",
    "FROM ", obs, " op ",
    "WHERE op.observation_period_start_date = (",
    "SELECT MIN(op2.observation_period_start_date) FROM ", obs, " op2 ",
    "WHERE op2.person_id = op.person_id) ",
    "GROUP BY op.person_id)")
}

#' Pull one per-person value (+ optional strata) for an obs-period distribution
#'
#' Computes a single value per SCOPED person from their first observation period
#' and returns the un-summarised long frame (\code{person_id}, \code{v}, plus any
#' requested strata columns). \code{value_kind}:
#' \describe{
#'   \item{"age"}{Age in whole years at first-observation-period start
#'     (\code{(op_start - birth_date)/365}), the Achilles 103/104 metric.}
#'   \item{"length"}{Length in days of the first observation period
#'     (\code{op_end - op_start}), the Achilles 105/106/107 metric.}
#' }
#' Population is restricted to \code{ctx$scoped_cohort} with an INNER JOIN on
#' \code{subject_id}; cohort-wide (no join) when un-scoped, preserving Achilles
#' whole-database semantics. \code{strata} selects extra columns to carry through:
#' \code{"gender"} adds \code{gender_concept_id} + translated \code{gender_name};
#' \code{"birth"} adds \code{year_of_birth} + \code{op_start_year} (for an
#' age-decile split computed in R). A person-count self-gate over the
#' observation_period population (scoped/whole-DB) runs BEFORE any rows are
#' materialised.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param value_kind Character; "age" or "length".
#' @param strata Character; "none" (default), "gender", or "birth".
#' @return Data frame (one row per qualifying person) or an empty frame.
#' @keywords internal
.omopAchillesObsPeriodValues <- function(handle, ctx, value_kind = "age",
                                         strata = "none") {
  person  <- .qualifyTable(handle, "person")
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  fop     <- .omopAchillesFirstObsPeriodSql(handle)

  # Scope join (INNER JOIN on subject_id) or "" for a cohort-wide run.
  sj <- .omopScopeJoin(ctx, "fop", "person_id")

  # Self-gate the scoped/whole-DB observation_period population before pulling
  # rows: assert it rests on >= nfilter.subset distinct persons (fail-closed).
  .omopDiagAssertPersons(handle, ctx, .qualifyTable(handle, "observation_period"),
                         "op", "person_id")

  # birth date as year-01-01 (spliced post-translate; author SQL, not user input).
  birth_dt <- if (identical(handle$target_dialect %||% "", "sqlite")) {
    "(CAST(p.year_of_birth AS VARCHAR) || '-01-01')"
  } else {
    "CAST((CAST(p.year_of_birth AS VARCHAR) || '-01-01') AS DATE)"
  }

  value_expr <- if (identical(value_kind, "age")) {
    age_days <- .omopDateDiffDays(handle, "fop.op_start", "p.birth_dt")
    paste0("CAST(", age_days, " / 365 AS INTEGER)")
  } else {
    .omopDateDiffDays(handle, "fop.op_end", "fop.op_start")
  }

  # Extra strata columns + the person join they require.
  sel_extra <- ""
  person_join <- ""
  needs_person <- identical(value_kind, "age") ||
    strata %in% c("gender", "birth")
  if (needs_person) {
    person_join <- paste0(" INNER JOIN ", person,
                          " p ON p.person_id = fop.person_id")
  }
  if (identical(strata, "gender")) {
    sel_extra <- paste0(
      ", p.gender_concept_id AS gender_concept_id, ",
      "gc.concept_name AS gender_name")
    person_join <- paste0(person_join,
      " LEFT JOIN ", concept, " gc ON gc.concept_id = p.gender_concept_id")
  } else if (identical(strata, "birth")) {
    op_year <- .omopYearExpr(handle, "fop.op_start")
    sel_extra <- paste0(
      ", p.year_of_birth AS year_of_birth, ", op_year, " AS op_start_year")
  }

  sql <- .sql_translate(paste0(
    "SELECT fop.person_id AS person_id, ", value_expr, " AS v", sel_extra,
    " FROM ", fop, " fop", sj$join, person_join),
    handle$target_dialect)
  sql <- gsub("p.birth_dt", birth_dt, sql, fixed = TRUE)

  raw <- .executeQuery(handle, sql)
  if (!is.data.frame(raw) || nrow(raw) == 0) return(data.frame())
  raw$v <- suppressWarnings(as.numeric(raw$v))
  raw <- raw[!is.na(raw$v), , drop = FALSE]
  if (nrow(raw) == 0) return(data.frame())
  rownames(raw) <- NULL
  raw
}

# --- Per-stratum dist summarisation (R) --------------------------------------

#' Summarise a per-person value vector into one Achilles dist row
#'
#' Mirrors the per-covariate summarisation in \code{\link{.omopCovariateContinuous}}:
#' \code{count_value} = number of persons, \code{min_value}/\code{max_value}
#' (emitted but stripped by the gate), \code{avg_value}, \code{stdev_value}, and
#' the p10/p25/median/p75/p90 quantiles (\code{stats::quantile} type 7) — NEVER
#' native 0%/100% (those are min/max). Optional \code{label_cols} (a named list)
#' are prepended as stratum identifier columns.
#'
#' @param v Numeric vector of per-person values (already NA-stripped).
#' @param label_cols Named list of scalar stratum labels (may be empty).
#' @return One-row data frame, or an empty frame when \code{v} is empty.
#' @keywords internal
.omopAchillesDistRow <- function(v, label_cols = list()) {
  v <- v[!is.na(v)]
  if (length(v) == 0) return(data.frame())
  qs <- stats::quantile(v, c(.10, .25, .5, .75, .90), names = FALSE, type = 7)
  stats_df <- data.frame(
    count_value  = length(v),
    min_value    = min(v),   # stripped by the gate
    max_value    = max(v),   # stripped by the gate
    avg_value    = mean(v),
    stdev_value  = stats::sd(v),
    p10_value    = qs[1], p25_value = qs[2], median_value = qs[3],
    p75_value    = qs[4], p90_value = qs[5],
    stringsAsFactors = FALSE)
  if (length(label_cols) > 0) {
    labels_df <- as.data.frame(label_cols, stringsAsFactors = FALSE)
    stats_df <- cbind(labels_df, stats_df)
  }
  stats_df
}

#' Build the un-gated dist frame for an Achilles observation-period analysis
#'
#' The single compute kernel behind every 103-107 entry: pull one value per
#' scoped person (\code{value_kind}), \code{split()} by the requested stratum, and
#' summarise each split into a dist row. Strata:
#' \describe{
#'   \item{"none"}{One overall dist row (103, 105).}
#'   \item{"gender"}{One row per gender, labelled by translated concept name
#'     (\code{gender_name}); the gender_concept_id is used only to group (104, 106).}
#'   \item{"age_decile"}{One row per 10-year age band at first-obs-period start,
#'     binned with \code{\link{.computeAgeGroups}} (107).}
#' }
#' Returns an AGGREGATE-ONLY frame (no person key); the gate applies all
#' suppression/masking/banding. Empty (gate-safe) frame when no persons qualify.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx.
#' @param value_kind Character; "age" or "length".
#' @param stratum Character; "none", "gender", or "age_decile".
#' @return Data frame of dist rows (un-gated) or an empty frame.
#' @keywords internal
.omopAchillesObsPeriodDist <- function(handle, ctx, value_kind, stratum) {
  fetch_strata <- switch(stratum,
    "gender"     = "gender",
    "age_decile" = "birth",
    "none")
  raw <- .omopAchillesObsPeriodValues(handle, ctx, value_kind, fetch_strata)
  if (!is.data.frame(raw) || nrow(raw) == 0) return(data.frame())

  if (identical(stratum, "none")) {
    return(.omopAchillesDistRow(raw$v))
  }

  if (identical(stratum, "gender")) {
    # Group by gender_concept_id (stable), label with the translated name. A
    # missing concept name (unmapped gender) is labelled by its id so the row is
    # still distinct and gate-able.
    raw$.grp <- ifelse(is.na(raw$gender_concept_id), "NA",
                       as.character(raw$gender_concept_id))
    parts <- split(raw, raw$.grp)
    rows <- lapply(parts, function(p) {
      nm <- p$gender_name[1]
      if (is.na(nm) || !nzchar(nm)) nm <- as.character(p$gender_concept_id[1])
      .omopAchillesDistRow(p$v, list(
        gender_concept_id = p$gender_concept_id[1],
        gender_name       = nm))
    })
    out <- do.call(rbind, rows)
    if (is.null(out) || nrow(out) == 0) return(data.frame())
    rownames(out) <- NULL
    return(out)
  }

  # age_decile: bin age (years) at first-obs start into 10-year deciles. Age is
  # derived in R from year_of_birth and the first-obs-period start year so the
  # decile label matches Achilles 107's "age decile" stratum.
  raw$age_decile <- .computeAgeGroups(raw$year_of_birth, raw$op_start_year,
                                      bin_width = 10L)
  raw <- raw[!is.na(raw$age_decile), , drop = FALSE]
  if (nrow(raw) == 0) return(data.frame())
  parts <- split(raw, raw$age_decile)
  rows <- lapply(parts, function(p) {
    .omopAchillesDistRow(p$v, list(age_decile = p$age_decile[1]))
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || nrow(out) == 0) return(data.frame())
  out <- out[order(out$age_decile), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# --- Catalog entries (103-107) -----------------------------------------------

#' Build one Achilles observation-period DIST catalog entry
#'
#' Shared entry constructor for analyses 103-107: a kind="r" entry whose
#' \code{compute$fn} calls \code{\link{.omopAchillesObsPeriodDist}} and whose
#' disclosure declares \code{unit="dist"} / \code{count_cols="count_value"} so the
#' ONE gate strips min/max, masks summary stats below nfilter_dist, drops
#' small-count rows, and bands \code{count_value}. Scope is LIVE (cohort + single
#' table, \code{max_tables=1}); \code{requires_cohort} stays FALSE so an un-scoped
#' run computes cohort-wide (Achilles whole-database semantics) rather than
#' erroring. The stable id \code{dsomop:achilles.<aid>} is preserved.
#'
#' @param aid Integer Achilles analysis id.
#' @param description Human-readable analysis name.
#' @param value_kind Character; "age" or "length".
#' @param stratum Character; "none", "gender", or "age_decile".
#' @return A \code{omop_analysis_entry}.
#' @keywords internal
.omopAchillesObsPeriodDistEntry <- function(aid, description, value_kind,
                                            stratum) {
  name <- paste0("dsomop:achilles.", aid)
  force(value_kind); force(stratum)
  fn <- function(handle, ctx, params) {
    .omopAchillesObsPeriodDist(handle, ctx, value_kind, stratum)
  }
  .omopAnalysisEntry(
    name        = name,
    description = description,
    domain      = "observation_period",
    params      = list(),
    compute     = list(kind = "r", sql = NULL, fn = fn),
    dependencies = list(
      tables   = c("observation_period", "person", "concept"),
      packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "dist", count_cols = "count_value", min_max = TRUE),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "achilles_live", analysis_id = aid))
}

#' Emit the live-computing Achilles observation-period DISTRIBUTION entries
#'
#' Analyses 103-107 (distribution of age at first observation period, optionally
#' by gender; distribution of first-observation-period length in days, optionally
#' by gender or age decile), each computing its metric LIVE from the CDM
#' \code{observation_period} table (NOT reading achilles_results_dist).
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by entry name.
#' @keywords internal
.omopAchillesObsPeriodDistEntries <- function(handle) {
  entries <- list(
    .omopAchillesObsPeriodDistEntry(
      103L, "Distribution of age at first observation period",
      value_kind = "age", stratum = "none"),
    .omopAchillesObsPeriodDistEntry(
      104L, "Distribution of age at first observation period by gender",
      value_kind = "age", stratum = "gender"),
    .omopAchillesObsPeriodDistEntry(
      105L, "Length of observation (days) of first observation period",
      value_kind = "length", stratum = "none"),
    .omopAchillesObsPeriodDistEntry(
      106L,
      "Length of observation (days) of first observation period by gender",
      value_kind = "length", stratum = "gender"),
    .omopAchillesObsPeriodDistEntry(
      107L,
      paste0("Length of observation (days) of first observation period by ",
             "age decile"),
      value_kind = "length", stratum = "age_decile")
  )
  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}

# --- Count sub-group: persons by age/length/year/period-count (101/102/108/ ---
# --- 109/113) ----------------------------------------------------------------
#
# The COUNT-based observation-period analyses. Each is unit="person":
# COUNT(DISTINCT person_id) per stratum, so the gate fully gates by small-cell
# suppression + banding on count_value (no record companion needed). Strata are
# kept coarse/non-disclosive: ages bucketed into 5-year bands (never single-year),
# obs-period length bucketed into 30-day increments, and the calendar-year spine
# (109) carries only coarse calendar years. The shared first-observation-period
# sub-select (.omopAchillesFirstObsPeriodSql) is reused; scope is applied via
# .omopScopeJoin and the population self-gated with .omopDiagAssertPersons BEFORE
# any rows are materialised. Concept ids (gender, 102) are translated to names.

#' Dialect-aware calendar-year spine (server-side), bounded to the data
#'
#' Analysis 109 (continuous observation per year) needs one row per calendar year
#' spanned by the observation periods. The bounds are coarse, non-disclosive
#' calendar years queried once (min start year .. max end year over the scoped /
#' whole-DB population); the spine itself is materialised SERVER-SIDE —
#' \code{generate_series} on PostgreSQL/ANSI engines, a recursive CTE on SQLite —
#' so no per-person dates are pulled into R. Returns NULL when the population has
#' no observation periods (caller fails closed / returns empty).
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param alias Character; alias for the spine derived table (column \code{yr}).
#' @return Character; a parenthesised year-spine derived table aliased as
#'   \code{alias} with a single integer column \code{yr}, or NULL.
#' @keywords internal
.omopAchillesYearSpineSql <- function(handle, ctx, alias = "ys") {
  obs <- .qualifyTable(handle, "observation_period")
  sj  <- .omopScopeJoin(ctx, "op", "person_id")
  lo_expr <- .omopYearExpr(handle, "MIN(op.observation_period_start_date)")
  hi_expr <- .omopYearExpr(handle, "MAX(op.observation_period_end_date)")
  bounds <- .executeQuery(handle, .sql_translate(paste0(
    "SELECT ", lo_expr, " AS lo_year, ", hi_expr, " AS hi_year FROM ",
    obs, " op", sj$join), handle$target_dialect))
  lo <- suppressWarnings(as.integer(bounds$lo_year[1]))
  hi <- suppressWarnings(as.integer(bounds$hi_year[1]))
  if (length(lo) == 0 || is.na(lo) || is.na(hi) || hi < lo) return(NULL)

  if (identical(handle$target_dialect %||% "", "sqlite")) {
    # SQLite has no generate_series(start, stop): recurse from lo to hi.
    paste0(
      "(WITH RECURSIVE ", alias, "_cte(yr) AS (",
      "SELECT ", lo, " UNION ALL SELECT yr + 1 FROM ", alias, "_cte ",
      "WHERE yr < ", hi, ") SELECT yr FROM ", alias, "_cte) ", alias)
  } else {
    # PostgreSQL/duckdb and friends: generate_series(lo, hi).
    paste0(
      "(SELECT CAST(gs AS INTEGER) AS yr FROM generate_series(", lo, ", ", hi,
      ") AS gs) ", alias)
  }
}

#' Persons by age at first observation period, optionally by gender (101/102)
#'
#' Age at first observation period is the Achilles 101/102 definition
#' \code{YEAR(op_start) - year_of_birth} (a coarse year difference; no birth date,
#' no extreme-tail leakage), floored into a fixed 5-year band so a single-year-of-
#' age stratum is never released. Counts \code{COUNT(DISTINCT person_id)} per age
#' band (101) or per gender x age band (102), with the gender concept id
#' translated to its name. Population is restricted to \code{ctx$scoped_cohort}
#' (cohort-wide when un-scoped) and self-gated with \code{.omopDiagAssertPersons}
#' BEFORE any rows are materialised. Returns an aggregate-only frame.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param by_gender Logical; add a gender stratum (102) when TRUE.
#' @return Aggregate-only data.frame (age_band[, gender_name], count_value).
#' @keywords internal
.omopAchillesAgeAtFirstObs <- function(handle, ctx, by_gender = FALSE) {
  person  <- .qualifyTable(handle, "person")
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  fop     <- .omopAchillesFirstObsPeriodSql(handle)
  sj      <- .omopScopeJoin(ctx, "fop", "person_id")

  # Self-gate the scoped/whole-DB observation_period population first.
  .omopDiagAssertPersons(handle, ctx,
                         .qualifyTable(handle, "observation_period"),
                         "op", "person_id")

  age_expr  <- paste0(.omopYearExpr(handle, "fop.op_start"),
                      " - p.year_of_birth")
  band_expr <- paste0("(CAST(", age_expr, " AS INTEGER) / 5) * 5")

  if (isTRUE(by_gender)) {
    select_sql <- paste0(band_expr, " AS age_band, gc.concept_name AS gender_name")
    group_sql  <- paste0(band_expr, ", p.gender_concept_id, gc.concept_name")
    gender_join <- paste0(" LEFT JOIN ", concept,
                          " gc ON gc.concept_id = p.gender_concept_id")
  } else {
    select_sql  <- paste0(band_expr, " AS age_band")
    group_sql   <- band_expr
    gender_join <- ""
  }

  sql <- .sql_translate(paste0(
    "SELECT ", select_sql, ", COUNT(DISTINCT fop.person_id) AS count_value ",
    "FROM ", fop, " fop", sj$join,
    " INNER JOIN ", person, " p ON p.person_id = fop.person_id", gender_join,
    " GROUP BY ", group_sql,
    " ORDER BY count_value DESC"),
    handle$target_dialect)
  .executeQuery(handle, sql)
}

#' Persons by length of first observation period, 30-day increments (108)
#'
#' Length (days) of the FIRST observation period (\code{op_end - op_start} from
#' the shared first-period sub-select), bucketed into 30-day increments (floor).
#' Counts \code{COUNT(DISTINCT person_id)} per 30-day bucket. Self-gates the
#' scoped/whole-DB population before materialising. Returns aggregate-only.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @return Aggregate-only data.frame (length_bucket, count_value).
#' @keywords internal
.omopAchillesObsPeriodLength <- function(handle, ctx) {
  fop <- .omopAchillesFirstObsPeriodSql(handle)
  sj  <- .omopScopeJoin(ctx, "fop", "person_id")

  .omopDiagAssertPersons(handle, ctx,
                         .qualifyTable(handle, "observation_period"),
                         "op", "person_id")

  len_expr <- .omopDateDiffDays(handle, "fop.op_end", "fop.op_start")
  bucket   <- paste0("(CAST(", len_expr, " AS INTEGER) / 30) * 30")

  sql <- .sql_translate(paste0(
    "SELECT ", bucket, " AS length_bucket, ",
    "COUNT(DISTINCT fop.person_id) AS count_value ",
    "FROM ", fop, " fop", sj$join,
    " GROUP BY ", bucket,
    " ORDER BY count_value DESC"),
    handle$target_dialect)
  .executeQuery(handle, sql)
}

#' Persons with continuous observation in each calendar year (109)
#'
#' For each calendar year Y in the data's span (server-side year spine), counts
#' \code{COUNT(DISTINCT person_id)} of persons having ANY observation period
#' covering Y (\code{Y BETWEEN year(start) AND year(end)}). The year spine bounds
#' are coarse, non-disclosive calendar years. Self-gates before materialising.
#' Returns aggregate-only; empty when the population has no observation periods.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @return Aggregate-only data.frame (calendar_year, count_value).
#' @keywords internal
.omopAchillesContinuousByYear <- function(handle, ctx) {
  obs <- .qualifyTable(handle, "observation_period")

  .omopDiagAssertPersons(handle, ctx, obs, "op", "person_id")

  spine <- .omopAchillesYearSpineSql(handle, ctx, "ys")
  if (is.null(spine)) {
    return(data.frame(calendar_year = integer(0), count_value = integer(0),
                      stringsAsFactors = FALSE))
  }

  # observation_period FIRST (so the scope join's ON resolves against op), then
  # CROSS JOIN the year spine, then the year-coverage predicate in WHERE. (Putting
  # the predicate in an ON clause would break the un-scoped run, where the scope
  # join is empty and there is no ON to attach to.)
  sj <- .omopScopeJoin(ctx, "op", "person_id")
  start_year <- .omopYearExpr(handle, "op.observation_period_start_date")
  end_year   <- .omopYearExpr(handle, "op.observation_period_end_date")

  sql <- .sql_translate(paste0(
    "SELECT ys.yr AS calendar_year, ",
    "COUNT(DISTINCT op.person_id) AS count_value ",
    "FROM ", obs, " op", sj$join,
    " CROSS JOIN ", spine,
    " WHERE ys.yr BETWEEN ", start_year, " AND ", end_year,
    " GROUP BY ys.yr",
    " ORDER BY ys.yr"),
    handle$target_dialect)
  .executeQuery(handle, sql)
}

#' Persons by number of observation periods (113)
#'
#' Inner query counts each person's number of observation periods
#' (\code{COUNT(*)} GROUP BY person_id, with the scope join applied there); the
#' outer query counts \code{COUNT(DISTINCT person_id)} per distinct period-count.
#' Self-gates before materialising. Returns aggregate-only.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @return Aggregate-only data.frame (num_periods, count_value).
#' @keywords internal
.omopAchillesNumObsPeriods <- function(handle, ctx) {
  obs <- .qualifyTable(handle, "observation_period")
  sj  <- .omopScopeJoin(ctx, "op", "person_id")

  .omopDiagAssertPersons(handle, ctx, obs, "op", "person_id")

  sql <- .sql_translate(paste0(
    "SELECT per.np AS num_periods, ",
    "COUNT(DISTINCT per.person_id) AS count_value FROM ",
    "(SELECT op.person_id AS person_id, COUNT(*) AS np FROM ", obs, " op",
    sj$join, " GROUP BY op.person_id) per ",
    "GROUP BY per.np",
    " ORDER BY per.np"),
    handle$target_dialect)
  .executeQuery(handle, sql)
}

#' Build one native observation-period COUNT catalog entry (101/102/108/109/113)
#'
#' Shared entry constructor for the count sub-group: a kind="r" entry whose
#' disclosure declares \code{unit="person"} / \code{count_cols="count_value"} so
#' the ONE gate small-cell-suppresses + bands the distinct-person count. Scope is
#' LIVE (cohort + single table, \code{max_tables=1}); \code{requires_cohort} stays
#' FALSE so an un-scoped run computes cohort-wide (Achilles whole-database
#' semantics). The stable id \code{dsomop:achilles.<aid>} is preserved;
#' \code{meta$analysis_id} is a LABEL only (never used to read a results table).
#'
#' @param aid Integer Achilles analysis id.
#' @param description Human-readable analysis name.
#' @param tables Character vector of CDM tables the fn reads (for the deps WARN).
#' @param fn The kind="r" compute function(handle, ctx, params).
#' @return A \code{omop_analysis_entry}.
#' @keywords internal
.omopAchillesObsPeriodCountEntry <- function(aid, description, tables, fn) {
  .omopAnalysisEntry(
    name        = paste0("dsomop:achilles.", aid),
    description = description,
    domain      = "observation_period",
    params      = list(),
    compute     = list(kind = "r", sql = NULL, fn = fn),
    dependencies = list(tables = tables, packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(unit = "person",
                                          count_cols = "count_value"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "achilles", analysis_id = as.integer(aid)))
}

#' Emit the live-computing Achilles observation-period COUNT entries
#'
#' Analyses 101/102/108/109/113 (persons by age at first observation period;
#' persons by gender x age at first observation period; persons by observation-
#' period length in 30-day increments; persons with continuous observation in each
#' year; persons by number of observation periods), each computing its distinct-
#' person count LIVE from the CDM \code{observation_period} table (NOT reading
#' achilles_results).
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by entry name.
#' @keywords internal
.omopAchillesObsPeriodCountEntries <- function(handle) {
  entries <- list(
    # 101 — persons by age at first observation period (5-year bands).
    .omopAchillesObsPeriodCountEntry(
      101L, "Number of persons by age at first observation period.",
      c("observation_period", "person"),
      function(handle, ctx, params) {
        .omopAchillesAgeAtFirstObs(handle, ctx, by_gender = FALSE)
      }),

    # 102 — persons by gender by age at first observation period.
    .omopAchillesObsPeriodCountEntry(
      102L,
      "Number of persons by gender by age at first observation period.",
      c("observation_period", "person", "concept"),
      function(handle, ctx, params) {
        .omopAchillesAgeAtFirstObs(handle, ctx, by_gender = TRUE)
      }),

    # 108 — persons by observation-period length, 30-day increments.
    .omopAchillesObsPeriodCountEntry(
      108L,
      "Number of persons by observation period length, 30-day increments.",
      c("observation_period"),
      function(handle, ctx, params) .omopAchillesObsPeriodLength(handle, ctx)),

    # 109 — persons with continuous observation in each calendar year.
    .omopAchillesObsPeriodCountEntry(
      109L, "Number of persons with continuous observation in each year.",
      c("observation_period"),
      function(handle, ctx, params) .omopAchillesContinuousByYear(handle, ctx)),

    # 113 — persons by number of observation periods.
    .omopAchillesObsPeriodCountEntry(
      113L, "Number of persons by number of observation periods.",
      c("observation_period"),
      function(handle, ctx, params) .omopAchillesNumObsPeriods(handle, ctx))
  )
  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}

#' Register the native Achilles observation-period catalog entries
#'
#' The single register_fn for the observation-period Achilles group that the
#' Achilles adapter (\code{\link{.omopAnalysisAchillesEntries}}) concatenates. It
#' aggregates the two sub-group builders — the distribution analyses
#' (\code{\link{.omopAchillesObsPeriodDistEntries}}, 103-107) and the count-based
#' analyses (\code{\link{.omopAchillesObsPeriodCountEntries}}, 101/102/108/109/
#' 113) — so they compose without sharing a function body. Returns a named list
#' keyed by entry id.
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by entry name.
#' @keywords internal
.omopAchillesObsPeriodEntries <- function(handle) {
  c(.omopAchillesObsPeriodDistEntries(handle),
    .omopAchillesObsPeriodCountEntries(handle))
}
