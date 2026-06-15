# Module: OHDSI CohortDiagnostics catalog entries (native live compute)
#
# Group OHDSI-B2-cohortdx-new. The CohortDiagnostics-family analyses, ported
# from the precomputed-results adapter (.ohdsiGetResults + .ohdsiPersonGate
# reading cd_*/cohort_diagnostics result tables) to native, live-computing
# catalog entries. Nothing here reads a precomputed OHDSI/CohortDiagnostics
# result table: every metric is COMPUTED LIVE from the CDM over the SCOPED
# cohort, restricted by an INNER JOIN on subject_id (ctx$scoped_cohort), with
# concepts translated to names by default (LEFT JOIN concept) and NO
# *_source_value / free-text ever selected.
#
# The group registrar .ohdsiPackCohortDxEntries(handle) returns ALL of the
# group's entries keyed by their STABLE ids so existing references keep
# resolving; only the compute changes (read-results -> live-compute). It is
# concatenated at the single OHDSI aggregation point (.omopAnalysisOhdsiEntries),
# exactly as the parallel Achilles pack registrars are concatenated at
# .omopAnalysisAchillesEntries.
#
# Two id families are emitted, both built here so the group is self-contained
# (no shared-file contention with other ported groups):
#   * the registry ids the precomputed adapter used:
#       dsomop:ohdsi.cohort_diagnostics.cohort_count
#       dsomop:ohdsi.cohort_diagnostics.temporal_covariate_value
#       dsomop:ohdsi.cohort_diagnostics.temporal_covariate_value_dist
#       dsomop:ohdsi.cohort_diagnostics.time_series
#       dsomop:ohdsi.cohort_diagnostics.included_source_concept
#       dsomop:characterization.c_cohort_counts  (shares the cohort_count fn)
#   * the NEW canonical natives the registry ids alias (same fn, canonical name):
#       dsomop:cohortdx.temporal_prevalence       (PLAN dsomop:fe.temporal_prevalence)
#       dsomop:cohortdx.time_series
#       dsomop:cohortdx.included_source_concepts
# Each canonical and its alias share the SAME compute$fn/disclosure/scope; only
# name/description differ, so the metric and its gate behaviour are identical
# whichever id is referenced.
#
# Disclosure (the SINGLE .omopAnalysisGate does ALL suppression/banding/coupling
# from the declared spec; nothing here pre-gates a final count):
#   * cohort_count / c_cohort_counts: a one-row self-count of the scoped cohort.
#     unit="record" with count_cols=c("cohort_subjects","cohort_entries") and
#     person_id_col="cohort_subjects" so the gate's GENERIC record branch gates
#     the record cell (cohort_entries) on the distinct-person companion
#     (cohort_subjects).
#   * temporal_prevalence: per-(covariate x time-window) distinct-person
#     prevalence. unit="person", count_cols="sum_value", with a paired
#     "average" proportion RECONCILED in-fn from the banded numerator over the
#     banded cohort denominator (.omopAnalysisReconcileRatio) so a raw ratio can
#     never re-derive the un-banded numerator; the gate's binary-prevalence
#     coupling then NAs it wherever sum_value is suppressed/zero.
#   * temporal_covariate_value_dist: per-(covariate x time-window) continuous
#     dist rows (count_value + min/max + avg/sd + p10..p90). unit="dist" so the
#     gate strips min/max and masks summary stats below nfilter_dist. Quantiles
#     are emitted as p10/p25/median/p75/p90 (never native 0%/100% = min/max).
#   * time_series: records/subjects/person_days per calendar year over the
#     scoped cohort. unit="person", count_cols=c("subjects","records",
#     "person_days") with person_id_col="subjects" companions.
#   * included_source_concepts: per source-concept distinct-person + record
#     count of cohort members' events. unit="record",
#     count_cols=c("concept_count","concept_subjects"),
#     person_id_col="concept_subjects".
# meta$adapter is "ohdsi_live" (NOT "ohdsi") on the record-unit entries so the
# gate routes them through its GENERIC record branch (the companion person count
# travels in the frame) rather than the precomputed-sibling .ohdsiPersonGate.
#
# Scope on every entry: accepts_cohort=TRUE, accepts_tables=TRUE, max_tables=1L,
# requires_cohort=FALSE (honest per the group spec). The compute fns are
# cohort-relative and return an empty (gate-safe) frame when un-scoped.

# --- Time-window helper -------------------------------------------------------

#' Cohort-index-relative day-window predicate for a temporal covariate
#'
#' The CohortDiagnostics temporal covariate analyses summarise covariates in
#' day windows ANCHORED on the cohort index date (e.g. [-365, -1] = the year
#' before index). This builds the SQL predicate that keeps an event whose date
#' falls in \code{[start_days, end_days]} relative to \code{cohort_start_date},
#' reusing the dialect-aware whole-day difference expression
#' (\code{\link{.omopDateDiffDays}}) so the window is computed identically on
#' every engine.
#'
#' @param handle CDM handle (selects the dialect).
#' @param event_date_expr Character; SQL date expression for the event date.
#' @param start_days,end_days Character/int; inclusive window bounds in days
#'   relative to the cohort index date (negative = before index).
#' @return Character; a SQL boolean predicate (no leading AND).
#' @keywords internal
.ohdsiCohortDxWindowPredicate <- function(handle, event_date_expr,
                                          start_days, end_days) {
  diff <- .omopDateDiffDays(handle, event_date_expr, "c.cohort_start_date")
  s <- as.integer(start_days)
  e <- as.integer(end_days)
  paste0(diff, " >= ", s, " AND ", diff, " <= ", e)
}

#' Restrict a domain to an optional covariate-id IN-list
#'
#' The temporal covariate analyses optionally scope to a covariate set
#' (\code{covariate_ids} = concept ids, descendants expanded server-side via
#' \code{\link{.resolveConceptSet}}). Returns the SQL IN-list predicate (no
#' leading AND) or \code{""} when no valid id was supplied (all concepts).
#'
#' @param handle CDM handle.
#' @param concept_col_expr Character; qualified concept column (e.g. "e.x").
#' @param covariate_ids Character/int scalar or NULL (sanitized literal).
#' @return Character; a SQL predicate fragment or "".
#' @keywords internal
.ohdsiCohortDxCovariatePredicate <- function(handle, concept_col_expr,
                                             covariate_ids) {
  if (is.null(covariate_ids) || !nzchar(as.character(covariate_ids))) return("")
  ids <- .resolveConceptSet(handle,
                            list(concepts = as.integer(covariate_ids),
                                 include_descendants = TRUE))
  if (length(ids) == 0) {
    ids <- suppressWarnings(as.integer(covariate_ids))
    ids <- ids[!is.na(ids)]
  }
  if (length(ids) == 0) return("")
  paste0(concept_col_expr, " IN (", paste(ids, collapse = ", "), ")")
}

# --- cohort_count / c_cohort_counts -------------------------------------------

#' Live self-count of the scoped cohort (cohort_count / c_cohort_counts fn)
#'
#' Replaces the precomputed CohortDiagnostics \code{cohort_count} /
#' Characterization \code{c_cohort_counts} table read with a single live count
#' over the scoped cohort temp table: \code{cohort_subjects} =
#' COUNT(DISTINCT subject_id), \code{cohort_entries} = COUNT(*) (one row per
#' cohort entry). Self-gated via \code{\link{.omopDiagAssertPersons}} over the
#' cohort BEFORE materialising; returned UN-GATED (the record cell is gated by
#' the gate's generic record branch on the \code{cohort_subjects} companion).
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param params Sanitized params (unused).
#' @return One-row data frame (cohort_subjects, cohort_entries) or empty.
#' @keywords internal
.ohdsiCohortDxCohortCountFn <- function(handle, ctx, params) {
  if (is.null(ctx$scoped_cohort)) return(data.frame())
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")

  # Self-gate the scoped population over the cohort itself BEFORE materialising.
  .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
    handle$target_dialect))

  sql <- .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS cohort_subjects, ",
    "COUNT(*) AS cohort_entries FROM ", cohort),
    handle$target_dialect)
  .executeQuery(handle, sql)
}

# --- temporal_covariate_value -> temporal_prevalence --------------------------

#' Per-(covariate x time-window) distinct-person prevalence (temporal_prevalence)
#'
#' Replaces the precomputed \code{temporal_covariate_value} read with a live
#' per-window binary prevalence reusing the covariate-domain mapping
#' (\code{\link{.omopCovariateSource}}) and the SAME column conventions as the
#' binary kernel (\code{\link{.omopCovariatePrevalence}}): one row per
#' (time-window x covariate), \code{sum_value} = distinct cohort persons with
#' >= 1 record of the concept INSIDE the window, \code{average} =
#' \code{sum_value / cohort_size}. The window predicate is index-relative
#' (\code{\link{.ohdsiCohortDxWindowPredicate}}). Concepts translated to names.
#' \code{unit="person"}: \code{average} is RECONCILED in-fn from the banded
#' numerator over the banded cohort denominator
#' (\code{\link{.omopAnalysisReconcileRatio}}) so the released proportion rests on
#' banded counts (a raw ratio would re-derive the un-banded numerator); the gate
#' then small-cell-suppresses + bands \code{sum_value} (idempotent on the already-
#' banded value) and couples \code{average} away wherever \code{sum_value} is
#' suppressed/zero.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param params Sanitized params: \code{domain_code}, \code{covariate_ids},
#'   \code{temporal_start_days}, \code{temporal_end_days}, \code{top_n}.
#' @return Data frame (time_window, covariate_id, covariate_name, sum_value,
#'   average) or empty.
#' @keywords internal
.ohdsiCohortDxTemporalPrevalenceFn <- function(handle, ctx, params) {
  if (is.null(ctx$scoped_cohort)) return(data.frame())
  cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  src     <- .omopCovariateSource(handle, params$domain_code %||% "0")
  top_n   <- max(as.integer(params$top_n %||% "50"), 1L)
  start_d <- as.integer(params$temporal_start_days %||% "-365")
  end_d   <- as.integer(params$temporal_end_days %||% "-1")

  win_pred <- .ohdsiCohortDxWindowPredicate(
    handle, paste0("e.", src$date_col), start_d, end_d)
  cov_pred <- .ohdsiCohortDxCovariatePredicate(
    handle, paste0("e.", src$concept_col), params$covariate_ids)
  where <- paste0(win_pred, if (nzchar(cov_pred)) paste0(" AND ", cov_pred) else "")

  # Self-gate the scoped + windowed population BEFORE materialising.
  .omopDiagAssertPersons(handle, ctx, src$table, "e", src$person_col,
                         where_sql = NULL)

  win_label <- paste0("[", start_d, ",", end_d, "]")
  denom_sql <- paste0("(SELECT COUNT(DISTINCT subject_id) FROM ", cohort, ")")
  sql <- .sql_translate(paste0(
    "SELECT '", win_label, "' AS time_window, ",
    "e.", src$concept_col, " AS covariate_id, cc.concept_name AS covariate_name, ",
    "COUNT(DISTINCT e.", src$person_col, ") AS sum_value, ",
    denom_sql, " AS cohort_size ",
    "FROM ", src$table, " e ",
    "INNER JOIN ", cohort, " c ON c.subject_id = e.", src$person_col,
    " LEFT JOIN ", concept, " cc ON cc.concept_id = e.", src$concept_col,
    " WHERE ", where,
    " GROUP BY e.", src$concept_col, ", cc.concept_name",
    " ORDER BY sum_value DESC"),
    handle$target_dialect)

  df <- .executeQuery(handle, sql)
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  if (nrow(df) > top_n) df <- df[seq_len(top_n), , drop = FALSE]

  # The proportion (average = sum_value / cohort_size) must NEVER be released as a
  # raw ratio: average * denom re-derives the EXACT un-banded numerator, defeating
  # the band the gate floors sum_value to. Recompute it from the BANDED numerator
  # over the BANDED denominator (NA'd when either is suppressed) via the SAME
  # reconcile helper the record-unit derived ratios use, BEFORE the frame reaches
  # the single gate. The gate's own band pass over sum_value is idempotent on the
  # already-banded value, and its binary-prevalence coupling NAs the (already
  # reconciled) average wherever sum_value is suppressed/zero.
  df$average <- NA_real_
  df <- .omopAnalysisReconcileRatio(df, numerator_col = "sum_value",
                                    denominator_col = "cohort_size",
                                    ratio_col = "average", scale = 1)
  # Drop the (now-banded) denominator helper so the output schema is unchanged.
  df <- df[, setdiff(names(df), "cohort_size"), drop = FALSE]
  rownames(df) <- NULL
  df
}

# --- temporal_covariate_value_dist --------------------------------------------

#' Per-(covariate x time-window) continuous distribution (temporal dist fn)
#'
#' Replaces the precomputed \code{temporal_covariate_value_dist} read with a
#' live per-window continuous distribution: pulls one numeric value per
#' (covariate, person) INSIDE the index-relative window into R and summarises
#' each covariate into a dist row with the SAME column conventions as the
#' continuous kernel (\code{\link{.omopCovariateContinuous}}): \code{count_value}
#' (persons contributing), min/max (stripped by the gate), avg/sd, and
#' p10/p25/median/p75/p90 (masked by the gate below nfilter_dist). Quantiles are
#' emitted as p10..p90 ONLY (never native 0%/100% = min/max). \code{value_kind}
#' selects the value: \code{"value"} (measurement/observation
#' \code{value_as_number}) or \code{"count"} (per-person record count).
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param params Sanitized params: \code{domain_code}, \code{value_kind},
#'   \code{covariate_ids}, \code{temporal_start_days}, \code{temporal_end_days},
#'   \code{top_n}.
#' @return Data frame of dist rows (one per covariate) or empty.
#' @keywords internal
.ohdsiCohortDxTemporalDistFn <- function(handle, ctx, params) {
  if (is.null(ctx$scoped_cohort)) return(data.frame())
  cohort     <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  concept    <- .qualifyTable(handle, "concept",
                              handle$vocab_schema %||% handle$cdm_schema)
  src        <- .omopCovariateSource(handle, params$domain_code %||% "3")
  value_kind <- params$value_kind %||% "value"
  top_n      <- max(as.integer(params$top_n %||% "50"), 1L)
  start_d    <- as.integer(params$temporal_start_days %||% "-365")
  end_d      <- as.integer(params$temporal_end_days %||% "-1")

  win_pred <- .ohdsiCohortDxWindowPredicate(
    handle, paste0("e.", src$date_col), start_d, end_d)
  cov_pred <- .ohdsiCohortDxCovariatePredicate(
    handle, paste0("e.", src$concept_col), params$covariate_ids)
  extra    <- paste0(" AND ", win_pred,
                     if (nzchar(cov_pred)) paste0(" AND ", cov_pred) else "")

  .omopDiagAssertPersons(handle, ctx, src$table, "e", src$person_col)

  if (identical(value_kind, "value") && !is.na(src$value_col)) {
    per_person <- paste0(
      "SELECT e.", src$concept_col, " AS covariate_id, e.", src$person_col,
      " AS person_id, AVG(CAST(e.", src$value_col, " AS FLOAT)) AS v ",
      "FROM ", src$table, " e ",
      "INNER JOIN ", cohort, " c ON c.subject_id = e.", src$person_col,
      " WHERE e.", src$value_col, " IS NOT NULL", extra,
      " GROUP BY e.", src$concept_col, ", e.", src$person_col)
  } else {
    per_person <- paste0(
      "SELECT e.", src$concept_col, " AS covariate_id, e.", src$person_col,
      " AS person_id, COUNT(*) AS v ",
      "FROM ", src$table, " e ",
      "INNER JOIN ", cohort, " c ON c.subject_id = e.", src$person_col,
      " WHERE 1 = 1", extra,
      " GROUP BY e.", src$concept_col, ", e.", src$person_col)
  }
  vsql <- .sql_translate(paste0(
    "SELECT pp.covariate_id, cc.concept_name AS covariate_name, pp.v ",
    "FROM (", per_person, ") pp",
    " LEFT JOIN ", concept, " cc ON cc.concept_id = pp.covariate_id"),
    handle$target_dialect)
  raw <- .executeQuery(handle, vsql)
  if (!is.data.frame(raw) || nrow(raw) == 0) return(data.frame())

  raw$v <- suppressWarnings(as.numeric(raw$v))
  raw <- raw[!is.na(raw$v), , drop = FALSE]
  if (nrow(raw) == 0) return(data.frame())

  win_label <- paste0("[", start_d, ",", end_d, "]")
  parts <- split(raw, raw$covariate_id)
  rows <- lapply(parts, function(p) {
    v  <- p$v
    qs <- stats::quantile(v, c(.10, .25, .5, .75, .90), names = FALSE, type = 7)
    data.frame(
      time_window    = win_label,
      covariate_id   = p$covariate_id[1],
      covariate_name = p$covariate_name[1],
      count_value    = length(v),
      min_value      = min(v),   # stripped by the gate
      max_value      = max(v),   # stripped by the gate
      avg_value      = mean(v),
      stdev_value    = stats::sd(v),
      p10_value      = qs[1], p25_value = qs[2], median_value = qs[3],
      p75_value      = qs[4], p90_value = qs[5],
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out <- out[order(-out$count_value), , drop = FALSE]
  if (nrow(out) > top_n) out <- out[seq_len(top_n), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# --- time_series --------------------------------------------------------------

#' Per-calendar-year cohort activity series (time_series fn)
#'
#' Replaces the precomputed \code{time_series} read with a live calendar-year
#' series over the SCOPED cohort ONLY (the CohortDiagnostics T1/T2
#' cohort-relative series; the whole-DB T3 series is DROPPED as not scopable):
#' per cohort-entry year (\code{\link{.omopYearExpr}} over
#' \code{cohort_start_date}), \code{subjects} = COUNT(DISTINCT subject_id),
#' \code{records} = COUNT(*) (cohort entries that year), and \code{person_days} =
#' summed in-cohort days (end - start, end column resolved via
#' \code{\link{.omopCohortEndDateCol}}, falling back to a single-day window).
#' Self-gated over the cohort BEFORE materialising. \code{unit="person"} with the
#' record/person-day companions gated alongside the distinct-person count.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param params Sanitized params (unused).
#' @return Data frame (calendar_year, subjects, records, person_days) or empty.
#' @keywords internal
.ohdsiCohortDxTimeSeriesFn <- function(handle, ctx, params) {
  if (is.null(ctx$scoped_cohort)) return(data.frame())
  cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  end_col <- .omopCohortEndDateCol(handle, cohort)
  year    <- .omopYearExpr(handle, "c.cohort_start_date")
  days    <- .omopDateDiffDays(handle, paste0("c.", end_col),
                               "c.cohort_start_date")

  .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
    handle$target_dialect))

  sql <- .sql_translate(paste0(
    "SELECT ", year, " AS calendar_year, ",
    "COUNT(DISTINCT c.subject_id) AS subjects, ",
    "COUNT(*) AS records, ",
    "SUM(", days, ") AS person_days ",
    "FROM ", cohort, " c",
    " GROUP BY ", year,
    " ORDER BY calendar_year"),
    handle$target_dialect)
  .executeQuery(handle, sql)
}

# --- included_source_concept -> included_source_concepts ----------------------

#' Per-source-concept cohort coverage (included_source_concepts fn)
#'
#' Replaces the precomputed \code{included_source_concept} read with a live
#' per-(STANDARD) concept count of the cohort members' events in the chosen
#' domain: \code{concept_count} = COUNT(*) records of the concept among cohort
#' members, \code{concept_subjects} = COUNT(DISTINCT person_id). The grouping is
#' on the STANDARD concept id (translated to its name); the gate drops any
#' \code{*_source_concept_id} column automatically and this fn NEVER selects a
#' \code{*_source_value} / \code{*_source_concept_id} column (hard rule). Self-
#' gated BEFORE materialising; returned UN-GATED (the record cell is gated by the
#' generic record branch on the \code{concept_subjects} companion).
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param params Sanitized params: \code{domain_code}, \code{top_n}.
#' @return Data frame (concept_id, concept_name, concept_count, concept_subjects)
#'   or empty.
#' @keywords internal
.ohdsiCohortDxIncludedSourceConceptFn <- function(handle, ctx, params) {
  if (is.null(ctx$scoped_cohort)) return(data.frame())
  cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  src     <- .omopCovariateSource(handle, params$domain_code %||% "0")
  top_n   <- max(as.integer(params$top_n %||% "50"), 1L)

  .omopDiagAssertPersons(handle, ctx, src$table, "e", src$person_col)

  sql <- .sql_translate(paste0(
    "SELECT e.", src$concept_col, " AS concept_id, cc.concept_name, ",
    "COUNT(*) AS concept_count, ",
    "COUNT(DISTINCT e.", src$person_col, ") AS concept_subjects ",
    "FROM ", src$table, " e ",
    "INNER JOIN ", cohort, " c ON c.subject_id = e.", src$person_col,
    " LEFT JOIN ", concept, " cc ON cc.concept_id = e.", src$concept_col,
    " WHERE e.", src$concept_col, " IS NOT NULL",
    " GROUP BY e.", src$concept_col, ", cc.concept_name",
    " ORDER BY concept_count DESC"),
    handle$target_dialect)

  df <- .executeQuery(handle, sql)
  if (is.data.frame(df) && nrow(df) > top_n) df <- df[seq_len(top_n), , drop = FALSE]
  df
}

# --- Entry builders -----------------------------------------------------------

#' Shared param spec for the temporal covariate analyses
#' @keywords internal
.ohdsiCohortDxTemporalParams <- function(include_value_kind = FALSE) {
  base <- list(
    list(name = "domain_code", type = "enum", required = FALSE,
         default = if (include_value_kind) "3" else "0",
         choices = c("0", "1", "2", "3", "4"),
         description = paste0("Covariate domain (0 condition,1 drug,2 procedure,",
                              "3 measurement,4 observation).")),
    list(name = "covariate_ids", type = "concept_id", required = FALSE,
         default = NULL,
         description = "Restrict to this covariate concept id and its descendants (all concepts when omitted)."),
    list(name = "temporal_start_days", type = "int", required = FALSE,
         default = "-365",
         description = "Window start in days relative to the cohort index date."),
    list(name = "temporal_end_days", type = "int", required = FALSE,
         default = "-1",
         description = "Window end in days relative to the cohort index date."),
    list(name = "top_n", type = "int", required = FALSE, default = "50")
  )
  if (include_value_kind) {
    base <- c(base, list(
      list(name = "value_kind", type = "enum", required = FALSE,
           default = "value", choices = c("value", "count"),
           description = "Distribute the measured value ('value') or the per-person record count ('count').")
    ))
  }
  base
}

#' Build the cohort-count entry under a given id (cohort_count / c_cohort_counts)
#' @keywords internal
.ohdsiCohortDxCohortCountEntry <- function(name, adapter, meta) {
  .omopAnalysisEntry(
    name        = name,
    description = "Distinct subjects and entry count of the scoped cohort.",
    domain      = "general",
    params      = list(),
    compute     = list(kind = "r", sql = NULL,
                       fn = .ohdsiCohortDxCohortCountFn),
    dependencies = list(tables = character(0), packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(
      unit          = "record",
      count_cols    = c("cohort_subjects", "cohort_entries"),
      person_id_col = "cohort_subjects"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = c(list(adapter = adapter), meta)
  )
}

#' Build the temporal-prevalence entry under a given id
#' @keywords internal
.ohdsiCohortDxTemporalPrevalenceEntry <- function(name, adapter, meta) {
  .omopAnalysisEntry(
    name        = name,
    description = paste0("Per-(covariate x index-relative time window) distinct-",
                         "person prevalence over the scoped cohort."),
    domain      = "general",
    params      = .ohdsiCohortDxTemporalParams(include_value_kind = FALSE),
    compute     = list(kind = "r", sql = NULL,
                       fn = .ohdsiCohortDxTemporalPrevalenceFn),
    dependencies = list(tables = c("condition_occurrence", "concept"),
                        packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(
      unit = "person", count_cols = "sum_value"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = c(list(adapter = adapter), meta)
  )
}

#' Build the temporal continuous-distribution entry under a given id
#' @keywords internal
.ohdsiCohortDxTemporalDistEntry <- function(name, adapter, meta) {
  .omopAnalysisEntry(
    name        = name,
    description = paste0("Per-(covariate x index-relative time window) continuous",
                         " value/count distribution over the scoped cohort."),
    domain      = "general",
    params      = .ohdsiCohortDxTemporalParams(include_value_kind = TRUE),
    compute     = list(kind = "r", sql = NULL,
                       fn = .ohdsiCohortDxTemporalDistFn),
    dependencies = list(tables = c("measurement", "concept"),
                        packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(
      unit = "dist", count_cols = "count_value", min_max = TRUE),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = c(list(adapter = adapter), meta)
  )
}

#' Build the time-series entry under a given id
#' @keywords internal
.ohdsiCohortDxTimeSeriesEntry <- function(name, adapter, meta) {
  .omopAnalysisEntry(
    name        = name,
    description = paste0("Per-calendar-year distinct subjects, cohort entries and",
                         " in-cohort person-days over the scoped cohort."),
    domain      = "general",
    params      = list(),
    compute     = list(kind = "r", sql = NULL,
                       fn = .ohdsiCohortDxTimeSeriesFn),
    dependencies = list(tables = character(0), packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(
      unit          = "person",
      count_cols    = c("subjects", "records", "person_days"),
      person_id_col = "subjects"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = c(list(adapter = adapter), meta)
  )
}

#' Build the included-source-concepts entry under a given id
#' @keywords internal
.ohdsiCohortDxIncludedSourceConceptEntry <- function(name, adapter, meta) {
  .omopAnalysisEntry(
    name        = name,
    description = paste0("Per-standard-concept record count and distinct-subject",
                         " coverage of the scoped cohort members' events."),
    domain      = "general",
    params      = list(
      list(name = "domain_code", type = "enum", required = FALSE, default = "0",
           choices = c("0", "1", "2", "3", "4"),
           description = paste0("Event domain (0 condition,1 drug,2 procedure,",
                                "3 measurement,4 observation).")),
      list(name = "top_n", type = "int", required = FALSE, default = "50")
    ),
    compute     = list(kind = "r", sql = NULL,
                       fn = .ohdsiCohortDxIncludedSourceConceptFn),
    dependencies = list(tables = c("condition_occurrence", "concept"),
                        packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(
      unit          = "record",
      count_cols    = c("concept_count", "concept_subjects"),
      person_id_col = "concept_subjects"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = c(list(adapter = adapter), meta)
  )
}

# --- Group registrar ----------------------------------------------------------

#' Emit the OHDSI CohortDiagnostics catalog entries (native live compute)
#'
#' The group registrar for \code{ohdsi_pack_cohortdx.R}: every entry COMPUTES
#' its metric LIVE from the CDM over the scoped cohort (no precomputed
#' CohortDiagnostics/Characterization result-table read). Returns a named list
#' keyed by stable entry id, exactly as \code{\link{.omopAnalysisOhdsiEntries}}
#' expects to concatenate.
#'
#' Both the registry ids the precomputed adapter used
#' (\code{dsomop:ohdsi.cohort_diagnostics.*}, \code{dsomop:characterization.c_cohort_counts})
#' and the NEW canonical natives they alias (\code{dsomop:cohortdx.temporal_prevalence},
#' \code{dsomop:cohortdx.time_series}, \code{dsomop:cohortdx.included_source_concepts})
#' are emitted here, each canonical sharing its alias's compute$fn/disclosure/
#' scope (only name/description differ), so the metric and gate behaviour are
#' identical whichever id is referenced. All ids are globally unique by
#' construction. \code{handle} is taken for signature parity ONLY and is never
#' queried at build time (catalog build is cached on
#' \code{handle$analysis_catalog}; all DB I/O happens later inside each
#' \code{compute$fn}).
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by entry id.
#' @keywords internal
.ohdsiPackCohortDxEntries <- function(handle) {
  entries <- list(
    # cohort_count (CohortDiagnostics) + c_cohort_counts (Characterization)
    # share the same live self-count fn.
    .ohdsiCohortDxCohortCountEntry(
      "dsomop:ohdsi.cohort_diagnostics.cohort_count", "ohdsi_live",
      list(tool_id = "cohort_diagnostics", table_name = "cohort_count")),
    .ohdsiCohortDxCohortCountEntry(
      "dsomop:characterization.c_cohort_counts", "ohdsi_live",
      list(tool_id = "characterization", table_name = "c_cohort_counts")),

    # temporal_covariate_value -> canonical temporal_prevalence (+ alias).
    .ohdsiCohortDxTemporalPrevalenceEntry(
      "dsomop:cohortdx.temporal_prevalence", "diagnostic",
      list()),
    .ohdsiCohortDxTemporalPrevalenceEntry(
      "dsomop:ohdsi.cohort_diagnostics.temporal_covariate_value", "diagnostic",
      list(tool_id = "cohort_diagnostics",
           table_name = "temporal_covariate_value")),

    # temporal_covariate_value_dist -> continuous covariate dist per window.
    .ohdsiCohortDxTemporalDistEntry(
      "dsomop:ohdsi.cohort_diagnostics.temporal_covariate_value_dist",
      "diagnostic",
      list(tool_id = "cohort_diagnostics",
           table_name = "temporal_covariate_value_dist")),

    # time_series -> canonical time_series (+ alias).
    .ohdsiCohortDxTimeSeriesEntry(
      "dsomop:cohortdx.time_series", "ohdsi_live", list()),
    .ohdsiCohortDxTimeSeriesEntry(
      "dsomop:ohdsi.cohort_diagnostics.time_series", "ohdsi_live",
      list(tool_id = "cohort_diagnostics", table_name = "time_series")),

    # included_source_concept -> canonical included_source_concepts (+ alias).
    .ohdsiCohortDxIncludedSourceConceptEntry(
      "dsomop:cohortdx.included_source_concepts", "ohdsi_live", list()),
    .ohdsiCohortDxIncludedSourceConceptEntry(
      "dsomop:ohdsi.cohort_diagnostics.included_source_concept", "ohdsi_live",
      list(tool_id = "cohort_diagnostics",
           table_name = "included_source_concept"))
  )
  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}
