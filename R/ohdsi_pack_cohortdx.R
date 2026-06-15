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
#       dsomop:ohdsi.cohort_diagnostics.orphan_concept
#       dsomop:characterization.c_cohort_counts  (shares the cohort_count fn)
#   * the NEW canonical natives the registry ids alias (same fn, canonical name):
#       dsomop:cohortdx.temporal_prevalence       (PLAN dsomop:fe.temporal_prevalence)
#       dsomop:cohortdx.time_series
#       dsomop:cohortdx.included_source_concepts
#       dsomop:cohortdx.resolved_concepts
#       dsomop:cohortdx.orphan_concepts
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
#   * resolved_concepts: the LIVE concept-set expansion (every standard concept a
#     concept-set definition resolves to). Concept METADATA only (no count), so
#     it is disclosure-safe by construction: unit="record" with NO count_cols and
#     adapter="ohdsi" so the gate routes the count-less frame through
#     .ohdsiPersonGate's pass-through (a table with no count columns is left
#     unchanged). The only enumeration risk (an unbounded vocabulary slice) is
#     fail-closed capped in-fn at nfilter.levels.max.
#   * orphan_concepts: candidate concept-set GAPS — concepts in the vocabulary
#     neighbourhood of the seed (descendants + Maps to/Is a) that are NOT in the
#     resolved set yet DO carry CDM data. The vocabulary walk is non-disclosive;
#     the per-candidate CDM presence IS a genuine single-site per-patient
#     aggregate, so unit="record" with count_cols=c("n_records","n_persons") and
#     person_id_col="n_persons" routes it (adapter "ohdsi_live") through the gate's
#     GENERIC record branch (n_records gated on the n_persons companion, both
#     banded, candidates below nfilter.subset dropped). The candidate LIST length
#     is fail-closed capped at nfilter.levels.max in-fn.
# meta$adapter is "ohdsi_live" (NOT "ohdsi") on the count-carrying record-unit
# entries so the gate routes them through its GENERIC record branch (the
# companion person count travels in the frame) rather than the precomputed-
# sibling .ohdsiPersonGate; resolved_concepts is the one exception (adapter
# "ohdsi") because its count-less metadata needs exactly that pass-through.
#
# Scope: the count-carrying entries are accepts_cohort=TRUE, accepts_tables=TRUE,
# max_tables=1L, requires_cohort=FALSE (honest per the group spec; cohort-
# relative, returning an empty gate-safe frame when un-scoped). resolved_concepts
# is the exception: it is a VOCABULARY computation with no person key, so it is
# honestly NOT scopable (accepts_cohort=FALSE, max_tables=0L), like the
# precomputed entry it overlays.

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

# --- resolved_concepts (live concept-set expansion) ---------------------------

#' Live concept-set expansion to its resolved standard concepts (resolved_concepts fn)
#'
#' Replaces the precomputed CohortDiagnostics \code{resolved_concepts} read
#' (\code{\link{.ohdsiGetResults}} over a \code{cd_resolved_concepts} result
#' table) with the LIVE vocabulary computation OHDSI's resolved_concepts IS: the
#' full expansion of a concept-set definition to every standard concept it
#' resolves to. This is metadata assembly over the vocabulary (NOT a per-patient
#' query): it never touches a clinical/CDM event table and carries NO counts.
#'
#' The concept-set spec is assembled from the sanitized scalar params (the run
#' path sanitizes every param to a single numeric literal, so a set is given as
#' an anchor \code{concepts} id whose hierarchy is expanded server-side rather
#' than a raw multi-id list): \code{concepts} (anchor concept id), optional
#' \code{exclude} (anchor id removed from the resolution), and
#' \code{include_descendants} (walk \code{concept_ancestor}). The INCLUDED set is
#' resolved via \code{\link{.resolveConceptSet}} -> \code{\link{.vocabExpandConceptSet}}
#' (the descendant walk dsOMOP already implements, exclusions removed); the
#' EXCLUDED set is resolved the same way so each excluded concept is still
#' reported with \code{is_excluded = TRUE} (mirroring OHDSI's flagged expansion).
#' Every resolved concept is LEFT JOINed to \code{concept} for its
#' name/domain/vocabulary/standard_concept. Concepts are translated to names by
#' construction (the join), and NO \code{*_source_value} / \code{*_source_concept_id}
#' column is ever selected (and the gate would strip one anyway).
#'
#' Disclosure: the result carries concept METADATA only (no count column), so it
#' is disclosure-safe by construction; the entry's \code{meta$adapter} is
#' \code{"ohdsi"} so the single \code{\link{.omopAnalysisGate}} routes the
#' count-less frame through \code{\link{.ohdsiPersonGate}}, which passes a table
#' with no count columns through unchanged. The only residual disclosure risk is
#' an UNBOUNDED enumeration (a pathological anchor resolving a huge vocabulary
#' slice), so the resolved-concept count is fail-closed capped at
#' \code{nfilter.levels.max} BEFORE the (already small, count-less) frame is
#' assembled.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (unused; resolution is vocabulary-only, not scoped).
#' @param params Sanitized params: \code{concepts} (anchor concept id),
#'   \code{exclude} (anchor id to exclude), \code{include_descendants} (bool).
#' @return Data frame (concept_id, concept_name, domain_id, vocabulary_id,
#'   standard_concept, is_excluded) or an empty frame when no anchor is given.
#' @keywords internal
.ohdsiCohortDxResolvedConceptsFn <- function(handle, ctx, params) {
  anchor <- suppressWarnings(as.integer(params$concepts))
  anchor <- anchor[!is.na(anchor)]
  if (length(anchor) == 0) return(data.frame())
  incl_desc <- identical(as.character(params$include_descendants %||% "1"), "1")
  excl_anchor <- suppressWarnings(as.integer(params$exclude))
  excl_anchor <- excl_anchor[!is.na(excl_anchor)]

  # Excluded set: the exclude anchor expanded the SAME way (so descendants of an
  # excluded concept are also flagged), resolved independently of the include set
  # so it can be reported with is_excluded = TRUE.
  excluded_ids <- integer(0)
  if (length(excl_anchor) > 0) {
    excluded_ids <- .resolveConceptSet(
      handle, list(concepts = excl_anchor, include_descendants = incl_desc))
  }
  # Included (resolved) set: anchor expanded by descendants, exclusions removed.
  included_ids <- .resolveConceptSet(
    handle, list(concepts = anchor, include_descendants = incl_desc,
                 exclude = excluded_ids))

  all_ids <- unique(c(included_ids, excluded_ids))
  if (length(all_ids) == 0) return(data.frame())

  # Fail-closed cap on the enumeration BEFORE assembling metadata, so a
  # pathological anchor cannot list an unbounded vocabulary slice. Only the
  # max-levels bound of .assertSafeLevels applies here (the companion per-person
  # density bound is for categorical re-identification and is meaningless for
  # count-less vocabulary metadata, which has no person basis), so apply the
  # nfilter.levels.max cap directly.
  levels_max <- .omopDisclosureSettings()$nfilter_levels_max
  if (length(all_ids) > levels_max) {
    stop("Disclosive: resolved concept set (", length(all_ids),
         " concepts) exceeds nfilter.levels.max (", levels_max, ").",
         call. = FALSE)
  }

  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  idlist <- paste(all_ids, collapse = ", ")
  sql <- .sql_translate(paste0(
    "SELECT c.concept_id, c.concept_name, c.domain_id, c.vocabulary_id, ",
    "c.standard_concept ",
    "FROM ", concept, " c WHERE c.concept_id IN (", idlist, ")",
    " ORDER BY c.concept_id"),
    handle$target_dialect)
  df <- .executeQuery(handle, sql)
  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())

  # Carry through any resolved id absent from the concept table (so the
  # resolution is complete even on a partial vocabulary load).
  missing_ids <- setdiff(all_ids, suppressWarnings(as.integer(df$concept_id)))
  if (length(missing_ids) > 0) {
    df <- rbind(df, data.frame(
      concept_id = missing_ids, concept_name = NA_character_,
      domain_id = NA_character_, vocabulary_id = NA_character_,
      standard_concept = NA_character_, stringsAsFactors = FALSE))
  }
  df$is_excluded <- as.integer(df$concept_id) %in% excluded_ids
  df <- df[order(as.integer(df$concept_id)), , drop = FALSE]
  rownames(df) <- NULL
  df
}

# --- orphan_concept -> orphan_concepts ----------------------------------------

#' Vocabulary candidate-orphan walk for a concept-set seed (non-disclosive)
#'
#' OHDSI's CohortDiagnostics \code{orphan_concept} surfaces concepts that lie in
#' the vocabulary NEIGHBOURHOOD of a concept-set's SEED concepts (their
#' descendants and direct \code{Maps to}/\code{Is a} relations) but were NOT
#' pulled into the resolved set — candidate GAPS in the concept-set definition.
#' This builds that candidate id list purely from the vocabulary, reusing the
#' existing helpers (\code{\link{.vocabGetDescendants}} over
#' \code{concept_ancestor}, \code{\link{.vocabGetRelationships}} over
#' \code{concept_relationship}) and SUBTRACTING the resolved (included) set. The
#' walk touches only vocabulary reference tables, so it is non-disclosive; the
#' per-candidate CDM presence counts (the only patient-derived quantities) are
#' computed and gated by the caller \code{\link{.ohdsiCohortDxOrphanConceptsFn}}.
#' Returns a frame keyed by candidate \code{concept_id} with the vocabulary
#' \code{concept_name} (no CDM counts), capped to \code{nfilter.levels.max}
#' candidates so the list itself can never become an exhaustive vocabulary
#' enumeration.
#'
#' @param handle CDM handle.
#' @param seed_ids Integer vector; the concept-set SEED concept ids.
#' @param resolved_ids Integer vector; the resolved (included) set to subtract.
#' @param relationships Character vector; concept_relationship ids to walk
#'   (default \code{c("Maps to", "Is a")}).
#' @return Data frame (concept_id, concept_name) of candidate orphans (possibly
#'   empty), capped to \code{nfilter.levels.max} rows.
#' @keywords internal
.ohdsiCohortDxOrphanCandidates <- function(handle, seed_ids, resolved_ids,
                                           relationships = c("Maps to", "Is a")) {
  seed_ids <- unique(as.integer(seed_ids))
  seed_ids <- seed_ids[!is.na(seed_ids)]
  empty <- data.frame(concept_id = integer(0), concept_name = character(0),
                      stringsAsFactors = FALSE)
  if (length(seed_ids) == 0) return(empty)

  # Hierarchical candidates: descendants of the seeds (excluding the seeds
  # themselves) via concept_ancestor.
  desc <- tryCatch(.vocabGetDescendants(handle, seed_ids, include_self = FALSE),
                   error = function(e) NULL)
  cand_ids   <- if (!is.null(desc) && nrow(desc) > 0) as.integer(desc$concept_id) else integer(0)
  cand_names <- if (!is.null(desc) && nrow(desc) > 0) as.character(desc$concept_name) else character(0)

  # Relational candidates: concepts reached from the seeds by the requested
  # relationships (Maps to / Is a) via concept_relationship.
  for (rel in relationships) {
    rl <- tryCatch(.vocabGetRelationships(handle, seed_ids, relationship_id = rel),
                   error = function(e) NULL)
    if (!is.null(rl) && nrow(rl) > 0) {
      cand_ids   <- c(cand_ids,   as.integer(rl$related_concept_id))
      cand_names <- c(cand_names, as.character(rl$related_concept_name))
    }
  }
  if (length(cand_ids) == 0) return(empty)

  # First-seen name per id, then subtract the resolved set + the seeds (an orphan
  # is by definition NOT already covered by the resolved concept set).
  keep_first <- !duplicated(cand_ids)
  cand_ids   <- cand_ids[keep_first]
  cand_names <- cand_names[keep_first]
  drop       <- unique(c(as.integer(resolved_ids), seed_ids))
  keep       <- !is.na(cand_ids) & !(cand_ids %in% drop)
  cand_ids   <- cand_ids[keep]
  cand_names <- cand_names[keep]
  if (length(cand_ids) == 0) return(empty)

  # Cap the candidate LIST length at nfilter.levels.max so a runaway vocabulary
  # walk can never return an exhaustive concept enumeration (the gate separately
  # bands/suppresses the per-candidate counts).
  levels_max <- as.integer(.omopDisclosureSettings()$nfilter_levels_max %||% 40L)
  if (length(cand_ids) > levels_max) {
    cand_ids   <- cand_ids[seq_len(levels_max)]
    cand_names <- cand_names[seq_len(levels_max)]
  }
  data.frame(concept_id = cand_ids, concept_name = cand_names,
             stringsAsFactors = FALSE)
}

#' Per-candidate-orphan CDM presence counts (orphan_concepts fn)
#'
#' NEW canonical \code{dsomop:cohortdx.orphan_concepts}, replacing the precomputed
#' \code{orphan_concept} read with a live R-in-session computation. It (1)
#' resolves the concept-set SEED into its included set the SAME way the cohort/
#' plan paths and the sibling \code{resolved_concepts} fn do
#' (\code{\link{.resolveConceptSet}} with \code{include_descendants}/
#' \code{include_mapped}/\code{exclude}); (2) walks the vocabulary neighbourhood
#' of the SEEDS for candidate orphans and subtracts that resolved set
#' (\code{\link{.ohdsiCohortDxOrphanCandidates}}); and (3) for each surviving
#' candidate counts its presence in the CDM domain table
#' (\code{\link{.omopCovariateSource}}): \code{n_persons} = COUNT(DISTINCT
#' person_id), \code{n_records} = COUNT(*), over the SCOPED cohort when scoped
#' (INNER JOIN on subject_id) or cohort-wide otherwise. A data controller thus
#' sees which UNCOVERED concepts actually CARRY DATA (true concept-set gaps), not
#' just vocabulary metadata.
#'
#' Disclosure: the vocabulary walk is non-disclosive; the ONLY patient-derived
#' quantities are the per-candidate CDM counts. \code{unit="record"} with
#' \code{count_cols=c("n_records","n_persons")} and
#' \code{person_id_col="n_persons"}: the single \code{\link{.omopAnalysisGate}}'s
#' GENERIC record branch (entry adapter \code{ohdsi_live}) gates \code{n_records}
#' on the distinct-person companion \code{n_persons}, bands BOTH, and DROPS any
#' candidate resting on fewer than \code{nfilter.subset}/\code{nfilter.tab}
#' persons. Self-gated over the scoped CDM domain BEFORE materialising; candidates
#' with no CDM presence are naturally absent (an orphan that carries no data is
#' not actionable). Concepts are translated to names; NO \code{*_source_value} /
#' \code{*_source_concept_id} is ever selected (and the gate would strip one).
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}; optional — cohort-wide
#'   when un-scoped).
#' @param params Sanitized params: \code{seed_concept_id} (concept-set seed
#'   anchor; required for a non-empty result), \code{domain_code},
#'   \code{include_descendants}, \code{include_mapped}, \code{exclude_concept_id},
#'   \code{top_n}.
#' @return Data frame (concept_id, concept_name, n_persons, n_records) or empty.
#' @keywords internal
.ohdsiCohortDxOrphanConceptsFn <- function(handle, ctx, params) {
  seed <- suppressWarnings(as.integer(params$seed_concept_id))
  seed <- seed[!is.na(seed)]
  if (length(seed) == 0) return(data.frame())
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  src      <- .omopCovariateSource(handle, params$domain_code %||% "0")
  top_n    <- max(as.integer(params$top_n %||% "50"), 1L)
  inc_desc <- identical(as.character(params$include_descendants %||% "1"), "1")
  inc_map  <- identical(as.character(params$include_mapped %||% "0"), "1")
  excl     <- suppressWarnings(as.integer(params$exclude_concept_id))
  excl     <- excl[!is.na(excl)]

  # (1) Resolve the SEED into its included (resolved) set, exactly as the cohort/
  # plan paths (and resolved_concepts) resolve a concept_set spec.
  resolved <- .resolveConceptSet(handle, list(
    concepts = seed, include_descendants = inc_desc,
    include_mapped = inc_map, exclude = excl))

  # (2) Vocabulary candidate-orphan walk minus the resolved set (non-disclosive).
  cand <- .ohdsiCohortDxOrphanCandidates(handle, seed, resolved)
  if (!is.data.frame(cand) || nrow(cand) == 0) return(data.frame())

  # (3) Per-candidate CDM presence over the scoped cohort (or cohort-wide). Self-
  # gate the scoped domain population BEFORE materialising any counts.
  .omopDiagAssertPersons(handle, ctx, src$table, "e", src$person_col)
  join_clause <- ""
  if (!is.null(ctx$scoped_cohort)) {
    cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
    join_clause <- paste0(" INNER JOIN ", cohort,
                          " c ON c.subject_id = e.", src$person_col)
  }
  idlist <- paste(cand$concept_id, collapse = ", ")
  sql <- .sql_translate(paste0(
    "SELECT e.", src$concept_col, " AS concept_id, cc.concept_name, ",
    "COUNT(DISTINCT e.", src$person_col, ") AS n_persons, ",
    "COUNT(*) AS n_records ",
    "FROM ", src$table, " e", join_clause,
    " LEFT JOIN ", concept, " cc ON cc.concept_id = e.", src$concept_col,
    " WHERE e.", src$concept_col, " IN (", idlist, ")",
    " GROUP BY e.", src$concept_col, ", cc.concept_name",
    " ORDER BY n_persons DESC"),
    handle$target_dialect)

  df <- .executeQuery(handle, sql)
  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())
  if (nrow(df) > top_n) df <- df[seq_len(top_n), , drop = FALSE]
  rownames(df) <- NULL
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

#' Build the resolved-concepts entry under a given id
#'
#' The concept-set resolution is a VOCABULARY computation, not a cohort-relative
#' one: it carries no person key and no count, so (unlike the other entries in
#' this pack) it is honestly NOT cohort/table-scopable
#' (\code{accepts_cohort=FALSE}, \code{max_tables=0L}) — matching the precomputed
#' \code{resolved_concepts} entry it overlays. \code{adapter="ohdsi"} +
#' \code{tool_id} route its count-less metadata frame through the gate's
#' \code{\link{.ohdsiPersonGate}} pass-through (a table with no count columns has
#' nothing to gate). The disclosure unit is \code{"record"} with NO count
#' columns; the only enumeration risk (an unbounded vocabulary slice) is capped
#' in-fn at \code{nfilter.levels.max}.
#' @keywords internal
.ohdsiCohortDxResolvedConceptsEntry <- function(name, meta) {
  .omopAnalysisEntry(
    name        = name,
    description = paste0("Live concept-set expansion: the resolved standard ",
                         "concepts (id, name, domain, vocabulary, standard, ",
                         "is_excluded) of a concept-set definition."),
    domain      = "general",
    params      = list(
      list(name = "concepts", type = "concept_id", required = FALSE,
           default = NULL,
           description = paste0("Anchor concept id of the concept set to ",
                                "resolve (its hierarchy is expanded ",
                                "server-side).")),
      list(name = "exclude", type = "concept_id", required = FALSE,
           default = NULL,
           description = paste0("Concept id to exclude from the resolution ",
                                "(still reported with is_excluded=TRUE).")),
      list(name = "include_descendants", type = "bool", required = FALSE,
           default = "1",
           description = "Expand each anchor to its concept_ancestor descendants.")
    ),
    compute     = list(kind = "r", sql = NULL,
                       fn = .ohdsiCohortDxResolvedConceptsFn),
    dependencies = list(tables = c("concept", "concept_ancestor"),
                        packages = character(0)),
    # Concept metadata only: no count column. unit="record" + adapter="ohdsi"
    # routes the count-less frame through .ohdsiPersonGate's pass-through.
    disclosure  = .omopAnalysisDisclosure(
      unit = "record", count_cols = character(0)),
    scope = .omopAnalysisScope(accepts_cohort = FALSE, accepts_tables = FALSE,
                               max_tables = 0L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = c(list(adapter = "ohdsi", tool_id = "cohort_diagnostics"), meta)
  )
}

#' Build the orphan-concepts entry under a given id
#'
#' Unlike \code{resolved_concepts} (pure vocabulary metadata), orphan_concepts
#' emits a genuine single-site per-patient aggregate — the distinct-person /
#' record CDM presence of each candidate orphan — so it is a LIVE
#' \code{adapter="ohdsi_live"} record-unit entry routed through the gate's GENERIC
#' record branch (NOT \code{.ohdsiPersonGate}): \code{count_cols=c("n_records",
#' "n_persons")} with \code{person_id_col="n_persons"}. It is cohort-OPTIONAL
#' (\code{accepts_cohort=TRUE}, \code{requires_cohort=FALSE}): the candidate walk
#' is vocabulary-only, but the presence counts are computed over the scoped
#' cohort when one is supplied, cohort-wide otherwise.
#' @keywords internal
.ohdsiCohortDxOrphanConceptsEntry <- function(name, adapter, meta) {
  .omopAnalysisEntry(
    name        = name,
    description = paste0("Candidate concept-set orphans: concepts in the ",
                         "vocabulary neighbourhood of the seed concepts that are ",
                         "NOT in the resolved set yet carry data in the CDM ",
                         "(distinct persons + records present)."),
    domain      = "general",
    params      = list(
      list(name = "seed_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = paste0("Concept-set SEED (anchor) concept id whose ",
                                "vocabulary neighbourhood is searched for ",
                                "candidate orphans.")),
      list(name = "domain_code", type = "enum", required = FALSE, default = "0",
           choices = c("0", "1", "2", "3", "4"),
           description = paste0("CDM domain to count candidate presence in ",
                                "(0 condition,1 drug,2 procedure,3 measurement,",
                                "4 observation).")),
      list(name = "include_descendants", type = "bool", required = FALSE,
           default = "1",
           description = "Expand the seed to its descendants for the resolved set."),
      list(name = "include_mapped", type = "bool", required = FALSE,
           default = "0",
           description = "Add mapped source concepts to the resolved set."),
      list(name = "exclude_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Concept id removed from the resolved set."),
      list(name = "top_n", type = "int", required = FALSE, default = "50")
    ),
    compute     = list(kind = "r", sql = NULL,
                       fn = .ohdsiCohortDxOrphanConceptsFn),
    dependencies = list(tables = c("concept", "concept_ancestor",
                                   "condition_occurrence"),
                        packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(
      unit          = "record",
      count_cols    = c("n_records", "n_persons"),
      person_id_col = "n_persons"),
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
#' \code{dsomop:cohortdx.time_series}, \code{dsomop:cohortdx.included_source_concepts},
#' \code{dsomop:cohortdx.resolved_concepts})
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
           table_name = "included_source_concept")),

    # resolved_concepts -> canonical resolved_concepts (+ registry alias). Both
    # COMPUTE the concept-set expansion LIVE from the vocabulary (no precomputed
    # cd_resolved_concepts read). They keep adapter="ohdsi" (count-less metadata
    # routed through .ohdsiPersonGate's pass-through) and are non-scopable
    # (vocabulary, not cohort-relative), exactly as the precomputed entry was.
    .ohdsiCohortDxResolvedConceptsEntry(
      "dsomop:cohortdx.resolved_concepts", list(table_name = "resolved_concepts")),
    .ohdsiCohortDxResolvedConceptsEntry(
      "dsomop:ohdsi.cohort_diagnostics.resolved_concepts",
      list(table_name = "resolved_concepts")),

    # orphan_concept -> canonical orphan_concepts (+ registry alias). Unlike
    # resolved_concepts these COMPUTE a genuine single-site per-patient aggregate
    # (each candidate orphan's distinct-person + record CDM presence), so they are
    # adapter="ohdsi_live" record-unit entries gated through the GENERIC record
    # branch (n_records on the n_persons companion), overlaying the read-
    # precomputed orphan_concept entry.
    .ohdsiCohortDxOrphanConceptsEntry(
      "dsomop:cohortdx.orphan_concepts", "ohdsi_live", list()),
    .ohdsiCohortDxOrphanConceptsEntry(
      "dsomop:ohdsi.cohort_diagnostics.orphan_concept", "ohdsi_live",
      list(tool_id = "cohort_diagnostics", table_name = "orphan_concept"))
  )
  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}
