# Module: OHDSI Characterization catalog entries (native live compute)
#
# Group OHDSI-B2 (characterization-new). The Characterization dechallenge/
# rechallenge analysis, ported from the precomputed OHDSI-results adapter
# (.ohdsiGetResults reading c_dechallenge_rechallenge) to a native, live-
# computing catalog entry.
#
# The OHDSI Characterization `DechallengeRechallenge` module summarises, for a
# (drug, outcome) pair, how often the outcome occurs while exposed, resolves
# when the drug is stopped (DECHALLENGE), and recurs when the drug is restarted
# (RECHALLENGE). We re-implement it over OUR connection: the scoped cohort IS
# the drug-exposure population (each cohort row is an exposure era with
# cohort_start_date/cohort_end_date) and the outcome is a condition (or other
# domain) concept resolved via .omopOutcomeSource (descendants expanded). The fn
# computes four PERSON counts as COUNT(DISTINCT person_id) per step:
#   * num_dechallenge_attempt  : persons with the outcome ON treatment whose
#                                exposure then STOPS within dechallenge_stop_interval
#                                days of the outcome;
#   * num_dechallenge_success  : of those, persons with NO outcome recurrence in
#                                the dechallenge_evaluation_window after stopping;
#   * num_rechallenge_attempt  : of the dechallenge attempts, persons who are
#                                RE-exposed (a later exposure era of the same cohort);
#   * num_rechallenge_success  : of those, persons with NO outcome recurrence in
#                                the evaluation window after re-exposure.
# The per-case (rechallenge_fail_case_series) subject-level drilldown of the
# OHDSI table is deliberately EXCLUDED (it is person-identifying, never gate-safe).
#
# Disclosure: unit="person"; count_cols name all four num_* person counts, so the
# SINGLE .omopAnalysisGate small-cell-suppresses + bands each (a wide one-row
# summary is dropped fail-closed if ANY step rests on too few persons). The two
# derived percentages (pct_dechallenge_success, pct_rechallenge_success) are the
# ONE sanctioned ratio exception: each is routed through
# .omopAnalysisReconcileRatio(scale=100) INSIDE the fn BEFORE returning, so it is
# recomputed from the banded numerator+denominator and NA'd whenever either side
# is suppressed (a surviving percentage never re-derives a sub-threshold count).
# The fn self-gates the exposure population via .omopDiagAssertPersons before
# materialising, runs cohort-wide when un-scoped (whole-DB / database-
# characterization semantics: requires_cohort=FALSE), and never selects any
# *_source_value / free-text column. Nothing here reads
# c_dechallenge_rechallenge or any other precomputed results table.
#
# Entry ids are stable: the canonical live entry is dsomop:char.dechallenge_rechallenge
# (added to .omopAnalysisDiagnosticEntries), and the legacy OHDSI id
# dsomop:ohdsi.characterization.c_dechallenge_rechallenge is preserved as an
# alias entry (this registrar) that runs the SAME live compute, so existing
# references to either id keep resolving.

#' Parameter specs shared by the dechallenge/rechallenge entries
#'
#' The (drug, outcome) pair is the scoped cohort (exposure) plus an outcome
#' concept; the two intervals bound the stop/evaluation windows (literal day
#' counts spliced into DATEADD, so they must be integers).
#'
#' @return List of parameter specs.
#' @keywords internal
.omopCharDechallengeParams <- function() {
  list(
    list(name = "outcome_cohort", type = "concept_id", required = FALSE,
         default = NULL,
         description = paste0("Outcome concept id (descendants expanded) whose ",
                              "challenge against the cohort's exposure is summarised.")),
    list(name = "domain_code", type = "enum", required = FALSE, default = "0",
         choices = c("0", "1", "2", "3", "4"),
         description = paste0("Outcome domain (0 condition,1 drug,2 procedure,",
                              "3 measurement,4 observation).")),
    list(name = "dechallenge_stop_interval", type = "int", required = FALSE,
         default = "30",
         description = paste0("Days after the on-treatment outcome within which ",
                              "the exposure must stop to count as a dechallenge.")),
    list(name = "dechallenge_evaluation_window", type = "int", required = FALSE,
         default = "30",
         description = paste0("Days after stopping (or after re-exposure) over ",
                              "which a non-recurrence counts as a success."))
  )
}

#' Live compute fn for the dechallenge/rechallenge summary
#'
#' Builds ONE SELECT over the scoped cohort eras (alias \code{c}) that derives,
#' per exposure era, the dechallenge/rechallenge step flags via correlated
#' EXISTS / NOT EXISTS over the outcome events and the cohort's later eras, then
#' aggregates each step to a distinct-person count
#' (\code{COUNT(DISTINCT CASE WHEN <flag> THEN c.subject_id END)}). Returns a
#' single AGGREGATE-ONLY row with the four num_* counts and two reconciled
#' percentages; un-scoped runs compute cohort-wide; the fn never gates the counts
#' (the gate does), and routes the two percentages through
#' \code{.omopAnalysisReconcileRatio} as the sanctioned derived-ratio exception.
#'
#' @return A \code{function(handle, ctx, params)} returning a data.frame.
#' @keywords internal
.omopCharDechallengeFn <- function() {
  function(handle, ctx, params) {
    outcome_id  <- params$outcome_cohort
    domain_code <- params$domain_code %||% "0"
    out_src <- .omopOutcomeSource(handle, outcome_id, domain_code)
    # No outcome supplied -> there is no (drug, outcome) pair to challenge;
    # return a gate-safe empty frame rather than a meaningless all-cohort count.
    if (is.null(out_src)) return(data.frame())

    stop_interval <- max(as.integer(params$dechallenge_stop_interval %||% "30"), 0L)
    eval_window   <- max(as.integer(params$dechallenge_evaluation_window %||% "30"), 0L)

    # The scoped cohort IS the exposure population: its rows are the exposure
    # eras (subject_id + cohort_start_date [+ cohort_end_date]) the metric is
    # defined over. The entry is requires_cohort=TRUE, so the run path already
    # rejected an un-scoped call; this guard is belt-and-suspenders (returns a
    # gate-safe empty frame should the fn ever be reached without a scope).
    if (is.null(ctx$scoped_cohort)) return(data.frame())
    cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
    end_col <- .omopCohortEndDateCol(handle, cohort)

    # Self-gate the exposure population BEFORE materialising any rows.
    .omopDiagAssertPersons(handle, ctx, cohort, "c", "subject_id")

    out_tbl  <- out_src$table
    out_pcol <- out_src$person_col
    out_ccol <- out_src$concept_col
    out_dcol <- out_src$date_col
    ids      <- out_src$id_list
    cend     <- paste0("c.", end_col)

    # Correlated outcome-event predicates relative to one exposure era `c`.
    # on-treatment outcome: an outcome event inside the exposure era.
    on_tx <- paste0(
      "EXISTS (SELECT 1 FROM ", out_tbl, " o WHERE o.", out_pcol,
      " = c.subject_id AND o.", out_ccol, " IN (", ids, ") AND o.", out_dcol,
      " >= c.cohort_start_date AND o.", out_dcol, " <= ", cend, ")")

    # dechallenge: exposure STOPS within stop_interval days AFTER the on-treatment
    # outcome (the drug was withdrawn in response to the event).
    stops <- paste0(
      "EXISTS (SELECT 1 FROM ", out_tbl, " o WHERE o.", out_pcol,
      " = c.subject_id AND o.", out_ccol, " IN (", ids, ") AND o.", out_dcol,
      " >= c.cohort_start_date AND o.", out_dcol, " <= ", cend,
      " AND ", cend, " <= DATEADD(day, ", stop_interval, ", o.", out_dcol, "))")
    attempt <- paste0("(", on_tx, " AND ", stops, ")")

    # dechallenge success: NO outcome recurrence in the evaluation window after
    # stopping (cend, cend + eval_window].
    no_recur_stop <- paste0(
      "NOT EXISTS (SELECT 1 FROM ", out_tbl, " o2 WHERE o2.", out_pcol,
      " = c.subject_id AND o2.", out_ccol, " IN (", ids, ") AND o2.", out_dcol,
      " > ", cend, " AND o2.", out_dcol, " <= DATEADD(day, ", eval_window,
      ", ", cend, "))")
    de_success <- paste0("(", attempt, " AND ", no_recur_stop, ")")

    # rechallenge attempt: a dechallenge era followed by a LATER exposure era of
    # the SAME cohort (re-exposure).
    re_start <- paste0(
      "(SELECT MIN(c2.cohort_start_date) FROM ", cohort, " c2 WHERE c2.subject_id",
      " = c.subject_id AND c2.cohort_start_date > ", cend, ")")
    re_exposed <- paste0(
      "EXISTS (SELECT 1 FROM ", cohort, " c2 WHERE c2.subject_id = c.subject_id",
      " AND c2.cohort_start_date > ", cend, ")")
    re_attempt <- paste0("(", attempt, " AND ", re_exposed, ")")

    # rechallenge success: NO outcome recurrence in the evaluation window after
    # the (first) re-exposure start.
    no_recur_re <- paste0(
      "NOT EXISTS (SELECT 1 FROM ", out_tbl, " o3 WHERE o3.", out_pcol,
      " = c.subject_id AND o3.", out_ccol, " IN (", ids, ") AND o3.", out_dcol,
      " > ", re_start, " AND o3.", out_dcol, " <= DATEADD(day, ", eval_window,
      ", ", re_start, "))")
    re_success <- paste0("(", re_attempt, " AND ", no_recur_re, ")")

    cnt <- function(flag, alias) {
      paste0("COUNT(DISTINCT CASE WHEN ", flag, " THEN c.subject_id END) AS ",
             alias)
    }
    sql <- .sql_translate(paste0(
      "SELECT ",
      cnt(attempt, "num_dechallenge_attempt"), ", ",
      cnt(de_success, "num_dechallenge_success"), ", ",
      cnt(re_attempt, "num_rechallenge_attempt"), ", ",
      cnt(re_success, "num_rechallenge_success"),
      " FROM ", cohort, " c"),
      handle$target_dialect)

    df <- .executeQuery(handle, sql)
    if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())

    # Carry the (drug, outcome) label for the row. The cohort is the drug
    # exposure; label the pair with the outcome concept id. No *_source_value /
    # free-text is ever emitted.
    df$outcome_concept_id <- suppressWarnings(as.integer(outcome_id))

    # Derived percentages: the ONE sanctioned ratio exception. Reconcile each
    # against its banded numerator+denominator (NA when either side is
    # suppressed) BEFORE returning to the gate. Seed the columns so the helper
    # has a target to overwrite.
    df$pct_dechallenge_success <- NA_real_
    df$pct_rechallenge_success <- NA_real_
    df <- .omopAnalysisReconcileRatio(
      df, numerator_col = "num_dechallenge_success",
      denominator_col = "num_dechallenge_attempt",
      ratio_col = "pct_dechallenge_success", scale = 100)
    df <- .omopAnalysisReconcileRatio(
      df, numerator_col = "num_rechallenge_success",
      denominator_col = "num_rechallenge_attempt",
      ratio_col = "pct_rechallenge_success", scale = 100)

    # Concept translation is DEFAULT ON: the run path / gate never translates
    # *_concept_id columns, so (matching every other native diagnostic in this
    # catalog) the fn resolves outcome_concept_id to its concept name itself
    # before returning, rather than releasing a raw concept id.
    .vocabTranslateColumns(handle, df)
  }
}

#' Disclosure spec shared by the dechallenge/rechallenge entries
#' @keywords internal
.omopCharDechallengeDisclosure <- function() {
  .omopAnalysisDisclosure(
    unit = "person",
    count_cols = c("num_dechallenge_attempt", "num_dechallenge_success",
                   "num_rechallenge_attempt", "num_rechallenge_success"))
}

#' Scope spec shared by the dechallenge/rechallenge entries
#'
#' Live cohort + single-table scope (the exposure cohort plus the outcome param).
#' \code{requires_cohort=TRUE}: the scoped cohort IS the exposure population
#' (its rows are the exposure eras the metric is defined over), so the analysis
#' is meaningless un-scoped — the run path turns an un-scoped run into a CLEAR
#' error instead of the fn's silently empty (gate-safe) frame. This matches how
#' the native diagnostic family is marked (\code{.omopAnalysisDiagnosticEntries}
#' forces the same flag on the canonical entry); the OHDSI-id alias sets it here
#' so both ids behave identically.
#' @keywords internal
.omopCharDechallengeScope <- function() {
  .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                     max_tables = 1L, requires_cohort = TRUE)
}

#' Canonical native dechallenge/rechallenge entry
#'
#' \code{dsomop:char.dechallenge_rechallenge} (Characterization, re-implemented):
#' the live-computing entry added to \code{\link{.omopAnalysisDiagnosticEntries}}.
#' Shares its compute fn / params / disclosure / scope with the legacy OHDSI-id
#' alias (\code{\link{.ohdsiPackCharacterizationEntries}}).
#'
#' @return A single \code{omop_analysis_entry}.
#' @keywords internal
.omopCharDechallengeRechallenge <- function() {
  .omopAnalysisEntry(
    name        = "dsomop:char.dechallenge_rechallenge",
    description = paste0("Dechallenge/rechallenge person counts (attempt/success ",
                         "for drug withdrawal and re-exposure) for the scoped ",
                         "exposure cohort against an outcome concept, with ",
                         "reconciled success percentages."),
    domain      = "drug",
    params      = .omopCharDechallengeParams(),
    compute     = list(kind = "r", sql = NULL, fn = .omopCharDechallengeFn(),
                       plot = NULL),
    dependencies = list(
      tables   = c("condition_occurrence", "concept"),
      packages = character(0)),
    disclosure = .omopCharDechallengeDisclosure(),
    scope      = .omopCharDechallengeScope(),
    mode       = "aggregate",
    meta       = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

#' Emit the OHDSI Characterization catalog entries (native live compute)
#'
#' The group registrar for \code{ohdsi_pack_characterization.R}. It preserves the
#' legacy OHDSI id \code{dsomop:ohdsi.characterization.c_dechallenge_rechallenge}
#' as an ALIAS entry running the SAME live compute as the canonical
#' \code{dsomop:char.dechallenge_rechallenge} (so references to the old id keep
#' resolving), while the read-precomputed path (\code{.ohdsiGetResults}) is no
#' longer used for this table. Returned as a named list keyed by entry id, exactly
#' as \code{\link{.omopAnalysisOhdsiEntries}} expects to overlay. \code{handle} is
#' taken for signature parity ONLY and is never queried at build time (catalog
#' build is cached on \code{handle$analysis_catalog}; all DB I/O happens later
#' inside the entry's \code{compute$fn}).
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by entry id.
#' @keywords internal
.ohdsiPackCharacterizationEntries <- function(handle) {
  name <- "dsomop:ohdsi.characterization.c_dechallenge_rechallenge"
  entry <- .omopAnalysisEntry(
    name        = name,
    description = paste0("Characterization dechallenge/rechallenge (live ",
                         "re-implementation): attempt/success person counts for ",
                         "drug withdrawal and re-exposure over the scoped ",
                         "exposure cohort and an outcome concept."),
    domain      = "drug",
    params      = .omopCharDechallengeParams(),
    compute     = list(kind = "r", sql = NULL, fn = .omopCharDechallengeFn(),
                       plot = NULL),
    dependencies = list(
      tables   = c("condition_occurrence", "concept"),
      packages = character(0)),
    disclosure = .omopCharDechallengeDisclosure(),
    scope      = .omopCharDechallengeScope(),
    mode       = "aggregate",
    meta       = list(adapter = "ohdsi_live", tool_id = "characterization",
                      table_name = "c_dechallenge_rechallenge")
  )
  stats::setNames(list(entry), name)
}
