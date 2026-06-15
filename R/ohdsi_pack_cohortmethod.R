# Module: OHDSI CohortMethod catalog entries (native live compute)
#
# Group OHDSI-B2 (cohort_method, new live SQL/R). The two CohortMethod analyses
# that have an honest descriptive substrate are ported from the precomputed
# OHDSI-results adapter (.ohdsiGetResults reading cm_attrition / cm_result) to
# native, live-computing catalog entries:
#
#   dsomop:ohdsi.cohort_method.cm_attrition  (canonical dsomop:cm.attrition)
#     Inclusion-step attrition: persons + exposures remaining after each ordered
#     inclusion predicate, computed LIVE over the scoped exposure cohort (a single
#     population; max_tables = 1L). Record cells with a distinct-person companion.
#
#   dsomop:ohdsi.cohort_method.cm_result    (canonical dsomop:cm.mdrr)
#     The DESCRIPTIVE substrate of a comparative cohort result — persons,
#     person-time and outcomes per arm — PLUS the Minimum Detectable Relative
#     Risk (MDRR) derived from those (gated, banded) aggregates. The FITTED effect
#     estimate (RR / HR / confidence interval) is intentionally OUT of scope: it
#     requires a regression over patient-level survival data and is not a
#     descriptive aggregate, so this port stops at the substrate + MDRR.
#
#   dsomop:ohdsi.cohort_method.cm_diagnostics_summary (canonical
#     dsomop:cm.diagnostics_summary)
#     The CohortMethod diagnostics summary is a single row of SCALAR study-quality
#     metrics. Each is recomputed LIVE as a DERIVED scalar over the gated single-
#     site aggregates the sibling ports already build (R-in-session: the sibling
#     kernels are called in one fn and their outputs assembled into ONE row):
#       * mdrr            -> .omopCmMdrr(total_outcomes, target_days,
#                            comparator_days) VERBATIM (the cm_result kernel; bands
#                            its inputs and is NA on any suppressed input).
#       * max_sdm         -> max(abs(SMD)) over .omopTwoArmCovariateSmd(target,
#                            comparator) (the covariate-balance kernel; each SMD is
#                            already NA when either arm is suppressed, so the max is
#                            over disclosure-safe SMDs only).
#       * attrition_fraction -> persons after the final inclusion step / persons at
#                            the baseline step, BOTH from .omopCmAttritionSteps over
#                            each arm, reconciled from BANDED counts via
#                            .omopAnalysisReconcileRatio.
#       * ease (shared_balance_pass) -> boolean (max_sdm < 0.1), NA when max_sdm is
#                            NA (i.e. when balance could not be released).
#       * equipoise / ps_auc -> NOT emitted as a raw value here: both need
#                            per-subject propensity scores / a fitted PS model whose
#                            DISCLOSURE-SAFE summary belongs to the propensity-
#                            overlap port (PLR-6, not yet built). Emitted as NA and
#                            documented, NEVER as an individual-score-derived value.
#
# Nothing here reads cm_attrition / cm_result / cm_diagnostics_summary (or any
# other precomputed OHDSI results table). All three entries keep their stable ids
# ("dsomop:ohdsi.<tool>.<table>") so existing references resolve; only the compute
# changes (read-results -> live-compute). The canonical short ids are registered
# alongside in .omopAnalysisDiagnosticEntries.
#
# Disclosure: every count is declared (never pre-gated) so the SINGLE
# .omopAnalysisGate suppresses + bands it. cm_attrition is unit="record" with the
# distinct-person companion as person_id_col and meta$adapter="ohdsi_live", so the
# gate's GENERIC record branch (analysis_catalog.R L3278-3291) gates on the
# companion. cm_result is unit="record" too; its derived MDRR is reconciled from
# BANDED aggregates inside the fn (NA wherever an input is suppressed), exactly as
# .omopAnalysisReconcileRatio does for a rate. cm_diagnostics_summary is unit=
# "record": its single row carries BOTH arms' distinct-person counts
# (n_persons_target / n_persons_comparator) as count_cols and its person_id_col is
# the MIN of the two (n_persons_min), so the generic record branch DROPS the whole
# row unless BOTH arms meet nfilter.subset (the fail-closed-on-min idiom the SCCS
# assumption_checks port uses); every scalar inherits its kernel's NA-on-suppressed
# behaviour (mdrr / max_sdm) or is reconciled from banded counts
# (attrition_fraction via .omopAnalysisReconcileRatio), and no PS-score-derived
# scalar is released. Concepts are translated to names by default; no
# *_source_value / free-text is ever selected. Population is the scoped exposure
# cohort (cm_attrition: a single population, max_tables = 1L) or the scoped pair
# (cm_result / cm_diagnostics_summary: target+comparator, max_tables = 2L);
# requires_cohort stays FALSE so an un-scoped cm_attrition run is whole-DB (a
# single pre-inclusion baseline step), matching the database-characterization
# semantics of the other ports.

# --- Inclusion predicates (ordered, descendant-expanded concept sets) --------

#' Resolve the ordered inclusion predicates for cm_attrition
#'
#' CohortMethod attrition is a FUNNEL: each successive inclusion criterion is
#' applied to the population that survived the previous ones, and the funnel
#' reports how many persons (and exposures) remain after each. We express the same
#' shape honestly over the CDM: each step is a SQL predicate against the scoped
#' cohort, and the steps are applied cumulatively (step k's WHERE is the AND of
#' predicates 1..k). The first step is always the unfiltered baseline ("entered
#' exposure cohort"), so the funnel starts from the full scoped population.
#'
#' Steps beyond the baseline are driven by parameters so the same entry covers the
#' common CohortMethod inclusion rules without patient-level free text:
#' \itemize{
#'   \item \code{require_concept_id} (+ \code{require_domain_code}): keep only
#'     persons with >= 1 record of the (descendant-expanded) concept set in the
#'     chosen domain — e.g. "has the indication".
#'   \item \code{exclude_concept_id} (+ \code{exclude_domain_code}): drop persons
#'     with >= 1 record of the (descendant-expanded) concept set — e.g. "no prior
#'     outcome".
#'   \item \code{min_age} / \code{max_age}: keep persons whose age at the cohort
#'     index date is within the bounds.
#'   \item \code{min_prior_observation}: keep persons with at least N days of
#'     observation before the cohort index date.
#' }
#' Each returned step is \code{list(label, predicate)} where \code{predicate} is a
#' SQL boolean over the cohort alias \code{coh} (NULL for the baseline). Concept
#' predicates are EXISTS sub-selects (descendants expanded server-side), so a
#' single seed concept stands for its whole sub-tree.
#'
#' @param handle CDM handle.
#' @param params Sanitized param literals.
#' @return Ordered list of \code{list(label, predicate)} steps.
#' @keywords internal
.omopCmAttritionSteps <- function(handle, params) {
  steps <- list(list(label = "entered_exposure_cohort", predicate = NULL))

  exists_predicate <- function(concept_id, domain_code, negate) {
    src <- .omopCovariateSource(handle, domain_code)
    ids <- .resolveConceptSet(handle, list(concepts = as.integer(concept_id),
                                           include_descendants = TRUE))
    if (length(ids) == 0) {
      ids <- suppressWarnings(as.integer(concept_id))
      ids <- ids[!is.na(ids)]
    }
    if (length(ids) == 0) return(NULL)
    idlist <- paste(ids, collapse = ", ")
    sub <- paste0("EXISTS (SELECT 1 FROM ", src$table, " e WHERE e.",
                  src$person_col, " = coh.subject_id AND e.", src$concept_col,
                  " IN (", idlist, "))")
    if (isTRUE(negate)) paste0("NOT ", sub) else sub
  }

  if (!is.null(params$require_concept_id)) {
    p <- exists_predicate(params$require_concept_id,
                          params$require_domain_code %||% "0", negate = FALSE)
    if (!is.null(p)) {
      steps[[length(steps) + 1L]] <- list(label = "has_required_concept",
                                          predicate = p)
    }
  }
  if (!is.null(params$exclude_concept_id)) {
    p <- exists_predicate(params$exclude_concept_id,
                          params$exclude_domain_code %||% "0", negate = TRUE)
    if (!is.null(p)) {
      steps[[length(steps) + 1L]] <- list(label = "no_excluded_concept",
                                          predicate = p)
    }
  }

  # Age bounds at the cohort index date (year_of_birth-based, dialect-spliced).
  min_age <- suppressWarnings(as.integer(params$min_age %||% NA))
  max_age <- suppressWarnings(as.integer(params$max_age %||% NA))
  if (!is.na(min_age) || !is.na(max_age)) {
    age_at <- .omopDateDiffDays(handle, "coh.cohort_start_date", "p.birth_dt")
    age_yrs <- paste0("(CAST(", age_at, " / 365 AS INTEGER))")
    bounds <- character(0)
    if (!is.na(min_age)) bounds <- c(bounds, paste0(age_yrs, " >= ", min_age))
    if (!is.na(max_age)) bounds <- c(bounds, paste0(age_yrs, " <= ", max_age))
    # The age predicate needs the person row; expose it as a correlated EXISTS so
    # the step predicate stays a boolean over coh (no extra join to thread).
    person <- .qualifyTable(handle, "person")
    steps[[length(steps) + 1L]] <- list(
      label = "within_age_bounds",
      predicate = paste0("EXISTS (SELECT 1 FROM ", person, " p WHERE ",
                         "p.person_id = coh.subject_id AND ",
                         paste(bounds, collapse = " AND "), ")"))
  }

  min_prior <- suppressWarnings(as.integer(params$min_prior_observation %||% NA))
  if (!is.na(min_prior) && min_prior > 0) {
    obs <- .qualifyTable(handle, "observation_period")
    prior_days <- .omopDateDiffDays(handle, "coh.cohort_start_date",
                                    "op.observation_period_start_date")
    steps[[length(steps) + 1L]] <- list(
      label = "has_prior_observation",
      predicate = paste0("EXISTS (SELECT 1 FROM ", obs, " op WHERE ",
                         "op.person_id = coh.subject_id AND ",
                         "op.observation_period_start_date <= coh.cohort_start_date",
                         " AND op.observation_period_end_date >= ",
                         "coh.cohort_start_date AND ", prior_days, " >= ",
                         min_prior, ")"))
  }
  steps
}

#' Splice the dialect-specific birth-date expression into attrition SQL
#'
#' The age-bound predicate references \code{p.birth_dt}; like the diagnostics, we
#' build the year-01-01 birth date per dialect and splice it post-translate with a
#' fixed gsub (author SQL, kept out of \code{@param} substitution).
#' @keywords internal
.omopCmSpliceBirthDate <- function(handle, sql) {
  birth_dt <- if (identical(handle$target_dialect %||% "", "sqlite")) {
    "(CAST(p.year_of_birth AS VARCHAR) || '-01-01')"
  } else {
    "CAST((CAST(p.year_of_birth AS VARCHAR) || '-01-01') AS DATE)"
  }
  gsub("p.birth_dt", birth_dt, sql, fixed = TRUE)
}

# --- Entry 1: cm_attrition (canonical cm.attrition) --------------------------

#' Build the cm_attrition live compute fn
#'
#' For each ordered inclusion step (\code{\link{.omopCmAttritionSteps}}) over the
#' scoped exposure cohort, count the persons remaining
#' (\code{COUNT(DISTINCT subject_id)}) and the exposures remaining
#' (\code{COUNT(*)}) after applying predicates 1..k cumulatively. The cohort is a
#' single population (\code{max_tables = 1L}; the run path folds any multi-source
#' scope into ONE cohort), un-scoped falling back to the whole CDM as a baseline.
#' The population is self-gated BEFORE materialising. The frame is returned
#' UN-GATED (declared, not pre-gated): the single \code{.omopAnalysisGate}
#' suppresses persons (the companion) then suppresses + bands persons and
#' exposures.
#'
#' @return A \code{function(handle, ctx, params)} returning an aggregate frame.
#' @keywords internal
.omopCmAttritionFn <- function() {
  function(handle, ctx, params) {
    # single scope yields one arm; un-scoped yields a single cohort-wide arm
    # (the baseline step only — whole-DB semantics).
    # Single-population funnel: the scoped cohort IS the exposure population
    # (max_tables = 1L; the run path folds any multi-source scope into ONE cohort).
    # Un-scoped -> the whole CDM as the pre-inclusion population (canonical cohort
    # table), preserving the whole-DB baseline semantics of the other ports.
    cohort <- if (!is.null(ctx$scoped_cohort)) {
      .validateIdentifier(ctx$scoped_cohort, "cohort")
    } else {
      .qualifyTable(handle, "cohort")
    }

    # Self-gate the population before materialising any step.
    .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
      "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort, " coh"),
      handle$target_dialect))

    steps <- .omopCmAttritionSteps(handle, params)

    rows <- list()
    cumulative <- character(0)
    for (i in seq_along(steps)) {
      st <- steps[[i]]
      if (!is.null(st$predicate)) cumulative <- c(cumulative, st$predicate)
      where <- if (length(cumulative) > 0) {
        paste0(" WHERE ", paste(cumulative, collapse = " AND "))
      } else ""
      sql <- .omopCmSpliceBirthDate(handle, .sql_translate(paste0(
        "SELECT COUNT(DISTINCT coh.subject_id) AS persons, ",
        "COUNT(*) AS exposures ",
        "FROM ", cohort, " coh", where),
        handle$target_dialect))
      cnt <- .executeQuery(handle, sql)
      rows[[length(rows) + 1L]] <- data.frame(
        step_order = i,
        step       = st$label,
        persons    = as.numeric(cnt$persons[1]),
        exposures  = as.numeric(cnt$exposures[1]),
        stringsAsFactors = FALSE)
    }
    if (length(rows) == 0) return(data.frame())
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }
}

#' \code{dsomop:ohdsi.cohort_method.cm_attrition} entry (CohortMethod attrition)
#' @keywords internal
.omopCmAttritionEntry <- function() {
  name <- "dsomop:ohdsi.cohort_method.cm_attrition"
  plot_code <- paste(
    "function(df, params) {",
    "  df <- df[order(df$step_order), , drop = FALSE]",
    "  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(step, step_order),",
    "                                   y = persons)) +",
    "    ggplot2::geom_col() + ggplot2::coord_flip() +",
    "    ggplot2::labs(x = 'Inclusion step', y = 'Persons remaining')",
    "}", sep = "\n")

  .omopAnalysisEntry(
    name        = name,
    description = paste0("CohortMethod attrition (live): persons and exposures ",
                         "remaining after each ordered inclusion step over the ",
                         "scoped exposure cohort. unit=record; persons is the ",
                         "gated distinct-person companion."),
    domain      = "general",
    params      = list(
      list(name = "require_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Keep persons with >=1 record of this concept (descendants expanded)."),
      list(name = "require_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4"),
           description = "Domain of require_concept_id (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "exclude_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Drop persons with >=1 record of this concept (descendants expanded)."),
      list(name = "exclude_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4"),
           description = "Domain of exclude_concept_id."),
      list(name = "min_age", type = "int", required = FALSE, default = NULL,
           description = "Minimum age at the cohort index date."),
      list(name = "max_age", type = "int", required = FALSE, default = NULL,
           description = "Maximum age at the cohort index date."),
      list(name = "min_prior_observation", type = "int", required = FALSE,
           default = NULL,
           description = "Minimum prior-observation days before the cohort index date.")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = .omopCmAttritionFn(),
      plot = list(type = "bar", code = plot_code)
    ),
    dependencies = list(
      tables = c("person", "observation_period", "condition_occurrence"),
      packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit          = "record",
      count_cols    = c("persons", "exposures"),
      person_id_col = "persons"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "ohdsi_live", tool_id = "cohort_method",
                 table_name = "cm_attrition")
  )
}

# --- Entry 2: cm_result substrate + MDRR (canonical cm.mdrr) ------------------

#' Minimum Detectable Relative Risk from gated comparative aggregates
#'
#' The descriptive substrate of a comparative cohort result is honestly
#' computable as an aggregate (persons, person-time and outcomes per arm); the
#' FITTED effect estimate (RR / HR / CI) is not — it needs a regression over
#' patient-level survival data — so this port returns the substrate plus the
#' Minimum Detectable Relative Risk (MDRR): the smallest relative risk the study
#' is powered to detect given its event count and exposure split.
#'
#' For a two-arm survival/Poisson comparison the log-RR standard error is
#' \code{sqrt(1 / (D * p * (1 - p)))} where \code{D} is the total number of
#' outcomes and \code{p} is the fraction of person-time in the target arm, so
#' \code{MDRR = exp((z_alpha + z_beta) * se_logRR)} with \code{z_alpha} the
#' (two-sided by default) type-I-error quantile and \code{z_beta} the power
#' quantile. CRITICALLY, \code{D}, the target person-time and the comparator
#' person-time are BANDED FIRST (the same \code{nfilter_band} the gate uses) and
#' \code{p} + the MDRR are computed from those banded values; the MDRR is NA
#' whenever any input is suppressed (below \code{nfilter_tab}, NA, or a zero
#' person-time) — mirroring \code{\link{.omopAnalysisReconcileRatio}} so the
#' released MDRR never rests on un-banded counts.
#'
#' @param outcomes Numeric; total outcomes across both arms (raw).
#' @param target_days,comparator_days Numeric; person-days per arm (raw).
#' @param alpha Numeric; type-I error (default 0.05).
#' @param power Numeric; target power (default 0.80).
#' @param two_sided Logical; two-sided test (default TRUE).
#' @return Numeric scalar MDRR, or \code{NA_real_} when any input is suppressed.
#' @keywords internal
.omopCmMdrr <- function(outcomes, target_days, comparator_days,
                        alpha = 0.05, power = 0.80, two_sided = TRUE) {
  settings <- .omopDisclosureSettings()
  bw  <- settings$nfilter_band
  thr <- settings$nfilter_tab
  band_or_na <- function(x) {
    x <- as.numeric(x)
    v <- .bandCount(x, band_width = bw)
    if (is.na(x) || x < thr) NA_real_ else v
  }
  D  <- band_or_na(outcomes)
  td <- band_or_na(target_days)
  cd <- band_or_na(comparator_days)
  if (is.na(D) || is.na(td) || is.na(cd) || D <= 0) return(NA_real_)
  total <- td + cd
  if (total <= 0) return(NA_real_)
  p <- td / total
  if (is.na(p) || p <= 0 || p >= 1) return(NA_real_)
  z_alpha <- stats::qnorm(1 - (if (isTRUE(two_sided)) alpha / 2 else alpha))
  z_beta  <- stats::qnorm(power)
  se_logrr <- sqrt(1 / (D * p * (1 - p)))
  exp((z_alpha + z_beta) * se_logrr)
}

#' Build the cm_result substrate + MDRR live compute fn
#'
#' Over the scoped target+comparator pair (\code{ctx$scoped_cohorts}): per arm,
#' count distinct persons, sum the time-at-risk (person-days within the TAR) and
#' count outcome occurrences (descendant-expanded outcome concept set). Self-gate
#' BOTH arms before materialising. Return UN-GATED arm rows (persons / person_days
#' / outcomes declared as count_cols) PLUS one mdrr row whose value is derived
#' from the BANDED aggregates (NA when any input is suppressed). The fitted
#' RR/HR/CI is never computed.
#'
#' @return A \code{function(handle, ctx, params)} returning an aggregate frame.
#' @keywords internal
.omopCmResultFn <- function() {
  function(handle, ctx, params) {
    cohorts <- .omopTwoPopCohorts(handle, ctx)
    # cm_result compares two arms; without a two-population scope there is no
    # comparison to summarise. Return a gate-safe empty frame.
    if (is.null(cohorts)) return(data.frame())

    outcome_id   <- params$outcome_concept_id
    domain_code  <- params$outcome_domain_code %||% "0"
    tar_start    <- as.integer(params$tar_start_offset %||% "1")
    tar_end      <- as.integer(params$tar_end_offset %||% "0")
    anchor_start <- params$tar_anchor_start %||% "start"
    anchor_end   <- params$tar_anchor_end %||% "end"
    alpha     <- suppressWarnings(as.numeric(params$alpha %||% "0.05"))
    power     <- suppressWarnings(as.numeric(params$power %||% "0.8"))
    two_sided <- !identical(params$two_sided %||% "1", "0")
    if (is.na(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05
    if (is.na(power) || power <= 0 || power >= 1) power <- 0.8

    out_src <- .omopOutcomeSource(handle, outcome_id, domain_code)

    arm_row <- function(arm, cohort) {
      end_col <- .omopCohortEndDateCol(handle, cohort)
      anchor <- function(a) if (identical(a, "end")) end_col else "cohort_start_date"
      tar_lo <- paste0("DATEADD(day, ", tar_start, ", coh.", anchor(anchor_start), ")")
      tar_hi <- paste0("DATEADD(day, ", tar_end, ", coh.", anchor(anchor_end), ")")
      pdays  <- .omopDateDiffDays(handle, tar_hi, tar_lo)

      outcome_join <- ""
      out_event_expr <- "0"
      if (!is.null(out_src)) {
        outcome_join <- paste0(
          " LEFT JOIN ", out_src$table, " o ON o.", out_src$person_col,
          " = coh.subject_id AND o.", out_src$concept_col, " IN (",
          out_src$id_list, ") AND o.", out_src$date_col, " >= ", tar_lo,
          " AND o.", out_src$date_col, " <= ", tar_hi)
        out_event_expr <- paste0("COUNT(o.", out_src$concept_col, ")")
      }
      sql <- .sql_translate(paste0(
        "SELECT COUNT(DISTINCT coh.subject_id) AS persons, ",
        "SUM(", pdays, ") AS person_days, ",
        out_event_expr, " AS outcomes ",
        "FROM ", cohort, " coh", outcome_join),
        handle$target_dialect)
      cnt <- .executeQuery(handle, sql)
      data.frame(
        arm         = arm,
        persons     = as.numeric(cnt$persons[1]),
        person_days = as.numeric(cnt$person_days[1]),
        outcomes    = as.numeric(cnt$outcomes[1]),
        mdrr        = NA_real_,
        stringsAsFactors = FALSE)
    }

    ta <- arm_row("target", cohorts$a)
    co <- arm_row("comparator", cohorts$b)

    # MDRR is a SINGLE study-level value derived from the BANDED aggregates (NA
    # when any input is suppressed). It is carried as a COLUMN on BOTH arm rows
    # (NOT a separate row): a person-less mdrr row would have an NA distinct-person
    # companion and be dropped by the gate's record branch, so the value rides on
    # the arm rows that survive gating on their own person counts — i.e. the MDRR
    # is released iff the arms it summarises are themselves releasable. The arm
    # counts are declared (not pre-gated); the single gate bands them.
    total_out <- sum(c(ta$outcomes, co$outcomes), na.rm = TRUE)
    mdrr <- .omopCmMdrr(total_out, ta$person_days, co$person_days,
                        alpha = alpha, power = power, two_sided = two_sided)
    ta$mdrr <- as.numeric(mdrr)
    co$mdrr <- as.numeric(mdrr)

    out <- rbind(ta, co)
    rownames(out) <- NULL
    out
  }
}

#' \code{dsomop:ohdsi.cohort_method.cm_result} entry (CohortMethod substrate + MDRR)
#' @keywords internal
.omopCmResultEntry <- function() {
  name <- "dsomop:ohdsi.cohort_method.cm_result"
  plot_code <- paste(
    "function(df, params) {",
    "  d <- df[df$arm %in% c('target','comparator'), , drop = FALSE]",
    "  ggplot2::ggplot(d, ggplot2::aes(x = arm, y = outcomes)) +",
    "    ggplot2::geom_col() +",
    "    ggplot2::labs(x = 'Arm', y = 'Outcome events')",
    "}", sep = "\n")

  .omopAnalysisEntry(
    name        = name,
    description = paste0("CohortMethod comparative substrate (live): persons, ",
                         "person-time and outcomes per arm over a scoped ",
                         "target+comparator pair, plus the Minimum Detectable ",
                         "Relative Risk derived from the gated aggregates. The ",
                         "fitted RR/HR/CI is out of scope. unit=record; persons ",
                         "is the gated distinct-person companion; MDRR is NA when ",
                         "any input is suppressed."),
    domain      = "general",
    params      = list(
      list(name = "outcome_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Outcome concept id (descendants expanded)."),
      list(name = "outcome_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4"),
           description = "Outcome domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "tar_start_offset", type = "int", required = FALSE,
           default = "1"),
      list(name = "tar_end_offset", type = "int", required = FALSE,
           default = "0"),
      list(name = "tar_anchor_start", type = "enum", required = FALSE,
           default = "start", choices = c("start", "end")),
      list(name = "tar_anchor_end", type = "enum", required = FALSE,
           default = "end", choices = c("start", "end")),
      list(name = "alpha", type = "number", required = FALSE, default = "0.05",
           description = "Type-I error for the MDRR power calculation."),
      list(name = "power", type = "number", required = FALSE, default = "0.8",
           description = "Target power for the MDRR power calculation."),
      list(name = "two_sided", type = "bool", required = FALSE, default = "1",
           description = "Two-sided test for the MDRR (1) or one-sided (0)."),
      list(name = "model_type", type = "enum", required = FALSE, default = "cox",
           choices = c("cox", "poisson"),
           description = "Power model family (descriptive label; both use the log-RR SE).")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = .omopCmResultFn(),
      plot = list(type = "bar", code = plot_code)
    ),
    dependencies = list(
      tables = c("condition_occurrence", "concept"),
      packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit          = "record",
      count_cols    = c("persons", "outcomes", "person_days"),
      person_id_col = "persons"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 2L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "ohdsi_live", tool_id = "cohort_method",
                 table_name = "cm_result")
  )
}

# --- Entry 3: cm_diagnostics_summary (canonical cm.diagnostics_summary) -------

#' Baseline + final-step distinct-person counts for one arm's inclusion funnel
#'
#' The attrition fraction needs only the two ENDPOINTS of an arm's inclusion
#' funnel: the persons at the baseline step ("entered exposure cohort") and the
#' persons after the LAST inclusion predicate is applied cumulatively. We reuse
#' the SAME ordered predicates as \code{\link{.omopCmAttritionSteps}} and the SAME
#' cumulative-WHERE / birth-date-splice idiom as \code{\link{.omopCmAttritionFn}},
#' but materialise only the first and last counts (the intermediate steps are not
#' needed for the ratio). Both counts are RAW distinct-person counts; the caller
#' bands them (the ratio is reconciled via
#' \code{\link{.omopAnalysisReconcileRatio}}). The arm population is self-gated
#' before any count is materialised.
#'
#' @param handle CDM handle.
#' @param cohort Validated arm cohort temp table.
#' @param params Sanitized param literals (the same inclusion params cm_attrition
#'   accepts).
#' @return list(initial = <baseline persons>, final = <persons after last step>),
#'   raw (un-banded) numerics.
#' @keywords internal
.omopCmAttritionEndpoints <- function(handle, cohort, params) {
  .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort, " coh"),
    handle$target_dialect))

  steps <- .omopCmAttritionSteps(handle, params)
  cumulative <- character(0)
  count_persons <- function(where_clauses) {
    where <- if (length(where_clauses) > 0) {
      paste0(" WHERE ", paste(where_clauses, collapse = " AND "))
    } else ""
    sql <- .omopCmSpliceBirthDate(handle, .sql_translate(paste0(
      "SELECT COUNT(DISTINCT coh.subject_id) AS persons FROM ", cohort,
      " coh", where), handle$target_dialect))
    as.numeric(.executeQuery(handle, sql)$persons[1])
  }
  initial <- count_persons(cumulative)
  for (i in seq_along(steps)) {
    if (!is.null(steps[[i]]$predicate)) {
      cumulative <- c(cumulative, steps[[i]]$predicate)
    }
  }
  final <- count_persons(cumulative)
  list(initial = initial, final = final)
}

#' Build the cm_diagnostics_summary live compute fn
#'
#' R-in-session assembly of the CohortMethod diagnostics summary as ONE row of
#' SCALAR study-quality metrics, each DERIVED from the gated single-site
#' aggregates the sibling ports already build. Over the scoped target+comparator
#' pair (\code{ctx$scoped_cohorts}; both arms self-gated by
#' \code{\link{.omopTwoPopCohorts}}), the sibling kernels are called in this one fn
#' and their outputs reduced to scalars:
#' \itemize{
#'   \item \strong{mdrr}: total outcomes + per-arm person-time are counted exactly
#'     as \code{\link{.omopCmResultFn}} does (descendant-expanded outcome set, TAR
#'     window), then passed to \code{\link{.omopCmMdrr}} VERBATIM — which bands
#'     each input and is NA on any suppressed input.
#'   \item \strong{max_sdm}: \code{max(abs(SMD))} over
#'     \code{\link{.omopTwoArmCovariateSmd}} (each SMD is already NA when either
#'     arm is suppressed, so the max is over disclosure-safe SMDs only; NA when no
#'     SMD survives).
#'   \item \strong{attrition_fraction}: final-step persons / baseline persons
#'     (per-arm endpoints from \code{\link{.omopCmAttritionEndpoints}}, summed
#'     across arms), reconciled from BANDED counts via
#'     \code{\link{.omopAnalysisReconcileRatio}} (NA when either endpoint is
#'     suppressed).
#'   \item \strong{shared_balance_pass} (the "ease" / shared-balance flag): the
#'     boolean \code{max_sdm < 0.1}, returned as 1/0 and NA when max_sdm is NA.
#'   \item \strong{equipoise}, \strong{ps_auc}: NA — both need per-subject
#'     propensity scores / a fitted PS model whose disclosure-safe summary belongs
#'     to the propensity-overlap port (PLR-6, not yet built). Never released as an
#'     individual-score-derived value.
#' }
#' The row carries BOTH arms' distinct-person counts (n_persons_target /
#' n_persons_comparator) and their MIN (n_persons_min, the person_id_col), all
#' declared as count_cols so the ONE gate bands them and DROPS the row unless BOTH
#' arms meet nfilter.subset. No scalar rests on an un-banded count.
#'
#' @return A \code{function(handle, ctx, params)} returning a one-row aggregate.
#' @keywords internal
.omopCmDiagnosticsSummaryFn <- function() {
  function(handle, ctx, params) {
    cohorts <- .omopTwoPopCohorts(handle, ctx)
    # The diagnostics summary describes a target-vs-comparator study; without a
    # two-population scope there is nothing to summarise. Gate-safe empty frame.
    if (is.null(cohorts)) return(data.frame())

    outcome_id   <- params$outcome_concept_id
    domain_code  <- params$outcome_domain_code %||% "0"
    cov_domain   <- params$covariate_domain_code %||% "0"
    top_n        <- as.integer(params$top_n %||% "50")
    tar_start    <- as.integer(params$tar_start_offset %||% "1")
    tar_end      <- as.integer(params$tar_end_offset %||% "0")
    anchor_start <- params$tar_anchor_start %||% "start"
    anchor_end   <- params$tar_anchor_end %||% "end"
    alpha     <- suppressWarnings(as.numeric(params$alpha %||% "0.05"))
    power     <- suppressWarnings(as.numeric(params$power %||% "0.8"))
    two_sided <- !identical(params$two_sided %||% "1", "0")
    ease_thr  <- suppressWarnings(as.numeric(params$ease_smd_threshold %||% "0.1"))
    if (is.na(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05
    if (is.na(power) || power <= 0 || power >= 1) power <- 0.8
    if (is.na(ease_thr) || ease_thr <= 0) ease_thr <- 0.1

    out_src <- .omopOutcomeSource(handle, outcome_id, domain_code)

    # --- Per-arm persons + person-time + outcomes (cm_result substrate) -------
    # Same counting idiom as .omopCmResultFn$arm_row: distinct persons, summed
    # time-at-risk over the TAR window, descendant-expanded outcome events.
    arm_substrate <- function(cohort) {
      end_col <- .omopCohortEndDateCol(handle, cohort)
      anchor <- function(a) if (identical(a, "end")) end_col else "cohort_start_date"
      tar_lo <- paste0("DATEADD(day, ", tar_start, ", coh.", anchor(anchor_start), ")")
      tar_hi <- paste0("DATEADD(day, ", tar_end, ", coh.", anchor(anchor_end), ")")
      pdays  <- .omopDateDiffDays(handle, tar_hi, tar_lo)
      outcome_join <- ""
      out_event_expr <- "0"
      if (!is.null(out_src)) {
        outcome_join <- paste0(
          " LEFT JOIN ", out_src$table, " o ON o.", out_src$person_col,
          " = coh.subject_id AND o.", out_src$concept_col, " IN (",
          out_src$id_list, ") AND o.", out_src$date_col, " >= ", tar_lo,
          " AND o.", out_src$date_col, " <= ", tar_hi)
        out_event_expr <- paste0("COUNT(o.", out_src$concept_col, ")")
      }
      sql <- .sql_translate(paste0(
        "SELECT COUNT(DISTINCT coh.subject_id) AS persons, ",
        "SUM(", pdays, ") AS person_days, ",
        out_event_expr, " AS outcomes ",
        "FROM ", cohort, " coh", outcome_join),
        handle$target_dialect)
      cnt <- .executeQuery(handle, sql)
      list(persons     = as.numeric(cnt$persons[1]),
           person_days = as.numeric(cnt$person_days[1]),
           outcomes    = as.numeric(cnt$outcomes[1]))
    }
    ta <- arm_substrate(cohorts$a)
    co <- arm_substrate(cohorts$b)

    # (a) mdrr -- reuse the cm_result kernel verbatim (bands inputs; NA-on-suppress).
    total_out <- sum(c(ta$outcomes, co$outcomes), na.rm = TRUE)
    mdrr <- .omopCmMdrr(total_out, ta$person_days, co$person_days,
                        alpha = alpha, power = power, two_sided = two_sided)

    # (b) max_sdm -- largest covariate imbalance over the banded-SMD kernel. Each
    # SMD is already NA where either arm is suppressed, so max(abs(.)) is over
    # disclosure-safe SMDs only; NA when none survives.
    bal <- .omopTwoArmCovariateSmd(handle, cohorts$a, cohorts$b,
                                   domain_code = cov_domain, top_n = top_n,
                                   a_label = "target", b_label = "comparator",
                                   smd_col = "smd")
    max_sdm <- NA_real_
    if (is.data.frame(bal) && nrow(bal) > 0 && "smd" %in% names(bal)) {
      smds <- abs(bal$smd[!is.na(bal$smd)])
      if (length(smds) > 0) max_sdm <- max(smds)
    }

    # (c) attrition_fraction -- final/baseline persons summed across arms,
    # reconciled from BANDED counts (NA when either endpoint is suppressed).
    ea <- .omopCmAttritionEndpoints(handle, cohorts$a, params)
    eb <- .omopCmAttritionEndpoints(handle, cohorts$b, params)
    attr_initial <- sum(c(ea$initial, eb$initial), na.rm = TRUE)
    attr_final   <- sum(c(ea$final, eb$final), na.rm = TRUE)
    attr_df <- .omopAnalysisReconcileRatio(
      data.frame(attr_final = attr_final, attr_initial = attr_initial,
                 attrition_fraction = NA_real_, stringsAsFactors = FALSE),
      numerator_col = "attr_final", denominator_col = "attr_initial",
      ratio_col = "attrition_fraction", scale = 1)
    attrition_fraction <- as.numeric(attr_df$attrition_fraction[1])

    # (d) ease / shared-balance pass -- boolean over the (disclosure-safe) max_sdm.
    shared_balance_pass <- if (is.na(max_sdm)) NA_real_ else {
      if (max_sdm < ease_thr) 1 else 0
    }

    # (e) equipoise / ps_auc -- per-subject PS scores / fitted PS model required;
    # the disclosure-safe summary is the propensity-overlap port's (PLR-6). NEVER
    # an individual-score-derived value here.
    equipoise <- NA_real_
    ps_auc    <- NA_real_

    # ONE row of scalars + the count basis each rests on. Both arms' distinct-
    # person counts AND their MIN are declared count_cols: the gate bands them and
    # (gating on n_persons_min) DROPS the row unless BOTH arms meet nfilter.subset.
    n_min <- min(c(ta$persons, co$persons), na.rm = FALSE)
    data.frame(
      summary             = "cm_diagnostics_summary",
      mdrr                = as.numeric(mdrr),
      max_sdm             = as.numeric(max_sdm),
      attrition_fraction  = as.numeric(attrition_fraction),
      shared_balance_pass = as.numeric(shared_balance_pass),
      equipoise           = as.numeric(equipoise),
      ps_auc              = as.numeric(ps_auc),
      n_persons_target    = as.numeric(ta$persons),
      n_persons_comparator = as.numeric(co$persons),
      n_persons_min       = as.numeric(n_min),
      stringsAsFactors = FALSE)
  }
}

#' \code{dsomop:ohdsi.cohort_method.cm_diagnostics_summary} entry (CM diagnostics)
#' @keywords internal
.omopCmDiagnosticsSummaryEntry <- function() {
  name <- "dsomop:ohdsi.cohort_method.cm_diagnostics_summary"
  plot_code <- paste(
    "function(df, params) {",
    "  vars <- c('mdrr','max_sdm','attrition_fraction')",
    "  d <- data.frame(metric = vars,",
    "                  value = as.numeric(df[1, vars]))",
    "  d <- d[!is.na(d$value), , drop = FALSE]",
    "  ggplot2::ggplot(d, ggplot2::aes(x = metric, y = value)) +",
    "    ggplot2::geom_col() + ggplot2::coord_flip() +",
    "    ggplot2::labs(x = 'Diagnostic', y = 'Value')",
    "}", sep = "\n")

  .omopAnalysisEntry(
    name        = name,
    description = paste0("CohortMethod diagnostics summary (live): one row of ",
                         "scalar study-quality metrics over a scoped ",
                         "target+comparator pair (MDRR, max covariate SMD, ",
                         "attrition fraction, shared-balance pass flag), each ",
                         "derived from gated single-site aggregates. Equipoise / ",
                         "PS-AUC are NA (need the PLR-6 propensity-overlap ",
                         "summary; never released from individual PS scores). ",
                         "unit=record; the row is dropped unless both arms meet ",
                         "the person threshold."),
    domain      = "general",
    params      = list(
      list(name = "outcome_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Outcome concept id (descendants expanded) for the MDRR substrate."),
      list(name = "outcome_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4"),
           description = "Outcome domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "covariate_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4"),
           description = "Covariate domain for the max-SMD balance metric."),
      list(name = "top_n", type = "int", required = FALSE, default = "50",
           description = "Covariates considered for the max-SMD metric."),
      list(name = "tar_start_offset", type = "int", required = FALSE,
           default = "1"),
      list(name = "tar_end_offset", type = "int", required = FALSE,
           default = "0"),
      list(name = "tar_anchor_start", type = "enum", required = FALSE,
           default = "start", choices = c("start", "end")),
      list(name = "tar_anchor_end", type = "enum", required = FALSE,
           default = "end", choices = c("start", "end")),
      list(name = "alpha", type = "number", required = FALSE, default = "0.05",
           description = "Type-I error for the MDRR power calculation."),
      list(name = "power", type = "number", required = FALSE, default = "0.8",
           description = "Target power for the MDRR power calculation."),
      list(name = "two_sided", type = "bool", required = FALSE, default = "1",
           description = "Two-sided test for the MDRR (1) or one-sided (0)."),
      list(name = "ease_smd_threshold", type = "number", required = FALSE,
           default = "0.1",
           description = "Max |SMD| under which shared_balance_pass is 1."),
      # Inclusion params for the attrition-fraction endpoints (same as cm_attrition).
      list(name = "require_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Attrition: keep persons with >=1 record of this concept."),
      list(name = "require_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4")),
      list(name = "exclude_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Attrition: drop persons with >=1 record of this concept."),
      list(name = "exclude_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4")),
      list(name = "min_age", type = "int", required = FALSE, default = NULL),
      list(name = "max_age", type = "int", required = FALSE, default = NULL),
      list(name = "min_prior_observation", type = "int", required = FALSE,
           default = NULL)
    ),
    compute = list(
      kind = "r", sql = NULL, fn = .omopCmDiagnosticsSummaryFn(),
      plot = list(type = "bar", code = plot_code)
    ),
    dependencies = list(
      tables = c("person", "observation_period", "condition_occurrence",
                 "concept"),
      packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit          = "record",
      count_cols    = c("n_persons_target", "n_persons_comparator",
                        "n_persons_min"),
      person_id_col = "n_persons_min"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 2L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "ohdsi_live", tool_id = "cohort_method",
                 table_name = "cm_diagnostics_summary")
  )
}

# --- Group registrar ----------------------------------------------------------

#' Emit the OHDSI-B2 CohortMethod catalog entries (native live compute)
#'
#' The group registrar for \code{ohdsi_pack_cohortmethod.R}: the three
#' CohortMethod analyses with an honest descriptive substrate (cm_attrition,
#' cm_result, cm_diagnostics_summary), each COMPUTING its metric LIVE from the CDM
#' (no precomputed OHDSI results read). Returned as a named list keyed by entry id,
#' exactly as \code{\link{.omopAnalysisOhdsiEntries}} concatenates. \code{handle}
#' is taken for signature parity ONLY and is never queried at build time (catalog
#' build is cached on \code{handle$analysis_catalog}; all DB I/O happens later
#' inside each entry's \code{compute$fn}).
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by entry id.
#' @keywords internal
.ohdsiPackCohortMethodEntries <- function(handle) {
  entries <- list(
    .omopCmAttritionEntry(),
    .omopCmResultEntry(),
    # PLR-4-cm-interaction (lives in ohdsi_pack_cm_interaction.R): the live
    # R-in-session effect-modification estimate overlaying the precomputed
    # cm_interaction_result id.
    .omopCmInteractionEntry(),
    .omopCmDiagnosticsSummaryEntry(),
    .omopCmKaplanMeierEntry()
  )
  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}

# --- Canonical short-id aliases (cm.attrition / cm.mdrr / cm.diagnostics_summary)
#
# The group plan introduces new canonical short ids in the dsomop:cm.* namespace
# that POINT AT the same live compute as the stable OHDSI ids above, so an analyst
# can reach them as "cm.attrition" / "cm.mdrr" / "cm.diagnostics_summary"
# alongside the existing cm.* diagnostics. They are thin re-labels of the OHDSI
# entries: same compute$fn, same disclosure, same scope — only the name + adapter
# label differ. They are registered by .omopAnalysisDiagnosticEntries (the single
# place that owns the dsomop:cm.* family), so they sit next to
# cm.followup_distribution / cm.covariate_balance and preserve their twin's scope
# there (the cm_* twins are appended AFTER the requires_cohort forcing pass).

#' \code{dsomop:cm.attrition} canonical alias of the live cm_attrition entry
#' @keywords internal
.omopCmAttritionCanonicalEntry <- function() {
  e <- .omopCmAttritionEntry()
  e$name <- "dsomop:cm.attrition"
  e$meta$adapter <- "ohdsi_live"
  e
}

#' \code{dsomop:cm.mdrr} canonical alias of the live cm_result substrate+MDRR entry
#' @keywords internal
.omopCmMdrrCanonicalEntry <- function() {
  e <- .omopCmResultEntry()
  e$name <- "dsomop:cm.mdrr"
  e$meta$adapter <- "ohdsi_live"
  e
}

#' \code{dsomop:cm.diagnostics_summary} canonical alias of the live
#' cm_diagnostics_summary entry
#' @keywords internal
.omopCmDiagnosticsSummaryCanonicalEntry <- function() {
  e <- .omopCmDiagnosticsSummaryEntry()
  e$name <- "dsomop:cm.diagnostics_summary"
  e$meta$adapter <- "ohdsi_live"
  e
}

#' \code{dsomop:cm.kaplan_meier} canonical alias of the live cm_kaplan_meier_dist
#' KM-curve entry
#' @keywords internal
.omopCmKaplanMeierCanonicalEntry <- function() {
  e <- .omopCmKaplanMeierEntry()
  e$name <- "dsomop:cm.kaplan_meier"
  e$meta$adapter <- "ohdsi_live"
  e
}

# --- Entry 4: cm.propensity_distribution (live in-session glm; PS substrate) --
#
# PLR-6-cm-propensity. The NEW canonical dsomop:cm.propensity_distribution: the
# diagnostic substrate behind cm_diagnostics_summary's equipoise / ps_auc and the
# PS-overlap plot. It ADDS a live analysis rather than overlaying a precomputed
# OHDSI results id of its own (cm_diagnostics_summary is the live id that leaves
# equipoise / ps_auc as NA and points here for the disclosure-safe summary). It is
# the glm-summary-only path the task names explicitly for propensity scores: a
# propensity model is FITTED in the server-side R session over a per-subject
# covariate design matrix, and ONLY disclosure-safe aggregates leave R.
#
# THE R-IN-SESSION COMPUTE (.omopCmPropensityFn): over the scoped target +
# comparator pair (ctx$scoped_cohorts) it (1) picks the top-K most prevalent
# covariates over the POOLED pair (the same .omopCovariateSource family as the
# PLR-5 / cm.covariate_balance kernels), (2) LOADS a per-subject binary design
# matrix (one row per subject; a 0/1 column per covariate) plus a binary arm
# label INTO R, (3) FITS stats::glm(arm ~ covariates, family = binomial) and
# reads each subject's fitted propensity score, all IN-SESSION. Then it emits
# ONLY: (a) the per-arm preference-score histogram on a FIXED [0,1] grid (count
# per (arm, ps_bin)); (b) the equipoise fraction (share with PS in [0.3,0.7])
# reconciled from BANDED counts; (c) the PS c-statistic / AUC scalar (Mann-Whitney
# rank statistic = a model-separation summary). The score vector and the glm
# coefficients are materialised in-session and DISCARDED — they never leave R.
#
# DISCLOSURE (PROPENSITY rule, verbatim): individual scores NEVER leave R — only
# the gated decile/preference bands and the summary scalars. The frame is
# unit="record", one row per (arm, ps_bin); person_count is the per-bin distinct-
# person count (each subject lands in exactly one bin, so it IS a person count)
# and is declared as BOTH the count column and the person_id_col, so the ONE
# .omopAnalysisGate's GENERIC record branch suppresses any bin with < nfilter
# persons and bands every surviving bin. The equipoise fraction is reconciled from
# BANDED numerator/denominator via .omopAnalysisReconcileRatio (NA wherever either
# is suppressed). The AUC scalar is NA'd in-fn when EITHER arm has < nfilter.subset
# persons (an explicit non-frame guard, since a scalar cannot ride the per-row
# gate) and rides only the surviving bin rows. Concepts drive covariate selection
# by id; no *_source_value / free text is ever read.

#' Resolve the top-K prevalent covariate concept ids over the pooled arm pair
#'
#' The propensity design matrix is built from the most prevalent covariates over
#' BOTH arms pooled (the standard "top-K by prevalence" covariate construction,
#' the same \code{\link{.omopCovariateSource}} domain family as
#' \code{\link{.omopTwoArmCovariateSmd}}). Selecting the universe by POOLED
#' distinct-person prevalence — not per arm — keeps the design matrix identical
#' across arms so the glm is well posed. Returns at most \code{top_k} integer
#' concept ids ordered by descending pooled prevalence (an empty vector when no
#' covariate is recorded over the pair).
#'
#' @param handle CDM handle.
#' @param cohort_a,cohort_b Validated arm cohort temp tables.
#' @param domain_code Character/int domain selector.
#' @param top_k Integer; number of covariates to keep.
#' @return Integer vector of concept ids (length <= top_k).
#' @keywords internal
.omopCmPropensityCovariates <- function(handle, cohort_a, cohort_b,
                                        domain_code = "0", top_k = 20L) {
  src   <- .omopCovariateSource(handle, domain_code)
  top_k <- max(as.integer(top_k %||% 20L), 1L)
  # Pooled person universe across both arms (UNION dedupes shared subjects), then
  # distinct-person prevalence per concept over that pooled population.
  pooled <- paste0("(SELECT subject_id FROM ", cohort_a,
                   " UNION SELECT subject_id FROM ", cohort_b, ")")
  sql <- .sql_translate(paste0(
    "SELECT e.", src$concept_col, " AS covariate_id, ",
    "COUNT(DISTINCT e.", src$person_col, ") AS n ",
    "FROM ", src$table, " e ",
    "INNER JOIN ", pooled, " u ON u.subject_id = e.", src$person_col,
    " GROUP BY e.", src$concept_col,
    " ORDER BY n DESC"),
    handle$target_dialect)
  df <- .executeQuery(handle, sql)
  if (!is.data.frame(df) || nrow(df) == 0) return(integer(0))
  ids <- suppressWarnings(as.integer(df$covariate_id))
  ids <- ids[!is.na(ids)]
  if (length(ids) > top_k) ids <- ids[seq_len(top_k)]
  ids
}

#' Load the per-subject binary covariate design matrix + arm label into R
#'
#' Pulls, for each subject in either arm, a 0/1 indicator for each of the supplied
#' covariate concept ids PLUS a binary arm label (target = 1), all INTO R as a
#' single data frame (the design matrix the in-session glm is fitted on). Each
#' subject contributes exactly one row; a covariate the subject has no record of
#' is 0. The query returns one (subject, covariate) presence row and the pivot to
#' a wide 0/1 matrix happens in R, so the SQL stays a simple membership join.
#' Subjects in BOTH arms are assigned to the target arm (a subject cannot hold two
#' arm labels in one regression); this de-duplication is documented and keeps the
#' label binary.
#'
#' @param handle CDM handle.
#' @param cohort_a,cohort_b Validated arm cohort temp tables (a = target).
#' @param concept_ids Integer vector of covariate concept ids.
#' @param domain_code Character/int domain selector.
#' @return Data frame with columns \code{subject_id}, \code{arm} (0/1) and one
#'   \code{cov_<id>} 0/1 column per covariate; or \code{NULL} if no subjects.
#' @keywords internal
.omopCmPropensityDesign <- function(handle, cohort_a, cohort_b, concept_ids,
                                    domain_code = "0") {
  src    <- .omopCovariateSource(handle, domain_code)

  # Arm roster: every subject in either arm, labelled 1 (target) or 0
  # (comparator). A subject in both arms is labelled target (MAX over the union),
  # so the label is strictly binary and each subject appears once.
  roster_sql <- .sql_translate(paste0(
    "SELECT u.subject_id, MAX(u.arm) AS arm FROM (",
    "SELECT subject_id, 1 AS arm FROM ", cohort_a,
    " UNION ALL SELECT subject_id, 0 AS arm FROM ", cohort_b,
    ") u GROUP BY u.subject_id"),
    handle$target_dialect)
  roster <- .executeQuery(handle, roster_sql)
  if (!is.data.frame(roster) || nrow(roster) == 0) return(NULL)
  roster$subject_id <- as.character(roster$subject_id)
  roster$arm <- as.integer(roster$arm)

  # Start from the roster (subject_id + arm); add one 0/1 column per covariate.
  design <- data.frame(subject_id = roster$subject_id, arm = roster$arm,
                       stringsAsFactors = FALSE)
  for (cid in concept_ids) design[[paste0("cov_", cid)]] <- 0L
  if (length(concept_ids) == 0) return(design)

  # (subject, covariate) presence over the pooled roster: one row per subject who
  # has >= 1 record of a selected covariate. Pivoted to wide 0/1 in R.
  idlist <- paste(concept_ids, collapse = ", ")
  pooled <- paste0("(SELECT subject_id FROM ", cohort_a,
                   " UNION SELECT subject_id FROM ", cohort_b, ")")
  pres_sql <- .sql_translate(paste0(
    "SELECT DISTINCT u.subject_id, e.", src$concept_col, " AS covariate_id ",
    "FROM ", pooled, " u ",
    "INNER JOIN ", src$table, " e ON e.", src$person_col, " = u.subject_id ",
    "AND e.", src$concept_col, " IN (", idlist, ")"),
    handle$target_dialect)
  pres <- .executeQuery(handle, pres_sql)
  if (is.data.frame(pres) && nrow(pres) > 0) {
    pres$subject_id   <- as.character(pres$subject_id)
    pres$covariate_id <- suppressWarnings(as.integer(pres$covariate_id))
    row_of <- match(pres$subject_id, design$subject_id)
    for (i in seq_len(nrow(pres))) {
      r <- row_of[i]
      if (is.na(r)) next
      col <- paste0("cov_", pres$covariate_id[i])
      if (col %in% names(design)) design[r, col] <- 1L
    }
  }
  design
}

#' Fit the in-session propensity model and return per-subject scores
#'
#' Fits \code{stats::glm(arm ~ covariates, family = binomial)} on the loaded
#' design matrix IN THE SERVER-SIDE R SESSION and returns each subject's fitted
#' propensity score (the predicted probability of being in the target arm).
#' Covariate columns that are constant across all subjects (no contrast) are
#' dropped before fitting so the glm is well posed; if NO covariate varies the
#' model is intercept-only (every score equals the marginal target prevalence,
#' which still yields a valid — if uninformative — distribution). The returned
#' vector aligns row-for-row with \code{design}; the fitted model object is NOT
#' returned (coefficients never leave R).
#'
#' @param design Design matrix from \code{\link{.omopCmPropensityDesign}}.
#' @return Numeric vector of per-subject propensity scores (length = nrow(design)).
#' @keywords internal
.omopCmPropensityScores <- function(design) {
  cov_cols <- grep("^cov_", names(design), value = TRUE)
  # Keep only covariates with >= 2 distinct values (a real contrast to fit on).
  varying <- cov_cols[vapply(cov_cols, function(c)
    length(unique(design[[c]])) > 1L, logical(1))]
  if (length(varying) == 0) {
    fit <- stats::glm(arm ~ 1, data = design, family = stats::binomial())
  } else {
    form <- stats::as.formula(paste("arm ~", paste(varying, collapse = " + ")))
    fit <- stats::glm(form, data = design, family = stats::binomial())
  }
  as.numeric(stats::predict(fit, type = "response"))
}

#' Preference-score transform of a propensity score
#'
#' The OHDSI preference score re-centres a propensity score on the marginal target
#' prevalence so equipoise is comparable across studies with different base rates:
#' \code{F = logit(ps) - logit(prev)} and the preference score is
#' \code{exp(F) / (1 + exp(F))}. With \code{prev = 0.5} this is the identity. PS
#' values are clamped off \{0,1\} before the logit to keep \code{F} finite. This
#' operates on the in-session score vector only; nothing here is released.
#'
#' @param ps Numeric vector of propensity scores in (0,1).
#' @param prev Numeric; marginal target prevalence.
#' @return Numeric vector of preference scores in (0,1).
#' @keywords internal
.omopCmPreferenceScore <- function(ps, prev) {
  ps <- pmin(pmax(as.numeric(ps), 1e-6), 1 - 1e-6)
  prev <- min(max(as.numeric(prev), 1e-6), 1 - 1e-6)
  f <- log(ps / (1 - ps)) - log(prev / (1 - prev))
  exp(f) / (1 + exp(f))
}

#' AUC (c-statistic) of the propensity model as a rank statistic
#'
#' The model-separation summary: the probability that a randomly chosen target
#' subject has a higher propensity score than a randomly chosen comparator subject
#' — the Mann-Whitney U / Wilcoxon rank-sum form of the c-statistic,
#' \code{AUC = (sum of target ranks - n_t (n_t + 1) / 2) / (n_t * n_c)} with ties
#' at 0.5. Computed in-session over the score vector; only the resulting scalar is
#' a candidate for release (the caller NAs it when either arm is below threshold).
#'
#' @param scores Numeric vector of propensity scores.
#' @param arm Integer 0/1 arm label aligned with \code{scores}.
#' @return Numeric scalar AUC in [0,1], or \code{NA_real_} when an arm is empty.
#' @keywords internal
.omopCmPropensityAuc <- function(scores, arm) {
  scores <- as.numeric(scores)
  t_idx <- which(arm == 1L)
  c_idx <- which(arm == 0L)
  n_t <- length(t_idx); n_c <- length(c_idx)
  if (n_t == 0 || n_c == 0) return(NA_real_)
  r <- rank(scores, ties.method = "average")
  (sum(r[t_idx]) - n_t * (n_t + 1) / 2) / (n_t * n_c)
}

#' Build the cm.propensity_distribution live in-session compute fn
#'
#' Over the scoped target+comparator pair: pick the top-K prevalent covariates,
#' LOAD the per-subject binary design matrix + arm label into R, FIT the propensity
#' glm IN-SESSION, transform each score to a preference score, and bin onto a FIXED
#' [0,1] grid. Emit per (arm, ps_bin) the distinct-person count (UN-gated: declared
#' as the record-unit person basis so the ONE gate suppresses + bands it), plus the
#' study-level equipoise fraction (reconciled from BANDED bin counts) and the AUC
#' scalar (NA when either arm < nfilter.subset). Both arms are self-gated by
#' \code{\link{.omopTwoPopCohorts}} before any rows are pulled. The score vector and
#' glm coefficients are discarded — never released.
#'
#' @return A \code{function(handle, ctx, params)} returning an aggregate frame.
#' @keywords internal
.omopCmPropensityFn <- function() {
  function(handle, ctx, params) {
    cohorts <- .omopTwoPopCohorts(handle, ctx)
    # A propensity model contrasts two arms; without a two-population scope there
    # is nothing to fit. Return a gate-safe empty frame (mirrors cm_result).
    if (is.null(cohorts)) return(data.frame())

    domain <- params$domain_code %||% "0"
    top_k  <- max(as.integer(params$top_k %||% "20"), 1L)
    n_bins <- max(as.integer(params$n_bins %||% "10"), 2L)

    # 1) Covariate universe (top-K prevalent over the pooled pair).
    cov_ids <- .omopCmPropensityCovariates(handle, cohorts$a, cohorts$b,
                                           domain_code = domain, top_k = top_k)

    # 2) Load the per-subject design matrix + arm label INTO R.
    design <- .omopCmPropensityDesign(handle, cohorts$a, cohorts$b, cov_ids,
                                      domain_code = domain)
    if (is.null(design) || nrow(design) == 0) return(data.frame())

    # 3) FIT the propensity glm IN-SESSION; read each subject's score.
    ps <- tryCatch(.omopCmPropensityScores(design),
                   error = function(e) rep(NA_real_, nrow(design)))
    if (length(ps) != nrow(design) || all(is.na(ps))) return(data.frame())

    arm  <- design$arm
    prev <- mean(arm == 1L)
    # Preference score (equipoise-comparable); the released histogram is binned on
    # the preference score, exactly as the OHDSI PS-overlap plot is.
    pref <- .omopCmPreferenceScore(ps, prev)

    # 4) Bin onto a FIXED [0,1] grid (study-independent edges so the histogram is
    #    never adapted to the data — no boundary leaks individual scores).
    edges <- seq(0, 1, length.out = n_bins + 1L)
    bin   <- findInterval(pref, edges, rightmost.closed = TRUE, all.inside = TRUE)
    bin   <- pmin(pmax(bin, 1L), n_bins)

    # Per (arm, bin) distinct-person count. Each subject is in exactly one bin and
    # one arm, so person_count IS a distinct-person count (declared as the record-
    # unit person basis; the ONE gate suppresses + bands it).
    key   <- paste(arm, bin, sep = "|")
    agg   <- tapply(seq_along(key), key, length)
    parts <- strsplit(names(agg), "|", fixed = TRUE)
    rows  <- data.frame(
      arm    = vapply(parts, function(p) if (p[1] == "1") "target" else
                        "comparator", character(1)),
      ps_bin = vapply(parts, function(p) as.integer(p[2]), integer(1)),
      stringsAsFactors = FALSE)
    rows$bin_low      <- edges[rows$ps_bin]
    rows$bin_high     <- edges[rows$ps_bin + 1L]
    rows$person_count <- as.numeric(agg)

    # NO-HINTS DROP: a bin whose distinct-person count is below the band width
    # would survive the gate's small-cell suppression (>= nfilter.tab) only to be
    # FLOORED to 0 by the band pass, surfacing a person_count == 0 row that leaks
    # the EXISTENCE of a near-individual PS cell (raw count in
    # [nfilter.tab, nfilter.band)) and its location on the [0,1] grid. Drop such
    # bins here so no banded-to-0 row is ever emitted — the same fail-closed
    # in-fn suppression the KM port applies to thin-tail bins, and the "no hints"
    # policy .suppressSmallCounts documents. Bins with >= nfilter.band persons
    # band to a non-zero multiple and flow through the gate unchanged.
    drop_thr <- max(.omopDisclosureSettings()$nfilter_band,
                    .omopDisclosureSettings()$nfilter_tab)
    rows <- rows[!is.na(rows$person_count) & rows$person_count >= drop_thr, ,
                 drop = FALSE]
    if (nrow(rows) == 0) return(data.frame())

    # 5) Equipoise fraction: share of ALL subjects with a preference score in
    #    [0.3, 0.7], reconciled from BANDED counts so it never rests on un-banded
    #    inputs. Numerator = banded equipoise persons, denominator = banded total;
    #    .omopAnalysisReconcileRatio NAs it whenever either is suppressed.
    settings <- .omopDisclosureSettings()
    bw  <- settings$nfilter_band
    thr <- settings$nfilter_tab
    band_or_na <- function(x) {
      x <- as.numeric(x)
      v <- .bandCount(x, band_width = bw)
      if (is.na(x) || x < thr) NA_real_ else v
    }
    n_equi  <- band_or_na(sum(pref >= 0.3 & pref <= 0.7))
    n_total <- band_or_na(length(pref))
    equipoise <- if (is.na(n_equi) || is.na(n_total) || n_total <= 0) {
      NA_real_
    } else {
      n_equi / n_total
    }

    # 6) AUC / c-statistic: NA'd when EITHER arm is below nfilter.subset (an
    #    explicit non-frame guard — a scalar cannot ride the per-row gate).
    n_t <- sum(arm == 1L); n_c <- sum(arm == 0L)
    auc <- if (n_t < settings$nfilter_subset || n_c < settings$nfilter_subset) {
      NA_real_
    } else {
      .omopCmPropensityAuc(ps, arm)
    }

    # The score vector + glm coefficients (ps, pref, design) go out of scope here
    # — only the gated per-bin counts + the two summary scalars are returned.
    rows$equipoise <- as.numeric(equipoise)
    rows$auc       <- as.numeric(auc)
    rows <- rows[order(rows$arm, rows$ps_bin), , drop = FALSE]
    rownames(rows) <- NULL
    rows
  }
}

#' \code{dsomop:cm.propensity_distribution} entry (live in-session PS substrate)
#' @keywords internal
.omopCmPropensityEntry <- function() {
  name <- "dsomop:cm.propensity_distribution"
  plot_code <- paste(
    "function(df, params) {",
    "  d <- df[df$arm %in% c('target','comparator'), , drop = FALSE]",
    "  ggplot2::ggplot(d, ggplot2::aes(x = bin_low, y = person_count,",
    "                                  fill = arm)) +",
    "    ggplot2::geom_col(position = 'identity', alpha = 0.5) +",
    "    ggplot2::labs(x = 'Preference score', y = 'Persons', fill = 'Arm')",
    "}", sep = "\n")

  .omopAnalysisEntry(
    name        = name,
    description = paste0("CohortMethod propensity-score distribution (live ",
                         "in-session): a propensity glm is FITTED in R over a ",
                         "per-subject covariate design matrix for a scoped ",
                         "target+comparator pair, and ONLY a banded per-arm ",
                         "preference-score histogram, the equipoise fraction ",
                         "(reconciled from banded counts) and the c-statistic / ",
                         "AUC are released. Individual scores never leave R. ",
                         "unit=record; person_count is the gated distinct-person ",
                         "basis; AUC is NA when either arm is below threshold."),
    domain      = "general",
    params      = list(
      list(name = "domain_code", type = "enum", required = FALSE, default = "0",
           choices = c("0", "1", "2", "3", "4"),
           description = "Covariate domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "top_k", type = "int", required = FALSE, default = "20",
           description = "Number of top-prevalence covariates in the propensity model."),
      list(name = "n_bins", type = "int", required = FALSE, default = "10",
           description = "Number of fixed [0,1] preference-score histogram bins.")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = .omopCmPropensityFn(),
      plot = list(type = "bar", code = plot_code)
    ),
    dependencies = list(
      tables = c("condition_occurrence", "concept"),
      packages = character(0)),
    # unit="record": person_count is a per-(arm,bin) distinct-person count that can
    # rest on too few persons, so the gate's GENERIC record branch
    # (meta$adapter="diagnostic") gates it on its declared sibling person column
    # (person_count itself) and bands it. The equipoise + auc scalars are derived
    # from banded inputs / NA'd in-fn and ride the surviving bin rows.
    disclosure = .omopAnalysisDisclosure(
      unit          = "record",
      count_cols    = "person_count",
      person_id_col = "person_count"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 2L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Entry 5: cm_kaplan_meier_dist (canonical cm.kaplan_meier) ----------------

#' Per-subject (arm, time, event) survival frame for the two-arm KM curve
#'
#' The R-in-session substrate of a Kaplan-Meier comparison: one row per cohort
#' member of one arm, carrying the observed follow-up time and a 0/1 event flag.
#' Built with the SAME two-query idiom as \code{\link{.extractSurvival}} (cohort
#' members + their qualifying outcome events fetched separately, the survival
#' fields computed on the R side), restricted to the validated arm cohort:
#' \enumerate{
#'   \item fetch \code{subject_id, cohort_start_date, cohort_end_date} for the arm;
#'   \item fetch the descendant-expanded outcome events (\code{out_src}) joined to
#'     the arm on \code{subject_id} (no \code{*_source_value} ever selected);
#'   \item per subject set the TAR window
#'     \code{[start + tar_start_offset .. min(first qualifying outcome,
#'     cohort_end + tar_end_offset)]}, the day count to that endpoint as the time,
#'     and \code{event = 1} iff the outcome fell inside the window (else a censor).
#' }
#' The frame is purely in-session scaffolding for \code{survival::survfit} +
#' binning; it is NEVER returned (only gated bin-level aggregates leave the
#' session). The arm population is self-gated by the caller
#' (\code{\link{.omopTwoPopCohorts}}) before this runs.
#'
#' @param handle CDM handle.
#' @param cohort Validated arm cohort temp table.
#' @param arm Character; the arm label on every row ("target"/"comparator").
#' @param out_src \code{\link{.omopOutcomeSource}} list (or NULL -> no events, all
#'   members censored at the TAR end).
#' @param tar_start_offset,tar_end_offset Integer day offsets for the TAR window.
#' @return Data frame (arm, time, event), one row per arm member (possibly empty).
#' @keywords internal
.omopCmKmSubjectFrame <- function(handle, cohort, arm, out_src,
                                  tar_start_offset, tar_end_offset) {
  members <- .executeQuery(handle, paste0(
    "SELECT subject_id, cohort_start_date, cohort_end_date FROM ", cohort,
    " ORDER BY subject_id"))
  if (!is.data.frame(members) || nrow(members) == 0) return(data.frame())

  members$tar_start <- as.Date(members$cohort_start_date) +
    as.integer(tar_start_offset)
  members$tar_end <- as.Date(members$cohort_end_date) +
    as.integer(tar_end_offset)

  members$outcome_date <- as.Date(NA)
  if (!is.null(out_src)) {
    ev <- .executeQuery(handle, .sql_translate(paste0(
      "SELECT o.", out_src$person_col, " AS subject_id, MIN(o.",
      out_src$date_col, ") AS outcome_date FROM ", out_src$table, " o ",
      "INNER JOIN ", cohort, " c ON c.subject_id = o.", out_src$person_col,
      " WHERE o.", out_src$concept_col, " IN (", out_src$id_list, ") ",
      "GROUP BY o.", out_src$person_col),
      handle$target_dialect))
    if (is.data.frame(ev) && nrow(ev) > 0) {
      first_out <- stats::setNames(as.Date(ev$outcome_date),
                                   as.character(ev$subject_id))
      hit <- first_out[as.character(members$subject_id)]
      members$outcome_date <- as.Date(unname(hit))
    }
  }

  # Event iff the FIRST qualifying outcome falls inside the TAR window; the
  # follow-up endpoint is the earlier of that outcome and the TAR end (censor).
  in_tar <- !is.na(members$outcome_date) &
    members$outcome_date >= members$tar_start &
    members$outcome_date <= members$tar_end
  endpoint <- members$tar_end
  endpoint[in_tar] <- members$outcome_date[in_tar]
  time_days <- as.integer(endpoint - members$tar_start)
  keep <- !is.na(time_days) & time_days >= 0
  if (!any(keep)) return(data.frame())

  data.frame(arm = arm, time = time_days[keep],
             event = as.integer(in_tar[keep]), stringsAsFactors = FALSE)
}

#' Fixed-grid bin edges (days) for the released Kaplan-Meier curve
#'
#' The released KM curve is reported on a COARSE fixed grid (monthly by default,
#' quarterly optional) rather than at raw event times: coarse bins are exactly
#' what makes per-time differencing of an individual's event date infeasible.
#' \code{time_unit} selects the bin width in days (month ~ 30.4375, quarter
#' ~ 91.3125, the average-length convention) and the grid spans 0 to the maximum
#' observed follow-up time across BOTH arms, so every subject lands in one bin.
#'
#' @param max_time Numeric; the largest follow-up time (days) across both arms.
#' @param time_unit Character; "month" (default) or "quarter".
#' @return Numeric vector of increasing bin edges starting at 0 (length >= 2).
#' @keywords internal
.omopCmKmBinEdges <- function(max_time, time_unit = "month") {
  width <- if (identical(time_unit, "quarter")) 91.3125 else 30.4375
  max_time <- max(as.numeric(max_time), 0)
  n_bins <- max(ceiling((max_time + 1) / width), 1)
  c(0, width * seq_len(n_bins))
}

#' Build the cm_kaplan_meier_dist live compute fn (R-in-session KM curve)
#'
#' Over the scoped target+comparator pair (\code{ctx$scoped_cohorts}; both arms
#' self-gated by \code{\link{.omopTwoPopCohorts}}), this COMPUTES a disclosure-safe
#' Kaplan-Meier curve per arm entirely in the server-side R session:
#' \enumerate{
#'   \item build the per-subject \code{(arm, time, event)} frame for each arm
#'     (\code{\link{.omopCmKmSubjectFrame}}) and LOAD it into R;
#'   \item fit \code{survival::survfit(Surv(time, event) ~ arm)} on the pooled
#'     frame (the in-session model the task mandates; its
#'     \code{time/surv/n.risk/n.event/strata} validate the per-arm risk-set
#'     bookkeeping);
#'   \item BIN onto a fixed coarse grid (\code{\link{.omopCmKmBinEdges}}; monthly
#'     by default), emitting ONE row per \code{(arm, time_bin)} with the at-risk
#'     count at the bin start and the events summed within the bin — both counted
#'     directly from the in-R per-subject frame (exact, censoring-aware), never the
#'     raw survfit step vectors.
#' }
#' The released \code{survival_probability} is recomputed as the cumulative product
#' of \code{1 - banded_events / banded_at_risk} over bins, NOT read from
#' \code{survfit$surv}: it must rest only on the banded bin counts so it can never
#' be differenced back to a raw KM step / individual event time. The frame is
#' returned with the raw per-bin counts DECLARED (not pre-gated) for the single
#' \code{\link{.omopAnalysisGate}}, after an explicit in-fn KM-curve disclosure
#' pass (the non-frame survival_probability cannot be gated generically):
#' \itemize{
#'   \item bands \code{at_risk} and \code{events} per bin (the gate's own band pass
#'     is idempotent over them);
#'   \item NAs \code{survival_probability} for any bin whose banded \code{at_risk}
#'     or banded \code{events} is NA-or-sub-threshold;
#'   \item DROPS trailing thin-tail bins once banded \code{at_risk} falls below
#'     \code{nfilter.subset} (no single-subject tail bins), per arm.
#' }
#'
#' @return A \code{function(handle, ctx, params)} returning an aggregate frame.
#' @keywords internal
.omopCmKaplanMeierFn <- function() {
  function(handle, ctx, params) {
    cohorts <- .omopTwoPopCohorts(handle, ctx)
    # KM compares two arms; without a two-population scope there is no curve to
    # draw. Gate-safe empty frame (same semantics as cm_result).
    if (is.null(cohorts)) return(data.frame())

    outcome_id   <- params$outcome_concept_id
    domain_code  <- params$outcome_domain_code %||% "0"
    tar_start    <- as.integer(params$tar_start_offset %||% "1")
    tar_end      <- as.integer(params$tar_end_offset %||% "0")
    time_unit    <- if (identical(params$time_unit %||% "month", "quarter")) {
      "quarter"
    } else "month"

    out_src <- .omopOutcomeSource(handle, outcome_id, domain_code)

    sa <- .omopCmKmSubjectFrame(handle, cohorts$a, "target", out_src,
                                tar_start, tar_end)
    sb <- .omopCmKmSubjectFrame(handle, cohorts$b, "comparator", out_src,
                                tar_start, tar_end)
    subj <- rbind(sa, sb)
    if (!is.data.frame(subj) || nrow(subj) == 0) return(data.frame())

    # Fit the in-session KM model the task mandates. survfit is REQUIRED here (it
    # is the template's in-R survival kernel); a missing package fails closed
    # (empty frame) rather than emitting a half-computed curve. survfit$strata /
    # $time / $n.risk / $n.event encode the per-arm risk sets we bin below.
    if (!requireNamespace("survival", quietly = TRUE)) return(data.frame())
    fit <- tryCatch(
      survival::survfit(survival::Surv(time, event) ~ arm, data = subj),
      error = function(e) NULL)
    if (is.null(fit)) return(data.frame())

    edges <- .omopCmKmBinEdges(max(subj$time, na.rm = TRUE), time_unit)
    settings <- .omopDisclosureSettings()
    bw  <- settings$nfilter_band
    thr <- settings$nfilter_tab
    sub_thr <- settings$nfilter_subset
    band_or_na <- function(x) {
      x <- as.numeric(x)
      v <- .bandCount(x, band_width = bw)
      if (is.na(x) || x < thr) NA_real_ else v
    }

    rows <- list()
    for (arm in c("target", "comparator")) {
      af <- subj[subj$arm == arm, , drop = FALSE]
      if (nrow(af) == 0) next
      surv_prob <- 1
      for (i in seq_len(length(edges) - 1L)) {
        lo <- edges[i]; hi <- edges[i + 1L]
        # at_risk at the bin START = subjects whose follow-up reaches lo; events
        # = those with an event inside [lo, hi). Counted from the in-R per-subject
        # frame (exact, censoring-aware), NOT survfit's raw step vectors.
        at_risk_raw <- sum(af$time >= lo)
        events_raw  <- sum(af$event == 1L & af$time >= lo & af$time < hi)
        at_risk_b <- band_or_na(at_risk_raw)
        events_b  <- band_or_na(events_raw)

        # Trailing thin tail: once the banded at-risk set falls below
        # nfilter.subset, this bin AND every later bin rests on a sub-threshold
        # risk set -> DROP this bin (do not emit it) and stop emitting bins for
        # this arm. Checked BEFORE appending so the boundary bin (the first below
        # nfilter.subset) is never released: a risk set below the subset floor is
        # not disclosure-safe even as a single tail bin, and emitting it would
        # leak a small risk-set count whenever nfilter.subset > nfilter.tab (the
        # gate's per-row companion suppression only enforces nfilter.tab).
        if (is.na(at_risk_b) || at_risk_b < sub_thr) break

        # KM step recomputed from the BANDED counts only (never survfit$surv): a
        # released survival_probability rests solely on banded bin counts, so no
        # per-time differencing can recover an individual's event time.
        if (!is.na(events_b) && at_risk_b > 0) {
          surv_prob <- surv_prob * (1 - events_b / at_risk_b)
          sp_out <- surv_prob
        } else {
          sp_out <- NA_real_
        }

        rows[[length(rows) + 1L]] <- data.frame(
          arm                  = arm,
          time_bin             = i,
          bin_start_days       = lo,
          bin_end_days         = hi,
          at_risk              = at_risk_raw,
          events               = events_raw,
          survival_probability = sp_out,
          stringsAsFactors = FALSE)
      }
    }
    if (length(rows) == 0) return(data.frame())
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }
}

#' \code{dsomop:ohdsi.cohort_method.cm_kaplan_meier_dist} entry (CM KM curve)
#' @keywords internal
.omopCmKaplanMeierEntry <- function() {
  name <- "dsomop:ohdsi.cohort_method.cm_kaplan_meier_dist"
  plot_code <- paste(
    "function(df, params) {",
    "  df <- df[order(df$arm, df$time_bin), , drop = FALSE]",
    "  ggplot2::ggplot(df, ggplot2::aes(x = bin_end_days,",
    "                                   y = survival_probability,",
    "                                   colour = arm)) +",
    "    ggplot2::geom_step() +",
    "    ggplot2::labs(x = 'Time (days)', y = 'Survival probability',",
    "                  colour = 'Arm')",
    "}", sep = "\n")

  .omopAnalysisEntry(
    name        = name,
    description = paste0("CohortMethod Kaplan-Meier curve (live, R-in-session): ",
                         "per-arm survival over a scoped target+comparator pair, ",
                         "computed by survival::survfit on the in-session ",
                         "per-subject (time,event) frame and reported on a coarse ",
                         "fixed time grid (monthly by default). Each (arm,bin) row ",
                         "carries banded at-risk + event counts; ",
                         "survival_probability is recomputed from the BANDED ",
                         "counts (never the raw KM step), small-count bins are ",
                         "NA'd and thin-tail bins dropped, so no individual event ",
                         "time can be differenced. unit=record; at_risk is the ",
                         "gated distinct-person companion."),
    domain      = "general",
    params      = list(
      list(name = "outcome_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Outcome concept id (descendants expanded)."),
      list(name = "outcome_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4"),
           description = "Outcome domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "tar_start_offset", type = "int", required = FALSE,
           default = "1",
           description = "TAR start day offset from the cohort index date."),
      list(name = "tar_end_offset", type = "int", required = FALSE,
           default = "0",
           description = "TAR end day offset from the cohort end date."),
      list(name = "time_unit", type = "enum", required = FALSE,
           default = "month", choices = c("month", "quarter"),
           description = "Fixed time-bin width for the released KM curve.")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = .omopCmKaplanMeierFn(),
      plot = list(type = "line", code = plot_code)
    ),
    dependencies = list(
      tables = c("condition_occurrence", "concept"),
      packages = "survival"),
    disclosure = .omopAnalysisDisclosure(
      unit          = "record",
      count_cols    = c("at_risk", "events"),
      person_id_col = "at_risk"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 2L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "ohdsi_live", tool_id = "cohort_method",
                 table_name = "cm_kaplan_meier_dist")
  )
}
