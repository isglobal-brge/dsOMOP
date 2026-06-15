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
# Nothing here reads cm_attrition / cm_result (or any other precomputed OHDSI
# results table). Both entries keep their stable ids ("dsomop:ohdsi.<tool>.<table>")
# so existing references resolve; only the compute changes (read-results ->
# live-compute). The canonical short ids are registered alongside in
# .omopAnalysisDiagnosticEntries.
#
# Disclosure: every count is declared (never pre-gated) so the SINGLE
# .omopAnalysisGate suppresses + bands it. cm_attrition is unit="record" with the
# distinct-person companion as person_id_col and meta$adapter="ohdsi_live", so the
# gate's GENERIC record branch (analysis_catalog.R L3278-3291) gates on the
# companion. cm_result is unit="record" too; its derived MDRR is reconciled from
# BANDED aggregates inside the fn (NA wherever an input is suppressed), exactly as
# .omopAnalysisReconcileRatio does for a rate. Concepts are translated to names by
# default; no *_source_value / free-text is ever selected. Population is the
# scoped exposure cohort (cm_attrition: a single population, max_tables = 1L) or
# the scoped pair (cm_result: target+comparator, max_tables = 2L);
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

# --- Group registrar ----------------------------------------------------------

#' Emit the OHDSI-B2 CohortMethod catalog entries (native live compute)
#'
#' The group registrar for \code{ohdsi_pack_cohortmethod.R}: the two CohortMethod
#' analyses with an honest descriptive substrate (cm_attrition, cm_result), each
#' COMPUTING its metric LIVE from the CDM (no precomputed OHDSI results read).
#' Returned as a named list keyed by entry id, exactly as
#' \code{\link{.omopAnalysisOhdsiEntries}} concatenates. \code{handle} is taken for
#' signature parity ONLY and is never queried at build time (catalog build is
#' cached on \code{handle$analysis_catalog}; all DB I/O happens later inside each
#' entry's \code{compute$fn}).
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by entry id.
#' @keywords internal
.ohdsiPackCohortMethodEntries <- function(handle) {
  entries <- list(
    .omopCmAttritionEntry(),
    .omopCmResultEntry()
  )
  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}

# --- Canonical short-id aliases (cm.attrition / cm.mdrr) ----------------------
#
# The group plan introduces two new canonical short ids in the dsomop:cm.*
# namespace that POINT AT the same live compute as the stable OHDSI ids above, so
# an analyst can reach them as "cm.attrition" / "cm.mdrr" alongside the existing
# cm.* diagnostics. They are thin re-labels of the OHDSI entries: same compute$fn,
# same disclosure, same scope — only the name + adapter label differ. They are
# registered by .omopAnalysisDiagnosticEntries (the single place that owns the
# dsomop:cm.* family), so they sit next to cm.followup_distribution /
# cm.covariate_balance and are marked requires_cohort there with the rest.

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
