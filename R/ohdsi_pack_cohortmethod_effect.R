# Module: OHDSI CohortMethod FITTED effect estimate (native live R-in-session)
#
# Group PLR-1-cm-effect. This file EXTENDS the CohortMethod pack
# (ohdsi_pack_cohortmethod.R) with the one analysis that file's cm_result port
# (dsomop:cm.mdrr) explicitly declared OUT-of-scope: the FITTED comparative effect
# estimate. It is kept in its own pack file purely so the group ports in parallel
# with the sibling CohortMethod groups (cm_diagnostics_summary, cm_propensity)
# without shared-file contention; the package sources every R/*.R, so these
# functions are part of the same namespace and the registrar in
# .omopAnalysisDiagnosticEntries (analysis_catalog.R) reaches
# .omopCmEffectEstimateCanonicalEntry() exactly as it reaches the other cm.*
# canonical natives.
#
#   dsomop:cm.effect_estimate  (NEW canonical native; no precomputed OHDSI twin)
#     The FITTED hazard ratio (survival::coxph) or rate ratio (stats::glm Poisson)
#     over a scoped target+comparator pair, with 95% CI and the log-estimate + SE
#     meta-analysis sufficient statistics, alongside the SAME per-arm
#     persons/person-days/outcomes substrate cm_result returns.
#
# THE R-IN-SESSION PRINCIPLE. cm_result stopped at the descriptive substrate + the
# MDRR because the fitted estimate "needs a regression over patient-level survival
# data and is not a descriptive aggregate". But a dsOMOP analysis runs in the
# server-side R session: it is NOT limited to one descriptive SQL query. It can
# LOAD the scoped cohort's MINIMAL time-at-risk frame into R and FIT the model
# there, returning ONLY disclosure-safe aggregates. "Cannot be done as pure SQL"
# is not "cannot be done". This entry is the fitted counterpart cm_result pointed
# at; cm_result keeps its MDRR substrate unchanged and nothing new is overlaid on
# the precomputed registry.
#
# Disclosure. unit="record"; count_cols = c("persons","outcomes","person_days")
# are DECLARED (never pre-gated) so the SINGLE .omopAnalysisGate suppresses + bands
# them, and the generic record branch drops each arm row whose own counts fall
# below threshold. The two arms are self-gated >= nfilter.subset persons by
# .omopTwoPopCohorts BEFORE any pull (fail-closed per-arm). The fitted estimate +
# CI are NOT counts, so the gate cannot suppress them on a count basis; the fn
# therefore applies the explicit non-frame guard the disclosure plan mandates (band
# each arm's outcome count, NA the estimate unless BOTH arms rest on a released
# outcome count AND >= nfilter.subset persons) — mirroring .omopCmMdrr's band_or_na
# and .omopAnalysisReconcileRatio's NA-on-suppressed contract. No per-subject row
# ever leaves R (the survival frame is materialised in-session and discarded); no
# *_source_value is selected; the outcome concept set is descendant-expanded.

#' Build the per-subject time-at-risk + outcome-event frame for ONE arm (in-session)
#'
#' The R-in-session data step for \code{\link{.omopCmEffectFn}}: over one arm's
#' scoped cohort it materialises a MINIMAL per-subject frame — one row per person =
#' \code{(arm, time_at_risk_days, event)} — using the SAME time-at-risk machinery
#' \code{\link{.omopCmResultFn}}'s \code{arm_row} uses (\code{\link{.omopCohortEndDateCol}}
#' for the exit anchor, \code{DATEADD} for the TAR window, \code{\link{.omopDateDiffDays}}
#' for day counts, \code{\link{.omopOutcomeSource}} for the descendant-expanded
#' outcome). Per subject the outcome is reduced to the FIRST event inside the TAR
#' window (\code{MIN} of the per-event day offset); a subject with no in-window
#' event is censored at the full TAR length. The aggregation is GROUP BY subject so
#' the SQL returns one row per person; the per-subject rows are pulled into R and
#' turned into a survival frame there — NO per-subject row ever leaves the session.
#'
#' Time is clamped to \code{[1, full_tar]} days: a zero/negative span (degenerate
#' TAR, or an event on the index day) becomes a single at-risk day so
#' \code{survival::coxph} / the Poisson offset stay defined. The function returns
#' the in-R data frame plus the gated-count substrate (distinct persons,
#' person-days, outcomes) so the caller can reuse \code{.omopCmResultFn}'s arm_row
#' aggregates verbatim as the disclosure basis.
#'
#' @param handle CDM handle.
#' @param cohort Validated arm cohort temp-table name.
#' @param arm Integer 0/1 arm code (0 comparator, 1 target).
#' @param out_src Resolved \code{\link{.omopOutcomeSource}} (or NULL: no outcome).
#' @param tar Named list of TAR params (start/end offset, start/end anchor).
#' @return list(data = <per-subject frame: arm, tar_days, event>,
#'   persons, person_days, outcomes).
#' @keywords internal
.omopCmEffectArmData <- function(handle, cohort, arm, out_src, tar) {
  end_col <- .omopCohortEndDateCol(handle, cohort)
  anchor  <- function(a) if (identical(a, "end")) end_col else "cohort_start_date"
  tar_lo  <- paste0("DATEADD(day, ", tar$start, ", coh.", anchor(tar$anchor_start), ")")
  tar_hi  <- paste0("DATEADD(day, ", tar$end, ", coh.", anchor(tar$anchor_end), ")")
  full_days <- .omopDateDiffDays(handle, tar_hi, tar_lo)

  outcome_join <- ""
  event_days_expr <- "NULL"
  if (!is.null(out_src)) {
    in_window <- paste0("o.", out_src$date_col, " >= ", tar_lo,
                        " AND o.", out_src$date_col, " <= ", tar_hi)
    outcome_join <- paste0(
      " LEFT JOIN ", out_src$table, " o ON o.", out_src$person_col,
      " = coh.subject_id AND o.", out_src$concept_col, " IN (",
      out_src$id_list, ") AND ", in_window)
    # FIRST in-window event per subject, as a day offset from the TAR start.
    event_days_expr <- paste0(
      "MIN(CASE WHEN o.", out_src$concept_col, " IS NOT NULL THEN ",
      .omopDateDiffDays(handle, paste0("o.", out_src$date_col), tar_lo),
      " END)")
  }

  sql <- .sql_translate(paste0(
    "SELECT coh.subject_id AS subject_id, ",
    "MAX(", full_days, ") AS full_days, ",
    event_days_expr, " AS event_days ",
    "FROM ", cohort, " coh", outcome_join,
    " GROUP BY coh.subject_id"),
    handle$target_dialect)
  raw <- .executeQuery(handle, sql)
  if (!is.data.frame(raw) || nrow(raw) == 0) {
    return(list(data = data.frame(arm = integer(0), tar_days = numeric(0),
                                  event = integer(0)),
                persons = 0, person_days = 0, outcomes = 0))
  }

  full <- as.numeric(raw$full_days)
  ev_days <- as.numeric(raw$event_days)
  event <- as.integer(!is.na(ev_days))
  # Censored: full TAR; event: time to first event. Clamp both to [1, full_tar].
  full_clamped <- pmax(full, 1)
  tar_days <- ifelse(event == 1L,
                     pmin(pmax(ev_days, 1), full_clamped),
                     full_clamped)

  list(
    data = data.frame(arm = rep(as.integer(arm), length(tar_days)),
                      tar_days = as.numeric(tar_days),
                      event = event,
                      stringsAsFactors = FALSE),
    persons     = nrow(raw),
    person_days = sum(tar_days, na.rm = TRUE),
    outcomes    = sum(event, na.rm = TRUE))
}

#' Fit the comparative effect model in the R session and extract log-estimate + SE
#'
#' The R-in-session model step: given the two-arm \code{(arm, tar_days, event)}
#' frame materialised by \code{\link{.omopCmEffectArmData}}, fit the comparative
#' model and return the log effect estimate and its standard error (the
#' meta-analysis sufficient statistics es_* synthesis consumes). Two model families
#' (both give a log-RR-scale coefficient + SE):
#' \itemize{
#'   \item \code{cox}: \code{survival::coxph(Surv(tar_days, event) ~ arm)} — the
#'     coefficient is the log hazard ratio. Used when \pkg{survival} is installed
#'     (a recommended, near-ubiquitous package; declared in Suggests).
#'   \item \code{poisson}: \code{stats::glm(event ~ arm + offset(log(tar_days)),
#'     family = poisson)} — the \code{arm} coefficient is the log rate ratio. The
#'     stats-only fallback when \pkg{survival} is absent, and the explicit choice
#'     for \code{model_type = "poisson"}.
#' }
#' The fit happens entirely in the session over the in-R frame; only the scalar
#' \code{log_estimate} + \code{se_log_estimate} are returned (never coefficients
#' per subject, never the frame). Returns \code{NA}s when the model cannot be fit
#' (single arm present, no events, non-finite SE, or fit error) — fail-closed.
#'
#' @param df Two-arm per-subject frame (arm, tar_days, event).
#' @param model_type "cox" or "poisson".
#' @return list(log_estimate, se_log_estimate, model_type) (NA on failure).
#' @keywords internal
.omopCmEffectFit <- function(df, model_type = "cox") {
  na_out <- function(mt) list(log_estimate = NA_real_,
                              se_log_estimate = NA_real_, model_type = mt)
  if (!is.data.frame(df) || nrow(df) == 0) return(na_out(model_type))
  # Both arms must be present and at least one event must occur, else the
  # comparison / the coefficient is undefined.
  if (length(unique(df$arm)) < 2L) return(na_out(model_type))
  if (sum(df$event, na.rm = TRUE) <= 0) return(na_out(model_type))
  df <- df[is.finite(df$tar_days) & df$tar_days > 0, , drop = FALSE]
  if (nrow(df) == 0 || length(unique(df$arm)) < 2L) return(na_out(model_type))

  use_cox <- identical(model_type, "cox") &&
    requireNamespace("survival", quietly = TRUE)
  mt <- if (use_cox) "cox" else "poisson"

  fit <- tryCatch({
    if (use_cox) {
      m <- survival::coxph(survival::Surv(df$tar_days, df$event) ~ df$arm)
      s <- summary(m)$coefficients
      list(coef = unname(s[1, "coef"]), se = unname(s[1, "se(coef)"]))
    } else {
      m <- stats::glm(event ~ arm, family = stats::poisson(),
                      offset = log(df$tar_days), data = df)
      s <- summary(m)$coefficients
      list(coef = unname(s["arm", "Estimate"]),
           se = unname(s["arm", "Std. Error"]))
    }
  }, error = function(e) NULL)

  if (is.null(fit) || !is.finite(fit$coef) || !is.finite(fit$se) || fit$se <= 0) {
    return(na_out(mt))
  }
  list(log_estimate = as.numeric(fit$coef),
       se_log_estimate = as.numeric(fit$se), model_type = mt)
}

#' Build the cm.effect_estimate live compute fn (fitted HR / RR, R-in-session)
#'
#' Over the scoped target+comparator pair (\code{\link{.omopTwoPopCohorts}};
#' \code{max_tables = 2L}, BOTH arms self-gated >= nfilter.subset persons before any
#' pull): build the per-subject \code{(arm, tar_days, event)} frame for each arm
#' (\code{\link{.omopCmEffectArmData}}), bind the two arms in the R session, FIT the
#' comparative model (\code{\link{.omopCmEffectFit}}; Cox hazard ratio, or Poisson
#' rate ratio), and emit the fitted effect + 95\% CI together with the SAME per-arm
#' persons / person-days / outcomes substrate \code{cm_result} returns.
#'
#' The estimate (exp(coef)), ci_lo/ci_hi (exp(coef +/- 1.96 SE)), log_estimate and
#' se_log_estimate ride on BOTH arm rows, exactly as \code{cm_result} carries the
#' MDRR: a person-less estimate row would have an NA distinct-person companion and
#' be dropped by the gate, so the value rides on the two arm rows that survive
#' gating on their OWN person/outcome/person-day counts — i.e. the fitted estimate
#' is released iff BOTH arms it summarises are themselves releasable. The arm
#' counts are declared (not pre-gated); the SINGLE \code{\link{.omopAnalysisGate}}
#' suppresses + bands them, dropping each arm row that falls below threshold.
#'
#' DISCLOSURE — sanctioned non-frame exception. The fitted estimate + CI are NOT
#' counts, so the gate cannot suppress them on a count basis. This fn therefore
#' applies the explicit in-fn guard the disclosure plan mandates for non-frame
#' model outputs BEFORE returning: it bands each arm's outcome count
#' (\code{\link{.bandCount}} at nfilter_band) and sets estimate / ci_lo / ci_hi /
#' log_estimate / se_log_estimate to NA whenever EITHER arm's banded outcome count
#' is NA-or-below nfilter_tab OR EITHER arm has < nfilter.subset persons — mirroring
#' \code{\link{.omopCmMdrr}}'s band_or_na gating and
#' \code{\link{.omopAnalysisReconcileRatio}}'s NA-on-suppressed contract. No
#' per-subject rows ever leave R (the survival frame is materialised in-session and
#' discarded); no \code{*_source_value} is selected.
#'
#' @return A \code{function(handle, ctx, params)} returning an aggregate frame.
#' @keywords internal
.omopCmEffectFn <- function() {
  function(handle, ctx, params) {
    cohorts <- .omopTwoPopCohorts(handle, ctx)
    # No two-population scope -> no comparison -> gate-safe empty frame.
    if (is.null(cohorts)) return(data.frame())

    outcome_id  <- params$outcome_concept_id
    domain_code <- params$outcome_domain_code %||% "0"
    model_type  <- params$model_type %||% "cox"
    if (!model_type %in% c("cox", "poisson")) model_type <- "cox"
    tar <- list(
      start = as.integer(params$tar_start_offset %||% "1"),
      end   = as.integer(params$tar_end_offset %||% "0"),
      anchor_start = params$tar_anchor_start %||% "start",
      anchor_end   = params$tar_anchor_end %||% "end")

    out_src <- .omopOutcomeSource(handle, outcome_id, domain_code)

    # Per-arm in-session data + count substrate (target = 1, comparator = 0).
    ta <- .omopCmEffectArmData(handle, cohorts$a, 1L, out_src, tar)
    co <- .omopCmEffectArmData(handle, cohorts$b, 0L, out_src, tar)

    # Bind both arms in the R session and FIT the comparative model.
    fit_df <- rbind(ta$data, co$data)
    fit <- .omopCmEffectFit(fit_df, model_type)

    log_est <- fit$log_estimate
    se_log  <- fit$se_log_estimate
    estimate <- if (is.finite(log_est)) exp(log_est) else NA_real_
    ci_lo <- if (is.finite(log_est) && is.finite(se_log)) {
      exp(log_est - 1.96 * se_log)
    } else NA_real_
    ci_hi <- if (is.finite(log_est) && is.finite(se_log)) {
      exp(log_est + 1.96 * se_log)
    } else NA_real_

    # --- Sanctioned non-frame guard (band the outcome counts, NA the estimate) -
    # Mirror .omopCmMdrr's band_or_na: band each arm's outcome count and require
    # BOTH arms to rest on a released outcome count (>= nfilter_tab after banding)
    # AND >= nfilter.subset persons before any fitted value is released.
    settings <- .omopDisclosureSettings()
    bw  <- settings$nfilter_band
    thr <- settings$nfilter_tab
    sub <- settings$nfilter_subset
    band_or_na <- function(x) {
      x <- as.numeric(x)
      v <- .bandCount(x, band_width = bw)
      if (is.na(x) || x < thr) NA_real_ else v
    }
    # band_or_na returns NA for a raw count below nfilter_tab, else the FLOORED
    # band (which is 0 when the raw count is in [nfilter_tab, nfilter_band) — a
    # count that survives suppression but is banded away). An estimate may rest on
    # an arm's outcome count ONLY when that banded count is itself releasable
    # (>= nfilter_tab), so require >= thr (a banded 0 is NOT a release).
    releasable_count <- function(x) is.finite(x) && x >= thr
    ta_out_b <- band_or_na(ta$outcomes)
    co_out_b <- band_or_na(co$outcomes)
    arms_releasable <- releasable_count(ta_out_b) && releasable_count(co_out_b) &&
      is.finite(ta$persons) && ta$persons >= sub &&
      is.finite(co$persons) && co$persons >= sub
    if (!arms_releasable) {
      estimate <- NA_real_; ci_lo <- NA_real_; ci_hi <- NA_real_
      log_est  <- NA_real_; se_log <- NA_real_
    }

    # One row per arm carrying the count substrate (declared count_cols; the gate
    # bands them) PLUS the fitted estimate replicated across both rows, so the
    # estimate survives iff BOTH arm rows survive gating on their own counts.
    arm_row <- function(label, a) data.frame(
      arm              = label,
      model_type       = fit$model_type,
      persons          = as.numeric(a$persons),
      person_days      = as.numeric(a$person_days),
      outcomes         = as.numeric(a$outcomes),
      estimate         = as.numeric(estimate),
      ci_lo            = as.numeric(ci_lo),
      ci_hi            = as.numeric(ci_hi),
      log_estimate     = as.numeric(log_est),
      se_log_estimate  = as.numeric(se_log),
      stringsAsFactors = FALSE)

    out <- rbind(arm_row("target", ta), arm_row("comparator", co))
    rownames(out) <- NULL
    out
  }
}

#' \code{dsomop:cm.effect_estimate} entry (CohortMethod fitted HR / RR + CI)
#' @keywords internal
.omopCmEffectEstimateEntry <- function() {
  name <- "dsomop:cm.effect_estimate"
  plot_code <- paste(
    "function(df, params) {",
    "  d <- df[df$arm == 'target', , drop = FALSE]",
    "  ggplot2::ggplot(d, ggplot2::aes(x = arm, y = estimate)) +",
    "    ggplot2::geom_point() +",
    "    ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lo, ymax = ci_hi),",
    "                           width = 0.1) +",
    "    ggplot2::geom_hline(yintercept = 1, linetype = 'dashed') +",
    "    ggplot2::labs(x = NULL, y = 'Effect estimate (HR / RR, 95% CI)')",
    "}", sep = "\n")

  .omopAnalysisEntry(
    name        = name,
    description = paste0("CohortMethod fitted comparative effect (live, ",
                         "R-in-session): the hazard ratio (survival::coxph) or ",
                         "rate ratio (stats::glm Poisson) over a scoped ",
                         "target+comparator pair, with 95% CI and the ",
                         "log-estimate + SE meta-analysis sufficient statistics, ",
                         "alongside the per-arm persons/person-days/outcomes ",
                         "substrate. unit=record; persons is the gated ",
                         "distinct-person companion; the fitted estimate is NA ",
                         "unless BOTH arms are releasable."),
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
      list(name = "model_type", type = "enum", required = FALSE, default = "cox",
           choices = c("cox", "poisson"),
           description = "Model family: cox (hazard ratio via survival::coxph) or poisson (rate ratio via stats::glm). Falls back to poisson when survival is absent.")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = .omopCmEffectFn(),
      plot = list(type = "point", code = plot_code)
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
                 table_name = "cm_effect_estimate")
  )
}

#' \code{dsomop:cm.effect_estimate} canonical entry (fitted effect; no OHDSI twin)
#'
#' Parity wrapper mirroring \code{\link{.omopCmMdrrCanonicalEntry}} so the registrar
#' in \code{.omopAnalysisDiagnosticEntries} appends this exactly like the other cm.*
#' canonical natives. Unlike cm.attrition / cm.mdrr there is NO precomputed OHDSI id
#' this aliases: cm.effect_estimate IS the canonical id (its builder already sets
#' that name and the ohdsi_live adapter label).
#' @keywords internal
.omopCmEffectEstimateCanonicalEntry <- function() {
  .omopCmEffectEstimateEntry()
}
