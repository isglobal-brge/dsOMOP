# Module: OHDSI CohortMethod interaction estimate (native live R-in-session)
#
# Group PLR-4-cm-interaction. The NEW canonical dsomop:cm.interaction_estimate
# OVERLAYING the read-precomputed dsomop:ohdsi.cohort_method.cm_interaction_result.
# OHDSI's interaction result is the treatment effect WITHIN pre-specified
# subgroups (effect modification). This is a LIVE R-in-session analysis (NOT a
# read of a precomputed table): for EACH subgroup level the server-side R session
# restricts BOTH arms of the scoped target+comparator pair to the subjects
# matching that level, LOADS the per-subject Surv/Poisson frame into R (a scoped
# fetch over the covariate source), and FITS a Cox / Poisson model to obtain that
# subgroup's HR/RR + CI. Only disclosure-safe AGGREGATES leave the session: per
# subgroup level a row of per-arm persons / outcomes / person-days plus the fitted
# estimate + CI. No patient-level frame, no individual score, no *_source_value
# ever leaves R.
#
# This file lives ALONGSIDE ohdsi_pack_cohortmethod.R (R sources every R/*.R), so
# its helpers + entry builder + canonical alias are available; the entry is wired
# into .ohdsiPackCohortMethodEntries (the cohort_method group registrar) and its
# canonical short id into .omopAnalysisDiagnosticEntries, exactly as the sibling
# cohort_method ports are. It REUSES the sibling helpers defined next door
# (.omopCmAttritionSteps' EXISTS idiom, .omopCmSpliceBirthDate, .omopCmMdrr's
# kernel siblings) plus the shared analysis-catalog helpers (.omopTwoPopCohorts,
# .omopOutcomeSource, .omopCohortEndDateCol, .omopDateDiffDays, .bandCount,
# .omopDisclosureSettings).
#
# Disclosure (the STRICTEST case — subgroups SHRINK the arms, applied PER ROW):
# every count is declared (never pre-gated) so the SINGLE .omopAnalysisGate
# suppresses + bands it. unit="record" with meta$adapter="ohdsi_live", so the
# gate's GENERIC record branch gates each row on its declared count_cols (both
# arms' persons/outcomes/person-days) and person_id_col (target_persons): a
# subgroup row is DROPPED unless EVERY declared per-arm count clears nfilter.tab.
# Additionally, IN-FN and fail-closed, the estimate + CI (and the log_estimate/se
# behind them) are NA'd whenever EITHER arm's BANDED persons fall below
# nfilter.subset (stricter than the gate's row-drop) so a released estimate always
# rests on two arms that each clear the subset floor. Subgroup levels are capped
# at nfilter.levels.max so a high-cardinality subgroup param cannot enumerate rare
# strata. Per-subject frames are in-session only; subgroup labels are translated
# concept names; no *_source_value is ever selected. Population is the scoped pair
# (target+comparator, max_tables = 2L); requires_cohort stays FALSE (un-scoped is a
# gate-safe empty frame, matching cm_result).

#' Resolve the ordered subgroup levels for the interaction estimate
#'
#' Effect modification is the treatment effect computed SEPARATELY within each
#' level of a pre-specified subgroup. This builds those levels as a list of
#' \code{list(label, predicate)} where \code{predicate} is a SQL boolean over the
#' arm-cohort alias \code{coh} (a correlated EXISTS / person predicate that
#' restricts EITHER arm's cohort to the subjects in that level), exactly the
#' \code{coh.subject_id} idiom \code{\link{.omopCmAttritionSteps}} uses. Three
#' subgroup kinds are supported (one selected by \code{subgroup_kind}):
#' \itemize{
#'   \item \code{"concept"}: a (descendant-expanded) \code{subgroup_concept_id} in
#'     \code{subgroup_domain_code}. Two levels: present (EXISTS) and absent
#'     (NOT EXISTS) — the classic "with vs without the comorbidity" modifier.
#'   \item \code{"sex"}: the two gender levels (male 8507 / female 8532), each a
#'     correlated EXISTS over \code{person}.
#'   \item \code{"age"}: contiguous age-at-index bands from \code{age_breaks} (a
#'     comma list of cut points, e.g. \code{"50,65,80"} -> \code{<50}, \code{50-64},
#'     \code{65-79}, \code{>=80}), each a correlated EXISTS over \code{person} with
#'     the year-of-birth age expression spliced post-translate (the
#'     \code{within_age_bounds} pattern of \code{\link{.omopCmAttritionSteps}}).
#' }
#' The number of levels is CAPPED at \code{nfilter.levels.max} so a high-
#' cardinality subgroup param can never enumerate a long list of rare strata; an
#' empty/invalid spec yields a single "overall" level (no restriction) so the
#' entry degrades to the cm_result substrate rather than erroring.
#'
#' @param handle CDM handle.
#' @param params Sanitized param literals.
#' @return Ordered list of \code{list(label, predicate)} subgroup levels
#'   (\code{predicate} NULL for the "overall" fallback level).
#' @keywords internal
.omopCmInteractionSubgroups <- function(handle, params) {
  cap <- .omopDisclosureSettings()$nfilter_levels_max
  kind <- tolower(params$subgroup_kind %||% "concept")

  concept_exists <- function(concept_id, domain_code, negate) {
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

  levels <- list()

  if (identical(kind, "concept") && !is.null(params$subgroup_concept_id)) {
    dom <- params$subgroup_domain_code %||% "0"
    label <- .omopCmInteractionConceptLabel(handle, params$subgroup_concept_id)
    p_yes <- concept_exists(params$subgroup_concept_id, dom, negate = FALSE)
    p_no  <- concept_exists(params$subgroup_concept_id, dom, negate = TRUE)
    if (!is.null(p_yes)) {
      levels[[length(levels) + 1L]] <- list(
        label = paste0(label, ": yes"), predicate = p_yes)
      levels[[length(levels) + 1L]] <- list(
        label = paste0(label, ": no"), predicate = p_no)
    }

  } else if (identical(kind, "sex")) {
    person <- .qualifyTable(handle, "person")
    sex_exists <- function(gender_id) paste0(
      "EXISTS (SELECT 1 FROM ", person, " p WHERE p.person_id = coh.subject_id ",
      "AND p.gender_concept_id = ", gender_id, ")")
    levels[[1L]] <- list(label = "sex: male",   predicate = sex_exists(8507L))
    levels[[2L]] <- list(label = "sex: female", predicate = sex_exists(8532L))

  } else if (identical(kind, "age")) {
    breaks <- suppressWarnings(as.integer(strsplit(
      as.character(params$age_breaks %||% ""), ",", fixed = TRUE)[[1]]))
    breaks <- sort(unique(breaks[!is.na(breaks)]))
    person <- .qualifyTable(handle, "person")
    # age at the cohort index date, year_of_birth-based (p.birth_dt spliced later).
    age_at  <- .omopDateDiffDays(handle, "coh.cohort_start_date", "p.birth_dt")
    age_yrs <- paste0("(CAST(", age_at, " / 365 AS INTEGER))")
    band_exists <- function(bounds) paste0(
      "EXISTS (SELECT 1 FROM ", person, " p WHERE p.person_id = coh.subject_id ",
      "AND ", paste(bounds, collapse = " AND "), ")")
    if (length(breaks) > 0) {
      lo <- breaks[1]
      levels[[1L]] <- list(label = paste0("age: <", lo),
                           predicate = band_exists(paste0(age_yrs, " < ", lo)))
      if (length(breaks) >= 2) {
        for (i in seq_len(length(breaks) - 1L)) {
          a <- breaks[i]; b <- breaks[i + 1L]
          levels[[length(levels) + 1L]] <- list(
            label = paste0("age: ", a, "-", b - 1L),
            predicate = band_exists(c(paste0(age_yrs, " >= ", a),
                                      paste0(age_yrs, " < ", b))))
        }
      }
      hi <- breaks[length(breaks)]
      levels[[length(levels) + 1L]] <- list(
        label = paste0("age: >=", hi),
        predicate = band_exists(paste0(age_yrs, " >= ", hi)))
    }
  }

  if (length(levels) == 0) {
    # No usable subgroup spec -> a single unrestricted "overall" level so the
    # entry degrades to the cm_result substrate (one estimate over both arms)
    # rather than erroring.
    return(list(list(label = "overall", predicate = NULL)))
  }
  # Cap the level count (defence against a high-cardinality subgroup enumerating
  # rare strata): the SAME server option the dsBase level gate uses, so a
  # subgroup can never out-enumerate a categorical variable.
  if (length(levels) > cap) levels <- levels[seq_len(cap)]
  levels
}

#' Translate a single subgroup concept id to its name (for the level label)
#'
#' The subgroup level label must carry the concept NAME (translation default ON),
#' never the raw id or any source value. Falls back to \code{"concept_<id>"} when
#' the vocabulary lookup is unavailable, mirroring
#' \code{\link{.vocabTranslateColumns}}. Named with an interaction-specific suffix
#' so it never collides with a sibling helper in the cohort_method pack.
#' @keywords internal
.omopCmInteractionConceptLabel <- function(handle, concept_id) {
  id <- suppressWarnings(as.integer(concept_id))[1]
  if (is.na(id)) return("subgroup")
  nm <- tryCatch({
    cs <- .vocabLookupConcepts(handle, id)
    if (is.data.frame(cs) && nrow(cs) > 0) as.character(cs$concept_name[1]) else NA
  }, error = function(e) NA)
  if (is.na(nm) || !nzchar(nm)) paste0("concept_", id) else nm
}

#' Fetch one arm's per-subject Surv/Poisson frame WITHIN a subgroup (in-session)
#'
#' The R-in-session core: for the given arm cohort restricted to a subgroup level,
#' return ONE row per subject with the time-at-risk (person-days over the TAR
#' window) and the number of outcome occurrences in that window. This frame is the
#' input the in-session model is FIT on; it stays in R and is reduced to gated
#' aggregates before anything leaves the session. The outcome is the descendant-
#' expanded \code{out_src} set; an arm with no outcome source yields zero events
#' for every subject (the fitter then returns NA rather than a spurious effect).
#' The subgroup predicate is a boolean over the cohort alias \code{coh}
#' (\code{\link{.omopCmInteractionSubgroups}}); the birth-date splice
#' (\code{\link{.omopCmSpliceBirthDate}}, defined in the cohort_method pack) is
#' applied so an age-band predicate resolves per dialect.
#'
#' @param handle CDM handle.
#' @param cohort Validated arm cohort temp table.
#' @param out_src Outcome source list (\code{\link{.omopOutcomeSource}}) or NULL.
#' @param tar A list with the TAR offsets/anchors (start/end/anchor_start/anchor_end).
#' @param predicate SQL boolean over \code{coh} restricting to the subgroup (or
#'   NULL for the unrestricted "overall" level).
#' @return Data frame (person_days, events) — one row per subject in the subgroup.
#' @keywords internal
.omopCmSubgroupSubjectFrame <- function(handle, cohort, out_src, tar, predicate) {
  end_col <- .omopCohortEndDateCol(handle, cohort)
  anchor <- function(a) if (identical(a, "end")) end_col else "cohort_start_date"
  tar_lo <- paste0("DATEADD(day, ", tar$start, ", coh.", anchor(tar$anchor_start), ")")
  tar_hi <- paste0("DATEADD(day, ", tar$end, ", coh.", anchor(tar$anchor_end), ")")
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
  where <- if (!is.null(predicate)) paste0(" WHERE ", predicate) else ""
  sql <- .omopCmSpliceBirthDate(handle, .sql_translate(paste0(
    "SELECT coh.subject_id AS subject_id, ",
    "MAX(", pdays, ") AS person_days, ",
    out_event_expr, " AS events ",
    "FROM ", cohort, " coh", outcome_join, where,
    " GROUP BY coh.subject_id"),
    handle$target_dialect))
  res <- .executeQuery(handle, sql)
  if (is.null(res) || nrow(res) == 0) {
    return(data.frame(person_days = numeric(0), events = numeric(0)))
  }
  data.frame(person_days = as.numeric(res$person_days),
             events      = as.numeric(res$events),
             stringsAsFactors = FALSE)
}

#' Fit the per-subgroup treatment effect (log-estimate + SE) in-session
#'
#' Given the two arms' per-subject frames for ONE subgroup level, fit the SAME
#' model the OHDSI CohortMethod interaction result reports and return the
#' log-scale effect + its standard error (the caller exponentiates + bands). Two
#' families:
#' \itemize{
#'   \item \code{"poisson"} (always available; \pkg{stats} is a hard dep): a
#'     Poisson rate-ratio GLM \code{glm(events ~ arm + offset(log(person_days)),
#'     family = poisson)}. The \code{arm} coefficient IS the log rate-ratio; its
#'     SE comes straight from the fit.
#'   \item \code{"cox"}: a Cox proportional-hazards model
#'     \code{survival::coxph(Surv(person_days, events > 0) ~ arm)} when the
#'     \pkg{survival} package is installed; the \code{arm} coefficient is the log
#'     hazard-ratio. If \pkg{survival} is NOT installed it FALLS BACK to the
#'     Poisson fit (same log-RR interpretation) rather than failing, so the entry
#'     runs in a minimal install.
#' }
#' The fit is wrapped in \code{tryCatch}: a separable / non-converging subgroup
#' (e.g. zero events in an arm) returns \code{NA} log-estimate + SE, which the
#' caller surfaces as an NA estimate/CI — never a spurious number.
#'
#' @param target_frame,comparator_frame Per-subject frames
#'   (\code{\link{.omopCmSubgroupSubjectFrame}}) for the two arms.
#' @param model_type Character; "poisson" (default) or "cox".
#' @return list(log_estimate, se_log_estimate) — \code{NA_real_} when unfittable.
#' @keywords internal
.omopCmFitSubgroupEffect <- function(target_frame, comparator_frame,
                                     model_type = "poisson") {
  na_fit <- list(log_estimate = NA_real_, se_log_estimate = NA_real_)
  nt <- nrow(target_frame); nc <- nrow(comparator_frame)
  if (nt == 0 || nc == 0) return(na_fit)

  df <- data.frame(
    arm = c(rep(1L, nt), rep(0L, nc)),  # 1 = target, 0 = comparator (reference)
    events = c(target_frame$events, comparator_frame$events),
    person_days = c(target_frame$person_days, comparator_frame$person_days),
    stringsAsFactors = FALSE)
  # Person-days must be positive for the offset / time-to-event; clamp a
  # zero/negative TAR to one day (a single at-risk day) so the fit is defined.
  df$person_days[is.na(df$person_days) | df$person_days < 1] <- 1
  df$events[is.na(df$events)] <- 0
  # The effect is unidentified without at least one event; bail to NA.
  if (sum(df$events) <= 0) return(na_fit)

  use_cox <- identical(tolower(model_type %||% "poisson"), "cox") &&
    requireNamespace("survival", quietly = TRUE)

  fit_res <- tryCatch({
    if (use_cox) {
      surv <- survival::Surv(time = df$person_days,
                             event = as.integer(df$events > 0))
      fit  <- survival::coxph(surv ~ arm, data = df)
      sm   <- summary(fit)$coefficients
      list(log_estimate = as.numeric(sm["arm", "coef"]),
           se_log_estimate = as.numeric(sm["arm", "se(coef)"]))
    } else {
      fit <- stats::glm(events ~ arm + offset(log(person_days)),
                        family = stats::poisson(), data = df)
      sm  <- summary(fit)$coefficients
      list(log_estimate = as.numeric(sm["arm", "Estimate"]),
           se_log_estimate = as.numeric(sm["arm", "Std. Error"]))
    }
  }, error = function(e) na_fit)

  if (is.null(fit_res) || !is.finite(fit_res$log_estimate %||% NA) ||
      !is.finite(fit_res$se_log_estimate %||% NA)) {
    return(na_fit)
  }
  fit_res
}

#' Build the cm_interaction_result live compute fn
#'
#' Over the scoped target+comparator pair (\code{ctx$scoped_cohorts}; both arms
#' self-gated by \code{\link{.omopTwoPopCohorts}}) and a subgroup definition
#' (\code{\link{.omopCmInteractionSubgroups}}): for EACH subgroup level, restrict
#' BOTH arms to the subjects in that level, LOAD each arm's per-subject
#' Surv/Poisson frame into R (\code{\link{.omopCmSubgroupSubjectFrame}}), FIT the
#' Cox/Poisson model (\code{\link{.omopCmFitSubgroupEffect}}) for that level's
#' HR/RR, and emit ONE row: \code{subgroup_label}, per-arm
#' persons/outcomes/person-days, \code{estimate} (exp(log-estimate)), \code{ci_lo},
#' \code{ci_hi}, \code{log_estimate}, \code{se_log_estimate}.
#'
#' DISCLOSURE (the strictest case — subgroups shrink the arms, applied PER ROW):
#' each row carries BOTH arms' distinct-person counts
#' (\code{target_persons}/\code{comparator_persons}) declared as count_cols so the
#' ONE gate small-cell-suppresses + bands them and DROPS any subgroup row where
#' EITHER arm is below threshold; \code{person_id_col} is \code{target_persons}.
#' Additionally, IN-FN and fail-closed, the estimate + CI (and the underlying
#' log_estimate/se) are NA'd whenever EITHER arm's BANDED persons fall below
#' \code{nfilter.subset} (stricter than the gate's nfilter.tab row-drop) OR EITHER
#' arm's BANDED outcome count is not releasable (the se_log = sqrt(1/O_t + 1/O_c)
#' closed form would otherwise re-derive a tiny outcome count from the released
#' SE/CI, defeating the gate's banding of the outcome columns), so a released
#' estimate always rests on two arms that each clear the subset floor AND a full
#' outcome band — exactly the .omopCmEffectFn (PLR-1) non-frame guard.
#' Per-arm outcomes + person-days are also declared count_cols (banded).
#' Per-subject frames live in-session only; subgroup labels are concept names (no
#' \code{*_source_value}); the level count is capped (nfilter.levels.max).
#'
#' @return A \code{function(handle, ctx, params)} returning an aggregate frame.
#' @keywords internal
.omopCmInteractionFn <- function() {
  function(handle, ctx, params) {
    cohorts <- .omopTwoPopCohorts(handle, ctx)
    # The interaction estimate compares two arms within subgroups; without a
    # two-population scope there is nothing to compare. Gate-safe empty frame.
    if (is.null(cohorts)) return(data.frame())

    outcome_id  <- params$outcome_concept_id
    domain_code <- params$outcome_domain_code %||% "0"
    model_type  <- tolower(params$model_type %||% "poisson")
    alpha     <- suppressWarnings(as.numeric(params$alpha %||% "0.05"))
    if (is.na(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05
    tar <- list(
      start = as.integer(params$tar_start_offset %||% "1"),
      end   = as.integer(params$tar_end_offset %||% "0"),
      anchor_start = params$tar_anchor_start %||% "start",
      anchor_end   = params$tar_anchor_end %||% "end")

    out_src    <- .omopOutcomeSource(handle, outcome_id, domain_code)
    subgroups  <- .omopCmInteractionSubgroups(handle, params)
    settings   <- .omopDisclosureSettings()
    z          <- stats::qnorm(1 - alpha / 2)

    rows <- list()
    for (sg in subgroups) {
      # Per-arm in-session frame + aggregate counts WITHIN the subgroup. The
      # counts are declared (the ONE gate bands them); the frames feed the fit.
      counts <- function(cohort) {
        fr <- .omopCmSubgroupSubjectFrame(handle, cohort, out_src, tar,
                                          sg$predicate)
        list(frame       = fr,
             persons     = nrow(fr),
             outcomes    = sum(fr$events, na.rm = TRUE),
             person_days = sum(fr$person_days, na.rm = TRUE))
      }
      ta <- counts(cohorts$a)
      co <- counts(cohorts$b)

      # Fit the per-subgroup effect IN-SESSION on the per-subject frames.
      fit <- .omopCmFitSubgroupEffect(ta$frame, co$frame, model_type = model_type)

      # Fail-closed estimate release (mirrors .omopCmEffectFn's non-frame guard):
      # NA the estimate + CI unless BOTH arms clear two floors.
      #   (1) BANDED person count >= nfilter.subset (stricter than the gate's
      #       nfilter.tab row-drop): an estimate over an arm that bands below the
      #       subset floor is never released.
      #   (2) BANDED outcome count releasable (>= nfilter.tab AFTER banding, so a
      #       raw count in [nfilter.tab, nfilter.band) that bands to 0 is NOT a
      #       release). This is essential, not cosmetic: the fitted log-estimate's
      #       standard error has the closed form se_log = sqrt(1/O_t + 1/O_c)
      #       (Poisson) / is pinned to the event total (Cox), so a RELEASED se_log
      #       (and the CI built from it) over a tiny outcome count re-derives that
      #       count EXACTLY (e.g. se_log = 0.7071 => O_t = O_c = 4) — defeating the
      #       gate's banding of target_outcomes/comparator_outcomes. Requiring each
      #       arm's outcome count to clear a full band before any estimate/CI/SE
      #       leaves R closes that recovery, exactly as PLR-1 does.
      band_or_na <- function(x) {
        x <- as.numeric(x)
        v <- .bandCount(x, band_width = settings$nfilter_band)
        if (is.na(x) || x < settings$nfilter_tab) NA_real_ else v
      }
      releasable_count <- function(x) is.finite(x) && x >= settings$nfilter_tab
      ta_band  <- .bandCount(ta$persons, band_width = settings$nfilter_band)
      co_band  <- .bandCount(co$persons, band_width = settings$nfilter_band)
      ta_out_b <- band_or_na(ta$outcomes)
      co_out_b <- band_or_na(co$outcomes)
      arms_ok <- !is.na(ta_band) && !is.na(co_band) &&
        ta_band >= settings$nfilter_subset && co_band >= settings$nfilter_subset &&
        releasable_count(ta_out_b) && releasable_count(co_out_b)

      log_est <- fit$log_estimate
      se_log  <- fit$se_log_estimate
      if (!arms_ok || is.na(log_est) || is.na(se_log)) {
        estimate <- NA_real_; ci_lo <- NA_real_; ci_hi <- NA_real_
        log_est  <- NA_real_; se_log <- NA_real_
      } else {
        estimate <- exp(log_est)
        ci_lo    <- exp(log_est - z * se_log)
        ci_hi    <- exp(log_est + z * se_log)
      }

      rows[[length(rows) + 1L]] <- data.frame(
        subgroup_label         = sg$label,
        target_persons         = as.numeric(ta$persons),
        comparator_persons     = as.numeric(co$persons),
        target_outcomes        = as.numeric(ta$outcomes),
        comparator_outcomes    = as.numeric(co$outcomes),
        target_person_days     = as.numeric(ta$person_days),
        comparator_person_days = as.numeric(co$person_days),
        estimate               = as.numeric(estimate),
        ci_lo                  = as.numeric(ci_lo),
        ci_hi                  = as.numeric(ci_hi),
        log_estimate           = as.numeric(log_est),
        se_log_estimate        = as.numeric(se_log),
        stringsAsFactors = FALSE)
    }
    if (length(rows) == 0) return(data.frame())
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }
}

#' \code{dsomop:ohdsi.cohort_method.cm_interaction_result} entry (effect modification)
#' @keywords internal
.omopCmInteractionEntry <- function() {
  name <- "dsomop:ohdsi.cohort_method.cm_interaction_result"
  plot_code <- paste(
    "function(df, params) {",
    "  d <- df[!is.na(df$estimate), , drop = FALSE]",
    "  ggplot2::ggplot(d, ggplot2::aes(x = subgroup_label, y = estimate)) +",
    "    ggplot2::geom_point() +",
    "    ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lo, ymax = ci_hi),",
    "                           width = 0.2) +",
    "    ggplot2::geom_hline(yintercept = 1, linetype = 'dashed') +",
    "    ggplot2::coord_flip() +",
    "    ggplot2::labs(x = 'Subgroup', y = 'Effect estimate (HR/RR)')",
    "}", sep = "\n")

  .omopAnalysisEntry(
    name        = name,
    description = paste0("CohortMethod interaction estimate (live, R-in-session): ",
                         "the treatment effect WITHIN each level of a pre-specified ",
                         "subgroup (effect modification). For each subgroup level ",
                         "both arms of the scoped target+comparator pair are ",
                         "restricted to the level, the per-subject Surv/Poisson ",
                         "frame is loaded into R, and a Cox/Poisson model is fit for ",
                         "that level's HR/RR + CI. One row per level: per-arm ",
                         "persons/outcomes/person-days, estimate, ci_lo, ci_hi, ",
                         "log_estimate, se_log_estimate. unit=record; a row is ",
                         "dropped and its estimate NA'd unless BOTH arms clear the ",
                         "per-arm person threshold (strictest subgroup case)."),
    domain      = "general",
    params      = list(
      list(name = "outcome_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Outcome concept id (descendants expanded)."),
      list(name = "outcome_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4"),
           description = "Outcome domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "subgroup_kind", type = "enum", required = FALSE,
           default = "concept", choices = c("concept", "sex", "age"),
           description = "Subgroup definition kind: a concept presence/absence, sex, or age bands."),
      list(name = "subgroup_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Modifier concept id (subgroup_kind=concept; descendants expanded). Levels: present / absent."),
      list(name = "subgroup_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4"),
           description = "Domain of subgroup_concept_id."),
      list(name = "age_breaks", type = "string", required = FALSE, default = NULL,
           description = "Comma-separated age cut points (subgroup_kind=age), e.g. '50,65,80'."),
      list(name = "tar_start_offset", type = "int", required = FALSE,
           default = "1"),
      list(name = "tar_end_offset", type = "int", required = FALSE,
           default = "0"),
      list(name = "tar_anchor_start", type = "enum", required = FALSE,
           default = "start", choices = c("start", "end")),
      list(name = "tar_anchor_end", type = "enum", required = FALSE,
           default = "end", choices = c("start", "end")),
      list(name = "model_type", type = "enum", required = FALSE,
           default = "poisson", choices = c("poisson", "cox"),
           description = "In-session model family: Poisson rate-ratio GLM (default) or Cox PH (needs the survival package; falls back to Poisson)."),
      list(name = "alpha", type = "number", required = FALSE, default = "0.05",
           description = "Two-sided alpha for the per-subgroup confidence interval.")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = .omopCmInteractionFn(),
      plot = list(type = "point", code = plot_code)
    ),
    dependencies = list(
      tables = c("person", "condition_occurrence", "concept"),
      packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit          = "record",
      count_cols    = c("target_persons", "comparator_persons",
                        "target_outcomes", "comparator_outcomes",
                        "target_person_days", "comparator_person_days"),
      person_id_col = "target_persons"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 2L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "ohdsi_live", tool_id = "cohort_method",
                 table_name = "cm_interaction_result")
  )
}

#' \code{dsomop:cm.interaction_estimate} canonical alias of the live
#' cm_interaction_result effect-modification entry
#'
#' The new canonical short id in the dsomop:cm.* namespace pointing at the SAME
#' live compute as the stable OHDSI id above (same compute$fn / disclosure /
#' scope; only the name + adapter label differ). Registered by
#' \code{.omopAnalysisDiagnosticEntries} (the single place that owns the
#' dsomop:cm.* family), appended after the requires_cohort forcing pass so it
#' keeps its twin's scope (un-scoped -> gate-safe empty frame, like cm.mdrr).
#' @keywords internal
.omopCmInteractionCanonicalEntry <- function() {
  e <- .omopCmInteractionEntry()
  e$name <- "dsomop:cm.interaction_estimate"
  e$meta$adapter <- "ohdsi_live"
  e
}
