# Module: OHDSI PatientLevelPrediction result-table ports (PLP pack)
#
# Native live-computing replacements for the OHDSI "B2" Patient-Level Prediction
# result table whose metric the federation CAN reproduce directly from the CDM.
# The legacy OHDSI adapter (.omopAnalysisOhdsiEntries) emitted one entry per
# (tool, table) that READ a precomputed plp_* results table via .ohdsiGetResults.
# This pack keeps the SAME id (dsomop:ohdsi.plp.plp_attrition) so existing
# references keep resolving, but its compute now COMPUTES the population-
# construction attrition LIVE from the CDM (NO precomputed plp_attrition table is
# read anywhere in this group).
#
# Scope of PLP, all ported LIVE (the R-in-session principle: a dsOMOP analysis
# runs in the server-side R session, so it is NOT limited to one SQL query — it
# can LOAD the scoped cohort into R and FIT a model there, returning ONLY
# disclosure-safe aggregates):
#   * plp_attrition         -> ported here (B2): persons/outcomes remaining per
#                              population-construction step (the attrition kernel
#                              shape, identical to CohortMethod cm_attrition).
#   * plp_covariate_summary -> handled as a B1 alias to dsomop:plp.covariate_summary
#                              (ohdsi_pack_aliases.R); it needs no model either.
#   * plp_performances / plp_calibration_summary / plp_threshold_summary /
#     plp_diagnostic_summary -> ported here (B2, FITTED MODEL) as live kind="r"
#     natives. The legacy header's claim ("needs a per-subject PREDICTED-RISK
#     column the federation cannot produce") is overturned by the R-in-session
#     principle: the prediction model CAN be fit in the Rock session. One shared
#     in-session kernel (.omopPlpFitModel) reuses the plp_attrition population
#     construction to define the scoped TAR population + binary outcome, builds a
#     FeatureExtraction-style binary covariate design matrix in R (top-K prevalent
#     covariates -> well-posed; stats::glm only, no new dep), fits
#     stats::glm(outcome ~ ., binomial), and the four canonical natives below emit
#     ONLY disclosure-safe AGGREGATES from the in-session predicted risk
#     (AUROC/AUPRC scalars; calibration deciles; a coarse threshold grid; the
#     attrition-derived diagnostic counts). The design matrix, coefficients and
#     per-subject predicted risks NEVER leave the session.
#   * plp_model_design -> config-only (the model's covariate-settings /
#     population-settings spec, no per-person metric); REFERENCE-ONLY, left on the
#     legacy read-precomputed adapter (untouched here).
#
# Each canonical metric lives in a native dsomop:plp.* entry (added to
# .omopAnalysisDiagnosticEntries); the legacy OHDSI ids are THIN DELEGATES to them
# (same pattern as ohdsi_pack_aliases.R) so the live compute/disclosure/params/
# scope live in exactly one place and the OHDSI id inherits them. No second gate is
# added: every delegated frame flows through the SINGLE .omopAnalysisGate exactly
# as its canonical id does.
#
# DISCLOSURE (predicted-risk model outputs — the propensity/risk-score rule from
# the task): individual predicted risks are NEVER emitted, only gated summary /
# decile / threshold bands. Calibration + threshold tables are unit="record" with
# a banded bin-n companion person column (the gate drops sparse deciles/thresholds
# below nfilter and bands the cell counts), and EVERY observed-rate / sensitivity /
# specificity / PPV is reconciled from BANDED numerator+denominator via
# .omopAnalysisReconcileRatio (no raw ratio over banded counts). AUROC / AUPRC /
# events-per-variable are scalars: NA'd in-fn whenever the population or event
# count is below nfilter (explicit non-frame guard). Coefficients are individually
# influenced and out of scope (covariate prevalence is already served by
# dsomop:plp.covariate_summary), so they are discarded. No *_source_value;
# covariate / outcome concepts translated.

# --- Canonical attrition kernel ----------------------------------------------

#' Population-construction attrition over a scoped cohort + outcome (kernel)
#'
#' The attrition kernel shared by the PatientLevelPrediction target-population
#' construction (and structurally identical to CohortMethod's \code{cm_attrition}):
#' starting from the scoped cohort it applies the standard sequential PLP
#' population filters and reports, AT EACH STEP, the distinct persons remaining
#' and the distinct persons remaining who experience the outcome inside the
#' time-at-risk window. Every step is a per-subject filter, so each \code{n_persons}
#' is a distinct-person count and each \code{n_outcomes} is a distinct-person
#' (outcome-positive) count over the SAME population — the gate's generic
#' record branch then gates \code{n_outcomes} on its sibling person column and
#' bands both. No model and no predicted risk is involved.
#'
#' Steps (each a superset filter of the next):
#' \enumerate{
#'   \item \code{initial_cohort} — all subjects in the scoped cohort.
#'   \item \code{in_observation_at_index} — subjects whose cohort index date lies
#'     within an \code{observation_period} (PLP requires observation at index).
#'   \item \code{sufficient_time_at_risk} — subjects (from step 2) whose full
#'     time-at-risk window (\code{index + tar_start} .. \code{index + tar_end}
#'     days) ends on/before their observation-period end (followed for the whole
#'     TAR; the standard PLP \code{requireTimeAtRisk}).
#' }
#'
#' The whole frame is produced by ONE query: a per-subject derived table tags
#' each subject with \code{in_obs}, \code{has_tar}, and \code{has_outcome} (a
#' correlated EXISTS over the outcome table within the TAR window), and the outer
#' query emits one row per step via conditional \code{COUNT(DISTINCT ...)}. The
#' result is AGGREGATE-ONLY (per-step counts; no person key). The fn self-gates
#' the scoped population BEFORE materialising and returns the raw frame UN-gated
#' (the single .omopAnalysisGate suppresses/bands/couples it).
#'
#' @keywords internal
.omopPlpAttrition <- function() {
  name <- "dsomop:plp.attrition"
  plot_code <- paste(
    "function(df, params) {",
    "  df$step <- factor(df$step, levels = df$step)",
    "  ggplot2::ggplot(df, ggplot2::aes(x = step, y = n_persons)) +",
    "    ggplot2::geom_col() + ggplot2::coord_flip() +",
    "    ggplot2::labs(x = 'Population-construction step', y = 'Persons remaining')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    # The scoped cohort IS the target population; un-scoped has nothing to
    # attrit, so return a gate-safe empty frame (requires_cohort makes an
    # un-scoped run a clear error before we ever get here).
    if (is.null(ctx$scoped_cohort)) return(data.frame())
    cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
    obs    <- .qualifyTable(handle, "observation_period")

    tar_start <- as.integer(params$tar_start_offset %||% "0")
    tar_end   <- as.integer(params$tar_end_offset %||% "365")
    out_src   <- .omopOutcomeSource(handle, params$outcome_concept_id,
                                    params$outcome_domain_code %||% "0")

    # Self-gate the scoped target population BEFORE pulling any rows into R.
    .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
      "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
      handle$target_dialect))

    # TAR window bounds, index-relative (DATEADD is dialect-translated by
    # .sql_translate, exactly as the incidence/follow-up TAR diagnostics do).
    tar_lo <- paste0("DATEADD(day, ", tar_start, ", c.cohort_start_date)")
    tar_hi <- paste0("DATEADD(day, ", tar_end, ", c.cohort_start_date)")

    # Outcome-positive flag: >= 1 outcome record inside the TAR window. With no
    # outcome concept supplied there is no outcome to count, so the flag is 0 for
    # everyone (n_outcomes columns come back 0 and the gate suppresses them).
    has_outcome_expr <- if (is.null(out_src)) {
      "0"
    } else {
      paste0(
        "(CASE WHEN EXISTS (SELECT 1 FROM ", out_src$table, " o WHERE o.",
        out_src$person_col, " = c.subject_id AND o.", out_src$concept_col,
        " IN (", out_src$id_list, ") AND o.", out_src$date_col, " >= ", tar_lo,
        " AND o.", out_src$date_col, " <= ", tar_hi, ") THEN 1 ELSE 0 END)")
    }

    # Per-subject derived table: in_obs (index inside an observation_period),
    # has_tar (TAR end on/before observation-period end, among in_obs subjects),
    # has_outcome (outcome inside TAR). Correlated EXISTS keep it per-subject.
    per_subject <- paste0(
      "SELECT c.subject_id, ",
      "(CASE WHEN EXISTS (SELECT 1 FROM ", obs, " op WHERE op.person_id = ",
      "c.subject_id AND op.observation_period_start_date <= c.cohort_start_date ",
      "AND op.observation_period_end_date >= c.cohort_start_date) ",
      "THEN 1 ELSE 0 END) AS in_obs, ",
      "(CASE WHEN EXISTS (SELECT 1 FROM ", obs, " op WHERE op.person_id = ",
      "c.subject_id AND op.observation_period_start_date <= c.cohort_start_date ",
      "AND op.observation_period_end_date >= ", tar_hi, ") ",
      "THEN 1 ELSE 0 END) AS has_tar, ",
      has_outcome_expr, " AS has_outcome ",
      "FROM ", cohort, " c")

    # One row per construction step via conditional distinct-person counts. Each
    # step is a superset filter of the next (initial >= in_obs >= in_obs&has_tar).
    step_counts <- function(step_no, label, keep_pred) paste0(
      "SELECT ", step_no, " AS step_order, '", label, "' AS step, ",
      "COUNT(DISTINCT CASE WHEN ", keep_pred, " THEN s.subject_id END) ",
      "AS n_persons, ",
      "COUNT(DISTINCT CASE WHEN (", keep_pred,
      ") AND s.has_outcome = 1 THEN s.subject_id END) AS n_outcomes ",
      "FROM (", per_subject, ") s")

    sql <- .sql_translate(paste0(
      step_counts(1, "initial_cohort", "1 = 1"),
      " UNION ALL ",
      step_counts(2, "in_observation_at_index", "s.in_obs = 1"),
      " UNION ALL ",
      step_counts(3, "sufficient_time_at_risk",
                  "s.in_obs = 1 AND s.has_tar = 1"),
      " ORDER BY step_order"),
      handle$target_dialect)

    .executeQuery(handle, sql)
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Patient-Level Prediction target-population attrition: ",
                        "distinct persons (and outcome-positive persons within ",
                        "the time-at-risk window) remaining after each ",
                        "population-construction step over the scoped cohort. ",
                        "Computed live from the CDM; no fitted model required."),
    domain      = "general",
    params      = list(
      list(name = "outcome_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Outcome concept id (descendants expanded) counted within the time-at-risk window."),
      list(name = "outcome_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4"),
           description = "Outcome domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "tar_start_offset", type = "int", required = FALSE, default = "0",
           description = "Time-at-risk start offset (days from cohort index)."),
      list(name = "tar_end_offset", type = "int", required = FALSE, default = "365",
           description = "Time-at-risk end offset (days from cohort index).")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "bar", code = plot_code)
    ),
    dependencies = list(tables = c("observation_period", "condition_occurrence"),
                        packages = character(0)),
    # unit="record": n_outcomes is a record-derived (outcome-positive) person
    # count that can rest on too few persons, so the gate's GENERIC record branch
    # (meta$adapter="diagnostic") gates it on its declared sibling person column
    # n_persons and bands both. n_persons is itself a distinct-person count, so it
    # is small-cell suppressed + banded by the universal final pass.
    disclosure = .omopAnalysisDisclosure(
      unit = "record", count_cols = c("n_persons", "n_outcomes"),
      person_id_col = "n_persons"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Fitted-model kernel (R-in-session) --------------------------------------

#' Parameter specs shared by the four fitted-model PLP entries
#'
#' The model is fit over the scoped cohort's PatientLevelPrediction target
#' population (same construction as \code{\link{.omopPlpAttrition}}: in observation
#' at index, sufficient time-at-risk) with a binary outcome inside the TAR window.
#' \code{covariate_domain_code} selects the FeatureExtraction-style binary
#' covariate family; \code{max_covariates} caps the design matrix at the top-K most
#' prevalent covariates so \code{stats::glm} stays well-posed (a ridge-style
#' restriction; stats only, no new dependency). All day offsets are literal ints.
#'
#' @return List of parameter specs.
#' @keywords internal
.omopPlpModelParams <- function() {
  list(
    list(name = "outcome_concept_id", type = "concept_id", required = FALSE,
         default = NULL,
         description = "Outcome concept id (descendants expanded); the binary prediction target inside the time-at-risk window."),
    list(name = "outcome_domain_code", type = "enum", required = FALSE,
         default = "0", choices = c("0", "1", "2", "3", "4"),
         description = "Outcome domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
    list(name = "covariate_domain_code", type = "enum", required = FALSE,
         default = "0", choices = c("0", "1", "2", "3", "4"),
         description = "Predictor (covariate) domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
    list(name = "max_covariates", type = "int", required = FALSE, default = "20",
         description = "Cap on the number of top-prevalence binary predictors in the design matrix (keeps the GLM well-posed)."),
    list(name = "tar_start_offset", type = "int", required = FALSE, default = "1",
         description = "Time-at-risk start offset (days from cohort index)."),
    list(name = "tar_end_offset", type = "int", required = FALSE, default = "365",
         description = "Time-at-risk end offset (days from cohort index).")
  )
}

#' Fit a Patient-Level Prediction model in the server-side R session (kernel)
#'
#' The single R-in-session kernel shared by the four fitted-model PLP entries.
#' Following the R-in-session principle it does NOT try to express prediction as
#' one SQL query: it LOADS the scoped target population's design matrix into R and
#' fits the model there, returning ONLY the in-session vectors the aggregate
#' emitters need (predicted risk, observed outcome, predictor count, population /
#' outcome counts). Steps:
#' \enumerate{
#'   \item Define the PLP target population exactly as \code{\link{.omopPlpAttrition}}
#'     does (in observation at index AND sufficient time-at-risk), with a binary
#'     outcome = >= 1 outcome record inside the TAR window. Self-gates that
#'     population with \code{\link{.assertMinPersons}} BEFORE pulling any rows.
#'   \item Pull, per retained subject, the outcome flag plus a 0/1 indicator for
#'     each of the top-K most prevalent binary covariates in the chosen domain
#'     (one correlated COUNT per covariate, pivoted to one column per concept in R).
#'   \item Drop zero-variance / collinear-degenerate predictor columns and fit
#'     \code{stats::glm(outcome ~ ., family = binomial)} (top-K restriction is the
#'     ridge-style well-posedness control; stats only). Predicted risk is the
#'     fitted response.
#' }
#' The design matrix, the fitted coefficients and the per-subject predicted risks
#' are all in-session ONLY — this kernel returns them to the SAME-session aggregate
#' emitters, which release nothing per-subject. Returns \code{NULL} when there is
#' no scope, no outcome, an empty/degenerate population, or the model cannot be fit
#' (the caller then emits a gate-safe empty/NA frame, fail-closed).
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param params Sanitized params (see \code{\link{.omopPlpModelParams}}).
#' @return list(risk, outcome, n_predictors, population_size, n_outcomes,
#'   covariate_ids) or NULL.
#' @keywords internal
.omopPlpFitModel <- function(handle, ctx, params) {
  if (is.null(ctx$scoped_cohort)) return(NULL)
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  obs    <- .qualifyTable(handle, "observation_period")

  out_src <- .omopOutcomeSource(handle, params$outcome_concept_id,
                                params$outcome_domain_code %||% "0")
  # No outcome -> no binary target to predict; fail-closed (NULL).
  if (is.null(out_src)) return(NULL)

  cov_src   <- .omopCovariateSource(handle, params$covariate_domain_code %||% "0")
  max_cov   <- max(as.integer(params$max_covariates %||% "20"), 1L)
  tar_start <- as.integer(params$tar_start_offset %||% "1")
  tar_end   <- as.integer(params$tar_end_offset %||% "365")

  # Self-gate the scoped target population BEFORE pulling any rows into R.
  .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
    handle$target_dialect))

  tar_lo <- paste0("DATEADD(day, ", tar_start, ", c.cohort_start_date)")
  tar_hi <- paste0("DATEADD(day, ", tar_end, ", c.cohort_start_date)")

  # PLP target population = in observation at index AND sufficient TAR (same
  # filters as the attrition kernel's final step). One row per retained subject
  # with the binary outcome (>= 1 outcome record inside the TAR window).
  pop_sql <- paste0(
    "SELECT c.subject_id, ",
    "(CASE WHEN EXISTS (SELECT 1 FROM ", out_src$table, " o WHERE o.",
    out_src$person_col, " = c.subject_id AND o.", out_src$concept_col, " IN (",
    out_src$id_list, ") AND o.", out_src$date_col, " >= ", tar_lo,
    " AND o.", out_src$date_col, " <= ", tar_hi, ") THEN 1 ELSE 0 END) AS outcome ",
    "FROM ", cohort, " c ",
    "WHERE EXISTS (SELECT 1 FROM ", obs, " op WHERE op.person_id = c.subject_id ",
    "AND op.observation_period_start_date <= c.cohort_start_date ",
    "AND op.observation_period_end_date >= c.cohort_start_date) ",
    "AND EXISTS (SELECT 1 FROM ", obs, " op2 WHERE op2.person_id = c.subject_id ",
    "AND op2.observation_period_start_date <= c.cohort_start_date ",
    "AND op2.observation_period_end_date >= ", tar_hi, ")")

  pop <- .executeQuery(handle, .sql_translate(pop_sql, handle$target_dialect))
  if (!is.data.frame(pop) || nrow(pop) == 0) return(NULL)
  pop$subject_id <- suppressWarnings(as.integer(pop$subject_id))
  pop$outcome    <- suppressWarnings(as.integer(pop$outcome))
  pop <- pop[!is.na(pop$subject_id) & !is.na(pop$outcome), , drop = FALSE]
  if (nrow(pop) == 0) return(NULL)

  # Top-K most prevalent binary covariates over the target population (distinct
  # persons per concept). Restricting to top-K is the ridge-style well-posedness
  # control. The pop SELECT is reused as a derived table to scope the covariates.
  cov_sql <- paste0(
    "SELECT e.", cov_src$concept_col, " AS covariate_id, ",
    "COUNT(DISTINCT e.", cov_src$person_col, ") AS n_persons ",
    "FROM ", cov_src$table, " e ",
    "INNER JOIN (", pop_sql, ") p ON p.subject_id = e.", cov_src$person_col,
    " GROUP BY e.", cov_src$concept_col,
    " ORDER BY n_persons DESC")
  cov_top <- .executeQuery(handle, .sql_translate(cov_sql, handle$target_dialect))
  if (!is.data.frame(cov_top) || nrow(cov_top) == 0) return(NULL)
  cov_top$covariate_id <- suppressWarnings(as.integer(cov_top$covariate_id))
  cov_top <- cov_top[!is.na(cov_top$covariate_id), , drop = FALSE]
  if (nrow(cov_top) > max_cov) cov_top <- cov_top[seq_len(max_cov), , drop = FALSE]
  cov_ids <- cov_top$covariate_id
  if (length(cov_ids) == 0) return(NULL)

  # Per-(subject, covariate) presence over the target population, pulled long and
  # pivoted to a 0/1 design matrix in R (one column per kept covariate concept).
  pres_sql <- paste0(
    "SELECT DISTINCT e.", cov_src$person_col, " AS subject_id, e.",
    cov_src$concept_col, " AS covariate_id ",
    "FROM ", cov_src$table, " e ",
    "INNER JOIN (", pop_sql, ") p ON p.subject_id = e.", cov_src$person_col,
    " WHERE e.", cov_src$concept_col, " IN (", paste(cov_ids, collapse = ", "), ")")
  pres <- .executeQuery(handle, .sql_translate(pres_sql, handle$target_dialect))

  X <- matrix(0L, nrow = nrow(pop), ncol = length(cov_ids),
              dimnames = list(NULL, paste0("cov_", cov_ids)))
  row_of <- match(as.integer(pres$subject_id), pop$subject_id)
  col_of <- match(as.integer(pres$covariate_id), cov_ids)
  ok <- !is.na(row_of) & !is.na(col_of)
  if (any(ok)) X[cbind(row_of[ok], col_of[ok])] <- 1L

  # Drop zero-variance predictors (a constant column is collinear with the
  # intercept and makes the GLM rank-deficient).
  keep <- apply(X, 2, function(col) length(unique(col)) > 1)
  X <- X[, keep, drop = FALSE]
  cov_ids <- cov_ids[keep]
  if (ncol(X) == 0) return(NULL)

  # Fit the prediction model IN-SESSION. Top-K restriction keeps it well-posed;
  # suppress the perfect-separation / non-convergence warnings (handled by the
  # caller's fail-closed NA guards on the resulting scalars).
  dat <- data.frame(outcome = pop$outcome, X, check.names = FALSE)
  fit <- tryCatch(
    suppressWarnings(stats::glm(outcome ~ ., data = dat, family = stats::binomial())),
    error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  risk <- tryCatch(
    suppressWarnings(as.numeric(stats::predict(fit, type = "response"))),
    error = function(e) NULL)
  if (is.null(risk) || length(risk) != nrow(pop) || any(is.na(risk))) return(NULL)

  list(
    risk            = risk,            # in-session ONLY; never released per-subject
    outcome         = pop$outcome,
    n_predictors    = ncol(X),
    population_size = nrow(pop),
    n_outcomes      = sum(pop$outcome == 1L),
    covariate_ids   = cov_ids
  )
}

#' AUROC from predicted risk vs binary outcome (rank-based, in-session)
#'
#' The Mann-Whitney form of the area under the ROC curve
#' (\code{(sum of ranks of positives - n1 (n1 + 1) / 2) / (n1 n0)}), computed
#' entirely in R from the in-session risk/outcome vectors. A scalar summary (no
#' per-subject value leaves the session).
#'
#' @param risk Numeric predicted-risk vector.
#' @param y Integer 0/1 outcome vector.
#' @return Numeric AUROC, or NA when undefined (no positives or no negatives).
#' @keywords internal
.omopPlpAuroc <- function(risk, y) {
  n1 <- sum(y == 1L); n0 <- sum(y == 0L)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(risk, ties.method = "average")
  (sum(r[y == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

#' AUPRC from predicted risk vs binary outcome (trapezoidal, in-session)
#'
#' Area under the precision-recall curve, computed in R by sweeping the unique
#' risk thresholds (descending) and trapezoidally integrating precision over
#' recall. A scalar summary (no per-subject value leaves the session).
#'
#' @param risk Numeric predicted-risk vector.
#' @param y Integer 0/1 outcome vector.
#' @return Numeric AUPRC, or NA when there are no positives.
#' @keywords internal
.omopPlpAuprc <- function(risk, y) {
  n1 <- sum(y == 1L)
  if (n1 == 0) return(NA_real_)
  ord <- order(risk, decreasing = TRUE)
  y <- y[ord]
  tp <- cumsum(y == 1L)
  fp <- cumsum(y == 0L)
  recall    <- tp / n1
  precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 1)
  recall    <- c(0, recall)
  precision <- c(1, precision)
  sum(diff(recall) * (precision[-1] + precision[-length(precision)]) / 2)
}

# --- Native PLP (fitted): performance ----------------------------------------

#' \code{dsomop:plp.performance} entry (PatientLevelPrediction performance)
#' @keywords internal
.omopPlpPerformance <- function() {
  name <- "dsomop:plp.performance"

  fn <- function(handle, ctx, params) {
    m <- .omopPlpFitModel(handle, ctx, params)
    if (is.null(m)) return(data.frame())
    settings <- .omopDisclosureSettings()
    sub <- settings$nfilter_subset

    # AUROC / AUPRC are scalar discrimination metrics computed in-R from the
    # in-session predicted risk vs outcome. EXPLICIT non-frame disclosure guard
    # (DOCUMENTED): NA them whenever the population OR either outcome class (the
    # positives or the negatives) has fewer than nfilter.subset persons — a
    # discrimination metric on a sub-threshold class is disclosive (it rests on
    # ranking a tiny set against the rest). Mirrors the propensity-AUC guard in
    # ohdsi_pack_cohortmethod.R. No per-subject risk is ever returned.
    n_neg <- m$population_size - m$n_outcomes
    too_small <- m$population_size < sub || m$n_outcomes < sub || n_neg < sub
    auroc <- if (too_small) NA_real_ else .omopPlpAuroc(m$risk, m$outcome)
    auprc <- if (too_small) NA_real_ else .omopPlpAuprc(m$risk, m$outcome)

    # population_size is a distinct-person count -> unit="person", suppressed +
    # banded by the gate. The metrics ride alongside it as derived scalars.
    data.frame(
      metric          = c("AUROC", "AUPRC"),
      value           = c(auroc, auprc),
      population_size = c(m$population_size, m$population_size),
      stringsAsFactors = FALSE)
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Patient-Level Prediction discrimination performance ",
                         "(AUROC, AUPRC) of a logistic model fit IN-SESSION over ",
                         "the scoped target population; scalar metrics only, NA ",
                         "below disclosure thresholds. No per-subject risk leaves ",
                         "the session."),
    domain      = "general",
    params      = .omopPlpModelParams(),
    compute     = list(kind = "r", sql = NULL, fn = fn, plot = NULL),
    dependencies = list(
      tables = c("observation_period", "condition_occurrence", "concept"),
      packages = character(0)),
    # population_size is a distinct-person count: small-cell suppression + banding
    # fully gate it; the gate also drops the whole (one-metric) row if it rests on
    # too few persons.
    disclosure = .omopAnalysisDisclosure(
      unit = "person", count_cols = "population_size"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native PLP (fitted): calibration ----------------------------------------

#' \code{dsomop:plp.calibration} entry (PatientLevelPrediction calibration)
#' @keywords internal
.omopPlpCalibration <- function() {
  name <- "dsomop:plp.calibration"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = mean_predicted, y = observed_rate)) +",
    "    ggplot2::geom_point() +",
    "    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +",
    "    ggplot2::labs(x = 'Mean predicted risk', y = 'Observed outcome rate')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    m <- .omopPlpFitModel(handle, ctx, params)
    if (is.null(m)) return(data.frame())

    # Bin the in-session predicted risk into deciles; per bin report the mean
    # predicted risk, the bin person count (banded), and the outcome count
    # (banded). The observed rate is reconciled from the BANDED outcome /
    # BANDED bin-n via .omopAnalysisReconcileRatio, so a surviving rate never
    # re-derives a sub-threshold cell. Sparse deciles are dropped by the gate.
    n_bins <- 10L
    br <- stats::quantile(m$risk, probs = seq(0, 1, length.out = n_bins + 1L),
                          names = FALSE, type = 7)
    br <- unique(br)
    if (length(br) < 2L) return(data.frame())
    bin <- findInterval(m$risk, br, rightmost.closed = TRUE, all.inside = TRUE)

    parts <- lapply(sort(unique(bin)), function(b) {
      idx <- bin == b
      data.frame(
        bin            = b,
        mean_predicted = mean(m$risk[idx]),
        n_persons      = sum(idx),
        n_outcomes     = sum(m$outcome[idx] == 1L),
        observed_rate  = NA_real_,
        stringsAsFactors = FALSE)
    })
    df <- do.call(rbind, parts)

    # mean_predicted is a CONTINUOUS summary of the in-session predicted risk over
    # the bin's persons. A surviving bin only needs nfilter_tab persons, but a
    # continuous summary that rests on fewer than nfilter_dist values can collapse
    # to (or tightly approximate) an INDIVIDUAL predicted risk — exactly the
    # propensity/risk-score leak the task forbids (a homogeneous small bin's mean
    # IS one person's risk). Mask it to NA below nfilter_dist, the same standard the
    # continuous (dist) kernels use for avg/quantile stats. Done BEFORE the
    # reconcile bands n_persons (so the test is on the RAW bin person count).
    nfd <- .omopDisclosureSettings()$nfilter_dist %||% 10L
    df$mean_predicted[is.na(df$n_persons) | df$n_persons < nfd] <- NA_real_

    # Reconcile observed_rate = n_outcomes / n_persons over BANDED counts (NA when
    # either side is suppressed). This ALSO bands n_outcomes in place — the bin
    # outcome count is a legitimately-small/zero number in a low-risk decile (a
    # zero-outcome bin is NOT disclosive), so it is banded here but NOT declared a
    # gate count_col (that would small-cell-DROP every low-risk bin). The gate
    # drops a sparse bin only on the n_persons person basis.
    df <- .omopAnalysisReconcileRatio(
      df, numerator_col = "n_outcomes", denominator_col = "n_persons",
      ratio_col = "observed_rate", scale = 1)
    # Couple the rate away wherever the banded outcome count is 0 (a raw count in
    # [nfilter_tab, nfilter_band) bands to 0; reconcile would still report rate 0,
    # betraying the small count). Same in-fn coupling as the gate applies to
    # binary prevalences.
    df$observed_rate[is.na(df$n_outcomes) | df$n_outcomes == 0] <- NA_real_
    df
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Patient-Level Prediction calibration: deciles of the ",
                         "in-session predicted risk with mean-predicted vs ",
                         "observed outcome rate per decile; bin counts banded, ",
                         "observed rate reconciled from banded counts, sparse ",
                         "deciles dropped. No per-subject risk leaves the session."),
    domain      = "general",
    params      = .omopPlpModelParams(),
    compute     = list(kind = "r", sql = NULL, fn = fn,
                       plot = list(type = "line", code = plot_code)),
    dependencies = list(
      tables = c("observation_period", "condition_occurrence", "concept"),
      packages = character(0)),
    # unit="record": the bin is gated on its distinct-person count n_persons (the
    # gate's generic record branch drops a bin below nfilter and bands n_persons).
    # n_outcomes is the reconcile numerator — banded in-fn but NOT a count_col, so
    # a safe zero-outcome low-risk decile is reported (banded) rather than dropped.
    disclosure = .omopAnalysisDisclosure(
      unit = "record", count_cols = "n_persons",
      person_id_col = "n_persons"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native PLP (fitted): threshold summary ----------------------------------

#' \code{dsomop:plp.threshold} entry (PatientLevelPrediction threshold summary)
#' @keywords internal
.omopPlpThreshold <- function() {
  name <- "dsomop:plp.threshold"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = 1 - specificity, y = sensitivity)) +",
    "    ggplot2::geom_line() + ggplot2::geom_point() +",
    "    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +",
    "    ggplot2::labs(x = '1 - specificity', y = 'Sensitivity')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    m <- .omopPlpFitModel(handle, ctx, params)
    if (is.null(m)) return(data.frame())

    y <- m$outcome
    pos <- sum(y == 1L); neg <- sum(y == 0L)
    settings <- .omopDisclosureSettings()
    bw <- settings$nfilter_band

    # A coarse fixed grid of probability thresholds. At each cut compute the
    # confusion-cell PERSON counts (tp/fp/tn/fn). sensitivity/specificity/PPV are
    # each reconciled from BANDED numerator+denominator via
    # .omopAnalysisReconcileRatio (no rate rests on a raw cell), which ALSO bands
    # tp/tn in place. The remaining cells fp/fn are banded here explicitly. The
    # confusion cells are legitimately small/zero (a good classifier has ~0 FP/FN)
    # and a zero cell is NOT disclosive, so they are banded but NOT declared gate
    # count_cols (that would small-cell-DROP every clean threshold row); the gate
    # gates the whole table on the ARM totals n_positive / n_negative instead.
    grid <- seq(0.1, 0.9, by = 0.1)
    parts <- lapply(grid, function(t) {
      pred1 <- m$risk >= t
      tp <- sum(pred1 & y == 1L); fp <- sum(pred1 & y == 0L)
      fn_ <- sum(!pred1 & y == 1L); tn <- sum(!pred1 & y == 0L)
      data.frame(
        threshold   = t,
        tp = tp, fp = fp, tn = tn, fn = fn_,
        n_positive  = pos, n_negative = neg,
        sensitivity = NA_real_, specificity = NA_real_, ppv = NA_real_,
        stringsAsFactors = FALSE)
    })
    df <- do.call(rbind, parts)

    # sensitivity = tp / (tp + fn) = tp / n_positive (banded);
    # specificity = tn / (tn + fp) = tn / n_negative (banded);
    # ppv         = tp / (tp + fp). Each reconciled over banded numerator+denom.
    df$tp_fp <- df$tp + df$fp
    df <- .omopAnalysisReconcileRatio(df, "tp", "n_positive", "sensitivity", 1)
    df <- .omopAnalysisReconcileRatio(df, "tn", "n_negative", "specificity", 1)
    df <- .omopAnalysisReconcileRatio(df, "tp", "tp_fp", "ppv", 1)
    df$tp_fp <- NULL
    # Couple each rate away where its banded numerator (tp for sensitivity/ppv, tn
    # for specificity) is 0: a raw cell in [nfilter_tab, nfilter_band) bands to 0,
    # and a rate over a banded-to-0 numerator must be NA, not 0 (would betray the
    # small cell). Reconcile already overwrote tp/tn with their banded values.
    df$sensitivity[is.na(df$tp) | df$tp == 0] <- NA_real_
    df$ppv[is.na(df$tp) | df$tp == 0]         <- NA_real_
    df$specificity[is.na(df$tn) | df$tn == 0] <- NA_real_
    # Band the two cells not covered by a reconcile (fp, fn) so no confusion cell
    # is ever released at 1-person resolution.
    df$fp <- vapply(df$fp, .bandCount, numeric(1), band_width = bw)
    df$fn <- vapply(df$fn, .bandCount, numeric(1), band_width = bw)
    df
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Patient-Level Prediction threshold summary: over a ",
                         "coarse probability-threshold grid, banded confusion-",
                         "cell person counts with sensitivity/specificity/PPV ",
                         "reconciled from banded counts; sparse thresholds ",
                         "dropped. No per-subject risk leaves the session."),
    domain      = "general",
    params      = .omopPlpModelParams(),
    compute     = list(kind = "r", sql = NULL, fn = fn,
                       plot = list(type = "line", code = plot_code)),
    dependencies = list(
      tables = c("observation_period", "condition_occurrence", "concept"),
      packages = character(0)),
    # unit="record": the whole threshold table is releasable iff BOTH arms meet
    # nfilter (so the count basis is the arm totals n_positive / n_negative — the
    # gate drops every row if either arm is sub-threshold and bands the totals).
    # The confusion cells tp/fp/tn/fn are banded in-fn (reconcile bands tp/tn; fp/
    # fn banded explicitly) but kept OUT of count_cols so a safe zero cell does not
    # drop the row.
    disclosure = .omopAnalysisDisclosure(
      unit = "record",
      count_cols = c("n_positive", "n_negative"),
      person_id_col = "n_positive"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native PLP (fitted): diagnostic summary ---------------------------------

#' \code{dsomop:plp.diagnostic} entry (PatientLevelPrediction diagnostic summary)
#' @keywords internal
.omopPlpDiagnostic <- function() {
  name <- "dsomop:plp.diagnostic"

  fn <- function(handle, ctx, params) {
    m <- .omopPlpFitModel(handle, ctx, params)
    if (is.null(m)) return(data.frame())
    settings <- .omopDisclosureSettings()
    thr <- settings$nfilter_tab

    # Design diagnostics: population size, outcome incidence, predictor count, and
    # events-per-variable (EVP). The row is gated on the population_size person
    # basis; n_outcomes is the reconcile numerator (banded in-fn, NOT a count_col),
    # so a small-but-safe outcome count does not drop the whole (population) row.
    # outcome_incidence is reconciled from the banded n_outcomes / population_size.
    # EVP = n_outcomes / n_predictors is a model-stability scalar: NA'd when the
    # event count is below nfilter (explicit non-frame guard, DOCUMENTED), else
    # computed from the BANDED event count so it never re-derives a small count.
    # EVP rests on the BANDED outcome count: a raw count in [nfilter_tab,
    # nfilter_band) bands to 0, and a derived value over a banded-to-0 numerator
    # must be coupled away (NA), not reported as 0 (the gate's binary-prevalence
    # coupling, applied in-fn here — the sanctioned place for a derived value).
    n_out_banded <- if (m$n_outcomes < thr) NA_real_ else
      .bandCount(m$n_outcomes, settings$nfilter_band)
    evp <- if (is.na(n_out_banded) || n_out_banded == 0 || m$n_predictors == 0)
      NA_real_ else n_out_banded / m$n_predictors

    df <- data.frame(
      population_size    = m$population_size,
      n_outcomes         = m$n_outcomes,
      n_predictors       = m$n_predictors,
      outcome_incidence  = NA_real_,
      events_per_variable = evp,
      stringsAsFactors = FALSE)
    # outcome_incidence = n_outcomes / population_size over banded counts.
    df <- .omopAnalysisReconcileRatio(
      df, numerator_col = "n_outcomes", denominator_col = "population_size",
      ratio_col = "outcome_incidence", scale = 1)
    # Couple incidence away when the banded outcome count is 0 (reconcile leaves a
    # banded-to-0 numerator's ratio at 0, which would still betray the small count).
    df$outcome_incidence[is.na(df$n_outcomes) | df$n_outcomes == 0] <- NA_real_
    df
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Patient-Level Prediction diagnostic summary: scoped ",
                         "target population size, outcome count + incidence, ",
                         "number of predictors and events-per-variable for an ",
                         "in-session logistic fit; counts banded, ratios ",
                         "reconciled, EVP NA below threshold."),
    domain      = "general",
    params      = .omopPlpModelParams(),
    compute     = list(kind = "r", sql = NULL, fn = fn, plot = NULL),
    dependencies = list(
      tables = c("observation_period", "condition_occurrence", "concept"),
      packages = character(0)),
    # unit="record": the row is gated on the population_size person basis (the gate
    # drops it if the target population is sub-threshold and bands population_size).
    # n_outcomes is the reconcile numerator — banded in-fn but NOT a count_col, so
    # a safe small outcome count reports (banded) rather than dropping the row.
    disclosure = .omopAnalysisDisclosure(
      unit = "record", count_cols = "population_size",
      person_id_col = "population_size"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- OHDSI plp_attrition id -> canonical delegate (B2 live port) -------------

#' Build one OHDSI plp_* id as a THIN DELEGATE to a canonical native PLP builder
#'
#' Mirrors \code{\link{.ohdsiAliasEntry}} (ohdsi_pack_aliases.R): keeps the legacy
#' OHDSI id (reference compatibility), computes the metric LIVE via the canonical
#' builder's compute fn (NO precomputed plp_* table is read), inherits the
#' canonical disclosure/params/dependencies/adapter verbatim (so the SINGLE
#' .omopAnalysisGate processes it identically to the canonical id), and FLIPS the
#' scope to live-computable (accepts_cohort=TRUE, accepts_tables=TRUE,
#' requires_cohort=TRUE) versus the precomputed adapter's
#' accepts_cohort=FALSE/max_tables=0L. The builder is re-invoked per run
#' (constructs a list only; no DB I/O) so the delegate always uses the current
#' canonical compute fn without a (circular) catalog lookup at build time.
#'
#' @param table_name Character; the legacy OHDSI plp_* table id stem.
#' @param builder Function; the canonical native builder (e.g. .omopPlpAttrition).
#' @return A single \code{omop_analysis_entry}.
#' @keywords internal
.omopPlpDelegateEntry <- function(table_name, builder) {
  canonical <- builder()
  name <- paste0("dsomop:ohdsi.plp.", table_name)
  fn <- function(handle, ctx, params) builder()$compute$fn(handle, ctx, params)

  .omopAnalysisEntry(
    name        = name,
    description = canonical$description,
    domain      = canonical$domain,
    params      = canonical$params,
    compute     = list(kind = "r", sql = NULL, fn = fn,
                       plot = canonical$compute$plot),
    dependencies = canonical$dependencies,
    # Inherit the canonical disclosure verbatim so the ONE gate behaves
    # identically to the canonical id (no new gate, no new disclosure spec).
    disclosure  = canonical$disclosure,
    # Flip the surface to live-computable; keep the canonical max_tables.
    # requires_cohort is forced TRUE: the canonical dsomop:plp.* entries return an
    # EMPTY frame un-scoped and are REGISTERED requires_cohort=TRUE
    # (.omopAnalysisDiagnosticEntries sets it after the builder runs, so the bare
    # builder output read here still says FALSE). Matching TRUE turns an un-scoped
    # run into a CLEAR error rather than a silently empty result — exactly as for
    # the canonical id and the B1 aliases (.ohdsiAliasEntry).
    scope = .omopAnalysisScope(
      accepts_cohort  = TRUE,
      accepts_tables  = TRUE,
      max_tables      = canonical$scope$max_tables %||% 1L,
      requires_cohort = TRUE),
    mode  = "aggregate",
    # Carry the canonical adapter tag (drives the gate's record-unit dispatch to
    # the GENERIC person_id_col branch) PLUS the OHDSI labels for listing + scope
    # text and an alias_target pointer.
    meta  = list(adapter      = canonical$meta$adapter %||% "diagnostic",
                 tool_id      = "plp",
                 table_name   = table_name,
                 alias_target = canonical$name)
  )
}

#' Emit the OHDSI PatientLevelPrediction live-computing catalog entries (PLP pack)
#'
#' Group registrar for \code{ohdsi_pack_plp.R}. Returns the five live-computable
#' PLP result-table ids as THIN DELEGATES to their canonical native twins:
#' \describe{
#'   \item{plp_attrition}{-> \code{dsomop:plp.attrition} (population-construction
#'     attrition; no model).}
#'   \item{plp_performances}{-> \code{dsomop:plp.performance} (AUROC/AUPRC of the
#'     in-session fitted model).}
#'   \item{plp_calibration_summary}{-> \code{dsomop:plp.calibration} (decile
#'     calibration table).}
#'   \item{plp_threshold_summary}{-> \code{dsomop:plp.threshold} (threshold-grid
#'     confusion summary).}
#'   \item{plp_diagnostic_summary}{-> \code{dsomop:plp.diagnostic} (population /
#'     EVP design diagnostics).}
#' }
#' The four fitted-model tables are now ported LIVE (the R-in-session principle:
#' the model is fit in the Rock session and only disclosure-safe aggregates are
#' returned), so the read-precomputed path is dead for these ids.
#' \code{plp_model_design} is config-only (reference-only) and stays on the legacy
#' read-precomputed adapter (untouched here); \code{plp_covariate_summary} is a B1
#' alias handled in ohdsi_pack_aliases.R.
#'
#' Takes \code{handle} for signature parity with the other adapters ONLY — it
#' performs NO DB I/O at build time (catalog build is cached on
#' \code{handle$analysis_catalog}; all queries run later inside the delegated
#' \code{compute$fn}).
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by id.
#' @keywords internal
.ohdsiPackPlpEntries <- function(handle) {
  delegates <- list(
    .omopPlpDelegateEntry("plp_attrition", .omopPlpAttrition),
    .omopPlpDelegateEntry("plp_performances", .omopPlpPerformance),
    .omopPlpDelegateEntry("plp_calibration_summary", .omopPlpCalibration),
    .omopPlpDelegateEntry("plp_threshold_summary", .omopPlpThreshold),
    .omopPlpDelegateEntry("plp_diagnostic_summary", .omopPlpDiagnostic)
  )
  stats::setNames(delegates, vapply(delegates, function(e) e$name, character(1)))
}
