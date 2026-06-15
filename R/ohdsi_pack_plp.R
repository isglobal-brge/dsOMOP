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
# Scope of PLP that is feasible without a fitted model:
#   * plp_attrition         -> ported here (B2): persons/outcomes remaining per
#                              population-construction step (the attrition kernel
#                              shape, identical to CohortMethod cm_attrition).
#   * plp_covariate_summary -> handled as a B1 alias to dsomop:plp.covariate_summary
#                              (ohdsi_pack_aliases.R); it needs no model either.
#   * plp_performances / plp_calibration_summary / plp_threshold_summary /
#     plp_diagnostic_summary / plp_model_design -> OUT: every one needs a
#     per-subject PREDICTED-RISK column produced by a fitted prediction model,
#     which the federation cannot compute from the CDM. Those (tool, table) ids
#     therefore remain on the legacy read-precomputed adapter (untouched here;
#     flagged for the maintainer's separate reject-stub pass) — this pack overlays
#     ONLY the feasible plp_attrition id.
#
# The canonical metric lives in a native diagnostic dsomop:plp.attrition (added to
# .omopAnalysisDiagnosticEntries); the OHDSI id is a THIN DELEGATE to it (same
# pattern as ohdsi_pack_aliases.R) so the live SQL/disclosure/params/scope live in
# exactly one place and the OHDSI id inherits them. No second gate is added: the
# delegated frame flows through the SINGLE .omopAnalysisGate exactly as the
# canonical id does.

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

# --- OHDSI plp_attrition id -> canonical delegate (B2 live port) -------------

#' Emit the OHDSI PatientLevelPrediction live-computing catalog entries (PLP pack)
#'
#' Group registrar for \code{ohdsi_pack_plp.R}. Returns the single feasible PLP
#' result-table id \code{dsomop:ohdsi.plp.plp_attrition} as a THIN DELEGATE to the
#' canonical native \code{dsomop:plp.attrition} diagnostic (mirroring
#' \code{\link{.ohdsiAliasEntry}} from \code{ohdsi_pack_aliases.R}): it keeps the
#' legacy id (reference compatibility), computes the attrition LIVE from the CDM
#' via the canonical fn (NO precomputed plp_attrition table is read), inherits the
#' canonical disclosure/params/dependencies/adapter verbatim (so the SINGLE
#' .omopAnalysisGate processes it identically to the canonical id), and FLIPS the
#' scope to live-computable (accepts_cohort=TRUE, accepts_tables=TRUE,
#' max_tables=1L) versus the precomputed adapter's
#' accepts_cohort=FALSE/max_tables=0L. The other PLP tables need a fitted-model
#' predicted-risk column and stay on the legacy read-precomputed adapter
#' (untouched here), so this group overlays ONLY plp_attrition.
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
  canonical <- .omopPlpAttrition()
  name <- "dsomop:ohdsi.plp.plp_attrition"

  # Thin delegate: compute the canonical attrition LIVE with the same ctx/params.
  # The builder is re-invoked per run (constructs a list only; no DB I/O) so the
  # delegate always uses the current canonical compute fn without a catalog
  # lookup (which would be circular during catalog build).
  fn <- function(handle, ctx, params) .omopPlpAttrition()$compute$fn(handle, ctx, params)

  entry <- .omopAnalysisEntry(
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
    # requires_cohort is forced TRUE: the canonical dsomop:plp.attrition returns an
    # EMPTY frame un-scoped and is REGISTERED requires_cohort=TRUE
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
                 table_name   = "plp_attrition",
                 alias_target = canonical$name)
  )

  stats::setNames(list(entry), name)
}
