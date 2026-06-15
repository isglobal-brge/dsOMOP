# Module: OHDSI result-table aliases onto native live diagnostics (alias pack)
#
# Native replacements for the OHDSI "B1" result tables whose metric is ALREADY
# computed live, disclosure-safe, by a native diagnostic catalog entry. The
# legacy OHDSI adapter (.omopAnalysisOhdsiEntries) emitted one entry per
# (tool, table) that READ a precomputed OHDSI results table via .ohdsiGetResults.
# Each entry here keeps the SAME id (dsomop:ohdsi.<tool>.<table>) so existing
# references keep resolving, but its compute is a THIN DELEGATE to the canonical
# native diagnostic that mirrors the same OHDSI tool output: the alias builds the
# same run-path ctx it was handed and calls the canonical entry's compute$fn,
# inheriting that entry's LIVE SQL/R, disclosure spec, params and scope. No
# precomputed results table is read anywhere in this group, and zero new compute
# logic is added (the canonical entries already compute live and gate correctly).
#
# Disclosure: each alias inherits the canonical entry's disclosure spec verbatim
# (unit / count_cols / person_id_col / min_max), so the SINGLE .omopAnalysisGate
# small-cell-suppresses + bands + couples the delegated frame EXACTLY as it does
# for the canonical id — there is no second gate. Critically the alias also
# inherits meta$adapter from the canonical entry (it is "diagnostic", not
# "ohdsi"): the gate's record-unit branch dispatches on meta$adapter, and the
# canonical record entries (incidence rate, index-event breakdown, time-to-event)
# are gated through the GENERIC record branch on their declared person_id_col
# sibling column — NOT through .ohdsiPersonGate (which expects a precomputed
# sibling table). Routing an alias to .ohdsiPersonGate would be wrong, so the
# diagnostic adapter tag is preserved and the OHDSI labels (tool_id, table_name,
# alias_target) are carried alongside it for listing + scope text only.
#
# The registrar .ohdsiPackAliasEntries(handle) returns the group's entries keyed
# by id; it takes handle for signature parity ONLY and performs NO DB I/O at
# build time (catalog build is cached on handle$analysis_catalog; all queries run
# later inside each delegated compute$fn).

#' Build one OHDSI-result-table alias onto a canonical native diagnostic
#'
#' Resolves the canonical native diagnostic by calling its zero-arg builder once
#' at catalog-build time (cheap; constructs a list, runs no query) and emits an
#' \code{omop_analysis_entry} that:
#' \itemize{
#'   \item keeps the legacy OHDSI id \code{dsomop:ohdsi.<tool>.<table>} as its
#'     name (reference compatibility);
#'   \item DELEGATES compute: its \code{compute$fn} calls the canonical entry's
#'     \code{compute$fn(handle, ctx, params)} verbatim, so the metric is computed
#'     LIVE from the CDM (the canonical fn restricts to \code{ctx$scoped_cohort} /
#'     \code{ctx$scoped_cohorts}, self-gates before materialising, translates
#'     concept ids, and selects no \code{*_source_value});
#'   \item inherits the canonical entry's \code{disclosure} (unit / count_cols /
#'     person_id_col / min_max), \code{params}, \code{dependencies}, \code{domain}
#'     and \code{meta$adapter} — so the SINGLE \code{.omopAnalysisGate} processes
#'     the delegated frame identically to the canonical id;
#'   \item FLIPS scope to live-computable: \code{accepts_cohort=TRUE},
#'     \code{accepts_tables=TRUE}, \code{max_tables} = the canonical entry's
#'     max_tables (1L single-population, 2L for the two-population covariate
#'     balance), \code{requires_cohort=TRUE} (the delegate target is meaningless
#'     un-scoped), versus the precomputed adapter's
#'     \code{accepts_cohort=FALSE}/\code{max_tables=0L}.
#' }
#'
#' @param tool_id Character; the OHDSI tool registry key (the \code{<tool>} in
#'   the id, e.g. "cohort_diagnostics").
#' @param table_name Character; the OHDSI result table name (the \code{<table>}
#'   in the id, e.g. "incidence_rate"); a LABEL only (never read).
#' @param builder Zero-arg function returning the canonical native
#'   \code{omop_analysis_entry} to delegate to (e.g.
#'   \code{.omopDiagIncidenceRate}).
#' @return An \code{omop_analysis_entry} keyed (by the registrar) on its id.
#' @keywords internal
.ohdsiAliasEntry <- function(tool_id, table_name, builder) {
  canonical <- builder()
  name <- paste0("dsomop:ohdsi.", tool_id, ".", table_name)

  # Thin delegate: compute the canonical metric LIVE with the same ctx/params.
  # `builder` is re-invoked per run (constructs a list only; no DB I/O) so the
  # delegate always uses the current canonical compute fn without a catalog
  # lookup (which would be circular during catalog build).
  fn <- function(handle, ctx, params) builder()$compute$fn(handle, ctx, params)

  # Inherit the canonical max_tables (2L for the two-population balance delegate)
  # but flip the surface to live-computable. requires_cohort is forced TRUE: every
  # delegate target is a native diagnostic whose fn returns an EMPTY frame when
  # un-scoped and which is REGISTERED requires_cohort=TRUE
  # (.omopAnalysisDiagnosticEntries sets it after the builder runs, so the bare
  # builder output here still reads FALSE). Matching TRUE turns an un-scoped run
  # into a CLEAR error rather than a silently empty (gate-safe) result — exactly
  # as for the canonical id.
  scope <- .omopAnalysisScope(
    accepts_cohort  = TRUE,
    accepts_tables  = TRUE,
    max_tables      = canonical$scope$max_tables %||% 1L,
    requires_cohort = TRUE)

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
    scope = scope,
    mode  = "aggregate",
    # Carry the canonical adapter tag (drives the gate's record-unit dispatch)
    # PLUS the OHDSI labels for listing + scope text and an alias_target pointer.
    meta  = list(adapter      = canonical$meta$adapter %||% "diagnostic",
                 tool_id      = tool_id,
                 table_name   = table_name,
                 alias_target = canonical$name)
  )
}

#' Emit the OHDSI "B1" alias catalog entries (live-computing delegates)
#'
#' Group registrar for \code{ohdsi_pack_aliases.R}. Returns the 11
#' \code{dsomop:ohdsi.<tool>.<table>} entries whose metric is already computed
#' live by a canonical native diagnostic, each keyed by its stable id and each
#' delegating its compute to that diagnostic (no precomputed OHDSI results table
#' is read). Takes \code{handle} for signature parity with the other adapters
#' ONLY — it performs NO DB I/O at build time; all queries run later inside each
#' delegated \code{compute$fn}.
#'
#' Mappings (OHDSI result table -> canonical native diagnostic):
#' \itemize{
#'   \item cohort_diagnostics.incidence_rate -> dsomop:incidence.rate
#'   \item cohort_diagnostics.index_event_breakdown ->
#'     dsomop:cohortdx.index_event_breakdown
#'   \item cohort_diagnostics.visit_context -> dsomop:cohortdx.visit_context
#'   \item cohort_incidence.incidence_summary -> dsomop:incidence.rate
#'   \item characterization.c_covariates -> dsomop:char.target_covariates
#'   \item characterization.c_covariates_continuous -> dsomop:fe.continuous
#'   \item characterization.c_time_to_event -> dsomop:char.time_to_event
#'   \item cohort_method.cm_covariate_balance -> dsomop:cm.covariate_balance
#'   \item cohort_method.cm_shared_covariate_balance ->
#'     dsomop:cm.covariate_balance (shared == before-matching balance)
#'   \item cohort_method.cm_follow_up_dist -> dsomop:cm.followup_distribution
#'   \item plp.plp_covariate_summary -> dsomop:plp.covariate_summary
#' }
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by id.
#' @keywords internal
.ohdsiPackAliasEntries <- function(handle) {
  entries <- list(
    # CohortDiagnostics -> native CohortIncidence/CohortDiagnostics ports.
    .ohdsiAliasEntry("cohort_diagnostics", "incidence_rate",
                     .omopDiagIncidenceRate),
    .ohdsiAliasEntry("cohort_diagnostics", "index_event_breakdown",
                     .omopDiagIndexEventBreakdown),
    .ohdsiAliasEntry("cohort_diagnostics", "visit_context",
                     .omopDiagVisitContext),

    # CohortIncidence -> the marquee live incidence-rate port.
    .ohdsiAliasEntry("cohort_incidence", "incidence_summary",
                     .omopDiagIncidenceRate),

    # Characterization -> covariate prevalence/continuous + time-to-event ports.
    .ohdsiAliasEntry("characterization", "c_covariates",
                     .omopCharTargetCovariates),
    .ohdsiAliasEntry("characterization", "c_covariates_continuous",
                     .omopFeContinuous),
    .ohdsiAliasEntry("characterization", "c_time_to_event",
                     .omopDiagTimeToEvent),

    # CohortMethod -> two-population covariate balance (max_tables = 2L via the
    # canonical entry) + follow-up distribution. "shared" == before-matching
    # balance, which is exactly what the federation's pre-matching balance port
    # computes, so both balance tables delegate to the same canonical entry.
    .ohdsiAliasEntry("cohort_method", "cm_covariate_balance",
                     .omopCmCovariateBalance),
    .ohdsiAliasEntry("cohort_method", "cm_shared_covariate_balance",
                     .omopCmCovariateBalance),
    .ohdsiAliasEntry("cohort_method", "cm_follow_up_dist",
                     .omopDiagFollowupDistribution),

    # PatientLevelPrediction -> covariate summary (prevalence by outcome status).
    .ohdsiAliasEntry("plp", "plp_covariate_summary",
                     .omopPlpCovariateSummary)
  )

  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}
