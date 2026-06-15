# ==============================================================================
# Pack: Evidence Synthesis (client_synthesis) — per-site estimate emitters
# ==============================================================================
#
# OHDSI's EvidenceSynthesis result tables (es_cm_result, es_sccs_result,
# es_cm_diagnostics_summary, es_sccs_diagnostics_summary) are inherently
# CROSS-DATABASE: each row is the META-ANALYSIS of a comparative effect (or its
# diagnostics) POOLED across every database in a network study. A single site
# cannot compute a cross-site pooled estimate — that synthesis is the CLIENT's
# job (dsOMOPClient::ds.omop.meta.effect_estimate over the per-site estimates).
#
# So these four ids no longer READ a precomputed es_* table. Instead each one
# OVERLAYS its precomputed registry id with a thin LIVE delegate onto the
# per-site PLR port that produces the sufficient statistics the client pools:
#
#   es_cm_result               -> dsomop:cm.effect_estimate          (PLR-1)
#       per-site log HR/RR + se_log_estimate (inverse-variance inputs)
#   es_sccs_result             -> dsomop:sccs.incidence_rate_ratio   (PLR-9)
#       per-site log IRR + se_log_irr (inverse-variance inputs)
#   es_cm_diagnostics_summary  -> dsomop:cm.diagnostics_summary      (PLR-3)
#       per-site CohortMethod study-quality scalars (client tabulates/rolls up)
#   es_sccs_diagnostics_summary-> dsomop:sccs.assumption_checks      (per-site)
#       per-site SCCS assumption pass/fail (client tabulates/rolls up)
#
# Each delegate is a FAITHFUL re-label: same compute$fn / disclosure / scope as
# its per-site canonical, so it inherits the SINGLE .omopAnalysisGate and every
# fail-closed guard the audited PLR port already carries (banded counts, NA'd
# fitted estimate on a small/suppressed arm, no *_source_value). The ONLY change
# is the id and a description note that the result is a PER-SITE estimate to be
# meta-analyzed CLIENT-side; no patient data crosses sites, only the already
# disclosure-safe per-site log-estimate + SE.

#' Build one evidence-synthesis delegate onto a per-site PLR canonical
#'
#' Overlays \code{dsomop:ohdsi.evidence_synthesis.<table_name>} onto the
#' per-site canonical \code{builder()} (e.g. \code{.omopCmEffectEstimateEntry}),
#' preserving its compute / disclosure / scope verbatim and only re-labelling the
#' id, table name, and description (a note that the value is a per-site estimate
#' to meta-analyze client-side). Mirrors \code{\link{.ohdsiSccsAliasEntry}}.
#'
#' @param table_name Character; the es_* table id this delegate overlays.
#' @param builder Zero-arg builder of the per-site canonical entry to delegate to.
#' @return An \code{omop_analysis_entry} keyed by the es_* overlay id.
#' @keywords internal
.ohdsiEvidenceSynthesisAliasEntry <- function(table_name, builder) {
  canonical <- builder()
  name <- paste0("dsomop:ohdsi.evidence_synthesis.", table_name)
  fn <- function(handle, ctx, params) builder()$compute$fn(handle, ctx, params)
  note <- paste0(" NOTE: this is the PER-SITE estimate (cross-database ",
                 "meta-analysis is performed CLIENT-side over the per-site ",
                 "log-estimate + SE via dsOMOPClient::ds.omop.meta.effect_",
                 "estimate); a single site cannot compute a pooled estimate.")
  .omopAnalysisEntry(
    name        = name,
    description = paste0(canonical$description, note),
    domain      = canonical$domain,
    params      = canonical$params,
    compute     = list(kind = "r", sql = NULL, fn = fn,
                       plot = canonical$compute$plot),
    dependencies = canonical$dependencies,
    disclosure  = canonical$disclosure,
    scope = .omopAnalysisScope(
      accepts_cohort  = isTRUE(canonical$scope$accepts_cohort),
      accepts_tables  = isTRUE(canonical$scope$accepts_tables),
      max_tables      = canonical$scope$max_tables %||% 1L,
      requires_cohort = isTRUE(canonical$scope$requires_cohort)),
    mode  = "aggregate",
    meta  = list(adapter      = canonical$meta$adapter %||% "ohdsi_live",
                 tool_id      = "evidence_synthesis",
                 table_name   = table_name,
                 alias_target = canonical$name)
  )
}

#' Emit the live evidence-synthesis per-site delegate entries
#'
#' Overlays the four precomputed \code{dsomop:ohdsi.evidence_synthesis.es_*}
#' registry ids with live delegates onto their per-site PLR canonical ports (see
#' file header). The read-precomputed es_* path is now dead for these ids — the
#' client meta-analyzes the per-site estimates these emit. Takes \code{handle}
#' for signature parity with the other pack registrars (no DB I/O at build time).
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by es_* id.
#' @keywords internal
.ohdsiPackEvidenceSynthesisEntries <- function(handle) {
  entries <- list(
    .ohdsiEvidenceSynthesisAliasEntry("es_cm_result",
                                      .omopCmEffectEstimateCanonicalEntry),
    .ohdsiEvidenceSynthesisAliasEntry("es_sccs_result",
                                      .omopSccsIncidenceRateRatio),
    .ohdsiEvidenceSynthesisAliasEntry("es_cm_diagnostics_summary",
                                      .omopCmDiagnosticsSummaryCanonicalEntry),
    .ohdsiEvidenceSynthesisAliasEntry("es_sccs_diagnostics_summary",
                                      .omopSccsAssumptionChecks)
  )
  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}
