# Module: Unified Analysis Catalog
#
# A single backbone that unifies the three previously-divergent analysis
# surfaces — the curated QueryLibrary SQL templates, the pre-computed Achilles
# analytics, and the generic OHDSI result tables — behind ONE registry, ONE
# fail-closed run path, and ONE disclosure gate.
#
# Why this exists: each surface had its own listing function, its own run
# function, and (critically) its own divergent post-run disclosure pass:
#   * QueryLibrary  : .query_exec()       -> .suppressSmallCounts on declared
#                                            sensitive_fields + a person-count
#                                            heuristic + strict-mode reject.
#   * Achilles      : .achillesGetResults -> .suppressSmallCounts(count_value)
#                                            + .achillesPersonGate (record gate).
#   * OHDSI         : .ohdsiGetResults    -> .suppressSmallCounts(count_cols)
#                                            + .ohdsiPersonGate.
# The three gates implement the SAME per-patient invariant (a returned row must
# rest on >= nfilter persons; distributions must rest on >= nfilter_dist values;
# every count is small-cell suppressed then banded) by three different code
# paths. This module funnels all three through .omopAnalysisGate(), so the
# invariant lives in exactly one place.
#
# Live-compute ports: the Achilles catalog family no longer READS precomputed
# achilles_results rows — every "dsomop:achilles.<id>" entry now computes its
# metric live from the CDM (assembled from the pack_achilles_*.R registrars), and
# the OHDSI families compute live wherever a single-site CDM metric is definable
# (ohdsi_pack_*.R). The only catalog entries that still read a precomputed table
# are OHDSI results with no single-site live definition (fitted-model PLP, cross-
# site evidence-synthesis, and pure definition/settings reference tables).
#
# The catalog entry contract (see .omopAnalysisEntry) is deliberately uniform so
# a future plotting layer (6b) and additional compute kinds can be added without
# touching the run path or the gate.

# --- Entry contract ----------------------------------------------------------

#' Construct one analysis-catalog entry
#'
#' A catalog entry is the uniform description of a single runnable analysis,
#' regardless of which adapter produced it. Every field has a stable meaning so
#' the run path (\code{\link{.omopAnalysisRun}}) and the unified gate
#' (\code{\link{.omopAnalysisGate}}) can operate on any entry without knowing its
#' origin.
#'
#' @param name Character; the pack-prefixed stable id. Native entries are
#'   \code{"dsomop:<id>"} (e.g. \code{"dsomop:achilles.401"}).
#' @param description Character; human-readable description.
#' @param domain Character; OMOP-ish domain ("condition", "person", "general").
#' @param params List of parameter specs, each a list with \code{name},
#'   \code{type} (one of int|number|concept_id|concept_set|enum|date|bool),
#'   \code{required}, \code{default}, and optional \code{min}/\code{max}/
#'   \code{choices}.
#' @param compute List describing how to produce the result: \code{kind} is
#'   "sql", "r", or "sql+r"; \code{sql} is a template (kind "sql"); \code{fn} is
#'   a \code{function(handle, ctx, params)} returning a data.frame (kind "r").
#'   An unused \code{plot=NULL} slot is carried for the future plotting layer.
#' @param dependencies List with \code{tables} and \code{packages}; missing
#'   dependencies WARN (never fail) at registry-build time.
#' @param disclosure List describing the per-patient gate for this entry:
#'   \code{person_id_col}, \code{count_cols}, \code{unit} (person|record|dist),
#'   \code{min_max} (logical), and \code{gate} ("distinct_person").
#' @param scope List describing how scoping applies: \code{accepts_cohort},
#'   \code{accepts_tables}, \code{max_tables}.
#' @param mode Character; "aggregate" (default) or "assign" (QueryLibrary
#'   loaders). Only "aggregate" entries may run through the aggregate run path.
#' @param meta Optional named list of adapter-private state the gate/run path
#'   needs (e.g. an Achilles gate spec, an OHDSI tool id, a query id).
#' @return A named list with class \code{omop_analysis_entry}.
#' @keywords internal
.omopAnalysisEntry <- function(name, description, domain, params, compute,
                               dependencies, disclosure, scope,
                               mode = "aggregate", meta = list()) {
  entry <- list(
    name         = name,
    description  = description %||% "",
    domain       = domain %||% "general",
    params       = params %||% list(),
    compute      = compute,
    dependencies = dependencies %||% list(tables = character(0),
                                          packages = character(0)),
    disclosure   = disclosure,
    scope        = scope,
    mode         = mode %||% "aggregate",
    meta         = meta %||% list()
  )
  # Carry an (unused) plot slot for the future plotting layer (6b) without
  # forcing every adapter to set it.
  if (is.null(entry$compute$plot)) entry$compute$plot <- NULL
  class(entry) <- c("omop_analysis_entry", "list")
  entry
}

#' Default scope spec for an entry
#'
#' \code{requires_cohort} marks an entry whose result is only meaningful over a
#' scoped population (the cohort IS the analysis population, e.g. the native
#' diagnostics) so the run path raises a clear error when it is run un-scoped,
#' rather than letting the entry's \code{compute$fn} silently return an empty
#' (gate-safe) frame. Cohort-OPTIONAL entries (SQL templates, the demo entry)
#' leave it \code{FALSE} and run cohort-wide when un-scoped.
#'
#' @keywords internal
.omopAnalysisScope <- function(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 2L, requires_cohort = FALSE) {
  list(accepts_cohort  = isTRUE(accepts_cohort),
       accepts_tables  = isTRUE(accepts_tables),
       max_tables      = as.integer(max_tables),
       requires_cohort = isTRUE(requires_cohort))
}

#' Default disclosure spec for an entry
#' @keywords internal
.omopAnalysisDisclosure <- function(unit = "person", count_cols = character(0),
                                    person_id_col = NULL, min_max = FALSE,
                                    gate = "distinct_person") {
  list(person_id_col = person_id_col,
       count_cols    = count_cols %||% character(0),
       unit          = unit,
       min_max       = isTRUE(min_max),
       gate          = gate)
}

# --- Pack-facing constructors (re-exported) ----------------------------------
#
# A third-party analysis pack (see .omopAnalysisPackEntries) cannot reach the
# dotted internals, so these thin wrappers re-export the exact same constructors
# the native adapters use. A pack's registrar builds its entries with
# omopAnalysisEntry()/omopAnalysisScope()/omopAnalysisDisclosure() and returns
# them as a named list; dsOMOP namespaces the ids and runs every entry through
# the ONE gate. The wrappers add no behaviour — entries are identical to native
# ones — so a pack can never construct an entry that bypasses the gate.

#' Construct an analysis-catalog entry (for third-party analysis packs)
#'
#' Public re-export of the internal entry constructor for packages contributing
#' analyses via \code{Config/dsOMOP/AnalysisCollection}. See
#' \code{\link{.omopAnalysisEntry}} for the field contract. The returned entry's
#' \code{compute$fn} output is always gated by dsOMOP's single disclosure gate;
#' a pack cannot register its own gate.
#'
#' @inheritParams .omopAnalysisEntry
#' @return A \code{omop_analysis_entry} object.
#' @export
omopAnalysisEntry <- function(name, description, domain, params, compute,
                              dependencies, disclosure, scope,
                              mode = "aggregate", meta = list()) {
  .omopAnalysisEntry(name = name, description = description, domain = domain,
                     params = params, compute = compute,
                     dependencies = dependencies, disclosure = disclosure,
                     scope = scope, mode = mode, meta = meta)
}

#' Build a scope spec for a pack analysis entry
#'
#' Public re-export of \code{\link{.omopAnalysisScope}}. Set
#' \code{max_tables = 2L} for a two-population entry (overlap / SMD /
#' covariate-balance) so the run path resolves \code{scope} into two
#' independently re-gated cohorts. Set \code{requires_cohort = TRUE} for an entry
#' that is only meaningful over a scoped population, so an un-scoped run fails
#' with a clear error instead of returning an empty frame.
#'
#' @inheritParams .omopAnalysisScope
#' @return A scope spec list.
#' @export
omopAnalysisScope <- function(accepts_cohort = TRUE, accepts_tables = TRUE,
                              max_tables = 2L, requires_cohort = FALSE) {
  .omopAnalysisScope(accepts_cohort = accepts_cohort,
                     accepts_tables = accepts_tables, max_tables = max_tables,
                     requires_cohort = requires_cohort)
}

#' Build a disclosure spec for a pack analysis entry
#'
#' Public re-export of \code{\link{.omopAnalysisDisclosure}}. The \code{unit}
#' (person/record/dist) selects which branch of the single gate applies to the
#' entry's result.
#'
#' @inheritParams .omopAnalysisDisclosure
#' @return A disclosure spec list.
#' @export
omopAnalysisDisclosure <- function(unit = "person", count_cols = character(0),
                                   person_id_col = NULL, min_max = FALSE,
                                   gate = "distinct_person") {
  .omopAnalysisDisclosure(unit = unit, count_cols = count_cols,
                          person_id_col = person_id_col, min_max = min_max,
                          gate = gate)
}

#' Reconcile a derived ratio over banded counts (for pack analysis entries)
#'
#' Public re-export of \code{\link{.omopAnalysisReconcileRatio}} for record-unit
#' pack \code{compute$fn}s that return a rate/percentage: recomputes it from the
#' BANDED numerator+denominator (or NAs it when either is suppressed) so a raw
#' ratio over banded counts is never released.
#'
#' @inheritParams .omopAnalysisReconcileRatio
#' @return \code{df} with banded counts and a reconciled (or NA) ratio column.
#' @export
omopAnalysisReconcileRatio <- function(df, numerator_col, denominator_col,
                                       ratio_col, scale = 1) {
  .omopAnalysisReconcileRatio(df = df, numerator_col = numerator_col,
                              denominator_col = denominator_col,
                              ratio_col = ratio_col, scale = scale)
}

# --- Param typing ------------------------------------------------------------

#' Map a QueryLibrary inferred parameter type to a catalog param type
#'
#' The QueryLibrary infers only "integer"/"numeric"/"unknown" from the template
#' example value (\code{\link{.inferParamType}}). Catalog params use a richer
#' vocabulary; we narrow integer->int, numeric->number, and treat unknown
#' (which the sanitizer still forces to be numeric) as "int" so the param is
#' typed consistently with how it is sanitized.
#'
#' @param inferred Character from \code{\link{.inferParamType}}.
#' @return Character catalog param type.
#' @keywords internal
.omopAnalysisParamType <- function(inferred) {
  switch(inferred,
    integer = "int",
    numeric = "number",
    "int"
  )
}

#' Build catalog param specs from a QueryLibrary inputs table
#'
#' One spec per row of the template's parsed inputs data.frame. Type is inferred
#' via \code{\link{.inferParamType}} (the same source the sanitizer trusts);
#' \code{required} comes from the Mandatory column; \code{default} from the
#' Example column.
#'
#' @param inputs_df Data frame; parsed template inputs (may be NULL/empty).
#' @return List of param specs.
#' @keywords internal
.omopAnalysisParamsFromInputs <- function(inputs_df) {
  if (is.null(inputs_df) || !is.data.frame(inputs_df) || nrow(inputs_df) == 0) {
    return(list())
  }
  param_col <- intersect(c("parameter", "param", "name"), names(inputs_df))
  example_col <- intersect(c("example", "default"), names(inputs_df))
  mand_col <- intersect(c("mandatory", "required"), names(inputs_df))
  if (length(param_col) == 0) return(list())

  specs <- list()
  for (i in seq_len(nrow(inputs_df))) {
    pname <- trimws(as.character(inputs_df[[param_col[1]]][i]))
    if (is.na(pname) || nchar(pname) == 0) next
    default_val <- if (length(example_col) > 0) {
      trimws(as.character(inputs_df[[example_col[1]]][i]))
    } else ""
    if (is.na(default_val) || nchar(default_val) == 0) default_val <- NULL
    required <- FALSE
    if (length(mand_col) > 0) {
      m <- tolower(trimws(as.character(inputs_df[[mand_col[1]]][i])))
      required <- m %in% c("yes", "true", "y", "1", "required")
    }
    specs[[length(specs) + 1L]] <- list(
      name     = pname,
      type     = .omopAnalysisParamType(.inferParamType(pname, inputs_df)),
      required = required,
      default  = default_val
    )
  }
  specs
}

# --- Adapter 1: QueryLibrary -------------------------------------------------

#' Emit catalog entries for the curated QueryLibrary templates
#'
#' One \code{kind="sql"} entry per allowlist-eligible template. Reuses the
#' existing QueryLibrary loading, allowlist, and strict-mode policy verbatim:
#' templates absent from the allowlist are skipped in strict mode, and BLOCKED
#' templates are never emitted. The entry's \code{compute$sql} is the raw
#' template (schema + \code{@param} placeholders intact); the run path renders
#' and sanitizes it exactly as \code{\link{.query_exec}} does.
#'
#' Disclosure: \code{count_cols} are the template's declared sensitive fields;
#' \code{unit} is "person" when a person-count column is identifiable among them
#' (or via the conventional person-column names \code{.query_exec} gates on),
#' else "record". \code{person_id_col} is the conventional person-count column
#' name when present, so the record-unit branch of the gate has a declared
#' sibling count to gate on.
#'
#' @param handle CDM handle (unused; signature parity with the other adapters).
#' @return List of \code{omop_analysis_entry} objects keyed by entry name.
#' @keywords internal
.omopAnalysisQueryEntries <- function(handle) {
  queries <- .ql_load_queries()
  allowlist <- .ql_load_allowlist()
  strict <- isTRUE(.omopDisclosureSettings()$query_strict)

  entries <- list()
  for (qid in names(queries)) {
    q <- queries[[qid]]
    al <- allowlist[[qid]]
    if (strict && is.null(al)) next

    safety <- if (!is.null(al)) al else .ql_classify(q$sql, q$mode)
    safety_class <- safety$class %||% safety[["class"]]
    if (identical(safety_class, "BLOCKED")) next

    # Sensitive count columns: declared by the template (preferred), else
    # auto-detected by the static classifier — the same fields .query_exec
    # suppresses.
    count_cols <- q$sensitive_fields
    if (length(count_cols) == 0) {
      count_cols <- .ql_classify(q$sql, q$mode)$sensitive_fields_detected
    }

    # Person basis: the conventional person-count column .query_exec gates on.
    person_pat <- "(^|_)(n_persons|num_persons|n_total|n_deaths|person_id)$"
    person_col <- grep(person_pat, count_cols, ignore.case = TRUE, value = TRUE)
    person_col <- if (length(person_col) > 0) person_col[[1]] else NULL
    unit <- if (!is.null(person_col)) "person" else "record"

    mode <- tolower(q$mode %||% "aggregate")

    # A template is cohort/table-scopable ONLY if its author placed a
    # person-level @cohort hook in the SQL AND declared the person column that
    # hook filters on. Anything else is honestly non-scopable: scoping it would
    # require wrapping aggregated output (which has no person_id), so we never
    # claim a capability we cannot honour. See .omopAnalysisInjectCohort.
    scope_col <- q$scope_column
    scopable <- !is.null(scope_col) && nzchar(scope_col) &&
      grepl("@cohort\\b", q$sql)

    name <- paste0("dsomop:", qid)
    entries[[name]] <- .omopAnalysisEntry(
      name        = name,
      description = q$description,
      domain      = tolower(q$group %||% "general"),
      params      = .omopAnalysisParamsFromInputs(q$inputs),
      compute     = list(kind = "sql", sql = q$sql, fn = NULL),
      dependencies = list(tables = character(0), packages = character(0)),
      disclosure  = .omopAnalysisDisclosure(
        unit = if (identical(mode, "assign")) "person" else unit,
        count_cols = count_cols,
        person_id_col = person_col
      ),
      scope = .omopAnalysisScope(accepts_cohort = scopable,
                                 accepts_tables = scopable,
                                 max_tables = if (scopable) 2L else 0L),
      mode  = if (identical(mode, "assign")) "assign" else "aggregate",
      meta  = list(adapter = "query", query_id = qid,
                   inputs_df = q$inputs, strict = strict,
                   scope_column = scope_col)
    )
  }
  entries
}

# --- Adapter 2: Achilles (live-computing native ports) -----------------------

#' Emit catalog entries for the Achilles analyses (all live-computing)
#'
#' One entry per legacy Achilles analysis id, named \code{"dsomop:achilles.<id>"},
#' but every entry now COMPUTES its metric LIVE from the CDM rather than reading a
#' precomputed \code{achilles_results}/\code{achilles_results_dist} row. The
#' read-precomputed catalog compute path (the old wrappers around
#' \code{.achillesGetResults}/\code{.achillesGetDistributions}) has been removed:
#' the entire family is now assembled by aggregating the per-group live registrars
#' in the \code{pack_achilles_*.R} files. Each registrar returns its entries keyed
#' by the SAME stable id, so existing references keep resolving — only the compute
#' changed (read-results -> live-compute, scopable, disclosure-safe through the
#' single \code{\link{.omopAnalysisGate}}).
#'
#' The standalone Achilles results API (\code{omopAchillesResultsDS} /
#' \code{omopAchillesDistributionDS} and their \code{.achilles*} backends) is a
#' SEPARATE feature for reading a site's own precomputed Achilles tables and is
#' untouched by this catalog change.
#'
#' The five groups cover disjoint id sets whose union is exactly the legacy
#' Achilles catalog, so concatenation needs no overlay/dedup:
#' \itemize{
#'   \item ACH-A person-domain counts (1/2/3/4/5/10/12 + 2000..2003) —
#'     \code{pack_achilles_person.R};
#'   \item ACH-B/ACH-C observation-period counts (101/102/108/109/113) +
#'     distributions (103..107) — \code{pack_achilles_obsperiod.R};
#'   \item ACH-D persons-with->=1 / by-month / by-year + death (person-unit) —
#'     \code{pack_achilles_counts.R};
#'   \item ACH-E record counts (x01/xx20/505/1818, record-unit) —
#'     \code{pack_achilles_records.R};
#'   \item ACH-F/ACH-G/ACH-H distinct-concepts-per-person + age + drug-field
#'     distributions (dist-unit) — \code{pack_achilles_dist.R}.
#' }
#'
#' @param handle CDM handle (passed to each registrar; no DB I/O at build time).
#' @return List of \code{omop_analysis_entry} objects keyed by entry name.
#' @keywords internal
.omopAnalysisAchillesEntries <- function(handle) {
  c(
    .omopAchillesPersonEntries(handle),
    .omopAchillesObsPeriodEntries(handle),
    .omopAchillesConceptCountEntries(handle),
    .omopAchillesRecordCountEntries(handle),
    .omopAchillesDistEntries(handle)
  )
}

# --- Adapter 3: OHDSI result tables ------------------------------------------

#' Emit catalog entries for the generic OHDSI result tables
#'
#' One entry per (tool, table) pair in \code{\link{.ohdsi_tool_registry}}, named
#' \code{"dsomop:ohdsi.<tool>.<table>"}. \code{count_cols} and the person basis
#' come from the registry entry verbatim (so they stay in sync with the registry
#' the rest of the OHDSI path uses), and the entry carries the tool id in
#' \code{meta} so the unified gate can delegate to \code{\link{.ohdsiPersonGate}}.
#' Compute is \code{kind="r"} wrapping \code{\link{.ohdsiGetResults}} for the one
#' table. \code{unit="record"} (OHDSI tables count records/events; the person
#' gate is the per-patient basis).
#'
#' Pre-computed result tables have no per-row person key, so the entry's scope
#' rejects cohort/table scoping (enforced in the run path).
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return List of \code{omop_analysis_entry} objects keyed by entry name.
#' @keywords internal
.omopAnalysisOhdsiEntries <- function(handle) {
  registry <- .ohdsi_tool_registry()

  entries <- list()
  for (tid in names(registry)) {
    tool <- registry[[tid]]
    for (tbl in tool$table_names) {
      name <- paste0("dsomop:ohdsi.", tid, ".", tbl)
      entries[[name]] <- .omopAnalysisEntry(
        name        = name,
        description = paste0(tool$tool_name, " result table: ", tbl),
        domain      = "general",
        params      = list(),
        compute     = list(
          kind = "r", sql = NULL,
          fn = function(handle, ctx, params) {
            .ohdsiGetResults(handle, ctx$table_name, tool_id = ctx$tool_id)
          }
        ),
        dependencies = list(tables = tbl, packages = character(0)),
        disclosure = .omopAnalysisDisclosure(
          unit = "record",
          count_cols = tool$count_columns %||% character(0)
        ),
        scope = .omopAnalysisScope(accepts_cohort = FALSE,
                                   accepts_tables = FALSE, max_tables = 0L),
        mode  = "aggregate",
        meta  = list(adapter = "ohdsi", tool_id = tid, table_name = tbl,
                     person_columns = tool$person_columns %||% character(0),
                     precomputed = TRUE)
      )
    }
  }

  # Native live-compute ports OVERLAY the precomputed entries by id: each ported
  # group returns entries keyed by the SAME "dsomop:ohdsi.<tool>.<table>" so it
  # replaces the read-results version above with one that COMPUTES the metric live
  # from the CDM (this is the single aggregation point; the L2834 c(...) list
  # stays as-is). Per-group registrars live in their own ohdsi_pack_*.R files
  # (groups port in parallel with no shared-file contention); further groups
  # overlay here as they are ported (id sets are disjoint, so overlay order is
  # immaterial).
  # OHDSI-B1 (result tables whose metric a canonical native diagnostic already
  # computes live -> thin delegates) lives in ohdsi_pack_aliases.R.
  alias_live <- .ohdsiPackAliasEntries(handle)
  entries[names(alias_live)] <- alias_live
  # OHDSI-B2 (CohortDiagnostics cohort_count / temporal prevalence + dist /
  # time_series / included_source_concepts + Characterization c_cohort_counts)
  # lives in ohdsi_pack_cohortdx.R. It overlays the precomputed registry ids with
  # live-compute versions AND adds the new canonical natives (dsomop:cohortdx.*)
  # those registry ids alias.
  cohortdx_live <- .ohdsiPackCohortDxEntries(handle)
  entries[names(cohortdx_live)] <- cohortdx_live
  # OHDSI-B2-cohortmethod (CohortMethod cm_attrition + cm_result substrate+MDRR)
  # lives in ohdsi_pack_cohortmethod.R. It overlays the precomputed cohort_method
  # registry ids with live-compute versions (the read-precomputed cm_result /
  # cm_attrition path is now dead for these two ids); the new canonical short ids
  # dsomop:cm.attrition / dsomop:cm.mdrr are registered in
  # .omopAnalysisDiagnosticEntries (which owns the dsomop:cm.* family).
  cohortmethod_live <- .ohdsiPackCohortMethodEntries(handle)
  entries[names(cohortmethod_live)] <- cohortmethod_live
  # OHDSI-B2-characterization-new (Characterization c_dechallenge_rechallenge ->
  # live attempt/success person counts + reconciled percentages) lives in
  # ohdsi_pack_characterization.R. It overlays the precomputed registry id with a
  # live-compute alias of the new canonical dsomop:char.dechallenge_rechallenge
  # (registered in .omopAnalysisDiagnosticEntries, which owns the dsomop:char.*
  # family); the read-precomputed c_dechallenge_rechallenge path is now dead.
  characterization_live <- .ohdsiPackCharacterizationEntries(handle)
  entries[names(characterization_live)] <- characterization_live
  # OHDSI-B2-plp (plp_attrition -> live population-construction attrition; a thin
  # delegate to the new canonical dsomop:plp.attrition registered in
  # .omopAnalysisDiagnosticEntries). The other plp_* tables need a fitted-model
  # predicted-risk column the federation cannot produce and stay on the legacy
  # read-precomputed adapter (untouched), so this overlays ONLY plp_attrition.
  plp_live <- .ohdsiPackPlpEntries(handle)
  entries[names(plp_live)] <- plp_live
  # OHDSI-B2-treatmentpatterns-new (the TreatmentPatterns treatment-sequence
  # pathways / percentage-treated / era-duration analyses) lives in
  # ohdsi_pack_treatmentpatterns.R. TreatmentPatterns has no precomputed registry
  # result table, so these are NEW canonical natives (dsomop:txpath.*) the overlay
  # simply ADDS (their ids are disjoint from the precomputed registry ids).
  txpath_live <- .ohdsiPackTreatmentPatternsEntries(handle)
  entries[names(txpath_live)] <- txpath_live
  # OHDSI-B2-sccs-new (SelfControlledCaseSeries sccs_attrition / sccs_result /
  # sccs_covariate_result / sccs_diagnostics_summary -> live descriptive substrate)
  # lives in ohdsi_pack_sccs.R. It overlays the precomputed sccs registry ids with
  # live-compute aliases (the read-precomputed sccs_* path -- which returned the
  # FITTED Poisson IRR + CI -- is now dead for these ids) AND adds the new canonical
  # natives (dsomop:sccs.*) those aliases delegate to.
  sccs_live <- .ohdsiPackSccsEntries(handle)
  entries[names(sccs_live)] <- sccs_live
  entries
}

# --- Adapter 4 (native reference): r-in-session aggregates -------------------

#' Emit native r-in-session reference analyses
#'
#' A minimal, ALWAYS-present \code{kind="r"} entry that exercises the
#' r-in-session scoped path end-to-end: it computes its aggregate over the
#' SCOPED cohort using OUR connection and gated SQL, and returns it through the
#' ONE gate — the template every natively re-implemented OHDSI aggregate
#' (FeatureExtraction-style covariate counts, etc.) follows, with no dependency
#' on those packages being installed.
#'
#' \code{"dsomop:demo.person_count_by_gender"}: distinct-person counts by gender
#' over the scoped cohort (cohort-wide when no scope). It honours the
#' \code{compute$fn} contract documented in \code{\link{.omopAnalysisRun}}:
#' joins \code{ctx$scoped_cohort} on \code{subject_id} when non-NULL, calls
#' \code{\link{.assertMinPersons}} on the scoped+filtered population BEFORE
#' materialising, and returns an aggregate-only frame (no person key). Gender is
#' translated to its concept name (translation default ON). \code{unit="person"}
#' so the gate small-cell-suppresses and bands the returned counts.
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return List with the single reference \code{omop_analysis_entry}.
#' @keywords internal
.omopAnalysisDemoEntries <- function(handle) {
  name <- "dsomop:demo.person_count_by_gender"
  fn <- function(handle, ctx, params) {
    person <- .qualifyTable(handle, "person")
    concept <- .qualifyTable(handle, "concept",
                             handle$vocab_schema %||% handle$cdm_schema)

    # Restrict to the scoped cohort with an INNER JOIN on subject_id (the cohort
    # keys its person on subject_id); cohort-wide when no scope was supplied.
    join_clause <- ""
    if (!is.null(ctx$scoped_cohort)) {
      cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
      join_clause <- paste0(" INNER JOIN ", cohort,
                            " coh ON coh.subject_id = p.person_id")
    }

    # Gate the scoped+filtered population BEFORE pulling any rows into R.
    count_sql <- .sql_translate(
      paste0("SELECT COUNT(DISTINCT p.person_id) AS n FROM ", person, " p",
             join_clause),
      handle$target_dialect)
    .assertMinPersons(handle = handle, sql = count_sql)

    sql <- .sql_translate(
      paste0("SELECT p.gender_concept_id, c.concept_name AS gender_name, ",
             "COUNT(DISTINCT p.person_id) AS n_persons FROM ", person, " p",
             join_clause,
             " LEFT JOIN ", concept,
             " c ON c.concept_id = p.gender_concept_id",
             " GROUP BY p.gender_concept_id, c.concept_name",
             " ORDER BY n_persons DESC"),
      handle$target_dialect)
    .executeQuery(handle, sql)
  }

  list(`dsomop:demo.person_count_by_gender` = .omopAnalysisEntry(
    name        = name,
    description = "Distinct-person counts by gender over the scoped cohort.",
    domain      = "person",
    params      = list(),
    compute     = list(kind = "r", sql = NULL, fn = fn),
    dependencies = list(tables = c("person", "concept"),
                        packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(unit = "person",
                                          count_cols = "n_persons"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "demo", accepts_cohort = TRUE)
  ))
}

# --- Adapter 5 (native): re-implemented OHDSI diagnostics --------------------
#
# Native re-implementations of OHDSI/HADES diagnostics (CohortIncidence,
# CohortDiagnostics, Characterization, CohortMethod) as kind="r" catalog entries.
# We do NOT depend on those packages: each entry computes its aggregate over OUR
# connection with gated SQL, restricted to the SCOPED cohort (the cohort IS the
# analysis population), and returns an aggregate-only frame that the ONE gate
# (.omopAnalysisGate) small-cell-suppresses + bands. The cohort temp table the
# run path hands the fn via ctx$scoped_cohort always carries subject_id +
# cohort_start_date (see .cohortMaterialize / .cohortFromTokenFrame), so
# index-date-relative computations (incidence, time-to-event, follow-up,
# visit-context) are expressible against the scope. cohort_end_date is present on
# the canonical OMOP cohort table and the observation-period-backed token-frame
# cohorts, but a cohort materialised from a source table without an _end_date
# column (measurement/observation) has only the index date; the TAR-based
# diagnostics resolve the end column via .omopCohortEndDateCol and fall back to
# cohort_start_date there (a single-day window) rather than emit broken SQL.

#' Dialect-aware whole-day difference expression
#'
#' Day count between two date expressions (\code{end_expr - start_expr}). Mirrors
#' the per-dialect branching the profiling module uses for date functions
#' (\code{\link{.sql_translate}} translates only DATEADD/TOP, not date diffs).
#' SQLite uses \code{julianday()}; everything else uses ANSI date subtraction
#' (Postgres returns an integer day count) with an explicit DAY extraction for
#' the engines that return an interval.
#'
#' @param handle CDM handle (selects the dialect).
#' @param end_expr Character; SQL date expression for the later date.
#' @param start_expr Character; SQL date expression for the earlier date.
#' @return Character; a SQL scalar expression evaluating to an integer day count.
#' @keywords internal
.omopDateDiffDays <- function(handle, end_expr, start_expr) {
  dialect <- handle$target_dialect %||% ""
  if (identical(dialect, "sqlite")) {
    paste0("CAST(julianday(", end_expr, ") - julianday(", start_expr,
           ") AS INTEGER)")
  } else if (dialect %in% c("sql server", "redshift")) {
    paste0("DATEDIFF(day, ", start_expr, ", ", end_expr, ")")
  } else if (identical(dialect, "bigquery")) {
    paste0("DATE_DIFF(", end_expr, ", ", start_expr, ", DAY)")
  } else {
    # PostgreSQL/duckdb and friends: date - date yields an integer day count.
    paste0("CAST(", end_expr, " - ", start_expr, " AS INTEGER)")
  }
}

#' Dialect-aware year-of-event expression (for calendar-year strata)
#' @keywords internal
.omopYearExpr <- function(handle, date_expr) {
  dialect <- handle$target_dialect %||% ""
  if (identical(dialect, "sqlite")) {
    paste0("CAST(strftime('%Y', ", date_expr, ") AS INTEGER)")
  } else {
    paste0("CAST(EXTRACT(YEAR FROM ", date_expr, ") AS INTEGER)")
  }
}

#' Resolve the outcome population SQL for a re-implemented diagnostic
#'
#' The OHDSI diagnostics that need an OUTCOME (incidence rate, time-to-event)
#' take it as a single \code{outcome_concept_id} (descendants expanded
#' server-side via \code{\link{.resolveConceptSet}} so a phenotype concept like
#' "Type 2 diabetes" pulls its whole sub-tree). The outcome is matched in an
#' OMOP event table chosen by \code{domain_code} (default condition). Returns the
#' qualified table, its person/date/concept columns, and the concept-id IN-list
#' (or NULL when no valid concept id was supplied).
#'
#' @param handle CDM handle.
#' @param outcome_concept_id Integer-ish scalar (sanitized literal) or NULL.
#' @param domain_code Character/int code selecting the event domain
#'   (0 condition, 1 drug, 2 procedure, 3 measurement, 4 observation).
#' @return list(table, person_col, date_col, concept_col, id_list) or NULL.
#' @keywords internal
.omopOutcomeSource <- function(handle, outcome_concept_id, domain_code = "0") {
  if (is.null(outcome_concept_id)) return(NULL)
  ids <- .resolveConceptSet(handle,
                            list(concepts = as.integer(outcome_concept_id),
                                 include_descendants = TRUE))
  if (length(ids) == 0) {
    ids <- suppressWarnings(as.integer(outcome_concept_id))
    ids <- ids[!is.na(ids)]
  }
  if (length(ids) == 0) return(NULL)

  dom <- as.character(domain_code %||% "0")
  spec <- switch(dom,
    "1" = list("drug_exposure", "drug_concept_id", "drug_exposure_start_date"),
    "2" = list("procedure_occurrence", "procedure_concept_id", "procedure_date"),
    "3" = list("measurement", "measurement_concept_id", "measurement_date"),
    "4" = list("observation", "observation_concept_id", "observation_date"),
    list("condition_occurrence", "condition_concept_id", "condition_start_date"))

  list(
    table       = .qualifyTable(handle, spec[[1]]),
    person_col  = "person_id",
    date_col    = spec[[3]],
    concept_col = spec[[2]],
    id_list     = paste(ids, collapse = ", ")
  )
}

#' INNER JOIN clause onto the scoped cohort (or "" when cohort-wide)
#'
#' Every native diagnostic restricts its population to \code{ctx$scoped_cohort}
#' the same way: an INNER JOIN on \code{subject_id}. NULL scope -> "" (cohort-wide).
#' The cohort identifier is re-validated here as defence-in-depth.
#'
#' @param ctx Run-path ctx.
#' @param alias Character; the alias of the table being joined to the cohort.
#' @param person_col Character; that table's person column (default person_id).
#' @return list(join = "...", cohort = "<validated table or NULL>").
#' @keywords internal
.omopScopeJoin <- function(ctx, alias, person_col = "person_id") {
  if (is.null(ctx$scoped_cohort)) return(list(join = "", cohort = NULL))
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  list(
    join = paste0(" INNER JOIN ", cohort, " coh ON coh.subject_id = ",
                  alias, ".", person_col),
    cohort = cohort
  )
}

#' Pre-pull person-count self-gate over the scoped + filtered population
#'
#' The mandatory defence-in-depth every native diagnostic fn runs BEFORE pulling
#' any rows into R: assert the scoped population (optionally further restricted by
#' \code{where_sql}) rests on >= nfilter.subset distinct persons, so a fn can
#' never "pull 2 people and summarise". Reuses \code{\link{.assertMinPersons}}.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (for the scope join).
#' @param base_table Character; qualified table to count persons over.
#' @param alias Character; alias for base_table.
#' @param person_col Character; person column on base_table.
#' @param where_sql Character; optional extra WHERE predicate (no leading AND).
#' @return invisible(TRUE) or stops fail-closed.
#' @keywords internal
.omopDiagAssertPersons <- function(handle, ctx, base_table, alias,
                                   person_col = "person_id", where_sql = NULL) {
  sj <- .omopScopeJoin(ctx, alias, person_col)
  where <- if (!is.null(where_sql) && nzchar(where_sql)) {
    paste0(" WHERE ", where_sql)
  } else ""
  sql <- .sql_translate(
    paste0("SELECT COUNT(DISTINCT ", alias, ".", person_col, ") AS n FROM ",
           base_table, " ", alias, sj$join, where),
    handle$target_dialect)
  .assertMinPersons(handle = handle, sql = sql)
}

#' Resolve the scoped cohort's end-date column (falling back to the index date)
#'
#' The TAR-based diagnostics (incidence, follow-up, time-in-cohort) anchor a
#' window on the cohort end date. The canonical OMOP \code{cohort} table and the
#' observation-period-backed token-frame cohorts always carry
#' \code{cohort_end_date}, but a cohort materialised by \code{\link{.cohortCreate}}
#' from a source table without an \code{_end_date} column (e.g. measurement or
#' observation) carries only \code{subject_id} + \code{cohort_start_date}. Rather
#' than emit broken SQL (\code{no such column: cohort_end_date}) for such a
#' cohort, resolve the end-date column once here: return \code{"cohort_end_date"}
#' when the scoped cohort actually has it, else fall back to
#' \code{"cohort_start_date"} (a zero-length, single-day window — the standard
#' OHDSI convention when no exit date is defined). The probe is a
#' \code{WHERE 1=0} metadata-only SELECT (no rows read), so it is cheap and
#' dialect-agnostic.
#'
#' @param handle CDM handle.
#' @param cohort Character; the validated scoped cohort table name.
#' @return Character; \code{"cohort_end_date"} or \code{"cohort_start_date"}.
#' @keywords internal
.omopCohortEndDateCol <- function(handle, cohort) {
  has_end <- tryCatch({
    .executeQuery(handle, paste0(
      "SELECT cohort_end_date FROM ", cohort, " WHERE 1 = 0"))
    TRUE
  }, error = function(e) FALSE)
  if (isTRUE(has_end)) "cohort_end_date" else "cohort_start_date"
}

# --- Native diagnostic 1: incidence rate -------------------------------------

#' \code{dsomop:incidence.rate} entry (CohortIncidence, re-implemented)
#' @keywords internal
.omopDiagIncidenceRate <- function() {
  name <- "dsomop:incidence.rate"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = stratum, y = rate)) +",
    "    ggplot2::geom_col() +",
    "    ggplot2::labs(x = 'Stratum', y = 'Incidence rate (per person-time)')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    person  <- .qualifyTable(handle, "person")
    concept <- .qualifyTable(handle, "concept",
                             handle$vocab_schema %||% handle$cdm_schema)

    outcome_id   <- params$outcome_concept_id
    domain_code  <- params$domain_code %||% "0"
    tar_start    <- as.integer(params$tar_start_offset %||% "0")
    tar_end      <- as.integer(params$tar_end_offset %||% "0")
    anchor_start <- params$tar_anchor_start %||% "start"  # start|end of cohort
    anchor_end   <- params$tar_anchor_end %||% "end"
    by_age       <- identical(params$strat_by_age %||% "0", "1")
    by_gender    <- identical(params$strat_by_gender %||% "0", "1")
    by_year      <- identical(params$strat_by_year %||% "0", "1")
    age_band     <- max(as.integer(params$age_band_years %||% "10"), 1L)

    # The scoped cohort IS the at-risk population; an un-scoped run has no
    # population to compute over, so return an empty (gate-safe) frame.
    if (is.null(ctx$scoped_cohort)) return(data.frame())
    cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")

    # Self-gate the at-risk population before materialising.
    .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
      "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
      handle$target_dialect))

    end_col <- .omopCohortEndDateCol(handle, cohort)
    anchor <- function(a) if (identical(a, "end")) end_col else "cohort_start_date"
    tar_lo <- paste0("DATEADD(day, ", tar_start, ", c.", anchor(anchor_start), ")")
    tar_hi <- paste0("DATEADD(day, ", tar_end, ", c.", anchor(anchor_end), ")")
    pdays  <- .omopDateDiffDays(handle, tar_hi, tar_lo)

    # Strata expressions.
    strata_sel <- c("'all' AS stratum")
    grp <- character(0)
    if (by_gender) {
      strata_sel <- "gc.concept_name AS stratum"
      grp <- "gc.concept_name"
    }
    if (by_age) {
      age_at <- .omopDateDiffDays(handle, paste0("c.", anchor(anchor_start)),
                                  "p.birth_dt")
      ageband <- paste0("(CAST(", age_at, " / 365 AS INTEGER) / ", age_band,
                        ") * ", age_band)
      strata_sel <- if (by_gender) {
        paste0("gc.concept_name || '|' || CAST(", ageband, " AS VARCHAR) AS stratum")
      } else paste0("CAST(", ageband, " AS VARCHAR) AS stratum")
      grp <- c(grp, ageband)
    }
    if (by_year) {
      yr <- .omopYearExpr(handle, paste0("c.", anchor(anchor_start)))
      strata_sel <- if (length(grp) > 0) {
        paste0(sub(" AS stratum$", "", strata_sel), " || '|' || CAST(", yr,
               " AS VARCHAR) AS stratum")
      } else paste0("CAST(", yr, " AS VARCHAR) AS stratum")
      grp <- c(grp, yr)
    }
    group_by <- if (length(grp) > 0) paste0(" GROUP BY ", paste(grp, collapse = ", ")) else ""

    # person birth date (year-01-01) for age; gender concept name.
    birth_dt <- if (identical(handle$target_dialect %||% "", "sqlite")) {
      "(CAST(p.year_of_birth AS VARCHAR) || '-01-01')"
    } else {
      "CAST((CAST(p.year_of_birth AS VARCHAR) || '-01-01') AS DATE)"
    }

    out_src <- .omopOutcomeSource(handle, outcome_id, domain_code)
    outcome_join <- ""
    out_event_expr <- "0"
    out_person_expr <- "NULL"
    if (!is.null(out_src)) {
      # outcomes (records) and person_outcomes (distinct persons) within TAR.
      outcome_join <- paste0(
        " LEFT JOIN ", out_src$table, " o ON o.", out_src$person_col,
        " = c.subject_id AND o.", out_src$concept_col, " IN (", out_src$id_list,
        ") AND o.", out_src$date_col, " >= ", tar_lo,
        " AND o.", out_src$date_col, " <= ", tar_hi)
      out_event_expr  <- paste0("COUNT(o.", out_src$concept_col, ")")
      out_person_expr <- paste0("COUNT(DISTINCT o.", out_src$person_col, ")")
    }

    sql <- .sql_translate(paste0(
      "SELECT ", strata_sel, ", ",
      "COUNT(DISTINCT c.subject_id) AS persons_at_risk, ",
      "SUM(", pdays, ") AS person_days, ",
      out_event_expr, " AS outcomes, ",
      out_person_expr, " AS person_outcomes ",
      "FROM ", cohort, " c ",
      "INNER JOIN ", person, " p ON p.person_id = c.subject_id ",
      "LEFT JOIN ", concept, " gc ON gc.concept_id = p.gender_concept_id",
      outcome_join,
      group_by,
      " ORDER BY persons_at_risk DESC"),
      handle$target_dialect)
    # Splice the dialect-specific birth-date expression in last (it is author SQL,
    # not user input, and is kept out of @param substitution).
    sql <- gsub("p.birth_dt", birth_dt, sql, fixed = TRUE)

    df <- .executeQuery(handle, sql)
    if (!is.data.frame(df) || nrow(df) == 0) return(df)

    # Both derived ratios — proportion = person_outcomes / persons_at_risk
    # (cumulative incidence) and rate = person_outcomes / person_days — must be
    # recomputed from BANDED inputs (or NA'd when a side is suppressed) so a raw
    # high-precision ratio over banded counts can never re-derive an un-banded
    # numerator. The shared numerator (person_outcomes) is banded ONCE here and
    # used for BOTH ratios; the gate's own band pass over the count columns is
    # idempotent on these already-banded values.
    settings <- .omopDisclosureSettings()
    bw  <- settings$nfilter_band
    thr <- settings$nfilter_tab
    suppress <- function(raw) {
      v <- vapply(as.numeric(raw), .bandCount, numeric(1), band_width = bw)
      v[is.na(raw) | as.numeric(raw) < thr] <- NA_real_
      v
    }
    b_outc <- suppress(df$person_outcomes)
    b_risk <- suppress(df$persons_at_risk)
    b_days <- suppress(df$person_days)
    prop <- b_outc / b_risk
    prop[is.na(b_outc) | is.na(b_risk) | b_risk == 0] <- NA_real_
    rate <- b_outc / b_days
    rate[is.na(b_outc) | is.na(b_days) | b_days == 0] <- NA_real_
    df$person_outcomes <- b_outc
    df$persons_at_risk <- b_risk
    df$person_days     <- b_days
    df$proportion      <- prop
    df$rate            <- rate
    df
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Incidence rate over a scoped at-risk cohort: ",
                         "persons-at-risk, person-days, outcomes, ",
                         "person-outcomes, cumulative-incidence proportion and ",
                         "rate, optionally by age/gender/calendar-year strata."),
    domain      = "general",
    params      = list(
      list(name = "outcome_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Outcome concept id (descendants expanded)."),
      list(name = "domain_code", type = "enum", required = FALSE, default = "0",
           choices = c("0", "1", "2", "3", "4"),
           description = "Outcome domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "tar_start_offset", type = "int", required = FALSE, default = "0"),
      list(name = "tar_end_offset", type = "int", required = FALSE, default = "0"),
      list(name = "tar_anchor_start", type = "enum", required = FALSE,
           default = "start", choices = c("start", "end")),
      list(name = "tar_anchor_end", type = "enum", required = FALSE,
           default = "end", choices = c("start", "end")),
      list(name = "clean_window", type = "int", required = FALSE, default = "0"),
      list(name = "strat_by_age", type = "bool", required = FALSE, default = "0"),
      list(name = "strat_by_gender", type = "bool", required = FALSE, default = "0"),
      list(name = "strat_by_year", type = "bool", required = FALSE, default = "0"),
      list(name = "age_band_years", type = "int", required = FALSE, default = "10")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "bar", code = plot_code)
    ),
    dependencies = list(tables = c("person", "concept", "condition_occurrence"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "record",
      count_cols = c("outcomes", "persons_at_risk", "person_outcomes",
                     "person_days"),
      person_id_col = "person_outcomes"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native diagnostic 2: index-event breakdown ------------------------------

#' \code{dsomop:cohortdx.index_event_breakdown} entry (CohortDiagnostics)
#' @keywords internal
.omopDiagIndexEventBreakdown <- function() {
  name <- "dsomop:cohortdx.index_event_breakdown"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(concept_name, concept_count),",
    "                                   y = concept_count)) +",
    "    ggplot2::geom_col() + ggplot2::coord_flip() +",
    "    ggplot2::labs(x = 'Index concept', y = 'Records at index')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    if (is.null(ctx$scoped_cohort)) return(data.frame())
    cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
    concept <- .qualifyTable(handle, "concept",
                             handle$vocab_schema %||% handle$cdm_schema)
    top_n   <- max(as.integer(params$top_n %||% "25"), 1L)
    dom <- as.character(params$domain_code %||% "0")
    spec <- switch(dom,
      "1" = list("drug_exposure", "drug_concept_id", "drug_exposure_start_date"),
      "2" = list("procedure_occurrence", "procedure_concept_id", "procedure_date"),
      "3" = list("measurement", "measurement_concept_id", "measurement_date"),
      "4" = list("observation", "observation_concept_id", "observation_date"),
      list("condition_occurrence", "condition_concept_id", "condition_start_date"))
    tbl <- .qualifyTable(handle, spec[[1]])

    # Events occurring ON the cohort index date, per concept.
    on_index <- paste0("e.", spec[[3]], " = c.cohort_start_date")
    .omopDiagAssertPersons(handle, ctx, tbl, "e", "person_id")

    sql <- .sql_translate(paste0(
      "SELECT e.", spec[[2]], " AS concept_id, cc.concept_name, ",
      "COUNT(*) AS concept_count, ",
      "COUNT(DISTINCT e.person_id) AS subject_count ",
      "FROM ", tbl, " e ",
      "INNER JOIN ", cohort, " c ON c.subject_id = e.person_id AND ", on_index,
      " LEFT JOIN ", concept, " cc ON cc.concept_id = e.", spec[[2]],
      " GROUP BY e.", spec[[2]], ", cc.concept_name",
      " ORDER BY concept_count DESC"),
      handle$target_dialect)

    df <- .executeQuery(handle, sql)
    if (is.data.frame(df) && nrow(df) > top_n) df <- df[seq_len(top_n), , drop = FALSE]
    df
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Per-concept record count and distinct-subject count of",
                         " the events occurring on the cohort index date."),
    domain      = "general",
    params      = list(
      list(name = "domain_code", type = "enum", required = FALSE, default = "0",
           choices = c("0", "1", "2", "3", "4"),
           description = "Index event domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "top_n", type = "int", required = FALSE, default = "25")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "bar", code = plot_code)
    ),
    dependencies = list(tables = c("condition_occurrence", "concept"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "record",
      count_cols = c("concept_count", "subject_count"),
      person_id_col = "subject_count"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native diagnostic 3: time distribution ----------------------------------

#' \code{dsomop:cohortdx.time_distribution} entry (CohortDiagnostics)
#' @keywords internal
.omopDiagTimeDistribution <- function() {
  name <- "dsomop:cohortdx.time_distribution"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = metric, y = median_value)) +",
    "    ggplot2::geom_boxplot(ggplot2::aes(lower = p25_value, upper = p75_value,",
    "      middle = median_value, ymin = p10_value, ymax = p90_value),",
    "      stat = 'identity') +",
    "    ggplot2::labs(x = 'Metric', y = 'Days')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    if (is.null(ctx$scoped_cohort)) return(data.frame())
    cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
    obs <- .qualifyTable(handle, "observation_period")
    metric <- params$metric %||% "time_in_cohort"
    end_col <- .omopCohortEndDateCol(handle, cohort)

    # Per-person day value depending on the metric.
    value_expr <- switch(metric,
      "prior_observation" =
        .omopDateDiffDays(handle, "c.cohort_start_date", "op.observation_period_start_date"),
      "post_observation" =
        .omopDateDiffDays(handle, "op.observation_period_end_date", "c.cohort_start_date"),
      .omopDateDiffDays(handle, paste0("c.", end_col), "c.cohort_start_date"))

    .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
      "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
      handle$target_dialect))

    # Per-person values, then summarise in R into a dist row (snake_case so the
    # gate's dist branch strips min/max + masks stats below nfilter_dist).
    join_obs <- if (metric %in% c("prior_observation", "post_observation")) {
      paste0(" INNER JOIN ", obs, " op ON op.person_id = c.subject_id")
    } else ""
    vsql <- .sql_translate(paste0(
      "SELECT ", value_expr, " AS v FROM ", cohort, " c", join_obs),
      handle$target_dialect)
    vals <- .executeQuery(handle, vsql)
    v <- suppressWarnings(as.numeric(vals$v))
    v <- v[!is.na(v)]
    if (length(v) == 0) {
      return(data.frame(metric = character(0), count_value = integer(0),
                        stringsAsFactors = FALSE))
    }
    qs <- stats::quantile(v, c(.10, .25, .5, .75, .90), names = FALSE, type = 7)
    data.frame(
      metric       = metric,
      count_value  = length(v),
      min_value    = min(v),   # stripped by the gate
      max_value    = max(v),   # stripped by the gate
      avg_value    = mean(v),
      stdev_value  = stats::sd(v),
      p10_value    = qs[1], p25_value = qs[2], median_value = qs[3],
      p75_value    = qs[4], p90_value = qs[5],
      stringsAsFactors = FALSE
    )
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Distribution (days) of prior-observation, ",
                         "post-observation, or time-in-cohort over the scoped ",
                         "cohort."),
    domain      = "general",
    params      = list(
      list(name = "metric", type = "enum", required = FALSE,
           default = "time_in_cohort",
           choices = c("prior_observation", "post_observation", "time_in_cohort"))
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "box", code = plot_code)
    ),
    dependencies = list(tables = c("observation_period"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "dist", count_cols = "count_value", min_max = TRUE),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native diagnostic 4: follow-up distribution -----------------------------

#' \code{dsomop:cm.followup_distribution} entry (CohortMethod)
#' @keywords internal
.omopDiagFollowupDistribution <- function() {
  name <- "dsomop:cm.followup_distribution"
  plot_code <- paste(
    "function(df, params) {",
    "  q <- data.frame(p = c(.10,.25,.5,.75,.90),",
    "    v = c(df$p10_value, df$p25_value, df$median_value,",
    "          df$p75_value, df$p90_value))",
    "  ggplot2::ggplot(q, ggplot2::aes(x = v, y = p)) +",
    "    ggplot2::geom_step() +",
    "    ggplot2::labs(x = 'Time at risk (days)', y = 'Cumulative proportion')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    if (is.null(ctx$scoped_cohort)) return(data.frame())
    cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
    tar_start    <- as.integer(params$tar_start_offset %||% "0")
    tar_end      <- as.integer(params$tar_end_offset %||% "0")
    anchor_start <- params$tar_anchor_start %||% "start"
    anchor_end   <- params$tar_anchor_end %||% "end"
    end_col <- .omopCohortEndDateCol(handle, cohort)
    anchor <- function(a) if (identical(a, "end")) end_col else "cohort_start_date"

    .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
      "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
      handle$target_dialect))

    tar_lo <- paste0("DATEADD(day, ", tar_start, ", c.", anchor(anchor_start), ")")
    tar_hi <- paste0("DATEADD(day, ", tar_end, ", c.", anchor(anchor_end), ")")
    tar    <- .omopDateDiffDays(handle, tar_hi, tar_lo)

    vsql <- .sql_translate(paste0(
      "SELECT ", tar, " AS v FROM ", cohort, " c"),
      handle$target_dialect)
    vals <- .executeQuery(handle, vsql)
    v <- suppressWarnings(as.numeric(vals$v))
    v <- v[!is.na(v)]
    if (length(v) == 0) {
      return(data.frame(metric = character(0), count_value = integer(0),
                        stringsAsFactors = FALSE))
    }
    # p10..p90 ONLY — NEVER native 0%/100% (those ARE min/max).
    qs <- stats::quantile(v, c(.10, .25, .5, .75, .90), names = FALSE, type = 7)
    data.frame(
      metric       = "time_at_risk",
      count_value  = length(v),
      avg_value    = mean(v),
      stdev_value  = stats::sd(v),
      p10_value    = qs[1], p25_value = qs[2], median_value = qs[3],
      p75_value    = qs[4], p90_value = qs[5],
      stringsAsFactors = FALSE
    )
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Quantiles (p10-p90) of time-at-risk (days) over the ",
                         "scoped cohort. Min/max are never emitted."),
    domain      = "general",
    params      = list(
      list(name = "tar_start_offset", type = "int", required = FALSE, default = "0"),
      list(name = "tar_end_offset", type = "int", required = FALSE, default = "0"),
      list(name = "tar_anchor_start", type = "enum", required = FALSE,
           default = "start", choices = c("start", "end")),
      list(name = "tar_anchor_end", type = "enum", required = FALSE,
           default = "end", choices = c("start", "end"))
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "step", code = plot_code)
    ),
    dependencies = list(tables = character(0), packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "dist", count_cols = "count_value", min_max = TRUE),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native diagnostic 5: time to event --------------------------------------

#' \code{dsomop:char.time_to_event} entry (Characterization)
#' @keywords internal
.omopDiagTimeToEvent <- function() {
  name <- "dsomop:char.time_to_event"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = day_offset, y = num_events)) +",
    "    ggplot2::geom_col() +",
    "    ggplot2::labs(x = 'Days from index', y = 'Outcome events')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    if (is.null(ctx$scoped_cohort)) return(data.frame())
    cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
    outcome_id  <- params$outcome_concept_id
    domain_code <- params$domain_code %||% "0"
    out_src <- .omopOutcomeSource(handle, outcome_id, domain_code)
    # No outcome supplied -> nothing to histogram; return a gate-safe empty frame.
    if (is.null(out_src)) return(data.frame())
    scale <- as.integer(params$time_scale %||% "1")  # bin width in days
    if (!scale %in% c(1L, 30L, 365L)) scale <- 1L
    first_only <- identical(params$first_occurrence_only %||% "0", "1")

    .omopDiagAssertPersons(handle, ctx, out_src$table, "o", out_src$person_col,
                           where_sql = paste0("o.", out_src$concept_col,
                                              " IN (", out_src$id_list, ")"))

    # Day-offset from index, binned by `scale`. For first-occurrence-only, reduce
    # each person to their earliest qualifying event via a correlated MIN.
    offset_expr <- .omopDateDiffDays(handle, paste0("o.", out_src$date_col),
                                     "c.cohort_start_date")
    bin_expr <- paste0("(CAST(", offset_expr, " AS INTEGER) / ", scale, ") * ", scale)
    first_clause <- if (first_only) {
      paste0(" AND o.", out_src$date_col, " = (SELECT MIN(o2.", out_src$date_col,
             ") FROM ", out_src$table, " o2 WHERE o2.", out_src$person_col,
             " = o.", out_src$person_col, " AND o2.", out_src$concept_col,
             " IN (", out_src$id_list, "))")
    } else ""

    sql <- .sql_translate(paste0(
      "SELECT ", bin_expr, " AS day_offset, ",
      "COUNT(*) AS num_events, ",
      "COUNT(DISTINCT o.", out_src$person_col, ") AS persons ",
      "FROM ", out_src$table, " o ",
      "INNER JOIN ", cohort, " c ON c.subject_id = o.", out_src$person_col,
      " AND o.", out_src$concept_col, " IN (", out_src$id_list, ")", first_clause,
      " GROUP BY ", bin_expr,
      " ORDER BY day_offset"),
      handle$target_dialect)

    .executeQuery(handle, sql)
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Outcome-event histogram by day-offset from the cohort ",
                         "index date (1/30/365-day bins) over the scoped cohort ",
                         "and an outcome concept."),
    domain      = "general",
    params      = list(
      list(name = "outcome_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Outcome concept id (descendants expanded)."),
      list(name = "domain_code", type = "enum", required = FALSE, default = "0",
           choices = c("0", "1", "2", "3", "4")),
      list(name = "time_scale", type = "enum", required = FALSE, default = "1",
           choices = c("1", "30", "365"),
           description = "Bin width in days (1, 30, or 365)."),
      list(name = "first_occurrence_only", type = "bool", required = FALSE,
           default = "0")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "line", code = plot_code)
    ),
    dependencies = list(tables = c("condition_occurrence"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "record", count_cols = c("num_events", "persons"),
      person_id_col = "persons"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native diagnostic 6: visit context --------------------------------------

#' \code{dsomop:cohortdx.visit_context} entry (CohortDiagnostics)
#' @keywords internal
.omopDiagVisitContext <- function() {
  name <- "dsomop:cohortdx.visit_context"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = concept_name, y = subjects,",
    "                                   fill = position)) +",
    "    ggplot2::geom_col() + ggplot2::coord_flip() +",
    "    ggplot2::labs(x = 'Visit concept', y = 'Subjects', fill = 'Index')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    if (is.null(ctx$scoped_cohort)) return(data.frame())
    cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
    visit <- .qualifyTable(handle, "visit_occurrence")
    concept <- .qualifyTable(handle, "concept",
                             handle$vocab_schema %||% handle$cdm_schema)
    top_n <- max(as.integer(params$top_n %||% "25"), 1L)

    .omopDiagAssertPersons(handle, ctx, visit, "v", "person_id")

    # Position of the visit relative to the cohort index date.
    pos <- paste0(
      "CASE WHEN v.visit_end_date < c.cohort_start_date THEN 'before' ",
      "WHEN v.visit_start_date > c.cohort_start_date THEN 'after' ",
      "WHEN v.visit_start_date = c.cohort_start_date THEN 'on' ",
      "ELSE 'during' END")

    sql <- .sql_translate(paste0(
      "SELECT v.visit_concept_id AS concept_id, vc.concept_name, ",
      pos, " AS position, ",
      "COUNT(DISTINCT v.person_id) AS subjects ",
      "FROM ", visit, " v ",
      "INNER JOIN ", cohort, " c ON c.subject_id = v.person_id",
      " LEFT JOIN ", concept, " vc ON vc.concept_id = v.visit_concept_id",
      " GROUP BY v.visit_concept_id, vc.concept_name, ", pos,
      " ORDER BY subjects DESC"),
      handle$target_dialect)

    df <- .executeQuery(handle, sql)
    if (is.data.frame(df) && nrow(df) > top_n) df <- df[seq_len(top_n), , drop = FALSE]
    df
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Distinct subjects per (visit concept x position ",
                         "before/during/on/after the cohort index date)."),
    domain      = "general",
    params      = list(
      list(name = "top_n", type = "int", required = FALSE, default = "25")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "bar", code = plot_code)
    ),
    dependencies = list(tables = c("visit_occurrence", "concept"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "person", count_cols = "subjects"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native FeatureExtraction / Characterization helpers ---------------------
#
# The six FeatureExtraction-/Characterization-/PatientLevelPrediction-style ports
# below all summarise covariates over the SCOPED cohort. They share two compute
# kernels so the per-covariate prevalence (binary) and per-covariate continuous
# statistics are produced identically wherever they appear (Table 1 reuses both).
# Both kernels self-gate the scoped population BEFORE materialising and return an
# aggregate-only frame the ONE gate (.omopAnalysisGate) then suppresses + bands.

#' Map a covariate domain code to its OMOP event table + concept/date columns
#'
#' The binary/continuous covariate kernels select their event source by the same
#' \code{domain_code} vocabulary the diagnostics use (0 condition, 1 drug,
#' 2 procedure, 3 measurement, 4 observation), so a caller scopes a covariate
#' family with one parameter. Returns the qualified table and its person/concept/
#' date columns (and, for measurement, the numeric value column).
#'
#' @param handle CDM handle.
#' @param domain_code Character/int domain selector.
#' @return list(table, person_col, concept_col, date_col, value_col).
#' @keywords internal
.omopCovariateSource <- function(handle, domain_code = "0") {
  dom <- as.character(domain_code %||% "0")
  spec <- switch(dom,
    "1" = list("drug_exposure", "drug_concept_id", "drug_exposure_start_date", NA),
    "2" = list("procedure_occurrence", "procedure_concept_id", "procedure_date", NA),
    "3" = list("measurement", "measurement_concept_id", "measurement_date",
               "value_as_number"),
    "4" = list("observation", "observation_concept_id", "observation_date",
               "value_as_number"),
    list("condition_occurrence", "condition_concept_id", "condition_start_date",
         NA))
  list(
    table       = .qualifyTable(handle, spec[[1]]),
    person_col  = "person_id",
    concept_col = spec[[2]],
    date_col    = spec[[3]],
    value_col   = spec[[4]]
  )
}

#' Per-covariate distinct-person prevalence over the scoped cohort (binary kernel)
#'
#' One row per concept in the chosen domain: \code{sum_value} = distinct persons
#' in the scoped cohort with >= 1 record of that concept, \code{average} =
#' \code{sum_value / cohort_size} (the cohort denominator is a single distinct-
#' person count, so the gate's binary-prevalence coupling NAs \code{average}
#' wherever \code{sum_value} is suppressed). Concepts are translated to their
#' names (translation default ON). \code{unit="person"}: the returned count IS a
#' distinct-person count, so small-cell suppression + banding fully gate it.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param domain_code Character/int domain selector.
#' @param top_n Integer; keep the top-N concepts by prevalence.
#' @return Data frame (covariate_id, covariate_name, domain, sum_value, average).
#' @keywords internal
.omopCovariatePrevalence <- function(handle, ctx, domain_code = "0", top_n = 50L) {
  if (is.null(ctx$scoped_cohort)) return(data.frame())
  cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  src     <- .omopCovariateSource(handle, domain_code)
  top_n   <- max(as.integer(top_n %||% 50L), 1L)

  .omopDiagAssertPersons(handle, ctx, src$table, "e", src$person_col)

  # Cohort denominator (distinct persons) as a scalar sub-select reused per row.
  denom_sql <- paste0("(SELECT COUNT(DISTINCT subject_id) FROM ", cohort, ")")
  sql <- .sql_translate(paste0(
    "SELECT e.", src$concept_col, " AS covariate_id, cc.concept_name AS ",
    "covariate_name, COUNT(DISTINCT e.", src$person_col, ") AS sum_value, ",
    "CAST(COUNT(DISTINCT e.", src$person_col, ") AS FLOAT) / ", denom_sql,
    " AS average ",
    "FROM ", src$table, " e ",
    "INNER JOIN ", cohort, " c ON c.subject_id = e.", src$person_col,
    " LEFT JOIN ", concept, " cc ON cc.concept_id = e.", src$concept_col,
    " GROUP BY e.", src$concept_col, ", cc.concept_name",
    " ORDER BY sum_value DESC"),
    handle$target_dialect)

  df <- .executeQuery(handle, sql)
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  if (nrow(df) > top_n) df <- df[seq_len(top_n), , drop = FALSE]
  df$domain <- as.character(domain_code %||% "0")
  df
}

#' Per-covariate continuous statistics over the scoped cohort (continuous kernel)
#'
#' Pulls one numeric value per (covariate, person) into R and summarises each
#' covariate into a dist row: \code{count_value} (persons contributing),
#' min/max (stripped by the gate), avg/sd, and p10/p25/median/p75/p90 (masked by
#' the gate below nfilter_dist). snake_case column names so the gate's dist
#' branch handles them. \code{value_kind} selects the value: \code{"value"}
#' (measurement \code{value_as_number}, requires a measurement/observation
#' domain) or \code{"count"} (per-person record count of the concept).
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param domain_code Character/int domain selector.
#' @param value_kind Character; "value" or "count".
#' @param top_n Integer; keep the top-N covariates by person count.
#' @return Data frame of dist rows (one per covariate).
#' @keywords internal
.omopCovariateContinuous <- function(handle, ctx, domain_code = "3",
                                     value_kind = "value", top_n = 50L) {
  if (is.null(ctx$scoped_cohort)) return(data.frame())
  cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  src     <- .omopCovariateSource(handle, domain_code)
  top_n   <- max(as.integer(top_n %||% 50L), 1L)

  .omopDiagAssertPersons(handle, ctx, src$table, "e", src$person_col)

  if (identical(value_kind, "value") && !is.na(src$value_col)) {
    # One numeric value per (covariate, person): the per-person mean of the
    # concept's measurements, so each person contributes one value to the
    # per-covariate distribution (a distinct-person basis for count_value).
    per_person <- paste0(
      "SELECT e.", src$concept_col, " AS covariate_id, e.", src$person_col,
      " AS person_id, AVG(CAST(e.", src$value_col, " AS FLOAT)) AS v ",
      "FROM ", src$table, " e ",
      "INNER JOIN ", cohort, " c ON c.subject_id = e.", src$person_col,
      " WHERE e.", src$value_col, " IS NOT NULL",
      " GROUP BY e.", src$concept_col, ", e.", src$person_col)
  } else {
    # Per-person record count of each concept (a count distribution).
    per_person <- paste0(
      "SELECT e.", src$concept_col, " AS covariate_id, e.", src$person_col,
      " AS person_id, COUNT(*) AS v ",
      "FROM ", src$table, " e ",
      "INNER JOIN ", cohort, " c ON c.subject_id = e.", src$person_col,
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

  parts <- split(raw, raw$covariate_id)
  rows <- lapply(parts, function(p) {
    v  <- p$v
    qs <- stats::quantile(v, c(.10, .25, .5, .75, .90), names = FALSE, type = 7)
    data.frame(
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

#' Demographic prevalence rows (gender / age-group / race / ethnicity)
#'
#' The binary demographic block shared by Table 1 / prevalence: one row per
#' demographic level, \code{sum_value} = distinct persons in the scoped cohort at
#' that level, \code{average} = proportion of the cohort. Age-group is banded into
#' decades from \code{year_of_birth} relative to the cohort index date. Gender /
#' race / ethnicity are translated to concept names. \code{unit="person"}.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @return Data frame (characteristic, level, sum_value, average).
#' @keywords internal
.omopDemographicPrevalence <- function(handle, ctx) {
  if (is.null(ctx$scoped_cohort)) return(data.frame())
  cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  person  <- .qualifyTable(handle, "person")
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)

  .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
    handle$target_dialect))

  denom_sql <- paste0("(SELECT COUNT(DISTINCT subject_id) FROM ", cohort, ")")
  age_at <- .omopDateDiffDays(handle, "c.cohort_start_date", "p.birth_dt")
  age_grp <- paste0("(CAST(", age_at, " / 365 AS INTEGER) / 10) * 10")

  # Three concept-named demographics + one derived age band, unioned into one
  # (characteristic, level, person-count, proportion) frame.
  blocks <- list(
    list("gender", "gc.concept_name", "p.gender_concept_id"),
    list("race", "rc.concept_name", "p.race_concept_id"),
    list("ethnicity", "ec.concept_name", "p.ethnicity_concept_id"))

  parts <- lapply(blocks, function(b) {
    label_join <- switch(b[[1]],
      gender    = paste0(" LEFT JOIN ", concept, " gc ON gc.concept_id = p.gender_concept_id"),
      race      = paste0(" LEFT JOIN ", concept, " rc ON rc.concept_id = p.race_concept_id"),
      ethnicity = paste0(" LEFT JOIN ", concept, " ec ON ec.concept_id = p.ethnicity_concept_id"))
    sql <- .sql_translate(paste0(
      "SELECT '", b[[1]], "' AS characteristic, ", b[[2]], " AS level, ",
      "COUNT(DISTINCT p.person_id) AS sum_value, ",
      "CAST(COUNT(DISTINCT p.person_id) AS FLOAT) / ", denom_sql, " AS average ",
      "FROM ", person, " p ",
      "INNER JOIN ", cohort, " c ON c.subject_id = p.person_id", label_join,
      " GROUP BY ", b[[2]]),
      handle$target_dialect)
    .executeQuery(handle, sql)
  })

  birth_dt <- if (identical(handle$target_dialect %||% "", "sqlite")) {
    "(CAST(p.year_of_birth AS VARCHAR) || '-01-01')"
  } else {
    "CAST((CAST(p.year_of_birth AS VARCHAR) || '-01-01') AS DATE)"
  }
  age_sql <- .sql_translate(paste0(
    "SELECT 'age_group' AS characteristic, CAST(", age_grp,
    " AS VARCHAR) AS level, COUNT(DISTINCT p.person_id) AS sum_value, ",
    "CAST(COUNT(DISTINCT p.person_id) AS FLOAT) / ", denom_sql, " AS average ",
    "FROM ", person, " p ",
    "INNER JOIN ", cohort, " c ON c.subject_id = p.person_id",
    " GROUP BY ", age_grp),
    handle$target_dialect)
  age_sql <- gsub("p.birth_dt", birth_dt, age_sql, fixed = TRUE)
  parts[[length(parts) + 1L]] <- .executeQuery(handle, age_sql)

  out <- do.call(rbind, parts)
  if (!is.data.frame(out) || nrow(out) == 0) return(data.frame())
  rownames(out) <- NULL
  out
}

# --- Native FE 1: standardized Table 1 ---------------------------------------

#' \code{dsomop:fe.table1} entry (FeatureExtraction createDefaultTable1)
#' @keywords internal
.omopFeTable1 <- function() {
  name <- "dsomop:fe.table1"
  plot_code <- paste(
    "function(df, params) {",
    "  bin <- df[df$unit == 'person', , drop = FALSE]",
    "  ggplot2::ggplot(bin, ggplot2::aes(x = stats::reorder(level, average),",
    "                                    y = average)) +",
    "    ggplot2::geom_col() + ggplot2::coord_flip() +",
    "    ggplot2::facet_wrap(~ characteristic, scales = 'free_y') +",
    "    ggplot2::labs(x = NULL, y = 'Prevalence')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    if (is.null(ctx$scoped_cohort)) return(data.frame())
    top_cond <- max(as.integer(params$top_n_condition %||% "20"), 1L)
    top_drug <- max(as.integer(params$top_n_drug %||% "20"), 1L)

    # Block 1 — demographic prevalence (gender/age-group/race/ethnicity).
    demo <- .omopDemographicPrevalence(handle, ctx)

    # Block 2/3 — condition-group + drug-group prevalence (top-N each).
    cond <- .omopCovariatePrevalence(handle, ctx, domain_code = "0", top_n = top_cond)
    drug <- .omopCovariatePrevalence(handle, ctx, domain_code = "1", top_n = top_drug)

    bin_parts <- list()
    if (is.data.frame(demo) && nrow(demo) > 0) {
      demo$unit <- "person"
      bin_parts[[length(bin_parts) + 1L]] <- demo[, c(
        "characteristic", "level", "sum_value", "average", "unit")]
    }
    cov_to_bin <- function(df, characteristic) {
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
      data.frame(characteristic = characteristic,
                 level = as.character(df$covariate_name),
                 sum_value = df$sum_value, average = df$average,
                 unit = "person", stringsAsFactors = FALSE)
    }
    bin_parts[[length(bin_parts) + 1L]] <- cov_to_bin(cond, "condition")
    bin_parts[[length(bin_parts) + 1L]] <- cov_to_bin(drug, "drug")
    bin_parts <- Filter(Negate(is.null), bin_parts)
    binary <- if (length(bin_parts) > 0) do.call(rbind, bin_parts) else NULL

    # Block 4 — comorbidity (Charlson) + age distributions (dist rows, clamped).
    como <- .omopComorbidityDistribution(handle, ctx, index_type = "charlson")
    aged <- .omopAgeDistribution(handle, ctx)
    dist_parts <- Filter(function(d) is.data.frame(d) && nrow(d) > 0,
                         list(como, aged))
    dist <- if (length(dist_parts) > 0) do.call(rbind, dist_parts) else NULL

    # Mixed-unit frame: person-unit prevalence rows + dist-unit rows in ONE
    # returned frame. The single gate keys its person branch on ONE count column
    # (sum_value) and DROPS any row whose declared count is NA, so the two
    # populations cannot each declare a different count column. We therefore make
    # sum_value the UNIFIED person-basis column across both populations (binary =
    # prevalence numerator; dist = contributing-person count_value) and pre-apply
    # the dist disclosure controls here so the dist rows are already gate-
    # consistent under the person branch: strip min/max, mask sub-nfilter_dist
    # stats, and band count_value in-fn (the gate then suppresses+bands sum_value
    # uniformly). No row is left with an NA in the single gated column.
    if (!is.null(dist)) {
      settings <- .omopDisclosureSettings()
      nfd <- settings$nfilter_dist %||% 10L
      bw  <- settings$nfilter_band
      thr <- settings$nfilter_tab
      dist$min_value <- NULL
      dist$max_value <- NULL
      stat_cols <- intersect(c("avg_value", "stdev_value", "p10_value",
                               "p25_value", "median_value", "p75_value",
                               "p90_value"), names(dist))
      small <- !is.na(dist$count_value) & dist$count_value < nfd
      if (any(small)) dist[small, stat_cols] <- NA_real_
      # Unified person basis + in-fn band of the dist count.
      dist$sum_value <- vapply(as.numeric(dist$count_value), .bandCount,
                               numeric(1), band_width = bw)
      dist$sum_value[is.na(dist$count_value) | dist$count_value < thr] <- NA_real_
      dist$count_value <- dist$sum_value
      dist$average <- NA_real_
      dist$characteristic <- dist$metric
      dist$level <- dist$metric
      dist$unit <- "dist"
    }

    # Bind into one long frame; columns present per unit, NA elsewhere.
    all_cols <- c("characteristic", "level", "unit", "sum_value", "average",
                  "count_value", "avg_value", "stdev_value", "p10_value",
                  "p25_value", "median_value", "p75_value", "p90_value")
    pad <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      for (cc in setdiff(all_cols, names(df))) df[[cc]] <- NA
      df[, all_cols, drop = FALSE]
    }
    out <- do.call(rbind, Filter(Negate(is.null), list(pad(binary), pad(dist))))
    if (is.null(out) || nrow(out) == 0) return(data.frame())
    rownames(out) <- NULL
    out
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Standardized Table 1 over the scoped cohort: ",
                         "gender / age-group / race / ethnicity prevalence, ",
                         "top condition- and drug-group prevalence (unit=person), ",
                         "and Charlson-comorbidity / age distributions ",
                         "(unit=dist, clamped). Mixed-unit rows."),
    domain      = "person",
    params      = list(
      list(name = "top_n_condition", type = "int", required = FALSE,
           default = "20"),
      list(name = "top_n_drug", type = "int", required = FALSE, default = "20")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "forest", code = plot_code)
    ),
    dependencies = list(
      tables = c("person", "concept", "condition_occurrence", "drug_exposure"),
      packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "person", count_cols = "sum_value"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native FE 2: covariate prevalence ---------------------------------------

#' \code{dsomop:fe.prevalence} entry (FeatureExtraction binary covariates)
#' @keywords internal
.omopFePrevalence <- function() {
  name <- "dsomop:fe.prevalence"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(covariate_name,",
    "                                                       average),",
    "                                   y = average)) +",
    "    ggplot2::geom_col() + ggplot2::coord_flip() +",
    "    ggplot2::labs(x = 'Covariate', y = 'Prevalence')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    .omopCovariatePrevalence(handle, ctx,
                             domain_code = params$domain_code %||% "0",
                             top_n = as.integer(params$top_n %||% "50"))
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Per-covariate distinct-person count and proportion ",
                         "over the scoped cohort, for a selectable domain ",
                         "(condition/drug/procedure/measurement/observation). ",
                         "Proportion is coupled to the suppressed numerator."),
    domain      = "general",
    params      = list(
      list(name = "domain_code", type = "enum", required = FALSE, default = "0",
           choices = c("0", "1", "2", "3", "4"),
           description = "Covariate domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "time_windows", type = "int", required = FALSE, default = "0",
           description = "Reserved window selector (0 = anytime in cohort)."),
      list(name = "top_n", type = "int", required = FALSE, default = "50")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "bar", code = plot_code)
    ),
    dependencies = list(tables = c("condition_occurrence", "concept"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "person", count_cols = "sum_value"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native FE 3: continuous covariates --------------------------------------

#' \code{dsomop:fe.continuous} entry (FeatureExtraction continuous covariates)
#' @keywords internal
.omopFeContinuous <- function() {
  name <- "dsomop:fe.continuous"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = covariate_name)) +",
    "    ggplot2::geom_boxplot(ggplot2::aes(lower = p25_value, upper = p75_value,",
    "      middle = median_value, ymin = p10_value, ymax = p90_value),",
    "      stat = 'identity') + ggplot2::coord_flip() +",
    "    ggplot2::labs(x = 'Covariate', y = 'Value')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    metric <- params$metric %||% "measurement_value"
    if (identical(metric, "age")) return(.omopAgeDistribution(handle, ctx))
    if (identical(metric, "time_in_cohort")) {
      # Reuse the time-in-cohort dist (single covariate row).
      return(.omopTimeInCohortDistribution(handle, ctx))
    }
    # Default: per-measurement-concept value distributions.
    .omopCovariateContinuous(handle, ctx,
                             domain_code = params$domain_code %||% "3",
                             value_kind = "value",
                             top_n = as.integer(params$top_n %||% "50"))
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Per-covariate count and avg/sd/median/p10-p90 over the ",
                         "scoped cohort: measurement value distributions, age, ",
                         "or time-in-cohort. Min/max stripped; stats below ",
                         "nfilter_dist masked."),
    domain      = "general",
    params      = list(
      list(name = "metric", type = "enum", required = FALSE,
           default = "measurement_value",
           choices = c("measurement_value", "age", "time_in_cohort")),
      list(name = "domain_code", type = "enum", required = FALSE, default = "3",
           choices = c("3", "4"),
           description = "Value domain for measurement_value (3 measurement,4 observation)."),
      list(name = "top_n", type = "int", required = FALSE, default = "50")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "box", code = plot_code)
    ),
    dependencies = list(tables = c("measurement", "person", "concept"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "dist", count_cols = "count_value", min_max = TRUE),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native FE 4: comorbidity index ------------------------------------------

#' Per-person comorbidity / risk score distribution over the scoped cohort
#'
#' Computes a per-person score by counting how many of the index's component
#' condition-concept GROUPS each cohort member has >= 1 record of (a weight-1
#' approximation of the named score; the disclosure-relevant property — a
#' per-person integer distribution — is identical to the weighted form). The
#' component groups are resolved as concept sets (descendants expanded) so a
#' single seed concept per component pulls its whole sub-tree. Returns a single
#' dist row (snake_case) the gate clamps + masks.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohort}).
#' @param index_type Character; charlson|dcsi|chads2|cha2ds2vasc|hfrs.
#' @return Data frame with one dist row (metric = the index name).
#' @keywords internal
.omopComorbidityDistribution <- function(handle, ctx, index_type = "charlson") {
  if (is.null(ctx$scoped_cohort)) return(data.frame())
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  cond   <- .qualifyTable(handle, "condition_occurrence")
  index_type <- tolower(index_type %||% "charlson")

  # Component seed concepts per index. Descendants are expanded server-side, so
  # each integer below stands for its whole condition sub-tree (one component).
  # These are standard SNOMED roots for the named score's components; an
  # unresolvable seed simply contributes no component (the score is robust to a
  # missing vocabulary branch).
  seeds <- switch(index_type,
    "dcsi"        = c(443767L, 4209145L, 4271003L, 321822L, 4030518L),
    "chads2"      = c(316139L, 320128L, 201820L, 381591L, 313217L),
    "cha2ds2vasc" = c(316139L, 320128L, 201820L, 381591L, 313217L, 321822L),
    "hfrs"        = c(316139L, 4182210L, 201820L, 192671L, 198199L),
    # charlson (default) — 17 component roots (myocardial infarction, CHF,
    # cerebrovascular, dementia, COPD, diabetes, renal, malignancy, etc.).
    c(4329847L, 316139L, 381591L, 4182210L, 4063381L, 255573L, 201820L,
      192671L, 4030518L, 443392L, 4178338L, 432851L, 4212540L, 439727L))

  .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
    handle$target_dialect))

  # One component-membership sub-select per seed, summed per person into a score;
  # cohort members with no component get a 0 (LEFT JOIN / COALESCE).
  comp_exprs <- character(0)
  for (s in seeds) {
    ids <- .resolveConceptSet(handle, list(concepts = s,
                                           include_descendants = TRUE))
    if (length(ids) == 0) ids <- s
    idlist <- paste(ids, collapse = ", ")
    comp_exprs <- c(comp_exprs, paste0(
      "(CASE WHEN EXISTS (SELECT 1 FROM ", cond, " co WHERE co.person_id = ",
      "c.subject_id AND co.condition_concept_id IN (", idlist,
      ")) THEN 1 ELSE 0 END)"))
  }
  score_expr <- paste(comp_exprs, collapse = " + ")

  vsql <- .sql_translate(paste0(
    "SELECT ", score_expr, " AS v FROM ", cohort, " c"),
    handle$target_dialect)
  vals <- .executeQuery(handle, vsql)
  v <- suppressWarnings(as.numeric(vals$v))
  v <- v[!is.na(v)]
  if (length(v) == 0) {
    return(data.frame(metric = character(0), count_value = integer(0),
                      stringsAsFactors = FALSE))
  }
  qs <- stats::quantile(v, c(.10, .25, .5, .75, .90), names = FALSE, type = 7)
  data.frame(
    metric       = paste0(index_type, "_index"),
    count_value  = length(v),
    min_value    = min(v),   # stripped by the gate
    max_value    = max(v),   # stripped by the gate
    avg_value    = mean(v),
    stdev_value  = stats::sd(v),
    p10_value    = qs[1], p25_value = qs[2], median_value = qs[3],
    p75_value    = qs[4], p90_value = qs[5],
    stringsAsFactors = FALSE)
}

#' Per-person age distribution over the scoped cohort (dist row helper)
#' @keywords internal
.omopAgeDistribution <- function(handle, ctx) {
  if (is.null(ctx$scoped_cohort)) return(data.frame())
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  person <- .qualifyTable(handle, "person")

  .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
    handle$target_dialect))

  age_at <- .omopDateDiffDays(handle, "c.cohort_start_date", "p.birth_dt")
  birth_dt <- if (identical(handle$target_dialect %||% "", "sqlite")) {
    "(CAST(p.year_of_birth AS VARCHAR) || '-01-01')"
  } else {
    "CAST((CAST(p.year_of_birth AS VARCHAR) || '-01-01') AS DATE)"
  }
  vsql <- .sql_translate(paste0(
    "SELECT CAST(", age_at, " / 365 AS INTEGER) AS v FROM ", cohort, " c ",
    "INNER JOIN ", person, " p ON p.person_id = c.subject_id"),
    handle$target_dialect)
  vsql <- gsub("p.birth_dt", birth_dt, vsql, fixed = TRUE)
  vals <- .executeQuery(handle, vsql)
  v <- suppressWarnings(as.numeric(vals$v))
  v <- v[!is.na(v)]
  if (length(v) == 0) {
    return(data.frame(metric = character(0), count_value = integer(0),
                      stringsAsFactors = FALSE))
  }
  qs <- stats::quantile(v, c(.10, .25, .5, .75, .90), names = FALSE, type = 7)
  data.frame(
    metric = "age", count_value = length(v),
    min_value = min(v), max_value = max(v),
    avg_value = mean(v), stdev_value = stats::sd(v),
    p10_value = qs[1], p25_value = qs[2], median_value = qs[3],
    p75_value = qs[4], p90_value = qs[5],
    stringsAsFactors = FALSE)
}

#' Per-person time-in-cohort distribution over the scoped cohort (dist helper)
#' @keywords internal
.omopTimeInCohortDistribution <- function(handle, ctx) {
  if (is.null(ctx$scoped_cohort)) return(data.frame())
  cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  end_col <- .omopCohortEndDateCol(handle, cohort)

  .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
    handle$target_dialect))

  diff <- .omopDateDiffDays(handle, paste0("c.", end_col), "c.cohort_start_date")
  vsql <- .sql_translate(paste0(
    "SELECT ", diff, " AS v FROM ", cohort, " c"), handle$target_dialect)
  vals <- .executeQuery(handle, vsql)
  v <- suppressWarnings(as.numeric(vals$v))
  v <- v[!is.na(v)]
  if (length(v) == 0) {
    return(data.frame(metric = character(0), count_value = integer(0),
                      stringsAsFactors = FALSE))
  }
  qs <- stats::quantile(v, c(.10, .25, .5, .75, .90), names = FALSE, type = 7)
  data.frame(
    metric = "time_in_cohort", count_value = length(v),
    min_value = min(v), max_value = max(v),
    avg_value = mean(v), stdev_value = stats::sd(v),
    p10_value = qs[1], p25_value = qs[2], median_value = qs[3],
    p75_value = qs[4], p90_value = qs[5],
    stringsAsFactors = FALSE)
}

#' \code{dsomop:fe.comorbidity_index} entry (FeatureExtraction comorbidity scores)
#' @keywords internal
.omopFeComorbidityIndex <- function() {
  name <- "dsomop:fe.comorbidity_index"
  plot_code <- paste(
    "function(df, params) {",
    "  q <- data.frame(p = c(.10,.25,.5,.75,.90),",
    "    v = c(df$p10_value, df$p25_value, df$median_value,",
    "          df$p75_value, df$p90_value))",
    "  ggplot2::ggplot(q, ggplot2::aes(x = factor(p), y = v)) +",
    "    ggplot2::geom_col() +",
    "    ggplot2::labs(x = 'Quantile', y = 'Comorbidity score')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    .omopComorbidityDistribution(handle, ctx,
                                 index_type = params$index_type %||% "charlson")
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Distribution of a comorbidity / risk score ",
                         "(Charlson / DCSI / CHADS2 / CHA2DS2-VASc / HFRS) over ",
                         "the scoped cohort. Min/max clamped; stats below ",
                         "nfilter_dist masked."),
    domain      = "condition",
    params      = list(
      list(name = "index_type", type = "enum", required = FALSE,
           default = "charlson",
           choices = c("charlson", "dcsi", "chads2", "cha2ds2vasc", "hfrs"))
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "histogram", code = plot_code)
    ),
    dependencies = list(tables = c("condition_occurrence"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "dist", count_cols = "count_value", min_max = TRUE),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native Characterization 5: target covariates ----------------------------

#' \code{dsomop:char.target_covariates} entry (Characterization aggregate covariates)
#' @keywords internal
.omopCharTargetCovariates <- function() {
  name <- "dsomop:char.target_covariates"
  plot_code <- paste(
    "function(df, params) {",
    "  bin <- df[df$kind == 'binary', , drop = FALSE]",
    "  ggplot2::ggplot(bin, ggplot2::aes(x = stats::reorder(covariate_name,",
    "                                                        average),",
    "                                   y = average)) +",
    "    ggplot2::geom_col() + ggplot2::coord_flip() +",
    "    ggplot2::labs(x = 'Covariate', y = 'Prevalence')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    if (is.null(ctx$scoped_cohort)) return(data.frame())
    domain   <- params$domain_code %||% "0"
    top_n    <- as.integer(params$top_n %||% "50")
    min_mean <- suppressWarnings(as.numeric(params$min_mean %||% "0"))
    min_count <- suppressWarnings(as.integer(params$min_count %||% "0"))

    # Binary (prevalence) covariates of the target.
    bin <- .omopCovariatePrevalence(handle, ctx, domain_code = domain,
                                    top_n = top_n)
    bin_out <- NULL
    if (is.data.frame(bin) && nrow(bin) > 0) {
      keep <- !is.na(bin$average) & bin$average >= (min_mean %||% 0)
      bin <- bin[keep, , drop = FALSE]
      if (nrow(bin) > 0) {
        bin_out <- data.frame(
          kind = "binary", covariate_id = bin$covariate_id,
          covariate_name = as.character(bin$covariate_name),
          sum_value = bin$sum_value, average = bin$average,
          count_value = NA_real_, avg_value = NA_real_, stdev_value = NA_real_,
          p10_value = NA_real_, p25_value = NA_real_, median_value = NA_real_,
          p75_value = NA_real_, p90_value = NA_real_,
          stringsAsFactors = FALSE)
      }
    }

    # Continuous covariates of the target (measurement value distributions).
    cont <- .omopCovariateContinuous(handle, ctx, domain_code = "3",
                                     value_kind = "value", top_n = top_n)
    cont_out <- NULL
    if (is.data.frame(cont) && nrow(cont) > 0) {
      keep <- !is.na(cont$count_value) & cont$count_value >= (min_count %||% 0)
      cont <- cont[keep, , drop = FALSE]
      if (nrow(cont) > 0) {
        cont_out <- data.frame(
          kind = "continuous", covariate_id = cont$covariate_id,
          covariate_name = as.character(cont$covariate_name),
          # sum_value is the UNIFIED person-basis column the single gate keys on
          # (see fe.table1): for a continuous covariate it IS the contributing-
          # person count_value, so a binary and a continuous row both carry a
          # non-NA sum_value and neither population is dropped for an NA count.
          sum_value = cont$count_value, average = NA_real_,
          count_value = cont$count_value, avg_value = cont$avg_value,
          stdev_value = cont$stdev_value, p10_value = cont$p10_value,
          p25_value = cont$p25_value, median_value = cont$median_value,
          p75_value = cont$p75_value, p90_value = cont$p90_value,
          stringsAsFactors = FALSE)
        # Pre-strip the continuous block so the person-unit gate (which does NOT
        # run the dist mask) still releases gate-consistent stats: mask sub-
        # nfilter_dist rows' stats (min/max already absent here) and band the
        # count in-fn (the gate bands sum_value; keep count_value consistent).
        settings <- .omopDisclosureSettings()
        nfd <- settings$nfilter_dist %||% 10L
        bw  <- settings$nfilter_band
        thr <- settings$nfilter_tab
        sc <- c("avg_value", "stdev_value", "p10_value", "p25_value",
                "median_value", "p75_value", "p90_value")
        small <- !is.na(cont_out$count_value) & cont_out$count_value < nfd
        if (any(small)) cont_out[small, sc] <- NA_real_
        banded <- vapply(as.numeric(cont_out$count_value), .bandCount,
                         numeric(1), band_width = bw)
        banded[is.na(cont_out$count_value) | cont_out$count_value < thr] <- NA_real_
        cont_out$count_value <- banded
      }
    }

    out <- do.call(rbind, Filter(Negate(is.null), list(bin_out, cont_out)))
    if (is.null(out) || nrow(out) == 0) return(data.frame())
    rownames(out) <- NULL
    out
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Binary (prevalence) and continuous ",
                         "(count/avg/sd/median/p10-p90) covariates of the scoped ",
                         "cohort, honoring min_mean / min_count thresholds."),
    domain      = "general",
    params      = list(
      list(name = "domain_code", type = "enum", required = FALSE, default = "0",
           choices = c("0", "1", "2", "3", "4"),
           description = "Binary covariate domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "min_mean", type = "number", required = FALSE, default = "0",
           description = "Minimum prevalence for a binary covariate to be returned."),
      list(name = "min_count", type = "int", required = FALSE, default = "0",
           description = "Minimum contributing persons for a continuous covariate."),
      list(name = "top_n", type = "int", required = FALSE, default = "50")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "bar", code = plot_code)
    ),
    dependencies = list(
      tables = c("condition_occurrence", "measurement", "concept"),
      packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "person", count_cols = "sum_value"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native PLP 6: covariate summary (prevalence by outcome status) ----------

#' \code{dsomop:plp.covariate_summary} entry (PatientLevelPrediction CovariateSummary)
#' @keywords internal
.omopPlpCovariateSummary <- function() {
  name <- "dsomop:plp.covariate_summary"
  plot_code <- paste(
    "function(df, params) {",
    "  w <- stats::reshape(df[, c('covariate_name','outcome','average')],",
    "    idvar = 'covariate_name', timevar = 'outcome', direction = 'wide')",
    "  names(w) <- make.names(names(w))",
    "  ggplot2::ggplot(w, ggplot2::aes(x = average.0, y = average.1)) +",
    "    ggplot2::geom_point() +",
    "    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +",
    "    ggplot2::labs(x = 'Prevalence (outcome = 0)',",
    "                  y = 'Prevalence (outcome = 1)')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    if (is.null(ctx$scoped_cohort)) return(data.frame())
    cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
    concept <- .qualifyTable(handle, "concept",
                             handle$vocab_schema %||% handle$cdm_schema)
    domain  <- params$domain_code %||% "0"
    top_n   <- max(as.integer(params$top_n %||% "50"), 1L)
    out_src <- .omopOutcomeSource(handle, params$outcome_concept_id,
                                  params$outcome_domain_code %||% "0")
    # No outcome -> no split to summarise; return a gate-safe empty frame.
    if (is.null(out_src)) return(data.frame())
    cov_src <- .omopCovariateSource(handle, domain)

    # Per-person outcome status (1 if >= 1 outcome record at/after index, else 0),
    # then per-(covariate, outcome-status) distinct-person prevalence. Both group
    # sizes are distinct-person counts (unit="person"), so the gate suppresses +
    # bands them and couples the proportion to the suppressed numerator.
    status <- paste0(
      "(SELECT c.subject_id, (CASE WHEN EXISTS (SELECT 1 FROM ", out_src$table,
      " o WHERE o.", out_src$person_col, " = c.subject_id AND o.",
      out_src$concept_col, " IN (", out_src$id_list, ") AND o.",
      out_src$date_col, " >= c.cohort_start_date) THEN 1 ELSE 0 END) AS outcome ",
      "FROM ", cohort, " c)")

    .omopDiagAssertPersons(handle, ctx, cov_src$table, "e", cov_src$person_col)

    # Per-outcome-arm denominators (distinct persons) for the proportion.
    sql <- .sql_translate(paste0(
      "SELECT e.", cov_src$concept_col, " AS covariate_id, ",
      "cc.concept_name AS covariate_name, s.outcome AS outcome, ",
      "COUNT(DISTINCT e.", cov_src$person_col, ") AS sum_value, ",
      "CAST(COUNT(DISTINCT e.", cov_src$person_col, ") AS FLOAT) / ",
      "(SELECT COUNT(*) FROM ", status, " s2 WHERE s2.outcome = s.outcome) ",
      "AS average ",
      "FROM ", cov_src$table, " e ",
      "INNER JOIN ", status, " s ON s.subject_id = e.", cov_src$person_col,
      " LEFT JOIN ", concept, " cc ON cc.concept_id = e.", cov_src$concept_col,
      " GROUP BY e.", cov_src$concept_col, ", cc.concept_name, s.outcome",
      " ORDER BY sum_value DESC"),
      handle$target_dialect)

    df <- .executeQuery(handle, sql)
    if (!is.data.frame(df) || nrow(df) == 0) return(df)
    # Keep the top-N covariates (by max prevalence across arms) — both arms of a
    # kept covariate are retained so the scatter pairs are complete.
    if (length(unique(df$covariate_id)) > top_n) {
      ord <- stats::aggregate(sum_value ~ covariate_id, data = df, FUN = max)
      ord <- ord[order(-ord$sum_value), , drop = FALSE]
      keep_ids <- ord$covariate_id[seq_len(top_n)]
      df <- df[df$covariate_id %in% keep_ids, , drop = FALSE]
    }
    rownames(df) <- NULL
    df
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Per-covariate distinct-person prevalence split by ",
                         "outcome status (cohort split by an outcome concept ",
                         "set; NO model needed) over the scoped cohort."),
    domain      = "general",
    params      = list(
      list(name = "outcome_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Outcome concept id (descendants expanded) splitting the cohort."),
      list(name = "outcome_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4")),
      list(name = "domain_code", type = "enum", required = FALSE, default = "0",
           choices = c("0", "1", "2", "3", "4"),
           description = "Covariate domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "top_n", type = "int", required = FALSE, default = "50")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "scatter", code = plot_code)
    ),
    dependencies = list(tables = c("condition_occurrence", "concept"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "person", count_cols = "sum_value"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native two-population helpers --------------------------------------------
#
# The three TWO-POPULATION ports below (cohort overlap, risk-factor SMD,
# covariate balance) compare two INDEPENDENT arms. The run path resolves their
# scope PER ELEMENT into ctx$scoped_cohorts (a length-2 vector of two
# independently re-gated cohort temp tables; see .omopAnalysisResolveScopePair)
# rather than folding into one cohort. Each fn reads that pair, self-gates BOTH
# arms BEFORE materialising, and returns an aggregate-only frame the ONE gate
# (.omopAnalysisGate) then suppresses + bands. No entry registers its own gate.

#' Resolve + self-gate the two arms of a two-population analysis
#'
#' Reads \code{ctx$scoped_cohorts} (the length-2 vector the run path resolves for
#' a \code{max_tables == 2} kind="r" entry), validates each identifier, and
#' self-gates BOTH arms with \code{\link{.assertMinPersons}} BEFORE the fn pulls
#' any rows (defence-in-depth on top of the per-arm re-gate already done by
#' \code{\link{.omopAnalysisResolveScopePair}}). Returns NULL when scope was not
#' two-population (so the fn returns a gate-safe empty frame, exactly like the
#' single-cohort ports do for a NULL \code{ctx$scoped_cohort}).
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries \code{scoped_cohorts}).
#' @return list(a = "<validated arm A>", b = "<validated arm B>") or NULL.
#' @keywords internal
.omopTwoPopCohorts <- function(handle, ctx) {
  pair <- ctx$scoped_cohorts
  if (is.null(pair) || length(pair) != 2) return(NULL)
  a <- .validateIdentifier(pair[[1]], "cohort")
  b <- .validateIdentifier(pair[[2]], "cohort")
  for (ct in c(a, b)) {
    .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
      "SELECT COUNT(DISTINCT subject_id) AS n FROM ", ct),
      handle$target_dialect))
  }
  list(a = a, b = b)
}

#' Standardized mean difference between two binary prevalences
#'
#' For a binary covariate the standardized mean difference is
#' \code{(p1 - p2) / sqrt((p1 (1 - p1) + p2 (1 - p2)) / 2)} (the classic
#' Cohen-style SMD on the pooled binomial SD). Computed elementwise; returns
#' \code{NA} where either prevalence is \code{NA} (a suppressed arm) or the
#' pooled SD is zero. Both prevalences MUST already be derived from BANDED
#' counts so the SMD never rests on un-banded inputs.
#'
#' @param p1 Numeric vector; prevalence in arm 1 (from banded counts).
#' @param p2 Numeric vector; prevalence in arm 2 (from banded counts).
#' @return Numeric vector of SMDs (NA where either side is suppressed).
#' @keywords internal
.omopBinarySmd <- function(p1, p2) {
  p1 <- as.numeric(p1); p2 <- as.numeric(p2)
  pooled_sd <- sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
  smd <- (p1 - p2) / pooled_sd
  smd[is.na(p1) | is.na(p2) | is.na(pooled_sd) | pooled_sd == 0] <- NA_real_
  smd
}

#' Per-covariate two-arm prevalence + SMD over banded counts (shared kernel)
#'
#' The compute kernel shared by \code{char.risk_factor_smd} and
#' \code{cm.covariate_balance}: for each concept in the chosen domain it counts
#' distinct persons in arm A and arm B of the scoped cohort pair, BANDS each arm
#' count + each arm size, recomputes each arm's prevalence from the banded
#' numerator / banded arm size, and computes the SMD from those banded
#' prevalences. Both arm counts are returned (and declared as the entry's
#' \code{count_cols}) so the ONE gate drops any covariate where EITHER arm is
#' below threshold (require both group N >= nfilter) and its idempotent band pass
#' is consistent with the in-fn banding; the SMD is already NA wherever an arm is
#' suppressed. Concepts are translated to names (translation default ON).
#'
#' @param handle CDM handle.
#' @param cohort_a,cohort_b Validated arm cohort temp tables.
#' @param domain_code Character/int domain selector.
#' @param top_n Integer; keep the top-N covariates by max arm count.
#' @param a_label,b_label Character; column-name stems for the two arms
#'   (e.g. "case"/"non_case" or "target"/"comparator").
#' @param smd_col Character; name of the SMD output column.
#' @return Data frame (covariate_id, covariate_name, <a>_sum_value,
#'   <b>_sum_value, <a>_average, <b>_average, <smd_col>).
#' @keywords internal
.omopTwoArmCovariateSmd <- function(handle, cohort_a, cohort_b,
                                    domain_code = "0", top_n = 50L,
                                    a_label = "a", b_label = "b",
                                    smd_col = "smd") {
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  src   <- .omopCovariateSource(handle, domain_code)
  top_n <- max(as.integer(top_n %||% 50L), 1L)

  # Distinct-person count per concept per arm, plus each arm size, in one frame.
  arm_sql <- function(cohort, arm) .sql_translate(paste0(
    "SELECT e.", src$concept_col, " AS covariate_id, cc.concept_name AS ",
    "covariate_name, '", arm, "' AS arm, ",
    "COUNT(DISTINCT e.", src$person_col, ") AS sum_value, ",
    "(SELECT COUNT(DISTINCT subject_id) FROM ", cohort, ") AS arm_n ",
    "FROM ", src$table, " e ",
    "INNER JOIN ", cohort, " c ON c.subject_id = e.", src$person_col,
    " LEFT JOIN ", concept, " cc ON cc.concept_id = e.", src$concept_col,
    " GROUP BY e.", src$concept_col, ", cc.concept_name"),
    handle$target_dialect)
  da <- .executeQuery(handle, arm_sql(cohort_a, "a"))
  db <- .executeQuery(handle, arm_sql(cohort_b, "b"))
  if ((!is.data.frame(da) || nrow(da) == 0) &&
      (!is.data.frame(db) || nrow(db) == 0)) {
    return(data.frame())
  }

  settings <- .omopDisclosureSettings()
  bw  <- settings$nfilter_band
  thr <- settings$nfilter_tab
  band <- function(x) {
    v <- vapply(as.numeric(x), .bandCount, numeric(1), band_width = bw)
    v[is.na(as.numeric(x)) | as.numeric(x) < thr] <- NA_real_
    v
  }

  # Union the concept universe; an arm with no record of a concept => 0 persons.
  ids <- union(da$covariate_id, db$covariate_id)
  names_map <- stats::setNames(
    c(as.character(da$covariate_name), as.character(db$covariate_name)),
    c(da$covariate_id, db$covariate_id))
  a_cnt <- stats::setNames(da$sum_value, da$covariate_id)
  b_cnt <- stats::setNames(db$sum_value, db$covariate_id)
  a_n <- band(if (nrow(da) > 0) da$arm_n[1] else 0L)
  b_n <- band(if (nrow(db) > 0) db$arm_n[1] else 0L)

  a_raw <- as.numeric(a_cnt[as.character(ids)]); a_raw[is.na(a_raw)] <- 0
  b_raw <- as.numeric(b_cnt[as.character(ids)]); b_raw[is.na(b_raw)] <- 0
  a_b <- band(a_raw)
  b_b <- band(b_raw)
  pa <- a_b / a_n; pa[is.na(a_b) | is.na(a_n) | a_n == 0] <- NA_real_
  pb <- b_b / b_n; pb[is.na(b_b) | is.na(b_n) | b_n == 0] <- NA_real_

  out <- data.frame(
    covariate_id   = ids,
    covariate_name = as.character(names_map[as.character(ids)]),
    a_sum_value    = a_b,
    b_sum_value    = b_b,
    a_average      = pa,
    b_average      = pb,
    smd            = .omopBinarySmd(pa, pb),
    stringsAsFactors = FALSE)
  # Keep top-N by the larger of the two (raw) arm counts before the gate prunes.
  ord <- order(-pmax(a_raw, b_raw))
  out <- out[ord, , drop = FALSE]
  if (nrow(out) > top_n) out <- out[seq_len(top_n), , drop = FALSE]
  names(out)[names(out) == "a_sum_value"] <- paste0(a_label, "_sum_value")
  names(out)[names(out) == "b_sum_value"] <- paste0(b_label, "_sum_value")
  names(out)[names(out) == "a_average"]   <- paste0(a_label, "_average")
  names(out)[names(out) == "b_average"]   <- paste0(b_label, "_average")
  names(out)[names(out) == "smd"]         <- smd_col
  rownames(out) <- NULL
  out
}

# --- Native two-population 1: cohort overlap ---------------------------------

#' \code{dsomop:cohortdx.cohort_overlap} entry (CohortDiagnostics)
#' @keywords internal
.omopDiagCohortOverlap <- function() {
  name <- "dsomop:cohortdx.cohort_overlap"
  plot_code <- paste(
    "function(df, params) {",
    "  d <- df[df$category %in% c('a_only','both','b_only'), , drop = FALSE]",
    "  d$category <- factor(d$category, levels = c('a_only','both','b_only'))",
    "  ggplot2::ggplot(d, ggplot2::aes(x = '', y = n, fill = category)) +",
    "    ggplot2::geom_col() +",
    "    ggplot2::labs(x = NULL, y = 'Distinct persons', fill = 'Region')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    cohorts <- .omopTwoPopCohorts(handle, ctx)
    if (is.null(cohorts)) return(data.frame())
    a <- cohorts$a; b <- cohorts$b

    # Distinct-person counts of the four overlap regions, each a COUNT(DISTINCT
    # subject_id) over a set predicate. Returned LONG (one count per row) so the
    # ONE gate suppresses + bands EACH region independently (the differencing
    # defence): a sub-threshold region's row is dropped and every surviving count
    # is floored to a multiple of nfilter_band, so no arithmetic over the
    # released regions can recover a suppressed one.
    in_a <- paste0("subject_id IN (SELECT subject_id FROM ", a, ")")
    in_b <- paste0("subject_id IN (SELECT subject_id FROM ", b, ")")
    union_sql <- paste0(
      "SELECT subject_id FROM ", a, " UNION SELECT subject_id FROM ", b)

    count_of <- function(where_sql) {
      v <- .executeQuery(handle, .sql_translate(paste0(
        "SELECT COUNT(DISTINCT subject_id) AS n FROM (", union_sql,
        ") u WHERE ", where_sql), handle$target_dialect))
      as.numeric(v$n[1])
    }
    both   <- count_of(paste0(in_a, " AND ", in_b))
    a_only <- count_of(paste0(in_a, " AND NOT ", in_b))
    b_only <- count_of(paste0(in_b, " AND NOT ", in_a))
    either <- count_of(paste0(in_a, " OR ", in_b))

    data.frame(
      category = c("both", "a_only", "b_only", "either"),
      n        = c(both, a_only, b_only, either),
      stringsAsFactors = FALSE)
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Distinct-person overlap of two scoped populations: ",
                         "both / A-only / B-only / either. unit=person; every ",
                         "region is suppressed + banded independently ",
                         "(differencing defence)."),
    domain      = "general",
    params      = list(),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "bar", code = plot_code)
    ),
    dependencies = list(tables = character(0), packages = character(0)),
    disclosure = .omopAnalysisDisclosure(unit = "person", count_cols = "n"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 2L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native two-population 2: risk-factor SMD --------------------------------

#' \code{dsomop:char.risk_factor_smd} entry (Characterization RiskFactor)
#' @keywords internal
.omopCharRiskFactorSmd <- function() {
  name <- "dsomop:char.risk_factor_smd"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = case_average, y = non_case_average)) +",
    "    ggplot2::geom_point() +",
    "    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +",
    "    ggplot2::labs(x = 'Prevalence (cases)',",
    "                  y = 'Prevalence (non-cases)')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    cohorts <- .omopTwoPopCohorts(handle, ctx)
    if (is.null(cohorts)) return(data.frame())
    domain <- params$domain_code %||% "0"
    top_n  <- as.integer(params$top_n %||% "50")
    # Arm A = cases (the target/outcome population); arm B = non-cases (the
    # comparison population). Both arm counts are banded, both prevalences are
    # recomputed from banded inputs, and the SMD is NA wherever either arm is
    # suppressed (computed inside the shared kernel).
    out <- .omopTwoArmCovariateSmd(handle, cohorts$a, cohorts$b,
                                   domain_code = domain, top_n = top_n,
                                   a_label = "case", b_label = "non_case",
                                   smd_col = "smd")
    if (!is.data.frame(out) || nrow(out) == 0) return(out)
    min_smd <- suppressWarnings(as.numeric(params$min_smd %||% "0"))
    if (!is.na(min_smd) && min_smd > 0) {
      keep <- is.na(out$smd) | abs(out$smd) >= min_smd
      out <- out[keep, , drop = FALSE]
    }
    rownames(out) <- NULL
    out
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Per-covariate case-vs-non-case distinct-person ",
                         "prevalence and standardized mean difference over two ",
                         "scoped populations (target + outcome). unit=person; ",
                         "both arms gated; SMD suppressed if either side is."),
    domain      = "general",
    params      = list(
      list(name = "domain_code", type = "enum", required = FALSE, default = "0",
           choices = c("0", "1", "2", "3", "4"),
           description = "Covariate domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "min_smd", type = "number", required = FALSE, default = "0",
           description = "Keep only covariates with |SMD| >= this (0 = all)."),
      list(name = "top_n", type = "int", required = FALSE, default = "50")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "scatter", code = plot_code)
    ),
    dependencies = list(tables = c("condition_occurrence", "concept"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "person", count_cols = c("case_sum_value", "non_case_sum_value")),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 2L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

# --- Native two-population 3: covariate balance (before matching) ------------

#' \code{dsomop:cm.covariate_balance} entry (CohortMethod, before matching)
#' @keywords internal
.omopCmCovariateBalance <- function() {
  name <- "dsomop:cm.covariate_balance"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = target_average,",
    "                                   y = comparator_average)) +",
    "    ggplot2::geom_point() +",
    "    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +",
    "    ggplot2::labs(x = 'Prevalence (target)',",
    "                  y = 'Prevalence (comparator)')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    cohorts <- .omopTwoPopCohorts(handle, ctx)
    if (is.null(cohorts)) return(data.frame())
    domain <- params$domain_code %||% "0"
    top_n  <- as.integer(params$top_n %||% "50")
    # Arm A = target, arm B = comparator. BEFORE-matching balance only (the
    # federation runs no PS model / matching, so there is no honest "after"
    # column). Std-mean-diff is recomputed from banded inputs in the shared
    # kernel; both arm counts are gated.
    .omopTwoArmCovariateSmd(handle, cohorts$a, cohorts$b,
                            domain_code = domain, top_n = top_n,
                            a_label = "target", b_label = "comparator",
                            smd_col = "std_mean_diff")
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("Per-covariate distinct-person prevalence per group and ",
                         "standardized mean difference (BEFORE matching) over a ",
                         "target + comparator scoped pair. unit=person; both ",
                         "groups gated; SMD recomputed from banded inputs."),
    domain      = "general",
    params      = list(
      list(name = "domain_code", type = "enum", required = FALSE, default = "0",
           choices = c("0", "1", "2", "3", "4"),
           description = "Covariate domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "top_n", type = "int", required = FALSE, default = "50")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = fn,
      plot = list(type = "scatter", code = plot_code)
    ),
    dependencies = list(tables = c("condition_occurrence", "concept"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "person",
      count_cols = c("target_sum_value", "comparator_sum_value")),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 2L),
    mode  = "aggregate",
    meta  = list(adapter = "diagnostic", accepts_cohort = TRUE)
  )
}

#' Emit the native re-implemented OHDSI diagnostic entries
#'
#' All kind="r", scope via \code{ctx$scoped_cohort} (the cohort IS the
#' analysis population), and funnel through the ONE gate
#' (\code{\link{.omopAnalysisGate}}). No entry registers its own gate.
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by name.
#' @keywords internal
.omopAnalysisDiagnosticEntries <- function(handle) {
  entries <- list(
    .omopDiagIncidenceRate(),
    .omopDiagIndexEventBreakdown(),
    .omopDiagTimeDistribution(),
    .omopDiagFollowupDistribution(),
    .omopDiagTimeToEvent(),
    .omopDiagVisitContext(),
    # FeatureExtraction / Characterization / PLP single-cohort ports.
    .omopFeTable1(),
    .omopFePrevalence(),
    .omopFeContinuous(),
    .omopFeComorbidityIndex(),
    .omopCharTargetCovariates(),
    # Characterization dechallenge/rechallenge (canonical native port; lives in
    # ohdsi_pack_characterization.R, which also exposes the legacy OHDSI-id alias).
    .omopCharDechallengeRechallenge(),
    .omopPlpCovariateSummary(),
    # PLP target-population attrition (canonical native port; lives in
    # ohdsi_pack_plp.R, which also exposes the legacy OHDSI-id delegate). Like the
    # other PLP/diagnostic ports it is meaningless un-scoped (returns an empty
    # frame), so it is marked requires_cohort by the forcing pass below.
    .omopPlpAttrition(),
    # Two-population ports (max_tables = 2; resolved via ctx$scoped_cohorts).
    .omopDiagCohortOverlap(),
    .omopCharRiskFactorSmd(),
    .omopCmCovariateBalance()
  )
  # Every native diagnostic computes over the SCOPED cohort (the cohort IS the
  # analysis population): its fn returns an empty frame when un-scoped. Mark them
  # all requires_cohort so the run path turns an un-scoped run into a CLEAR error
  # instead of a silently empty (gate-safe) frame (see .omopAnalysisRun). This is
  # the single place that knows the whole diagnostic family, so the flag never
  # drifts from the fn guard.
  entries <- lapply(entries, function(e) {
    e$scope$requires_cohort <- TRUE
    e
  })
  # Canonical short-id aliases of the live OHDSI CohortMethod ports
  # (ohdsi_pack_cohortmethod.R). They are FAITHFUL re-labels of their
  # "dsomop:ohdsi.cohort_method.*" twins — same compute$fn / disclosure / scope —
  # so they MUST preserve their twin's requires_cohort (cm_attrition keeps the
  # whole-DB requires_cohort=FALSE semantics, cm_result returns a gate-safe empty
  # frame un-scoped). Appended AFTER the forcing pass above so they retain exactly
  # the scope their builders set, never diverging from the twin.
  cm_aliases <- list(
    .omopCmAttritionCanonicalEntry(),
    .omopCmMdrrCanonicalEntry()
  )
  entries <- c(entries, cm_aliases)
  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}

# --- Registry (build-once + cache) -------------------------------------------

#' Warn (never fail) about missing entry dependencies
#'
#' Mirrors the "warn-if-missing" dependency contract: an entry whose declared
#' tables are absent from the DB, or whose declared packages are not installed,
#' is still registered (so it is discoverable), but a single grouped warning is
#' emitted at build time so a data controller sees what is non-runnable.
#'
#' @param handle CDM handle.
#' @param entries List of entries.
#' @return invisible(NULL); emits at most one warning.
#' @keywords internal
.omopAnalysisWarnDeps <- function(handle, entries) {
  bp <- tryCatch(.buildBlueprint(handle), error = function(e) NULL)
  present <- if (!is.null(bp)) bp$tables$table_name[bp$tables$present_in_db] else character(0)

  missing_tbls <- character(0)
  missing_pkgs <- character(0)
  for (e in entries) {
    tabs <- e$dependencies$tables %||% character(0)
    miss_t <- setdiff(tabs, present)
    if (length(miss_t) > 0) missing_tbls <- c(missing_tbls, miss_t)
    for (pkg in (e$dependencies$packages %||% character(0))) {
      if (!requireNamespace(pkg, quietly = TRUE)) missing_pkgs <- c(missing_pkgs, pkg)
    }
  }
  missing_tbls <- unique(missing_tbls)
  missing_pkgs <- unique(missing_pkgs)
  if (length(missing_tbls) > 0 || length(missing_pkgs) > 0) {
    warning("analysis catalog: some entries depend on resources not present: ",
            if (length(missing_tbls) > 0) paste0("tables {",
              paste(missing_tbls, collapse = ", "), "} ") else "",
            if (length(missing_pkgs) > 0) paste0("packages {",
              paste(missing_pkgs, collapse = ", "), "}") else "",
            call. = FALSE)
  }
  invisible(NULL)
}

# --- Adapter 4: pluggable third-party analysis packs -------------------------

#' Scan installed packages for the analysis-pack DESCRIPTION fields (cached)
#'
#' The handle-INDEPENDENT half of pack discovery: a single
#' \code{utils::installed.packages(fields=)} pass reads both opt-in fields for
#' every installed package at once (far cheaper than one
#' \code{packageDescription} per package). The result is cached on
#' \code{.dsomop_env} because the installed package set does not change within an
#' R session — so the disk scan runs once per process, not once per handle. Pass
#' \code{force=TRUE} to rescan (used by tests).
#'
#' @param force Logical; rescan even if cached.
#' @return Data frame with columns \code{pkg}, \code{prefix}, \code{spec} for
#'   each package declaring \code{Config/dsOMOP/AnalysisCollection}.
#' @keywords internal
.omopAnalysisPackScan <- function(force = FALSE) {
  if (!force && exists("analysis_pack_scan", envir = .dsomop_env,
                       inherits = FALSE)) {
    return(get("analysis_pack_scan", envir = .dsomop_env, inherits = FALSE))
  }
  empty <- data.frame(pkg = character(0), prefix = character(0),
                      spec = character(0), stringsAsFactors = FALSE)
  ip <- tryCatch(
    utils::installed.packages(fields = c("Config/dsOMOP/AnalysisCollection",
                                         "Config/dsOMOP/AnalysisPrefix")),
    error = function(e) NULL)
  if (is.null(ip) || nrow(ip) == 0 ||
      !"Config/dsOMOP/AnalysisCollection" %in% colnames(ip)) {
    assign("analysis_pack_scan", empty, envir = .dsomop_env)
    return(empty)
  }
  collection <- ip[, "Config/dsOMOP/AnalysisCollection"]
  prefix <- if ("Config/dsOMOP/AnalysisPrefix" %in% colnames(ip)) {
    ip[, "Config/dsOMOP/AnalysisPrefix"]
  } else rep(NA_character_, nrow(ip))
  keep <- !is.na(collection) & nzchar(trimws(collection))
  scan <- if (any(keep)) {
    pkgs <- ip[keep, "Package"]
    pfx  <- trimws(prefix[keep])
    pfx[is.na(pfx) | !nzchar(pfx)] <- pkgs[is.na(pfx) | !nzchar(pfx)]
    data.frame(pkg = unname(pkgs), prefix = unname(pfx),
               spec = trimws(unname(collection[keep])),
               stringsAsFactors = FALSE)
  } else empty
  assign("analysis_pack_scan", scan, envir = .dsomop_env)
  scan
}

#' Discover and load analysis entries contributed by installed packages
#'
#' Lets a third-party package contribute catalog entries WITHOUT modifying
#' dsOMOP, while keeping the disclosure invariant non-negotiable: a pack entry's
#' \code{compute$fn} output is STILL funnelled through
#' \code{\link{.omopAnalysisGate}} by the run path (a pack cannot register its
#' own gate or bypass the one gate). A pack opts in via two DESCRIPTION fields:
#' \describe{
#'   \item{\code{Config/dsOMOP/AnalysisCollection}}{Required;
#'     \code{"pkg::registrar"} — the function dsOMOP calls as
#'     \code{registrar(handle)} to obtain a named list of entries (each built
#'     with the re-exported \code{\link{.omopAnalysisEntry}}).}
#'   \item{\code{Config/dsOMOP/AnalysisPrefix}}{Optional; the id namespace for
#'     this pack (defaults to the package name). The final id of every entry is
#'     \code{paste0(prefix, ":", id)}.}
#' }
#' Fail-closed rules (never silently overwrite a native or another pack's id):
#' the \code{"dsomop:"} prefix is RESERVED for native entries and a pack
#' claiming it is rejected; a pack producing an id that collides with an
#' already-registered final id is rejected. Both raise a clear error. The
#' (expensive) DESCRIPTION scan is delegated to \code{\link{.omopAnalysisPackScan}}
#' (cached per session); this function only invokes the discovered registrars and
#' enforces the namespacing + collision rules per handle.
#'
#' @param handle CDM handle (passed to each registrar).
#' @param existing Named list of already-registered entries (the three native
#'   adapters), used for cross-pack / native collision detection.
#' @return Named list of \code{omop_analysis_entry} objects keyed by final id.
#' @keywords internal
.omopAnalysisPackEntries <- function(handle, existing = list()) {
  scan <- .omopAnalysisPackScan()
  if (nrow(scan) == 0) return(list())

  taken <- names(existing)
  out <- list()
  for (i in seq_len(nrow(scan))) {
    pkg    <- scan$pkg[i]
    prefix <- scan$prefix[i]
    spec   <- scan$spec[i]

    # The "dsomop:" namespace is reserved for native entries.
    if (identical(tolower(prefix), "dsomop")) {
      stop("Analysis pack '", pkg, "' claims the reserved 'dsomop:' prefix; ",
           "choose a different Config/dsOMOP/AnalysisPrefix.", call. = FALSE)
    }

    # Resolve + call the declared registrar ("pkg::registrar" or "registrar").
    fn_name <- sub("^.*::", "", spec)
    registrar <- tryCatch(
      get(fn_name, envir = asNamespace(pkg), inherits = FALSE),
      error = function(e) NULL)
    if (!is.function(registrar)) {
      warning("Analysis pack '", pkg, "': registrar '", spec,
              "' not found or not a function; skipping.", call. = FALSE)
      next
    }
    pack_entries <- tryCatch(registrar(handle), error = function(e) {
      warning("Analysis pack '", pkg, "': registrar errored (", conditionMessage(e),
              "); skipping.", call. = FALSE)
      NULL
    })
    if (is.null(pack_entries) || length(pack_entries) == 0) next

    for (id in names(pack_entries)) {
      entry <- pack_entries[[id]]
      final_id <- paste0(prefix, ":", id)
      if (final_id %in% taken) {
        stop("Analysis pack '", pkg, "' registers id '", final_id,
             "' which already exists; duplicate analysis ids are not allowed.",
             call. = FALSE)
      }
      # Keep the entry's authoritative name in sync with its final id so the
      # gate/run path and listings agree regardless of what the pack set.
      entry$name <- final_id
      out[[final_id]] <- entry
      taken <- c(taken, final_id)
    }
  }
  out
}

#' Build (once) and cache the unified analysis catalog
#'
#' Assembles the catalog from the three native adapters (QueryLibrary, Achilles,
#' OHDSI — all \code{"dsomop:"} namespaced) and then from any installed analysis
#' PACKS (\code{\link{.omopAnalysisPackEntries}}), keyed by final id. Built once
#' per handle and cached on \code{handle$analysis_catalog} — exactly the
#' build-once + cache pattern used by \code{handle$blueprint}
#' (\code{\link{.buildBlueprint}}) and \code{handle$achilles_gate_cache}. Pass
#' \code{force=TRUE} to rebuild. Pack discovery is fail-closed on duplicate ids
#' and on any pack claiming the reserved \code{"dsomop:"} prefix.
#'
#' @param handle CDM handle.
#' @param force Logical; rebuild even if cached.
#' @return Named list of \code{omop_analysis_entry} objects keyed by name.
#' @keywords internal
.omopAnalysisRegistry <- function(handle, force = FALSE) {
  if (!is.null(handle$analysis_catalog) && !force) {
    return(handle$analysis_catalog)
  }

  native <- c(
    .omopAnalysisQueryEntries(handle),
    .omopAnalysisAchillesEntries(handle),
    .omopAnalysisOhdsiEntries(handle),
    .omopAnalysisDemoEntries(handle),
    .omopAnalysisDiagnosticEntries(handle)
  )

  # Append pack-contributed entries, rejecting any id that collides with a
  # native id or another pack's id (fail-closed; never silently overwrite).
  pack <- .omopAnalysisPackEntries(handle, existing = native)
  entries <- c(native, pack)

  .omopAnalysisWarnDeps(handle, entries)

  handle$analysis_catalog <- entries
  entries
}

#' Resolve a catalog entry by name (fail-closed, bare-name friendly)
#'
#' Accepts the full pack-prefixed id (\code{"dsomop:fe.prevalence"}) and the two
#' shorthands an analyst naturally reaches for, resolving in this order so the
#' exact id always wins and shorthand never silently picks a different entry:
#' \enumerate{
#'   \item exact id match;
#'   \item the native id \code{"dsomop:<name>"} (the common case — drop the pack
#'     prefix, e.g. \code{"fe.prevalence"});
#'   \item a UNIQUE id suffix match on the part after the colon
#'     (e.g. \code{"prevalence"} -> \code{"dsomop:fe.prevalence"}). An ambiguous
#'     suffix lists the candidates and asks the caller to disambiguate.
#' }
#'
#' @param handle CDM handle.
#' @param name Character; the entry id or a bare shorthand for it.
#' @return The matching \code{omop_analysis_entry}; stops if not found/ambiguous.
#' @keywords internal
.omopAnalysisResolve <- function(handle, name) {
  name <- trimws(as.character(name)[[1]])
  registry <- .omopAnalysisRegistry(handle)

  # 1. Exact id.
  entry <- registry[[name]]
  if (!is.null(entry)) return(entry)

  # 2. Native id (drop the implied "dsomop:" prefix) when the caller passed a
  #    bare, colon-less shorthand.
  if (!grepl(":", name, fixed = TRUE)) {
    entry <- registry[[paste0("dsomop:", name)]]
    if (!is.null(entry)) return(entry)

    # 3. Unique suffix match on the id's local part (after the colon), so
    #    "prevalence" resolves "dsomop:fe.prevalence". Fail-closed on ambiguity.
    ids <- names(registry)
    locals <- sub("^[^:]*:", "", ids)
    hits <- ids[locals == name | endsWith(locals, paste0(".", name))]
    if (length(hits) == 1L) return(registry[[hits]])
    if (length(hits) > 1L) {
      stop("Analysis '", name, "' is ambiguous; it matches: ",
           paste(hits, collapse = ", "),
           ". Pass the full id to disambiguate.", call. = FALSE)
    }
  }

  stop("Analysis '", name, "' not found in the analysis catalog.",
       call. = FALSE)
}

# --- Listing / metadata ------------------------------------------------------

#' Flatten a param spec list into a compact character description
#' @keywords internal
.omopAnalysisParamSummary <- function(params) {
  if (length(params) == 0) return("")
  paste(vapply(params, function(p) {
    paste0(p$name, ":", p$type %||% "?",
           if (isTRUE(p$required)) "*" else "")
  }, character(1)), collapse = ", ")
}

#' List catalog entries as a data frame (metadata only)
#'
#' @param handle CDM handle.
#' @param domain Character; optional domain filter.
#' @return Data frame of entry metadata (no SQL, no compute fn).
#' @keywords internal
.omopAnalysisList <- function(handle, domain = NULL) {
  registry <- .omopAnalysisRegistry(handle)

  empty <- data.frame(
    name = character(0), domain = character(0), adapter = character(0),
    mode = character(0), unit = character(0), description = character(0),
    params = character(0), accepts_cohort = logical(0),
    accepts_tables = logical(0), requires_cohort = logical(0),
    has_plot = logical(0), stringsAsFactors = FALSE
  )
  if (length(registry) == 0) return(empty)

  rows <- list()
  for (e in registry) {
    if (!is.null(domain) && !identical(tolower(e$domain), tolower(domain))) next
    rows[[length(rows) + 1L]] <- data.frame(
      name           = e$name,
      domain         = e$domain,
      adapter        = e$meta$adapter %||% "unknown",
      mode           = e$mode,
      unit           = e$disclosure$unit,
      description    = substr(e$description, 1, 200),
      params         = .omopAnalysisParamSummary(e$params),
      accepts_cohort = isTRUE(e$scope$accepts_cohort),
      accepts_tables = isTRUE(e$scope$accepts_tables),
      # Discovery aids: whether an un-scoped run errors (the cohort IS the
      # population) and whether the entry ships a client-side plot recipe.
      requires_cohort = isTRUE(e$scope$requires_cohort),
      has_plot        = !is.null(e$compute$plot),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) return(empty)
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

#' Return one entry's metadata (client-safe; no compute fn / raw SQL)
#'
#' @param handle CDM handle.
#' @param name Character; entry name.
#' @return Named list of entry metadata.
#' @keywords internal
.omopAnalysisGet <- function(handle, name) {
  e <- .omopAnalysisResolve(handle, name)
  list(
    name        = e$name,
    description = e$description,
    domain      = e$domain,
    mode        = e$mode,
    params      = e$params,
    compute_kind = e$compute$kind,
    disclosure  = e$disclosure,
    scope       = e$scope,
    adapter     = e$meta$adapter %||% "unknown",
    # Inert client-side plot recipe (type + a deparsed function(df, params) string).
    # The server NEVER evaluates this; the client renders it locally on the
    # already-gated aggregate. NULL for analyses without a plot.
    plot        = e$compute$plot
  )
}

# --- Parameter validation + sanitization -------------------------------------

#' Validate + sanitize the params supplied for an entry
#'
#' Reuses \code{\link{.sanitizeQueryParam}} (and the template inputs typing) so
#' the catalog run path enforces EXACTLY the same fail-closed numeric-type
#' policy as \code{\link{.query_exec}}: required params must be present, and
#' every supplied value is sanitized to a safe SQL literal. Returns a named list
#' of sanitized string literals keyed by param name.
#'
#' @param entry The catalog entry.
#' @param params Named list of user-supplied values.
#' @return Named list of sanitized literals.
#' @keywords internal
.omopAnalysisSanitizeParams <- function(entry, params) {
  params <- params %||% list()
  specs <- entry$params %||% list()
  spec_names <- vapply(specs, function(p) p$name, character(1))

  # Required-param presence (defaults satisfy requirement).
  for (p in specs) {
    if (isTRUE(p$required) && is.null(params[[p$name]]) && is.null(p$default)) {
      stop("Analysis '", entry$name, "': required parameter '", p$name,
           "' is missing.", call. = FALSE)
    }
  }

  inputs_df <- entry$meta$inputs_df  # NULL for non-query adapters
  spec_by_name <- stats::setNames(specs, spec_names)
  sanitized <- list()
  for (pname in names(params)) {
    val <- params[[pname]]
    if (is.null(val)) next
    # Length guard, same as the query path.
    .validateString(as.character(val)[1])

    # Native r-in-session entries declare richer scalar types than the numeric-
    # only QueryLibrary path. An "enum"/"bool" param is validated by EXACT-MATCH
    # against its declared, fixed allowlist of choices — a value that is not one
    # of the known choices is rejected, so there is no SQL-injection surface (the
    # accepted value is always one of a closed set the entry author wrote, never
    # spliced from free user text). Every other type falls through to the
    # fail-closed numeric sanitizer .query_exec trusts.
    spec <- spec_by_name[[pname]]
    ptype <- if (!is.null(spec)) spec$type %||% "" else ""
    if (identical(ptype, "enum") || identical(ptype, "bool")) {
      choices <- if (identical(ptype, "bool")) {
        c("0", "1", "TRUE", "FALSE", "true", "false")
      } else {
        as.character(spec$choices %||% character(0))
      }
      cand <- trimws(as.character(val)[1])
      if (length(choices) == 0 || !(cand %in% choices)) {
        stop("Analysis '", entry$name, "': parameter '", pname,
             "' must be one of {", paste(choices, collapse = ", "),
             "} but received '", cand, "'.", call. = FALSE)
      }
      sanitized[[pname]] <- if (identical(ptype, "bool")) {
        if (cand %in% c("1", "TRUE", "true")) "1" else "0"
      } else cand
      next
    }
    sanitized[[pname]] <- .sanitizeQueryParam(val, pname, inputs_df)
  }
  sanitized
}

# --- Scoping -----------------------------------------------------------------

#' Resolve the scope= argument to a single server-side cohort temp table
#'
#' \code{scope} may be:
#' \itemize{
#'   \item NULL -> no scoping (returns NULL).
#'   \item a cohort ref (temp-table name or cohort_definition_id) -> resolved and
#'     re-gated via \code{\link{.resolveCohortArg}} (fail-closed on subjects).
#'   \item one or more workspace \code{omop.table} SYMBOLS already resolved to
#'     frames -> each validated as an \code{omop.table} and turned into a cohort
#'     via \code{\link{.cohortFromTokenFrame}}.
#'   \item a mix -> all resolved to cohort temp tables, then folded together with
#'     \code{\link{.cohortCombine}} using \code{combine} (union/intersect on
#'     subject_id) into one scoped temp cohort.
#' }
#' The final scoped cohort is re-gated with \code{\link{.assertMinPersons}}
#' BEFORE it is returned, so a too-small scope can never reach the run path.
#'
#' @param handle CDM handle.
#' @param scope The scope argument (NULL, a cohort ref, an omop.table, or a list
#'   mixing cohort refs and omop.tables).
#' @param combine Character; "union" (default) or "intersect" when folding
#'   multiple scope sources.
#' @return Character cohort temp table name, or NULL.
#' @keywords internal
.omopAnalysisResolveScope <- function(handle, scope = NULL, combine = "union") {
  if (is.null(scope)) return(NULL)

  # Normalise to a list of scope sources. A single omop.table frame is itself a
  # list (a data.frame), so only un-list a plain (non-data.frame) list.
  sources <- if (is.list(scope) && !is.data.frame(scope) &&
                 !.is_omop.table(scope)) {
    scope
  } else {
    list(scope)
  }
  sources <- Filter(Negate(is.null), sources)
  if (length(sources) == 0) return(NULL)

  cohorts <- character(0)
  for (src in sources) {
    if (.is_omop.table(src)) {
      cohorts <- c(cohorts, .cohortFromTokenFrame(handle, src))
    } else {
      ct <- .resolveCohortArg(handle, cohort = src)
      if (!is.null(ct)) cohorts <- c(cohorts, ct)
    }
  }
  cohorts <- cohorts[!is.na(cohorts) & nzchar(cohorts)]
  if (length(cohorts) == 0) return(NULL)

  # Fold multiple cohort sources into one scoped temp cohort on subject_id.
  combine <- match.arg(tolower(combine), c("union", "intersect"))
  scoped <- cohorts[[1]]
  if (length(cohorts) > 1) {
    for (k in 2:length(cohorts)) {
      scoped <- .cohortCombine(handle, combine, scoped, cohorts[[k]],
                               new_name = paste0("dsomop_analysis_scope_",
                                                 sample(100000:999999, 1)))
    }
  }

  # Re-gate the final scope BEFORE running (fail-closed).
  count_sql <- paste0("SELECT COUNT(DISTINCT subject_id) AS n FROM ", scoped)
  .assertMinPersons(handle = handle, sql = count_sql)
  scoped
}

#' Resolve a two-population \code{scope=} into TWO re-gated cohort temp tables
#'
#' Two-population analyses (entries declaring \code{max_tables == 2}: cohort
#' overlap, standardised mean differences, covariate balance) compare two
#' INDEPENDENT arms, so the single-source fold of
#' \code{\link{.omopAnalysisResolveScope}} (which unions/intersects everything
#' into ONE cohort) is wrong for them. This resolver instead splits \code{scope}
#' into exactly two elements and resolves EACH through
#' \code{\link{.omopAnalysisResolveScope}} — so each arm is folded (if it is
#' itself a list) and, critically, independently re-gated with
#' \code{\link{.assertMinPersons}} before it is returned. Neither arm can leak
#' into the other and a too-small arm fails closed here, before the fn runs.
#'
#' \code{scope} must be a length-2 list \code{list(a, b)} where each element is
#' any single-source scope (a cohort ref, an \code{omop.table}, or a sub-list of
#' those to be folded for that arm).
#'
#' @param handle CDM handle.
#' @param scope A length-2 list of scope sources (one per population arm).
#' @param combine Character; fold operator applied WITHIN an arm that is itself
#'   a list of sources ("union" default or "intersect"). The two arms are never
#'   combined with each other.
#' @return Character vector of two cohort temp-table names.
#' @keywords internal
.omopAnalysisResolveScopePair <- function(handle, scope, combine = "union") {
  arms <- if (is.list(scope) && !is.data.frame(scope) &&
              !.is_omop.table(scope)) {
    Filter(Negate(is.null), scope)
  } else {
    list(scope)
  }
  if (length(arms) != 2) {
    stop("Two-population analysis requires exactly two scope elements ",
         "(scope = list(a, b)); received ", length(arms), ".", call. = FALSE)
  }

  # Resolve EACH arm through the single-source resolver: each is folded (if a
  # sub-list) and independently re-gated. A NULL arm (empty source) is rejected
  # — a two-population analysis needs both populations present.
  pair <- vapply(arms, function(arm) {
    ct <- .omopAnalysisResolveScope(handle, arm, combine)
    if (is.null(ct)) {
      stop("Two-population analysis: a population arm resolved to no cohort.",
           call. = FALSE)
    }
    ct
  }, character(1))
  unname(pair)
}

#' Substitute the person-level \code{@cohort} hook in a SQL-entry template
#'
#' SQL entries are scoped HONESTLY: the template author places a \code{@cohort}
#' placeholder in the right person-level \code{WHERE} clause (so it sits BEFORE
#' any \code{GROUP BY}, never around aggregated output) and declares the person
#' column it filters on (\code{scope_column}, e.g. \code{"co.person_id"}). This
#' function realises that hook:
#' \itemize{
#'   \item \code{scoped} given -> \code{@cohort} becomes
#'     \code{ AND <scope_column> IN (SELECT subject_id FROM <scoped>) }, so only
#'     persons in the scoped cohort contribute to the template's own aggregation.
#'   \item \code{scoped} NULL -> \code{@cohort} becomes \code{""} (no predicate),
#'     so an un-scoped run is the bare template.
#' }
#' We NEVER wrap-and-filter the template's output (the old design broke on
#' aggregate templates whose rows have no \code{person_id}). The cohort name is
#' identifier-validated by the caller
#' (\code{\link{.omopAnalysisResolveScope}} -> \code{.resolveCohortArg} /
#' \code{.cohortFromTokenFrame}); we re-validate the column expression and the
#' table name here as a defence-in-depth before the literal splice.
#'
#' @param sql Character; the rendered SQL (schema + params already substituted,
#'   \code{@cohort} still intact).
#' @param scoped Character; the scoped cohort temp table name, or NULL.
#' @param scope_column Character; the person-level column expression the hook
#'   filters on (e.g. \code{"co.person_id"}). Required when \code{scoped} is set.
#' @return The SQL with \code{@cohort} substituted.
#' @keywords internal
.omopAnalysisInjectCohort <- function(sql, scoped, scope_column) {
  has_hook <- grepl("@cohort\\b", sql)

  if (is.null(scoped)) {
    # No scope: the hook is a no-op. (Templates without a hook are unaffected.)
    return(gsub("@cohort\\b", "", sql))
  }

  # Scope requested. The entry must carry a person-level hook + column; the run
  # path already rejected non-scopable entries, so reaching here without one is
  # an internal inconsistency we fail closed on rather than emit broken SQL.
  if (!has_hook || is.null(scope_column) || !nzchar(scope_column)) {
    stop("Analysis scope requested but this SQL has no person-level @cohort ",
         "hook to scope on.", call. = FALSE)
  }
  scoped <- .validateIdentifier(scoped, "cohort")
  # The scope column is an author-declared "alias.person_id" expression, not
  # user input. Validate its shape (optional alias + bare identifier) so a
  # malformed template cannot splice arbitrary SQL into the predicate.
  if (!grepl("^([A-Za-z_][A-Za-z0-9_]*\\.)?[A-Za-z_][A-Za-z0-9_]*$",
             scope_column)) {
    stop("Analysis scope column '", scope_column, "' is not a valid ",
         "column reference.", call. = FALSE)
  }
  predicate <- paste0(" AND ", scope_column, " IN (SELECT subject_id FROM ",
                      scoped, ") ")
  gsub("@cohort\\b", predicate, sql)
}

# --- The single fail-closed disclosure gate ----------------------------------

#' THE unified per-patient disclosure gate for every analysis result
#'
#' This is the single funnel that replaces the three divergent post-run passes
#' (\code{.query_exec}'s person heuristic, \code{.achillesPersonGate},
#' \code{.ohdsiPersonGate}). It generalises them by the entry's declared
#' disclosure \code{unit}:
#' \describe{
#'   \item{\code{"person"}}{The count columns ARE person counts. Small-cell
#'     suppress them (\code{\link{.suppressSmallCounts}}): rows with any person
#'     count below \code{nfilter.tab} are dropped.}
#'   \item{\code{"record"}}{The count columns count records/events, so a large
#'     count can still rest on too few persons. Require a COMPANION
#'     \code{COUNT(DISTINCT person_id)} over the SAME strata and drop rows whose
#'     person basis is below \code{nfilter.subset}. The companion is provided by
#'     the originating gate: Achilles -> \code{\link{.achillesPersonGate}};
#'     OHDSI -> \code{\link{.ohdsiPersonGate}}; SQL -> the declared
#'     \code{person_id_col} (a sibling person-count column). With NO provable
#'     person basis, REJECT in strict mode (reusing the \code{query_strict}
#'     policy) and pass through (cell-suppressed only) otherwise.}
#'   \item{\code{"dist"}}{Distribution rows: mask summary statistics below
#'     \code{nfilter_dist} and never release min/max (clamp). For Achilles this
#'     is already done by \code{\link{.achillesGetDistributions}}; the gate
#'     re-asserts the \code{nfilter_dist} mask and strips any min/max columns
#'     that slipped through.}
#' }
#' Finally — for EVERY unit — every count column is small-cell suppressed and
#' then \code{\link{.bandCount}}-banded (the universal differencing defence), so
#' a count that survives is never released at 1-person resolution. Columns named
#' like \code{*_source_value} / \code{*_source_concept_id} are dropped up front:
#' they are treated as nonexistent regardless of how an adapter produced them.
#'
#' @param handle CDM handle (needed for the record-unit companion gates).
#' @param df Data frame produced by the entry's compute step.
#' @param entry The catalog entry (supplies disclosure spec + adapter meta).
#' @return \code{df} with the per-patient invariant enforced.
#' @keywords internal
.omopAnalysisGate <- function(handle, df, entry) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)

  disc <- entry$disclosure
  settings <- .omopDisclosureSettings()
  strict <- isTRUE(settings$query_strict)

  # source_value / source_concept_id columns are treated as nonexistent.
  drop_cols <- grep("_source_value$|_source_concept_id$", names(df),
                    ignore.case = TRUE, value = TRUE)
  if (length(drop_cols) > 0) {
    df <- df[, setdiff(names(df), drop_cols), drop = FALSE]
  }
  if (nrow(df) == 0 || ncol(df) == 0) return(df)

  count_cols <- intersect(disc$count_cols %||% character(0), names(df))
  unit <- disc$unit %||% "person"

  if (identical(unit, "person")) {
    # Count columns ARE person counts: small-cell suppression fully gates them.
    if (length(count_cols) > 0) {
      df <- .suppressSmallCounts(df, count_cols)
    } else if (strict) {
      stop("Disclosive: analysis '", entry$name, "' returns no identifiable ",
           "person-count column to gate on (strict mode).", call. = FALSE)
    }

  } else if (identical(unit, "record")) {
    # Record counts need a distinct-person companion over the same strata.
    adapter <- entry$meta$adapter %||% ""
    if (identical(adapter, "achilles")) {
      # Reuse the Achilles record gate verbatim (sibling/companion person counts).
      if (any(df$analysis_id %in% .achillesRecordGatedIds())) {
        df <- .achillesPersonGate(handle, df)
      }
      # Cell-suppress the record count itself too (orthogonal small-cell control).
      df <- .suppressSmallCounts(df, count_cols)
    } else if (identical(adapter, "ohdsi")) {
      # Reuse the OHDSI person gate verbatim (registry person columns; strict
      # mode rejects a person-less count basis).
      df <- .suppressSmallCounts(df, count_cols)
      df <- .ohdsiPersonGate(df, entry$meta$tool_id, count_cols)
    } else {
      # SQL record entry: gate on the declared sibling person-count column.
      person_col <- intersect(disc$person_id_col %||% character(0), names(df))
      if (length(person_col) > 0) {
        df <- .suppressSmallCounts(df, person_col)
        df <- .suppressSmallCounts(df, count_cols)
      } else if (strict) {
        # No provable person basis -> reject (reuse query_strict policy).
        stop("Disclosive: analysis '", entry$name, "' has no provable ",
             "distinct-person basis for its record counts (strict mode).",
             call. = FALSE)
      } else {
        df <- .suppressSmallCounts(df, count_cols)
      }
    }

  } else if (identical(unit, "dist")) {
    # Distribution rows: drop small-count rows, mask summary stats below
    # nfilter_dist, and never release min/max. Achilles produces snake_case
    # (min_value/avg_value/...); natively re-implemented OHDSI aggregate
    # analyses produce camelCase (minValue/averageValue/...). Both spellings
    # are handled here so the ONE gate covers either origin.
    if (length(count_cols) > 0) {
      df <- .suppressSmallCounts(df, count_cols)
    }
    df <- df[, setdiff(names(df),
                       grep("^min_value$|^max_value$|^minValue$|^maxValue$",
                            names(df), ignore.case = TRUE, value = TRUE)),
             drop = FALSE]
    nfilter_dist <- settings$nfilter_dist %||% 10L
    # snake_case (Achilles) summary stats, keyed on count_value.
    mask_cols <- intersect(
      c("avg_value", "stdev_value", "p10_value", "p25_value", "median_value",
        "p75_value", "p90_value"),
      names(df))
    if (length(mask_cols) > 0 && "count_value" %in% names(df) && nrow(df) > 0) {
      small <- !is.na(df$count_value) & df$count_value < nfilter_dist
      df[small, mask_cols] <- NA_real_
    }
    # camelCase (native OHDSI aggregate) summary stats, keyed on countValue.
    mask_cols_cc <- intersect(
      c("averageValue", "standardDeviation", "medianValue", "p10Value",
        "p25Value", "p75Value", "p90Value"),
      names(df))
    if (length(mask_cols_cc) > 0 && "countValue" %in% names(df) && nrow(df) > 0) {
      small_cc <- !is.na(df$countValue) & df$countValue < nfilter_dist
      df[small_cc, mask_cols_cc] <- NA_real_
    }
  }

  if (nrow(df) == 0) return(df)

  # UNIVERSAL final pass for every unit: small-cell suppress then band every
  # count column (differencing defence). Banding is idempotent, so an
  # already-suppressed/banded column is unharmed.
  band_cols <- count_cols
  if (identical(unit, "dist") && "count_value" %in% names(df)) {
    band_cols <- union(band_cols, "count_value")
  }
  if (identical(unit, "dist") && "countValue" %in% names(df)) {
    band_cols <- union(band_cols, "countValue")
  }
  band_cols <- intersect(band_cols, names(df))
  if (length(band_cols) > 0) {
    df <- .suppressSmallCounts(df, band_cols)
    for (col in band_cols) {
      df[[col]] <- vapply(df[[col]], .bandCount, numeric(1),
                          band_width = settings$nfilter_band)
    }
  }

  # BINARY-PREVALENCE COUPLING (person unit): a covariate-prevalence row pairs a
  # person numerator (sumValue/sum_value) with its derived proportion
  # (averageValue/average/proportion). If the numerator was suppressed (dropped,
  # or banded) the surviving proportion would still betray the sub-threshold
  # count (numerator = proportion * denominator). Run AFTER the band pass and NA
  # every paired proportion wherever the person count is NA, so a surviving
  # prevalence can never reveal a suppressed numerator. NB .bandCount FLOORS a
  # sub-band count (a raw numerator in [nfilter_tab, nfilter_band)) to 0, not to
  # NA, so a numerator of exactly 0 with a non-zero proportion is the
  # banded-away small count — its proportion must be coupled away too (a true
  # zero numerator has proportion 0, so NA-ing it loses nothing).
  if (identical(unit, "person") && nrow(df) > 0) {
    num_col   <- intersect(c("sumValue", "sum_value"), names(df))
    prop_cols <- intersect(c("averageValue", "average", "proportion"),
                           names(df))
    if (length(num_col) > 0 && length(prop_cols) > 0) {
      num <- df[[num_col[1]]]
      coupled <- is.na(num) | num == 0
      if (any(coupled)) df[coupled, prop_cols] <- NA_real_
    }
  }
  rownames(df) <- NULL
  df
}

#' Reconcile a derived rate/percentage against its BANDED numerator+denominator
#'
#' A record-unit aggregate that returns a derived ratio (a rate, percentage, or
#' proportion = numerator / denominator) must NEVER release the raw ratio: a
#' high-precision ratio over banded counts re-derives the un-banded numerator
#' (raw_num = ratio * denominator), defeating the banding defence. This helper
#' is the single place a record-unit \code{compute$fn} routes such a column
#' through BEFORE returning its frame to \code{\link{.omopAnalysisGate}}:
#' \itemize{
#'   \item it bands BOTH the numerator and denominator with
#'     \code{\link{.bandCount}} (the same \code{nfilter_band} the gate uses);
#'   \item it RECOMPUTES the ratio from those banded counts (optionally scaled,
#'     e.g. \code{scale = 100} for a percentage);
#'   \item it NAs the ratio (and leaves the banded counts) whenever EITHER count
#'     is suppressed (below \code{nfilter_tab}, or NA, or a zero denominator),
#'     so a surviving ratio always rests on two released, banded counts.
#' }
#' The numerator/denominator columns are overwritten with their banded values so
#' the frame the gate then sees is already internally consistent (the gate's
#' own band pass is idempotent over them).
#'
#' @param df Data frame from a record-unit \code{compute$fn}.
#' @param numerator_col Character; the numerator count column name.
#' @param denominator_col Character; the denominator count column name.
#' @param ratio_col Character; the derived rate/percentage column to recompute.
#' @param scale Numeric; multiplier for the recomputed ratio (1 for a
#'   proportion, 100 for a percentage). Default 1.
#' @return \code{df} with banded counts and a reconciled (or NA) ratio column.
#' @keywords internal
.omopAnalysisReconcileRatio <- function(df, numerator_col, denominator_col,
                                        ratio_col, scale = 1) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  needed <- c(numerator_col, denominator_col, ratio_col)
  if (!all(needed %in% names(df))) return(df)

  settings <- .omopDisclosureSettings()
  band <- settings$nfilter_band
  thr  <- settings$nfilter_tab

  raw_num <- as.numeric(df[[numerator_col]])
  raw_den <- as.numeric(df[[denominator_col]])
  num <- vapply(raw_num, .bandCount, numeric(1), band_width = band)
  den <- vapply(raw_den, .bandCount, numeric(1), band_width = band)

  # Either count below threshold (or NA / zero denominator) => suppress both the
  # count and the ratio it would otherwise reveal.
  num[is.na(raw_num) | raw_num < thr] <- NA_real_
  den[is.na(raw_den) | raw_den < thr] <- NA_real_

  ratio <- (num / den) * scale
  ratio[is.na(num) | is.na(den) | den == 0] <- NA_real_

  df[[numerator_col]]   <- num
  df[[denominator_col]] <- den
  df[[ratio_col]]       <- ratio
  df
}

# --- The single fail-closed run path -----------------------------------------

#' Render + execute a SQL-entry, with optional person-scope injection
#'
#' Mirrors \code{\link{.query_exec}}'s render path: substitute the standard
#' schema placeholders, then substitute every \code{@param} with its sanitized
#' literal, inject the scope predicate (if any), translate to the target
#' dialect, and execute. Returns the RAW frame (the gate is applied by the
#' caller).
#'
#' @param handle CDM handle.
#' @param entry SQL catalog entry.
#' @param sanitized Named list of sanitized param literals.
#' @param scoped Character; scoped cohort temp table name, or NULL.
#' @return Data frame (un-gated).
#' @keywords internal
# Render @cdm./@vocab./@results. schema placeholders to qualified or (on SQLite /
# no configured schema) BARE table names, mirroring .qualifyTable. Avoids the old
# hardcoded "public." default that breaks schemaless backends.
.qlRenderSchema <- function(handle, sql) {
  bare <- identical(handle$target_dialect, "sqlite")
  schema_params <- list(
    cdm     = handle$cdm_schema,
    vocab   = handle$vocab_schema %||% handle$cdm_schema,
    results = handle$results_schema %||% handle$cdm_schema
  )
  for (schema_name in names(schema_params)) {
    schema_val <- schema_params[[schema_name]]
    prefix <- if (bare || is.null(schema_val) || !nzchar(schema_val)) {
      ""
    } else {
      paste0(schema_val, ".")
    }
    sql <- gsub(paste0("@", schema_name, "\\."), prefix, sql, fixed = FALSE)
  }
  sql
}

.omopAnalysisRunSql <- function(handle, entry, sanitized, scoped = NULL) {
  sql <- .qlRenderSchema(handle, entry$compute$sql)

  # Apply template defaults for params the caller omitted (same as .query_exec).
  effective <- sanitized
  for (p in (entry$params %||% list())) {
    if (!is.null(p$default) && is.null(effective[[p$name]])) {
      effective[[p$name]] <- .sanitizeQueryParam(p$default, p$name,
                                                 entry$meta$inputs_df)
    }
  }
  for (param_name in names(effective)) {
    sql <- gsub(paste0("@", param_name), effective[[param_name]], sql,
                fixed = TRUE)
  }

  # Realise the person-level @cohort hook (predicate when scoped, no-op when
  # not). Always called so a template's @cohort placeholder never leaks into the
  # executed SQL.
  sql <- .omopAnalysisInjectCohort(sql, scoped, entry$meta$scope_column)

  sql <- .sql_translate(sql, handle$target_dialect)
  .executeQuery(handle, sql)
}

#' THE single fail-closed run path for catalog analyses
#'
#' Resolves the entry, validates + sanitizes params, applies scope, runs the
#' compute step (SQL or R wrapper), and funnels the result through the ONE
#' unified gate (\code{\link{.omopAnalysisGate}}). This is the only place an
#' analysis result is produced for the client; the per-patient invariant is
#' enforced here, once, for every adapter.
#'
#' Scoping rules: \code{scope=} is resolved to a single re-gated cohort temp
#' table. SQL entries get a \code{person_id IN (SELECT subject_id FROM <scope>)}
#' predicate injected. Pre-computed Achilles/OHDSI entries hold no per-row person
#' key, so a non-NULL \code{scope} is REJECTED with a clear error.
#'
#' @param handle CDM handle.
#' @param name Character; entry name.
#' @param params Named list of parameter values.
#' @param scope Scope argument (see \code{\link{.omopAnalysisResolveScope}}).
#' @param combine Character; fold operator when scope has multiple sources.
#' @param assign Logical; TRUE for the assign run path (QueryLibrary mode=="assign"
#'   loaders) — returns the server-side assignment result un-gated (data stays on
#'   the server), FALSE (default) for the gated aggregate path.
#' @return For aggregate: a disclosure-controlled data frame. For assign: the
#'   server-side assignment result.
#' @keywords internal
.omopAnalysisRun <- function(handle, name, params = list(), scope = NULL,
                             combine = "union", assign = FALSE) {
  entry <- .omopAnalysisResolve(handle, name)

  # Mode admission: the aggregate path refuses assign-only entries and vice versa.
  if (assign && !identical(entry$mode, "assign")) {
    stop("Analysis '", name, "' is not an assign-mode loader.", call. = FALSE)
  }
  if (!assign && identical(entry$mode, "assign")) {
    stop("Analysis '", name, "' is assign-only; use the assign run path.",
         call. = FALSE)
  }

  sanitized <- .omopAnalysisSanitizeParams(entry, params)

  kind <- entry$compute$kind %||% "sql"

  # Scope resolution + applicability. Reject BEFORE resolving/running anything
  # so a non-scopable entry never emits broken SQL — the failure is a clear,
  # specific message, not a downstream SQL error.
  #
  # Single-source entries (the common case) fold every scope source into ONE
  # re-gated temp cohort (scoped). A TWO-POPULATION entry (overlap / SMD /
  # covariate-balance) instead resolves scope PER ELEMENT into TWO independently
  # re-gated temp cohorts (scoped_pair) so the two arms never bleed into one
  # another. Two-population is a kind="r" capability: such an entry declares
  # max_tables == 2 to mean "two distinct populations". SQL templates also carry
  # max_tables == 2, but for them it means "fold up to two sources into one
  # @cohort predicate" (a single population), so they always take the fold path.
  two_population <- !identical(kind, "sql") && !identical(kind, "sql+r") &&
    identical(as.integer(entry$scope$max_tables %||% 0L), 2L)
  scoped <- NULL
  scoped_pair <- NULL
  # Entries that require a cohort (the cohort IS the analysis population) fail
  # closed with a clear, specific error when run un-scoped, rather than letting
  # their compute$fn return a silently empty (gate-safe) frame a caller could
  # mistake for "no results in this population".
  if (is.null(scope) && isTRUE(entry$scope$requires_cohort)) {
    stop("Analysis '", name, "' requires a cohort/population scope (the cohort ",
         "is the analysis population); pass cohort= or tables= so it has a ",
         "population to compute over.", call. = FALSE)
  }
  if (!is.null(scope)) {
    if (!isTRUE(entry$scope$accepts_cohort) &&
        !isTRUE(entry$scope$accepts_tables)) {
      adapter <- entry$meta$adapter %||% ""
      reason <- if (identical(adapter, "achilles") ||
                    identical(adapter, "ohdsi")) {
        "it is a pre-computed result with no per-row person key"
      } else {
        "its SQL has no person-level @cohort hook"
      }
      stop("Analysis '", name, "' does not support cohort/population scoping (",
           reason, ").", call. = FALSE)
    }
    if (two_population) {
      scoped_pair <- .omopAnalysisResolveScopePair(handle, scope, combine)
    } else {
      scoped <- .omopAnalysisResolveScope(handle, scope, combine)
    }
  }

  if (identical(kind, "sql") || identical(kind, "sql+r")) {
    if (assign) {
      # Assign loader: render + execute server-side, no gate (data stays put).
      sql <- .qlRenderSchema(handle, entry$compute$sql)
      effective <- sanitized
      for (p in (entry$params %||% list())) {
        if (!is.null(p$default) && is.null(effective[[p$name]])) {
          effective[[p$name]] <- .sanitizeQueryParam(p$default, p$name,
                                                     entry$meta$inputs_df)
        }
      }
      for (param_name in names(effective)) {
        sql <- gsub(paste0("@", param_name), effective[[param_name]], sql,
                    fixed = TRUE)
      }
      sql <- .omopAnalysisInjectCohort(sql, scoped, entry$meta$scope_column)
      sql <- .sql_translate(sql, handle$target_dialect)
      return(.executeQuery(handle, sql))
    }
    df <- .omopAnalysisRunSql(handle, entry, sanitized, scoped)
  } else {
    # kind == "r": call the entry's compute$fn. ctx carries the adapter-private
    # identifiers the pre-computed wrappers need AND — the load-bearing addition
    # for natively re-implemented aggregate analyses — the resolved scope.
    #
    # compute$fn contract: function(handle, ctx, params) -> an AGGREGATE-ONLY
    # data.frame (counts/rates/summary stats), NEVER a person key. The fn MUST
    # honour the scope it is handed:
    #   * ctx$scoped_cohort  : a single re-gated cohort temp-table name, or NULL
    #     for a cohort-wide run. When non-NULL, restrict the population by an
    #     INNER JOIN on subject_id, e.g.
    #       INNER JOIN <ctx$scoped_cohort> coh ON t.person_id = coh.subject_id
    #     (the cohort keys its person on subject_id).
    #   * ctx$person_set_sql : the same scope as a ready-to-splice membership
    #     sub-select "(SELECT subject_id FROM <scoped>)", or NULL — for fns that
    #     prefer "... WHERE t.person_id IN <ctx$person_set_sql>".
    #   * ctx$scoped_cohorts : for two-population entries (max_tables == 2) a
    #     length-2 character vector of two INDEPENDENTLY re-gated cohort temp
    #     tables, or NULL. Each arm joins on subject_id exactly as above.
    # If the fn must pull rows into R it MUST call .assertMinPersons on the
    # scoped + filtered population BEFORE materialising. The returned frame is
    # STILL funnelled through .omopAnalysisGate below — the fn never gates.
    ctx <- list(
      analysis_id   = entry$meta$analysis_id,
      table_name    = entry$meta$table_name,
      tool_id       = entry$meta$tool_id,
      scoped_cohort = scoped,
      person_set_sql = if (!is.null(scoped)) {
        paste0("(SELECT subject_id FROM ", scoped, ")")
      } else NULL,
      scoped_cohorts = scoped_pair
    )
    df <- entry$compute$fn(handle, ctx, sanitized)
  }

  if (assign) {
    # An R-kind assign entry is not a thing today; return as-is for safety.
    return(df)
  }
  .omopAnalysisGate(handle, df, entry)
}
