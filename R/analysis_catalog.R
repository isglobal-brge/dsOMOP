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
#' @keywords internal
.omopAnalysisScope <- function(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 2L) {
  list(accepts_cohort = isTRUE(accepts_cohort),
       accepts_tables  = isTRUE(accepts_tables),
       max_tables      = as.integer(max_tables))
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

# --- Adapter 2: Achilles -----------------------------------------------------

#' Emit catalog entries for the pre-computed Achilles analyses
#'
#' One entry per analysis_id in \code{\link{.achilles_catalog_static}}, named
#' \code{"dsomop:achilles.<id>"}. The \code{unit} comes from the catalog's
#' \code{unit} column (person/record/dist). Record-unit entries carry their
#' \code{.achilles_record_gate_spec} (sibling/companion) in \code{meta} so the
#' unified gate can apply the SAME person-gate \code{\link{.achillesPersonGate}}
#' enforces. Compute is \code{kind="r"}, wrapping
#' \code{\link{.achillesGetResults}} (count/person units) or
#' \code{\link{.achillesGetDistributions}} (dist unit) for the single id.
#'
#' These are PRE-COMPUTED aggregates with no per-row person key, so the entry's
#' scope rejects \code{accepts_tables}/cohort scoping (handled in the run path).
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return List of \code{omop_analysis_entry} objects keyed by entry name.
#' @keywords internal
.omopAnalysisAchillesEntries <- function(handle) {
  catalog <- .achilles_catalog_static()
  gate_spec_all <- .achilles_record_gate_spec()

  entries <- list()
  for (i in seq_len(nrow(catalog))) {
    aid  <- as.integer(catalog$analysis_id[i])
    unit <- catalog$unit[i]
    name <- paste0("dsomop:achilles.", aid)
    is_dist <- identical(unit, "dist")
    gate_spec <- gate_spec_all[[as.character(aid)]]

    compute_fn <- if (is_dist) {
      function(handle, ctx, params) .achillesGetDistributions(handle, ctx$analysis_id)
    } else {
      function(handle, ctx, params) .achillesGetResults(handle, ctx$analysis_id)
    }

    entries[[name]] <- .omopAnalysisEntry(
      name        = name,
      description = catalog$analysis_name[i],
      domain      = tolower(catalog$domain[i] %||% "general"),
      params      = list(),
      compute     = list(kind = "r", sql = NULL, fn = compute_fn),
      dependencies = list(
        tables   = catalog$result_table[i],
        packages = character(0)
      ),
      disclosure = .omopAnalysisDisclosure(
        unit = unit,
        count_cols = "count_value",
        min_max = FALSE
      ),
      scope = .omopAnalysisScope(accepts_cohort = FALSE, accepts_tables = FALSE,
                                 max_tables = 0L),
      mode  = "aggregate",
      meta  = list(adapter = "achilles", analysis_id = aid,
                   gate_spec = gate_spec, precomputed = TRUE)
    )
  }
  entries
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
  entries
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

#' Build (once) and cache the unified analysis catalog
#'
#' Assembles the catalog from all three adapters, keyed by stable entry name
#' (\code{"dsomop:<id>"}). Built once per handle and cached on
#' \code{handle$analysis_catalog} — exactly the build-once + cache pattern used
#' by \code{handle$blueprint} (\code{\link{.buildBlueprint}}) and
#' \code{handle$achilles_gate_cache}. Pass \code{force=TRUE} to rebuild.
#'
#' @param handle CDM handle.
#' @param force Logical; rebuild even if cached.
#' @return Named list of \code{omop_analysis_entry} objects keyed by name.
#' @keywords internal
.omopAnalysisRegistry <- function(handle, force = FALSE) {
  if (!is.null(handle$analysis_catalog) && !force) {
    return(handle$analysis_catalog)
  }

  entries <- c(
    .omopAnalysisQueryEntries(handle),
    .omopAnalysisAchillesEntries(handle),
    .omopAnalysisOhdsiEntries(handle)
  )

  .omopAnalysisWarnDeps(handle, entries)

  handle$analysis_catalog <- entries
  entries
}

#' Resolve a catalog entry by name (fail-closed)
#'
#' @param handle CDM handle.
#' @param name Character; the entry name (pack-prefixed stable id).
#' @return The matching \code{omop_analysis_entry}; stops if not found.
#' @keywords internal
.omopAnalysisResolve <- function(handle, name) {
  name <- trimws(as.character(name)[[1]])
  registry <- .omopAnalysisRegistry(handle)
  entry <- registry[[name]]
  if (is.null(entry)) {
    stop("Analysis '", name, "' not found in the analysis catalog.",
         call. = FALSE)
  }
  entry
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
    accepts_tables = logical(0), stringsAsFactors = FALSE
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
    adapter     = e$meta$adapter %||% "unknown"
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
  sanitized <- list()
  for (pname in names(params)) {
    val <- params[[pname]]
    if (is.null(val)) next
    # Length guard, same as the query path.
    .validateString(as.character(val)[1])
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
    # nfilter_dist, and never release min/max.
    if (length(count_cols) > 0) {
      df <- .suppressSmallCounts(df, count_cols)
    }
    df <- df[, setdiff(names(df),
                       grep("^min_value$|^max_value$", names(df),
                            ignore.case = TRUE, value = TRUE)), drop = FALSE]
    nfilter_dist <- settings$nfilter_dist %||% 10L
    mask_cols <- intersect(
      c("avg_value", "stdev_value", "p10_value", "p25_value", "median_value",
        "p75_value", "p90_value"),
      names(df))
    if (length(mask_cols) > 0 && "count_value" %in% names(df) && nrow(df) > 0) {
      small <- !is.na(df$count_value) & df$count_value < nfilter_dist
      df[small, mask_cols] <- NA_real_
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
  band_cols <- intersect(band_cols, names(df))
  if (length(band_cols) > 0) {
    df <- .suppressSmallCounts(df, band_cols)
    for (col in band_cols) {
      df[[col]] <- vapply(df[[col]], .bandCount, numeric(1),
                          band_width = settings$nfilter_band)
    }
  }
  rownames(df) <- NULL
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

  # Scope resolution + applicability. Reject BEFORE resolving/running anything
  # so a non-scopable entry never emits broken SQL — the failure is a clear,
  # specific message, not a downstream SQL error.
  scoped <- NULL
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
    scoped <- .omopAnalysisResolveScope(handle, scope, combine)
  }

  kind <- entry$compute$kind %||% "sql"

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
    # kind == "r": call the wrapped Achilles/OHDSI accessor. ctx carries the
    # adapter-private identifiers the wrapper needs.
    ctx <- list(
      analysis_id = entry$meta$analysis_id,
      table_name  = entry$meta$table_name,
      tool_id     = entry$meta$tool_id
    )
    df <- entry$compute$fn(handle, ctx, sanitized)
  }

  if (assign) {
    # An R-kind assign entry is not a thing today; return as-is for safety.
    return(df)
  }
  .omopAnalysisGate(handle, df, entry)
}
