# Differential-privacy locality for the normalised v2 graph. The frozen IR
# schema supplies the closed set of variants; no unrecognised node is given a
# person-local default. Runtime scope frames are authenticated separately from
# the semantic plan, and never enter its public semantic digest.

.dsomopDpIsV2 <- function(ir) {
  is.list(ir) && (identical(ir$version, 2L) || identical(ir$version, 2))
}

#' Resolve the authenticated input-scope contract for DP preparation
#' @keywords internal
.dsomopDpAuthenticatedScope <- function(ir, policy, scopes = NULL) {
  scope <- ir$scope
  if (is.null(scope)) return(list(local = TRUE, sources = NULL, ir = ir))
  if (.is_omop.table(scope) || is.data.frame(scope)) {
    frames <- scope
  } else if (is.list(scope)) {
    # Persistent cohort membership has no authenticated locality capsule. It
    # remains tainted even when authenticated tables are supplied as well.
    if (!is.null(scope$cohort)) return(list(local = FALSE))
    frames <- scopes %||% scope$tables_frames
    # Exact lookup: `$tables` partially matches the server-only tables_frames
    # when a legacy plan has no declared symbol list.
    requested <- scope[["tables"]] %||% character(0)
    if (length(requested) > 0L) {
      frame_list <- if (.is_omop.table(frames)) list(frames) else frames
      if (!is.list(frame_list) || is.data.frame(frame_list) ||
          length(frame_list) != length(requested)) return(list(local = FALSE))
      if (!is.null(names(frame_list)) &&
          !setequal(names(frame_list), unlist(requested, use.names = FALSE))) {
        return(list(local = FALSE))
      }
    }
    # tables_frames is server-only resolved state, not a change to the IR.
    ir$scope$tables_frames <- NULL
  } else {
    return(list(local = FALSE))
  }
  local <- tryCatch(.dsomopDpScopePersonLocal(frames, policy),
                    error = function(e) FALSE)
  if (!isTRUE(local)) return(list(local = FALSE))
  sources <- tryCatch(.dsomopDpScopeLineageSemantic(frames, policy),
                      error = function(e) FALSE)
  if (identical(sources, FALSE)) return(list(local = FALSE))
  list(local = TRUE, sources = sources, ir = ir)
}

#' Compute locality through every dependency of the normalised IR graph
#'
#' Match descendants and persistent-cohort inputs are non-local. Joins inherit
#' all input localities; source adapters, reducer windows and anchors are
#' followed through their typed schema references, including references nested
#' inside criteria and output options. Cycles and unresolved references fail
#' closed independently of the compiler's semantic validation.
#' @keywords internal
.dsomopDpIrLocality <- function(ir, policy, scopes = NULL) {
  output_names <- names(ir$outputs) %||% character(0)
  refused <- stats::setNames(rep(FALSE, length(output_names)), output_names)
  authenticated <- .dsomopDpAuthenticatedScope(ir, policy, scopes)
  if (!isTRUE(authenticated$local)) return(refused)
  ir <- authenticated$ir
  schema <- .jsonSchemaPackaged("ir-v2")
  valid <- tryCatch(.jsonSchemaValidate(schema, ir)$valid,
                    error = function(e) FALSE)
  if (!isTRUE(valid)) return(refused)
  memo <- new.env(parent = emptyenv())
  visiting <- new.env(parent = emptyenv())
  all_local <- function(x, fun) all(vapply(x, fun, logical(1L)))

  # Traverse against the matching closed schema variant. Looking for arbitrary
  # field names would confuse user-chosen names with source references.
  walk <- function(value, spec) {
    if (is.logical(spec)) return(isTRUE(spec))
    ref <- spec[["$ref"]]
    if (!is.null(ref)) {
      if (identical(ref, "#/$defs/event_ref")) {
        return(node("events", value$event))
      }
      if (identical(ref, "#/$defs/interval_ref")) {
        return(node("intervals", value$interval))
      }
      if (identical(ref, "#/$defs/criteria") && !is.null(value$cohort)) {
        return(FALSE)
      }
      if (identical(ref, "#/$defs/legacy_leaf") &&
          identical(value$type, "cohort")) return(FALSE)
      if (identical(ref, "#/$defs/index") &&
          !is.null(value$source$cohort_definition_id)) return(FALSE)
      if (!walk(value, .jsonSchemaResolveRef(ref, schema))) return(FALSE)
    }
    for (union in intersect(c("oneOf", "anyOf"), names(spec))) {
      alternatives <- Filter(function(branch) {
        .jsv_quiet(branch, value, schema, "")
      }, spec[[union]])
      if (length(alternatives) == 0L ||
          !all_local(alternatives, function(branch) walk(value, branch))) {
        return(FALSE)
      }
    }
    if (!all_local(spec$allOf %||% list(), function(branch) {
      walk(value, branch)
    })) return(FALSE)
    if (!is.null(spec[["if"]])) {
      branch <- if (.jsv_quiet(spec[["if"]], value, schema, "")) {
        spec[["then"]]
      } else spec[["else"]]
      if (!is.null(branch) && !walk(value, branch)) return(FALSE)
    }
    if (is.list(value) && !is.null(names(value))) {
      props <- spec$properties %||% list()
      for (name in intersect(names(value), names(props))) {
        if (!walk(value[[name]], props[[name]])) return(FALSE)
      }
      if (is.list(spec$additionalProperties)) {
        for (name in setdiff(names(value), names(props))) {
          if (!walk(value[[name]], spec$additionalProperties)) return(FALSE)
        }
      }
    } else if (!is.null(spec$items) &&
               identical(.jsonInstanceType(value), "array")) {
      if (!all_local(.jsonArrayItems(value), function(item) {
        walk(item, spec$items)
      })) return(FALSE)
    }
    TRUE
  }
  node <- function(group, name) {
    if (!.isScalarString(name) || is.null(ir[[group]][[name]])) return(FALSE)
    key <- paste(group, name, sep = "/")
    if (exists(key, memo, inherits = FALSE)) return(get(key, memo))
    if (exists(key, visiting, inherits = FALSE)) return(FALSE)
    assign(key, TRUE, visiting)
    on.exit(rm(list = key, envir = visiting), add = TRUE)
    value <- ir[[group]][[name]]
    local <- TRUE
    if (identical(group, "populations")) {
      local <- !identical(value$kind, "match") &&
        is.null(value$cohort_definition_id)
      if (!is.null(value$parent)) local <- local && node(group, value$parent)
      if (identical(value$kind, "setop")) {
        local <- local && all_local(value$setop$members, function(member) {
          node(group, member)
        })
      }
    } else if (identical(group, "variables")) {
      # These analytic keys describe a cross-person matched set. index_source
      # alone is a per-episode tag and does not create cross-person dependence.
      local <- is.null(value$population_column) ||
        identical(value$population_column$name, "index_source")
    } else if (identical(group, "outputs")) {
      local <- node("populations", value$rows)
      columns <- value$columns %||% list()
      local <- local && all_local(columns, function(column) {
        node("variables", column)
      })
      if (identical(value$format, "joined")) {
        local <- local && all_local(value$options$from, function(input) {
          node("outputs", input$output)
        })
      }
    }
    definition <- switch(group, events = "event", intervals = "interval",
                         populations = "population", variables = "variable",
                         outputs = "output")
    local <- local && walk(value, schema[["$defs"]][[definition]])
    assign(key, local, memo)
    local
  }
  stats::setNames(vapply(output_names, function(name) {
    node("outputs", name)
  }, logical(1L)), output_names)
}

#' Semantic provenance shared with the federation's normalised IR digest
#' @keywords internal
.dsomopDpIrLineageSemantic <- function(ir, output_name, policy,
                                       component = NULL, scopes = NULL) {
  authenticated <- .dsomopDpAuthenticatedScope(ir, policy, scopes)
  if (!isTRUE(authenticated$local)) .dsomopDpNotPersonLocal(output_name)
  list(kind = "plan_output", contract_version = 3L,
       semantic_digest = .planSemanticDigest(authenticated$ir)$sha256,
       output = output_name, component = component,
       scope = authenticated$sources,
       population = ir$outputs[[output_name]]$rows)
}

.dsomopDpNotPersonLocal <- function(output, hint = "") {
  stop(structure(list(
    message = paste0("Plan output '", output,
                     "' cannot be prepared for DP releases: not_person_local.",
                     hint),
    call = NULL, code = "not_person_local", output = output
  ), class = c("dsomop_dp_rejection", "error", "condition")))
}

#' Construct a private sealing step for the publication transaction
#'
#' Called only by the DP executor after scope resolution. The public plan
#' executor never takes a client-supplied seal function. A patient component
#' must be sealed and verified before the publisher assigns any component.
#' @keywords internal
.dsomopDpPublicationSeal <- function(ctx) {
  ir <- ctx$ir
  policy <- .dsomopDpPolicy()
  dataset_identity <- .dsomopDpDatasetIdentity(ctx$handle)
  locality <- if (.dsomopDpIsV2(ir)) {
    .dsomopDpIrLocality(ir, policy, ctx$scopes)
  } else {
    stats::setNames(vapply(names(ir$outputs), function(name) {
      .dsomopDpPlanOutputPersonLocal(ir, name, policy)
    }, logical(1L)), names(ir$outputs))
  }
  function(value, candidate) {
    if (identical(candidate$lineage$mode, "reference")) return(value)
    output <- candidate$output
    if (!isTRUE(locality[[output]])) {
      hint <- if (!.dsomopDpIsV2(ir) &&
          "age_at_index" %in% tolower(ir$outputs[[output]]$derived %||%
                                     character(0))) {
        " Use an age at a public reference year instead."
      } else ""
      .dsomopDpNotPersonLocal(output, hint)
    }
    sealed <- .dsomopDpSealPlanOutput(
      value, ir, output, dataset_identity = dataset_identity,
      component = candidate$component, scopes = ctx$scopes,
      person_key = .personKey(ctx$handle),
      read_snapshot = .dsomopReadSnapshotLabel(ctx$handle)
    )
    # A sealing failure is technical, not an invitation to publish unsealed
    # rows. The publication transaction owns and aborts all artefacts.
    .dsomopDpVerifyPersonLocal(sealed, policy)
    sealed
  }
}
