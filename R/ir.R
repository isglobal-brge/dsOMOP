# Module: Plan IR (design-2.10.3 sections 4.5, 5 and 6.1)
#
# Every plan -- a language 2.0 wire plan or a v1 plan from an older client --
# reaches the compilers through ONE frozen sequence (section 4.5):
#
#   (1) the incoming version's closed envelope and variants are validated
#       before any normalisation: plan-v2.json for `version: 2`, the corrected
#       v1 validators (R/ir_v1.R) for a v1 plan (no `version`, or 1);
#   (2) .irNormalize() lowers a v1 plan (section 5; its result-affecting
#       legacy semantics become the closed IR-only variants of amendment
#       A-10) and expands every default through the one frozen normaliser
#       shared with the client, .planNormalizeV2() (amendment A-11); it never
#       discards an unknown field, so invalid input cannot become valid;
#   (3) the IR is validated against ir-v2.json (wire schema plus the closed
#       compatibility variants only the v1 lowering produces);
#   (4) .irValidate() applies the semantic rules of section 4.1, then the
#       handle-dependent checks (role registry, filterable and typed columns,
#       column classes, deployment-disabled operations, dialect preflight).
#
# Failures are typed plan rejections: conditions of classes
# dsomop_ir_validation_error, dsomop_plan_rejection, error, condition with
# fields code ("validation_error", "unavailable_table" or "dialect_unsupported"
# from R/sql_expr.R), output (NULL for a plan-level refusal) and field. Their
# messages name fields, outputs and constructs, never data values or counts.

.IR_LANGUAGE_VERSION <- "2.0"
.IR_SEMANTIC_VERSION <- "2.0.0"
.IR_SCHEMA_ID <- "https://isglobal-brge.github.io/dsOMOP/schema/plan-v2.json"

.irCache <- new.env(parent = emptyenv())

#' Packaged schema, parsed and linted once per session
#' @param name "plan-v2", "ir-v2" or "mutate-v1".
#' @keywords internal
.irSchema <- function(name = "ir-v2") {
  key <- paste0("schema_", name)
  cached <- .irCache[[key]]
  if (is.null(cached)) {
    cached <- .jsonSchemaPackaged(name)
    assign(key, cached, envir = .irCache)
  }
  cached
}

# --- Typed rejections ---------------------------------------------------------

#' Signal a typed plan rejection
#'
#' @param message Human-readable reason (names fields/outputs/constructs only).
#' @param code Rejection code (section 7.3): "validation_error" (default) or
#'   "unavailable_table".
#' @param output Optional output (publication root) the refusal belongs to.
#' @param field Optional JSON-pointer-like path of the offending field.
#' @param errors Optional character vector of individual findings.
#' @keywords internal
.irReject <- function(message, code = "validation_error", output = NULL,
                      field = NULL, errors = NULL) {
  stop(structure(
    list(message = paste0(code, ": ", message), call = NULL, code = code,
         output = output, field = field, errors = errors),
    class = c("dsomop_ir_validation_error", "dsomop_plan_rejection",
              "error", "condition")
  ))
}

# Refuse with every finding, keeping the message bounded.
.irRejectAll <- function(what, errors, code = "validation_error",
                         output = NULL) {
  errors <- unique(as.character(errors))
  shown <- utils::head(errors, 8L)
  .irReject(paste0(what, ": ", paste(shown, collapse = "; "),
                   if (length(errors) > length(shown)) "; ..." else ""),
            code = code, output = output, errors = errors)
}

# --- JSON form ----------------------------------------------------------------

#' Canonical JSON form of a plan received as an R object
#'
#' Plans arrive either JSON-parsed (\code{.ds_arg()}, simplifyVector = FALSE)
#' or as R lists built in-process. Both are mapped to one form, the one the
#' DataSHIELD transport produces: named atomic vectors become objects (as the
#' client's \code{.ds_encode()} does), length-one vectors scalars, NA and NULL
#' null. Server-resolved workspace frames (\code{scope$tables_frames}) and any
#' other non-JSON value are replaced by an explicit marker object, which no
#' schema accepts, so they can never pass validation by accident.
#'
#' @param plan Plan object.
#' @return The plan in parsed JSON form.
#' @keywords internal
.irJsonForm <- function(plan) {
  coerce <- function(x) {
    if (is.data.frame(x)) {
      return(list(`__resolved_frame__` = list(ncol = ncol(x))))
    }
    if (is.environment(x) || is.function(x) || inherits(x, "R6")) {
      return(list(`__unsupported__` = class(x)[[1L]]))
    }
    if (is.list(x)) {
      out <- lapply(x, coerce)
      names(out) <- names(x)
      return(out)
    }
    if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
    if (is.factor(x)) x <- as.character(x)
    if (is.atomic(x) && !is.null(names(x))) {
      nm <- names(x)
      attributes(x) <- NULL
      out <- as.list(x)
      names(out) <- nm
      return(out)
    }
    if (is.atomic(x)) attributes(x) <- NULL
    x
  }
  json <- jsonlite::toJSON(coerce(plan), auto_unbox = TRUE, null = "null",
                           na = "null", digits = NA)
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

#' Version of an incoming plan: 1 (no version, or 1) or 2
#' @keywords internal
.irPlanVersion <- function(plan) {
  if (!is.list(plan) || (length(plan) > 0L && is.null(names(plan)))) {
    .irReject("a plan must be a JSON object", field = "/")
  }
  version <- plan[["version"]]
  if (is.null(version)) return(1L)
  if (is.numeric(version) && length(version) == 1L && !is.na(version) &&
      version %in% c(1, 2)) {
    return(as.integer(version))
  }
  .irReject("plan version must be 1 or 2", field = "/version")
}

# --- The four steps -----------------------------------------------------------

#' Step 1 for a language 2.0 plan: the closed wire schema
#' @keywords internal
.irValidateWire <- function(plan) {
  res <- .jsonSchemaValidate(.irSchema("plan-v2"), plan)
  if (!res[["valid"]]) .irRejectAll("plan does not validate against plan-v2.json",
                               res[["errors"]])
  invisible(TRUE)
}

#' Step 1: the closed envelope and variants of the plan's own version
#'
#' plan-v2.json for a language 2.0 plan; for a v1 plan the closed v1 envelope
#' with the corrected legacy value validators (\code{.planV1Envelope()}).
#' @param plan Plan in parsed JSON form.
#' @keywords internal
.irValidateEnvelope <- function(plan) {
  if (identical(.irPlanVersion(plan), 2L)) .irValidateWire(plan) else .planV1Envelope(plan)
  invisible(TRUE)
}

#' Step 3: the IR schema (wire schema plus closed compatibility variants)
#' @keywords internal
.irValidateSchema <- function(ir) {
  res <- .jsonSchemaValidate(.irSchema("ir-v2"), unclass(.irStripPrivate(ir)))
  if (!res[["valid"]]) .irRejectAll("normalised plan does not validate against ir-v2.json",
                               res[["errors"]])
  invisible(TRUE)
}

# The server-private attributes of an IR (never part of the IR document).
.irStripPrivate <- function(ir) {
  attributes(ir) <- attributes(ir)[intersect(names(attributes(ir)), "names")]
  ir
}

#' The IR document: the plain list the IR schema, explain and the shared
#' semantic digest (\code{.planSemanticDigest()}, design 4.4) see
#' @param ir The IR (\code{dsomop_ir}).
#' @keywords internal
.irDocument <- function(ir) unclass(.irStripPrivate(ir))

#' Normalise a plan to the IR (step 2)
#'
#' Every default is expanded by the one frozen normaliser shared with the
#' client, \code{.planNormalizeV2()} (\code{R/plan_normalize_v2.R}, amendment
#' A-11). Language 2.0: \code{.planNormalizeV2(plan)} against plan-v2.json. A
#' v1 plan: lowered to the compatibility IR (section 5, \code{.irLowerV1()}),
#' whose result-affecting legacy semantics are closed IR variants of the
#' document itself (amendment A-10), then normalised by the same function
#' against ir-v2.json -- except \code{publish}: a v1 plan never names
#' publication roots, its roots are the outputs the execution maps to symbols
#' (\code{out}; unmapped outputs stay private computed inputs, as in 2.10.2),
#' so the lowered IR carries no \code{publish} and consumers use
#' \code{ir$publish \%||\% names(out)}. Pure: no database access. The
#' normalised plan is what \code{omopPlanExplainDS} returns.
#'
#' @param plan Plan in parsed JSON form (see \code{.irJsonForm()}).
#' @param scope_tables Number of workspace scope tables the server resolved
#'   for a v1 execution (endpoint arguments; never carried by the plan).
#' @param validated TRUE when step 1 (the closed envelope) already ran.
#' @return The IR, class \code{dsomop_ir}, with attribute \code{source_version}
#'   (1 or 2) and, for a lowered v1 plan with advisory fields,
#'   \code{legacy_annex} (server-private, listed by explain, covered by no
#'   digest, never read by an execution decision; see \code{.irLowerV1()}).
#' @keywords internal
.irNormalize <- function(plan, scope_tables = 0L, validated = FALSE) {
  version <- .irPlanVersion(plan)
  annex <- NULL
  if (identical(version, 2L)) {
    ir <- .planNormalizeV2(plan)
  } else {
    lowered <- .irLowerV1(plan, scope_tables = scope_tables, validated = validated)
    annex <- attr(lowered, "legacy_annex")
    # a plan that failed the envelope is kept intact (never repaired, never
    # normalised: the frozen normaliser expects a valid plan) and stays
    # invalid for every schema
    ir <- if (!is.null(lowered[["v1_envelope_errors"]])) {
      .irStripPrivate(lowered)
    } else {
      .planNormalizeV2(.irStripPrivate(lowered), .irSchema("ir-v2"))
    }
    ir["publish"] <- NULL
  }
  structure(ir, class = "dsomop_ir", source_version = version,
            legacy_annex = annex)
}

#' Run the frozen validation sequence (steps 1 to 4)
#'
#' Validates the plan as it arrived on the wire: server-resolved workspace
#' frames are never part of it (they travel in the execution context; a plan
#' carrying \code{scope.tables_frames} is refused by the closed envelope).
#'
#' @param plan Incoming plan (any R or JSON-parsed form).
#' @param handle CDM handle for the handle-dependent checks, or NULL to stop
#'   after the semantic rules (client-side mirrors, explain without a site).
#' @param scope_tables Number of workspace scope tables the server resolved
#'   for this execution (v1; see \code{.irNormalize()}).
#' @return The validated IR (\code{dsomop_ir}); NULL for a v1 plan without
#'   outputs, which has no IR (amendment A-10: it runs privately on the v1
#'   executor and publishes nothing).
#' @keywords internal
.irPrepare <- function(plan, handle = NULL, scope_tables = 0L) {
  plan <- .irJsonForm(plan)
  version <- .irPlanVersion(plan)
  .irValidateEnvelope(plan)
  if (!identical(version, 2L)) {
    outputs <- plan[["outputs"]]
    if (is.null(outputs) || (is.list(outputs) && length(outputs) == 0L)) {
      if (!is.null(handle)) .planV1SiteCheck(plan, handle)
      return(invisible(NULL))
    }
  }
  ir <- .irNormalize(plan, scope_tables = scope_tables, validated = TRUE)
  .irValidateSchema(ir)
  .irValidate(ir, handle, v1_plan = if (!identical(version, 2L)) plan)
  ir
}

#' The wire part of a server-side execution plan
#'
#' Removes the workspace frames the server injected under
#' \code{scope$tables_frames} (and the scope object when nothing else is in
#' it), so validation sees exactly what a client can send.
#' @param plan Execution plan (R list).
#' @return list(plan = the wire part, scope_tables = number of frames).
#' @keywords internal
.planWirePart <- function(plan) {
  scope <- if (is.list(plan)) plan[["scope"]] else NULL
  if (!is.list(scope) || is.data.frame(scope) || is.null(scope[["tables_frames"]])) {
    return(list(plan = plan, scope_tables = 0L))
  }
  frames <- scope[["tables_frames"]]
  n <- if (is.data.frame(frames)) 1L else length(frames)
  scope[["tables_frames"]] <- NULL
  if (length(scope)) plan[["scope"]] <- scope else plan[["scope"]] <- NULL
  list(plan = plan, scope_tables = n)
}

# --- References and nodes -----------------------------------------------------

.IR_NODE_MAPS <- c(concept_set = "concept_sets", event = "events",
                   interval = "intervals", population = "populations",
                   variable = "variables", output = "outputs")

#' Resolve one IR node by kind and name, or a typed source reference
#'
#' @param ir The IR.
#' @param kind One of concept_set, event, interval, population, variable,
#'   output -- or a typed reference list such as \code{list(event = "mi")}.
#' @param name Node name (when \code{kind} is a kind).
#' @return The node; a dangling reference is a typed validation error.
#' @keywords internal
.irNode <- function(ir, kind, name = NULL) {
  if (is.list(kind)) {
    ref <- kind
    kind <- names(ref)[[1L]]
    name <- ref[[1L]]
  }
  map <- .IR_NODE_MAPS[[kind]]
  if (is.null(map)) .irReject(paste0("unknown node kind '", kind, "'"))
  node <- if (is.character(name) && length(name) == 1L) ir[[map]][[name]]
  if (is.null(node)) {
    .irReject(paste0("dangling ", kind, " reference '", name %||% "", "'"),
              field = paste0("/", map, "/", name %||% ""))
  }
  node
}

# Every typed source reference ({event: e} / {interval: i}) inside a value,
# with its JSON path.
.irCollectRefs <- function(x, path = "") {
  out <- list()
  if (!is.list(x)) return(out)
  nm <- names(x)
  if (!is.null(nm) && length(x) == 1L && nm %in% c("event", "interval") &&
      is.character(x[[1L]]) && length(x[[1L]]) == 1L) {
    return(list(list(kind = nm, name = x[[1L]], path = path)))
  }
  for (i in seq_along(x)) {
    key <- if (is.null(nm)) as.character(i - 1L) else nm[[i]]
    out <- c(out, .irCollectRefs(x[[i]], paste0(path, "/", key)))
  }
  out
}

# Byte-order (C locale) sort of a character vector; NULL gives character(0).
.irSortNames <- function(x) {
  x <- unique(as.character(unlist(x, use.names = FALSE)))
  if (!length(x)) return(character(0))
  sort(x, method = "radix")
}

# Topological order of `names` given deps(name) -> character. Every node comes
# after all of its dependencies; among the nodes that are ready, the lowest
# priority(name) comes first, then byte order of the name (a tie-breaker
# only: it never moves a node before a dependency or after a consumer).
# Returns list(order, cycle = names on a cycle or character(0)).
.irTopoSort <- function(names, deps, priority = function(n) 0L) {
  names <- .irSortNames(names)
  dep_of <- lapply(stats::setNames(names, names), function(n) {
    .irSortNames(intersect(as.character(deps(n)), names))
  })
  state <- stats::setNames(rep(0L, length(names)), names)
  cycle <- character(0)
  visit <- function(n, stack) {
    if (length(cycle)) return(invisible(NULL))
    if (identical(state[[n]], 2L)) return(invisible(NULL))
    if (identical(state[[n]], 1L)) {
      cycle <<- c(stack[seq.int(match(n, stack), length(stack))], n)
      return(invisible(NULL))
    }
    state[[n]] <<- 1L
    for (d in dep_of[[n]]) visit(d, c(stack, n))
    state[[n]] <<- 2L
    invisible(NULL)
  }
  for (n in names) visit(n, character(0))
  if (length(cycle)) return(list(order = character(0), cycle = cycle))
  order <- character(0)
  pending <- names
  while (length(pending)) {
    ready <- pending[vapply(pending, function(n) all(dep_of[[n]] %in% order), logical(1))]
    rank <- vapply(ready, function(n) as.integer(priority(n)), integer(1))
    pick <- ready[order(rank, ready, method = "radix")][[1L]]
    order <- c(order, pick)
    pending <- setdiff(pending, pick)
  }
  list(order = order, cycle = character(0))
}

.irEventDeps <- function(ir, e) {
  node <- ir[["events"]][[e]]
  if (is.list(node[["episode_members"]])) return(node[["episode_members"]][["episode"]][["event"]])
  if (is.list(node[["cost"]])) return(node[["cost"]][["event"]][["event"]])
  character(0)
}

.irPopulationDeps <- function(ir, p) {
  node <- ir[["populations"]][[p]]
  c(if (is.character(node[["parent"]])) node[["parent"]],
    unlist(node[["setop"]][["members"]] %||% list(), use.names = FALSE),
    if (is.list(node[["match"]])) c(node[["match"]][["cases"]], node[["match"]][["controls"]]))
}

.irOutputDeps <- function(ir, o) {
  node <- ir[["outputs"]][[o]]
  opts <- node[["options"]] %||% list()
  c(vapply(opts[["from"]] %||% list(), function(f) f[["output"]] %||% "", ""),
    unlist(opts[["source_outputs"]] %||% list(), use.names = FALSE))
}

#' Variables an output projects
#'
#' Its \code{columns}; when \code{columns} is null, every variable for
#' \code{wide} and the other variable-driven formats (\code{sparse},
#' \code{temporal_covariates}, \code{person_period}: the conservative reading
#' until their compilers fix it), and none for formats whose columns come from
#' their sources, roster or inputs (\code{survival}, \code{long},
#' \code{intervals}, \code{membership}, \code{dictionary}, \code{joined}) --
#' as the WS-0 acceptance expectations show for \code{wide} and
#' \code{survival}.
#'
#' @param ir The IR.
#' @param o Output name.
#' @return Character vector of variable names (in \code{columns} order, or in
#'   plan order).
#' @keywords internal
.irOutputVariables <- function(ir, o) {
  node <- ir[["outputs"]][[o]]
  vars <- names(ir[["variables"]] %||% list())
  cols <- node[["columns"]]
  if (!is.null(cols)) {
    return(intersect(as.character(unlist(cols, use.names = FALSE)), vars))
  }
  if (isTRUE(node[["format"]] %in% c("survival", "long", "intervals", "membership",
                                      "dictionary", "joined"))) {
    return(character(0))
  }
  vars
}

# Names reachable from `start` through deps(name), restricted to `pool`.
.irReach <- function(start, deps, pool) {
  seen <- character(0)
  queue <- intersect(as.character(start), pool)
  while (length(queue)) {
    n <- queue[[1L]]
    queue <- queue[-1L]
    if (n %in% seen) next
    seen <- c(seen, n)
    queue <- c(queue, setdiff(intersect(as.character(deps(n)), pool), seen))
  }
  seen
}

#' The part of an IR one output depends on
#'
#' The output, the outputs it joins or describes, its \code{rows} population
#' with parents, set-op members and match arms, the variables it projects
#' (\code{.irOutputVariables()}), and every event, interval and concept set
#' these reference (transitively).
#'
#' @param ir The IR (references resolved).
#' @param o Output name.
#' @return The IR restricted to those nodes, publishing the kept outputs.
#' @keywords internal
.irOutputClosure <- function(ir, o) {
  outs <- ir[["outputs"]] %||% list()
  pops <- ir[["populations"]] %||% list()
  vars <- ir[["variables"]] %||% list()
  ev <- ir[["events"]] %||% list()
  iv <- ir[["intervals"]] %||% list()
  keep_o <- .irReach(o, function(n) .irOutputDeps(ir, n), names(outs))
  rows <- vapply(outs[keep_o], function(x) x[["rows"]] %||% "", "")
  keep_p <- .irReach(rows, function(n) .irPopulationDeps(ir, n), names(pops))
  keep_v <- unique(unlist(lapply(keep_o, function(n) .irOutputVariables(ir, n)),
                          use.names = FALSE))
  keep_v <- intersect(names(vars), as.character(keep_v))
  keep_e <- character(0)
  keep_i <- character(0)
  pending <- c(pops[keep_p], vars[keep_v], outs[keep_o])
  while (length(pending)) {
    refs <- .irCollectRefs(unname(pending))
    kinds <- vapply(refs, function(r) r[["kind"]], "")
    targets <- vapply(refs, function(r) r[["name"]], "")
    new_e <- setdiff(intersect(targets[kinds == "event"], names(ev)), keep_e)
    new_e <- setdiff(.irReach(new_e, function(n) .irEventDeps(ir, n), names(ev)), keep_e)
    new_i <- setdiff(intersect(targets[kinds == "interval"], names(iv)), keep_i)
    keep_e <- c(keep_e, new_e)
    keep_i <- c(keep_i, new_i)
    pending <- c(ev[new_e], iv[new_i])
  }
  # an event's own concept set and the v1 output-level concept set that also
  # restricts it (A-10 legacy_concept_scope)
  cs_refs <- unlist(lapply(ev[keep_e], function(e) {
    c(if (is.character(e[["concept_set"]])) e[["concept_set"]],
      e[["legacy_concept_scope"]][["concept_set_ref"]])
  }), use.names = FALSE)
  pick <- function(map, keep) {
    map <- map %||% list()
    out <- map[names(map) %in% keep]
    if (!length(out)) .pnv2EmptyObject() else out
  }
  sub <- ir
  sub[["concept_sets"]] <- pick(ir[["concept_sets"]], cs_refs)
  sub[["events"]] <- pick(ev, keep_e)
  sub[["intervals"]] <- pick(iv, keep_i)
  sub[["populations"]] <- pick(pops, keep_p)
  sub[["variables"]] <- pick(vars, keep_v)
  sub[["outputs"]] <- pick(outs, keep_o)
  sub[["publish"]] <- as.list(intersect(names(outs), keep_o))
  sub
}

#' Evaluation order of an IR (design section 6.1)
#'
#' Concept sets, events (including adapters) and intervals in reference
#' order; populations topologically over parent, set-op members and match
#' cases/controls (never "all indexed populations first"); variables; outputs
#' topologically over joined inputs and dictionary sources, a \code{joined}
#' output coming as late as its consumers allow (a tie-breaker among ready
#' outputs, never a reordering across a dependency). Each
#' population also lists its \strong{candidate} anchors (those of its own
#' criteria, resolved against its candidate roster) and its \strong{final}
#' anchors (those of the variables and outputs over it, resolved against the
#' final roster); one anchor definition used in both stages appears in both.
#' Every value of a long output's named \code{options.anchors} map is a final
#' anchor of the output's population, nested search anchors included; a
#' matching controls template's candidate anchors include those of the match
#' population's strata windows; the same strata anchors are final anchors of
#' its cases population (design 6.10; amendment A-20(b)). Anchors are
#' collected from the normalised IR, so omitted and explicit defaults give one
#' canonical anchor.
#'
#' @param ir The IR (validated).
#' @return list(concept_sets, events, intervals, populations, variables,
#'   outputs, anchors = list(candidate = list(<pop> = list(<anchor>)), final =
#'   ...)).
#' @keywords internal
.irOrder <- function(ir) {
  ev <- .irTopoSort(names(ir[["events"]]), function(e) .irEventDeps(ir, e))
  po <- .irTopoSort(names(ir[["populations"]]), function(p) .irPopulationDeps(ir, p))
  joined <- function(o) if (identical(ir[["outputs"]][[o]][["format"]], "joined")) 1L else 0L
  ou <- .irTopoSort(names(ir[["outputs"]]), function(o) .irOutputDeps(ir, o),
                    priority = joined)
  if (length(ev[["cycle"]]) || length(po[["cycle"]]) || length(ou[["cycle"]])) {
    .irReject(paste0("reference cycle: ",
                     paste(c(ev[["cycle"]], po[["cycle"]], ou[["cycle"]]), collapse = " -> ")))
  }
  list(
    concept_sets = .irSortNames(names(ir[["concept_sets"]])),
    events = ev[["order"]],
    intervals = .irSortNames(names(ir[["intervals"]])),
    populations = po[["order"]],
    variables = .irSortNames(names(ir[["variables"]])),
    outputs = ou[["order"]],
    anchors = .irAnchorInventory(ir)
  )
}

# Distinct source anchors (objects with a `source`) inside a value: the
# anchor positions of the IR (a window's or bound's `anchor`, a reducer's or
# age leaf's `reference`) and every value of a named anchor map (a long
# output's `options.anchors`, amendment A-20(b)); nested search windows are
# visited by the recursion.
.irCollectAnchors <- function(x) {
  out <- list()
  if (!is.list(x)) return(out)
  nm <- names(x)
  if (!is.null(nm)) {
    for (key in intersect(nm, c("anchor", "reference", "from", "to"))) {
      a <- x[[key]]
      if (is.list(a) && !is.null(a[["source"]])) out <- c(out, list(a))
    }
    # Named output anchors (A-20(b)), including nested search anchors.
    if ("anchors" %in% nm && is.list(x[["anchors"]])) {
      for (a in x[["anchors"]]) {
        if (is.list(a) && !is.null(a[["source"]])) out <- c(out, list(a))
      }
    }
  }
  for (v in x) out <- c(out, .irCollectAnchors(v))
  out
}

.irUniqueAnchors <- function(anchors) {
  if (!length(anchors)) return(list())
  keys <- vapply(anchors, .jcsCanonical, "")
  anchors[!duplicated(keys)][order(keys[!duplicated(keys)], method = "radix")]
}

.irAnchorInventory <- function(ir) {
  candidate <- list()
  final <- list()
  pops <- ir[["populations"]] %||% list()
  # matching (design 6.10): the controls template is instantiated at each case
  # time, so the anchors of a match population's strata windows are resolved
  # on the template's candidate roster, with the template's own criteria,
  # and independently on the cases' final roster. Template-only criteria do
  # not become case-side anchors.
  templates <- list()
  cases <- list()
  for (p in names(pops)) {
    m <- pops[[p]][["match"]]
    controls <- m[["controls"]]
    if (is.list(m) && is.character(controls) && length(controls) == 1L) {
      strata <- .irCollectAnchors(m[["match"]])
      templates[[controls]] <- c(templates[[controls]], strata)
      case_pop <- m[["cases"]]
      if (is.character(case_pop) && length(case_pop) == 1L) {
        cases[[case_pop]] <- c(cases[[case_pop]], strata)
      }
    }
  }
  for (p in names(pops)) {
    candidate[[p]] <- .irUniqueAnchors(c(.irCollectAnchors(pops[[p]][["where"]]), templates[[p]]))
    used <- cases[[p]] %||% list()
    for (o in names(ir[["outputs"]])) {
      out <- ir[["outputs"]][[o]]
      if (!identical(out[["rows"]], p)) next
      for (v in .irOutputVariables(ir, o)) {
        used <- c(used, .irCollectAnchors(ir[["variables"]][[v]]))
      }
      used <- c(used, .irCollectAnchors(out[["options"]]))
    }
    final[[p]] <- .irUniqueAnchors(used)
  }
  list(candidate = candidate, final = final)
}
