# Module: Anchors and windows (design-2.10.3 sections 2.4, 6.1 and 6.8)
#
# A window is whole inclusive days relative to anchors: integer bounds are
# offsets from the window's own anchor, a `bound` {anchor, offset} carries its
# own anchor, and a NULL bound is unbounded (a fully unbounded window needs no
# anchor). Anchors are
#   built-ins   roster expressions: index -> cohort_start_date, index_end ->
#               cohort_end_date, observation_start / observation_end -> the
#               selected coverage record or component (op_start / op_end);
#   dates       {date: "YYYY-MM-DD"} (.sqlDateLiteral);
#   sources     {source: {event|interval}, which, n, field, scope, search}: one
#               complete row of the source per roster row, selected by the
#               total order (start_date, source_ns, row key) -- first / nth
#               ascending, last descending, latest_end by end descending (valid
#               only with field = "end") -- whose start or end (field) is the
#               anchor date. scope = "episode" confines the rows to the
#               episode's observed span (op_start .. op_end); `search` is a
#               window compiled through this same resolver (acyclic), so "the
#               first B after the selected A" is expressible.
# A source anchor compiles to one fragment anc_<pop>_<k> per anchor and roster
# (phase "candidate" against cand_<pop>, keyed by cand_key; phase "final"
# against pop_<id>, keyed by cohort_row_id); k is the anchor's position in the
# IR's anchor inventory (.irOrder()$anchors), and the same definition is
# compiled twice when both stages use it (the candidate one is suffixed
# _cand). A roster row without a selected row has no anchor (left join: NULL),
# which makes every window that requires it undefined: the window predicate is
# UNKNOWN, a criterion over it is UNKNOWN (guard: .windowGuardSql()), a reducer
# NA.
#
# The roster contract (A-20 (a)): a roster or candidates fragment names its
# population explicitly (attr(f, "population") for candidates,
# roster_namespace for final rosters), its row key, and, where a consumer
# needs them, index_visit_id and the lossless index_source / index_key of its
# origin (design 6.9). One consumer-association resolver
# (.consumerAssociation()) serves streams and anchors: an index-visit event
# joins source.visit_id = roster.index_visit_id (a NULL visit matches nothing);
# an episode-indexed consumer of an adapter source (roster index_source = the
# adapter's parent namespace) joins the parent identity (parent_source_ns,
# parent_event_id) to the row's origin and the person, reading the per-parent
# interval relation for an interval; other rows keep the person association.
# Missing association metadata fails closed; it never broadens to person
# scope.

.ANCHOR_BUILTINS <- c(index = "cohort_start_date", index_end = "cohort_end_date",
                      observation_start = "op_start", observation_end = "op_end")
.ANCHOR_FIELDS <- c("source", "which", "n", "field", "scope", "search")
.ANCHOR_SEARCH_DEPTH <- 8L

#' Normalise a phase name
#' @keywords internal
.anchorPhase <- function(phase) {
  if (is.numeric(phase) && length(phase) == 1L) phase <- if (phase == 4) "candidate" else if (phase == 6) "final" else ""
  if (!is.character(phase) || length(phase) != 1L || !phase %in% c("candidate", "final")) {
    stop("phase must be \"candidate\" or \"final\".", call. = FALSE)
  }
  phase
}

#' Column contract of a roster fragment, for anchors, windows and coverage
#'
#' Candidates (\code{cand_<pop>}) are keyed by the private \code{cand_key};
#' final rosters (\code{pop_<id>}) by \code{cohort_row_id}. Both carry the
#' person column of their lineage (\code{subject_id} or \code{person_id}) and,
#' where the population has them, \code{cohort_start_date} (the index after
#' offsets), \code{cohort_end_date}, \code{op_start}, \code{op_end}, the
#' private \code{op_component_id} of the selected coverage component,
#' \code{index_visit_id} and the origin \code{index_source} /
#' \code{index_key} (design 6.9). The population is named explicitly --
#' \code{attr(f, "population")} for candidates, \code{roster_namespace} for
#' final rosters -- and never derived from the fragment name, which an
#' allocated identifier cannot be reversed to (A-20 (a)); a roster may declare
#' the possible \code{index_source} values of its rows as \code{attr(f,
#' "index_sources")}.
#'
#' @param roster_fragment A roster or candidates fragment.
#' @param phase \code{"candidate"} or \code{"final"}.
#' @return list(name, phase, key, key_type, person, person_type, index,
#'   index_end, op_start, op_end, op_component, population, namespace,
#'   index_visit, index_source, index_key, index_sources).
#' @keywords internal
.rosterSpec <- function(roster_fragment, phase) {
  .assertFragment(roster_fragment)
  phase <- .anchorPhase(phase)
  cols <- roster_fragment$columns
  key <- if (identical(phase, "candidate")) "cand_key" else "cohort_row_id"
  if (!key %in% names(cols)) {
    stop("A ", phase, " roster fragment needs its row key '", key, "'.", call. = FALSE)
  }
  person <- roster_fragment$person_lineage$person_col %||%
    intersect(c("subject_id", "person_id"), names(cols))[1L]
  if (!.isScalarString(person) || !person %in% names(cols)) {
    stop("A roster fragment needs a person column.", call. = FALSE)
  }
  has <- function(col) if (col %in% names(cols)) col
  candidate <- identical(phase, "candidate")
  population <- if (candidate) attr(roster_fragment, "population", exact = TRUE) else
    roster_fragment$roster_namespace
  if (!.isScalarString(population)) {
    stop("A ", phase, " roster fragment names its population (",
         if (candidate) "attr(f, \"population\")" else "roster_namespace",
         "); it is never derived from the fragment name (A-20 (a)).", call. = FALSE)
  }
  sources <- attr(roster_fragment, "index_sources", exact = TRUE)
  if (!is.null(sources) && (!is.character(sources) || !length(sources) || anyNA(sources))) {
    stop("attr(f, \"index_sources\") is a non-empty character vector.", call. = FALSE)
  }
  list(name = roster_fragment$name, phase = phase, key = key, key_type = cols[[key]]$type,
       person = person, person_type = cols[[person]]$type,
       index = has("cohort_start_date"), index_end = has("cohort_end_date"),
       op_start = has("op_start"), op_end = has("op_end"),
       op_component = has("op_component_id"), population = population,
       namespace = if (!candidate) roster_fragment$roster_namespace,
       index_visit = has("index_visit_id"), index_source = has("index_source"),
       index_key = has("index_key"), index_sources = sources)
}

#' The possible index_source values of a population's rows (from the IR)
#'
#' An index population's sources (an event or interval name, each of an
#' \code{any_of}; \code{"cohort:<id>"} for a results cohort,
#' \code{"legacy_spec"} for a legacy spec), a criteria population's parent's
#' (or \code{"cohort:<id>"}, or \code{"(persons)"} without either), the
#' union over a set operation's members, \code{"match:<pop>"} for a matched
#' population. NULL when the population is unknown or a member cannot be
#' resolved: a consumer then needs the rows' own metadata. Tags that are not
#' plan names can never equal an adapter's parent namespace.
#'
#' @param ir Normalised IR.
#' @param pop Population id.
#' @return Character vector or NULL.
#' @keywords internal
.irPopulationIndexSources <- function(ir, pop, seen = character(0)) {
  pops <- if (is.list(ir)) ir[["populations"]] else NULL
  p <- if (.isScalarString(pop) && is.list(pops)) pops[[pop]]
  if (!is.list(p) || pop %in% seen) return(NULL)
  seen <- c(seen, pop)
  scalar <- function(x) if (is.list(x) && length(x) == 1L && is.null(names(x))) x[[1L]] else x
  of_source <- function(src) {
    if (!is.list(src) || !length(src)) return(NULL)
    if (!is.null(src[["any_of"]])) {
      parts <- lapply(src[["any_of"]], of_source)
      if (any(vapply(parts, is.null, logical(1)))) return(NULL)
      return(unique(unlist(parts)))
    }
    if (length(src) == 1L && names(src) %in% c("event", "interval")) {
      ref <- unlist(src[[1L]])
      return(if (.isScalarString(ref)) ref)
    }
    if (!is.null(src[["cohort_definition_id"]])) {
      return(paste0("cohort:", format(unlist(src[["cohort_definition_id"]]), scientific = FALSE)))
    }
    if (!is.null(src[["legacy_spec"]])) return("legacy_spec")
    NULL
  }
  kind <- scalar(p[["kind"]])
  if (!.isScalarString(kind)) return(NULL)
  switch(kind,
    index = of_source(p[["index"]][["source"]]),
    criteria = {
      parent <- scalar(p[["parent"]])
      cohort <- scalar(p[["cohort_definition_id"]])
      if (.isScalarString(parent)) {
        .irPopulationIndexSources(ir, parent, seen)
      } else if (is.numeric(cohort) && length(cohort) == 1L) {
        paste0("cohort:", format(cohort, scientific = FALSE))
      } else "(persons)"
    },
    setop = {
      members <- unlist(p[["setop"]][["members"]])
      parts <- lapply(members, function(m) .irPopulationIndexSources(ir, m, seen))
      if (!length(parts) || any(vapply(parts, is.null, logical(1)))) NULL else unique(unlist(parts))
    },
    match = paste0("match:", pop),
    NULL)
}

#' The consumer association of a source for a roster (A-20 (a), item 6; B3)
#'
#' One resolver for streams and anchors (and later consumers): the source's
#' base event (an interval's source) decides
#'   - the index visit: an event restricted by \code{visit = {index: true}}
#'     joins \code{source.visit_id = roster.index_visit_id} (the roster must
#'     carry \code{index_visit_id}; an interval over such an event is refused);
#'   - the episode parent: an adapter source whose parent namespace can be the
#'     roster's \code{index_source} (\code{attr(roster, "index_sources")}, else
#'     \code{.irPopulationIndexSources()}, else unknown) is read through the
#'     \code{"parent"} branch for the rows indexed by that parent (the parent
#'     identity bound to the row's origin and the person; an interval's
#'     per-parent relation) and through the \code{"person"} branch for the
#'     other rows; the roster must carry \code{index_source} and
#'     \code{index_key}.
#' Missing metadata fails closed, never broadening to person scope.
#'
#' @param ctx Execution context.
#' @param source Typed source reference \code{list(event = )} or
#'   \code{list(interval = )}.
#' @param roster_fragment Roster or candidates fragment.
#' @param phase \code{"candidate"} or \code{"final"}.
#' @param path JSON path used in rejections.
#' @return list(spec (\code{.rosterSpec()}), kind, ref, base, visit (logical),
#'   parent (namespace or NULL), availability, branches (\code{"person"},
#'   \code{"parent"} or both)).
#' @keywords internal
.consumerAssociation <- function(ctx, source, roster_fragment, phase, path = "/source") {
  spec <- .rosterSpec(roster_fragment, phase)
  if (!is.list(source) || length(source) != 1L || !names(source) %in% c("event", "interval")) {
    .irReject("a source is {event: <name>} or {interval: <name>}", field = path)
  }
  kind <- names(source)[[1L]]
  ref <- unlist(source[[1L]])
  if (!.isScalarString(ref)) .irReject("a source names one node", field = path)
  base <- if (identical(kind, "interval")) {
    .intervalSpec(.irNamedNode(ctx[["ir"]], "interval", ref), ref)$source
  } else ref
  .compileEventByName(ctx, base)
  emeta <- .eventMeta(ctx, base)
  out <- list(spec = spec, kind = kind, ref = ref, base = base,
              visit = isTRUE(emeta$index_visit), parent = emeta$parent,
              availability = emeta$availability, branches = "person")
  if (out$visit) {
    if (identical(kind, "interval")) {
      .irReject(paste0("interval '", ref, "': its source is restricted to the index visit"),
                field = path)
    }
    if (is.null(spec$index_visit)) {
      stop("The roster '", spec$name, "' carries no index_visit_id: the index-visit source '",
           base, "' cannot be scoped to its rows (fail closed).", call. = FALSE)
    }
  }
  if (!is.null(out$parent)) {
    sources <- spec$index_sources %||% .irPopulationIndexSources(ctx[["ir"]], spec$population)
    if (is.null(sources) || out$parent %in% sources) {
      if (is.null(spec$index_source) || is.null(spec$index_key)) {
        stop("The roster '", spec$name, "' carries no index_source/index_key: the members of ",
             "'", base, "' cannot be bound to an episode-indexed row (fail closed).", call. = FALSE)
      }
      out$branches <- if (!is.null(sources) && all(sources == out$parent)) "parent" else
        c("parent", "person")
    }
  }
  out
}

#' Join conditions of one consumer-association branch
#'
#' @param h Handle, context, profile or dialect name.
#' @param assoc The association (\code{.consumerAssociation()}).
#' @param branch \code{"person"} or \code{"parent"}.
#' @param i SQL alias of the source relation.
#' @param r SQL alias of the roster.
#' @return Character vector of SQL predicates (ANDed by the caller).
#' @keywords internal
.consumerJoinSql <- function(h, assoc, branch, i = "i", r = "r") {
  spec <- assoc$spec
  col <- function(a, x) paste0(a, ".", x)
  on <- paste0(col(i, "person_id"), " = ", col(r, spec$person))
  if (isTRUE(assoc$visit)) on <- c(on, paste0(col(i, "visit_id"), " = ", col(r, spec$index_visit)))
  if (identical(branch, "parent")) {
    on <- c(on, paste0(col(i, "parent_source_ns"), " = ", col(r, spec$index_source)),
            paste0(.sqlLosslessKeyText(h, col(i, "parent_event_id")), " = ", col(r, spec$index_key)))
  } else if ("parent" %in% assoc$branches) {
    src <- col(r, spec$index_source)
    on <- c(on, paste0("(", src, " IS NULL OR ", src, " <> ", .compileTextLiteral(assoc$parent), ")"))
  }
  on
}

#' The relation one consumer-association branch reads
#'
#' An event source: its event fragment (the branch's join binds the parent);
#' an interval: the branch's relation (\code{"parent"}: the per-parent
#' relation of an adapter source, A-20 item 6) -- its course
#' (\code{use = "course"}) or its recorded / effective rows.
#'
#' @param ctx Execution context.
#' @param assoc The association (\code{.consumerAssociation()}).
#' @param branch \code{"person"} or \code{"parent"}.
#' @param use \code{"rows"} or \code{"course"}.
#' @param coverage \code{"recorded"} or \code{"effective_supply"} (rows).
#' @param path JSON path used in rejections.
#' @return The fragment.
#' @keywords internal
.consumerRelation <- function(ctx, assoc, branch, use = "rows", coverage = "recorded",
                              path = "/coverage") {
  if (identical(assoc$kind, "event") || identical(use, "rows")) {
    return(.intervalRelation(ctx, stats::setNames(list(assoc$ref), assoc$kind), coverage,
                             path = path, association = branch))
  }
  .compileIntervalByName(ctx, assoc$ref, association = branch)$course
}

#' Classify and validate one anchor (defaults applied)
#'
#' @param anchor \code{"index"}, \code{"index_end"}, \code{"observation_start"},
#'   \code{"observation_end"}, \code{list(date = )} or a source anchor object.
#' @param path JSON path used in rejections.
#' @return list(kind = "builtin" | "date" | "source", ...).
#' @keywords internal
.anchorNormalize <- function(anchor, path = "/anchor") {
  if (is.null(anchor)) anchor <- "index"
  if (is.list(anchor) && length(anchor) == 1L && is.null(names(anchor)) &&
      is.character(anchor[[1L]])) anchor <- anchor[[1L]]
  if (is.character(anchor)) {
    if (length(anchor) != 1L || !anchor %in% names(.ANCHOR_BUILTINS)) {
      .irReject(paste0("unknown anchor '", paste(anchor, collapse = ","), "'"), field = path)
    }
    return(list(kind = "builtin", name = anchor, column = .ANCHOR_BUILTINS[[anchor]]))
  }
  if (!is.list(anchor) || is.null(names(anchor))) .irReject("an anchor is a name or an object", field = path)
  if (identical(names(anchor), "date")) {
    date <- anchor[["date"]]
    if (is.list(date)) date <- unlist(date)
    iso <- tryCatch(.sqlParseIsoDate(date), error = function(e) {
      .irReject("an anchor date is one ISO date (YYYY-MM-DD)", field = paste0(path, "/date"))
    })
    return(list(kind = "date", date = iso))
  }
  unknown <- setdiff(names(anchor), .ANCHOR_FIELDS)
  if (length(unknown)) {
    .irReject(paste0("unknown anchor field(s): ", paste(unknown, collapse = ", ")), field = path)
  }
  src <- anchor[["source"]]
  if (!is.list(src) || length(src) != 1L || !names(src) %in% c("event", "interval")) {
    .irReject("an anchor source is {event: <name>} or {interval: <name>}", field = paste0(path, "/source"))
  }
  ref <- unlist(src[[1L]])
  if (!.isScalarString(ref)) .irReject("an anchor source names one node", field = paste0(path, "/source"))
  scalar <- function(x) if (is.list(x) && length(x) == 1L) x[[1L]] else x
  which <- scalar(anchor[["which"]] %||% "first")
  field <- scalar(anchor[["field"]] %||% "start")
  scope <- scalar(anchor[["scope"]] %||% "person")
  n <- scalar(anchor[["n"]] %||% 1L)
  if (!is.character(which) || !which %in% c("first", "last", "nth", "latest_end")) {
    .irReject("anchor which is first, last, nth or latest_end", field = paste0(path, "/which"))
  }
  if (!is.character(field) || !field %in% c("start", "end")) {
    .irReject("anchor field is start or end", field = paste0(path, "/field"))
  }
  if (!is.character(scope) || !scope %in% c("person", "episode")) {
    .irReject("anchor scope is person or episode", field = paste0(path, "/scope"))
  }
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n != trunc(n) || n < 1) {
    .irReject("anchor n is an integer >= 1", field = paste0(path, "/n"))
  }
  if (identical(which, "latest_end") && !identical(field, "end")) {
    .irReject("which = \"latest_end\" selects by end and is valid only with field = \"end\"",
              field = paste0(path, "/field"))
  }
  search <- anchor[["search"]]
  if (is.list(search) && !length(search)) search <- NULL
  list(kind = "source", source_kind = names(src)[[1L]], source = ref, which = which,
       n = as.integer(n), field = field, scope = scope, search = search)
}

# Canonical form of an anchor (defaults applied, nested search windows
# canonical too), so that an anchor matches its IR inventory entry whether or
# not the caller's copy carries the normalised defaults.
.anchorCanonical <- function(anchor) {
  if (is.null(anchor)) return("index")
  if (is.list(anchor) && length(anchor) == 1L && is.null(names(anchor)) &&
      is.character(anchor[[1L]])) anchor <- anchor[[1L]]
  if (is.character(anchor)) return(anchor)
  if (is.list(anchor) && identical(names(anchor), "date")) return(list(date = unlist(anchor[["date"]])))
  scalar <- function(x, default) {
    if (is.null(x)) return(default)
    if (is.list(x) && length(x) == 1L) x <- x[[1L]]
    x
  }
  src <- anchor[["source"]]
  list(source = stats::setNames(list(unlist(src[[1L]])), names(src)[[1L]]),
       which = scalar(anchor[["which"]], "first"), n = as.integer(scalar(anchor[["n"]], 1L)),
       field = scalar(anchor[["field"]], "start"), scope = scalar(anchor[["scope"]], "person"),
       search = .windowCanonical(anchor[["search"]]))
}

.windowCanonical <- function(window) {
  if (is.null(window) || (is.list(window) && !length(window))) return(NULL)
  bound <- function(x) {
    if (is.list(x) && length(x) == 1L && is.null(names(x))) x <- x[[1L]]
    if (is.null(x)) return(NULL)
    if (is.numeric(x)) return(as.integer(x))
    list(anchor = .anchorCanonical(x[["anchor"]]), offset = as.integer(unlist(x[["offset"]])))
  }
  list(start = bound(window[["start"]]), end = bound(window[["end"]]),
       anchor = .anchorCanonical(window[["anchor"]] %||% "index"))
}

.anchorKey <- function(anchor) .jcsCanonical(.anchorCanonical(anchor))

# The IR's anchor inventory (.irOrder()$anchors), keyed canonically; memoised.
.anchorInventoryKeys <- function(ctx) {
  cached <- ctx[["anchor_inventory_keys"]]
  if (!is.null(cached)) return(cached)
  inventory <- if (!is.null(ctx[["ir"]])) .irAnchorInventory(ctx[["ir"]]) else list()
  keys <- lapply(inventory, function(phase) {
    lapply(phase, function(anchors) vapply(anchors, .anchorKey, ""))
  })
  assign("anchor_inventory_keys", keys, envir = ctx)
  keys
}

#' Identities of the anchor fragments of a plan (amendment A-17)
#'
#' One per distinct source anchor of the IR's anchor inventory
#' (\code{.irOrder()$anchors}): \code{("anc", "<pop>_<k>", "cand")} for the
#' candidate stage and \code{("anc", "<pop>_<k>")} for the final stage, k
#' the anchor's position in the population's sorted inventory.
#' @keywords internal
.anchorFragmentIdentities <- function(ir) {
  inventory <- .irAnchorInventory(ir)
  out <- list()
  for (phase in c("candidate", "final")) {
    for (p in names(inventory[[phase]])) {
      for (k in seq_along(inventory[[phase]][[p]])) {
        out[[length(out) + 1L]] <- .fragmentIdentity(
          "anc", paste0(p, "_", k), if (identical(phase, "candidate")) "cand")
      }
    }
  }
  out
}

# The allocated name of the anchor fragment of (population, phase, anchor):
# the anchor's position in the population's inventory for that phase. An
# anchor the IR does not declare cannot be named (fail closed, A-17).
.anchorFragmentName <- function(ctx, spec, anchor) {
  keys <- .anchorInventoryKeys(ctx)[[spec$phase]][[spec$population]] %||% character(0)
  k <- match(.anchorKey(anchor), keys)
  if (is.na(k)) {
    stop("Anchor not declared by the plan's anchor inventory for population '",
         spec$population, "' (", spec$phase, " stage); it cannot be compiled.", call. = FALSE)
  }
  .fragmentName("anc", paste0(spec$population, "_", k),
                if (identical(spec$phase, "candidate")) "cand", ctx = ctx)
}

#' Compile one anchor against a roster (frozen compiler signature, section 6)
#'
#' @param ctx Execution context.
#' @param anchor A source anchor of the IR's anchor inventory (built-in and
#'   date anchors are roster expressions: \code{.anchorExpr()}).
#' @param roster_fragment The candidates fragment (\code{phase =
#'   "candidate"}) or the final roster (\code{phase = "final"}).
#' @param phase \code{"candidate"} (design phase 4) or \code{"final"} (phase 6).
#' @param .depth Search-window nesting depth (internal).
#' @return The anchor fragment \code{anc_<pop>_<k>} (kind anchor, grain
#'   episode): \code{row_key} (the phase's row key), \code{person_id},
#'   \code{anchor_date}; one row per roster row that has the anchor.
#' @keywords internal
.compileAnchor <- function(ctx, anchor, roster_fragment, phase, .depth = 0L) {
  if (.depth > .ANCHOR_SEARCH_DEPTH) .irReject("anchor search windows are nested too deeply")
  spec <- .rosterSpec(roster_fragment, phase)
  a <- .anchorNormalize(anchor)
  if (!identical(a$kind, "source")) {
    stop("Built-in and date anchors are roster expressions without a fragment; ",
         "use .anchorExpr().", call. = FALSE)
  }
  h <- .compileDialect(ctx)
  name <- .anchorFragmentName(ctx, spec, anchor)
  existing <- .fragmentGet(ctx, name)
  if (!is.null(existing)) return(existing)
  r <- function(col) paste0("r.", col)
  # the consumer association (index visit, episode parent, person; B3)
  assoc <- .consumerAssociation(ctx, stats::setNames(list(a$source), a$source_kind),
                                roster_fragment, phase, path = "/anchor/source")
  srcs <- lapply(assoc$branches, function(b) .consumerRelation(ctx, assoc, b, use = "course"))
  deps <- unique(c(spec$name, vapply(srcs, function(x) x$name, "")))
  src <- srcs[[1L]]
  order <- unique(c(src$ordering, src$unique_key))
  order <- c("start_date", setdiff(order, c("start_date", "end_date")))
  on <- "e.start_date IS NOT NULL"
  if (identical(a$field, "end")) on <- c(on, "e.end_date IS NOT NULL")
  joins <- character(0)
  if (!is.null(a$search)) {
    win <- .compileWindow(ctx, a$search, roster_fragment, phase, alias = "r",
                          .depth = .depth + 1L)
    joins <- win$joins
    deps <- c(deps, win$dependencies)
    pred <- .windowStartPredicateSql(win, "e.start_date")
    if (!is.null(win$defined)) on <- c(on, win$defined)
    if (!is.null(pred)) on <- c(on, pred)
  }
  if (identical(a$scope, "episode")) {
    if (is.null(spec$op_start) || is.null(spec$op_end)) {
      stop("scope = \"episode\" needs the roster's coverage bounds (op_start, op_end).",
           call. = FALSE)
    }
    on <- c(on, paste0("e.start_date >= ", r(spec$op_start)), paste0("e.start_date <= ", r(spec$op_end)))
  }
  field <- if (identical(a$field, "end")) "e.end_date" else "e.start_date"
  joined <- if (length(joins)) paste0(" ", paste(unname(joins), collapse = " ")) else ""
  rank <- function(alias, cols) {
    switch(a$which,
      first = , nth = paste0(alias, ".", cols),
      last = paste0(alias, ".", cols, " DESC"),
      latest_end = c(paste0(alias, ".", cols[[length(cols)]], " DESC"),
                     paste0(alias, ".", cols[-length(cols)], " DESC")))
  }
  ranked_cols <- if (identical(a$which, "latest_end")) c(order, "end_date") else order
  branch_on <- function(b) paste(c(.consumerJoinSql(h, assoc, b, "e", "r"), on), collapse = " AND ")
  inner <- if (length(srcs) == 1L) {
    rn <- .sqlRowNumber(h, r(spec$key), rank("e", ranked_cols))
    paste0("SELECT ", r(spec$key), " AS row_key, ", r(spec$person), " AS person_id, ",
           field, " AS anchor_date, ", rn, " AS rn FROM ", spec$name, " r", joined,
           " INNER JOIN ", src$name, " e ON ", branch_on(assoc$branches[[1L]]))
  } else {
    # rows indexed by the adapter's parent and the other rows read different
    # relations; each roster row matches one branch, ranked over the union
    oc <- paste0("o_", seq_along(ranked_cols))
    parts <- vapply(seq_along(srcs), function(k) {
      paste0("SELECT ", r(spec$key), " AS row_key, ", r(spec$person), " AS person_id, ",
             field, " AS anchor_date, ", paste(paste0("e.", ranked_cols, " AS ", oc), collapse = ", "),
             " FROM ", spec$name, " r", joined, " INNER JOIN ", srcs[[k]]$name, " e ON ",
             branch_on(assoc$branches[[k]]))
    }, "")
    rn <- .sqlRowNumber(h, "y.row_key", rank("y", oc))
    paste0("SELECT y.row_key AS row_key, y.person_id AS person_id, y.anchor_date AS anchor_date, ",
           rn, " AS rn FROM (", paste(parts, collapse = " UNION ALL "), ") y")
  }
  body <- paste0("SELECT x.row_key AS row_key, x.person_id AS person_id, x.anchor_date AS anchor_date FROM (",
                 inner, ") x WHERE x.rn = ", .sqlIntegerLiteral(h, if (identical(a$which, "nth")) a$n else 1L))
  f <- .fragment(name, "anchor", "episode", body,
                 columns = list(row_key = .fragmentColumn(spec$key_type, FALSE, "episode_key"),
                                person_id = .fragmentColumn(spec$person_type, FALSE, "person_key"),
                                anchor_date = .fragmentColumn("date", FALSE, "start")),
                 dependencies = unique(deps), unique_key = "row_key",
                 person_lineage = list(person_col = "person_id", via_roster = NULL),
                 roster_namespace = spec$namespace, private_columns = "row_key")
  .sqlAssertTypedSql(h, f$select_body)
  .fragmentRegister(ctx, f)
}

#' SQL expression of an anchor for the rows of a roster alias
#'
#' Built-in anchors are roster columns and dates are literals (no join);
#' a source anchor joins its fragment on the phase's row key.
#'
#' @param ctx Execution context.
#' @param anchor An anchor.
#' @param roster_fragment Roster or candidates fragment.
#' @param phase \code{"candidate"} or \code{"final"}.
#' @param alias SQL alias of the roster in the consumer's query.
#' @return list(expr, joins (named by alias), dependencies).
#' @keywords internal
.anchorExpr <- function(ctx, anchor, roster_fragment, phase, alias = "r", .depth = 0L) {
  spec <- .rosterSpec(roster_fragment, phase)
  a <- .anchorNormalize(anchor)
  h <- .compileDialect(ctx)
  if (identical(a$kind, "date")) {
    return(list(expr = .sqlDateLiteral(h, a$date), joins = character(0), dependencies = character(0)))
  }
  if (identical(a$kind, "builtin")) {
    slot <- switch(a$name, index = "index", index_end = "index_end",
                   observation_start = "op_start", observation_end = "op_end")
    col <- spec[[slot]]
    if (is.null(col)) {
      stop("The roster '", spec$name, "' has no column for anchor '", a$name, "'.", call. = FALSE)
    }
    return(list(expr = paste0(alias, ".", col), joins = character(0), dependencies = character(0)))
  }
  f <- .compileAnchor(ctx, anchor, roster_fragment, phase, .depth = .depth)
  a_alias <- paste0("a_", f$name)
  if (nchar(a_alias) > 63L) a_alias <- paste0("a_", substr(.fragmentName("x", f$name), 3L, 62L))
  join <- paste0("LEFT JOIN ", f$name, " ", a_alias, " ON ", a_alias, ".row_key = ", alias, ".", spec$key)
  list(expr = paste0(a_alias, ".anchor_date"), joins = stats::setNames(join, a_alias),
       dependencies = f$name)
}

# One bound of a window -> list(anchor, offset) or NULL.
.windowBound <- function(x, default_anchor, path) {
  if (is.null(x)) return(NULL)
  if (is.list(x) && length(x) == 1L && is.null(names(x))) x <- x[[1L]]
  if (is.null(x)) return(NULL)
  if (is.numeric(x)) {
    if (length(x) != 1L || is.na(x) || !is.finite(x) || x != trunc(x) || abs(x) > .Machine$integer.max) {
      .irReject("a window bound is an integer number of days", field = path)
    }
    return(list(anchor = default_anchor, offset = as.integer(x)))
  }
  if (is.list(x) && setequal(names(x), c("anchor", "offset"))) {
    off <- x[["offset"]]
    if (is.list(off)) off <- unlist(off)
    if (!is.numeric(off) || length(off) != 1L || is.na(off) || off != trunc(off)) {
      .irReject("a bound offset is an integer number of days", field = paste0(path, "/offset"))
    }
    return(list(anchor = x[["anchor"]], offset = as.integer(off)))
  }
  .irReject("a window bound is null, an integer or {anchor, offset}", field = path)
}

#' Resolve a window against a roster (design section 6.8)
#'
#' @param ctx Execution context.
#' @param window NULL (unbounded) or a window object \code{list(start, end,
#'   anchor)} whose bounds are NULL, integers or \code{list(anchor, offset)}.
#' @param roster_fragment Roster or candidates fragment.
#' @param phase \code{"candidate"} or \code{"final"}.
#' @param alias SQL alias of the roster in the consumer's query.
#' @return list(lower, upper (SQL date expressions or NULL), required (anchor
#'   expressions the window needs), defined (SQL predicate "every required
#'   anchor is present", NULL when nothing is required), joins (named LEFT
#'   JOIN clauses to add after the roster alias), dependencies).
#' @keywords internal
.compileWindow <- function(ctx, window, roster_fragment, phase, alias = "r", .depth = 0L) {
  h <- .compileDialect(ctx)
  out <- list(lower = NULL, upper = NULL, required = character(0), defined = NULL,
              joins = character(0), dependencies = character(0))
  if (is.null(window)) return(out)
  if (!is.list(window) || (length(window) && is.null(names(window)))) {
    .irReject("a window is an object with start, end and anchor", field = "/window")
  }
  unknown <- setdiff(names(window), c("start", "end", "anchor"))
  if (length(unknown)) .irReject(paste0("unknown window field(s): ", paste(unknown, collapse = ", ")),
                                 field = "/window")
  default_anchor <- window[["anchor"]] %||% "index"
  bounds <- list(lower = .windowBound(window[["start"]], default_anchor, "/window/start"),
                 upper = .windowBound(window[["end"]], default_anchor, "/window/end"))
  for (side in names(bounds)) {
    b <- bounds[[side]]
    if (is.null(b)) next
    ax <- .anchorExpr(ctx, b$anchor, roster_fragment, phase, alias = alias, .depth = .depth)
    out[[side]] <- if (b$offset == 0L) ax$expr else .sqlDateAddDays(h, ax$expr, b$offset)
    out$required <- unique(c(out$required, ax$expr))
    out$joins <- c(out$joins, ax$joins[!names(ax$joins) %in% names(out$joins)])
    out$dependencies <- unique(c(out$dependencies, ax$dependencies))
  }
  if (length(out$required)) {
    out$defined <- paste(paste0(out$required, " IS NOT NULL"), collapse = " AND ")
  }
  out
}

#' Window predicate on a date column (inclusive bounds)
#' @param win A resolved window (\code{.compileWindow()}).
#' @param col SQL date expression (e.g. \code{"e.start_date"}).
#' @return SQL predicate, or NULL when the window is unbounded.
#' @keywords internal
.windowStartPredicateSql <- function(win, col) {
  parts <- c(if (!is.null(win$lower)) paste0(col, " >= ", win$lower),
             if (!is.null(win$upper)) paste0(col, " <= ", win$upper))
  if (!length(parts)) return(NULL)
  paste(parts, collapse = " AND ")
}

#' Window predicate for closed intervals overlapping the window
#' @inheritParams .windowStartPredicateSql
#' @param start_col,end_col SQL date expressions of the interval.
#' @keywords internal
.windowOverlapSql <- function(win, start_col, end_col) {
  parts <- c(if (!is.null(win$upper)) paste0(start_col, " <= ", win$upper),
             if (!is.null(win$lower)) paste0(end_col, " >= ", win$lower))
  if (!length(parts)) return(NULL)
  paste(parts, collapse = " AND ")
}

#' UNKNOWN guard of a window-dependent leaf
#'
#' \code{CASE WHEN <every required anchor present> THEN (<leaf>) END}: a
#' missing anchor makes the leaf UNKNOWN (NULL) -- also for NOT EXISTS shaped
#' leaves such as \code{has_not} and \code{count == 0}.
#' @param win A resolved window.
#' @param leaf SQL boolean expression.
#' @keywords internal
.windowGuardSql <- function(win, leaf) {
  if (is.null(win$defined)) return(leaf)
  paste0("CASE WHEN ", win$defined, " THEN (", leaf, ") END")
}

#' Per-row window bounds as a fragment (for R-side reducers)
#'
#' @param ctx Execution context.
#' @param window Window object or NULL.
#' @param roster_fragment Roster or candidates fragment.
#' @param phase \code{"candidate"} or \code{"final"}.
#' @param name Fragment name.
#' @return Fragment with \code{row_key}, \code{person_id}, \code{win_start},
#'   \code{win_end} (NULL = unbounded) and \code{win_defined} (1 when every
#'   required anchor is present).
#' @keywords internal
.compileWindowBounds <- function(ctx, window, roster_fragment, phase, name) {
  existing <- .fragmentGet(ctx, name)
  if (!is.null(existing)) return(existing)
  h <- .compileDialect(ctx)
  spec <- .rosterSpec(roster_fragment, phase)
  win <- .compileWindow(ctx, window, roster_fragment, phase, alias = "r")
  defined <- if (is.null(win$defined)) .sqlIntegerLiteral(h, 1L) else
    paste0("CASE WHEN ", win$defined, " THEN 1 ELSE 0 END")
  body <- paste0("SELECT r.", spec$key, " AS row_key, r.", spec$person, " AS person_id, ",
                 win$lower %||% .sqlNullDate(h), " AS win_start, ",
                 win$upper %||% .sqlNullDate(h), " AS win_end, ", defined, " AS win_defined FROM ",
                 spec$name, " r", if (length(win$joins)) paste0(" ", paste(unname(win$joins), collapse = " ")))
  f <- .fragment(name, "stream", "episode", body,
                 columns = list(row_key = .fragmentColumn(spec$key_type, FALSE, "episode_key"),
                                person_id = .fragmentColumn(spec$person_type, FALSE, "person_key"),
                                win_start = .fragmentColumn("date", TRUE, "start"),
                                win_end = .fragmentColumn("date", TRUE, "end"),
                                win_defined = .fragmentColumn("integer", FALSE, "measure")),
                 dependencies = unique(c(spec$name, win$dependencies)), unique_key = "row_key",
                 person_lineage = list(person_col = "person_id", via_roster = NULL),
                 roster_namespace = spec$namespace, private_columns = "row_key")
  .sqlAssertTypedSql(h, f$select_body)
  .fragmentRegister(ctx, f)
}
