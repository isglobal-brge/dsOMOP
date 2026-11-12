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
#' offsets), \code{cohort_end_date}, \code{op_start}, \code{op_end} and the
#' private \code{op_component_id} of the selected coverage component.
#'
#' @param roster_fragment A roster or candidates fragment.
#' @param phase \code{"candidate"} or \code{"final"}.
#' @return list(name, key, key_type, person, person_type, index, index_end,
#'   op_start, op_end, op_component, population, namespace).
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
  population <- attr(roster_fragment, "population", exact = TRUE) %||%
    roster_fragment$roster_namespace %||% sub("^(cand|pop)_", "", roster_fragment$name)
  list(name = roster_fragment$name, phase = phase, key = key, key_type = cols[[key]]$type,
       person = person, person_type = cols[[person]]$type,
       index = has("cohort_start_date"), index_end = has("cohort_end_date"),
       op_start = has("op_start"), op_end = has("op_end"),
       op_component = has("op_component_id"), population = population,
       namespace = if (identical(phase, "final")) roster_fragment$roster_namespace)
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

# The fragment a source anchor selects rows from.
.anchorSourceFragment <- function(ctx, a) {
  if (identical(a$source_kind, "event")) return(.compileEventByName(ctx, a$source))
  .compileIntervalByName(ctx, a$source)$course
}

# Deterministic name of the anchor fragment of (population, phase, anchor).
.anchorFragmentName <- function(ctx, spec, anchor) {
  memo <- .compileMemo(ctx, "anchor_names")
  canonical <- .jcsCanonical(if (is.list(anchor)) unclass(anchor) else anchor)
  key <- paste(spec$phase, spec$population, spec$name, canonical, sep = "\r")
  if (exists(key, envir = memo, inherits = FALSE)) return(get(key, envir = memo, inherits = FALSE))
  inventory <- ctx[["anchor_inventory"]]
  if (is.null(inventory) && !is.null(ctx[["ir"]])) {
    inventory <- tryCatch(.irAnchorInventory(ctx[["ir"]]), error = function(e) list())
    assign("anchor_inventory", inventory, envir = ctx)
  }
  listed <- inventory[[spec$phase]][[spec$population]] %||% list()
  keys <- vapply(listed, function(x) .jcsCanonical(x), "")
  k <- match(canonical, keys)
  if (is.na(k)) {
    extra <- (ctx[["anchor_extra"]] %||% list())
    pk <- paste(spec$phase, spec$population, sep = "\r")
    used <- extra[[pk]] %||% 0L
    k <- length(listed) + used + 1L
    extra[[pk]] <- used + 1L
    assign("anchor_extra", extra, envir = ctx)
  }
  name <- .fragmentName("anc", paste0(spec$population, "_", k),
                        if (identical(spec$phase, "candidate")) "cand")
  assign(key, name, envir = memo)
  name
}

#' Compile one anchor against a roster (frozen compiler signature, section 6)
#'
#' @param ctx Execution context.
#' @param anchor An anchor (built-in name, date object or source anchor).
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
  h <- .compileDialect(ctx)
  name <- .anchorFragmentName(ctx, spec, anchor)
  existing <- .fragmentGet(ctx, name)
  if (!is.null(existing)) return(existing)
  r <- function(col) paste0("r.", col)
  deps <- spec$name
  if (a$kind %in% c("builtin", "date")) {
    expr <- if (identical(a$kind, "date")) .sqlDateLiteral(h, a$date) else {
      col <- spec[[switch(a$name, index = "index", index_end = "index_end",
                          observation_start = "op_start", observation_end = "op_end")]]
      if (is.null(col)) stop("The roster has no column for anchor '", a$name, "'.", call. = FALSE)
      r(col)
    }
    body <- paste0("SELECT ", r(spec$key), " AS row_key, ", r(spec$person), " AS person_id, ",
                   expr, " AS anchor_date FROM ", spec$name, " r")
  } else {
    src <- .anchorSourceFragment(ctx, a)
    deps <- c(deps, src$name)
    order <- unique(c(src$ordering, src$unique_key))
    order <- c("start_date", setdiff(order, c("start_date", "end_date")))
    order_items <- switch(a$which,
      first = , nth = paste0("e.", order),
      last = paste0("e.", order, " DESC"),
      latest_end = c("e.end_date DESC", paste0("e.", order, " DESC")))
    rn <- .sqlRowNumber(h, r(spec$key), order_items)
    on <- c(paste0("e.person_id = ", r(spec$person)), "e.start_date IS NOT NULL")
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
    inner <- paste0("SELECT ", r(spec$key), " AS row_key, ", r(spec$person), " AS person_id, ",
                    field, " AS anchor_date, ", rn, " AS rn FROM ", spec$name, " r",
                    if (length(joins)) paste0(" ", paste(unname(joins), collapse = " ")),
                    " INNER JOIN ", src$name, " e ON ", paste(on, collapse = " AND "))
    body <- paste0("SELECT x.row_key AS row_key, x.person_id AS person_id, x.anchor_date AS anchor_date FROM (",
                   inner, ") x WHERE x.rn = ", .sqlIntegerLiteral(h, if (identical(a$which, "nth")) a$n else 1L))
  }
  f <- .fragment(name, "anchor", "episode", body,
                 columns = list(row_key = .fragmentColumn(spec$key_type, FALSE, "episode_key"),
                                person_id = .fragmentColumn(spec$person_type, FALSE, "person_key"),
                                anchor_date = .fragmentColumn("date", identical(a$kind, "builtin"), "start")),
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
