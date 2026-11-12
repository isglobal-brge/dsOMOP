# Module: Events (design-2.10.3 sections 2.2, 6.2 and 6.6)
#
# One compiler for every table event. Three stages, each a fragment (CTE), so
# that unit normalisation precedes every predicate and reducer:
#
#   ev_<name>_src   projection of the table's roles (role registry, never
#                   name heuristics): person, event key, start, the EVENT's own
#                   end policy, the original recorded end and supply (kept for
#                   era overrides and supply reducers), the scoping concept,
#                   value/unit/operator/range SOURCE columns, the visit link,
#                   and the table's other filterable columns; restricted by the
#                   concept set (state all/set/none) and a visit-type EXISTS;
#   ev_<name>_norm  normalised value, unit and ranges (one affine map per
#                   source unit, the target projected for converted rows;
#                   unknown units NA + unit_unknown = 1 or dropped; reviewed
#                   inequality operators kept or dropped);
#   ev_<name>       the `where` predicate over the NORMALISED columns, with
#                   native three-valued logic.
#
# Roles a table lacks are projected as typed NULLs so every event fragment has
# one column contract; predicates naming such a role are refused (no silent
# NULL comparison). Raw role column names bind to their projections: the
# configured value column and `value` bind to the normalised value, the unit
# column to the normalised unit, the start column to start_date, the recorded
# end column to end_recorded, days_supply to supply, the concept column to
# concept_id. Source value/unit stay available as value_source / unit_source.
#
# on_missing_table = "unavailable": a table this site lacks compiles to a
# zero-row fragment with availability "unavailable" (criteria over it are
# UNKNOWN, reducers NA, panels contribute nothing -- the consumers' contract);
# "error" refuses with unavailable_table. Index-visit events (visit = {index:
# true}) are joined at the consumer on visit_id = the roster's index visit.

# Core columns of every event fragment (stage 1), in projection order.
.EVENT_SRC_COLUMNS <- c("person_id", "event_id", "source_ns", "start_date",
                        "end_date", "end_recorded", "supply", "concept_id",
                        "value_source", "unit_source", "operator_concept_id",
                        "range_low_source", "range_high_source", "visit_id")
# Columns added by the normalisation stage.
.EVENT_NORM_COLUMNS <- c("value", "unit_concept_id", "range_low", "range_high",
                         "unit_unknown")
.EVENT_PRIVATE_COLUMNS <- c("event_id", "source_ns", "visit_id")
.EVENT_ORDERING <- c("start_date", "source_ns", "event_id")

.eventColumnSpec <- function(person_type = "integer") {
  list(
    person_id = .fragmentColumn(person_type, FALSE, "person_key"),
    event_id = .fragmentColumn("bigint", FALSE, "origin_identity"),
    source_ns = .fragmentColumn("text", FALSE, "origin_identity"),
    start_date = .fragmentColumn("date", TRUE, "start"),
    end_date = .fragmentColumn("date", TRUE, "end"),
    end_recorded = .fragmentColumn("date", TRUE, "end_recorded"),
    supply = .fragmentColumn("integer", TRUE, "supply"),
    concept_id = .fragmentColumn("integer", TRUE, "concept"),
    value_source = .fragmentColumn("numeric", TRUE, "value"),
    unit_source = .fragmentColumn("integer", TRUE, "unit"),
    operator_concept_id = .fragmentColumn("integer", TRUE, "operator"),
    range_low_source = .fragmentColumn("numeric", TRUE, "range"),
    range_high_source = .fragmentColumn("numeric", TRUE, "range"),
    visit_id = .fragmentColumn("bigint", TRUE, "visit"),
    value = .fragmentColumn("numeric", TRUE, "value"),
    unit_concept_id = .fragmentColumn("integer", TRUE, "unit"),
    range_low = .fragmentColumn("numeric", TRUE, "range"),
    range_high = .fragmentColumn("numeric", TRUE, "range"),
    unit_unknown = .fragmentColumn("integer", FALSE, "measure")
  )
}

#' Fragment column type of a blueprint column (NULL = not projected)
#'
#' Integer, numeric, date and text columns are projected; datetime and time
#' columns (class-2 timestamps whose day is the event's date role) are not.
#' @keywords internal
.eventColumnType <- function(col_df, column) {
  i <- match(column, col_df$column_name)
  if (is.na(i)) return(NULL)
  cdm <- tolower(as.character(col_df$cdm_datatype[[i]] %||% ""))
  db <- tolower(as.character(col_df$db_datatype[[i]] %||% ""))
  if (is.na(cdm)) cdm <- ""
  if (is.na(db)) db <- ""
  if (grepl("datetime|timestamp", cdm) || grepl("datetime|timestamp", db) ||
      grepl("_datetime$|_time$", column)) {
    return(NULL)
  }
  big <- grepl("bigint|int8|int64|longlong", db)
  if (cdm %in% c("integer", "bigint") || (!nzchar(cdm) && grepl("int", db))) {
    return(if (big || identical(cdm, "bigint")) "bigint" else "integer")
  }
  if (cdm %in% c("float", "numeric") ||
      (!nzchar(cdm) && grepl("real|double|float|numeric|decimal|number", db))) {
    return("numeric")
  }
  if (identical(cdm, "date") || (!nzchar(cdm) && identical(db, "date"))) return("date")
  if (startsWith(cdm, "varchar") || (!nzchar(cdm) && grepl("char|text|string|clob", db))) {
    return("text")
  }
  NULL
}

#' The name of an IR node handed to a compiler
#'
#' Compilers receive the IR node with its plan name in \code{attr(node,
#' "name")} (\code{.irNamedNode()}).
#' @keywords internal
.compileNodeName <- function(node, kind = "event") {
  name <- attr(node, "name", exact = TRUE)
  if (!.isScalarString(name) || !grepl(.NAME_PATTERN, name)) {
    stop("The ", kind, " compiler needs the node's plan name (attr(node, 'name')).",
         call. = FALSE)
  }
  name
}

#' An IR node carrying its plan name, as the compilers take it
#'
#' @param ir The IR.
#' @param kind Node kind (\code{"event"}, \code{"concept_set"}, ...).
#' @param name Node name.
#' @return The node with \code{attr(node, "name")}.
#' @keywords internal
.irNamedNode <- function(ir, kind, name) {
  node <- .irNode(ir, kind, name)
  attr(node, "name") <- name
  node
}

# Store event metadata (roles, catalogue, fragment names) in the context.
.eventMetaSet <- function(ctx, name, meta) {
  memo <- .compileMemo(ctx, "event_meta")
  assign(name, meta, envir = memo)
  invisible(meta)
}

#' Metadata of a compiled event source
#'
#' @param ctx Execution context.
#' @param name Event name.
#' @return list(name, kind ("table", "episode_members", "cost"), table,
#'   availability, roles (available role names), catalogue (the predicate
#'   catalogue of its final fragment), fragments (stage names), index_visit,
#'   concept (resolved state), role (role summary)), or NULL when not
#'   compiled.
#' @keywords internal
.eventMeta <- function(ctx, name) {
  memo <- if (is.environment(ctx) && is.environment(ctx[["event_meta"]])) ctx[["event_meta"]]
  if (is.null(memo) || !exists(name, envir = memo, inherits = FALSE)) return(NULL)
  get(name, envir = memo, inherits = FALSE)
}

#' Compile an event source by name, with its dependencies
#'
#' Resolves the node through \code{ctx$ir}, its concept set through
#' \code{.compileConceptSet()}, an adapter's parent event recursively, and
#' dispatches to \code{.compileEvent()}, \code{.compileEpisodeMembers()} or
#' \code{.compileCost()}. Memoised: the registered fragment is returned when
#' the event was compiled before.
#'
#' @param ctx Execution context (\code{handle}, \code{ir}, \code{dialect}).
#' @param name Event name.
#' @return The event's final fragment (\code{ev_<name>}).
#' @keywords internal
.compileEventByName <- function(ctx, name, .stack = character(0)) {
  meta <- .eventMeta(ctx, name)
  if (!is.null(meta)) return(.fragmentGet(ctx, meta$fragments[["final"]]))
  if (name %in% .stack) {
    .irReject(paste0("event reference cycle: ", paste(c(.stack, name), collapse = " -> ")),
              field = paste0("/events/", name))
  }
  node <- .irNamedNode(ctx[["ir"]], "event", name)
  if (!is.null(node[["episode_members"]])) {
    parent <- node[["episode_members"]][["episode"]][["event"]]
    pf <- .compileEventByName(ctx, parent, c(.stack, name))
    return(.compileEpisodeMembers(ctx, node, pf))
  }
  if (!is.null(node[["cost"]])) {
    parent <- node[["cost"]][["event"]][["event"]]
    pf <- .compileEventByName(ctx, parent, c(.stack, name))
    return(.compileCost(ctx, node, pf))
  }
  concept <- .compileConceptSet(ctx, node[["concept_set"]])
  .compileEvent(ctx, node, concept)
}

#' Compile a table event (frozen compiler signature, section 6)
#'
#' @param ctx Execution context (environment with \code{handle}, \code{ir}
#'   and the probed \code{dialect}).
#' @param node The IR event node (\code{event_table} variant) with its plan
#'   name in \code{attr(node, "name")}.
#' @param concept The resolved concept set of the node
#'   (\code{.compileConceptSet(ctx, node$concept_set)}); NULL resolves it.
#' @return The event fragment \code{ev_<name>} (kind event, grain row). Its
#'   dependencies \code{ev_<name>_norm} and \code{ev_<name>_src} are
#'   registered in the context; \code{.eventMeta(ctx, name)} describes it.
#' @keywords internal
.compileEvent <- function(ctx, node, concept) {
  name <- .compileNodeName(node)
  base <- paste0("/events/", name)
  if (!is.null(node[["episode_members"]]) || !is.null(node[["cost"]])) {
    stop("Adapter events compile through .compileEpisodeMembers() / .compileCost().",
         call. = FALSE)
  }
  existing <- .eventMeta(ctx, name)
  if (!is.null(existing)) return(.fragmentGet(ctx, existing$fragments[["final"]]))
  h <- .compileDialect(ctx)
  handle <- ctx[["handle"]]
  table <- tolower(as.character(node[["table"]] %||% ""))
  if (!nzchar(table)) .irReject("a table event needs a table", field = paste0(base, "/table"))
  if (is.null(concept)) concept <- .compileConceptSet(ctx, node[["concept_set"]])
  if (!is.null(node[["legacy_concept_scope"]])) {
    scope <- .compileConceptSet(ctx, node[["legacy_concept_scope"]][["concept_set_ref"]])
    concept <- .conceptIntersect(concept, scope)
  }
  mode <- node[["on_missing_table"]] %||% "error"
  role <- .roleOf(handle, table)
  if (is.null(role)) {
    if (identical(.roleAvailability(handle, table), "not_event_source")) {
      .irReject(paste0("'", table, "' is not an event table (no event role record)"),
                field = paste0(base, "/table"))
    }
    if (identical(mode, "unavailable")) {
      return(.eventUnavailable(ctx, name, table, kind = "table", concept = concept))
    }
    .irReject(paste0("table '", table, "' is not available at this site"),
              code = "unavailable_table", field = paste0(base, "/table"))
  }
  plan <- .eventPlan(ctx, node, name, role, concept)
  .eventEmit(ctx, plan)
}

# Resolve the role columns of one table event and validate the request
# against them (the handle-dependent checks of section 6.1, refused before any
# query: concept column with a concept role, numeric value column, unit and
# operator roles, the supply role for an end fallback, the visit link).
.eventPlan <- function(ctx, node, name, role, concept) {
  handle <- ctx[["handle"]]
  base <- paste0("/events/", name)
  table <- role[["table"]]
  bp <- .buildBlueprint(handle)
  col_df <- bp$columns[[table]]
  cols <- role[["columns"]]
  filterable <- cols$column_name[cols$filterable]
  type_of <- function(column) .eventColumnType(col_df, column)

  is_int <- function(column) isTRUE(type_of(column) %in% c("integer", "bigint"))
  is_num <- function(column) isTRUE(type_of(column) %in% c("integer", "bigint", "numeric"))
  concept_col <- node[["concept_col"]]
  if (!is.null(concept_col)) {
    concept_col <- tolower(concept_col)
    i <- match(concept_col, cols$column_name)
    role_name <- if (is.na(i)) "" else as.character(cols$concept_role[[i]] %||% "")
    if (is.na(role_name)) role_name <- ""
    is_concept <- !is.na(i) && (nzchar(role_name) || grepl("_concept_id$", concept_col))
    if (is.na(i) || !isTRUE(cols$filterable[[i]]) || !is_concept || !is_int(concept_col)) {
      .irReject(paste0("'", concept_col, "' is not a filterable concept column of '", table, "'"),
                field = paste0(base, "/concept_col"))
    }
  } else {
    concept_col <- role[["domain_concept"]]
  }
  if (is.null(concept_col) && !identical(concept[["state"]], "all")) {
    .irReject(paste0("table '", table, "' has no domain concept column: name the scoping ",
                     "column with concept_col"), field = paste0(base, "/concept_set"))
  }

  value_col <- node[["value"]][["column"]]
  if (!is.null(value_col)) {
    value_col <- tolower(value_col)
    if (!value_col %in% filterable || !is_num(value_col) ||
        grepl("_concept_id$", value_col)) {
      .irReject(paste0("'", value_col, "' is not a filterable numeric column of '", table, "'"),
                field = paste0(base, "/value/column"))
    }
  } else {
    value_col <- role[["value"]]
  }
  unit <- node[["unit"]]
  if (!is.null(unit)) {
    if (is.null(value_col)) {
      .irReject(paste0("a unit conversion needs a value column; table '", table,
                       "' has no value role (name one with value)"), field = paste0(base, "/unit"))
    }
    if (is.null(role[["unit"]])) {
      .irReject(paste0("a unit conversion needs a unit column; table '", table,
                       "' has no unit role"), field = paste0(base, "/unit"))
    }
    unit <- .eventUnitSpec(unit, paste0(base, "/unit"))
  }
  end <- node[["end"]]
  end_policy <- if (is.null(end)) {
    "role"
  } else if (identical(end, "start") || identical(unlist(end), "start")) {
    "start"
  } else if (is.list(end) && identical(end[["fallback"]], "days_supply")) {
    if (is.null(role[["supply"]])) {
      .irReject(paste0("table '", table, "' has no days_supply role"),
                field = paste0(base, "/end/fallback"))
    }
    "fallback"
  } else {
    .irReject("end is null, \"start\" or {fallback: \"days_supply\"}", field = paste0(base, "/end"))
  }
  visit <- node[["visit"]]
  visit_types <- NULL
  index_visit <- FALSE
  if (!is.null(visit)) {
    if (is.null(role[["visit_occurrence"]])) {
      .irReject(paste0("table '", table, "' has no visit link"), field = paste0(base, "/visit"))
    }
    if (isTRUE(visit[["index"]]) || isTRUE(unlist(visit[["index"]]))) {
      index_visit <- TRUE
    } else {
      visit_types <- tryCatch(.conceptIdList(visit[["type"]]),
                              error = function(e) .irReject(conditionMessage(e),
                                                            field = paste0(base, "/visit/type")))
      if (!length(visit_types)) .irReject("visit type needs concept ids", field = paste0(base, "/visit/type"))
      if (is.null(.roleOf(handle, "visit_occurrence"))) {
        .irReject("a visit-type restriction needs the visit_occurrence table",
                  code = "unavailable_table", field = paste0(base, "/visit"))
      }
    }
  }

  # Columns bound to roles are projected under their role names only.
  role_bound <- unique(c(role[["person"]], role[["event_key"]], role[["start"]],
                         role[["end"]], role[["supply"]], concept_col, value_col,
                         role[["unit"]], role[["operator"]], role[["range_low"]],
                         role[["range_high"]], role[["visit_occurrence"]],
                         role[["visit_detail"]]))
  others <- setdiff(filterable, c(role_bound, .identifierColumns(),
                                  .EVENT_SRC_COLUMNS, .EVENT_NORM_COLUMNS))
  other_types <- list()
  for (col in others) {
    t <- type_of(col)
    if (!is.null(t)) other_types[[col]] <- t
  }
  person_type <- type_of(role[["person"]]) %||% "integer"
  if (!person_type %in% c("integer", "bigint")) person_type <- "integer"
  list(name = name, table = table, role = role, concept = concept,
       concept_col = concept_col, value_col = value_col, unit = unit,
       end_policy = end_policy, visit_types = visit_types,
       index_visit = index_visit, where = node[["where"]],
       others = other_types, person_type = person_type,
       col_df = col_df, bp = bp)
}

# Validate and normalise a unit node: target, convert map (integer keys,
# finite factor > 0, finite offset), unknown and censored policies.
.eventUnitSpec <- function(unit, path) {
  target <- tryCatch(.conceptIdList(unit[["target"]]), error = function(e) integer(0))
  if (length(target) != 1L) .irReject("unit target is one concept id", field = paste0(path, "/target"))
  convert <- unit[["convert"]] %||% list()
  keys <- names(convert)
  if (length(convert) && (is.null(keys) || any(!grepl("^[0-9]+$", keys)))) {
    .irReject("unit convert keys are unit concept ids", field = paste0(path, "/convert"))
  }
  maps <- list()
  for (k in keys) {
    entry <- convert[[k]]
    factor <- entry[["factor"]]; offset <- entry[["offset"]] %||% 0
    if (is.list(factor)) factor <- factor[[1L]]
    if (is.list(offset)) offset <- offset[[1L]]
    if (!is.numeric(factor) || length(factor) != 1L || !is.finite(factor) || factor <= 0) {
      .irReject("a conversion factor must be finite and > 0", field = paste0(path, "/convert/", k, "/factor"))
    }
    if (!is.numeric(offset) || length(offset) != 1L || !is.finite(offset)) {
      .irReject("a conversion offset must be finite", field = paste0(path, "/convert/", k, "/offset"))
    }
    id <- as.integer(k)
    if (is.na(id) || identical(id, target)) {
      .irReject("a convert key must be a unit other than the target", field = paste0(path, "/convert/", k))
    }
    maps[[k]] <- list(unit = id, factor = as.numeric(factor), offset = as.numeric(offset))
  }
  unknown <- unit[["unknown"]] %||% "missing"
  censored <- unit[["censored"]] %||% "keep"
  if (!unknown %in% c("missing", "exclude")) .irReject("unit unknown is missing or exclude", field = paste0(path, "/unknown"))
  if (!censored %in% c("keep", "exclude")) .irReject("unit censored is keep or exclude", field = paste0(path, "/censored"))
  list(target = target, maps = unname(maps[order(as.integer(names(maps)))]),
       unknown = unknown, censored = censored)
}

# The affine map of one source column: CASE over the known units.
.eventAffine <- function(h, unit, column, unit_col) {
  target <- .sqlIntegerLiteral(h, unit$target)
  branches <- paste0("WHEN ", unit_col, " = ", target, " THEN ", column)
  for (m in unit$maps) {
    expr <- paste0(column, " * ", .predicateNumericLiteral(m$factor))
    if (m$offset != 0) expr <- paste0(expr, " + ", .predicateNumericLiteral(m$offset))
    branches <- c(branches, paste0("WHEN ", unit_col, " = ",
                                   .sqlIntegerLiteral(h, m$unit), " THEN (", expr, ")"))
  }
  paste0("CASE ", paste(branches, collapse = " "), " ELSE NULL END")
}

# The predicate catalogue of a table event: every non-identifier column of the
# final fragment, raw role names bound to their projections, and the roles
# the source lacks refused with a reason.
.eventCatalogue <- function(plan, available = TRUE) {
  role <- plan$role
  name <- plan$name
  roles <- .eventAvailableRoles(plan)
  columns <- list(
    start_date = list(type = "date"), end_date = list(type = "date"),
    end_recorded = list(type = "date"), supply = list(type = "integer"),
    concept_id = list(type = "integer", concept = TRUE),
    value_source = list(type = "numeric"), unit_source = list(type = "integer", concept = TRUE),
    operator_concept_id = list(type = "integer", concept = TRUE),
    range_low_source = list(type = "numeric"), range_high_source = list(type = "numeric"),
    value = list(type = "numeric"), unit_concept_id = list(type = "integer", concept = TRUE),
    range_low = list(type = "numeric"), range_high = list(type = "numeric"),
    unit_unknown = list(type = "integer"))
  for (col in names(plan$others)) {
    columns[[col]] <- list(type = plan$others[[col]], concept = grepl("_concept_id$", col))
  }
  aliases <- character(0)
  add_alias <- function(raw, to) if (!is.null(raw) && !raw %in% names(columns)) aliases[[raw]] <<- to
  if (!is.null(role)) {
    add_alias(role[["start"]], "start_date")
    add_alias(role[["end"]], "end_recorded")
    add_alias(role[["supply"]], "supply")
    add_alias(plan$concept_col, "concept_id")
    add_alias(plan$value_col, "value")
    add_alias(role[["unit"]], "unit_concept_id")
    add_alias(role[["operator"]], "operator_concept_id")
    add_alias(role[["range_low"]], "range_low")
    add_alias(role[["range_high"]], "range_high")
  }
  refused <- c(person_id = "a person identifier is never filterable",
               event_id = "row identifiers are never filterable",
               source_ns = "the source namespace is internal",
               visit_id = "row identifiers are never filterable")
  for (id in setdiff(.identifierColumns(), names(refused))) {
    refused[[id]] <- "row identifiers are never filterable"
  }
  lacks <- function(cols, why) for (c in cols) refused[[c]] <<- why
  if (!"value" %in% roles) lacks(c("value", "value_source", plan$value_col), "the source has no value role")
  if (!"unit" %in% roles) lacks(c("unit_concept_id", "unit_source"), "the source has no unit role")
  if (!"operator" %in% roles) lacks("operator_concept_id", "the source has no operator role")
  if (!"range" %in% roles) lacks(c("range_low", "range_high", "range_low_source", "range_high_source"),
                                 "the source has no reference-range role")
  if (!"supply" %in% roles) lacks("supply", "the source has no days_supply role")
  if (!"end_recorded" %in% roles) lacks("end_recorded", "the source has no recorded end role")
  if (!"concept" %in% roles) lacks("concept_id", "the source has no concept column")
  if (is.null(plan$unit)) lacks("unit_unknown", "unit_unknown exists only with a unit conversion")
  if (!is.null(plan$col_df)) {
    for (col in plan$col_df$column_name) {
      if (col %in% names(columns) || col %in% names(aliases) || col %in% names(refused)) next
      if (isTRUE(tryCatch(.assignmentBlockedColumn(col, plan$col_df), error = function(e) TRUE))) {
        refused[[col]] <- "a class-1 column is never released or filtered (design 7.6)"
      } else if (is.null(.eventColumnType(plan$col_df, col))) {
        refused[[col]] <- "datetime and time columns are not event predicate columns (use the date roles)"
      }
    }
  }
  .predicateCatalogue(columns, aliases, refused,
                      what = paste0("event '", name, "'"))
}

# Roles this event genuinely carries (absent roles are typed NULL columns).
.eventAvailableRoles <- function(plan) {
  role <- plan$role
  if (is.null(role)) return(character(0))
  c(if (!is.null(plan$value_col)) "value",
    if (!is.null(role[["unit"]])) "unit",
    if (!is.null(role[["operator"]])) "operator",
    if (!is.null(role[["range_low"]]) && !is.null(role[["range_high"]])) "range",
    if (!is.null(role[["supply"]])) "supply",
    if (!is.null(role[["end"]])) "end_recorded",
    if (!is.null(plan$concept_col)) "concept",
    if (!is.null(role[["visit_occurrence"]])) "visit")
}

# Emit the three stage fragments of a resolved table event.
.eventEmit <- function(ctx, plan) {
  h <- .compileDialect(ctx)
  handle <- ctx[["handle"]]
  role <- plan$role
  name <- plan$name
  table <- plan$table
  nm_src <- .fragmentName("ev", name, "src", ctx = ctx)
  nm_norm <- .fragmentName("ev", name, "norm", ctx = ctx)
  nm_final <- .fragmentName("ev", name, ctx = ctx)
  t <- function(col) paste0("t.", col)
  tnull <- function(type) .compileTypedNull(h, type)

  start <- t(role[["start"]])
  end_expr <- switch(plan$end_policy,
    start = start,
    role = if (!is.null(role[["end"]])) paste0("COALESCE(", t(role[["end"]]), ", ", start, ")") else start,
    fallback = {
      supply <- t(role[["supply"]])
      valid <- paste0(supply, " >= 1 AND ", .sqlIntegerCast(h, supply), " = ", supply)
      fb <- .sqlDateAddDays(h, start, paste0(supply, " - 1"))
      paste0("COALESCE(", if (!is.null(role[["end"]])) paste0(t(role[["end"]]), ", "),
             "CASE WHEN ", valid, " THEN ", fb, " END, ", start, ")")
    })
  src_cols <- c(
    person_id = t(role[["person"]]),
    event_id = .sqlLosslessKey(h, t(role[["event_key"]])),
    source_ns = .compileTextLiteral(name),
    start_date = start,
    end_date = end_expr,
    end_recorded = if (!is.null(role[["end"]])) t(role[["end"]]) else tnull("date"),
    supply = if (!is.null(role[["supply"]])) t(role[["supply"]]) else tnull("integer"),
    concept_id = if (!is.null(plan$concept_col)) t(plan$concept_col) else tnull("integer"),
    value_source = if (!is.null(plan$value_col)) t(plan$value_col) else tnull("numeric"),
    unit_source = if (!is.null(role[["unit"]])) t(role[["unit"]]) else tnull("integer"),
    operator_concept_id = if (!is.null(role[["operator"]])) t(role[["operator"]]) else tnull("integer"),
    range_low_source = if (!is.null(role[["range_low"]])) t(role[["range_low"]]) else tnull("numeric"),
    range_high_source = if (!is.null(role[["range_high"]])) t(role[["range_high"]]) else tnull("numeric"),
    visit_id = if (!is.null(role[["visit_occurrence"]])) .sqlLosslessKey(h, t(role[["visit_occurrence"]])) else tnull("bigint"))
  other_names <- names(plan$others)
  select_src <- c(paste0(src_cols, " AS ", names(src_cols)),
                  if (length(other_names)) paste0(t(other_names), " AS ", other_names))
  qualified <- .qualifyTable(handle, table, .resolveTableSchema(
    handle, table, plan$bp$tables$schema_category[plan$bp$tables$table_name == table][1L]))
  where <- character(0)
  if (!identical(plan$concept[["state"]], "all")) {
    where <- c(where, .conceptPredicateSql(t(plan$concept_col), plan$concept))
  }
  if (!is.null(plan$visit_types)) {
    vo <- .roleOf(handle, "visit_occurrence")
    vo_q <- .qualifyTable(handle, "visit_occurrence", .resolveTableSchema(
      handle, "visit_occurrence",
      plan$bp$tables$schema_category[plan$bp$tables$table_name == "visit_occurrence"][1L]))
    where <- c(where, paste0(
      "EXISTS (SELECT 1 FROM ", vo_q, " v WHERE v.", vo[["event_key"]], " = ",
      t(role[["visit_occurrence"]]), " AND ",
      .sqlIdInPredicate(paste0("v.", vo[["domain_concept"]]), plan$visit_types), ")"))
  }
  body_src <- paste0("SELECT ", paste(select_src, collapse = ", "), " FROM ", qualified, " t",
                     if (length(where)) paste0(" WHERE ", paste(where, collapse = " AND ")))

  spec <- .eventColumnSpec(plan$person_type)
  other_spec <- lapply(other_names, function(col) {
    .fragmentColumn(plan$others[[col]], TRUE, if (grepl("_concept_id$", col)) "concept" else "measure")
  })
  names(other_spec) <- other_names
  src_spec <- c(spec[.EVENT_SRC_COLUMNS], other_spec)
  lineage <- list(person_col = "person_id", via_roster = NULL)
  f_src <- .fragment(nm_src, "event", "row", body_src, src_spec,
                     unique_key = "event_id", person_lineage = lineage,
                     source_identity_columns = c("source_ns", "event_id"),
                     private_columns = .EVENT_PRIVATE_COLUMNS,
                     ordering = .EVENT_ORDERING)

  # Stage 2: normalisation.
  s <- function(col) paste0("s.", col)
  carry <- c(.EVENT_SRC_COLUMNS, other_names)
  unit <- plan$unit
  norm_where <- character(0)
  if (is.null(unit)) {
    norm_cols <- c(value = s("value_source"), unit_concept_id = s("unit_source"),
                   range_low = s("range_low_source"), range_high = s("range_high_source"),
                   unit_unknown = .sqlIntegerLiteral(h, 0L))
  } else {
    known <- c(unit$target, vapply(unit$maps, function(m) m$unit, integer(1)))
    known_sql <- .sqlIdInPredicate(s("unit_source"), known)
    norm_cols <- c(
      value = .eventAffine(h, unit, s("value_source"), s("unit_source")),
      unit_concept_id = paste0("CASE WHEN ", known_sql, " THEN ",
                               .sqlIntegerLiteral(h, unit$target), " ELSE NULL END"),
      range_low = .eventAffine(h, unit, s("range_low_source"), s("unit_source")),
      range_high = .eventAffine(h, unit, s("range_high_source"), s("unit_source")),
      unit_unknown = paste0("CASE WHEN ", known_sql, " THEN 0 ELSE 1 END"))
    if (identical(unit$unknown, "exclude")) norm_where <- c(norm_where, known_sql)
    if (identical(unit$censored, "exclude") && !is.null(role[["operator"]])) {
      norm_where <- c(norm_where, paste0(
        "(", s("operator_concept_id"), " IS NULL OR NOT (",
        .sqlIdInPredicate(s("operator_concept_id"), .operatorInequalityIds()), "))"))
    }
  }
  body_norm <- paste0("SELECT ", paste(c(paste0(s(carry), " AS ", carry),
                                         paste0(norm_cols, " AS ", names(norm_cols))),
                                       collapse = ", "),
                      " FROM ", nm_src, " s",
                      if (length(norm_where)) paste0(" WHERE ", paste(norm_where, collapse = " AND ")))
  norm_spec <- c(spec[.EVENT_SRC_COLUMNS], other_spec, spec[.EVENT_NORM_COLUMNS])
  f_norm <- .fragment(nm_norm, "event", "row", body_norm, norm_spec,
                      dependencies = nm_src, unique_key = "event_id",
                      person_lineage = lineage,
                      source_identity_columns = c("source_ns", "event_id"),
                      private_columns = .EVENT_PRIVATE_COLUMNS,
                      ordering = .EVENT_ORDERING)

  # Stage 3: the predicate over normalised columns.
  catalogue <- .eventCatalogue(plan)
  all_cols <- names(norm_spec)
  pred_sql <- if (!is.null(plan$where)) {
    .predicateSql(h, plan$where, catalogue, "n", paste0("/events/", name, "/where"))
  }
  body_final <- paste0("SELECT ", paste(paste0("n.", all_cols), collapse = ", "),
                       " FROM ", nm_norm, " n",
                       if (!is.null(pred_sql)) paste0(" WHERE ", pred_sql))
  f_final <- .fragment(nm_final, "event", "row", body_final, norm_spec,
                       dependencies = nm_norm, unique_key = "event_id",
                       person_lineage = lineage,
                       source_identity_columns = c("source_ns", "event_id"),
                       private_columns = .EVENT_PRIVATE_COLUMNS,
                       ordering = .EVENT_ORDERING)
  for (f in list(f_src, f_norm, f_final)) {
    .sqlAssertTypedSql(h, f$select_body)
    .fragmentRegister(ctx, f)
  }
  .eventMetaSet(ctx, name, list(
    name = name, kind = "table", table = table, availability = "available",
    roles = .eventAvailableRoles(plan), catalogue = catalogue,
    fragments = c(src = nm_src, norm = nm_norm, final = nm_final),
    index_visit = plan$index_visit, concept = plan$concept,
    concept_col = plan$concept_col, value_col = plan$value_col,
    unit = plan$unit, end_policy = plan$end_policy,
    role = .roleRecordSummary(role), columns = all_cols))
  f_final
}

# An unavailable source still has a declared type contract. Preserve the
# packaged CDM fields and role aliases without inspecting patient rows or
# guessing types from the requested name. Adapter columns use their own
# normalized contract; a cost's parent table is not the cost stream.
.eventUnavailableCatalogue <- function(ctx, name, table, kind, spec) {
  refused <- stats::setNames(rep("row identifiers are never filterable",
                                 length(.identifierColumns())), .identifierColumns())
  private <- c("person_id", "event_id", "source_ns", "visit_id", "parent_source_ns",
               "parent_event_id")
  for (col in private) refused[[col]] <- "source identities are never filterable"
  columns <- lapply(spec[setdiff(names(spec), names(refused))], function(col) {
    list(type = col$type, concept = identical(col$role, "concept"))
  })
  aliases <- character(0)
  if (identical(kind, "table")) {
    version <- .buildBlueprint(ctx[["handle"]])$spec_version
    metadata <- .loadVendoredSpec(version)
    if (is.null(metadata) || (!is.null(version) &&
        !identical(.normalizeCdmVersion(version), metadata$version))) {
      return(.predicateCatalogue(columns, aliases, refused, what = paste0("event '", name, "'")))
    }
    fields <- metadata$field_level
    fields <- fields[tolower(fields$cdmTableName) == table, , drop = FALSE]
    col_df <- data.frame(column_name = tolower(fields$cdmFieldName),
                         cdm_datatype = fields$cdmDatatype, stringsAsFactors = FALSE)
    roles <- .roleRecordsCsv(metadata$version)
    role <- roles[roles$table_name == table, , drop = FALSE]
    if (nrow(role) == 1L) {
      bindings <- c(start_col = "start_date", end_col = "end_recorded",
                    supply_col = "supply", domain_concept_col = "concept_id",
                    value_col = "value", unit_col = "unit_concept_id",
                    operator_col = "operator_concept_id", range_low_col = "range_low",
                    range_high_col = "range_high")
      node <- ctx[["ir"]][["events"]][[name]]
      if (!is.null(node[["concept_col"]])) role$domain_concept_col <- tolower(node[["concept_col"]])
      if (!is.null(node[["value"]][["column"]])) role$value_col <- tolower(node[["value"]][["column"]])
      for (key in names(bindings)) {
        raw <- role[[key]][[1L]]
        if (nzchar(raw) && raw %in% col_df$column_name && !raw %in% names(columns) &&
            !.assignmentBlockedColumn(raw, col_df)) {
          aliases[[raw]] <- bindings[[key]]
        }
      }
    }
    for (col in setdiff(col_df$column_name, c(names(columns), names(aliases), names(refused)))) {
      type <- .eventColumnType(col_df, col)
      if (.assignmentBlockedColumn(col, col_df)) {
        refused[[col]] <- "a class-1 column is never released or filtered (design 7.6)"
      } else if (is.null(type)) {
        refused[[col]] <- "datetime and time columns are not event predicate columns (use the date roles)"
      } else {
        columns[[col]] <- list(type = type, concept = grepl("_concept_id$", col))
      }
    }
  }
  .predicateCatalogue(columns, aliases, refused, what = paste0("event '", name, "'"))
}

# The zero-row fragment of an event source this site cannot serve under
# on_missing_table = "unavailable" (also used by the adapters). It keeps the
# source kind's declared columns, unique key, source identity, ordering and
# private columns (.eventKindContract(), amendment A-17); an adapter's
# parent_source_ns is fixed by its declaration.
.eventUnavailable <- function(ctx, name, table, kind = "table", concept = NULL,
                              parent = NULL, index_visit = FALSE) {
  h <- .compileDialect(ctx)
  contract <- .eventKindContract(kind)
  nm_final <- .fragmentName("ev", name, ctx = ctx)
  spec <- c(.eventColumnSpec("integer")[c(.EVENT_SRC_COLUMNS, .EVENT_NORM_COLUMNS)],
            contract$extra)
  catalogue <- .eventUnavailableCatalogue(ctx, name, table, kind, spec)
  for (col in setdiff(names(catalogue$columns), names(spec))) {
    spec[[col]] <- .fragmentColumn(catalogue$columns[[col]]$type, TRUE, "measure")
  }
  types <- vapply(spec, function(c) c$type, "")
  nulls <- vapply(names(spec), function(col) {
    if (identical(col, "source_ns")) return(.compileTextLiteral(name))
    if (identical(col, "parent_source_ns") && !is.null(parent)) {
      return(.compileTextLiteral(parent))
    }
    if (identical(col, "unit_unknown")) return(.sqlIntegerLiteral(h, 0L))
    .compileTypedNull(h, types[[col]])
  }, "")
  body <- paste0("SELECT ", paste(paste0(nulls, " AS ", names(spec)), collapse = ", "),
                 .compileNoTableFrom(h), " WHERE 1 = 0")
  f <- .fragment(nm_final, "event", "row", body, spec, unique_key = contract$unique_key,
                 person_lineage = list(person_col = "person_id", via_roster = NULL),
                 source_identity_columns = contract$identity,
                 private_columns = contract$private, availability = "unavailable",
                 ordering = contract$ordering)
  .sqlAssertTypedSql(h, f$select_body)
  .fragmentRegister(ctx, f)
  .eventMetaSet(ctx, name, list(
    name = name, kind = kind, table = table, availability = "unavailable",
    roles = character(0), catalogue = catalogue, fragments = c(final = nm_final),
    index_visit = isTRUE(index_visit), concept = concept, parent = parent, columns = names(spec)))
  f
}

# --- Site checks without compilation (for .irValidate, design 6.1) ------------

#' Predicate catalogue of an event source, from metadata only
#'
#' The catalogue the compilers use for the event's own \code{where} and for
#' reducer predicates over its rows, derived from the role registry and the
#' blueprint without resolving concept sets or reading data.
#'
#' @param handle CDM handle.
#' @param ir The IR.
#' @param name Event name.
#' @return A \code{.predicateCatalogue()}, or NULL when the source is not
#'   available at this site (the caller applies \code{on_missing_table}).
#' @keywords internal
.eventSiteCatalogue <- function(handle, ir, name, .stack = character(0)) {
  if (name %in% .stack) {
    .irReject(paste0("event reference cycle: ", paste(c(.stack, name), collapse = " -> ")),
              field = paste0("/events/", name))
  }
  node <- .irNamedNode(ir, "event", name)
  if (!is.null(node[["episode_members"]])) {
    spec <- node[["episode_members"]]
    pnode <- .irNode(ir, "event", spec[["episode"]][["event"]])
    if (!identical(tolower(pnode[["table"]] %||% ""), "episode")) {
      .irReject("episode_members needs an event over the episode table",
                field = paste0("/events/", name, "/episode_members/episode"))
    }
    fields <- .irEpisodeEventFields()
    tables <- tolower(as.character(unlist(spec[["tables"]] %||% fields$table_name)))
    unknown <- setdiff(tables, fields$table_name)
    if (length(unknown)) {
      .irReject(paste0("'", unknown[[1L]], "' has no reviewed episode_event field mapping"),
                field = paste0("/events/", name, "/episode_members/tables"))
    }
    records <- Filter(Negate(is.null), lapply(tables, function(t) .roleOf(handle, t)))
    if (!length(records)) return(NULL)
    return(.episodeMemberCatalogue(name, .episodeMemberRoles(records)))
  }
  if (!is.null(node[["cost"]])) {
    parent <- node[["cost"]][["event"]][["event"]]
    pnode <- .irNamedNode(ir, "event", parent)
    if (is.null(pnode[["table"]])) {
      .irReject("the cost source must reference a table event",
                field = paste0("/events/", name, "/cost/event"))
    }
    prole <- .roleOf(handle, pnode[["table"]])
    if (is.null(prole)) return(NULL)
    roles <- c(if (!is.null(prole[["end"]])) "end_recorded",
               if (!is.null(prole[["visit_occurrence"]])) "visit")
    return(.costCatalogue(name, .costRoles(roles)))
  }
  role <- .roleOf(handle, node[["table"]])
  if (is.null(role)) return(NULL)
  ctx <- new.env(parent = emptyenv())
  ctx$handle <- handle
  ctx$ir <- ir
  ctx$dialect <- .sqlDialectProfile(handle, probe = FALSE)
  concept <- if (is.null(node[["concept_set"]])) {
    list(ids = NULL, state = "all")
  } else list(ids = integer(0), state = "set")
  .eventCatalogue(.eventPlan(ctx, node, name, role, concept))
}

#' Handle-dependent findings of one event source, without compiling it
#'
#' The same rules the event and adapter compilers apply, from metadata only:
#' concept columns with a concept role, numeric value columns, unit, operator,
#' supply and visit roles, and the \code{where} predicate typed against the
#' normalised columns (design 6.1; WS-2's site checks may call it so the plan
#' is refused before any query with the compiler's own rules).
#'
#' @param handle CDM handle.
#' @param ir The IR.
#' @param name Event name.
#' @param predicates Optional named list path -> extra predicate over the
#'   event's rows (e.g. a reducer's \code{predicate}).
#' @return Character vector of findings ("<path>: <reason>"), empty when valid.
#' @keywords internal
.eventSiteCheck <- function(handle, ir, name, predicates = list()) {
  finding <- function(e, default) {
    paste0(e$field %||% default, ": ", sub("^[a-z_]+: ", "", conditionMessage(e)))
  }
  base <- paste0("/events/", name)
  catalogue <- tryCatch(.eventSiteCatalogue(handle, ir, name),
                        dsomop_plan_rejection = function(e) finding(e, base))
  if (is.character(catalogue)) return(catalogue)
  if (is.null(catalogue)) return(character(0))
  node <- .irNode(ir, "event", name)
  where <- node[["where"]] %||% node[["episode_members"]][["where"]] %||% node[["cost"]][["where"]]
  checks <- c(if (!is.null(where)) list(list(pred = where, path = paste0(base, "/where"))),
              lapply(names(predicates), function(p) list(pred = predicates[[p]], path = p)))
  h <- .sqlDialectProfile(handle, probe = FALSE)
  out <- character(0)
  for (chk in checks) {
    msg <- tryCatch({
      .predicateSql(h, chk$pred, catalogue, "x", chk$path)
      NULL
    }, dsomop_plan_rejection = function(e) finding(e, chk$path))
    out <- c(out, msg)
  }
  out
}

#' Compile the event sources of a set of outputs (the plan's source stage)
#'
#' Design 6.1 orders a plan's evaluation concept sets, events (adapters
#' included), intervals, populations, variables, outputs. This is the event
#' stage: every event the outputs read (their execution dependency closures),
#' with its concept set and adapter parents, compiled in the plan order inside the
#' private compilation context. The data-dependent adapter refusals of
#' section 11 (\code{adapter_person_mismatch}, \code{cost_currency_mixed},
#' amendment A-18) surface here, before any output is materialised.
#' Dictionary source outputs remain static concept dependencies and do not
#' cause clinical events, adapters or populations to be materialised.
#'
#' @param cctx Compilation context (\code{.compileContext()}).
#' @param outputs Names of the outputs to compute.
#' @return The names of the compiled events, invisibly.
#' @keywords internal
.planCompileSources <- function(cctx, outputs) {
  ir <- cctx[["ir"]]
  outputs <- .irReach(outputs, function(o) .outputExecutionDeps(ir, o), names(ir[["outputs"]]))
  outputs <- outputs[!vapply(ir[["outputs"]][outputs], function(o) {
    identical(o[["format"]], "dictionary")
  }, logical(1))]
  execution_ir <- ir
  execution_ir[["outputs"]] <- ir[["outputs"]][outputs]
  events <- unique(unlist(lapply(outputs, function(o) {
    names(.irOutputClosure(execution_ir, o)[["events"]] %||% list())
  }), use.names = FALSE))
  events <- intersect(.irOrder(ir)[["events"]], events)
  for (e in events) .compileEventByName(cctx, e)
  invisible(events)
}
