# Module: Intervals and eras (design-2.10.3 sections 2.6 and 6.7; amendment A-20)
#
# An interval node (`omop_era()`) turns the rows of one event source into
# three relations, each a typed fragment:
#
#   iv_<name>_rec   RECORDED intervals: one closed day interval per source row,
#                   normalised by the ERA's own end/fallback from the event's
#                   ORIGINAL roles (end_recorded, supply) -- never from the
#                   event's already coalesced end_date: end = "start" makes
#                   point records; otherwise the recorded end, then
#                   start + supply - 1 only under fallback = "days_supply" with
#                   a positive integer supply, then the start. Rows failing the
#                   one private data-quality policy (NULL start, end before
#                   start, a non-positive / non-integer supply when the supply
#                   is the only end source) contribute nothing anywhere. Each
#                   row carries its group and the era_no of its course.
#   iv_<name>       the COURSE: gaps and islands per (person, group) with a
#                   running maximum of prior ends (the 1-PRECEDING window
#                   frame): a record starts a new era when start >
#                   previous_max_end + gap + 1, previous ends extended by
#                   `persistence` (course construction only). One row per era
#                   with the event column contract (start_date, end_date,
#                   concept_id = the group concept or NULL, ...), so every
#                   event consumer accepts it; identity (source_ns, person_id,
#                   group_id, era_no) and the lossless index key
#                   era_key = "<group_id>:<era_no>" (never era_no alone, which
#                   restarts per group).
#   iv_<name>_eff   EFFECTIVE SUPPLY (stockpile = TRUE only): a bounded
#                   deterministic R iterator per (person, group) over the
#                   person's whole recorded history sorted by (start_date, row
#                   key) carries unused supply forward:
#                     supply_i  = positive integer supply, else the recorded
#                                 length end_recorded - start + 1 when valid,
#                                 else one day;
#                     carry_i   = min(cap, max(0, eff_end_{i-1} - start_i + 1)),
#                                 eff_end_{i-1} the previous row's effective
#                                 end (never a running maximum: stock the cap
#                                 discards does not carry into later fills);
#                     eff_end_i = start_i + supply_i - 1 + carry_i,
#                   cap = getOption("dsomop.max_stockpile_days", 90). The rows
#                   are materialised as a session temporary table and feed the
#                   gaps-and-islands step of the stockpiled course; the recorded
#                   relation is kept unchanged (coverage reducers select one of
#                   the two explicitly).
#
# Three ends, named once (A-20 item 3): the ORIGINAL end_recorded (the source
# role, possibly overridden by the era's end/fallback), the NORMALISED
# RECORDED END (end_date of iv_<name>_rec: the era's own end/fallback applied)
# and the course end. The course's end_date is MAX(effective end +
# persistence) for a stockpiled source and MAX(normalised recorded end +
# persistence) otherwise; its end_recorded column is MAX(normalised recorded
# end), never an effective end; the unextended course end is end_date -
# persistence (not a column).
#
# `min_days` keeps the eras whose normalised recorded span MIN(start_date) ..
# MAX(normalised recorded end) has at least min_days days, persistence and
# stockpile carry excluded. A stockpiled source carries the normalised
# recorded end as its own column (norm_end) through the effective rows and the
# marks. The retained flag is a window aggregate over the marks (stage ek,
# era_kept), and the course, recorded and effective relations filter on it:
# no relation re-reads the course. On MySQL the effective-source CTE is also
# non-mergeable (DISTINCT on its unique rows), so a combined downstream
# statement reuses one materialisation rather than reopening the TEMPORARY
# table through separate consumer branches (A-20 (l)). The rows of a dropped
# era leave the recorded and effective relations as well.
#
# Over an adapter source (episode members, costs; design 11) an interval is
# compiled per consumer association (A-20 item 6), never once for all: the
# unreduced adapter relation keeps one row per episode-member link.
#   person   (person-indexed consumers) the physical events are deduplicated
#            by (person, source_ns, member table, event_id) before era
#            normalisation, so a fill linked to two episodes counts once;
#   parent   (episode-indexed consumers; identities iv_<name>_par[_<stage>])
#            one row per link with its parent provenance (parent_source_ns,
#            parent_event_id); eras, stockpile and carry-in per (person, group,
#            parent), so each episode's eras use only its own members; courses
#            keyed per parent (era_key "<parent_event_id>|<group_id>:<era_no>")
#            and never shared with another association.
# An event restricted to the index visit (visit = {index: true}) is normalised
# and stockpiled per visit; consumers bind that visit to index_visit_id.
#
# Grouping (`by`): "event" = one group (group_id 1); "concept" = the scoping
# concept; "ingredient" = RxNorm ingredient ancestors through
# .compileIngredientMap() (a drug concept with two ingredients contributes to
# both), unmapped concepts kept as their own group or dropped (`unmapped`).
# A row without a concept cannot be grouped by concept or ingredient and is
# not part of such an era (it still counts for by = "event").
#
# The reducer kernels at the end of the module (covered_days, supply_days,
# observed days) are pure R over fetched streams: covered_days takes the
# union length of the selected relation clipped to the window, optionally
# intersected with the episode's actually observed segments (observed_only,
# the PDC numerator); supply_days sums days_supply of fills starting in the
# window or the per-fill supplied days inside it (overlaps not unioned).
# .intervalReducerValues() is their executable base: consumer-allocated
# fragment names (A-20 (e)), the consumer association of
# .consumerAssociation() (index visit, episode parent; B3), the one coverage
# quality predicate at direct-event consumers (g), NA for an unavailable source
# (i) and bounded private fetches (j). Compiler session tables are released by
# .compileReleaseTempTables(), which the execution context's closure calls on
# every exit path (k).

.INTERVAL_FIELDS <- c("source", "gap", "persistence", "min_days", "by",
                      "unmapped", "end", "fallback", "stockpile")
.INTERVAL_PRIVATE_COLUMNS <- c("event_id", "source_ns", "group_id", "era_no")
# The last date a materialised effective end may take (the supported range of
# every dialect's DATE).
.INTERVAL_MAX_DATE <- as.Date("9999-12-31")

# Event-contract columns of a course row that eras do not carry (typed NULLs).
.INTERVAL_COURSE_NULLS <- c(supply = "integer", value_source = "numeric",
                            unit_source = "integer",
                            operator_concept_id = "integer",
                            range_low_source = "numeric",
                            range_high_source = "numeric", visit_id = "bigint",
                            value = "numeric", unit_concept_id = "integer",
                            range_low = "numeric", range_high = "numeric")

# --- node validation ---------------------------------------------------------------

.intervalInt <- function(x, field, path, minimum = 0L) {
  if (is.list(x) && length(x) == 1L && is.null(names(x))) x <- x[[1L]]
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    x == trunc(x) && x >= minimum && abs(x) <= .Machine$integer.max
  if (!ok) {
    .irReject(paste0(field, " must be an integer >= ", minimum),
              field = paste0(path, "/", field))
  }
  as.integer(x)
}

.intervalEnum <- function(x, allowed, field, path) {
  if (is.list(x) && length(x) == 1L && is.null(names(x))) x <- x[[1L]]
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% allowed) {
    .irReject(paste0(field, " must be one of ", paste(allowed, collapse = ", ")),
              field = paste0(path, "/", field))
  }
  x
}

#' Validated settings of one interval node (defaults applied)
#'
#' @param node IR interval node.
#' @param name Interval name.
#' @return list(name, source, gap, persistence, min_days, by, unmapped, end,
#'   fallback, stockpile).
#' @keywords internal
.intervalSpec <- function(node, name) {
  path <- paste0("/intervals/", name)
  if (!is.list(node) || is.null(names(node))) {
    .irReject("an interval is an object", field = path)
  }
  unknown <- setdiff(names(node), .INTERVAL_FIELDS)
  if (length(unknown)) {
    .irReject(paste0("unknown interval field(s): ", paste(unknown, collapse = ", ")),
              field = path)
  }
  src <- node[["source"]]
  source <- if (is.list(src) && identical(names(src), "event")) src[["event"]]
  if (is.list(source)) source <- unlist(source)
  if (!.isScalarString(source) || !grepl(.NAME_PATTERN, source)) {
    .irReject("an interval source is one event reference {event: <name>}",
              field = paste0(path, "/source"))
  }
  stock <- node[["stockpile"]] %||% FALSE
  if (is.list(stock)) stock <- unlist(stock)
  if (!is.logical(stock) || length(stock) != 1L || is.na(stock)) {
    .irReject("stockpile must be TRUE or FALSE", field = paste0(path, "/stockpile"))
  }
  list(
    name = name, source = source,
    gap = .intervalInt(node[["gap"]] %||% 30L, "gap", path),
    persistence = .intervalInt(node[["persistence"]] %||% 0L, "persistence", path),
    min_days = .intervalInt(node[["min_days"]] %||% 0L, "min_days", path),
    by = .intervalEnum(node[["by"]] %||% "event", c("event", "concept", "ingredient"), "by", path),
    unmapped = .intervalEnum(node[["unmapped"]] %||% "own_concept", c("own_concept", "drop"),
                             "unmapped", path),
    end = .intervalEnum(node[["end"]] %||% "recorded_end", c("recorded_end", "start"), "end", path),
    fallback = .intervalEnum(node[["fallback"]] %||% "days_supply", c("days_supply", "none"),
                             "fallback", path),
    stockpile = stock)
}

#' Server cap on carried stockpile days (design section 6.7)
#'
#' \code{getOption("dsomop.max_stockpile_days", 90)}: one integer between 0
#' and \code{.Machine$integer.max}, validated before any coercion (a larger
#' value is refused, never turned into NA).
#' @keywords internal
.intervalStockpileCap <- function() {
  value <- suppressWarnings(as.numeric(getOption("dsomop.max_stockpile_days", 90L)))
  if (length(value) != 1L || is.na(value) || !is.finite(value) ||
      value != floor(value) || value < 0 || value > .Machine$integer.max) {
    stop("dsomop.max_stockpile_days must be one integer between 0 and ",
         .Machine$integer.max, ".", call. = FALSE)
  }
  as.integer(value)
}

# --- metadata ------------------------------------------------------------------------

.INTERVAL_ASSOCIATIONS <- c("person", "parent")

.intervalMetaKey <- function(name, association = "person") {
  if (identical(association, "parent")) paste0(name, "\rparent") else name
}

.intervalMetaSet <- function(ctx, name, meta, association = "person") {
  memo <- .compileMemo(ctx, "interval_meta")
  assign(.intervalMetaKey(name, association), meta, envir = memo)
  invisible(meta)
}

#' Metadata of a compiled interval
#'
#' @param ctx Execution context.
#' @param name Interval name.
#' @param association \code{"person"} (default) or \code{"parent"} (the
#'   per-parent relation of an adapter source, A-20 item 6).
#' @return list(name, spec, source, association, availability, fragments =
#'   c(norm, marks[, kept], course, recorded[, effective, effective_source]),
#'   temp_table), or NULL when not compiled.
#' @keywords internal
.intervalMeta <- function(ctx, name, association = "person") {
  memo <- if (is.environment(ctx) && is.environment(ctx[["interval_meta"]])) ctx[["interval_meta"]]
  key <- .intervalMetaKey(name, association)
  if (is.null(memo) || !exists(key, envir = memo, inherits = FALSE)) return(NULL)
  get(key, envir = memo, inherits = FALSE)
}

.intervalResult <- function(ctx, meta) {
  fr <- meta$fragments
  list(course = .fragmentGet(ctx, fr[["course"]]),
       recorded = .fragmentGet(ctx, fr[["recorded"]]),
       effective = if (!is.na(fr["effective"])) .fragmentGet(ctx, fr[["effective"]]))
}

#' Identities of the interval fragments of a plan (amendment A-17)
#'
#' Per interval: the stages \code{norm}, \code{mk} (gaps-and-islands marks),
#' \code{ek} (marks with the retained flag of \code{min_days}), \code{rec}
#' (recorded relation), for a stockpiled interval \code{effsrc} (the
#' materialised effective rows) and \code{eff} (effective relation), and the
#' final fragment (the course). An interval over an adapter source also
#' declares its per-parent relation (A-20 item 6): the same stages prefixed
#' \code{par_} and the course \code{("iv", <name>, "par")}.
#' @keywords internal
.intervalFragmentIdentities <- function(ir) {
  out <- list()
  for (i in names(ir[["intervals"]] %||% list())) {
    node <- ir[["intervals"]][[i]]
    stock <- isTRUE(unlist(node[["stockpile"]]))
    src <- unlist(node[["source"]][["event"]])
    ev <- if (.isScalarString(src)) ir[["events"]][[src]]
    adapter <- is.list(ev) && (!is.null(ev[["episode_members"]]) || !is.null(ev[["cost"]]))
    stages <- c("norm", if (stock) "effsrc", "mk", "ek", "rec", if (stock) "eff")
    for (variant in c("person", if (adapter) "parent")) {
      par <- identical(variant, "parent")
      for (stage in stages) {
        out[[length(out) + 1L]] <- .fragmentIdentity("iv", i, if (par) paste0("par_", stage) else stage)
      }
      out[[length(out) + 1L]] <- .fragmentIdentity("iv", i, if (par) "par")
    }
  }
  out
}

#' Compile an interval by name, with its source event
#'
#' Resolves the node through \code{ctx$ir}, compiles its source event
#' (\code{.compileEventByName()}) and dispatches to \code{.compileInterval()}.
#' Memoised per execution context and association.
#'
#' @param ctx Execution context.
#' @param name Interval name.
#' @param association \code{"person"} or \code{"parent"} (adapter sources).
#' @return \code{list(course, recorded, effective)} (see
#'   \code{.compileInterval()}).
#' @keywords internal
.compileIntervalByName <- function(ctx, name, association = "person") {
  meta <- .intervalMeta(ctx, name, association)
  if (!is.null(meta)) return(.intervalResult(ctx, meta))
  node <- .irNamedNode(ctx[["ir"]], "interval", name)
  spec <- .intervalSpec(node, name)
  ef <- .compileEventByName(ctx, spec$source)
  .compileInterval(ctx, node, ef, association = association)
}

# --- SQL builders -----------------------------------------------------------------------

# Extra row-identity columns of an event source beyond event_id. The physical
# identity of an adapter row is its member table and event key; the person
# relation of an interval keeps that identity only (its physical events are
# deduplicated), the per-parent relation keeps the parent provenance too
# (A-20 item 6).
.intervalExtraKeys <- function(event_fragment, association = "person") {
  base <- setdiff(unique(c(event_fragment$unique_key, event_fragment$ordering)),
                  c("person_id", "event_id", "source_ns", "start_date", "end_date",
                    "end_recorded", "supply", "concept_id", .ADAPTER_PARENT_COLUMNS))
  if (identical(association, "parent")) c(base, .ADAPTER_PARENT_COLUMNS) else base
}

# The era's recorded end, from the ORIGINAL roles (design 6.7).
.intervalEndSql <- function(h, spec, e = "e") {
  start <- paste0(e, ".start_date")
  if (identical(spec$end, "start")) return(start)
  if (identical(spec$fallback, "days_supply")) {
    supply <- paste0(e, ".supply")
    valid <- paste0(supply, " >= 1 AND ", .sqlIntegerCast(h, supply), " = ", supply)
    return(paste0("COALESCE(", e, ".end_recorded, CASE WHEN ", valid, " THEN ",
                  .sqlDateAddDays(h, start, paste0(supply, " - 1")), " END, ", start, ")"))
  }
  paste0("COALESCE(", e, ".end_recorded, ", start, ")")
}

# The data-quality predicate of the recorded relation (one policy, every
# consumer): start present, end not before start, and -- when the supply is
# the only end source -- a positive integer supply.
.intervalQualitySql <- function(h, spec, end_sql, e = "e") {
  start <- paste0(e, ".start_date")
  out <- paste0(start, " IS NOT NULL")
  if (!identical(spec$end, "start")) out <- c(out, paste0(end_sql, " >= ", start))
  if (identical(spec$end, "recorded_end") && identical(spec$fallback, "days_supply")) {
    supply <- paste0(e, ".supply")
    out <- c(out, paste0("(", e, ".end_recorded IS NOT NULL OR ", supply, " IS NULL OR (",
                         supply, " >= 1 AND ", .sqlIntegerCast(h, supply), " = ", supply, "))"))
  }
  out
}

#' Gaps-and-islands marks over a row relation (shared by eras and coverage)
#'
#' Adds \code{prev_max_end} (running maximum of the previous rows' ends,
#' extended by \code{persistence}, through the 1-PRECEDING frame),
#' \code{new_era} (1 when \code{start > prev_max_end + gap + 1}) and the
#' cumulative \code{era_no} per partition, in the given deterministic order.
#'
#' @param h Handle, context, profile or dialect name.
#' @param from Name of the source relation (fragment).
#' @param columns Columns of the source relation to carry, in order.
#' @param partition Partition columns.
#' @param order Order columns after \code{start_date} (must make the order
#'   total within a partition).
#' @param gap Whole uncovered days tolerated.
#' @param persistence Days added to previous ends.
#' @return One SELECT body.
#' @keywords internal
.intervalMarksSql <- function(h, from, columns, partition, order, gap, persistence) {
  n <- function(a, cols) paste0(a, ".", cols)
  end <- if (persistence > 0L) .sqlDateAddDays(h, "n.end_date", persistence) else "n.end_date"
  prev <- .sqlWindow(h, paste0("MAX(", end, ")"), partition = n("n", partition),
                     order = n("n", c("start_date", order)), frame = "preceding")
  inner <- paste0("SELECT ", paste(n("n", columns), collapse = ", "), ", ", prev,
                  " AS prev_max_end FROM ", from, " n")
  # Use a difference for extreme gaps: adding one to the largest accepted
  # integer overflows R, and adding that many days can overflow SQL DATE.
  separated <- if (gap == .Machine$integer.max) {
    paste0(.sqlDateDiffDays(h, "y.prev_max_end", "y.start_date"), " > ",
           .sqlIntegerLiteral(h, as.numeric(gap) + 1))
  } else paste0("y.start_date > ", .sqlDateAddDays(h, "y.prev_max_end", gap + 1L))
  flag <- paste0("CASE WHEN y.prev_max_end IS NULL OR ", separated, " THEN 1 ELSE 0 END")
  middle <- paste0("SELECT ", paste(n("y", c(columns, "prev_max_end")), collapse = ", "),
                   ", ", flag, " AS new_era FROM (", inner, ") y")
  era <- .sqlIntegerCast(h, .sqlWindow(h, "SUM(x.new_era)", partition = n("x", partition),
                                       order = n("x", c("start_date", order)),
                                       frame = "cumulative"))
  paste0("SELECT ", paste(n("x", c(columns, "prev_max_end", "new_era")), collapse = ", "),
         ", ", era, " AS era_no FROM (", middle, ") x")
}

#' The retained flag of min_days over the marks (A-20 item 3)
#'
#' One pass of window aggregates per era (\code{PARTITION BY} the marks'
#' partition and \code{era_no}): \code{era_kept} is 1 when the era's span
#' \code{MIN(start_date) .. MAX(<span>)} has at least \code{min_days} days.
#' \code{span} is the normalised recorded end (\code{end_date} of an ordinary
#' source, \code{norm_end} of a stockpiled one): persistence and stockpile
#' carry never lengthen it.
#'
#' @param h Handle, context, profile or dialect name.
#' @param from Name of the marks relation.
#' @param columns Columns of the marks relation to carry, in order.
#' @param partition Partition columns of the marks.
#' @param span Column holding the normalised recorded end.
#' @param min_days Minimum era length in days.
#' @return One SELECT body.
#' @keywords internal
.intervalKeptSql <- function(h, from, columns, partition, span, min_days) {
  over <- paste0("m.", c(partition, "era_no"))
  inner <- paste0("SELECT ", paste(paste0("m.", columns), collapse = ", "), ", ",
                  .sqlWindow(h, "MIN(m.start_date)", partition = over), " AS span_start, ",
                  .sqlWindow(h, paste0("MAX(m.", span, ")"), partition = over), " AS span_end FROM ",
                  from, " m")
  flag <- paste0("CASE WHEN ", .sqlDateDiffDays(h, "z.span_start", "z.span_end"), " + 1 >= ",
                 .sqlIntegerLiteral(h, min_days), " THEN 1 ELSE 0 END")
  paste0("SELECT ", paste(paste0("z.", columns), collapse = ", "), ", ", flag,
         " AS era_kept FROM (", inner, ") z")
}

# Column specs -----------------------------------------------------------------------

.intervalRowSpec <- function(event_fragment, extras) {
  ec <- event_fragment$columns
  person <- ec[["person_id"]]
  spec <- list(
    person_id = .fragmentColumn(person$type, FALSE, "person_key"),
    event_id = .fragmentColumn("bigint", FALSE, "origin_identity"),
    source_ns = .fragmentColumn("text", FALSE, "origin_identity"),
    group_id = .fragmentColumn("integer", FALSE, "group"),
    concept_id = .fragmentColumn("integer", TRUE, "concept"),
    start_date = .fragmentColumn("date", FALSE, "start"),
    end_date = .fragmentColumn("date", FALSE, "end"),
    end_recorded = .fragmentColumn("date", TRUE, "end_recorded"),
    supply = .fragmentColumn("integer", TRUE, "supply"))
  for (x in extras) {
    col <- ec[[x]]
    spec[[x]] <- .fragmentColumn(col$type, col$nullable, col$role)
  }
  spec
}

.intervalMarkSpec <- function(row_spec) {
  c(row_spec, list(
    prev_max_end = .fragmentColumn("date", TRUE, "private"),
    new_era = .fragmentColumn("integer", FALSE, "private"),
    era_no = .fragmentColumn("integer", FALSE, "era_no")))
}

.intervalCourseSpec <- function(person_type, per_parent = FALSE) {
  out <- list(
    person_id = .fragmentColumn(person_type, FALSE, "person_key"),
    event_id = .fragmentColumn("bigint", FALSE, "origin_identity"),
    source_ns = .fragmentColumn("text", FALSE, "origin_identity"),
    start_date = .fragmentColumn("date", FALSE, "start"),
    end_date = .fragmentColumn("date", FALSE, "end"),
    end_recorded = .fragmentColumn("date", FALSE, "end_recorded"))
  roles <- c(supply = "supply", value_source = "value", unit_source = "unit",
             operator_concept_id = "operator", range_low_source = "range",
             range_high_source = "range", visit_id = "visit", value = "value",
             unit_concept_id = "unit", range_low = "range", range_high = "range")
  out$supply <- .fragmentColumn("integer", TRUE, "supply")
  out$concept_id <- .fragmentColumn("integer", TRUE, "concept")
  for (col in setdiff(names(.INTERVAL_COURSE_NULLS), "supply")) {
    out[[col]] <- .fragmentColumn(.INTERVAL_COURSE_NULLS[[col]], TRUE, roles[[col]])
  }
  out <- c(out, list(
    unit_unknown = .fragmentColumn("integer", FALSE, "measure"),
    group_id = .fragmentColumn("integer", FALSE, "group"),
    era_no = .fragmentColumn("integer", FALSE, "era_no"),
    n_events = .fragmentColumn("integer", FALSE, "private"),
    era_key = .fragmentColumn("text", FALSE, "origin_identity")))
  if (per_parent) out <- c(out, .adapterParentSpec())
  out
}

# --- the compiler ---------------------------------------------------------------------

#' Compile an interval (frozen compiler signature, section 6)
#'
#' Pipeline: \code{norm} (normalised recorded rows) -> [\code{effsrc}: the
#' stockpiled effective rows, a session table] -> \code{mk} (gaps and islands)
#' -> [\code{ek}: the retained flag of \code{min_days}] -> the course, the
#' recorded relation and the effective relation, each reading the one marks
#' path once.
#'
#' @param ctx Execution context (environment with \code{handle}, \code{ir},
#'   \code{dialect}).
#' @param node The IR interval node with its plan name in \code{attr(node,
#'   "name")}.
#' @param event_fragment The compiled source event (\code{ev_<source>}).
#' @param association \code{"person"} (default: person-indexed consumers) or
#'   \code{"parent"} (episode-indexed consumers of an adapter source: one
#'   relation per parent, A-20 item 6). An optional argument after the frozen
#'   ones; the default keeps the frozen behaviour.
#' @return \code{list(course, recorded, effective)}: the course fragment
#'   \code{iv_<name>} (one row per era, event column contract), the recorded
#'   relation \code{iv_<name>_rec} (one row per normalised source row with its
#'   group and era_no) and, only for \code{stockpile = TRUE}, the effective
#'   supply relation \code{iv_<name>_eff} (NULL otherwise). Intermediate
#'   fragments (\code{iv_<name>_norm}, \code{iv_<name>_mk}, ...) are registered
#'   in the context; \code{.intervalMeta(ctx, name, association)} describes
#'   them.
#' @keywords internal
.compileInterval <- function(ctx, node, event_fragment, association = c("person", "parent")) {
  association <- match.arg(association)
  name <- .compileNodeName(node, "interval")
  existing <- .intervalMeta(ctx, name, association)
  if (!is.null(existing)) return(.intervalResult(ctx, existing))
  spec <- .intervalSpec(node, name)
  .assertFragment(event_fragment)
  if (!identical(event_fragment$kind, "event")) {
    stop("An interval compiles over an event fragment.", call. = FALSE)
  }
  smeta <- .eventMeta(ctx, spec$source)
  if (!is.null(smeta) && !identical(smeta$fragments[["final"]], event_fragment$name)) {
    stop("The interval's source event fragment does not match its event node.", call. = FALSE)
  }
  adapter <- all(.ADAPTER_PARENT_COLUMNS %in% names(event_fragment$columns))
  per_parent <- identical(association, "parent")
  per_visit <- isTRUE(smeta$index_visit)
  if (per_parent && !adapter) {
    stop("A per-parent interval needs an adapter source (episode members, cost).", call. = FALSE)
  }
  h <- .compileDialect(ctx)
  availability <- event_fragment$availability
  extras <- .intervalExtraKeys(event_fragment, association)
  if (per_visit) extras <- unique(c(extras, "visit_id"))
  order_keys <- c("event_id", extras)
  partition <- c("person_id", "group_id", if (per_parent) .ADAPTER_PARENT_COLUMNS,
                 if (per_visit) "visit_id")
  stockpile <- isTRUE(spec$stockpile)
  ev <- event_fragment$name
  nm <- function(suffix = NULL) {
    stage <- if (per_parent) paste(c("par", suffix), collapse = "_") else suffix
    .fragmentName("iv", name, stage, ctx = ctx)
  }
  lineage <- list(person_col = "person_id", via_roster = NULL)
  register <- function(f) {
    .sqlAssertTypedSql(h, f$select_body)
    .fragmentRegister(ctx, f)
  }

  # 1. normalised recorded rows (data-quality policy applied) -----------------
  end_sql <- .intervalEndSql(h, spec, "e")
  deps <- ev
  join <- ""
  group_sql <- switch(spec$by,
    event = .sqlIntegerLiteral(h, 1L),
    concept = "e.concept_id",
    ingredient = {
      map <- .compileIngredientMap(ctx)
      deps <- c(map$name, ev)
      join <- paste0(if (identical(spec$unmapped, "drop")) " INNER JOIN " else " LEFT JOIN ",
                     map$name, " m ON m.descendant_concept_id = e.concept_id")
      if (identical(spec$unmapped, "drop")) "m.ingredient_concept_id" else
        "COALESCE(m.ingredient_concept_id, e.concept_id)"
    })
  where <- .intervalQualitySql(h, spec, end_sql, "e")
  if (per_visit) where <- c(where, "e.visit_id IS NOT NULL")
  if (!identical(spec$by, "event")) where <- c(where, paste0(group_sql, " IS NOT NULL"))
  row_spec <- .intervalRowSpec(event_fragment, extras)
  row_cols <- names(row_spec)
  select_norm <- c(
    person_id = "e.person_id", event_id = "e.event_id",
    source_ns = .compileTextLiteral(name), group_id = group_sql,
    concept_id = "e.concept_id", start_date = "e.start_date", end_date = end_sql,
    end_recorded = "e.end_recorded", supply = "e.supply")
  for (x in extras) select_norm[[x]] <- paste0("e.", x)
  # The person relation of an adapter analyses physical events: an adapter row
  # repeats a physical event once per parent link, and the projection (which
  # omits the parent) keeps it once (A-20 item 6 (ii)); the per-parent
  # relation keeps every link (iii).
  distinct <- adapter && !per_parent
  body_norm <- paste0("SELECT ", if (distinct) "DISTINCT ",
                      paste(paste0(select_norm, " AS ", names(select_norm)), collapse = ", "),
                      " FROM ", ev, " e", join, " WHERE ", paste(where, collapse = " AND "))
  row_private <- c(.INTERVAL_PRIVATE_COLUMNS[.INTERVAL_PRIVATE_COLUMNS %in% row_cols],
                   intersect(extras, event_fragment$private_columns))
  row_private <- setdiff(row_private, "era_no")
  row_key <- c("group_id", "event_id", extras)
  row_order <- c("start_date", "source_ns", "group_id", "event_id", extras)
  identity <- c("source_ns", "group_id", "event_id", extras)
  f_norm <- register(.fragment(
    nm("norm"), "interval", "row", body_norm, row_spec, dependencies = deps,
    unique_key = row_key, person_lineage = lineage, source_identity_columns = identity,
    private_columns = row_private, availability = availability, ordering = row_order))

  # 2. stockpiled supply (effective rows, a session table) ---------------------
  temp_table <- NULL
  course_input <- f_norm
  if (stockpile) {
    eff <- .intervalEffectiveSource(ctx, name, f_norm, row_spec, extras, availability,
                                    partition, nm("effsrc"))
    temp_table <- eff$temp_table
    course_input <- register(eff$fragment)
  }
  # the normalised recorded end of a row: its own end, or the norm_end column
  # the effective rows carry (A-20 item 3)
  span_col <- if (stockpile) "norm_end" else "end_date"

  # 3. gaps and islands per (person, group[, parent]) ------------------------------
  input_cols <- names(course_input$columns)
  mark_spec <- .intervalMarkSpec(course_input$columns)
  mark_private <- unique(c(course_input$private_columns, "prev_max_end", "new_era"))
  f_mk <- register(.fragment(
    nm("mk"), "interval", "row",
    .intervalMarksSql(h, course_input$name, input_cols, partition, order_keys, spec$gap,
                      spec$persistence),
    mark_spec, dependencies = course_input$name, unique_key = row_key,
    person_lineage = lineage, source_identity_columns = identity,
    private_columns = mark_private, availability = availability, ordering = row_order))

  # 4. the retained flag of min_days (one pass over the marks) -----------------------
  kept <- f_mk
  kept_where <- function(alias) ""
  if (spec$min_days > 0L) {
    kept <- register(.fragment(
      nm("ek"), "interval", "row",
      .intervalKeptSql(h, f_mk$name, names(mark_spec), partition, span_col, spec$min_days),
      c(mark_spec, list(era_kept = .fragmentColumn("integer", FALSE, "private"))),
      dependencies = f_mk$name, unique_key = row_key, person_lineage = lineage,
      source_identity_columns = identity, private_columns = c(mark_private, "era_kept"),
      availability = availability, ordering = row_order))
    kept_where <- function(alias) paste0(" WHERE ", alias, ".era_kept = 1")
  }

  # 5. the course: one row per retained era ------------------------------------------------
  person_type <- row_spec$person_id$type
  course_spec <- .intervalCourseSpec(person_type, per_parent)
  if (per_visit) course_spec$visit_id <- .fragmentColumn("bigint", FALSE, "visit")
  legacy <- .sqlLegacyHandle(.sqlFamily(h))
  ext <- if (spec$persistence > 0L) .sqlDateAddDays(h, "x.end_date", spec$persistence) else "x.end_date"
  course_select <- c(
    person_id = "x.person_id",
    event_id = .sqlLosslessKey(h, .sqlRowNumber(h, character(0), paste0("x.", c(partition, "era_no")))),
    source_ns = .compileTextLiteral(name),
    start_date = "MIN(x.start_date)",
    end_date = paste0("MAX(", ext, ")"),
    end_recorded = paste0("MAX(x.", span_col, ")"),
    supply = .compileTypedNull(h, "integer"),
    concept_id = if (identical(spec$by, "event")) .compileTypedNull(h, "integer") else "x.group_id")
  for (col in setdiff(names(.INTERVAL_COURSE_NULLS), "supply")) {
    course_select[[col]] <- .compileTypedNull(h, .INTERVAL_COURSE_NULLS[[col]])
  }
  course_select[["unit_unknown"]] <- .sqlIntegerLiteral(h, 0L)
  course_select[["group_id"]] <- "x.group_id"
  course_select[["era_no"]] <- "x.era_no"
  course_select[["n_events"]] <- .sqlIntegerCast(h, "COUNT(*)")
  era <- c(.sqlLosslessKeyText(h, "x.group_id"), "':'", .sqlLosslessKeyText(h, "x.era_no"))
  if (per_parent) era <- c(.sqlLosslessKeyText(h, "x.parent_event_id"), "'|'", era)
  if (per_visit) era <- c(.sqlLosslessKeyText(h, "x.visit_id"), "'|'", era)
  course_select[["era_key"]] <- .omopConcatSql(legacy, era)
  if (per_parent) {
    course_select[["parent_source_ns"]] <- "x.parent_source_ns"
    course_select[["parent_event_id"]] <- "x.parent_event_id"
  }
  if (per_visit) course_select[["visit_id"]] <- "x.visit_id"
  course_select <- course_select[names(course_spec)]
  body_course <- paste0("SELECT ", paste(paste0(course_select, " AS ", names(course_select)), collapse = ", "),
                        " FROM ", kept$name, " x", kept_where("x"), " GROUP BY ",
                        paste(paste0("x.", c(partition, "era_no")), collapse = ", "))
  course_identity <- c("source_ns", "person_id", "group_id", "era_no",
                       if (per_parent) .ADAPTER_PARENT_COLUMNS, if (per_visit) "visit_id")
  f_course <- register(.fragment(
    nm(), "interval", "row", body_course, course_spec, dependencies = kept$name,
    unique_key = "event_id", person_lineage = lineage, source_identity_columns = course_identity,
    private_columns = c("event_id", "source_ns", "visit_id", "group_id", "era_no", "n_events",
                        "era_key", if (per_parent) .ADAPTER_PARENT_COLUMNS),
    availability = availability, ordering = c("start_date", "source_ns", "event_id")))

  # 6. the recorded relation (era_no attached) ------------------------------------------------
  rec_spec <- c(row_spec, list(era_no = .fragmentColumn("integer", FALSE, "era_no")))
  rec_cols <- names(rec_spec)
  body_rec <- if (!stockpile) {
    paste0("SELECT ", paste(paste0("x.", rec_cols), collapse = ", "), " FROM ", kept$name, " x",
           kept_where("x"))
  } else {
    keys <- unique(c("person_id", "group_id", "event_id", extras))
    on <- paste0("k.", keys, " = x.", keys)
    paste0("SELECT ", paste(c(paste0("x.", row_cols), "k.era_no"), collapse = ", "),
           " FROM ", f_norm$name, " x INNER JOIN ", kept$name, " k ON ",
           paste(on, collapse = " AND "), kept_where("k"))
  }
  f_rec <- register(.fragment(
    nm("rec"), "interval", "row", body_rec, rec_spec,
    dependencies = unique(c(if (stockpile) f_norm$name, kept$name)),
    unique_key = row_key, person_lineage = lineage, source_identity_columns = identity,
    private_columns = c(row_private, "era_no"), availability = availability,
    ordering = row_order))

  # 7. the effective relation (stockpiled sources) ----------------------------------------
  f_eff <- NULL
  if (stockpile) {
    eff_cols <- names(course_input$columns)
    eff_spec <- c(course_input$columns, list(era_no = .fragmentColumn("integer", FALSE, "era_no")))
    body_eff <- paste0("SELECT ", paste(paste0("x.", c(eff_cols, "era_no")), collapse = ", "),
                       " FROM ", kept$name, " x", kept_where("x"))
    f_eff <- register(.fragment(
      nm("eff"), "interval", "row", body_eff, eff_spec, dependencies = kept$name,
      unique_key = row_key, person_lineage = lineage, source_identity_columns = identity,
      private_columns = unique(c(course_input$private_columns, "era_no")),
      availability = availability, ordering = row_order))
  }

  fragments <- c(norm = f_norm$name, marks = f_mk$name,
                 if (!identical(kept$name, f_mk$name)) c(kept = kept$name),
                 course = f_course$name, recorded = f_rec$name)
  if (!is.null(f_eff)) fragments <- c(fragments, effective = f_eff$name,
                                      effective_source = course_input$name)
  .intervalMetaSet(ctx, name, list(
    name = name, spec = spec, source = spec$source, association = association,
    availability = availability, fragments = fragments, temp_table = temp_table),
    association = association)
  list(course = f_course, recorded = f_rec, effective = f_eff)
}

# --- stockpiling ------------------------------------------------------------------------

#' Effective supply of stockpiled fills (the bounded deterministic iterator)
#'
#' Pure R; one pass per partition (person, group[, parent]) over rows sorted
#' by (partition, start, row key). \code{supply_i} = positive integer supply,
#' else the recorded length \code{end_recorded - start + 1} when the recorded
#' end is not before the start, else one day; \code{carry_i = min(cap, max(0,
#' eff_end_{i-1} - start_i + 1))} with \code{eff_end_{i-1}} the previous row's
#' effective end in the same partition (never a running maximum: stock the cap
#' discards does not carry into later fills); \code{eff_end_i = start_i +
#' supply_i - 1 + carry_i}. Day arithmetic in doubles (no integer overflow);
#' the caller refuses effective ends beyond the materialisable range.
#'
#' @param rows data.frame with the partition columns, start_date, end_recorded,
#'   supply (sorted).
#' @param cap Carry cap in days.
#' @param partition Partition columns (default person_id, group_id).
#' @param path Interval path used for an unsupported supply rejection.
#' @return \code{rows} with \code{end_date} (effective end, Date),
#'   \code{supply_days} and \code{carry}.
#' @keywords internal
.stockpileIterate <- function(rows, cap, partition = c("person_id", "group_id"), path = "/intervals") {
  n <- nrow(rows)
  rows$end_date <- as.Date(character(0))[seq_len(n)]
  rows$supply_days <- integer(n)
  rows$carry <- integer(n)
  if (n == 0L) return(rows)
  cap <- as.numeric(cap)
  start <- as.numeric(as.Date(rows$start_date))
  rec <- suppressWarnings(as.numeric(as.Date(rows$end_recorded)))
  supply <- suppressWarnings(as.numeric(rows$supply))
  valid <- !is.na(supply) & is.finite(supply) & supply >= 1 & supply == floor(supply)
  sup <- ifelse(valid, supply, ifelse(!is.na(rec) & rec >= start, rec - start + 1, 1))
  if (any(!is.finite(sup) | sup > .Machine$integer.max)) {
    .compileReject("a stockpiled supply lies outside the supported integer range", field = path)
  }
  key <- do.call(paste, c(lapply(partition, function(p) as.character(rows[[p]])), sep = "\r"))
  first <- c(TRUE, key[-1L] != key[-n])
  eff <- numeric(n)
  carry <- numeric(n)
  prev <- NA_real_
  for (i in seq_len(n)) {
    if (first[[i]]) prev <- NA_real_
    c_i <- if (is.na(prev)) 0 else min(cap, max(0, prev - start[[i]] + 1))
    eff[[i]] <- start[[i]] + sup[[i]] - 1 + c_i
    carry[[i]] <- c_i
    prev <- eff[[i]]
  }
  rows$end_date <- as.Date(eff, origin = "1970-01-01")
  # a supply is an integer column value and the carry is capped by an integer
  # option: both fit an integer
  rows$supply_days <- as.integer(sup)
  rows$carry <- as.integer(carry)
  rows
}

# Literal of one value of a typed column (for the materialised rows).
.intervalLiteral <- function(h, value, type) {
  if (length(value) != 1L || is.na(value)) return("NULL")
  switch(type,
    date = .sqlDateLiteral(h, format(as.Date(value), "%Y-%m-%d")),
    text = .compileTextLiteral(as.character(value)),
    integer = , bigint = .sqlIntegerLiteral(h, if (inherits(value, "integer64")) as.character(value) else value),
    numeric = .predicateNumericLiteral(as.numeric(value)),
    stop("Unsupported materialised column type.", call. = FALSE))
}

#' Bounded private fetch (amendment A-20 (j))
#'
#' Fetches at most \code{cap + 1} rows of \code{sql}; the extra row means the
#' realisation exceeds the server cap and \code{reject()} raises the typed,
#' non-skippable rejection. Rows are never silently truncated, and no count or
#' row travels in the rejection.
#'
#' @param handle CDM handle.
#' @param sql One SELECT statement.
#' @param cap The row cap (\code{dsomop.max_memory_rows}).
#' @param reject Function raising the typed rejection.
#' @return data.frame (lower-case names, integer64 coerced as .executeQuery()).
#' @keywords internal
.fetchBounded <- function(handle, sql, cap, reject) {
  rows <- .withDbReconnect(handle, function(conn) {
    res <- DBI::dbSendQuery(conn, sql)
    on.exit(DBI::dbClearResult(res), add = TRUE)
    DBI::dbFetch(res, n = cap + 1)
  })
  if (nrow(rows) > cap) reject()
  names(rows) <- tolower(names(rows))
  .coerce_integer64(rows)
}

# Date columns of fetched rows as Date (text and native date representations).
.intervalDates <- function(rows, cols) {
  for (col in intersect(cols, names(rows))) {
    rows[[col]] <- as.Date(substr(as.character(rows[[col]]), 1L, 10L))
  }
  rows
}

# Fetch the normalised rows of a stockpiled interval, run the iterator and
# materialise the effective rows (with the normalised recorded end, norm_end)
# as a session temporary table owned by the execution context. Returns
# list(fragment, temp_table).
.intervalEffectiveSource <- function(ctx, name, f_norm, row_spec, extras, availability,
                                     partition = c("person_id", "group_id"),
                                     nm_src = .fragmentName("iv", name, "effsrc", ctx = ctx)) {
  h <- .compileDialect(ctx)
  handle <- ctx[["handle"]]
  eff_spec <- c(row_spec[setdiff(names(row_spec), c("end_recorded", "supply"))],
                list(norm_end = .fragmentColumn("date", FALSE, "private"),
                     supply_days = .fragmentColumn("integer", FALSE, "supply"),
                     carry = .fragmentColumn("integer", FALSE, "private")))
  eff_spec$end_date <- .fragmentColumn("date", FALSE, "end")
  cols <- names(eff_spec)
  types <- vapply(eff_spec, function(c) c$type, "")
  private <- unique(c(intersect(f_norm$private_columns, cols), "norm_end", "carry"))
  row_key <- c("group_id", "event_id", extras)
  make <- function(body) {
    .fragment(nm_src, "interval", "row", body, eff_spec, dependencies = character(0),
              unique_key = row_key,
              person_lineage = list(person_col = "person_id", via_roster = NULL),
              source_identity_columns = c("source_ns", row_key),
              private_columns = private, availability = availability,
              ordering = c("start_date", "source_ns", row_key))
  }
  nulls <- vapply(cols, function(col) .compileTypedNull(h, types[[col]]), "")
  if (identical(availability, "unavailable")) {
    body <- paste0("SELECT ", paste(paste0(nulls, " AS ", cols), collapse = ", "),
                   .compileNoTableFrom(h), " WHERE 1 = 0")
    return(list(fragment = make(body), temp_table = NULL))
  }
  .sqlRequire(h, "session_table")
  cap <- .intervalStockpileCap()
  max_rows <- .extractionCap("dsomop.max_memory_rows", 1000000L)
  field <- paste0("/intervals/", name)
  over_cap <- function() {
    .compileReject(paste0("interval '", name, "': the stockpiled source exceeds the server ",
                          "in-memory row cap (dsomop.max_memory_rows)"),
                   reason = "stockpile_row_cap", field = field)
  }
  # an optional early check; the realisation below is bounded as well
  n <- .adapterCount(ctx, f_norm, paste0("SELECT COUNT(*) AS n FROM ", f_norm$name))
  if (n > max_rows) over_cap()
  order <- unique(c(partition, "start_date", "event_id", extras))
  fetch <- c(person_id = "x.person_id", event_id = "x.event_id", group_id = "x.group_id",
             concept_id = "x.concept_id", start_date = "x.start_date", norm_end = "x.end_date",
             end_recorded = "x.end_recorded", supply = "x.supply")
  for (x in extras) fetch[[x]] <- paste0("x.", x)
  sql <- .buildStatement(ctx, f_norm, select = paste0(
    "SELECT ", paste(paste0(fetch, " AS ", names(fetch)), collapse = ", "), " FROM ",
    f_norm$name, " x ORDER BY ", paste(paste0("x.", order), collapse = ", ")), dialect = h)
  rows <- .intervalDates(.fetchBounded(handle, sql, max_rows, over_cap),
                         c("start_date", "norm_end", "end_recorded"))
  rows <- .stockpileIterate(rows, cap, partition, path = field)
  if (nrow(rows) && any(!is.finite(as.numeric(rows$end_date)) | rows$end_date > .INTERVAL_MAX_DATE)) {
    .compileReject(paste0("interval '", name, "': a stockpiled effective end lies beyond ",
                          format(.INTERVAL_MAX_DATE), ", outside the supported date range"),
                   field = field)
  }
  rows$source_ns <- rep(name, nrow(rows))
  base <- substr(gsub("[^a-z0-9_]", "_", tolower(paste0("dsomop_eff_", nm_src))), 1L, 48L)
  tname <- .reserveTempTableName(handle, base)
  # Registered before the CREATE, so the context's closure releases it on every
  # exit path (A-20 (k)); a failed materialisation releases it at once (a
  # failed DROP stays registered for the retry).
  assign("compile_temp_tables", unique(c(ctx[["compile_temp_tables"]] %||% character(0), tname)),
         envir = ctx)
  tryCatch({
    .createTempTable(handle, tname, paste0(
      "SELECT ", paste(paste0(nulls, " AS ", cols), collapse = ", "), .compileNoTableFrom(h),
      " WHERE 1 = 0"))
    if (nrow(rows)) {
      batch <- 500L
      for (from in seq(1L, nrow(rows), by = batch)) {
        idx <- seq.int(from, min(nrow(rows), from + batch - 1L))
        values <- vapply(idx, function(i) {
          paste0("(", paste(vapply(cols, function(col) {
            .intervalLiteral(h, rows[[col]][[i]], types[[col]])
          }, ""), collapse = ", "), ")")
        }, "")
        insert <- paste0("INSERT INTO ", tname, " (", paste(cols, collapse = ", "), ") VALUES ",
                         paste(values, collapse = ", "))
        .withDbReconnect(handle, function(conn) DBI::dbExecute(conn, insert))
      }
    }
  }, error = function(e) {
    cleanup <- tryCatch({
      .compileReleaseTempTables(ctx, tname)
      NULL
    }, error = identity)
    if (!is.null(cleanup)) {
      # Keep the materialisation error (and any typed rejection fields), but
      # also report cleanup failure instead of losing it. Ownership remains
      # registered so execution-context closure can retry the DROP.
      e$message <- paste(conditionMessage(e), conditionMessage(cleanup), sep = "\n")
      e$cleanup_error <- cleanup
    }
    stop(e)
  })
  # MySQL otherwise merges this CTE into each downstream consumer, reopening
  # the same temporary table when (for example) the course anchors a stream
  # of effective rows. DISTINCT preserves these already-unique rows and
  # makes the source CTE materialisable once for all its references.
  body <- paste0("SELECT ", if (identical(.sqlFamily(h), "mysql")) "DISTINCT ",
                 paste(paste0("t.", cols), collapse = ", "), " FROM ", tname, " t")
  list(fragment = make(body), temp_table = tname)
}

#' Release the temporary tables the compilers of a context materialised
#'
#' Drops each registered name through the session-safe
#' \code{.dropTempTable()} (only tables the handle owns, on their own session;
#' objects owned before the operation are never registered here), clears
#' only the names it released, keeps every failure registered for a retry and
#' raises one aggregated cleanup error (A-20 (k)). The execution context's
#' closure (\code{.planContextClose()}) calls it for the context and its
#' compilation child; callers that compile outside it (tests, explain) call it
#' themselves.
#'
#' @param ctx Execution or compilation context.
#' @param tables Optional subset of the registered names.
#' @return NULL invisibly.
#' @keywords internal
.compileReleaseTempTables <- function(ctx, tables = NULL) {
  if (!is.environment(ctx)) return(invisible(NULL))
  owned <- ctx[["compile_temp_tables"]] %||% character(0)
  targets <- if (is.null(tables)) owned else intersect(owned, tables)
  failed <- character(0)
  for (t in rev(targets)) {
    ok <- tryCatch({
      .dropTempTable(ctx[["handle"]], t)
      TRUE
    }, error = function(e) FALSE)
    if (!ok) failed <- c(failed, t)
  }
  assign("compile_temp_tables", setdiff(owned, setdiff(targets, failed)), envir = ctx)
  if (length(failed)) {
    stop("Could not release the compiler temporary table(s) ", paste(rev(failed), collapse = ", "),
         "; they stay registered for a retry.", call. = FALSE)
  }
  invisible(NULL)
}

# --- reducer kernels (pure R) --------------------------------------------------------------

#' Day numbers of dates (days since 1970-01-01; NA stays NA)
#' @keywords internal
.dayNumber <- function(x) {
  if (is.null(x)) return(integer(0))
  if (is.numeric(x) && !inherits(x, "Date")) return(as.integer(x))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) x <- as.Date(substr(x, 1L, 10L))
  as.integer(as.Date(x))
}

#' Union length of closed whole-day intervals
#'
#' @param start,end Integer day numbers with \code{start <= end}.
#' @return Integer number of distinct days covered.
#' @keywords internal
.dayUnionLength <- function(start, end) {
  ok <- !is.na(start) & !is.na(end) & start <= end
  start <- start[ok]
  end <- end[ok]
  if (!length(start)) return(0L)
  o <- order(start, end)
  start <- start[o]
  end <- end[o]
  total <- 0
  cs <- start[[1L]]
  ce <- end[[1L]]
  for (i in seq_along(start)[-1L]) {
    if (start[[i]] > ce + 1L) {
      total <- total + (ce - cs + 1)
      cs <- start[[i]]
      ce <- end[[i]]
    } else if (end[[i]] > ce) {
      ce <- end[[i]]
    }
  }
  as.integer(total + (ce - cs + 1))
}

# Clip closed intervals to per-key windows (NA bound = unbounded).
.clipToWindows <- function(key, start, end, windows) {
  i <- match(key, windows$key)
  lo <- .dayNumber(windows$lo)[i]
  hi <- .dayNumber(windows$hi)[i]
  s <- .dayNumber(start)
  e <- .dayNumber(end)
  s <- ifelse(is.na(lo), s, pmax(s, lo))
  e <- ifelse(is.na(hi), e, pmin(e, hi))
  keep <- !is.na(i) & !is.na(s) & !is.na(e) & s <= e
  data.frame(key = key[keep], start = as.integer(s[keep]), end = as.integer(e[keep]),
             stringsAsFactors = FALSE)
}

#' Covered days (design section 2.7, reducer \code{covered_days})
#'
#' Union length of the selected interval relation clipped to each key's
#' window and, with \code{observed}, intersected with that key's actually
#' observed segments (the PDC numerator: exposure [1,10] with observed
#' [1,2] and [9,10] over window [1,10] gives 4). Never a sum of durations;
#' grace gaps and persistence are not in the relation.
#'
#' @param intervals data.frame(key, start, end): rows of the selected relation
#'   (recorded or effective) for the roster's keys.
#' @param windows data.frame(key, lo, hi, defined = TRUE): one row per roster
#'   key; NA bounds are unbounded; \code{defined = FALSE} (a missing required
#'   anchor) gives NA.
#' @param observed NULL or data.frame(key, start, end): the key's actually
#'   observed segments (the recorded observation periods of its coverage
#'   component).
#' @return Integer vector aligned with \code{windows$key} (0 when no row).
#' @keywords internal
.coveredDays <- function(intervals, windows, observed = NULL) {
  keys <- windows$key
  if (anyDuplicated(keys)) stop("Window keys must be unique.", call. = FALSE)
  out <- integer(length(keys))
  defined <- windows$defined %||% rep(TRUE, length(keys))
  clipped <- .clipToWindows(intervals$key, intervals$start, intervals$end, windows)
  if (!is.null(observed)) {
    obs <- .clipToWindows(observed$key, observed$start, observed$end, windows)
    if (nrow(clipped) && nrow(obs)) {
      pairs <- merge(clipped, obs, by = "key", suffixes = c("", ".o"))
      s <- pmax(pairs$start, pairs$start.o)
      e <- pmin(pairs$end, pairs$end.o)
      keep <- s <= e
      clipped <- data.frame(key = pairs$key[keep], start = s[keep], end = e[keep],
                            stringsAsFactors = FALSE)
    } else {
      clipped <- clipped[0, , drop = FALSE]
    }
  }
  if (nrow(clipped)) {
    groups <- split(seq_len(nrow(clipped)), as.character(clipped$key))
    lens <- vapply(groups, function(ix) .dayUnionLength(clipped$start[ix], clipped$end[ix]), integer(1))
    out[match(names(groups), as.character(keys))] <- lens
  }
  out[!as.logical(defined) | is.na(defined)] <- NA_integer_
  out
}

#' Observed days of each key's window (derived \code{observed_days})
#'
#' Union of the key's actually observed segments clipped to its window; a gap
#' allowance never adds days (the segments are recorded periods).
#' @inheritParams .coveredDays
#' @keywords internal
.observedDays <- function(observed, windows) {
  .coveredDays(observed, windows, observed = NULL)
}

#' Supplied days (design section 2.7, reducer \code{supply_days})
#'
#' \code{fills = "start_in_window"} sums \code{days_supply} of the fills
#' starting inside each key's window; \code{"clipped"} sums, per fill, the days
#' of \code{[start, start + days_supply - 1]} inside the window (overlapping
#' fills are not unioned). A NULL, non-positive or non-integer supply
#' contributes 0; a fill failing the data-quality policy (NULL start or a
#' recorded end before its start) contributes nothing.
#'
#' @param fills data.frame(key, start, supply, end_recorded = NA).
#' @param windows data.frame(key, lo, hi, defined = TRUE).
#' @param policy \code{"start_in_window"} or \code{"clipped"}.
#' @return Integer vector aligned with \code{windows$key}.
#' @keywords internal
.supplyDays <- function(fills, windows, policy = c("start_in_window", "clipped")) {
  policy <- match.arg(policy)
  keys <- windows$key
  if (anyDuplicated(keys)) stop("Window keys must be unique.", call. = FALSE)
  out <- integer(length(keys))
  defined <- windows$defined %||% rep(TRUE, length(keys))
  if (nrow(fills)) {
    i <- match(fills$key, keys)
    lo <- .dayNumber(windows$lo)[i]
    hi <- .dayNumber(windows$hi)[i]
    s <- .dayNumber(fills$start)
    rec <- .dayNumber(fills$end_recorded %||% rep(NA, nrow(fills)))
    supply <- suppressWarnings(as.numeric(fills$supply))
    valid <- !is.na(supply) & is.finite(supply) & supply >= 1 & supply == floor(supply)
    quality <- !is.na(i) & !is.na(s) & (is.na(rec) | rec >= s)
    contribution <- if (identical(policy, "start_in_window")) {
      inside <- (is.na(lo) | s >= lo) & (is.na(hi) | s <= hi)
      ifelse(quality & valid & inside, supply, 0)
    } else {
      e <- s + supply - 1
      cs <- ifelse(is.na(lo), s, pmax(s, lo))
      ce <- ifelse(is.na(hi), e, pmin(e, hi))
      ifelse(quality & valid & ce >= cs, ce - cs + 1, 0)
    }
    contribution[is.na(contribution)] <- 0
    sums <- tapply(contribution[!is.na(i)], i[!is.na(i)], sum)
    if (any(!is.finite(sums) | sums > .Machine$integer.max)) {
      .compileReject("supplied days exceed the supported integer range", field = "/reducer")
    }
    out[as.integer(names(sums))] <- as.integer(sums)
  }
  out[!as.logical(defined) | is.na(defined)] <- NA_integer_
  out
}


# --- reducer streams over rosters (covered_days, supply_days, observed_days) ---------------

#' The row relation a coverage reducer reads
#'
#' \code{coverage = "recorded"}: the recorded relation of an interval source
#' (\code{iv_<name>_rec}) or the rows of an event source (its own end policy);
#' \code{"effective_supply"}: the effective relation of a stockpiled interval
#' (refused for any other source, as the IR validator does).
#'
#' @param ctx Execution context.
#' @param source Typed source reference \code{list(event = )} or
#'   \code{list(interval = )}.
#' @param coverage \code{"recorded"} or \code{"effective_supply"}.
#' @param path JSON path used in rejections.
#' @param association \code{"person"} or \code{"parent"} (the per-parent
#'   relation of an interval over an adapter, A-20 item 6).
#' @return The fragment.
#' @keywords internal
.intervalRelation <- function(ctx, source, coverage = "recorded", path = "/reducer/coverage",
                              association = "person") {
  if (!coverage %in% c("recorded", "effective_supply")) {
    .irReject("coverage is recorded or effective_supply", field = path)
  }
  kind <- names(source)[[1L]]
  ref <- unlist(source[[1L]])
  if (identical(kind, "interval")) {
    iv <- .compileIntervalByName(ctx, ref, association = association)
    if (identical(coverage, "effective_supply")) {
      if (is.null(iv$effective)) {
        .irReject("effective_supply needs a stockpiled interval source", field = path)
      }
      return(iv$effective)
    }
    return(iv$recorded)
  }
  if (identical(coverage, "effective_supply")) {
    .irReject("effective_supply needs a stockpiled interval source", field = path)
  }
  .compileEventByName(ctx, ref)
}

#' The coverage-quality predicate of a direct event source (A-20 item 2, (g))
#'
#' The one data-quality policy of section 6.7, evaluated at a coverage
#' consumer (\code{covered_days}, \code{supply_days}) on the event's declared
#' end policy and its ORIGINAL \code{end_recorded}/\code{supply} roles: start
#' present, end not before start, and -- under \code{end = {fallback:
#' "days_supply"}}, where the supply is an end source -- a present supply that
#' is not a positive integer drops a row without a recorded end. A NULL supply
#' is a one-day record; under the other policies the supply is not an end
#' source. Source events are never deleted (an era applies its own override
#' from the original roles).
#'
#' @param h Handle, context, profile or dialect name.
#' @param meta The event's metadata (\code{.eventMeta()}).
#' @param alias SQL alias of the event relation.
#' @return Character vector of SQL predicates (ANDed by the caller).
#' @keywords internal
.eventCoverageQualitySql <- function(h, meta, alias = "i") {
  a <- function(col) paste0(alias, ".", col)
  out <- c(paste0(a("start_date"), " IS NOT NULL"), paste0(a("end_date"), " >= ", a("start_date")))
  if (identical(meta$end_policy, "fallback")) {
    supply <- a("supply")
    out <- c(out, paste0("(", a("end_recorded"), " IS NOT NULL OR ", supply, " IS NULL OR (",
                         supply, " >= 1 AND ", .sqlIntegerCast(h, supply), " = ", supply, "))"))
  }
  out
}

#' Rows of a source overlapping each roster row's window
#'
#' The relation is resolved through the consumer association
#' (\code{.consumerAssociation()}): the index visit, the episode parent of an
#' adapter source (the per-parent interval relation, or the adapter's links
#' bound to the row's origin) and the person; a person-indexed stream over an
#' adapter event keeps each physical event once per roster row (A-20 item 6).
#' A direct event source gets the coverage-quality predicate
#' (\code{.eventCoverageQualitySql()}).
#'
#' @param ctx Execution context.
#' @param source Typed source reference \code{list(event = )} or
#'   \code{list(interval = )}.
#' @param roster_fragment Roster or candidates fragment.
#' @param window Window object or NULL.
#' @param phase \code{"candidate"} or \code{"final"}.
#' @param name Fragment name (allocated by the consumer, A-20 (e)).
#' @param coverage \code{"recorded"} or \code{"effective_supply"}.
#' @param overlap Logical; select rows overlapping the window (intervals) or
#'   starting inside it (fills under \code{start_in_window}).
#' @param extra Named character vector of extra relation columns to project.
#' @param path JSON path used in rejections.
#' @param use \code{"rows"} (the recorded / effective rows of an interval, the
#'   rows of an event: coverage consumers) or \code{"course"} (the course of
#'   an interval source, an event consumer such as an era count).
#' @return Stream fragment: row_key, person_id, start_date, end_date (+ extra
#'   and, over an adapter event, the private physical identity src_*).
#' @keywords internal
.compileIntervalStream <- function(ctx, source, roster_fragment, window, phase, name,
                                   coverage = "recorded", overlap = TRUE, extra = character(0),
                                   path = "/reducer", use = c("rows", "course")) {
  use <- match.arg(use)
  existing <- .fragmentGet(ctx, name)
  if (!is.null(existing)) return(existing)
  h <- .compileDialect(ctx)
  assoc <- .consumerAssociation(ctx, source, roster_fragment, phase, path = paste0(path, "/source"))
  spec <- assoc$spec
  win <- .compileWindow(ctx, window, roster_fragment, phase, alias = "r")
  pred <- if (isTRUE(overlap)) .windowOverlapSql(win, "i.start_date", "i.end_date") else
    .windowStartPredicateSql(win, "i.start_date")
  joins <- if (length(win$joins)) paste0(" ", paste(unname(win$joins), collapse = " ")) else ""
  event <- identical(assoc$kind, "event")
  quality <- if (event) .eventCoverageQualitySql(h, .eventMeta(ctx, assoc$ref), "i")
  rels <- list()
  phys <- character(0)
  bodies <- character(0)
  for (b in assoc$branches) {
    rel <- .consumerRelation(ctx, assoc, b, use, coverage, path = paste0(path, "/coverage"))
    rels[[b]] <- rel
    if (event && !is.null(assoc$parent)) {
      phys <- unique(c("source_ns", setdiff(rel$unique_key, .ADAPTER_PARENT_COLUMNS)))
    }
    on <- c(.consumerJoinSql(h, assoc, b, "i", "r"), win$defined, pred, quality)
    cols <- c(row_key = paste0("r.", spec$key), person_id = paste0("r.", spec$person),
              start_date = "i.start_date", end_date = "i.end_date")
    for (x in names(extra)) cols[[x]] <- paste0("i.", extra[[x]])
    for (x in phys) cols[[paste0("src_", x)]] <- paste0("i.", x)
    bodies <- c(bodies, paste0(
      "SELECT ", if (length(phys)) "DISTINCT ", paste(paste0(cols, " AS ", names(cols)), collapse = ", "),
      " FROM ", spec$name, " r", joins, " INNER JOIN ", rel$name, " i ON ",
      paste(on, collapse = " AND ")))
  }
  rel <- rels[[1L]]
  spec_cols <- list(row_key = .fragmentColumn(spec$key_type, FALSE, "episode_key"),
                    person_id = .fragmentColumn(spec$person_type, FALSE, "person_key"),
                    start_date = .fragmentColumn("date", TRUE, "start"),
                    end_date = .fragmentColumn("date", TRUE, "end"))
  for (x in names(extra)) {
    rc <- rel$columns[[extra[[x]]]]
    spec_cols[[x]] <- .fragmentColumn(rc$type, TRUE, if (rc$role %in% c("private", "person_key", "episode_key")) "measure" else rc$role)
  }
  for (x in phys) spec_cols[[paste0("src_", x)]] <- .fragmentColumn(rel$columns[[x]]$type, FALSE, "private")
  f <- .fragment(name, "stream", "row", paste(bodies, collapse = " UNION ALL "), spec_cols,
                 dependencies = unique(c(spec$name, win$dependencies,
                                         vapply(rels, function(x) x$name, ""))),
                 person_lineage = list(person_col = "person_id", via_roster = NULL),
                 roster_namespace = spec$namespace,
                 private_columns = c("row_key", if (length(phys)) paste0("src_", phys)),
                 availability = rel$availability)
  .sqlAssertTypedSql(h, f$select_body)
  .fragmentRegister(ctx, f)
}

# Availability of a coverage reducer's source (the source event's), with the
# source/coverage checks of .intervalRelation() made without compiling the
# relation (the consumer association picks the relation later).
.reducerSourceAvailability <- function(ctx, source, coverage, path = "/reducer/coverage") {
  if (!coverage %in% c("recorded", "effective_supply")) {
    .irReject("coverage is recorded or effective_supply", field = path)
  }
  kind <- names(source)[[1L]]
  ref <- unlist(source[[1L]])
  base <- ref
  if (identical(kind, "interval")) {
    s <- .intervalSpec(.irNamedNode(ctx[["ir"]], "interval", ref), ref)
    if (identical(coverage, "effective_supply") && !isTRUE(s$stockpile)) {
      .irReject("effective_supply needs a stockpiled interval source", field = path)
    }
    base <- s$source
  } else if (identical(coverage, "effective_supply")) {
    .irReject("effective_supply needs a stockpiled interval source", field = path)
  }
  .compileEventByName(ctx, base)
  .eventMeta(ctx, base)$availability
}

#' Fetch a fragment's rows (dates as Date), bounded (A-20 (j))
#'
#' At most \code{dsomop.max_memory_rows} rows: one more is the typed rejection
#' \code{reason} (\code{"reducer_row_cap"} by default), never a truncation.
#'
#' @param ctx Execution context.
#' @param f The fragment.
#' @param order Optional ORDER BY columns.
#' @param reason Rejection reason when the cap is exceeded.
#' @param field JSON path of the consumer, for the rejection.
#' @return data.frame.
#' @keywords internal
.fetchFragment <- function(ctx, f, order = NULL, reason = "reducer_row_cap", field = "/reducer") {
  sql <- .buildStatement(ctx, f, select = paste0(
    "SELECT * FROM ", f$name, if (length(order)) paste0(" ORDER BY ", paste(order, collapse = ", "))),
    dialect = .compileDialect(ctx))
  cap <- .extractionCap("dsomop.max_memory_rows", 1000000L)
  rows <- .fetchBounded(ctx[["handle"]], sql, cap, function() {
    .compileReject("a reducer stream exceeds the server in-memory row cap (dsomop.max_memory_rows)",
                   reason = reason, field = field)
  })
  for (col in names(rows)) {
    if (identical(f$columns[[col]]$type, "date")) rows[[col]] <- as.Date(substr(as.character(rows[[col]]), 1L, 10L))
  }
  rows
}

#' Values of a coverage-family reducer for every roster row
#'
#' The executable base of \code{covered_days} (union of the selected relation
#' clipped to the window, optionally intersected with the row's observed
#' segments), \code{supply_days} (both fill policies) and the derived
#' \code{observed_days(window)}. Every roster row gets a value: 0 without
#' rows of an available source, NA when the source is unavailable (A-20 (i)),
#' NA when the window's required anchor is missing; observation-dependent
#' values (observed days, \code{observed_only = TRUE}) are NA for a row kept
#' without a coverage component (\code{on_missing = "keep"}). The streams are
#' scoped by the consumer association (index visit, episode parent, person;
#' \code{.consumerAssociation()}), and every fetch is bounded by
#' \code{dsomop.max_memory_rows} (typed rejection \code{reducer_row_cap}).
#'
#' @param ctx Execution context.
#' @param fn \code{"covered_days"}, \code{"supply_days"} or
#'   \code{"observed_days"}.
#' @param roster_fragment Roster or candidates fragment.
#' @param phase \code{"candidate"} or \code{"final"}.
#' @param source Typed source reference (not for observed_days).
#' @param window Window object or NULL.
#' @param coverage \code{"recorded"} or \code{"effective_supply"}.
#' @param observed_only Logical (covered_days).
#' @param fills \code{"start_in_window"} or \code{"clipped"} (supply_days).
#' @param cov The roster's coverage (\code{.compileCoverage()}), required for
#'   observed_days and \code{observed_only = TRUE}.
#' @param names \code{list(bounds =, stream =)}: the fragment identifiers the
#'   consumer allocated from its own identity family (A-20 (e); required,
#'   \code{stream} not for observed_days). There is no context-free default.
#' @param path JSON path of the consumer (rejections).
#' @return data.frame(row_key, value).
#' @keywords internal
.intervalReducerValues <- function(ctx, fn, roster_fragment, phase = "final", source = NULL,
                                   window = NULL, coverage = "recorded", observed_only = FALSE,
                                   fills = "start_in_window", cov = NULL, names = NULL,
                                   path = "/reducer") {
  fn <- match.arg(fn, c("covered_days", "supply_days", "observed_days"))
  .rosterSpec(roster_fragment, phase)
  need <- c("bounds", if (!identical(fn, "observed_days")) "stream")
  if (!is.list(names) || !all(vapply(need, function(k) .isScalarString(names[[k]]), logical(1)))) {
    stop("A reducer's bounds and stream fragments are named by its consumer: pass names = ",
         "list(bounds =, stream =) allocated from the consumer's identity family (A-20 (e)).",
         call. = FALSE)
  }
  bounds <- .compileWindowBounds(ctx, window, roster_fragment, phase, names[["bounds"]])
  b <- .fetchFragment(ctx, bounds, order = "row_key", field = path)
  windows <- data.frame(key = b$row_key, lo = b$win_start, hi = b$win_end,
                        defined = as.integer(b$win_defined) == 1L, stringsAsFactors = FALSE)
  observed <- NULL
  if (identical(fn, "observed_days") || isTRUE(observed_only)) {
    if (is.null(cov)) stop("Observed days need the roster's coverage.", call. = FALSE)
    seg <- .compileObservedSegments(ctx, cov, roster_fragment, phase)
    o <- .fetchFragment(ctx, seg, order = c("row_key", "start_date"), field = path)
    observed <- data.frame(key = o$row_key, start = o$start_date, end = o$end_date,
                           stringsAsFactors = FALSE)
    # A row without a coverage component (kept under on_missing = "keep") has
    # no observation: its observation-dependent values are NA, never 0.
    covered <- .coverageCoveredKeys(ctx, cov, roster_fragment, phase, field = path)
    windows$defined <- windows$defined & as.character(windows$key) %in% as.character(covered)
  }
  # An unavailable source (on_missing_table = "unavailable") has no value:
  # NA for every roster row, never the zero of an available, event-free source.
  unavailable <- rep(NA_integer_, nrow(windows))
  value <- switch(fn,
    observed_days = .observedDays(observed, windows),
    covered_days = {
      avail <- .reducerSourceAvailability(ctx, source, coverage, path = paste0(path, "/coverage"))
      if (identical(avail, "unavailable")) unavailable else {
        st <- .compileIntervalStream(ctx, source, roster_fragment, window, phase,
                                     names[["stream"]], coverage = coverage, path = path)
        s <- .fetchFragment(ctx, st, order = c("row_key", "start_date"), field = path)
        .coveredDays(data.frame(key = s$row_key, start = s$start_date, end = s$end_date,
                                stringsAsFactors = FALSE), windows, observed)
      }
    },
    supply_days = {
      kind <- names(source)[[1L]]
      if (!identical(kind, "event")) {
        .irReject("supply_days reads the days_supply role of an event source", field = path)
      }
      ref <- unlist(source[[1L]])
      .compileEventByName(ctx, ref)
      meta <- .eventMeta(ctx, ref)
      if (identical(meta$availability, "unavailable")) unavailable else {
        if (!"supply" %in% meta$roles) {
          .irReject("supply_days needs a source with the days_supply role", field = path)
        }
        # A fill starting before the window can supply days inside it
        # (clipped); its supplied span is not its recorded end, so the stream
        # keeps every fill starting no later than the window's end.
        clipped <- identical(fills, "clipped")
        st <- .compileIntervalStream(
          ctx, source, roster_fragment,
          if (clipped && !is.null(window)) utils::modifyList(window, list(start = NULL), keep.null = TRUE) else window,
          phase, names[["stream"]], overlap = FALSE,
          extra = c(supply = "supply", end_recorded = "end_recorded"), path = path)
        s <- .fetchFragment(ctx, st, order = c("row_key", "start_date"), field = path)
        .supplyDays(data.frame(key = s$row_key, start = s$start_date, supply = s$supply,
                               end_recorded = s$end_recorded, stringsAsFactors = FALSE),
                    windows, fills)
      }
    })
  data.frame(row_key = windows$key, value = value, stringsAsFactors = FALSE)
}
