# Module: Variables and reducers (design-2.10.3 sections 2.7, 6.11, 6.14)
#
# A variable is computed for every row of the roster of the output that
# projects it (a final roster, keyed by cohort_row_id, section 6.9), in four
# kinds:
#
#   source + reducer  rows of an event or interval source joined to each
#                     roster row on its person and the consumer association
#                     (an index-visit event joins the row's index visit; an
#                     episode-member source over an episode-indexed roster
#                     joins the row's parent episode identity; otherwise the
#                     person, each physical event once), restricted to the
#                     reducer's window, then reduced in R under the frozen
#                     reducer contract of section 2.7 (order (start_date,
#                     source namespace, event key) -- the source's declared
#                     tie-breakers; NULL handling; empty and all-missing
#                     results; output types). covered_days / supply_days and
#                     the derived observed_days run on the interval kernels of
#                     WS-5 (.intervalReducerValues()) under names this module
#                     allocates (amendment A-20 (e));
#   derived           person-derived values (age with optional .bandAges()
#                     bands, sex, race, ethnicity, birth-year bands, death,
#                     observation periods, observed days, comorbidity scores:
#                     native A-2 semantics, conditions on or before the
#                     reference date);
#   raw               person / death / observation_period columns, broadcast
#                     per episode (class-1 columns refused, birth components
#                     permitted, section 7.6);
#   population_column roster columns (index_source, set_id, is_case,
#                     sampling_weight).
#
# The `any` / `all` reducers evaluate their predicate on every row of the
# window with native three-valued logic and combine with Kleene logic: `any`
# is 1 when some row is TRUE, 0 when every row is FALSE (and over an empty
# window), NA otherwise; `all` is 0 when some row is FALSE, 1 when every row is
# TRUE, NA when no row is FALSE and one is UNKNOWN, and NA over an empty window
# (a person without measurements is not "all normal").
#
# Every value is NA for a roster row whose window is undefined (a missing
# required anchor) and for every row when the source is unavailable at this
# site (on_missing_table = "unavailable"); declared zero-fill reducers are 0
# for an available source without rows in a defined window.
#
# Streams are private fragments of the identity family "rd" (amendment A-17):
# ("rd", <output>, "<kind>_<variable>"), one per variable and consumer, never
# shared across coverage relations, observation domains or consumer
# associations (A-20 (e)). Every fetch into R is bounded by
# dsomop.max_memory_rows: at most cap + 1 rows are read and the extra row is
# the typed, non-skippable rejection validation_error / reducer_row_cap
# (A-20 (j)); nothing is ever truncated.

# Reducers whose empty input is a structural zero (section 2.7).
.REDUCER_ZERO_FILL <- c("binary", "count", "n_distinct", "sum", "abnormal_high",
                        "abnormal_low", "any", "era_count", "covered_days",
                        "supply_days")
# Reducers over the source's (normalised) value column by default.
.REDUCER_VALUE_FNS <- c("first_value", "last_value", "nth_value", "closest_value",
                        "mean", "min", "max", "sum", "sd", "cv", "median",
                        "quantile", "slope", "abnormal_high", "abnormal_low")
# Occurrence reducers combined by OR / MAX under collapse = "any".
.REDUCER_OCCURRENCE <- c("binary", "count", "n_distinct", "abnormal_high",
                         "abnormal_low", "any", "era_count")
.REDUCER_INTEGER_FNS <- c("binary", "count", "n_distinct", "any", "all",
                          "first_date", "last_date", "days_since", "days_until",
                          "time_since", "duration_sum", "covered_days",
                          "supply_days", "abnormal_high", "abnormal_low",
                          "era_count", "era_days", "first_era_days")

# --- identity family ("rd") ---------------------------------------------------------------

# Fragment kinds of one variable (single tokens without "_", so that the suffix
# "<kind>_<variable>" is an injective encoding of (kind, variable)).
.REDUCE_VARIABLE_KINDS <- c("s", "b", "a")
# Per-output person attribute fragments: person row, first death, observation
# periods.
.REDUCE_OUTPUT_KINDS <- c("pp", "pd", "po")

#' Identities of the reducer fragments of a plan (amendment A-17, A-20 (e))
#'
#' For every output and every variable it projects
#' (\code{.irOutputVariables()}): \code{("rd", <output>, "s_<variable>")} (the
#' stream), \code{"b_<variable>"} (per-row window bounds) and
#' \code{"a_<variable>"} (an auxiliary relation: reference bounds, the score
#' condition rows); and per output the person attribute fragments
#' \code{"pp"}, \code{"pd"}, \code{"po"}. Readable forms are illustrative; the
#' allocator of \code{R/compile_statement.R} names them per context.
#' @param ir The IR.
#' @return List of \code{.fragmentIdentity()} records.
#' @keywords internal
.reduceFragmentIdentities <- function(ir) {
  out <- list()
  for (o in names(ir[["outputs"]] %||% list())) {
    for (k in .REDUCE_OUTPUT_KINDS) out[[length(out) + 1L]] <- .fragmentIdentity("rd", o, k)
    for (v in .irOutputVariables(ir, o)) {
      for (k in .REDUCE_VARIABLE_KINDS) {
        out[[length(out) + 1L]] <- .fragmentIdentity("rd", o, paste0(k, "_", v))
      }
    }
  }
  out
}

# The allocated name of a reducer fragment.
.reduceName <- function(ctx, cons, kind, variable = NULL) {
  suffix <- if (is.null(variable)) kind else paste0(kind, "_", variable)
  .fragmentName("rd", cons$output, suffix, ctx = ctx)
}

# --- bounded realisation ------------------------------------------------------------------

#' The in-memory row cap of reducer and output fetches
#' @keywords internal
.reduceRowCap <- function() .extractionCap("dsomop.max_memory_rows", 1000000L)

#' Fetch at most cap + 1 rows of a statement; refuse the extra row
#'
#' The realised rows are the only rows any later step sees (one realisation,
#' never a separate count). More than \code{dsomop.max_memory_rows} rows are the
#' typed rejection \code{validation_error} with reason \code{reducer_row_cap}
#' (amendment A-20 (j)); nothing is truncated and no count leaves the server.
#'
#' @param ctx Compilation context.
#' @param sql One SELECT statement.
#' @param field JSON path of the consumer (for the rejection).
#' @return data.frame with lower-case names.
#' @keywords internal
.reduceFetchSql <- function(ctx, sql, field = "/outputs") {
  .fetchBounded(ctx[["handle"]], sql, .reduceRowCap(), function() {
    .compileReject(paste0("a private stream exceeds the server in-memory row cap ",
                          "(dsomop.max_memory_rows)"),
                   reason = "reducer_row_cap", field = field)
  })
}

#' Rows of a fragment, bounded, with its date columns as Date
#'
#' @param ctx Compilation context.
#' @param f A registered fragment.
#' @param order Optional ORDER BY columns.
#' @param field JSON path of the consumer.
#' @return data.frame.
#' @keywords internal
.reduceFetch <- function(ctx, f, order = NULL, field = "/outputs") {
  sql <- .buildStatement(ctx, f, select = paste0(
    "SELECT * FROM ", f$name,
    if (length(order)) paste0(" ORDER BY ", paste(order, collapse = ", "))),
    dialect = .compileDialect(ctx))
  rows <- .reduceFetchSql(ctx, sql, field)
  .reduceTypeFrame(rows, f)
}

# Coerce fetched columns to the fragment's declared R classes.
.reduceTypeFrame <- function(rows, f) {
  for (col in names(f$columns)) {
    if (!col %in% names(rows)) next
    type <- f$columns[[col]]$type
    x <- rows[[col]]
    rows[[col]] <- switch(type,
      date = if (inherits(x, "Date")) x else as.Date(substr(as.character(x), 1L, 10L)),
      integer = if (is.character(x)) x else if (is.logical(x) && all(is.na(x))) {
        rep(NA_integer_, length(x))
      } else x,
      numeric = if (is.logical(x) && all(is.na(x))) rep(NA_real_, length(x)) else
        suppressWarnings(as.numeric(x)),
      text = if (is.factor(x)) as.character(x) else if (is.logical(x) && all(is.na(x))) {
        rep(NA_character_, length(x))
      } else as.character(x),
      x)
  }
  rows
}

# --- the consumer ---------------------------------------------------------------------------

#' The consumer record of the variables of one output
#'
#' @param ctx Compilation context.
#' @param output Output name.
#' @param roster The final roster fragment of the output's rows.
#' @param coverage The population's coverage record (\code{.compileCoverage()}),
#'   or NULL when the population has none.
#' @param grain \code{"episode"} or \code{"person"} (the output's grain).
#' @return list(output, node, roster, spec, coverage, population, grain,
#'   time_window, temporal).
#' @keywords internal
.reduceConsumer <- function(ctx, output, roster, coverage = NULL, grain = NULL) {
  node <- ctx[["ir"]][["outputs"]][[output]]
  if (is.null(node)) stop("Unknown output '", output, "'.", call. = FALSE)
  spec <- .rosterSpec(roster, "final")
  opts <- node[["options"]] %||% list()
  list(output = output, node = node, roster = roster, spec = spec,
       coverage = coverage, population = node[["rows"]],
       grain = grain %||% node[["grain"]] %||% "episode",
       time_window = if (identical(node[["format"]], "wide")) opts[["time_window"]],
       temporal = if (node[["format"]] %in% c("wide", "long")) opts[["temporal"]])
}

#' Rows of a final roster (memoised per context and roster)
#'
#' @param ctx Compilation context.
#' @param roster The final roster fragment.
#' @return data.frame ordered by cohort_row_id, with the person column also as
#'   \code{person_id} and the roster's dates as Date.
#' @keywords internal
.reduceRosterFrame <- function(ctx, roster) {
  memo <- .compileMemo(ctx, "reduce_rosters")
  if (exists(roster$name, envir = memo, inherits = FALSE)) {
    return(get(roster$name, envir = memo, inherits = FALSE))
  }
  spec <- .rosterSpec(roster, "final")
  rows <- .reduceFetch(ctx, roster, order = "cohort_row_id",
                       field = paste0("/populations/", spec$namespace %||% roster$name))
  rows$person_id <- rows[[spec$person]]
  assign(roster$name, rows, envir = memo)
  rows
}

# --- sources --------------------------------------------------------------------------------

# Predicate catalogue of an interval course (the event-shaped course columns).
.reduceCourseCatalogue <- function(name) {
  .predicateCatalogue(
    list(start_date = list(type = "date"), end_date = list(type = "date"),
         end_recorded = list(type = "date"),
         concept_id = list(type = "integer", concept = TRUE)),
    refused = c(person_id = "a person identifier is never filterable",
                event_id = "row identifiers are never filterable",
                value = "an interval course has no value role",
                unit_concept_id = "an interval course has no unit role",
                range_low = "an interval course has no reference-range role",
                range_high = "an interval course has no reference-range role",
                supply = "an interval course has no days_supply role"),
    what = paste0("interval '", name, "'"))
}

#' Resolve a typed source reference to its fragment, catalogue and availability
#'
#' @param ctx Compilation context.
#' @param source \code{list(event = )} or \code{list(interval = )}.
#' @return list(kind, name, fragment, meta, catalogue, available).
#' @keywords internal
.reduceSource <- function(ctx, source) {
  kind <- names(source)[[1L]]
  ref <- as.character(unlist(source[[1L]]))
  if (identical(kind, "event")) {
    f <- .compileEventByName(ctx, ref)
    meta <- .eventMeta(ctx, ref)
    catalogue <- meta$catalogue
  } else if (identical(kind, "interval")) {
    f <- .compileIntervalByName(ctx, ref)$course
    meta <- .intervalMeta(ctx, ref)
    catalogue <- .reduceCourseCatalogue(ref)
  } else {
    .irReject("a source is {event: <name>} or {interval: <name>}", field = "/variables")
  }
  list(kind = kind, name = ref, fragment = f, meta = meta, catalogue = catalogue,
       available = identical(f$availability, "available"))
}

#' The consumer association of a source over a final roster
#'
#' Uses the same resolver as anchors and interval coverage. Its branches bind
#' the index visit and parent origin before any selection or reduction.
#' @param ctx Compilation context.
#' @param src Resolved source.
#' @param cons Consumer record.
#' @return The record from \code{.consumerAssociation()}.
#' @keywords internal
.reduceAssociation <- function(ctx, src, cons) {
  .consumerAssociation(ctx, stats::setNames(list(src$name), src$kind), cons$roster,
                        "final", path = paste0("/outputs/", cons$output, "/source"))
}

# Resolve every branch before assembling one roster join. Keeping the roster
# reference outside the branch union also works with MySQL session tables.
# Registered source fragments are retained intact, including the private
# mysql_stockpile_source attribute used by .buildStatement().
.reduceSourceFrom <- function(ctx, src, assoc, alias = "e", use = "course", coverage = "recorded") {
  h <- .compileDialect(ctx)
  rels <- lapply(assoc$branches, function(b) .consumerRelation(ctx, assoc, b, use = use, coverage = coverage))
  names(rels) <- assoc$branches
  cols <- unique(unlist(lapply(rels, function(f) names(f$columns))))
  from <- rels[[1L]]$name
  branches <- vapply(assoc$branches, function(b) {
    paste(.consumerJoinSql(h, assoc, b, alias, "r"), collapse = " AND ")
  }, "")
  if (identical(src$kind, "event")) {
    # Every parent branch binds one origin. Only a person branch deduplicates
    # links; the physical identity includes the adapter's member table.
    if (!is.null(assoc$parent) && "person" %in% assoc$branches) {
      f <- rels[[1L]]
      physical <- unique(c("person_id", "source_ns", setdiff(f$unique_key, .ADAPTER_PARENT_COLUMNS)))
      rn <- .sqlRowNumber(h, paste0("m.", physical), paste0("m.", .ADAPTER_PARENT_COLUMNS))
      from <- paste0("(SELECT m.*, ", rn, " AS dsomop_link_rank FROM ", f$name, " m)")
      branches[["person"]] <- paste0("(", branches[["person"]], ") AND ", alias, ".dsomop_link_rank = 1")
    }
  } else if (length(rels) > 1L) {
    types <- list()
    for (f in rels) types[names(f$columns)] <- f$columns
    bodies <- lapply(names(rels), function(b) {
      f <- rels[[b]]
      projected <- vapply(cols, function(col) {
        if (col %in% names(f$columns)) paste0("m.", col) else .compileTypedNull(h, types[[col]]$type)
      }, "")
      paste0("SELECT ", paste(paste0(projected, " AS ", cols), collapse = ", "), ", ",
             .compileTextLiteral(b), " AS dsomop_association FROM ", f$name, " m")
    })
    from <- paste0("(", paste(bodies, collapse = " UNION ALL "), ")")
    for (b in names(branches)) {
      branches[[b]] <- paste0(alias, ".dsomop_association = ", .compileTextLiteral(b),
                              " AND (", branches[[b]], ")")
    }
  }
  list(from = paste0(from, " ", alias),
       predicates = paste0("(", paste(paste0("(", branches, ")"), collapse = " OR "), ")"),
       dependencies = unique(vapply(rels, function(f) f$name, "")), fragment = rels[[1L]])
}

# The deterministic order columns of a source (A-17: the source's own
# tie-breakers, start date first).
.reduceSourceOrdering <- function(src) {
  f <- src$fragment
  ord <- unique(c(f$ordering, f$unique_key))
  c("start_date", setdiff(ord, c("start_date", "end_date")))
}

# --- windows and anchors ------------------------------------------------------------------------
#
# A window's per-row bounds are resolved in R: built-in anchors are columns of
# the roster frame (one fetch of the roster), dates are literals, and a source
# anchor is the anchor fragment of the final stage (.anchorExpr(), A-20 (d))
# fetched on its own. No statement of this module therefore reads the roster
# twice (MySQL refuses a session temporary table referenced more than once in
# one statement): a stream applies in SQL only the bounds whose anchors are
# roster columns or dates, and the exact window -- every bound -- is applied in
# R before reducing.

# A window object, or NULL for an unbounded window (an omitted window is
# unbounded, section 2.4).
.reduceWindow <- function(window) {
  if (is.null(window) || (is.list(window) && !length(window))) return(NULL)
  window
}

# The anchor a reducer reports against: its explicit reference, else its
# window's anchor, else the index (section 2.7, "A").
.reduceAnchor <- function(r) {
  r[["reference"]] %||% (.reduceWindow(r[["window"]])[["anchor"]]) %||% "index"
}

# The two bounds of a window as list(anchor, offset) or NULL (unbounded).
.reduceWindowSides <- function(window) {
  window <- .reduceWindow(window)
  if (is.null(window)) return(list(lower = NULL, upper = NULL))
  default <- window[["anchor"]] %||% "index"
  list(lower = .windowBound(window[["start"]], default, "/window/start"),
       upper = .windowBound(window[["end"]], default, "/window/end"))
}

#' Anchor dates of every roster row
#'
#' @param ctx Compilation context.
#' @param cons Consumer.
#' @param anchor An anchor (built-in name, date object or source anchor).
#' @param field JSON path of the consumer.
#' @return Date vector aligned with the roster frame's rows (NA = no anchor).
#' @keywords internal
.reduceAnchorDates <- function(ctx, cons, anchor, field = "/outputs") {
  roster <- .reduceRosterFrame(ctx, cons$roster)
  a <- .anchorNormalize(anchor)
  if (identical(a$kind, "date")) return(rep(as.Date(a$date), nrow(roster)))
  # every anchor resolves through the one resolver (A-20 (d)): a built-in is
  # the roster column it names, read from the roster frame
  ax <- .anchorExpr(ctx, anchor, cons$roster, "final", alias = "r")
  if (identical(a$kind, "builtin")) {
    col <- sub("^r\\.", "", ax$expr)
    if (!col %in% names(roster)) {
      stop("The roster '", cons$roster$name, "' has no column for anchor '", a$name, "'.",
           call. = FALSE)
    }
    return(as.Date(roster[[col]]))
  }
  memo <- .compileMemo(ctx, "reduce_anchor_dates")
  key <- ax$dependencies[[1L]]
  if (exists(key, envir = memo, inherits = FALSE)) {
    rows <- get(key, envir = memo, inherits = FALSE)
  } else {
    rows <- .reduceFetch(ctx, .fragmentGet(ctx, key), order = "row_key", field = field)
    assign(key, rows, envir = memo)
  }
  as.Date(rows$anchor_date[match(as.character(roster$cohort_row_id), as.character(rows$row_key))])
}

#' Per-row bounds of a window
#'
#' @param ctx Compilation context.
#' @param cons Consumer.
#' @param window Window object or NULL.
#' @param field JSON path of the consumer.
#' @return data.frame(key, lo, hi, defined) aligned with the roster frame;
#'   NA bounds are unbounded; \code{defined} is FALSE when a required anchor
#'   is missing.
#' @keywords internal
.reduceBoundsR <- function(ctx, cons, window, field = "/outputs") {
  roster <- .reduceRosterFrame(ctx, cons$roster)
  n <- nrow(roster)
  out <- data.frame(key = roster$cohort_row_id, lo = as.Date(rep(NA_character_, n)),
                    hi = as.Date(rep(NA_character_, n)), defined = rep(TRUE, n),
                    stringsAsFactors = FALSE)
  sides <- .reduceWindowSides(window)
  for (side in c("lower", "upper")) {
    b <- sides[[side]]
    if (is.null(b)) next
    d <- .reduceAnchorDates(ctx, cons, b$anchor, field) + as.integer(b$offset)
    out$defined <- out$defined & !is.na(d)
    if (identical(side, "lower")) out$lo <- d else out$hi <- d
  }
  out
}

# The SQL bounds of a window a stream can apply exactly in one roster
# reference: sides anchored on roster columns or dates; a source-anchored side
# is NULL here (broadened) and applied in R.
.reduceSqlBounds <- function(ctx, cons, window, alias = "r") {
  sides <- .reduceWindowSides(window)
  partial <- list()
  for (side in c("lower", "upper")) {
    b <- sides[[side]]
    if (is.null(b) || identical(.anchorNormalize(b$anchor)$kind, "source")) next
    partial[[if (identical(side, "lower")) "start" else "end"]] <- b
  }
  .compileWindow(ctx, if (length(partial)) partial else NULL, cons$roster, "final", alias = alias)
}

# Keep the rows inside each roster row's exact window (start inside, or for
# courses [start, end] overlapping); rows of undefined windows are dropped.
.reduceWindowFilter <- function(rows, bounds, overlap = FALSE) {
  if (!nrow(rows)) return(rows)
  i <- match(as.character(rows$row_key), as.character(bounds$key))
  lo <- bounds$lo[i]
  hi <- bounds$hi[i]
  ok <- !is.na(i) & bounds$defined[i] %in% TRUE
  s <- rows$start_date
  e <- if (isTRUE(overlap)) rows$end_date else s
  ok <- ok & (is.na(lo) | (!is.na(e) & e >= lo)) & (is.na(hi) | (!is.na(s) & s <= hi))
  rows[ok, , drop = FALSE]
}

# --- event streams -----------------------------------------------------------------------------

#' Build (and register) the event stream of one variable
#'
#' Rows of the source joined to each roster row through the consumer
#' association, restricted in SQL to the window bounds that are roster
#' columns or dates (the exact window, source anchors included, is applied in
#' R); projects the source's ordering columns and the requested extra
#' expressions. The roster is referenced once.
#'
#' @param ctx Compilation context.
#' @param cons Consumer.
#' @param src Resolved source.
#' @param window Window or NULL.
#' @param name Allocated fragment name.
#' @param extra Named list: column -> list(expr (SQL over e/r), type, role).
#' @param overlap Logical: select rows whose [start, end] overlaps the window
#'   (courses) instead of rows starting in it.
#' @param use Course or recorded/effective rows of an interval.
#' @param coverage Coverage relation for interval rows.
#' @param quality Apply the direct-event coverage quality policy.
#' @return The stream fragment.
#' @keywords internal
.reduceStream <- function(ctx, cons, src, window, name, extra = list(), overlap = FALSE,
                          use = "course", coverage = "recorded", quality = FALSE) {
  existing <- .fragmentGet(ctx, name)
  if (!is.null(existing)) return(existing)
  h <- .compileDialect(ctx)
  spec <- cons$spec
  win <- .reduceSqlBounds(ctx, cons, window, "r")
  tw <- .reduceSqlBounds(ctx, cons, cons$time_window, "r")
  assoc <- .reduceAssociation(ctx, src, cons)
  source <- .reduceSourceFrom(ctx, src, assoc, "e", use, coverage)
  pred <- if (isTRUE(overlap)) {
    c(if (!is.null(win$upper)) paste0("e.start_date <= ", win$upper),
      if (!is.null(win$lower)) paste0("e.end_date >= ", win$lower))
  } else {
    c(if (!is.null(win$lower)) paste0("e.start_date >= ", win$lower),
      if (!is.null(win$upper)) paste0("e.start_date <= ", win$upper))
  }
  tw_pred <- c(if (!is.null(tw$lower)) paste0("e.start_date >= ", tw$lower),
               if (!is.null(tw$upper)) paste0("e.start_date <= ", tw$upper))
  on <- c(source$predicates, "e.start_date IS NOT NULL", pred, tw_pred,
          if (quality && identical(src$kind, "event")) .eventCoverageQualitySql(h, src$meta, "e"))
  f <- source$fragment
  ordered_src <- src
  ordered_src$fragment <- f
  ord <- .reduceSourceOrdering(ordered_src)
  keep <- unique(c("event_id", "source_ns", setdiff(ord, "start_date")))
  keep <- keep[keep %in% names(f$columns)]
  cols <- c(row_key = paste0("r.", spec$key), person_id = paste0("r.", spec$person),
            start_date = "e.start_date", end_date = "e.end_date")
  for (k in keep) cols[[k]] <- paste0("e.", k)
  for (x in names(extra)) cols[[x]] <- extra[[x]]$expr
  body <- paste0("SELECT ", paste(paste0(cols, " AS ", names(cols)), collapse = ", "),
                 " FROM ", spec$name, " r INNER JOIN ", source$from,
                 " ON ", paste(on, collapse = " AND "))
  spec_cols <- list(row_key = .fragmentColumn(spec$key_type, FALSE, "private"),
                    person_id = .fragmentColumn(spec$person_type, FALSE, "person_key"),
                    start_date = .fragmentColumn("date", TRUE, "start"),
                    end_date = .fragmentColumn("date", TRUE, "end"))
  for (k in keep) spec_cols[[k]] <- .fragmentColumn(f$columns[[k]]$type, TRUE, "private")
  for (x in names(extra)) {
    spec_cols[[x]] <- .fragmentColumn(extra[[x]]$type, TRUE, extra[[x]]$role %||% "measure")
  }
  private <- names(spec_cols)[vapply(spec_cols, function(c) c$role == "private", logical(1))]
  out <- .fragment(name, "stream", "row", body, spec_cols,
                   dependencies = unique(c(spec$name, source$dependencies)),
                   person_lineage = list(person_col = "person_id", via_roster = NULL),
                   roster_namespace = spec$namespace, private_columns = private,
                   availability = f$availability, ordering = c("row_key", ord))
  .sqlAssertTypedSql(h, out$select_body)
  .fragmentRegister(ctx, out)
}

#' Rows of a variable's stream inside each roster row's exact window
#'
#' @param ctx Compilation context.
#' @param cons Consumer.
#' @param stream The stream fragment.
#' @param window The variable's window.
#' @param field JSON path.
#' @param overlap Logical (courses).
#' @return list(rows = filtered rows, bounds = .reduceBoundsR() of the window
#'   (with the output's time window folded into \code{defined})).
#' @keywords internal
.reduceStreamRows <- function(ctx, cons, stream, window, field, overlap = FALSE) {
  rows <- .reduceFetch(ctx, stream, order = c("row_key", "start_date"), field = field)
  bounds <- .reduceBoundsR(ctx, cons, window, field)
  rows <- .reduceWindowFilter(rows, bounds, overlap)
  if (!is.null(cons$time_window)) {
    tw <- .reduceBoundsR(ctx, cons, cons$time_window, field)
    rows <- .reduceWindowFilter(rows, tw, FALSE)
    bounds$defined <- bounds$defined & tw$defined
  }
  list(rows = rows, bounds = bounds)
}

# --- reducer kernels (pure R) ---------------------------------------------------------------------

# Orderable ranks for lossless BIGINT keys. Decimal strings are compared by
# sign and digit length before byte order, never converted to a double.
.reduceKeyOrder <- function(x) {
  if (inherits(x, "integer64")) x <- as.character(x)
  if (!is.character(x) && !is.factor(x)) return(as.numeric(x))
  x <- as.character(x)
  have <- !is.na(x)
  if (!any(have)) return(rep(NA_integer_, length(x)))
  if (!all(grepl("^[+-]?[0-9]+$", x[have]))) {
    return(match(x, sort(unique(x[have]), method = "radix")))
  }
  digits <- sub("^[+-]", "", x)
  digits <- sub("^0+(?=.)", "", digits, perl = TRUE)
  negative <- have & startsWith(x, "-") & digits != "0"
  normalized <- ifelse(negative, paste0("-", digits), digits)
  neg <- unique(normalized[negative])
  pos <- unique(normalized[have & !negative])
  neg <- neg[order(nchar(neg), neg, decreasing = TRUE, method = "radix")]
  pos <- pos[order(nchar(pos), pos, method = "radix")]
  match(normalized, c(neg, pos))
}

.reduceOrderRows <- function(rows, columns, decreasing = FALSE) {
  keys <- c("row_key", "cohort_row_id", "person_id", "event_id", "visit_id", "era_no", "parent_event_id")
  cols <- lapply(columns, function(col) {
    x <- rows[[col]]
    if (sub("^src_", "", col) %in% keys) .reduceKeyOrder(x) else x
  })
  do.call(order, c(cols, list(decreasing = decreasing, method = "radix")))
}

# Group indices of rows by key, in key order of `keys` (missing keys absent).
.reduceGroups <- function(row_keys, keys) {
  split(seq_along(row_keys), factor(as.character(row_keys), levels = as.character(keys)))
}

# Kleene OR / AND over 1 / 0 / NA truth values (empty: any 0, all NA).
.reduceKleeneAny <- function(x) {
  if (!length(x)) return(0L)
  if (any(x %in% 1L)) return(1L)
  if (anyNA(x)) return(NA_integer_)
  0L
}
.reduceKleeneAll <- function(x) {
  if (!length(x)) return(NA_integer_)
  if (any(x %in% 0L)) return(0L)
  if (anyNA(x)) return(NA_integer_)
  1L
}

# Mode with ties resolved to the smallest value (NULLs ignored).
.reduceMode <- function(v, value_type = NULL) {
  v <- v[!is.na(v)]
  if (!length(v)) return(v[NA_integer_])
  vals <- unique(v)
  counts <- tabulate(match(v, vals), nbins = length(vals))
  tied <- vals[counts == max(counts)]
  # A BIGINT stream can arrive as decimal text. Its declared type, rather
  # than the values another person's rows put in the transport column,
  # determines numeric ordering; genuine text keeps lexical ordering.
  ordering <- if (isTRUE(value_type %in% c("integer", "bigint"))) .reduceKeyOrder(tied) else tied
  tied[[order(ordering, method = "radix")[[1L]]]]
}

# Per-day least-squares slope of v on dates (>= 2 distinct dates).
.reduceSlope <- function(d, v) {
  ok <- !is.na(d) & !is.na(v)
  d <- as.numeric(d[ok])
  v <- as.numeric(v[ok])
  if (length(unique(d)) < 2L) return(NA_real_)
  md <- mean(d)
  sum((d - md) * (v - mean(v))) / sum((d - md)^2)
}

# Whole months from an earlier to a later date (v1 time_since semantics).
.reduceMonths <- function(from, to) {
  a <- as.POSIXlt(from)
  b <- as.POSIXlt(to)
  as.integer((b$year - a$year) * 12L + (b$mon - a$mon) - as.integer(b$mday < a$mday))
}

# Integer-valued accumulated durations must not silently become NA when
# their sum exceeds R's declared integer output range (as for supply_days).
.reduceIntegerTotal <- function(x, field = "/reducer") {
  total <- sum(as.numeric(x))
  if (!is.finite(total) || abs(total) > .Machine$integer.max) {
    .compileReject("a reducer duration exceeds the supported integer range", field = field)
  }
  as.integer(total)
}

#' Apply one reducer to the ordered rows of one roster row
#'
#' @param fn Reducer name.
#' @param x Named list of the row's column vectors in contract order
#'   (\code{start_date}, \code{end_date}, \code{v}, \code{visit_id},
#'   \code{concept_id}, \code{pred}, \code{range_low}, \code{range_high}).
#' @param r The reducer node (defaults applied).
#' @param anchor Anchor date of the row (Date or NA): the window's anchor for
#'   first_date, last_date and closest_value; the reference anchor for
#'   days_since / days_until.
#' @param value_type Declared value column type, including lossless BIGINT.
#' @return One value (a vector for nth_value with a vector n).
#' @keywords internal
.reduceOne <- function(fn, x, r, anchor = NA, value_type = NULL) {
  d <- x[["start_date"]]
  n <- length(d)
  v <- x[["v"]]
  skip <- identical(r[["missing"]] %||% "skip", "skip")
  candidates <- function() {
    rows <- seq_len(n)
    if (skip) rows <- rows[!is.na(v[rows])]
    rows
  }
  numeric_v <- function() {
    vv <- suppressWarnings(as.numeric(v))
    vv[!is.na(vv)]
  }
  distinct_count <- function(by) switch(by,
    row = as.integer(n),
    day = length(unique(d[!is.na(d)])),
    visit = { vi <- x[["visit_id"]]; length(unique(vi[!is.na(vi)])) },
    concept = { ci <- x[["concept_id"]]; length(unique(ci[!is.na(ci)])) })
  switch(fn,
    binary = as.integer(n > 0L),
    count = distinct_count(r[["distinct"]] %||% "row"),
    n_distinct = if (!is.null(r[["value"]])) length(unique(v[!is.na(v)])) else
      distinct_count(r[["distinct"]] %||% "concept"),
    first_value = { rows <- candidates(); if (length(rows)) as.numeric(v[[rows[[1L]]]]) else NA_real_ },
    last_value = { rows <- candidates(); if (length(rows)) as.numeric(v[[rows[[length(rows)]]]]) else NA_real_ },
    nth_value = {
      rows <- candidates()
      vapply(as.integer(unlist(r[["n"]] %||% 1L)), function(k) {
        if (k >= 1L && k <= length(rows)) as.numeric(v[[rows[[k]]]]) else NA_real_
      }, numeric(1))
    },
    closest_value = {
      if (is.na(anchor)) return(NA_real_)
      rows <- candidates()
      dd <- as.integer(d[rows]) - as.integer(anchor)
      keep <- switch(r[["side"]] %||% "either", before = dd <= 0L, after = dd >= 0L,
                     rep(TRUE, length(rows)))
      keep[is.na(keep)] <- FALSE
      rows <- rows[keep]
      if (!length(rows)) return(NA_real_)
      dist <- abs(as.integer(d[rows]) - as.integer(anchor))
      as.numeric(v[[rows[[which(dist == min(dist))[[1L]]]]]])
    },
    mean = { vv <- numeric_v(); if (length(vv)) mean(vv) else NA_real_ },
    min = { vv <- numeric_v(); if (length(vv)) min(vv) else NA_real_ },
    max = { vv <- numeric_v(); if (length(vv)) max(vv) else NA_real_ },
    sum = { vv <- numeric_v(); if (length(vv)) sum(vv) else 0 },
    sd = { vv <- numeric_v(); if (length(vv) >= 2L) stats::sd(vv) else NA_real_ },
    cv = {
      vv <- numeric_v()
      if (length(vv) < 2L) return(NA_real_)
      m <- mean(vv)
      if (m == 0) NA_real_ else stats::sd(vv) / m * 100
    },
    median = { vv <- numeric_v(); if (length(vv)) stats::median(vv) else NA_real_ },
    quantile = {
      vv <- numeric_v()
      if (!length(vv)) return(NA_real_)
      as.numeric(stats::quantile(vv, probs = as.numeric(unlist(r[["p"]])), type = 7L,
                                 names = FALSE))
    },
    mode = .reduceMode(v, value_type),
    slope = .reduceSlope(d, v),
    any = .reduceKleeneAny(as.integer(x[["pred"]])),
    all = .reduceKleeneAll(as.integer(x[["pred"]])),
    first_date = if (!n || is.na(anchor) || is.na(d[[1L]])) NA_integer_ else
      as.integer(d[[1L]] - anchor),
    last_date = if (!n || is.na(anchor) || is.na(d[[n]])) NA_integer_ else
      as.integer(d[[n]] - anchor),
    days_since = {
      if (is.na(anchor)) return(NA_integer_)
      dd <- d[!is.na(d) & d <= anchor]
      if (!length(dd)) NA_integer_ else as.integer(anchor - max(dd))
    },
    days_until = {
      if (is.na(anchor)) return(NA_integer_)
      dd <- d[!is.na(d) & d >= anchor]
      if (!length(dd)) NA_integer_ else as.integer(min(dd) - anchor)
    },
    time_since = {
      ref <- as.Date(as.character(unlist(r[["reference_date"]])))
      dd <- d[!is.na(d) & d <= ref]
      if (!length(dd)) return(NA_integer_)
      latest <- max(dd)
      if (identical(r[["unit"]] %||% "day", "month")) .reduceMonths(latest, ref) else
        as.integer(ref - latest)
    },
    duration_sum = {
      dur <- as.numeric(x[["end_date"]] - d)
      dur <- dur[!is.na(dur)]
      if (length(dur)) .reduceIntegerTotal(dur) else NA_integer_
    },
    drug_duration = {
      dur <- as.numeric(x[["end_date"]] - d)
      dur <- dur[!is.na(dur)]
      if (!length(dur)) return(NA_real_)
      switch(r[["agg"]] %||% "mean", mean = mean(dur), sum = sum(dur), max = max(dur))
    },
    gap_max = , gap_mean = {
      dd <- sort(unique(d[!is.na(d)]))
      if (length(dd) < 2L) return(NA_real_)
      gaps <- as.numeric(diff(dd))
      if (identical(fn, "gap_max")) max(gaps) else mean(gaps)
    },
    abnormal_high = {
      vv <- suppressWarnings(as.numeric(v))
      rh <- x[["range_high"]]
      as.integer(sum(!is.na(vv) & !is.na(rh) & vv > rh))
    },
    abnormal_low = {
      vv <- suppressWarnings(as.numeric(v))
      rl <- x[["range_low"]]
      as.integer(sum(!is.na(vv) & !is.na(rl) & vv < rl))
    },
    stop("Unsupported reducer '", fn, "'.", call. = FALSE))
}

# The value of an empty, defined, available input (section 2.7).
.reduceEmptyValue <- function(fn) {
  switch(fn,
    binary = 0L, count = 0L, n_distinct = 0L, sum = 0, abnormal_high = 0L,
    abnormal_low = 0L, any = 0L, era_count = 0L, covered_days = 0L,
    supply_days = 0L,
    NA)
}

# Column type of a reducer's values.
.reduceValueType <- function(fn, value_type = "numeric") {
  if (fn %in% .REDUCER_INTEGER_FNS) return("integer")
  if (identical(fn, "mode")) return(value_type)
  "numeric"
}

#' Apply a reducer to every roster key
#'
#' @param fn Reducer name.
#' @param rows Stream rows (sorted here by key, then the source's ordering
#'   columns, in byte order).
#' @param keys Roster row keys.
#' @param r Reducer node.
#' @param ordering Ordering columns of the source.
#' @param anchors NULL or a Date vector aligned with keys.
#' @param value_type Declared value column type, including lossless BIGINT.
#' @return A list of value vectors (one per output column) aligned with keys.
#' @keywords internal
.reduceApply <- function(fn, rows, keys, r, ordering, anchors = NULL, value_type = NULL) {
  ord_cols <- c("row_key", intersect(ordering, names(rows)))
  if (nrow(rows)) {
    o <- .reduceOrderRows(rows, ord_cols)
    rows <- rows[o, , drop = FALSE]
  }
  groups <- .reduceGroups(rows$row_key, keys)
  vec_cols <- intersect(c("start_date", "end_date", "v", "visit_id", "concept_id", "pred",
                          "range_low", "range_high"), names(rows))
  cols <- lapply(stats::setNames(vec_cols, vec_cols), function(cn) rows[[cn]])
  nvec <- if (identical(fn, "nth_value")) length(unlist(r[["n"]] %||% 1L)) else 1L
  out <- lapply(seq_len(nvec), function(i) vector("list", length(keys)))
  always <- fn %in% c("closest_value", "first_date", "last_date", "days_since", "days_until",
                      "nth_value", "time_since", "mode", "first_value", "last_value", "mean",
                      "min", "max", "sd", "cv", "median", "quantile", "slope", "all",
                      "duration_sum", "drug_duration", "gap_max", "gap_mean")
  empty <- .reduceEmptyValue(fn)
  for (i in seq_along(keys)) {
    ix <- groups[[i]]
    anchor <- if (!is.null(anchors)) anchors[[i]] else NA
    val <- if (!length(ix) && !always) empty else {
      .reduceOne(fn, lapply(cols, function(col) col[ix]), r, anchor, value_type)
    }
    if (nvec == 1L) out[[1L]][[i]] <- val else for (j in seq_len(nvec)) out[[j]][[i]] <- val[[j]]
  }
  lapply(out, function(x) {
    x[vapply(x, is.null, logical(1))] <- list(NA)
    x
  })
}

# --- variable compilation ---------------------------------------------------------------------

#' Compile one variable over a roster (frozen compiler signature, section 6)
#'
#' @param ctx Compilation context (\code{.compileContext()}).
#' @param node The IR variable node with its plan name in
#'   \code{attr(node, "name")}.
#' @param roster The final roster fragment of the consuming output.
#' @param anchors The consumer record (\code{.reduceConsumer()}): the
#'   output, its roster and coverage, through which every anchor and window of
#'   the variable is resolved.
#' @return list(stream = the variable's stream fragment (NULL for roster-level
#'   values), reducer = the pure kernel, declared_empty_value, columns =
#'   named list of \code{.fragmentColumn()} of its output columns, evaluate =
#'   function() returning data.frame(row_key, <columns>) for every roster row,
#'   available = logical).
#' @keywords internal
.compileVariable <- function(ctx, node, roster, anchors) {
  name <- .compileNodeName(node, "variable")
  cons <- anchors
  if (!identical(cons$roster$name, roster$name)) {
    stop("The variable consumer belongs to another roster.", call. = FALSE)
  }
  if (!is.null(node[["source"]])) return(.reduceSourceVariable(ctx, node, name, cons))
  if (!is.null(node[["derived"]])) return(.reduceDerivedVariable(ctx, node, name, cons))
  if (!is.null(node[["raw"]])) return(.reduceRawVariable(ctx, node, name, cons))
  if (!is.null(node[["population_column"]])) return(.reducePopulationColumn(ctx, node, name, cons))
  .irReject("a variable is a source reducer, a derived, raw or population column",
            field = paste0("/variables/", name))
}

# Output columns of a variable (nth_value with a vector n is suffixed).
.reduceColumnNames <- function(name, r) {
  if (identical(r[["fn"]], "nth_value")) {
    n <- as.integer(unlist(r[["n"]] %||% 1L))
    if (length(n) > 1L || (is.list(r[["n"]]) && is.null(names(r[["n"]])))) {
      return(paste0(name, "_", n))
    }
  }
  name
}

.reduceTypedNA <- function(type, n) {
  switch(type, integer = rep(NA_integer_, n), bigint = rep(NA_real_, n),
         numeric = rep(NA_real_, n), text = rep(NA_character_, n),
         date = as.Date(rep(NA_character_, n)), boolean = rep(NA, n), rep(NA, n))
}

.reduceCoerce <- function(x, type) {
  if (is.list(x)) x <- unlist(lapply(x, function(e) if (is.null(e) || length(e) == 0L) NA else e))
  if (is.null(x)) x <- .reduceTypedNA(type, 0L)
  switch(type,
    integer = suppressWarnings(as.integer(x)),
    numeric = { y <- suppressWarnings(as.numeric(x)); y[!is.finite(y)] <- NA_real_; y },
    text = as.character(x),
    bigint = if (is.character(x)) x else if (inherits(x, "integer64")) as.character(x) else x,
    date = if (is.numeric(x)) as.Date(x, origin = "1970-01-01") else as.Date(x),
    x)
}

# A variable record whose every value is NA (unavailable source).
.reduceUnavailable <- function(ctx, cons, name, columns) {
  list(stream = NULL, reducer = NULL, declared_empty_value = NA, columns = columns,
       available = FALSE,
       evaluate = function() {
         keys <- .reduceRosterFrame(ctx, cons$roster)$cohort_row_id
         out <- data.frame(row_key = keys, stringsAsFactors = FALSE)
         for (col in names(columns)) out[[col]] <- .reduceTypedNA(columns[[col]]$type, length(keys))
         out
       })
}

# A source variable: event-row reducers, era reducers and the interval
# kernels.
.reduceSourceVariable <- function(ctx, node, name, cons) {
  r <- node[["reducer"]]
  fn <- r[["fn"]]
  field <- paste0("/variables/", name, "/reducer")
  src <- .reduceSource(ctx, node[["source"]])
  if (fn %in% c("covered_days", "supply_days")) {
    return(.reduceIntervalVariable(ctx, node, name, cons, src))
  }
  if (fn %in% c("era_count", "era_days", "first_era_days")) {
    return(.reduceEraVariable(ctx, node, name, cons, src))
  }
  value_col <- NULL
  value_type <- "numeric"
  needs_value <- fn %in% .REDUCER_VALUE_FNS || identical(fn, "mode") ||
    (identical(fn, "n_distinct") && !is.null(r[["value"]]))
  if (needs_value) {
    requested <- r[["value"]] %||% if (identical(fn, "mode")) "concept_id" else "value"
    res <- .predicateResolve(src$catalogue, requested, paste0(field, "/value"))
    value_col <- res$column
    value_type <- if (res$type %in% c("integer", "bigint")) res$type else
      if (identical(res$type, "text")) "text" else if (identical(res$type, "date")) "date" else
        "numeric"
    if (fn %in% .REDUCER_VALUE_FNS && !res$type %in% c("integer", "bigint", "numeric")) {
      .irReject(paste0("reducer '", fn, "' needs a numeric value column; '", requested, "' is ",
                       res$type), field = paste0(field, "/value"))
    }
  }
  type <- .reduceValueType(fn, value_type)
  out_names <- .reduceColumnNames(name, r)
  columns <- stats::setNames(lapply(out_names, function(c) .fragmentColumn(type, TRUE, "measure")),
                             out_names)
  if (!src$available) return(.reduceUnavailable(ctx, cons, name, columns))
  window <- r[["window"]]
  h <- .compileDialect(ctx)
  extra <- list()
  if (!is.null(value_col)) {
    extra$v <- list(expr = paste0("e.", value_col),
                    type = value_type)
  }
  if (fn %in% c("count", "n_distinct")) {
    if ("visit_id" %in% names(src$fragment$columns) && !"visit_id" %in% names(extra)) {
      extra$visit_id <- list(expr = "e.visit_id", type = "bigint", role = "private")
    }
    if ("concept_id" %in% names(src$fragment$columns)) {
      extra$concept_id <- list(expr = "e.concept_id", type = "integer")
    }
  }
  if (fn %in% c("abnormal_high", "abnormal_low")) {
    col <- if (identical(fn, "abnormal_high")) "range_high" else "range_low"
    .predicateResolve(src$catalogue, col, field)
    extra[[col]] <- list(expr = paste0("e.", col), type = "numeric")
  }
  if (fn %in% c("any", "all")) {
    pred <- .predicateSql(h, r[["predicate"]], src$catalogue, "e", paste0(field, "/predicate"))
    extra$pred <- list(expr = paste0("CASE WHEN (", pred, ") THEN 1 WHEN NOT (", pred,
                                     ") THEN 0 END"), type = "integer")
  }
  anchor_needed <- fn %in% c("first_date", "last_date", "closest_value", "days_since",
                             "days_until")
  anchor <- if (fn %in% c("days_since", "days_until")) r[["reference"]] else .reduceAnchor(r)
  if (anchor_needed) .anchorNormalize(anchor, paste0(field, "/reference"))
  stream <- .reduceStream(ctx, cons, src, window, .reduceName(ctx, cons, "s", name),
                          extra = extra)
  ordering <- .reduceSourceOrdering(src)
  kernel <- function(rows, keys, anchors = NULL) {
    .reduceApply(fn, rows, keys, r, ordering, anchors, value_type)
  }
  evaluate <- function() {
    roster <- .reduceRosterFrame(ctx, cons$roster)
    keys <- roster$cohort_row_id
    got <- .reduceStreamRows(ctx, cons, stream, window, field)
    rows <- .reduceTemporal(got$rows, cons$temporal, ordering)
    anchors <- if (anchor_needed) .reduceAnchorDates(ctx, cons, anchor, field) else NULL
    vals <- kernel(rows, keys, anchors)
    defined <- got$bounds$defined
    out <- data.frame(row_key = keys, stringsAsFactors = FALSE)
    for (j in seq_along(columns)) {
      x <- .reduceCoerce(vals[[j]], columns[[j]]$type)
      x[!defined %in% TRUE] <- .reduceTypedNA(columns[[j]]$type, 1L)
      out[[names(columns)[[j]]]] <- x
    }
    out
  }
  list(stream = stream, reducer = kernel, declared_empty_value = .reduceEmptyValue(fn),
       columns = columns, evaluate = evaluate, available = TRUE)
}

# The output-level temporal selection (event_select first / last, min_gap)
# applied per roster row to its ordered rows (section 2.9 `temporal`).
.reduceTemporal <- function(rows, temporal, ordering) {
  if (is.null(temporal) || !nrow(rows)) return(rows)
  sel <- temporal[["event_select"]] %||% "all"
  gap <- as.integer(unlist(temporal[["min_gap"]] %||% 0L))
  if (identical(sel, "all") && gap <= 0L) return(rows)
  ord_cols <- c("row_key", intersect(ordering, names(rows)))
  o <- .reduceOrderRows(rows, ord_cols)
  rows <- rows[o, , drop = FALSE]
  keep <- rep(FALSE, nrow(rows))
  for (ix in split(seq_len(nrow(rows)), as.character(rows$row_key))) {
    take <- ix
    if (gap > 0L) {
      kept <- integer(0)
      last <- NA
      for (i in take) {
        if (is.na(last) || as.integer(rows$start_date[[i]] - last) >= gap) {
          kept <- c(kept, i)
          last <- rows$start_date[[i]]
        }
      }
      take <- kept
    }
    if (identical(sel, "first")) take <- utils::head(take, 1L)
    if (identical(sel, "last")) take <- utils::tail(take, 1L)
    keep[take] <- TRUE
  }
  rows[keep, , drop = FALSE]
}

# covered_days / supply_days through the WS-5 executable kernels, under names
# allocated here (A-20 (e)); an unavailable source is NA without a query.
.reduceIntervalVariable <- function(ctx, node, name, cons, src) {
  r <- node[["reducer"]]
  fn <- r[["fn"]]
  field <- paste0("/variables/", name, "/reducer")
  columns <- stats::setNames(list(.fragmentColumn("integer", TRUE, "measure")), name)
  if (identical(fn, "covered_days")) {
    available <- .reducerSourceAvailability(ctx, node[["source"]], r[["coverage"]] %||% "recorded",
                                            path = paste0(field, "/coverage"))
  } else {
    if (!identical(src$kind, "event")) {
      .irReject("supply_days reads the days_supply role of an event source", field = field)
    }
    available <- src$fragment$availability
    if (identical(available, "available") && !"supply" %in% src$meta$roles) {
      .irReject("supply_days needs a source with the days_supply role", field = field)
    }
  }
  if (!identical(available, "available")) return(.reduceUnavailable(ctx, cons, name, columns))
  if (isTRUE(r[["observed_only"]]) && is.null(cons$coverage)) {
    .irReject("covered_days(observed_only = TRUE) needs a population with observation coverage",
              field = paste0(field, "/observed_only"))
  }
  allocated <- list(bounds = .reduceName(ctx, cons, "b", name),
                    stream = .reduceName(ctx, cons, "s", name))
  evaluate <- function() {
    roster <- .reduceRosterFrame(ctx, cons$roster)
    v <- if (!is.null(cons$time_window) || !is.null(cons$temporal)) {
      .reduceSelectedCoverage(ctx, cons, src, r, allocated$stream, field)
    } else .intervalReducerValues(ctx, fn, cons$roster, "final", source = node[["source"]],
                                window = .reduceWindow(r[["window"]]),
                                coverage = r[["coverage"]] %||% "recorded",
                                observed_only = isTRUE(r[["observed_only"]]),
                                fills = r[["fills"]] %||% "start_in_window",
                                cov = cons$coverage, names = allocated, path = field)
    out <- data.frame(row_key = roster$cohort_row_id, stringsAsFactors = FALSE)
    out[[name]] <- as.integer(v$value[match(as.character(roster$cohort_row_id),
                                            as.character(v$row_key))])
    out
  }
  list(stream = NULL, reducer = NULL, declared_empty_value = 0L, columns = columns,
       evaluate = evaluate, available = TRUE)
}

# Intersect two consumer windows without dropping missing-anchor information.
.reduceIntersectBounds <- function(left, right) {
  at <- match(as.character(left$key), as.character(right$key))
  for (side in c("lo", "hi")) {
    a <- left[[side]]
    b <- right[[side]][at]
    both <- !is.na(a) & !is.na(b)
    a[is.na(a)] <- b[is.na(a)]
    a[both] <- if (identical(side, "lo")) pmax(a[both], b[both]) else pmin(a[both], b[both])
    left[[side]] <- a
  }
  left$defined <- left$defined & !is.na(at) & right$defined[at] %in% TRUE
  left
}

# Output-wide restrictions select the same recorded/effective fill rows as
# the ordinary WS-5 kernel. Temporal selection precedes the union or supply
# sum, and observation still uses the component's actual recorded segments.
.reduceSelectedCoverage <- function(ctx, cons, src, reducer, stream_name, field) {
  bounds <- .reduceBoundsR(ctx, cons, reducer[["window"]], field)
  if (!is.null(cons$time_window)) {
    bounds <- .reduceIntersectBounds(bounds, .reduceBoundsR(ctx, cons, cons$time_window, field))
  }
  fn <- reducer[["fn"]]
  fills <- reducer[["fills"]] %||% "start_in_window"
  extra <- if (identical(fn, "supply_days")) {
    list(supply = list(expr = "e.supply", type = "numeric"),
         end_recorded = list(expr = "e.end_recorded", type = "date"))
  } else list()
  unrestricted <- cons
  unrestricted$time_window <- NULL
  # Push a usable window into SQL before bounded realisation; the exact
  # intersection and any source-anchored sides are still applied below.
  prefilter <- .reduceWindow(reducer[["window"]]) %||% cons$time_window
  if (identical(fn, "supply_days") && identical(fills, "clipped") && !is.null(prefilter)) {
    prefilter$start <- NULL
  }
  stream <- .reduceStream(ctx, unrestricted, src, prefilter, stream_name, extra = extra,
                           overlap = identical(fn, "covered_days"), use = "rows",
                           coverage = reducer[["coverage"]] %||% "recorded", quality = TRUE)
  rows <- .reduceFetch(ctx, stream, order = c("row_key", "start_date"), field = field)
  select_bounds <- bounds
  if (identical(fn, "supply_days") && identical(fills, "clipped")) select_bounds$lo[] <- NA
  rows <- .reduceWindowFilter(rows, select_bounds, overlap = identical(fn, "covered_days"))
  rows <- .reduceTemporal(rows, cons$temporal, stream$ordering)
  observed <- NULL
  if (isTRUE(reducer[["observed_only"]])) {
    segments <- .compileObservedSegments(ctx, cons$coverage, cons$roster, "final")
    o <- .reduceFetch(ctx, segments, order = c("row_key", "start_date"), field = field)
    observed <- data.frame(key = o$row_key, start = o$start_date, end = o$end_date)
    covered <- .coverageCoveredKeys(ctx, cons$coverage, cons$roster, "final", field = field)
    bounds$defined <- bounds$defined & as.character(bounds$key) %in% as.character(covered)
  }
  value <- if (identical(fn, "covered_days")) {
    .coveredDays(data.frame(key = rows$row_key, start = rows$start_date, end = rows$end_date), bounds, observed)
  } else {
    .supplyDays(data.frame(key = rows$row_key, start = rows$start_date, supply = rows$supply,
                           end_recorded = rows$end_recorded), bounds, fills)
  }
  data.frame(row_key = bounds$key, value = value)
}

#' The coverage policy in force for a coverage-family reducer (A-20 (g))
#'
#' What explain states for \code{covered_days} and \code{supply_days}: the
#' relation read and, for a direct event source, the event's declared end
#' policy and the one data-quality predicate applied at the consumer (section
#' 6.7): a row needs a start; unless the end policy is \code{"start"}, its end
#' must not be before its start; under the \code{days_supply} fallback a
#' present supply that is the only end source must be a positive integer (a
#' NULL supply is a one-day record). An interval source reads its own
#' normalised relation (\code{recorded}, or \code{effective_supply} for a
#' stockpiled course), whose era policy is the interval's.
#' @param ir The IR.
#' @param variable The IR variable node.
#' @return Named list, or NULL for any other reducer.
#' @keywords internal
.reduceCoveragePolicy <- function(ir, variable) {
  r <- variable[["reducer"]]
  fn <- if (is.list(r)) r[["fn"]]
  if (!is.character(fn) || !fn %in% c("covered_days", "supply_days")) return(NULL)
  src <- variable[["source"]]
  kind <- if (is.list(src) && length(src)) names(src)[[1L]] else NA_character_
  out <- list(fn = fn, source = src)
  if (identical(fn, "supply_days")) out$fills <- r[["fills"]] %||% "start_in_window"
  if (identical(fn, "covered_days")) out$observed_only <- isTRUE(r[["observed_only"]])
  if (identical(kind, "interval")) {
    out$relation <- if (identical(fn, "covered_days")) r[["coverage"]] %||% "recorded" else "recorded"
    return(out)
  }
  ev <- ir[["events"]][[unlist(src[[1L]])]] %||% list()
  end <- ev[["end"]]
  policy <- if (is.null(end)) "recorded_end" else if (identical(unlist(end), "start")) "start" else
    if (is.list(end) && identical(end[["fallback"]], "days_supply")) "recorded_end_then_days_supply" else "recorded_end"
  out$relation <- "recorded"
  out$end_policy <- policy
  out$quality <- c("start_present",
                   if (!identical(policy, "start")) "end_not_before_start",
                   if (identical(policy, "recorded_end_then_days_supply")) "supply_positive_integer_when_only_end_source")
  out
}

# era_count / era_days / first_era_days over the course rows overlapping the
# window (section 2.7: the course envelope, clipped to the window).
.reduceEraVariable <- function(ctx, node, name, cons, src) {
  r <- node[["reducer"]]
  fn <- r[["fn"]]
  field <- paste0("/variables/", name, "/reducer")
  if (!identical(src$kind, "interval")) {
    .irReject(paste0("reducer '", fn, "' reads the course of an interval source"), field = field)
  }
  columns <- stats::setNames(list(.fragmentColumn("integer", TRUE, "measure")), name)
  if (!src$available) return(.reduceUnavailable(ctx, cons, name, columns))
  window <- r[["window"]]
  stream <- .reduceStream(ctx, cons, src, window, .reduceName(ctx, cons, "s", name), overlap = TRUE)
  ordering <- stream$ordering
  kernel <- function(rows, keys, bounds) {
    i <- match(as.character(rows$row_key), as.character(bounds$key))
    lo <- .dayNumber(bounds$lo)[i]
    hi <- .dayNumber(bounds$hi)[i]
    s <- .dayNumber(rows$start_date)
    e <- .dayNumber(rows$end_date)
    cs <- ifelse(is.na(lo), s, pmax(s, lo))
    ce <- ifelse(is.na(hi), e, pmin(e, hi))
    len <- pmax(0L, ce - cs + 1L)
    o <- if (nrow(rows)) {
      .reduceOrderRows(rows, c("row_key", intersect(ordering, names(rows))))
    } else integer(0)
    groups <- .reduceGroups(rows$row_key[o], keys)
    vapply(seq_along(keys), function(k) {
      ix <- o[groups[[k]]]
      switch(fn,
        era_count = length(ix),
        era_days = if (length(ix)) .reduceIntegerTotal(len[ix], field) else NA_integer_,
        first_era_days = if (length(ix)) .reduceIntegerTotal(len[ix[[1L]]], field) else NA_integer_)
    }, integer(1))
  }
  evaluate <- function() {
    roster <- .reduceRosterFrame(ctx, cons$roster)
    keys <- roster$cohort_row_id
    got <- .reduceStreamRows(ctx, cons, stream, window, field, overlap = TRUE)
    rows <- .reduceTemporal(got$rows, cons$temporal, ordering)
    vals <- kernel(rows, keys, got$bounds)
    vals[!got$bounds$defined %in% TRUE] <- NA_integer_
    out <- data.frame(row_key = keys, stringsAsFactors = FALSE)
    out[[name]] <- vals
    out
  }
  list(stream = stream, reducer = kernel, declared_empty_value = .reduceEmptyValue(fn),
       columns = columns, evaluate = evaluate, available = TRUE)
}

# --- person attributes ----------------------------------------------------------------------------

# Qualified name of a CDM table of the handle.
.reduceTable <- function(ctx, table) {
  handle <- ctx[["handle"]]
  bp <- .buildBlueprint(handle)
  row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, , drop = FALSE]
  if (!nrow(row)) {
    .irReject(paste0("table '", table, "' is not available at this site"),
              code = "unavailable_table", field = "/variables")
  }
  .qualifyTable(handle, table, .resolveTableSchema(handle, table, row$schema_category[[1L]]))
}

.reduceTableColumns <- function(ctx, table) {
  bp <- .buildBlueprint(ctx[["handle"]])
  bp$columns[[table]]
}

#' The person row of every roster row (one fetch per output)
#' @keywords internal
.reducePersonFrame <- function(ctx, cons) {
  memo <- .compileMemo(ctx, "reduce_person")
  key <- cons$output
  if (exists(key, envir = memo, inherits = FALSE)) return(get(key, envir = memo, inherits = FALSE))
  h <- .compileDialect(ctx)
  spec <- cons$spec
  person <- .reduceTable(ctx, "person")
  col_df <- .reduceTableColumns(ctx, "person")
  have <- tolower(col_df$column_name)
  want <- c("year_of_birth", "month_of_birth", "day_of_birth", "gender_concept_id",
            "race_concept_id", "ethnicity_concept_id")
  cols <- vapply(want, function(c) if (c %in% have) paste0("p.", c) else .compileTypedNull(h, "integer"), "")
  name <- .reduceName(ctx, cons, "pp")
  body <- paste0("SELECT r.", spec$key, " AS row_key, ",
                 paste(paste0(cols, " AS ", want), collapse = ", "), " FROM ", spec$name,
                 " r LEFT JOIN ", person, " p ON p.person_id = r.", spec$person)
  spec_cols <- c(list(row_key = .fragmentColumn(spec$key_type, FALSE, "private")),
                 stats::setNames(lapply(want, function(c) .fragmentColumn("integer", TRUE, "measure")), want))
  f <- .fragment(name, "stream", "episode", body, spec_cols, dependencies = spec$name,
                 unique_key = "row_key",
                 person_lineage = list(person_col = NULL, via_roster = cons$roster$name),
                 roster_namespace = spec$namespace, private_columns = "row_key")
  .sqlAssertTypedSql(h, f$select_body)
  .fragmentRegister(ctx, f)
  rows <- .reduceFetch(ctx, f, order = "row_key", field = paste0("/outputs/", cons$output))
  assign(key, rows, envir = memo)
  rows
}

#' The first death date of every roster row's person
#' @keywords internal
.reduceDeathFrame <- function(ctx, cons) {
  memo <- .compileMemo(ctx, "reduce_death")
  key <- cons$output
  if (exists(key, envir = memo, inherits = FALSE)) return(get(key, envir = memo, inherits = FALSE))
  h <- .compileDialect(ctx)
  spec <- cons$spec
  role <- .roleOf(ctx[["handle"]], "death")
  name <- .reduceName(ctx, cons, "pd")
  body <- if (is.null(role)) {
    paste0("SELECT r.", spec$key, " AS row_key, ", .sqlNullDate(h), " AS death_date FROM ",
           spec$name, " r")
  } else {
    death <- .reduceTable(ctx, "death")
    paste0("SELECT r.", spec$key, " AS row_key, MIN(d.", role[["start"]], ") AS death_date FROM ",
           spec$name, " r LEFT JOIN ", death, " d ON d.", role[["person"]], " = r.", spec$person,
           " GROUP BY r.", spec$key)
  }
  f <- .fragment(name, "stream", "episode", body,
                 list(row_key = .fragmentColumn(spec$key_type, FALSE, "private"),
                      death_date = .fragmentColumn("date", TRUE, "start")),
                 dependencies = spec$name, unique_key = "row_key",
                 person_lineage = list(person_col = NULL, via_roster = cons$roster$name),
                 roster_namespace = spec$namespace, private_columns = "row_key")
  .sqlAssertTypedSql(h, f$select_body)
  .fragmentRegister(ctx, f)
  rows <- .reduceFetch(ctx, f, order = "row_key", field = paste0("/outputs/", cons$output))
  rows$available <- !is.null(role)
  assign(key, rows, envir = memo)
  rows
}

#' The valid observation periods of every roster row's person
#' @keywords internal
.reducePeriodFrame <- function(ctx, cons) {
  memo <- .compileMemo(ctx, "reduce_periods")
  key <- cons$output
  if (exists(key, envir = memo, inherits = FALSE)) return(get(key, envir = memo, inherits = FALSE))
  h <- .compileDialect(ctx)
  spec <- cons$spec
  op <- .coverageSource(ctx)
  name <- .reduceName(ctx, cons, "po")
  body <- paste0("SELECT r.", spec$key, " AS row_key, o.period_id AS period_id, ",
                 "o.start_date AS start_date, o.end_date AS end_date FROM ", spec$name,
                 " r INNER JOIN ", op$name, " o ON o.person_id = r.", spec$person)
  f <- .fragment(name, "stream", "row", body,
                 list(row_key = .fragmentColumn(spec$key_type, FALSE, "private"),
                      period_id = .fragmentColumn("bigint", FALSE, "private"),
                      start_date = .fragmentColumn("date", FALSE, "start"),
                      end_date = .fragmentColumn("date", FALSE, "end")),
                 dependencies = c(spec$name, op$name),
                 person_lineage = list(person_col = NULL, via_roster = cons$roster$name),
                 roster_namespace = spec$namespace, private_columns = c("row_key", "period_id"))
  .sqlAssertTypedSql(h, f$select_body)
  .fragmentRegister(ctx, f)
  rows <- .reduceFetch(ctx, f, order = c("row_key", "start_date", "period_id"),
                       field = paste0("/outputs/", cons$output))
  assign(key, rows, envir = memo)
  rows
}

# Concept names of a set of ids (translate_concepts; unknown -> "concept_<id>").
.reduceConceptNames <- function(ctx, ids) {
  out <- rep(NA_character_, length(ids))
  have <- !is.na(ids)
  if (!any(have)) return(out)
  concepts <- .vocabLookupConcepts(ctx[["handle"]], unique(ids[have]))
  map <- stats::setNames(as.character(concepts$concept_name), as.character(concepts$concept_id))
  name <- unname(map[as.character(ids[have])])
  name[is.na(name)] <- paste0("concept_", ids[have][is.na(name)])
  out[have] <- name
  out
}

.reduceTranslate <- function(ctx) {
  !isFALSE(unlist(ctx[["ir"]][["options"]][["translate_concepts"]]))
}

# --- derived variables ---------------------------------------------------------------------------

.reduceDerivedVariable <- function(ctx, node, name, cons) {
  d <- node[["derived"]]
  kind <- d[["kind"]]
  field <- paste0("/variables/", name, "/derived")
  if (kind %in% c("died", "days_to_death") && is.null(.roleOf(ctx[["handle"]], "death"))) {
    columns <- stats::setNames(list(.fragmentColumn("integer", TRUE, "measure")), name)
    return(.reduceUnavailable(ctx, cons, name, columns))
  }
  one <- function(type, fun) {
    columns <- stats::setNames(list(.fragmentColumn(type, TRUE, "measure")), name)
    list(stream = NULL, reducer = NULL, declared_empty_value = NA, columns = columns,
         available = TRUE,
         evaluate = function() {
           roster <- .reduceRosterFrame(ctx, cons$roster)
           out <- data.frame(row_key = roster$cohort_row_id, stringsAsFactors = FALSE)
           out[[name]] <- .reduceCoerce(fun(roster), type)
           out
         })
  }
  index_of <- function(roster) {
    if (!"cohort_start_date" %in% names(roster)) return(as.Date(rep(NA_character_, nrow(roster))))
    roster$cohort_start_date
  }
  person_col <- function(roster, col) {
    p <- .reducePersonFrame(ctx, cons)
    p[[col]][match(as.character(roster$cohort_row_id), as.character(p$row_key))]
  }
  annual_age <- function(roster, ref_year) {
    yob <- suppressWarnings(as.integer(person_col(roster, "year_of_birth")))
    age <- as.integer(ref_year) - yob
    age[!is.na(age) & age < 0L] <- NA_integer_
    age
  }
  switch(kind,
    age = {
      bands <- isTRUE(unlist(d[["bands"]]))
      one(if (bands) "text" else "integer", function(roster) {
        ref_year <- if (!is.null(d[["reference_date"]])) {
          rep(as.integer(substr(as.character(unlist(d[["reference_date"]])), 1L, 4L)), nrow(roster))
        } else if (identical(d[["reference"]] %||% "index", "index")) {
          as.integer(format(index_of(roster), "%Y"))
        } else {
          .irReject("age with reference = \"today\" needs a fixed reference_date",
                    field = paste0(field, "/reference_date"))
        }
        age <- annual_age(roster, ref_year)
        if (bands) .bandAges(age, d[["age_breaks"]]) else age
      })
    },
    sex_mf = one("text", function(roster) {
      g <- suppressWarnings(as.integer(person_col(roster, "gender_concept_id")))
      ifelse(g %in% 8507L, "M", ifelse(g %in% 8532L, "F", NA_character_))
    }),
    race = , ethnicity = {
      col <- paste0(kind, "_concept_id")
      translate <- .reduceTranslate(ctx)
      one(if (translate) "text" else "integer", function(roster) {
        ids <- suppressWarnings(as.integer(person_col(roster, col)))
        if (translate) .reduceConceptNames(ctx, ids) else ids
      })
    },
    demo_missingness = one("integer", function(roster) {
      p <- .reducePersonFrame(ctx, cons)
      p <- p[match(as.character(roster$cohort_row_id), as.character(p$row_key)), , drop = FALSE]
      cols <- c("year_of_birth", "month_of_birth", "day_of_birth", "race_concept_id",
                "ethnicity_concept_id", "gender_concept_id")
      miss <- rowSums(is.na(p[cols]))
      for (cc in c("race_concept_id", "ethnicity_concept_id", "gender_concept_id")) {
        miss <- miss + as.integer(!is.na(p[[cc]]) & p[[cc]] == 0L)
      }
      as.integer(miss)
    }),
    birth_year_band = {
      width <- as.integer(unlist(d[["width"]] %||% 5L))
      one("integer", function(roster) {
        yob <- suppressWarnings(as.integer(person_col(roster, "year_of_birth")))
        as.integer((yob %/% width) * width)
      })
    },
    died = {
      before <- d[["before"]]
      one("integer", function(roster) {
        dd <- .reduceDeathFrame(ctx, cons)
        death <- dd$death_date[match(as.character(roster$cohort_row_id), as.character(dd$row_key))]
        if (is.null(before)) return(as.integer(!is.na(death)))
        limit <- index_of(roster) + as.integer(unlist(before))
        out <- as.integer(!is.na(death) & death <= limit)
        out[is.na(limit)] <- NA_integer_
        out
      })
    },
    days_to_death = one("integer", function(roster) {
      dd <- .reduceDeathFrame(ctx, cons)
      death <- dd$death_date[match(as.character(roster$cohort_row_id), as.character(dd$row_key))]
      as.integer(death - index_of(roster))
    }),
    n_observation_periods = one("integer", function(roster) {
      po <- .reducePeriodFrame(ctx, cons)
      n <- table(factor(as.character(po$row_key), levels = as.character(roster$cohort_row_id)))
      as.integer(n)
    }),
    obs_duration = {
      policy <- d[["period_policy"]] %||% "total"
      one("integer", function(roster) {
        po <- .reducePeriodFrame(ctx, cons)
        groups <- .reduceGroups(po$row_key, roster$cohort_row_id)
        vapply(groups, function(ix) {
          if (!length(ix)) return(NA_integer_)
          s <- po$start_date[ix]
          e <- po$end_date[ix]
          dur <- as.integer(e - s)
          switch(policy,
            total = {
              o <- order(s, e)
              s <- s[o]; e <- e[o]
              total <- 0L; cs <- s[[1L]]; ce <- e[[1L]]
              for (k in seq_along(s)[-1L]) {
                if (s[[k]] > ce) { total <- total + as.integer(ce - cs); cs <- s[[k]]; ce <- e[[k]] }
                else if (e[[k]] > ce) ce <- e[[k]]
              }
              as.integer(total + as.integer(ce - cs))
            },
            first = dur[order(s, e)[[1L]]],
            last = dur[order(s, e, decreasing = TRUE)[[1L]]],
            longest = dur[order(-dur, s, e)[[1L]]])
        }, integer(1))
      })
    },
    prior_obs = , followup = {
      ref <- as.Date(as.character(unlist(d[["reference_date"]])))
      one("integer", function(roster) {
        po <- .reducePeriodFrame(ctx, cons)
        groups <- .reduceGroups(po$row_key, roster$cohort_row_id)
        vapply(groups, function(ix) {
          hit <- ix[po$start_date[ix] <= ref & po$end_date[ix] >= ref]
          if (length(hit) != 1L) return(NA_integer_)
          if (identical(kind, "prior_obs")) as.integer(ref - po$start_date[[hit]]) else
            as.integer(po$end_date[[hit]] - ref)
        }, integer(1))
      })
    },
    observed_days = .reduceObservedDays(ctx, node, name, cons),
    charlson = , chads2 = , chadsvasc = , dcsi = , hfrs = .reduceScoreVariable(ctx, node, name, cons),
    .irReject(paste0("unknown derived variable kind '", kind, "'"), field = field))
}

# observed_days(window) (and its v1 containing-period variant, A-10).
.reduceObservedDays <- function(ctx, node, name, cons) {
  d <- node[["derived"]]
  field <- paste0("/variables/", name, "/derived")
  columns <- stats::setNames(list(.fragmentColumn("integer", TRUE, "measure")), name)
  if (is.null(cons$coverage)) {
    .irReject("observed_days needs a population with observation coverage", field = field)
  }
  if (isTRUE(unlist(d[["legacy_containing_period"]]))) {
    w <- d[["window"]]
    prior <- is.null(w[["start"]])
    evaluate <- function() {
      roster <- .reduceRosterFrame(ctx, cons$roster)
      idx <- roster$cohort_start_date
      v <- if (prior) as.integer(idx - roster$op_start) else as.integer(roster$op_end - idx)
      out <- data.frame(row_key = roster$cohort_row_id, stringsAsFactors = FALSE)
      out[[name]] <- v
      out
    }
    return(list(stream = NULL, reducer = NULL, declared_empty_value = NA, columns = columns,
                evaluate = evaluate, available = TRUE))
  }
  names <- list(bounds = .reduceName(ctx, cons, "b", name), stream = .reduceName(ctx, cons, "s", name))
  evaluate <- function() {
    roster <- .reduceRosterFrame(ctx, cons$roster)
    v <- .intervalReducerValues(ctx, "observed_days", cons$roster, "final",
                                window = .reduceWindow(d[["window"]]), cov = cons$coverage,
                                names = names)
    out <- data.frame(row_key = roster$cohort_row_id, stringsAsFactors = FALSE)
    out[[name]] <- as.integer(v$value[match(as.character(roster$cohort_row_id), as.character(v$row_key))])
    out
  }
  list(stream = NULL, reducer = NULL, declared_empty_value = 0L, columns = columns,
       evaluate = evaluate, available = TRUE)
}

# --- comorbidity scores (amendment A-2) ----------------------------------------------------------

#' The reviewed definition of a comorbidity score
#'
#' The concept categories and weights of \code{.computeComorbidityScore()}
#' (FeatureExtraction-traced, descendants expanded through
#' \code{concept_ancestor}; DCSI and HFRS vocabulary-resolved), shared by the
#' v1 adapter and the native v2 variable so their vocabulary policy is one.
#'
#' @param handle CDM handle.
#' @param score_type One of charlson, chads2, chadsvasc, dcsi, hfrs.
#' @return list(mode = "simple_weighted" | "tiered" | "weighted_binary",
#'   entries = list(list(category, concepts, weight, tier))).
#' @keywords internal
.comorbidityDefinition <- function(handle, score_type) {
  if (score_type %in% c("dcsi", "hfrs")) {
    resolved <- .resolveScoreConcepts(handle, score_type)
    return(list(mode = if (identical(score_type, "dcsi")) "tiered" else "weighted_binary",
                entries = resolved))
  }
  categories <- .comorbidityCategories(score_type)
  hierarchy_cache <- paste0("resolved_hierarchy_", score_type)
  expanded <- handle[[hierarchy_cache]]
  if (is.null(expanded)) {
    expanded <- lapply(categories, function(category) {
      descendants <- .vocabGetDescendants(handle, category$concepts, include_self = TRUE)
      category$concepts <- unique(c(as.integer(category$concepts), as.integer(descendants$concept_id)))
      category
    })
    handle[[hierarchy_cache]] <- expanded
  }
  list(mode = "simple_weighted",
       entries = lapply(names(expanded), function(n) {
         list(category = n, concepts = expanded[[n]]$concepts, weight = expanded[[n]]$weight)
       }))
}

#' Score per key from condition rows (key, concept_id)
#'
#' @param def \code{.comorbidityDefinition()}.
#' @param key Key of each condition row.
#' @param concept Concept of each condition row.
#' @param keys The keys to score (every key gets a score, 0 without rows).
#' @return Integer (simple weighted) or numeric (tiered, weighted binary)
#'   scores aligned with \code{keys}.
#' @keywords internal
.comorbidityScoreRows <- function(def, key, concept, keys) {
  key <- as.character(key)
  keys_chr <- as.character(keys)
  numeric_mode <- def$mode %in% c("tiered", "weighted_binary")
  score <- if (numeric_mode) rep(0, length(keys)) else rep(0L, length(keys))
  if (!length(key)) return(score)
  if (identical(def$mode, "tiered")) {
    for (cn in unique(vapply(def$entries, function(e) e$category, character(1)))) {
      best <- rep(0, length(keys))
      for (entry in Filter(function(e) e$category == cn, def$entries)) {
        hit <- keys_chr %in% unique(key[concept %in% entry$concepts])
        best[hit] <- pmax(best[hit], entry$tier)
      }
      score <- score + best
    }
    return(score)
  }
  for (entry in def$entries) {
    hit <- keys_chr %in% unique(key[concept %in% entry$concepts])
    score[hit] <- score[hit] + entry$weight
  }
  score
}

#' Age and sex points of CHADS2 / CHA2DS2-VASc
#' @keywords internal
.comorbidityAgePoints <- function(score_type, age, gender) {
  pts <- integer(length(age))
  if (identical(score_type, "chads2")) {
    pts <- pts + ifelse(!is.na(age) & age >= 75L, 1L, 0L)
  }
  if (identical(score_type, "chadsvasc")) {
    pts <- pts + ifelse(!is.na(age) & age >= 75L, 2L, ifelse(!is.na(age) & age >= 65L, 1L, 0L))
    pts <- pts + ifelse(!is.na(gender) & gender == 8532L, 1L, 0L)
  }
  as.integer(pts)
}

# A native score variable: condition rows dated on or before the reference
# date (a fixed ISO date, else the episode's index date), the reviewed
# categories, and the same date for the age points (A-2). A lowered v1 score
# (legacy_all_time_conditions) counts every condition row.
.reduceScoreVariable <- function(ctx, node, name, cons) {
  d <- node[["derived"]]
  kind <- d[["kind"]]
  field <- paste0("/variables/", name, "/derived")
  legacy <- isTRUE(unlist(d[["legacy_all_time_conditions"]]))
  type <- if (kind %in% c("dcsi", "hfrs")) "numeric" else "integer"
  columns <- stats::setNames(list(.fragmentColumn(type, TRUE, "measure")), name)
  h <- .compileDialect(ctx)
  handle <- ctx[["handle"]]
  spec <- cons$spec
  ref_date <- d[["reference_date"]]
  if (!is.null(ref_date)) ref_date <- as.character(unlist(ref_date))
  if (is.null(ref_date) && is.null(spec$index)) {
    .irReject(paste0("score '", kind, "' over a population without an index date needs a fixed ",
                     "reference_date"), field = paste0(field, "/reference_date"))
  }
  role <- .roleOf(handle, "condition_occurrence")
  if (is.null(role)) {
    .irReject(paste0(kind, " score needs condition_occurrence"), code = "unavailable_table",
              field = field)
  }
  def <- .comorbidityDefinition(handle, kind)
  ids <- unique(unlist(lapply(def$entries, function(e) e$concepts)))
  ref_expr <- if (!is.null(ref_date)) .sqlDateLiteral(h, ref_date) else paste0("r.", spec$index)
  cond <- .reduceTable(ctx, "condition_occurrence")
  where <- c(paste0("c.", role[["person"]], " = r.", spec$person),
             .sqlIdInPredicate(paste0("c.", role[["domain_concept"]]), ids))
  if (!legacy) where <- c(where, paste0("c.", role[["start"]], " <= ", ref_expr))
  stream_name <- .reduceName(ctx, cons, "a", name)
  body <- paste0("SELECT DISTINCT r.", spec$key, " AS row_key, c.", role[["domain_concept"]],
                 " AS concept_id FROM ", spec$name, " r INNER JOIN ", cond, " c ON ",
                 paste(where, collapse = " AND "))
  stream <- .fragment(stream_name, "stream", "row", body,
                      list(row_key = .fragmentColumn(spec$key_type, FALSE, "private"),
                           concept_id = .fragmentColumn("integer", TRUE, "concept")),
                      dependencies = spec$name,
                      person_lineage = list(person_col = NULL, via_roster = cons$roster$name),
                      roster_namespace = spec$namespace, private_columns = "row_key")
  .sqlAssertTypedSql(h, stream$select_body)
  .fragmentRegister(ctx, stream)
  evaluate <- function() {
    roster <- .reduceRosterFrame(ctx, cons$roster)
    keys <- roster$cohort_row_id
    rows <- .reduceFetch(ctx, stream, order = c("row_key", "concept_id"), field = field)
    score <- .comorbidityScoreRows(def, rows$row_key, rows$concept_id, keys)
    if (kind %in% c("chads2", "chadsvasc")) {
      p <- .reducePersonFrame(ctx, cons)
      at <- match(as.character(keys), as.character(p$row_key))
      ref_year <- if (!is.null(ref_date)) as.integer(substr(ref_date, 1L, 4L)) else
        as.integer(format(roster$cohort_start_date, "%Y"))
      age <- ref_year - suppressWarnings(as.integer(p$year_of_birth[at]))
      age[!is.na(age) & age < 0L] <- NA_integer_
      score <- score + .comorbidityAgePoints(kind, age,
                                             suppressWarnings(as.integer(p$gender_concept_id[at])))
      if (is.null(ref_date)) score[is.na(roster$cohort_start_date)] <- NA
    }
    out <- data.frame(row_key = keys, stringsAsFactors = FALSE)
    out[[name]] <- .reduceCoerce(score, type)
    if (is.null(ref_date)) out[[name]][is.na(roster$cohort_start_date)] <- NA
    out
  }
  list(stream = stream, reducer = NULL, declared_empty_value = 0L, columns = columns,
       evaluate = evaluate, available = TRUE)
}

# --- raw columns ------------------------------------------------------------------------------------

.reduceRawVariable <- function(ctx, node, name, cons) {
  raw <- node[["raw"]]
  table <- tolower(as.character(unlist(raw[["table"]])))
  column <- tolower(as.character(unlist(raw[["column"]])))
  field <- paste0("/variables/", name, "/raw")
  h <- .compileDialect(ctx)
  spec <- cons$spec
  col_df <- .reduceTableColumns(ctx, table)
  if (is.null(col_df) || !column %in% tolower(col_df$column_name)) {
    .irReject(paste0("'", column, "' is not a column of '", table, "'"), field = paste0(field, "/column"))
  }
  if (isTRUE(.assignmentBlockedColumn(column, col_df)) ||
      column %in% c(.identifierColumns(), "person_id")) {
    .irReject(paste0("'", table, ".", column, "' is a class-1 or identifier column and is never ",
                     "released (design 7.6)"), field = paste0(field, "/column"))
  }
  ftype <- .eventColumnType(col_df, column)
  if (is.null(ftype)) {
    if (grepl("_datetime$", column)) ftype <- "date" else {
      .irReject(paste0("'", table, ".", column, "' has no supported column type"),
                field = paste0(field, "/column"))
    }
  }
  if (identical(ftype, "bigint")) ftype <- "numeric"
  concept <- grepl("_concept_id$", column)
  translate <- concept && .reduceTranslate(ctx)
  out_type <- if (translate) "text" else ftype
  columns <- stats::setNames(list(.fragmentColumn(out_type, TRUE, "measure")), name)
  qualified <- .reduceTable(ctx, table)
  person_key <- if (identical(table, "person")) "person_id" else
    (.roleOf(ctx[["handle"]], table)[["person"]] %||% "person_id")
  expr <- if (grepl("_datetime$", column)) paste0("x.", column) else paste0("x.", column)
  body <- switch(table,
    person = paste0("SELECT r.", spec$key, " AS row_key, ", expr, " AS value FROM ", spec$name,
                    " r LEFT JOIN ", qualified, " x ON x.", person_key, " = r.", spec$person),
    death = {
      role <- .roleOf(ctx[["handle"]], "death")
      rn <- .sqlRowNumber(h, paste0("d.", person_key), c(paste0("d.", role[["start"]]), paste0("d.", column)))
      paste0("SELECT r.", spec$key, " AS row_key, ", expr, " AS value FROM ", spec$name,
             " r LEFT JOIN (SELECT d.", person_key, " AS dsomop_person, d.", column, ", ", rn,
             " AS dsomop_rn FROM ", qualified, " d) x ON x.dsomop_person = r.", spec$person,
             " AND x.dsomop_rn = 1")
    },
    observation_period = {
      role <- .roleOf(ctx[["handle"]], "observation_period")
      s <- paste0("o.", role[["start"]])
      e <- paste0("o.", role[["end"]])
      # Native person rosters also declare the date columns, with NULL
      # values. Indexed status belongs to the population, not a row's date.
      indexed <- .pnv2PopulationIndexed(ctx[["ir"]][["populations"]], spec$population)
      if (indexed && is.null(spec$index)) {
        stop("Raw observation columns need the indexed roster's index date column.", call. = FALSE)
      }
      inside <- if (indexed) {
        paste0(" AND r.", spec$index, " >= ", s, " AND r.", spec$index, " <= ", e)
      } else ""
      policy <- cons$coverage$policy$policy
      if (indexed && isTRUE(policy %in% c("unique", "earliest"))) {
        if (is.null(spec$op_component)) {
          stop("Raw observation columns need the indexed roster's selected op_component_id.", call. = FALSE)
        }
        # These policies select one complete record; its component is its
        # period key. A kept row without a component has no selected record.
        inside <- paste0(inside, " AND ", .sqlLosslessKey(h, paste0("o.", role[["event_key"]])),
                         " = r.", spec$op_component)
      }
      rn <- .sqlRowNumber(h, paste0("r.", spec$key), c(s, e, paste0("o.", role[["event_key"]])))
      paste0("SELECT y.row_key AS row_key, y.value AS value FROM (SELECT r.", spec$key,
             " AS row_key, o.", column, " AS value, ", rn, " AS dsomop_rn FROM ", spec$name,
             " r LEFT JOIN ", qualified, " o ON o.", role[["person"]], " = r.", spec$person, inside,
             ") y WHERE y.dsomop_rn = 1")
    },
    .irReject("raw variables read person, death or observation_period", field = paste0(field, "/table")))
  fname <- .reduceName(ctx, cons, "s", name)
  vtype <- if (identical(ftype, "date")) "date" else ftype
  f <- .fragment(fname, "stream", "episode", body,
                 list(row_key = .fragmentColumn(spec$key_type, FALSE, "private"),
                      value = .fragmentColumn(vtype, TRUE, "measure")),
                 dependencies = spec$name, unique_key = "row_key",
                 person_lineage = list(person_col = NULL, via_roster = cons$roster$name),
                 roster_namespace = spec$namespace, private_columns = "row_key")
  .sqlAssertTypedSql(h, f$select_body)
  .fragmentRegister(ctx, f)
  evaluate <- function() {
    roster <- .reduceRosterFrame(ctx, cons$roster)
    rows <- .reduceFetch(ctx, f, order = "row_key", field = field)
    v <- rows$value[match(as.character(roster$cohort_row_id), as.character(rows$row_key))]
    if (translate) v <- .reduceConceptNames(ctx, suppressWarnings(as.integer(v)))
    out <- data.frame(row_key = roster$cohort_row_id, stringsAsFactors = FALSE)
    out[[name]] <- if (identical(out_type, "date")) as.Date(substr(as.character(v), 1L, 10L)) else
      .reduceCoerce(v, out_type)
    out
  }
  list(stream = f, reducer = NULL, declared_empty_value = NA, columns = columns,
       evaluate = evaluate, available = TRUE, date = identical(out_type, "date"))
}

# --- population columns -----------------------------------------------------------------------------

.reducePopulationColumn <- function(ctx, node, name, cons) {
  col <- as.character(unlist(node[["population_column"]][["name"]]))
  type <- switch(col, index_source = "text", set_id = "bigint", is_case = "integer",
                 sampling_weight = "numeric",
                 .irReject(paste0("unknown population column '", col, "'"),
                           field = paste0("/variables/", name, "/population_column/name")))
  if (!col %in% names(cons$roster$columns)) {
    stop("The roster lacks its population column '", col, "'.", call. = FALSE)
  }
  columns <- stats::setNames(list(.fragmentColumn(type, TRUE,
                                                  if (identical(col, "set_id")) "analytic_key" else "measure")),
                             name)
  list(stream = NULL, reducer = NULL, declared_empty_value = NA, columns = columns,
       available = TRUE,
       evaluate = function() {
         roster <- .reduceRosterFrame(ctx, cons$roster)
         out <- data.frame(row_key = roster$cohort_row_id, stringsAsFactors = FALSE)
         out[[name]] <- .reduceCoerce(roster[[col]], type)
         out
       })
}

# --- collapse to person grain ---------------------------------------------------------------------

#' Collapse episode values to one row per person (section 6.11)
#'
#' \code{first_episode} / \code{last_episode} take the episode with the
#' lowest / highest \code{cohort_row_id} (chronological, section 6.9);
#' \code{any} combines occurrence reducers over the person's episodes (Kleene
#' OR for binary and any; the maximum for counts) and takes the first
#' episode's value otherwise.
#'
#' @param frame data.frame(cohort_row_id, person_id, <variables>) over every
#'   episode.
#' @param collapse \code{"first_episode"}, \code{"last_episode"} or
#'   \code{"any"}.
#' @param occurrence Named character: variable column -> reducer fn (only for
#'   reducer columns).
#' @return data.frame(person_id, <variables>), one row per person in order of
#'   the person's first episode.
#' @keywords internal
.reduceCollapse <- function(frame, collapse, occurrence = character(0)) {
  persons <- unique(as.character(frame$person_id))
  vars <- setdiff(names(frame), c("cohort_row_id", "person_id", "row_id"))
  if (!nrow(frame)) {
    out <- frame[0, c("person_id", vars), drop = FALSE]
    return(out)
  }
  groups <- split(seq_len(nrow(frame)), factor(as.character(frame$person_id), levels = persons))
  pick <- vapply(groups, function(ix) {
    ix <- ix[order(.reduceKeyOrder(frame$cohort_row_id[ix]))]
    if (identical(collapse, "last_episode")) ix[[length(ix)]] else ix[[1L]]
  }, integer(1))
  out <- frame[pick, c("person_id", vars), drop = FALSE]
  if (identical(collapse, "any")) {
    for (v in intersect(names(occurrence), vars)) {
      fn <- occurrence[[v]]
      if (!fn %in% .REDUCER_OCCURRENCE) next
      out[[v]] <- vapply(groups, function(ix) {
        x <- frame[[v]][ix]
        if (fn %in% c("binary", "any")) {
          if (any(x %in% 1L)) 1 else if (anyNA(x)) NA_real_ else 0
        } else {
          if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
        }
      }, numeric(1))
      out[[v]] <- as.integer(out[[v]])
    }
  }
  rownames(out) <- NULL
  out
}
