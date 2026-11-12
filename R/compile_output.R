# Module: Outputs (design-2.10.3 sections 2.9, 6.12, 7.1-7.3)
#
# `.compileOutput()` turns one IR output over its population into a frozen
# `CompiledOutput` (R/types.R): its components -- frames assembled in R from
# private, bounded fetches (every statement reads the population roster once,
# so MySQL's one-reference rule for session temporary tables holds), each with
# its typed fragment -- and one publication candidate per component with its
# exact person lineage (section 7.3):
#
#   episode-grain patient components   roster lineage on (person_id,
#                                      cohort_row_id) in the population's
#                                      roster namespace; row_id = cohort_row_id;
#   person-grain patient components    direct lineage on person_id;
#   reference companions               covariateRef, timeRef, transition_ref
#                                      and the dictionary (grain none, no
#                                      person or episode keys).
#
# Formats: wide (one row per episode or, after the declared collapse, per
# person), long (event rows with derived projections and relations, or the
# common-schema event log), survival (explicit censoring, independent TAR
# bounds, effective entry, last-observed-day endpoints, every native format
# and component of amendment A-5), intervals, temporal_covariates and
# person_period (seeded partitions, unioned exposures, carry-forward at the
# period start, dense missingness, the per-plan covariate registry), sparse,
# membership, dictionary (plan concepts from the vocabulary only) and joined
# (private inputs merged on the roster key with checked cardinality).
#
# Nothing here enforces support: publication counts each component's own rows
# (R/publish.R). Every intermediate -- streams, rosters, private joined inputs
# -- stays in the private context.

# --- identity family ("ox") ------------------------------------------------------------------

#' Identities of the output-level fragments of a plan (amendment A-17)
#'
#' \code{("ox", <output>, "<kind>_<i>[_<j>]")} with single-token kinds and
#' 1-based positions in the sorted names of the output's TARs, outcomes,
#' censors, sources, exposures and partition boundaries (so the suffix is an
#' injective encoding); readable forms are illustrative.
#' @param ir The IR.
#' @return List of \code{.fragmentIdentity()} records.
#' @keywords internal
.outputFragmentIdentities <- function(ir) {
  out <- list()
  add <- function(o, suffix) out[[length(out) + 1L]] <<- .fragmentIdentity("ox", o, suffix)
  for (o in names(ir[["outputs"]] %||% list())) {
    node <- ir[["outputs"]][[o]]
    opts <- node[["options"]] %||% list()
    switch(node[["format"]] %||% "",
      survival = {
        tars <- .outputTars(node)
        outs <- .outputSortedNames(opts[["outcomes"]])
        cens <- seq_along(.irItems(opts[["censor"]]))
        for (ti in seq_along(tars)) for (ki in seq_along(outs)) add(o, paste0("k_", ti, "_", ki))
        for (ci in cens) add(o, paste0("c_", ci))
      },
      long = , intervals = , temporal_covariates = , sparse = {
        srcs <- .outputSources(node)
        for (i in seq_along(srcs)) add(o, paste0("s_", i))
        for (j in seq_along(.outputSortedNames(opts[["relations"]]))) add(o, paste0("r_", j))
      },
      person_period = {
        for (i in seq_along(.outputSources(node))) add(o, paste0("s_", i))
        for (j in seq_along(.outputSortedNames(opts[["exposures"]]))) add(o, paste0("x_", j))
        for (k in seq_along(.outputSortedNames(opts[["outcomes"]]))) add(o, paste0("y_", k))
        for (b in seq_along(.irItems(opts[["partition"]][["boundaries"]]))) add(o, paste0("z_", b))
      },
      NULL)
  }
  out
}

.outputSortedNames <- function(x) {
  n <- names(x %||% list())
  if (!length(n)) return(character(0))
  sort(n, method = "radix")
}

# The declared sources of an output (top-level `sources` and `options.sources`,
# deduplicated in declaration order).
.outputSources <- function(node) {
  srcs <- c(.irItems(node[["sources"]]), .irItems(node[["options"]][["sources"]]))
  if (!length(srcs)) return(list())
  keys <- vapply(srcs, .jcsCanonical, "")
  srcs[!duplicated(keys)]
}

.outputSourceName <- function(src) as.character(unlist(src[["event"]] %||% src[["interval"]]))

# --- the driver -----------------------------------------------------------------------------------

#' The population record an output reads
#'
#' The final roster and coverage of a population, compiled once per context
#' (WS-6's populations compiler); tests and adapters may supply records.
#' @keywords internal
.outputPopulation <- function(cctx, pop, populations = NULL) {
  if (!is.null(populations[[pop]])) return(populations[[pop]])
  memo <- .compileMemo(cctx, "output_populations")
  if (exists(pop, envir = memo, inherits = FALSE)) return(get(pop, envir = memo, inherits = FALSE))
  compile <- get0(".compilePopulationByName", envir = asNamespace("dsOMOP"), mode = "function",
                  inherits = FALSE)
  if (is.null(compile)) {
    .irReject(paste0("this server validates language 2.0 plans but does not ",
                     "execute them yet; use omopPlanExplainDS"))
  }
  rec <- compile(cctx, pop)
  assign(pop, rec, envir = memo)
  rec
}

#' Compile the publication roots of a language 2.0 plan
#'
#' Every root of \code{ir$publish} and the private outputs it joins, in the
#' plan's output order (\code{.irOrder()}), each over its population's final
#' roster. The rosters are registered in the execution context as the
#' immutable namespaces publication resolves lineage through.
#'
#' @param cctx Compilation context (\code{.compileContext()}).
#' @param out_symbols Named list output -> requested symbol.
#' @param populations Optional named list of precompiled population records
#'   (\code{list(roster, coverage)}).
#' @return Named list root -> \code{CompiledOutput}.
#' @keywords internal
.planCompileOutputs <- function(cctx, out_symbols = list(), populations = NULL) {
  ir <- cctx[["ir"]]
  ctx <- cctx[["execution"]]
  roots <- as.character(unlist(ir[["publish"]] %||% names(ir[["outputs"]]), use.names = FALSE))
  needed <- .irReach(roots, function(o) .irOutputDeps(ir, o), names(ir[["outputs"]]))
  order <- intersect(.irOrder(ir)[["outputs"]], needed)
  compiled <- list()
  for (o in order) {
    node <- .irNamedNode(ir, "output", o)
    pop <- if (identical(node[["format"]], "dictionary")) NULL else
      .outputPopulation(cctx, node[["rows"]], populations)
    deps <- intersect(.irOutputDeps(ir, o), names(compiled))
    inputs <- list(population = pop, outputs = compiled[deps],
                   symbol = as.character(out_symbols[[o]] %||% node[["result_symbol"]] %||% o),
                   mode = if (.isPlanContext(ctx)) ctx$output_mode else "memory")
    compiled[[o]] <- .compileOutput(cctx, node, inputs)
    if (!is.null(pop) && .isPlanContext(ctx)) .outputRegisterRoster(cctx, ctx, node[["rows"]], pop)
  }
  compiled[roots]
}

# Register a population's roster (cohort_row_id and its person column) as the
# immutable namespace roster lineage resolves through.
.outputRegisterRoster <- function(cctx, ctx, pop, rec) {
  spec <- .rosterSpec(rec$roster, "final")
  frame <- .reduceRosterFrame(cctx, rec$roster)
  .planRegisterRoster(ctx, spec$namespace %||% pop, frame[c("cohort_row_id", spec$person)])
}

#' Compile one output (frozen compiler signature, section 6)
#'
#' @param ctx Compilation context.
#' @param node IR output node with its plan name in \code{attr(node, "name")}.
#' @param inputs list(population = the rows' population record (roster,
#'   coverage), outputs = compiled outputs it depends on (joined inputs),
#'   symbol = the requested result symbol, mode = "memory" | "staged").
#' @return A validated \code{CompiledOutput}.
#' @keywords internal
.compileOutput <- function(ctx, node, inputs) {
  name <- .compileNodeName(node, "output")
  legacy <- intersect(names(node), c("legacy_split", "legacy_columns", "legacy_temporal",
                                     "legacy_automatic_features", "legacy_raw_filter"))
  legacy <- legacy[!vapply(node[legacy], function(x) is.null(x) || isFALSE(unlist(x)), logical(1))]
  if (length(legacy)) {
    .irReject(paste0("output '", name, "' carries the v1 compatibility variant ", legacy[[1L]],
                     ", which runs on the v1 executors"), field = paste0("/outputs/", name))
  }
  co <- switch(node[["format"]] %||% "",
    wide = .compileWideOutput(ctx, node, name, inputs),
    long = .compileLongOutput(ctx, node, name, inputs),
    survival = .compileSurvivalOutput(ctx, node, name, inputs),
    intervals = .compileIntervalsOutput(ctx, node, name, inputs),
    temporal_covariates = .compileTemporalOutput(ctx, node, name, inputs),
    person_period = .compilePanelOutput(ctx, node, name, inputs),
    sparse = .compileSparseOutput(ctx, node, name, inputs),
    membership = .compileMembershipOutput(ctx, node, name, inputs),
    dictionary = .compileDictionaryOutput(ctx, node, name, inputs),
    joined = .compileJoinedOutput(ctx, node, name, inputs),
    .irReject(paste0("unknown output format '", node[["format"]] %||% "", "'"),
              field = paste0("/outputs/", name, "/format")))
  .assertCompiledOutput(co)
}

# --- component helpers --------------------------------------------------------------------------

# The requested symbol of a component (the output's symbol, or
# <symbol>.<component>).
.outputSymbol <- function(inputs, output, component) {
  if (identical(component, output)) inputs$symbol else paste0(inputs$symbol, ".", component)
}

# Roster lineage of an episode-grain component of a consumer.
.outputRosterLineage <- function(cons) {
  .lineage("roster", c("person_id", "cohort_row_id"), cons$spec$namespace %||% cons$population,
           c(cons$spec$person, "cohort_row_id"), cons$spec$person)
}

#' A patient or reference component: frame, fragment and candidate
#'
#' @param inputs The output's inputs (symbol, mode).
#' @param output Output name.
#' @param component Component name.
#' @param frame The materialised frame.
#' @param columns Named list of \code{.fragmentColumn()} for the frame's
#'   columns, in order.
#' @param cons Consumer (patient components) or NULL (reference).
#' @param grain \code{"episode"}, \code{"person"} or \code{"none"}.
#' @param status \code{"ok"} or \code{"unavailable"}.
#' @return list(component = list(frame, fragment), candidate).
#' @keywords internal
.outputComponent <- function(inputs, output, component, frame, columns, cons = NULL,
                             grain = "episode", status = "ok") {
  .outputRowCap(nrow(frame), output)
  if (!grepl(.NAME_PATTERN, component)) {
    .irReject(paste0("component name '", component, "' exceeds the name grammar"),
              field = paste0("/outputs/", output))
  }
  frame <- frame[names(columns)]
  rownames(frame) <- NULL
  reference <- identical(grain, "none")
  lineage_frag <- if (reference) list(person_col = NULL, via_roster = NULL) else
    list(person_col = "person_id", via_roster = NULL)
  fragment <- .fragment(component, "result", grain, "SELECT 1", columns,
                        person_lineage = lineage_frag,
                        roster_namespace = if (identical(grain, "episode")) {
                          cons$spec$namespace %||% cons$population
                        })
  lineage <- if (reference) .lineage("reference") else if (identical(grain, "episode")) {
    .outputRosterLineage(cons)
  } else .lineage("direct", "person_id")
  symbol <- .outputSymbol(inputs, output, component)
  staged <- identical(inputs$mode, "staged")
  candidate <- .publicationCandidate(output, component, if (staged) "staged" else "frame", lineage,
                                     requested_symbol = if (!staged) symbol,
                                     requested_path = if (staged) paste0(symbol, ".parquet"),
                                     grain = grain, status = status)
  list(component = list(frame = frame, fragment = fragment), candidate = candidate)
}

# Assemble a CompiledOutput from component records.
.outputCompiled <- function(output, parts, status = "ok") {
  comps <- lapply(parts, function(p) p$component)
  names(comps) <- vapply(parts, function(p) p$candidate$component, "")
  if (anyDuplicated(names(comps))) {
    .irReject(paste0("output '", output, "' uses a reserved companion component name"),
              field = paste0("/outputs/", output))
  }
  refs <- names(comps)[vapply(parts, function(p) identical(p$candidate$grain, "none"), logical(1))]
  .compiledOutput(output, comps, lapply(unname(parts), function(p) p$candidate),
                  reference_components = refs, status = status)
}

# Identity columns of an episode-grain component, from the roster frame.
.outputIdentity <- function(roster) {
  data.frame(row_id = roster$cohort_row_id, cohort_row_id = roster$cohort_row_id,
             person_id = roster$person_id, stringsAsFactors = FALSE)
}

.outputIdentityColumns <- function(cons) {
  list(row_id = .fragmentColumn("bigint", FALSE, "episode_key"),
       cohort_row_id = .fragmentColumn("bigint", FALSE, "episode_key"),
       person_id = .fragmentColumn(cons$spec$person_type, FALSE, "person_key"))
}

# Bound the number of rows an R-assembled component may hold.
.outputRowCap <- function(n, output) {
  if (length(n) != 1L || !is.finite(n) || n > .reduceRowCap()) {
    .compileReject("an output component exceeds the server in-memory row cap (dsomop.max_memory_rows)",
                   reason = "reducer_row_cap", field = paste0("/outputs/", output))
  }
  invisible(TRUE)
}

#' Apply an output's date handling to its date-typed columns
#'
#' \code{relative}: whole days from the row's index (NA without one);
#' \code{remove}: the columns are dropped; \code{binned}: the first day of the
#' week (Monday), month or year as ISO text; \code{absolute}: dates unchanged,
#' only when \code{dsomop.allow_absolute_dates} is TRUE. Only declared date
#' columns are touched (an integer reducer named \code{*_date} is not a date).
#'
#' @param frame The component frame.
#' @param columns Its declared columns.
#' @param index Date vector aligned with the frame's rows (the index).
#' @param dh The normalised date_handling object.
#' @param output Output name (for rejections).
#' @return list(frame, columns).
#' @keywords internal
.outputDateHandling <- function(frame, columns, index, dh, output) {
  dates <- names(columns)[vapply(columns, function(c) identical(c$type, "date"), logical(1))]
  if (!length(dates)) return(list(frame = frame, columns = columns))
  mode <- dh[["mode"]] %||% "relative"
  requested <- as.character(unlist(dh[["date_columns"]] %||% list(), use.names = FALSE))
  if (length(requested)) {
    unknown <- setdiff(requested, dates)
    if (length(unknown)) {
      .irReject(paste0("date_handling date_columns names no date column: ", unknown[[1L]]),
                field = paste0("/outputs/", output, "/options/date_handling/date_columns"))
    }
    allow <- isTRUE(getOption("dsomop.allow_absolute_dates",
                              getOption("default.dsomop.allow_absolute_dates", FALSE)))
    if (!allow && length(setdiff(dates, requested))) {
      .irReject("date_handling date_columns omits date columns while absolute dates are disabled",
                field = paste0("/outputs/", output, "/options/date_handling/date_columns"))
    }
    dates <- requested
  }
  switch(mode,
    absolute = {
      allow <- isTRUE(getOption("dsomop.allow_absolute_dates",
                                getOption("default.dsomop.allow_absolute_dates", FALSE)))
      if (!allow) {
        .irReject("absolute date handling is not permitted by the server",
                  field = paste0("/outputs/", output, "/options/date_handling/mode"))
      }
    },
    remove = {
      frame[dates] <- NULL
      columns <- columns[setdiff(names(columns), dates)]
    },
    relative = {
      for (d in dates) {
        frame[[d]] <- as.integer(as.Date(frame[[d]]) - as.Date(index))
        columns[[d]] <- .fragmentColumn("integer", TRUE, columns[[d]]$role)
      }
    },
    binned = {
      width <- dh[["bin_width"]]
      for (d in dates) {
        x <- as.Date(frame[[d]])
        frame[[d]] <- switch(width,
          year = ifelse(is.na(x), NA_character_, format(x, "%Y-01-01")),
          month = ifelse(is.na(x), NA_character_, format(x, "%Y-%m-01")),
          week = ifelse(is.na(x), NA_character_,
                        format(x - (as.integer(format(x, "%u")) - 1L), "%Y-%m-%d")))
        columns[[d]] <- .fragmentColumn("text", TRUE, columns[[d]]$role)
      }
    })
  list(frame = frame, columns = columns)
}

# --- wide -------------------------------------------------------------------------------------------

#' Compile a wide output (section 6.12)
#'
#' The roster (or its persons after \code{collapse}) left-joined with every
#' variable on the roster key; reducers declare their zero / NA; several
#' sources at episode grain share one frame (defect B-19).
#' @keywords internal
.compileWideOutput <- function(ctx, node, name, inputs) {
  pop <- inputs$population
  grain <- node[["grain"]] %||% "episode"
  cons <- .reduceConsumer(ctx, name, pop$roster, pop$coverage, grain)
  ir <- ctx[["ir"]]
  vars <- .irOutputVariables(ir, name)
  records <- lapply(vars, function(v) {
    .compileVariable(ctx, .irNamedNode(ir, "variable", v), pop$roster, cons)
  })
  names(records) <- vars
  var_cols <- list()
  for (rec in records) var_cols <- c(var_cols, rec$columns)
  .outputCheckNames(names(var_cols), name, grain)
  roster <- .reduceRosterFrame(ctx, pop$roster)
  .outputRowCap(nrow(roster), name)
  frame <- .outputIdentity(roster)
  keys <- as.character(roster$cohort_row_id)
  for (rec in records) {
    vals <- rec$evaluate()
    at <- match(keys, as.character(vals$row_key))
    for (col in names(rec$columns)) frame[[col]] <- vals[[col]][at]
  }
  columns <- c(.outputIdentityColumns(cons), var_cols)
  dh <- node[["options"]][["date_handling"]] %||% list(mode = "relative")
  index <- if ("cohort_start_date" %in% names(roster)) roster$cohort_start_date else
    as.Date(rep(NA_character_, nrow(roster)))
  handled <- .outputDateHandling(frame, columns, index, dh, name)
  frame <- handled$frame
  columns <- handled$columns
  status <- if (all(vapply(records, function(r) isTRUE(r$available), logical(1)))) "ok" else
    "unavailable"
  if (identical(grain, "person")) {
    collapse <- node[["collapse"]] %||% "first_episode"
    occurrence <- unlist(lapply(vars, function(v) {
      n <- ir[["variables"]][[v]]
      if (!is.null(n[["reducer"]])) stats::setNames(n[["reducer"]][["fn"]], v)
    }))
    frame <- .reduceCollapse(frame, collapse, occurrence %||% character(0))
    columns <- c(list(person_id = .fragmentColumn(cons$spec$person_type, FALSE, "person_key")),
                 columns[setdiff(names(columns), c("row_id", "cohort_row_id", "person_id"))])
  }
  part <- .outputComponent(inputs, name, name, frame, columns, cons, grain = grain, status = status)
  .outputCompiled(name, list(part), status = status)
}

# Variable column names cannot repeat or take an identity column's name.
.outputCheckNames <- function(cols, output, grain) {
  reserved <- c("row_id", "cohort_row_id", "person_id", "subject_id")
  bad <- intersect(cols, reserved)
  if (length(bad)) {
    .irReject(paste0("variable column '", bad[[1L]], "' takes a reserved identity column name"),
              field = paste0("/outputs/", output, "/columns"))
  }
  dup <- cols[duplicated(cols)]
  if (length(dup)) {
    .irReject(paste0("output '", output, "' would carry the column '", dup[[1L]], "' twice"),
              field = paste0("/outputs/", output, "/columns"))
  }
  invisible(TRUE)
}

# --- membership ------------------------------------------------------------------------------------

#' Compile a membership output: the roster's own rows and dates
#' @keywords internal
.compileMembershipOutput <- function(ctx, node, name, inputs) {
  pop <- inputs$population
  cons <- .reduceConsumer(ctx, name, pop$roster, pop$coverage, node[["grain"]] %||% "episode")
  roster <- .reduceRosterFrame(ctx, pop$roster)
  .outputRowCap(nrow(roster), name)
  frame <- .outputIdentity(roster)
  columns <- .outputIdentityColumns(cons)
  for (col in c("cohort_start_date", "cohort_end_date")) {
    frame[[col]] <- if (col %in% names(roster)) roster[[col]] else as.Date(rep(NA_character_, nrow(roster)))
    columns[[col]] <- .fragmentColumn("date", TRUE, if (identical(col, "cohort_start_date")) "start" else "end")
  }
  if ("index_source" %in% names(roster)) {
    frame$index_source <- as.character(roster$index_source)
    columns$index_source <- .fragmentColumn("text", TRUE, "measure")
  }
  dh <- node[["options"]][["date_handling"]] %||% list(mode = "relative")
  handled <- .outputDateHandling(frame, columns, roster$cohort_start_date %||% frame$cohort_start_date,
                                 dh, name)
  grain <- node[["grain"]] %||% "episode"
  frame <- handled$frame
  columns <- handled$columns
  if (identical(grain, "person")) {
    frame <- .reduceCollapse(frame, node[["collapse"]] %||% "first_episode")
    columns <- c(list(person_id = .fragmentColumn(cons$spec$person_type, FALSE, "person_key")),
                 columns[setdiff(names(columns), c("row_id", "cohort_row_id", "person_id"))])
  }
  part <- .outputComponent(inputs, name, name, frame, columns, cons, grain = grain)
  .outputCompiled(name, list(part))
}

# --- dictionary ------------------------------------------------------------------------------------

#' Compile a dictionary output (a reference component, section 6.12)
#'
#' The resolved concepts of every table event the source outputs read
#' (\code{source_outputs}; empty = every other output of the plan) with their
#' vocabulary attributes: \code{concept_id}, \code{concept_name},
#' \code{domain_id}, \code{vocabulary_id}, \code{concept_class_id} and the
#' \code{source} event. Plan and vocabulary only -- never patient rows -- and
#' identical in memory and staged mode (defect D-14).
#' @keywords internal
.compileDictionaryOutput <- function(ctx, node, name, inputs) {
  ir <- ctx[["ir"]]
  sources <- as.character(unlist(node[["options"]][["source_outputs"]] %||% list(), use.names = FALSE))
  if (!length(sources)) {
    sources <- names(ir[["outputs"]])[vapply(ir[["outputs"]], function(o) {
      !identical(o[["format"]], "dictionary")
    }, logical(1))]
  }
  events <- unique(unlist(lapply(sources, function(o) names(.irOutputClosure(ir, o)[["events"]] %||% list())),
                          use.names = FALSE))
  events <- sort(events, method = "radix")
  pairs <- list()
  for (e in events) {
    ev <- ir[["events"]][[e]]
    if (is.null(ev[["table"]])) next
    concept <- .compileConceptSet(ctx, ev[["concept_set"]])
    if (!identical(concept$state, "set")) next
    ids <- sort(unique(as.integer(concept$ids)))
    if (length(ids)) pairs[[length(pairs) + 1L]] <- data.frame(source = e, concept_id = ids,
                                                               stringsAsFactors = FALSE)
  }
  pairs <- if (length(pairs)) do.call(rbind, pairs) else
    data.frame(source = character(0), concept_id = integer(0), stringsAsFactors = FALSE)
  info <- if (nrow(pairs)) .vocabLookupConcepts(ctx[["handle"]], unique(pairs$concept_id)) else NULL
  pick <- function(col) {
    if (is.null(info) || !col %in% names(info)) return(rep(NA_character_, nrow(pairs)))
    as.character(info[[col]][match(pairs$concept_id, info$concept_id)])
  }
  frame <- data.frame(concept_id = as.integer(pairs$concept_id), concept_name = pick("concept_name"),
                      domain_id = pick("domain_id"), vocabulary_id = pick("vocabulary_id"),
                      concept_class_id = pick("concept_class_id"), source = as.character(pairs$source),
                      stringsAsFactors = FALSE)
  columns <- list(concept_id = .fragmentColumn("integer", FALSE, "concept"),
                  concept_name = .fragmentColumn("text", TRUE, "measure"),
                  domain_id = .fragmentColumn("text", TRUE, "measure"),
                  vocabulary_id = .fragmentColumn("text", TRUE, "measure"),
                  concept_class_id = .fragmentColumn("text", TRUE, "measure"),
                  source = .fragmentColumn("text", FALSE, "measure"))
  part <- .outputComponent(inputs, name, name, frame, columns, NULL, grain = "none")
  .outputCompiled(name, list(part))
}

# --- joined ------------------------------------------------------------------------------------------

#' Compile a joined output (section 6.12)
#'
#' Merges the named components of private (or published) outputs of the same
#' execution on the roster key -- \code{person_id} at person grain,
#' \code{(person_id, cohort_row_id)} at episode grain -- with the declared
#' \code{how} and checked \code{cardinality} (\code{one_to_one}: unique keys on
#' both sides; \code{many_to_one}: on the right), a typed
#' \code{join_cardinality} failure otherwise. Inputs that are not publication
#' roots are never gated or assigned.
#' @keywords internal
.compileJoinedOutput <- function(ctx, node, name, inputs) {
  opts <- node[["options"]] %||% list()
  from <- .irItems(opts[["from"]])
  grain <- node[["grain"]] %||% "episode"
  how <- opts[["how"]] %||% "inner"
  cardinality <- opts[["cardinality"]] %||% "one_to_one"
  keys <- if (identical(grain, "person")) "person_id" else c("person_id", "cohort_row_id")
  pop <- inputs$population
  cons <- .reduceConsumer(ctx, name, pop$roster, pop$coverage, grain)
  frames <- list()
  cols <- list()
  status <- "ok"
  for (f in from) {
    o <- as.character(unlist(f[["output"]]))
    co <- inputs$outputs[[o]]
    if (is.null(co)) stop("Joined input '", o, "' was not compiled.", call. = FALSE)
    comp <- as.character(unlist(f[["component"]] %||% o))
    part <- co$components[[comp]]
    if (is.null(part) || is.null(part$frame)) {
      .irReject(paste0("joined input '", o, "' has no frame component '", comp, "'"),
                field = paste0("/outputs/", name, "/options/from"))
    }
    if (!all(keys %in% names(part$frame)) || comp %in% co$reference_components) {
      .irReject(paste0("joined input '", o, "/", comp, "' does not carry the ", grain, " key"),
                field = paste0("/outputs/", name, "/options/from"))
    }
    if (!identical(part$fragment$grain, grain) ||
        (identical(grain, "episode") &&
         !identical(part$fragment$roster_namespace, cons$spec$namespace %||% cons$population))) {
      .irReject(paste0("joined input '", o, "/", comp, "' has a different grain or roster namespace"),
                field = paste0("/outputs/", name, "/options/from"))
    }
    if (identical(co$status, "unavailable")) status <- "unavailable"
    frames[[length(frames) + 1L]] <- part$frame
    cols[[length(cols) + 1L]] <- part$fragment$columns
  }
  key_of <- function(df) do.call(paste, c(lapply(df[keys], as.character), sep = "\r"))
  left <- frames[[1L]]
  left_cols <- cols[[1L]]
  identity <- intersect(c("row_id", "cohort_row_id", "person_id"), names(left))
  for (i in seq_along(frames)[-1L]) {
    right <- frames[[i]]
    lk <- key_of(left)
    rk <- key_of(right)
    unique_right <- !anyDuplicated(rk)
    unique_left <- !anyDuplicated(lk)
    if (!unique_right || (identical(cardinality, "one_to_one") && !unique_left)) {
      .compileReject(paste0("joined output '", name, "': an input does not satisfy cardinality ",
                            cardinality), reason = "join_cardinality", code = "join_cardinality",
                     output = name, field = paste0("/outputs/", name, "/options/cardinality"))
    }
    extra <- setdiff(names(right), c(identity, keys))
    clash <- intersect(extra, names(left))
    if (length(clash)) {
      .irReject(paste0("joined inputs share the column '", clash[[1L]], "'"),
                field = paste0("/outputs/", name, "/options/from"))
    }
    at <- match(lk, rk)
    keep <- if (identical(how, "inner")) !is.na(at) else rep(TRUE, length(at))
    left <- left[keep, , drop = FALSE]
    at <- at[keep]
    for (col in extra) left[[col]] <- right[[col]][at]
    left_cols <- c(left_cols, cols[[i]][extra])
  }
  .outputRowCap(nrow(left), name)
  part <- .outputComponent(inputs, name, name, left, left_cols, cons, grain = grain, status = status)
  .outputCompiled(name, list(part), status = status)
}

# --- survival ----------------------------------------------------------------------------------------

# The TARs of a survival output: list(<name> = window); a single `tar` is
# named after the output (its component is the primary one).
.outputTarList <- function(node, name) {
  opts <- node[["options"]] %||% list()
  tars <- opts[["tars"]] %||% list()
  if (length(tars)) {
    n <- .outputSortedNames(tars)
    return(stats::setNames(lapply(n, function(t) tars[[t]]), n))
  }
  stats::setNames(list(opts[["tar"]]), name)
}

.outputTars <- function(node) {
  name <- attr(node, "name", exact = TRUE) %||% "tar"
  names(.outputTarList(node, name))
}

# Merge one row key's recorded observation periods into observed blocks
# (overlapping or adjacent periods form one block: no unobserved day between).
.survivalBlocks <- function(seg, keys) {
  groups <- .reduceGroups(seg$row_key, keys)
  lapply(groups, function(ix) {
    if (!length(ix)) return(data.frame(start = as.Date(character(0)), end = as.Date(character(0))))
    s <- seg$start_date[ix]
    e <- seg$end_date[ix]
    o <- order(s, e)
    s <- s[o]
    e <- e[o]
    bs <- s[[1L]]; be <- e[[1L]]
    out_s <- as.Date(character(0)); out_e <- as.Date(character(0))
    for (k in seq_along(s)[-1L]) {
      if (s[[k]] > be + 1L) {
        out_s <- c(out_s, bs); out_e <- c(out_e, be)
        bs <- s[[k]]; be <- e[[k]]
      } else if (e[[k]] > be) be <- e[[k]]
    }
    data.frame(start = c(out_s, bs), end = c(out_e, be))
  })
}

#' The normalised form of one survival censor entry
#'
#' Defaults applied (section 6.12, amendments A-5, A-7): a built-in
#' (\code{observation_end}, \code{death}, \code{index_end}), an
#' administrative date, a source entry (the first event starting on or after
#' the entry) or an \code{omop_censor} entry with its \code{which},
#' \code{offset}, \code{search}, \code{missing} and
#' \code{when_index_source}. The compiler and explain read the same form
#' (defect A-14: explain states the effective censoring).
#' @param item One entry of \code{options$censor}.
#' @return list(kind, ...).
#' @keywords internal
.survivalCensorSpec <- function(item) {
  if (is.list(item) && length(item) == 1L && is.null(names(item)) && is.character(item[[1L]])) {
    item <- item[[1L]]
  }
  if (is.character(item)) {
    if (length(item) != 1L || !item %in% c("observation_end", "death", "index_end")) {
      stop("Unknown censor '", paste(item, collapse = ","), "'.", call. = FALSE)
    }
    return(list(kind = item))
  }
  if (!is.null(item[["admin_date"]])) {
    return(list(kind = "admin_date", date = as.character(unlist(item[["admin_date"]]))))
  }
  spec <- item[["censor"]]
  if (is.null(spec)) {
    return(list(kind = "source", source = item, which = "first_start_after_entry", offset = 0L,
                search = NULL, missing = "ignore", when_index_source = NULL))
  }
  list(kind = "omop_censor", source = spec[["source"]],
       which = spec[["which"]] %||% "first_end_after_entry",
       offset = as.integer(unlist(spec[["offset"]] %||% 0L)), search = spec[["search"]],
       missing = spec[["missing"]] %||% "ignore",
       when_index_source = if (!is.null(spec[["when_index_source"]])) {
         as.character(unlist(spec[["when_index_source"]]))
       })
}

#' The effective censoring of a survival output (defect A-14)
#'
#' What explain states for every survival output: the censor entries in
#' force with their defaults, the implicit cohort-end cap of a lowered v1
#' plan (\code{legacy_cohort_end_cap}, defect A-E12), how observation ends the
#' risk (a native plan always censors at the last observed day before the
#' first observation gap after entry, or re-enters at the next observed
#' segment for \code{counting_process} with \code{gap_policy = "reenter"}; a
#' lowered v1 plan runs on the v1 executor, which censors at the end of the
#' observation period containing the index only when \code{observation_end}
#' is listed), the prior-outcome rule and the same-day priority.
#' @param opts The survival output's options.
#' @return Named list.
#' @keywords internal
.survivalEffectiveCensoring <- function(opts) {
  opts <- opts %||% list()
  entries <- lapply(.irItems(opts[["censor"]]), .survivalCensorSpec)
  kinds <- vapply(entries, function(e) e$kind, character(1))
  legacy <- isTRUE(unlist(opts[["legacy_cohort_end_cap"]]))
  if (legacy && !"index_end" %in% kinds) {
    entries <- c(entries, list(list(kind = "index_end", implicit = TRUE)))
  }
  fmt <- opts[["format"]] %||% "survival"
  prior <- opts[["prior_outcome"]]
  list(
    entries = entries,
    cohort_end_cap = legacy,
    observation = if (legacy) {
      if ("observation_end" %in% kinds) "containing_period_end" else "none"
    } else if (identical(fmt, "counting_process") &&
               identical(opts[["gap_policy"]] %||% "censor_at_gap", "reenter")) {
      "reenter_at_next_observed_segment"
    } else "last_observed_day_before_first_gap",
    prior_outcome = if (is.null(prior)) list(action = "ignore") else
      list(action = prior[["action"]] %||% "exclude", lookback = prior[["lookback"]]),
    same_day_priority = opts[["same_day_priority"]] %||% "event"
  )
}

# The censor dates of one censor item for every roster row (NA = none) and
# the rows it makes ineligible (missing = "ineligible").
.survivalCensorDates <- function(item, rows, keys, entry, index_source, roster, death) {
  n <- length(keys)
  none <- as.Date(rep(NA_character_, n))
  out <- list(date = none, ineligible = rep(FALSE, n))
  spec <- .survivalCensorSpec(item)
  if (spec$kind %in% c("observation_end", "death", "index_end")) {
    out$date <- switch(spec$kind,
      observation_end = as.Date(roster$op_end),
      death = death,
      index_end = as.Date(roster$cohort_end_date))
    return(out)
  }
  if (identical(spec$kind, "admin_date")) {
    out$date <- rep(as.Date(spec$date), n)
    return(out)
  }
  applies <- rep(TRUE, n)
  if (!is.null(spec$when_index_source)) applies <- as.character(index_source) %in% spec$when_index_source
  groups <- .reduceGroups(rows$row_key, keys)
  for (i in seq_len(n)) {
    if (!applies[[i]] || is.na(entry[[i]])) next
    ix <- groups[[i]]
    s <- rows$start_date[ix]
    e <- rows$end_date[ix]
    d <- switch(spec$which,
      first_end_after_entry = { x <- e[!is.na(e) & e >= entry[[i]]]; if (length(x)) min(x) else NA },
      first_start_after_entry = { x <- s[!is.na(s) & s >= entry[[i]]]; if (length(x)) min(x) else NA },
      containing_entry_end = {
        hit <- which(!is.na(s) & !is.na(e) & s <= entry[[i]] & e >= entry[[i]])
        if (length(hit)) e[[hit[[1L]]]] else NA
      })
    if (is.na(d)) {
      if (identical(spec$missing, "ineligible")) out$ineligible[[i]] <- TRUE
      next
    }
    out$date[[i]] <- as.Date(d) + spec$offset
  }
  out
}

# Retained outcome events of one outcome for every roster row: inside
# [entry, exit], inside an observed block, the washout rule over the raw
# events of the outcome (an event within washout days of the previous raw
# event is not a new event), and same-day priority.
.survivalRetained <- function(rows, keys, entry, exit, blocks, washout, censor_wins) {
  if (!nrow(rows)) return(rows[0, , drop = FALSE])
  o <- .outputOrderRows(rows)
  rows <- rows[o, , drop = FALSE]
  keep <- rep(FALSE, nrow(rows))
  at <- match(as.character(rows$row_key), as.character(keys))
  for (ix in split(seq_len(nrow(rows)), as.character(rows$row_key))) {
    i <- at[[ix[[1L]]]]
    if (is.na(i) || is.na(entry[[i]]) || is.na(exit[[i]])) next
    d <- rows$start_date[ix]
    new <- if (washout > 0L) {
      prev <- c(as.Date(NA), d[-length(d)])
      is.na(prev) | as.integer(d - prev) > washout
    } else rep(TRUE, length(d))
    inside <- d >= entry[[i]] & (if (censor_wins) d < exit[[i]] else d <= exit[[i]])
    b <- blocks[[i]]
    observed <- vapply(d, function(x) any(b$start <= x & b$end >= x), logical(1))
    keep[ix] <- new & inside & observed
  }
  out <- rows[keep, , drop = FALSE]
  attr(out, "source_ordering") <- attr(rows, "source_ordering", exact = TRUE)
  out
}

# Preserve each stream's declared tie-breakers, including adapter member
# tables and parent/course identity, through private R assembly.
.outputOrderRows <- function(rows, ordering = NULL) {
  columns <- ordering %||% attr(rows, "source_ordering", exact = TRUE) %||%
    c("row_key", "start_date", "source_ns", "member_table", "event_id", "parent_event_id")
  .reduceOrderRows(rows, intersect(columns, names(rows)))
}

#' Compile a survival output (section 6.12; amendments A-5, A-7; defects
#' A-14, A-E12)
#'
#' Per TAR: the start and end bounds resolved independently per episode, the
#' effective entry (the later of the TAR start and \code{entry}), the
#' explicit censor list only (\code{index_end} is one selectable source; no
#' implicit cohort-end cap unless the v1 flag \code{legacy_cohort_end_cap}),
#' event / interval / \code{omop_censor} sources with \code{which},
#' \code{offset}, \code{search}, \code{missing} and \code{when_index_source},
#' \code{prior_outcome} relative to the entry (exclude, censor -- a censor date
#' before the entry makes the episode ineligible --, ignore), observation: the
#' risk ends at the last observed day before the first gap after entry
#' (counting process with \code{gap_policy = "reenter"} resumes at the next
#' observed segment), outcomes only inside observed segments,
#' \code{same_day_priority}, \code{washout_days}, \code{tie_policy}, and
#' \code{complete = TRUE} sentinels for recurrent events. One component per
#' TAR (and per recurrent-events part).
#' @keywords internal
.compileSurvivalOutput <- function(ctx, node, name, inputs) {
  opts <- node[["options"]] %||% list()
  fmt <- opts[["format"]] %||% "survival"
  field <- paste0("/outputs/", name, "/options")
  # the v1 shapes run on the v1 executor (A-14); only the cohort-end cap has
  # a native meaning
  for (flag in c("legacy_single_outcome", "legacy_states", "legacy_state_step")) {
    if (!is.null(opts[[flag]]) && !isFALSE(unlist(opts[[flag]]))) {
      .irReject(paste0("output '", name, "' carries the v1 compatibility variant ", flag,
                       ", which runs on the v1 executors"), field = paste0(field, "/", flag))
    }
  }
  pop <- inputs$population
  cons <- .reduceConsumer(ctx, name, pop$roster, pop$coverage, "episode")
  if (is.null(cons$coverage)) {
    .irReject("a survival output needs a population with observation coverage", field = field)
  }
  tars <- .outputTarList(node, name)
  for (t in names(tars)) {
    if (is.null(.reduceWindowSides(tars[[t]])$lower)) {
      .irReject(paste0("TAR '", t, "' needs a start bound (an unbounded start has no entry)"),
                field = paste0(field, if (length(opts[["tars"]] %||% list())) paste0("/tars/", t) else "/tar"))
    }
  }
  outcome_names <- names(opts[["outcomes"]])
  sorted_outcomes <- .outputSortedNames(opts[["outcomes"]])
  if (identical(fmt, "multi_state") && is.null(opts[["transitions"]])) {
    .irReject("multi_state needs transitions", field = paste0(field, "/transitions"))
  }
  roster <- .reduceRosterFrame(ctx, pop$roster)
  keys <- roster$cohort_row_id
  n <- length(keys)
  index <- as.Date(roster$cohort_start_date)
  seg <- .reduceFetch(ctx, .compileObservedSegments(ctx, cons$coverage, pop$roster, "final"),
                      order = c("row_key", "start_date"), field = field)
  blocks <- .survivalBlocks(seg, keys)
  censors <- .irItems(opts[["censor"]])
  specs <- lapply(censors, .survivalCensorSpec)
  needs_death <- any(vapply(specs, function(x) identical(x$kind, "death"), logical(1)))
  death <- if (needs_death) {
    dd <- .reduceDeathFrame(ctx, cons)
    as.Date(dd$death_date[match(as.character(keys), as.character(dd$row_key))])
  } else as.Date(rep(NA_character_, n))
  prior <- opts[["prior_outcome"]]
  prior_action <- if (is.null(prior)) "ignore" else prior[["action"]] %||% "exclude"
  lookback <- if (is.null(prior)) NULL else prior[["lookback"]]
  washout <- as.integer(unlist(opts[["washout_days"]] %||% 0L))
  extend <- if (!identical(prior_action, "ignore") && is.null(lookback)) NA_integer_ else
    max(washout, if (!identical(prior_action, "ignore")) as.integer(unlist(lookback)) else 0L)
  # an unavailable censor source cannot be evaluated: the output is
  # published with status "unavailable" (section 2.3), as for an outcome
  censor_unavailable <- FALSE
  censor_rows <- lapply(seq_along(specs), function(ci) {
    sp <- specs[[ci]]
    if (!sp$kind %in% c("source", "omop_censor")) return(NULL)
    src <- .reduceSource(ctx, sp$source)
    if (!src$available) {
      censor_unavailable <<- TRUE
      return(NULL)
    }
    st <- .reduceStream(ctx, cons, src, sp$search, .fragmentName("ox", name, paste0("c_", ci), ctx = ctx))
    got <- .reduceStreamRows(ctx, cons, st, sp$search, paste0(field, "/censor/", ci - 1L))
    got$rows[.outputOrderRows(got$rows, st$ordering), , drop = FALSE]
  })
  censor_wins <- identical(opts[["same_day_priority"]] %||% "event", "censor")
  gap_reenter <- identical(fmt, "counting_process") &&
    identical(opts[["gap_policy"]] %||% "censor_at_gap", "reenter")
  parts <- list()
  status <- if (censor_unavailable) "unavailable" else "ok"
  for (ti in seq_along(tars)) {
    t <- names(tars)[[ti]]
    tar <- tars[[t]]
    b <- .reduceBoundsR(ctx, cons, tar, field)
    entry <- b$lo
    defined <- b$defined
    if (!is.null(opts[["entry"]])) {
      e <- .reduceBoundsR(ctx, cons, list(start = opts[["entry"]], end = NULL, anchor = "index"), field)
      entry <- as.Date(ifelse(is.na(e$lo) | entry > e$lo, entry, e$lo), origin = "1970-01-01")
      defined <- defined & e$defined
    }
    entry[!defined] <- NA
    # observed block containing the entry; the first gap after it
    in_block <- vapply(seq_len(n), function(i) {
      if (is.na(entry[[i]])) return(NA_integer_)
      bl <- blocks[[i]]
      hit <- which(bl$start <= entry[[i]] & bl$end >= entry[[i]])
      if (length(hit)) hit[[1L]] else NA_integer_
    }, integer(1))
    block_end <- as.Date(vapply(seq_len(n), function(i) {
      if (is.na(in_block[[i]])) NA_real_ else as.numeric(blocks[[i]]$end[[in_block[[i]]]])
    }, numeric(1)), origin = "1970-01-01")
    last_obs <- as.Date(vapply(seq_len(n), function(i) {
      bl <- blocks[[i]]
      if (!nrow(bl)) NA_real_ else as.numeric(max(bl$end))
    }, numeric(1)), origin = "1970-01-01")
    eligible <- !is.na(entry) & !is.na(in_block)
    exit <- b$hi
    cap <- function(d) {
      d <- as.Date(d)
      exit <<- as.Date(ifelse(is.na(exit) | (!is.na(d) & d < exit), d, exit), origin = "1970-01-01")
    }
    for (ci in seq_along(censors)) {
      res <- .survivalCensorDates(censors[[ci]], if (is.data.frame(censor_rows[[ci]])) censor_rows[[ci]] else
                                    data.frame(row_key = keys[0], start_date = as.Date(character(0)),
                                               end_date = as.Date(character(0))),
                                  keys, entry, roster$index_source, roster, death)
      cap(res$date)
      eligible <- eligible & !res$ineligible
    }
    if (isTRUE(unlist(opts[["legacy_cohort_end_cap"]]))) cap(roster$cohort_end_date)
    cap(if (gap_reenter) last_obs else block_end)
    # outcome events per outcome (declared order is the outcome priority)
    events <- list()
    for (k in outcome_names) {
      ki <- match(k, sorted_outcomes)
      src <- .reduceSource(ctx, opts[["outcomes"]][[k]])
      if (!src$available) {
        status <- "unavailable"
        events[[k]] <- data.frame(row_key = keys[0], start_date = as.Date(character(0)),
                                  event_id = numeric(0))
        next
      }
      lower <- .reduceWindowSides(tar)$lower
      win <- list(start = if (is.na(extend)) NULL else list(anchor = lower$anchor, offset = lower$offset - extend),
                  end = .reduceWindowSides(tar)$upper, anchor = "index")
      st <- .reduceStream(ctx, cons, src, win, .fragmentName("ox", name, paste0("k_", ti, "_", ki), ctx = ctx))
      events[[k]] <- .reduceStreamRows(ctx, cons, st, win, paste0(field, "/outcomes/", k))$rows
      attr(events[[k]], "source_ordering") <- st$ordering
    }
    # prior outcome: any declared outcome in [entry - lookback, entry)
    if (!identical(prior_action, "ignore")) {
      lb <- if (is.null(lookback)) NA_integer_ else as.integer(unlist(lookback))
      prior_hit <- rep(FALSE, n)
      for (k in outcome_names) {
        ev <- events[[k]]
        if (!nrow(ev)) next
        i <- match(as.character(ev$row_key), as.character(keys))
        hit <- !is.na(i) & !is.na(entry[i]) & ev$start_date < entry[i] &
          (is.na(lb) | ev$start_date >= entry[i] - lb)
        prior_hit[unique(i[hit])] <- TRUE
      }
      eligible <- eligible & !prior_hit
    }
    eligible <- eligible & !is.na(exit) & exit >= entry
    entry[!eligible] <- NA
    exit[!eligible] <- NA
    retained <- lapply(outcome_names, function(k) {
      .survivalRetained(events[[k]], keys, entry, exit, blocks, washout, censor_wins)
    })
    names(retained) <- outcome_names
    family <- .survivalFamily(fmt, opts, t, name, length(opts[["tars"]] %||% list()) > 0L)
    rows <- .survivalEmit(fmt, opts, keys, roster, index, entry, exit, eligible, retained,
                          outcome_names, blocks, gap_reenter)
    for (part_name in names(rows)) {
      comp <- family[[part_name]]
      frame <- rows[[part_name]]
      .outputRowCap(nrow(frame), name)
      columns <- .survivalColumns(part_name, cons, fmt)
      grain <- if (identical(part_name, "transition_ref")) "none" else "episode"
      if (identical(grain, "none") && any(vapply(parts, function(p) identical(p$candidate$component, comp),
                                                 logical(1)))) next
      parts[[length(parts) + 1L]] <- .outputComponent(inputs, name, comp, frame, columns,
                                                      if (identical(grain, "none")) NULL else cons,
                                                      grain = grain, status = status)
    }
  }
  .outputCompiled(name, parts, status = status)
}

# Allocate generated component names together, preserving declared TAR names
# and ordinary readable names. Hash only overlength/colliding generated names;
# collision probing also covers an adversarial user TAR matching a hash name.
.outputAllocateComponents <- function(readable, identities, fixed = rep(FALSE, length(readable))) {
  out <- rep(NA_character_, length(readable))
  out[fixed] <- readable[fixed]
  repeated <- duplicated(readable) | duplicated(readable, fromLast = TRUE)
  plain <- !fixed & !repeated & nchar(readable) <= 64L & grepl(.NAME_PATTERN, readable)
  out[plain] <- readable[plain]
  used <- out[!is.na(out)]
  for (i in order(identities, method = "radix")) {
    if (!is.na(out[[i]])) next
    hash <- .fragmentHash(identities[[i]])
    probe <- 1L
    repeat {
      tail <- paste0("_", hash, if (probe > 1L) paste0("_", probe))
      candidate <- paste0(substr(readable[[i]], 1L, 64L - nchar(tail)), tail)
      if (!candidate %in% used) break
      probe <- probe + 1L
    }
    out[[i]] <- candidate
    used <- c(used, candidate)
  }
  if (anyDuplicated(out) || any(!grepl(.NAME_PATTERN, out))) {
    stop("Output component allocation failed its distinctness check.", call. = FALSE)
  }
  out
}

# Component names of one TAR's family, allocated against all sibling TARs.
.survivalFamily <- function(fmt, opts, tar, output, multi) {
  tars <- if (multi) .outputSortedNames(opts[["tars"]]) else tar
  part_names <- switch(fmt, recurrent_events = c("events", "risk_sets"),
                       multi_state = "msdata", "primary")
  readable <- identities <- character(0)
  fixed <- logical(0)
  for (t in tars) for (p in part_names) {
    readable <- c(readable, if (identical(fmt, "recurrent_events")) {
      if (multi) paste0(t, "_", p) else p
    } else if (multi) t else output)
    identities <- c(identities, .jcsCanonical(list(output, t, p)))
    fixed <- c(fixed, !identical(fmt, "recurrent_events"))
  }
  if (identical(fmt, "multi_state")) {
    readable <- c(readable, "transitionRef")
    identities <- c(identities, .jcsCanonical(list(output, "", "transition_ref")))
    fixed <- c(fixed, FALSE)
  }
  allocated <- .outputAllocateComponents(readable, identities, fixed)
  pick <- vapply(part_names, function(p) .jcsCanonical(list(output, tar, p)), "")
  out <- as.list(stats::setNames(allocated[match(pick, identities)], part_names))
  if (identical(fmt, "multi_state")) out$transition_ref <- tail(allocated, 1L)
  out
}

# Declared columns of a survival component (A-5; v1 shapes for the counting
# process and the multi-state risk set).
.survivalColumns <- function(part, cons, fmt) {
  id <- .outputIdentityColumns(cons)
  int <- function() .fragmentColumn("integer", TRUE, "measure")
  switch(part,
    primary = if (identical(fmt, "counting_process")) {
      c(id, list(outcome_name = .fragmentColumn("text", TRUE, "measure"), event = int(),
                 interval_number = int(), interval_start_days = int(), interval_end_days = int()))
    } else {
      c(id, list(outcome_name = .fragmentColumn("text", TRUE, "measure"), event = int(),
                 entry_days_from_index = int(), exit_days_from_index = int(), follow_up_days = int()))
    },
    events = c(id, list(outcome_name = .fragmentColumn("text", TRUE, "measure"), event = int(),
                        event_number = int(), outcome_event_number = int(),
                        event_days_from_index = int(), entry_days_from_index = int(),
                        exit_days_from_index = int())),
    risk_sets = c(id, list(entry_days_from_index = int(), exit_days_from_index = int(),
                           follow_up_days = int())),
    msdata = c(id, list(from = int(), to = int(), trans = int(),
                        Tstart = .fragmentColumn("numeric", TRUE, "measure"),
                        Tstop = .fragmentColumn("numeric", TRUE, "measure"),
                        time = .fragmentColumn("numeric", TRUE, "measure"), status = int(),
                        from_name = .fragmentColumn("text", TRUE, "measure"),
                        to_name = .fragmentColumn("text", TRUE, "measure"),
                        state_visit_number = int())),
    transition_ref = list(from = int(), to = int(), trans = int(),
                          from_name = .fragmentColumn("text", TRUE, "measure"),
                          to_name = .fragmentColumn("text", TRUE, "measure"),
                          from_hierarchy_rank = int(), to_hierarchy_rank = int(),
                          from_is_initial = .fragmentColumn("boolean", TRUE, "measure"),
                          to_is_absorbing = .fragmentColumn("boolean", TRUE, "measure"),
                          tie_policy = .fragmentColumn("text", TRUE, "measure"),
                          state_step = .fragmentColumn("numeric", TRUE, "measure")))
}

# The ordered candidate events of one episode across outcomes:
# data.frame(outcome, priority, date, key) sorted by (date, priority, key).
.survivalEpisodeEvents <- function(retained, outcome_names, key) {
  parts <- lapply(seq_along(outcome_names), function(p) {
    ev <- retained[[outcome_names[[p]]]]
    if (!nrow(ev)) return(NULL)
    ix <- which(as.character(ev$row_key) == as.character(key))
    if (!length(ix)) return(NULL)
    ix <- ix[.outputOrderRows(ev[ix, , drop = FALSE], attr(ev, "source_ordering", exact = TRUE))]
    data.frame(outcome = outcome_names[[p]], priority = p, date = ev$start_date[ix],
               source = as.character(ev$source_ns[ix] %||% rep("", length(ix))),
               key = as.character(ev$event_id[ix]), source_order = seq_along(ix), stringsAsFactors = FALSE)
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) {
    return(data.frame(outcome = character(0), priority = integer(0), date = as.Date(character(0)),
                      source = character(0), key = character(0), source_order = integer(0), stringsAsFactors = FALSE))
  }
  x <- do.call(rbind, parts)
  x[order(x$date, x$priority, x$source_order, method = "radix"), , drop = FALSE]
}

# Emit the component frames of one TAR.
.survivalEmit <- function(fmt, opts, keys, roster, index, entry, exit, eligible, retained,
                          outcome_names, blocks, gap_reenter) {
  ids <- which(eligible)
  person <- roster$person_id
  days <- function(d, i) as.integer(as.Date(d) - index[[i]])
  order_mode <- opts[["event_order"]] %||% if (fmt %in% c("survival", "competing_risk")) "first" else "all"
  composite <- identical(opts[["outcome_mode"]] %||% (if (identical(fmt, "survival")) "composite" else "named"),
                         "composite")
  tie <- opts[["tie_policy"]] %||% "priority"
  ident <- function(i) data.frame(row_id = keys[[i]], cohort_row_id = keys[[i]], person_id = person[[i]],
                                  stringsAsFactors = FALSE)
  bind <- function(rows, template) {
    rows <- Filter(Negate(is.null), rows)
    if (length(rows)) do.call(rbind, rows) else template
  }
  empty_ident <- data.frame(row_id = keys[0], cohort_row_id = keys[0], person_id = person[0],
                            stringsAsFactors = FALSE)
  single <- function(i, ev, outcome_label) {
    entry_d <- days(entry[[i]], i)
    if (nrow(ev)) {
      pick <- if (identical(order_mode, "last")) {
        ev[order(-as.numeric(ev$date), ev$priority, -ev$source_order, method = "radix"), , drop = FALSE][1L, ]
      } else ev[1L, ]
      exit_d <- days(pick$date, i)
      cbind(ident(i), data.frame(outcome_name = outcome_label %||% pick$outcome, event = 1L,
                                 entry_days_from_index = entry_d, exit_days_from_index = exit_d,
                                 follow_up_days = exit_d - entry_d, stringsAsFactors = FALSE))
    } else {
      exit_d <- days(exit[[i]], i)
      cbind(ident(i), data.frame(outcome_name = if (is.null(outcome_label)) NA_character_ else outcome_label,
                                 event = 0L, entry_days_from_index = entry_d,
                                 exit_days_from_index = exit_d, follow_up_days = exit_d - entry_d,
                                 stringsAsFactors = FALSE))
    }
  }
  single_template <- cbind(empty_ident, data.frame(outcome_name = character(0), event = integer(0),
                                                   entry_days_from_index = integer(0),
                                                   exit_days_from_index = integer(0),
                                                   follow_up_days = integer(0), stringsAsFactors = FALSE))
  switch(fmt,
    survival = {
      rows <- list()
      for (i in ids) {
        ev_all <- .survivalEpisodeEvents(retained, outcome_names, keys[[i]])
        if (composite) {
          label <- if (length(outcome_names) == 1L) outcome_names[[1L]] else NULL
          rows[[length(rows) + 1L]] <- single(i, ev_all, label)
        } else {
          for (k in outcome_names) {
            rows[[length(rows) + 1L]] <- single(i, ev_all[ev_all$outcome == k, , drop = FALSE], k)
          }
        }
      }
      list(primary = bind(rows, single_template))
    },
    competing_risk = {
      rows <- lapply(ids, function(i) {
        single(i, .survivalEpisodeEvents(retained, outcome_names, keys[[i]]), NULL)
      })
      list(primary = bind(rows, single_template))
    },
    recurrent_events = {
      ev_rows <- list()
      risk_rows <- list()
      for (i in ids) {
        ev <- .survivalEpisodeEvents(retained, outcome_names, keys[[i]])
        if (identical(tie, "priority") && nrow(ev)) ev <- ev[!duplicated(ev$date), , drop = FALSE]
        if (identical(order_mode, "first") && nrow(ev)) ev <- ev[1L, , drop = FALSE]
        entry_d <- days(entry[[i]], i)
        exit_d <- days(exit[[i]], i)
        risk_rows[[length(risk_rows) + 1L]] <- cbind(ident(i), data.frame(
          entry_days_from_index = entry_d, exit_days_from_index = exit_d,
          follow_up_days = exit_d - entry_d))
        if (nrow(ev)) {
          oen <- stats::ave(seq_len(nrow(ev)), ev$outcome, FUN = seq_along)
          ev_rows[[length(ev_rows) + 1L]] <- cbind(ident(i)[rep(1L, nrow(ev)), , drop = FALSE], data.frame(
            outcome_name = ev$outcome, event = 1L, event_number = seq_len(nrow(ev)),
            outcome_event_number = as.integer(oen),
            event_days_from_index = as.integer(ev$date - index[[i]]),
            entry_days_from_index = entry_d, exit_days_from_index = exit_d, stringsAsFactors = FALSE))
        } else if (isTRUE(unlist(opts[["complete"]]))) {
          ev_rows[[length(ev_rows) + 1L]] <- cbind(ident(i), data.frame(
            outcome_name = NA_character_, event = 0L, event_number = 1L,
            outcome_event_number = NA_integer_, event_days_from_index = NA_integer_,
            entry_days_from_index = entry_d, exit_days_from_index = exit_d, stringsAsFactors = FALSE))
        }
      }
      ev_template <- cbind(empty_ident, data.frame(outcome_name = character(0), event = integer(0),
                                                   event_number = integer(0),
                                                   outcome_event_number = integer(0),
                                                   event_days_from_index = integer(0),
                                                   entry_days_from_index = integer(0),
                                                   exit_days_from_index = integer(0),
                                                   stringsAsFactors = FALSE))
      risk_template <- cbind(empty_ident, data.frame(entry_days_from_index = integer(0),
                                                     exit_days_from_index = integer(0),
                                                     follow_up_days = integer(0)))
      list(events = bind(ev_rows, ev_template), risk_sets = bind(risk_rows, risk_template))
    },
    counting_process = {
      rows <- list()
      for (i in ids) {
        ev <- .survivalEpisodeEvents(retained, outcome_names, keys[[i]])
        if (identical(tie, "priority") && nrow(ev)) ev <- ev[!duplicated(ev$date), , drop = FALSE]
        if (identical(order_mode, "first") && nrow(ev)) ev <- ev[1L, , drop = FALSE]
        segments <- if (gap_reenter) {
          bl <- blocks[[i]]
          s <- pmax(bl$start, entry[[i]])
          e <- pmin(bl$end, exit[[i]])
          keep <- s <= e
          data.frame(start = s[keep], end = e[keep])
        } else data.frame(start = entry[[i]], end = exit[[i]])
        number <- 0L
        for (sg in seq_len(nrow(segments))) {
          from <- days(segments$start[[sg]], i) - 1L
          seg_ev <- ev[ev$date >= segments$start[[sg]] & ev$date <= segments$end[[sg]], , drop = FALSE]
          last_day <- from
          for (j in seq_len(nrow(seg_ev))) {
            number <- number + 1L
            stop_day <- days(seg_ev$date[[j]], i)
            rows[[length(rows) + 1L]] <- cbind(ident(i), data.frame(
              outcome_name = seg_ev$outcome[[j]], event = 1L, interval_number = number,
              interval_start_days = last_day, interval_end_days = stop_day, stringsAsFactors = FALSE))
            last_day <- stop_day
          }
          seg_end <- days(segments$end[[sg]], i)
          if (last_day < seg_end) {
            number <- number + 1L
            rows[[length(rows) + 1L]] <- cbind(ident(i), data.frame(
              outcome_name = NA_character_, event = 0L, interval_number = number,
              interval_start_days = last_day, interval_end_days = seg_end, stringsAsFactors = FALSE))
          }
          if (identical(order_mode, "first") && nrow(seg_ev)) break
        }
      }
      template <- cbind(empty_ident, data.frame(outcome_name = character(0), event = integer(0),
                                                interval_number = integer(0),
                                                interval_start_days = integer(0),
                                                interval_end_days = integer(0), stringsAsFactors = FALSE))
      list(primary = bind(rows, template))
    },
    multi_state = .survivalMultistate(opts, keys, person, index, entry, exit, ids, retained,
                                      outcome_names, tie),
    .irReject(paste0("unknown survival format '", fmt, "'")))
}

# Native multi-state risk sets: the episode risk interval and its retained
# events fed to the reviewed state machine (.newMultistateStreamTransformer()).
.survivalMultistate <- function(opts, keys, person, index, entry, exit, ids, retained,
                                outcome_names, tie) {
  edges <- .irItems(opts[["transitions"]])
  initial <- as.character(unlist(opts[["initial_state"]] %||% "index"))
  states <- unique(c(initial, outcome_names))
  outcomes <- lapply(seq_along(outcome_names), function(p) list(name = outcome_names[[p]], priority = p))
  transitions <- list(states = states, edges = lapply(seq_along(edges), function(j) {
    list(from = as.character(unlist(edges[[j]][["from"]])), to = as.character(unlist(edges[[j]][["to"]])),
         trans = j)
  }))
  spec <- tryCatch(.normalizeMultistateSpec(outcomes, transitions, initial,
                                            unlist(opts[["state_hierarchy"]]), opts[["state_step"]]),
                   error = function(e) .irReject(conditionMessage(e), field = "/outputs/options/transitions"))
  state_of <- stats::setNames(spec$states$state_id, spec$states$state_name)
  rank_of <- stats::setNames(spec$states$hierarchy_rank, spec$states$state_name)
  stream <- list()
  for (i in ids) {
    entry_d <- as.integer(entry[[i]] - index[[i]])
    exit_d <- as.integer(exit[[i]] - index[[i]])
    stream[[length(stream) + 1L]] <- data.frame(
      cohort_row_id = keys[[i]], person_id = person[[i]], stream_kind = 0L,
      state_id = spec$initial_state_id, state_time = entry_d - 1, state_day = entry_d - 1L,
      within_day_order = 0L, within_day_count = 0L, risk_end_days = exit_d, stringsAsFactors = FALSE)
    ev <- .survivalEpisodeEvents(retained, outcome_names, keys[[i]])
    ev <- ev[!duplicated(paste(ev$outcome, ev$date)), , drop = FALSE]
    if (nrow(ev)) {
      day <- as.integer(ev$date - index[[i]])
      rank <- rank_of[ev$outcome]
      o <- order(day, rank, state_of[ev$outcome])
      ev <- ev[o, , drop = FALSE]
      day <- day[o]
      wdo <- stats::ave(seq_along(day), day, FUN = seq_along)
      wdc <- stats::ave(seq_along(day), day, FUN = length)
      time <- if (identical(tie, "sequential")) day - (wdc - wdo) * spec$state_step else as.numeric(day)
      stream[[length(stream) + 1L]] <- data.frame(
        cohort_row_id = keys[[i]], person_id = person[[i]], stream_kind = 1L,
        state_id = as.integer(state_of[ev$outcome]), state_time = time, state_day = day,
        within_day_order = as.integer(wdo), within_day_count = as.integer(wdc),
        risk_end_days = exit_d, stringsAsFactors = FALSE)
    }
    stream[[length(stream) + 1L]] <- data.frame(
      cohort_row_id = keys[[i]], person_id = person[[i]], stream_kind = 2L, state_id = NA_integer_,
      state_time = exit_d, state_day = exit_d, within_day_order = 0L, within_day_count = 0L,
      risk_end_days = exit_d, stringsAsFactors = FALSE)
  }
  machine <- .newMultistateStreamTransformer(list(multistate = spec, tie_policy = tie),
                                             max_rows = .reduceRowCap())
  ms <- if (length(stream)) machine$transform(do.call(rbind, stream)) else .emptyMultistateData()
  machine$assert_complete()
  ms$person_id <- person[match(as.character(ms$cohort_row_id), as.character(keys))]
  ms$row_id <- ms$cohort_row_id
  ref <- .multistateTransitionReference(spec, tie)
  list(msdata = ms, transition_ref = ref)
}

# --- source identities and the covariate registry ---------------------------------------------

#' The concept identities of an output source (amendment A-3)
#'
#' From the plan and the vocabulary, never from the clinical rows at a site:
#' an event source's resolved concept set, the declared concepts first in
#' declaration order, then the other resolved concepts ascending; an interval
#' source is one combined identity (NA). An unrestricted or adapter source has
#' no reviewed identity set and is refused for identity-keyed outputs.
#'
#' @param ctx Compilation context.
#' @param src A typed source reference.
#' @param path JSON path for rejections.
#' @return list(name, kind, ids (integer, or NA for a combined identity),
#'   source = .reduceSource()).
#' @keywords internal
.outputSourceIdentities <- function(ctx, src, path) {
  s <- .reduceSource(ctx, src)
  if (identical(s$kind, "interval")) return(list(name = s$name, kind = "interval", ids = NA_integer_,
                                                 source = s))
  node <- ctx[["ir"]][["events"]][[s$name]]
  if (is.null(node[["table"]])) {
    .irReject(paste0("source '", s$name, "' (an adapter) has no reviewed concept identity set for ",
                     "identity-keyed columns"), field = path)
  }
  concept <- .compileConceptSet(ctx, node[["concept_set"]])
  if (identical(concept$state, "all")) {
    .irReject(paste0("source '", s$name, "' is unrestricted: identity-keyed columns need a declared ",
                     "concept set"), field = path)
  }
  ids <- as.integer(concept$ids %||% integer(0))
  cs <- node[["concept_set"]]
  if (is.character(cs) || (is.list(cs) && length(cs) == 1L && is.character(cs[[1L]]))) {
    cs <- ctx[["ir"]][["concept_sets"]][[as.character(unlist(cs))]]
  }
  declared <- suppressWarnings(as.integer(unlist(cs[["concepts"]] %||% list(), use.names = FALSE)))
  ordered <- c(declared[declared %in% ids], sort(setdiff(ids, declared), method = "radix"))
  list(name = s$name, kind = "event", ids = unique(ordered), source = s)
}

# Analysis order of the released columns: the closed enumeration
# (binary, count, mean, last, exposed_days).
.OUTPUT_ANALYSES <- c("binary", "count", "mean", "last", "exposed_days")

# The effective analyses of one source (A-14 source_analyses): exposed days
# only for interval sources, value analyses (mean, last) only for sources
# with a value role.
.outputSourceAnalyses <- function(opts, source_name, kind, has_value = TRUE) {
  analyses <- as.character(unlist(opts[["analyses"]] %||% list("binary"), use.names = FALSE))
  sa <- opts[["source_analyses"]][[source_name]]
  if (!is.null(sa)) analyses <- intersect(analyses, as.character(unlist(sa, use.names = FALSE)))
  if (identical(kind, "event")) analyses <- setdiff(analyses, "exposed_days")
  if (!isTRUE(has_value)) analyses <- setdiff(analyses, c("mean", "last"))
  analyses
}

# Does a resolved source carry a value role (an unavailable one counts as
# having its declared roles)?
.outputSourceHasValue <- function(ctx, idn) {
  if (identical(idn$kind, "interval")) return(FALSE)
  meta <- idn$source$meta
  if (identical(meta$availability, "unavailable")) {
    node <- ctx[["ir"]][["events"]][[idn$name]]
    if (!is.null(node[["value"]][["column"]])) return(TRUE)
    # Missing physical tables have no available roles. Infer only the
    # declared numeric role from the packaged CDM map, never invent a value
    # role for conditions, notes or another nonmeasurement source.
    version <- .roleRegistry(ctx[["handle"]])$cdm_version
    rows <- .roleRecordsCsv(version)
    return(!is.null(rows) && any(rows$table_name == node[["table"]] & nzchar(rows$value_col)))
  }
  "value" %in% meta$roles
}

#' The per-plan covariate registry of an output (section 6.12)
#'
#' A dense integer \code{covariate_id} for every \code{(source, concept_id,
#' analysis)} tuple across the plan's required sparse and temporal outputs.
#' Output declaration order, source order, identity order and analysis order
#' determine the first occurrence of a tuple; later consumers reuse its ID.
#' Each reference companion contains the tuples that its output uses.
#' @keywords internal
.outputCovariateRegistry <- function(ctx, node, name, analyses_of) {
  reg <- .outputCovariateRows(ctx, node, name, analyses_of)
  memo <- .compileMemo(ctx, "output_covariates")
  if (!exists("registry", envir = memo, inherits = FALSE)) {
    ir <- ctx[["ir"]]
    roots <- as.character(unlist(ir[["publish"]] %||% names(ir[["outputs"]]), use.names = FALSE))
    needed <- .irReach(roots, function(o) .irOutputDeps(ir, o), names(ir[["outputs"]]))
    all <- list()
    for (o in intersect(names(ir[["outputs"]]), needed)) {
      other <- ir[["outputs"]][[o]]
      fmt <- other[["format"]]
      if (!fmt %in% c("sparse", "temporal_covariates")) next
      opts <- other[["options"]] %||% list()
      if (isTRUE(unlist(opts[["legacy_covariate_ids"]]))) next
      analyses <- if (identical(fmt, "sparse")) {
        function(src, kind, has_value) intersect(
          as.character(unlist(opts[["analyses"]] %||% list("binary"), use.names = FALSE)),
          c("binary", "count"))
      } else function(src, kind, has_value) .outputSourceAnalyses(opts, src, kind, has_value)
      rows <- .outputCovariateRows(ctx, other, o, analyses)
      all[[length(all) + 1L]] <- rows[c("source", "concept_id", "analysis")]
    }
    # Standalone compiler callers may supply a node outside publish roots.
    all[[length(all) + 1L]] <- reg[c("source", "concept_id", "analysis")]
    tuples <- unique(do.call(rbind, all))
    .outputRowCap(nrow(tuples), name)
    assign("registry", tuples, envir = memo)
  }
  tuples <- get("registry", envir = memo, inherits = FALSE)
  key <- function(x) paste(x$source, x$concept_id, x$analysis, sep = "\r")
  reg$covariate_id <- match(key(reg), key(tuples))
  reg
}

# Local tuples retain source_index for dispatch; IDs are allocated once above.
.outputCovariateRows <- function(ctx, node, name, analyses_of) {
  srcs <- .outputSources(node)
  rows <- list()
  for (i in seq_along(srcs)) {
    idn <- .outputSourceIdentities(ctx, srcs[[i]], paste0("/outputs/", name, "/options/sources/", i - 1L))
    analyses <- analyses_of(idn$name, idn$kind, .outputSourceHasValue(ctx, idn))
    .outputRowCap(length(rows) + as.numeric(length(idn$ids)) * length(analyses), name)
    for (a in analyses) {
      for (cid in idn$ids) {
        rows[[length(rows) + 1L]] <- data.frame(source = idn$name, source_index = i,
                                                concept_id = cid, analysis = a,
                                                stringsAsFactors = FALSE)
      }
    }
  }
  reg <- if (length(rows)) do.call(rbind, rows) else
    data.frame(source = character(0), source_index = integer(0), concept_id = integer(0),
               analysis = character(0), stringsAsFactors = FALSE)
  reg
}

.outputCovariateRef <- function(ctx, reg) {
  ids <- reg$concept_id[!is.na(reg$concept_id)]
  names <- if (length(ids)) .reduceConceptNames(ctx, reg$concept_id) else rep(NA_character_, nrow(reg))
  data.frame(covariate_id = as.integer(reg$covariate_id), source = as.character(reg$source),
             concept_id = as.integer(reg$concept_id), concept_name = names,
             analysis = as.character(reg$analysis), stringsAsFactors = FALSE)
}

.outputCovariateRefColumns <- function() {
  list(covariate_id = .fragmentColumn("integer", FALSE, "measure"),
       source = .fragmentColumn("text", FALSE, "measure"),
       concept_id = .fragmentColumn("integer", TRUE, "concept"),
       concept_name = .fragmentColumn("text", TRUE, "measure"),
       analysis = .fragmentColumn("text", FALSE, "measure"))
}

# The rows of one output source for every roster row, inside the given
# window (exact, in R), with value and concept columns.
.outputSourceRows <- function(ctx, cons, src, window, name, field) {
  if (!src$available) return(NULL)
  extra <- list()
  cols <- names(src$fragment$columns)
  if ("value" %in% cols) extra$v <- list(expr = "e.value", type = "numeric")
  if ("concept_id" %in% cols) extra$concept_id <- list(expr = "e.concept_id", type = "integer")
  st <- .reduceStream(ctx, cons, src, window, name, extra = extra,
                      overlap = identical(src$kind, "interval"))
  got <- .reduceStreamRows(ctx, cons, st, window, field, overlap = identical(src$kind, "interval"))
  rows <- got$rows
  if (!"v" %in% names(rows)) rows$v <- rep(NA_real_, nrow(rows))
  if (!"concept_id" %in% names(rows)) rows$concept_id <- rep(NA_integer_, nrow(rows))
  if (nrow(rows)) rows <- rows[.outputOrderRows(rows, st$ordering), , drop = FALSE]
  rows
}

# Observed blocks of every roster row (recorded periods of its component,
# merged), from the population's coverage.
.outputBlocks <- function(ctx, cons, keys, field) {
  seg <- .reduceFetch(ctx, .compileObservedSegments(ctx, cons$coverage, cons$roster, "final"),
                      order = c("row_key", "start_date"), field = field)
  .survivalBlocks(seg, keys)
}

# --- person_period ----------------------------------------------------------------------------------

# Firsts of the calendar unit strictly after `from` and not after `to`.
.panelCalendarFirsts <- function(from, to, unit) {
  if (is.na(from) || is.na(to) || from >= to) return(as.Date(character(0)))
  y0 <- as.integer(format(from, "%Y"))
  m0 <- as.integer(format(from, "%m"))
  y1 <- as.integer(format(to, "%Y"))
  steps <- switch(unit, year = y1 - y0 + 1,
                  month = (y1 - y0) * 12 + as.integer(format(to, "%m")) - m0 + 1,
                  quarter = ((y1 - y0) * 12 + as.integer(format(to, "%m")) - m0) %/% 3 + 2)
  .outputRowCap(steps, "person_period")
  out <- switch(unit,
    year = as.Date(sprintf("%04d-01-01", seq.int(y0, y1))),
    month = {
      n <- (y1 - y0) * 12L + as.integer(format(to, "%m")) - m0
      ms <- (m0 - 1L) + seq.int(0L, n)
      as.Date(sprintf("%04d-%02d-01", y0 + ms %/% 12L, ms %% 12L + 1L))
    },
    quarter = {
      q0 <- (m0 - 1L) %/% 3L
      n <- ((y1 - y0) * 12L + as.integer(format(to, "%m")) - m0) %/% 3L + 1L
      qs <- q0 + seq.int(0L, n)
      as.Date(sprintf("%04d-%02d-01", y0 + qs %/% 4L, (qs %% 4L) * 3L + 1L))
    })
  out[out > from & out <= to]
}

#' The periods of one episode (section 6.12, partition algorithm)
#'
#' Boundaries seeded with \code{L} and \code{U + 1}, plus -- clipped to
#' \code{[L, U + 1]} -- the starts and ends + 1 of the observed blocks, the
#' bin starts, the starts and ends + 1 of every boundary interval, the dates
#' of every boundary event, calendar firsts and, with age bands, every
#' 1 January; closed periods \code{[b_i, b_{i+1} - 1]} kept when observed.
#' @keywords internal
.panelPeriods <- function(L, U, blocks, bins, partition, boundary_dates, output = "person_period") {
  if (is.na(L) || is.na(U) || L > U) return(data.frame(start = as.Date(character(0)), end = as.Date(character(0))))
  b <- c(L, U + 1L, blocks$start, blocks$end + 1L, boundary_dates)
  if (!is.null(bins[["width"]])) {
    w <- as.numeric(unlist(bins[["width"]]))
    nb <- as.numeric(U - L) %/% w + 1
    .outputRowCap(nb, output)
    b <- c(b, L + w * seq.int(0, nb))
  } else if (!is.null(bins[["calendar"]])) {
    b <- c(b, .panelCalendarFirsts(L, U, as.character(unlist(bins[["calendar"]]))))
  }
  cal <- partition[["calendar"]]
  if (!is.null(cal)) b <- c(b, .panelCalendarFirsts(L, U, as.character(unlist(cal))))
  if (!is.null(partition[["age_bands"]])) b <- c(b, .panelCalendarFirsts(L, U, "year"))
  b <- sort(unique(b[!is.na(b) & b >= L & b <= U + 1L]))
  if (length(b) < 2L) return(data.frame(start = as.Date(character(0)), end = as.Date(character(0))))
  s <- b[-length(b)]
  e <- b[-1L] - 1L
  observed <- vapply(seq_along(s), function(i) any(blocks$start <= s[[i]] & blocks$end >= e[[i]]), logical(1))
  data.frame(start = s[observed], end = e[observed])
}

# Union days of intervals [s, e] inside [lo, hi].
.panelUnionDays <- function(s, e, lo, hi) {
  cs <- pmax(s, lo)
  ce <- pmin(e, hi)
  ok <- !is.na(cs) & !is.na(ce) & cs <= ce
  .dayUnionLength(as.integer(cs[ok]), as.integer(ce[ok]))
}

#' Compile a person_period output (section 6.12; amendments A-3, A-14)
#' @keywords internal
.compilePanelOutput <- function(ctx, node, name, inputs) {
  opts <- node[["options"]] %||% list()
  field <- paste0("/outputs/", name, "/options")
  for (flag in c("legacy_composite", "legacy_covariate_ids")) {
    if (isTRUE(unlist(opts[[flag]]))) {
      .irReject(paste0("output '", name, "' carries the v1 compatibility variant ", flag,
                       ", which runs on the v1 executors"), field = paste0(field, "/", flag))
    }
  }
  pop <- inputs$population
  cons <- .reduceConsumer(ctx, name, pop$roster, pop$coverage, "episode")
  if (is.null(cons$coverage)) {
    .irReject("a person_period output needs a population with observation coverage", field = field)
  }
  dense <- identical(opts[["representation"]] %||% "dense", "dense")
  roster <- .reduceRosterFrame(ctx, pop$roster)
  keys <- roster$cohort_row_id
  n <- length(keys)
  index <- as.Date(roster$cohort_start_date)
  blocks <- .outputBlocks(ctx, cons, keys, field)
  window <- opts[["window"]]
  W <- .reduceBoundsR(ctx, cons, window, field)
  L <- as.Date(ifelse(is.na(W$lo) | (!is.na(roster$op_start) & roster$op_start > W$lo),
                      roster$op_start, W$lo), origin = "1970-01-01")
  U <- as.Date(ifelse(is.na(W$hi) | (!is.na(roster$op_end) & roster$op_end < W$hi),
                      roster$op_end, W$hi), origin = "1970-01-01")
  L[!W$defined | is.na(roster$op_start)] <- NA
  U[!W$defined | is.na(roster$op_end)] <- NA
  # the stream window: the requested window, extended back by the longest
  # carry-forward lookback
  carry <- opts[["carry_forward"]] %||% list()
  look <- max(c(0L, vapply(carry, function(c) as.integer(unlist(c[["lookback"]])), integer(1))))
  sides <- .reduceWindowSides(window)
  stream_window <- if (is.null(window)) NULL else list(
    start = if (is.null(sides$lower)) NULL else list(anchor = sides$lower$anchor, offset = sides$lower$offset - look),
    end = sides$upper, anchor = "index")
  ox <- function(kind, i) .fragmentName("ox", name, paste0(kind, "_", i), ctx = ctx)
  # boundary sources
  bnds <- .irItems(opts[["partition"]][["boundaries"]])
  boundary_rows <- lapply(seq_along(bnds), function(bi) {
    src <- .reduceSource(ctx, bnds[[bi]])
    rows <- .outputSourceRows(ctx, cons, src, window, ox("z", bi), paste0(field, "/partition/boundaries/", bi - 1L))
    if (!is.null(rows)) attr(rows, "boundary_kind") <- src$kind
    rows
  })
  # exposures (courses) and outcomes
  ex_names <- names(opts[["exposures"]] %||% list())
  sorted_ex <- .outputSortedNames(opts[["exposures"]])
  exposures <- lapply(ex_names, function(x) {
    src <- .reduceSource(ctx, opts[["exposures"]][[x]])
    .outputSourceRows(ctx, cons, src, window, ox("x", match(x, sorted_ex)), paste0(field, "/exposures/", x))
  })
  names(exposures) <- ex_names
  out_names <- names(opts[["outcomes"]] %||% list())
  sorted_out <- .outputSortedNames(opts[["outcomes"]])
  outcomes <- lapply(out_names, function(k) {
    src <- .reduceSource(ctx, opts[["outcomes"]][[k]])
    .outputSourceRows(ctx, cons, src, window, ox("y", match(k, sorted_out)), paste0(field, "/outcomes/", k))
  })
  names(outcomes) <- out_names
  # sources with identities, analyses and carry-forward
  srcs <- .outputSources(node)
  source_specs <- lapply(seq_along(srcs), function(i) {
    path <- paste0(field, "/sources/", i - 1L)
    idn <- .outputSourceIdentities(ctx, srcs[[i]], path)
    an <- .outputSourceAnalyses(opts, idn$name, idn$kind, .outputSourceHasValue(ctx, idn))
    an <- .OUTPUT_ANALYSES[.OUTPUT_ANALYSES %in% an]
    cf <- carry[[idn$name]]
    if (!is.null(cf) && identical(idn$kind, "interval")) {
      .irReject(paste0("carry_forward of interval source '", idn$name, "' has no value"), field = path)
    }
    rows <- .outputSourceRows(ctx, cons, idn$source, if (is.null(cf)) window else stream_window,
                              ox("s", i), path)
    list(idn = idn, analyses = an, carry = cf, rows = rows)
  })
  carry_names <- setdiff(names(carry), vapply(source_specs, function(s) s$idn$name, ""))
  if (length(carry_names)) {
    .irReject(paste0("carry_forward key '", carry_names[[1L]], "' names no source of this output"),
              field = paste0(field, "/carry_forward"))
  }
  # periods
  person_yob <- if (!is.null(opts[["partition"]][["age_bands"]])) {
    p <- .reducePersonFrame(ctx, cons)
    suppressWarnings(as.integer(p$year_of_birth[match(as.character(keys), as.character(p$row_key))]))
  } else NULL
  boundary_dates <- lapply(seq_len(n), function(i) as.Date(character(0)))
  for (rows in boundary_rows) {
    if (is.null(rows) || !nrow(rows)) next
    groups <- .reduceGroups(rows$row_key, keys)
    for (i in seq_len(n)) {
      ix <- groups[[i]]
      if (length(ix)) {
        boundary_dates[[i]] <- c(boundary_dates[[i]], rows$start_date[ix],
          if (identical(attr(rows, "boundary_kind"), "interval")) rows$end_date[ix] + 1L)
      }
    }
  }
  periods <- lapply(seq_len(n), function(i) {
    .panelPeriods(L[[i]], U[[i]], blocks[[i]], opts[["bins"]] %||% list(width = 30L),
                  opts[["partition"]] %||% list(), boundary_dates[[i]], output = name)
  })
  np <- vapply(periods, nrow, integer(1))
  .outputRowCap(sum(np), name)
  ep <- rep(seq_len(n), np)
  ps <- do.call(c, c(list(as.Date(character(0))), lapply(periods, function(p) p$start)))
  pe <- do.call(c, c(list(as.Date(character(0))), lapply(periods, function(p) p$end)))
  frame <- data.frame(row_id = keys[ep], cohort_row_id = keys[ep], person_id = roster$person_id[ep],
                      period_start_days = as.integer(ps - index[ep]),
                      period_end_days = as.integer(pe - index[ep]),
                      days = as.integer(pe - ps + 1L), stringsAsFactors = FALSE)
  int <- function() .fragmentColumn("integer", TRUE, "measure")
  columns <- c(.outputIdentityColumns(cons),
               list(period_start_days = int(), period_end_days = int(), days = int()))
  if (!is.null(person_yob)) {
    width <- as.integer(unlist(opts[["partition"]][["age_bands"]]))
    age <- as.integer(format(ps, "%Y")) - person_yob[ep]
    age[!is.na(age) & age < 0L] <- NA_integer_
    frame$age <- age
    frame$age_band <- as.integer(age %/% width)
    columns$age <- int()
    columns$age_band <- int()
  }
  bins_cal <- opts[["bins"]][["calendar"]]
  part_cal <- opts[["partition"]][["calendar"]]
  if (identical(bins_cal, "year") || identical(part_cal, "year")) {
    frame$calendar_year <- as.integer(format(ps, "%Y"))
    columns$calendar_year <- int()
  }
  if (identical(part_cal, "month")) {
    frame$calendar_month <- as.integer(format(ps, "%m"))
    columns$calendar_month <- int()
  }
  # period lookup of dated rows
  period_of <- function(rows) {
    if (is.null(rows) || !nrow(rows)) return(integer(0))
    out <- rep(NA_integer_, nrow(rows))
    first <- cumsum(c(1L, np))[seq_len(n)]
    at <- match(as.character(rows$row_key), as.character(keys))
    for (ix in split(seq_len(nrow(rows)), at)) {
      i <- at[[ix[[1L]]]]
      if (is.na(i) || np[[i]] == 0L) next
      p <- periods[[i]]
      j <- findInterval(as.numeric(rows$start_date[ix]), as.numeric(p$start))
      ok <- j >= 1L & rows$start_date[ix] <= p$end[pmax(j, 1L)]
      out[ix[ok]] <- first[[i]] + j[ok] - 1L
    }
    out
  }
  total <- nrow(frame)
  event_any <- rep(FALSE, total)
  unavailable <- any(vapply(boundary_rows, is.null, logical(1)))
  for (x in ex_names) {
    rows <- exposures[[x]]
    if (is.null(rows)) {
      unavailable <- TRUE
      frame[[paste0("exposed_", x)]] <- rep(NA_integer_, total)
      frame[[paste0("active_", x)]] <- rep(NA_integer_, total)
    } else {
      exp_days <- integer(total)
      act <- integer(total)
      at <- match(as.character(rows$row_key), as.character(keys))
      for (r in seq_len(total)) {
        ix <- which(at == ep[[r]])
        if (!length(ix)) next
        exp_days[[r]] <- .panelUnionDays(rows$start_date[ix], rows$end_date[ix], ps[[r]], pe[[r]])
        act[[r]] <- as.integer(any(rows$start_date[ix] <= ps[[r]] & rows$end_date[ix] >= ps[[r]]))
      }
      frame[[paste0("exposed_", x)]] <- exp_days
      frame[[paste0("active_", x)]] <- act
      event_any <- event_any | exp_days > 0L
    }
    columns[[paste0("exposed_", x)]] <- int()
    columns[[paste0("active_", x)]] <- int()
  }
  for (k in out_names) {
    rows <- outcomes[[k]]
    col <- paste0("n_", k)
    if (is.null(rows)) {
      unavailable <- TRUE
      frame[[col]] <- rep(NA_integer_, total)
    } else {
      p <- period_of(rows)
      frame[[col]] <- tabulate(p[!is.na(p)], nbins = total)
      event_any <- event_any | frame[[col]] > 0L
    }
    columns[[col]] <- int()
  }
  for (sp in source_specs) {
    idn <- sp$idn
    rows <- sp$rows
    single <- length(idn$ids) == 1L
    for (cid in idn$ids) {
      stem <- if (single) idn$name else paste0(idn$name, "_", cid)
      sel <- if (is.null(rows)) NULL else if (is.na(cid)) rows else rows[rows$concept_id %in% cid, , drop = FALSE]
      p <- if (is.null(sel)) integer(0) else period_of(sel)
      inside <- !is.na(p)
      if (!is.null(sel)) event_any <- event_any | tabulate(p[inside], nbins = total) > 0L
      for (a in sp$analyses) {
        col <- paste0(stem, "_", a)
        values <- if (is.null(sel)) {
          rep(NA, total)
        } else switch(a,
          binary = as.integer(tabulate(p[inside], nbins = total) > 0L),
          count = tabulate(p[inside], nbins = total),
          mean = {
            v <- rep(NA_real_, total)
            ok <- inside & !is.na(sel$v)
            if (any(ok)) {
              m <- tapply(sel$v[ok], p[ok], mean)
              v[as.integer(names(m))] <- as.numeric(m)
            }
            v
          },
          last = {
            v <- rep(NA_real_, total)
            if (any(inside)) {
              lastrow <- tapply(which(inside), p[inside], function(ix) ix[[length(ix)]])
              v[as.integer(names(lastrow))] <- sel$v[as.integer(lastrow)]
            }
            v
          },
          exposed_days = vapply(seq_len(total), function(r) {
            ix <- which(as.character(sel$row_key) == as.character(keys[[ep[[r]]]]))
            if (!length(ix)) 0L else .panelUnionDays(sel$start_date[ix], sel$end_date[ix], ps[[r]], pe[[r]])
          }, integer(1)))
        if (is.null(sel)) unavailable <- TRUE
        if (!is.null(sel) && identical(a, "exposed_days")) event_any <- event_any | values > 0L
        frame[[col]] <- if (a %in% c("mean", "last")) as.numeric(values) else as.integer(values)
        columns[[col]] <- if (a %in% c("mean", "last")) .fragmentColumn("numeric", TRUE, "measure") else int()
      }
      if (!is.null(sp$carry)) {
        col <- paste0(stem, "_carry")
        lb <- as.integer(unlist(sp$carry[["lookback"]]))
        ex <- sp$carry[["expire"]]
        ex <- if (is.null(ex)) NA_integer_ else as.integer(unlist(ex))
        values <- rep(NA_real_, total)
        if (!is.null(sel)) {
          nm <- sel[!is.na(sel$v), , drop = FALSE]
          at <- match(as.character(nm$row_key), as.character(keys))
          for (i in unique(ep)) {
            ix <- which(at == i)
            if (!length(ix)) next
            rr <- which(ep == i)
            d <- nm$start_date[ix]
            j <- findInterval(as.numeric(ps[rr]), as.numeric(d))
            ok <- j >= 1L
            age <- rep(NA_integer_, length(rr))
            age[ok] <- as.integer(ps[rr][ok] - d[j[ok]])
            ok <- ok & age <= lb & (is.na(ex) | age <= ex)
            values[rr[ok]] <- nm$v[ix][j[ok]]
          }
        } else unavailable <- TRUE
        frame[[col]] <- values
        columns[[col]] <- .fragmentColumn("numeric", TRUE, "measure")
      }
    }
  }
  .outputCheckNames(setdiff(names(columns), c("row_id", "cohort_row_id", "person_id")), name, "episode")
  if (!dense) frame <- frame[event_any, , drop = FALSE]
  status <- if (unavailable) "unavailable" else "ok"
  part <- .outputComponent(inputs, name, name, frame, columns, cons, grain = "episode", status = status)
  .outputCompiled(name, list(part), status = status)
}

# --- temporal_covariates ------------------------------------------------------------------------------

#' Compile a temporal_covariates output (section 6.12)
#'
#' Index-relative bins of \code{bin_width} days over a bounded, index-anchored
#' window; per (source, concept identity, analysis) covariates from the
#' per-plan registry; sparse \code{covariates} rows (non-zero / non-missing
#' values) for events inside the episode's observed segments, with the
#' \code{covariateRef} and \code{timeRef} reference companions.
#' @keywords internal
.compileTemporalOutput <- function(ctx, node, name, inputs) {
  opts <- node[["options"]] %||% list()
  field <- paste0("/outputs/", name, "/options")
  for (flag in c("legacy_composite", "legacy_covariate_ids")) {
    if (isTRUE(unlist(opts[[flag]]))) {
      .irReject(paste0("output '", name, "' carries the v1 compatibility variant ", flag,
                       ", which runs on the v1 executors"), field = paste0(field, "/", flag))
    }
  }
  window <- opts[["window"]]
  sides <- .reduceWindowSides(window)
  index_anchor <- function(b) !is.null(b) && identical(.anchorNormalize(b$anchor)$kind, "builtin") &&
    identical(.anchorNormalize(b$anchor)$name, "index")
  if (!index_anchor(sides$lower) || !index_anchor(sides$upper)) {
    .irReject("temporal_covariates needs a window with both bounds relative to the index",
              field = paste0(field, "/window"))
  }
  ws <- sides$lower$offset
  we <- sides$upper$offset
  if (we < ws) .irReject("the window end precedes its start", field = paste0(field, "/window"))
  width <- as.integer(unlist(opts[["bin_width"]] %||% 30L))
  nbins <- (as.numeric(we) - as.numeric(ws)) %/% width + 1
  .outputRowCap(nbins, name)
  time_ref <- data.frame(time_id = seq_len(nbins), start_day = as.integer(ws + as.numeric(width) * (seq_len(nbins) - 1L)),
                         stringsAsFactors = FALSE)
  time_ref$end_day <- as.integer(pmin(as.numeric(time_ref$start_day) + width - 1, we))
  pop <- inputs$population
  cons <- .reduceConsumer(ctx, name, pop$roster, pop$coverage, "episode")
  if (is.null(cons$coverage)) {
    .irReject("a temporal_covariates output needs a population with observation coverage", field = field)
  }
  roster <- .reduceRosterFrame(ctx, pop$roster)
  keys <- roster$cohort_row_id
  index <- as.Date(roster$cohort_start_date)
  blocks <- .outputBlocks(ctx, cons, keys, field)
  analyses_of <- function(src, kind, has_value) .outputSourceAnalyses(opts, src, kind, has_value)
  reg <- .outputCovariateRegistry(ctx, node, name, analyses_of)
  srcs <- .outputSources(node)
  out <- list()
  unavailable <- FALSE
  for (i in seq_along(srcs)) {
    path <- paste0(field, "/sources/", i - 1L)
    idn <- .outputSourceIdentities(ctx, srcs[[i]], path)
    rows <- .outputSourceRows(ctx, cons, idn$source, window,
                              .fragmentName("ox", name, paste0("s_", i), ctx = ctx), path)
    if (is.null(rows)) { unavailable <- TRUE; next }
    at <- match(as.character(rows$row_key), as.character(keys))
    day <- as.integer(rows$start_date - index[at])
    obs <- vapply(seq_len(nrow(rows)), function(r) {
      b <- blocks[[at[[r]]]]
      any(b$start <= rows$start_date[[r]] & b$end >= rows$start_date[[r]])
    }, logical(1))
    bin <- ifelse(day >= ws & day <= we & obs, (day - ws) %/% width + 1L, NA_integer_)
    sub <- reg[reg$source_index == i, , drop = FALSE]
    for (k in seq_len(nrow(sub))) {
      a <- sub$analysis[[k]]
      cid <- sub$concept_id[[k]]
      sel <- if (is.na(cid)) rep(TRUE, nrow(rows)) else rows$concept_id %in% cid
      sel <- sel & !is.na(bin)
      if (identical(a, "exposed_days")) {
        cells <- list()
        for (e in unique(at)) {
          ix <- which(at == e & (is.na(cid) | rows$concept_id %in% cid))
          for (t in seq_len(nbins)) {
            lo <- index[[e]] + time_ref$start_day[[t]]
            hi <- index[[e]] + time_ref$end_day[[t]]
            observed <- blocks[[e]]
            dd <- sum(vapply(seq_len(nrow(observed)), function(j) {
              start <- max(lo, observed$start[[j]])
              end <- min(hi, observed$end[[j]])
              if (start > end) 0L else .panelUnionDays(rows$start_date[ix], rows$end_date[ix], start, end)
            }, integer(1)))
            if (dd > 0L) cells[[length(cells) + 1L]] <- c(e, t, dd)
          }
        }
        if (length(cells)) {
          m <- do.call(rbind, cells)
          out[[length(out) + 1L]] <- data.frame(ep = m[, 1], time_id = m[, 2], covariate_id = sub$covariate_id[[k]],
                                                covariate_value = as.numeric(m[, 3]))
        }
        next
      }
      if (!any(sel)) next
      g <- paste(at[sel], bin[sel])
      vals <- switch(a,
        binary = tapply(rep(1, sum(sel)), g, function(x) 1),
        count = tapply(rep(1, sum(sel)), g, sum),
        mean = tapply(rows$v[sel], g, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)),
        last = tapply(rows$v[sel], g, function(x) x[[length(x)]]))
      parts <- do.call(rbind, strsplit(names(vals), " ", fixed = TRUE))
      cell <- data.frame(ep = as.integer(parts[, 1]), time_id = as.integer(parts[, 2]),
                         covariate_id = sub$covariate_id[[k]], covariate_value = as.numeric(vals))
      out[[length(out) + 1L]] <- cell[!is.na(cell$covariate_value) & cell$covariate_value != 0, , drop = FALSE]
    }
  }
  cov <- if (length(out)) do.call(rbind, out) else
    data.frame(ep = integer(0), time_id = integer(0), covariate_id = integer(0), covariate_value = numeric(0))
  cov <- cov[order(cov$ep, cov$time_id, cov$covariate_id), , drop = FALSE]
  .outputRowCap(nrow(cov), name)
  frame <- data.frame(row_id = keys[cov$ep], cohort_row_id = keys[cov$ep], person_id = roster$person_id[cov$ep],
                      time_id = as.integer(cov$time_id), covariate_id = as.integer(cov$covariate_id),
                      covariate_value = as.numeric(cov$covariate_value), stringsAsFactors = FALSE)
  columns <- c(.outputIdentityColumns(cons),
               list(time_id = .fragmentColumn("integer", FALSE, "measure"),
                    covariate_id = .fragmentColumn("integer", FALSE, "measure"),
                    covariate_value = .fragmentColumn("numeric", TRUE, "measure")))
  status <- if (unavailable) "unavailable" else "ok"
  parts <- list(
    .outputComponent(inputs, name, name, frame, columns, cons, grain = "episode", status = status),
    .outputComponent(inputs, name, "covariateRef", .outputCovariateRef(ctx, reg),
                     .outputCovariateRefColumns(), NULL, grain = "none"),
    .outputComponent(inputs, name, "timeRef", time_ref,
                     list(time_id = .fragmentColumn("integer", FALSE, "measure"),
                          start_day = .fragmentColumn("integer", FALSE, "measure"),
                          end_day = .fragmentColumn("integer", FALSE, "measure")), NULL, grain = "none"))
  .outputCompiled(name, parts, status = status)
}

# --- sparse --------------------------------------------------------------------------------------------

#' Compile a sparse output (section 6.12; defect B-22)
#'
#' Per episode (or person) and registry covariate -- \code{(source, concept
#' identity, analysis)} with analyses \code{binary} / \code{count} over all of
#' the person's rows of the source -- one row for each non-zero value, with
#' the \code{covariateRef} reference companion. Persons without a covariate
#' contribute no rows (the component's own support is what publication
#' counts).
#' @keywords internal
.compileSparseOutput <- function(ctx, node, name, inputs) {
  opts <- node[["options"]] %||% list()
  field <- paste0("/outputs/", name, "/options")
  pop <- inputs$population
  grain <- node[["grain"]] %||% "episode"
  cons <- .reduceConsumer(ctx, name, pop$roster, pop$coverage, grain)
  roster <- .reduceRosterFrame(ctx, pop$roster)
  keys <- roster$cohort_row_id
  reg <- .outputCovariateRegistry(ctx, node, name, function(src, kind, has_value) {
    an <- as.character(unlist(opts[["analyses"]] %||% list("binary"), use.names = FALSE))
    intersect(an, c("binary", "count"))
  })
  srcs <- .outputSources(node)
  out <- list()
  unavailable <- FALSE
  for (i in seq_along(srcs)) {
    path <- paste0(field, "/sources/", i - 1L)
    idn <- .outputSourceIdentities(ctx, srcs[[i]], path)
    rows <- .outputSourceRows(ctx, cons, idn$source, NULL,
                              .fragmentName("ox", name, paste0("s_", i), ctx = ctx), path)
    if (is.null(rows)) { unavailable <- TRUE; next }
    sub <- reg[reg$source_index == i, , drop = FALSE]
    for (k in seq_len(nrow(sub))) {
      cid <- sub$concept_id[[k]]
      sel <- if (is.na(cid)) rep(TRUE, nrow(rows)) else rows$concept_id %in% cid
      if (!any(sel)) next
      n <- table(as.character(rows$row_key[sel]))
      ep <- match(names(n), as.character(keys))
      out[[length(out) + 1L]] <- data.frame(ep = ep, covariate_id = sub$covariate_id[[k]],
        covariate_value = if (identical(sub$analysis[[k]], "binary")) 1 else as.numeric(n))
    }
  }
  cov <- if (length(out)) do.call(rbind, out) else
    data.frame(ep = integer(0), covariate_id = integer(0), covariate_value = numeric(0))
  if (identical(grain, "person") && nrow(cov)) {
    persons <- as.character(roster$person_id)
    cov$person <- persons[cov$ep]
    first_ep <- match(unique(persons), persons)
    agg <- stats::aggregate(covariate_value ~ person + covariate_id, data = cov, FUN = max)
    cov <- data.frame(ep = first_ep[match(agg$person, unique(persons))], covariate_id = agg$covariate_id,
                      covariate_value = agg$covariate_value)
  }
  cov <- cov[order(cov$ep, cov$covariate_id), , drop = FALSE]
  .outputRowCap(nrow(cov), name)
  if (identical(grain, "person")) {
    frame <- data.frame(person_id = roster$person_id[cov$ep], covariate_id = as.integer(cov$covariate_id),
                        covariate_value = as.numeric(cov$covariate_value), stringsAsFactors = FALSE)
    columns <- list(person_id = .fragmentColumn(cons$spec$person_type, FALSE, "person_key"),
                    covariate_id = .fragmentColumn("integer", FALSE, "measure"),
                    covariate_value = .fragmentColumn("numeric", TRUE, "measure"))
  } else {
    frame <- data.frame(row_id = keys[cov$ep], cohort_row_id = keys[cov$ep], person_id = roster$person_id[cov$ep],
                        covariate_id = as.integer(cov$covariate_id),
                        covariate_value = as.numeric(cov$covariate_value), stringsAsFactors = FALSE)
    columns <- c(.outputIdentityColumns(cons),
                 list(covariate_id = .fragmentColumn("integer", FALSE, "measure"),
                      covariate_value = .fragmentColumn("numeric", TRUE, "measure")))
  }
  status <- if (unavailable) "unavailable" else "ok"
  parts <- list(
    .outputComponent(inputs, name, name, frame, columns, cons, grain = grain, status = status),
    .outputComponent(inputs, name, "covariateRef", .outputCovariateRef(ctx, reg),
                     .outputCovariateRefColumns(), NULL, grain = "none"))
  .outputCompiled(name, parts, status = status)
}

# --- long ------------------------------------------------------------------------------------------------

# Released event columns of a long output beyond the identity: the concept,
# the dates and the source's other filterable (non-identifier, non-class-1)
# columns under their own names.
.LONG_CORE <- c("person_id", "event_id", "source_ns", "start_date", "end_date", "end_recorded",
                "supply", "concept_id", "value_source", "unit_source", "operator_concept_id",
                "range_low_source", "range_high_source", "visit_id", "value", "unit_concept_id",
                "range_low", "range_high", "unit_unknown")

.longOtherColumns <- function(src) {
  f <- src$fragment
  cols <- names(f$columns)
  setdiff(cols[!cols %in% f$private_columns], c(.LONG_CORE, .ADAPTER_PARENT_COLUMNS))
}

#' Compile a long output (section 6.12)
#'
#' One row per (episode, event) -- \code{unit = "event"}: per (person, event),
#' each physical event once -- ordered by \code{(person_id, cohort_row_id,
#' start_date, source_ns, event_id)}: the event's concept and dates (under the
#' output's date handling) and its other filterable columns, the requested
#' derived projections (\code{days_from_index}, \code{days_from_<anchor>},
#' \code{event_number}, \code{days_since_previous}, \code{duration_days},
#' \code{value}, \code{unit_concept_id}, \code{visit_type},
#' \code{concept_name}), \code{relations} (\code{days_to_<name>}: days to the
#' first related row inside \code{[start + within[1], start + within[2]]},
#' self excluded on the same source, \code{distinct_by} concept or visit) and
#' \code{broadcast} reducer columns; or, with \code{event_log = TRUE}, the
#' common-schema log across sources (\code{source}, \code{concept_id},
#' \code{start_days}, \code{end_days}, \code{value}, \code{unit_concept_id},
#' \code{event_number}). Several sources without \code{event_log} are the v1
#' split adapter and are refused here.
#' @keywords internal
.compileLongOutput <- function(ctx, node, name, inputs) {
  opts <- node[["options"]] %||% list()
  field <- paste0("/outputs/", name, "/options")
  ir <- ctx[["ir"]]
  srcs <- .outputSources(node)
  if (!length(srcs)) .irReject("a long output needs sources", field = paste0("/outputs/", name, "/sources"))
  event_log <- isTRUE(unlist(opts[["event_log"]]))
  if (length(srcs) > 1L && !event_log) {
    .irReject("a long output over several sources needs event_log = TRUE (the per-table split is a v1 shape)",
              field = paste0(field, "/event_log"))
  }
  per_event <- identical(opts[["unit"]] %||% "episode_event", "event")
  grain <- if (per_event) "person" else node[["grain"]] %||% "episode"
  pop <- inputs$population
  cons <- .reduceConsumer(ctx, name, pop$roster, pop$coverage, if (per_event) "person" else grain)
  roster <- .reduceRosterFrame(ctx, pop$roster)
  keys <- roster$cohort_row_id
  index <- as.Date(roster$cohort_start_date)
  derived <- as.character(unlist(opts[["columns_derived"]] %||% list(), use.names = FALSE))
  h <- .compileDialect(ctx)
  all_rows <- list()
  others <- character(0)
  unavailable <- FALSE
  for (i in seq_along(srcs)) {
    path <- paste0("/outputs/", name, "/sources/", i - 1L)
    src <- .reduceSource(ctx, srcs[[i]])
    if (!src$available) { unavailable <- TRUE; next }
    cols <- names(src$fragment$columns)
    extra <- list()
    if ("value" %in% cols) extra$v <- list(expr = "e.value", type = "numeric")
    if ("unit_concept_id" %in% cols) extra$unit_concept_id <- list(expr = "e.unit_concept_id", type = "integer")
    if ("concept_id" %in% cols) extra$concept_id <- list(expr = "e.concept_id", type = "integer")
    if ("visit_id" %in% cols) extra$visit_id <- list(expr = "e.visit_id", type = "bigint", role = "private")
    if (!event_log) {
      others <- .longOtherColumns(src)
      for (o in others) {
        extra[[o]] <- list(expr = paste0("e.", o), type = src$fragment$columns[[o]]$type)
      }
    }
    if ("visit_type" %in% derived && "visit_id" %in% cols) {
      vo <- .roleOf(ctx[["handle"]], "visit_occurrence")
      if (!is.null(vo)) {
        vq <- .reduceTable(ctx, "visit_occurrence")
        extra$visit_type <- list(type = "integer", expr = paste0(
          "(SELECT MIN(vo.", vo[["domain_concept"]], ") FROM ", vq, " vo WHERE ",
          .sqlLosslessKey(h, paste0("vo.", vo[["event_key"]])), " = e.visit_id)"))
      }
    }
    st <- .reduceStream(ctx, cons, src, NULL, .fragmentName("ox", name, paste0("s_", i), ctx = ctx),
                        extra = extra)
    rows <- .reduceFetch(ctx, st, order = c("row_key", "start_date"), field = path)
    # The physical ordering remains valid when unit = event unions rows
    # owned by different episodes; the roster key is ordered separately.
    rows$.source_order <- order(.outputOrderRows(rows, setdiff(st$ordering, "row_key")))
    rows$source <- rep(src$name, nrow(rows))
    if (!"v" %in% names(rows)) rows$v <- rep(NA_real_, nrow(rows))
    if (!"unit_concept_id" %in% names(rows)) rows$unit_concept_id <- rep(NA_integer_, nrow(rows))
    if (!"concept_id" %in% names(rows)) rows$concept_id <- rep(NA_integer_, nrow(rows))
    if (!"visit_id" %in% names(rows)) rows$visit_id <- rep(NA_real_, nrow(rows))
    if (!"visit_type" %in% names(rows)) rows$visit_type <- rep(NA_integer_, nrow(rows))
    if (!"member_table" %in% names(rows)) rows$member_table <- rep("", nrow(rows))
    all_rows[[length(all_rows) + 1L]] <- rows
  }
  keep_cols <- unique(c("row_key", "person_id", "start_date", "end_date", "event_id", "source_ns", "source",
                 "v", "unit_concept_id", "concept_id", "visit_id", "visit_type", "member_table", ".source_order", others))
  .outputRowCap(sum(vapply(all_rows, nrow, integer(1))), name)
  rows <- if (length(all_rows)) do.call(rbind, lapply(all_rows, function(r) r[keep_cols])) else NULL
  if (is.null(rows)) {
    rows <- data.frame(row_key = keys[0], person_id = roster$person_id[0], start_date = as.Date(character(0)),
                       end_date = as.Date(character(0)), event_id = numeric(0), source_ns = character(0),
                       source = character(0), v = numeric(0), unit_concept_id = integer(0),
                       concept_id = integer(0), visit_id = numeric(0), visit_type = integer(0),
                       member_table = character(0), .source_order = integer(0), stringsAsFactors = FALSE)
    for (o in others) rows[[o]] <- vector(mode = "logical", length = 0L)
  }
  if (per_event) {
    # Union the scoped events of every visit/parent; attribute each physical
    # event to its first associated episode for index-relative projections.
    rows <- rows[.reduceOrderRows(rows, c("person_id", "row_key", ".source_order")), , drop = FALSE]
    rows <- rows[!duplicated(paste(rows$person_id, rows$source_ns, rows$member_table, rows$event_id)), , drop = FALSE]
  }
  o <- .reduceOrderRows(rows, c("person_id", if (!per_event) "row_key", "start_date", "source_ns", ".source_order"))
  rows <- rows[o, , drop = FALSE]
  if (per_event) {
    rows$.owner_row_key <- rows$row_key
    rows$row_key <- rows$person_id
  }
  rows <- .reduceTemporal(rows, opts[["temporal"]], c("start_date", "source_ns", ".source_order"))
  if (per_event) {
    rows$row_key <- rows$.owner_row_key
    rows$.owner_row_key <- NULL
  }
  .outputRowCap(nrow(rows), name)
  at <- match(as.character(rows$row_key), as.character(keys))
  idx <- index[at]
  grp <- as.character(if (per_event) rows$person_id else rows$row_key)
  event_number <- as.integer(stats::ave(seq_len(nrow(rows)), grp, FUN = seq_along))
  int <- function() .fragmentColumn("integer", TRUE, "measure")
  ident_cols <- if (per_event) {
    list(person_id = .fragmentColumn(cons$spec$person_type, FALSE, "person_key"))
  } else .outputIdentityColumns(cons)
  ident <- if (per_event) data.frame(person_id = rows$person_id, stringsAsFactors = FALSE) else
    data.frame(row_id = rows$row_key, cohort_row_id = rows$row_key, person_id = rows$person_id,
               stringsAsFactors = FALSE)
  if (event_log) {
    if (per_event) {
      .irReject("the event log reports days from the index; it needs unit = \"episode_event\"",
                field = paste0(field, "/unit"))
    }
    frame <- cbind(ident, data.frame(source = rows$source, concept_id = as.integer(rows$concept_id),
      start_days = as.integer(rows$start_date - idx), end_days = as.integer(rows$end_date - idx),
      value = as.numeric(rows$v), unit_concept_id = as.integer(rows$unit_concept_id),
      event_number = event_number, stringsAsFactors = FALSE))
    columns <- c(ident_cols, list(source = .fragmentColumn("text", FALSE, "measure"),
                                  concept_id = .fragmentColumn("integer", TRUE, "concept"),
                                  start_days = int(), end_days = int(),
                                  value = .fragmentColumn("numeric", TRUE, "value"),
                                  unit_concept_id = .fragmentColumn("integer", TRUE, "unit"),
                                  event_number = int()))
  } else {
    frame <- cbind(ident, data.frame(concept_id = as.integer(rows$concept_id), start_date = rows$start_date,
                                     end_date = rows$end_date, stringsAsFactors = FALSE))
    columns <- c(ident_cols, list(concept_id = .fragmentColumn("integer", TRUE, "concept"),
                                  start_date = .fragmentColumn("date", TRUE, "start"),
                                  end_date = .fragmentColumn("date", TRUE, "end")))
    for (oc in others) {
      frame[[oc]] <- rows[[oc]]
      type <- all_rows[[1L]][[oc]]
      columns[[oc]] <- .fragmentColumn(
        if (inherits(type, "Date")) "date" else if (is.character(type)) "text" else
          if (is.integer(type)) "integer" else "numeric", TRUE, "measure")
    }
    if ("days_from_index" %in% derived) { frame$days_from_index <- as.integer(rows$start_date - idx); columns$days_from_index <- int() }
    if ("days_from_anchor" %in% derived) {
      for (an in .outputSortedNames(opts[["anchors"]])) {
        d <- .reduceAnchorDates(ctx, cons, opts[["anchors"]][[an]], paste0(field, "/anchors/", an))
        col <- paste0("days_from_", an)
        frame[[col]] <- as.integer(rows$start_date - d[at])
        columns[[col]] <- int()
      }
    }
    if ("event_number" %in% derived) { frame$event_number <- event_number; columns$event_number <- int() }
    if ("days_since_previous" %in% derived) {
      prev <- stats::ave(as.numeric(rows$start_date), grp, FUN = function(x) c(NA, x[-length(x)]))
      frame$days_since_previous <- as.integer(as.numeric(rows$start_date) - prev)
      columns$days_since_previous <- int()
    }
    if ("duration_days" %in% derived) {
      frame$duration_days <- as.integer(rows$end_date - rows$start_date)
      columns$duration_days <- int()
    }
    if ("value" %in% derived) { frame$value <- as.numeric(rows$v); columns$value <- .fragmentColumn("numeric", TRUE, "value") }
    if ("unit" %in% derived) {
      frame$unit_concept_id <- as.integer(rows$unit_concept_id)
      columns$unit_concept_id <- .fragmentColumn("integer", TRUE, "unit")
    }
    if ("visit_type" %in% derived) {
      frame$visit_type <- as.integer(rows$visit_type)
      columns$visit_type <- .fragmentColumn("integer", TRUE, "concept")
    }
    if ("concept_name" %in% derived) {
      frame$concept_name <- .reduceConceptNames(ctx, as.integer(rows$concept_id))
      columns$concept_name <- .fragmentColumn("text", TRUE, "measure")
    }
    relations <- opts[["relations"]] %||% list()
    for (ri in seq_along(.outputSortedNames(relations))) {
      rn <- .outputSortedNames(relations)[[ri]]
      rel <- relations[[rn]]
      rsrc <- .reduceSource(ctx, rel[["source"]])
      col <- paste0("days_to_", rn)
      columns[[col]] <- int()
      if (!rsrc$available) { frame[[col]] <- rep(NA_integer_, nrow(frame)); unavailable <- TRUE; next }
      rextra <- list()
      if ("concept_id" %in% names(rsrc$fragment$columns)) rextra$concept_id <- list(expr = "e.concept_id", type = "integer")
      if ("visit_id" %in% names(rsrc$fragment$columns)) rextra$visit_id <- list(expr = "e.visit_id", type = "bigint", role = "private")
      rst <- .reduceStream(ctx, cons, rsrc, NULL, .fragmentName("ox", name, paste0("r_", ri), ctx = ctx),
                           extra = rextra)
      rrows <- .reduceFetch(ctx, rst, order = c("row_key", "start_date"), field = paste0(field, "/relations/", rn))
      if (!"concept_id" %in% names(rrows)) rrows$concept_id <- rep(NA_integer_, nrow(rrows))
      if (!"visit_id" %in% names(rrows)) rrows$visit_id <- rep(NA_character_, nrow(rrows))
      frame[[col]] <- .longRelation(rows, rrows, rel, same_source = rows$source == rsrc$name)
    }
  }
  # broadcast reducer columns joined on the roster key
  vars <- .irOutputVariables(ir, name)
  if (length(vars)) {
    bcons <- .reduceConsumer(ctx, name, pop$roster, pop$coverage, "episode")
    for (v in vars) {
      vnode <- ir[["variables"]][[v]]
      if (!is.null(vnode[["reducer"]]) && !isTRUE(unlist(vnode[["broadcast"]]))) {
        .irReject(paste0("variable '", v, "' is a reducer; a long output takes it only with broadcast = TRUE"),
                  field = paste0("/variables/", v, "/broadcast"))
      }
      rec <- .compileVariable(ctx, .irNamedNode(ir, "variable", v), pop$roster, bcons)
      vals <- rec$evaluate()
      vat <- match(as.character(rows$row_key), as.character(vals$row_key))
      for (cn in names(rec$columns)) {
        if (cn %in% names(columns)) {
          .irReject(paste0("broadcast variable column '", cn, "' duplicates an output column"),
                    field = paste0("/outputs/", name, "/columns"))
        }
        frame[[cn]] <- vals[[cn]][vat]
        columns[[cn]] <- rec$columns[[cn]]
      }
      if (!isTRUE(rec$available)) unavailable <- TRUE
    }
  }
  # Broadcast raw variables may themselves be dates; apply the policy only
  # after every released column has been assembled.
  handled <- .outputDateHandling(frame, columns, idx, opts[["date_handling"]] %||% list(mode = "relative"), name)
  frame <- handled$frame
  columns <- handled$columns
  .outputCheckNames(setdiff(names(columns), c("row_id", "cohort_row_id", "person_id")), name, grain)
  status <- if (unavailable) "unavailable" else "ok"
  part <- .outputComponent(inputs, name, name, frame, columns, cons,
                           grain = if (per_event) "person" else "episode", status = status)
  .outputCompiled(name, list(part), status = status)
}

# days_to_<relation>: days from each row's start to the first related row in
# [start + within[1], start + within[2]] of the same roster row; `same_source`
# marks the rows whose own source is the relation's (self excluded).
.longRelation <- function(rows, rrows, rel, same_source) {
  within <- as.integer(unlist(rel[["within"]]))
  distinct_by <- rel[["distinct_by"]]
  exclude_self <- !isFALSE(unlist(rel[["exclude_self"]]))
  out <- rep(NA_integer_, nrow(rows))
  if (!nrow(rows) || !nrow(rrows)) return(out)
  rg <- split(seq_len(nrow(rrows)), as.character(rrows$row_key))
  for (i in seq_len(nrow(rows))) {
    cand <- rg[[as.character(rows$row_key[[i]])]]
    if (is.null(cand)) next
    lo <- rows$start_date[[i]] + within[[1L]]
    hi <- rows$start_date[[i]] + within[[2L]]
    ok <- rrows$start_date[cand] >= lo & rrows$start_date[cand] <= hi
    if (same_source[[i]] && exclude_self) {
      same <- as.character(rrows$event_id[cand]) == as.character(rows$event_id[[i]])
      for (col in intersect(c("source_ns", "member_table"), intersect(names(rows), names(rrows)))) {
        same <- same & as.character(rrows[[col]][cand]) == as.character(rows[[col]][[i]])
      }
      ok <- ok & !same
    }
    if (identical(distinct_by, "concept")) ok <- ok & !is.na(rows$concept_id[[i]]) & !is.na(rrows$concept_id[cand]) &
      !(rrows$concept_id[cand] %in% rows$concept_id[[i]])
    if (identical(distinct_by, "visit")) ok <- ok & !is.na(rows$visit_id[[i]]) & !is.na(rrows$visit_id[cand]) &
      !(as.character(rrows$visit_id[cand]) %in% as.character(rows$visit_id[[i]]))
    ok[is.na(ok)] <- FALSE
    if (any(ok)) out[[i]] <- as.integer(min(rrows$start_date[cand][ok]) - rows$start_date[[i]])
  }
  out
}

# --- intervals --------------------------------------------------------------------------------------------

#' Compile an intervals output (section 6.12; defect A-16)
#'
#' Closed intervals of every source (point tables have end = start through
#' the role registry) matched to each episode's window by
#' \code{interval_match} (overlaps, starts_in, ends_in, or active_at the
#' \code{anchor}), selected per episode and source (and concept) by
#' \code{event_select} (all, first, last, nearest to the anchor) and
#' \code{select_n}; days relative to the index.
#' @keywords internal
.compileIntervalsOutput <- function(ctx, node, name, inputs) {
  opts <- node[["options"]] %||% list()
  field <- paste0("/outputs/", name, "/options")
  if (!is.null(opts[["legacy_anchor_offset"]])) {
    .irReject(paste0("output '", name, "' carries the v1 compatibility variant legacy_anchor_offset, ",
                     "which runs on the v1 executors"), field = paste0(field, "/legacy_anchor_offset"))
  }
  pop <- inputs$population
  cons <- .reduceConsumer(ctx, name, pop$roster, pop$coverage, "episode")
  roster <- .reduceRosterFrame(ctx, pop$roster)
  keys <- roster$cohort_row_id
  index <- as.Date(roster$cohort_start_date)
  match_mode <- opts[["interval_match"]] %||% "overlaps"
  select <- opts[["event_select"]] %||% "all"
  n_sel <- as.integer(unlist(opts[["select_n"]] %||% 1L))
  by_concept <- identical(opts[["select_by"]] %||% "episode_source", "episode_source_concept")
  window <- opts[["window"]]
  bounds <- .reduceBoundsR(ctx, cons, window, field)
  anchor <- .reduceAnchorDates(ctx, cons, opts[["anchor"]] %||% "index", paste0(field, "/anchor"))
  srcs <- .outputSources(node)
  parts <- list()
  unavailable <- FALSE
  for (i in seq_along(srcs)) {
    src <- .reduceSource(ctx, srcs[[i]])
    if (!src$available) { unavailable <- TRUE; next }
    extra <- if ("concept_id" %in% names(src$fragment$columns)) {
      list(concept_id = list(expr = "e.concept_id", type = "integer"))
    } else list()
    st <- .reduceStream(ctx, cons, src, if (identical(match_mode, "active_at")) NULL else window,
                        .fragmentName("ox", name, paste0("s_", i), ctx = ctx), extra = extra,
                        overlap = match_mode %in% c("overlaps", "ends_in"))
    rows <- .reduceFetch(ctx, st, order = c("row_key", "start_date"), field = paste0(field, "/sources/", i - 1L))
    rows$.source_order <- order(.outputOrderRows(rows, st$ordering))
    if (!"concept_id" %in% names(rows)) rows$concept_id <- rep(NA_integer_, nrow(rows))
    if (!nrow(rows)) next
    rows$end_date[is.na(rows$end_date)] <- rows$start_date[is.na(rows$end_date)]
    rows <- rows[rows$end_date >= rows$start_date, , drop = FALSE]
    j <- match(as.character(rows$row_key), as.character(keys))
    lo <- bounds$lo[j]; hi <- bounds$hi[j]; def <- bounds$defined[j]
    a <- anchor[j]
    keep <- switch(match_mode,
      overlaps = def & (is.na(hi) | rows$start_date <= hi) & (is.na(lo) | rows$end_date >= lo),
      starts_in = def & (is.na(lo) | rows$start_date >= lo) & (is.na(hi) | rows$start_date <= hi),
      ends_in = def & (is.na(lo) | rows$end_date >= lo) & (is.na(hi) | rows$end_date <= hi),
      active_at = !is.na(a) & rows$start_date <= a & rows$end_date >= a &
        def & (is.na(hi) | rows$start_date <= hi) & (is.na(lo) | rows$end_date >= lo))
    keep[is.na(keep)] <- FALSE
    rows <- rows[keep, , drop = FALSE]
    if (!nrow(rows)) next
    rows$source <- src$name
    rows$anchor <- anchor[match(as.character(rows$row_key), as.character(keys))]
    if (!identical(select, "all")) {
      g <- paste(rows$row_key, if (by_concept) rows$concept_id else "")
      o <- switch(select,
        first = order(g, rows$.source_order, method = "radix"),
        last = order(g, -rows$.source_order, method = "radix"),
        nearest = order(g, abs(as.numeric(rows$start_date - rows$anchor)), rows$start_date,
                        rows$.source_order, method = "radix"))
      rows <- rows[o, , drop = FALSE]
      rank <- stats::ave(seq_len(nrow(rows)), g[o], FUN = seq_along)
      rows <- rows[rank <= n_sel, , drop = FALSE]
    }
    parts[[length(parts) + 1L]] <- rows[c("row_key", "person_id", "source", "concept_id", "start_date",
                                          "end_date", "event_id", ".source_order")]
  }
  rows <- if (length(parts)) do.call(rbind, parts) else
    data.frame(row_key = keys[0], person_id = roster$person_id[0], source = character(0),
               concept_id = integer(0), start_date = as.Date(character(0)), end_date = as.Date(character(0)),
               event_id = numeric(0), .source_order = integer(0), stringsAsFactors = FALSE)
  at <- match(as.character(rows$row_key), as.character(keys))
  rows$start_days <- as.integer(rows$start_date - index[at])
  rows$end_days <- as.integer(rows$end_date - index[at])
  o <- order(.reduceKeyOrder(rows$row_key), rows$source, rows$.source_order, method = "radix")
  rows <- rows[o, , drop = FALSE]
  .outputRowCap(nrow(rows), name)
  frame <- data.frame(row_id = rows$row_key, cohort_row_id = rows$row_key, person_id = rows$person_id,
                      interval_type = rows$source, concept_id = as.integer(rows$concept_id),
                      start_days_from_index = rows$start_days, end_days_from_index = rows$end_days,
                      stringsAsFactors = FALSE)
  columns <- c(.outputIdentityColumns(cons),
               list(interval_type = .fragmentColumn("text", FALSE, "measure"),
                    concept_id = .fragmentColumn("integer", TRUE, "concept"),
                    start_days_from_index = .fragmentColumn("integer", TRUE, "measure"),
                    end_days_from_index = .fragmentColumn("integer", TRUE, "measure")))
  status <- if (unavailable) "unavailable" else "ok"
  part <- .outputComponent(inputs, name, name, frame, columns, cons, grain = "episode", status = status)
  .outputCompiled(name, list(part), status = status)
}

# --- execution -------------------------------------------------------------------------------------------

#' Execute a validated language 2.0 plan in its private context
#'
#' Compiles the publication roots (\code{.planCompileOutputs()}) and returns
#' them for the publication boundary (\code{.publishOutputs()}), or, for an
#' internal caller without a context, publishes them itself and returns the
#' published values. Every component is materialised before this returns,
#' so the temporary objects of the compilation are released on every exit
#' path (A-20 (k)): the operation's own tables only, never objects owned
#' before it.
#'
#' @param ctx The execution context (\code{.planContext()}).
#' @param ir The validated IR.
#' @param out_symbols Named list output -> requested symbol.
#' @param own_context Logical: the caller brought no context.
#' @return Named list root -> \code{CompiledOutput} (or published values).
#' @keywords internal
.planExecuteNative <- function(ctx, ir, out_symbols, own_context = FALSE) {
  handle <- ctx$handle
  before <- unique(handle$temp_tables %||% character(0))
  cctx <- NULL
  compiled <- FALSE
  # Install cleanup before context creation as well as compilation. On a
  # compilation failure close both context owners; after success the caller
  # still needs the roster registry for publication. Attempt operation-table
  # cleanup even when a compiler DROP fails, and report every failure.
  on.exit({
    failures <- character(0)
    attempt <- function(expr) tryCatch(expr, error = function(e) {
      failures <<- c(failures, conditionMessage(e))
    })
    if (!compiled) attempt(.planContextClose(ctx)) else if (is.environment(cctx)) {
      attempt(.compileReleaseTempTables(cctx))
    }
    attempt(.dropTempTablesCreatedSince(handle, before))
    if (length(failures)) stop(paste(unique(failures), collapse = "\n"), call. = FALSE)
  }, add = TRUE)
  cctx <- .compileContext(ctx, ir)
  outputs <- .planCompileOutputs(cctx, out_symbols)
  compiled <- TRUE
  if (!own_context) return(outputs)
  status <- .publishOutputs(ctx, outputs, out_symbols)
  if (exists(ctx$id, .publicationRegistry, inherits = FALSE)) rm(list = ctx$id, envir = .publicationRegistry)
  if (identical(status$status, "refused")) .publishReject(status$rejected[[1L]]$code)
  published <- list()
  for (root in names(status$published)) {
    comps <- status$published[[root]]
    values <- lapply(comps, function(entry) get(entry$symbol, ctx$target_env))
    published[[root]] <- if (length(values) == 1L && identical(names(values), root)) values[[1L]] else values
  }
  published
}
