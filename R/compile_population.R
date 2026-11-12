# Module: Populations (design-2.10.3 sections 2.5, 6.1, 6.9 and 6.13;
# amendments A-1, A-17, A-20; defects B-8 and A-E4)
#
# Every population compiles to one episode table, the final roster pop_<id>:
#
#   cohort_row_id, subject_id, cohort_start_date, cohort_end_date,
#   index_source, index_key, index_visit_id, set_id, is_case,
#   sampling_weight, op_start, op_end
#
# plus private typed columns: op_component_id (the selected coverage
# component, A-20 c), source_rank and key_1..key_3 (the typed components of
# the origin identity, below). Rosters are deterministic CTE chains over the
# CDM: a roster is recomputed by every statement that reads it, and the total
# orders below make every recomputation identical, so cohort_row_id is
# immutable for the plan execution. Only the matched rows a matching sampler
# realises in R are materialised (R/compile_match.R). No statement of this
# module reads a session temporary table more than once (MySQL refuses a
# temporary table referenced twice in one statement).
#
# Origin identity (section 6.9). Each roster row carries
#   (index_source, subject_id, cohort_start_date, cohort_end_date, index_key)
# with index_key a lossless text key: the decimal event key of a table event
# or cost, "<parent_source_ns>:<parent_event_id>:<member_table>:<event_id>" of
# an episode member, the era key "<group_id>:<era_no>" of an interval course,
# "<cohort_definition_id>:<subject_id>:<start>:<end>" of a results-cohort
# source (A-1), "<serialised case identity>|case|control" of a matched member
# (section 6.10); NULL for a person roster (criteria population without an
# index). The same identity is also carried as typed components:
#   source_rank   the rank of index_source among every index-source tag of the
#                 plan in byte order (a plan constant per tag), and
#   key_1..key_3  the integer components of index_key (event key; parent key,
#                 member-table rank, event key; group, era number; cohort id,
#                 start day, end day; set_id, role).
# Orders and identity comparisons use the typed components, never text
# collation, so they are identical on every dialect (a case-insensitive MySQL
# collation cannot merge two source names, and decimal keys order
# numerically, as the legacy .rankedCohortSql() orders its key).
#
# cohort_row_id is allocated once per roster by .rankedRosterSql(): ROW_NUMBER
# over (subject_id, cohort_start_date, cohort_end_date, source_rank, key_1,
# key_2, key_3), NULLs last -- chronological first, the complete typed origin
# identity as the deterministic tie-breaker.
#
# Index populations (section 6.1 sequence): source candidates (UNION ALL of
# the sources, any_of arms tagged with their declared order) -> offsets ->
# candidate coverage lookup (the population's own policy, compiled once on
# the candidates, A-20 c) and ends (end.strategy, capped at op_end when
# covered) -> candidate anchors and the complete criterion tree per candidate
# (three-valued, only TRUE survives) -> selection (first/last/nth select
# before eligibility, as the legacy executor does; first_eligible keeps the
# lowest-ranked TRUE candidate; all keeps every TRUE candidate) -> the final
# roster. End-before-start candidates are privately ineligible.
#
# Criteria populations: persons (the person table, in scope), optionally
# seeded from a results cohort (criteria evaluated per cohort episode and
# combined by episode_policy), or -- with a parent -- the parent's final
# roster as candidates (cand_key := cohort_row_id, A-20 c), whose episodes a
# child inherits (B-8). Set operations work on full origin identity: union
# deduplicates identical identities only and refuses contradictory metadata
# (setop_identity_conflict), intersect/setdiff keep the left member's episodes
# by person (A-E4). A parent narrows a set operation's persons after the
# operation and an index population's persons from the start.

.POP_KEY_COLUMNS <- c("source_rank", "key_1", "key_2", "key_3")
.POP_ROSTER_COLUMNS <- c("subject_id", "cohort_start_date", "cohort_end_date",
                         "index_source", "index_key", "index_visit_id",
                         "set_id", "is_case", "sampling_weight", "op_start",
                         "op_end", "op_component_id", .POP_KEY_COLUMNS)
.POP_PRIVATE_COLUMNS <- c("index_key", "index_visit_id", "op_component_id",
                          .POP_KEY_COLUMNS)
.POP_EPISODE_POLICIES <- c("any_episode", "all_episodes", "first_episode",
                           "last_episode")

# --- typed column contracts ---------------------------------------------------

#' Column contract of a population roster (design section 6.9)
#'
#' @param indexed Logical; whether the roster's rows are indexed episodes.
#' @return Named list of \code{.fragmentColumn()} records, without
#'   \code{cohort_row_id}.
#' @keywords internal
.populationColumnSpec <- function(indexed = TRUE) {
  col <- .fragmentColumn
  list(
    subject_id = col("bigint", FALSE, "person_key"),
    cohort_start_date = col("date", !isTRUE(indexed), "start"),
    cohort_end_date = col("date", TRUE, "end"),
    index_source = col("text", TRUE, "origin_identity"),
    index_key = col("text", TRUE, "origin_identity"),
    index_visit_id = col("bigint", TRUE, "visit"),
    set_id = col("bigint", TRUE, "analytic_key"),
    is_case = col("integer", TRUE, "measure"),
    sampling_weight = col("numeric", TRUE, "measure"),
    op_start = col("date", TRUE, "start"),
    op_end = col("date", TRUE, "end"),
    op_component_id = col("bigint", TRUE, "private"),
    source_rank = col("integer", TRUE, "origin_identity"),
    key_1 = col("bigint", TRUE, "origin_identity"),
    key_2 = col("bigint", TRUE, "origin_identity"),
    key_3 = col("bigint", TRUE, "origin_identity"))
}

# The roster fragment of population `pop` over a SELECT body producing
# cohort_row_id plus the roster columns.
.populationRosterFragment <- function(ctx, pop, body, dependencies, indexed) {
  cols <- c(list(cohort_row_id = .fragmentColumn("bigint", FALSE, "episode_key")),
            .populationColumnSpec(indexed))
  f <- .fragment(.fragmentName("pop", pop, ctx = ctx), "roster", "episode", body,
                 columns = cols, dependencies = unique(dependencies),
                 unique_key = "cohort_row_id",
                 person_lineage = list(person_col = "subject_id", via_roster = NULL),
                 roster_namespace = pop,
                 source_identity_columns = c("index_source", "subject_id",
                                             "cohort_start_date", "cohort_end_date",
                                             "index_key"),
                 private_columns = .POP_PRIVATE_COLUMNS,
                 ordering = c("subject_id", "cohort_start_date", "cohort_end_date",
                              .POP_KEY_COLUMNS))
  .sqlAssertTypedSql(.compileDialect(ctx), f$select_body)
  .fragmentRegister(ctx, f)
}

# A candidates fragment of population `pop` (attr(f, "population") names it;
# A-20 a): the roster columns keyed by the private cand_key, plus extras.
.populationCandidateFragment <- function(ctx, pop, suffix, body, dependencies,
                                         indexed = TRUE, extra = list()) {
  cols <- c(list(cand_key = .fragmentColumn("bigint", FALSE, "episode_key")),
            .populationColumnSpec(indexed), extra)
  f <- .fragment(.fragmentName("cand", pop, suffix, ctx = ctx), "candidates", "episode",
                 body, columns = cols, dependencies = unique(dependencies),
                 unique_key = "cand_key",
                 person_lineage = list(person_col = "subject_id", via_roster = NULL),
                 source_identity_columns = c("index_source", "subject_id",
                                             "cohort_start_date", "cohort_end_date",
                                             "index_key"),
                 private_columns = unique(c("cand_key", .POP_PRIVATE_COLUMNS, names(extra))))
  attr(f, "population") <- pop
  .sqlAssertTypedSql(.compileDialect(ctx), f$select_body)
  .fragmentRegister(ctx, f)
}

# SELECT-list items "x.<col> AS <col>" for the roster columns, with optional
# per-column overrides (named expressions).
.populationSelect <- function(alias, override = list(), columns = .POP_ROSTER_COLUMNS) {
  vapply(columns, function(col) {
    expr <- override[[col]] %||% paste0(alias, ".", col)
    paste0(expr, " AS ", col)
  }, "", USE.NAMES = FALSE)
}

# Typed NULL overrides for the roster columns a source does not carry.
.populationNulls <- function(h, columns) {
  spec <- .populationColumnSpec(TRUE)
  out <- lapply(columns, function(col) .compileTypedNull(h, spec[[col]]$type))
  names(out) <- columns
  out
}

# ORDER BY items, NULLs last, for a deterministic typed order.
.populationOrder <- function(h, alias, columns) {
  vapply(columns, function(col) .sqlNullsLast(h, paste0(alias, ".", col)), "",
         USE.NAMES = FALSE)
}

# An always-UNKNOWN boolean (NULL compared with a literal).
.populationUnknownSql <- function(h) {
  paste0("(1 = ", .compileTypedNull(h, "integer"), ")")
}

#' Allocate cohort_row_id over a roster relation (design section 6.9)
#'
#' Replaces the legacy \code{.rankedCohortSql()} for compiled rosters (the
#' legacy executors keep theirs): \code{ROW_NUMBER()} over the chronological
#' total order \code{(subject_id, cohort_start_date, cohort_end_date)}, then
#' the complete typed origin identity \code{(source_rank, key_1, key_2,
#' key_3)} as deterministic tie-breakers, NULLs last. Two sources on
#' 2020-01-01 (\code{z_earlier}) and 2020-12-01 (\code{a_later}) give January
#' the lower row number; a legacy roster without typed keys keeps the legacy
#' tie order \code{(subject_id, start, end)}.
#'
#' @param h Handle, context, profile or dialect name.
#' @param from Name of the relation (fragment) carrying the roster columns.
#' @return One SELECT body: \code{cohort_row_id} plus the roster columns.
#' @keywords internal
.rankedRosterSql <- function(h, from) {
  if (!.isScalarString(from) || !grepl(.NAME_PATTERN, from)) {
    stop("The ranked roster reads one relation by name.", call. = FALSE)
  }
  order <- c("x.subject_id", .populationOrder(h, "x", c("cohort_start_date",
                                                        "cohort_end_date",
                                                        .POP_KEY_COLUMNS)))
  rn <- .sqlLosslessKey(h, .sqlRowNumber(h, character(0), order))
  paste0("SELECT ", rn, " AS cohort_row_id, ",
         paste(.populationSelect("x"), collapse = ", "), " FROM ", from, " x")
}

# --- identities (amendment A-17) ---------------------------------------------

#' Identities of the population fragments of a plan (amendment A-17)
#'
#' Per index or criteria population: the candidate stages \code{("cand",
#' <pop>, "src")} (source union), \code{("cand", <pop>, "raw")} (candidates
#' before coverage), \code{("cand", <pop>)} (candidates with coverage and
#' ends), \code{("cand", <pop>, "crit")} (criteria per candidate) and
#' \code{("pop", <pop>, "sel")} (the selected episodes); per set operation
#' \code{("pop", <pop>, "all")} (the members' rows); per match the matching
#' family (\code{.matchFragmentIdentities()}); per population the final
#' roster \code{("pop", <pop>)}; and \code{("scope", "persons")} when the plan
#' declares a scope. A matching template's instantiation uses its template
#' population's candidate identities (one instantiation per plan, A-20 c).
#' @keywords internal
.populationFragmentIdentities <- function(ir) {
  pops <- ir[["populations"]] %||% list()
  out <- list()
  add <- function(...) out[[length(out) + 1L]] <<- .fragmentIdentity(...)
  for (p in names(pops)) {
    kind <- pops[[p]][["kind"]] %||% ""
    if (kind %in% c("index", "criteria")) {
      add("cand", p, "src")
      add("cand", p, "raw")
      add("cand", p)
      add("cand", p, "crit")
      add("pop", p, "sel")
    }
    if (identical(kind, "setop")) add("pop", p, "all")
    add("pop", p)
  }
  if (.populationScopeDeclared(ir)) add("scope", "persons")
  out
}

# --- static population information --------------------------------------------

# Does the IR declare a plan scope (results cohort or workspace tables)?
.populationScopeDeclared <- function(ir) {
  scope <- ir[["scope"]]
  if (!is.list(scope)) return(FALSE)
  !is.null(scope[["cohort"]]) || length(scope[["tables"]] %||% list()) > 0L
}

#' Is a population indexed (amendment A-11's rule)?
#' @keywords internal
.populationIndexed <- function(ir, pop) {
  .pnv2PopulationIndexed(ir[["populations"]] %||% list(), pop)
}

#' Effective coverage policy of a population
#'
#' An index or criteria population's own policy; a set operation's members'
#' shared policy (validated); a match's cases policy (shared with its
#' template, validated).
#' @keywords internal
.populationCoveragePolicy <- function(ir, pop, seen = character(0)) {
  node <- ir[["populations"]][[pop]]
  if (!is.list(node) || pop %in% seen) return(NULL)
  seen <- c(seen, pop)
  switch(node[["kind"]] %||% "",
    index = , criteria = .coveragePolicy(node[["coverage"]], paste0("/populations/", pop, "/coverage")),
    setop = .populationCoveragePolicy(ir, as.character(unlist(node[["setop"]][["members"]]))[[1L]], seen),
    match = .populationCoveragePolicy(ir, node[["match"]][["cases"]], seen),
    NULL)
}

# Index-source tags of the plan in byte order, with their ranks (source_rank).
.populationSourceRanks <- function(ctx) {
  cached <- ctx[["population_source_ranks"]]
  if (!is.null(cached)) return(cached)
  ir <- ctx[["ir"]]
  tags <- unique(unlist(lapply(names(ir[["populations"]] %||% list()), function(p) {
    .irIndexSourceTags(ir, p)
  }), use.names = FALSE))
  tags <- .irSortNames(tags)
  ranks <- stats::setNames(seq_along(tags), tags)
  assign("population_source_ranks", ranks, envir = ctx)
  ranks
}

.populationSourceRank <- function(ctx, tag) {
  r <- .populationSourceRanks(ctx)[tag]
  if (length(r) != 1L || is.na(r)) {
    stop("Index-source tag '", tag, "' is not declared by the plan.", call. = FALSE)
  }
  as.integer(r)
}

# The roles a population plays in the plan: ordinary roster (output rows,
# parent, set-op member, match cases) and matching controls template.
.populationUses <- function(ir) {
  pops <- ir[["populations"]] %||% list()
  outs <- ir[["outputs"]] %||% list()
  ordinary <- unique(c(
    vapply(outs, function(o) as.character(o[["rows"]] %||% ""), ""),
    unlist(lapply(pops, function(q) {
      c(if (is.character(q[["parent"]])) q[["parent"]],
        as.character(unlist(q[["setop"]][["members"]] %||% list(), use.names = FALSE)),
        if (is.list(q[["match"]])) as.character(q[["match"]][["cases"]]))
    }), use.names = FALSE)))
  templates <- unlist(lapply(names(pops), function(p) {
    q <- pops[[p]]
    if (is.list(q[["match"]])) stats::setNames(as.character(q[["match"]][["controls"]]), p)
  }))
  list(ordinary = ordinary[nzchar(ordinary)], templates = templates %||% character(0))
}

# --- the plan scope ----------------------------------------------------------------

#' The persons of the plan scope as a fragment (or NULL without a scope)
#'
#' A scope narrows every population (design section 5: scope, not base). A
#' results-cohort scope (\code{scope.cohort} = an id, or \code{{type:
#' cohort_table, cohort_definition_id}}) without workspace tables compiles
#' over the results cohort table directly. Any other scope (a temporary
#' cohort name, \code{omop.table} workspace tables, combinations) is resolved
#' by the executor, which registers the person set as \code{ctx$scope_persons}
#' (a fragment with \code{person_id}); without it the populations refuse to
#' compile (fail closed: a scope is never ignored).
#' @keywords internal
.populationScopeFragment <- function(ctx) {
  ir <- ctx[["ir"]]
  if (!.populationScopeDeclared(ir)) return(NULL)
  nm <- .fragmentName("scope", "persons", ctx = ctx)
  existing <- .fragmentGet(ctx, nm)
  if (!is.null(existing)) return(existing)
  h <- .compileDialect(ctx)
  scope <- ir[["scope"]]
  cohort <- scope[["cohort"]]
  if (is.list(cohort) && !is.null(cohort[["cohort_definition_id"]])) {
    cohort <- cohort[["cohort_definition_id"]]
  }
  if (is.list(cohort) && length(cohort) == 1L && is.null(names(cohort))) cohort <- cohort[[1L]]
  provided <- ctx[["scope_persons"]]
  if (inherits(provided, "dsomop_fragment")) {
    body <- paste0("SELECT DISTINCT s.person_id AS person_id FROM ", provided$name, " s")
    deps <- provided$name
  } else if (is.numeric(cohort) && length(cohort) == 1L &&
             length(scope[["tables"]] %||% list()) == 0L) {
    body <- paste0("SELECT DISTINCT ", .sqlLosslessKey(h, "c.subject_id"), " AS person_id FROM ",
                   .populationCohortTable(ctx), " c WHERE c.cohort_definition_id = ",
                   .sqlIntegerLiteral(h, cohort))
    deps <- character(0)
  } else {
    stop("The plan scope must be resolved by the executor (ctx$scope_persons) before ",
         "populations compile.", call. = FALSE)
  }
  f <- .fragment(nm, "roster", "person", body,
                 columns = list(person_id = .fragmentColumn("bigint", FALSE, "person_key")),
                 dependencies = deps, unique_key = "person_id",
                 person_lineage = list(person_col = "person_id", via_roster = NULL),
                 source_identity_columns = "person_id")
  .sqlAssertTypedSql(h, f$select_body)
  .fragmentRegister(ctx, f)
}

# Qualified results cohort table.
.populationCohortTable <- function(ctx) {
  handle <- ctx[["handle"]]
  .qualifyTable(handle, "cohort", .effectiveResultsSchema(handle))
}

# Qualified CDM table through the blueprint.
.populationTable <- function(ctx, table) {
  handle <- ctx[["handle"]]
  bp <- .buildBlueprint(handle)
  if (!.adapterTablePresent(bp, table)) {
    .irReject(paste0("table '", table, "' is not available at this site"),
              code = "unavailable_table", field = "/populations")
  }
  .adapterQualified(handle, bp, table)
}

# --- compilation by name ---------------------------------------------------------

.populationMetaGet <- function(ctx, name) {
  memo <- if (is.environment(ctx[["population_meta"]])) ctx[["population_meta"]]
  if (is.null(memo) || !exists(name, envir = memo, inherits = FALSE)) return(NULL)
  get(name, envir = memo, inherits = FALSE)
}

.populationMetaSet <- function(ctx, name, result) {
  memo <- .compileMemo(ctx, "population_meta")
  assign(name, result, envir = memo)
  invisible(result)
}

#' Compile a population by name, with its dependencies
#'
#' Resolves the node through \code{ctx$ir} (\code{.irNamedNode()}) and its
#' dependencies in topological order (design section 6.1: parent, set-op
#' members, match cases, controls template and outcome source), then
#' dispatches to \code{.compilePopulation()}. Memoised per context.
#'
#' @param ctx Execution context.
#' @param name Population id.
#' @return The result of \code{.compilePopulation()}.
#' @keywords internal
.compilePopulationByName <- function(ctx, name, .stack = character(0)) {
  existing <- .populationMetaGet(ctx, name)
  if (!is.null(existing)) return(existing)
  if (name %in% .stack) {
    .irReject(paste0("population reference cycle: ", paste(c(.stack, name), collapse = " -> ")),
              field = paste0("/populations/", name))
  }
  ir <- ctx[["ir"]]
  node <- .irNamedNode(ir, "population", name)
  stack <- c(.stack, name)
  deps <- list()
  if (is.character(node[["parent"]])) {
    deps$parent <- .compilePopulationByName(ctx, node[["parent"]], stack)
  }
  if (identical(node[["kind"]], "setop")) {
    members <- as.character(unlist(node[["setop"]][["members"]], use.names = FALSE))
    deps$members <- lapply(stats::setNames(members, members), function(m) {
      .compilePopulationByName(ctx, m, stack)
    })
  }
  if (identical(node[["kind"]], "match")) {
    m <- node[["match"]]
    deps$cases <- .compilePopulationByName(ctx, m[["cases"]], stack)
    template <- .irNamedNode(ir, "population", m[["controls"]])
    if (is.character(template[["parent"]])) {
      deps$template_parent <- .compilePopulationByName(ctx, template[["parent"]], stack)
    }
    deps$controls <- template
    deps$outcome <- .populationSourceFragment(ctx, m[["outcome"]])
  }
  result <- .compilePopulation(ctx, node, deps)
  .populationMetaSet(ctx, name, result)
}

# The fragment of a typed source reference ({event} or {interval}: the course).
.populationSourceFragment <- function(ctx, ref) {
  kind <- names(ref)[[1L]]
  name <- unlist(ref[[1L]])
  if (identical(kind, "event")) return(.compileEventByName(ctx, name))
  if (identical(kind, "interval")) return(.compileIntervalByName(ctx, name)$course)
  .irReject("a source reference is {event: <name>} or {interval: <name>}")
}

#' Compile a population (frozen compiler signature, design section 6)
#'
#' @param ctx Execution context (\code{handle}, \code{ir}, \code{dialect}).
#' @param node The IR population node with its plan name in \code{attr(node,
#'   "name")}.
#' @param deps The compiled dependencies: \code{parent} (a population
#'   result), \code{members} (named list of population results, set
#'   operations), and for a match \code{cases} (population result),
#'   \code{controls} (the template population node), \code{template_parent}
#'   and \code{outcome} (the outcome source fragment).
#' @return \code{list(candidates, roster, coverage, segments)}: the candidates
#'   fragment (\code{cand_<pop>}; NULL for set operations), the final roster
#'   fragment (\code{pop_<pop>}), the coverage object of the roster (the
#'   result of \code{.compileCoverage()} on the candidates, or the derived
#'   object of a set operation or match; NULL for a person roster) and the
#'   actually observed segments of the final roster rows
#'   (\code{.compileObservedSegments()}, NULL without coverage); plus
#'   \code{kind}, \code{indexed} and, for a match, \code{pairs} (the private
#'   eligible pair relation).
#' @keywords internal
.compilePopulation <- function(ctx, node, deps = list()) {
  name <- .compileNodeName(node, "population")
  existing <- .populationMetaGet(ctx, name)
  if (!is.null(existing)) return(existing)
  .populationRefuseCompatibility(node, name)
  kind <- node[["kind"]] %||% ""
  result <- switch(kind,
    index = .compileIndexPopulation(ctx, name, node, deps),
    criteria = .compileCriteriaPopulation(ctx, name, node, deps),
    setop = .compileSetopPopulation(ctx, name, node, deps),
    match = .compileMatch(ctx, node, deps$cases$roster, deps$controls, deps$outcome,
                          cases = deps$cases, template_parent = deps$template_parent),
    .irReject(paste0("unknown population kind '", kind, "'"),
              field = paste0("/populations/", name, "/kind")))
  result$kind <- kind
  result$indexed <- .populationIndexed(ctx[["ir"]], name)
  if (is.null(result$segments) && !is.null(result$coverage) && isTRUE(result$indexed)) {
    result$segments <- .compileObservedSegments(ctx, result$coverage, result$roster, "final")
  }
  .populationMetaSet(ctx, name, result)
}

# The IR-only compatibility variants of lowered v1 plans run on the v1
# executors (amendment A-14); the language 2.0 compilers never execute them.
.populationRefuseCompatibility <- function(node, name) {
  src <- node[["index"]][["source"]]
  legacy <- !is.null(src[["legacy_spec"]]) || !is.null(node[["index"]][["legacy_end_strategy"]]) ||
    !is.null(node[["legacy_episodes"]]) || !is.null(node[["legacy_index_anchor"]])
  found <- FALSE
  .irWalkCriteria(node[["where"]], function(k, leaf, path) {
    if (identical(k, "legacy")) found <<- TRUE
  }, "")
  if (legacy || found) {
    stop("Population '", name, "' carries v1 compatibility variants; lowered v1 plans run ",
         "on the v1 executors (amendment A-14).", call. = FALSE)
  }
  invisible(TRUE)
}

# --- index populations --------------------------------------------------------------

# One source branch of an index population's candidates: a SELECT over the
# source fragment projecting the candidate columns.
.populationSourceBranch <- function(ctx, ref, order, pop, path) {
  h <- .compileDialect(ctx)
  legacy <- .sqlLegacyHandle(.sqlFamily(h))
  kind <- names(ref)[[1L]]
  nulls <- function(cols) .populationNulls(h, cols)
  key <- function(x) .sqlLosslessKey(h, x)
  if (identical(kind, "cohort_definition_id")) {
    cid <- as.integer(unlist(ref[[1L]]))
    tag <- paste0("cohort:", format(cid, scientific = FALSE))
    text_key <- .omopConcatSql(legacy, .compileTextLiteral(paste0(cid, ":")),
                               .sqlLosslessKeyText(h, "c.subject_id"), "':'",
                               .sqlDateText(h, "c.cohort_start_date"), "':'",
                               paste0("COALESCE(", .sqlDateText(h, "c.cohort_end_date"), ", '')"))
    epoch <- .sqlDateLiteral(h, "1970-01-01")
    cols <- c(
      subject_id = key("c.subject_id"),
      source_start = "c.cohort_start_date",
      source_end = "c.cohort_end_date",
      index_source = .compileTextLiteral(tag),
      index_key = text_key,
      index_visit_id = .compileTypedNull(h, "bigint"),
      source_rank = .sqlIntegerLiteral(h, .populationSourceRank(ctx, tag)),
      source_order = .sqlIntegerLiteral(h, order),
      key_1 = key(.sqlIntegerLiteral(h, cid)),
      key_2 = key(.sqlDateDiffDays(h, epoch, "c.cohort_start_date")),
      key_3 = key(.sqlDateDiffDays(h, epoch, "c.cohort_end_date")))
    body <- paste0("SELECT ", paste(paste0(cols, " AS ", names(cols)), collapse = ", "),
                   " FROM (SELECT DISTINCT d.subject_id AS subject_id, d.cohort_start_date AS ",
                   "cohort_start_date, d.cohort_end_date AS cohort_end_date FROM ",
                   .populationCohortTable(ctx), " d WHERE d.cohort_definition_id = ",
                   .sqlIntegerLiteral(h, cid), ") c WHERE c.cohort_start_date IS NOT NULL")
    return(list(body = body, deps = character(0), tag = tag))
  }
  if (!kind %in% c("event", "interval")) {
    .irReject("an index source is an event, an interval, a results cohort or any_of",
              field = path)
  }
  name <- unlist(ref[[1L]])
  tag <- name
  rank <- .sqlIntegerLiteral(h, .populationSourceRank(ctx, tag))
  if (identical(kind, "interval")) {
    f <- .compileIntervalByName(ctx, name)$course
    cols <- c(
      subject_id = key("e.person_id"), source_start = "e.start_date", source_end = "e.end_date",
      index_source = .compileTextLiteral(tag), index_key = "e.era_key",
      index_visit_id = .compileTypedNull(h, "bigint"), source_rank = rank,
      source_order = .sqlIntegerLiteral(h, order), key_1 = key("e.group_id"),
      key_2 = key("e.era_no"), key_3 = .compileTypedNull(h, "bigint"))
  } else {
    f <- .compileEventByName(ctx, name)
    meta <- .eventMeta(ctx, name)
    if (identical(meta$kind, "episode_members")) {
      members <- .irSortNames(.irEpisodeEventFields()$table_name)
      rank_sql <- paste0("CASE ", paste(paste0("WHEN e.member_table = ", vapply(members, .compileTextLiteral, ""),
                                               " THEN ", seq_along(members)), collapse = " "),
                         " END")
      text_key <- .omopConcatSql(legacy, "e.parent_source_ns", "':'",
                                 .sqlLosslessKeyText(h, "e.parent_event_id"), "':'",
                                 "e.member_table", "':'", .sqlLosslessKeyText(h, "e.event_id"))
      keys <- c(key_1 = key("e.parent_event_id"), key_2 = key(rank_sql), key_3 = key("e.event_id"))
    } else {
      text_key <- .sqlLosslessKeyText(h, "e.event_id")
      keys <- c(key_1 = key("e.event_id"), key_2 = .compileTypedNull(h, "bigint"),
                key_3 = .compileTypedNull(h, "bigint"))
    }
    cols <- c(
      subject_id = key("e.person_id"), source_start = "e.start_date", source_end = "e.end_date",
      index_source = .compileTextLiteral(tag), index_key = text_key,
      index_visit_id = key("e.visit_id"), source_rank = rank,
      source_order = .sqlIntegerLiteral(h, order), keys)
  }
  body <- paste0("SELECT ", paste(paste0(cols, " AS ", names(cols)), collapse = ", "),
                 " FROM ", f$name, " e WHERE e.start_date IS NOT NULL")
  list(body = body, deps = f$name, tag = tag)
}

# The person restrictions of a population's candidates: the plan scope and,
# for an indexed child (index population with a parent), the parent's persons.
.populationPersonFilters <- function(ctx, alias, person_col, parent = NULL) {
  conds <- character(0)
  deps <- character(0)
  scope <- .populationScopeFragment(ctx)
  if (!is.null(scope)) {
    conds <- c(conds, paste0("EXISTS (SELECT 1 FROM ", scope$name, " sp WHERE sp.person_id = ",
                             alias, ".", person_col, ")"))
    deps <- c(deps, scope$name)
  }
  if (!is.null(parent)) {
    conds <- c(conds, paste0("EXISTS (SELECT 1 FROM ", parent$name, " pp WHERE pp.subject_id = ",
                             alias, ".", person_col, ")"))
    deps <- c(deps, parent$name)
  }
  list(conds = conds, deps = deps)
}

.compileIndexPopulation <- function(ctx, name, node, deps) {
  h <- .compileDialect(ctx)
  base <- paste0("/populations/", name)
  idx <- node[["index"]]
  src <- idx[["source"]]
  refs <- if (!is.null(src[["any_of"]])) .jsonArrayItems(src[["any_of"]]) else list(src)
  branches <- lapply(seq_along(refs), function(i) {
    .populationSourceBranch(ctx, refs[[i]], i, name, paste0(base, "/index/source"))
  })
  src_cols <- c("subject_id", "source_start", "source_end", "index_source", "index_key",
                "index_visit_id", "source_rank", "source_order", "key_1", "key_2", "key_3")
  col <- .fragmentColumn
  src_spec <- list(subject_id = col("bigint", FALSE, "person_key"),
                   source_start = col("date", FALSE, "start"), source_end = col("date", TRUE, "end"),
                   index_source = col("text", FALSE, "origin_identity"),
                   index_key = col("text", TRUE, "origin_identity"),
                   index_visit_id = col("bigint", TRUE, "visit"),
                   source_rank = col("integer", FALSE, "origin_identity"),
                   source_order = col("integer", FALSE, "private"),
                   key_1 = col("bigint", TRUE, "origin_identity"),
                   key_2 = col("bigint", TRUE, "origin_identity"),
                   key_3 = col("bigint", TRUE, "origin_identity"))
  f_src <- .fragment(.fragmentName("cand", name, "src", ctx = ctx), "candidates", "episode",
                     paste(vapply(branches, function(b) b$body, ""), collapse = " UNION ALL "),
                     columns = src_spec,
                     dependencies = unique(unlist(lapply(branches, function(b) b$deps))),
                     unique_key = c("subject_id", "source_start", "source_rank", "key_1", "key_2", "key_3"),
                     person_lineage = list(person_col = "subject_id", via_roster = NULL),
                     private_columns = c("index_key", "index_visit_id", "source_rank", "source_order",
                                         "key_1", "key_2", "key_3"))
  attr(f_src, "population") <- name
  .sqlAssertTypedSql(h, f_src$select_body)
  .fragmentRegister(ctx, f_src)

  # candidates before coverage: offsets applied, persons restricted, keyed
  offset <- as.integer(unlist(idx[["offset"]] %||% 0L))
  start <- if (offset == 0L) "s.source_start" else .sqlDateAddDays(h, "s.source_start", offset)
  filters <- .populationPersonFilters(ctx, "s", "subject_id", deps$parent$roster)
  cand_key <- .sqlLosslessKey(h, .sqlRowNumber(h, character(0), c(
    "s.subject_id", .populationOrder(h, "s", c("source_start", "source_end", "source_rank",
                                               "key_1", "key_2", "key_3")))))
  raw_over <- c(list(cohort_start_date = start, cohort_end_date = "s.source_end"),
                .populationNulls(h, c("set_id", "is_case", "sampling_weight", "op_start", "op_end",
                                      "op_component_id")))
  raw_body <- paste0("SELECT ", cand_key, " AS cand_key, ",
                     paste(.populationSelect("s", raw_over), collapse = ", "),
                     ", s.source_order AS source_order FROM ", f_src$name, " s",
                     if (length(filters$conds)) paste0(" WHERE ", paste(filters$conds, collapse = " AND ")))
  f_raw <- .populationCandidateFragment(
    ctx, name, "raw", raw_body, c(f_src$name, filters$deps), indexed = TRUE,
    extra = list(source_order = col("integer", FALSE, "private")))

  # coverage (once, on the candidates; A-20 c) and ends
  policy <- .coveragePolicy(node[["coverage"]], paste0(base, "/coverage"))
  cov <- .compileCoverage(ctx, name, policy, f_raw, "candidate")
  .coverageAssertUnambiguous(ctx, cov, name)
  end <- .populationEndSql(h, idx[["end"]], "r", base)
  rank_order <- c(.populationOrder(h, "r", c("cohort_start_date", "source_order", "source_rank",
                                             "key_1", "key_2", "key_3")))
  rank_asc <- .sqlRowNumber(h, "r.subject_id", rank_order)
  rank_last <- paste0("(", .sqlWindow(h, "COUNT(*)", partition = "r.subject_id"), " - ",
                      rank_asc, " + 1)")
  cand_over <- list(cohort_end_date = end, op_start = "o.op_start", op_end = "o.op_end",
                    op_component_id = "o.component_id")
  cand_body <- paste0("SELECT r.cand_key AS cand_key, ",
                      paste(.populationSelect("r", cand_over), collapse = ", "),
                      ", r.source_order AS source_order, ",
                      .sqlIntegerCast(h, rank_asc), " AS rank_asc, ",
                      .sqlIntegerCast(h, rank_last), " AS rank_last FROM ", f_raw$name,
                      " r INNER JOIN ", cov$coverage$name, " o ON o.row_key = r.cand_key WHERE ",
                      .coverageKeepSql(policy, "o"))
  f_cand <- .populationCandidateFragment(
    ctx, name, NULL, cand_body, c(f_raw$name, cov$coverage$name), indexed = TRUE,
    extra = list(source_order = col("integer", FALSE, "private"),
                 rank_asc = col("integer", FALSE, "private"),
                 rank_last = col("integer", FALSE, "private")))

  # criteria per candidate, then selection
  select <- idx[["select"]] %||% "first"
  n <- as.integer(unlist(idx[["n"]] %||% 1L))
  restrict <- switch(select,
    first = "c.rank_asc = 1",
    nth = paste0("c.rank_asc = ", .sqlIntegerLiteral(h, n)),
    last = "c.rank_last = 1",
    NULL)
  f_crit <- .populationCriteriaFragment(ctx, name, node[["where"]], f_cand, f_raw, policy,
                                        restrict = restrict, carry = c("source_order"))
  eligible <- "(x.cohort_end_date IS NULL OR x.cohort_end_date >= x.cohort_start_date)"
  sel_body <- if (identical(select, "first_eligible")) {
    rn <- .sqlRowNumber(h, "x.subject_id", .populationOrder(h, "x", c(
      "cohort_start_date", "source_order", "source_rank", "key_1", "key_2", "key_3")))
    paste0("SELECT ", paste(.populationSelect("y"), collapse = ", "), " FROM (SELECT ",
           paste(.populationSelect("x"), collapse = ", "), ", ", rn, " AS rn FROM ", f_crit$name,
           " x WHERE x.crit_true = 1 AND ", eligible, ") y WHERE y.rn = 1")
  } else {
    paste0("SELECT ", paste(.populationSelect("x"), collapse = ", "), " FROM ", f_crit$name,
           " x WHERE x.crit_true = 1 AND ", eligible)
  }
  f_sel <- .populationSelectionFragment(ctx, name, sel_body, f_crit$name, indexed = TRUE)
  roster <- .populationRosterFragment(ctx, name, .rankedRosterSql(h, f_sel$name), f_sel$name,
                                      indexed = TRUE)
  list(candidates = f_cand, roster = roster, coverage = cov, segments = NULL)
}

# The selected episodes of a population (before cohort_row_id allocation).
.populationSelectionFragment <- function(ctx, name, body, dependencies, indexed) {
  f <- .fragment(.fragmentName("pop", name, "sel", ctx = ctx), "candidates", "episode", body,
                 columns = .populationColumnSpec(indexed), dependencies = unique(dependencies),
                 unique_key = c("subject_id", "cohort_start_date", "cohort_end_date", .POP_KEY_COLUMNS),
                 person_lineage = list(person_col = "subject_id", via_roster = NULL),
                 private_columns = .POP_PRIVATE_COLUMNS)
  attr(f, "population") <- name
  .sqlAssertTypedSql(.compileDialect(ctx), f$select_body)
  .fragmentRegister(ctx, f)
}

#' End of an index episode (design section 6.9, amendment A-1)
#'
#' \code{observation_end} -> \code{op_end}; \code{fixed} -> index + days;
#' \code{event_end} / \code{interval_end} -> the source end (the event's own
#' end, the course end of an interval, the results-cohort end) + pad; capped
#' at \code{op_end} when covered (an uncovered candidate kept under
#' \code{on_missing = "keep"} keeps its finite end); a NULL source end stays
#' NULL.
#' @keywords internal
.populationEndSql <- function(h, end, alias, base) {
  strategy <- end[["strategy"]] %||% "observation_end"
  days <- end[["days"]]
  if (is.list(days)) days <- unlist(days)
  pad <- as.integer(unlist(end[["pad"]] %||% 0L))
  op_end <- "o.op_end"
  capped <- function(expr) {
    paste0("CASE WHEN ", expr, " IS NULL THEN NULL ELSE ", .sqlLeast(h, expr, op_end), " END")
  }
  switch(strategy,
    observation_end = op_end,
    fixed = {
      if (is.null(days)) .irReject("a fixed end strategy requires days", field = paste0(base, "/index/end/days"))
      capped(.sqlDateAddDays(h, paste0(alias, ".cohort_start_date"), as.integer(days)))
    },
    event_end = , interval_end = {
      src <- paste0(alias, ".cohort_end_date")
      capped(if (pad == 0L) src else .sqlDateAddDays(h, src, pad))
    },
    .irReject(paste0("unknown end strategy '", strategy, "'"), field = paste0(base, "/index/end/strategy")))
}

# --- criteria ----------------------------------------------------------------------

#' The criteria of a population evaluated per candidate (design section 6.9)
#'
#' One fragment \code{cand_<pop>_crit}: the candidate columns plus
#' \code{crit_true} (1 when the complete criterion tree is TRUE; FALSE and
#' UNKNOWN give 0). Leaves compile with native three-valued logic; every
#' window-dependent leaf is wrapped by \code{.windowGuardSql()} so a missing
#' anchor is UNKNOWN also for absence and \code{count == 0}.
#'
#' @param ctx Execution context.
#' @param pop Population id.
#' @param where The criteria tree (or NULL: every candidate is TRUE).
#' @param cand The candidates fragment the criteria read (alias \code{c}).
#' @param raw The candidates fragment the population's coverage was compiled
#'   on (criterion-specific \code{omop_observed(gap =)} requests use it too).
#' @param policy The population's coverage policy.
#' @param restrict Optional SQL predicate on \code{c} restricting the rows
#'   evaluated (first/last/nth selection).
#' @param carry Extra candidate columns carried to the result.
#' @return The fragment.
#' @keywords internal
.populationCriteriaFragment <- function(ctx, pop, where, cand, raw, policy, restrict = NULL,
                                        carry = character(0), indexed = TRUE) {
  h <- .compileDialect(ctx)
  state <- new.env(parent = emptyenv())
  state$joins <- character(0)
  state$deps <- c(cand$name)
  expr <- if (is.null(where)) "1 = 1" else {
    .populationCriteriaSql(ctx, where, cand, raw, pop, policy, state,
                           paste0("/populations/", pop, "/where"))
  }
  carry_cols <- carry[carry %in% names(cand$columns)]
  extra <- lapply(carry_cols, function(x) cand$columns[[x]])
  names(extra) <- carry_cols
  body <- paste0("SELECT c.cand_key AS cand_key, ", paste(.populationSelect("c"), collapse = ", "),
                 if (length(carry_cols)) paste0(", ", paste(paste0("c.", carry_cols, " AS ", carry_cols), collapse = ", ")),
                 ", CASE WHEN ", expr, " THEN 1 ELSE 0 END AS crit_true FROM ", cand$name, " c",
                 if (length(state$joins)) paste0(" ", paste(unname(state$joins), collapse = " ")),
                 if (!is.null(restrict)) paste0(" WHERE ", restrict))
  extra$crit_true <- .fragmentColumn("integer", FALSE, "private")
  .populationCandidateFragment(ctx, pop, "crit", body, state$deps, indexed = indexed, extra = extra)
}

# Compile a criteria tree to one SQL boolean expression over alias c.
.populationCriteriaSql <- function(ctx, node, cand, raw, pop, policy, state, path) {
  h <- .compileDialect(ctx)
  if (!is.list(node) || is.null(names(node)) || length(node) != 1L) {
    .irReject("a criterion is an object with one key", field = path)
  }
  key <- names(node)[[1L]]
  value <- node[[key]]
  walk <- function(x, p) .populationCriteriaSql(ctx, x, cand, raw, pop, policy, state, p)
  switch(key,
    and = , or = {
      items <- .jsonArrayItems(value)
      if (!length(items)) .irReject(paste0("an ", key, " group needs at least one criterion"), field = path)
      parts <- vapply(seq_along(items), function(i) walk(items[[i]], paste0(path, "/", key, "/", i - 1L)), "")
      if (length(parts) == 1L) parts else
        paste0("(", paste(parts, collapse = if (identical(key, "and")) " AND " else " OR "), ")")
    },
    not = paste0("(NOT ", walk(value, paste0(path, "/not")), ")"),
    has = .populationHasSql(ctx, value, cand, "candidate", "c", pop, state, paste0(path, "/has")),
    person = {
      .populationPersonJoin(ctx, state)
      cat <- .predicateTableCatalogue(ctx[["handle"]], "person", what = "population person criteria")
      .predicateSql(h, value, cat, "pp", paste0(path, "/person"))
    },
    age = .populationAgeSql(ctx, value, state, paste0(path, "/age")),
    observed = .populationObservedSql(ctx, value, raw, pop, state, paste0(path, "/observed")),
    cohort = {
      cid <- as.integer(unlist(value[["cohort_definition_id"]]))
      paste0("EXISTS (SELECT 1 FROM ", .populationCohortTable(ctx), " rc WHERE ",
             "rc.cohort_definition_id = ", .sqlIntegerLiteral(h, cid),
             " AND rc.subject_id = c.subject_id)")
    },
    legacy = stop("Legacy criteria leaves run on the v1 executors (amendment A-14).", call. = FALSE),
    .irReject(paste0("unknown criterion '", key, "'"), field = path))
}

# Add the person-table join (alias pp) once.
.populationPersonJoin <- function(ctx, state) {
  if (!"pp" %in% names(state$joins)) {
    state$joins[["pp"]] <- paste0("LEFT JOIN ", .populationTable(ctx, "person"),
                                  " pp ON pp.person_id = c.subject_id")
  }
  invisible(TRUE)
}

# Add named joins (anchors, criterion-specific coverage) once each.
.populationAddJoins <- function(state, joins, deps) {
  for (nm in names(joins)) if (!nm %in% names(state$joins)) state$joins[[nm]] <- joins[[nm]]
  state$deps <- unique(c(state$deps, deps))
  invisible(TRUE)
}

#' The \code{has} leaf (design sections 2.3 and 6.9)
#'
#' \code{(SELECT COUNT(<distinct key>) FROM <source> e WHERE e.person_id =
#' <roster>.subject_id AND <window>) <op> n}, as EXISTS / NOT EXISTS for
#' \code{>= 1} and \code{== 0}; \code{distinct = "day"} counts start dates,
#' \code{"visit"} non-NULL visits, \code{"concept"} non-NULL concepts;
#' \code{nonmissing} keeps rows with a non-NULL column. The consumer
#' association of amendment A-20 (a) applies: an index-visit event joins
#' \code{e.visit_id = <roster>.index_visit_id} (a NULL visit matches no row),
#' and an episode-member source on a roster indexed on its parent episode
#' source binds \code{parent_event_id} to the row's origin with the same person
#' (person scope for rows of other origins). An unavailable source is UNKNOWN;
#' a window with a missing anchor is UNKNOWN (\code{.windowGuardSql()}).
#'
#' @param ctx Execution context.
#' @param leaf The \code{has} object.
#' @param roster The roster or candidates fragment the alias reads.
#' @param phase \code{"candidate"} or \code{"final"}.
#' @param alias SQL alias of the roster rows.
#' @param pop The population the roster belongs to.
#' @param state Accumulator environment (\code{joins}, \code{deps}).
#' @param path JSON path for rejections.
#' @return SQL boolean expression.
#' @keywords internal
.populationHasSql <- function(ctx, leaf, roster, phase, alias, pop, state, path) {
  h <- .compileDialect(ctx)
  ref <- leaf[["source"]]
  src_kind <- names(ref)[[1L]]
  src_name <- unlist(ref[[1L]])
  f <- .populationSourceFragment(ctx, ref)
  win <- .compileWindow(ctx, leaf[["window"]], roster, phase, alias = alias)
  .populationAddJoins(state, win$joins, c(win$dependencies, f$name))
  if (identical(f$availability, "unavailable")) return(.populationUnknownSql(h))
  conds <- c(paste0("e.person_id = ", alias, ".subject_id"),
             .windowStartPredicateSql(win, "e.start_date"),
             .populationAssociationSql(ctx, src_kind, src_name, alias, pop))
  distinct <- leaf[["distinct"]] %||% "row"
  nonmissing <- leaf[["nonmissing"]]
  if (is.list(nonmissing)) nonmissing <- unlist(nonmissing)
  if (!is.null(nonmissing)) {
    column <- .populationSourceColumn(ctx, src_kind, src_name, nonmissing, paste0(path, "/nonmissing"))
    conds <- c(conds, paste0("e.", column, " IS NOT NULL"))
  }
  distinct_col <- switch(distinct, row = NULL, day = "e.start_date", visit = "e.visit_id",
                         concept = "e.concept_id",
                         .irReject("distinct is row, day, visit or concept", field = paste0(path, "/distinct")))
  exists_conds <- c(conds, if (!is.null(distinct_col)) paste0(distinct_col, " IS NOT NULL"))
  from <- paste0(" FROM ", f$name, " e WHERE ")
  exists <- paste0("EXISTS (SELECT 1", from, paste(exists_conds, collapse = " AND "), ")")
  count <- paste0("(SELECT ", if (is.null(distinct_col)) "COUNT(*)" else paste0("COUNT(DISTINCT ", distinct_col, ")"),
                  from, paste(conds, collapse = " AND "), ")")
  op <- leaf[["op"]] %||% ">="
  n <- as.integer(unlist(leaf[["n"]] %||% 1L))
  lit <- function(x) .sqlIntegerLiteral(h, x)
  sql <- switch(op,
    `>=` = if (n[[1L]] <= 0L) "1 = 1" else if (n[[1L]] == 1L) exists else paste0(count, " >= ", lit(n[[1L]])),
    `==` = if (n[[1L]] == 0L) paste0("NOT ", exists) else paste0(count, " = ", lit(n[[1L]])),
    `<=` = if (n[[1L]] == 0L) paste0("NOT ", exists) else paste0(count, " <= ", lit(n[[1L]])),
    between = {
      if (length(n) != 2L) .irReject("between takes two bounds", field = paste0(path, "/n"))
      if (n[[1L]] <= 0L) paste0(count, " <= ", lit(n[[2L]])) else
        paste0("(", count, " >= ", lit(n[[1L]]), " AND ", count, " <= ", lit(n[[2L]]), ")")
    },
    .irReject("has op is >=, <=, == or between", field = paste0(path, "/op")))
  .windowGuardSql(win, paste0("(", sql, ")"))
}

#' Consumer association of a source over a roster (amendment A-20 a)
#'
#' An index-visit event (\code{visit = {index: true}}) joins the roster row's
#' \code{index_visit_id}; an episode-member source whose parent episode source
#' is an index source of the population binds \code{parent_event_id} to the
#' row's origin (\code{key_1}, the source row key) for rows of that origin,
#' with the same person; rows of other origins keep person scope.
#' @return Character vector of SQL conditions on alias \code{e}.
#' @keywords internal
.populationAssociationSql <- function(ctx, kind, name, alias, pop) {
  if (!identical(kind, "event")) return(character(0))
  meta <- .eventMeta(ctx, name)
  if (is.null(meta)) return(character(0))
  out <- character(0)
  if (isTRUE(meta$index_visit)) out <- c(out, paste0("e.visit_id = ", alias, ".index_visit_id"))
  if (identical(meta$kind, "episode_members") && .isScalarString(meta$parent)) {
    tags <- .irIndexSourceTags(ctx[["ir"]], pop)
    if (meta$parent %in% tags) {
      rank <- .sqlIntegerLiteral(.compileDialect(ctx), .populationSourceRank(ctx, meta$parent))
      out <- c(out, paste0("(", alias, ".source_rank IS NULL OR ", alias, ".source_rank <> ", rank,
                           " OR e.parent_event_id = ", alias, ".key_1)"))
    }
  }
  out
}

# A column of a source a criterion may test for presence (nonmissing): the
# event's predicate catalogue (the configured value column binds to the
# normalised value); an interval course offers its dates and concept.
.populationSourceColumn <- function(ctx, kind, name, column, path) {
  if (identical(kind, "event")) {
    meta <- .eventMeta(ctx, name)
    if (is.null(meta$catalogue)) return(.irReject("nonmissing needs an available event source", field = path))
    return(.predicateResolve(meta$catalogue, column, path)$column)
  }
  column <- tolower(column)
  if (!column %in% c("start_date", "end_date", "concept_id")) {
    .irReject("nonmissing on an interval names start_date, end_date or concept_id", field = path)
  }
  column
}

#' The age criterion (design sections 2.3 and 6.9)
#'
#' Annual age \code{.sqlYear(<reference>) - year_of_birth} within
#' \code{[min, max]} (either bound may be NULL); a NULL birth year or a
#' negative age (a future birth year) is UNKNOWN; \code{reference = "index"}
#' reads the row's index (UNKNOWN for a person roster), \code{{year: Y}} a
#' fixed year.
#' @keywords internal
.populationAgeSql <- function(ctx, leaf, state, path) {
  h <- .compileDialect(ctx)
  .populationPersonJoin(ctx, state)
  ref <- leaf[["reference"]] %||% "index"
  year <- if (is.list(ref) && !is.null(ref[["year"]])) {
    .sqlIntegerLiteral(h, as.integer(unlist(ref[["year"]])))
  } else if (identical(unlist(ref), "index")) {
    .sqlYear(h, "c.cohort_start_date")
  } else {
    .irReject("an age reference is index or {year}", field = paste0(path, "/reference"))
  }
  age <- paste0("(", year, " - pp.year_of_birth)")
  parts <- character(0)
  lo <- leaf[["min"]]
  hi <- leaf[["max"]]
  if (is.list(lo)) lo <- unlist(lo)
  if (is.list(hi)) hi <- unlist(hi)
  if (!is.null(lo)) parts <- c(parts, paste0(age, " >= ", .sqlIntegerLiteral(h, as.integer(lo))))
  if (!is.null(hi)) parts <- c(parts, paste0(age, " <= ", .sqlIntegerLiteral(h, as.integer(hi))))
  bounds <- if (length(parts)) paste(parts, collapse = " AND ") else "1 = 1"
  paste0("(CASE WHEN pp.year_of_birth IS NULL OR ", age, " < 0 OR ", year,
         " IS NULL THEN NULL ELSE (", bounds, ") END)")
}

#' The observed criterion (design sections 2.3, 6.9 and 6.13)
#'
#' On the population's policy (the candidate's \code{op_start}/\code{op_end})
#' or, with \code{gap}, on a criterion-specific union coverage request
#' (suffix \code{g<gap>}) compiled once on the same candidates. NULL bounds
#' (an uncovered or ambiguous candidate, a person roster) are UNKNOWN.
#' @keywords internal
.populationObservedSql <- function(ctx, leaf, raw, pop, state, path) {
  h <- .compileDialect(ctx)
  num <- function(x) { if (is.list(x)) x <- unlist(x); if (is.null(x)) NULL else as.integer(x) }
  before <- num(leaf[["before"]])
  after <- num(leaf[["after"]])
  gap <- num(leaf[["gap"]])
  sql <- if (is.null(gap)) {
    .coverageObservedSql(h, "c", "c.cohort_start_date", before, after)
  } else {
    if (is.null(raw)) return(.populationUnknownSql(h))
    request <- list(policy = "union", on_ambiguous = "drop", on_missing = "drop", gap = gap)
    cov <- .compileCoverage(ctx, pop, request, raw, "candidate", suffix = paste0("g", gap))
    alias <- paste0("og", gap)
    .populationAddJoins(state, stats::setNames(paste0(
      "LEFT JOIN ", cov$coverage$name, " ", alias, " ON ", alias, ".row_key = c.cand_key"), alias),
      cov$coverage$name)
    .coverageObservedSql(h, alias, "c.cohort_start_date", before, after)
  }
  paste0("(", sql, ")")
}

# --- criteria populations --------------------------------------------------------

.compileCriteriaPopulation <- function(ctx, name, node, deps) {
  h <- .compileDialect(ctx)
  base <- paste0("/populations/", name)
  ir <- ctx[["ir"]]
  parent <- deps$parent
  cohort <- node[["cohort_definition_id"]]
  if (is.list(cohort)) cohort <- unlist(cohort)
  policy <- .coveragePolicy(node[["coverage"]], paste0(base, "/coverage"))
  episode_policy <- node[["episode_policy"]]
  if (is.list(episode_policy)) episode_policy <- unlist(episode_policy)
  if (!is.null(parent)) {
    return(.compileChildPopulation(ctx, name, node, parent, cohort, policy))
  }
  if (!is.null(cohort)) {
    return(.compileSeededPopulation(ctx, name, node, as.integer(cohort), policy, episode_policy))
  }
  # every person in scope
  filters <- .populationPersonFilters(ctx, "p", "person_id")
  over <- c(list(subject_id = .sqlLosslessKey(h, "p.person_id")),
            .populationNulls(h, setdiff(.POP_ROSTER_COLUMNS, "subject_id")))
  raw_body <- paste0("SELECT ", .sqlLosslessKey(h, "p.person_id"), " AS cand_key, ",
                     paste(.populationSelect("p", over), collapse = ", "), " FROM ",
                     .populationTable(ctx, "person"), " p",
                     if (length(filters$conds)) paste0(" WHERE ", paste(filters$conds, collapse = " AND ")))
  f_raw <- .populationCandidateFragment(ctx, name, "raw", raw_body, filters$deps, indexed = FALSE)
  f_cand <- .populationCandidateFragment(
    ctx, name, NULL, paste0("SELECT r.cand_key AS cand_key, ", paste(.populationSelect("r"), collapse = ", "),
                            " FROM ", f_raw$name, " r"), f_raw$name, indexed = FALSE)
  f_crit <- .populationCriteriaFragment(ctx, name, node[["where"]], f_cand, NULL, policy, indexed = FALSE)
  f_sel <- .populationSelectionFragment(
    ctx, name, paste0("SELECT ", paste(.populationSelect("x"), collapse = ", "), " FROM ", f_crit$name,
                      " x WHERE x.crit_true = 1"), f_crit$name, indexed = FALSE)
  roster <- .populationRosterFragment(ctx, name, .rankedRosterSql(h, f_sel$name), f_sel$name, indexed = FALSE)
  list(candidates = f_cand, roster = roster, coverage = NULL, segments = NULL)
}

# A criteria population with a parent (design section 6.9; defect B-8): the
# parent's final roster is the candidate roster (cand_key := cohort_row_id,
# A-20 c), criteria evaluated per episode; an indexed child applies its own
# coverage policy (compiled once on its candidates); a results-cohort id
# additionally restricts the persons.
.compileChildPopulation <- function(ctx, name, node, parent, cohort, policy) {
  h <- .compileDialect(ctx)
  base <- paste0("/populations/", name)
  indexed <- isTRUE(parent$indexed)
  conds <- character(0)
  if (!is.null(cohort)) {
    conds <- paste0("EXISTS (SELECT 1 FROM ", .populationCohortTable(ctx), " sc WHERE ",
                    "sc.cohort_definition_id = ", .sqlIntegerLiteral(h, as.integer(cohort)),
                    " AND sc.subject_id = p.subject_id)")
  }
  raw_over <- .populationNulls(h, c("op_start", "op_end", "op_component_id"))
  raw_body <- paste0("SELECT p.cohort_row_id AS cand_key, ",
                     paste(.populationSelect("p", raw_over), collapse = ", "), " FROM ",
                     parent$roster$name, " p",
                     if (length(conds)) paste0(" WHERE ", paste(conds, collapse = " AND ")))
  f_raw <- .populationCandidateFragment(ctx, name, "raw", raw_body, parent$roster$name, indexed = indexed)
  cov <- NULL
  if (indexed) {
    cov <- .compileCoverage(ctx, name, policy, f_raw, "candidate")
    .coverageAssertUnambiguous(ctx, cov, name)
    over <- list(op_start = "o.op_start", op_end = "o.op_end", op_component_id = "o.component_id")
    cand_body <- paste0("SELECT r.cand_key AS cand_key, ", paste(.populationSelect("r", over), collapse = ", "),
                        " FROM ", f_raw$name, " r INNER JOIN ", cov$coverage$name,
                        " o ON o.row_key = r.cand_key WHERE ", .coverageKeepSql(policy, "o"))
    f_cand <- .populationCandidateFragment(ctx, name, NULL, cand_body,
                                           c(f_raw$name, cov$coverage$name), indexed = TRUE)
  } else {
    f_cand <- .populationCandidateFragment(
      ctx, name, NULL, paste0("SELECT r.cand_key AS cand_key, ", paste(.populationSelect("r"), collapse = ", "),
                              " FROM ", f_raw$name, " r"), f_raw$name, indexed = FALSE)
  }
  f_crit <- .populationCriteriaFragment(ctx, name, node[["where"]], f_cand, if (indexed) f_raw,
                                        policy, indexed = indexed)
  f_sel <- .populationSelectionFragment(
    ctx, name, paste0("SELECT ", paste(.populationSelect("x"), collapse = ", "), " FROM ", f_crit$name,
                      " x WHERE x.crit_true = 1"), f_crit$name, indexed = indexed)
  roster <- .populationRosterFragment(ctx, name, .rankedRosterSql(h, f_sel$name), f_sel$name,
                                      indexed = indexed)
  list(candidates = f_cand, roster = roster, coverage = cov, segments = NULL)
}

# A criteria population seeded from a results cohort (design section 6.9,
# "episode anchoring and episode_policy for criteria populations as today",
# S/R/plan.R:959-1023): the cohort's episodes are the candidates (index = the
# episode start), the criteria are evaluated per episode, and the person
# roster keeps the persons whose episodes satisfy the episode policy:
# any_episode (some episode TRUE), all_episodes (every episode TRUE; UNKNOWN
# counts as not TRUE), first_episode / last_episode (the earliest / latest
# episode by (start, end) TRUE). Without a policy the criteria are
# index-independent (validated), so every episode gives the same value.
.compileSeededPopulation <- function(ctx, name, node, cohort, policy, episode_policy) {
  h <- .compileDialect(ctx)
  base <- paste0("/populations/", name)
  ref <- list(cohort_definition_id = cohort)
  branch <- .populationSourceBranch(ctx, ref, 1L, name, paste0(base, "/cohort_definition_id"))
  filters <- .populationPersonFilters(ctx, "s", "subject_id")
  cand_key <- .sqlLosslessKey(h, .sqlRowNumber(h, character(0), c(
    "s.subject_id", .populationOrder(h, "s", c("source_start", "source_end")))))
  over <- c(list(cohort_start_date = "s.source_start", cohort_end_date = "s.source_end"),
            .populationNulls(h, c("set_id", "is_case", "sampling_weight", "op_start", "op_end",
                                  "op_component_id")))
  raw_body <- paste0("SELECT ", cand_key, " AS cand_key, ", paste(.populationSelect("s", over), collapse = ", "),
                     " FROM (", branch$body, ") s",
                     if (length(filters$conds)) paste0(" WHERE ", paste(filters$conds, collapse = " AND ")))
  f_raw <- .populationCandidateFragment(ctx, name, "raw", raw_body, c(branch$deps, filters$deps),
                                        indexed = TRUE)
  needs_cov <- .populationCriteriaNeedObservation(node[["where"]])
  cov <- NULL
  rank_order <- .populationOrder(h, "r", c("cohort_start_date", "cohort_end_date"))
  rank_asc <- .sqlRowNumber(h, "r.subject_id", rank_order)
  rank_last <- paste0("(", .sqlWindow(h, "COUNT(*)", partition = "r.subject_id"), " - ", rank_asc, " + 1)")
  ranks <- paste0(", ", .sqlIntegerCast(h, rank_asc), " AS rank_asc, ", .sqlIntegerCast(h, rank_last),
                  " AS rank_last")
  if (needs_cov) {
    cov <- .compileCoverage(ctx, name, policy, f_raw, "candidate")
    .coverageAssertUnambiguous(ctx, cov, name)
    over_c <- list(op_start = "o.op_start", op_end = "o.op_end", op_component_id = "o.component_id")
    cand_body <- paste0("SELECT r.cand_key AS cand_key, ", paste(.populationSelect("r", over_c), collapse = ", "),
                        ranks, " FROM ", f_raw$name, " r INNER JOIN ", cov$coverage$name,
                        " o ON o.row_key = r.cand_key")
    deps <- c(f_raw$name, cov$coverage$name)
  } else {
    cand_body <- paste0("SELECT r.cand_key AS cand_key, ", paste(.populationSelect("r"), collapse = ", "),
                        ranks, " FROM ", f_raw$name, " r")
    deps <- f_raw$name
  }
  col <- .fragmentColumn
  f_cand <- .populationCandidateFragment(ctx, name, NULL, cand_body, deps, indexed = TRUE,
                                         extra = list(rank_asc = col("integer", FALSE, "private"),
                                                      rank_last = col("integer", FALSE, "private")))
  f_crit <- .populationCriteriaFragment(ctx, name, node[["where"]], f_cand, f_raw, policy,
                                        carry = c("rank_asc", "rank_last"))
  having <- switch(episode_policy %||% "any_episode",
    any_episode = "MAX(x.crit_true) = 1",
    all_episodes = "MIN(x.crit_true) = 1",
    first_episode = "MAX(CASE WHEN x.rank_asc = 1 THEN x.crit_true ELSE 0 END) = 1",
    last_episode = "MAX(CASE WHEN x.rank_last = 1 THEN x.crit_true ELSE 0 END) = 1",
    .irReject("episode_policy is any_episode, all_episodes, first_episode or last_episode",
              field = paste0(base, "/episode_policy")))
  over_p <- .populationNulls(h, setdiff(.POP_ROSTER_COLUMNS, "subject_id"))
  sel_body <- paste0("SELECT ", paste(.populationSelect("g", over_p), collapse = ", "),
                     " FROM (SELECT x.subject_id AS subject_id FROM ", f_crit$name,
                     " x GROUP BY x.subject_id HAVING ", having, ") g")
  f_sel <- .populationSelectionFragment(ctx, name, sel_body, f_crit$name, indexed = FALSE)
  roster <- .populationRosterFragment(ctx, name, .rankedRosterSql(h, f_sel$name), f_sel$name, indexed = FALSE)
  list(candidates = f_cand, roster = roster, coverage = NULL, segments = NULL)
}

# Do criteria need the population's coverage (an observed leaf without gap)?
.populationCriteriaNeedObservation <- function(where) {
  found <- FALSE
  .irWalkCriteria(where, function(k, leaf, path) {
    if (identical(k, "observed") && is.null(unlist(leaf[["gap"]]))) found <<- TRUE
  }, "")
  found || length(.irCollectAnchors(where)) > 0L ||
    grepl("observation_(start|end)", .jcsCanonical(where %||% list()))
}

# --- set operations ----------------------------------------------------------------------

.compileSetopPopulation <- function(ctx, name, node, deps) {
  h <- .compileDialect(ctx)
  base <- paste0("/populations/", name)
  op <- node[["setop"]][["op"]]
  members <- deps$members
  indexed <- .populationIndexed(ctx[["ir"]], name)
  rosters <- lapply(members, function(m) m$roster)
  parent <- deps$parent
  parent_cond <- function(alias) {
    if (is.null(parent)) return(character(0))
    paste0("EXISTS (SELECT 1 FROM ", parent$roster$name, " pp WHERE pp.subject_id = ", alias, ".subject_id)")
  }
  if (identical(op, "union")) {
    all_body <- paste(vapply(rosters, function(r) {
      paste0("SELECT ", paste(.populationSelect("m"), collapse = ", "), " FROM ", r$name, " m")
    }, ""), collapse = " UNION ALL ")
    f_all <- .fragment(.fragmentName("pop", name, "all", ctx = ctx), "candidates", "episode", all_body,
                       columns = .populationColumnSpec(indexed),
                       dependencies = vapply(rosters, function(r) r$name, ""),
                       person_lineage = list(person_col = "subject_id", via_roster = NULL),
                       private_columns = .POP_PRIVATE_COLUMNS)
    attr(f_all, "population") <- name
    .sqlAssertTypedSql(h, f_all$select_body)
    .fragmentRegister(ctx, f_all)
    .populationAssertNoIdentityConflict(ctx, f_all, name)
    ident <- c("subject_id", "cohort_start_date", "cohort_end_date", .POP_KEY_COLUMNS)
    meta_cols <- setdiff(.POP_ROSTER_COLUMNS, ident)
    sel_cols <- c(paste0("u.", ident, " AS ", ident), paste0("MIN(u.", meta_cols, ") AS ", meta_cols))
    sel_cols <- sel_cols[match(.POP_ROSTER_COLUMNS, c(ident, meta_cols))]
    conds <- parent_cond("g")
    sel_body <- paste0("SELECT ", paste(.populationSelect("g"), collapse = ", "), " FROM (SELECT ",
                       paste(sel_cols, collapse = ", "), " FROM ", f_all$name, " u GROUP BY ",
                       paste(paste0("u.", ident), collapse = ", "), ") g",
                       if (length(conds)) paste0(" WHERE ", conds))
    sel_deps <- c(f_all$name, if (!is.null(parent)) parent$roster$name)
  } else {
    left <- rosters[[1L]]
    others <- rosters[-1L]
    conds <- vapply(others, function(r) {
      paste0(if (identical(op, "setdiff")) "NOT " else "", "EXISTS (SELECT 1 FROM ", r$name,
             " q WHERE q.subject_id = l.subject_id)")
    }, "")
    conds <- c(conds, parent_cond("l"))
    sel_body <- paste0("SELECT ", paste(.populationSelect("l"), collapse = ", "), " FROM ", left$name, " l",
                       if (length(conds)) paste0(" WHERE ", paste(conds, collapse = " AND ")))
    sel_deps <- c(vapply(rosters, function(r) r$name, ""), if (!is.null(parent)) parent$roster$name)
  }
  f_sel <- .populationSelectionFragment(ctx, name, sel_body, sel_deps, indexed = indexed)
  roster <- .populationRosterFragment(ctx, name, .rankedRosterSql(h, f_sel$name), f_sel$name,
                                      indexed = indexed)
  covs <- Filter(Negate(is.null), lapply(members, function(m) m$coverage))
  cov <- if (length(covs)) .populationDerivedCoverage(ctx, name, covs, roster) else NULL
  list(candidates = NULL, roster = roster, coverage = cov, segments = NULL)
}

#' Refuse a union whose members carry one origin identity with contradictory
#' metadata (design section 6.9, \code{setop_identity_conflict})
#'
#' A private, data-dependent validation error transported as
#' \code{validation_error} (never "keep the left-most"). The count never
#' leaves the server.
#' @keywords internal
.populationAssertNoIdentityConflict <- function(ctx, f_all, name) {
  ident <- c("subject_id", "cohort_start_date", "cohort_end_date", .POP_KEY_COLUMNS)
  meta_cols <- c("index_source", "index_key", "index_visit_id", "set_id", "is_case",
                 "sampling_weight", "op_start", "op_end", "op_component_id")
  differs <- vapply(meta_cols, function(m) {
    paste0("(MIN(u.", m, ") <> MAX(u.", m, ") OR (COUNT(u.", m, ") > 0 AND COUNT(u.", m,
           ") < COUNT(*)))")
  }, "")
  select <- paste0("SELECT COUNT(*) AS n FROM (SELECT 1 AS one FROM ", f_all$name, " u GROUP BY ",
                   paste(paste0("u.", ident), collapse = ", "), " HAVING ",
                   paste(differs, collapse = " OR "), ") k")
  n <- .adapterCount(ctx, f_all, select)
  if (n > 0) {
    .compileReject(paste0("population '", name, "': two members carry one origin identity with ",
                          "contradictory metadata"),
                   reason = "setop_identity_conflict", field = paste0("/populations/", name, "/setop"))
  }
  invisible(TRUE)
}

#' The coverage object of a derived roster (set operation, match)
#'
#' The members share one coverage policy (validated), so their component ids
#' are comparable: the derived object's recorded relation and segments are the
#' distinct union of the members' own (each compiled once on its candidates,
#' A-20 c), and its \code{coverage} fragment reads the roster's transferred
#' \code{op_start}, \code{op_end} and \code{op_component_id}. Consumers read
#' observed segments through the roster's \code{op_component_id}
#' (\code{.compileObservedSegments()}); no coverage is compiled again.
#' @keywords internal
.populationDerivedCoverage <- function(ctx, name, covs, roster) {
  h <- .compileDialect(ctx)
  policies <- unique(lapply(covs, function(c) c$policy))
  if (length(policies) != 1L) {
    .irReject("members of a derived population must share one coverage policy",
              field = paste0("/populations/", name))
  }
  person_type <- covs[[1L]]$recorded$columns$person_id$type
  register <- function(nm, body, cols, deps, key) {
    f <- .fragment(nm, "coverage", "row", body, columns = cols, dependencies = deps, unique_key = key,
                   person_lineage = list(person_col = "person_id", via_roster = NULL),
                   private_columns = intersect(c("period_id", "component_id"), names(cols)),
                   ordering = intersect(c("start_date", "period_id", "component_id"), names(cols)))
    .sqlAssertTypedSql(h, f$select_body)
    .fragmentRegister(ctx, f)
  }
  col <- .fragmentColumn
  rec_cols <- list(person_id = col(person_type, FALSE, "person_key"),
                   period_id = col("bigint", FALSE, "origin_identity"),
                   component_id = col("bigint", FALSE, "group"),
                   start_date = col("date", FALSE, "start"), end_date = col("date", FALSE, "end"))
  rec_body <- paste0("SELECT DISTINCT r.person_id AS person_id, r.period_id AS period_id, ",
                     "r.component_id AS component_id, r.start_date AS start_date, r.end_date AS end_date ",
                     "FROM (", paste(vapply(covs, function(c) {
                       paste0("SELECT x.person_id, x.period_id, x.component_id, x.start_date, x.end_date FROM ",
                              c$recorded$name, " x")
                     }, ""), collapse = " UNION ALL "), ") r")
  f_rec <- register(.fragmentName("seg", name, "rec", ctx = ctx), rec_body, rec_cols,
                    vapply(covs, function(c) c$recorded$name, ""), c("person_id", "period_id"))
  seg_cols <- rec_cols[c("person_id", "component_id", "start_date", "end_date")]
  seg_body <- paste0("SELECT DISTINCT s.person_id AS person_id, s.component_id AS component_id, ",
                     "s.start_date AS start_date, s.end_date AS end_date FROM (",
                     paste(vapply(covs, function(c) {
                       paste0("SELECT x.person_id, x.component_id, x.start_date, x.end_date FROM ",
                              c$segments$name, " x")
                     }, ""), collapse = " UNION ALL "), ") s")
  f_seg <- register(.fragmentName("seg", name, ctx = ctx), seg_body, seg_cols,
                    vapply(covs, function(c) c$segments$name, ""), c("person_id", "component_id"))
  opc_body <- paste0("SELECT r.cohort_row_id AS row_key, r.subject_id AS person_id, ",
                     "CASE WHEN r.op_component_id IS NULL THEN 0 ELSE 1 END AS op_count, ",
                     "r.op_start AS op_start, r.op_end AS op_end, r.op_component_id AS component_id FROM ",
                     roster$name, " r")
  f_opc <- .fragment(.fragmentName("opc", name, ctx = ctx), "coverage", "episode", opc_body,
                     columns = list(row_key = col("bigint", FALSE, "episode_key"),
                                    person_id = col("bigint", FALSE, "person_key"),
                                    op_count = col("integer", FALSE, "private"),
                                    op_start = col("date", TRUE, "start"),
                                    op_end = col("date", TRUE, "end"),
                                    component_id = col("bigint", TRUE, "group")),
                     dependencies = roster$name, unique_key = "row_key",
                     person_lineage = list(person_col = "person_id", via_roster = NULL),
                     roster_namespace = name,
                     private_columns = c("row_key", "op_count", "component_id"))
  .sqlAssertTypedSql(h, f_opc$select_body)
  .fragmentRegister(ctx, f_opc)
  list(coverage = f_opc, segments = f_seg, recorded = f_rec, policy = policies[[1L]], pop = name,
       suffix = NULL)
}

# --- the population stage of a plan --------------------------------------------------

#' Compile the populations of a set of outputs (the plan's population stage)
#'
#' Design section 6.1: after events and intervals, the populations the
#' requested outputs read (their rows, with parents, set-op members and match
#' arms) compile in topological order inside the private compilation context.
#' Data-dependent refusals of this stage (\code{coverage_ambiguous} under
#' \code{on_ambiguous = "error"}, \code{setop_identity_conflict},
#' \code{match_candidates_exceeded}) are raised here, before any output is
#' materialised.
#'
#' @param cctx Compilation context.
#' @param outputs Names of the outputs to compute.
#' @return The names of the compiled populations, invisibly.
#' @keywords internal
.planCompilePopulations <- function(cctx, outputs) {
  ir <- cctx[["ir"]]
  outputs <- intersect(as.character(outputs), names(ir[["outputs"]] %||% list()))
  pops <- unique(unlist(lapply(outputs, function(o) {
    names(.irOutputClosure(ir, o)[["populations"]] %||% list())
  }), use.names = FALSE))
  # a matching template is instantiated by its match, never compiled as a roster
  templates <- .populationUses(ir)$templates
  pops <- intersect(.irOrder(ir)[["populations"]], setdiff(pops, setdiff(templates, .populationUses(ir)$ordinary)))
  for (p in pops) .compilePopulationByName(cctx, p)
  invisible(pops)
}

# --- semantic rules of populations (section 4.1, rules a schema cannot express) ---

# Does a window depend on a roster's index (an anchor other than a fixed date)?
.populationWindowUsesIndex <- function(window) {
  if (!is.list(window) || !length(window)) return(FALSE)
  bound_uses <- function(b, default) {
    if (is.null(b)) return(FALSE)
    if (is.numeric(b) || (is.list(b) && length(b) == 1L && is.numeric(b[[1L]]))) {
      return(.populationAnchorUsesIndex(default))
    }
    if (is.list(b) && !is.null(b[["anchor"]])) return(.populationAnchorUsesIndex(b[["anchor"]]))
    FALSE
  }
  default <- window[["anchor"]] %||% "index"
  bound_uses(window[["start"]], default) || bound_uses(window[["end"]], default)
}

.populationAnchorUsesIndex <- function(anchor) {
  if (is.null(anchor)) return(TRUE)
  if (is.list(anchor) && length(anchor) == 1L && is.null(names(anchor))) anchor <- anchor[[1L]]
  if (is.character(anchor)) return(TRUE)
  if (is.list(anchor) && !is.null(anchor[["date"]])) return(FALSE)
  if (is.list(anchor) && !is.null(anchor[["source"]])) {
    scope <- unlist(anchor[["scope"]] %||% "person")
    return(identical(scope, "episode") || .populationWindowUsesIndex(anchor[["search"]]))
  }
  TRUE
}

# Do criteria depend on the roster row's index?
.populationCriteriaUseIndex <- function(where) {
  uses <- FALSE
  .irWalkCriteria(where, function(k, leaf, path) {
    if (identical(k, "observed")) uses <<- TRUE
    if (identical(k, "age") && identical(unlist(leaf[["reference"]] %||% "index"), "index")) uses <<- TRUE
    if (identical(k, "has") && .populationWindowUsesIndex(leaf[["window"]])) uses <<- TRUE
  }, "")
  uses
}

#' Semantic rules of language 2.0 populations (design sections 2.5, 6.9,
#' 6.10; change request CR-21)
#'
#' Rules the schema cannot express, checked before any query: a matching
#' controls template is instantiated once per plan (A-20 c), so it cannot also
#' be an ordinary roster (output rows, parent, set-op member, match cases) or
#' the template of a second match; the cases and the template of a match share
#' one coverage policy (one observation semantics for the matched roster, as
#' set-op members do); a strata window anchors at the case time (built-in or
#' date anchors, never a source anchor); \code{episode_policy} applies to a
#' criteria population seeded from a results cohort without a parent, and is
#' required there when its criteria depend on the episode's index.
#'
#' @param ir The normalised IR (language 2.0).
#' @return Character vector of findings ("<path>: <reason>").
#' @keywords internal
.irPopulationSemanticErrors <- function(ir) {
  errors <- character(0)
  add <- function(...) errors <<- c(errors, paste0(...))
  pops <- ir[["populations"]] %||% list()
  uses <- .populationUses(ir)
  templates <- uses$templates
  for (m in names(templates)) {
    t <- templates[[m]]
    base <- paste0("/populations/", m, "/match")
    if (t %in% uses$ordinary) {
      add(base, "/controls: the controls template '", t, "' is instantiated at each case time and ",
          "cannot also be used as an ordinary population (output rows, parent, set-op member or cases)")
    }
    if (sum(templates == t) > 1L) {
      add(base, "/controls: the controls template '", t, "' is the template of more than one match")
    }
    cases_policy <- tryCatch(.populationCoveragePolicy(ir, pops[[m]][["match"]][["cases"]]),
                             error = function(e) NULL)
    tpl_policy <- tryCatch(.populationCoveragePolicy(ir, t), error = function(e) NULL)
    if (!is.null(cases_policy) && !is.null(tpl_policy) && !identical(cases_policy, tpl_policy)) {
      add(base, ": the cases and the controls template must share one coverage policy")
    }
    items <- .irItems(pops[[m]][["match"]][["match"]])
    for (i in seq_along(items)) {
      s <- items[[i]]
      if (is.list(s) && !is.null(s[["strata"]]) &&
          length(.irCollectAnchors(list(window = s[["strata"]][["window"]])))) {
        add(base, "/match/", i - 1L, "/strata/window: a strata window anchors at the case time ",
            "(built-in or date anchors); source anchors are not supported")
      }
    }
  }
  for (p in names(pops)) {
    node <- pops[[p]]
    if (!identical(node[["kind"]], "criteria")) next
    policy <- node[["episode_policy"]]
    if (is.list(policy)) policy <- unlist(policy)
    if (!is.null(policy) && is.character(node[["parent"]])) {
      add("/populations/", p, "/episode_policy: episode_policy applies to a population seeded from a ",
          "results cohort; a child inherits its parent's episodes")
    } else if (!is.null(policy) && p %in% templates) {
      add("/populations/", p, "/episode_policy: a matching controls template is evaluated at each ",
          "case time; episode_policy does not apply")
    } else if (!is.null(policy) && is.null(node[["cohort_definition_id"]])) {
      add("/populations/", p, "/episode_policy: episode_policy needs a results-cohort seed (cohort_definition_id)")
    }
    if (is.null(policy) && !is.null(node[["cohort_definition_id"]]) && !is.character(node[["parent"]]) &&
        .populationCriteriaUseIndex(node[["where"]]) && !p %in% templates) {
      add("/populations/", p, "/episode_policy: criteria that depend on the episode's index over a ",
          "results-cohort seed need an explicit episode_policy (any_episode, all_episodes, ",
          "first_episode or last_episode)")
    }
  }
  errors
}
