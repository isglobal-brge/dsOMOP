# Module: IR semantic and handle-dependent validation (design-2.10.3 sections
# 4.1 "rules the schema cannot express", 6.1 and 9).
#
# .irValidate(ir, handle) runs after ir-v2.json (step 4 of the frozen
# sequence). The semantic rules are pure and run in the order of section 4.1;
# the handle-dependent checks need the site: tables and columns through the
# role registry, filterable and typed predicate columns, column classes,
# deployment-disabled operations and the dialect preflight.
#
# A lowered v1 plan passes every stage too (no stage is exempted): its legacy
# semantics are carried by the compatibility variants of amendment A-10, the
# site checks and the dialect preflight are enforced with the requirements of
# the v1 executors that still run it in this release stage (session tables
# for its cohorts and populations), and .irPrepare() adds the v1-specific
# site requirements those executors check (R/ir_v1.R). Only the language 2.0
# population rules (grain/collapse, index-relative age) do not apply to the
# legacy populations, whose episodes legacy_episodes defines.

# --- small helpers ------------------------------------------------------------

.irIsName <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl("^[A-Za-z][A-Za-z0-9_]{0,63}$", x)
}

.irItems <- function(x) if (is.null(x)) list() else .jsonArrayItems(x)

# A-25 ownership is a registry-only preflight, before even site metadata is
# built. A named input is borrowed, never registered as execution-owned, and
# membership says nothing about its support or authenticated DP lineage.
.irCheckOwnedCohort <- function(handle, name, path) {
  if (!is.null(name) && !name %in% (handle$temp_tables %||% character(0))) {
    .irReject("named cohort reference must belong to this handle", field = path,
              errors = paste0(path, ": named cohort reference must belong to this handle"))
  }
  invisible(TRUE)
}

# Does a node contain a negation (an object whose only key is `not`)?
.irHasNot <- function(x) {
  if (!is.list(x)) return(FALSE)
  if (identical(names(x), "not")) return(TRUE)
  any(vapply(x, .irHasNot, logical(1)))
}

# The assignment age-grid shape (design 4.5 b: at least two strictly
# increasing integers starting at 0), decided by the one rule the executors
# and .planValidate() use, .bandAges() in R/assignment_policy.R.
.irAgeGridValid <- function(breaks) {
  if (is.null(breaks)) return(FALSE)
  tryCatch({
    .bandAges(20L, age_breaks = breaks)
    TRUE
  }, error = function(e) FALSE)
}

# Index-source tags a population's episodes carry (section 6.9; A-1, A-7).
.irIndexSourceTags <- function(ir, pid, seen = character(0)) {
  if (pid %in% seen) return(character(0))
  pop <- ir[["populations"]][[pid]]
  if (!is.list(pop)) return(character(0))
  # The criteria-base parent is a membership seed, not inherited identity:
  # observation-period episodes carry none of its event/source tags.
  if (identical(pop[["kind"]], "criteria") &&
      identical(pop[["legacy_episodes"]], "observation_periods")) return(character(0))
  seen <- c(seen, pid)
  switch(pop[["kind"]] %||% "",
    index = {
      src <- pop[["index"]][["source"]]
      if (!is.null(src[["event"]])) src[["event"]]
      else if (!is.null(src[["interval"]])) src[["interval"]]
      else if (!is.null(src[["cohort_definition_id"]])) {
        paste0("cohort:", format(src[["cohort_definition_id"]], scientific = FALSE))
      } else if (!is.null(src[["any_of"]])) {
        vapply(.irItems(src[["any_of"]]), function(r) r[["event"]] %||% r[["interval"]] %||% "", "")
      } else if (!is.null(src[["legacy_spec"]])) "legacy_spec"
      else character(0)
    },
    match = paste0("match:", pid),
    setop = {
      members <- .irItems(pop[["setop"]][["members"]])
      unique(unlist(lapply(members, function(m) .irIndexSourceTags(ir, m, seen))))
    },
    criteria = if (is.character(pop[["parent"]])) .irIndexSourceTags(ir, pop[["parent"]], seen) else character(0),
    character(0)
  )
}

# Walk a criteria tree, calling f(leaf_kind, leaf, path) on every leaf.
.irWalkCriteria <- function(node, f, path) {
  if (!is.list(node) || is.null(names(node))) return(invisible(NULL))
  key <- names(node)[[1L]]
  if (key %in% c("and", "or")) {
    items <- .irItems(node[[key]])
    for (i in seq_along(items)) .irWalkCriteria(items[[i]], f, paste0(path, "/", key, "/", i - 1L))
  } else if (identical(key, "not")) {
    .irWalkCriteria(node[["not"]], f, paste0(path, "/not"))
  } else {
    f(key, node[[key]], paste0(path, "/", key))
  }
  invisible(NULL)
}

# Walk a predicate tree, calling f(leaf, path) on every leaf.
.irWalkPredicate <- function(node, f, path) {
  if (!is.list(node) || is.null(names(node))) return(invisible(NULL))
  if ("and" %in% names(node) || "or" %in% names(node)) {
    key <- intersect(names(node), c("and", "or"))[[1L]]
    items <- .irItems(node[[key]])
    for (i in seq_along(items)) .irWalkPredicate(items[[i]], f, paste0(path, "/", key, "/", i - 1L))
  } else if ("not" %in% names(node)) {
    .irWalkPredicate(node[["not"]], f, paste0(path, "/not"))
  } else {
    f(node, path)
  }
  invisible(NULL)
}

.irIsDateVar <- function(var) {
  grepl("_date$|_datetime$", var) || var %in% c("start_date", "end_date")
}

.irIsoDateOk <- function(x) {
  is.character(x) && length(x) == 1L && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x) &&
    !is.na(suppressWarnings(as.Date(x, "%Y-%m-%d"))) &&
    identical(format(as.Date(x, "%Y-%m-%d"), "%Y-%m-%d"), x)
}

# Typing of one predicate leaf against its column name (dates take ISO
# strings, concept columns integers) and ordered `between` bounds. The v1
# envelope already runs the historical predicate validator; its admitted
# literals retain the actual site's legacy SQL comparison semantics (A-19
# item 7), including the database's own coercions and refusals.
.irCheckPredicateLeaf <- function(leaf, path, add, legacy = FALSE) {
  if (legacy) return(invisible(NULL))
  var <- leaf[["var"]]
  op <- leaf[["op"]]
  values <- switch(op,
    `in` =, not_in = .irItems(leaf[["value"]]),
    between = list(leaf[["value"]][["lower"]], leaf[["value"]][["upper"]]),
    is_null =, not_null = list(),
    list(leaf[["value"]]))
  if (.irIsDateVar(var)) {
    for (v in values) {
      if (!.irIsoDateOk(v)) add(paste0(path, ": date column '", var,
                                       "' compares with ISO dates only"))
    }
  } else if (grepl("_concept_id$", var)) {
    for (v in values) {
      if (!is.numeric(v) || v != round(v)) {
        add(paste0(path, ": concept column '", var, "' compares with integer concept ids only"))
      }
    }
  }
  if (identical(op, "between")) {
    lo <- leaf[["value"]][["lower"]]; hi <- leaf[["value"]][["upper"]]
    comparable <- (is.numeric(lo) && is.numeric(hi)) ||
      (is.character(lo) && is.character(hi))
    if (!comparable) {
      add(paste0(path, ": between bounds must have one type"))
    } else if (lo > hi) {
      add(paste0(path, ": between bounds must be ordered (lower <= upper)"))
    }
  }
}

# Size of one criteria or predicate tree for the server caps: nodes, depth and
# literal values, descending through and/or/not groups, the predicate embedded
# in a criteria `person` leaf, and counting a legacy leaf's parameter values
# as the v1 filter-complexity cap did (.filterComplexityVisit).
.irTreeStats <- function(node, depth = 1L, stats = NULL) {
  if (is.null(stats)) {
    stats <- new.env(parent = emptyenv())
    stats[["nodes"]] <- 0L; stats[["depth"]] <- 0L; stats[["values"]] <- 0L
  }
  if (!is.list(node) || is.null(names(node))) return(stats)
  stats[["nodes"]] <- stats[["nodes"]] + 1L
  stats[["depth"]] <- max(stats[["depth"]], depth)
  nm <- names(node)
  group <- intersect(nm, c("and", "or"))
  if (length(group)) {
    for (child in .irItems(node[[group[[1L]]]])) .irTreeStats(child, depth + 1L, stats)
  } else if (identical(nm, "not")) {
    .irTreeStats(node[["not"]], depth + 1L, stats)
  } else if (identical(nm, "person")) {
    .irTreeStats(node[["person"]], depth + 1L, stats)
  } else if (identical(nm, "legacy")) {
    stats[["values"]] <- stats[["values"]] +
      length(unlist(node[["legacy"]][["params"]], use.names = FALSE))
  } else if ("value" %in% nm) {
    v <- node[["value"]]
    n <- if (identical(node[["op"]], "between")) 2L else length(unlist(v, use.names = FALSE))
    stats[["values"]] <- stats[["values"]] + n
  }
  stats
}

#' Every criteria and predicate tree of an IR, namespace-qualified
#'
#' Positional over each node map, so a population and an event of the same
#' name are distinct positions. Trees: population criteria (with their
#' embedded person predicates), event predicates, adapter-local predicates
#' (episode members, cost), reducer predicates and a lowered v1 output's
#' \code{legacy_raw_filter} (amendment A-16).
#'
#' @param ir The IR.
#' @return list of list(path, tree, kind = "criteria" | "predicate", context),
#'   context = list(kind = "event" | "episode_members" | "cost", event) for an
#'   event's predicates, list(kind = "source", source) for a reducer
#'   predicate, list(kind = "population", population) for criteria,
#'   list(kind = "legacy_raw", output) for a raw-table filter.
#' @keywords internal
.irPredicateTrees <- function(ir) {
  out <- list()
  push <- function(path, tree, kind, context) {
    if (!is.null(tree)) out[[length(out) + 1L]] <<- list(path = path, tree = tree, kind = kind, context = context)
  }
  ev <- ir[["events"]] %||% list()
  for (i in seq_along(ev)) {
    e <- names(ev)[[i]]
    node <- ev[[i]]
    base <- paste0("/events/", e)
    push(paste0(base, "/where"), node[["where"]], "predicate", list(kind = "event", event = e))
    if (is.list(node[["episode_members"]])) {
      push(paste0(base, "/episode_members/where"), node[["episode_members"]][["where"]], "predicate",
           list(kind = "episode_members", event = e))
    }
    if (is.list(node[["cost"]])) {
      push(paste0(base, "/cost/where"), node[["cost"]][["where"]], "predicate", list(kind = "cost", event = e))
    }
  }
  pops <- ir[["populations"]] %||% list()
  for (i in seq_along(pops)) {
    push(paste0("/populations/", names(pops)[[i]], "/where"), pops[[i]][["where"]], "criteria",
         list(kind = "population", population = names(pops)[[i]]))
  }
  # A-16: the v1 person_level custom filter over the raw person/death tables
  # (typed by the v1 executors' own checks, .planV1SiteCheck())
  outs <- ir[["outputs"]] %||% list()
  for (i in seq_along(outs)) {
    push(paste0("/outputs/", names(outs)[[i]], "/legacy_raw_filter"), outs[[i]][["legacy_raw_filter"]],
         "predicate", list(kind = "legacy_raw", output = names(outs)[[i]]))
  }
  vars <- ir[["variables"]] %||% list()
  for (i in seq_along(vars)) {
    r <- vars[[i]][["reducer"]]
    if (is.list(r)) {
      push(paste0("/variables/", names(vars)[[i]], "/reducer/predicate"), r[["predicate"]], "predicate",
           list(kind = "source", source = vars[[i]][["source"]]))
    }
  }
  out
}

# Every predicate (with its own path and context) inside the trees, the
# person predicates embedded in criteria included.
.irPredicates <- function(ir) {
  out <- list()
  for (t in .irPredicateTrees(ir)) {
    if (identical(t[["kind"]], "predicate")) {
      out[[length(out) + 1L]] <- t
    } else {
      .irWalkCriteria(t[["tree"]], function(kind, leaf, path) {
        if (identical(kind, "person")) {
          out[[length(out) + 1L]] <<- list(path = paste0(t[["path"]], path), tree = leaf,
                                           kind = "predicate", context = list(kind = "person"))
        }
      }, "")
    }
  }
  out
}

# --- semantic rules (pure) ----------------------------------------------------

#' Semantic rules of section 4.1, in order
#'
#' @param ir The IR (schema-valid).
#' @return Character vector of findings ("<path>: <reason>"), empty when valid.
#' @keywords internal
.irSemanticErrors <- function(ir) {
  errors <- character(0)
  add <- function(...) errors <<- c(errors, paste0(...))
  cs <- ir[["concept_sets"]] %||% list(); ev <- ir[["events"]] %||% list()
  iv <- ir[["intervals"]] %||% list(); pops <- ir[["populations"]] %||% list()
  vars <- ir[["variables"]] %||% list(); outs <- ir[["outputs"]] %||% list()

  # 1. name resolution and acyclicity ----------------------------------------
  for (e in names(ev)) {
    ref <- ev[[e]][["concept_set"]]
    if (is.character(ref) && !ref %in% names(cs)) {
      add("/events/", e, "/concept_set: dangling concept set '", ref, "'")
    }
    # A-10: the v1 output-level concept set that also restricts the event
    scope_ref <- ev[[e]][["legacy_concept_scope"]][["concept_set_ref"]]
    if (is.character(scope_ref) && !scope_ref %in% names(cs)) {
      add("/events/", e, "/legacy_concept_scope/concept_set_ref: dangling concept set '", scope_ref, "'")
    }
  }
  refs <- .irCollectRefs(ir[c("events", "intervals", "populations", "variables", "outputs", "options")])
  for (r in refs) {
    pool <- if (identical(r[["kind"]], "event")) names(ev) else names(iv)
    if (!r[["name"]] %in% pool) add(r[["path"]], ": dangling ", r[["kind"]], " reference '", r[["name"]], "'")
  }
  for (p in names(pops)) {
    pop <- pops[[p]]
    if (is.character(pop[["parent"]]) && !pop[["parent"]] %in% names(pops)) {
      add("/populations/", p, "/parent: dangling population '", pop[["parent"]], "'")
    }
    if (identical(pop[["parent"]], p)) add("/populations/", p, "/parent: a population cannot be its own parent")
    for (m in .irItems(pop[["setop"]][["members"]])) {
      if (!m %in% names(pops)) add("/populations/", p, "/setop/members: dangling population '", m, "'")
    }
    if (is.list(pop[["match"]])) {
      for (k in c("cases", "controls")) {
        if (!pop[["match"]][[k]] %in% names(pops)) {
          add("/populations/", p, "/match/", k, ": dangling population '", pop[["match"]][[k]], "'")
        }
      }
    }
  }
  for (o in names(outs)) {
    out <- outs[[o]]
    if (!out[["rows"]] %in% names(pops)) add("/outputs/", o, "/rows: dangling population '", out[["rows"]], "'")
    for (v in unlist(out[["columns"]] %||% list(), use.names = FALSE)) {
      if (!length(out$number_columns) && !length(out$category_columns) && !length(.pnv2ColumnVariables(v, vars))) add("/outputs/", o, "/columns: dangling variable '", v, "'")
    }
    opts <- out[["options"]] %||% list()
    if (identical(out[["format"]], "joined")) {
      for (f in .irItems(opts[["from"]])) {
        if (!f[["output"]] %in% names(outs)) add("/outputs/", o, "/options/from: dangling output '", f[["output"]], "'")
        if (identical(f[["output"]], o)) add("/outputs/", o, "/options/from: an output cannot join itself")
      }
    }
    if (identical(out[["format"]], "dictionary")) {
      for (s in .irItems(opts[["source_outputs"]])) {
        if (!s %in% names(outs)) add("/outputs/", o, "/options/source_outputs: dangling output '", s, "'")
      }
    }
  }
  for (pub in .irItems(ir[["publish"]])) {
    if (!pub %in% names(outs)) add("/publish: dangling output '", pub, "'")
  }
  if (length(errors)) return(errors)
  cyc <- c(.irTopoSort(names(ev), function(e) .irEventDeps(ir, e))[["cycle"]],
           .irTopoSort(names(pops), function(p) .irPopulationDeps(ir, p))[["cycle"]],
           .irTopoSort(names(outs), function(o) .irOutputDeps(ir, o))[["cycle"]])
  if (length(cyc)) return(paste0("/: reference cycle ", paste(cyc, collapse = " -> ")))

  # 2. grain and collapse (language 2.0 population semantics; a lowered v1
  # plan keeps the v1 episode materialisation of its legacy populations, so
  # these two rules do not apply to it) ---------------------------------------
  legacy <- identical(attr(ir, "source_version"), 1L)
  for (o in if (legacy) character(0) else names(outs)) {
    out <- outs[[o]]
    indexed <- .pnv2PopulationIndexed(pops, out[["rows"]])
    if (identical(out[["grain"]], "person") && indexed && is.null(out[["collapse"]]) &&
        !out[["format"]] %in% c("membership", "dictionary", "joined")) {
      add("/outputs/", o, "/collapse: a person-grain output over the indexed population '",
          out[["rows"]], "' requires an explicit collapse policy")
    }
    if (identical(out[["grain"]], "episode") && !indexed) {
      add("/outputs/", o, "/grain: episode grain needs an indexed population")
    }
  }

  # Populations a consumer evaluates as ordinary rosters (the rows of an
  # output, a parent, a set-op member, match cases), and those used only as a
  # matching controls template, which section 6.10 compiles per case with
  # index := case time (so index-relative criteria have an index there).
  ordinary <- unique(c(
    vapply(outs, function(o) as.character(o[["rows"]] %||% ""), ""),
    unlist(lapply(pops, function(q) {
      c(if (is.character(q[["parent"]])) q[["parent"]],
        as.character(unlist(q[["setop"]][["members"]] %||% list(), use.names = FALSE)),
        if (is.list(q[["match"]])) as.character(q[["match"]][["cases"]]))
    }), use.names = FALSE)))
  controls_only <- setdiff(unlist(lapply(pops, function(q) {
    if (is.list(q[["match"]])) as.character(q[["match"]][["controls"]])
  }), use.names = FALSE), ordinary)

  # 3. shape rules ---------------------------------------------------------------
  for (p in names(pops)) {
    pop <- pops[[p]]
    base <- paste0("/populations/", p)
    if (identical(pop[["kind"]], "index")) {
      idx <- pop[["index"]]
      if (!identical(idx[["select"]], "nth") && !identical(as.integer(idx[["n"]]), 1L)) {
        add(base, "/index/n: n is used only with select = 'nth'")
      }
      if (identical(idx[["end"]][["strategy"]], "fixed") && is.null(idx[["end"]][["days"]])) {
        add(base, "/index/end/days: a fixed end strategy requires days")
      }
      members <- .irItems(idx[["source"]][["any_of"]])
      if (length(members)) {
        keys <- vapply(members, .jcsCanonical, "")
        if (anyDuplicated(keys)) add(base, "/index/source/any_of: members must be distinct")
      }
      if (isFALSE(idx[["restrict_to_observation"]]) &&
          !identical(pop[["coverage"]][["on_missing"]], "keep")) {
        add(base, "/coverage/on_missing: restrict_to_observation = false conflicts with coverage on_missing = 'drop'")
      }
    }
    if (identical(pop[["kind"]], "setop")) {
      members <- .irItems(pop[["setop"]][["members"]])
      if (identical(pop[["setop"]][["op"]], "setdiff") && length(members) != 2L) {
        add(base, "/setop/members: setdiff takes exactly two members")
      }
      # A-21: self-difference and repeated subtrahends are valid private
      # populations. Membership need not be distinct for any set operation.
      covs <- unique(vapply(members, function(m) {
        mp <- pops[[m]]
        if (is.list(mp[["coverage"]])) .jcsCanonical(mp[["coverage"]]) else ""
      }, ""))
      covs <- covs[nzchar(covs)]
      if (length(covs) > 1L) add(base, "/setop/members: set-op members must share one coverage policy")
    }
    if (identical(pop[["kind"]], "match")) {
      m <- pop[["match"]]
      if (!.pnv2PopulationIndexed(pops, m[["cases"]])) {
        add(base, "/match/cases: the cases population must be indexed")
      }
      if (!identical(pops[[m[["controls"]]]][["kind"]], "criteria")) {
        add(base, "/match/controls: the controls template must be a criteria population")
      }
    }
    # criteria rules: has(between) ordered; age needs an index
    .irWalkCriteria(pop[["where"]], function(kind, leaf, path) {
      if (identical(kind, "has") && identical(leaf[["op"]], "between")) {
        n <- unlist(leaf[["n"]])
        if (length(n) == 2L && n[[1L]] > n[[2L]]) add(base, "/where", path, "/n: between bounds must be ordered")
      }
      if (identical(kind, "age") && identical(leaf[["reference"]], "index") &&
          !legacy && !.pnv2PopulationIndexed(pops, p) && !p %in% controls_only) {
        add(base, "/where", path, ": an index-referenced age criterion needs an indexed population ",
            "(or a population used only as a matching controls template, evaluated at case time)")
      }
    }, "")
  }
  # language 2.0 population rules of WS-6 (sections 6.9/6.10, A-20): matching templates and
  # their coverage, strata windows, episode_policy
  if (!legacy) errors <- c(errors, .irPopulationSemanticErrors(ir))

  # 4. predicate typing at every predicate position (events, adapters,
  # criteria person leaves, reducers) by column name; the site checks type
  # them against the actual columns ---------------------------------------------
  for (pr in .irPredicates(ir)) {
    .irWalkPredicate(pr[["tree"]], function(l, lp) .irCheckPredicateLeaf(l, paste0(pr[["path"]], lp), add, legacy), "")
  }

  # 5. variables -----------------------------------------------------------------
  for (v in names(vars)) {
    node <- vars[[v]]
    base <- paste0("/variables/", v)
    r <- node[["reducer"]]
    if (is.list(r)) {
      if (!identical(r[["fn"]], "supply_days") && !identical(r[["fills"]], "start_in_window")) {
        add(base, "/reducer/fills: fills applies to supply_days only")
      }
      if (identical(r[["coverage"]], "effective_supply")) {
        src <- node[["source"]]
        stock <- !is.null(src[["interval"]]) && isTRUE(iv[[src[["interval"]]]][["stockpile"]])
        if (!stock) add(base, "/reducer/coverage: effective_supply needs a stockpiled interval source")
      }
      if (identical(r[["fn"]], "quantile") && is.null(r[["p"]])) add(base, "/reducer/p: quantile requires p")
      if (r[["fn"]] %in% c("days_since", "days_until") && is.null(r[["reference"]])) {
        add(base, "/reducer/reference: ", r[["fn"]], " requires a reference anchor")
      }
      if (r[["fn"]] %in% c("any", "all") && is.null(r[["predicate"]])) {
        add(base, "/reducer/predicate: ", r[["fn"]], " requires a predicate")
      }
    }
    d <- node[["derived"]]
    if (is.list(d) && identical(d[["kind"]], "age") && !is.null(d[["age_breaks"]]) &&
        !.irAgeGridValid(d[["age_breaks"]])) {
      add(base, "/derived/age_breaks: age_breaks must be at least two strictly increasing integers starting at 0")
    }
  }

  # 6. outputs -------------------------------------------------------------------
  declared_sources <- function(out) vapply(.irItems(out[["sources"]]), .jcsCanonical, "")
  for (o in names(outs)) {
    out <- outs[[o]]
    base <- paste0("/outputs/", o, "/options")
    opts <- out[["options"]] %||% list()
    if (identical(out[["format"]], "survival")) {
      has_tar <- !is.null(opts[["tar"]])
      has_tars <- length(opts[["tars"]] %||% list()) > 0L
      if (has_tar == has_tars) add(base, ": survival takes exactly one of tar or tars")
      fmt <- opts[["format"]]; ord <- opts[["event_order"]]; tie <- opts[["tie_policy"]]
      if (identical(fmt, "survival") && identical(ord, "all")) add(base, "/event_order: survival requires event_order first or last")
      if (identical(fmt, "competing_risk") && !identical(ord, "first")) add(base, "/event_order: competing_risk requires event_order first")
      if (fmt %in% c("recurrent_events", "counting_process") && identical(ord, "last")) add(base, "/event_order: recurrent/counting formats require event_order first or all")
      if (identical(fmt, "multi_state") && !identical(ord, "all")) add(base, "/event_order: multi_state requires event_order all")
      if (identical(fmt, "multi_state") && !identical(opts[["outcome_mode"]], "named")) add(base, "/outcome_mode: multi_state requires outcome_mode named")
      if (identical(tie, "error")) add(base, "/tie_policy: tie_policy = 'error' is refused (a data-dependent failure is a disclosure oracle)")
      if (identical(tie, "all") && !identical(fmt, "recurrent_events")) add(base, "/tie_policy: tie_policy = 'all' applies to recurrent_events only")
      if (identical(tie, "sequential") && !identical(fmt, "multi_state")) add(base, "/tie_policy: tie_policy = 'sequential' applies to multi_state only")
      if (!identical(fmt, "multi_state") &&
          (length(opts[["transitions"]] %||% list()) > 0L || !is.null(opts[["state_hierarchy"]]) ||
           !is.null(opts[["state_step"]]) || !identical(opts[["initial_state"]] %||% "index", "index"))) {
        add(base, ": transitions and the multi-state graph options require format multi_state")
      }
      tags <- .irIndexSourceTags(ir, out[["rows"]])
      items <- .irItems(opts[["censor"]])
      for (i in seq_along(items)) {
        when <- if (is.list(items[[i]])) items[[i]][["censor"]][["when_index_source"]]
        if (!is.null(when)) {
          bad <- setdiff(unlist(when), tags)
          if (length(bad)) add(base, "/censor/", i - 1L, "/censor/when_index_source: impossible index_source tag(s) ", paste(bad, collapse = ", "))
        }
      }
    }
    if (identical(out[["format"]], "joined")) {
      from <- .irItems(opts[["from"]])
      members <- lapply(from, function(f) outs[[f[["output"]]]])
      rows <- unique(vapply(members, function(m) m[["rows"]] %||% "", ""))
      grains <- unique(vapply(members, function(m) m[["grain"]] %||% "", ""))
      if (length(rows) != 1L || !identical(rows, out[["rows"]])) add(base, "/from: joined inputs must share the output's rows (one roster namespace)")
      if (length(grains) != 1L || !identical(grains, out[["grain"]])) add(base, "/from: joined inputs must share one grain")
    }
    if (identical(out[["format"]], "person_period")) {
      ds <- declared_sources(out)
      for (b in .irItems(opts[["partition"]][["boundaries"]])) {
        if (!.jcsCanonical(b) %in% c(ds, vapply(.irItems(opts[["sources"]]), .jcsCanonical, ""),
                                     vapply(unname(opts[["exposures"]] %||% list()), .jcsCanonical, ""))) {
          add(base, "/partition/boundaries: boundary ", .jcsCanonical(b), " is not a declared source")
        }
      }
    }
    # A-14: per-source analyses name exactly one declared source each and
    # select from the output's own analyses
    sa <- opts[["source_analyses"]]
    if (out[["format"]] %in% c("temporal_covariates", "person_period") && is.list(sa) && length(sa)) {
      declared <- c(.irItems(out[["sources"]]), .irItems(opts[["sources"]]))
      declared <- declared[!duplicated(vapply(declared, .jcsCanonical, ""))]
      source_names <- vapply(declared, function(s) {
        as.character(s[["event"]] %||% s[["interval"]] %||% "")
      }, "")
      analyses <- as.character(unlist(opts[["analyses"]], use.names = FALSE))
      for (k in names(sa)) {
        hits <- sum(source_names == k)
        if (hits == 0L) add(base, "/source_analyses/", k, ": names no source of this output")
        if (hits > 1L) add(base, "/source_analyses/", k, ": names more than one source of this output")
        bad <- setdiff(as.character(unlist(sa[[k]], use.names = FALSE)), analyses)
        if (length(bad)) {
          add(base, "/source_analyses/", k, ": analyses not declared by the output: ",
              paste(bad, collapse = ", "))
        }
      }
    }
    # A-10 multi-state compatibility: the validated v1 state order, edges and
    # step (positive, at most nine decimals, step * (states - 1) < 1 day)
    if (!is.null(opts[["legacy_states"]]) || !is.null(opts[["legacy_state_step"]])) {
      if (!identical(opts[["format"]], "multi_state")) {
        add(base, ": legacy_states and legacy_state_step require format multi_state")
      }
      states <- as.character(unlist(opts[["legacy_states"]], use.names = FALSE))
      if (length(states)) {
        for (i in seq_along(.irItems(opts[["transitions"]]))) {
          t <- .irItems(opts[["transitions"]])[[i]]
          for (end in c("from", "to")) {
            if (!as.character(t[[end]] %||% "") %in% states) {
              add(base, "/transitions/", i - 1L, "/", end, ": names no state of legacy_states")
            }
          }
        }
        bad <- setdiff(as.character(unlist(opts[["state_hierarchy"]], use.names = FALSE)), states)
        if (length(bad)) add(base, "/state_hierarchy: names no state of legacy_states: ", paste(bad, collapse = ", "))
      }
      step <- opts[["legacy_state_step"]]
      if (is.numeric(step) && length(step) == 1L) {
        if (abs(step * 1e9 - round(step * 1e9)) > 1e-6) {
          add(base, "/legacy_state_step: at most nine decimals")
        }
        if (length(states) && step * (length(states) - 1L) >= 1) {
          add(base, "/legacy_state_step: state_step * (states - 1) must stay below one day")
        }
      }
    }
    # A-10 legacy temporal policy: ordered integer bounds
    lt <- out[["legacy_temporal"]][["index_window"]]
    if (is.list(lt) && is.numeric(lt[["start"]]) && is.numeric(lt[["end"]]) &&
        lt[["start"]] > lt[["end"]]) {
      add("/outputs/", o, "/legacy_temporal/index_window: start must not exceed end")
    }
  }

  # A-10 original v1 names: every map resolves to its declaration and is
  # one-to-one (the reverse mapping is preserved)
  ln <- ir[["legacy_names"]]
  if (is.list(ln)) {
    check_map <- function(map, pool, path) {
      for (k in names(map)) {
        if (!k %in% pool) add(path, "/", k, ": names no declaration of the IR")
      }
      if (anyDuplicated(as.character(unlist(map, use.names = FALSE)))) {
        add(path, ": two IR names map to one original name")
      }
    }
    check_map(ln[["outputs"]], names(outs), "/legacy_names/outputs")
    check_map(ln[["populations"]], names(pops), "/legacy_names/populations")
    for (kind in c("outcomes", "states", "columns")) {
      for (o in names(ln[[kind]])) {
        path <- paste0("/legacy_names/", kind, "/", o)
        if (!o %in% names(outs)) {
          add(path, ": names no output of the IR")
          next
        }
        oo <- outs[[o]][["options"]] %||% list()
        pool <- switch(kind,
          outcomes = names(oo[["outcomes"]] %||% list()),
          states = as.character(unlist(oo[["legacy_states"]], use.names = FALSE)),
          columns = as.character(unlist(outs[[o]][["columns"]], use.names = FALSE)))
        check_map(ln[[kind]][[o]], pool, path)
      }
    }
  }

  # A-16 scope cohort sources: scope.cohort is the first source v1 folds in
  sources <- ir[["scope"]][["legacy_cohort_sources"]]
  if (length(sources) &&
      !identical(as.character(unlist(sources[[1L]])), as.character(unlist(ir[["scope"]][["cohort"]])))) {
    add("/scope/legacy_cohort_sources: scope.cohort must be the first cohort source")
  }

  # 7. caps --------------------------------------------------------------------
  # Caps are wire limits on the predicates a client wrote. A lowered v1 plan's
  # trees were capped as written by the v1 validators the closed envelope runs
  # (.assertCustomFilterSafe(), .validateCohortFilterTree(), the same server
  # options); the conjunctions the lowering adds around them are not counted
  # again (re-review R3: an issued bin at the depth cap stays executable).
  max_depth <- .extractionCap("dsomop.max_filter_depth", 32L)
  max_nodes <- .extractionCap("dsomop.max_filter_nodes", 1024L)
  max_values <- .extractionCap("dsomop.max_filter_values", 10000L)
  capped <- if (identical(attr(ir, "source_version"), 1L)) list() else .irPredicateTrees(ir)
  for (t in capped) {
    st <- .irTreeStats(t[["tree"]])
    if (st[["depth"]] > max_depth) add(t[["path"]], ": predicate depth exceeds the server cap of ", max_depth)
    if (st[["nodes"]] > max_nodes) add(t[["path"]], ": predicate size exceeds the server cap of ", max_nodes)
    if (st[["values"]] > max_values) add(t[["path"]], ": predicate values exceed the server cap of ", max_values)
  }
  # the node cap counts the nodes a language 2.0 client wrote; the nodes the v1
  # lowering synthesises (one event and variable per feature, ...) are bounded
  # by the v1 caps instead (outputs, feature specs, filter complexity)
  n_nodes <- length(cs) + length(ev) + length(iv) + length(pops) + length(vars) + length(outs)
  max_plan_nodes <- .extractionCap("dsomop.max_plan_nodes", 1000L)
  if (!identical(attr(ir, "source_version"), 1L) && n_nodes > max_plan_nodes) {
    add("/: plan has more nodes than the server cap of ", max_plan_nodes)
  }
  if (length(outs) > .extractionCap("dsomop.max_plan_outputs", 100L)) {
    add("/outputs: plan exceeds the server max_plan_outputs cap")
  }
  errors
}

# --- operations (capability names, section 4.4) ---------------------------------

#' Operations of section 4.4 a plan uses
#' @keywords internal
.irOperations <- function(ir) {
  ops <- if (.conceptNeedsPresentation(ir)) "output.concept_labels" else character(0)
  ops <- c(ops, .numberCapabilities(ir), .categoryCapabilities(ir), if(.categoryNative(ir))"output.codebook_metadata")
  date_vars <- .numberVariables(ir)
