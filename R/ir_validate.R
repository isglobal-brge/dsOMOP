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
# strings, concept columns integers) and ordered `between` bounds.
.irCheckPredicateLeaf <- function(leaf, path, add) {
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
  refs <- .irCollectRefs(ir[c("events", "intervals", "populations", "variables", "outputs")])
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
      if (!v %in% names(vars)) add("/outputs/", o, "/columns: dangling variable '", v, "'")
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
      if (anyDuplicated(unlist(members))) add(base, "/setop/members: members must be distinct")
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

  # 4. predicate typing at every predicate position (events, adapters,
  # criteria person leaves, reducers) by column name; the site checks type
  # them against the actual columns ---------------------------------------------
  for (pr in .irPredicates(ir)) {
    .irWalkPredicate(pr[["tree"]], function(l, lp) .irCheckPredicateLeaf(l, paste0(pr[["path"]], lp), add), "")
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
  max_depth <- .extractionCap("dsomop.max_filter_depth", 32L)
  max_nodes <- .extractionCap("dsomop.max_filter_nodes", 1024L)
  max_values <- .extractionCap("dsomop.max_filter_values", 10000L)
  for (t in .irPredicateTrees(ir)) {
    st <- .irTreeStats(t[["tree"]])
    if (st[["depth"]] > max_depth) add(t[["path"]], ": predicate depth exceeds the server cap of ", max_depth)
    if (st[["nodes"]] > max_nodes) add(t[["path"]], ": predicate size exceeds the server cap of ", max_nodes)
    if (st[["values"]] > max_values) add(t[["path"]], ": predicate values exceed the server cap of ", max_values)
  }
  n_nodes <- length(cs) + length(ev) + length(iv) + length(pops) + length(vars) + length(outs)
  max_plan_nodes <- .extractionCap("dsomop.max_plan_nodes", 1000L)
  if (n_nodes > max_plan_nodes) add("/: plan has more nodes than the server cap of ", max_plan_nodes)
  if (length(outs) > .extractionCap("dsomop.max_plan_outputs", 100L)) {
    add("/outputs: plan exceeds the server max_plan_outputs cap")
  }
  errors
}

# --- operations (capability names, section 4.4) ---------------------------------

#' Operations of section 4.4 a plan uses
#' @keywords internal
.irOperations <- function(ir) {
  ops <- character(0)
  op <- function(...) ops <<- c(ops, ...)
  if (length(ir[["concept_sets"]]) || any(vapply(ir[["events"]] %||% list(), function(e) !is.null(e[["concept_set"]]), logical(1)))) op("concept_set")
  for (e in ir[["events"]] %||% list()) {
    if (!is.null(e[["episode_members"]])) op("event.episode_members")
    else if (!is.null(e[["cost"]])) op("event.cost")
    else op("event")
    if (!is.null(e[["unit"]])) op("event.unit")
    on_missing <- e[["on_missing_table"]] %||% e[["episode_members"]][["on_missing_table"]] %||% e[["cost"]][["on_missing_table"]]
    if (identical(on_missing, "unavailable")) op("event.on_missing_table")
  }
  for (i in ir[["intervals"]] %||% list()) {
    op("interval")
    if (identical(i[["by"]], "ingredient")) op("interval.ingredient")
    if (isTRUE(i[["stockpile"]])) op("interval.stockpile")
  }
  for (p in ir[["populations"]] %||% list()) {
    kind <- p[["kind"]]
    if (identical(kind, "index")) {
      op("population.index")
      if (identical(p[["index"]][["select"]], "first_eligible")) op("population.first_eligible")
      if (!is.null(p[["index"]][["source"]][["any_of"]])) op("population.any_of")
    }
    if (identical(kind, "criteria")) op("population.criteria")
    if (identical(kind, "setop")) op("population.setop")
    if (identical(kind, "match")) op("population.match")
    if (is.character(p[["parent"]])) op("population.parent")
    if (identical(p[["coverage"]][["policy"]], "union")) op("population.coverage.union")
    .irWalkCriteria(p[["where"]], function(k, leaf, path) {
      if (identical(k, "age")) op("criteria.age")
      # A criterion-specific coverage union (sections 1.3, 6.13): an observed()
      # leaf with an explicit gap, 0 included, coalesces observation periods.
      if (identical(k, "observed") && is.list(leaf) && !is.null(leaf[["gap"]])) {
        op("population.coverage.union")
      }
    }, "")
    if (.irHasNot(p)) op("criteria.not")
  }
  anchors <- .irCollectAnchors(ir[c("populations", "variables", "outputs")])
  if (length(anchors)) op("window.anchor.source")
  for (v in ir[["variables"]] %||% list()) {
    if (!is.null(v[["reducer"]])) {
      op("variable.reducer")
      if (identical(v[["reducer"]][["fn"]], "supply_days")) op("reducer.supply_days")
      if (identical(v[["reducer"]][["coverage"]], "effective_supply")) op("reducer.covered_days.effective_supply")
    }
    if (!is.null(v[["derived"]])) op("variable.derived")
    if (!is.null(v[["raw"]])) op("variable.raw")
    if (!is.null(v[["population_column"]])) op("variable.population_column")
  }
  for (o in ir[["outputs"]] %||% list()) {
    op(paste0("output.", o[["format"]]))
    if (identical(o[["format"]], "long") && isTRUE(o[["options"]][["event_log"]])) op("output.long.event_log")
    if (identical(o[["format"]], "survival")) {
      if (any(vapply(.irItems(o[["options"]][["censor"]]), function(c) is.list(c) && (!is.null(c[["censor"]]) || !is.null(c[["event"]]) || !is.null(c[["interval"]])), logical(1)))) {
        op("output.survival.censor_source")
      }
    }
    if (identical(o[["format"]], "person_period") &&
        length(o[["options"]][["partition"]][["boundaries"]] %||% list()) + length(o[["options"]][["partition"]][["age_bands"]] %||% NULL) +
        length(o[["options"]][["partition"]][["calendar"]] %||% NULL) > 0L) {
      op("output.person_period.partition")
    }
  }
  # publish.roots: a declared strict subset of the outputs (a lowered v1 plan
  # has no publish: its roots are the outputs the execution maps)
  if (!is.null(ir[["publish"]]) &&
      length(ir[["publish"]]) != length(ir[["outputs"]] %||% list())) {
    op("publish.roots")
  }
  unique(ops)
}

#' Operations a deployment disabled (server option, none by default)
#' @keywords internal
.irDisabledOperations <- function() {
  disabled <- getOption("dsomop.recipe_operations_disabled",
                        getOption("default.dsomop.recipe_operations_disabled",
                                  character(0)))
  as.character(unlist(disabled, use.names = FALSE))
}

# --- handle-dependent checks --------------------------------------------------------

# Normalised columns of an event stream (design section 6.6, ev_<name>): the
# role each needs (NA = always present) and its value type.
.IR_EVENT_ALIASES <- list(
  start_date = c("start", "date"), end_date = c(NA, "date"),
  end_recorded = c("end", "date"), supply = c("supply", "numeric"),
  concept_id = c("domain_concept", "integer"), value = c("value", "numeric"),
  unit_concept_id = c("unit", "integer"), operator_concept_id = c("operator", "integer"),
  range_low = c("range_low", "numeric"), range_high = c("range_high", "numeric"),
  visit_id = c("visit_occurrence", "integer")
)

# Typed columns a predicate may reference: data.frame(name, value_type).
.irColumns <- function(names, types) {
  data.frame(name = as.character(names), value_type = as.character(types),
             stringsAsFactors = FALSE)
}

.irEventPredicateColumns <- function(role) {
  aliases <- Filter(function(a) {
    needs <- .IR_EVENT_ALIASES[[a]][[1L]]
    is.na(needs) || !is.null(role[[needs]])
  }, names(.IR_EVENT_ALIASES))
  alias <- .irColumns(aliases, vapply(.IR_EVENT_ALIASES[aliases], `[[`, "", 2L))
  cols <- role[["columns"]]
  phys <- .irColumns(cols[["column_name"]][cols[["filterable"]]],
                     cols[["value_type"]][cols[["filterable"]]])
  out <- rbind(alias, phys)
  out[!duplicated(out[["name"]]), , drop = FALSE]
}

#' Columns a predicate position may reference at this site
#'
#' @param ir The IR.
#' @param handle CDM handle.
#' @param bp Its blueprint.
#' @param context The position's context (\code{.irPredicateTrees()}).
#' @return data.frame(name, value_type), or NULL when the source is not
#'   typed here (an unavailable or absent source, an interval source).
#' @keywords internal
.irPredicateColumns <- function(ir, handle, bp, context) {
  ev <- ir[["events"]] %||% list()
  event_columns <- function(e, seen = character(0)) {
    node <- ev[[e]]
    if (is.null(node) || e %in% seen) return(NULL)
    if (!is.null(node[["table"]])) {
      role <- .roleOf(handle, node[["table"]])
      return(if (is.null(role)) NULL else .irEventPredicateColumns(role))
    }
    if (is.list(node[["episode_members"]])) return(members_columns(node))
    if (is.list(node[["cost"]])) return(cost_columns())
    NULL
  }
  members_columns <- function(node) {
    fields <- .irEpisodeEventFields()
    tables <- unlist(node[["episode_members"]][["tables"]] %||% fields[["table_name"]], use.names = FALSE)
    roles <- Filter(Negate(is.null), lapply(unique(tables), function(t) .roleOf(handle, t)))
    if (!length(roles)) return(NULL)
    out <- do.call(rbind, lapply(roles, function(r) {
      cols <- .irEventPredicateColumns(r)
      cols[cols[["name"]] %in% names(.IR_EVENT_ALIASES), , drop = FALSE]
    }))
    out <- rbind(out, .irColumns(c("episode_id", "episode_concept_id"), c("integer", "integer")))
    out[!duplicated(out[["name"]]), , drop = FALSE]
  }
  cost_columns <- function() {
    out <- .irColumns(c("value", "currency_concept_id", "cost_type_concept_id", "start_date", "end_date"),
                      c("numeric", "integer", "integer", "date", "date"))
    col_df <- bp[["columns"]][["cost"]]
    if (!is.null(col_df) && nrow(col_df)) {
      frame <- .roleColumnFrame(col_df, .filterableColumns(bp, "cost"))
      out <- rbind(out, .irColumns(frame[["column_name"]][frame[["filterable"]]],
                                   frame[["value_type"]][frame[["filterable"]]]))
    }
    out[!duplicated(out[["name"]]), , drop = FALSE]
  }
  switch(context[["kind"]],
    event = event_columns(context[["event"]]),
    episode_members = members_columns(ev[[context[["event"]]]]),
    cost = cost_columns(),
    person = {
      col_df <- bp[["columns"]][["person"]]
      if (is.null(col_df) || !nrow(col_df)) return(NULL)
      frame <- .roleColumnFrame(col_df, .filterableColumns(bp, "person"))
      .irColumns(frame[["column_name"]][frame[["filterable"]]],
                 frame[["value_type"]][frame[["filterable"]]])
    },
    source = {
      src <- context[["source"]]
      if (is.character(src[["event"]])) event_columns(src[["event"]]) else NULL
    },
    NULL)
}

# One literal against a column type ("unknown": not typed).
.irLiteralTypeOk <- function(v, type) {
  switch(type,
    integer = is.numeric(v) && length(v) == 1L && !is.na(v) && v == round(v),
    numeric = is.numeric(v) && length(v) == 1L && !is.na(v),
    date = .irIsoDateOk(v),
    text = is.character(v) && length(v) == 1L,
    boolean = is.logical(v) && length(v) == 1L && !is.na(v),
    TRUE)
}

# Site typing of one predicate leaf: the column exists and is filterable for
# this source, and every literal has the column's type.
.irCheckSiteLeaf <- function(leaf, cols, path, add) {
  var <- leaf[["var"]]
  hit <- match(var, cols[["name"]])
  if (is.na(hit)) {
    return(add(path, ": column '", var, "' is not a filterable column of this source ",
               "(absent, an identifier, class 1, or a role the source does not have)"))
  }
  type <- cols[["value_type"]][[hit]]
  values <- switch(leaf[["op"]],
    `in` =, not_in = .irItems(leaf[["value"]]),
    between = list(leaf[["value"]][["lower"]], leaf[["value"]][["upper"]]),
    is_null =, not_null = list(),
    list(leaf[["value"]]))
  for (v in values) {
    if (!.irLiteralTypeOk(v, type)) {
      add(path, ": column '", var, "' is of type ", type, "; the literal does not match")
      break
    }
  }
}

#' Predicate catalogue of an interval's course rows (design 6.7, 6.11)
#'
#' A reducer over an interval source reads the course relation
#' \code{iv_<name>}: one row per era with the event column contract
#' (WS-5, \code{R/compile_interval.R}). The columns an era actually carries
#' are its dates and its group concept; its identity and grouping columns are
#' private, and the value, unit, operator, range and supply columns of the
#' contract are typed NULLs for an era (\code{.INTERVAL_COURSE_NULLS}), so a
#' predicate over them is refused as a role the source does not have. Pure:
#' the contract does not depend on the site.
#'
#' @param name Interval name.
#' @return A \code{.predicateCatalogue()}.
#' @keywords internal
.irIntervalCatalogue <- function(name) {
  columns <- list(start_date = list(type = "date"), end_date = list(type = "date"),
                  end_recorded = list(type = "date"),
                  concept_id = list(type = "integer", concept = TRUE))
  private <- "an era's identity, grouping and record count are private"
  no_role <- "an era has no value, unit, operator, range or supply role"
  refused <- c(person_id = "a person identifier is never filterable",
               event_id = "row identifiers are never filterable",
               source_ns = "the source namespace is internal",
               visit_id = "row identifiers are never filterable",
               group_id = private, era_no = private, era_key = private, n_events = private,
               value = no_role, value_source = no_role, unit_concept_id = no_role,
               unit_source = no_role, unit_unknown = no_role, operator_concept_id = no_role,
               range_low = no_role, range_high = no_role, range_low_source = no_role,
               range_high_source = no_role, supply = no_role)
  .predicateCatalogue(columns, character(0), refused, what = paste0("interval '", name, "'"))
}

# One column name resolved against a predicate catalogue: a finding, or none.
.irResolveFinding <- function(catalogue, var, path) {
  tryCatch({
    .predicateResolve(catalogue, var, path)
    character(0)
  }, dsomop_plan_rejection = function(e) {
    paste0(e[["field"]] %||% path, ": ", sub("^[a-z_]+: ", "", conditionMessage(e)))
  })
}

#' Handle-dependent findings (design section 6.1)
#'
#' Role records for every event table (respecting on_missing_table); every
#' predicate position -- event and adapter predicates, criteria person
#' predicates, reducer predicates -- resolved against its source's columns
#' (existence, filterability, the roles the source has, literal types); a
#' concept role for \code{concept_col} and a numeric type for an event value
#' column; reducer value columns; raw columns and their classes (section
#' 7.6).
#'
#' Each IR is checked with the rules of the engine that executes it. A native
#' plan meets the compilers' own metadata rules: WS-4's
#' \code{.eventSiteCheck()} for every event and adapter source that this site
#' serves (its concept, value, unit, supply and visit roles, its
#' \code{where} and the reducer predicates over its rows, typed against the
#' normalised columns) and \code{.predicateTableCatalogue()} /
#' \code{.predicateSql()} for criteria person predicates. A lowered v1 plan
#' runs on the v1 executors, whose own site requirements
#' \code{.planV1SiteCheck()} checks first; here its predicates are typed
#' against the columns the v1 executors accept (the role aliases and the
#' table's filterable physical columns), and an event over a table with no
#' event role record that is present at the site is a legacy row source (the
#' v1 executors read its rows directly: \code{event_level} over
#' \code{person}, for instance) whose predicates are the executors' to check.
#'
#' @return list(errors = character() of plan findings, missing_tables =
#'   character() of findings for event sources this site cannot serve under
#'   on_missing_table = "error" (the unavailable_table refusal), unavailable
#'   = character() of events whose sources this site cannot serve under
#'   on_missing_table = "unavailable").
#' @keywords internal
.irSiteErrors <- function(ir, handle) {
  errors <- character(0)
  missing_tables <- character(0)
  unavailable <- character(0)
  add <- function(...) errors <<- c(errors, paste0(...))
  bp <- .buildBlueprint(handle)
  native <- !identical(attr(ir, "source_version"), 1L)
  present <- bp[["tables"]][["table_name"]][bp[["tables"]][["present_in_db"]]]
  ev <- ir[["events"]] %||% list()
  vars <- ir[["variables"]] %||% list()
  # reducer predicates over each event's rows: typed with that event's own
  # catalogue (native plans)
  reducer_predicates <- list()
  for (i in seq_along(vars)) {
    src <- vars[[i]][["source"]][["event"]]
    pred <- vars[[i]][["reducer"]][["predicate"]]
    if (is.character(src) && length(src) == 1L && !is.null(pred)) {
      path <- paste0("/variables/", names(vars)[[i]], "/reducer/predicate")
      reducer_predicates[[src]][[path]] <- pred
    }
  }
  for (i in seq_along(ev)) {
    e <- names(ev)[[i]]
    node <- ev[[i]]
    base <- paste0("/events/", e)
    n_errors <- length(errors)
    n_missing <- length(missing_tables)
    # An event's or adapter's own tables are site resources under its
    # on_missing_table.
    site_missing <- function(field, table, mode) {
      if (identical(mode, "unavailable")) {
        unavailable <<- unique(c(unavailable, e))
      } else {
        missing_tables <<- c(missing_tables, paste0(
          base, field, ": table '", table, "' is not available at this site"))
      }
    }
    if (!is.null(node[["table"]])) {
      role <- .roleOf(handle, node[["table"]])
      if (is.null(role)) {
        if (!native && tolower(node[["table"]]) %in% present) next
        if (identical(.roleAvailability(handle, node[["table"]]), "not_event_source")) {
          add(base, "/table: '", node[["table"]], "' is not an event table (no event role record)")
        } else {
          site_missing("/table", node[["table"]], node[["on_missing_table"]])
        }
        next
      }
      visit <- node[["visit"]]
      if (native && is.list(visit) && !isTRUE(unlist(visit[["index"]])) && !is.null(visit[["type"]]) &&
          is.null(.roleOf(handle, "visit_occurrence"))) {
        site_missing("/visit", "visit_occurrence", node[["on_missing_table"]])
      }
      if (!native) {
        cols <- role[["columns"]]
        col <- function(name) {
          k <- match(name, cols[["column_name"]])
          if (is.na(k) || !isTRUE(cols[["filterable"]][[k]])) NULL else cols[k, , drop = FALSE]
        }
        cc <- node[["concept_col"]]
        if (!is.null(cc)) {
          r <- col(cc)
          if (is.null(r)) {
            add(base, "/concept_col: '", cc, "' is not a filterable column of '", node[["table"]], "'")
          } else if (!isTRUE(r[["is_concept"]])) {
            add(base, "/concept_col: '", cc, "' is not a concept column of '", node[["table"]], "'")
          }
        }
        vc <- node[["value"]][["column"]]
        if (!is.null(vc)) {
          r <- col(vc)
          if (is.null(r)) {
            add(base, "/value/column: '", vc, "' is not a filterable column of '", node[["table"]], "'")
          } else if (!r[["value_type"]] %in% c("integer", "numeric", "unknown")) {
            add(base, "/value/column: '", vc, "' is not a numeric column of '", node[["table"]], "'")
          }
        }
        if (!is.null(node[["unit"]]) && (is.null(role[["unit"]]) ||
                                          (is.null(role[["value"]]) && is.null(vc)))) {
          add(base, "/unit: table '", node[["table"]], "' has no value and unit roles to normalise")
        }
        if (!is.null(node[["end"]][["fallback"]]) && is.null(role[["supply"]])) {
          add(base, "/end/fallback: table '", node[["table"]], "' has no days_supply role")
        }
      }
    }
    if (!is.null(node[["episode_members"]])) {
      fields <- .irEpisodeEventFields()
      mode <- node[["episode_members"]][["on_missing_table"]]
      if (is.null(bp[["columns"]][["episode_event"]])) {
        site_missing("/episode_members", "episode_event", mode)
      }
      for (t in unlist(node[["episode_members"]][["tables"]] %||% list())) {
        if (!t %in% fields[["table_name"]]) {
          add(base, "/episode_members/tables: '", t, "' has no reviewed episode_event field mapping")
        } else if (is.null(.roleOf(handle, t))) {
          site_missing("/episode_members/tables", t, mode)
        }
      }
    }
    if (!is.null(node[["cost"]])) {
      parent <- ev[[node[["cost"]][["event"]][["event"]]]]
      domains <- .irCostDomains()
      if (is.null(parent[["table"]]) || !parent[["table"]] %in% domains[["table_name"]]) {
        add(base, "/cost/event: the cost source must reference a table event with a reviewed cost_domain_id mapping")
      }
      if (is.null(bp[["columns"]][["cost"]])) {
        site_missing("/cost", "cost", node[["cost"]][["on_missing_table"]])
      }
    }
    # a native source this site serves meets the compilers' own rules
    if (native && length(errors) == n_errors && length(missing_tables) == n_missing &&
        !e %in% unavailable) {
      errors <- c(errors, .eventSiteCheck(handle, ir, e, predicates = reducer_predicates[[e]] %||% list()))
    }
  }
  # every other predicate position, typed against the columns of its source
  h <- if (native) .sqlDialectProfile(handle, probe = FALSE)
  person_catalogue <- NULL
  for (pr in .irPredicates(ir)) {
    context <- pr[["context"]]
    if (native) {
      # event, adapter and reducer predicates over events were typed above;
      # a reducer predicate over an interval is typed against the interval's
      # own catalogue (its course rows), never left untyped
      if (!identical(context[["kind"]], "person")) {
        interval <- context[["source"]][["interval"]]
        if (identical(context[["kind"]], "source") && is.character(interval) && length(interval) == 1L) {
          errors <- c(errors, tryCatch({
            .predicateSql(h, pr[["tree"]], .irIntervalCatalogue(interval), "x", pr[["path"]])
            NULL
          }, dsomop_plan_rejection = function(e) {
            paste0(e[["field"]] %||% pr[["path"]], ": ", sub("^[a-z_]+: ", "", conditionMessage(e)))
          }))
        }
        next
      } else {
        if (is.null(person_catalogue)) {
          person_catalogue <- tryCatch(.predicateTableCatalogue(handle, "person", "the person table"),
                                       dsomop_plan_rejection = function(e) e)
        }
        if (inherits(person_catalogue, "dsomop_plan_rejection")) {
          missing_tables <- unique(c(missing_tables, paste0(
            pr[["path"]], ": table 'person' is not available at this site")))
          next
        }
        errors <- c(errors, tryCatch({
          .predicateSql(h, pr[["tree"]], person_catalogue, "p", pr[["path"]])
          NULL
        }, dsomop_plan_rejection = function(e) {
          paste0(e[["field"]] %||% pr[["path"]], ": ", sub("^[a-z_]+: ", "", conditionMessage(e)))
        }))
        next
      }
    } else {
      cols <- .irPredicateColumns(ir, handle, bp, context)
    }
    if (is.null(cols)) next
    .irWalkPredicate(pr[["tree"]], function(l, lp) .irCheckSiteLeaf(l, cols, paste0(pr[["path"]], lp), add), "")
  }
  for (i in seq_along(vars)) {
    v <- names(vars)[[i]]
    node <- vars[[i]]
    raw <- node[["raw"]]
    if (is.list(raw)) {
      col_df <- bp[["columns"]][[raw[["table"]]]]
      if (is.null(col_df) || !raw[["column"]] %in% col_df[["column_name"]]) {
        add("/variables/", v, "/raw: column '", raw[["column"]], "' not found in table '", raw[["table"]], "'")
      } else if (identical(.roleColumnClass(raw[["column"]], col_df), 1L)) {
        add("/variables/", v, "/raw: column '", raw[["column"]], "' is never released (class 1)")
      }
    }
    # a reducer reading a named column needs that column on its source:
    # resolved like a predicate variable (existence, private keys, the roles
    # the source has) against the source's own catalogue
    value <- node[["reducer"]][["value"]]
    src <- node[["source"]][["event"]]
    interval <- node[["source"]][["interval"]]
    path <- paste0("/variables/", v, "/reducer/value")
    if (is.character(value) && length(value) == 1L) {
      if (native && is.character(interval)) {
        errors <- c(errors, .irResolveFinding(.irIntervalCatalogue(interval), value, path))
      } else if (native && is.character(src) && !src %in% unavailable) {
        catalogue <- tryCatch(.eventSiteCatalogue(handle, ir, src), dsomop_plan_rejection = function(e) NULL)
        if (!is.null(catalogue)) errors <- c(errors, .irResolveFinding(catalogue, value, path))
      } else if (!native && !identical(value, "value") && is.character(src)) {
        cols <- .irPredicateColumns(ir, handle, bp, list(kind = "event", event = src))
        if (!is.null(cols) && !value %in% cols[["name"]]) {
          add(path, ": column '", value, "' is not a filterable column of the source")
        }
      }
    }
  }
  list(errors = errors, missing_tables = missing_tables, unavailable = unavailable)
}

#' Outputs whose sources this site cannot serve (the status field
#' \code{unavailable}, design section 7.3)
#'
#' @param ir The IR.
#' @param events Events marked unavailable by \code{.irSiteErrors()}.
#' @return Character vector of output names (declaration order) whose
#'   dependency closure (\code{.irOutputClosure()}) reads one of them.
#' @keywords internal
.irUnavailableOutputs <- function(ir, events) {
  if (!length(events)) return(character(0))
  outs <- names(ir[["outputs"]] %||% list())
  outs[vapply(outs, function(o) {
    any(events %in% names(.irOutputClosure(ir, o)[["events"]]))
  }, logical(1))]
}

.irEpisodeEventFields <- function() {
  cached <- .irCache[["episode_event_fields"]]
  if (is.null(cached)) {
    cached <- utils::read.csv(system.file("ohdsi", "episode_event_fields.csv", package = "dsOMOP"),
                              stringsAsFactors = FALSE, check.names = FALSE)
    assign("episode_event_fields", cached, envir = .irCache)
  }
  cached
}

.irCostDomains <- function() {
  cached <- .irCache[["cost_domains"]]
  if (is.null(cached)) {
    cached <- utils::read.csv(system.file("ohdsi", "cost_domains.csv", package = "dsOMOP"),
                              stringsAsFactors = FALSE, check.names = FALSE)
    assign("cost_domains", cached, envir = .irCache)
  }
  cached
}

#' Does a lowered v1 plan materialise session temporary tables?
#'
#' The v1 executors create them for the plan cohort (results cohort, inline
#' spec, filter tree), every declared population and the resolved scope
#' (R/plan.R, R/cohort.R: \code{.cohortCreate}, \code{.cohortCombine},
#' \code{.materializeIndexEventCohort}, \code{.cohortFromTokenFrame}); only a
#' plan over the implicit all-persons base with no scope reads its tables
#' directly. Read from the IR document, so explain and execution agree.
#'
#' @param ir A lowered v1 IR.
#' @return TRUE or FALSE.
#' @keywords internal
.irV1NeedsSessionTables <- function(ir) {
  scope <- ir[["scope"]] %||% list()
  if (!is.null(scope[["cohort"]]) || length(scope[["tables"]] %||% list())) return(TRUE)
  pops <- ir[["populations"]] %||% list()
  if (!identical(names(pops), "base")) return(TRUE)
  b <- pops[["base"]]
  !(identical(b[["kind"]], "criteria") && is.null(b[["where"]]) && is.null(b[["parent"]]) &&
      is.null(b[["cohort_definition_id"]]) && is.null(b[["legacy_episodes"]]))
}

#' Dialect preflight of an IR (design sections 6.1, 6.3 and 9)
#'
#' Calls WS-3's \code{.sqlDialectPreflight(h, .sqlPlanConstructs(ops),
#' session_tables = ...)} (\code{R/sql_expr.R}): once per output, over the
#' part of the plan the output depends on (\code{.irOutputClosure()}), so that
#' the typed \code{dialect_unsupported} rejection names the output, then once
#' for the whole plan, which refuses whatever no single output accounts for.
#' The database is probed once. A native plan with populations is taken to
#' reuse a roster in more than one statement (session temp tables), the
#' conservative reading until the compilers decide where rosters are
#' materialised (design section 6.4). A lowered v1 plan is executed by the v1
#' executors in this release stage and needs exactly what they need: no typed
#' construct (their SQL is the legacy translator's), and session temp tables
#' whenever they materialise a cohort, a population or a scope
#' (\code{.irV1NeedsSessionTables()}).
#'
#' @param ir The IR (semantically valid).
#' @param handle CDM handle (or context / profile) to preflight against.
#' @return The probed dialect profile, invisibly.
#' @keywords internal
.irDialectPreflight <- function(ir, handle) {
  profile <- .sqlDialectProfile(handle, probe = TRUE)
  if (identical(attr(ir, "source_version"), 1L)) {
    return(invisible(.sqlDialectPreflight(profile, constructs = character(0),
                                          session_tables = .irV1NeedsSessionTables(ir))))
  }
  needs <- function(part) {
    list(constructs = .sqlPlanConstructs(.irOperations(part)),
         session_tables = length(part[["populations"]] %||% list()) > 0L)
  }
  for (o in .irOrder(ir)[["outputs"]]) {
    n <- needs(.irOutputClosure(ir, o))
    .sqlDialectPreflight(profile, constructs = n[["constructs"]],
                         session_tables = n[["session_tables"]], output = o)
  }
  n <- needs(ir)
  invisible(.sqlDialectPreflight(profile, constructs = n[["constructs"]],
                                 session_tables = n[["session_tables"]]))
}

#' Semantic and handle-dependent validation (step 4)
#'
#' Stages, each refusing with one typed rejection (design section 7.3 codes,
#' as the publication status transports them): the semantic rules and
#' deployment-disabled operations (\code{validation_error}); the site checks
#' -- plan findings first (\code{validation_error}), then event sources this
#' site cannot serve under \code{on_missing_table = "error"}
#' (\code{unavailable_table}); the dialect preflight
#' (\code{dialect_unsupported}). All run before any query, for native and
#' lowered v1 plans alike (a lowered plan's legacy semantics are in its
#' compatibility variants, never an exemption from a stage). A lowered v1
#' plan first meets the site requirements of the v1 executors that run it,
#' with their own rules and messages (\code{.planV1SiteCheck()} on
#' \code{v1_plan}), then the IR's site checks. Sources served under
#' \code{on_missing_table = "unavailable"} are never a refusal: their events
#' and the outputs reading them are reported (\code{unavailable},
#' \code{unavailable_outputs}). \code{insufficient_support} is never raised
#' here: support exists only for computed outputs, at publication.
#'
#' @param ir The IR (schema-valid).
#' @param handle CDM handle, or NULL for the pure semantic rules only.
#' @param v1_plan For a lowered v1 IR, the v1 plan it was lowered from (the
#'   parsed JSON form that passed the closed envelope), checked against the
#'   v1 executors' site requirements when a handle is given.
#' @return Invisibly, a list(operations, unavailable, unavailable_outputs,
#'   site_findings, dialect); stops with a typed rejection on the first
#'   failing stage.
#' @keywords internal
.irValidate <- function(ir, handle = NULL, v1_plan = NULL) {
  errors <- .irSemanticErrors(ir)
  if (length(errors)) .irRejectAll("plan fails the semantic rules", errors)
  ops <- .irOperations(ir)
  disabled <- intersect(ops, .irDisabledOperations())
  if (length(disabled)) {
    .irReject(paste0("operation(s) disabled by this deployment: ",
                     paste(disabled, collapse = ", ")))
  }
  report <- list(operations = ops, unavailable = character(0),
                 unavailable_outputs = character(0),
                 site_findings = character(0), dialect = NULL)
  if (is.null(handle)) return(invisible(report))
  if (!is.null(v1_plan)) .planV1SiteCheck(v1_plan, handle)
  site <- .irSiteErrors(ir, handle)
  report[["unavailable"]] <- site[["unavailable"]]
  report[["unavailable_outputs"]] <- .irUnavailableOutputs(ir, site[["unavailable"]])
  report[["site_findings"]] <- c(site[["errors"]], site[["missing_tables"]])
  if (length(site[["errors"]])) {
    .irRejectAll("plan fails the site checks", report[["site_findings"]])
  }
  if (length(site[["missing_tables"]])) {
    .irRejectAll("plan needs tables this site does not provide",
                 site[["missing_tables"]], code = "unavailable_table")
  }
  report[["dialect"]] <- .irDialectPreflight(ir, handle)
  invisible(report)
}
