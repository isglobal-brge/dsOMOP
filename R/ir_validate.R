# Module: IR semantic and handle-dependent validation (design-2.10.3 sections
# 4.1 "rules the schema cannot express", 6.1 and 9).
#
# .irValidate(ir, handle) runs after ir-v2.json (step 4 of the frozen
# sequence). The semantic rules are pure and run in the order of section 4.1;
# the handle-dependent checks need the site: tables and columns through the
# role registry, filterable and typed predicate columns, column classes,
# deployment-disabled operations and the dialect preflight.
#
# A lowered v1 plan is still executed by the unchanged v1 executors in this
# release stage (the v2 compilers arrive in WS-4..WS-7). Its handle-dependent
# checks therefore stay the executors' own (same messages, same per-output
# behaviour); .irValidate applies the semantic rules to it and reports the
# site checks without enforcing them (explain shows them).

# --- small helpers ------------------------------------------------------------

.irIsName <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl("^[A-Za-z][A-Za-z0-9_]{0,63}$", x)
}

.irItems <- function(x) if (is.null(x)) list() else .jsonArrayItems(x)

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

.irPredicateStats <- function(node, depth = 1L, stats = new.env()) {
  if (is.null(stats[["nodes"]])) { stats[["nodes"]] <- 0L; stats[["depth"]] <- 0L; stats[["values"]] <- 0L }
  if (!is.list(node)) return(stats)
  stats[["nodes"]] <- stats[["nodes"]] + 1L
  stats[["depth"]] <- max(stats[["depth"]], depth)
  nm <- names(node)
  if (!is.null(nm) && any(c("and", "or") %in% nm)) {
    for (child in .irItems(node[[intersect(nm, c("and", "or"))[[1L]]]])) {
      .irPredicateStats(child, depth + 1L, stats)
    }
  } else if (!is.null(nm) && "not" %in% nm) {
    .irPredicateStats(node[["not"]], depth + 1L, stats)
  } else if (!is.null(nm) && "value" %in% nm) {
    stats[["values"]] <- stats[["values"]] + length(unlist(node[["value"]], use.names = FALSE))
  }
  stats
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
    indexed <- .irPopulationIndexed(pops, out[["rows"]])
    if (identical(out[["grain"]], "person") && indexed && is.null(out[["collapse"]]) &&
        !out[["format"]] %in% c("membership", "dictionary", "joined")) {
      add("/outputs/", o, "/collapse: a person-grain output over the indexed population '",
          out[["rows"]], "' requires an explicit collapse policy")
    }
    if (identical(out[["grain"]], "episode") && !indexed) {
      add("/outputs/", o, "/grain: episode grain needs an indexed population")
    }
  }

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
      if (!.irPopulationIndexed(pops, m[["cases"]])) {
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
          !legacy && !.irPopulationIndexed(pops, p)) {
        add(base, "/where", path, ": an index-referenced age criterion needs an indexed population")
      }
      if (identical(kind, "person")) {
        .irWalkPredicate(leaf, function(l, lp) .irCheckPredicateLeaf(l, paste0(base, "/where", path, lp), add), "")
      }
    }, "")
  }

  # 4. predicate typing on events ---------------------------------------------------
  for (e in names(ev)) {
    node <- ev[[e]]
    where <- node[["where"]] %||% node[["episode_members"]][["where"]] %||% node[["cost"]][["where"]]
    if (!is.null(where)) {
      .irWalkPredicate(where, function(l, lp) .irCheckPredicateLeaf(l, paste0("/events/", e, "/where", lp), add), "")
    }
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
      if (!is.null(r[["predicate"]])) {
        .irWalkPredicate(r[["predicate"]], function(l, lp) .irCheckPredicateLeaf(l, paste0(base, "/reducer/predicate", lp), add), "")
      }
    }
    d <- node[["derived"]]
    if (is.list(d) && identical(d[["kind"]], "age") && !is.null(d[["age_breaks"]])) {
      b <- unlist(d[["age_breaks"]])
      if (length(b) < 2L || b[[1L]] != 0 || any(diff(b) <= 0)) {
        add(base, "/derived/age_breaks: age_breaks must be at least two strictly increasing integers starting at 0")
      }
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
  }

  # 7. caps --------------------------------------------------------------------
  max_depth <- .extractionCap("dsomop.max_filter_depth", 32L)
  max_nodes <- .extractionCap("dsomop.max_filter_nodes", 1024L)
  max_values <- .extractionCap("dsomop.max_filter_values", 10000L)
  trees <- c(lapply(pops, function(p) p[["where"]]), lapply(ev, function(e) e[["where"]]))
  for (nm in names(trees)) {
    st <- .irPredicateStats(trees[[nm]])
    if (st[["depth"]] > max_depth) add("/", nm, ": predicate depth exceeds the server cap of ", max_depth)
    if (st[["nodes"]] > max_nodes) add("/", nm, ": predicate size exceeds the server cap of ", max_nodes)
    if (st[["values"]] > max_values) add("/", nm, ": predicate values exceed the server cap of ", max_values)
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
    }, "")
    if (grepl('"not"', .jcsCanonical(p[["where"]] %||% list()), fixed = TRUE)) op("criteria.not")
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
  if (length(ir[["publish"]] %||% list()) != length(ir[["outputs"]] %||% list())) op("publish.roots")
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

#' Handle-dependent findings (design section 6.1)
#'
#' Role records for every event table (respecting on_missing_table),
#' filterable predicate columns, typed value / concept columns, raw columns
#' and their classes (section 7.6).
#'
#' @return list(errors = character(), unavailable = character() of events
#'   whose tables are absent under on_missing_table = "unavailable").
#' @keywords internal
.irSiteErrors <- function(ir, handle) {
  errors <- character(0)
  unavailable <- character(0)
  add <- function(...) errors <<- c(errors, paste0(...))
  bp <- .buildBlueprint(handle)
  for (e in names(ir[["events"]])) {
    node <- ir[["events"]][[e]]
    base <- paste0("/events/", e)
    if (!is.null(node[["table"]])) {
      role <- .roleOf(handle, node[["table"]])
      if (is.null(role)) {
        if (identical(node[["on_missing_table"]], "unavailable")) {
          unavailable <- c(unavailable, e)
        } else {
          add(base, "/table: table '", node[["table"]], "' has no event role record at this site")
        }
        next
      }
      cols <- role[["columns"]]
      filterable <- c(cols[["column_name"]][cols[["filterable"]]], "value", "unit_concept_id",
                      "start_date", "end_date", "concept_id", "range_low", "range_high")
      if (!is.null(node[["where"]])) {
        .irWalkPredicate(node[["where"]], function(l, lp) {
          if (!l[["var"]] %in% filterable) add(base, "/where", lp, ": column '", l[["var"]], "' is not filterable on '", node[["table"]], "'")
        }, "")
      }
      if (!is.null(node[["concept_col"]]) && !node[["concept_col"]] %in% cols[["column_name"]][cols[["filterable"]]]) {
        add(base, "/concept_col: '", node[["concept_col"]], "' is not a filterable column of '", node[["table"]], "'")
      }
      if (!is.null(node[["value"]][["column"]]) && !node[["value"]][["column"]] %in% cols[["column_name"]][cols[["filterable"]]]) {
        add(base, "/value/column: '", node[["value"]][["column"]], "' is not a filterable column of '", node[["table"]], "'")
      }
      if (!is.null(node[["end"]][["fallback"]]) && is.null(role[["supply"]])) {
        add(base, "/end/fallback: table '", node[["table"]], "' has no days_supply role")
      }
    }
    if (!is.null(node[["episode_members"]])) {
      fields <- .irEpisodeEventFields()
      for (t in unlist(node[["episode_members"]][["tables"]] %||% list())) {
        if (is.null(.roleOf(handle, t)) || !t %in% fields[["table_name"]]) {
          add(base, "/episode_members/tables: '", t, "' has no role record or reviewed episode_event field mapping")
        }
      }
    }
    if (!is.null(node[["cost"]])) {
      parent <- ir[["events"]][[node[["cost"]][["event"]][["event"]]]]
      domains <- .irCostDomains()
      if (is.null(parent[["table"]]) || !parent[["table"]] %in% domains[["table_name"]]) {
        add(base, "/cost/event: the cost source must reference a table event with a reviewed cost_domain_id mapping")
      }
    }
  }
  for (v in names(ir[["variables"]])) {
    raw <- ir[["variables"]][[v]][["raw"]]
    if (is.list(raw)) {
      col_df <- bp[["columns"]][[raw[["table"]]]]
      if (is.null(col_df) || !raw[["column"]] %in% col_df[["column_name"]]) {
        add("/variables/", v, "/raw: column '", raw[["column"]], "' does not exist on '", raw[["table"]], "'")
      } else if (identical(.roleColumnClass(raw[["column"]], col_df), 1L)) {
        add("/variables/", v, "/raw: column '", raw[["column"]], "' is never released (class 1)")
      }
    }
  }
  list(errors = errors, unavailable = unavailable)
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

#' Dialect preflight of an IR (design sections 6.1, 6.3 and 9)
#'
#' Calls WS-3's \code{.sqlDialectPreflight(h, .sqlPlanConstructs(ops),
#' session_tables = ...)} (\code{R/sql_expr.R}): once per output, over the
#' part of the plan the output depends on (\code{.irOutputClosure()}), so that
#' the typed \code{dialect_unsupported} rejection names the output, then once
#' for the whole plan, which refuses whatever no single output accounts for.
#' The database is probed once. A plan with populations is taken to reuse a
#' roster in more than one statement (session temp tables), the conservative
#' reading until the compilers decide where rosters are materialised (design
#' section 6.4). A lowered v1 plan is executed by the v1 executors in this
#' release stage and needs exactly what they need, so no typed construct is
#' required of it (v1 temp tables keep their own refusal).
#'
#' @param ir The IR (semantically valid).
#' @param handle CDM handle (or context / profile) to preflight against.
#' @return The probed dialect profile, invisibly.
#' @keywords internal
.irDialectPreflight <- function(ir, handle) {
  profile <- .sqlDialectProfile(handle, probe = TRUE)
  if (identical(attr(ir, "source_version"), 1L)) {
    return(invisible(.sqlDialectPreflight(profile, constructs = character(0),
                                          session_tables = FALSE)))
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
#' @param ir The IR (schema-valid).
#' @param handle CDM handle, or NULL for the pure semantic rules only.
#' @return Invisibly, a list(operations, unavailable, site_findings,
#'   dialect); stops with a typed rejection on the first failing stage.
#' @keywords internal
.irValidate <- function(ir, handle = NULL) {
  errors <- .irSemanticErrors(ir)
  if (length(errors)) .irRejectAll("plan fails the semantic rules", errors)
  ops <- .irOperations(ir)
  disabled <- intersect(ops, .irDisabledOperations())
  if (length(disabled)) {
    .irReject(paste0("operation(s) disabled by this deployment: ",
                     paste(disabled, collapse = ", ")))
  }
  report <- list(operations = ops, unavailable = character(0),
                 site_findings = character(0), dialect = NULL)
  if (is.null(handle)) return(invisible(report))
  site <- .irSiteErrors(ir, handle)
  report[["unavailable"]] <- site[["unavailable"]]
  report[["site_findings"]] <- site[["errors"]]
  legacy <- identical(attr(ir, "source_version"), 1L)
  if (length(site[["errors"]]) && !legacy) {
    .irRejectAll("plan fails the site checks", site[["errors"]])
  }
  report[["dialect"]] <- .irDialectPreflight(ir, handle)
  invisible(report)
}
