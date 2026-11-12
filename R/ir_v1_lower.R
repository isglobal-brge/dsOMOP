# Module: lowering of v1 plans to the compatibility IR (design-2.10.3
# section 5, amendment A-10). R/ir_v1.R runs the closed envelope first: what
# reaches .irLowerV1() is a valid v1 plan, whose every result-affecting field
# is written into the IR document -- natively, or as one of the closed IR-only
# compatibility variants of ir-v2.json -- so the one IR validator, explain and
# the one shared semantic digest see it. Advisory fields (no effect on any
# result) go to the server-private annex instead (attribute `legacy_annex`,
# listed by explain, covered by no digest, read by no execution decision).
#
# Mapping (one row per v1 element):
#
#   cohort {cohort_table, id}       scope.cohort = id; base = index on the
#                                   results cohort, legacy_episodes cohort_rows
#   cohort {spec}                   base = index on the closed legacy_spec
#                                   (select all, no coverage)
#   cohort.filter_tree              base = criteria (legacy leaves), the
#                                   persons' observation periods
#                                   (legacy_episodes observation_periods),
#                                   legacy_index_anchor scope_cohort
#   populations[*]                  same ids (A-10 name allocation): criteria
#                                   (observation periods, plan-level index),
#                                   index (index_event, legacy_end_strategy) or
#                                   setop (a one-member set operation is its
#                                   member; an n-ary difference a left fold of
#                                   binary differences)
#   person_level                    wide, grain person: raw person/death
#                                   variables, feature specs as reducers over
#                                   synthesised events, derived_columns;
#                                   output-level custom filter on raw tables:
#                                   legacy_raw_filter (CR-16)
#   event_level long                long over one event; projection
#                                   legacy_columns, temporal legacy_temporal
#   event_level wide / sparse       the legacy automatic builder
#                                   (legacy_automatic_features)
#   event_level features            reducers per feature spec (the output
#                                   concept set as legacy_concept_scope), or the
#                                   automatic feature builder without specs
#   baseline                        wide, grain episode: raw person variables,
#                                   age bands, observed days of the containing
#                                   period (legacy_containing_period)
#   survival                        survival, legacy_cohort_end_cap; the
#                                   single-outcome shape legacy_single_outcome;
#                                   multi-state graph from the v1 normaliser
#                                   (legacy_states, transitions in trans order,
#                                   materialised hierarchy, legacy_state_step)
#   cohort_membership / dictionary  membership / dictionary
#   intervals_long                  intervals over one event per table
#   temporal_covariates / person_period
#                                   same formats, legacy_composite and
#                                   legacy_covariate_ids
#   row filter leaf value_bin       and[>= lower, < upper]
#
# Precedence of duplicated v1 spellings follows the executors exactly
# (event_level: filters$concept_set$ids, filters$concept_col and
# filters$visit win over the output-level fields; time_window and the
# temporal calendar both apply).
#
# What the compatibility variants mean to a v1 adapter (A-10):
#   legacy_episodes "cohort_rows": the population's rows are its results
#     cohort's rows, without coverage;
#   legacy_episodes "observation_periods": the rows are the observation
#     periods of the population's persons;
#   legacy_index_anchor "scope_cohort": criteria leaves anchor on the v1
#     plan-level index cohort -- the scope cohort when the plan has one (a
#     results-cohort plan cohort is folded into it), else the plan cohort's
#     own rows;
#   a lowered set operation keeps the v1 row algebra: union = the distinct
#     member rows, intersect / setdiff = the left member's rows of the persons
#     kept; under a scope its rows are the scope cohort's rows of its persons.

.V1_LOWERING_GENERATOR <- "dsOMOP v1 lowering"

# --- state, annex and names ---------------------------------------------------

.v1State <- function(plan) {
  st <- new.env(parent = emptyenv())
  st[["plan"]] <- plan
  for (m in c("concept_sets", "events", "populations", "outputs")) st[[m]] <- list()
  st[["requests"]] <- list()
  st[["annex"]] <- list()
  st[["names"]] <- list()
  st
}

# Record advisory v1 data (A-10 annex) under an IR path.
.v1Annex <- function(st, path, ...) {
  items <- list(...)
  items <- items[!vapply(items, is.null, logical(1))]
  if (!length(items)) return(invisible(NULL))
  current <- st[["annex"]][[path]] %||% list()
  for (nm in names(items)) current[[nm]] <- items[[nm]]
  st[["annex"]][[path]] <- current
  invisible(NULL)
}

.v1Hash <- function(raw) {
  substr(paste(as.character(openssl::sha256(charToRaw(enc2utf8(raw)))), collapse = ""), 1L, 10L)
}
.v1Stem <- function(raw) {
  stem <- gsub("[^A-Za-z0-9_]", "_", raw)
  if (!grepl("^[A-Za-z]", stem)) stem <- paste0("n", stem)
  stem
}
# The k-th A-10 candidate for a raw name: <stem>_<10 hex>, then _2, _3, ...,
# the stem shortened so that hash and suffix fit the 64-character grammar.
.v1Candidate <- function(raw, k = 1L) {
  suffix <- paste0("_", .v1Hash(raw), if (k > 1L) paste0("_", k))
  paste0(substr(.v1Stem(raw), 1L, 64L - nchar(suffix)), suffix)
}

#' Allocate IR names for the v1 names of one namespace (amendment A-10)
#'
#' Unchanged names (already in the 2.0 grammar) are reserved first; the others
#' are allocated in declaration order without colliding with reserved,
#' already allocated or \code{taken} names.
#' @param raws v1 names in declaration order (unique).
#' @param taken Names already used in the namespace.
#' @return Named character vector, v1 name -> IR name.
#' @keywords internal
.v1AllocateNames <- function(raws, taken = character(0)) {
  raws <- as.character(raws)
  out <- stats::setNames(rep(NA_character_, length(raws)), raws)
  valid <- grepl(.V1_NAME_PATTERN, raws)
  out[valid] <- raws[valid]
  used <- c(taken, raws[valid])
  for (i in which(!valid)) {
    k <- 1L
    repeat {
      cand <- .v1Candidate(raws[[i]], k)
      if (!cand %in% used) break
      k <- k + 1L
    }
    out[[i]] <- cand
    used <- c(used, cand)
  }
  out
}

# A synthesised IR name (not a v1 name) unique in `used`.
.v1Fresh <- function(used, wanted) {
  base <- if (grepl(.V1_NAME_PATTERN, wanted)) wanted else .v1Candidate(wanted)
  name <- base
  k <- 1L
  while (name %in% used) {
    k <- k + 1L
    suffix <- paste0("_", k)
    name <- paste0(substr(base, 1L, 64L - nchar(suffix)), suffix)
  }
  name
}

# Remember an IR name -> original v1 name (legacy_names).
.v1RecordName <- function(st, kind, ir, raw, output = NULL) {
  if (identical(ir, raw)) return(invisible(NULL))
  map <- st[["names"]]
  if (is.null(output)) {
    map[[kind]][[ir]] <- raw
  } else {
    if (is.null(map[[kind]][[output]])) map[[kind]][[output]] <- list()
    map[[kind]][[output]][[ir]] <- raw
  }
  st[["names"]] <- map
  invisible(NULL)
}

# --- values ----------------------------------------------------------------------

# The executors' integer / number coercion (.v1AsInteger(), .v1AsNumber()) as
# validated compatibility normalisation: a value the closed envelope accepted
# lowers to the number 2.10.2 read ("9000" -> 9000L); anything else is kept
# as it is (and is refused by the IR schema or the envelope).
.v1Int <- function(x) {
  x <- .v1Scalar(x)
  .v1AsInteger(x) %||% x
}
.v1Num <- function(x) {
  x <- .v1Scalar(x)
  number <- .v1AsNumber(x)
  if (is.null(number)) return(x)
  if (number == round(number) && abs(number) < .Machine$integer.max) as.integer(number) else number
}
.v1CohortInteger <- function(x) .v1AsCohortInteger(x) %||% x

.v1IdList <- function(x) {
  if (is.null(x)) return(list())
  lapply(unname(as.list(unlist(x, use.names = FALSE))), .v1Int)
}

# v1 concept set (ids or the {concepts, include_descendants, include_mapped,
# exclude} object), resolved as the event streams resolve it -> IR inline set.
.v1ConceptSet <- function(x) {
  if (is.null(x)) return(NULL)
  if (.v1IsObject(x) && length(x) > 0L) {
    out <- list(concepts = .v1IdList(x[["concepts"]]))
    for (flag in c("include_descendants", "include_mapped")) {
      if (!is.null(x[[flag]])) out[[flag]] <- isTRUE(.v1Scalar(x[[flag]]))
    }
    if (!is.null(x[["exclude"]])) out[["exclude"]] <- .v1IdList(x[["exclude"]])
    return(out)
  }
  list(concepts = .v1IdList(x))
}

# The restriction a person_level feature table's own concept_set applies: the
# executor passes it unresolved to .compileSelect(), which reads
# unlist(concept_filter) -- an object's flags become ids 1/0, its exclusions
# are included and no descendant is expanded. The IR states exactly that set.
.v1UnresolvedConceptSet <- function(x) {
  if (is.null(x)) return(NULL)
  ids <- suppressWarnings(as.integer(unlist(x, use.names = FALSE)))
  list(concepts = as.list(unique(ids[!is.na(ids)])))
}

# Intern an IR concept set under a synthesised name; identical sets share one.
.v1InternConceptSet <- function(st, cs, wanted) {
  key <- .jcsCanonical(cs)
  for (nm in names(st[["concept_sets"]])) {
    if (identical(.jcsCanonical(st[["concept_sets"]][[nm]]), key)) return(nm)
  }
  name <- .v1Fresh(names(st[["concept_sets"]]), wanted)
  st[["concept_sets"]][[name]] <- cs
  name
}

# v1 date_handling (mode string or object; NULL = the server default) ->
# the closed 2.0 object.
.v1DateHandling <- function(x) {
  if (is.null(x)) x <- getOption("dsomop.default_date_handling", "remove")
  if (is.character(x) && length(x) == 1L) x <- list(mode = x)
  x <- .v1Compact(x)
  mode <- tolower(.v1Text(x[["mode"]]) %||% "")
  if (identical(mode, "relative_to_index")) mode <- "relative"
  out <- list(mode = mode, reference = tolower(.v1Text(x[["reference"]]) %||% "index"))
  out["bin_width"] <- list(if (!is.null(x[["bin_width"]])) tolower(.v1Text(x[["bin_width"]]) %||% "") else NULL)
  cols <- x[["date_columns"]]
  out["date_columns"] <- list(if (is.null(cols)) NULL else lapply(.v1Items(cols), tolower))
  out
}

.V1_OP_ALIASES <- c(eq = "==", ne = "!=", gte = ">=", lte = "<=", gt = ">", lt = "<")

# v1 row filter (custom DSL) -> 2.0 predicate. A value_bin's safe scope is an
# authentication artefact of the bin contract (annex).
.v1Predicate <- function(x, st = NULL, path = NULL) {
  if (is.null(x) || (is.list(x) && length(x) == 0L)) return(NULL)
  x <- .v1Compact(x)
  if (identical(names(x), "not")) {
    return(list(not = .v1Predicate(x[["not"]], st = st, path = path)))
  }
  group <- intersect(names(x), c("and", "or"))
  if (length(group) == 1L && length(x) == 1L) {
    items <- lapply(.v1Items(x[[group]]), .v1Predicate, st = st, path = path)
    items <- items[!vapply(items, is.null, logical(1))]
    out <- list(items)
    names(out) <- group
    return(out)
  }
  op <- tolower(.v1Text(x[["op"]]) %||% "")
  if (op %in% names(.V1_OP_ALIASES)) op <- .V1_OP_ALIASES[[op]]
  var <- tolower(.v1Text(x[["var"]]) %||% "")
  if (!is.null(x[["safe_scope"]]) && !is.null(st)) {
    .v1Annex(st, path, value_bin_safe_scope = x[["safe_scope"]])
  }
  # an integer string on a concept column is the concept id every database
  # compared it as (A-19 item 7: validated compatibility normalisation)
  literal <- function(v) .v1ConceptLiteral(var, .v1Scalar(v))
  switch(op,
    # the half-open bin [lower, upper) the executor compiles (lower <= x <
    # upper), one leaf as in v1, so v1 cap accounting holds (re-review R3)
    value_bin = list(var = var, op = "between",
                     value = list(lower = .v1Num(x[["value"]][["lower"]]),
                                  upper = .v1Num(x[["value"]][["upper"]]),
                                  upper_inclusive = FALSE)),
    between = {
      values <- unlist(x[["value"]], use.names = FALSE)
      list(var = var, op = "between",
           value = list(lower = if (length(values) >= 1L) literal(values[[1L]]),
                        upper = if (length(values) >= 2L) literal(values[[2L]]),
                        upper_inclusive = TRUE))
    },
    `in` =, not_in = list(var = var, op = op, value = lapply(.v1Items(x[["value"]]), literal)),
    is_null =, not_null = list(var = var, op = op),
    list(var = var, op = op, value = literal(x[["value"]])))
}

# A v1 literal on a *_concept_id column (the executors' concept-column rule):
# an integer in decimal notation, quoted or not, is that concept id.
.v1ConceptLiteral <- function(var, v) {
  if (!grepl("_concept_id$", var) || !is.character(v) || length(v) != 1L || is.na(v) ||
      !grepl("^\\s*[+-]?[0-9]+\\s*$", v)) {
    return(v)
  }
  number <- as.numeric(v)
  if (abs(number) <= .Machine$integer.max) as.integer(number) else number
}

.v1And <- function(...) {
  parts <- list(...)
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0L) return(NULL)
  if (length(parts) == 1L) return(parts[[1L]])
  list(and = parts)
}

# v1 population filter tree -> 2.0 criteria with legacy leaves.
.v1Criteria <- function(x, st = NULL, path = NULL) {
  if (is.null(x)) return(NULL)
  if (is.null(names(x))) return(list(and = lapply(x, .v1Criteria, st = st, path = path)))
  if ("type" %in% names(x)) {
    type <- tolower(.v1Text(x[["type"]]) %||% "")
    params <- .v1Compact(x[["params"]] %||% structure(list(), names = character(0)))
    if (!is.null(params[["concept_name"]]) && !is.null(st)) {
      seen <- st[["annex"]][[path]][["concept_names"]] %||% list()
      .v1Annex(st, path, concept_names = c(seen, list(params[["concept_name"]])))
    }
    params[["concept_name"]] <- NULL
    for (pair in list(c("concept_id", "concept_ids"), c("visit_concept_id", "visit_concept_ids"))) {
      single <- params[[pair[[1L]]]]
      if (is.list(single) && is.null(names(single))) {
        if (length(single) == 1L) {
          params[[pair[[1L]]]] <- .v1Int(single[[1L]])
        } else if (is.null(params[[pair[[2L]]]])) {
          params[[pair[[1L]]]] <- NULL
          params[[pair[[2L]]]] <- lapply(single, .v1Int)
        }
      }
      multi <- params[[pair[[2L]]]]
      if (!is.null(multi)) params[[pair[[2L]]]] <- lapply(.v1Items(multi), .v1Int)
    }
    # visit_count with an empty concept list counts every visit. The native
    # legacy leaf states that by omitting its optional concept restriction.
    if (identical(type, "visit_count")) {
      for (field in c("visit_concept_id", "visit_concept_ids")) {
        if (!length(unlist(params[[field]], use.names = FALSE))) params[[field]] <- NULL
      }
    }
    if (!is.null(params[["groups"]])) {
      params[["groups"]] <- lapply(.v1Items(params[["groups"]]), function(g) trimws(as.character(g)))
    }
    for (k in intersect(names(params), c("min", "max", "min_count", "min_days",
                                         "cohort_definition_id", "concept_id", "visit_concept_id"))) {
      params[[k]] <- .v1Int(params[[k]])
    }
    for (k in intersect(names(params), c("min_value", "max_value"))) params[[k]] <- .v1Num(params[[k]])
    if (is.list(params[["window"]])) params[["window"]] <- lapply(.v1Compact(params[["window"]]), .v1Int)
    if (!is.null(params[["safe_scope"]])) params[["safe_scope"]] <- .v1SafeScope(params[["safe_scope"]])
    if (length(params) == 0L) params <- structure(list(), names = character(0))
    return(list(legacy = list(type = type, params = params)))
  }
  if (identical(names(x), "not")) {
    return(list(not = .v1Criteria(x[["not"]], st = st, path = path)))
  }
  group <- intersect(names(x), c("and", "or"))
  out <- list(lapply(.v1Items(x[[group]]), .v1Criteria, st = st, path = path))
  names(out) <- group
  out
}

# The domain concept column of a CDM table from the packaged role records
# (handle-free: the lowering is pure), or NULL.
.v1DomainConceptColumn <- function(table) {
  table <- tolower(.v1Text(table) %||% "")
  for (version in rev(.ROLE_VERSIONS)) {
    rows <- .roleRecordsCsv(version)
    hit <- rows[["domain_concept_col"]][rows[["table_name"]] == table]
    if (length(hit) && nzchar(hit[[1L]])) return(hit[[1L]])
  }
  NULL
}

# A synthesised table event; equal definitions share one node.
.v1Event <- function(st, base_name, table, concept_set = NULL, where = NULL,
                     concept_col = NULL, visit = NULL, concept_scope = NULL) {
  node <- list(table = tolower(.v1Text(table) %||% ""))
  node["concept_set"] <- list(concept_set)
  node["concept_col"] <- list(if (!is.null(concept_col)) tolower(.v1Text(concept_col) %||% concept_col))
  node["where"] <- list(where)
  node["visit"] <- list(visit)
  if (!is.null(concept_scope)) node[["legacy_concept_scope"]] <- list(concept_set_ref = concept_scope)
  key <- .jcsCanonical(node)
  for (nm in names(st[["events"]])) {
    if (identical(.jcsCanonical(st[["events"]][[nm]]), key)) return(nm)
  }
  name <- .v1Fresh(names(st[["events"]]), base_name)
  st[["events"]][[name]] <- node
  name
}

# A requested output column: the v1 alias and the IR variable definition;
# IR variable names are allocated once every output is lowered.
.v1Column <- function(alias, def) list(alias = as.character(alias), def = def)

# v1 visit filter {concept_ids | visit_concept_id} -> 2.0 visit restriction.
.v1Visit <- function(x) {
  if (is.null(x) || (is.list(x) && length(x) == 0L)) return(NULL)
  list(type = .v1IdList(x[["concept_ids"]] %||% x[["visit_concept_id"]]))
}

# v1 window {start, end} (index-relative days) -> 2.0 window.
.v1Window <- function(x) {
  if (is.null(x)) return(NULL)
  x <- .v1Compact(x)
  out <- list()
  out["start"] <- list(.v1Int(x[["start"]]))
  out["end"] <- list(.v1Int(x[["end"]]))
  out[["anchor"]] <- "index"
  out
}

# v1 event_level temporal policy beyond the calendar (which lowers to event
# predicates) -> the closed legacy_temporal variant, defaults materialised as
# A-10 states (absent bounds null; event_select n = 1, by = grain; min_gap by =
# concept, keep = first; a scalar min_gap in its object form).
.v1LegacyTemporal <- function(temporal) {
  if (!.v1IsObject(temporal)) return(NULL)
  out <- list()
  iw <- temporal[["index_window"]]
  if (.v1IsObject(iw)) {
    w <- list()
    w["start"] <- list(.v1Int(iw[["start"]]))
    w["end"] <- list(.v1Int(iw[["end"]]))
    out[["index_window"]] <- w
  }
  es <- temporal[["event_select"]]
  if (.v1IsObject(es)) {
    out[["event_select"]] <- list(order = tolower(.v1Text(es[["order"]]) %||% ""),
                                  n = .v1Int(es[["n"]] %||% 1L),
                                  by = tolower(.v1Text(es[["by"]]) %||% "grain"))
  }
  mg <- temporal[["min_gap"]]
  if (!is.null(mg)) {
    if (!.v1IsObject(mg)) mg <- list(days = mg)
    out[["min_gap"]] <- list(days = .v1Int(mg[["days"]]),
                             by = tolower(.v1Text(mg[["by"]]) %||% "concept"),
                             keep = tolower(.v1Text(mg[["keep"]]) %||% "first"))
  }
  if (length(out)) out else NULL
}

.V1_REDUCERS <- c(
  boolean = "binary", count = "count", n_distinct = "n_distinct",
  mean_value = "mean", min_value = "min", max_value = "max",
  sum_value = "sum", sd_value = "sd", cv_value = "cv",
  slope_value = "slope", first_value = "first_value",
  latest_value = "last_value", time_since = "time_since",
  drug_duration = "drug_duration", abnormal_high = "abnormal_high",
  abnormal_low = "abnormal_low", gap_max_days = "gap_max",
  gap_mean_days = "gap_mean", duration_sum = "duration_sum"
)
.V1_VALUE_REDUCERS <- c("mean_value", "min_value", "max_value", "first_value",
                        "latest_value", "sum_value", "sd_value", "cv_value",
                        "slope_value")

# Feature specs (list, possibly named) over one stream -> reducer columns.
# stream: list(concept_set = the stream's own restriction (IR set or NULL),
# concept_col, visit, where, window).
.v1FeatureColumns <- function(st, output, table, specs, stream) {
  out <- list()
  nm <- names(specs)
  for (i in seq_along(specs)) {
    spec <- .v1Compact(specs[[i]])
    key <- if (!is.null(nm) && nzchar(nm[[i]])) nm[[i]] else NULL
    fname <- .v1Text(spec[["name"]]) %||% key
    if (is.null(fname) || !nzchar(fname)) {
      ids <- unlist(.v1ConceptSet(spec[["concept_set"]])[["concepts"]] %||% list())
      fname <- if (length(ids)) paste0("c", paste(ids, collapse = "_")) else paste0("feature_", i)
    }
    type <- .v1Text(spec[["type"]]) %||% "boolean"
    path <- paste0("outputs/", output)
    # The spec's own concept set selects within the stream's restriction
    # (their intersection: legacy_concept_scope); without one the event is
    # the stream itself.
    own <- .v1ConceptSet(spec[["concept_set"]])
    scope <- if (!is.null(own) && !is.null(stream[["concept_set"]])) {
      .v1InternConceptSet(st, stream[["concept_set"]], paste0("cs_", output))
    }
    ev <- .v1Event(st, paste0("ev_", output, "_", fname), table,
                   concept_set = own %||% stream[["concept_set"]],
                   where = .v1And(stream[["where"]], .v1Predicate(spec[["filter"]], st, path)),
                   concept_col = spec[["concept_col"]] %||% stream[["concept_col"]],
                   visit = stream[["visit"]], concept_scope = scope)
    reducer <- list(fn = unname(.V1_REDUCERS[[type]]))
    if (type %in% .V1_VALUE_REDUCERS || !is.null(spec[["value_column"]])) {
      reducer[["value"]] <- tolower(.v1Text(spec[["value_column"]]) %||% "value_as_number")
    }
    window <- spec[["time_window"]] %||% stream[["window"]]
    if (!is.null(window)) reducer[["window"]] <- .v1Window(window)
    if (!is.null(spec[["reference_date"]])) reducer[["reference_date"]] <- .v1Text(spec[["reference_date"]])
    if (!is.null(spec[["unit"]])) reducer[["unit"]] <- tolower(.v1Text(spec[["unit"]]) %||% "")
    if (!is.null(spec[["agg"]])) reducer[["agg"]] <- tolower(.v1Text(spec[["agg"]]) %||% "")
    out <- c(out, list(.v1Column(fname, list(source = list(event = ev), reducer = reducer))))
  }
  out
}

.V1_DERIVED_KINDS <- c("age", "sex_mf", "obs_duration", "prior_obs",
                       "followup", "demo_missingness", "charlson", "chads2",
                       "chadsvasc", "dcsi", "hfrs")

# v1 derived_columns -> derived columns.
.v1DerivedColumns <- function(specs) {
  specs <- if (.v1IsObject(specs) && length(specs) > 0L && all(vapply(specs, is.list, logical(1)))) {
    unname(specs)
  } else {
    .v1Items(specs)
  }
  out <- list()
  for (spec in specs) {
    spec <- .v1Compact(spec)
    kind <- .v1Text(spec[["kind"]]) %||% ""
    name <- .v1Text(spec[["name"]]) %||% kind
    d <- switch(kind,
      age = {
        ref <- .v1Text(spec[["reference"]]) %||% "today"
        rd <- .v1Text(spec[["reference_date"]])
        node <- list(kind = "age", reference = if (!is.null(rd)) "today" else ref)
        node["reference_date"] <- list(rd)
        node
      },
      sex_mf = list(kind = "sex_mf"),
      demo_missingness = list(kind = "demo_missingness"),
      obs_duration = list(kind = "obs_duration", period_policy = .v1Text(spec[["period_policy"]]) %||% "total"),
      prior_obs =, followup = {
        node <- list(kind = kind)
        node["reference_date"] <- list(.v1Text(spec[["reference_date"]]))
        node[["period_policy"]] <- .v1Text(spec[["period_policy"]]) %||% "containing"
        node
      },
      charlson =, dcsi =, hfrs =, chads2 =, chadsvasc = {
        node <- list(kind = kind)
        node["reference_date"] <- list(.v1Text(spec[["reference_date"]]))
        node[["legacy_all_time_conditions"]] <- TRUE
        node
      })
    out <- c(out, list(.v1Column(name, list(derived = d))))
  }
  out
}

# v1 raw column spec (array of names, or alias -> name object).
.v1ColumnSpec <- function(entry) {
  if (is.null(entry)) return(list(source = character(0), alias = character(0)))
  if (is.character(entry) && length(entry) == 1L) entry <- list(entry)
  if (is.list(entry) && !is.null(names(entry))) {
    src <- vapply(entry, function(v) as.character(.v1Scalar(v)), "")
    return(list(source = unname(src), alias = names(entry)))
  }
  src <- vapply(entry, function(v) as.character(.v1Scalar(v)), "")
  list(source = src, alias = src)
}

.V1_IDENTITY_COLUMNS <- c("person_id", "subject_id", "row_id", "cohort_row_id")

# Raw columns of person or death (the person_level/baseline raw tables);
# identity columns are implicit in every v1 frame (annex).
.v1RawColumns <- function(st, output, table, entry) {
  spec <- .v1ColumnSpec(entry)
  out <- list()
  for (i in seq_along(spec[["source"]])) {
    col <- tolower(spec[["source"]][[i]])
    alias <- spec[["alias"]][[i]]
    if (col %in% .V1_IDENTITY_COLUMNS) {
      seen <- st[["annex"]][[paste0("outputs/", output)]][["identity_columns"]]
      .v1Annex(st, paste0("outputs/", output), identity_columns = unique(c(seen, alias)))
      next
    }
    out <- c(out, list(.v1Column(alias, list(raw = list(table = table, column = col)))))
  }
  out
}

# --- outputs -----------------------------------------------------------------------

.v1LowerOutput <- function(st, name, raw_name, out, rows) {
  type <- .v1Text(out[["type"]]) %||% "event_level"
  path <- paste0("outputs/", name)
  out <- .v1Compact(out)
  filters <- if (.v1IsObject(out[["filters"]])) .v1Compact(out[["filters"]]) else list()
  custom <- .v1Predicate(filters[["custom"]], st, path)
  if (!is.null(out[["options"]]) && length(out[["options"]]) > 0L) {
    .v1Annex(st, path, options = out[["options"]])
  }
  if (!identical(type, "event_level")) {
    ignored <- filters[setdiff(names(filters), "custom")]
    if (length(ignored)) .v1Annex(st, path, ignored_filters = ignored)
  }
  node <- list(rows = rows)
  columns <- NULL
  switch(type,
    person_level = {
      node[["grain"]] <- "person"
      node[["format"]] <- "wide"
      columns <- list()
      tables <- out[["tables"]] %||% list()
      has_raw <- FALSE
      for (tbl in names(tables)) {
        entry <- tables[[tbl]]
        tbl_l <- tolower(tbl)
        if (.v1IsObject(entry) && !is.null(entry[["features"]])) {
          entry <- .v1Compact(entry)
          stream <- list(concept_set = .v1UnresolvedConceptSet(entry[["concept_set"]]),
                         concept_col = entry[["concept_col"]], visit = .v1Visit(entry[["visit"]]),
                         where = .v1And(.v1Predicate(entry[["filters"]], st, path), custom))
          columns <- c(columns, .v1FeatureColumns(st, name, tbl_l, entry[["features"]], stream))
        } else {
          has_raw <- TRUE
          columns <- c(columns, .v1RawColumns(st, name, tbl_l, entry))
        }
      }
      # the output-level custom filter also restricts each raw table (CR-16)
      if (has_raw && !is.null(custom)) node[["legacy_raw_filter"]] <- custom
      if (!is.null(out[["derived_columns"]])) columns <- c(columns, .v1DerivedColumns(out[["derived_columns"]]))
      if (!is.null(out[["representation"]])) .v1Annex(st, path, representation = out[["representation"]])
    },
    event_level = {
      rep <- if (.v1IsObject(out[["representation"]])) .v1Compact(out[["representation"]]) else list()
      fmt <- tolower(.v1Text(rep[["format"]]) %||% "long")
      grain <- tolower(.v1Text(rep[["grain"]]) %||% "person")
      # the executor's precedence (review B3): filters$... over the output fields
      cs <- .v1ConceptSet(filters[["concept_set"]][["ids"]] %||% out[["concept_set"]])
      concept_col <- filters[["concept_col"]] %||% out[["concept_col"]]
      # the extraction's precedence (.compileSelect()): an override column that
      # is also requested or filtered on is a surfacing choice -- the concept
      # set then scopes the table's domain concept column (A-19 item 7)
      cc <- tolower(.v1Text(concept_col) %||% "")
      domain <- .v1DomainConceptColumn(out[["table"]])
      if (nzchar(cc) && !is.null(domain) && !identical(cc, domain) &&
          (cc %in% tolower(as.character(unlist(out[["columns"]], use.names = FALSE))) ||
           .filterTreeReferencesColumn(filters[["custom"]], cc))) {
        .v1Annex(st, path, surfacing_concept_col = cc)
        concept_col <- NULL
      }
      visit <- .v1Visit(filters[["visit"]] %||% out[["visit_filter"]])
      if (!is.null(filters[["concept_set"]][["ids"]]) && !is.null(out[["concept_set"]])) {
        .v1Annex(st, path, overridden_concept_set = out[["concept_set"]])
      }
      tw <- filters[["time_window"]]
      calendar <- NULL
      if (.v1IsObject(tw) && length(tw) > 0L) {
        tw <- .v1Compact(tw)
        var <- tolower(.v1Text(tw[["date_column"]]) %||% "start_date")
        calendar <- .v1And(
          if (!is.null(tw[["start_date"]])) list(var = var, op = ">=", value = .v1Text(tw[["start_date"]])),
          if (!is.null(tw[["end_date"]])) list(var = var, op = "<=", value = .v1Text(tw[["end_date"]])))
      }
      temporal <- if (.v1IsObject(out[["temporal"]])) out[["temporal"]] else NULL
      if (.v1IsObject(temporal[["calendar"]])) {
        cal <- .v1Compact(temporal[["calendar"]])
        calendar <- .v1And(calendar,
          if (!is.null(cal[["start"]])) list(var = "start_date", op = ">=", value = .v1Text(cal[["start"]])),
          if (!is.null(cal[["end"]])) list(var = "start_date", op = "<=", value = .v1Text(cal[["end"]])))
      }
      where <- .v1And(custom, calendar)
      lt <- .v1LegacyTemporal(temporal)
      if (!is.null(lt)) node[["legacy_temporal"]] <- lt
      if (!is.null(out[["columns"]])) {
        node[["legacy_columns"]] <- as.list(unique(tolower(vapply(.v1Items(out[["columns"]]), function(x) as.character(.v1Scalar(x)), ""))))
      }
      stream_event <- function() {
        .v1Event(st, paste0("ev_", name), out[["table"]], concept_set = cs, where = where,
                 concept_col = concept_col, visit = visit)
      }
      date_handling <- .v1DateHandling(out[["date_handling"]])
      specs <- rep[["features"]]
      if (identical(fmt, "long")) {
        node[["format"]] <- "long"
        node[["sources"]] <- list(list(event = stream_event()))
        node[["options"]] <- list(date_handling = date_handling)
      } else if (identical(fmt, "features") && length(specs) > 0L) {
        node[["format"]] <- "wide"
        node[["grain"]] <- grain
        node[["options"]] <- list(date_handling = date_handling)
        stream <- list(concept_set = cs, concept_col = concept_col, visit = visit, where = where,
                       window = temporal[["index_window"]])
        columns <- .v1FeatureColumns(st, name, tolower(.v1Text(out[["table"]]) %||% ""), specs, stream)
      } else {
        # the legacy automatic builders: wide (which never reads feature
        # specs), features without specs, sparse (binary, count and value
        # statistics per observed concept; feature specs unread)
        node[["format"]] <- if (identical(fmt, "sparse")) "sparse" else "wide"
        node[["grain"]] <- grain
        ev <- stream_event()
        node[["legacy_automatic_features"]] <- list(format = fmt, source = list(event = ev))
        if (identical(fmt, "sparse")) {
          node[["options"]] <- list(sources = list(list(event = ev)))
        } else {
          node[["options"]] <- list(date_handling = date_handling)
          columns <- list()
        }
        if (length(specs)) .v1Annex(st, path, unread_feature_specs = specs)
      }
    },
    baseline = {
      node[["grain"]] <- "episode"
      node[["format"]] <- "wide"
      # baseline reads the column values (tolower(columns)): names are ignored
      requested <- out[["columns"]] %||% list("gender_concept_id", "race_concept_id")
      if (.v1IsObject(requested) && length(requested)) {
        .v1Annex(st, path, unread_column_names = names(requested))
        requested <- unname(requested)
      }
      columns <- .v1RawColumns(st, name, "person", requested)
      derived <- tolower(unlist(out[["derived"]] %||% list(), use.names = FALSE))
      if ("age_at_index" %in% derived) {
        age <- list(kind = "age", reference = "index", reference_date = NULL, bands = TRUE)
        age["age_breaks"] <- list(if (!is.null(out[["age_breaks"]])) .v1IdList(out[["age_breaks"]]))
        columns <- c(columns, list(.v1Column("age_group", list(derived = age))))
      }
      if ("prior_observation" %in% derived) {
        columns <- c(columns, list(.v1Column("prior_observation", list(derived = list(
          kind = "observed_days", window = list(start = NULL, end = -1L, anchor = "index"),
          legacy_containing_period = TRUE)))))
      }
      if ("future_observation" %in% derived) {
        columns <- c(columns, list(.v1Column("future_observation", list(derived = list(
          kind = "observed_days", window = list(start = 1L, end = NULL, anchor = "index"),
          legacy_containing_period = TRUE)))))
      }
    },
    survival = {
      node[["format"]] <- "survival"
      single <- is.null(out[["outcomes"]])
      if (!single && !is.null(out[["outcome"]])) .v1Annex(st, path, unread_outcome = out[["outcome"]])
      outcomes <- if (single) list(outcome = out[["outcome"]]) else out[["outcomes"]]
      fmt <- tolower(.v1Text(out[["format"]]) %||% "survival")
      multi_state <- identical(fmt, "multi_state")
      graph <- if (multi_state) {
        .normalizeMultistateSpec(lapply(names(outcomes), function(n) list(name = n)),
                                 transitions = out[["transitions"]],
                                 initial_state = .v1Text(out[["initial_state"]]),
                                 state_hierarchy = out[["state_hierarchy"]],
                                 state_step = .v1Scalar(out[["state_step"]]))
      }
      # one namespace per output for outcomes and states: an outcome and its
      # multi-state state share one encoded name
      raws <- unique(c(if (multi_state) graph[["transitions"]][["states"]], names(outcomes)))
      alloc <- .v1AllocateNames(raws)
      for (r in raws) {
        if (r %in% names(outcomes)) .v1RecordName(st, "outcomes", alloc[[r]], r, output = name)
        if (multi_state) .v1RecordName(st, "states", alloc[[r]], r, output = name)
      }
      opts <- list(outcomes = list())
      for (oname in names(outcomes)) {
        o <- .v1Compact(outcomes[[oname]] %||% list())
        key <- alloc[[oname]]
        ev <- .v1Event(st, paste0("ev_", name, "_", key), o[["table"]],
                       concept_set = .v1ConceptSet(o[["concept_set"]]),
                       where = .v1And(.v1Predicate(o[["filters"]], st, path), custom))
        opts[["outcomes"]][[key]] <- list(event = ev)
      }
      tar <- .v1Compact(out[["tar"]] %||% list())
      opts[["tar"]] <- list(start = .v1Int(tar[["start_offset"]] %||% 0L), end = .v1Int(tar[["end_offset"]]),
                            anchor = "index")
      censoring <- out[["censoring"]]
      if (single && is.null(censoring)) censoring <- list(observation_period_end = TRUE, death = FALSE)
      censoring <- .v1Compact(censoring %||% list())
      censor <- list()
      if (!isFALSE(.v1Scalar(censoring[["observation_period_end"]]))) censor <- c(censor, list("observation_end"))
      if (!isFALSE(.v1Scalar(censoring[["death"]]))) censor <- c(censor, list("death"))
      if (!is.null(censoring[["admin_date"]])) censor <- c(censor, list(list(admin_date = .v1Text(censoring[["admin_date"]]))))
      if (!length(censor)) censor <- list("index_end")
      opts[["censor"]] <- censor
      opts[["format"]] <- fmt
      if (!is.null(out[["event_order"]])) opts[["event_order"]] <- tolower(.v1Text(out[["event_order"]]))
      if (!is.null(out[["washout_days"]])) opts[["washout_days"]] <- .v1Int(out[["washout_days"]])
      if (!is.null(out[["tie_policy"]])) opts[["tie_policy"]] <- tolower(.v1Text(out[["tie_policy"]]))
      if (multi_state) {
        opts[["initial_state"]] <- alloc[[graph[["initial_state"]]]]
        opts[["legacy_states"]] <- as.list(unname(alloc[graph[["transitions"]][["states"]]]))
        edges <- graph[["transitions"]][["edges"]]
        edges <- edges[order(vapply(edges, function(e) as.integer(e[["trans"]]), integer(1)))]
        opts[["transitions"]] <- lapply(edges, function(e) list(from = alloc[[e[["from"]]]], to = alloc[[e[["to"]]]]))
        opts[["state_hierarchy"]] <- as.list(unname(alloc[graph[["state_hierarchy"]]]))
        opts[["legacy_state_step"]] <- graph[["state_step"]]
        if (!is.null(out[["transitions"]])) .v1Annex(st, path, transition_syntax = out[["transitions"]])
      } else {
        unread <- out[intersect(names(out), c("transitions", "initial_state", "state_hierarchy", "state_step"))]
        if (length(unread)) .v1Annex(st, path, unread_graph_options = unread)
      }
      opts[["legacy_cohort_end_cap"]] <- TRUE
      if (single) opts[["legacy_single_outcome"]] <- TRUE
      node[["options"]] <- opts
    },
    cohort_membership = {
      node[["format"]] <- "membership"
      node[["options"]] <- list(date_handling = .v1DateHandling(out[["date_handling"]]))
    },
    concept_dictionary = {
      node[["format"]] <- "dictionary"
      sources <- out[["source_outputs"]]
      if (is.null(sources)) {
        others <- setdiff(names(st[["plan"]][["outputs"]]), raw_name)
        types <- vapply(st[["plan"]][["outputs"]][others], function(o) .v1Text(o[["type"]]) %||% "event_level", "")
        sources <- others[types != "concept_dictionary"]
        .v1Annex(st, path, source_outputs = "all")
      }
      map <- st[["output_names"]]
      node[["options"]] <- list(source_outputs = lapply(.v1Items(sources), function(s) {
        raw <- .v1Text(s) %||% ""
        if (!is.null(map) && raw %in% names(map)) map[[raw]] else raw
      }))
    },
    intervals_long = {
      node[["format"]] <- "intervals"
      tables <- lapply(.v1Items(out[["tables"]]), function(t) tolower(.v1Text(t) %||% ""))
      cf <- out[["concept_filter"]] %||% list()
      sf <- out[["source_filters"]] %||% list()
      names(cf) <- tolower(names(cf) %||% character(0))
      names(sf) <- tolower(names(sf) %||% character(0))
      sources <- lapply(tables, function(t) {
        list(event = .v1Event(st, paste0("ev_", name, "_", t), t,
                              concept_set = .v1ConceptSet(cf[[t]]),
                              where = .v1And(.v1Predicate(sf[[t]], st, path), custom)))
      })
      opts <- list(sources = sources)
      window <- if (.v1IsObject(out[["window"]])) .v1Compact(out[["window"]]) else NULL
      if (!is.null(window[["at"]])) {
        # {at: x} under active_at is the window [x, x]
        opts[["window"]] <- list(start = .v1Int(window[["at"]]), end = .v1Int(window[["at"]]), anchor = "index")
        .v1Annex(st, path, window_syntax = out[["window"]])
      } else if (length(window)) {
        opts[["window"]] <- .v1Window(window)
      }
      for (k in c("interval_match", "event_select", "select_by")) {
        if (!is.null(out[[k]])) opts[[k]] <- tolower(.v1Text(out[[k]]) %||% "")
      }
      if (!is.null(out[["select_n"]])) opts[["select_n"]] <- .v1Int(out[["select_n"]])
      opts[["anchor"]] <- "index"
      anchor <- .v1Int(out[["anchor"]] %||% 0L)
      # the v1 offset in days from the index orders event_select = nearest
      # only (A-16): a zero offset is the native anchor, an offset under any
      # other selection is ignored by the executor and stays advisory
      if (!identical(anchor, 0L)) {
        if (identical(opts[["event_select"]], "nearest")) {
          opts[["legacy_anchor_offset"]] <- anchor
        } else {
          .v1Annex(st, path, ignored_anchor_offset = anchor)
        }
      }
      node[["options"]] <- opts
    },
    temporal_covariates =, person_period = {
      node[["format"]] <- type
      ev <- .v1Event(st, paste0("ev_", name), out[["table"]], concept_set = .v1ConceptSet(out[["concept_set"]]),
                     where = custom)
      window <- list(start = .v1Int(out[["window_start"]] %||% -365L),
                     end = .v1Int(out[["window_end"]] %||% 0L), anchor = "index")
      analyses <- lapply(.v1Items(out[["analyses"]] %||% list("binary")), function(a) tolower(.v1Text(a) %||% ""))
      opts <- if (identical(type, "temporal_covariates")) {
        list(sources = list(list(event = ev)), bin_width = .v1Int(out[["bin_width"]] %||% 30L),
             window = window, analyses = analyses)
      } else {
        list(representation = "sparse", sources = list(list(event = ev)),
             bins = list(width = .v1Int(out[["bin_width"]] %||% 30L)), window = window,
             analyses = analyses)
      }
      opts[["legacy_composite"]] <- TRUE
      opts[["legacy_covariate_ids"]] <- TRUE
      if (identical(type, "person_period")) node[["grain"]] <- "episode"
      node[["options"]] <- opts
    },
    .irReject(paste0("Unsupported output type '", type, "'."),
              output = raw_name, field = paste0("/outputs/", raw_name, "/type"))
  )
  if (!is.null(columns)) st[["requests"]][[name]] <- columns
  node
}

# Allocate the IR variables of every output's requested columns (A-10):
# unchanged aliases are reserved first, each for its first definition in
# declaration order; an alias whose name is outside the grammar, or taken by
# another definition, gets the first free <stem>_<10 hex>[_k]; identical
# definitions of one alias share one variable.
.v1AllocateVariables <- function(st) {
  owner <- list()
  for (o in names(st[["requests"]])) {
    for (r in st[["requests"]][[o]]) {
      if (grepl(.V1_NAME_PATTERN, r[["alias"]]) && is.null(owner[[r[["alias"]]]])) {
        owner[[r[["alias"]]]] <- .jcsCanonical(r[["def"]])
      }
    }
  }
  variables <- list()
  for (o in names(st[["requests"]])) {
    cols <- character(0)
    for (r in st[["requests"]][[o]]) {
      key <- .jcsCanonical(r[["def"]])
      alias <- r[["alias"]]
      name <- if (grepl(.V1_NAME_PATTERN, alias) && identical(owner[[alias]], key)) alias else {
        k <- 1L
        repeat {
          cand <- .v1Candidate(alias, k)
          if (is.null(owner[[cand]])) {
            owner[[cand]] <- key
            break
          }
          if (identical(owner[[cand]], key)) break
          k <- k + 1L
        }
        cand
      }
      if (name %in% cols) next
      variables[[name]] <- r[["def"]]
      cols <- c(cols, name)
      .v1RecordName(st, "columns", name, alias, output = o)
    }
    st[["outputs"]][[o]][["columns"]] <- as.list(cols)
  }
  variables
}

# --- populations -------------------------------------------------------------------

.v1LowerIndexEvent <- function(st, pid, ie) {
  ie <- .v1Compact(ie)
  ev <- .v1Event(st, paste0("ev_", pid, "_index"), ie[["table"]], concept_set = .v1ConceptSet(ie[["concept_set"]]))
  index <- list(source = list(event = ev),
                select = tolower(.v1Text(ie[["primary_limit"]]) %||% "first"),
                restrict_to_observation = TRUE)
  es <- .normalizeIndexEventEndStrategy(ie[["end_strategy"]])
  if (!is.null(es)) {
    index[["legacy_end_strategy"]] <- list(DateOffset = list(
      DateField = es[["DateOffset"]][["DateField"]], Offset = as.integer(es[["DateOffset"]][["Offset"]])))
  }
  index
}

.v1LowerPopulation <- function(st, id, pid, pop, anchored) {
  pop <- .v1Compact(pop)
  path <- paste0("populations/", id)
  label <- .v1Text(pop[["label"]])
  if (!is.null(pop[["id"]]) && !identical(.v1Text(pop[["id"]]), pid)) .v1Annex(st, path, id = pop[["id"]])
  if (!is.null(pop[["filters"]])) .v1Annex(st, path, filters = pop[["filters"]])
  pop_name <- function(m) st[["population_names"]][[m]] %||% m
  kind <- tolower(.v1Text(pop[["kind"]]) %||% "criteria")
  node <- if (identical(kind, "setop") || !is.null(pop[["setop"]])) {
    op <- tolower(.v1Text(pop[["setop"]][["op"]]) %||% "union")
    if (identical(op, "difference")) op <- "setdiff"
    members <- vapply(.v1Items(pop[["setop"]][["members"]]), function(m) pop_name(.v1Text(m) %||% ""), "")
    # v1 union/intersect of a population with itself is that population
    if (!identical(op, "setdiff")) members <- unique(members)
    if (length(members) == 1L) {
      # a one-member set operation is its member: same roster, rows and dates
      # (review B11); its own id keeps the output namespace
      list(kind = "criteria", parent = members[[1L]])
    } else if (identical(op, "setdiff") && length(members) > 2L) {
      # the v1 left fold: ((m1 \ m2) \ m3) \ ...
      left <- members[[1L]]
      for (k in seq_len(length(members) - 2L)) {
        fold <- .v1Fresh(c(names(st[["populations"]]), unlist(st[["population_names"]])), paste0(id, "_fold", k))
        st[["populations"]][[fold]] <- list(kind = "setop", setop = list(op = "setdiff", members = list(left, members[[k + 1L]])))
        left <- fold
      }
      list(kind = "setop", setop = list(op = "setdiff", members = list(left, members[[length(members)]])))
    } else {
      list(kind = "setop", setop = list(op = op, members = as.list(members)))
    }
  } else if (!is.null(pop[["index_event"]])) {
    node <- list(kind = "index", index = .v1LowerIndexEvent(st, id, pop[["index_event"]]))
    node["where"] <- list(.v1And(
      if (!is.null(pop[["cohort_definition_id"]])) list(cohort = list(cohort_definition_id = .v1CohortInteger(pop[["cohort_definition_id"]]))),
      .v1Criteria(pop[["filter_tree"]], st, path)))
    node
  } else if (!identical(pid, "base") && is.null(pop[["filter_tree"]]) &&
             is.null(pop[["cohort_definition_id"]])) {
    # a non-base v1 criteria population with neither filter_tree nor cohort id
    # resolves to no person: an empty observation-period roster (CR-21), a
    # private empty intermediate 2.10.2 executed (re-review R3)
    if (!is.null(pop[["episode_policy"]])) .v1Annex(st, path, unread_episode_policy = pop[["episode_policy"]])
    list(kind = "criteria", legacy_episodes = "observation_periods", legacy_empty = TRUE)
  } else {
    node <- list(kind = "criteria")
    node["where"] <- list(.v1Criteria(pop[["filter_tree"]], st, path))
    node["cohort_definition_id"] <- list(.v1CohortInteger(pop[["cohort_definition_id"]]))
    node["episode_policy"] <- list(if (!is.null(pop[["episode_policy"]])) tolower(.v1Text(pop[["episode_policy"]])))
    # v1 criteria populations are the observation periods of their persons;
    # their windowed leaves anchor on the plan-level index cohort
    if (!is.null(pop[["filter_tree"]]) || !is.null(pop[["cohort_definition_id"]])) {
      node[["legacy_episodes"]] <- "observation_periods"
      if (!is.null(pop[["filter_tree"]]) && anchored) node[["legacy_index_anchor"]] <- "scope_cohort"
    }
    node
  }
  if (!is.null(label)) node <- c(list(kind = node[["kind"]], label = label), node[setdiff(names(node), "kind")])
  st[["populations"]][[id]] <- node
  id
}

# The base population a v1 plan cohort defines (or NULL when there is none).
.v1PlanCohortPopulation <- function(st, cohort, id, scoped) {
  cohort <- .v1Compact(cohort)
  path <- paste0("populations/", id)
  type <- .v1Text(cohort[["type"]])
  node <- NULL
  if (!is.null(cohort[["filter_tree"]])) {
    node <- list(kind = "criteria", label = "Base population")
    node["where"] <- list(.v1Criteria(cohort[["filter_tree"]], st, path))
    node["episode_policy"] <- list(if (!is.null(cohort[["episode_policy"]])) tolower(.v1Text(cohort[["episode_policy"]])))
    node[["legacy_episodes"]] <- "observation_periods"
    # its windowed leaves anchor on the scope cohort (a results-cohort plan
    # cohort is folded into the scope); without a scope they have no index
    if (scoped) node[["legacy_index_anchor"]] <- "scope_cohort"
    if (!is.null(cohort[["spec"]])) .v1Annex(st, path, unread_spec = cohort[["spec"]])
  } else if (!is.null(cohort[["spec"]])) {
    spec <- .v1Compact(cohort[["spec"]])
    ls <- list()
    if (!is.null(spec[["type"]])) ls[["type"]] <- tolower(.v1Text(spec[["type"]]))
    if (!is.null(spec[["concept_set"]])) ls[["concept_set"]] <- .v1ConceptSet(spec[["concept_set"]])
    if (!is.null(spec[["value_bin"]])) {
      vb <- .v1Compact(spec[["value_bin"]])
      ls[["value_bin"]] <- list(lower = .v1Num(vb[["lower"]]), upper = .v1Num(vb[["upper"]]),
                                safe_scope = .v1SafeScope(vb[["safe_scope"]]))
    }
    if (!is.null(spec[["inclusion_criteria"]])) ls[["inclusion_criteria"]] <- .v1Inclusion(spec[["inclusion_criteria"]])
    # the plan cohort id of an inline spec names its table and fills the
    # cohort_membership id column (A-16): present only when supplied, any
    # non-negative integer, never defaulted
    if (!is.null(cohort[["cohort_definition_id"]])) ls[["cohort_definition_id"]] <- .v1CohortInteger(cohort[["cohort_definition_id"]])
    if (length(ls) == 0L) ls <- structure(list(), names = character(0))
    node <- list(kind = "index", label = "Base population",
                 index = list(source = list(legacy_spec = ls), select = "all",
                              restrict_to_observation = FALSE),
                 coverage = list(on_missing = "keep"))
    if (!is.null(cohort[["episode_policy"]])) .v1Annex(st, path, unread_episode_policy = cohort[["episode_policy"]])
  } else if (identical(type, "cohort_table") && !is.null(cohort[["cohort_definition_id"]])) {
    node <- list(kind = "index", label = "Base population",
                 index = list(source = list(cohort_definition_id = .v1CohortInteger(cohort[["cohort_definition_id"]])),
                              select = "all", end = list(strategy = "event_end", days = NULL, pad = 0L),
                              restrict_to_observation = FALSE),
                 coverage = list(on_missing = "keep"),
                 legacy_episodes = "cohort_rows")
    if (!is.null(cohort[["episode_policy"]])) .v1Annex(st, path, unread_episode_policy = cohort[["episode_policy"]])
  }
  if (is.null(node)) return(NULL)
  st[["populations"]][[id]] <- node
  id
}

.v1SafeScope <- function(x) {
  if (is.null(x)) return(NULL)
  out <- .v1Compact(x)
  if (!is.null(out[["n_bins"]])) out[["n_bins"]] <- .v1Int(out[["n_bins"]])
  if (!is.null(out[["concept_id"]])) {
    ids <- .v1Items(out[["concept_id"]])
    out[["concept_id"]] <- if (length(ids) == 1L) .v1Int(ids[[1L]]) else lapply(ids, .v1Int)
  }
  out
}

# Inline-spec inclusion criteria -> the closed legacy variant (the historical
# occurrence object {type, count}, review B2).
.v1Inclusion <- function(crits) {
  nm <- names(crits)
  out <- list()
  for (i in seq_along(crits)) {
    key <- if (is.null(nm) || !nzchar(nm[[i]])) paste0("criterion_", i) else .v1Fresh(names(out), nm[[i]])
    crit <- .v1Compact(crits[[i]])
    if (!is.null(crit[["concept_set"]])) crit[["concept_set"]] <- .v1ConceptSet(crit[["concept_set"]])
    if (!is.null(crit[["table"]])) crit[["table"]] <- tolower(.v1Text(crit[["table"]]))
    if (.v1IsObject(crit[["temporal"]])) {
      t <- .v1Compact(crit[["temporal"]])
      if (!is.null(t[["index_window"]])) t[["index_window"]] <- lapply(.v1Compact(t[["index_window"]]), .v1Int)
      if (!is.null(t[["calendar"]])) t[["calendar"]] <- lapply(.v1Compact(t[["calendar"]]), .v1Text)
      crit[["temporal"]] <- t
    }
    # A-15: the effective occurrence object, type kept as the exact text the
    # executor matches, count an integer; omitted members and an omitted
    # occurrence are the defaults (at_least, 1), so omitted and explicit
    # defaults share one normal form and one digest
    occ <- if (.v1IsObject(crit[["occurrence"]])) .v1Compact(crit[["occurrence"]]) else list()
    crit[["occurrence"]] <- list(type = .v1Text(occ[["type"]]) %||% "at_least",
                                 count = .v1Int(occ[["count"]] %||% 1L))
    out[[key]] <- crit
  }
  if (length(out) == 0L) structure(list(), names = character(0)) else out
}

# --- the plan ---------------------------------------------------------------------

#' Lower a v1 plan to the compatibility IR (design section 5, amendment A-10)
#'
#' Pure: no database access. Defaults are expanded afterwards by
#' \code{.planNormalizeV2()} (amendment A-11). Every result-affecting v1 field
#' is in the returned IR document; the advisory ones are attached as attribute
#' \code{legacy_annex} (a named list keyed by IR path). An envelope finding
#' (a plan that did not pass \code{.planV1Envelope()}) is carried as the
#' \code{v1_envelope_errors} marker, which no IR schema admits, so a direct
#' call can never turn an invalid plan into a valid IR.
#'
#' @param plan v1 plan in parsed JSON form, through \code{.planV1Envelope()}.
#' @param scope_tables Number of workspace scope tables the server resolved for
#'   this execution (they never travel in the plan).
#' @param validated TRUE when the caller already ran the closed envelope.
#' @return The lowered IR (before default expansion).
#' @keywords internal
.irLowerV1 <- function(plan, scope_tables = 0L, validated = FALSE) {
  envelope <- if (validated) character(0) else .planV1EnvelopeErrors(plan)
  if (length(envelope)) {
    # an invalid plan is never lowered: it is kept intact, unknown properties
    # included, beside the findings, so no schema can admit it
    invalid <- plan
    invalid[["version"]] <- 2L
    invalid[["v1_envelope_errors"]] <- as.list(envelope)
    return(invalid)
  }
  st <- .v1State(plan)
  multi <- isTRUE(tryCatch(.planHasMultiPopulation(plan), error = function(e) FALSE))
  pops <- if (.v1IsObject(plan[["populations"]])) .v1Compact(plan[["populations"]]) else list()
  cohort <- if (.v1IsObject(plan[["cohort"]]) && length(plan[["cohort"]]) > 0L) .v1Compact(plan[["cohort"]]) else NULL
  scope <- if (.v1IsObject(plan[["scope"]])) .v1Compact(plan[["scope"]]) else list()

  # Root advisory fields.
  if (!is.null(plan[["anchor"]])) .v1Annex(st, "root", anchor = plan[["anchor"]])
  if (!is.null(plan[["harmonization"]])) .v1Annex(st, "root", harmonization = plan[["harmonization"]])

  # Names of the outputs and the declared populations (A-10 allocation).
  outputs <- if (.v1IsObject(plan[["outputs"]])) plan[["outputs"]] else list()
  out_names <- .v1AllocateNames(names(outputs))
  st[["output_names"]] <- out_names
  for (raw in names(out_names)) .v1RecordName(st, "outputs", out_names[[raw]], raw)
  declared <- if (multi) names(pops) else character(0)
  pop_names <- .v1AllocateNames(declared)
  st[["population_names"]] <- as.list(pop_names)
  for (raw in names(pop_names)) .v1RecordName(st, "populations", pop_names[[raw]], raw)

  # Populations: the plan cohort's base, then every declared population.
  base_has_own <- multi && "base" %in% names(pops) && {
    b <- .v1Compact(pops[["base"]])
    any(c("filter_tree", "cohort_definition_id", "index_event", "setop") %in% names(b))
  }
  scoped <- !is.null(scope[["cohort"]]) || length(scope[["tables"]] %||% list()) > 0L ||
    scope_tables > 0L ||
    (identical(.v1Text(cohort[["type"]]), "cohort_table") && !is.null(cohort[["cohort_definition_id"]]))
  plan_base <- if (!is.null(cohort)) {
    target <- if (base_has_own) .v1Fresh(unname(pop_names), "v1_plan_cohort") else "base"
    .v1PlanCohortPopulation(st, cohort, target, scoped)
  }
  # multi-population criteria leaves anchor on the scope cohort, else the plan
  # cohort's own rows; with neither they have no index
  anchored <- scoped || !is.null(cohort)
  if (multi) {
    for (pid in names(pops)) {
      id <- pop_names[[pid]]
      if (identical(pid, "base") && !base_has_own && !is.null(plan_base)) {
        label <- .v1Text(.v1Compact(pops[["base"]])[["label"]])
        if (!is.null(label)) st[["populations"]][["base"]][["label"]] <- label
        next
      }
      .v1LowerPopulation(st, id, pid, pops[[pid]], anchored)
    }
  } else if (length(pops)) {
    .v1Annex(st, "root", populations_inactive = pops)
  }
  if (is.null(st[["populations"]][["base"]])) {
    st[["populations"]][["base"]] <- list(kind = "criteria", label = "Base population")
  }

  # Scope: the plan-level results cohort is a scope source (v1 folds it in).
  cohort_sources <- list()
  if (!is.null(scope[["cohort"]])) cohort_sources <- c(cohort_sources, .v1Items(scope[["cohort"]]))
  if (identical(.v1Text(cohort[["type"]]), "cohort_table") && !is.null(cohort[["cohort_definition_id"]])) {
    cohort_sources <- c(cohort_sources, list(.v1CohortInteger(cohort[["cohort_definition_id"]])))
  }
  # results-cohort ids stay integers, owned cohort temp-table names strings
  cohort_sources <- lapply(cohort_sources, function(s) {
    s <- .v1Scalar(s)
    if (is.character(s)) s else .v1CohortInteger(s)
  })
  ir_scope <- list()
  ir_scope["cohort"] <- list(if (length(cohort_sources)) cohort_sources[[1L]])
  # every source v1 folds into one scope cohort (CR-16)
  if (length(cohort_sources) > 1L) ir_scope[["legacy_cohort_sources"]] <- cohort_sources
  tables <- lapply(.v1Items(scope[["tables"]]), function(t) .v1Text(t) %||% t)
  if (!length(tables) && scope_tables > 0L) tables <- as.list(paste0("scope_table_", seq_len(scope_tables)))
  ir_scope[["tables"]] <- tables
  ir_scope[["combine"]] <- tolower(.v1Text(scope[["combine"]]) %||% "union")

  # Outputs.
  for (raw in names(outputs)) {
    out <- outputs[[raw]]
    key <- out_names[[raw]]
    pid <- .v1Text(.v1Compact(out)[["population_id"]]) %||% "base"
    rows <- if (multi) pop_names[[pid]] %||% pid else "base"
    if (!multi && !identical(pid, "base")) .v1Annex(st, paste0("outputs/", key), population_id = pid)
    st[["outputs"]][[key]] <- .v1LowerOutput(st, key, raw, out, rows)
  }
  variables <- .v1AllocateVariables(st)

  empty <- structure(list(), names = character(0))
  options <- if (.v1IsObject(plan[["options"]])) .v1Compact(plan[["options"]]) else empty
  if ("min_persons" %in% names(plan[["options"]])) {
    .v1Annex(st, "root", unread_options = plan[["options"]]["min_persons"])
    options[["min_persons"]] <- NULL
  }
  ir <- list(
    version = 2L,
    meta = list(language_version = .IR_LANGUAGE_VERSION,
                semantic_version = .IR_SEMANTIC_VERSION,
                generated_by = .V1_LOWERING_GENERATOR),
    options = if (length(options)) options else empty,
    concept_sets = if (length(st[["concept_sets"]])) st[["concept_sets"]] else empty,
    events = if (length(st[["events"]])) st[["events"]] else empty,
    intervals = empty,
    populations = st[["populations"]],
    variables = if (length(variables)) variables else empty,
    outputs = if (length(st[["outputs"]])) st[["outputs"]] else empty,
    scope = ir_scope
  )
  if (length(st[["names"]])) ir[["legacy_names"]] <- st[["names"]]
  attr(ir, "legacy_annex") <- if (length(st[["annex"]])) st[["annex"]] else NULL
  ir
}
