# Module: lowering of v1 plans to the compatibility IR (design-2.10.3
# section 5). See R/ir_v1.R for the closed envelope that runs first.
#
# Mapping (one row per v1 element; every v1 field without an exact language
# 2.0 / compatibility-variant equivalent is kept verbatim in the IR's
# `legacy_residue` attribute under the node's IR path):
#
#   cohort {cohort_table, id}            scope.cohort = id; the base population
#                                        is the cohort's rows (index on the
#                                        results cohort, select all, legacy
#                                        rows kept without coverage)
#   cohort {spec}                        populations.base: index on the closed
#                                        legacy_spec, select all,
#                                        restrict_to_observation = false,
#                                        coverage on_missing = keep
#   cohort.filter_tree / episode_policy  populations.base.where (legacy leaves)
#                                        and episode_policy
#   populations[*]                       same ids: criteria (where, results
#                                        cohort seed, episode_policy), index
#                                        (index_event -> index with
#                                        legacy_end_strategy) or setop
#   person_level                         wide, grain person: raw person/death
#                                        variables, feature specs as reducer
#                                        variables over synthesised events,
#                                        derived_columns as derived variables
#   event_level long                     long over one synthesised event
#   event_level wide/features/sparse     wide (grain kept) / sparse
#   baseline                             wide, grain episode: raw person
#                                        variables, age bands (age_group,
#                                        requested age_breaks), observed days
#   survival                             survival with legacy_cohort_end_cap
#   cohort_membership / dictionary       membership / dictionary
#   intervals_long                       intervals over one event per table
#   temporal_covariates / person_period  same names, legacy_composite and
#                                        legacy_covariate_ids (person_period
#                                        lowers to the sparse representation)
#   row filter leaf value_bin            and[>= lower, < upper]

.V1_LOWERING_GENERATOR <- "dsOMOP v1 lowering"

# Lowering state: the IR maps being built plus the residue.
.v1State <- function(plan) {
  st <- new.env(parent = emptyenv())
  st[["plan"]] <- plan
  st[["events"]] <- list()
  st[["variables"]] <- list()
  st[["populations"]] <- list()
  st[["outputs"]] <- list()
  st[["residue"]] <- list()
  st[["names"]] <- list()
  st
}

# Record verbatim v1 fields that the IR does not express exactly.
.v1Residue <- function(st, path, ...) {
  items <- list(...)
  items <- items[!vapply(items, is.null, logical(1))]
  if (!length(items)) return(invisible(NULL))
  current <- st[["residue"]][[path]] %||% list()
  for (nm in names(items)) current[[nm]] <- items[[nm]]
  st[["residue"]][[path]] <- current
  invisible(NULL)
}

# Map a v1 identifier to a valid IR name, remembering renames.
.v1MapName <- function(st, kind, raw) {
  name <- .v1Name(raw)
  if (!identical(name, raw)) {
    map <- st[["names"]][[kind]] %||% list()
    map[[name]] <- raw
    st[["names"]][[kind]] <- map
  }
  name
}

.v1Int <- function(x) {
  x <- .v1Scalar(x)
  if (is.numeric(x) && length(x) == 1L && !is.na(x) && x == round(x)) as.integer(x) else x
}

.v1IdList <- function(x) {
  if (is.null(x)) return(list())
  lapply(unname(.v1Items(x)), .v1Int)
}

# v1 concept set (ids or the {concepts, include_descendants, include_mapped,
# exclude} object) -> IR inline concept set.
.v1ConceptSet <- function(x) {
  if (is.null(x)) return(NULL)
  if (.v1IsObject(x) && length(x) > 0L && !is.null(x[["concepts"]])) {
    out <- list(concepts = .v1IdList(x[["concepts"]]))
    for (flag in c("include_descendants", "include_mapped")) {
      if (!is.null(x[[flag]])) out[[flag]] <- .v1Scalar(x[[flag]])
    }
    if (!is.null(x[["exclude"]])) out[["exclude"]] <- .v1IdList(x[["exclude"]])
    return(out)
  }
  list(concepts = .v1IdList(x))
}

# v1 date_handling (mode string or object; NULL = the server default) ->
# the closed 2.0 object.
.v1DateHandling <- function(x) {
  if (is.null(x)) x <- getOption("dsomop.default_date_handling", "remove")
  if (is.character(x) && length(x) == 1L) x <- list(mode = x)
  if (!.v1IsObject(x)) return(x)
  x <- .v1Compact(x)
  mode <- tolower(.v1Text(x[["mode"]]) %||% "")
  if (identical(mode, "relative_to_index")) mode <- "relative"
  out <- list(mode = mode, reference = tolower(.v1Text(x[["reference"]]) %||% "index"))
  out["bin_width"] <- list(if (!is.null(x[["bin_width"]])) tolower(.v1Text(x[["bin_width"]]) %||% "") else NULL)
  cols <- x[["date_columns"]]
  out["date_columns"] <- list(if (is.null(cols)) NULL else lapply(.v1Items(cols), tolower))
  extra <- setdiff(names(x), c("mode", "reference", "bin_width", "date_columns"))
  if (length(extra)) out <- c(out, x[extra])
  out
}

.V1_OP_ALIASES <- c(eq = "==", ne = "!=", gte = ">=", lte = "<=", gt = ">",
                    lt = "<")

# v1 row filter (custom DSL) -> 2.0 predicate. Unknown fields stay in the
# leaf so the IR schema refuses them (never discarded).
.v1Predicate <- function(x, st = NULL, path = NULL) {
  if (is.null(x) || (is.list(x) && length(x) == 0L)) return(NULL)
  if (!.v1IsObject(x)) return(x)
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
  extra <- x[setdiff(names(x), c("var", "op", "value", "safe_scope"))]
  if (!is.null(x[["safe_scope"]]) && !is.null(st)) {
    .v1Residue(st, path, value_bin_safe_scope = x[["safe_scope"]])
  }
  leaf <- switch(op,
    value_bin = list(and = list(
      list(var = var, op = ">=", value = .v1Scalar(x[["value"]][["lower"]])),
      list(var = var, op = "<", value = .v1Scalar(x[["value"]][["upper"]])))),
    between = {
      values <- unlist(x[["value"]], use.names = FALSE)
      list(var = var, op = "between",
           value = list(lower = if (length(values) >= 1L) values[[1L]],
                        upper = if (length(values) >= 2L) values[[2L]],
                        upper_inclusive = TRUE))
    },
    `in` =, not_in = list(var = var, op = op,
                          value = lapply(.v1Items(x[["value"]]), .v1Scalar)),
    is_null =, not_null = list(var = var, op = op),
    list(var = var, op = op, value = .v1Scalar(x[["value"]]))
  )
  if (length(extra)) leaf <- c(leaf, extra)
  leaf
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
  if (!is.list(x) || length(x) == 0L) return(x)
  if (is.null(names(x))) {
    items <- lapply(x, .v1Criteria, st = st, path = path)
    return(list(and = items))
  }
  if ("type" %in% names(x)) {
    type <- tolower(.v1Text(x[["type"]]) %||% "")
    params <- .v1Compact(x[["params"]] %||% structure(list(), names = character(0)))
    if (!is.null(params[["concept_name"]]) && !is.null(st)) {
      names_seen <- st[["residue"]][[path]][["concept_names"]] %||% list()
      .v1Residue(st, path, concept_names = c(names_seen, list(params[["concept_name"]])))
    }
    params[["concept_name"]] <- NULL
    for (pair in list(c("concept_id", "concept_ids"),
                      c("visit_concept_id", "visit_concept_ids"))) {
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
      if (!is.null(multi) && !is.list(multi)) params[[pair[[2L]]]] <- list(.v1Int(multi))
    }
    if (!is.null(params[["groups"]])) {
      params[["groups"]] <- lapply(.v1Items(params[["groups"]]), function(g) trimws(as.character(g)))
    }
    for (k in intersect(names(params), c("min", "max", "min_count", "min_days",
                                         "cohort_definition_id", "concept_id",
                                         "visit_concept_id"))) {
      params[[k]] <- .v1Int(params[[k]])
    }
    if (is.list(params[["window"]])) {
      params[["window"]] <- lapply(.v1Compact(params[["window"]]), .v1Int)
    }
    if (length(params) == 0L) params <- structure(list(), names = character(0))
    return(list(legacy = list(type = type, params = params)))
  }
  if (identical(names(x), "not")) {
    return(list(not = .v1Criteria(x[["not"]], st = st, path = path)))
  }
  group <- intersect(names(x), c("and", "or"))
  if (length(group) == 1L && length(x) == 1L) {
    items <- lapply(.v1Items(x[[group]]), .v1Criteria, st = st, path = path)
    out <- list(items)
    names(out) <- group
    return(out)
  }
  x
}

# A synthesised table event; equal definitions share one node.
.v1Event <- function(st, base_name, table, concept_set = NULL,
                     where = NULL, concept_col = NULL, visit = NULL) {
  node <- list(table = tolower(.v1Text(table) %||% ""))
  node["concept_set"] <- list(.v1ConceptSet(concept_set))
  node["concept_col"] <- list(if (!is.null(concept_col)) tolower(.v1Text(concept_col) %||% concept_col))
  node["where"] <- list(where)
  node["visit"] <- list(visit)
  key <- .jcsCanonical(node)
  for (nm in names(st[["events"]])) {
    if (identical(.jcsCanonical(st[["events"]][[nm]]), key)) return(nm)
  }
  name <- .v1Name(base_name)
  while (name %in% names(st[["events"]])) name <- .v1Name(paste0(base_name, "_", length(st[["events"]]) + 1L))
  st[["events"]][[name]] <- node
  name
}

# Add an output-scoped variable; identical definitions share a name.
.v1Variable <- function(st, wanted, node, output) {
  name <- .v1Name(wanted)
  if (!is.null(st[["variables"]][[name]]) &&
      !identical(.jcsCanonical(st[["variables"]][[name]]), .jcsCanonical(node))) {
    alt <- .v1Name(paste0(output, "__", wanted))
    map <- st[["residue"]][[paste0("outputs/", output)]][["column_names"]] %||% list()
    map[[alt]] <- wanted
    .v1Residue(st, paste0("outputs/", output), column_names = map)
    name <- alt
  } else if (!identical(name, wanted)) {
    map <- st[["residue"]][[paste0("outputs/", output)]][["column_names"]] %||% list()
    map[[name]] <- wanted
    .v1Residue(st, paste0("outputs/", output), column_names = map)
  }
  st[["variables"]][[name]] <- node
  name
}

# v1 visit filter {concept_ids | visit_concept_id} -> 2.0 visit restriction.
.v1Visit <- function(x) {
  if (is.null(x) || (is.list(x) && length(x) == 0L)) return(NULL)
  ids <- x[["concept_ids"]] %||% x[["visit_concept_id"]]
  list(type = .v1IdList(ids))
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

# Feature specs (list, possibly named) -> reducer variables.
.v1FeatureVariables <- function(st, output, table, specs, stream) {
  names_out <- character(0)
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
    fn <- .V1_REDUCERS[type]
    path <- paste0("outputs/", output)
    ev <- .v1Event(st, paste0("ev_", output, "_", fname), table,
                   concept_set = spec[["concept_set"]] %||% stream[["concept_set"]],
                   where = .v1And(stream[["where"]], .v1Predicate(spec[["filter"]], st, path)),
                   concept_col = spec[["concept_col"]] %||% stream[["concept_col"]],
                   visit = stream[["visit"]])
    reducer <- list(fn = if (is.na(fn)) type else unname(fn))
    if (type %in% .V1_VALUE_REDUCERS || !is.null(spec[["value_column"]])) {
      reducer[["value"]] <- tolower(.v1Text(spec[["value_column"]]) %||% "value_as_number")
    }
    window <- spec[["time_window"]] %||% stream[["window"]]
    if (!is.null(window)) reducer[["window"]] <- .v1Window(window)
    if (!is.null(spec[["reference_date"]])) reducer[["reference_date"]] <- .v1Text(spec[["reference_date"]])
    if (!is.null(spec[["unit"]])) reducer[["unit"]] <- tolower(.v1Text(spec[["unit"]]) %||% "")
    if (!is.null(spec[["agg"]])) reducer[["agg"]] <- tolower(.v1Text(spec[["agg"]]) %||% "")
    if (!is.null(stream[["concept_set"]]) && !is.null(spec[["concept_set"]])) {
      .v1Residue(st, path, stream_concept_set = stream[["concept_set"]])
    }
    var <- .v1Variable(st, fname, list(source = list(event = ev), reducer = reducer), output)
    names_out <- c(names_out, var)
  }
  names_out
}

.V1_DERIVED_KINDS <- c("age", "sex_mf", "obs_duration", "prior_obs",
                       "followup", "demo_missingness", "charlson", "chads2",
                       "chadsvasc", "dcsi", "hfrs")

# v1 derived_columns -> derived variables.
.v1DerivedVariables <- function(st, output, specs) {
  out <- character(0)
  specs <- if (.v1IsArray(specs) || (.v1IsObject(specs) && length(specs) > 0L &&
                                     all(vapply(specs, is.list, logical(1))))) {
    unname(specs)
  } else {
    .v1Items(specs)
  }
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
      obs_duration = list(kind = "obs_duration",
                          period_policy = .v1Text(spec[["period_policy"]]) %||% "total"),
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
      },
      c(spec)
    )
    out <- c(out, .v1Variable(st, name, list(derived = d), output))
  }
  out
}

.V1_RAW_TABLES <- c("person", "death", "observation_period")

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

.v1RawVariables <- function(st, output, table, entry) {
  spec <- .v1ColumnSpec(entry)
  out <- character(0)
  for (i in seq_along(spec[["source"]])) {
    col <- tolower(spec[["source"]][[i]])
    alias <- spec[["alias"]][[i]]
    if (col %in% .V1_IDENTITY_COLUMNS) {
      .v1Residue(st, paste0("outputs/", output),
                 identity_columns = unique(c(st[["residue"]][[paste0("outputs/", output)]][["identity_columns"]], alias)))
      next
    }
    out <- c(out, .v1Variable(st, alias, list(raw = list(table = table, column = col)), output))
  }
  out
}


# Normalisation never discards an unknown field (design section 4.5): a v1
# field outside the closed envelope is carried into the IR node verbatim,
# where ir-v2.json refuses it, so invalid input cannot become valid.
.v1CarryUnknown <- function(node, obj, known) {
  if (!.v1IsObject(obj)) return(node)
  obj <- .v1Compact(obj)
  unknown <- setdiff(names(obj), known)
  if (length(unknown)) node[unknown] <- obj[unknown]
  node
}

# --- outputs -----------------------------------------------------------------------

.v1LowerOutput <- function(st, name, out, rows) {
  type <- .v1Text(out[["type"]]) %||% "event_level"
  path <- paste0("outputs/", name)
  out <- .v1Compact(out)
  filters <- if (.v1IsObject(out[["filters"]])) .v1Compact(out[["filters"]]) else list()
  custom <- .v1Predicate(filters[["custom"]], st, path)
  if (!is.null(out[["options"]]) && length(out[["options"]]) > 0L) {
    .v1Residue(st, path, options = out[["options"]])
  }
  node <- list(rows = rows)
  switch(type,
    person_level = {
      node[["grain"]] <- "person"
      node[["format"]] <- "wide"
      cols <- character(0)
      tables <- out[["tables"]] %||% list()
      for (tbl in names(tables)) {
        entry <- tables[[tbl]]
        tbl_l <- tolower(tbl)
        if (.v1IsObject(entry) && !is.null(entry[["features"]])) {
          entry <- .v1Compact(entry)
          stream <- list(concept_set = entry[["concept_set"]], concept_col = entry[["concept_col"]],
                         visit = .v1Visit(entry[["visit"]]),
                         where = .v1And(.v1Predicate(entry[["filters"]], st, path), custom))
          cols <- c(cols, .v1FeatureVariables(st, name, tbl_l, entry[["features"]], stream))
        } else if (tbl_l %in% .V1_RAW_TABLES) {
          cols <- c(cols, .v1RawVariables(st, name, tbl_l, entry))
          if (!is.null(custom)) .v1Residue(st, path, raw_table_filters = filters[["custom"]])
        } else {
          raw_tables <- st[["residue"]][[path]][["raw_tables"]] %||% list()
          raw_tables[[tbl]] <- entry
          .v1Residue(st, path, raw_tables = raw_tables)
        }
      }
      if (!is.null(out[["derived_columns"]])) {
        cols <- c(cols, .v1DerivedVariables(st, name, out[["derived_columns"]]))
      }
      if (!is.null(out[["representation"]])) .v1Residue(st, path, representation = out[["representation"]])
      node[["columns"]] <- as.list(cols)
    },
    event_level = {
      rep <- if (.v1IsObject(out[["representation"]])) .v1Compact(out[["representation"]]) else list()
      fmt <- tolower(.v1Text(rep[["format"]]) %||% "long")
      cs <- out[["concept_set"]] %||% filters[["concept_set"]][["ids"]]
      tw <- filters[["time_window"]]
      calendar <- NULL
      if (.v1IsObject(tw) && length(tw) > 0L) {
        tw <- .v1Compact(tw)
        var <- tolower(.v1Text(tw[["date_column"]]) %||% "start_date")
        calendar <- .v1And(
          if (!is.null(tw[["start_date"]])) list(var = var, op = ">=", value = .v1Text(tw[["start_date"]])),
          if (!is.null(tw[["end_date"]])) list(var = var, op = "<=", value = .v1Text(tw[["end_date"]])))
      }
      temporal <- if (.v1IsObject(out[["temporal"]])) .v1Compact(out[["temporal"]]) else NULL
      if (!is.null(temporal[["calendar"]])) {
        cal <- .v1Compact(temporal[["calendar"]])
        calendar <- .v1And(calendar,
          if (!is.null(cal[["start"]])) list(var = "start_date", op = ">=", value = .v1Text(cal[["start"]])),
          if (!is.null(cal[["end"]])) list(var = "start_date", op = "<=", value = .v1Text(cal[["end"]])))
      }
      stream <- list(concept_set = cs,
                     concept_col = filters[["concept_col"]] %||% out[["concept_col"]],
                     visit = .v1Visit(filters[["visit"]] %||% out[["visit_filter"]]),
                     where = .v1And(custom, calendar),
                     window = temporal[["index_window"]])
      if (!is.null(out[["columns"]])) .v1Residue(st, path, columns = out[["columns"]])
      residual_temporal <- temporal[setdiff(names(temporal), "calendar")]
      if (length(residual_temporal)) .v1Residue(st, path, temporal = residual_temporal)
      if (identical(fmt, "long")) {
        ev <- .v1Event(st, paste0("ev_", name), out[["table"]], concept_set = cs,
                       where = stream[["where"]], concept_col = stream[["concept_col"]],
                       visit = stream[["visit"]])
        node[["format"]] <- "long"
        node[["sources"]] <- list(list(event = ev))
        select <- "all"
        es <- if (.v1IsObject(temporal[["event_select"]])) .v1Compact(temporal[["event_select"]]) else NULL
        if (!is.null(es) && identical(as.integer(.v1Int(es[["n"]] %||% 1L)), 1L) &&
            identical(tolower(.v1Text(es[["by"]]) %||% "grain"), "grain")) {
          select <- tolower(.v1Text(es[["order"]]) %||% "all")
        }
        node[["options"]] <- list(date_handling = .v1DateHandling(out[["date_handling"]]),
                             temporal = list(event_select = select, min_gap = 0L))
      } else if (fmt %in% c("wide", "features")) {
        node[["grain"]] <- tolower(.v1Text(rep[["grain"]]) %||% "person")
        node[["format"]] <- "wide"
        specs <- rep[["features"]]
        if (length(specs) > 0L) {
          node[["columns"]] <- as.list(.v1FeatureVariables(st, name, tolower(.v1Text(out[["table"]]) %||% ""),
                                                      specs, stream))
        } else {
          ev <- .v1Event(st, paste0("ev_", name), out[["table"]], concept_set = cs,
                         where = stream[["where"]], concept_col = stream[["concept_col"]],
                         visit = stream[["visit"]])
          .v1Residue(st, path, automatic_features = list(format = fmt, event = ev))
          node["columns"] <- list(list())
        }
        if (!is.null(out[["date_handling"]])) .v1Residue(st, path, date_handling = out[["date_handling"]])
      } else if (identical(fmt, "sparse")) {
        ev <- .v1Event(st, paste0("ev_", name), out[["table"]], concept_set = cs,
                       where = stream[["where"]], concept_col = stream[["concept_col"]],
                       visit = stream[["visit"]])
        node[["grain"]] <- tolower(.v1Text(rep[["grain"]]) %||% "person")
        node[["format"]] <- "sparse"
        node[["options"]] <- list(sources = list(list(event = ev)), analyses = list("binary"))
        if (length(rep[["features"]])) .v1Residue(st, path, sparse_features = rep[["features"]])
      } else {
        node[["format"]] <- fmt
      }
    },
    baseline = {
      node[["grain"]] <- "episode"
      node[["format"]] <- "wide"
      columns <- out[["columns"]] %||% list("gender_concept_id", "race_concept_id")
      cols <- .v1RawVariables(st, name, "person", columns)
      derived <- tolower(unlist(out[["derived"]] %||% list(), use.names = FALSE))
      if ("age_at_index" %in% derived) {
        age <- list(kind = "age", reference = "index", reference_date = NULL,
                    bands = TRUE)
        age["age_breaks"] <- list(if (!is.null(out[["age_breaks"]])) lapply(.v1Items(out[["age_breaks"]]), .v1Int))
        cols <- c(cols, .v1Variable(st, "age_group", list(derived = age), name))
      }
      if ("prior_observation" %in% derived) {
        cols <- c(cols, .v1Variable(st, "prior_observation", list(derived = list(
          kind = "observed_days", window = list(start = NULL, end = -1L, anchor = "index"))), name))
      }
      if ("future_observation" %in% derived) {
        cols <- c(cols, .v1Variable(st, "future_observation", list(derived = list(
          kind = "observed_days", window = list(start = 1L, end = NULL, anchor = "index"))), name))
      }
      other <- setdiff(derived, c("age_at_index", "prior_observation", "future_observation"))
      if (length(other)) .v1Residue(st, path, derived = as.list(other))
      if (any(c("prior_observation", "future_observation") %in% derived)) {
        .v1Residue(st, path, legacy_baseline_observation = TRUE)
      }
      node[["columns"]] <- as.list(cols)
    },
    survival = {
      node[["format"]] <- "survival"
      legacy <- is.null(out[["outcomes"]])
      outcomes <- if (legacy) list(outcome = out[["outcome"]]) else out[["outcomes"]]
      opts <- list(outcomes = list())
      for (oname in names(outcomes)) {
        o <- .v1Compact(outcomes[[oname]] %||% list())
        key <- .v1MapName(st, "outcomes", oname)
        ev <- .v1Event(st, paste0("ev_", name, "_", key), o[["table"]],
                       concept_set = o[["concept_set"]],
                       where = .v1And(.v1Predicate(o[["filters"]], st, path), custom))
        opts[["outcomes"]][[key]] <- list(event = ev)
      }
      tar <- .v1Compact(out[["tar"]] %||% list())
      opts[["tar"]] <- list(start = .v1Int(tar[["start_offset"]] %||% 0L), end = .v1Int(tar[["end_offset"]]),
                       anchor = "index")
      censoring <- out[["censoring"]]
      if (legacy && is.null(censoring)) {
        censoring <- list(observation_period_end = TRUE, death = FALSE)
      }
      censoring <- .v1Compact(censoring %||% list())
      censor <- list()
      if (!isFALSE(.v1Scalar(censoring[["observation_period_end"]]))) censor <- c(censor, list("observation_end"))
      if (!isFALSE(.v1Scalar(censoring[["death"]]))) censor <- c(censor, list("death"))
      if (!is.null(censoring[["admin_date"]])) censor <- c(censor, list(list(admin_date = .v1Text(censoring[["admin_date"]]))))
      if (!length(censor)) censor <- list("index_end")
      opts[["censor"]] <- censor
      opts[["format"]] <- tolower(.v1Text(out[["format"]]) %||% "survival")
      if (!is.null(out[["event_order"]])) opts[["event_order"]] <- tolower(.v1Text(out[["event_order"]]))
      if (!is.null(out[["washout_days"]])) opts[["washout_days"]] <- .v1Int(out[["washout_days"]])
      if (!is.null(out[["tie_policy"]])) opts[["tie_policy"]] <- tolower(.v1Text(out[["tie_policy"]]))
      if (!is.null(out[["initial_state"]])) opts[["initial_state"]] <- .v1MapName(st, "states", .v1Text(out[["initial_state"]]) %||% "index")
      if (!is.null(out[["state_hierarchy"]])) {
        opts[["state_hierarchy"]] <- lapply(.v1Items(out[["state_hierarchy"]]), function(s) .v1MapName(st, "states", .v1Text(s)))
      }
      if (!is.null(out[["transitions"]])) {
        opts[["transitions"]] <- .v1Transitions(st, out[["transitions"]])
        .v1Residue(st, path, transitions = out[["transitions"]])
      }
      if (!is.null(out[["state_step"]])) {
        step <- .v1Scalar(out[["state_step"]])
        if (is.numeric(step) && step == round(step) && step >= 0) {
          opts[["state_step"]] <- as.integer(step)
        } else {
          .v1Residue(st, path, state_step = out[["state_step"]])
        }
      }
      opts[["legacy_cohort_end_cap"]] <- TRUE
      if (legacy) .v1Residue(st, path, legacy_single_outcome = TRUE)
      if (!is.null(censoring[["cohort_end"]])) .v1Residue(st, path, censoring_cohort_end = censoring[["cohort_end"]])
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
        others <- setdiff(names(st[["plan"]][["outputs"]]), name)
        dict_types <- vapply(st[["plan"]][["outputs"]][others], function(o) .v1Text(o[["type"]]) %||% "event_level", "")
        sources <- others[dict_types != "concept_dictionary"]
        .v1Residue(st, path, source_outputs = "all")
      }
      node[["options"]] <- list(source_outputs = lapply(.v1Items(sources), function(s) .v1Name(.v1Text(s) %||% "")))
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
                              concept_set = cf[[t]],
                              where = .v1And(.v1Predicate(sf[[t]], st, path), custom)))
      })
      opts <- list(sources = sources)
      window <- out[["window"]]
      if (.v1IsObject(window) && length(window) > 0L) {
        window <- .v1Compact(window)
        if (!is.null(window[["at"]])) {
          .v1Residue(st, path, window = out[["window"]])
          opts[["window"]] <- list(start = .v1Int(window[["at"]]), end = .v1Int(window[["at"]]), anchor = "index")
        } else {
          opts[["window"]] <- .v1Window(window)
        }
      }
      for (k in c("interval_match", "event_select", "select_by")) {
        if (!is.null(out[[k]])) opts[[k]] <- tolower(.v1Text(out[[k]]) %||% "")
      }
      if (!is.null(out[["select_n"]])) opts[["select_n"]] <- .v1Int(out[["select_n"]])
      anchor <- .v1Int(out[["anchor"]] %||% 0L)
      if (!identical(anchor, 0L)) .v1Residue(st, path, anchor = out[["anchor"]])
      opts[["anchor"]] <- "index"
      node[["options"]] <- opts
    },
    temporal_covariates =, person_period = {
      node[["format"]] <- type
      ev <- .v1Event(st, paste0("ev_", name), out[["table"]], concept_set = out[["concept_set"]],
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
      if (identical(type, "person_period")) {
        node[["grain"]] <- "episode"
        if (!is.null(out[["grain"]]) || !is.null(out[["time_origin"]])) {
          .v1Residue(st, path, grain = out[["grain"]], time_origin = out[["time_origin"]])
        }
      }
      node[["options"]] <- opts
    },
    .irReject(paste0("Unsupported output type '", type, "'."),
              output = name, field = paste0("/outputs/", name, "/type"))
  )
  node
}

# v1 multi-state transitions: list(states, edges = [{from, to, trans}]),
# adjacency {from: [to, ...]}, or an edge list -> [{from, to}].
.v1Transitions <- function(st, x) {
  edges <- NULL
  if (.v1IsObject(x) && !is.null(x[["edges"]])) {
    edges <- lapply(.v1Items(x[["edges"]]), function(e) list(from = .v1Text(e[["from"]]), to = .v1Text(e[["to"]])))
  } else if (.v1IsObject(x) && length(x) > 0L) {
    edges <- list()
    for (from in names(x)) {
      for (to in .v1Items(x[[from]])) edges <- c(edges, list(list(from = from, to = .v1Text(to))))
    }
  } else if (is.list(x)) {
    edges <- lapply(.v1Items(x), function(e) list(from = .v1Text(e[["from"]]), to = .v1Text(e[["to"]])))
  }
  lapply(edges %||% list(), function(e) {
    list(from = .v1MapName(st, "states", e[["from"]] %||% ""), to = .v1MapName(st, "states", e[["to"]] %||% ""))
  })
}

# --- populations -------------------------------------------------------------------

.v1LowerIndexEvent <- function(st, pid, ie) {
  ie <- .v1Compact(ie)
  ev <- .v1Event(st, paste0("ev_", pid, "_index"), ie[["table"]], concept_set = ie[["concept_set"]])
  index <- list(source = list(event = ev),
                select = tolower(.v1Text(ie[["primary_limit"]]) %||% "first"),
                restrict_to_observation = TRUE)
  if (!is.null(ie[["end_strategy"]])) {
    es <- ie[["end_strategy"]]
    if (.v1IsObject(es) && .v1IsObject(es[["DateOffset"]])) {
      do <- .v1Compact(es[["DateOffset"]])
      index[["legacy_end_strategy"]] <- list(DateOffset = list(
        DateField = .v1Text(do[["DateField"]]), Offset = .v1Int(do[["Offset"]])))
      extra <- setdiff(names(.v1Compact(es)), "DateOffset")
      if (length(extra)) index[["legacy_end_strategy"]] <- c(index[["legacy_end_strategy"]], es[extra])
    } else {
      index[["legacy_end_strategy"]] <- es
    }
  }
  index
}

.v1LowerPopulation <- function(st, pid, pop, parent = NULL) {
  pop <- .v1Compact(pop)
  id <- .v1MapName(st, "populations", pid)
  path <- paste0("populations/", id)
  label <- .v1Text(pop[["label"]])
  if (!is.null(pop[["id"]]) && !identical(.v1Text(pop[["id"]]), pid)) .v1Residue(st, path, id = pop[["id"]])
  if (!is.null(pop[["filters"]])) .v1Residue(st, path, filters = pop[["filters"]])
  kind <- tolower(.v1Text(pop[["kind"]]) %||% "criteria")
  node <- if (identical(kind, "setop") || !is.null(pop[["setop"]])) {
    op <- tolower(.v1Text(pop[["setop"]][["op"]]) %||% "union")
    if (identical(op, "difference")) op <- "setdiff"
    list(kind = "setop", setop = list(
      op = op, members = lapply(.v1Items(pop[["setop"]][["members"]]), function(m) .v1MapName(st, "populations", .v1Text(m) %||% ""))))
  } else if (!is.null(pop[["index_event"]])) {
    where <- .v1And(
      if (!is.null(pop[["cohort_definition_id"]])) list(cohort = list(cohort_definition_id = .v1Int(pop[["cohort_definition_id"]]))),
      .v1Criteria(pop[["filter_tree"]], st, path))
    node <- list(kind = "index", index = .v1LowerIndexEvent(st, id, pop[["index_event"]]))
    node["where"] <- list(where)
    if (!is.null(pop[["episode_policy"]])) .v1Residue(st, path, episode_policy = pop[["episode_policy"]])
    node
  } else {
    node <- list(kind = "criteria")
    node["parent"] <- list(parent)
    node["where"] <- list(.v1Criteria(pop[["filter_tree"]], st, path))
    node["cohort_definition_id"] <- list(.v1Int(pop[["cohort_definition_id"]]))
    node["episode_policy"] <- list(if (!is.null(pop[["episode_policy"]])) tolower(.v1Text(pop[["episode_policy"]]) %||% ""))
    node
  }
  if (!is.null(label)) node <- c(list(kind = node[["kind"]], label = label), node[setdiff(names(node), "kind")])
  node <- .v1CarryUnknown(node, pop, .V1_POPULATION_FIELDS)
  st[["populations"]][[id]] <- node
  id
}

# The base population a v1 plan cohort defines (or NULL when there is none).
.v1PlanCohortPopulation <- function(st, cohort, id) {
  cohort <- .v1Compact(cohort)
  path <- paste0("populations/", id)
  type <- .v1Text(cohort[["type"]])
  node <- NULL
  if (!is.null(cohort[["filter_tree"]])) {
    node <- list(kind = "criteria", label = "Base population")
    node["where"] <- list(.v1Criteria(cohort[["filter_tree"]], st, path))
    node["episode_policy"] <- list(if (!is.null(cohort[["episode_policy"]])) tolower(.v1Text(cohort[["episode_policy"]]) %||% ""))
    if (!is.null(cohort[["spec"]])) .v1Residue(st, path, ignored_spec = cohort[["spec"]])
    if (identical(type, "cohort_table")) .v1Residue(st, path, legacy_index_anchor = "scope_cohort")
    .v1Residue(st, path, legacy_base_episodes = "observation_periods")
  } else if (!is.null(cohort[["spec"]])) {
    spec <- cohort[["spec"]]
    if (.v1IsObject(spec)) {
      spec <- .v1Compact(spec)
      ls <- list()
      if (!is.null(spec[["type"]])) ls[["type"]] <- tolower(.v1Text(spec[["type"]]) %||% "")
      if (!is.null(spec[["concept_set"]])) ls[["concept_set"]] <- .v1ConceptSet(spec[["concept_set"]])
      if (!is.null(spec[["value_bin"]])) {
        vb <- .v1Compact(spec[["value_bin"]])
        ls[["value_bin"]] <- list(lower = .v1Scalar(vb[["lower"]]), upper = .v1Scalar(vb[["upper"]]),
                             safe_scope = .v1SafeScope(vb[["safe_scope"]]))
      }
      if (!is.null(spec[["inclusion_criteria"]])) ls[["inclusion_criteria"]] <- .v1Inclusion(spec[["inclusion_criteria"]])
      extra <- setdiff(names(spec), c("type", "concept_set", "value_bin", "inclusion_criteria"))
      if (length(extra)) ls <- c(ls, spec[extra])
      if (length(ls) == 0L) ls <- structure(list(), names = character(0))
    } else {
      ls <- spec
    }
    node <- list(kind = "index", label = "Base population",
                 index = list(source = list(legacy_spec = ls), select = "all",
                              restrict_to_observation = FALSE),
                 coverage = list(on_missing = "keep"))
    if (!is.null(cohort[["cohort_definition_id"]])) .v1Residue(st, path, cohort_id = cohort[["cohort_definition_id"]])
  } else if (identical(type, "cohort_table") && !is.null(cohort[["cohort_definition_id"]])) {
    cid <- .v1Int(cohort[["cohort_definition_id"]])
    node <- list(kind = "index", label = "Base population",
                 index = list(source = list(cohort_definition_id = cid), select = "all",
                              end = list(strategy = "event_end", days = NULL, pad = 0L),
                              restrict_to_observation = FALSE),
                 coverage = list(on_missing = "keep"))
    .v1Residue(st, path, legacy_cohort_rows = TRUE)
  }
  if (!is.null(node) && !is.null(cohort[["episode_policy"]]) && is.null(cohort[["filter_tree"]])) {
    .v1Residue(st, path, episode_policy = cohort[["episode_policy"]])
  }
  if (is.null(node)) return(NULL)
  st[["populations"]][[id]] <- node
  id
}

.v1SafeScope <- function(x) {
  if (is.null(x)) return(NULL)
  x <- .v1Compact(x)
  out <- x
  if (!is.null(out[["n_bins"]])) out[["n_bins"]] <- .v1Int(out[["n_bins"]])
  if (!is.null(out[["concept_id"]])) {
    ids <- .v1Items(out[["concept_id"]])
    out[["concept_id"]] <- if (length(ids) == 1L) .v1Int(ids[[1L]]) else lapply(ids, .v1Int)
  }
  out
}

.v1Inclusion <- function(crits) {
  if (!is.list(crits)) return(crits)
  nm <- names(crits)
  out <- list()
  for (i in seq_along(crits)) {
    key <- if (is.null(nm) || !nzchar(nm[[i]])) paste0("criterion_", i) else .v1Name(nm[[i]])
    crit <- .v1Compact(crits[[i]])
    if (!is.null(crit[["concept_set"]])) crit[["concept_set"]] <- .v1ConceptSet(crit[["concept_set"]])
    if (!is.null(crit[["table"]])) crit[["table"]] <- tolower(.v1Text(crit[["table"]]) %||% "")
    if (.v1IsObject(crit[["temporal"]])) {
      t <- .v1Compact(crit[["temporal"]])
      if (!is.null(t[["index_window"]])) t[["index_window"]] <- lapply(.v1Compact(t[["index_window"]]), .v1Int)
      if (!is.null(t[["calendar"]])) t[["calendar"]] <- lapply(.v1Compact(t[["calendar"]]), .v1Text)
      crit[["temporal"]] <- t
    }
    if (.v1IsObject(crit[["occurrence"]])) crit[["occurrence"]] <- lapply(.v1Compact(crit[["occurrence"]]), .v1Int)
    out[[key]] <- crit
  }
  if (length(out) == 0L) structure(list(), names = character(0)) else out
}

# --- the plan ---------------------------------------------------------------------

#' Lower a v1 plan to the compatibility IR (design section 5)
#'
#' Pure: no database access. Defaults are expanded afterwards by
#' \code{.irNormalizeWire()}. The verbatim v1 fields the IR does not express
#' exactly are attached as attribute \code{legacy_residue} (a named list keyed
#' by IR path, plus \code{names}, the map of v1 identifiers re-encoded to
#' valid 2.0 names); nothing is discarded.
#'
#' @param plan v1 plan in parsed JSON form, already through
#'   \code{.planV1Envelope()}.
#' @return The lowered IR (before default expansion).
#' @keywords internal
.irLowerV1 <- function(plan) {
  st <- .v1State(plan)
  multi <- isTRUE(tryCatch(.planHasMultiPopulation(plan), error = function(e) FALSE))
  pops <- if (.v1IsObject(plan[["populations"]])) .v1Compact(plan[["populations"]]) else list()
  cohort <- if (.v1IsObject(plan[["cohort"]]) && length(plan[["cohort"]]) > 0L) plan[["cohort"]] else NULL

  # Root advisory and server-injected fields.
  if (!is.null(plan[["anchor"]])) .v1Residue(st, "root", anchor = plan[["anchor"]])
  if (!is.null(plan[["harmonization"]])) .v1Residue(st, "root", harmonization = TRUE)

  # Populations: the plan cohort's base, then every declared population.
  base_has_own <- multi && "base" %in% names(pops) && {
    b <- .v1Compact(pops[["base"]])
    any(c("filter_tree", "cohort_definition_id", "index_event", "setop") %in% names(b))
  }
  plan_base <- if (!is.null(cohort)) {
    .v1PlanCohortPopulation(st, cohort, if (base_has_own) "v1_plan_cohort" else "base")
  }
  if (multi) {
    for (pid in names(pops)) {
      if (identical(pid, "base") && !base_has_own && !is.null(plan_base)) {
        label <- .v1Text(.v1Compact(pops[["base"]])[["label"]])
        if (!is.null(label)) st[["populations"]][["base"]][["label"]] <- label
        next
      }
      parent <- if (identical(pid, "base") && base_has_own && !is.null(plan_base) &&
                    is.null(.v1Compact(pops[["base"]])[["index_event"]])) plan_base
      .v1LowerPopulation(st, pid, pops[[pid]], parent = parent)
    }
  } else if (length(pops)) {
    .v1Residue(st, "root", populations_inactive = pops)
  }
  if (is.null(st[["populations"]][["base"]])) {
    st[["populations"]][["base"]] <- list(kind = "criteria", label = "Base population")
  }

  # Scope: the plan-level results cohort is a scope source (today's semantics).
  scope <- if (.v1IsObject(plan[["scope"]])) .v1Compact(plan[["scope"]]) else list()
  cohort_sources <- list()
  if (!is.null(scope[["cohort"]])) cohort_sources <- c(cohort_sources, .v1Items(scope[["cohort"]]))
  if (identical(.v1Text(cohort[["type"]]), "cohort_table") && !is.null(cohort[["cohort_definition_id"]])) {
    cohort_sources <- c(cohort_sources, list(.v1Int(cohort[["cohort_definition_id"]])))
  }
  ir_scope <- list()
  ir_scope["cohort"] <- list(if (length(cohort_sources)) .v1Scalar(cohort_sources[[1L]]))
  if (length(cohort_sources) > 1L) .v1Residue(st, "root", scope_cohort_sources = cohort_sources)
  tables <- lapply(.v1Items(scope[["tables"]]), function(t) .v1Text(t) %||% t)
  if (!length(tables) && !is.null(scope[["tables_frames"]])) {
    tables <- as.list(paste0("scope_table_", seq_along(.v1Items(scope[["tables_frames"]]))))
  }
  ir_scope[["tables"]] <- tables
  ir_scope[["combine"]] <- tolower(.v1Text(scope[["combine"]]) %||% "union")

  # Outputs.
  outputs <- if (.v1IsObject(plan[["outputs"]])) plan[["outputs"]] else list()
  for (name in names(outputs)) {
    out <- outputs[[name]]
    key <- .v1MapName(st, "outputs", name)
    pid <- .v1Text(.v1Compact(out)[["population_id"]]) %||% "base"
    rows <- if (multi) .v1MapName(st, "populations", pid) else "base"
    if (!multi && !identical(pid, "base")) {
      .v1Residue(st, paste0("outputs/", key), population_id = pid)
    }
    type <- .v1Text(.v1Compact(out)[["type"]]) %||% "event_level"
    known <- c(.V1_OUTPUT_COMMON, .V1_OUTPUT_FIELDS[[type]] %||% character(0))
    st[["outputs"]][[key]] <- .v1CarryUnknown(.v1LowerOutput(st, key, out, rows), out, known)
  }

  options <- if (.v1IsObject(plan[["options"]])) .v1Compact(plan[["options"]]) else .irEmptyObject()
  ir <- list(
    version = 2L,
    meta = list(language_version = .IR_LANGUAGE_VERSION,
                semantic_version = .IR_SEMANTIC_VERSION,
                generated_by = .V1_LOWERING_GENERATOR),
    options = options,
    concept_sets = structure(list(), names = character(0)),
    events = if (length(st[["events"]])) st[["events"]] else structure(list(), names = character(0)),
    intervals = structure(list(), names = character(0)),
    populations = st[["populations"]],
    variables = if (length(st[["variables"]])) st[["variables"]] else structure(list(), names = character(0)),
    outputs = if (length(st[["outputs"]])) st[["outputs"]] else structure(list(), names = character(0)),
    scope = ir_scope
  )
  ir <- .v1CarryUnknown(ir, plan, .V1_ROOT_FIELDS)
  # Any envelope finding (an unknown field anywhere, a forged shape) keeps the
  # IR invalid: the marker is not an IR property, so ir-v2.json refuses it.
  envelope <- .planV1EnvelopeErrors(plan)
  if (length(envelope)) ir[["v1_envelope_errors"]] <- as.list(envelope)
  ir[["scope"]] <- .v1CarryUnknown(ir[["scope"]], plan[["scope"]], .V1_SCOPE_FIELDS)
  if (!is.null(cohort) && !is.null(ir[["populations"]][["base"]])) {
    target <- if (!is.null(ir[["populations"]][["v1_plan_cohort"]])) "v1_plan_cohort" else "base"
    ir[["populations"]][[target]] <- .v1CarryUnknown(ir[["populations"]][[target]], cohort, .V1_COHORT_FIELDS)
  }
  residue <- st[["residue"]]
  if (length(st[["names"]])) residue[["names"]] <- st[["names"]]
  attr(ir, "legacy_residue") <- if (length(residue)) residue else NULL
  ir
}
