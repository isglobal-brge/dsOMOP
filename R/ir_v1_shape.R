# Module: the closed v1 envelope as data (design-2.10.3 section 4.5, step 1).
#
# Every position of a v1 plan is described by a shape, and .v1ShapeErrors()
# walks the plan against it before anything is normalised:
#   - closed objects: every RAW member name is checked before any null
#     compaction, so an unknown member is refused even when its value is null;
#     a known member may be null (in v1 a null member is an absent one);
#   - maps (keys chosen by the plan: outputs, populations, tables, ...);
#   - arrays (a JSON scalar stands for a one-element array: R vectors of
#     length one arrive unboxed);
#   - scalars with a type and, optionally, an enumeration;
#   - alternatives (the forms a v1 field accepts, such as a concept-id list or
#     a concept-set object);
#   - opaque advisory content (documented uses only: the client-side
#     federation evidence inside `harmonization`, which the server never
#     reads; it goes to the server-private annex).
# A shape may carry `check`, a function(x, path) returning findings: the value
# rules of the corrected legacy validators that own that position.

.v1sObj <- function(..., .extra = NULL, .check = NULL, .required = character(0)) {
  list(kind = "object", fields = list(...), extra = .extra, check = .check,
       required = .required)
}
.v1sMap <- function(values, .check = NULL) list(kind = "map", values = values, check = .check)
.v1sArr <- function(items, .check = NULL, .min = 0L) {
  list(kind = "array", items = items, check = .check, min = .min)
}
.v1sScalar <- function(type = "any", enum = NULL, .check = NULL) {
  list(kind = "scalar", type = type, enum = enum, check = .check)
}
.v1sOneOf <- function(..., .check = NULL) list(kind = "oneof", options = list(...), check = .check)
.v1sOpaque <- function() list(kind = "opaque")
# A recursive grammar (row filters, population filter trees) walked by its
# own function(x, path) returning findings.
.v1sCustom <- function(fn) list(kind = "custom", check = fn)
# An object whose shape is chosen by the value of one member (an output by
# its `type`); `default` is the case of an absent member.
.v1sSwitch <- function(field, cases, default) {
  list(kind = "switch", field = field, cases = cases, default = default)
}

# JSON type of a parsed value: "null", "object", "array" or "scalar".
.v1JsonType <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    if (!is.null(names(x))) return("object")
    return("array")
  }
  if (is.atomic(x) && length(x) == 1L) return("scalar")
  "array"
}

.v1ScalarOk <- function(x, type) {
  if (!is.atomic(x) || length(x) != 1L || is.na(x)) return(FALSE)
  switch(type,
    string = is.character(x),
    integer = is.numeric(x) && is.finite(x) && x == round(x),
    number = is.numeric(x) && is.finite(x),
    boolean = is.logical(x),
    scalar = TRUE,
    any = TRUE,
    FALSE)
}

#' Findings of a v1 value against a shape (empty when it conforms)
#'
#' @param x The value (parsed JSON form).
#' @param shape The shape (\code{.v1sObj()}, ...).
#' @param path JSON-pointer-like path of \code{x}.
#' @return Character vector of findings, "<path>: <reason>".
#' @keywords internal
.v1ShapeErrors <- function(x, shape, path = "") {
  errors <- character(0)
  add <- function(...) errors <<- c(errors, paste0(...))
  type <- .v1JsonType(x)
  label <- if (nzchar(path)) path else "/"
  switch(shape[["kind"]],
    opaque = NULL,
    custom = NULL,
    switch = {
      key <- if (identical(type, "object")) x[[shape[["field"]]]] else NULL
      key <- if (is.character(key) && length(key) == 1L) key else shape[["default"]]
      sub <- shape[["cases"]][[key]]
      if (is.null(sub)) {
        add(label, "/", shape[["field"]], ": must be one of ", paste(names(shape[["cases"]]), collapse = ", "))
      } else {
        errors <- c(errors, .v1ShapeErrors(x, sub, path))
      }
    },
    scalar = {
      if (!identical(type, "scalar") || !.v1ScalarOk(x, shape[["type"]])) {
        add(label, ": must be a ", shape[["type"]])
      } else if (!is.null(shape[["enum"]]) &&
                 !tolower(as.character(x)) %in% tolower(shape[["enum"]])) {
        add(label, ": must be one of ", paste(shape[["enum"]], collapse = ", "))
      }
    },
    array = {
      items <- if (identical(type, "array")) {
        if (is.list(x)) x else as.list(x)
      } else if (identical(type, "scalar")) {
        list(x)
      } else {
        add(label, ": must be an array")
        NULL
      }
      if (!is.null(items)) {
        if (length(items) < shape[["min"]]) add(label, ": needs at least ", shape[["min"]], " item(s)")
        for (i in seq_along(items)) {
          p <- paste0(path, "/", i - 1L)
          if (is.null(items[[i]])) {
            add(p, ": an array item cannot be null")
          } else {
            errors <- c(errors, .v1ShapeErrors(items[[i]], shape[["items"]], p))
          }
        }
      }
    },
    object = ,
    map = {
      empty_array <- identical(type, "array") && length(x) == 0L
      if (!identical(type, "object") && !empty_array) {
        add(label, ": must be an object")
      } else {
        keys <- names(x) %||% character(0)
        if (anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys)) {
          add(label, ": member names must be non-empty and unique")
        }
        if (identical(shape[["kind"]], "map")) {
          for (k in keys) {
            if (!is.null(x[[k]])) errors <- c(errors, .v1ShapeErrors(x[[k]], shape[["values"]], paste0(path, "/", k)))
          }
        } else {
          fields <- shape[["fields"]]
          unknown <- setdiff(keys, names(fields))
          if (length(unknown) && is.null(shape[["extra"]])) {
            add(label, ": unknown field(s) ", paste(unknown, collapse = ", "))
          }
          for (k in keys) {
            v <- x[[k]]
            if (is.null(v)) next
            sub <- if (k %in% names(fields)) fields[[k]] else shape[["extra"]]
            if (!is.null(sub)) errors <- c(errors, .v1ShapeErrors(v, sub, paste0(path, "/", k)))
          }
          for (k in shape[["required"]]) {
            if (is.null(x[[k]])) add(label, ": requires ", k)
          }
        }
      }
    },
    oneof = {
      opts <- shape[["options"]]
      fits <- vapply(opts, function(o) {
        k <- o[["kind"]]
        switch(type,
          object = k %in% c("object", "map", "opaque", "custom", "switch"),
          array = k %in% c("array", "opaque", "custom") || (k %in% c("object", "map") && length(x) == 0L),
          scalar = k %in% c("scalar", "array", "opaque", "custom"),
          FALSE)
      }, logical(1))
      if (!any(fits)) {
        add(label, ": has none of the accepted forms")
      } else {
        found <- lapply(opts[fits], function(o) .v1ShapeErrors(x, o, path))
        ok <- vapply(found, function(f) length(f) == 0L, logical(1))
        if (!any(ok)) errors <- c(errors, found[[1L]])
      }
    },
    stop("unknown v1 shape kind '", shape[["kind"]], "'", call. = FALSE)
  )
  if (!length(errors) && is.function(shape[["check"]])) {
    errors <- c(errors, shape[["check"]](x, label))
  }
  errors
}

# Run a legacy validator that stops on its first finding; return the finding
# as "<path>: <message>" (character(0) when it passes).
.v1Legacy <- function(path, expr) {
  tryCatch({
    force(expr)
    character(0)
  }, error = function(e) paste0(path, ": ", conditionMessage(e)))
}

# --- shared v1 shapes ------------------------------------------------------------

.v1sConceptIds <- function() .v1sArr(.v1sScalar("integer"))
.v1sSafeScope <- function() {
  .v1sObj(table = .v1sScalar("string"), column = .v1sScalar("string"),
          concept_id = .v1sConceptIds(), concept_col = .v1sScalar("string"),
          n_bins = .v1sScalar("integer"))
}

# A results-cohort id: a positive integer (amendment A-10: 0 is refused).
.v1sCohortId <- function() {
  .v1sScalar("integer", .check = function(x, path) {
    if (x < 1) paste0(path, ": a v1 cohort_definition_id must be a positive integer (0 is refused)")
  })
}

#' Findings of a v1 row filter (the custom DSL of \code{.assertCustomFilterSafe()})
#'
#' Closed at every node on raw member names (and/or/not groups, leaves
#' \code{{var, op, value, safe_scope}}, a value_bin operand
#' \code{{lower, upper}}, a safe scope), then the legacy validator itself
#' with the filter's own columns as its column list; the site's filterable
#' columns and bin contracts are checked at step 4.
#' @keywords internal
.v1RowFilterErrors <- function(x, path) {
  errors <- character(0)
  vars <- character(0)
  push <- function(...) errors <<- c(errors, paste0(...))
  walk <- function(node, p, depth) {
    if (depth > 64L) return(push(p, ": filter nesting is too deep"))
    if (!.v1IsObject(node) || length(node) == 0L) {
      return(push(p, ": a row filter is an and/or group or a leaf object"))
    }
    keys <- names(node)
    group <- intersect(keys, c("and", "or", "not"))
    if (length(group)) {
      if (length(keys) != 1L) return(push(p, ": a filter group carries exactly one of and/or/not"))
      # NOT (design 6.6): one negated predicate
      if (identical(group, "not")) {
        if (!.v1IsObject(node[["not"]]) || length(node[["not"]]) == 0L) {
          return(push(p, "/not: a NOT group negates one predicate"))
        }
        return(walk(node[["not"]], paste0(p, "/not"), depth + 1L))
      }
      items <- node[[group]]
      if (!is.list(items) || !is.null(names(items)) || !length(items)) {
        return(push(p, "/", group, ": must be a non-empty array of filters"))
      }
      for (i in seq_along(items)) walk(items[[i]], paste0(p, "/", group, "/", i - 1L), depth + 1L)
      return(invisible())
    }
    unknown <- setdiff(keys, c("var", "op", "value", "safe_scope"))
    if (length(unknown)) push(p, ": unknown filter field(s) ", paste(unknown, collapse = ", "))
    if (!is.null(node[["safe_scope"]])) {
      errors <<- c(errors, .v1ShapeErrors(node[["safe_scope"]], .v1sSafeScope(), paste0(p, "/safe_scope")))
    }
    op <- tolower(.v1Text(node[["op"]]) %||% "")
    value <- node[["value"]]
    if (identical(op, "value_bin")) {
      errors <<- c(errors, .v1ShapeErrors(value, .v1sObj(lower = .v1sScalar("number"),
                                                          upper = .v1sScalar("number"),
                                                          .required = c("lower", "upper")),
                                          paste0(p, "/value")))
    } else if (identical(.v1JsonType(value), "object")) {
      push(p, "/value: only value_bin takes an object operand")
    }
    var <- .v1Text(node[["var"]])
    if (!is.null(var)) vars <<- c(vars, tolower(var))
  }
  walk(x, path, 1L)
  if (!length(errors)) {
    errors <- .v1Legacy(path, .assertCustomFilterSafe(x, valid_columns = unique(vars)))
  }
  errors
}

#' Findings of a v1 population filter tree
#'
#' The legacy validator \code{.validateCohortFilterTree()} (closed leaves,
#' known types and parameters, the complexity caps) plus the closure of the
#' nested parameter objects (\code{window}, \code{safe_scope}) and amendment
#' A-10's refusal of a \code{cohort_definition_id} of 0.
#' @keywords internal
.v1FilterTreeErrors <- function(x, path) {
  errors <- .v1Legacy(path, .validateCohortFilterTree(x))
  if (length(errors)) return(errors)
  walk <- function(node, p) {
    if (!is.list(node)) return(invisible())
    if (is.null(names(node))) {
      for (i in seq_along(node)) walk(node[[i]], paste0(p, "/", i - 1L))
      return(invisible())
    }
    if ("type" %in% names(node)) {
      params <- node[["params"]] %||% list()
      pp <- paste0(p, "/params")
      if (!is.null(params[["window"]])) {
        errors <<- c(errors, .v1ShapeErrors(params[["window"]], .v1sObj(start = .v1sScalar("integer"),
                                                                        end = .v1sScalar("integer")),
                                            paste0(pp, "/window")))
      }
      if (!is.null(params[["safe_scope"]])) {
        errors <<- c(errors, .v1ShapeErrors(params[["safe_scope"]], .v1sSafeScope(), paste0(pp, "/safe_scope")))
      }
      if (!is.null(params[["cohort_definition_id"]])) {
        errors <<- c(errors, .v1ShapeErrors(params[["cohort_definition_id"]], .v1sCohortId(),
                                            paste0(pp, "/cohort_definition_id")))
      }
      return(invisible())
    }
    if ("not" %in% names(node)) walk(node[["not"]], paste0(p, "/not"))
    for (g in intersect(names(node), c("and", "or"))) {
      items <- node[[g]]
      for (i in seq_along(items)) walk(items[[i]], paste0(p, "/", g, "/", i - 1L))
    }
  }
  walk(x, path)
  errors
}

# --- the v1 plan ---------------------------------------------------------------------

.v1sStr <- function(enum = NULL) .v1sScalar("string", enum = enum)
.v1sInt <- function() .v1sScalar("integer")
.v1sNum <- function() .v1sScalar("number")
.v1sBool <- function() .v1sScalar("boolean")
.v1sStrs <- function(enum = NULL) .v1sArr(.v1sStr(enum))
.v1sWindow <- function() .v1sObj(start = .v1sInt(), end = .v1sInt())
.v1sRowFilter <- function() .v1sCustom(.v1RowFilterErrors)
.v1sFilterTree <- function() .v1sCustom(.v1FilterTreeErrors)
.v1sConceptSet <- function() {
  .v1sOneOf(.v1sConceptIds(),
            .v1sObj(concepts = .v1sConceptIds(), include_descendants = .v1sBool(),
                    include_mapped = .v1sBool(), exclude = .v1sConceptIds()))
}
.v1sVisit <- function() .v1sObj(concept_ids = .v1sConceptIds(), visit_concept_id = .v1sConceptIds())
.v1sDateHandling <- function() {
  .v1sOneOf(.v1sStr(),
            .v1sObj(mode = .v1sStr(), reference = .v1sStr(), bin_width = .v1sStr(),
                    date_columns = .v1sStrs()),
            .check = function(x, path) .v1Legacy(path, .normalizeDateHandling(x)))
}
.v1sTemporal <- function() {
  .v1sObj(index_window = .v1sWindow(),
          calendar = .v1sObj(start = .v1sStr(), end = .v1sStr()),
          event_select = .v1sObj(order = .v1sStr(c("first", "last")), n = .v1sInt(),
                                 by = .v1sStr(c("grain", "concept"))),
          min_gap = .v1sOneOf(.v1sInt(), .v1sObj(days = .v1sInt(), by = .v1sStr(c("grain", "concept")),
                                                 keep = .v1sStr(c("first", "last")))),
          .check = function(x, path) .v1Legacy(path, .validateTemporalSpec(x)))
}
.V1_FEATURE_TYPES <- c("boolean", "count", "n_distinct", "mean_value", "min_value", "max_value",
                       "sum_value", "sd_value", "cv_value", "slope_value", "first_value",
                       "latest_value", "time_since", "drug_duration", "abnormal_high",
                       "abnormal_low", "gap_max_days", "gap_mean_days", "duration_sum")
.v1sFeatureSpecs <- function() {
  spec <- .v1sObj(name = .v1sStr(), type = .v1sStr(.V1_FEATURE_TYPES), concept_set = .v1sConceptSet(),
                  value_column = .v1sStr(), concept_col = .v1sStr(), filter = .v1sRowFilter(),
                  time_window = .v1sWindow(), reference_date = .v1sStr(), unit = .v1sStr(),
                  agg = .v1sStr())
  .v1sOneOf(.v1sArr(spec), .v1sMap(spec))
}
.v1sOutcome <- function() .v1sObj(table = .v1sStr(), concept_set = .v1sConceptSet(), filters = .v1sRowFilter())
.V1_EPISODE_POLICIES <- c("first_episode", "last_episode", "all_episodes", "any_episode")

# Recipe options the client copies onto an output (advisory: no server path
# reads them; they are kept in the annex). Closed keys; object values take the
# shape of the plan field of the same name.
.v1sOutputOptions <- function() {
  scalar <- .v1sOneOf(.v1sScalar("scalar"), .v1sArr(.v1sScalar("scalar")))
  keys <- c("grain", "outcome_mode", "bin_width", "window_start", "window_end", "analyses",
            "format", "tie_policy", "event_order", "washout_days", "initial_state",
            "state_hierarchy", "state_step", "select_n", "select_by", "interval_match",
            "event_select", "anchor", "time_origin")
  fields <- stats::setNames(rep(list(scalar), length(keys)), keys)
  fields[["date_handling"]] <- .v1sDateHandling()
  fields[["temporal"]] <- .v1sTemporal()
  fields[["time_window"]] <- .v1sObj(date_column = .v1sStr(), start_date = .v1sStr(), end_date = .v1sStr())
  fields[["window"]] <- .v1sObj(start = .v1sInt(), end = .v1sInt(), at = .v1sInt())
  fields[["tar"]] <- .v1sObj(start_offset = .v1sInt(), end_offset = .v1sInt())
  fields[["censoring"]] <- .v1sObj(cohort_end = .v1sBool(), observation_period_end = .v1sBool(),
                                   death = .v1sBool(), admin_date = .v1sStr())
  fields[["transitions"]] <- .v1sTransitions()
  do.call(.v1sObj, fields)
}
.v1sTransitions <- function() {
  .v1sOneOf(.v1sObj(states = .v1sStrs(), edges = .v1sArr(.v1sObj(from = .v1sStr(), to = .v1sStr(),
                                                                  trans = .v1sInt()))),
            .v1sMap(.v1sStrs()))
}

.v1sOutput <- function() {
  common <- list(
    type = .v1sStr(), population_id = .v1sStr(),
    filters = .v1sObj(concept_set = .v1sObj(ids = .v1sConceptSet()),
                      time_window = .v1sObj(date_column = .v1sStr(), start_date = .v1sStr(), end_date = .v1sStr(),
                                            .check = function(x, path) .v1Legacy(path, .validateDateBounds(
                                              x[["start_date"]], x[["end_date"]], "time_window"))),
                      custom = .v1sRowFilter(), visit = .v1sVisit(), concept_col = .v1sStr()),
    options = .v1sOutputOptions())
  out <- function(...) do.call(.v1sObj, c(common, list(...)))
  derived_spec <- .v1sObj(kind = .v1sStr(.V1_DERIVED_KINDS), name = .v1sStr(),
                          reference = .v1sStr(c("today", "index")), reference_date = .v1sStr(),
                          period_policy = .v1sStr(c("containing", "total")))
  feature_table <- .v1sObj(features = .v1sFeatureSpecs(), concept_set = .v1sConceptSet(),
                           filters = .v1sRowFilter(), concept_col = .v1sStr(), visit = .v1sVisit(),
                           .required = "features")
  .v1sSwitch("type", default = "event_level", cases = list(
    person_level = out(
      tables = .v1sMap(.v1sOneOf(.v1sStrs(), feature_table, .v1sMap(.v1sStr()))),
      representation = .v1sStr(),
      derived_columns = .v1sOneOf(.v1sArr(derived_spec), .v1sMap(derived_spec))),
    event_level = out(
      table = .v1sStr(), columns = .v1sStrs(), concept_set = .v1sConceptSet(),
      visit_filter = .v1sVisit(), concept_col = .v1sStr(), temporal = .v1sTemporal(),
      date_handling = .v1sDateHandling(),
      representation = .v1sObj(format = .v1sStr(c("long", "wide", "features", "sparse")),
                               features = .v1sFeatureSpecs(), grain = .v1sStr(c("person", "episode")))),
    baseline = out(
      # the executor reads the values (tolower(columns)); names are ignored
      columns = .v1sOneOf(.v1sStrs(), .v1sMap(.v1sStr())),
      derived = .v1sStrs(c("age_at_index", "prior_observation", "future_observation")),
      age_breaks = .v1sArr(.v1sInt(), .check = function(x, path) {
        .v1Legacy(path, .bandAges(20L, age_breaks = x))
      })),
    survival = out(
      outcome = .v1sOutcome(), outcomes = .v1sMap(.v1sOutcome()),
      tar = .v1sObj(start_offset = .v1sInt(), end_offset = .v1sInt(),
                    .check = function(x, path) .v1Legacy(path, .normalizeSurvivalTar(x))),
      censoring = .v1sObj(cohort_end = .v1sBool(), observation_period_end = .v1sBool(),
                          death = .v1sBool(), admin_date = .v1sStr(),
                          .check = function(x, path) .v1Legacy(path, .normalizeSurvivalCensoring(x))),
      format = .v1sStr(c("survival", "competing_risk", "recurrent_events", "counting_process", "multi_state")),
      event_order = .v1sStr(c("first", "last", "all")), washout_days = .v1sInt(),
      tie_policy = .v1sStr(c("priority", "error", "all", "sequential")),
      transitions = .v1sTransitions(), initial_state = .v1sStr(), state_hierarchy = .v1sStrs(),
      state_step = .v1sNum()),
    cohort_membership = out(date_handling = .v1sDateHandling()),
    concept_dictionary = out(source_outputs = .v1sStrs()),
    intervals_long = out(
      tables = .v1sStrs(), concept_filter = .v1sMap(.v1sConceptSet()), source_filters = .v1sMap(.v1sRowFilter()),
      window = .v1sObj(start = .v1sInt(), end = .v1sInt(), at = .v1sInt()),
      interval_match = .v1sStr(), event_select = .v1sStr(), select_n = .v1sInt(), select_by = .v1sStr(),
      anchor = .v1sInt()),
    temporal_covariates = out(
      table = .v1sStr(), concept_set = .v1sConceptSet(), bin_width = .v1sInt(), window_start = .v1sInt(),
      window_end = .v1sInt(), analyses = .v1sStrs(c("binary", "count"))),
    person_period = out(
      table = .v1sStr(), concept_set = .v1sConceptSet(), bin_width = .v1sInt(), window_start = .v1sInt(),
      window_end = .v1sInt(), analyses = .v1sStrs(c("binary", "count")), grain = .v1sStr(),
      time_origin = .v1sStr())))
}

.v1sSpec <- function() {
  .v1sObj(type = .v1sStr(c("condition", "drug", "measurement", "observation", "procedure")),
          concept_set = .v1sConceptSet(),
          value_threshold = .v1sCustom(function(x, path) paste0(
            path, ": Disclosive: value_threshold is no longer executable. ",
            "Use a server-issued value_bin from ds.omop.safe.filter.value().")),
          value_bin = .v1sObj(lower = .v1sNum(), upper = .v1sNum(), safe_scope = .v1sSafeScope(),
                              .required = c("lower", "upper"),
                              .check = function(x, path) {
                                if (!(x[["lower"]] < x[["upper"]])) {
                                  paste0(path, ": value_bin requires finite scalar lower/upper bounds with lower < upper.")
                                }
                              }),
          inclusion_criteria = .v1sMap(.v1sObj(
            table = .v1sStr(), concept_set = .v1sConceptSet(),
            temporal = .v1sObj(index_window = .v1sWindow(), calendar = .v1sObj(start = .v1sStr(), end = .v1sStr())),
            # the historical .applyInclusionCriteria() contract (review B2)
            occurrence = .v1sObj(type = .v1sStr(c("at_least", "at_most", "exactly")),
                                 count = .v1sScalar("integer", .check = function(x, path) {
                                   if (x < 0) paste0(path, ": occurrence$count must be one non-negative integer.")
                                 })))))
}

#' The closed shape of a v1 plan (step 1 of the frozen sequence)
#' @keywords internal
.v1PlanShape <- function() {
  cached <- .irCache[["v1_plan_shape"]]
  if (!is.null(cached)) return(cached)
  population <- .v1sObj(
    id = .v1sStr(), label = .v1sStr(), kind = .v1sStr(c("criteria", "setop")),
    filter_tree = .v1sFilterTree(), cohort_definition_id = .v1sCohortId(),
    episode_policy = .v1sStr(.V1_EPISODE_POLICIES),
    index_event = .v1sObj(table = .v1sStr(), concept_set = .v1sConceptSet(),
                          primary_limit = .v1sStr(c("first", "last", "all")),
                          end_strategy = .v1sCustom(function(x, path) {
                            .v1Legacy(path, .normalizeIndexEventEndStrategy(x))
                          })),
    setop = .v1sObj(op = .v1sStr(c("union", "intersect", "setdiff", "difference")),
                    members = .v1sStrs()),
    filters = .v1sRowFilter())
  shape <- .v1sObj(
    version = .v1sScalar("integer"),
    # the executor reads `spec` only without a filter_tree (it then ignores
    # it, even in the legacy flat-array form): validated where it is read
    cohort = .v1sObj(type = .v1sStr(c("cohort_table", "spec")), cohort_definition_id = .v1sCohortId(),
                     spec = .v1sOpaque(), filter_tree = .v1sFilterTree(),
                     episode_policy = .v1sStr(.V1_EPISODE_POLICIES),
                     .check = function(x, path) {
                       if (is.null(x[["filter_tree"]]) && !is.null(x[["spec"]])) {
                         .v1ShapeErrors(x[["spec"]], .v1sSpec(), paste0(path, "/spec"))
                       }
                     }),
    anchor = .v1sObj(table = .v1sStr(), id_col = .v1sStr()),
    outputs = .v1sMap(.v1sOutput()),
    # min_persons is a removed option older exports still carry (unread)
    options = .v1sObj(translate_concepts = .v1sBool(), block_sensitive = .v1sBool(),
                      factor_concepts = .v1sBool(), min_persons = .v1sInt()),
    populations = .v1sMap(population),
    # resolved workspace frames are server state and never part of a plan
    scope = .v1sObj(cohort = .v1sOneOf(.v1sCohortId(), .v1sStr(), .v1sArr(.v1sOneOf(.v1sCohortId(), .v1sStr()))),
                    tables = .v1sStrs(), combine = .v1sStr(c("union", "intersect"))),
    # client-side federation evidence (never read by the server; annex)
    harmonization = .v1sObj(version = .v1sScalar("integer"), mode = .v1sStr(), strict = .v1sBool(),
                            manifest = .v1sOpaque(), schema = .v1sOpaque(), plan_signature = .v1sStr()))
  assign("v1_plan_shape", shape, envir = .irCache)
  shape
}
