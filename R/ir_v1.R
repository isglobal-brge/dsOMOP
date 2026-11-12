# Module: v1 plans -- closed envelope and lowering to the compatibility IR
# (design-2.10.3 sections 4.5 and 5).
#
# A v1 plan (no `version`, or `version: 1`) is first validated by the
# corrected v1 validator (.planV1Envelope, step 1 of the frozen sequence):
# the existing v1 shape rules reused with exactly the enumerated 2.10.3
# corrections --
#   (a) unknown-field rejection at the root, in every output object and in
#       every nested object (the 2.10.1 validators accepted, e.g.,
#       `already_gated` at the root and in a concept_dictionary output);
#   (b) assignment grid-shape validation of a baseline `age_breaks` (at least
#       two strictly increasing integers starting at 0), replacing the
#       .computeAgeGroups(min_cell = 0) call whose on-grid rule rejected an
#       off-grid grid such as c(0, 18, 19, 65); .computeAgeGroups itself is
#       unchanged for aggregate callers;
#   (c) the width/grid gates deleted in section 7.4 are not applied here;
# plus the shape exclusivity of defect A-E2 (a set-op population carries no
# filter_tree, cohort_definition_id, index_event or episode_policy, and an
# unknown population kind is refused instead of being read as criteria).
# Value-level checks of known fields stay with the v1 executors, which still
# run v1 plans in this release stage, so their messages and per-output
# behaviour are unchanged. A null-valued field is absent, as it always was
# for v1 (R lists cannot tell the two apart).
#
# .irLowerV1() then maps the plan onto language 2.0 nodes plus the closed
# compatibility variants of ir-v2.json (section 5). Whatever v1 states and
# the frozen IR cannot express exactly is NOT discarded: it is kept verbatim
# in the server-private attribute `legacy_residue` of the IR (keyed by IR
# path), which the v1 adapters read, explain reports and .irSemanticDigest()
# binds. Change request CR-8 proposes closed IR variants for it.

# --- shared helpers -------------------------------------------------------------

.V1_NAME_PATTERN <- "^[A-Za-z][A-Za-z0-9_]{0,63}$"

# Drop null-valued members of a JSON object (v1: null is absence).
.v1Compact <- function(x) {
  if (!is.list(x)) return(x)
  if (length(x) == 0L) return(structure(list(), names = character(0)))
  if (is.null(names(x))) return(x)
  keep <- !vapply(x, is.null, logical(1))
  out <- x[keep]
  if (length(out) == 0L) structure(list(), names = character(0)) else out
}

.v1IsObject <- function(x) is.list(x) && (length(x) == 0L || !is.null(names(x)))
.v1IsArray <- function(x) is.list(x) && is.null(names(x)) && length(x) > 0L
.v1Items <- function(x) {
  if (is.null(x)) return(list())
  if (is.list(x) && is.null(names(x))) return(x)
  if (is.atomic(x) && length(x) != 1L) return(as.list(unname(x)))
  list(x)
}
.v1Scalar <- function(x) if (is.list(x) && length(x) == 1L && is.null(names(x))) x[[1L]] else x
.v1Text <- function(x) {
  x <- .v1Scalar(x)
  if (is.character(x) && length(x) == 1L && !is.na(x)) x else NULL
}

# A deterministic, valid 2.0 name for a v1 identifier (or a synthesised
# node). Valid names are kept; others are encoded injectively.
.v1Name <- function(raw, prefix = "") {
  candidate <- paste0(prefix, raw)
  if (grepl(.V1_NAME_PATTERN, candidate)) return(candidate)
  stem <- gsub("[^A-Za-z0-9_]", "_", candidate)
  if (!grepl("^[A-Za-z]", stem)) stem <- paste0("n", stem)
  hash <- substr(paste(as.character(openssl::sha256(charToRaw(enc2utf8(raw)))),
                       collapse = ""), 1L, 10L)
  paste0(substr(stem, 1L, 52L), "_", hash)
}

# --- step 1: closed envelope ----------------------------------------------------

.V1_ROOT_FIELDS <- c("version", "cohort", "anchor", "outputs", "options",
                     "populations", "scope", "harmonization")
.V1_OPTION_FIELDS <- c("translate_concepts", "block_sensitive",
                       "factor_concepts")
.V1_COHORT_FIELDS <- c("type", "cohort_definition_id", "spec", "filter_tree",
                       "episode_policy")
.V1_SPEC_FIELDS <- c("type", "concept_set", "value_threshold", "value_bin",
                     "inclusion_criteria")
.V1_POPULATION_FIELDS <- c("id", "label", "kind", "filter_tree",
                           "cohort_definition_id", "episode_policy",
                           "index_event", "setop", "filters")
.V1_SCOPE_FIELDS <- c("cohort", "tables", "combine", "tables_frames")
.V1_OUTPUT_COMMON <- c("type", "population_id", "filters", "options")
.V1_OUTPUT_FIELDS <- list(
  person_level = c("tables", "representation", "derived_columns"),
  event_level = c("table", "columns", "concept_set", "visit_filter",
                  "concept_col", "temporal", "date_handling",
                  "representation"),
  baseline = c("columns", "derived", "age_breaks"),
  survival = c("outcome", "outcomes", "tar", "censoring", "format",
               "event_order", "washout_days", "tie_policy", "transitions",
               "initial_state", "state_hierarchy", "state_step"),
  cohort_membership = c("date_handling"),
  concept_dictionary = c("source_outputs"),
  intervals_long = c("tables", "concept_filter", "source_filters", "window",
                     "interval_match", "event_select", "select_n",
                     "select_by", "anchor"),
  temporal_covariates = c("table", "concept_set", "bin_width",
                          "window_start", "window_end", "analyses"),
  person_period = c("table", "concept_set", "bin_width", "window_start",
                    "window_end", "analyses", "grain", "time_origin")
)
.V1_OUTPUT_FILTER_FIELDS <- c("concept_set", "time_window", "custom",
                              "visit", "concept_col")
.V1_FEATURE_FIELDS <- c("name", "type", "concept_set", "value_column",
                        "concept_col", "filter", "time_window",
                        "reference_date", "unit", "agg")
.V1_CONCEPT_SET_FIELDS <- c("concepts", "include_descendants",
                            "include_mapped", "exclude")
.V1_DERIVED_FIELDS <- c("kind", "name", "reference", "reference_date",
                        "period_policy")
.V1_OUTCOME_FIELDS <- c("table", "concept_set", "filters")
.V1_TEMPORAL_FIELDS <- c("index_window", "calendar", "event_select", "min_gap")
.V1_INCLUSION_FIELDS <- c("table", "concept_set", "temporal", "occurrence")

#' Closed-envelope validation of a v1 plan (step 1; see the module header)
#'
#' @param plan v1 plan in parsed JSON form.
#' @return Invisibly TRUE; stops with a typed validation error listing every
#'   finding.
#' @keywords internal
.planV1Envelope <- function(plan) {
  errors <- .planV1EnvelopeErrors(plan)
  if (length(errors)) .irRejectAll("v1 plan fails the closed v1 envelope", errors)
  invisible(TRUE)
}

#' Findings of the closed v1 envelope (empty when valid)
#' @keywords internal
.planV1EnvelopeErrors <- function(plan) {
  errors <- character(0)
  add <- function(...) errors <<- c(errors, paste0(...))
  closed <- function(x, allowed, path) {
    if (is.null(x)) return(invisible(FALSE))
    if (!.v1IsObject(x)) {
      add(path, ": must be an object")
      return(invisible(FALSE))
    }
    present <- names(.v1Compact(x))
    unknown <- setdiff(present, allowed)
    if (length(unknown)) {
      add(path, ": unknown field(s) ", paste(unknown, collapse = ", "))
    }
    invisible(TRUE)
  }
  concept_set <- function(x, path) {
    if (.v1IsObject(x) && length(x) > 0L) {
      closed(x, .V1_CONCEPT_SET_FIELDS, path)
      if (.v1IsObject(x[["exclude"]]) && length(x[["exclude"]]) > 0L) {
        add(path, "/exclude: a v1 exclusion is a list of concept ids")
      }
    }
  }
  row_filter <- function(x, path, depth = 1L) {
    if (is.null(x) || (is.list(x) && length(x) == 0L)) return(invisible())
    if (depth > 64L) return(add(path, ": filter nesting is too deep"))
    if (!.v1IsObject(x)) return(add(path, ": a row filter is an and/or group or a leaf object"))
    keys <- names(.v1Compact(x))
    group <- intersect(keys, c("and", "or"))
    if (length(group)) {
      if (length(keys) != 1L) return(add(path, ": a filter group carries exactly one of and/or"))
      items <- .v1Items(x[[group]])
      for (i in seq_along(items)) row_filter(items[[i]], paste0(path, "/", group, "/", i - 1L), depth + 1L)
      return(invisible())
    }
    unknown <- setdiff(keys, c("var", "op", "value", "safe_scope"))
    if (length(unknown)) add(path, ": unknown filter field(s) ", paste(unknown, collapse = ", "))
    if (is.list(x[["safe_scope"]]) && length(x[["safe_scope"]]) > 0L) {
      closed(x[["safe_scope"]], c("table", "column", "concept_id", "concept_col", "n_bins"),
             paste0(path, "/safe_scope"))
    }
  }
  filter_tree <- function(x, path, depth = 1L) {
    if (is.null(x)) return(invisible())
    if (depth > 64L) return(add(path, ": filter nesting is too deep"))
    if (!is.list(x) || length(x) == 0L) return(add(path, ": population filter nodes must be non-empty lists"))
    if (is.null(names(x))) {
      for (i in seq_along(x)) filter_tree(x[[i]], paste0(path, "/", i - 1L), depth + 1L)
      return(invisible())
    }
    keys <- names(x)
    if ("type" %in% keys) {
      if (!setequal(keys, c("type", "params")) && !identical(keys, "type")) {
        add(path, ": a population filter leaf carries only type and params")
      }
      type <- tolower(.v1Text(x[["type"]]) %||% "")
      if (!type %in% .cohortFilterTypes()) {
        return(add(path, ": unknown population filter type"))
      }
      params <- .v1Compact(x[["params"]] %||% list())
      if (!.v1IsObject(params)) return(add(path, "/params: must be an object"))
      unknown <- setdiff(names(params), .cohortFilterParamNames(type))
      if (length(unknown)) {
        add(path, "/params: population filter '", type, "' has unknown parameter(s) ",
            paste(unknown, collapse = ", "))
      }
      if (is.list(params[["window"]]) && length(params[["window"]]) > 0L) {
        closed(params[["window"]], c("start", "end"), paste0(path, "/params/window"))
      }
      if (is.list(params[["safe_scope"]]) && length(params[["safe_scope"]]) > 0L) {
        closed(params[["safe_scope"]], c("table", "column", "concept_id", "concept_col", "n_bins"),
               paste0(path, "/params/safe_scope"))
      }
      return(invisible())
    }
    group <- intersect(keys, c("and", "or"))
    if (length(group) != 1L || length(keys) != 1L) {
      return(add(path, ": unknown or malformed population filter specification"))
    }
    items <- .v1Items(x[[group]])
    for (i in seq_along(items)) filter_tree(items[[i]], paste0(path, "/", group, "/", i - 1L), depth + 1L)
  }
  temporal <- function(x, path) {
    if (!closed(x, .V1_TEMPORAL_FIELDS, path)) return(invisible())
    closed(x[["index_window"]], c("start", "end"), paste0(path, "/index_window"))
    closed(x[["calendar"]], c("start", "end"), paste0(path, "/calendar"))
    closed(x[["event_select"]], c("order", "n", "by"), paste0(path, "/event_select"))
    if (.v1IsObject(x[["min_gap"]]) && length(x[["min_gap"]]) > 0L) {
      closed(x[["min_gap"]], c("days", "by", "keep"), paste0(path, "/min_gap"))
    }
  }
  date_handling <- function(x, path) {
    if (.v1IsObject(x) && length(x) > 0L) {
      closed(x, c("mode", "reference", "bin_width", "date_columns"), path)
    }
  }
  feature_specs <- function(x, path) {
    if (is.null(x)) return(invisible())
    if (!is.list(x)) return(add(path, ": feature specifications are a list"))
    nm <- names(x)
    for (i in seq_along(x)) {
      key <- if (is.null(nm) || !nzchar(nm[[i]])) as.character(i - 1L) else nm[[i]]
      spec <- x[[i]]
      p <- paste0(path, "/", key)
      if (!closed(spec, .V1_FEATURE_FIELDS, p)) next
      concept_set(spec[["concept_set"]], paste0(p, "/concept_set"))
      row_filter(spec[["filter"]], paste0(p, "/filter"))
      closed(spec[["time_window"]], c("start", "end"), paste0(p, "/time_window"))
    }
  }

  if (!.v1IsObject(plan)) return("/: a v1 plan must be a JSON object")
  closed(plan, .V1_ROOT_FIELDS, "")
  if (!is.null(plan[["version"]]) && !identical(as.numeric(.v1Scalar(plan[["version"]])), 1)) {
    add("/version: a v1 plan carries no version or version 1")
  }
  closed(plan[["options"]], .V1_OPTION_FIELDS, "/options")
  closed(plan[["anchor"]], c("table", "id_col"), "/anchor")
  if (!is.null(plan[["scope"]]) && .v1IsObject(plan[["scope"]])) {
    closed(plan[["scope"]], .V1_SCOPE_FIELDS, "/scope")
  }
  if (!is.null(plan[["harmonization"]]) && !.v1IsObject(plan[["harmonization"]])) {
    add("/harmonization: must be an object")
  }

  cohort <- plan[["cohort"]]
  if (closed(cohort, .V1_COHORT_FIELDS, "/cohort")) {
    filter_tree(cohort[["filter_tree"]], "/cohort/filter_tree")
    spec <- cohort[["spec"]]
    if (.v1IsObject(spec) && length(spec) > 0L && is.null(cohort[["filter_tree"]])) {
      closed(spec, .V1_SPEC_FIELDS, "/cohort/spec")
      if (!is.null(spec[["value_threshold"]])) {
        add("/cohort/spec/value_threshold: Disclosive: value_threshold is no longer executable. ",
            "Use a server-issued value_bin from ds.omop.safe.filter.value().")
      }
      concept_set(spec[["concept_set"]], "/cohort/spec/concept_set")
      if (is.list(spec[["value_bin"]]) && length(spec[["value_bin"]]) > 0L) {
        closed(spec[["value_bin"]], c("lower", "upper", "safe_scope"), "/cohort/spec/value_bin")
        closed(spec[["value_bin"]][["safe_scope"]], c("table", "column", "concept_id", "concept_col", "n_bins"),
               "/cohort/spec/value_bin/safe_scope")
      }
      crits <- spec[["inclusion_criteria"]]
      if (is.list(crits)) {
        cn <- names(crits)
        for (i in seq_along(crits)) {
          key <- if (is.null(cn) || !nzchar(cn[[i]])) as.character(i - 1L) else cn[[i]]
          p <- paste0("/cohort/spec/inclusion_criteria/", key)
          crit <- crits[[i]]
          if (!closed(crit, .V1_INCLUSION_FIELDS, p)) next
          concept_set(crit[["concept_set"]], paste0(p, "/concept_set"))
          if (closed(crit[["temporal"]], c("index_window", "calendar"), paste0(p, "/temporal"))) {
            closed(crit[["temporal"]][["index_window"]], c("start", "end"), paste0(p, "/temporal/index_window"))
            closed(crit[["temporal"]][["calendar"]], c("start", "end"), paste0(p, "/temporal/calendar"))
          }
          closed(crit[["occurrence"]], c("min", "max"), paste0(p, "/occurrence"))
        }
      }
    }
  }

  pops <- plan[["populations"]]
  if (!is.null(pops) && !(is.list(pops) && length(pops) == 0L)) {
    if (!.v1IsObject(pops)) {
      add("/populations: must be an object keyed by population id")
    } else {
      for (pid in names(pops)) {
        pop <- pops[[pid]]
        p <- paste0("/populations/", pid)
        if (!closed(pop, .V1_POPULATION_FIELDS, p)) next
        pop <- .v1Compact(pop)
        kind <- .v1Text(pop[["kind"]])
        if (!is.null(pop[["kind"]]) && (is.null(kind) || !tolower(kind) %in% c("criteria", "setop"))) {
          add(p, "/kind: unknown population kind (criteria or setop)")
        }
        is_setop <- identical(tolower(kind %||% ""), "setop") || !is.null(pop[["setop"]])
        if (is_setop) {
          misplaced <- intersect(names(pop), c("filter_tree", "cohort_definition_id",
                                               "index_event", "episode_policy"))
          for (field in misplaced) {
            add(p, ": Set-operation populations cannot declare ", field, ".")
          }
          closed(pop[["setop"]], c("op", "members"), paste0(p, "/setop"))
        }
        filter_tree(pop[["filter_tree"]], paste0(p, "/filter_tree"))
        ie <- pop[["index_event"]]
        if (closed(ie, c("table", "concept_set", "primary_limit", "end_strategy"),
                   paste0(p, "/index_event"))) {
          concept_set(ie[["concept_set"]], paste0(p, "/index_event/concept_set"))
        }
      }
    }
  }

  outputs <- plan[["outputs"]]
  if (!is.null(outputs) && !(is.list(outputs) && length(outputs) == 0L)) {
    if (!.v1IsObject(outputs)) {
      add("/outputs: must be an object keyed by output name")
    } else {
      for (name in names(outputs)) {
        out <- outputs[[name]]
        p <- paste0("/outputs/", name)
        if (!.v1IsObject(out)) {
          add(p, ": must be an object")
          next
        }
        type <- .v1Text(out[["type"]]) %||% "event_level"
        fields <- .V1_OUTPUT_FIELDS[[type]]
        if (is.null(fields)) {
          # The v1 executor names an unsupported type itself; the envelope
          # only keeps every field closed.
          fields <- unique(unlist(.V1_OUTPUT_FIELDS, use.names = FALSE))
        }
        closed(out, c(.V1_OUTPUT_COMMON, fields), p)
        out <- .v1Compact(out)
        f <- out[["filters"]]
        if (.v1IsObject(f) && length(f) > 0L) {
          closed(f, .V1_OUTPUT_FILTER_FIELDS, paste0(p, "/filters"))
          if (.v1IsObject(f[["concept_set"]]) && length(f[["concept_set"]]) > 0L) {
            closed(f[["concept_set"]], "ids", paste0(p, "/filters/concept_set"))
            concept_set(f[["concept_set"]][["ids"]], paste0(p, "/filters/concept_set/ids"))
          }
          closed(f[["time_window"]], c("date_column", "start_date", "end_date"),
                 paste0(p, "/filters/time_window"))
          row_filter(f[["custom"]], paste0(p, "/filters/custom"))
          closed(f[["visit"]], c("concept_ids", "visit_concept_id"), paste0(p, "/filters/visit"))
        }
        concept_set(out[["concept_set"]], paste0(p, "/concept_set"))
        if (.v1IsObject(out[["visit_filter"]]) && length(out[["visit_filter"]]) > 0L) {
          closed(out[["visit_filter"]], c("concept_ids", "visit_concept_id"), paste0(p, "/visit_filter"))
        }
        if (!is.null(out[["temporal"]])) temporal(out[["temporal"]], paste0(p, "/temporal"))
        date_handling(out[["date_handling"]], paste0(p, "/date_handling"))
        rep <- out[["representation"]]
        if (identical(type, "event_level") && .v1IsObject(rep) && length(rep) > 0L) {
          closed(rep, c("format", "features", "grain"), paste0(p, "/representation"))
          feature_specs(rep[["features"]], paste0(p, "/representation/features"))
        }
        if (identical(type, "person_level") && .v1IsObject(out[["tables"]])) {
          for (tbl in names(out[["tables"]])) {
            entry <- out[["tables"]][[tbl]]
            if (.v1IsObject(entry) && length(entry) > 0L && !is.null(entry[["features"]])) {
              closed(entry, c("features", "concept_set", "filters", "concept_col", "visit"),
                     paste0(p, "/tables/", tbl))
              feature_specs(entry[["features"]], paste0(p, "/tables/", tbl, "/features"))
              concept_set(entry[["concept_set"]], paste0(p, "/tables/", tbl, "/concept_set"))
              row_filter(entry[["filters"]], paste0(p, "/tables/", tbl, "/filters"))
            }
          }
        }
        if (identical(type, "person_level")) {
          dc <- out[["derived_columns"]]
          for (i in seq_along(dc %||% list())) {
            key <- if (is.null(names(dc)) || !nzchar(names(dc)[[i]])) as.character(i - 1L) else names(dc)[[i]]
            closed(dc[[i]], .V1_DERIVED_FIELDS, paste0(p, "/derived_columns/", key))
          }
        }
        if (identical(type, "survival")) {
          outcomes <- out[["outcomes"]]
          if (.v1IsObject(outcomes)) {
            for (o in names(outcomes)) {
              closed(outcomes[[o]], .V1_OUTCOME_FIELDS, paste0(p, "/outcomes/", o))
              row_filter(outcomes[[o]][["filters"]], paste0(p, "/outcomes/", o, "/filters"))
            }
          }
          if (!is.null(out[["outcome"]])) closed(out[["outcome"]], .V1_OUTCOME_FIELDS, paste0(p, "/outcome"))
          closed(out[["tar"]], c("start_offset", "end_offset"), paste0(p, "/tar"))
          closed(out[["censoring"]], c("cohort_end", "observation_period_end", "death", "admin_date"),
                 paste0(p, "/censoring"))
        }
        if (identical(type, "baseline") && !is.null(out[["age_breaks"]])) {
          breaks <- unlist(out[["age_breaks"]], use.names = FALSE)
          ok <- is.numeric(breaks) && length(breaks) >= 2L && !anyNA(breaks) &&
            all(is.finite(breaks)) && all(breaks == round(breaks)) &&
            breaks[[1L]] == 0 && all(diff(breaks) > 0)
          if (!ok) {
            add(p, "/age_breaks: invalid age_breaks: at least two strictly increasing ",
                "integers starting at 0 are required")
          }
        }
        if (identical(type, "intervals_long")) {
          sf <- out[["source_filters"]]
          if (.v1IsObject(sf)) {
            for (tbl in names(sf)) row_filter(sf[[tbl]], paste0(p, "/source_filters/", tbl))
          }
          if (.v1IsObject(out[["window"]]) && length(out[["window"]]) > 0L) {
            closed(out[["window"]], c("start", "end", "at"), paste0(p, "/window"))
          }
        }
        if (type %in% c("temporal_covariates", "person_period")) {
          concept_set(out[["concept_set"]], paste0(p, "/concept_set"))
        }
      }
    }
  }
  errors
}
