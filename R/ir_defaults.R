# Normalisation of a plan in the language 2.0 form (design-2.10.3 sections
# 4.1, 4.4 and 6.1): every default written in the schema, plus the documented
# semantic defaults (the rule set of change request CR-11), expanded exactly
# once, so that two spellings of one plan normalise identically (the
# canonical example A of WS-0 writes these semantic defaults out; leaving
# them implicit gives the same normal form). A field with neither a schema
# nor a semantic default stays absent.
#
# Shared by client and server: the identical file is vendored in
# dsOMOPClient (tests pin the byte identity), because the semantic digest
# (.planSemanticDigest(), section 4.4) and the byte-identity test of section
# 10.1 compare NORMALISED forms produced by one normaliser on both sides. The
# server passes ir-v2.json (wire schema plus the closed compatibility
# variants); the client passes plan-v2.json. The two give the same result for
# a wire plan: the only additions of ir-v2.json are oneOf branches no wire
# instance can match and IR-only `legacy_*` properties, whose defaults are
# never inserted (they appear only where the v1 lowering sets them).
#
# Instances are R values as produced by jsonlite::fromJSON(simplifyVector =
# FALSE). Requires json_schema.R (.jsonSchemaResolveRef, .jsonInstanceType,
# .jsonArrayItems, .jsv_quiet).

#' Expand the defaults a schema declares for an instance
#'
#' Walks the schema alongside the instance: an absent property that declares
#' a \code{default} receives it (and the inserted value is expanded in turn);
#' \code{oneOf}/\code{anyOf} expand the branch the instance matches,
#' \code{if}/\code{then}/\code{else} the selected branch, \code{allOf} every
#' member. Properties named \code{legacy_*} are never inserted.
#'
#' @param x Instance.
#' @param schema Schema node (list or boolean).
#' @param root Root schema for \code{$ref} resolution.
#' @return The instance with its defaults.
#' @keywords internal
.irExpandDefaults <- function(x, schema, root = schema) {
  if (!is.list(schema) || length(schema) == 0L) return(x)
  if (!is.null(schema[["$ref"]])) {
    x <- .irExpandDefaults(x, .jsonSchemaResolveRef(schema[["$ref"]], root),
                           root)
  }
  itype <- tryCatch(.jsonInstanceType(x), error = function(e) "unsupported")
  if (identical(itype, "object")) {
    props <- schema[["properties"]]
    for (nm in names(props)) {
      ps <- props[[nm]]
      if (!nm %in% names(x)) {
        if (startsWith(nm, "legacy_") || !is.list(ps) ||
            !"default" %in% names(ps)) next
        x[nm] <- list(ps[["default"]])
      }
      x[nm] <- list(.irExpandDefaults(x[[nm]], ps, root))
    }
    ap <- schema[["additionalProperties"]]
    if (is.list(ap) && length(ap) > 0L) {
      for (nm in setdiff(names(x), names(props))) {
        x[nm] <- list(.irExpandDefaults(x[[nm]], ap, root))
      }
    }
  } else if (identical(itype, "array") && !is.null(schema[["items"]])) {
    x <- lapply(.jsonArrayItems(x), .irExpandDefaults,
                schema = schema[["items"]], root = root)
  }
  for (member in .jsonArrayItems(schema[["allOf"]] %||% list())) {
    x <- .irExpandDefaults(x, member, root)
  }
  alternatives <- schema[["oneOf"]] %||% schema[["anyOf"]]
  if (!is.null(alternatives)) {
    branches <- .jsonArrayItems(alternatives)
    hit <- which(vapply(branches, function(s) .jsv_quiet(s, x, root, ""),
                        logical(1)))
    if (length(hit) >= 1L) x <- .irExpandDefaults(x, branches[[hit[[1L]]]], root)
  }
  if (!is.null(schema[["if"]])) {
    branch <- if (.jsv_quiet(schema[["if"]], x, root, "")) {
      schema[["then"]]
    } else {
      schema[["else"]]
    }
    if (!is.null(branch)) x <- .irExpandDefaults(x, branch, root)
  }
  x
}

# An empty JSON object as parsed by jsonlite (simplifyVector = FALSE).
.irEmptyObject <- function() structure(list(), names = character(0))

# Is a population id (transitively) episode-indexed? index and match
# populations are; a set-op is when any member is; a criteria child inherits
# the episode roster of an indexed parent (section 2.5; CR-11).
.irPopulationIndexed <- function(populations, id, seen = character(0)) {
  if (!is.character(id) || length(id) != 1L || id %in% seen) return(FALSE)
  pop <- populations[[id]]
  if (!is.list(pop)) return(FALSE)
  kind <- pop[["kind"]]
  if (!is.character(kind) || length(kind) != 1L) return(FALSE)
  seen <- c(seen, id)
  switch(kind,
    index = TRUE,
    match = TRUE,
    setop = {
      members <- .jsonArrayItems(pop[["setop"]][["members"]] %||% list())
      any(vapply(members, function(m) .irPopulationIndexed(populations, m, seen),
                 logical(1)))
    },
    criteria = {
      parent <- pop[["parent"]]
      is.character(parent) && length(parent) == 1L &&
        .irPopulationIndexed(populations, parent, seen)
    },
    FALSE
  )
}

#' Normalise a language 2.0 plan or IR: schema defaults plus semantic defaults
#'
#' The schema \code{default} keywords, applied in the branch each instance
#' matches, then the semantic defaults the design states in prose (sections
#' 1.3, 2.5, 2.9, 2.10, 4.1; the rule set of change request CR-11, which the
#' canonical example A of WS-0 writes out): absent root \code{options}
#' and registries are empty objects; an absent \code{publish} is the output
#' names in declaration order; an index or criteria population without
#' \code{coverage} takes \code{options.coverage} (or the coverage defaults),
#' with \code{on_missing = "keep"} when its \code{restrict_to_observation} is
#' false; an output without \code{grain} takes \code{episode} over an indexed
#' population and \code{person} otherwise; a \code{wide} output with
#' \code{columns: null} takes every variable in declaration order; survival
#' \code{outcome_mode} and \code{event_order} take the format matrix defaults.
#' No other field is added and none is removed. Pure: no database access, no
#' validation (conflicts are reported by the validator).
#'
#' @param plan Instance in parsed JSON form.
#' @param schema Root schema: ir-v2.json on the server, plan-v2.json on the
#'   client.
#' @return The normalised instance.
#' @keywords internal
.irNormalizeWire <- function(plan, schema) {
  if (!is.list(plan) || is.null(names(plan))) return(plan)
  for (map in c("concept_sets", "events", "intervals", "populations",
                "variables")) {
    if (!map %in% names(plan) || is.null(plan[[map]]) ||
        (is.list(plan[[map]]) && length(plan[[map]]) == 0L)) {
      plan[map] <- list(.irEmptyObject())
    }
  }
  if (!"options" %in% names(plan) || is.null(plan[["options"]])) {
    plan["options"] <- list(.irEmptyObject())
  }
  plan <- .irExpandDefaults(plan, schema, schema)

  outputs <- plan[["outputs"]]
  output_names <- if (is.list(outputs)) names(outputs) %||% character(0) else character(0)
  publish <- plan[["publish"]]
  publish <- if (is.null(publish)) output_names else unlist(publish, use.names = FALSE)
  plan[["publish"]] <- as.list(as.character(unlist(publish, use.names = FALSE)))

  pops <- plan[["populations"]]
  coverage_schema <- .jsonSchemaResolveRef("#/$defs/coverage", schema)
  plan_coverage <- plan[["options"]][["coverage"]]
  for (pid in names(pops)) {
    pop <- pops[[pid]]
    kind <- if (is.list(pop)) pop[["kind"]] %||% "" else ""
    if (!identical(kind, "index") && !identical(kind, "criteria")) next
    if (is.null(pop[["coverage"]])) {
      coverage <- plan_coverage %||% .irEmptyObject()
      if (identical(pop[["kind"]], "index") &&
          isFALSE(pop[["index"]][["restrict_to_observation"]])) {
        coverage[["on_missing"]] <- "keep"
      }
      pop["coverage"] <- list(.irExpandDefaults(coverage, coverage_schema, schema))
    }
    pops[[pid]] <- pop
  }
  if (length(pops) > 0L) plan[["populations"]] <- pops

  for (nm in output_names) {
    out <- outputs[[nm]]
    if (!is.list(out)) next
    if (is.null(out[["grain"]]) && is.character(out[["rows"]])) {
      out[["grain"]] <- if (.irPopulationIndexed(pops, out[["rows"]])) {
        "episode"
      } else {
        "person"
      }
    }
    fmt <- out[["format"]] %||% ""
    if (identical(fmt, "wide") && is.null(out[["columns"]])) {
      out["columns"] <- list(as.list(names(plan[["variables"]]) %||% character(0)))
    }
    opts <- out[["options"]]
    if (is.list(opts) && identical(fmt, "survival")) {
      sformat <- opts[["format"]] %||% "survival"
      if (is.null(opts[["outcome_mode"]])) {
        opts[["outcome_mode"]] <- if (identical(sformat, "survival")) {
          "composite"
        } else {
          "named"
        }
      }
      if (is.null(opts[["event_order"]])) {
        opts[["event_order"]] <- if (sformat %in% c("recurrent_events",
                                                    "counting_process",
                                                    "multi_state")) {
          "all"
        } else {
          "first"
        }
      }
    }
    if (!is.null(opts)) out["options"] <- list(opts)
    outputs[[nm]] <- out
  }
  if (length(output_names) > 0L) plan[["outputs"]] <- outputs
  plan
}
