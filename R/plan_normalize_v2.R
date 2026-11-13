# The normal form of a version-2 plan (amendment A-11 to design-2.10.3).
#
# This file is a frozen WS-0 artefact vendored BYTE-IDENTICALLY in dsOMOP and
# dsOMOPClient (test-ws0-schema.R pins the identity); the Python reference is
# tools/ws0/normalize_reference.py and inst/schema/normalized-vectors.json pins
# the normal forms of the canonical examples and the acceptance plans. The
# semantic digest (R/semantic_digest.R) hashes this normal form, so the
# federation harmonisation signature and the DP provenance capsule agree on
# both sides. Package wrappers may adapt input representations but never add
# defaulting or population-indexedness rules of their own. Wire validation
# precedes normalisation; nothing here repairs an invalid plan.

.pnv2Cache <- new.env(parent = emptyenv())

.pnv2EmptyObject <- function() structure(list(), names = character(0))

.pnv2Array <- function(x) {
  if (is.null(x)) return(list())
  unname(as.list(x))
}

# Plain lists only: S3 classes and attributes other than names are dropped.
.pnv2Plain <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    nms <- names(x)
    out <- lapply(x, .pnv2Plain)
    if (is.null(nms)) return(unname(out))
    return(structure(out, names = nms))
  }
  x
}

.pnv2Schema <- function(schema = NULL) {
  if (!is.null(schema)) return(schema)
  if (is.null(.pnv2Cache$plan)) {
    pkg <- utils::packageName()
    if (is.null(pkg)) pkg <- "dsOMOP"
    .pnv2Cache$plan <- .jsonSchemaPackaged("plan-v2", package = pkg)
  }
  .pnv2Cache$plan
}

#' Apply a JSON Schema's `default` keywords to an instance
#'
#' Defaults are added in the schema branch the instance matches (properties,
#' additionalProperties, items, allOf, the first matching oneOf/anyOf branch,
#' if/then/else), recursively. Nothing else is added.
#'
#' @param schema Parsed schema node.
#' @param x Instance (as parsed with `simplifyVector = FALSE`).
#' @param root Root schema for `$ref` resolution.
#' @return The instance with every absent defaulted property present.
#' @keywords internal
.pnv2ApplyDefaults <- function(schema, x, root) {
  if (is.logical(schema) || !is.list(schema)) return(x)
  if (!is.null(schema[["$ref"]])) {
    x <- .pnv2ApplyDefaults(.jsonSchemaResolveRef(schema[["$ref"]], root), x, root)
  }
  type <- tryCatch(.jsonInstanceType(x), error = function(e) "unsupported")
  if (identical(type, "object")) {
    props <- schema[["properties"]]
    if (!is.null(props)) {
      for (nm in names(props)) {
        if (!nm %in% names(x) && is.list(props[[nm]]) && "default" %in% names(props[[nm]])) {
          x[nm] <- list(props[[nm]][["default"]])
        }
      }
      for (nm in intersect(names(props), names(x))) {
        x[nm] <- list(.pnv2ApplyDefaults(props[[nm]], x[[nm]], root))
      }
    }
    extra <- schema[["additionalProperties"]]
    if (is.list(extra)) {
      for (nm in setdiff(names(x), names(props))) {
        x[nm] <- list(.pnv2ApplyDefaults(extra, x[[nm]], root))
      }
    }
  }
  if (identical(type, "array") && !is.null(schema[["items"]])) {
    x <- lapply(.jsonArrayItems(x), function(item) .pnv2ApplyDefaults(schema[["items"]], item, root))
  }
  for (sub in .jsonArrayItems(schema[["allOf"]])) {
    x <- .pnv2ApplyDefaults(sub, x, root)
  }
  for (key in c("oneOf", "anyOf")) {
    branches <- .jsonArrayItems(schema[[key]])
    if (length(branches)) {
      hits <- vapply(branches, function(sub) .jsv_quiet(sub, x, root, ""), logical(1))
      if (any(hits)) x <- .pnv2ApplyDefaults(branches[[which(hits)[[1L]]]], x, root)
    }
  }
  if (!is.null(schema[["if"]])) {
    branch <- if (.jsv_quiet(schema[["if"]], x, root, "")) "then" else "else"
    if (!is.null(schema[[branch]])) x <- .pnv2ApplyDefaults(schema[[branch]], x, root)
  }
  x
}

#' Is a population indexed (does its roster carry episodes)?
#'
#' `index` and `match` populations are indexed; a `criteria` population
#' inherits from its parent; a `union` is indexed when any member is;
#' `intersect` and `setdiff` inherit from their first (left) member, whose
#' episodes they keep (design 6.9). A set-op's parent only narrows membership.
#' @keywords internal
.pnv2PopulationIndexed <- function(populations, id, seen = character(0)) {
  p <- populations[[id]]
  if (!is.list(p) || is.null(id) || id %in% seen) return(FALSE)
  kind <- p$kind
  # A legacy criteria base's parent supplies persons only. Its fresh OP rows
  # do not inherit the parent's index episodes (unlike a singleton set-op).
  if (identical(kind, "criteria") && identical(p$legacy_episodes, "observation_periods")) return(FALSE)
  if (identical(kind, "index") || identical(kind, "match")) return(TRUE)
  seen <- c(seen, id)
  if (identical(kind, "setop")) {
    members <- as.character(unlist(p$setop$members, use.names = FALSE))
    if (!length(members)) return(FALSE)
    if (identical(p$setop$op, "union")) {
      return(any(vapply(members, function(m) .pnv2PopulationIndexed(populations, m, seen), logical(1))))
    }
    return(.pnv2PopulationIndexed(populations, members[[1L]], seen))
  }
  if (identical(kind, "criteria") && !is.null(p$parent)) {
    return(.pnv2PopulationIndexed(populations, p$parent, seen))
  }
  FALSE
}

.pnv2DefaultEventOrder <- function(format) {
  if (format %in% c("recurrent_events", "counting_process", "multi_state")) "all" else "first"
}

.PNV2_DEFAULT_COVERAGE <- list(policy = "unique", on_ambiguous = "drop", on_missing = "drop", gap = 0L)

#' Normalise a version-2 plan: every default expanded exactly once
#'
#' Rules (amendment A-11), in this order: an absent root `options` is `{}` and
#' absent registries are `{}`; the schema `default` keywords are applied in the
#' branch each instance matches; an absent `publish` is the output names in
#' declaration order; an `index`/`criteria` population without `coverage` takes
#' `options.coverage`, else unique/drop/drop/0 (`on_missing = "keep"` when its
#' `index.restrict_to_observation` is false); an output without `grain` takes
#' `episode` when its rows population is indexed and `person` otherwise; a
#' `wide` output with `columns: null` takes every variable in declaration
#' order; a `survival` output without `event_order` takes `first` for
#' survival/competing_risk and `all` otherwise, and without `outcome_mode`
#' takes `composite` for survival and `named` otherwise. No other field is
#' added and none is removed; the result is a fixed point.
#'
#' @param plan A version-2 plan (a parsed JSON list or a plan object).
#' @param schema Optional parsed plan-v2 schema (the packaged one by default).
#' @return The normalised plan as a plain list (no S3 classes).
#' @keywords internal
.planNormalizeV2 <- function(plan, schema = NULL) {
  x <- .pnv2Plain(plan)
  if (!is.list(x) || is.null(names(x))) stop("A version-2 plan is a JSON object.", call. = FALSE)
  version <- x$version
  if (!is.numeric(version) || length(version) != 1L || is.na(version) || version != 2) {
    stop("Only version 2 plans are normalised.", call. = FALSE)
  }
  root <- .pnv2Schema(schema)
  empty <- .pnv2EmptyObject()
  if (is.null(x$options)) x["options"] <- list(empty)
  for (slot in c("concept_sets", "events", "intervals", "populations", "variables")) {
    if (is.null(x[[slot]])) x[slot] <- list(empty)
  }
  x <- .pnv2ApplyDefaults(root, x, root)
  x <- .datePlanNormalize(x)
  if (is.null(x$publish)) x["publish"] <- list(.pnv2Array(names(x$outputs)))
  plan_cov <- x$options$coverage
  for (id in names(x$populations)) {
    p <- x$populations[[id]]
    if (identical(p$kind, "index") || identical(p$kind, "criteria")) {
      if (is.null(p$coverage)) {
        cov <- if (!is.null(plan_cov)) plan_cov else .PNV2_DEFAULT_COVERAGE
        if (identical(p$kind, "index") && isFALSE(p$index$restrict_to_observation)) cov$on_missing <- "keep"
        p$coverage <- cov
        x$populations[[id]] <- p
      }
    }
  }
  for (nm in names(x$outputs)) {
    o <- x$outputs[[nm]]
    if (is.null(o$grain)) {
      o$grain <- if (.pnv2PopulationIndexed(x$populations, o$rows)) "episode" else "person"
    }
    if (identical(o$format, "wide") && is.null(o$columns)) {
      o["columns"] <- list(.pnv2Array(names(x$variables)))
    }
    if (identical(o$format, "survival")) {
      if (is.null(o$options)) o["options"] <- list(empty)
      f <- o$options$format
      if (is.null(f)) f <- "survival"
      if (is.null(o$options$event_order)) o$options$event_order <- .pnv2DefaultEventOrder(f)
      if (is.null(o$options$outcome_mode)) {
        o$options$outcome_mode <- if (identical(f, "survival")) "composite" else "named"
      }
    }
    x$outputs[[nm]] <- o
  }
  x
}

#' Semantic digest of a version-2 plan over its normal form
#' @param plan A version-2 plan.
#' @param schema Optional parsed plan-v2 schema.
#' @return `list(payload, sha256)` as returned by `.planSemanticDigest()`.
#' @keywords internal
.planNormalizedDigest <- function(plan, schema = NULL) {
  .planSemanticDigest(.planNormalizeV2(plan, schema))
}
