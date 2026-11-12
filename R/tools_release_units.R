# Module: Unit-aware protected measurement values (dsOMOP 2.10.3, review B13)
#
# A numeric measurement or observation value is only meaningful with its unit.
# Protected per-person reductions therefore run within concept x unit: records
# of one concept in different (or unknown) units are separate strata and are
# never averaged together. A public, versioned concept x canonical-unit scale
# registry (a small conservative built-in set plus the custodian option
# dsomop.release.measurement_scales) gives a stratum public bounds and a grid,
# which enables bounded moments; an unregistered stratum keeps a coarse public
# signed 1-2-5 histogram and unavailable moments. Records are converted to a
# canonical unit only when the registry declares that conversion publicly.

# Built-in public scales: concept x canonical unit, with clinically wide
# bounds (values are clipped to them for the moments; histogram tails are
# explicit).
.DSOMOP_RELEASE_MEASUREMENT_SCALES <- list(
  list(concept_id = 3004410L, unit_concept_id = 8554L, lower = 0, upper = 20,
       label = "Hemoglobin A1c (percent)"),
  list(concept_id = 3025315L, unit_concept_id = 9529L, lower = 0, upper = 500,
       label = "Body weight (kilogram)"),
  list(concept_id = 3036277L, unit_concept_id = 8582L, lower = 0, upper = 300,
       label = "Body height (centimeter)"),
  list(concept_id = 3038553L, unit_concept_id = 9531L, lower = 0, upper = 100,
       label = "Body mass index (kilogram per square meter)"),
  list(concept_id = 3004249L, unit_concept_id = 8876L, lower = 0, upper = 300,
       label = "Systolic blood pressure (millimeter mercury column)"),
  list(concept_id = 3012888L, unit_concept_id = 8876L, lower = 0, upper = 200,
       label = "Diastolic blood pressure (millimeter mercury column)")
)

.dsomopReleaseScaleInteger <- function(x, what) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || !is.finite(x) || x != floor(x) || x < 0 ||
      x > .Machine$integer.max) {
    stop("A public measurement scale has an invalid ", what, ".",
         call. = FALSE)
  }
  as.integer(x)
}

# Finite public bounds lower < upper and a grid within them (default: 20
# equal bins).
.dsomopReleaseScaleBounds <- function(entry) {
  lower <- suppressWarnings(as.numeric(entry$lower))
  upper <- suppressWarnings(as.numeric(entry$upper))
  if (length(lower) != 1L || length(upper) != 1L || !is.finite(lower) ||
      !is.finite(upper) || lower >= upper) {
    stop("A public scale needs finite bounds lower < upper.", call. = FALSE)
  }
  breaks <- entry$breaks %||% seq(lower, upper, length.out = 21L)
  breaks <- sort(unique(as.numeric(breaks)))
  if (length(breaks) < 2L || any(!is.finite(breaks)) ||
      breaks[[1L]] < lower || breaks[[length(breaks)]] > upper) {
    stop("A public scale grid must lie within its bounds.", call. = FALSE)
  }
  list(lower = lower, upper = upper, breaks = breaks)
}

# A custodian scale option as a list of entries (a data frame gives one entry
# per row).
.dsomopReleaseScaleOption <- function(name) {
  custom <- getOption(name, getOption(paste0("default.", name), list()))
  if (is.data.frame(custom)) {
    custom <- lapply(seq_len(nrow(custom)), function(i) as.list(custom[i, ]))
  }
  if (!is.list(custom) || any(!vapply(custom, is.list, logical(1L)))) {
    stop(name, " must be a list of scales.", call. = FALSE)
  }
  custom
}

# Validate one registry entry and complete its public grid.
.dsomopReleaseScaleEntry <- function(entry) {
  if (!is.list(entry)) {
    stop("A public measurement scale must be a list.", call. = FALSE)
  }
  concept <- .dsomopReleaseScaleInteger(entry$concept_id, "concept_id")
  unit <- .dsomopReleaseScaleInteger(entry$unit_concept_id, "unit_concept_id")
  b <- .dsomopReleaseScaleBounds(entry)
  lower <- b$lower
  upper <- b$upper
  breaks <- b$breaks
  convert <- lapply(entry$convert %||% list(), function(cv) {
    from <- .dsomopReleaseScaleInteger(cv$unit_concept_id, "conversion unit")
    factor <- suppressWarnings(as.numeric(cv$factor %||% 1))
    offset <- suppressWarnings(as.numeric(cv$offset %||% 0))
    if (length(factor) != 1L || length(offset) != 1L || !is.finite(factor) ||
        !is.finite(offset) || factor == 0 || from == unit) {
      stop("A public unit conversion needs a finite non-zero factor and a ",
           "different source unit.", call. = FALSE)
    }
    list(unit_concept_id = from, factor = factor, offset = offset)
  })
  id <- paste0("measurement-scale/v1/", substr(.dsomopDpSha256(.dsomopFrame(
    "measurement-scale/v1", concept, unit, lower, upper, breaks,
    lapply(convert, function(cv) c(cv$unit_concept_id, cv$factor,
                                   cv$offset)))), 1L, 16L))
  list(id = id, concept_id = concept, unit_concept_id = unit, lower = lower,
       upper = upper, breaks = breaks, convert = convert,
       label = as.character(entry$label %||% NA_character_))
}

#' The public concept x unit measurement scale registry
#'
#' Built-in entries plus the custodian option
#' \code{dsomop.release.measurement_scales} (a list of entries with
#' \code{concept_id}, \code{unit_concept_id}, \code{lower}, \code{upper}, and
#' optional \code{breaks}, \code{label} and \code{convert}, a list of
#' \code{list(unit_concept_id, factor, offset)} affine conversions into the
#' canonical unit). A custodian entry replaces a built-in entry of the same
#' concept and unit. Public configuration only; never derived from data.
#'
#' @return Named list of validated scales, keyed "concept=<id>|unit=<id>".
#' @keywords internal
.dsomopReleaseMeasurementScales <- function() {
  custom <- .dsomopReleaseScaleOption("dsomop.release.measurement_scales")
  entries <- lapply(c(.DSOMOP_RELEASE_MEASUREMENT_SCALES, custom),
                    .dsomopReleaseScaleEntry)
  keys <- vapply(entries, function(s) paste0("concept=", s$concept_id,
                                              "|unit=", s$unit_concept_id),
                 character(1L))
  # Later (custodian) entries win over built-in ones.
  keep <- !duplicated(keys, fromLast = TRUE)
  stats::setNames(entries[keep], keys[keep])
}

# Stratum key of one concept x unit (NULL and 0 units are the explicit
# "unknown" unit stratum).
.omopReleaseUnitKey <- function(concept_id, unit_concept_id) {
  unit <- suppressWarnings(as.numeric(unit_concept_id))
  unit_key <- ifelse(is.na(unit) | unit == 0, "unknown",
                     format(unit, scientific = FALSE, trim = TRUE))
  paste0("concept=", format(as.numeric(concept_id), scientific = FALSE,
                            trim = TRUE), "|unit=", unit_key)
}

# Whether a domain table records units (some CDMs' observation tables do
# not); without one every value is in the unknown-unit stratum.
.omopReleaseHasUnit <- function(handle, src) {
  cols <- tryCatch(
    .buildBlueprint(handle)$columns[[tolower(src$table_name %||% "")]]$column_name,
    error = function(e) NULL)
  is.null(cols) || "unit_concept_id" %in% tolower(cols)
}

# SQL expressions applying the registry's public conversions (value and
# effective unit) to records of the alias (default e).
.omopReleaseUnitConversionSql <- function(src, scales, alias = "e",
                                          has_unit = TRUE) {
  a <- paste0(.validateIdentifier(alias, "alias"), ".")
  value <- paste0(a, src$value_col)
  if (!isTRUE(has_unit)) {
    return(list(value = paste0("CAST(", value, " AS FLOAT)"), unit = "0"))
  }
  unit <- paste0("COALESCE(", a, "unit_concept_id, 0)")
  cases_v <- character(0)
  cases_u <- character(0)
  for (s in scales) {
    for (cv in s$convert) {
      cond <- paste0(a, src$concept_col, " = ", s$concept_id,
                     " AND ", a, "unit_concept_id = ", cv$unit_concept_id)
      cases_v <- c(cases_v, paste0("WHEN ", cond, " THEN ",
                                   format(cv$factor, digits = 17), " * ",
                                   "CAST(", value, " AS FLOAT) + ",
                                   format(cv$offset, digits = 17)))
      cases_u <- c(cases_u, paste0("WHEN ", cond, " THEN ",
                                   s$unit_concept_id))
    }
  }
  if (length(cases_v)) {
    value <- paste0("(CASE ", paste(cases_v, collapse = " "),
                    " ELSE CAST(", value, " AS FLOAT) END)")
    unit <- paste0("(CASE ", paste(cases_u, collapse = " "), " ELSE ", unit,
                   " END)")
  } else {
    value <- paste0("CAST(", value, " AS FLOAT)")
  }
  list(value = value, unit = unit)
}

#' Unit-aware per-person mean values of a concept domain in the scope
#'
#' One row per (concept, canonical unit, person): the person's mean value of
#' the concept's records in that unit (after the registry's public
#' conversions), with concept and unit names. Records in different or unknown
#' units are never averaged together.
#'
#' @param handle CDM handle.
#' @param ctx Run-path context (scoped cohort).
#' @param domain_code Measurement ("3") or observation ("4") domain.
#' @param extra_where Optional SQL predicates over the record alias \code{e}.
#' @return Data frame: covariate_id, covariate_name, unit_concept_id,
#'   unit_name, person_id, v.
#' @keywords internal
.omopReleaseConceptUnitValues <- function(handle, ctx, domain_code,
                                          extra_where = character(0)) {
  cohort <- .validateIdentifier(ctx$scoped_cohort, "cohort")
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  src <- .omopCovariateSource(handle, domain_code)
  if (is.na(src$value_col)) {
    stop("Unit-aware values need a measurement or observation domain.",
         call. = FALSE)
  }
  .omopDiagAssertPersons(handle, ctx, src$table, "e", src$person_col)
  conv <- .omopReleaseUnitConversionSql(src, .dsomopReleaseMeasurementScales(),
                                        has_unit = .omopReleaseHasUnit(handle,
                                                                       src))
  where <- c(paste0("e.", src$value_col, " IS NOT NULL"), extra_where)
  raw <- .omopReleaseFetch(handle, paste0(
    "SELECT pp.covariate_id, cc.concept_name AS covariate_name, ",
    "pp.unit_concept_id, uu.concept_name AS unit_name, pp.person_id, pp.v ",
    "FROM (SELECT e.", src$concept_col, " AS covariate_id, ", conv$unit,
    " AS unit_concept_id, e.", src$person_col, " AS person_id, AVG(",
    conv$value, ") AS v FROM ", src$table,
    " e INNER JOIN (SELECT DISTINCT subject_id FROM ", cohort,
    ") c ON c.subject_id = e.", src$person_col, " WHERE ",
    paste(where, collapse = " AND "), " GROUP BY e.", src$concept_col, ", ",
    conv$unit, ", e.", src$person_col, ") pp LEFT JOIN ", concept,
    " cc ON cc.concept_id = pp.covariate_id LEFT JOIN ", concept,
    " uu ON uu.concept_id = pp.unit_concept_id"))
  if (!nrow(raw)) {
    return(data.frame(covariate_id = numeric(0), covariate_name = character(0),
                      unit_concept_id = numeric(0), unit_name = character(0),
                      person_id = character(0), v = numeric(0),
                      stringsAsFactors = FALSE))
  }
  raw
}

# Person-level substrate of concept x unit strata.
.omopReleaseUnitSubstrate <- function(raw) {
  if (!is.data.frame(raw) || nrow(raw) == 0L) {
    return(data.frame(person_id = character(0), stratum = character(0),
                      stratum_order = numeric(0), v = numeric(0),
                      stringsAsFactors = FALSE))
  }
  sub <- data.frame(
    person_id = raw$person_id,
    stratum = .omopReleaseUnitKey(raw$covariate_id, raw$unit_concept_id),
    stratum_order = suppressWarnings(as.numeric(raw$covariate_id)),
    v = suppressWarnings(as.numeric(raw$v)), stringsAsFactors = FALSE)
  sub[!is.na(sub$stratum_order), , drop = FALSE]
}

# The owner of one scale group: a registered concept x unit scale (bounded
# HM moments on its grid) or the generic unregistered owner (a public signed
# 1-2-5 histogram, moments unavailable).
.omopReleaseUnitOwner <- function(owner_base, estimand, scale = NULL) {
  K <- .DSOMOP_RELEASE_MULTI_CAP
  if (is.null(scale)) {
    grid <- .dsomopReleaseGrid("signed_125")
    return(.dsomopReleaseOwner(
      id = paste0(owner_base, "/unregistered-scale/v1"), family = "HM",
      grid = grid, strata = list(kind = "data"), cap = K, bounds = NULL,
      estimand = paste0(estimand, " within concept x unit (unknown units a ",
                        "separate stratum); each person counted in at most ",
                        K, " concept x unit strata (lowest concept ids); no ",
                        "registered public scale, so the mean and SD are ",
                        "unavailable and quantiles come from a public ",
                        "1-2-5 histogram")))
  }
  grid <- list(id = scale$id, breaks = scale$breaks, lower = scale$lower,
               upper = scale$upper, unit = "registered")
  .dsomopReleaseOwner(
    id = paste0(owner_base, "/", scale$id), family = "HM", grid = grid,
    strata = list(kind = "data"), cap = K,
    bounds = c(scale$lower, scale$upper),
    estimand = paste0(estimand, " of concept ", scale$concept_id, " in unit ",
                      scale$unit_concept_id, " (registered public scale [",
                      scale$lower, ", ", scale$upper, "]; values clipped for ",
                      "the moments); each person counted in at most ", K,
                      " concept x unit strata (lowest concept ids)"))
}

#' Build unit-aware protected strata over several public scales
#'
#' The contribution cap K is applied once to each person's complete concept x
#' unit expansion (ascending public concept order), then strata are grouped by
#' their public scale (one owner per registered scale, one generic owner for
#' the rest). Displayed strata are ranked on their released person totals only
#' (top N), and every owner then draws its full components.
#'
#' @param route Protected route.
#' @param raw Output of \code{.omopReleaseConceptUnitValues}.
#' @param owner_base Scientific owner prefix (reducer, domain, window).
#' @param estimand Estimand text.
#' @param top_n Number of strata to display, or NULL for \code{keys}.
#' @param keys Explicit stratum keys to release (overlays), or NULL.
#' @return List: \code{built} (owner builds) and \code{strata} (one row per
#'   released stratum with concept, unit and displayed summaries).
#' @keywords internal
.omopReleaseUnitStrataBuild <- function(route, raw, owner_base, estimand,
                                        top_n = NULL, keys = NULL) {
  sub <- .omopReleaseUnitSubstrate(raw)
  sub <- sub[!is.na(sub$person_id) & is.finite(sub$v), , drop = FALSE]
  sub$person_id <- .dsomopReleasePersonIdText(sub$person_id)
  scales <- .dsomopReleaseMeasurementScales()
  K <- .DSOMOP_RELEASE_MULTI_CAP
  sub <- .dsomopReleaseCapSubstrate(sub, list(strata = list(kind = "data"),
                                              cap = K))
  # Exact support of the whole eligible population (the parent basis) before
  # any release, with the generic disclosure error.
  floor_n <- .dsomopReleaseSupport()$floor
  if (length(unique(sub$person_id)) < floor_n) .dsomopReleaseDisclosive()
  # One group per registered scale; every other stratum in the generic
  # unregistered group (a key no stratum can take).
  scale_key <- ifelse(sub$stratum %in% names(scales), sub$stratum,
                      "unregistered")
  # A scale group below the support floor has no supported stratum.
  groups <- sort(unique(scale_key), method = "radix")
  persons <- vapply(groups, function(g) {
    length(unique(sub$person_id[scale_key == g]))
  }, numeric(1L))
  groups <- groups[persons >= floor_n]
  owners <- stats::setNames(lapply(groups, function(g) {
    .omopReleaseUnitOwner(owner_base, estimand,
                          if (identical(g, "unregistered")) NULL else
                            scales[[g]])
  }), groups)
  group_sub <- lapply(groups, function(g) sub[scale_key == g, , drop = FALSE])
  names(group_sub) <- groups
  # First pass: released person totals only, to choose the displayed strata.
  if (is.null(keys)) {
    totals <- do.call(rbind, lapply(groups, function(g) {
      first <- .dsomopReleaseBuild(route$policy, route$protection,
                                   owners[[g]], group_sub[[g]],
                                   n_only = TRUE)
      if (nrow(first$strata) == 0L) return(NULL)
      first$strata[, c("stratum", "n_display")]
    }))
    if (is.null(totals) || nrow(totals) == 0L) {
      return(list(built = list(), strata = data.frame()))
    }
    concept <- suppressWarnings(as.numeric(sub("^concept=([0-9.]+)\\|.*$",
                                               "\\1", totals$stratum)))
    ord <- order(-totals$n_display, concept, totals$stratum, method = "radix")
    keys <- utils::head(totals$stratum[ord],
                        max(0L, as.integer(top_n %||% 50L)))
  }
  built <- list()
  rows <- list()
  for (g in groups) {
    want <- intersect(keys, unique(group_sub[[g]]$stratum))
    if (!length(want)) next
    b <- .dsomopReleaseBuild(route$policy, route$protection, owners[[g]],
                             group_sub[[g]], data_strata_keys = want)
    if (nrow(b$strata) == 0L) next
    built[[length(built) + 1L]] <- b
    rows[[length(rows) + 1L]] <- b$strata
  }
  strata <- if (length(rows)) do.call(rbind, rows) else data.frame()
  list(built = built, strata = strata)
}

# One output row per released concept x unit stratum, in the fe.continuous
# column order (ranked on released person totals, then concept and unit).
.omopReleaseUnitRows <- function(strata, raw, extra_cols = list()) {
  if (!is.data.frame(strata) || nrow(strata) == 0L) return(data.frame())
  concept <- suppressWarnings(as.numeric(sub("^concept=([0-9.]+)\\|.*$", "\\1",
                                             strata$stratum)))
  unit_key <- sub("^.*\\|unit=", "", strata$stratum)
  unit <- ifelse(unit_key == "unknown", 0,
                 suppressWarnings(as.numeric(unit_key)))
  raw_key <- .omopReleaseUnitKey(raw$covariate_id, raw$unit_concept_id)
  first <- match(strata$stratum, raw_key)
  out <- data.frame(
    covariate_id = concept,
    covariate_name = as.character(raw$covariate_name[first]),
    unit_concept_id = unit,
    unit_name = ifelse(unit == 0, "unknown",
                       as.character(raw$unit_name[first])),
    count_value = strata$n_display, n_persons = strata$n_display,
    stringsAsFactors = FALSE)
  out <- cbind(out, .omopReleaseDistColumns(strata)[, -1L, drop = FALSE])
  for (name in rev(names(extra_cols))) {
    out <- cbind(stats::setNames(data.frame(rep(extra_cols[[name]],
                                                nrow(out)),
                                            stringsAsFactors = FALSE), name),
                 out)
  }
  out <- out[order(-out$count_value, out$covariate_id, out$unit_concept_id,
                   method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}
