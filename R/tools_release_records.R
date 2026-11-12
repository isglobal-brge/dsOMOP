# Module: Capped record-weighted moments of the QueryLibrary summaries
# (dsOMOP 2.10.3, review section 6)
#
# Eight reviewed QueryLibrary summaries report record-weighted statistics: per
# stratum (concept, concept x unit, concept x currency or start year) the
# persons, the records and the mean (and SD) of a value over the records.
# Their protected route keeps that estimand under a whole-person cap: each
# person's eligible records are taken in a fixed public order (event date,
# then the table's stable key) and only the first K = 10 are kept, across all
# strata, before any stratum is selected or displayed. Each kept value is
# clipped to a public scale [L, U] and normalized to u = (x - L) / (U - L) in
# [0, 1]; the record contributes round(J u) to S and round(J u^2) to Q
# (J = 65535). Per stratum the release draws the eligible-person total N (the
# shared person-count owner; sensitivity K, since a person is in at most K
# strata), the capped record count R (sensitivity K) and S and Q (sensitivity
# J K each). The capped record-weighted mean and SD are post-processing of R,
# S and Q only; N is never the record denominator. Without a public scale
# (quantities, costs and measurements whose concept x unit or currency is not
# registered) only N and R are released and the moments are unavailable.

# Static allocations of the reference epsilon: N belongs to the shared
# eligible-person owner, R, S and Q to the record-moment owner.
.DSOMOP_RELEASE_RECORD_FRACTIONS <- list(
  mean_sd = list(N = 0.2, R = 0.3, S = 0.3, Q = 0.2),
  mean = list(N = 0.2, R = 0.4, S = 0.4)
)

# The public calendar-month duration grid; values above 360 months are its
# declared tail (clipped to 360 for the moments).
.DSOMOP_RELEASE_MONTH_BREAKS <- c(0, 1, 2, 3, 4, 6, 9, 12, 18, 24, 36, 48, 60,
                                  120, 240, 360)

.DSOMOP_RELEASE_COST_MEASURES <- c("cost_per_unit", "out_of_pocket")

# The reviewed record-weighted summaries and the public scale of their value.
.DSOMOP_RELEASE_RECORD_SUMMARIES <- list(
  "condition_era.length_stats" = list(scale = "days"),
  "drug_era.length_stats" = list(scale = "days"),
  "observation_period.length_stats" = list(scale = "days"),
  "observation_period.length_months_stats" = list(scale = "months"),
  "drug.quantity_stats" = list(scale = "quantity"),
  "measurement.value_stats" = list(scale = "measurement"),
  "drug_cost.cost_per_unit_stats" = list(scale = "cost",
                                         measure = "cost_per_unit"),
  "drug_cost.out_of_pocket_stats" = list(scale = "cost",
                                         measure = "out_of_pocket")
)

# --- Public scales ------------------------------------------------------------

# Durations: days [0, 10957] on the duration grid D; calendar months
# [0, 360] on their own grid (never days / 30).
.dsomopReleaseRecordDurationScale <- function(unit) {
  if (identical(unit, "days")) {
    g <- .dsomopReleaseGrid("duration_days")
    return(list(id = paste0("record-scale/", g$id), lower = g$lower,
                upper = g$upper, breaks = g$breaks, unit = "days"))
  }
  list(id = "record-scale/duration-months-v1", lower = 0, upper = 360,
       breaks = .DSOMOP_RELEASE_MONTH_BREAKS, unit = "calendar months")
}

#' The custodian's public drug quantity scales
#'
#' \code{dsomop.release.quantity_scales}: a list of entries with
#' \code{concept_id} (drug concept), \code{lower}, \code{upper} and optional
#' \code{breaks} and \code{label}. There are no built-in entries: a drug's
#' recorded quantity has no defensible universal bounds. Public configuration
#' only; never derived from data.
#'
#' @return Named list of validated scales, keyed "concept=<id>".
#' @keywords internal
.dsomopReleaseQuantityScales <- function() {
  entries <- lapply(.dsomopReleaseScaleOption("dsomop.release.quantity_scales"),
                    function(entry) {
    concept <- .dsomopReleaseScaleInteger(entry$concept_id, "concept_id")
    b <- .dsomopReleaseScaleBounds(entry)
    list(id = paste0("quantity-scale/v1/", substr(.dsomopDpSha256(.dsomopFrame(
           "quantity-scale/v1", concept, b$lower, b$upper, b$breaks)), 1L, 16L)),
         concept_id = concept, lower = b$lower, upper = b$upper,
         breaks = b$breaks, unit = "quantity",
         label = as.character(entry$label %||% NA_character_))
  })
  keys <- vapply(entries, function(s) paste0("concept=", s$concept_id),
                 character(1L))
  keep <- !duplicated(keys, fromLast = TRUE)
  stats::setNames(entries[keep], keys[keep])
}

#' The custodian's public drug cost scales
#'
#' \code{dsomop.release.cost_scales}: a list of entries with \code{measure}
#' ("cost_per_unit" or "out_of_pocket"), \code{concept_id} (drug concept),
#' \code{currency_concept_id}, \code{lower}, \code{upper} and optional
#' \code{breaks} and \code{label}. A "cost_per_unit" entry must also declare
#' \code{per_unit_denominator = TRUE}: the custodian states that the drug's
#' recorded quantity is a valid per-unit denominator. There are no built-in
#' entries. Public configuration only; never derived from data.
#'
#' @return Named list of validated scales, keyed
#'   "measure=<m>|concept=<id>|currency=<id>".
#' @keywords internal
.dsomopReleaseCostScales <- function() {
  entries <- lapply(.dsomopReleaseScaleOption("dsomop.release.cost_scales"),
                    function(entry) {
    measure <- as.character(entry$measure %||% NA_character_)
    if (length(measure) != 1L || !measure %in% .DSOMOP_RELEASE_COST_MEASURES) {
      stop("A public cost scale needs measure \"cost_per_unit\" or ",
           "\"out_of_pocket\".", call. = FALSE)
    }
    if (identical(measure, "cost_per_unit") &&
        !isTRUE(entry$per_unit_denominator)) {
      stop("A public per-unit cost scale must declare per_unit_denominator = ",
           "TRUE (the drug's recorded quantity is a valid per-unit ",
           "denominator).", call. = FALSE)
    }
    concept <- .dsomopReleaseScaleInteger(entry$concept_id, "concept_id")
    currency <- .dsomopReleaseScaleInteger(entry$currency_concept_id,
                                           "currency_concept_id")
    b <- .dsomopReleaseScaleBounds(entry)
    list(id = paste0("cost-scale/v1/", substr(.dsomopDpSha256(.dsomopFrame(
           "cost-scale/v1", measure, concept, currency, b$lower, b$upper,
           b$breaks)), 1L, 16L)),
         measure = measure, concept_id = concept,
         currency_concept_id = currency, lower = b$lower, upper = b$upper,
         breaks = b$breaks, unit = "currency",
         label = as.character(entry$label %||% NA_character_))
  })
  keys <- vapply(entries, function(s) paste0(
    "measure=", s$measure, "|concept=", s$concept_id, "|currency=",
    s$currency_concept_id), character(1L))
  keep <- !duplicated(keys, fromLast = TRUE)
  stats::setNames(entries[keep], keys[keep])
}

# The public scale of each stratum of a question (NULL: no public scale).
.omopReleaseRecordScaleOf <- function(plan) {
  if (plan$scale %in% c("days", "months")) {
    scale <- .dsomopReleaseRecordDurationScale(plan$scale)
    return(function(stratum) scale)
  }
  scales <- switch(plan$scale,
    quantity = .dsomopReleaseQuantityScales(),
    measurement = .dsomopReleaseMeasurementScales(),
    cost = .dsomopReleaseCostScales(),
    stop("Unknown public record scale.", call. = FALSE))
  prefix <- if (identical(plan$scale, "cost")) {
    paste0("measure=", plan$measure, "|")
  } else ""
  function(stratum) scales[[paste0(prefix, stratum)]]
}

# --- Record substrate -----------------------------------------------------------

# Fixed public order of a question's eligible records: event date, then the
# table's stable key (and the cost row's key).
.omopReleaseRecordOrder <- function(spec) {
  cols <- switch(spec$table,
    condition_era = c("x.condition_era_start_date", "x.condition_era_id"),
    drug_era = c("x.drug_era_start_date", "x.drug_era_id"),
    observation_period = c("x.observation_period_start_date",
                           "x.observation_period_id"),
    drug_exposure = c("x.drug_exposure_start_date", "x.drug_exposure_id"),
    measurement = c("x.measurement_date", "x.measurement_id"),
    NULL)
  if (is.null(cols)) {
    stop("This question has no declared public record order.", call. = FALSE)
  }
  if (isTRUE(spec$cost)) cols <- c(cols, "co.cost_id")
  cols
}

.omopReleaseRecordStrataKind <- function(summary) {
  if (identical(summary$group, "start_year")) return("year")
  if (identical(summary$split, "unit")) return("concept_unit")
  if (identical(summary$split, "currency")) return("concept_currency")
  "concept"
}

.omopReleaseRecordNumberText <- function(x) {
  ifelse(is.na(x), "unknown", format(x, scientific = FALSE, trim = TRUE))
}

#' Whole-person capped record substrate of a reviewed summary
#'
#' Every eligible record of the question (its template filters), ordered per
#' person by the public record order; only each person's first K records are
#' read, whatever their strata, so the cap applies to the person's whole
#' vector before any stratum is selected. Measurement units go through the
#' public conversions of the measurement scale registry; unknown units and
#' currencies are their own strata.
#'
#' @param handle CDM handle.
#' @param spec Reviewed question spec.
#' @param concept_id Concept parameter or NULL.
#' @param kind Strata kind.
#' @return Data frame: person_id, stratum, stratum_order, v, concept_id,
#'   unit_id, year (one row per kept record).
#' @keywords internal
.omopReleaseRecordSubstrate <- function(handle, spec, concept_id, kind) {
  s <- spec$summary
  K <- .DSOMOP_RELEASE_MULTI_CAP
  src <- .dsomopToolsFromSql(handle, spec, concept_id)
  raw_value <- spec$value(handle)
  value <- paste0("CAST(", raw_value, " AS FLOAT)")
  select <- "x.person_id AS person_id"
  if (!identical(kind, "year")) {
    select <- c(select, paste0("x.", s$concept_col, " AS concept_id"))
  }
  if (identical(kind, "concept_unit")) {
    conv <- .omopReleaseUnitConversionSql(
      list(value_col = "value_as_number", concept_col = s$concept_col),
      .dsomopReleaseMeasurementScales(), alias = "x")
    value <- conv$value
    select <- c(select, paste0(conv$unit, " AS unit_id"))
  } else if (identical(kind, "concept_currency")) {
    select <- c(select, "COALESCE(co.currency_concept_id, 0) AS unit_id")
  } else if (identical(kind, "year")) {
    select <- c(select, paste0(
      .omopYearExpr(handle, "x.observation_period_start_date"), " AS year_id"))
  }
  select <- c(select, paste0(value, " AS v"), paste0(
    "ROW_NUMBER() OVER (PARTITION BY x.person_id ORDER BY ",
    paste(.omopReleaseRecordOrder(spec), collapse = ", "), ") AS rn"))
  where <- c(src$where, paste0(raw_value, " IS NOT NULL"))
  raw <- .omopReleaseFetch(handle, paste0(
    "SELECT r.* FROM (SELECT ", paste(select, collapse = ", "), src$from,
    .dsomopToolsWhere(where), ") r WHERE r.rn <= ", K))
  empty <- data.frame(person_id = character(0), stratum = character(0),
                      stratum_order = numeric(0), v = numeric(0),
                      concept_id = numeric(0), unit_id = numeric(0),
                      year = numeric(0), stringsAsFactors = FALSE)
  if (!nrow(raw)) return(empty)
  person <- .dsomopReleasePersonIdText(raw$person_id)
  rn <- suppressWarnings(as.numeric(raw$rn))
  if (anyNA(person) || anyNA(rn) || any(rn > K) ||
      max(tabulate(match(person, unique(person)))) > K) {
    stop("A protected record substrate exceeds its whole-person cap.",
         call. = FALSE)
  }
  concept <- suppressWarnings(as.numeric(raw$concept_id %||%
                                           rep(NA_real_, nrow(raw))))
  unit <- suppressWarnings(as.numeric(raw$unit_id %||%
                                        rep(NA_real_, nrow(raw))))
  unit[is.na(unit)] <- 0
  year <- suppressWarnings(as.numeric(raw$year_id %||%
                                        rep(NA_real_, nrow(raw))))
  stratum <- switch(kind,
    concept = paste0("concept=", .omopReleaseRecordNumberText(concept)),
    concept_unit = .omopReleaseUnitKey(concept, unit),
    concept_currency = paste0(
      "concept=", .omopReleaseRecordNumberText(concept), "|currency=",
      ifelse(unit == 0, "unknown", .omopReleaseRecordNumberText(unit))),
    year = paste0("year=", .omopReleaseRecordNumberText(year)))
  if (identical(kind, "concept_unit")) {
    stratum[is.na(concept)] <- paste0("concept=unknown|unit=",
                                      sub("^.*\\|unit=", "",
                                          stratum[is.na(concept)]))
  }
  out <- data.frame(
    person_id = person, stratum = stratum,
    stratum_order = if (identical(kind, "year")) year else concept,
    v = suppressWarnings(as.numeric(raw$v)), concept_id = concept,
    unit_id = if (kind %in% c("concept_unit", "concept_currency")) unit else
      NA_real_,
    year = year, stringsAsFactors = FALSE)
  out[is.finite(out$v), , drop = FALSE]
}

# --- The record-moment builder ------------------------------------------------------

#' The owner of a capped record-moment release
#'
#' @param id Owner id (scientific identity: question, concept parameter).
#' @param estimand Estimand text.
#' @param scale_of Function from a stratum key to its public scale or NULL.
#' @param with_sd Whether the question displays an SD (draws Q).
#' @param record_order Public record order (for the label).
#' @keywords internal
.dsomopReleaseRecordOwner <- function(id, estimand, scale_of, with_sd = TRUE,
                                      record_order = character(0)) {
  list(id = id, population = id, family = "RM",
       strata = list(kind = "data"), cap = .DSOMOP_RELEASE_MULTI_CAP,
       scale_of = scale_of,
       allocation = if (isTRUE(with_sd)) .DSOMOP_RELEASE_RECORD_FRACTIONS$mean_sd
         else .DSOMOP_RELEASE_RECORD_FRACTIONS$mean,
       record_order = record_order, grid = NULL, bounds = NULL,
       estimand = estimand)
}

#' Build, noise and display capped record-weighted components
#'
#' Per released stratum: the eligible-person total N (shared person-count
#' owner, sensitivity K), the capped record count R (sensitivity K) and, on a
#' public scale and with enough distinct persons, the normalized lattice sums
#' S and Q of the records (sensitivity J K each). Displayed persons and
#' records are \code{max(0, .)} of N and R; the mean and SD are the
#' projection of (S, Q) onto their feasible set for the displayed R, never
#' using N. Strata need the exact support floor of distinct persons, and
#' moments \code{nfilter.dist} distinct persons (exact support gates outside
#' the fixed-vector guarantee, as for every protected owner).
#'
#' @param policy DP policy.
#' @param protection "fixed_synopsis" or "realised_sticky".
#' @param owner From \code{.dsomopReleaseRecordOwner}.
#' @param sub Record substrate: person_id, stratum, stratum_order, v; at most
#'   K rows per person (the caller's whole-person cap).
#' @param data_strata_keys Strata to release, or NULL for every supported one.
#' @param n_only Draw only the person totals (a first ranking pass; the noise
#'   is deterministic, so the full pass reproduces them).
#' @return List: owner, protection, strata (displayed results), export.
#' @keywords internal
.dsomopReleaseBuildRecordMoments <- function(policy, protection, owner, sub,
                                             data_strata_keys = NULL,
                                             n_only = FALSE) {
  keys <- .dsomopReleaseKeys(policy)
  support <- .dsomopReleaseSupport()
  J <- .DSOMOP_RELEASE_LATTICE
  K <- max(1L, as.integer(owner$cap))
  frac <- owner$allocation
  if (!is.data.frame(sub) ||
      !all(c("person_id", "stratum", "v") %in% names(sub))) {
    stop("A protected record substrate lacks its columns.", call. = FALSE)
  }
  sub$v <- suppressWarnings(as.numeric(sub$v))
  if (is.null(sub$stratum_order)) sub$stratum_order <- rep(NA_real_, nrow(sub))
  sub <- sub[!is.na(sub$person_id) & !is.na(sub$stratum) & is.finite(sub$v),
             , drop = FALSE]
  sub$person_id <- .dsomopReleasePersonIdText(sub$person_id)
  sub$stratum <- as.character(sub$stratum)
  if (nrow(sub) &&
      max(tabulate(match(sub$person_id, unique(sub$person_id)))) > K) {
    stop("A protected record substrate exceeds its whole-person cap.",
         call. = FALSE)
  }
  # Exact eligible-population support before any release.
  n_total <- length(unique(sub$person_id))
  if (n_total < support$floor) .dsomopReleaseDisclosive()
  tokens <- if (nrow(sub)) .dsomopReleasePersonTokens(keys, policy,
                                                      sub$person_id) else
    character(0)
  persons_in <- tapply(sub$person_id, sub$stratum,
                       function(p) length(unique(p)))
  supported <- names(persons_in)[as.numeric(persons_in) >= support$floor]
  strata_keys <- if (is.null(data_strata_keys)) supported else
    intersect(as.character(data_strata_keys), supported)
  order_value <- suppressWarnings(as.numeric(
    sub$stratum_order[match(strata_keys, sub$stratum)]))
  strata_keys <- strata_keys[order(order_value, strata_keys, method = "radix",
                                   na.last = TRUE)]
  noisy <- function(family, coordinate, value, allocation, sensitivity,
                    map_tokens, contributions, owner_id = owner$id) {
    fp <- if (identical(protection, "realised_sticky")) {
      .dsomopReleaseFingerprint(keys, owner_id, family, coordinate,
                                map_tokens, contributions)
    } else NULL
    .dsomopReleaseNoisyComponent(policy, keys, protection, list(
      owner = owner_id, population = owner$population, family = family,
      coordinate = coordinate, value = value, allocation = allocation,
      sensitivity = sensitivity, fingerprint = fp))
  }
  rows <- vector("list", length(strata_keys))
  for (i in seq_along(strata_keys)) {
    s <- strata_keys[[i]]
    in_s <- which(sub$stratum == s)
    first <- !duplicated(sub$person_id[in_s])
    tok <- tokens[in_s][first]
    idx <- match(sub$person_id[in_s], sub$person_id[in_s][first])
    n_exact <- sum(first)
    scale <- owner$scale_of(s)
    dist_ok <- n_exact >= support$dist
    n_noisy <- noisy("N", paste0("stratum=", s), n_exact, frac$N, K, tok,
                     rep(1, n_exact), owner_id = .DSOMOP_RELEASE_N_OWNER)
    r_noisy <- s_noisy <- q_noisy <- NA_real_
    if (!isTRUE(n_only)) {
      r_p <- tabulate(idx, n_exact)
      r_noisy <- noisy("R", paste0("stratum=", s), sum(r_p), frac$R, K, tok,
                       r_p)
      if (!is.null(scale) && dist_ok) {
        L <- scale$lower
        U <- scale$upper
        u <- (pmin(U, pmax(L, sub$v[in_s])) - L) / (U - L)
        s_p <- as.numeric(rowsum(round(J * u), idx, reorder = TRUE))
        q_p <- as.numeric(rowsum(round(J * u * u), idx, reorder = TRUE))
        coordinate <- paste0("stratum=", s, "|scale=", scale$id)
        s_noisy <- noisy("S", coordinate, sum(s_p), frac$S, J * K, tok, s_p)
        if (!is.null(frac$Q)) {
          q_noisy <- noisy("Q", coordinate, sum(q_p), frac$Q, J * K, tok, q_p)
        }
      }
    }
    rows[[i]] <- data.frame(
      stratum = s, n_noisy = n_noisy, r_noisy = r_noisy, s_noisy = s_noisy,
      q_noisy = q_noisy, dist_ok = dist_ok,
      scale_id = scale$id %||% NA_character_,
      lower = scale$lower %||% NA_real_, upper = scale$upper %||% NA_real_,
      stringsAsFactors = FALSE)
  }
  strata <- if (length(rows)) do.call(rbind, rows) else
    data.frame(stratum = character(0), n_noisy = numeric(0), r_noisy = numeric(0),
               s_noisy = numeric(0), q_noisy = numeric(0),
               dist_ok = logical(0), scale_id = character(0),
               lower = numeric(0), upper = numeric(0),
               stringsAsFactors = FALSE)
  strata$stratum_order <- suppressWarnings(as.numeric(
    sub$stratum_order[match(strata$stratum, sub$stratum)]))
  strata$n_display <- pmax(0, strata$n_noisy)
  strata$r_display <- pmax(0, strata$r_noisy)
  strata$mean <- NA_real_
  strata$sd <- NA_real_
  usable <- strata$dist_ok & is.finite(strata$s_noisy) &
    strata$n_display >= max(2, support$dist) &
    !is.na(strata$r_display) & strata$r_display >= 2
  for (i in which(usable)) {
    R <- strata$r_display[[i]]
    L <- strata$lower[[i]]
    U <- strata$upper[[i]]
    Su0 <- strata$s_noisy[[i]] / J
    if (is.finite(strata$q_noisy[[i]])) {
      # The u-moments map one-to-one onto z = 2u - 1 in [-1, 1]; project
      # (S_z, Q_z) onto |S| <= R, 0 <= Q <= R, S^2 <= R Q.
      Qu0 <- strata$q_noisy[[i]] / J
      p <- .dsomopReleaseProjectMoments(2 * Su0 - R, 4 * Qu0 - 4 * Su0 + R, R)
      strata$mean[[i]] <- (L + U) / 2 + (U - L) / 2 * p[["S"]] / R
      strata$sd[[i]] <- (U - L) / 2 *
        sqrt(max(0, (p[["Q"]] - p[["S"]]^2 / R) / (R - 1)))
    } else {
      strata$mean[[i]] <- L + (U - L) * min(R, max(0, Su0)) / R
    }
  }
  for (col in c("n_display", "r_display", "mean", "sd")) {
    strata[[col]] <- .dsomopReleaseRound(strata[[col]])
  }
  export <- lapply(seq_len(nrow(strata)), function(i) {
    has_scale <- !is.na(strata$scale_id[[i]])
    list(stratum = strata$stratum[[i]], n = strata$n_noisy[[i]],
         r = if (isTRUE(n_only)) NULL else strata$r_noisy[[i]],
         s = if (is.finite(strata$s_noisy[[i]])) strata$s_noisy[[i]] else NULL,
         q = if (is.finite(strata$q_noisy[[i]])) strata$q_noisy[[i]] else NULL,
         scale = if (has_scale) strata$scale_id[[i]] else NULL,
         bounds = if (has_scale) c(strata$lower[[i]], strata$upper[[i]]) else
           NULL)
  })
  list(
    owner = owner, protection = protection, strata = strata,
    root_n = NA_real_, bins = list(), bin_keys = character(0),
    export = list(
      protocol = .DSOMOP_RELEASE_PROTOCOL, owner = owner$id,
      population = owner$population, family = "RM", weighting = "record",
      strata_kind = "data", cap = K, cap_scope = "whole_person_records",
      record_order = owner$record_order, lattice = J,
      normalization = "unit_interval", allocation = frac,
      components = export)
  )
}

#' Exact capped record components of one owner (private; sensitivity checks)
#'
#' The same whole-person cap check, scales, clipping and lattice as the
#' builder, without noise. Never released.
#' @keywords internal
.dsomopReleaseRecordExactComponents <- function(owner, sub) {
  J <- .DSOMOP_RELEASE_LATTICE
  sub$v <- suppressWarnings(as.numeric(sub$v))
  sub <- sub[!is.na(sub$person_id) & !is.na(sub$stratum) & is.finite(sub$v),
             , drop = FALSE]
  sub$person_id <- .dsomopReleasePersonIdText(sub$person_id)
  keys <- sort(unique(as.character(sub$stratum)))
  out <- lapply(keys, function(k) {
    in_s <- sub$stratum == k
    scale <- owner$scale_of(k)
    m <- if (!is.null(scale)) {
      u <- (pmin(scale$upper, pmax(scale$lower, sub$v[in_s])) - scale$lower) /
        (scale$upper - scale$lower)
      list(s = sum(round(J * u)), q = sum(round(J * u * u)))
    } else list(s = 0, q = 0)
    list(n = length(unique(sub$person_id[in_s])), r = sum(in_s), s = m$s,
         q = m$q)
  })
  stats::setNames(out, keys)
}

#' Public label component of a record-moment owner
#'
#' @param built Output of \code{.dsomopReleaseBuildRecordMoments}.
#' @param epsilon Reference epsilon.
#' @keywords internal
.dsomopReleaseRecordLabel <- function(built, epsilon) {
  o <- built$owner
  K <- max(1L, as.integer(o$cap))
  J <- .DSOMOP_RELEASE_LATTICE
  alloc <- o$allocation
  sens <- lapply(names(alloc), function(f) if (f %in% c("S", "Q")) J * K else K)
  names(sens) <- names(alloc)
  sd <- lapply(names(alloc), function(f) {
    s <- .dsomopReleaseNoiseSd(epsilon * alloc[[f]], sens[[f]])
    if (f %in% c("S", "Q")) s / J else s
  })
  names(sd) <- names(alloc)
  st <- built$strata
  scaled <- !is.na(st$scale_id)
  scales <- unique(lapply(which(scaled), function(i) {
    list(id = st$scale_id[[i]], lower = st$lower[[i]], upper = st$upper[[i]])
  }))
  out <- list(owner = o$id, estimand = o$estimand, family = "RM",
              population = o$population, weighting = "record",
              record_order = o$record_order, contribution_cap = K,
              cap_scope = "whole_person_records",
              normalization = paste0("u = (clip(x, L, U) - L) / (U - L); ",
                                     "S = sum round(J u), Q = sum ",
                                     "round(J u^2), J = ", J),
              scales = scales, allocation_fraction = alloc,
              allocation_epsilon = lapply(alloc, function(a) a * epsilon),
              sensitivity = sens, noise_sd = sd)
  if (any(!scaled)) {
    out$moments_unavailable <- list(reason = "public_scale_unavailable",
                                    strata = st$stratum[!scaled])
  }
  out
}

# --- QueryLibrary summary contracts -------------------------------------------------

# Concept (or unit/currency) names of the displayed ids.
.omopReleaseRecordNames <- function(handle, ids) {
  ids <- unique(ids[!is.na(ids) & ids != 0])
  if (!length(ids)) return(stats::setNames(character(0), character(0)))
  found <- tryCatch(.vocabLookupConcepts(handle, ids), error = function(e) NULL)
  if (!is.data.frame(found) || !nrow(found)) {
    return(stats::setNames(character(0), character(0)))
  }
  stats::setNames(as.character(found$concept_name),
                  format(as.numeric(found$concept_id), scientific = FALSE,
                         trim = TRUE))
}

# The displayed rows in the question's closed schema; attribute
# "dsomop_record_strata" names the stratum of each row (removed by the run).
.omopReleaseRecordRows <- function(handle, built, sub, spec, kind) {
  s <- spec$summary
  cols <- s$cols
  st <- built$strata
  if (!nrow(st)) {
    df <- .dsomopToolsEmptyFrame(cols)
    attr(df, "dsomop_record_strata") <- character(0)
    return(df)
  }
  at <- match(st$stratum, sub$stratum)
  concept <- sub$concept_id[at]
  unit <- sub$unit_id[at]
  lookup <- function(ids) {
    names_by_id <- .omopReleaseRecordNames(handle, ids)
    unname(names_by_id[format(ids, scientific = FALSE, trim = TRUE)])
  }
  name_of <- function(ids) {
    out <- lookup(ids)
    ifelse(!is.na(ids) & ids == 0, "unknown", out)
  }
  out <- list()
  for (col in cols) {
    out[[col]] <- if (identical(col, s$concept_col)) concept
      else if (identical(col, "concept_name")) lookup(concept)
      else if (col %in% c("unit_concept_id", "currency_concept_id")) unit
      else if (col %in% c("unit_name", "currency_name")) name_of(unit)
      else if (identical(col, "start_year")) sub$year[at]
      else if (identical(col, "n_persons")) st$n_display
      else if (identical(col, s$n_col %||% "")) st$r_display
      else if (identical(col, s$mean_col)) st$mean
      else if (identical(col, s$sd_col %||% "")) st$sd
      else rep(NA, nrow(st))
  }
  df <- as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
  names(df) <- cols
  ord <- if (identical(s$order, "n_persons")) {
    order(-st$n_display, st$stratum_order, st$stratum, method = "radix",
          na.last = TRUE)
  } else {
    order(st$stratum_order, unit %||% 0, st$stratum, method = "radix",
          na.last = TRUE)
  }
  df <- df[ord, , drop = FALSE]
  rownames(df) <- NULL
  attr(df, "dsomop_record_strata") <- st$stratum[ord]
  df
}

#' Public pooling fields of a record-moment export
#'
#' Adds what dsOMOPClient needs to pool the record components of compatible
#' servers and to show the pooled strata in the question's own schema: the
#' columns holding the persons (N), the capped records (R) and the moments,
#' the public display order and, per released stratum, its displayed key and
#' label values (its displayed row without the statistics) and its public
#' ordering values. Everything added is already displayed; nothing private
#' (support, tokens, fingerprints) is exported.
#'
#' @param built Output of \code{.dsomopReleaseBuildRecordMoments}.
#' @param df Displayed frame with attribute \code{dsomop_record_strata}.
#' @param sub The record substrate (public unit ids of the strata).
#' @param summary The question's summary spec.
#' @return The export with \code{display} and per-component \code{display},
#'   \code{order_value} and \code{order_unit}.
#' @keywords internal
.omopReleaseRecordPoolingExport <- function(built, df, sub, summary) {
  export <- built$export
  absent <- function(x) if (is.null(x)) NA_character_ else x
  stats_cols <- c("n_persons", summary$n_col, summary$mean_col,
                  summary$sd_col)
  shown <- attr(df, "dsomop_record_strata", exact = TRUE) %||% character(0)
  display_cols <- setdiff(names(df), stats_cols)
  export$display <- list(
    columns = names(df), persons = "n_persons",
    records = absent(summary$n_col), mean = absent(summary$mean_col),
    sd = absent(summary$sd_col),
    order = if (identical(summary$order, "n_persons")) "n_persons" else
      "stratum")
  st <- built$strata
  unit <- suppressWarnings(as.numeric(sub$unit_id[match(st$stratum,
                                                        sub$stratum)]))
  unit[is.na(unit)] <- 0
  export$components <- lapply(seq_along(export$components), function(i) {
    comp <- export$components[[i]]
    row <- match(comp$stratum, shown)
    comp$display <- if (is.na(row)) NULL else
      lapply(df[row, display_cols, drop = FALSE], function(x) x[[1L]])
    comp$order_value <- as.numeric(st$stratum_order[[i]])
    comp$order_unit <- unit[[i]]
    comp
  })
  export
}

#' Protected run of one record-weighted QueryLibrary summary
#'
#' @keywords internal
.omopReleaseRunRecordSummary <- function(handle, ctx, params, route, snap) {
  entry <- ctx$entry
  qid <- entry$meta$query_id
  spec <- entry$meta$tools_spec
  plan <- .DSOMOP_RELEASE_RECORD_SUMMARIES[[qid]]
  if (is.null(plan) || is.null(spec)) {
    stop("This question has no record-moment release.", call. = FALSE)
  }
  s <- spec$summary
  concept_id <- .dsomopToolsConceptParam(spec, params)
  kind <- .omopReleaseRecordStrataKind(s)
  sub <- .omopReleaseRecordSubstrate(handle, spec, concept_id, kind)
  order_cols <- sub("^[a-z]+\\.", "", .omopReleaseRecordOrder(spec))
  K <- .DSOMOP_RELEASE_MULTI_CAP
  scale_of <- .omopReleaseRecordScaleOf(plan)
  owner <- .dsomopReleaseRecordOwner(
    id = paste0("querylibrary/", qid, "/record-moments/concept=",
                if (is.null(concept_id)) "none" else concept_id, "/v1"),
    estimand = paste0(
      "capped record-weighted ", if (is.null(s$sd_col)) "mean" else
        "mean and SD", " of the ", spec$value_label, ": each person's first ",
      K, " eligible records (by ", paste(order_cols, collapse = ", then "),
      ") across all strata, each value clipped to its public scale; ",
      "n_persons are the persons with a kept record in the stratum",
      if (!is.null(s$n_col)) paste0(", ", s$n_col, " the kept records") else
        "",
      if (plan$scale %in% c("quantity", "measurement", "cost")) paste0(
        "; strata without a registered public scale have unavailable ",
        "moments") else ""),
    scale_of = scale_of, with_sd = !is.null(s$sd_col),
    record_order = order_cols)
  keys <- NULL
  if (isTRUE(s$top_n)) {
    p <- Filter(function(x) identical(x$name, "top_n"), entry$params)
    default <- if (length(p) == 1L) p[[1L]]$default else NULL
    top_n <- suppressWarnings(as.integer(params$top_n %||% default))
    if (length(top_n) != 1L || is.na(top_n) || top_n < 1L) {
      stop("top_n must be a positive integer.", call. = FALSE)
    }
    first <- .dsomopReleaseBuildRecordMoments(route$policy, route$protection,
                                              owner, sub, n_only = TRUE)
    st <- first$strata
    ord <- order(-st$n_display, st$stratum_order, st$stratum,
                 method = "radix", na.last = TRUE)
    keys <- utils::head(st$stratum[ord], top_n)
  }
  built <- .dsomopReleaseBuildRecordMoments(route$policy, route$protection,
                                            owner, sub,
                                            data_strata_keys = keys)
  df <- .omopReleaseRecordRows(handle, built, sub, spec, kind)
  built$export <- .omopReleaseRecordPoolingExport(built, df, sub, s)
  attr(df, "dsomop_record_strata") <- NULL
  list(df = df, built = list(built),
       extra = list(weighting = "record", record_cap = K,
                    record_order = order_cols))
}

#' Release contracts of the record-weighted QueryLibrary summaries
#'
#' C7 contracts over the unscoped CDM population (a public basis), released
#' in their original summary schemas. dsOMOPClient 2.10.3 pools compatible
#' servers by summing the signed N, R, S and Q of each stratum and deriving
#' the record-weighted mean and SD once (\code{.omopReleaseRecordPoolingExport}
#' carries the display fields); older clients see a not-poolable contract.
#'
#' @return Named list of contracts.
#' @keywords internal
.omopReleaseRecordSummaryContracts <- function() {
  ids <- paste0("dsomop:", names(.DSOMOP_RELEASE_RECORD_SUMMARIES))
  stats::setNames(lapply(ids, function(id) {
    list(class = "C7", run = .omopReleaseRunRecordSummary, fixed_basis = TRUE,
         masked_scoped = character(0), masked_always = character(0),
         summary_shape = TRUE)
  }), ids)
}
