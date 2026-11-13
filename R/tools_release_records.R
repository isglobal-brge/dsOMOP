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
# registered) the moments are unavailable and the stratum releases instead a
# weighted value histogram H of the same kept records on the public signed
# 1-2-5 grid (explicit underflow and overflow tails; sensitivity K), whose
# cells are reconciled to the stratum's protected record total R (never to
# the person total N) and give interval quantiles (review section 6, R07).
#
# Candidate strata have public meanings: every concept identifier
# (with an explicit unknown-concept cell), concept x unit or concept x
# currency (unknown units or currencies their own cell), or the calendar
# start years 1900 to the reference year with explicit before-1900 and
# after-reference-year tails and an unknown-year cell. The exact
# distinct-person support floor chooses the displayed cells (an exact gate
# outside any joint privacy guarantee).

# Static allocations of the reference epsilon: N belongs to the shared
# eligible-person owner, R, S, Q and H to the record-moment owner. A stratum
# draws either the moments S and Q (registered scale) or the value histogram H
# (no registered scale), never both, and every person keeps at most K records
# across all strata, so S, Q and H share one whole-person record budget: their
# joint privacy loss is max(S + Q, H), and H takes the moments' share.
.DSOMOP_RELEASE_RECORD_FRACTIONS <- list(
  mean_sd = list(N = 0.2, R = 0.3, S = 0.3, Q = 0.2, H = 0.5),
  mean = list(N = 0.2, R = 0.4, S = 0.4, H = 0.4)
)

# The versioned value histogram of strata without a registered scale and the
# versioned display projection of record releases (dsOMOPClient applies the
# same projection to pooled components).
.DSOMOP_RELEASE_RECORD_HISTOGRAM <- "record-value-histogram-v1"
.DSOMOP_RELEASE_RECORD_DISPLAY <- "record-display-v1"

# The public scale-free grid of the record value histograms.
.dsomopReleaseRecordValueGrid <- function() .dsomopReleaseGrid("signed_125")

#' Displayed persons and records of one record stratum
#'
#' The versioned display projection \code{record-display-v1}: persons are
#' \code{max(0, N)}; records are held inside their feasible range for the
#' displayed persons, \code{[N, K N]} (every displayed person has at least one
#' and at most K kept records), before the moments and the value histogram are
#' projected onto them. The signed components are never changed.
#'
#' @param n Signed noisy person total.
#' @param r Signed noisy record total (NA when not drawn).
#' @param K Whole-person record cap.
#' @return Named numeric: \code{n}, \code{r}.
#' @keywords internal
.dsomopReleaseRecordDisplay <- function(n, r, K) {
  n_display <- pmax(0, n)
  r_display <- ifelse(is.na(r), NA_real_,
                      pmin(K * n_display, pmax(n_display, r)))
  list(n = n_display, r = r_display)
}

#' Declared public stratum domain of a record summary
#'
#' @param kind Strata kind ("concept", "concept_unit", "concept_currency",
#'   "year").
#' @param year Public reference year (the end of the start-year domain).
#' @return List: kind, cells, unknown and tails (public metadata).
#' @keywords internal
.omopReleaseRecordDomain <- function(kind, year = .dsomopToolsReferenceYear()) {
  concept <- paste0("every integer concept identifier of the question's ",
                    "domain (the public OMOP vocabulary)")
  switch(kind,
    concept = list(kind = "concept", cells = concept,
                   unknown = "concept=unknown (records without a concept)",
                   tails = character(0)),
    concept_unit = list(
      kind = "concept x unit",
      cells = paste0(concept, " x every unit concept identifier"),
      unknown = "unit=unknown (NULL or 0 units); concept=unknown",
      tails = character(0)),
    concept_currency = list(
      kind = "concept x currency",
      cells = paste0(concept, " x every currency concept identifier"),
      unknown = "currency=unknown (NULL or 0 currencies); concept=unknown",
      tails = character(0)),
    year = list(
      kind = "start year",
      cells = paste0("calendar years 1900 to ", year),
      unknown = "year=unknown (no start year)",
      tails = c("year=before-1900 (displayed as start_year 1899)",
                paste0("year=after-", year, " (displayed as start_year ",
                       year + 1L, ")"))),
    stop("Unknown record strata kind.", call. = FALSE))
}

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
  # The public start-year domain: 1900 to the reference year, explicit tails
  # before and after it (displayed as 1899 and the year after) and an unknown
  # cell, so no displayed year is a private out-of-range value.
  ref <- .dsomopToolsReferenceYear()
  year_key <- ifelse(is.na(year), "unknown",
                     ifelse(year < 1900, "before-1900",
                            ifelse(year > ref, paste0("after-", ref),
                                   .omopReleaseRecordNumberText(year))))
  year <- ifelse(is.na(year), NA_real_, pmin(pmax(year, 1899), ref + 1))
  stratum <- switch(kind,
    concept = paste0("concept=", .omopReleaseRecordNumberText(concept)),
    concept_unit = .omopReleaseUnitKey(concept, unit),
    concept_currency = paste0(
      "concept=", .omopReleaseRecordNumberText(concept), "|currency=",
      ifelse(unit == 0, "unknown", .omopReleaseRecordNumberText(unit))),
    year = paste0("year=", year_key))
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
#' @param domain Declared public stratum domain
#'   (\code{.omopReleaseRecordDomain}), or NULL.
#' @keywords internal
.dsomopReleaseRecordOwner <- function(id, estimand, scale_of, with_sd = TRUE,
                                      record_order = character(0),
                                      domain = NULL) {
  list(id = id, population = id, family = "RM",
       strata = list(kind = "data"), cap = .DSOMOP_RELEASE_MULTI_CAP,
       scale_of = scale_of,
       allocation = if (isTRUE(with_sd)) .DSOMOP_RELEASE_RECORD_FRACTIONS$mean_sd
         else .DSOMOP_RELEASE_RECORD_FRACTIONS$mean,
       record_order = record_order, grid = NULL, bounds = NULL,
       domain = domain, value_grid = .dsomopReleaseRecordValueGrid(),
       estimand = estimand)
}

#' Build, noise and display capped record-weighted components
#'
#' Per released stratum: the eligible-person total N (shared person-count
#' owner, sensitivity K), the capped record count R (sensitivity K) and, with
#' enough distinct persons, either the normalized lattice sums S and Q of the
#' records on the stratum's registered public scale (sensitivity J K each) or,
#' without a registered scale, the weighted value histogram H of the same
#' records on the public signed 1-2-5 grid (sensitivity K; tails explicit).
#' Displayed values follow the versioned projection
#' \code{.DSOMOP_RELEASE_RECORD_DISPLAY}: persons \code{max(0, N)}, records
#' held in \code{[N, K N]} (\code{.dsomopReleaseRecordDisplay}); the mean and
#' SD are the projection of (S, Q) onto their feasible set for the displayed
#' records, never using N; H cells are reconciled (signed) to the stratum's
#' signed R and projected (displayed) onto its displayed R, and give interval
#' quantiles. Strata need the exact support floor of distinct persons, and
#' moments and histograms \code{nfilter.dist} distinct persons (exact support
#' gates on a realised-sticky route, with no joint DP guarantee).
#'
#' @param policy DP policy.
#' @param protection "fixed_synopsis" or "realised_sticky".
#' @param owner From \code{.dsomopReleaseRecordOwner}.
#' @param sub Record substrate: person_id, stratum, stratum_order, v; at most
#'   K rows per person (the caller's whole-person cap).
#' @param data_strata_keys Strata to release, or NULL for every supported one.
#' @param n_only Draw only the person totals (a first ranking pass; the noise
#'   is deterministic, so the full pass reproduces them).
#' @return List: owner, protection, strata (displayed results), hist
#'   (displayed value histograms and quantile intervals by stratum), export.
#' @keywords internal
.dsomopReleaseBuildRecordMoments <- function(policy, protection, owner, sub,
                                             data_strata_keys = NULL,
                                             n_only = FALSE) {
  keys <- .dsomopReleaseKeys(policy)
  support <- .dsomopReleaseSupport()
  J <- .DSOMOP_RELEASE_LATTICE
  K <- max(1L, as.integer(owner$cap))
  frac <- owner$allocation
  value_grid <- owner$value_grid %||% .dsomopReleaseRecordValueGrid()
  value_keys <- .dsomopReleaseBinKeys(value_grid)
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
    fp <- .dsomopReleaseFingerprint(keys, owner_id, family, coordinate,
                                    map_tokens, contributions)
    .dsomopReleaseNoisyComponent(policy, keys, protection, list(
      owner = owner_id, population = owner$population, family = family,
      coordinate = coordinate, value = value, allocation = allocation,
      sensitivity = sensitivity, fingerprint = fp))
  }
  rows <- vector("list", length(strata_keys))
  h_noisy <- vector("list", length(strata_keys))
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
      } else if (is.null(scale) && dist_ok && !is.null(frac$H)) {
        # No registered scale: the weighted value histogram of the same kept
        # records (each record one unit in its public value cell).
        cell <- .dsomopReleaseBinIndex(sub$v[in_s], value_grid)
        h_noisy[[i]] <- vapply(seq_along(value_keys), function(b) {
          w <- tabulate(idx[cell == b], n_exact)
          hit <- which(w > 0)
          noisy("H", paste0("stratum=", s, "|cell=", value_keys[[b]]),
                sum(w), frac$H, K, tok[hit], w[hit])
        }, numeric(1L))
      }
    }
    rows[[i]] <- data.frame(
      stratum = s, n_noisy = n_noisy, r_noisy = r_noisy, s_noisy = s_noisy,
      q_noisy = q_noisy, dist_ok = dist_ok, h_drawn = !is.null(h_noisy[[i]]),
      scale_id = scale$id %||% NA_character_,
      lower = scale$lower %||% NA_real_, upper = scale$upper %||% NA_real_,
      stringsAsFactors = FALSE)
  }
  strata <- if (length(rows)) do.call(rbind, rows) else
    data.frame(stratum = character(0), n_noisy = numeric(0), r_noisy = numeric(0),
               s_noisy = numeric(0), q_noisy = numeric(0),
               dist_ok = logical(0), h_drawn = logical(0),
               scale_id = character(0), lower = numeric(0), upper = numeric(0),
               stringsAsFactors = FALSE)
  strata$stratum_order <- suppressWarnings(as.numeric(
    sub$stratum_order[match(strata$stratum, sub$stratum)]))
  shown <- .dsomopReleaseRecordDisplay(strata$n_noisy, strata$r_noisy, K)
  strata$n_display <- shown$n
  strata$r_display <- shown$r
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
  # Value histograms: signed cells reconciled to the signed record total R,
  # displayed cells projected onto the displayed records; interval quantiles
  # (N02) from the displayed cells.
  bin_frame <- .dsomopReleaseBinFrame(value_grid)
  h_signed <- vector("list", nrow(strata))
  hist <- list()
  for (i in which(strata$h_drawn)) {
    h <- h_noisy[[i]]
    h_signed[[i]] <- h + (strata$r_noisy[[i]] - sum(h)) / length(h)
    display <- .dsomopReleaseSimplex(h, strata$r_display[[i]])
    quantiles <- if (strata$n_display[[i]] >= max(2, support$dist) &&
                     strata$r_display[[i]] > 0) {
      .dsomopReleaseQuantileBounds(display, value_grid)
    } else NULL
    hist[[strata$stratum[[i]]]] <- list(
      bins = data.frame(bin = bin_frame$bin, bin_label = bin_frame$bin_label,
                        bin_lower = bin_frame$bin_lower,
                        bin_upper = bin_frame$bin_upper,
                        records = .dsomopReleaseRound(display),
                        stringsAsFactors = FALSE),
      quantiles = quantiles)
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
         h = h_signed[[i]],
         scale = if (has_scale) strata$scale_id[[i]] else NULL,
         bounds = if (has_scale) c(strata$lower[[i]], strata$upper[[i]]) else
           NULL)
  })
  any_h <- any(strata$h_drawn)
  list(
    owner = owner, protection = protection, strata = strata, hist = hist,
    root_n = NA_real_, bins = list(), bin_keys = character(0),
    export = c(list(
      protocol = .DSOMOP_RELEASE_PROTOCOL, owner = owner$id,
      population = owner$population, family = "RM", weighting = "record",
      strata_kind = "data", cap = K, cap_scope = "whole_person_records",
      record_order = owner$record_order, lattice = J,
      normalization = "unit_interval", allocation = frac,
      display_projection = .DSOMOP_RELEASE_RECORD_DISPLAY,
      distribution_floor = max(2, support$dist)),
      if (any_h) list(
        value_histogram = .DSOMOP_RELEASE_RECORD_HISTOGRAM,
        value_grid = value_grid$id, value_breaks = value_grid$breaks,
        value_cell_keys = value_keys,
        value_reconciliation = "cells_sum_to_signed_record_total_R"),
      list(components = export))
  )
}

#' Exact capped record components of one owner (private; sensitivity checks)
#'
#' The same whole-person cap check, scales, clipping, lattice and value cells
#' as the builder, without noise. Never released.
#' @keywords internal
.dsomopReleaseRecordExactComponents <- function(owner, sub) {
  J <- .DSOMOP_RELEASE_LATTICE
  value_grid <- owner$value_grid %||% .dsomopReleaseRecordValueGrid()
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
      list(s = sum(round(J * u)), q = sum(round(J * u * u)), h = NULL)
    } else {
      cell <- .dsomopReleaseBinIndex(sub$v[in_s], value_grid)
      list(s = 0, q = 0,
           h = tabulate(cell, length(value_grid$breaks)))
    }
    list(n = length(unique(sub$person_id[in_s])), r = sum(in_s), s = m$s,
         q = m$q, h = m$h)
  })
  stats::setNames(out, keys)
}

#' Public label component of a record-moment owner
#'
#' Reports the allocations, sensitivities and noise scales of the component
#' families actually drawn (no S or Q when no stratum has a registered scale,
#' no H when every stratum has one), the union epsilon of the owner (N + R +
#' max(S + Q, H): moments and the value histogram share the whole-person
#' record cap), the declared public stratum domain, the display projection
#' and, for strata without a registered scale, the value histogram grid.
#'
#' @param built Output of \code{.dsomopReleaseBuildRecordMoments}.
#' @param epsilon Reference epsilon.
#' @keywords internal
.dsomopReleaseRecordLabel <- function(built, epsilon) {
  o <- built$owner
  K <- max(1L, as.integer(o$cap))
  J <- .DSOMOP_RELEASE_LATTICE
  st <- built$strata
  drawn <- c("N",
             if (any(is.finite(st$r_noisy))) "R",
             if (any(is.finite(st$s_noisy))) "S",
             if (any(is.finite(st$q_noisy))) "Q",
             if (any(st$h_drawn %||% FALSE)) "H")
  alloc <- o$allocation[intersect(names(o$allocation), drawn)]
  sens <- lapply(names(alloc), function(f) if (f %in% c("S", "Q")) J * K else K)
  names(sens) <- names(alloc)
  sd <- lapply(names(alloc), function(f) {
    s <- .dsomopReleaseNoiseSd(epsilon * alloc[[f]], sens[[f]])
    if (f %in% c("S", "Q")) s / J else s
  })
  names(sd) <- names(alloc)
  scaled <- !is.na(st$scale_id)
  scales <- unique(lapply(which(scaled), function(i) {
    list(id = st$scale_id[[i]], lower = st$lower[[i]], upper = st$upper[[i]])
  }))
  moments <- sum(unlist(alloc[intersect(c("S", "Q"), names(alloc))]))
  values <- max(moments, alloc$H %||% 0)
  union <- c(alloc[intersect(c("N", "R"), names(alloc))],
             if (values > 0) list(record_values = values))
  out <- list(owner = o$id, estimand = o$estimand, family = "RM",
              population = o$population, weighting = "record",
              record_order = o$record_order, contribution_cap = K,
              cap_scope = "whole_person_records",
              normalization = paste0("u = (clip(x, L, U) - L) / (U - L); ",
                                     "S = sum round(J u), Q = sum ",
                                     "round(J u^2), J = ", J),
              scales = scales, allocation_fraction = alloc,
              allocation_epsilon = lapply(alloc, function(a) a * epsilon),
              sensitivity = sens, noise_sd = sd,
              union_epsilon = lapply(union, function(a) a * epsilon),
              joint_cap = paste0(
                "S and Q (strata with a registered scale) and the value ",
                "histogram H (strata without one) share the whole-person cap ",
                "of ", K, " records: a record enters either the moments or ",
                "the histogram, so their joint privacy loss is max(S + Q, H)"),
              display_projection = paste0(
                .DSOMOP_RELEASE_RECORD_DISPLAY, ": persons max(0, N); records ",
                "held in [N, ", K, " N] for the displayed persons; moments ",
                "projected for the displayed records; histogram cells ",
                "projected onto the displayed records"),
              public_domain = o$domain)
  if (any(!scaled)) {
    out$moments_unavailable <- list(reason = "public_scale_unavailable",
                                    strata = st$stratum[!scaled])
  }
  if (any(st$h_drawn %||% FALSE)) {
    grid <- o$value_grid %||% .dsomopReleaseRecordValueGrid()
    out$value_histogram <- list(
      version = .DSOMOP_RELEASE_RECORD_HISTOGRAM, grid = grid$id,
      strata = st$stratum[st$h_drawn],
      cells = paste0("[b_i, b_i+1) on the public signed 1-2-5 grid; ",
                     "(-Inf, ", grid$breaks[[2L]], ") and [",
                     grid$breaks[[length(grid$breaks)]],
                     ", Inf) are the explicit underflow and overflow tails"),
      reconciliation = paste0("signed cells sum to the signed capped record ",
                              "total R; displayed cells to the displayed ",
                              "records (never to the person total N)"))
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
        "moments and release a weighted value histogram of the same kept ",
        "records on the public signed 1-2-5 grid (interval quantiles in the ",
        "label)") else ""),
    scale_of = scale_of, with_sd = !is.null(s$sd_col),
    record_order = order_cols, domain = .omopReleaseRecordDomain(kind))
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
  views <- .omopReleaseRecordHistogramViews(built, df, s)
  attr(df, "dsomop_record_strata") <- NULL
  list(df = df, built = list(built),
       extra = c(list(weighting = "record", record_cap = K,
                      record_order = order_cols,
                      public_domain = .omopReleaseRecordDomain(kind),
                      displayed_strata = paste0(
                        "data-enumerated strata whose exact distinct-",
                        "person support reaches the floor (an exact gate ",
                        "on a data-enumerated realised-sticky route)")),
                 views))
}

#' Companion value-histogram views of a record summary
#'
#' The summary schemas have no histogram columns, so the displayed value
#' histograms of the strata without a registered scale and their interval
#' quantiles (N02) travel in the public label: \code{value_histograms} (one
#' row per displayed stratum and public value cell: the stratum's displayed
#' key and label values, the cell interval and the displayed records) and
#' \code{value_quantiles} (p10 to p90 with each quantile's cell interval and
#' open/censored flags). Everything is post-processing of the displayed
#' protected cells.
#'
#' @param built Output of \code{.dsomopReleaseBuildRecordMoments}.
#' @param df Displayed frame with attribute \code{dsomop_record_strata}.
#' @param summary The question's summary spec.
#' @return List with \code{value_histograms} and \code{value_quantiles}, or
#'   an empty list.
#' @keywords internal
.omopReleaseRecordHistogramViews <- function(built, df, summary) {
  hist <- built$hist %||% list()
  shown <- attr(df, "dsomop_record_strata", exact = TRUE) %||% character(0)
  keep <- intersect(shown, names(hist))
  if (!length(keep)) return(list())
  stats_cols <- c("n_persons", summary$n_col, summary$mean_col,
                  summary$sd_col)
  key_cols <- setdiff(names(df), stats_cols)
  key_of <- function(stratum) {
    row <- df[match(stratum, shown), key_cols, drop = FALSE]
    rownames(row) <- NULL
    row
  }
  frames <- lapply(keep, function(k) {
    b <- hist[[k]]$bins
    cbind(key_of(k)[rep(1L, nrow(b)), , drop = FALSE], b, row.names = NULL)
  })
  quantiles <- lapply(keep, function(k) {
    q <- hist[[k]]$quantiles
    if (is.null(q)) return(NULL)
    cbind(key_of(k)[rep(1L, nrow(q)), , drop = FALSE], q, row.names = NULL)
  })
  quantiles <- Filter(Negate(is.null), quantiles)
  list(value_histograms = do.call(rbind, frames),
       value_quantiles = if (length(quantiles)) do.call(rbind, quantiles))
}

#' Release contracts of the record-weighted QueryLibrary summaries
#'
#' C7 contracts over data-enumerated supported CDM strata, released
#' in their original summary schemas. dsOMOPClient 2.10.3 pools compatible
#' servers by summing the signed N, R, S, Q and H of each stratum and deriving
#' the record-weighted mean and SD once (\code{.omopReleaseRecordPoolingExport}
#' carries the display fields); older clients see a not-poolable contract.
#' Public stratum descriptions do not materialize the complete domain: these
#' contracts use realised-sticky protection even for an unscoped request, and
#' DP-exclusive mode refuses them without a fixed-public-basis claim.
#'
#' @return Named list of contracts.
#' @keywords internal
.omopReleaseRecordSummaryContracts <- function() {
  ids <- paste0("dsomop:", names(.DSOMOP_RELEASE_RECORD_SUMMARIES))
  stats::setNames(lapply(ids, function(id) {
    list(class = "C7", run = .omopReleaseRunRecordSummary, fixed_basis = FALSE,
         masked_scoped = character(0), masked_always = character(0),
         summary_shape = TRUE)
  }), ids)
}
