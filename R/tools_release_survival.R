# Module: Protected survival, SCCS and standard-route person-time releases
# (dsOMOP 2.10.3, Astra review section 6: "secure without losing features")
#
# * Grouped-time Cox (C8): a complete public histogram per arm of exit-time
#   bins x status (event / censored / administratively censored) with one
#   atom per person, reconciled to the arm person total; risk sets and events
#   come from the projected protected counts only and the hazard ratio is the
#   maximum of the Breslow grouped likelihood. It is a grouped-time estimator
#   on public bins, not the raw Efron/continuous-time Cox fit.
# * Conditional SCCS IRR: per case and observation period the exposed
#   fraction of observed time q on a public logit-spaced grid and at most
#   K = 10 events per person; one weighted histogram H(q, exposed status)
#   with whole-person sensitivity K. The conditional likelihood depends on
#   the data only through M_q and E; the estimate is the root of its score
#   on the signed protected cells (unbiased in the noise).
# * Standard-route person-time: in standard mode with the DP service, the
#   person-time block of the C6 entries (person-days, rates, MDRR, effects)
#   comes from the same protected components as in auto mode, in a labelled
#   mixed frame; the established standard counts stay standard.
#
# All of it is post-processing of protected components: no history, budget,
# quota or rate limit is involved, and uncertainty simulations use a local
# hash-based generator seeded by public displayed values (never R's RNG).

.DSOMOP_RELEASE_SCCS_CAP <- 10L
.DSOMOP_RELEASE_SURVIVAL_FRACTIONS <- list(C8 = 0.8, SCCS = list(N = 0.2,
                                                                 W = 0.8))
.DSOMOP_RELEASE_SIM_REPLICATES <- 200L

# --- Deterministic noise-law simulation (post-processing only) -----------------

#' Uniforms of a public, hash-based stream
#'
#' Counter-mode SHA-256 of a public seed text: deterministic, independent of
#' R's random number generator (whose state is never read or changed).
#'
#' @param seed Public seed text (derived from displayed protected values).
#' @param n Number of uniforms.
#' @return Numeric vector in (0, 1).
#' @keywords internal
.dsomopReleaseSimUniform <- function(seed, n) {
  if (n <= 0L) return(numeric(0))
  h <- as.character(openssl::sha256(paste0(seed, ":", seq_len(n))))
  (strtoi(substr(h, 1L, 7L), 16L) + 0.5) / 2^28
}

#' Discrete-Laplace draws of the public noise law (simulation only)
#'
#' @param seed Public seed text.
#' @param n Number of draws.
#' @param epsilon,sensitivity The components' public calibration (recycled
#'   along the draws).
#' @return Integer-valued numeric vector.
#' @keywords internal
.dsomopReleaseSimNoise <- function(seed, n, epsilon, sensitivity) {
  if (n <= 0L) return(numeric(0))
  la <- rep_len(-epsilon / sensitivity, n)
  u <- .dsomopReleaseSimUniform(seed, 2L * n)
  floor(log(u[seq_len(n)]) / la) - floor(log(u[n + seq_len(n)]) / la)
}

# Public seed text of a simulation: the displayed values it perturbs.
.dsomopReleaseSimSeed <- function(label, values) {
  .dsomopFrame("dsomop-release-simulation/v1", label,
               signif(as.numeric(values), 12))
}

# --- C8: grouped-time Cox from a public exit-time x status basis ---------------

#' Public exit-time bins of the protected survival basis
#'
#' The clinical day grid L within [0, 3650]; when both window anchors are the
#' same, exits cannot exceed the public window length, so the bins beyond it
#' are structurally empty and omitted (a function of public parameters only).
#'
#' @param max_exit Public maximum exit in days, or NULL.
#' @return Grid list (id, breaks, unit).
#' @keywords internal
.dsomopReleaseExitGrid <- function(max_exit = NULL) {
  L <- .dsomopReleaseDayGrid()
  if (!is.null(max_exit) && is.finite(max_exit)) {
    L <- L[L < max(1, max_exit)]
  }
  list(id = paste0("exit-days-clinical-v1/max=",
                   if (is.null(max_exit)) "3650" else max_exit),
       breaks = L, lower = 0, upper = 3650, unit = "days")
}

.DSOMOP_RELEASE_EXIT_STATUSES <- c("event", "censored", "admin_censored")

#' Build the protected C8 exit-time x status histogram of the two arms
#'
#' One atom per person (arms are disjoint), so the vector has L1 sensitivity
#' 1; cells are the H family at 0.8 of the reference calibration. Each arm's
#' signed cells are reconciled to its shared person total (the C6 N component
#' of the same people) and displayed by projection onto its displayed total.
#'
#' @param policy DP policy.
#' @param protection Route.
#' @param owner_id Scientific owner of the exit histogram.
#' @param sub Data frame: person_id, stratum ("arm=target"/"arm=comparator"),
#'   exit (days at risk, >= 1), status (one of the public statuses).
#' @param n_signed,n_display Named per-arm signed and displayed person totals.
#' @param grid Exit grid.
#' @return List: cells (per arm displayed matrix bins x statuses), export,
#'   noise_sd, allocation.
#' @keywords internal
.dsomopReleaseBuildC8 <- function(policy, protection, owner_id, sub, n_signed,
                                  n_display, grid) {
  keys <- .dsomopReleaseKeys(policy)
  statuses <- .DSOMOP_RELEASE_EXIT_STATUSES
  bin_keys <- .dsomopReleaseBinKeys(grid)
  nb <- length(bin_keys)
  alloc <- .DSOMOP_RELEASE_SURVIVAL_FRACTIONS$C8
  sub <- sub[!is.na(sub$person_id), , drop = FALSE]
  sub$person_id <- .dsomopReleasePersonIdText(sub$person_id)
  if (anyDuplicated(paste(sub$person_id, sub$stratum, sep = "\u001f"))) {
    stop("A survival substrate lists one person twice in one arm.",
         call. = FALSE)
  }
  tokens <- if (nrow(sub)) .dsomopReleasePersonTokens(keys, policy,
                                                      sub$person_id) else
    character(0)
  coords <- paste0("stratum=root|cell=", rep(bin_keys, each = length(statuses)),
                   "|status=", rep(statuses, times = nb))
  arms <- c("arm=target", "arm=comparator")
  cells <- list(); export <- list()
  for (s in arms) {
    in_s <- which(sub$stratum == s)
    b <- .dsomopReleaseBinIndex(as.numeric(sub$exit[in_s]), grid)
    b <- pmin(pmax(b, 1L), nb)
    st <- match(sub$status[in_s], statuses)
    if (anyNA(st)) stop("A survival substrate has an unknown status.",
                        call. = FALSE)
    cell <- (b - 1L) * length(statuses) + st
    tok <- tokens[in_s]
    rows_at <- split(seq_along(cell), factor(cell,
                                             levels = seq_along(coords)))
    raw <- vapply(seq_along(coords), function(k) {
      hit <- rows_at[[k]]
      fp <- if (identical(protection, "realised_sticky")) {
        .dsomopReleaseFingerprint(keys, owner_id, "H", coords[[k]], tok[hit],
                                  rep(1, length(hit)))
      } else NULL
      .dsomopReleaseNoisyComponent(policy, keys, protection, list(
        owner = owner_id, population = "c6/persons-at-risk/v1", family = "H",
        coordinate = coords[[k]], value = length(hit), allocation = alloc,
        sensitivity = 1, fingerprint = fp))
    }, numeric(1L))
    signed <- raw + (n_signed[[s]] - sum(raw)) / length(raw)
    shown <- .dsomopReleaseSimplex(raw, max(0, n_display[[s]]))
    cells[[s]] <- matrix(shown, nrow = nb, byrow = TRUE,
                         dimnames = list(bin_keys, statuses))
    export[[length(export) + 1L]] <- list(stratum = s, n = n_signed[[s]],
                                          h = signed)
  }
  e <- policy$release_epsilon
  list(cells = cells, coordinates = coords, grid = grid,
       allocation = alloc,
       noise_sd = .dsomopReleaseNoiseSd(e * alloc, 1),
       export = list(protocol = .DSOMOP_RELEASE_PROTOCOL, owner = owner_id,
                     population = "c6/persons-at-risk/v1", family = "C8",
                     grid = grid$id, breaks = grid$breaks,
                     statuses = statuses, strata_kind = "independent",
                     cell_keys = coords, cap = 1L,
                     allocation = list(H = alloc),
                     reconciliation = "exit_cells_sum_to_arm_N",
                     components = export))
}

#' Risk sets and events of the grouped-time Cox likelihood
#'
#' @param target,comparator Displayed bins x statuses matrices.
#' @return Data frame per bin: d0, d1, y0, y1.
#' @keywords internal
.dsomopReleaseCoxRiskSets <- function(target, comparator) {
  rev_cum <- function(m) rev(cumsum(rev(rowSums(m))))
  data.frame(d0 = comparator[, "event"], d1 = target[, "event"],
             y0 = rev_cum(comparator), y1 = rev_cum(target))
}

#' Maximum of the Breslow grouped-time Cox likelihood
#'
#' \code{l(b) = sum_j d1_j b - sum_j (d0_j + d1_j) log(Y0_j + Y1_j e^b)} over
#' public time groups j, with its observed information. NA when the hazard
#' ratio is not identifiable (no events, an empty arm, or a boundary
#' solution).
#'
#' @param rs Risk-set frame (\code{.dsomopReleaseCoxRiskSets}).
#' @return c(beta, se_model) or NAs, with attribute \code{reason}.
#' @keywords internal
.dsomopReleaseGroupedCoxFit <- function(rs) {
  na <- structure(c(beta = NA_real_, se_model = NA_real_),
                  reason = "not_identifiable")
  rs <- rs[is.finite(rs$d0) & is.finite(rs$d1) & (rs$d0 + rs$d1) > 0, ,
           drop = FALSE]
  if (nrow(rs) == 0L || sum(rs$d1) <= 0 || sum(rs$d0) <= 0) return(na)
  if (any(rs$y0 <= 0 & rs$y1 <= 0)) {
    rs <- rs[rs$y0 > 0 | rs$y1 > 0, , drop = FALSE]
    if (nrow(rs) == 0L) return(na)
  }
  if (all(rs$y1 <= 0) || all(rs$y0 <= 0)) return(na)
  m <- rs$d0 + rs$d1
  ll <- function(b) sum(rs$d1) * b - sum(m * log(rs$y0 + rs$y1 * exp(b)))
  opt <- stats::optimize(ll, c(-10, 10), maximum = TRUE, tol = 1e-10)
  b <- opt$maximum
  if (!is.finite(b) || abs(b) > 9.9) return(na)
  w <- rs$y0 * rs$y1 * exp(b) / (rs$y0 + rs$y1 * exp(b))^2
  info <- sum(m * w)
  if (!is.finite(info) || info <= 0) return(na)
  c(beta = b, se_model = 1 / sqrt(info))
}

#' Noise-law simulation standard deviation of a protected estimator
#'
#' Perturbs the displayed protected values with draws of their public noise
#' law (per-value calibrations), passes them through the release's own
#' post-processing (\code{refit}: projection and fit; or clipping at zero
#' first when \code{clip}) and returns the SD of the finite estimates (NA
#' unless at least half the replicates are finite). Pure post-processing,
#' deterministic in the displayed values.
#'
#' @keywords internal
.dsomopReleaseSimSd <- function(label, values, epsilon, sensitivity, refit,
                                replicates = .DSOMOP_RELEASE_SIM_REPLICATES,
                                clip = TRUE) {
  seed <- .dsomopReleaseSimSeed(label, values)
  n <- length(values)
  noise <- .dsomopReleaseSimNoise(seed, n * replicates, rep_len(epsilon, n),
                                  rep_len(sensitivity, n))
  est <- vapply(seq_len(replicates), function(r) {
    v <- values + noise[(r - 1L) * n + seq_len(n)]
    refit(if (clip) pmax(0, v) else v)
  }, numeric(1L))
  ok <- is.finite(est)
  if (sum(ok) < replicates / 2) return(NA_real_)
  stats::sd(est[ok])
}

#' Per-person exits and censoring causes of the two arms (protected horizon)
#'
#' The same observed window as the protected Poisson route (horizon-capped);
#' a person without a first outcome exits at the end of the window, which is
#' administrative censoring when the window ended at its requested end or the
#' horizon and censoring when observation ended first.
#'
#' @keywords internal
.omopReleaseCoxExits <- function(handle, cohort, arm_label, out_src, tar,
                                 horizon, data = NULL) {
  if (is.null(data)) {
    data <- .omopCmEffectArmData(handle, cohort, if (identical(
      arm_label, "arm=target")) 1L else 0L, out_src, tar,
      horizon = horizon)$data
  }
  if (!nrow(data)) {
    return(data.frame(person_id = character(0), stratum = character(0),
                      event = integer(0), time = numeric(0),
                      status = character(0), stringsAsFactors = FALSE))
  }
  observed <- .omopObservedTarSql(handle, cohort, tar, horizon = horizon)
  anchor_end <- if (identical(tar$anchor_end, "end")) "w.cohort_end_date" else
    "w.cohort_start_date"
  req_hi <- paste0("DATEADD(day, ", as.integer(tar$end) + 1L, ", ",
                   anchor_end, ")")
  cap_hi <- paste0("DATEADD(day, ", as.integer(horizon), ", w.tar_lo)")
  raw <- .omopReleaseFetch(handle, paste0(
    "SELECT w.subject_id AS person_id, CASE WHEN w.tar_hi < ", req_hi,
    " AND w.tar_hi < ", cap_hi, " THEN 1 ELSE 0 END AS lost FROM ", observed,
    " w"))
  lost <- stats::setNames(as.integer(raw$lost),
                          .dsomopReleasePersonIdText(raw$person_id))
  pid <- .dsomopReleasePersonIdText(data$person_id)
  status <- ifelse(data$event == 1L, "event",
                   ifelse(!is.na(lost[pid]) & lost[pid] == 1L, "censored",
                          "admin_censored"))
  data.frame(person_id = data$person_id, stratum = arm_label,
             event = data$event, time = data$tar_days, status = status,
             stringsAsFactors = FALSE)
}

#' Protected grouped-time Cox hazard ratio of two arms
#'
#' @param handle,cohorts,out_src,tar,outcome_key Request pieces.
#' @param route Protected route.
#' @param c6 The C6 build of the same arms (person totals shared).
#' @param sub_c6 Its substrate (time to first outcome).
#' @return List: rr (estimate, ci_lo, ci_hi, log_estimate, se_log_estimate),
#'   built (the C8 build as a release-owner list), note.
#' @keywords internal
.omopReleaseGroupedCox <- function(handle, cohorts, out_src, tar, outcome_key,
                                   route, c6, arm_data = list()) {
  horizon <- .DSOMOP_RELEASE_TIME_CAP
  exits <- rbind(
    .omopReleaseCoxExits(handle, cohorts$a, "arm=target", out_src, tar,
                         horizon, arm_data$target),
    .omopReleaseCoxExits(handle, cohorts$b, "arm=comparator", out_src, tar,
                         horizon, arm_data$comparator))
  same_anchor <- identical(tar$anchor_start, tar$anchor_end)
  max_exit <- if (same_anchor) {
    min(horizon, as.integer(tar$end) - as.integer(tar$start) + 1L)
  } else NULL
  grid <- .dsomopReleaseExitGrid(max_exit)
  st <- c6$strata
  n_signed <- stats::setNames(st$n_signed, st$stratum)
  n_display <- stats::setNames(st$n_display, st$stratum)
  owner_id <- paste0("c8/first-outcome-exit-status/", outcome_key)
  c8 <- .dsomopReleaseBuildC8(route$policy, route$protection, owner_id,
                              data.frame(person_id = exits$person_id,
                                         stratum = exits$stratum,
                                         exit = exits$time,
                                         status = exits$status,
                                         stringsAsFactors = FALSE),
                              n_signed, n_display, grid)
  target <- c8$cells[["arm=target"]]
  comparator <- c8$cells[["arm=comparator"]]
  fit <- .dsomopReleaseGroupedCoxFit(.dsomopReleaseCoxRiskSets(target,
                                                               comparator))
  na_rr <- c(estimate = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
             log_estimate = NA_real_, se_log_estimate = NA_real_)
  rr <- na_rr
  reason <- attr(fit, "reason")
  if (is.finite(fit[["beta"]])) {
    nc <- length(target)
    values <- c(as.vector(t(target)), as.vector(t(comparator)),
                n_display[["arm=target"]], n_display[["arm=comparator"]])
    e <- route$policy$release_epsilon
    # Each simulated arm is projected onto its simulated person total, as
    # released.
    refit <- function(v) {
      arm <- function(cells, n) {
        matrix(.dsomopReleaseSimplex(cells, max(0, n)), ncol = 3L,
               byrow = TRUE, dimnames = dimnames(target))
      }
      tm <- arm(v[seq_len(nc)], v[[2L * nc + 1L]])
      cm <- arm(v[nc + seq_len(nc)], v[[2L * nc + 2L]])
      .dsomopReleaseGroupedCoxFit(.dsomopReleaseCoxRiskSets(tm, cm))[["beta"]]
    }
    sim_sd <- .dsomopReleaseSimSd(
      "grouped-cox/v2", values,
      c(rep(e * .DSOMOP_RELEASE_SURVIVAL_FRACTIONS$C8, 2L * nc),
        rep(e * .DSOMOP_RELEASE_FRACTIONS$N, 2L)), 1, refit, clip = FALSE)
    se <- if (is.finite(sim_sd)) sqrt(fit[["se_model"]]^2 + sim_sd^2) else
      NA_real_
    if (is.finite(se)) {
      b <- fit[["beta"]]
      out <- c(estimate = exp(b), ci_lo = exp(b - 1.96 * se),
               ci_hi = exp(b + 1.96 * se), log_estimate = b,
               se_log_estimate = se)
      rr <- stats::setNames(.dsomopReleaseRound(out), names(out))
      reason <- NULL
    } else reason <- "noise_dominated"
  }
  # Labelled as an H-family owner of the same eligible population (its person
  # totals are the C6 components, counted once in the label's union).
  built <- list(owner = list(id = owner_id, family = "H", cap = 1L,
                             population = "c6/persons-at-risk/v1",
                             grid = list(id = grid$id), bounds = NULL,
                             estimand = paste0(
                               "exit-time bin x status (event, censored, ",
                               "administratively censored) per arm, capped ",
                               "at ", .DSOMOP_RELEASE_TIME_CAP, " days")),
                export = c8$export, c8 = c8)
  list(rr = rr, built = built, reason = reason)
}

# --- Conditional SCCS IRR from the q x exposed-status event histogram --------

#' Public q grid of the protected SCCS basis
#'
#' 36 points evenly spaced on the logit scale from 0.0001 to 0.99 (3
#' significant digits): a short window over a long observation (q of a few
#' per thousand) keeps its exposure fraction, and few cells lie where cases
#' are rare. A uniform 0.01 grid clamps every q below 0.015 to 0.01 (a
#' zero-noise log-IRR bias of about -0.6 for a 7-day window over 1-10 years
#' of observation) and spreads the noise over 198 cells.
#'
#' @return Increasing numeric vector.
#' @keywords internal
.dsomopReleaseSccsQGrid <- function() {
  signif(stats::plogis(seq(stats::qlogis(1e-4), stats::qlogis(0.99),
                           length.out = 36L)), 3)
}

# Public coordinate text of a q grid point.
.dsomopReleaseSccsQText <- function(q) {
  vapply(q, function(x) format(x, scientific = FALSE, digits = 3, trim = TRUE),
         character(1L))
}

#' Event-level SCCS substrate: per case and observation period its observed
#' exposed fraction, and the person's first K events (by date, then concept,
#' then record)
#'
#' The case and exposure definitions of the exact SCCS fit
#' (\code{.omopSccsIrrCaseData}): the first exposure opens the closed risk
#' window \code{[exposure, exposure + window]}. Each conditional stratum is one
#' (person, coalesced observation island) with an outcome record in it; its
#' \code{q} is the window's overlap with the island over the island's length
#' (half-open day counts), so distinct periods keep distinct exposure
#' fractions. Strata with \code{q} 0 or 1 carry no within-person contrast and
#' are dropped before the cap; the person's remaining records share the one
#' cap of K events. Islands are disjoint, so a record is one event.
#'
#' @return Data frame: person_id, stratum, q, n_exposed, n_unexposed (one row
#'   per informative stratum; a person may have several).
#' @keywords internal
.omopReleaseSccsCases <- function(handle, ctx, exp_src, out_src, window,
                                  cap = .DSOMOP_RELEASE_SCCS_CAP) {
  empty <- data.frame(person_id = character(0), stratum = character(0),
                      q = numeric(0), n_exposed = numeric(0),
                      n_unexposed = numeric(0), stringsAsFactors = FALSE)
  islands <- .omopObservationIslandsSql(handle)
  sj <- .omopScopeJoin(ctx, "o", out_src$person_col)
  first_exp <- paste0(
    "(SELECT x.", exp_src$person_col, " AS person_id, ",
    "MIN(x.", exp_src$date_col, ") AS exposure_date ",
    "FROM ", exp_src$table, " x ",
    "WHERE ", .omopSccsOutcomeWhere(exp_src, "x"),
    " GROUP BY x.", exp_src$person_col, ")")
  # The closed window [exposure, exposure + window] as the half-open
  # [exposure, exposure + window + 1), intersected with the island.
  win_end <- paste0("DATEADD(day, ", window, ", fe.exposure_date)")
  win_hi <- paste0("DATEADD(day, ", window + 1L, ", fe.exposure_date)")
  ov_lo <- paste0("CASE WHEN fe.exposure_date > isl.op_start ",
                  "THEN fe.exposure_date ELSE isl.op_start END")
  ov_hi <- paste0("CASE WHEN (", win_hi, ") < isl.op_end THEN (", win_hi,
                  ") ELSE isl.op_end END")
  ov_days <- .omopDateDiffDays(handle, ov_hi, ov_lo)
  exposed_event <- paste0(
    "CASE WHEN o.", out_src$date_col, " >= fe.exposure_date ",
    "AND o.", out_src$date_col, " <= (", win_end, ") THEN 1 ELSE 0 END")
  raw <- .omopReleaseFetch(handle, paste0(
    "SELECT o.", out_src$person_col, " AS person_id, o.", out_src$table_name,
    "_id AS event_id, ",
    .omopDateDiffDays(handle, paste0("o.", out_src$date_col),
                      "fe.exposure_date"), " AS day_offset, ",
    "o.", out_src$concept_col, " AS concept_id, ",
    exposed_event, " AS exposed, ",
    .omopDateDiffDays(handle, "isl.op_start", "fe.exposure_date"),
    " AS island_offset, ",
    .omopDateDiffDays(handle, "isl.op_end", "isl.op_start"),
    " AS observed_days, CASE WHEN (", ov_days, ") > 0 THEN (", ov_days,
    ") ELSE 0 END AS exposed_days FROM ", out_src$table, " o",
    " INNER JOIN ", first_exp, " fe ON fe.person_id = o.", out_src$person_col,
    " INNER JOIN ", islands, " isl ON isl.person_id = o.", out_src$person_col,
    " AND o.", out_src$date_col, " >= isl.op_start",
    " AND o.", out_src$date_col, " < isl.op_end",
    sj$join, " WHERE ", .omopSccsOutcomeWhere(out_src, "o")))
  if (!nrow(raw)) return(empty)
  num <- function(x) suppressWarnings(as.numeric(x))
  raw$person_id <- .dsomopReleasePersonIdText(raw$person_id)
  raw$event_id <- .dsomopReleasePersonIdText(raw$event_id)
  raw$stratum <- .dsomopReleaseIntegerText(num(raw$island_offset))
  observed <- num(raw$observed_days)
  raw$q <- ifelse(is.finite(observed) & observed > 0,
                  pmin(pmax(num(raw$exposed_days), 0), observed) / observed,
                  NA_real_)
  raw$exposed <- as.integer(num(raw$exposed) > 0)
  raw$day_offset <- num(raw$day_offset)
  raw$concept_id <- num(raw$concept_id)
  raw <- raw[is.finite(raw$q) & raw$q > 0 & raw$q < 1 &
               !duplicated(paste(raw$person_id, raw$event_id,
                                 sep = "\u001f")), , drop = FALSE]
  if (!nrow(raw)) return(empty)
  raw <- raw[order(raw$person_id, raw$day_offset, raw$concept_id,
                   num(raw$event_id), raw$event_id, method = "radix",
                   na.last = TRUE), , drop = FALSE]
  rank <- stats::ave(seq_len(nrow(raw)), raw$person_id, FUN = seq_along)
  raw <- raw[rank <= cap, , drop = FALSE]
  key <- paste(raw$person_id, raw$stratum, sep = "\u001f")
  first <- !duplicated(key)
  n1 <- rowsum(as.numeric(raw$exposed), key, reorder = FALSE)[key[first], 1L]
  n <- rowsum(rep(1, nrow(raw)), key, reorder = FALSE)[key[first], 1L]
  data.frame(person_id = raw$person_id[first], stratum = raw$stratum[first],
             q = raw$q[first], n_exposed = unname(n1),
             n_unexposed = unname(n - n1), stringsAsFactors = FALSE)
}

# Snap an interior exposed fraction to the public grid: clamped to the grid's
# range, nearest on the logit scale, ties to the smaller point.
.dsomopReleaseSccsQ <- function(q) {
  grid <- .dsomopReleaseSccsQGrid()
  lg <- stats::qlogis(grid)
  z <- stats::qlogis(pmin(pmax(as.numeric(q), grid[[1L]]),
                          grid[[length(grid)]]))
  mids <- (lg[-1L] + lg[-length(lg)]) / 2
  grid[findInterval(z, mids, left.open = TRUE) + 1L]
}

#' Score root of the q-grouped conditional SCCS likelihood
#'
#' \code{l(b) = b E - sum_q M_q log(1 - q + q e^b)}: its score
#' \code{E - sum_q M_q q e^b / (1 - q + q e^b)} is linear in the cells, so on
#' the signed (unclipped) protected cells it is unbiased in the noise; with
#' non-negative cells its root is the maximum likelihood estimate. Returns
#' the root with its observed information, or NA when the score has no root
#' in [-10, 10] or the information is not positive (not identifiable).
#'
#' @param q Grid points; @param m Events per point; @param e Exposed events.
#' @keywords internal
.dsomopReleaseSccsFit <- function(q, m, e) {
  na <- structure(c(beta = NA_real_, se_model = NA_real_),
                  reason = "not_identifiable")
  keep <- is.finite(q) & is.finite(m)
  q <- q[keep]; m <- m[keep]
  if (!length(q) || !is.finite(e)) return(na)
  score <- function(b) e - sum(m * q * exp(b) / (1 - q + q * exp(b)))
  lo <- score(-10); hi <- score(10)
  if (!is.finite(lo) || !is.finite(hi) || lo <= 0 || hi >= 0) return(na)
  b <- stats::uniroot(score, c(-10, 10), tol = 1e-10)$root
  if (!is.finite(b) || abs(b) > 9.9) return(na)
  w <- q * (1 - q) * exp(b) / (1 - q + q * exp(b))^2
  info <- sum(m * w)
  if (!is.finite(info) || info <= 0) return(na)
  c(beta = b, se_model = 1 / sqrt(info))
}

#' Protected release of the live SCCS incidence-rate ratio
#'
#' @keywords internal
.omopReleaseSccsIrrRun <- function(handle, ctx, params, route, snap) {
  exp_src <- .omopOutcomeSource(handle, params$exposure_concept_id,
                                params$exposure_domain_code %||% "1")
  out_src <- .omopOutcomeSource(handle, params$outcome_concept_id,
                                params$outcome_domain_code %||% "0")
  if (is.null(exp_src) || is.null(out_src)) {
    return(list(df = data.frame(), built = list()))
  }
  .omopDiagAssertPersons(handle, ctx, out_src$table, "o", out_src$person_col,
                         where_sql = .omopSccsOutcomeWhere(out_src, "o"))
  window <- max(as.integer(params$window %||% "30"), 0L)
  K <- .DSOMOP_RELEASE_SCCS_CAP
  cases <- .omopReleaseSccsCases(handle, ctx, exp_src, out_src, window, K)
  policy <- route$policy
  keys <- .dsomopReleaseKeys(policy)
  protection <- route$protection
  support <- .dsomopReleaseSupport()
  persons <- unique(cases$person_id)
  if (length(persons) < support$floor) .dsomopReleaseDisclosive()
  tokens <- .dsomopReleasePersonTokens(keys, policy, cases$person_id)
  person_tokens <- tokens[match(persons, cases$person_id)]
  frac <- .DSOMOP_RELEASE_SURVIVAL_FRACTIONS$SCCS
  owner_id <- paste0("sccs/q-exposed-status-events/K=", K, "/",
                     .dsomopFrame(
                       .omopReleaseOutcomeKey(params$exposure_concept_id,
                                              params$exposure_domain_code %||%
                                                "1"),
                       .omopReleaseOutcomeKey(params$outcome_concept_id,
                                              params$outcome_domain_code %||%
                                                "0")))
  noisy <- function(owner, family, coordinate, value, allocation, sensitivity,
                    tok, contributions) {
    fp <- if (identical(protection, "realised_sticky")) {
      .dsomopReleaseFingerprint(keys, owner, family, coordinate, tok,
                                contributions)
    } else NULL
    .dsomopReleaseNoisyComponent(policy, keys, protection, list(
      owner = owner, population = "sccs/cases/v1", family = family,
      coordinate = coordinate, value = value, allocation = allocation,
      sensitivity = sensitivity, fingerprint = fp))
  }
  n_noisy <- noisy(.DSOMOP_RELEASE_N_OWNER, "N", "stratum=root",
                   length(persons), frac$N, 1, person_tokens,
                   rep(1, length(persons)))
  grid <- .dsomopReleaseSccsQGrid()
  qg <- .dsomopReleaseSccsQ(cases$q)
  statuses <- c("unexposed", "exposed")
  coords <- paste0("cell=q=", rep(.dsomopReleaseSccsQText(grid), each = 2L),
                   "|status=", rep(statuses, times = length(grid)))
  rows_at <- split(seq_along(qg), factor(match(qg, grid),
                                         levels = seq_along(grid)))
  signed <- vapply(seq_along(coords), function(k) {
    j <- (k - 1L) %/% 2L + 1L
    s <- statuses[[(k - 1L) %% 2L + 1L]]
    hit <- rows_at[[j]]
    w <- if (identical(s, "exposed")) cases$n_exposed[hit] else
      cases$n_unexposed[hit]
    keep <- w > 0
    # One contribution per person and cell (a person's strata may share q).
    tok <- character(0); contrib <- numeric(0)
    if (any(keep)) {
      per <- tapply(w[keep], tokens[hit][keep], sum)
      tok <- names(per); contrib <- as.numeric(per)
    }
    noisy(owner_id, "W", coords[[k]], sum(w), frac$W, K, tok, contrib)
  }, numeric(1L))
  # The fit uses the signed cells (clipping them at zero manufactures events
  # in every empty cell and biases the IRR); displayed totals are clipped.
  h0 <- signed[seq(1L, length(signed), by = 2L)]
  h1 <- signed[seq(2L, length(signed), by = 2L)]
  fit <- .dsomopReleaseSccsFit(grid, h0 + h1, sum(h1))
  rr <- c(irr = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
          log_irr = NA_real_, se_log_irr = NA_real_)
  reason <- attr(fit, "reason")
  if (is.finite(fit[["beta"]])) {
    e <- policy$release_epsilon
    refit <- function(v) {
      v0 <- v[seq(1L, length(v), by = 2L)]
      v1 <- v[seq(2L, length(v), by = 2L)]
      .dsomopReleaseSccsFit(grid, v0 + v1, sum(v1))[["beta"]]
    }
    sim_sd <- .dsomopReleaseSimSd("sccs-q-irr/v2", signed, e * frac$W, K,
                                  refit, clip = FALSE)
    se <- if (is.finite(sim_sd)) sqrt(fit[["se_model"]]^2 + sim_sd^2) else
      NA_real_
    if (is.finite(se)) {
      b <- fit[["beta"]]
      out <- c(irr = exp(b), ci_lo = exp(b - 1.96 * se),
               ci_hi = exp(b + 1.96 * se), log_irr = b, se_log_irr = se)
      rr <- stats::setNames(.dsomopReleaseRound(out), names(out))
      reason <- NULL
    } else reason <- "noise_dominated"
  }
  model_type <- paste0("q_gridded_conditional_poisson_capped_k", K)
  df <- data.frame(
    model_type = model_type,
    n_cases = .dsomopReleaseRound(max(0, n_noisy)),
    exposed_events = .dsomopReleaseRound(max(0, sum(h1))),
    unexposed_events = .dsomopReleaseRound(max(0, sum(h0))),
    irr = rr[["irr"]], ci_lo = rr[["ci_lo"]], ci_hi = rr[["ci_hi"]],
    log_irr = rr[["log_irr"]], se_log_irr = rr[["se_log_irr"]],
    stringsAsFactors = FALSE)
  estimand <- paste0("event-capped (K=", K, "), q-gridded conditional SCCS ",
                     "IRR: per case and observation period the exposed ",
                     "fraction of observed time on a public logit-spaced ",
                     "grid of ", length(grid), " points in [0.0001, 0.99] ",
                     "and at most ", K, " events per person (first by ",
                     "date); the score root on the signed protected cells")
  built <- list(owner = list(id = owner_id, family = "SCCS", cap = K,
                             population = "sccs/cases/v1",
                             estimand = estimand),
                export = list(protocol = .DSOMOP_RELEASE_PROTOCOL,
                              owner = owner_id, population = "sccs/cases/v1",
                              family = "SCCS", grid = "sccs-q-logit36-v1",
                              cell_keys = coords, cap = K,
                              allocation = list(N = frac$N, W = frac$W),
                              reconciliation = "none_event_weighted_cells",
                              components = list(list(stratum = "root",
                                                     n = n_noisy,
                                                     w = signed))))
  e <- policy$release_epsilon
  components <- list(list(
    owner = owner_id, estimand = estimand, family = "SCCS",
    population = "sccs/cases/v1", grid = "sccs-q-logit36-v1",
    contribution_cap = K,
    allocation_fraction = list(N = frac$N, W = frac$W),
    allocation_epsilon = list(N = frac$N * e, W = frac$W * e),
    sensitivity = list(N = 1, W = K),
    noise_sd = list(N = .dsomopReleaseNoiseSd(frac$N * e, 1),
                    W = .dsomopReleaseNoiseSd(frac$W * e, K))))
  list(df = df, built = list(built),
       extra = c(list(estimand = estimand, components = components),
                 if (!is.null(reason)) list(unavailable_reason = paste0(
                   "The protected SCCS IRR is unavailable (", reason, ")."))))
}

# --- Standard-route person-time from the protected components -------------------

#' Compatibility route of a C6/C9 entry in standard mode
#'
#' In standard mode with the DP service available, the person-time block that
#' the standard route masks (person-days, rates, MDRR, effects) is taken from
#' the same protected components and displayed values as in auto mode and
#' matched to the standard rows by the entry's public row key. The
#' established standard counts stay in the standard block; the frame is
#' labelled "mixed". Without the DP service the stable masking stands.
#'
#' @param handle,ctx,params Request.
#' @param df Masked standard frame (labelled).
#' @param contract Release contract (with \code{compat_key}).
#' @param policy DP policy.
#' @param snap Snapped parameters.
#' @return The mixed frame.
#' @keywords internal
.omopToolsReleaseCompat <- function(handle, ctx, params, df, contract, policy,
                                    snap) {
  label_std <- attr(df, "dsomop_protection", exact = TRUE) %||% list()
  masked <- label_std$masked_fields %||% character(0)
  if (!is.data.frame(df) || nrow(df) == 0L || length(masked) == 0L) return(df)
  route <- list(protection = "realised_sticky", mode = "standard",
                policy = policy)
  run <- contract$compat_run %||% contract$run
  res <- tryCatch(run(handle, ctx, params, route, snap), error = function(e) {
    if (inherits(e, "dsomop_disclosure_error") ||
        grepl("^Disclosive", conditionMessage(e))) return(NULL)
    stop(e)
  })
  if (is.null(res) || !is.data.frame(res$df) || nrow(res$df) == 0L) return(df)
  key <- contract$compat_key
  idx <- if (is.null(key)) {
    if (nrow(res$df) == nrow(df)) seq_len(nrow(df)) else
      rep(1L, nrow(df))
  } else match(as.character(df[[key]]), as.character(res$df[[key]]))
  filled <- intersect(masked, names(res$df))
  if (!length(filled)) return(df)
  # Fields that describe the protected estimate (its model) travel with it.
  filled <- c(filled, setdiff(intersect(contract$compat_also %||% character(0),
                                        names(res$df)), filled))
  for (f in filled) df[[f]] <- res$df[[f]][idx]
  block <- contract$compat_block %||% "person_time"
  blocks <- list(standard = setdiff(names(df), filled))
  blocks[[block]] <- route$protection
  label <- .omopToolsReleaseLabel(route, res$built, contract, snap, c(
    res$extra %||% list(),
    list(protection = "mixed", blocks = blocks,
         compatibility = paste0(
           "standard mode: the ", gsub("_", "-", block), " block (",
           paste(filled, collapse = ", "), ") comes from the protected ",
           "components of this question, the other fields keep their ",
           "standard controls"))))
  rest <- setdiff(masked, filled)
  if (length(rest)) {
    label$masked_fields <- rest
    label$masked_reason <- label_std$masked_reason
  }
  attr(df, "dsomop_protection") <- label
  attr(df, "dsomop_release") <- list(
    protocol = .DSOMOP_RELEASE_PROTOCOL,
    contract = .DSOMOP_RELEASE_CONTRACT_VERSION,
    entry = contract$name, protection = "mixed",
    reference_epsilon = policy$release_epsilon,
    source = list(logical_source_id = .dsomopReleaseLogicalSourceId(policy)),
    owners = lapply(res$built, function(b) b$export))
  df
}

#' Survival/SCCS additions to the closed release registry
#'
#' The protected SCCS IRR route and the standard-mode compatibility keys of
#' the C6/C9 entries.
#'
#' @param out Registry list.
#' @return The registry.
#' @keywords internal
.omopToolsReleaseSurvivalContracts <- function(out) {
  keys <- list(
    "dsomop:incidence.rate" = "stratum",
    "dsomop:cm.mdrr" = "arm",
    "dsomop:ohdsi.cohort_method.cm_result" = "arm",
    "dsomop:cm.effect_estimate" = "arm",
    "dsomop:cm.interaction_estimate" = "subgroup_label",
    "dsomop:ohdsi.cohort_method.cm_interaction_result" = "subgroup_label",
    "dsomop:sccs.incidence_rate_ratio" = NULL)
  for (id in c("dsomop:sccs.incidence_rate_ratio")) {
    if (is.null(out[[id]])) next
    out[[id]]$run <- .omopReleaseSccsIrrRun
    out[[id]]$compat_also <- "model_type"
    out[[id]]$compat_block <- "estimate"
  }
  for (id in names(keys)) {
    if (is.null(out[[id]])) next
    out[[id]]$compat <- TRUE
    out[[id]]["compat_key"] <- list(keys[[id]])
  }
  for (id in c("dsomop:cm.diagnostics_summary",
               "dsomop:ohdsi.cohort_method.cm_diagnostics_summary")) {
    if (is.null(out[[id]])) next
    out[[id]]$compat <- TRUE
    out[[id]]["compat_key"] <- list(NULL)
    out[[id]]$compat_run <- function(handle, ctx, params, route, snap) {
      .omopReleaseCmArmsRun("mdrr")(handle, ctx, params, route, snap)
    }
  }
  out
}
