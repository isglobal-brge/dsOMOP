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
#   on the signed protected cells. The score is linear and unbiased in the
#   release noise; its selected root need not be unbiased.
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
  # Eight disjoint 28-bit uniforms per 256-bit digest; do not discard 228
  # bits and repeat the expensive hash for every simulated coordinate.
  blocks <- ceiling(n / 8L)
  h <- as.character(openssl::sha256(paste0(seed, ":", seq_len(blocks))))
  starts <- rep(seq.int(1L, 50L, by = 7L), times = blocks)
  words <- substring(rep(h, each = 8L), starts, starts + 6L)
  ((strtoi(words, 16L) + 0.5) / 2^28)[seq_len(n)]
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
  # Hash the complete public state once. Repeating a many-kilobyte framed
  # state for every counter would allocate gigabytes for a full C8 family.
  as.character(openssl::sha256(.dsomopFrame(
    "dsomop-release-simulation/v2", label, signif(as.numeric(values), 12))))
}

# --- C8: grouped-time Cox from the canonical exit-time x status basis ----------

.DSOMOP_RELEASE_EXIT_STATUSES <- c("event", "censored", "admin_censored")

#' Canonical public exit-time bins of the protected survival basis
#'
#' One versioned basis for every request (dsOMOP 2.10.3, review R06): the
#' clinical day grid L within [0, 3650], the last interval [3650, Inf) holding
#' exits at the horizon. Interval identities never depend on the requested
#' window, its spelling, scope labels or the observed exits, so equal bounded
#' exit/status maps always reach the same atoms.
#'
#' @return Grid list (id, breaks, lower, upper, unit).
#' @keywords internal
.dsomopReleaseExitGrid <- function() {
  list(id = "exit-days-clinical-v2", breaks = .dsomopReleaseDayGrid(),
       lower = 0, upper = .DSOMOP_RELEASE_TIME_CAP, unit = "days")
}

#' Fixed structural mask of the complete canonical exit basis
#'
#' Exits below one day are structurally impossible. Every remaining interval
#' and censoring cause is drawn, reconciled and displayed for every window.
#' A request-dependent mask would change the projection even when all actual
#' contribution maps are identical, creating another estimate of the same
#' atoms. Shorter windows affect per-person exits in the substrate only.
#'
#' @param tar Public request window (does not select release coordinates).
#' @param horizon Public horizon (the versioned basis uses 3650 days).
#' @return Logical matrix bins x statuses (TRUE: feasible).
#' @keywords internal
.dsomopReleaseExitMask <- function(tar, horizon = .DSOMOP_RELEASE_TIME_CAP) {
  grid <- .dsomopReleaseExitGrid()
  feasible <- grid$breaks >= 1 & grid$breaks <= .DSOMOP_RELEASE_TIME_CAP
  m <- cbind(event = feasible, censored = feasible,
             admin_censored = feasible)
  rownames(m) <- .dsomopReleaseBinKeys(grid)
  m
}

# Canonical coordinates of every cell of the basis (bin-major, then status).
.dsomopReleaseExitCoordinates <- function() {
  keys <- .dsomopReleaseBinKeys(.dsomopReleaseExitGrid())
  statuses <- .DSOMOP_RELEASE_EXIT_STATUSES
  paste0("stratum=root|cell=", rep(keys, each = length(statuses)),
         "|status=", rep(statuses, times = length(keys)))
}

#' Display of one arm's exit cells under its status parents
#'
#' Event cells are projected onto the simplex of the displayed C6 event
#' parent, censored and administratively censored cells jointly onto the
#' displayed no-event parent (displayed persons minus displayed events); the
#' projection is invariant to a uniform shift within a group, so it equals
#' the projection of the reconciled signed cells. Masked cells are zero.
#'
#' @param event,nonevent Raw (or signed) values of the feasible event and
#'   non-event cells, in canonical order.
#' @param events_display,n_display Displayed C6 parents of the arm.
#' @param mask Feasibility matrix.
#' @return Displayed bins x statuses matrix.
#' @keywords internal
.dsomopReleaseC8Display <- function(event, nonevent, events_display,
                                    n_display, mask) {
  e_disp <- max(0, events_display)
  ne_disp <- max(0, n_display - e_disp)
  feasible <- as.vector(t(mask))
  status <- rep(colnames(mask), times = nrow(mask))
  values <- numeric(length(feasible))
  is_event <- feasible & status == "event"
  is_non <- feasible & status != "event"
  values[is_event] <- .dsomopReleaseSimplex(event, e_disp)
  values[is_non] <- .dsomopReleaseSimplex(nonevent, ne_disp)
  matrix(values, nrow = nrow(mask), byrow = TRUE, dimnames = dimnames(mask))
}

#' Draw the raw protected exit cells of one owner's strata
#'
#' One atom per person and stratum (arms are disjoint), L1 sensitivity 1 at
#' 0.8 of the reference calibration, keyed on the canonical coordinate and the
#' contribution fingerprint of the people in the cell. Only the cells of the
#' request's view are drawn: a masked cell never influences the view, and a
#' shared cell reaches the same atom whatever the window.
#'
#' @param policy,protection Route.
#' @param owner_id Scientific owner of the exit histogram.
#' @param sub Data frame: person_id, stratum, exit, status.
#' @param strata Stratum keys to draw.
#' @param mask Feasibility matrix.
#' @return Named list per stratum: list(event, nonevent) raw feasible values.
#' @keywords internal
.dsomopReleaseDrawC8 <- function(policy, protection, owner_id, sub, strata,
                                 mask) {
  keys <- .dsomopReleaseKeys(policy)
  statuses <- .DSOMOP_RELEASE_EXIT_STATUSES
  grid <- .dsomopReleaseExitGrid()
  nb <- length(grid$breaks)
  alloc <- .DSOMOP_RELEASE_SURVIVAL_FRACTIONS$C8
  coords <- .dsomopReleaseExitCoordinates()
  feasible <- as.vector(t(mask))
  status_of <- rep(statuses, times = nb)
  sub <- sub[!is.na(sub$person_id), , drop = FALSE]
  sub$person_id <- .dsomopReleasePersonIdText(sub$person_id)
  if (anyDuplicated(paste(sub$person_id, sub$stratum, sep = "\u001f"))) {
    stop("A survival substrate lists one person twice in one stratum.",
         call. = FALSE)
  }
  tokens <- if (nrow(sub)) .dsomopReleasePersonTokens(keys, policy,
                                                      sub$person_id) else
    character(0)
  out <- list()
  for (s in strata) {
    in_s <- which(sub$stratum == s)
    b <- .dsomopReleaseBinIndex(as.numeric(sub$exit[in_s]), grid)
    st <- match(sub$status[in_s], statuses)
    if (anyNA(st) || anyNA(b)) {
      stop("A survival substrate has an unknown status or exit.",
           call. = FALSE)
    }
    cell <- (b - 1L) * length(statuses) + st
    if (any(!feasible[cell])) {
      stop("A survival substrate falls outside its public structural mask.",
           call. = FALSE)
    }
    tok <- tokens[in_s]
    rows_at <- split(seq_along(cell), factor(cell, levels = seq_along(coords)))
    raw <- rep(NA_real_, length(coords))
    for (k in which(feasible)) {
      hit <- rows_at[[k]]
      fp <- .dsomopReleaseFingerprint(keys, owner_id, "H", coords[[k]],
                                      tok[hit], rep(1, length(hit)))
      raw[[k]] <- .dsomopReleaseNoisyComponent(policy, keys, protection, list(
        owner = owner_id, population = "c6/persons-at-risk/v1", family = "H",
        coordinate = coords[[k]], value = length(hit), allocation = alloc,
        sensitivity = 1, fingerprint = fp))
    }
    out[[s]] <- list(event = raw[feasible & status_of == "event"],
                     nonevent = raw[feasible & status_of != "event"])
  }
  out
}

# The C6 status parents of each arm: signed (reconciled to the signed person
# total, as exported) and displayed.
.dsomopReleaseC6Parents <- function(c6) {
  st <- c6$strata
  comps <- c6$export$components
  status <- stats::setNames(lapply(comps, `[[`, "status"),
                            vapply(comps, `[[`, character(1L), "stratum"))
  stats::setNames(lapply(seq_len(nrow(st)), function(i) {
    s <- st$stratum[[i]]
    list(n_signed = st$n_signed[[i]], status_signed = status[[s]],
         n_display = st$n_display[[i]],
         events_display = st$events_display[[i]])
  }), st$stratum)
}

#' Build the protected C8 exit-time x status cells of two arms
#'
#' Signed event cells are reconciled (uniform shift, the least-squares
#' projection) to the C6 signed event parent of the same arm, censored and
#' administratively censored cells jointly to the C6 signed no-event parent;
#' the parents are never moved. Displayed cells are projected onto the
#' already displayed C6 parents (\code{.dsomopReleaseC8Display}).
#'
#' @param policy,protection Route.
#' @param owner_id Scientific owner.
#' @param sub Data frame: person_id, stratum, exit, status.
#' @param parents Per stratum C6 parents (\code{.dsomopReleaseC6Parents}).
#' @param mask Feasibility matrix (\code{.dsomopReleaseExitMask}).
#' @return List: cells (displayed matrix per stratum), raw, signed, export,
#'   allocation, noise_sd, mask.
#' @keywords internal
.dsomopReleaseBuildC8 <- function(policy, protection, owner_id, sub, parents,
                                  mask) {
  strata <- names(parents)
  raw <- .dsomopReleaseDrawC8(policy, protection, owner_id, sub, strata, mask)
  feasible <- as.vector(t(mask))
  status_of <- rep(colnames(mask), times = nrow(mask))
  coords <- .dsomopReleaseExitCoordinates()[feasible]
  is_event <- status_of[feasible] == "event"
  cells <- list(); export <- list()
  for (s in strata) {
    p <- parents[[s]]
    r <- raw[[s]]
    signed <- numeric(length(coords))
    signed[is_event] <- r$event + (p$status_signed[[1L]] - sum(r$event)) /
      max(1L, length(r$event))
    signed[!is_event] <- r$nonevent + (p$status_signed[[2L]] -
                                         sum(r$nonevent)) /
      max(1L, length(r$nonevent))
    cells[[s]] <- .dsomopReleaseC8Display(r$event, r$nonevent,
                                          p$events_display, p$n_display, mask)
    export[[length(export) + 1L]] <- list(stratum = s, n = p$n_signed,
                                          status = p$status_signed,
                                          h = signed)
  }
  e <- policy$release_epsilon
  alloc <- .DSOMOP_RELEASE_SURVIVAL_FRACTIONS$C8
  grid <- .dsomopReleaseExitGrid()
  list(cells = cells, raw = raw, coordinates = coords, grid = grid,
       mask = mask, allocation = alloc,
       noise_sd = .dsomopReleaseNoiseSd(e * alloc, 1),
       export = list(protocol = .DSOMOP_RELEASE_PROTOCOL, owner = owner_id,
                     population = "c6/persons-at-risk/v1", family = "C8",
                     grid = grid$id, breaks = grid$breaks,
                     statuses = .DSOMOP_RELEASE_EXIT_STATUSES,
                     strata_kind = "independent",
                     cell_keys = coords, cap = 1L,
                     status_of_cell = status_of[feasible],
                     display_projection = "c8-status-parent-simplex-v1",
                     structural_mask = paste0(
                       "only exits below one day are structural zeros; every ",
                       "window uses the same complete horizon basis"),
                     allocation = list(H = alloc),
                     reconciliation = paste0(
                       "event_cells_sum_to_C6_signed_event_parent_",
                       "censored_and_admin_cells_sum_to_C6_signed_no_event_",
                       "parent"),
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
#' post-processing (\code{refit}: reconciliation, projection and fit; or
#' clipping at zero first when \code{clip}) and returns the SD of the finite
#' estimates (NA unless at least half the replicates are finite). Pure
#' post-processing, deterministic in the displayed values. Every simulated
#' component gets an independent draw: equal-map components that share one
#' sticky draw in the mechanism (for example a cell that is empty in both
#' arms) are treated as independent, so the result is an approximation.
#'
#' @keywords internal
.dsomopReleaseSimSd <- function(label, values, epsilon, sensitivity, refit,
                                replicates = .DSOMOP_RELEASE_SIM_REPLICATES,
                                clip = TRUE) {
  sims <- .dsomopReleaseSimEstimates(label, values, epsilon, sensitivity,
                                     refit, replicates = replicates,
                                     clip = clip)
  if (is.null(dim(sims))) sims <- matrix(sims, ncol = 1L)
  apply(sims, 2L, function(est) {
    ok <- is.finite(est)
    if (sum(ok) < replicates / 2) return(NA_real_)
    stats::sd(est[ok])
  })
}

# Replicated estimates of the noise-law simulation (one row per replicate,
# one column per estimate returned by refit).
.dsomopReleaseSimEstimates <- function(label, values, epsilon, sensitivity,
                                       refit,
                                       replicates =
                                         .DSOMOP_RELEASE_SIM_REPLICATES,
                                       clip = TRUE) {
  seed <- .dsomopReleaseSimSeed(label, values)
  n <- length(values)
  noise <- .dsomopReleaseSimNoise(seed, n * replicates, rep_len(epsilon, n),
                                  rep_len(sensitivity, n))
  est <- lapply(seq_len(replicates), function(r) {
    v <- values + noise[(r - 1L) * n + seq_len(n)]
    as.numeric(refit(if (clip) pmax(0, v) else v))
  })
  do.call(rbind, est)
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

# Grouped Cox fit from two displayed arms.
.dsomopReleaseCoxArmsFit <- function(target, comparator) {
  .dsomopReleaseGroupedCoxFit(.dsomopReleaseCoxRiskSets(target, comparator))
}

#' Release-pipeline simulation of the two-arm grouped Cox estimator
#'
#' Rebuilds the release's display from simulated raw components, exactly as
#' the release does: per arm the displayed person total \code{max(0, N)}, the
#' displayed events from the C6 status pair projected onto it, then the exit
#' cells projected onto the displayed status parents
#' (\code{.dsomopReleaseC8Display}); then the grouped Breslow fit.
#'
#' @param arms Per arm list(n, e, ne, event, nonevent) of raw values.
#' @param mask Feasibility matrix.
#' @return Named list per arm of displayed matrices, plus \code{beta}.
#' @keywords internal
.omopReleaseCoxPipeline <- function(arms, mask) {
  disp <- lapply(arms, function(a) {
    n_disp <- .dsomopReleaseRound(max(0, a$n))
    e_disp <- .dsomopReleaseRound(.dsomopReleaseSimplex(c(a$e, a$ne),
                                                        n_disp)[[1L]])
    .dsomopReleaseC8Display(a$event, a$nonevent, e_disp, n_disp, mask)
  })
  list(cells = disp, beta = .dsomopReleaseCoxArmsFit(
    disp[["arm=target"]], disp[["arm=comparator"]])[["beta"]])
}

# Public displayed values of one arm in the order the simulation perturbs
# them, and their calibrations (N 0.2e, status pair 0.3e, cells 0.8e; all
# sensitivity 1 for these person-partitioned components).
.omopReleaseCoxSimValues <- function(parents, cells, mask, e) {
  feasible <- as.vector(t(mask))
  status_of <- rep(colnames(mask), times = nrow(mask))
  frac <- .DSOMOP_RELEASE_FRACTIONS
  vals <- list(); eps <- list()
  for (s in names(parents)) {
    p <- parents[[s]]
    flat <- as.vector(t(cells[[s]]))
    ev <- flat[feasible & status_of == "event"]
    ne <- flat[feasible & status_of != "event"]
    vals[[s]] <- c(p$n_display, p$events_display,
                   p$n_display - p$events_display, ev, ne)
    eps[[s]] <- c(e * frac$N, rep(e * frac$C6$status, 2L),
                  rep(e * .DSOMOP_RELEASE_SURVIVAL_FRACTIONS$C8,
                      length(ev) + length(ne)))
    attr(vals[[s]], "sizes") <- c(length(ev), length(ne))
  }
  list(values = vals, epsilon = eps)
}

# Inverse of .omopReleaseCoxSimValues: raw arm components from a vector.
.omopReleaseCoxSimArms <- function(v, sizes, strata) {
  out <- list(); at <- 0L
  for (s in strata) {
    ne_e <- sizes[[s]][[1L]]; ne_n <- sizes[[s]][[2L]]
    x <- v[at + seq_len(3L + ne_e + ne_n)]
    at <- at + 3L + ne_e + ne_n
    out[[s]] <- list(n = x[[1L]], e = x[[2L]], ne = x[[3L]],
                     event = x[3L + seq_len(ne_e)],
                     nonevent = x[3L + ne_e + seq_len(ne_n)])
  }
  out
}

#' Protected grouped-time Cox hazard ratio of two arms
#'
#' @param handle,cohorts,out_src,tar,outcome_key Request pieces.
#' @param route Protected route.
#' @param c6 The C6 build of the same arms (person totals and status
#'   parents shared).
#' @param arm_data Per arm first-outcome data (\code{.omopCmEffectArmData}).
#' @return List: rr (estimate, ci_lo, ci_hi, log_estimate, se_log_estimate),
#'   built (the C8 build as a release-owner list), reason.
#' @keywords internal
.omopReleaseGroupedCox <- function(handle, cohorts, out_src, tar, outcome_key,
                                   route, c6, arm_data = list()) {
  horizon <- .DSOMOP_RELEASE_TIME_CAP
  exits <- rbind(
    .omopReleaseCoxExits(handle, cohorts$a, "arm=target", out_src, tar,
                         horizon, arm_data$target),
    .omopReleaseCoxExits(handle, cohorts$b, "arm=comparator", out_src, tar,
                         horizon, arm_data$comparator))
  .omopReleaseGroupedCoxCore(route, exits, c6, tar, outcome_key)
}

#' Core of the protected grouped Cox release (substrate in hand)
#'
#' Shared by the SQL route and the utility study: the C8 build under the C6
#' parents, the fit on displayed cells and the release-pipeline simulation
#' of its noise SD.
#'
#' @param route Protected route (policy, protection).
#' @param exits Data frame: person_id, stratum, time, status.
#' @param c6 C6 build of the same arms.
#' @param tar Public window (for the structural mask).
#' @param outcome_key Canonical outcome identity.
#' @return List: rr, built, reason, cells.
#' @keywords internal
.omopReleaseGroupedCoxCore <- function(route, exits, c6, tar, outcome_key) {
  mask <- .dsomopReleaseExitMask(tar)
  exits <- .dsomopReleaseExitNormalize(exits, tar)
  parents <- .dsomopReleaseC6Parents(c6)
  owner_id <- paste0("c8/first-outcome-exit-status/", outcome_key)
  c8 <- .dsomopReleaseBuildC8(route$policy, route$protection, owner_id,
                              data.frame(person_id = exits$person_id,
                                         stratum = exits$stratum,
                                         exit = exits$time,
                                         status = exits$status,
                                         stringsAsFactors = FALSE),
                              parents, mask)
  target <- c8$cells[["arm=target"]]
  comparator <- c8$cells[["arm=comparator"]]
  fit <- .dsomopReleaseCoxArmsFit(target, comparator)
  na_rr <- c(estimate = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
             log_estimate = NA_real_, se_log_estimate = NA_real_)
  rr <- na_rr
  reason <- attr(fit, "reason")
  if (is.finite(fit[["beta"]])) {
    e <- route$policy$release_epsilon
    sim <- .omopReleaseCoxSimValues(parents, c8$cells, mask, e)
    strata <- names(parents)
    sizes <- lapply(sim$values, attr, "sizes")
    values <- unlist(lapply(sim$values, as.numeric), use.names = FALSE)
    refit <- function(v) {
      .omopReleaseCoxPipeline(.omopReleaseCoxSimArms(v, sizes, strata),
                              mask)$beta
    }
    sim_sd <- .dsomopReleaseSimSd("grouped-cox/v3", values,
                                  unlist(sim$epsilon, use.names = FALSE), 1,
                                  refit, clip = FALSE)
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
  # totals and status parents are the C6 components, counted once in the
  # label's union).
  built <- list(owner = list(id = owner_id, family = "H", cap = 1L,
                             population = "c6/persons-at-risk/v1",
                             grid = list(id = c8$grid$id), bounds = NULL,
                             estimand = paste0(
                               "exit-time bin x status (event, censored, ",
                               "administratively censored) per arm on the ",
                               "canonical basis ", c8$grid$id, ", capped at ",
                               .DSOMOP_RELEASE_TIME_CAP, " days; cells ",
                               "reconciled to the arm's C6 event and ",
                               "no-event parents")),
                export = c8$export, c8 = c8)
  list(rr = rr, built = built, reason = reason, cells = c8$cells)
}

#' Normalize per-person exits onto the public structural mask
#'
#' A deterministic per-person rule from public parameters only, so no data
#' choice decides the mask: exits are at least one day and at most the
#' public maximum; with equal anchors and a window starting at or after its
#' anchor, an administratively censored exit before the window's end is a
#' censoring and a censored exit at the end is administrative.
#'
#' @param exits Data frame with time and status.
#' @param tar Public window.
#' @param horizon Public horizon.
#' @return The normalized frame.
#' @keywords internal
.dsomopReleaseExitNormalize <- function(exits, tar,
                                        horizon = .DSOMOP_RELEASE_TIME_CAP) {
  if (!nrow(exits)) return(exits)
  same <- identical(tar$anchor_start %||% "start", tar$anchor_end %||% "end")
  start <- as.integer(tar$start %||% 0L)
  end <- as.integer(tar$end %||% 0L)
  mx <- max(1L, if (same) min(horizon, end - start + 1L) else horizon)
  t <- suppressWarnings(as.numeric(exits$time))
  t[!is.finite(t)] <- 1
  exits$time <- pmin(pmax(floor(t), 1), mx)
  if (same && start >= 0L) {
    st <- exits$status
    st[st == "admin_censored" & exits$time < mx] <- "censored"
    st[st == "censored" & exits$time >= mx] <- "admin_censored"
    exits$status <- st
  }
  exits
}

# --- Hierarchical reconciliation helpers ---------------------------------------

#' Projection onto a box-constrained simplex
#'
#' \code{argmin ||x - y||} subject to \code{0 <= x <= upper} and
#' \code{sum(x) = total}: \code{x = clip(y + lambda, 0, upper)} with the
#' unique \code{lambda} found on the piecewise-linear sum (exact breakpoints).
#'
#' @param y Values; @param total Target total; @param upper Upper bounds.
#' @return Projected values.
#' @keywords internal
.dsomopReleaseBoxSimplex <- function(y, total, upper) {
  y <- as.numeric(y)
  u <- pmax(0, as.numeric(rep_len(upper, length(y))))
  n <- length(y)
  if (n == 0L) return(numeric(0))
  total <- max(0, as.numeric(total))
  if (total <= 0) return(rep(0, n))
  if (total >= sum(u)) return(u)
  g <- function(l) sum(pmin(pmax(y + l, 0), u))
  bp <- sort(unique(c(-y, u - y)))
  vals <- vapply(bp, g, numeric(1L))
  j <- which(vals >= total)[[1L]]
  l <- if (j == 1L) bp[[1L]] else {
    l0 <- bp[[j - 1L]]; l1 <- bp[[j]]; g0 <- vals[[j - 1L]]; g1 <- vals[[j]]
    if (g1 > g0) l0 + (total - g0) * (l1 - l0) / (g1 - g0) else l1
  }
  pmin(pmax(y + l, 0), u)
}

#' Least-squares reconciliation of a table to fixed row and column parents
#'
#' The minimum-norm additive adjustment \code{X = Y + a 1' + 1 b'} with
#' \code{rowSums(X) = r} and \code{colSums(X) = c} (which need equal totals):
#' \code{X = Y + R/m (rows) + C/k (columns) - D/(k m)}.
#'
#' @param Y k x m matrix; @param r Row parents; @param c Column parents.
#' @return Reconciled matrix.
#' @keywords internal
.dsomopReleaseTwoWay <- function(Y, r, c) {
  Y <- as.matrix(Y)
  k <- nrow(Y); m <- ncol(Y)
  if (k == 0L || m == 0L) return(Y)
  R <- as.numeric(r) - rowSums(Y)
  C <- as.numeric(c) - colSums(Y)
  D <- sum(as.numeric(r)) - sum(Y)
  Y + outer(R / m, rep(1, m)) + outer(rep(1, k), C / k) - D / (m * k)
}

#' Non-negative display of a table under fixed displayed row and column parents
#'
#' Iterative proportional fitting of the positive part of the reconciled
#' table (a fixed floor keeps every cell positive) to the displayed parents.
#' A final nonnegative mass transfer from surplus columns to deficit columns
#' preserves each row and fixes both margins even if IPF reaches its iteration
#' limit. This is a deterministic feasible display, not a claim of an exact
#' KL or Euclidean minimizer after the finite iteration cap.
#'
#' @param X Reconciled (signed) k x m table.
#' @param r Displayed row parents; @param c Displayed column parents.
#' @return Displayed matrix.
#' @keywords internal
.dsomopReleaseTwoWayDisplay <- function(X, r, c, iterations = 2000L,
                                        tol = 1e-10) {
  X <- as.matrix(X)
  k <- nrow(X); m <- ncol(X)
  out <- matrix(0, k, m, dimnames = dimnames(X))
  if (k == 0L || m == 0L) return(out)
  r <- pmax(0, as.numeric(r)); c <- pmax(0, as.numeric(c))
  if (sum(r) <= 0 || sum(c) <= 0) return(out)
  c <- c * sum(r) / sum(c)
  Z <- pmax(X, 0) + 1e-9 * max(1, sum(r)) / (k * m)
  Z[r == 0, ] <- 0
  Z[, c == 0] <- 0
  scale_rows <- function(Z) {
    rs <- rowSums(Z)
    Z * ifelse(rs > 0, r / rs, 0)
  }
  for (it in seq_len(iterations)) {
    Z <- scale_rows(Z)
    cs <- colSums(Z)
    Z <- sweep(Z, 2L, ifelse(cs > 0, c / cs, 0), `*`)
    if (max(abs(rowSums(Z) - r)) <= tol * max(1, sum(r))) break
  }
  Z <- .dsomopReleaseRepairColumns(scale_rows(Z), c)
  dimnames(Z) <- dimnames(X)
  Z
}

# Repair column margins without changing nonnegative row margins. Remove a
# surplus column's mass proportionally within that column, then distribute
# each row's removed mass over the column deficits in their fixed proportions.
# No row loses more mass than it contains, and every change is post-processing.
.dsomopReleaseRepairColumns <- function(Z, target) {
  current <- colSums(Z)
  surplus <- pmax(current - target, 0)
  deficit <- pmax(target - current, 0)
  if (sum(surplus) <= 0 || sum(deficit) <= 0) return(Z)
  fraction <- ifelse(current > 0, pmin(surplus / current, 1), 0)
  removed <- sweep(Z, 2L, fraction, `*`)
  Z - removed + outer(rowSums(removed), deficit / sum(deficit))
}

#' Display pipeline of a complete subgroup family under its arm (Cox)
#'
#' Within each arm: the arm's displayed person total and C6 status (as the C6
#' build displays independent arms) and its exit cells under those parents;
#' the subgroups' person totals reconciled to the arm (a public partition),
#' their status to both their person totals and the arm's status (two-way),
#' their exit cells to both their status totals and the arm's cells
#' (two-way); displayed values are projected top-down onto the displayed
#' parents. Used by the release (raw draws) and by its noise simulation
#' (simulated raw values), so both follow one rule.
#'
#' @param arm Per arm list(n, e, ne, event, nonevent) of raw values.
#' @param subs Per subgroup, per arm the same raw values.
#' @param mask Feasibility matrix.
#' @return List: arm (per arm displays), subgroups (per subgroup per arm:
#'   n, events, signed status, cells), beta (per subgroup).
#' @keywords internal
.omopReleaseInteractionCoxPipeline <- function(arm, subs, mask,
                                                combine = NULL) {
  feasible <- as.vector(t(mask))
  status_of <- rep(colnames(mask), times = nrow(mask))
  is_event <- status_of[feasible] == "event"
  k <- length(subs)
  fam <- .omopReleaseFamilyC6(arm, subs)
  sub_out <- lapply(seq_len(k), function(i) list())
  arm_out <- list()
  for (a in names(arm)) {
    x <- arm[[a]]
    p <- fam$arm[[a]]
    ev_sig <- x$event + (p$status_signed[[1L]] - sum(x$event)) /
      max(1L, length(x$event))
    ne_sig <- x$nonevent + (p$status_signed[[2L]] - sum(x$nonevent)) /
      max(1L, length(x$nonevent))
    disp <- .dsomopReleaseC8Display(x$event, x$nonevent, p$events_display,
                                    p$n_display, mask)
    flat <- as.vector(t(disp))
    ev_disp <- flat[feasible & status_of == "event"]
    ne_disp <- flat[feasible & status_of != "event"]
    arm_out[[a]] <- c(p, list(cells = disp))
    sv <- lapply(fam$subgroups, `[[`, a)
    st <- do.call(rbind, lapply(sv, `[[`, "status_signed"))
    es_disp <- vapply(sv, `[[`, numeric(1L), "events_display")
    ns_disp <- vapply(sv, `[[`, numeric(1L), "n_display")
    # Exit cells: rows = the subgroups' status totals, columns = the arm's
    # cells of the same status group (both parents fixed).
    YE <- do.call(rbind, lapply(subs, function(s) s[[a]]$event))
    YN <- do.call(rbind, lapply(subs, function(s) s[[a]]$nonevent))
    XE <- .dsomopReleaseTwoWay(YE, st[, 1L], ev_sig)
    XN <- .dsomopReleaseTwoWay(YN, st[, 2L], ne_sig)
    DE <- .dsomopReleaseTwoWayDisplay(XE, es_disp, ev_disp)
    DN <- .dsomopReleaseTwoWayDisplay(XN, pmax(0, ns_disp - es_disp), ne_disp)
    for (i in seq_len(k)) {
      values <- numeric(length(feasible))
      values[feasible & status_of == "event"] <- DE[i, ]
      values[feasible & status_of != "event"] <- DN[i, ]
      h <- numeric(length(is_event))
      h[is_event] <- XE[i, ]
      h[!is_event] <- XN[i, ]
      sub_out[[i]][[a]] <- c(sv[[i]], list(
        h = h, cells = matrix(values, nrow = nrow(mask), byrow = TRUE,
                              dimnames = dimnames(mask))))
    }
  }
  if (!is.null(combine)) {
    sub_out <- lapply(combine, function(idx) {
      stats::setNames(lapply(names(arm), function(a) {
        parts <- lapply(sub_out[idx], `[[`, a)
        fields <- names(parts[[1L]])
        stats::setNames(lapply(fields, function(f) {
          Reduce(`+`, lapply(parts, `[[`, f))
        }), fields)
      }), names(arm))
    })
  }
  beta <- vapply(sub_out, function(s) {
    .dsomopReleaseCoxArmsFit(s[["arm=target"]]$cells,
                             s[["arm=comparator"]]$cells)[["beta"]]
  }, numeric(1L))
  list(arm = arm_out, subgroups = sub_out, beta = beta)
}

#' Person-time hierarchy of a complete subgroup family under its arms (C6)
#'
#' Per arm, from raw components: the arm's displays (as the C6 build displays
#' independent arms), the subgroups' person totals reconciled to the arm's
#' total (a public partition; displayed by projection onto the arm's
#' displayed total), their status reconciled two-way to their own totals and
#' to the arm's status pair (displayed by the box-constrained projection that
#' keeps both displayed margins) and their observed time reconciled to the
#' arm's time (displayed within [0, persons x T] summing to the arm's
#' displayed time). A child never moves its parent.
#'
#' @param arm Per arm list(n, e, ne, t) raw values.
#' @param subs Per subgroup, per arm the same raw values.
#' @return List: arm (per arm displays and signed parents), subgroups (per
#'   subgroup per arm signed and displayed values).
#' @keywords internal
.omopReleaseFamilyC6 <- function(arm, subs) {
  T_cap <- .DSOMOP_RELEASE_TIME_CAP
  k <- length(subs)
  arm_out <- list(); sub_out <- lapply(seq_len(k), function(i) list())
  for (a in names(arm)) {
    x <- arm[[a]]
    n_sig <- x$n
    st_sig <- c(x$e, x$ne) + (n_sig - x$e - x$ne) / 2
    n_disp <- max(0, x$n)
    e_disp <- .dsomopReleaseSimplex(c(x$e, x$ne), n_disp)[[1L]]
    t_sig <- x$t %||% NA_real_
    t_disp <- if (is.finite(t_sig)) min(max(t_sig, 0), n_disp * T_cap) else
      NA_real_
    arm_out[[a]] <- list(n_signed = n_sig, status_signed = st_sig,
                         n_display = n_disp, events_display = e_disp,
                         time_signed = t_sig, time_display = t_disp)
    ns <- vapply(subs, function(s) s[[a]]$n, numeric(1L))
    ns_sig <- ns + (n_sig - sum(ns)) / k
    ns_disp <- .dsomopReleaseSimplex(ns, n_disp)
    Y <- do.call(rbind, lapply(subs, function(s) c(s[[a]]$e, s[[a]]$ne)))
    X <- .dsomopReleaseTwoWay(Y, ns_sig, st_sig)
    es_disp <- .dsomopReleaseBoxSimplex((X[, 1L] + ns_disp - X[, 2L]) / 2,
                                        e_disp, ns_disp)
    ts <- vapply(subs, function(s) s[[a]]$t %||% NA_real_, numeric(1L))
    has_t <- all(is.finite(ts)) && is.finite(t_sig)
    ts_sig <- if (has_t) ts + (t_sig - sum(ts)) / k else rep(NA_real_, k)
    ts_disp <- if (has_t) {
      .dsomopReleaseBoxSimplex(ts_sig, t_disp, ns_disp * T_cap)
    } else rep(NA_real_, k)
    for (i in seq_len(k)) {
      sub_out[[i]][[a]] <- list(
        n_signed = ns_sig[[i]], n_display = ns_disp[[i]],
        status_signed = as.numeric(X[i, ]), events_display = es_disp[[i]],
        time_signed = ts_sig[[i]], time_display = ts_disp[[i]])
    }
  }
  list(arm = arm_out, subgroups = sub_out)
}

# Raw C6 components of the strata of a C6 build (independent arms).
.omopReleaseC6Raw <- function(c6) {
  st <- c6$strata
  stats::setNames(lapply(seq_len(nrow(st)), function(i) {
    list(n = st$n_noisy[[i]], e = st$event_noisy[[i]],
         ne = st$no_event_noisy[[i]], t = st$time_noisy[[i]])
  }), st$stratum)
}

#' Rewrite a subgroup C6 build with its reconciled family values
#'
#' @param built C6 build of one subgroup.
#' @param fam Its per-arm values from \code{.omopReleaseFamilyC6}.
#' @return The build with reconciled displayed strata and signed export.
#' @keywords internal
.omopReleaseFamilyC6Apply <- function(built, fam) {
  st <- built$strata
  for (j in seq_len(nrow(st))) {
    v <- fam[[st$stratum[[j]]]]
    st$n_signed[[j]] <- v$n_signed
    st$n_display[[j]] <- .dsomopReleaseRound(v$n_display)
    st$events_display[[j]] <- .dsomopReleaseRound(v$events_display)
    st$time_display[[j]] <- .dsomopReleaseRound(v$time_display)
  }
  built$strata <- st
  built$export$components <- lapply(built$export$components, function(comp) {
    v <- fam[[comp$stratum]]
    comp$n <- v$n_signed
    comp$status <- v$status_signed
    comp$time <- v$time_signed
    comp
  })
  built$export$reconciliation <- paste0(
    "subgroup_N_sum_to_arm_N_status_two_way_to_subgroup_N_and_arm_status_",
    "time_sum_to_arm_time")
  built$export$parent <- "arm"
  built
}

#' Protected grouped-time Cox fits of a complete subgroup family
#'
#' The arm's exit cells (the ordinary C8 atoms of the same arms) and every
#' subgroup's exit cells on the canonical basis (one atom per person across
#' the family: sensitivity 1), reconciled within the declared hierarchy
#' (\code{.omopReleaseInteractionCoxPipeline}); each subgroup's grouped
#' Breslow fit uses only its displayed protected cells, and its noise SD comes
#' from the release-pipeline simulation of the whole family.
#'
#' @param route Protected route.
#' @param exits Arm exits (person_id, stratum, time, status).
#' @param arm_c6 C6 build of the arms.
#' @param sub_c6 C6 builds of the subgroups (raw draws).
#' @param members Per subgroup list(target, comparator) of person ids.
#' @param tar Public window; @param outcome_key Outcome identity.
#' @param combine Optional canonical atom indices for each requested view.
#' @param family_meta Public family identity, canonical atom keys and view keys.
#' @return List: rr (per subgroup named vectors), reason (per subgroup),
#'   built (C8 owner lists: arm, then subgroups), family (the pipeline).
#' @keywords internal
.omopReleaseInteractionCoxCore <- function(route, exits, arm_c6, sub_c6,
                                           members, tar, outcome_key,
                                           combine = NULL, family_meta = NULL) {
  policy <- route$policy
  mask <- .dsomopReleaseExitMask(tar)
  exits <- .dsomopReleaseExitNormalize(exits, tar)
  owner_id <- paste0("c8/first-outcome-exit-status/", outcome_key)
  arms <- c("arm=target", "arm=comparator")
  sub_df <- data.frame(person_id = .dsomopReleasePersonIdText(exits$person_id),
                       stratum = exits$stratum, exit = exits$time,
                       status = exits$status, stringsAsFactors = FALSE)
  raw8 <- .dsomopReleaseDrawC8(policy, route$protection, owner_id, sub_df,
                               arms, mask)
  raw6 <- .omopReleaseC6Raw(arm_c6)
  arm <- stats::setNames(lapply(arms, function(a) c(raw6[[a]], raw8[[a]])),
                         arms)
  subs <- lapply(seq_along(members), function(i) {
    m <- members[[i]]
    keep <- (sub_df$stratum == "arm=target" &
               sub_df$person_id %in% m$target) |
      (sub_df$stratum == "arm=comparator" &
         sub_df$person_id %in% m$comparator)
    r8 <- .dsomopReleaseDrawC8(policy, route$protection, owner_id,
                               sub_df[keep, , drop = FALSE], arms, mask)
    r6 <- .omopReleaseC6Raw(sub_c6[[i]])
    stats::setNames(lapply(arms, function(a) c(r6[[a]], r8[[a]])), arms)
  })
  pipe <- .omopReleaseInteractionCoxPipeline(arm, subs, mask, combine)
  k_atoms <- length(subs)
  k <- length(pipe$subgroups)
  fits <- lapply(pipe$subgroups, function(s) {
    .dsomopReleaseCoxArmsFit(s[["arm=target"]]$cells,
                             s[["arm=comparator"]]$cells)
  })
  e <- policy$release_epsilon
  frac <- .DSOMOP_RELEASE_FRACTIONS
  feasible <- as.vector(t(mask))
  status_of <- rep(colnames(mask), times = nrow(mask))
  flat_parts <- function(d) {
    flat <- as.vector(t(d$cells))
    c(d$n_display, d$events_display, d$n_display - d$events_display,
      flat[feasible & status_of == "event"],
      flat[feasible & status_of != "event"])
  }
  b_e <- sum(feasible & status_of == "event")
  b_n <- sum(feasible & status_of != "event")
  width <- 3L + b_e + b_n
  sim_pipe <- if (is.null(combine)) pipe else
    .omopReleaseInteractionCoxPipeline(arm, subs, mask)
  values <- c(unlist(lapply(arms, function(a) flat_parts(sim_pipe$arm[[a]]))),
              unlist(lapply(sim_pipe$subgroups, function(s) {
                unlist(lapply(arms, function(a) flat_parts(s[[a]])))
              })))
  eps_block <- c(e * frac$N, rep(e * frac$C6$status, 2L),
                 rep(e * .DSOMOP_RELEASE_SURVIVAL_FRACTIONS$C8, b_e + b_n))
  epsilon <- rep(eps_block, times = 2L * (1L + k_atoms))
  unpack <- function(x) list(n = x[[1L]], e = x[[2L]], ne = x[[3L]],
                             event = x[3L + seq_len(b_e)],
                             nonevent = x[3L + b_e + seq_len(b_n)])
  refit <- function(v) {
    block <- function(j) v[(j - 1L) * width + seq_len(width)]
    arm_s <- stats::setNames(lapply(1:2, function(j) unpack(block(j))), arms)
    subs_s <- lapply(seq_len(k_atoms), function(i) {
      stats::setNames(lapply(1:2, function(j) {
        unpack(block(2L + (i - 1L) * 2L + j))
      }), arms)
    })
    .omopReleaseInteractionCoxPipeline(arm_s, subs_s, mask, combine)$beta
  }
  any_fit <- any(vapply(fits, function(f) is.finite(f[["beta"]]),
                        logical(1L)))
  sim_sd <- if (any_fit) {
    .dsomopReleaseSimSd("interaction-grouped-cox/v1", values, epsilon, 1,
                        refit, clip = FALSE)
  } else rep(NA_real_, k)
  na_rr <- c(estimate = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
             log_estimate = NA_real_, se_log_estimate = NA_real_)
  rr <- vector("list", k); reason <- vector("list", k)
  for (i in seq_len(k)) {
    f <- fits[[i]]
    rr[[i]] <- na_rr
    reason[i] <- list(attr(f, "reason"))
    if (!is.finite(f[["beta"]])) next
    se <- if (is.finite(sim_sd[[i]])) sqrt(f[["se_model"]]^2 +
                                             sim_sd[[i]]^2) else NA_real_
    if (!is.finite(se)) {
      reason[[i]] <- "noise_dominated"
      next
    }
    b <- f[["beta"]]
    out <- c(estimate = exp(b), ci_lo = exp(b - 1.96 * se),
             ci_hi = exp(b + 1.96 * se), log_estimate = b,
             se_log_estimate = se)
    rr[[i]] <- stats::setNames(.dsomopReleaseRound(out), names(out))
    reason[i] <- list(NULL)
  }
  coords <- .dsomopReleaseExitCoordinates()[feasible]
  is_event <- status_of[feasible] == "event"
  grid <- .dsomopReleaseExitGrid()
  arm_export <- lapply(arms, function(a) {
    p <- pipe$arm[[a]]
    x <- arm[[a]]
    h <- numeric(length(is_event))
    h[is_event] <- x$event + (p$status_signed[[1L]] - sum(x$event)) /
      max(1L, length(x$event))
    h[!is_event] <- x$nonevent + (p$status_signed[[2L]] - sum(x$nonevent)) /
      max(1L, length(x$nonevent))
    list(stratum = a, n = p$n_signed, status = p$status_signed, h = h)
  })
  family_basis <- .dsomopReleaseFamilyBasis(arm_export,
    lapply(sim_pipe$subgroups, function(s) lapply(arms, function(a) {
      v <- s[[a]]
      list(stratum = a, n = v$n_signed, status = v$status_signed, h = v$h)
    })), combine, family_meta, "finite-ipf-feasible-repair-v1")
  family_basis$status_of_cell <- status_of[feasible]
  export_of <- function(components, reconciliation, parent = NULL) {
    c(list(protocol = .DSOMOP_RELEASE_PROTOCOL, owner = owner_id,
           population = "c6/persons-at-risk/v1", family = "C8",
           grid = grid$id, breaks = grid$breaks,
           statuses = .DSOMOP_RELEASE_EXIT_STATUSES,
           strata_kind = "independent", cell_keys = coords, cap = 1L,
           status_of_cell = status_of[feasible],
           display_projection = "c8-status-parent-simplex-v1",
           allocation = list(H = .DSOMOP_RELEASE_SURVIVAL_FRACTIONS$C8),
           reconciliation = reconciliation),
      if (!is.null(parent)) list(parent = parent),
      list(components = components))
  }
  owner_of <- function(estimand) {
    list(id = owner_id, family = "H", cap = 1L,
         population = "c6/persons-at-risk/v1", grid = list(id = grid$id),
         bounds = NULL, estimand = estimand)
  }
  built <- c(list(list(
    owner = owner_of(paste0("exit-time bin x status per arm on the ",
                            "canonical basis ", grid$id)),
    export = export_of(arm_export, paste0(
      "event_cells_sum_to_C6_signed_event_parent_censored_and_admin_",
      "cells_sum_to_C6_signed_no_event_parent")))),
    lapply(seq_len(k), function(i) {
      comps <- lapply(arms, function(a) {
        v <- pipe$subgroups[[i]][[a]]
        list(stratum = a, n = v$n_signed, status = v$status_signed, h = v$h)
      })
      ex <- export_of(comps, paste0(
        "subgroup_cells_two_way_to_subgroup_status_and_arm_cells"), parent = "arm")
      ex$family_basis <- c(family_basis, list(view_index = i))
      list(owner = owner_of(paste0(
        "exit-time bin x status per arm within a subgroup of the family on ",
        "the canonical basis ", grid$id)),
        export = ex)
    }))
  list(rr = rr, reason = reason, built = built, family = pipe)
}

# --- Conditional SCCS IRR from the q x exposed-status event histogram --------

#' Public q grid of the protected SCCS basis
#'
#' 36 points evenly spaced on the logit scale from 0.0001 to 0.99 (3
#' significant digits): a short window over a long observation (q of a few
#' per thousand) keeps its exposure fraction, and few cells lie where cases
#' are rare. A uniform 0.01 grid would clamp every q below 0.015 to 0.01 (a
#' zero-noise log-IRR bias of about -0.6 for a 7-day window over 1-10 years
#' of observation) and spread the noise over 198 cells. Snapping q to the
#' nearest grid point changes the estimand: for a homogeneous q inside the
#' grid range the noise-free log-IRR shifts by \code{logit(q) -
#' logit(q_grid)}, at most half the logit spacing (about 0.2); heterogeneous
#' q within a cell partly cancel. Informative strata outside the grid range
#' are retained and clamped to its endpoints; the bound does not apply there
#' (for example q=0.999 incurs about +2.3 log-IRR bias when clamped to 0.99).
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
#' fractions. Only strata with q=0 or q=1 are excluded because they carry no
#' within-person contrast. Every informative stratum with 0<q<1 is retained;
#' exposed fractions outside the public grid range [0.0001, 0.99] are clamped
#' to its endpoints. This can cause substantial approximation bias near the
#' boundaries: homogeneous q=0.999 clamped to 0.99 shifts the noise-free
#' log-IRR by about +2.3. Exclusion depends only on exposure and observation
#' geometry, never on event timing. The person's
#' remaining records share the one cap of K events. Islands are disjoint, so
#' a record is one event.
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

#' Estimating-equation root of the q-grouped conditional SCCS score
#'
#' \code{U(b) = E - sum_q M_q q e^b / (1 - q + q e^b)}, the score of the
#' conditional likelihood \code{l(b) = b E - sum_q M_q log(1 - q + q e^b)}.
#' \code{U} is linear in the cells, so on the signed (unclipped) protected
#' cells it is unbiased in the release noise (its root need not be). With
#' non-negative cells \code{U} is strictly decreasing and its root is the
#' maximum-likelihood estimate; with signed cells it can be non-monotone.
#' Root selection: interval bounds for the score and its derivative certify
#' all of [-10, 10]; monotone intervals locate and refine each root. A
#' numerically unresolved interval gives \code{uncertified_root}. A root is
#' admissible when the information
#' \code{sum_q M_q w_q(b)} (the negative score slope) is positive and
#' \code{|b| <= 9.9}. Exactly one admissible root is returned with its
#' information; otherwise a stable NA with reason \code{"no_root"} (no sign
#' change), \code{"multiple_roots"} (more than one admissible root),
#' \code{"nonpositive_information"} (only roots where the score increases)
#' or \code{"boundary"} (only roots beyond 9.9).
#'
#' @param q Grid points; @param m Events per point; @param e Exposed events.
#' @return c(beta, se_model) with attribute \code{reason} when NA.
#' @keywords internal
.dsomopReleaseSccsFit <- function(q, m, e) {
  na <- function(reason) structure(c(beta = NA_real_, se_model = NA_real_),
                                   reason = reason)
  keep <- is.finite(q) & is.finite(m)
  q <- q[keep]; m <- m[keep]
  if (!length(q) || !is.finite(e) || all(m == 0)) {
    return(na("not_identifiable"))
  }
  score <- function(b) e - sum(m * q * exp(b) / (1 - q + q * exp(b)))
  info <- function(b) sum(m * q * (1 - q) * exp(b) / (1 - q + q * exp(b))^2)
  # Interval bounds certify every part of the parameter range: endpoint
  # signs alone (or a finite grid alone) can miss pairs of nearby roots of
  # the signed score. p(b) is monotone; p(1-p) is unimodal with peak 1/4.
  log_odds <- stats::qlogis(q)
  roots <- numeric(0)
  uncertain <- FALSE
  visited <- 0L
  visit <- function(lo, hi, depth = 0L) {
    if (uncertain) return(invisible(NULL))
    visited <<- visited + 1L
    if (visited > 8192L) {
      uncertain <<- TRUE
      return(invisible(NULL))
    }
    pl <- stats::plogis(log_odds + lo)
    ph <- stats::plogis(log_odds + hi)
    ul <- e - sum(ifelse(m >= 0, m * ph, m * pl))
    uh <- e - sum(ifelse(m >= 0, m * pl, m * ph))
    if (ul > 0 || uh < 0) return(invisible(NULL))
    wl <- pl * (1 - pl); wh <- ph * (1 - ph)
    wmin <- pmin(wl, wh)
    wmax <- pmax(wl, wh)
    wmax[log_odds + lo <= 0 & log_odds + hi >= 0] <- 0.25
    il <- sum(ifelse(m >= 0, m * wmin, m * wmax))
    ih <- sum(ifelse(m >= 0, m * wmax, m * wmin))
    sl <- score(lo); sh <- score(hi)
    if (il > 0 || ih < 0) {
      # Strictly monotone on this complete interval: at most one root.
      if (sl == 0) roots <<- c(roots, lo)
      if (sh == 0) roots <<- c(roots, hi)
      if (sl * sh < 0) roots <<- c(roots, stats::uniroot(
        score, c(lo, hi), tol = 1e-12)$root)
      return(invisible(NULL))
    }
    if (depth >= 32L || hi - lo < 1e-8) {
      uncertain <<- TRUE
      return(invisible(NULL))
    }
    mid <- (lo + hi) / 2
    visit(lo, mid, depth + 1L)
    visit(mid, hi, depth + 1L)
    invisible(NULL)
  }
  visit(-10, 10)
  if (uncertain) return(na("uncertified_root"))
  roots <- sort(unique(roots))
  if (length(roots) > 1L) roots <- roots[c(TRUE, diff(roots) > 1e-8)]
  if (!length(roots)) return(na("no_root"))
  inf <- vapply(roots, info, numeric(1L))
  ok <- is.finite(inf) & inf > 0 & abs(roots) <= 9.9
  if (sum(ok) > 1L) return(na("multiple_roots"))
  if (sum(ok) == 0L) {
    return(na(if (any(is.finite(inf) & inf > 0)) "boundary" else
      "nonpositive_information"))
  }
  b <- roots[ok]
  c(beta = b, se_model = 1 / sqrt(inf[ok]))
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
  owner_id <- paste0("sccs/q-exposed-status-events/K=", K, "/",
                     .dsomopFrame(
                       .omopReleaseOutcomeKey(params$exposure_concept_id,
                                              params$exposure_domain_code %||%
                                                "1"),
                       .omopReleaseOutcomeKey(params$outcome_concept_id,
                                              params$outcome_domain_code %||%
                                                "0")))
  .omopReleaseSccsCore(route, cases, owner_id, K)
}

#' Core of the protected SCCS release (case strata in hand)
#'
#' Shared by the SQL route and the utility study: the case-total and
#' weighted q x exposed-status components, the estimating-equation fit and
#' the release-pipeline simulation of its noise SD.
#'
#' @param route Protected route.
#' @param cases Case strata (\code{.omopReleaseSccsCases}).
#' @param owner_id Scientific owner.
#' @param K Event cap.
#' @return List: df, built, extra.
#' @keywords internal
.omopReleaseSccsCore <- function(route, cases, owner_id,
                                 K = .DSOMOP_RELEASE_SCCS_CAP) {
  policy <- route$policy
  keys <- .dsomopReleaseKeys(policy)
  protection <- route$protection
  support <- .dsomopReleaseSupport()
  persons <- unique(cases$person_id)
  if (length(persons) < support$floor) .dsomopReleaseDisclosive()
  tokens <- .dsomopReleasePersonTokens(keys, policy, cases$person_id)
  person_tokens <- tokens[match(persons, cases$person_id)]
  frac <- .DSOMOP_RELEASE_SURVIVAL_FRACTIONS$SCCS
  noisy <- function(owner, family, coordinate, value, allocation, sensitivity,
                    tok, contributions) {
    fp <- .dsomopReleaseFingerprint(keys, owner, family, coordinate, tok,
                                    contributions)
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
                     "IRR: one stratum per case and merged observation ",
                     "period (island), its exposed fraction of observed time ",
                     "snapped to a public logit-spaced grid of ", length(grid),
                     " points in [0.0001, 0.99] (informative q outside that range ",
                     "clamped; only q=0 or q=1 excluded), at most ", K, " events per person (first by ",
                     "date); the estimating-equation root of the conditional ",
                     "score on the signed protected cells (not the exact ",
                     "per-person fit)")
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
