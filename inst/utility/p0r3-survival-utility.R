#!/usr/bin/env Rscript
# Run from the dsOMOP source root on the R test pod:
# Rscript inst/utility/p0r3-survival-utility.R [replicates=100] [output-directory=inst/utility/results] [sampler=law|crypto] [scenario-ids=1,2,...,12]
# Uses production contribution fingerprints, HMAC contexts, complete-basis
# projection and 200-refit SE. Default law mode samples the calibrated
# discrete-Laplace law using two inverse-geometric draws (28-bit uniforms),
# preserving exactly the same equal-context correlations. Optional crypto
# mode uses the production arbitrary-precision CKS sampler and is much slower.
# Law mode is a utility study, not a validation of exact privacy arithmetic.
# Seed 2026111200 + scenario*10000 + replicate determines synthetic sampling;
# each replicate has an independent synthetic corpus identity (same fixed
# test root), so its sticky noise includes equal-map correlations.
# Informative = finite 95% CI whose upper/lower ratio is <= 4. Coverage is
# reported both conditional on finite intervals and over all replicates.
# Bias/RMSE use the log-HR/log-IRR scale, conditional on finite estimates.
args <- commandArgs(TRUE)
replicates <- if (length(args)) as.integer(args[[1L]]) else 100L
outdir <- if (length(args) >= 2L) args[[2L]] else "inst/utility/results"
sampler <- if (length(args) >= 3L) args[[3L]] else "law"
selected <- if (length(args) >= 4L) as.integer(strsplit(args[[4L]], ",")[[1L]]) else 1:12
stopifnot(is.finite(replicates), replicates > 0, sampler %in% c("law", "crypto"))
pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
source("tests/testthat/helper-db.R")
source("tests/testthat/helper-tools-release.R")

main <- function() {
  .release_state()
  h <- create_test_handle(n_persons = 5)
  on.exit(cleanup_handle(h), add = TRUE)
  policy <- suppressMessages(.dsomopToolsDpReady(h))$policy
  stopifnot(identical(policy$release_epsilon, 0.1))
  # Bootstrap/self-tests above always use the exact production sampler.
  if (identical(sampler, "law")) {
    testthat::local_mocked_bindings(.dsomopDpDiscreteLaplace = law_sampler,
                                    .package = "dsOMOP")
  }
  # Retain executable provenance and measure the pre-repair residual without
  # changing the production projection. Calls 1:4 are the released two-arm
  # family; later calls are the 200 uncertainty refits.
  source_sha256 <- as.character(openssl::sha256(readBin(
    "R/tools_release_survival.R", "raw",
    n = file.info("R/tools_release_survival.R")$size)))
  margin_diag <- list(calls = 0L, nonzero = 0L, material = 0L,
                      max_relative = 0, initial_max_relative = 0)
  repair_columns <- .dsomopReleaseRepairColumns
  testthat::local_mocked_bindings(.dsomopReleaseRepairColumns = function(Z, target) {
    relative <- max(abs(colSums(Z) - target)) / max(1, sum(target))
    margin_diag$calls <<- margin_diag$calls + 1L
    margin_diag$nonzero <<- margin_diag$nonzero + as.integer(relative > 0)
    margin_diag$material <<- margin_diag$material + as.integer(relative > 1e-8)
    margin_diag$max_relative <<- max(margin_diag$max_relative, relative)
    if (margin_diag$calls <= 4L) margin_diag$initial_max_relative <<-
      max(margin_diag$initial_max_relative, relative)
    repair_columns(Z, target)
  }, .package = "dsOMOP")
  scenarios <- data.frame(
    model = c(rep("cox", 3), rep("interaction_cox", 2), rep("sccs", 3),
              "cox", "cox", "sccs", "sccs"),
    n = c(500L, 2000L, 10000L, 2000L, 10000L, 500L, 2000L, 10000L,
          rep(2000L, 4)),
    q_min = c(rep(NA_real_, 5), 0.001, 0.02, 0.02, NA, NA, 0.95, 0.02),
    q_max = c(rep(NA_real_, 5), 0.02, 0.5, 0.5, NA, NA, 0.999, 0.5),
    horizon_days = c(rep(366L, 8), 366L, 7L, NA, NA),
    baseline_event_days = c(rep(730, 8), 7300, 730, NA, NA),
    periods_per_person = c(rep(1L, 11), 2L),
    stringsAsFactors = FALSE)
  outcome_key <- "utility-first-outcome/v1"
  owner <- c(list(id = "utility-arms/v1", population = "c6/persons-at-risk/v1",
                  strata = .omopReleaseArmStrata(), cap = 1L,
                  estimand = "synthetic first-outcome arms"),
             .omopReleaseC6Owners(outcome_key, "to_first"))
  result <- list()
  for (j in intersect(seq_len(nrow(scenarios)), selected)) {
    scenario <- scenarios[j, ]
    for (r in seq_len(replicates)) {
      started <- proc.time()[["elapsed"]]
      margin_diag <- list(calls = 0L, nonzero = 0L, material = 0L,
                          max_relative = 0, initial_max_relative = 0)
      seed <- 2026111200L + j * 10000L + r
      set.seed(seed)
      p <- policy
      p$domain <- paste0("p0r3-utility-synthetic-corpus-", seed)
      route <- list(policy = p, protection = "realised_sticky", mode = "auto")
      beta <- log(1.5)
      n <- scenario$n
      shared_empty <- NA_real_
      target_censoring <- comparator_censoring <- excluded_strata <- NA_real_
      eligible_persons <- max_events_person <- NA_real_
      tar <- list(start = 0L, end = scenario$horizon_days - 1L,
                  anchor_start = "start", anchor_end = "start")
      if (identical(scenario$model, "sccs")) {
        periods <- scenario$periods_per_person
        person <- rep(as.character(seq_len(n)), each = periods)
        island <- rep(seq_len(periods), times = n)
        q <- plogis(runif(n * periods, qlogis(scenario$q_min),
                         qlogis(scenario$q_max)))
        m <- 1 + rpois(n * periods, if (periods == 1L) 3 else 7)
        if (periods > 1L) m[island == 1L] <- pmin(8, m[island == 1L])
        # Production selects informative OP strata before applying one K10
        # cap to each person's chronological records. This conditional-model
        # study fixes retained totals first, then samples exposed counts; it
        # does not model bias induced by truncating a full event-time process.
        keep <- is.finite(q) & q > 0 & q < 1
        excluded_strata <- mean(!keep)
        person <- person[keep]; island <- island[keep]; q <- q[keep]; m <- m[keep]
        if (periods == 1L) m <- pmin(10, m) else {
          prior <- ave(m, person, FUN = function(x) c(0, head(cumsum(x), -1L)))
          m <- pmin(m, pmax(0, 10 - prior))
        }
        keep <- m > 0
        person <- person[keep]; island <- island[keep]; q <- q[keep]; m <- m[keep]
        max_events_person <- max(tapply(m, person, sum))
        eligible_persons <- length(unique(person))
        stopifnot(max_events_person <= 10)
        exposed <- rbinom(length(m), m, plogis(qlogis(q) + beta))
        cases <- data.frame(person_id = person,
                            stratum = paste0("island=", island), q = q,
                            n_exposed = exposed, n_unexposed = m - exposed)
        fit <- .omopReleaseSccsCore(route, cases, "utility-sccs/v1")
        oracle <- .dsomopReleaseSccsFit(.dsomopReleaseSccsQ(q), m,
                                       sum(exposed))[["beta"]]
        estimates <- data.frame(level = "all", log_estimate = fit$df$log_irr,
          se = fit$df$se_log_irr, lo = fit$df$ci_lo, hi = fit$df$ci_hi,
          oracle = oracle)
      } else {
        arm <- rep(c("arm=target", "arm=comparator"), each = n)
        duration <- rexp(2L * n, rate = ifelse(arm == "arm=target", 1.5, 1) /
                           scenario$baseline_event_days)
        event <- as.integer(duration < scenario$horizon_days)
        exit <- pmin(scenario$horizon_days, pmax(1, floor(duration) + 1))
        target_censoring <- mean(event[arm == "arm=target"] == 0)
        comparator_censoring <- mean(event[arm == "arm=comparator"] == 0)
        exits <- data.frame(person_id = as.character(seq_len(2L * n)),
          stratum = arm, event = event, time = exit,
          status = ifelse(event == 1L, "event", "admin_censored"))
        sub <- exits[c("person_id", "stratum", "event", "time")]
        c6 <- .dsomopReleaseBuildC6(p, route$protection, owner, sub)
        grid <- .dsomopReleaseExitGrid()
        exact_cells <- function(idx, a) {
          z <- exits[idx & exits$stratum == a, ]
          cell <- (.dsomopReleaseBinIndex(z$time, grid) - 1L) * 3L +
            match(z$status, .DSOMOP_RELEASE_EXIT_STATUSES)
          matrix(tabulate(cell, 3L * length(grid$breaks)), byrow = TRUE,
                 ncol = 3L, dimnames = list(NULL, .DSOMOP_RELEASE_EXIT_STATUSES))
        }
        oracle <- function(idx) .dsomopReleaseCoxArmsFit(
          exact_cells(idx, "arm=target"), exact_cells(idx, "arm=comparator"))[["beta"]]
        if (identical(scenario$model, "cox")) {
          fit <- .omopReleaseGroupedCoxCore(route, exits, c6, tar, outcome_key)
          estimates <- data.frame(level = "all", log_estimate = fit$rr[["log_estimate"]],
            se = fit$rr[["se_log_estimate"]], lo = fit$rr[["ci_lo"]],
            hi = fit$rr[["ci_hi"]], oracle = oracle(rep(TRUE, 2L * n)))
          # The same empty-map coordinate has one sticky draw in both arms.
          # This equality is retained by real crypto; SE simulation does not
          # model its private map equivalence and is approximate.
          raw <- fit$built$c8$raw
          mask <- fit$built$c8$mask[, "event"]
          empty <- exact_cells(rep(TRUE, 2L * n), "arm=target")[mask, "event"] == 0 &
            exact_cells(rep(TRUE, 2L * n), "arm=comparator")[mask, "event"] == 0
          shared_empty <- sum(empty & raw[[1L]]$event == raw[[2L]]$event)
        } else {
          group <- rep(rep(1:3, length.out = n), 2L)
          members <- lapply(1:3, function(g) list(
            target = exits$person_id[group == g & arm == "arm=target"],
            comparator = exits$person_id[group == g & arm == "arm=comparator"]))
          sub_c6 <- lapply(1:3, function(g) .dsomopReleaseBuildC6(
            p, route$protection, owner, sub[group == g, ]))
          fit <- .omopReleaseInteractionCoxCore(route, exits, c6, sub_c6,
                                                members, tar, outcome_key)
          estimates <- do.call(rbind, lapply(1:3, function(g) data.frame(
            level = paste0("group", g), log_estimate = fit$rr[[g]][["log_estimate"]],
            se = fit$rr[[g]][["se_log_estimate"]], lo = fit$rr[[g]][["ci_lo"]],
            hi = fit$rr[[g]][["ci_hi"]], oracle = oracle(group == g))))
        }
      }
      estimates$finite <- with(estimates, is.finite(log_estimate) &
        is.finite(se) & is.finite(lo) & is.finite(hi) & lo > 0)
      estimates$informative <- with(estimates, finite & log(hi / lo) <= log(4))
      estimates$covered <- with(estimates, finite & lo <= exp(beta) & hi >= exp(beta))
      result[[length(result) + 1L]] <- cbind(data.frame(
        scenario = j, model = scenario$model, n = n, replicate = r,
        seed = seed, sampler = sampler, epsilon = p$release_epsilon, truth_log = beta,
        shared_empty_event_atoms = shared_empty,
        horizon_days = scenario$horizon_days,
        baseline_event_days = scenario$baseline_event_days,
        target_censoring = target_censoring, comparator_censoring = comparator_censoring,
        periods_per_person = scenario$periods_per_person,
        eligible_persons = eligible_persons, excluded_strata_fraction = excluded_strata,
        max_events_per_person = max_events_person,
        survival_source_sha256 = source_sha256,
        margin_projection_calls = margin_diag$calls,
        margin_repairs_nonzero = margin_diag$nonzero,
        margin_repairs_above_1e_minus8_relative = margin_diag$material,
        margin_error_max_relative = margin_diag$max_relative,
        margin_initial_error_max_relative = margin_diag$initial_max_relative), estimates)
      cat("scenario", j, scenario$model, "n", n,
          "replicate", r, "of", replicates, "seconds",
          round(proc.time()[["elapsed"]] - started, 3), "\n")
    }
  }
  raw <- do.call(rbind, result)
  groups <- split(raw, interaction(raw$scenario, raw$level, drop = TRUE))
  summary <- do.call(rbind, lapply(groups, function(d) {
    ok <- d$finite
    error <- d$log_estimate[ok] - d$truth_log[ok]
    data.frame(scenario = d$scenario[[1]], model = d$model[[1]], sampler = sampler, n = d$n[[1]],
      level = d$level[[1]], replicates = nrow(d), finite = sum(ok),
      informative = sum(d$informative), finite_rate = mean(ok),
      informative_rate = mean(d$informative),
      coverage_given_finite = if (any(ok)) mean(d$covered[ok]) else NA_real_,
      coverage_all = mean(d$covered),
      bias_log = mean(error), rmse_log = sqrt(mean(error^2)),
      mean_ci_log_width = mean(log(d$hi[ok] / d$lo[ok])),
      oracle_bias_log = mean(d$oracle - d$truth_log, na.rm = TRUE),
      release_minus_oracle_bias_log = mean(d$log_estimate[ok] - d$oracle[ok]))
  }))
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  write.csv(raw, file.path(outdir, "p0r3-survival-raw.csv"), row.names = FALSE)
  write.csv(summary, file.path(outdir, "p0r3-survival-summary.csv"), row.names = FALSE)
  capture.output(sessionInfo(), file = file.path(outdir, "p0r3-survival-session.txt"))
  print(summary, row.names = FALSE)
}
law_sampler <- function(key, context, coordinate, epsilon, sensitivity) {
  seed <- .dsomopDpHmac(key, .dsomopDpCanonicalJson(list(
    context = context, coordinate = coordinate)))
  .dsomopBigInteger(.dsomopReleaseSimNoise(seed, 1L, epsilon, sensitivity))
}
main()
