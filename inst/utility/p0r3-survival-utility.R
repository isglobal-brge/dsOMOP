#!/usr/bin/env Rscript
# Run from the dsOMOP source root on the R test pod:
# Rscript inst/utility/p0r3-survival-utility.R [replicates=100] [output-directory=inst/utility/results]
# Uses the production crypto, complete-basis projection and 200-refit SE.
# Seed 2026111200 + scenario*10000 + replicate determines synthetic sampling;
# each replicate has an independent synthetic corpus identity (same fixed
# test root), so its actual sticky noise includes equal-map correlations.
# Informative = finite 95% CI whose upper/lower ratio is <= 4. Coverage is
# reported both conditional on finite intervals and over all replicates.
# Bias/RMSE use the log-HR/log-IRR scale, conditional on finite estimates.
args <- commandArgs(TRUE)
replicates <- if (length(args)) as.integer(args[[1L]]) else 100L
outdir <- if (length(args) >= 2L) args[[2L]] else "inst/utility/results"
stopifnot(is.finite(replicates), replicates > 0)
pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
source("tests/testthat/helper-db.R")
source("tests/testthat/helper-tools-release.R")

main <- function() {
  .release_state()
  h <- create_test_handle(n_persons = 5)
  on.exit(cleanup_handle(h), add = TRUE)
  policy <- suppressMessages(.dsomopToolsDpReady(h))$policy
  stopifnot(identical(policy$release_epsilon, 0.1))
  scenarios <- data.frame(
    model = c(rep("cox", 3), rep("interaction_cox", 2), rep("sccs", 3)),
    n = c(500L, 2000L, 10000L, 2000L, 10000L, 500L, 2000L, 10000L),
    q_min = c(rep(NA_real_, 5), 0.001, 0.02, 0.02),
    q_max = c(rep(NA_real_, 5), 0.02, 0.5, 0.5),
    stringsAsFactors = FALSE)
  tar <- list(start = 0L, end = 365L, anchor_start = "start", anchor_end = "start")
  outcome_key <- "utility-first-outcome/v1"
  owner <- c(list(id = "utility-arms/v1", population = "c6/persons-at-risk/v1",
                  strata = .omopReleaseArmStrata(), cap = 1L,
                  estimand = "synthetic first-outcome arms"),
             .omopReleaseC6Owners(outcome_key, "to_first"))
  result <- list()
  for (j in seq_len(nrow(scenarios))) {
    scenario <- scenarios[j, ]
    for (r in seq_len(replicates)) {
      seed <- 2026111200L + j * 10000L + r
      set.seed(seed)
      p <- policy
      p$domain <- paste0("p0r3-utility-synthetic-corpus-", seed)
      route <- list(policy = p, protection = "realised_sticky", mode = "auto")
      beta <- log(1.5)
      n <- scenario$n
      shared_empty <- NA_real_
      if (identical(scenario$model, "sccs")) {
        q <- plogis(runif(n, qlogis(scenario$q_min), qlogis(scenario$q_max)))
        m <- pmin(10, 1 + rpois(n, 3))
        exposed <- rbinom(n, m, plogis(qlogis(q) + beta))
        cases <- data.frame(person_id = as.character(seq_len(n)),
                            stratum = "island=1", q = q,
                            n_exposed = exposed, n_unexposed = m - exposed)
        fit <- .omopReleaseSccsCore(route, cases, "utility-sccs/v1")
        oracle <- .dsomopReleaseSccsFit(.dsomopReleaseSccsQ(q), m,
                                       sum(exposed))[["beta"]]
        estimates <- data.frame(level = "all", log_estimate = fit$df$log_irr,
          se = fit$df$se_log_irr, lo = fit$df$ci_lo, hi = fit$df$ci_hi,
          oracle = oracle)
      } else {
        arm <- rep(c("arm=target", "arm=comparator"), each = n)
        duration <- rexp(2L * n, rate = ifelse(arm == "arm=target", 1.5, 1) / 730)
        event <- as.integer(duration < 366)
        exit <- pmin(366, pmax(1, floor(duration) + 1))
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
        seed = seed, epsilon = p$release_epsilon, truth_log = beta,
        shared_empty_event_atoms = shared_empty), estimates)
      cat("scenario", j, scenario$model, "n", n,
          "replicate", r, "of", replicates, "\n")
    }
  }
  raw <- do.call(rbind, result)
  groups <- split(raw, interaction(raw$scenario, raw$level, drop = TRUE))
  summary <- do.call(rbind, lapply(groups, function(d) {
    ok <- d$finite
    error <- d$log_estimate[ok] - d$truth_log[ok]
    data.frame(scenario = d$scenario[[1]], model = d$model[[1]], n = d$n[[1]],
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
main()
