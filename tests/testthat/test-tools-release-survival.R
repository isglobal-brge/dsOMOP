# dsOMOP 2.10.3 protected survival and SCCS releases (Astra review section 6):
# grouped-time Cox from the public exit-time x status basis, the q-gridded
# conditional SCCS IRR, and the standard-mode person-time block. Helpers in
# helper-tools-release.R.

test_that("the grouped likelihoods are sufficient: Cox (Breslow) and conditional SCCS", {
  skip_if_not_installed("survival")
  # Arm-only Cox on public grid times needs only the exit-time x status x arm
  # histogram (the review's sufficiency check, through the release builder's
  # own binning and fitter).
  fr <- data.frame(time = rep(c(7, 14, 30, 60, 90, 180), each = 12),
                   status = rep(c(rep(1, 5), rep(0, 7)), 6),
                   arm = rep(c(0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1), 6))
  ref <- survival::coxph(survival::Surv(time, status) ~ arm, data = fr,
                         ties = "breslow")
  grid <- .dsomopReleaseExitGrid()
  statuses <- .DSOMOP_RELEASE_EXIT_STATUSES
  cells <- function(a) {
    x <- fr[fr$arm == a, ]
    b <- .dsomopReleaseBinIndex(x$time, grid)
    m <- matrix(0, nrow = length(grid$breaks), ncol = 3,
                dimnames = list(NULL, statuses))
    for (i in seq_len(nrow(x))) {
      s <- if (x$status[[i]] == 1) "event" else "admin_censored"
      m[b[[i]], s] <- m[b[[i]], s] + 1
    }
    m
  }
  fit <- .dsomopReleaseGroupedCoxFit(.dsomopReleaseCoxRiskSets(cells(1),
                                                               cells(0)))
  expect_equal(fit[["beta"]], unname(stats::coef(ref)[1L]), tolerance = 1e-6)
  expect_equal(fit[["se_model"]], sqrt(diag(stats::vcov(ref)))[[1L]],
               tolerance = 1e-5)
  # SCCS: the conditional likelihood depends on q and the events per q only.
  q <- rep(c(.1, .25, .5, .75), each = 10)
  n <- rep(3:7, length.out = 40)
  exposed <- pmin(n, round(n * (2 * q / (1 + q))))
  cases <- data.frame(case_id = rep(1:40, 2), exposed = rep(c(1, 0), each = 40),
                      events = c(exposed, n - exposed),
                      person_time = c(100 * q, 100 * (1 - q)))
  ref_sccs <- .omopSccsIrrFit(cases)
  qs <- sort(unique(q))
  m <- vapply(qs, function(x) sum(n[q == x]), numeric(1L))
  sccs <- .dsomopReleaseSccsFit(qs, m, sum(exposed))
  expect_equal(sccs[["beta"]], ref_sccs$log_irr, tolerance = 1e-6)
  expect_equal(sccs[["se_model"]], ref_sccs$se_log_irr, tolerance = 1e-4)
  # Unidentifiable inputs give a stable NA, never a boundary value.
  expect_true(is.na(.dsomopReleaseGroupedCoxFit(.dsomopReleaseCoxRiskSets(
    cells(1) * 0, cells(0)))[["beta"]]))
  expect_true(is.na(.dsomopReleaseSccsFit(qs, m, 0)[["beta"]]))
})

test_that("the noise-law simulation is deterministic and never touches R's RNG", {
  set.seed(1)
  before <- .Random.seed
  a <- .dsomopReleaseSimNoise("seed-a", 1000L, 0.08, 1)
  expect_identical(.Random.seed, before)
  expect_identical(.dsomopReleaseSimNoise("seed-a", 1000L, 0.08, 1), a)
  expect_false(identical(.dsomopReleaseSimNoise("seed-b", 1000L, 0.08, 1), a))
  # The draws follow the public discrete-Laplace law.
  expect_equal(stats::sd(a), .dsomopReleaseNoiseSd(0.08, 1), tolerance = 0.15)
  expect_true(all(a == round(a)))
})

.survival_handle <- function(n = 400, rate_t = 0.5, rate_c = 0.25) {
  h <- create_test_handle(n_persons = n)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = seq_len(n), person_id = seq_len(n),
    observation_period_start_date = "2015-01-01",
    observation_period_end_date = "2024-12-31",
    period_type_concept_id = 44814724L), append = TRUE)
  DBI::dbExecute(h$conn,
                 "DELETE FROM condition_occurrence WHERE condition_concept_id = 4000002")
  half <- n / 2
  .release_dated_cohort(h, "surv_t", seq_len(half), start = "2020-01-01",
                        end = "2023-12-31")
  .release_dated_cohort(h, "surv_c", half + seq_len(half),
                        start = "2020-01-01", end = "2023-12-31")
  set.seed(7)
  for (p in seq_len(n)) {
    rate <- if (p <= half) rate_t else rate_c
    if (stats::runif(1) < rate) {
      day <- sample.int(360L, 1L)
      .release_add_condition(h, p, 4000002L,
                             format(as.Date("2020-01-01") + day))
    }
  }
  h
}

test_that("the protected route of a Cox request is a grouped-time hazard ratio", {
  .release_state()
  withr::local_options(list(dsomop.dp.release_epsilon = 8))
  .release_restart()
  h <- .survival_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  params <- list(outcome_concept_id = "4000002", model_type = "cox",
                 tar_start_offset = "0", tar_end_offset = "365",
                 tar_anchor_start = "start", tar_anchor_end = "start")
  cox <- .release_run(h, "dsomop:cm.effect_estimate", params,
                      scope = list("surv_t", "surv_c"))
  label <- attr(cox, "dsomop_protection")
  expect_identical(label$protection, "realised_sticky")
  expect_match(label$estimand, "grouped-time Cox")
  expect_true(all(is.finite(c(cox$estimate, cox$ci_lo, cox$ci_hi,
                              cox$se_log_estimate))))
  expect_identical(cox$estimate[[1L]], cox$estimate[[2L]])
  # A higher target hazard (twice the event rate) gives a hazard ratio > 1.
  expect_gt(cox$estimate[[1L]], 1)
  expect_true(cox$ci_lo[[1L]] < cox$estimate[[1L]] &&
                cox$estimate[[1L]] < cox$ci_hi[[1L]])
  # Persons, outcomes and person-days are the Poisson route's components.
  params$model_type <- "poisson"
  poisson <- .release_run(h, "dsomop:cm.effect_estimate", params,
                          scope = list("surv_t", "surv_c"))
  expect_identical(cox$persons, poisson$persons)
  expect_identical(cox$outcomes, poisson$outcomes)
  expect_identical(cox$person_days, poisson$person_days)
  owners <- attr(cox, "dsomop_release")$owners
  expect_length(owners, 2L)
  expect_identical(owners[[2L]]$family, "C8")
  # One atom per person: each arm's signed exit cells sum to its person total.
  for (comp in owners[[2L]]$components) {
    expect_equal(sum(comp$h), comp$n, tolerance = 1e-6)
  }
  # Replay: the same question returns the same answer.
  params$model_type <- "cox"
  expect_identical(.release_values(.release_run(
    h, "dsomop:cm.effect_estimate", params, scope = list("surv_t", "surv_c"))),
    .release_values(cox))
  # The SE is never the raw model's: no finite raw coxph scalar appears.
  expect_false("se_raw" %in% names(cox))
})

.sccs_handle <- function(n = 300, effect = 3) {
  h <- create_test_handle(n_persons = n)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = seq_len(n), person_id = seq_len(n),
    observation_period_start_date = "2020-01-01",
    observation_period_end_date = "2021-12-31",
    period_type_concept_id = 44814724L), append = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  DBI::dbExecute(h$conn, "DELETE FROM drug_exposure")
  set.seed(11)
  for (p in seq_len(n)) {
    ex <- as.Date("2020-03-01") + sample(0:300, 1)
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO drug_exposure (drug_exposure_id, person_id, ",
      "drug_concept_id, drug_exposure_start_date, drug_exposure_end_date, ",
      "drug_type_concept_id) VALUES (%d, %d, 1112807, '%s', '%s', ",
      "38000177)"), 5000L + p, p, format(ex), format(ex)))
    # Background events over the two years plus extra events in the window.
    k <- 1 + stats::rpois(1, 1)
    days <- sample(0:729, k)
    if (stats::runif(1) < 0.6) days <- c(days, as.numeric(ex - as.Date(
      "2020-01-01")) + sample(0:29, 1))
    for (d in days) {
      .release_add_condition(h, p, 4000002L,
                             format(as.Date("2020-01-01") + d))
    }
  }
  h
}

test_that("SCCS strata are (person, observation island) with one shared event cap", {
  h <- create_test_handle(n_persons = 10)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  DBI::dbExecute(h$conn, "DELETE FROM drug_exposure")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = 1:5, person_id = c(1L, 1L, 2L, 2L, 3L),
    observation_period_start_date = c("2015-01-01", "2018-01-01",
                                      "2015-01-01", "2016-01-01",
                                      "2015-01-01"),
    observation_period_end_date = c("2016-12-31", "2019-12-31",
                                    "2020-12-31", "2016-12-31",
                                    "2020-12-31"),
    period_type_concept_id = 44814724L), append = TRUE)
  exposure <- c("2018-06-01", "2017-01-01", "2017-01-01")
  for (p in 1:3) {
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO drug_exposure (drug_exposure_id, person_id, ",
      "drug_concept_id, drug_exposure_start_date, drug_exposure_end_date, ",
      "drug_type_concept_id) VALUES (%d, %d, 1112807, '%s', '%s', ",
      "38000177)"), 7000L + p, p, exposure[[p]], exposure[[p]]))
  }
  # Person 1: an event in an island without exposure (q = 0, dropped) and an
  # exposed and an unexposed event in the exposure's island.
  add <- function(p, dates) {
    for (d in dates) .release_add_condition(h, p, 4000002L, d)
  }
  add(1L, c("2016-05-01", "2018-06-10", "2019-03-01"))
  # Person 2: overlapping periods coalesce; the covered record counts once.
  add(2L, c("2016-06-01", "2017-01-15"))
  # Person 3: twelve events, only the first ten (by date) are kept.
  add(3L, format(as.Date("2016-01-01") + 30 * (0:11)))
  exp_src <- .omopOutcomeSource(h, "1112807", "1")
  out_src <- .omopOutcomeSource(h, "4000002", "0")
  cases <- .omopReleaseSccsCases(h, list(scoped_cohort = NULL), exp_src,
                                 out_src, 30L)
  cases <- cases[order(cases$person_id), ]
  expect_identical(cases$person_id, c("1", "2", "3"))
  # The closed 30-day window holds 31 days; islands are half-open.
  expect_equal(cases$q, c(31 / 730, 31 / 2192, 31 / 2192), tolerance = 1e-12)
  expect_equal(cases$n_exposed, c(1, 1, 0))
  expect_equal(cases$n_unexposed, c(1, 1, 10))
  expect_true(all(cases$n_exposed + cases$n_unexposed <=
                    .DSOMOP_RELEASE_SCCS_CAP))
})

test_that("the live SCCS IRR is a protected q-gridded conditional estimate", {
  .release_state()
  withr::local_options(list(dsomop.dp.release_epsilon = 8))
  .release_restart()
  h <- .sccs_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  params <- list(exposure_concept_id = "1112807",
                 outcome_concept_id = "4000002", window = "30")
  r <- .release_run(h, "dsomop:sccs.incidence_rate_ratio", params)
  label <- attr(r, "dsomop_protection")
  expect_identical(label$protection, "realised_sticky")
  expect_match(label$estimand, "q-gridded conditional SCCS")
  expect_identical(r$model_type, "q_gridded_conditional_poisson_capped_k10")
  expect_true(all(is.finite(c(r$irr, r$ci_lo, r$ci_hi, r$log_irr,
                              r$se_log_irr))))
  # The window events raise the exposed rate: IRR > 1.
  expect_gt(r$irr, 1)
  # Each person contributes at most K = 10 events, so the cells are a
  # weighted histogram with sensitivity K, not a person partition.
  comp <- label$components[[1L]]
  expect_identical(comp$sensitivity$W, 10L)
  expect_false("delta" %in% names(label))
  # Replay and window snapping: the same effective window, the same answer.
  expect_identical(.release_values(.release_run(
    h, "dsomop:sccs.incidence_rate_ratio", params)), .release_values(r))
  # No cases: the exact case support fails the floor before any release and
  # the generic disclosure error is all the analyst sees.
  expect_error(.release_run(h, "dsomop:sccs.incidence_rate_ratio",
                            utils::modifyList(params,
                                              list(exposure_concept_id = "1"))),
               "^Disclosive")
  # Standard mode: the same protected estimate (and its model) in a mixed
  # frame; the case and event counts keep their standard bands.
  withr::local_options(list(dsomop.tools.mode = "standard"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  std <- .release_run(h, "dsomop:sccs.incidence_rate_ratio", params)
  slab <- attr(std, "dsomop_protection")
  expect_identical(slab$protection, "mixed")
  expect_identical(slab$blocks$estimate, "realised_sticky")
  expect_identical(std$model_type, r$model_type)
  fields <- c("irr", "ci_lo", "ci_hi", "log_irr", "se_log_irr")
  expect_equal(std[, fields], r[, fields], tolerance = 1e-9)
  expect_true(all(std$n_cases %% 5 == 0))
})

test_that("standard mode takes the masked person-time block from the protected components", {
  .release_state()
  h <- .survival_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  scope <- list("surv_t", "surv_c")
  params <- list(outcome_concept_id = "4000002", tar_start_offset = "0",
                 tar_end_offset = "365", tar_anchor_start = "start",
                 tar_anchor_end = "start")
  auto_mdrr <- .release_run(h, "dsomop:cm.mdrr", params, scope = scope)
  auto_eff <- .release_run(h, "dsomop:cm.effect_estimate",
                           c(params, list(model_type = "poisson")),
                           scope = scope)
  auto_inc <- .release_run(h, "dsomop:incidence.rate", params,
                           scope = "surv_t")
  withr::local_options(list(dsomop.tools.mode = "standard"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  std_mdrr <- .release_run(h, "dsomop:cm.mdrr", params, scope = scope)
  label <- attr(std_mdrr, "dsomop_protection")
  expect_identical(label$protection, "mixed")
  expect_identical(label$mode, "standard")
  expect_identical(label$blocks$person_time, "realised_sticky")
  expect_equal(std_mdrr$person_days, auto_mdrr$person_days, tolerance = 1e-9)
  expect_equal(std_mdrr$mdrr, auto_mdrr$mdrr, tolerance = 1e-9)
  # Established counts keep their standard (banded) controls.
  expect_true(all(std_mdrr$persons %% 5 == 0))
  std_eff <- .release_run(h, "dsomop:cm.effect_estimate",
                          c(params, list(model_type = "poisson")),
                          scope = scope)
  expect_equal(std_eff$estimate, auto_eff$estimate, tolerance = 1e-9)
  expect_equal(std_eff$se_log_estimate, auto_eff$se_log_estimate,
               tolerance = 1e-9)
  std_inc <- .release_run(h, "dsomop:incidence.rate", params,
                          scope = "surv_t")
  expect_equal(std_inc$person_days, auto_inc$person_days, tolerance = 1e-9)
  expect_equal(std_inc$rate, auto_inc$rate, tolerance = 1e-9)
  diag <- .release_run(h, "dsomop:cm.diagnostics_summary", params,
                       scope = scope)
  expect_equal(diag$mdrr, auto_mdrr$mdrr[[1L]], tolerance = 1e-9)
  # Without the DP service the stable masking stands.
  withr::local_options(list(dsomop.dp.enabled = FALSE))
  .release_restart()
  off <- .release_run(h, "dsomop:cm.mdrr", params, scope = scope)
  expect_true(all(is.na(off$person_days)))
  expect_true(all(is.na(off$mdrr)))
  expect_identical(attr(off, "dsomop_protection")$masked_reason,
                   "protected_output_required")
})
