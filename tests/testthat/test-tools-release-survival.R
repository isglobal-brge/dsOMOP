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
  by_gender <- c(params, list(strat_by_gender = "1"))
  auto_gender <- .release_run(h, "dsomop:incidence.rate", by_gender,
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
  # Stratified rows match the protected strata by their public labels.
  std_gender <- .release_run(h, "dsomop:incidence.rate", by_gender,
                             scope = "surv_t")
  hit <- match(std_gender$stratum, auto_gender$stratum)
  expect_true(any(!is.na(hit)))
  expect_equal(std_gender$person_days[!is.na(hit)],
               auto_gender$person_days[hit[!is.na(hit)]], tolerance = 1e-9)
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

# --- Round 3: canonical exit basis, structural mask, family hierarchy --------

test_that("the canonical exit basis and its structural masks are public and stable", {
  g <- .dsomopReleaseExitGrid()
  expect_identical(g$id, "exit-days-clinical-v2")
  expect_identical(g$breaks, .dsomopReleaseDayGrid())
  coords <- .dsomopReleaseExitCoordinates()
  expect_length(coords, 3L * length(g$breaks))
  expect_false(any(grepl("max=", coords)))
  w <- function(start, end, a = "start", b = "start") {
    .dsomopReleaseExitMask(list(start = start, end = end, anchor_start = a,
                                anchor_end = b))
  }
  m <- w(0L, 365L)
  lower <- g$breaks
  # The release basis and its projection are independent of the window.
  expect_false(any(m[lower < 1, ]))
  expect_true(all(m[lower >= 1, ]))
  expect_identical(w(0L, 90L), w(0L, 120L))
  expect_identical(w(-30L, 365L), m)
  expect_identical(w(0L, 30L, "start", "end"), m)
  expect_identical(w(0L, 5000L), m)
  # The per-person normalization keeps every exit inside its mask.
  ex <- data.frame(time = c(0, 400, 200, 366, 366),
                   status = c("event", "event", "admin_censored", "censored",
                              "admin_censored"), stringsAsFactors = FALSE)
  nx <- .dsomopReleaseExitNormalize(ex, list(start = 0L, end = 365L,
                                             anchor_start = "start",
                                             anchor_end = "start"))
  expect_identical(nx$time, c(1, 366, 200, 366, 366))
  expect_identical(nx$status, c("event", "event", "censored",
                                "admin_censored", "admin_censored"))
})

test_that("two-way reconciliation and display keep both parents", {
  Y <- matrix(c(5, -2, 7, 1, 3, 0), nrow = 2)
  r <- c(10, 4); cc <- c(3, 6, 5)
  X <- .dsomopReleaseTwoWay(Y, r, cc)
  expect_equal(rowSums(X), r, tolerance = 1e-12)
  expect_equal(colSums(X), cc, tolerance = 1e-12)
  # The additive adjustment is the least-squares projection: Y - X is of the
  # form a_i + b_j.
  A <- Y - X
  expect_equal(A[1, ] - A[2, ], rep(A[1, 1] - A[2, 1], 3), tolerance = 1e-12)
  D <- .dsomopReleaseTwoWayDisplay(X, c(8, 3), c(2, 5, 4))
  expect_true(all(D >= 0))
  expect_equal(rowSums(D), c(8, 3), tolerance = 1e-9)
  expect_equal(colSums(D), c(2, 5, 4), tolerance = 1e-6)
  b <- .dsomopReleaseBoxSimplex(c(5, -1, 9), 7, c(4, 4, 4))
  expect_equal(sum(b), 7, tolerance = 1e-12)
  expect_true(all(b >= 0 & b <= 4))
  expect_equal(.dsomopReleaseBoxSimplex(c(1, 2), 10, c(3, 3)), c(3, 3))
})

.family_handle <- function(n = 400, seed = 3) {
  h <- create_test_handle(n_persons = n)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = seq_len(n), person_id = seq_len(n),
    observation_period_start_date = "2015-01-01",
    observation_period_end_date = c(rep("2024-12-31", n - 40),
                                    rep("2020-04-15", 40)),
    period_type_concept_id = 44814724L), append = TRUE)
  # Some persons of unknown gender complete the sex family.
  DBI::dbExecute(h$conn, paste0("UPDATE person SET gender_concept_id = 0 ",
                                "WHERE person_id % 9 = 0"))
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  half <- n / 2
  .release_dated_cohort(h, "fam_t", seq_len(half), "2020-01-01", "2023-12-31")
  .release_dated_cohort(h, "fam_c", half + seq_len(half), "2020-01-01",
                        "2023-12-31")
  set.seed(seed)
  for (p in seq_len(n)) {
    rate <- if (p <= half) 0.45 else 0.25
    if (stats::runif(1) < rate) {
      .release_add_condition(h, p, 4000002L, format(
        as.Date("2020-01-01") + sample.int(300L, 1L)))
    }
  }
  # A covariate for concept-defined subgroups.
  .release_add_condition(h, seq(1, n, by = 3), 317009L, "2019-06-01")
  .buildBlueprint(h, force = TRUE)
  h
}

.family_params <- function(model = "cox", kind = "sex", ...) {
  utils::modifyList(list(outcome_concept_id = "4000002", model_type = model,
                         tar_start_offset = "0", tar_end_offset = "365",
                         tar_anchor_start = "start", tar_anchor_end = "start",
                         subgroup_kind = kind), list(...))
}

test_that("interaction families hang under their arms: signed and displayed margins", {
  .release_state()
  h <- .family_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  scope <- list("fam_t", "fam_c")
  for (model in c("poisson", "cox")) {
    overall <- .release_run(h, "dsomop:cm.interaction_estimate",
                            .family_params(model, "overall"), scope)
    sex <- .release_run(h, "dsomop:cm.interaction_estimate",
                        .family_params(model, "sex"), scope)
    expect_identical(nrow(sex), 3L)
    for (col in c("target_persons", "comparator_persons", "target_outcomes",
                  "comparator_outcomes", "target_person_days",
                  "comparator_person_days")) {
      expect_equal(sum(sex[[col]]), overall[[col]][[1L]], tolerance = 1e-6,
                   info = paste(model, col))
    }
    ow <- attr(sex, "dsomop_release")$owners
    arm <- ow[[1L]]
    subs <- ow[2:4]
    for (j in 1:2) {
      a <- arm$components[[j]]
      s <- lapply(subs, function(o) o$components[[j]])
      expect_equal(sum(vapply(s, `[[`, numeric(1L), "n")), a$n,
                   tolerance = 1e-9)
      expect_equal(Reduce(`+`, lapply(s, `[[`, "status")), a$status,
                   tolerance = 1e-9)
      expect_equal(sum(vapply(s, `[[`, numeric(1L), "time")), a$time,
                   tolerance = 1e-6)
      for (x in s) expect_equal(sum(x$status), x$n, tolerance = 1e-9)
    }
    if (identical(model, "cox")) {
      c8_arm <- ow[[5L]]
      c8_subs <- ow[6:8]
      expect_identical(c8_arm$family, "C8")
      event <- c8_arm$status_of_cell == "event"
      for (j in 1:2) {
        cell_sum <- Reduce(`+`, lapply(c8_subs, function(o) {
          o$components[[j]]$h
        }))
        expect_equal(cell_sum, c8_arm$components[[j]]$h, tolerance = 1e-9)
        for (i in 1:3) {
          comp <- c8_subs[[i]]$components[[j]]
          expect_equal(sum(comp$h[event]), comp$status[[1L]],
                       tolerance = 1e-9)
          expect_equal(sum(comp$h[!event]), comp$status[[2L]],
                       tolerance = 1e-9)
          # The subgroup's C8 status parent is its reconciled C6 status.
          expect_equal(comp$status, subs[[i]]$components[[j]]$status,
                       tolerance = 1e-9)
        }
      }
    }
  }
})

test_that("interaction Cox subgroups equal coxph (Breslow) on public bins without noise", {
  skip_if_not_installed("survival")
  .release_state()
  # Zero release noise; a high calibration keeps the uncertainty simulation
  # (which perturbs the displayed values) informative at this small size.
  withr::local_options(list(dsomop.dp.release_epsilon = 8))
  .release_restart()
  h <- .family_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  scope <- list("fam_t", "fam_c")
  # The DP runtime (and its sampler self-test) starts before the noise is
  # replaced by zeros for this sufficiency check.
  expect_true(suppressMessages(.dsomopToolsDpReady(h))$ready)
  local_mocked_bindings(.dsomopDpDiscreteLaplace = function(...) {
    .dsomopBigInteger(0L)
  })
  out <- .release_run(h, "dsomop:cm.interaction_estimate",
                      .family_params("cox", "concept",
                                     subgroup_concept_id = "317009",
                                     subgroup_domain_code = "0"), scope)
  expect_identical(nrow(out), 2L)
  tar <- list(start = 0L, end = 365L, anchor_start = "start",
              anchor_end = "start")
  src <- .omopOutcomeSource(h, "4000002", "0")
  grid <- .dsomopReleaseExitGrid()
  arm_data <- function(cohort, arm) {
    d <- .omopCmEffectArmData(h, cohort, arm, src, tar, horizon = 3650)$data
    d$bin <- .dsomopReleaseBinIndex(d$tar_days, grid)
    d
  }
  d <- rbind(arm_data("fam_t", 1L), arm_data("fam_c", 0L))
  has <- DBI::dbGetQuery(h$conn, paste0(
    "SELECT DISTINCT person_id FROM condition_occurrence WHERE ",
    "condition_concept_id = 317009"))$person_id
  d$yes <- d$person_id %in% as.character(has)
  for (i in 1:2) {
    sub <- d[d$yes == (i == 1L), ]
    ref <- survival::coxph(survival::Surv(bin, event) ~ arm, data = sub,
                           ties = "breslow")
    expect_equal(out$log_estimate[[i]], unname(stats::coef(ref)[[1L]]),
                 tolerance = 1e-6, info = out$subgroup_label[[i]])
  }
})

test_that("interaction Cox reuses components across labels and keeps windows' atoms", {
  .release_state()
  h <- .family_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  scope <- list("fam_t", "fam_c")
  # Another concept selecting exactly the same people: a label alias.
  .release_add_condition(h, seq(1, 400, by = 3), 201820L, "2019-06-01")
  .buildBlueprint(h, force = TRUE)
  run <- function(concept, end = "365") {
    .release_run(h, "dsomop:cm.interaction_estimate",
                 .family_params("cox", "concept",
                                subgroup_concept_id = concept,
                                subgroup_domain_code = "0",
                                tar_end_offset = end), scope)
  }
  a <- run("317009"); b <- run("201820")
  for (f in c("estimate", "se_log_estimate", "target_persons",
              "target_outcomes")) {
    expect_identical(a[[f]], b[[f]], info = f)
  }
  expect_identical(attr(a, "dsomop_release")$owners,
                   attr(b, "dsomop_release")$owners)
})

test_that("near-zero subgroup events give a stable unavailable Cox estimate", {
  .release_state()
  h <- .family_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  # No outcomes at all among persons with the covariate.
  DBI::dbExecute(h$conn, paste0(
    "DELETE FROM condition_occurrence WHERE condition_concept_id = 4000002 ",
    "AND person_id IN (SELECT person_id FROM condition_occurrence WHERE ",
    "condition_concept_id = 317009)"))
  out <- .release_run(h, "dsomop:cm.interaction_estimate",
                      .family_params("cox", "concept",
                                     subgroup_concept_id = "317009",
                                     subgroup_domain_code = "0"),
                      list("fam_t", "fam_c"))
  if (nrow(out)) {
    expect_identical(nrow(out), 2L)
    label <- attr(out, "dsomop_protection")
    yes <- grepl(": yes$", out$subgroup_label)
    if (!is.finite(out$estimate[yes])) {
      expect_match(label$unavailable_reason, "unavailable for")
    }
    expect_true(all(c("estimate", "ci_lo", "ci_hi") %in% names(out)))
  }
})

test_that("the ordinary protected Cox equals coxph (Breslow) on public bins without noise; the horizon caps exits", {
  skip_if_not_installed("survival")
  .release_state()
  withr::local_options(list(dsomop.dp.release_epsilon = 8))
  .release_restart()
  h <- .family_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  # Observation long beyond the horizon for half of the persons.
  DBI::dbExecute(h$conn, paste0("UPDATE observation_period SET ",
                                "observation_period_end_date = '2035-12-31' ",
                                "WHERE person_id % 2 = 0"))
  expect_true(suppressMessages(.dsomopToolsDpReady(h))$ready)
  local_mocked_bindings(.dsomopDpDiscreteLaplace = function(...) {
    .dsomopBigInteger(0L)
  })
  params <- list(outcome_concept_id = "4000002", model_type = "cox",
                 tar_start_offset = "0", tar_end_offset = "5000",
                 tar_anchor_start = "start", tar_anchor_end = "start")
  out <- .release_run(h, "dsomop:cm.effect_estimate", params,
                      list("fam_t", "fam_c"))
  tar <- list(start = 0L, end = 5000L, anchor_start = "start",
              anchor_end = "start")
  src <- .omopOutcomeSource(h, "4000002", "0")
  grid <- .dsomopReleaseExitGrid()
  d <- rbind(
    .omopCmEffectArmData(h, "fam_t", 1L, src, tar, horizon = 3650)$data,
    .omopCmEffectArmData(h, "fam_c", 0L, src, tar, horizon = 3650)$data)
  expect_lte(max(d$tar_days), 3650)
  expect_true(any(d$tar_days == 3650))
  d$bin <- .dsomopReleaseBinIndex(d$tar_days, grid)
  ref <- survival::coxph(survival::Surv(bin, event) ~ arm, data = d,
                         ties = "breslow")
  expect_equal(out$log_estimate[[1L]], unname(stats::coef(ref)[[1L]]),
               tolerance = 1e-6)
  # Administrative exits at the horizon are in the last canonical interval.
  owners <- attr(out, "dsomop_release")$owners
  expect_true("stratum=root|cell=exit-days-clinical-v2:[3650, Inf)|status=admin_censored" %in%
                owners[[2L]]$cell_keys)
})

test_that("large protected simulation states use a bounded public stream seed", {
  seed <- .dsomopReleaseSimSeed("full-C8-family", seq_len(10000L))
  expect_identical(nchar(seed), 64L)
  expect_match(seed, "^[0-9a-f]{64}$")
  expect_false(identical(seed, .dsomopReleaseSimSeed(
    "full-C8-family", c(seq_len(9999L), -1L))))
  set.seed(34)
  before <- .Random.seed
  expect_identical(.dsomopReleaseSimUniform(seed, 19L)[seq_len(11L)],
                   .dsomopReleaseSimUniform(seed, 11L))
  expect_identical(.Random.seed, before)
})
