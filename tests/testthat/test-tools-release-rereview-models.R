# Astra's second-round model probes (astra-scratch/rereview-models,
# test-astra-rereview-models.R) against the round-3 fixes: R06 (one
# canonical exit/status/arm basis), the C8 part of B03 (exit cells under the
# C6 status parents), interaction Cox (R07) and the SCCS q-grid claims
# (deviation 16). Defect witnesses are inverted to assert the fixed
# behaviour; spec assertions are unchanged. Default calibration (0.1) unless
# a test says otherwise. Helpers in helper-tools-release.R.

.astra_same_exit_fixture <- function() {
  h <- create_test_handle(n_persons = 200)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = 1:200, person_id = 1:200,
    observation_period_start_date = "2019-01-01",
    observation_period_end_date = "2020-03-01",
    period_type_concept_id = 44814724L), append = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  .release_dated_cohort(h, "astra_cox_t", 1:100, "2020-01-01", "2023-12-31")
  .release_dated_cohort(h, "astra_cox_c", 101:200, "2020-01-01", "2023-12-31")
  .release_add_condition(h, 1:40, 4000002L, "2020-01-10")
  .release_add_condition(h, 101:130, 4000002L, "2020-02-10")
  h
}

# Signed exported C8 cells of one arm, named by coordinate.
.astra_c8_cells <- function(owner, arm = "arm=target") {
  comp <- Filter(function(c) identical(c$stratum, arm), owner$components)[[1L]]
  stats::setNames(comp$h, owner$cell_keys)
}

test_that("R06: the same Cox exit map reuses the same atoms whatever the requested end", {
  .release_state()
  h <- .astra_same_exit_fixture()
  on.exit(cleanup_handle(h), add = TRUE)
  pars <- list(outcome_concept_id = "4000002", model_type = "cox",
               tar_start_offset = "0", tar_end_offset = "90",
               tar_anchor_start = "start", tar_anchor_end = "start")
  scope <- list("astra_cox_t", "astra_cox_c")
  a <- .release_run(h, "dsomop:cm.effect_estimate", pars, scope)
  pars$tar_end_offset <- "120"
  b <- .release_run(h, "dsomop:cm.effect_estimate", pars, scope)
  ca <- attr(a, "dsomop_release")$owners
  cb <- attr(b, "dsomop_release")$owners
  # C6 proves identical eligible membership, status, and first-exit maps.
  expect_identical(ca[[1]]$components, cb[[1]]$components)
  expect_identical(a$persons, b$persons)
  expect_identical(a$outcomes, b$outcomes)
  expect_identical(ca[[2]]$owner, cb[[2]]$owner)
  # One canonical basis: the grid id and every shared interval identity are
  # independent of the requested end (formerly max=91 versus max=121).
  expect_identical(ca[[2]]$grid, "exit-days-clinical-v2")
  expect_identical(cb[[2]]$grid, ca[[2]]$grid)
  shared <- intersect(ca[[2]]$cell_keys, cb[[2]]$cell_keys)
  expect_gte(length(shared), 16L)
  expect_false(any(grepl("max=", c(ca[[2]]$cell_keys, cb[[2]]$cell_keys))))
  # Both signed cells and final fits replay exactly: a request-dependent
  # mask/projection would create another estimate despite stable raw draws.
  expect_identical(ca[[2]]$components, cb[[2]]$components)
  for (f in c("estimate", "ci_lo", "ci_hi", "log_estimate",
              "se_log_estimate")) expect_identical(a[[f]], b[[f]], info = f)

})

test_that("B03 (C8): exit cells keep the already released C6 status parents", {
  .release_state()
  h <- .astra_same_exit_fixture()
  on.exit(cleanup_handle(h), add = TRUE)
  pars <- list(outcome_concept_id = "4000002", model_type = "cox",
               tar_start_offset = "0", tar_end_offset = "90",
               tar_anchor_start = "start", tar_anchor_end = "start")
  a <- .release_run(h, "dsomop:cm.effect_estimate", pars,
                    list("astra_cox_t", "astra_cox_c"))
  ow <- attr(a, "dsomop_release")$owners
  c6 <- ow[[1L]]; c8 <- ow[[2L]]
  for (arm in c("arm=target", "arm=comparator")) {
    p <- Filter(function(c) identical(c$stratum, arm), c6$components)[[1L]]
    x <- .astra_c8_cells(c8, arm)
    event <- grepl("status=event$", names(x))
    # The review's probe: the signed event children sum to the C6 signed event
    # parent (formerly 81.5 versus 66.33) and the censored plus administrative
    # cells to the no-event parent; together they give the person total.
    expect_equal(sum(x[event]), p$status[[1L]], tolerance = 1e-9, info = arm)
    expect_equal(sum(x[!event]), p$status[[2L]], tolerance = 1e-9, info = arm)
    expect_equal(sum(x), p$n, tolerance = 1e-9, info = arm)
  }
  # The fit's displayed cells sum to the displayed persons and outcomes.
  built <- .omopReleaseGroupedCoxCore(
    list(policy = suppressMessages(.dsomopToolsDpReady(h))$policy,
         protection = "realised_sticky"),
    rbind(.omopReleaseCoxExits(h, "astra_cox_t", "arm=target",
                               .omopOutcomeSource(h, "4000002", "0"),
                               list(start = 0L, end = 90L,
                                    anchor_start = "start",
                                    anchor_end = "start"), 3650L),
          .omopReleaseCoxExits(h, "astra_cox_c", "arm=comparator",
                               .omopOutcomeSource(h, "4000002", "0"),
                               list(start = 0L, end = 90L,
                                    anchor_start = "start",
                                    anchor_end = "start"), 3650L)),
    .dsomopReleaseBuildC6(
      suppressMessages(.dsomopToolsDpReady(h))$policy, "realised_sticky",
      c(list(id = "cohortmethod/arms-time-to-first-outcome/v2",
             population = "c6/persons-at-risk/v1",
             strata = .omopReleaseArmStrata(), cap = 1L, estimand = "x"),
        .omopReleaseC6Owners(.omopReleaseOutcomeKey("4000002", "0"),
                             "to_first")),
      rbind(
        transform(.omopCmEffectArmData(h, "astra_cox_t", 1L,
                                       .omopOutcomeSource(h, "4000002", "0"),
                                       list(start = 0L, end = 90L,
                                            anchor_start = "start",
                                            anchor_end = "start"),
                                       horizon = 3650L)$data,
                  stratum = "arm=target", time = tar_days),
        transform(.omopCmEffectArmData(h, "astra_cox_c", 0L,
                                       .omopOutcomeSource(h, "4000002", "0"),
                                       list(start = 0L, end = 90L,
                                            anchor_start = "start",
                                            anchor_end = "start"),
                                       horizon = 3650L)$data,
                  stratum = "arm=comparator", time = tar_days))),
    list(start = 0L, end = 90L, anchor_start = "start", anchor_end = "start"),
    .omopReleaseOutcomeKey("4000002", "0"))
  cells_t <- built$cells[["arm=target"]]
  expect_equal(sum(cells_t), a$persons[[1L]], tolerance = 1e-6)
  expect_equal(sum(cells_t[, "event"]), a$outcomes[[1L]], tolerance = 1e-6)
  expect_equal(built$rr[["estimate"]], a$estimate[[1L]], tolerance = 1e-9)
})

test_that("R07: the overall interaction subgroup is the ordinary protected Cox", {
  .release_state()
  h <- .astra_same_exit_fixture()
  on.exit(cleanup_handle(h), add = TRUE)
  pars <- list(outcome_concept_id = "4000002", model_type = "cox",
               tar_start_offset = "0", tar_end_offset = "90",
               tar_anchor_start = "start", tar_anchor_end = "start")
  scope <- list("astra_cox_t", "astra_cox_c")
  a <- .release_run(h, "dsomop:cm.effect_estimate", pars, scope)
  b <- .release_run(h, "dsomop:cm.interaction_estimate",
                    c(pars, list(subgroup_kind = "overall")), scope)
  expect_true(all(is.finite(a$estimate)))
  expect_equal(nrow(b), 1L)
  # Formerly NA (feature-loss witness); now the same atoms, parents, fit and
  # uncertainty as the ordinary route.
  for (f in c("estimate", "ci_lo", "ci_hi", "log_estimate",
              "se_log_estimate")) {
    expect_identical(b[[f]][[1L]], a[[f]][[1L]], info = f)
  }
  expect_identical(b$target_persons, a$persons[[1L]])
  expect_identical(b$target_outcomes, a$outcomes[[1L]])
})

test_that("Astra B10 boundary probe: exact closed days share protected horizon for incidence and effects", {
  .release_state()
  h <- create_test_handle(n_persons = 20)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = 1:20, person_id = 1:20,
    observation_period_start_date = "1990-01-01",
    observation_period_end_date = "2030-01-01",
    period_type_concept_id = 44814724L), append = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  .release_dated_cohort(h, "astra_horizon", 1:20, "2000-01-01", "2020-01-01")
  for (p in 1:6) .release_add_condition(h, p, 4000002L,
    format(as.Date("2000-01-01") + c(0, 1, 3284, 3285, 3649, 3650)[[p]]))
  src <- .omopOutcomeSource(h, "4000002", "0")
  check <- function(start, end, anchor_end, expected, max_days) {
    tar <- list(start = start, end = end, anchor_start = "start",
                anchor_end = anchor_end)
    f <- .omopCmEffectArmData(h, "astra_horizon", 1L, src, tar,
                              horizon = 3650)$data
    g <- .omopCmSubgroupSubjectFrame(h, "astra_horizon", src, tar, NULL,
                                     horizon = 3650)
    sql <- .omopPersonOutcomeSubstrateSql(h, "astra_horizon", src, start,
                                          end, "start", anchor_end,
                                          horizon = 3650)
    inc <- .omopReleaseFetch(h, paste0("SELECT * FROM ", sql, " x"))
    expect_equal(sum(f$event), expected)
    expect_equal(sum(g$events > 0), expected)
    expect_equal(sum(inc$person_outcomes), expected)
    expect_lte(max(f$tar_days), max_days)
    expect_lte(max(g$person_days), max_days)
    expect_lte(max(inc$person_days), max_days)
  }
  check(0L, 0L, "end", 5, 3650)
  check(-365L, 0L, "end", 3, 3650)
  check(0L, 0L, "start", 1, 1)
})

test_that("Deviation 16: the q-grid shift is bounded only inside the grid", {
  g <- .dsomopReleaseSccsQGrid()
  # Astra's witness: just below an odds midpoint the exact conditional
  # expected counts (no sampling or release noise) give the shift
  # logit(q) - logit(q_grid), about +0.20.
  q <- plogis((qlogis(g[[20]]) + qlogis(g[[21]])) / 2 - 1e-7)
  snapped <- .dsomopReleaseSccsQ(q)
  beta <- log(2)
  M <- 1e6
  E <- M * plogis(qlogis(q) + beta)
  fit <- .dsomopReleaseSccsFit(snapped, M, E)
  expect_equal(fit[["beta"]], beta + qlogis(q) - qlogis(snapped),
               tolerance = 1e-8)
  expect_gt(abs(fit[["beta"]] - beta), .19)
  # That is the worst case inside the grid: the shift never exceeds half the
  # largest logit spacing of the public grid (about 0.20).
  half <- max(diff(qlogis(g))) / 2
  expect_lt(half, 0.21)
  probe <- plogis(seq(qlogis(min(g)), qlogis(max(g)), length.out = 5001L))
  expect_true(all(abs(qlogis(probe) - qlogis(.dsomopReleaseSccsQ(probe))) <=
                    half + 1e-9))
  # q = .999 is retained and clamped to .99 (+2.31 log-IRR), beyond
  # the approximation bound for q inside the grid range.
  q2 <- .999
  b2 <- .dsomopReleaseSccsFit(.dsomopReleaseSccsQ(q2), M,
                              M * plogis(qlogis(q2) + beta))[["beta"]]
  expect_gt(b2 - beta, 2.3)
})

test_that("Deviation 16: all informative OP strata survive endpoint grid clipping", {
  h <- create_test_handle(n_persons = 10)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  DBI::dbExecute(h$conn, "DELETE FROM drug_exposure")
  # Person 1: a 31-day exposed window over a 31-day island (q = 1, excluded);
  # person 2: the window over a 32-day island (q = 31/32 = 0.969, kept);
  # person 3: the window over a 3100-day island (q = 0.01, kept).
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = 1:5, person_id = 1:5,
    observation_period_start_date = c("2020-01-01", "2020-01-01",
                                      "2015-01-01", "2020-01-01", "2020-01-01"),
    observation_period_end_date = c("2020-01-31", "2020-02-01",
                                    as.character(as.Date("2015-01-01") + 3099),
                                    as.character(as.Date("2020-01-01") + 399999),
                                    as.character(as.Date("2020-01-01") + 999)),
    period_type_concept_id = 44814724L), append = TRUE)
  for (p in 1:5) {
    ex <- if (p == 3) "2016-01-01" else "2020-01-01"
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO drug_exposure (drug_exposure_id, person_id, ",
      "drug_concept_id, drug_exposure_start_date, drug_exposure_end_date, ",
      "drug_type_concept_id) VALUES (%d, %d, 1112807, '%s', '%s', ",
      "38000177)"), 8000L + p, p, ex, ex))
  }
  .release_add_condition(h, 1L, 4000002L, "2020-01-10")
  .release_add_condition(h, 2L, 4000002L, "2020-01-10")
  .release_add_condition(h, 3L, 4000002L, "2016-01-10")
  .release_add_condition(h, 4:5, 4000002L, "2020-01-10")
  cases <- .omopReleaseSccsCases(h, list(scoped_cohort = NULL),
                                 .omopOutcomeSource(h, "1112807", "1"),
                                 .omopOutcomeSource(h, "4000002", "0"), 30L)
  expect_setequal(cases$person_id, c("2", "3", "4", "5"))
  expect_lt(cases$q[cases$person_id == "4"], min(.dsomopReleaseSccsQGrid()))
  high <- .omopReleaseSccsCases(h, list(scoped_cohort = NULL),
                                .omopOutcomeSource(h, "1112807", "1"),
                                .omopOutcomeSource(h, "4000002", "0"), 998L)
  expect_equal(high$q[high$person_id == "5"], 0.999)
  expect_equal(.dsomopReleaseSccsQ(high$q[high$person_id == "5"]), 0.99)
})

test_that("Deviation 16: the estimating-equation root selection is documented and stable", {
  q <- c(0.1, 0.5, 0.9)
  # Non-negative cells: one decreasing root (the conditional MLE).
  ok <- .dsomopReleaseSccsFit(q, c(100, 100, 100), 150)
  expect_true(is.finite(ok[["beta"]]))
  # No sign change: no root.
  none <- .dsomopReleaseSccsFit(q, c(100, 100, 100), 400)
  expect_identical(attr(none, "reason"), "no_root")
  expect_identical(attr(.dsomopReleaseSccsFit(q, c(10, 10, 10), -5),
                        "reason"), "no_root")
  # Signed cells can make the score non-monotone: here it falls through zero
  # near b = -4.6, rises through zero near 0 and falls again near 9.2; two
  # admissible roots are refused rather than one being picked.
  multi <- .dsomopReleaseSccsFit(c(1e-4, 0.5, 0.99), c(100, -100, 100), 50)
  expect_identical(attr(multi, "reason"), "multiple_roots")
  expect_true(is.na(multi[["beta"]]))
  # Only an increasing crossing: non-positive information.
  inc <- .dsomopReleaseSccsFit(c(0.2, 0.8), c(-100, -100), -100)
  expect_identical(attr(inc, "reason"), "nonpositive_information")
  # Unidentifiable inputs.
  expect_identical(attr(.dsomopReleaseSccsFit(numeric(0), numeric(0), 1),
                        "reason"), "not_identifiable")
})

test_that("R06/R07: equal maps under another window, label or alias reuse the same atoms", {
  .release_state()
  h <- .astra_same_exit_fixture()
  on.exit(cleanup_handle(h), add = TRUE)
  scope <- list("astra_cox_t", "astra_cox_c")
  pars <- list(outcome_concept_id = "4000002", model_type = "cox",
               tar_start_offset = "0", tar_end_offset = "90",
               tar_anchor_start = "start", tar_anchor_end = "start")
  a <- .release_run(h, "dsomop:cm.effect_estimate", pars, scope)
  # The same outcome definition spelled with its explicit default domain.
  spelled <- .release_run(h, "dsomop:cm.effect_estimate",
                          utils::modifyList(pars, list(
                            outcome_domain_code = "0")), scope)
  expect_identical(.release_values(spelled), .release_values(a))
  # The interaction family under two ends: every exit precedes both, so each
  # subgroup's shared cells keep their within-group contrasts.
  fam <- function(entry, end) {
    .release_run(h, entry, utils::modifyList(pars, list(
      tar_end_offset = end, subgroup_kind = "sex")), scope)
  }
  x90 <- fam("dsomop:cm.interaction_estimate", "90")
  x120 <- fam("dsomop:cm.interaction_estimate", "120")
  o90 <- attr(x90, "dsomop_release")$owners
  o120 <- attr(x120, "dsomop_release")$owners
  c8_90 <- Filter(function(o) identical(o$family, "C8"), o90)
  c8_120 <- Filter(function(o) identical(o$family, "C8"), o120)
  expect_identical(length(c8_90), 4L)
  for (i in seq_along(c8_90)) {
    shared <- intersect(c8_90[[i]]$cell_keys, c8_120[[i]]$cell_keys)
    for (arm in c("arm=target", "arm=comparator")) {
      xa <- .astra_c8_cells(c8_90[[i]], arm)[shared]
      xb <- .astra_c8_cells(c8_120[[i]], arm)[shared]
      for (st in c("event", "censored")) {
        j <- grep(paste0("status=", st, "$"), shared)
        expect_equal(diff(xa[j]), diff(xb[j]), tolerance = 1e-9,
                     info = paste(i, arm, st))
      }
    }
  }
  expect_identical(c8_90, c8_120)
  expect_identical(x90$estimate, x120$estimate)
  # The OHDSI alias of the interaction entry is the same release.
  alias <- fam("dsomop:ohdsi.cohort_method.cm_interaction_result", "90")
  if (nrow(alias)) {
    expect_identical(attr(alias, "dsomop_release")$owners, o90)
    expect_identical(alias$estimate, x90$estimate)
  }
})

test_that("signed SCCS root certification refuses unresolved and degenerate scores", {
  # Exact cancellation makes the entire range roots, not an identifiable fit.
  z <- .dsomopReleaseSccsFit(c(0.5, 0.5), c(100, -100), 0)
  expect_true(is.na(z[["beta"]]))
  expect_identical(attr(z, "reason"), "uncertified_root")
  expect_identical(attr(.dsomopReleaseSccsFit(c(0.1, 0.9), c(0, 0), 0),
                       "reason"), "not_identifiable")
})

test_that("C8 allocation includes parent and complete subgroup vectors", {
  .release_state()
  h <- .astra_same_exit_fixture()
  on.exit(cleanup_handle(h), add = TRUE)
  x <- .release_run(h, "dsomop:cm.interaction_estimate", list(
    outcome_concept_id = "4000002", model_type = "cox", subgroup_kind = "sex",
    tar_start_offset = "0", tar_end_offset = "90",
    tar_anchor_start = "start", tar_anchor_end = "start"),
    list("astra_cox_t", "astra_cox_c"))
  components <- attr(x, "dsomop_protection")$components
  # One C6 vector and one C8 vector at each hierarchy level, not one draw
  # counted per requested displayed row and not one allocation for both.
  family6 <- Filter(function(c) identical(c$family, "C6") &&
                      identical(c$hierarchy, "family:sex"), components)
  family8 <- Filter(function(c) identical(c$family, "C8") &&
                      identical(c$hierarchy, "family:sex"), components)
  expect_length(family6, 3L)
  expect_length(family8, 3L)
  expect_equal(sum(unlist(family6[[1L]]$allocation_epsilon)), 0.1)
  expect_equal(sum(unlist(family8[[1L]]$allocation_epsilon)), 0.08)
})
