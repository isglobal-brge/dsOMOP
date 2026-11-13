# ==============================================================================
# 2.10.3 P0 item G-06: fitted-model scalars and complement families.
#
# Regression tests for the attacks of the 2.10.5 OHDSI-tool audit
# (design/2.10.5/scratch/ohdsi-b/exp/exp1, exp2, exp8, exp9 and
# astra-scratch/small-support/*), run through the real analysis run path:
#   * exp2  PLP calibration with 1-2 outcomes: the single bin's mean predicted
#           risk recovered the exact outcome count.
#   * exp8  PLP AUC + mean risk solved a two-person case-by-covariate cell.
#   * exp9  propensity AUC solved a two-person arm-by-covariate cell.
#   * exp1  interaction "overall minus level" recovered a suppressed subgroup
#           (2 target persons, 1 outcome).
# plus the admission rule, the public model grids, the calibration release
# rule and the subgroup-family decision as units.
# ==============================================================================

g06_opts <- function() {
  list(nfilter.subset = 3, nfilter.tab = 3, nfilter.glm = 0.33,
       dsomop.nfilter.dist = 10, dsomop.nfilter.band = 5,
       nfilter.levels.max = 40)
}

g06_handle <- function(n_persons = 200) {
  h <- create_test_handle(n_persons = n_persons)
  .setLegacyTestPersonKey(h, "g06-models", .local_envir = parent.frame())
  .buildBlueprint(h)
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

g06_cohort <- function(h, name, ids, start = "2020-01-01",
                       end = "2023-12-31") {
  DBI::dbExecute(h$conn, paste0("DROP TABLE IF EXISTS ", name))
  DBI::dbExecute(h$conn, sprintf(paste0(
    "CREATE TEMP TABLE %s AS SELECT person_id AS subject_id, '%s' AS ",
    "cohort_start_date, '%s' AS cohort_end_date FROM person WHERE ",
    "person_id IN (%s)"), name, start, end, paste(ids, collapse = ",")))
  register_test_temp(h, name)
  name
}

g06_condition_inserter <- function(h) {
  next_id <- 900000L
  function(pid, concept, date) {
    for (p in pid) {
      next_id <<- next_id + 1L
      DBI::dbExecute(h$conn, sprintf(paste0(
        "INSERT INTO condition_occurrence (condition_occurrence_id, ",
        "person_id, condition_concept_id, condition_start_date, ",
        "condition_end_date, condition_type_concept_id) VALUES ",
        "(%d, %d, %d, '%s', '%s', 44818518)"),
        next_id, as.integer(p), as.integer(concept), date, date))
    }
  }
}

# ---------------------------------------------------------------------------- #
# Admission rule (units).
# ---------------------------------------------------------------------------- #

test_that("admission inspects present and absent cells within each class", {
  withr::local_options(g06_opts())
  y <- c(rep(1L, 6), rep(0L, 111))
  # Marginal 42 present / 75 absent, but only 2 of the 6 cases have it.
  cov_case_cell <- c(1, 1, 0, 0, 0, 0, rep(1, 40), rep(0, 71))
  # Present in 5 of 6 cases: only 1 case lacks it.
  cov_case_absent <- c(1, 1, 1, 1, 1, 0, rep(1, 30), rep(0, 81))
  # Fully supported in both classes (3/3 cases, 50/61 non-cases).
  cov_ok <- c(1, 1, 1, 0, 0, 0, rep(1, 50), rep(0, 61))
  X <- cbind(cov_case_cell, cov_case_absent, cov_ok)
  expect_identical(.omopModelAdmitBinaryDesign(X, y = y), 3L)
  # The legacy marginal-only check (no response) would admit all three.
  expect_identical(.omopModelAdmitBinaryDesign(X), 1:3)
})

test_that("an inadmissible response is NULL, never an empty design", {
  withr::local_options(g06_opts())
  X <- cbind(a = rep(c(1, 0), 10))
  expect_null(.omopModelAdmitBinaryDesign(X, y = c(1, 1, rep(0, 18))))
  expect_null(.omopModelAdmitBinaryDesign(X, y = c(NA, rep(c(1, 0), 9), 1)))
  expect_null(.omopModelAdmitBinaryDesign(X, y = rep(c(1, 0), 5)))   # length
  expect_null(.omopModelAdmitBinaryDesign(X, y = rep(c(2, 0), 10)))  # not 0/1
  # An admissible response with no admissible predictor is an empty set.
  y <- rep(c(1L, 0L), 10)
  expect_identical(.omopModelAdmitBinaryDesign(cbind(a = rep(1, 20)), y = y),
                   integer(0))
  expect_identical(
    .omopModelAdmitBinaryDesign(matrix(0L, 20L, 0L), y = y), integer(0))
})

test_that("non-binary or missing predictor values are never admitted", {
  withr::local_options(g06_opts())
  y <- rep(c(1L, 0L), 15)
  X <- cbind(ok = rep(c(1, 1, 0, 0, 1, 0), 5),
             missing = c(NA, rep(c(1, 1, 0, 0, 1, 0), 5)[-1]),
             count = rep(c(2, 0, 1), 10))
  expect_identical(.omopModelAdmitBinaryDesign(X, y = y), 1L)
})

# ---------------------------------------------------------------------------- #
# Public model grids (units).
# ---------------------------------------------------------------------------- #

test_that("model parameters snap to the public grid: nearest, ties smaller", {
  features <- c(10, 20, 50, 100)
  cases <- list(c(1, 10), c(10, 10), c(14, 10), c(15, 10), c(16, 20),
                c(35, 20), c(36, 50), c(75, 50), c(76, 100), c(100, 100),
                c(1000, 100), c(-5, 10), c(0, 10), c(15.0000001, 20))
  for (case in cases) {
    expect_identical(.omopSnapToPublicGrid(case[[1]], features), case[[2]],
                     info = case[[1]])
  }
  bins <- c(5, 10, 20)
  cases <- list(c(2, 5), c(7.5, 5), c(8, 10), c(15, 10), c(16, 20),
                c(200, 20), c(5, 5))
  for (case in cases) {
    expect_identical(.omopSnapToPublicGrid(case[[1]], bins), case[[2]],
                     info = case[[1]])
  }
  expect_error(.omopSnapToPublicGrid(Inf, bins), "finite")
  expect_error(.omopSnapToPublicGrid(NA, bins), "finite")
  expect_error(.omopSnapToPublicGrid(5, numeric(0)), "empty")
})

test_that("the feature grid is capped by dsomop.max_feature_specs", {
  withr::local_options(list(dsomop.max_feature_specs = 1000))
  expect_identical(.omopModelReleaseGrid("features"), c(10L, 20L, 50L, 100L))
  expect_identical(.omopModelReleaseGrid("bins"), c(5L, 10L, 20L))
  withr::local_options(list(dsomop.max_feature_specs = 30))
  expect_identical(.omopModelReleaseGrid("features"), c(10L, 20L))
  p <- .omopModelGridParam("100", "features", "20", "top_k")
  expect_identical(p$effective, 20L)
  expect_identical(p$requested, "100")
  withr::local_options(list(dsomop.max_feature_specs = 5))
  expect_length(.omopModelReleaseGrid("features"), 0L)
  expect_error(.omopModelGridParam("20", "features", "20", "top_k"),
               "release grid is empty")
})

# ---------------------------------------------------------------------------- #
# Calibration release rule (unit).
# ---------------------------------------------------------------------------- #

test_that("calibration means need supported outcomes and non-outcomes per bin", {
  withr::local_options(g06_opts())
  raw <- data.frame(
    bin = 1:6,
    mean_predicted = c(0.01, 0.02, 0.98, 0.3, 0.3, 0.3),
    expected_outcomes = c(0.4, 2.0, 49.0, 12.6, 3.3, 30.4),
    n_persons = c(40, 100, 50, 42, 11, 101),
    n_outcomes = c(0, 2, 48, 13, 4, 31),
    observed_rate = NA_real_)
  out <- .omopPlpCalibrationRelease(raw)
  expect_false("expected_outcomes" %in% names(out))
  # 0 outcomes, 2 outcomes, 2 non-outcomes: masked.
  expect_true(all(is.na(out$mean_predicted[1:3])))
  # Supported: released at banded precision, band(E) / band(n).
  expect_equal(out$mean_predicted[4], 10 / 40)
  # 11 persons < nfilter.dist would be masked; here n = 11 >= 10 but the
  # expected count bands to 0 -> coupled away.
  expect_true(is.na(out$mean_predicted[5]))
  expect_equal(out$mean_predicted[6], 30 / 100)
  # Never more precise than the band: every released mean times its banded
  # bin count is a multiple of the band width.
  ok <- !is.na(out$mean_predicted)
  implied <- out$mean_predicted[ok] * out$n_persons[ok]
  expect_true(all(abs(implied - round(implied / 5) * 5) < 1e-9))
  # observed_rate is reconciled from banded counts and coupled at 0.
  expect_true(is.na(out$observed_rate[1]))
  expect_equal(out$observed_rate[4], 10 / 40)
  # Below nfilter.dist persons the mean is always masked.
  small <- raw[4, ]
  small$n_persons <- 9; small$n_outcomes <- 4; small$expected_outcomes <- 4.2
  expect_true(is.na(.omopPlpCalibrationRelease(small)$mean_predicted))
})

# ---------------------------------------------------------------------------- #
# exp2: PLP calibration with 1-2 outcomes.
# ---------------------------------------------------------------------------- #

test_that("exp2: 1-2 outcomes cannot be recovered from the calibration mean", {
  h <- g06_handle(200)
  on.exit(cleanup_handle(h))
  withr::with_options(g06_opts(), {
    add <- g06_condition_inserter(h)
    DBI::dbExecute(h$conn, paste0(
      "DELETE FROM condition_occurrence WHERE condition_concept_id IN ",
      "(4000002, 201820)"))
    # Index 2021-03-01: the snapped look-back window [-390, -1] must lie
    # inside the fixture observation periods (2020-01-01 onwards).
    g06_cohort(h, "g06_plp_t", 1:113, start = "2021-03-01",
               end = "2021-03-01")
    add(1:40, 201820L, "2020-06-01")
    P <- list(outcome_concept_id = "4000002", covariate_domain_code = "0",
              max_covariates = "1")
    for (outcomes in list(5L, c(5L, 77L))) {
      DBI::dbExecute(h$conn, paste0(
        "DELETE FROM condition_occurrence WHERE condition_concept_id = ",
        "4000002"))
      add(outcomes, 4000002L, "2021-06-01")
      cal <- .omopAnalysisRun(h, "dsomop:plp.calibration", P,
                              scope = "g06_plp_t")
      perf <- .omopAnalysisRun(h, "dsomop:plp.performance", P,
                               scope = "g06_plp_t")
      dia <- .omopAnalysisRun(h, "dsomop:plp.diagnostic", P,
                              scope = "g06_plp_t")
      thr <- .omopAnalysisRun(h, "dsomop:plp.threshold", P,
                              scope = "g06_plp_t")
      info <- paste(length(outcomes), "outcome person(s)")
      # The outcome class is below nfilter.tab: no model is admitted, so no
      # calibration/threshold table and no discrimination metric exists, and
      # no intercept-only substitute is fitted.
      expect_equal(nrow(cal), 0L, info = info)
      expect_equal(nrow(thr), 0L, info = info)
      expect_true(all(is.na(perf$value)), info = info)
      expect_equal(dia$n_predictors, 0, info = info)
      expect_true(is.na(dia$events_per_variable), info = info)
      expect_identical(
        attr(cal, "dsomop_effective_params")$model_admission,
        "not_admitted", info = info)
      # The baseline attack: mean_predicted * n is an exact integer.
      if (nrow(cal) == 1L && is.finite(cal$mean_predicted)) {
        fail("a single calibration bin released its mean predicted risk")
      }
    }
  })
})

# ---------------------------------------------------------------------------- #
# exp8: PLP two-person case-by-covariate cell.
# ---------------------------------------------------------------------------- #

test_that("exp8: PLP AUC and mean risk no longer solve a 2-person case cell", {
  h <- g06_handle(200)
  on.exit(cleanup_handle(h))
  withr::with_options(g06_opts(), {
    add <- g06_condition_inserter(h)
    DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
    g06_cohort(h, "g06_pa", 1:117, start = "2021-03-01", end = "2021-03-01")
    add(c(1, 2, 50:89), 201820L, "2020-06-01")   # 42 persons, cases 1 and 2
    add(1:5, 4000002L, "2021-06-01")              # 5 outcome persons
    P <- list(outcome_concept_id = "4000002", max_covariates = "1")
    perf <- .omopAnalysisRun(h, "dsomop:plp.performance", P, scope = "g06_pa")
    cal <- .omopAnalysisRun(h, "dsomop:plp.calibration", P, scope = "g06_pa")
    # The covariate fails the within-class rule (2 cases have it): no
    # admissible predictor, hence no model and no coupled scalars.
    expect_true(all(is.na(perf$value)))
    expect_equal(nrow(cal), 0L)
    meta <- attr(perf, "dsomop_effective_params")
    expect_identical(meta$model_admission, "not_admitted")
    expect_identical(meta$requested$max_covariates, "1")
    expect_identical(meta$effective$max_covariates, 10L)
    # The audit's brute force has no AUC to fit.
    auc <- perf$value[perf$metric == "AUROC"]
    expect_true(is.na(auc))
  })
})

test_that("a supported PLP model still fits and releases its scalars", {
  h <- g06_handle(200)
  on.exit(cleanup_handle(h))
  withr::with_options(g06_opts(), {
    add <- g06_condition_inserter(h)
    DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
    g06_cohort(h, "g06_pb", 1:120, start = "2021-03-01", end = "2021-03-01")
    # Covariate in 10 of 25 cases and 30 of 95 non-cases (all cells >= 3).
    add(c(1:10, 26:55), 201820L, "2020-06-01")
    add(1:25, 4000002L, "2021-06-01")
    P <- list(outcome_concept_id = "4000002", max_covariates = "14")
    perf <- .omopAnalysisRun(h, "dsomop:plp.performance", P, scope = "g06_pb")
    expect_false(any(is.na(perf$value)))
    meta <- attr(perf, "dsomop_effective_params")
    expect_identical(meta$model_admission, "admitted")
    expect_identical(meta$effective$max_covariates, 10L)
    cal <- .omopAnalysisRun(h, "dsomop:plp.calibration", P, scope = "g06_pb")
    expect_true(nrow(cal) >= 1L)
    ok <- !is.na(cal$mean_predicted)
    # Released means are at banded precision (band(E) / band(n)).
    expect_true(all(abs(cal$mean_predicted[ok] * cal$n_persons[ok] -
                          round(cal$mean_predicted[ok] * cal$n_persons[ok] /
                                  5) * 5) < 1e-9))
  })
})

# ---------------------------------------------------------------------------- #
# exp9: propensity two-person arm-by-covariate cell.
# ---------------------------------------------------------------------------- #

test_that("exp9: the propensity AUC no longer solves a 2-person arm cell", {
  h <- g06_handle(200)
  on.exit(cleanup_handle(h))
  withr::with_options(g06_opts(), {
    add <- g06_condition_inserter(h)
    DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
    g06_cohort(h, "g06_pt", 1:97)
    g06_cohort(h, "g06_pc", 101:188)
    add(c(1, 2, 101:140), 201820L, "2019-06-01")  # 2 target, 40 comparator
    r <- .omopAnalysisRun(
      h, "dsomop:cm.propensity_distribution",
      list(domain_code = "0", top_k = "1", n_bins = "10"),
      scope = list("g06_pt", "g06_pc"))
    expect_true(nrow(r) >= 1L)
    meta <- attr(r, "dsomop_effective_params")
    # The covariate fails the within-arm rule: the explicit intercept-only
    # propensity contract applies (every score is the arm prevalence).
    expect_identical(meta$model_admission, "intercept_only")
    expect_identical(meta$effective$top_k, 10L)
    auc <- unique(r$auc)
    expect_length(auc, 1L)
    expect_equal(auc, 0.5)
    # The audit's brute force: (2 target, 40 comparator) must not fit the
    # released AUC.
    bt <- sum(r$person_count[r$arm == "target"])
    bc <- sum(r$person_count[r$arm == "comparator"])
    found <- FALSE
    for (nt in bt:(bt + 4)) for (nc in bc:(bc + 4)) {
      a <- 2; b <- 40
      u <- (nt - a) * b + 0.5 * ((nt - a) * (nc - b) + a * b)
      if (abs(u / (nt * nc) - auc) < 1e-12) found <- TRUE
    }
    expect_false(found)
  })
})

test_that("the propensity model refuses an inadmissible arm vector", {
  withr::local_options(g06_opts())
  design <- data.frame(subject_id = as.character(1:12),
                       arm = c(1L, 1L, rep(0L, 10)),
                       cov_1 = rep(c(1L, 0L), 6),
                       stringsAsFactors = FALSE)
  expect_error(.omopCmPropensityScores(design), "not admitted")
  design$arm[1] <- NA_integer_
  expect_error(.omopCmPropensityScores(design), "not admitted")
})

test_that("an admitted propensity covariate keeps an informative model", {
  withr::local_options(g06_opts())
  design <- data.frame(subject_id = as.character(1:40),
                       arm = c(rep(1L, 20), rep(0L, 20)),
                       cov_1 = c(rep(1L, 15), rep(0L, 5), rep(1L, 4),
                                 rep(0L, 16)),
                       cov_2 = c(rep(1L, 2), rep(0L, 18), rep(1L, 10),
                                 rep(0L, 10)),
                       stringsAsFactors = FALSE)
  ps <- .omopCmPropensityScores(design)
  admission <- attr(ps, "dsomop_model_admission")
  # cov_2 has only 2 target persons: not admitted. cov_1 passes.
  expect_identical(admission$status, "admitted")
  expect_identical(admission$n_admitted, 1L)
  expect_gt(.omopCmPropensityAuc(ps, design$arm), 0.5)
})

# ---------------------------------------------------------------------------- #
# Subgroup families (units).
# ---------------------------------------------------------------------------- #

g06_counts <- function(persons, outcome_persons) {
  list(persons = persons, outcome_persons = outcome_persons)
}
g06_level <- function(tp, to, cp, co) {
  list(target = g06_counts(tp, to), comparator = g06_counts(cp, co))
}

test_that("a subgroup family is released only when levels and complements pass", {
  withr::local_options(g06_opts())
  arms <- g06_level(100, 30, 100, 16)
  ok <- list(g06_level(40, 10, 50, 6), g06_level(60, 20, 50, 10))
  expect_true(.omopCmInteractionFamilyCheck(ok, arms)$releasable)
  # A level with 2 persons.
  tiny <- list(g06_level(2, 1, 12, 4), g06_level(98, 29, 88, 12))
  expect_false(.omopCmInteractionFamilyCheck(tiny, arms)$releasable)
  # A level with enough persons but 2 outcome persons.
  few_out <- list(g06_level(40, 2, 50, 6), g06_level(60, 28, 50, 10))
  expect_false(.omopCmInteractionFamilyCheck(few_out, arms)$releasable)
  # A level with 2 non-outcome persons (an arm-by-outcome model cell).
  few_non <- list(g06_level(12, 10, 50, 6), g06_level(88, 20, 50, 10))
  expect_false(.omopCmInteractionFamilyCheck(few_non, arms)$releasable)
  # Zero is not a small cell: an empty level is fine.
  empty <- list(g06_level(0, 0, 0, 0), g06_level(100, 30, 100, 16))
  expect_true(.omopCmInteractionFamilyCheck(empty, arms)$releasable)
  # A family that does not partition the arm is withheld.
  gap <- list(g06_level(40, 10, 50, 6), g06_level(58, 20, 50, 10))
  expect_false(.omopCmInteractionFamilyCheck(gap, arms)$releasable)
  # One small level withholds every level of the family (a valid partition
  # makes each complement a sum of zero-or-supported levels; the explicit
  # complement check is defence in depth).
  three <- list(g06_level(48, 14, 34, 5), g06_level(50, 15, 33, 6),
                g06_level(2, 1, 33, 5))
  expect_false(.omopCmInteractionFamilyCheck(three, arms)$releasable)
})

test_that("subgroup families are complete partitions with residual levels", {
  h <- g06_handle(40)
  on.exit(cleanup_handle(h))
  withr::with_options(g06_opts(), {
    sex <- .omopCmInteractionSubgroups(h, list(subgroup_kind = "sex"))
    expect_identical(vapply(sex, `[[`, character(1), "label"),
                     c("sex: male", "sex: female", "sex: other or unknown"))
    # Unknown gender is the complement of the two existence predicates, never
    # a NOT IN list (which loses NULLs).
    expect_match(sex[[3]]$predicate, "^NOT \\(EXISTS")
    expect_false(grepl("NOT IN", sex[[3]]$predicate))
    age <- .omopCmInteractionSubgroups(
      h, list(subgroup_kind = "age", age_breaks = "50,65"))
    expect_identical(vapply(age, `[[`, character(1), "label"),
                     c("age: <50", "age: 50-64", "age: >=65", "age: unknown"))
    expect_match(age[[4]]$predicate, "^NOT \\(EXISTS")
    concept <- .omopCmInteractionSubgroups(
      h, list(subgroup_kind = "concept", subgroup_concept_id = "317009"))
    expect_length(concept, 2L)
    expect_identical(concept[[2]]$predicate,
                     paste0("NOT ", concept[[1]]$predicate))
    overall <- .omopCmInteractionSubgroups(h, list(subgroup_kind = "overall"))
    expect_identical(overall[[1]]$label, "overall")
    # An absent concept is an error, never a silent "overall" level.
    expect_error(.omopCmInteractionSubgroups(h, list(subgroup_kind = "concept")),
                 "requires subgroup_concept_id")
    expect_error(.omopCmInteractionSubgroups(h, list(subgroup_kind = "age")),
                 "requires age_breaks")
  })
})

test_that("a family over the level cap is refused, never truncated", {
  h <- g06_handle(40)
  on.exit(cleanup_handle(h))
  withr::with_options(list(nfilter.levels.max = 4), {
    expect_error(.omopCmInteractionSubgroups(
      h, list(subgroup_kind = "age", age_breaks = "30,40,50")),
      "more levels than")
    # 2 breaks -> 3 bands + unknown = 4 levels: at the cap.
    expect_length(.omopCmInteractionSubgroups(
      h, list(subgroup_kind = "age", age_breaks = "40,60")), 4L)
  })
})

# ---------------------------------------------------------------------------- #
# exp1: interaction overall-minus-level differencing.
# ---------------------------------------------------------------------------- #

g06_interaction_fixture <- function(h, asthma_target, asthma_comparator) {
  add <- g06_condition_inserter(h)
  DBI::dbExecute(h$conn, paste0(
    "DELETE FROM condition_occurrence WHERE condition_concept_id IN ",
    "(4000002, 317009)"))
  g06_cohort(h, "g06_arm_t", 1:100)
  g06_cohort(h, "g06_arm_c", 101:200)
  add(c(3, 10:38), 4000002L, "2021-03-15")      # 30 target outcome persons
  add(101:116, 4000002L, "2022-03-10")          # 16 comparator outcome persons
  add(c(asthma_target, asthma_comparator), 317009L, "2019-05-01")
  invisible(NULL)
}

g06_ix_params <- function(extra = list()) {
  c(list(outcome_concept_id = "4000002", outcome_domain_code = "0",
         model_type = "poisson", tar_start_offset = "1",
         tar_end_offset = "0"), extra)
}

test_that("exp1: overall minus level no longer recovers a suppressed subgroup", {
  h <- g06_handle(200)
  on.exit(cleanup_handle(h))
  withr::with_options(g06_opts(), {
    # Asthma: 2 target persons (3 has the outcome, 7 does not), 12 comparator.
    g06_interaction_fixture(h, c(3, 7), 150:161)
    scope <- list("g06_arm_t", "g06_arm_c")
    sub <- .omopAnalysisRun(h, "dsomop:cm.interaction_estimate",
                            g06_ix_params(list(
                              subgroup_kind = "concept",
                              subgroup_concept_id = "317009",
                              subgroup_domain_code = "0")),
                            scope = scope)
    # The whole family is withheld: no "no" row exists to subtract.
    expect_equal(nrow(sub), 0L)
    expect_identical(attr(sub, "dsomop_release_notes")$subgroup_family,
                     "withheld")
    # The whole-arm estimate is still available, but only on request.
    all <- .omopAnalysisRun(h, "dsomop:cm.interaction_estimate",
                            g06_ix_params(list(subgroup_kind = "overall")),
                            scope = scope)
    expect_equal(nrow(all), 1L)
    expect_identical(all$subgroup_label, "overall")
    # The silent fallback of the baseline (concept kind without a concept).
    expect_error(.omopAnalysisRun(h, "dsomop:cm.interaction_estimate",
                                  g06_ix_params(list(subgroup_kind = "concept")),
                                  scope = scope),
                 "requires subgroup_concept_id")
  })
})

test_that("a fully supported concept family is still released", {
  h <- g06_handle(200)
  on.exit(cleanup_handle(h))
  withr::with_options(g06_opts(), {
    # Target "yes": 3 outcome + 3 non-outcome persons; comparator "yes": 7
    # outcome persons (110..116) + 5 non-outcome persons (117..121).
    g06_interaction_fixture(h, c(3, 10, 11, 40, 41, 42), 110:121)
    sub <- .omopAnalysisRun(h, "dsomop:cm.interaction_estimate",
                            g06_ix_params(list(
                              subgroup_kind = "concept",
                              subgroup_concept_id = "317009",
                              subgroup_domain_code = "0")),
                            scope = list("g06_arm_t", "g06_arm_c"))
    expect_identical(attr(sub, "dsomop_release_notes")$subgroup_family,
                     "released")
    expect_setequal(sub$subgroup_label, c("Asthma: yes", "Asthma: no"))
  })
})

test_that("unknown gender is its own level and small unknowns withhold sex", {
  h <- g06_handle(200)
  on.exit(cleanup_handle(h))
  withr::with_options(g06_opts(), {
    g06_interaction_fixture(h, integer(0), integer(0))
    scope <- list("g06_arm_t", "g06_arm_c")
    P <- g06_ix_params(list(subgroup_kind = "sex"))
    # No unknown gender: male/female released (the empty level is not small).
    base <- .omopAnalysisRun(h, "dsomop:cm.interaction_estimate", P,
                             scope = scope)
    expect_identical(attr(base, "dsomop_release_notes")$subgroup_family,
                     "released")
    # Two target persons with a NULL or other gender (non-outcome persons).
    DBI::dbExecute(h$conn, "UPDATE person SET gender_concept_id = NULL WHERE person_id = 50")
    DBI::dbExecute(h$conn, "UPDATE person SET gender_concept_id = 8551 WHERE person_id = 51")
    small <- .omopAnalysisRun(h, "dsomop:cm.interaction_estimate", P,
                              scope = scope)
    expect_equal(nrow(small), 0L)
    expect_identical(attr(small, "dsomop_release_notes")$subgroup_family,
                     "withheld")
    # A third unknown makes the level releasable (3 non-outcome persons).
    DBI::dbExecute(h$conn, "UPDATE person SET gender_concept_id = NULL WHERE person_id = 52")
    three <- .omopAnalysisRun(h, "dsomop:cm.interaction_estimate", P,
                              scope = scope)
    expect_identical(attr(three, "dsomop_release_notes")$subgroup_family,
                     "released")
  })
})

test_that("missing ages stay in the explicit unknown level", {
  h <- g06_handle(200)
  on.exit(cleanup_handle(h))
  withr::with_options(g06_opts(), {
    g06_interaction_fixture(h, integer(0), integer(0))
    DBI::dbExecute(h$conn, paste0(
      "UPDATE person SET year_of_birth = NULL WHERE person_id IN (60, 61)"))
    out <- .omopAnalysisRun(h, "dsomop:cm.interaction_estimate",
                            g06_ix_params(list(subgroup_kind = "age",
                                               age_breaks = "60")),
                            scope = list("g06_arm_t", "g06_arm_c"))
    # Two target persons with no usable age: the family is withheld rather
    # than silently dropping them from every band.
    expect_equal(nrow(out), 0L)
    expect_identical(attr(out, "dsomop_release_notes")$subgroup_family,
                     "withheld")
  })
})
