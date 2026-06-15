# ==============================================================================
# Tests for the live R-in-session CohortMethod fitted effect-estimate port
# (dsomop:cm.effect_estimate; group PLR-1; NO precomputed OHDSI twin id).
#
# This kind="r" two-population entry loads the per-subject (arm, time, event)
# frame into the server-side R session, FITS the comparative model
# (survival::coxph hazard ratio, or stats::glm Poisson rate ratio), and emits one
# row per arm carrying the per-arm persons/person-days/outcomes substrate PLUS
# the fitted estimate + 95% CI + the log-estimate/SE meta-analysis sufficient
# statistics (replicated across both rows). It funnels through the ONE gate
# (.omopAnalysisGate). This file proves, on the SQLite fixture:
#   (a) the id resolves, two-population (max_tables=2), unit=record, plot parses;
#   (b) it RUNS over two scoped arms and returns banded counts + a finite HR+CI;
#   (c) the released log_estimate equals a hand-fit model over the SAME frame;
#   (d) sub-threshold arm fails closed; un-scoped / single-arm returns gate-empty
#       or errors; the estimate is NA'd when an arm's outcomes band below thr.
# ==============================================================================

cmeff_handle <- function(n_persons = 40) {
  h <- create_test_handle(n_persons = n_persons)
  h$person_key <- as.raw(1:16)
  .buildBlueprint(h)
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

cmeff_opts <- function() {
  list(nfilter.subset = 3, nfilter.tab = 3,
       dsomop.nfilter.dist = 10, dsomop.nfilter.band = 5)
}

# Two synthetic arms (20 persons each) with a long follow-up window; the outcome
# (4000002, a clean leaf in the fixture vocab) is elevated in the target arm so
# the fitted estimate is finite and > 1.
cmeff_setup_arms <- function(h, target_events = 1:12, comparator_events = 21:25) {
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE cme_target AS SELECT person_id AS subject_id, ",
    "'2020-01-01' AS cohort_start_date, '2023-12-31' AS cohort_end_date ",
    "FROM person WHERE person_id BETWEEN 1 AND 20"))
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE cme_comparator AS SELECT person_id AS subject_id, ",
    "'2020-01-01' AS cohort_start_date, '2023-12-31' AS cohort_end_date ",
    "FROM person WHERE person_id BETWEEN 21 AND 40"))
  DBI::dbExecute(h$conn,
    "DELETE FROM condition_occurrence WHERE condition_concept_id = 4000002")
  cid <- 8200L
  add_out <- function(pid, dt) {
    cid <<- cid + 1L
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
      "condition_concept_id, condition_start_date, condition_end_date, ",
      "condition_type_concept_id) VALUES (%d, %d, 4000002, '%s', '%s', ",
      "44818518)"), cid, pid, dt, dt))
  }
  for (pid in target_events)     add_out(pid, "2020-02-15")
  for (pid in comparator_events) add_out(pid, "2020-04-15")
  invisible(NULL)
}

EFF_PARAMS <- list(outcome_concept_id = "4000002", outcome_domain_code = "0",
                   tar_start_offset = "0", model_type = "cox")

# ---------------------------------------------------------------------------- #
# (a) Discovery: id resolves, two-population, unit=record, plot parses.
# ---------------------------------------------------------------------------- #

test_that("(a) cm.effect_estimate resolves as a two-population record entry", {
  h <- cmeff_handle()
  on.exit(cleanup_handle(h))
  e <- .omopAnalysisResolve(h, "dsomop:cm.effect_estimate")
  expect_equal(as.integer(e$scope$max_tables), 2L)
  expect_equal(e$disclosure$unit, "record")
  expect_equal(e$disclosure$person_id_col, "persons")
  expect_setequal(e$disclosure$count_cols,
                  c("persons", "outcomes", "person_days"))
  expect_equal(e$meta$adapter, "ohdsi_live")
  fn <- eval(parse(text = e$compute$plot$code))
  expect_true(is.function(fn))
  expect_named(formals(fn), c("df", "params"))
})

test_that("(a) cm.effect_estimate is in the live registry (no precomputed twin)", {
  h <- cmeff_handle()
  on.exit(cleanup_handle(h))
  reg <- suppressWarnings(.omopAnalysisRegistry(h))
  expect_true("dsomop:cm.effect_estimate" %in% names(reg))
  # The fitted estimate has no read-precomputed cohort_method twin id.
  expect_null(reg[["dsomop:cm.effect_estimate"]]$meta$precomputed)
})

# ---------------------------------------------------------------------------- #
# (b) Runs over two arms; banded counts + a finite HR + CI.
# ---------------------------------------------------------------------------- #

test_that("(b) returns per-arm banded counts and a finite HR + CI", {
  h <- cmeff_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(cmeff_opts(), {
    cmeff_setup_arms(h)
    df <- .omopAnalysisRun(h, "dsomop:cm.effect_estimate", params = EFF_PARAMS,
                           scope = list("cme_target", "cme_comparator"))
    expect_equal(nrow(df), 2L)
    expect_true(all(c("arm", "model_type", "persons", "person_days", "outcomes",
                      "estimate", "ci_lo", "ci_hi", "log_estimate",
                      "se_log_estimate") %in% names(df)))
    expect_setequal(df$arm, c("target", "comparator"))
    # Every released count is banded (floor to nfilter_band) or NA.
    for (col in c("persons", "outcomes", "person_days")) {
      v <- df[[col]]
      expect_true(all(v %% 5 == 0 | is.na(v)))
    }
    # The fitted estimate is finite, positive and the CI brackets it.
    expect_true(is.finite(df$estimate[1]) && df$estimate[1] > 0)
    expect_true(df$ci_lo[1] <= df$estimate[1] && df$estimate[1] <= df$ci_hi[1])
    # exp(log_estimate) reproduces the estimate; exp(log +/- 1.96 SE) the CI.
    expect_equal(exp(df$log_estimate[1]), df$estimate[1], tolerance = 1e-8)
    expect_equal(exp(df$log_estimate[1] - 1.96 * df$se_log_estimate[1]),
                 df$ci_lo[1], tolerance = 1e-8)
    # The estimate is replicated identically across both arm rows.
    expect_equal(df$estimate[1], df$estimate[2])
    expect_false(any(grepl("_source_value$", names(df))))
  })
})

# ---------------------------------------------------------------------------- #
# (c) The released log_estimate equals a hand-fit model over the SAME frame.
# ---------------------------------------------------------------------------- #

test_that("(c) the released log_estimate equals a hand-fit Cox/Poisson", {
  skip_if_not_installed("survival")
  h <- cmeff_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(cmeff_opts(), {
    cmeff_setup_arms(h)
    out_src <- .omopOutcomeSource(h, "4000002", "0")
    tar <- list(start = 0L, end = 0L, anchor_start = "start", anchor_end = "end")
    ta <- .omopCmEffectArmData(h, "cme_target", 1L, out_src, tar)
    co <- .omopCmEffectArmData(h, "cme_comparator", 0L, out_src, tar)
    fit <- .omopCmEffectFit(rbind(ta$data, co$data), "cox")
    expect_true(is.finite(fit$log_estimate))

    df <- .omopAnalysisRun(h, "dsomop:cm.effect_estimate", params = EFF_PARAMS,
                           scope = list("cme_target", "cme_comparator"))
    # The gate does not touch the non-count estimate columns.
    expect_equal(df$log_estimate[1], fit$log_estimate, tolerance = 1e-8)
    expect_equal(df$se_log_estimate[1], fit$se_log_estimate, tolerance = 1e-8)
  })
})

# ---------------------------------------------------------------------------- #
# (d) Disclosure: fail-closed on small arm; un-scoped/single-arm; estimate NA'd.
# ---------------------------------------------------------------------------- #

test_that("(d) a sub-threshold arm fails closed before any fit", {
  h <- cmeff_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(cmeff_opts(), {
    cmeff_setup_arms(h)
    DBI::dbExecute(h$conn, paste0(
      "CREATE TEMP TABLE cme_tiny AS SELECT person_id AS subject_id, ",
      "'2020-01-01' AS cohort_start_date, '2023-12-31' AS cohort_end_date ",
      "FROM person WHERE person_id IN (1, 2)"))
    expect_error(
      .omopAnalysisRun(h, "dsomop:cm.effect_estimate", params = EFF_PARAMS,
                       scope = list("cme_tiny", "cme_comparator")),
      "insufficient individuals|disclosure threshold")
  })
})

test_that("(d) un-scoped returns a gate-safe empty frame; single-arm errors", {
  h <- cmeff_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(cmeff_opts(), {
    cmeff_setup_arms(h)
    # No two-population scope -> no comparison -> gate-safe empty frame.
    d0 <- .omopAnalysisRun(h, "dsomop:cm.effect_estimate", params = EFF_PARAMS)
    expect_equal(nrow(d0), 0L)
    # A single-element scope cannot form a target+comparator pair.
    expect_error(
      .omopAnalysisRun(h, "dsomop:cm.effect_estimate", params = EFF_PARAMS,
                       scope = "cme_target"),
      "two")
  })
})

test_that("(d) the estimate is NA'd when an arm's outcomes band below threshold", {
  # Comparator carries only ONE outcome event -> outcomes band to 0 (< nfilter_tab
  # after banding), so the fitted estimate must be NA on BOTH arm rows even though
  # the per-arm person counts pass the subset gate.
  h <- cmeff_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(cmeff_opts(), {
    cmeff_setup_arms(h, target_events = 1:12, comparator_events = 21L)
    df <- .omopAnalysisRun(h, "dsomop:cm.effect_estimate", params = EFF_PARAMS,
                           scope = list("cme_target", "cme_comparator"))
    if (nrow(df) > 0) {
      expect_true(all(is.na(df$estimate)))
      expect_true(all(is.na(df$log_estimate)))
      expect_true(all(is.na(df$ci_lo) & is.na(df$ci_hi)))
    } else {
      expect_equal(nrow(df), 0L)
    }
  })
})
