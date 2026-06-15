# ==============================================================================
# Tests for the live R-in-session SCCS fitted incidence-rate-ratio port
# (dsomop:sccs.incidence_rate_ratio; group PLR-9).
#
# This kind="r" entry loads the per-case exposed/unexposed (events, person-time)
# substrate into the server-side R session, FITS the SCCS conditional Poisson
# (stratified per case; stats fallback, or gnm::gnm when installed), and emits ONE
# aggregate row: the exposed-vs-unexposed IRR + 95% CI, the log-IRR + SE
# meta-analysis sufficient statistics, and the gated exposed/unexposed event +
# case counts it rests on. It funnels through the ONE gate (.omopAnalysisGate).
# This file proves, on the SQLite fixture:
#   (a) the id resolves, single-population record entry, unit=record, plot parses;
#   (b) it RUNS over a scoped case population and returns a sane banded IRR row;
#   (c) the fitted IRR matches a hand-fit stratified Poisson over the SAME frame,
#       and counts are banded;
#   (d) the non-frame guard NAs the IRR / drops the row when a window is
#       sub-threshold, and missing exposure/outcome returns a gate-safe empty frame.
# ==============================================================================

sccs_irr_handle <- function(n_persons = 40) {
  h <- create_test_handle(n_persons = n_persons)
  h$person_key <- as.raw(1:16)
  .buildBlueprint(h)
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

sccs_irr_opts <- function() {
  list(nfilter.subset = 3, nfilter.tab = 3,
       dsomop.nfilter.dist = 10, dsomop.nfilter.band = 5)
}

# Synthetic SCCS scenario: `n_cases` persons exposed to drug 7777 on 2021-01-01,
# each with 2 outcome events (4000002) INSIDE the 30-day risk window and
# `unexp_each` events well outside it (so both periods carry events + person-time
# and the exposed rate is elevated). The fixture's observation_period spans
# 2018..2030 for every person, giving a long unexposed baseline.
sccs_irr_setup <- function(h, cases = 1:20, unexp_each = 1L) {
  DBI::dbExecute(h$conn, "DELETE FROM drug_exposure")
  did <- 5000L
  for (pid in cases) {
    did <- did + 1L
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO drug_exposure (drug_exposure_id, person_id, drug_concept_id, ",
      "drug_exposure_start_date, drug_exposure_end_date, drug_type_concept_id) ",
      "VALUES (%d, %d, 7777, '2021-01-01', '2021-01-31', 38000177)"), did, pid))
  }
  DBI::dbExecute(h$conn,
    "DELETE FROM condition_occurrence WHERE condition_concept_id = 4000002")
  cid <- 9000L
  addc <- function(pid, dt) {
    cid <<- cid + 1L
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
      "condition_concept_id, condition_start_date, condition_end_date, ",
      "condition_type_concept_id) VALUES (%d, %d, 4000002, '%s', '%s', ",
      "44818518)"), cid, pid, dt, dt))
  }
  for (pid in cases) { addc(pid, "2021-01-10"); addc(pid, "2021-01-20") }
  for (pid in cases) for (k in seq_len(unexp_each)) addc(pid, "2022-06-01")
  invisible(NULL)
}

IRR_PARAMS <- list(exposure_concept_id = "7777", exposure_domain_code = "1",
                   outcome_concept_id = "4000002", outcome_domain_code = "0",
                   window = "30")

# ---------------------------------------------------------------------------- #
# (a) Discovery: id resolves, record unit, person companion, plot parses.
# ---------------------------------------------------------------------------- #

test_that("(a) sccs.incidence_rate_ratio resolves as a record entry", {
  h <- sccs_irr_handle()
  on.exit(cleanup_handle(h))
  e <- .omopAnalysisResolve(h, "dsomop:sccs.incidence_rate_ratio")
  expect_equal(e$disclosure$unit, "record")
  expect_equal(e$disclosure$person_id_col, "n_cases")
  expect_setequal(e$disclosure$count_cols,
                  c("n_cases", "exposed_events", "unexposed_events"))
  expect_equal(e$meta$adapter, "ohdsi_live")
  fn <- eval(parse(text = e$compute$plot$code))
  expect_true(is.function(fn))
  expect_named(formals(fn), c("df", "params"))
})

test_that("(a) there is NO precomputed ohdsi twin id for the fitted IRR", {
  # The fitted IRR is reclaimed by the R-in-session principle and has no
  # read-precomputed counterpart, so only the canonical id exists.
  h <- sccs_irr_handle()
  on.exit(cleanup_handle(h))
  ids <- names(.ohdsiPackSccsEntries(h))
  expect_true("dsomop:sccs.incidence_rate_ratio" %in% ids)
  # The sccs_result OHDSI alias delegates to the DESCRIPTIVE rate substrate, not
  # to the fitted IRR, so no ohdsi alias points at incidence_rate_ratio.
  irr_alias <- vapply(.ohdsiPackSccsEntries(h), function(e)
    identical(e$meta$alias_target, "dsomop:sccs.incidence_rate_ratio"),
    logical(1))
  expect_false(any(irr_alias))
})

# ---------------------------------------------------------------------------- #
# (b) Runs over a scoped case population; returns a sane banded IRR row.
# ---------------------------------------------------------------------------- #

test_that("(b) returns one banded IRR row with sufficient statistics", {
  h <- sccs_irr_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(sccs_irr_opts(), {
    sccs_irr_setup(h)
    df <- .omopAnalysisRun(h, "dsomop:sccs.incidence_rate_ratio",
                           params = IRR_PARAMS)
    expect_equal(nrow(df), 1L)
    expect_true(all(c("model_type", "n_cases", "exposed_events",
                      "unexposed_events", "irr", "ci_lo", "ci_hi",
                      "log_irr", "se_log_irr") %in% names(df)))
    # All released counts are banded (floor to nfilter_band) or NA.
    for (col in c("n_cases", "exposed_events", "unexposed_events")) {
      v <- df[[col]]
      expect_true(all(v %% 5 == 0 | is.na(v)))
    }
    # The IRR is finite, positive, and the CI brackets it.
    expect_true(is.finite(df$irr) && df$irr > 0)
    expect_true(df$ci_lo <= df$irr && df$irr <= df$ci_hi)
    # exp(log_irr) reproduces the IRR; exp(log_irr +/- 1.96 SE) the CI.
    expect_equal(exp(df$log_irr), df$irr, tolerance = 1e-8)
    expect_equal(exp(df$log_irr - 1.96 * df$se_log_irr), df$ci_lo,
                 tolerance = 1e-8)
    # The exposed window has the elevated rate, so IRR > 1.
    expect_true(df$irr > 1)
    expect_false(any(grepl("_source_value$", names(df))))
  })
})

# ---------------------------------------------------------------------------- #
# (c) The fitted IRR matches a hand-fit stratified Poisson over the SAME frame.
# ---------------------------------------------------------------------------- #

test_that("(c) the released log_irr equals a hand-fit conditional Poisson", {
  h <- sccs_irr_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(sccs_irr_opts(), {
    sccs_irr_setup(h)
    # Reconstruct the per-case two-period frame exactly as the data step does,
    # then fit the same stratified Poisson and compare the exposed coefficient.
    exp_src <- .omopOutcomeSource(h, "7777", "1")
    out_src <- .omopOutcomeSource(h, "4000002", "0")
    ctx <- list(scoped_cohort = NULL)
    cd <- .omopSccsIrrCaseData(h, ctx, exp_src, out_src, 30L)
    expect_gt(cd$n_cases, 2)
    expect_gt(cd$exposed_events, 0)
    expect_gt(cd$unexposed_events, 0)
    fit <- .omopSccsIrrFit(cd$data)
    expect_true(is.finite(fit$log_irr))

    df <- .omopAnalysisRun(h, "dsomop:sccs.incidence_rate_ratio",
                           params = IRR_PARAMS)
    # The released frame's log_irr is the in-session fit (the gate does not touch
    # the non-count estimate columns).
    expect_equal(df$log_irr, fit$log_irr, tolerance = 1e-8)
    expect_equal(df$se_log_irr, fit$se_log_irr, tolerance = 1e-8)
    # And the model family is the always-available stratified Poisson here (gnm is
    # optional / not assumed installed in CI).
    expect_true(df$model_type %in% c("stratified_poisson",
                                     "gnm_conditional_poisson"))
  })
})

# ---------------------------------------------------------------------------- #
# (d) Disclosure: non-frame guard + missing anchors fail closed / gate-empty.
# ---------------------------------------------------------------------------- #

test_that("(d) a sub-threshold case population fails closed before any fit", {
  # Only 2 exposed cases -> the self-gate (.omopDiagAssertPersons over the scoped
  # case population) errors before any per-case frame is pulled.
  h <- sccs_irr_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(sccs_irr_opts(), {
    sccs_irr_setup(h, cases = 1:2)
    DBI::dbExecute(h$conn, paste0(
      "CREATE TEMP TABLE sccs_tiny AS SELECT person_id AS subject_id, ",
      "cast(person_id AS text) AS person_token FROM person ",
      "WHERE person_id IN (1, 2)"))
    expect_error(
      .omopAnalysisRun(h, "dsomop:sccs.incidence_rate_ratio",
                       params = IRR_PARAMS, scope = "sccs_tiny"),
      "insufficient individuals|disclosure threshold")
  })
})

test_that("(d) missing exposure OR outcome concept returns a gate-safe empty frame", {
  h <- sccs_irr_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(sccs_irr_opts(), {
    sccs_irr_setup(h)
    # No exposure concept -> no risk window -> empty.
    d1 <- .omopAnalysisRun(h, "dsomop:sccs.incidence_rate_ratio",
                           params = list(outcome_concept_id = "4000002"))
    expect_equal(nrow(d1), 0L)
    # No outcome concept -> no cases -> empty.
    d2 <- .omopAnalysisRun(h, "dsomop:sccs.incidence_rate_ratio",
                           params = list(exposure_concept_id = "7777"))
    expect_equal(nrow(d2), 0L)
  })
})

test_that("(d) the IRR is NA'd in-fn when a window's banded events fall below threshold", {
  # Drive the non-frame guard directly: a fitted estimate over an exposed window
  # whose event count bands below nfilter_tab must be NA, even though the fit
  # itself succeeded. Build a frame with a healthy unexposed window but a single
  # exposed event, with enough cases to pass the person self-gate.
  h <- sccs_irr_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(sccs_irr_opts(), {
    # 6 exposed cases, but only ONE exposed-window outcome event total (on case 1)
    # and a healthy unexposed window for all -> exposed_events bands to 0 (<tab).
    DBI::dbExecute(h$conn, "DELETE FROM drug_exposure")
    for (pid in 1:6) {
      DBI::dbExecute(h$conn, sprintf(paste0(
        "INSERT INTO drug_exposure (drug_exposure_id, person_id, ",
        "drug_concept_id, drug_exposure_start_date, drug_exposure_end_date, ",
        "drug_type_concept_id) VALUES (%d, %d, 7777, '2021-01-01', ",
        "'2021-01-31', 38000177)"), 5100L + pid, pid))
    }
    DBI::dbExecute(h$conn,
      "DELETE FROM condition_occurrence WHERE condition_concept_id = 4000002")
    cid <- 9500L
    addc <- function(pid, dt) {
      cid <<- cid + 1L
      DBI::dbExecute(h$conn, sprintf(paste0(
        "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
        "condition_concept_id, condition_start_date, condition_end_date, ",
        "condition_type_concept_id) VALUES (%d, %d, 4000002, '%s', '%s', ",
        "44818518)"), cid, pid, dt, dt))
    }
    addc(1L, "2021-01-10")                       # the single exposed-window event
    for (pid in 1:6) addc(pid, "2022-06-01")     # healthy unexposed window
    df <- .omopAnalysisRun(h, "dsomop:sccs.incidence_rate_ratio",
                           params = IRR_PARAMS)
    # Only case 1 contributes to BOTH periods (others are wholly unexposed and are
    # eliminated), so n_cases collapses below nfilter.subset and the whole row is
    # dropped by the gate; if any row survives, its IRR must be NA.
    if (nrow(df) > 0) {
      expect_true(is.na(df$irr))
      expect_true(is.na(df$log_irr))
      expect_true(is.na(df$ci_lo) && is.na(df$ci_hi))
    } else {
      expect_equal(nrow(df), 0L)
    }
  })
})
