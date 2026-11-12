# Regression tests for the small-support findings G-04 (OHDSI reader statistic
# support) and G-05 (cohort overlap union, MDRR next to a suppressed arm) of
# the dsOMOP 2.10.3 OHDSI-tools audit. The fixtures reproduce the audit's
# attacks (design/2.10.5/astra-scratch/small-support/sccs-overlap.R and
# exp4_mdrr.R) through the real reader, catalog run path and DataSHIELD method.

ss_opts <- function(...) {
  utils::modifyList(list(nfilter.subset = 3, nfilter.tab = 3,
                         dsomop.nfilter.band = 5, dsomop.nfilter.dist = 10,
                         dsomop.query_strict = TRUE), list(...))
}

ss_stat_cols <- c("rr", "ci_95_lb", "ci_95_ub", "p", "log_rr", "se_log_rr")

# Fixture sccs_result (4 rows) with the upstream exposed-case basis columns.
# `values` assigns one column to all rows; `per_row` assigns a column row by row
# in rowid order (NA becomes NULL).
ss_sccs_handle <- function(values = list(outcome_subjects = 20L,
                                         outcome_events = 100L,
                                         covariate_subjects = 2L,
                                         covariate_outcomes = 30L),
                           per_row = list(), add_columns = TRUE,
                           n_persons = 15) {
  h <- create_test_handle(n_persons = n_persons)
  if (add_columns) {
    DBI::dbExecute(h$conn,
                   "ALTER TABLE sccs_result ADD COLUMN covariate_subjects INTEGER")
    DBI::dbExecute(h$conn,
                   "ALTER TABLE sccs_result ADD COLUMN covariate_outcomes INTEGER")
  }
  literal <- function(v) if (is.na(v)) "NULL" else as.character(v)
  if (length(values) > 0L) {
    sets <- paste(sprintf("%s = %s", names(values),
                          vapply(values, literal, character(1))),
                  collapse = ", ")
    DBI::dbExecute(h$conn, paste("UPDATE sccs_result SET", sets))
  }
  for (column in names(per_row)) {
    cases <- paste(sprintf("WHEN %d THEN %s", seq_along(per_row[[column]]),
                           vapply(per_row[[column]], literal, character(1))),
                   collapse = " ")
    DBI::dbExecute(h$conn, sprintf(
      "UPDATE sccs_result SET %s = CASE rowid %s END", column, cases))
  }
  .buildBlueprint(h, force = TRUE)
  # Pre-build the catalog once, swallowing the expected build-time dependency
  # warning of the minimal fixture (asserted in test-analysis-catalog.R).
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

ss_finite_stats <- function(df) {
  cols <- intersect(ss_stat_cols, names(df))
  if (length(cols) == 0L || nrow(df) == 0L) return(0L)
  as.integer(sum(vapply(cols, function(col) {
    sum(is.finite(suppressWarnings(as.numeric(df[[col]]))))
  }, numeric(1))))
}

# --- G-04: SCCS reader ---------------------------------------------------------

test_that("G-04: 2 exposed cases of 20 release no finite SCCS statistic", {
  h <- ss_sccs_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(ss_opts(), {
    r <- .ohdsiGetResults(h, "sccs_result", tool_id = "sccs")
    # The rows and their own supported outcome counts survive...
    expect_equal(nrow(r), 4L)
    expect_true(all(r$outcome_subjects == 20))
    expect_true(all(r$outcome_events == 100))
    # ...but the coupled effect family is masked together, on every row.
    expect_true(all(ss_stat_cols %in% names(r)))
    expect_identical(ss_finite_stats(r), 0L)
    for (col in ss_stat_cols) expect_true(all(is.na(r[[col]])))
    # The private basis is read but never released.
    expect_false(any(c("covariate_subjects", "covariate_outcomes") %in%
                       names(r)))
  })
})

test_that("G-04: a caller selecting only rr cannot drop the hidden basis", {
  h <- ss_sccs_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(ss_opts(), {
    r <- .ohdsiGetResults(h, "sccs_result", columns = "rr", tool_id = "sccs")
    expect_identical(names(r), "rr")
    expect_equal(nrow(r), 4L)
    expect_true(all(is.na(r$rr)))
    r2 <- .ohdsiGetResults(h, "sccs_result",
                           columns = c("covariate_id", "rr", "se_log_rr"))
    expect_identical(names(r2), c("covariate_id", "rr", "se_log_rr"))
    expect_identical(ss_finite_stats(r2), 0L)
    # The hidden basis is not a public column and cannot be requested.
    expect_error(
      .ohdsiGetResults(h, "sccs_result", columns = c("rr", "covariate_subjects")),
      "no public disclosure contract"
    )
  })
})

test_that("G-04: a physical sccs_result without the exposed-case basis masks", {
  h <- ss_sccs_handle(values = list(), add_columns = FALSE)
  on.exit(cleanup_handle(h))
  withr::with_options(ss_opts(), {
    r <- .ohdsiGetResults(h, "sccs_result")
    # Old schemas are not a table-wide failure: rows and supported outcome
    # counts stay, the statistics are stable NA (never an exact fallback).
    expect_equal(nrow(r), 4L)
    expect_true(all(r$outcome_subjects %% 5 == 0))
    expect_identical(ss_finite_stats(r), 0L)
  })
})

test_that("G-04: supported exposed cases keep the reviewed statistics", {
  h <- ss_sccs_handle(values = list(outcome_subjects = 120L,
                                    outcome_events = 180L,
                                    covariate_subjects = 50L,
                                    covariate_outcomes = 60L))
  on.exit(cleanup_handle(h))
  withr::with_options(ss_opts(), {
    r <- .ohdsiGetResults(h, "sccs_result")
    expect_equal(nrow(r), 4L)
    expect_equal(r$rr, c(2.50, 1.85, 1.20, 3.10))
    expect_equal(r$se_log_rr, c(0.167, 0.200, 0.147, 0.199))
    expect_true(all(r$outcome_subjects == 120))
    expect_false("covariate_subjects" %in% names(r))
  })
})

test_that("G-04: exposed-case support edges 0, 1, 2, threshold, NA", {
  h <- ss_sccs_handle(
    values = list(outcome_subjects = 20L, outcome_events = 100L,
                  covariate_outcomes = 30L),
    per_row = list(covariate_subjects = c(0L, 1L, 2L, 3L)))
  on.exit(cleanup_handle(h))
  withr::with_options(ss_opts(), {
    r <- .ohdsiGetResults(h, "sccs_result")
    expect_equal(nrow(r), 4L)
    finite <- is.finite(r$rr)
    expect_identical(finite, c(FALSE, FALSE, FALSE, TRUE))
    # Every member of the family is masked together with rr.
    for (col in ss_stat_cols) {
      expect_identical(is.finite(r[[col]]), finite, info = col)
    }
  })
  DBI::dbExecute(h$conn,
                 "UPDATE sccs_result SET covariate_subjects = NULL WHERE rowid = 4")
  withr::with_options(ss_opts(), {
    r <- .ohdsiGetResults(h, "sccs_result")
    expect_identical(ss_finite_stats(r), 0L)
  })
  # A custodian threshold above nfilter.tab applies to the exposed cases too.
  DBI::dbExecute(h$conn, paste(
    "UPDATE sccs_result SET covariate_subjects =",
    "CASE rowid WHEN 1 THEN 4 WHEN 2 THEN 5 WHEN 3 THEN 6 ELSE 3 END"))
  withr::with_options(ss_opts(nfilter.subset = 5), {
    r <- .ohdsiGetResults(h, "sccs_result")
    expect_identical(is.finite(r$rr), c(FALSE, TRUE, TRUE, FALSE))
  })
})

test_that("G-04: exposed and unexposed event floors gate the IRR family", {
  # 20 exposed cases but only 2 outcomes during exposed time, or only 2
  # outcomes outside it: either side of the ratio rests on 1-2 events.
  h <- ss_sccs_handle(
    values = list(outcome_subjects = 20L, covariate_subjects = 20L),
    per_row = list(covariate_outcomes = c(2L, 30L, 98L, 30L),
                   outcome_events = c(100L, 100L, 100L, 33L)))
  on.exit(cleanup_handle(h))
  withr::with_options(ss_opts(), {
    r <- .ohdsiGetResults(h, "sccs_result")
    expect_equal(nrow(r), 4L)
    # row 1: 2 exposed events; row 3: 2 unexposed events; row 4: 3 unexposed.
    expect_identical(is.finite(r$rr), c(FALSE, TRUE, FALSE, TRUE))
    # The recurrent exposed-event count is never released as a person count.
    expect_false("covariate_outcomes" %in% names(r))
  })
})

test_that("G-04: prefixed physical SCCS tables use the same statistic gate", {
  h <- ss_sccs_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn,
                 "CREATE TABLE sccs_sccs_result AS SELECT * FROM sccs_result")
  withr::with_options(ss_opts(), {
    r <- .ohdsiGetResults(h, "sccs_sccs_result", tool_id = "sccs")
    expect_equal(nrow(r), 4L)
    expect_identical(ss_finite_stats(r), 0L)
    expect_false("covariate_subjects" %in% names(r))
    r_rr <- .ohdsiGetResults(h, "sccs_sccs_result", columns = "rr")
    expect_true(all(is.na(r_rr$rr)))
  })
})

test_that("G-04: the DataSHIELD reader method and every alias are gated", {
  h <- ss_sccs_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn,
                 "CREATE TABLE sccs_sccs_result AS SELECT * FROM sccs_result")
  symbol <- paste0("ss_sccs_", Sys.getpid())
  .setHandle(symbol, h)
  on.exit(.removeHandle(symbol), add = TRUE)
  withr::with_options(ss_opts(), {
    calls <- list(
      omopOhdsiResultsDS(symbol, "sccs_result"),
      omopOhdsiResultsDS(symbol, "sccs_result", tool_id = "sccs"),
      omopOhdsiResultsDS(symbol, "sccs_result", columns = "rr"),
      omopOhdsiResultsDS(symbol, "sccs_result",
                         columns = c("analysis_id", "rr", "p")),
      omopOhdsiResultsDS(symbol, "sccs_sccs_result", tool_id = "sccs"),
      omopOhdsiResultsDS(symbol, "sccs_result",
                         filters = list(covariate_id = 100L)),
      omopOhdsiResultsDS(symbol, "sccs_result", order_by = "covariate_id DESC")
    )
    for (r in calls) {
      expect_gt(nrow(r), 0L)
      expect_identical(ss_finite_stats(r), 0L)
      expect_false(any(c("covariate_subjects", "covariate_outcomes") %in%
                         names(r)))
    }
  })
  # The analysis-catalog ids of the effect tables are live ports, not
  # precomputed readers; any precomputed reader that remained would funnel
  # through the same gated .ohdsiGetResults.
  withr::with_options(ss_opts(), {
    for (id in c("dsomop:ohdsi.sccs.sccs_result",
                 "dsomop:ohdsi.cohort_method.cm_result",
                 "dsomop:ohdsi.plp.plp_performances")) {
      entry <- .omopAnalysisResolve(h, id)
      if (isTRUE(entry$meta$precomputed)) {
        r <- .omopAnalysisRun(h, id)
        expect_identical(ss_finite_stats(r), 0L, info = id)
      } else {
        expect_false(identical(entry$meta$adapter %||% "", "ohdsi"),
                     info = id)
      }
    }
  })
})

test_that("G-04: non-strict administrator reads also mask the statistics", {
  h <- ss_sccs_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(ss_opts(dsomop.query_strict = FALSE), {
    # The development heuristic treats the small exposed-case column as a
    # count and drops the rows; either way no statistic survives.
    r <- .ohdsiGetResults(h, "sccs_result", tool_id = "sccs")
    expect_identical(ss_finite_stats(r), 0L)
  })
  # Rows whose counts all clear the cell floor still get the statistic gate:
  # 20 exposed cases, but only 2 outcomes outside exposed time.
  DBI::dbExecute(h$conn, paste(
    "UPDATE sccs_result SET covariate_subjects = 20, covariate_outcomes = 30,",
    "outcome_events = 32"))
  withr::with_options(ss_opts(dsomop.query_strict = FALSE), {
    r <- .ohdsiGetResults(h, "sccs_result", tool_id = "sccs")
    expect_equal(nrow(r), 4L)
    expect_identical(ss_finite_stats(r), 0L)
    expect_false(isTRUE(attr(r, "dsomop.disclosure_safe")))
  })
})

test_that("G-04: the statistic gate masks families on raw values only", {
  withr::local_options(ss_opts())
  basis <- .ohdsiReviewedContracts()$sccs$sccs_result$statistic_basis
  expect_setequal(basis$person, c("outcome_subjects", "covariate_subjects"))
  expect_identical(basis$event, "covariate_outcomes")
  expect_setequal(basis$statistics, ss_stat_cols)
  rows <- data.frame(
    rr = c(2, 2, 2, 2, 2), p = c(0.1, 0.1, 0.1, 0.1, 0.1),
    outcome_subjects = c(20, 20, 20, 20, Inf),
    outcome_events = c(100, 100, 100, 100, 100),
    covariate_subjects = c(3, 2, -3, NA, 20),
    covariate_outcomes = c(30, 30, 30, 30, 30)
  )
  gated <- .ohdsiStatisticGate(rows, basis)
  expect_identical(is.finite(gated$rr), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(is.finite(gated$p), is.finite(gated$rr))
  # Basis and dimension columns are untouched; no failure marker is added.
  expect_identical(gated$covariate_subjects, rows$covariate_subjects)
  expect_identical(names(gated), names(rows))
  # A missing basis column fails every row.
  expect_identical(
    ss_finite_stats(.ohdsiStatisticGate(rows[, -5], basis)), 0L)
  # No basis contract: unchanged.
  expect_identical(.ohdsiStatisticGate(rows, NULL), rows)
})

# --- G-04: CohortMethod and PLP effect/performance tables ------------------------

test_that("G-04: cm_result effect statistics need both arms' bases", {
  withr::local_options(ss_opts())
  basis <- .ohdsiReviewedContracts()$cohort_method$cm_result$statistic_basis
  expect_setequal(basis$person, c("target_subjects", "comparator_subjects"))
  expect_setequal(basis$event, c("target_outcomes", "comparator_outcomes"))
  rows <- data.frame(
    rr = rep(0.8, 4), se_log_rr = rep(0.1, 4),
    target_subjects = c(500, 500, 500, 500),
    comparator_subjects = c(480, 2, 480, 480),
    target_outcomes = c(45, 45, 2, 45),
    comparator_outcomes = c(52, 52, 52, NA)
  )
  gated <- .ohdsiStatisticGate(rows, basis)
  expect_identical(is.finite(gated$rr), c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(is.finite(gated$se_log_rr), is.finite(gated$rr))

  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  .buildBlueprint(h)
  r <- .ohdsiGetResults(h, "cm_result")
  expect_equal(nrow(r), 4L)
  expect_equal(r$rr, c(0.85, 1.15, 0.72, 1.05))
  # An arm below the person threshold leaves no released statistic behind.
  DBI::dbExecute(h$conn,
                 "UPDATE cm_result SET comparator_subjects = 2 WHERE rowid = 1")
  r <- .ohdsiGetResults(h, "cm_result")
  expect_false(any(r$rr %in% 0.85))
  expect_true(all(r$comparator_subjects %% 5 == 0))
})

test_that("G-04: plp_performances scalars need both outcome classes", {
  withr::local_options(ss_opts())
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  # Row 1: 150 outcomes in a population of 152 -> 2 non-outcome persons.
  DBI::dbExecute(h$conn, paste(
    "UPDATE plp_performances SET population_size = 152",
    "WHERE rowid = 1"))
  .buildBlueprint(h, force = TRUE)
  r <- .ohdsiGetResults(h, "plp_performances")
  expect_equal(nrow(r), 4L)
  scalars <- c("auc", "auprc", "calibration_in_large",
               "calibration_intercept", "calibration_slope")
  for (col in scalars) {
    expect_identical(is.finite(r[[col]]), c(FALSE, TRUE, TRUE, TRUE),
                     info = col)
  }
  expect_equal(r$auc[2:4], c(0.78, 0.85, 0.75))
  r_auc <- .ohdsiGetResults(h, "plp_performances", columns = "auc")
  expect_identical(is.finite(r_auc$auc), c(FALSE, TRUE, TRUE, TRUE))
})

# --- G-05: cohort overlap -------------------------------------------------------------

ss_overlap_handle <- function(n_persons = 200) {
  h <- create_test_handle(n_persons = n_persons)
  .buildBlueprint(h)
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

ss_temp_cohort <- function(h, name, ids, start = "2020-01-01",
                           end = "2023-12-31") {
  DBI::dbExecute(h$conn, sprintf(paste(
    "CREATE TEMP TABLE %s AS SELECT person_id AS subject_id,",
    "'%s' AS cohort_start_date, '%s' AS cohort_end_date",
    "FROM person WHERE person_id IN (%s)"),
    name, start, end, paste(ids, collapse = ",")))
  register_test_temp(h, name)
  name
}

# Every (a_only, b_only, both) consistent with one released overlap response:
# a released region lies in its band [n, n + band - 1]; an absent region was
# suppressed (0 .. nfilter.tab - 1). Any released union would add a sum
# constraint; none may be present.
ss_overlap_candidates <- function(df, band = 5L, tab = 3L, max_n = 200L) {
  released <- stats::setNames(df$n, df$category)
  range_of <- function(region) {
    if (region %in% names(released)) {
      seq.int(released[[region]], released[[region]] + band - 1L)
    } else {
      seq.int(0L, tab - 1L)
    }
  }
  grid <- expand.grid(a_only = range_of("a_only"), b_only = range_of("b_only"),
                      both = range_of("both"))
  if ("either" %in% names(released)) {
    union <- grid$a_only + grid$b_only + grid$both
    grid <- grid[union >= released[["either"]] &
                   union <= released[["either"]] + band - 1L, , drop = FALSE]
  }
  grid
}

test_that("G-05: 54 A-only / 54 B-only / 2 both cannot be solved", {
  h <- ss_overlap_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(ss_opts(), {
    a <- ss_temp_cohort(h, "ss_ov_a", 1:56)
    b <- ss_temp_cohort(h, "ss_ov_b", c(55:56, 57:110))
    r <- .omopAnalysisRun(h, "dsomop:cohortdx.cohort_overlap",
                          scope = list(a, b))
    expect_true(all(r$category %in% c("both", "a_only", "b_only")))
    expect_false("either" %in% r$category)
    got <- stats::setNames(r$n, r$category)
    expect_equal(unname(got[["a_only"]]), 50)
    expect_equal(unname(got[["b_only"]]), 50)
    expect_false("both" %in% r$category)
    # The audit's inverse (either - a_only - b_only) has nothing to work on:
    # the suppressed region stays ambiguous over all of 0..2.
    cand <- ss_overlap_candidates(r)
    expect_setequal(unique(cand$both), 0:2)
  })
})

test_that("G-05: overlap edge cases 0/1/2, permutations and full support", {
  h <- ss_overlap_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(ss_opts(), {
    run <- function(a_ids, b_ids, tag) {
      a <- ss_temp_cohort(h, paste0("ss_e_a_", tag), a_ids)
      b <- ss_temp_cohort(h, paste0("ss_e_b_", tag), b_ids)
      .omopAnalysisRun(h, "dsomop:cohortdx.cohort_overlap", scope = list(a, b))
    }
    for (k in 0:2) {
      shared <- seq_len(k)
      r <- run(c(shared, 101:120), c(shared, 121:140), paste0("k", k))
      expect_setequal(r$category, c("a_only", "b_only"))
      expect_false("either" %in% r$category)
      expect_identical(nrow(ss_overlap_candidates(r)) > 1L, TRUE)
      expect_setequal(unique(ss_overlap_candidates(r)$both), 0:2)
    }
    # Swapping the arms swaps the one-sided regions, nothing else.
    r1 <- run(c(1:30), c(21:50, 151:155), "p1")
    r2 <- run(c(21:50, 151:155), c(1:30), "p2")
    g1 <- stats::setNames(r1$n, r1$category)
    g2 <- stats::setNames(r2$n, r2$category)
    expect_equal(g1[["a_only"]], g2[["b_only"]])
    expect_equal(g1[["b_only"]], g2[["a_only"]])
    expect_equal(g1[["both"]], g2[["both"]])
    # All three regions supported: exactly three disjoint rows, no union.
    r <- run(1:40, 21:60, "full")
    expect_setequal(r$category, c("both", "a_only", "b_only"))
    expect_true(all(r$n %% 5 == 0))
  })
})

# --- G-05: MDRR ----------------------------------------------------------------------

ss_mdrr_handle <- function(target_cases, comparator_cases,
                           events_per_case = 1L) {
  h <- ss_overlap_handle(n_persons = 200)
  DBI::dbExecute(h$conn,
                 "DELETE FROM condition_occurrence WHERE condition_concept_id = 4000002")
  ss_temp_cohort(h, "ss_m_t", 1:100)
  ss_temp_cohort(h, "ss_m_c", 101:200)
  id <- 900000L
  for (person in c(target_cases, comparator_cases)) {
    for (k in seq_len(events_per_case)) {
      id <- id + 1L
      day <- sprintf("2021-03-%02d", k)
      DBI::dbExecute(h$conn, sprintf(paste(
        "INSERT INTO condition_occurrence (condition_occurrence_id, person_id,",
        "condition_concept_id, condition_start_date, condition_end_date,",
        "condition_type_concept_id) VALUES (%d, %d, 4000002, '%s', '%s',",
        "44818518)"), id, person, day, day))
    }
  }
  h
}

ss_mdrr_formula <- function(D, td, cd, alpha = 0.05, power = 0.8) {
  p <- td / (td + cd)
  exp((stats::qnorm(1 - alpha / 2) + stats::qnorm(power)) *
        sqrt(1 / (D * p * (1 - p))))
}

ss_mdrr_params <- list(outcome_concept_id = "4000002")

test_that("G-05: an arm with 2 outcome persons leaves no MDRR behind", {
  h <- ss_mdrr_handle(1:13, c(150, 160))
  on.exit(cleanup_handle(h))
  withr::with_options(ss_opts(), {
    for (id in c("dsomop:cm.mdrr", "dsomop:ohdsi.cohort_method.cm_result")) {
      r <- .omopAnalysisRun(h, id, ss_mdrr_params,
                            scope = list("ss_m_t", "ss_m_c"))
      # The comparator arm (2 outcome persons) is suppressed...
      expect_identical(r$arm, "target", info = id)
      expect_equal(r$outcomes, 10, info = id)
      # ...and the surviving target row no longer carries an MDRR built from
      # the total (which bounded the hidden arm to 1-2 outcome persons).
      expect_true(all(is.na(r$mdrr)), info = id)
    }
    diag <- .omopAnalysisRun(h, "dsomop:cm.diagnostics_summary",
                             ss_mdrr_params, scope = list("ss_m_t", "ss_m_c"))
    expect_equal(nrow(diag), 1L)
    expect_true(is.na(diag$mdrr))
  })
})

test_that("G-05: recurrent events of 2 comparator persons give no MDRR", {
  h <- ss_mdrr_handle(1:13, c(150, 160), events_per_case = 10L)
  on.exit(cleanup_handle(h))
  withr::with_options(ss_opts(), {
    r <- .omopAnalysisRun(h, "dsomop:cm.mdrr", ss_mdrr_params,
                          scope = list("ss_m_t", "ss_m_c"))
    expect_identical(r$arm, "target")
    expect_true(all(is.na(r$mdrr)))
  })
})

test_that("G-05: supported 13 + 7 uses the released 10 + 5, not band(20)", {
  h <- ss_mdrr_handle(1:13, 150:156)
  on.exit(cleanup_handle(h))
  withr::with_options(ss_opts(), {
    r <- .omopAnalysisRun(h, "dsomop:cm.mdrr", ss_mdrr_params,
                          scope = list("ss_m_t", "ss_m_c"))
    expect_setequal(r$arm, c("target", "comparator"))
    t <- r[r$arm == "target", ]
    c <- r[r$arm == "comparator", ]
    expect_equal(c(t$outcomes, c$outcomes), c(10, 5))
    expect_true(all(r$person_days %% 5 == 0))
    expected <- ss_mdrr_formula(t$outcomes + c$outcomes, t$person_days,
                                c$person_days)
    expect_equal(t$mdrr, expected)
    expect_equal(c$mdrr, expected)
    # Not the MDRR of the banded raw total (20), whose carry bit leaks.
    expect_false(isTRUE(all.equal(
      t$mdrr, ss_mdrr_formula(20, t$person_days, c$person_days))))
    # The exact inverse recovers only the released total outcomes.
    K <- ((stats::qnorm(0.975) + stats::qnorm(0.8)) / log(t$mdrr))^2
    p <- t$person_days / (t$person_days + c$person_days)
    expect_equal(K / (p * (1 - p)), t$outcomes + c$outcomes)
    # The legacy alias and the diagnostics summary share the value.
    alias <- .omopAnalysisRun(h, "dsomop:ohdsi.cohort_method.cm_result",
                              ss_mdrr_params, scope = list("ss_m_t", "ss_m_c"))
    expect_equal(unique(alias$mdrr), expected)
    diag <- .omopAnalysisRun(h, "dsomop:cm.diagnostics_summary",
                             ss_mdrr_params, scope = list("ss_m_t", "ss_m_c"))
    expect_equal(diag$mdrr, expected)
  })
})

test_that("G-05: each arm's bases are checked separately", {
  withr::local_options(ss_opts())
  arm <- function(persons = 100, person_days = 145900, outcomes = 13,
                  outcome_persons = 13) {
    list(persons = persons, person_days = person_days, outcomes = outcomes,
         outcome_persons = outcome_persons)
  }
  ok <- .omopCmReleasableArmStats(arm())
  expect_true(ok$releasable)
  expect_equal(c(ok$persons, ok$person_days, ok$outcomes), c(100, 145900, 10))
  for (bad in list(arm(persons = 2), arm(person_days = NA), arm(person_days = 0),
                   arm(person_days = 2), arm(outcomes = NA),
                   arm(outcome_persons = 2), arm(outcome_persons = NA),
                   arm(persons = Inf), arm(outcomes = -1), list())) {
    stats <- .omopCmReleasableArmStats(bad)
    expect_false(stats$releasable)
    expect_true(all(is.na(c(stats$persons, stats$person_days,
                            stats$outcomes))))
  }
  expect_true(is.na(.omopCmReleasedMdrr(arm(), arm(outcome_persons = 2))))
  expect_true(is.na(.omopCmReleasedMdrr(arm(persons = 1), arm())))
  expect_equal(.omopCmReleasedMdrr(arm(), arm(outcomes = 7, outcome_persons = 7)),
               ss_mdrr_formula(15, 145900, 145900))
  # A small arm population under a custodian subset threshold above
  # nfilter.tab fails the MDRR although its row may still clear nfilter.tab.
  withr::local_options(list(nfilter.subset = 5))
  expect_false(.omopCmReleasableArmStats(arm(persons = 4))$releasable)
})
