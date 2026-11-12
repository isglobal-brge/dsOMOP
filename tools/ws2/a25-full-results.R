# A-25 independent full-result replay. Source this from a focused testthat
# wrapper in a pristine package snapshot. The only external code loaded below
# is this regression's five fixture/plan helpers; no validator or executor is
# changed. See a25-replay.py for the immutable source and manifest workflow.
.a25_fixture_path <- Sys.getenv("A25_FIXTURE_FILE", "tools/ws2/a25-fixture.R")
.a25_fixture_expressions <- parse(.a25_fixture_path)
.a25_fixture_names <- c(".a25_plan", ".a25_error", ".a25_ref", ".a25_events", ".a25_filter")
for (.a25_expr in .a25_fixture_expressions) {
  if (is.call(.a25_expr) && identical(.a25_expr[[1L]], as.name("<-")) &&
      as.character(.a25_expr[[2L]]) %in% .a25_fixture_names) {
    eval(.a25_expr)
  }
}
stopifnot(all(vapply(.a25_fixture_names, exists, logical(1L), mode = "function")))

.a25_capture_all <- function() {
  records <- list()
  record <- function(name, plan, h, expected) {
    validation <- .planValidate(h, plan)
    testthat::expect_true(validation$valid, info = name)
    testthat::expect_length(validation$errors, 0L)
    testthat::expect_length(validation$warnings, 0L)
    symbols <- stats::setNames(as.list(paste0("D_", names(plan$outputs))), names(plan$outputs))
    result <- .planExecute(h, plan, symbols)
    for (output in names(expected)) {
      testthat::expect_identical(sort(result[[output]]$person_id), expected[[output]],
                                 info = paste(name, output))
    }
    # Every output field, row order, value, R type and attribute is retained.
    # Direct resolver evidence below supplements, never replaces, this call.
    records[[name]] <<- list(plan = plan, validation = validation[c("valid", "errors", "warnings")],
                            result = result)
  }
  withr::local_options(list(nfilter.subset = 3, nfilter.tab = 1))
  persons <- c(1L, 3L, 5L, 7L, 9L, 11L)
  local({
    h <- create_test_handle()
    on.exit(cleanup_handle(h))
    ref <- .a25_ref(h)
    original <- DBI::dbReadTable(h$conn, ref)
    record("criteria_local", .a25_plan(ref, siblings = TRUE), h,
           list(demo = persons, sibling = 1:15))
    record("criteria_padded", .a25_plan(paste0(" \t", ref, " \n"), siblings = TRUE), h,
           list(demo = persons, sibling = 1:15))
    testthat::expect_identical(DBI::dbReadTable(h$conn, ref), original)
    testthat::expect_true(ref %in% h$temp_tables)
    for (reference in list(1L, "1", "01")) {
      record(paste0("numeric_", typeof(reference), "_", reference), .a25_plan(reference), h,
             list(demo = persons))
    }
  })
  for (policy in c("first_episode", "last_episode", "any_episode")) local({
    h <- create_test_handle()
    on.exit(cleanup_handle(h))
    .a25_events(h)
    DBI::dbExecute(h$conn, paste0("DELETE FROM condition_occurrence WHERE ",
      "(person_id IN (1,3,5) AND condition_start_date = '2022-06-01') OR ",
      "(person_id IN (7,9,11) AND condition_start_date = '2021-06-01')"))
    DBI::dbExecute(h$conn, "UPDATE cohort SET cohort_start_date = '2021-06-01' WHERE cohort_definition_id = 10")
    DBI::dbExecute(h$conn, paste0("INSERT INTO cohort SELECT cohort_definition_id, subject_id, ",
      "'2022-06-01', cohort_end_date FROM cohort WHERE cohort_definition_id = 10"))
    ref <- .a25_ref(h)
    plan <- .a25_plan(ref, siblings = TRUE)
    plan$scope <- list(cohort = 10L)
    plan$populations$p$filter_tree <- .a25_filter()
    plan$populations$p$episode_policy <- policy
    expected <- switch(policy, first_episode = c(1L, 3L, 5L),
                       last_episode = c(7L, 9L, 11L), any_episode = persons)
    record(paste0("criteria_scope_", policy), plan, h,
           list(demo = rep(expected, each = 2L), sibling = rep(1:12, each = 2L)))
    # The criteria roster uses observation-period dates, not either named
    # cohort dates or the two filter-anchor dates.
    scope <- .planResolveScopeCohort(h, plan)
    resolved <- .planResolvePopulations(h, plan, .buildBlueprint(h),
                                       index_cohort_table = scope)$p
    episodes <- DBI::dbGetQuery(h$conn, paste0("SELECT subject_id, cohort_start_date, ",
      "cohort_end_date FROM ", resolved$cohort_table, " ORDER BY subject_id"))
    testthat::expect_identical(episodes$subject_id, expected)
    testthat::expect_identical(episodes$cohort_start_date, rep("2020-01-01", length(expected)))
    testthat::expect_identical(episodes$cohort_end_date, rep("2024-12-31", length(expected)))
    records[[paste0("criteria_scope_", policy)]]$episodes <<- episodes
  })
  local({
    h <- create_test_handle()
    on.exit(cleanup_handle(h))
    .a25_events(h)
    ref <- .a25_ref(h)
    plan <- .a25_plan(ref, index = TRUE, siblings = TRUE)
    plan$populations$p$filter_tree <- .a25_filter()
    record("index_recurrent", plan, h,
           list(demo = rep(persons, each = 2L), sibling = 1:15))
    resolved <- .planResolvePopulations(h, plan, .buildBlueprint(h))$p
    episodes <- DBI::dbGetQuery(h$conn, paste0("SELECT subject_id, cohort_start_date, ",
      "cohort_end_date, index_event_id FROM ", resolved$cohort_table,
      " ORDER BY subject_id, cohort_start_date"))
    testthat::expect_identical(episodes$subject_id, rep(persons, each = 2L))
    testthat::expect_identical(episodes$cohort_start_date, rep(c("2021-06-01", "2022-06-01"), 6L))
    testthat::expect_identical(episodes$cohort_end_date, rep("2024-12-31", 12L))
    testthat::expect_identical(episodes$index_event_id,
      as.integer(rep(persons * 10L, each = 2L) + rep(1:2, 6L)))
    records$index_recurrent$episodes <<- episodes
  })
  records
}

testthat::test_that("A-25 pristine source full results and episode evidence reproduce exactly", {
  records <- .a25_capture_all()
  testthat::expect_length(records, 9L)
  baseline <- Sys.getenv("A25_BASELINE_RDS")
  if (nzchar(baseline)) {
    prior <- readRDS(baseline)
    testthat::expect_identical(names(records), names(prior))
    for (name in names(records)) {
      testthat::expect_identical(records[[name]], prior[[name]], info = name)
    }
  }
  target <- Sys.getenv("A25_OUTPUT_RDS")
  if (nzchar(target)) saveRDS(records, target, version = 2L)
  cat("\nA25_FULL_RESULTS ")
  dput(records)
})
