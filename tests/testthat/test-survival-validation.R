test_that("survival validates raw plan semantics before extracting", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  cohort <- .cohortCreate(
    handle,
    list(type = "condition", concept_set = 201820L),
    mode = "temporary", cohort_id = 951L
  )
  outcome <- list(table = "condition_occurrence", concept_set = 201820L)

  expect_error(
    .extractSurvival(handle, cohort, outcome, event_order = "nearest"),
    "event_order must be"
  )
  expect_error(
    .extractSurvival(handle, cohort, outcome,
                     tar = list(start_offset = 0.5, end_offset = 30)),
    "start_offset.*exact integer"
  )
  expect_error(
    .extractSurvival(handle, cohort, outcome,
                     tar = list(start_offset = 30, end_offset = 0)),
    "must not be after"
  )
  expect_error(
    .extractSurvival(handle, cohort, outcome,
                     tar = list(start_offset = 0, ignored = TRUE)),
    "Unknown TAR"
  )
  expect_error(
    .extractSurvival(handle, cohort, outcome,
                     tar = list(censoring = "observation_period_end")),
    "only 'cohort_end'"
  )
  expect_error(
    .extractSurvival(handle, cohort,
                     list(table = "condition_occurrence",
                          concept_set = integer(0))),
    "resolved to no concepts"
  )
})

test_that("survival rejects invalid cohort episode dates", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  source <- .cohortCreate(
    handle,
    list(type = "condition", concept_set = 201820L),
    mode = "temporary", cohort_id = 952L
  )
  bad <- .createTempTable(handle, "dsomop_bad_survival_dates", paste0(
    "SELECT subject_id, cohort_start_date, ",
    "DATE(cohort_start_date, '-1 day') AS cohort_end_date FROM ", source
  ))
  expect_error(
    .extractSurvival(
      handle, bad,
      list(table = "condition_occurrence", concept_set = 201820L)
    ),
    "end >= start"
  )
})
