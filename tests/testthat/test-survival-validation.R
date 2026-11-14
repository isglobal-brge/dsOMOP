# Defect F-6 (design-2.10.3 section 8): the retired .extractSurvival() was dead
# for plan dispatch; its validation tests run on the v1 survival adapter
# (.compilePlanSurvivalSql(), legacy = TRUE single-outcome shape), the
# executor every v1 survival plan uses.

.v1_survival <- function(handle, cohort, output) {
  .executeLongitudinalSurvivalSql(handle, .compilePlanSurvivalSql(handle, cohort, output))
}

test_that("the v1 survival adapter validates raw plan semantics before extracting", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  cohort <- .cohortCreate(
    handle,
    list(type = "condition", concept_set = 201820L),
    mode = "temporary", cohort_id = 951L
  )
  outcome <- list(table = "condition_occurrence", concept_set = 201820L)

  expect_error(.v1_survival(handle, cohort, list(outcome = outcome, event_order = "nearest")),
               "event_order must be")
  expect_error(.v1_survival(handle, cohort, list(outcome = outcome,
                                                 tar = list(start_offset = 0.5, end_offset = 30))),
               "start_offset.*exact integer")
  expect_error(.v1_survival(handle, cohort, list(outcome = outcome,
                                                 tar = list(start_offset = 30, end_offset = 0))),
               "must not be before")
  expect_error(.v1_survival(handle, cohort, list(outcome = outcome,
                                                 tar = list(start_offset = 0, ignored = TRUE))),
               "Unknown TAR")
  # the v1 adapter keeps its mandatory cohort-end cap (defect A-E12, v1 side)
  expect_error(.v1_survival(handle, cohort, list(outcome = outcome,
                                                 censoring = list(cohort_end = FALSE))),
               "cohort_end must remain TRUE")
  expect_error(.v1_survival(handle, cohort,
                            list(outcome = list(table = "condition_occurrence",
                                                concept_set = integer(0)))),
               "resolved to no concepts")
  compiled <- .compilePlanSurvivalSql(handle, cohort, list(outcome = outcome))
  expect_true(compiled$legacy)
  expect_true(compiled$semantics$censoring$cohort_end)
  expect_false(compiled$semantics$censoring$death)              # death opt-in for v1 (A-14)
})

test_that("the v1 survival adapter keeps invalid cohort episode dates out of the risk set", {
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
  # an end before the start is a private eligibility rule, never an
  # observable data-dependent failure
  out <- .v1_survival(handle, bad, list(outcome = list(table = "condition_occurrence",
                                                       concept_set = 201820L)))
  expect_identical(nrow(out), 0L)
  expect_identical(names(out), c("row_id", "cohort_row_id", "person_id", "event", "time_to_event_days"))
})
