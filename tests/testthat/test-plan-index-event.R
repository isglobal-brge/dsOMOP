.ie_handle <- function() {
  testthat::skip_if_not_installed("RSQLite")
  h <- create_test_handle(n_persons = 1L)
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  DBI::dbExecute(h$conn, paste0(
    "INSERT INTO condition_occurrence ",
    "(condition_occurrence_id, person_id, condition_concept_id, ",
    "condition_start_date, condition_end_date, condition_type_concept_id, ",
    "visit_occurrence_id) VALUES ",
    "(101, 1, 201820, '2020-01-15', NULL, 44818518, NULL), ",
    "(102, 1, 201820, '2020-07-01', '2020-07-02', 44818518, NULL), ",
    "(103, 1, 255573, '2020-07-10', '2020-07-10', 44818518, NULL)"
  ))
  .buildBlueprint(h, force = TRUE)
  h
}

.ie_population <- function(primary_limit, inclusion = FALSE,
                           end_strategy = NULL) {
  pop <- list(
    id = "study", kind = "criteria",
    index_event = list(
      table = "condition_occurrence",
      concept_set = 201820L,
      primary_limit = primary_limit
    )
  )
  if (!is.null(end_strategy)) {
    pop$index_event$end_strategy <- end_strategy
  }
  if (isTRUE(inclusion)) {
    pop$filter_tree <- list(
      type = "has_concept",
      params = list(
        concept_id = 255573L,
        table = "condition_occurrence",
        window = list(start = 0L, end = 30L),
        min_count = 1L,
        concept_name = NULL,
        reference_date = NULL
      )
    )
  }
  pop
}

.ie_resolve <- function(handle, primary_limit, inclusion = FALSE,
                        end_strategy = NULL) {
  plan <- list(populations = list(
    study = .ie_population(primary_limit, inclusion, end_strategy)
  ))
  .planResolvePopulations(handle, plan, .buildBlueprint(handle))$study
}

.ie_rows <- function(handle, resolved) {
  .executeQuery(handle, paste0(
    "SELECT subject_id, cohort_start_date, cohort_end_date, index_event_id ",
    "FROM ", resolved$cohort_table,
    " ORDER BY cohort_start_date, index_event_id"
  ))
}

test_that("index primary First Last All materialize real recurrent OMOP events", {
  expected <- list(
    first = "2020-01-15",
    last = "2020-07-01",
    all = c("2020-01-15", "2020-07-01")
  )
  for (limit in names(expected)) {
    local({
      h <- .ie_handle()
      on.exit(cleanup_handle(h), add = TRUE)
      resolved <- withr::with_options(list(nfilter.subset = 0),
        .ie_resolve(h, limit))
      rows <- .ie_rows(h, resolved)
      expect_equal(rows$cohort_start_date, expected[[limit]])
      expect_equal(rows$index_event_id,
                   if (limit == "first") 101L else if (limit == "last") 102L
                   else c(101L, 102L))
      expect_false(any(rows$cohort_start_date == "2020-01-01"))
      if (limit %in% c("first", "all")) {
        # Circe's default exit is the end of the unique observation period that
        # contains the index, not the source event's physical end.
        expect_equal(rows$cohort_end_date[rows$index_event_id == 101L],
                     "2024-12-31")
      }
    })
  }
})

test_that("default OP exit keeps post-index outcomes inside follow-up", {
  h <- .ie_handle()
  on.exit(cleanup_handle(h))
  resolved <- withr::with_options(list(nfilter.subset = 0),
    .ie_resolve(h, "first"))

  compiled <- .compileLongitudinalSurvivalSql(
    handle = h,
    cohort_table = resolved$cohort_table,
    outcomes = list(later_condition = list(
      table = "condition_occurrence", concept_set = 255573L
    )),
    tar = list(start_offset = 0L, end_offset = 365L),
    censoring = list(observation_period_end = FALSE, death = FALSE),
    format = "survival"
  )
  result <- withr::with_options(list(nfilter.subset = 0),
    .executeLongitudinalSurvivalSql(h, compiled))

  expect_equal(result$event, 1L)
  expect_equal(result$exit_days_from_index, 177L)
})

test_that("OHDSI DateOffset selects index start or physical event end", {
  expected <- list(
    start_plus_30 = list(
      strategy = list(DateOffset = list(
        DateField = "StartDate", Offset = 30L
      )),
      dates = c("2020-02-14", "2020-07-31")
    ),
    physical_end = list(
      strategy = list(DateOffset = list(
        DateField = "EndDate", Offset = 0L
      )),
      dates = c("2020-01-15", "2020-07-02")
    ),
    capped_at_op_end = list(
      strategy = list(DateOffset = list(
        DateField = "StartDate", Offset = 9999L
      )),
      dates = c("2024-12-31", "2024-12-31")
    )
  )
  for (case in expected) {
    local({
      h <- .ie_handle()
      on.exit(cleanup_handle(h), add = TRUE)
      resolved <- withr::with_options(list(nfilter.subset = 0),
        .ie_resolve(h, "all", end_strategy = case$strategy))
      expect_equal(.ie_rows(h, resolved)$cohort_end_date, case$dates)
    })
  }
})

test_that("index events exclude unobserved candidates and reject ambiguous coverage", {
  h <- .ie_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn, paste0(
    "UPDATE observation_period SET observation_period_start_date = ",
    "'2020-06-01'"
  ))
  resolved <- withr::with_options(
    list(nfilter.subset = 0), .ie_resolve(h, "first")
  )
  expect_equal(.ie_rows(h, resolved)$index_event_id, 102L)

  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbExecute(h$conn, paste0(
    "INSERT INTO observation_period ",
    "(observation_period_id, person_id, observation_period_start_date, ",
    "observation_period_end_date, period_type_concept_id) VALUES ",
    "(1, 1, '2020-01-01', '2024-12-31', 44814724), ",
    "(2, 1, '2019-01-01', '2021-12-31', 44814724)"
  ))
  expect_error(
    withr::with_options(list(nfilter.subset = 0), .ie_resolve(h, "first")),
    "overlapping covering observation_period"
  )
})

test_that("unsupported or malformed index end strategies fail closed", {
  h <- .ie_handle()
  on.exit(cleanup_handle(h))
  malformed <- list(
    list(CustomEra = list()),
    list(DateOffset = list(DateField = "EventDate", Offset = 0L)),
    list(DateOffset = list(DateField = "EndDate", Offset = 0.5)),
    list(DateOffset = list(DateField = "EndDate"))
  )
  for (strategy in malformed) {
    expect_error(
      withr::with_options(list(nfilter.subset = 0),
        .ie_resolve(h, "first", end_strategy = strategy)),
      "end_strategy"
    )
  }
})

test_that("Primary All preserves distinct same-date source events downstream", {
  h <- .ie_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn, paste0(
    "INSERT INTO condition_occurrence ",
    "(condition_occurrence_id, person_id, condition_concept_id, ",
    "condition_start_date, condition_end_date, condition_type_concept_id, ",
    "visit_occurrence_id) VALUES ",
    "(104, 1, 201820, '2020-07-01', '2020-07-02', 44818518, NULL)"
  ))

  resolved <- withr::with_options(list(nfilter.subset = 0),
    .ie_resolve(h, "all"))
  rows <- .ie_rows(h, resolved)
  expect_equal(rows$index_event_id, c(101L, 102L, 104L))

  ranked <- .executeQuery(h, paste0(
    "SELECT cohort_row_id, subject_id, dsomop_episode_key FROM ",
    .rankedCohortSql(resolved$cohort_table, h),
    " ORDER BY cohort_row_id"
  ))
  expect_equal(ranked$cohort_row_id, 1:3)
  expect_equal(ranked$dsomop_episode_key, c(101L, 102L, 104L))

  features <- withr::with_options(list(nfilter.subset = 0),
    .extractTable(
      h, "condition_occurrence", concept_filter = 201820L,
      cohort_table = resolved$cohort_table,
      temporal = list(index_window = list(start = 0L, end = 0L)),
      representation = "features", representation_grain = "episode",
      feature_specs = list(n = list(
        type = "count", name = "n", concept_set = 201820L
      )), translate_concepts = FALSE
    ))
  expect_equal(features$cohort_row_id, 1:3)
  expect_equal(nrow(features), 3L)
})

test_that("index-relative eligibility filters episodes after the primary limit", {
  expected <- list(first = character(0), last = "2020-07-01",
                   all = "2020-07-01")
  for (limit in names(expected)) {
    local({
      h <- .ie_handle()
      on.exit(cleanup_handle(h), add = TRUE)
      resolved <- withr::with_options(list(nfilter.subset = 0),
        .ie_resolve(h, limit, inclusion = TRUE))
      rows <- .ie_rows(h, resolved)
      expect_equal(rows$cohort_start_date, expected[[limit]])
    })
  }
})

test_that("index-event populations remain subject to the disclosure gate", {
  h <- .ie_handle()
  on.exit(cleanup_handle(h))
  expect_error(
    withr::with_options(list(nfilter.subset = 2), .ie_resolve(h, "all")),
    "insufficient individuals"
  )
})

test_that("person scope preserves index-event episode dates and recurrence", {
  h <- .ie_handle()
  on.exit(cleanup_handle(h))
  bp <- .buildBlueprint(h)
  resolved <- withr::with_options(list(nfilter.subset = 0),
    list(study = .ie_resolve(h, "all")))
  scope <- .materializeCohortFromIds(h, bp, 1L, "ie_person_scope")
  scoped <- withr::with_options(list(nfilter.subset = 0),
    .planScopePopulations(h, resolved, scope, bp))
  rows <- .ie_rows(h, scoped$study)
  expect_equal(rows$cohort_start_date, c("2020-01-15", "2020-07-01"))
  expect_equal(rows$index_event_id, c(101L, 102L))
})
