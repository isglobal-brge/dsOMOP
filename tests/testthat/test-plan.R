test_that("plan validate returns structure", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      baseline = list(type = "person_level", tables = list(person = c("person_id", "gender_concept_id")))
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planPreview(handle, plan)
    expect_true(is.list(result))
  })
})

test_that("plan execute creates output data frames", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      conditions = list(
        type = "event_level",
        table = "condition_occurrence",
        columns = NULL,
        concept_set = c(201820),
        representation = list(format = "long"),
        filters = list(concept_set = list(ids = c(201820)))
      )
    ),
    options = list(
      translate_concepts = FALSE,
      block_sensitive = TRUE,
      min_persons = NULL
    )
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan, list(conditions = "cond_df"))
    expect_true(is.list(result))
    expect_true("conditions" %in% names(result))
    expect_true(is.data.frame(result$conditions))
    expect_true(nrow(result$conditions) > 0)
    expect_true(all(result$conditions$condition_concept_id == 201820))
  })
})

test_that("plan execute passes temporal and date_handling to extraction", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      conditions = list(
        type = "event_level",
        table = "condition_occurrence",
        columns = NULL,
        concept_set = c(201820),
        representation = list(format = "long"),
        filters = list(concept_set = list(ids = c(201820))),
        temporal = list(
          calendar = list(start = "2010-01-01", end = "2030-12-31")
        ),
        date_handling = list(mode = "absolute")
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3,
                           dsomop.allow_absolute_dates = TRUE), {
    result <- .planExecute(handle, plan, list(conditions = "cond_df"))
    expect_true(is.list(result))
    expect_true("conditions" %in% names(result))
    expect_true(is.data.frame(result$conditions))
  })
})

test_that("plan execute blocks absolute dates without server authorization", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      conditions = list(
        type = "event_level",
        table = "condition_occurrence",
        columns = NULL,
        concept_set = c(201820),
        representation = list(format = "long"),
        filters = list(concept_set = list(ids = c(201820))),
        date_handling = list(mode = "absolute")
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3,
                           dsomop.allow_absolute_dates = FALSE), {
    # Error is caught by .planExecute's tryCatch and converted to warning
    expect_warning(
      .planExecute(handle, plan, list(conditions = "cond_df")),
      "not permitted by the server"
    )
  })
})

test_that("cohort temp table includes dates", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Create a cohort table with dates in the test database
  DBI::dbExecute(handle$conn, "DROP TABLE IF EXISTS cohort")
  DBI::dbExecute(handle$conn, paste0(
    "CREATE TABLE cohort (",
    "cohort_definition_id INTEGER, ",
    "subject_id INTEGER, ",
    "cohort_start_date TEXT, ",
    "cohort_end_date TEXT)"
  ))
  # Insert rows for all test persons
  for (pid in 1:15) {
    DBI::dbExecute(handle$conn, paste0(
      "INSERT INTO cohort VALUES (1, ", pid,
      ", '2020-01-01', '2020-12-31')"
    ))
  }

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      conditions = list(
        type = "event_level",
        table = "condition_occurrence",
        columns = NULL,
        representation = list(format = "long"),
        filters = list()
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    # Force blueprint rebuild to pick up cohort table
    .buildBlueprint(handle, force = TRUE)
    result <- .planExecute(handle, plan, list(conditions = "cond_df"))
    expect_true(is.list(result))
    expect_true("conditions" %in% names(result))
  })
})

test_that("materializeConceptSet returns NULL for small sets", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  result <- .materializeConceptSet(handle, c(1, 2, 3))
  expect_null(result)
})

test_that("materializeConceptSet creates temp table for large sets", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  large_set <- seq_len(100)
  result <- .materializeConceptSet(handle, large_set)
  expect_true(!is.null(result))
  expect_true(grepl("^dsomop_cs_", result))

  # Verify the temp table was created
  count <- DBI::dbGetQuery(handle$conn,
    paste0("SELECT COUNT(*) AS n FROM ", result))
  expect_equal(count$n, 100)

  .dropTempTable(handle, result)
})
