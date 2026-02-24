test_that("compileSelect generates valid SQL", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  sql <- .compileSelect(handle, "person")
  expect_true(grepl("SELECT", sql))
  expect_true(grepl("person", sql))
})

test_that("compileSelect applies concept filter", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  sql <- .compileSelect(handle, "condition_occurrence",
                         concept_filter = c(201820))
  expect_true(grepl("201820", sql))
  expect_true(grepl("IN", sql))
})

test_that("compileSelect blocks sensitive columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  sql <- .compileSelect(handle, "observation", block_sensitive = TRUE)
  expect_false(grepl("value_as_string", sql))
})

test_that("compileSelect includes sensitive columns when unblocked", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  sql <- .compileSelect(handle, "observation", block_sensitive = FALSE)
  expect_true(grepl("value_as_string", sql))
})

test_that("compileSelect applies time window", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  sql <- .compileSelect(handle, "condition_occurrence",
                         time_window = list(start_date = "2020-01-01",
                                            end_date = "2022-12-31"))
  expect_true(grepl("2020-01-01", sql))
  expect_true(grepl("2022-12-31", sql))
})

test_that("compileSelect applies column filter", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  sql <- .compileSelect(handle, "measurement",
                         columns = c("value_as_number", "unit_concept_id"))
  expect_true(grepl("value_as_number", sql))
  # person_id should always be included
  expect_true(grepl("person_id", sql))
})

test_that("executeQuery returns data frame with lowercase names", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  result <- .executeQuery(handle, "SELECT 1 AS MyCol")
  expect_true(is.data.frame(result))
  expect_true("mycol" %in% names(result))
})

test_that("extractTable returns data for conditions", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    result <- .extractTable(handle, "condition_occurrence")
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true("person_id" %in% names(result))
    expect_true("condition_concept_id" %in% names(result))
  })
})

test_that("extractTable filters by concept", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    result <- .extractTable(handle, "condition_occurrence",
                            concept_filter = c(201820))
    expect_true(all(result$condition_concept_id == 201820))
  })
})

test_that("extractTable blocks sensitive columns by default", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    result <- .extractTable(handle, "observation")
    expect_false("value_as_string" %in% names(result))
  })
})

test_that("convertTypes converts date columns", {
  df <- data.frame(
    person_id = 1,
    measurement_date = "2020-01-15",
    value_as_number = "7.2",
    stringsAsFactors = FALSE
  )
  result <- .convertTypes(df)
  expect_true(inherits(result$measurement_date, "Date"))
  expect_true(is.numeric(result$value_as_number))
})

test_that("compileFilter handles simple equality", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  filter <- list(var = "person_id", op = "==", value = 1)
  sql <- .compileFilter(handle, filter)
  expect_true(grepl("person_id", sql))
  expect_true(grepl("= 1", sql))
})

test_that("compileFilter handles IN operator", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  filter <- list(var = "person_id", op = "in", value = c(1, 2, 3))
  sql <- .compileFilter(handle, filter)
  expect_true(grepl("IN", sql))
})

test_that("compileFilter handles AND/OR", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  filter <- list(and = list(
    list(var = "person_id", op = "gt", value = 0),
    list(var = "person_id", op = "lt", value = 100)
  ))
  sql <- .compileFilter(handle, filter)
  expect_true(grepl("AND", sql))
})

test_that("standardizeName works correctly", {
  expect_equal(.standardizeName("Diabetes mellitus"), "diabetes_mellitus")
  expect_equal(.standardizeName("HbA1c %"), "hba1c")
  expect_true(is.na(.standardizeName(NA)))
})

# --- Temporal filtering tests ---

test_that("compileTemporalWhere generates calendar filter", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  temporal <- list(
    calendar = list(start = "2020-01-01", end = "2023-12-31")
  )
  where <- .compileTemporalWhere(handle, temporal, "t",
                                  "condition_start_date")
  expect_length(where, 2)
  expect_true(any(grepl("2020-01-01", where)))
  expect_true(any(grepl("2023-12-31", where)))
})

test_that("compileTemporalWhere generates index window filter", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  temporal <- list(
    index_window = list(start = -365, end = 0)
  )
  where <- .compileTemporalWhere(handle, temporal, "t",
                                  "condition_start_date")
  expect_length(where, 2)
  expect_true(any(grepl("cohort_start_date", where)))
})

test_that("compileTemporalWhere returns empty for NULL temporal", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  where <- .compileTemporalWhere(handle, NULL, "t",
                                  "condition_start_date")
  expect_length(where, 0)
})

test_that("wrapEventSelect wraps query with ROW_NUMBER CTE", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  base_sql <- "SELECT * FROM condition_occurrence"
  temporal <- list(
    event_select = list(order = "first", n = 1)
  )
  result <- .wrapEventSelect(handle, base_sql, temporal,
                              "condition_start_date")
  expect_true(grepl("ROW_NUMBER", result))
  expect_true(grepl("rn <= 1", result))
})

test_that("wrapEventSelect with last order uses DESC", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  base_sql <- "SELECT * FROM condition_occurrence"
  temporal <- list(
    event_select = list(order = "last", n = 1)
  )
  result <- .wrapEventSelect(handle, base_sql, temporal,
                              "condition_start_date")
  expect_true(grepl("DESC", result))
})

test_that("applyDateHandling relative converts dates to integers", {
  df <- data.frame(
    person_id = c(1, 2),
    cohort_start_date = as.Date(c("2020-01-01", "2020-06-01")),
    condition_start_date = as.Date(c("2019-12-01", "2020-05-15")),
    stringsAsFactors = FALSE
  )
  dh <- list(mode = "relative", reference = "index")
  result <- .applyDateHandling(df, dh, "cohort_start_date")
  expect_true(is.integer(result$condition_start_date))
  expect_equal(result$condition_start_date[1],
               as.integer(as.Date("2019-12-01") - as.Date("2020-01-01")))
})

test_that("applyDateHandling binned truncates dates by month", {
  df <- data.frame(
    person_id = 1,
    condition_start_date = as.Date("2020-03-15"),
    stringsAsFactors = FALSE
  )
  dh <- list(mode = "binned", bin_width = "month")
  result <- .applyDateHandling(df, dh)
  expect_equal(result$condition_start_date, "2020-03-01")
})

test_that("applyDateHandling remove drops date columns", {
  df <- data.frame(
    person_id = 1,
    condition_start_date = as.Date("2020-03-15"),
    condition_end_date = as.Date("2020-04-01"),
    value = 42,
    stringsAsFactors = FALSE
  )
  dh <- list(mode = "remove")
  result <- .applyDateHandling(df, dh)
  expect_false("condition_start_date" %in% names(result))
  expect_false("condition_end_date" %in% names(result))
  expect_true("person_id" %in% names(result))
  expect_true("value" %in% names(result))
})
