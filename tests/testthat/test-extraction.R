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

# === New feature types via .toFeatures() ===

test_that("sd_value feature computes standard deviation per person", {
  df <- data.frame(
    person_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    measurement_concept_id = rep(100L, 6),
    value_as_number = c(5.0, 7.0, 6.0, 10.0, 10.0, 10.0),
    stringsAsFactors = FALSE
  )
  specs <- list(test_sd = list(type = "sd_value", name = "test_sd",
                                concept_set = 100L,
                                value_column = "value_as_number"))
  result <- dsOMOP:::.toFeatures(df, "measurement", specs)
  expect_true("test_sd" %in% names(result))
  expect_equal(result$test_sd[result$person_id == 1], sd(c(5, 7, 6)))
  expect_equal(result$test_sd[result$person_id == 2], 0)
})

test_that("cv_value feature computes coefficient of variation", {
  df <- data.frame(
    person_id = c(1L, 1L, 1L),
    measurement_concept_id = rep(100L, 3),
    value_as_number = c(5.0, 7.0, 6.0),
    stringsAsFactors = FALSE
  )
  specs <- list(test_cv = list(type = "cv_value", name = "test_cv",
                                concept_set = 100L,
                                value_column = "value_as_number"))
  result <- dsOMOP:::.toFeatures(df, "measurement", specs)
  expect_true("test_cv" %in% names(result))
  expected_cv <- sd(c(5, 7, 6)) / mean(c(5, 7, 6)) * 100
  expect_equal(result$test_cv[1], expected_cv)
})

test_that("slope_value feature computes linear slope", {
  df <- data.frame(
    person_id = c(1L, 1L, 1L),
    measurement_concept_id = rep(100L, 3),
    value_as_number = c(5.0, 7.0, 9.0),
    measurement_date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    stringsAsFactors = FALSE
  )
  specs <- list(test_slope = list(type = "slope_value", name = "test_slope",
                                   concept_set = 100L,
                                   value_column = "value_as_number"))
  result <- dsOMOP:::.toFeatures(df, "measurement", specs)
  expect_true("test_slope" %in% names(result))
  expect_true(result$test_slope[1] > 0)
})

test_that("abnormal_high counts values above range_high", {
  df <- data.frame(
    person_id = c(1L, 1L, 1L, 2L, 2L),
    measurement_concept_id = rep(100L, 5),
    value_as_number = c(7.0, 5.5, 8.0, 5.0, 4.5),
    range_high = rep(6.0, 5),
    stringsAsFactors = FALSE
  )
  specs <- list(high_count = list(type = "abnormal_high", name = "high_count",
                                   concept_set = 100L))
  result <- dsOMOP:::.toFeatures(df, "measurement", specs)
  expect_equal(result$high_count[result$person_id == 1], 2L)
  expect_equal(result$high_count[result$person_id == 2], 0L)
})

test_that("abnormal_low counts values below range_low", {
  df <- data.frame(
    person_id = c(1L, 1L, 1L),
    measurement_concept_id = rep(100L, 3),
    value_as_number = c(3.5, 5.0, 3.8),
    range_low = rep(4.0, 3),
    stringsAsFactors = FALSE
  )
  specs <- list(low_count = list(type = "abnormal_low", name = "low_count",
                                  concept_set = 100L))
  result <- dsOMOP:::.toFeatures(df, "measurement", specs)
  expect_equal(result$low_count[1], 2L)
})

test_that("gap_max_days computes max gap between events", {
  df <- data.frame(
    person_id = c(1L, 1L, 1L),
    visit_concept_id = rep(9202L, 3),
    visit_start_date = as.Date(c("2020-01-01", "2020-03-01", "2020-04-01")),
    stringsAsFactors = FALSE
  )
  specs <- list(max_gap = list(type = "gap_max_days", name = "max_gap",
                                concept_set = 9202L))
  result <- dsOMOP:::.toFeatures(df, "visit_occurrence", specs)
  expect_equal(result$max_gap[1], 60)
})

test_that("gap_mean_days computes mean gap between events", {
  df <- data.frame(
    person_id = c(1L, 1L, 1L),
    visit_concept_id = rep(9202L, 3),
    visit_start_date = as.Date(c("2020-01-01", "2020-03-01", "2020-04-01")),
    stringsAsFactors = FALSE
  )
  specs <- list(mean_gap = list(type = "gap_mean_days", name = "mean_gap",
                                 concept_set = 9202L))
  result <- dsOMOP:::.toFeatures(df, "visit_occurrence", specs)
  expect_equal(result$mean_gap[1], mean(c(60, 31)))
})

test_that("duration_sum sums start-to-end durations", {
  df <- data.frame(
    person_id = c(1L, 1L),
    drug_concept_id = rep(1124300L, 2),
    drug_exposure_start_date = as.Date(c("2020-01-01", "2020-06-01")),
    drug_exposure_end_date = as.Date(c("2020-02-01", "2020-07-01")),
    stringsAsFactors = FALSE
  )
  specs <- list(total_dur = list(type = "duration_sum", name = "total_dur",
                                  concept_set = 1124300L))
  result <- dsOMOP:::.toFeatures(df, "drug_exposure", specs)
  expect_equal(result$total_dur[1], 31 + 30)
})
