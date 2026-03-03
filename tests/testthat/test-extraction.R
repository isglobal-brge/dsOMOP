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

test_that("compileSelect includes sensitive columns when unblocked (server-authorized)", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(dsomop.allow_sensitive_columns = TRUE), {
    sql <- .compileSelect(handle, "observation", block_sensitive = FALSE)
    expect_true(grepl("value_as_string", sql))
  })
})

test_that("compileSelect rejects block_sensitive=FALSE without server authorization", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(dsomop.allow_sensitive_columns = FALSE), {
    expect_error(
      .compileSelect(handle, "observation", block_sensitive = FALSE),
      "not permitted by the server"
    )
  })
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

# === DCSI / HFRS score definitions and scoring ===

test_that("getScoreDefinitions returns DCSI with 12 entries and 7 categories", {
  defs <- dsOMOP:::.getScoreDefinitions("dcsi")
  expect_equal(defs$source_vocabulary, "ICD9CM")
  expect_equal(defs$scoring_mode, "tiered")
  expect_equal(length(defs$categories), 12)
  cat_names <- unique(vapply(defs$categories, `[[`, character(1), "category"))
  expect_equal(length(cat_names), 7)
  expect_true(all(c("retinopathy", "nephropathy", "neuropathy",
                     "cerebrovascular", "cardiovascular", "pvd",
                     "metabolic") %in% cat_names))
})

test_that("getScoreDefinitions returns HFRS with 109 entries", {
  defs <- dsOMOP:::.getScoreDefinitions("hfrs")
  expect_equal(defs$source_vocabulary, c("ICD10CM", "ICD10"))
  expect_equal(defs$scoring_mode, "weighted_binary")
  expect_equal(length(defs$categories), 109)
  # Check first entry (highest weight)
  expect_equal(defs$categories[[1]]$weight, 7.1)
  # Check last entry (lowest weight)
  expect_equal(defs$categories[[109]]$weight, 0.1)
})

test_that("DCSI tiered scoring produces expected scores", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  # Person 1: retinopathy(t1=1) + CKD(nephropathy t2=2) -> 3
  result1 <- dsOMOP:::.computeComorbidityScore(handle, "dcsi", c(1L))
  expect_equal(result1$score[result1$person_id == 1], 3)

  # Person 5: neuropathy(t1=1) + TIA(cerebrovascular t1=1) -> 2
  result5 <- dsOMOP:::.computeComorbidityScore(handle, "dcsi", c(5L))
  expect_equal(result5$score[result5$person_id == 5], 2)

  # Person 7: MI(cardiovascular t2=2) + DKA(metabolic t2=2) -> 4
  result7 <- dsOMOP:::.computeComorbidityScore(handle, "dcsi", c(7L))
  expect_equal(result7$score[result7$person_id == 7], 4)

  # Person 3: no DCSI conditions -> 0
  result3 <- dsOMOP:::.computeComorbidityScore(handle, "dcsi", c(3L))
  expect_equal(result3$score[result3$person_id == 3], 0)
})

test_that("HFRS weighted scoring produces expected scores", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  # Person 3: delirium (weight 3.2)
  result3 <- dsOMOP:::.computeComorbidityScore(handle, "hfrs", c(3L))
  expect_equal(result3$score[result3$person_id == 3], 3.2)

  # Person 9: dementia(7.1) + hemiplegia(4.4) = 11.5
  result9 <- dsOMOP:::.computeComorbidityScore(handle, "hfrs", c(9L))
  expect_equal(result9$score[result9$person_id == 9], 11.5)

  # Person 1: no HFRS conditions -> 0
  result1 <- dsOMOP:::.computeComorbidityScore(handle, "hfrs", c(1L))
  expect_equal(result1$score[result1$person_id == 1], 0)
})

test_that("DCSI/HFRS graceful fallback when concept_relationship is empty", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  # Delete all concept_relationship rows to simulate missing vocab
  DBI::dbExecute(handle$conn, "DELETE FROM concept_relationship")
  dsOMOP:::.buildBlueprint(handle)

  result <- suppressWarnings(
    dsOMOP:::.computeComorbidityScore(handle, "dcsi", c(1L, 5L)))
  expect_equal(nrow(result), 2)
  expect_true(all(result$score == 0))

  # Reset cache so HFRS also triggers resolution
  handle$resolved_hfrs <- NULL
  result_hfrs <- suppressWarnings(
    dsOMOP:::.computeComorbidityScore(handle, "hfrs", c(3L, 9L)))
  expect_equal(nrow(result_hfrs), 2)
  expect_true(all(result_hfrs$score == 0))
})

test_that("DCSI score metadata has correct analysis_id", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  result <- dsOMOP:::.computeComorbidityScore(handle, "dcsi", c(1L))
  meta <- attr(result, "score_meta")
  expect_equal(meta$analysis_id, 902L)
  expect_equal(meta$matching, "vocabulary_resolved")
})

test_that("HFRS score metadata has correct analysis_id", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  result <- dsOMOP:::.computeComorbidityScore(handle, "hfrs", c(3L))
  meta <- attr(result, "score_meta")
  expect_equal(meta$analysis_id, 926L)
  expect_equal(meta$matching, "vocabulary_resolved")
})

# === End-to-end: .computeDerivedColumns with DCSI/HFRS ===

test_that("computeDerivedColumns produces DCSI scores via full pipeline", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  specs <- list(
    list(kind = "dcsi", name = "dcsi_score")
  )
  withr::with_options(list(nfilter.subset = 3), {
    result <- dsOMOP:::.computeDerivedColumns(
      handle, specs, person_ids = c(1L, 3L, 5L, 7L))
    expect_true(is.data.frame(result))
    expect_true("dcsi_score" %in% names(result))
    expect_true(is.numeric(result$dcsi_score))
    # Person 1: retinopathy(t1) + CKD(t2) = 3
    expect_equal(result$dcsi_score[result$person_id == 1], 3)
    # Person 3: no DCSI conditions = 0
    expect_equal(result$dcsi_score[result$person_id == 3], 0)
    # Person 5: neuropathy(t1) + TIA(t1) = 2
    expect_equal(result$dcsi_score[result$person_id == 5], 2)
    # Person 7: MI(t2) + DKA(t2) = 4
    expect_equal(result$dcsi_score[result$person_id == 7], 4)
  })
})

test_that("computeDerivedColumns produces HFRS scores via full pipeline", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  specs <- list(
    list(kind = "hfrs", name = "hfrs_score")
  )
  withr::with_options(list(nfilter.subset = 3), {
    result <- dsOMOP:::.computeDerivedColumns(
      handle, specs, person_ids = c(1L, 3L, 5L, 9L))
    expect_true(is.data.frame(result))
    expect_true("hfrs_score" %in% names(result))
    expect_true(is.numeric(result$hfrs_score))
    # Person 1: no HFRS conditions = 0
    expect_equal(result$hfrs_score[result$person_id == 1], 0)
    # Person 3: delirium = 3.2
    expect_equal(result$hfrs_score[result$person_id == 3], 3.2)
    # Person 9: dementia(7.1) + hemiplegia(4.4) = 11.5
    expect_equal(result$hfrs_score[result$person_id == 9], 11.5)
  })
})

test_that("computeDerivedColumns with all 5 score types together", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  specs <- list(
    list(kind = "charlson", name = "charlson"),
    list(kind = "chads2", name = "chads2"),
    list(kind = "chadsvasc", name = "chadsvasc"),
    list(kind = "dcsi", name = "dcsi"),
    list(kind = "hfrs", name = "hfrs")
  )
  withr::with_options(list(nfilter.subset = 3), {
    result <- dsOMOP:::.computeDerivedColumns(
      handle, specs, person_ids = c(1L, 3L, 5L, 7L, 9L))
    expect_true(is.data.frame(result))
    # All 5 score columns present
    expect_true(all(c("charlson", "chads2", "chadsvasc", "dcsi", "hfrs") %in%
                      names(result)))
    # Charlson/CHADS2 are integer
    expect_true(is.integer(result$charlson))
    expect_true(is.integer(result$chads2))
    # DCSI and HFRS are numeric
    expect_true(is.numeric(result$dcsi))
    expect_true(is.numeric(result$hfrs))
    # All persons have a row
    expect_equal(nrow(result), 5)
  })
})

# === End-to-end: .planExecute with derived_columns ===

test_that("planExecute with DCSI derived column produces correct results", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      wide = list(
        type = "person_level",
        tables = list(),
        derived_columns = list(
          list(kind = "dcsi", name = "dcsi_score")
        )
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE,
                   min_persons = NULL)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- dsOMOP:::.planExecute(handle, plan, list(wide = "wide_df"))
    expect_true("wide" %in% names(result))
    df <- result$wide
    expect_true(is.data.frame(df))
    expect_true("dcsi_score" %in% names(df))
    expect_true(is.numeric(df$dcsi_score))
    # Person 1 should have dcsi = 3
    expect_equal(df$dcsi_score[df$person_id == 1], 3)
    # Person 7 should have dcsi = 4
    expect_equal(df$dcsi_score[df$person_id == 7], 4)
    # Persons without DCSI conditions get 0
    expect_true(all(df$dcsi_score[!df$person_id %in% c(1, 5, 7)] == 0))
  })
})

test_that("planExecute with HFRS derived column produces correct results", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      wide = list(
        type = "person_level",
        tables = list(),
        derived_columns = list(
          list(kind = "hfrs", name = "hfrs_score")
        )
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE,
                   min_persons = NULL)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- dsOMOP:::.planExecute(handle, plan, list(wide = "wide_df"))
    df <- result$wide
    expect_true("hfrs_score" %in% names(df))
    expect_true(is.numeric(df$hfrs_score))
    # Person 3: delirium = 3.2
    expect_equal(df$hfrs_score[df$person_id == 3], 3.2)
    # Person 9: dementia + hemiplegia = 11.5
    expect_equal(df$hfrs_score[df$person_id == 9], 11.5)
  })
})

test_that("planExecute with mixed derived columns (age + dcsi + hfrs)", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      wide = list(
        type = "person_level",
        tables = list(),
        derived_columns = list(
          list(kind = "age", name = "age"),
          list(kind = "sex_mf", name = "sex"),
          list(kind = "dcsi", name = "dcsi"),
          list(kind = "hfrs", name = "hfrs")
        )
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE,
                   min_persons = NULL)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- dsOMOP:::.planExecute(handle, plan, list(wide = "wide_df"))
    df <- result$wide
    expect_true(all(c("age", "sex", "dcsi", "hfrs") %in% names(df)))
    expect_true(is.integer(df$age))
    expect_true(is.numeric(df$dcsi))
    expect_true(is.numeric(df$hfrs))
    # Every person has all columns (no NAs after fill)
    expect_false(any(is.na(df$dcsi)))
    expect_false(any(is.na(df$hfrs)))
  })
})

# === v5.3 database tests ===

test_that("DCSI works on v5.3 database", {
  handle <- create_test_handle_v53()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  result <- dsOMOP:::.computeComorbidityScore(handle, "dcsi", c(1L, 5L, 7L))
  expect_equal(nrow(result), 3)
  # v5.3 fixture now has vocab data, so should produce real scores
  expect_equal(result$score[result$person_id == 1], 3)
  expect_equal(result$score[result$person_id == 5], 2)
})

test_that("HFRS works on v5.3 database", {
  handle <- create_test_handle_v53()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  result <- dsOMOP:::.computeComorbidityScore(handle, "hfrs", c(3L, 9L))
  expect_equal(nrow(result), 2)
  expect_equal(result$score[result$person_id == 3], 3.2)
  expect_equal(result$score[result$person_id == 9], 11.5)
})

# === Warning diagnostics ===

test_that("DCSI warns when concept_relationship is empty", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(handle$conn, "DELETE FROM concept_relationship")
  dsOMOP:::.buildBlueprint(handle)

  expect_warning(
    dsOMOP:::.computeComorbidityScore(handle, "dcsi", c(1L)),
    "no ICD-to-SNOMED mappings"
  )
})

test_that("HFRS warns when concept_relationship is empty", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(handle$conn, "DELETE FROM concept_relationship")
  dsOMOP:::.buildBlueprint(handle)

  expect_warning(
    dsOMOP:::.computeComorbidityScore(handle, "hfrs", c(3L)),
    "no ICD-to-SNOMED mappings"
  )
})

test_that("DCSI warns when concept_relationship table does not exist", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(handle$conn, "DROP TABLE concept_relationship")
  dsOMOP:::.buildBlueprint(handle)

  expect_warning(
    dsOMOP:::.computeComorbidityScore(handle, "dcsi", c(1L)),
    "concept_relationship table"
  )
})

# === Type safety and edge cases ===

test_that("DCSI score column is numeric even when all scores are zero", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  # Person 2 has no DCSI conditions (asthma patient)
  result <- dsOMOP:::.computeComorbidityScore(handle, "dcsi", c(2L, 4L, 6L))
  expect_true(is.numeric(result$score))
  expect_true(all(result$score == 0))
})

test_that("HFRS score column is numeric even when all scores are zero", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  result <- dsOMOP:::.computeComorbidityScore(handle, "hfrs", c(1L, 2L, 4L))
  expect_true(is.numeric(result$score))
  expect_true(all(result$score == 0))
})

test_that("resolveScoreConcepts caches results on handle", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  # First call resolves and caches
  result1 <- dsOMOP:::.resolveScoreConcepts(handle, "dcsi")
  expect_true(length(result1) > 0)
  expect_false(is.null(handle$resolved_dcsi))

  # Second call returns cached result (same object)
  result2 <- dsOMOP:::.resolveScoreConcepts(handle, "dcsi")
  expect_identical(result1, result2)
})

test_that("unknown score type raises informative error", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  expect_error(
    dsOMOP:::.computeComorbidityScore(handle, "unknown_score", c(1L)),
    "Unknown score type"
  )
})

test_that("all DCSI categories resolve at least one concept", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  resolved <- dsOMOP:::.resolveScoreConcepts(handle, "dcsi")
  # Our test data has 6 ICD9CM concepts covering 6 categories
  # (retinopathy, nephropathy, neuropathy, cerebrovascular, cardiovascular, metabolic)
  cat_names <- unique(vapply(resolved, `[[`, character(1), "category"))
  expect_true(length(cat_names) >= 6)
})

test_that("HFRS categories resolve at least test concepts", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  resolved <- dsOMOP:::.resolveScoreConcepts(handle, "hfrs")
  # Our test data has 3 ICD10CM concepts: F05(delirium), F00(dementia), G81(hemiplegia)
  cat_names <- unique(vapply(resolved, `[[`, character(1), "category"))
  expect_true("delirium" %in% cat_names)
  expect_true("dementia_alzheimers" %in% cat_names)
  expect_true("hemiplegia" %in% cat_names)
})

test_that("DCSI score with empty person_ids returns NULL", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  dsOMOP:::.buildBlueprint(handle)

  result <- dsOMOP:::.computeComorbidityScore(handle, "dcsi", integer(0))
  expect_null(result)
  result2 <- dsOMOP:::.computeComorbidityScore(handle, "dcsi", NULL)
  expect_null(result2)
})
