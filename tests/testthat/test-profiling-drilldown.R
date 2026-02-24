# ==============================================================================
# Tests for .profileConceptDrilldown() and .profileLocateConcept()
# ==============================================================================

# --- Drilldown ---

test_that("profileConceptDrilldown returns structured list", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3), {
    # Diabetes (201820) in condition_occurrence — 6 persons with diabetes
    result <- .profileConceptDrilldown(handle, "condition_occurrence", 201820L)

    expect_type(result, "list")
    expect_true(all(c("summary", "numeric_summary", "categorical_values",
                       "date_range", "missingness") %in% names(result)))

    # Summary checks
    s <- result$summary
    expect_equal(s$concept_id, 201820L)
    expect_true(nchar(s$concept_name) > 0)  # "Diabetes mellitus"
    expect_true(!is.null(s$n_records))
    expect_true(!is.null(s$n_persons))
  })
})

test_that("profileConceptDrilldown returns numeric_summary for measurement", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3), {
    # HbA1c (3004410) in measurement — has value_as_number
    result <- .profileConceptDrilldown(handle, "measurement", 3004410L)

    expect_true(!is.null(result$numeric_summary))
    expect_true("quantiles" %in% names(result$numeric_summary))
    expect_true(is.data.frame(result$numeric_summary$quantiles))
    expect_true("probability" %in% names(result$numeric_summary$quantiles))
    expect_true("value" %in% names(result$numeric_summary$quantiles))
  })
})

test_that("profileConceptDrilldown returns NULL numeric_summary for condition_occurrence", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3), {
    # condition_occurrence has no value_as_number column
    result <- .profileConceptDrilldown(handle, "condition_occurrence", 201820L)
    expect_null(result$numeric_summary)
  })
})

test_that("profileConceptDrilldown returns date_range", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3), {
    result <- .profileConceptDrilldown(handle, "condition_occurrence", 201820L)

    expect_true(!is.null(result$date_range))
    expect_true("column" %in% names(result$date_range))
    expect_true("min_date_safe" %in% names(result$date_range))
    expect_true("max_date_safe" %in% names(result$date_range))
  })
})

test_that("profileConceptDrilldown returns missingness data frame", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3), {
    result <- .profileConceptDrilldown(handle, "condition_occurrence", 201820L)

    expect_true(is.data.frame(result$missingness))
    expect_true("column_name" %in% names(result$missingness))
    expect_true("missing_rate" %in% names(result$missingness))
  })
})

test_that("profileConceptDrilldown blocks with Disclosive error for too few persons", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 20), {
    # Diabetes has 6 distinct persons but threshold set to 20
    expect_error(
      .profileConceptDrilldown(handle, "condition_occurrence", 201820L),
      "Disclosive"
    )
  })
})

# --- Locator ---

test_that("profileLocateConcept returns data frame with expected columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 3), {
    result <- .profileLocateConcept(handle, c(201820L, 3004410L))

    expect_true(is.data.frame(result))
    expect_true(all(c("table_name", "concept_column", "concept_id",
                       "n_records", "n_persons") %in% names(result)))
  })
})

test_that("profileLocateConcept finds known concept in expected table", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 3), {
    result <- .profileLocateConcept(handle, 201820L)

    # Diabetes should be found in condition_occurrence
    co_rows <- result[result$table_name == "condition_occurrence", ]
    expect_true(nrow(co_rows) > 0)
    expect_true(201820L %in% co_rows$concept_id)
  })
})

test_that("profileLocateConcept suppresses small counts", {
  handle <- create_test_handle(n_persons = 2)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 10), {
    # With a high threshold, counts should be suppressed (NA)
    result <- .profileLocateConcept(handle, 201820L)

    if (nrow(result) > 0) {
      # All counts should be NA since they're below threshold of 10
      expect_true(all(is.na(result$n_records) | result$n_records >= 10))
    }
  })
})

test_that("profileLocateConcept returns empty data frame for missing concept", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 3), {
    # Concept ID 999999 doesn't exist in test data
    result <- .profileLocateConcept(handle, 999999L)
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 0)
  })
})
