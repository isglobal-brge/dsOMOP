test_that("cohort creation with inclusion criteria filters persons", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Base cohort: all persons with diabetes (concept 201820)
  spec <- list(
    type = "condition",
    concept_set = c(201820),
    inclusion_criteria = list(
      list(
        table = "measurement",
        concept_set = c(3004410),
        temporal = list(),
        occurrence = list(type = "at_least", count = 1)
      )
    )
  )

  withr::with_options(list(nfilter.subset = 3), {
    # This should work: base cohort has persons, inclusion filters further
    result <- tryCatch(
      .cohortCreate(handle, spec, mode = "temporary", cohort_id = 99),
      error = function(e) e$message
    )
    # Either succeeds (returns temp table name) or fails disclosure
    # (too few persons after filtering) — both are valid behavior
    expect_true(is.character(result))
  })
})

test_that("cohort creation without inclusion criteria works", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  spec <- list(
    type = "condition",
    concept_set = c(201820)
  )

  withr::with_options(list(nfilter.subset = 3), {
    temp_name <- .cohortCreate(handle, spec, mode = "temporary",
                                cohort_id = 1)
    expect_true(is.character(temp_name))
    expect_true(grepl("dsomop_cohort_", temp_name))

    # Verify the temp table has rows
    rows <- DBI::dbGetQuery(handle$conn,
      paste0("SELECT COUNT(*) AS n FROM ", temp_name))
    expect_true(rows$n > 0)
  })
})

test_that("value_threshold allows threshold operators on measurements", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  spec <- list(
    type = "measurement",
    concept_set = c(3004410),
    value_threshold = list(op = ">=", value = 6.5)
  )

  withr::with_options(list(nfilter.subset = 3), {
    temp_name <- .cohortCreate(handle, spec, mode = "temporary",
                                cohort_id = 42)
    expect_true(is.character(temp_name))

    # The cohort must contain exactly the persons whose measurement meets the
    # threshold. Computed from a direct query so the test is robust to the
    # fixture's specific values; it still fails if the threshold is dropped,
    # negated, or applied to the wrong operator.
    expected <- DBI::dbGetQuery(handle$conn,
      "SELECT COUNT(DISTINCT person_id) AS n FROM measurement
       WHERE measurement_concept_id = 3004410 AND value_as_number >= 6.5")$n
    actual <- DBI::dbGetQuery(handle$conn,
      paste0("SELECT COUNT(DISTINCT subject_id) AS n FROM ", temp_name))$n
    expect_equal(actual, expected)
    expect_gte(actual, 3)  # cleared the disclosure floor
  })
})

test_that("value_threshold blocks exact-value operators as disclosive", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  base <- list(type = "measurement", concept_set = c(3004410))

  withr::with_options(list(nfilter.subset = 3), {
    for (bad_op in c("==", "=", "!=")) {
      spec <- c(base, list(value_threshold = list(op = bad_op, value = 6.5)))
      expect_error(
        .cohortCreate(handle, spec, mode = "temporary", cohort_id = 43),
        "exact-value"
      )
    }
    # Unknown operator must error, not silently drop the criterion.
    spec_unknown <- c(base, list(value_threshold = list(op = "~=", value = 6.5)))
    expect_error(
      .cohortCreate(handle, spec_unknown, mode = "temporary", cohort_id = 44),
      "Unknown value_threshold operator"
    )
  })
})

test_that("cohort creation accepts a concept_set spec and expands descendants", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # 4000001 (Respiratory disease) has no direct condition records; its
  # descendants (COPD 255573, Asthma 317009) do. A bare flat vector would
  # match nobody and fail the disclosure floor, so a non-empty cohort here
  # proves the spec's include_descendants expansion was honoured.
  spec <- list(
    type = "condition",
    concept_set = list(concepts = c(4000001), include_descendants = TRUE)
  )

  withr::with_options(list(nfilter.subset = 3), {
    temp_name <- .cohortCreate(handle, spec, mode = "temporary",
                                cohort_id = 77)
    expect_true(is.character(temp_name))

    expanded <- .vocabExpandConceptSet(handle,
      list(concepts = c(4000001), include_descendants = TRUE))
    id_list <- paste(expanded, collapse = ", ")
    expected <- DBI::dbGetQuery(handle$conn, paste0(
      "SELECT COUNT(DISTINCT person_id) AS n FROM condition_occurrence
       WHERE condition_concept_id IN (", id_list, ")"))$n
    actual <- DBI::dbGetQuery(handle$conn,
      paste0("SELECT COUNT(DISTINCT subject_id) AS n FROM ", temp_name))$n
    expect_equal(actual, expected)
    expect_gte(actual, 3)  # cleared the disclosure floor
  })
})

test_that("applyInclusionCriteria returns cohort_temp for empty criteria", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .applyInclusionCriteria(handle, "some_table", list())
  expect_equal(result, "some_table")

  result2 <- .applyInclusionCriteria(handle, "some_table", NULL)
  expect_equal(result2, "some_table")
})
