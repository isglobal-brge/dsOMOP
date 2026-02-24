# ==============================================================================
# dsOMOP v2 - Output Contract Tests
# ==============================================================================
# Tests for baseline, survival, sparse, days_from_index, concept_dictionary,
# and integration of all output types.
# ==============================================================================

# --- Baseline Tests ---

test_that("baseline output produces one row per cohort member", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(
      demo = list(
        type = "baseline",
        columns = c("gender_concept_id", "year_of_birth", "race_concept_id"),
        derived = c("age_at_index", "prior_observation", "future_observation")
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan, list(demo = "demo_df"))
    expect_true("demo" %in% names(result))
    df <- result$demo
    expect_true(is.data.frame(df))

    # Cohort 1 has 6 members (persons 1, 3, 5, 7, 9, 11)
    expect_equal(nrow(df), 6)

    # Required columns present
    expect_true("row_id" %in% names(df))
    expect_true("person_id" %in% names(df))
    expect_true("gender_concept_id" %in% names(df))
    expect_true("year_of_birth" %in% names(df))
    expect_true("race_concept_id" %in% names(df))

    # Derived fields present and valid
    expect_true("age_at_index" %in% names(df))
    expect_true("prior_observation" %in% names(df))
    expect_true("future_observation" %in% names(df))

    expect_true(all(df$age_at_index > 0))
    # Some persons may have cohort_start before obs_period (LEFT JOIN → NA)
    valid_obs <- !is.na(df$prior_observation)
    expect_true(any(valid_obs))
    expect_true(all(df$prior_observation[valid_obs] >= 0))
    expect_true(all(df$future_observation[valid_obs] > 0))

    # row_id is sequential
    expect_equal(df$row_id, 1:6)
  })
})

test_that("baseline without cohort returns NULL with warning", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(
      demo = list(
        type = "baseline",
        columns = c("gender_concept_id"),
        derived = NULL
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    expect_warning(
      result <- .planExecute(handle, plan, list(demo = "demo_df")),
      "requires a cohort"
    )
    expect_null(result$demo)
  })
})

test_that("baseline with concept translation works", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(
      demo = list(
        type = "baseline",
        columns = c("gender_concept_id", "year_of_birth"),
        derived = NULL
      )
    ),
    options = list(translate_concepts = TRUE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan, list(demo = "demo_df"))
    df <- result$demo
    expect_true(is.data.frame(df))
    # Gender concept IDs should be translated to names
    expect_true(all(df$gender_concept_id %in% c("male", "female")))
  })
})

# --- Event-level days_from_index Tests ---

test_that("days_from_index present when index_window is active", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(
      conditions = list(
        type = "event_level",
        table = "condition_occurrence",
        columns = NULL,
        representation = list(format = "long"),
        filters = list(),
        temporal = list(
          index_window = list(start = -365, end = 365)
        )
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan, list(conditions = "cond_df"))
    df <- result$conditions
    expect_true(is.data.frame(df))
    expect_true(nrow(df) > 0)
    expect_true("days_from_index" %in% names(df))

    # days_from_index should be within the window bounds
    expect_true(all(df$days_from_index >= -365))
    expect_true(all(df$days_from_index <= 365))

    # cohort_start_date should NOT be in the output (stripped)
    expect_false("cohort_start_date" %in% names(df))
  })
})

test_that("days_from_index NOT present without index_window", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
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
    result <- .planExecute(handle, plan, list(conditions = "cond_df"))
    df <- result$conditions
    expect_true(is.data.frame(df))
    # Without index_window, days_from_index should NOT be present
    expect_false("days_from_index" %in% names(df))
  })
})

# --- Sparse Format Tests ---

test_that("sparse format returns list with covariates and covariateRef", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(
      meas = list(
        type = "event_level",
        table = "measurement",
        columns = NULL,
        concept_set = c(3004410, 3025315),
        representation = list(format = "sparse"),
        filters = list(concept_set = list(ids = c(3004410, 3025315)))
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan, list(meas = "meas_df"))
    sparse <- result$meas
    expect_true(is.list(sparse))
    expect_true("covariates" %in% names(sparse))
    expect_true("covariateRef" %in% names(sparse))

    # Check column names match FeatureExtraction convention
    expect_true(all(c("rowId", "covariateId", "covariateValue") %in%
                      names(sparse$covariates)))
    expect_true(all(c("covariateId", "covariateName", "analysisId", "conceptId") %in%
                      names(sparse$covariateRef)))

    # CovariateId = conceptId * 1000 + analysisId
    for (i in seq_len(nrow(sparse$covariateRef))) {
      expected <- sparse$covariateRef$conceptId[i] * 1000 +
        sparse$covariateRef$analysisId[i]
      expect_equal(sparse$covariateRef$covariateId[i], expected)
    }

    # All covariateIds in covariates exist in covariateRef
    expect_true(all(sparse$covariates$covariateId %in%
                      sparse$covariateRef$covariateId))
  })
})

test_that("sparse format includes numeric analyses for measurements", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Extract HbA1c only (has value_as_number)
  withr::with_options(list(nfilter.subset = 3), {
    df <- .extractTable(
      handle,
      table = "measurement",
      concept_filter = c(3004410),
      representation = "sparse"
    )
    expect_true(is.list(df))
    ref <- df$covariateRef
    # Should have binary (1), count (2), mean (3), min (4), max (5)
    expect_true(all(c(1L, 2L, 3L, 4L, 5L) %in% ref$analysisId))
  })
})

# --- Survival Tests ---

test_that("survival output produces correct event/censoring", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(
      tte = list(
        type = "survival",
        outcome = list(
          table = "condition_occurrence",
          concept_set = c(4000002)
        ),
        tar = list(start_offset = 0, end_offset = 730),
        event_order = "first"
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan, list(tte = "tte_df"))
    df <- result$tte
    expect_true(is.data.frame(df))

    # 6 cohort members -> 6 rows
    expect_equal(nrow(df), 6)

    # Required columns
    expect_true(all(c("row_id", "person_id", "event", "time_to_event_days") %in%
                      names(df)))

    # Persons 1, 5, 9 have MI -> event = 1
    event_persons <- df$person_id[df$event == 1]
    expect_true(all(c(1, 5, 9) %in% event_persons))

    # Persons 3, 7, 11 do NOT have MI -> event = 0
    censored_persons <- df$person_id[df$event == 0]
    expect_true(all(c(3, 7, 11) %in% censored_persons))

    # Censored persons have time_to_event = tar_end - tar_start = 730
    censored_rows <- df[df$event == 0, ]
    expect_true(all(censored_rows$time_to_event_days == 730))

    # No calendar dates in output (privacy)
    date_cols <- grep("_date$|_datetime$", names(df), value = TRUE)
    expect_equal(length(date_cols), 0)
  })
})

test_that("survival output requires cohort", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(
      tte = list(
        type = "survival",
        outcome = list(
          table = "condition_occurrence",
          concept_set = c(4000002)
        ),
        tar = list(start_offset = 0, end_offset = 730)
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    expect_warning(
      result <- .planExecute(handle, plan, list(tte = "tte_df")),
      "requires a cohort"
    )
    expect_null(result$tte)
  })
})

# --- Concept Dictionary Tests ---

test_that("concept dictionary collects concepts from other outputs", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(
      conditions = list(
        type = "event_level",
        table = "condition_occurrence",
        columns = NULL,
        concept_set = c(201820),
        representation = list(format = "long"),
        filters = list(concept_set = list(ids = c(201820)))
      ),
      dictionary = list(
        type = "concept_dictionary",
        source_outputs = c("conditions")
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan, list(conditions = "c_df",
                                               dictionary = "dict_df"))
    dict <- result$dictionary
    expect_true(is.data.frame(dict))
    expect_true(nrow(dict) > 0)

    # Has expected columns
    expect_true(all(c("concept_id", "concept_name", "domain_id",
                       "used_in_outputs") %in% names(dict)))

    # Should include the diabetes concept (201820) and type concept (44818518)
    expect_true(201820 %in% dict$concept_id)

    # used_in_outputs should reference "conditions"
    row_201820 <- dict[dict$concept_id == 201820, ]
    expect_true(grepl("conditions", row_201820$used_in_outputs))
  })
})

test_that("concept dictionary source_outputs filtering works", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(
      conditions = list(
        type = "event_level",
        table = "condition_occurrence",
        columns = NULL,
        concept_set = c(201820),
        representation = list(format = "long"),
        filters = list(concept_set = list(ids = c(201820)))
      ),
      drugs = list(
        type = "event_level",
        table = "drug_exposure",
        columns = NULL,
        concept_set = c(1124300),
        representation = list(format = "long"),
        filters = list(concept_set = list(ids = c(1124300)))
      ),
      dict_cond_only = list(
        type = "concept_dictionary",
        source_outputs = c("conditions")
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan, list(conditions = "c_df",
                                               drugs = "d_df",
                                               dict_cond_only = "dict_df"))
    dict <- result$dict_cond_only
    expect_true(is.data.frame(dict))

    # Should contain condition concepts but not drug concepts
    # used_in_outputs should only mention "conditions"
    expect_false(any(grepl("drugs", dict$used_in_outputs)))
  })
})

# --- Integration Test ---

test_that("single plan with all output types executes successfully", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(
      events = list(
        type = "event_level",
        table = "condition_occurrence",
        columns = NULL,
        concept_set = c(201820),
        representation = list(format = "long"),
        filters = list(concept_set = list(ids = c(201820)))
      ),
      demo = list(
        type = "baseline",
        columns = c("gender_concept_id", "year_of_birth"),
        derived = c("age_at_index")
      ),
      tte = list(
        type = "survival",
        outcome = list(
          table = "condition_occurrence",
          concept_set = c(4000002)
        ),
        tar = list(start_offset = 0, end_offset = 730),
        event_order = "first"
      ),
      dictionary = list(
        type = "concept_dictionary",
        source_outputs = c("events", "demo")
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan, list(
      events = "ev_df", demo = "demo_df",
      tte = "tte_df", dictionary = "dict_df"
    ))

    # All 4 outputs present
    expect_true(all(c("events", "demo", "tte", "dictionary") %in% names(result)))

    # events: data.frame
    expect_true(is.data.frame(result$events))
    expect_true(nrow(result$events) > 0)

    # demo: data.frame with derived fields
    expect_true(is.data.frame(result$demo))
    expect_equal(nrow(result$demo), 6)
    expect_true("age_at_index" %in% names(result$demo))

    # tte: data.frame with event/censoring
    expect_true(is.data.frame(result$tte))
    expect_equal(nrow(result$tte), 6)
    expect_true(all(c("event", "time_to_event_days") %in% names(result$tte)))

    # dictionary: concept lookup
    expect_true(is.data.frame(result$dictionary))
    expect_true(nrow(result$dictionary) > 0)
    expect_true("concept_name" %in% names(result$dictionary))
  })
})

# --- Validation Tests ---

test_that("planValidate handles baseline type", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(
      demo = list(type = "baseline")
    )
  )
  class(plan) <- c("omop_plan", "list")

  result <- .planValidate(handle, plan)
  expect_true(result$valid)
})

test_that("planValidate handles survival type", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(
      tte = list(
        type = "survival",
        outcome = list(table = "condition_occurrence", concept_set = c(4000002))
      )
    )
  )
  class(plan) <- c("omop_plan", "list")

  result <- .planValidate(handle, plan)
  expect_true(result$valid)
})

test_that("planPreview handles new output types", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(
      demo = list(type = "baseline"),
      tte = list(
        type = "survival",
        outcome = list(table = "condition_occurrence", concept_set = c(4000002)),
        tar = list(start_offset = 0, end_offset = 365)
      ),
      dict = list(type = "concept_dictionary", source_outputs = c("demo"))
    )
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    preview <- .planPreview(handle, plan)
    expect_true(is.list(preview))
    expect_true(all(c("demo", "tte", "dict") %in% names(preview$outputs)))

    expect_equal(preview$outputs$demo$type, "baseline")
    expect_equal(preview$outputs$tte$type, "survival")
    expect_equal(preview$outputs$dict$type, "concept_dictionary")
  })
})
