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
    expect_true("race_concept_id" %in% names(df))

    # Derived fields present and valid
    # age_at_index is now returned as age_group (binned, not exact)
    expect_true("age_group" %in% names(df))
    expect_true("prior_observation" %in% names(df))
    expect_true("future_observation" %in% names(df))

    # year_of_birth should be removed when age_at_index is derived
    expect_false("year_of_birth" %in% names(df))
    expect_false("age_at_index" %in% names(df))

    expect_true(all(!is.na(df$age_group)))
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

# --- Cohort Membership Tests ---

test_that("cohort_membership output has correct structure", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(
      cm = list(type = "cohort_membership")
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan, list(cm = "cm_df"))
    df <- result$cm
    expect_true(is.data.frame(df))

    # Cohort 1 has 6 members
    expect_equal(nrow(df), 6)

    # Correct columns present
    expect_true(all(c("row_id", "subject_id", "cohort_definition_id",
                       "cohort_start_date", "cohort_end_date") %in% names(df)))

    # Uses subject_id, NOT person_id
    expect_true("subject_id" %in% names(df))
    expect_false("person_id" %in% names(df))

    # row_id is sequential
    expect_equal(df$row_id, 1:6)

    # cohort_definition_id matches
    expect_true(all(df$cohort_definition_id == 1L))
  })
})

test_that("cohort_membership without cohort returns NULL with warning", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(
      cm = list(type = "cohort_membership")
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    expect_warning(
      result <- .planExecute(handle, plan, list(cm = "cm_df")),
      "requires a cohort"
    )
    expect_null(result$cm)
  })
})

# --- Intervals Long Tests ---

test_that("intervals_long has rows from multiple tables", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(
      iv = list(
        type = "intervals_long",
        tables = c("condition_occurrence", "drug_exposure",
                    "visit_occurrence", "observation_period")
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan, list(iv = "iv_df"))
    df <- result$iv
    expect_true(is.data.frame(df))
    expect_true(nrow(df) > 0)

    # Correct columns
    expect_true(all(c("row_id", "subject_id", "interval_type",
                       "concept_id", "start_days_from_index",
                       "end_days_from_index") %in% names(df)))

    # Multiple interval_type values from different tables
    types <- unique(df$interval_type)
    expect_true(length(types) > 1)

    # No calendar dates; days are integers
    date_cols <- grep("_date$|_datetime$", names(df), value = TRUE)
    expect_equal(length(date_cols), 0)
    expect_true(is.integer(df$start_days_from_index))
    expect_true(is.integer(df$end_days_from_index))

    # end >= start
    expect_true(all(df$end_days_from_index >= df$start_days_from_index))

    # observation_period rows have NA concept_id
    op_rows <- df[df$interval_type == "observation_period", ]
    expect_true(nrow(op_rows) > 0)
    expect_true(all(is.na(op_rows$concept_id)))

    # row_id is sequential
    expect_equal(df$row_id, seq_len(nrow(df)))
  })
})

test_that("intervals_long concept_filter narrows results", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(
      iv = list(
        type = "intervals_long",
        tables = c("condition_occurrence"),
        concept_filter = list(condition_occurrence = c(201820))
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan, list(iv = "iv_df"))
    df <- result$iv
    expect_true(is.data.frame(df))
    expect_true(nrow(df) > 0)

    # Only diabetes concept
    expect_true(all(df$concept_id == 201820L))
  })
})

test_that("intervals_long without cohort returns NULL with warning", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(
      iv = list(
        type = "intervals_long",
        tables = c("condition_occurrence")
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    expect_warning(
      result <- .planExecute(handle, plan, list(iv = "iv_df")),
      "requires a cohort"
    )
    expect_null(result$iv)
  })
})

# --- Temporal Covariates Tests ---

test_that("temporal_covariates returns 3-element list", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(
      tc = list(
        type = "temporal_covariates",
        table = "condition_occurrence",
        concept_set = c(201820, 255573),
        bin_width = 90L,
        window_start = -365L,
        window_end = 0L,
        analyses = c("binary", "count")
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan, list(tc = "tc_df"))
    tc <- result$tc
    expect_true(is.list(tc))
    expect_true(all(c("temporalCovariates", "covariateRef", "timeRef") %in%
                      names(tc)))

    # Check column names
    expect_true(all(c("rowId", "timeId", "covariateId", "covariateValue") %in%
                      names(tc$temporalCovariates)))
    expect_true(all(c("covariateId", "covariateName", "analysisId",
                       "conceptId") %in% names(tc$covariateRef)))
    expect_true(all(c("timeId", "startDay", "endDay") %in%
                      names(tc$timeRef)))

    # All timeIds in covariates reference timeRef
    if (nrow(tc$temporalCovariates) > 0) {
      expect_true(all(tc$temporalCovariates$timeId %in%
                        tc$timeRef$timeId))
    }

    # All covariateIds in covariates reference covariateRef
    if (nrow(tc$temporalCovariates) > 0) {
      expect_true(all(tc$temporalCovariates$covariateId %in%
                        tc$covariateRef$covariateId))
    }

    # CovariateId = conceptId * 1000 + analysisId
    for (i in seq_len(nrow(tc$covariateRef))) {
      expected <- tc$covariateRef$conceptId[i] * 1000 +
        tc$covariateRef$analysisId[i]
      expect_equal(tc$covariateRef$covariateId[i], expected)
    }

    # timeRef bins span the requested window
    expect_equal(min(tc$timeRef$startDay), -365L)
    expect_true(max(tc$timeRef$endDay) >= -1L)
  })
})

test_that("temporal_covariates without cohort returns NULL with warning", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(
      tc = list(
        type = "temporal_covariates",
        table = "condition_occurrence",
        concept_set = c(201820),
        bin_width = 30L,
        window_start = -365L,
        window_end = 0L,
        analyses = c("binary")
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    expect_warning(
      result <- .planExecute(handle, plan, list(tc = "tc_df")),
      "requires a cohort"
    )
    expect_null(result$tc)
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
    expect_true("age_group" %in% names(result$demo))

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
