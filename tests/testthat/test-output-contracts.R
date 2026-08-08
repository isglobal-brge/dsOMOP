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
        columns = c("gender_concept_id", "race_concept_id"),
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

    # Exact year_of_birth never crosses the output boundary.
    expect_false("year_of_birth" %in% names(df))
    expect_false("age_at_index" %in% names(df))

    # Locally under-supported canonical bands are NA rather than being merged
    # into data-dependent labels that would differ across federated servers.
    age_breaks <- .omopDisclosureSettings()$age_breaks
    canonical_age_groups <- c(
      paste0(age_breaks[-length(age_breaks)], "-", age_breaks[-1L] - 1L),
      paste0(age_breaks[length(age_breaks)], "+"))
    expect_true(all(is.na(df$age_group) |
                    df$age_group %in% canonical_age_groups))
    # Some persons may have cohort_start before obs_period (LEFT JOIN → NA)
    valid_obs <- !is.na(df$prior_observation)
    expect_true(any(valid_obs))
    expect_true(all(df$prior_observation[valid_obs] >= 0))
    expect_true(all(df$future_observation[valid_obs] > 0))

    # row_id is sequential
    expect_equal(df$row_id, 1:6)
  })
})

test_that("baseline rejects blocked and unknown person columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  make_plan <- function(column) {
    plan <- list(
      cohort = list(type = "cohort_table", cohort_definition_id = 1),
      outputs = list(demo = list(type = "baseline", columns = column)),
      options = list(translate_concepts = FALSE, block_sensitive = TRUE)
    )
    class(plan) <- c("omop_plan", "list")
    plan
  }

  withr::with_options(list(nfilter.subset = 3, dsomop.query_strict = TRUE), {
    expect_error(
      .planExecute(handle, make_plan("day_of_birth"), list(demo = "demo_df")),
      "blocked"
    )
    expect_error(
      .planExecute(handle, make_plan("not_a_person_column"),
                   list(demo = "demo_df")),
      "not found"
    )
  })
})

test_that("baseline accepts only a public common age-grid coarsening", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  common <- seq(0L, 80L, 10L)
  plan <- structure(list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(demo = list(
      type = "baseline", columns = "gender_concept_id",
      derived = "age_at_index", age_breaks = common
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  ), class = c("omop_plan", "list"))

  result <- withr::with_options(list(nfilter.subset = 3, nfilter.tab = 1),
    .planExecute(handle, plan, list(demo = "demo_df"))$demo)
  labels <- c(paste0(common[-length(common)], "-",
                     common[-1L] - 1L), "80+")
  expect_true(all(result$age_group %in% labels))

  plan$outputs$demo$age_breaks <- c(0L, 18L, 40L)
  validation <- .planValidate(handle, plan)
  expect_false(validation$valid)
  expect_match(validation$errors, "invalid age_breaks")
})

test_that("baseline without cohort fails closed", {
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
    expect_error(
      .planExecute(handle, plan, list(demo = "demo_df")),
      "requires a cohort"
    )
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
        columns = c("gender_concept_id"),
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
    # Gender concept IDs are translated to the readable concept_name verbatim,
    # matching the catalog (value.counts etc.) — not a standardized form.
    expect_true(all(df$gender_concept_id %in% c("MALE", "FEMALE")))
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

test_that("sparse format returns covariates, references, and person map", {
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
    expect_true("personRef" %in% names(sparse))

    # Check column names match FeatureExtraction convention
    expect_true(all(c("rowId", "covariateId", "covariateValue") %in%
                      names(sparse$covariates)))
    expect_true(all(c("covariateId", "covariateName", "analysisId", "conceptId") %in%
                      names(sparse$covariateRef)))
    expect_true(all(c("rowId", "person_id") %in% names(sparse$personRef)))

    # CovariateId = conceptId * 1000 + analysisId
    for (i in seq_len(nrow(sparse$covariateRef))) {
      expected <- sparse$covariateRef$conceptId[i] * 1000 +
        sparse$covariateRef$analysisId[i]
      expect_equal(sparse$covariateRef$covariateId[i], expected)
    }

    # All covariateIds in covariates exist in covariateRef
    expect_true(all(sparse$covariates$covariateId %in%
                      sparse$covariateRef$covariateId))
    expect_true(all(sparse$covariates$rowId %in% sparse$personRef$rowId))
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
      person_ids = 1:6,
      representation = "sparse"
    )
    expect_true(is.list(df))
    ref <- df$covariateRef
    # Should have binary (1), count (2), mean (3), min (4), max (5)
    expect_true(all(c(1L, 2L, 3L, 4L, 5L) %in% ref$analysisId))
    expect_equal(df$personRef$person_id, 1:6)
    expect_equal(df$personRef$rowId, 1:6)
    expect_true(all(df$covariates$rowId %in% df$personRef$rowId))

    released <- .testPseudonymize(
      df, .testPseudonymKey("output-contract-person")
    )
    expect_true(all(grepl("^p", released$personRef$person_id)))
    expect_true("rowId" %in%
                  attr(released$covariates, "dsomop_protected"))
  })
})

test_that("sparse keeps the declared roster when no event qualifies", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  sparse <- withr::with_options(list(nfilter.subset = 0),
    .extractTable(
      handle, table = "measurement", concept_filter = 99999999L,
      person_ids = 1:4, representation = "sparse"
    ))

  expect_equal(sparse$personRef$person_id, 1:4)
  expect_equal(sparse$personRef$rowId, 1:4)
  expect_equal(nrow(sparse$covariates), 0L)
  expect_equal(nrow(sparse$covariateRef), 0L)
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

test_that("survival output applies output$filters$custom to outcome events", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  base_plan <- function(custom = NULL) {
    out <- list(
      type = "survival",
      outcome = list(table = "condition_occurrence", concept_set = c(4000002)),
      tar = list(start_offset = 0, end_offset = 730),
      event_order = "first"
    )
    if (!is.null(custom)) out$filters <- list(custom = custom)
    p <- list(
      cohort = list(type = "cohort_table", cohort_definition_id = 1),
      outputs = list(tte = out),
      options = list(translate_concepts = FALSE, block_sensitive = TRUE)
    )
    class(p) <- c("omop_plan", "list")
    p
  }

  withr::with_options(list(nfilter.subset = 3), {
    # Filter that MATCHES every MI row (all carry this type) -> events unchanged.
    keep <- .planExecute(handle,
      base_plan(list(var = "condition_type_concept_id", op = "in",
                     value = list(44818518))),
      list(tte = "tte_df"))$tte
    expect_true(all(c(1, 5, 9) %in% keep$person_id[keep$event == 1]))

    # Filter that matches NO row -> all members censored (filter narrowed events).
    none <- .planExecute(handle,
      base_plan(list(var = "condition_type_concept_id", op = "in",
                     value = list(99999))),
      list(tte = "tte_df"))$tte
    expect_equal(nrow(none), 6)           # still one row per cohort member
    expect_true(all(none$event == 0))     # but no qualifying outcome events
  })
})

test_that("survival custom filter on an identifier column is rejected", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(tte = list(
      type = "survival",
      outcome = list(table = "condition_occurrence", concept_set = c(4000002)),
      tar = list(start_offset = 0, end_offset = 730),
      # person_id is an identifier -> .assertCustomFilterSafe must fail-closed.
      filters = list(custom = list(var = "person_id", op = "in",
                                   value = list(1, 5)))
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3, dsomop.query_strict = TRUE), {
    expect_error(
      .planExecute(handle, plan, list(tte = "tte_df")),
      "not permitted"
    )
  })
  withr::with_options(list(nfilter.subset = 3, dsomop.query_strict = FALSE), {
    expect_warning(
      result <- .planExecute(handle, plan, list(tte = "tte_df")),
      "not permitted"
    )
    expect_null(result$tte)
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
    expect_error(
      .planExecute(handle, plan, list(tte = "tte_df")),
      "requires a cohort"
    )
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

    # Exact cohort dates follow the same privacy default as event outputs.
    expect_true(all(c("row_id", "subject_id", "cohort_definition_id") %in%
                      names(df)))
    expect_false(any(c("cohort_start_date", "cohort_end_date") %in% names(df)))

    # Uses subject_id, NOT person_id
    expect_true("subject_id" %in% names(df))
    expect_false("person_id" %in% names(df))

    # row_id is sequential
    expect_equal(df$row_id, 1:6)

    # cohort_definition_id matches
    expect_true(all(df$cohort_definition_id == 1L))
  })
})

test_that("cohort_membership absolute dates require explicit authorization", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(cm = list(
      type = "cohort_membership", date_handling = list(mode = "absolute")
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3,
                           dsomop.allow_absolute_dates = FALSE,
                           dsomop.query_strict = TRUE), {
    expect_error(.planExecute(handle, plan, list(cm = "cm_df")),
                 "Absolute date handling")
  })
  withr::with_options(list(nfilter.subset = 3,
                           dsomop.allow_absolute_dates = TRUE,
                           dsomop.query_strict = TRUE), {
    result <- .planExecute(handle, plan, list(cm = "cm_df"))$cm
    expect_true(all(c("cohort_start_date", "cohort_end_date") %in%
                      names(result)))
  })
})

test_that("cohort_membership without cohort fails closed", {
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
    expect_error(
      .planExecute(handle, plan, list(cm = "cm_df")),
      "requires a cohort"
    )
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

test_that("intervals_long applies a generic custom date range", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(iv = list(
      type = "intervals_long",
      tables = "condition_occurrence",
      filters = list(custom = list(
        var = "start_date", op = "between",
        value = list("2019-01-01", "2020-12-31")
      ))
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3, dsomop.query_strict = TRUE), {
    result <- .planExecute(handle, plan, list(iv = "iv_df"))$iv
    expect_equal(nrow(result), 20L)
    expect_true(all(result$interval_type == "condition_occurrence"))
  })
})

test_that("intervals_long ANDs global and source-specific filters", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(iv = list(
      type = "intervals_long",
      tables = "condition_occurrence",
      source_filters = list(condition_occurrence = list(
        var = "condition_concept_id", op = "in", value = 201820L
      )),
      filters = list(custom = list(
        var = "start_date", op = "between",
        value = list("2019-01-01", "2020-12-31")
      ))
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3, dsomop.query_strict = TRUE), {
    result <- .planExecute(handle, plan, list(iv = "iv_df"))$iv
    expect_gt(nrow(result), 0L)
    expect_true(all(result$concept_id == 201820L))
    expect_true(all(result$interval_type == "condition_occurrence"))
  })
})

test_that("intervals_long rejects requested tables without interval dates", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(iv = list(type = "intervals_long", tables = "measurement")),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3, dsomop.query_strict = TRUE), {
    expect_error(.planExecute(handle, plan, list(iv = "iv_df")),
                 "no reviewed start/end date pair")
  })
})

test_that("intervals_long without cohort fails closed", {
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
    expect_error(
      .planExecute(handle, plan, list(iv = "iv_df")),
      "requires a cohort"
    )
  })
})

# --- Temporal Covariates Tests ---

.seed_multi_episode_conditions <- function(handle) {
  cohort <- data.frame(
    subject_id = rep(1:3, each = 2L),
    cohort_start_date = rep(c("2020-01-01", "2021-01-01"), 3L),
    cohort_end_date = rep(c("2020-01-10", "2021-01-10"), 3L),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(handle$conn, "multi_episode_cohort", cohort,
                    temporary = TRUE)

  make_event <- function(ids, concept_id, offsets) {
    data.frame(
      condition_occurrence_id = as.integer(ids),
      person_id = cohort$subject_id,
      condition_concept_id = as.integer(concept_id),
      condition_start_date = as.character(
        as.Date(cohort$cohort_start_date) + offsets
      ),
      condition_end_date = as.character(
        as.Date(cohort$cohort_start_date) + offsets
      ),
      condition_type_concept_id = 44818518L,
      visit_occurrence_id = NA_integer_,
      stringsAsFactors = FALSE
    )
  }

  episode_base <- 2000L + (seq_len(nrow(cohort)) - 1L) * 10L
  temporal_ids <- 1000L + seq_len(nrow(cohort))
  first_ids <- episode_base + 1L
  last_low_ids <- episode_base + 2L
  last_high_ids <- episode_base + 3L
  events <- rbind(
    make_event(temporal_ids, 765432L, 5L),
    make_event(first_ids, 765433L, 2L),
    make_event(last_low_ids, 765433L, 7L),
    make_event(last_high_ids, 765433L, 7L)
  )
  DBI::dbWriteTable(handle$conn, "condition_occurrence", events,
                    append = TRUE)

  list(
    temporal_ids = temporal_ids,
    first_ids = first_ids,
    last_ids = last_low_ids
  )
}

test_that("temporal_covariates preserve numeric concepts and cohort episodes", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_multi_episode_conditions(handle)

  withr::with_options(list(nfilter.subset = 3), {
    tc <- .extractTemporalCovariates(
      handle,
      cohort_table = "multi_episode_cohort",
      table = "condition_occurrence",
      concept_filter = 765432L,
      bin_width = 11L,
      window_start = 0L,
      window_end = 10L,
      analyses = c("binary", "count")
    )

    expect_false(anyNA(tc$covariateRef$conceptId))
    expect_setequal(tc$covariateRef$conceptId, 765432L)
    expect_setequal(tc$temporalCovariates$rowId, 1:6)
    expect_equal(tc$personRef$rowId, 1:6)
    expect_equal(tc$personRef$person_id, rep(1:3, each = 2L))

    binary_id <- 765432001
    count_id <- 765432002
    binary <- tc$temporalCovariates[
      tc$temporalCovariates$covariateId == binary_id, , drop = FALSE
    ]
    counts <- tc$temporalCovariates[
      tc$temporalCovariates$covariateId == count_id, , drop = FALSE
    ]
    expect_equal(nrow(binary), 6L)
    expect_equal(nrow(counts), 6L)
    expect_true(all(binary$covariateValue == 1))
    expect_true(all(counts$covariateValue == 1))

    released <- .testPseudonymize(
      tc, .testPseudonymKey("output-contract-temporal")
    )
    expect_true(all(grepl("^p", released$personRef$person_id)))
    expect_equal(released$personRef$rowId, 1:6)
    expect_equal(length(unique(released$personRef$person_id)), 3L)

    # The episode map is a cohort contract, not an event-derived map: an era
    # with no matching event remains linkable even though it has no sparse row.
    DBI::dbExecute(
      handle$conn,
      "DELETE FROM condition_occurrence WHERE condition_occurrence_id = 1006"
    )
    tc_missing <- .extractTemporalCovariates(
      handle, "multi_episode_cohort", "condition_occurrence",
      concept_filter = 765432L, bin_width = 11L,
      window_start = 0L, window_end = 10L, analyses = "binary"
    )
    expect_equal(tc_missing$personRef, tc$personRef)
    expect_setequal(tc_missing$temporalCovariates$rowId, 1:5)
  })
})

test_that("event_select first and last partition by cohort episode", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  expected <- .seed_multi_episode_conditions(handle)

  extract_selected <- function(order) {
    .extractTable(
      handle,
      table = "condition_occurrence",
      columns = c("condition_occurrence_id", "person_id",
                  "condition_concept_id", "condition_start_date"),
      concept_filter = 765433L,
      cohort_table = "multi_episode_cohort",
      temporal = list(
        index_window = list(start = 0L, end = 10L),
        event_select = list(order = order, n = 1L)
      ),
      add_cohort_date = TRUE,
      date_handling = "remove",
      translate_concepts = FALSE,
      block_sensitive = TRUE
    )
  }

  withr::with_options(list(nfilter.subset = 3), {
    first <- extract_selected("first")
    last <- extract_selected("last")

    expect_equal(nrow(first), 6L)
    expect_equal(nrow(last), 6L)
    expect_setequal(first$cohort_row_id, 1:6)
    expect_setequal(last$cohort_row_id, 1:6)
    expect_setequal(first$condition_occurrence_id, expected$first_ids)
    expect_setequal(last$condition_occurrence_id, expected$last_ids)
    expect_false(any(c("rn", "dsomop_event_order_id", "cohort_start_date",
                       "cohort_end_date") %in% names(first)))
    expect_false(any(c("rn", "dsomop_event_order_id", "cohort_start_date",
                       "cohort_end_date") %in% names(last)))
  })
})

test_that("index-window features require and preserve episode grain", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_multi_episode_conditions(handle)

  args <- list(
    handle = handle,
    table = "condition_occurrence",
    concept_filter = 765432L,
    cohort_table = "multi_episode_cohort",
    temporal = list(index_window = list(start = 0L, end = 10L)),
    add_cohort_date = TRUE,
    representation = "features",
    feature_specs = list(n = list(
      type = "count", concept_set = 765432L, name = "n"
    )),
    translate_concepts = FALSE
  )

  expect_error(do.call(.extractTable, args), "grain='episode'")
  args$representation_grain <- "episode"
  features <- withr::with_options(list(nfilter.subset = 3),
    do.call(.extractTable, args))
  expect_equal(features$cohort_row_id, 1:6)
  expect_equal(features$person_id, rep(1:3, each = 2L))
  expect_equal(features$n, rep(1L, 6L))
})

test_that("episode-wide output keeps cohort members without matching events", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_multi_episode_conditions(handle)
  DBI::dbExecute(
    handle$conn,
    "DELETE FROM condition_occurrence WHERE condition_occurrence_id = 1006"
  )

  wide <- withr::with_options(list(nfilter.subset = 3),
    .extractTable(
      handle, table = "condition_occurrence",
      columns = "condition_concept_id", concept_filter = 765432L,
      cohort_table = "multi_episode_cohort",
      temporal = list(
        index_window = list(start = 0L, end = 10L),
        event_select = list(order = "first", n = 1L)
      ),
      representation = "wide", representation_grain = "episode",
      translate_concepts = FALSE
    ))

  expect_equal(wide$cohort_row_id, 1:6)
  expect_equal(wide$person_id, rep(1:3, each = 2L))
  presence <- setdiff(names(wide), c("cohort_row_id", "person_id"))
  expect_length(presence, 1L)
  expect_equal(wide[[presence]], c(rep(1L, 5L), NA_integer_))
})

test_that("episode-grain sparse output preserves recurrent episode linkage", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_multi_episode_conditions(handle)

  args <- list(
    handle = handle,
    table = "condition_occurrence",
    concept_filter = c(765432L, 765433L),
    cohort_table = "multi_episode_cohort",
    temporal = list(index_window = list(start = 0L, end = 10L)),
    representation = "sparse",
    translate_concepts = FALSE
  )
  expect_error(do.call(.extractTable, args), "grain='episode'")

  args$representation_grain <- "episode"
  sparse <- withr::with_options(list(nfilter.subset = 3),
    do.call(.extractTable, args))
  expect_equal(sparse$personRef$rowId, 1:6)
  expect_equal(sparse$personRef$cohort_row_id, 1:6)
  expect_equal(sparse$personRef$person_id, rep(1:3, each = 2L))
  expect_true(all(sparse$covariates$rowId %in% sparse$personRef$rowId))
  expect_setequal(sparse$covariateRef$conceptId, c(765432L, 765433L))

  released <- .testPseudonymize(
    sparse, .testPseudonymKey("output-contract-sparse")
  )
  expect_true(all(grepl("^p", released$personRef$person_id)))
  expect_true(all(c("rowId", "cohort_row_id") %in%
                    attr(released$personRef, "dsomop_protected")))
})

test_that("temporal_covariates returns covariates and episode references", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(
    handle$conn,
    "UPDATE observation_period SET observation_period_start_date = '2018-01-01'"
  )
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
    expect_true(all(c("temporalCovariates", "covariateRef", "timeRef",
                      "personRef") %in%
                      names(tc)))

    # Check column names
    expect_true(all(c("rowId", "timeId", "covariateId", "covariateValue") %in%
                      names(tc$temporalCovariates)))
    expect_true(all(c("covariateId", "covariateName", "analysisId",
                       "conceptId") %in% names(tc$covariateRef)))
    expect_true(all(c("timeId", "startDay", "endDay") %in%
                      names(tc$timeRef)))
    expect_true(all(c("rowId", "person_id") %in% names(tc$personRef)))

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

test_that("temporal_covariates applies custom value bins", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(
    handle$conn,
    "UPDATE observation_period SET observation_period_start_date = '2018-01-01'"
  )
  .buildBlueprint(handle)
  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(tc = list(
      type = "temporal_covariates",
      table = "measurement",
      concept_set = 3004410L,
      bin_width = 600L,
      window_start = -100L,
      window_end = 500L,
      analyses = "binary",
      filters = list(custom = list(
        var = "value_as_number", op = "value_bin",
        value = list(lower = 7.5, upper = 10)
      ))
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3, dsomop.query_strict = TRUE), {
    plan$outputs$tc$filters$custom$safe_scope <- .test_issue_safe_bins(
      handle, c(0, 7.5, 10, 20), concept_id = 3004410L)
    result <- .planExecute(handle, plan, list(tc = "tc_df"))$tc
    expect_equal(sum(result$temporalCovariates$covariateValue), 3)
  })
})

test_that("temporal_covariates validates binning configuration", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  expect_error(
    .extractTemporalCovariates(handle, "unused", "measurement",
                               bin_width = 2.5),
    "finite integer"
  )
  expect_error(
    .extractTemporalCovariates(handle, "unused", "measurement",
                               bin_width = 0L),
    "greater than zero"
  )
  expect_error(
    .extractTemporalCovariates(handle, "unused", "measurement",
                               window_start = 1L, window_end = 0L),
    "not be after"
  )
  expect_error(
    .extractTemporalCovariates(handle, "unused", "measurement",
                               analyses = "median"),
    "binary and count"
  )

  expect_equal(
    .generateTimeWindows(30L, 0L, 0L),
    data.frame(timeId = 1L, startDay = 0L, endDay = 0L)
  )
  boundary_windows <- .generateTimeWindows(10L, 0L, 10L)
  expect_equal(boundary_windows$startDay, c(0L, 10L))
  expect_equal(boundary_windows$endDay, c(9L, 10L))
  boundary_time_id <- floor((10L - 0L) / 10L) + 1L
  expect_equal(boundary_windows$timeId[[boundary_time_id]], 2L)
  expect_equal(boundary_windows$endDay[[boundary_time_id]], 10L)
  expect_error(
    withr::with_options(list(dsomop.max_temporal_bins = 2L),
      .extractTemporalCovariates(
        handle, "unused", "measurement", bin_width = 1L,
        window_start = 0L, window_end = 2L
      )),
    "server cap"
  )
})

test_that("temporal_covariates window is clock-independent (deterministic)", {
  # Regression guard: the index-relative window (days_from_index =
  # event_date - cohort_start_date) and the per-patient disclosure gate must NOT
  # drift with the real calendar date. A previous failure mode under-counted
  # qualifying persons as the wall-clock advanced, eventually tripping the
  # nfilter.subset gate even though ~6 persons legitimately qualified. Here we
  # run the SAME extraction under the real date and under a simulated far-future
  # "today" and require byte-identical results (same persons, same bins).
  extract_tc <- function() {
    handle <- create_test_handle()
    on.exit(cleanup_handle(handle))
    DBI::dbExecute(
      handle$conn,
      "UPDATE observation_period SET observation_period_start_date = '2018-01-01'"
    )
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
      .planExecute(handle, plan, list(tc = "tc_df"))$tc
    })
  }

  tc_now <- suppressWarnings(extract_tc())

  # Six distinct cohort-1 persons have a qualifying condition in [-365, 0] days
  # from index 2020-01-01, so the gate must pass and capture all six.
  expect_true(is.list(tc_now))
  expect_equal(length(unique(tc_now$temporalCovariates$rowId)), 6L)
  expect_equal(sort(unique(tc_now$temporalCovariates$rowId)), 1:6)

  # Simulate a "today" years in the future. If any part of the temporal /
  # cohort-join / observation path leaked Sys.Date(), the captured population
  # (and thus the gate outcome) would change.
  tc_future <- suppressWarnings(
    testthat::with_mocked_bindings(
      extract_tc(),
      Sys.Date = function() as.Date("2099-12-31"),
      .package = "base"
    )
  )

  expect_true(is.list(tc_future))
  expect_equal(length(unique(tc_future$temporalCovariates$rowId)), 6L)

  # The full result must be identical regardless of the wall-clock date.
  expect_equal(tc_future$temporalCovariates, tc_now$temporalCovariates)
  expect_equal(tc_future$covariateRef, tc_now$covariateRef)
  expect_equal(tc_future$timeRef, tc_now$timeRef)
})

test_that("temporal_covariates still fail-closes below disclosure threshold", {
  # The per-patient gate must remain intact: when fewer than nfilter.subset
  # persons legitimately qualify, the output is suppressed (returned as NULL via
  # the per-output handler) rather than disclosed.
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Concept 317009 occurs only on persons {2, 4}, neither of whom is in cohort 1
  # ({1,3,5,7,9,11}); the qualifying population is therefore empty (< threshold).
  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(
      tc = list(
        type = "temporal_covariates",
        table = "condition_occurrence",
        concept_set = c(317009),
        bin_width = 90L,
        window_start = -365L,
        window_end = 0L,
        analyses = c("binary", "count")
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3, dsomop.query_strict = TRUE), {
    expect_error(
      .planExecute(handle, plan, list(tc = "tc_df")),
      "insufficient individuals"
    )
  })
})

test_that("temporal_covariates without cohort fails closed", {
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
    expect_error(
      .planExecute(handle, plan, list(tc = "tc_df")),
      "requires a cohort"
    )
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
        columns = c("gender_concept_id"),
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

test_that("all cohort-dependent outputs reject a plan with no cohort source", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  required <- list(
    baseline = list(type = "baseline"),
    survival = list(
      type = "survival",
      outcome = list(table = "condition_occurrence", concept_set = 201820L)
    ),
    cohort_membership = list(type = "cohort_membership"),
    intervals_long = list(
      type = "intervals_long", tables = "condition_occurrence"
    ),
    temporal_covariates = list(
      type = "temporal_covariates", table = "condition_occurrence",
      concept_set = 201820L
    ),
    person_period = list(
      type = "person_period", table = "condition_occurrence",
      concept_set = 201820L, grain = "episode", time_origin = "index"
    )
  )

  for (name in names(required)) {
    plan <- structure(list(
      cohort = NULL,
      outputs = stats::setNames(list(required[[name]]), name),
      options = list(translate_concepts = FALSE, block_sensitive = TRUE)
    ), class = c("omop_plan", "list"))

    validation <- .planValidate(handle, plan)
    expect_false(validation$valid, info = name)
    expect_match(
      paste(validation$errors, collapse = "\n"),
      paste0("Output '", name,
             "'.*requires a cohort.*no executable cohort"),
      info = name
    )
    expect_error(
      .planPreview(handle, plan),
      paste0("Output '", name,
             "'.*requires a cohort.*no executable cohort"),
      info = name
    )
    withr::with_options(list(dsomop.query_strict = FALSE), {
      expect_error(
        .planExecute(handle, plan, stats::setNames(list("out"), name)),
        paste0("Output '", name, "'.*requires a cohort"),
        info = name
      )
    })
  }
})

test_that("resolved cohort preflight rejects a source that cannot materialize", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(handle$conn, "DROP TABLE observation_period")
  handle$blueprint <- NULL
  plan <- structure(list(
    cohort = list(
      type = "spec",
      filter_tree = list(type = "sex", params = list(value = "F"))
    ),
    outputs = list(demo = list(type = "baseline")),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  ), class = c("omop_plan", "list"))

  withr::with_options(list(
    nfilter.subset = 3,
    dsomop.query_strict = FALSE
  ), {
    expect_error(
      .planExecute(handle, plan, list(demo = "demo_df")),
      "did not resolve to an executable cohort table"
    )
  })
})

test_that("validation and preview reject a missing results cohort table", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(handle$conn, "DROP TABLE cohort")
  handle$blueprint <- NULL
  plan <- structure(list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    outputs = list(demo = list(type = "baseline"))
  ), class = c("omop_plan", "list"))

  validation <- .planValidate(handle, plan)
  expect_false(validation$valid)
  expect_match(validation$errors, "Plan cohort cannot execute")
  expect_error(.planPreview(handle, plan), "Plan cohort cannot execute")
})

test_that("failed cohort preflight cannot leave an old output looking fresh", {
  handle <- create_test_handle()
  handle_symbol <- paste0("cohort_preflight_", Sys.getpid())
  .setHandle(handle_symbol, handle)
  on.exit(.removeHandle(handle_symbol), add = TRUE)
  old_output <- structure("unchanged", marker = TRUE)
  plan <- structure(list(
    cohort = NULL,
    outputs = list(old = list(type = "baseline")),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  ), class = c("omop_plan", "list"))

  withr::with_options(list(dsomop.query_strict = FALSE), {
    expect_error(
      omopPlanExecuteDS(
        handle_symbol, plan, out = list(old = "old_output")
      ),
      "requires a cohort"
    )
  })
  expect_identical(old_output, structure("unchanged", marker = TRUE))
})

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
  expect_false(result$valid)
  expect_match(result$errors,
               "requires a cohort.*no executable cohort")
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
  expect_false(result$valid)
  expect_match(result$errors,
               "requires a cohort.*no executable cohort")
})

test_that("planPreview handles new output types", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
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
