# Extracted from test-output-contracts.R:50

# test -------------------------------------------------------------------------
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
    expect_true(all(df$prior_observation >= 0))
    expect_true(all(df$future_observation > 0))

    # row_id is sequential
    expect_equal(df$row_id, 1:6)
  })
