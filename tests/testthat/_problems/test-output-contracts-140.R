# Extracted from test-output-contracts.R:140

# test -------------------------------------------------------------------------
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
