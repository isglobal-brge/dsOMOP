# Extracted from test-output-contracts.R:541

# test -------------------------------------------------------------------------
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
