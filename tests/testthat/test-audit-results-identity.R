test_that("the pinned CohortMethod 6.0.3 schema refuses without reading results", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  spec <- read.csv(test_path("..", "fixtures", "ohdsi-audit", "cohortmethod-6.0.3-schema.csv"))
  for (table in unique(spec$table_name)) {
    DBI::dbExecute(h$conn, paste("DROP TABLE IF EXISTS", table))
    cols <- spec[spec$table_name == table, ]
    DBI::dbExecute(h$conn, paste0("CREATE TABLE ", table, " (",
      paste(cols$column_name, cols$data_type, collapse = ", "), ")"))
  }
  .buildBlueprint(h)
  local_mocked_bindings(.executeQuery = function(...) stop("RESULT_ROWS_READ"))
  expect_error(.ohdsiGetResults(h, "cm_result"), "unsupported_results_schema.*cm_target_comparator")
  expect_error(.ohdsiResultPoolingContract(h, "cm_result"), "unsupported_results_schema")
})

test_that("missing mandatory identities refuse before optional completion even for empty tables", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn, "ALTER TABLE cm_result RENAME COLUMN target_id TO unsupported_arm")
  DBI::dbExecute(h$conn, "DELETE FROM cm_result")
  .buildBlueprint(h)
  local_mocked_bindings(.executeQuery = function(...) stop("RESULT_ROWS_READ"))
  expect_error(.ohdsiGetResults(h, "cm_result"), "unsupported_results_schema")
  expect_error(.ohdsiGetResults(h, "cm_result", columns = "rr"), "unsupported_results_schema")
  expect_error(.ohdsiResultPoolingContract(h, "cm_result"), "unsupported_results_schema")
})

test_that("invalid private row identities are never exposed by projection or completion", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn, "UPDATE cm_result SET target_id = NULL")
  .buildBlueprint(h)
  expect_error(.ohdsiGetResults(h, "cm_result", columns = "rr"), "results_schema_integrity")
  DBI::dbExecute(h$conn, "UPDATE cm_result SET target_id = 100")
  expect_true(nrow(.ohdsiGetResults(h, "cm_result")) > 0L)
  DBI::dbExecute(h$conn, "UPDATE cm_result SET target_subjects = -1")
  expect_error(.ohdsiGetResults(h, "cm_result"), "unsupported_results_suppression")
})

test_that("supported empty physical schemas retain declared columns", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn, "DELETE FROM cm_result")
  result <- .ohdsiGetResults(h, "cm_result")
  expect_equal(nrow(result), 0L)
  expect_named(result, .ohdsiReviewedContracts()$cohort_method$cm_result$public_columns)
})

test_that("identity physical types are checked without reading an empty table", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn, "DROP TABLE cm_result")
  DBI::dbExecute(h$conn, paste0("CREATE TABLE cm_result (analysis_id INTEGER, target_id REAL, ",
    "comparator_id INTEGER, outcome_id INTEGER, target_subjects INTEGER, ",
    "comparator_subjects INTEGER, target_outcomes INTEGER, comparator_outcomes INTEGER)"))
  .buildBlueprint(h)
  local_mocked_bindings(.executeQuery = function(...) stop("RESULT_ROWS_READ"))
  expect_error(.ohdsiGetResults(h, "cm_result"), "unsupported physical type")
})
