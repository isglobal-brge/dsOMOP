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

test_that("strict SQLite identities refuse malformed storage before projection", {
  withr::local_options(dsomop.query_strict = TRUE)
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  .buildBlueprint(h)
  expect_true(.ohdsiResolveResultContract("cm_result")$strict)
  message <- paste0("results_schema_integrity: required row identities are missing or invalid; ",
                    "use a complete supported physical export.")
  # Mixed columns matter: DBI may otherwise coerce later REAL/TEXT values to
  # the type inferred from an earlier valid integer row.
  for (literal in c("1.5", "'AUDIT_INVALID_ID'", "NULL", "X'313030'")) {
    for (selection in c("all", "one")) {
      DBI::dbExecute(h$conn, "UPDATE cm_result SET target_id = 100")
      predicate <- if (selection == "one") " WHERE rowid = (SELECT MAX(rowid) FROM cm_result)" else ""
      DBI::dbExecute(h$conn, paste0("UPDATE cm_result SET target_id = ", literal, predicate))
      for (columns in list(NULL, "rr")) {
        err <- tryCatch(.ohdsiGetResults(h, "cm_result", columns = columns), error = identity)
        expect_s3_class(err, "error")
        expect_identical(conditionMessage(err), message)
      }
    }
  }
})

test_that("valid SQLite BIGINT identities survive exactly and remain private when projected", {
  withr::local_options(dsomop.query_strict = TRUE)
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  .buildBlueprint(h)
  for (key in c("0", "2147483647", "2147483648", "9007199254740993",
                "9223372036854775807", "-9223372036854775808")) {
    DBI::dbExecute(h$conn, paste0("UPDATE cm_result SET target_id = ", key))
    result <- .ohdsiGetResults(h, "cm_result")
    expect_gt(nrow(result), 0L)
    expect_true(all(as.character(result$target_id) == key))
    projected <- .ohdsiGetResults(h, "cm_result", columns = "rr")
    expect_named(projected, "rr")
    expect_equal(projected$rr, result$rr)
  }
})

test_that("integer identity contract validates exact representations without coercion", {
  valid <- list(integer(), character(), 0L, 42, 9007199254740991,
                c("0", "2147483648", "9007199254740993", "9223372036854775807",
                  "-9223372036854775808"), bit64::as.integer64("9007199254740993"))
  for (x in valid) expect_true(.ohdsiExactIntegerIdentity(x))
  invalid <- list(1.5, Inf, NaN, NA_integer_, TRUE, factor("1"), as.Date("2020-01-01"),
                  9007199254740992, "9223372036854775808", "-9223372036854775809",
                  "", "01", "+1", "-0", "1.0", "1e3", " 1", "1 ", "AUDIT_INVALID_ID",
                  list(1L), bit64::as.integer64(NA))
  for (x in invalid) expect_false(.ohdsiExactIntegerIdentity(x))
  expect_error(.ohdsiValidateResultIdentity(data.frame(database_id = 1L),
    "database_id", character()), "results_schema_integrity")
})
