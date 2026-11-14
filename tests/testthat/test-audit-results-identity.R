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

test_that("only contracted optional columns are completed with their declared type", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn, "ALTER TABLE cm_result DROP COLUMN rr")
  DBI::dbExecute(h$conn, "ALTER TABLE index_event_breakdown DROP COLUMN concept_name")
  .buildBlueprint(h)
  for (empty in c(FALSE, TRUE)) {
    if (empty) {
      DBI::dbExecute(h$conn, "DELETE FROM cm_result")
      DBI::dbExecute(h$conn, "DELETE FROM index_event_breakdown")
    }
    cm <- .ohdsiGetResults(h, "cm_result")
    cd <- .ohdsiGetResults(h, "index_event_breakdown")
    expect_type(cm$rr, "double")
    expect_type(cd$concept_name, "character")
    expect_true(all(is.na(cm$rr)))
    expect_true(all(is.na(cd$concept_name)))
  }
})

test_that("schema discovery describes layout status without inventing upstream versions", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  supported <- .ohdsiInspectResultSchema(h, "cm_result")
  expect_identical(supported$status, "supported")
  expect_identical(supported$upstream_version, "unknown")
  expect_match(supported$structural_fingerprint, "^[0-9a-f]{64}$")
  expect_identical(.ohdsiInspectResultSchema(h, "es_cm_result")$status, "admin_only")
  expect_identical(.ohdsiInspectResultSchema(h, "cm_unregistered")$status, "unknown")
  DBI::dbExecute(h$conn, "ALTER TABLE cm_result RENAME COLUMN target_id TO target_comparator_id")
  recognized <- .ohdsiInspectResultSchema(h, "cm_result")
  expect_identical(recognized$status, "recognized_unsupported")
  expect_identical(recognized$upstream_version, "unknown")
  expect_false(identical(recognized$structural_fingerprint, supported$structural_fingerprint))
  status <- .ohdsiStatus(h)
  expect_identical(status$cohort_method$schema_contracts$schema_status[
    status$cohort_method$schema_contracts$table_name == "cm_result"], "recognized_unsupported")
  expect_true(all(c("table_name", "schema_status", "layout_id", "upstream_version",
    "structural_fingerprint") %in% names(status$cohort_method$schema_contracts)))
  expect_false(any(c("n_rows", "qualified_name") %in% names(status$cohort_method$schema_contracts)))
})

test_that("physical fingerprints are metadata-only and independent of column order", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  before <- .ohdsiInspectResultSchema(h, "cm_result")
  DBI::dbExecute(h$conn, "DELETE FROM cm_result")
  expect_identical(.ohdsiInspectResultSchema(h, "cm_result"), before)
  real <- .listColumnsRaw
  local_mocked_bindings(.listColumnsRaw = function(...) {
    info <- real(...)
    info[rev(seq_len(nrow(info))), , drop = FALSE]
  })
  expect_identical(.ohdsiInspectResultSchema(h, "cm_result"), before)
  manifest <- .ohdsiResultSchemaManifest()
  expect_identical(manifest$cm_result$relationships$unique_row,
    c("analysis_id", "target_id", "comparator_id", "outcome_id"))
  expect_identical(manifest$cm_lookup$status, "recognized_unsupported")
})

test_that("duplicate identities refuse before projection filters and result limits", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  expect_gt(nrow(.ohdsiGetResults(h, "cm_result")), 0L)
  DBI::dbExecute(h$conn, "INSERT INTO cm_result SELECT * FROM cm_result WHERE analysis_id = 2")
  for (columns in list(NULL, "rr")) {
    err <- tryCatch(.ohdsiGetResults(h, "cm_result", columns = columns,
      filters = list(analysis_id = 1L), limit = 1L), error = identity)
    expect_s3_class(err, "error")
    expect_identical(conditionMessage(err), paste0(
      "results_schema_integrity: required row identities are missing or invalid; ",
      "use a complete supported physical export."))
  }
})

test_that("wrong optional physical types refuse before data access", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn, "ALTER TABLE cm_result DROP COLUMN rr")
  DBI::dbExecute(h$conn, "ALTER TABLE cm_result ADD COLUMN rr TEXT")
  .buildBlueprint(h)
  local_mocked_bindings(.executeQuery = function(...) stop("RESULT_ROWS_READ"))
  expect_identical(.ohdsiInspectResultSchema(h, "cm_result")$status, "unknown")
  expect_error(.ohdsiGetResults(h, "cm_result"), "unsupported_results_schema")
})

test_that("schema status gates discovery without reading unsupported result rows", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn, "ALTER TABLE cm_result RENAME COLUMN target_id TO target_comparator_id")
  queries <- character()
  real <- .executeQuery
  local_mocked_bindings(.executeQuery = function(handle, sql, ...) {
    queries <<- c(queries, sql)
    real(handle, sql, ...)
  })
  status <- .ohdsiStatus(h)
  expect_false(status$cohort_method$available)
  expect_false(any(grepl("FROM [\"`]?cm_result", queries)))
  expect_identical(status$cohort_method$schema_contracts$schema_status[
    status$cohort_method$schema_contracts$table_name == "cm_result"], "recognized_unsupported")
})

test_that("invalid keys outside a caller slice cannot bypass whole-relation integrity", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  for (literal in c("NULL", "1.5", "'AUDIT_INVALID_ID'")) {
    DBI::dbExecute(h$conn, paste0("UPDATE cm_result SET target_id = ", literal, " WHERE analysis_id = 2"))
    expect_error(.ohdsiGetResults(h, "cm_result", columns = "rr", limit = 1L,
      filters = list(analysis_id = 1L)), "^results_schema_integrity:")
  }
  DBI::dbExecute(h$conn, "UPDATE cm_result SET target_id = 1")
  result <- .ohdsiGetResults(h, "cm_result")
  expect_identical(attr(result, "dsomop.results_schema")$status, "supported")
  expect_identical(attr(result, "dsomop.results_schema")$upstream_version, "unknown")
})

test_that("uncontracted columns make the physical layout unknown before any result access", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  original <- .ohdsiInspectResultSchema(h, "cm_result")
  expect_identical(original$status, "supported")
  DBI::dbExecute(h$conn, "ALTER TABLE cm_result ADD COLUMN uncontracted_semantic_dimension INTEGER")
  .buildBlueprint(h)
  queries <- character()
  real <- .executeQuery
  local_mocked_bindings(.executeQuery = function(handle, sql, ...) {
    queries <<- c(queries, sql)
    real(handle, sql, ...)
  })
  observed <- .ohdsiInspectResultSchema(h, "cm_result")
  expect_identical(observed$status, "unknown")
  expect_false(identical(original$structural_fingerprint, observed$structural_fingerprint))
  expect_false(.ohdsiStatus(h)$cohort_method$available)
  expect_false(any(grepl("FROM [\"`]?cm_result", queries)))
  local_mocked_bindings(.executeQuery = function(...) stop("RESULT_ROWS_READ"))
  expect_error(.ohdsiGetResults(h, "cm_result"), "^unsupported_results_schema:")
  expect_error(.ohdsiGetResults(h, "cm_result", columns = "rr"), "^unsupported_results_schema:")
  expect_error(.ohdsiResultPoolingContract(h, "cm_result"), "^unsupported_results_schema:")
})
