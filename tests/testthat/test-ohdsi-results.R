# ==============================================================================
# Tests for OHDSI Results Consumer Framework
# ==============================================================================

# --- Registry Tests ---

test_that("ohdsi_tool_registry returns expected structure", {
  registry <- .ohdsi_tool_registry()
  expect_type(registry, "list")
  expect_true(all(c("dqd", "cohort_diagnostics", "cohort_incidence",
                     "characterization") %in% names(registry)))

  for (tid in names(registry)) {
    tool <- registry[[tid]]
    expect_true("tool_name" %in% names(tool))
    expect_true("table_names" %in% names(tool))
    expect_true("count_columns" %in% names(tool))
    expect_true("sensitive_columns" %in% names(tool))
    expect_type(tool$table_names, "character")
    expect_true(length(tool$table_names) > 0)
  }
})

test_that("ohdsi_table_to_tool maps known tables correctly", {
  expect_equal(.ohdsi_table_to_tool("dqdashboard_results"), "dqd")
  expect_equal(.ohdsi_table_to_tool("cohort_count"), "cohort_diagnostics")
  expect_equal(.ohdsi_table_to_tool("incidence_rate"), "cohort_diagnostics")
  expect_equal(.ohdsi_table_to_tool("incidence_summary"), "cohort_incidence")
  expect_equal(.ohdsi_table_to_tool("c_cohort_counts"), "characterization")
  expect_equal(.ohdsi_table_to_tool("c_covariates"), "characterization")
  expect_null(.ohdsi_table_to_tool("unknown_table"))
})

# --- Discovery Tests ---

test_that("ohdsiFindResultTables discovers OHDSI tables in test DB", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  found <- .ohdsiFindResultTables(handle)
  expect_s3_class(found, "data.frame")
  expect_true(nrow(found) > 0)
  expect_true(all(c("table_name", "tool_id", "tool_name",
                     "qualified_name", "n_rows") %in% names(found)))

  # Should find DQD table
  expect_true("dqdashboard_results" %in% found$table_name)
  expect_true("dqd" %in% found$tool_id)

  # Should find CohortDiagnostics tables
  expect_true("cohort_count" %in% found$table_name)
  expect_true("incidence_rate" %in% found$table_name)

  # Should find CohortIncidence table
  expect_true("incidence_summary" %in% found$table_name)

  # Should find Characterization tables
  expect_true("c_cohort_counts" %in% found$table_name)
  expect_true("c_covariates" %in% found$table_name)

  # Row counts should be positive
  dqd_rows <- found$n_rows[found$table_name == "dqdashboard_results"]
  expect_true(dqd_rows > 0)
})

test_that("ohdsiFindResultTables returns empty when no OHDSI tables", {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conn))

  DBI::dbExecute(conn, "CREATE TABLE person (person_id INTEGER PRIMARY KEY)")
  DBI::dbExecute(conn, "CREATE TABLE concept (concept_id INTEGER PRIMARY KEY,
    concept_name TEXT, domain_id TEXT, vocabulary_id TEXT, concept_class_id TEXT,
    standard_concept TEXT, concept_code TEXT, valid_start_date TEXT,
    valid_end_date TEXT, invalid_reason TEXT)")
  DBI::dbExecute(conn, "CREATE TABLE cdm_source (cdm_source_name TEXT,
    cdm_source_abbreviation TEXT, cdm_holder TEXT, source_description TEXT,
    cdm_version TEXT, vocabulary_version TEXT)")
  DBI::dbWriteTable(conn, "cdm_source", data.frame(
    cdm_source_name = "Test", cdm_source_abbreviation = "T",
    cdm_holder = "T", source_description = "T",
    cdm_version = "v5.4", vocabulary_version = "v5.0",
    stringsAsFactors = FALSE), append = TRUE)

  handle <- new.env(parent = emptyenv())
  handle$conn <- conn
  handle$dbms <- "sqlite"
  handle$target_dialect <- "sqlite"
  handle$cdm_schema <- NULL
  handle$vocab_schema <- NULL
  handle$results_schema <- NULL
  handle$temp_schema <- NULL
  handle$resource_client <- NULL
  handle$config <- list()
  handle$blueprint <- NULL
  handle$temp_tables <- character(0)

  .buildBlueprint(handle)

  found <- .ohdsiFindResultTables(handle)
  expect_equal(nrow(found), 0)
})

# --- Count Column Detection ---

test_that("ohdsiDetectCountColumns finds registry-based columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # DQD columns
  cols <- .ohdsiDetectCountColumns(handle, "dqdashboard_results", "dqd")
  expect_true("num_violated_rows" %in% cols)
  expect_true("num_denominator_rows" %in% cols)
})

test_that("ohdsiDetectCountColumns uses heuristic fallback", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # cohort_count has cohort_entries/cohort_subjects from registry
  cols <- .ohdsiDetectCountColumns(handle, "cohort_count", "cohort_diagnostics")
  expect_true(length(cols) > 0)
})

# --- Generic Query ---

test_that("ohdsiGetResults returns DQD data", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "dqdashboard_results")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("check_name" %in% names(result))
  expect_true("category" %in% names(result))
})

test_that("ohdsiGetResults excludes sensitive columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "dqdashboard_results")
  # query_text is a sensitive column for DQD
  expect_false("query_text" %in% names(result))
})

test_that("ohdsiGetResults applies disclosure control", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # DQD fixture has num_violated_rows = 1 and 2 (below threshold 3)
  # Those rows should be dropped
  result <- .ohdsiGetResults(handle, "dqdashboard_results")
  expect_true(nrow(result) > 0)
  # All surviving rows should have count columns >= threshold
  if ("num_violated_rows" %in% names(result)) {
    expect_true(all(result$num_violated_rows >= 3))
  }
  # Rows with num_violated_rows = 1 or 2 should not be present
  expect_true(nrow(result) < 8)  # 8 total, some suppressed
})

test_that("ohdsiGetResults respects filters", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "dqdashboard_results",
                              filters = list(category = "Completeness"))
  expect_true(nrow(result) > 0)
  expect_true(all(result$category == "Completeness"))
})

test_that("ohdsiGetResults respects limit", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "dqdashboard_results", limit = 2L)
  expect_true(nrow(result) <= 2)
})

test_that("ohdsiGetResults respects order_by", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "incidence_rate",
                              order_by = "cohort_count")
  if (nrow(result) > 1) {
    # Should be ordered by cohort_count ascending
    counts <- result$cohort_count[!is.na(result$cohort_count)]
    if (length(counts) > 1) {
      expect_true(all(diff(counts) >= 0))
    }
  }
})

test_that("ohdsiGetResults selects specific columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "dqdashboard_results",
                              columns = c("check_name", "category"))
  expect_true(all(names(result) %in% c("check_name", "category")))
})

test_that("ohdsiGetResults errors on nonexistent table", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(.ohdsiGetResults(handle, "nonexistent_table"),
               "not found")
})

test_that("ohdsiGetResults errors on invalid table name", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(.ohdsiGetResults(handle, "DROP TABLE; --"),
               "Invalid")
})

test_that("ohdsiGetResults returns empty data.frame for empty table", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  # Create an empty OHDSI-like table
  DBI::dbExecute(handle$conn, "
    CREATE TABLE target_def (
      target_id INTEGER,
      target_name TEXT
    )")
  handle$blueprint <- NULL  # force rebuild
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "target_def")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

# --- Status ---

test_that("ohdsiStatus reports tool availability correctly", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  status <- .ohdsiStatus(handle)
  expect_type(status, "list")
  expect_true("dqd" %in% names(status))
  expect_true("cohort_diagnostics" %in% names(status))
  expect_true("cohort_incidence" %in% names(status))
  expect_true("characterization" %in% names(status))

  # DQD should be available
  expect_true(status$dqd$available)
  expect_true("dqdashboard_results" %in% status$dqd$tables)
  expect_true(status$dqd$total_rows > 0)

  # CohortDiagnostics should be available
  expect_true(status$cohort_diagnostics$available)

  # CohortIncidence should be available
  expect_true(status$cohort_incidence$available)

  # Characterization should be available
  expect_true(status$characterization$available)
})

# --- Summary ---

test_that("ohdsiGetSummary returns tool-specific info", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  summary <- .ohdsiGetSummary(handle, "dqd")
  expect_equal(summary$tool_id, "dqd")
  expect_true(summary$available)
  expect_s3_class(summary$tables, "data.frame")
  expect_true(nrow(summary$tables) > 0)
})

test_that("ohdsiGetSummary errors on unknown tool", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  expect_error(.ohdsiGetSummary(handle, "unknown_tool"),
               "Unknown tool_id")
})

# --- Blueprint Integration ---

test_that("blueprint discovers OHDSI result tables", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  bp <- .buildBlueprint(handle)
  ohdsi_tables <- c("dqdashboard_results", "cohort_count", "incidence_rate",
                     "incidence_summary", "c_cohort_counts", "c_covariates")
  found_in_bp <- intersect(ohdsi_tables, bp$tables$table_name)
  expect_true(length(found_in_bp) > 0)
  expect_true(handle$has_ohdsi_results)
})

# --- CohortDiagnostics Query ---

test_that("ohdsiGetResults works for cohort_count", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "cohort_count")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("cohort_id" %in% names(result))
})

test_that("ohdsiGetResults works for incidence_rate", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "incidence_rate")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

# --- CohortIncidence Query ---

test_that("ohdsiGetResults works for incidence_summary", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "incidence_summary")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("persons_at_risk" %in% names(result))
})

# --- Characterization Query ---

test_that("ohdsiGetResults works for c_cohort_counts", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "c_cohort_counts")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("ohdsiGetResults works for c_covariates", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "c_covariates")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("covariate_name" %in% names(result))
})

# --- Disclosure Control Details ---

test_that("cohort_count with count below threshold is suppressed", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Fixture has cohort_id=2 with cohort_entries=2, cohort_subjects=2
  # These are below threshold (3) and should be dropped
  result <- .ohdsiGetResults(handle, "cohort_count")
  # Cohort 2 should be suppressed (entries=2, subjects=2 < threshold 3)
  cohort2 <- result[result$cohort_id == 2, ]
  expect_equal(nrow(cohort2), 0)
})

test_that("c_cohort_counts with small num_persons is suppressed", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Fixture has cohort_id=2 with num_persons=1 (below threshold)
  result <- .ohdsiGetResults(handle, "c_cohort_counts")
  cohort2 <- result[result$cohort_id == 2, ]
  expect_equal(nrow(cohort2), 0)
})

test_that("incidence_summary small person_outcomes suppressed", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Fixture outcome_id=20: person_outcomes=1, outcomes=1 (below threshold)
  result <- .ohdsiGetResults(handle, "incidence_summary")
  outcome20 <- result[result$outcome_id == 20, ]
  expect_equal(nrow(outcome20), 0)
})
