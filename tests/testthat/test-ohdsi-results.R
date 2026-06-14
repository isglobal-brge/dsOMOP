# ==============================================================================
# Tests for OHDSI Results Consumer Framework
# ==============================================================================

# --- Registry Tests ---

test_that("ohdsi_tool_registry returns expected structure", {
  registry <- .ohdsi_tool_registry()
  expect_type(registry, "list")
  expect_true(all(c("cohort_diagnostics", "cohort_incidence",
                     "characterization", "cohort_method", "sccs",
                     "plp", "evidence_synthesis") %in% names(registry)))
  # DQD was removed from the registry: no dqd tool, no dqdashboard_results table.
  expect_false("dqd" %in% names(registry))

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
  # DQD removed: dqdashboard_results no longer maps to any registered tool.
  expect_null(.ohdsi_table_to_tool("dqdashboard_results"))
  expect_equal(.ohdsi_table_to_tool("cohort_count"), "cohort_diagnostics")
  expect_equal(.ohdsi_table_to_tool("incidence_rate"), "cohort_diagnostics")
  expect_equal(.ohdsi_table_to_tool("incidence_summary"), "cohort_incidence")
  expect_equal(.ohdsi_table_to_tool("c_cohort_counts"), "characterization")
  expect_equal(.ohdsi_table_to_tool("c_covariates"), "characterization")
  expect_null(.ohdsi_table_to_tool("unknown_table"))
})

test_that("ohdsi_tool_registry includes CohortMethod with correct tables", {
  registry <- .ohdsi_tool_registry()
  cm <- registry$cohort_method
  expect_equal(cm$tool_name, "CohortMethod")
  expect_true("cm_result" %in% cm$table_names)
  expect_true("cm_diagnostics_summary" %in% cm$table_names)
  expect_equal(length(cm$table_names), 8)
  expect_true("target_subjects" %in% cm$count_columns)
})

test_that("ohdsi_tool_registry includes SCCS with correct tables", {
  registry <- .ohdsi_tool_registry()
  sccs <- registry$sccs
  expect_equal(sccs$tool_name, "Self-Controlled Case Series")
  expect_true("sccs_result" %in% sccs$table_names)
  expect_true("sccs_diagnostics_summary" %in% sccs$table_names)
  expect_equal(length(sccs$table_names), 4)
  expect_true("outcome_subjects" %in% sccs$count_columns)
})

test_that("ohdsi_tool_registry includes PLP with correct tables", {
  registry <- .ohdsi_tool_registry()
  plp <- registry$plp
  expect_equal(plp$tool_name, "Patient-Level Prediction")
  expect_true("plp_performances" %in% plp$table_names)
  expect_true("plp_model_design" %in% plp$table_names)
  expect_equal(length(plp$table_names), 7)
  expect_true("population_size" %in% plp$count_columns)
})

test_that("ohdsi_tool_registry includes EvidenceSynthesis with correct tables", {
  registry <- .ohdsi_tool_registry()
  es <- registry$evidence_synthesis
  expect_equal(es$tool_name, "Evidence Synthesis")
  expect_true("es_cm_result" %in% es$table_names)
  expect_true("es_sccs_result" %in% es$table_names)
  expect_equal(length(es$table_names), 4)
  expect_true("n_databases" %in% es$count_columns)
})

test_that("ohdsi_table_to_tool maps new tool tables correctly", {
  expect_equal(.ohdsi_table_to_tool("cm_result"), "cohort_method")
  expect_equal(.ohdsi_table_to_tool("cm_diagnostics_summary"), "cohort_method")
  expect_equal(.ohdsi_table_to_tool("sccs_result"), "sccs")
  expect_equal(.ohdsi_table_to_tool("sccs_diagnostics_summary"), "sccs")
  expect_equal(.ohdsi_table_to_tool("plp_performances"), "plp")
  expect_equal(.ohdsi_table_to_tool("plp_model_design"), "plp")
  expect_equal(.ohdsi_table_to_tool("es_cm_result"), "evidence_synthesis")
  expect_equal(.ohdsi_table_to_tool("es_sccs_result"), "evidence_synthesis")
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

  # DQD was removed from the registry: although the fixture still has a
  # dqdashboard_results table, discovery must IGNORE it (only registered OHDSI
  # result tables are surfaced).
  expect_false("dqdashboard_results" %in% found$table_name)
  expect_false("dqd" %in% found$tool_id)

  # Should find CohortDiagnostics tables
  expect_true("cohort_count" %in% found$table_name)
  expect_true("incidence_rate" %in% found$table_name)

  # Should find CohortIncidence table
  expect_true("incidence_summary" %in% found$table_name)

  # Should find Characterization tables
  expect_true("c_cohort_counts" %in% found$table_name)
  expect_true("c_covariates" %in% found$table_name)

  # Should find CohortMethod tables
  expect_true("cm_result" %in% found$table_name)
  expect_true("cm_diagnostics_summary" %in% found$table_name)

  # Should find SCCS tables
  expect_true("sccs_result" %in% found$table_name)

  # Should find PLP tables
  expect_true("plp_performances" %in% found$table_name)

  # Should find Evidence Synthesis tables
  expect_true("es_cm_result" %in% found$table_name)
  expect_true("es_sccs_result" %in% found$table_name)

  # Row counts should be positive
  cc_rows <- found$n_rows[found$table_name == "cohort_count"]
  expect_true(cc_rows > 0)
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

  # Registry-declared count columns for a CohortDiagnostics table.
  cols <- .ohdsiDetectCountColumns(handle, "cohort_count", "cohort_diagnostics")
  expect_true("cohort_entries" %in% cols)
  expect_true("cohort_subjects" %in% cols)
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

test_that("ohdsiGetResults respects filters", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # cohort_count has a filterable database_id column; cohort 1 (85 subjects)
  # clears the count threshold so a row survives the filter + person gate.
  result <- .ohdsiGetResults(handle, "cohort_count",
                              filters = list(database_id = "test_db"))
  expect_true(nrow(result) > 0)
  expect_true(all(result$database_id == "test_db"))
})

test_that("ohdsiGetResults respects limit", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "cohort_count", limit = 1L)
  expect_true(nrow(result) <= 1)
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

  result <- .ohdsiGetResults(handle, "cohort_count",
                              columns = c("cohort_id", "database_id"))
  expect_true(all(names(result) %in% c("cohort_id", "database_id")))
})

test_that("ohdsiGetResults errors on non-allowlisted table", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Tables not in the OHDSI tool registry are rejected (allowlist enforcement)
  expect_error(.ohdsiGetResults(handle, "nonexistent_table"),
               "not a registered OHDSI result table")
})

test_that("ohdsiGetResults rejects raw CDM tables via allowlist", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # 'person' exists in the DB but is NOT an OHDSI result table
  expect_error(.ohdsiGetResults(handle, "person"),
               "not a registered OHDSI result table")
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
  # DQD was removed from the registry: no dqd status entry.
  expect_false("dqd" %in% names(status))
  expect_true("cohort_diagnostics" %in% names(status))
  expect_true("cohort_incidence" %in% names(status))
  expect_true("characterization" %in% names(status))
  expect_true("cohort_method" %in% names(status))
  expect_true("sccs" %in% names(status))
  expect_true("plp" %in% names(status))
  expect_true("evidence_synthesis" %in% names(status))

  # CohortDiagnostics should be available
  expect_true(status$cohort_diagnostics$available)

  # CohortIncidence should be available
  expect_true(status$cohort_incidence$available)

  # Characterization should be available
  expect_true(status$characterization$available)

  # CohortMethod should be available
  expect_true(status$cohort_method$available)

  # SCCS should be available
  expect_true(status$sccs$available)

  # PLP should be available
  expect_true(status$plp$available)

  # Evidence Synthesis should be available
  expect_true(status$evidence_synthesis$available)
})

# --- Summary ---

test_that("ohdsiGetSummary returns tool-specific info", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  summary <- .ohdsiGetSummary(handle, "cohort_diagnostics")
  expect_equal(summary$tool_id, "cohort_diagnostics")
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
  ohdsi_tables <- c("cohort_count", "incidence_rate",
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

  # incidence_rate carries cohort_count but no person column, so strict mode
  # fail-closes it (covered separately); verify the consumer with strict off.
  withr::with_options(list(dsomop.query_strict = FALSE), {
    result <- .ohdsiGetResults(handle, "incidence_rate")
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
  })
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

  # This fixture's c_covariates carries sum_value but no person column, so the
  # person gate fail-closes it in strict mode; verify the consumer with it off.
  withr::with_options(list(dsomop.query_strict = FALSE), {
    result <- .ohdsiGetResults(handle, "c_covariates")
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true("covariate_name" %in% names(result))
  })
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

# --- CohortDiagnostics New Table Queries ---

test_that("ohdsiGetResults works for index_event_breakdown", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "index_event_breakdown")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("concept_id" %in% names(result))
  expect_true("concept_name" %in% names(result))
})

test_that("ohdsiGetResults works for visit_context", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "visit_context")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("visit_concept_id" %in% names(result))
  expect_true("subjects" %in% names(result))
})

test_that("ohdsiGetResults works for temporal_covariate_value", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "temporal_covariate_value")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("time_id" %in% names(result))
  expect_true("mean" %in% names(result))
})

test_that("ohdsiGetResults works for time_series", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "time_series")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("calendar_year" %in% names(result))
})

test_that("ohdsiGetResults works for included_source_concept", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Concept-reference rows (concept_count, no person column) fail-close in strict
  # mode; verify the consumer shapes the result with strict mode off.
  withr::with_options(list(dsomop.query_strict = FALSE), {
    result <- .ohdsiGetResults(handle, "included_source_concept")
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true("concept_id" %in% names(result))
  })
})

test_that("ohdsiGetResults works for orphan_concept", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Orphan-concept rows (concept_count, no person column) fail-close in strict
  # mode; verify the consumer shapes the result with strict mode off.
  withr::with_options(list(dsomop.query_strict = FALSE), {
    result <- .ohdsiGetResults(handle, "orphan_concept")
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true("concept_id" %in% names(result))
  })
})

# --- Characterization New Table Queries ---

test_that("ohdsiGetResults works for c_covariates_continuous", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # This fixture's c_covariates_continuous carries count_value but no person
  # column, so strict mode fail-closes it; verify the consumer with it off.
  withr::with_options(list(dsomop.query_strict = FALSE), {
    result <- .ohdsiGetResults(handle, "c_covariates_continuous")
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true("median_value" %in% names(result))
    expect_true("p25_value" %in% names(result))
  })
})

test_that("ohdsiGetResults works for c_time_to_event", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "c_time_to_event")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("time_value" %in% names(result))
  expect_true("value" %in% names(result))
})

test_that("ohdsiGetResults works for c_dechallenge_rechallenge", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "c_dechallenge_rechallenge")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("num_cases" %in% names(result))
  expect_true("num_dechallenge_attempt" %in% names(result))
})

test_that("c_dechallenge_rechallenge count columns have disclosure control", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  cols <- .ohdsiDetectCountColumns(handle, "c_dechallenge_rechallenge",
                                    "characterization")
  expect_true("num_dechallenge_attempt" %in% cols)
  expect_true("num_dechallenge_success" %in% cols)
  expect_true("num_cases" %in% cols)
})

# --- CohortMethod Queries ---

test_that("ohdsiGetResults works for cm_result", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "cm_result")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("rr" %in% names(result))
  expect_true("ci_95_lb" %in% names(result))
  expect_true("ci_95_ub" %in% names(result))
})

test_that("ohdsiGetResults works for cm_diagnostics_summary", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "cm_diagnostics_summary")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("analysis_id" %in% names(result))
})

# --- SCCS Queries ---

test_that("ohdsiGetResults works for sccs_result", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "sccs_result")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("rr" %in% names(result))
  expect_true("ci_95_lb" %in% names(result))
})

test_that("ohdsiGetResults works for sccs_diagnostics_summary", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "sccs_diagnostics_summary")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

# --- PLP Queries ---

test_that("ohdsiGetResults works for plp_performances", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .ohdsiGetResults(handle, "plp_performances")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("auc" %in% names(result))
})

# --- Evidence Synthesis Queries ---

test_that("ohdsiGetResults works for es_cm_result", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Evidence-synthesis results are cross-database meta-analyses with no
  # per-person basis (n_databases only), so strict mode fail-closes them
  # (covered separately); verify the consumer with strict mode off.
  withr::with_options(list(dsomop.query_strict = FALSE), {
    result <- .ohdsiGetResults(handle, "es_cm_result")
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true("rr" %in% names(result))
    expect_true("n_databases" %in% names(result))
  })
})

test_that("ohdsiGetResults works for es_sccs_result", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Evidence-synthesis results are cross-database meta-analyses with no
  # per-person basis (n_databases only), so strict mode fail-closes them
  # (covered separately); verify the consumer with strict mode off.
  withr::with_options(list(dsomop.query_strict = FALSE), {
    result <- .ohdsiGetResults(handle, "es_sccs_result")
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true("rr" %in% names(result))
    expect_true("n_databases" %in% names(result))
  })
})
