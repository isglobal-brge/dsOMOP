# ==============================================================================
# Tests for Achilles query functions
# ==============================================================================

test_that("achillesStatus returns available=TRUE when tables exist", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .achillesStatus(handle)
  expect_true(result$available)
  expect_true("achilles_results" %in% result$tables)
  expect_true("achilles_results_dist" %in% result$tables)
  expect_true("achilles_heel_results" %in% result$tables)
  expect_true(result$n_analyses > 0)
  expect_true(result$n_heel_warnings > 0)
})

test_that("achillesStatus returns available=FALSE when tables missing", {
  # Create a handle with minimal tables (no Achilles)
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

  result <- .achillesStatus(handle)
  expect_false(result$available)
  expect_equal(length(result$tables), 0)
})

test_that("achillesListAnalyses returns analysis catalog", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  catalog <- .achillesListAnalyses(handle)
  expect_true(is.data.frame(catalog))
  expect_true(nrow(catalog) > 0)
  expect_true("analysis_id" %in% names(catalog))
  expect_true("analysis_name" %in% names(catalog))
  expect_true("domain" %in% names(catalog))
  expect_true(0L %in% catalog$analysis_id)
  expect_true(400L %in% catalog$analysis_id)
})

test_that("achillesListAnalyses filters by domain", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  cond <- .achillesListAnalyses(handle, "condition")
  expect_true(all(cond$domain == "condition"))
  expect_true(400L %in% cond$analysis_id)
  expect_false(0L %in% cond$analysis_id)

  person <- .achillesListAnalyses(handle, "person")
  expect_true(all(person$domain == "person"))
  expect_true(0L %in% person$analysis_id)
})

test_that("achillesGetResults retrieves correct analysis_ids", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 1), {
    result <- .achillesGetResults(handle, c(0L, 1L))
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true(all(result$analysis_id %in% c(0, 1)))

    # Analysis 0 = total persons = 15
    total <- result[result$analysis_id == 0, ]
    expect_equal(total$count_value[1], 15)

    # Analysis 1 = gender distribution
    gender <- result[result$analysis_id == 1, ]
    expect_true(nrow(gender) == 2)
    expect_equal(sum(gender$count_value), 15)
  })
})

test_that("achillesGetResults no longer accepts stratum_filters (Fix C)", {
  # Fix C: arbitrary stratum filtering removed to prevent probing attacks.
  # .achillesGetResults() now only accepts (handle, analysis_ids).
  expect_equal(length(formals(.achillesGetResults)), 2)
})

test_that("achillesGetResults suppresses counts below threshold", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 3), {
    result <- .achillesGetResults(handle, c(700L))
    # Albuterol has count_value = 2, which is < 3 threshold → row is dropped
    alb <- result[result$stratum_1 == "1154029", ]
    expect_equal(nrow(alb), 0)
  })
})

test_that("achillesGetDistributions retrieves dist stats", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 1), {
    result <- .achillesGetDistributions(handle, c(3L, 113L))
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true(all(c("min_value", "max_value", "avg_value", "median_value") %in%
                    names(result)))
    # Age dist (analysis 113) should have plausible values
    age <- result[result$analysis_id == 113, ]
    expect_true(nrow(age) > 0)
    expect_true(!is.na(age$avg_value[1]))
  })
})

test_that("achillesGetDistributions suppresses rows with small counts", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Insert a dist row with low count
  DBI::dbExecute(handle$conn, "INSERT INTO achilles_results_dist VALUES
    (999, 'test', NULL, NULL, NULL, NULL, 2, 10, 20, 15, 3, 15, 11, 12, 18, 19)")

  withr::with_options(list(nfilter.tab = 3), {
    result <- .achillesGetDistributions(handle, 999L)
    expect_true(nrow(result) > 0)
    expect_true(is.na(result$count_value[1]))
    expect_true(is.na(result$avg_value[1]))
    expect_true(is.na(result$median_value[1]))
  })
})

test_that("achillesGetHeelResults returns heel warnings", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .achillesGetHeelResults(handle)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) >= 3)
  expect_true("achilles_heel_warning" %in% names(result))
  expect_true("rule_id" %in% names(result))
})

test_that("achilles_catalog_static returns well-formed data.frame", {
  catalog <- .achilles_catalog_static()
  expect_true(is.data.frame(catalog))
  expect_true(nrow(catalog) > 20)
  expect_true(all(c("analysis_id", "analysis_name", "domain",
                     "stratum_1_name", "result_table") %in% names(catalog)))
  # No duplicate analysis IDs
  expect_equal(length(unique(catalog$analysis_id)), nrow(catalog))
})

test_that("blueprint discovers Achilles tables", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  bp <- handle$blueprint
  present <- bp$tables[bp$tables$present_in_db, ]
  expect_true("achilles_results" %in% present$table_name)
  expect_true("achilles_results_dist" %in% present$table_name)
  expect_true("achilles_heel_results" %in% present$table_name)
  expect_true("achilles_analysis" %in% present$table_name)
  expect_true(isTRUE(handle$has_achilles))
})

test_that("achillesAnalysisDomain classifies by ID range", {
  expect_equal(.achillesAnalysisDomain(1L), "person")
  expect_equal(.achillesAnalysisDomain(101L), "observation_period")
  expect_equal(.achillesAnalysisDomain(200L), "visit")
  expect_equal(.achillesAnalysisDomain(400L), "condition")
  expect_equal(.achillesAnalysisDomain(600L), "procedure")
  expect_equal(.achillesAnalysisDomain(700L), "drug")
  expect_equal(.achillesAnalysisDomain(800L), "observation")
  expect_equal(.achillesAnalysisDomain(1800L), "measurement")
  expect_equal(.achillesAnalysisDomain(2100L), "device")
})

test_that("achillesAnalysisResultTable classifies correctly", {
  expect_equal(.achillesAnalysisResultTable(0L), "achilles_results")
  expect_equal(.achillesAnalysisResultTable(1L), "achilles_results")
  expect_equal(.achillesAnalysisResultTable(3L), "achilles_results_dist")
  expect_equal(.achillesAnalysisResultTable(103L), "achilles_results_dist")
  expect_equal(.achillesAnalysisResultTable(404L), "achilles_results_dist")
  expect_equal(.achillesAnalysisResultTable(400L), "achilles_results")
})

test_that("achillesDiscoverIds returns analysis IDs from DB", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  ids <- .achillesDiscoverIds(handle)
  expect_true(length(ids) > 0)
  expect_true(0L %in% ids)
  expect_true(400L %in% ids)
  expect_true(113L %in% ids)  # from achilles_results_dist
})

test_that("achillesDiscoverCatalog returns catalog from achilles_analysis table", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  catalog <- .achillesDiscoverCatalog(handle)
  expect_true(is.data.frame(catalog))
  expect_true(nrow(catalog) > 0)
  expect_true("analysis_id" %in% names(catalog))
  expect_true("analysis_name" %in% names(catalog))
  expect_true("domain" %in% names(catalog))
  expect_true("result_table" %in% names(catalog))
  expect_true(0L %in% catalog$analysis_id)
  expect_true(400L %in% catalog$analysis_id)
})

test_that("achillesStatus reports has_analysis_table", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  status <- .achillesStatus(handle)
  expect_true(status$available)
  expect_true(status$has_analysis_table)
  expect_true("achilles_analysis" %in% status$tables)
})
