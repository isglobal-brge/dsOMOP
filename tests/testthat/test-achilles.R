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
  expect_true(1L %in% catalog$analysis_id)
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
  expect_true(1L %in% person$analysis_id)
})

test_that("achillesGetResults retrieves correct analysis_ids", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 1), {
    result <- .achillesGetResults(handle, c(1L, 2L))
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true(all(result$analysis_id %in% c(1, 2)))

    # Analysis 1 = total persons = 15
    total <- result[result$analysis_id == 1, ]
    expect_equal(total$count_value[1], 15)

    # Analysis 2 = gender distribution
    gender <- result[result$analysis_id == 2, ]
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

test_that("achillesGetDistributions retrieves dist stats without min/max (Fix B)", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 1), {
    result <- .achillesGetDistributions(handle, c(3L, 103L))
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    # Fix B: min_value and max_value must NOT be returned
    expect_false("min_value" %in% names(result))
    expect_false("max_value" %in% names(result))
    # Safe stats should be present
    expect_true(all(c("avg_value", "median_value") %in% names(result)))
    age <- result[result$analysis_id == 103, ]
    expect_true(nrow(age) > 0)
    expect_true(!is.na(age$avg_value[1]))
  })
})

test_that("achillesGetDistributions drops rows with small counts (Fix A)", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Insert a dist row with low count
  DBI::dbExecute(handle$conn, "INSERT INTO achilles_results_dist VALUES
    (999, 'test', NULL, NULL, NULL, NULL, 2, 10, 20, 15, 3, 15, 11, 12, 18, 19)")

  withr::with_options(list(nfilter.tab = 3), {
    result <- .achillesGetDistributions(handle, 999L)
    # Fix A: rows below threshold are DROPPED entirely (no NA skeletons)
    expect_equal(nrow(result), 0)
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

test_that("achillesGetHeelResults masks small counts and scrubs text (Gap G1)", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 3), {
    result <- .achillesGetHeelResults(handle)
    # Rows are data-quality rule indicators: kept, not dropped.
    expect_true(nrow(result) >= 3)
    # record_count below nfilter.tab (the fixture's 0) is NA-masked, larger kept.
    expect_true(any(is.na(result$record_count)))
    expect_true(15 %in% result$record_count)
    # Heel free-text has every numeric run scrubbed to "N".
    expect_false(any(grepl("[0-9]", result$achilles_heel_warning)))
  })
})

test_that("achillesGetDistributions masks avg/stdev for small samples (Gap G2)", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # count_value = 5: survives the nfilter.tab=3 row-drop, but is below
  # nfilter_dist=10 so ALL summary stats (incl. mean + SD) must be masked.
  DBI::dbExecute(handle$conn, "INSERT INTO achilles_results_dist VALUES
    (998, 'test', NULL, NULL, NULL, NULL, 5, 10, 20, 15, 3, 15, 11, 12, 18, 19)")

  withr::with_options(list(nfilter.tab = 3, dsomop.nfilter.dist = 10), {
    result <- .achillesGetDistributions(handle, 998L)
    expect_equal(nrow(result), 1)
    expect_equal(result$count_value[1], 5)        # count stays visible
    expect_true(is.na(result$avg_value[1]))       # mean masked
    expect_true(is.na(result$stdev_value[1]))     # SD masked
    expect_true(is.na(result$median_value[1]))    # percentiles masked
  })
})

# ==============================================================================
# Tests for Achilles person-gating (record-unit analyses)
# ==============================================================================

# Sibling-gate fixture: condition records-by-concept (401) gated by the
# precomputed person-by-concept sibling (400). Concept "888881" has 4 persons
# (>= 3 -> kept); concept "888882" has 2 persons (< 3 -> dropped). Fresh concept
# ids avoid colliding with create_test_handle()'s built-in 400 rows. Record
# counts are deliberately LARGE to prove gating keys on PERSONS, not records.
.seed_sibling_gate <- function(handle) {
  ins <- function(sql) DBI::dbExecute(handle$conn, sql)
  ins("INSERT INTO achilles_results VALUES (400, '888881', NULL, NULL, NULL, NULL, 4)")
  ins("INSERT INTO achilles_results VALUES (400, '888882', NULL, NULL, NULL, NULL, 2)")
  ins("INSERT INTO achilles_results VALUES (401, '888881', NULL, NULL, NULL, NULL, 50)")
  ins("INSERT INTO achilles_results VALUES (401, '888882', NULL, NULL, NULL, NULL, 40)")
}

test_that("person-gate keeps record cells whose sibling person count >= nfilter.tab", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_sibling_gate(handle)

  withr::with_options(list(nfilter.tab = 3), {
    result <- .achillesGetResults(handle, c(401L))
    rec <- result[result$analysis_id == 401, ]
    expect_true("888881" %in% rec$stratum_1)   # 4 sibling persons -> kept
    expect_false("888882" %in% rec$stratum_1)  # 2 sibling persons -> dropped
    expect_equal(rec$count_value[rec$stratum_1 == "888881"], 50)  # record count kept
  })
})

test_that("person-gate self-fetches the sibling and never returns it", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_sibling_gate(handle)

  withr::with_options(list(nfilter.tab = 3), {
    result <- .achillesGetResults(handle, c(401L))  # client asks only for 401
    expect_false(any(result$analysis_id == 400))    # sibling consumed internally
    expect_equal(sort(unique(result$analysis_id)), 401)
    expect_false("888882" %in% result$stratum_1)
  })
})

test_that("person-gate fail-closes record cells with no sibling person row", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  DBI::dbExecute(handle$conn,
    "INSERT INTO achilles_results VALUES (401, '999999', NULL, NULL, NULL, NULL, 99)")

  withr::with_options(list(nfilter.tab = 3), {
    result <- .achillesGetResults(handle, c(401L))
    expect_false("999999" %in% result$stratum_1)  # no sibling -> 0 persons -> drop
  })
})

test_that("person-gate leaves person-unit analyses untouched", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_sibling_gate(handle)

  withr::with_options(list(nfilter.tab = 3), {
    # 400 is person-unit: gated only by its own count_value (888882 count 2 ->
    # dropped by .suppressSmallCounts, NOT by person-gating; 888881 count 4 kept).
    result <- .achillesGetResults(handle, c(400L))
    s1 <- result$stratum_1[result$analysis_id == 400]
    expect_true("888881" %in% s1)
    expect_false("888882" %in% s1)
  })
})

test_that("person-gate drops companion (monthly) cells below the person threshold", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  DBI::dbExecute(handle$conn,
    "INSERT INTO achilles_results VALUES (420, '201906', NULL, NULL, NULL, NULL, 30)")
  DBI::dbExecute(handle$conn,
    "INSERT INTO achilles_results VALUES (420, '201907', NULL, NULL, NULL, NULL, 25)")

  testthat::local_mocked_bindings(
    .achillesCompanionPersonCounts = function(handle, analysis_id) {
      data.frame(stratum_1 = c("201906", "201907"), n_persons = c(4, 2),
                 stringsAsFactors = FALSE)
    },
    .package = "dsOMOP"
  )

  withr::with_options(list(nfilter.tab = 3), {
    result <- .achillesGetResults(handle, c(420L))
    rec <- result[result$analysis_id == 420, ]
    expect_true("201906" %in% rec$stratum_1)   # 4 persons -> kept
    expect_false("201907" %in% rec$stratum_1)  # 2 persons -> dropped
    expect_equal(rec$count_value[rec$stratum_1 == "201906"], 30)
  })
})

test_that("companion person-counts are cached per analysis on the handle", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  DBI::dbExecute(handle$conn,
    "INSERT INTO achilles_results VALUES (420, '201906', NULL, NULL, NULL, NULL, 30)")

  calls <- 0L
  testthat::local_mocked_bindings(
    .achillesCompanionPersonCounts = function(handle, analysis_id) {
      calls <<- calls + 1L
      data.frame(stratum_1 = "201906", n_persons = 4, stringsAsFactors = FALSE)
    },
    .package = "dsOMOP"
  )

  withr::with_options(list(nfilter.tab = 3), {
    .achillesGetResults(handle, c(420L))
    expect_equal(calls, 1L)
    expect_true(!is.null(handle$achilles_gate_cache[["420"]]))
  })
})

test_that("companion person-counts query the CDM with distinct persons (real SQL)", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  # 505 = death records by death_type_concept_id; companion counts distinct
  # persons from the death table grouped by death_type_concept_id.
  counts <- .achillesCompanionPersonCounts(handle, 505L)
  expect_true(is.data.frame(counts))
  expect_true(all(c("stratum_1", "n_persons") %in% names(counts)))
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
  expect_true(1L %in% ids)
  expect_true(400L %in% ids)
  expect_true(103L %in% ids)  # from achilles_results_dist
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
  expect_true(1L %in% catalog$analysis_id)
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

# ==============================================================================
# Tests for results_schema resolution / auto-detection
# ==============================================================================

.rs_handle <- function(cdm_schema = "cdm", results_schema = NULL,
                       dialect = "postgresql") {
  h <- new.env(parent = emptyenv())
  h$dbms <- if (dialect == "sqlite") "sqlite" else "postgresql"
  h$target_dialect <- dialect
  h$cdm_schema <- cdm_schema
  h$results_schema <- results_schema
  h$results_schema_resolved <- NULL
  h$results_schema_resolved_done <- FALSE
  h
}

test_that("resolveResultsSchema honors an explicit pin without probing", {
  h <- .rs_handle(results_schema = "my_results")
  called <- 0L
  testthat::local_mocked_bindings(
    .listTablesRaw = function(handle, schema) { called <<- called + 1L; character(0) },
    .package = "dsOMOP")
  expect_equal(.resolveResultsSchema(h), "my_results")
  expect_equal(called, 0L)  # explicit pin => no probing
})

test_that("resolveResultsSchema auto-detects a dedicated results schema", {
  h <- .rs_handle()
  testthat::local_mocked_bindings(
    .listTablesRaw = function(handle, schema) {
      if (schema == "results") c("achilles_results", "achilles_analysis") else character(0)
    }, .package = "dsOMOP")
  expect_equal(.resolveResultsSchema(h), "results")
})

test_that("resolveResultsSchema finds achilles co-located in the cdm schema", {
  h <- .rs_handle()
  testthat::local_mocked_bindings(
    .listTablesRaw = function(handle, schema) {
      if (schema == "cdm") c("person", "achilles_results") else character(0)
    }, .package = "dsOMOP")
  expect_equal(.resolveResultsSchema(h), "cdm")
})

test_that("resolveResultsSchema returns NULL when achilles absent everywhere", {
  h <- .rs_handle()
  testthat::local_mocked_bindings(
    .listTablesRaw = function(handle, schema) character(0), .package = "dsOMOP")
  expect_null(.resolveResultsSchema(h))
})

test_that("resolveResultsSchema skips detection on sqlite (no schemas)", {
  h <- .rs_handle(dialect = "sqlite")
  testthat::local_mocked_bindings(
    .listTablesRaw = function(handle, schema) stop("should not probe on sqlite"),
    .package = "dsOMOP")
  expect_null(.resolveResultsSchema(h))
})

test_that("resolveResultsSchema caches (probes at most once)", {
  h <- .rs_handle()
  calls <- 0L
  testthat::local_mocked_bindings(
    .listTablesRaw = function(handle, schema) {
      calls <<- calls + 1L
      if (schema == "results") "achilles_results" else character(0)
    }, .package = "dsOMOP")
  .resolveResultsSchema(h); .resolveResultsSchema(h)
  expect_equal(.resolveResultsSchema(h), "results")
  expect_equal(calls, 1L)  # cached after first resolve
})
