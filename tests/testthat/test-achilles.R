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
  expect_true(is.na(result$n_heel_warnings))
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
    expect_true(all(gender$count_value %% 5 == 0))
    expect_lte(sum(gender$count_value), 15)
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
    result <- .achillesGetDistributions(handle, 103L)
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

test_that("unknown Achilles distributions fail closed", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Insert a dist row with low count
  DBI::dbExecute(handle$conn, "INSERT INTO achilles_results_dist VALUES
    (999, 'test', NULL, NULL, NULL, NULL, 2, 10, 20, 15, 3, 15, 11, 12, 18, 19)")

  withr::with_options(list(nfilter.tab = 3), {
    expect_error(.achillesGetDistributions(handle, 999L),
                 "one-person contribution contract")
  })
})

test_that("achillesGetHeelResults returns heel warnings", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .achillesGetHeelResults(handle)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) >= 1)
  expect_true("achilles_heel_warning" %in% names(result))
  expect_true("rule_id" %in% names(result))
})

test_that("achillesGetHeelResults omits small counts and scrubs text", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 3), {
    result <- .achillesGetHeelResults(handle)
    expect_true(nrow(result) >= 1)
    expect_false(any(is.na(result$record_count)))
    expect_true(all(result$record_count >= 3))
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
    (103, 'small_test', NULL, NULL, NULL, NULL, 5, 10, 20, 15, 3, 15, 11, 12, 18, 19)")

  withr::with_options(list(nfilter.tab = 3, dsomop.nfilter.dist = 10), {
    result <- .achillesGetDistributions(handle, 103L)
    small <- result[!is.na(result$stratum_1) &
                      result$stratum_1 == "small_test", , drop = FALSE]
    expect_equal(nrow(small), 1)
    expect_equal(small$count_value[1], 5)
    expect_true(is.na(small$avg_value[1]))
    expect_true(is.na(small$stdev_value[1]))
    expect_true(is.na(small$median_value[1]))
  })
})

test_that("Achilles contracts block exact ages and record-value distributions", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(.achillesGetResults(handle, 3L), "exact age/birth-year")
  expect_error(.achillesGetResults(handle, 999L), "reviewed release contract")
  expect_error(.achillesGetDistributions(handle, 715L),
               "one-person contribution contract")
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

test_that("companion person-counts are recomputed on every read (B07)", {
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
    # A release decision never rests on support remembered from an earlier
    # read: the next read recomputes the basis, and nothing is cached.
    .achillesGetResults(handle, c(420L))
    expect_equal(calls, 2L)
    expect_null(handle$achilles_gate_cache)
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
  withr::with_options(list(dsomop.allowed_results_schemas = "results"), {
    expect_equal(.resolveResultsSchema(h), "results")
  })
})

test_that("allowlisted results namespaces use each DBMS grammar", {
  for (case in list(
    list(dbms = "mariadb", dialect = "mysql", schema = "results-prod"),
    list(dbms = "bigquery", dialect = "bigquery",
         schema = "project-id.results_dataset")
  )) {
    handle <- .rs_handle(cdm_schema = "cdm", dialect = case$dialect)
    handle$dbms <- case$dbms
    testthat::local_mocked_bindings(
      .listTablesRaw = function(handle, schema) {
        if (identical(schema, case$schema)) "achilles_results" else character(0)
      },
      .package = "dsOMOP"
    )
    withr::with_options(
      list(dsomop.allowed_results_schemas = case$schema),
      expect_identical(.resolveResultsSchema(handle), case$schema)
    )
  }
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
  withr::with_options(list(dsomop.allowed_results_schemas = "results"), {
    .resolveResultsSchema(h); .resolveResultsSchema(h)
    expect_equal(.resolveResultsSchema(h), "results")
  })
  expect_equal(calls, 2L)  # CDM + one allowlisted schema, only on first resolve
})

# --- G-03: the reader's companion person basis follows observation periods ---
#
# Upstream Achilles counts a record only when its event date lies inside one
# of the person's observation periods. A companion person count over all
# records let out-of-period people lend support to an in-period cell (the
# OHDSI-tool audit, checkA: 11 in-period records from 2 people released as 10).

.g03_insert_condition <- function(handle, id, person, date) {
  DBI::dbExecute(handle$conn, sprintf(paste0(
    "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
    "condition_concept_id, condition_start_date, condition_end_date, ",
    "condition_type_concept_id) VALUES (%d, %d, 201820, '%s', '%s', ",
    "44818518)"), as.integer(id), as.integer(person), date, date))
}

.g03_handle <- function() {
  handle <- create_test_handle()
  .buildBlueprint(handle)
  # Persons 13 and 14 are observed only from 2021: their June 2020 records
  # are outside every observation period.
  DBI::dbExecute(handle$conn, paste0(
    "UPDATE observation_period SET observation_period_start_date = ",
    "'2021-01-01' WHERE person_id IN (13, 14)"))
  for (k in 1:10) {
    .g03_insert_condition(handle, 1000 + k, 2, sprintf("2020-06-%02d", k))
  }
  .g03_insert_condition(handle, 2001, 13, "2020-06-15")
  .g03_insert_condition(handle, 2002, 14, "2020-06-20")
  handle
}

.g03_upstream_420 <- function(handle, month = "202006") {
  DBI::dbGetQuery(handle$conn, paste0(
    "SELECT COUNT(co.person_id) AS records, ",
    "COUNT(DISTINCT co.person_id) AS persons ",
    "FROM condition_occurrence co JOIN observation_period op ",
    "ON co.person_id = op.person_id ",
    "AND co.condition_start_date >= op.observation_period_start_date ",
    "AND co.condition_start_date <= op.observation_period_end_date ",
    "WHERE strftime('%Y%m', co.condition_start_date) = '", month, "'"))
}

test_that("G-03: an in-period 420 cell from 2 people stays withheld", {
  handle <- .g03_handle()
  on.exit(cleanup_handle(handle))
  upstream <- .g03_upstream_420(handle)
  expect_equal(upstream$persons, 2)
  DBI::dbExecute(handle$conn, sprintf(paste0(
    "INSERT INTO achilles_results VALUES (420, '202006', NULL, NULL, NULL, ",
    "NULL, %d)"), as.integer(upstream$records)))

  companion <- .achillesCompanionPersonCounts(handle, 420L)
  expect_equal(companion$n_persons[companion$stratum_1 == "202006"], 2)

  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3), {
    result <- .achillesGetResults(handle, 420L)
    expect_false("202006" %in% result$stratum_1)
  })
  # Support exactly at the threshold is released (banded).
  withr::with_options(list(nfilter.tab = 2, nfilter.subset = 2,
                           dsomop.nfilter.band = 5), {
    result <- .achillesGetResults(handle, 420L)
    expect_equal(result$count_value[result$stratum_1 == "202006"],
                 floor(upstream$records / 5) * 5)
  })
})

test_that("G-03: overlapping or duplicate periods never inflate support", {
  handle <- .g03_handle()
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO observation_period VALUES ",
    "(9001, 2, '2020-01-01', '2024-12-31', 44818518), ",
    "(9002, 2, '2020-05-01', '2020-07-31', 44818518), ",
    "(9003, 2, '2020-06-01', '2020-06-30', 44818518)"))
  companion <- .achillesCompanionPersonCounts(handle, 420L)
  expect_equal(companion$n_persons[companion$stratum_1 == "202006"], 2)

  # A period that genuinely covers person 13's record adds that person.
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO observation_period VALUES ",
    "(9004, 13, '2020-06-01', '2020-06-30', 44818518)"))
  companion <- .achillesCompanionPersonCounts(handle, 420L)
  expect_equal(companion$n_persons[companion$stratum_1 == "202006"], 3)
})

test_that("G-03: record support edges 0/1/2/threshold", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  people <- list("2022-01-10" = 1L, "2022-02-10" = 1:2, "2022-03-10" = 1:3)
  id <- 5000L
  for (date in names(people)) {
    for (p in people[[date]]) {
      id <- id + 1L
      .g03_insert_condition(handle, id, p, date)
    }
  }
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO achilles_results VALUES ",
    "(420, '202201', NULL, NULL, NULL, NULL, 10), ",
    "(420, '202202', NULL, NULL, NULL, NULL, 10), ",
    "(420, '202203', NULL, NULL, NULL, NULL, 10), ",
    "(420, '202204', NULL, NULL, NULL, NULL, 10)"))
  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3), {
    result <- .achillesGetResults(handle, 420L)
    months <- result$stratum_1[result$analysis_id == 420]
    expect_true("202203" %in% months)
    expect_false(any(c("202201", "202202", "202204") %in% months))
  })
})

test_that("G-03: a missing observation_period withholds every record cell", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  for (p in 1:5) .g03_insert_condition(handle, 6000 + p, p, "2022-05-10")
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO achilles_results VALUES ",
    "(420, '202205', NULL, NULL, NULL, NULL, 50)"))
  DBI::dbExecute(handle$conn, "DROP TABLE observation_period")
  .buildBlueprint(handle, force = TRUE)

  companion <- .achillesCompanionPersonCounts(handle, 420L)
  expect_equal(nrow(companion), 0L)
  expect_true(all(c("stratum_1", "n_persons") %in% names(companion)))
  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3), {
    result <- .achillesGetResults(handle, 420L)
    expect_false("202205" %in% result$stratum_1)
  })
})

test_that("G-03: every contracted stratum column is required", {
  expect_true(all(is.na(.achillesStratumKey(
    data.frame(stratum_1 = "1", stratum_2 = "2"),
    c("stratum_1", "stratum_2", "stratum_3")
  ))))
  expect_true(all(is.na(.achillesStratumKey(
    data.frame(stratum_1 = c("1", "2")), character(0)
  ))))
  expect_identical(
    .achillesStratumKey(data.frame(stratum_1 = c("1", NA),
                                   stratum_2 = c("23", "4")),
                        c("stratum_1", "stratum_2")),
    c("1\x1f23", "\x1f4")
  )

  # A companion that lost its range-bucket column must not let the gate match
  # 1818 cells on concept and unit alone.
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO achilles_results VALUES ",
    "(1818, '3004410', '8554', 'Within Range', NULL, NULL, 40)"))
  testthat::local_mocked_bindings(
    .achillesCompanionPersonCounts = function(handle, analysis_id) {
      data.frame(stratum_1 = "3004410", stratum_2 = "8554", n_persons = 40,
                 stringsAsFactors = FALSE)
    },
    .package = "dsOMOP"
  )
  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3), {
    result <- .achillesGetResults(handle, 1818L)
    expect_equal(sum(result$analysis_id == 1818), 0L)
  })
})

test_that("G-03: 1818 buckets, range edges, NULL unit and NULL ranges", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  # A concept of its own so the fixture's HbA1c rows cannot interfere.
  values <- c(3.9, 3.9, 3.9, 4.0, 4.0, 4.0, 5.6, 5.6, 5.6, 5.7, 5.7, 5.7,
              5.0, 5.0, NA)
  unit <- c(rep(8554L, 12), NA, 8554L, 8554L)
  low <- c(rep(4.0, 13), NA, 4.0)
  for (p in 1:15) {
    DBI::dbExecute(handle$conn, sprintf(paste0(
      "INSERT INTO measurement (measurement_id, person_id, ",
      "measurement_concept_id, measurement_date, measurement_type_concept_id, ",
      "value_as_number, unit_concept_id, range_low, range_high) VALUES ",
      "(%d, %d, 3999999, '2021-05-05', 44818518, %s, %s, %s, 5.6)"),
      900000L + p, p,
      if (is.na(values[p])) "NULL" else format(values[p]),
      if (is.na(unit[p])) "NULL" else unit[p],
      if (is.na(low[p])) "NULL" else format(low[p])))
  }
  # Person 13's out-of-period record (before 2020) adds no support.
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO measurement (measurement_id, person_id, ",
    "measurement_concept_id, measurement_date, measurement_type_concept_id, ",
    "value_as_number, unit_concept_id, range_low, range_high) VALUES ",
    "(900100, 13, 3999999, '2019-05-05', 44818518, 3.0, 8554, 4.0, 5.6)"))

  companion <- .achillesCompanionPersonCounts(handle, 1818L)
  companion <- companion[companion$stratum_1 == "3999999", , drop = FALSE]
  basis <- stats::setNames(companion$n_persons, companion$stratum_3)
  expect_setequal(names(basis),
                  c("Below Range Low", "Within Range", "Above Range High"))
  expect_equal(basis[["Below Range Low"]], 3)
  expect_equal(basis[["Within Range"]], 6)
  expect_equal(basis[["Above Range High"]], 3)
  expect_true(all(companion$stratum_2 == "8554"))

  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO achilles_results VALUES ",
    "(1818, '3999999', '8554', 'Below Range Low', NULL, NULL, 3), ",
    "(1818, '3999999', '8554', 'Within Range', NULL, NULL, 6), ",
    "(1818, '3999999', '8554', 'Above Range High', NULL, NULL, 3), ",
    "(1818, '3999999', NULL, 'Within Range', NULL, NULL, 4)"))
  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3,
                           dsomop.nfilter.band = 1), {
    result <- .achillesGetResults(handle, 1818L)
    result <- result[result$stratum_1 == "3999999", , drop = FALSE]
    expect_equal(nrow(result), 3L)
    expect_false(any(is.na(result$stratum_2)))
    expect_equal(stats::setNames(result$count_value, result$stratum_3)[
      c("Below Range Low", "Within Range", "Above Range High")],
      c("Below Range Low" = 3, "Within Range" = 6, "Above Range High" = 3))
  })
})

test_that("G-03: 505 counts in-period deaths and keeps the NULL type group", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  # Fixture deaths: persons 1..12, type 32817, inside their periods. Person
  # 13 dies before their period; persons 14 and 15 have no death type.
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO death (person_id, death_date, death_type_concept_id, ",
    "cause_concept_id) VALUES (13, '2019-01-01', 32817, 4000002), ",
    "(14, '2022-01-01', NULL, 4000002), (15, '2022-02-01', NULL, 4000002)"))
  companion <- .achillesCompanionPersonCounts(handle, 505L)
  typed <- companion$n_persons[!is.na(companion$stratum_1) &
                                 companion$stratum_1 == "32817"]
  expect_equal(typed, 12)
  expect_equal(companion$n_persons[is.na(companion$stratum_1)], 2)
})

# --- B07 (2.10.3 review): support is never remembered across reads -------------

.b07_handle <- function(persons) {
  h <- create_test_handle(n_persons = 15)
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  DBI::dbExecute(h$conn, "DELETE FROM achilles_results WHERE analysis_id = 420")
  # Five in-period June-2020 records for each listed person.
  for (p in persons) {
    for (k in 1:5) .release_add_condition(h, p, 201820L, "2020-06-01")
  }
  DBI::dbExecute(h$conn, sprintf(paste(
    "INSERT INTO achilles_results VALUES (420, '202006', NULL, NULL, NULL,",
    "NULL, %d)"), 5L * length(persons)))
  .buildBlueprint(h, force = TRUE)
  h
}

.b07_copy <- function(h) {
  h2 <- new.env(parent = emptyenv())
  for (name in ls(h, all.names = TRUE)) {
    assign(name, get(name, envir = h, inherits = FALSE), envir = h2)
  }
  h2
}

test_that("B07: a refresh that drops a cell below the floor withholds it at once", {
  withr::local_options(list(nfilter.tab = 3, nfilter.subset = 3,
                            dsomop.nfilter.band = 5))
  h <- .b07_handle(1:3)
  on.exit(cleanup_handle(h))
  before <- .achillesGetResults(h, 420L)
  expect_equal(before$count_value[before$stratum_1 == "202006"], 15)
  # The review's probe: person 3's records go and the stored count follows,
  # with a forced blueprint and catalogue refresh.
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence WHERE person_id = 3")
  DBI::dbExecute(h$conn,
                 "UPDATE achilles_results SET count_value = 10 WHERE analysis_id = 420")
  .buildBlueprint(h, force = TRUE)
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  current <- .achillesCompanionPersonCounts(h, 420L)
  expect_equal(current$n_persons[current$stratum_1 == "202006"], 2)
  # The handle that read the 3-person cell withholds the 2-person cell...
  expect_equal(nrow(.achillesGetResults(h, 420L)), 0L)
  # ...and so does a new handle on the same database.
  expect_equal(nrow(.achillesGetResults(.b07_copy(h), 420L)), 0L)
  expect_null(h$achilles_gate_cache)
})

test_that("B07: no refresh is needed: decreases and increases apply on the next read", {
  withr::local_options(list(nfilter.tab = 3, nfilter.subset = 3,
                            dsomop.nfilter.band = 5))
  h <- .b07_handle(1:2)
  on.exit(cleanup_handle(h))
  # Two people: withheld on this handle and on a copy.
  expect_equal(nrow(.achillesGetResults(h, 420L)), 0L)
  h2 <- .b07_copy(h)
  expect_equal(nrow(.achillesGetResults(h2, 420L)), 0L)
  # A third person crosses the floor: released on the next read of either.
  for (k in 1:5) .release_add_condition(h, 3L, 201820L, "2020-06-01")
  DBI::dbExecute(h$conn,
                 "UPDATE achilles_results SET count_value = 15 WHERE analysis_id = 420")
  expect_equal(.achillesGetResults(h, 420L)$count_value, 15)
  expect_equal(.achillesGetResults(h2, 420L)$count_value, 15)
  # Back below the floor (no blueprint refresh): withheld again on both.
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence WHERE person_id = 1")
  DBI::dbExecute(h$conn,
                 "UPDATE achilles_results SET count_value = 10 WHERE analysis_id = 420")
  expect_equal(nrow(.achillesGetResults(h, 420L)), 0L)
  expect_equal(nrow(.achillesGetResults(h2, 420L)), 0L)
})

test_that("G-03: 2102 is persons by device and month, served by the reader", {
  expect_false(2102L %in% .achillesRecordGatedIds())
  catalog <- .achilles_catalog_static()
  row <- catalog[catalog$analysis_id == 2102L, , drop = FALSE]
  expect_identical(row$unit, "person")
  expect_identical(row$stratum_1_name, "device_concept_id")
  expect_identical(row$stratum_2_name, "calendar month")
  expect_identical(.achillesCountContract(2102L), 2102L)

  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(handle$conn, paste0(
    "CREATE TABLE device_exposure (device_exposure_id INTEGER, ",
    "person_id INTEGER, device_concept_id INTEGER, ",
    "device_exposure_start_date TEXT, device_exposure_end_date TEXT, ",
    "device_type_concept_id INTEGER)"))
  # One month shared by two devices with overlapping people.
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO achilles_results VALUES ",
    "(2102, '4000001', '202006', NULL, NULL, NULL, 8), ",
    "(2102, '4000002', '202006', NULL, NULL, NULL, 6), ",
    "(2102, '4000001', '202007', NULL, NULL, NULL, 2)"))
  .buildBlueprint(handle, force = TRUE)
  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3,
                           dsomop.nfilter.band = 5), {
    result <- .achillesGetResults(handle, 2102L)
    key <- paste(result$stratum_1, result$stratum_2)
    expect_setequal(key, c("4000001 202006", "4000002 202006"))
    expect_equal(result$count_value[key == "4000001 202006"], 5)
    expect_equal(result$count_value[key == "4000002 202006"], 5)
  })
})
