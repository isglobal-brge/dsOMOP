# Every live Achilles catalogue entry executes through the real run path on
# the embedded engines. DuckDB enforces the strict GROUP BY rule of PostgreSQL,
# SQL Server, Oracle and BigQuery, so it exercises G-16 without a server; the
# PostgreSQL and MySQL-family runs are in test-vendor-db-integration.R.

.achillesLiveEngineHandle <- function(conn, dbms) {
  frames <- .achillesLiveFrames()
  for (statement in .achillesLiveStatements(frames, dbms)) {
    DBI::dbExecute(conn, statement)
  }
  handle <- new.env(parent = emptyenv())
  handle$conn <- conn
  handle$dbms <- dbms
  handle$target_dialect <- dbms
  # DuckDB lists tables of a named schema; SQLite has no schemas.
  handle$cdm_schema <- if (identical(dbms, "duckdb")) "main" else NULL
  handle$vocab_schema <- NULL
  handle$results_schema <- NULL
  handle$temp_schema <- NULL
  handle$resource_client <- NULL
  handle$config <- list()
  handle$blueprint <- NULL
  handle$temp_tables <- character(0)
  handle$temp_connection <- NULL
  handle$person_key <- .testPseudonymKey("achilles-live-engines")
  .buildBlueprint(handle)
  handle
}

test_that("SQLite executes every live Achilles entry and the G-03 reader", {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  handle <- .achillesLiveEngineHandle(conn, "sqlite")
  rows <- .expectAchillesLiveCatalogue(handle, "sqlite")
  expect_true(all(rows > 0L))
  .expectAchillesLiveReader(handle, "sqlite")

  # The live 2102 entry is the upstream person analysis by device and start
  # month (not the monthly device record count, upstream 2120).
  entry <- .omopAnalysisResolve(handle, "dsomop:achilles.2102")
  expect_identical(entry$disclosure$unit, "person")
  withr::local_options(list(nfilter.subset = 1, nfilter.tab = 1,
                            dsomop.nfilter.band = 1))
  live <- .omopAnalysisRun(handle, "dsomop:achilles.2102")
  expect_true(all(c("calendar_month", "concept_id", "concept_name",
                    "count_value") %in% names(live)))
  expect_false("n_persons" %in% names(live))
  key <- paste(live$concept_id, live$calendar_month)
  expect_equal(
    stats::setNames(as.numeric(live$count_value), key)[
      c("4206863 202006", "4044008 202006", "4206863 202007")],
    c("4206863 202006" = 10, "4044008 202006" = 8, "4206863 202007" = 6)
  )
})

test_that("DuckDB executes every live Achilles entry (strict GROUP BY)", {
  skip_if_not_installed("duckdb")
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  message("[vendor-versions] duckdb ", utils::packageVersion("duckdb"),
          " engine ", DBI::dbGetQuery(conn, "SELECT version() AS v")$v[[1L]],
          "; DBI ", utils::packageVersion("DBI"))
  handle <- .achillesLiveEngineHandle(conn, "duckdb")

  # The unpatched shape (a concept name selected but not grouped) is rejected
  # by this engine, so a passing run below is a real G-16 check.
  expect_error(DBI::dbGetQuery(conn, paste0(
    "SELECT p.gender_concept_id, cj1.concept_name AS gender_name, ",
    "COUNT(DISTINCT p.person_id) AS count_value FROM person p ",
    "LEFT JOIN concept cj1 ON cj1.concept_id = p.gender_concept_id ",
    "GROUP BY p.gender_concept_id"
  )))

  rows <- .expectAchillesLiveCatalogue(handle, "duckdb")
  expect_true(all(rows > 0L))
})
