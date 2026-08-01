test_that("database support profiles distinguish tested and contract-only adapters", {
  declared <- c(
    "postgresql", "sqlite", "duckdb", "mysql", "mariadb",
    "sql_server", "synapse", "pdw", "oracle", "redshift", "bigquery",
    "snowflake", "spark", "databricks"
  )
  profiles <- lapply(declared, .databaseSupportProfile)

  expect_true(all(vapply(profiles, function(x) {
    is.character(x$target_dialect) && length(x$target_dialect) == 1L &&
      identical(x$live_vendor_ci, FALSE)
  }, logical(1))))
  expect_equal(.databaseSupportProfile("sqlite")$verification,
               "embedded_integration_tests")
  expect_equal(.databaseSupportProfile("duckdb")$verification,
               "optional_embedded_integration_tests")
  expect_equal(.databaseSupportProfile("postgresql")$verification,
               "sql_contract_tests_only")
  expect_true(all(vapply(profiles, function(x) {
    identical(x$support_tier, x$verification) &&
      identical(x$sql_translation, "builtin_top_dateadd_subset") &&
      identical(x$sql_translation_patterns,
                c("select_top_integer", "dateadd_day_integer")) &&
      identical(x$sqlrender_runtime, FALSE)
  }, logical(1))))

  network_backends <- setdiff(declared, c("sqlite", "duckdb"))
  expect_true(all(vapply(network_backends, function(dbms) {
    identical(.databaseSupportProfile(dbms)$support_tier,
              "sql_contract_tests_only")
  }, logical(1))))
})

test_that("temporary-object capabilities match SQL actually generated", {
  h <- new.env(parent = emptyenv())

  for (dbms in c("postgresql", "sqlite", "duckdb", "redshift", "snowflake")) {
    h$dbms <- dbms
    expect_match(.tempCreateSql(h, "tmp_x", "SELECT 1"),
                 "^CREATE TEMP TABLE tmp_x AS")
    expect_equal(.tempDropSql(h, "tmp_x"), "DROP TABLE IF EXISTS tmp_x")
  }

  for (dbms in c("mysql", "mariadb")) {
    h$dbms <- dbms
    expect_equal(.tempCreateSql(h, "tmp_x", "SELECT 1"),
                 "CREATE TEMPORARY TABLE tmp_x AS SELECT 1")
    expect_equal(.tempDropSql(h, "tmp_x"),
                 "DROP TEMPORARY TABLE IF EXISTS tmp_x")
  }

  for (dbms in c("spark", "databricks")) {
    h$dbms <- dbms
    expect_equal(.tempCreateSql(h, "tmp_x", "SELECT 1"),
                 "CREATE TEMPORARY VIEW tmp_x AS SELECT 1")
    expect_equal(.tempDropSql(h, "tmp_x"), "DROP VIEW IF EXISTS tmp_x")
  }

  for (dbms in c("sqlserver", "synapse", "pdw", "oracle", "bigquery")) {
    h$dbms <- dbms
    expect_error(.tempCreateSql(h, "tmp_x", "SELECT 1"),
                 "not implemented safely")
  }
})

test_that("SQLite temporary materialization remains executable", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  name <- .createTempTable(handle, "portability_tmp", "SELECT 1 AS value")
  expect_equal(name, "portability_tmp")
  expect_equal(DBI::dbGetQuery(handle$conn,
    "SELECT value FROM portability_tmp")$value, 1L)
  .dropTempTable(handle, name)
  expect_false(name %in% DBI::dbListTables(handle$conn))

  DBI::dbExecute(handle$conn,
    "CREATE TABLE portability_events (person_id INTEGER, event_date TEXT)")
  DBI::dbExecute(handle$conn,
    paste0("INSERT INTO portability_events VALUES ",
           "(1, '2020-01-01'), (2, '2020-01-02'), (3, '2020-01-03')"))
  sql <- .sql_translate(
    paste0("SELECT TOP 2 person_id, DATEADD(day, 7, event_date) AS shifted ",
           "FROM portability_events ORDER BY person_id"),
    "sqlite"
  )
  result <- DBI::dbGetQuery(handle$conn, sql)
  expect_equal(result$person_id, 1:2)
  expect_equal(result$shifted, c("2020-01-08", "2020-01-09"))
})

.create_empty_query_catalog_schema <- function(conn, dialect) {
  spec <- .loadCdmSpec("5.4")$field_level
  for (table in unique(spec$cdmTableName)) {
    fields <- spec[spec$cdmTableName == table, , drop = FALSE]
    types <- if (identical(dialect, "sqlite")) {
      ifelse(grepl("integer", fields$cdmDatatype, ignore.case = TRUE),
             "INTEGER",
        ifelse(grepl("float|numeric|decimal", fields$cdmDatatype,
                     ignore.case = TRUE), "REAL", "TEXT"))
    } else {
      ifelse(grepl("integer", fields$cdmDatatype, ignore.case = TRUE),
             "BIGINT",
        ifelse(grepl("float|numeric|decimal", fields$cdmDatatype,
                     ignore.case = TRUE), "DOUBLE",
          ifelse(grepl("datetime", fields$cdmDatatype, ignore.case = TRUE),
                 "TIMESTAMP",
            ifelse(grepl("^date", fields$cdmDatatype, ignore.case = TRUE),
                   "DATE", "VARCHAR"))))
    }
    definitions <- paste(
      DBI::dbQuoteIdentifier(conn, fields$cdmFieldName), types
    )
    DBI::dbExecute(conn, paste(
      "CREATE TABLE", DBI::dbQuoteIdentifier(conn, table),
      "(", paste(definitions, collapse = ", "), ")"
    ))
  }
}

.query_catalog_handle <- function(conn, dialect) {
  handle <- new.env(parent = emptyenv())
  handle$conn <- conn
  handle$dbms <- dialect
  handle$target_dialect <- dialect
  handle$cdm_schema <- NULL
  handle$vocab_schema <- NULL
  handle$results_schema <- NULL
  handle$temp_schema <- NULL
  handle$resource_client <- NULL
  handle$config <- list()
  handle$blueprint <- NULL
  handle$temp_tables <- character(0)
  handle$temp_connection <- NULL
  handle$analysis_catalog <- NULL
  handle
}

.expect_announced_queries_execute <- function(handle) {
  entries <- .omopAnalysisQueryEntries(handle)
  expect_gt(length(entries), 0L)
  for (name in names(entries)) {
    entry <- entries[[name]]
    expect_true(handle$target_dialect %in%
                  entry$meta$supported_dialects, info = name)
    expect_error({
      params <- .omopAnalysisSanitizeParams(entry, list())
      .omopAnalysisRunSql(handle, entry, params, scoped = NULL)
    }, NA, info = name)
  }
  entries
}

test_that("every QueryLibrary entry announced for SQLite is executable", {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  .create_empty_query_catalog_schema(conn, "sqlite")
  entries <- .expect_announced_queries_execute(
    .query_catalog_handle(conn, "sqlite")
  )

  expect_false("dsomop:condition.prevalence_by_year" %in% names(entries))
  expect_false("dsomop:condition.duration_stats" %in% names(entries))
  expect_true("dsomop:condition.prevalence_by_concept" %in% names(entries))
})

test_that("every QueryLibrary entry announced for DuckDB is executable", {
  skip_if_not_installed("duckdb")
  driver <- duckdb::duckdb()
  conn <- DBI::dbConnect(driver)
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  .create_empty_query_catalog_schema(conn, "duckdb")
  entries <- .expect_announced_queries_execute(
    .query_catalog_handle(conn, "duckdb")
  )

  expect_true("dsomop:condition.prevalence_by_year" %in% names(entries))
  expect_true("dsomop:drug_era.length_stats" %in% names(entries))
})

test_that("contract-only query catalogs omit unsupported SQL constructs", {
  for (dialect in c("sql server", "oracle", "bigquery", "spark", "mysql")) {
    handle <- new.env(parent = emptyenv())
    handle$target_dialect <- dialect
    entries <- .omopAnalysisQueryEntries(handle)
    expect_true(all(vapply(entries, function(entry) {
      dialect %in% entry$meta$supported_dialects
    }, logical(1))), info = dialect)
  }
  sql_server <- new.env(parent = emptyenv())
  sql_server$target_dialect <- "sql server"
  sql_server_entries <- .omopAnalysisQueryEntries(sql_server)
  expect_false(any(vapply(sql_server_entries, function(entry) {
    grepl("\\b(LIMIT|EXTRACT|STDDEV)\\b", entry$compute$sql,
          ignore.case = TRUE, perl = TRUE)
  }, logical(1))))
})

test_that("SQLite resource URLs open the declared DBI connector", {
  skip_if_not_installed("RSQLite")
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(path), add = TRUE)
  resource <- resourcer::newResource(
    name = "sqlite-portability",
    url = paste0("omop+dbi:sqlite://", path),
    format = "omop.dbi.db"
  )
  client <- OMOPResourceClient$new(resource)
  on.exit(client$close(), add = TRUE)

  expect_equal(client$getDBMS(), "sqlite")
  expect_equal(DBI::dbGetQuery(client$getConnection(),
                               "SELECT 1 AS value")$value, 1L)
})

test_that("network dialect evidence remains contract-only and explicit", {
  source_sql <- paste0(
    "SELECT TOP 3 DATEADD(day, 2, event_date) AS shifted ",
    "FROM events ORDER BY person_id"
  )
  contracts <- list(
    postgresql = paste0(
      "SELECT (event_date + 2 * INTERVAL '1 day') AS shifted ",
      "FROM events ORDER BY person_id LIMIT 3"),
    `sql server` = paste0(
      "SELECT TOP 3 DATEADD(day, 2, event_date) AS shifted ",
      "FROM events ORDER BY person_id"),
    oracle = paste0(
      "SELECT (event_date + 2) AS shifted ",
      "FROM events ORDER BY person_id FETCH FIRST 3 ROWS ONLY"),
    bigquery = paste0(
      "SELECT DATE_ADD(event_date, INTERVAL 2 DAY) AS shifted ",
      "FROM events ORDER BY person_id LIMIT 3"),
    snowflake = paste0(
      "SELECT DATEADD(day, 2, event_date) AS shifted ",
      "FROM events ORDER BY person_id LIMIT 3"),
    spark = paste0(
      "SELECT DATE_ADD(event_date, 2) AS shifted ",
      "FROM events ORDER BY person_id LIMIT 3")
  )

  profile_names <- c(
    postgresql = "postgresql", `sql server` = "sqlserver",
    oracle = "oracle", bigquery = "bigquery", snowflake = "snowflake",
    spark = "spark"
  )
  for (dialect in names(contracts)) {
    expect_equal(.sql_translate(source_sql, dialect), contracts[[dialect]],
                 info = dialect)
    profile <- .databaseSupportProfile(profile_names[[dialect]])
    expect_equal(profile$support_tier, "sql_contract_tests_only",
                 info = dialect)
    expect_false(profile$live_vendor_ci, info = dialect)
  }
})

test_that("TOP translation preserves nested SELECT boundaries", {
  nested <- .sql_translate(
    "SELECT * FROM (SELECT TOP 5 * FROM x ORDER BY a) s WHERE s.a > 0",
    "postgresql"
  )
  expect_equal(
    nested,
    "SELECT * FROM (SELECT * FROM x ORDER BY a LIMIT 5) s WHERE s.a > 0"
  )

  both <- .sql_translate(
    "SELECT TOP 7 * FROM (SELECT TOP 3 * FROM x) s",
    "postgresql"
  )
  expect_equal(both,
               "SELECT * FROM (SELECT * FROM x LIMIT 3) s LIMIT 7")

  statements <- .sql_translate(
    "SELECT TOP 5 a FROM x; SELECT TOP 2 b FROM y", "postgresql")
  expect_equal(statements,
               "SELECT a FROM x LIMIT 5; SELECT b FROM y LIMIT 2")

  quoted <- .sql_translate(
    "SELECT TOP 1 ')' AS marker FROM x WHERE note = ';'", "postgresql")
  expect_equal(quoted,
               "SELECT ')' AS marker FROM x WHERE note = ';' LIMIT 1")
})

test_that("translation normalizes dialect case and rejects unknown targets", {
  expect_equal(.sql_translate("SELECT TOP 1 * FROM x", " PostgreSQL "),
               "SELECT * FROM x LIMIT 1")
  expect_error(.sql_translate("SELECT 1", "mongodb"),
               "Unsupported target SQL dialect")
  expect_error(.sql_translate("SELECT 1", c("sqlite", "duckdb")),
               "must be one character value")
})

test_that("resource URLs validate ports and bracketed IPv6", {
  parsed <- .parseOmopUrl(
    "omop+dbi:postgresql://[2001:db8::7]:5432/omop")
  expect_equal(parsed$host, "2001:db8::7")
  expect_equal(parsed$port, 5432L)

  expect_error(
    .parseOmopUrl("omop+dbi:postgresql://host:not-a-port/omop"),
    "port must be an integer"
  )
  expect_error(
    .parseOmopUrl("omop+dbi:postgresql://host:70000/omop"),
    "between 1 and 65535"
  )
  expect_error(
    .parseOmopUrl("omop+dbi:postgresql://2001:db8::7/omop"),
    "must use brackets"
  )
})

test_that("connector argument helpers do not invent malformed endpoints", {
  expect_equal(.compactConnectionArgs(list(host = "db", port = NULL,
                                            user = "alice")),
               list(host = "db", user = "alice"))
  expect_equal(.hostWithOptionalPort("db.example", NULL, ","), "db.example")
  expect_equal(.hostWithOptionalPort("db.example", 1433L, ","),
               "db.example,1433")
  expect_equal(.hostWithOptionalPort("2001:db8::7", 1433L, ","),
               "[2001:db8::7],1433")
  expect_equal(.snowflakeServer("account.region"),
               "account.region.snowflakecomputing.com")
  expect_equal(.snowflakeServer("account.region.snowflakecomputing.com"),
               "account.region.snowflakecomputing.com")
})

test_that("SQLite metadata includes views and attached schemas", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE person (person_id INTEGER)")
  DBI::dbExecute(con, "CREATE VIEW person_view AS SELECT * FROM person")
  DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS vocab")
  DBI::dbExecute(con,
                 "CREATE TABLE vocab.concept (concept_id INTEGER, concept_name TEXT)")

  h <- new.env(parent = emptyenv())
  h$conn <- con
  h$resource_client <- NULL
  h$dbms <- "sqlite"
  h$target_dialect <- "sqlite"
  h$cdm_schema <- "main"
  h$temp_tables <- character(0)
  h$temp_connection <- NULL

  expect_true(all(c("person", "person_view") %in%
                    .listTablesRaw(h, "main")))
  expect_equal(.listTablesRaw(h, "vocab"), "concept")
  expect_equal(.listColumnsRaw(h, "concept", "vocab")$column_name,
               c("concept_id", "concept_name"))
  expect_equal(.qualifyTable(h, "concept", "vocab"), "vocab.concept")
})

test_that("DuckDB metadata and session temp tables execute when available", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE person (person_id BIGINT)")
  DBI::dbExecute(con, "CREATE VIEW person_view AS SELECT * FROM person")

  h <- new.env(parent = emptyenv())
  h$conn <- con
  h$resource_client <- NULL
  h$dbms <- "duckdb"
  h$target_dialect <- "duckdb"
  h$cdm_schema <- "main"
  h$temp_tables <- character(0)
  h$temp_connection <- NULL

  expect_true(all(c("person", "person_view") %in%
                    .listTablesRaw(h, "main")))
  expect_equal(.listColumnsRaw(h, "person", "main")$data_type, "bigint")
  name <- .createTempTable(h, "duck_tmp", "SELECT 1 AS value")
  expect_equal(DBI::dbGetQuery(con, "SELECT value FROM duck_tmp")$value, 1L)
  .dropTempTable(h, name)
  expect_false(name %in% DBI::dbListTables(con))

  DBI::dbExecute(con,
    "CREATE TABLE portability_events (person_id INTEGER, event_date DATE)")
  DBI::dbExecute(con,
    paste0("INSERT INTO portability_events VALUES ",
           "(1, DATE '2020-01-01'), (2, DATE '2020-01-02'), ",
           "(3, DATE '2020-01-03')"))
  sql <- .sql_translate(
    paste0("SELECT TOP 2 person_id, DATEADD(day, 7, event_date) AS shifted ",
           "FROM portability_events ORDER BY person_id"),
    "duckdb"
  )
  result <- DBI::dbGetQuery(con, sql)
  expect_equal(result$person_id, 1:2)
  expect_equal(as.character(result$shifted),
               c("2020-01-08", "2020-01-09"))
})

test_that("DuckDB resource URLs open the declared DBI connector", {
  skip_if_not_installed("duckdb")
  path <- tempfile(fileext = ".duckdb")
  on.exit(unlink(path), add = TRUE)
  resource <- resourcer::newResource(
    name = "duckdb-portability",
    url = paste0("omop+dbi:duckdb://", path),
    format = "omop.dbi.db"
  )
  client <- OMOPResourceClient$new(resource)
  on.exit(client$close(), add = TRUE)

  expect_equal(client$getDBMS(), "duckdb")
  expect_equal(DBI::dbGetQuery(client$getConnection(),
                               "SELECT 1 AS value")$value, 1)
})

test_that("CommonDataModel can supply local metadata without executing DDL", {
  skip_if_not_installed("CommonDataModel")
  local_mocked_bindings(
    .loadVendoredSpec = function(version = NULL) NULL,
    .package = "dsOMOP"
  )

  spec <- .loadCdmSpec("5.4")
  expect_equal(spec$source, "CommonDataModel")
  expect_equal(spec$version, "5.4")
  expect_true(all(c("cdmTableName", "cdmFieldName") %in%
                    c(names(spec$table_level), names(spec$field_level))))
  expect_null(.loadCdmSpec("99.9"))
})

test_that("BigQuery metadata uses dataset-qualified INFORMATION_SCHEMA", {
  h <- new.env(parent = emptyenv())
  h$dbms <- "bigquery"
  h$target_dialect <- "bigquery"
  h$cdm_schema <- "project.dataset"
  observed <- character(0)
  local_mocked_bindings(
    .metadataQuery = function(handle, sql) {
      observed <<- c(observed, sql)
      if (grepl("COLUMNS", sql, fixed = TRUE)) {
        data.frame(COLUMN_NAME = "PERSON_ID", DATA_TYPE = "INT64",
                   IS_NULLABLE = "NO")
      } else {
        data.frame(TABLE_NAME = "PERSON")
      }
    },
    .package = "dsOMOP"
  )

  expect_equal(.listTablesRaw(h), "person")
  expect_equal(.listColumnsRaw(h, "person")$data_type, "int64")
  expect_true(any(grepl("`project.dataset.INFORMATION_SCHEMA.TABLES`",
                        observed, fixed = TRUE)))
  expect_false(any(grepl("table_type", observed, ignore.case = TRUE)))
})

test_that("Snowflake metadata normalizes unquoted object lookup case", {
  h <- new.env(parent = emptyenv())
  h$dbms <- "snowflake"
  h$target_dialect <- "snowflake"
  h$cdm_schema <- "cdm"
  observed <- character(0)
  local_mocked_bindings(
    .metadataQuery = function(handle, sql) {
      observed <<- c(observed, sql)
      if (grepl("COLUMNS", sql, fixed = TRUE)) {
        data.frame(column_name = "PERSON_ID", data_type = "NUMBER",
                   is_nullable = "NO")
      } else {
        data.frame(table_name = "PERSON")
      }
    },
    .package = "dsOMOP"
  )

  expect_equal(.listTablesRaw(h), "person")
  expect_equal(.listColumnsRaw(h, "person")$column_name, "person_id")
  expect_true(all(grepl("CDM", observed, fixed = TRUE)))
  expect_true(any(grepl("TABLE_NAME = 'PERSON'", observed, fixed = TRUE)))
})

test_that("Spark metadata uses SHOW and DESCRIBE without partition sentinels", {
  h <- new.env(parent = emptyenv())
  h$dbms <- "databricks"
  h$target_dialect <- "spark"
  h$cdm_schema <- "catalog.cdm"
  observed <- character(0)
  local_mocked_bindings(
    .metadataQuery = function(handle, sql) {
      observed <<- c(observed, sql)
      if (startsWith(sql, "DESCRIBE")) {
        data.frame(
          col_name = c("PERSON_ID", "# Partition Information", ""),
          data_type = c("bigint", "", "")
        )
      } else {
        data.frame(namespace = "catalog.cdm", tableName = "PERSON",
                   isTemporary = FALSE)
      }
    },
    .package = "dsOMOP"
  )

  expect_equal(.listTablesRaw(h), "person")
  expect_equal(.listColumnsRaw(h, "person")$column_name, "person_id")
  expect_equal(observed[[1]], "SHOW TABLES IN catalog.cdm")
  expect_equal(observed[[2]], "DESCRIBE TABLE catalog.cdm.person")
})
