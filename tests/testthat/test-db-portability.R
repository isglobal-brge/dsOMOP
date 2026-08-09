test_that("database support profiles distinguish tested and contract-only adapters", {
  declared <- c(
    "postgresql", "sqlite", "duckdb", "mysql", "mariadb",
    "sql_server", "synapse", "pdw", "oracle", "redshift", "bigquery",
    "snowflake", "spark", "databricks"
  )
  profiles <- lapply(declared, .databaseSupportProfile)

  expect_true(all(vapply(profiles, function(x) {
    is.character(x$target_dialect) && length(x$target_dialect) == 1L
  }, logical(1))))
  expect_true(all(vapply(c("postgresql", "mysql", "mariadb"), function(dbms) {
    isTRUE(.databaseSupportProfile(dbms)$live_vendor_ci)
  }, logical(1))))
  expect_true(all(vapply(
    setdiff(declared, c("postgresql", "mysql", "mariadb")), function(dbms) {
      identical(.databaseSupportProfile(dbms)$live_vendor_ci, FALSE)
    }, logical(1)
  )))
  expect_equal(.databaseSupportProfile("sqlite")$verification,
               "embedded_integration_tests")
  expect_equal(.databaseSupportProfile("duckdb")$verification,
               "optional_embedded_integration_tests")
  expect_equal(.databaseSupportProfile("postgresql")$verification,
               "vendor_integration_tests")
  expect_equal(.databaseSupportProfile("mysql")$verification,
               "vendor_integration_tests")
  expect_equal(.databaseSupportProfile("mariadb")$verification,
               "vendor_integration_tests")
  expect_true(all(vapply(profiles, function(x) {
      identical(x$support_tier, x$verification) &&
      identical(x$sql_translation, "builtin_reviewed_subset") &&
      identical(x$sql_translation_patterns,
                c("select_top_integer", "dateadd_day_integer",
                  "datediff_day_integer", "cast_target_normalization",
                  "sample_stddev_mysql", "oracle_bare_alias")) &&
      is.logical(x$sqlrender_installed) &&
      is.logical(x$sqlrender_runtime)
  }, logical(1))))

  expect_equal(.databaseSupportProfile("sqlserver")$sqlrender_target_dialect,
               "sql server")
  expect_equal(.databaseSupportProfile("synapse")$sqlrender_target_dialect,
               "synapse")
  expect_equal(.databaseSupportProfile("pdw")$sqlrender_target_dialect,
               "pdw")
  expect_equal(.databaseSupportProfile("databricks")$sqlrender_target_dialect,
               "spark")
  expect_null(.databaseSupportProfile("mysql")$sqlrender_target_dialect)
  expect_equal(.databaseSupportProfile("mysql")$ohdsi_sql_translation,
               "reviewed_mysql_extension_only")
  sqlrender_path <- suppressWarnings(tryCatch(
    find.package("SqlRender", quiet = TRUE), error = function(e) ""
  ))
  expect_identical(
    .databaseSupportProfile("postgresql")$sqlrender_version,
    if (nzchar(sqlrender_path)) {
      as.character(utils::packageVersion("SqlRender"))
    } else NULL
  )
  expect_equal(.databaseSupportProfile("oracle")$ohdsi_temporary_objects,
               "unsafe_lifecycle_blocked")
  expect_equal(.databaseSupportProfile("bigquery")$ohdsi_temporary_objects,
               "unsafe_lifecycle_blocked")
  expect_equal(.databaseSupportProfile("spark")$ohdsi_temporary_objects,
               "unsafe_lifecycle_blocked")
  expect_equal(.databaseSupportProfile("redshift")$ohdsi_temporary_objects,
               "unsafe_lifecycle_blocked")
  expect_equal(.databaseSupportProfile("snowflake")$ohdsi_temporary_objects,
               "unsafe_lifecycle_blocked")

  network_backends <- setdiff(
    declared, c("postgresql", "sqlite", "duckdb", "mysql", "mariadb")
  )
  expect_true(all(vapply(network_backends, function(dbms) {
    identical(.databaseSupportProfile(dbms)$support_tier,
              "sql_contract_tests_only")
  }, logical(1))))
})

test_that("static database profiles never initialize SqlRender", {
  inspected <- NULL
  local_mocked_bindings(
    .sqlRenderRuntimeInfo = function(dbms, inspect_runtime = TRUE) {
      inspected <<- inspect_runtime
      list(
        target_dialect = "postgresql", installed = TRUE,
        version = "test", target_available = NA,
        runtime_inspected = inspect_runtime, supported_dialects = character(0)
      )
    },
    .package = "dsOMOP"
  )
  profile <- .databaseSupportProfile("postgresql")
  expect_false(inspected)
  expect_true(is.na(profile$sqlrender_runtime))
})

test_that("temporary-object capabilities match SQL actually generated", {
  h <- new.env(parent = emptyenv())

  for (dbms in c("postgresql", "sqlite", "duckdb")) {
    h$dbms <- dbms
    expect_match(.tempCreateSql(h, "tmp_x", "SELECT 1"),
                 "^CREATE TEMP TABLE tmp_x AS")
  }
  h$dbms <- "postgresql"
  expect_equal(.tempDropSql(h, "tmp_x"),
               "DROP TABLE IF EXISTS pg_temp.tmp_x")
  for (dbms in c("sqlite", "duckdb")) {
    h$dbms <- dbms
    expect_equal(.tempDropSql(h, "tmp_x"),
                 "DROP TABLE IF EXISTS temp.tmp_x")
  }
  for (dbms in c("mysql", "mariadb")) {
    h$dbms <- dbms
    expect_equal(.tempCreateSql(h, "tmp_x", "SELECT 1"),
                 "CREATE TEMPORARY TABLE tmp_x AS SELECT 1")
    expect_equal(.tempDropSql(h, "tmp_x"),
                 "DROP TEMPORARY TABLE IF EXISTS tmp_x")
  }

  for (dbms in c("sqlserver", "synapse", "pdw", "oracle", "redshift",
                 "bigquery", "snowflake", "spark", "databricks")) {
    h$dbms <- dbms
    expect_error(.tempCreateSql(h, "tmp_x", "SELECT 1"),
                 "not implemented safely")
    expect_error(.tempDropSql(h, "tmp_x"),
                 "refusing to generate a DROP")
  }
})

test_that("SqlRender dialect resolution preserves OHDSI targets", {
  expect_equal(.resolveSqlRenderDialect("postgres"), "postgresql")
  expect_equal(.resolveSqlRenderDialect("SQL Server"), "sql server")
  expect_equal(.resolveSqlRenderDialect("azure_synapse"), "synapse")
  expect_equal(.resolveSqlRenderDialect("pdw"), "pdw")
  expect_equal(.resolveSqlRenderDialect("databricks"), "spark")
  expect_equal(.resolveSqlRenderDialect("duckdb"), "duckdb")
  expect_null(.resolveSqlRenderDialect("mysql"))
  expect_null(.resolveSqlRenderDialect("mariadb"))
  expect_error(.resolveSqlRenderDialect(NULL), "one non-empty character")
  expect_error(.resolveSqlRenderDialect("mongodb"), "Unsupported DBMS")
})

test_that("canonical OHDSI translation uses SqlRender and fails closed", {
  skip_if_not_installed("SqlRender")

  source_sql <- paste0(
    "SELECT TOP 3 DATEADD(day, 2, event_date) AS shifted ",
    "FROM events ORDER BY person_id;"
  )
  pg <- .translateOhdsiSql(source_sql, "postgresql")
  expect_match(pg, "LIMIT 3", fixed = TRUE)
  expect_match(pg, "INTERVAL", ignore.case = TRUE)
  expect_false(grepl("SELECT\\s+TOP", pg, ignore.case = TRUE))

  top_sql <- "SELECT TOP 1 * FROM person;"
  expect_equal(.translateOhdsiSql(top_sql, "sqlserver"), top_sql)
  expect_equal(.translateOhdsiSql(top_sql, "synapse"), top_sql)
  expect_equal(.translateOhdsiSql(top_sql, "pdw"), top_sql)
  for (dbms in c("oracle", "bigquery", "snowflake", "spark", "databricks")) {
    translated <- .translateOhdsiSql(top_sql, dbms)
    expect_false(grepl("SELECT\\s+TOP", translated, ignore.case = TRUE),
                 info = dbms)
    expect_match(translated,
                 if (dbms == "oracle") "FETCH FIRST 1 ROWS ONLY" else "LIMIT 1",
                 ignore.case = TRUE, info = dbms)
    expect_null(attr(translated, "sqlDialect"), info = dbms)
  }
  expect_error(.translateOhdsiSql("SELECT 1", "mysql"),
               "does not support MySQL/MariaDB")
  expect_error(.translateOhdsiSql("SELECT 1", "mariadb"),
               "does not support MySQL/MariaDB")
  expect_error(.translateOhdsiSql("SELECT 1", "mongodb"),
               "Unsupported DBMS")
  expect_error(.translateOhdsiSql(character(0), "sqlite"),
               "one non-empty character")
})

test_that("canonical OHDSI translation never falls back when runtime is absent", {
  local_mocked_bindings(
    .sqlRenderRuntimeInfo = function(dbms) list(
      target_dialect = "postgresql",
      installed = FALSE,
      version = NULL,
      target_available = FALSE,
      supported_dialects = character(0)
    ),
    .package = "dsOMOP"
  )
  expect_error(
    .translateOhdsiSql("SELECT TOP 1 * FROM person;", "postgresql"),
    "requires the optional SqlRender package"
  )

  local_mocked_bindings(
    .sqlRenderRuntimeInfo = function(dbms) list(
      target_dialect = "postgresql",
      installed = TRUE,
      version = "0.0.0",
      target_available = FALSE,
      supported_dialects = "sqlite"
    ),
    .package = "dsOMOP"
  )
  expect_error(
    .translateOhdsiSql("SELECT TOP 1 * FROM person;", "postgresql"),
    "does not support target dialect"
  )
})

test_that("OHDSI temp translation is explicit and blocks persistent emulation", {
  skip_if_not_installed("SqlRender")
  source_sql <- "SELECT * INTO #tmp FROM person;"

  expect_error(.translateOhdsiSql(source_sql, "postgresql"),
               "temporary objects are disabled")
  pg <- .translateOhdsiSql(
    source_sql,
    "postgresql",
    allow_temp_objects = TRUE
  )
  expect_match(pg, "CREATE TEMP TABLE", fixed = TRUE)
  expect_error(
    .translateOhdsiSql(source_sql, "oracle", allow_temp_objects = TRUE),
    "temp-only create/drop lifecycle"
  )
  expect_error(
    .translateOhdsiSql(source_sql, "bigquery", allow_temp_objects = TRUE),
    "temp-only create/drop lifecycle"
  )
  expect_error(
    .translateOhdsiSql(source_sql, "databricks", allow_temp_objects = TRUE),
    "temp-only create/drop lifecycle"
  )
  expect_error(
    .translateOhdsiSql(source_sql, "redshift", allow_temp_objects = TRUE),
    "temp-only create/drop lifecycle"
  )
  expect_error(
    .translateOhdsiSql(source_sql, "snowflake", allow_temp_objects = TRUE),
    "temp-only create/drop lifecycle"
  )
})

test_that("canonical OHDSI read queries execute on embedded SQLite", {
  skip_if_not_installed("SqlRender")
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  rendered <- .renderOhdsiSql(
    handle,
    "SELECT TOP @n person_id FROM @table ORDER BY person_id;",
    parameters = list(n = 2L, table = "person")
  )
  expect_match(rendered, "LIMIT 2", fixed = TRUE)
  expect_false(grepl("@n|@table", rendered))

  result <- .queryOhdsiSql(
    handle,
    "SELECT TOP @n person_id FROM @table ORDER BY person_id;",
    parameters = list(n = 2L, table = "person")
  )
  expect_equal(result$person_id, 1:2)

  expect_error(
    .renderOhdsiSql(handle, "SELECT @missing FROM person;"),
    "unresolved @parameter",
    fixed = TRUE
  )
  expect_error(
    .renderOhdsiSql(handle, "SELECT 1;", parameters = list(1L)),
    "named list"
  )
  expect_error(
    .queryOhdsiSql(handle, "SELECT 1; SELECT 2;"),
    "exactly one SQL statement"
  )
  expect_error(
    .queryOhdsiSql(handle, "DELETE FROM person;"),
    "only SELECT or WITH"
  )
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

test_that("SQLite cleanup cannot drop a persistent temporary-table homonym", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  name <- "dsomop_sqlite_drop_guard"
  DBI::dbExecute(handle$conn, paste0(
    "CREATE TABLE main.", name, " (value INTEGER)"
  ))
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO main.", name, " VALUES (7)"
  ))

  .createTempTable(handle, name, "SELECT 1 AS value")
  DBI::dbExecute(handle$conn, paste0("DROP TABLE temp.", name))
  .dropTempTable(handle, name)

  expect_equal(
    DBI::dbGetQuery(handle$conn, paste0(
      "SELECT value FROM main.", name
    ))$value,
    7L
  )
  expect_false(name %in% handle$temp_tables)
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
  # Exact AVG/SD/MIN/MAX templates are available only through the typed,
  # person-bounded sticky redesign path, never the direct SQL catalog.
  expect_false("dsomop:drug_era.length_stats" %in% names(entries))
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
      "SELECT (event_date + 2) shifted ",
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
    expected_tier <- if (identical(dialect, "postgresql")) {
      "vendor_integration_tests"
    } else {
      "sql_contract_tests_only"
    }
    expect_equal(profile$support_tier, expected_tier, info = dialect)
    expect_identical(profile$live_vendor_ci,
                     identical(dialect, "postgresql"), info = dialect)
  }
})

test_that("DATEDIFF day translation has an explicit calendar-day contract", {
  source_sql <- "DATEDIFF(day, e.start_date, e.end_date)"
  contracts <- list(
    postgresql = paste0(
      "(CAST(e.end_date AS DATE) - CAST(e.start_date AS DATE))"
    ),
    mysql = "DATEDIFF(e.end_date, e.start_date)",
    sqlite = paste0(
      "CAST(julianday(DATE(e.end_date)) - ",
      "julianday(DATE(e.start_date)) AS INTEGER)"
    ),
    oracle = "CAST(TRUNC(e.end_date) - TRUNC(e.start_date) AS INTEGER)",
    bigquery = paste0(
      "DATE_DIFF(CAST(e.end_date AS DATE), ",
      "CAST(e.start_date AS DATE), DAY)"
    ),
    spark = paste0(
      "DATEDIFF(CAST(e.end_date AS DATE), ",
      "CAST(e.start_date AS DATE))"
    ),
    duckdb = paste0(
      "DATE_DIFF('day', CAST(e.start_date AS DATE), ",
      "CAST(e.end_date AS DATE))"
    ),
    `sql server` = source_sql,
    redshift = source_sql,
    snowflake = source_sql
  )

  for (dialect in names(contracts)) {
    expect_equal(.sql_translate(source_sql, dialect), contracts[[dialect]],
                 info = dialect)
  }
  expect_setequal(
    .omopQuerySupportedDialects(source_sql),
    names(contracts)
  )

  protected <- .sql_translate(
    paste0(
      "SELECT 'DATEDIFF(day, a.start_date, a.end_date)' AS label, ",
      source_sql, " AS elapsed FROM events e"
    ),
    "postgresql"
  )
  expect_match(
    protected, "'DATEDIFF(day, a.start_date, a.end_date)'", fixed = TRUE
  )
  expect_match(protected, contracts$postgresql, fixed = TRUE)
})

test_that("curated duration templates use portable DATEDIFF", {
  ids <- c(
    "condition.duration_stats", "drug.duration_stats",
    "drug_era.length_stats", "observation_period.length_by_gender",
    "observation_period.long_period_count",
    "observation_period.length_stats", "visit.duration_stats"
  )
  queries <- .ql_load_queries()
  expect_true(all(ids %in% names(queries)))

  date_column <- paste0(
    "(?:[A-Za-z_][A-Za-z0-9_]*\\.)?",
    "[A-Za-z_][A-Za-z0-9_]*(?:_date|_datetime)"
  )
  direct_subtraction <- paste0(
    date_column, "\\s*-\\s*", date_column
  )
  for (id in ids) {
    sql <- queries[[id]]$sql
    expect_match(sql, "\\bDATEDIFF\\s*\\(\\s*day\\s*,",
                 ignore.case = TRUE, perl = TRUE, info = id)
    expect_false(grepl(direct_subtraction, sql, ignore.case = TRUE,
                       perl = TRUE), info = id)
    expect_true(all(c("postgresql", "mysql") %in%
                      .omopQuerySupportedDialects(sql)), info = id)
    expect_false(grepl("DATEDIFF\\s*\\(\\s*day\\s*,",
                       .sql_translate(sql, "postgresql"),
                       ignore.case = TRUE, perl = TRUE), info = id)
    expect_false(grepl("DATEDIFF\\s*\\(\\s*day\\s*,",
                       .sql_translate(sql, "mysql"),
                       ignore.case = TRUE, perl = TRUE), info = id)
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

test_that("TOP translation fails closed around set operators", {
  unsafe <- c(
    "SELECT TOP 1 x FROM a UNION ALL SELECT x FROM b",
    "SELECT x FROM a UNION ALL SELECT TOP 1 x FROM b",
    "SELECT TOP 1 x FROM a EXCEPT SELECT x FROM b",
    "SELECT TOP 1 x FROM a INTERSECT SELECT x FROM b"
  )
  for (statement in unsafe) {
    expect_error(
      .sql_translate(statement, "postgresql"),
      "Cannot safely translate SELECT TOP",
      info = statement
    )
  }
  expect_equal(
    .sql_translate(
      "SELECT TOP 1 'UNION EXCEPT INTERSECT' AS label FROM person",
      "postgresql"
    ),
    "SELECT 'UNION EXCEPT INTERSECT' AS label FROM person LIMIT 1"
  )
})

test_that("built-in translation never rewrites quoted SQL text", {
  sql <- paste0(
    "SELECT TOP 2 CAST(value AS INTEGER), STDDEV(value), ",
    "'SELECT TOP 1 x FROM y' AS top_text, ",
    "'DATEADD(day, 1, event_date)' AS dateadd_text, ",
    "'AS INTEGER and STDDEV(value)' AS cast_text ",
    "FROM measurement /* SELECT TOP 1 AS INTEGER */"
  )
  translated <- .sql_translate(sql, "mysql")

  expect_match(translated, "SELECT CAST\\(value AS SIGNED\\)")
  expect_match(translated, "STDDEV_SAMP\\(value\\)")
  expect_match(translated, "'SELECT TOP 1 x FROM y'", fixed = TRUE)
  expect_match(translated, "'DATEADD(day, 1, event_date)'", fixed = TRUE)
  expect_match(translated, "'AS INTEGER and STDDEV(value)'", fixed = TRUE)
  expect_match(translated, "/* SELECT TOP 1 AS INTEGER */", fixed = TRUE)
  expect_match(translated, "LIMIT 2$", perl = TRUE)

  expect_equal(
    .sql_translate(
      "SELECT \"AS INTEGER\", `SELECT TOP 1`, [DATEADD] FROM x",
      "mysql"
    ),
    "SELECT \"AS INTEGER\", `SELECT TOP 1`, [DATEADD] FROM x"
  )
  expect_equal(
    .sql_translate("SELECT $$SELECT TOP 1 AS INTEGER$$ FROM x", "postgresql"),
    "SELECT $$SELECT TOP 1 AS INTEGER$$ FROM x"
  )
})

test_that("literal protection follows PostgreSQL and MySQL backslash rules", {
  expect_equal(
    .sql_translate(
      paste0(
        "SELECT TOP 1 '\\' AS slash_value, ",
        "DATEADD(day, 1, event_date) AS shifted FROM events"
      ),
      "postgresql"
    ),
    paste0(
      "SELECT '\\' AS slash_value, ",
      "(event_date + 1 * INTERVAL '1 day') AS shifted ",
      "FROM events LIMIT 1"
    )
  )
  expect_equal(
    .sql_translate(
      "SELECT TOP 1 E'it\\'s TOP 9' AS note FROM events",
      "postgresql"
    ),
    "SELECT E'it\\'s TOP 9' AS note FROM events LIMIT 1"
  )
  expect_equal(
    .sql_translate(
      "SELECT TOP 1 'it\\'s TOP 9' AS note FROM events",
      "mysql"
    ),
    "SELECT 'it\\'s TOP 9' AS note FROM events LIMIT 1"
  )
})

test_that("TOP limits precede terminal comments and preserve #temp", {
  expect_equal(
    .sql_translate(
      "SELECT TOP 1 * FROM person -- final TOP 9",
      "postgresql"
    ),
    "SELECT * FROM person LIMIT 1 -- final TOP 9"
  )

  sql <- paste0(
    "SELECT TOP 1 CAST(value AS INTEGER) FROM #temp ",
    "WHERE event_date = DATEADD(day, 1, index_date) # final TOP 9"
  )
  expect_equal(
    .sql_translate(sql, "mysql"),
    paste0(
      "SELECT CAST(value AS SIGNED) FROM #temp ",
      "WHERE event_date = DATE_ADD(index_date, INTERVAL 1 DAY) ",
      "LIMIT 1 # final TOP 9"
    )
  )
})

test_that("MySQL-family STDDEV is explicitly sample-standard-deviation", {
  translated <- .sql_translate(
    "SELECT STDDEV(value), STDDEV_POP(value), STDDEV_SAMP(value) FROM x",
    "mysql"
  )
  expect_equal(
    translated,
    paste0(
      "SELECT STDDEV_SAMP(value), STDDEV_POP(value), ",
      "STDDEV_SAMP(value) FROM x"
    )
  )
})

test_that("Oracle translation removes aliases without mutating literals or casts", {
  sql <- paste0(
    "WITH source_rows AS (SELECT CAST(t.person_id AS NUMBER(19)) AS row_id ",
    "FROM cdm.person AS t WHERE t.label = 'AS treatment') ",
    "SELECT q.row_id AS result_id FROM (SELECT row_id FROM source_rows) AS q"
  )
  translated <- .sql_translate(sql, "oracle")

  expect_match(translated, "source_rows AS \\(")
  expect_match(translated, "CAST\\(t.person_id AS NUMBER\\(19\\)\\)")
  expect_match(translated, "FROM cdm.person t")
  expect_match(translated, "FROM \\(SELECT row_id FROM source_rows\\) q")
  expect_match(translated, "'AS treatment'", fixed = TRUE)
  expect_false(grepl("FROM cdm\\.person AS t", translated))
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

test_that("PostgreSQL day differences cast DATEADD bounds before subtraction", {
  h <- new.env(parent = emptyenv())
  h$target_dialect <- "postgresql"
  sql <- .sql_translate(
    .omopDateDiffDays(
      h, "DATEADD(day, 30, c.cohort_start_date)", "c.cohort_start_date"
    ),
    "postgresql"
  )

  expect_match(sql, "CAST\\(\\(c.cohort_start_date \\+ 30 \\* INTERVAL")
  expect_match(sql, "AS DATE\\) - CAST\\(c.cohort_start_date AS DATE\\)")
  expect_false(grepl("AS INTEGER", sql, fixed = TRUE))
})

test_that("MySQL-family scalar SQL avoids incompatible casts and pipe concat", {
  h <- new.env(parent = emptyenv())
  h$target_dialect <- "mysql"
  concat <- .omopConcatSql(
    h, .omopTextCastSql(h, "x"), "':'", .omopTextCastSql(h, "y")
  )
  translated <- .sql_translate(
    paste(
      "SELECT CAST(x AS VARCHAR(64)), CAST(y AS INTEGER),",
      "CAST(z AS BIGINT), CAST(q AS FLOAT)"
    ),
    "mysql"
  )

  expect_match(concat, "^CONCAT\\(")
  expect_false(grepl("||", concat, fixed = TRUE))
  expect_match(translated, "AS CHAR\\(64\\)")
  expect_equal(length(gregexpr("AS SIGNED", translated, fixed = TRUE)[[1L]]), 2L)
  expect_match(translated, "AS DECIMAL\\(38,10\\)")
  expect_false(grepl("AS VARCHAR|AS INTEGER|AS BIGINT|AS FLOAT",
                     translated, ignore.case = TRUE))
})

test_that("Achilles calendar spine is bounded portable SQL", {
  h <- new.env(parent = emptyenv())
  h$target_dialect <- "mysql"
  h$cdm_schema <- "cdm"
  ctx <- list(scoped_cohort = NULL)
  local_mocked_bindings(
    .executeQuery = function(handle, sql) {
      data.frame(lo_year = 2020L, hi_year = 2022L)
    },
    .package = "dsOMOP"
  )

  sql <- .omopAchillesYearSpineSql(h, ctx)
  expect_match(sql, "SELECT 2020 AS yr UNION ALL SELECT 2021 AS yr")
  expect_match(sql, "SELECT 2022 AS yr")
  expect_false(grepl("generate_series|recursive", sql, ignore.case = TRUE))
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

  guard <- "dsomop_duckdb_drop_guard"
  DBI::dbExecute(con, paste0("CREATE TABLE main.", guard,
                             " (value INTEGER)"))
  DBI::dbExecute(con, paste0("INSERT INTO main.", guard, " VALUES (7)"))
  .createTempTable(h, guard, "SELECT 1 AS value")
  DBI::dbExecute(con, paste0("DROP TABLE temp.", guard))
  .dropTempTable(h, guard)
  expect_equal(DBI::dbGetQuery(
    con, paste0("SELECT value FROM main.", guard)
  )$value, 7)
  expect_false(guard %in% h$temp_tables)

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

test_that("MySQL metadata preserves case-sensitive physical table names", {
  h <- new.env(parent = emptyenv())
  h$dbms <- "mysql"
  h$target_dialect <- "mysql"
  h$cdm_schema <- "cdm"
  observed <- character(0)
  local_mocked_bindings(
    .metadataQuery = function(handle, sql) {
      observed <<- c(observed, sql)
      if (grepl("COLUMNS", sql, fixed = TRUE)) {
        data.frame(column_name = "PERSON_ID", data_type = "BIGINT",
                   is_nullable = "NO")
      } else {
        data.frame(table_name = "Person")
      }
    },
    .package = "dsOMOP"
  )

  expect_identical(.listTablesRaw(h), "person")
  expect_identical(.qualifyTable(h, "person"), "cdm.`Person`")
  expect_identical(.listColumnsRaw(h, "person")$column_name, "person_id")
  expect_true(any(grepl("TABLE_NAME = 'Person'", observed, fixed = TRUE)))

  local_mocked_bindings(
    .metadataQuery = function(handle, sql) {
      data.frame(table_name = c("PERSON", "person"))
    },
    .package = "dsOMOP"
  )
  expect_error(.listTablesRaw(h), "Ambiguous physical table names")
})

test_that("Snowflake metadata normalizes unquoted object lookup case", {
  h <- new.env(parent = emptyenv())
  h$dbms <- "snowflake"
  h$target_dialect <- "snowflake"
  h$cdm_schema <- "analytics.cdm"
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
  expect_true(all(grepl("ANALYTICS.INFORMATION_SCHEMA", observed,
                        fixed = TRUE)))
  expect_true(all(grepl("TABLE_CATALOG = 'ANALYTICS'", observed,
                        fixed = TRUE)))
  expect_true(any(grepl("TABLE_NAME = 'PERSON'", observed, fixed = TRUE)))
})

test_that("metadata discovery quotes non-default namespace components", {
  cases <- list(
    sqlserver = list(
      dialect = "sql server", schema = "Clinical-DB.cdm-research",
      table_sql = "[Clinical-DB].INFORMATION_SCHEMA.TABLES",
      column_sql = "[Clinical-DB].INFORMATION_SCHEMA.COLUMNS"
    ),
    snowflake = list(
      dialect = "snowflake", schema = "Analytics-Prod.Cdm-Research",
      table_sql = '"Analytics-Prod".INFORMATION_SCHEMA.TABLES',
      column_sql = '"Analytics-Prod".INFORMATION_SCHEMA.COLUMNS'
    ),
    duckdb = list(
      dialect = "duckdb", schema = "attached-db.cdm-research",
      table_sql = '"attached-db".INFORMATION_SCHEMA.TABLES',
      column_sql = '"attached-db".INFORMATION_SCHEMA.COLUMNS'
    )
  )

  for (dbms in names(cases)) {
    case <- cases[[dbms]]
    handle <- new.env(parent = emptyenv())
    handle$dbms <- dbms
    handle$target_dialect <- case$dialect
    handle$cdm_schema <- case$schema
    observed <- character(0)
    local_mocked_bindings(
      .metadataQuery = function(handle, sql) {
        observed <<- c(observed, sql)
        if (grepl("COLUMNS", sql, fixed = TRUE)) {
          data.frame(column_name = "person_id", data_type = "bigint",
                     is_nullable = "NO")
        } else {
          data.frame(table_name = "person")
        }
      },
      .package = "dsOMOP"
    )
    expect_identical(.listTablesRaw(handle), "person", info = dbms)
    expect_identical(.listColumnsRaw(handle, "person")$column_name,
                     "person_id", info = dbms)
    expect_match(observed[[1L]], case$table_sql, fixed = TRUE, info = dbms)
    expect_match(observed[[2L]], case$column_sql, fixed = TRUE, info = dbms)
  }

  spark <- new.env(parent = emptyenv())
  spark$dbms <- "spark"
  spark$target_dialect <- "spark"
  spark$cdm_schema <- "catalog-prod.cdm-research"
  observed <- character(0)
  local_mocked_bindings(
    .metadataQuery = function(handle, sql) {
      observed <<- c(observed, sql)
      if (startsWith(sql, "DESCRIBE")) {
        data.frame(col_name = "person_id", data_type = "bigint")
      } else {
        data.frame(tableName = "person")
      }
    },
    .package = "dsOMOP"
  )
  expect_identical(.listTablesRaw(spark), "person")
  expect_identical(.listColumnsRaw(spark, "person")$column_name, "person_id")
  expect_identical(observed[[1L]],
                   "SHOW TABLES IN `catalog-prod`.`cdm-research`")
  expect_identical(observed[[2L]],
                   "DESCRIBE TABLE `catalog-prod`.`cdm-research`.person")
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
