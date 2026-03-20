# --- Resource URL B64 Parsing Tests ---

test_that("B64 resource URL round-trips correctly", {
  # Simulate what resource.js produces
  config <- list(
    dbms = "postgresql", host = "db.hospital.org",
    port = 5432L, database = "omop_cdm",
    cdm_schema = "cdm", vocabulary_schema = "vocab"
  )
  json <- as.character(jsonlite::toJSON(config, auto_unbox = TRUE))
  b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json)))
  b64 <- gsub("+", "-", b64, fixed = TRUE)
  b64 <- gsub("/", "_", b64, fixed = TRUE)
  b64 <- gsub("=+$", "", b64)
  url <- paste0("omop+dbi:///B64:", b64)

  # Parse using the same logic as resource.R
  body <- sub("^omop[+]dbi:///", "", url)
  expect_true(startsWith(body, "B64:"))
  b64r <- substring(body, 5)
  b64r <- gsub("-", "+", b64r, fixed = TRUE)
  b64r <- gsub("_", "/", b64r, fixed = TRUE)
  pad <- (4 - nchar(b64r) %% 4) %% 4
  if (pad > 0) b64r <- paste0(b64r, strrep("=", pad))
  params <- jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(b64r)),
                                simplifyVector = FALSE)

  expect_equal(params$dbms, "postgresql")
  expect_equal(params$host, "db.hospital.org")
  expect_equal(params$port, 5432)
  expect_equal(params$database, "omop_cdm")
  expect_equal(params$cdm_schema, "cdm")
  expect_equal(params$vocabulary_schema, "vocab")
})

test_that("B64 URL contains only parser-safe characters", {
  config <- list(dbms = "sql_server", host = "10.0.0.1",
                 port = 1433L, database = "my_db",
                 cdm_schema = "dbo", results_schema = "results")
  json <- as.character(jsonlite::toJSON(config, auto_unbox = TRUE))
  b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json)))
  b64 <- gsub("+", "-", b64, fixed = TRUE)
  b64 <- gsub("/", "_", b64, fixed = TRUE)
  b64 <- gsub("=+$", "", b64)
  url <- paste0("omop+dbi:///B64:", b64)

  # Must not contain ?, &, +, or = (Opal parser-unsafe)
  body <- sub("^omop[+]dbi:///B64:", "", url)
  expect_false(grepl("[?&+=]", body))
  # Only alphanumeric, -, _
  expect_true(grepl("^[A-Za-z0-9_-]+$", body))
})

test_that("B64 URL handles special chars in schema names", {
  config <- list(dbms = "postgresql", host = "db.example.com",
                 port = 5432L, database = "prod",
                 cdm_schema = "omop-v5.4_data")
  json <- as.character(jsonlite::toJSON(config, auto_unbox = TRUE))
  b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json)))
  b64 <- gsub("+", "-", b64, fixed = TRUE)
  b64 <- gsub("/", "_", b64, fixed = TRUE)
  b64 <- gsub("=+$", "", b64)

  # Decode back
  b64r <- gsub("-", "+", b64, fixed = TRUE)
  b64r <- gsub("_", "/", b64r, fixed = TRUE)
  pad <- (4 - nchar(b64r) %% 4) %% 4
  if (pad > 0) b64r <- paste0(b64r, strrep("=", pad))
  params <- jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(b64r)),
                                simplifyVector = FALSE)

  expect_equal(params$cdm_schema, "omop-v5.4_data")
})

# ===========================================================================
# DATEADD Translation Tests — one per dialect
# ===========================================================================

# --- PostgreSQL: (expr + N * INTERVAL '1 day') ---

test_that("DATEADD translates correctly for postgresql", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "postgresql")
  expect_true(grepl("start_date", sql))
  expect_true(grepl("30 \\* INTERVAL '1 day'", sql))
  expect_false(grepl("DATEADD", sql))
})

test_that("DATEADD with negative days for postgresql", {
  sql <- .sql_translate("DATEADD(day, -7, end_date)", "postgresql")
  expect_true(grepl("-7 \\* INTERVAL '1 day'", sql))
})

# --- MySQL / MariaDB: DATE_ADD(expr, INTERVAL N DAY) ---

test_that("DATEADD translates correctly for mysql", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "mysql")
  expect_true(grepl("DATE_ADD", sql))
  expect_true(grepl("INTERVAL 30 DAY", sql))
  expect_false(grepl("DATEADD", sql))
})

test_that("DATEADD with negative days for mysql", {
  sql <- .sql_translate("DATEADD(day, -7, end_date)", "mysql")
  expect_true(grepl("DATE_ADD", sql))
  expect_true(grepl("INTERVAL -7 DAY", sql))
})

# --- Oracle: (expr + N) ---

test_that("DATEADD translates correctly for oracle", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "oracle")
  expect_true(grepl("start_date \\+ 30", sql))
  expect_false(grepl("DATEADD", sql))
  expect_false(grepl("INTERVAL", sql))
})

test_that("DATEADD with negative days for oracle", {
  sql <- .sql_translate("DATEADD(day, -7, end_date)", "oracle")
  expect_true(grepl("end_date \\+ -7", sql))
})

# --- BigQuery: DATE_ADD(expr, INTERVAL N DAY) ---

test_that("DATEADD translates correctly for bigquery", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "bigquery")
  expect_true(grepl("DATE_ADD", sql))
  expect_true(grepl("INTERVAL 30 DAY", sql))
  expect_false(grepl("DATEADD", sql))
})

# --- Spark: DATE_ADD(expr, N) ---

test_that("DATEADD translates correctly for spark", {
  sql <- .sql_translate("DATEADD(day, 10, obs_date)", "spark")
  expect_true(grepl("DATE_ADD\\(obs_date, 10\\)", sql))
  expect_false(grepl("DATEADD", sql))
  expect_false(grepl("INTERVAL", sql))
})

# --- SQLite / DuckDB: DATE(expr, 'N days') ---

test_that("DATEADD translates correctly for sqlite", {
  sql <- .sql_translate("DATEADD(day, 5, start_date)", "sqlite")
  expect_true(grepl("DATE\\(start_date", sql))
  expect_true(grepl("5 days", sql))
  expect_false(grepl("DATEADD", sql))
})

# --- SQL Server / Redshift / Snowflake: DATEADD preserved (native) ---

test_that("DATEADD is preserved for sql server (native)", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "sql server")
  expect_true(grepl("DATEADD", sql))
  expect_equal(sql, "DATEADD(day, 30, start_date)")
})

test_that("DATEADD is preserved for redshift (native)", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "redshift")
  expect_equal(sql, "DATEADD(day, 30, start_date)")
})

test_that("DATEADD is preserved for snowflake (native)", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "snowflake")
  expect_equal(sql, "DATEADD(day, 30, start_date)")
})

# ===========================================================================
# TOP/LIMIT Translation Tests — one per dialect
# ===========================================================================

# --- SQL Server: TOP preserved (native) ---

test_that("TOP is preserved for sql server", {
  sql <- .sql_translate("SELECT TOP 10 * FROM person", "sql server")
  expect_true(grepl("TOP 10", sql))
  expect_false(grepl("LIMIT", sql))
})

# --- Oracle: FETCH FIRST N ROWS ONLY ---

test_that("TOP converts to FETCH FIRST for oracle", {
  sql <- .sql_translate("SELECT TOP 10 * FROM person", "oracle")
  expect_true(grepl("FETCH FIRST 10 ROWS ONLY", sql))
  expect_false(grepl("TOP", sql))
  expect_false(grepl("LIMIT", sql))
})

# --- PostgreSQL: LIMIT N ---

test_that("TOP converts to LIMIT for postgresql", {
  sql <- .sql_translate("SELECT TOP 10 * FROM person", "postgresql")
  expect_true(grepl("LIMIT 10", sql))
  expect_false(grepl("TOP", sql))
})

# --- MySQL: LIMIT N ---

test_that("TOP converts to LIMIT for mysql", {
  sql <- .sql_translate("SELECT TOP 10 * FROM person", "mysql")
  expect_true(grepl("LIMIT 10", sql))
  expect_false(grepl("TOP", sql))
})

# --- SQLite: LIMIT N ---

test_that("TOP converts to LIMIT for sqlite", {
  sql <- .sql_translate("SELECT TOP 5 * FROM observation", "sqlite")
  expect_true(grepl("LIMIT 5", sql))
  expect_false(grepl("TOP", sql))
})

# --- BigQuery: LIMIT N ---

test_that("TOP converts to LIMIT for bigquery", {
  sql <- .sql_translate("SELECT TOP 100 * FROM person", "bigquery")
  expect_true(grepl("LIMIT 100", sql))
  expect_false(grepl("TOP", sql))
})

# --- Redshift: LIMIT N ---

test_that("TOP converts to LIMIT for redshift", {
  sql <- .sql_translate("SELECT TOP 50 * FROM person", "redshift")
  expect_true(grepl("LIMIT 50", sql))
  expect_false(grepl("TOP", sql))
})

# --- Snowflake: LIMIT N ---

test_that("TOP converts to LIMIT for snowflake", {
  sql <- .sql_translate("SELECT TOP 20 * FROM person", "snowflake")
  expect_true(grepl("LIMIT 20", sql))
  expect_false(grepl("TOP", sql))
})

# --- Spark: LIMIT N ---

test_that("TOP converts to LIMIT for spark", {
  sql <- .sql_translate("SELECT TOP 10 * FROM person", "spark")
  expect_true(grepl("LIMIT 10", sql))
  expect_false(grepl("TOP", sql))
})

# ===========================================================================
# Combined Translation Tests (DATEADD + TOP in same query)
# ===========================================================================

test_that("combined DATEADD + TOP translates correctly for postgresql", {
  sql <- .sql_translate(
    "SELECT TOP 10 * FROM observation WHERE obs_date > DATEADD(day, -30, GETDATE())",
    "postgresql"
  )
  expect_true(grepl("LIMIT 10", sql))
  expect_true(grepl("INTERVAL '1 day'", sql))
  expect_false(grepl("TOP", sql))
  expect_false(grepl("DATEADD", sql))
})

test_that("combined DATEADD + TOP translates correctly for oracle", {
  sql <- .sql_translate(
    "SELECT TOP 5 * FROM drug_exposure WHERE start_date > DATEADD(day, -90, end_date)",
    "oracle"
  )
  expect_true(grepl("FETCH FIRST 5 ROWS ONLY", sql))
  expect_true(grepl("end_date \\+ -90", sql))
  expect_false(grepl("TOP", sql))
  expect_false(grepl("DATEADD", sql))
})

test_that("combined DATEADD + TOP translates correctly for mysql", {
  sql <- .sql_translate(
    "SELECT TOP 10 * FROM person WHERE birth_date > DATEADD(day, 365, start_date)",
    "mysql"
  )
  expect_true(grepl("LIMIT 10", sql))
  expect_true(grepl("DATE_ADD.*INTERVAL 365 DAY", sql))
  expect_false(grepl("TOP", sql))
  expect_false(grepl("DATEADD", sql))
})

# ===========================================================================
# No-op / passthrough tests
# ===========================================================================

test_that("SQL without DATEADD or TOP passes through unchanged", {
  original <- "SELECT person_id, gender_concept_id FROM person WHERE year_of_birth > 1990"
  for (dialect in c("postgresql", "mysql", "oracle", "sqlite", "bigquery", "spark", "sql server")) {
    expect_equal(.sql_translate(original, dialect), original)
  }
})

test_that("NULL or empty dialect returns SQL unchanged", {
  sql <- "SELECT TOP 10 * FROM person"
  expect_equal(.sql_translate(sql, NULL), sql)
  expect_equal(.sql_translate(sql, ""), sql)
})

# ===========================================================================
# Parameter Substitution Tests (.sql_render)
# ===========================================================================

test_that(".sql_render substitutes parameters correctly", {
  sql <- .sql_render("SELECT * FROM @schema.@table WHERE id = @id",
                     schema = "cdm", table = "person", id = "42")
  expect_equal(sql, "SELECT * FROM cdm.person WHERE id = 42")
})

test_that(".sql_render handles longest-first substitution", {
  # @schema_name should not be partially replaced by a shorter @schema param
  sql <- .sql_render("SELECT * FROM @schema_name.person",
                     schema_name = "my_cdm", schema = "WRONG")
  expect_equal(sql, "SELECT * FROM my_cdm.person")
})

# ===========================================================================
# Statement Splitting Tests (.sql_split)
# ===========================================================================

test_that(".sql_split splits on semicolons outside quotes", {
  stmts <- .sql_split("SELECT 1; SELECT 2; SELECT 3")
  expect_equal(length(stmts), 3L)
  expect_equal(stmts[1], "SELECT 1")
})

test_that(".sql_split preserves semicolons inside single quotes", {
  stmts <- .sql_split("SELECT 'hello; world' FROM t; SELECT 2")
  expect_equal(length(stmts), 2L)
  expect_true(grepl("hello; world", stmts[1]))
})

# ===========================================================================
# Dialect Mapping Completeness
# ===========================================================================

test_that("all DBMS in resource.js enum are mapped in dialect resolver", {
  dbms_list <- c("postgresql", "sql_server", "oracle", "redshift",
                 "bigquery", "snowflake", "spark", "databricks",
                 "duckdb", "sqlite")
  for (dbms in dbms_list) {
    expect_silent(.resolve_target_dialect(dbms))
  }
})

test_that("dialect aliases resolve correctly", {
  expect_equal(.resolve_target_dialect("postgres"), "postgresql")
  expect_equal(.resolve_target_dialect("sqlserver"), "sql server")
  expect_equal(.resolve_target_dialect("duckdb"), "sqlite")
  expect_equal(.resolve_target_dialect("databricks"), "spark")
  expect_equal(.resolve_target_dialect("mysql"), "mysql")
  expect_equal(.resolve_target_dialect("mariadb"), "mysql")
})

test_that("unsupported DBMS throws error", {
  expect_error(.resolve_target_dialect("mongodb"), "Unsupported DBMS")
  expect_error(.resolve_target_dialect("cassandra"), "Unsupported DBMS")
})
