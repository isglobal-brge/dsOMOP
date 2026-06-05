# --- Readable Resource URL Parsing Tests (.parseOmopUrl) ---

test_that(".parseOmopUrl parses a full server URL", {
  p <- .parseOmopUrl(
    "omop+dbi:postgresql://db.example.org:5432/omop?cdm_schema=cdm&vocabulary_schema=vocab")
  expect_equal(p$dbms, "postgresql")
  expect_equal(p$host, "db.example.org")
  expect_equal(p$port, 5432L)
  expect_equal(p$database, "omop")
  expect_equal(p$cdm_schema, "cdm")
  expect_equal(p$vocabulary_schema, "vocab")
})

test_that(".parseOmopUrl accepts a bare scheme (no omop+dbi wrapper)", {
  p <- .parseOmopUrl("postgresql://omopdb:5432/omop?cdm_schema=cdm")
  expect_equal(p$dbms, "postgresql")
  expect_equal(p$host, "omopdb")
  expect_equal(p$port, 5432L)
  expect_equal(p$cdm_schema, "cdm")
})

test_that(".parseOmopUrl treats the port as optional", {
  p <- .parseOmopUrl("omop+dbi:postgresql://localhost/omop")
  expect_equal(p$host, "localhost")
  expect_null(p$port)
  expect_equal(p$database, "omop")
})

test_that(".parseOmopUrl normalizes SQL Server spellings to 'sqlserver'", {
  expect_equal(.parseOmopUrl("omop+dbi:sql_server://h:1433/db")$dbms, "sqlserver")
  expect_equal(.parseOmopUrl("omop+dbi:mssql://h:1433/db")$dbms, "sqlserver")
  expect_equal(.normalizeDBMS("SQL Server"), "sqlserver")
  expect_equal(.normalizeDBMS("PostgreSQL"), "postgresql")
  expect_equal(.normalizeDBMS("MariaDB"), "mariadb")
  expect_equal(.normalizeDBMS("postgres"), "postgresql")
})

test_that(".parseOmopUrl handles a file URL (empty authority, absolute path)", {
  p <- .parseOmopUrl("omop+dbi:sqlite:///srv/data/omop.sqlite")
  expect_equal(p$dbms, "sqlite")
  expect_null(p$host)
  expect_null(p$port)
  expect_equal(p$database, "/srv/data/omop.sqlite")
})

test_that(".parseOmopUrl percent-decodes path and query values", {
  p <- .parseOmopUrl("omop+dbi:postgresql://h:5432/my%20db?cdm_schema=odd%2Fschema")
  expect_equal(p$database, "my db")
  expect_equal(p$cdm_schema, "odd/schema")
})

test_that(".parseOmopUrl rejects the retired base64 format", {
  expect_error(.parseOmopUrl("omop+dbi:///B64:eyJhIjoxfQ"), "base64")
})

test_that(".parseOmopUrl errors on a malformed URL", {
  expect_error(.parseOmopUrl("not-a-url"), "Malformed")
  expect_error(.parseOmopUrl(""), "empty")
})

# --- Per-DBMS Default Schema (.dbmsDefaultSchema) ---

test_that(".dbmsDefaultSchema returns the correct default for every DBMS", {
  expect_equal(.dbmsDefaultSchema("postgresql"), "public")
  expect_equal(.dbmsDefaultSchema("redshift"),   "public")
  expect_equal(.dbmsDefaultSchema("sql_server"), "dbo")
  expect_equal(.dbmsDefaultSchema("synapse"),    "dbo")
  expect_equal(.dbmsDefaultSchema("pdw"),        "dbo")
  expect_equal(.dbmsDefaultSchema("mysql",   database = "mydb"), "mydb")
  expect_equal(.dbmsDefaultSchema("mariadb", database = "mydb"), "mydb")
  expect_equal(.dbmsDefaultSchema("bigquery", database = "ds"),  "ds")
  expect_equal(.dbmsDefaultSchema("oracle",  user = "scott"),    "SCOTT")
  expect_equal(.dbmsDefaultSchema("sqlite"),     "main")
  expect_equal(.dbmsDefaultSchema("duckdb"),     "main")
  expect_equal(.dbmsDefaultSchema("snowflake"),  "PUBLIC")
  expect_equal(.dbmsDefaultSchema("spark"),      "default")
  expect_equal(.dbmsDefaultSchema("databricks"), "default")
})

test_that(".dbmsDefaultSchema returns NULL when the schema cannot be inferred", {
  expect_null(.dbmsDefaultSchema("mysql"))   # database doubles as schema, none given
  expect_null(.dbmsDefaultSchema("oracle"))  # schema is the user, none given
})

# --- Four-case CDM/vocabulary schema resolution (.createHandle) ---

# A minimal stand-in for OMOPResourceClient: .createHandle only reads the parsed
# URL, the resource identity, and stores the (here unused) connection.
fake_client <- function(parsed, identity = NULL) {
  list(
    getConnection = function() NULL,
    getParsed     = function() parsed,
    getResource   = function() list(identity = identity)
  )
}

test_that("schema resolution case 1: neither set -> both the DBMS default", {
  h <- .createHandle(fake_client(.parseOmopUrl("omop+dbi:postgresql://h:5432/omop")))
  expect_equal(h$cdm_schema, "public")
  expect_equal(h$vocab_schema, "public")
})

test_that("schema resolution case 2: only CDM set -> both that schema", {
  h <- .createHandle(fake_client(
    .parseOmopUrl("omop+dbi:postgresql://h:5432/omop?cdm_schema=cdm")))
  expect_equal(h$cdm_schema, "cdm")
  expect_equal(h$vocab_schema, "cdm")
})

test_that("schema resolution case 3: only vocabulary set -> CDM default, vocab apart", {
  h <- .createHandle(fake_client(
    .parseOmopUrl("omop+dbi:postgresql://h:5432/omop?vocabulary_schema=vocab")))
  expect_equal(h$cdm_schema, "public")
  expect_equal(h$vocab_schema, "vocab")
})

test_that("schema resolution case 4: both set -> one each", {
  h <- .createHandle(fake_client(
    .parseOmopUrl("omop+dbi:postgresql://h:5432/omop?cdm_schema=cdm&vocabulary_schema=vocab")))
  expect_equal(h$cdm_schema, "cdm")
  expect_equal(h$vocab_schema, "vocab")
})

test_that("schema resolution uses the connecting user for Oracle's default", {
  h <- .createHandle(fake_client(
    .parseOmopUrl("omop+dbi:oracle://h:1521/ORCL"), identity = "scott"))
  expect_equal(h$cdm_schema, "SCOTT")
  expect_equal(h$vocab_schema, "SCOTT")
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
  dbms_list <- c("postgresql", "sqlite", "mysql", "mariadb",
                 "sql_server", "synapse", "pdw",
                 "oracle", "redshift",
                 "bigquery", "snowflake", "spark", "databricks",
                 "duckdb")
  for (dbms in dbms_list) {
    expect_silent(.resolve_target_dialect(dbms))
  }
})

test_that("dialect aliases resolve correctly", {
  expect_equal(.resolve_target_dialect("postgres"), "postgresql")
  expect_equal(.resolve_target_dialect("sqlserver"), "sql server")
  expect_equal(.resolve_target_dialect("synapse"), "sql server")
  expect_equal(.resolve_target_dialect("pdw"), "sql server")
  expect_equal(.resolve_target_dialect("duckdb"), "sqlite")
  expect_equal(.resolve_target_dialect("databricks"), "spark")
  expect_equal(.resolve_target_dialect("mysql"), "mysql")
  expect_equal(.resolve_target_dialect("mariadb"), "mysql")
})

test_that("unsupported DBMS throws error", {
  expect_error(.resolve_target_dialect("mongodb"), "Unsupported DBMS")
  expect_error(.resolve_target_dialect("cassandra"), "Unsupported DBMS")
})
