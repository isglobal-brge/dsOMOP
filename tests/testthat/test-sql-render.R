# --- Dialect mapping tests ---

test_that(".resolve_target_dialect maps supported DBMS correctly", {
  expect_equal(.resolve_target_dialect("postgresql"), "postgresql")
  expect_equal(.resolve_target_dialect("postgres"), "postgresql")
  expect_equal(.resolve_target_dialect("sql_server"), "sql server")
  expect_equal(.resolve_target_dialect("sqlserver"), "sql server")
  expect_equal(.resolve_target_dialect("oracle"), "oracle")
  expect_equal(.resolve_target_dialect("redshift"), "redshift")
  expect_equal(.resolve_target_dialect("bigquery"), "bigquery")
  expect_equal(.resolve_target_dialect("snowflake"), "snowflake")
  expect_equal(.resolve_target_dialect("spark"), "spark")
  expect_equal(.resolve_target_dialect("sqlite"), "sqlite")
  expect_equal(.resolve_target_dialect("duckdb"), "sqlite")
})

test_that(".resolve_target_dialect maps mysql and mariadb", {
  expect_equal(.resolve_target_dialect("mysql"), "mysql")
  expect_equal(.resolve_target_dialect("mariadb"), "mysql")
})

test_that(".resolve_target_dialect rejects unsupported DBMS", {
  expect_error(.resolve_target_dialect("access"), "Unsupported DBMS")
})

test_that(".resolve_target_dialect is case insensitive", {
  expect_equal(.resolve_target_dialect("PostgreSQL"), "postgresql")
  expect_equal(.resolve_target_dialect("SQLITE"), "sqlite")
  expect_equal(.resolve_target_dialect("SQL_SERVER"), "sql server")
})

# --- .sql_render tests ---

test_that(".sql_render substitutes @param placeholders", {
  expect_equal(
    .sql_render("SELECT * FROM @table WHERE id = @id", table = "person", id = 42),
    "SELECT * FROM person WHERE id = 42"
  )
})

test_that(".sql_render handles longest-first ordering to avoid substring collision", {
  result <- .sql_render("@schema_name.@schema", schema_name = "cdm", schema = "public")
  expect_equal(result, "cdm.public")
})

test_that(".sql_render returns sql unchanged when no params given", {
  expect_equal(.sql_render("SELECT 1"), "SELECT 1")
})

# --- .sql_translate TOP -> LIMIT tests ---

test_that(".sql_translate converts TOP to LIMIT for postgresql", {
  sql <- .sql_translate("SELECT TOP 10 * FROM person", "postgresql")
  expect_true(grepl("LIMIT 10", sql))
  expect_false(grepl("TOP", sql))
})

test_that(".sql_translate converts TOP to LIMIT for sqlite", {
  sql <- .sql_translate("SELECT TOP 5 a, b FROM t1 WHERE x = 1", "sqlite")
  expect_true(grepl("LIMIT 5", sql))
  expect_false(grepl("TOP", sql))
  expect_true(grepl("WHERE x = 1", sql))
})

test_that(".sql_translate keeps TOP for sql server", {
  sql <- .sql_translate("SELECT TOP 10 * FROM person", "sql server")
  expect_true(grepl("TOP 10", sql))
  expect_false(grepl("LIMIT", sql))
})

test_that(".sql_translate handles TOP with ORDER BY", {
  sql <- .sql_translate(
    "SELECT TOP 100 value, COUNT(*) AS n FROM t1 GROUP BY value ORDER BY COUNT(*) DESC",
    "postgresql"
  )
  expect_true(grepl("LIMIT 100", sql))
  expect_false(grepl("TOP", sql))
  expect_true(grepl("ORDER BY", sql))
})

# --- .sql_translate DATEADD tests ---

test_that(".sql_translate converts DATEADD for postgresql", {
  sql <- .sql_translate("DATEADD(day, 30, c.cohort_start_date)", "postgresql")
  expect_true(grepl("INTERVAL '1 day'", sql))
  expect_true(grepl("30", sql))
  expect_false(grepl("DATEADD", sql))
})

test_that(".sql_translate converts DATEADD with negative days for postgresql", {
  sql <- .sql_translate("DATEADD(day, -90, c.cohort_start_date)", "postgresql")
  expect_true(grepl("-90", sql))
  expect_true(grepl("INTERVAL", sql))
})

test_that(".sql_translate converts DATEADD for sqlite", {
  sql <- .sql_translate("DATEADD(day, 7, start_date)", "sqlite")
  expect_true(grepl("DATE", sql))
  expect_true(grepl("7 days", sql))
  expect_false(grepl("DATEADD", sql))
})

test_that(".sql_translate converts DATEADD for bigquery", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "bigquery")
  expect_true(grepl("DATE_ADD", sql))
  expect_true(grepl("INTERVAL 30 DAY", sql))
})

test_that(".sql_translate converts DATEADD for spark", {
  sql <- .sql_translate("DATEADD(day, 5, start_date)", "spark")
  expect_equal(sql, "DATE_ADD(start_date, 5)")
})

test_that(".sql_translate converts DATEADD for oracle", {
  sql <- .sql_translate("DATEADD(day, 10, start_date)", "oracle")
  expect_equal(sql, "(start_date + 10)")
})

test_that(".sql_translate keeps DATEADD for sql server", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "sql server")
  expect_true(grepl("DATEADD", sql))
})

test_that(".sql_translate keeps DATEADD for redshift (native support)", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "redshift")
  expect_true(grepl("DATEADD", sql))
})

test_that(".sql_translate keeps DATEADD for snowflake (native support)", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "snowflake")
  expect_true(grepl("DATEADD", sql))
})

# --- .sql_split tests ---

test_that(".sql_split splits on semicolons", {
  stmts <- .sql_split("SELECT 1; SELECT 2; SELECT 3")
  expect_equal(length(stmts), 3)
  expect_equal(stmts[1], "SELECT 1")
  expect_equal(stmts[2], "SELECT 2")
  expect_equal(stmts[3], "SELECT 3")
})

test_that(".sql_split handles trailing semicolons and whitespace", {
  stmts <- .sql_split("  SELECT 1 ;  SELECT 2 ;  ")
  expect_equal(length(stmts), 2)
  expect_equal(stmts[1], "SELECT 1")
  expect_equal(stmts[2], "SELECT 2")
})

test_that(".sql_split does not split on semicolons inside quotes", {
  stmts <- .sql_split("SELECT 'a;b' FROM t1; SELECT 2")
  expect_equal(length(stmts), 2)
  expect_true(grepl("a;b", stmts[1]))
})

test_that(".sql_split handles single statement without semicolon", {
  stmts <- .sql_split("SELECT 1")
  expect_equal(length(stmts), 1)
  expect_equal(stmts[1], "SELECT 1")
})

# --- .renderSql integration tests ---

test_that(".renderSql produces valid translated SQL for sqlite", {
  handle <- new.env(parent = emptyenv())
  handle$target_dialect <- "sqlite"

  sql <- .renderSql(handle, "SELECT TOP 10 * FROM person")
  expect_true(grepl("LIMIT 10", sql))
  expect_false(grepl("TOP", sql))
})

test_that(".renderSql produces valid translated SQL for postgresql", {
  handle <- new.env(parent = emptyenv())
  handle$target_dialect <- "postgresql"

  sql <- .renderSql(handle, "SELECT TOP 5 * FROM person")
  expect_true(grepl("LIMIT 5", sql))
})

test_that(".renderSql handles parameter substitution", {
  handle <- new.env(parent = emptyenv())
  handle$target_dialect <- "sqlite"

  sql <- .renderSql(handle, "SELECT * FROM @table WHERE id = @id",
                    table = "person", id = 42)
  expect_true(grepl("person", sql))
  expect_true(grepl("42", sql))
})

test_that(".renderSql translates DATEADD with param substitution", {
  handle <- new.env(parent = emptyenv())
  handle$target_dialect <- "postgresql"

  sql <- .renderSql(handle, "DATEADD(day, @days, c.cohort_start_date)",
                    days = 30)
  expect_true(grepl("INTERVAL", sql))
  expect_true(grepl("30", sql))
  expect_false(grepl("DATEADD", sql))
  expect_false(grepl("@days", sql))
})

# --- Existing utility tests ---

test_that(".qualifyTable works with and without schema", {
  handle <- new.env(parent = emptyenv())
  handle$target_dialect <- "sqlite"
  handle$cdm_schema <- NULL

  # SQLite: no schema qualification
  expect_equal(.qualifyTable(handle, "person"), "person")

  # With schema on non-sqlite
  handle$target_dialect <- "postgresql"
  handle$cdm_schema <- "cdm"
  expect_equal(.qualifyTable(handle, "person"), "cdm.person")

  # Explicit schema overrides default
  expect_equal(.qualifyTable(handle, "person", schema = "vocab"), "vocab.person")

  # NULL schema on non-sqlite
  handle$cdm_schema <- NULL
  expect_equal(.qualifyTable(handle, "person"), "person")
})

test_that(".quoteLiteral handles strings and numbers", {
  expect_equal(.quoteLiteral(42), "42")
  expect_equal(.quoteLiteral(3.14), "3.14")
  expect_equal(.quoteLiteral("hello"), "'hello'")
  expect_equal(.quoteLiteral("it's"), "'it''s'")
})
