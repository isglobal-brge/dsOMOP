test_that("OHDSI read-only lexer ignores protected SQL text", {
  statement <- paste(
    "-- DELETE FROM person",
    "WITH \"UPDATE\" AS (",
    "  SELECT 'INSERT INTO secret' AS [DROP],",
    "         $$COPY person TO '/tmp/x'$$ AS `ALTER`",
    "  /* MERGE, CREATE, and TRUNCATE are comment text */",
    ") SELECT * FROM \"UPDATE\"",
    sep = "\n"
  )

  expect_silent(.assertOhdsiReadOnlySql(statement))
  expect_silent(.assertOhdsiReadOnlySql(
    "SELECT 'it''s a DELETE token' AS \"COPY\""
  ))
  expect_silent(.assertOhdsiReadOnlySql(paste(
    "SELECT 'pg_terminate_backend(1)' AS text_value",
    "/* dblink_exec('connection', 'DELETE FROM person') */"
  )))
})

test_that("OHDSI and QueryLibrary scalar/window calls remain available", {
  statement <- paste(
    "WITH age_age_grp(age, age_grp) AS (SELECT 1, '0 to 9')",
    "SELECT COUNT(*), AVG(age), STDEV(age),",
    "       LOWER(REPLACE(age_grp, 'to', '-')),",
    "       ROW_NUMBER() OVER (ORDER BY age),",
    "       DATEADD(day, 1, DATEFROMPARTS(2020, 1, 1))",
    "FROM age_age_grp"
  )

  expect_silent(.assertOhdsiReadOnlySql(statement))
})

test_that("OHDSI read-only lexer rejects mutating and transfer statements", {
  unsafe <- c(
    "WITH q AS (SELECT 1) DELETE FROM person",
    "WITH q AS (UPDATE person SET person_id = 2 RETURNING *) SELECT * FROM q",
    "WITH q AS (SELECT 1) CREATE TABLE copied AS SELECT * FROM q",
    "SELECT * INTO copied FROM person",
    "WITH q AS (SELECT * FROM person) COPY q TO '/tmp/q.csv'",
    "WITH q AS (DROP TABLE person) SELECT * FROM q"
  )

  for (statement in unsafe) {
    expect_error(
      .assertOhdsiReadOnlySql(statement),
      "DML, DDL, SELECT INTO or data-transfer",
      info = statement
    )
  }
  expect_error(
    .assertOhdsiReadOnlySql("WITH q AS (SELECT 1) VALUES (1)"),
    "top-level SELECT"
  )
})

test_that("side-effecting and external-access SELECT calls are rejected", {
  unsafe <- c(
    "SELECT pg_terminate_backend(1)",
    "SELECT pg_catalog.pg_cancel_backend(1)",
    "SELECT pg_catalog.\"pg_terminate_backend\"(1)",
    "SELECT pg_advisory_lock(1)",
    "SELECT set_config('search_path', 'public', false)",
    "SELECT pg_read_file('/etc/passwd')",
    "SELECT dblink_exec('remote', 'DELETE FROM person')",
    "SELECT GET_LOCK('dsomop', 10)",
    "SELECT LOAD_FILE('/etc/passwd')",
    "SELECT OPENROWSET(BULK 'secret.csv', SINGLE_CLOB)",
    "SELECT UTL_HTTP.REQUEST('https://example.invalid') FROM dual",
    "SELECT DBMS_LOCK.SLEEP(10) FROM dual",
    "SELECT load_extension('extension')",
    "SELECT read_parquet('/tmp/private.parquet')",
    "SELECT EXTERNAL_QUERY('connection', 'SELECT secret')",
    "SELECT SYSTEM$WAIT(10)",
    "SELECT reflect('java.lang.Runtime', 'getRuntime')"
  )

  for (statement in unsafe) {
    expect_error(
      .assertOhdsiReadOnlySql(statement),
      "side-effecting, dynamic, resource-control or external-access",
      info = statement
    )
  }
})

test_that("sequence, assignment and executable-comment bypasses fail closed", {
  expect_error(
    .assertOhdsiReadOnlySql("SELECT nextval('person_seq')"),
    "advance database sequences"
  )
  expect_error(
    .assertOhdsiReadOnlySql("SELECT person_seq.NEXTVAL FROM dual"),
    "advance database sequences"
  )
  expect_error(
    .assertOhdsiReadOnlySql("SELECT NEXT VALUE FOR person_seq"),
    "advance database sequences"
  )
  expect_error(
    .assertOhdsiReadOnlySql("SELECT @session_value := 1"),
    "assign session variables"
  )
  expect_error(
    .assertOhdsiReadOnlySql("SELECT 1 /*!50000, SLEEP(1) */"),
    "executable block comment"
  )
  expect_error(
    .assertOhdsiReadOnlySql("SELECT 1 /*M!100000, SLEEP(1) */"),
    "executable block comment"
  )
  expect_error(
    .assertOhdsiReadOnlySql(
      "SELECT 1 /* outer /* inner */ , SLEEP(1) -- */\n"
    ),
    "nested block comment"
  )
  expect_error(
    .assertOhdsiReadOnlySql(
      "SELECT 1, --SLEEP(1)\n 2", dbms = "mariadb"
    ),
    "dash comment"
  )
  expect_silent(.assertOhdsiReadOnlySql(
    "SELECT 1 --SLEEP(1) is comment text", dbms = "postgresql"
  ))
  expect_error(
    .assertOhdsiReadOnlySql("SELECT U&\"pg_\\0074erminate_backend\"(1)"),
    "Unicode-escaped identifier"
  )
})

test_that("WITH DELETE is rejected before it can mutate the database", {
  skip_if_not_installed("SqlRender")
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  before <- DBI::dbGetQuery(
    handle$conn, "SELECT COUNT(*) AS n FROM person"
  )$n[[1L]]
  expect_error(
    .queryOhdsiSql(
      handle,
      paste0(
        "WITH q AS (SELECT 1) DELETE FROM person ",
        "WHERE person_id = 1;"
      )
    ),
    "found 'DELETE'"
  )
  after <- DBI::dbGetQuery(
    handle$conn, "SELECT COUNT(*) AS n FROM person"
  )$n[[1L]]

  expect_identical(after, before)
  expect_error(
    .queryOhdsiSql(handle, "SELECT load_extension('not-real');"),
    "side-effecting, dynamic, resource-control or external-access"
  )
  safe <- .queryOhdsiSql(
    handle,
    paste0(
      "WITH q AS (SELECT person_id FROM person) ",
      "SELECT COUNT(*) AS n FROM q;"
    )
  )
  expect_identical(safe$n[[1L]], before)
})
