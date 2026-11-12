# Live vendor execution of every typed SQL helper (R/sql_expr.R; design-2.10.3
# sections 9 and 10.5). Opt-in like test-vendor-db-integration.R: set
# DSOMOP_TEST_POSTGRES_HOST / DSOMOP_TEST_MYSQL_HOST / DSOMOP_TEST_MARIADB_HOST
# (plus the _PORT, _DATABASE, _ADMIN_USER, _ADMIN_PASSWORD variables). The
# vendor CI workflow runs this file through its "vendor-db-integration"
# filter against PostgreSQL 16, MySQL 8.4 and MariaDB 11.4.
#
# The checks need only a session: they create one TEMPORARY table, which is
# dropped at the end and disappears with the session, and no persistent
# object or role, so any account with the TEMPORARY privilege can run them.

.ws3_vendor_operations <- function() names(.SQL_OPERATION_CONSTRUCTS)

.ws3_run_vendor_sql_expr <- function(dbms) {
  config <- .dsomopVendorTestConfig(dbms)
  testthat::skip_if(is.null(config), paste0(
    "Set DSOMOP_TEST_", toupper(if (identical(dbms, "postgresql")) {
      "POSTGRES"
    } else {
      dbms
    }), "_HOST to run the typed SQL helpers on a live server."
  ))
  conn <- .dsomopVendorAdminConnection(config)
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)
  if (identical(dbms, "postgresql")) {
    .ensurePostgresUtc(conn)
  } else {
    .ensureMariaSession(conn, require_tls = FALSE)
  }
  family <- .sqlFamily(dbms)
  handle <- .sqlExprHandle(conn, dbms, .resolve_target_dialect(dbms))
  profile <- .sqlDialectPreflight(
    handle, .sqlPlanConstructs(.ws3_vendor_operations()),
    session_tables = TRUE
  )
  expect_identical(profile$family, family)
  expect_true(isTRUE(profile$probed))
  expect_false(is.null(profile$runtime))
  if (identical(dbms, "postgresql")) {
    expect_match(profile$runtime, "^PostgreSQL [0-9]+")
    expect_true(profile$sha256_sql)
  } else {
    expect_match(profile$runtime,
                 if (identical(dbms, "mariadb")) "^MariaDB [0-9]+" else
                   "^(MySQL|MariaDB) [0-9]+")
    expect_true(profile$analytic_baseline)
  }
  cap <- .sqlDialectCapability(handle)
  expect_identical(cap$family, family)
  expect_identical(cap$temporary_materialization, "session_table")
  expect_true(cap$window_frames)
  label <- switch(dbms, postgresql = "PostgreSQL", mysql = "MySQL",
                  mariadb = "MariaDB")
  n <- .sqlExprRunChecks(conn, family, label)
  expect_gt(n, 35L)

  # The published matching vectors hash identically in SQL and in R.
  rows <- .sqlExprLoadCases(conn, family)
  on.exit(try(DBI::dbExecute(conn, paste0("DROP TABLE IF EXISTS ",
                                          .sqlExprCaseTable)), silent = TRUE),
          add = TRUE, after = FALSE)
  vectors <- .sqlExprMatchVectors()
  got <- DBI::dbGetQuery(conn, paste0(
    "SELECT ", .sqlSha256Hex(family, "c.hx", input = "hex"), " AS h FROM ",
    .sqlExprCaseTable, " c WHERE c.id <= 3 ORDER BY c.id"))$h
  expect_identical(as.character(got),
                   vapply(vectors, function(v) v$sha256, character(1)))
  .sqlExprRecordRuntime(profile, label, n)
}

test_that("typed SQL helpers execute on live PostgreSQL", {
  .ws3_run_vendor_sql_expr("postgresql")
})

test_that("typed SQL helpers execute on live MySQL", {
  .ws3_run_vendor_sql_expr("mysql")
})

test_that("typed SQL helpers execute on live MariaDB", {
  .ws3_run_vendor_sql_expr("mariadb")
})
