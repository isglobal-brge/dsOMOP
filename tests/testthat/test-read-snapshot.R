# Requests pin the connection and roll back on success as well as failure.
test_that("snapshot rollback and connection-loss are request-wide", {
  h <- create_test_handle(n_persons = 3)
  on.exit(cleanup_handle(h), add = TRUE)
  before <- DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM person")$n
  .dsomopWithReadSnapshot(h, {
    DBI::dbExecute(h$conn, "DELETE FROM person")
    expect_equal(.executeQuery(h, "SELECT COUNT(*) AS n FROM person")$n, 0)
  })
  expect_identical(DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM person")$n, before)
  expect_null(h$read_snapshot)
  expect_error(.dsomopWithReadSnapshot(h, {
    DBI::dbExecute(h$conn, "DELETE FROM person")
    stop("request failed")
  }), "request failed")
  expect_identical(DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM person")$n, before)
  expect_error(suppressMessages(.dsomopWithReadSnapshot(h, {
    tryCatch(.dsomopSnapshotGuard(h, "COMMIT"), error = function(e) NULL)
    1
  })), "read snapshot was lost")
  valid <- TRUE
  original <- .dbIsValid
  local_mocked_bindings(.dbIsValid = function(conn) if (valid) original(conn) else FALSE)
  expect_error(suppressMessages(.dsomopWithReadSnapshot(h, {
    .executeQuery(h, "SELECT COUNT(*) AS n FROM person")
    valid <- FALSE
    tryCatch(.executeQuery(h, "SELECT COUNT(*) AS n FROM person"), error = function(e) NULL)
    1
  })), "read snapshot was lost")
  valid <- TRUE
  expect_null(h$read_snapshot)
})

for (engine in c("postgresql", "mariadb", "mysql")) local({
  dbms <- engine
  test_that(paste(dbms, "membership digest matches bounded streaming and snapshots resist writes"), {
    config <- .dsomopVendorTestConfig(dbms)
    skip_if(is.null(config), "Vendor host not configured")
    fixture <- .dsomopVendorCreateFixture(config)
    on.exit(fixture$cleanup(), add = TRUE)
    h <- fixture$handle
    table <- .qualifyTable(h, "person")
    before <- .dsomopWithReadSnapshot(h, {
      a <- .dsomopDpMembershipHash(h, "database")
      b <- .dsomopDpMembershipHash(h, "stream", page = 1L)
      expect_identical(a[c("n", "digest")], b[c("n", "digest")])
      # The other connection commits between support/generation and values.
      DBI::dbExecute(fixture$admin, paste0("UPDATE ", table,
        " SET year_of_birth = 2010 WHERE person_id = 1"))
      current <- .executeQuery(h, paste0("SELECT year_of_birth FROM ", table,
                                        " WHERE person_id = 1"))[[1L]]
      expect_false(identical(as.integer(current), 2010L))
      .createTempTable(h, "dsomop_snapshot_test", paste0(
        "SELECT person_id, year_of_birth FROM ", table))
      copied <- .executeQuery(h, paste0("SELECT year_of_birth FROM dsomop_snapshot_test ",
                                        "WHERE person_id = 1"))[[1L]]
      expect_identical(as.integer(copied), as.integer(current))
      .dropTempTable(h, "dsomop_snapshot_test")
      a
    })
    expect_equal(as.integer(.executeQuery(h, paste0("SELECT year_of_birth FROM ",
      table, " WHERE person_id = 1"))[[1L]]), 2010L)
    expect_null(h$read_snapshot)
    # Exact signed 64-bit IDs, duplicate memberships and bucket boundaries.
    DBI::dbExecute(fixture$admin, paste0("UPDATE ", table,
      " SET person_id = -9223372036854775807 WHERE person_id = 1"))
    DBI::dbExecute(fixture$admin, paste0("UPDATE ", table,
      " SET person_id = 9223372036854775807 WHERE person_id = 2"))
    DBI::dbExecute(fixture$admin, paste0("INSERT INTO ", table,
      " SELECT * FROM ", table, " WHERE person_id = 3"))
    .dsomopWithReadSnapshot(h, {
      a <- .dsomopDpMembershipHash(h, "database")
      b <- .dsomopDpMembershipHash(h, "stream", page = 1L)
      expect_identical(a[c("n", "digest")], b[c("n", "digest")])
      expect_false(identical(before$digest, a$digest))
    })
  })
})

test_that("DuckDB membership and read snapshot are consistent", {
  skip_if_not_installed("duckdb")
  path <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE person AS SELECT i::BIGINT AS person_id FROM range(-20001, 20002) t(i)")
  h <- new.env(parent = emptyenv())
  h$conn <- con
  h$dbms <- h$target_dialect <- "duckdb"
  h$cdm_schema <- "main"
  h$temp_tables <- character()
  .dsomopWithReadSnapshot(h, {
    a <- .dsomopDpMembershipHash(h, "database")
    b <- .dsomopDpMembershipHash(h, "stream", page = 777L)
    expect_identical(a[c("n", "digest")], b[c("n", "digest")])
  })
  expect_null(h$read_snapshot)
})

test_that("option-free native adapters use serializable or native transactions", {
  statements <- character()
  testthat::local_mocked_bindings(
    dbBegin = function(conn, ...) { statements <<- c(statements, "BEGIN"); TRUE },
    dbRollback = function(conn, ...) { statements <<- c(statements, "ROLLBACK"); TRUE },
    dbExecute = function(conn, statement, ...) { statements <<- c(statements, statement); 0L },
    dbGetQuery = function(conn, statement, ...) data.frame(isolation = 2L),
    .package = "DBI")
  local_mocked_bindings(.dbIsValid = function(conn) TRUE)
  for (dbms in c("sqlserver", "oracle", "redshift")) {
    statements <- character()
    h <- new.env(parent = emptyenv()); h$dbms <- dbms
    ctx <- new.env(parent = emptyenv())
    ctx$conn <- "structural-test"; ctx$dbms <- dbms
    ctx$kind <- .dsomopReadSnapshotKind(h); ctx$aborted <- FALSE
    .dsomopSnapshotBegin(ctx)
    .dsomopSnapshotValidate(ctx)
    .dsomopSnapshotRollback(ctx)
    expect_identical(statements[[1L]], "BEGIN")
    expect_identical(tail(statements, 1L), "ROLLBACK")
    expect_false(any(grepl("ALTER DATABASE|ALLOW_SNAPSHOT|COMMIT", statements)))
    if (dbms != "redshift") expect_true("SET TRANSACTION ISOLATION LEVEL SERIALIZABLE" %in% statements)
    h$read_snapshot <- ctx
    dangerous <- if (dbms == "oracle") "CREATE TABLE x (i INTEGER)" else "COMMIT"
    expect_error(suppressMessages(.dsomopSnapshotGuard(h, dangerous)), "read snapshot was lost")
    expect_true(ctx$aborted)
  }
})

for (engine in c("mysql", "mariadb")) local({
  dbms <- engine
  test_that(paste(dbms, "cannot claim a snapshot over a nontransactional source"), {
    config <- .dsomopVendorTestConfig(dbms)
    skip_if(is.null(config), "Vendor host not configured")
    fixture <- .dsomopVendorCreateFixture(config)
    on.exit(fixture$cleanup(), add = TRUE)
    DBI::dbExecute(fixture$admin, paste0("ALTER TABLE ",
      .qualifyTable(fixture$handle, "person"), " ENGINE = MyISAM"))
    expect_error(suppressMessages(.dsomopWithReadSnapshot(fixture$handle,
      .executeQuery(fixture$handle, paste("SELECT COUNT(*) AS n FROM",
        .qualifyTable(fixture$handle, "person"))))),
                  "nontransactional tables")
    expect_null(fixture$handle$read_snapshot)
    # The engine can also change after entry but before the first read.
    DBI::dbExecute(fixture$admin, paste0("ALTER TABLE ",
      .qualifyTable(fixture$handle, "person"), " ENGINE = InnoDB"))
    expect_error(suppressMessages(.dsomopWithReadSnapshot(fixture$handle, {
      DBI::dbExecute(fixture$admin, paste0("ALTER TABLE ",
        .qualifyTable(fixture$handle, "person"), " ENGINE = MyISAM"))
      tryCatch(.executeQuery(fixture$handle, paste0("SELECT COUNT(*) AS n FROM ",
        .qualifyTable(fixture$handle, "person"))), error = function(e) NULL)
      1
    })), "nontransactional tables")
    expect_null(fixture$handle$read_snapshot)
  })
})
