# R09: actual external-schema sources, with the normal restricted connection.
# Privilege changes below belong only to these disposable integration fixtures;
# production never grants privileges or requires an owner configuration step.
.snapshotViewFixture <- function(dbms, engine = "InnoDB", nested = FALSE) {
  config <- .dsomopVendorTestConfig(dbms)
  skip_if(is.null(config), "Vendor host not configured")
  f <- .dsomopVendorCreateFixture(config)
  prepared <- FALSE
  on.exit(if (!prepared) f$cleanup(), add = TRUE)
  f$external <- paste0(f$schemas[["cdm"]], "_external")
  f$declared <- .qualifyTable(f$handle, "person")
  f$physical <- paste0(f$external, ".person")
  original_cleanup <- f$cleanup
  f$cleanup <- function() {
    if (!is.null(f$handle$read_snapshot)) stop("Request snapshot leaked into cleanup.")
    try(DBI::dbExecute(f$admin, paste("DROP DATABASE IF EXISTS", f$external)), silent = TRUE)
    original_cleanup()
  }
  DBI::dbExecute(f$admin, paste("CREATE DATABASE", f$external))
  DBI::dbExecute(f$admin, paste0("CREATE TABLE ", f$physical,
    " ENGINE=", engine, " AS SELECT * FROM ", f$declared))
  DBI::dbExecute(f$admin, paste("DROP TABLE", f$declared))
  source <- f$physical
  if (nested) {
    source <- paste0(f$external, ".inner_person")
    DBI::dbExecute(f$admin, paste("CREATE SQL SECURITY DEFINER VIEW", source,
      "AS SELECT * FROM", f$physical))
  }
  DBI::dbExecute(f$admin, paste("CREATE SQL SECURITY DEFINER VIEW", f$declared,
    "AS SELECT * FROM", source))
  f$grant_metadata <- function() {
    DBI::dbExecute(f$admin, paste("GRANT SHOW VIEW ON", f$declared, "TO", f$runtime_principal))
    # MySQL exposes SHOW CREATE through REFERENCES alone. MariaDB documents
    # SELECT as necessary for SHOW CREATE; these grants are fixture setup only.
    privilege <- if (identical(dbms, "mariadb")) "SELECT" else "REFERENCES"
    DBI::dbExecute(f$admin, paste("GRANT", privilege, "ON", f$physical, "TO", f$runtime_principal))
    if (nested) DBI::dbExecute(f$admin, paste("GRANT SELECT, SHOW VIEW ON", source,
      "TO", f$runtime_principal))
  }
  prepared <- TRUE
  f
}

for (engine in c("mariadb", "mysql")) local({
  dbms <- engine
  test_that(paste(dbms, "rejects the exact external MyISAM view witness"), {
    f <- .snapshotViewFixture(dbms, "MyISAM")
    on.exit(f$cleanup(), add = TRUE)
    h <- f$handle
    # Preserve the independent witness: real counts and membership hashes can
    # change across a writer commit when a definer view reaches MyISAM.
    count <- function() .executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared))$n
    first <- count(); g1 <- .dsomopDpMembershipHash(h, "database")$digest
    DBI::dbExecute(f$admin, paste0("INSERT INTO ", f$physical,
      " SELECT 900001, gender_concept_id, year_of_birth, race_concept_id, ethnicity_concept_id FROM ",
      f$physical, " WHERE person_id=1"))
    expect_equal(as.integer(first), 3L)
    expect_equal(as.integer(count()), 4L)
    expect_false(identical(g1, .dsomopDpMembershipHash(h, "database")$digest))
    attempt <- function() .dsomopWithReadSnapshot(h, {
      a <- count(); ga <- .dsomopDpMembershipHash(h, "database")$digest
      DBI::dbExecute(f$admin, paste0("INSERT INTO ", f$physical,
        " SELECT 900002, gender_concept_id, year_of_birth, race_concept_id, ethnicity_concept_id FROM ",
        f$physical, " WHERE person_id=1"))
      list(first = a, second = count(), digest = ga,
           second_digest = .dsomopDpMembershipHash(h, "database")$digest,
           capability = .dsomopReadSnapshotLabel(h))
    })
    # Ordinary runtime privileges cannot certify the hidden dependency.
    expect_error(suppressMessages(attempt()), class = "dsomop_snapshot_unavailable")
    expect_null(h$read_snapshot)
    f$grant_metadata()
    # Even with complete metadata the actual MyISAM dependency refuses.
    expect_error(suppressMessages(attempt()), class = "dsomop_snapshot_unavailable")
    expect_null(h$read_snapshot)
    expect_equal(as.integer(count()), 4L)
  })

  test_that(paste(dbms, "certifies nested definer InnoDB views and keeps one snapshot"), {
    f <- .snapshotViewFixture(dbms, nested = TRUE)
    on.exit(f$cleanup(), add = TRUE)
    h <- f$handle
    f$grant_metadata()
    DBI::dbExecute(f$admin, paste("CREATE TABLE", paste0(f$schemas[["cdm"]], ".unused"),
      "(id INTEGER) ENGINE=MyISAM"))
    DBI::dbExecute(f$admin, paste("CREATE OR REPLACE SQL SECURITY DEFINER VIEW", f$declared,
      "AS SELECT * FROM", paste0(f$external, ".inner_person"), "WITH CASCADED CHECK OPTION"))
    if (identical(dbms, "mysql")) {
      expect_error(DBI::dbGetQuery(h$conn, paste("SELECT * FROM", f$physical)), "denied")
    } else {
      expect_equal(nrow(DBI::dbGetQuery(h$conn, paste("SELECT * FROM", f$physical))), 3L)
    }
    result <- .dsomopWithReadSnapshot(h, {
      first <- .executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared))$n
      g1 <- .dsomopDpMembershipHash(h, "database")$digest
      DBI::dbExecute(f$admin, paste0("INSERT INTO ", f$physical,
        " SELECT 900001, gender_concept_id, year_of_birth, race_concept_id, ethnicity_concept_id FROM ",
        f$physical, " WHERE person_id=1"))
      second <- .executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared))$n
      g2 <- .dsomopDpMembershipHash(h, "database")$digest
      expect_identical(first, second)
      expect_identical(g1, g2)
      expect_length(ls(h$read_snapshot$sources), 3L)
      data.frame(n = as.integer(second))
    })
    expect_equal(result$n, 3L)
    expect_null(h$read_snapshot)
    expect_equal(as.integer(.executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared))$n), 4L)
    # Final validation is automatic; transaction cleanup releases MDL so a
    # subsequent definition/engine change completes and the next request fails.
    DBI::dbExecute(f$admin, paste("ALTER TABLE", f$physical, "ENGINE=MyISAM"))
    expect_error(suppressMessages(.dsomopWithReadSnapshot(h,
      .executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared)))),
      class = "dsomop_snapshot_unavailable")
    expect_null(h$read_snapshot)
  })

  test_that(paste(dbms, "hidden nested metadata poisons the whole request only when read"), {
    f <- .snapshotViewFixture(dbms, nested = TRUE)
    on.exit(f$cleanup(), add = TRUE)
    h <- f$handle
    DBI::dbExecute(f$admin, paste("GRANT SHOW VIEW ON", f$declared, "TO", f$runtime_principal))
    # The unrelated unread view does not prevent a certified ordinary read.
    expect_equal(.dsomopWithReadSnapshot(h, .executeQuery(h, paste(
      "SELECT COUNT(*) AS n FROM", .qualifyTable(h, "observation_period"))))$n, 3)
    expect_error(suppressMessages(.dsomopWithReadSnapshot(h, {
      tryCatch(.executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared)),
        error = function(e) NULL)
      1
    })), class = "dsomop_snapshot_unavailable")
    expect_null(h$read_snapshot)
    expect_error(suppressMessages(.dsomopWithReadSnapshot(h, {
      tryCatch(.executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared)),
        error = function(e) stop("an inner generation wrapper replaced the condition"))
    })), class = "dsomop_snapshot_unavailable")
    expect_null(h$read_snapshot)
    f$grant_metadata()
    # Changing the graph after transaction entry but before first read cannot
    # exploit an entry-only certification (the external table is not declared).
    expect_error(suppressMessages(.dsomopWithReadSnapshot(h, {
      DBI::dbExecute(f$admin, paste("ALTER TABLE", f$physical, "ENGINE=MyISAM"))
      .executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared))
    })), class = "dsomop_snapshot_unavailable")
    expect_null(h$read_snapshot)
  })

  test_that(paste(dbms, "final boundary revalidates privileges and metadata locks"), {
    f <- .snapshotViewFixture(dbms, nested = TRUE)
    on.exit(f$cleanup(), add = TRUE)
    h <- f$handle
    f$grant_metadata()
    expect_error(suppressMessages(.dsomopWithReadSnapshot(h, {
      .executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared))
      DBI::dbExecute(f$admin, paste("REVOKE SHOW VIEW ON", f$declared,
        "FROM", f$runtime_principal))
      data.frame(n = 3)
    })), class = "dsomop_snapshot_unavailable")
    expect_null(h$read_snapshot)
    f$grant_metadata()
    DBI::dbExecute(f$admin, "SET SESSION lock_wait_timeout=1")
    .dsomopWithReadSnapshot(h, {
      expect_equal(.executeQuery(h, "SELECT 1 AS n FROM DUAL")$n, 1)
      expect_equal(.executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared))$n, 3)
      expect_error(DBI::dbExecute(f$admin, paste("ALTER TABLE", f$physical,
        "ENGINE=MyISAM")), "[Ll]ock wait timeout")
      expect_error(DBI::dbExecute(f$admin, paste("CREATE OR REPLACE VIEW", f$declared,
        "AS SELECT * FROM", f$physical)), "[Ll]ock wait timeout")
    })
    expect_null(h$read_snapshot)
  })

  test_that(paste(dbms, "source certification honors SQL quoting modes and namespace"), {
    f <- .snapshotViewFixture(dbms)
    on.exit(f$cleanup(), add = TRUE)
    h <- f$handle
    f$grant_metadata()
    DBI::dbExecute(h$conn, "SET SESSION sql_mode='ANSI_QUOTES,NO_BACKSLASH_ESCAPES'")
    .dsomopWithReadSnapshot(h, {
      quoted <- paste0('"', f$schemas[["cdm"]], '"."person"')
      expect_equal(.executeQuery(h, paste("SELECT COUNT(*) AS n FROM", quoted))$n, 3)
    })
    expect_error(suppressMessages(.dsomopWithReadSnapshot(h,
      .executeStatement(h, paste("USE", f$external)))), "read snapshot was lost")
    expect_null(h$read_snapshot)
    # A low-level session switch is also detected at the final boundary,
    # without relying on the query front door having guarded it.
    expect_error(suppressMessages(.dsomopWithReadSnapshot(h, {
      .executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared))
      DBI::dbExecute(h$conn, "SET SESSION sql_mode=''")
      1
    })), class = "dsomop_snapshot_unavailable")
    expect_null(h$read_snapshot)
  })

  test_that(paste(dbms, "stored-function views cannot hide external MyISAM reads"), {
    f <- .snapshotViewFixture(dbms)
    on.exit(f$cleanup(), add = TRUE)
    h <- f$handle
    f$grant_metadata()
    hidden <- paste0(f$external, ".hidden")
    fun <- paste0(f$external, ".hidden_count")
    DBI::dbExecute(f$admin, paste("CREATE TABLE", hidden, "(id INTEGER) ENGINE=MyISAM"))
    DBI::dbExecute(f$admin, paste("INSERT INTO", hidden, "VALUES (1)"))
    DBI::dbExecute(f$admin, paste("CREATE FUNCTION", paste0(fun, "()"),
      "RETURNS BIGINT READS SQL DATA RETURN (SELECT COUNT(*) FROM", paste0(hidden, ")")))
    DBI::dbExecute(f$admin, paste("CREATE OR REPLACE SQL SECURITY DEFINER VIEW", f$declared,
      "AS SELECT * FROM", f$physical, "WHERE", paste0(fun, "()"), "= 1"))
    expect_equal(.executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared))$n, 3)
    expect_error(suppressMessages(.dsomopWithReadSnapshot(h,
      .executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared)))),
      class = "dsomop_snapshot_unavailable")
    expect_null(h$read_snapshot)
    DBI::dbExecute(f$admin, paste("INSERT INTO", hidden, "VALUES (2)"))
    expect_equal(.executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared))$n, 0)
    DBI::dbExecute(f$admin, paste("CREATE OR REPLACE SQL SECURITY DEFINER VIEW", f$declared,
      "AS SELECT * FROM", f$physical, "WHERE RAND() < 0.5"))
    expect_error(suppressMessages(.dsomopWithReadSnapshot(h,
      .executeQuery(h, paste("SELECT COUNT(*) AS n FROM", f$declared)))),
      class = "dsomop_snapshot_unavailable")
    expect_null(h$read_snapshot)
  })

  test_that(paste(dbms, "base tables named VIEW cannot masquerade as views"), {
    f <- .snapshotViewFixture(dbms)
    on.exit(f$cleanup(), add = TRUE)
    bad <- paste0(f$external, ".`VIEW`")
    DBI::dbExecute(f$admin, paste("CREATE TABLE", bad, "(person_id BIGINT) ENGINE=MyISAM"))
    DBI::dbExecute(f$admin, paste("CREATE OR REPLACE VIEW", f$declared,
      "AS SELECT * FROM", bad))
    DBI::dbExecute(f$admin, paste("GRANT SHOW VIEW ON", f$declared, "TO", f$runtime_principal))
    privilege <- if (identical(dbms, "mariadb")) "SELECT" else "REFERENCES"
    DBI::dbExecute(f$admin, paste("GRANT", privilege, "ON", bad, "TO", f$runtime_principal))
    expect_error(suppressMessages(.dsomopWithReadSnapshot(f$handle,
      .executeQuery(f$handle, paste("SELECT * FROM", f$declared)))),
      class = "dsomop_snapshot_unavailable")
    expect_null(f$handle$read_snapshot)
  })
})

test_that("snapshot dependency parser follows scoped relational sources", {
  sources <- .dsomopSnapshotSqlSources(paste(
    "WITH q AS (SELECT * FROM one.base) SELECT p.x FROM (two.person p",
    "JOIN three.visit v ON p.x=v.x), q WHERE EXISTS (SELECT 1 FROM four.other)",
    "UNION ALL SELECT * FROM five.last"), "default")
  expect_setequal(paste(sources$schema, sources$table),
    c("one base", "two person", "three visit", "four other", "five last"))
  expect_equal(.dsomopSnapshotSqlSources(
    "WITH person AS (SELECT * FROM person) SELECT * FROM person", "main"),
    data.frame(schema = "main", table = "person"))
  expect_equal(.dsomopSnapshotSqlSources(
    "SELECT 'FROM secret.hidden' AS x FROM `odd`.`JOIN` JOIN `odd`.`(` ON 1=1", "main"),
    data.frame(schema = c("odd", "odd"), table = c("JOIN", "(")))
  expect_equal(.dsomopSnapshotSqlSources(
    "CREATE VIEW db.v AS SELECT * FROM db.person WITH CASCADED CHECK OPTION", "main", view = TRUE),
    data.frame(schema = "db", table = "person"))
  expect_equal(nrow(.dsomopSnapshotSqlSources("SELECT 1 FROM DUAL", "main")), 0L)
  expect_equal(.dsomopSnapshotSqlSources("SELECT * FROM `DUAL`", "main"),
    data.frame(schema = "main", table = "DUAL"))
  expect_equal(.dsomopSnapshotSqlSources(
    "SELECT * FROM JSON_TABLE((SELECT j FROM db.person), '$[*]' COLUMNS(id INT PATH '$')) t", "main"),
    data.frame(schema = "db", table = "person"))
  expect_error(.dsomopSnapshotSqlSources("SELECT * FROM other.TABLE_FN()", "main"), "Stored-routine")
  expect_equal(.dsomopSnapshotSqlSources(
    "WITH q AS (TABLE external.person) SELECT * FROM q", "main"),
    data.frame(schema = "external", table = "person"))
  for (f in c("RAND()", "LOAD_FILE('/unread')", "CURRENT_DATE")) {
    expect_error(.dsomopSnapshotSqlSources(paste("SELECT * FROM db.person WHERE", f),
      "main", view = TRUE), "Volatile or external")
  }
  expect_error(.dsomopSnapshotSqlSources("SELECT hidden_udf(id) FROM db.person", "main", view = TRUE),
    "unknown or stateful")
  expect_error(.dsomopSnapshotSqlSources("SELECT UNIX_TIMESTAMP() FROM db.person", "main", view = TRUE),
    "unknown or stateful")
  expect_equal(.dsomopSnapshotSqlSources(paste(
    "WITH q(id) AS (SELECT CAST(COALESCE(id, 0) AS SIGNED) FROM db.person)",
    "SELECT SUM(ABS(id)) OVER () FROM q WHERE id IN (1, 2)"), "main", view = TRUE),
    data.frame(schema = "db", table = "person"))
  expect_error(.dsomopSnapshotSqlSources("SELECT hidden_udf()", "main"), "unknown or stateful")
  expect_equal(.dsomopSnapshotSqlSources("SELECT NOW(), COUNT(*) FROM db.person", "main"),
    data.frame(schema = "db", table = "person"))
  expect_equal(.dsomopSnapshotSqlSources("INSERT INTO db.target(id) SELECT id FROM db.person", "main"),
    data.frame(schema = "db", table = "person"))
  ambiguous <- paste0("SELECT '", intToUtf8(92L), "' FROM hidden.person # ' FROM safe.person")
  expect_equal(.dsomopSnapshotSqlSources(ambiguous, "main", sql_mode = "NO_BACKSLASH_ESCAPES"),
    data.frame(schema = "hidden", table = "person"))
  expect_equal(.dsomopSnapshotSqlSources(ambiguous, "main"),
    data.frame(schema = "safe", table = "person"))
  expect_equal(.dsomopSnapshotSqlSources('SELECT "FROM secret.fake" FROM actual.person', "main"),
    data.frame(schema = "actual", table = "person"))
  expect_equal(.dsomopSnapshotSqlSources('SELECT * FROM "odd"."JOIN"', "main", sql_mode = "ANSI_QUOTES"),
    data.frame(schema = "odd", table = "JOIN"))
  expect_error(.dsomopSnapshotSqlSources("SELECT db.hidden_reader() FROM db.person", "main", view = TRUE),
    "Stored-routine")
  expect_error(.dsomopSnapshotSqlSources("SELECT 1 /*! FROM hidden.data */", "main"), "Executable")
})

test_that("actual request capability and all finalizers survive snapshot exit", {
  h <- create_test_handle(n_persons = 3)
  on.exit(cleanup_handle(h), add = TRUE)
  result <- .dsomopWithReadSnapshot(h, {
    h$read_snapshot$kind <- "not_isolated"
    structure(data.frame(n = 1), dsomop_protection = list(read_snapshot = "stale"))
  })
  expect_identical(attr(result, "dsomop_protection")$read_snapshot, "not_isolated")
  expect_null(h$read_snapshot)
  calls <- character()
  expect_error(.dsomopWithReadSnapshot(h, {
    h$read_snapshot$finalizers <- list(
      function(success) { calls <<- c(calls, paste0("a", success)); if (success) stop("cleanup failed") },
      function(success) { calls <<- c(calls, paste0("b", success)) })
    1
  }), "cleanup failed")
  expect_identical(calls, c("aTRUE", "bTRUE", "aFALSE", "bFALSE"))
  expect_null(h$read_snapshot)
})
