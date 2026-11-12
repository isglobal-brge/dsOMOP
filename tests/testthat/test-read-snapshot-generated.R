# R12: generated expressions are part of the transitive source certificate.
test_that("generated source expressions have a closed deterministic dependency contract", {
  ddl <- function(expr, extra = "") paste0("CREATE TABLE `person` (\n",
    "`person_id` bigint NOT NULL,\n",
    "`created` timestamp DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,\n",
    "`note` varchar(100) DEFAULT 'AS (RAND()) VIRTUAL',\n",
    "`year_of_birth` int GENERATED ALWAYS AS (", expr, ") VIRTUAL", extra,
    "\n) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8mb4")
  safe <- ddl("1960 + MOD(`person_id`, 50)")
  cert <- .dsomopSnapshotTableDefinition(safe)
  expect_identical(cert$dependencies$year_of_birth, "person_id")
  expect_identical(cert, .dsomopSnapshotTableDefinition(sub("AUTO_INCREMENT=4", "AUTO_INCREMENT=99", safe)))
  expect_false(identical(cert, .dsomopSnapshotTableDefinition(sub("1960", "1970", safe))))
  expect_false(identical(cert, .dsomopSnapshotTableDefinition(sub("VIRTUAL\\n", "STORED\n", safe))))
  expect_identical(.dsomopSnapshotTableDefinition(ddl(
    "CASE WHEN `person_id` > 1 THEN ABS(`person_id`) + 1.5 ELSE 0 END"))$dependencies$year_of_birth,
    "person_id")
  for (expr in c("RAND()", "NOW()", "CURRENT_TIMESTAMP", "UUID()", "LAST_INSERT_ID()",
                 "@@session.time_zone", "@pick", "hidden_udf(person_id)",
                 "db.ABS(person_id)", "(SELECT person_id FROM secret)", "missing + 1")) {
    expect_error(.dsomopSnapshotTableDefinition(ddl(expr)), "generated source")
  }
  expect_error(.dsomopSnapshotTableDefinition(ddl("year_of_birth + 1")), "Cyclic")
  expect_error(.dsomopSnapshotTableDefinition(ddl("next_year", ",\n`next_year` int AS (year_of_birth + 1) VIRTUAL")), "Cyclic")
  expect_length(.dsomopSnapshotTableDefinition(ddl("person_id", ",\n`next_year` int AS (year_of_birth + 1) STORED"))$generated, 2L)
})

for (engine in c("mariadb", "mysql")) local({
  dbms <- engine
  test_that(paste(dbms, "deterministic generated columns and stored defaults survive concurrent writes"), {
    config <- .dsomopVendorTestConfig(dbms)
    skip_if(is.null(config), "Vendor host not configured")
    f <- .dsomopVendorCreateFixture(config)
    on.exit(f$cleanup(), add = TRUE)
    h <- f$handle; p <- .qualifyTable(h, "person")
    DBI::dbExecute(f$admin, paste("ALTER TABLE", p,
      "DROP COLUMN year_of_birth,",
      "ADD COLUMN year_of_birth INTEGER GENERATED ALWAYS AS (1960+MOD(person_id,50)) VIRTUAL,",
      "ADD COLUMN next_year INTEGER GENERATED ALWAYS AS (year_of_birth+1) STORED,",
      "ADD COLUMN created TIMESTAMP DEFAULT CURRENT_TIMESTAMP"))
    read <- function() .executeQuery(h, paste("SELECT person_id,year_of_birth,next_year,created FROM", p, "ORDER BY person_id"))
    .dsomopWithReadSnapshot(h, {
      first <- read(); digest <- .dsomopDpMembershipHash(h, "database")$digest
      cert <- as.list(h$read_snapshot$sources)[[1L]]
      expect_setequal(names(cert$columns$generated), c("year_of_birth", "next_year"))
      DBI::dbExecute(f$admin, paste("INSERT INTO", p,
        "(person_id,gender_concept_id,race_concept_id,ethnicity_concept_id) VALUES (4,8507,8527,38003564)"))
      DBI::dbExecute(f$admin, paste("UPDATE", p, "SET person_id=51 WHERE person_id=1"))
      expect_identical(first, read())
      expect_identical(digest, .dsomopDpMembershipHash(h, "database")$digest)
      expect_equal(as.numeric(first$year_of_birth), 1961:1963)
      expect_equal(as.numeric(first$next_year), 1962:1964)
      expect_identical(.dsomopReadSnapshotLabel(h), "consistent_snapshot_transaction")
    })
    expect_null(h$read_snapshot)
    expect_equal(nrow(read()), 4L)
    # Metadata locks block real expression replacement until request rollback.
    DBI::dbExecute(f$admin, "SET SESSION lock_wait_timeout=1")
    .dsomopWithReadSnapshot(h, {
      read()
      expect_error(DBI::dbExecute(f$admin, paste("ALTER TABLE", p,
        "MODIFY year_of_birth INTEGER AS (1970+MOD(person_id,50)) VIRTUAL")), "[Ll]ock wait timeout")
    })
    DBI::dbExecute(f$admin, paste("ALTER TABLE", p,
      "MODIFY year_of_birth INTEGER AS (1970+MOD(person_id,50)) VIRTUAL"))
    expect_equal(as.numeric(.dsomopWithReadSnapshot(h, read())$year_of_birth), c(1972,1973,1974,1971))
    # Simulate a stale certificate, while the current definition is fetched
    # from the real database: final validation must compare expression semantics.
    expect_error(suppressMessages(.dsomopWithReadSnapshot(h, {
      read()
      key <- ls(h$read_snapshot$sources)[[1L]]
      cert <- get(key, h$read_snapshot$sources)
      cert$columns$generated$year_of_birth$value[[2L]] <- "stale"
      assign(key, cert, h$read_snapshot$sources)
      1
    })), class = "dsomop_snapshot_unavailable")
    expect_null(h$read_snapshot)
    # Even deterministic expressions must retain their session interpretation.
    prior <- DBI::dbGetQuery(h$conn, "SELECT @@session.div_precision_increment AS x")$x[[1L]]
    expect_error(suppressMessages(.dsomopWithReadSnapshot(h, {
      read()
      DBI::dbExecute(h$conn, paste("SET SESSION div_precision_increment=", (prior + 1L) %% 30L))
      1
    })), class = "dsomop_snapshot_unavailable")
    DBI::dbExecute(h$conn, paste("SET SESSION div_precision_increment=", prior))
    expect_null(h$read_snapshot)
    expect_equal(nrow(.dsomopWithReadSnapshot(h, read())), 4L)
    expect_length(h$temp_tables, 0L)
  })
})

test_that("MariaDB stable IDs behind nested definer views refuse volatile virtual membership", {
  config <- .dsomopVendorTestConfig("mariadb")
  skip_if(is.null(config), "Vendor host not configured")
  f <- .dsomopVendorCreateFixture(config)
  on.exit(f$cleanup(), add = TRUE)
  h <- f$handle; p <- .qualifyTable(h, "person")
  physical <- paste0(f$schemas[["results"]], ".source_person")
  nested <- paste0(f$schemas[["results"]], ".inner_person")
  DBI::dbExecute(f$admin, paste("CREATE TABLE", physical,
    "(person_id BIGINT NOT NULL, gender_concept_id INTEGER, year_of_birth INTEGER,",
    "race_concept_id INTEGER, ethnicity_concept_id INTEGER,",
    "pick DOUBLE GENERATED ALWAYS AS (RAND()) VIRTUAL) ENGINE=InnoDB"))
  rows <- paste0("(", seq_len(64), ",8507,1970,8527,38003564)", collapse = ",")
  DBI::dbExecute(f$admin, paste("INSERT INTO", physical,
    "(person_id,gender_concept_id,year_of_birth,race_concept_id,ethnicity_concept_id) VALUES", rows))
  DBI::dbExecute(f$admin, paste("DROP TABLE", p))
  DBI::dbExecute(f$admin, paste("CREATE SQL SECURITY DEFINER VIEW", nested,
    "AS SELECT person_id,gender_concept_id,year_of_birth,race_concept_id,ethnicity_concept_id FROM", physical, "WHERE pick < 0.5"))
  DBI::dbExecute(f$admin, paste("CREATE SQL SECURITY DEFINER VIEW", p, "AS SELECT * FROM", nested))
  for (v in c(p, nested)) DBI::dbExecute(f$admin, paste("GRANT SHOW VIEW ON", v, "TO", f$runtime_principal))
  read <- function() .executeQuery(h, paste("SELECT person_id FROM", p, "ORDER BY person_id"))
  expect_error(suppressMessages(.dsomopWithReadSnapshot(h, read())), class = "dsomop_snapshot_unavailable")
  expect_null(h$read_snapshot)
  expect_error(suppressMessages(.dsomopWithReadSnapshot(h,
    .dsomopDpMembershipHash(h, "database"))), class = "dsomop_snapshot_unavailable")
  expect_null(h$read_snapshot)
  finalized <- logical()
  expect_error(suppressMessages(.dsomopWithReadSnapshot(h, {
    h$read_snapshot$finalizers <- list(function(success) finalized <<- c(finalized, success))
    tryCatch(read(), error = function(e) NULL)
    1
  })), class = "dsomop_snapshot_unavailable")
  expect_identical(finalized, FALSE)
  expect_null(h$read_snapshot)
  expect_length(h$temp_tables, 0L)
  expect_error(suppressMessages(.dsomopWithReadSnapshot(h, {
    tryCatch(read(), error = function(e) stop("inner wrapper replaced error"))
  })), class = "dsomop_snapshot_unavailable")
  expect_null(h$read_snapshot)
  DBI::dbExecute(f$admin, paste("ALTER TABLE", physical,
    "MODIFY pick DOUBLE GENERATED ALWAYS AS (MOD(person_id,2)) VIRTUAL"))
  .dsomopWithReadSnapshot(h, {
    first <- read(); digest <- .dsomopDpMembershipHash(h, "database")$digest
    DBI::dbExecute(f$admin, paste("INSERT INTO", physical,
      "(person_id,gender_concept_id,year_of_birth,race_concept_id,ethnicity_concept_id) VALUES (66,8507,1970,8527,38003564)"))
    expect_identical(read(), first)
    expect_identical(digest, .dsomopDpMembershipHash(h, "database")$digest)
    expect_equal(as.numeric(first$person_id), seq(2,64,2))
    expect_length(ls(h$read_snapshot$sources), 3L)
  })
  expect_null(h$read_snapshot)
  expect_equal(nrow(.dsomopWithReadSnapshot(h, read())), 33L)
})

test_that("MariaDB stable keys refuse volatile virtual clinical fields and recover", {
  config <- .dsomopVendorTestConfig("mariadb")
  skip_if(is.null(config), "Vendor host not configured")
  f <- .dsomopVendorCreateFixture(config)
  on.exit(f$cleanup(), add = TRUE)
  h <- f$handle; p <- .qualifyTable(h, "person")
  DBI::dbExecute(f$admin, paste("ALTER TABLE", p,
    "DROP COLUMN year_of_birth, ADD COLUMN year_of_birth INTEGER AS (1900+FLOOR(RAND()*100)) VIRTUAL"))
  read <- function() .executeQuery(h, paste("SELECT person_id,year_of_birth FROM", p, "ORDER BY person_id"))
  expect_error(suppressMessages(.dsomopWithReadSnapshot(h, read())), class = "dsomop_snapshot_unavailable")
  expect_null(h$read_snapshot)
  expect_length(h$temp_tables, 0L)
  DBI::dbExecute(f$admin, paste("ALTER TABLE", p,
    "MODIFY year_of_birth INTEGER AS (1960+person_id) VIRTUAL"))
  expect_equal(as.numeric(.dsomopWithReadSnapshot(h, read())$year_of_birth), 1961:1963)
  expect_null(h$read_snapshot)
})
