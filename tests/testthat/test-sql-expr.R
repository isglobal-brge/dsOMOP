# Typed SQL helpers (R/sql_expr.R; design-2.10.3 sections 6.3 and 9, defect
# A-E8). Rendered SQL per family is pinned in test-sql-expr-snapshots.R; the
# live PostgreSQL/MySQL/MariaDB runs are in
# test-vendor-db-integration-sql-expr.R.

.ws3_all_operations <- c(
  "concept_set", "event", "event.unit", "event.on_missing_table", "interval",
  "interval.ingredient", "interval.stockpile", "population.index",
  "population.first_eligible", "population.any_of", "population.criteria",
  "population.parent", "population.setop", "population.match",
  "population.coverage.union", "criteria.not", "criteria.age",
  "window.anchor.source", "variable.reducer", "variable.derived",
  "variable.raw", "variable.population_column", "output.wide", "output.long",
  "output.long.event_log", "output.survival", "output.survival.censor_source",
  "output.intervals", "output.temporal_covariates", "output.person_period",
  "output.person_period.partition", "output.sparse", "output.membership",
  "output.dictionary", "output.joined", "output.concept_labels", "publish.roots",
  "event.episode_members", "event.cost", "reducer.supply_days",
  "reducer.covered_days.effective_supply", "status.accessor", "verb.mutate",
  "verb.filter_expr"
)

# Every helper rendered for one family, with the construct it belongs to.
.ws3_render_all <- function(f) {
  list(
    date_add_days = c(.sqlDateAddDays(f, "t.start_date", 30L),
                      .sqlDateAddDays(f, "t.start_date", "t.days_supply - 1"),
                      .sqlDateAddDays(f, "t.start_date", -7L)),
    date_diff_days = .sqlDateDiffDays(f, "t.start_date", "t.end_date"),
    calendar = c(.sqlYear(f, "t.d"), .sqlMonth(f, "t.d"),
                 .sqlMonthKey(f, "t.d"),
                 .sqlCalendarFloor(f, "t.d", "month"),
                 .sqlCalendarFloor(f, "t.d", "quarter"),
                 .sqlCalendarFloor(f, "t.d", "year"),
                 .sqlCalendarAdvance(f, "t.d", 1L, "month"),
                 .sqlCalendarAdvance(f, "t.d", "b.n", "quarter"),
                 .sqlCalendarAdvance(f, "t.d", -2L, "year"),
                 .sqlMakeDate(f, "p.year_of_birth", "p.month_of_birth",
                              "p.day_of_birth"),
                 .sqlMakeDate(f, 2020L, 2L, 29L),
                 .sqlMakeDate(f, 2019L, 2L, 29L)),
    date_literal = .sqlDateLiteral(f, "2020-01-01"),
    integer_literal = c(.sqlIntegerLiteral(f, 42L),
                        .sqlIntegerLiteral(f, -3L),
                        .sqlIntegerLiteral(f, "9007199254740993")),
    least_greatest = c(.sqlLeast(f, "a.end_date", "b.op_end"),
                       .sqlGreatest(f, "x.a", "x.b", "x.c"),
                       .sqlLeast(f, "x.a")),
    nulls_last = c(.sqlNullsLast(f, "e.value"),
                   .sqlNullsLast(f, "e.value", descending = TRUE)),
    window_functions = .sqlRowNumber(f, "r.person_id",
                                     c("e.start_date", "e.event_id")),
    window_frames = c(
      .sqlWindow(f, "MAX(r.end_date)", c("r.person_id", "r.group_id"),
                 c("r.start_date", "r.event_id"), frame = "preceding"),
      .sqlWindow(f, "SUM(r.new_era)", c("r.person_id", "r.group_id"),
                 c("r.start_date", "r.event_id"), frame = "cumulative")),
    lossless_key = c(.sqlLosslessKey(f, "e.event_id"),
                     .sqlLosslessKeyText(f, "e.event_id")),
    date_text = .sqlDateText(f, "c.cohort_start_date"),
    sha256_sql = c(unlist(.sqlSha256Hex(f, "t.label")),
                   unlist(.sqlSha256Hex(f, "t.encoding_hex", input = "hex")))
  )
}

test_that("the section 6.3 signatures are the frozen ones", {
  # Frozen in section 12.1; only optional trailing arguments may be added
  # (CR-6: .sqlNullsLast(descending), .sqlSha256Hex(input)).
  frozen <- list(
    .sqlDateAddDays = c("h", "date_expr", "days_expr"),
    .sqlDateDiffDays = c("h", "from_expr", "to_expr"),
    .sqlYear = c("h", "date_expr"),
    .sqlMonthKey = c("h", "date_expr"),
    .sqlCalendarFloor = c("h", "date_expr", "unit"),
    .sqlCalendarAdvance = c("h", "date_expr", "n_expr", "unit"),
    .sqlMakeDate = c("h", "year_expr", "month_expr", "day_expr"),
    .sqlDateLiteral = c("h", "x"),
    .sqlIntegerLiteral = c("h", "x"),
    .sqlLeast = c("h", "..."),
    .sqlGreatest = c("h", "..."),
    .sqlNullsLast = c("h", "order_expr", "descending"),
    .sqlRowNumber = c("h", "partition", "order"),
    .sqlLosslessKey = c("h", "expr"),
    .sqlSha256Hex = c("h", "expr", "input")
  )
  for (name in names(frozen)) {
    expect_identical(names(formals(get(name))), frozen[[name]], info = name)
  }
  expect_identical(eval(formals(.sqlCalendarFloor)$unit),
                   c("month", "quarter", "year"))
  expect_identical(formals(.sqlNullsLast)$descending, FALSE)
  expect_identical(eval(formals(.sqlSha256Hex)$input), c("text", "hex"))
})

test_that("families resolve from names, handles, contexts and profiles", {
  expect_identical(.sqlFamily("postgresql"), "postgresql")
  expect_identical(.sqlFamily("postgres"), "postgresql")
  expect_identical(.sqlFamily("sqlite"), "sqlite")
  expect_identical(.sqlFamily("duckdb"), "duckdb")
  expect_identical(.sqlFamily("mysql"), "mysql")
  expect_identical(.sqlFamily("MariaDB"), "mysql")
  expect_identical(.sqlEngine("mariadb"), "mariadb")
  expect_identical(.sqlFamily("sql server"), "sqlserver")
  expect_identical(.sqlFamily("sql_server"), "sqlserver")
  expect_identical(.sqlFamily("synapse"), "sqlserver")
  expect_identical(.sqlFamily("pdw"), "sqlserver")
  expect_identical(.sqlFamily("oracle"), "oracle")
  expect_identical(.sqlFamily("redshift"), "redshift")
  expect_identical(.sqlFamily("bigquery"), "bigquery")
  expect_identical(.sqlFamily("snowflake"), "snowflake")
  expect_identical(.sqlFamily("databricks"), "spark")
  expect_identical(.sqlEngine("databricks"), "databricks")
  expect_error(.sqlFamily("access"), "Unsupported SQL dialect family")
  expect_error(.sqlFamily(""), "non-empty")

  handle <- new.env(parent = emptyenv())
  handle$dbms <- "mariadb"
  handle$target_dialect <- "mysql"
  expect_identical(.sqlFamily(handle), "mysql")
  expect_identical(.sqlEngine(handle), "mariadb")
  expect_identical(.sqlFamily(list(target_dialect = "sql server")),
                   "sqlserver")
  expect_identical(.sqlFamily(list(dbms = "duckdb")), "duckdb")
  expect_error(.sqlFamily(list(dbms = "postgresql", target_dialect = "sqlite")),
               "different SQL families")
  expect_error(.sqlFamily(list()), "neither target_dialect nor dbms")
  expect_error(.sqlFamily(42), "Cannot resolve")

  ctx <- list(handle = handle)
  expect_identical(.sqlFamily(ctx), "mysql")
  profile <- .sqlDialectProfile("snowflake")
  expect_s3_class(profile, "dsomop_sql_dialect")
  expect_identical(.sqlFamily(profile), "snowflake")
  expect_identical(.sqlFamily(list(dialect = profile, handle = handle)),
                   "snowflake")
})

test_that("the availability matrix is the design section 9 matrix", {
  m <- .sqlHelperMatrix()
  expect_identical(colnames(m), .SQL_FAMILIES)
  expect_identical(rownames(m), .SQL_CONSTRUCTS)
  expect_true(all(m %in% c("executable", "snapshot", "unsupported")))
  executable <- c("postgresql", "sqlite", "duckdb", "mysql")
  contract_only <- setdiff(.SQL_FAMILIES, executable)
  expect_identical(.SQL_EXECUTABLE_FAMILIES, executable)
  expect_true(all(m[setdiff(.SQL_CONSTRUCTS, "sha256_sql"), executable] ==
                    "executable"))
  expect_identical(unname(m["sha256_sql", executable]),
                   c("executable", "unsupported", "executable", "executable"))
  expect_true(all(m["session_table", contract_only] == "unsupported"))
  expect_true(all(m[setdiff(.SQL_CONSTRUCTS, "session_table"),
                    contract_only] == "snapshot"))
})

test_that("NULL helpers and unsupported constructs agree family by family", {
  m <- .sqlHelperMatrix()
  for (f in .SQL_FAMILIES) {
    rendered <- .ws3_render_all(f)
    for (construct in names(rendered)) {
      if (identical(m[construct, f], "unsupported")) {
        expect_length(rendered[[construct]], 0L)
        expect_false(.sqlHelperAvailable(f, construct))
      } else {
        expect_gt(length(rendered[[construct]]), 0L)
        expect_true(all(nzchar(rendered[[construct]])), info = paste(f, construct))
        expect_true(.sqlHelperAvailable(f, construct))
      }
    }
    expect_identical(
      identical(.sqlDialectProfile(f)$temporary_materialization,
                "session_table"),
      !identical(m["session_table", f], "unsupported"),
      info = f
    )
    expect_identical(isTRUE(.sqlDialectProfile(f)$sha256_sql),
                     !identical(m["sha256_sql", f], "unsupported"), info = f)
  }
  expect_null(.sqlSha256Hex("sqlite", "t.x"))
  expect_null(.sqlSha256Hex("sqlite", "t.x", input = "hex"))
})

test_that("helper output is unchanged by the legacy translator", {
  for (f in .SQL_FAMILIES) {
    dialect <- unname(.SQL_TARGET_DIALECT_OF_FAMILY[[f]])
    for (sql in unlist(.ws3_render_all(f), use.names = FALSE)) {
      expect_identical(.sql_translate(sql, dialect), sql,
                       info = paste(f, sql))
      expect_identical(.renderSql(list(target_dialect = dialect), sql), sql,
                       info = paste(f, sql))
      expect_silent(.sqlAssertTypedSql(f, sql))
    }
  }
})

test_that("new SQL refuses the OHDSI DATEADD/DATEDIFF spellings (A-E8 guard)", {
  legacy <- "SELECT DATEADD(day, c.n1, c.d1) AS v FROM cases c"
  legacy_diff <- "SELECT DATEDIFF(day, c.d1, c.d2) AS v FROM cases c"
  for (f in c("postgresql", "sqlite", "duckdb", "mysql", "oracle", "bigquery",
              "spark")) {
    expect_error(.sqlAssertTypedSql(f, legacy), "typed helpers")
    expect_error(.sqlAssertTypedSql(f, legacy_diff), "typed helpers")
  }
  for (f in c("sqlserver", "redshift", "snowflake")) {
    expect_identical(.sqlAssertTypedSql(f, legacy), legacy)
  }
  quoted <- "SELECT 'DATEADD(day, 1, x)' AS label -- DATEDIFF(day, a, b)\n"
  expect_identical(.sqlAssertTypedSql("sqlite", quoted), quoted)
  expect_identical(.sqlAssertTypedSql("mysql", "SELECT DATEDIFF(a, b)"),
                   "SELECT DATEDIFF(a, b)")
})

test_that("integer literals are exact, typed and composition-safe", {
  expect_identical(.sqlIntegerLiteral("postgresql", 42L), "42")
  expect_identical(.sqlIntegerLiteral("postgresql", 0), "0")
  expect_identical(.sqlIntegerLiteral("postgresql", -0), "0")
  expect_identical(.sqlIntegerLiteral("postgresql", -3L), "(-3)")
  expect_identical(.sqlIntegerLiteral("sqlite", 2^53), "9007199254740992")
  expect_identical(.sqlIntegerLiteral("mysql", "9223372036854775807"),
                   "9223372036854775807")
  expect_identical(.sqlIntegerLiteral("mysql", "-9223372036854775808"),
                   "(-9223372036854775808)")
  expect_identical(.sqlIntegerLiteral("mysql", "-0007"), "(-7)")
  expect_identical(.sqlIntegerLiteral("mysql", "000"), "0")
  expect_error(.sqlIntegerLiteral("mysql", "9223372036854775808"),
               "64-bit range")
  expect_error(.sqlIntegerLiteral("mysql", "-9223372036854775809"),
               "64-bit range")
  expect_error(.sqlIntegerLiteral("postgresql", 1.5), "whole number")
  expect_error(.sqlIntegerLiteral("postgresql", 2^53 + 2), "2\\^53")
  expect_error(.sqlIntegerLiteral("postgresql", Inf), "whole number")
  expect_error(.sqlIntegerLiteral("postgresql", NA_integer_), "non-missing")
  expect_error(.sqlIntegerLiteral("postgresql", c(1L, 2L)), "one")
  expect_error(.sqlIntegerLiteral("postgresql", TRUE), "whole number")
  expect_error(.sqlIntegerLiteral("postgresql", "1e5"), "decimal integer")
  expect_error(.sqlIntegerLiteral("access", 1L), "Unsupported")
  if (requireNamespace("bit64", quietly = TRUE)) {
    expect_identical(
      .sqlIntegerLiteral("postgresql", bit64::as.integer64("9007199254740993")),
      "9007199254740993"
    )
  }
  # A negative operand can never form a line comment.
  expect_identical(paste0("x - ", .sqlIntegerLiteral("sqlite", -1L)),
                   "x - (-1)")
})

test_that("date literals are typed per family and strictly validated", {
  expect_identical(.sqlDateLiteral("postgresql", "2020-01-01"),
                   "DATE '2020-01-01'")
  expect_identical(.sqlDateLiteral("sqlite", "2020-01-01"), "'2020-01-01'")
  expect_identical(.sqlDateLiteral("sqlserver", as.Date("2020-02-29")),
                   "CAST('2020-02-29' AS DATE)")
  expect_identical(.sqlDateLiteral("redshift", "2020-01-01"),
                   "CAST('2020-01-01' AS DATE)")
  expect_identical(.sqlDateLiteral("mysql", "9999-12-31"),
                   "DATE '9999-12-31'")
  expect_error(.sqlDateLiteral("postgresql", "2019-02-29"), "valid calendar")
  expect_error(.sqlDateLiteral("postgresql", "2020-1-1"), "ISO date")
  expect_error(.sqlDateLiteral("postgresql", "0999-12-31"), "1000-01-01")
  expect_error(.sqlDateLiteral("postgresql", "2020-01-01'; --"), "ISO date")
  expect_error(.sqlDateLiteral("postgresql", NA_character_), "ISO date")
  expect_error(.sqlDateLiteral("postgresql", as.Date(NA)), "non-missing")
})

test_that("operands are validated as single well-formed expressions", {
  expect_error(.sqlDateAddDays("sqlite", "t.d", "a--1"), "line comment")
  expect_error(.sqlDateAddDays("sqlite", "t.d; DROP TABLE x", 1L),
               "statement separator")
  expect_error(.sqlDateAddDays("sqlite", "t.d /* x */", 1L), "block comment")
  expect_error(.sqlDateAddDays("sqlite", "f(t.d", 1L), "unbalanced")
  expect_error(.sqlDateAddDays("sqlite", "t.d)", 1L), "unbalanced")
  expect_error(.sqlDateAddDays("sqlite", "'open", 1L), "unterminated")
  expect_error(.sqlDateAddDays("mysql", "t.d # comment", 1L), "line comment")
  expect_error(.sqlDateAddDays("sqlite", NA_character_, 1L), "non-empty")
  expect_error(.sqlDateAddDays("sqlite", c("a", "b"), 1L), "non-empty")
  expect_error(.sqlDateAddDays("sqlite", 5L, 1L), "date expression")
  expect_error(.sqlDateAddDays("sqlite", "t.d", 1.5), "whole number")
  expect_error(.sqlDateAddDays("sqlite", "t.d", as.Date("2020-01-01")),
               "SQL expression or one whole number")
  # Quoted segments may contain separators and comment markers.
  expect_match(.sqlDateAddDays("sqlite", "COALESCE(t.d, '2020-01-01')",
                               "LENGTH('a;b--c')"), "LENGTH")
  expect_match(.sqlSha256Hex("mysql", "CONCAT(t.a, 'it\\'s; ok')"), "SHA2")
  expect_match(.sqlDateAddDays("postgresql", as.Date("2020-01-01"), 1L),
               "DATE '2020-01-01'")
})

test_that("window helpers refuse implicit or meaningless frames", {
  expect_match(.sqlRowNumber("postgresql", character(0), "x"),
               "^ROW_NUMBER\\(\\) OVER \\(ORDER BY x\\)$")
  expect_error(.sqlRowNumber("postgresql", "p", character(0)),
               "deterministic ORDER BY")
  expect_error(.sqlWindow("postgresql", "ROW_NUMBER()", "p", "x",
                          frame = "preceding"), "takes no window frame")
  expect_error(.sqlWindow("postgresql", "LAG(x.v)", "p", character(0)),
               "deterministic ORDER BY")
  expect_error(.sqlWindow("postgresql", "SUM(x.v)", "p", "x.d"),
               "must declare its frame")
  expect_error(.sqlWindow("postgresql", "SUM(x.v)", "p", character(0),
                          frame = "cumulative"), "needs a deterministic")
  expect_error(.sqlWindow("postgresql", "SUM(x.v)", "p", "x.d",
                          frame = "range"), "frame must be")
  expect_match(.sqlWindow("mysql", "LAG(x.v)", "x.p", "x.d"),
               "^LAG\\(x.v\\) OVER \\(PARTITION BY x.p ORDER BY x.d\\)$")
  expect_match(.sqlWindow("sqlite", "MAX(x.v)", "x.p", frame = "partition"),
               "UNBOUNDED FOLLOWING")
})

test_that("calendar and extremum helpers validate their arguments", {
  expect_error(.sqlCalendarFloor("postgresql", "t.d", "week"), "should be one")
  expect_error(.sqlCalendarAdvance("postgresql", "t.d", 1L, "week"),
               "unit must be")
  expect_error(.sqlCalendarAdvance("postgresql", "t.d", 2^40, "month"),
               "32-bit")
  expect_error(.sqlLeast("postgresql"), "at least one operand")
  expect_identical(.sqlLeast("mysql", "x.a"), "(x.a)")
  expect_identical(.sqlGreatest("sqlite", "x.a", "x.b"),
                   "MAX(COALESCE(x.a, x.b), COALESCE(x.b, x.a))")
  expect_identical(.sqlLeast("postgresql", "x.a", 0L), "LEAST(x.a, 0)")
  expect_identical(.sqlMakeDate("postgresql", 2019L, 2L, 29L),
                   "CAST(NULL AS DATE)")
  expect_identical(.sqlMakeDate("sqlite", 2019L, 2L, 29L), "NULL")
  expect_identical(.sqlMakeDate("sqlite", 2024L, 2L, 29L), "'2024-02-29'")
  expect_error(.sqlNullsLast("postgresql", "x", descending = NA),
               "TRUE or FALSE")
  expect_error(.sqlSha256Hex("postgresql", "x", input = "base64"),
               "should be one")
})

test_that("dialect_unsupported is a typed rejection naming the construct", {
  err <- tryCatch(.sqlRequire("sqlite", "sha256_sql"), error = identity)
  expect_s3_class(err, "dsomop_dialect_unsupported")
  expect_s3_class(err, "dsomop_plan_rejection")
  expect_identical(err$code, "dialect_unsupported")
  expect_identical(err$construct, "sha256_sql")
  expect_identical(err$family, "sqlite")
  expect_null(err$output)
  # Constant text: the construct and the family, never data values or counts.
  expect_identical(conditionMessage(err), paste0(
    "dialect_unsupported: the plan needs the SQL construct 'sha256_sql', ",
    "which is not available for the sqlite SQL family of this server."))

  err <- tryCatch(
    .sqlRequire("sqlite", "sha256_sql", .sqlSha256Hex("sqlite", "x.k"),
                output = "matched"),
    error = identity)
  expect_s3_class(err, "dsomop_dialect_unsupported")
  expect_identical(err$output, "matched")
  expect_match(conditionMessage(err), "output 'matched' needs")
  expect_identical(.sqlRequire("duckdb", "sha256_sql",
                               .sqlSha256Hex("duckdb", "x.k")),
                   "SHA256(x.k)")
  expect_true(.sqlRequire("oracle", "window_frames"))
  expect_error(.sqlRequire("oracle", "no_such_construct"), "Unknown SQL construct")
  expect_error(.sqlDialectUnsupported("oracle", "nope"), "Unknown SQL construct")
})

test_that("every capability operation maps to constructs", {
  expect_setequal(names(.SQL_OPERATION_CONSTRUCTS), .ws3_all_operations)
  all <- .sqlPlanConstructs(.ws3_all_operations)
  expect_true(all(all %in% .SQL_CONSTRUCTS))
  expect_false("session_table" %in% all)
  expect_false("sha256_sql" %in% all)  # matching falls back to R (section 6.10)
  expect_true(all(.SQL_BASELINE_CONSTRUCTS %in% .sqlPlanConstructs()))
  expect_true("window_frames" %in% .sqlPlanConstructs("interval"))
  expect_true("calendar" %in% .sqlPlanConstructs("criteria.age"))
  expect_error(.sqlPlanConstructs("output.unknown"), "No SQL construct mapping")
})

test_that("preflight refuses missing constructs before any query", {
  constructs <- .sqlPlanConstructs(.ws3_all_operations)
  for (f in .SQL_FAMILIES) {
    expect_s3_class(.sqlDialectPreflight(f, constructs), "dsomop_sql_dialect")
  }
  for (f in c("sqlserver", "oracle", "redshift", "bigquery", "snowflake",
              "spark")) {
    err <- tryCatch(.sqlDialectPreflight(f, constructs, session_tables = TRUE,
                                         output = "baseline"),
                    error = identity)
    expect_s3_class(err, "dsomop_dialect_unsupported")
    expect_identical(err$construct, "session_table")
    expect_identical(err$output, "baseline")
  }
  for (f in c("postgresql", "sqlite", "duckdb", "mysql")) {
    expect_silent(.sqlDialectPreflight(f, constructs, session_tables = TRUE))
  }
  err <- tryCatch(.sqlDialectPreflight("sqlite", "sha256_sql"),
                  error = identity)
  expect_identical(err$construct, "sha256_sql")

  # A SQLite runtime whose probe could not execute the frame is refused.
  old_sqlite <- .sqlDialectProfile("sqlite")
  expect_true(is.na(old_sqlite$window_frames))
  old_sqlite$window_frames <- FALSE
  err <- tryCatch(.sqlDialectPreflight(old_sqlite, "window_frames"),
                  error = identity)
  expect_identical(err$construct, "window_frames")
  err <- tryCatch(.sqlDialectPreflight(old_sqlite, "window_functions"),
                  error = identity)
  expect_identical(err$construct, "window_functions")

  # MySQL/MariaDB below the CTE/window baseline (versions from the handle).
  below <- list(conn = new.env(), dbms = "mysql", target_dialect = "mysql",
                dbms_version = "5.7.44-log")
  err <- tryCatch(.sqlDialectPreflight(below, "date_add_days"),
                  error = identity)
  expect_s3_class(err, "dsomop_dialect_unsupported")
  expect_identical(err$construct, "subqueries_cte")
  below_profile <- .sqlDialectProfile(below, probe = TRUE)
  expect_false(below_profile$window_frames)
  expect_false(below_profile$analytic_baseline)
  expect_false(.sqlHelperAvailable(below_profile, "date_add_days"))
  # PostgreSQL has sha256() only from version 11: the profile says so and the
  # helper returns NULL (matching then orders in R).
  pg10 <- list(conn = new.env(), dbms = "postgresql",
               target_dialect = "postgresql", dbms_version = "10.23")
  expect_true(.sqlDialectProfile(pg10, probe = FALSE)$sha256_sql)
  pg10_profile <- .sqlDialectProfile(pg10, probe = TRUE)
  expect_identical(pg10_profile$runtime, "PostgreSQL 10.23")
  expect_false(pg10_profile$sha256_sql)
  expect_null(.sqlSha256Hex(pg10_profile, "x.k"))
  expect_false(.sqlHelperAvailable(pg10_profile, "sha256_sql"))
  expect_identical(
    tryCatch(.sqlDialectPreflight(pg10_profile, "sha256_sql"),
             error = identity)$construct,
    "sha256_sql")
  pg14 <- list(conn = new.env(), dbms = "postgresql",
               target_dialect = "postgresql",
               dbms_version = "PostgreSQL 14.24 (Ubuntu 14.24-0ubuntu0.22.04.1)")
  expect_identical(.sqlDialectProfile(pg14, probe = TRUE)$runtime,
                   "PostgreSQL 14.24")
  maria_old <- list(conn = new.env(), dbms = "mariadb",
                    target_dialect = "mysql",
                    dbms_version = "5.5.5-10.1.48-MariaDB")
  expect_identical(
    tryCatch(.sqlDialectPreflight(maria_old, character(0)),
             error = identity)$construct,
    "subqueries_cte")
  maria_ok <- list(conn = new.env(), dbms = "mariadb", target_dialect = "mysql",
                   dbms_version = "11.4.2-MariaDB-ubu2404")
  profile <- .sqlDialectPreflight(maria_ok, constructs, session_tables = TRUE)
  expect_identical(profile$engine, "mariadb")
  expect_identical(profile$runtime, "MariaDB 11.4.2")
  mysql_ok <- list(conn = new.env(), dbms = "mysql", target_dialect = "mysql",
                   dbms_version = "8.4.3")
  expect_identical(.sqlDialectPreflight(mysql_ok, constructs)$runtime,
                   "MySQL 8.4.3")

  expect_error(.sqlDialectPreflight("sqlite", "nope"), "Unknown SQL construct")
  expect_error(.sqlDialectPreflight("sqlite", session_tables = NA),
               "TRUE or FALSE")
})

test_that("MariaDB and MySQL versions are read from the server's own number", {
  # Ubuntu 22.04's MariaDB 10.6 package: the distribution suffix carries
  # dotted numbers ("0.22.04") that the last-number rule took for the server
  # version, so the connection floor refused a MariaDB 10.6 as below 10.2.
  ubuntu <- "10.6.23-MariaDB-0ubuntu0.22.04.1"
  expect_identical(.databaseVersionNumber(ubuntu, "mariadb"), "10.6.23")
  expect_true(.databaseVersionAtLeast(ubuntu, "10.2.0", "mariadb"))
  expect_true(.assertAnalyticDbmsSupport(list(
    dbms = "mariadb", target_dialect = "mysql", dbms_version = ubuntu)))
  expect_true(.assertAnalyticDbmsSupport(list(
    dbms = "mysql", target_dialect = "mysql", dbms_version = ubuntu)))
  expect_identical(.sqlDialectProfile(list(
    conn = new.env(), dbms = "mariadb", target_dialect = "mysql",
    dbms_version = ubuntu), probe = TRUE)$runtime, "MariaDB 10.6.23")
  # Replication prefix, other distribution suffixes, untagged strings.
  expect_identical(.databaseVersionNumber("5.5.5-10.6.12-MariaDB-log",
                                          "mariadb"), "10.6.12")
  expect_false(.databaseVersionAtLeast("5.5.5-10.1.48-MariaDB", "10.2.0",
                                       "mariadb"))
  expect_identical(.databaseVersionNumber("11.4.2-MariaDB-ubu2404",
                                          "mariadb"), "11.4.2")
  expect_identical(.databaseVersionNumber("10.11.6-MariaDB-0+deb12u1",
                                          "mariadb"), "10.11.6")
  expect_identical(.databaseVersionNumber("10.6.23", "mariadb"), "10.6.23")
  expect_identical(.databaseVersionNumber("8.0.36-0ubuntu0.22.04.1", "mysql"),
                   "8.0.36")
  expect_true(.databaseVersionAtLeast("8.4.3", "8.0.0", "mysql"))
  expect_false(.databaseVersionAtLeast("5.7.44-log", "8.0.0", "mysql"))
  expect_null(.databaseVersionNumber("", "mysql"))
  expect_null(.databaseVersionNumber("unknown", "mariadb"))
  expect_false(.databaseVersionAtLeast(NA_character_, "8.0.0", "mysql"))
})

test_that("the capability dialect block has exactly the section 4.4 fields", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  handle <- .sqlExprHandle(con, "sqlite")
  cap <- .sqlDialectCapability(handle)
  expect_identical(names(cap), c("family", "temporary_materialization",
                                 "typed_sql", "sha256_sql", "window_frames",
                                 "runtime"))
  expect_identical(cap$family, "sqlite")
  expect_identical(cap$temporary_materialization, "session_table")
  expect_true(cap$typed_sql)
  expect_false(cap$sha256_sql)
  expect_true(cap$window_frames)
  expect_match(cap$runtime, "^SQLite [0-9]+\\.[0-9]+")
  expect_s3_class(handle$sql_dialect_profile, "dsomop_sql_dialect")
  expect_true(handle$sql_dialect_profile$probed)
  expect_match(handle$sql_dialect_profile$driver, "^RSQLite ")
})

test_that("typed helpers execute on SQLite with dynamic operands", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  handle <- .sqlExprHandle(con, "sqlite")
  profile <- .sqlDialectPreflight(handle,
                                  .sqlPlanConstructs(.ws3_all_operations),
                                  session_tables = TRUE)
  expect_true(profile$window_frames)
  n <- .sqlExprRunChecks(con, "sqlite", "SQLite")
  expect_gt(n, 35L)
  .sqlExprRecordRuntime(profile, "SQLite", n)
})

test_that("typed helpers execute on DuckDB with dynamic operands", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  handle <- .sqlExprHandle(con, "duckdb")
  profile <- .sqlDialectPreflight(handle,
                                  .sqlPlanConstructs(.ws3_all_operations),
                                  session_tables = TRUE)
  expect_match(profile$runtime, "^DuckDB [0-9]")
  n <- .sqlExprRunChecks(con, "duckdb", "DuckDB")
  expect_gt(n, 35L)
  .sqlExprRecordRuntime(profile, "DuckDB", n)
})

test_that("A-E8: dynamic DATEADD/DATEDIFF pass the legacy translator untranslated; the typed helpers compute them", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- .sqlExprLoadCases(con, "sqlite")

  # Legacy behaviour, unchanged for v1 paths: a column offset and a
  # non-identifier DATEDIFF operand are not translated ...
  dynamic_add <- paste0("SELECT DATEADD(day, c.n1, c.d1) AS v FROM ",
                        .sqlExprCaseTable, " c ORDER BY c.id")
  expect_identical(.sql_translate(dynamic_add, "sqlite"), dynamic_add)
  dynamic_diff <- paste0("SELECT DATEDIFF(day, c.d1, DATEADD(day, 1, c.d2)) ",
                         "AS v FROM ", .sqlExprCaseTable, " c ORDER BY c.id")
  translated <- .sql_translate(dynamic_diff, "sqlite")
  expect_match(translated, "DATEDIFF(day, c.d1, DATE(c.d2, '1 days'))",
               fixed = TRUE)
  # ... so SQLite receives SQL it cannot run (the probe of the defect), and
  # the typed-SQL guard refuses both forms in new SQL.
  expect_error(DBI::dbGetQuery(con, dynamic_add), "no such")
  expect_error(DBI::dbGetQuery(con, translated), "no such")
  expect_error(.sqlAssertTypedSql("sqlite", dynamic_add), "typed helpers")
  expect_error(.sqlAssertTypedSql("sqlite", translated), "typed helpers")

  # The typed helpers compute both dynamic forms correctly.
  add <- DBI::dbGetQuery(con, paste0(
    "SELECT ", .sqlDateAddDays("sqlite", "c.d1", "c.n1"), " AS v FROM ",
    .sqlExprCaseTable, " c ORDER BY c.id"))$v
  expect_identical(add, .sqlExprDateChr(as.Date(rows$d1) + rows$n1))
  diff <- DBI::dbGetQuery(con, paste0(
    "SELECT ", .sqlDateDiffDays("sqlite", "c.d1",
                                .sqlDateAddDays("sqlite", "c.d2", 1L)),
    " AS v FROM ", .sqlExprCaseTable, " c ORDER BY c.id"))$v
  expect_identical(diff, as.integer(as.Date(rows$d2) + 1L - as.Date(rows$d1)))
})

test_that("A-E8 regression also holds on DuckDB", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  rows <- .sqlExprLoadCases(con, "duckdb")
  dynamic_add <- paste0("SELECT DATEADD(day, c.n1, c.d1) AS v FROM ",
                        .sqlExprCaseTable, " c ORDER BY c.id")
  expect_identical(.sql_translate(dynamic_add, "duckdb"), dynamic_add)
  expect_error(DBI::dbGetQuery(con, dynamic_add))
  add <- DBI::dbGetQuery(con, paste0(
    "SELECT ", .sqlDateAddDays("duckdb", "c.d1", "c.n1"), " AS v FROM ",
    .sqlExprCaseTable, " c ORDER BY c.id"))$v
  expect_identical(as.character(add),
                   .sqlExprDateChr(as.Date(rows$d1) + rows$n1))
})

test_that("SQL SHA-256 reproduces the published matching vectors byte for byte", {
  vectors <- .sqlExprMatchVectors()
  rows <- .sqlExprCaseRows()
  for (i in seq_along(vectors)) {
    expect_identical(.sqlExprSha256(.sqlExprHexToRaw(rows$hx[[i]])),
                     vectors[[i]]$sha256)
  }
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .sqlExprLoadCases(con, "duckdb", rows)
  got <- DBI::dbGetQuery(con, paste0(
    "SELECT ", .sqlSha256Hex("duckdb", "c.hx", input = "hex"), " AS h FROM ",
    .sqlExprCaseTable, " c WHERE c.id <= 3 ORDER BY c.id"))$h
  expect_identical(got, vapply(vectors, function(v) v$sha256, character(1)))
})
