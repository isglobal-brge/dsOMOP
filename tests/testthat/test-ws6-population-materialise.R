# Database-resident population snapshots keep episode identities immutable
# across later statements, including MySQL's repeated temporary-table reads.
.ws6_freeze_backends <- .ws6_backends()
withr::defer(.ws4_cleanup(.ws6_freeze_backends), testthat::teardown_env())

.ws6_freeze_fixture <- function(h) {
  ctx <- .ws4_ctx(h, .ws6_plan(populations = list(p = list(kind = "criteria"))))
  d <- .compileDialect(ctx)
  long_key <- paste(rep("origin:", 120L), collapse = "")
  high_keys <- c("9223372036854775807", "9223372036854775806", "9223372036854775805")
  columns <- list(cohort_row_id = .fragmentColumn("bigint", FALSE, "episode_key"),
                   subject_id = .fragmentColumn("bigint", FALSE, "person_key"),
                   index_date = .fragmentColumn("date", FALSE, "start"),
                   index_key = .fragmentColumn("text", FALSE, "origin_identity"),
                   source_key = .fragmentColumn("bigint", FALSE, "origin_identity"),
                   sampling_weight = .fragmentColumn("numeric", TRUE, "measure"))
  # Deliberately project the subject before the row key: freezing must select
  # the named contract columns explicitly instead of relying on SELECT order.
  source <- .fragment("ws6_freeze_input", "candidates", "episode", paste0(
    "SELECT p.person_id AS subject_id, ROW_NUMBER() OVER (ORDER BY p.person_id) AS cohort_row_id, ",
    .sqlDateLiteral(d, "2020-01-01"), " AS index_date, ", .compileTextLiteral(long_key), " AS index_key, ",
    "CASE WHEN p.person_id = 1 THEN ", .sqlIntegerLiteral(d, high_keys[[1L]]),
    " WHEN p.person_id = 2 THEN ", .sqlIntegerLiteral(d, high_keys[[2L]]),
    " ELSE ", .sqlIntegerLiteral(d, high_keys[[3L]]), " END AS source_key, CASE WHEN p.person_id = 2 THEN ",
    .compileTypedNull(d, "numeric"), " ELSE ", .predicateNumericLiteral(1 / 3), " END AS sampling_weight ",
    "FROM ", .qualifyTable(h, "person"), " p WHERE p.person_id IN (1, 2, 3)"),
    columns = columns, unique_key = "cohort_row_id",
    person_lineage = list(person_col = "subject_id", via_roster = NULL))
  .fragmentRegister(ctx, source)
  f <- .fragment("ws6_frozen_roster", "roster", "episode",
                  paste0("SELECT * FROM ", source$name), columns = columns,
                  dependencies = source$name, unique_key = "cohort_row_id",
                  person_lineage = list(person_col = "subject_id", via_roster = NULL),
                  roster_namespace = "p", source_identity_columns = c("subject_id", "index_key"),
                  private_columns = c("index_key", "source_key"), ordering = "cohort_row_id")
  attr(f, "population") <- "p"
  attr(f, "index_sources") <- "fixture"
  list(ctx = ctx, fragment = f, long_key = long_key, high_keys = high_keys)
}

test_that("population snapshots retain typed rows without a fetch, count or memory cap", {
  for (b in names(.ws6_freeze_backends)) {
    h <- .ws6_freeze_backends[[b]]$handle
    x <- .ws6_freeze_fixture(h)
    ctx <- x$ctx
    frozen <- withr::with_options(list(dsomop.max_memory_rows = 1L),
      testthat::with_mocked_bindings(.populationFreezeFragment(ctx, x$fragment),
        .fetchBounded = function(...) stop("population rows were fetched"),
        .adapterCount = function(...) stop("population rows were counted")))
    expect_null(.fragmentGet(ctx, frozen$name), info = b)
    expect_identical(frozen$dependencies, character(0), info = b)
    fields <- setdiff(names(frozen), c("select_body", "dependencies"))
    expect_identical(frozen[fields], x$fragment[fields], info = b)
    expect_identical(attr(frozen, "population"), "p", info = b)
    expect_identical(attr(frozen, "index_sources"), "fixture", info = b)
    .fragmentRegister(ctx, frozen)
    expect_error(.populationFreezeFragment(ctx, frozen), "before registration")
    # DuckDB's default driver maps BIGINT to R doubles. Check the stored
    # identities using a lossless SQL text projection, before that conversion.
    read <- function() {
      columns <- vapply(names(frozen$columns), function(column) {
        if (identical(column, "source_key")) paste0(.sqlLosslessKeyText(ctx$handle, "t.source_key"),
                                                    " AS source_key") else paste0("t.", column)
      }, "")
      sql <- .buildStatement(ctx, frozen, select = paste0("SELECT ", paste(columns, collapse = ", "),
        " FROM ", frozen$name, " t ORDER BY t.cohort_row_id"), dialect = ctx)
      rows <- .executeQuery(h, sql)
      rows$index_date <- as.Date(as.character(rows$index_date))
      rows
    }
    before <- read()
    expect_equal(as.numeric(before$cohort_row_id), 1:3, info = b)
    expect_equal(as.numeric(before$subject_id), 1:3, info = b)
    expect_identical(as.character(before$index_key), rep(x$long_key, 3L), info = b)
    expect_identical(as.character(before$source_key), x$high_keys, info = b)
    expect_equal(as.numeric(before$sampling_weight), c(1 / 3, NA, 1 / 3), tolerance = 0, info = b)
    expect_identical(format(before$index_date, "%Y-%m-%d"), rep("2020-01-01", 3L), info = b)
    # A database update would both change membership and renumber the live CTE.
    .executeStatement(h, paste0("DELETE FROM ", .qualifyTable(h, "person"), " WHERE person_id = 1"))
    expect_identical(read(), before, info = b)
    # Reusing the relation twice must work on MySQL, including its numeric
    # fraction and NULL transport; bigint identities keep their separate type.
    sql <- .buildStatement(ctx, frozen, select = paste0(
      "SELECT a.cohort_row_id, ", .sqlLosslessKeyText(h, "a.source_key"),
      " AS source_key, a.sampling_weight, b.index_key FROM ", frozen$name,
      " a INNER JOIN ", frozen$name, " b ON a.subject_id = b.subject_id ORDER BY a.cohort_row_id"), dialect = ctx)
    twice <- .executeQuery(h, sql)
    expect_identical(as.character(twice$source_key), as.character(before$source_key), info = b)
    expect_equal(as.numeric(twice$sampling_weight), as.numeric(before$sampling_weight), tolerance = 0, info = b)
    expect_identical(as.character(twice$index_key), as.character(before$index_key), info = b)
    .compileReleaseTempTables(ctx)
    expect_length(ctx$compile_temp_tables, 0L)
  }
  .ws5_expect_backends(.ws6_freeze_backends, "WS-6 population snapshots")
})

test_that("failed population materialisation releases its own table and retains failed drops", {
  h <- .ws4_reach_handle()
  on.exit(try(DBI::dbDisconnect(h$conn), silent = TRUE), add = TRUE)
  pre <- .createTempTable(h, "ws6_preexisting", "SELECT 1 AS x")
  x <- .ws6_freeze_fixture(h)
  ctx <- x$ctx
  physical <- function() DBI::dbGetQuery(h$conn, "SELECT name FROM sqlite_temp_master WHERE type = 'table'")$name
  testthat::with_mocked_bindings(
    expect_error(.populationFreezeFragment(ctx, x$fragment), "injected insertion failure"),
    .executeStatement = function(...) stop("injected insertion failure"))
  expect_identical(ctx$compile_temp_tables, character(0))
  expect_identical(h$temp_tables, pre)
  expect_identical(physical(), pre)
  err <- testthat::with_mocked_bindings(
    tryCatch(.populationFreezeFragment(ctx, x$fragment), error = identity),
    .executeStatement = function(...) stop("injected insertion failure"),
    .dropTempTable = function(...) stop("injected drop failure"))
  expect_match(conditionMessage(err), "injected insertion failure")
  expect_match(conditionMessage(err), "Could not release the compiler temporary table")
  expect_s3_class(err$cleanup_error, "error")
  expect_length(ctx$compile_temp_tables, 1L)
  expect_true(ctx$compile_temp_tables %in% physical())
  .compileReleaseTempTables(ctx)
  expect_identical(ctx$compile_temp_tables, character(0))
  expect_identical(h$temp_tables, pre)
  expect_identical(physical(), pre)
})
