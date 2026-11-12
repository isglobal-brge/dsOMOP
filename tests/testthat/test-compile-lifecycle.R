# A-20 (k), B7: compilation is private operation-owned state. Exercise the
# executor and publication exits as well as the compiler's standalone helper.

.ws5_lifecycle_plan <- function() {
  .ws5_plan(events = list(d = list(table = "drug_exposure")),
             intervals = list(i = list(source = list(event = "d"), stockpile = TRUE)))
}

.ws5_temp_names <- function(handle) {
  DBI::dbGetQuery(handle$conn,
    "SELECT name FROM sqlite_temp_master WHERE type = 'table'")$name
}

test_that("native execution registers operation cleanup before compilation", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  pre <- .createTempTable(h, "ws5_native_preexisting", "SELECT 17 AS value")
  p <- .ws5_lifecycle_plan()
  for (supplied_context in c(FALSE, TRUE)) {
    for (failure in c("error", "rejection", "validation")) {
      ctx <- if (supplied_context) .ws4_ctx(h, p) else NULL
      compile <- NULL
      compile_sources <- function(cctx, ...) {
        compile <<- cctx
        .compileIntervalByName(cctx, "i")
        # Also cover work tables owned by the operation but not registered as
        # compiler tables. Before B7 the native branch missed this snapshot.
        .createTempTable(h, "ws5_native_operation", "SELECT 23 AS value")
        if (failure == "error") stop("downstream compile failure")
        if (failure == "rejection") {
          .compileReject("downstream compile refusal", reason = "reducer_row_cap")
        }
        # Inject a validation refusal explicitly; do not depend on a later
        # compiler being absent once the native population stage is merged.
        .irReject("downstream validation refusal")
      }
      testthat::with_mocked_bindings({
        error <- tryCatch(.planExecute(h, p, list(o = "out"), ctx = ctx), error = identity)
      }, .planCompileSources = compile_sources)
      expect_s3_class(error, if (failure == "error") "simpleError" else "dsomop_plan_rejection")
      expect_true(is.environment(compile))
      expect_setequal(h$temp_tables, pre)
      expect_setequal(.ws5_temp_names(h), pre)
      if (!is.null(ctx)) .planContextClose(ctx)
      expect_length(compile$compile_temp_tables, 0L)
    }
  }
  expect_identical(DBI::dbGetQuery(h$conn, paste0("SELECT value FROM ", pre))$value, 17L)
})

test_that("a database insertion error exposes failed cleanup and retains ownership for retry", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  pre <- .createTempTable(h, "ws5_insert_preexisting", "SELECT 29 AS value")
  create <- .createTempTable
  create_rejecting_insert <- function(handle, name, select_sql) {
    result <- create(handle, name, select_sql)
    DBI::dbExecute(handle$conn, paste0(
      "CREATE TEMP TRIGGER ws5_reject_insert BEFORE INSERT ON ", name,
      " BEGIN SELECT RAISE(ABORT, 'injected insert failure'); END"))
    result
  }
  for (drop_fails in c(FALSE, TRUE)) {
    ctx <- .ws4_ctx(h, .ws5_lifecycle_plan())
    drop <- if (drop_fails) function(...) stop("injected DROP failure") else .dropTempTable
    testthat::with_mocked_bindings({
      error <- tryCatch(.compileIntervalByName(ctx, "i"), error = identity)
    }, .createTempTable = create_rejecting_insert, .dropTempTable = drop)
    expect_match(conditionMessage(error), "injected insert failure")
    if (drop_fails) {
      expect_match(conditionMessage(error), "Could not release the compiler temporary table")
      expect_s3_class(error$cleanup_error, "error")
      expect_length(ctx$compile_temp_tables, 1L)
      expect_true(ctx$compile_temp_tables %in% h$temp_tables)
      expect_true(ctx$compile_temp_tables %in% .ws5_temp_names(h))
    } else {
      expect_length(ctx$compile_temp_tables, 0L)
    }
    .planContextClose(ctx)
    expect_setequal(h$temp_tables, pre)
    expect_setequal(.ws5_temp_names(h), pre)
    expect_length(ctx$compile_temp_tables, 0L)
  }
})

test_that("context closure aggregates parent and child failures and releases successful drops", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  pre <- .createTempTable(h, "ws5_close_preexisting", "SELECT 31 AS value")
  ctx <- .ws4_ctx(h, .ws5_lifecycle_plan())
  child <- .compileContext(ctx, ctx$ir)
  .compileIntervalByName(ctx, "i")
  .compileIntervalByName(child, "i")
  parent_table <- ctx$compile_temp_tables
  child_table <- child$compile_temp_tables
  released <- .createTempTable(h, "ws5_successful_drop", "SELECT 37 AS value")
  ctx$compile_temp_tables <- c(ctx$compile_temp_tables, released)
  drop <- .dropTempTable
  testthat::with_mocked_bindings({
    error <- tryCatch(.planContextClose(ctx), error = identity)
  }, .dropTempTable = function(handle, name) {
    if (name %in% c(parent_table, child_table)) stop("injected DROP failure")
    drop(handle, name)
  })
  expect_match(conditionMessage(error), parent_table, fixed = TRUE)
  expect_match(conditionMessage(error), child_table, fixed = TRUE)
  expect_true(ctx$closed)
  expect_false(exists(ctx$id, .planContextRegistry, inherits = FALSE))
  expect_identical(ctx$compile_temp_tables, parent_table)
  expect_identical(child$compile_temp_tables, child_table)
  expect_false(released %in% .ws5_temp_names(h))
  # A second closure retries both retained registries even though the context
  # is already closed, and preserves the caller's original table.
  .planContextClose(ctx)
  expect_length(ctx$compile_temp_tables, 0L)
  expect_length(child$compile_temp_tables, 0L)
  expect_setequal(h$temp_tables, pre)
  expect_setequal(.ws5_temp_names(h), pre)
})

test_that("publication success, refusal and downstream failure all close compiler state", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  pre <- .createTempTable(h, "ws5_publish_preexisting", "SELECT 41 AS value")
  for (outcome in c("success", "refusal", "failure")) {
    ctx <- .publication_test_context(threshold = 3L)
    ctx$handle <- h
    child <- .compileContext(ctx, .irNormalize(.irJsonForm(.ws5_lifecycle_plan())))
    .compileIntervalByName(child, "i")
    .publishPrepare(ctx, .publication_test_candidate(),
      data.frame(person_id = if (outcome == "refusal") 1L else 1:3, value = 1L),
      already_tokenized = TRUE)
    if (outcome == "failure") {
      ctx$failure_hook <- function(step) {
        if (step == "assign") stop("downstream publication failure")
      }
      expect_error(.publication_test_commit(ctx), "downstream publication failure")
    } else {
      expect_identical(.publication_test_commit(ctx)$status,
                        if (outcome == "success") "ok" else "refused")
    }
    expect_true(ctx$closed)
    expect_length(child$compile_temp_tables, 0L)
    expect_setequal(h$temp_tables, pre)
    expect_setequal(.ws5_temp_names(h), pre)
    expect_identical(exists("out", ctx$target_env, inherits = FALSE), outcome == "success")
  }
})

test_that("native output compilation installs cleanup before creating its child context", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  pre <- .createTempTable(h, "ws7_native_preexisting", "SELECT 43 AS value")
  ctx <- .ws4_ctx(h, .ws5_lifecycle_plan())
  create_context <- .compileContext
  child <- NULL
  testthat::with_mocked_bindings({
    expect_error(.planExecuteNative(ctx, ctx$ir, list(o = "out")), "child creation failure")
  }, .compileContext = function(ctx, ir) {
    child <<- create_context(ctx, ir)
    .compileIntervalByName(child, "i")
    .createTempTable(h, "ws7_native_unregistered", "SELECT 47 AS value")
    stop("child creation failure")
  })
  expect_true(.isPlanContext(ctx))
  expect_false(is.null(ctx$ir))
  expect_length(child$compile_temp_tables, 0L)
  expect_setequal(h$temp_tables, pre)
  expect_setequal(.ws5_temp_names(h), pre)
  .planContextClose(ctx)
})

test_that("native output cleanup attempts unregistered objects after compiler DROP failure", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  pre <- .createTempTable(h, "ws7_native_keep", "SELECT 53 AS value")
  ctx <- .ws4_ctx(h, .ws5_lifecycle_plan())
  child <- .compileContext(ctx, ctx$ir)
  drop <- .dropTempTable
  failing <- NULL
  released <- NULL
  testthat::with_mocked_bindings({
    # Capture after unwinding: the original compile error is followed by a
    # cleanup error, so an expectation's calling handler would see both.
    failure <- tryCatch(.planExecuteNative(ctx, ctx$ir, list(o = "out")), error = identity)
  }, .planCompileOutputs = function(cctx, ...) {
    .compileIntervalByName(cctx, "i")
    failing <<- cctx$compile_temp_tables
    released <<- .createTempTable(h, "ws7_native_release", "SELECT 59 AS value")
    stop("output compilation failure")
  }, .dropTempTable = function(handle, name) {
    if (name %in% failing) stop("injected native DROP failure")
    drop(handle, name)
  })
  expect_s3_class(failure, "error")
  expect_match(conditionMessage(failure), "injected native DROP failure")
  expect_true(.isPlanContext(ctx))
  expect_identical(child$compile_temp_tables, failing)
  expect_false(released %in% .ws5_temp_names(h))
  expect_true(pre %in% .ws5_temp_names(h))
  .planContextClose(ctx)
  expect_length(child$compile_temp_tables, 0L)
  expect_setequal(h$temp_tables, pre)
  expect_setequal(.ws5_temp_names(h), pre)
})
