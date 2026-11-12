# A-26 compiler handoff. The immutable oracle is the complete pristine
# 30b7905 result, not a regenerated expectation from the implementation.
.ws7f_member_backends <- function() {
  source <- create_test_handle()
  out <- list(sqlite = list(handle = source, cleanup = function() cleanup_handle(source)))
  complete <- FALSE
  on.exit(if (!complete) .ws4_cleanup(out), add = TRUE)
  if (requireNamespace("duckdb", quietly = TRUE)) {
    conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    out$duckdb <- local({
      db <- conn
      list(handle = .ws4_handle_on(db, "duckdb", "duckdb", "main"),
           cleanup = function() DBI::dbDisconnect(db, shutdown = TRUE))
    })
    .ws4_copy_tables(source$conn, conn)
  }
  pg <- .ws4_pg_connect()
  if (!is.null(pg)) {
    schema <- .ws4_namespace("ws7f_membership")
    out$postgresql <- list(handle = .ws4_handle_on(pg, "postgresql", "postgresql", schema),
      cleanup = function() {
        on.exit(DBI::dbDisconnect(pg), add = TRUE)
        DBI::dbExecute(pg, paste0("DROP SCHEMA IF EXISTS ", schema, " CASCADE"))
      })
    DBI::dbExecute(pg, "SET client_min_messages TO WARNING")
    DBI::dbExecute(pg, paste0("CREATE SCHEMA ", schema))
    .ws4_copy_tables(source$conn, pg, schema)
  }
  for (vendor in c("mariadb", "mysql")) local({
    config <- .dsomopVendorTestConfig(vendor)
    if (is.null(config) || !requireNamespace("RMariaDB", quietly = TRUE)) return(NULL)
    conn <- DBI::dbConnect(RMariaDB::MariaDB(), host = config$host, port = config$port,
      dbname = config$database, user = config$admin_user, password = config$admin_password)
    db <- .ws4_namespace("ws7f_membership")
    out[[vendor]] <<- list(handle = .ws4_handle_on(conn, vendor, "mysql", db),
      cleanup = function() {
        on.exit(DBI::dbDisconnect(conn), add = TRUE)
        DBI::dbExecute(conn, paste0("DROP DATABASE IF EXISTS ", db))
      })
    .ensureMariaSession(conn, require_tls = FALSE)
    DBI::dbExecute(conn, paste0("CREATE DATABASE ", db))
    .ws4_copy_tables(source$conn, conn, db)
  })
  complete <- TRUE
  out
}

.ws7f_member_dbs <- .ws7f_member_backends()
withr::defer(.ws4_cleanup(.ws7f_member_dbs), testthat::teardown_env())

.ws7f_member_plan <- function(id, different = FALSE) {
  p <- list(cohort = list(type = "spec", cohort_definition_id = id,
    filter_tree = list(type = "sex", params = list(value = "M"))),
    outputs = list(members = list(type = "cohort_membership")))
  if (different) {
    p$populations <- list(women = list(kind = "criteria",
      filter_tree = list(type = "sex", params = list(value = "F"))))
    p$outputs$members$population_id <- "women"
  }
  p
}

.ws7f_member_cases <- function() {
  values <- list(one = 1L, two = 2L, zero = 0L, text = "02", fraction = 2.9,
    logical = TRUE, negative = -2L, negative_fraction = -2.9,
    bad_text = "foo", overflow = 2147483648, null = NULL)
  cases <- lapply(values, .ws7f_member_plan)
  cases$omitted <- .ws7f_member_plan(NULL)
  cases$omitted$cohort$cohort_definition_id <- NULL
  cases$different_population1 <- .ws7f_member_plan(1L, TRUE)
  cases$different_population2 <- .ws7f_member_plan(2L, TRUE)
  cases
}

.ws7f_member_capture <- function(expr) {
  warnings <- character(0)
  result <- withCallingHandlers(tryCatch(force(expr), error = identity), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(result = result, warnings = warnings)
}

# Native population compilation supplies genuine frozen WS-6 roster records
# through the supported populations argument. A-14 still routes v1 legacy
# population variants to their adapters; no population/compiler is mocked.
.ws7f_member_roster <- function(handle, different = FALSE, empty = FALSE) {
  population <- if (different) "women" else "base"
  native <- list(version = 2L, meta = list(language_version = "2.0",
    semantic_version = "2.0.0", generated_by = "ws7f-membership"),
    populations = stats::setNames(list(list(kind = "index", index = list(
      source = list(cohort_definition_id = 1L), select = "all",
      end = list(strategy = "event_end")))), population),
    outputs = list(members = list(rows = population, format = "membership")))
  run <- .ws7_ctx(handle, native)
  complete <- FALSE
  on.exit(if (!complete) .ws7_close(run), add = TRUE)
  rows <- .executeQuery(handle, paste0("SELECT p.person_id AS subject_id, ",
    "o.observation_period_start_date AS cohort_start_date, ",
    "o.observation_period_end_date AS cohort_end_date FROM ",
    .qualifyTable(handle, "person", handle$cdm_schema), " p INNER JOIN ",
    .qualifyTable(handle, "observation_period", handle$cdm_schema),
    " o ON o.person_id = p.person_id WHERE p.gender_concept_id = ",
    if (different) "8532" else "8507", if (empty) " AND 1 = 0" else "",
    " ORDER BY p.person_id, o.observation_period_start_date, o.observation_period_end_date"))
  pop <- .ws7_population(run, population, rows)
  complete <- TRUE
  list(run = run, pop = pop, name = population,
       roster = .reduceRosterFrame(run$cctx, pop$roster))
}

.ws7f_member_compile <- function(handle, plan, supplied, mode = NULL, raw = plan) {
  ir <- .irPrepare(plan, handle)
  ctx <- .planContext(handle, raw, mode %||% "memory")
  on.exit(.ws7_close(list(ctx = ctx)), add = TRUE)
  ctx$validation <- list(ir = ir, wire = .irJsonForm(raw))
  cctx <- .compileContext(ctx, ir)
  # Frozen production roster fragments have no live source dependencies.
  .fragmentRegister(cctx, supplied$pop$roster)
  compiled <- .planCompileOutputs(cctx, list(members = "D_members"),
    stats::setNames(list(supplied$pop), supplied$name))$members
  component <- compiled$components$members
  result <- list(frame = component$frame, columns = component$fragment$columns)
  if (!is.null(mode)) {
    .publishCompiled(ctx, compiled)
    result$status <- .publishCommit(ctx)
    symbol <- result$status$published$members$members$symbol
    result$published <- .ws7f_member_read(get(symbol, ctx$target_env, inherits = FALSE))
  }
  result
}

.ws7f_member_read <- function(value) {
  if (inherits(value, "FlowerDatasetDescriptor")) {
    expect_true(file.exists(omopStagedDatasetPath(value)))
    schema <- unlist(value$metadata$column_types, use.names = TRUE)
    format <- value$metadata$format
    chunks <- list()
    .publishReadChunks(value, function(x) chunks[[length(chunks) + 1L]] <<- x)
    value <- do.call(rbind, chunks)
    expect_identical(names(value), names(schema))
    # CSV is deliberately read as text for lossless publication lineage.
    # Restore only types explicitly declared by the persisted descriptor;
    # never infer a type from the expected baseline or the observed values.
    if (identical(format, "csv")) for (name in names(value)) {
      value[[name]] <- switch(schema[[name]],
        "integer|integer" = as.integer(value[[name]]),
        "double|numeric" = as.numeric(value[[name]]),
        "double|Date" = as.Date(value[[name]]),
        "logical|logical" = as.logical(value[[name]]),
        "character|character" = value[[name]],
        stop("Unexpected membership descriptor type: ", schema[[name]]))
    }
    expect_identical(vapply(value, function(x) paste(typeof(x), paste(class(x), collapse = "/"),
                                                    sep = "|"), ""), schema)
  }
  # Publication adds protective attributes; compare every data column, its
  # type and order separately from that publication metadata.
  as.data.frame(lapply(value, identity), check.names = FALSE, stringsAsFactors = FALSE)
}

.ws7f_member_public <- function(symbol, plan, mode) {
  target <- new.env(parent = environment())
  result <- .ws7f_member_capture(eval(substitute(omopPlanExecuteDS(SYMBOL, PLAN,
    list(members = "D_members"), output_mode = MODE),
    list(SYMBOL = symbol, PLAN = plan, MODE = mode)), target))
  status <- result$result
  if (!inherits(status, "error")) {
    ctx <- get(attr(status, "execution_id"), .publicationRegistry, inherits = FALSE)
    on.exit(.publishAbort(ctx), add = TRUE)
    expect_true(ctx$closed)
    expect_false(exists(ctx$id, .planContextRegistry, inherits = FALSE))
    expect_length(ls(ctx$staging_env, all.names = TRUE), 0L)
    expect_true(.validatePlanStatus(unclass(status)))
    if (exists("D_members", target, inherits = FALSE)) {
      result$frame <- .ws7f_member_read(get("D_members", target, inherits = FALSE))
    }
  }
  result$bindings <- ls(target)
  result
}

test_that("A-26 membership tests exercise every configured vendor", {
  env <- c(postgresql = "DSOMOP_TEST_POSTGRES_HOST", mariadb = "DSOMOP_TEST_MARIADB_HOST",
           mysql = "DSOMOP_TEST_MYSQL_HOST")
  expect_true(all(c("sqlite", names(env)[nzchar(Sys.getenv(env))]) %in% names(.ws7f_member_dbs)))
  if (requireNamespace("duckdb", quietly = TRUE)) expect_true("duckdb" %in% names(.ws7f_member_dbs))
})

test_that("A-26 lowered membership compiler preserves the complete baseline matrix", {
  baseline <- dget(testthat::test_path("fixtures", "a26-membership-baseline.R"))
  cases <- .ws7f_member_cases()
  expect_identical(names(cases), names(baseline))
  withr::local_options(list(nfilter.subset = 5L, nfilter.tab = 1L))
  for (vendor in names(.ws7f_member_dbs)) local({
    h <- .ws7f_member_dbs[[vendor]]$handle
    key <- .ws7_keys(list(h))
    before <- h$temp_tables
    male <- .ws7f_member_roster(h)
    on.exit(.ws7_close(male$run), add = TRUE)
    female <- .ws7f_member_roster(h, TRUE)
    on.exit(.ws7_close(female$run), add = TRUE)
    for (name in names(cases)) {
      plan <- cases[[name]]
      expected <- baseline[[name]]
      supplied <- if (grepl("different_population", name)) female else male
      validation <- .planValidate(h, plan)
      expect_identical(validation[c("valid", "errors", "warnings")], expected$validation,
                       info = paste(vendor, name))
      for (mode in c("memory", "staged")) {
        info <- paste(vendor, name, mode)
        actual <- .ws7f_member_capture(.ws7f_member_compile(h, plan, supplied, mode))
        expect_identical(actual$warnings, expected$warnings, info = info)
        if (!is.null(expected$result$error)) {
          expect_true(inherits(actual$result, "error"), info = info)
          expect_identical(conditionMessage(actual$result), expected$result$error, info = info)
          expect_identical(class(actual$result), expected$result$class, info = info)
        } else {
          # Lowered criteria membership declares person grain. The native
          # consumer therefore projects away episode keys while retaining
          # baseline person order, label values/types and source provenance.
          # Public v1 parity below separately checks the full episode frame.
          expect_identical(.irPrepare(plan, h)$outputs$members$grain, "person", info = info)
          frame <- expected$result$members[c("subject_id", "cohort_definition_id")]
          names(frame)[names(frame) == "subject_id"] <- "person_id"
          frame$index_source <- rep("cohort:1", nrow(frame))
          expect_identical(actual$result$frame, frame, info = info)
          expect_identical(actual$result$columns$cohort_definition_id,
                           .fragmentColumn("integer", TRUE, "measure"), info = info)
          expect_identical(actual$result$status$status, "ok", info = info)
          published <- .ws7f_member_read(.testPseudonymize(frame, key))
          expect_identical(actual$result$published, published, info = info)
        }
      }
    }
    .ws7_close(female$run)
    .ws7_close(male$run)
    expect_setequal(h$temp_tables, before)
  })
})

test_that("A-26 public memory and staged v1 membership preserve baseline parity at k5", {
  baseline <- dget(testthat::test_path("fixtures", "a26-membership-baseline.R"))
  withr::local_options(list(nfilter.subset = 5L, nfilter.tab = 1L))
  for (vendor in names(.ws7f_member_dbs)) local({
    h <- .ws7f_member_dbs[[vendor]]$handle
    key <- .ws7_keys(list(h))
    symbol <- paste0("ws7f_membership_", vendor)
    .setHandle(symbol, h)
    on.exit(rm(list = paste0("handle_", symbol), envir = .dsomop_env), add = TRUE)
    before <- h$temp_tables
    cases <- .ws7f_member_cases()
    for (name in names(cases)) for (mode in c("memory", "staged")) {
      info <- paste(vendor, name, mode)
      expected <- baseline[[name]]
      actual <- .ws7f_member_public(symbol, cases[[name]], mode)
      expect_identical(actual$warnings, expected$warnings, info = info)
      if (!is.null(expected$result$error)) {
        expect_true(inherits(actual$result, "error"), info = info)
        expect_identical(conditionMessage(actual$result), expected$result$error, info = info)
        expect_identical(class(actual$result), expected$result$class, info = info)
        expect_length(actual$bindings, 0L)
      } else {
        expect_identical(actual$result$status, "ok", info = info)
        expect_identical(actual$bindings, "D_members", info = info)
        frame <- .ws7f_member_read(.testPseudonymize(expected$result$members, key))
        expect_identical(actual$frame, frame, info = info)
      }
      expect_setequal(h$temp_tables, before)
    }
  })
})

test_that("A-26 labels preserve dates, empty behavior, warning provenance and native absence", {
  withr::local_options(list(dsomop.allow_absolute_dates = TRUE))
  for (vendor in names(.ws7f_member_dbs)) local({
    h <- .ws7f_member_dbs[[vendor]]$handle
    supplied <- .ws7f_member_roster(h, TRUE)
    on.exit(.ws7_close(supplied$run), add = TRUE)
    empty <- .ws7f_member_roster(h, empty = TRUE)
    on.exit(.ws7_close(empty$run), add = TRUE)
    one <- .ws7f_member_plan(1L, TRUE)
    one$outputs$members$date_handling <- "absolute"
    two <- one
    two$cohort$cohort_definition_id <- 2L
    first <- .ws7f_member_compile(h, one, supplied)$frame
    second <- .ws7f_member_compile(h, two, supplied)$frame
    expect_identical(first$cohort_definition_id, rep(1L, 7L), info = vendor)
    expect_identical(second$cohort_definition_id, rep(2L, 7L), info = vendor)
    second$cohort_definition_id <- first$cohort_definition_id
    expect_identical(second, first, info = vendor)
    expect_identical(first$cohort_start_date, rep(as.Date("2020-01-01"), 7L), info = vendor)
    expect_identical(first$cohort_end_date, rep(as.Date("2024-12-31"), 7L), info = vendor)
    for (id in list(NULL, "foo", 2147483648)) {
      actual <- .ws7f_member_capture(.ws7f_member_compile(h, .ws7f_member_plan(id), empty))
      expect_identical(actual$warnings, character(0), info = vendor)
      expect_identical(nrow(actual$result$frame), 0L, info = vendor)
      expect_identical(actual$result$frame$cohort_definition_id, integer(0), info = vendor)
    }
    # Semantically equivalent null labels retain their original diagnostics;
    # changing only raw provenance cannot change the effective IR label.
    bad <- .ws7f_member_plan("foo", TRUE)
    raw <- bad
    raw$cohort$cohort_definition_id <- 2147483648
    actual <- .ws7f_member_capture(.ws7f_member_compile(h, bad, supplied, raw = raw))
    expect_identical(actual$warnings, "NAs introduced by coercion to integer range", info = vendor)
    expect_identical(actual$result$frame$cohort_definition_id, rep(NA_integer_, 7L), info = vendor)
    raw$cohort$cohort_definition_id <- 7L
    actual <- .ws7f_member_capture(.ws7f_member_compile(h, bad, supplied, raw = raw))
    expect_identical(actual$warnings, character(0), info = vendor)
    expect_identical(actual$result$frame$cohort_definition_id, rep(NA_integer_, 7L), info = vendor)
    native <- .planCompileOutputs(supplied$run$cctx, list(members = "D_members"),
      stats::setNames(list(supplied$pop), supplied$name))$members$components$members$frame
    expect_identical(names(native), c("row_id", "cohort_row_id", "person_id",
      "cohort_start_date", "cohort_end_date", "index_source"), info = vendor)
    expect_false("cohort_definition_id" %in% names(native), info = vendor)
    expect_identical(native$cohort_start_date, rep(0L, 7L), info = vendor)
  })
})
