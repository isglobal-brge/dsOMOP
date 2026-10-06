.binding_fixture <- function(.local_envir = parent.frame()) {
  skip_on_os("windows")
  state <- withr::local_tempdir("dsomop-binding-", .local_envir = .local_envir)
  Sys.chmod(state, "0700")
  state <- normalizePath(state, mustWork = TRUE)
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state, DSOMOP_MARKER_DIR = NA_character_,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1", DSOMOP_DP_NOISE_ROOT = NA_character_,
    DSOMOP_DP_RELEASE_STORE_ID = NA_character_,
    DSOMOP_DP_RELEASE_STORE_BYTES = NA_character_
  ), .local_envir = .local_envir)
  withr::local_options(list(
    dsomop.state_dir = state, dsomop.dp.noise_root = NULL,
    default.dsomop.dp.noise_root = NULL,
    dsomop.dp.release_store_id = "", dsomop.dp.release_store_bytes = 2^30
  ), .local_envir = .local_envir)
  id <- omopInitializeReleaseStore()
  withr::local_options(list(dsomop.dp.release_store_id = id),
                       .local_envir = .local_envir)
  list(state = state, id = id, policy = list(noise_root = .dsomopDpNoiseRoot()),
       database = file.path(state, "release-bindings", "releases.sqlite"))
}

.binding_id <- function(letter = "a") paste(rep(letter, 64L), collapse = "")
.binding_charge <- 65536
.binding_bytes <- function(value) serialize(value, NULL, version = 3L)
.binding_get <- function(fixture, id = .binding_id(), fn = function() list(value = 1)) {
  .dsomopDpBindingRelease(fixture$policy, id, .binding_charge, fn)
}
.binding_sql <- function(fixture, sql, params = NULL) {
  connection <- DBI::dbConnect(RSQLite::SQLite(), fixture$database)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  if (startsWith(sql, "SELECT")) {
    DBI::dbGetQuery(connection, sql, params = params)
  } else DBI::dbExecute(connection, sql, params = params)
}

test_that("release store initialization is explicit, idempotent, and private", {
  fixture <- .binding_fixture()
  expect_identical(omopInitializeReleaseStore(), fixture$id)
  expect_identical(.dsomopDpBindingIdentity(),
                   list(store_id = fixture$id, state_root = fixture$state))
  expect_true(.dsomopDpBindingCheck(fixture$policy))
  paths <- list.files(fixture$state, recursive = TRUE, full.names = TRUE,
                      all.files = TRUE, include.dirs = TRUE, no.. = TRUE)
  info <- file.info(paths)
  expect_true(all(as.integer(info$mode[info$isdir]) == strtoi("700", 8L)))
  expect_true(all(as.integer(info$mode[!info$isdir]) == strtoi("600", 8L)))
  expect_equal(.binding_sql(fixture, "SELECT count(*) AS n FROM bindings")$n, 0)

  withr::local_options(dsomop.dp.release_store_id = "")
  expect_error(suppressMessages(.binding_get(fixture)), .DSOMOP_DP_STATE_ERROR,
               fixed = TRUE)
  expect_identical(omopInitializeReleaseStore(), fixture$id)
})

test_that("a permanent answer is decoded identically without executing another payload", {
  fixture <- .binding_fixture()
  executions <- 0L
  make <- function() {
    executions <<- executions + 1L
    list(count = executions, labels = c("alpha", "é"), metadata = list(sticky = TRUE))
  }
  first <- .binding_get(fixture, fn = make)
  replay <- .binding_get(fixture, fn = function() stop("must not execute"))
  second <- .binding_get(fixture, .binding_id("b"), make)
  expect_identical(.binding_bytes(replay), .binding_bytes(first))
  expect_identical(executions, 2L)
  expect_identical(second$count, 2L)
  raw <- .binding_sql(fixture,
    "SELECT payload FROM bindings WHERE request_id = ?", list(.binding_id()))$payload[[1L]]
  expect_identical(raw, .binding_bytes(first))
  expect_equal(.binding_sql(fixture, "SELECT count(*) AS n FROM bindings")$n, 2)
})

test_that("public reservations refuse new identities and preserve existing reads", {
  fixture <- .binding_fixture()
  withr::local_options(dsomop.dp.release_store_bytes = .binding_charge)
  first <- .binding_get(fixture)
  expect_error(.binding_get(fixture, .binding_id("b"),
                            function() stop("must not execute")), "public reservation capacity")
  expect_false(file.exists(file.path(fixture$state, "release-bindings", "locks",
                                     paste0(.binding_id("b"), ".lock"))))
  withr::local_options(dsomop.dp.release_store_bytes = 0)
  expect_identical(.binding_bytes(.binding_get(fixture)), .binding_bytes(first))
  withr::local_options(dsomop.dp.release_store_bytes = 2 * .binding_charge)
  expect_identical(.binding_get(fixture, .binding_id("b"))$value, 1)
  expect_identical(.binding_bytes(.binding_get(fixture)), .binding_bytes(first))
})

test_that("environment store settings override defaults and reject explicit conflicts", {
  fixture <- .binding_fixture()
  withr::local_options(list(
    dsomop.dp.release_store_id = NULL, dsomop.dp.release_store_bytes = NULL,
    default.dsomop.dp.release_store_id = "",
    default.dsomop.dp.release_store_bytes = 2^30
  ))
  withr::local_envvar(c(DSOMOP_DP_RELEASE_STORE_ID = fixture$id,
                       DSOMOP_DP_RELEASE_STORE_BYTES = as.character(.binding_charge)))
  expect_identical(.dsomopDpBindingSettings(),
                   list(store_id = fixture$id, capacity = .binding_charge))
  first <- .binding_get(fixture)
  expect_error(.binding_get(fixture, .binding_id("b")), "public reservation capacity")
  expect_identical(.binding_get(fixture), first)
  withr::local_options(dsomop.dp.release_store_bytes = 2 * .binding_charge)
  expect_error(.dsomopDpBindingSettings(), "Conflicting release state settings")
  withr::local_options(list(dsomop.dp.release_store_bytes = NULL,
    dsomop.dp.release_store_id = "00000000-0000-0000-0000-000000000000"))
  expect_error(.dsomopDpBindingSettings(), "Conflicting release state settings")
})

test_that("failed payloads retain their reservation without publishing a response", {
  fixture <- .binding_fixture()
  expect_error(.binding_get(fixture, fn = function() stop("interrupted before publication")),
               "interrupted before publication")
  row <- .binding_sql(fixture, "SELECT * FROM bindings")
  expect_identical(row$state, "pending")
  expect_identical(row$payload[[1L]], raw())
  withr::local_options(dsomop.dp.release_store_bytes = 0)
  first <- .binding_get(fixture, fn = function() list(value = 17L))
  expect_identical(first$value, 17L)
  expect_identical(.binding_get(fixture), first)
  expect_equal(.binding_sql(fixture,
    "SELECT sum(reservation_bytes) AS n FROM bindings")$n, .binding_charge)
})

test_that("a failure after commit but before return replays the committed answer", {
  fixture <- .binding_fixture()
  local({
    testthat::local_mocked_bindings(
      .dsomopDpBindingDecode = function(bytes) stop("interrupted before send"),
      .package = "dsOMOP")
    expect_error(suppressMessages(.binding_get(fixture,
      fn = function() list(value = 43L))), .DSOMOP_DP_STATE_ERROR, fixed = TRUE)
  })
  expect_identical(.binding_sql(fixture, "SELECT state FROM bindings")$state, "committed")
  expect_identical(.binding_get(fixture, fn = function() stop("must not execute")),
                   list(value = 43L))
})

test_that("whole authentic snapshot rollback is an explicit operational residual", {
  fixture <- .binding_fixture()
  first <- .binding_get(fixture)
  backup <- withr::local_tempdir()
  store <- file.path(fixture$state, "release-bindings")
  # No workers run during backup/restore. A matching older header and inventory
  # are authentic: without an independent monotonic witness they cannot prove
  # that a later committed answer ever existed. Custodians must prevent rollback.
  expect_true(file.copy(store, backup, recursive = TRUE, copy.mode = TRUE))
  second <- .binding_get(fixture, .binding_id("b"), function() list(value = 2L))
  unlink(store, recursive = TRUE)
  expect_true(file.copy(file.path(backup, "release-bindings"), fixture$state,
                         recursive = TRUE, copy.mode = TRUE))
  expect_true(.dsomopDpBindingCheck(fixture$policy))
  expect_identical(.binding_get(fixture), first)
  expect_equal(.binding_sql(fixture, "SELECT count(*) AS n FROM bindings")$n, 1)
  after_rollback <- .binding_get(fixture, .binding_id("b"), function() list(value = 99L))
  expect_false(identical(.binding_bytes(after_rollback), .binding_bytes(second)))
})

test_that("corrupt response bytes are authenticated before decoding", {
  fixture <- .binding_fixture()
  .binding_get(fixture)
  .binding_sql(fixture, "UPDATE bindings SET payload = ?",
               list(list(charToRaw("not a serialized R value"))))
  expect_message(
    expect_error(.binding_get(fixture, fn = function() stop("must not execute")),
                 .DSOMOP_DP_STATE_ERROR, fixed = TRUE),
    "Custodian recovery required:.*authentication")
})

test_that("authenticated inventory rejects a deleted valid row", {
  fixture <- .binding_fixture()
  .binding_get(fixture)
  .binding_get(fixture, .binding_id("b"))
  .binding_sql(fixture, "DELETE FROM bindings WHERE request_id = ?", list(.binding_id()))
  expect_error(suppressMessages(.binding_get(fixture, .binding_id("b"))),
               .DSOMOP_DP_STATE_ERROR, fixed = TRUE)
  expect_error(suppressMessages(.binding_get(fixture)),
               .DSOMOP_DP_STATE_ERROR, fixed = TRUE)
})

test_that("missing established store and missing permanent lock never regenerate", {
  fixture <- .binding_fixture()
  .binding_get(fixture)
  path <- file.path(fixture$state, "release-bindings", "locks",
                    paste0(.binding_id(), ".lock"))
  unlink(path)
  expect_error(suppressMessages(.binding_get(fixture)), .DSOMOP_DP_STATE_ERROR,
               fixed = TRUE)
  expect_false(file.exists(path))
  unlink(file.path(fixture$state, "release-bindings"), recursive = TRUE)
  expect_error(suppressMessages(.binding_get(fixture)), .DSOMOP_DP_STATE_ERROR,
               fixed = TRUE)
  expect_error(suppressMessages(omopInitializeReleaseStore()), .DSOMOP_DP_STATE_ERROR,
               fixed = TRUE)
  expect_false(dir.exists(file.path(fixture$state, "release-bindings")))
  unlink(fixture$state, recursive = TRUE)
  expect_error(suppressMessages(omopInitializeReleaseStore()), .DSOMOP_DP_STATE_ERROR,
               fixed = TRUE)
  expect_false(dir.exists(fixture$state))
})

test_that("missing and changed roots fail even with a cached policy", {
  fixture <- .binding_fixture()
  .binding_get(fixture)
  root_path <- file.path(fixture$state, "secrets", "dp_noise_root")
  old_root <- readBin(root_path, "raw", 32L)
  writeBin(as.raw((as.integer(old_root) + 1L) %% 256L), root_path)
  expect_error(suppressMessages(.binding_get(fixture)), .DSOMOP_DP_STATE_ERROR,
               fixed = TRUE)
  unlink(root_path)
  expect_error(suppressMessages(.binding_get(fixture)), .DSOMOP_DP_STATE_ERROR,
               fixed = TRUE)
  expect_false(file.exists(root_path))
})

test_that("unsafe database paths and runtime store changes fail closed", {
  fixture <- .binding_fixture()
  .binding_get(fixture)
  policy <- fixture$policy
  policy$binding_store_id <- fixture$id
  policy$binding_state_root <- paste0(fixture$state, "-different")
  expect_error(suppressMessages(.dsomopDpBindingCheck(policy)),
               .DSOMOP_DP_STATE_ERROR, fixed = TRUE)
  policy$binding_state_root <- fixture$state
  policy$binding_store_id <- "00000000-0000-0000-0000-000000000000"
  expect_error(suppressMessages(.dsomopDpBindingCheck(policy)),
               .DSOMOP_DP_STATE_ERROR, fixed = TRUE)
  Sys.chmod(fixture$database, "0644")
  expect_error(suppressMessages(.binding_get(fixture)), .DSOMOP_DP_STATE_ERROR,
               fixed = TRUE)
  Sys.chmod(fixture$database, "0600")
  other <- paste0(fixture$database, ".original")
  expect_true(file.rename(fixture$database, other))
  expect_true(file.symlink(other, fixture$database))
  expect_error(suppressMessages(.binding_get(fixture)), .DSOMOP_DP_STATE_ERROR,
               fixed = TRUE)
})

.binding_worker_script <- function(directory) {
  files <- testthat::test_path("..", "..", "R",
    c("state_secrets.R", "privacy_state.R", "privacy_release_bindings.R"))
  load_code <- if (all(file.exists(files))) {
    files <- normalizePath(files, mustWork = TRUE)
    sprintf("source(%s)", vapply(files, encodeString, character(1L), quote = '"'))
  } else c(
    "suppressPackageStartupMessages(library(dsOMOP))",
    ".dsomopDpNoiseRoot <- getFromNamespace('.dsomopDpNoiseRoot', 'dsOMOP')",
    ".dsomopDpBindingRelease <- getFromNamespace('.dsomopDpBindingRelease', 'dsOMOP')"
  )
  path <- file.path(directory, "binding-worker.R")
  writeLines(c(
    "args <- commandArgs(TRUE)",
    "`%||%` <- function(x, y) if (is.null(x)) y else x",
    "Sys.setenv(DSOMOP_STATE_DIR = args[[1L]], DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = '1')",
    "options(dsomop.state_dir = args[[1L]], dsomop.dp.release_store_id = args[[2L]])",
    load_code,
    "policy <- list(noise_root = .dsomopDpNoiseRoot())",
    "fn <- if (args[[3L]] == 'crash') function() quit(save = 'no', status = 17L, runLast = FALSE) else function() stop('payload reran after restart')",
    sprintf("value <- .dsomopDpBindingRelease(policy, '%s', %s, fn)",
            .binding_id(), .binding_charge),
    "writeBin(serialize(value, NULL, version = 3L), args[[4L]])"
  ), path)
  path
}

test_that("fresh R sessions replay bytes and recover a process dying before commit", {
  fixture <- .binding_fixture()
  directory <- withr::local_tempdir()
  script <- .binding_worker_script(directory)
  output <- file.path(directory, "response.bin")
  log <- file.path(directory, "worker.log")
  run <- function(mode) suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    shQuote(c(script, fixture$state, fixture$id, mode, output)), stdout = log, stderr = log))
  expect_identical(run("crash"), 17L)
  expect_false(file.exists(output))
  expect_identical(.binding_sql(fixture, "SELECT state FROM bindings")$state, "pending")
  first <- .binding_get(fixture, fn = function() list(value = 29L, stable = "first answer"))
  expect_identical(run("replay"), 0L, info = paste(readLines(log), collapse = "\n"))
  expect_identical(readBin(output, "raw", file.info(output)$size), .binding_bytes(first))
  expect_identical(.binding_sql(fixture, "SELECT state FROM bindings")$state, "committed")
})

test_that("two processes with different candidate payloads publish exactly one answer", {
  fixture <- .binding_fixture()
  counter <- tempfile("executions-")
  on.exit(unlink(counter), add = TRUE)
  values <- parallel::mclapply(1:2, function(candidate) {
    .binding_bytes(.binding_get(fixture, fn = function() {
      cat(candidate, "\n", file = counter, append = TRUE)
      Sys.sleep(0.1)
      list(candidate = candidate, count = candidate * 100L)
    }))
  }, mc.cores = 2L, mc.preschedule = FALSE)
  expect_true(all(vapply(values, is.raw, logical(1L))))
  expect_identical(values[[1L]], values[[2L]])
  expect_length(readLines(counter), 1L)
  expect_equal(.binding_sql(fixture, "SELECT count(*) AS n FROM bindings")$n, 1)
})
