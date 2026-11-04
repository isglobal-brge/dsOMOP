# Seed-only (stateless) DP binding, the default since 2.10.0: every answer is a
# deterministic function of the secret root, the question, the protected
# statistic and a private automatic database version. Only the root persists.

.stateless_environment <- c(
  "DSOMOP_DP_ENABLED", "DSOMOP_DP_DOMAIN", "DSOMOP_DP_SNAPSHOT_ID",
  "DSOMOP_DP_RELEASE_EPSILON", "DSOMOP_DP_PRIVACY_EPOCH",
  "DSOMOP_DP_MAX_LEVELS", "DSOMOP_DP_MAX_CONTRIBUTIONS",
  "DSOMOP_DP_NUMERIC_GRID", "DSOMOP_DP_NOISE_ROOT",
  "DSOMOP_DP_RELEASE_STORE_ID", "DSOMOP_DP_RELEASE_STORE_BYTES",
  "DSOMOP_DP_BINDING"
)

# A private state directory and no custodial snapshot, so the DP identity and
# the database version are derived from the connected resource.
.stateless_state <- function(binding = NULL, .local_envir = parent.frame()) {
  state <- withr::local_tempdir(pattern = "dsomop-stateless-",
                                .local_envir = .local_envir)
  Sys.chmod(state, mode = "0700")
  state <- normalizePath(state, winslash = "/", mustWork = TRUE)
  environment <- stats::setNames(
    rep.int(NA_character_, length(.stateless_environment)),
    .stateless_environment
  )
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state, DSOMOP_MARKER_DIR = NA_character_,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1", environment
  ), .local_envir = .local_envir)
  withr::local_options(list(
    dsomop.state_dir = state, dsomop.dp.enabled = TRUE,
    dsomop.dp.domain = NULL, dsomop.dp.snapshot_id = NULL,
    default.dsomop.dp.domain = NULL, default.dsomop.dp.snapshot_id = NULL,
    dsomop.dp.noise_root = NULL, dsomop.dp.binding = binding,
    default.dsomop.dp.binding = NULL, dsomop.dp.release_store_id = "",
    dsomop.dp.release_epsilon = 0.1, dsomop.dp.privacy_epoch = 1L
  ), .local_envir = .local_envir)
  previous <- list(status = .pkg_state$dp_status,
                   runtime = .pkg_state$dp_runtime,
                   handle = .pkg_state$dp_handle,
                   warned = .pkg_state$dp_snapshot_fallback_warned)
  withr::defer({
    .pkg_state$dp_status <- previous$status
    .pkg_state$dp_runtime <- previous$runtime
    .pkg_state$dp_handle <- previous$handle
    .pkg_state$dp_snapshot_fallback_warned <- previous$warned
  }, envir = .local_envir)
  .stateless_restart()
  state
}

# A new R session: the in-memory runtime is gone, the state directory stays.
.stateless_restart <- function() {
  .pkg_state$dp_status <- NULL
  .pkg_state$dp_runtime <- NULL
  .pkg_state$dp_handle <- NULL
  .pkg_state$dp_bootstrap_in_progress <- FALSE
}

# A test CDM whose resource has persistent coordinates (a database file).
.stateless_handle <- function(n_persons = 120) {
  path <- tempfile(fileext = ".sqlite")
  conn <- create_test_omop_db(path, n_persons = n_persons)
  handle <- create_test_handle(n_persons = 1)
  DBI::dbDisconnect(handle$conn)
  handle$conn <- conn
  handle$resource_client <- list(
    getParsed = function() list(database = path, dbms = "sqlite", host = ""),
    getConnection = function() conn
  )
  handle
}

.stateless_release <- function(handle) {
  suppressWarnings(
    .omopAnalysisRun(handle, "dsomop:person.gender_by_age_decade")
  )
}

test_that("seed-only is the default binding and reports its contract", {
  .stateless_state()
  expect_identical(.dsomopDpBindingMode(), "stateless")
  handle <- .stateless_handle()
  on.exit(cleanup_handle(handle))
  .dsomopDpEnsureRuntime(handle)
  status <- omopDpStatusDS()
  expect_identical(status$release_binding, "seed_data_versioned_v1")
  expect_identical(status$persistent_state, "noise_root_only")
  expect_identical(status$service_capacity, "none")
  expect_false(status$history_dependent)
  expect_identical(status$privacy_contract,
                   "fixed_per_release_seed_data_versioned_v1")
  withr::with_options(list(dsomop.dp.binding = "loose"),
                      expect_error(.dsomopDpBindingMode(),
                                   "stateless.*first_answer"))
})

test_that("answers are deterministic across restarts and only the root persists", {
  state <- .stateless_state()
  handle <- .stateless_handle()
  on.exit(cleanup_handle(handle))
  first <- .stateless_release(handle)
  expect_identical(attr(first, "dsomop_protection")$protection, "dp")
  .stateless_restart()
  second <- .stateless_release(handle)
  expect_identical(second$n_persons, first$n_persons)
  files <- list.files(state, recursive = TRUE, all.files = TRUE)
  expect_setequal(files, c("secrets/dp_noise_root",
                           "secrets/dp_noise_root.lock"))
  expect_false(dir.exists(file.path(state, "release-bindings")))
})

test_that("a database reload re-draws every answer at once", {
  .stateless_state()
  handle <- .stateless_handle()
  on.exit(cleanup_handle(handle))
  before <- .stateless_release(handle)
  # A new person anywhere changes the private database version.
  DBI::dbExecute(handle$conn, paste(
    "INSERT INTO person VALUES (900001, 8532, 1990, 1, 1, 8527, NULL)"))
  .stateless_restart()
  handle$blueprint <- NULL
  after <- .stateless_release(handle)
  positive <- before$n_persons > 0 | after$n_persons > 0
  # Cells far from the person also changed: no single statistic stands out.
  expect_gt(mean(before$n_persons[positive] != after$n_persons[positive]), 0.5)
})

test_that("a damaged root fails closed instead of being replaced", {
  state <- .stateless_state()
  handle <- .stateless_handle()
  on.exit(cleanup_handle(handle))
  .stateless_release(handle)
  root <- file.path(state, "secrets", "dp_noise_root")
  writeBin(as.raw(1:5), root)
  .stateless_restart()
  ready <- suppressMessages(.dsomopToolsDpReady(handle))
  expect_false(ready$ready)
  expect_identical(length(readBin(root, "raw", 64L)), 5L)
})

test_that("a custodian-supplied root leaves no state on disk", {
  state <- .stateless_state()
  withr::local_envvar(c(DSOMOP_DP_NOISE_ROOT = strrep("ab", 32L)))
  handle <- .stateless_handle()
  on.exit(cleanup_handle(handle))
  first <- .stateless_release(handle)
  .stateless_restart()
  expect_identical(.stateless_release(handle)$n_persons, first$n_persons)
  expect_length(list.files(state, recursive = TRUE, all.files = TRUE), 0L)
})

test_that("seed-only releases need the resource or a custodial snapshot", {
  .stateless_state()
  expect_error(.dsomopDpDataVersion(NULL, as.raw(1:32)),
               "connected OMOP resource")
  expect_match(.dsomopDpDataVersion(NULL, as.raw(1:32),
                                    custodial_snapshot = TRUE),
               "^[0-9a-f]{64}$")
})

test_that("the first-answer binding keeps its store", {
  .stateless_state(binding = "first_answer")
  handle <- .stateless_handle()
  on.exit(cleanup_handle(handle))
  # Without a custodial snapshot the first-answer binding warns that it
  # cannot follow reloads; the stateless binding has no such limitation.
  suppressWarnings(.dsomopDpEnsureRuntime(handle))
  status <- omopDpStatusDS()
  expect_identical(status$release_binding, "snapshot_first_answer_v1")
  expect_identical(status$persistent_state, "noise_root_and_release_bindings")
  expect_true(status$history_dependent)
})
