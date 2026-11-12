# Stateless protection (dsOMOP 2.10.0, contract of 2.10.3): every answer is a
# deterministic function of the secret root, the canonical component and,
# without a custodial snapshot, the realised bounded contributions of that
# component. Only the root persists; nothing depends on earlier questions.

.stateless_environment <- c(
  "DSOMOP_DP_ENABLED", "DSOMOP_DP_DOMAIN", "DSOMOP_DP_SNAPSHOT_ID",
  "DSOMOP_DP_RELEASE_EPSILON", "DSOMOP_DP_PRIVACY_EPOCH",
  "DSOMOP_DP_MAX_LEVELS", "DSOMOP_DP_MAX_CONTRIBUTIONS",
  "DSOMOP_DP_NUMERIC_GRID", "DSOMOP_DP_NOISE_ROOT"
)

# A private state directory and no custodial snapshot, so the DP identity and
# the database version are derived from the connected resource.
.stateless_state <- function(.local_envir = parent.frame()) {
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
    dsomop.dp.noise_root = NULL,
    dsomop.dp.release_epsilon = 0.1, dsomop.dp.privacy_epoch = 1L
  ), .local_envir = .local_envir)
  previous <- list(status = .pkg_state$dp_status,
                   runtime = .pkg_state$dp_runtime,
                   handle = .pkg_state$dp_handle)
  withr::defer({
    .pkg_state$dp_status <- previous$status
    .pkg_state$dp_runtime <- previous$runtime
    .pkg_state$dp_handle <- previous$handle
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

test_that("the DP service reports the stateless protected-release contract", {
  .stateless_state()
  handle <- .stateless_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .dsomopDpEnsureRuntime(handle)
  status <- omopDpStatusDS()
  expect_identical(status$release_binding,
                   "fixed_basis_or_realised_contributions_v1")
  expect_identical(status$persistent_state, "noise_root_only")
  expect_identical(status$service_capacity, "none")
  expect_false(status$history_dependent)
  expect_false(status$history_based_admission)
  expect_identical(status$privacy_contract,
                   "protected_release_fixed_basis_or_realised_v1")
  expect_identical(status$release_protocol, "dsomop-protected-release-v1")
  expect_warning(expect_identical(omopInitializeReleaseStore(), NA_character_),
                 "stores no DP answers")
})

test_that("answers are deterministic across restarts and only the root persists", {
  state <- .stateless_state()
  handle <- .stateless_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  first <- .stateless_release(handle)
  # No custodial snapshot: the heuristic realised-contribution route.
  expect_identical(attr(first, "dsomop_protection")$protection,
                   "realised_sticky")
  .stateless_restart()
  second <- .stateless_release(handle)
  expect_identical(second$n_persons, first$n_persons)
  files <- list.files(state, recursive = TRUE, all.files = TRUE)
  expect_setequal(files, c("secrets/dp_noise_root",
                           "secrets/dp_noise_root.lock"))
  expect_false(dir.exists(file.path(state, "release-bindings")))
})

test_that("a database reload redraws only the components whose people changed", {
  .stateless_state()
  handle <- .stateless_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  before <- .stateless_release(handle)
  # A new woman born in 1990 changes the realised maps of the female total
  # and of her birth-year cell only (dsOMOP 2.10.3: no whole-database data
  # version reseeds every answer).
  DBI::dbExecute(handle$conn, paste(
    "INSERT INTO person VALUES (900001, 8532, 1990, 1, 1, 8527, NULL)"))
  .stateless_restart()
  handle$blueprint <- NULL
  after <- .stateless_release(handle)
  expect_identical(attr(after, "dsomop_protection")$protection,
                   "realised_sticky")
  components <- function(x) {
    owner <- attr(x, "dsomop_release")$owners[[1L]]
    stats::setNames(owner$components,
                    vapply(owner$components, `[[`, character(1L), "stratum"))
  }
  b <- components(before)
  a <- components(after)
  expect_setequal(names(a), names(b))
  # Exported strata are reconciled to the parent person total (which the new
  # person changes), so every stratum shifts uniformly; the other strata keep
  # their own draws: their contrasts and their cells' contrasts are unchanged.
  others <- setdiff(names(b), "gender=8532")
  expect_false(isTRUE(all.equal(a[["gender=8532"]]$n - a[[others[[1L]]]]$n,
                                b[["gender=8532"]]$n - b[[others[[1L]]]]$n)))
  for (k in seq_along(others)[-1L]) {
    i <- others[[k]]; j <- others[[k - 1L]]
    expect_equal(a[[i]]$n - a[[j]]$n, b[[i]]$n - b[[j]]$n, tolerance = 1e-9,
                 info = i)
  }
  for (s in others) {
    expect_equal(diff(a[[s]]$h), diff(b[[s]]$h), tolerance = 1e-9, info = s)
  }
})

test_that("a damaged root fails closed instead of being replaced", {
  state <- .stateless_state()
  handle <- .stateless_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
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
  on.exit(cleanup_handle(handle), add = TRUE)
  first <- .stateless_release(handle)
  .stateless_restart()
  expect_identical(.stateless_release(handle)$n_persons, first$n_persons)
  expect_length(list.files(state, recursive = TRUE, all.files = TRUE), 0L)
})

test_that("seed-only releases need the resource or a custodial snapshot", {
  .stateless_state()
  # Without custodial identifiers the DP identity comes from the connected
  # resource; nothing else (no database version) is read for any key.
  expect_error(.dsomopDpDerivedIdentity(NULL), "Cannot derive DP identity")
  expect_false(exists(".dsomopDpDataVersion", envir = asNamespace("dsOMOP"),
                      inherits = FALSE))
})

test_that("irrelevant tables and rows never redraw a release", {
  .stateless_state()
  handle <- .stateless_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  # A clinical table without its standard identifier is simply irrelevant.
  DBI::dbExecute(handle$conn, "CREATE TABLE cost (cost_event_id INTEGER)")
  before <- .stateless_release(handle)
  DBI::dbExecute(handle$conn, "INSERT INTO cost VALUES (7)")
  DBI::dbExecute(handle$conn, paste(
    "INSERT INTO condition_occurrence (condition_occurrence_id, person_id,",
    "condition_concept_id, condition_start_date, condition_end_date,",
    "condition_type_concept_id) VALUES (990001, 1, 201820, '2020-01-01',",
    "'2020-01-01', 32020)"))
  .stateless_restart()
  handle$blueprint <- NULL
  after <- .stateless_release(handle)
  # Gender x birth-year contributions are unchanged: the same answer.
  expect_identical(after$n_persons, before$n_persons)
})
