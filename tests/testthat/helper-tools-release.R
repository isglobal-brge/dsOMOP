# Shared helpers of the protected-release tests (dsOMOP 2.10.3).

.release_env_names <- c(
  "DSOMOP_DP_ENABLED", "DSOMOP_DP_DOMAIN", "DSOMOP_DP_SNAPSHOT_ID",
  "DSOMOP_DP_RELEASE_EPSILON", "DSOMOP_DP_PRIVACY_EPOCH",
  "DSOMOP_DP_MAX_LEVELS", "DSOMOP_DP_MAX_CONTRIBUTIONS",
  "DSOMOP_DP_NUMERIC_GRID", "DSOMOP_DP_NOISE_ROOT"
)
# A fixed test noise root: protected answers are reproducible test fixtures.
.release_test_root <- paste(rep("5a", 32L), collapse = "")

.release_state <- function(snapshot_id = "release-2026-11-12",
                           enabled = TRUE, mode = "auto",
                           .local_envir = parent.frame()) {
  state <- withr::local_tempdir(pattern = "dsomop-release-",
                                .local_envir = .local_envir)
  Sys.chmod(state, mode = "0700")
  state <- normalizePath(state, winslash = "/", mustWork = TRUE)
  environment <- stats::setNames(
    rep.int(NA_character_, length(.release_env_names)), .release_env_names)
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state, DSOMOP_MARKER_DIR = NA_character_,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1", environment
  ), .local_envir = .local_envir)
  withr::local_options(list(
    dsomop.state_dir = state, dsomop.dp.enabled = enabled,
    dsomop.dp.domain = "dsomop-release-test",
    dsomop.dp.snapshot_id = snapshot_id,
    dsomop.dp.release_epsilon = 0.1, dsomop.dp.privacy_epoch = 1L,
    dsomop.dp.noise_root = .release_test_root,
    dsomop.dp.reference_year = 2026L,
    dsomop.tools.mode = mode,
    nfilter.subset = 3, nfilter.tab = 3, dsomop.nfilter.dist = 10,
    dsomop.nfilter.band = 5
  ), .local_envir = .local_envir)
  .release_restart()
  withr::defer(.release_restart(), envir = .local_envir)
  state
}

# A connected test CDM registered as this session's OMOP handle for the
# calling test only. Returns the dataset identity of that handle.
.release_session_dataset <- function(.local_envir = parent.frame()) {
  h <- create_test_handle(n_persons = 5)
  h$person_key_identity <- "dsomop-test-session"
  key <- "handle_dsomop_test_session"
  assign(key, h, envir = .dsomop_env)
  withr::defer({
    if (exists(key, envir = .dsomop_env, inherits = FALSE)) {
      rm(list = key, envir = .dsomop_env)
    }
    if (identical(.pkg_state$dp_handle, h)) .pkg_state$dp_handle <- NULL
    cleanup_handle(h)
  }, envir = .local_envir)
  .dsomopDpDatasetIdentity(h)
}

# Current source generation for a pinned loader fixture. The caller registers
# the matching source with .release_session_dataset before sealing its table.
.release_loader_generation <- function(label = "fixture") {
  handle <- .dsomopDpState()$dp_handle %||% .dsomopDpSessionHandle()
  .dsomopWithReadSnapshot(handle, .dsomopDpDataGeneration(
    handle, .dsomopDpPolicy())$id)
}

# Test device: answer unscoped fixed-basis questions on the realised route
# (production computes the generation in every fixed request, or fails it).
.release_without_generation <- function(.local_envir = parent.frame()) {
  testthat::local_mocked_bindings(
    .dsomopDpDataGeneration = function(handle, policy) {
      list(available = FALSE, id = NA_character_)
    }, .env = .local_envir)
}

.release_restart <- function() {
  .pkg_state$dp_status <- NULL
  .pkg_state$dp_runtime <- NULL
  .pkg_state$dp_bootstrap_in_progress <- FALSE
  invisible(NULL)
}

# A handle whose resource has persistent coordinates, so a DP identity can be
# derived without a custodial snapshot (the realised-contribution route for
# unscoped questions).
.release_derived_handle <- function(n_persons = 60) {
  h <- create_test_handle(n_persons = n_persons)
  h$resource_client <- list(getParsed = function() list(
    dbms = "postgresql", host = "db.example", port = 5432L,
    database = "release", user = "ignored", password = "ignored"
  ), getConnection = function() h$conn)
  DBI::dbExecute(h$conn, "ALTER TABLE cdm_source ADD COLUMN cdm_release_date TEXT")
  DBI::dbExecute(h$conn, "UPDATE cdm_source SET cdm_release_date = '2026-08-01'")
  h
}

.release_run <- function(h, id, params = list(), scope = NULL) {
  suppressWarnings(.omopAnalysisRun(h, id, params, scope = scope))
}

# A temporary cohort holding exactly the given persons.
.release_cohort <- function(h, name, person_ids) {
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE ", name, " (cohort_definition_id INTEGER, ",
    "subject_id INTEGER, cohort_start_date TEXT, cohort_end_date TEXT)"))
  DBI::dbWriteTable(h$conn, name, data.frame(
    cohort_definition_id = 1L, subject_id = as.integer(person_ids),
    cohort_start_date = "2015-01-01", cohort_end_date = "2016-01-01"),
    append = TRUE)
  register_test_temp(h, name)
  name
}

# Insert one condition record (an outcome event) for a person.
.release_add_condition <- local({
  next_id <- 950000L
  function(h, person_id, concept_id, date) {
    for (p in person_id) {
      next_id <<- next_id + 1L
      DBI::dbExecute(h$conn, sprintf(paste0(
        "INSERT INTO condition_occurrence (condition_occurrence_id, ",
        "person_id, condition_concept_id, condition_start_date, ",
        "condition_end_date, condition_type_concept_id) VALUES ",
        "(%d, %d, %d, '%s', '%s', 44818518)"), next_id, as.integer(p),
        as.integer(concept_id), date, date))
    }
    invisible(NULL)
  }
})

# A cohort table with explicit index and end dates.
.release_dated_cohort <- function(h, name, person_ids, start = "2020-06-01",
                                  end = "2023-12-31") {
  DBI::dbExecute(h$conn, sprintf(paste0(
    "CREATE TEMP TABLE %s AS SELECT person_id AS subject_id, '%s' AS ",
    "cohort_start_date, '%s' AS cohort_end_date FROM person WHERE ",
    "person_id IN (%s)"), name, start, end,
    paste(as.integer(person_ids), collapse = ",")))
  register_test_temp(h, name)
  name
}

# The released values of a frame, without its label and payload attributes
# (which echo the requested parameters).
.release_values <- function(df) {
  attributes(df) <- attributes(df)[c("names", "row.names", "class")]
  df
}
