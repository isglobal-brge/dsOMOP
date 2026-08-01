# Test Helper: Create a test handle backed by SQLite
# Sources the shared fixture and builds a handle for testing.

library(dsOMOP)

# Deterministic 32-byte material and its matching public contract for focused
# tests of person-bearing outputs. Production code never receives this helper.
.testPseudonymKey <- function(label = "dsomop-test-key") {
  as.raw(openssl::sha256(charToRaw(label)))
}

.testPublicPseudonymization <- function(key = .testPseudonymKey(), epoch = 1L,
                                        provider = "scoped") {
  list(
    available = TRUE,
    contract_version = 1L,
    provider = provider,
    key_id = dsOMOP:::.personKeyId(key),
    epoch = epoch
  )
}

.testPseudonymize <- function(x, key = .testPseudonymKey(), epoch = 1L) {
  dsOMOP:::.pseudonymizeIdentifiers(
    x, key,
    pseudonymization = .testPublicPseudonymization(key, epoch = epoch)
  )
}

# Restore the process settings used by a historical raw-key test handle.  The
# explicit handle-owned snapshot is needed in addition to withr's deferred
# cleanup because helper factories can otherwise register their defer on the
# test-file frame rather than the individual test frame.
.restoreLegacyTestPersonKey <- function(handle) {
  state <- handle$.test_legacy_person_key_state
  if (is.null(state)) return(invisible(FALSE))
  handle$.test_legacy_person_key_state <- NULL

  if (is.na(state$environment)) {
    Sys.unsetenv("DSOMOP_ALLOW_LEGACY_GLOBAL_PSEUDONYMS")
  } else {
    Sys.setenv(DSOMOP_ALLOW_LEGACY_GLOBAL_PSEUDONYMS = state$environment)
  }
  options(state$options)
  invisible(TRUE)
}

# Explicitly scoped compatibility fixture for tests that exercise historical
# raw-key handles. Every caller must pass its test evaluation environment; the
# handle cleanup is the primary restoration boundary and the deferred cleanup
# is a fail-safe for tests that abort before closing their fixture.
.setLegacyTestPersonKey <- function(handle, label,
                                    .local_envir) {
  if (missing(.local_envir) || !is.environment(.local_envir)) {
    stop("Legacy test-key opt-in requires an explicit local environment.")
  }
  if (is.null(handle$.test_legacy_person_key_state)) {
    option_names <- c(
      "dsomop.allow_legacy_global_pseudonyms",
      "default.dsomop.allow_legacy_global_pseudonyms"
    )
    old_options <- lapply(option_names, getOption)
    names(old_options) <- option_names
    handle$.test_legacy_person_key_state <- list(
      environment = Sys.getenv(
        "DSOMOP_ALLOW_LEGACY_GLOBAL_PSEUDONYMS", unset = NA_character_
      ),
      options = old_options
    )
    withr::defer(
      .restoreLegacyTestPersonKey(handle),
      envir = .local_envir
    )
  }
  Sys.setenv(DSOMOP_ALLOW_LEGACY_GLOBAL_PSEUDONYMS = "true")
  options(
    dsomop.allow_legacy_global_pseudonyms = TRUE,
    default.dsomop.allow_legacy_global_pseudonyms = TRUE
  )
  handle$person_key <- .testPseudonymKey(label)
  invisible(handle$person_key)
}

# Source the test fixture
# Resolve the package's OWN tracked fixture (dsOMOP/tests/fixtures/), package-
# rooted via test_path so it works under devtools::test / R CMD check / a fresh
# clone. Self-contained: never climbs out of the package to a shared parent.
tp <- tryCatch(testthat::test_path(), error = function(e) NULL)
fixture_candidates <- c(
  if (!is.null(tp)) file.path(tp, "..", "fixtures", "create_test_db.R"),
  file.path("tests", "fixtures", "create_test_db.R"),
  file.path("..", "fixtures", "create_test_db.R")
)
fixture_path <- NULL
for (fp in fixture_candidates) {
  if (file.exists(fp)) { fixture_path <- fp; break }
}
if (is.null(fixture_path)) {
  stop("Could not find create_test_db.R fixture. Tried:\n",
       paste(fixture_candidates, collapse = "\n"))
}
source(fixture_path, local = TRUE)

# Create a test handle for use in tests
create_test_handle <- function(n_persons = 15) {
  conn <- create_test_omop_db(n_persons = n_persons)

  handle <- new.env(parent = emptyenv())
  handle$conn            <- conn
  handle$dbms            <- "sqlite"
  handle$target_dialect  <- "sqlite"
  handle$cdm_schema      <- NULL
  handle$vocab_schema    <- NULL
  handle$results_schema  <- NULL
  handle$temp_schema     <- NULL
  handle$resource_client <- NULL
  handle$config          <- list()
  handle$blueprint       <- NULL
  handle$temp_tables     <- character(0)
  handle$temp_connection <- NULL

  handle
}

# Create a v5.3 test handle
create_test_handle_v53 <- function(n_persons = 15) {
  conn <- create_test_omop_db_v53(n_persons = n_persons)

  handle <- new.env(parent = emptyenv())
  handle$conn            <- conn
  handle$dbms            <- "sqlite"
  handle$target_dialect  <- "sqlite"
  handle$cdm_schema      <- NULL
  handle$vocab_schema    <- NULL
  handle$results_schema  <- NULL
  handle$temp_schema     <- NULL
  handle$resource_client <- NULL
  handle$config          <- list()
  handle$blueprint       <- NULL
  handle$temp_tables     <- character(0)
  handle$temp_connection <- NULL

  handle
}

# Create a test handle with no cdm_source table
create_test_handle_no_source <- function(n_persons = 15) {
  conn <- create_test_omop_db(n_persons = n_persons)
  DBI::dbExecute(conn, "DROP TABLE IF EXISTS cdm_source")

  handle <- new.env(parent = emptyenv())
  handle$conn            <- conn
  handle$dbms            <- "sqlite"
  handle$target_dialect  <- "sqlite"
  handle$cdm_schema      <- NULL
  handle$vocab_schema    <- NULL
  handle$results_schema  <- NULL
  handle$temp_schema     <- NULL
  handle$resource_client <- NULL
  handle$config          <- list()
  handle$blueprint       <- NULL
  handle$temp_tables     <- character(0)
  handle$temp_connection <- NULL

  handle
}

# Create a test handle where cdm_source version disagrees with structure
create_test_handle_mismatch <- function(n_persons = 15) {
  # Create a v5.4-style DB (default) but set cdm_source to say v5.3
  conn <- create_test_omop_db(n_persons = n_persons)
  DBI::dbExecute(conn, "DELETE FROM cdm_source")
  DBI::dbExecute(conn, "INSERT INTO cdm_source VALUES ('Mismatch Test', 'MIS', 'Test', 'Mismatch test', 'v5.3', 'v5.0')")

  handle <- new.env(parent = emptyenv())
  handle$conn            <- conn
  handle$dbms            <- "sqlite"
  handle$target_dialect  <- "sqlite"
  handle$cdm_schema      <- NULL
  handle$vocab_schema    <- NULL
  handle$results_schema  <- NULL
  handle$temp_schema     <- NULL
  handle$resource_client <- NULL
  handle$config          <- list()
  handle$blueprint       <- NULL
  handle$temp_tables     <- character(0)
  handle$temp_connection <- NULL

  handle
}

# Register fixture-created TEMP tables under the same ownership contract as
# .createTempTable(). Tests that bypass the production constructor must still
# model the physical connection that owns the object.
register_test_temp <- function(handle, names) {
  handle$temp_tables <- union(handle$temp_tables, as.character(names))
  handle$temp_connection <- handle$conn
  invisible(names)
}

# Cleanup helper
cleanup_handle <- function(handle) {
  on.exit(.restoreLegacyTestPersonKey(handle), add = TRUE)
  if (!is.null(handle$conn) && DBI::dbIsValid(handle$conn)) {
    DBI::dbDisconnect(handle$conn)
  }
}
