# Test Helper: Create a test handle backed by SQLite
# Sources the shared fixture and builds a handle for testing.

library(dsOMOP)

# Source the test fixture
tp <- tryCatch(testthat::test_path(), error = function(e) NULL)
fixture_candidates <- c(
  if (!is.null(tp)) file.path(tp, "..", "..", "..", "tests", "fixtures", "create_test_db.R"),
  file.path("tests", "fixtures", "create_test_db.R"),
  file.path("..", "tests", "fixtures", "create_test_db.R"),
  file.path("..", "..", "tests", "fixtures", "create_test_db.R")
)
fixture_path <- NULL
for (fp in fixture_candidates) {
  if (file.exists(fp)) { fixture_path <- fp; break }
}
if (is.null(fixture_path)) {
  stop("Could not find create_test_db.R fixture. Tried:\n",
       paste(fixture_candidates, collapse = "\n"))
}
source(fixture_path)

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

  handle
}

# Cleanup helper
cleanup_handle <- function(handle) {
  if (!is.null(handle$conn) && DBI::dbIsValid(handle$conn)) {
    DBI::dbDisconnect(handle$conn)
  }
}
