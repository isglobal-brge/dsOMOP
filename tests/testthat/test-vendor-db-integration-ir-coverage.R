# The v1 unique/drop/drop coverage default must execute on every live vendor.
.expectLegacyIndexCoverage <- function(dbms) {
  config <- .dsomopVendorTestConfig(dbms)
  skip_if(is.null(config), paste("Vendor integration is not configured for", dbms))
  fixture <- .dsomopVendorCreateFixture(config)
  on.exit(fixture$cleanup(), add = TRUE)
  handle <- fixture$handle
  cdm <- fixture$schemas[["cdm"]]
  DBI::dbExecute(fixture$admin, paste0(
    "INSERT INTO ", cdm, ".observation_period VALUES ",
    "(4, 1, '2019-12-01', '2020-01-15', 44814724)"
  ))
  plan <- list(populations = list(study = list(kind = "criteria",
    index_event = list(table = "condition_occurrence", concept_set = 201820L,
                       primary_limit = "first"))))
  bp <- .buildBlueprint(handle)
  withr::local_options(list(nfilter.subset = 3))
  read_index <- function() {
    resolved <- .planResolvePopulations(handle, plan, bp)$study
    .executeQuery(handle, paste0("SELECT subject_id, index_event_id FROM ",
      resolved$cohort_table, " ORDER BY subject_id"))
  }
  rows <- read_index()
  expect_equal(as.integer(rows$subject_id), 1:3)
  expect_equal(as.integer(rows$index_event_id), c(2L, 3L, 4L))

  # A missing period removes only its candidate. The resulting private
  # two-person population is legal; its requested release still fails k = 3.
  DBI::dbExecute(fixture$admin, paste0("DELETE FROM ", cdm,
                                     ".observation_period WHERE person_id = 3"))
  rows <- read_index()
  expect_equal(as.integer(rows$subject_id), 1:2)
  expect_equal(as.integer(rows$index_event_id), c(2L, 3L))
  expect_error(.publishStandalone(rows, handle = handle,
    caller = "legacy-index-coverage-test", already_tokenized = FALSE),
    "insufficient individuals")
}

test_that("legacy index coverage drops ambiguous candidates on PostgreSQL", {
  .expectLegacyIndexCoverage("postgresql")
})

test_that("legacy index coverage drops ambiguous candidates on MySQL", {
  .expectLegacyIndexCoverage("mysql")
})

test_that("legacy index coverage drops ambiguous candidates on MariaDB", {
  .expectLegacyIndexCoverage("mariadb")
})
