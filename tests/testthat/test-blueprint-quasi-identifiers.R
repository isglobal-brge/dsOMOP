test_that("quasi-identifier reporting leaves every aggregate flag unchanged", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  bp <- .buildBlueprint(handle, force = TRUE)
  reported <- bp$columns

  for (table in names(reported)) {
    columns <- reported[[table]]
    expected <- vapply(seq_len(nrow(columns)), function(i) {
      .assignmentQuasiIdentifier(columns$column_name[[i]]) &&
        !.assignmentBlockedColumn(columns$column_name[[i]], columns[i, , drop = FALSE])
    }, logical(1))
    expect_identical(columns$is_quasi_identifier, expected, info = table)
  }
  person <- reported$person
  birth <- person$column_name %in% c("year_of_birth", "month_of_birth", "day_of_birth")
  expect_true(any(birth))
  expect_true(all(person$is_quasi_identifier[birth]))
  expect_true(all(person$is_sensitive[birth]))
  expect_true(all(person$is_blocked[birth]))
  expect_false(person$is_quasi_identifier[person$column_name == "gender_concept_id"])
  measurement <- reported$measurement
  date <- measurement$column_name == "measurement_date"
  expect_true(measurement$is_quasi_identifier[date])
  expect_false(measurement$is_sensitive[date])
  expect_false(measurement$is_blocked[date])
  expect_error(.profileColumnStats(handle, "person", "year_of_birth"), "blocked")

  testthat::local_mocked_bindings(
    .assignmentQuasiIdentifier = function(column) FALSE, .package = "dsOMOP")
  unreported <- .buildBlueprint(handle, force = TRUE)$columns
  without_reporting <- function(columns) {
    lapply(columns, function(x) x[, names(x) != "is_quasi_identifier", drop = FALSE])
  }
  expect_identical(without_reporting(reported), without_reporting(unreported))
  expect_error(.profileColumnStats(handle, "person", "year_of_birth"), "blocked")
})

test_that("quasi-identifier metadata never overrides independent class-1 matches", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  DBI::dbExecute(handle$conn, paste(
    "CREATE TABLE person_ext (person_id INTEGER, birth_date DATE,",
    "birth_date_source_value TEXT, mrn_birth_date DATE, member_key INTEGER)"))
  withr::local_options(list(dsomop.allowed_cdm_extensions = list(
    person_ext = c("person_id", "birth_date", "birth_date_source_value",
                   "mrn_birth_date", "member_key"))))
  cols <- .buildBlueprint(handle, force = TRUE)$columns$person_ext
  expect_true(cols$is_quasi_identifier[cols$column_name == "birth_date"])
  expect_true(cols$is_sensitive[cols$column_name == "birth_date"])
  expect_true(cols$is_blocked[cols$column_name == "birth_date"])
  blocked <- cols$column_name %in% c("birth_date_source_value", "mrn_birth_date", "member_key")
  expect_true(all(cols$is_blocked[blocked]))
  expect_false(any(cols$is_quasi_identifier[blocked]))
})
