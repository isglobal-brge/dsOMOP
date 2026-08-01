# Cohort disclosure hardening: a cohort with < nfilter_subset distinct subjects
# is INVISIBLE + INACCESSIBLE (never listed, definition indistinguishable from a
# nonexistent id), and survivors' sizes are banded.

.setup_cohorts <- function(handle) {
  mk <- function(id, n) data.frame(
    cohort_definition_id = rep(id, n), subject_id = seq_len(n) * 100L + id,
    cohort_start_date = rep("2020-01-01", n), cohort_end_date = rep("2020-12-31", n),
    stringsAsFactors = FALSE)
  DBI::dbWriteTable(handle$conn, "cohort",
    rbind(mk(101L, 2L), mk(102L, 8L), mk(103L, 40L), mk(104L, 3L)),
    overwrite = TRUE)
  DBI::dbWriteTable(handle$conn, "cohort_definition", data.frame(
    cohort_definition_id = c(101L, 102L, 103L, 104L),
    cohort_definition_name = c(
      "TINY rare-disease", "Medium", "Large", "At threshold"
    ),
    stringsAsFactors = FALSE), overwrite = TRUE)
}

test_that("cohort.list omits sub-threshold cohorts and reports banded lower bounds", {
  h <- create_test_handle(); .setup_cohorts(h)
  on.exit(cleanup_handle(h), add = TRUE)
  band <- 5L
  lst <- withr::with_options(
    list(nfilter.subset = 3, dsomop.nfilter.band = band),
    dsOMOP:::.cohortList(h)
  )
  expect_false(101L %in% lst$cohort_definition_id)              # tiny (2) invisible
  expect_true(all(c(102L, 103L, 104L) %in% lst$cohort_definition_id))
  expect_true(all(lst$size %% band == 0))                       # banded, not exact
  expect_false(any(lst$size == 8))                              # 8 -> banded, never exact
  expect_identical(lst$size[lst$cohort_definition_id == 104L], 0)
  expect_false(any(grepl("rare-disease", lst$cohort_definition_name))) # tiny name absent
})

test_that("cohort.definition: sub-threshold is INDISTINGUISHABLE from absent", {
  h <- create_test_handle(); .setup_cohorts(h)
  on.exit(cleanup_handle(h), add = TRUE)
  small  <- tryCatch(dsOMOP:::.cohortGetDefinition(h, 101L), error = function(e) conditionMessage(e))
  absent <- tryCatch(dsOMOP:::.cohortGetDefinition(h, 999L), error = function(e) conditionMessage(e))
  # identical response shape (only the caller-supplied id differs) -> existence not confirmable
  expect_equal(sub("101", "X", small), sub("999", "X", absent))
  expect_false(grepl("rare-disease", small))   # name never leaks
  expect_false(grepl("small|too few|threshold|insufficient", small, ignore.case = TRUE)) # no "too small" hint
})

test_that("cohort.definition: at/above-threshold cohort IS readable", {
  h <- create_test_handle(); .setup_cohorts(h)
  on.exit(cleanup_handle(h), add = TRUE)
  big <- dsOMOP:::.cohortGetDefinition(h, 103L)
  expect_true(is.list(big))
  expect_equal(big$cohort_definition_name, "Large")
})
