# ==============================================================================
# Phase 1: population-filter window= wiring (server)
#
# has_concept / not_has_concept / missing_measurement now apply a `window=`
# (list(start, end) day offsets relative to the current date, negative = past)
# as a date predicate inside their EXISTS / NOT EXISTS subquery. Without a
# window the subquery must carry NO date predicate (back-compat); with one it
# must constrain the table's event date column. This proves the client-carried
# window is no longer a silent no-op server-side.
# ==============================================================================

test_that("population filter window= adds a current-date-relative event-date predicate", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  bp <- .buildBlueprint(handle)
  pc <- c("person_id")
  date_col <- .getDateColumn(bp, "measurement")
  expect_false(is.null(date_col))
  win <- list(start = -365L, end = 0L)
  mk <- function(type, params) list(type = type, params = params)

  # has_concept: no window -> no date predicate; window -> date predicate inside EXISTS
  hc0 <- .compileCohortFilterLeaf(handle,
    mk("has_concept", list(concept_ids = 3001L, table = "measurement")), bp, pc)
  hc1 <- .compileCohortFilterLeaf(handle,
    mk("has_concept", list(concept_ids = 3001L, table = "measurement", window = win)), bp, pc)
  expect_true(grepl("EXISTS", hc0))
  expect_false(grepl(date_col, hc0, fixed = TRUE))
  expect_true(grepl(date_col, hc1, fixed = TRUE))

  # not_has_concept
  nhc0 <- .compileCohortFilterLeaf(handle,
    mk("not_has_concept", list(concept_ids = 3001L, table = "measurement")), bp, pc)
  nhc1 <- .compileCohortFilterLeaf(handle,
    mk("not_has_concept", list(concept_ids = 3001L, table = "measurement", window = win)), bp, pc)
  expect_true(grepl("NOT EXISTS", nhc1))
  expect_false(grepl(date_col, nhc0, fixed = TRUE))
  expect_true(grepl(date_col, nhc1, fixed = TRUE))

  # missing_measurement
  mm0 <- .compileCohortFilterLeaf(handle,
    mk("missing_measurement", list(concept_ids = 3001L)), bp, pc)
  mm1 <- .compileCohortFilterLeaf(handle,
    mk("missing_measurement", list(concept_ids = 3001L, window = win)), bp, pc)
  expect_false(grepl(date_col, mm0, fixed = TRUE))
  expect_true(grepl(date_col, mm1, fixed = TRUE))

  # one-sided window: only start -> only a >= predicate
  hc_start <- .compileCohortFilterLeaf(handle,
    mk("has_concept", list(concept_ids = 3001L, table = "measurement",
                           window = list(start = -30L))), bp, pc)
  expect_true(grepl(paste0(date_col, " >="), hc_start, fixed = TRUE))
  expect_false(grepl(paste0(date_col, " <="), hc_start, fixed = TRUE))
})
