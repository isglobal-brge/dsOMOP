.seed_overlap_panel <- function(handle) {
  starts <- rep(c("2020-01-01", "2020-01-05"), 3L)
  cohort <- data.frame(
    subject_id = rep(1:3, each = 2L),
    cohort_start_date = starts,
    cohort_end_date = rep("2020-01-15", 6L),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(handle$conn, "overlap_panel_cohort", cohort,
                    temporary = TRUE)

  people <- rep(1:3, each = 3L)
  event_dates <- rep(c("2020-01-06", "2020-01-08", "2020-01-08"), 3L)
  base_id <- DBI::dbGetQuery(
    handle$conn,
    "SELECT COALESCE(MAX(condition_occurrence_id), 0) AS max_id FROM condition_occurrence"
  )$max_id[[1L]] + 100L
  event_ids <- as.integer(unlist(lapply(1:3, function(person) {
    base_id + person * 10L + 1:3
  })))
  events <- data.frame(
    condition_occurrence_id = event_ids,
    person_id = people,
    condition_concept_id = 900001L,
    condition_start_date = event_dates,
    condition_end_date = event_dates,
    condition_type_concept_id = 44818518L,
    visit_occurrence_id = NA_integer_,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(handle$conn, "condition_occurrence", events, append = TRUE)
  invisible(list(first = event_ids[c(1L, 4L, 7L)],
                 last = event_ids[c(2L, 5L, 8L)]))
}

test_that("min_gap deterministically collapses chains within each episode", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  expected <- .seed_overlap_panel(handle)

  extract <- function(keep) {
    .extractTable(
      handle,
      table = "condition_occurrence",
      columns = c("condition_occurrence_id", "person_id",
                  "condition_concept_id"),
      concept_filter = 900001L,
      cohort_table = "overlap_panel_cohort",
      temporal = list(
        index_window = list(start = 0L, end = 10L),
        min_gap = list(days = 2L, by = "concept", keep = keep)
      ),
      add_cohort_date = TRUE,
      date_handling = "remove",
      translate_concepts = FALSE
    )
  }

  withr::with_options(list(nfilter.subset = 3), {
    first <- extract("first")
    last <- extract("last")
    repeated <- extract("last")

    first <- first[order(first$cohort_row_id), , drop = FALSE]
    last <- last[order(last$cohort_row_id), , drop = FALSE]
    repeated <- repeated[order(repeated$cohort_row_id), , drop = FALSE]

    expect_equal(first$cohort_row_id, 1:6)
    expect_equal(last$cohort_row_id, 1:6)
    expect_equal(first$condition_occurrence_id, rep(expected$first, each = 2L))
    expect_equal(last$condition_occurrence_id, rep(expected$last, each = 2L))
    expect_identical(last, repeated)
    expect_false(any(grepl("^dsomop_gap_", names(first))))
  })
})

test_that("min_gap validation is explicit and fail-closed", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  expect_error(.validateTemporalSpec(list(min_gap = 0L)), "positive integer")
  expect_error(.validateTemporalSpec(list(
    min_gap = list(days = 30L, by = "visit"))), "grain or concept")
  expect_error(.validateTemporalSpec(list(
    min_gap = list(days = 30L, keep = "random"))), "first or last")
  expect_error(.wrapMinGap(
    handle, "SELECT 1", list(min_gap = 30L),
    date_col = "dsomop_event_order_date", tie_col = NULL
  ), "primary key")

  where <- .compileTemporalWhere(
    handle, list(min_gap = 30L), "t", "condition_start_date"
  )
  expect_equal(where, "t.condition_start_date IS NOT NULL")

  dialect_markers <- c(
    postgresql = "INTERVAL '1 day'", `sql server` = "DATEADD",
    oracle = "dsomop_gap_previous_date + 30", redshift = "DATEADD",
    bigquery = "DATE_ADD", snowflake = "DATEADD", spark = "DATE_ADD",
    sqlite = "DATE(", duckdb = "INTERVAL '1 day'", mysql = "DATE_ADD"
  )
  for (dialect in names(dialect_markers)) {
    handle$target_dialect <- dialect
    sql <- .wrapMinGap(
      handle,
      paste(
        "SELECT person_id, dsomop_event_partition_concept,",
        "dsomop_event_order_date, dsomop_event_order_id FROM events"
      ),
      list(min_gap = 30L),
      date_col = "dsomop_event_order_date",
      tie_col = "dsomop_event_order_id"
    )
    expect_match(sql, dialect_markers[[dialect]], fixed = TRUE,
                 info = dialect)
    expect_false(grepl("@days", sql, fixed = TRUE), info = dialect)
  }
})

test_that("person_period returns a complete regular overlapping-episode roster", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_overlap_panel(handle)
  DBI::dbExecute(
    handle$conn,
    "DELETE FROM condition_occurrence WHERE person_id = 3 AND condition_concept_id = 900001"
  )

  panel <- withr::with_options(list(nfilter.subset = 2),
    .extractPersonPeriod(
      handle,
      cohort_table = "overlap_panel_cohort",
      table = "condition_occurrence",
      concept_filter = c(900001L, 900002L),
      bin_width = 5L,
      window_start = 0L,
      window_end = 9L,
      analyses = c("binary", "count"),
      grain = "episode",
      time_origin = "index"
    ))

  expect_equal(panel$personRef$rowId, 1:6)
  expect_equal(panel$personRef$person_id, rep(1:3, each = 2L))
  expect_equal(nrow(panel$personPeriods), 12L)
  expect_equal(unname(as.integer(table(panel$personPeriods$rowId))),
               rep(2L, 6L))
  expect_equal(panel$timeRef$startDay, c(0L, 5L))
  expect_equal(panel$timeRef$endDay, c(4L, 9L))
  expect_setequal(panel$covariateRef$conceptId, c(900001L, 900002L))
  expect_false(any(panel$temporalCovariates$covariateId %in%
                     c(900002001, 900002002)))

  binary <- panel$temporalCovariates[
    panel$temporalCovariates$covariateId == 900001001, , drop = FALSE
  ]
  counts <- panel$temporalCovariates[
    panel$temporalCovariates$covariateId == 900001002, , drop = FALSE
  ]
  binary <- binary[order(binary$rowId), , drop = FALSE]
  counts <- counts[order(counts$rowId), , drop = FALSE]
  expect_equal(binary$rowId, 1:4)
  expect_equal(binary$timeId, c(2L, 1L, 2L, 1L))
  expect_true(all(counts$covariateValue == 3))

  roster_keys <- paste(panel$personPeriods$rowId,
                       panel$personPeriods$timeId, sep = ":")
  observed_keys <- paste(binary$rowId, binary$timeId, sep = ":")
  implicit_zero <- setdiff(roster_keys, observed_keys)
  expect_true(all(c("5:1", "5:2", "6:1", "6:2") %in% implicit_zero))
  expect_false(any(grepl("date|occurrence_id", unlist(lapply(panel, names)),
                         ignore.case = TRUE)))

  released <- .testPseudonymize(
    panel, .testPseudonymKey("longitudinal-panel")
  )
  expect_true(all(grepl("^p", released$personRef$person_id)))
  expect_true("rowId" %in%
                attr(released$personPeriods, "dsomop_protected"))
})

test_that("person_period enforces grain, origin, bin and roster caps", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_overlap_panel(handle)

  common <- list(
    handle = handle,
    cohort_table = "overlap_panel_cohort",
    table = "condition_occurrence",
    concept_filter = 900001L,
    bin_width = 5L,
    window_start = 0L,
    window_end = 9L,
    grain = "episode",
    time_origin = "index"
  )
  expect_error(do.call(.extractPersonPeriod,
                       utils::modifyList(common, list(grain = "person"))),
               "grain.*episode")
  expect_error(do.call(.extractPersonPeriod,
                       utils::modifyList(common, list(time_origin = "calendar"))),
               "time_origin.*index")

  withr::with_options(list(nfilter.subset = 3, dsomop.max_temporal_bins = 1L), {
    expect_error(do.call(.extractPersonPeriod, common), "bin.*server cap")
  })
  DBI::dbExecute(
    handle$conn,
    paste0(
      "DELETE FROM condition_occurrence WHERE condition_concept_id = 900001 ",
      "AND condition_start_date != '2020-01-06'"
    )
  )
  withr::with_options(list(nfilter.subset = 3, dsomop.max_memory_rows = 11L), {
    expect_error(do.call(.extractPersonPeriod, common),
                 "episode-bin rows.*row cap")
  })
})
