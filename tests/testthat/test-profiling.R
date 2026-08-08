test_that("profileTableStats returns row count", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    result <- .profileTableStats(handle, "person", stats = c("rows"))
    expect_true(!is.null(result$rows))
    expect_equal(result$rows, 15)  # 15 test persons
  })
})

test_that("profileTableStats returns distinct persons", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    result <- .profileTableStats(handle, "condition_occurrence",
                                  stats = c("persons"))
    expect_true(!is.null(result$persons))
    expect_true(result$persons > 0)
  })
})

test_that("profileTableStats suppresses small counts", {
  handle <- create_test_handle(n_persons = 2)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    result <- .profileTableStats(handle, "person", stats = c("rows"))
    expect_null(result$rows)
    expect_null(result$rows_suppressed)
  })
})

test_that("profileColumnStats returns statistics", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .profileColumnStats(handle, "measurement", "value_as_number")
  expect_true(!is.null(result$n_total))
  expect_true(!is.null(result$n_missing))
  expect_true(!is.null(result$n_distinct))
  # min/max are suppressed to prevent identification of outlier individuals
  expect_null(result$min)
  expect_null(result$max)
  expect_true(!is.null(result$mean))
})

test_that("profileColumnStats blocks sensitive columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .profileColumnStats(handle, "observation", "value_as_string"),
    "blocked"
  )
})

test_that("profileDomainCoverage returns coverage data", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    result <- .profileDomainCoverage(handle)
    expect_true(is.data.frame(result))
    expect_true("table_name" %in% names(result))
    expect_true("n_persons" %in% names(result))
    expect_true("person" %in% result$table_name)
  })
})

test_that("profileMissingness returns rates", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .profileMissingness(handle, "person")
  expect_true(is.data.frame(result))
  expect_true("column_name" %in% names(result))
  expect_true("missing_rate" %in% names(result))
  expect_true(all(result$missing_rate >= 0 & result$missing_rate <= 1))
})

test_that("profileMissingness excludes blocked columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .profileMissingness(handle, "observation")
  expect_false("value_as_string" %in% result$column_name)
})

test_that("profileValueCounts returns frequencies", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(
    nfilter.tab = 3,
    nfilter.levels.max = 40,
    nfilter.levels.density = 0.33
  ), {
    result <- .profileValueCounts(handle, "person", "gender_concept_id")
    expect_true(is.data.frame(result))
    expect_true("value" %in% names(result))
    expect_true("n" %in% names(result))
  })
})

test_that("profileValueCounts blocks sensitive columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .profileValueCounts(handle, "observation", "value_as_string"),
    "blocked"
  )
})

test_that("clinical profilers require a reviewed person_id path", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .profileTableStats(handle, "episode_event", stats = "rows"),
    "reviewed path to person_id"
  )
  expect_error(
    .profileColumnStats(
      handle, "episode_event", "episode_event_field_concept_id"
    ),
    "reviewed path to person_id"
  )
  expect_error(
    .profileMissingness(
      handle, "episode_event", "episode_event_field_concept_id"
    ),
    "reviewed path to person_id"
  )
  expect_error(
    .profileValueCounts(
      handle, "episode_event", "episode_event_field_concept_id"
    ),
    "reviewed path to person_id"
  )
  expect_error(
    .profileConceptDrilldown(
      handle, "episode_event", 1147127L,
      concept_col = "episode_event_field_concept_id"
    ),
    "reviewed path to person_id"
  )
  expect_error(
    .profileConceptPrevalence(
      handle, "episode_event",
      concept_col = "episode_event_field_concept_id"
    ),
    "reviewed path to person_id"
  )
  expect_error(
    .profileSafeCutpoints(handle, "episode_event", "event_id"),
    "reviewed path to person_id"
  )
  expect_error(
    .profileNumericRange(handle, "episode_event", "event_id"),
    "reviewed path to person_id"
  )
  expect_error(
    .profileNumericHistogram(handle, "episode_event", "event_id"),
    "reviewed path to person_id"
  )
  expect_error(
    .profileNumericQuantiles(handle, "episode_event", "event_id"),
    "reviewed path to person_id"
  )
  expect_error(
    .profileDateCounts(handle, "episode_event"),
    "reviewed path to person_id"
  )
  expect_error(
    .profileCrossTab(
      handle, "episode_event", "episode_event_field_concept_id", "event_id",
      count_mode = "records"
    ),
    "reviewed path to person_id"
  )
})

test_that("cohort scope is never ignored on a person-less profiling table", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .profileColumnStats(
      handle, "episode_event", "episode_event_field_concept_id",
      cohort_table = "cohort"
    ),
    "cohort.*no reviewed path to person_id"
  )
  expect_error(
    .profileMissingness(
      handle, "episode_event", "episode_event_field_concept_id",
      cohort_table = "cohort"
    ),
    "cohort.*no reviewed path to person_id"
  )
  expect_error(
    .profileValueCounts(
      handle, "episode_event", "episode_event_field_concept_id",
      cohort_table = "cohort"
    ),
    "cohort.*no reviewed path to person_id"
  )
  expect_error(
    .profileConceptPrevalence(
      handle, "episode_event",
      concept_col = "episode_event_field_concept_id",
      cohort_table = "cohort"
    ),
    "cohort.*no reviewed path to person_id"
  )
  expect_error(
    .profileNumericRange(
      handle, "episode_event", "event_id", cohort_table = "cohort"
    ),
    "cohort.*no reviewed path to person_id"
  )
  expect_error(
    .profileNumericHistogram(
      handle, "episode_event", "event_id", cohort_table = "cohort"
    ),
    "cohort.*no reviewed path to person_id"
  )
  expect_error(
    .profileNumericQuantiles(
      handle, "episode_event", "event_id", cohort_table = "cohort"
    ),
    "cohort.*no reviewed path to person_id"
  )
  expect_error(
    .profileDateCounts(
      handle, "episode_event", cohort_table = "cohort"
    ),
    "cohort.*no reviewed path to person_id"
  )
  expect_error(
    .profileCrossTab(
      handle, "episode_event", "episode_event_field_concept_id", "event_id",
      count_mode = "records", cohort_table = "cohort"
    ),
    "cohort.*no reviewed path to person_id"
  )
})

test_that("public Vocabulary tables remain explicitly profileable", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(
    nfilter.subset = 1,
    nfilter.tab = 1,
    nfilter.levels.max = 100,
    nfilter.levels.density = 1,
    dsomop.nfilter.band = 1
  ), {
    expect_true(!is.null(
      .profileTableStats(handle, "concept", stats = "rows")$rows
    ))
    expect_true(is.list(
      .profileColumnStats(handle, "concept", "standard_concept")
    ))
    expect_true(is.data.frame(
      .profileMissingness(handle, "concept", "standard_concept")
    ))
    expect_true(is.data.frame(
      .profileValueCounts(handle, "concept", "standard_concept")
    ))
    expect_true(is.data.frame(
      .profileDateCounts(handle, "concept", "valid_start_date")
    ))
  })
})

# --- Regression: numeric-distribution profilers must gate on DISTINCT PERSONS,
# not record counts. A concept with many records but < nfilter_subset distinct
# persons (e.g. one patient with 20 lab values) previously leaked p05/p95,
# quantiles, histogram bin counts/edges, and "safe" cutpoints — all sitting at
# that handful of individuals' values. The gate must fail closed (stop), not
# return a suppressed-but-inferable result.

# Helper: a handle whose `measurement` table holds ONE concept with `n_persons`
# distinct persons and `recs_per_person` records each (distinct values), so the
# record count clears the thresholds while the person count does not.
.few_person_many_record_handle <- function(n_persons = 2L,
                                            recs_per_person = 12L,
                                            concept_id = 9990001L) {
  handle <- create_test_handle(n_persons = 15)
  DBI::dbExecute(handle$conn, "DELETE FROM measurement")
  ppl <- seq_len(n_persons)
  total <- n_persons * recs_per_person
  # Column set must match the fixture's `measurement` schema exactly so an
  # append-write succeeds (the fixture carries visit_occurrence_id, not the
  # *_source_* columns).
  meas <- data.frame(
    measurement_id = seq_len(total),
    person_id = rep(ppl, each = recs_per_person),
    measurement_concept_id = rep(as.integer(concept_id), total),
    measurement_date = rep("2019-12-15", total),
    measurement_type_concept_id = rep(44818702L, total),
    value_as_number = seq(50, by = 1, length.out = total),
    value_as_concept_id = rep(0L, total),
    unit_concept_id = rep(8840L, total),
    range_low = rep(4.0, total),
    range_high = rep(6.0, total),
    visit_occurrence_id = rep(NA_integer_, total),
    stringsAsFactors = FALSE
  )
  fields <- DBI::dbListFields(handle$conn, "measurement")
  meas <- meas[, intersect(fields, names(meas)), drop = FALSE]
  DBI::dbWriteTable(handle$conn, "measurement", meas, append = TRUE)
  handle$blueprint <- NULL
  .buildBlueprint(handle)
  handle
}

.replace_profile_measurements <- function(handle, person_id, value, date,
                                           concept_id = 9990100L) {
  stopifnot(length(person_id) == length(value), length(value) == length(date))
  n <- length(value)
  measurement <- data.frame(
    measurement_id = seq_len(n), person_id = as.integer(person_id),
    measurement_concept_id = rep(as.integer(concept_id), n),
    measurement_date = as.character(date),
    measurement_type_concept_id = rep(44818702L, n),
    value_as_number = as.numeric(value), value_as_concept_id = rep(0L, n),
    unit_concept_id = rep(8840L, n), range_low = rep(4, n),
    range_high = rep(6, n), visit_occurrence_id = rep(NA_integer_, n),
    stringsAsFactors = FALSE
  )
  DBI::dbExecute(handle$conn, "DELETE FROM measurement")
  fields <- DBI::dbListFields(handle$conn, "measurement")
  DBI::dbWriteTable(handle$conn, "measurement",
                    measurement[, intersect(fields, names(measurement)),
                                drop = FALSE],
                    append = TRUE)
  handle$blueprint <- NULL
  .buildBlueprint(handle)
  invisible(handle)
}

.test_public_numeric_grid <- function(concept_id, breaks,
                                      concept_col = "measurement_concept_id") {
  list(
    table = "measurement", column = "value_as_number",
    concept_id = concept_id, concept_col = concept_col,
    lower = breaks[1], upper = breaks[length(breaks)],
    breaks = as.numeric(breaks), clipping = "winsorize"
  )
}

test_that("table stats gate every branch on distinct persons", {
  handle <- .few_person_many_record_handle(n_persons = 2L,
                                           recs_per_person = 20L)
  on.exit(cleanup_handle(handle), add = TRUE)
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    result <- .profileTableStats(
      handle, "measurement", stats = c("rows", "persons", "date_range")
    )
    expect_null(result$rows)
    expect_null(result$rows_suppressed)
    expect_null(result$persons)
    expect_null(result$persons_suppressed)
    expect_null(result$date_range)
  })
})

test_that("table date range uses only periods supported by enough persons", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  # Six people clear the table gate, but every month belongs to just one person.
  .replace_profile_measurements(
    handle,
    person_id = rep(1:6, each = 4L),
    value = seq_len(24L),
    date = rep(sprintf("2020-%02d-15", 1:6), each = 4L)
  )
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    result <- .profileTableStats(handle, "measurement", stats = "date_range")
    expect_null(result$date_range)
  })
})

test_that("column mean SD and missingness gate distinct contributors", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  # Two people contribute many values; four additional people contribute NULL.
  # Record counts clear every threshold, distinct value contributors do not.
  .replace_profile_measurements(
    handle,
    person_id = c(rep(1:2, each = 20L), 3:6),
    value = c(seq_len(40L), rep(NA_real_, 4L)),
    date = rep("2020-01-15", 44L)
  )
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.dist = 3), {
    result <- .profileColumnStats(handle, "measurement", "value_as_number")
    expect_true(is.na(result$mean))
    expect_true(is.na(result$sd))
    # The non-missing complement has only two people, so even though four
    # people have NULL rows the missing-record count must not be released.
    expect_true(is.na(result$n_missing))
  })
})

test_that("numeric-distribution profilers fail closed on < nfilter persons (many records)", {
  handle <- .few_person_many_record_handle(n_persons = 2L, recs_per_person = 12L,
                                           concept_id = 9990001L)
  on.exit(cleanup_handle(handle))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.dist = 10), {
    # Sanity: the scenario really is 2 distinct persons with >= nfilter_dist records.
    chk <- DBI::dbGetQuery(handle$conn, paste0(
      "SELECT COUNT(DISTINCT person_id) np, COUNT(*) nr FROM measurement ",
      "WHERE measurement_concept_id = 9990001"))
    expect_equal(chk$np, 2)
    expect_gte(chk$nr, 10)

    # Concept-scoped: every numeric-distribution profiler must STOP, not return.
    expect_error(
      .profileNumericRange(handle, "measurement", "value_as_number",
                           concept_id = 9990001L),
      "disclosure threshold")
    expect_error(
      .profileNumericQuantiles(handle, "measurement", "value_as_number",
                               concept_id = 9990001L),
      "disclosure threshold")
    expect_error(
      .profileNumericHistogram(handle, "measurement", "value_as_number",
                               concept_id = 9990001L),
      "disclosure threshold")
    expect_error(
      .profileSafeCutpoints(handle, "measurement", "value_as_number",
                            concept_id = 9990001L),
      "disclosure threshold")

    # Unscoped (the whole table is the same 2-person concept) must also fail closed.
    expect_error(
      .profileNumericRange(handle, "measurement", "value_as_number"),
      "disclosure threshold")
  })
})

test_that("numeric-distribution profilers still return for >= nfilter persons", {
  # 6 distinct persons, plenty of records: the gate must NOT block legitimate use.
  handle <- .few_person_many_record_handle(n_persons = 6L, recs_per_person = 4L,
                                           concept_id = 9990002L)
  on.exit(cleanup_handle(handle))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.dist = 3,
                           dsomop.safe_numeric_grids = list(
                             .test_public_numeric_grid(
                               9990002L, c(0, 60, 100)
                             )
                           )), {
    expect_type(
      .profileNumericRange(handle, "measurement", "value_as_number",
                           concept_id = 9990002L), "list")
    expect_s3_class(
      .profileNumericQuantiles(handle, "measurement", "value_as_number",
                               concept_id = 9990002L), "data.frame")
    expect_s3_class(
      .profileNumericHistogram(handle, "measurement", "value_as_number",
                               concept_id = 9990002L), "data.frame")
    expect_type(
      .profileSafeCutpoints(handle, "measurement", "value_as_number",
                            concept_id = 9990002L, n_bins = 2L), "list")
  })
})

# --- Differencing defence: every returned person/record count is banded -------
# Counts surviving the suppression gate are floored to a multiple of nfilter_band
# (default 5) so an exact supra-threshold count is never returned. The gate must
# still compare the TRUE distinct-person count to nfilter_subset, not the banded
# value (banded reports, exact gates).

test_that("profileTableStats bands surviving rows/persons to a multiple of 5", {
  handle <- create_test_handle(n_persons = 13)  # 13 -> floors to 10
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    res <- .profileTableStats(handle, "person", stats = c("rows", "persons"))
    expect_equal(res$rows, 10)
    expect_equal(res$persons, 10)
    expect_equal(res$persons %% 5, 0)
    expect_null(res$persons_suppressed)
  })
})

test_that("profileColumnStats bands n_total / n_persons (not the mean)", {
  handle <- create_test_handle(n_persons = 13)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    res <- .profileColumnStats(handle, "person", "gender_concept_id")
    expect_equal(res$n_total %% 5, 0)
    expect_equal(res$n_persons, 10)   # 13 distinct persons -> 10
  })
})

test_that("count gate uses the EXACT count while the report is banded", {
  handle <- create_test_handle(n_persons = 13)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Exact 13 >= subset 12 must PASS even though banded(13) = 10 < 12 ...
  withr::with_options(list(nfilter.subset = 12, nfilter.tab = 3), {
    res <- .profileTableStats(handle, "person", stats = c("persons"))
    expect_null(res$persons_suppressed)
    expect_equal(res$persons, 10)     # reported value is banded, gate is exact
  })
  # ... and exact 13 < subset 14 must BLOCK (proves the gate is not the band).
  withr::with_options(list(nfilter.subset = 14, nfilter.tab = 3), {
    expect_error(
      .profileColumnStats(handle, "person", "gender_concept_id"),
      "insufficient|Disclosive")
  })
})

test_that("profileConceptPrevalence bands n_persons / n_records", {
  handle <- create_test_handle(n_persons = 13)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    res <- .profileConceptPrevalence(handle, "condition_occurrence",
                                     metric = "persons")
    skip_if(nrow(res) == 0, "no prevalence rows in fixture")
    np <- res$n_persons[!is.na(res$n_persons)]
    nr <- res$n_records[!is.na(res$n_records)]
    expect_true(all(np %% 5 == 0))
    expect_true(all(nr %% 5 == 0))
  })
})

test_that("concept prevalence accepts only reviewed concept dimensions", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  DBI::dbExecute(
    handle$conn,
    "ALTER TABLE condition_occurrence ADD COLUMN condition_source_concept_id INTEGER"
  )
  .buildBlueprint(handle)

  expect_error(
    .profileConceptPrevalence(
      handle, "condition_occurrence",
      concept_col = "condition_source_concept_id"
    ),
    "not a valid scope column"
  )
  expect_error(
    .profileConceptPrevalence(
      handle, "condition_occurrence", concept_col = "provider_id"
    ),
    "not found|not a valid scope column"
  )
})

test_that("profileValueCounts bands the per-value n / n_persons", {
  handle <- create_test_handle(n_persons = 13)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.tab = 3, nfilter.levels.max = 40,
                           nfilter.levels.density = 0.9), {
    res <- .profileValueCounts(handle, "person", "gender_concept_id")
    skip_if(nrow(res) == 0, "no value-count rows in fixture")
    expect_true(all(res$n %% 5 == 0))
    if ("n_persons" %in% names(res)) {
      expect_true(all(res$n_persons %% 5 == 0))
    }
  })
})

test_that("top value counts rank by released bands and public values", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  person_id <- c(rep(1L, 20L), 2:9, 10:15)
  .replace_profile_measurements(
    handle, person_id = person_id, value = seq_along(person_id),
    date = rep("2020-01-01", length(person_id)), concept_id = 9990106L
  )
  DBI::dbExecute(
    handle$conn,
    paste0(
      "UPDATE measurement SET unit_concept_id = CASE ",
      "WHEN measurement_id <= 20 THEN 300 ",
      "WHEN measurement_id <= 28 THEN 200 ELSE 100 END"
    )
  )

  withr::with_options(list(
    nfilter.subset = 3, nfilter.tab = 3,
    nfilter.levels.max = 10, nfilter.levels.density = 1,
    dsomop.nfilter.band = 5
  ), {
    # Value 300 has the largest record count (20) but only one person and must be
    # removed before top_n. Safe values 200 (8) and 100 (6) share release band 5,
    # so their tie is broken on the public value rather than the hidden count.
    first <- .profileValueCounts(
      handle, "measurement", "unit_concept_id", top_n = 1
    )
    both <- .profileValueCounts(
      handle, "measurement", "unit_concept_id", top_n = 2
    )
    expect_equal(as.character(first$value), "100")
    expect_equal(as.character(both$value), c("100", "200"))
    expect_equal(both$n, c(5, 5))
  })
})

test_that("prevalence pagination never ranks within a hidden count band", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  DBI::dbExecute(handle$conn, "DELETE FROM condition_occurrence")
  person_id <- c(rep(1L, 20L), 2:9, 10:15)
  conditions <- data.frame(
    condition_occurrence_id = seq_along(person_id),
    person_id = person_id,
    condition_concept_id = c(rep(300L, 20), rep(200L, 8), rep(100L, 6)),
    condition_start_date = rep("2020-01-01", length(person_id)),
    condition_end_date = rep("2020-12-31", length(person_id)),
    condition_type_concept_id = rep(44818518L, length(person_id)),
    visit_occurrence_id = rep(NA_integer_, length(person_id)),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(
    handle$conn, "condition_occurrence", conditions, append = TRUE
  )
  .buildBlueprint(handle)

  withr::with_options(list(
    nfilter.subset = 3, nfilter.tab = 3, dsomop.nfilter.band = 5
  ), {
    first <- .profileConceptPrevalence(
      handle, "condition_occurrence", top_n = 1, offset = 0
    )
    second <- .profileConceptPrevalence(
      handle, "condition_occurrence", top_n = 1, offset = 1
    )
    records_first <- .profileConceptPrevalence(
      handle, "condition_occurrence", metric = "records", top_n = 1
    )
    expect_equal(first$concept_id, 100L)
    expect_equal(second$concept_id, 200L)
    expect_equal(records_first$concept_id, 100L)
    expect_equal(c(first$n_persons, second$n_persons), c(5, 5))
  })
})

test_that("concept drilldown categorical order uses released bands", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  person_id <- c(rep(1L, 20L), 2:9, 10:15)
  .replace_profile_measurements(
    handle, person_id = person_id, value = seq_along(person_id),
    date = rep("2020-01-01", length(person_id)), concept_id = 9990105L
  )
  DBI::dbExecute(
    handle$conn,
    paste0(
      "UPDATE measurement SET value_as_concept_id = CASE ",
      "WHEN measurement_id <= 20 THEN 300 ",
      "WHEN measurement_id <= 28 THEN 200 ELSE 100 END"
    )
  )

  withr::with_options(list(
    nfilter.subset = 3, nfilter.tab = 3,
    nfilter.levels.max = 10, nfilter.levels.density = 1,
    dsomop.nfilter.band = 5
  ), {
    result <- .profileConceptDrilldown(handle, "measurement", 9990105L)
    expect_equal(
      result$categorical_values$value_as_concept_id, c(100L, 200L)
    )
    expect_equal(result$categorical_values$n, c(5, 5))
  })
})

# --- Central profiler column gate --------------------------------------------

test_that("public profilers reject identifiers and sensitive values before SQL", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  .buildBlueprint(handle)  # cache metadata so the policy gate needs no query
  symbol <- paste0("profile_column_gate_", Sys.getpid())
  .setHandle(symbol, handle)
  on.exit(.removeHandle(symbol), add = TRUE)

  # Disconnect after caching the blueprint. Matching the policy error below
  # proves rejection happened before any profiler SQL was attempted.
  DBI::dbDisconnect(handle$conn)

  identifier_calls <- list(
    function() omopColumnStatsDS(symbol, "person", "person_id"),
    function() omopValueCountsDS(symbol, "person", "person_id"),
    function() omopNumericRangeDS(symbol, "person", "person_id"),
    function() omopNumericHistogramDS(symbol, "person", "person_id"),
    function() omopNumericQuantilesDS(symbol, "person", "person_id"),
    function() omopSafeCutpointsDS(symbol, "person", "person_id"),
    function() omopValueCountsDS(symbol, "condition_occurrence",
                                 "visit_occurrence_id"),
    function() omopCrossTabDS(symbol, "condition_occurrence", "person_id",
                              "condition_concept_id"),
    function() omopCrossTabDS(symbol, "condition_occurrence",
                              "condition_concept_id",
                              "condition_type_concept_id",
                              stratify_by = "person_id")
  )
  for (call in identifier_calls) {
    expect_error(call(), "Identifier column.*not permitted for profiling")
  }

  expect_error(
    omopNumericRangeDS(symbol, "concept", "concept_name"),
    "not a numeric measure"
  )
  expect_error(
    omopValueCountsDS(symbol, "measurement", "value_as_number"),
    "continuous"
  )
  expect_error(
    omopCrossTabDS(symbol, "measurement", "value_as_number",
                   "measurement_concept_id"),
    "categorical"
  )
  expect_error(
    omopMissingnessDS(symbol, "observation", columns = "value_as_string"),
    "blocked.*sensitive"
  )
})

test_that("public numeric profilers still accept a clinical numeric measure", {
  handle <- .few_person_many_record_handle(
    n_persons = 6L, recs_per_person = 4L, concept_id = 9990003L
  )
  on.exit(cleanup_handle(handle), add = TRUE)
  symbol <- paste0("profile_numeric_valid_", Sys.getpid())
  .setHandle(symbol, handle)
  on.exit(.removeHandle(symbol), add = TRUE)

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.dist = 3,
                           dsomop.safe_numeric_grids = list(
                             .test_public_numeric_grid(
                               9990003L, c(0, 60, 100)
                             )
                           )), {
    expect_type(
      omopNumericRangeDS(symbol, "measurement", "value_as_number",
                         concept_id = 9990003L),
      "list"
    )
    expect_s3_class(
      omopNumericHistogramDS(symbol, "measurement", "value_as_number",
                             concept_id = 9990003L),
      "data.frame"
    )
    expect_s3_class(
      omopNumericQuantilesDS(symbol, "measurement", "value_as_number",
                             concept_id = 9990003L),
      "data.frame"
    )
    expect_type(
      omopSafeCutpointsDS(symbol, "measurement", "value_as_number",
                          concept_id = 9990003L, n_bins = 2L),
      "list"
    )
    stats <- omopColumnStatsDS(symbol, "measurement", "value_as_number",
                               concept_id = 9990003L)
    expect_true(is.numeric(stats$mean))
  })
})

test_that("numeric histogram rejects malformed or injectable breaks", {
  handle <- .few_person_many_record_handle(
    n_persons = 6L, recs_per_person = 4L, concept_id = 9990004L
  )
  on.exit(cleanup_handle(handle), add = TRUE)

  bad_breaks <- list(
    c("0", "1); DROP TABLE person; --"),
    c(0, NA_real_, 2),
    c(0, Inf, 2),
    c(0, 2, 1),
    c(0, 1, 1)
  )
  for (candidate in bad_breaks) {
    expect_error(
      .profileNumericHistogram(handle, "measurement", "value_as_number",
                               breaks = candidate,
                               concept_id = 9990004L),
      "finite, strictly increasing numeric"
    )
  }
  expect_error(
    .profileNumericHistogram(handle, "measurement", "value_as_number",
                             bins = 2.5, concept_id = 9990004L),
    "one integer"
  )

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.dist = 3), {
    result <- .profileNumericHistogram(
      handle, "measurement", "value_as_number",
      breaks = c(0, 10, 30), concept_id = 9990004L
    )
    expect_s3_class(result, "data.frame")
  })
})

test_that("safe cutpoints use one contribution per person and a public grid", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  # One person's 50 repeated extreme records must still contribute to only one
  # configured-grid cell after the person-level collapse.
  .replace_profile_measurements(
    handle,
    person_id = c(seq_len(14), rep(15L, 50L)),
    value = c(10:23, rep(999, 50L)),
    date = rep("2020-01-15", 64L)
  )

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.dist = 10,
                           dsomop.nfilter.band = 5,
                           dsomop.safe_numeric_grids = list(
                             .test_public_numeric_grid(
                               9990100L,
                               c(0, 12.5, 15.5, 18.5, 21.5, 1000)
                             )
                           )), {
    result <- .profileSafeCutpoints(handle, "measurement", "value_as_number",
                                    concept_id = 9990100L, n_bins = 5L)
    expect_equal(result$breaks,
                 c(0, 12.5, 15.5, 18.5, 21.5, 1000))
    expect_identical(result$grid$clipping, "winsorize")
    expect_identical(result$grid$source, "server_configured_public_grid")
    expect_true(all(result$counts %% 5 == 0))
    expect_lte(sum(result$counts), 15)
  })
})

test_that("safe cutpoints reject a public grid with an under-supported bin", {
  handle <- .few_person_many_record_handle(n_persons = 5L,
                                           recs_per_person = 20L)
  on.exit(cleanup_handle(handle), add = TRUE)
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.dist = 3,
                           dsomop.safe_numeric_grids = list(
                             .test_public_numeric_grid(
                               NULL, c(0, 100, 200), concept_col = NULL
                             )
                           )), {
    expect_error(
      .profileSafeCutpoints(handle, "measurement", "value_as_number",
                            n_bins = 2L),
      "not supported by enough individuals"
    )
    expect_length(handle$safe_numeric_bins %||% list(), 0L)
  })
})

test_that("safe cutpoints fail closed without a server-configured public grid", {
  handle <- .few_person_many_record_handle(
    n_persons = 6L, recs_per_person = 4L, concept_id = 9990200L
  )
  on.exit(cleanup_handle(handle), add = TRUE)
  withr::with_options(list(
    nfilter.subset = 3, nfilter.tab = 3, dsomop.nfilter.dist = 3,
    dsomop.safe_numeric_grids = list(),
    default.dsomop.safe_numeric_grids = list()
  ), {
    expect_error(
      .profileSafeCutpoints(
        handle, "measurement", "value_as_number",
        concept_id = 9990200L, n_bins = 2L
      ),
      "server administrator must configure a public numeric grid"
    )
    expect_length(handle$safe_numeric_bins %||% list(), 0L)
  })
})

test_that("omitted grid concept_col means the domain column, not a wildcard", {
  handle <- .few_person_many_record_handle(
    n_persons = 6L, recs_per_person = 4L, concept_id = 9990203L
  )
  on.exit(cleanup_handle(handle), add = TRUE)
  default_grid <- .test_public_numeric_grid(9990203L, c(0, 60, 100))
  default_grid$concept_col <- NULL

  withr::with_options(list(
    nfilter.subset = 3, nfilter.tab = 3, dsomop.nfilter.dist = 3,
    dsomop.safe_numeric_grids = list(default_grid)
  ), {
    result <- .profileSafeCutpoints(
      handle, "measurement", "value_as_number",
      concept_id = 9990203L, n_bins = 2L
    )
    expect_equal(result$breaks, c(0, 60, 100))
    expect_identical(result$contract$concept_col,
                     "measurement_concept_id")
  })

  override_grid <- .test_public_numeric_grid(8840L, c(0, 60, 100))
  override_grid$concept_col <- NULL
  withr::with_options(list(
    nfilter.subset = 3, nfilter.tab = 3, dsomop.nfilter.dist = 3,
    dsomop.safe_numeric_grids = list(override_grid)
  ), {
    expect_error(
      .profileSafeCutpoints(
        handle, "measurement", "value_as_number", concept_id = 8840L,
        concept_col = "unit_concept_id", n_bins = 2L
      ),
      "configure an exact public numeric grid"
    )
  })
})

test_that("safe cutpoint edges are independent of protected observations", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  public_breaks <- c(0, 50, 100)
  grid <- .test_public_numeric_grid(9990201L, public_breaks)

  run_cuts <- function(values) {
    .replace_profile_measurements(
      handle, person_id = seq_along(values), value = values,
      date = rep("2020-01-15", length(values)), concept_id = 9990201L
    )
    .profileSafeCutpoints(
      handle, "measurement", "value_as_number",
      concept_id = 9990201L, n_bins = 2L
    )
  }

  withr::with_options(list(
    nfilter.subset = 3, nfilter.tab = 3, dsomop.nfilter.dist = 3,
    dsomop.nfilter.band = 1,
    dsomop.safe_numeric_grids = list(grid)
  ), {
    first <- run_cuts(c(1, 2, 3, 101, 102, 103))
    second <- run_cuts(c(-999, -998, -997, 90, 91, 92))
    expect_equal(first$breaks, public_breaks)
    expect_equal(second$breaks, public_breaks)
    expect_equal(first$counts, c(3, 3))
    expect_equal(second$counts, c(3, 3))
    expect_equal(first$grid[c("lower", "upper", "clipping")],
                 list(lower = 0, upper = 100, clipping = "winsorize"))
  })
})

test_that("malformed public grids and internal contracts fail closed", {
  handle <- .few_person_many_record_handle(
    n_persons = 6L, recs_per_person = 4L, concept_id = 9990202L
  )
  on.exit(cleanup_handle(handle), add = TRUE)
  malformed <- .test_public_numeric_grid(9990202L, c(0, 50, 100))
  malformed$lower <- "0; DROP TABLE person; --"

  withr::with_options(list(
    nfilter.subset = 3, nfilter.tab = 3, dsomop.nfilter.dist = 3,
    dsomop.safe_numeric_grids = list(malformed)
  ), {
    expect_error(
      .profileSafeCutpoints(
        handle, "measurement", "value_as_number",
        concept_id = 9990202L, n_bins = 2L
      ),
      "Invalid server option"
    )
  })
  expect_true(DBI::dbExistsTable(handle$conn, "person"))
  expect_error(
    .rememberSafeNumericBins(
      handle, list(table = "measurement", column = "value_as_number",
                   concept_id = 9990202L,
                   concept_col = "measurement_concept_id", n_bins = 2L),
      breaks = c(0, NA_real_, 100)
    ),
    "Invalid internal safe numeric-bin contract"
  )
  expect_length(handle$safe_numeric_bins %||% list(), 0L)
})

test_that("numeric range and histogram release only banded counts", {
  handle <- .few_person_many_record_handle(n_persons = 11L,
                                           recs_per_person = 4L,
                                           concept_id = 9990101L)
  on.exit(cleanup_handle(handle), add = TRUE)
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.dist = 3,
                           dsomop.nfilter.band = 5), {
    range <- .profileNumericRange(handle, "measurement", "value_as_number",
                                  concept_id = 9990101L)
    expect_equal(range$n_total, 10)

    below_dist <- withr::with_options(list(dsomop.nfilter.dist = 30), {
      .profileNumericRange(handle, "measurement", "value_as_number",
                           concept_id = 9990101L)
    })
    expect_true(is.na(below_dist$p05) && is.na(below_dist$p95))
    expect_equal(below_dist$n_total, 10)

    histogram <- .profileNumericHistogram(
      handle, "measurement", "value_as_number", breaks = c(49, 72, 100),
      concept_id = 9990101L
    )
    expect_equal(histogram$count, c(5, 5))
    expect_true(all(histogram$count %% 5 == 0))
  })
})

test_that("numeric distributions default to one value per person with protected tails", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  .replace_profile_measurements(
    handle,
    person_id = c(seq_len(14), rep(15L, 50L)),
    value = c(10:23, rep(999, 50L)),
    date = rep("2020-01-15", 64L), concept_id = 9990102L
  )

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.dist = 10,
                           dsomop.nfilter.band = 5), {
    range <- .profileNumericRange(
      handle, "measurement", "value_as_number", concept_id = 9990102L
    )
    expect_gt(range$p05, 10)
    expect_lt(range$p95, 999)
    expect_equal(range$n_total, 15)

    quantiles <- .profileNumericQuantiles(
      handle, "measurement", "value_as_number", probs = c(0.05, 0.95),
      concept_id = 9990102L
    )
    expect_gt(quantiles$value[1], 10)
    expect_lt(quantiles$value[2], 999)

    histogram <- .profileNumericHistogram(
      handle, "measurement", "value_as_number", breaks = c(0, 100, 1000),
      concept_id = 9990102L
    )
    expect_equal(nrow(histogram), 1L)
    expect_equal(histogram$count, 10)

    expect_error(
      .profileNumericRange(
        handle, "measurement", "value_as_number", concept_id = 9990102L,
        unit = "record"
      ),
      "multiple scoped records"
    )
  })
})

test_that("recurrent cohort membership does not multiply profiling records", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  .replace_profile_measurements(
    handle, person_id = 1:6, value = 1:6,
    date = rep("2020-01-15", 6L), concept_id = 9990103L
  )
  recurrent <- data.frame(
    subject_id = rep(1:6, each = 3L),
    cohort_start_date = rep(c("2018-01-01", "2019-01-01", "2020-01-01"), 6L),
    cohort_end_date = rep(c("2018-12-31", "2019-12-31", "2020-12-31"), 6L)
  )
  DBI::dbWriteTable(handle$conn, "recurrent_profile_cohort", recurrent,
                    overwrite = TRUE)
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           nfilter.levels.density = 1,
                           dsomop.nfilter.band = 5), {
    result <- .profileValueCounts(
      handle, "measurement", "measurement_concept_id",
      cohort_table = "recurrent_profile_cohort"
    )
    expect_equal(result$n, 5)
    expect_equal(result$n_persons, 5)
  })
})

test_that("concept drilldown inherits person-unit distributions and safe ratios", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  .replace_profile_measurements(
    handle,
    person_id = c(rep(1L, 50L), 2:15),
    value = c(rep(999, 50L), 10:23),
    date = rep("2020-01-15", 64L), concept_id = 9990104L
  )
  DBI::dbExecute(
    handle$conn,
    "UPDATE measurement SET range_low = NULL WHERE person_id >= 3"
  )

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.dist = 10,
                           dsomop.nfilter.band = 5), {
    result <- .profileConceptDrilldown(handle, "measurement", 9990104L)
    expect_equal(
      result$summary$records_per_person_mean,
      result$summary$n_records / result$summary$n_persons
    )
    expect_true(is.na(result$summary$pct_persons_multi))
    expect_gt(result$numeric_summary$quantiles$value[1], 10)
    expect_lt(tail(result$numeric_summary$quantiles$value, 1), 999)
    if (nrow(result$numeric_summary$histogram) > 0L) {
      expect_true(all(result$numeric_summary$histogram$count %% 5 == 0))
    }
    range_low <- result$missingness[
      result$missingness$column_name == "range_low", "missing_rate"
    ]
    expect_true(is.na(range_low))
  })
})

test_that("date counts gate each period on persons and band released counts", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  .replace_profile_measurements(
    handle,
    person_id = c(rep(1L, 20L), 2:7),
    value = seq_len(26L),
    date = c(rep("2018-01-15", 20L), rep("2019-01-15", 6L))
  )

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    result <- .profileDateCounts(handle, "measurement", granularity = "year")
    expect_false("2018" %in% result$period)
    expect_equal(result$period, "2019")
    expect_equal(result$n_records, 5)
    expect_equal(result$n_persons, 5)
  })
})

test_that("date counts require an OMOP date column and a closed window", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  .buildBlueprint(handle)

  expect_error(
    .profileDateCounts(handle, "measurement", date_col = "value_as_number"),
    "not a declared OMOP date field"
  )
  expect_error(
    .profileDateCounts(
      handle, "measurement", window = list(start = "2020-01-01")
    ),
    "require both start and end"
  )
})

test_that("public date counts rejects an unowned named cohort scope", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  symbol <- paste0("date_counts_scope_", Sys.getpid())
  .setHandle(symbol, handle)
  on.exit(.removeHandle(symbol), add = TRUE)

  expect_error(
    omopDateCountsDS(symbol, "measurement", cohort_table = "person"),
    "temporary cohorts created by this handle"
  )
  expect_true(DBI::dbExistsTable(handle$conn, "person"))
})

test_that("numeric quantile and profiling-window inputs fail closed", {
  handle <- .few_person_many_record_handle(n_persons = 10L,
                                           recs_per_person = 2L)
  on.exit(cleanup_handle(handle), add = TRUE)
  bad_probs <- list(c(0.5, 0.5), c(0.01, 0.5), c(0.5, Inf), seq(0.1, 0.9, 0.08))
  for (probs in bad_probs) {
    expect_error(
      .profileNumericQuantiles(handle, "measurement", "value_as_number",
                               probs = probs),
      "unique finite probabilities"
    )
  }
  expect_error(
    .profileNumericQuantiles(handle, "measurement", "value_as_number",
                             rounding = 2.5),
    "rounding must be one integer"
  )
  expect_error(
    .profileNumericRange(
      handle, "measurement", "value_as_number",
      window = list(start = "2020-01-01' OR 1=1 --", end = "2020-12-31")
    ),
    "ISO date"
  )
  withr::with_options(list(dsomop.nfilter.date_range = 30), {
    expect_error(
      .profileDateCounts(
        handle, "measurement",
        window = list(start = "2020-01-01", end = "2020-01-10")
      ),
      "span at least 30 days"
    )
  })
})
