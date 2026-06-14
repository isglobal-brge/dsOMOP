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
    expect_true(is.na(result$rows))
    expect_true(result$rows_suppressed)
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
                           dsomop.nfilter.dist = 3), {
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
                            concept_id = 9990002L), "list")
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
    expect_false(res$persons_suppressed)
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
    expect_false(res$persons_suppressed)
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
