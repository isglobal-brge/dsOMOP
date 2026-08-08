test_that("in-memory feature date filters compare typed ISO bounds", {
  dates <- data.frame(
    event_date = as.Date(c("2020-01-01", "2020-01-31", "2020-02-01")),
    event_datetime = as.POSIXct(
      c("2020-01-01 12:00:00", "2020-01-31 23:59:59",
        "2020-02-01 00:00:00"), tz = "UTC"
    )
  )

  date_filter <- list(
    var = "event_date", op = "between",
    value = list("2020-01-01", "2020-01-31")
  )
  datetime_filter <- list(
    var = "event_datetime", op = "between",
    value = list("2020-01-01", "2020-01-31")
  )

  expect_equal(.evalFilterMask(date_filter, dates), c(TRUE, TRUE, FALSE))
  expect_equal(.evalFilterMask(datetime_filter, dates), c(TRUE, TRUE, FALSE))
  expect_equal(
    .evalFilterMask(
      list(var = "event_date", op = "==", value = "2020-01-31"), dates
    ),
    c(FALSE, TRUE, FALSE)
  )
  expect_equal(
    .evalFilterMask(
      list(var = "event_date", op = "in",
           value = list("2020-01-01", "2020-02-01")), dates
    ),
    c(TRUE, FALSE, TRUE)
  )
  expect_equal(
    .evalFilterMask(
      list(var = "event_datetime", op = "in", value = "2020-02-01"), dates
    ),
    c(FALSE, FALSE, TRUE)
  )
  expect_error(
    .evalFilterMask(
      list(var = "event_date", op = "between",
           value = list("2020-01-01", "2020-02-31")),
      dates
    ),
    "valid date"
  )
  expect_error(
    .evalFilterMask(
      list(var = "event_date", op = "==", value = "2020-02-31"), dates
    ),
    "valid date"
  )

  sql <- .compileFilter(
    NULL,
    list(var = "measurement_datetime", op = "between",
         value = list("2020-01-01", "2020-01-31"))
  )
  expect_match(sql, "measurement_datetime >= '2020-01-01'")
  expect_match(sql, "measurement_datetime < '2020-02-01'")

  missing_date <- data.frame(event_date = as.Date(c(NA, "2020-01-01")))
  expect_equal(
    .evalFilterMask(
      list(var = "event_date", op = "not_in", value = "2020-02-01"),
      missing_date
    ),
    c(FALSE, TRUE)
  )
})

test_that("first and latest values use OMOP date and primary-key ordering", {
  events <- data.frame(
    measurement_id = c(30L, 10L, 20L, 40L),
    person_id = c(1L, 1L, 1L, 2L),
    measurement_concept_id = 100L,
    measurement_date = as.Date(c(
      "2020-01-01", "2020-03-01", "2020-03-01", "2020-02-01"
    )),
    value_as_number = c(1, 3, 99, 4)
  )
  specs <- list(
    first = list(type = "first_value", name = "first",
                 concept_set = 100L, value_column = "value_as_number"),
    latest = list(type = "latest_value", name = "latest",
                  concept_set = 100L, value_column = "value_as_number")
  )

  result <- .toFeatures(events, "measurement", specs)
  reversed <- .toFeatures(events[nrow(events):1, ], "measurement", specs)

  expect_equal(result$first[match(c(1L, 2L), result$person_id)], c(1, 4))
  # Same-date ties choose the lower OMOP primary key, independent of input order.
  expect_equal(result$latest[match(c(1L, 2L), result$person_id)], c(3, 4))
  expect_equal(
    result[order(result$person_id), c("person_id", "first", "latest")],
    reversed[order(reversed$person_id), c("person_id", "first", "latest")],
    ignore_attr = "row.names"
  )

  expect_error(
    .toFeatures(
      events[, setdiff(names(events), "measurement_date")],
      "measurement", specs
    ),
    "usable OMOP date"
  )

  no_pk <- events[events$person_id == 1L,
                  setdiff(names(events), "measurement_id")]
  expect_error(
    .toFeatures(no_pk, "measurement", specs),
    "same-date.*primary key"
  )
})

test_that("feature reductions preserve a complete roster and stable schema", {
  events <- data.frame(
    person_id = c(1L, 2L),
    measurement_concept_id = 100L,
    measurement_date = as.Date(c("2020-01-01", "2020-02-01")),
    value_as_number = c(5, 7)
  )
  specs <- list(
    present = list(type = "boolean", name = "present", concept_set = 100L),
    n = list(type = "count", name = "n", concept_set = 100L),
    mean = list(type = "mean_value", name = "mean", concept_set = 100L,
                value_column = "value_as_number"),
    first = list(type = "first_value", name = "first", concept_set = 100L,
                 value_column = "value_as_number")
  )

  roster <- 1:4
  result <- .toFeatures(events, "measurement", specs, person_ids = roster)
  expect_equal(result$person_id, roster)
  expect_equal(result$present, c(1L, 1L, 0L, 0L))
  expect_equal(result$n, c(1L, 1L, 0L, 0L))
  expect_equal(result$mean, c(5, 7, NA, NA))
  expect_equal(result$first, c(5, 7, NA, NA))

  empty <- .toFeatures(events[FALSE, ], "measurement", specs,
                       person_ids = roster)
  expect_equal(empty$person_id, roster)
  expect_equal(names(empty), names(result))
  expect_equal(empty$present, rep(0L, 4))
  expect_equal(empty$n, rep(0L, 4))
  expect_true(all(is.na(empty$mean)))
  expect_true(all(is.na(empty$first)))
})

test_that("index-derived age preserves recurrent cohort episodes", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  cohort <- "tmp_recurrent_derived_age"
  DBI::dbExecute(handle$conn, paste0(
    "CREATE TEMP TABLE ", cohort,
    " (subject_id INTEGER, cohort_start_date TEXT, cohort_end_date TEXT)"
  ))
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO ", cohort, " VALUES ",
    "(1, '2010-01-01', '2010-12-31'), ",
    "(1, '2020-01-01', '2020-12-31'), ",
    "(2, '2020-01-01', '2020-12-31'), ",
    "(3, '2020-01-01', '2020-12-31')"
  ))

  specs <- list(
    list(kind = "age", name = "age", reference = "index"),
    list(kind = "sex_mf", name = "sex")
  )
  withr::with_options(list(nfilter.subset = 0), {
    result <- .computeDerivedColumns(
      handle, specs, person_ids = 1:3, cohort_table = cohort
    )
  })

  expect_equal(nrow(result), 4L)
  expect_equal(result$row_id, result$cohort_row_id)
  expect_equal(result$cohort_row_id, 1:4)
  p1 <- result[result$person_id == 1L, , drop = FALSE]
  expect_equal(p1$age, c(48L, 58L))
  expect_equal(p1$sex, c("M", "M"))
})

test_that("fixed age and explicit multi-period observation policies are deterministic", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  cohort <- "tmp_fixed_derived_age"
  DBI::dbExecute(handle$conn, paste0(
    "CREATE TEMP TABLE ", cohort,
    " (subject_id INTEGER, cohort_start_date TEXT, cohort_end_date TEXT)"
  ))
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO ", cohort, " VALUES ",
    "(1, '2010-01-01', '2010-12-31'), ",
    "(1, '2020-01-01', '2020-12-31'), ",
    "(2, '2020-01-01', '2020-12-31'), ",
    "(3, '2020-01-01', '2020-12-31')"
  ))

  fixed <- list(list(
    kind = "age", name = "age", reference = "index",
    reference_date = "2015-07-01"
  ))
  withr::with_options(list(nfilter.subset = 0), {
    result <- .computeDerivedColumns(
      handle, fixed, person_ids = 1:3, cohort_table = cohort
    )
  })
  expect_equal(nrow(result), 3L)
  expect_false("cohort_row_id" %in% names(result))
  expect_equal(result$age, c(53L, 51L, 49L))

  extra_period <- data.frame(
    observation_period_id = 999L,
    person_id = 1L,
    observation_period_start_date = "2010-01-01",
    observation_period_end_date = "2011-12-31",
    period_type_concept_id = 44818518L
  )
  DBI::dbWriteTable(handle$conn, "observation_period", extra_period,
                    append = TRUE)
  withr::with_options(list(nfilter.subset = 0), {
    observed <- .computeDerivedColumns(
      handle,
      list(
        list(kind = "obs_duration", name = "total_days",
             period_policy = "total"),
        list(kind = "obs_duration", name = "first_days",
             period_policy = "first"),
        list(kind = "obs_duration", name = "last_days",
             period_policy = "last"),
        list(kind = "obs_duration", name = "longest_days",
             period_policy = "longest"),
        list(kind = "prior_obs", name = "prior",
             reference_date = "2010-06-01", period_policy = "containing"),
        list(kind = "followup", name = "followup",
             reference_date = "2010-06-01", period_policy = "containing")
      ),
      person_ids = 1:3
    )
  })
  p1 <- observed[observed$person_id == 1L, , drop = FALSE]
  early_days <- as.integer(as.Date("2011-12-31") - as.Date("2010-01-01"))
  recent_days <- as.integer(as.Date("2024-12-31") - as.Date("2020-01-01"))
  expect_equal(p1$total_days, early_days + recent_days)
  expect_equal(p1$first_days, early_days)
  expect_equal(p1$last_days, recent_days)
  expect_equal(p1$longest_days, max(early_days, recent_days))
  expect_equal(p1$prior,
               as.integer(as.Date("2010-06-01") - as.Date("2010-01-01")))
  expect_equal(p1$followup,
               as.integer(as.Date("2011-12-31") - as.Date("2010-06-01")))

  overlap <- extra_period
  overlap$observation_period_id <- 1000L
  overlap$observation_period_start_date <- "2010-05-01"
  overlap$observation_period_end_date <- "2010-07-01"
  DBI::dbWriteTable(handle$conn, "observation_period", overlap, append = TRUE)
  withr::with_options(list(nfilter.subset = 0), {
    expect_error(
      .computeDerivedColumns(
        handle,
        list(list(kind = "prior_obs", name = "prior",
                  reference_date = "2010-06-01",
                  period_policy = "containing")),
        person_ids = 1:3
      ),
      "covered by multiple periods"
    )
  })
})

test_that("index-derived age requires an index cohort", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .computeDerivedColumns(
      handle,
      list(list(kind = "age", name = "age", reference = "index")),
      person_ids = 1:3
    ),
    "requires a cohort"
  )
})

test_that("derived annual age treats future birth years as missing", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  DBI::dbExecute(handle$conn,
    "UPDATE person SET year_of_birth = 2030 WHERE person_id = 1")

  result <- .computeDerivedColumns(
    handle,
    list(list(kind = "age", name = "age", reference = "today",
              reference_date = "2024-07-01")),
    person_ids = 1:3
  )
  expect_true(is.na(result$age[result$person_id == 1L]))
})

test_that("extractTable feature mode retains persons without matching events", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  specs <- list(
    present = list(type = "boolean", name = "present",
                   concept_set = 3004410L),
    n = list(type = "count", name = "n", concept_set = 3004410L),
    mean = list(type = "mean_value", name = "mean",
                concept_set = 3004410L, value_column = "value_as_number")
  )

  withr::with_options(list(nfilter.subset = 3), {
    result <- .extractTable(
      handle, "measurement", concept_filter = 3004410L,
      person_ids = 1:6, representation = "features",
      feature_specs = specs, translate_concepts = FALSE
    )
    expect_equal(result$person_id, 1:6)
    expect_equal(result$present, c(1L, 0L, 1L, 0L, 1L, 0L))
    expect_equal(result$n, c(1L, 0L, 1L, 0L, 1L, 0L))
    expect_true(all(is.na(result$mean[c(2L, 4L, 6L)])))
  })

  withr::with_options(list(nfilter.subset = 0), {
    empty_specs <- lapply(specs, function(spec) {
      spec$concept_set <- 99999999L
      spec
    })
    empty <- .extractTable(
      handle, "measurement", concept_filter = 99999999L,
      person_ids = 1:4, representation = "features",
      feature_specs = empty_specs, translate_concepts = FALSE
    )
    expect_equal(empty$person_id, 1:4)
    expect_equal(names(empty), c("person_id", "present", "n", "mean"))
    expect_equal(empty$present, rep(0L, 4))
    expect_equal(empty$n, rep(0L, 4))
    expect_true(all(is.na(empty$mean)))
  })
})

test_that("feature aliases cannot disguise identifiers or linkage columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .extractTable(
      handle, "measurement", representation = "features",
      feature_specs = list(leak = list(
        type = "first_value", name = "leak", concept_set = 3004410L,
        value_column = "measurement_id"
      )), translate_concepts = FALSE
    ),
    "identifier or blocked column"
  )
  expect_error(
    .extractTable(
      handle, "measurement", representation = "features",
      feature_specs = list(leak = list(
        type = "latest_value", name = "leak", concept_set = 3004410L,
        value_column = "person_id"
      )), translate_concepts = FALSE
    ),
    "identifier or blocked column"
  )
  expect_error(
    .extractTable(
      handle, "measurement", representation = "features",
      feature_specs = list(list(
        type = "count", name = "person_id", concept_set = 3004410L
      )), translate_concepts = FALSE
    ),
    "reserved"
  )
  expect_error(
    .extractTable(
      handle, "measurement", representation = "features",
      feature_specs = list(list(
        type = "boolean", name = "cohort_row_id", concept_set = 3004410L
      )), translate_concepts = FALSE
    ),
    "reserved"
  )
})

test_that("feature output names are unique and value columns must exist", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .extractTable(
      handle, "measurement", representation = "features",
      feature_specs = list(
        a = list(type = "count", name = "same", concept_set = 3004410L),
        b = list(type = "boolean", name = "SAME", concept_set = 3025315L)
      ), translate_concepts = FALSE
    ),
    "must be unique"
  )
  expect_error(
    .extractTable(
      handle, "measurement", representation = "features",
      feature_specs = list(x = list(
        type = "mean_value", name = "x", concept_set = 3004410L,
        value_column = "not_a_column"
      )), translate_concepts = FALSE
    ),
    "does not exist"
  )
})

test_that("extractTable orders first/latest by date and an internal OMOP key", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  seeded <- do.call(rbind, lapply(1:3, function(person) {
    base <- 1000L + person * 10L
    data.frame(
      measurement_id = c(base + 2L, base + 3L, base + 1L),
      person_id = person,
      measurement_concept_id = 777001L,
      measurement_date = c("2020-03-01", "2020-01-01", "2020-03-01"),
      measurement_type_concept_id = 44818518L,
      value_as_number = c(person * 10 + 99, person * 10 + 1,
                          person * 10 + 2),
      value_as_concept_id = NA_integer_, unit_concept_id = 8554L,
      range_low = 0, range_high = 100,
      visit_occurrence_id = NA_integer_
    )
  }))
  DBI::dbWriteTable(handle$conn, "measurement", seeded, append = TRUE)

  specs <- list(
    first = list(type = "first_value", name = "first",
                 concept_set = 777001L, value_column = "value_as_number"),
    latest = list(type = "latest_value", name = "latest",
                  concept_set = 777001L, value_column = "value_as_number")
  )
  withr::with_options(list(nfilter.subset = 3), {
    result <- .extractTable(
      handle, "measurement", columns = "value_as_number",
      concept_filter = 777001L, person_ids = 1:4,
      representation = "features", feature_specs = specs,
      translate_concepts = FALSE
    )
  })

  expect_equal(result$person_id, 1:4)
  expect_equal(result$first, c(11, 21, 31, NA))
  expect_equal(result$latest, c(12, 22, 32, NA))
  expect_false("dsomop_event_order_id" %in% names(result))
})

test_that("time_since supports a fixed reference without collapsing episodes", {
  events <- data.frame(
    condition_occurrence_id = c(1L, 2L, 3L),
    person_id = c(1L, 1L, 2L),
    condition_concept_id = 100L,
    condition_start_date = as.Date(c(
      "2020-02-01", "2020-04-01", "2020-01-31"
    ))
  )
  day_spec <- list(recency = list(
    type = "time_since", name = "recency", concept_set = 100L,
    reference_date = "2020-03-01", unit = "day"
  ))
  month_spec <- list(recency = list(
    type = "time_since", name = "recency", concept_set = 100L,
    reference_date = "2020-03-30", unit = "month"
  ))

  days <- .toFeatures(events, "condition_occurrence", day_spec,
                      person_ids = 1:3)
  months <- .toFeatures(events, "condition_occurrence", month_spec,
                        person_ids = 1:3)
  # Future records do not replace the latest event at/before the reference.
  expect_equal(days$recency, c(29L, 30L, NA_integer_))
  expect_equal(months$recency, c(1L, 1L, NA_integer_))

  no_reference <- day_spec
  no_reference$recency$reference_date <- NULL
  expect_error(
    .toFeatures(events, "condition_occurrence", no_reference),
    "fixed reference_date.*episode-aware"
  )
})

test_that("each feature concept set keeps descendant expansion at execution", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  specs <- list(
    respiratory = list(
      type = "boolean", name = "respiratory",
      concept_set = list(concepts = 4000001L, include_descendants = TRUE)
    ),
    copd = list(
      type = "boolean", name = "copd", concept_set = 255573L
    )
  )

  withr::with_options(list(nfilter.subset = 3), {
    result <- .extractTable(
      handle, "condition_occurrence", person_ids = 1:15,
      representation = "features",
      feature_specs = specs, translate_concepts = FALSE
    )
  })

  respiratory_people <- c(2L, 4L, 5L, 7L, 9L, 13L)
  copd_people <- c(5L, 7L, 9L, 13L)
  expect_equal(result$person_id, 1:15)
  expect_equal(result$respiratory,
               as.integer(result$person_id %in% respiratory_people))
  expect_equal(result$copd,
               as.integer(result$person_id %in% copd_people))
})

test_that("unscoped and alternate-concept features keep independent scopes", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  specs <- list(
    diversity = list(
      type = "n_distinct", name = "diversity", concept_set = integer(0)
    ),
    percent_records = list(
      type = "count", name = "percent_records", concept_set = 8554L,
      concept_col = "unit_concept_id"
    )
  )

  withr::with_options(list(nfilter.subset = 3), {
    result <- .extractTable(
      handle, "measurement", columns = "value_as_number",
      person_ids = 1:15, representation = "features",
      feature_specs = specs, translate_concepts = FALSE
    )
  })

  # The unscoped reducer sees both HbA1c and body-weight concepts for person 1;
  # the alternate-column reducer sees only percent-unit HbA1c records.
  expect_equal(result$diversity[result$person_id == 1L], 2L)
  expect_equal(result$percent_records,
               as.integer(result$person_id %in% c(1L, 3L, 5L, 7L, 9L)))
  expect_equal(result$person_id, 1:15)

  unsafe <- specs
  unsafe$percent_records$concept_col <- "person_id"
  expect_error(
    .extractTable(
      handle, "measurement", person_ids = 1:15,
      representation = "features", feature_specs = unsafe,
      translate_concepts = FALSE
    ),
    "not a safe concept column"
  )
})

.seed_episode_identity_contract <- function(handle) {
  cohort <- data.frame(
    subject_id = rep(1:3, each = 2L),
    cohort_start_date = rep(c("2020-01-01", "2021-01-01"), 3L),
    cohort_end_date = rep(c("2020-01-10", "2021-01-10"), 3L)
  )
  DBI::dbWriteTable(handle$conn, "episode_identity_cohort", cohort,
                    temporary = TRUE)

  events <- data.frame(
    condition_occurrence_id = 9001:9006,
    person_id = rep(1:3, each = 2L),
    condition_concept_id = rep(c(888001L, 888002L), 3L),
    condition_start_date = rep("2020-01-06", 6L),
    condition_end_date = rep("2020-01-06", 6L),
    condition_type_concept_id = 44818518L,
    visit_occurrence_id = NA_integer_
  )
  DBI::dbWriteTable(handle$conn, "condition_occurrence", events, append = TRUE)
  invisible(cohort)
}

test_that("longitudinal outputs share one stable cohort episode identity", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_episode_identity_contract(handle)

  withr::with_options(list(nfilter.subset = 3), {
    baseline <- .extractBaseline(
      handle, "episode_identity_cohort", columns = "gender_concept_id",
      translate_concepts = FALSE
    )
    membership <- .extractCohortMembership(
      handle, "episode_identity_cohort", cohort_definition_id = 42L,
      date_handling = "remove"
    )
    survival <- .extractSurvival(
      handle, "episode_identity_cohort",
      outcome = list(table = "condition_occurrence",
                     concept_set = 888002L),
      tar = list(start_offset = 0L, end_offset = 9L)
    )
    intervals <- .extractIntervalsLong(
      handle, "episode_identity_cohort", "condition_occurrence",
      concept_filter = list(condition_occurrence = 888001L)
    )
  })

  expect_equal(baseline$row_id, baseline$cohort_row_id)
  expect_equal(membership$row_id, membership$cohort_row_id)
  expect_equal(survival$row_id, survival$cohort_row_id)
  expect_setequal(baseline$cohort_row_id, 1:6)
  expect_setequal(membership$cohort_row_id, 1:6)
  expect_setequal(survival$cohort_row_id, 1:6)

  # Outcomes exist only near the first episode for each person; the second
  # episode remains a distinct censored row rather than being collapsed.
  expect_equal(survival$event, c(1L, 0L, 1L, 0L, 1L, 0L))
  expect_equal(survival$time_to_event_days,
               c(5L, 9L, 5L, 9L, 5L, 9L))

  # The default relationship is episode overlap, so January events are not
  # multiplied into the later recurrent episodes for the same people.
  expect_equal(intervals$row_id, seq_len(nrow(intervals)))
  expect_setequal(intervals$cohort_row_id, c(1L, 3L, 5L))
  expect_equal(nrow(intervals), 3L)
  expect_equal(
    intervals$start_days_from_index[intervals$cohort_row_id %in% c(1L, 3L, 5L)],
    rep(5L, 3L)
  )
})

test_that("episode feature windows are contained and keep empty episodes", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_episode_identity_contract(handle)

  spec <- list(n = list(
    type = "count", name = "n", concept_set = 888001L,
    time_window = list(start = -5L, end = 5L)
  ))
  features <- withr::with_options(list(nfilter.subset = 0),
    .extractTable(
      handle, "condition_occurrence", concept_filter = 888001L,
      cohort_table = "episode_identity_cohort",
      temporal = list(index_window = list(start = -10L, end = 10L)),
      representation = "features", representation_grain = "episode",
      feature_specs = spec, translate_concepts = FALSE
    ))
  expect_equal(features$cohort_row_id, 1:6)
  expect_equal(features$n, c(1L, 0L, 1L, 0L, 1L, 0L))

  too_wide <- spec
  too_wide$n$time_window$start <- -20L
  expect_error(
    .extractTable(
      handle, "condition_occurrence", concept_filter = 888001L,
      cohort_table = "episode_identity_cohort",
      temporal = list(index_window = list(start = -10L, end = 10L)),
      representation = "features", representation_grain = "episode",
      feature_specs = too_wide, translate_concepts = FALSE
    ),
    "must be contained"
  )

  empty_spec <- spec
  empty_spec$n$concept_set <- 99999999L
  empty <- withr::with_options(list(nfilter.subset = 0),
    .extractTable(
      handle, "condition_occurrence", concept_filter = 99999999L,
      cohort_table = "episode_identity_cohort",
      temporal = list(index_window = list(start = -10L, end = 10L)),
      representation = "features", representation_grain = "episode",
      feature_specs = empty_spec, translate_concepts = FALSE
    ))
  expect_equal(empty$cohort_row_id, 1:6)
  expect_equal(empty$n, rep(0L, 6L))
})

test_that("empty intervals retain the cohort episode column in their schema", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_episode_identity_contract(handle)

  withr::with_options(list(nfilter.subset = 0), {
    empty <- .extractIntervalsLong(
      handle, "episode_identity_cohort", "condition_occurrence",
      concept_filter = list(condition_occurrence = 99999999L)
    )
  })
  expect_equal(nrow(empty), 0L)
  expect_equal(
    names(empty),
    c("row_id", "cohort_row_id", "subject_id", "interval_type",
      "concept_id", "start_days_from_index", "end_days_from_index")
  )
})
