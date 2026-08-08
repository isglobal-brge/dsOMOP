.seed_temporal_sql_fixture <- function(handle) {
  DBI::dbExecute(
    handle$conn,
    paste(
      "UPDATE observation_period",
      "SET observation_period_start_date = '2039-01-01',",
      "observation_period_end_date = '2042-12-31'",
      "WHERE person_id IN (1, 2, 3)"
    )
  )
  cohort <- data.frame(
    subject_id = rep(1:3, each = 2L),
    cohort_start_date = rep(c("2040-01-01", "2041-01-01"), 3L),
    cohort_end_date = rep(c("2040-01-11", "2041-01-11"), 3L),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(
    handle$conn, "temporal_sql_cohort", cohort, temporary = TRUE
  )
  register_test_temp(handle, "temporal_sql_cohort")

  offsets <- rep(c(0L, 9L, 10L), each = nrow(cohort))
  episode_rows <- cohort[rep(seq_len(nrow(cohort)), times = 3L), , drop = FALSE]
  concepts <- c(
    rep(765432L, nrow(cohort) * 3L),
    rep(765433L, nrow(cohort))
  )
  event_cohort <- rbind(
    episode_rows,
    cohort
  )
  event_offsets <- c(offsets, rep(10L, nrow(cohort)))
  event_count <- nrow(event_cohort)
  events <- data.frame(
    condition_occurrence_id = 900000L + seq_len(event_count),
    person_id = event_cohort$subject_id,
    condition_concept_id = concepts,
    condition_start_date = as.character(
      as.Date(event_cohort$cohort_start_date) + event_offsets
    ),
    condition_end_date = as.character(
      as.Date(event_cohort$cohort_start_date) + event_offsets
    ),
    condition_type_concept_id = 44818518L,
    visit_occurrence_id = NA_integer_,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(
    handle$conn, "condition_occurrence", events, append = TRUE
  )
  invisible(cohort)
}

.order_temporal_covariates <- function(x) {
  x$rowId <- as.integer(x$rowId)
  x$timeId <- as.integer(x$timeId)
  x$covariateId <- as.numeric(x$covariateId)
  x$covariateValue <- as.numeric(x$covariateValue)
  x[order(x$rowId, x$timeId, x$covariateId), , drop = FALSE]
}

test_that("SQL temporal components equal the in-memory episode extractor", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_temporal_sql_fixture(handle)

  withr::with_options(list(
    nfilter.subset = 3,
    dsomop.max_memory_rows = 100000L
  ), {
    compiled <- .compileTemporalSqlComponents(
      handle = handle,
      cohort_table = "temporal_sql_cohort",
      table = "condition_occurrence",
      concept_filter = 765432L,
      bin_width = 10L,
      window_start = 0L,
      window_end = 10L,
      analyses = c("binary", "count")
    )
    in_memory <- .extractTemporalCovariates(
      handle = handle,
      cohort_table = "temporal_sql_cohort",
      table = "condition_occurrence",
      concept_filter = 765432L,
      bin_width = 10L,
      window_start = 0L,
      window_end = 10L,
      analyses = c("binary", "count")
    )

    qualifying <- .executeQuery(
      handle, compiled$validations$min_persons$sql
    )$n_persons[[1L]]
    expect_silent(.assertMinPersons(n_persons = qualifying))

    sql_covariates <- .executeQuery(
      handle, compiled$components$temporalCovariates$sql
    )
    sql_covariates <- .normalizeTemporalSqlChunk(
      sql_covariates, compiled$components$temporalCovariates
    )
    expect_equal(
      .order_temporal_covariates(sql_covariates),
      .order_temporal_covariates(in_memory$temporalCovariates),
      ignore_attr = TRUE
    )

    sql_person_ref <- .executeQuery(
      handle, compiled$components$personRef$sql
    )
    sql_person_ref <- .normalizeTemporalSqlChunk(
      sql_person_ref, compiled$components$personRef
    )
    sql_person_ref$rowId <- as.integer(sql_person_ref$rowId)
    expect_equal(sql_person_ref, in_memory$personRef, ignore_attr = TRUE)
    expect_equal(compiled$components$timeRef$data, in_memory$timeRef)
    expect_equal(
      compiled$components$covariateRef$data,
      in_memory$covariateRef,
      ignore_attr = TRUE
    )

    # The 10-day bin is [0, 9]; the inclusive final day receives its own bin.
    binary_id <- 765432001
    count_id <- 765432002
    first_bin_counts <- sql_covariates[
      sql_covariates$timeId == 1L &
        as.numeric(sql_covariates$covariateId) == count_id,
      "covariateValue"
    ]
    second_bin_binary <- sql_covariates[
      sql_covariates$timeId == 2L &
        as.numeric(sql_covariates$covariateId) == binary_id,
      "covariateValue"
    ]
    expect_equal(as.numeric(first_bin_counts), rep(2, 6L))
    expect_equal(as.numeric(second_bin_binary), rep(1, 6L))

    # Only generated relative identifiers leave the outer covariate query.
    expect_identical(
      names(sql_covariates),
      c("rowId", "timeId", "covariateId", "covariateValue")
    )
    expect_false(any(grepl(
      "_date$|occurrence_id$|person_id$", names(sql_covariates)
    )))
  })
})

test_that("person-period SQL streams the complete recurrent-episode roster", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  cohort <- .seed_temporal_sql_fixture(handle)

  withr::with_options(list(
    nfilter.subset = 3,
    dsomop.max_memory_rows = 100000L
  ), {
    compiled <- .compileTemporalSqlComponents(
      handle = handle,
      cohort_table = "temporal_sql_cohort",
      table = "condition_occurrence",
      concept_filter = 765432L,
      bin_width = 10L,
      window_start = 0L,
      window_end = 10L,
      analyses = "binary",
      output_type = "person_period",
      grain = "episode",
      time_origin = "index"
    )
    in_memory <- .extractPersonPeriod(
      handle = handle,
      cohort_table = "temporal_sql_cohort",
      table = "condition_occurrence",
      concept_filter = 765432L,
      bin_width = 10L,
      window_start = 0L,
      window_end = 10L,
      analyses = "binary",
      grain = "episode",
      time_origin = "index"
    )
    sql_periods <- .executeQuery(
      handle, compiled$components$personPeriods$sql
    )
    sql_periods <- .normalizeTemporalSqlChunk(
      sql_periods, compiled$components$personPeriods
    )
    for (column in names(sql_periods)) {
      sql_periods[[column]] <- as.integer(sql_periods[[column]])
    }

    expect_equal(sql_periods, in_memory$personPeriods, ignore_attr = TRUE)
    expect_equal(nrow(sql_periods), nrow(cohort) * 2L)
    expect_equal(
      unname(as.integer(table(sql_periods$rowId))), rep(2L, nrow(cohort))
    )
    expect_identical(
      names(sql_periods),
      c(
        "rowId", "timeId", "startDay", "endDay",
        "observationStartDay", "observationEndDay", "daysObserved"
      )
    )
    expect_false(any(grepl("date|person_id", names(sql_periods))))
  })
})

.seed_observation_aware_panel <- function(handle) {
  cohort <- data.frame(
    subject_id = 1:3,
    cohort_start_date = rep("2020-01-10", 3L),
    cohort_end_date = rep("2020-01-20", 3L),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(
    handle$conn, "observation_aware_cohort", cohort, temporary = TRUE
  )
  register_test_temp(handle, "observation_aware_cohort")
  DBI::dbExecute(
    handle$conn,
    paste(
      "UPDATE observation_period",
      "SET observation_period_start_date = '2020-01-08',",
      "observation_period_end_date = '2020-01-13'",
      "WHERE person_id IN (1, 2, 3)"
    )
  )

  offsets <- rep(c(-4L, -2L, 3L, 4L), each = 3L)
  people <- rep(1:3, times = 4L)
  events <- data.frame(
    condition_occurrence_id = 880000L + seq_along(offsets),
    person_id = people,
    condition_concept_id = 888001L,
    condition_start_date = as.character(
      as.Date("2020-01-10") + offsets
    ),
    condition_end_date = as.character(
      as.Date("2020-01-10") + offsets
    ),
    condition_type_concept_id = 44818518L,
    visit_occurrence_id = NA_integer_,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(
    handle$conn, "condition_occurrence", events, append = TRUE
  )
  invisible(cohort)
}

test_that("temporal panels use only the observation period covering index", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_observation_aware_panel(handle)

  withr::with_options(list(
    nfilter.subset = 3,
    dsomop.max_memory_rows = 100000L
  ), {
    compiled <- .compileTemporalSqlComponents(
      handle, "observation_aware_cohort", "condition_occurrence",
      concept_filter = 888001L, bin_width = 5L,
      window_start = -5L, window_end = 5L,
      analyses = c("binary", "count"),
      output_type = "person_period", grain = "episode",
      time_origin = "index"
    )
    expect_equal(
      as.numeric(.executeQuery(
        handle, compiled$validations$observation_period$sql
      )[[1L]][[1L]]),
      0
    )

    memory <- .extractPersonPeriod(
      handle, "observation_aware_cohort", "condition_occurrence",
      concept_filter = 888001L, bin_width = 5L,
      window_start = -5L, window_end = 5L,
      analyses = c("binary", "count"), grain = "episode",
      time_origin = "index"
    )
    sql_periods <- .normalizeTemporalSqlChunk(
      .executeQuery(handle, compiled$components$personPeriods$sql),
      compiled$components$personPeriods
    )
    for (column in names(sql_periods)) {
      sql_periods[[column]] <- as.integer(sql_periods[[column]])
    }
    expect_equal(sql_periods, memory$personPeriods, ignore_attr = TRUE)
    expect_equal(nrow(sql_periods), 6L)
    expect_equal(sql_periods$timeId, rep(c(1L, 2L), 3L))
    expect_equal(sql_periods$startDay, rep(c(-5L, 0L), 3L))
    expect_equal(sql_periods$endDay, rep(c(-1L, 4L), 3L))
    expect_equal(
      sql_periods$observationStartDay, rep(c(-2L, 0L), 3L)
    )
    expect_equal(
      sql_periods$observationEndDay, rep(c(-1L, 3L), 3L)
    )
    expect_equal(sql_periods$daysObserved, rep(c(2L, 4L), 3L))

    sql_covariates <- .normalizeTemporalSqlChunk(
      .executeQuery(handle, compiled$components$temporalCovariates$sql),
      compiled$components$temporalCovariates
    )
    expect_equal(
      .order_temporal_covariates(sql_covariates),
      .order_temporal_covariates(memory$temporalCovariates),
      ignore_attr = TRUE
    )
    counts <- sql_covariates[
      as.numeric(sql_covariates$covariateId) == 888001002,
      , drop = FALSE
    ]
    expect_equal(nrow(counts), 6L)
    expect_equal(as.numeric(counts$covariateValue), rep(1, 6L))
  })
})

test_that("temporal panels fail closed on missing or duplicate index OP", {
  missing <- create_test_handle()
  on.exit(cleanup_handle(missing), add = TRUE)
  .buildBlueprint(missing)
  .seed_observation_aware_panel(missing)
  DBI::dbExecute(
    missing$conn, "DELETE FROM observation_period WHERE person_id = 1"
  )
  expect_error(
    .extractTemporalCovariates(
      missing, "observation_aware_cohort", "condition_occurrence",
      concept_filter = 888001L, bin_width = 5L,
      window_start = -5L, window_end = 5L
    ),
    "exactly one observation_period"
  )
  missing_compiled <- .compileTemporalSqlComponents(
    missing, "observation_aware_cohort", "condition_occurrence",
    concept_filter = 888001L, bin_width = 5L,
    window_start = -5L, window_end = 5L
  )
  expect_equal(
    as.numeric(.executeQuery(
      missing, missing_compiled$validations$observation_period$sql
    )[[1L]][[1L]]),
    1
  )

  duplicate <- create_test_handle()
  on.exit(cleanup_handle(duplicate), add = TRUE)
  .buildBlueprint(duplicate)
  .seed_observation_aware_panel(duplicate)
  DBI::dbExecute(
    duplicate$conn,
    paste(
      "INSERT INTO observation_period",
      "(observation_period_id, person_id,",
      "observation_period_start_date, observation_period_end_date,",
      "period_type_concept_id)",
      "VALUES (999991, 1, '2020-01-09', '2020-01-12', 44814724)"
    )
  )
  expect_error(
    .extractPersonPeriod(
      duplicate, "observation_aware_cohort", "condition_occurrence",
      concept_filter = 888001L, bin_width = 5L,
      window_start = -5L, window_end = 5L,
      grain = "episode", time_origin = "index"
    ),
    "exactly one observation_period"
  )
  duplicate_compiled <- .compileTemporalSqlComponents(
    duplicate, "observation_aware_cohort", "condition_occurrence",
    concept_filter = 888001L, bin_width = 5L,
    window_start = -5L, window_end = 5L,
    output_type = "person_period", grain = "episode",
    time_origin = "index"
  )
  expect_equal(
    as.numeric(.executeQuery(
      duplicate, duplicate_compiled$validations$observation_period$sql
    )[[1L]][[1L]]),
    1
  )
})

test_that("temporal OP diagnostics run only after the disclosure gate", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .buildBlueprint(handle)
  .seed_observation_aware_panel(handle)
  DBI::dbExecute(
    handle$conn, "DELETE FROM observation_period WHERE person_id = 1"
  )

  withr::local_options(list(nfilter.subset = 4L))
  expect_error(
    .extractTemporalCovariates(
      handle, "observation_aware_cohort", "condition_occurrence",
      concept_filter = 888001L, bin_width = 5L,
      window_start = -5L, window_end = 5L
    ),
    "Disclosive: operation blocked"
  )
})

test_that("dynamic concept references stay SQL-first and bounded", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .seed_temporal_sql_fixture(handle)

  compiled <- .compileTemporalSqlComponents(
    handle = handle,
    cohort_table = "temporal_sql_cohort",
    table = "condition_occurrence",
    concept_filter = NULL,
    bin_width = 10L,
    window_start = 0L,
    window_end = 10L,
    analyses = c("binary", "count")
  )
  expect_identical(compiled$components$covariateRef$kind, "sql")
  expect_identical(compiled$validations$max_concepts$kind, "max_value")
  distinct_concepts <- .executeQuery(
    handle, compiled$validations$max_concepts$sql
  )$value[[1L]]
  expect_equal(as.numeric(distinct_concepts), 2)

  ref <- .executeQuery(handle, compiled$components$covariateRef$sql)
  ref <- .normalizeTemporalSqlChunk(ref, compiled$components$covariateRef)
  expect_setequal(as.integer(ref$conceptId), c(765432L, 765433L))
  expect_setequal(as.integer(ref$analysisId), c(1L, 2L))
  expect_true(all(grepl("^x[0-9]+_(binary|count)$", ref$covariateName)))
  expect_identical(
    names(ref),
    c("covariateId", "covariateName", "analysisId", "conceptId")
  )
})

test_that("temporal routes resolve the same OHDSI concept-set specification", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  cohort <- .seed_temporal_sql_fixture(handle)

  # Seed one descendant, one excluded descendant, and one reverse-mapped
  # source concept in every recurrent episode.
  expanded_concepts <- rep(c(255573L, 317009L, 44826430L),
                           each = nrow(cohort))
  expanded_cohort <- cohort[rep(seq_len(nrow(cohort)), times = 3L), ,
                            drop = FALSE]
  offsets <- rep(c(4L, 5L, 6L), each = nrow(cohort))
  extra <- data.frame(
    condition_occurrence_id = 990000L + seq_along(expanded_concepts),
    person_id = expanded_cohort$subject_id,
    condition_concept_id = expanded_concepts,
    condition_start_date = as.character(
      as.Date(expanded_cohort$cohort_start_date) + offsets
    ),
    condition_end_date = as.character(
      as.Date(expanded_cohort$cohort_start_date) + offsets
    ),
    condition_type_concept_id = 44818518L,
    visit_occurrence_id = NA_integer_,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(handle$conn, "condition_occurrence", extra, append = TRUE)

  compare_routes <- function(spec) {
    compiled <- .compileTemporalSqlComponents(
      handle = handle, cohort_table = "temporal_sql_cohort",
      table = "condition_occurrence", concept_filter = spec,
      bin_width = 10L, window_start = 0L, window_end = 10L,
      analyses = "binary"
    )
    in_memory <- .extractTemporalCovariates(
      handle = handle, cohort_table = "temporal_sql_cohort",
      table = "condition_occurrence", concept_filter = spec,
      bin_width = 10L, window_start = 0L, window_end = 10L,
      analyses = "binary"
    )
    sql_covariates <- .normalizeTemporalSqlChunk(
      .executeQuery(handle, compiled$components$temporalCovariates$sql),
      compiled$components$temporalCovariates
    )
    expect_equal(
      .order_temporal_covariates(sql_covariates),
      .order_temporal_covariates(in_memory$temporalCovariates),
      ignore_attr = TRUE
    )
    expect_equal(
      compiled$components$covariateRef$data,
      in_memory$covariateRef,
      ignore_attr = TRUE
    )
    as.integer(in_memory$covariateRef$conceptId)
  }

  withr::with_options(list(nfilter.subset = 3), {
    descendants <- compare_routes(list(
      concepts = 4000001L, include_descendants = TRUE,
      include_mapped = FALSE, exclude = 317009L
    ))
    expect_setequal(descendants, c(4000001L, 255573L))
    expect_false(317009L %in% descendants)

    mapped <- compare_routes(list(
      concepts = 4174977L, include_descendants = FALSE,
      include_mapped = TRUE, exclude = integer(0)
    ))
    expect_setequal(mapped, c(4174977L, 44826430L))
  })
})

test_that("temporal SQL compiler validates fail closed", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .compileTemporalSqlComponents(
      handle, NULL, "condition_occurrence"
    ),
    "requires a cohort"
  )
  expect_error(
    .compileTemporalSqlComponents(
      handle, "unused", "condition_occurrence", bin_width = 2.5
    ),
    "finite integer"
  )
  expect_error(
    .compileTemporalSqlComponents(
      handle, "unused", "condition_occurrence", bin_width = 0L
    ),
    "greater than zero"
  )
  expect_error(
    .compileTemporalSqlComponents(
      handle, "unused", "condition_occurrence",
      window_start = 1L, window_end = 0L
    ),
    "not be after"
  )
  expect_error(
    .compileTemporalSqlComponents(
      handle, "unused", "condition_occurrence", analyses = "median"
    ),
    "binary and count"
  )
  expect_error(
    .compileTemporalSqlComponents(
      handle, "unused", "condition_occurrence", concept_filter = -1L
    ),
    "non-negative integer"
  )
  expect_error(
    withr::with_options(list(dsomop.max_pivot_concepts = 1L), {
      .compileTemporalSqlComponents(
        handle, "unused", "condition_occurrence",
        concept_filter = c(1L, 2L)
      )
    }),
    "concept cap"
  )
  expect_error(
    .compileTemporalSqlComponents(
      handle, "unused", "person"
    ),
    "event date"
  )
  expect_error(
    .compileTemporalSqlComponents(
      handle, "unused", "condition_occurrence",
      output_type = "person_period", grain = "person", time_origin = "index"
    ),
    "grain"
  )
  expect_error(
    .compileTemporalSqlComponents(
      handle, "unused", "condition_occurrence",
      output_type = "person_period", grain = "episode", time_origin = "calendar"
    ),
    "time_origin"
  )
})

test_that("temporal scalar SQL uses reviewed dialect-specific forms", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  handle$target_dialect <- "bigquery"
  expect_match(
    .temporalSqlFloorDivide("days_from_index", 10L, handle),
    "AS INT64",
    fixed = TRUE
  )
  expect_match(
    .temporalSqlConceptName("concept_id", "binary", handle),
    "CONCAT(",
    fixed = TRUE
  )

  handle$target_dialect <- "mysql"
  expect_match(
    .temporalSqlDateDiffDays(handle, "event_date", "index_date"),
    "DATEDIFF(event_date, index_date)",
    fixed = TRUE
  )
  expect_match(
    .temporalSqlBigInteger("concept_id", handle), "AS SIGNED", fixed = TRUE
  )

  handle$target_dialect <- "oracle"
  grid <- .temporalNumberGridSql(10000L, handle)
  expect_equal(length(gregexpr("FROM DUAL", grid$from, fixed = TRUE)[[1L]]),
               40L)
  expect_match(grid$index, "d3.n * 1000", fixed = TRUE)

  component <- .temporalSqlComponent("SELECT 1 AS rowId", "rowId")
  expect_error(
    .normalizeTemporalSqlChunk(data.frame(wrong = 1L), component),
    "unexpected column shape"
  )
})
