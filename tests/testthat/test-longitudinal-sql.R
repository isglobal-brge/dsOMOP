seed_longitudinal_intervals <- function(handle) {
  cohort <- data.frame(
    cohort_row_id = 1:6,
    subject_id = rep(1:3, each = 2L),
    cohort_start_date = rep(c("2020-01-01", "2020-06-01"), 3L),
    cohort_end_date = rep(c("2020-01-31", "2020-06-30"), 3L)
  )
  DBI::dbWriteTable(
    handle$conn, "longitudinal_interval_cohort", cohort, temporary = TRUE
  )
  events <- do.call(rbind, lapply(1:3, function(person) {
    data.frame(
      condition_occurrence_id = 9900000L + person * 100L + 1:4,
      person_id = person,
      condition_concept_id = c(990001L, 990002L, 990001L, 990002L),
      condition_start_date = c(
        "2020-01-05", "2020-01-15", "2020-06-03", "2020-06-20"
      ),
      condition_end_date = c(
        "2020-01-08", "2020-01-15", "2020-06-10", "2020-06-20"
      ),
      condition_type_concept_id = 44818518L,
      visit_occurrence_id = NA_integer_
    )
  }))
  DBI::dbWriteTable(handle$conn, "condition_occurrence", events, append = TRUE)
  invisible(cohort)
}

seed_overlapping_longitudinal_records <- function(handle) {
  cohort <- do.call(rbind, lapply(1:3, function(person) {
    data.frame(
      subject_id = person,
      cohort_start_date = c("2020-01-01", "2020-01-10", "2020-01-01"),
      cohort_end_date = c("2020-01-20", "2020-01-29", "2020-01-20"),
      stringsAsFactors = FALSE
    )
  }))
  DBI::dbWriteTable(
    handle$conn, "overlapping_longitudinal_cohort", cohort, temporary = TRUE
  )

  conditions <- do.call(rbind, lapply(1:3, function(person) {
    data.frame(
      condition_occurrence_id = 9920000L + person * 100L + 1:4,
      person_id = person,
      condition_concept_id = 992001L,
      condition_start_date = c(
        "2020-01-05", "2020-01-15", "2020-01-25", "2020-02-05"
      ),
      condition_end_date = c(
        "2020-01-06", "2020-01-16", "2020-01-26", "2020-02-06"
      ),
      condition_type_concept_id = 44818518L,
      visit_occurrence_id = NA_integer_,
      stringsAsFactors = FALSE
    )
  }))
  DBI::dbWriteTable(
    handle$conn, "condition_occurrence", conditions, append = TRUE
  )

  measurements <- do.call(rbind, lapply(1:3, function(person) {
    data.frame(
      measurement_id = 9930000L + person * 100L + 1:4,
      person_id = person,
      measurement_concept_id = 993001L,
      measurement_date = c(
        "2020-01-05", "2020-01-15", "2020-01-25", "2020-02-05"
      ),
      measurement_type_concept_id = 44818518L,
      value_as_number = c(1, 2, 3, 4),
      value_as_concept_id = NA_integer_,
      unit_concept_id = 8554L,
      range_low = 0,
      range_high = 5,
      visit_occurrence_id = NA_integer_,
      stringsAsFactors = FALSE
    )
  }))
  DBI::dbWriteTable(handle$conn, "measurement", measurements, append = TRUE)
  invisible(cohort)
}

set_longitudinal_person_key <- function(handle) {
  key <- .testPseudonymKey("longitudinal-resource")
  identity <- "test://resource-scoped/longitudinal"
  resource_id <- substr(
    as.character(openssl::sha256(charToRaw(identity))), 1L, 32L
  )
  environment <- paste(format(key), collapse = "")
  names(environment) <- paste0("DSOMOP_PSEUDONYM_KEY_", resource_id)
  environment <- c(
    environment,
    DSOMOP_PSEUDONYM_PROVIDER = "scoped",
    DSOMOP_PSEUDONYM_EPOCH = "1",
    DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false",
    DSOMOP_PSEUDONYM_ROOT = "",
    DSOMOP_PSEUDONYM_KEY = ""
  )
  withr::local_envvar(environment, .local_envir = parent.frame())
  handle$person_key <- key
  handle$person_key_identity <- identity
  handle$person_key_id <- .personKeyId(key)
  handle$person_key_provider <- "scoped"
  handle$person_key_epoch <- 1L
  handle$person_key_require_existing <- FALSE
  handle$person_key_contract_version <- 1L
  invisible(key)
}

test_that("interval SQL attaches records only to matching recurrent episodes", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  seed_longitudinal_intervals(handle)

  result <- withr::with_options(list(nfilter.subset = 0), {
    .extractIntervalsLongSql(
      handle, "longitudinal_interval_cohort", "condition_occurrence",
      concept_filter = list(condition_occurrence = c(990001L, 990002L))
    )
  })

  expect_equal(nrow(result), 12L)
  expect_equal(tabulate(result$cohort_row_id, nbins = 6L), rep(2L, 6L))
  expect_true(all(abs(result$start_days_from_index) <= 30L))
  expect_equal(result$row_id, seq_len(nrow(result)))
  expect_false(any(grepl("occurrence_id|_date$", names(result))))
})

test_that("interval relationship and repeated-event policies are explicit", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  seed_longitudinal_intervals(handle)

  first <- withr::with_options(list(nfilter.subset = 0), {
    .extractIntervalsLongSql(
      handle, "longitudinal_interval_cohort", "condition_occurrence",
      concept_filter = list(condition_occurrence = c(990001L, 990002L)),
      window = list(start = 0L, end = 30L),
      interval_match = "starts_in", event_select = "first"
    )
  })
  by_concept <- withr::with_options(list(nfilter.subset = 0), {
    .extractIntervalsLongSql(
      handle, "longitudinal_interval_cohort", "condition_occurrence",
      concept_filter = list(condition_occurrence = c(990001L, 990002L)),
      window = list(start = 0L, end = 30L),
      interval_match = "starts_in", event_select = "first",
      select_by = "episode_source_concept"
    )
  })
  active <- withr::with_options(list(nfilter.subset = 0), {
    .extractIntervalsLongSql(
      handle, "longitudinal_interval_cohort", "condition_occurrence",
      concept_filter = list(condition_occurrence = 990001L),
      window = list(at = 5L), interval_match = "active_at"
    )
  })

  expect_equal(nrow(first), 6L)
  expect_equal(nrow(by_concept), 12L)
  expect_equal(nrow(active), 6L)
  expect_true(all(active$start_days_from_index <= 5L))
  expect_true(all(active$end_days_from_index >= 5L))
})

test_that("overlapping interval episodes preserve all rows without accidental fan-out", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  seed_overlapping_longitudinal_records(handle)

  extract <- function(event_select = "all") {
    .extractIntervalsLongSql(
      handle, "overlapping_longitudinal_cohort", "condition_occurrence",
      concept_filter = list(condition_occurrence = 992001L),
      event_select = event_select
    )
  }

  withr::with_options(list(nfilter.subset = 3), {
    all_rows <- extract()
    first <- extract("first")
    last <- extract("last")

    # Each person has two distinct overlapping episodes. The exact duplicate
    # cohort row is canonicalized away, while the event in the overlap belongs
    # once to each distinct episode by design.
    expect_equal(nrow(all_rows), 12L)
    expect_equal(tabulate(all_rows$cohort_row_id, nbins = 6L), rep(2L, 6L))
    expect_equal(
      unname(split(all_rows$start_days_from_index, all_rows$cohort_row_id)),
      rep(list(c(4L, 14L), c(5L, 15L)), 3L)
    )

    # Selection is episode-scoped, never person-scoped: no overlapping episode
    # is lost and no selected source row is multiplied within one episode.
    expect_equal(first$cohort_row_id, 1:6)
    expect_equal(first$start_days_from_index, rep(c(4L, 5L), 3L))
    expect_equal(last$cohort_row_id, 1:6)
    expect_equal(last$start_days_from_index, rep(c(14L, 15L), 3L))
  })
})

test_that("point measurements retain repeated rows at overlapping episode grain", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  seed_overlapping_longitudinal_records(handle)

  result <- withr::with_options(list(nfilter.subset = 3), {
    .extractTable(
      handle,
      table = "measurement",
      columns = c("measurement_id", "person_id", "measurement_concept_id",
                  "measurement_date", "value_as_number"),
      concept_filter = 993001L,
      cohort_table = "overlapping_longitudinal_cohort",
      temporal = list(index_window = list(start = 0L, end = 19L)),
      add_cohort_date = TRUE,
      date_handling = "remove",
      translate_concepts = FALSE
    )
  })

  expect_equal(nrow(result), 12L)
  expect_equal(tabulate(result$cohort_row_id, nbins = 6L), rep(2L, 6L))
  expect_equal(
    unname(lapply(split(result$value_as_number, result$cohort_row_id), sort)),
    rep(list(c(1, 2), c(2, 3)), 3L)
  )
  expect_equal(
    unname(lapply(split(result$days_from_index, result$cohort_row_id), sort)),
    rep(list(c(4L, 14L), c(5L, 15L)), 3L)
  )
  expect_false(any(grepl("_date$|cohort_end_date", names(result))))
})

test_that("interval sources resolve standard OHDSI concept-set specifications", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  seed_longitudinal_intervals(handle)
  observed <- NULL
  testthat::local_mocked_bindings(
    .resolveConceptSet = function(handle, x) {
      observed <<- x
      990001L
    },
    .package = "dsOMOP"
  )

  result <- withr::with_options(list(nfilter.subset = 0), {
    .extractIntervalsLongSql(
      handle, "longitudinal_interval_cohort", "condition_occurrence",
      concept_filter = list(condition_occurrence = list(
        concepts = 123L, include_descendants = TRUE
      ))
    )
  })

  expect_true(observed$include_descendants)
  expect_equal(unique(result$concept_id), 990001L)
})

test_that("streamed interval plans publish one chunked Parquet file", {
  skip_if_not(.arrowAvailable())
  handle <- create_test_handle()
  set_longitudinal_person_key(handle)
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_longitudinal_")
  withr::local_options(list(
    dsstaging.base_dir = base,
    nfilter.subset = 3,
    dsomop.max_staged_rows = 100000L
  ))
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  plan <- structure(list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    outputs = list(intervals = list(
      type = "intervals_long",
      tables = "condition_occurrence",
      concept_filter = list(condition_occurrence = 201820L),
      interval_match = "overlaps",
      event_select = "all"
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  ), class = c("omop_plan", "list"))

  descriptor <- .planExecute(
    handle, plan, list(intervals = "D_intervals"), output_mode = "staged"
  )$intervals

  expect_identical(descriptor$metadata$layout, "file")
  path <- omopStagedDatasetPath(descriptor)
  expect_true(file.exists(path))
  landed <- as.data.frame(arrow::read_parquet(path))
  expect_gte(nrow(landed), descriptor$metadata$n_rows)
  expect_lt(
    nrow(landed) - descriptor$metadata$n_rows,
    .omopDisclosureSettings()$nfilter_band
  )
  expect_true(all(grepl("^p2", landed$subject_id)))
  expect_false(any(grepl("occurrence_id|_date$", names(landed))))
})
