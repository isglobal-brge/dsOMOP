.survival_sql_fixture <- function() {
  handle <- create_test_handle(n_persons = 15)
  DBI::dbExecute(handle$conn, paste0(
    "CREATE TEMP TABLE dsomop_survival_long (",
    "subject_id INTEGER, cohort_start_date TEXT, cohort_end_date TEXT)"
  ))
  register_test_temp(handle, "dsomop_survival_long")
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO dsomop_survival_long VALUES ",
    "(1, '2020-01-01', '2020-01-20'), ",
    "(1, '2020-01-05', '2020-01-15'), ",
    "(2, '2020-01-01', '2020-01-30'), ",
    "(3, '2020-01-01', '2020-01-30')"
  ))
  DBI::dbExecute(handle$conn, paste0(
    "UPDATE observation_period SET observation_period_start_date = ",
    "'2019-12-01', observation_period_end_date = ",
    "CASE person_id WHEN 1 THEN '2020-01-12' ELSE '2020-01-30' END ",
    "WHERE person_id IN (1, 2, 3)"
  ))
  DBI::dbExecute(handle$conn, paste0(
    "UPDATE death SET death_date = CASE person_id ",
    "WHEN 1 THEN '2020-01-25' WHEN 2 THEN '2020-01-10' ",
    "ELSE '2020-01-25' END WHERE person_id IN (1, 2, 3)"
  ))
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO condition_occurrence VALUES ",
    "(10001, 1, 4000002, '2020-01-08', '2020-01-08', 44818518, NULL), ",
    "(10002, 1, 317009,  '2020-01-08', '2020-01-08', 44818518, NULL), ",
    "(10003, 1, 4000002, '2020-01-13', '2020-01-13', 44818518, NULL), ",
    "(10004, 2, 4000002, '2020-01-03', '2020-01-03', 44818518, NULL), ",
    "(10005, 2, 4000002, '2020-01-05', '2020-01-05', 44818518, NULL), ",
    "(10006, 2, 4000002, '2020-01-09', '2020-01-09', 44818518, NULL), ",
    "(10007, 2, 4000002, '2020-01-11', '2020-01-11', 44818518, NULL)"
  ))
  handle
}

.survival_sql_outcomes <- function() {
  list(
    myocardial_infarction = list(
      table = "condition_occurrence", concept_set = 4000002L
    ),
    asthma = list(
      table = "condition_occurrence", concept_set = 317009L
    )
  )
}

.survival_set_scoped_person_key <- function(
    handle, key = .testPseudonymKey("survival-resource")) {
  identity <- "test://resource-scoped/survival"
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
  withr::local_options(list(
    dsomop.pseudonym_provider = NULL,
    dsomop.pseudonym_epoch = NULL,
    dsomop.pseudonym_require_existing = NULL,
    dsomop.pseudonym_root = NULL,
    dsomop.pseudonym_key = NULL
  ), .local_envir = parent.frame())
  handle$person_key_identity <- identity
  handle$person_key_id <- .personKeyId(key)
  handle$person_key_provider <- "scoped"
  handle$person_key_epoch <- 1L
  handle$person_key_require_existing <- FALSE
  handle$person_key_contract_version <- 1L
  invisible(key)
}

test_that("SQL-first competing risks respect episode-specific clinical censoring", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))

  compiled <- .compileLongitudinalSurvivalSql(
    handle = handle,
    cohort_table = "dsomop_survival_long",
    outcomes = .survival_sql_outcomes(),
    tar = list(start_offset = 0L, end_offset = 40L),
    format = "competing_risk",
    tie_policy = "priority"
  )
  result <- .executeLongitudinalSurvivalSql(handle, compiled)

  expect_s3_class(compiled, "dsomop_longitudinal_sql")
  expect_equal(nrow(result), 4L)
  expect_false(any(c(
    "subject_id", "event_key", "event_date",
    "cohort_start_date", "cohort_end_date", "censor_date"
  ) %in% names(result)))
  expect_true("person_id" %in% names(result))
  expect_equal(result$event, c(1L, 1L, 1L, 0L))
  expect_equal(result$outcome_name[1:3], rep("myocardial_infarction", 3L))
  # The same event is valid for both overlapping episodes, with offsets from
  # each episode's own index date.
  expect_equal(result$exit_days_from_index[1:2], c(7L, 3L))
  # Person 1 is censored at observation end (day 11), person 2 at death (day 9).
  expect_true(all(result$exit_days_from_index[1:2] <= c(11L, 7L)))
  expect_equal(result$exit_days_from_index[3], 2L)
  # Events on 13 January (after OP end), and 11 January (after death), vanished.
  expect_false(any(result$exit_days_from_index > c(11L, 7L, 9L, 24L)))
  expect_identical(compiled$semantics$date_output, "integer_offsets_only")
  expect_true(compiled$semantics$internal_person_id)
  expect_false(compiled$semantics$source_event_identifiers_output)
})

test_that("cause-specific survival keeps named outcomes and delayed entry", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))

  compiled <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", .survival_sql_outcomes(),
    tar = list(start_offset = 2L, end_offset = 20L),
    censoring = list(admin_date = "2020-01-18"),
    format = "survival"
  )
  result <- .executeLongitudinalSurvivalSql(handle, compiled)

  expect_equal(nrow(result), 8L)
  expect_setequal(unique(result$outcome_name),
                  c("myocardial_infarction", "asthma"))
  expect_true(all(result$entry_days_from_index == 2L))
  expect_true(all(result$exit_days_from_index >= result$entry_days_from_index))
  expect_true(all(result$follow_up_days >= 0L))
})

test_that("recurrent output has deterministic event numbers and a risk-set component", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))

  compiled <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", .survival_sql_outcomes(),
    format = "recurrent_events", event_order = "all",
    washout_days = 3L, tie_policy = "priority"
  )
  result <- .executeLongitudinalSurvivalSql(handle, compiled)

  expect_named(result, c("events", "risk_sets"))
  expect_equal(nrow(result$risk_sets), 4L)
  expect_false(any(c("subject_id", "event_key", "event_date") %in%
                   names(result$events)))
  expect_true("person_id" %in% names(result$events))
  by_episode <- split(result$events$event_number,
                      result$events$cohort_row_id)
  expect_true(all(vapply(by_episode, function(x) {
    identical(as.integer(x), seq_along(x))
  }, logical(1))))
  # For person 2 the Jan 5 event is inside the washout after Jan 3, while Jan 9
  # is retained; the Jan 11 event is after death and can never be emitted.
  person2_episode <- result$events[
    result$events$cohort_row_id == 3L, , drop = FALSE
  ]
  expect_equal(person2_episode$event_days_from_index, c(2L, 8L))
  expect_true(all(person2_episode$event_days_from_index <= 9L))
})

test_that("tie error and malformed censoring data fail closed before release", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))

  tied <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", .survival_sql_outcomes(),
    format = "competing_risk", tie_policy = "error"
  )
  expect_error(
    .executeLongitudinalSurvivalSql(handle, tied),
    "validation failed: event_ties"
  )

  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO observation_period VALUES ",
    "(999, 3, '2019-01-01', '2021-01-01', 44818518)"
  ))
  malformed <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", .survival_sql_outcomes(),
    format = "competing_risk"
  )
  expect_error(
    .executeLongitudinalSurvivalSql(handle, malformed),
    "validation failed: observation_period_coverage"
  )
})

test_that("survival requires observable TAR entry and washout lookback", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))

  DBI::dbExecute(handle$conn, paste0(
    "UPDATE observation_period SET observation_period_start_date = ",
    "'2019-12-30' WHERE person_id = 3"
  ))
  uncovered <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", .survival_sql_outcomes(),
    tar = list(start_offset = 0L, end_offset = 20L),
    washout_days = 3L, format = "recurrent_events"
  )
  expect_error(
    .executeLongitudinalSurvivalSql(handle, uncovered),
    "validation failed: observation_period_coverage"
  )

  DBI::dbExecute(handle$conn, paste0(
    "UPDATE observation_period SET observation_period_start_date = ",
    "'2019-12-20' WHERE person_id = 3"
  ))
  expect_silent(.executeLongitudinalSurvivalSql(handle, uncovered))
})

test_that("counting-process SQL emits ordered non-overlapping daily intervals", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))

  # An event on the TAR entry date occupies the discrete interval (-1, 0]
  # rather than producing an invalid zero-width (0, 0] row.
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO condition_occurrence VALUES ",
    "(10999, 3, 4000002, '2020-01-01', '2020-01-01', 44818518, NULL)"
  ))

  compiled <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", .survival_sql_outcomes(),
    format = "counting_process", event_order = "all",
    tie_policy = "priority"
  )
  result <- .executeLongitudinalSurvivalSql(handle, compiled)

  expect_identical(names(result), compiled$columns)
  expect_true(all(result$interval_start_days < result$interval_end_days))
  entry_event <- result[
    result$cohort_row_id == 4L & result$event == 1L, , drop = FALSE
  ]
  expect_equal(entry_event$interval_start_days[[1L]], -1L)
  expect_equal(entry_event$interval_end_days[[1L]], 0L)
  intervals <- split(result, result$cohort_row_id)
  expect_true(all(vapply(intervals, function(x) {
    if (nrow(x) < 2L) return(TRUE)
    all(x$interval_start_days[-1L] ==
          x$interval_end_days[-nrow(x)])
  }, logical(1))))
  expect_false(any(c("event_date", "censor_date") %in%
                   names(result)))
  expect_true("person_id" %in% names(result))
  expect_match(compiled$semantics$interval_convention,
               "start_offset - 1", fixed = TRUE)

  # The risk-set component must also work without the extra washout CTE.
  recurrent <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", .survival_sql_outcomes(),
    format = "recurrent_events", washout_days = 0L,
    tie_policy = "priority"
  )
  expect_equal(nrow(.executeLongitudinalSurvivalSql(handle, recurrent)$risk_sets),
               4L)
})

test_that("longitudinal survival compiler rejects ambiguous or unsupported shapes", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))

  expect_error(
    .compileLongitudinalSurvivalSql(
      handle, "dsomop_survival_long", .survival_sql_outcomes(),
      format = "survival", event_order = "all"
    ),
    "first or last"
  )
  expect_error(
    .compileLongitudinalSurvivalSql(
      handle, "dsomop_survival_long", .survival_sql_outcomes(),
      format = "competing_risk", event_order = "all"
    ),
    "requires event_order='first'"
  )
  expect_error(
    .compileLongitudinalSurvivalSql(
      handle, "dsomop_survival_long", .survival_sql_outcomes(),
      tar = list(start_offset = 10L, end_offset = 5L)
    ),
    "must not be before"
  )
  expect_error(
    .compileLongitudinalSurvivalSql(
      handle, "dsomop_survival_long", .survival_sql_outcomes(),
      censoring = list(cohort_end = FALSE)
    ),
    "must remain TRUE"
  )

  .buildBlueprint(handle)
  handle$dbms <- "mysql"
  handle$target_dialect <- "mysql"
  expect_error(
    .compileLongitudinalSurvivalSql(
      handle, "dsomop_survival_long", .survival_sql_outcomes()
    ),
    "verified MySQL >= 8.0"
  )

  handle$dbms_version <- "8.0.36"
  mysql <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", .survival_sql_outcomes()
  )
  expect_identical(mysql$dbms, "mysql")
  expect_match(mysql$sql, "DATEDIFF\\(")
  expect_match(mysql$sql, "DATE_ADD\\(")

  handle$dbms <- "mariadb"
  handle$dbms_version <- "5.5.5-10.11.6-MariaDB"
  mariadb <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", .survival_sql_outcomes()
  )
  expect_identical(mariadb$dbms, "mariadb")
})

test_that("plan compiler preserves the historical survival contract", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO dsomop_survival_long VALUES ",
    "(4, '2020-01-01', '2020-03-31')"
  ))
  DBI::dbExecute(handle$conn, paste0(
    "UPDATE observation_period SET observation_period_start_date = ",
    "'2020-01-01', observation_period_end_date = '2020-01-10' ",
    "WHERE person_id = 4"
  ))
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO observation_period VALUES ",
    "(1004, 4, '2020-02-01', '2020-12-31', 44818518)"
  ))
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO condition_occurrence VALUES ",
    "(10404, 4, 4000002, '2020-02-05', '2020-02-05', 44818518, NULL)"
  ))

  compiled <- .compilePlanSurvivalSql(
    handle,
    "dsomop_survival_long",
    list(
      type = "survival",
      outcome = list(
        table = "condition_occurrence", concept_set = 4000002L
      ),
      tar = list(start_offset = 0L, end_offset = 20L),
      event_order = "last"
    )
  )
  result <- withr::with_options(
    list(nfilter.subset = 0),
    .executeLongitudinalSurvivalSql(handle, compiled)
  )

  expect_true(compiled$legacy)
  expect_identical(
    compiled$columns,
    c("row_id", "cohort_row_id", "person_id", "event",
      "time_to_event_days")
  )
  expect_identical(names(result), compiled$columns)
  expect_true(compiled$semantics$censoring$observation_period_end)
  expect_false(compiled$semantics$censoring$death)
  expect_identical(compiled$semantics$event_order, "last")
  gap_episode <- result[result$cohort_row_id == 5L, , drop = FALSE]
  expect_equal(gap_episode$event, 0L)
  expect_equal(gap_episode$time_to_event_days, 9L)
})

test_that("advanced plan endpoints execute their own safe filters", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))

  compiled <- .compilePlanSurvivalSql(
    handle,
    "dsomop_survival_long",
    list(
      type = "survival",
      outcomes = list(mi = list(
        table = "condition_occurrence",
        concept_set = 4000002L,
        filters = list(
          var = "condition_start_date", op = "between",
          value = c("2020-01-04", "2020-12-31")
        )
      )),
      tar = list(start_offset = 0L, end_offset = 30L),
      format = "recurrent_events",
      event_order = "all",
      tie_policy = "priority"
    )
  )
  result <- .executeLongitudinalSurvivalSql(handle, compiled)$events

  person_two <- result[result$person_id == 2L, , drop = FALSE]
  expect_identical(person_two$event_days_from_index, c(4L, 8L))
  expect_true(all(person_two$outcome_name == "mi"))
})

test_that("recurrent plan assignment splits and pseudonymizes both components", {
  handle <- create_test_handle(n_persons = 15)
  .survival_set_scoped_person_key(handle)
  handle_symbol <- paste0("survival_recurrent_handle_", Sys.getpid())
  .setHandle(handle_symbol, handle)
  on.exit(.removeHandle(handle_symbol), add = TRUE)

  plan <- structure(list(
    cohort = list(type = "cohort_table", cohort_definition_id = 10L),
    outputs = list(recurrent = list(
      type = "survival",
      outcomes = list(mi = list(
        table = "condition_occurrence", concept_set = 4000002L
      )),
      tar = list(start_offset = 0L, end_offset = 730L),
      format = "recurrent_events",
      event_order = "all",
      washout_days = 0L,
      tie_policy = "priority"
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  ), class = c("omop_plan", "list"))

  memory_base <- paste0("survival_memory_", Sys.getpid())
  withr::with_options(list(nfilter.subset = 3L), {
    omopPlanExecuteDS(
      handle_symbol, plan, stats::setNames(memory_base, "recurrent")
    )
  })
  memory_events <- get(paste0(memory_base, ".events"),
                       envir = environment(), inherits = FALSE)
  memory_risk <- get(paste0(memory_base, ".riskSets"),
                     envir = environment(), inherits = FALSE)
  expect_s3_class(memory_events, "omop.table")
  expect_s3_class(memory_risk, "omop.table")
  expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$",
                        memory_risk$person_id)))
  expect_false(exists(memory_base, envir = environment(), inherits = FALSE))

  staged_base <- paste0("survival_staged_", Sys.getpid())
  withr::with_options(list(nfilter.subset = 3L), {
    omopPlanExecuteDS(
      handle_symbol, plan, stats::setNames(staged_base, "recurrent"),
      output_mode = "staged"
    )
  })
  staged_events <- get(paste0(staged_base, ".events"),
                       envir = environment(), inherits = FALSE)
  staged_risk <- get(paste0(staged_base, ".riskSets"),
                     envir = environment(), inherits = FALSE)
  expect_s3_class(staged_events, "FlowerDatasetDescriptor")
  expect_s3_class(staged_risk, "FlowerDatasetDescriptor")
  staged_data <- if (identical(staged_risk$metadata$format, "parquet")) {
    as.data.frame(arrow::read_parquet(staged_risk$metadata$file))
  } else {
    utils::read.csv(staged_risk$metadata$file, stringsAsFactors = FALSE)
  }
  expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$",
                        staged_data$person_id)))
  expect_identical(
    staged_risk$metadata$semantic_contract$component, "risk_sets"
  )
  on.exit(unlink(dirname(staged_risk$metadata$file), recursive = TRUE),
          add = TRUE)
})
