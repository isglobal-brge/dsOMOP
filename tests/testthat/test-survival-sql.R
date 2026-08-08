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

test_that("malformed event rows are excluded and duplicate keys are deterministic", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))

  # The fixture primary key reflects a conforming CDM. Recreate this one test
  # table without the constraint to exercise defensive handling of bad source
  # rows that can occur in non-conforming imports.
  DBI::dbExecute(handle$conn, paste0(
    "CREATE TABLE condition_occurrence_unconstrained AS ",
    "SELECT * FROM condition_occurrence"
  ))
  DBI::dbExecute(handle$conn, "DROP TABLE condition_occurrence")
  DBI::dbExecute(handle$conn, paste0(
    "ALTER TABLE condition_occurrence_unconstrained ",
    "RENAME TO condition_occurrence"
  ))
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO condition_occurrence VALUES ",
    "(13001, 3, 4000002, '2020-01-02', NULL, 44818518, NULL), ",
    "(13001, 3, 4000002, '2020-01-06', NULL, 44818518, NULL), ",
    "(NULL, 3, 4000002, '2020-01-07', NULL, 44818518, NULL), ",
    "(13002, 3, 4000002, NULL, NULL, 44818518, NULL)"
  ))

  compiled <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", .survival_sql_outcomes(),
    format = "recurrent_events", event_order = "all"
  )
  result <- .executeLongitudinalSurvivalSql(handle, compiled)$events
  person_three <- result[result$cohort_row_id == 4L, , drop = FALSE]

  expect_identical(compiled$validation_sql, list())
  expect_equal(person_three$event_days_from_index, 1L)
  expect_equal(person_three$outcome_event_number, 1L)
})

test_that("private eligibility failures expose only the final population gate", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))

  expect_error(
    .compileLongitudinalSurvivalSql(
      handle, "dsomop_survival_long", .survival_sql_outcomes(),
      format = "competing_risk", tie_policy = "error"
    ),
    "disclosure oracle"
  )

  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO observation_period VALUES ",
    "(999, 3, '2019-01-01', '2021-01-01', 44818518)"
  ))
  malformed <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", .survival_sql_outcomes(),
    format = "competing_risk"
  )
  expect_identical(malformed$validation_sql, list())
  expect_match(malformed$population_gate_sql,
               "FROM risk_episodes", fixed = TRUE)
  expect_equal(.executeQuery(handle, malformed$population_gate_sql)$n_persons,
               2L)
  expect_error(
    .executeLongitudinalSurvivalSql(handle, malformed),
    "insufficient individuals"
  )

  withr::local_options(list(nfilter.subset = 1L))
  eligible <- .executeLongitudinalSurvivalSql(handle, malformed)
  expect_false(4L %in% eligible$cohort_row_id)
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
    "insufficient individuals"
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

test_that("multi-state output follows reachable cyclic paths in bounded chunks", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))

  # The earlier MI on Jan 3 is not reachable from index and must not hide the
  # first reachable asthma state. The remaining records form a full cycle.
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO condition_occurrence VALUES ",
    "(11001, 2, 317009, '2020-01-04', '2020-01-04', 44818518, NULL), ",
    "(11002, 2, 317009, '2020-01-07', '2020-01-07', 44818518, NULL)"
  ))
  outcomes <- list(
    mi = list(table = "condition_occurrence", concept_set = 4000002L),
    asthma = list(table = "condition_occurrence", concept_set = 317009L)
  )
  compiled <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", outcomes,
    format = "multi_state", event_order = "all",
    transitions = list(
      index = "asthma", mi = "asthma", asthma = "mi"
    ),
    initial_state = "index", tie_policy = "priority"
  )

  one_row_chunks <- .executeLongitudinalSurvivalSql(
    handle, compiled, chunk_size = 1L
  )
  regular_chunks <- .executeLongitudinalSurvivalSql(
    handle, compiled, chunk_size = 1000L
  )
  reordered_compiled <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", outcomes[c("asthma", "mi")],
    format = "multi_state", event_order = "all",
    transitions = list(
      index = "asthma", mi = "asthma", asthma = "mi"
    ),
    initial_state = "index", tie_policy = "priority"
  )
  reordered <- .executeLongitudinalSurvivalSql(
    handle, reordered_compiled, chunk_size = 2L
  )
  expect_named(one_row_chunks, c("msdata", "transition_ref"))
  expect_equal(one_row_chunks$msdata, regular_chunks$msdata,
               ignore_attr = TRUE)
  expect_identical(one_row_chunks, reordered)
  expect_s3_class(one_row_chunks$msdata, "msdata")
  expect_identical(names(one_row_chunks$msdata), compiled$columns)
  expect_false(any(c("event_key", "event_date", "subject_id") %in%
                   names(one_row_chunks$msdata)))

  person_two <- one_row_chunks$msdata[
    one_row_chunks$msdata$cohort_row_id == 3, , drop = FALSE
  ]
  expect_equal(person_two$from_name,
               c("index", "asthma", "mi", "asthma", "mi"))
  expect_equal(person_two$to_name,
               c("asthma", "mi", "asthma", "mi", "asthma"))
  expect_equal(person_two$status, c(1L, 1L, 1L, 1L, 0L))
  expect_equal(person_two$Tstart, c(-1, 3, 4, 6, 8))
  expect_equal(person_two$Tstop, c(3, 4, 6, 8, 9))
  expect_true(all(person_two$Tstart < person_two$Tstop))
  expect_identical(compiled$semantics$grain, "episode_transition")
  expect_identical(
    compiled$semantics$multi_state$unreachable_event_policy,
    "skip_until_reachable"
  )
  expect_equal(one_row_chunks$transition_ref$trans, 1:3)
  expect_equal(attr(one_row_chunks$msdata, "trans")["index", "asthma"], 1L)
})

test_that("the initial state can be observed again in a reversible model", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO condition_occurrence VALUES ",
    "(11501, 2, 317009, '2020-01-04', '2020-01-04', 44818518, NULL), ",
    "(11502, 2, 317009, '2020-01-07', '2020-01-07', 44818518, NULL)"
  ))
  outcomes <- list(
    mi = list(table = "condition_occurrence", concept_set = 4000002L),
    well = list(table = "condition_occurrence", concept_set = 317009L)
  )
  compiled <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", outcomes,
    format = "multi_state", event_order = "all",
    transitions = list(well = "mi", mi = "well"),
    initial_state = "well", tie_policy = "priority"
  )
  result <- .executeLongitudinalSurvivalSql(handle, compiled)$msdata
  person_two <- result[result$cohort_row_id == 3, , drop = FALSE]

  expect_equal(person_two$from_name,
               c("well", "mi", "well", "mi", "well", "mi"))
  expect_equal(person_two$to_name,
               c("mi", "well", "mi", "well", "mi", "well"))
  expect_equal(person_two$status, c(1L, 1L, 1L, 1L, 1L, 0L))
  expect_identical(compiled$semantics$outcome_priority, c("well", "mi"))
})

test_that("multi-state sequential ties stay inside the observed calendar day", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO condition_occurrence VALUES ",
    "(12001, 3, 4000002, '2020-01-01', '2020-01-01', 44818518, NULL), ",
    "(12002, 3, 317009, '2020-01-01', '2020-01-01', 44818518, NULL)"
  ))
  compiled <- .compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", .survival_sql_outcomes(),
    format = "multi_state", event_order = "all",
    transitions = list(
      index = "myocardial_infarction",
      myocardial_infarction = "asthma",
      asthma = character(0)
    ),
    state_hierarchy = c("myocardial_infarction", "asthma", "index"),
    state_step = 0.01, tie_policy = "sequential"
  )
  result <- .executeLongitudinalSurvivalSql(handle, compiled)$msdata
  person_three <- result[result$cohort_row_id == 4, , drop = FALSE]

  expect_equal(person_three$status, c(1L, 1L))
  expect_equal(person_three$Tstop, c(-0.01, 0), tolerance = 1e-9)
  expect_true(all(person_three$Tstart < person_three$Tstop))
  expect_true(all(person_three$Tstop <= 0))
  expect_match(compiled$semantics$date_output, "public_within_day")
})

test_that("sequential decimals retain nine-place scale on SQL Server", {
  sqlserver <- list(dbms = "sqlserver", target_dialect = "sql server")
  expression <- paste0(
    .survivalDecimalCast(sqlserver, "state_day"), " - (",
    .survivalDecimalCast(sqlserver, "within_day_count - within_day_order"),
    " * ", .survivalDecimalCast(sqlserver, "0.0000001"), ")"
  )

  expect_equal(lengths(regmatches(
    expression, gregexpr("DECIMAL(20,9)", expression, fixed = TRUE)
  )), 3L)
  expect_false(grepl("DECIMAL(38,9)", expression, fixed = TRUE))
})

test_that("multi-state graph and tie contracts reject ambiguous shapes", {
  handle <- .survival_sql_fixture()
  on.exit(cleanup_handle(handle))
  outcomes <- .survival_sql_outcomes()

  expect_error(.compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", outcomes,
    format = "multi_state", event_order = "first",
    transitions = list(
      index = "myocardial_infarction",
      myocardial_infarction = "asthma", asthma = character(0)
    )
  ), "event_order='all'")
  expect_error(.compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", outcomes,
    format = "multi_state", event_order = "all",
    transitions = list(
      index = "myocardial_infarction",
      myocardial_infarction = character(0), asthma = character(0)
    )
  ), "graph-reachable")
  expect_error(.compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", outcomes,
    format = "multi_state", event_order = "all", tie_policy = "all",
    transitions = list(
      index = "myocardial_infarction",
      myocardial_infarction = "asthma", asthma = character(0)
    )
  ), "only for recurrent_events")

  duplicate_edge <- list(
    from = "index", to = "myocardial_infarction", 1L, 2L
  )
  names(duplicate_edge)[3:4] <- "trans"
  expect_error(.compileLongitudinalSurvivalSql(
    handle, "dsomop_survival_long", outcomes,
    format = "multi_state", event_order = "all",
    transitions = list(
      states = c("index", "myocardial_infarction", "asthma"),
      edges = list(duplicate_edge)
    )
  ), "must contain from, to and trans")

  expect_error(
    .compileLongitudinalSurvivalSql(
      handle, "dsomop_survival_long", outcomes,
      format = "multi_state", event_order = "all", tie_policy = "error",
      transitions = list(
        index = c("myocardial_infarction", "asthma"),
        myocardial_infarction = character(0), asthma = character(0)
      )
    ),
    "disclosure oracle"
  )
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

test_that("multi-state plan assignment streams both protected components", {
  handle <- create_test_handle(n_persons = 15)
  .survival_set_scoped_person_key(handle)
  handle_symbol <- paste0("survival_multistate_handle_", Sys.getpid())
  .setHandle(handle_symbol, handle)
  on.exit(.removeHandle(handle_symbol), add = TRUE)

  plan <- structure(list(
    cohort = list(type = "cohort_table", cohort_definition_id = 10L),
    outputs = list(course = list(
      type = "survival",
      outcomes = list(mi = list(
        table = "condition_occurrence", concept_set = 4000002L
      )),
      tar = list(start_offset = 0L, end_offset = 730L),
      format = "multi_state",
      event_order = "all",
      washout_days = 0L,
      tie_policy = "priority",
      transitions = list(index = "mi", mi = character(0)),
      initial_state = "index"
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  ), class = c("omop_plan", "list"))

  memory_base <- paste0("multistate_memory_", Sys.getpid())
  withr::with_options(list(nfilter.subset = 3L), {
    omopPlanExecuteDS(
      handle_symbol, plan, stats::setNames(memory_base, "course")
    )
  })
  memory_msdata <- get(paste0(memory_base, ".msdata"),
                       envir = environment(), inherits = FALSE)
  memory_ref <- get(paste0(memory_base, ".transitionRef"),
                    envir = environment(), inherits = FALSE)
  expect_s3_class(memory_msdata, "msdata")
  expect_s3_class(memory_msdata, "omop.table")
  expect_true(all(grepl(
    "^p2[0-9a-f]+\\.[0-9a-f]{64}$", memory_msdata$person_id
  )))
  expect_identical(memory_ref$from_name, "index")
  expect_false(exists(memory_base, envir = environment(), inherits = FALSE))

  staged_base <- paste0("multistate_staged_", Sys.getpid())
  withr::with_options(list(nfilter.subset = 3L), {
    omopPlanExecuteDS(
      handle_symbol, plan, stats::setNames(staged_base, "course"),
      output_mode = "staged"
    )
  })
  staged_msdata <- get(paste0(staged_base, ".msdata"),
                       envir = environment(), inherits = FALSE)
  staged_ref <- get(paste0(staged_base, ".transitionRef"),
                    envir = environment(), inherits = FALSE)
  expect_s3_class(staged_msdata, "FlowerDatasetDescriptor")
  expect_s3_class(staged_ref, "FlowerDatasetDescriptor")
  read_staged <- function(descriptor) {
    if (identical(descriptor$metadata$format, "parquet")) {
      as.data.frame(arrow::read_parquet(descriptor$metadata$file))
    } else {
      utils::read.csv(descriptor$metadata$file, stringsAsFactors = FALSE)
    }
  }
  staged_data <- read_staged(staged_msdata)
  staged_graph <- read_staged(staged_ref)
  expect_true(all(grepl(
    "^p2[0-9a-f]+\\.[0-9a-f]{64}$", staged_data$person_id
  )))
  expect_identical(staged_graph$from_name, "index")
  expect_identical(
    staged_msdata$metadata$semantic_contract$grain, "episode_transition"
  )
  expect_identical(
    staged_ref$metadata$semantic_contract$grain, "state_transition"
  )
  on.exit(unlink(dirname(staged_msdata$metadata$file), recursive = TRUE),
          add = TRUE)
})
