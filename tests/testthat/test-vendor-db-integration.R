.expectVendorDatabaseContract <- function(dbms) {
  config <- .dsomopVendorTestConfig(dbms)
  testthat::skip_if(is.null(config), paste0(
    "Set DSOMOP_TEST_", toupper(if (identical(dbms, "postgresql")) {
      "POSTGRES"
    } else {
      dbms
    }), "_HOST to run this vendor integration test."
  ))

  fixture <- .dsomopVendorCreateFixture(config)
  on.exit(fixture$cleanup(), add = TRUE)
  handle <- fixture$handle
  schemas <- fixture$schemas

  expect_equal(handle$dbms, dbms)
  expect_equal(handle$target_dialect,
               if (identical(dbms, "postgresql")) "postgresql" else "mysql")
  expect_identical(handle$cdm_schema, schemas[["cdm"]])
  expect_identical(handle$vocab_schema, schemas[["vocab"]])
  expect_identical(handle$results_schema, schemas[["results"]])
  expect_false(identical(fixture$runtime_user, config$admin_user))
  runtime_identity_sql <- if (identical(dbms, "postgresql")) {
    "SELECT CURRENT_USER AS runtime_user"
  } else {
    "SELECT CURRENT_USER() AS runtime_user"
  }
  runtime_identity <- as.character(DBI::dbGetQuery(
    .conn(handle), runtime_identity_sql
  )$runtime_user[[1L]])
  expect_true(startsWith(runtime_identity, fixture$runtime_user))

  expect_equal(as.integer(DBI::dbGetQuery(
    .conn(handle), paste0(
      "SELECT COUNT(*) AS n FROM ", schemas[["cdm"]], ".person"
    )
  )$n[[1L]]), 3L)
  expect_error(DBI::dbExecute(.conn(handle), paste0(
    "INSERT INTO ", schemas[["cdm"]], ".person ",
    "SELECT * FROM ", schemas[["cdm"]], ".person WHERE person_id = 1"
  )))
  expect_error(DBI::dbExecute(.conn(handle), paste0(
    "CREATE TABLE ", schemas[["cdm"]],
    ".dsomop_runtime_forbidden (value INTEGER)"
  )))
  runtime_temp <- .createTempTable(
    handle, paste0("dsomop_runtime_temp_", Sys.getpid()),
    "SELECT 1 AS value"
  )
  expect_equal(as.integer(.executeQuery(
    handle, paste0("SELECT value FROM ", runtime_temp)
  )$value[[1L]]), 1L)
  .dropTempTable(handle, runtime_temp)

  if (identical(dbms, "postgresql")) {
    expect_identical(
      toupper(as.character(DBI::dbGetQuery(
        .conn(handle), "SHOW TIME ZONE"
      )[[1L]][[1L]])),
      "UTC"
    )
    expect_identical(
      tolower(as.character(DBI::dbGetQuery(
        .conn(handle), "SHOW standard_conforming_strings"
      )[[1L]][[1L]])),
      "on"
    )
    DBI::dbExecute(fixture$admin, paste0(
      "CREATE MATERIALIZED VIEW ", schemas[["cdm"]], ".death AS ",
      "SELECT person_id, DATE '2020-06-01' AS death_date, ",
      "CAST(NULL AS TIMESTAMP) AS death_datetime, ",
      "CAST(0 AS INTEGER) AS death_type_concept_id, ",
      "CAST(NULL AS INTEGER) AS cause_concept_id, ",
      "CAST(NULL AS VARCHAR(50)) AS cause_source_value, ",
      "CAST(NULL AS INTEGER) AS cause_source_concept_id FROM ",
      schemas[["cdm"]], ".person WHERE person_id = -1"
    ))
    fixture$grant_select(paste0(schemas[["cdm"]], ".death"))
  } else {
    sql_modes <- strsplit(as.character(DBI::dbGetQuery(
      .conn(handle), "SELECT @@SESSION.sql_mode AS sql_mode"
    )$sql_mode[[1L]]), ",", fixed = TRUE)[[1L]]
    expect_true("ANSI_QUOTES" %in% sql_modes)
    expect_false("NO_BACKSLASH_ESCAPES" %in% sql_modes)
    physical_guard <- "DsOmopCaseGuard"
    canonical_guard <- tolower(physical_guard)
    DBI::dbExecute(fixture$admin, paste0(
      "CREATE TABLE ", schemas[["cdm"]], ".`", physical_guard,
      "` (value INTEGER)"
    ))
    DBI::dbExecute(fixture$admin, paste0(
      "INSERT INTO ", schemas[["cdm"]], ".`", physical_guard,
      "` VALUES (7)"
    ))
    fixture$grant_select(paste0(
      schemas[["cdm"]], ".`", physical_guard, "`"
    ))
    expect_true(canonical_guard %in%
                  .listTablesRaw(handle, schemas[["cdm"]]))
    qualified_guard <- .qualifyTable(
      handle, canonical_guard, schemas[["cdm"]]
    )
    expect_equal(
      as.integer(.executeQuery(
        handle, paste0("SELECT value FROM ", qualified_guard)
      )$value),
      7L
    )
    lower_case_mode <- as.integer(DBI::dbGetQuery(
      .conn(handle), "SELECT @@lower_case_table_names AS mode"
    )$mode[[1L]])
    if (identical(lower_case_mode, 0L)) {
      expect_identical(
        qualified_guard,
        paste0(schemas[["cdm"]], ".`", physical_guard, "`")
      )
    }
  }

  expect_true(all(c("person", "condition_occurrence", "concept", "cohort") %in%
                    .listTablesRaw(handle, schemas[["cdm"]])))
  expect_true("concept" %in% .listTablesRaw(handle, schemas[["vocab"]]))
  expect_true("cohort" %in% .listTablesRaw(handle, schemas[["results"]]))
  if (identical(dbms, "postgresql")) {
    expect_true("death" %in% .listTablesRaw(handle, schemas[["cdm"]]))
    death_columns <- .listColumnsRaw(handle, "death", schemas[["cdm"]])
    expect_identical(
      death_columns$column_name,
      c("person_id", "death_date", "death_datetime",
        "death_type_concept_id", "cause_concept_id", "cause_source_value",
        "cause_source_concept_id")
    )
  }

  blueprint <- .buildBlueprint(handle)
  table_row <- function(table) {
    blueprint$tables[blueprint$tables$table_name == table, , drop = FALSE]
  }
  expect_identical(table_row("person")$qualified_name,
                   paste0(schemas[["cdm"]], ".person"))
  expect_identical(table_row("concept")$qualified_name,
                   paste0(schemas[["vocab"]], ".concept"))
  expect_identical(table_row("cohort")$qualified_name,
                   paste0(schemas[["results"]], ".cohort"))
  expect_true(all(vapply(c("person", "concept", "cohort"), function(table) {
    isTRUE(table_row(table)$present_in_db)
  }, logical(1))))
  if (identical(dbms, "postgresql")) {
    expect_true(isTRUE(table_row("death")$present_in_db))
    expect_true(all(c("person_id", "death_date", "death_datetime") %in%
                      blueprint$columns$death$column_name))
  }

  concepts <- .vocabSearchConcepts(
    handle, "diabetes", domain = "Condition", standard_only = TRUE
  )
  expect_identical(as.integer(concepts$concept_id), 201820L)
  expect_identical(concepts$concept_name, "Type 2 diabetes mellitus")

  filtered_sql <- .compileSelect(
    handle, "condition_occurrence",
    columns = c("condition_occurrence_id", "condition_concept_id",
                "condition_start_date"),
    concept_filter = 201820L,
    filters = list(
      var = "condition_start_date", op = "between",
      value = c("2020-01-01", "2020-12-31")
    )
  )
  filtered <- .executeQuery(handle, filtered_sql)
  expect_equal(nrow(filtered), 3L)
  expect_equal(sort(as.integer(filtered$condition_occurrence_id)), 1:3)

  date_sql <- .sql_translate(paste0(
    "SELECT TOP 2 person_id, ",
    "DATEADD(day, 7, condition_start_date) AS shifted ",
    "FROM ", schemas[["cdm"]],
    ".condition_occurrence ORDER BY condition_occurrence_id"
  ), handle$target_dialect)
  shifted <- .executeQuery(handle, date_sql)
  expect_equal(as.integer(shifted$person_id), c(1L, 1L))
  expect_equal(as.character(shifted$shifted), c("2020-01-08", "2020-02-08"))

  sample_sd <- .executeQuery(handle, .sql_translate(
    paste0(
      "SELECT STDDEV(CAST(value AS FLOAT)) AS sample_sd FROM ",
      "(SELECT 1 AS value UNION ALL SELECT 2 UNION ALL SELECT 3) values_"
    ),
    handle$target_dialect
  ))
  expect_equal(as.numeric(sample_sd$sample_sd), 1, tolerance = 1e-10)

  withr::local_options(list(
    nfilter.subset = 1,
    nfilter.tab = 1,
    dsomop.nfilter.band = 1
  ))

  longitudinal_cohort <- paste0(
    schemas[["results"]], ".longitudinal_cohort"
  )
  intervals <- .extractIntervalsLongSql(
    handle, longitudinal_cohort, "condition_occurrence",
    concept_filter = list(condition_occurrence = 201820L)
  )
  expect_equal(nrow(intervals), 4L)
  expect_equal(as.integer(intervals$cohort_row_id), c(1L, 2L, 4L, 6L))
  expect_equal(as.integer(intervals$subject_id), c(1L, 1L, 2L, 3L))
  expect_equal(as.integer(intervals$start_days_from_index), rep(0L, 4L))
  expect_equal(as.integer(intervals$end_days_from_index), rep(2L, 4L))
  expect_false(any(grepl("occurrence_id|_date$", names(intervals))))

  survival_contract <- .compileLongitudinalSurvivalSql(
    handle = handle,
    cohort_table = longitudinal_cohort,
    outcomes = list(diabetes = list(
      table = "condition_occurrence", concept_set = 201820L
    )),
    censoring = list(observation_period_end = TRUE, death = FALSE),
    format = "survival"
  )
  survival <- .executeLongitudinalSurvivalSql(handle, survival_contract)
  expect_s3_class(survival_contract, "dsomop_longitudinal_sql")
  expect_equal(nrow(survival), 6L)
  expect_equal(as.integer(survival$cohort_row_id), 1:6)
  expect_equal(sum(as.integer(survival$event)), 4L)
  expect_true(all(as.integer(
    survival$exit_days_from_index[survival$event == 1L]
  ) == 0L))
  expect_identical(survival_contract$semantics$grain, "episode_outcome")

  full_cohort <- paste0(schemas[["results"]], ".full_cohort")
  temporal <- .compileTemporalSqlComponents(
    handle = handle,
    cohort_table = full_cohort,
    table = "condition_occurrence",
    concept_filter = 201820L,
    bin_width = 60L,
    window_start = 0L,
    window_end = 60L,
    analyses = c("binary", "count")
  )
  for (validation in temporal$validations) {
    value <- .executeQuery(handle, validation$sql)
    if (identical(validation$kind, "min_persons")) {
      expect_silent(.assertMinPersons(n_persons = value$n_persons[[1L]]))
    } else {
      expect_lte(as.numeric(value[[1L]][[1L]]), validation$max)
    }
  }
  temporal_values <- .normalizeTemporalSqlChunk(
    .executeQuery(handle, temporal$components$temporalCovariates$sql),
    temporal$components$temporalCovariates
  )
  expect_equal(nrow(temporal_values), 4L)
  expect_equal(
    as.numeric(temporal_values$covariateValue[
      temporal_values$rowId == 1L &
        temporal_values$timeId == 1L &
        temporal_values$covariateId == 201820002
    ]),
    2
  )

  person_period <- .compileTemporalSqlComponents(
    handle = handle,
    cohort_table = full_cohort,
    table = "condition_occurrence",
    concept_filter = 201820L,
    bin_width = 60L,
    window_start = 0L,
    window_end = 60L,
    analyses = "binary",
    output_type = "person_period",
    grain = "episode",
    time_origin = "index"
  )
  periods <- .normalizeTemporalSqlChunk(
    .executeQuery(handle, person_period$components$personPeriods$sql),
    person_period$components$personPeriods
  )
  expect_equal(nrow(periods), 6L)
  expect_identical(
    names(periods),
    c(
      "rowId", "timeId", "startDay", "endDay",
      "observationStartDay", "observationEndDay", "daysObserved"
    )
  )
  expect_true(all(as.integer(periods$daysObserved) > 0L))

  advanced_survival <- lapply(
    c("competing_risk", "recurrent_events", "counting_process"),
    function(format) {
      compiled <- .compileLongitudinalSurvivalSql(
        handle = handle,
        cohort_table = full_cohort,
        outcomes = list(diabetes = list(
          table = "condition_occurrence", concept_set = 201820L
        )),
        censoring = list(observation_period_end = TRUE, death = FALSE),
        format = format,
        event_order = if (format %in%
                          c("recurrent_events", "counting_process")) {
          "all"
        } else {
          "first"
        }
      )
      .executeLongitudinalSurvivalSql(handle, compiled)
    }
  )
  names(advanced_survival) <- c(
    "competing_risk", "recurrent_events", "counting_process"
  )
  expect_equal(nrow(advanced_survival$competing_risk), 3L)
  expect_equal(sum(advanced_survival$competing_risk$event), 2L)
  expect_equal(nrow(advanced_survival$recurrent_events$risk_sets), 3L)
  expect_equal(nrow(advanced_survival$recurrent_events$events), 3L)
  expect_equal(
    advanced_survival$recurrent_events$events$event_number[
      advanced_survival$recurrent_events$events$cohort_row_id == 1L
    ],
    c(1L, 2L)
  )
  counting <- advanced_survival$counting_process
  expect_equal(sum(as.integer(counting$event)), 3L)
  expect_true(all(
    as.integer(counting$interval_start_days) <
      as.integer(counting$interval_end_days)
  ))

  multistate_contract <- .compileLongitudinalSurvivalSql(
    handle = handle,
    cohort_table = full_cohort,
    outcomes = list(diabetes = list(
      table = "condition_occurrence", concept_set = 201820L
    )),
    censoring = list(observation_period_end = TRUE, death = FALSE),
    format = "multi_state",
    event_order = "all",
    transitions = list(index = "diabetes", diabetes = character(0)),
    initial_state = "index"
  )
  multistate <- .executeLongitudinalSurvivalSql(
    handle, multistate_contract, chunk_size = 1L
  )
  expect_s3_class(multistate$msdata, "msdata")
  expect_equal(nrow(multistate$msdata), 3L)
  expect_equal(sum(as.integer(multistate$msdata$status)), 2L)
  expect_identical(multistate$transition_ref$from_name, "index")
  expect_identical(multistate$transition_ref$to_name, "diabetes")
  expect_match(
    multistate_contract$sql,
    paste0(schemas[["cdm"]], ".condition_occurrence"),
    fixed = TRUE
  )

  staging_base <- tempfile(paste0("dsomop_vendor_stage_", dbms, "_"))
  withr::local_options(list(
    dsstaging.base_dir = staging_base,
    dsomop.max_staged_rows = 1000L
  ))
  on.exit(unlink(staging_base, recursive = TRUE), add = TRUE)
  staging_dir <- .createStagingDir(.generateStagingToken())
  interval_sql <- .compileIntervalsLongSql(
    handle, full_cohort, "condition_occurrence",
    concept_filter = list(condition_occurrence = 201820L)
  )
  streamed <- .executeQueryToParquet(
    .conn(handle), interval_sql,
    file.path(staging_dir, "vendor_intervals.parquet"),
    chunk_size = 1L
  )
  expect_equal(streamed$n_rows, 3L)
  expect_true(file.exists(streamed$file))
  expect_identical(
    streamed$columns,
    c(
      "row_id", "cohort_row_id", "subject_id", "interval_type",
      "concept_id", "start_days_from_index", "end_days_from_index"
    )
  )

  multistate_machine <- .newMultistateStreamTransformer(
    multistate_contract, max_rows = Inf
  )
  streamed_multistate <- .executeQueryToParquet(
    .conn(handle), multistate_contract$sql,
    file.path(staging_dir, "vendor_multistate.parquet"),
    chunk_size = 1L,
    chunk_fn = multistate_machine$transform
  )
  expect_silent(multistate_machine$assert_complete())
  expect_equal(streamed_multistate$n_rows, 3L)
  expect_identical(streamed_multistate$columns,
                   multistate_contract$columns)

  cohorts <- .cohortList(handle)
  expect_setequal(as.integer(cohorts$cohort_definition_id), c(7L, 8L))
  expect_setequal(cohorts$cohort_definition_name,
                  c("All fixture persons", "Recurrent fixture episodes"))
  expect_identical(as.numeric(cohorts$size), c(3, 3))

  expect_warning(
    prevalence <- .omopAnalysisRun(
      handle, "dsomop:condition.prevalence_by_concept",
      params = list(top_n = 10L)
    ),
    "some entries depend on resources not present"
  )
  expect_identical(as.integer(prevalence$concept_id), 201820L)
  expect_identical(prevalence$concept_name, "Type 2 diabetes mellitus")
  expect_identical(as.numeric(prevalence$n_persons), 3)
  expect_identical(as.numeric(prevalence$n_records), 4)

  temporary <- .createTempTable(
    handle, paste0("dsomop_vendor_tmp_", Sys.getpid()),
    paste0("SELECT subject_id FROM ", schemas[["results"]],
           ".cohort WHERE cohort_definition_id = 7")
  )
  expect_equal(.executeQuery(
    handle, paste0("SELECT COUNT(*) AS n FROM ", temporary)
  )$n, 3)
  .dropTempTable(handle, temporary)
  expect_false(temporary %in% handle$temp_tables)

  if (identical(dbms, "postgresql")) {
    guard <- paste0("dsomop_vendor_drop_guard_", Sys.getpid())
    DBI::dbExecute(fixture$admin, paste0(
      "CREATE TABLE ", schemas[["cdm"]], ".", guard,
      " (value INTEGER)"
    ))
    DBI::dbExecute(fixture$admin, paste0(
      "INSERT INTO ", schemas[["cdm"]], ".", guard, " VALUES (7)"
    ))
    fixture$grant_select(paste0(schemas[["cdm"]], ".", guard))
    DBI::dbExecute(.conn(handle), paste0(
      "SET search_path TO ", schemas[["cdm"]], ", public"
    ))
    .createTempTable(handle, guard, "SELECT 1 AS value")
    DBI::dbExecute(.conn(handle), paste0("DROP TABLE pg_temp.", guard))
    .dropTempTable(handle, guard)
    expect_equal(.executeQuery(handle, paste0(
      "SELECT value FROM ", schemas[["cdm"]], ".", guard
    ))$value, 7L)
    expect_false(guard %in% handle$temp_tables)
  }
}

test_that("PostgreSQL executes the separated OMOP namespace contract", {
  .expectVendorDatabaseContract("postgresql")
})

test_that("MySQL executes the separated OMOP database contract", {
  .expectVendorDatabaseContract("mysql")
})

test_that("MariaDB executes the separated OMOP database contract", {
  .expectVendorDatabaseContract("mariadb")
})
