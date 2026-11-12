# Automatic protection of the community tools (dsomop.tools.mode, 2.9.0).

.tools_dp_environment <- c(
  "DSOMOP_DP_ENABLED", "DSOMOP_DP_DOMAIN", "DSOMOP_DP_SNAPSHOT_ID",
  "DSOMOP_DP_RELEASE_EPSILON", "DSOMOP_DP_PRIVACY_EPOCH",
  "DSOMOP_DP_MAX_LEVELS", "DSOMOP_DP_MAX_CONTRIBUTIONS",
  "DSOMOP_DP_NUMERIC_GRID", "DSOMOP_DP_NOISE_ROOT"
)

# A private DP service for one test (same contract as test-privacy-dp.R).
.tools_dp_state <- function(enabled = TRUE, .local_envir = parent.frame()) {
  state <- withr::local_tempdir(pattern = "dsomop-tools-",
                                .local_envir = .local_envir)
  Sys.chmod(state, mode = "0700")
  state <- normalizePath(state, winslash = "/", mustWork = TRUE)
  environment <- stats::setNames(
    rep.int(NA_character_, length(.tools_dp_environment)),
    .tools_dp_environment
  )
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state, DSOMOP_MARKER_DIR = NA_character_,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1", environment
  ), .local_envir = .local_envir)
  withr::local_options(list(
    dsomop.state_dir = state, dsomop.dp.enabled = enabled,
    dsomop.dp.domain = "dsomop-tools-test",
    dsomop.dp.snapshot_id = "tools-2026-11-01",
    dsomop.dp.release_epsilon = 0.1, dsomop.dp.privacy_epoch = 1L,
    dsomop.dp.noise_root = NULL
  ), .local_envir = .local_envir)
  previous <- list(status = .pkg_state$dp_status,
                   runtime = .pkg_state$dp_runtime,
                   in_progress = .pkg_state$dp_bootstrap_in_progress)
  withr::defer({
    .pkg_state$dp_status <- previous$status
    .pkg_state$dp_runtime <- previous$runtime
    .pkg_state$dp_bootstrap_in_progress <- previous$in_progress
  }, envir = .local_envir)
  .pkg_state$dp_status <- NULL
  .pkg_state$dp_runtime <- NULL
  .pkg_state$dp_bootstrap_in_progress <- FALSE
  state
}

# The test CDM plus the era, cost and days-supply data the reviewed questions
# read.
tools_handle <- function(n_persons = 60) {
  h <- create_test_handle(n_persons = n_persons)
  conn <- h$conn
  DBI::dbExecute(conn, "ALTER TABLE drug_exposure ADD COLUMN days_supply INTEGER")
  DBI::dbExecute(conn, paste(
    "UPDATE drug_exposure SET days_supply = 30 * (drug_exposure_id % 4 + 1)"))
  DBI::dbExecute(conn, paste(
    "CREATE TABLE condition_era (condition_era_id INTEGER, person_id INTEGER,",
    "condition_concept_id INTEGER, condition_era_start_date TEXT,",
    "condition_era_end_date TEXT, condition_occurrence_count INTEGER)"))
  persons <- rep(seq_len(n_persons), times = rep_len(c(1L, 2L, 3L), n_persons))
  DBI::dbWriteTable(conn, "condition_era", data.frame(
    condition_era_id = seq_along(persons), person_id = persons,
    condition_concept_id = 201820L, condition_era_start_date = "2020-01-01",
    condition_era_end_date = as.character(
      as.Date("2020-01-01") + (seq_along(persons) * 37L) %% 900L),
    condition_occurrence_count = 1L), append = TRUE)
  DBI::dbExecute(conn, paste(
    "CREATE TABLE drug_era (drug_era_id INTEGER, person_id INTEGER,",
    "drug_concept_id INTEGER, drug_era_start_date TEXT,",
    "drug_era_end_date TEXT, drug_exposure_count INTEGER, gap_days INTEGER)"))
  DBI::dbWriteTable(conn, "drug_era", data.frame(
    drug_era_id = seq_len(n_persons), person_id = seq_len(n_persons),
    drug_concept_id = rep(c(1124300L, 1154029L), length.out = n_persons),
    drug_era_start_date = "2021-01-01",
    drug_era_end_date = as.character(
      as.Date("2021-01-01") + 10L + seq_len(n_persons) * 5L),
    drug_exposure_count = rep_len(1:12, n_persons), gap_days = 0L),
    append = TRUE)
  DBI::dbExecute(conn, paste(
    "CREATE TABLE cost (cost_id INTEGER, cost_event_id INTEGER,",
    "cost_domain_id TEXT, currency_concept_id INTEGER, total_paid REAL,",
    "paid_by_patient REAL)"))
  DBI::dbWriteTable(conn, "cost", data.frame(
    cost_id = 1:6, cost_event_id = 1:6, cost_domain_id = "Drug",
    currency_concept_id = 44818668L, total_paid = c(10, 20, 30, 40, 5, 6),
    paid_by_patient = c(1, 2, 3, 4, 1, 1)), append = TRUE)
  h
}

# The closed column schema of an entry: protected releases publish it in
# release_pooling (their legacy contract is not_poolable for older clients).
.tools_columns <- function(entry) {
  entry$meta$release_pooling$columns %||% entry$meta$pooling_contract$columns
}

# The test CDM lacks some optional tables; the registry says so once per build.
tools_run <- function(h, id, params = list()) {
  suppressWarnings(.omopAnalysisRun(h, paste0("dsomop:", id), params))
}

.tools_histogram_ids <- c(
  "condition.duration_stats", "drug.duration_stats", "drug.days_supply_stats",
  "drug_era.exposure_count_stats", "condition_era.eras_per_person_stats",
  "visit.duration_stats", "observation_period.length_by_gender"
)
.tools_partition_ids <- c(
  "person.year_of_birth_distribution", "person.birth_month_distribution",
  "person.gender_by_age_decade"
)
.tools_summary_ids <- c(
  "drug.quantity_stats", "measurement.value_stats",
  "drug_cost.cost_per_unit_stats", "drug_cost.out_of_pocket_stats",
  "condition_era.length_stats", "drug_era.length_stats",
  "observation_period.length_stats", "observation_period.length_months_stats"
)
.tools_params <- list(
  "condition.duration_stats" = list(concept_id = 201820),
  "drug.duration_stats" = list(concept_id = 1124300),
  "drug.days_supply_stats" = list(concept_id = 1124300),
  "drug_era.exposure_count_stats" = list(concept_id = 1124300),
  "condition_era.eras_per_person_stats" = list(concept_id = 201820),
  "visit.duration_stats" = list(visit_concept_id = 0),
  "drug.quantity_stats" = list(concept_id = 1124300),
  "measurement.value_stats" = list(concept_id = 3025315),
  "drug_cost.cost_per_unit_stats" = list(concept_id = 1124300),
  "drug_cost.out_of_pocket_stats" = list(concept_id = 1124300),
  "drug_era.length_stats" = list(top_n = 5)
)

test_that("the tools mode defaults to auto and follows the custodian", {
  withr::local_options(list(dsomop.tools.mode = NULL,
                            default.dsomop.tools.mode = NULL,
                            dsomop.dp.exclusive = NULL))
  expect_identical(.dsomopToolsMode(), "auto")
  withr::with_options(list(dsomop.tools.mode = "standard"),
                      expect_identical(.dsomopToolsMode(), "standard"))
  withr::with_options(list(dsomop.tools.mode = " DP "),
                      expect_identical(.dsomopToolsMode(), "dp"))
  withr::with_options(list(default.dsomop.tools.mode = "standard"),
                      expect_identical(.dsomopToolsMode(), "standard"))
  withr::with_options(list(dsomop.tools.mode = "loose"),
                      expect_error(.dsomopToolsMode(), "auto, standard or dp"))
  # The legacy strict override selects DP-only mode only when DP is enabled.
  .tools_dp_state(enabled = FALSE)
  withr::with_options(list(dsomop.dp.exclusive = TRUE),
                      expect_identical(.dsomopToolsMode(), "auto"))
  .tools_dp_state()
  withr::with_options(list(dsomop.dp.exclusive = TRUE),
                      expect_identical(.dsomopToolsMode(), "dp"))
})

test_that("every reviewed question has a closed schema and pooling contract", {
  h <- tools_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  reviewed <- .dsomopToolsReviewedQueries()
  expect_setequal(names(reviewed), c(.tools_histogram_ids,
                                     .tools_partition_ids, .tools_summary_ids))
  for (mode in c("auto", "standard", "dp")) {
    withr::with_options(list(dsomop.tools.mode = mode), {
      registry <- suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
      expect_null(registry[["dsomop:person.load"]])
      for (id in names(reviewed)) {
        entry <- registry[[paste0("dsomop:", id)]]
        expect_identical(entry$meta$adapter, "tools_auto", info = id)
        expect_identical(names(.tools_columns(entry)),
                         entry$meta$output_fields, info = paste(mode, id))
        shape <- .dsomopToolsShape(reviewed[[id]], mode)
        expect_identical(entry$meta$tools_shape, shape, info = paste(mode, id))
      }
    })
  }
})

test_that("auto mode without DP releases the same tables under the standard gate", {
  h <- tools_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  withr::local_options(list(dsomop.tools.mode = "auto",
                            dsomop.dp.enabled = FALSE,
                            nfilter.tab = 3, dsomop.nfilter.band = 5))
  for (id in c(.tools_histogram_ids, .tools_partition_ids,
               .tools_summary_ids)) {
    out <- tools_run(h, id, .tools_params[[id]] %||% list())
    label <- attr(out, "dsomop_protection")
    # Every reviewed question has a protected route; without the DP service
    # auto mode releases its standard table under the gate.
    expect_identical(label$protection, "standard_fallback", info = id)
    expect_identical(names(out),
                     .omopAnalysisResolve(h, paste0("dsomop:", id))$meta$output_fields,
                     info = id)
    released <- out$n_persons[!is.na(out$n_persons)]
    expect_true(all(released >= 3 | released == 0), info = id)
    expect_true(all(released %% 5 == 0), info = id)
  }
})

test_that("auto mode with DP releases complete protected histograms", {
  .tools_dp_state()
  h <- tools_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  withr::local_options(list(dsomop.tools.mode = "auto"))
  out <- tools_run(h, "condition.duration_stats", list(concept_id = 201820))
  label <- attr(out, "dsomop_protection")
  # The test service declares a custodial snapshot: an unscoped question over
  # a public basis is a fixed synopsis.
  expect_identical(label$protection, "fixed_synopsis")
  expect_identical(label$guarantee_scope,
                   "fixed_vector_before_exact_support_gate")
  expect_equal(label$noise_reference_epsilon, 0.1)
  expect_equal(label$mechanism_epsilon, 0.1)
  expect_true(is.na(label$api_transcript_epsilon))
  expect_identical(label$adjacency, "add_remove_person")
  expect_identical(label$privacy_call_quota, "none")
  expect_false(label$history_based_admission)
  component <- label$components[[1L]]
  expect_equal(component$allocation_epsilon$H, 0.08)
  expect_equal(component$allocation_epsilon$N, 0.02)
  expect_equal(component$noise_sd$H, .dsomopReleaseNoiseSd(0.08, 1),
               tolerance = 1e-12)
  expect_equal(nrow(out), length(.DSOMOP_TOOLS_DAY_BREAKS))
  expect_identical(out$bin, seq_along(.DSOMOP_TOOLS_DAY_BREAKS))
  expect_true(all(out$n_persons >= 0))
  expect_true(is.na(out$bin_upper[nrow(out)]))
  # Displayed cells are the simplex projection onto the one noisy total.
  release <- attr(out, "dsomop_release")
  noisy_n <- release$owners[[1L]]$components[[1L]]$n
  expect_equal(sum(out$n_persons), max(0, noisy_n), tolerance = 1e-8)
  again <- tools_run(h, "condition.duration_stats", list(concept_id = 201820))
  expect_identical(again$n_persons, out$n_persons)

  for (id in c(.tools_histogram_ids, .tools_partition_ids)) {
    res <- tools_run(h, id, .tools_params[[id]] %||% list())
    expect_identical(attr(res, "dsomop_protection")$protection,
                     "fixed_synopsis", info = id)
  }
  year <- tools_run(h, "person.year_of_birth_distribution")
  expect_equal(nrow(year), .dsomopToolsReferenceYear() - 1900L + 2L)
  expect_true(anyNA(year$year_of_birth))
  month <- tools_run(h, "person.birth_month_distribution")
  expect_equal(nrow(month), 13L)
  age <- tools_run(h, "person.gender_by_age_decade")
  expect_equal(nrow(age), nrow(.dsomopToolsGenders()) *
                 length(.DSOMOP_TOOLS_AGE_GROUPS))
  by_gender <- tools_run(h, "observation_period.length_by_gender")
  expect_equal(nrow(by_gender), nrow(.dsomopToolsGenders()) *
                 length(.DSOMOP_TOOLS_DAY_BREAKS))
  # Record-weighted summaries are capped record moments on public scales
  # (tools_release_records.R), not standard statistics.
  for (id in .tools_summary_ids) {
    res <- tools_run(h, id, .tools_params[[id]] %||% list())
    expect_identical(attr(res, "dsomop_protection")$protection,
                     "fixed_synopsis", info = id)
  }
})

test_that("each person changes exactly one cell of a DP histogram by one", {
  h <- tools_handle(40)
  on.exit(cleanup_handle(h), add = TRUE)
  reviewed <- .dsomopToolsReviewedQueries()
  cells <- function(id, params) {
    spec <- reviewed[[id]]
    if (identical(spec$kind, "partition")) {
      return(.dsomopToolsPartitionCells(h, spec)$counts)
    }
    .dsomopToolsHistogramCells(h, spec,
                               .dsomopToolsConceptParam(spec, params))$counts
  }
  tables <- c("person", "observation_period", "condition_occurrence",
              "drug_exposure", "visit_occurrence", "condition_era", "drug_era")
  for (id in c(.tools_histogram_ids, .tools_partition_ids)) {
    params <- .tools_params[[id]] %||% list()
    full <- cells(id, params)
    for (person in c(1L, 2L, 5L, 9L)) {
      backup <- lapply(tables, function(t) DBI::dbGetQuery(
        h$conn, paste0("SELECT * FROM ", t, " WHERE person_id = ", person)))
      for (t in tables) {
        DBI::dbExecute(h$conn, paste0("DELETE FROM ", t,
                                      " WHERE person_id = ", person))
      }
      without <- cells(id, params)
      for (i in seq_along(tables)) {
        DBI::dbWriteTable(h$conn, tables[[i]], backup[[i]], append = TRUE)
      }
      expect_lte(sum(abs(full - without)), 1, label = paste(id, person))
    }
  }
})

test_that("standard mode answers the original summaries under the gate", {
  h <- tools_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  withr::local_options(list(dsomop.tools.mode = "standard",
                            dsomop.nfilter.dist = 10, nfilter.tab = 3,
                            dsomop.nfilter.band = 5))
  out <- tools_run(h, "measurement.value_stats", list(concept_id = 3025315))
  expect_identical(names(out), c("measurement_concept_id", "concept_name",
                                 "unit_concept_id", "unit_name", "n_persons",
                                 "n_values", "mean_value", "sd_value"))
  values <- DBI::dbGetQuery(h$conn, paste(
    "SELECT value_as_number FROM measurement",
    "WHERE measurement_concept_id = 3025315"))$value_as_number
  expect_equal(out$mean_value, mean(values))
  expect_equal(out$sd_value, stats::sd(values))
  expect_identical(attr(out, "dsomop_protection")$protection, "standard")

  # Six persons have the condition: counts are released, the mean is masked
  # below nfilter.dist.
  duration <- tools_run(h, "condition.duration_stats", list(concept_id = 201820))
  expect_identical(names(duration), c("condition_concept_id", "concept_name",
                                      "n_persons", "n_records", "avg_days",
                                      "sd_days"))
  expect_true(is.na(duration$avg_days))
  expect_equal(duration$n_persons, 5)

  eras <- tools_run(h, "condition_era.eras_per_person_stats",
                    list(concept_id = 201820))
  expect_identical(names(eras), c("n_persons", "avg_eras", "stddev_eras"))
  per_person <- table(DBI::dbGetQuery(
    h$conn, "SELECT person_id FROM condition_era")$person_id)
  expect_equal(eras$avg_eras, mean(as.numeric(per_person)))

  top <- tools_run(h, "drug_era.length_stats", list(top_n = 1))
  expect_equal(nrow(top), 1L)
})

test_that("DP-only mode refuses standard statistics and keeps DP questions", {
  .tools_dp_state()
  h <- tools_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  withr::local_options(list(dsomop.tools.mode = "dp"))
  out <- tools_run(h, "condition.duration_stats", list(concept_id = 201820))
  expect_identical(attr(out, "dsomop_protection")$protection,
                   "fixed_synopsis")
  value <- tools_run(h, "measurement.value_stats", list(concept_id = 3025315))
  expect_identical(attr(value, "dsomop_protection")$protection,
                   "fixed_synopsis")
  expect_error(tools_run(h, "demo.person_count_by_gender"),
               "DP-only mode")
  expect_error(.dsomopRequireCommunityTool(), "DP-only mode")
  expect_invisible(.dsomopRequireCommunityTool(dp_routes = TRUE))
})

test_that("DP-only mode refuses when the DP service is unavailable", {
  h <- tools_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  withr::local_options(list(dsomop.tools.mode = "dp",
                            dsomop.dp.enabled = FALSE))
  expect_error(tools_run(h, "condition.duration_stats",
                         list(concept_id = 201820)),
               "not available on this server")
})

test_that("released order never depends on raw counts inside a band", {
  expect_identical(
    .omopSqlOrderSpec("SELECT a, n_persons FROM t GROUP BY a ORDER BY n_persons DESC"),
    list(list(col = "n_persons", desc = TRUE)))
  expect_identical(
    .omopSqlOrderSpec("SELECT x FROM t ORDER BY p.visit_year, n_persons DESC LIMIT 5"),
    list(list(col = "visit_year", desc = FALSE),
         list(col = "n_persons", desc = TRUE)))
  expect_identical(
    .omopSqlOrderSpec("SELECT ROW_NUMBER() OVER (ORDER BY n) AS r FROM t"),
    list())
  entry <- list(meta = list(adapter = "query",
                            compute = NULL),
                disclosure = list(count_cols = "n_persons"),
                compute = list(sql = "SELECT c, n_persons FROM t ORDER BY n_persons DESC"))
  first <- data.frame(c = c("b", "a", "z"), n_persons = c(19, 16, 40))
  second <- data.frame(c = c("a", "b", "z"), n_persons = c(19, 16, 40))
  withr::local_options(list(dsomop.nfilter.band = 5))
  release <- function(df) {
    spec <- .omopAnalysisRawOrder(df, entry)
    df$n_persons <- vapply(df$n_persons, .bandCount, numeric(1), band_width = 5)
    .omopAnalysisReleasedOrder(df, entry, spec)
  }
  expect_identical(release(first), release(second))
  expect_identical(release(first)$c, c("z", "a", "b"))
})

test_that("model admission follows the DataSHIELD glm rules", {
  withr::local_options(list(nfilter.tab = 3, nfilter.glm = 0.33))
  X <- cbind(common = c(rep(1, 28), 0, 0),   # only two persons without it
             rare = c(1, 1, rep(0, 28)),     # only two persons with it
             ok = rep(c(1, 0), 15),
             ok2 = rep(c(1, 1, 0), 10))
  expect_identical(.omopModelAdmitBinaryDesign(X), c(3L, 4L))
  expect_null(.omopModelAdmitBinaryDesign(X, y = c(1, 1, rep(0, 28))))
  # The dimension cap: (1 + p) <= nfilter.glm * n.
  wide <- matrix(rep(c(1, 0), 30), nrow = 12, ncol = 10)
  expect_length(.omopModelAdmitBinaryDesign(wide), 2L)
})

test_that("CohortMethod outcome releases need distinct outcome persons", {
  withr::local_options(list(nfilter.tab = 3, nfilter.subset = 3,
                            dsomop.nfilter.band = 5))
  expect_identical(.omopCmOutcomeSupported(c(0, 2, 3, NA)),
                   c(FALSE, FALSE, TRUE, FALSE))
  arm <- function(outcomes, outcome_persons, persons = 100,
                  person_days = 1000) {
    list(persons = persons, person_days = person_days, outcomes = outcomes,
         outcome_persons = outcome_persons)
  }
  expect_true(is.na(.omopCmSupportedOutcomes(arm(100, 1), arm(0, 1))))
  # Each arm needs its own outcome persons: a supported total of 3 outcome
  # persons split 2 + 1 is not enough.
  expect_true(is.na(.omopCmSupportedOutcomes(arm(10, 2), arm(4, 1))))
  # Released total = band(13) + band(7) = 10 + 5, never band(20).
  expect_equal(.omopCmSupportedOutcomes(arm(13, 13), arm(7, 7)), 15)
})

test_that("discovery releases reviewed capabilities, not occupancy", {
  catalog <- .achillesReviewedCatalog("achilles_results")
  expect_true(nrow(catalog) > 0L)
  expect_true(all(catalog$result_table == "achilles_results"))
  expect_equal(nrow(.achillesReviewedCatalog(character(0))), 0L)
})

test_that("reviewed questions build dialect-specific SQL for every dialect", {
  reviewed <- .dsomopToolsReviewedQueries()
  captured <- character(0)
  testthat::local_mocked_bindings(
    .executeQuery = function(handle, sql) {
      captured <<- c(captured, sql)
      data.frame()
    },
    .qualifyTable = function(handle, table, schema = NULL) {
      paste0("cdm.", table)
    },
    .vocabLookupConcepts = function(handle, concept_ids) {
      data.frame(concept_id = concept_ids, concept_name = "concept")
    }
  )
  for (dialect in .DSOMOP_TOOLS_DIALECTS) {
    captured <- character(0)
    h <- new.env(parent = emptyenv())
    h$target_dialect <- dialect
    for (id in names(reviewed)) {
      spec <- reviewed[[id]]
      concept_id <- if (!is.null(spec$concept_param)) 201820L else NULL
      if (identical(spec$kind, "partition")) {
        .dsomopToolsPartitionCells(h, spec)
        next
      }
      if (identical(spec$kind, "histogram")) {
        .dsomopToolsHistogramCells(h, spec, concept_id)
      }
      .dsomopToolsSummary(h, spec, concept_id)
    }
    expect_gt(length(captured), 20L)
    sql <- paste(captured, collapse = "\n")
    # No exact statistics are computed in SQL; only counts and sums.
    expect_false(grepl("\\b(STDDEV|STDEV|AVG\\s*\\(\\s*DISTINCT|LIMIT)\\b",
                       sql, ignore.case = TRUE, perl = TRUE), info = dialect)
    if (!dialect %in% c("sql server", "redshift", "snowflake")) {
      expect_false(grepl("DATEDIFF\\s*\\(\\s*day", sql, ignore.case = TRUE),
                   info = dialect)
    }
    if (dialect %in% c("sqlite", "sql server", "mysql", "spark")) {
      expect_false(grepl("EXTRACT\\s*\\(", sql, ignore.case = TRUE),
                   info = dialect)
    }
    if (identical(dialect, "mysql")) {
      expect_false(grepl("AS FLOAT", sql, fixed = TRUE), info = dialect)
    }
  }
})

test_that("pooling keys identify every released row of a reviewed question", {
  .tools_dp_state()
  h <- tools_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  for (mode in c("auto", "standard")) {
    withr::with_options(list(dsomop.tools.mode = mode), {
      registry <- suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
      for (id in names(.dsomopToolsReviewedQueries())) {
        entry <- registry[[paste0("dsomop:", id)]]
        roles <- vapply(.tools_columns(entry), `[[`, character(1L), "role")
        keys <- names(roles)[roles == "key"]
        out <- tools_run(h, id, .tools_params[[id]] %||% list())
        if (nrow(out) > 1L) {
          expect_gt(length(keys), 0L, label = paste(mode, id))
          expect_false(anyDuplicated(do.call(paste, c(
            lapply(out[keys], function(x) ifelse(is.na(x), "<NA>", x)),
            sep = "\r"))) > 0L, label = paste(mode, id))
        }
        # Presentation columns never act as keys.
        expect_false(any(c("concept_name", "bin_label", "gender_name") %in%
                           keys), label = paste(mode, id))
      }
    })
  }
})
