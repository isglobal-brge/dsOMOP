# A-31 Q04/Q05: these selectors were advertised but did not affect selection.
.audit_cases <- list(
  list(id = "dsomop:incidence.rate", parameter = "clean_window", reason = "unsupported_clean_window"),
  list(id = "dsomop:ohdsi.cohort_diagnostics.incidence_rate", parameter = "clean_window", reason = "unsupported_clean_window"),
  list(id = "dsomop:ohdsi.cohort_incidence.incidence_summary", parameter = "clean_window", reason = "unsupported_clean_window"),
  list(id = "dsomop:fe.prevalence", parameter = "time_windows", reason = "unsupported_fe_window"))

test_that("effective audit settings are strict and normalize only supported defaults", {
  for (case in .audit_cases) {
    omitted <- .auditEffectiveSettings(case$id, list())
    for (zero in list(0L, 0, "0", "+0", "-0", "0.0", "0e0")) {
      explicit <- .auditEffectiveSettings(case$id, setNames(list(zero), case$parameter))
      expect_identical(explicit, omitted)
    }
    for (value in list(1, -1, "365")) {
      expect_error(.auditEffectiveSettings(case$id, setNames(list(value), case$parameter)), case$reason)
    }
    for (value in list(NULL, NA, NaN, Inf, .5, "0.5", "NaN", "Inf", "secret-sql", TRUE,
                       integer(0), c(0, 1), matrix(0), list(0), structure(0, class = "opaque"))) {
      expect_error(.auditEffectiveSettings(case$id, setNames(list(value), case$parameter)), "invalid_setting")
    }
  }
})

test_that("canonical, protected and alias run paths refuse before snapshot or patient work", {
  entries <- list(.omopDiagIncidenceRate(), .omopFePrevalence())
  entries <- c(entries, .ohdsiPackAliasEntries(NULL))
  registry <- setNames(entries, vapply(entries, `[[`, character(1), "name"))
  local_mocked_bindings(
    .omopAnalysisRegistry = function(handle, force = FALSE) registry,
    .dsomopReadSnapshotActive = function(handle) stop("SNAPSHOT_REACHED"),
    .omopAnalysisResolveScope = function(...) stop("PATIENT_READ"),
    .omopToolsSnapReleaseParams = function(...) stop("PROTECTION_PREPARED"))
  for (mode in c("standard", "protected")) {
    withr::local_options(list(dsomop.tools.mode = mode))
    for (case in .audit_cases) {
      expect_error(.omopAnalysisRun(NULL, case$id,
        params = setNames(list(30), case$parameter)), case$reason)
      expect_error(.omopAnalysisRun(NULL, case$id,
        params = setNames(list(NULL), case$parameter)), "invalid_setting")
    }
    expect_error(.omopAnalysisRun(NULL, "incidence.rate", list(clean_window = 1)),
                 "unsupported_clean_window")
  }
})

test_that("catalogue discovery and sanitizer agree about the active selector", {
  for (entry in list(.omopDiagIncidenceRate(), .omopFePrevalence())) {
    rule <- .auditSettingContract(entry$name)
    param <- Filter(function(x) identical(x$name, rule$parameter), entry$params)[[1L]]
    expect_identical(param$choices, "0")
    omitted <- .omopAnalysisSanitizeParams(entry, list())
    explicit <- .omopAnalysisSanitizeParams(entry, setNames(list(0), rule$parameter))
    expect_identical(omitted, explicit)
  }
})

test_that("fully qualified rejected settings do not even construct the catalogue", {
  local_mocked_bindings(.omopAnalysisRegistry = function(...) stop("CATALOGUE_READ"))
  for (case in .audit_cases) {
    expect_error(.omopAnalysisRun(NULL, case$id,
      params = setNames(list(1), case$parameter)), case$reason)
  }
})

.audit_estimand_handle <- function() {
  h <- create_test_handle(n_persons = 40L)
  cohort <- data.frame(subject_id = rep(1:40, each = 2L),
    cohort_start_date = rep(c("2020-01-01", "2020-02-01"), 40),
    cohort_end_date = rep(c("2020-01-10", "2020-02-10"), 40))
  DBI::dbWriteTable(h$conn, "audit_cohort", cohort)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = 1:40, person_id = 1:40,
    observation_period_start_date = "2019-01-01",
    observation_period_end_date = "2020-12-31", period_type_concept_id = 0L), append = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  events <- data.frame(condition_occurrence_id = 1:120,
    person_id = c(rep(1:20, each = 2), 1:40, 1:40),
    condition_concept_id = c(rep(201820L, 80), rep(201826L, 40)),
    condition_start_date = c(rep(c("2020-01-03", "2020-01-05"), 20),
                             rep("2020-02-03", 40), rep("2019-12-01", 40)))
  DBI::dbWriteTable(h$conn, "condition_occurrence", events, append = TRUE)
  .buildBlueprint(h)
  register_test_temp(h, "audit_cohort")
  h
}

test_that("the retained estimands distinguish first-episode time, persons, records and all history", {
  h <- .audit_estimand_handle()
  on.exit(cleanup_handle(h))
  source <- list(table = "condition_occurrence", person_col = "person_id",
                 concept_col = "condition_concept_id", date_col = "condition_start_date",
                 id_list = "201820")
  substrate <- .omopPersonOutcomeSubstrateSql(h, "audit_cohort", source, 0L, 0L)
  private <- .executeQuery(h, .sql_translate(paste("SELECT * FROM", substrate), h$target_dialect))
  # Independent hand oracle: 40 people x 10 inclusive days; 20 people have
  # two January events each. All 40 February events are in a later episode.
  expect_equal(nrow(private), 40L)
  expect_equal(sum(private$person_days), 400)
  expect_equal(sum(private$person_outcomes), 20)
  expect_equal(sum(private$outcomes), 40)
  expect_equal(sum(private$person_outcomes) / sum(private$person_days), .05)
  ctx <- list(scoped_cohort = "audit_cohort")
  prevalence <- .omopCovariatePrevalence(h, ctx, domain_code = "0", top_n = 50L)
  outside <- prevalence[prevalence$covariate_id == 201826L, ]
  expect_equal(nrow(outside), 1L)
  expect_equal(outside$sum_value, 40)
  # This retained catalogue route includes every December-only member even
  # though no cohort period contains that date; its selector cannot limit it.
  expect_equal(outside$average, 1)
})

test_that("published protected incidence uses the independent first-episode oracle", {
  .release_state(mode = "auto")
  h <- .audit_estimand_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  symbol <- "audit_estimand"
  .setHandle(symbol, h)
  on.exit(.removeHandle(symbol), add = TRUE)
  real <- .dsomopReleaseBuildC6Tree
  captured <- NULL
  expected <- NULL
  local_mocked_bindings(.dsomopReleaseBuildC6Tree = function(policy, protection, owner, sub, ...) {
    captured <<- sub
    # Independent scientific oracle passed through the unchanged production
    # protection mechanism. No noise is disabled and no published gate is mocked.
    oracle <- sub
    oracle$event <- as.integer(as.integer(oracle$person_id) <= 20L)
    oracle$time <- rep(10, 40L)
    oracle$recurrent <- 2 * oracle$event
    expected <<- real(policy, protection, owner, oracle, ...)
    real(policy, protection, owner, sub, ...)
  })
  params <- list(outcome_concept_id = "201820", tar_start_offset = "0",
    tar_end_offset = "0", tar_anchor_start = "start", tar_anchor_end = "end")
  published <- suppressWarnings(omopAnalysisRunDS(symbol, "dsomop:incidence.rate",
    params = params, scope = "audit_cohort"))
  expect_equal(nrow(captured), 40L)
  expect_equal(sum(captured$event), 20)
  expect_equal(sum(captured$time), 400)
  expect_equal(sum(captured$recurrent), 40)
  expect_equal(nrow(published), 1L)
  expect_true(attr(published, "dsomop_protection")$protection %in% c("fixed_synopsis", "realised_sticky"))
  cells <- .dsomopReleaseC6TreeView(expected, FALSE, FALSE, FALSE, 10L)$cells
  expect_equal(published$persons_at_risk, .dsomopReleaseRound(cells$N_d))
  expect_equal(published$person_outcomes, .dsomopReleaseRound(cells$E_d))
  expect_equal(published$outcomes, .dsomopReleaseRound(cells$R_d))
  expect_equal(published$person_days, .dsomopReleaseRound(cells$T_d))
  expect_equal(published$rate, .dsomopReleaseRound(ifelse(published$person_days > 0,
    published$person_outcomes / published$person_days, NA_real_)))
  explicit <- suppressWarnings(omopAnalysisRunDS(symbol, "dsomop:incidence.rate",
    params = c(params, list(clean_window = 0L)), scope = "audit_cohort"))
  expect_identical(.release_values(published), .release_values(explicit))
  expect_identical(attr(published, "dsomop_release"), attr(explicit, "dsomop_release"))
})

test_that("a bounded native feature excludes the catalogue's outside-period events", {
  .release_state()
  h <- .audit_estimand_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  .ws7_keys(list(h))
  symbol <- "audit_fe_window"
  .setHandle(symbol, h)
  on.exit(.removeHandle(symbol), add = TRUE)
  catalogue <- suppressWarnings(omopAnalysisRunDS(symbol, "dsomop:fe.prevalence",
    list(domain_code = "0"), scope = "audit_cohort"))
  outside <- catalogue[catalogue$covariate_id == 201826L, ]
  expect_equal(nrow(outside), 1L)
  expect_gt(outside$sum_value, 0)
  explicit <- suppressWarnings(omopAnalysisRunDS(symbol, "dsomop:fe.prevalence",
    list(domain_code = "0", time_windows = 0L), scope = "audit_cohort"))
  expect_identical(.release_values(catalogue), .release_values(explicit))
  plan <- .ws7_scenario_plan(list(
    bounded = list(source = list(event = "outside"), reducer = list(fn = "count",
      window = list(start = 0L, end = 9L, anchor = "index"))),
    history = list(source = list(event = "outside"), reducer = list(fn = "count"))))
  plan$events <- list(outside = list(table = "condition_occurrence",
    concept_set = list(concepts = list(201826L))))
  cohort <- DBI::dbReadTable(h$conn, "audit_cohort")
  cohort$cohort_start_date <- as.Date(cohort$cohort_start_date)
  cohort$cohort_end_date <- as.Date(cohort$cohort_end_date)
  actual <- .ws7_scenario_run(h, plan, frame = cohort)$frame
  expect_equal(nrow(actual), 80L)
  expect_true(all(actual$bounded == 0L))
  expect_true(all(actual$history == 1L))
  expect_equal(length(unique(actual$pid)), 40L)
})
