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

test_that("the retained estimands distinguish first-episode time, persons, records and all history", {
  h <- create_test_handle(n_persons = 40L)
  on.exit(cleanup_handle(h))
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
