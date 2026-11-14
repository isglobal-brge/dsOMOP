# A-31 Q04/Q05: these selectors were advertised but did not affect selection.
.audit_cases <- list(
  list(id = "dsomop:incidence.rate", parameter = "clean_window", reason = "unsupported_clean_window"),
  list(id = "dsomop:ohdsi.cohort_diagnostics.incidence_rate", parameter = "clean_window", reason = "unsupported_clean_window"),
  list(id = "dsomop:ohdsi.cohort_incidence.incidence_summary", parameter = "clean_window", reason = "unsupported_clean_window"),
  list(id = "dsomop:fe.prevalence", parameter = "time_windows", reason = "unsupported_fe_window"))

test_that("effective audit settings are strict and normalize only supported defaults", {
  for (case in .audit_cases) {
    omitted <- .auditEffectiveSettings(case$id, list())
    for (zero in list(0L, 0, "0", "+0", "-0")) {
      explicit <- .auditEffectiveSettings(case$id, setNames(list(zero), case$parameter))
      expect_identical(explicit, omitted)
    }
    for (value in list(1, -1, "365")) {
      expect_error(.auditEffectiveSettings(case$id, setNames(list(value), case$parameter)), case$reason)
    }
    for (value in list(NULL, NA, NaN, Inf, .5, "0.5", "NaN", "Inf", "secret-sql", TRUE,
                       integer(0), c(0, 1), list(0), structure(0, class = "opaque"))) {
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
