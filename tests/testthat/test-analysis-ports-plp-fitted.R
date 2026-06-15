# ==============================================================================
# Tests for the four FITTED-MODEL PatientLevelPrediction ports (PLR-5):
#   dsomop:plp.performance  (AUROC / AUPRC)
#   dsomop:plp.calibration  (decile calibration table)
#   dsomop:plp.threshold    (threshold-grid confusion summary)
#   dsomop:plp.diagnostic   (population / outcome / EVP design diagnostics)
# plus their legacy OHDSI-id delegates dsomop:ohdsi.plp.plp_* .
#
# These are live kind="r" entries (the R-in-session principle): each FITS a
# logistic prediction model IN-SESSION over the scoped target population (built
# the same way as dsomop:plp.attrition) and emits ONLY disclosure-safe aggregates
# through the ONE gate (.omopAnalysisGate). The per-subject design matrix,
# coefficients and predicted risks never leave the session. This file proves:
#   (a) all four canonicals + four delegates are registered, dsomop:-prefixed,
#       accepts_cohort, with the declared gate unit and parsing plot recipes;
#   (b) each RUNS over a cohort scope and returns a sane aggregate (no per-subject
#       risk/score column ever surfaces);
#   (c) the GATE + in-fn coupling fire: counts are banded (floor nfilter_band),
#       observed-rate/sensitivity/specificity/PPV/incidence are reconciled from
#       banded counts (never raw) and coupled away when a numerator bands to 0,
#       and the AUROC/AUPRC/EVP scalars are NA below threshold;
#   (d) a too-small cohort fails closed;
#   (e) an un-scoped run is rejected (requires_cohort).
#
# A controlled fixture grounds (b)/(c): a 30-person cohort (persons 1..30, index
# 2020-01-01) with two binary covariate concepts (8888001, 8888003) and a binary
# outcome 7777001 (persons 1..12, inside the TAR window). The covariates carry
# real signal, so the in-session glm produces a usable predicted-risk vector.
# ==============================================================================

PLP_FIT_CANON <- c("dsomop:plp.performance", "dsomop:plp.calibration",
                   "dsomop:plp.threshold", "dsomop:plp.diagnostic")
PLP_FIT_DELEG <- c("dsomop:ohdsi.plp.plp_performances",
                   "dsomop:ohdsi.plp.plp_calibration_summary",
                   "dsomop:ohdsi.plp.plp_threshold_summary",
                   "dsomop:ohdsi.plp.plp_diagnostic_summary")

plp_fit_opts <- function() {
  list(nfilter.subset = 3, nfilter.tab = 3,
       dsomop.nfilter.dist = 10, dsomop.nfilter.band = 5)
}

plp_fit_handle <- function() {
  h <- create_test_handle(n_persons = 60)
  h$person_key <- as.raw(1:16)
  .buildBlueprint(h)
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

# Insert a controlled predictive scenario and return a scoped cohort name.
plp_fit_setup <- function(h, positives = 1:12,
                          covA = c(1:8, 13:20), covC = c(9:14, 21:26),
                          cohort_max = 30L) {
  conn <- h$conn
  DBI::dbExecute(conn, "DELETE FROM condition_occurrence")
  nid <- 0L
  mk <- function(pid, cid, d) {
    nid <<- nid + 1L
    DBI::dbExecute(conn, sprintf(paste0(
      "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
      "condition_concept_id, condition_start_date, condition_end_date, ",
      "condition_type_concept_id, visit_occurrence_id) ",
      "VALUES (%d,%d,%d,%s,%s,44818518,%d)"),
      nid, pid, cid, shQuote(d), shQuote(d), pid))
  }
  for (p in covA) mk(p, 8888001L, "2019-06-01")
  for (p in covC) mk(p, 8888003L, "2019-06-01")
  for (p in positives) mk(p, 7777001L, "2020-03-01")
  for (cid in c(7777001L, 8888001L, 8888003L)) {
    DBI::dbExecute(conn, sprintf(paste0(
      "INSERT INTO concept (concept_id, concept_name, domain_id, vocabulary_id, ",
      "concept_class_id, standard_concept, concept_code, valid_start_date, ",
      "valid_end_date, invalid_reason) VALUES (%d,%s,%s,%s,%s,%s,%s,%s,%s,NULL)"),
      cid, shQuote(paste0("C", cid)), shQuote("Condition"), shQuote("SNOMED"),
      shQuote("Clinical Finding"), shQuote("S"), shQuote(paste0("c", cid)),
      shQuote("1970-01-01"), shQuote("2099-12-31")))
    DBI::dbExecute(conn, sprintf(paste0(
      "INSERT INTO concept_ancestor (ancestor_concept_id, descendant_concept_id, ",
      "min_levels_of_separation, max_levels_of_separation) VALUES (%d,%d,0,0)"),
      cid, cid))
  }
  h$blueprint <- NULL
  .buildBlueprint(h)
  DBI::dbExecute(conn, sprintf(paste0(
    "CREATE TEMP TABLE plp_fit_cohort AS SELECT person_id AS subject_id, ",
    "'2020-01-01' AS cohort_start_date, '2024-12-31' AS cohort_end_date ",
    "FROM person WHERE person_id <= %d"), as.integer(cohort_max)))
  "plp_fit_cohort"
}

plp_fit_params <- function(extra = list()) {
  c(list(outcome_concept_id = "7777001", covariate_domain_code = "0",
         max_covariates = "10"), extra)
}

# --- (a) Registration, metadata, plot recipes --------------------------------

test_that("(a) four fitted PLP canonicals + four delegates are registered", {
  h <- plp_fit_handle()
  on.exit(cleanup_handle(h))
  lst <- .omopAnalysisList(h)
  ids <- c(PLP_FIT_CANON, PLP_FIT_DELEG)
  sub <- lst[lst$name %in% ids, ]
  expect_equal(nrow(sub), length(ids))
  expect_true(all(sub$adapter == "diagnostic"))
  expect_true(all(sub$accepts_cohort))

  unit_of <- stats::setNames(lst$unit, lst$name)
  expect_equal(unit_of[["dsomop:plp.performance"]], "person")
  expect_equal(unit_of[["dsomop:plp.calibration"]], "record")
  expect_equal(unit_of[["dsomop:plp.threshold"]], "record")
  expect_equal(unit_of[["dsomop:plp.diagnostic"]], "record")
})

test_that("(a) each delegate inherits its canonical twin's disclosure", {
  pairs <- list(
    c("plp_performances", ".omopPlpPerformance"),
    c("plp_calibration_summary", ".omopPlpCalibration"),
    c("plp_threshold_summary", ".omopPlpThreshold"),
    c("plp_diagnostic_summary", ".omopPlpDiagnostic"))
  for (p in pairs) {
    builder <- get(p[2])
    d <- .omopPlpDelegateEntry(p[1], builder)
    can <- builder()
    expect_identical(d$disclosure, can$disclosure, info = p[1])
    expect_equal(d$meta$tool_id, "plp")
    expect_equal(d$meta$table_name, p[1])
    expect_equal(d$meta$alias_target, can$name)
    expect_true(d$scope$requires_cohort)
  }
})

test_that("(a) calibration + threshold plot recipes parse to 2-arg functions", {
  for (b in list(.omopPlpCalibration(), .omopPlpThreshold())) {
    expect_true(b$compute$plot$type %in% c("line", "bar"))
    fn <- eval(parse(text = b$compute$plot$code))
    expect_true(is.function(fn))
    expect_named(formals(fn), c("df", "params"))
  }
})

# --- (b) Runs over a cohort scope and returns a sane aggregate ---------------

test_that("(b) diagnostic returns population / EVP design summary", {
  h <- plp_fit_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(plp_fit_opts(), {
    ct <- plp_fit_setup(h)
    d <- .omopAnalysisRun(h, "dsomop:plp.diagnostic",
                          params = plp_fit_params(), scope = ct)
    expect_equal(nrow(d), 1L)
    expect_true(d$population_size %% 5 == 0)        # banded (30)
    expect_true(d$n_predictors >= 1)
    expect_false(is.na(d$outcome_incidence))        # 12 outcomes -> not coupled
    expect_false(is.na(d$events_per_variable))
    # No per-subject column leaks.
    expect_false(any(grepl("risk|score|subject|person_id|_source_",
                           names(d), ignore.case = TRUE)))
  })
})

test_that("(b) performance returns AUROC + AUPRC scalars in [0,1]", {
  h <- plp_fit_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(plp_fit_opts(), {
    ct <- plp_fit_setup(h)
    p <- .omopAnalysisRun(h, "dsomop:plp.performance",
                          params = plp_fit_params(), scope = ct)
    expect_setequal(p$metric, c("AUROC", "AUPRC"))
    expect_true(all(is.na(p$value) | (p$value >= 0 & p$value <= 1)))
    expect_true(all(p$population_size %% 5 == 0))
  })
})

test_that("(b) calibration returns binned deciles (banded persons)", {
  h <- plp_fit_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(plp_fit_opts(), {
    ct <- plp_fit_setup(h)
    cal <- .omopAnalysisRun(h, "dsomop:plp.calibration",
                            params = plp_fit_params(), scope = ct)
    expect_true(nrow(cal) >= 1L)
    expect_true(all(c("mean_predicted", "n_persons", "observed_rate")
                    %in% names(cal)))
    expect_true(all(cal$n_persons %% 5 == 0))       # banded
    # observed_rate is a proportion (or NA when its bin's outcome count banded to 0)
    expect_true(all(is.na(cal$observed_rate) |
                    (cal$observed_rate >= 0 & cal$observed_rate <= 1)))
  })
})

test_that("(b) threshold returns a grid with reconciled sens/spec/ppv", {
  h <- plp_fit_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(plp_fit_opts(), {
    ct <- plp_fit_setup(h)
    th <- .omopAnalysisRun(h, "dsomop:plp.threshold",
                           params = plp_fit_params(), scope = ct)
    expect_true(nrow(th) >= 1L)
    expect_true(all(c("threshold", "sensitivity", "specificity", "ppv")
                    %in% names(th)))
    for (col in c("tp", "fp", "tn", "fn", "n_positive", "n_negative")) {
      v <- th[[col]]
      expect_true(all(is.na(v) | v %% 5 == 0), info = col)   # banded cells
    }
    for (col in c("sensitivity", "specificity", "ppv")) {
      v <- th[[col]]
      expect_true(all(is.na(v) | (v >= 0 & v <= 1)), info = col)
    }
  })
})

test_that("(b) the OHDSI delegate computes the SAME frame as its canonical twin", {
  h <- plp_fit_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(plp_fit_opts(), {
    ct <- plp_fit_setup(h)
    canon <- .omopAnalysisRun(h, "dsomop:plp.diagnostic",
                              params = plp_fit_params(), scope = ct)
    deleg <- .omopAnalysisRun(h, "dsomop:ohdsi.plp.plp_diagnostic_summary",
                              params = plp_fit_params(), scope = ct)
    expect_equal(deleg, canon)
  })
})

# --- (c) Disclosure: scalar NA guards + banded-to-0 coupling -----------------

test_that("(c) scalars NA when an outcome class is below nfilter.subset", {
  # 30-person cohort but only 2 positives -> AUROC/AUPRC NA (positive class
  # below nfilter.subset), and the diagnostic's incidence/EVP coupled to NA.
  h <- plp_fit_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(plp_fit_opts(), {
    ct <- plp_fit_setup(h, positives = 1:2,
                        covA = c(1, 15:25), covC = c(2, 10:20))
    p <- .omopAnalysisRun(h, "dsomop:plp.performance",
                          params = plp_fit_params(), scope = ct)
    expect_true(all(is.na(p$value)))                # AUROC + AUPRC NA
    expect_true(all(p$population_size %% 5 == 0))    # population still banded

    d <- .omopAnalysisRun(h, "dsomop:plp.diagnostic",
                          params = plp_fit_params(), scope = ct)
    expect_true(is.na(d$outcome_incidence))
    expect_true(is.na(d$events_per_variable))
  })
})

test_that("(c) a numerator that bands to 0 couples its ratio away (never 0)", {
  # 30-person cohort, 4 positives: 4 >= nfilter.tab so the count survives, but
  # banding floors 4 -> 0. The reconciled incidence / EVP must be NA (a banded-
  # to-0 numerator must not surface as a rate of 0, which would betray the small
  # count) rather than 0.
  h <- plp_fit_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(plp_fit_opts(), {
    ct <- plp_fit_setup(h, positives = 1:4,
                        covA = c(1:2, 15:25), covC = c(3:4, 10:20))
    d <- .omopAnalysisRun(h, "dsomop:plp.diagnostic",
                          params = plp_fit_params(), scope = ct)
    expect_equal(d$n_outcomes, 0)                   # 4 banded down to 0
    expect_true(is.na(d$outcome_incidence))         # coupled away, NOT 0
    expect_true(is.na(d$events_per_variable))       # coupled away, NOT 0
  })
})

# --- (d) Too-small cohort fails closed ---------------------------------------

test_that("(d) a 2-person cohort fails closed before any fit", {
  h <- plp_fit_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(plp_fit_opts(), {
    plp_fit_setup(h)
    DBI::dbExecute(h$conn, "DROP TABLE IF EXISTS plp_fit_tiny")
    DBI::dbExecute(h$conn, paste0(
      "CREATE TEMP TABLE plp_fit_tiny AS SELECT person_id AS subject_id, ",
      "'2020-01-01' AS cohort_start_date, '2024-12-31' AS cohort_end_date ",
      "FROM person WHERE person_id IN (1, 2)"))
    for (id in PLP_FIT_CANON) {
      expect_error(
        .omopAnalysisRun(h, id, params = plp_fit_params(), scope = "plp_fit_tiny"),
        "insufficient individuals|disclosure threshold", info = id)
    }
  })
})

# --- (e) Un-scoped run rejected (requires_cohort) ----------------------------

test_that("(e) an un-scoped fitted-PLP run is rejected cleanly", {
  h <- plp_fit_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(plp_fit_opts(), {
    for (id in c(PLP_FIT_CANON, PLP_FIT_DELEG)) {
      expect_error(.omopAnalysisRun(h, id, params = plp_fit_params()),
                   "requires a cohort/population scope", info = id)
    }
  })
})

# --- Single-gate invariant ---------------------------------------------------

test_that("the fitted PLP fns return a plain frame gated by the ONE gate", {
  for (b in list(.omopPlpPerformance(), .omopPlpCalibration(),
                 .omopPlpThreshold(), .omopPlpDiagnostic())) {
    expect_true(is.function(b$compute$fn))
    expect_equal(b$compute$kind, "r")
    expect_true(b$disclosure$unit %in% c("person", "record"))
  }
})
