# ==============================================================================
# Tests for the live R-in-session CohortMethod Kaplan-Meier curve port
# (dsomop:cm.kaplan_meier, overlaying dsomop:ohdsi.cohort_method.cm_kaplan_meier_dist).
#
# This kind="r" two-population entry loads the per-subject (arm, time, event)
# frame into the server-side R session, fits survival::survfit, and emits a coarse
# fixed-grid per-arm survival curve whose at-risk/event counts are banded and whose
# survival_probability is recomputed from those banded counts. It funnels through
# the ONE gate (.omopAnalysisGate). This file proves, on the SQLite fixture:
#   (a) both ids resolve, two-population (max_tables=2), unit=record, plot parses;
#   (b) it RUNS over two scoped arms and returns a sane banded KM curve;
#   (c) survival_probability rests ONLY on banded counts (recomputable from them),
#       counts are banded, thin-tail bins are dropped;
#   (d) un-scoped / single-arm / sub-threshold scope fail closed or gate-empty.
# ==============================================================================

km_handle <- function(n_persons = 40) {
  h <- create_test_handle(n_persons = n_persons)
  h$person_key <- as.raw(1:16)
  .buildBlueprint(h)
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

km_opts <- function() {
  list(nfilter.subset = 3, nfilter.tab = 3,
       dsomop.nfilter.dist = 10, dsomop.nfilter.band = 5)
}

KM_IDS <- c("dsomop:cm.kaplan_meier",
            "dsomop:ohdsi.cohort_method.cm_kaplan_meier_dist")

# Two synthetic arms (20 persons each) with a long follow-up window and MI
# (4000002, a clean leaf outcome in the fixture vocab) inserted at staggered
# dates so the KM curve has events across the first monthly bins.
km_setup_arms <- function(h) {
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE km_target AS SELECT person_id AS subject_id, ",
    "'2020-01-01' AS cohort_start_date, '2023-12-31' AS cohort_end_date ",
    "FROM person WHERE person_id BETWEEN 1 AND 20"))
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE km_comparator AS SELECT person_id AS subject_id, ",
    "'2020-01-01' AS cohort_start_date, '2023-12-31' AS cohort_end_date ",
    "FROM person WHERE person_id BETWEEN 21 AND 40"))
  cid <- 8000L
  add_mi <- function(pid, dt) {
    cid <<- cid + 1L
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
      "condition_concept_id, condition_start_date, condition_end_date, ",
      "condition_type_concept_id) VALUES (%d, %d, 4000002, '%s', '%s', ",
      "44818518)"), cid, pid, dt, dt))
  }
  # target: 10 events ~bin 2 (2020-02-15 ~ 45 days)
  for (pid in 1:10)   add_mi(pid, "2020-02-15")
  # comparator: 5 events ~bin 2, 5 events ~bin 4 (2020-04-15 ~ 105 days)
  for (pid in 21:25) add_mi(pid, "2020-02-15")
  for (pid in 26:30) add_mi(pid, "2020-04-15")
  invisible(NULL)
}

# ---------------------------------------------------------------------------- #
# (a) Discovery: both ids resolve, two-population, unit=record, plot parses.
# ---------------------------------------------------------------------------- #

test_that("(a) both KM ids resolve as two-population record entries", {
  h <- km_handle()
  on.exit(cleanup_handle(h))
  for (id in KM_IDS) {
    e <- .omopAnalysisResolve(h, id)
    expect_equal(as.integer(e$scope$max_tables), 2L)
    expect_equal(e$disclosure$unit, "record")
    expect_equal(e$disclosure$person_id_col, "at_risk")
    expect_true("survival" %in% (e$dependencies$packages %||% character(0)))
    fn <- eval(parse(text = e$compute$plot$code))
    expect_true(is.function(fn))
    expect_named(formals(fn), c("df", "params"))
  }
})

# ---------------------------------------------------------------------------- #
# (b) Runs over two arms; returns a sane banded KM curve.
# ---------------------------------------------------------------------------- #

test_that("(b) cm.kaplan_meier returns a per-arm banded survival curve", {
  h <- km_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(km_opts(), {
    km_setup_arms(h)
    df <- .omopAnalysisRun(h, "dsomop:cm.kaplan_meier",
                           params = list(outcome_concept_id = "4000002",
                                         outcome_domain_code = "0",
                                         tar_start_offset = "0",
                                         time_unit = "month"),
                           scope = list("km_target", "km_comparator"))
    expect_true(nrow(df) >= 1L)
    expect_true(all(c("arm", "time_bin", "bin_start_days", "bin_end_days",
                      "at_risk", "events", "survival_probability") %in%
                    names(df)))
    expect_true(all(df$arm %in% c("target", "comparator")))
    # Every released count is banded (floor to nfilter_band) or NA.
    expect_true(all(df$at_risk %% 5 == 0 | is.na(df$at_risk)))
    expect_true(all(df$events %% 5 == 0 | is.na(df$events)))
    # Survival probability is a proper probability in [0, 1] where defined.
    sp <- df$survival_probability[!is.na(df$survival_probability)]
    expect_true(all(sp >= 0 & sp <= 1))
    # Both arms produced at least the first bin (20 at risk -> banded 20).
    expect_true(all(c("target", "comparator") %in% df$arm))
    first_target <- df[df$arm == "target" &
                         df$time_bin == min(df$time_bin[df$arm == "target"]), ]
    expect_equal(first_target$at_risk, 20)
    expect_false(any(grepl("_source_value$", names(df))))
  })
})

test_that("(b) the OHDSI twin id computes the SAME live curve", {
  h <- km_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(km_opts(), {
    km_setup_arms(h)
    p <- list(outcome_concept_id = "4000002", tar_start_offset = "0")
    a <- .omopAnalysisRun(h, "dsomop:cm.kaplan_meier", params = p,
                          scope = list("km_target", "km_comparator"))
    b <- .omopAnalysisRun(h, "dsomop:ohdsi.cohort_method.cm_kaplan_meier_dist",
                          params = p, scope = list("km_target", "km_comparator"))
    expect_equal(a, b)
  })
})

# ---------------------------------------------------------------------------- #
# (c) Disclosure: survival_probability rests only on banded counts; thin tail
#     dropped.
# ---------------------------------------------------------------------------- #

test_that("(c) survival_probability is recomputable from the BANDED counts only", {
  h <- km_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(km_opts(), {
    km_setup_arms(h)
    df <- .omopAnalysisRun(h, "dsomop:cm.kaplan_meier",
                           params = list(outcome_concept_id = "4000002",
                                         tar_start_offset = "0"),
                           scope = list("km_target", "km_comparator"))
    expect_true(nrow(df) >= 1L)
    # Re-derive the KM step from the RELEASED (banded) at_risk/events per arm and
    # confirm survival_probability matches it exactly: the released value cannot
    # encode any sub-band information, so a raw KM step / event time is not
    # recoverable by differencing.
    for (arm in unique(df$arm)) {
      ad <- df[df$arm == arm, , drop = FALSE]
      ad <- ad[order(ad$time_bin), , drop = FALSE]
      sp <- 1
      for (i in seq_len(nrow(ad))) {
        ar <- ad$at_risk[i]; ev <- ad$events[i]
        if (!is.na(ar) && !is.na(ev) && ar > 0) {
          sp <- sp * (1 - ev / ar)
          expect_equal(ad$survival_probability[i], sp)
        } else {
          expect_true(is.na(ad$survival_probability[i]))
        }
      }
    }
  })
})

test_that("(c) thin-tail bins below nfilter.subset are dropped (no 1-subject tail)", {
  h <- km_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(km_opts(), {
    km_setup_arms(h)
    df <- .omopAnalysisRun(h, "dsomop:cm.kaplan_meier",
                           params = list(outcome_concept_id = "4000002",
                                         tar_start_offset = "0"),
                           scope = list("km_target", "km_comparator"))
    # The follow-up window is identical for all 20 subjects per arm, so at_risk
    # stays 20 until the censor bin then drops to 0 in one step: every emitted bin
    # must carry a banded at_risk >= nfilter.subset (no thin single-subject tail).
    expect_true(all(df$at_risk >= 3 | is.na(df$at_risk)))
    # All at-risk values are full bands (20) here; none is a sub-threshold residue.
    expect_true(all(df$at_risk[!is.na(df$at_risk)] %% 5 == 0))
  })
})

# ---------------------------------------------------------------------------- #
# (d) Wrong arity / sub-threshold / un-scoped fail closed.
# ---------------------------------------------------------------------------- #

test_that("(d) a single-arm scope is rejected (two populations required)", {
  h <- km_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(km_opts(), {
    km_setup_arms(h)
    expect_error(
      .omopAnalysisRun(h, "dsomop:cm.kaplan_meier",
                       params = list(outcome_concept_id = "4000002"),
                       scope = "km_target"),
      "exactly two scope elements")
  })
})

test_that("(d) a sub-threshold arm fails closed", {
  h <- km_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(km_opts(), {
    km_setup_arms(h)
    DBI::dbExecute(h$conn, paste0(
      "CREATE TEMP TABLE km_tiny AS SELECT person_id AS subject_id, ",
      "'2020-01-01' AS cohort_start_date, '2023-12-31' AS cohort_end_date ",
      "FROM person WHERE person_id IN (1, 2)"))
    expect_error(
      .omopAnalysisRun(h, "dsomop:cm.kaplan_meier",
                       params = list(outcome_concept_id = "4000002"),
                       scope = list("km_target", "km_tiny")),
      "insufficient individuals|disclosure threshold")
  })
})

test_that("(d) the canonical id un-scoped returns a gate-safe empty frame", {
  # cm.kaplan_meier mirrors cm.mdrr: requires_cohort = FALSE, so an un-scoped run
  # is not an error but a gate-safe empty frame (no two-population pair).
  h <- km_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(km_opts(), {
    df <- .omopAnalysisRun(h, "dsomop:cm.kaplan_meier",
                           params = list(outcome_concept_id = "4000002"))
    expect_equal(nrow(df), 0L)
  })
})
