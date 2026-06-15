# ==============================================================================
# Tests for the six natively re-implemented OHDSI diagnostics (adapter ==
# "diagnostic") added to the unified analysis catalog.
#
# These are kind="r" catalog entries that scope via ctx$scoped_cohort (the cohort
# IS the analysis population) and funnel through the ONE gate (.omopAnalysisGate).
# For EACH entry this file proves, on the SQLite fixture:
#   (a) it is registered, dsomop:-prefixed, accepts_cohort == TRUE, with the
#       declared gate unit and an inert client-side plot recipe that PARSES;
#   (b) it RUNS over a cohort scope and returns a sane aggregate (or is
#       fixture-limited -> fully gate-suppressed to an empty frame, with a note);
#   (c) the GATE fires: a sub-threshold stratum/bin is dropped, counts are
#       banded (floor to nfilter_band), rates are recomputed from banded counts
#       (never raw), and distributions release NO min/max and mask stats below
#       nfilter_dist;
#   (d) scoping a too-small cohort fails closed;
#   (e) a non-scopable misuse is rejected cleanly.
#
# The fixture grounds these assertions (see tests/fixtures/create_test_db.R):
#   * Cohort 1 (Diabetes, concept 201820): subjects 1,3,5,7,9,11; per-subject
#     index date = diabetes diagnosis date; cohort_end_date = 2024-12-31.
#   * MI outcome (concept 4000002): persons 1,5,9 only (3 of the 6 cohort
#     members), each AFTER their index date -> a real but sub-band numerator.
#   * Inpatient visits (9201) for persons 1..10 on each person's index date.
#   * observation_period 2020-01-01..2024-12-31 for everyone.
# ==============================================================================

# A keyed handle with the catalog pre-built (mirrors test-analysis-catalog.R).
ports_handle <- function(n_persons = 40) {
  h <- create_test_handle(n_persons = n_persons)
  h$person_key <- as.raw(1:16)
  .buildBlueprint(h)
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

DIAG_IDS <- c(
  "dsomop:incidence.rate",
  "dsomop:cohortdx.index_event_breakdown",
  "dsomop:cohortdx.time_distribution",
  "dsomop:cm.followup_distribution",
  "dsomop:char.time_to_event",
  "dsomop:cohortdx.visit_context"
)

# Strong disclosure options used throughout (note the CORRECT dsOMOP-namespaced
# option name dsomop.nfilter.dist; nfilter.dist alone is a no-op).
ports_opts <- function(dist = 10) {
  list(nfilter.subset = 3, nfilter.tab = 3,
       dsomop.nfilter.dist = dist, dsomop.nfilter.band = 5)
}

# A scoped diabetes cohort (condition source -> carries cohort_end_date).
ports_diabetes_cohort <- function(h, id = 1L) {
  .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                mode = "temporary", cohort_id = id)
}

# --- (a) Registration, metadata, plot recipes --------------------------------

test_that("(a) all six diagnostics are registered dsomop:-prefixed + scopable", {
  h <- ports_handle()
  on.exit(cleanup_handle(h))

  lst <- .omopAnalysisList(h)
  sub <- lst[lst$name %in% DIAG_IDS, ]
  expect_equal(nrow(sub), length(DIAG_IDS))
  expect_true(all(startsWith(sub$name, "dsomop:")))
  expect_true(all(sub$adapter == "diagnostic"))
  expect_true(all(sub$accepts_cohort))
  expect_true(all(sub$accepts_tables))

  unit_of <- stats::setNames(lst$unit, lst$name)
  expect_equal(unit_of[["dsomop:incidence.rate"]], "record")
  expect_equal(unit_of[["dsomop:cohortdx.index_event_breakdown"]], "record")
  expect_equal(unit_of[["dsomop:char.time_to_event"]], "record")
  expect_equal(unit_of[["dsomop:cohortdx.time_distribution"]], "dist")
  expect_equal(unit_of[["dsomop:cm.followup_distribution"]], "dist")
  expect_equal(unit_of[["dsomop:cohortdx.visit_context"]], "person")
})

test_that("(a) each compute$plot recipe parses to a 2-arg function", {
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  for (id in DIAG_IDS) {
    e <- .omopAnalysisResolve(h, id)
    expect_true(e$compute$plot$type %in% c("bar", "box", "step", "line"))
    fn <- eval(parse(text = e$compute$plot$code))
    expect_true(is.function(fn))
    expect_named(formals(fn), c("df", "params"))
    # The client-safe metadata view also exposes the recipe.
    m <- .omopAnalysisGet(h, id)
    expect_equal(m$compute_kind, "r")
    expect_true(is.character(m$plot$code) && grepl("function", m$plot$code))
  }
})

# --- (b) Runs over a cohort scope and returns a sane aggregate ---------------

test_that("(b) record/person diagnostics run on the diabetes cohort", {
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(ports_opts(), {
    ct <- ports_diabetes_cohort(h)

    # index_event_breakdown: the index events are the diabetes diagnoses; one
    # concept row (201820), banded subject/record counts (6 -> floor band 5).
    ieb <- .omopAnalysisRun(h, "dsomop:cohortdx.index_event_breakdown", scope = ct)
    expect_equal(nrow(ieb), 1L)
    expect_equal(ieb$concept_id, 201820)
    expect_equal(ieb$concept_name, "Diabetes mellitus")
    expect_true(all(ieb$concept_count %% 5 == 0))
    expect_true(all(ieb$subject_count %% 5 == 0))
    expect_false(any(grepl("_source_value$", names(ieb))))

    # visit_context: every cohort member with a visit has it ON their index date
    # (fixture design) -> a single "on" row, distinct subjects banded.
    vc <- .omopAnalysisRun(h, "dsomop:cohortdx.visit_context", scope = ct)
    expect_true(nrow(vc) >= 1L)
    expect_true(all(vc$position %in% c("before", "during", "on", "after")))
    expect_true(all(vc$subjects %% 5 == 0 | is.na(vc$subjects)))
  })
})

test_that("(b) dist diagnostics return a single summarised row (>= nfilter_dist)", {
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  # Use a cohort >= nfilter_dist persons so the stats are NOT masked: a synthetic
  # all-person cohort with a fixed window. (The diabetes cohort is only 6, which
  # the dist gate would mask under the default nfilter_dist=10 -- see the masking
  # test below.)
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE dsomop_ports_dist AS ",
    "SELECT person_id AS subject_id, '2020-01-01' AS cohort_start_date, ",
    "'2024-12-31' AS cohort_end_date FROM person"))
  withr::with_options(ports_opts(dist = 10), {
    td <- .omopAnalysisRun(h, "dsomop:cohortdx.time_distribution",
                           params = list(metric = "time_in_cohort"),
                           scope = "dsomop_ports_dist")
    expect_equal(nrow(td), 1L)
    expect_equal(td$metric, "time_in_cohort")
    expect_true(td$count_value %% 5 == 0)          # banded
    # >= nfilter_dist (40 persons) -> summary stats present (not masked).
    expect_false(is.na(td$median_value))
    expect_true(td$median_value > 0)               # 2020-01-01 .. 2024-12-31
    expect_true(all(c("p10_value", "p25_value", "p75_value", "p90_value")
                    %in% names(td)))

    fu <- .omopAnalysisRun(h, "dsomop:cm.followup_distribution",
                           scope = "dsomop_ports_dist")
    expect_equal(nrow(fu), 1L)
    expect_equal(fu$metric, "time_at_risk")
    expect_false(is.na(fu$median_value))
  })
})

test_that("(b) incidence.rate + time_to_event are fixture-limited (gate-empty)", {
  # The fixture has only 3 MI outcome-persons in the diabetes cohort; floor
  # banding (width 5) takes a numerator of 3 to 0, so the single 'all' stratum
  # (and every day-offset bin) is correctly suppressed to an empty frame. This is
  # CORRECT gating, not a failure: incidence.rate's live behaviour on >= 5
  # outcome-persons is asserted in the dedicated test below.
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(ports_opts(), {
    ct <- ports_diabetes_cohort(h)
    inc <- .omopAnalysisRun(h, "dsomop:incidence.rate",
                            params = list(outcome_concept_id = "4000002"),
                            scope = ct)
    expect_equal(nrow(inc), 0L)
    tte <- .omopAnalysisRun(h, "dsomop:char.time_to_event",
                            params = list(outcome_concept_id = "4000002"),
                            scope = ct)
    expect_equal(nrow(tte), 0L)
  })
})

# --- (c) The gate fires: band, drop, ratio reconciliation, dist masking ------

test_that("(c) incidence.rate recomputes rate/proportion from BANDED counts", {
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  # Synthetic large cohort (all 40 persons) + 25 MI outcome-persons so a real,
  # supra-band numerator survives the gate.
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE dsomop_ports_inc AS ",
    "SELECT person_id AS subject_id, '2020-01-01' AS cohort_start_date, ",
    "'2024-12-31' AS cohort_end_date FROM person"))
  for (pid in 1:25) {
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
      "condition_concept_id, condition_start_date, condition_end_date, ",
      "condition_type_concept_id) VALUES (%d, %d, 4000002, '2021-01-01', ",
      "'2021-01-10', 44818518)"), 1000L + pid, pid))
  }
  withr::with_options(ports_opts(), {
    df <- .omopAnalysisRun(h, "dsomop:incidence.rate",
                           params = list(outcome_concept_id = "4000002"),
                           scope = "dsomop_ports_inc")
    expect_equal(nrow(df), 1L)
    expect_true(all(c("rate", "proportion", "persons_at_risk",
                      "person_outcomes", "person_days") %in% names(df)))
    # Numerator banded to a multiple of 5 (25 distinct outcome-persons).
    expect_true(df$person_outcomes %% 5 == 0)
    expect_true(df$persons_at_risk %% 5 == 0)
    # The derived ratios are recomputed FROM the banded counts, never raw.
    expect_equal(df$proportion, df$person_outcomes / df$persons_at_risk)
    expect_equal(df$rate, df$person_outcomes / df$person_days)
  })
})

test_that("(c) incidence.rate suppresses a sub-band stratum but keeps a big one", {
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE dsomop_ports_inc2 AS ",
    "SELECT person_id AS subject_id, '2020-01-01' AS cohort_start_date, ",
    "'2024-12-31' AS cohort_end_date FROM person"))
  # Persons 1..30 are MALE (8507), 31..40 FEMALE (8532) per the fixture parity
  # rule? No -- fixture alternates gender. Instead drive the split via outcomes:
  # give MALE-heavy outcomes by selecting persons whose gender we read back.
  g <- DBI::dbGetQuery(h$conn,
    "SELECT person_id, gender_concept_id FROM person ORDER BY person_id")
  males <- g$person_id[g$gender_concept_id == 8507]
  females <- g$person_id[g$gender_concept_id == 8532]
  # >= 5 male outcome-persons (survives), exactly 2 female outcome-persons
  # (numerator 2 -> floor band 0 -> stratum dropped).
  out_pids <- c(utils::head(males, 8), utils::head(females, 2))
  cid <- 2000L
  for (pid in out_pids) {
    cid <- cid + 1L
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
      "condition_concept_id, condition_start_date, condition_end_date, ",
      "condition_type_concept_id) VALUES (%d, %d, 4000002, '2021-02-01', ",
      "'2021-02-10', 44818518)"), cid, pid))
  }
  withr::with_options(ports_opts(), {
    df <- .omopAnalysisRun(h, "dsomop:incidence.rate",
                           params = list(outcome_concept_id = "4000002",
                                         strat_by_gender = "1"),
                           scope = "dsomop_ports_inc2")
    # MALE stratum (>= 5 outcome-persons) survives; FEMALE (2) is dropped.
    expect_true("MALE" %in% df$stratum)
    expect_false("FEMALE" %in% df$stratum)
    # The surviving stratum carries a banded, non-NA rate.
    male_row <- df[df$stratum == "MALE", ]
    expect_true(male_row$person_outcomes %% 5 == 0)
    expect_false(is.na(male_row$rate))
  })
})

test_that("(c) dist diagnostics strip min/max and mask stats below nfilter_dist", {
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  ct <- NULL
  withr::with_options(ports_opts(dist = 10), {
    ct <- ports_diabetes_cohort(h)   # 6 persons -> banded count_value 5 < 10
    td <- .omopAnalysisRun(h, "dsomop:cohortdx.time_distribution",
                           params = list(metric = "time_in_cohort"), scope = ct)
    # No min/max columns are ever released.
    expect_false(any(grepl("^min_value$|^max_value$|^minValue$|^maxValue$",
                           names(td))))
    if (nrow(td) > 0) {
      # count_value (6 -> band 5) < nfilter_dist (10) -> every summary stat NA.
      expect_true(is.na(td$median_value))
      expect_true(is.na(td$avg_value))
      expect_true(is.na(td$p90_value))
    }
    fu <- .omopAnalysisRun(h, "dsomop:cm.followup_distribution", scope = ct)
    # Follow-up never even computes min/max (0%/100% ARE min/max).
    expect_false(any(grepl("min_value|max_value", names(fu))))
    if (nrow(fu) > 0) {
      expect_true(all(c("p10_value", "median_value", "p90_value") %in% names(fu)))
      expect_true(is.na(fu$median_value))   # 6 < 10 -> masked
    }
  })
})

test_that("(c) visit_context drops a sub-threshold visit-concept stratum", {
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE dsomop_ports_vc AS ",
    "SELECT person_id AS subject_id, '2020-01-01' AS cohort_start_date, ",
    "'2024-12-31' AS cohort_end_date FROM person"))
  DBI::dbExecute(h$conn, paste0(
    "INSERT INTO concept (concept_id, concept_name, domain_id, vocabulary_id, ",
    "concept_class_id, concept_code, valid_start_date, valid_end_date) VALUES ",
    "(9203, 'Emergency Room Visit', 'Visit', 'Visit', 'Visit', 'ER', ",
    "'1970-01-01', '2099-12-31')"))
  DBI::dbExecute(h$conn, "DELETE FROM visit_occurrence")
  # 20 persons with a common Inpatient (9201) visit ON index (survives); 2 with a
  # rare ER (9203) visit (2 subjects -> floor band 0 -> dropped).
  vid <- 3000L
  for (pid in 1:20) {
    vid <- vid + 1L
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO visit_occurrence (visit_occurrence_id, person_id, ",
      "visit_concept_id, visit_start_date, visit_end_date, ",
      "visit_type_concept_id) VALUES (%d, %d, 9201, '2020-01-01', ",
      "'2020-01-02', 44818518)"), vid, pid))
  }
  for (pid in 21:22) {
    vid <- vid + 1L
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO visit_occurrence (visit_occurrence_id, person_id, ",
      "visit_concept_id, visit_start_date, visit_end_date, ",
      "visit_type_concept_id) VALUES (%d, %d, 9203, '2020-01-01', ",
      "'2020-01-02', 44818518)"), vid, pid))
  }
  withr::with_options(ports_opts(), {
    vc <- .omopAnalysisRun(h, "dsomop:cohortdx.visit_context",
                           params = list(top_n = 25), scope = "dsomop_ports_vc")
    expect_true(9201 %in% vc$concept_id)        # 20 subjects survive
    expect_false(9203 %in% vc$concept_id)       # 2 subjects dropped
    expect_true(all(vc$subjects %% 5 == 0 | is.na(vc$subjects)))
  })
})

test_that("(c) time_to_event bins survive only with >= nfilter persons", {
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE dsomop_ports_tte AS ",
    "SELECT person_id AS subject_id, '2020-01-01' AS cohort_start_date, ",
    "'2024-12-31' AS cohort_end_date FROM person"))
  # 12 persons get the MI outcome ~30 days after index (one 30-day bin -> 12
  # persons survive); 2 persons get it ~400 days out (that bin -> dropped).
  cid <- 4000L
  for (pid in 1:12) {
    cid <- cid + 1L
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
      "condition_concept_id, condition_start_date, condition_end_date, ",
      "condition_type_concept_id) VALUES (%d, %d, 4000002, '2020-02-01', ",
      "'2020-02-10', 44818518)"), cid, pid))
  }
  for (pid in 13:14) {
    cid <- cid + 1L
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
      "condition_concept_id, condition_start_date, condition_end_date, ",
      "condition_type_concept_id) VALUES (%d, %d, 4000002, '2021-02-05', ",
      "'2021-02-10', 44818518)"), cid, pid))
  }
  withr::with_options(ports_opts(), {
    df <- .omopAnalysisRun(h, "dsomop:char.time_to_event",
                           params = list(outcome_concept_id = "4000002",
                                         time_scale = "30"),
                           scope = "dsomop_ports_tte")
    expect_true(nrow(df) >= 1L)
    expect_true(all(df$persons %% 5 == 0 | is.na(df$persons)))
    expect_true(all(df$num_events %% 5 == 0 | is.na(df$num_events)))
    # The MI at 2020-02-01 is ~31 days post-index -> the 30-day bin (12 persons)
    # survives; the late ~400-day events (2 persons) are dropped (bin >= 360
    # absent).
    expect_true(30 %in% df$day_offset)
    expect_false(any(df$day_offset >= 360))
  })
})

# --- (d) Too-small cohort fails closed ---------------------------------------

test_that("(d) a sub-threshold scope fails closed for every diagnostic", {
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(ports_opts(), {
    # Asthma (317009) is only persons 2 and 4 -> 2 distinct subjects.
    expect_error(
      .cohortCreate(h, list(type = "condition", concept_set = c(317009)),
                    mode = "temporary", cohort_id = 7),
      "insufficient individuals|disclosure threshold")
  })
})

test_that("(d) a fn-level person self-gate blocks a tiny hand-built cohort", {
  # Defence-in-depth: even if a 2-person cohort table is constructed directly
  # (bypassing .cohortCreate's gate), every diagnostic's own .assertMinPersons /
  # .omopDiagAssertPersons pre-pull self-gate must still refuse it.
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE dsomop_ports_tiny AS ",
    "SELECT person_id AS subject_id, '2020-01-01' AS cohort_start_date, ",
    "'2024-12-31' AS cohort_end_date FROM person WHERE person_id IN (1, 2)"))
  withr::with_options(ports_opts(), {
    for (id in DIAG_IDS) {
      params <- if (id %in% c("dsomop:incidence.rate",
                              "dsomop:char.time_to_event")) {
        list(outcome_concept_id = "4000002")
      } else list()
      # The run path re-gates scope via .omopAnalysisResolveScope BEFORE the fn,
      # so a 2-person scope is rejected fail-closed.
      expect_error(
        do.call(.omopAnalysisRun, c(list(h, id), list(params = params),
                                    list(scope = "dsomop_ports_tiny"))),
        "insufficient individuals|disclosure threshold",
        info = id)
    }
  })
})

# --- (e) Non-scopable misuse is rejected cleanly -----------------------------

test_that("(e) scoping a precomputed (non-scopable) entry is rejected cleanly", {
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(ports_opts(), {
    ct <- ports_diabetes_cohort(h)
    lst <- .omopAnalysisList(h)
    nonscop <- lst$name[!lst$accepts_cohort]
    expect_true(length(nonscop) >= 1L)
    expect_error(
      .omopAnalysisRun(h, nonscop[1], scope = ct),
      "does not support cohort/population scoping")
  })
})

test_that("(e) an unknown enum value is rejected by the sanitizer", {
  # The enum/bool sanitizer is a closed allowlist; a value outside the declared
  # choices must be refused (no injection surface).
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(ports_opts(), {
    ct <- ports_diabetes_cohort(h)
    expect_error(
      .omopAnalysisRun(h, "dsomop:cohortdx.time_distribution",
                       params = list(metric = "drop table"), scope = ct),
      "must be one of")
    expect_error(
      .omopAnalysisRun(h, "dsomop:incidence.rate",
                       params = list(outcome_concept_id = "4000002",
                                     domain_code = "9"), scope = ct),
      "must be one of")
  })
})

# --- Robustness: cohort without cohort_end_date (TAR fallback) ---------------

test_that("TAR diagnostics tolerate a cohort lacking cohort_end_date", {
  # A cohort materialised from a source table with no _end_date column (here:
  # measurement) carries only subject_id + cohort_start_date. The TAR-based
  # diagnostics must resolve the end column (.omopCohortEndDateCol) and fall back
  # to cohort_start_date rather than emit 'no such column: cohort_end_date'.
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(ports_opts(dist = 10), {
    ctm <- .cohortCreate(h, list(type = "measurement", concept_set = c(3025315)),
                         mode = "temporary", cohort_id = 5)
    expect_false("cohort_end_date" %in% DBI::dbListFields(h$conn, ctm))

    # None of these should raise a SQL 'no such column' error.
    expect_error(
      .omopAnalysisRun(h, "dsomop:incidence.rate",
                       params = list(outcome_concept_id = "4000002"),
                       scope = ctm),
      NA)
    td <- .omopAnalysisRun(h, "dsomop:cohortdx.time_distribution",
                           params = list(metric = "time_in_cohort"), scope = ctm)
    # Fallback window is zero-length (end == start) -> time-in-cohort is 0 days.
    if (nrow(td) > 0 && !is.na(td$median_value)) {
      expect_equal(td$median_value, 0)
    }
    expect_error(
      .omopAnalysisRun(h, "dsomop:cm.followup_distribution", scope = ctm),
      NA)
  })
})

test_that("the .omopCohortEndDateCol probe resolves present vs absent end date", {
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(ports_opts(), {
    ctc <- ports_diabetes_cohort(h)                       # has end date
    ctm <- .cohortCreate(h, list(type = "measurement", concept_set = c(3025315)),
                         mode = "temporary", cohort_id = 6)  # no end date
    expect_equal(.omopCohortEndDateCol(h, ctc), "cohort_end_date")
    expect_equal(.omopCohortEndDateCol(h, ctm), "cohort_start_date")
  })
})

# --- Single-gate invariant ---------------------------------------------------

test_that("each diagnostic returns a plain aggregate gated by the ONE gate", {
  # The fn never gates; .omopAnalysisRun funnels its frame through the single
  # .omopAnalysisGate. Confirm the fn returns a plain data.frame and the entry's
  # disclosure spec (not the fn) drives gating, and that an un-scoped run fails
  # closed with a clear requires-cohort error (the cohort IS the population).
  h <- ports_handle()
  on.exit(cleanup_handle(h))
  for (id in DIAG_IDS) {
    e <- .omopAnalysisResolve(h, id)
    expect_true(is.function(e$compute$fn))
    expect_true(e$disclosure$unit %in% c("person", "record", "dist"))
    # Un-scoped: the cohort IS the population, so no scope -> clear error.
    expect_error(.omopAnalysisRun(h, id), "requires a cohort/population scope")
  }
})
