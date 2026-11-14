# WS-7 acceptance (design-2.10.3 sections 10.7, 12.2): the real population,
# matching, reducer and output compilers reproduce the independent WS-0
# oracle (tools/ws0/acceptance_expected.R -> expected.json) on every available
# backend (SQLite, DuckDB, PostgreSQL, MariaDB, MySQL). Expected rows are used
# only for assertions; no roster is constructed from them. The separate
# client test-ws7-client-acceptance gate runs these recipes through the public
# API in memory and staged at k = 5 on SQLite and PostgreSQL.

.ws7_acc_backends <- .ws7_acceptance_backends()
withr::defer(.ws4_cleanup(.ws7_acc_backends), testthat::teardown_env())

# Run one recipe through the native output driver, which resolves each
# population with the WS-6 compiler. Read those same final rosters only after
# compilation, to associate output rows with independent oracle identities.
.ws7_recipe <- function(recipe, check) {
  plan <- .ws7_plan(recipe)
  for (b in names(.ws7_acc_backends)) {
    h <- .ws7_acc_backends[[b]]$handle
    local({
      run <- .ws7_ctx(h, plan)
      on.exit(.ws7_close(run), add = TRUE)
      out <- stats::setNames(as.list(paste0("D_", names(run$ir$outputs))), names(run$ir$outputs))
      compiled <- .planCompileOutputs(run$cctx, out)
      populations <- unique(vapply(run$ir$outputs, function(output) output$rows, ""))
      rosters <- lapply(stats::setNames(populations, populations), function(population) {
        pop <- .compilePopulationByName(run$cctx, population)
        .reduceRosterFrame(run$cctx, pop$roster)
      })
      check(compiled, rosters, b)
    })
  }
}

test_that("WS-7 acceptance exercises every configured database", {
  .ws5_expect_backends(.ws7_acc_backends, "WS-7 acceptance")
})

.ws7_frame <- function(compiled, output, component = output) compiled[[output]]$components[[component]]$frame

.ws7_check_schema <- function(frame, recipe, output, info, ordered = TRUE) {
  released <- .ws7_released(recipe, output)
  if (ordered) expect_identical(names(frame), released, info = info) else
    expect_setequal(names(frame), released)
}

test_that("recipe 01: characterisation table (40 binaries, unit-converted last values, native Charlson)", {
  want <- .ws7_expected("01-characterisation", "table1")
  .ws7_recipe("01-characterisation", function(compiled, rosters, b) {
    frame <- .ws7_frame(compiled, "table1")
    # the released schema; the column order is the normalised IR's (A-11:
    # every variable in declaration order)
    .ws7_check_schema(frame, "01-characterisation", "table1", b, ordered = FALSE)
    ir <- .irNormalize(.irJsonForm(.ws7_plan("01-characterisation")))
    expect_identical(names(frame), c("row_id", "cohort_row_id", "person_id",
                                     as.character(unlist(ir$outputs$table1$columns))), info = b)
    .ws7_compare(frame, rosters$c1, want, setdiff(names(want), c("person_id", "index_date")), info = b)
    expect_identical(sort(as.numeric(rosters$c1$subject_id[frame$cond_covar2 == 1L])), c(1013, 1014), info = b)
  })
})

test_that("recipe 02: baseline and two-TAR competing risks with arm-conditioned switch censoring (A-7)", {
  base <- .ws7_expected("02-active-comparator", "baseline")
  itt <- .ws7_expected("02-active-comparator", "tte.itt")
  ont <- .ws7_expected("02-active-comparator", "tte.on_treatment")
  .ws7_recipe("02-active-comparator", function(compiled, rosters, b) {
    r <- rosters$new_users
    f <- .ws7_frame(compiled, "baseline")
    .ws7_check_schema(f, "02-active-comparator", "baseline", b)
    .ws7_compare(f, r, base, c("arm", "age", "sex", "prior_mi"), info = paste(b, "baseline"))
    expect_identical(names(compiled$tte$components), c("itt", "on_treatment"), info = b)
    for (t in c("itt", "on_treatment")) {
      f <- .ws7_frame(compiled, "tte", t)
      .ws7_check_schema(f, "02-active-comparator", paste0("tte.", t), b)
      want <- if (t == "itt") itt else ont
      .ws7_compare(f, r, want, c("outcome_name", "event", "entry_days_from_index", "exit_days_from_index",
                                 "follow_up_days"), info = paste(b, t))
      expect_true(all(f$follow_up_days == f$exit_days_from_index - f$entry_days_from_index), info = b)
    }
  })
})

test_that("recipe 03 (and 13): distinct-day counts on a parent-inheriting criteria population", {
  want <- .ws7_expected("03-distinct-days", "two_days")
  for (recipe in c("03-distinct-days", "13-parametrised")) {
    .ws7_recipe(recipe, function(compiled, rosters, b) {
      f <- .ws7_frame(compiled, "two_days")
      .ws7_check_schema(f, recipe, "two_days", b)
      .ws7_compare(f, rosters$dm2, want, c("dm_days", "dm_rows"), info = paste(recipe, b))
    })
  }
})

test_that("recipe 04: normalisation before predicates and reducers, closest value ties, censored operators", {
  want <- .ws7_expected("04-units-censored", "labs")
  .ws7_recipe("04-units-censored", function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "labs")
    .ws7_check_schema(f, "04-units-censored", "labs", b)
    .ws7_compare(f, rosters$with_a1c, want, c("a1c_closest", "a1c_n_ge65", "a1c_n_uncensored",
                                              "a1c_n_known_unit", "a1c_max"), info = b)
  })
})

test_that("recipe 05: same-day index rows are distinct episodes; index-visit association", {
  want <- .ws7_expected("05-same-day-same-visit", "episodes")
  .ws7_recipe("05-same-day-same-visit", function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "episodes")
    .ws7_check_schema(f, "05-same-day-same-visit", "episodes", b)
    .ws7_compare(f, rosters$mi_all, want, c("n_same_day_mi", "pci_same_visit", "chest_pain_same_visit",
                                            "inpatient_30"), info = b,
                 keys = c("person_id", "index_date", "index_key"))
  })
})

test_that("recipe 06: covered / effective / era / supply reducers and discontinuation censoring", {
  cov <- .ws7_expected("06-coverage-persistence", "coverage")
  per <- .ws7_expected("06-coverage-persistence", "persistence")
  .ws7_recipe("06-coverage-persistence", function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "coverage")
    .ws7_check_schema(f, "06-coverage-persistence", "coverage", b)
    g <- .ws7_compare(f, rosters$users, cov, c("covered_rec", "covered_eff", "era_count", "first_era_days",
                                               "supply_start", "supply_clipped"), info = b)
    expect_identical(as.integer(unlist(g[g$person_id == 1004, c("covered_rec", "covered_eff")])), c(44L, 60L))
    f <- .ws7_frame(compiled, "persistence")
    .ws7_check_schema(f, "06-coverage-persistence", "persistence", b)
    .ws7_compare(f, rosters$users, per, c("outcome_name", "event", "entry_days_from_index",
                                          "exit_days_from_index", "follow_up_days"), info = b)
  })
})

test_that("recipe 07: dense monthly panel, per-concept identities (A-3) and carry-forward at period start", {
  want <- .ws7_expected("07-monthly-panel", "panel")
  .ws7_recipe("07-monthly-panel", function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "panel")
    .ws7_check_schema(f, "07-monthly-panel", "panel", b)
    .ws7_compare(f, rosters$c1, want, setdiff(names(want), c("person_id", "index_date")), info = b,
                 keys = c("person_id", "index_date", "period_start_days"))
  })
})

test_that("recipe 08: SCCS partition at eras, age bands and calendar years with observed days", {
  want <- .ws7_expected("08-sccs-matrix", "matrix")
  .ws7_recipe("08-sccs-matrix", function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "matrix")
    .ws7_check_schema(f, "08-sccs-matrix", "matrix", b)
    expect_identical(nrow(f), 139L, info = b)
    .ws7_compare(f, rosters$sccs, want, setdiff(names(want), c("person_id", "index_date")), info = b,
                 keys = c("person_id", "index_date", "period_start_days"))
  })
})

test_that("recipe 09: recurrent events with complete = TRUE sentinels and risk sets (A-5)", {
  ev <- .ws7_expected("09-recurrent-events", "recurrent.events")
  rs <- .ws7_expected("09-recurrent-events", "recurrent.risk_sets")
  .ws7_recipe("09-recurrent-events", function(compiled, rosters, b) {
    expect_identical(names(compiled$recurrent$components), c("events", "risk_sets"), info = b)
    f <- .ws7_frame(compiled, "recurrent", "events")
    .ws7_check_schema(f, "09-recurrent-events", "recurrent.events", b)
    .ws7_compare(f, rosters$c1, ev, setdiff(names(ev), c("person_id", "index_date")), info = b,
                 keys = c("person_id", "index_date", "event_number"))
    f <- .ws7_frame(compiled, "recurrent", "risk_sets")
    .ws7_check_schema(f, "09-recurrent-events", "recurrent.risk_sets", b)
    .ws7_compare(f, rosters$c1, rs, setdiff(names(rs), c("person_id", "index_date")), info = b)
  })
})

test_that("recipe 10: matched roster columns (set_id, is_case, weights) and case-time reducers", {
  want <- .ws7_expected("10-matching", "cc_table")
  .ws7_recipe("10-matching", function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "cc_table")
    .ws7_check_schema(f, "10-matching", "cc_table", b)
    r <- rosters$cc
    at <- match(as.character(f$cohort_row_id), as.character(r$cohort_row_id))
    key <- paste(r$subject_id[at], format(r$cohort_start_date[at]), r$set_id[at])
    wkey <- paste(want$person_id, want$index_date, want$set_id)
    expect_identical(length(key), length(wkey), info = b)
    expect_identical(anyDuplicated(key), 0L, info = b)
    expect_setequal(key, wkey)
    g <- f[match(wkey, key), , drop = FALSE]
    for (c in c("set_id", "is_case", "w", "age", "statin_365")) {
      expect_equal(as.numeric(g[[c]]), as.numeric(want[[c]]), info = paste(b, c))
    }
    expect_identical(as.character(g$sex), as.character(want$sex), info = b)
  })
})

test_that("recipe 11: calendar-year incidence denominators and numerators", {
  want <- .ws7_expected("11-incidence-years", "years")
  .ws7_recipe("11-incidence-years", function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "years")
    .ws7_check_schema(f, "11-incidence-years", "years", b)
    .ws7_compare(f, rosters$c1, want, setdiff(names(want), c("person_id", "index_date")), info = b,
                 keys = c("person_id", "index_date", "period_start_days"))
  })
})

test_that("recipe 12: legacy age bands, sex and observed prior days on the Circe population", {
  want <- .ws7_expected("12-circe-baseline", "baseline")
  .ws7_recipe("12-circe-baseline", function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "baseline")
    .ws7_check_schema(f, "12-circe-baseline", "baseline", b)
    .ws7_compare(f, rosters$t2dm_first, want, c("age_group", "sex", "prior_obs"), info = b)
  })
})

test_that("recipe 14: episode-indexed member association and first-line coverage", {
  ep <- .ws7_expected("14-oncology-episodes", "episodes")
  fl <- .ws7_expected("14-oncology-episodes", "first_line")
  .ws7_recipe("14-oncology-episodes", function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "episodes")
    .ws7_check_schema(f, "14-oncology-episodes", "episodes", b)
    .ws7_compare(f, rosters$onc, ep, c("n_members", "n_member_drugs", "first_member_days", "n_specimens"),
                 info = b, keys = c("person_id", "index_date", "index_key"))
    f <- .ws7_frame(compiled, "first_line")
    .ws7_check_schema(f, "14-oncology-episodes", "first_line", b)
    .ws7_compare(f, rosters$first_line, fl, c("line_days", "n_member_drugs"), info = b,
                 keys = c("person_id", "index_date", "index_key"))
  })
})

test_that("recipe 15: costs in one currency, MPR and PDC numerators on union coverage", {
  want <- .ws7_expected("15-costs-adherence", "adherence")
  .ws7_recipe("15-costs-adherence", function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "adherence")
    .ws7_check_schema(f, "15-costs-adherence", "adherence", b)
    g <- .ws7_compare(f, rosters$users, want, setdiff(names(want), c("person_id", "index_date")), info = b)
    p <- g[g$person_id == 1003, ]
    expect_identical(as.integer(c(p$pdc_num_eff_10, p$observed_10, p$covered_eff_10)), c(4L, 4L, 10L), info = b)
  })
})
