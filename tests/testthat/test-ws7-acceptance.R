# WS-7 acceptance on roster doubles (design-2.10.3 sections 10.7, 12.2): the
# WS-0 oracle (tools/ws0/acceptance_expected.R -> expected.json) reproduced by
# the reducer and output compilers over hand-built rosters of the frozen
# section 6.9 contract, on every backend available (SQLite, DuckDB,
# PostgreSQL, MariaDB, MySQL). The rosters stand in for WS-6's populations
# compiler until it lands (some are built from the oracle's own episode keys,
# where the population is first_eligible or matched); the recipes then run end
# to end through the public client API (test-ws7-client-acceptance in the
# client).

.ws7_acc_backends <- .ws7_backends()
withr::defer(.ws4_cleanup(.ws7_acc_backends), testthat::teardown_env())

# Run one recipe on every backend: build the roster doubles, compile the
# roots, and hand each backend's compiled outputs and roster frames to check().
.ws7_recipe <- function(recipe, populations, check) {
  plan <- .ws7_plan(recipe)
  for (b in names(.ws7_acc_backends)) {
    h <- .ws7_acc_backends[[b]]$handle
    run <- .ws7_ctx(h, plan)
    pops <- populations(run, h)
    out <- stats::setNames(as.list(paste0("D_", names(run$ir$outputs))), names(run$ir$outputs))
    compiled <- .planCompileOutputs(run$cctx, out, populations = pops)
    rosters <- lapply(pops, function(p) .reduceRosterFrame(run$cctx, p$roster))
    check(compiled, rosters, b)
    .ws7_close(run)
  }
}

.ws7_frame <- function(compiled, output, component = output) compiled[[output]]$components[[component]]$frame

.ws7_check_schema <- function(frame, recipe, output, info, ordered = TRUE) {
  released <- .ws7_released(recipe, output)
  if (ordered) expect_identical(names(frame), released, info = info) else
    expect_setequal(names(frame), released)
}

test_that("recipe 01: characterisation table (40 binaries, unit-converted last values, native Charlson)", {
  want <- .ws7_expected("01-characterisation", "table1")
  .ws7_recipe("01-characterisation", function(run, h) {
    list(c1 = .ws7_population(run, "c1", .ws7_cohort_frame(h, 1L)))
  }, function(compiled, rosters, b) {
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

# Roster double of recipe 02: the first eligible ACEI/ARB era per person
# (>= 365 observed days before it under unique coverage, no era of either arm
# starting in [index - 365, index - 1]), index_end = the course end capped at
# op_end (interval_end strategy), index_source = the arm.
.ws7_double_02 <- function(run, h) {
  ctx <- run$cctx
  courses <- lapply(c(acei = "acei", arb = "arb"), function(a) {
    r <- .reduceFetch(ctx, .compileIntervalByName(ctx, a)$course, order = c("person_id", "start_date"))
    data.frame(person_id = as.numeric(r$person_id), s = r$start_date, e = r$end_date, arm = a,
               key = as.character(r$era_key), ord = if (a == "acei") 1L else 2L, stringsAsFactors = FALSE)
  })
  cand <- do.call(rbind, courses)
  ops <- .executeQuery(h, paste0("SELECT person_id, observation_period_start_date AS s, ",
                                 "observation_period_end_date AS e FROM ",
                                 .qualifyTable(h, "observation_period", h$cdm_schema)))
  ops$s <- as.Date(substr(as.character(ops$s), 1L, 10L))
  ops$e <- as.Date(substr(as.character(ops$e), 1L, 10L))
  rows <- list()
  for (pid in sort(unique(cand$person_id))) {
    cc <- cand[cand$person_id == pid, , drop = FALSE]
    cc <- cc[order(cc$s, cc$ord, cc$key), , drop = FALSE]
    for (i in seq_len(nrow(cc))) {
      cov <- ops[ops$person_id == pid & ops$s <= cc$s[[i]] & ops$e >= cc$s[[i]], , drop = FALSE]
      if (nrow(cov) != 1L || cov$s > cc$s[[i]] - 365L) next
      if (any(cand$person_id == pid & cand$s >= cc$s[[i]] - 365L & cand$s <= cc$s[[i]] - 1L)) next
      rows[[length(rows) + 1L]] <- data.frame(subject_id = pid, cohort_start_date = cc$s[[i]],
                                              cohort_end_date = cc$e[[i]], index_source = cc$arm[[i]],
                                              index_key = cc$key[[i]], stringsAsFactors = FALSE)
      break
    }
  }
  list(new_users = .ws7_population(run, "new_users", do.call(rbind, rows)))
}

test_that("recipe 02: baseline and two-TAR competing risks with arm-conditioned switch censoring (A-7)", {
  base <- .ws7_expected("02-active-comparator", "baseline")
  itt <- .ws7_expected("02-active-comparator", "tte.itt")
  ont <- .ws7_expected("02-active-comparator", "tte.on_treatment")
  .ws7_recipe("02-active-comparator", .ws7_double_02, function(compiled, rosters, b) {
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

# Roster double of a criteria child of cohort 1 (parent inheritance): the
# parent's covered episodes that satisfy `keep(pid, index)`.
.ws7_double_child <- function(run, h, pop, keep) {
  fr <- .ws7_cohort_frame(h, 1L)
  fr <- fr[vapply(seq_len(nrow(fr)), function(i) keep(fr$subject_id[[i]], fr$cohort_start_date[[i]]),
                  logical(1)), , drop = FALSE]
  stats::setNames(list(.ws7_population(run, pop, fr)), pop)
}

.ws7_dates <- function(h, sql) {
  x <- .executeQuery(h, sql)
  for (c in names(x)) if (grepl("date|^d$", c)) x[[c]] <- as.Date(substr(as.character(x[[c]]), 1L, 10L))
  x
}

test_that("recipe 03 (and 13): distinct-day counts on a parent-inheriting criteria population", {
  want <- .ws7_expected("03-distinct-days", "two_days")
  for (recipe in c("03-distinct-days", "13-parametrised")) {
    .ws7_recipe(recipe, function(run, h) {
      dm <- .ws7_dates(h, paste0("SELECT c.person_id, c.condition_start_date AS d FROM ",
                                 .qualifyTable(h, "condition_occurrence", h$cdm_schema), " c WHERE ",
                                 "c.condition_concept_id IN (SELECT descendant_concept_id FROM ",
                                 .qualifyTable(h, "concept_ancestor", h$cdm_schema),
                                 " WHERE ancestor_concept_id = 201820)"))
      .ws7_double_child(run, h, "dm2", function(pid, idx) {
        d <- dm$d[dm$person_id == pid]
        length(unique(d[d >= idx - 365L & d <= idx])) >= 2L
      })
    }, function(compiled, rosters, b) {
      f <- .ws7_frame(compiled, "two_days")
      .ws7_check_schema(f, recipe, "two_days", b)
      .ws7_compare(f, rosters$dm2, want, c("dm_days", "dm_rows"), info = paste(recipe, b))
    })
  }
})

test_that("recipe 04: normalisation before predicates and reducers, closest value ties, censored operators", {
  want <- .ws7_expected("04-units-censored", "labs")
  .ws7_recipe("04-units-censored", function(run, h) {
    m <- .ws7_dates(h, paste0("SELECT person_id, measurement_date AS d FROM ",
                              .qualifyTable(h, "measurement", h$cdm_schema),
                              " WHERE measurement_concept_id = 3004410"))
    .ws7_double_child(run, h, "with_a1c", function(pid, idx) {
      d <- m$d[m$person_id == pid]
      any(d >= idx - 180L & d <= idx)
    })
  }, function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "labs")
    .ws7_check_schema(f, "04-units-censored", "labs", b)
    .ws7_compare(f, rosters$with_a1c, want, c("a1c_closest", "a1c_n_ge65", "a1c_n_uncensored",
                                              "a1c_n_known_unit", "a1c_max"), info = b)
  })
})

test_that("recipe 05: same-day index rows are distinct episodes; index-visit association", {
  want <- .ws7_expected("05-same-day-same-visit", "episodes")
  .ws7_recipe("05-same-day-same-visit", function(run, h) {
    mi <- .ws7_dates(h, paste0(
      "SELECT c.condition_occurrence_id AS id, c.person_id, c.condition_start_date AS d, ",
      "c.condition_end_date AS e, c.visit_occurrence_id AS v FROM ",
      .qualifyTable(h, "condition_occurrence", h$cdm_schema), " c WHERE c.condition_concept_id IN ",
      "(SELECT descendant_concept_id FROM ", .qualifyTable(h, "concept_ancestor", h$cdm_schema),
      " WHERE ancestor_concept_id = 4329847)"))
    fr <- data.frame(subject_id = as.numeric(mi$person_id), cohort_start_date = mi$d,
                     cohort_end_date = as.Date(ifelse(is.na(mi$e), mi$d, mi$e), origin = "1970-01-01"),
                     index_source = "mi", index_key = as.character(mi$id),
                     index_visit_id = as.numeric(mi$v), stringsAsFactors = FALSE)
    list(mi_all = .ws7_population(run, "mi_all", fr))
  }, function(compiled, rosters, b) {
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
  .ws7_recipe("06-coverage-persistence", function(run, h) {
    list(users = .ws7_population(run, "users", .ws7_cohort_frame(h, 1001L)))
  }, function(compiled, rosters, b) {
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
  .ws7_recipe("07-monthly-panel", function(run, h) {
    list(c1 = .ws7_population(run, "c1", .ws7_cohort_frame(h, 1L)))
  }, function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "panel")
    .ws7_check_schema(f, "07-monthly-panel", "panel", b)
    .ws7_compare(f, rosters$c1, want, setdiff(names(want), c("person_id", "index_date")), info = b,
                 keys = c("person_id", "index_date", "period_start_days"))
  })
})

test_that("recipe 08: SCCS partition at eras, age bands and calendar years with observed days", {
  want <- .ws7_expected("08-sccs-matrix", "matrix")
  .ws7_recipe("08-sccs-matrix", function(run, h) {
    list(sccs = .ws7_population(run, "sccs", .ws7_cohort_frame(h, 1002L)))
  }, function(compiled, rosters, b) {
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
  .ws7_recipe("09-recurrent-events", function(run, h) {
    list(c1 = .ws7_population(run, "c1", .ws7_cohort_frame(h, 1L)))
  }, function(compiled, rosters, b) {
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
  .ws7_recipe("10-matching", function(run, h) {
    fr <- data.frame(subject_id = want$person_id, cohort_start_date = as.Date(want$index_date),
                     cohort_end_date = as.Date(want$index_date), index_source = "match:cc",
                     index_key = paste0(want$set_id, "|", ifelse(want$is_case == 1L, "case", "control")),
                     set_id = want$set_id, is_case = want$is_case, sampling_weight = want$w,
                     stringsAsFactors = FALSE)
    list(cc = .ws7_population(run, "cc", fr))
  }, function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "cc_table")
    .ws7_check_schema(f, "10-matching", "cc_table", b)
    r <- rosters$cc
    at <- match(as.character(f$cohort_row_id), as.character(r$cohort_row_id))
    key <- paste(r$subject_id[at], format(r$cohort_start_date[at]), r$set_id[at])
    wkey <- paste(want$person_id, want$index_date, want$set_id)
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
  .ws7_recipe("11-incidence-years", function(run, h) {
    list(c1 = .ws7_population(run, "c1", .ws7_cohort_frame(h, 1L)))
  }, function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "years")
    .ws7_check_schema(f, "11-incidence-years", "years", b)
    .ws7_compare(f, rosters$c1, want, setdiff(names(want), c("person_id", "index_date")), info = b,
                 keys = c("person_id", "index_date", "period_start_days"))
  })
})

test_that("recipe 12: legacy age bands, sex and observed prior days on the Circe population", {
  want <- .ws7_expected("12-circe-baseline", "baseline")
  .ws7_recipe("12-circe-baseline", function(run, h) {
    fr <- data.frame(subject_id = want$person_id, cohort_start_date = as.Date(want$index_date),
                     cohort_end_date = as.Date(want$index_date), index_source = "type_2_diabetes_dx",
                     index_key = paste0("k", seq_len(nrow(want))), stringsAsFactors = FALSE)
    list(t2dm_first = .ws7_population(run, "t2dm_first", fr, cap_end = FALSE))
  }, function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "baseline")
    .ws7_check_schema(f, "12-circe-baseline", "baseline", b)
    .ws7_compare(f, rosters$t2dm_first, want, c("age_group", "sex", "prior_obs"), info = b)
  })
})

test_that("recipe 14: episode-indexed member association and first-line coverage", {
  ep <- .ws7_expected("14-oncology-episodes", "episodes")
  fl <- .ws7_expected("14-oncology-episodes", "first_line")
  .ws7_recipe("14-oncology-episodes", function(run, h) {
    e <- .ws7_dates(h, paste0("SELECT episode_id AS id, person_id, episode_start_date AS d, ",
                              "episode_end_date AS e FROM ", .qualifyTable(h, "episode", h$cdm_schema),
                              " WHERE episode_concept_id = 32531"))
    fr <- data.frame(subject_id = as.numeric(e$person_id), cohort_start_date = e$d, cohort_end_date = e$e,
                     index_source = "episodes", index_key = as.character(e$id), stringsAsFactors = FALSE)
    first <- fr[order(fr$subject_id, fr$cohort_start_date, fr$cohort_end_date, fr$index_key), , drop = FALSE]
    first <- first[!duplicated(first$subject_id), , drop = FALSE]
    list(onc = .ws7_population(run, "onc", fr), first_line = .ws7_population(run, "first_line", first))
  }, function(compiled, rosters, b) {
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
  .ws7_recipe("15-costs-adherence", function(run, h) {
    list(users = .ws7_population(run, "users", .ws7_cohort_frame(h, 1001L)))
  }, function(compiled, rosters, b) {
    f <- .ws7_frame(compiled, "adherence")
    .ws7_check_schema(f, "15-costs-adherence", "adherence", b)
    g <- .ws7_compare(f, rosters$users, want, setdiff(names(want), c("person_id", "index_date")), info = b)
    p <- g[g$person_id == 1003, ]
    expect_identical(as.integer(c(p$pdc_num_eff_10, p$observed_10, p$covered_eff_10)), c(4L, 4L, 10L), info = b)
  })
})
