# Output formats of design-2.10.3 sections 2.9 and 6.12 (WS-7) beyond the
# acceptance corpus: survival (composite / named, competing risks, entry,
# prior outcome, explicit censoring without an implicit cohort-end cap vs the
# v1 flag, same-day priority, washout, last observed day and re-entry,
# counting process, multi-state), long (projections, relations, event log,
# broadcast), intervals, temporal covariates, sparse, membership, dictionary,
# joined, date handling -- on every backend available.

.ws7_out_backends <- .ws7_scenario_backends()
withr::defer(.ws4_cleanup(.ws7_out_backends), testthat::teardown_env())

.ws7_sv_frame <- function(pids = 7101:7107) .ws7_scenario_frame(pids, index = "2020-01-10", end = "2020-06-30")

.ws7_sv_plan <- function(options, coverage = NULL) {
  base <- list(outcomes = list(a = list(event = "a"), b = list(event = "b")),
               tar = list(start = 1L, end = NULL, anchor = "index"),
               censor = list("observation_end"))
  base[names(options)] <- options
  plan <- .ws7_scenario_plan(list(), output = list(format = "survival", options = base),
                             events = list(a = list(table = "condition_occurrence",
                                                    concept_set = list(concepts = list(.WS7$DX))),
                                           b = list(table = "condition_occurrence",
                                                    concept_set = list(concepts = list(.WS7$DX2)))))
  plan$variables <- NULL
  if (!is.null(coverage)) plan$populations$t$coverage <- coverage
  plan
}

.ws7_rows <- function(f, pid) f[as.numeric(f$pid) == pid, , drop = FALSE]

test_that("survival: composite, named and competing risks; prior outcome; observation end", {
  for (b in names(.ws7_out_backends)) {
    h <- .ws7_out_backends[[b]]$handle
    plan <- .ws7_sv_plan(list(prior_outcome = list(lookback = 365L, action = "exclude")))
    f <- .ws7_scenario_run(h, plan, .ws7_sv_frame())$frame
    expect_identical(names(f)[1:8], c("row_id", "cohort_row_id", "person_id", "outcome_name", "event",
                                      "entry_days_from_index", "exit_days_from_index", "follow_up_days"))
    expect_false(7103 %in% f$pid, info = b)                          # prior outcome: excluded
    expect_identical(as.list(.ws7_rows(f, 7101)[, c("outcome_name", "event", "exit_days_from_index")]),
                     list(outcome_name = "a", event = 1L, exit_days_from_index = 22L), info = b)
    expect_identical(as.list(.ws7_rows(f, 7102)[, c("outcome_name", "event", "exit_days_from_index")]),
                     list(outcome_name = "b", event = 1L, exit_days_from_index = 10L), info = b)
    r <- .ws7_rows(f, 7104)
    expect_identical(c(r$event, r$exit_days_from_index), c(0L, 81L), info = b)   # observation end
    expect_true(is.na(r$outcome_name), info = b)                     # composite of two: NA when censored
    r <- .ws7_rows(f, 7105)
    expect_identical(c(r$event, r$exit_days_from_index), c(0L, 50L), info = b)   # unique coverage ends 02-29
    expect_identical(.ws7_rows(f, 7106)$exit_days_from_index, 172L, info = b)
    expect_true(all(f$follow_up_days == f$exit_days_from_index - f$entry_days_from_index))
    # named: one row per outcome with its own first event
    f <- .ws7_scenario_run(h, .ws7_sv_plan(list(outcome_mode = "named")), .ws7_sv_frame())$frame
    r <- .ws7_rows(f, 7102)
    expect_identical(r$outcome_name, c("a", "b"), info = b)
    expect_identical(r$exit_days_from_index, c(31L, 10L), info = b)
    # competing risks: the first of any cause, ties by outcome priority
    f <- .ws7_scenario_run(h, .ws7_sv_plan(list(format = "competing_risk")), .ws7_sv_frame())$frame
    expect_identical(.ws7_rows(f, 7101)$outcome_name, "a", info = b)
    r <- .ws7_rows(f, 7103)                                          # no prior_outcome: eligible,
    expect_identical(c(nrow(r), r$event), c(1L, 0L), info = b)       # its only event precedes the entry
    expect_true(is.na(.ws7_rows(f, 7104)$outcome_name), info = b)
  }
})

test_that("survival: no implicit cohort-end cap (A-E12); index_end is a selectable censor; the v1 flag keeps the cap", {
  for (b in names(.ws7_out_backends)) {
    h <- .ws7_out_backends[[b]]$handle
    f <- .ws7_scenario_run(h, .ws7_sv_plan(list(outcomes = list(a = list(event = "a")))), .ws7_sv_frame())$frame
    r <- .ws7_rows(f, 7107)
    expect_identical(c(r$event, r$exit_days_from_index), c(1L, 204L), info = b)  # event after cohort end
    f <- .ws7_scenario_run(h, .ws7_sv_plan(list(outcomes = list(a = list(event = "a")),
                                                censor = list("observation_end", "index_end"))),
                           .ws7_sv_frame())$frame
    r <- .ws7_rows(f, 7107)
    expect_identical(c(r$event, r$exit_days_from_index), c(0L, 172L), info = b)
    expect_identical(.ws7_rows(f, 7106)$event, 1L, info = b)          # same day: the event wins (default)
    f <- .ws7_scenario_run(h, .ws7_sv_plan(list(outcomes = list(a = list(event = "a")),
                                                censor = list("observation_end", "index_end"),
                                                same_day_priority = "censor")), .ws7_sv_frame())$frame
    r <- .ws7_rows(f, 7106)
    expect_identical(c(r$event, r$exit_days_from_index), c(0L, 172L), info = b)
    # the lowered-v1 flag legacy_cohort_end_cap restores the mandatory cap
    plan <- .ws7_sv_plan(list(outcomes = list(a = list(event = "a"))))
    run <- .ws7_ctx(h, plan)
    run$cctx$ir$outputs$o$options$legacy_cohort_end_cap <- TRUE
    pop <- .ws7_population(run, "t", .ws7_sv_frame(), cap_end = FALSE)
    co <- .planCompileOutputs(run$cctx, list(o = "D"), populations = list(t = pop))
    f <- co$o$components$o$frame
    roster <- .reduceRosterFrame(run$cctx, pop$roster)
    f$pid <- as.numeric(roster$subject_id[match(f$cohort_row_id, roster$cohort_row_id)])
    r <- .ws7_rows(f, 7107)
    expect_identical(c(r$event, r$exit_days_from_index), c(0L, 172L), info = b)
    .ws7_close(run)
  }
})

test_that("survival: entry, admin date, source and omop_censor censoring, washout in recurrent events", {
  for (b in names(.ws7_out_backends)) {
    h <- .ws7_out_backends[[b]]$handle
    # entry later than the TAR start; prior outcome relative to the entry
    f <- .ws7_scenario_run(h, .ws7_sv_plan(list(entry = list(anchor = "index", offset = 15L),
                                                prior_outcome = list(lookback = 365L, action = "exclude"))),
                           .ws7_sv_frame())$frame
    expect_false(7102 %in% f$pid, info = b)                          # b on 01-20 precedes the entry
    expect_identical(unique(f$entry_days_from_index), 15L, info = b)
    # prior outcome "censor": a censor date before the entry makes the episode ineligible
    f <- .ws7_scenario_run(h, .ws7_sv_plan(list(prior_outcome = list(lookback = 365L, action = "censor"))),
                           .ws7_sv_frame())$frame
    expect_false(7103 %in% f$pid, info = b)
    # administrative date and a source censor
    f <- .ws7_scenario_run(h, .ws7_sv_plan(list(outcomes = list(a = list(event = "a")),
                                                censor = list("observation_end", list(admin_date = "2020-01-31")))),
                           .ws7_sv_frame())$frame
    r <- .ws7_rows(f, 7101)
    expect_identical(c(r$event, r$exit_days_from_index), c(0L, 21L), info = b)
    f <- .ws7_scenario_run(h, .ws7_sv_plan(list(outcomes = list(a = list(event = "a")),
                                                censor = list("observation_end", list(event = "b")))),
                           .ws7_sv_frame())$frame
    expect_identical(as.list(.ws7_rows(f, 7102)[, c("event", "exit_days_from_index")]),
                     list(event = 0L, exit_days_from_index = 10L), info = b)   # censored at b, 01-20
    expect_identical(.ws7_rows(f, 7101)$event, 1L, info = b)          # same day as the censor: event first
    f <- .ws7_scenario_run(h, .ws7_sv_plan(list(outcomes = list(a = list(event = "a")),
      censor = list("observation_end", list(censor = list(source = list(event = "b"),
                                                           which = "first_start_after_entry", offset = 5L,
                                                           missing = "ineligible"))))),
      .ws7_sv_frame())$frame
    expect_identical(sort(f$pid), c(7101, 7102), info = b)            # missing = ineligible drops the rest
    expect_identical(.ws7_rows(f, 7102)$exit_days_from_index, 15L, info = b)
    # recurrent events with a 30-day washout: 7101's a on 03-01 is 29 days after 02-01
    plan <- .ws7_sv_plan(list(outcomes = list(a = list(event = "a")), format = "recurrent_events",
                              event_order = "all", washout_days = 30L, complete = TRUE))
    res <- .ws7_scenario_run(h, plan, .ws7_sv_frame(), output = "o")
    ev <- res$compiled$components$events$frame
    ev$pid <- as.numeric(res$roster$subject_id[match(ev$cohort_row_id, res$roster$cohort_row_id)])
    expect_identical(nrow(ev[ev$pid == 7101 & ev$event == 1L, ]), 1L, info = b)
    expect_identical(nrow(ev[ev$pid == 7104, ]), 1L, info = b)         # one sentinel
    expect_identical(ev$event[ev$pid == 7104], 0L, info = b)
  }
})

test_that("observation gaps: the last observed day ends single-row risk; the counting process re-enters", {
  cov <- list(policy = "union", on_ambiguous = "drop", on_missing = "drop", gap = 30L)
  for (b in names(.ws7_out_backends)) {
    h <- .ws7_out_backends[[b]]$handle
    f <- .ws7_scenario_run(h, .ws7_sv_plan(list(outcomes = list(a = list(event = "a"))), coverage = cov),
                           .ws7_sv_frame(7105))$frame
    # the component spans the gap, but risk ends on 02-29; 03-05 is unobserved
    expect_identical(c(f$event, f$exit_days_from_index), c(0L, 50L), info = b)
    plan <- .ws7_sv_plan(list(outcomes = list(a = list(event = "a")), format = "counting_process",
                              event_order = "all", gap_policy = "reenter"), coverage = cov)
    f <- .ws7_scenario_run(h, plan, .ws7_sv_frame(7105))$frame
    expect_identical(names(f)[4:8], c("outcome_name", "event", "interval_number", "interval_start_days",
                                      "interval_end_days"))
    expect_identical(f$interval_start_days, c(0L, 59L, 70L), info = b)
    expect_identical(f$interval_end_days, c(50L, 70L, as.integer(as.Date("2030-12-31") - as.Date("2020-01-10"))),
                     info = b)
    expect_identical(f$event, c(0L, 1L, 0L), info = b)
    plan$outputs$o$options$gap_policy <- "censor_at_gap"
    f <- .ws7_scenario_run(h, plan, .ws7_sv_frame(7105))$frame
    expect_identical(c(nrow(f), f$interval_end_days, f$event), c(1L, 50L, 0L), info = b)
  }
})

test_that("multi-state: native risk sets through the reviewed state machine, with its transitionRef", {
  plan <- .ws7_sv_plan(list(format = "multi_state", event_order = "all", outcome_mode = "named",
                            transitions = list(list(from = "index", to = "a"), list(from = "a", to = "b"),
                                               list(from = "index", to = "b"))))
  for (b in names(.ws7_out_backends)) {
    res <- .ws7_scenario_run(.ws7_out_backends[[b]]$handle, plan, .ws7_sv_frame(c(7101, 7104)))
    expect_identical(names(res$compiled$components), c("o", "transitionRef"), info = b)
    ms <- res$frame
    expect_identical(names(ms)[4:13], c("from", "to", "trans", "Tstart", "Tstop", "time", "status",
                                        "from_name", "to_name", "state_visit_number"))
    m1 <- ms[ms$pid == 7101, ]
    # index -> a on day 22 (a beats b on the same day by hierarchy), then a -> b at risk until the end
    expect_identical(m1$to_name, c("a", "b", "b"), info = b)
    expect_identical(m1$status, c(1L, 0L, 0L), info = b)
    expect_identical(m1$Tstop[1:2], c(22, 22), info = b)
    ref <- res$compiled$components$transitionRef$frame
    expect_identical(ref$trans, 1:3)
    expect_true("transitionRef" %in% res$compiled$reference_components)
  }
})

test_that("long: projections, named anchors, relations and broadcast reducers; the event log", {
  vars <- list(n_lab = list(source = list(event = "lab"), reducer = list(fn = "count"), broadcast = TRUE))
  plan <- .ws7_scenario_plan(vars, output = list(format = "long", sources = list(list(event = "lab")),
    columns = list("n_lab"),
    options = list(columns_derived = list("days_from_index", "event_number", "days_since_previous",
                                          "duration_days", "value", "concept_name", "days_from_anchor"),
                   anchors = list(first_dx = list(source = list(event = "dx"), which = "first")),
                   relations = list(next_lab = list(source = list(event = "lab"), within = list(0L, 10L))))))
  for (b in names(.ws7_out_backends)) {
    f <- .ws7_scenario_run(.ws7_out_backends[[b]]$handle, plan, .ws7_scenario_frame(c(7001, 7002)))$frame
    expect_identical(names(f)[1:6], c("row_id", "cohort_row_id", "person_id", "concept_id", "start_date",
                                      "end_date"))
    r <- f[f$pid == 7001, ]
    expect_identical(nrow(r), 5L, info = b)
    expect_identical(r$start_date, c(-9L, -5L, -5L, 0L, 10L), info = b)   # relative date handling
    expect_identical(r$days_from_index, r$start_date, info = b)
    expect_identical(r$event_number, 1:5, info = b)
    expect_identical(r$days_since_previous, c(NA, 4L, 0L, 5L, 10L), info = b)
    expect_identical(r$duration_days, rep(0L, 5L), info = b)           # a point table: end = start
    expect_equal(r$value, c(5, NA, 7, 3, 9), info = b)
    expect_identical(unique(r$concept_name), "ws7 lab", info = b)
    expect_identical(r$days_from_first_dx, c(-1L, 3L, 3L, 8L, 18L), info = b)
    expect_identical(r$days_to_next_lab, c(4L, 0L, 0L, 10L, NA), info = b)   # self excluded, same-day kept
    expect_identical(unique(r$n_lab), 5L, info = b)
    expect_false(7002 %in% f$pid, info = b)
  }
  plan$outputs$o$sources <- list(list(event = "lab"), list(event = "dx"))
  plan$outputs$o$options <- list(event_log = TRUE)
  plan$outputs$o$columns <- NULL
  plan$variables <- NULL
  f <- .ws7_scenario_run(.ws7_out_backends$sqlite$handle, plan, .ws7_scenario_frame(7001))$frame
  expect_identical(names(f), c("row_id", "cohort_row_id", "person_id", "source", "concept_id", "start_days",
                               "end_days", "value", "unit_concept_id", "event_number", "pid"))
  expect_identical(nrow(f), 10L)
  expect_identical(f$event_number, 1:10)
  # several sources without the event log are the v1 split shape: refused
  plan$outputs$o$options <- NULL
  .ws4_expect_rejection(.ws7_scenario_run(.ws7_out_backends$sqlite$handle, plan, .ws7_scenario_frame(7001)),
                        pattern = "event_log")
})

test_that("intervals: point tables (A-16), match modes and selections", {
  base <- list(format = "intervals", options = list(sources = list(list(event = "drug"), list(event = "lab")),
                                                     window = list(start = -5L, end = 5L, anchor = "index")))
  plan <- .ws7_scenario_plan(list(), output = base)
  plan$variables <- NULL
  for (b in names(.ws7_out_backends)) {
    f <- .ws7_scenario_run(.ws7_out_backends[[b]]$handle, plan, .ws7_scenario_frame(7001))$frame
    expect_identical(names(f)[1:7], c("row_id", "cohort_row_id", "person_id", "interval_type", "concept_id",
                                      "start_days_from_index", "end_days_from_index"))
    expect_identical(f$interval_type, c("drug", "drug", "lab", "lab", "lab"), info = b)
    lab <- f[f$interval_type == "lab", ]
    expect_identical(lab$start_days_from_index, lab$end_days_from_index, info = b)   # end = start
  }
  plan$outputs$o$options$interval_match <- "starts_in"
  plan$outputs$o$options$event_select <- "last"
  f <- .ws7_scenario_run(.ws7_out_backends$sqlite$handle, plan, .ws7_scenario_frame(7001))$frame
  expect_identical(f$start_days_from_index, c(-5L, 0L))
  plan$outputs$o$options$event_select <- "nearest"
  plan$outputs$o$options$select_n <- 2L
  f <- .ws7_scenario_run(.ws7_out_backends$sqlite$handle, plan, .ws7_scenario_frame(7001))$frame
  expect_identical(f$start_days_from_index[f$interval_type == "lab"], c(-5L, 0L))
})

test_that("temporal covariates, sparse and the per-plan covariate registry", {
  plan <- .ws7_scenario_plan(list(), output = list(format = "temporal_covariates", options = list(
    sources = list(list(event = "dx"), list(event = "lab")), bin_width = 5L,
    window = list(start = -10L, end = 9L, anchor = "index"), analyses = list("count", "mean"))))
  plan$variables <- NULL
  for (b in names(.ws7_out_backends)) {
    res <- .ws7_scenario_run(.ws7_out_backends[[b]]$handle, plan, .ws7_scenario_frame(c(7001, 7002)))
    comps <- res$compiled$components
    expect_identical(names(comps), c("o", "covariateRef", "timeRef"), info = b)
    ref <- comps$covariateRef$frame
    # dx: two identities x count (mean has no value role on conditions); lab: count, mean
    expect_identical(ref$source, c("dx", "dx", "lab", "lab"), info = b)
    expect_identical(ref$analysis, c("count", "count", "count", "mean"), info = b)
    expect_identical(ref$covariate_id, 1:4, info = b)
    expect_identical(comps$timeRef$frame$start_day, c(-10L, -5L, 0L, 5L), info = b)
    cv <- comps$o$frame
    expect_false(any(cv$covariate_value == 0))
    lab_count <- cv[cv$covariate_id == 3L, ]
    expect_identical(lab_count$time_id, c(1L, 2L, 3L), info = b)        # 01-01; 01-05 x2; 01-10
    expect_identical(lab_count$covariate_value, c(1, 2, 1), info = b)
  }
  plan <- .ws7_scenario_plan(list(), output = list(format = "sparse", options = list(
    sources = list(list(event = "dx")), analyses = list("binary", "count"))))
  plan$variables <- NULL
  res <- .ws7_scenario_run(.ws7_out_backends$sqlite$handle, plan, .ws7_scenario_frame(c(7001, 7002)))
  cv <- res$compiled$components$o$frame
  expect_identical(cv$covariate_value, c(1, 1, 4, 1))                 # binary DX, DX2; count DX 4, DX2 1
  expect_identical(nrow(res$compiled$components$covariateRef$frame), 4L)
})

test_that("membership, dictionary, joined and date handling", {
  vars <- list(n = list(source = list(event = "lab"), reducer = list(fn = "count")),
               dd = list(raw = list(table = "observation_period", column = "observation_period_start_date")))
  plan <- .ws7_scenario_plan(vars, output = list(columns = list("n")))
  plan$outputs$m <- list(rows = "t", format = "membership")
  plan$outputs$w2 <- list(rows = "t", format = "wide", grain = "episode", columns = list("dd"))
  plan$outputs$j <- list(rows = "t", format = "joined", grain = "episode",
                         options = list(from = list(list(output = "o"), list(output = "w2"))))
  plan$outputs$d <- list(rows = "t", format = "dictionary")
  plan$publish <- list("m", "j", "d")
  for (b in names(.ws7_out_backends)) {
    h <- .ws7_out_backends[[b]]$handle
    run <- .ws7_ctx(h, plan)
    pop <- .ws7_population(run, "t", .ws7_scenario_frame(c(7001, 7002, 7003)), cap_end = FALSE)
    co <- .planCompileOutputs(run$cctx, list(m = "M", j = "J", d = "DICT"), populations = list(t = pop))
    expect_identical(names(co), c("m", "j", "d"), info = b)               # private inputs not roots
    m <- co$m$components$m$frame
    expect_identical(names(m), c("row_id", "cohort_row_id", "person_id", "cohort_start_date",
                                 "cohort_end_date", "index_source"))
    expect_identical(unique(m$cohort_start_date), 0L, info = b)          # relative to the index
    j <- co$j$components$j$frame
    expect_identical(names(j), c("row_id", "cohort_row_id", "person_id", "n", "dd"), info = b)
    expect_identical(unique(j$dd), as.integer(as.Date("2015-01-01") - as.Date("2020-01-10")), info = b)
    d <- co$d$components$d$frame
    expect_identical(names(d), c("concept_id", "concept_name", "domain_id", "vocabulary_id",
                                 "concept_class_id", "source"))
    expect_identical(d$concept_id, .WS7$LAB, info = b)                    # the events the outputs read
    expect_identical(d$source, "lab", info = b)
    expect_identical(co$d$reference_components, "d")
    .ws7_close(run)
  }
  # date handling: remove drops the raw date; binned truncates; absolute needs the server option
  for (mode in c("remove", "binned", "absolute")) {
    p <- .ws7_scenario_plan(vars["dd"], output = list(columns = list("dd"), options = list(
      date_handling = if (mode == "binned") list(mode = mode, bin_width = "year") else list(mode = mode))))
    if (mode == "absolute") {
      .ws4_expect_rejection(.ws7_scenario_run(.ws7_out_backends$sqlite$handle, p, .ws7_scenario_frame(7001)),
                            pattern = "absolute")
      next
    }
    f <- .ws7_scenario_run(.ws7_out_backends$sqlite$handle, p, .ws7_scenario_frame(7001))$frame
    if (mode == "remove") expect_false("dd" %in% names(f)) else expect_identical(f$dd, "2015-01-01")
  }
})

test_that("joined: cardinality failures are typed join_cardinality refusals naming the output", {
  vars <- list(n = list(source = list(event = "lab"), reducer = list(fn = "count")))
  plan <- .ws7_scenario_plan(vars, output = list(columns = list("n")))
  plan$outputs$l <- list(rows = "t", format = "long", sources = list(list(event = "lab")))
  plan$outputs$j <- list(rows = "t", format = "joined", grain = "episode",
                         options = list(from = list(list(output = "l"), list(output = "o")),
                                        cardinality = "one_to_one"))
  plan$publish <- list("j")
  h <- .ws7_out_backends$sqlite$handle
  run <- .ws7_ctx(h, plan)
  on.exit(.ws7_close(run), add = TRUE)
  pop <- .ws7_population(run, "t", .ws7_scenario_frame(c(7001, 7002)), cap_end = FALSE)
  err <- tryCatch(.planCompileOutputs(run$cctx, list(j = "J"), populations = list(t = pop)),
                  dsomop_plan_rejection = function(e) e)
  expect_identical(err$code, "join_cardinality")
  expect_identical(err$output, "j")
  # many_to_one accepts the repeated left keys
  run$cctx$ir$outputs$j$options$cardinality <- "many_to_one"
  co <- .planCompileOutputs(run$cctx, list(j = "J"), populations = list(t = pop))
  expect_identical(nrow(co$j$components$j$frame), 5L)
})
