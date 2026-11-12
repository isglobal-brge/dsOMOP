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

test_that("survival: v1 shapes run on the v1 executors; an unavailable censor source; explain states the censoring (A-14)", {
  h <- .ws7_out_backends$sqlite$handle
  plan <- .ws7_sv_plan(list(outcomes = list(a = list(event = "a"))))
  for (flag in list(list(legacy_single_outcome = TRUE), list(legacy_state_step = 0.01),
                    list(legacy_states = list("a")))) {
    run <- .ws7_ctx(h, plan)
    run$cctx$ir$outputs$o$options[names(flag)] <- flag
    pop <- .ws7_population(run, "t", .ws7_sv_frame(), cap_end = FALSE)
    err <- tryCatch(.planCompileOutputs(run$cctx, list(o = "D"), populations = list(t = pop)),
                    dsomop_plan_rejection = function(e) e)
    expect_s3_class(err, "dsomop_plan_rejection")
    expect_match(conditionMessage(err), paste0(names(flag), ", which runs on the v1 executors"), fixed = TRUE)
    .ws7_close(run)
  }
  # a censor source on an absent table cannot be evaluated: the output is
  # unavailable, its rows those without that censor
  plain <- .ws7_scenario_run(h, plan, .ws7_sv_frame())
  with_notes <- .ws7_sv_plan(list(outcomes = list(a = list(event = "a")),
                                  censor = list("observation_end", list(event = "notes"))))
  with_notes$events$notes <- list(table = "note", on_missing_table = "unavailable")
  res <- .ws7_scenario_run(h, with_notes, .ws7_sv_frame())
  expect_identical(res$compiled$status, "unavailable")
  expect_identical(res$frame, plain$frame)
  # the effective censoring: defaults applied, no implicit cap natively
  rich <- .ws7_sv_plan(list(
    outcomes = list(a = list(event = "a")),
    censor = list("observation_end", "death", list(admin_date = "2020-06-01"), list(event = "b"),
                  list(censor = list(source = list(event = "b"), which = "containing_entry_end",
                                     missing = "ineligible"))),
    prior_outcome = list(lookback = 30L, action = "censor")))
  ir <- .irNormalize(.irJsonForm(rich))
  d <- .irEffectiveDefaults(ir)$outputs$o$effective_censoring
  expect_identical(vapply(d$entries, function(e) e$kind, ""),
                   c("observation_end", "death", "admin_date", "source", "omop_censor"))
  expect_identical(d$entries[[3]]$date, "2020-06-01")
  expect_identical(d$entries[[4]][c("which", "offset")], list(which = "first_start_after_entry", offset = 0L))
  expect_identical(d$entries[[5]][c("which", "offset", "missing")],
                   list(which = "containing_entry_end", offset = 0L, missing = "ineligible"))
  expect_false(d$cohort_end_cap)
  expect_identical(d$observation, "last_observed_day_before_first_gap")
  expect_identical(d$prior_outcome, list(action = "censor", lookback = 30L))
  expect_identical(d$same_day_priority, "event")
  reenter <- .ws7_sv_plan(list(outcomes = list(a = list(event = "a")), format = "counting_process",
                               gap_policy = "reenter"))
  expect_identical(.irEffectiveDefaults(.irNormalize(.irJsonForm(reenter)))$outputs$o$effective_censoring$observation,
                   "reenter_at_next_observed_segment")
  # a lowered v1 plan: the v1 executor's censoring, with its cohort-end cap
  # (death opt-in for the historical single-outcome shape, A-14)
  v1 <- .irNormalize(.irJsonForm(list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    outputs = list(tte = list(type = "survival",
                              outcome = list(table = "condition_occurrence", concept_set = .WS7$DX))))))
  d <- .irEffectiveDefaults(v1)$outputs[[1L]]$effective_censoring
  expect_identical(vapply(d$entries, function(e) e$kind, ""), c("observation_end", "index_end"))
  expect_true(d$entries[[2L]]$implicit)
  expect_true(d$cohort_end_cap)
  expect_identical(d$observation, "containing_period_end")
})

test_that("explain states the coverage policy of covered_days and supply_days (A-20 (g))", {
  W <- .WS7
  vars <- list(
    cd = list(source = list(event = "drug"), reducer = list(fn = "covered_days")),
    cs = list(source = list(event = "drug_start"), reducer = list(fn = "covered_days", observed_only = TRUE)),
    sd = list(source = list(event = "drug_supply"), reducer = list(fn = "supply_days", fills = "clipped")),
    n = list(source = list(event = "drug"), reducer = list(fn = "count")))
  plan <- .ws7_scenario_plan(vars, output = list(columns = list("cd", "cs", "sd", "n")),
                             events = list(
                               drug_start = list(table = "drug_exposure", end = "start",
                                                 concept_set = list(concepts = list(W$DRUG))),
                               drug_supply = list(table = "drug_exposure", end = list(fallback = "days_supply"),
                                                  concept_set = list(concepts = list(W$DRUG)))))
  red <- .irEffectiveDefaults(.irNormalize(.irJsonForm(plan)))$reducers
  expect_null(red$n$coverage_policy)
  expect_identical(red$cd$coverage_policy[c("relation", "end_policy", "quality")],
                   list(relation = "recorded", end_policy = "recorded_end",
                        quality = c("start_present", "end_not_before_start")))
  expect_false(red$cd$coverage_policy$observed_only)
  expect_identical(red$cs$coverage_policy$quality, "start_present")
  expect_true(red$cs$coverage_policy$observed_only)
  expect_identical(red$sd$coverage_policy[c("fills", "end_policy", "quality")],
                   list(fills = "clipped", end_policy = "recorded_end_then_days_supply",
                        quality = c("start_present", "end_not_before_start",
                                    "supply_positive_integer_when_only_end_source")))
  # an interval source reads its own normalised relation
  ivp <- .ws7_scenario_plan(list(cv = list(source = list(interval = "rx"),
                                           reducer = list(fn = "covered_days"))),
                            output = list(columns = list("cv")))
  ivp$intervals <- list(rx = list(source = list(event = "drug"), gap = 0L))
  red <- .irEffectiveDefaults(.irNormalize(.irJsonForm(ivp)))$reducers
  expect_identical(red$cv$coverage_policy$relation, "recorded")
  expect_null(red$cv$coverage_policy$end_policy)
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

test_that("covariate identities are shared across outputs regardless of source order", {
  plan <- .ws7_scenario_plan(list(), output = list(format = "sparse", options = list(
    sources = list(list(event = "lab")), analyses = list("count"))))
  plan$variables <- NULL
  plan$outputs$second <- list(rows = "t", format = "sparse", grain = "episode", options = list(
    sources = list(list(event = "dx"), list(event = "lab")), analyses = list("count", "binary")))
  for (b in names(.ws7_out_backends)) {
    run <- .ws7_ctx(.ws7_out_backends[[b]]$handle, plan)
    pop <- .ws7_population(run, "t", .ws7_scenario_frame(c(7001, 7002)), cap_end = FALSE)
    co <- .planCompileOutputs(run$cctx, populations = list(t = pop))
    one <- co$o$components$covariateRef$frame
    two <- co$second$components$covariateRef$frame
    shared <- two$source == "lab" & two$analysis == "count"
    expect_identical(two$covariate_id[shared], one$covariate_id, info = b)
    expect_identical(sort(unique(c(one$covariate_id, two$covariate_id))), 1:6, info = b)
    expect_false(anyDuplicated(two[c("source", "concept_id", "analysis")]) > 0L, info = b)
    .ws7_close(run)
  }
})

test_that("dictionary source outputs remain static dependencies through public execution", {
  plan <- .ws7_scenario_plan(list(n = list(source = list(event = "lab"), reducer = list(fn = "count"))),
                              output = list(columns = list("n")))
  plan$outputs$d <- list(rows = "t", format = "dictionary", options = list(source_outputs = list("o")))
  plan$publish <- list("d")
  ir <- .irNormalize(.irJsonForm(plan))
  expect_identical(.irOutputDeps(ir, "d"), "o")
  expect_identical(names(.irOutputClosure(ir, "d")$events), "lab")
  invalid <- plan
  invalid$outputs$d$options$source_outputs <- list("missing_output")
  expect_error(.irPrepare(invalid), "dangling output 'missing_output'", fixed = TRUE)
  testthat::local_mocked_bindings(
    .outputPopulation = function(...) stop("dictionary materialised a population"),
    .compileEventByName = function(...) stop("dictionary compiled a clinical event"))
  for (b in names(.ws7_out_backends)) {
    h <- .ws7_out_backends[[b]]$handle
    .ws7_keys(list(h))
    sym <- paste0("ws7_dictionary_", b)
    .setHandle(sym, h)
    for (mode in c("memory", "staged")) {
      target <- new.env(parent = emptyenv())
      status <- withr::with_options(list(nfilter.subset = 5L),
        .omopPlanExecuteDS(sym, plan, list(d = "DICT"), output_mode = mode, target_env = target))
      expect_identical(status$status, "ok", info = paste(b, mode))
      expect_identical(status$published$d$d$grain, "none", info = paste(b, mode))
      frame <- get(status$published$d$d$symbol, target)
      if (identical(mode, "staged")) {
        chunks <- list()
        .publishReadChunks(frame, function(x) chunks[[length(chunks) + 1L]] <<- x)
        frame <- do.call(rbind, chunks)
      }
      expect_identical(as.integer(frame$concept_id), .WS7$LAB, info = paste(b, mode))
      expect_identical(as.character(frame$source), "lab", info = paste(b, mode))
    }
    rm(list = paste0("handle_", sym), envir = .dsomop_env)
  }
})

test_that("dictionary-only sparse dependencies do not enter source preflight or the live registry", {
  plan <- .ws7_scenario_plan(list(), output = list(format = "sparse", options = list(
    sources = list(list(event = "lab")), analyses = list("count"))))
  plan$variables <- NULL
  plan$outputs <- c(list(metadata = list(rows = "t", format = "sparse", options = list(
    sources = list(list(event = "dx")), analyses = list("count")))), plan$outputs)
  plan$outputs$d <- list(rows = "t", format = "dictionary", options = list(source_outputs = list("metadata")))
  plan$publish <- list("o", "d")
  compile_event <- .compileEventByName
  testthat::local_mocked_bindings(.compileEventByName = function(ctx, name, ...) {
    if (identical(name, "dx")) stop("dictionary-only source was compiled")
    compile_event(ctx, name, ...)
  })
  run <- .ws7_ctx(.ws7_out_backends$sqlite$handle, plan)
  on.exit(.ws7_close(run), add = TRUE)
  expect_identical(.planCompileSources(run$cctx, c("o", "d")), "lab")
  pop <- .ws7_population(run, "t", .ws7_scenario_frame(7001), cap_end = FALSE)
  co <- .planCompileOutputs(run$cctx, populations = list(t = pop))
  expect_identical(names(co), c("o", "d"))
  expect_identical(co$o$components$covariateRef$frame$covariate_id, 1L)
  expect_identical(co$o$components$covariateRef$frame$source, "lab")
  expect_identical(co$d$components$d$frame$concept_id, c(.WS7$DX, .WS7$DX2))
  expect_identical(co$d$components$d$frame$source, c("dx", "dx"))
})

test_that("sparse and temporal output names cannot collide with reference companions", {
  h <- .ws7_out_backends$sqlite$handle
  .ws7_keys(list(h))
  for (case in list(c("sparse", "covariateRef"), c("temporal_covariates", "covariateRef"),
                    c("temporal_covariates", "timeRef"))) {
    fmt <- case[[1L]]
    root <- case[[2L]]
    opts <- list(sources = list(list(event = "lab")), analyses = list("count"))
    if (identical(fmt, "temporal_covariates")) {
      opts$bin_width <- 5L
      opts$window <- list(start = -10L, end = 9L, anchor = "index")
    }
    plan <- .ws7_scenario_plan(list(), output = list(format = fmt, options = opts))
    plan$variables <- NULL
    names(plan$outputs) <- root
    expected_refs <- if (identical(fmt, "sparse")) "covariateRef" else c("covariateRef", "timeRef")
    frames <- list()
    inventories <- list()
    for (mode in c("memory", "staged")) {
      run <- .ws7_ctx(h, plan, mode = mode)
      pop <- .ws7_population(run, "t", .ws7_scenario_frame(c(7001, 7003, 7004, 7005, 7006, 7007)),
                             cap_end = FALSE)
      res <- .ws7_publish(run, list(t = pop), stats::setNames(list("D_collision"), root), threshold = 5L)
      expect_identical(res$status$status, "ok", info = paste(fmt, root, mode))
      co <- res$compiled[[root]]
      inventory <- res$status$published[[root]]
      expect_identical(names(inventory), names(co$components))
      expect_identical(anyDuplicated(names(inventory)), 0L)
      expect_true(all(grepl(.NAME_PATTERN, names(inventory))))
      expect_identical(inventory[[root]]$symbol, "D_collision")
      expect_identical(inventory[[root]]$grain, "episode")
      expect_identical(length(co$reference_components), length(expected_refs))
      expect_false(root %in% co$reference_components)
      expect_true(all(setdiff(expected_refs, root) %in% co$reference_components))
      expect_true(all(vapply(inventory[co$reference_components], function(x) identical(x$grain, "none"), logical(1))))
      if (identical(mode, "staged")) {
        primary <- get(inventory[[root]]$symbol, run$ctx$target_env)
        expect_null(primary$metadata$semantic_contract$component)
        expect_identical(primary$metadata$semantic_contract$grain, "episode")
        for (ref in co$reference_components) {
          descriptor <- get(inventory[[ref]]$symbol, run$ctx$target_env)
          expect_identical(descriptor$metadata$semantic_contract$component, ref)
          expect_identical(descriptor$metadata$semantic_contract$grain, "none")
        }
      }
      inventories[[mode]] <- names(inventory)
      frames[[mode]] <- lapply(res$values[[root]], function(frame) lapply(frame, as.character))
      .ws7_close(run)
    }
    expect_identical(inventories$memory, inventories$staged)
    expect_identical(frames$memory, frames$staged)
  }
})

test_that("empty long and person sparse sources retain their typed empty schemas", {
  for (b in names(.ws7_out_backends)) {
    for (fmt in c("long", "sparse")) {
      plan <- .ws7_scenario_plan(list(), output = list(format = fmt,
        grain = if (fmt == "sparse") "person" else "episode", sources = list(list(event = "lab")),
        options = if (fmt == "sparse") list(sources = list(list(event = "lab")), analyses = list("binary")) else
          list(columns_derived = list("value"))))
      plan$variables <- NULL
      res <- .ws7_scenario_run(.ws7_out_backends[[b]]$handle, plan, .ws7_scenario_frame(7002))
      expect_identical(nrow(res$compiled$components$o$frame), 0L, info = paste(b, fmt))
      expect_identical(res$compiled$status, "ok", info = paste(b, fmt))
      expect_true("person_id" %in% names(res$compiled$components$o$frame), info = paste(b, fmt))
    }
  }
})

test_that("interval ends_in includes a record starting before the requested window", {
  plan <- .ws7_scenario_plan(list(), output = list(format = "intervals", options = list(
    sources = list(list(event = "drug")), window = list(start = 0L, end = 0L, anchor = "index"),
    interval_match = "ends_in")))
  plan$variables <- NULL
  for (b in names(.ws7_out_backends)) {
    f <- .ws7_scenario_run(.ws7_out_backends[[b]]$handle, plan, .ws7_scenario_frame(7001))$frame
    expect_identical(f$start_days_from_index, -9L, info = b)
    expect_identical(f$end_days_from_index, 0L, info = b)
  }
})

test_that("long applies date handling to broadcast dates and refuses column collisions", {
  vars <- list(op_date = list(raw = list(table = "observation_period",
                                        column = "observation_period_start_date")))
  plan <- .ws7_scenario_plan(vars, output = list(format = "long", sources = list(list(event = "lab")),
                                                columns = list("op_date")))
  for (b in names(.ws7_out_backends)) {
    f <- .ws7_scenario_run(.ws7_out_backends[[b]]$handle, plan, .ws7_scenario_frame(7001))$frame
    expect_identical(unique(f$op_date), as.integer(as.Date("2015-01-01") - as.Date("2020-01-10")), info = b)
    p <- plan
    p$outputs$o$options <- list(date_handling = list(mode = "remove"))
    f <- .ws7_scenario_run(.ws7_out_backends[[b]]$handle, p, .ws7_scenario_frame(7001))$frame
    expect_false("op_date" %in% names(f), info = b)
  }
  plan$variables <- list(concept_id = list(source = list(event = "lab"),
                                          reducer = list(fn = "count"), broadcast = TRUE))
  plan$outputs$o$columns <- list("concept_id")
  .ws4_expect_rejection(.ws7_scenario_run(.ws7_out_backends$sqlite$handle, plan, .ws7_scenario_frame(7001)),
                        pattern = "duplicates an output column")
})

test_that("panels seed both window edges and event boundaries add only the event date", {
  plan <- .ws7_scenario_plan(list(), output = list(format = "person_period", options = list(
    sources = list(list(event = "lab")), bins = list(width = 100L),
    window = list(start = 0L, end = 20L, anchor = "index"),
    partition = list(boundaries = list(list(event = "lab"))), analyses = list("mean"))))
  plan$variables <- NULL
  for (b in names(.ws7_out_backends)) {
    f <- .ws7_scenario_run(.ws7_out_backends[[b]]$handle, plan, .ws7_scenario_frame(c(7001, 7002)))$frame
    r <- .ws7_rows(f, 7001)
    expect_identical(r$period_start_days, c(0L, 10L), info = b)
    expect_identical(r$period_end_days, c(9L, 20L), info = b)
    expect_identical(r$days, c(10L, 11L), info = b)
    expect_identical(r$lab_mean, c(3, 9), info = b)
    empty <- .ws7_rows(f, 7002)
    expect_identical(empty$days, 21L, info = b)
    expect_true(is.na(empty$lab_mean), info = b)
    sparse <- plan
    sparse$outputs$o$options$representation <- "sparse"
    sf <- .ws7_scenario_run(.ws7_out_backends[[b]]$handle, sparse, .ws7_scenario_frame(c(7001, 7002)))$frame
    expect_identical(sf$period_start_days, c(0L, 10L), info = b)
    expect_identical(sf$lab_mean, c(3, 9), info = b)
    expect_identical(unique(sf$pid), 7001, info = b)
  }
})

test_that("sparse panels retain exposure-only periods and dense unavailable columns remain NA", {
  plan <- .ws7_scenario_plan(list(), output = list(format = "person_period", options = list(
    representation = "sparse", bins = list(width = 5L), window = list(start = 0L, end = 9L, anchor = "index"),
    exposures = list(rx = list(interval = "rx")))))
  plan$variables <- NULL
  plan$intervals <- list(rx = list(source = list(event = "drug"), gap = 0L))
  for (b in names(.ws7_out_backends)) {
    f <- .ws7_scenario_run(.ws7_out_backends[[b]]$handle, plan, .ws7_scenario_frame(c(7001, 7002)))$frame
    expect_identical(f$exposed_rx, c(5L, 1L), info = b)
    expect_identical(f$active_rx, c(1L, 1L), info = b)
    expect_identical(unique(f$pid), 7001, info = b)
  }
  plan$events$notes <- list(table = "note", concept_set = list(concepts = list(.WS7$LAB)),
                            on_missing_table = "unavailable")
  plan$outputs$o$options <- list(sources = list(list(event = "notes")), bins = list(width = 5L),
    window = list(start = 0L, end = 9L, anchor = "index"), analyses = list("binary", "count", "mean", "last"))
  res <- .ws7_scenario_run(.ws7_out_backends$sqlite$handle, plan, .ws7_scenario_frame(7002))
  expect_identical(nrow(res$frame), 2L)
  expect_identical(res$compiled$status, "unavailable")
  expect_true(all(is.na(res$frame[c("notes_binary", "notes_count")])))
  expect_false(any(c("notes_mean", "notes_last") %in% names(res$frame)))
})

test_that("temporal exposed days exclude tolerated observation gaps", {
  plan <- .ws7_scenario_plan(list(), output = list(format = "temporal_covariates", options = list(
    sources = list(list(interval = "rx")), bin_width = 41L,
    window = list(start = 50L, end = 90L, anchor = "index"), analyses = list("exposed_days"))))
  plan$variables <- NULL
  plan$intervals <- list(rx = list(source = list(event = "dx"), gap = 30L, persistence = 20L))
  plan$populations$t$coverage <- list(policy = "union", on_missing = "drop", on_ambiguous = "drop", gap = 30L)
  for (b in names(.ws7_out_backends)) {
    f <- .ws7_scenario_run(.ws7_out_backends[[b]]$handle, plan, .ws7_scenario_frame(7105))$frame
    # The course is March 5..April 9; March 5..9 are unobserved.
    expect_identical(f$covariate_value, 31, info = b)
  }
})

test_that("output row caps apply before constructing large period and reference grids", {
  withr::local_options(list(dsomop.max_memory_rows = 10L))
  plan <- .ws7_scenario_plan(list(), output = list(format = "temporal_covariates", options = list(
    sources = list(list(event = "lab")), bin_width = 1L,
    window = list(start = 0L, end = 1000000L, anchor = "index"))))
  plan$variables <- NULL
  err <- tryCatch(.ws7_scenario_run(.ws7_out_backends$sqlite$handle, plan, .ws7_scenario_frame(7002)),
                   dsomop_plan_rejection = function(e) e)
  expect_s3_class(err, "dsomop_plan_rejection")
  expect_identical(err$code, "validation_error")
  expect_identical(err$reason, "reducer_row_cap")
  err <- tryCatch(.panelPeriods(as.Date("2000-01-01"), as.Date("2030-01-01"),
    data.frame(start = as.Date("2000-01-01"), end = as.Date("2030-01-01")),
    list(width = 1L), list(), as.Date(character(0))), dsomop_plan_rejection = function(e) e)
  expect_identical(err$reason, "reducer_row_cap")
})

test_that("survival tie keys remain distinct beyond the exact-double range", {
  ev <- list(a = data.frame(row_key = "1", start_date = as.Date(c("2020-01-10", "2020-01-10")),
                            source_ns = "event:a", event_id = c("9007199254740993", "9007199254740992")))
  got <- .survivalEpisodeEvents(ev, "a", "1")
  expect_identical(got$key, c("9007199254740992", "9007199254740993"))
})

test_that("joined inputs must retain the consumer roster namespace", {
  plan <- .ws7_scenario_plan(list(n = list(source = list(event = "lab"), reducer = list(fn = "count"))),
                             output = list(columns = list("n")))
  plan$outputs$other <- plan$outputs$o
  plan$outputs$j <- list(rows = "t", format = "joined", grain = "episode", options = list(
    from = list(list(output = "o"), list(output = "other"))))
  plan$publish <- list("o", "other")
  run <- .ws7_ctx(.ws7_out_backends$sqlite$handle, plan)
  on.exit(.ws7_close(run), add = TRUE)
  pop <- .ws7_population(run, "t", .ws7_scenario_frame(7001), cap_end = FALSE)
  co <- .planCompileOutputs(run$cctx, populations = list(t = pop))
  co$o$components$o$fragment$roster_namespace <- "another_population"
  node <- .irNamedNode(run$cctx$ir, "output", "j")
  .ws4_expect_rejection(.compileOutput(run$cctx, node, list(population = pop, outputs = co,
                                                          symbol = "J", mode = "memory")),
                        pattern = "different grain or roster namespace")
})

test_that("unavailable sources retain the declared value-role schema", {
  plan <- .ws7_scenario_plan(list(), output = list(format = "person_period", options = list(
    sources = list(list(event = "dx"), list(event = "lab")), bins = list(width = 20L),
    window = list(start = -10L, end = 9L, anchor = "index"), analyses = list("count", "mean", "last"))))
  plan$variables <- NULL
  plan$events$lab$value <- NULL
  h <- .ws7_out_backends$sqlite$handle
  expected <- .ws7_scenario_run(h, plan, .ws7_scenario_frame(7001))$compiled$components$o$frame
  run <- .ws7_ctx(h, plan)
  on.exit(.ws7_close(run), add = TRUE)
  # Unavailable fragments deliberately have no physically available roles.
  .eventUnavailable(run$cctx, "dx", "condition_occurrence")
  .eventUnavailable(run$cctx, "lab", "measurement")
  pop <- .ws7_population(run, "t", .ws7_scenario_frame(7001), cap_end = FALSE)
  actual <- .planCompileOutputs(run$cctx, populations = list(t = pop))$o
  expect_identical(names(actual$components$o$frame), names(expected))
  expect_identical(actual$status, "unavailable")
  features <- c(paste0("dx_", c(.WS7$DX, .WS7$DX2), "_count"), "lab_count", "lab_mean", "lab_last")
  expect_true(all(is.na(actual$components$o$frame[features])))
})

test_that("named TAR components survive maximal names and generated-name collisions", {
  tar_a <- paste(rep("a", 64L), collapse = "")
  tar_b <- paste0(substr(tar_a, 1L, 63L), "b")
  tars <- stats::setNames(rep(list(list(start = 1L, end = 90L, anchor = "index")), 2L), c(tar_a, tar_b))
  plan <- .ws7_sv_plan(list(format = "recurrent_events", event_order = "all", complete = TRUE, tars = tars))
  plan$outputs$o$options$tar <- NULL
  res <- .ws7_scenario_run(.ws7_out_backends$sqlite$handle, plan, .ws7_sv_frame(c(7101, 7104)))$compiled
  expect_identical(length(res$components), 4L)
  expect_true(all(nchar(names(res$components)) <= 64L))
  expect_identical(anyDuplicated(names(res$components)), 0L)
  expect_true(all(vapply(res$components, function(p) nrow(p$frame) >= 2L, logical(1))))
  # A hash collision must not overwrite a sibling component.
  testthat::local_mocked_bindings(.fragmentHash = function(key) "0000000000")
  a <- .survivalFamily("recurrent_events", plan$outputs$o$options, tar_a, "o", TRUE)
  b <- .survivalFamily("recurrent_events", plan$outputs$o$options, tar_b, "o", TRUE)
  allocated <- unlist(c(a, b), use.names = FALSE)
  expect_identical(anyDuplicated(allocated), 0L)
  expect_true(all(nchar(allocated) <= 64L))
  expect_identical(a, .survivalFamily("recurrent_events", plan$outputs$o$options, tar_a, "o", TRUE))
  # User TAR names are preserved; a colliding generated reference is renamed.
  opts <- list(tars = list(transitionRef = list(start = 1L, end = 90L)))
  family <- .survivalFamily("multi_state", opts, "transitionRef", "o", TRUE)
  expect_identical(family$msdata, "transitionRef")
  expect_false(identical(family$transition_ref, "transitionRef"))
})

test_that("a single named TAR keeps its declared component name", {
  plan <- .ws7_sv_plan(list(tars = list(short = list(start = 1L, end = 30L, anchor = "index"))))
  plan$outputs$o$options$tar <- NULL
  res <- .ws7_scenario_run(.ws7_out_backends$sqlite$handle, plan, .ws7_sv_frame(c(7101, 7104)))$compiled
  expect_identical(names(res$components), "short")
  expect_identical(res$publication_candidates[[1L]]$requested_symbol, "D.short")
})

test_that("person-event long output unions all associated episodes before physical deduplication", {
  plan <- .ws7_scenario_plan(list(), output = list(format = "long", sources = list(list(event = "members")),
    options = list(unit = "event", columns_derived = list("event_number", "days_since_previous"))), events = list(
      episodes = list(table = "episode", concept_set = list(concepts = list(32531))),
      members = list(episode_members = list(episode = list(event = "episodes")))))
  plan$variables <- NULL
  plan$populations$t$index$source <- list(event = "episodes")
  frame <- data.frame(subject_id = 3301L, cohort_start_date = as.Date("2020-01-01"),
                      cohort_end_date = as.Date("2020-12-31"), index_source = "episodes",
                      index_key = c("3301001", "3301002"))
  for (b in names(.ws7_out_backends)) {
    f <- .ws7_scenario_run(.ws7_out_backends[[b]]$handle, plan, frame)$compiled$components$o$frame
    # The shared January 1 fill appears once; the distinct January 15 and
    # February 10 fills from the two parents must both survive.
    expect_identical(f$start_date, c(0L, 14L, 40L), info = b)
    expect_identical(f$event_number, 1:3, info = b)
    expect_identical(f$days_since_previous, c(NA_integer_, 14L, 26L), info = b)
    expect_false(any(c("row_id", "cohort_row_id") %in% names(f)), info = b)
    p <- plan
    p$outputs$o$options$temporal <- list(event_select = "last")
    f <- .ws7_scenario_run(.ws7_out_backends[[b]]$handle, p, frame)$compiled$components$o$frame
    expect_identical(f$start_date, 40L, info = b)
  }
  # Pin the ordering of same-day members whose owning episodes differ. The
  # private stream double puts the later parent's distinct member in a
  # table that sorts first; the former episode key must not override it.
  fetch <- .reduceFetch
  testthat::local_mocked_bindings(.reduceFetch = function(ctx, fragment, ...) {
    rows <- fetch(ctx, fragment, ...)
    if (identical(fragment$kind, "stream") &&
        all(c("parent_event_id", "member_table") %in% names(rows)) && nrow(rows)) {
      last_id <- rows$event_id[[which.max(.reduceKeyOrder(rows$event_id))]]
      rows$member_table[rows$event_id %in% last_id] <- "condition_occurrence"
      rows$start_date <- rep(as.Date("2020-01-01"), nrow(rows))
      rows$end_date <- rows$start_date
    }
    rows
  })
  f <- .ws7_scenario_run(.ws7_out_backends$sqlite$handle, plan, frame)$compiled$components$o$frame
  expect_identical(f$member_table, c("condition_occurrence", "drug_exposure", "drug_exposure"))
  plan$outputs$o$options$temporal <- list(event_select = "first")
  f <- .ws7_scenario_run(.ws7_out_backends$sqlite$handle, plan, frame)$compiled$components$o$frame
  expect_identical(f$member_table, "condition_occurrence")
})

test_that("adapter ordering and relation self-exclusion keep physical table identity", {
  rows <- data.frame(row_key = "1", source_ns = "members",
    start_date = as.Date(c("2020-01-10", "2020-01-10")), end_date = as.Date(c("2020-01-10", "2020-01-10")),
    member_table = c("drug_exposure", "condition_occurrence"), event_id = c("1", "99"),
    parent_event_id = "5", concept_id = c(2L, 3L), visit_id = c(NA_character_, "2"))
  attr(rows, "source_ordering") <- c("row_key", "start_date", "source_ns", "member_table", "event_id", "parent_event_id")
  got <- .survivalEpisodeEvents(list(a = rows), "a", "1")
  expect_identical(got$key, c("99", "1"))
  # Equal event IDs in different physical tables are not the same event.
  rows$event_id <- "1"
  rel <- list(within = list(0L, 0L), exclude_self = TRUE)
  expect_identical(.longRelation(rows[1L, ], rows, rel, TRUE), 0L)
  rel$distinct_by <- "visit"
  expect_identical(.longRelation(rows[1L, ], rows, rel, TRUE), NA_integer_)
  rows$concept_id[[1L]] <- NA_integer_
  rel$distinct_by <- "concept"
  expect_identical(.longRelation(rows[1L, ], rows, rel, TRUE), NA_integer_)
})
