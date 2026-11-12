# WS-5: intervals and eras (design-2.10.3 sections 2.6 and 6.7; 10.2 interval
# fixtures) on every executable backend available: era normalisation from the
# original end/supply roles, gaps and islands with a running maximum per
# (person, group), recorded coverage separate from the course, ingredient
# grouping, group-qualified era identities, the stockpiling iterator and the
# covered_days / supply_days bases.

.ws5_iv_backends <- .ws5_backends()
withr::defer(.ws4_cleanup(.ws5_iv_backends), testthat::teardown_env())

.ws5_iv_ctx <- function(h, intervals, events = list(drug = .ws5_drug_event()),
                        populations = NULL) {
  .ws4_ctx(h, .ws5_plan(events = events, intervals = intervals, populations = populations))
}

.ws5_course <- function(ctx, name, persons) {
  iv <- .compileIntervalByName(ctx, name)
  r <- .ws5_rows(ctx, iv$course, c("person_id", "start_date", "event_id"))
  r[r$person_id %in% persons, c("person_id", "group_id", "era_no", "start_date", "end_date",
                                "n_events", "era_key", "concept_id"), drop = FALSE]
}

.ws5_recorded <- function(ctx, name, persons, relation = "recorded") {
  iv <- .compileIntervalByName(ctx, name)
  r <- .ws5_rows(ctx, iv[[relation]], c("person_id", "group_id", "start_date", "event_id"))
  r[r$person_id %in% persons, , drop = FALSE]
}

test_that("the era counterexample: [1,2] + [10,11] with gap 30 is one 11-day course and 4 covered days", {
  for (b in names(.ws5_iv_backends)) {
    h <- .ws5_iv_backends[[b]]$handle
    ctx <- .ws5_iv_ctx(h, list(era = list(source = list(event = "drug"), gap = 30)))
    course <- .ws5_course(ctx, "era", 3001)
    expect_identical(nrow(course), 1L, info = b)
    expect_identical(c(course$start_date, course$end_date), c("2020-01-01", "2020-01-11"), info = b)
    expect_identical(as.integer(as.Date(course$end_date) - as.Date(course$start_date)) + 1L, 11L)
    expect_identical(course$n_events, 2, info = b)
    rec <- .ws5_recorded(ctx, "era", 3001)
    expect_identical(rec$start_date, c("2020-01-01", "2020-01-10"), info = b)
    expect_identical(rec$end_date, c("2020-01-02", "2020-01-11"), info = b)
    expect_identical(rec$era_no, c(1, 1), info = b)
    # covered_days = union length of the recorded intervals (grace gap excluded)
    win <- data.frame(key = 1, lo = as.Date(NA), hi = as.Date(NA))
    expect_identical(.coveredDays(data.frame(key = 1, start = rec$start_date, end = rec$end_date), win), 4L)
    # the same records with gap 6 form two courses (10 > 2 + 6 + 1)
    ctx6 <- .ws5_iv_ctx(h, list(era = list(source = list(event = "drug"), gap = 6)))
    expect_identical(nrow(.ws5_course(ctx6, "era", 3001)), 2L, info = b)
    ctx7 <- .ws5_iv_ctx(h, list(era = list(source = list(event = "drug"), gap = 7)))
    expect_identical(nrow(.ws5_course(ctx7, "era", 3001)), 1L, info = b)
  }
})

test_that("gaps and islands use the running maximum of prior ends; adjacency merges at gap 0", {
  for (b in names(.ws5_iv_backends)) {
    h <- .ws5_iv_backends[[b]]$handle
    ctx <- .ws5_iv_ctx(h, list(era0 = list(source = list(event = "drug"), gap = 0),
                               era1 = list(source = list(event = "drug"), gap = 1),
                               maxgap = list(source = list(event = "drug"), gap = .Machine$integer.max)))
    # nested: [01-20, 01-25] starts after the nested record's end (01-10) but
    # inside the running maximum (01-31): one era, never three
    nested <- .ws5_course(ctx, "era0", 3002)
    expect_identical(nrow(nested), 1L, info = b)
    expect_identical(c(nested$start_date, nested$end_date), c("2020-01-01", "2020-01-31"), info = b)
    expect_identical(nested$n_events, 3, info = b)
    # adjacent records merge at gap 0; one uncovered day needs gap 1
    adj0 <- .ws5_course(ctx, "era0", 3003)
    expect_identical(adj0$start_date, c("2020-01-01", "2020-01-22"), info = b)
    expect_identical(adj0$end_date, c("2020-01-20", "2020-01-25"), info = b)
    expect_identical(adj0$era_no, c(1, 2), info = b)
    expect_identical(adj0$era_key, c("1:1", "1:2"), info = b)
    adj1 <- .ws5_course(ctx, "era1", 3003)
    expect_identical(nrow(adj1), 1L, info = b)
    expect_no_warning(maxgap <- .ws5_course(ctx, "maxgap", 3003))
    expect_identical(nrow(maxgap), 1L, info = b)
    # a one-day record is a one-day era
    one <- .ws5_course(ctx, "era0", 3004)
    expect_identical(c(one$start_date, one$end_date), c("2020-03-01", "2020-03-01"), info = b)
  }
})

test_that("era ends come from the original end/supply roles, never from the event's coalesced end", {
  for (b in names(.ws5_iv_backends)) {
    h <- .ws5_iv_backends[[b]]$handle
    # the EVENT declares a supply fallback; the eras override it
    events <- list(drug = .ws5_drug_event(end = list(fallback = "days_supply")),
                   plain = .ws5_drug_event())
    ctx <- .ws5_iv_ctx(h, list(
      supply = list(source = list(event = "plain"), gap = 0, fallback = "days_supply"),
      nofb = list(source = list(event = "drug"), gap = 0, fallback = "none"),
      point = list(source = list(event = "drug"), gap = 0, end = "start")), events = events)
    # NULL end with supply 10: fallback -> [01-01, 01-10] even over an event
    # without its own fallback; fallback none -> the start day only, even over an
    # event whose own end policy used the supply
    expect_identical(.ws5_course(ctx, "supply", 3005)$end_date, "2020-01-10", info = b)
    expect_identical(.ws5_course(ctx, "nofb", 3005)$end_date, "2020-01-01", info = b)
    ev <- .ws4_rows(ctx, "drug", where = "x.person_id = 3005")
    expect_identical(ev$end_date, "2020-01-10", info = b)   # the event's own policy is untouched
    # end = "start": a recorded [01-01, 01-31] becomes a point record
    expect_identical(.ws5_course(ctx, "point", 3006)$end_date, "2020-01-01", info = b)
    expect_identical(.ws5_course(ctx, "supply", 3006)$end_date, "2020-01-31", info = b)
    # data-quality policy: a non-positive supply as the only end source never
    # creates coverage; without the supply fallback the start day stands
    expect_identical(nrow(.ws5_course(ctx, "supply", 3007)), 0L, info = b)
    expect_identical(nrow(.ws5_recorded(ctx, "supply", 3007)), 0L, info = b)
    expect_identical(.ws5_course(ctx, "nofb", 3007)$start_date, c("2020-01-01", "2020-02-01"), info = b)
    # NULL end and NULL supply is a one-day record; an end before the start
    # contributes nothing (reach person 1005)
    plan <- .ws5_plan(events = list(metf = list(table = "drug_exposure",
                                                concept_set = list(concepts = list(1503297)))),
                      intervals = list(m = list(source = list(event = "metf"), gap = 30)))
    ctx2 <- .ws4_ctx(h, plan)
    rec <- .ws5_recorded(ctx2, "m", 1005)
    expect_identical(rec$start_date, "2020-02-01", info = b)
    expect_identical(rec$end_date, "2020-02-01", info = b)
  }
})

test_that("persistence extends course construction only; min_days filters eras by recorded span", {
  for (b in names(.ws5_iv_backends)) {
    h <- .ws5_iv_backends[[b]]$handle
    ctx <- .ws5_iv_ctx(h, list(
      p0 = list(source = list(event = "drug"), gap = 30),
      p15 = list(source = list(event = "drug"), gap = 30, persistence = 15),
      md = list(source = list(event = "drug"), gap = 0, min_days = 2)))
    expect_identical(nrow(.ws5_course(ctx, "p0", 3008)), 2L, info = b)
    p15 <- .ws5_course(ctx, "p15", 3008)
    expect_identical(nrow(p15), 1L, info = b)
    expect_identical(c(p15$start_date, p15$end_date), c("2020-01-01", "2020-03-11"), info = b)
    # the recorded relation is unchanged by persistence (covered_days excludes it)
    rec <- .ws5_recorded(ctx, "p15", 3008)
    expect_identical(rec$end_date, c("2020-01-10", "2020-02-25"), info = b)
    expect_identical(rec$era_no, c(1, 1), info = b)
    # min_days = 2 drops the one-day era (3004) from the course and its records
    expect_identical(nrow(.ws5_course(ctx, "md", 3004)), 0L, info = b)
    expect_identical(nrow(.ws5_recorded(ctx, "md", 3004)), 0L, info = b)
    expect_identical(nrow(.ws5_course(ctx, "md", 3001)), 2L, info = b)
  }
})

test_that("by = ingredient groups by RxNorm ingredient ancestors with the unmapped policy", {
  W <- .WS5
  for (b in names(.ws5_iv_backends)) {
    h <- .ws5_iv_backends[[b]]$handle
    events <- list(drug = .ws5_drug_event(c(W$DRUG_A, W$DRUG_AB, W$DRUG_X)))
    ctx <- .ws5_iv_ctx(h, list(
      own = list(source = list(event = "drug"), gap = 0, by = "ingredient"),
      drop = list(source = list(event = "drug"), gap = 0, by = "ingredient", unmapped = "drop"),
      byc = list(source = list(event = "drug"), gap = 0, by = "concept"),
      one = list(source = list(event = "drug"), gap = 0)), events = events)
    own <- .ws5_course(ctx, "own", 3009)
    own <- own[order(own$group_id), ]
    expect_identical(own$group_id, c(W$ING_A, W$ING_B, W$DRUG_X) + 0, info = b)
    expect_identical(own$concept_id, own$group_id, info = b)
    expect_identical(own$start_date, c("2020-01-01", "2020-01-05", "2020-01-03"), info = b)
    expect_identical(own$end_date, c("2020-01-20", "2020-01-20", "2020-01-04"), info = b)
    # group-qualified identities: both ingredients' first eras are distinct
    expect_identical(own$era_no, c(1, 1, 1), info = b)
    expect_identical(own$era_key, paste0(c(W$ING_A, W$ING_B, W$DRUG_X), ":1"), info = b)
    expect_false(anyDuplicated(own$era_key) > 0)
    # the combination drug contributes to both ingredient groups
    rec <- .ws5_recorded(ctx, "own", 3009)
    expect_identical(sum(rec$concept_id == W$DRUG_AB), 2L, info = b)
    drop <- .ws5_course(ctx, "drop", 3009)
    expect_setequal(drop$group_id, c(W$ING_A, W$ING_B))
    byc <- .ws5_course(ctx, "byc", 3009)
    expect_identical(nrow(byc), 3L, info = b)
    # by = event: one group (the unmapped record [01-03, 01-04] is nested)
    one <- .ws5_course(ctx, "one", 3009)
    expect_identical(c(nrow(one), one$group_id, one$era_key), c("1", "1", "1:1"), info = b)
    expect_true(is.na(one$concept_id), info = b)
  }
})

test_that("recomputed eras equal OMOP drug_era on the matched-definition fixture", {
  W <- .WS5
  for (b in names(.ws5_iv_backends)) {
    h <- .ws5_iv_backends[[b]]$handle
    events <- list(fills = .ws5_drug_event(c(W$PAR_DRUG1, W$PAR_DRUG2)),
                   std = list(table = "drug_era", concept_set = list(concepts = list(W$ING_A))))
    ctx <- .ws5_iv_ctx(h, list(era = list(source = list(event = "fills"), gap = 30, persistence = 0,
                                          by = "ingredient", fallback = "days_supply")),
                       events = events)
    mine <- .ws5_course(ctx, "era", 3020:3022)
    std <- .ws4_rows(ctx, "std", where = "x.person_id >= 3020 AND x.person_id <= 3022")
    got <- data.frame(person_id = mine$person_id, concept = mine$group_id,
                      start = mine$start_date, end = mine$end_date)
    want <- data.frame(person_id = as.numeric(std$person_id), concept = as.numeric(std$concept_id),
                       start = std$start_date, end = std$end_date)
    got <- got[do.call(order, got), ]
    want <- want[do.call(order, want), ]
    rownames(got) <- rownames(want) <- NULL
    expect_identical(got, want, info = b)
  }
})

test_that("stockpiling: a separate effective relation; recorded coverage is never replaced", {
  for (b in names(.ws5_iv_backends)) {
    h <- .ws5_iv_backends[[b]]$handle
    events <- list(metf = list(table = "drug_exposure", concept_set = list(concepts = list(1503297)),
                               end = list(fallback = "days_supply")),
                   drug = .ws5_drug_event())
    ctx <- .ws5_iv_ctx(h, list(
      rec = list(source = list(event = "metf"), gap = 30),
      stock = list(source = list(event = "metf"), gap = 30, stockpile = TRUE),
      dstock = list(source = list(event = "drug"), gap = 30, stockpile = TRUE)), events = events)
    .ws5_release(ctx)
    plain <- .compileIntervalByName(ctx, "rec")
    expect_null(plain$effective)
    st <- .compileIntervalByName(ctx, "stock")
    expect_s3_class(st$effective, "dsomop_fragment")
    # overlapping fills [1,30] and [15,44] with supply 30 each: recorded 44 days,
    # effective 60 days (reach person 1004)
    rec <- .ws5_recorded(ctx, "stock", 1004)
    eff <- .ws5_recorded(ctx, "stock", 1004, "effective")
    win <- data.frame(key = 1004, lo = as.Date(NA), hi = as.Date(NA))
    expect_identical(.coveredDays(data.frame(key = 1004, start = rec$start_date, end = rec$end_date), win), 44L, info = b)
    expect_identical(.coveredDays(data.frame(key = 1004, start = eff$start_date, end = eff$end_date), win), 60L, info = b)
    expect_identical(eff$carry, c(0, 16), info = b)
    expect_identical(eff$end_date, c("2020-01-30", "2020-02-29"), info = b)
    # the recorded relation of a stockpiled source equals the plain one
    expect_identical(as.list(rec[, c("start_date", "end_date")]),
                     as.list(.ws5_recorded(ctx, "rec", 1004)[, c("start_date", "end_date")]))
    # the stockpiled course is built from the effective rows
    expect_identical(.ws5_course(ctx, "stock", 1004)$end_date, "2020-02-29", info = b)
    expect_identical(.ws5_course(ctx, "rec", 1004)$end_date, "2020-02-13", info = b)
    # pre-window carry-in: fills before the window carry into it
    eff10 <- .ws5_recorded(ctx, "dstock", 3010, "effective")
    expect_identical(eff10$end_date, c("2019-12-30", "2020-01-29"), info = b)
    jan <- data.frame(key = 3010, lo = as.Date("2020-01-01"), hi = as.Date("2020-01-31"))
    expect_identical(.coveredDays(data.frame(key = 3010, start = eff10$start_date, end = eff10$end_date), jan), 29L)
    rec10 <- .ws5_recorded(ctx, "dstock", 3010)
    expect_identical(.coveredDays(data.frame(key = 3010, start = rec10$start_date, end = rec10$end_date), jan), 18L)
    # the carry cap (90 days) binds: carry_i uses the previous row's effective end
    eff11 <- .ws5_recorded(ctx, "dstock", 3011, "effective")
    expect_identical(eff11$carry, c(0, 90, 0), info = b)
    expect_identical(eff11$end_date, c("2020-07-18", "2020-04-10", "2020-06-10"), info = b)
  }
})

test_that("the stockpile iterator is deterministic, bounded and follows the frozen precedence", {
  rows <- data.frame(person_id = c(1, 1, 1, 2), group_id = 1,
                     start_date = as.Date(c("2020-01-01", "2020-01-15", "2020-03-01", "2020-01-01")),
                     end_recorded = as.Date(c(NA, "2020-01-24", NA, NA)),
                     supply = c(30, NA, 2.5, NA))
  out <- .stockpileIterate(rows, cap = 90L)
  # supply precedence: positive integer supply, else the recorded length, else one day
  expect_identical(out$supply_days, c(30L, 10L, 1L, 1L))
  expect_identical(out$carry, c(0L, 16L, 0L, 0L))
  expect_identical(format(out$end_date), c("2020-01-30", "2020-02-09", "2020-03-01", "2020-01-01"))
  # carry never exceeds the cap; a new (person, group) starts without carry
  capped <- .stockpileIterate(data.frame(person_id = 1, group_id = 1,
                                         start_date = as.Date(c("2020-01-01", "2020-01-02")),
                                         end_recorded = as.Date(NA), supply = c(300, 5)), cap = 7L)
  expect_identical(capped$carry, c(0L, 7L))
  expect_identical(.stockpileIterate(rows[0, ], 90L)$carry, integer(0))
  withr::with_options(list(dsomop.max_stockpile_days = -1), expect_error(.intervalStockpileCap(), "between 0 and"))
  withr::with_options(list(dsomop.max_stockpile_days = 30), expect_identical(.intervalStockpileCap(), 30L))
  # N2: a cap beyond the integer range is refused before coercion (never NA)
  withr::with_options(list(dsomop.max_stockpile_days = 2147483648),
                      expect_error(.intervalStockpileCap(), "between 0 and 2147483647"))
  withr::with_options(list(dsomop.max_stockpile_days = 2147483647),
                      expect_identical(.intervalStockpileCap(), .Machine$integer.max))
  withr::with_options(list(dsomop.max_stockpile_days = Inf), expect_error(.intervalStockpileCap()))
  # N2: no integer overflow -- the largest integer supply gives a finite date
  big <- .stockpileIterate(data.frame(person_id = 1, group_id = 1, start_date = as.Date("2020-01-01"),
                                      end_recorded = as.Date(NA), supply = 2147483647), cap = 90L)
  expect_false(is.na(big$end_date))
  expect_true(big$end_date > as.Date("9999-12-31"))
  expect_identical(big$supply_days, .Machine$integer.max)
  expect_no_warning(.ws4_expect_rejection(.stockpileIterate(
    data.frame(person_id = 1, group_id = 1, start_date = as.Date("2020-01-01"),
               end_recorded = as.Date(NA), supply = 2147483648), cap = 90L),
    pattern = "supported integer range"))
  # a non-finite supply is not a valid supply (recorded length, else one day)
  nf <- .stockpileIterate(data.frame(person_id = 1, group_id = 1, start_date = as.Date("2020-01-01"),
                                     end_recorded = as.Date(c(NA, "2020-01-03")), supply = c(Inf, NaN)), 90L)
  expect_identical(nf$supply_days, c(1L, 3L))
  # the partition may carry the parent identity (per-parent stockpiles, A-20 item 6)
  par <- .stockpileIterate(data.frame(person_id = 1, group_id = 1, parent_event_id = c(1, 2),
                                      start_date = as.Date(c("2020-01-01", "2020-01-10")),
                                      end_recorded = as.Date(NA), supply = c(30, 30)), 90L,
                           partition = c("person_id", "group_id", "parent_event_id"))
  expect_identical(par$carry, c(0L, 0L))
})

test_that("stockpile materialisation refuses dates and supplies outside its supported range (N2)", {
  h <- .ws4_reach_handle()
  on.exit(try(DBI::dbDisconnect(h$conn), silent = TRUE), add = TRUE)
  DBI::dbExecute(h$conn, paste0(
    "INSERT INTO drug_exposure (drug_exposure_id, person_id, drug_concept_id, drug_exposure_start_date, ",
    "drug_exposure_end_date, drug_type_concept_id, days_supply) VALUES ",
    "(939091, 1004, 1503297, '2020-01-01', '2020-01-01', 38000177, 2147483647)"))
  compile <- function() {
    ctx <- .ws5_iv_ctx(h, list(s = list(source = list(event = "drug"), stockpile = TRUE)),
                      events = list(drug = list(table = "drug_exposure")))
    on.exit(.planContextClose(ctx), add = TRUE)
    before <- h$temp_tables
    error <- tryCatch(.compileIntervalByName(ctx, "s"), error = identity)
    expect_s3_class(error, "dsomop_plan_rejection")
    expect_identical(error$field, "/intervals/s")
    expect_identical(h$temp_tables, before)
    expect_length(ctx$compile_temp_tables, 0L)
    error
  }
  expect_no_warning(e <- compile())
  expect_match(conditionMessage(e), "supported date range")
  DBI::dbExecute(h$conn, "UPDATE drug_exposure SET days_supply = 2147483648 WHERE drug_exposure_id = 939091")
  expect_no_warning(e <- compile())
  expect_match(conditionMessage(e), "supported integer range")
})

test_that("supply kernels treat non-finite supply as invalid and reject unrepresentable totals", {
  windows <- data.frame(key = 1L, lo = as.Date(NA), hi = as.Date(NA))
  fills <- data.frame(key = 1L, start = as.Date("2020-01-01"), supply = c(Inf, -Inf, NaN))
  for (policy in c("start_in_window", "clipped")) {
    expect_no_warning(value <- .supplyDays(fills, windows, policy))
    expect_identical(value, 0L)
  }
  fills$supply <- c(.Machine$integer.max, 1, 0)
  expect_no_warning(.ws4_expect_rejection(.supplyDays(fills, windows),
                                          pattern = "supported integer range"))
})

test_that("covered_days, supply_days and observed days over rosters and windows", {
  for (b in names(.ws5_iv_backends)) {
    h <- .ws5_iv_backends[[b]]$handle
    events <- list(metf = list(table = "drug_exposure", concept_set = list(concepts = list(1503297)),
                               end = list(fallback = "days_supply")),
                   drug = .ws5_drug_event())
    ctx <- .ws5_iv_ctx(h, list(
      rec = list(source = list(event = "drug"), gap = 30),
      stock = list(source = list(event = "drug"), gap = 30, stockpile = TRUE),
      mrec = list(source = list(event = "metf"), gap = 30),
      mstock = list(source = list(event = "metf"), gap = 30, stockpile = TRUE)), events = events,
      populations = list(cv = list(kind = "criteria", coverage = list(policy = "union", gap = 30L))))
    .ws5_release(ctx)
    # a roster of fixture episodes at index 2019-12-31 (union coverage, gap 30)
    cand <- .fragment("cand_cv", "candidates", "episode",
                      paste0("SELECT ", .sqlLosslessKey(ctx, "w.cand_key"), " AS cand_key, w.subject_id AS subject_id, ",
                             "w.cohort_start_date AS cohort_start_date, w.cohort_end_date AS cohort_end_date FROM ",
                             .qualifyTable(h, "ws5_cov_roster", h$cdm_schema), " w"),
                      columns = list(cand_key = .fragmentColumn("bigint", FALSE, "episode_key"),
                                     subject_id = .fragmentColumn("integer", FALSE, "person_key"),
                                     cohort_start_date = .fragmentColumn("date", FALSE, "start"),
                                     cohort_end_date = .fragmentColumn("date", TRUE, "end")),
                      unique_key = "cand_key", person_lineage = list(person_col = "subject_id", via_roster = NULL),
                      private_columns = "cand_key")
    attr(cand, "population") <- "cv"
    .fragmentRegister(ctx, cand)
    cov <- .compileCoverage(ctx, "cv", list(policy = "union", gap = 30), cand, "candidate")
    roster <- .ws5_roster(ctx, cand, cov, pop = "cv")
    val <- function(fn, ...) {
      v <- .ws5_rv(ctx, fn, roster, "final", cov = cov, ...)
      stats::setNames(v$value, as.character(v$row_key))
    }
    w31 <- list(start = 1, end = 31, anchor = "index")
    w10 <- list(start = 1, end = 10, anchor = "index")
    # supply_days under both fill policies, with invalid supplies (3012)
    s1 <- val("supply_days", source = list(event = "drug"), window = w31)
    s2 <- val("supply_days", source = list(event = "drug"), window = w31, fills = "clipped")
    expect_identical(unname(s1[["1"]]), 10L, info = b)   # 01-05 (10); 01-20 (0) and 01-25 (NULL) contribute 0
    expect_identical(unname(s2[["1"]]), 23L, info = b)   # 01-05..01-14 (10) + 12-25..01-13 inside (13)
    # covered_days(observed_only) against the episode's observation union:
    # 4 on the PDC fixture (1003), unchanged when observation covers the window
    # (3012), 0 when it does not (3013) -- for both coverage relations
    for (rel in c("recorded", "effective_supply")) {
      src <- if (identical(rel, "recorded")) "mrec" else "mstock"
      plain <- val("covered_days", source = list(interval = src), window = w10, coverage = rel)
      obs <- val("covered_days", source = list(interval = src), window = w10, coverage = rel,
                 observed_only = TRUE)
      expect_identical(unname(plain[["3"]]), 10L, info = paste(b, rel))
      expect_identical(unname(obs[["3"]]), 4L, info = paste(b, rel))
      dsrc <- if (identical(rel, "recorded")) "rec" else "stock"
      dplain <- val("covered_days", source = list(interval = dsrc), window = w31, coverage = rel)
      dobs <- val("covered_days", source = list(interval = dsrc), window = w31, coverage = rel,
                  observed_only = TRUE)
      expect_identical(dobs[["1"]], dplain[["1"]], info = paste(b, rel))
      expect_true(dplain[["2"]] > 0L, info = paste(b, rel))
      expect_identical(unname(dobs[["2"]]), 0L, info = paste(b, rel))
    }
    od <- val("observed_days", window = w10)
    expect_identical(unname(od[c("1", "2", "3")]), c(10L, 0L, 4L), info = b)
    # pre-window carry-in reaches the window through the effective relation (3010)
    eff <- val("covered_days", source = list(interval = "stock"), window = w31, coverage = "effective_supply")
    rec <- val("covered_days", source = list(interval = "rec"), window = w31)
    expect_identical(unname(c(eff[["4"]], rec[["4"]])), c(29L, 18L), info = b)
    # effective_supply needs a stockpiled interval source
    .ws4_expect_rejection(val("covered_days", source = list(interval = "rec"), window = w31,
                              coverage = "effective_supply"), pattern = "stockpiled")
  }
})

test_that("covered_days and supply_days kernels: unions, clipping, NA for undefined windows", {
  w <- data.frame(key = c(1, 2, 3), lo = as.Date(c("2020-01-01", NA, "2020-01-01")),
                  hi = as.Date(c("2020-01-10", NA, "2020-01-10")), defined = c(TRUE, TRUE, FALSE))
  iv <- data.frame(key = c(1, 1, 1, 2), start = as.Date(c("2019-12-25", "2020-01-05", "2020-01-08", "2020-01-01")),
                   end = as.Date(c("2020-01-02", "2020-01-09", "2020-01-20", "2020-01-01")))
  # [01-01,01-02] + [01-05,01-10] = 8 (overlaps unioned, never summed); unbounded 1; undefined NA
  expect_identical(.coveredDays(iv, w), c(8L, 1L, NA))
  obs <- data.frame(key = 1, start = as.Date(c("2020-01-01", "2020-01-09")), end = as.Date(c("2020-01-02", "2020-01-10")))
  expect_identical(.coveredDays(iv, w, obs)[[1L]], 4L)
  expect_identical(.coveredDays(iv[0, ], w), c(0L, 0L, NA))
  # the PDC pin: exposure [1,10], observed [1,2] + [9,10], window [1,10] -> 4 and 4
  pdc_w <- data.frame(key = 1, lo = as.Date("2020-01-01"), hi = as.Date("2020-01-10"))
  pdc_e <- data.frame(key = 1, start = as.Date("2020-01-01"), end = as.Date("2020-01-10"))
  expect_identical(.coveredDays(pdc_e, pdc_w, obs), 4L)
  expect_identical(.observedDays(obs, pdc_w), 4L)
  fills <- data.frame(key = 1, start = as.Date(c("2019-12-28", "2020-01-05", "2020-01-07", "2020-01-08", "2020-01-09")),
                      supply = c(10, 3, 0, 2.5, 5), end_recorded = as.Date(c(NA, NA, NA, NA, "2020-01-01")))
  # start_in_window: 3 (01-05); 0, non-integer and the end-before-start row give 0
  expect_identical(.supplyDays(fills, w[1, ]), 3L)
  # clipped: 12-28..01-06 inside = 6, 01-05..01-07 = 3 (overlaps not unioned)
  expect_identical(.supplyDays(fills, w[1, ], "clipped"), 9L)
  expect_identical(.supplyDays(fills, w[3, ]), NA_integer_)
  expect_identical(.dayUnionLength(c(1L, 3L, 10L), c(2L, 5L, 10L)), 6L)
})

test_that("an adapter source contributes each physical event once (episode members)", {
  for (b in names(.ws5_iv_backends)) {
    h <- .ws5_iv_backends[[b]]$handle
    events <- list(episodes = list(table = "episode", concept_set = list(concepts = list(32531))),
                   members = list(episode_members = list(episode = list(event = "episodes"))))
    ctx <- .ws5_iv_ctx(h, list(m = list(source = list(event = "members"), gap = 30, by = "concept"),
                               ms = list(source = list(event = "members"), gap = 30, stockpile = TRUE)),
                       events = events)
    .ws5_release(ctx)
    # 1015's chemotherapy fill of 2019-04-12 is linked to two episodes: one record
    rec <- .ws5_recorded(ctx, "m", 1015)
    expect_identical(nrow(rec), 5L, info = b)
    expect_false(anyDuplicated(paste(rec$member_table, rec$event_id)) > 0, info = b)
    course <- .ws5_course(ctx, "m", 1015)
    chemo <- course[course$group_id == 1718850, ]
    expect_identical(chemo$n_events, c(3, 1), info = b)
    expect_identical(chemo$era_key, c("1718850:1", "1718850:2"), info = b)
    eff <- .ws5_recorded(ctx, "ms", 1015, "effective")
    expect_identical(sum(eff$carry), 0, info = b)
    expect_identical(nrow(eff), 5L, info = b)
  }
})

test_that("an unavailable source contributes no coverage and no course", {
  for (b in names(.ws5_iv_backends)) {
    h <- .ws5_iv_backends[[b]]$handle
    events <- list(notes = list(table = "note", on_missing_table = "unavailable"))
    if (!is.null(.roleOf(h, "note"))) next
    ctx <- .ws5_iv_ctx(h, list(n = list(source = list(event = "notes")),
                               ns = list(source = list(event = "notes"), stockpile = TRUE)), events = events)
    iv <- .compileIntervalByName(ctx, "n")
    expect_identical(iv$course$availability, "unavailable", info = b)
    expect_identical(iv$recorded$availability, "unavailable", info = b)
    expect_identical(nrow(.ws5_rows(ctx, iv$course, "person_id")), 0L, info = b)
    st <- .compileIntervalByName(ctx, "ns")
    expect_identical(st$effective$availability, "unavailable", info = b)
    expect_identical(nrow(.ws5_rows(ctx, st$effective, "person_id")), 0L, info = b)
    expect_null(.intervalMeta(ctx, "ns")$temp_table)
  }
  # an unavailable adapter keeps its declared contract (A-17): the interval
  # over it has the same keys and still contributes nothing
  h2 <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h2$conn), add = TRUE)
  DBI::dbExecute(h2$conn, "DROP TABLE episode_event")
  h2$blueprint <- NULL
  h2$role_registry <- NULL
  ctx2 <- .ws5_iv_ctx(h2, list(m = list(source = list(event = "members"), gap = 30, stockpile = TRUE)),
                      events = list(episodes = list(table = "episode", concept_set = list(concepts = list(32531))),
                                    members = list(episode_members = list(episode = list(event = "episodes"),
                                                                          on_missing_table = "unavailable"))))
  iv <- .compileIntervalByName(ctx2, "m")
  expect_identical(.eventMeta(ctx2, "members")$availability, "unavailable")
  expect_identical(c(iv$course$availability, iv$recorded$availability, iv$effective$availability),
                   rep("unavailable", 3L))
  expect_true("member_table" %in% iv$recorded$unique_key)
  expect_identical(nrow(.ws5_rows(ctx2, iv$recorded, "person_id")), 0L)
  expect_identical(nrow(.ws5_rows(ctx2, iv$course, "person_id")), 0L)
  expect_null(.intervalMeta(ctx2, "m")$temp_table)
})

test_that("interval nodes are validated and compiled once per context", {
  h <- .ws5_iv_backends$sqlite$handle
  ctx <- .ws5_iv_ctx(h, list(e = list(source = list(event = "drug"), gap = 30)))
  a <- .compileIntervalByName(ctx, "e")
  expect_identical(.compileIntervalByName(ctx, "e"), a)
  expect_identical(a$course$name, "iv_e")
  expect_identical(a$recorded$name, "iv_e_rec")
  expect_identical(a$course$source_identity_columns, c("source_ns", "person_id", "group_id", "era_no"))
  expect_true(all(c("era_no", "group_id", "n_events", "era_key", "event_id") %in% a$course$private_columns))
  expect_identical(.intervalMeta(ctx, "e")$spec$gap, 30L)
  # identifiers are allocated per context from the declared identities (A-17):
  # an interval "e_rec" and the recorded stage of "e" coexist
  ctx2 <- .ws5_iv_ctx(h, list(e = list(source = list(event = "drug"), gap = 30),
                              e_rec = list(source = list(event = "drug"), gap = 0)))
  x <- .compileIntervalByName(ctx2, "e")
  y <- .compileIntervalByName(ctx2, "e_rec")
  expect_false(identical(tolower(x$recorded$name), tolower(y$course$name)))
  expect_match(x$recorded$name, "^iv_e_rec_[0-9a-f]{10}$")
  expect_identical(nrow(.ws5_course(ctx2, "e", 3003)), 1L)
  expect_identical(nrow(.ws5_course(ctx2, "e_rec", 3003)), 2L)
  expect_error(.fragmentName("iv", "nope", ctx = ctx2), "not declared by an identity generator")
  .ws4_expect_rejection(.intervalSpec(list(source = list(event = "drug"), gap = -1), "x"), pattern = "gap")
  .ws4_expect_rejection(.intervalSpec(list(source = list(interval = "y")), "x"), pattern = "event reference")
  .ws4_expect_rejection(.intervalSpec(list(source = list(event = "drug"), by = "atc"), "x"), pattern = "by")
  .ws4_expect_rejection(.intervalSpec(list(source = list(event = "drug"), extra = 1), "x"), pattern = "unknown")
})

# --- A-20 fix round (Astra's WS-5 review B1, B2, B4, B6, B7; item 6) ---------------------

.ws5_course_full <- function(ctx, name, persons, association = "person") {
  iv <- .compileIntervalByName(ctx, name, association = association)
  r <- .ws5_rows(ctx, iv$course, c("person_id", "start_date", "event_id"))
  r[r$person_id %in% persons, , drop = FALSE]
}

test_that("min_days measures the normalised recorded span, for stockpiled sources too (A-20 item 3, B1)", {
  for (b in names(.ws5_iv_backends)) {
    h <- .ws5_iv_backends[[b]]$handle
    events <- list(metf = list(table = "drug_exposure", concept_set = list(concepts = list(1503297)),
                               end = list(fallback = "days_supply")))
    iv <- function(...) c(list(source = list(event = "metf"), gap = 30), list(...))
    ctx <- .ws5_iv_ctx(h, list(
      s44 = iv(stockpile = TRUE, min_days = 44), s45 = iv(stockpile = TRUE, min_days = 45),
      s50 = iv(stockpile = TRUE, min_days = 50),
      s44p = iv(stockpile = TRUE, min_days = 44, persistence = 30),
      s45p = iv(stockpile = TRUE, min_days = 45, persistence = 30),
      s15points = iv(stockpile = TRUE, end = "start", min_days = 15),
      s16points = iv(stockpile = TRUE, end = "start", min_days = 16, persistence = 30),
      r44p = iv(min_days = 44, persistence = 30), r45p = iv(min_days = 45, persistence = 30)),
      events = events)
    .ws5_release(ctx)
    # reach person 1004: fills [01-01, 01-30] + [01-15, 02-13] -- normalised
    # recorded span 44 days; effective span 60 days (carry 16)
    for (name in c("s45", "s50", "s45p", "r45p", "s16points")) {
      info <- paste(b, name)
      expect_identical(nrow(.ws5_course(ctx, name, 1004)), 0L, info = info)
      expect_identical(nrow(.ws5_recorded(ctx, name, 1004)), 0L, info = info)
      if (startsWith(name, "s")) {
        expect_identical(nrow(.ws5_recorded(ctx, name, 1004, "effective")), 0L, info = info)
      }
    }
    # at the threshold the era stays, in all three relations: course end =
    # effective end (+ persistence) when stockpiled, recorded end + persistence
    # otherwise; course end_recorded = the normalised recorded end, never effective
    ends <- function(name) {
      c <- .ws5_course_full(ctx, name, 1004)
      c(c$start_date, c$end_date, c$end_recorded)
    }
    expect_identical(ends("s44"), c("2020-01-01", "2020-02-29", "2020-02-13"), info = b)
    expect_identical(ends("s44p"), c("2020-01-01", "2020-03-30", "2020-02-13"), info = b)
    expect_identical(ends("r44p"), c("2020-01-01", "2020-03-14", "2020-02-13"), info = b)
    # An era override changes the measured recorded end, even though the
    # original recorded roles and effective supply still extend beyond it.
    expect_identical(ends("s15points"), c("2020-01-01", "2020-02-29", "2020-01-15"), info = b)
    expect_identical(nrow(.ws5_recorded(ctx, "s44", 1004)), 2L, info = b)
    expect_identical(.ws5_recorded(ctx, "s44", 1004, "effective")$end_date,
                     c("2020-01-30", "2020-02-29"), info = b)
    expect_identical(.ws5_recorded(ctx, "s44", 1004, "effective")$norm_end,
                     c("2020-01-30", "2020-02-13"), info = b)
    # the retained flag is one marks pass: no relation reads its own course
    meta <- .intervalMeta(ctx, "s44")
    for (rel in c("course", "recorded", "effective")) {
      f <- .fragmentGet(ctx, meta$fragments[[rel]])
      expect_false(meta$fragments[["course"]] %in% setdiff(f$dependencies, f$name), info = paste(b, rel))
      expect_false(grepl("EXISTS", f$select_body, fixed = TRUE), info = paste(b, rel))
    }
  }
})

test_that("direct-event coverage applies the one quality policy: event/interval parity (A-20 item 2, B2)", {
  for (b in names(.ws5_iv_backends)) {
    h <- .ws5_iv_backends[[b]]$handle
    ctx <- .ws5_iv_ctx(h, list(
      i_fb = list(source = list(event = "d_fb")),
      i_none = list(source = list(event = "d_role"), fallback = "none"),
      i_start = list(source = list(event = "d_start"), end = "start")),
      events = list(d_fb = .ws5_drug_event(end = list(fallback = "days_supply")),
                    d_role = .ws5_drug_event(), d_start = .ws5_drug_event(end = "start")),
      populations = list(t = list(kind = "criteria")))
    # 3007: NULL end with supply 0 and -5; 3015: recorded ends with supply 0,
    # -5, NULL; 3016: NULL end and supply; 3005: NULL end, supply 10; 3004: [03-01]
    cand <- .ws5_literal_roster(ctx, data.frame(cand_key = 1:5, subject_id = c(3007, 3015, 3016, 3005, 3004),
                                                cohort_start_date = "2019-12-31", cohort_end_date = "2020-12-31"))
    w <- list(start = 1L, end = 120L, anchor = "index")
    want <- list(c(0L, 10L, 1L, 10L, 1L), c(2L, 10L, 1L, 1L, 1L), c(2L, 3L, 1L, 1L, 1L))
    pairs <- list(c("d_fb", "i_fb"), c("d_role", "i_none"), c("d_start", "i_start"))
    for (k in seq_along(pairs)) {
      ev <- .ws5_rv(ctx, "covered_days", cand, "candidate", source = list(event = pairs[[k]][[1L]]), window = w)
      iv <- .ws5_rv(ctx, "covered_days", cand, "candidate", source = list(interval = pairs[[k]][[2L]]), window = w)
      expect_identical(ev$value, want[[k]], info = paste(b, pairs[[k]][[1L]]))
      expect_identical(iv$value, want[[k]], info = paste(b, pairs[[k]][[2L]]))
    }
    # supply_days reads the same quality: an invalid supply contributes nothing
    sd <- .ws5_rv(ctx, "supply_days", cand, "candidate", source = list(event = "d_fb"), window = w)
    expect_identical(sd$value, c(0L, 0L, 0L, 10L, 0L), info = b)
  }
  # a fractional supply (SQLite keeps the REAL a vendor integer column cannot)
  h <- .ws4_reach_handle()
  on.exit(try(DBI::dbDisconnect(h$conn), silent = TRUE), add = TRUE)
  .ws5_extend(h$conn)
  DBI::dbExecute(h$conn, paste0(
    "INSERT INTO drug_exposure (drug_exposure_id, person_id, drug_concept_id, drug_exposure_start_date, ",
    "drug_exposure_end_date, drug_type_concept_id, days_supply) VALUES ",
    "(939001, 3017, ", .WS5$ERA_DRUG, ", '2020-01-01', NULL, 38000177, 2.5), ",
    "(939002, 3018, ", .WS5$ERA_DRUG, ", '2020-01-01', '2020-01-03', 38000177, 2.5)"))
  h$blueprint <- NULL
  h$role_registry <- NULL
  ctx <- .ws5_iv_ctx(h, list(i_fb = list(source = list(event = "d_fb")),
                             i_none = list(source = list(event = "d_role"), fallback = "none")),
                     events = list(d_fb = .ws5_drug_event(end = list(fallback = "days_supply")),
                                   d_role = .ws5_drug_event()),
                     populations = list(t = list(kind = "criteria")))
  cand <- .ws5_literal_roster(ctx, data.frame(cand_key = 1:2, subject_id = c(3017, 3018),
                                              cohort_start_date = "2019-12-31", cohort_end_date = "2020-12-31"))
  w <- list(start = 1L, end = 30L, anchor = "index")
  val <- function(src) .ws5_rv(ctx, "covered_days", cand, "candidate", source = src, window = w)$value
  expect_identical(val(list(event = "d_fb")), c(0L, 3L))
  expect_identical(val(list(interval = "i_fb")), c(0L, 3L))
  expect_identical(val(list(event = "d_role")), c(1L, 3L))
  expect_identical(val(list(interval = "i_none")), c(1L, 3L))
})

test_that("an unavailable source is NA for every row; an available event-free source is 0 (A-20 (i), B4)", {
  for (b in names(.ws5_iv_backends)) {
    h <- .ws5_iv_backends[[b]]$handle
    if (!is.null(.roleOf(h, "note"))) next
    ctx <- .ws5_iv_ctx(h, list(n = list(source = list(event = "notes")),
                               ns = list(source = list(event = "notes"), stockpile = TRUE),
                               d = list(source = list(event = "drug")),
                               ds = list(source = list(event = "drug"), stockpile = TRUE)),
                       events = list(notes = list(table = "note", on_missing_table = "unavailable"),
                                     drug = .ws5_drug_event()),
                       populations = list(t = list(kind = "criteria")))
    .ws5_release(ctx)
    # 3001 has drug rows (4 covered days in January); 3202 has none
    cand <- .ws5_literal_roster(ctx, data.frame(cand_key = 1:2, subject_id = c(3001, 3202),
                                                cohort_start_date = "2019-12-31", cohort_end_date = "2020-12-31"))
    w <- list(start = 1L, end = 31L, anchor = "index")
    rv <- function(fn, src, ...) .ws5_rv(ctx, fn, cand, "candidate", source = src, window = w, ...)$value
    na <- c(NA_integer_, NA_integer_)
    expect_identical(rv("covered_days", list(event = "notes")), na, info = b)
    expect_identical(rv("covered_days", list(interval = "n")), na, info = b)
    expect_identical(rv("covered_days", list(interval = "ns"), coverage = "effective_supply"), na, info = b)
    expect_identical(rv("supply_days", list(event = "notes")), na, info = b)
    # available controls: values, and 0 for the event-free row
    expect_identical(rv("covered_days", list(event = "drug")), c(4L, 0L), info = b)
    expect_identical(rv("covered_days", list(interval = "d")), c(4L, 0L), info = b)
    expect_identical(rv("covered_days", list(interval = "ds"), coverage = "effective_supply"), c(4L, 0L), info = b)
    expect_identical(rv("supply_days", list(event = "drug")), c(0L, 0L), info = b)
    # effective_supply over a non-stockpiled source is refused even when unavailable
    .ws4_expect_rejection(rv("covered_days", list(interval = "n"), coverage = "effective_supply"),
                          pattern = "stockpiled")
  }
})

test_that("index-visit stockpiles partition carry and courses before consumer windows (B3)", {
  h <- .ws4_reach_handle()
  on.exit(try(DBI::dbDisconnect(h$conn), silent = TRUE), add = TRUE)
  DBI::dbExecute(h$conn, "ALTER TABLE drug_exposure ADD COLUMN visit_occurrence_id BIGINT")
  DBI::dbExecute(h$conn, paste0(
    "INSERT INTO drug_exposure (drug_exposure_id, person_id, drug_concept_id, drug_exposure_start_date, ",
    "drug_exposure_end_date, drug_type_concept_id, days_supply, visit_occurrence_id) VALUES ",
    "(939081, 1007, 1503297, '2020-01-01', '2020-01-30', 38000177, 30, 700701), ",
    "(939082, 1007, 1503297, '2020-01-15', '2020-02-13', 38000177, 30, 700702), ",
    "(939083, 1007, 1503297, '2020-01-20', '2020-02-18', 38000177, 30, 700701)"))
  h$blueprint <- NULL
  h$role_registry <- NULL
  ctx <- .ws5_iv_ctx(h, list(s = list(source = list(event = "drug"), stockpile = TRUE)),
    events = list(drug = list(table = "drug_exposure", visit = list(index = TRUE))),
    populations = list(t = list(kind = "criteria")))
  on.exit(.planContextClose(ctx), add = TRUE, after = FALSE)
  cand <- .ws5_literal_roster(ctx, data.frame(cand_key = 1:3, subject_id = 1007,
    index_visit_id = c(700701, 700702, NA),
    cohort_start_date = "2019-12-31", cohort_end_date = "2020-12-31"))
  val <- .ws5_rv(ctx, "covered_days", cand, "candidate", source = list(interval = "s"),
                 coverage = "effective_supply")
  expect_identical(val$value, c(60L, 30L, 0L))
  course <- .ws5_course_full(ctx, "s", 1007)
  expect_identical(course$visit_id, c(700701, 700702))
  expect_identical(course$end_date, c("2020-02-29", "2020-02-13"))
  expect_identical(course$era_key, c("700701|1:1", "700702|1:1"))
})

test_that("reducer and stockpile fetches are bounded: cap, cap + 1 and a grown realisation (A-20 (j), B6)", {
  h <- .ws5_iv_backends$sqlite$handle
  ctx <- .ws5_iv_ctx(h, list(d = list(source = list(event = "drug"))),
                     populations = list(t = list(kind = "criteria")))
  # two roster rows; 3002 and 3003 have 3 drug rows each (6 stream rows)
  cand <- .ws5_literal_roster(ctx, data.frame(cand_key = 1:2, subject_id = c(3002, 3003),
                                              cohort_start_date = "2019-12-31", cohort_end_date = "2020-12-31"))
  rv <- function() .ws5_rv(ctx, "covered_days", cand, "candidate", source = list(event = "drug"))
  withr::with_options(list(dsomop.max_memory_rows = 6), expect_identical(rv()$value, c(31L, 24L)))
  e <- withr::with_options(list(dsomop.max_memory_rows = 5), tryCatch(rv(), error = identity))
  expect_s3_class(e, "dsomop_plan_rejection")
  expect_identical(c(e$code, e$reason), c("validation_error", "reducer_row_cap"))
  expect_identical(.rejectionEntry(e), list(code = "validation_error",
                                            message = "Publication refused: validation_error.",
                                            reason = "reducer_row_cap"))
  expect_false(grepl("[0-9]", conditionMessage(e)))
  # the roster bounds are bounded too (two rows, cap 1)
  e <- withr::with_options(list(dsomop.max_memory_rows = 1), tryCatch(rv(), error = identity))
  expect_identical(e$reason, "reducer_row_cap")
  # stockpile: the realised rows are bounded; the COUNT is only an early check
  n <- nrow(.ws5_rows(ctx, .compileIntervalByName(ctx, "d")$recorded, "person_id"))
  stock <- function() {
    sctx <- .ws5_iv_ctx(h, list(s = list(source = list(event = "drug"), stockpile = TRUE)))
    on.exit(.compileReleaseTempTables(sctx), add = TRUE)
    before <- h$temp_tables
    out <- tryCatch(.compileIntervalByName(sctx, "s"), error = identity)
    list(out = out, left = setdiff(h$temp_tables, before), owned = sctx$compile_temp_tables)
  }
  withr::with_options(list(dsomop.max_memory_rows = n), {
    ok <- stock()
    expect_s3_class(ok$out$effective, "dsomop_fragment")
  })
  withr::with_options(list(dsomop.max_memory_rows = n - 1), {
    over <- stock()
    expect_identical(c(over$out$code, over$out$reason), c("validation_error", "stockpile_row_cap"))
    expect_length(over$left, 0L)
    expect_length(over$owned, 0L)
  })
  # a realisation that grew after the pre-count: the bounded fetch refuses it
  testthat::local_mocked_bindings(.adapterCount = function(...) 0)
  withr::with_options(list(dsomop.max_memory_rows = n - 1), {
    grown <- stock()
    expect_identical(grown$out$reason, "stockpile_row_cap")
    expect_length(grown$left, 0L)
  })
})

test_that("compiler session tables are released on every exit path; a failed DROP is retried (A-20 (k), B7)", {
  h <- .ws4_reach_handle()
  on.exit(try(DBI::dbDisconnect(h$conn), silent = TRUE), add = TRUE)
  p <- .ws5_plan(events = list(d = list(table = "drug_exposure")),
                 intervals = list(i = list(source = list(event = "d"), stockpile = TRUE)))
  physical <- function() DBI::dbGetQuery(h$conn, "SELECT name FROM sqlite_temp_master WHERE type = 'table'")$name
  pre <- .createTempTable(h, "ws5_preexisting", "SELECT 1 AS x")
  # success: closing the execution context releases the compiler's table
  ctx <- .ws4_ctx(h, p)
  .compileIntervalByName(ctx, "i")
  owned <- ctx$compile_temp_tables
  expect_length(owned, 1L)
  expect_true(owned %in% physical())
  .planContextClose(ctx)
  expect_length(intersect(owned, c(h$temp_tables, physical())), 0L)
  expect_length(ctx$compile_temp_tables, 0L)
  # the compilation child of a publication context is released with it
  ctx <- .ws4_ctx(h, p)
  cctx <- .compileContext(ctx, ctx$ir)
  .compileIntervalByName(cctx, "i")
  owned <- cctx$compile_temp_tables
  expect_length(owned, 1L)
  .planContextClose(ctx)
  expect_length(intersect(owned, c(h$temp_tables, physical())), 0L)
  # a downstream failure and a rejection after materialisation
  for (fail in list(function() stop("downstream compile failure"),
                    function() .compileReject("downstream refusal", reason = "reducer_row_cap"))) {
    run <- function() {
      ctx <- .ws4_ctx(h, p)
      on.exit(.planContextClose(ctx), add = TRUE)
      .compileIntervalByName(ctx, "i")
      fail()
    }
    expect_error(run(), "downstream")
    expect_identical(setdiff(h$temp_tables, pre), character(0))
    expect_identical(setdiff(physical(), pre), character(0))
  }
  # an insertion failure releases the table at once and re-raises
  ctx <- .ws4_ctx(h, p)
  testthat::with_mocked_bindings(
    expect_error(.compileIntervalByName(ctx, "i"), "injected insert failure"),
    .intervalLiteral = function(...) stop("injected insert failure"))
  expect_length(ctx$compile_temp_tables, 0L)
  expect_identical(setdiff(h$temp_tables, pre), character(0))
  expect_identical(setdiff(physical(), pre), character(0))
  .planContextClose(ctx)
  # a failed DROP keeps the name for a retry and surfaces an aggregated error;
  # the context still closes
  ctx <- .ws4_ctx(h, p)
  .compileIntervalByName(ctx, "i")
  owned <- ctx$compile_temp_tables
  testthat::with_mocked_bindings(
    expect_error(.planContextClose(ctx), "Could not release the compiler temporary table"),
    .dropTempTable = function(handle, name) stop("injected DROP failure"))
  expect_true(isTRUE(ctx$closed))
  expect_identical(ctx$compile_temp_tables, owned)
  expect_true(owned %in% physical())
  .compileReleaseTempTables(ctx)
  expect_length(ctx$compile_temp_tables, 0L)
  expect_length(intersect(owned, c(h$temp_tables, physical())), 0L)
  # objects owned before the operation are never released by the compiler
  expect_true("ws5_preexisting" %in% h$temp_tables)
  expect_true("ws5_preexisting" %in% physical())
})

test_that("an adapter interval is compiled per consumer association (A-20 item 6)", {
  for (b in names(.ws5_iv_backends)) {
    h <- .ws5_iv_backends[[b]]$handle
    events <- list(episodes = list(table = "episode", concept_set = list(concepts = list(32531))),
                   members = list(episode_members = list(episode = list(event = "episodes"),
                                                         tables = list("drug_exposure"))))
    pops <- list(ep = list(kind = "index", index = list(source = list(event = "episodes"), select = "all")),
                 pp = list(kind = "criteria"),
                 mx = list(kind = "index", index = list(source = list(any_of = list(
                   list(event = "episodes"), list(event = "drug"))), select = "all")))
    ctx <- .ws5_iv_ctx(h, list(m = list(source = list(event = "members"), gap = 0),
                               ms = list(source = list(event = "members"), gap = 0, stockpile = TRUE)),
                       events = c(events, list(drug = .ws5_drug_event())), populations = pops)
    .ws5_release(ctx)
    # 3301: F1 (01-01) linked to E1 and E2, F2 (01-15) E1 only, F3 (02-10) E2 only
    person <- .ws5_recorded(ctx, "m", 3301)
    expect_identical(person$start_date, c("2020-01-01", "2020-01-15", "2020-02-10"), info = b)
    par <- .compileIntervalByName(ctx, "m", association = "parent")
    prow <- .ws5_rows(ctx, par$recorded, c("parent_event_id", "start_date"))
    prow <- prow[prow$person_id == 3301, ]
    expect_identical(paste(prow$parent_event_id, prow$start_date),
                     c("3301001 2020-01-01", "3301001 2020-01-15", "3301002 2020-01-01",
                       "3301002 2020-02-10"), info = b)
    # courses keyed per parent: E2's fills are two eras at gap 0, the person's one
    pc <- .ws5_course_full(ctx, "m", 3301, "parent")
    expect_identical(paste(pc$parent_event_id, pc$era_key, pc$start_date, pc$end_date),
                     c("3301001 3301001|1:1 2020-01-01 2020-02-13", "3301002 3301002|1:1 2020-01-01 2020-01-30",
                       "3301002 3301002|1:2 2020-02-10 2020-03-10"), info = b)
    expect_identical(nrow(.ws5_course(ctx, "m", 3301)), 1L, info = b)
    expect_false(identical(par$course$name, .compileIntervalByName(ctx, "m")$course$name))
    # per-parent stockpile: E2's F3 gets no carry from E1's F2
    pe <- .ws5_rows(ctx, .compileIntervalByName(ctx, "ms", association = "parent")$effective,
                    c("parent_event_id", "start_date"))
    pe <- pe[pe$person_id == 3301, ]
    expect_identical(paste(pe$parent_event_id, pe$end_date, pe$carry),
                     c("3301001 2020-01-30 0", "3301001 2020-02-29 16", "3301002 2020-01-30 0",
                       "3301002 2020-03-10 0"), info = b)
    # the person relation counts the shared fill once (carry 16, then 20)
    expect_identical(.ws5_recorded(ctx, "ms", 3301, "effective")$carry, c(0, 16, 20), info = b)
    # consumers: an episode-indexed roster, a person-indexed one, a mixed one
    ep <- .ws5_literal_roster(ctx, data.frame(cand_key = 1:2, subject_id = 3301, cohort_start_date = "2020-01-01",
                                              cohort_end_date = "2020-03-31", index_source = "episodes",
                                              index_key = c("3301001", "3301002")),
                              name = "cand_ep", population = "ep")
    pp <- .ws5_literal_roster(ctx, data.frame(cand_key = 1L, subject_id = 3301, cohort_start_date = "2020-01-01",
                                              cohort_end_date = "2020-03-31"), name = "cand_pp", population = "pp")
    mx <- .ws5_literal_roster(ctx, data.frame(cand_key = 1:2, subject_id = 3301, cohort_start_date = "2020-01-01",
                                              cohort_end_date = "2020-03-31", index_source = c("episodes", "drug"),
                                              index_key = c("3301001", "939999")),
                              name = "cand_mx", population = "mx")
    w <- list(start = 0L, end = 90L, anchor = "index")
    rv <- function(r, fn, src, ...) .ws5_rv(ctx, fn, r, "candidate", source = src, window = w, ...)$value
    expect_identical(rv(ep, "covered_days", list(interval = "m")), c(44L, 60L), info = b)
    expect_identical(rv(pp, "covered_days", list(interval = "m")), 70L, info = b)
    expect_identical(rv(mx, "covered_days", list(interval = "m")), c(44L, 70L), info = b)
    expect_identical(rv(ep, "covered_days", list(interval = "ms"), coverage = "effective_supply"), c(60L, 60L), info = b)
    expect_identical(rv(pp, "covered_days", list(interval = "ms"), coverage = "effective_supply"), 90L, info = b)
    expect_identical(rv(mx, "covered_days", list(interval = "ms"), coverage = "effective_supply"), c(60L, 90L), info = b)
    # direct member events (point records: the members' recorded end is NULL)
    expect_identical(rv(ep, "covered_days", list(event = "members")), c(2L, 2L), info = b)
    expect_identical(rv(pp, "covered_days", list(event = "members")), 3L, info = b)
    # supply: the shared fill counts once for the person, once in each episode
    expect_identical(rv(ep, "supply_days", list(event = "members")), c(60L, 60L), info = b)
    expect_identical(rv(pp, "supply_days", list(event = "members")), 90L, info = b)
    expect_identical(rv(mx, "supply_days", list(event = "members")), c(60L, 90L), info = b)
    # missing association metadata fails closed
    bare <- .ws5_literal_roster(ctx, data.frame(cand_key = 1L, subject_id = 3301, cohort_start_date = "2020-01-01",
                                                cohort_end_date = "2020-03-31"), name = "cand_bare", population = "ep")
    expect_error(rv(bare, "covered_days", list(interval = "m")), "index_source/index_key")
  }
})

test_that("reducer fragments are named by their consumer only (A-20 (e))", {
  h <- .ws5_iv_backends$sqlite$handle
  ctx <- .ws5_iv_ctx(h, list(d = list(source = list(event = "drug"))), populations = list(t = list(kind = "criteria")))
  cand <- .ws5_candidates(ctx)
  expect_error(.intervalReducerValues(ctx, "covered_days", cand, "candidate", source = list(event = "drug")),
               "named by its consumer")
  expect_error(.intervalReducerValues(ctx, "covered_days", cand, "candidate", source = list(event = "drug"),
                                      names = list(bounds = "b_1")), "named by its consumer")
  v <- .intervalReducerValues(ctx, "covered_days", cand, "candidate", source = list(event = "drug"),
                              names = list(bounds = "str_o_1_b", stream = "str_o_1_s"))
  expect_identical(nrow(v), 10L)
  expect_s3_class(.fragmentGet(ctx, "str_o_1_s"), "dsomop_fragment")
})

test_that("compiled interval SQL renders for every dialect family", {
  bp <- .buildBlueprint(.ws5_iv_backends$sqlite$handle)
  plan <- .ws5_plan(events = list(drug = .ws5_drug_event(c(.WS5$DRUG_A, .WS5$DRUG_AB))),
                    intervals = list(
                      era = list(source = list(event = "drug"), gap = 30, persistence = 7, min_days = 3,
                                 by = "ingredient", fallback = "days_supply"),
                      pt = list(source = list(event = "drug"), gap = 0, end = "start", by = "concept")))
  dbms <- c(postgresql = "postgresql", sqlite = "sqlite", duckdb = "duckdb", mysql = "mysql",
            sqlserver = "sql server", oracle = "oracle", redshift = "redshift", bigquery = "bigquery",
            snowflake = "snowflake", spark = "spark")
  expect_snapshot({
    for (family in names(dbms)) {
      fh <- new.env(parent = emptyenv())
      fh$dbms <- dbms[[family]]
      fh$target_dialect <- .resolve_target_dialect(dbms[[family]])
      fh$blueprint <- bp
      fh$cdm_schema <- if (identical(family, "sqlite")) NULL else "cdm"
      fh$vocab_schema <- fh$cdm_schema
      fh$results_schema <- NULL
      fh$temp_tables <- character(0)
      ctx <- .ws4_ctx(fh, plan)
      cat("--", family, "\n")
      for (i in c("era", "pt")) {
        iv <- .compileIntervalByName(ctx, i)
        cat(.buildStatement(ctx, iv$course, dialect = ctx), "\n")
        cat(.buildStatement(ctx, iv$recorded, dialect = ctx), "\n")
      }
    }
  })
})

test_that("the executable WS-5 interval tests ran on every configured backend", {
  .ws5_expect_backends(.ws5_iv_backends, "ws5 interval")
})
