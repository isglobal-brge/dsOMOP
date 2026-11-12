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
                               era1 = list(source = list(event = "drug"), gap = 1)))
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
    on.exit(.compileReleaseTempTables(ctx), add = TRUE)
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
  withr::with_options(list(dsomop.max_stockpile_days = -1), expect_error(.intervalStockpileCap(), "non-negative"))
  withr::with_options(list(dsomop.max_stockpile_days = 30), expect_identical(.intervalStockpileCap(), 30L))
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
    on.exit(.compileReleaseTempTables(ctx), add = TRUE)
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
    .fragmentRegister(ctx, cand)
    cov <- .compileCoverage(ctx, "cv", list(policy = "union", gap = 30), cand, "candidate")
    roster <- .ws5_roster(ctx, cand, cov, pop = "cv")
    val <- function(fn, ...) {
      v <- .intervalReducerValues(ctx, fn, roster, "final", cov = cov, ...)
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
    on.exit(.compileReleaseTempTables(ctx), add = TRUE)
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
