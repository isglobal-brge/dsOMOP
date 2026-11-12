# The reducer contract of design-2.10.3 section 2.7 (WS-7): every reducer,
# its ordering and selection, NULL / missing handling, empty and all-missing
# inputs, output types and suffixes, the Kleene any / all truth table (all
# over an empty window stays NA), undefined windows, unavailable sources,
# derived / raw / population columns and the collapse policies -- executed on
# every backend available over the WS-7 scenario rows (helper-ws7.R).

.ws7_red_backends <- .ws7_scenario_backends()
withr::defer(.ws4_cleanup(.ws7_red_backends), testthat::teardown_env())

.rd <- function(fn, source = "lab", ...) {
  list(source = list(event = source), reducer = c(list(fn = fn), list(...)))
}

test_that("pure kernels: Kleene any / all, mode ties, slope, months, empty values", {
  expect_identical(.reduceKleeneAny(c(1L, NA, 0L)), 1L)
  expect_identical(.reduceKleeneAny(c(0L, NA)), NA_integer_)
  expect_identical(.reduceKleeneAny(c(0L, 0L)), 0L)
  expect_identical(.reduceKleeneAny(integer(0)), 0L)
  expect_identical(.reduceKleeneAll(c(1L, 1L)), 1L)
  expect_identical(.reduceKleeneAll(c(1L, NA)), NA_integer_)
  expect_identical(.reduceKleeneAll(c(0L, NA)), 0L)
  expect_identical(.reduceKleeneAll(integer(0)), NA_integer_)
  expect_identical(.reduceMode(c(3, 1, 3, 1, NA)), 1)
  expect_identical(.reduceMode(c(NA, NA)), NA)
  d <- as.Date(c("2020-01-01", "2020-01-03", "2020-01-05"))
  expect_equal(.reduceSlope(d, c(1, 2, 3)), 0.5)
  expect_identical(.reduceSlope(as.Date(c("2020-01-01", "2020-01-01")), c(1, 2)), NA_real_)
  expect_identical(.reduceMonths(as.Date("2020-01-20"), as.Date("2020-02-01")), 0L)
  expect_identical(.reduceMonths(as.Date("2020-01-20"), as.Date("2020-02-20")), 1L)
  expect_identical(.reduceEmptyValue("count"), 0L)
  expect_identical(.reduceEmptyValue("sum"), 0)
  expect_true(is.na(.reduceEmptyValue("mean")))
  expect_true(is.na(.reduceEmptyValue("all")))
})

test_that("every reducer of the contract table on the scenario rows", {
  W <- .WS7
  vars <- list(
    binary = .rd("binary"), count = .rd("count"),
    nd_value = .rd("n_distinct", value = "value"),
    first_skip = .rd("first_value"), last_skip = .rd("last_value"),
    first_keep5 = .rd("first_value", missing = "keep", window = list(start = -5L, end = -5L, anchor = "index")),
    first_skip5 = .rd("first_value", window = list(start = -5L, end = -5L, anchor = "index")),
    last_keep5 = .rd("last_value", missing = "keep", window = list(start = -5L, end = -5L, anchor = "index")),
    nth = .rd("nth_value", n = list(1L, 2L, 3L, 6L)),
    closest_either = .rd("closest_value"), closest_before = .rd("closest_value", side = "before"),
    closest_after = .rd("closest_value", side = "after"),
    mean = .rd("mean"), min = .rd("min"), max = .rd("max"), sum = .rd("sum"), sd = .rd("sd"),
    cv = .rd("cv"), median = .rd("median"), q25 = .rd("quantile", p = 0.25), mode = .rd("mode", value = "value"),
    mode_concept = .rd("mode"), slope = .rd("slope"),
    any_gt4 = .rd("any", predicate = list(var = "value", op = ">", value = 4)),
    all_gt4 = .rd("all", predicate = list(var = "value", op = ">", value = 4)),
    first_date = .rd("first_date"), last_date = .rd("last_date"),
    since = .rd("days_since", reference = list(date = "2020-01-07")),
    until = .rd("days_until", reference = list(date = "2020-01-07")),
    since_days = .rd("time_since", reference_date = "2020-02-01"),
    since_months = .rd("time_since", reference_date = "2020-02-01", unit = "month"),
    gap_max = .rd("gap_max"), gap_mean = .rd("gap_mean"),
    hi = .rd("abnormal_high"), lo = .rd("abnormal_low"),
    dur_sum = .rd("duration_sum", source = "drug"),
    dur_mean = .rd("drug_duration", source = "drug"), dur_max = .rd("drug_duration", source = "drug", agg = "max"),
    dx_rows = .rd("count", source = "dx"), dx_days = .rd("count", source = "dx", distinct = "day"),
    dx_visits = .rd("count", source = "dx", distinct = "visit"),
    dx_concepts = .rd("count", source = "dx", distinct = "concept"),
    dx_nd = .rd("n_distinct", source = "dx"),
    covered = .rd("covered_days", source = "drug"))
  plan <- .ws7_scenario_plan(vars)
  for (b in names(.ws7_red_backends)) {
    f <- .ws7_scenario_run(.ws7_red_backends[[b]]$handle, plan)$frame
    v <- function(pid, col) .ws7_value(f, pid, col)
    info <- b
    # 7001: values 5, NULL, 7, 3, 9 on 01-01, 01-05 (x2), 01-10, 01-20
    expect_identical(v(7001, "binary"), 1L, info = info)
    expect_identical(v(7001, "count"), 5L, info = info)
    expect_identical(v(7001, "nd_value"), 4L, info = info)
    expect_identical(v(7001, "first_skip"), 5, info = info)
    expect_identical(v(7001, "last_skip"), 9, info = info)
    expect_true(is.na(v(7001, "first_keep5")), info = info)        # the NULL row selected first
    expect_identical(v(7001, "first_skip5"), 7, info = info)        # NULL skipped before selection
    expect_identical(v(7001, "last_keep5"), 7, info = info)
    expect_identical(c(v(7001, "nth_1"), v(7001, "nth_2"), v(7001, "nth_3")), c(5, 7, 3), info = info)
    expect_true(is.na(v(7001, "nth_6")), info = info)               # beyond count
    expect_identical(v(7001, "closest_either"), 3, info = info)
    expect_identical(c(v(7004, "closest_either"), v(7004, "closest_before"), v(7004, "closest_after")),
                     c(1, 1, 2), info = info)                       # equidistance: first by order
    expect_equal(v(7001, "mean"), 6, info = info)
    expect_identical(c(v(7001, "min"), v(7001, "max"), v(7001, "sum")), c(3, 9, 24), info = info)
    expect_equal(v(7001, "sd"), stats::sd(c(5, 7, 3, 9)), info = info)
    expect_equal(v(7001, "cv"), stats::sd(c(5, 7, 3, 9)) / 6 * 100, info = info)
    expect_equal(v(7001, "median"), 6, info = info)
    expect_equal(v(7001, "q25"), as.numeric(stats::quantile(c(5, 7, 3, 9), 0.25, type = 7)), info = info)
    expect_identical(v(7001, "mode"), 3, info = info)
    expect_identical(v(7001, "mode_concept"), W$LAB, info = info)
    dd <- as.numeric(as.Date(c("2020-01-01", "2020-01-05", "2020-01-10", "2020-01-20")))
    expect_equal(v(7001, "slope"), unname(stats::coef(stats::lm(c(5, 7, 3, 9) ~ dd))[2]), info = info)
    expect_identical(c(v(7001, "any_gt4"), v(7001, "all_gt4")), c(1L, 0L), info = info)
    expect_identical(c(v(7001, "first_date"), v(7001, "last_date")), c(-9L, 10L), info = info)
    expect_identical(c(v(7001, "since"), v(7001, "until")), c(2L, 3L), info = info)
    expect_identical(c(v(7001, "since_days"), v(7001, "since_months")), c(12L, 0L), info = info)
    expect_identical(v(7001, "gap_max"), 10, info = info)
    expect_equal(v(7001, "gap_mean"), 19 / 3, info = info)
    expect_identical(c(v(7001, "hi"), v(7001, "lo")), c(1L, 1L), info = info)
    expect_identical(v(7001, "dur_sum"), 19L, info = info)          # overlaps counted twice (A-12)
    expect_identical(v(7001, "covered"), 15L, info = info)          # the union
    expect_equal(c(v(7001, "dur_mean"), v(7001, "dur_max")), c(9.5, 10), info = info)
    expect_identical(c(v(7001, "dx_rows"), v(7001, "dx_days"), v(7001, "dx_visits"), v(7001, "dx_concepts"),
                       v(7001, "dx_nd")), c(5L, 3L, 2L, 2L, 2L), info = info)
    # 7002: no rows -- declared zeros, NA otherwise
    zero <- c("binary", "count", "nd_value", "sum", "hi", "lo", "any_gt4", "dx_rows", "dx_days", "covered")
    for (c in zero) expect_equal(v(7002, c), 0, info = paste(info, c))
    for (c in c("first_skip", "mean", "sd", "median", "closest_either", "first_date", "since", "gap_max",
                "dur_sum", "dur_mean", "all_gt4", "mode")) {
      expect_true(is.na(v(7002, c)), info = paste(info, c))
    }
    # 7003: rows whose values are all NULL
    expect_identical(c(v(7003, "binary"), v(7003, "count"), v(7003, "nd_value")), c(1L, 2L, 0L), info = info)
    expect_identical(v(7003, "sum"), 0, info = info)
    for (c in c("first_skip", "last_skip", "mean", "sd", "any_gt4", "all_gt4")) {
      expect_true(is.na(v(7003, c)), info = paste(info, c))
    }
    # the Kleene table over non-empty windows
    expect_identical(c(v(7005, "any_gt4"), v(7005, "all_gt4")), c(1L, 1L), info = info)
    expect_identical(c(v(7006, "any_gt4"), v(7006, "all_gt4")), c(1L, NA_integer_), info = info)
    expect_identical(c(v(7007, "any_gt4"), v(7007, "all_gt4")), c(NA_integer_, 0L), info = info)
    # output types
    expect_type(f$count, "integer")
    expect_type(f$mean, "double")
    expect_type(f$any_gt4, "integer")
  }
})

test_that("an undefined window is NA for every reducer; an unavailable source is NA everywhere", {
  vars <- list(
    b_anchored = .rd("binary", window = list(start = 0L, end = 30L,
                                              anchor = list(source = list(event = "dx")))),
    c_anchored = .rd("count", window = list(start = 0L, end = 30L,
                                             anchor = list(source = list(event = "dx")))),
    notes = .rd("count", source = "notes"), notes_any = .rd("binary", source = "notes"))
  events <- list(notes = list(table = "note", on_missing_table = "unavailable"))
  plan <- .ws7_scenario_plan(vars, events = events)
  for (b in names(.ws7_red_backends)) {
    res <- .ws7_scenario_run(.ws7_red_backends[[b]]$handle, plan)
    f <- res$frame
    # 7001 has dx rows from 2020-01-02: lab rows in [01-02, 02-01] -> binary 1, count 4
    expect_identical(c(.ws7_value(f, 7001, "b_anchored"), .ws7_value(f, 7001, "c_anchored")), c(1L, 4L), info = b)
    # 7002 has no dx row: the anchor is missing, so even binary / count are NA
    expect_true(is.na(.ws7_value(f, 7002, "b_anchored")), info = b)
    expect_true(is.na(.ws7_value(f, 7002, "c_anchored")), info = b)
    expect_true(all(is.na(f$notes)) && all(is.na(f$notes_any)), info = b)
    expect_identical(res$compiled$status, "unavailable", info = b)
  }
})

test_that("derived, raw and population columns; native scores at the index (A-2)", {
  vars <- list(
    age = list(derived = list(kind = "age")),
    age_band = list(derived = list(kind = "age", bands = TRUE, age_breaks = list(0L, 18L, 19L, 65L))),
    age_2030 = list(derived = list(kind = "age", reference = "today", reference_date = "2030-06-01")),
    sex = list(derived = list(kind = "sex_mf")),
    byb = list(derived = list(kind = "birth_year_band", width = 10L)),
    nop = list(derived = list(kind = "n_observation_periods")),
    dur = list(derived = list(kind = "obs_duration")),
    prior = list(derived = list(kind = "prior_obs", reference_date = "2020-01-10")),
    follow = list(derived = list(kind = "followup", reference_date = "2020-01-10")),
    observed = list(derived = list(kind = "observed_days", window = list(start = -10L, end = 0L, anchor = "index"))),
    yob = list(raw = list(table = "person", column = "year_of_birth")),
    gender = list(raw = list(table = "person", column = "gender_concept_id")),
    src = list(population_column = list(name = "index_source")))
  plan <- .ws7_scenario_plan(vars)
  plan$options <- list(translate_concepts = FALSE)
  for (b in names(.ws7_red_backends)) {
    f <- .ws7_scenario_run(.ws7_red_backends[[b]]$handle, plan)$frame
    v <- function(pid, col) .ws7_value(f, pid, col)
    expect_identical(v(7001, "age"), 50L, info = b)
    expect_true(is.na(v(7008, "age")), info = b)                    # no year of birth
    expect_identical(v(7007, "age_band"), "19-64", info = b)        # off-grid breaks, no width floor
    expect_identical(v(7001, "age_band"), "19-64", info = b)
    expect_identical(v(7001, "age_2030"), 60L, info = b)
    expect_identical(c(v(7001, "sex"), v(7002, "sex")), c("M", "F"), info = b)
    expect_identical(v(7004, "byb"), 1960L, info = b)
    expect_identical(v(7001, "nop"), 1L, info = b)
    expect_identical(v(7001, "dur"), as.integer(as.Date("2030-12-31") - as.Date("2015-01-01")), info = b)
    expect_identical(v(7001, "prior"), as.integer(as.Date("2020-01-10") - as.Date("2015-01-01")), info = b)
    expect_identical(v(7001, "follow"), as.integer(as.Date("2030-12-31") - as.Date("2020-01-10")), info = b)
    expect_identical(v(7001, "observed"), 11L, info = b)
    expect_identical(v(7004, "yob"), 1960L, info = b)               # class-2 birth component released
    expect_identical(v(7001, "gender"), 8507L, info = b)
    expect_identical(unique(f$src), "cohort:1", info = b)
  }
  # translate_concepts (default TRUE) names concept-valued raw and derived columns
  plan2 <- .ws7_scenario_plan(list(gender = list(raw = list(table = "person", column = "gender_concept_id"))))
  f <- .ws7_scenario_run(.ws7_red_backends$sqlite$handle, plan2)$frame
  expect_type(f$gender, "character")
  # class-1 and identifier columns are refused
  bad <- .ws7_scenario_plan(list(psv = list(raw = list(table = "person", column = "person_source_value"))))
  .ws4_expect_rejection(.ws7_scenario_run(.ws7_red_backends$sqlite$handle, bad), pattern = "class-1")
})

test_that("native scores count conditions on or before the index, v1 counts all time (A-2)", {
  path <- testthat::test_path("..", "..", "inst", "plans", "acceptance", "score-reference.json")
  ref <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  cs <- ref$cases[ref$cases$cohort_definition_id == 1002 & ref$cases$reference == "index", ]
  vars <- list(charlson = list(derived = list(kind = "charlson")),
               chads2 = list(derived = list(kind = "chads2")),
               chadsvasc = list(derived = list(kind = "chadsvasc")))
  plan <- .ws7_scenario_plan(vars)
  for (b in names(.ws7_red_backends)) {
    h <- .ws7_red_backends[[b]]$handle
    fr <- .ws7_cohort_frame(h, 1002L)
    fr <- fr[order(fr$subject_id, fr$cohort_start_date), , drop = FALSE]
    fr <- fr[!duplicated(fr$subject_id), , drop = FALSE]
    res <- .ws7_scenario_run(h, plan, frame = fr)
    for (kind in c("charlson", "chads2", "chadsvasc")) {
      want <- as.integer(cs$scores[[kind]]$native[[1]])
      pids <- as.numeric(cs$scores[[kind]]$person_id[[1]])
      got <- res$frame[[kind]][match(pids, res$frame$pid)]
      keep <- !is.na(got)
      expect_true(mean(keep) > 0.9, info = paste(b, kind))           # covered episodes
      expect_identical(as.integer(got[keep]), want[keep], info = paste(b, kind))
    }
  }
})

test_that("collapse to person grain: first_episode, last_episode and any", {
  frame <- rbind(.ws7_scenario_frame(c(7001, 7005)),
                 .ws7_scenario_frame(c(7001, 7005), index = "2020-01-21", end = "2020-12-31"))
  vars <- list(n = .rd("count", window = list(start = -30L, end = 0L, anchor = "index")),
               has = .rd("binary", window = list(start = 0L, end = 0L, anchor = "index")),
               last = .rd("last_value", window = list(start = -30L, end = 0L, anchor = "index")))
  for (collapse in c("first_episode", "last_episode", "any")) {
    plan <- .ws7_scenario_plan(vars, output = list(grain = "person", collapse = collapse))
    for (b in names(.ws7_red_backends)) {
      f <- .ws7_scenario_run(.ws7_red_backends[[b]]$handle, plan, frame = frame)$frame
      expect_identical(names(f)[1:4], c("person_id", "n", "has", "last"), info = b)
      expect_identical(nrow(f), 2L, info = b)
      v <- function(col) f[[col]][match(7001, as.numeric(f$person_id))]
      # 7001 episodes: index 01-10 (rows 01-01..01-10: n 4, has 1, last 3) and
      # 01-21 (rows 12-22..01-21: n 5, has 0, last 9)
      want <- switch(collapse, first_episode = list(4L, 1L, 3), last_episode = list(5L, 0L, 9),
                     any = list(5L, 1L, 3))
      expect_identical(list(v("n"), v("has"), v("last")), want, info = paste(b, collapse))
    }
  }
})

test_that("output time_window and temporal selection restrict every event reducer", {
  vars <- list(n = .rd("count"), first = .rd("first_value"))
  plan <- .ws7_scenario_plan(vars, output = list(options = list(
    time_window = list(start = list(anchor = list(date = "2020-01-05"), offset = 0L),
                       end = list(anchor = list(date = "2020-01-12"), offset = 0L), anchor = "index"))))
  f <- .ws7_scenario_run(.ws7_red_backends$sqlite$handle, plan)$frame
  expect_identical(c(.ws7_value(f, 7001, "n"), .ws7_value(f, 7001, "first")), c(3L, 7))
  plan <- .ws7_scenario_plan(vars, output = list(options = list(temporal = list(event_select = "last"))))
  f <- .ws7_scenario_run(.ws7_red_backends$sqlite$handle, plan)$frame
  expect_identical(c(.ws7_value(f, 7001, "n"), .ws7_value(f, 7001, "first")), c(1L, 9))
  plan <- .ws7_scenario_plan(vars, output = list(options = list(temporal = list(min_gap = 5L))))
  f <- .ws7_scenario_run(.ws7_red_backends$sqlite$handle, plan)$frame
  # kept: 01-01, 01-10 (>= 5 days after 01-01? 9 yes), 01-20: the 01-05 rows are within 5 days
  expect_identical(.ws7_value(f, 7001, "n"), 3L)
})

test_that("bounded realisation: one row over dsomop.max_memory_rows is the typed reducer_row_cap refusal", {
  plan <- .ws7_scenario_plan(list(n = .rd("count")))
  h <- .ws7_red_backends$sqlite$handle
  withr::local_options(list(dsomop.max_memory_rows = 3L))
  err <- tryCatch(.ws7_scenario_run(h, plan), dsomop_plan_rejection = function(e) e)
  expect_s3_class(err, "dsomop_plan_rejection")
  expect_identical(err$code, "validation_error")
  expect_identical(err$reason, "reducer_row_cap")
  expect_false(grepl("[0-9]", sub("dsomop.max_memory_rows", "", conditionMessage(err), fixed = TRUE)))
})

test_that("fragment identities: the rd and ox families are declared and allocated per context", {
  plan <- .ws7_scenario_plan(list(n = .rd("count"), a = list(derived = list(kind = "age"))))
  ir <- .irNormalize(.irJsonForm(plan))
  ids <- .reduceFragmentIdentities(ir)
  keys <- vapply(ids, .fragmentIdentityKey, "")
  expect_true(.fragmentIdentityKey(.fragmentIdentity("rd", "o", "s_n")) %in% keys)
  expect_true(.fragmentIdentityKey(.fragmentIdentity("rd", "o", "pp")) %in% keys)
  expect_true(all(c("rd", "ox") %in% names(.fragmentIdentityGenerators())))
  # distinct (kind, variable) pairs never share an identity
  plan2 <- .ws7_scenario_plan(list(s_x = .rd("count"), x = .rd("binary")))
  ir2 <- .irNormalize(.irJsonForm(plan2))
  keys2 <- vapply(.reduceFragmentIdentities(ir2), .fragmentIdentityKey, "")
  expect_false(anyDuplicated(keys2) > 0L)
})

test_that("lossless row and event ordering survives adjacent BIGINT keys and date modes", {
  keys <- c("9007199254740993", "10", "9007199254740992", "2", "-10", "-2", NA)
  expect_identical(keys[order(.reduceKeyOrder(keys))],
                   c("-10", "-2", "2", "10", "9007199254740992", "9007199254740993", NA))
  rows <- data.frame(row_key = "1", event_id = c("9007199254740993", "9007199254740992"),
                     start_date = as.Date("2020-01-01"), v = c(9, 2))
  got <- .reduceApply("first_value", rows, "1", list(), c("start_date", "event_id"))
  expect_identical(.reduceCoerce(got[[1L]], "numeric"), 2)
  frame <- data.frame(cohort_row_id = rows$event_id, person_id = "1", value = rows$v)
  expect_identical(.reduceCollapse(frame, "first_episode")$value, 2)
  expect_identical(.reduceCollapse(frame, "last_episode")$value, 9)
  dates <- as.Date(c("2020-01-02", "2020-01-01", "2020-01-02"))
  expect_identical(.reduceCoerce(list(.reduceMode(dates)), "date"), as.Date("2020-01-02"))
  expect_identical(.reduceCoerce(list(), "integer"), integer(0))
  expect_identical(.reduceCoerce(keys, "bigint"), keys)
})

test_that("unavailable reducers retain their result types and still validate effective coverage", {
  vars <- list(n = .rd("count", "notes"), avg = .rd("mean", "notes"),
               mode = .rd("mode", "notes"), day = .rd("mode", "notes", value = "start_date"),
               cov = .rd("covered_days", "notes"), supply = .rd("supply_days", "notes"))
  plan <- .ws7_scenario_plan(vars, events = list(notes = list(table = "note", on_missing_table = "unavailable")))
  for (b in names(.ws7_red_backends)) {
    f <- .ws7_scenario_run(.ws7_red_backends[[b]]$handle, plan)$frame
    expect_true(all(vapply(f[names(vars)], function(x) all(is.na(x)), logical(1))), info = b)
    for (col in c("n", "mode", "cov", "supply")) expect_type(f[[col]], "integer")
    expect_type(f$avg, "double")
    expect_type(f$day, "integer") # Date mode follows the output's relative-date policy
  }
  plan$variables <- list(bad = .rd("covered_days", "notes", coverage = "effective_supply"))
  .ws4_expect_rejection(.ws7_scenario_run(.ws7_red_backends$sqlite$handle, plan),
                        pattern = "stockpiled interval")
})

test_that("mixed origins scope members and course reducers before parent-local stockpiling", {
  vars <- list(n = .rd("count", "members"),
               courses = list(source = list(interval = "mc"), reducer = list(fn = "era_count")),
               duration = list(source = list(interval = "ms"), reducer = list(fn = "era_days")))
  plan <- .ws7_scenario_plan(vars, events = list(
    episodes = list(table = "episode", concept_set = list(concepts = list(32531))),
    members = list(episode_members = list(episode = list(event = "episodes")))))
  plan$intervals <- list(mc = list(source = list(event = "members"), gap = 0L),
                         ms = list(source = list(event = "members"), gap = 0L, stockpile = TRUE))
  plan$populations$t$index$source <- list(any_of = list(list(event = "episodes"), list(event = "drug")))
  frame <- data.frame(subject_id = 3301L, cohort_start_date = as.Date("2020-01-01"),
                       cohort_end_date = as.Date("2020-12-31"),
                       index_source = c("episodes", "episodes", "drug", NA),
                       index_key = c("3301001", "3301002", "other", NA))
  for (b in names(.ws7_red_backends)) {
    got <- .ws7_scenario_run(.ws7_red_backends[[b]]$handle, plan, frame)
    at <- match(c("3301001", "3301002", "other", NA), got$roster$index_key)
    out <- got$frame[match(got$roster$cohort_row_id[at], got$frame$cohort_row_id), ]
    expect_identical(out$n, c(2L, 2L, 3L, 0L), info = b)
    expect_identical(out$courses, c(1L, 2L, 1L, 0L), info = b)
    expect_identical(out$duration, c(60L, 60L, 90L, NA_integer_), info = b)
  }
})

test_that("event and interval reducer streams inherit index-visit restrictions", {
  vars <- list(n = .rd("count", "proc"),
               course = list(source = list(interval = "pi"), reducer = list(fn = "era_count")),
               stock = list(source = list(interval = "ps"), reducer = list(fn = "era_count")))
  plan <- .ws7_scenario_plan(vars, events = list(
    proc = list(table = "procedure_occurrence", concept_set = list(concepts = list(4283095)),
                visit = list(index = TRUE)), vis = list(table = "visit_occurrence")))
  plan$intervals <- list(pi = list(source = list(event = "proc")),
                         ps = list(source = list(event = "proc"), stockpile = TRUE))
  plan$populations$t$index$source <- list(event = "vis")
  frame <- data.frame(subject_id = 1007L, cohort_start_date = as.Date("2020-01-01"),
                       cohort_end_date = as.Date("2020-12-31"), index_source = "vis",
                       index_key = c("700701", "700702", "missing"),
                       index_visit_id = c(700701, 700702, NA))
  for (b in names(.ws7_red_backends)) {
    got <- .ws7_scenario_run(.ws7_red_backends[[b]]$handle, plan, frame)
    at <- match(frame$index_key, got$roster$index_key)
    out <- got$frame[match(got$roster$cohort_row_id[at], got$frame$cohort_row_id), ]
    for (col in names(vars)) expect_identical(out[[col]], c(1L, 0L, 0L), info = paste(b, col))
  }
})

test_that("coverage reducers combine output windows and temporal selection without broadening", {
  vars <- list(coverage = .rd("covered_days", "drug"),
               observed = .rd("covered_days", "drug", observed_only = TRUE),
               supply = .rd("supply_days", "drug", fills = "clipped"))
  plan <- .ws7_scenario_plan(vars, output = list(options = list(
    time_window = list(start = -3L, end = 2L, anchor = "index"),
    temporal = list(event_select = "last"))))
  for (b in names(.ws7_red_backends)) {
    f <- .ws7_scenario_run(.ws7_red_backends[[b]]$handle, plan)$frame
    expect_identical(.ws7_value(f, 7001L, "coverage"), 6L, info = b)
    expect_identical(.ws7_value(f, 7001L, "observed"), 6L, info = b)
    expect_identical(.ws7_value(f, 7001L, "supply"), 0L, info = b)
    expect_identical(.ws7_value(f, 7002L, "coverage"), 0L, info = b)
  }
  plan$outputs$o$options$time_window$anchor <- list(source = list(event = "dx"))
  f <- .ws7_scenario_run(.ws7_red_backends$sqlite$handle, plan)$frame
  expect_identical(.ws7_value(f, 7001L, "coverage"), 4L)
  for (col in names(vars)) expect_identical(.ws7_value(f, 7002L, col), NA_integer_)
})

test_that("the reducer stream cap admits the exact limit and refuses the next realised row", {
  h <- .ws7_red_backends$sqlite$handle
  plan <- .ws7_scenario_plan(list(n = .rd("count")))
  frame <- .ws7_scenario_frame(7001L)
  withr::local_options(list(dsomop.max_memory_rows = 5L))
  expect_identical(.ws7_scenario_run(h, plan, frame)$frame$n, 5L)
  options(dsomop.max_memory_rows = 4L)
  err <- tryCatch(.ws7_scenario_run(h, plan, frame), dsomop_plan_rejection = identity)
  expect_s3_class(err, "dsomop_plan_rejection")
  expect_identical(err$code, "validation_error")
  expect_identical(err$reason, "reducer_row_cap")
  expect_identical(err$field, "/variables/n/reducer")
  expect_false(any(c("rows", "count", "condition", "data") %in% names(err)))
})
