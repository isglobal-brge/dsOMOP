# WS-6 acceptance at the population and roster level (design-2.10.3 sections
# 10.7 and 12.2): the populations of the fifteen acceptance recipes compiled
# by the real population and matching compilers reproduce the hand-computed
# WS-0 oracle (tools/ws0/acceptance_expected.R) -- the episodes each output
# reads, their index keys and arms, and for recipe 10 the complete matched
# roster (sets, cases, controls, weights) -- on every backend available, with
# identical rosters (cohort_row_id included) across backends.

.ws6_acc_backends <- .ws6_backends()
withr::defer(.ws4_cleanup(.ws6_acc_backends), testthat::teardown_env())

# Roster of one acceptance population on one backend.
.ws6_acc_roster <- function(h, recipe, pop) {
  ctx <- .ws4_ctx(h, .ws5_acceptance_plan(recipe))
  on.exit(.compileReleaseTempTables(ctx), add = TRUE)
  .ws6_roster(ctx, pop)
}

# Exact and subset comparisons against an oracle output.
.ws6_acc_expect <- function(got, recipe, output, cols = c("person_id", "index_date"),
                            mode = c("exact", "superset"), info = NULL) {
  mode <- match.arg(mode)
  want <- .ws5_expected(recipe, output)
  want$person_id <- as.numeric(want$person_id)
  wk <- unique(.ws6_key(want, cols))
  gk <- .ws6_key(got, cols)
  if (identical(mode, "exact")) {
    expect_setequal(gk, wk)
    expect_false(anyDuplicated(gk) > 0L, info = info)
  } else {
    expect_true(all(wk %in% gk), info = info)
  }
  invisible(want)
}

test_that("recipes 01, 03, 04, 07, 09, 11, 13: cohort-1 rosters and their children reproduce the oracle", {
  for (b in names(.ws6_acc_backends)) {
    h <- .ws6_acc_backends[[b]]$handle
    c1 <- .ws6_acc_roster(h, "01-characterisation", "c1")
    .ws6_acc_expect(c1, "01-characterisation", "table1", info = b)
    # every episode of cohort 1 uniquely covered: 1001 (two covering periods) is dropped
    expect_false(1001 %in% c1$person_id, info = b)
    expect_true(all(c1$index_source == "cohort:1"), info = b)
    expect_identical(c1$cohort_row_id, as.numeric(seq_len(nrow(c1))), info = b)
    # child populations (B-8): subsets of their parent, episodes inherited
    dm2 <- .ws6_acc_roster(h, "03-distinct-days", "dm2")
    .ws6_acc_expect(dm2, "03-distinct-days", "two_days", info = b)
    expect_true(all(.ws6_key(dm2) %in% .ws6_key(c1)), info = b)
    expect_identical(dm2$index_key, c1$index_key[match(.ws6_key(dm2), .ws6_key(c1))], info = b)
    dm13 <- .ws6_acc_roster(h, "13-parametrised", "dm2")
    .ws6_acc_expect(dm13, "13-parametrised", "two_days", info = b)
    a1c <- .ws6_acc_roster(h, "04-units-censored", "with_a1c")
    .ws6_acc_expect(a1c, "04-units-censored", "labs", info = b)
    # outputs over c1 read only roster episodes
    for (x in list(c("07-monthly-panel", "panel"), c("09-recurrent-events", "recurrent.risk_sets"),
                   c("09-recurrent-events", "recurrent.events"), c("11-incidence-years", "years"))) {
      r <- .ws6_acc_roster(h, x[[1]], "c1")
      expect_identical(r, c1, info = paste(b, x[[1]]))
      .ws6_acc_expect(r, x[[1]], x[[2]], mode = "superset", info = paste(b, x[[1]]))
    }
  }
})

test_that("recipe 02: any_of era arms with first_eligible reproduce the oracle's index and arm", {
  for (b in names(.ws6_acc_backends)) {
    h <- .ws6_acc_backends[[b]]$handle
    r <- .ws6_acc_roster(h, "02-active-comparator", "new_users")
    want <- .ws6_acc_expect(r, "02-active-comparator", "baseline", info = b)
    got_arm <- r$index_source[match(.ws6_key(want), .ws6_key(r))]
    expect_identical(got_arm, want$arm, info = b)
    # 1031 skips its first, under-observed era and is indexed on the eligible one
    expect_identical(r$index_date[r$person_id == 1031], "2020-05-01", info = b)
    # index_end of an interval index is the course end, capped at op_end
    expect_true(all(r$end_date >= r$index_date & r$end_date <= r$op_end), info = b)
    expect_true(all(grepl("^[0-9]+:[0-9]+$", r$index_key)), info = b)
  }
})

test_that("recipes 05 and 14: event-indexed rosters keep every source event with its lossless key", {
  for (b in names(.ws6_acc_backends)) {
    h <- .ws6_acc_backends[[b]]$handle
    mi <- .ws6_acc_roster(h, "05-same-day-same-visit", "mi_all")
    .ws6_acc_expect(mi, "05-same-day-same-visit", "episodes", cols = c("person_id", "index_date", "index_key"),
                    info = b)
    # two same-day MI rows of 1007 are two episodes with distinct index keys
    expect_gte(sum(mi$person_id == 1007 & mi$index_date == "2020-01-01"), 2L)
    onc <- .ws6_acc_roster(h, "14-oncology-episodes", "onc")
    .ws6_acc_expect(onc, "14-oncology-episodes", "episodes", cols = c("person_id", "index_date", "index_key"),
                    info = b)
    first <- .ws6_acc_roster(h, "14-oncology-episodes", "first_line")
    .ws6_acc_expect(first, "14-oncology-episodes", "first_line",
                    cols = c("person_id", "index_date", "index_key"), info = b)
    expect_false(anyDuplicated(first$person_id) > 0L, info = b)
  }
})

test_that("recipes 06, 08, 12, 15: results-cohort and first-eligible rosters under their coverage policies", {
  for (b in names(.ws6_acc_backends)) {
    h <- .ws6_acc_backends[[b]]$handle
    users <- .ws6_acc_roster(h, "06-coverage-persistence", "users")
    .ws6_acc_expect(users, "06-coverage-persistence", "coverage", info = b)
    .ws6_acc_expect(users, "06-coverage-persistence", "persistence", mode = "superset", info = b)
    sccs <- .ws6_acc_roster(h, "08-sccs-matrix", "sccs")
    .ws6_acc_expect(sccs, "08-sccs-matrix", "matrix", mode = "superset", info = b)
    expect_true(all(sccs$index_source == "cohort:1002"), info = b)
    expect_true(all(grepl("^1002:[0-9]+:2019-07-01:2020-06-30$", sccs$index_key)), info = b)
    t2dm <- .ws6_acc_roster(h, "12-circe-baseline", "t2dm_first")
    .ws6_acc_expect(t2dm, "12-circe-baseline", "baseline", info = b)
    expect_false(1030 %in% t2dm$person_id, info = b)
    union <- .ws6_acc_roster(h, "15-costs-adherence", "users")
    .ws6_acc_expect(union, "15-costs-adherence", "adherence", info = b)
    # 1003: union coverage (gap 30) bridges its 6-day gap; unique coverage does not
    expect_identical(union$op_end[union$person_id == 1003], "2030-12-31", info = b)
    expect_identical(users$op_end[users$person_id == 1003], "2020-01-02", info = b)
  }
})

test_that("recipe 10: the matched roster reproduces the oracle sampler exactly", {
  want <- .ws5_expected("10-matching", "cc_table")
  want$person_id <- as.numeric(want$person_id)
  ref <- NULL
  for (b in names(.ws6_acc_backends)) {
    h <- .ws6_acc_backends[[b]]$handle
    r <- .ws6_acc_roster(h, "10-matching", "cc")
    cols <- c("person_id", "index_date", "set_id")
    expect_setequal(.ws6_key(r, cols), .ws6_key(want, cols))
    m <- match(.ws6_key(want, cols), .ws6_key(r, cols))
    expect_equal(r$is_case[m], as.numeric(want$is_case), info = b)
    expect_equal(r$w[m], as.numeric(want$w), info = b)
    # one case per set; the zero-control case 1012 is kept (keep_available)
    expect_true(all(tapply(r$is_case, r$set_id, sum) == 1), info = b)
    expect_identical(sum(r$set_id == r$set_id[r$person_id == 1012 & r$is_case == 1]), 1L, info = b)
    expect_true(all(r$index_source == "match:cc"), info = b)
    expect_true(all(grepl("\\|(case|control)$", r$index_key)), info = b)
    expect_identical(r$index_date, r$end_date, info = b)
    # identical rosters (cohort_row_id, identities, coverage) on every backend
    if (is.null(ref)) ref <- r else expect_identical(r, ref, info = b)
  }
  .ws5_expect_backends(.ws6_acc_backends, "WS-6 acceptance")
})
