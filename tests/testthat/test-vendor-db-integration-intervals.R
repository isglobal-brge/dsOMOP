# WS-5 in the vendor CI (design-2.10.3 sections 9 and 10.5): eras with the
# 1-PRECEDING window frame, the stockpile materialisation in a session
# temporary table, coverage policies and source anchors on the configured live
# servers (PostgreSQL, MySQL, MariaDB). The release workflow runs the files
# matching "vendor-db-integration"; the full WS-5 suites (test-compile-interval.R,
# test-compile-anchor.R, test-compile-coverage.R, test-ws5-acceptance.R) run
# the same fixture on every backend the test pod configures.

.ws5_vendor_hosts <- c(postgresql = "DSOMOP_TEST_POSTGRES_HOST", mysql = "DSOMOP_TEST_MYSQL_HOST",
                       mariadb = "DSOMOP_TEST_MARIADB_HOST")

test_that("WS-5 eras, stockpiles, coverage and anchors execute on the configured vendor servers", {
  configured <- names(.ws5_vendor_hosts)[nzchar(Sys.getenv(.ws5_vendor_hosts))]
  skip_if(length(configured) == 0L,
          "set DSOMOP_TEST_<DBMS>_HOST to run the WS-5 compilers on a live server")
  bk <- .ws5_backends(vendors = configured)
  on.exit(.ws4_cleanup(bk), add = TRUE)
  expect_true(all(configured %in% names(bk)))
  for (b in setdiff(names(bk), "sqlite")) {
    h <- bk[[b]]$handle
    profile <- .sqlDialectProfile(h, probe = TRUE)
    expect_true(isTRUE(profile$window_frames), info = b)
    cat("\n[ws5 vendor]", b, profile$runtime %||% "unknown runtime", "\n")
    W <- .WS5
    last_end <- list(source = list(event = "a"), which = "last", field = "end")
    first_a <- list(source = list(event = "a"), which = "first")
    cr <- function(...) list(kind = "criteria", coverage = list(...))
    ctx <- .ws4_ctx(h, .ws5_plan(
      events = list(drug = .ws5_drug_event(),
                    mix = .ws5_drug_event(c(W$DRUG_A, W$DRUG_AB, W$DRUG_X)),
                    metf = list(table = "drug_exposure", concept_set = list(concepts = list(1503297)),
                                end = list(fallback = "days_supply")),
                    a = list(table = "condition_occurrence", concept_set = list(concepts = list(W$ANCHOR_A))),
                    bb = list(table = "measurement", concept_set = list(concepts = list(W$ANCHOR_B)))),
      intervals = list(e30 = list(source = list(event = "drug"), gap = 30),
                       e0 = list(source = list(event = "drug"), gap = 0),
                       ing = list(source = list(event = "mix"), gap = 0, by = "ingredient"),
                       stock = list(source = list(event = "metf"), gap = 30, stockpile = TRUE)),
      populations = list(t = .ws5_anchor_population(list(last_end, first_a)),
                         u = cr(policy = "unique"), g7 = cr(policy = "union", gap = 7L),
                         ue = cr(policy = "unique", on_ambiguous = "error"),
                         cv = cr(policy = "union", gap = 30L))))
    on.exit(.compileReleaseTempTables(ctx), add = TRUE)
    course <- function(i, p) {
      r <- .ws5_rows(ctx, .compileIntervalByName(ctx, i)$course, c("person_id", "group_id", "start_date"))
      r[r$person_id == p, , drop = FALSE]
    }
    # the counterexample and the running maximum
    c1 <- course("e30", 3001)
    expect_identical(c(nrow(c1), c1$start_date, c1$end_date), c("1", "2020-01-01", "2020-01-11"), info = b)
    expect_identical(nrow(course("e0", 3002)), 1L, info = b)
    expect_identical(course("e0", 3003)$era_key, c("1:1", "1:2"), info = b)
    expect_identical(sort(course("ing", 3009)$era_key),
                     sort(paste0(c(W$ING_A, W$ING_B, W$DRUG_X), ":1")), info = b)
    # stockpiling: recorded 44 vs effective 60 days (reach person 1004)
    st <- .compileIntervalByName(ctx, "stock")
    rec <- .ws5_rows(ctx, st$recorded, c("person_id", "start_date"))
    eff <- .ws5_rows(ctx, st$effective, c("person_id", "start_date"))
    win <- data.frame(key = 1004, lo = as.Date(NA), hi = as.Date(NA))
    span <- function(r) .coveredDays(data.frame(key = 1004, start = r$start_date[r$person_id == 1004],
                                                end = r$end_date[r$person_id == 1004]), win)
    expect_identical(c(span(rec), span(eff)), c(44L, 60L), info = b)
    # coverage: ambiguous, missing and the gap-index case
    cand <- .ws5_candidates(ctx)
    k <- function(cov, key) {
      r <- .ws5_rows(ctx, cov$coverage, "row_key")
      r[r$row_key == key, , drop = FALSE]
    }
    pcov <- function(p) .compileCoverage(ctx, p, ctx$ir$populations[[p]]$coverage, cand, "candidate")
    uni <- pcov("u")
    un7 <- pcov("g7")
    expect_identical(c(k(uni, 1)$op_count, k(uni, 3)$op_count, k(un7, 3)$op_count), c(2, 0, 0), info = b)
    expect_identical(k(un7, 2)$component_id, 2, info = b)
    .ws4_expect_rejection(.coverageAssertUnambiguous(ctx, pcov("ue"), "ue"), code = "coverage_ambiguous")
    # anchors: the tie rule and a missing anchor
    f <- .compileAnchor(ctx, last_end, cand, "candidate")
    ad <- .ws5_rows(ctx, f, "row_key")
    expect_identical(ad$anchor_date[ad$row_key == 6], "2020-03-20", info = b)
    expect_false(7 %in% ad$row_key)
    v <- .intervalReducerValues(ctx, "covered_days", cand, "candidate", source = list(event = "bb"),
                                window = list(start = 0L, end = 40L, anchor = first_a))
    expect_identical(v$value[v$row_key == 6], 1L, info = b)
    expect_true(is.na(v$value[v$row_key == 7]), info = b)
    # the PDC pin on the union-coverage roster (reach person 1003: 4 / 4)
    cov <- pcov("cv")
    roster <- .ws5_roster(ctx, cand, cov, pop = "cv")
    w10 <- list(start = 1L, end = 10L, anchor = "index")
    pdc <- .intervalReducerValues(ctx, "covered_days", roster, "final", source = list(interval = "stock"),
                                  window = w10, coverage = "effective_supply", observed_only = TRUE, cov = cov)
    od <- .intervalReducerValues(ctx, "observed_days", roster, "final", window = w10, cov = cov)
    expect_identical(c(pdc$value[pdc$row_key == 10], od$value[od$row_key == 10]), c(4L, 4L), info = b)
  }
})
