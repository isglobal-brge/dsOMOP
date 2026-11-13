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
    .ws5_release(ctx)
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
    v <- .ws5_rv(ctx, "covered_days", cand, "candidate", source = list(event = "bb"),
                                window = list(start = 0L, end = 40L, anchor = first_a))
    expect_identical(v$value[v$row_key == 6], 1L, info = b)
    expect_true(is.na(v$value[v$row_key == 7]), info = b)
    # the PDC pin on the union-coverage roster (reach person 1003: 4 / 4)
    cov <- pcov("cv")
    roster <- .ws5_roster(ctx, cand, cov, pop = "cv")
    w10 <- list(start = 1L, end = 10L, anchor = "index")
    pdc <- .ws5_rv(ctx, "covered_days", roster, "final", source = list(interval = "stock"),
                                  window = w10, coverage = "effective_supply", observed_only = TRUE, cov = cov)
    od <- .ws5_rv(ctx, "observed_days", roster, "final", window = w10, cov = cov)
    expect_identical(c(pdc$value[pdc$row_key == 10], od$value[od$row_key == 10]), c(4L, 4L), info = b)
  }
})

# The number of reference paths from fragment `f` to fragment `target` in the
# statement that hoists f's closure (each textual reference to a dependency in
# a body is one path). This checks the individual relation structure, not
# physical opens: a combined consumer can have several paths while MySQL
# reconstructs reusable rows from one private payload per physical stockpile
# table (amendment A-20 (l)).
.ws5_paths <- function(ctx, f, target) {
  if (identical(f$name, target)) return(1)
  sum(vapply(f$dependencies, function(d) {
    k <- lengths(regmatches(f$select_body, gregexpr(paste0("\\b", d, "\\b"), f$select_body)))
    k * .ws5_paths(ctx, .fragmentGet(ctx, d), target)
  }, numeric(1)))
}

test_that("MySQL 8.4: a stockpile with min_days > 0 reads its session table once per statement (A-20 (l))", {
  skip_if(!nzchar(Sys.getenv("DSOMOP_TEST_MYSQL_HOST")),
          "set DSOMOP_TEST_MYSQL_HOST (and DSOMOP_TEST_MYSQL_PORT) to run the live MySQL case")
  bk <- .ws5_backends(vendors = "mysql")
  on.exit(.ws4_cleanup(bk), add = TRUE)
  expect_true("mysql" %in% names(bk))
  h <- bk$mysql$handle
  runtime <- .sqlDialectProfile(h, probe = TRUE)$runtime %||% ""
  cat("\n[ws5 mysql]", runtime, "\n")
  # MySQL evidence, not MariaDB (which reopens temporary tables)
  expect_match(runtime, "MySQL")
  last_course <- list(source = list(interval = "s44"), which = "last")
  ctx <- .ws4_ctx(h, .ws5_plan(
    events = list(metf = list(table = "drug_exposure", concept_set = list(concepts = list(1503297)),
                              end = list(fallback = "days_supply"))),
    intervals = list(s44 = list(source = list(event = "metf"), gap = 30, stockpile = TRUE, min_days = 44,
                                persistence = 30),
                     s45 = list(source = list(event = "metf"), gap = 30, stockpile = TRUE, min_days = 45)),
    populations = list(t = .ws5_anchor_population(list(last_course), source = "metf"),
                       cv = list(kind = "criteria", coverage = list(policy = "union", gap = 30L)))))
  closed <- FALSE
  on.exit(if (!closed) .planContextClose(ctx), add = TRUE, after = FALSE)
  iv <- .compileIntervalByName(ctx, "s44")
  tname <- .intervalMeta(ctx, "s44")$temp_table
  expect_true(.isScalarString(tname))
  # every relation reaches the session table through exactly one reference
  src <- .intervalMeta(ctx, "s44")$fragments[["effective_source"]]
  expect_identical(attr(.fragmentGet(ctx, src), "mysql_stockpile_source")$table, tname)
  for (rel in c("course", "recorded", "effective")) {
    expect_identical(.ws5_paths(ctx, iv[[rel]], src), 1, info = rel)
  }
  rows <- function(f, order) {
    r <- .ws5_rows(ctx, f, order)
    r[r$person_id == 1004, , drop = FALSE]
  }
  # course, recorded and effective relations, each fetched in its own statement
  course <- rows(iv$course, c("person_id", "start_date"))
  expect_identical(c(course$start_date, course$end_date, course$end_recorded),
                   c("2020-01-01", "2020-03-30", "2020-02-13"))
  expect_identical(rows(iv$recorded, c("person_id", "start_date"))$end_date, c("2020-01-30", "2020-02-13"))
  eff <- rows(iv$effective, c("person_id", "start_date"))
  expect_identical(c(eff$end_date, eff$norm_end), c("2020-01-30", "2020-02-29", "2020-01-30", "2020-02-13"))
  # min_days = 45 drops the era from all three relations
  s45 <- .compileIntervalByName(ctx, "s45")
  expect_identical(vapply(s45, function(f) nrow(rows(f, "person_id")), integer(1)),
                   c(course = 0L, recorded = 0L, effective = 0L))
  # downstream consumers reusing them: covered days of the effective and the
  # recorded relation, a window anchored on the stockpiled course, and the PDC
  # pair over a union-coverage roster
  cand <- .ws5_literal_roster(ctx, data.frame(cand_key = 1:2, subject_id = c(1004, 3202),
                                              cohort_start_date = "2019-12-31", cohort_end_date = "2020-12-31"))
  w90 <- list(start = 1L, end = 90L, anchor = "index")
  cov_rec <- .ws5_rv(ctx, "covered_days", cand, "candidate", source = list(interval = "s44"), window = w90)
  cov_eff <- .ws5_rv(ctx, "covered_days", cand, "candidate", source = list(interval = "s44"), window = w90,
                     coverage = "effective_supply")
  expect_identical(c(cov_rec$value, cov_eff$value), c(44L, 0L, 60L, 0L))
  anc <- .compileAnchor(ctx, last_course, cand, "candidate")
  expect_identical(.ws5_paths(ctx, anc, src), 1)
  a <- .ws5_rows(ctx, anc, "row_key")
  expect_identical(paste(a$row_key, a$anchor_date), "1 2020-01-01")
  from_course <- .ws5_rv(ctx, "supply_days", cand, "candidate", source = list(event = "metf"),
                         window = list(start = 0L, end = 30L, anchor = last_course))
  expect_identical(from_course$value, c(60L, NA))
  # One downstream statement uses the stockpiled course as its anchor and the
  # same interval's effective rows as its stream. Its shared effective-source
  # table is captured once by MySQL despite the two consumer branches.
  anchored <- .ws5_rv(ctx, "covered_days", cand, "candidate", source = list(interval = "s44"),
                      coverage = "effective_supply",
                      window = list(start = 0L, end = 59L, anchor = last_course))
  expect_identical(anchored$value, c(60L, NA_integer_))
  combined_body <- paste0(
    "SELECT c.person_id, c.end_recorded, COUNT(*) AS fills, MAX(e.end_date) AS effective_end ",
    "FROM ", iv$course$name, " c INNER JOIN ", iv$recorded$name,
    " r ON r.person_id = c.person_id AND r.group_id = c.group_id AND r.era_no = c.era_no ",
    "INNER JOIN ", iv$effective$name,
    " e ON e.person_id = r.person_id AND e.group_id = r.group_id AND e.event_id = r.event_id ",
    "WHERE c.person_id = 1004 GROUP BY c.person_id, c.end_recorded")
  combined_fragment <- .fragment("ws5_combined", "stream", "row", combined_body,
    columns = list(person_id = .fragmentColumn("bigint", FALSE, "person_key"),
                   end_recorded = .fragmentColumn("date", FALSE, "end_recorded"),
                   fills = .fragmentColumn("integer", FALSE, "private"),
                   effective_end = .fragmentColumn("date", FALSE, "end")),
    dependencies = c(iv$course$name, iv$recorded$name, iv$effective$name),
    person_lineage = list(person_col = "person_id", via_roster = NULL), private_columns = "fills")
  combined_sql <- .buildStatement(ctx, combined_fragment, dialect = h)
  expect_identical(lengths(regmatches(combined_sql, gregexpr(tname, combined_sql, fixed = TRUE))), 1L)
  combined <- .executeQuery(h, combined_sql)
  expect_identical(as.integer(combined$fills), 2L)
  expect_identical(as.character(combined$end_recorded), "2020-02-13")
  expect_identical(as.character(combined$effective_end), "2020-02-29")
  # 1004: effective [01-01, 02-29] inside one period; 1003: its one fill is a
  # 10-day era that min_days = 44 drops, observed 01-01..01-02 and 01-09..01-10
  ccand <- .ws5_literal_roster(ctx, data.frame(cand_key = 1:2, subject_id = c(1004, 1003),
                                               cohort_start_date = "2019-12-31", cohort_end_date = "2020-12-31"),
                               name = "cand_cv", population = "cv")
  cov <- .compileCoverage(ctx, "cv", ctx$ir$populations$cv$coverage, ccand, "candidate")
  roster <- .ws5_roster(ctx, ccand, cov, pop = "cv")
  w10 <- list(start = 1L, end = 10L, anchor = "index")
  pdc <- .ws5_rv(ctx, "covered_days", roster, "final", source = list(interval = "s44"), window = w10,
                 coverage = "effective_supply", observed_only = TRUE, cov = cov)
  od <- .ws5_rv(ctx, "observed_days", roster, "final", window = w10, cov = cov)
  expect_identical(pdc$value[order(pdc$row_key)], c(10L, 0L))
  expect_identical(od$value[order(od$row_key)], c(10L, 4L))
  # cleanup: closing the context drops the session table (A-20 (k))
  .planContextClose(ctx)
  closed <- TRUE
  expect_length(ctx$compile_temp_tables, 0L)
  expect_false(tname %in% h$temp_tables)
  expect_error(DBI::dbGetQuery(h$conn, paste0("SELECT 1 FROM ", tname, " LIMIT 1")), "exist")
})
