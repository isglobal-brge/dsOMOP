# WS-5: observation-period coverage (design-2.10.3 sections 1.3, 2.5, 6.13;
# defect A-E6) -- the coverage matrix: unique / earliest / union x on_ambiguous
# drop / error x on_missing drop / keep x gap, including the gap-index case
# (records [1,2] and [10,11], gap 7, index on day 5: uncovered), eligibility
# spans versus actually observed segments, on every backend available.

.ws5_cov_backends <- .ws5_backends()
withr::defer(.ws4_cleanup(.ws5_cov_backends), testthat::teardown_env())

.ws5_cov_ctx <- function(h) .ws4_ctx(h, .ws5_plan(events = list(drug = .ws5_drug_event())))

.ws5_opc <- function(ctx, cov) {
  r <- .ws5_rows(ctx, cov$coverage, "row_key")
  r[, c("row_key", "person_id", "op_count", "op_start", "op_end", "component_id")]
}

test_that("the coverage matrix: policies, missing before ambiguity, the gap-index case", {
  for (b in names(.ws5_cov_backends)) {
    h <- .ws5_cov_backends[[b]]$handle
    ctx <- .ws5_cov_ctx(h)
    cand <- .ws5_candidates(ctx)
    uni <- .ws5_opc(ctx, .compileCoverage(ctx, "t", list(policy = "unique"), cand, "candidate", "u"))
    ear <- .ws5_opc(ctx, .compileCoverage(ctx, "t", list(policy = "earliest"), cand, "candidate", "e"))
    un0 <- .ws5_opc(ctx, .compileCoverage(ctx, "t", list(policy = "union", gap = 0), cand, "candidate", "g0"))
    un7 <- .ws5_opc(ctx, .compileCoverage(ctx, "t", list(policy = "union", gap = 7), cand, "candidate", "g7"))
    un8 <- .ws5_opc(ctx, .compileCoverage(ctx, "t", list(policy = "union", gap = 8), cand, "candidate", "g8"))
    k <- function(df, key) df[df$row_key == key, , drop = FALSE]
    # 3101: two overlapping periods cover the index -> ambiguous under unique
    # (bounds withheld), the earliest complete record under earliest, one
    # coalesced component under union
    expect_identical(k(uni, 1)$op_count, 2, info = b)
    expect_true(is.na(k(uni, 1)$op_start) && is.na(k(uni, 1)$op_end), info = b)
    expect_identical(c(k(ear, 1)$op_start, k(ear, 1)$op_end), c("2015-01-01", "2020-06-30"), info = b)
    expect_identical(k(ear, 1)$component_id, 310101, info = b)
    expect_identical(c(k(un0, 1)$op_count, k(un0, 1)$component_id), c(1, 1), info = b)
    expect_identical(c(k(un0, 1)$op_start, k(un0, 1)$op_end), c("2015-01-01", "2030-12-31"), info = b)
    # 3102: exactly 8 uncovered days -- separate components at gap 7, one at gap 8
    expect_identical(c(k(uni, 2)$op_start, k(uni, 2)$op_end), c("2020-01-01", "2030-12-31"), info = b)
    expect_identical(c(k(un7, 2)$op_start, k(un7, 2)$component_id), c("2020-01-01", "2"), info = b)
    expect_identical(c(k(un8, 2)$op_start, k(un8, 2)$op_end), c("2015-01-01", "2030-12-31"), info = b)
    # 3103: the gap-index case -- [01-01,01-02] + [01-10,01-11] coalesce under
    # gap 7, but an index on 01-05 lies in no recorded period: uncovered
    expect_identical(k(un7, 3)$op_count, 0, info = b)
    expect_true(is.na(k(un7, 3)$op_start), info = b)
    expect_identical(k(uni, 3)$op_count, 0, info = b)
    expect_identical(k(ear, 3)$op_count, 0, info = b)
    # 3104: no period at all; 3105: one period
    expect_identical(c(k(uni, 4)$op_count, k(ear, 4)$op_count, k(un7, 4)$op_count), c(0, 0, 0), info = b)
    expect_identical(c(k(uni, 5)$op_count, k(ear, 5)$op_count, k(un7, 5)$op_count), c(1, 1, 1), info = b)
    # 1003: two periods 6 days apart -- unique: the covering record; union gap 7:
    # one component whose span bridges the gap
    expect_identical(c(k(uni, 10)$op_start, k(uni, 10)$op_end), c("2015-01-01", "2020-01-02"), info = b)
    expect_identical(c(k(un7, 10)$op_start, k(un7, 10)$op_end), c("2015-01-01", "2030-12-31"), info = b)
  }
})

test_that("drop / keep / error: what each policy keeps, and coverage_ambiguous", {
  for (b in names(.ws5_cov_backends)) {
    h <- .ws5_cov_backends[[b]]$handle
    ctx <- .ws5_cov_ctx(h)
    cand <- .ws5_candidates(ctx)
    kept <- function(coverage, suffix) {
      cov <- .compileCoverage(ctx, "t", coverage, cand, "candidate", suffix)
      f <- cov$coverage
      sql <- .buildStatement(ctx, f, select = paste0(
        "SELECT o.row_key FROM ", f$name, " o WHERE ", .coverageKeepSql(cov$policy, "o"),
        " ORDER BY o.row_key"), dialect = ctx)
      as.numeric(.executeQuery(h, sql)$row_key)
    }
    expect_identical(kept(list(policy = "unique"), "dd"), c(2, 5, 6, 7, 8, 9, 10), info = b)
    expect_identical(kept(list(policy = "unique", on_missing = "keep"), "dk"), c(2, 3, 4, 5, 6, 7, 8, 9, 10), info = b)
    expect_identical(kept(list(policy = "earliest"), "ed"), c(1, 2, 5, 6, 7, 8, 9, 10), info = b)
    expect_identical(kept(list(policy = "union", gap = 7), "gd"), c(1, 2, 5, 6, 7, 8, 9, 10), info = b)
    expect_identical(kept(list(policy = "union", gap = 7, on_missing = "keep"), "gk"), as.numeric(1:10), info = b)
    # on_ambiguous = "error": the user-chosen typed failure, never a count
    err <- .compileCoverage(ctx, "t", list(policy = "unique", on_ambiguous = "error"), cand, "candidate", "ue")
    rej <- .ws4_expect_rejection(.coverageAssertUnambiguous(ctx, err, "t"), code = "coverage_ambiguous",
                                 reason = "coverage_ambiguous")
    expect_false(grepl("[0-9]", sub("^.*population 't'", "", conditionMessage(rej))))
    # the same policy over unambiguous rows passes; drop never asserts
    expect_true(.coverageAssertUnambiguous(ctx, .compileCoverage(ctx, "t", list(policy = "unique"),
                                                                 cand, "candidate", "dd"), "t"))
  }
})

test_that("eligibility spans versus actually observed segments: a gap never adds observed days", {
  for (b in names(.ws5_cov_backends)) {
    h <- .ws5_cov_backends[[b]]$handle
    ctx <- .ws5_cov_ctx(h)
    cand <- .ws5_candidates(ctx)
    cov <- .compileCoverage(ctx, "g8", list(policy = "union", gap = 8), cand, "candidate")
    roster <- .ws5_roster(ctx, cand, cov, pop = "g8")
    r <- .ws5_rows(ctx, roster, "cohort_row_id")
    # 3102 (row 2): the eligibility span bridges the 8 unobserved days ...
    expect_identical(c(r$op_start[r$cohort_row_id == 2], r$op_end[r$cohort_row_id == 2]),
                     c("2015-01-01", "2030-12-31"), info = b)
    # ... but its actually observed segments are the two recorded periods
    seg <- .ws5_rows(ctx, .compileObservedSegments(ctx, cov, roster, "final"), c("row_key", "start_date"))
    s2 <- seg[seg$row_key == 2, ]
    expect_identical(s2$start_date, c("2015-01-01", "2020-01-01"), info = b)
    expect_identical(s2$end_date, c("2019-12-23", "2030-12-31"), info = b)
    # observed days in [index - 365, index]: 366 window days minus the 8 gap days
    od <- .intervalReducerValues(ctx, "observed_days", roster, "final",
                                 window = list(start = -365, end = 0, anchor = "index"), cov = cov)
    expect_identical(od$value[od$row_key == 2], 358L, info = b)
    # a continuity requirement may be satisfied through the gap (eligibility span)
    obs <- .coverageObservedSql(ctx, "r", "r.cohort_start_date", before = 365)
    sql <- .buildStatement(ctx, roster, select = paste0(
      "SELECT r.cohort_row_id, CASE WHEN ", obs, " THEN 1 ELSE 0 END AS ok FROM ", roster$name,
      " r ORDER BY r.cohort_row_id"), dialect = ctx)
    ok <- .executeQuery(h, sql)
    expect_identical(as.integer(ok$ok[as.numeric(ok$cohort_row_id) == 2]), 1L, info = b)
    # under unique coverage the same requirement fails (the covering record
    # starts 9 days before the index)
    ucov <- .compileCoverage(ctx, "u", list(policy = "unique"), cand, "candidate")
    uroster <- .ws5_roster(ctx, cand, ucov, pop = "u")
    sql <- .buildStatement(ctx, uroster, select = paste0(
      "SELECT r.cohort_row_id, CASE WHEN ", obs, " THEN 1 ELSE 0 END AS ok FROM ", uroster$name,
      " r ORDER BY r.cohort_row_id"), dialect = ctx)
    uok <- .executeQuery(h, sql)
    expect_identical(as.integer(uok$ok[as.numeric(uok$cohort_row_id) == 2]), 0L, info = b)
    # unique / earliest: the observed segment is the selected record itself
    useg <- .ws5_rows(ctx, .compileObservedSegments(ctx, ucov, uroster, "final"), c("row_key", "start_date"))
    expect_identical(useg$start_date[useg$row_key == 2], "2020-01-01", info = b)
    # without op_component_id on the roster, the coverage of the same roster
    # supplies the component
    cseg <- .ws5_rows(ctx, .compileObservedSegments(ctx, ucov, cand, "candidate", name = "seg_u_cand"),
                      c("row_key", "start_date"))
    expect_identical(cseg$start_date[cseg$row_key == 2], "2020-01-01", info = b)
    expect_false(any(cseg$row_key %in% c(1, 3, 4)))
  }
})

test_that("rows kept without coverage have no observed days (NA, never 0)", {
  for (b in names(.ws5_cov_backends)) {
    h <- .ws5_cov_backends[[b]]$handle
    ctx <- .ws5_cov_ctx(h)
    cand <- .ws5_candidates(ctx)
    cov <- .compileCoverage(ctx, "k", list(policy = "unique", on_missing = "keep"), cand, "candidate")
    roster <- .ws5_roster(ctx, cand, cov, pop = "k")
    r <- .ws5_rows(ctx, roster, "cohort_row_id")
    expect_true(all(c(3, 4) %in% r$cohort_row_id), info = b)
    expect_true(all(is.na(r$op_end[r$cohort_row_id %in% c(3, 4)])), info = b)
    w <- list(start = -30L, end = 30L, anchor = "index")
    od <- .intervalReducerValues(ctx, "observed_days", roster, "final", window = w, cov = cov)
    val <- function(v, k) v$value[as.numeric(v$row_key) == k]
    expect_true(is.na(val(od, 3)) && is.na(val(od, 4)), info = b)
    expect_identical(val(od, 5), 61L, info = b)
    # the same holds through the coverage of the candidates themselves
    oc <- .intervalReducerValues(ctx, "observed_days", cand, "candidate", window = w, cov = cov)
    expect_true(is.na(val(oc, 1)) && is.na(val(oc, 4)), info = b)
    expect_identical(val(oc, 5), 61L, info = b)
  }
})

test_that("coverage policies are validated; the observation table is required", {
  expect_identical(.coveragePolicy(NULL), list(policy = "unique", on_ambiguous = "drop",
                                               on_missing = "drop", gap = 0L))
  .ws4_expect_rejection(.coveragePolicy(list(policy = "any")), pattern = "policy")
  .ws4_expect_rejection(.coveragePolicy(list(gap = -1)), pattern = "gap")
  .ws4_expect_rejection(.coveragePolicy(list(on_missing = "error")), pattern = "on_missing")
  .ws4_expect_rejection(.coveragePolicy(list(extra = 1)), pattern = "unknown")
  h <- .ws5_cov_backends$sqlite$handle
  ctx <- .ws5_cov_ctx(h)
  cand <- .ws5_candidates(ctx)
  a <- .compileCoverage(ctx, "t", NULL, cand, "candidate")
  expect_identical(a$coverage$name, "opc_t")
  expect_identical(a$segments$name, "seg_t")
  expect_identical(a$recorded$name, "seg_t_rec")
  expect_true(all(c("op_count", "component_id", "row_key") %in% a$coverage$private_columns))
  expect_identical(.compileCoverage(ctx, "t", NULL, cand, "candidate")$coverage, a$coverage)
  expect_error(.compileCoverage(ctx, "t", list(policy = "union"), cand, "candidate"), "another policy")
})

test_that("compiled coverage SQL renders for every dialect family", {
  bp <- .buildBlueprint(.ws5_cov_backends$sqlite$handle)
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
      ctx <- .ws4_ctx(fh, .ws5_plan(events = list(drug = .ws5_drug_event())))
      cand <- .fragment("cand_t", "candidates", "episode",
                        "SELECT 1 AS cand_key, 1 AS subject_id, NULL AS cohort_start_date, NULL AS cohort_end_date",
                        columns = list(cand_key = .fragmentColumn("bigint", FALSE, "episode_key"),
                                       subject_id = .fragmentColumn("integer", FALSE, "person_key"),
                                       cohort_start_date = .fragmentColumn("date", FALSE, "start"),
                                       cohort_end_date = .fragmentColumn("date", TRUE, "end")),
                        unique_key = "cand_key", person_lineage = list(person_col = "subject_id", via_roster = NULL),
                        private_columns = "cand_key")
      .fragmentRegister(ctx, cand)
      cat("--", family, "\n")
      for (p in c("unique", "earliest", "union")) {
        cov <- .compileCoverage(ctx, "t", list(policy = p, gap = 7), cand, "candidate", p)
        cat(.buildStatement(ctx, cov$coverage, dialect = ctx), "\n")
      }
    }
  })
})

test_that("the executable WS-5 coverage tests ran on every configured backend", {
  .ws5_expect_backends(.ws5_cov_backends, "ws5 coverage")
})
