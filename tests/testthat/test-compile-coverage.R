# WS-5: observation-period coverage (design-2.10.3 sections 1.3, 2.5, 6.13;
# defect A-E6) -- the coverage matrix: unique / earliest / union x on_ambiguous
# drop / error x on_missing drop / keep x gap, including the gap-index case
# (records [1,2] and [10,11], gap 7, index on day 5: uncovered), eligibility
# spans versus actually observed segments, on every backend available.

.ws5_cov_backends <- .ws5_backends()
withr::defer(.ws4_cleanup(.ws5_cov_backends), testthat::teardown_env())

# One population per coverage request (fragment identifiers are allocated from
# the IR, amendment A-17): the policy of each is its declared coverage.
.ws5_cov_pops <- function() {
  cr <- function(...) list(kind = "criteria", coverage = list(...))
  list(t = list(kind = "criteria"), u = cr(policy = "unique"), e = cr(policy = "earliest"),
       g0 = cr(policy = "union", gap = 0L), g7 = cr(policy = "union", gap = 7L),
       g8 = cr(policy = "union", gap = 8L), dk = cr(policy = "unique", on_missing = "keep"),
       ue = cr(policy = "unique", on_ambiguous = "error"),
       gk = cr(policy = "union", gap = 7L, on_missing = "keep"), k = cr(policy = "unique", on_missing = "keep"),
       obs = list(kind = "criteria", where = list(observed = list(before = 30L, gap = 7L))))
}
.ws5_cov_ctx <- function(h) {
  .ws4_ctx(h, .ws5_plan(events = list(drug = .ws5_drug_event()), populations = .ws5_cov_pops()))
}
# The coverage of population `pop` (its declared policy) over a roster.
.ws5_cov <- function(ctx, pop, roster, phase = "candidate") {
  .compileCoverage(ctx, pop, ctx$ir$populations[[pop]]$coverage, roster, phase)
}

.ws5_opc <- function(ctx, cov) {
  r <- .ws5_rows(ctx, cov$coverage, "row_key")
  r[, c("row_key", "person_id", "op_count", "op_start", "op_end", "component_id")]
}

test_that("the coverage matrix: policies, missing before ambiguity, the gap-index case", {
  for (b in names(.ws5_cov_backends)) {
    h <- .ws5_cov_backends[[b]]$handle
    ctx <- .ws5_cov_ctx(h)
    cand <- .ws5_candidates(ctx)
    uni <- .ws5_opc(ctx, .ws5_cov(ctx, "u", cand))
    ear <- .ws5_opc(ctx, .ws5_cov(ctx, "e", cand))
    un0 <- .ws5_opc(ctx, .ws5_cov(ctx, "g0", cand))
    un7 <- .ws5_opc(ctx, .ws5_cov(ctx, "g7", cand))
    un8 <- .ws5_opc(ctx, .ws5_cov(ctx, "g8", cand))
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
    kept <- function(pop) {
      cov <- .ws5_cov(ctx, pop, cand)
      f <- cov$coverage
      sql <- .buildStatement(ctx, f, select = paste0(
        "SELECT o.row_key FROM ", f$name, " o WHERE ", .coverageKeepSql(cov$policy, "o"),
        " ORDER BY o.row_key"), dialect = ctx)
      as.numeric(.executeQuery(h, sql)$row_key)
    }
    expect_identical(kept("u"), c(2, 5, 6, 7, 8, 9, 10), info = b)
    expect_identical(kept("dk"), c(2, 3, 4, 5, 6, 7, 8, 9, 10), info = b)
    expect_identical(kept("e"), c(1, 2, 5, 6, 7, 8, 9, 10), info = b)
    expect_identical(kept("g7"), c(1, 2, 5, 6, 7, 8, 9, 10), info = b)
    expect_identical(kept("gk"), as.numeric(1:10), info = b)
    # on_ambiguous = "error": the user-chosen typed failure, never a count
    err <- .ws5_cov(ctx, "ue", cand)
    rej <- .ws4_expect_rejection(.coverageAssertUnambiguous(ctx, err, "ue"), code = "coverage_ambiguous",
                                 reason = "coverage_ambiguous")
    expect_false(grepl("[0-9]", sub("^.*population 'ue'", "", conditionMessage(rej))))
    # the same policy over unambiguous rows passes; drop never asserts
    expect_true(.coverageAssertUnambiguous(ctx, .ws5_cov(ctx, "u", cand), "u"))
  }
})

test_that("eligibility spans versus actually observed segments: a gap never adds observed days", {
  for (b in names(.ws5_cov_backends)) {
    h <- .ws5_cov_backends[[b]]$handle
    ctx <- .ws5_cov_ctx(h)
    cand <- .ws5_candidates(ctx)
    cov <- .ws5_cov(ctx, "g8", cand)
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
    od <- .ws5_rv(ctx, "observed_days", roster, "final",
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
    ucov <- .ws5_cov(ctx, "u", cand)
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
    cov <- .ws5_cov(ctx, "k", cand)
    roster <- .ws5_roster(ctx, cand, cov, pop = "k")
    r <- .ws5_rows(ctx, roster, "cohort_row_id")
    expect_true(all(c(3, 4) %in% r$cohort_row_id), info = b)
    expect_true(all(is.na(r$op_end[r$cohort_row_id %in% c(3, 4)])), info = b)
    w <- list(start = -30L, end = 30L, anchor = "index")
    od <- .ws5_rv(ctx, "observed_days", roster, "final", window = w, cov = cov)
    val <- function(v, k) v$value[as.numeric(v$row_key) == k]
    expect_true(is.na(val(od, 3)) && is.na(val(od, 4)), info = b)
    expect_identical(val(od, 5), 61L, info = b)
    # the same holds through the coverage of the candidates themselves
    oc <- .ws5_rv(ctx, "observed_days", cand, "candidate", window = w, cov = cov)
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
  expect_identical(.coverageSource(ctx)$name, "op_periods")
  expect_true(all(c("op_count", "component_id", "row_key") %in% a$coverage$private_columns))
  expect_identical(.compileCoverage(ctx, "t", NULL, cand, "candidate")$coverage, a$coverage)
  expect_error(.compileCoverage(ctx, "t", list(policy = "union"), cand, "candidate"), "another policy")
  # identifiers come from the plan's declared identities (A-17): a criterion-
  # specific omop_observed(gap = 7) request is declared as suffix g7; any other
  # coverage request cannot be named
  o7 <- .compileCoverage(ctx, "obs", list(policy = "union", gap = 7L), cand, "candidate", suffix = "g7")
  expect_identical(c(o7$coverage$name, o7$recorded$name), c("opc_obs_g7", "seg_obs_g7_rec"))
  expect_error(.compileCoverage(ctx, "obs", list(policy = "union", gap = 3L), cand, "candidate", suffix = "g3"),
               "not declared by an identity generator")
  expect_error(.compileCoverage(ctx, "nope", NULL, cand, "candidate"), "not declared by an identity generator")
  expect_true(any(vapply(.coverageFragmentIdentities(ctx$ir), function(id) identical(id$suffix, "g7_obs_final"),
                         logical(1))))
})

test_that("observed segments are distinct; malformed periods never cover (A-20 (f), (h); N1)", {
  for (b in names(.ws5_cov_backends)) {
    h <- .ws5_cov_backends[[b]]$handle
    tbl <- .qualifyTable(h, "observation_period", h$cdm_schema)
    ins <- function(id, pid, start, end) {
      DBI::dbExecute(h$conn, paste0(
        "INSERT INTO ", tbl, " (observation_period_id, person_id, observation_period_start_date, ",
        "observation_period_end_date, period_type_concept_id) VALUES (", id, ", ", pid, ", ",
        .sqlDateLiteral(h, start), ", ", if (is.na(end)) "NULL" else .sqlDateLiteral(h, end), ", 44814724)"))
    }
    # 3105: a second record with identical dates (one component) and a
    # malformed open record from 2010; 3104 (no period): a malformed record
    # starting before its index with a NULL end, and one ending before it starts
    ins(399991, 3105, "2015-01-01", "2030-12-31")
    ins(399992, 3105, "2010-01-01", NA)
    ins(399993, 3104, "2020-01-01", NA)
    ins(399994, 3104, "2020-01-05", "2019-01-01")
    on.exit(DBI::dbExecute(h$conn, paste0("DELETE FROM ", tbl, " WHERE observation_period_id IN ",
                                          "(399991, 399992, 399993, 399994)")), add = TRUE)
    ctx <- .ws5_cov_ctx(h)
    cand <- .ws5_candidates(ctx)
    cov <- .ws5_cov(ctx, "g7", cand)
    roster <- .ws5_roster(ctx, cand, cov, pop = "g7")
    f <- .compileObservedSegments(ctx, cov, roster, "final")
    v <- .ws5_rows(ctx, f, c("row_key", "start_date"))
    expect_identical(anyDuplicated(v[f$unique_key]), 0L, info = b)
    expect_identical(nrow(v[v$row_key == 5, ]), 1L, info = b)
    candidate_segments <- .compileObservedSegments(ctx, cov, cand, "candidate")
    candidate_rows <- .ws5_rows(ctx, candidate_segments, c("row_key", "start_date"))
    expect_identical(anyDuplicated(candidate_rows[candidate_segments$unique_key]), 0L, info = b)
    expect_identical(nrow(candidate_rows[candidate_rows$row_key == 5, ]), 1L, info = b)
    # the malformed records never enter the component or the count
    un <- .ws5_opc(ctx, cov)
    expect_identical(c(un$op_start[un$row_key == 5], un$op_end[un$row_key == 5]),
                     c("2015-01-01", "2030-12-31"), info = b)
    expect_identical(un$op_count[un$row_key == 4], 0, info = b)
    uni <- .ws5_opc(ctx, .ws5_cov(ctx, "u", cand))
    expect_identical(uni$op_count[uni$row_key == 4], 0, info = b)
    # 3105's two identical records are ambiguous under unique (two records), one
    # segment once coalesced, and 61 observed days in [-30, 30]
    expect_identical(uni$op_count[uni$row_key == 5], 2, info = b)
    od <- .ws5_rv(ctx, "observed_days", roster, "final", window = list(start = -30L, end = 30L, anchor = "index"),
                  cov = cov)
    expect_identical(od$value[as.numeric(od$row_key) == 5], 61L, info = b)
  }
})

test_that("observed-segment and covered-key realisations enforce the reducer cap (B6)", {
  h <- .ws5_cov_backends$sqlite$handle
  ctx <- .ws5_cov_ctx(h)
  cand <- .ws5_literal_roster(ctx, data.frame(cand_key = 1L, subject_id = 1003,
    cohort_start_date = "2020-01-01", cohort_end_date = "2020-12-31"))
  cov <- .ws5_cov(ctx, "g7", cand)
  roster <- .ws5_roster(ctx, cand, cov, pop = "g7")
  value <- function() .ws5_rv(ctx, "observed_days", roster, "final",
    window = list(start = 0L, end = 9L, anchor = "index"), cov = cov)$value
  withr::with_options(list(dsomop.max_memory_rows = 2), expect_identical(value(), 4L))
  # One roster row fits; its two recorded segments exceed the cap.
  error <- withr::with_options(list(dsomop.max_memory_rows = 1), tryCatch(value(), error = identity))
  expect_s3_class(error, "dsomop_plan_rejection")
  expect_identical(error$reason, "reducer_row_cap")

  ctx <- .ws5_cov_ctx(h)
  cand <- .ws5_candidates(ctx)
  cov <- .ws5_cov(ctx, "g7", cand)
  roster <- .ws5_roster(ctx, cand, cov, pop = "g7")
  for (phase in c("candidate", "final")) {
    r <- if (phase == "candidate") cand else roster
    keys <- function() .coverageCoveredKeys(ctx, cov, r, phase)
    n <- length(keys())
    expect_gt(n, 1L)
    withr::with_options(list(dsomop.max_memory_rows = n), expect_length(keys(), n))
    error <- withr::with_options(list(dsomop.max_memory_rows = n - 1L), tryCatch(keys(), error = identity))
    expect_s3_class(error, "dsomop_plan_rejection")
    expect_identical(error$reason, "reducer_row_cap")
  }
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
      ctx <- .ws4_ctx(fh, .ws5_plan(events = list(drug = .ws5_drug_event()), populations = .ws5_cov_pops()))
      cand <- .fragment("cand_t", "candidates", "episode",
                        "SELECT 1 AS cand_key, 1 AS subject_id, NULL AS cohort_start_date, NULL AS cohort_end_date",
                        columns = list(cand_key = .fragmentColumn("bigint", FALSE, "episode_key"),
                                       subject_id = .fragmentColumn("integer", FALSE, "person_key"),
                                       cohort_start_date = .fragmentColumn("date", FALSE, "start"),
                                       cohort_end_date = .fragmentColumn("date", TRUE, "end")),
                        unique_key = "cand_key", person_lineage = list(person_col = "subject_id", via_roster = NULL),
                        private_columns = "cand_key")
      attr(cand, "population") <- "t"
      .fragmentRegister(ctx, cand)
      cat("--", family, "\n")
      for (p in c("u", "e", "g7")) {
        cov <- .ws5_cov(ctx, p, cand)
        cat(.buildStatement(ctx, cov$coverage, dialect = ctx), "\n")
      }
    }
  })
})

test_that("the executable WS-5 coverage tests ran on every configured backend", {
  .ws5_expect_backends(.ws5_cov_backends, "ws5 coverage")
})
