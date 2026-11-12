# WS-5: anchors and windows (design-2.10.3 sections 2.4, 6.1, 6.8) -- the
# anchor matrix: first / last / nth / latest_end x start / end x scope, ties
# broken by (start_date, source_ns, row key), search windows ("the first B
# after the selected A"), anchors before the index, candidate versus final
# resolution, and a missing anchor making windows undefined (UNKNOWN for has,
# has_not and count == 0; NA for reducers), on every backend available.

.ws5_an_backends <- .ws5_backends()
withr::defer(.ws4_cleanup(.ws5_an_backends), testthat::teardown_env())

# Every source anchor the tests compile, declared in the plan (anchor
# fragments are named from the IR's anchor inventory, amendment A-17).
.ws5_an_set <- function() {
  a <- function(...) list(source = list(event = "a"), ...)
  first_a <- a(which = "first")
  ep <- function(w) a(which = w, n = 2L, field = "end", scope = "episode")
  list(fs = a(which = "first", field = "start"), fe = a(which = "first", field = "end"),
       ls = a(which = "last", field = "start"), le = a(which = "last", field = "end"),
       n2s = a(which = "nth", n = 2L, field = "start"), n2e = a(which = "nth", n = 2L, field = "end"),
       n3e = a(which = "nth", n = 3L, field = "end"), n4e = a(which = "nth", n = 4L, field = "end"),
       n5 = a(which = "nth", n = 5L), lte = a(which = "latest_end", field = "end"),
       first_a = first_a, epf = c(first_a, list(scope = "episode")),
       ep_last = ep("last"), ep_nth = ep("nth"), ep_lte = ep("latest_end"),
       plain_b = list(source = list(event = "b"), which = "first"),
       b_after_a = list(source = list(event = "b"), which = "first",
                        search = list(start = 1L, end = NULL, anchor = first_a)),
       after_index = a(which = "first", search = list(start = 0L, end = NULL, anchor = "index")),
       snap = list(source = list(event = "b"), which = "last", field = "end",
                   search = list(start = 1L, end = NULL, anchor = a(which = "latest_end", field = "end"))))
}

.ws5_an_ctx <- function(h) {
  set <- .ws5_an_set()
  .ws4_ctx(h, .ws5_plan(
    events = list(
      a = list(table = "condition_occurrence", concept_set = list(concepts = list(.WS5$ANCHOR_A))),
      b = list(table = "measurement", concept_set = list(concepts = list(.WS5$ANCHOR_B)))),
    populations = list(
      t = .ws5_anchor_population(set, source = "b"),
      cc = .ws5_anchor_population(set[c("first_a", "epf", "ep_last", "ep_nth", "ep_lte")], source = "b")),
    variables = list(v_after = list(source = list(event = "b"), reducer = list(
      fn = "count", window = list(start = 0L, end = 0L, anchor = set$after_index)))),
    outputs = list(ot = list(rows = "t", format = "wide"))))
}

# Anchor dates per candidate key (NA when the row has no anchor).
.ws5_anchor_dates <- function(ctx, anchor, roster, phase = "candidate") {
  f <- .compileAnchor(ctx, anchor, roster, phase)
  spec <- .rosterSpec(roster, phase)
  sql <- .buildStatement(ctx, f, select = paste0(
    "SELECT r.", spec$key, " AS k, a.anchor_date AS d FROM ", roster$name, " r LEFT JOIN ",
    f$name, " a ON a.row_key = r.", spec$key, " ORDER BY r.", spec$key), dialect = ctx)
  rows <- .executeQuery(ctx$handle, sql)
  stats::setNames(ifelse(is.na(rows$d), NA_character_, substr(as.character(rows$d), 1L, 10L)),
                  as.character(as.numeric(rows$k)))
}

test_that("the anchor matrix: which x field, with deterministic ties", {
  for (b in names(.ws5_an_backends)) {
    h <- .ws5_an_backends[[b]]$handle
    ctx <- .ws5_an_ctx(h)
    cand <- .ws5_candidates(ctx)
    a <- function(...) list(source = list(event = "a"), ...)
    d <- function(anchor) .ws5_anchor_dates(ctx, anchor, cand)[["6"]]   # person 3201, index 2020-01-01
    expect_identical(d(a(which = "first", field = "start")), "2019-06-01", info = b)
    expect_identical(d(a(which = "first", field = "end")), "2019-06-10", info = b)
    # the last row in (start, source_ns, key) order is the later-keyed of the
    # two rows starting 2020-03-01, and field = "end" reads THAT row's end
    expect_identical(d(a(which = "last", field = "start")), "2020-03-01", info = b)
    expect_identical(d(a(which = "last", field = "end")), "2020-03-20", info = b)
    expect_identical(d(a(which = "nth", n = 2L, field = "start")), "2019-12-01", info = b)
    expect_identical(d(a(which = "nth", n = 2L, field = "end")), "2020-02-15", info = b)
    expect_identical(d(a(which = "nth", n = 3L, field = "end")), "2020-03-05", info = b)
    expect_identical(d(a(which = "nth", n = 4L, field = "end")), "2020-03-20", info = b)
    expect_true(is.na(d(a(which = "nth", n = 5L))), info = b)
    expect_identical(d(a(which = "latest_end", field = "end")), "2020-03-20", info = b)
    # persons without rows have no anchor
    expect_true(is.na(.ws5_anchor_dates(ctx, a(), cand)[["7"]]), info = b)
    .ws4_expect_rejection(.compileAnchor(ctx, a(which = "latest_end", field = "start"), cand, "candidate"),
                          pattern = "latest_end")
  }
})

test_that("scope = episode confines anchor rows to the observed span", {
  for (b in names(.ws5_an_backends)) {
    h <- .ws5_an_backends[[b]]$handle
    ctx <- .ws5_an_ctx(h)
    cand <- .ws5_candidates(ctx)
    cov <- .compileCoverage(ctx, "cc", list(policy = "unique"), cand, "candidate")
    cc <- .ws5_covered_candidates(ctx, cand, cov)
    a <- list(source = list(event = "a"), which = "first")
    person <- .ws5_anchor_dates(ctx, a, cc)
    episode <- .ws5_anchor_dates(ctx, c(a, list(scope = "episode")), cc)
    # 3203 (key 8): its only row (2020-06-01) lies after its observation ends
    expect_identical(person[["8"]], "2020-06-01", info = b)
    expect_true(is.na(episode[["8"]]), info = b)
    expect_identical(episode[["6"]], "2019-06-01", info = b)
    # an uncovered candidate (3101 is ambiguous: NULL bounds) has no episode anchor
    expect_true(is.na(episode[["1"]]), info = b)
    # every selection honours the scope
    for (which in c("last", "nth", "latest_end")) {
      an <- list(source = list(event = "a"), which = which, n = 2L, field = "end", scope = "episode")
      d <- .ws5_anchor_dates(ctx, an, cc)
      expect_identical(d[["6"]], switch(which, last = "2020-03-20", nth = "2020-02-15",
                                        latest_end = "2020-03-20"), info = paste(b, which))
      expect_true(is.na(d[["8"]]), info = paste(b, which))
    }
    expect_error(.compileAnchor(ctx, c(a, list(scope = "episode")), cand, "candidate"), "coverage bounds")
  }
})

test_that("search windows resolve through the same resolver: the first B after the selected A", {
  for (b in names(.ws5_an_backends)) {
    h <- .ws5_an_backends[[b]]$handle
    ctx <- .ws5_an_ctx(h)
    cand <- .ws5_candidates(ctx)
    first_a <- list(source = list(event = "a"), which = "first")
    plain_b <- list(source = list(event = "b"), which = "first")
    b_after_a <- list(source = list(event = "b"), which = "first",
                      search = list(start = 1L, end = NULL, anchor = first_a))
    expect_identical(.ws5_anchor_dates(ctx, plain_b, cand)[["6"]], "2019-05-01", info = b)
    expect_identical(.ws5_anchor_dates(ctx, b_after_a, cand)[["6"]], "2019-07-01", info = b)
    # without an A there is no search window, hence no B
    expect_true(is.na(.ws5_anchor_dates(ctx, b_after_a, cand)[["7"]]), info = b)
    # anchors may precede the index: a window anchored on the first A
    .compileEventByName(ctx, "b")
    win <- .compileWindow(ctx, list(start = 0L, end = 40L, anchor = first_a), cand, "candidate")
    expect_length(win$required, 1L)
    pred <- .windowStartPredicateSql(win, "e.start_date")
    probe <- .fragment("q_win", "result", "episode",
                       paste0("SELECT r.cand_key AS k, COUNT(e.event_id) AS n FROM ", cand$name, " r ",
                              paste(unname(win$joins), collapse = " "),
                              " LEFT JOIN ev_b e ON e.person_id = r.subject_id AND ", pred,
                              " GROUP BY r.cand_key"),
                       columns = list(k = .fragmentColumn("bigint", FALSE, "episode_key"),
                                      n = .fragmentColumn("integer", FALSE, "measure")),
                       dependencies = unique(c(cand$name, win$dependencies, "ev_b")),
                       person_lineage = list(person_col = NULL, via_roster = cand$name),
                       private_columns = "k")
    n <- .executeQuery(h, .buildStatement(ctx, probe, select = "SELECT * FROM q_win ORDER BY k", dialect = ctx))
    expect_identical(as.integer(n$n[as.numeric(n$k) == 6]), 1L, info = b)   # B on 2019-07-01 is within [06-01, 07-11]
    expect_identical(as.integer(n$n[as.numeric(n$k) == 7]), 0L, info = b)
  }
})

test_that("a missing anchor makes windows undefined: UNKNOWN for has, has_not and count == 0", {
  for (b in names(.ws5_an_backends)) {
    h <- .ws5_an_backends[[b]]$handle
    ctx <- .ws5_an_ctx(h)
    cand <- .ws5_candidates(ctx)
    .compileEventByName(ctx, "b")
    win <- .compileWindow(ctx, list(start = 0L, end = 40L,
                                    anchor = list(source = list(event = "a"), which = "first")),
                          cand, "candidate")
    pred <- .windowStartPredicateSql(win, "e.start_date")
    has <- paste0("EXISTS (SELECT 1 FROM ev_b e WHERE e.person_id = r.subject_id AND ", pred, ")")
    count <- paste0("(SELECT COUNT(*) FROM ev_b e WHERE e.person_id = r.subject_id AND ", pred, ")")
    leaf <- function(x) paste0("CASE WHEN ", .windowGuardSql(win, x), " THEN 1 WHEN NOT (",
                               .windowGuardSql(win, x), ") THEN 0 END")
    probe <- .fragment("q_probe", "result", "episode",
                       paste0("SELECT r.cand_key AS k, ", leaf(has), " AS has, ", leaf(paste0("NOT ", has)),
                              " AS has_not, ", leaf(paste0(count, " = 0")), " AS none FROM ", cand$name, " r ",
                              paste(unname(win$joins), collapse = " ")),
                       columns = list(k = .fragmentColumn("bigint", FALSE, "episode_key"),
                                      has = .fragmentColumn("integer", TRUE, "measure"),
                                      has_not = .fragmentColumn("integer", TRUE, "measure"),
                                      none = .fragmentColumn("integer", TRUE, "measure")),
                       dependencies = unique(c(cand$name, win$dependencies, "ev_b")),
                       person_lineage = list(person_col = NULL, via_roster = cand$name),
                       private_columns = "k")
    rows <- .executeQuery(h, .buildStatement(ctx, probe, select = "SELECT * FROM q_probe ORDER BY k", dialect = ctx))
    r6 <- rows[as.numeric(rows$k) == 6, ]
    r7 <- rows[as.numeric(rows$k) == 7, ]
    expect_identical(as.integer(c(r6$has, r6$has_not, r6$none)), c(1L, 0L, 0L), info = b)
    # 3202 has no A: every quantifier is UNKNOWN, never TRUE for absence
    expect_true(all(is.na(c(r7$has, r7$has_not, r7$none))), info = b)
    # the unguarded absence leaf would wrongly be TRUE
    expect_identical(.windowGuardSql(list(defined = NULL), "x"), "x")
    # reducers over an undefined window are NA
    v <- .intervalReducerValues(ctx, "covered_days", cand, "candidate", source = list(event = "b"),
                                window = list(start = 0L, end = 40L,
                                              anchor = list(source = list(event = "a"), which = "first")))
    expect_identical(v$value[as.numeric(v$row_key) == 6], 1L, info = b)
    expect_true(is.na(v$value[as.numeric(v$row_key) == 7]), info = b)
    # a fully unbounded window needs no anchor
    expect_null(.compileWindow(ctx, list(start = NULL, end = NULL, anchor = "index"), cand, "candidate")$defined)
  }
})

test_that("candidate anchors resolve against each candidate; final anchors against the final roster", {
  for (b in names(.ws5_an_backends)) {
    h <- .ws5_an_backends[[b]]$handle
    ctx <- .ws5_an_ctx(h)
    cand <- .ws5_candidates(ctx)
    cov <- .compileCoverage(ctx, "t", list(policy = "unique"), cand, "candidate")
    roster <- .ws5_roster(ctx, cand, cov, pop = "t")
    after_index <- list(source = list(event = "a"), which = "first",
                        search = list(start = 0L, end = NULL, anchor = "index"))
    c_dates <- .ws5_anchor_dates(ctx, after_index, cand, "candidate")
    f_dates <- .ws5_anchor_dates(ctx, after_index, roster, "final")
    # person 3201: candidate 6 (index 2020-01-01) and candidate 9 (2020-03-02)
    expect_identical(c_dates[["6"]], "2020-03-01", info = b)
    expect_true(is.na(c_dates[["9"]]), info = b)
    expect_identical(f_dates[["6"]], "2020-03-01", info = b)
    expect_true(is.na(f_dates[["9"]]), info = b)
    cf <- .compileAnchor(ctx, after_index, cand, "candidate")
    ff <- .compileAnchor(ctx, after_index, roster, "final")
    expect_false(identical(cf$name, ff$name))
    expect_match(cf$name, "_cand$")
    expect_identical(ff$roster_namespace, "t")
    expect_null(cf$roster_namespace)
    expect_identical(.compileAnchor(ctx, after_index, cand, "candidate"), cf)
    # built-in anchors are roster expressions (no fragment, no join)
    ax <- .anchorExpr(ctx, "index_end", roster, "final", alias = "q")
    expect_identical(ax$expr, "q.cohort_end_date")
    expect_length(ax$joins, 0L)
    expect_identical(.anchorExpr(ctx, "observation_start", roster, "final")$expr, "r.op_start")
    expect_error(.anchorExpr(ctx, "observation_end", cand, "candidate"), "no column")
    expect_match(.anchorExpr(ctx, list(date = "2020-05-01"), cand, "candidate")$expr, "2020-05-01")
  }
})

test_that("anchor fragments are named by the IR anchor inventory", {
  h <- .ws5_an_backends$sqlite$handle
  anc <- list(source = list(event = "a"), which = "first", n = 1L, field = "start",
              scope = "person", search = NULL)
  plan <- .ws5_plan(events = list(
    a = list(table = "condition_occurrence", concept_set = list(concepts = list(.WS5$ANCHOR_A))),
    b = list(table = "measurement", concept_set = list(concepts = list(.WS5$ANCHOR_B)))),
    populations = list(t = list(kind = "criteria", where = list(has = list(
      source = list(event = "b"), window = list(start = 0L, end = 30L, anchor = anc))))))
  ctx <- .ws4_ctx(h, plan)
  inv <- .irAnchorInventory(ctx$ir)
  expect_length(inv$candidate$t, 1L)
  cand <- .ws5_candidates(ctx)
  f <- .compileAnchor(ctx, inv$candidate$t[[1L]], cand, "candidate")
  expect_identical(f$name, "anc_t_1_cand")
  # a copy without the normalised defaults matches its inventory entry
  expect_identical(.compileAnchor(ctx, list(source = list(event = "a")), cand, "candidate")$name,
                   "anc_t_1_cand")
  # an anchor the IR does not declare cannot be named (fail closed, A-17)
  expect_error(.compileAnchor(ctx, list(source = list(event = "b")), cand, "candidate"),
               "not declared by the plan's anchor inventory")
  expect_error(.compileAnchor(ctx, "index", cand, "candidate"), "roster expressions")
  ids <- .anchorFragmentIdentities(ctx$ir)
  expect_identical(vapply(ids, function(id) paste(c(id$name, id$suffix), collapse = "/"), ""), "t_1/cand")
  .ws4_expect_rejection(.anchorNormalize("tomorrow"), pattern = "unknown anchor")
  .ws4_expect_rejection(.anchorNormalize(list(source = list(table = "x"))), pattern = "source")
  .ws4_expect_rejection(.anchorNormalize(list(date = "2020-02-30")), pattern = "ISO date")
  .ws4_expect_rejection(.compileWindow(ctx, list(start = 1.5), cand, "candidate"), pattern = "integer")
})

test_that("compiled anchor and window SQL renders for every dialect family", {
  bp <- .buildBlueprint(.ws5_an_backends$sqlite$handle)
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
      ctx <- .ws5_an_ctx(fh)
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
      anc <- list(source = list(event = "b"), which = "last", field = "end",
                  search = list(start = 1L, end = NULL,
                                anchor = list(source = list(event = "a"), which = "latest_end", field = "end")))
      cat(.buildStatement(ctx, .compileAnchor(ctx, anc, cand, "candidate"), dialect = ctx), "\n")
      win <- .compileWindow(ctx, list(start = -30L, end = list(anchor = "index_end", offset = 5L), anchor = "index"),
                            cand, "candidate")
      cat(.windowStartPredicateSql(win, "e.start_date"), "|", .windowOverlapSql(win, "e.start_date", "e.end_date"), "\n")
    }
  })
})

test_that("the executable WS-5 anchor tests ran on every configured backend", {
  .ws5_expect_backends(.ws5_an_backends, "ws5 anchor")
})
