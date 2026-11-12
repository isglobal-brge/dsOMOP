# WS-6: the exact bounded matching sampler (design-2.10.3 sections 2.8 and
# 6.10; amendments A-20 c, f, j; defect A-E10) -- the published encoding and
# its test vectors, the R/SQL hash order equality, the eligible pair relation
# (self-exclusion, same-time outcomes under exclude and allow, future cases,
# deaths, actual coverage at t, strata with NULL strata under both policies),
# between-sets and greedy selection with allocation rollback, zero-candidate
# cases, weights, the cap + 1 refusal without truncation (also when the pool
# changes between two runs), seed, row-order and dialect reproducibility --
# against an independent base-R reference sampler over the raw fixture tables,
# on every backend available (persons 7001..8012 of helper-ws6.R).

.ws6_m_backends <- .ws6_backends(extend = .ws6_match_extend)
withr::defer(.ws4_cleanup(.ws6_m_backends), testthat::teardown_env())

# --- the plan -----------------------------------------------------------------

.ws6_m_strata <- function() {
  list("sex", list(age_band = 5L), "calendar_year",
       list(strata = list(source = list(event = "str"), window = list(start = NULL, end = -1L, anchor = "index"))))
}

# Cases (first eligible OUT, observed one year, end = index) and one match
# with its own template per variant.
.ws6_m_plan <- function(variants) {
  pops <- list(cases = list(kind = "index", index = list(source = list(event = "out"), select = "first_eligible",
                                                        end = list(strategy = "fixed", days = 0L)),
                            where = list(observed = list(before = 365L))))
  for (v in names(variants)) {
    pops[[paste0("tpl_", v)]] <- list(kind = "criteria", where = list(observed = list(before = 365L)))
    pops[[v]] <- list(kind = "match", match = c(list(cases = "cases", controls = paste0("tpl_", v),
                                                     outcome = list(event = "out"), match = .ws6_m_strata()),
                                                variants[[v]]))
  }
  .ws6_plan(list(out = .ws6_event("condition_occurrence", .WS6M$OUT),
                 str = .ws6_event("condition_occurrence", .WS6M$STR)), pops)
}

.ws6_m_variants <- list(
  default = list(seed = 20261111L),
  seed2 = list(seed = 2L),
  allow = list(seed = 20261111L, prior_outcome = "allow"),
  future = list(seed = 20261111L, future_cases = "ineligible"),
  missing = list(seed = 20261111L, missing_strata = "match_missing"),
  none_keep = list(seed = 20261111L, replacement = "none"),
  rollback = list(seed = 20261111L, replacement = "none", insufficient = "drop_case", controls_per_case = 3L,
                  future_cases = "ineligible"),
  between_drop = list(seed = 20261111L, insufficient = "drop_case"),
  k10 = list(seed = 20261111L, controls_per_case = 10L))

# --- the independent reference -------------------------------------------------

# The published encoding re-implemented from its definition (design 6.10).
.ws6_ref_field <- function(tag, text = NULL) {
  bytes <- if (is.null(text)) raw(0) else charToRaw(enc2utf8(as.character(text)))
  n <- length(bytes)
  c(as.raw(tag), as.raw(c(n %/% 16777216L, (n %/% 65536L) %% 256L, (n %/% 256L) %% 256L, n %% 256L)), bytes)
}
.ws6_ref_hash <- function(seed, ns, pid, start, end, source, key, control) {
  e <- c(.ws6_ref_field(2, "dsomop-match-v1"), .ws6_ref_field(1, seed), .ws6_ref_field(2, ns),
         .ws6_ref_field(1, pid), .ws6_ref_field(3, start),
         if (is.null(end)) .ws6_ref_field(0) else .ws6_ref_field(3, end),
         .ws6_ref_field(2, source), if (is.null(key)) .ws6_ref_field(0) else .ws6_ref_field(2, key),
         .ws6_ref_field(1, control))
  paste(as.character(openssl::sha256(e)), collapse = "")
}

# Base-R reference sampler over the raw tables (no engine code).
.ws6_ref_match <- function(conn, s, rollback = TRUE) {
  s <- utils::modifyList(list(controls_per_case = 4L, prior_outcome = "exclude", future_cases = "eligible",
                              replacement = "between_sets", insufficient = "keep_available",
                              missing_strata = "ineligible"), s)
  q <- function(sql) DBI::dbGetQuery(conn, sql)
  persons <- q("SELECT person_id, gender_concept_id AS sex, year_of_birth AS yob FROM person")
  ops <- q(paste0("SELECT person_id, observation_period_start_date AS s, observation_period_end_date AS e ",
                  "FROM observation_period WHERE observation_period_end_date >= observation_period_start_date"))
  deaths <- q("SELECT person_id, death_date AS d FROM death")
  ev <- function(concept) q(paste0("SELECT condition_occurrence_id AS id, person_id, condition_start_date AS d ",
                                   "FROM condition_occurrence WHERE condition_concept_id = ", concept))
  out <- ev(.WS6M$OUT)
  str <- ev(.WS6M$STR)
  D <- as.Date
  covering <- function(pid, t) {
    o <- ops[ops$person_id == pid & D(ops$s) <= D(t) & D(ops$e) >= D(t), , drop = FALSE]
    if (nrow(o) == 1L) o else NULL
  }
  out <- out[order(out$person_id, out$d, out$id), ]
  cases <- list()
  for (pid in unique(out$person_id)) {
    r <- out[out$person_id == pid, ]
    for (k in seq_len(nrow(r))) {
      cov <- covering(pid, r$d[k])
      if (is.null(cov) || D(cov$s) > D(r$d[k]) - 365L) next
      cases[[length(cases) + 1L]] <- list(pid = pid, t = r$d[k], key = as.character(r$id[k]))
      break
    }
  }
  cases <- cases[order(vapply(cases, function(c) as.numeric(D(c$t)), 1), vapply(cases, function(c) c$pid, 1))]
  yob <- function(pid) persons$yob[match(pid, persons$person_id)]
  strata <- function(pid, t) {
    y <- yob(pid)
    list(persons$sex[match(pid, persons$person_id)],
         if (is.na(y)) NA else (as.integer(substr(t, 1, 4)) - y) %/% 5L,
         as.integer(substr(t, 1, 4)),
         as.integer(any(D(str$d[str$person_id == pid]) <= D(t) - 1L)))
  }
  same <- function(a, b) {
    if (identical(s$missing_strata, "match_missing") && is.na(a) && is.na(b)) return(TRUE)
    !is.na(a) && !is.na(b) && a == b
  }
  used <- character(0)
  rows <- list()
  set_id <- 0L
  pairs <- 0L
  for (cs in cases) {
    set_id <- set_id + 1L
    t <- cs$t
    st <- strata(cs$pid, t)
    pool <- integer(0)
    for (k in persons$person_id) {
      if (k == cs$pid) next
      cov <- covering(k, t)
      if (is.null(cov) || D(cov$s) > D(t) - 365L) next
      dd <- D(deaths$d[deaths$person_id == k])
      if (length(dd) && any(dd < D(t))) next
      od <- D(out$d[out$person_id == k])
      if (identical(s$prior_outcome, "exclude") && any(od <= D(t))) next
      if (any(od == D(t))) next
      if (identical(s$future_cases, "ineligible") && any(od > D(t))) next
      sk <- strata(k, t)
      if (!all(mapply(same, st, sk))) next
      pool <- c(pool, k)
    }
    n <- length(pool)
    pairs <- pairs + n
    ord <- if (n) pool[order(vapply(pool, function(k) .ws6_ref_hash(s$seed, "cases", cs$pid, t, t, "out",
                                                                     cs$key, k), ""), pool)] else integer(0)
    chosen <- if (identical(s$replacement, "between_sets")) ord[seq_len(min(s$controls_per_case, n))] else {
      free <- ord[!as.character(ord) %in% used]
      free[seq_len(min(s$controls_per_case, length(free)))]
    }
    dropped <- length(chosen) < s$controls_per_case && identical(s$insufficient, "drop_case")
    if (identical(s$replacement, "none") && (!dropped || !rollback)) used <- c(used, as.character(chosen))
    if (dropped) next
    w <- if (identical(s$replacement, "between_sets")) n / length(chosen) else NA_real_
    rows[[length(rows) + 1L]] <- data.frame(person_id = c(cs$pid, chosen), index_date = t, set_id = set_id,
                                            is_case = c(1, rep(0, length(chosen))), w = c(1, rep(w, length(chosen))),
                                            stringsAsFactors = FALSE)
  }
  res <- do.call(rbind, rows)
  attr(res, "pairs") <- pairs
  res
}

.ws6_m_cols <- c("person_id", "index_date", "set_id", "is_case", "w")
.ws6_m_sorted <- function(df) {
  df <- df[, .ws6_m_cols]
  df <- df[order(df$set_id, -df$is_case, df$person_id), ]
  rownames(df) <- NULL
  df
}

# --- the published encoding --------------------------------------------------------

test_that("the published matching test vectors reproduce with the engine encoder", {
  v <- jsonlite::fromJSON(testthat::test_path("fixtures", "match-vectors.json"), simplifyVector = FALSE)
  for (x in v$vectors) {
    got <- .matchHash(x$seed, x$namespace, x$subject_id, x$cohort_start_date, x$cohort_end_date,
                      x$index_source, x$index_key, x$control)
    expect_identical(got, x$sha256, info = paste(x$seed, x$index_source))
  }
  # field layout: tag byte, u32 big-endian length, bytes; null = tag 0 length 0
  expect_identical(.matchField(0x02, "ab"), as.raw(c(2, 0, 0, 0, 2, 0x61, 0x62)))
  expect_identical(.matchField(0x00), as.raw(c(0, 0, 0, 0, 0)))
  expect_identical(.matchDecimal("007"), "7")
  expect_identical(.matchDecimal(-12), "-12")
  expect_identical(.matchDecimal("9223372036854775807"), "9223372036854775807")
  # numeric order of 64-bit decimal keys (never through a double)
  expect_identical(.matchDecimalRank(c("10", "9", "-3", "9223372036854775807", "-40")), c(4L, 3L, 2L, 5L, 1L))
  expect_error(.matchText("café"), "ASCII")
  # length prefixes keep field boundaries ("ca" + "ses" != "cas" + "es")
  expect_false(identical(.matchHash(1, "ca", 17, "2019-03-04", NULL, "ses", NULL, 1),
                         .matchHash(1, "cas", 17, "2019-03-04", NULL, "es", NULL, 1)))
})

# --- the sampler against the reference ----------------------------------------------

test_that("every variant reproduces the reference sampler on every backend, identically", {
  plan <- .ws6_m_plan(.ws6_m_variants)
  ref_conn <- .ws6_m_backends$sqlite$handle$conn
  want <- lapply(.ws6_m_variants, function(s) .ws6_ref_match(ref_conn, s))
  first <- list()
  for (b in names(.ws6_m_backends)) {
    ctx <- .ws4_ctx(.ws6_m_backends[[b]]$handle, plan)
    for (v in names(.ws6_m_variants)) {
      got <- .ws6_roster(ctx, v)
      expect_equal(.ws6_m_sorted(got), .ws6_m_sorted(want[[v]]), info = paste(v, b))
      if (is.null(first[[v]])) first[[v]] <- got else expect_identical(got, first[[v]], info = paste(v, b))
    }
    .compileReleaseTempTables(ctx)
  }
})

test_that("pair rules: self-exclusion, same-time outcomes, future cases, deaths, coverage at t, strata", {
  ref_conn <- .ws6_m_backends$sqlite$handle$conn
  set_of <- function(r, case) r$person_id[r$set_id == r$set_id[r$person_id == case & r$is_case == 1]]
  plan <- .ws6_m_plan(.ws6_m_variants[c("default", "allow", "future", "missing", "k10")])
  ctx <- .ws4_ctx(.ws6_m_backends$sqlite$handle, plan)
  on.exit(.compileReleaseTempTables(ctx), add = TRUE)
  d <- .ws6_roster(ctx, "default")
  k10 <- .ws6_roster(ctx, "k10")
  # self-exclusion: the case is the only member of its set with its person
  expect_true(all(tapply(d$person_id, d$set_id, function(x) !anyDuplicated(x))))
  # 7001 (2020-03-01): pool {7003, 7013..7024, 7031} = 14, w = 14/4; never 7004
  # (outcome exactly at t), 7005 (prior outcome), 7010..7012 (stratum), 7030
  # (died before t), 7032 (not observed at t), 7034 (observed < 365 days),
  # 7035 (other age band), 7006/7033 (NULL band)
  expect_identical(unique(d$w[d$set_id == d$set_id[d$person_id == 7001 & d$is_case == 1] & d$is_case == 0]), 3.5)
  pool <- c(7003, 7013:7024, 7031)
  expect_true(all(setdiff(set_of(k10, 7001), 7001) %in% pool))
  expect_identical(length(setdiff(set_of(k10, 7001), 7001)), 10L)
  # same-time outcome: 7004 never controls a 2020-03-01 case, nor 7001 for 7004
  expect_false(7004 %in% set_of(k10, 7001))
  expect_false(7001 %in% set_of(k10, 7004))
  # allow: earlier outcomes permitted (7005 joins: N = 15), an outcome at t
  # still excludes
  a <- .ws6_roster(ctx, "allow")
  expect_identical(unique(a$w[a$set_id == a$set_id[a$person_id == 7001 & a$is_case == 1] & a$is_case == 0]), 3.75)
  # future cases ineligible: 7003 and 7005 leave the pool (N = 13)
  f <- .ws6_roster(ctx, "future")
  expect_identical(unique(f$w[f$set_id == f$set_id[f$person_id == 7001 & f$is_case == 1] & f$is_case == 0]), 3.25)
  # NULL strata: ineligible by default (7006 alone), matched under match_missing
  expect_identical(set_of(d, 7006), 7006)
  m <- .ws6_roster(ctx, "missing")
  expect_setequal(set_of(m, 7006), c(7006, 1009, 7033))
  # a zero-candidate case is kept with no control (keep_available)
  expect_identical(set_of(d, 7007), 7007)
  # a control in two same-day sets keeps two rows with distinct identities
  shared <- intersect(set_of(k10, 7001), set_of(k10, 7004))
  expect_gte(length(shared), 6L)
  two <- k10[k10$person_id == shared[[1]] & k10$index_date == "2020-03-01", ]
  expect_identical(nrow(two), 2L)
  expect_false(anyDuplicated(two$cohort_row_id) > 0L)
  expect_false(identical(two$index_key[[1]], two$index_key[[2]]))
  # cases first by time, then identity: 7005 (2018) is set 1
  expect_identical(d$set_id[d$person_id == 7005 & d$is_case == 1], 1)
})

test_that("greedy selection without replacement and the allocation rollback of a dropped case", {
  ref_conn <- .ws6_m_backends$sqlite$handle$conn
  s <- .ws6_m_variants$rollback
  with_rollback <- .ws6_ref_match(ref_conn, s)
  without <- .ws6_ref_match(ref_conn, s, rollback = FALSE)
  # the fixture discriminates: 8001 is dropped (2 of 3 controls); its tentative
  # controls return to the pool, so 8002 is matched with 8010, 8011, 8012
  expect_true(8002 %in% with_rollback$person_id)
  expect_false(8002 %in% without$person_id)
  plan <- .ws6_m_plan(.ws6_m_variants[c("rollback", "none_keep", "between_drop")])
  ctx <- .ws4_ctx(.ws6_m_backends$sqlite$handle, plan)
  on.exit(.compileReleaseTempTables(ctx), add = TRUE)
  r <- .ws6_roster(ctx, "rollback")
  expect_false(8001 %in% r$person_id[r$is_case == 1])
  expect_setequal(r$person_id[r$set_id == r$set_id[r$person_id == 8002 & r$is_case == 1]], c(8002, 8010, 8011, 8012))
  # without replacement every control is used at most once; weights are NA
  n <- .ws6_roster(ctx, "none_keep")
  expect_false(anyDuplicated(n$person_id[n$is_case == 0]) > 0L)
  expect_true(all(is.na(n$w[n$is_case == 0])) && all(n$w[n$is_case == 1] == 1))
  # drop_case under between_sets: cases short of k (zero-candidate ones too) go
  bd <- .ws6_roster(ctx, "between_drop")
  expect_false(any(c(7006, 7007) %in% bd$person_id))
  expect_true(all(table(bd$set_id) == 5L))
  # set_id is the rank in the complete ordered case roster. Independently
  # obtain that order with keep_available, then map the retained cases back
  # to it. Cases dropped between retained ones leave holes in the keys.
  all_cases <- .ws6_ref_match(ref_conn, .ws6_m_variants$default)
  all_cases <- all_cases[all_cases$is_case == 1, , drop = FALSE]
  kept <- bd[bd$is_case == 1, , drop = FALSE]
  full_ranks <- all_cases$set_id[match(kept$person_id, all_cases$person_id)]
  expect_identical(kept$set_id, as.numeric(full_ranks))
  expect_true(any(diff(sort(unique(bd$set_id))) > 1))
})

test_that("mixed-union matching excludes undated persons and preserves all dated case ranks", {
  variants <- .ws6_m_variants[c("default", "between_drop")]
  plan <- .ws6_m_plan(variants)
  plan$populations$indexed <- plan$populations$cases
  plan$populations$plain <- list(kind = "criteria")
  plan$populations$cases <- list(kind = "setop", setop = list(op = "union", members = list("indexed", "plain")))
  ref_conn <- .ws6_m_backends$sqlite$handle$conn
  want <- lapply(variants, function(s) .ws6_ref_match(ref_conn, s))
  all_cases <- want$default[want$default$is_case == 1, , drop = FALSE]
  people <- nrow(DBI::dbGetQuery(ref_conn, "SELECT person_id FROM person"))
  zero_rank <- all_cases$set_id[all_cases$person_id == 7007]
  expect_length(zero_rank, 1L)
  for (b in names(.ws6_m_backends)) {
    h <- .ws6_m_backends[[b]]$handle
    ctx <- .ws4_ctx(h, plan)
    expect_length(.irSemanticErrors(ctx$ir), 0L)
    expect_silent(.irValidate(ctx$ir, h))
    expect_true(.populationIndexed(ctx$ir, "cases"), info = b)
    mixed <- .compilePopulationByName(ctx, "cases")
    input <- .fetchFragment(ctx, mixed$roster, "cohort_row_id")
    expect_equal(sum(is.na(input$cohort_start_date)), people, info = b)
    expect_equal(sum(!is.na(input$cohort_start_date)), nrow(all_cases), info = b)
    for (v in names(variants)) {
      got <- .ws6_roster(ctx, v)
      expect_false(anyNA(got$index_date), info = paste(b, v))
      expect_false(anyNA(got$end_date), info = paste(b, v))
      expect_equal(.ws6_m_sorted(got), .ws6_m_sorted(want[[v]]), info = paste(b, v))
      frozen <- .fragmentGet(ctx, .fragmentName("mt", v, "cases", ctx = ctx))
      cases <- .fetchFragment(ctx, frozen, "set_rank")
      expect_false(anyNA(cases$t), info = paste(b, v))
      expect_identical(as.numeric(cases$set_rank), as.numeric(seq_len(nrow(all_cases))), info = paste(b, v))
      expect_identical(as.numeric(cases$subject_id), as.numeric(all_cases$person_id), info = paste(b, v))
      pairs <- .fetchFragment(ctx, .compilePopulationByName(ctx, v)$pairs, "set_rank")
      expect_true(all(pairs$set_rank %in% cases$set_rank), info = paste(b, v))
      expect_false(zero_rank %in% as.numeric(pairs$set_rank), info = paste(b, v))
      if (identical(v, "default")) {
        zero <- got[got$set_id == zero_rank, , drop = FALSE]
        expect_identical(zero$person_id, 7007, info = b)
        expect_identical(zero$index_date, "2020-03-01", info = b)
        expect_identical(zero$is_case, 1, info = b)
        expect_identical(zero$w, 1, info = b)
      } else {
        expect_false(zero_rank %in% got$set_id, info = b)
        kept <- got[got$is_case == 1, , drop = FALSE]
        expect_identical(kept$set_id, as.numeric(all_cases$set_id[match(kept$person_id, all_cases$person_id)]), info = b)
        expect_true(any(diff(sort(unique(got$set_id))) > 1), info = b)
      }
    }
    .planContextClose(ctx)
  }
})

test_that("seed sensitivity and seed, order and dialect reproducibility", {
  plan <- .ws6_m_plan(.ws6_m_variants[c("default", "seed2")])
  ctx <- .ws4_ctx(.ws6_m_backends$sqlite$handle, plan)
  on.exit(.compileReleaseTempTables(ctx), add = TRUE)
  d <- .ws6_roster(ctx, "default")
  s2 <- .ws6_roster(ctx, "seed2")
  # same pools, different seeds: the selection differs on a nondegenerate pool
  expect_identical(nrow(d), nrow(s2))
  expect_false(identical(sort(d$person_id[d$set_id == 2]), sort(s2$person_id[s2$set_id == 2])))
  # the same plan again: identical
  ctx2 <- .ws4_ctx(.ws6_m_backends$sqlite$handle, plan)
  on.exit(.compileReleaseTempTables(ctx2), add = TRUE)
  expect_identical(.ws6_roster(ctx2, "default"), d)
  # fixture rows inserted in reverse order: identical roster
  shuffled <- .ws4_reach_handle()
  on.exit(try(DBI::dbDisconnect(shuffled$conn), silent = TRUE), add = TRUE)
  .ws6_match_extend(shuffled$conn, reverse = TRUE)
  shuffled$blueprint <- NULL
  shuffled$role_registry <- NULL
  ctx3 <- .ws4_ctx(shuffled, plan)
  on.exit(.compileReleaseTempTables(ctx3), add = TRUE, after = FALSE)
  expect_identical(.ws6_roster(ctx3, "default"), d)
})

test_that("the matching order computed in SQL equals the R order byte for byte", {
  plan <- .ws6_m_plan(.ws6_m_variants["default"])
  checked <- character(0)
  for (b in names(.ws6_m_backends)) {
    ctx <- .ws4_ctx(.ws6_m_backends[[b]]$handle, plan)
    .compilePopulationByName(ctx, "default")
    h <- .compileDialect(ctx)
    txt <- function(x) .sqlLosslessKeyText(h, x)
    hash <- .matchHashSql(h, 20261111L, "cases", txt("c.subject_id"), .sqlDateText(h, "c.t"),
                          .sqlDateText(h, "c.case_end"), "c.index_source", "c.index_key", txt("x.subject_id"))
    if (is.null(hash)) {
      expect_identical(b, "sqlite")
      .compileReleaseTempTables(ctx)
      next
    }
    pairs <- .fragmentGet(ctx, .fragmentName("mt", "default", "pairs", ctx = ctx))
    cases <- .fragmentGet(ctx, .fragmentName("mt", "default", "cases", ctx = ctx))
    # The main probe reads both snapshots. Raw template materialisation cuts
    # the old transitive cases dependency, so declare this extra probe read.
    pairs$dependencies <- unique(c(pairs$dependencies, cases$name))
    sql <- .buildStatement(ctx, pairs, select = paste0(
      "SELECT ", txt("c.subject_id"), " AS case_id, ", .sqlDateText(h, "c.t"), " AS t, ",
      .sqlDateText(h, "c.case_end"), " AS case_end, c.index_source AS src, c.index_key AS k, ",
      txt("x.subject_id"), " AS control, ", hash, " AS h FROM ", pairs$name, " x INNER JOIN ", cases$name,
      " c ON c.set_rank = x.set_rank"), dialect = h)
    rows <- .executeQuery(ctx$handle, sql)
    expect_gt(nrow(rows), 50L)
    r_hash <- vapply(seq_len(nrow(rows)), function(i) {
      .matchHash(20261111L, "cases", rows$case_id[i], rows$t[i], rows$case_end[i], rows$src[i], rows$k[i],
                 rows$control[i])
    }, "")
    expect_identical(tolower(as.character(rows$h)), r_hash, info = b)
    checked <- c(checked, b)
    .compileReleaseTempTables(ctx)
  }
  cat("\n[WS-6 SQL hash push-down checked] ", paste(checked, collapse = ", "), "\n")
})

test_that("one capped realisation: cap + 1 refuses without truncation, also when the pool grows", {
  ref <- .ws6_ref_match(.ws6_m_backends$sqlite$handle$conn, list(seed = 20261111L))
  total <- attr(ref, "pairs")
  h <- .ws4_reach_handle()
  on.exit(try(DBI::dbDisconnect(h$conn), silent = TRUE), add = TRUE)
  .ws6_match_extend(h$conn)
  h$blueprint <- NULL
  h$role_registry <- NULL
  at <- function(cap) .ws6_m_plan(list(default = list(seed = 20261111L, max_candidates = cap)))
  # exactly the pool: the complete realisation, never truncated
  ctx <- .ws4_ctx(h, at(total))
  r <- .ws6_roster(ctx, "default")
  expect_equal(.ws6_m_sorted(r), .ws6_m_sorted(ref))
  .compileReleaseTempTables(ctx)
  # one row short: the typed refusal, no count in the message
  ctx <- .ws4_ctx(h, at(total - 1L))
  err <- .ws4_expect_rejection(.compilePopulationByName(ctx, "default"), code = "match_candidates_exceeded")
  expect_false(grepl("[0-9]{2,}", conditionMessage(err)))
  .compileReleaseTempTables(ctx)
  # the pool changes between two runs: a new eligible control (M 1913) joins
  # four pools; the same cap now refuses
  DBI::dbExecute(h$conn, paste0("INSERT INTO person (person_id, gender_concept_id, year_of_birth, month_of_birth, ",
                                "day_of_birth, race_concept_id, ethnicity_concept_id) VALUES (7036, 8507, 1913, 1, 1, ",
                                "8527, 38003564)"))
  DBI::dbExecute(h$conn, paste0("INSERT INTO observation_period (observation_period_id, person_id, ",
                                "observation_period_start_date, observation_period_end_date, period_type_concept_id) ",
                                "VALUES (707036, 7036, '2010-01-01', '2030-12-31', 44814724)"))
  ctx <- .ws4_ctx(h, at(total))
  .ws4_expect_rejection(.compilePopulationByName(ctx, "default"), code = "match_candidates_exceeded")
  .compileReleaseTempTables(ctx)
  # the case roster is realised under the same stop (dsomop.max_memory_rows)
  withr::with_options(list(dsomop.max_memory_rows = 3L), {
    ctx <- .ws4_ctx(h, at(1000000L))
    .ws4_expect_rejection(.compilePopulationByName(ctx, "default"), code = "match_candidates_exceeded")
    .compileReleaseTempTables(ctx)
  })
})

test_that("the matched roster: identities, coverage handoff, derivations and the reviewed analytic keys (A-E10)", {
  plan <- .ws6_m_plan(.ws6_m_variants["default"])
  plan$populations$cc_union <- list(kind = "setop", setop = list(op = "union", members = list("default", "cases")))
  plan$populations$cc_child <- list(kind = "criteria", parent = "default",
                                    where = list(has = list(source = list(event = "str"))))
  plan$outputs$o_union <- list(rows = "cc_union", format = "wide")
  plan$outputs$o_child <- list(rows = "cc_child", format = "wide")
  for (b in names(.ws6_m_backends)) {
    h <- .ws6_m_backends[[b]]$handle
    ctx <- .ws4_ctx(h, plan)
    res <- .compilePopulationByName(ctx, "default")
    r <- .ws6_roster(ctx, "default")
    # every member indexed at its case time; the matched namespace
    expect_true(all(r$index_date == r$end_date), info = b)
    expect_true(all(r$index_source == "match:default"), info = b)
    # coverage handed over: op_* of the respective lookups, observed segments
    # through the transferred component (never a recompiled coverage)
    expect_false(anyNA(r$op_start), info = b)
    seg <- .fetchFragment(ctx, res$segments, "row_key")
    expect_setequal(as.numeric(unique(seg$row_key)), r$cohort_row_id)
    expect_true(inherits(res$pairs, "dsomop_fragment") && "set_rank" %in% res$pairs$private_columns, info = b)
    # a union keeps every matched identity; a child keeps set_id / is_case / w
    u <- .ws6_roster(ctx, "cc_union")
    expect_true(all(.ws6_key(r, c("person_id", "index_date", "index_key")) %in%
                      .ws6_key(u, c("person_id", "index_date", "index_key"))), info = b)
    ch <- .ws6_roster(ctx, "cc_child")
    expect_true(all(ch$person_id %in% 7010:7012), info = b)
    expect_true(all(!is.na(ch$set_id) & ch$is_case == 0), info = b)
    # A-E10: set_id and cohort_row_id are reviewed analytic keys that survive
    # pseudonymisation; private identity columns are stripped by publication
    frame <- .fetchFragment(ctx, res$roster, "cohort_row_id")
    frame <- frame[setdiff(names(frame), res$roster$private_columns)]
    tok <- .testPseudonymize(frame)
    expect_true(all(c("cohort_row_id", "set_id", "is_case", "sampling_weight") %in% names(tok)), info = b)
    expect_identical(as.numeric(tok$set_id), as.numeric(frame$set_id))
    expect_false(any(as.character(tok$subject_id) %in% as.character(frame$subject_id)), info = b)
    expect_true(all(c("cohort_row_id", "set_id") %in% attr(tok, "dsomop_protected")), info = b)
    .compileReleaseTempTables(ctx)
  }
})

test_that("matching carries case visit and origin while union gaps never enter the risk set", {
  extend <- function(conn) {
    .ws6_match_extend(conn)
    # One case has a real index visit. Control 7013 is in the tolerated gap
    # on 1 March, but in a recorded segment for the later case on 1 June.
    DBI::dbExecute(conn, "UPDATE condition_occurrence SET visit_occurrence_id = 771001 WHERE condition_occurrence_id = 970001")
    DBI::dbExecute(conn, "UPDATE observation_period SET observation_period_end_date = '2020-02-28' WHERE person_id = 7013")
    DBI::dbExecute(conn, paste0(
      "INSERT INTO observation_period (observation_period_id, person_id, observation_period_start_date, ",
      "observation_period_end_date, period_type_concept_id) VALUES (777013, 7013, '2020-03-03', '2030-12-31', 44814724)"))
  }
  backends <- .ws6_backends(extend = extend)
  on.exit(.ws4_cleanup(backends), add = TRUE)
  plan <- .ws6_m_plan(list(default = list(seed = 20261111L, controls_per_case = 100L)))
  policy <- list(policy = "union", gap = 7L)
  plan$populations$cases$coverage <- policy
  plan$populations$tpl_default$coverage <- policy
  first <- NULL
  for (b in names(backends)) {
    ctx <- .ws4_ctx(backends[[b]]$handle, plan)
    result <- .compilePopulationByName(ctx, "default")
    r <- .fetchFragment(ctx, result$roster, "cohort_row_id")
    cs <- r[r$subject_id == 7001 & r$is_case == 1, , drop = FALSE]
    expect_identical(as.character(cs$index_visit_id), "771001", info = b)
    expect_identical(cs$origin_source, "out", info = b)
    expect_identical(cs$origin_key, "970001", info = b)
    expect_true(all(is.na(r$index_visit_id[r$is_case == 0])), info = b)
    expect_true(all(is.na(r$origin_source[r$is_case == 0]) & is.na(r$origin_key[r$is_case == 0])), info = b)
    expect_true(all(c("index_visit_id", "origin_source", "origin_key") %in% result$roster$private_columns), info = b)
    early <- r$set_id[r$subject_id == 7001 & r$is_case == 1]
    later <- r$set_id[r$subject_id == 7003 & r$is_case == 1]
    expect_false(any(r$subject_id == 7013 & r$set_id == early), info = b)
    expect_true(any(r$subject_id == 7013 & r$set_id == later & r$is_case == 0), info = b)
    member <- r[r$subject_id == 7013 & r$set_id == later, , drop = FALSE]
    expect_identical(as.character(member$op_start), "2010-01-01", info = b)
    # Its selected component has two actual recorded segments; transferred
    # final row ids recover those segments, without turning the gap into time.
    seg <- .fetchFragment(ctx, result$segments, "row_key")
    seg <- seg[seg$row_key %in% member$cohort_row_id, , drop = FALSE]
    expect_identical(nrow(seg), 2L, info = b)
    expect_setequal(as.character(seg$start_date), c("2010-01-01", "2020-03-03"))
    got <- .ws6_roster(ctx, "default")
    if (is.null(first)) first <- got else expect_identical(got, first, info = b)
    .planContextClose(ctx)
  }
})

test_that("the exact sampler breaks hash ties with lossless signed integer keys", {
  cases <- data.frame(set_rank = "1", subject_id = "9223372036854775807", t = "2020-01-01",
                       case_end = "2020-01-01", index_source = "out", index_key = "9007199254740993",
                       index_visit_id = "9007199254740995", origin_source = "out",
                       origin_key = "9007199254740993", op_start = "2010-01-01", op_end = "2030-01-01",
                       op_component_id = "1", stringsAsFactors = FALSE)
  ids <- c("9007199254740993", "10", "-9007199254740993", "9007199254740992", "9", "-9007199254740992")
  pool <- data.frame(set_rank = "1", subject_id = ids, op_start = "2010-01-01", op_end = "2030-01-01",
                     op_component_id = "2", stringsAsFactors = FALSE)
  spec <- .matchSpec(list(match = list(cases = "cases", controls = "template", controls_per_case = 6L)), "m")
  # A forced collision proves the secondary order is numeric without ever
  # converting adjacent integers above 2^53 to doubles.
  testthat::local_mocked_bindings(.matchHashes = function(prefix, controls) rep("same", length(controls)))
  got <- .matchSelect(spec, "m", cases, pool)
  want <- c("-9007199254740993", "-9007199254740992", "9", "10", "9007199254740992", "9007199254740993")
  expect_identical(got$subject_id[got$role == 1], want)
  expect_identical(got$index_visit_id[got$role == 0], "9007199254740995")
  expect_identical(got$origin_key[got$role == 0], "9007199254740993")
  expect_identical(.matchSelect(spec, "m", cases, pool[rev(seq_len(nrow(pool))), ]), got)
})

test_that("matching insertion failures clean up immediately and retain failed DROP for retry", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  .ws6_match_extend(h$conn)
  h$blueprint <- NULL
  h$role_registry <- NULL
  pre <- .createTempTable(h, "ws6_match_preexisting", "SELECT 29 AS value")
  physical <- function() DBI::dbGetQuery(h$conn, "SELECT name FROM sqlite_temp_master WHERE type = 'table'")$name
  create <- .createTempTable
  rejecting_create <- function(handle, name, select_sql) {
    result <- create(handle, name, select_sql)
    if (startsWith(name, "dsomop_mt_")) DBI::dbExecute(handle$conn, paste0(
      "CREATE TEMP TRIGGER ws6_match_reject_insert BEFORE INSERT ON ", name,
      " BEGIN SELECT RAISE(ABORT, 'injected matching insertion failure'); END"))
    result
  }
  for (drop_fails in c(FALSE, TRUE)) {
    ctx <- .ws4_ctx(h, .ws6_m_plan(.ws6_m_variants["default"]))
    drop <- if (drop_fails) function(...) stop("injected DROP failure") else .dropTempTable
    testthat::with_mocked_bindings({
      error <- tryCatch(.compilePopulationByName(ctx, "default"), error = identity)
    }, .createTempTable = rejecting_create, .dropTempTable = drop)
    expect_match(conditionMessage(error), "injected matching insertion failure")
    if (drop_fails) {
      expect_match(conditionMessage(error), "Could not release the compiler temporary table")
      expect_s3_class(error$cleanup_error, "error")
      owned_match <- ctx$compile_temp_tables[startsWith(ctx$compile_temp_tables, "dsomop_mt_")]
      expect_length(owned_match, 1L)
      expect_true(owned_match %in% h$temp_tables)
      expect_true(owned_match %in% physical())
    } else {
      expect_false(any(startsWith(ctx$compile_temp_tables, "dsomop_mt_")))
      expect_false(any(startsWith(physical(), "dsomop_mt_")))
    }
    .planContextClose(ctx)
    expect_setequal(h$temp_tables, pre)
    expect_setequal(physical(), pre)
    expect_length(ctx$compile_temp_tables, 0L)
  }
})

test_that("case ranks and strata stay fixed when source rows change between the two bounded fetches", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  .ws6_match_extend(h$conn)
  h$blueprint <- NULL
  h$role_registry <- NULL
  plan <- .ws6_m_plan(.ws6_m_variants["default"])
  ctx <- .ws4_ctx(h, plan)
  on.exit(.planContextClose(ctx), add = TRUE, after = FALSE)
  fetch <- .matchFetchBounded
  realised_cases <- NULL
  testthat::local_mocked_bindings(.matchFetchBounded = function(ctx, final, select, ...) {
    rows <- fetch(ctx, final, select, ...)
    if ("case_row_id" %in% names(final$columns)) {
      realised_cases <<- rows
      # This changes the first-eligible case, its rank and its age/sex strata
      # under a fresh compilation. The already realised cases remain fixed.
      DBI::dbExecute(h$conn, paste0(
        "UPDATE condition_occurrence SET condition_start_date = '2017-01-01', ",
        "condition_end_date = '2017-01-01' WHERE condition_occurrence_id = 970001"))
      DBI::dbExecute(h$conn, "UPDATE person SET gender_concept_id = 8532, year_of_birth = 1925 WHERE person_id = 7001")
    }
    rows
  })
  res <- .compilePopulationByName(ctx, "default")
  expect_s3_class(realised_cases, "data.frame")
  fc <- .fragmentGet(ctx, .fragmentName("mt", "default", "cases", ctx = ctx))
  after <- .fetchFragment(ctx, fc, "set_rank")
  expect_identical(as.character(after$subject_id), realised_cases$subject_id[order(as.numeric(realised_cases$set_rank))])
  expect_identical(as.character(after$t), realised_cases$t[order(as.numeric(realised_cases$set_rank))])
  original <- after[after$subject_id == 7001, , drop = FALSE]
  expect_identical(as.character(original$t), "2020-03-01")
  expect_identical(as.numeric(original$s_1), 8507)
  expect_identical(as.numeric(original$s_2), 21)
  r <- .fetchFragment(ctx, res$roster, "cohort_row_id")
  matched <- r[r$is_case == 1, , drop = FALSE]
  matched <- matched[order(as.numeric(matched$set_id)), , drop = FALSE]
  expect_identical(as.character(matched$subject_id), as.character(after$subject_id))
  expect_identical(as.character(matched$cohort_start_date), as.character(after$t))
})
