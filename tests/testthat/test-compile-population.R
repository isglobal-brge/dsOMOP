# WS-6: populations (design-2.10.3 sections 2.5, 6.1, 6.9 and 6.13; amendments
# A-1, A-20; defects B-8 and A-E4) -- index candidates and selection
# (first / last / nth / all / first_eligible, the legacy select-before-
# eligibility order), any_of arms, offsets, every end strategy capped at
# op_end, coverage policies at the population level, the criteria leaves
# with three-valued logic and the consumer associations, criteria, seeded and
# child populations, set operations on full origin identity, the plan scope
# and the population semantic rules -- on every backend available, over the
# scenario rows of helper-ws6.R (persons 6001..6019).

.ws6_pop_backends <- .ws6_backends(extend = .ws6_population_extend)
withr::defer(.ws4_cleanup(.ws6_pop_backends), testthat::teardown_env())

.ws6_ev <- function() {
  W <- .WS6
  list(
    idx = .ws6_event("condition_occurrence", W$IDX),
    anc = .ws6_event("condition_occurrence", W$ANC),
    other = .ws6_event("condition_occurrence", W$OTHER),
    anc_or_other = .ws6_event("condition_occurrence", c(W$ANC, W$OTHER)),
    z_earlier = .ws6_event("condition_occurrence", W$Z),
    a_later = .ws6_event("condition_occurrence", W$A),
    arm1 = .ws6_event("condition_occurrence", W$ARM),
    arm2 = .ws6_event("condition_occurrence", c(W$ARM, W$ARM2)),
    lab = list(table = "measurement", concept_set = list(concepts = list(W$LAB)),
               value = list(column = "value_as_number")),
    proc = .ws6_event("procedure_occurrence", W$PROC),
    proc_visit = c(.ws6_event("procedure_occurrence", W$PROC), list(visit = list(index = TRUE))))
}

.ws6_idx <- function(select = "all", where = NULL, source = list(event = "idx"), ...) {
  pop <- list(kind = "index", index = c(list(source = source, select = select), list(...)))
  if (!is.null(where)) pop$where <- where
  pop
}
.ws6_has <- function(event, ..., window = NULL) {
  leaf <- list(source = list(event = event), ...)
  if (!is.null(window)) leaf$window <- window
  list(has = leaf)
}
.ws6_win <- function(start, end, anchor = "index") list(start = start, end = end, anchor = anchor)

# Run one population of a plan on every backend; rows of the WS-6 persons.
.ws6_pop <- function(plan, pop, persons = 6001:6019) {
  out <- list()
  for (b in names(.ws6_pop_backends)) {
    h <- .ws6_pop_backends[[b]]$handle
    ctx <- .ws4_ctx(h, plan)
    r <- tryCatch(.ws6_roster(ctx, pop), finally = .compileReleaseTempTables(ctx))
    out[[b]] <- r[r$person_id %in% persons, , drop = FALSE]
  }
  # identical rosters on every backend
  for (b in names(out)[-1L]) testthat::expect_identical(out[[b]], out[[1L]], info = paste(pop, b))
  out[[1L]]
}

.ws6_dates <- function(r, person) r$index_date[r$person_id == person]

test_that("selection: first_eligible, first, last, nth, all (legacy select-before-eligibility order)", {
  crit <- .ws6_has("anc", window = .ws6_win(-60L, 0L))
  plan <- .ws6_plan(.ws6_ev(), list(
    fe = .ws6_idx("first_eligible", crit), fi = .ws6_idx("first", crit), la = .ws6_idx("last", crit),
    al = .ws6_idx("all", crit), n2 = .ws6_idx("nth", crit, n = 2L), raw_first = .ws6_idx("first"),
    raw_last = .ws6_idx("last")))
  fe <- .ws6_pop(plan, "fe")
  fi <- .ws6_pop(plan, "fi")
  la <- .ws6_pop(plan, "la")
  al <- .ws6_pop(plan, "al")
  n2 <- .ws6_pop(plan, "n2")
  # 6001: only the second candidate has ANC in [-60, 0]
  expect_identical(.ws6_dates(fe, 6001), "2020-01-10")
  expect_identical(.ws6_dates(fi, 6001), character(0))
  expect_identical(.ws6_dates(la, 6001), "2020-01-10")
  expect_identical(.ws6_dates(al, 6001), "2020-01-10")
  expect_identical(.ws6_dates(n2, 6001), "2020-01-10")
  # 6002 is the legacy order fixture (test-plan-index-event.R): first -> none,
  # last -> 2020-07-01, all -> 2020-07-01; first_eligible finds 2020-07-01
  expect_identical(.ws6_dates(fi, 6002), character(0))
  expect_identical(.ws6_dates(la, 6002), "2020-07-01")
  expect_identical(.ws6_dates(al, 6002), "2020-07-01")
  expect_identical(.ws6_dates(fe, 6002), "2020-07-01")
  expect_identical(.ws6_dates(.ws6_pop(plan, "raw_first"), 6002), "2020-01-01")
  expect_identical(.ws6_dates(.ws6_pop(plan, "raw_last"), 6002), "2020-07-01")
  # identities: source namespace and the decimal source key
  expect_true(all(fe$index_source == "idx") && all(grepl("^[0-9]+$", fe$index_key)))
})

test_that("first_eligible with an anchor that exists only for the second candidate", {
  anchored <- list(start = list(anchor = list(source = list(event = "anc"), which = "first",
                                              search = .ws6_win(-60L, 0L)), offset = 0L),
                   end = list(anchor = "index", offset = 0L), anchor = "index")
  crit <- .ws6_has("other", window = anchored)
  plan <- .ws6_plan(.ws6_ev(), list(fe = .ws6_idx("first_eligible", crit),
                                    fe_not = .ws6_idx("first_eligible", list(not = crit)),
                                    al_not = .ws6_idx("all", list(not = crit))))
  # the first candidate's anchor is missing (UNKNOWN): the second is TRUE
  expect_identical(.ws6_dates(.ws6_pop(plan, "fe"), 6001), "2020-01-10")
  # NOT UNKNOWN stays UNKNOWN: no candidate of 6001 is TRUE under the negation
  expect_identical(.ws6_dates(.ws6_pop(plan, "fe_not"), 6001), character(0))
  expect_identical(.ws6_dates(.ws6_pop(plan, "al_not"), 6001), character(0))
})

test_that("a missing anchor is UNKNOWN for has, has_not and count == 0", {
  anchored <- list(start = list(anchor = list(source = list(event = "anc")), offset = 0L),
                   end = list(anchor = "index", offset = 0L), anchor = "index")
  plan <- .ws6_plan(.ws6_ev(), list(
    has_not = .ws6_idx("all", .ws6_has("other", op = "==", n = 0L, window = anchored)),
    count0 = .ws6_idx("all", .ws6_has("other", op = "<=", n = 0L, window = anchored)),
    neg = .ws6_idx("all", list(not = .ws6_has("other", window = anchored))),
    unbounded = .ws6_idx("all", .ws6_has("other", op = "==", n = 0L))))
  # 6016 has no ANC: every anchored leaf is UNKNOWN and drops the episode
  for (p in c("has_not", "count0", "neg")) expect_false(6016 %in% .ws6_pop(plan, p)$person_id, info = p)
  # without the anchored window the absence is TRUE
  expect_true(6016 %in% .ws6_pop(plan, "unbounded")$person_id)
  # 6001 has ANC (first 2019-12-01) and OTHER inside [anchor, index] for the
  # 2020-01-10 candidate: has_not is FALSE there; before the anchor the window
  # is empty for the 2019-01-10 candidate: TRUE
  expect_identical(.ws6_dates(.ws6_pop(plan, "has_not"), 6001), "2019-01-10")
})

test_that("has: rows, distinct days, visits and concepts, between, nonmissing", {
  w <- .ws6_win(-30L, 0L)
  plan <- .ws6_plan(.ws6_ev(), list(
    rows4 = .ws6_idx("all", .ws6_has("anc", op = "==", n = 4L, window = w)),
    days2 = .ws6_idx("all", .ws6_has("anc", op = "==", n = 2L, window = w, distinct = "day")),
    visits2 = .ws6_idx("all", .ws6_has("anc", op = "==", n = 2L, window = w, distinct = "visit")),
    concepts2 = .ws6_idx("all", .ws6_has("anc_or_other", op = "==", n = 2L, window = w, distinct = "concept")),
    between34 = .ws6_idx("all", .ws6_has("anc", op = "between", n = list(3L, 4L), window = w)),
    between59 = .ws6_idx("all", .ws6_has("anc", op = "between", n = list(5L, 9L), window = w)),
    ge2 = .ws6_idx("all", .ws6_has("anc", op = ">=", n = 2L, window = w)),
    lab = .ws6_idx("all", .ws6_has("lab")),
    lab_value = .ws6_idx("all", .ws6_has("lab", nonmissing = "value_as_number")),
    no_lab_value = .ws6_idx("all", .ws6_has("lab", op = "==", n = 0L, nonmissing = "value_as_number"))))
  inn <- function(p, person) person %in% .ws6_pop(plan, p)$person_id
  for (p in c("rows4", "days2", "visits2", "concepts2", "between34", "ge2")) expect_true(inn(p, 6010), info = p)
  expect_false(inn("between59", 6010))
  expect_true(inn("lab", 6011))
  expect_false(inn("lab_value", 6011))
  expect_true(inn("no_lab_value", 6011))
})

test_that("person and age criteria: NULL and future birth years are UNKNOWN", {
  plan <- .ws6_plan(.ws6_ev(), list(
    person = .ws6_idx("all", list(person = list(var = "year_of_birth", op = ">=", value = 1960L))),
    not_person = .ws6_idx("all", list(not = list(person = list(var = "year_of_birth", op = ">=", value = 1960L)))),
    age = .ws6_idx("all", list(age = list(max = 60L))),
    not_age = .ws6_idx("all", list(not = list(age = list(max = 60L)))),
    age_fixed = .ws6_idx("all", list(age = list(min = 69L, max = 71L, reference = list(year = 2020L))))))
  persons <- c(6012, 6013, 6014, 6016)
  get <- function(p) sort(intersect(.ws6_pop(plan, p)$person_id, persons))
  expect_identical(get("person"), c(6013, 6016))
  expect_identical(get("not_person"), 6014)
  expect_identical(get("age"), 6016)
  expect_identical(get("not_age"), 6014)
  expect_identical(get("age_fixed"), 6014)
})

test_that("observed: the population's policy, and a criterion-specific union with gap", {
  plan <- .ws6_plan(.ws6_ev(), list(
    obs = .ws6_idx("all", list(observed = list(before = 365L))),
    obs_gap = .ws6_idx("all", list(observed = list(before = 365L, gap = 7L))),
    obs_after = .ws6_idx("all", list(observed = list(after = 3650L)))))
  # 6009: indexed in its second record (2020-01-09..); 6 uncovered days before
  expect_false(6009 %in% .ws6_pop(plan, "obs")$person_id)
  expect_true(6009 %in% .ws6_pop(plan, "obs_gap")$person_id)
  expect_true(6016 %in% .ws6_pop(plan, "obs_after")$person_id)
  expect_false(6005 %in% .ws6_pop(plan, "obs_after")$person_id)
})

test_that("coverage policies at the population level (A-E6): unique, earliest, union, drop, keep, error", {
  pops <- list(
    uni = .ws6_idx("all"),
    ear = c(.ws6_idx("all"), list(coverage = list(policy = "earliest"))),
    un0 = c(.ws6_idx("all"), list(coverage = list(policy = "union", gap = 0L))),
    un7 = c(.ws6_idx("all"), list(coverage = list(policy = "union", gap = 7L))),
    keep = c(.ws6_idx("all"), list(coverage = list(policy = "unique", on_missing = "keep"))),
    err = c(.ws6_idx("all"), list(coverage = list(policy = "unique", on_ambiguous = "error"))))
  plan <- .ws6_plan(.ws6_ev(), pops)
  uni <- .ws6_pop(plan, "uni")
  expect_false(any(c(6006, 6007, 6008) %in% uni$person_id))
  ear <- .ws6_pop(plan, "ear")
  expect_identical(c(ear$op_start[ear$person_id == 6006], ear$op_end[ear$person_id == 6006]),
                   c("2015-01-01", "2020-06-30"))
  un0 <- .ws6_pop(plan, "un0")
  expect_identical(c(un0$op_start[un0$person_id == 6006], un0$op_end[un0$person_id == 6006]),
                   c("2015-01-01", "2030-12-31"))
  # the gap-index case: [01-01,02] + [01-10,11] coalesce under gap 7, but the
  # index on 01-05 lies in no recorded period -- uncovered
  expect_false(6008 %in% .ws6_pop(plan, "un7")$person_id)
  keep <- .ws6_pop(plan, "keep")
  expect_true(6007 %in% keep$person_id)
  expect_true(is.na(keep$op_start[keep$person_id == 6007]) && is.na(keep$end_date[keep$person_id == 6007]))
  expect_false(6006 %in% keep$person_id)
  for (b in names(.ws6_pop_backends)) {
    ctx <- .ws4_ctx(.ws6_pop_backends[[b]]$handle, plan)
    .ws4_expect_rejection(.compilePopulationByName(ctx, "err"), code = "coverage_ambiguous",
                          reason = "coverage_ambiguous")
  }
})

test_that("ends: every strategy capped at op_end when covered; offsets; end before start is ineligible", {
  plan <- .ws6_plan(.ws6_ev(), list(
    obs_end = .ws6_idx("all"),
    fixed60 = .ws6_idx("all", end = list(strategy = "fixed", days = 60L)),
    fixed10 = .ws6_idx("all", end = list(strategy = "fixed", days = 10L)),
    event_end = .ws6_idx("all", end = list(strategy = "event_end")),
    event_pad = .ws6_idx("all", end = list(strategy = "event_end", pad = 5L)),
    era_end = .ws6_idx("all", source = list(interval = "idx_era"), end = list(strategy = "interval_end")),
    offset7 = .ws6_idx("all", offset = 7L, end = list(strategy = "fixed", days = 0L)),
    negative = .ws6_idx("all", end = list(strategy = "fixed", days = -5L)),
    keep_fixed = c(.ws6_idx("all", end = list(strategy = "fixed", days = 10L)),
                   list(coverage = list(on_missing = "keep"))),
    keep_event = c(.ws6_idx("all", end = list(strategy = "event_end")),
                   list(coverage = list(on_missing = "keep")))),
    intervals = list(idx_era = list(source = list(event = "idx"), gap = 0L)))
  end_of <- function(p, person) {
    r <- .ws6_pop(plan, p)
    r$end_date[r$person_id == person]
  }
  # 6005: observed until 2020-03-31, index 2020-03-01, event end 2020-06-30
  expect_identical(end_of("obs_end", 6005), "2020-03-31")
  expect_identical(end_of("fixed60", 6005), "2020-03-31")
  expect_identical(end_of("fixed10", 6005), "2020-03-11")
  expect_identical(end_of("event_end", 6005), "2020-03-31")
  expect_identical(end_of("event_pad", 6001), c("2019-01-15", "2020-01-15"))
  expect_identical(end_of("era_end", 6005), "2020-03-31")
  r <- .ws6_pop(plan, "offset7")
  expect_identical(c(.ws6_dates(r, 6005), r$end_date[r$person_id == 6005]), c("2020-03-08", "2020-03-08"))
  expect_false(6005 %in% .ws6_pop(plan, "negative")$person_id)
  # an uncovered candidate kept under on_missing = "keep" keeps its finite end
  expect_identical(end_of("keep_fixed", 6007), "2020-01-25")
  expect_identical(end_of("keep_event", 6007), "2020-01-15")
})

test_that("any_of: arm labels, one event in two arms, and chronological row numbers across sources", {
  plan <- .ws6_plan(.ws6_ev(), list(
    arms = .ws6_idx("all", source = list(any_of = list(list(event = "arm1"), list(event = "arm2")))),
    arms_first = .ws6_idx("first", source = list(any_of = list(list(event = "arm1"), list(event = "arm2")))),
    two = .ws6_idx("all", source = list(any_of = list(list(event = "z_earlier"), list(event = "a_later"))))))
  arms <- .ws6_pop(plan, "arms")
  a <- arms[arms$person_id == 6004, ]
  expect_identical(sort(a$index_source), c("arm1", "arm2"))
  expect_identical(length(unique(a$index_key)), 1L)
  expect_identical(.ws6_pop(plan, "arms_first")$index_source[.ws6_pop(plan, "arms_first")$person_id == 6004], "arm1")
  # two sources z_earlier (2020-01-01) and a_later (2020-12-01): January gets
  # the lower row number although its source name sorts later
  two <- .ws6_pop(plan, "two")
  t3 <- two[two$person_id == 6003, ]
  t3 <- t3[order(t3$cohort_row_id), ]
  expect_identical(t3$index_source, c("z_earlier", "a_later"))
  expect_identical(t3$index_date, c("2020-01-01", "2020-12-01"))
})

test_that("consumer association (A-20 a): index visits in criteria", {
  plan <- .ws6_plan(.ws6_ev(), list(
    same_visit = .ws6_idx("all", .ws6_has("proc_visit")),
    any_proc = .ws6_idx("all", .ws6_has("proc"))))
  # two candidates of 6015 on the same day, visits 601501 and 601502; the
  # procedure is on 601501 only: [1, 0], not [1, 1]
  for (b in names(.ws6_pop_backends)) {
    ctx <- .ws4_ctx(.ws6_pop_backends[[b]]$handle, plan)
    res <- .compilePopulationByName(ctx, "same_visit")
    cand <- .fetchFragment(ctx, .fragmentGet(ctx, .fragmentName("cand", "same_visit", "crit", ctx = ctx)),
                           "cand_key")
    cand <- cand[as.numeric(cand$subject_id) == 6015, ]
    expect_identical(as.numeric(cand$crit_true[order(as.numeric(cand$index_visit_id))]), c(1, 0), info = b)
  }
  expect_identical(nrow(.ws6_pop(plan, "same_visit", 6015)), 1L)
  expect_identical(nrow(.ws6_pop(plan, "any_proc", 6015)), 2L)
})

test_that("consumer association (A-20 a): episode members bind the roster's parent episode", {
  plan <- .ws5_acceptance_plan("14-oncology-episodes")
  plan$populations$two_members <- list(kind = "index", index = list(source = list(event = "episodes"), select = "all",
                                                                   end = list(strategy = "event_end")),
                                       where = .ws6_has("members", op = ">=", n = 2L))
  plan$populations$no_member_drug <- list(kind = "index", index = list(source = list(event = "episodes"),
                                                                      select = "all", end = list(strategy = "event_end")),
                                          where = .ws6_has("member_drugs", op = "==", n = 0L))
  plan$outputs$o2 <- list(rows = "two_members", format = "wide")
  plan$outputs$o3 <- list(rows = "no_member_drug", format = "wide")
  want <- .ws5_expected("14-oncology-episodes", "episodes")
  for (b in names(.ws6_pop_backends)) {
    ctx <- .ws4_ctx(.ws6_pop_backends[[b]]$handle, plan)
    r2 <- .ws6_roster(ctx, "two_members")
    r0 <- .ws6_roster(ctx, "no_member_drug")
    # per-episode member counts of the oracle (an event linked to two episodes
    # counts once in each)
    expect_setequal(r2$index_key, want$index_key[want$n_members >= 2])
    expect_setequal(r0$index_key, want$index_key[want$n_member_drugs == 0])
  }
})

test_that("results-cohort criterion and a seeded criteria population with each episode policy", {
  crit <- .ws6_has("anc", window = .ws6_win(0L, 0L))
  seeded <- function(policy) list(kind = "criteria", cohort_definition_id = 93002L, episode_policy = policy,
                                  where = crit)
  plan <- .ws6_plan(.ws6_ev(), list(
    in93001 = .ws6_idx("all", list(cohort = list(cohort_definition_id = 93001L))),
    any = seeded("any_episode"), all = seeded("all_episodes"), first = seeded("first_episode"),
    last = seeded("last_episode"),
    plain = list(kind = "criteria", cohort_definition_id = 93002L, where = .ws6_has("anc"))))
  expect_identical(intersect(.ws6_pop(plan, "in93001")$person_id, 6001:6019), 6017)
  get <- function(p) sort(.ws6_pop(plan, p, 6018:6019)$person_id)
  expect_identical(get("any"), c(6018, 6019))
  expect_identical(get("all"), 6019)
  expect_identical(get("first"), 6019)
  expect_identical(get("last"), c(6018, 6019))
  # index-independent criteria need no episode policy; the roster is persons
  plain <- .ws6_pop(plan, "plain", 6018:6019)
  expect_identical(sort(plain$person_id), c(6018, 6019))
  expect_true(all(is.na(plain$index_date)) && all(is.na(plain$index_source)))
})

test_that("criteria populations are persons; index-dependent criteria on a person roster are UNKNOWN", {
  plan <- .ws6_plan(.ws6_ev(), list(
    with_lab = list(kind = "criteria", where = .ws6_has("lab")),
    observed = list(kind = "criteria", where = list(observed = list(before = 1L))),
    everyone = list(kind = "criteria")))
  r <- .ws6_pop(plan, "with_lab")
  expect_identical(r$person_id, 6011)
  expect_identical(nrow(.ws6_pop(plan, "observed")), 0L)
  expect_identical(sort(.ws6_pop(plan, "everyone")$person_id), as.numeric(6001:6019))
})

test_that("parent (B-8): the three child shapes are subsets of their parent", {
  plan <- .ws6_plan(.ws6_ev(), list(
    all_idx = .ws6_idx("all"),
    child = list(kind = "criteria", parent = "all_idx", where = .ws6_has("anc", window = .ws6_win(-60L, 0L))),
    anc_persons = list(kind = "criteria", where = .ws6_has("anc")),
    indexed_child = c(.ws6_idx("all"), list(parent = "anc_persons")),
    z_pop = .ws6_idx("all", source = list(event = "z_earlier")),
    a_pop = .ws6_idx("all", source = list(event = "a_later")),
    z_persons = list(kind = "criteria", where = .ws6_has("z_earlier")),
    lab_persons = list(kind = "criteria", where = .ws6_has("lab")),
    za = list(kind = "setop", setop = list(op = "union", members = list("z_pop", "a_pop")), parent = "z_persons"),
    za_none = list(kind = "setop", setop = list(op = "union", members = list("z_pop", "a_pop")), parent = "lab_persons")))
  all_idx <- .ws6_pop(plan, "all_idx")
  child <- .ws6_pop(plan, "child")
  # a child without an index inherits the parent's episodes, then filters them
  expect_true(all(.ws6_key(child, c("person_id", "index_date", "index_key")) %in%
                    .ws6_key(all_idx, c("person_id", "index_date", "index_key"))))
  expect_identical(.ws6_dates(child, 6001), "2020-01-10")
  expect_identical(.ws6_dates(child, 6002), "2020-07-01")
  expect_false(6016 %in% child$person_id)
  # an index child keeps its own episodes of the parent's persons
  ic <- .ws6_pop(plan, "indexed_child")
  expect_identical(sort(unique(ic$person_id)), c(6001, 6002, 6010))
  expect_identical(.ws6_dates(ic, 6001), c("2019-01-10", "2020-01-10"))
  # a set-op child is narrowed after the set operation
  expect_identical(nrow(.ws6_pop(plan, "za", 6003)), 2L)
  expect_identical(nrow(.ws6_pop(plan, "za_none", 6003)), 0L)
})

test_that("set operations on full origin identity (A-E4): union keeps distinct sources, deduplicates identities", {
  plan <- .ws6_plan(.ws6_ev(), list(
    arm1_pop = .ws6_idx("all", source = list(event = "arm1")),
    arm2_pop = .ws6_idx("all", source = list(event = "arm2")),
    idx_a = .ws6_idx("all"), idx_b = .ws6_idx("all"),
    anc_persons = list(kind = "criteria", where = .ws6_has("anc")),
    u_arms = list(kind = "setop", setop = list(op = "union", members = list("arm1_pop", "arm2_pop"))),
    u_same = list(kind = "setop", setop = list(op = "union", members = list("idx_a", "idx_b"))),
    inter = list(kind = "setop", setop = list(op = "intersect", members = list("idx_a", "anc_persons"))),
    diff = list(kind = "setop", setop = list(op = "setdiff", members = list("idx_a", "anc_persons")))))
  # one physical event of 6004 seen by two sources: two episodes (same date)
  u <- .ws6_pop(plan, "u_arms", 6004)
  expect_identical(sort(u$index_source), c("arm1", "arm2"))
  # identical identities are one episode
  idx_a <- .ws6_pop(plan, "idx_a")
  u_same <- .ws6_pop(plan, "u_same")
  expect_identical(u_same[, -1L], idx_a[, -1L])
  inter <- .ws6_pop(plan, "inter")
  expect_identical(sort(unique(inter$person_id)), c(6001, 6002, 6010))
  expect_identical(.ws6_dates(inter, 6001), c("2019-01-10", "2020-01-10"))
  diff <- .ws6_pop(plan, "diff")
  expect_false(any(c(6001, 6002, 6010) %in% diff$person_id))
  expect_true(6016 %in% diff$person_id)
})

test_that("a union whose members carry one identity with contradictory metadata is refused", {
  plan <- .ws6_plan(.ws6_ev(), list(
    p1 = .ws6_idx("all"), p2 = .ws6_idx("all"),
    u = list(kind = "setop", setop = list(op = "union", members = list("p1", "p2")))))
  for (b in names(.ws6_pop_backends)) {
    h <- .ws6_pop_backends[[b]]$handle
    ctx <- .ws4_ctx(h, plan)
    hh <- .compileDialect(ctx)
    row <- function(w) {
      over <- c(list(subject_id = .sqlIntegerLiteral(hh, 6001L), cohort_start_date = .sqlDateLiteral(hh, "2020-01-10"),
                     index_source = "'idx'", source_rank = "1", key_1 = .sqlLosslessKey(hh, "1"),
                     sampling_weight = .predicateNumericLiteral(w)),
                .populationNulls(hh, setdiff(.POP_ROSTER_COLUMNS, c("subject_id", "cohort_start_date",
                                                                   "index_source", "source_rank", "key_1",
                                                                   "sampling_weight"))))
      paste0("SELECT ", paste(.populationSelect("x", over), collapse = ", "))
    }
    f <- .fragment(.fragmentName("pop", "u", "all", ctx = ctx), "candidates", "episode",
                   paste(row(1), "UNION ALL", row(2)), columns = .populationColumnSpec(TRUE),
                   person_lineage = list(person_col = "subject_id", via_roster = NULL),
                   private_columns = .POP_PRIVATE_COLUMNS)
    attr(f, "population") <- "u"
    .fragmentRegister(ctx, f)
    .ws4_expect_rejection(.populationAssertNoIdentityConflict(ctx, f, "u"), reason = "setop_identity_conflict")
  }
})

test_that("the plan scope narrows every population; an unresolved scope fails closed", {
  plan <- .ws6_plan(.ws6_ev(), list(idx_all = .ws6_idx("all"), persons = list(kind = "criteria")),
                    scope = list(cohort = 93001L))
  expect_identical(unique(.ws6_pop(plan, "idx_all")$person_id), 6017)
  expect_identical(.ws6_pop(plan, "persons", 1:7000)$person_id, 6017)
  plan$scope <- list(tables = list("omop_tbl"))
  for (b in names(.ws6_pop_backends)) {
    ctx <- .ws4_ctx(.ws6_pop_backends[[b]]$handle, plan, validate = FALSE)
    expect_error(.compilePopulationByName(ctx, "idx_all"), "scope must be resolved")
  }
})

test_that("rosters are deterministic: cohort_row_id is the typed chronological rank", {
  plan <- .ws6_plan(.ws6_ev(), list(idx_all = .ws6_idx("all")))
  a <- .ws6_pop(plan, "idx_all", 1:7000)
  b <- .ws6_pop(plan, "idx_all", 1:7000)
  expect_identical(a, b)
  o <- order(a$person_id, a$index_date, a$end_date, as.numeric(a$index_key))
  expect_identical(a$cohort_row_id[o], as.numeric(seq_len(nrow(a))))
})

test_that("semantic rules of populations refuse before any query", {
  ev <- .ws6_ev()
  refuse <- function(plan, pattern) {
    err <- tryCatch(.irPrepare(plan), dsomop_plan_rejection = function(e) e)
    expect_s3_class(err, "dsomop_plan_rejection")
    expect_match(paste(c(conditionMessage(err), err$errors), collapse = " "), pattern)
  }
  match <- function(controls = "tpl", ...) list(kind = "match", match = c(list(cases = "cases", controls = controls,
                                                                           outcome = list(event = "idx")), list(...)))
  base <- list(cases = .ws6_idx("first"), tpl = list(kind = "criteria"))
  refuse(.ws6_plan(ev, c(base, list(m = match())), outputs = list(o = list(rows = "tpl", format = "wide"),
                                                                  p = list(rows = "m", format = "wide"))),
         "cannot also be used as an ordinary population")
  refuse(.ws6_plan(ev, c(base, list(m = match(), m2 = match()))), "template of more than one match")
  refuse(.ws6_plan(ev, list(cases = .ws6_idx("first"), tpl = list(kind = "criteria", coverage = list(policy = "earliest")),
                            m = match())), "share one coverage policy")
  strata <- list(list(strata = list(source = list(event = "anc"),
                                    window = list(start = list(anchor = list(source = list(event = "anc")), offset = 0L),
                                                  end = NULL, anchor = "index"))))
  # A-20(b) admits source anchors in strata windows. Matching uses the same
  # definition on final case rows and on controls instantiated at case time.
  expect_s3_class(.irPrepare(.ws6_plan(ev, c(base, list(m = match(match = strata))))), "dsomop_ir")
  refuse(.ws6_plan(ev, list(a = .ws6_idx("all"),
                            b = list(kind = "criteria", parent = "a", episode_policy = "any_episode"))),
         "a child inherits its parent's episodes")
  refuse(.ws6_plan(ev, list(b = list(kind = "criteria", episode_policy = "any_episode"))),
         "needs a results-cohort seed")
  refuse(.ws6_plan(ev, list(b = list(kind = "criteria", cohort_definition_id = 93002L,
                                     where = list(observed = list(before = 30L))))),
         "need an explicit episode_policy")
  # accepted: a seed with index-independent criteria, a policy with a seed
  expect_s3_class(.irPrepare(.ws6_plan(ev, list(b = list(kind = "criteria", cohort_definition_id = 93002L,
                                                         where = .ws6_has("anc"))))), "dsomop_ir")
})

test_that("visit-local interval indices retain full keys through unions and scoped criteria", {
  ev <- .ws6_ev()
  ev$idx_visit <- c(ev$idx, list(visit = list(index = TRUE)))
  has_iv <- list(has = list(source = list(interval = "proc_era"), op = ">=", n = 1L))
  pop <- .ws6_idx("all", source = list(interval = "idx_era"), end = list(strategy = "interval_end"))
  plan <- .ws6_plan(ev, list(a = pop, b = pop,
    u = list(kind = "setop", setop = list(op = "union", members = list("a", "b"))),
    scoped = c(pop, list(where = has_iv))), intervals = list(
      idx_era = list(source = list(event = "idx_visit"), gap = 0L),
      proc_era = list(source = list(event = "proc_visit"), gap = 0L)))
  for (b in names(.ws6_pop_backends)) {
    ctx <- .ws4_ctx(.ws6_pop_backends[[b]]$handle, plan)
    tryCatch({
      roster <- .compilePopulationByName(ctx, "u")$roster
      r <- .fetchFragment(ctx, roster, "cohort_row_id")
      r <- r[as.numeric(r$subject_id) == 6015, ]
      expect_equal(nrow(r), 2L, info = b)
      expect_equal(as.numeric(r$index_visit_id), c(601501, 601502), info = b)
      expect_equal(length(unique(r$index_key)), 2L, info = b)
      expect_true(all(grepl("^60150[12]\\|", r$index_key)), info = b)
      scoped <- .fetchFragment(ctx, .compilePopulationByName(ctx, "scoped")$roster, "cohort_row_id")
      expect_equal(as.numeric(scoped$index_visit_id[as.numeric(scoped$subject_id) == 6015]), 601501, info = b)
    }, finally = .compileReleaseTempTables(ctx))
  }
})

test_that("member-indexed criteria preserve parent origins and person criteria deduplicate links", {
  plan <- .ws5_acceptance_plan("14-oncology-episodes")
  # Person 1015 has six episode-member links but five physical events.
  plan$populations$physical <- list(kind = "criteria", where = .ws6_has("members", op = "==", n = 5L))
  plan$populations$linked <- .ws6_idx("all", source = list(event = "members"),
                                     end = list(strategy = "event_end"))
  plan$populations$child <- list(kind = "criteria", parent = "linked",
                                 where = .ws6_has("members", op = "==", n = 2L))
  plan$intervals <- list(mc = list(source = list(event = "members"), gap = 0L))
  plan$populations$era_child <- list(kind = "criteria", parent = "linked",
    where = list(has = list(source = list(interval = "mc"), op = "==", n = 2L)))
  for (p in c("physical", "linked", "child", "era_child")) plan$outputs[[p]] <- list(rows = p, format = "wide")
  for (b in names(.ws6_pop_backends)) {
    ctx <- .ws4_ctx(.ws6_pop_backends[[b]]$handle, plan)
    tryCatch({
      expect_true(1015 %in% .ws6_roster(ctx, "physical")$person_id, info = b)
      for (p in c("child", "era_child")) {
        r <- .fetchFragment(ctx, .compilePopulationByName(ctx, p)$roster, "cohort_row_id")
        r <- r[as.numeric(r$subject_id) == 1015, ]
        expect_equal(nrow(r), 2L, info = paste(b, p))
        expect_true(all(r$origin_source == "episodes" & r$origin_key == "2"), info = paste(b, p))
      }
    }, finally = .compileReleaseTempTables(ctx))
  }
})

test_that("final observed segments use transferred components after candidate renumbering", {
  plan <- .ws6_plan(.ws6_ev(), list(
    all = .ws6_idx("all"),
    child = list(kind = "criteria", parent = "all", where = .ws6_has("anc", window = .ws6_win(-60L, 0L)))))
  for (b in names(.ws6_pop_backends)) {
    ctx <- .ws4_ctx(.ws6_pop_backends[[b]]$handle, plan)
    tryCatch({
      res <- .compilePopulationByName(ctx, "child")
      cand <- .fetchFragment(ctx, res$candidates, "cand_key")
      roster <- .fetchFragment(ctx, res$roster, "cohort_row_id")
      seg <- .fetchFragment(ctx, res$segments, "row_key")
      r <- roster[as.numeric(roster$subject_id) == 6001, ]
      c <- cand[as.numeric(cand$subject_id) == 6001 & as.character(cand$index_key) == r$index_key, ]
      expect_false(identical(as.character(r$cohort_row_id), as.character(c$cand_key)), info = b)
      expect_equal(as.character(r$op_component_id), as.character(c$op_component_id), info = b)
      s <- seg[as.numeric(seg$person_id) == 6001 & as.character(seg$row_key) == as.character(r$cohort_row_id), ]
      expect_equal(nrow(s), 1L, info = b)
      expect_equal(format(s$start_date, "%Y-%m-%d"), "2010-01-01", info = b)
      expect_equal(format(s$end_date, "%Y-%m-%d"), "2030-12-31", info = b)
    }, finally = .compileReleaseTempTables(ctx))
  }
})

test_that("case-distinct namespaces retain separate parent and person associations", {
  plan <- .ws5_acceptance_plan("14-oncology-episodes")
  plan$events$Episodes <- plan$events$episodes
  plan$populations$mixed <- .ws6_idx("all", source = list(any_of = list(
    list(event = "episodes"), list(event = "Episodes"))), end = list(strategy = "event_end"),
    where = .ws6_has("members", op = "==", n = 5L))
  plan$outputs$mixed <- list(rows = "mixed", format = "wide")
  for (b in names(.ws6_pop_backends)) {
    ctx <- .ws4_ctx(.ws6_pop_backends[[b]]$handle, plan)
    tryCatch({
      r <- .ws6_roster(ctx, "mixed")
      r <- r[r$person_id == 1015, ]
      expect_equal(nrow(r), 2L, info = b)
      expect_true(all(r$index_source == "Episodes"), info = b)
    }, finally = .compileReleaseTempTables(ctx))
  }
})
