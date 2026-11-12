# WS-4: the bounded domain adapters (design-2.10.3 sections 4.1 and 11,
# amendment A-4) on the CDM 5.4 extended reach fixture, on every available
# backend: episode membership through the reviewed episode_event field
# mapping and event-linked costs through the reviewed cost-domain mapping.

.ws4_ad_backends <- .ws4_backends()
withr::defer(.ws4_cleanup(.ws4_ad_backends), testthat::teardown_env())

.ws4_onc_events <- function() {
  list(
    episodes = list(table = "episode", concept_set = list(concepts = list(32531))),
    members = list(episode_members = list(episode = list(event = "episodes"))),
    member_drugs = list(episode_members = list(episode = list(event = "episodes"),
                                               tables = list("drug_exposure"))),
    member_procs = list(episode_members = list(
      episode = list(event = "episodes"),
      where = list(var = "member_table", op = "==", value = "procedure_occurrence"))))
}

.ws4_cost_events <- function() {
  list(
    metf = list(table = "drug_exposure", concept_set = list(concepts = list(1503297)),
                end = list(fallback = "days_supply")),
    visits = list(table = "visit_occurrence"),
    visit_cost = list(cost = list(event = list(event = "visits"), field = "total_cost",
                                  currency = 44818668)),
    visit_cost_any = list(cost = list(event = list(event = "visits"), field = "total_cost")),
    drug_cost = list(cost = list(event = list(event = "metf"), field = "total_cost")),
    typed_cost = list(cost = list(event = list(event = "visits"), field = "total_charge",
                                  currency = 44818668, cost_type = list(32817))),
    other_type = list(cost = list(event = list(event = "visits"), field = "total_cost",
                                  currency = 44818668, cost_type = list(99999))))
}

test_that("episode members: one row per (episode, member) link with the parent identity", {
  for (b in names(.ws4_ad_backends)) {
    h <- .ws4_ad_backends[[b]]$handle
    ctx <- .ws4_ctx(h, .ws4_plan(events = .ws4_onc_events()))
    ord <- "parent_event_id, member_table, event_id"
    m <- .ws4_rows(ctx, "members", order = ord)
    expect_identical(nrow(m), 15L, info = b)
    per_episode <- table(as.numeric(m$parent_event_id))
    expect_identical(as.integer(per_episode[c("1", "2", "3")]), c(4L, 2L, 1L), info = b)
    expect_true(all(per_episode[as.character(101:108)] == 1L), info = b)
    # the same drug row linked to two episodes yields two membership rows
    shared <- m[duplicated(m$event_id) | duplicated(m$event_id, fromLast = TRUE), ]
    expect_identical(nrow(shared), 2L, info = b)
    expect_setequal(as.numeric(shared$parent_event_id), c(1, 2))
    expect_identical(unique(m$parent_source_ns), "episodes")
    expect_identical(unique(m$source_ns), "members")
    expect_identical(sort(unique(m$member_table)), c("drug_exposure", "procedure_occurrence"))
    # person_id comes from the episode; member roles come from the member table
    expect_true(all(m$person_id[as.numeric(m$parent_event_id) %in% c(1, 2)] == 1015L), info = b)
    expect_identical(unique(m$episode_concept_id), 32531L)
    proc <- m[m$member_table == "procedure_occurrence", ]
    expect_identical(proc$start_date, "2019-03-05", info = b)
    expect_identical(sort(unique(m$concept_id)), sort(unique(c(1718850L, 4044013L, 1503297L))))
    # restricted member tables and adapter-local predicates
    expect_identical(nrow(.ws4_rows(ctx, "member_drugs", order = ord)), 14L, info = b)
    expect_identical(nrow(.ws4_rows(ctx, "member_procs", order = ord)), 1L, info = b)
    # an episode-indexed consumer joins members by parent identity and person:
    # every episode counts its own links (the shared fill once in each)
    ep <- .compileEventByName(ctx, "episodes")
    mf <- .compileEventByName(ctx, "members")
    sql <- .buildStatement(ctx, mf, select = paste0(
      "SELECT e.event_id AS episode, COUNT(x.event_id) AS n FROM ", ep$name, " e LEFT JOIN ",
      mf$name, " x ON x.parent_source_ns = e.source_ns AND x.parent_event_id = e.event_id ",
      "AND x.person_id = e.person_id GROUP BY e.event_id ORDER BY e.event_id"), dialect = ctx)
    n <- .executeQuery(h, sql)
    expect_identical(as.integer(n$n[as.numeric(n$episode) %in% c(1, 2, 3)]), c(4L, 2L, 1L), info = b)
    f <- .fragmentGet(ctx, "ev_members")
    expect_identical(f$unique_key, c("parent_source_ns", "parent_event_id", "member_table", "event_id"))
    expect_true(all(c("parent_source_ns", "parent_event_id", "episode_id", "event_id") %in% f$private_columns))
    expect_identical(f$person_lineage$person_col, "person_id")
  }
})

test_that("a member row whose person disagrees with its episode is refused (adapter_person_mismatch)", {
  h <- .ws4_reach_handle(with_defects = TRUE)
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  ctx <- .ws4_ctx(h, .ws4_plan(events = .ws4_onc_events()))
  .ws4_expect_rejection(.compileEventByName(ctx, "members"), reason = "adapter_person_mismatch",
                        pattern = "belongs to another person")
  # the refusal never re-attributes or drops the row silently: the compiled
  # SQL also joins on the member's own person
  expect_match(.fragmentGet(ctx, "ev_members_src")$select_body, "m.person_id = ep.person_id", fixed = TRUE)
})

test_that("episode-member requests are checked against the reviewed mapping", {
  h <- .ws4_ad_backends$sqlite$handle
  bad <- function(events, ...) {
    ctx <- .ws4_ctx(h, .ws4_plan(events = events))
    .ws4_expect_rejection(.compileEventByName(ctx, "m"), ...)
  }
  ev <- list(table = "episode")
  bad(list(e = ev, m = list(episode_members = list(episode = list(event = "e"), tables = list("note")))),
      pattern = "no reviewed episode_event field mapping")
  bad(list(e = list(table = "condition_occurrence"),
           m = list(episode_members = list(episode = list(event = "e")))),
      pattern = "needs an event over the episode table")
  bad(list(e = ev, m = list(episode_members = list(episode = list(event = "e"),
                                                  where = list(var = "value", op = ">", value = 1),
                                                  tables = list("drug_exposure")))),
      pattern = "no value role")
  # the reviewed mapping is the offline-pinned A-4 table
  fields <- .irEpisodeEventFields()
  expect_identical(fields$episode_event_field_concept_id[fields$table_name == "drug_exposure"], 1147094L)
  expect_identical(fields$episode_event_field_concept_id[fields$table_name == "visit_occurrence"], 1147070L)
})

test_that("a site without episode_event or cost yields the declared unavailable state", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  DBI::dbExecute(h$conn, "DROP TABLE episode_event")
  DBI::dbExecute(h$conn, "DROP TABLE cost")
  h$blueprint <- NULL
  events <- list(
    episodes = list(table = "episode"),
    m_unavail = list(episode_members = list(episode = list(event = "episodes"),
                                            on_missing_table = "unavailable")),
    m_error = list(episode_members = list(episode = list(event = "episodes"))),
    visits = list(table = "visit_occurrence"),
    c_unavail = list(cost = list(event = list(event = "visits"), field = "total_cost",
                                 on_missing_table = "unavailable")),
    c_error = list(cost = list(event = list(event = "visits"), field = "total_cost")))
  ctx <- .ws4_ctx(h, .ws4_plan(events = events))
  for (e in c("m_unavail", "c_unavail")) {
    f <- .compileEventByName(ctx, e)
    expect_identical(f$availability, "unavailable", info = e)
    expect_identical(nrow(.ws4_rows(ctx, e, order = "event_id")), 0L)
  }
  expect_true(all(c("parent_source_ns", "parent_event_id") %in% names(.fragmentGet(ctx, "ev_c_unavail")$columns)))
  .ws4_expect_rejection(.compileEventByName(ctx, "m_error"), code = "unavailable_table")
  .ws4_expect_rejection(.compileEventByName(ctx, "c_error"), code = "unavailable_table")
})

test_that("costs: one row per cost row of the event, one declared field, one currency", {
  for (b in names(.ws4_ad_backends)) {
    h <- .ws4_ad_backends[[b]]$handle
    ctx <- .ws4_ctx(h, .ws4_plan(events = .ws4_cost_events()))
    vc <- .ws4_rows(ctx, "visit_cost", order = "event_id")
    expect_equal(as.numeric(vc$event_id), c(1, 2, 5), info = b)
    expect_equal(vc$value, c(100, 50, 1000), info = b)
    expect_identical(vc$person_id, c(1017L, 1017L, 1007L), info = b)
    expect_identical(vc$start_date, c("2020-01-10", "2020-01-10", "2020-01-01"), info = b)
    expect_true(all(vc$currency_concept_id == 44818668L))
    expect_equal(as.numeric(vc$parent_event_id), c(701701, 701701, 700701))
    expect_identical(unique(vc$parent_source_ns), "visits")
    # the parent event's dates and concept, its own amount field only
    dc <- .ws4_rows(ctx, "drug_cost", order = "event_id")
    expect_equal(as.numeric(dc$event_id), 3, info = b)
    expect_equal(dc$value, 20)
    expect_identical(dc$concept_id, 1503297L)
    expect_identical(dc$start_date, "2020-01-05")
    expect_identical(dc$end_date, "2020-02-03")
    tc <- .ws4_rows(ctx, "typed_cost", order = "event_id")
    expect_equal(tc$value, c(150, 60, 1200), info = b)
    expect_identical(nrow(.ws4_rows(ctx, "other_type", order = "event_id")), 0L)
    # visit costs mixing USD and EUR without a currency restriction are refused
    .ws4_expect_rejection(.compileEventByName(ctx, "visit_cost_any"), reason = "cost_currency_mixed",
                          pattern = "mix currencies")
    f <- .fragmentGet(ctx, "ev_visit_cost")
    expect_identical(f$unique_key, "event_id")
    expect_true(all(c("parent_source_ns", "parent_event_id", "event_id") %in% f$private_columns))
  }
})

test_that("cost sources reference a table event with a reviewed cost-domain mapping", {
  h <- .ws4_ad_backends$sqlite$handle
  events <- c(.ws4_onc_events(), list(
    d = list(table = "death"),
    death_cost = list(cost = list(event = list(event = "d"), field = "total_cost")),
    member_cost = list(cost = list(event = list(event = "members"), field = "total_cost"))))
  ctx <- .ws4_ctx(h, .ws4_plan(events = events))
  .ws4_expect_rejection(.compileEventByName(ctx, "death_cost"), pattern = "reviewed cost_domain_id mapping")
  .ws4_expect_rejection(.compileEventByName(ctx, "member_cost"), pattern = "must reference a table event")
  domains <- .irCostDomains()
  expect_identical(domains$cost_domain_id[domains$table_name == "visit_occurrence"], "Visit")
  expect_identical(domains$cost_domain_id[domains$table_name == "drug_exposure"], "Drug")
})

test_that("compiled adapter SQL renders for every dialect family", {
  bp <- .buildBlueprint(.ws4_ad_backends$sqlite$handle)
  events <- c(.ws4_onc_events(), .ws4_cost_events()[c("visits", "visit_cost")])
  plan <- .ws4_plan(events = events)
  dbms <- c(postgresql = "postgresql", sqlite = "sqlite", duckdb = "duckdb", mysql = "mysql",
            sqlserver = "sql server", oracle = "oracle", redshift = "redshift", bigquery = "bigquery",
            snowflake = "snowflake", spark = "spark")
  # the data checks (person agreement) need a site; the rendering does not
  local_mocked_bindings(.adapterCount = function(ctx, final, select) 0)
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
      for (e in c("member_procs", "visit_cost")) {
        cat(.buildStatement(ctx, .compileEventByName(ctx, e), dialect = ctx), "\n")
      }
    }
  })
})

.ws4_expected_rows <- function(recipe, output) {
  path <- testthat::test_path("..", "..", "inst", "plans", "acceptance", recipe)
  expected <- jsonlite::fromJSON(file.path(path, "expected.json"), simplifyVector = FALSE)
  rows <- expected$outputs[[output]]$rows
  cols <- unique(unlist(lapply(rows, names)))
  out <- lapply(cols, function(cn) {
    v <- lapply(rows, function(r) if (is.null(r[[cn]])) NA else r[[cn]])
    unlist(v)
  })
  names(out) <- cols
  as.data.frame(out, stringsAsFactors = FALSE)
}

test_that("acceptance recipe 14: member streams reproduce the WS-0 oracle (links per episode identity)", {
  path <- testthat::test_path("..", "..", "inst", "plans", "acceptance", "14-oncology-episodes")
  plan <- jsonlite::fromJSON(file.path(path, "plan.json"), simplifyVector = FALSE)
  want <- .ws4_expected_rows("14-oncology-episodes", "episodes")
  for (b in names(.ws4_ad_backends)) {
    h <- .ws4_ad_backends[[b]]$handle
    ctx <- .ws4_ctx(h, plan)
    num <- function(r) { r$event_id <- as.numeric(r$event_id); r$parent_event_id <- as.numeric(r$parent_event_id); r }
    members <- num(.ws4_rows(ctx, "members", order = "parent_event_id, member_table, event_id"))
    drugs <- num(.ws4_rows(ctx, "member_drugs", order = "parent_event_id, member_table, event_id"))
    spec <- .ws4_rows(ctx, "specimens")
    got <- do.call(rbind, lapply(seq_len(nrow(want)), function(i) {
      ep <- as.numeric(want$index_key[[i]]); p <- want$person_id[[i]]
      idx <- as.Date(want$index_date[[i]])
      m <- members[members$parent_event_id == ep & members$person_id == p, , drop = FALSE]
      s <- spec[spec$person_id == p & as.Date(spec$start_date) >= idx - 30 &
                  as.Date(spec$start_date) <= idx + 365, , drop = FALSE]
      data.frame(person_id = p, index_date = want$index_date[[i]], index_key = want$index_key[[i]],
                 n_members = nrow(m),
                 n_member_drugs = sum(drugs$parent_event_id == ep & drugs$person_id == p),
                 first_member_days = if (nrow(m)) as.integer(min(as.Date(m$start_date)) - idx) else NA_integer_,
                 n_specimens = nrow(s), stringsAsFactors = FALSE)
    }))
    expect_equal(got, want[names(got)], info = b)
  }
})

test_that("acceptance recipe 15: cost streams reproduce the WS-0 oracle (USD only, parent dates)", {
  path <- testthat::test_path("..", "..", "inst", "plans", "acceptance", "15-costs-adherence")
  plan <- jsonlite::fromJSON(file.path(path, "plan.json"), simplifyVector = FALSE)
  want <- .ws4_expected_rows("15-costs-adherence", "adherence")
  for (b in names(.ws4_ad_backends)) {
    h <- .ws4_ad_backends[[b]]$handle
    ctx <- .ws4_ctx(h, plan)
    vc <- .ws4_rows(ctx, "visit_cost", order = "event_id")
    dc <- .ws4_rows(ctx, "drug_cost", order = "event_id")
    sum_in <- function(r, p, idx) {
      x <- r[r$person_id == p & as.Date(r$start_date) >= idx + 1 & as.Date(r$start_date) <= idx + 60, ]
      sum(x$value)
    }
    got <- do.call(rbind, lapply(seq_len(nrow(want)), function(i) {
      p <- want$person_id[[i]]; idx <- as.Date(want$index_date[[i]])
      data.frame(visit_cost_60 = sum_in(vc, p, idx), drug_cost_60 = sum_in(dc, p, idx))
    }))
    expect_equal(got$visit_cost_60, as.numeric(want$visit_cost_60), info = b)
    expect_equal(got$drug_cost_60, as.numeric(want$drug_cost_60), info = b)
    expect_true(any(got$visit_cost_60 > 0))
  }
})
