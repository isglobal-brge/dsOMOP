# WS6R2-B1: a controls template's parent limits persons, not association.
.ws6m_backends <- .ws6_backends(extend = function(conn) {
  DBI::dbExecute(conn, paste0(
    "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
    "condition_concept_id, condition_start_date, condition_end_date, condition_type_concept_id) ",
    "VALUES (777777, 1016, 990001, '2019-06-01', '2019-06-01', 44818518)"))
})
withr::defer(.ws4_cleanup(.ws6m_backends), testthat::teardown_env())

.ws6m_has <- function(n, source = list(event = "members"), window = NULL) {
  leaf <- list(source = source, op = "==", n = n)
  if (!is.null(window)) leaf$window <- window
  list(has = leaf)
}

.ws6m_plan <- function(parent = "episode", mode = "positive") {
  p <- .ws6_plan(events = list(
    idx = .ws6_event("condition_occurrence", 990001L),
    out = .ws6_event("condition_occurrence", 990099L),
    episodes = .ws6_event("episode"),
    members = list(episode_members = list(episode = list(event = "episodes")))),
    intervals = list(courses = list(source = list(event = "members"), gap = 0L)),
    populations = list(
      cases = list(kind = "index", index = list(source = list(event = "idx"),
        select = "all", end = list(strategy = "fixed", days = 0L))),
      ep_parent = list(kind = "index", index = list(source = list(event = "episodes"),
        select = "all", end = list(strategy = "event_end"))),
      person_parent = list(kind = "criteria", where = list(has = list(
        source = list(event = "episodes"), op = ">=", n = 1L))),
      tpl = list(kind = "criteria", parent = if (parent == "episode") "ep_parent" else "person_parent",
        where = .ws6m_has(5L)),
      matched = list(kind = "match", match = list(cases = "cases", controls = "tpl",
        outcome = list(event = "out"), controls_per_case = 1L))))
  if (mode == "absence") p$populations$tpl$where <- .ws6m_has(0L)
  if (mode == "links") p$populations$tpl$where <- .ws6m_has(6L)
  if (mode == "interval") p$populations$tpl$where <- .ws6m_has(5L, list(interval = "courses"))
  if (mode %in% c("event_anchor", "interval_anchor")) {
    source <- if (mode == "event_anchor") list(event = "members") else list(interval = "courses")
    # Fifth physical event/course is October 1. The shared April event must
    # not count twice.
    anchor <- list(source = source, which = "nth", n = 5L)
    p$populations$tpl$where <- .ws6m_has(1L, window = list(start = 0L, end = 0L, anchor = anchor))
  }
  if (mode %in% c("event_strata", "interval_strata")) {
    source <- if (mode == "event_strata") list(event = "members") else list(interval = "courses")
    p$populations$matched$match$match <- list(list(strata = list(source = source,
      window = list(start = NULL, end = 0L, anchor = "index"))))
  }
  p
}

test_that("template regressions execute on every configured backend", {
  .ws5_expect_backends(.ws6m_backends, "WS6R2 matching template association")
})

test_that("equivalent parents give identical positive, absent and deduplicated member pools", {
  for (backend in names(.ws6m_backends)) for (mode in c("positive", "absence", "links")) {
    results <- list()
    for (parent in c("person", "episode")) {
      h <- .ws6m_backends[[backend]]$handle
      ctx <- .ws4_ctx(h, .ws6m_plan(parent, mode))
      info <- paste(backend, parent, mode)
      tryCatch({
        expect_length(.irSemanticErrors(ctx$ir), 0L)
        expect_silent(.irValidate(ctx$ir, h))
        ep <- .fetchFragment(ctx, .compilePopulationByName(ctx, "ep_parent")$roster)
        pp <- .fetchFragment(ctx, .compilePopulationByName(ctx, "person_parent")$roster)
        expect_setequal(as.character(ep$subject_id), as.character(pp$subject_id))
        expect_identical(.irPopulationAssociationSources(ctx$ir, "tpl"), "(persons)", info = info)
        res <- .compilePopulationByName(ctx, "matched")
        pairs <- .fetchFragment(ctx, res$pairs)
        rows <- .fetchFragment(ctx, res$roster, "cohort_row_id")
        expected <- if (mode == "positive") "1015" else character(0)
        expect_identical(as.character(pairs$subject_id), expected, info = info)
        expect_identical(as.character(rows$subject_id[rows$is_case == 0]), expected, info = info)
        expect_identical(as.character(rows$subject_id[rows$is_case == 1]), "1016", info = info)
        expect_equal(as.numeric(rows$sampling_weight), rep(1, nrow(rows)), info = info)
        # Marker and case-time identity survive raw, coverage and criteria stages.
        stages <- list(.fragmentGet(ctx, .fragmentName("cand", "tpl", "raw", ctx = ctx)),
          res$candidates, .fragmentGet(ctx, .fragmentName("cand", "tpl", "crit", ctx = ctx)))
        for (stage in stages) {
          candidates <- .fetchFragment(ctx, stage)
          expect_true(nrow(candidates) > 0L, info = info)
          expect_true(all(candidates$origin_source == "(persons)"), info = info)
          expect_true(all(is.na(candidates$origin_key)), info = info)
          expect_true(all(candidates$index_source == "match:matched"), info = info)
          expect_true(all(as.character(candidates$cohort_start_date) == "2019-06-01"), info = info)
          expect_equal(anyDuplicated(paste(candidates$mt_set, candidates$subject_id)), 0L, info = info)
          a <- .consumerAssociation(ctx, list(event = "members"), stage, "candidate")
          expect_identical(a$branches, "person", info = info)
        }
        results[[parent]] <- rows
      }, finally = .planContextClose(ctx))
      expect_length(h$temp_tables, 0L)
    }
    expect_equal(results$episode, results$person, info = paste(backend, mode))
  }
})

test_that("template source anchors, adapter intervals and strata use person association", {
  for (backend in names(.ws6m_backends)) {
    for (mode in c("interval", "event_anchor", "interval_anchor", "event_strata", "interval_strata")) {
      results <- list()
      for (parent in c("person", "episode")) {
        h <- .ws6m_backends[[backend]]$handle
        ctx <- .ws4_ctx(h, .ws6m_plan(parent, mode))
        info <- paste(backend, parent, mode)
        tryCatch({
          expect_length(.irSemanticErrors(ctx$ir), 0L)
          expect_silent(.irValidate(ctx$ir, h))
          res <- .compilePopulationByName(ctx, "matched")
          pairs <- .fetchFragment(ctx, res$pairs)
          rows <- .fetchFragment(ctx, res$roster, "cohort_row_id")
          expect_identical(as.character(pairs$subject_id), "1015", info = info)
          expect_identical(as.character(rows$subject_id[rows$is_case == 0]), "1015", info = info)
          expect_identical(as.character(rows$subject_id[rows$is_case == 1]), "1016", info = info)
          results[[parent]] <- rows
        }, finally = .planContextClose(ctx))
        expect_length(h$temp_tables, 0L)
      }
      expect_equal(results$episode, results$person, info = paste(backend, mode))
    }
  }
})

test_that("ordinary episode association and missing-provenance guards remain intact", {
  for (backend in names(.ws6m_backends)) for (corrupt in c(FALSE, TRUE)) {
    h <- .ws6m_backends[[backend]]$handle
    p <- .ws6m_plan()
    p$populations$child <- list(kind = "criteria", parent = "ep_parent", where = .ws6m_has(2L))
    ctx <- .ws4_ctx(h, p)
    info <- paste(backend, corrupt)
    tryCatch({
      parent <- .compilePopulationByName(ctx, "ep_parent")
      if (corrupt) {
        table <- tail(ctx$compile_temp_tables, 1L)
        expect_match(parent$roster$select_body, paste0("FROM ", table, " t"), fixed = TRUE)
        .executeStatement(h, paste0("UPDATE ", table,
          " SET origin_source=NULL, origin_key=NULL, index_source=NULL, index_key=NULL WHERE subject_id=1015"))
      }
      a <- .consumerAssociation(ctx, list(event = "members"), parent$roster, "final")
      expect_identical(a$branches, "parent", info = info)
      expect_true(any(grepl("person_id = r.subject_id", .consumerJoinSql(h, a, "parent"), fixed = TRUE)))
      child <- .fetchFragment(ctx, .compilePopulationByName(ctx, "child")$roster)
      child <- child[as.character(child$subject_id) == "1015", , drop = FALSE]
      expect_equal(nrow(child), if (corrupt) 0L else 1L, info = info)
      if (!corrupt) expect_identical(as.character(child$index_key), "2", info = info)
      res <- .compilePopulationByName(ctx, "matched")
      rows <- .fetchFragment(ctx, res$roster)
      expect_identical(as.character(rows$subject_id[rows$is_case == 0]), "1015", info = info)
    }, finally = .planContextClose(ctx))
    expect_length(h$temp_tables, 0L)
  }
})

test_that("controls templates cannot also be ordinary populations", {
  p <- .ws6m_plan()
  p$outputs$o_tpl <- list(rows = "tpl", format = "wide")
  expect_error(.irPrepare(p), "cannot also be used as an ordinary population")
})

test_that("templates still reject cross-person episode member links", {
  backends <- .ws6_backends(extend = function(conn) {
    DBI::dbExecute(conn, paste0(
      "INSERT INTO episode_event (episode_id, event_id, episode_event_field_concept_id) ",
      "SELECT 2, drug_exposure_id, 1147094 FROM drug_exposure WHERE person_id = 1016"))
  })
  on.exit(.ws4_cleanup(backends), add = TRUE)
  for (backend in names(backends)) {
    h <- backends[[backend]]$handle
    ctx <- .ws4_ctx(h, .ws6m_plan())
    tryCatch({
      expect_error(.compilePopulationByName(ctx, "matched"), "adapter_person_mismatch")
    }, finally = .planContextClose(ctx))
    expect_length(h$temp_tables, 0L)
  }
})
