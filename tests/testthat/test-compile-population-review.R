# WS6-B1/B3/N1: independent review regressions for person associations,
# closed compatibility empties and the fragment nullability contract.
.ws6f_pop_backends <- .ws6_backends(extend = function(conn) {
  DBI::dbWriteTable(conn, "cohort", data.frame(
    cohort_definition_id = 93991L, subject_id = 1015L,
    cohort_start_date = "2019-03-01", cohort_end_date = "2020-03-31"), append = TRUE)
})
withr::defer(.ws4_cleanup(.ws6f_pop_backends), testthat::teardown_env())

.ws6f_episode_events <- function() list(
  episodes = .ws6_event("episode", 32531L),
  members = list(episode_members = list(episode = list(event = "episodes"))))
.ws6f_episode_population <- function() list(
  kind = "index", coverage = list(on_missing = "keep"),
  index = list(source = list(event = "episodes"), select = "all",
               end = list(strategy = "event_end")))
.ws6f_setop <- function(op, ...) list(kind = "setop", setop = list(op = op, members = list(...)))
.ws6f_has_members <- function(n) list(has = list(source = list(event = "members"), op = "==", n = n))
.ws6f_child <- function(parent, where = NULL) {
  node <- list(kind = "criteria", parent = parent, coverage = list(on_missing = "keep"))
  if (!is.null(where)) node$where <- where
  node
}
.ws6f_person_rows <- function(rows, person = 1015) {
  rows[as.numeric(rows$subject_id) == person & is.na(rows$cohort_start_date), , drop = FALSE]
}

test_that("review regressions execute on every configured backend", {
  .ws5_expect_backends(.ws6f_pop_backends, "WS-6 population review regressions")
})

test_that("intersection and difference preserve their left person's adapter association", {
  policy <- list(on_missing = "keep")
  plan <- .ws6_plan(.ws6f_episode_events(), list(
    people = list(kind = "criteria", coverage = policy),
    episodes = .ws6f_episode_population(),
    no_members = .ws6f_child("episodes", .ws6f_has_members(0L)),
    inter = .ws6f_setop("intersect", "people", "episodes"),
    diff = .ws6f_setop("setdiff", "people", "no_members"),
    kept_inter = .ws6f_child("inter", .ws6f_has_members(5L)),
    kept_diff = .ws6f_child("diff", .ws6f_has_members(5L)),
    control = list(kind = "criteria", coverage = policy, where = .ws6f_has_members(5L))))
  ir <- .irPrepare(plan)
  expect_s3_class(ir, "dsomop_ir")
  for (population in c("inter", "diff", "kept_inter", "kept_diff")) {
    expect_identical(.irPopulationIndexSources(ir, population), "(persons)", info = population)
    expect_identical(.irPopulationAssociationSources(ir, population), "(persons)", info = population)
  }
  for (backend in names(.ws6f_pop_backends)) {
    ctx <- .ws4_ctx(.ws6f_pop_backends[[backend]]$handle, plan)
    tryCatch({
      control <- .ws6_roster(ctx, "control")
      expect_true(1015 %in% control$person_id, info = backend)
      expect_false(1015 %in% .ws6_roster(ctx, "no_members")$person_id, info = backend)
      for (population in c("inter", "diff", "kept_inter", "kept_diff")) {
        rows <- .fetchFragment(ctx, .compilePopulationByName(ctx, population)$roster)
        person <- .ws6f_person_rows(rows)
        expect_equal(nrow(person), 1L, info = paste(backend, population))
        expect_identical(as.character(person$origin_source), "(persons)", info = paste(backend, population))
        expect_true(all(is.na(person$index_source)), info = paste(backend, population))
        expect_true(all(is.na(person$index_key)), info = paste(backend, population))
      }
    }, finally = .planContextClose(ctx))
  }
})

test_that("mixed unions preserve unseeded and cohort-seeded person rows through children", {
  policy <- list(on_missing = "keep")
  plan <- .ws6_plan(.ws6f_episode_events(), list(
    people = list(kind = "criteria", coverage = policy),
    seeded = list(kind = "criteria", cohort_definition_id = 93991L, coverage = policy),
    episodes = .ws6f_episode_population(),
    mixed = .ws6f_setop("union", "people", "episodes"),
    seeded_mixed = .ws6f_setop("union", "seeded", "episodes"),
    kept = .ws6f_child("mixed", .ws6f_has_members(5L)),
    seeded_kept = .ws6f_child("seeded_mixed", .ws6f_has_members(5L)),
    grandchild = .ws6f_child("kept", .ws6f_has_members(5L))))
  expect_s3_class(.irPrepare(plan), "dsomop_ir")
  for (backend in names(.ws6f_pop_backends)) {
    ctx <- .ws4_ctx(.ws6f_pop_backends[[backend]]$handle, plan)
    tryCatch({
      for (population in c("people", "seeded", "mixed", "seeded_mixed", "kept", "seeded_kept", "grandchild")) {
        rows <- .fetchFragment(ctx, .compilePopulationByName(ctx, population)$roster)
        person <- .ws6f_person_rows(rows)
        expect_equal(nrow(person), 1L, info = paste(backend, population))
        expect_identical(as.character(person$origin_source), "(persons)", info = paste(backend, population))
        expect_true(all(is.na(person$index_source)), info = paste(backend, population))
        expect_true(all(is.na(person$index_key)), info = paste(backend, population))
      }
      # The five physical members span two linked episodes (four and two
      # links, with a shared member). Only the person row satisfies == 5.
      expect_equal(nrow(.ws6_roster(ctx, "kept")), 1L, info = backend)
      expect_equal(nrow(.ws6_roster(ctx, "seeded_kept")), 1L, info = backend)
    }, finally = .planContextClose(ctx))
  }
})

test_that("missing required episode provenance does not become a person association", {
  plan <- .ws6_plan(.ws6f_episode_events(), list(
    people = list(kind = "criteria", coverage = list(on_missing = "keep")),
    episodes = .ws6f_episode_population(), mixed = .ws6f_setop("union", "people", "episodes"),
    kept = .ws6f_child("mixed", .ws6f_has_members(5L))))
  for (backend in names(.ws6f_pop_backends)) {
    h <- .ws6f_pop_backends[[backend]]$handle
    ctx <- .ws4_ctx(h, plan)
    tryCatch({
      mixed <- .compilePopulationByName(ctx, "mixed")
      before <- .fetchFragment(ctx, mixed$roster)
      episodes <- before[as.numeric(before$subject_id) == 1015 & !is.na(before$cohort_start_date), , drop = FALSE]
      expect_equal(nrow(episodes), 2L, info = backend)
      # The just-frozen final roster is the newest owned table. Corrupt only
      # its episode rows, leaving the genuine person association intact.
      table <- tail(ctx$compile_temp_tables, 1L)
      expect_match(mixed$roster$select_body, paste0("FROM ", table, " t"), fixed = TRUE, info = backend)
      .executeStatement(h, paste0("UPDATE ", table,
        " SET origin_source = NULL, origin_key = NULL, index_source = NULL, index_key = NULL",
        " WHERE subject_id = 1015 AND cohort_start_date IS NOT NULL"))
      rows <- .fetchFragment(ctx, .compilePopulationByName(ctx, "kept")$roster)
      expect_equal(nrow(.ws6f_person_rows(rows)), 1L, info = backend)
      expect_equal(sum(as.numeric(rows$subject_id) == 1015 & !is.na(rows$cohort_start_date)), 0L, info = backend)
    }, finally = .planContextClose(ctx))
  }
})

test_that("start-date nullability follows members and survives every child fragment", {
  policy <- list(on_missing = "keep")
  plan <- .ws6_plan(.ws6f_episode_events(), list(
    people = list(kind = "criteria", coverage = policy),
    nobody = list(kind = "criteria", coverage = policy, where = list(cohort = list(cohort_definition_id = 93992L))),
    episodes = .ws6f_episode_population(),
    mixed = .ws6f_setop("union", "people", "episodes"),
    reversed = .ws6f_setop("union", "episodes", "people"),
    indexed_union = .ws6f_setop("union", "episodes", "episodes"),
    inter_person = .ws6f_setop("intersect", "people", "episodes"),
    inter_episode = .ws6f_setop("intersect", "episodes", "people"),
    diff_mixed = .ws6f_setop("setdiff", "mixed", "nobody"),
    diff_episode = .ws6f_setop("setdiff", "episodes", "nobody"),
    child = .ws6f_child("mixed"), grandchild = .ws6f_child("child")))
  expect_s3_class(.irPrepare(plan), "dsomop_ir")
  want <- c(people = TRUE, episodes = FALSE, mixed = TRUE, reversed = TRUE,
            indexed_union = FALSE, inter_person = TRUE, inter_episode = FALSE,
            diff_mixed = TRUE, diff_episode = FALSE, child = TRUE, grandchild = TRUE)
  for (backend in names(.ws6f_pop_backends)) {
    ctx <- .ws4_ctx(.ws6f_pop_backends[[backend]]$handle, plan)
    tryCatch({
      for (population in names(want)) {
        result <- .compilePopulationByName(ctx, population)
        fragments <- list(result$roster, result$candidates,
          .fragmentGet(ctx, .fragmentName("pop", population, "sel", ctx = ctx)))
        if (identical(result$kind, "setop")) {
          fragments <- c(fragments, list(.fragmentGet(ctx,
            .fragmentName("pop", population, "all", ctx = ctx))))
        } else {
          fragments <- c(fragments, list(
            .fragmentGet(ctx, .fragmentName("cand", population, "raw", ctx = ctx)),
            .fragmentGet(ctx, .fragmentName("cand", population, "crit", ctx = ctx))))
        }
        for (fragment in Filter(Negate(is.null), fragments)) {
          info <- paste(backend, population, fragment$name)
          expect_identical(fragment$columns$cohort_start_date$nullable, unname(want[[population]]), info = info)
          expect_identical(fragment$columns$cohort_start_date$type, "date", info = info)
          rows <- .fetchFragment(ctx, fragment)
          expect_true(nrow(rows) > 0L, info = info)
          expect_identical(anyNA(rows$cohort_start_date), unname(want[[population]]), info = info)
        }
        if (population %in% c("mixed", "reversed", "diff_mixed", "child", "grandchild")) {
          expect_true(result$indexed, info = paste(backend, population))
          expect_equal(nrow(.ws6f_person_rows(.fetchFragment(ctx, result$roster))), 1L, info = paste(backend, population))
        }
      }
    }, finally = .planContextClose(ctx))
  }
})

# Start from a native validated context, then supply the closed IR-only node
# directly at the frozen compiler interface, as the independent review did.
.ws6f_empty_context <- function(handle, populations = list()) {
  ctx <- .ws4_ctx(handle, .ws6_plan(.ws6f_episode_events(),
    c(list(empty = list(kind = "criteria", coverage = list(on_missing = "keep"))), populations)))
  ctx$ir$populations$empty$legacy_empty <- TRUE
  ctx$ir$populations$empty$legacy_episodes <- "observation_periods"
  ctx
}

test_that("the closed legacy empty compiles to typed owned memoised empty relations", {
  for (backend in names(.ws6f_pop_backends)) {
    h <- .ws6f_pop_backends[[backend]]$handle
    owned_before <- h$temp_tables
    ctx <- .ws6f_empty_context(h)
    tryCatch({
      node <- .irNamedNode(ctx$ir, "population", "empty")
      result <- .compilePopulation(ctx, node)
      expect_null(result$coverage, info = backend)
      expect_null(result$segments, info = backend)
      fragments <- list(result$candidates, result$roster,
        .fragmentGet(ctx, .fragmentName("cand", "empty", "raw", ctx = ctx)),
        .fragmentGet(ctx, .fragmentName("pop", "empty", "sel", ctx = ctx)))
      for (fragment in fragments) {
        expect_false(is.null(fragment), info = backend)
        expect_equal(nrow(.fetchFragment(ctx, fragment)), 0L, info = paste(backend, fragment$name))
        expect_identical(fragment$columns$subject_id$type, "bigint", info = backend)
        expect_identical(fragment$columns$cohort_start_date$type, "date", info = backend)
        expect_identical(fragment$columns$cohort_end_date$type, "date", info = backend)
      }
      tables <- ctx$compile_temp_tables
      expect_true(length(tables) >= 2L, info = backend)
      expect_true(all(tables %in% h$temp_tables), info = backend)
      expect_identical(.compilePopulation(ctx, node), result, info = backend)
      expect_identical(.compilePopulationByName(ctx, "empty"), result, info = backend)
      expect_identical(ctx$compile_temp_tables, tables, info = backend)
    }, finally = .planContextClose(ctx))
    expect_identical(h$temp_tables, owned_before, info = backend)
  }
})

test_that("closed legacy empty operands obey union intersection and difference identities", {
  populations <- list(people = list(kind = "criteria", coverage = list(on_missing = "keep")),
                      episodes = .ws6f_episode_population())
  for (member in c("people", "episodes")) {
    for (op in c("union", "intersect", "setdiff")) {
      populations[[paste(member, op, "left", sep = "_")]] <- .ws6f_setop(op, "empty", member)
      populations[[paste(member, op, "right", sep = "_")]] <- .ws6f_setop(op, member, "empty")
    }
  }
  for (backend in names(.ws6f_pop_backends)) {
    ctx <- .ws6f_empty_context(.ws6f_pop_backends[[backend]]$handle, populations)
    tryCatch({
      for (member in c("people", "episodes")) {
        reference <- .ws6_roster(ctx, member)
        expect_true(nrow(reference) > 0L, info = paste(backend, member))
        for (op in c("union", "intersect", "setdiff")) {
          for (side in c("left", "right")) {
            population <- paste(member, op, side, sep = "_")
            rows <- .ws6_roster(ctx, population)
            info <- paste(backend, population)
            if (op == "union" || (op == "setdiff" && side == "right")) {
              expect_identical(rows, reference, info = info)
            } else {
              expect_equal(nrow(rows), 0L, info = info)
            }
          }
        }
      }
    }, finally = .planContextClose(ctx))
  }
})

test_that("the legacy empty exception refuses open shapes and remains IR-only", {
  invalid <- list(
    false = list(legacy_empty = FALSE), missing_mode = list(legacy_episodes = NULL),
    null_without_mode = list(legacy_empty = NULL),
    null_with_mode = list(legacy_empty = NULL),
    wrong_mode = list(legacy_episodes = "cohort_rows"), wrong_kind = list(kind = "index"),
    parent = list(parent = "people"), where = list(where = .ws6f_has_members(5L)),
    cohort = list(cohort_definition_id = 93991L),
    named_cohort = list(legacy_cohort_table = "owned_cohort"),
    named_cohort_null = list(legacy_cohort_table = NULL),
    episode_policy = list(episode_policy = "unsupported"))
  for (variant in names(invalid)) {
    ctx <- .ws6f_empty_context(.ws6f_pop_backends$sqlite$handle)
    tryCatch({
      node <- .irNamedNode(ctx$ir, "population", "empty")
      for (field in names(invalid[[variant]])) node[field] <- invalid[[variant]][field]
      if (identical(variant, "null_without_mode")) node$legacy_episodes <- NULL
      expect_error(.compilePopulation(ctx, node), info = variant)
      expect_equal(length(ctx$compile_temp_tables), 0L, info = variant)
    }, finally = .planContextClose(ctx))
  }
  ctx <- .ws6f_empty_context(.ws6f_pop_backends$sqlite$handle)
  tryCatch({
    node <- .irNamedNode(ctx$ir, "population", "empty")
    node$legacy_empty <- NULL
    expect_error(.compilePopulation(ctx, node), "v1 compatibility")
  }, finally = .planContextClose(ctx))
  plan <- .ws6_plan(populations = list(empty = list(
    kind = "criteria", legacy_empty = TRUE, legacy_episodes = "observation_periods")))
  expect_error(.irValidateWire(.irJsonForm(plan)))
})

test_that("a schema-admitted episode policy cannot populate a legacy empty relation", {
  for (policy in c("any_episode", "all_episodes", "first_episode", "last_episode")) {
    ctx <- .ws6f_empty_context(.ws6f_pop_backends$sqlite$handle)
    tryCatch({
      node <- .irNamedNode(ctx$ir, "population", "empty")
      node$episode_policy <- policy
      result <- .compilePopulation(ctx, node)
      expect_equal(nrow(.fetchFragment(ctx, result$roster)), 0L, info = policy)
      expect_null(result$coverage, info = policy)
      expect_null(result$segments, info = policy)
    }, finally = .planContextClose(ctx))
  }
})
