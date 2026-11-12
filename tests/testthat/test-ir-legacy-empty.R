# A-21's closed compatibility dependency for the WS-6 empty compiler path.
.legacy_empty_v1_plan <- function() {
  list(populations = list(
    base = list(kind = "criteria"),
    nobody = list(kind = "criteria", episode_policy = "last_episode"),
    seeded = list(kind = "criteria", cohort_definition_id = 1L),
    filtered = list(kind = "criteria", filter_tree = list(
      type = "sex", params = list(value = "F")))),
    outputs = list(p = list(type = "person_level", population_id = "seeded",
                            tables = list(person = list("gender_concept_id")))))
}

test_that("A-21 lowering distinguishes bare legacy emptiness from native persons", {
  plan <- .legacy_empty_v1_plan()
  ir <- .irPrepare(plan)
  node <- ir$populations$nobody
  expect_identical(node$kind, "criteria")
  expect_identical(node$legacy_empty, TRUE)
  expect_identical(node$legacy_episodes, "observation_periods")
  for (key in c("parent", "where", "cohort_definition_id", "episode_policy",
                "legacy_cohort_table")) expect_null(node[[key]], info = key)
  for (key in c("base", "seeded", "filtered")) {
    expect_null(ir$populations[[key]]$legacy_empty, info = key)
  }
  expect_identical(attr(ir, "legacy_annex")[["populations/nobody"]]$unread_episode_policy,
                   "last_episode")
  plan$populations$nobody$episode_policy <- "first_episode"
  other <- .irPrepare(plan)
  expect_identical(.planSemanticDigest(.irDocument(ir))$sha256,
                   .planSemanticDigest(.irDocument(other))$sha256)
  changed <- .irDocument(ir)
  changed$populations$nobody$legacy_empty <- NULL
  expect_false(identical(.planSemanticDigest(.irDocument(ir))$sha256,
                         .planSemanticDigest(changed)$sha256))
})

test_that("A-21 empty nodes validate only in their closed IR shape", {
  ir <- .irDocument(.irPrepare(.legacy_empty_v1_plan()))
  valid <- function(node) {
    plan <- ir
    plan$populations$nobody <- node
    .jsonSchemaValidate(.irSchema("ir-v2"), plan)$valid
  }
  empty <- list(kind = "criteria", legacy_empty = TRUE,
                legacy_episodes = "observation_periods")
  expect_true(valid(empty))
  for (key in c("parent", "where", "cohort_definition_id")) {
    node <- empty
    node[key] <- list(NULL)
    expect_true(valid(node), info = paste("null", key))
  }
  invalid <- list(
    false = modifyList(empty, list(legacy_empty = FALSE)),
    missing_mode = empty[setdiff(names(empty), "legacy_episodes")],
    wrong_mode = modifyList(empty, list(legacy_episodes = "cohort_rows")),
    parent = modifyList(empty, list(parent = "base")),
    cohort = modifyList(empty, list(cohort_definition_id = 1L)),
    criteria = modifyList(empty, list(where = list(person = list(
      var = "gender_concept_id", op = "==", value = 8532L)))),
    named_cohort = modifyList(empty, list(legacy_cohort_table = "owned_cohort")),
    named_cohort_null = c(empty, list(legacy_cohort_table = NULL)),
    index = list(kind = "index", index = list(source = list(cohort_definition_id = 1L)),
                 legacy_empty = TRUE, legacy_episodes = "observation_periods"))
  for (key in names(invalid)) expect_false(valid(invalid[[key]]), info = key)
  wire <- list(version = 2L,
    meta = list(language_version = "2.0", semantic_version = "2.0.0", generated_by = "test"),
    populations = list(nobody = empty),
    outputs = list(p = list(rows = "nobody", format = "wide")))
  expect_false(.jsonSchemaValidate(.irSchema("plan-v2"), wire)$valid)
  expect_error(.irPrepare(wire), class = "dsomop_ir_validation_error")
})

test_that("A-21 empty operands preserve the historical private set results", {
  h <- create_test_handle()
  ctx <- NULL
  on.exit({ if (!is.null(ctx)) .planContextClose(ctx); cleanup_handle(h) })
  plan <- .legacy_empty_v1_plan()
  for (op in c("union", "intersect", "setdiff")) {
    plan$populations[[op]] <- list(kind = "setop", setop = list(
      op = op, members = list("seeded", "nobody")))
  }
  plan$populations$self <- list(kind = "setop", setop = list(
    op = "setdiff", members = list("seeded", "seeded")))
  plan$populations$repeated <- list(kind = "setop", setop = list(
    op = "setdiff", members = list("seeded", "filtered", "filtered")))
  plan$populations$once <- list(kind = "setop", setop = list(
    op = "setdiff", members = list("seeded", "filtered")))
  ir <- .irPrepare(plan, h)
  expect_true(ir$populations$nobody$legacy_empty)
  ctx <- .planContext(h, plan)
  withr::with_options(list(nfilter.subset = 3L), {
    pops <- .planResolvePopulations(h, plan, .buildBlueprint(h), ctx = ctx)
    expect_length(pops$nobody$person_ids, 0L)
    expect_length(pops$intersect$person_ids, 0L)
    expect_length(pops$self$person_ids, 0L)
    expect_gt(length(pops$seeded$person_ids), 0L)
    expect_setequal(pops$union$person_ids, pops$seeded$person_ids)
    expect_setequal(pops$setdiff$person_ids, pops$seeded$person_ids)
    expect_setequal(pops$repeated$person_ids, pops$once$person_ids)
  })
})
