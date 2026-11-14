# B2: the original criteria base alone inherits the plan cohort's person seed.
.ws2f_seed_sex <- function(value) list(type = "sex", params = list(value = value))
.ws2f_seed_plan <- function(sex = "M", ref = "Ws2f_owned") {
  list(cohort = list(type = "spec", filter_tree = .ws2f_seed_sex(sex)),
    populations = list(
      base = list(kind = "criteria", cohort_definition_id = ref),
      sibling = list(kind = "criteria", filter_tree = list(or = list(
        .ws2f_seed_sex("M"), .ws2f_seed_sex("F"))))),
    outputs = list(
      demo = list(type = "baseline", columns = "gender_concept_id", population_id = "base"),
      other = list(type = "baseline", columns = "gender_concept_id", population_id = "sibling")),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE))
}
.ws2f_seed_owned <- function(h) {
  DBI::dbExecute(h$conn, paste0("CREATE TEMP TABLE Ws2f_owned AS SELECT ",
    "person_id AS subject_id, '2030-01-01' AS cohort_start_date, ",
    "'2031-01-01' AS cohort_end_date FROM person WHERE person_id IN (1,3,5,7,9,11)"))
  register_test_temp(h, "Ws2f_owned")
}
.ws2f_seed_events <- function(h, dates = "2021-06-01") {
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  rows <- expand.grid(person_id = 1:15, episode = seq_along(dates))
  DBI::dbWriteTable(h$conn, "condition_occurrence", data.frame(
    condition_occurrence_id = as.integer(seq_len(nrow(rows))),
    person_id = as.integer(rows$person_id), condition_concept_id = 201820L,
    condition_start_date = dates[rows$episode], condition_end_date = NA_character_,
    condition_type_concept_id = 44818518L, visit_occurrence_id = NA_integer_), append = TRUE)
}

test_that("B2 disjoint and overlapping named bases retain their seed and sibling locality", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  withr::local_options(list(nfilter.subset = 3, nfilter.tab = 1))
  .ws2f_seed_owned(h)
  for (sex in c("F", "M")) {
    plan <- .ws2f_seed_plan(sex)
    ir <- .irPrepare(plan, h)
    expect_identical(ir$populations$base$parent, "v1_plan_cohort")
    expect_identical(ir$populations$base$legacy_cohort_table, "Ws2f_owned")
    expect_identical(ir$populations$base$legacy_episodes, "observation_periods")
    expect_null(ir$populations$sibling$parent)
    expect_null(ir$scope$cohort)
    expect_setequal(names(.irOutputClosure(ir, "demo")$populations),
                    c("base", "v1_plan_cohort"))
    expect_identical(names(.irOutputClosure(ir, "other")$populations), "sibling")
    order <- .irOrder(ir)$populations
    expect_lt(match("v1_plan_cohort", order), match("base", order))
    expect_identical(.irExplain(ir)$plan$populations$base$parent, "v1_plan_cohort")
    # An empty base remains private while the independent sibling still runs.
    other <- .planExecute(h, plan, list(other = "D_ws2f_other"))
    expect_identical(other$other$person_id, 1:15)
    if (identical(sex, "F")) {
      expect_error(.planExecute(h, plan, list(demo = "D_ws2f_demo")),
                   "insufficient individuals|Insufficient|insufficient_support")
    } else {
      result <- .planExecute(h, plan, list(demo = "D_ws2f_demo", other = "D_ws2f_other"))
      expect_identical(result$demo$person_id, c(1L, 3L, 5L, 7L, 9L, 11L))
      expect_identical(result$demo$gender_concept_id, rep(8507L, 6L))
      expect_identical(result$other$person_id, 1:15)
    }
  }
  expect_true("Ws2f_owned" %in% h$temp_tables)
  expect_equal(nrow(DBI::dbReadTable(h$conn, "Ws2f_owned")), 6L)
})

test_that("B2 the parent references the allocated seed and applies to filter-only bases", {
  plan <- .ws2f_seed_plan()
  plan$populations$base <- list(kind = "criteria", filter_tree = .ws2f_seed_sex("F"))
  plan$populations$v1_plan_cohort <- list(kind = "criteria", filter_tree = .ws2f_seed_sex("F"))
  ir <- .irPrepare(plan)
  parent <- ir$populations$base$parent
  expect_true(is.character(parent) && length(parent) == 1L)
  expect_false(identical(parent, "v1_plan_cohort"))
  expect_true(parent %in% names(ir$populations))
  expect_identical(ir$populations[[parent]]$where$legacy$params$value, "M")
  expect_identical(ir$populations$base$where$legacy$params$value, "F")
  expect_identical(ir$populations$base$legacy_episodes, "observation_periods")
  expect_identical(ir$populations$base$legacy_index_anchor, "scope_cohort")
  expect_null(ir$populations$v1_plan_cohort$parent)
  expect_setequal(names(.irOutputClosure(ir, "demo")$populations), c("base", parent))
  order <- .irOrder(ir)$populations
  expect_lt(match(parent, order), match("base", order))
  plan$cohort <- NULL
  expect_null(.irPrepare(plan)$populations$base$parent)
})

test_that("B2 criteria base episodes are fresh OP rows with independent plan and scope anchors", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  withr::local_options(list(nfilter.subset = 3, nfilter.tab = 1))
  .ws2f_seed_owned(h)
  .ws2f_seed_events(h, c("2021-06-01", "2022-06-01"))
  # The inherited seed carries two recurrent episodes and event identities;
  # neither these nor the named restriction's 2030 dates become the OP roster.
  seed <- data.frame(subject_id = rep(seq.int(1L, 15L, 2L), each = 2L),
    cohort_start_date = rep(c("2021-06-01", "2022-06-01"), 8L),
    cohort_end_date = rep(c("2021-06-30", "2022-06-30"), 8L), index_event_id = 1:16)
  DBI::dbWriteTable(h$conn, "Ws2f_seed", seed, temporary = TRUE)
  register_test_temp(h, "Ws2f_seed")
  scope <- data.frame(subject_id = 1:15, cohort_start_date = "2022-06-01",
                       cohort_end_date = "2022-06-30")
  DBI::dbWriteTable(h$conn, "Ws2f_anchor", scope, temporary = TRUE)
  register_test_temp(h, "Ws2f_anchor")
  plan <- .ws2f_seed_plan()
  plan$populations$base$filter_tree <- list(type = "has_concept", params = list(
    concept_id = 201820L, table = "condition_occurrence", min_count = 1L,
    window = list(start = 0L, end = 30L)))
  plan$populations$base$episode_policy <- "all_episodes"
  for (anchor in c("Ws2f_seed", "Ws2f_anchor")) {
    if (identical(anchor, "Ws2f_anchor")) plan$scope <- list(cohort = anchor)
    ir <- .irPrepare(plan, h)
    expect_identical(ir$populations$base$parent, "v1_plan_cohort")
    expect_identical(ir$populations$base$legacy_index_anchor, "scope_cohort")
    expect_identical(ir$populations$base$episode_policy, "all_episodes")
    resolved <- .planResolvePopulations(h, plan, .buildBlueprint(h),
      base_cohort_table = "Ws2f_seed", index_cohort_table = anchor)
    roster <- DBI::dbReadTable(h$conn, resolved$base$cohort_table)
    roster <- roster[order(roster$subject_id), , drop = FALSE]
    expect_identical(roster$subject_id, c(1L, 3L, 5L, 7L, 9L, 11L))
    expect_identical(roster$cohort_start_date, rep("2020-01-01", 6L))
    expect_identical(roster$cohort_end_date, rep("2024-12-31", 6L))
    expect_false("index_event_id" %in% names(roster))
  }
})

test_that("B2 an indexed seed contributes membership without inherited index identity", {
  plan <- .ws2f_seed_plan()
  plan$cohort <- list(type = "spec", spec = list(type = "condition", concept_set = 201820L))
  ir <- .irPrepare(plan)
  parent <- ir$populations$base$parent
  expect_identical(parent, "v1_plan_cohort")
  expect_true(.pnv2PopulationIndexed(ir$populations, parent))
  expect_identical(.irIndexSourceTags(ir, parent), "legacy_spec")
  expect_false(.pnv2PopulationIndexed(ir$populations, "base"))
  expect_identical(.irIndexSourceTags(ir, "base"), character(0))
  expect_identical(ir$populations$base$legacy_episodes, "observation_periods")
  expect_setequal(names(.irOutputClosure(ir, "demo")$populations), c("base", parent))
  document <- .irDocument(ir)
  document$outputs$demo$grain <- NULL
  normalized <- .planNormalizeV2(document, schema = .irSchema("ir-v2"))
  expect_identical(normalized$outputs$demo$grain, "person")

  # A singleton set-op lowered to criteria does inherit its member's identity.
  plan$populations <- list(
    indexed = list(kind = "criteria", index_event = list(table = "condition_occurrence",
      concept_set = 201820L, primary_limit = "all")),
    base = list(kind = "setop", setop = list(op = "union", members = list("indexed"))))
  plan$outputs$other <- NULL
  ir <- .irPrepare(plan)
  expect_identical(ir$populations$base$parent, "indexed")
  expect_null(ir$populations$base$legacy_episodes)
  expect_true(.pnv2PopulationIndexed(ir$populations, "base"))
  expect_identical(.irIndexSourceTags(ir, "base"), .irIndexSourceTags(ir, "indexed"))
  expect_setequal(names(.irOutputClosure(ir, "demo")$populations), c("base", "indexed"))
})

test_that("B2 original index base dispatch never inherits the plan seed", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  withr::local_options(list(nfilter.subset = 3, nfilter.tab = 1))
  .ws2f_seed_events(h)
  plan <- .ws2f_seed_plan("F")
  plan$populations$base <- list(kind = "criteria", index_event = list(
    table = "condition_occurrence", concept_set = 201820L, primary_limit = "all"))
  ir <- .irPrepare(plan, h)
  expect_identical(ir$populations$base$kind, "index")
  expect_null(ir$populations$base$parent)
  expect_identical(names(.irOutputClosure(ir, "demo")$populations), "base")
  result <- .planExecute(h, plan, list(demo = "D_ws2f_demo"))
  expect_identical(result$demo$person_id, 1:15)
})

test_that("B2 set base dispatch keeps its members including singleton criteria lowering", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  withr::local_options(list(nfilter.subset = 3, nfilter.tab = 1))
  for (members in list(list("men"), list("men", "men"), list("men", "women"))) {
    plan <- .ws2f_seed_plan("F")
    plan$populations <- list(
      men = list(kind = "criteria", filter_tree = .ws2f_seed_sex("M")),
      women = list(kind = "criteria", filter_tree = .ws2f_seed_sex("F")),
      base = list(kind = "setop", setop = list(op = "union", members = members)))
    plan$outputs$other <- NULL
    ir <- .irPrepare(plan, h)
    singleton <- length(unique(unlist(members))) == 1L
    expect_identical(ir$populations$base$kind, if (singleton) "criteria" else "setop")
    expect_identical(ir$populations$base$parent, if (singleton) "men" else NULL)
    expect_null(ir$populations$base$legacy_episodes)
    expect_setequal(names(.irOutputClosure(ir, "demo")$populations),
                    c("base", unique(unlist(members))))
    order <- .irOrder(ir)$populations
    expect_true(all(match(unlist(members), order) < match("base", order)))
    result <- .planExecute(h, plan, list(demo = "D_ws2f_demo"))
    expect_identical(result$demo$person_id, if (singleton) seq.int(1L, 15L, 2L) else 1:15)
  }
})
