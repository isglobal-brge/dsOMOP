# WS-2c: source typing, result classes and anchor/closure integration after the
# stopped fix round. These plans pass the complete wire-to-IR preparation.
.roles_review_example <- function(name = "A") {
  filename <- paste0("plan-v2-example-", name, ".json")
  path <- system.file("schema", "examples", filename, package = "dsOMOP")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "schema", "examples", filename)
  plan <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (identical(name, "A")) plan$events$metf_rx$end <- NULL
  plan
}

.roles_review_interval <- function(reducer) {
  plan <- .roles_review_example()
  plan$variables$course <- list(source = list(interval = "metf_era"), reducer = reducer)
  plan$outputs$baseline$columns <- list("course")
  plan
}

test_that("R2: interval predicates retain source typing inside boolean trees and unavailable plans", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  predicate <- function(leaf) list(and = list(
    list(var = "start_date", op = ">=", value = "2000-01-01"),
    list(not = list(or = list(leaf, list(var = "end_date", op = "is_null"))))))
  invalid <- list(
    list(var = "made_up", op = "is_null"),
    list(var = "person_id", op = "==", value = 1L),
    list(var = "source_ns", op = "in", value = list("metf_rx")),
    list(var = "n_events", op = ">", value = 0L),
    list(var = "supply", op = "not_null"),
    list(var = "concept_id", op = "in", value = list("1")),
    list(var = "end_date", op = "between", value = list(lower = 1L, upper = 2L)))
  for (leaf in invalid) {
    plan <- .roles_review_interval(list(fn = "all", predicate = predicate(leaf)))
    # Unavailability does not exempt an interval's declared columns from typing.
    plan$events$metf_rx$table <- "specimen"
    plan$events$metf_rx$on_missing_table <- "unavailable"
    error <- tryCatch(.irPrepare(plan, handle), dsomop_plan_rejection = identity)
    expect_s3_class(error, "dsomop_plan_rejection")
    expect_match(conditionMessage(error), "/variables/course/reducer/predicate", fixed = TRUE)
  }
  plan <- .roles_review_interval(list(fn = "all", predicate = predicate(
    list(var = "concept_id", op = "in", value = list(1L, 2L)))))
  expect_s3_class(.irPrepare(plan, handle), "dsomop_ir")
  for (column in c("person_id", "event_id", "era_key", "unit_concept_id", "made_up")) {
    error <- tryCatch(.irPrepare(.roles_review_interval(list(fn = "mode", value = column)), handle),
                      dsomop_plan_rejection = identity)
    expect_s3_class(error, "dsomop_plan_rejection")
    expect_match(conditionMessage(error), "/variables/course/reducer/value", fixed = TRUE)
  }
})

test_that("N1: preserved interval and event dates keep class 2", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  for (column in c("start_date", "end_date", "end_recorded")) {
    plan <- .roles_review_interval(list(fn = "mode", value = column))
    ir <- .irPrepare(plan, handle)
    expect_identical(.irExplain(ir, handle)$column_classes$baseline$course$class, 2L, info = column)
    expect_identical(.irExplain(ir)$column_classes$baseline$course$class, 2L, info = column)
    plan$variables$course$source <- list(event = "metf_rx")
    ir <- .irPrepare(plan, handle)
    expect_identical(.irExplain(ir, handle)$column_classes$baseline$course$class, 2L, info = column)
  }
  for (reducer in list(list(fn = "count"), list(fn = "mode", value = "concept_id"))) {
    ir <- .irPrepare(.roles_review_interval(reducer), handle)
    expect_identical(.irExplain(ir, handle)$column_classes$baseline$course$class, 3L)
  }
})

test_that("N1: adapter date results use their declared type without a physical table column", {
  handle <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(handle$conn))
  plan <- .ws4_plan(events = list(
    episodes = list(table = "episode"),
    members = list(episode_members = list(episode = list(event = "episodes"), tables = list("drug_exposure"))),
    visits = list(table = "visit_occurrence"),
    cost = list(cost = list(event = list(event = "visits"), field = "total_cost"))))
  plan$variables <- list(
    member_date = list(source = list(event = "members"), reducer = list(fn = "mode", value = "start_date")),
    cost_date = list(source = list(event = "cost"), reducer = list(fn = "mode", value = "end_date")))
  ir <- .irPrepare(plan, handle)
  for (classes in list(.irExplain(ir, handle)$column_classes$o, .irExplain(ir)$column_classes$o)) {
    expect_identical(classes$member_date$class, 2L)
    expect_identical(classes$cost_date$class, 2L)
  }
})

test_that("concept closure retains both effective v1 scopes and excludes unrelated output concepts", {
  plan <- list(outputs = list(
    scoped = list(type = "person_level", tables = list(measurement = list(
      concept_set = 3004410L, features = list(lab_mean = list(
        type = "mean_value", name = "lab_mean", concept_set = 3004411L,
        value_column = "value_as_number"))))),
    unrelated = list(type = "person_level", tables = list(measurement = list(
      concept_set = 3004412L, features = list(lab_other = list(
        type = "mean_value", name = "lab_other", concept_set = 3004413L,
        value_column = "value_as_number")))))))
  ir <- .irPrepare(plan)
  closure <- .irOutputClosure(ir, "scoped")
  event <- Filter(function(node) !is.null(node$legacy_concept_scope), closure$events)[[1L]]
  expect_identical(names(closure$concept_sets), event$legacy_concept_scope$concept_set_ref)
  expect_identical(as.integer(unlist(event$concept_set$concepts)), 3004411L)
  expect_identical(closure$events, ir$events[names(closure$events)])
  expect_identical(names(closure$outputs), "scoped")
  expect_lt(length(closure$concept_sets), length(ir$concept_sets))
  expect_length(.irSemanticErrors(closure), 0L)
})

test_that("A-20(b): matching templates inventory nested search anchors in their candidate phase", {
  plan <- .roles_review_example("B")
  inner <- list(source = list(event = "diabetes"))
  outer <- list(source = list(event = "statin_rx"), which = "last",
                search = list(start = -30L, end = 0L, anchor = inner))
  criterion <- list(has = list(source = list(event = "diabetes"),
                              window = list(start = -365L, end = 0L, anchor = outer)))
  plan$populations$eligible$where <- criterion
  plan$populations$cc$match$match <- list(list(strata = list(source = list(event = "diabetes"),
    window = list(start = -365L, end = 0L, anchor = outer))))
  plan$outputs$anchor_dates <- list(rows = "cc", format = "long", sources = list(list(event = "diabetes")),
    options = list(anchors = list(a = outer, b = outer, c = inner)))
  plan$publish <- NULL
  ir <- .irPrepare(plan)
  key <- function(anchors) vapply(anchors, .jcsCanonical, "")
  anchors <- .irOrder(ir)$anchors
  cases <- ir$populations$cc$match$cases
  expected <- key(list(ir$outputs$anchor_dates$options$anchors$a,
                       ir$outputs$anchor_dates$options$anchors$c))
  expect_setequal(key(anchors$candidate$eligible), expected)
  expect_setequal(key(anchors$final[[cases]]), expected)
  expect_setequal(key(anchors$final$cc), expected)
  expect_length(anchors$candidate$eligible, 2L)
  expect_length(anchors$final[[cases]], 2L)
  expect_length(anchors$final$cc, 2L)
  expect_length(anchors$final[[cases]], 2L)
  expect_length(anchors$candidate[[cases]], 0L)
  expect_length(anchors$final$eligible, 0L)
  expect_length(anchors$candidate$cc, 0L)
  # The complete dependency closure must keep both the outer and search sources.
  closure <- .irOutputClosure(ir, "anchor_dates")
  expect_true(all(c("statin_rx", "diabetes") %in% names(closure$events)))
  expect_identical(.irOrder(closure)$anchors$final$cc, anchors$final$cc)
})
