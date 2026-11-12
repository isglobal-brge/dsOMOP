# WS-8, Astra review B1 (server side): a federation with a dsOMOP 2.10.2 site
# runs a v1 plan as the legacy call of dsOMOPClient 2.10.2 on every site. A
# 2.10.4 server accepts that call -- positional, without commit_policy -- and
# applies the documented defaults: commit_policy = "server" (the server's own
# strict mode decides), output_mode as sent, no scope, combine = "union".
# Strict: one root below nfilter.subset refuses the plan and nothing is
# assigned. Non-strict: the passing family is committed and the status is
# partial. Fresh destinations land under the requested symbols, which is what
# the 2.10.2 client checks afterwards.

.legacy_plan <- function() {
  list(outputs = list(
    people = list(type = "event_level", table = "person",
                  columns = c("person_id", "gender_concept_id"),
                  representation = list(format = "long")),
    drugs = list(type = "event_level", table = "drug_exposure",
                 columns = c("person_id", "drug_concept_id"),
                 representation = list(format = "long"))),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE))
}

test_that("the documented defaults of the execution methods are the 2.10.2 behaviour", {
  expect_identical(formals(omopPlanExecuteDS)$commit_policy, "server")
  expect_identical(formals(omopDpPlanExecuteDS)$commit_policy, "server")
  expect_identical(formals(omopPlanExecuteDS)$output_mode, "memory")
  expect_null(formals(omopPlanExecuteDS)$scope)
  expect_identical(formals(omopPlanExecuteDS)$combine, "union")
})

test_that("a 2.10.4 server runs the 2.10.2 client's call with commit_policy = 'server'", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .setLegacyTestPersonKey(handle, "legacy-execution-call", .local_envir = environment())
  .setHandle("legacy_call", handle)
  on.exit(.removeHandle("legacy_call"), add = TRUE)
  out <- c(people = "D_people", drugs = "D_drugs")
  call <- .legacy_execution_call("omopPlanExecuteDS", "legacy_call", .legacy_plan(), out)
  expect_null(as.list(call)$commit_policy)
  expect_true(startsWith(call[[3L]], "B64:"))
  expect_true(startsWith(call[[4L]], "B64:"))
  # the test database: 15 persons, 6 of them with a drug exposure
  withr::local_options(list(nfilter.subset = 8L))

  # strict server (the default): the small root refuses the whole plan
  strict <- new.env(parent = environment())
  withr::with_options(list(dsomop.query_strict = TRUE),
    strict$exec <- eval(call, strict))
  expect_identical(strict$exec$status, "refused")
  expect_identical(strict$exec$rejected$drugs$code, "insufficient_support")
  expect_identical(ls(strict), "exec")

  # non-strict server: the passing family is committed, status partial
  relaxed <- new.env(parent = environment())
  withr::with_options(list(dsomop.query_strict = FALSE),
    relaxed$exec <- eval(call, relaxed))
  expect_identical(relaxed$exec$status, "partial")
  expect_identical(names(relaxed$exec$published), "people")
  expect_identical(relaxed$exec$published$people$people$symbol, "D_people")
  expect_identical(relaxed$exec$rejected$drugs$code, "insufficient_support")
  expect_setequal(ls(relaxed), c("exec", "D_people"))
  expect_identical(nrow(relaxed$D_people), 15L)

  # one output: 2.10.2 sends the bare named vector, not a B64 mapping
  one <- .legacy_execution_call("omopPlanExecuteDS", "legacy_call", .legacy_plan(),
                                c(people = "D_one"))
  expect_identical(one[[4L]], c(people = "D_one"))
  single <- new.env(parent = environment())
  single$exec <- eval(one, single)
  expect_identical(single$exec$status, "ok")
  expect_setequal(ls(single), c("exec", "D_one"))
  # the 2.10.2 client removes the status symbol; the outputs stay
  rm("exec", envir = single)
  expect_identical(nrow(single$D_one), 15L)
})

.empty_roots_plan <- function() {
  list(
    version = 2L,
    meta = list(language_version = "2.0", semantic_version = "2.0.0", generated_by = "ws8-b6-test"),
    events = list(visits = list(table = "visit_occurrence")),
    populations = list(all = list(kind = "criteria")),
    variables = list(n_visits = list(source = list(event = "visits"), reducer = list(fn = "count"))),
    outputs = list(visits_out = list(rows = "all", format = "wide", columns = list("n_visits"))),
    publish = list())
}

test_that("publish = [] completes real validation and empty publication (A-22)", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .setHandle("no_roots", handle)
  on.exit(.removeHandle("no_roots"), add = TRUE)
  plan <- .empty_roots_plan()
  tables <- DBI::dbListTables(handle$conn)
  contexts <- ls(.planContextRegistry, all.names = TRUE)
  staging <- handle$staging_dirs
  for (mode in c("memory", "staged")) {
    target <- new.env(parent = environment())
    # Use the exported endpoint and transport; no executor or validator mock.
    call <- as.call(list(as.name("omopPlanExecuteDS"), "no_roots",
                        .legacy_ds_encode(plan), "B64:e30", mode))
    target$exec <- eval(call, target)
    status <- eval(quote(omopPlanStatusDS("exec")), target)
    expect_identical(status$status, "ok")
    expect_length(status$published, 0L)
    expect_length(status$rejected, 0L)
    expect_identical(ls(target, all.names = TRUE), "exec")
    expect_identical(DBI::dbListTables(handle$conn), tables)
    expect_identical(handle$temp_tables, character(0))
    expect_identical(handle$staging_dirs, staging)
    expect_identical(ls(.planContextRegistry, all.names = TRUE), contexts)
    expect_true(eval(quote(omopStagedRevokeDS("exec")), target))
    expect_identical(ls(target, all.names = TRUE), character(0))
  }
})

test_that("an empty out mapping requires an explicitly empty v2 root array", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .setHandle("empty_mapping", handle)
  on.exit(.removeHandle("empty_mapping"), add = TRUE)
  target <- new.env(parent = emptyenv())
  for (publish in list(NULL, list("visits_out"), .pnv2EmptyObject())) {
    plan <- .empty_roots_plan()
    plan["publish"] <- list(publish)
    expect_error(.omopPlanExecuteDS("empty_mapping", plan, "B64:e30",
                                    target_env = target),
                 "out must have unique, non-empty output names")
  }
  legacy <- .legacy_plan()
  legacy$publish <- list()
  expect_error(.omopPlanExecuteDS("empty_mapping", legacy, list(),
                                  target_env = target),
               "out must have unique, non-empty output names")
  expect_identical(ls(target, all.names = TRUE), character(0))
})

test_that("empty roots do not bypass invalid unused declarations or site rules", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .setHandle("empty_validation", handle)
  on.exit(.removeHandle("empty_validation"), add = TRUE)
  target <- new.env(parent = emptyenv())
  invalid <- .empty_roots_plan()
  invalid$events$visits$unknown <- TRUE
  expect_error(.omopPlanExecuteDS("empty_validation", invalid, "B64:e30",
                                  target_env = target),
               "plan-v2.json", class = "dsomop_plan_rejection")
  invalid <- .empty_roots_plan()
  invalid$variables$n_visits$source$event <- "undeclared"
  expect_error(.omopPlanExecuteDS("empty_validation", invalid, "B64:e30",
                                  target_env = target),
               "semantic rules", class = "dsomop_plan_rejection")
  invalid <- .empty_roots_plan()
  invalid$events$visits$table <- "provider"
  expect_error(.omopPlanExecuteDS("empty_validation", invalid, "B64:e30",
                                  target_env = target),
               "site checks", class = "dsomop_plan_rejection")
  withr::with_options(list(dsomop.recipe_operations_disabled = "publish.roots"), {
    expect_error(.omopPlanExecuteDS("empty_validation", .empty_roots_plan(),
                                    "B64:e30", target_env = target),
                 "operation.*disabled", class = "dsomop_plan_rejection")
  })
  expect_identical(ls(target, all.names = TRUE), character(0))
  expect_identical(handle$temp_tables, character(0))
})
