# WS-2: omopPlanExplainDS and the recipe_language capability block
# (design-2.10.3 sections 4.4 and 6.15, defect A-E7).

.explain_example <- function(name) {
  path <- system.file("schema", "examples", name, package = "dsOMOP")
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

.explain_handle <- function(env = parent.frame()) {
  handle <- create_test_handle()
  symbol <- paste0("explain_", Sys.getpid(), "_", sample.int(1e6, 1L))
  .setHandle(symbol, handle)
  withr::defer({ .removeHandle(symbol); cleanup_handle(handle) }, envir = env)
  symbol
}

.explain_has_number <- function(x, name_pattern) {
  # no person/row counts anywhere in the payload
  if (is.list(x)) {
    nm <- names(x) %||% rep("", length(x))
    for (i in seq_along(x)) {
      if (grepl(name_pattern, nm[[i]]) && is.numeric(x[[i]])) return(TRUE)
      if (.explain_has_number(x[[i]], name_pattern)) return(TRUE)
    }
  }
  FALSE
}

test_that("explain returns the normalised v2 plan with defaults visible", {
  symbol <- .explain_handle()
  out <- omopPlanExplainDS(symbol, .explain_example("plan-v2-example-A.json"))
  expect_identical(out$language_version, "2.0")
  expect_identical(out$source_version, 2L)
  expect_identical(out$plan$populations$new_users$coverage$policy, "unique")
  expect_identical(unlist(out$publish), c("baseline", "tte"))
  expect_identical(out$defaults$outputs$tte$event_order, "first")
  expect_identical(unlist(out$defaults$outputs$tte$censor), c("observation_end", "death"))
  expect_false(out$defaults$outputs$tte$cohort_end_cap)
  expect_identical(out$defaults$reducers$hba1c_base$missing, "skip")
  expect_match(out$semantic_digest, "^[0-9a-f]{64}$")
  expect_true(all(c("measurement", "drug_exposure", "condition_occurrence") %in% names(out$roles)))
  expect_identical(out$roles$measurement$value, "value_as_number")
  expect_identical(out$column_classes$baseline$age$class, 2L)
  expect_identical(out$column_classes$baseline$sex$class, 3L)
  expect_true(is.list(out$order) && "new_users" %in% out$order$populations)
  expect_false(.explain_has_number(out[setdiff(names(out), "plan")],
                                  "^(n|count|n_persons|persons|rows|size|support)$"))
  expect_false(grepl("SELECT ", jsonlite::toJSON(out, auto_unbox = TRUE), fixed = TRUE))
})

test_that("explain reports the lowering of a v1 plan and its advisory annex", {
  symbol <- .explain_handle()
  v1 <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    outputs = list(
      demo = list(type = "baseline", columns = list("gender_concept_id", "year_of_birth"),
                  derived = list("age_at_index")),
      labs = list(type = "event_level", table = "measurement", columns = list("measurement_date"),
                  representation = list(format = "long"))))
  out <- omopPlanExplainDS(symbol, v1)
  expect_identical(out$source_version, 1L)
  expect_identical(out$plan$scope$cohort, 1L)
  expect_identical(out$plan$outputs$demo$grain, "episode")
  expect_identical(unlist(out$plan$outputs$labs$legacy_columns), "measurement_date")
  expect_true(is.list(out$legacy_annex) || is.null(out$legacy_annex))
  expect_identical(out$column_classes$demo$year_of_birth$class, 2L)
  expect_identical(out$plan$outputs$labs$options$date_handling$mode, "remove")
})

test_that("explain reports site findings and refuses invalid plans with the typed error", {
  symbol <- .explain_handle()
  a <- .explain_example("plan-v2-example-A.json")
  a$events$extra <- list(table = "specimen")
  out <- omopPlanExplainDS(symbol, a)
  expect_true(any(grepl("specimen", out$site_findings)))
  a$events$extra <- list(table = "specimen", on_missing_table = "unavailable")
  out <- omopPlanExplainDS(symbol, a)
  expect_identical(out$unavailable, "extra")
  a$outputs$baseline$roster <- TRUE
  err <- tryCatch(omopPlanExplainDS(symbol, a), dsomop_plan_rejection = function(e) e)
  expect_s3_class(err, "dsomop_ir_validation_error")
  # an encoded (B64 JSON) plan is decoded like any DataSHIELD argument
  enc <- paste0("B64:", gsub("=+$|[\r\n]", "", chartr("+/", "-_", jsonlite::base64_enc(
    charToRaw(as.character(jsonlite::toJSON(.explain_example("plan-v2-example-B.json"),
                                            auto_unbox = TRUE, null = "null", digits = NA)))))))
  out <- omopPlanExplainDS(symbol, enc)
  expect_identical(out$plan$populations$cc$kind, "match")
})

test_that("site resources: unavailable_table, unavailable sources and non-event tables", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  a <- .explain_example("plan-v2-example-A.json")
  # the unit-test CDM has no drug_exposure.days_supply: drop the supply
  # fallback, a plan finding that would precede the site resource refusal
  a$events$metf_rx$end <- NULL
  # hba1c is read only by the baseline output; specimen is an event source
  # this unit-test CDM does not have
  a$events$hba1c$table <- "specimen"
  expect_identical(.roleAvailability(handle, "specimen"), "absent")
  err <- tryCatch(.irPrepare(a, handle), dsomop_plan_rejection = function(e) e)
  expect_s3_class(err, "dsomop_ir_validation_error")
  expect_identical(err$code, "unavailable_table")
  expect_match(conditionMessage(err),
               "/events/hba1c/table: table 'specimen' is not available at this site",
               fixed = TRUE)
  a$events$hba1c$on_missing_table <- "unavailable"
  report <- .irValidate(.irPrepare(a), handle)
  expect_identical(report$unavailable, "hba1c")
  expect_identical(report$unavailable_outputs, "baseline")
  # a table that is never an event source is a plan error under either mode
  a$events$hba1c$table <- "provider"
  expect_identical(.roleAvailability(handle, "provider"), "not_event_source")
  err <- tryCatch(.irPrepare(a, handle), dsomop_plan_rejection = function(e) e)
  expect_identical(err$code, "validation_error")
  expect_match(conditionMessage(err), "'provider' is not an event table")
  expect_identical(.roleAvailability(handle, "measurement"), "available")
})

test_that("explain is listed as a reviewed aggregate method", {
  root <- testthat::test_path("..", "..")
  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  expect_true(grepl("omopPlanExplainDS", desc[1, "AggregateMethods"], fixed = TRUE))
  ns <- readLines(file.path(root, "NAMESPACE"))
  expect_true("export(omopPlanExplainDS)" %in% ns)
})

test_that("the capability block declares the language (A-E7)", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  caps <- .getCapabilities(handle)
  rl <- caps$recipe_language
  expect_identical(rl$version, 2L)
  expect_identical(rl$semantic_version, "2.0.0")
  expect_identical(rl$schema_id, "https://isglobal-brge.github.io/dsOMOP/schema/plan-v2.json")
  expect_true(is.character(rl$operations))
  expect_true(all(rl$operations %in% .RECIPE_OPERATIONS))
  expect_identical(length(.RECIPE_OPERATIONS), 43L)
  expect_identical(length(.RECIPE_REDUCERS), 35L)
  reducers_csv <- utils::read.csv(system.file("contracts", "reducers.csv", package = "dsOMOP"),
                                  stringsAsFactors = FALSE)
  expect_setequal(.RECIPE_REDUCERS, reducers_csv$fn)
  expect_identical(rl$dialect, .sqlDialectCapability(handle))
  expect_identical(rl$dialect$family, "sqlite")
  # every operation the language names has a construct mapping in R/sql_expr.R
  expect_identical(.sqlPlanConstructs(.RECIPE_OPERATIONS),
                   .sqlPlanConstructs(names(.SQL_OPERATION_CONSTRUCTS)))
  expect_setequal(names(.SQL_OPERATION_CONSTRUCTS), .RECIPE_OPERATIONS)
})

test_that("the dialect preflight names the output that needs a missing construct", {
  a <- .irPrepare(.explain_example("plan-v2-example-A.json"))
  # SQL Server cannot keep a roster across statements: the first output in
  # evaluation order is named
  err <- tryCatch(.irDialectPreflight(a, "sql server"),
                  dsomop_dialect_unsupported = function(e) e)
  expect_s3_class(err, "dsomop_plan_rejection")
  expect_identical(err$construct, "session_table")
  expect_identical(err$output, .irOrder(a)$outputs[[1L]])
  expect_identical(err$family, "sqlserver")
  expect_s3_class(.irDialectPreflight(a, "postgresql"), "dsomop_sql_dialect")
  # a construct only the second output's dependencies need names that output
  # (first_line's columns read the episode_course interval, which needs window
  # frames; the episodes output reads no interval)
  acc <- system.file("plans", "acceptance", "14-oncology-episodes", "plan.json",
                     package = "dsOMOP")
  if (!nzchar(acc)) {
    acc <- testthat::test_path("..", "..", "inst", "plans", "acceptance",
                               "14-oncology-episodes", "plan.json")
  }
  p14 <- .irPrepare(jsonlite::fromJSON(acc, simplifyVector = FALSE))
  expect_identical(.irOrder(p14)$outputs, c("episodes", "first_line"))
  testthat::local_mocked_bindings(
    .sqlHelperAvailable = function(h, construct) construct != "window_frames")
  err <- tryCatch(.irDialectPreflight(p14, "postgresql"),
                  dsomop_dialect_unsupported = function(e) e)
  expect_identical(err$output, "first_line")
  expect_identical(err$construct, "window_frames")
  expect_match(conditionMessage(err), "output 'first_line' needs")
  # the whole-plan check refuses what no output's closure accounts for
  testthat::local_mocked_bindings(
    .sqlHelperAvailable = function(h, construct) construct != "session_table",
    .irOutputClosure = function(ir, o) {
      ir$populations <- .pnv2EmptyObject()
      ir
    })
  err <- tryCatch(.irDialectPreflight(a, "postgresql"),
                  dsomop_dialect_unsupported = function(e) e)
  expect_identical(err$construct, "session_table")
  expect_null(err$output)
  # a lowered v1 plan needs exactly what the v1 executors need: session
  # tables for its cohort (refused on SQL Server), nothing for a plan over the
  # implicit all-persons base (review B5)
  v1 <- .irPrepare(list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
                        outputs = list(demo = list(type = "baseline",
                                                   columns = list("gender_concept_id")))))
  err <- tryCatch(.irDialectPreflight(v1, "sql server"), dsomop_dialect_unsupported = function(e) e)
  expect_identical(err$construct, "session_table")
  plain <- .irPrepare(list(outputs = list(p = list(type = "person_level",
                                                   tables = list(person = list("gender_concept_id"))))))
  expect_s3_class(.irDialectPreflight(plain, "sql server"), "dsomop_sql_dialect")
})

test_that("explain reports the dialect preflight outcome with the output", {
  symbol <- .explain_handle()
  out <- omopPlanExplainDS(symbol, .explain_example("plan-v2-example-A.json"))
  expect_identical(out$dialect$status, "ok")
  expect_identical(out$dialect$family, "sqlite")
  testthat::local_mocked_bindings(
    .sqlHelperAvailable = function(h, construct) construct != "window_frames")
  out <- omopPlanExplainDS(symbol, .explain_example("plan-v2-example-A.json"))
  expect_identical(out$dialect$status, "dialect_unsupported")
  expect_identical(out$dialect$output, "baseline")
  expect_identical(out$dialect$construct, "window_frames")
})

test_that("operations are advertised only when their compilers are installed", {
  testthat::local_mocked_bindings(.recipeFunctionInstalled = function(name) TRUE)
  expect_identical(.recipeAvailableOperations(), .RECIPE_OPERATIONS)
  expect_identical(.recipeLanguageCapability(NULL)$reducers, .RECIPE_REDUCERS)
  withr::local_options(list(dsomop.recipe_operations_disabled = c("population.match", "verb.mutate")))
  ops <- .recipeAvailableOperations()
  expect_false(any(c("population.match", "verb.mutate") %in% ops))
  testthat::local_mocked_bindings(.recipeFunctionInstalled = function(name) name != ".compileMatch")
  withr::local_options(list(dsomop.recipe_operations_disabled = character(0)))
  expect_false("population.match" %in% .recipeAvailableOperations())
  expect_true("population.index" %in% .recipeAvailableOperations())
  testthat::local_mocked_bindings(.recipeFunctionInstalled = function(name) FALSE)
  expect_identical(.recipeAvailableOperations(), character(0))
  expect_identical(.recipeLanguageCapability(NULL)$reducers, character(0))
})

test_that("a v2 plan is validated by execution but not executed yet", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  b <- .explain_example("plan-v2-example-B.json")
  # the unit test CDM has no drug_exposure.days_supply: a supply fallback is a
  # site finding there (shown by the next block of this test)
  b$events$statin_rx$end <- NULL
  err <- tryCatch(.planExecute(handle, b, list()), dsomop_plan_rejection = function(e) e)
  expect_s3_class(err, "dsomop_ir_validation_error")
  expect_match(conditionMessage(err), "does not execute them yet")
  bad <- .explain_example("plan-v2-example-B.json")
  bad$outputs$cc_table$lineage <- "forged"
  err <- tryCatch(.planExecute(handle, bad, list()), dsomop_plan_rejection = function(e) e)
  expect_match(conditionMessage(err), "plan-v2.json")
  err <- tryCatch(.planExecute(handle, .explain_example("plan-v2-example-B.json"), list()),
                  dsomop_plan_rejection = function(e) e)
  expect_match(conditionMessage(err), "statin_rx/end/fallback.*days_supply")
})
