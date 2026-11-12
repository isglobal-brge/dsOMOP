# WS-2 cross-review regressions (design/2.10.3/astra-ws2-review.md): each
# block reproduces one of Astra's probes (design/2.10.3/astra-scratch/ws2-*)
# with the contract it requires, plus controls that must keep passing.

.rv_example <- function(name) {
  path <- system.file("schema", "examples", name, package = "dsOMOP")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "schema", "examples", name)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}
.rv_refused <- function(expr) {
  tryCatch({ force(expr); NULL }, dsomop_plan_rejection = function(e) e)
}
.rv_v1 <- function() list(outputs = list(p = list(type = "person_level",
                                                   tables = list(person = list("gender_concept_id")))))
.rv_v2 <- function() list(version = 2L, meta = list(language_version = "2.0", semantic_version = "2.0.0",
                                                    generated_by = "dsOMOPClient"),
                          populations = list(p = list(kind = "criteria")),
                          outputs = list(o = list(rows = "p", format = "wide")))
# Example A without the supply fallback (the unit-test CDM has no days_supply).
.rv_plan_a <- function() {
  x <- .rv_example("plan-v2-example-A.json")
  x$events$metf_rx$end <- NULL
  x
}

# --- B1: the v1 envelope is closed before normalisation ------------------------

test_that("B1: unknown v1 members are refused at every nesting point, null-valued included", {
  forged <- list(
    root_null = { p <- .rv_v1(); p["private"] <- list(NULL); p },
    output_options = { p <- .rv_v1(); p$outputs$p$options <- list(private = TRUE); p },
    tables_frames = { p <- .rv_v1(); p$scope <- list(tables_frames = list(list(person_id = list(1L)))); p },
    visit = list(outputs = list(p = list(type = "person_level", tables = list(measurement = list(
      features = list(list(name = "n", type = "count", concept_set = list(1L))),
      visit = list(concept_ids = list(1L), private = TRUE)))))),
    outcome_concept_set = list(
      cohort = list(type = "cohort_table", cohort_definition_id = 1L),
      outputs = list(s = list(type = "survival",
                              outcome = list(table = "condition_occurrence",
                                             concept_set = list(concepts = list(1L), lineage = "forged")),
                              tar = list(start_offset = 0L, end_offset = 30L)))),
    value_bin_operand = list(outputs = list(e = list(type = "event_level", table = "measurement",
      filters = list(custom = list(var = "value_as_number", op = "value_bin",
                                   value = list(lower = 1, upper = 2, private = TRUE)))))),
    output_null = { p <- .rv_v1(); p$outputs$p["already_gated"] <- list(NULL); p },
    filter_null = list(outputs = list(e = list(type = "event_level", table = "measurement",
      filters = list(custom = list(var = "value_as_number", op = ">", value = 1, gate = NULL))))),
    cohort_null = list(cohort = list(type = "cohort_table", cohort_definition_id = 1L, lineage = NULL),
                       outputs = list(m = list(type = "cohort_membership"))),
    population_null = list(populations = list(base = list(kind = "criteria", filter_tree = NULL,
                                                          roster = NULL)),
                           outputs = list(m = list(type = "cohort_membership")))
  )
  for (nm in names(forged)) {
    err <- .rv_refused(.irPrepare(forged[[nm]]))
    expect_s3_class(err, "dsomop_ir_validation_error")
    expect_match(conditionMessage(err), "closed v1 envelope", info = nm)
  }
  # controls: known members may be null (v1: null is absent)
  ok <- .rv_v1()
  ok$outputs$p["population_id"] <- list(NULL)
  ok["options"] <- list(NULL)
  expect_s3_class(.irPrepare(ok), "dsomop_ir")
})

test_that("B1: the corrected legacy value validators run before lowering", {
  p <- list(outputs = list(e = list(type = "event_level", table = "measurement",
                                    temporal = list(event_select = list(order = "first", n = 0L)))))
  expect_error(.validateTemporalSpec(p$outputs$e$temporal), "positive integer")
  err <- .rv_refused(.irPrepare(p))
  expect_s3_class(err, "dsomop_ir_validation_error")
  expect_match(conditionMessage(err), "/outputs/e/temporal", fixed = TRUE)
  bad_op <- list(outputs = list(e = list(type = "event_level", table = "measurement",
    filters = list(custom = list(var = "value_as_number", op = "like", value = 1)))))
  expect_s3_class(.rv_refused(.irPrepare(bad_op)), "dsomop_ir_validation_error")
  bad_bin <- list(outputs = list(e = list(type = "event_level", table = "measurement",
    filters = list(custom = list(var = "value_as_number", op = "value_bin",
                                 value = list(lower = 2, upper = 1))))))
  expect_s3_class(.rv_refused(.irPrepare(bad_bin)), "dsomop_ir_validation_error")
})

test_that("B1: workspace frames never travel on the wire; resolved frames go through the context", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  # server-injected frames are stripped before validation and counted
  exec <- .rv_v1()
  exec$scope <- list(tables_frames = list(data.frame(person_id = 1:3)), combine = "union")
  wire <- .planWirePart(exec)
  expect_null(wire$plan$scope$tables_frames)
  expect_identical(wire$scope_tables, 1L)
  ir <- .irPrepare(wire$plan, scope_tables = wire$scope_tables)
  expect_identical(unlist(ir$scope$tables), "scope_table_1")
})

test_that("A-10: a v1 cohort_definition_id of 0 is refused with the field named", {
  for (p in list(
    list(cohort = list(type = "cohort_table", cohort_definition_id = 0L),
         outputs = list(m = list(type = "cohort_membership"))),
    list(populations = list(base = list(kind = "criteria", cohort_definition_id = 0L)),
         outputs = list(m = list(type = "cohort_membership"))))) {
    err <- .rv_refused(.irPrepare(p))
    expect_s3_class(err, "dsomop_ir_validation_error")
    expect_match(conditionMessage(err), "cohort_definition_id")
  }
})

# --- B4: site checks type every predicate against its source -------------------

test_that("B4: column types, roles and predicate sources are checked at the site", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  edits <- list(
    concept_column = function(x) { x$events$hba1c$concept_col <- "measurement_date"; x },
    value_column = function(x) { x$events$hba1c$value$column <- "measurement_date"; x },
    concept_predicate = function(x) { x$events$hba1c$where <- list(var = "measurement_concept_id", op = "==", value = "not_an_integer"); x },
    date_predicate = function(x) { x$events$hba1c$where <- list(var = "measurement_date", op = "==", value = 7L); x },
    missing_role = function(x) { x$events$t2dm_dx$where <- list(var = "value", op = ">", value = 1L); x },
    person_nonexistent = function(x) { x$populations$new_users$where <- list(person = list(var = "made_up", op = "==", value = 1L)); x },
    person_blocked = function(x) { x$populations$new_users$where <- list(person = list(var = "person_source_value", op = "==", value = "private")); x },
    numeric_literal = function(x) { x$events$hba1c$where <- list(var = "value_as_number", op = ">", value = "bad"); x },
    reducer_column = function(x) {
      x$variables$bad <- list(source = list(event = "hba1c"),
                              reducer = list(fn = "any", predicate = list(var = "made_up", op = ">", value = 1)))
      x$outputs$baseline$columns <- c(x$outputs$baseline$columns, list("bad"))
      x
    })
  for (nm in names(edits)) {
    err <- .rv_refused(.irPrepare(edits[[nm]](.rv_plan_a()), h))
    expect_true(inherits(err, "dsomop_plan_rejection"), info = nm)
  }
  # controls: the unedited plan and well-typed predicates pass
  expect_s3_class(.irPrepare(.rv_plan_a(), h), "dsomop_ir")
  ok <- .rv_plan_a()
  ok$events$hba1c$where <- list(and = list(list(var = "value_as_number", op = ">", value = 5.5),
                                           list(var = "measurement_date", op = ">=", value = "2010-01-01")))
  ok$populations$new_users$where <- list(and = list(ok$populations$new_users$where,
    list(person = list(var = "gender_concept_id", op = "in", value = list(8507L, 8532L)))))
  expect_s3_class(.irPrepare(ok, h), "dsomop_ir")
})

# --- B5: a lowered v1 plan passes the site and dialect stages ------------------

test_that("B5: v1 plans are refused for absent sources and session-table dialects", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  x <- list(outputs = list(x = list(type = "event_level", table = "specimen",
                                    representation = list(format = "long"))))
  err <- .rv_refused(.irPrepare(x, h))
  expect_s3_class(err, "dsomop_plan_rejection")
  expect_identical(err$code, "unavailable_table")
  idx <- list(populations = list(study = list(id = "study", index_event = list(table = "condition_occurrence"))),
              outputs = list(x = list(type = "person_level", population_id = "study",
                                      tables = list(person = list("gender_concept_id")))))
  ir <- .irPrepare(idx, h)
  expect_true(.irV1NeedsSessionTables(ir))
  err <- tryCatch(.irDialectPreflight(ir, "sql server"), dsomop_dialect_unsupported = function(e) e)
  expect_s3_class(err, "dsomop_dialect_unsupported")
  expect_identical(err$construct, "session_table")
  # a v1 plan over the implicit all-persons base reads its tables directly
  plain <- .irPrepare(.rv_v1(), h)
  expect_false(.irV1NeedsSessionTables(plain))
  expect_s3_class(.irDialectPreflight(plain, "sql server"), "dsomop_sql_dialect")
  withr::local_options(list(dsomop.recipe_operations_disabled = "event"))
  expect_s3_class(.rv_refused(.irPrepare(idx, h)), "dsomop_plan_rejection")
})

test_that("B5: the v1 site requirements of the executors are checked before execution", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  # survival censoring on death needs the death table: the test CDM has one,
  # an absent outcome table does not pass
  p <- list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
            outputs = list(s = list(type = "survival",
                                    outcome = list(table = "specimen", concept_set = list(1L)),
                                    tar = list(start_offset = 0L, end_offset = 30L))))
  err <- .rv_refused(.irPrepare(p, h))
  expect_s3_class(err, "dsomop_plan_rejection")
  expect_identical(err$code, "unavailable_table")
})

# --- B6: criterion-specific union coverage ------------------------------------

test_that("B6: an observed() gap requests union coverage and its window frames", {
  x <- .rv_plan_a()
  x$intervals <- NULL
  x$events <- list(hba1c = x$events$hba1c)
  x$populations$new_users$index$source <- list(event = "hba1c")
  x$populations$new_users$where <- list(observed = list(before = 365L, gap = 7L))
  x$variables <- list(sex = x$variables$sex)
  x$outputs <- list(baseline = x$outputs$baseline)
  x$outputs$baseline$columns <- list("sex")
  x$publish <- NULL
  ir <- .irPrepare(x)
  expect_true("population.coverage.union" %in% .irOperations(ir))
  expect_true("population.coverage.union" %in% .irOperations(.irOutputClosure(ir, "baseline")))
  testthat::local_mocked_bindings(.sqlHelperAvailable = function(h, construct) construct != "window_frames")
  err <- tryCatch(.irDialectPreflight(ir, "postgresql"), dsomop_dialect_unsupported = function(e) e)
  expect_s3_class(err, "dsomop_dialect_unsupported")
  expect_identical(err$output, "baseline")
  # an explicit gap of 0 is a request too; no gap is none
  x$populations$new_users$where <- list(observed = list(before = 365L, gap = 0L))
  expect_true("population.coverage.union" %in% .irOperations(.irPrepare(x)))
  x$populations$new_users$where <- list(observed = list(before = 365L))
  expect_false("population.coverage.union" %in% .irOperations(.irPrepare(x)))
})

# --- B7: caps cover every predicate position, namespace-qualified ---------------

test_that("B7: predicate caps count embedded, reducer and same-named predicates", {
  withr::local_options(dsomop.max_filter_values = 1L)
  p <- .rv_v2(); p$populations$p$where <- list(person = list(var = "gender_concept_id", op = "in", value = list(1L, 2L)))
  q <- .rv_v2(); q$events <- list(e = list(table = "measurement"))
  q$variables <- list(v = list(source = list(event = "e"),
                               reducer = list(fn = "any", predicate = list(var = "value", op = "in", value = list(1L, 2L)))))
  r <- .rv_v2(); r$events <- list(p = list(table = "measurement", where = list(var = "value", op = "in", value = list(1L, 2L))))
  for (x in list(p, q, r)) {
    expect_true(.jsonSchemaValidate(.irSchema("plan-v2"), x)$valid)
    err <- .rv_refused(.irPrepare(x))
    expect_s3_class(err, "dsomop_ir_validation_error")
    expect_match(conditionMessage(err), "predicate values exceed")
  }
  # the event and the population of one name are two positions
  expect_match(conditionMessage(.rv_refused(.irPrepare(r))), "/events/p/where", fixed = TRUE)
  withr::local_options(dsomop.max_filter_values = 2L)
  for (x in list(p, q, r)) expect_s3_class(.irPrepare(x), "dsomop_ir")
})

# --- B8: evaluation order is topological ----------------------------------------

test_that("B8: a dictionary over a joined output comes after it", {
  b <- .rv_example("plan-v2-example-B.json")
  b$outputs$extra <- list(rows = "cc", format = "wide", columns = list("age"))
  b$outputs$joined <- list(rows = "cc", format = "joined",
                           options = list(from = list(list(output = "cc_table"), list(output = "extra"))))
  b$outputs$dict <- list(rows = "cc", format = "dictionary", options = list(source_outputs = list("joined")))
  ord <- .irOrder(.irPrepare(b))$outputs
  expect_lt(match("joined", ord), match("dict", ord))
  expect_lt(match("cc_table", ord), match("joined", ord))
  expect_lt(match("extra", ord), match("joined", ord))
  # nested joins keep their dependencies; a joined output still waits for
  # ready non-joined outputs (tie-breaker only)
  b$outputs$joined2 <- list(rows = "cc", format = "joined",
                            options = list(from = list(list(output = "joined"), list(output = "extra"))))
  b$outputs$zzz <- list(rows = "cc", format = "wide", columns = list("age"))
  ord <- .irOrder(.irPrepare(b))$outputs
  expect_lt(match("joined", ord), match("joined2", ord))
  expect_lt(match("zzz", ord), match("joined", ord))
})

# --- B9: index-relative criteria in a matching controls template ---------------

test_that("B9: an age-at-case-time criterion is valid in a controls template", {
  b <- .rv_example("plan-v2-example-B.json")
  b$populations$eligible$where <- list(and = list(list(observed = list(before = 365L)),
                                                  list(age = list(min = 18L, reference = "index"))))
  ir <- .irPrepare(b)
  expect_s3_class(ir, "dsomop_ir")
  expect_true("criteria.age" %in% .irOperations(ir))
  # the same declaration also used as an ordinary roster is still refused there
  b$outputs$eligible_table <- list(rows = "eligible", format = "wide", columns = list("sex"))
  err <- .rv_refused(.irPrepare(b))
  expect_match(conditionMessage(err), "index-referenced age criterion needs an indexed population")
})

# --- A-11: one shared normaliser and indexedness rule ---------------------------

test_that("A-11: intersect/setdiff take their first member's indexedness", {
  b <- .rv_example("plan-v2-example-B.json")
  b$populations$mixed <- list(kind = "setop", setop = list(op = "intersect", members = list("eligible", "cases")))
  b$outputs$people <- list(rows = "mixed", format = "wide", columns = list("age"))
  ir <- .irPrepare(b)
  expect_identical(ir$outputs$people$grain, "person")
  b$populations$mixed$setop$op <- "union"
  expect_identical(.irPrepare(b)$outputs$people$grain, "episode")
})

# --- N1: explain classes of projected reducers ----------------------------------

test_that("N1: date-preserving reducers are reported as class 2", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  x <- .rv_plan_a()
  x$variables$first_lab <- list(source = list(event = "hba1c"), reducer = list(fn = "first_date"))
  x$variables$last_lab <- list(source = list(event = "hba1c"), reducer = list(fn = "last_date"))
  x$variables$n_lab <- list(source = list(event = "hba1c"), reducer = list(fn = "count"))
  x$outputs$baseline$columns <- c(x$outputs$baseline$columns, list("first_lab", "last_lab", "n_lab"))
  ir <- .irPrepare(x, h)
  classes <- .irExplain(ir, h)$column_classes$baseline
  expect_identical(classes$first_lab$class, 2L)
  expect_identical(classes$last_lab$class, 2L)
  expect_identical(classes$n_lab$class, 3L)
})

# --- control: explain discloses nothing about patients --------------------------

test_that("control: explain is invariant under patient data and runs no execution query", {
  h1 <- create_test_handle(1L)
  h2 <- create_test_handle(22L)
  on.exit({ cleanup_handle(h1); cleanup_handle(h2) })
  .setHandle("rv_explain_1", h1)
  .setHandle("rv_explain_2", h2)
  on.exit({ .removeHandle("rv_explain_1"); .removeHandle("rv_explain_2") }, add = TRUE)
  testthat::local_mocked_bindings(.executeQuery = function(...) stop("PATIENT_EXECUTION_QUERY"))
  expect_identical(omopPlanExplainDS("rv_explain_1", .rv_plan_a()),
                   omopPlanExplainDS("rv_explain_2", .rv_plan_a()))
})

# --- B2: the legacy-spec occurrence contract ---------------------------------------

test_that("B2: the {type, count} occurrence lowers and the adapter reproduces direct rows", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  id <- DBI::dbGetQuery(handle$conn, "SELECT condition_concept_id AS id FROM condition_occurrence LIMIT 1")$id
  rows <- function(table) {
    DBI::dbGetQuery(handle$conn, paste0("SELECT subject_id, cohort_start_date, cohort_end_date FROM ",
                                        table, " ORDER BY subject_id, cohort_start_date, cohort_end_date"))
  }
  for (occ in list(list(type = "at_least", count = 1L), list(type = "at_most", count = 0L),
                   list(type = "exactly", count = 0L), list(type = "exactly", count = 1L), NULL)) {
    crit <- list(table = "condition_occurrence", concept_set = id,
                 temporal = list(index_window = list(start = 0L, end = 0L)))
    if (!is.null(occ)) crit$occurrence <- occ
    spec <- list(type = "condition", concept_set = id, inclusion_criteria = list(same = crit))
    plan <- list(cohort = list(type = "spec", spec = spec), outputs = list(m = list(type = "cohort_membership")))
    ir <- .irPrepare(plan)
    lowered <- ir$populations$base$index$source$legacy_spec
    expect_identical(lowered$inclusion_criteria$same$occurrence,
                     occ %||% list(type = "at_least", count = 1L))
    direct <- withr::with_options(list(nfilter.subset = 0L), .cohortCreate(handle, spec, mode = "temporary"))
    want <- rows(direct)
    .dropTempTable(handle, direct)
    fr <- withr::with_options(list(nfilter.subset = 0L), .adaptLegacySpec(handle, lowered))
    got <- DBI::dbGetQuery(handle$conn, paste0(
      "SELECT subject_id, cohort_start_date, cohort_end_date FROM (", fr$select_body,
      ") q ORDER BY subject_id, cohort_start_date, cohort_end_date"))
    .dropTempTable(handle, attr(fr, "temp_table"))
    expect_identical(got, want, info = paste(unlist(occ), collapse = " "))
  }
  # the {min, max} form the executor never accepted is refused before lowering
  bad <- list(cohort = list(type = "spec", spec = list(type = "condition", concept_set = id,
    inclusion_criteria = list(same = list(table = "condition_occurrence", occurrence = list(min = 1L))))),
    outputs = list(m = list(type = "cohort_membership")))
  expect_match(conditionMessage(.rv_refused(.irPrepare(bad))), "occurrence: unknown field(s) min", fixed = TRUE)
})

# --- B3: the executed concept restriction is the lowered one ----------------------

test_that("B3: event_level lowering follows the executor's concept-filter precedence", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  ids <- DBI::dbGetQuery(handle$conn, "SELECT DISTINCT condition_concept_id AS id FROM condition_occurrence ORDER BY condition_concept_id")$id
  expect_gte(length(ids), 2L)
  p <- list(outputs = list(e = list(type = "event_level", table = "condition_occurrence",
              concept_set = ids[[1]], filters = list(concept_set = list(ids = ids[[2]])),
              representation = list(format = "long"))),
            options = list(translate_concepts = FALSE, factor_concepts = FALSE))
  ir <- .irPrepare(p)
  lowered <- unlist(ir$events[[ir$outputs$e$sources[[1]]$event]]$concept_set$concepts)
  res <- withr::with_options(list(nfilter.subset = 0L), .planExecute(handle, p, list(e = "rv_e")))
  expect_gt(nrow(res$e), 0L)
  expect_true(all(res$e$condition_concept_id == ids[[2]]))
  expect_identical(lowered, as.integer(ids[[2]]))
  digest <- function(x) .planSemanticDigest(.irDocument(.irPrepare(x)))$sha256
  # changing the executed restriction changes the digest ...
  q <- p
  q$outputs$e$filters$concept_set$ids <- ids[[1]]
  expect_false(identical(digest(p), digest(q)))
  # ... changing the overridden spelling does not (it is unread: annex)
  r <- p
  r$outputs$e$concept_set <- ids[[2]]
  expect_identical(digest(p), digest(r))
})

# --- B11: a one-member v1 set operation is its member ----------------------------

test_that("B11: a singleton union lowers to its member and returns the member's rows", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  bp <- .buildBlueprint(handle)
  p <- list(populations = list(
    base = list(kind = "criteria", cohort_definition_id = 1L),
    alias = list(kind = "setop", setop = list(op = "union", members = list("base")))),
    outputs = list(on_alias = list(type = "person_level", population_id = "alias",
                                   tables = list(person = list("gender_concept_id"))),
                   on_base = list(type = "person_level", population_id = "base",
                                  tables = list(person = list("gender_concept_id")))))
  pop <- withr::with_options(list(nfilter.subset = 0L), .planResolvePopulations(handle, p, bp))
  expect_identical(pop$alias$cohort_table, pop$base$cohort_table)
  ir <- .irPrepare(p)
  expect_identical(ir$populations$alias[c("kind", "parent")], list(kind = "criteria", parent = "base"))
  expect_identical(ir$outputs$on_alias$rows, "alias")
  # direct (the member) versus lowered (the singleton): identical rows
  res <- withr::with_options(list(nfilter.subset = 0L),
                             .planExecute(handle, p, list(on_alias = "rv_alias", on_base = "rv_base")))
  expect_gt(nrow(res$on_alias), 0L)
  expect_identical(res$on_alias, res$on_base)
  # n-ary differences lower to the v1 left fold; a self-difference is refused
  p3 <- list(populations = list(
    a = list(kind = "criteria", cohort_definition_id = 1L),
    b = list(kind = "criteria", filter_tree = list(type = "sex", params = list(value = "F"))),
    c = list(kind = "criteria", filter_tree = list(type = "sex", params = list(value = "M"))),
    d = list(kind = "setop", setop = list(op = "difference", members = list("a", "b", "c")))),
    outputs = list(x = list(type = "person_level", population_id = "d", tables = list(person = list("gender_concept_id")))))
  ir3 <- .irPrepare(p3)
  fold <- ir3$populations$d$setop$members[[1]]
  expect_identical(ir3$populations[[fold]]$setop, list(op = "setdiff", members = list("a", "b")))
  expect_identical(unlist(ir3$populations$d$setop$members), c(fold, "c"))
  p3$populations$d$setop$members <- list("a", "a")
  expect_match(conditionMessage(.rv_refused(.irPrepare(p3))), "difference of a population with itself")
})

# --- the multi-population base is every person (Corrected) ------------------------

test_that("a multi-population base without criteria is every person, as in single mode", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  single <- list(outputs = list(p = list(type = "person_level", tables = list(person = list("gender_concept_id")))))
  multi <- single
  multi$populations <- list(base = list(id = "base", kind = "criteria"),
                            women = list(kind = "criteria", filter_tree = list(type = "sex", params = list(value = "F"))))
  a <- withr::with_options(list(nfilter.subset = 0L), .planExecute(handle, single, list(p = "rv_single")))$p
  b <- withr::with_options(list(nfilter.subset = 0L), .planExecute(handle, multi, list(p = "rv_multi")))$p
  expect_gt(nrow(a), 0L)
  expect_identical(b, a)
})

# --- a lowered v1 plan meets its executors' site rules first (B4/B5) ------------

test_that("v1 plans meet the site rules of the v1 executors, with their messages", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  .buildBlueprint(h)
  baseline <- function(columns) list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
                                     outputs = list(d = list(type = "baseline", columns = columns)))
  err <- .rv_refused(.irPrepare(baseline(list("not_a_person_column")), h))
  expect_identical(err$code, "validation_error")
  expect_match(conditionMessage(err), "/outputs/d/columns: Baseline person column(s) not found: not_a_person_column.",
               fixed = TRUE)
  expect_s3_class(.irPrepare(baseline(list("gender_concept_id", "day_of_birth")), h), "dsomop_ir")
  # a class-1 person column (the default fixture has none)
  DBI::dbExecute(h$conn, "ALTER TABLE person ADD COLUMN person_source_value TEXT")
  h$blueprint <- NULL
  .buildBlueprint(h)
  err <- .rv_refused(.irPrepare(baseline(list("person_source_value")), h))
  expect_match(conditionMessage(err), "Disclosive: baseline person column(s) 'person_source_value' are blocked.",
               fixed = TRUE)
  # a custom filter on an identifier: the executor's own refusal, before the IR's
  surv <- list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
               outputs = list(s = list(type = "survival",
                                       outcome = list(table = "condition_occurrence", concept_set = list(4000002L)),
                                       tar = list(start_offset = 0L, end_offset = 730L),
                                       filters = list(custom = list(var = "person_id", op = "in", value = list(1L, 5L))))))
  err <- .rv_refused(.irPrepare(surv, h))
  expect_match(conditionMessage(err), "filter on column 'person_id' is not permitted", fixed = TRUE)
  # a plan-declaration error keeps the executor's untyped error (never a
  # transported refusal) and .planValidate() reports it
  nocohort <- list(outputs = list(d = list(type = "baseline")))
  err <- tryCatch(.irPrepare(nocohort, h), error = function(e) e)
  expect_s3_class(err, "dsomop_v1_declaration_error")
  expect_false(inherits(err, "dsomop_plan_rejection"))
  expect_match(conditionMessage(err), "requires a cohort")
  v <- .planValidate(h, nocohort)
  expect_false(v$valid)
  expect_identical(sum(grepl("requires a cohort", v$errors)), 1L)
  # explain lists the executors' findings first
  ex <- .irExplain(.irNormalize(baseline(list("not_a_person_column"))), h,
                   v1_plan = baseline(list("not_a_person_column")))
  expect_match(ex$site_findings[[1L]], "Baseline person column(s) not found", fixed = TRUE)
})

test_that("an event over a present table without an event role is a v1 row source only", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  expect_identical(.roleAvailability(h, "person"), "not_event_source")
  v1 <- list(outputs = list(people = list(type = "event_level", table = "person",
                                          columns = list("person_id", "gender_concept_id"),
                                          representation = list(format = "long"))))
  ir <- .irPrepare(v1, h)
  expect_identical(ir$events[[1L]]$table, "person")
  # ... but never a native event table
  x <- .rv_plan_a()
  x$events$hba1c$table <- "person"
  x$events$hba1c$concept_set <- NULL
  err <- .rv_refused(.irPrepare(x, h))
  expect_identical(err$code, "validation_error")
  expect_match(conditionMessage(err), "'person' is not an event table")
  # and an absent v1 table is still the typed unavailable_table refusal
  v1$outputs$people$table <- "specimen"
  expect_identical(.rv_refused(.irPrepare(v1, h))$code, "unavailable_table")
})

test_that("native sources meet the compilers' own metadata rules (WS-4 .eventSiteCheck)", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  x <- .rv_plan_a()
  # a role-bound physical column is its role's alias; a datetime is not a predicate column
  x$events$hba1c$where <- list(var = "value_as_number", op = ">", value = 5)
  expect_s3_class(.irPrepare(x, h), "dsomop_ir")
  # the compiler's own value-column rule: an integer concept column is no value
  x$events$hba1c$value$column <- "measurement_type_concept_id"
  err <- .rv_refused(.irPrepare(x, h))
  expect_identical(err$code, "validation_error")
  expect_match(conditionMessage(err), "/events/hba1c/value/column: 'measurement_type_concept_id' is not a filterable numeric column",
               fixed = TRUE)
  # person predicates use the compiler's catalogue of the person table
  x <- .rv_plan_a()
  x$populations$new_users$where <- list(and = list(x$populations$new_users$where,
    list(person = list(var = "year_of_birth", op = ">=", value = "1980"))))
  err <- .rv_refused(.irPrepare(x, h))
  expect_match(conditionMessage(err), "/populations/new_users/where/and/1/person")
  x$populations$new_users$where$and[[2]]$person$value <- 1980L
  expect_s3_class(.irPrepare(x, h), "dsomop_ir")
})

# --- the execution context carries the validated IR (WS-1) ----------------------

test_that("the context of a language 2.0 plan carries its validated IR, a v1 plan's its plan", {
  v1 <- .rv_v1()
  ir1 <- .irPrepare(v1)
  expect_identical(.planContextIr(list(ir = ir1, wire = .irJsonForm(v1)), v1), v1)
  v2 <- .rv_v2()
  ir2 <- .irPrepare(v2)
  got <- .planContextIr(list(ir = ir2, wire = .irJsonForm(v2)), v2)
  expect_s3_class(got, "dsomop_ir")
  expect_true(.dsomopDpIsV2(got))
  expect_identical(got$outputs$o$options, ir2$outputs$o$options)
  expect_identical(.planContextIr(list(error = simpleError("refused")), v2), v2)
  # omopPlanExecuteDS creates the context after validation, with that IR
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .setHandle("rv_context_ir", handle)
  on.exit(.removeHandle("rv_context_ir"), add = TRUE)
  seen <- new.env()
  local_mocked_bindings(.planExecute = function(handle, plan, out_symbols, output_mode, ctx) {
    seen$ir <- ctx$ir
    seen$validated <- ctx$validation$ir
    .irReject("stops after the context checks")
  })
  status <- .omopPlanExecuteDS("rv_context_ir", v1, list(p = "A"), target_env = new.env(parent = emptyenv()))
  expect_identical(status$status, "refused")
  expect_identical(seen$ir$outputs, v1$outputs)
  expect_identical(attr(seen$validated, "source_version"), 1L)
})

test_that("A-10: a v1 plan without outputs has no IR and publishes nothing", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  plan <- list(cohort = list(type = "cohort_table", cohort_definition_id = 1L))
  expect_null(.irPrepare(plan, h))
  expect_length(.planExecute(h, plan, list()), 0L)
})

# --- A-15 / A-16: the compatibility variants carry the executed semantics -------
# Direct v1-executor results next to the IR each plan lowers to: a result
# difference is a document (and digest) difference, and the forms the executor
# treats alike share one normal form and one digest.

.rv_run <- function(h, plan, name) {
  withr::with_options(list(nfilter.subset = 0L, dsomop.query_strict = TRUE),
    .planExecute(h, plan, stats::setNames(list(paste0("rv_", name)), name)))[[name]]
}
.rv_digest <- function(plan) .planSemanticDigest(.irDocument(.irPrepare(plan)))[["sha256"]]

test_that("A-15: omitted and explicit default occurrences are one IR; the type is matched exactly", {
  crit <- function(occ = NULL) {
    x <- list(table = "condition_occurrence", concept_set = list(201820L))
    if (!is.null(occ)) x$occurrence <- occ
    list(cohort = list(type = "spec", spec = list(type = "condition", concept_set = list(201820L),
                                                    inclusion_criteria = list(c1 = x))),
         outputs = list(m = list(type = "cohort_membership")))
  }
  expect_identical(.rv_digest(crit()), .rv_digest(crit(list(type = "at_least", count = 1L))))
  expect_identical(.rv_digest(crit()), .rv_digest(crit(list(count = 1L))))
  expect_false(identical(.rv_digest(crit()), .rv_digest(crit(list(type = "at_least", count = 0L)))))
  lowered <- .irPrepare(crit(list(type = "at_most", count = 2L)))$populations$base$index$source$legacy_spec
  expect_identical(lowered$inclusion_criteria$c1$occurrence, list(type = "at_most", count = 2L))
  # the executor's switch() matches the type exactly
  err <- .rv_refused(.irPrepare(crit(list(type = "AT_MOST", count = 1L))))
  expect_match(conditionMessage(err), "Unknown occurrence type: 'AT_MOST'", fixed = TRUE)
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  expect_error(.cohortCreate(h, crit(list(type = "AT_MOST", count = 1L))$cohort$spec, mode = "temporary"),
               "Unknown occurrence type")
})

test_that("A-15: every operator, zero included, gives the executor's rows through the adapter", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  .buildBlueprint(h)
  rows <- function(sql) DBI::dbGetQuery(h$conn, paste0(
    "SELECT subject_id, cohort_start_date, cohort_end_date FROM (", sql,
    ") q ORDER BY subject_id, cohort_start_date, cohort_end_date"))
  sizes <- integer(0)
  for (occ in list(list(type = "at_least", count = 0L), list(type = "at_least", count = 2L),
                   list(type = "at_most", count = 0L), list(type = "at_most", count = 1L),
                   list(type = "exactly", count = 0L), list(type = "exactly", count = 2L))) {
    spec <- list(type = "condition", concept_set = list(201820L),
                 inclusion_criteria = list(any_dx = list(table = "condition_occurrence", occurrence = occ)))
    plan <- list(cohort = list(type = "spec", spec = spec), outputs = list(m = list(type = "cohort_membership")))
    lowered <- .irPrepare(plan)$populations$base$index$source$legacy_spec
    direct <- withr::with_options(list(nfilter.subset = 0L), .cohortCreate(h, spec, mode = "temporary"))
    want <- rows(paste0("SELECT * FROM ", direct))
    .dropTempTable(h, direct)
    fr <- withr::with_options(list(nfilter.subset = 0L), .adaptLegacySpec(h, lowered))
    got <- rows(fr$select_body)
    .dropTempTable(h, attr(fr, "temp_table"))
    expect_identical(got, want, info = paste(unlist(occ), collapse = " "))
    sizes <- c(sizes, nrow(want))
  }
  # the operators select different rows on this fixture (the comparison is not vacuous)
  expect_gt(length(unique(sizes)), 1L)
})

test_that("A-16: an inline spec's id is absent, zero or positive as supplied, through the adapter too", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  spec <- list(type = "condition", concept_set = list(201820L))
  plan <- function(id = NULL) {
    cohort <- list(type = "spec", spec = spec)
    if (!is.null(id)) cohort$cohort_definition_id <- id
    list(cohort = cohort, outputs = list(m = list(type = "cohort_membership")))
  }
  ids <- list(absent = NULL, zero = 0L, five = 5L)
  lowered <- lapply(ids, function(id) .irPrepare(plan(id))$populations$base$index$source$legacy_spec)
  expect_null(lowered$absent$cohort_definition_id)
  expect_identical(lowered$zero$cohort_definition_id, 0L)
  expect_identical(lowered$five$cohort_definition_id, 5L)
  digests <- vapply(ids, function(id) .rv_digest(plan(id)), "")
  expect_identical(anyDuplicated(digests), 0L)
  rows_of <- function(t) DBI::dbGetQuery(h$conn, paste0(
    "SELECT subject_id, cohort_start_date, cohort_end_date FROM ", t,
    " ORDER BY subject_id, cohort_start_date, cohort_end_date"))
  for (nm in names(ids)) {
    id <- ids[[nm]]
    # the id names the session cohort (dsomop_cohort_<id>, 0 when absent);
    # one such cohort at a time, as for the v1 executor
    direct <- withr::with_options(list(nfilter.subset = 0L),
      .cohortCreate(h, spec, mode = "temporary", cohort_id = id))
    want <- list(name = direct, rows = rows_of(direct))
    .dropTempTable(h, direct)
    expect_identical(want$name, paste0("dsomop_cohort_", id %||% 0L), info = nm)
    fr <- withr::with_options(list(nfilter.subset = 0L), .adaptLegacySpec(h, lowered[[nm]]))
    table <- attr(fr, "temp_table")
    expect_identical(table, want$name, info = nm)
    expect_identical(rows_of(table), want$rows, info = nm)
    .dropTempTable(h, table)
  }
  # the executed membership id column is the supplied id; without one the
  # v1 executor fails while running, as in 2.10.2 (the legacy outcome is kept)
  expect_true(all(.rv_run(h, plan(5L), "m")$cohort_definition_id == 5L))
  expect_true(all(.rv_run(h, plan(0L), "m")$cohort_definition_id == 0L))
  expect_error(.rv_run(h, plan(), "m"))
  # a negative id is refused before lowering, as before any query
  expect_s3_class(.rv_refused(.irPrepare(plan(-1L))), "dsomop_ir_validation_error")
  # a results-cohort reference of 0 stays refused (A-10)
  err <- .rv_refused(.irPrepare(list(cohort = list(type = "cohort_table", cohort_definition_id = 0L),
                                     outputs = list(m = list(type = "cohort_membership")))))
  expect_match(conditionMessage(err), "0 is refused")
})

test_that("A-16: a nearest offset of either sign is in the IR; zero and other selections are not", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  iv <- function(select, anchor = NULL) {
    out <- list(type = "intervals_long", tables = "condition_occurrence",
                window = list(start = -36500L, end = 36500L), event_select = select, select_n = 1L)
    if (!is.null(anchor)) out$anchor <- anchor
    list(cohort = list(type = "cohort_table", cohort_definition_id = 1L), outputs = list(iv = out))
  }
  opts <- function(p) .irPrepare(p)$outputs$iv$options
  expect_identical(opts(iv("nearest", 9000L))$legacy_anchor_offset, 9000L)
  expect_identical(opts(iv("nearest", -9000L))$legacy_anchor_offset, -9000L)
  late <- .rv_run(h, iv("nearest", 9000L), "iv")
  early <- .rv_run(h, iv("nearest", -9000L), "iv")
  expect_false(identical(late, early))
  expect_false(identical(.rv_digest(iv("nearest", 9000L)), .rv_digest(iv("nearest", -9000L))))
  # omitted and zero offsets: one normal form, one digest, one result
  expect_null(opts(iv("nearest", 0L))$legacy_anchor_offset)
  expect_identical(.rv_digest(iv("nearest")), .rv_digest(iv("nearest", 0L)))
  expect_identical(.rv_run(h, iv("nearest"), "iv"), .rv_run(h, iv("nearest", 0L), "iv"))
  # under another selection the executor ignores the offset: advisory annex only
  ir <- .irPrepare(iv("first", 9000L))
  expect_null(ir$outputs$iv$options$legacy_anchor_offset)
  expect_identical(attr(ir, "legacy_annex")[["outputs/iv"]][["ignored_anchor_offset"]], 9000L)
  expect_identical(.rv_digest(iv("first")), .rv_digest(iv("first", 9000L)))
  expect_identical(.rv_run(h, iv("first"), "iv"), .rv_run(h, iv("first", 9000L), "iv"))
})

test_that("A-16: folded scope cohort sources keep their order and operator", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  scoped <- function(combine) list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    scope = list(cohort = 2L, combine = combine),
    outputs = list(m = list(type = "cohort_membership")))
  ir <- .irPrepare(scoped("intersect"))
  expect_identical(ir$scope$cohort, 2L)
  expect_identical(ir$scope$legacy_cohort_sources, list(2L, 1L))
  expect_identical(ir$scope$combine, "intersect")
  union <- .rv_run(h, scoped("union"), "m")
  inter <- .rv_run(h, scoped("intersect"), "m")
  expect_false(identical(union, inter))
  expect_false(identical(.rv_digest(scoped("union")), .rv_digest(scoped("intersect"))))
  # the default operator is materialised: absent and "union" are one IR
  no_op <- scoped("union")
  no_op$scope$combine <- NULL
  expect_identical(.rv_digest(no_op), .rv_digest(scoped("union")))
  # owned temp-table names stay names
  named <- list(scope = list(cohort = list("dsomop_cohort_7", 2L)),
                outputs = list(m = list(type = "cohort_membership")))
  expect_identical(.irPrepare(named)$scope$legacy_cohort_sources, list("dsomop_cohort_7", 2L))
  # the validator ties scope.cohort to the first source
  bad <- .irDocument(ir)
  bad$scope$cohort <- 1L
  expect_match(paste(.irSemanticErrors(structure(bad, class = "dsomop_ir", source_version = 1L)), collapse = " "),
               "scope.cohort must be the first cohort source", fixed = TRUE)
})

test_that("A-16: a person_level custom filter restricts its raw tables; unread sparse specs stay unread", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  pl <- function(custom = NULL) {
    out <- list(type = "person_level", tables = list(person = list("gender_concept_id", "year_of_birth")))
    if (!is.null(custom)) out$filters <- list(custom = custom)
    list(outputs = list(p = out))
  }
  women <- list(var = "gender_concept_id", op = "==", value = 8532L)
  ir <- .irPrepare(pl(women))
  expect_identical(ir$outputs$p$legacy_raw_filter, list(var = "gender_concept_id", op = "==", value = 8532L))
  all_rows <- .rv_run(h, pl(), "p")
  filtered <- .rv_run(h, pl(women), "p")
  expect_lt(nrow(filtered), nrow(all_rows))
  expect_gt(nrow(filtered), 0L)
  expect_false(identical(.rv_digest(pl()), .rv_digest(pl(women))))
  # the executor's own refusal of an identifier filter, before any query
  err <- .rv_refused(.irPrepare(pl(list(var = "person_id", op = "==", value = 1L)), h))
  expect_match(conditionMessage(err), "/outputs/p/filters/custom: Disclosive: filter on column 'person_id'", fixed = TRUE)
  # sparse: the legacy automatic builder never reads feature specs
  sparse <- function(features = NULL) {
    rep <- list(format = "sparse")
    if (!is.null(features)) rep$features <- features
    list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
         outputs = list(s = list(type = "event_level", table = "condition_occurrence", representation = rep)))
  }
  specs <- list(list(name = "n_dx", type = "count", concept_set = list(201820L)))
  ir <- .irPrepare(sparse(specs))
  expect_identical(ir$outputs$s$legacy_automatic_features$format, "sparse")
  expect_identical(.rv_digest(sparse()), .rv_digest(sparse(specs)))
  expect_identical(.rv_run(h, sparse(), "s"), .rv_run(h, sparse(specs), "s"))
})

# --- re-review R2: interval-backed reducers resolve against their source -------

test_that("R2: interval reducer predicates and named values are typed against the interval", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h))
  with_var <- function(reducer) {
    x <- .rv_plan_a()
    x$variables$iv <- list(source = list(interval = "metf_era"), reducer = reducer)
    x$outputs$baseline$columns <- c(x$outputs$baseline$columns, list("iv"))
    x
  }
  any_of <- function(predicate) list(fn = "any", predicate = predicate)
  refused <- list(
    made_up = any_of(list(var = "made_up", op = ">", value = 1)),
    person_id = any_of(list(var = "person_id", op = "==", value = 1)),
    era_key = any_of(list(var = "era_no", op = "==", value = 1)),
    no_value_role = any_of(list(var = "value", op = ">", value = 1)),
    date_type = any_of(list(var = "start_date", op = "==", value = 7)),
    value_made_up = list(fn = "mode", value = "made_up"),
    value_private = list(fn = "max", value = "group_id"))
  for (nm in names(refused)) {
    err <- .rv_refused(.irPrepare(with_var(refused[[nm]]), h))
    expect_true(inherits(err, "dsomop_plan_rejection"), info = nm)
    expect_match(conditionMessage(err), "/variables/iv/reducer/", fixed = TRUE, info = nm)
  }
  # controls: the interval's own dates and group concept
  for (ok in list(any_of(list(var = "start_date", op = ">=", value = "2010-01-01")),
                  any_of(list(var = "concept_id", op = "in", value = list(1L, 2L))),
                  list(fn = "mode", value = "start_date"))) {
    expect_s3_class(.irPrepare(with_var(ok), h), "dsomop_ir")
  }
  # an event reducer's named value resolves against the event's own catalogue
  x <- .rv_plan_a()
  x$variables$ev <- list(source = list(event = "hba1c"), reducer = list(fn = "max", value = "made_up"))
  x$outputs$baseline$columns <- c(x$outputs$baseline$columns, list("ev"))
  expect_match(conditionMessage(.rv_refused(.irPrepare(x, h))), "/variables/ev/reducer/value", fixed = TRUE)
  x$variables$ev$reducer$value <- "value_as_number"
  expect_s3_class(.irPrepare(x, h), "dsomop_ir")
})

test_that("N1: an interval's exact dates are class 2 in explain", {
  x <- .rv_plan_a()
  x$variables$iv <- list(source = list(interval = "metf_era"), reducer = list(fn = "mode", value = "start_date"))
  x$outputs$baseline$columns <- c(x$outputs$baseline$columns, list("iv"))
  classes <- .irExplain(.irPrepare(x))$column_classes$baseline
  expect_identical(classes$iv$class, 2L)
})

test_that("an output's closure keeps the concept set of a v1 legacy concept scope", {
  # a table-level concept set next to a feature's own one (corpus v1-004)
  p <- list(outputs = list(p = list(type = "person_level",
    tables = list(measurement = list(concept_set = 3004410L, features = list(
      lab_mean = list(type = "mean_value", name = "lab_mean", concept_set = 3004411L,
                      value_column = "value_as_number")))))))
  ir <- .irPrepare(p)
  scoped <- Filter(function(e) !is.null(e$legacy_concept_scope), ir$events)
  expect_length(scoped, 1L)
  ref <- scoped[[1L]]$legacy_concept_scope$concept_set_ref
  expect_true(ref %in% names(.irOutputClosure(ir, "p")$concept_sets))
})
