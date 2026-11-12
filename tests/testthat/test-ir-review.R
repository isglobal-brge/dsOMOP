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
