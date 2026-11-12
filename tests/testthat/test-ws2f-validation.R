.ws2f_reference_plan <- function(ref, index = FALSE) {
  population <- list(kind = "criteria", cohort_definition_id = ref)
  if (index) population$index_event <- list(
    table = "condition_occurrence", concept_set = 201820L, primary_limit = "all")
  list(populations = list(p = population), outputs = list(demo = list(
    type = "baseline", columns = "gender_concept_id", population_id = "p")),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE))
}

test_that("B3 numeric population wrappers refuse before public endpoint work", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .setLegacyTestPersonKey(handle, "ws2f-validation", .local_envir = environment())
  .setHandle("ws2f_validation", handle)
  on.exit(.removeHandle("ws2f_validation"), add = TRUE)
  withr::local_options(list(nfilter.subset = 3L, nfilter.tab = 1L))
  queries <- statements <- metadata <- dialect_queries <- 0L
  contexts <- conditions <- list()
  original_context <- .planContext
  original_rejection <- .rejectionEntry
  local_mocked_bindings(
    .executeQuery = function(...) { queries <<- queries + 1L; stop("unexpected query") },
    .executeStatement = function(...) { statements <<- statements + 1L; stop("unexpected statement") },
    .buildBlueprint = function(...) { metadata <<- metadata + 1L; stop("unexpected metadata") },
    .sqlQueryOne = function(...) { dialect_queries <<- dialect_queries + 1L; stop("unexpected dialect query") },
    .planContext = function(...) {
      ctx <- original_context(...)
      contexts[[length(contexts) + 1L]] <<- ctx
      ctx
    },
    .rejectionEntry = function(e) {
      conditions[[length(conditions) + 1L]] <<- e
      original_rejection(e)
    }
  )
  for (index in c(FALSE, TRUE)) for (ref in list(list(1L), list("1"), list("01"),
                                                TRUE, FALSE, list(TRUE), list(FALSE))) {
    plan <- .ws2f_reference_plan(ref, index)
    # This first, private population would query person before a late refusal.
    plan$populations <- c(list(earlier = list(kind = "criteria",
      filter_tree = list(type = "sex", params = list(value = "M")))), plan$populations)
    err <- tryCatch(.irPrepare(plan, handle), error = identity)
    expect_s3_class(err, "dsomop_plan_rejection")
    expect_identical(err$code, "validation_error")
    expect_match(conditionMessage(err), "/populations/p/cohort_definition_id", fixed = TRUE)
    target <- new.env(parent = environment())
    handle$sql_dialect_profile <- NULL
    status <- evalq(omopPlanExecuteDS("ws2f_validation", plan, list(demo = "D_ws2f")), target)
    expect_identical(status$status, "refused")
    expect_identical(status$rejected$demo$code, "validation_error")
    expect_length(status$published, 0L)
    expect_identical(ls(target, all.names = TRUE), character(0))
    expect_length(contexts[[length(contexts)]]$inventory, 0L)
    expect_s3_class(conditions[[length(conditions)]], "dsomop_plan_rejection")
    expect_match(conditionMessage(conditions[[length(conditions)]]),
                 "/populations/p/cohort_definition_id", fixed = TRUE)
  }
  expect_identical(queries, 0L)
  expect_identical(statements, 0L)
  expect_identical(metadata, 0L)
  expect_identical(dialect_queries, 0L)
})

test_that("B3 scalar IDs and historical named wrappers retain their results", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  withr::local_options(list(nfilter.subset = 3L, nfilter.tab = 1L))
  DBI::dbExecute(handle$conn, paste0("CREATE TEMP TABLE ReviewWrapped AS ",
    "SELECT subject_id, cohort_start_date, cohort_end_date ",
    "FROM cohort WHERE cohort_definition_id = 1"))
  register_test_temp(handle, "ReviewWrapped")
  reference_ir <- reference_result <- NULL
  for (ref in list(1L, "1", "01", "ReviewWrapped", list("ReviewWrapped"))) {
    plan <- .ws2f_reference_plan(ref)
    expect_true(.planValidate(handle, plan)$valid)
    ir <- .irPrepare(plan, handle)
    result <- .planExecute(handle, plan, list(demo = "D_ws2f"))$demo
    expect_equal(nrow(result), 6L)
    if (is.null(reference_result)) reference_result <- result
    expect_identical(result, reference_result)
    if (is.null(reference_ir)) reference_ir <- ir
    if (!is.list(ref) && ref %in% c(1L, "1", "01")) {
      expect_identical(.planSemanticDigest(ir), .planSemanticDigest(reference_ir))
    } else {
      expect_identical(ir$populations$p$legacy_cohort_table, "ReviewWrapped")
    }
  }
  # scope.cohort is separately an array of sources, not one population ref.
  for (sources in list(list(1L), list("1"), list(1L, "ReviewWrapped"))) {
    plan <- list(scope = list(cohort = sources), outputs = list(demo = list(
      type = "baseline", columns = "gender_concept_id")),
      options = list(translate_concepts = FALSE, block_sensitive = TRUE))
    expect_s3_class(.irPrepare(plan, handle), "dsomop_ir")
    result <- .planExecute(handle, plan, list(demo = "D_ws2f"))$demo
    expect_identical(result, reference_result)
  }
})

test_that("N3 malformed envelopes precede validation, preview and audit traversal", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .setHandle("ws2f_preview", handle)
  on.exit(.removeHandle("ws2f_preview"), add = TRUE)
  plans <- list(
    output_boolean = list(outputs = list(x = TRUE)),
    cohort_boolean = list(cohort = TRUE, outputs = list(x = list(
      type = "person_level", tables = list(person = "gender_concept_id")))),
    type_array = list(outputs = list(x = list(type = c("person_level", "event_level")))),
    filters_boolean = list(outputs = list(x = list(type = "person_level",
      tables = list(person = "gender_concept_id"), filters = TRUE)))
  )
  metadata <- queries <- audits <- 0L
  local_mocked_bindings(
    .buildBlueprint = function(...) { metadata <<- metadata + 1L; stop("unexpected metadata") },
    .executeQuery = function(...) { queries <<- queries + 1L; stop("unexpected query") },
    .omopAuditLog = function(...) { audits <<- audits + 1L; invisible(NULL) }
  )
  for (name in names(plans)) {
    plan <- plans[[name]]
    validation <- .planValidate(handle, plan)
    expect_false(validation$valid, info = name)
    expect_true(length(validation$errors) > 0L, info = name)
    expect_length(validation$available_tables, 0L)
    for (preview in list(function() .planPreview(handle, plan),
                         function() omopPlanPreviewDS("ws2f_preview", plan))) {
      error <- tryCatch(preview(), error = identity)
      expect_s3_class(error, "dsomop_plan_rejection")
      expect_identical(error$code, "validation_error", info = name)
    }
  }
  expect_identical(metadata, 0L)
  expect_identical(queries, 0L)
  expect_identical(audits, 0L)
})

test_that("N3 native validation uses prepared IR without legacy table assumptions", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  plan <- list(version = 2L,
    meta = list(language_version = "2.0", semantic_version = "2.0.0", generated_by = "ws2f"),
    populations = list(base = list(kind = "criteria")),
    variables = list(sex = list(raw = list(table = "person", column = "gender_concept_id"))),
    outputs = list(x = list(rows = "base", format = "wide", grain = "person", columns = list("sex"))))
  local_mocked_bindings(.planLegacyFindings = function(...) stop("unexpected v1 diagnostics"))
  expect_s3_class(.irPrepare(plan, handle), "dsomop_ir")
  validation <- .planValidate(handle, plan)
  expect_true(validation$valid)
  expect_length(validation$errors, 0L)
  expect_true("person" %in% validation$available_tables)
  preview <- .planPreview(handle, plan)
  expect_true(preview$validation$valid)
  expect_identical(preview$outputs$x$type, "wide")
  expect_identical(preview$outputs$x$columns, "sex")
  expect_false(preview$outputs$x$columns_complete)
  expect_false(preview$outputs$x$n_persons_available)

  plan$events <- list(diagnosis = list(table = "condition_occurrence"))
  plan$populations$base <- list(kind = "index", index = list(source = list(event = "diagnosis")))
  plan$outputs <- list(s = list(rows = "base", format = "survival", options = list(
    outcomes = list(diagnosis = list(event = "diagnosis")),
    tar = list(start = 7L, end = 100L), censor = list("observation_end"))))
  ir <- .irPrepare(plan, handle)
  native_survival <- .planPreview(handle, plan)
  expect_true(native_survival$validation$valid)
  expect_identical(native_survival$outputs$s$type, "survival")
  expect_identical(native_survival$outputs$s$options, ir$outputs$s$options)
  expect_null(native_survival$outputs$s$outcomes)
  expect_null(native_survival$outputs$s$tar)
  expect_false(native_survival$outputs$s$columns_complete)
  expect_false(native_survival$outputs$s$n_persons_available)
})

test_that("N3 safe v1 validation keeps useful historical diagnostics", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  missing <- .planValidate(handle, list(outputs = list(x = list(
    type = "event_level", table = "missing_table"))))
  expect_false(missing$valid)
  expect_true("Output 'x': table 'missing_table' not found." %in% missing$errors)
  warning <- .planValidate(handle, list(outputs = list(x = list(
    type = "event_level", table = "person", columns = "missing_column"))))
  expect_match(paste(warning$warnings, collapse = " "), "columns not found: missing_column", fixed = TRUE)
  cohort <- .planValidate(handle, list(outputs = list(x = list(type = "baseline"))))
  expect_false(cohort$valid)
  expect_match(paste(cohort$errors, collapse = " "), "requires a cohort.*no executable cohort")
  age <- .planValidate(handle, list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    outputs = list(x = list(type = "baseline", age_breaks = c(5L, 18L)))))
  expect_false(age$valid)
  expect_match(paste(age$errors, collapse = " "), "invalid age_breaks", fixed = TRUE)
})

test_that("N3 preview traverses validated named atomic objects consistently", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .setHandle("ws2f_object_preview", handle)
  on.exit(.removeHandle("ws2f_object_preview"), add = TRUE)
  plans <- list(
    list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
      outputs = list(x = c(type = "event_level", table = "person"))),
    list(version = 2L,
      meta = list(language_version = "2.0", semantic_version = "2.0.0", generated_by = "ws2f"),
      populations = list(base = list(kind = "criteria")),
      variables = list(sex = list(raw = list(table = "person", column = "gender_concept_id"))),
      outputs = list(x = c(rows = "base", format = "wide", grain = "person")))
  )
  for (plan in plans) {
    canonical <- .irJsonForm(plan)
    expect_true(.planValidate(handle, plan)$valid)
    expect_identical(.planValidate(handle, plan), .planValidate(handle, canonical))
    expected <- .planPreview(handle, canonical)
    expect_identical(.planPreview(handle, plan), expected)
    expect_identical(omopPlanPreviewDS("ws2f_object_preview", plan), expected)
    expect_identical(omopPlanPreviewDS("ws2f_object_preview", canonical), expected)
  }
})

test_that("N3 internal frame scopes remain declared and restricted during preview", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .buildBlueprint(handle)
  local_mocked_bindings(.executeQuery = function(...) stop("unexpected result query"))
  frame <- data.frame(person_id = 1:6)
  for (frames in list(frame, list(frame))) {
    plan <- list(scope = list(tables_frames = frames), outputs = list(
      demo = list(type = "baseline", columns = "gender_concept_id"),
      people = list(type = "event_level", table = "person", columns = "gender_concept_id")))
    expect_identical(.planWirePart(plan)$scope_tables, 1L)
    validation <- .planValidate(handle, plan)
    expect_true(validation$valid)
    expect_length(validation$errors, 0L)
    preview <- .planPreview(handle, plan)
    expect_true(preview$validation$valid)
    expect_false(preview$outputs$people$n_persons_available)
    expect_true(is.na(preview$outputs$people$n_persons))
    expect_false(preview$outputs$people$n_persons_banded)
  }
})
