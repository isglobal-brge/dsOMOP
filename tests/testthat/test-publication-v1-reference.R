.reference_plan <- function(table = "concept", columns = c("concept_id", "concept_code", "vocabulary_id"),
                            filter = NULL) {
  list(outputs = list(ref = list(type = "event_level", table = table,
    columns = columns, representation = list(format = "long"),
    filters = if (is.null(filter)) NULL else list(custom = filter))),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE))
}

test_that("legacy vocabulary and declared metadata sources retain reference publication", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  withr::local_options(list(nfilter.subset = 100L, dsomop.query_strict = TRUE))
  for (entry in list(
    list(table = "concept", columns = c("concept_id", "concept_code", "vocabulary_id")),
    list(table = "cdm_source", columns = c("cdm_source_name", "cdm_version")))) {
    plan <- .reference_plan(entry$table, entry$columns)
    actual <- .planExecute(handle, plan, c(ref = "R"))$ref
    expected <- DBI::dbGetQuery(handle$conn, paste("SELECT", paste(entry$columns, collapse = ","),
                                                "FROM", entry$table))
    expect_identical(actual, expected)
    expect_lt(nrow(actual), 100L)
  }
  # A reference result can have fewer than k rows, including zero. Neither
  # case supplies patient support or changes the patient publication rule.
  for (vocabulary in c("Race", "no_such_vocabulary")) {
    plan <- .reference_plan(filter = list(var = "vocabulary_id", op = "in", value = list(vocabulary)))
    actual <- .planExecute(handle, plan, c(ref = "R"))$ref
    expected <- DBI::dbGetQuery(handle$conn, paste0(
      "SELECT concept_id, concept_code, vocabulary_id FROM concept WHERE vocabulary_id = ",
      as.character(DBI::dbQuoteLiteral(handle$conn, vocabulary))))
    expect_identical(actual, expected)
    expect_lt(nrow(actual), 100L)
  }
})

test_that("legacy reference proof reaches public memory and staged inventories", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .setLegacyTestPersonKey(handle, "publication-v1-reference", .local_envir = environment())
  .setHandle("reference_handle", handle)
  on.exit(.removeHandle("reference_handle"), add = TRUE)
  staging <- withr::local_tempdir()
  Sys.chmod(staging, "0700")
  withr::local_options(list(nfilter.subset = 100L, dsomop.query_strict = TRUE,
                            dsstaging.base_dir = staging, dsomop.staged_chunk_size = 2L))
  plan <- .reference_plan(filter = list(var = "vocabulary_id", op = "in", value = list("Race")))
  expected <- DBI::dbGetQuery(handle$conn,
    "SELECT concept_id, concept_code, vocabulary_id FROM concept WHERE vocabulary_id = 'Race'")
  for (mode in c("memory", "staged")) {
    target <- new.env(parent = environment())
    target$exec <- evalq(omopPlanExecuteDS("reference_handle", plan, c(ref = "R"),
                                          output_mode = mode), target)
    status <- evalq(omopPlanStatusDS("exec"), target)
    expect_identical(status$status, "ok")
    expect_identical(status$published$ref$ref$grain, "none")
    value <- get(status$published$ref$ref$symbol, target)
    if (mode == "staged") {
      expect_s3_class(value, "FlowerDatasetDescriptor")
      value <- if (identical(value$metadata$format, "parquet")) {
        as.data.frame(arrow::read_parquet(value$metadata$file))
      } else {
        frame <- utils::read.csv(value$metadata$file, colClasses = "character",
                                 check.names = FALSE)
        frame$concept_id <- as.integer(frame$concept_id)
        frame
      }
    }
    expect_identical(value, expected)
    expect_true(evalq(omopStagedRevokeDS("exec"), target))
    expect_identical(ls(target), character(0))
  }
})

test_that("reference publication requires executor proof and rejects patient-shaped materialisation", {
  plan <- .reference_plan()
  ctx <- .publication_test_context()
  ctx$ir <- plan
  frame <- data.frame(concept_id = 1L, concept_code = "one", vocabulary_id = "test")
  # A caller-provided table declaration, frame attribute or missing person key
  # is not the private executor proof.
  attr(frame, "reference") <- TRUE
  expect_error(.publishOutputs(ctx, list(ref = frame), c(ref = "R")), "lineage_malformed")
  expect_identical(ls(ctx$target_env), character(0))
  for (key in unique(c(.PERSON_KEY_COLS(), .EPISODE_KEY_COLS(), "set_id"))) {
    ctx <- .publication_test_context()
    ctx$ir <- plan
    ctx$reference_outputs <- "ref"
    patient <- frame
    patient[[key]] <- 1L
    expect_error(.publishOutputs(ctx, list(ref = patient), c(ref = "R")), "lineage_malformed")
    expect_identical(ls(ctx$target_env), character(0))
  }
})

test_that("reference source proof does not exempt patient sources or patient scopes", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  withr::local_options(list(nfilter.subset = 100L, dsomop.query_strict = TRUE))
  plan <- .reference_plan("person", "gender_concept_id")
  expect_error(.planExecute(handle, plan, c(ref = "R")), "insufficient individuals")
  expect_error(.compileSelect(handle, "concept", person_ids = 1L), "person_ids cannot scope")
  expect_error(.compileSelect(handle, "concept", cohort_table = "owned_cohort"),
               "cohort_table cannot scope")

  # An authorized extension to a reference table can carry patient keys. Such
  # a source must get direct lineage and pass support, even with a narrow
  # projection that did not explicitly ask for its person key.
  DBI::dbExecute(handle$conn, "ALTER TABLE concept ADD COLUMN person_id INTEGER")
  DBI::dbExecute(handle$conn, "UPDATE concept SET person_id = 1")
  withr::local_options(list(dsomop.allowed_cdm_extensions = list(concept = "person_id")))
  handle$blueprint <- NULL
  expect_error(.planExecute(handle, .reference_plan(), c(ref = "R")), "insufficient individuals")
})

test_that("reference proof excludes effective visit dependencies in memory and staged outputs", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .setLegacyTestPersonKey(handle, "publication-reference-visit", .local_envir = environment())
  staging <- withr::local_tempdir()
  Sys.chmod(staging, "0700")
  DBI::dbExecute(handle$conn, "ALTER TABLE concept ADD COLUMN visit_occurrence_id INTEGER")
  visit <- DBI::dbGetQuery(handle$conn, paste(
    "SELECT visit_occurrence_id, visit_concept_id FROM visit_occurrence",
    "ORDER BY visit_occurrence_id LIMIT 1"))
  DBI::dbExecute(handle$conn, paste0("UPDATE concept SET visit_occurrence_id = ",
    visit$visit_occurrence_id[[1L]], " WHERE concept_id = 201820"))
  withr::local_options(list(nfilter.subset = 100L, dsomop.query_strict = TRUE,
    dsstaging.base_dir = staging,
    dsomop.allowed_cdm_extensions = list(concept = "visit_occurrence_id")))
  handle$blueprint <- NULL
  linked <- list(concept_ids = list(visit$visit_concept_id[[1L]]))
  empty <- list(concept_ids = list(99999999L))
  for (mode in c("memory", "staged")) {
    for (spelling in c("visit_filter", "filters.visit", "precedence")) {
      plan <- .reference_plan()
      if (spelling != "filters.visit") plan$outputs$ref$visit_filter <- linked
      if (spelling != "visit_filter") {
        plan$outputs$ref$filters <- list(visit = if (spelling == "precedence") empty else linked)
      }
      effective <- plan$outputs$ref$filters$visit %||% plan$outputs$ref$visit_filter
      sql <- .compileSelect(handle, "concept", columns = plan$outputs$ref$columns,
                             visit_filter = effective)
      selected <- .executeQuery(handle, sql)
      expect_identical(nrow(selected), if (spelling == "precedence") 0L else 1L)
      expect_false("visit_occurrence_id" %in% names(selected))
      refused <- tryCatch(.planExecute(handle, plan, c(ref = "R"), output_mode = mode),
                           dsomop_plan_rejection = identity)
      expect_s3_class(refused, "dsomop_plan_rejection")
      expect_identical(refused$code, "lineage_malformed")
      expect_length(handle$staging_dirs %||% character(0), 0L)
      expect_length(list.files(staging, recursive = TRUE, all.files = TRUE,
                                no.. = TRUE), 0L)
    }
  }
  # Source-local date predicates do not read patients. They retain reference
  # publication even when the authorized source has an unused visit extension.
  expected <- DBI::dbGetQuery(handle$conn,
    "SELECT concept_id, concept_code, vocabulary_id FROM concept")
  for (kind in c("calendar", "time_window")) {
    plan <- .reference_plan()
    if (kind == "calendar") {
      plan$outputs$ref$temporal <- list(calendar = list(start = "1900-01-01", end = "2100-01-01"))
    } else {
      plan$outputs$ref$filters <- list(time_window = list(date_column = "valid_start_date",
        start_date = "1900-01-01", end_date = "2100-01-01"))
    }
    expect_identical(.planExecute(handle, plan, c(ref = "R"))$ref, expected)
  }
})
