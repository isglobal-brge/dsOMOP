# Public deliberately stale vocabulary: scientific behavior must differ only
# when the executable imported policy was declared.
test_that("imported validity filters stale descendants and rejects invalid explicit definitions", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn))
  DBI::dbExecute(h$conn, "UPDATE concept SET invalid_reason = 'D' WHERE concept_id = 201822")
  DBI::dbExecute(h$conn, "UPDATE concept SET standard_concept = 'C', concept_code = '001' WHERE concept_id = 201820")
  native <- list(concepts = list(201820), include_descendants = TRUE)
  imported <- c(native, list(validity_policy = "ohdsi-import-1"))
  resolve <- function(x) .resolveConceptSetNode(h, x)$ids
  expect_identical(resolve(native), c(201820L, 201821L, 201822L))
  expect_identical(resolve(imported), c(201820L, 201821L))
  expect_identical(resolve(c(imported, list(exclude = list(concepts = list(201821),
    validity_policy = "ohdsi-import-1")))), 201820L)
  for (id in c(201822, 2147483647)) {
    expect_identical(resolve(list(concepts = list(id))), as.integer(id))
    err <- .ws4_expect_rejection(resolve(list(concepts = list(id),
      validity_policy = "ohdsi-import-1")), code = "unsupported_vocabulary_reference")
    expect_false(grepl(as.character(id), conditionMessage(err), fixed = TRUE))
    .ws4_expect_rejection(resolve(c(imported, list(exclude = list(concepts = list(id),
      validity_policy = "ohdsi-import-1")))), code = "unsupported_vocabulary_reference")
  }
  DBI::dbExecute(h$conn, "DELETE FROM concept WHERE concept_id = 0")
  .ws4_expect_rejection(resolve(list(concepts = list(0), validity_policy = "ohdsi-import-1")),
    code = "unsupported_vocabulary_reference")
  DBI::dbExecute(h$conn, paste("INSERT INTO concept (concept_id, concept_name, domain_id,",
    "vocabulary_id, concept_class_id, concept_code, valid_start_date, valid_end_date)",
    "VALUES (0, 'Unknown', 'Metadata', 'None', 'Undefined', '0', '1970-01-01', '2099-12-31')"))
  expect_identical(resolve(list(concepts = list(0), validity_policy = "ohdsi-import-1")), 0L)
  DBI::dbExecute(h$conn, paste("INSERT INTO concept_relationship",
    "(concept_id_1, concept_id_2, relationship_id, invalid_reason)",
    "VALUES (201821, 201820, 'Maps to', NULL), (201822, 201820, 'Maps to', NULL)"))
  mapped <- list(concepts = list(201820), include_mapped = TRUE,
    validity_policy = "ohdsi-import-1")
  expect_identical(resolve(mapped), c(201820L, 201821L))
  expect_identical(resolve(list(concepts = list(201821), include_mapped = TRUE,
    validity_policy = "ohdsi-import-1")), 201821L)
})

test_that("policy is schema-validated and preflight refuses before patient reads", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn))
  plan <- .ws4_plan(events = list(e = list(table = "condition_occurrence", concept_set =
    list(concepts = list(2147483647), validity_policy = "ohdsi-import-1"))))
  queries <- character()
  real <- .executeQuery
  local_mocked_bindings(.executeQuery = function(handle, sql, ...) {
    queries <<- c(queries, sql)
    real(handle, sql, ...)
  })
  .ws4_expect_rejection(.irPrepare(plan, h), code = "unsupported_vocabulary_reference")
  expect_gt(length(queries), 0L)
  expect_false(any(grepl("condition_occurrence|person|measurement|observation_period", queries)))
  queries <- character()
  local_mocked_bindings(.recipeAvailableOperations = function() character())
  .ws4_expect_rejection(.irPrepare(plan, h), code = "unavailable_capability")
  expect_length(queries, 0L)
  plan$events$e$concept_set$validity_policy <- "undeclared"
  .ws4_expect_rejection(.irPrepare(plan, h))
  expect_length(queries, 0L)
})

test_that("wire normalization and IR bind imported policy without changing native declarations", {
  native <- .ws4_plan(events = list(e = list(table = "condition_occurrence",
    concept_set = list(concepts = list(201820)))))
  annotated <- native
  annotated$meta$example <- list(concepts = list(2147483647), validity_policy = "ohdsi-import-1")
  expect_false(.ohdsiHasImportedValidity(annotated))
  imported <- native
  imported$events$e$concept_set$validity_policy <- "ohdsi-import-1"
  ir <- .irPrepare(imported)
  expect_identical(ir$events$e$concept_set$validity_policy, "ohdsi-import-1")
  expect_false(identical(.planNormalizedDigest(native), .planNormalizedDigest(imported)))
  expect_true("vocabulary.ohdsi_import_validity" %in% .irOperations(ir))
  expect_false("vocabulary.ohdsi_import_validity" %in% .irOperations(.irPrepare(native)))
})

test_that("registry names never hide imported definitions from preflight or IR operations", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn))
  queries <- character()
  real <- .executeQuery
  local_mocked_bindings(.executeQuery = function(handle, sql, ...) {
    queries <<- c(queries, sql)
    real(handle, sql, ...)
  })
  for (name in c("ordinary", "meta", "metadata", "circe_provenance")) {
    for (registry in c(FALSE, TRUE)) {
      definition <- dsOMOPClient::omop_concept_set(2147483647L,
        validity_policy = "ohdsi-import-1", name = if (registry) name else NULL)
      recipe <- dsOMOPClient::omop_recipe(events = list(dsOMOPClient::omop_event(
        "condition_occurrence", definition, name = name)),
        outputs = dsOMOPClient::omop_output("o", format = "long", sources = name))
      plan <- dsOMOPClient::recipe_to_plan(recipe, version = 2)
      if (registry) expect_true(name %in% names(plan$concept_sets))
      expect_true(.ohdsiHasImportedValidity(plan))
      expect_true("vocabulary.ohdsi_import_validity" %in% .irOperations(.irPrepare(plan)))
      queries <- character()
      .ws4_expect_rejection(.irPrepare(plan, h), code = "unsupported_vocabulary_reference")
      expect_gt(length(queries), 0L)
      expect_false(any(grepl("condition_occurrence|person|measurement|observation_period", queries)))
      local({
        local_mocked_bindings(.recipeAvailableOperations = function() character())
        queries <<- character()
        .ws4_expect_rejection(.irPrepare(plan, h), code = "unavailable_capability")
        expect_length(queries, 0L)
      })
    }
  }
})

test_that("only actual advisory metadata locations are non-executable", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn))
  plan <- .ws4_plan()
  # The closed wire meta object has no arbitrary annotation fields. Test the
  # visitor directly, before the independent envelope validator, with a decoy.
  plan$meta$example <- list(concepts = list(2147483647), validity_policy = "ohdsi-import-1")
  local_mocked_bindings(.executeQuery = function(...) stop("unexpected query"))
  expect_false(.ohdsiHasImportedValidity(plan))
  expect_invisible(.ohdsiPreflightValidity(h, plan))
  # Exclusions are executable even when only the exclusion carries the policy.
  plan$events <- list(meta = list(table = "condition_occurrence", concept_set = list(
    concepts = list(201820), exclude = plan$meta$example)))
  expect_true(.ohdsiHasImportedValidity(plan))
})
