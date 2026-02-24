test_that("concept search finds concepts by name", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .vocabSearchConcepts(handle, "diabetes")
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true(201820 %in% result$concept_id)
})

test_that("concept search filters by domain", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .vocabSearchConcepts(handle, "a", domain = "Condition")
  expect_true(all(result$domain_id == "Condition"))
})

test_that("concept search respects standard_only", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .vocabSearchConcepts(handle, "a", standard_only = TRUE)
  if (nrow(result) > 0 && "standard_concept" %in% names(result)) {
    expect_true(all(result$standard_concept == "S"))
  }
})

test_that("concept lookup by ID works", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .vocabLookupConcepts(handle, c(201820, 3004410))
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true("Diabetes mellitus" %in% result$concept_name)
  expect_true("Hemoglobin A1c" %in% result$concept_name)
})

test_that("concept lookup returns empty for missing IDs", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .vocabLookupConcepts(handle, c(999999))
  expect_equal(nrow(result), 0)
})

test_that("descendant lookup works via concept_ancestor", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # 4000000 (Metabolic disease) is ancestor of 201820 (Diabetes)
  result <- .vocabGetDescendants(handle, c(4000000), include_self = TRUE)
  expect_true(is.data.frame(result))
  expect_true(201820 %in% result$concept_id)
  expect_true(4000000 %in% result$concept_id)  # self included
})

test_that("descendant lookup excludes self when requested", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .vocabGetDescendants(handle, c(4000000), include_self = FALSE)
  expect_false(4000000 %in% result$concept_id)
  expect_true(201820 %in% result$concept_id)
})

test_that("ancestor lookup works", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .vocabGetAncestors(handle, c(201820), include_self = TRUE)
  expect_true(is.data.frame(result))
  expect_true(4000000 %in% result$concept_id)  # Metabolic disease is ancestor
  expect_true(201820 %in% result$concept_id)     # self included
})

test_that("concept set expansion includes descendants", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  cs <- list(
    concepts = c(4000001),
    include_descendants = TRUE,
    exclude = NULL
  )
  expanded <- .vocabExpandConceptSet(handle, cs)
  expect_true(4000001 %in% expanded)
  expect_true(255573 %in% expanded)   # COPD
  expect_true(317009 %in% expanded)   # Asthma
})

test_that("concept set expansion excludes IDs", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  cs <- list(
    concepts = c(4000001),
    include_descendants = TRUE,
    exclude = c(317009)
  )
  expanded <- .vocabExpandConceptSet(handle, cs)
  expect_true(255573 %in% expanded)
  expect_false(317009 %in% expanded)
})

test_that("translate columns replaces concept IDs with names", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  df <- data.frame(
    person_id = 1:2,
    condition_concept_id = c(201820, 255573),
    stringsAsFactors = FALSE
  )
  result <- .vocabTranslateColumns(handle, df)
  expect_false(any(result$condition_concept_id %in% c("201820", "255573")))
})
