# Independent review of 2.10.6 (concept IDs as categories): each test pins one
# fix. Assignment never answers about columns the site refuses; untranslated
# and translated generated columns share one admission; dates keep their gate
# in counted releases; refusals never name concept-bearing keys; legacy
# dictionaries are references; old clients cannot leave unreconciled factors;
# legacy interval concepts are categories.

.cfr_frame <- function(values) {
  frame <- data.frame(person_id = paste0("p", seq_along(values)),
                      cause_concept_id = .conceptFactor(values, sort(unique(values))),
                      stringsAsFactors = FALSE)
  class(frame) <- c("omop.table", "data.frame")
  attr(frame, "omop_concept_cols") <- "cause_concept_id"
  frame
}

test_that("legacy assignment applies the site's level rule before any coverage answer", {
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 0.33))
  small <- .cfr_frame(c("10", "10", "10"))
  # One level over three rows fails the density rule: a covering and a probing
  # contract get the same refusal, so the outcome reveals nothing about values.
  covering <- tryCatch(omopAsFactorColumnsDS(small, list(cause_concept_id = "10")), error = conditionMessage)
  probing <- tryCatch(omopAsFactorColumnsDS(small, list(cause_concept_id = "20")), error = conditionMessage)
  expect_match(covering, "factor-level rule")
  expect_identical(covering, probing)
  withr::local_options(list(nfilter.levels.max = 1))
  two <- .cfr_frame(rep(c("10", "20", "10"), 3L))
  expect_error(omopAsFactorColumnsDS(two, list(cause_concept_id = c("10", "20"))), "factor-level rule")
  withr::local_options(list(nfilter.levels.max = 40))
  expect_error(omopAsFactorColumnsDS(two, list(cause_concept_id = "10")), "coverage")
  expect_s3_class(omopAsFactorColumnsDS(two, list(cause_concept_id = c("10", "20"))), "omop.table")
})

test_that("A-27 assignment refuses a column this site refuses, whatever contract is sent", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  ctx <- list(handle = h, ir = list(options = list(), variables = list()))
  raw <- data.frame(person_id = paste0("p", 1:6), cause = c(10L, 20L, 10L, 20L, 10L, 20L))
  value <- .conceptPresent(ctx, raw, list(cause = .fragmentColumn("integer", TRUE, "concept")))
  withr::local_options(list(nfilter.levels.max = 1, nfilter.levels.density = 1))
  expect_identical(.conceptLevelReport(value)$unsafe, "cause")
  expect_error(.conceptRelevel(value, list(concept_protocol = 1L)), "factor-level rule")
})

.cfr_pivot_plan <- function(translate) {
  plan <- list(cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(m = list(type = "event_level", table = "measurement",
      columns = c("measurement_concept_id", "value_as_number"),
      representation = list(format = "wide"),
      temporal = list(event_select = list(order = "first", n = 1L)))),
    options = list(translate_concepts = translate, block_sensitive = TRUE))
  class(plan) <- c("omop_plan", "list")
  plan
}

test_that("generated concept columns share one admission and refusals never name their keys", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  .ws7_keys(list(h))
  withr::local_options(list(nfilter.subset = 3, nfilter.tab = 3, nfilter.levels.max = 1,
                            nfilter.levels.density = 0.33))
  # Refused at publication, translated or not: never assigned, never named.
  for (translate in c(TRUE, FALSE)) {
    error <- tryCatch(.planExecute(h, .cfr_pivot_plan(translate), list(m = "M")), error = identity)
    expect_s3_class(error, "dsomop_plan_rejection")
    expect_identical(error$reason, "concept_level_rule")
    expect_identical(.rejectionEntry(error)$reason, "concept_level_rule")
    expect_false(grepl("[0-9]{4,}", conditionMessage(error)))
  }
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 1))
  admitted <- .planExecute(h, .cfr_pivot_plan(TRUE), list(m = "M"))$m
  expect_length(.conceptLevelReport(admitted)$unsafe, 0L)
})

test_that("counted releases keep the level gate on date columns", {
  h <- create_test_handle(n_persons = 120)
  on.exit(cleanup_handle(h), add = TRUE)
  .buildBlueprint(h)
  DBI::dbExecute(h$conn, "DELETE FROM observation")
  dates <- format(as.Date("2020-01-01") + rep(0:59, each = 2L))
  DBI::dbWriteTable(h$conn, "observation", data.frame(observation_id = 1:120, person_id = 1:120,
    observation_concept_id = 4500000L, observation_date = dates,
    observation_type_concept_id = rep(c(32817L, 32818L), 60L), value_as_concept_id = 4600001L), append = TRUE)
  withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3, nfilter.levels.max = 40,
                           nfilter.levels.density = 0.33), {
    expect_error(.profileValueCounts(h, "observation", "observation_date"), "Disclosive")
    expect_error(.profileCrossTab(h, "observation", "observation_date", "observation_type_concept_id"),
                 "Disclosive")
  })
})

test_that("legacy dictionaries publish under the default density rule", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .ws7_keys(list(handle))
  withr::local_options(list(nfilter.subset = 3, nfilter.tab = 3, nfilter.levels.max = 40,
                            nfilter.levels.density = 0.33))
  plan <- list(cohort = list(type = "cohort_table", cohort_definition_id = 1), outputs = list(
    events = list(type = "event_level", table = "condition_occurrence",
      columns = c("condition_concept_id", "condition_type_concept_id"), concept_set = 201820L,
      representation = list(format = "long"), filters = list(concept_set = list(ids = 201820L))),
    dictionary = list(type = "concept_dictionary", source_outputs = "events")),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE))
  result <- .planExecute(handle, plan, list(events = "E", dictionary = "D"))
  expect_length(.conceptLevelReport(result$dictionary)$unsafe, 0L)
})

test_that("untranslated legacy plans need a client that reconciles concept factors", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  .ws7_keys(list(h)); .dsomop_handle_cfr <- h
  plan <- list(version = 1, options = list(translate_concepts = FALSE),
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    outputs = list(baseline = list(type = "baseline", columns = "gender_concept_id")))
  withr::local_options(list(nfilter.subset = 3))
  expect_error(dsOMOP::omopPlanExecuteDS("cfr", plan, list(baseline = "B")), "dsOMOPClient 2.10.6")
  expect_error(.conceptRequireProtocol(NULL), "update the client")
  expect_true(.conceptRequireProtocol(1))
  status <- dsOMOP::omopPlanExecuteDS("cfr", plan, list(baseline = "B"), concept_protocol = 1)
  expect_true(is.factor(B$gender_concept_id))
  translated <- plan; translated$options$translate_concepts <- TRUE
  expect_no_error(dsOMOP::omopPlanExecuteDS("cfr", translated, list(baseline = "B2")))
})

test_that("legacy interval rows present their concept IDs as categories", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .buildBlueprint(handle)
  plan <- list(cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(iv = list(type = "intervals_long", tables = c("condition_occurrence", "drug_exposure"))),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE))
  class(plan) <- c("omop_plan", "list")
  withr::with_options(list(nfilter.subset = 3), {
    df <- .planExecute(handle, plan, list(iv = "iv_df"))$iv
  })
  expect_true(is.factor(df$concept_id))
  expect_true(all(grepl("^[0-9]+$", levels(df$concept_id))))
})

test_that("legacy dictionaries name only concept columns, and declared concepts when publishing partially", {
  h <- create_test_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  .ws7_keys(list(h))
  outputs <- list(events = list(type = "event_level", table = "condition_occurrence", concept_set = 201820L))
  # n_concept_id is a count alias whose numbers happen to be valid concept IDs.
  results <- list(events = data.frame(person_id = c("a", "b"), condition_concept_id = c(201820L, 255573L),
                                      n_concept_id = c(8507L, 8532L)))
  withr::local_options(list(dsomop.query_strict = TRUE))
  full <- .buildDeclaredConceptDictionary(h, outputs, results = results)
  expect_true(all(c("201820", "255573") %in% as.character(full$concept_id)))
  expect_false(any(c("8507", "8532") %in% as.character(full$concept_id)))
  withr::local_options(list(dsomop.query_strict = FALSE))
  partial <- .buildDeclaredConceptDictionary(h, outputs, results = results)
  expect_setequal(as.character(partial$concept_id), "201820")
})

test_that("bind rows keeps shared concept factors and refuses different level sets", {
  withr::local_options(list(nfilter.subset = 3, nfilter.levels.max = 40, nfilter.levels.density = 1))
  frame_x <- .testPseudonymize(data.frame(person_id = 1:4))
  frame_x$condition_concept_id <- .conceptFactor(c("10", "20", "10", "20"), c("10", "20"))
  frame_y <- .testPseudonymize(data.frame(person_id = 5:8))
  frame_y$condition_concept_id <- .conceptFactor(c("20", "20", "10", "10"), c("10", "20"))
  for (f in list(frame_x, frame_y)) expect_true(.is_omop.table(f))
  bound <- omopBindRowsDS(frame_x, frame_y)
  expect_s3_class(bound$condition_concept_id, "dsomop_concept_factor")
  expect_identical(levels(bound$condition_concept_id), c("10", "20"))
  frame_z <- frame_y
  frame_z$condition_concept_id <- .conceptFactor(c("20", "20", "10", "10"), c("10", "20", "30"))
  expect_error(omopBindRowsDS(frame_x, frame_z), "different shared levels")
})
