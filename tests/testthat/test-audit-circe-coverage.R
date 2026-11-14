# R5: execute the imported recipe before and after a coverage-only edit.
test_that("imported coverage override changes selected observation periods and episodes", {
  h <- create_test_handle(n_persons = 40L)
  on.exit(cleanup_handle(h))
  .ws7_keys(list(h))
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = 1:3, person_id = c(1L, 1L, 2L),
    observation_period_start_date = c("2020-01-01", "2020-01-05", "2020-01-01"),
    observation_period_end_date = c("2020-01-31", "2020-02-29", "2020-03-31"),
    period_type_concept_id = 0L), append = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  DBI::dbWriteTable(h$conn, "condition_occurrence", data.frame(
    condition_occurrence_id = 1:2, person_id = 1:2, condition_concept_id = 201820L,
    condition_start_date = "2020-01-10"), append = TRUE)
  definition <- list(ConceptSets = list(list(id = 0L, name = "condition",
    expression = list(items = list(list(concept = list(CONCEPT_ID = 201820L)))))),
    PrimaryCriteria = list(CriteriaList = list(list(ConditionOccurrence = list(CodesetId = 0L))),
      PrimaryCriteriaLimit = list(Type = "First"),
      ObservationWindow = list(PriorDays = 0L, PostDays = 0L)))
  pop <- dsOMOPClient::recipe_import_circe(definition, id = "p")
  r <- dsOMOPClient::omop_recipe(populations = pop,
    outputs = dsOMOPClient::omop_output("o", rows = "p", format = "wide"))
  members <- function(recipe) {
    run <- .ws7_ctx(h, dsOMOPClient::recipe_to_plan(recipe, version = 2))
    on.exit(.ws7_close(run))
    rows <- .fetchFragment(run$cctx, .compilePopulationByName(run$cctx, "p")$roster, "subject_id")
    rows$cohort_start_date <- as.character(rows$cohort_start_date)
    rows$cohort_end_date <- as.character(rows$cohort_end_date)
    rows[, c("subject_id", "cohort_start_date", "cohort_end_date")]
  }
  unique <- members(r)
  expect_equal(unique$subject_id, 2L)
  r$populations$p$coverage <- dsOMOPClient::omop_coverage("earliest")
  expected <- data.frame(subject_id = 1:2, cohort_start_date = "2020-01-10",
    cohort_end_date = c("2020-01-31", "2020-03-31"))
  expect_equal(members(r), expected, ignore_attr = TRUE)
  loaded <- dsOMOPClient::recipe_import_json(dsOMOPClient::recipe_export_json(r))
  expect_equal(members(loaded), expected, ignore_attr = TRUE)
  # Evaluate generated public constructors in their package namespace.
  rebuilt <- eval(parse(text = dsOMOPClient::recipe_to_code(r)), new.env(parent = asNamespace("dsOMOPClient")))
  expect_equal(members(rebuilt), expected, ignore_attr = TRUE)
})
