local_plan_filter_handle <- function() {
  testthat::skip_if_not_installed("RSQLite")

  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  handle <- new.env(parent = emptyenv())
  handle$conn <- conn
  handle$dbms <- "sqlite"
  handle$target_dialect <- "sqlite"
  handle$cdm_schema <- NULL
  handle$vocab_schema <- NULL
  handle$results_schema <- NULL
  handle$temp_schema <- NULL
  handle$resource_client <- NULL
  handle$config <- list()
  handle$blueprint <- NULL
  handle$temp_tables <- character(0)

  current_year <- as.integer(format(Sys.Date(), "%Y"))
  DBI::dbWriteTable(conn, "person", data.frame(
    person_id = 1:5,
    gender_concept_id = c(8532L, 8507L, 8507L, 8507L, 8532L),
    year_of_birth = c(
      current_year - 36L,
      current_year - 46L,
      current_year - 16L,
      current_year - 80L,
      current_year - 14L
    )
  ))
  DBI::dbWriteTable(conn, "observation_period", data.frame(
    person_id = 1:5,
    observation_period_start_date = rep("2020-01-01", 5),
    observation_period_end_date = rep("2030-12-31", 5)
  ))
  DBI::dbWriteTable(conn, "condition_occurrence", data.frame(
    condition_occurrence_id = 1:4,
    person_id = c(2L, 2L, 3L, 4L),
    condition_concept_id = c(201820L, 201820L, 201820L, 999L)
  ))

  handle
}

local_nested_cohort_filter_tree <- function() {
  list(or = list(
    list(type = "sex", params = list(value = "F")),
    list(and = list(
      list(type = "age_range", params = list(min = 18L, max = 65L)),
      list(
        type = "has_concept",
        params = list(
          concept_id = 201820L,
          table = "condition_occurrence",
          min_count = 2L
        )
      )
    ))
  ))
}

local_observation_period_blueprint <- function() {
  list(
    tables = data.frame(
      table_name = "observation_period",
      present_in_db = TRUE,
      qualified_name = "observation_period",
      stringsAsFactors = FALSE
    )
  )
}

test_that("nested population cohort filter trees preserve OR and AND semantics", {
  handle <- local_plan_filter_handle()
  on.exit(DBI::dbDisconnect(handle$conn), add = TRUE)

  ids <- sort(.buildCohortFromFilters(handle, local_nested_cohort_filter_tree()))

  expect_equal(ids, c(1L, 2L, 5L))
})

test_that("plan execute uses filter_tree before legacy flat cohort spec", {
  handle <- local_plan_filter_handle()
  on.exit(DBI::dbDisconnect(handle$conn), add = TRUE)

  plan <- list(
    cohort = list(
      type = "spec",
      spec = list(
        list(type = "sex", params = list(value = "F")),
        list(
          type = "age_range",
          params = list(min = 18L, max = 65L)
        ),
        list(
          type = "has_concept",
          params = list(
            concept_id = 201820L,
            table = "condition_occurrence",
            min_count = 2L
          )
        )
      ),
      filter_tree = local_nested_cohort_filter_tree()
    ),
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      people = list(
        type = "person_level",
        tables = list(person = c("person_id", "gender_concept_id"))
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 2), {
    result <- .planExecute(handle, plan, list(people = "people_df"))
  })

  expect_equal(sort(result$people$person_id), c(1L, 2L, 5L))
})

test_that("multi-concept has_concept / not_has_concept match any of the concepts", {
  handle <- local_plan_filter_handle()
  on.exit(DBI::dbDisconnect(handle$conn), add = TRUE)

  # condition_occurrence: person 2 -> {201820, 201820}, 3 -> {201820}, 4 -> {999}
  has <- sort(.buildCohortFromFilters(handle, list(
    type = "has_concept",
    params = list(concept_id = c(201820L, 999L),
                  table = "condition_occurrence")
  )))
  expect_equal(has, c(2L, 3L, 4L))

  # not_has excludes anyone with either concept -> only 1 and 5 remain
  not_has <- sort(.buildCohortFromFilters(handle, list(
    type = "not_has_concept",
    params = list(concept_id = c(201820L, 999L),
                  table = "condition_occurrence")
  )))
  expect_equal(not_has, c(1L, 5L))

  # Single concept still works (regression): 201820 -> persons 2 and 3
  single <- sort(.buildCohortFromFilters(handle, list(
    type = "has_concept",
    params = list(concept_id = 201820L, table = "condition_occurrence")
  )))
  expect_equal(single, c(2L, 3L))

  # The concept_ids alias is honoured identically to concept_id
  via_alias <- sort(.buildCohortFromFilters(handle, list(
    type = "has_concept",
    params = list(concept_ids = c(201820L, 999L),
                  table = "condition_occurrence")
  )))
  expect_equal(via_alias, c(2L, 3L, 4L))

  # Mimic the JSON-decoded shape: arrays arrive server-side as R lists
  as_list <- sort(.buildCohortFromFilters(handle, list(
    type = "has_concept",
    params = list(concept_id = list(201820L, 999L),
                  table = "condition_occurrence")
  )))
  expect_equal(as_list, c(2L, 3L, 4L))
})

test_that("prior observation and followup cohort filters use translated dates", {
  dialects <- c("sqlite", "postgresql", "mysql", "oracle", "bigquery",
                "spark", "sql server", "redshift", "snowflake")
  bp <- local_observation_period_blueprint()

  for (dialect in dialects) {
    handle <- new.env(parent = emptyenv())
    handle$target_dialect <- dialect

    prior_sql <- .compileCohortFilterLeaf(
      handle,
      list(type = "prior_observation", params = list(min_days = 365L)),
      bp,
      person_cols = character(0)
    )
    followup_sql <- .compileCohortFilterLeaf(
      handle,
      list(type = "followup", params = list(min_days = 90L)),
      bp,
      person_cols = character(0)
    )

    expect_match(prior_sql, "observation_period_start_date <=")
    expect_match(followup_sql, "observation_period_end_date >=")
    expect_false(grepl("CURRENT_DATE\\s*-|\\-\\s*CURRENT_DATE",
                       prior_sql))
    expect_false(grepl("CURRENT_DATE\\s*-|\\-\\s*CURRENT_DATE",
                       followup_sql))
    if (!dialect %in% c("sql server", "redshift", "snowflake")) {
      expect_false(grepl("DATEADD", prior_sql, ignore.case = TRUE))
      expect_false(grepl("DATEADD", followup_sql, ignore.case = TRUE))
    }
  }
})

test_that("age_range filter honors an explicit reference_date", {
  handle <- new.env(parent = emptyenv())
  handle$target_dialect <- "sqlite"
  bp <- local_observation_period_blueprint()

  anchored <- .compileCohortFilterLeaf(
    handle,
    list(type = "age_range",
         params = list(min = 65L, max = 150L, reference_date = "2024-07-01")),
    bp,
    person_cols = "year_of_birth"
  )
  expect_match(anchored, "year_of_birth <= (2024 - 65)", fixed = TRUE)

  current_year <- as.integer(format(Sys.Date(), "%Y"))
  default <- .compileCohortFilterLeaf(
    handle,
    list(type = "age_range", params = list(min = 65L, max = 150L)),
    bp,
    person_cols = "year_of_birth"
  )
  expect_match(default,
               paste0("year_of_birth <= (", current_year, " - 65)"),
               fixed = TRUE)
})
