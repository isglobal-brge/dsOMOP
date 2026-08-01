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

  DBI::dbWriteTable(conn, "person", data.frame(
    person_id = 1:5,
    gender_concept_id = c(8532L, 8507L, 8507L, 8507L, 8532L),
    year_of_birth = c(1988L, 1978L, 2008L, 1944L, 2010L)
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
      list(type = "age_range", params = list(
        min = 18L, max = 65L, reference_date = "2024-07-01"
      )),
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

test_that("population filter contract contains only executable server types", {
  expect_identical(.cohortFilterTypes(), c(
    "sex", "age_range", "age_group", "cohort", "has_concept",
    "not_has_concept", "concept_count", "prior_observation", "followup",
    "visit_count", "has_measurement", "missing_measurement"))
})

test_that("population filter trees are validated recursively and fail closed", {
  expect_true(.validateCohortFilterTree(local_nested_cohort_filter_tree()))
  expect_true(.isCohortFilterSpec(local_nested_cohort_filter_tree()))

  expect_error(.validateCohortFilterTree(list(and = list(
    list(type = "sex", params = list(value = "F")),
    list(or = list(
      list(type = "min_count", params = list(min_count = 2L)),
      list(type = "cohort", params = list(cohort_definition_id = 1L))
    ))
  ))), "Unknown population filter type")
  expect_false(.isCohortFilterSpec(list(
    type = "date_range", params = list(start = "2020-01-01",
                                       end = "2020-03-01"))))
  expect_error(.validateCohortFilterTree(list(
    type = "sex", params = list(value = "F", ignored = TRUE))),
    "unknown parameter.*ignored")
  expect_error(.validateCohortFilterTree(list(
    and = list(list(type = "sex", params = list(value = "F"))),
    or = list(list(type = "sex", params = list(value = "M"))))),
    "cannot mix AND/OR")
})

test_that("planValidate rejects non-executable population filter types", {
  handle <- local_plan_filter_handle()
  on.exit(DBI::dbDisconnect(handle$conn), add = TRUE)

  plan <- list(
    populations = list(base = list(
      id = "base", kind = "criteria",
      filter_tree = list(
        type = "min_count", params = list(min_count = 2L)))),
    outputs = list(people = list(
      type = "person_level",
      tables = list(person = c("person_id", "gender_concept_id")))))
  class(plan) <- c("omop_plan", "list")

  validation <- .planValidate(handle, plan)
  expect_false(validation$valid)
  expect_match(paste(validation$errors, collapse = "\n"),
               "Population 'base'.*Unknown population filter type")
})

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
          params = list(min = 18L, max = 65L,
                        reference_date = "2024-07-01")
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
      list(type = "prior_observation", params = list(
        min_days = 365L, reference_date = "2024-07-01"
      )),
      bp,
      person_cols = character(0)
    )
    followup_sql <- .compileCohortFilterLeaf(
      handle,
      list(type = "followup", params = list(
        min_days = 90L, reference_date = "2024-07-01"
      )),
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

test_that("temporal population filters never use an implicit wall clock", {
  handle <- new.env(parent = emptyenv())
  handle$target_dialect <- "sqlite"
  bp <- local_observation_period_blueprint()

  expect_error(
    .compileCohortFilterLeaf(
      handle,
      list(type = "prior_observation", params = list(min_days = 365L)),
      bp, person_cols = character(0)
    ),
    "cohort index or an explicit reference_date"
  )
})

test_that("prior observation and followup anchor to and cover the cohort index", {
  handle <- new.env(parent = emptyenv())
  handle$target_dialect <- "sqlite"
  bp <- local_observation_period_blueprint()
  anchor <- "idx.cohort_start_date"

  prior_sql <- .compileCohortFilterLeaf(
    handle,
    list(type = "prior_observation", params = list(min_days = 365L)),
    bp, person_cols = character(0), index_anchor = anchor
  )
  followup_sql <- .compileCohortFilterLeaf(
    handle,
    list(type = "followup", params = list(min_days = 90L)),
    bp, person_cols = character(0), index_anchor = anchor
  )

  expect_match(prior_sql, anchor, fixed = TRUE)
  expect_match(prior_sql, "observation_period_end_date >= idx.cohort_start_date",
               fixed = TRUE)
  expect_match(followup_sql,
               "observation_period_start_date <= idx.cohort_start_date",
               fixed = TRUE)
  expect_match(followup_sql, anchor, fixed = TRUE)
  expect_false(grepl("CURRENT_DATE|DATE\\('now'\\)", prior_sql))
  expect_false(grepl("CURRENT_DATE|DATE\\('now'\\)", followup_sql))
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

  expect_error(
    .compileCohortFilterLeaf(
      handle,
      list(type = "age_range", params = list(min = 65L, max = 150L)),
      bp,
      person_cols = "year_of_birth"
    ),
    "explicit reference_date/year"
  )
})

test_that("a max-only age range excludes negative ages", {
  handle <- local_plan_filter_handle()
  on.exit(DBI::dbDisconnect(handle$conn), add = TRUE)
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO person (person_id, gender_concept_id, year_of_birth) ",
    "VALUES (99, 8507, 2030)"
  ))

  sql <- .compileCohortFilterLeaf(
    handle,
    list(type = "age_range",
         params = list(max = 65L, reference_date = "2024-07-01")),
    local_observation_period_blueprint(),
    person_cols = c("person_id", "year_of_birth")
  )
  expect_match(sql, "year_of_birth <= (2024)", fixed = TRUE)

  ids <- withr::with_options(list(nfilter.subset = 0),
    .buildCohortFromFilters(handle, list(
      type = "age_range",
      params = list(max = 65L, reference_date = "2024-07-01")
    )))
  expect_false(99L %in% ids)
})

test_that("age_range reference_date requires an exact valid ISO date", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  bp <- .buildBlueprint(handle)
  person_cols <- bp$columns[["person"]]$column_name

  expect_error(
    .compileCohortFilterLeaf(
      handle,
      list(type = "age_range",
           params = list(min = 65L, max = 150L,
                         reference_date = "2024-7-1")),
      bp, person_cols
    ),
    "ISO date"
  )
  expect_error(
    .compileCohortFilterLeaf(
      handle,
      list(type = "age_range",
           params = list(min = 65L, max = 150L,
                         reference_date = "2024-02-30")),
      bp, person_cols
    ),
    "valid date"
  )
})

test_that("numeric has_measurement filters require a matching issued bin", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  bp <- .buildBlueprint(handle)
  person_cols <- bp$columns[["person"]]$column_name
  unbound <- list(
    type = "has_measurement",
    params = list(concept_id = 3004410L, min_value = 7, max_value = 10)
  )

  expect_error(
    .compileCohortFilterLeaf(handle, unbound, bp, person_cols),
    "not issued"
  )
  one_sided <- unbound
  one_sided$params$max_value <- NULL
  expect_error(
    .compileCohortFilterLeaf(handle, one_sided, bp, person_cols),
    "require both"
  )

  scope <- .test_issue_safe_bins(
    handle, c(0, 7, 10, 20), concept_id = 3004410L)
  bound <- unbound
  bound$params$safe_scope <- scope
  sql <- .compileCohortFilterLeaf(handle, bound, bp, person_cols)
  expect_match(sql, "m.value_as_number >= 7", fixed = TRUE)
  expect_match(sql, "m.value_as_number < 10", fixed = TRUE)

  wrong_concept <- bound
  wrong_concept$params$concept_id <- 3013682L
  expect_error(
    .compileCohortFilterLeaf(handle, wrong_concept, bp, person_cols),
    "scope must match"
  )
})

test_that("index-dependent filters reject an implicit recurrent-cohort anchor", {
  handle <- local_plan_filter_handle()
  on.exit(DBI::dbDisconnect(handle$conn), add = TRUE)

  DBI::dbWriteTable(handle$conn, "recurrent_cohort", data.frame(
    subject_id = c(1L, 1L, 2L, 3L),
    cohort_start_date = c("2020-01-01", "2021-01-01",
                          "2020-01-01", "2020-01-01"),
    cohort_end_date = c("2020-01-31", "2021-01-31",
                        "2020-01-31", "2020-01-31")
  ))

  expect_error(
    .buildCohortFromFilters(
      handle,
      list(type = "age_group", params = list(groups = "30-39")),
      index_cohort_table = "recurrent_cohort"
    ),
    "recurrent cohort without an explicit episode policy"
  )

  # A fixed reference date is person-level and does not depend on which cohort
  # episode is selected, so it remains executable for the same recurrent cohort.
  ids <- .buildCohortFromFilters(
    handle,
    list(type = "age_range", params = list(
      min = 30L, max = 90L, reference_date = "2024-07-01"
    )),
    index_cohort_table = "recurrent_cohort"
  )
  expect_true(is.integer(ids) || is.numeric(ids))

  grouped_ids <- .buildCohortFromFilters(
    handle,
    list(type = "age_group", params = list(
      groups = "30-39", reference_date = "2024-07-01"
    )),
    index_cohort_table = "recurrent_cohort"
  )
  expect_true(is.integer(grouped_ids) || is.numeric(grouped_ids))
})

test_that("recurrent cohort filters implement explicit episode policies", {
  handle <- local_plan_filter_handle()
  on.exit(DBI::dbDisconnect(handle$conn), add = TRUE)

  DBI::dbWriteTable(handle$conn, "person", data.frame(
    person_id = 6:9,
    gender_concept_id = rep(8507L, 4),
    year_of_birth = c(1995L, 1985L, 1990L, NA_integer_)
  ), append = TRUE)
  DBI::dbWriteTable(handle$conn, "recurrent_cohort", data.frame(
    subject_id = c(6L, 6L, 7L, 7L, 8L, 8L, 9L, 9L),
    cohort_start_date = c("2020-01-01", "2030-01-01",
                          "2020-01-01", "2030-01-01",
                          "2020-01-01", "2025-01-01",
                          "2020-01-01", "2025-01-01"),
    cohort_end_date = c("2020-01-31", "2030-01-31",
                        "2020-01-31", "2030-01-31",
                        "2020-01-31", "2025-01-31",
                        "2020-01-31", "2025-01-31")
  ))
  filter <- list(type = "age_group", params = list(groups = "30-39"))
  ids_for <- function(policy) {
    sort(.buildCohortFromFilters(
      handle, filter,
      index_cohort_table = "recurrent_cohort",
      episode_policy = policy
    ))
  }

  # Person 6 qualifies only at the last episode, person 7 only at the first,
  # person 8 at both, and person 9's unknown age never counts as a match.
  expect_equal(ids_for("any_episode"), 6:8)
  expect_equal(ids_for("all_episodes"), 8L)
  expect_equal(ids_for("first_episode"), c(7L, 8L))
  expect_equal(ids_for("last_episode"), c(6L, 8L))

  expect_error(
    .buildCohortFromFilters(
      handle, filter,
      index_cohort_table = "recurrent_cohort",
      episode_policy = "per_episode"
    ),
    "episode_policy must be one of"
  )
})

test_that("age_group compiles an open-ended 85+ band", {
  handle <- new.env(parent = emptyenv())
  handle$target_dialect <- "sqlite"
  bp <- local_observation_period_blueprint()
  sql <- .compileCohortFilterLeaf(
    handle,
    list(type = "age_group", params = list(
      groups = "85+", reference_date = "2024-07-01"
    )),
    bp,
    person_cols = "year_of_birth"
  )

  expect_match(sql,
    "year_of_birth <= (2024 - 85)", fixed = TRUE)
})

test_that("population filters never silently compile unknown dependencies away", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  bp <- .buildBlueprint(handle)
  person_cols <- bp$columns[["person"]]$column_name

  expect_error(
    .compileCohortFilterWhere(handle,
      list(type = "unknown_filter", params = list()),
      bp, person_cols),
    "Unknown population"
  )
  expect_error(
    .compileCohortFilterLeaf(handle,
      list(type = "has_concept",
           params = list(table = "does_not_exist", concept_id = 1L)),
      bp, person_cols),
    "unavailable|no domain concept"
  )
  expect_error(
    .compileCohortFilterLeaf(handle,
      list(type = "sex", params = list(value = "F")),
      bp, person_cols = "year_of_birth"),
    "gender_concept_id is unavailable"
  )
  expect_error(
    .compileCohortFilterLeaf(handle,
      list(type = "has_concept",
           params = list(table = "person", concept_id = 8532L,
                         window = list(start = -365L, end = 0L))),
      bp, person_cols),
    "no usable date column"
  )
})

test_that("population filter grammar rejects mixed groups and unknown params", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  bp <- .buildBlueprint(handle)
  person_cols <- bp$columns[["person"]]$column_name
  sex <- list(type = "sex", params = list(value = "F"))

  expect_error(
    .compileCohortFilterWhere(handle,
      list(and = list(sex), or = list(sex)), bp, person_cols),
    "cannot mix"
  )
  expect_error(
    .compileCohortFilterWhere(handle,
      list(type = "sex", params = list(value = "F"), and = list(sex)),
      bp, person_cols),
    "only type and params"
  )
  expect_error(
    .compileCohortFilterWhere(handle,
      list(type = "sex", params = list(value = "F", typo = TRUE)),
      bp, person_cols),
    "unknown parameter"
  )
})

test_that("population filter trees respect server complexity caps", {
  leaf <- list(type = "sex", params = list(value = "F"))

  withr::local_options(list(dsomop.max_filter_depth = 2L))
  deep <- list(and = list(list(and = list(leaf))))
  expect_error(.validateCohortFilterTree(deep), "max_filter_depth")

  withr::local_options(list(
    dsomop.max_filter_depth = 32L,
    dsomop.max_filter_nodes = 2L
  ))
  wide <- list(and = list(leaf, leaf))
  expect_error(.validateCohortFilterTree(wide), "max_filter_nodes")

  withr::local_options(list(
    dsomop.max_filter_nodes = 1024L,
    dsomop.max_filter_values = 2L
  ))
  concepts <- list(
    type = "has_concept",
    params = list(table = "condition_occurrence", concept_ids = 1:3)
  )
  expect_error(.validateCohortFilterTree(concepts), "max_filter_values")
})

test_that("large recipe concept filters use portable chunked predicates", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  bp <- .buildBlueprint(handle)
  person_cols <- bp$columns[["person"]]$column_name
  filter <- list(
    type = "has_concept",
    params = list(
      table = "condition_occurrence", concept_ids = seq_len(1001L)
    )
  )

  sql <- withr::with_options(
    list(dsomop.max_filter_values = 2000L),
    .compileCohortFilterWhere(handle, filter, bp, person_cols)
  )
  hits <- gregexpr("condition_concept_id IN (", sql, fixed = TRUE)[[1L]]
  expect_gte(sum(hits > 0L), 2L)
  expect_match(sql, " OR ", fixed = TRUE)
})
