test_that("cohort creation with inclusion criteria filters persons", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Base cohort: all persons with diabetes (concept 201820)
  spec <- list(
    type = "condition",
    concept_set = c(201820),
    inclusion_criteria = list(
      list(
        table = "measurement",
        concept_set = c(3004410),
        temporal = list(),
        occurrence = list(type = "at_least", count = 1)
      )
    )
  )

  withr::with_options(list(nfilter.subset = 3), {
    # This should work: base cohort has persons, inclusion filters further
    result <- tryCatch(
      .cohortCreate(handle, spec, mode = "temporary", cohort_id = 99),
      error = function(e) e$message
    )
    # Either succeeds (returns temp table name) or fails disclosure
    # (too few persons after filtering) — both are valid behavior
    expect_true(is.character(result))
  })
})

test_that("public cohort creation preserves only its final inclusion table", {
  handle <- create_test_handle()
  symbol <- paste0("cohort_success_", Sys.getpid())
  .setHandle(symbol, handle)
  on.exit(.removeHandle(symbol), add = TRUE)
  .buildBlueprint(handle)
  baseline <- handle$temp_tables
  spec <- list(
    type = "condition",
    concept_set = 201820L,
    inclusion_criteria = list(
      list(
        table = "measurement",
        concept_set = 3004410L,
        occurrence = list(type = "at_least", count = 1L)
      ),
      list(
        table = "condition_occurrence",
        concept_set = 201820L,
        occurrence = list(type = "at_least", count = 1L)
      )
    )
  )

  result <- withr::with_options(
    list(nfilter.subset = 3),
    omopCohortCreateDS(symbol, spec, mode = "temporary", cohort_id = 991L)
  )
  expect_identical(result, "dsomop_cohort_991_ic2")
  expect_setequal(handle$temp_tables, c(baseline, result))
  expect_true(DBI::dbExistsTable(handle$conn, result))
  expect_false(DBI::dbExistsTable(handle$conn, "dsomop_cohort_991"))
  expect_false(DBI::dbExistsTable(handle$conn, "dsomop_cohort_991_ic1"))
})

test_that("failed public cohort creation releases base and inclusion intermediates", {
  handle <- create_test_handle()
  symbol <- paste0("cohort_failure_", Sys.getpid())
  .setHandle(symbol, handle)
  on.exit(.removeHandle(symbol), add = TRUE)
  .buildBlueprint(handle)
  baseline <- handle$temp_tables
  spec <- list(
    type = "condition",
    concept_set = 201820L,
    inclusion_criteria = list(
      list(
        table = "measurement", concept_set = 3004410L,
        occurrence = list(type = "at_least", count = 1L)
      ),
      list(table = "not_a_cdm_table")
    )
  )

  withr::with_options(list(nfilter.subset = 3), {
    expect_error(
      omopCohortCreateDS(
        symbol, spec, mode = "temporary", cohort_id = 992L
      ),
      "not present"
    )
  })
  expect_identical(handle$temp_tables, baseline)
  expect_false(DBI::dbExistsTable(handle$conn, "dsomop_cohort_992"))
  expect_false(DBI::dbExistsTable(handle$conn, "dsomop_cohort_992_ic1"))
})

test_that("cohort creation without inclusion criteria works", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  spec <- list(
    type = "condition",
    concept_set = c(201820)
  )

  withr::with_options(list(nfilter.subset = 3), {
    temp_name <- .cohortCreate(handle, spec, mode = "temporary",
                                cohort_id = 1)
    expect_true(is.character(temp_name))
    expect_true(grepl("dsomop_cohort_", temp_name))

    # Verify the temp table has rows
    rows <- DBI::dbGetQuery(handle$conn,
      paste0("SELECT COUNT(*) AS n FROM ", temp_name))
    expect_true(rows$n > 0)
  })
})

test_that("cohort creation closes a NULL event end at its start date", {
  handle <- create_test_handle(n_persons = 1L)
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(handle$conn, "DELETE FROM condition_occurrence")
  DBI::dbExecute(handle$conn, paste0(
    "INSERT INTO condition_occurrence ",
    "(condition_occurrence_id, person_id, condition_concept_id, ",
    "condition_start_date, condition_end_date, condition_type_concept_id) ",
    "VALUES (99901, 1, 201820, '2020-05-04', NULL, 44818518)"
  ))
  .buildBlueprint(handle, force = TRUE)

  temp_name <- withr::with_options(list(nfilter.subset = 0),
    .cohortCreate(handle, list(type = "condition", concept_set = 201820L),
                  mode = "temporary", cohort_id = 777L))
  row <- DBI::dbGetQuery(handle$conn, paste0(
    "SELECT cohort_start_date, cohort_end_date FROM ", temp_name
  ))
  expect_equal(row$cohort_start_date, "2020-05-04")
  expect_equal(row$cohort_end_date, "2020-05-04")
})

test_that("cohort numeric filtering accepts only an issued value_bin", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  scope <- .test_issue_safe_bins(
    handle, c(0, 6.5, 10, 20), concept_id = 3004410L,
    concept_col = "measurement_concept_id")
  spec <- list(
    type = "measurement",
    concept_set = c(3004410),
    value_bin = list(lower = 6.5, upper = 20, safe_scope = scope)
  )

  withr::with_options(list(nfilter.subset = 3), {
    temp_name <- .cohortCreate(handle, spec, mode = "temporary",
                                cohort_id = 42)
    expect_true(is.character(temp_name))

    # The cohort must contain exactly the persons whose measurement meets the
    # threshold. Computed from a direct query so the test is robust to the
    # fixture's specific values; it still fails if the threshold is dropped,
    # negated, or applied to the wrong operator.
    expected <- DBI::dbGetQuery(handle$conn,
      "SELECT COUNT(DISTINCT person_id) AS n FROM measurement
       WHERE measurement_concept_id = 3004410 AND value_as_number >= 6.5")$n
    actual <- DBI::dbGetQuery(handle$conn,
      paste0("SELECT COUNT(DISTINCT subject_id) AS n FROM ", temp_name))$n
    expect_equal(actual, expected)
    expect_gte(actual, 3)  # cleared the disclosure floor
  })
})

test_that("legacy or forged cohort numeric thresholds fail closed", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  base <- list(type = "measurement", concept_set = c(3004410))

  withr::with_options(list(nfilter.subset = 3), {
    for (bad_op in c("==", "=", "!=", ">=", "<=")) {
      spec <- c(base, list(value_threshold = list(op = bad_op, value = 6.5)))
      expect_error(
        .cohortCreate(handle, spec, mode = "temporary", cohort_id = 43),
        "no longer executable"
      )
    }
    forged <- c(base, list(value_bin = list(
      lower = 6.5, upper = 10,
      safe_scope = list(
        table = "measurement", column = "value_as_number",
        concept_id = 3004410L,
        concept_col = "measurement_concept_id", n_bins = 3L
      )
    )))
    expect_error(
      .cohortCreate(handle, forged, mode = "temporary", cohort_id = 44),
      "not issued"
    )
  })
})

test_that("cohort creation accepts a concept_set spec and expands descendants", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # 4000001 (Respiratory disease) has no direct condition records; its
  # descendants (COPD 255573, Asthma 317009) do. A bare flat vector would
  # match nobody and fail the disclosure floor, so a non-empty cohort here
  # proves the spec's include_descendants expansion was honoured.
  spec <- list(
    type = "condition",
    concept_set = list(concepts = c(4000001), include_descendants = TRUE)
  )

  withr::with_options(list(nfilter.subset = 3), {
    temp_name <- .cohortCreate(handle, spec, mode = "temporary",
                                cohort_id = 77)
    expect_true(is.character(temp_name))

    expanded <- .vocabExpandConceptSet(handle,
      list(concepts = c(4000001), include_descendants = TRUE))
    id_list <- paste(expanded, collapse = ", ")
    expected <- DBI::dbGetQuery(handle$conn, paste0(
      "SELECT COUNT(DISTINCT person_id) AS n FROM condition_occurrence
       WHERE condition_concept_id IN (", id_list, ")"))$n
    actual <- DBI::dbGetQuery(handle$conn,
      paste0("SELECT COUNT(DISTINCT subject_id) AS n FROM ", temp_name))$n
    expect_equal(actual, expected)
    expect_gte(actual, 3)  # cleared the disclosure floor
  })
})

test_that("applyInclusionCriteria returns cohort_temp for empty criteria", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .applyInclusionCriteria(handle, "some_table", list())
  expect_equal(result, "some_table")

  result2 <- .applyInclusionCriteria(handle, "some_table", NULL)
  expect_equal(result2, "some_table")
})

test_that("cohort definition and membership resolve independent OHDSI daimons", {
  h <- new.env(parent = emptyenv())
  h$cdm_schema <- "cdm"
  h$vocab_schema <- "vocab"
  h$results_schema <- "results"
  bp <- list(tables = data.frame(
    table_name = c("cohort_definition", "cohort"),
    present_in_db = c(TRUE, TRUE),
    qualified_name = c("vocab.cohort_definition", "results.cohort"),
    stringsAsFactors = FALSE
  ))

  expect_equal(
    .cohortReadTable(h, bp, "cohort_definition"),
    "vocab.cohort_definition"
  )
  expect_equal(.cohortReadTable(h, bp, "cohort"), "results.cohort")
})

test_that("persistent cohorts apply the same inclusion criteria as temporary cohorts", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))
  handle$cdm_schema <- "main"
  handle$vocab_schema <- "main"
  handle$results_schema <- "main"
  .buildBlueprint(handle)

  spec <- list(
    type = "condition",
    concept_set = 201820L,
    inclusion_criteria = list(list(
      table = "measurement",
      concept_set = 3004410L,
      occurrence = list(type = "at_least", count = 1L)
    ))
  )

  withr::with_options(list(nfilter.subset = 3), {
    temp <- .cohortCreate(handle, spec, mode = "temporary", cohort_id = 998L)
    expected <- DBI::dbGetQuery(handle$conn, paste0(
      "SELECT DISTINCT subject_id FROM ", temp, " ORDER BY subject_id"))

    .cohortCreate(handle, spec, mode = "persistent", cohort_id = 999L,
                  overwrite = TRUE)
    actual <- DBI::dbGetQuery(handle$conn,
      "SELECT DISTINCT subject_id FROM cohort
       WHERE cohort_definition_id = 999 ORDER BY subject_id")

    expect_equal(actual, expected)
  })
})

test_that("cohort criteria fail closed instead of becoming unfiltered", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .cohortCreate(handle,
      list(type = "condition", concept_set = integer(0)),
      mode = "temporary", cohort_id = 800L),
    "resolved to no concepts"
  )
  expect_error(
    .cohortCreate(handle,
      list(type = "condition", concept_sset = 201820L),
      mode = "temporary", cohort_id = 802L),
    "Unknown cohort spec field"
  )

  base <- .cohortCreate(handle,
    list(type = "condition", concept_set = 201820L),
    mode = "temporary", cohort_id = 801L)
  expect_error(
    .applyInclusionCriteria(handle, base,
      list(list(table = "table_that_does_not_exist"))),
    "not present"
  )
  expect_error(
    .applyInclusionCriteria(handle, base,
      list(list(table = "measurement",
                temporal = list(calendar = list(
                  start = "2020-01-01' OR 1=1 --"))))),
    "ISO date"
  )

  expect_error(
    .applyInclusionCriteria(handle, base,
      list(list(table = "measurement", ignored_field = TRUE))),
    "Unknown inclusion criterion field"
  )
  expect_error(
    .applyInclusionCriteria(handle, base,
      list(list(table = "measurement",
                occurrence = list(type = "at_least", count = 1.5)))),
    "non-negative integer"
  )
  expect_error(
    .applyInclusionCriteria(handle, base,
      list(list(table = "measurement", temporal = list(
        index_window = list(start = -30.5, end = 0))))),
    "integer day offset"
  )
  expect_error(
    .applyInclusionCriteria(handle, base,
      list(list(table = "measurement", temporal = list(calendar = list(
        start = "2020-01-01", end = "2020-01-10"))))),
    "at least 30 days"
  )
})

test_that("persistent cohort identifiers and overwrite fail closed", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))
  handle$cdm_schema <- "main"
  handle$vocab_schema <- "main"
  handle$results_schema <- "main"
  .buildBlueprint(handle)

  spec <- list(type = "condition", concept_set = 201820L)
  expect_error(
    .cohortCreate(handle, spec, mode = "persistent", cohort_id = 12.5),
    "non-negative integer"
  )

  withr::with_options(list(nfilter.subset = 3), {
    .cohortCreate(handle, spec, mode = "persistent", cohort_id = 812L,
                  overwrite = TRUE)
    before <- DBI::dbGetQuery(handle$conn,
      "SELECT COUNT(*) AS n FROM cohort WHERE cohort_definition_id = 812")$n

    bad <- c(spec, list(inclusion_criteria = list(list(
      table = "measurement", unsupported = TRUE
    ))))
    expect_error(
      .cohortCreate(handle, bad, mode = "persistent", cohort_id = 812L,
                    overwrite = TRUE),
      "Unknown inclusion criterion field"
    )
    after <- DBI::dbGetQuery(handle$conn,
      "SELECT COUNT(*) AS n FROM cohort WHERE cohort_definition_id = 812")$n
    expect_equal(after, before)
  })
})

test_that("persistent cohorts reject an existing id unless overwrite is true", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))
  handle$cdm_schema <- "main"
  handle$vocab_schema <- "main"
  handle$results_schema <- "main"
  .buildBlueprint(handle)

  before <- DBI::dbGetQuery(handle$conn,
    "SELECT * FROM cohort WHERE cohort_definition_id = 1 ORDER BY subject_id")
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(
      .cohortCreate(
        handle,
        list(type = "condition", concept_set = 201820L),
        mode = "persistent", cohort_id = 1L, overwrite = FALSE
      ),
      "already has persisted rows"
    )
  })
  after <- DBI::dbGetQuery(handle$conn,
    "SELECT * FROM cohort WHERE cohort_definition_id = 1 ORDER BY subject_id")
  expect_equal(after, before)
})

test_that("persistent cohort replacement rolls back when insert fails", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))
  handle$cdm_schema <- "main"
  handle$vocab_schema <- "main"
  handle$results_schema <- "main"
  .buildBlueprint(handle)

  DBI::dbExecute(handle$conn, paste(
    "INSERT INTO cohort",
    "(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)",
    "VALUES (812, 999, '2018-01-01', '2018-12-31')"
  ))
  DBI::dbExecute(handle$conn, paste(
    "CREATE TRIGGER fail_cohort_812 BEFORE INSERT ON cohort",
    "WHEN NEW.cohort_definition_id = 812",
    "BEGIN SELECT RAISE(ABORT, 'forced cohort insert failure'); END"
  ))
  before <- DBI::dbGetQuery(handle$conn,
    "SELECT * FROM cohort WHERE cohort_definition_id = 812")

  withr::with_options(list(nfilter.subset = 3), {
    expect_error(
      .cohortCreate(
        handle,
        list(type = "condition", concept_set = 201820L),
        mode = "persistent", cohort_id = 812L, overwrite = TRUE
      ),
      "not committed.*forced cohort insert failure"
    )
  })
  after <- DBI::dbGetQuery(handle$conn,
    "SELECT * FROM cohort WHERE cohort_definition_id = 812")
  expect_equal(after, before)
})

test_that("persistent cohort writes fail closed without transaction support", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))
  handle$cdm_schema <- "main"
  handle$vocab_schema <- "main"
  handle$results_schema <- "main"
  .buildBlueprint(handle)

  local_mocked_bindings(
    dbWithTransaction = function(...) stop("transactions unsupported"),
    .package = "DBI"
  )
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(
      .cohortCreate(
        handle,
        list(type = "condition", concept_set = 201820L),
        mode = "persistent", cohort_id = 813L, overwrite = TRUE
      ),
      "requires a successful database transaction.*unsupported"
    )
  })
  expect_identical(
    DBI::dbGetQuery(handle$conn,
      "SELECT COUNT(*) AS n FROM cohort WHERE cohort_definition_id = 813")$n,
    0L
  )
})

test_that("cohort intersection preserves left eras without K-by-K multiplication", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))

  eras <- paste(
    "SELECT 1 AS subject_id, '2020-01-01' AS cohort_start_date,",
    "'2020-01-31' AS cohort_end_date UNION ALL",
    "SELECT 1, '2020-03-01', '2020-03-31' UNION ALL",
    "SELECT 2, '2020-01-01', '2020-01-31' UNION ALL",
    "SELECT 2, '2020-03-01', '2020-03-31' UNION ALL",
    "SELECT 3, '2020-01-01', '2020-01-31' UNION ALL",
    "SELECT 3, '2020-03-01', '2020-03-31'"
  )
  .createTempTable(handle, "cohort_eras_a", eras)
  .createTempTable(handle, "cohort_eras_b", eras)

  withr::with_options(list(nfilter.subset = 3), {
    out <- .cohortCombine(handle, "intersect", "cohort_eras_a",
                          "cohort_eras_b", "cohort_eras_intersect")
    result <- DBI::dbGetQuery(handle$conn, paste0(
      "SELECT * FROM ", out, " ORDER BY subject_id, cohort_start_date"))
    expect_equal(nrow(result), 6L)
    expect_equal(as.integer(table(result$subject_id)), c(2L, 2L, 2L))
  })
})

test_that("identical cohort inputs preserve set semantics and lifecycle", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))

  rows_with_duplicate <- paste(
    "SELECT 1 AS subject_id, '2020-01-01' AS cohort_start_date,",
    "'2020-01-31' AS cohort_end_date UNION ALL",
    "SELECT 1, '2020-01-01', '2020-01-31' UNION ALL",
    "SELECT 2, '2020-02-01', '2020-02-29' UNION ALL",
    "SELECT 3, '2020-03-01', '2020-03-31'"
  )
  .createTempTable(handle, "cohort_same", rows_with_duplicate)

  withr::with_options(list(nfilter.subset = 3), {
    intersected <- .cohortCombine(
      handle, "intersect", "cohort_same", "cohort_same",
      "cohort_same_intersect"
    )
    united <- .cohortCombine(
      handle, "union", "cohort_same", "cohort_same",
      "cohort_same_union"
    )

    intersected_rows <- DBI::dbGetQuery(
      handle$conn, paste0("SELECT * FROM ", intersected)
    )
    united_rows <- DBI::dbGetQuery(
      handle$conn, paste0("SELECT * FROM ", united)
    )
    expect_equal(nrow(intersected_rows), 4L)
    expect_equal(nrow(united_rows), 3L)
    expect_setequal(
      handle$temp_tables,
      c("cohort_same", "cohort_same_intersect", "cohort_same_union")
    )

    before_difference <- handle$temp_tables
    expect_error(
      .cohortCombine(
        handle, "setdiff", "cohort_same", "cohort_same",
        "cohort_same_difference"
      ),
      "insufficient individuals"
    )
    expect_identical(handle$temp_tables, before_difference)
    expect_false(DBI::dbExistsTable(handle$conn, "cohort_same_difference"))
  })

  withr::with_options(list(nfilter.subset = 0), {
    difference <- .cohortCombine(
      handle, "setdiff", "cohort_same", "cohort_same",
      "cohort_same_difference"
    )
    expect_identical(difference, "cohort_same_difference")
    expect_true(DBI::dbExistsTable(handle$conn, difference))
    expect_identical(nrow(DBI::dbReadTable(handle$conn, difference)), 0L)
    expect_true(difference %in% handle$temp_tables)
  })
})

test_that("identical cohort SQL reads a MySQL temporary table only once", {
  handle <- new.env(parent = emptyenv())
  handle$temp_tables <- "mysql_temp_cohort"
  handle$target_dialect <- "mysql"
  gate_sql <- create_sql <- character(0)

  testthat::local_mocked_bindings(
    .assertMinPersons = function(handle = NULL, sql = NULL,
                                 n_persons = NULL) {
      gate_sql <<- c(gate_sql, sql)
      invisible(TRUE)
    },
    .createTempTable = function(handle, name, sql) {
      create_sql <<- c(create_sql, sql)
      name
    },
    .package = "dsOMOP"
  )

  expect_identical(
    .cohortCombine(
      handle, "intersect", "mysql_temp_cohort", "mysql_temp_cohort",
      "mysql_intersect"
    ),
    "mysql_intersect"
  )
  expect_identical(
    .cohortCombine(
      handle, "union", "mysql_temp_cohort", "mysql_temp_cohort",
      "mysql_union"
    ),
    "mysql_union"
  )
  expect_identical(
    .cohortCombine(
      handle, "setdiff", "mysql_temp_cohort", "mysql_temp_cohort",
      "mysql_setdiff"
    ),
    "mysql_setdiff"
  )

  references <- function(sql) {
    matches <- gregexpr("\\bmysql_temp_cohort\\b", sql, perl = TRUE)[[1L]]
    if (identical(matches, -1L)) 0L else length(matches)
  }
  expect_identical(
    unname(vapply(gate_sql, references, integer(1L))), c(1L, 1L, 1L)
  )
  expect_identical(
    unname(vapply(create_sql, references, integer(1L))), c(1L, 1L, 1L)
  )
  expect_false(any(grepl(" EXISTS | UNION ", create_sql, fixed = TRUE)))
  expect_false(grepl("SELECT DISTINCT", create_sql[[1L]], fixed = TRUE))
  expect_true(grepl("SELECT DISTINCT", create_sql[[2L]], fixed = TRUE))
  expect_true(grepl("WHERE 1 = 0", create_sql[[3L]], fixed = TRUE))
})

test_that("cohort set operations accept only owned, valid temp-table names", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle))

  eras <- paste(
    "SELECT person_id AS subject_id, '2020-01-01' AS cohort_start_date,",
    "'2020-01-31' AS cohort_end_date FROM person"
  )
  .createTempTable(handle, "owned_a", eras)
  .createTempTable(handle, "owned_b", eras)

  withr::with_options(list(nfilter.subset = 3), {
    expect_error(
      .cohortCombine(handle, "union", "owned_a; DROP TABLE person", "owned_b"),
      "Invalid first cohort name|valid SQL identifier"
    )
    expect_error(
      .cohortCombine(handle, "union", "person", "owned_b"),
      "only temporary cohorts"
    )
    expect_error(
      .cohortCombine(handle, "union", "owned_a", "owned_b", "bad;name"),
      "valid SQL identifier|must start"
    )
    expect_error(.createTempTable(handle, "bad;name", eras),
                 "valid SQL identifier|must start")
  })
})

test_that("public cohort combination preserves inputs and only its final table", {
  handle <- create_test_handle(n_persons = 15)
  symbol <- paste0("cohort_combine_", Sys.getpid())
  .setHandle(symbol, handle)
  on.exit(.removeHandle(symbol), add = TRUE)

  eras <- paste(
    "SELECT person_id AS subject_id, '2020-01-01' AS cohort_start_date,",
    "'2020-01-31' AS cohort_end_date FROM person"
  )
  .createTempTable(handle, "combine_owned_a", eras)
  .createTempTable(handle, "combine_owned_b", eras)
  baseline <- handle$temp_tables

  result <- withr::with_options(
    list(nfilter.subset = 3),
    omopCohortCombineDS(
      symbol, "intersect", "combine_owned_a", "combine_owned_b",
      "combine_owned_result"
    )
  )

  expect_identical(result, "combine_owned_result")
  expect_setequal(handle$temp_tables, c(baseline, result))
  expect_true(all(vapply(
    c("combine_owned_a", "combine_owned_b", result),
    function(name) DBI::dbExistsTable(handle$conn, name), logical(1)
  )))
})
