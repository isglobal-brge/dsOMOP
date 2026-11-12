test_that("assignment and aggregate admission match the frozen column corpus", {
  corpus <- utils::read.csv(testthat::test_path("..", "..", "inst", "column_classes.csv"),
                            stringsAsFactors = FALSE)
  for (i in seq_len(nrow(corpus))) {
    column <- corpus$column[[i]]
    untyped <- identical(corpus$context[[i]], "extension") &&
      length(.untypedIdentifierColumns(column, reviewed = .PERSON_KEY_COLS())) > 0L
    aggregate_blocked <- .detectSensitiveColumns(column) || untyped
    metadata <- data.frame(column_name = column, is_blocked = aggregate_blocked,
                            is_untyped_identifier = untyped)
    blocked <- .assignmentBlockedColumn(column, metadata)
    assignment_class <- if (blocked) 1L else if (.assignmentQuasiIdentifier(column)) 2L else 3L
    expect_identical(assignment_class, corpus$assignment_class[[i]], info = column)
    expect_identical(if (aggregate_blocked) "blocked" else "admitted",
                     corpus$aggregate_admission[[i]], info = column)
  }
  expect_true(.assignmentBlockedColumn("birth_date_source_value"))
  expect_true(.assignmentBlockedColumn("mrn_birth_date"))
})

test_that("assigned age bands are pure and the aggregate policy is unchanged", {
  withr::with_options(list(nfilter.tab = 100L, dsomop.nfilter.age_range = 10L,
                          dsomop.nfilter.date_range = 90L), {
    expect_equal(.bandAges(c(17, 18, 19, 40, NA, -1), c(0, 18, 19, 40)),
                 c("0-17", "18-18", "19-39", "40+", NA, NA))
    expect_equal(.bandAges(23), "20-24")
    expect_error(.computeAgeGroups(2000, 2023, age_breaks = c(0, 18, 19, 40)),
                 "coarsening")
    expect_true(is.na(.computeAgeGroups(2000, 2023)))
  })
  expect_error(.bandAges(20, c(0, 18, 18)), "strictly increasing")
  expect_error(.bandAges(20, c(0, 18.5, 40)), "integer")
})

test_that("baseline keeps requested birth components and only drops internally added birth year", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  cohort <- .resolveCohortTable(handle, 1L)
  withr::with_options(list(nfilter.subset = 3L, nfilter.tab = 100L), {
    birth <- .extractBaseline(handle, cohort,
      columns = c("year_of_birth", "month_of_birth", "day_of_birth"),
      derived = "age_at_index", age_breaks = c(0L, 18L, 41L, 42L, 80L),
      translate_concepts = FALSE)
    expect_true(all(c("year_of_birth", "month_of_birth", "day_of_birth") %in% names(birth)))
    expect_false(anyNA(birth$age_group))
    published <- .publishStandalone(.testPseudonymize(birth), already_tokenized = TRUE)
    expect_identical(published$year_of_birth, birth$year_of_birth)
    private_birth <- .extractBaseline(handle, cohort, columns = "gender_concept_id",
                                      derived = "age_at_index")
    expect_false("year_of_birth" %in% names(private_birth))
    expect_match(.compileSelect(handle, "person", columns = "year_of_birth"),
                 "year_of_birth", fixed = TRUE)
    expect_error(.compileSelect(handle, "observation", columns = "value_as_string"),
                 "blocked")
    expect_error(.profileColumnStats(handle, "person", "year_of_birth"), "blocked")
  })
})

test_that("cohort numeric bins accept ordinary bounds and authenticate a supplied scope", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  spec <- list(type = "measurement", concept_set = 3004410L,
               value_bin = list(lower = 6, upper = 10))
  withr::with_options(list(nfilter.subset = 3), {
    cohort <- .cohortCreate(handle, spec, cohort_id = 734L)
    rows <- .executeQuery(handle, paste0("SELECT DISTINCT subject_id FROM ", cohort))
    expect_setequal(rows$subject_id, c(1L, 3L, 5L, 7L, 9L))
    spec$value_bin$safe_scope <- list(table = "measurement", column = "value_as_number", n_bins = 3L)
    expect_error(.cohortCreate(handle, spec, cohort_id = 735L), "not issued")
  })
})
