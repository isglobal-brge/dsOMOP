test_that("profileTableStats returns row count", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    result <- .profileTableStats(handle, "person", stats = c("rows"))
    expect_true(!is.null(result$rows))
    expect_equal(result$rows, 15)  # 15 test persons
  })
})

test_that("profileTableStats returns distinct persons", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    result <- .profileTableStats(handle, "condition_occurrence",
                                  stats = c("persons"))
    expect_true(!is.null(result$persons))
    expect_true(result$persons > 0)
  })
})

test_that("profileTableStats suppresses small counts", {
  handle <- create_test_handle(n_persons = 2)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    result <- .profileTableStats(handle, "person", stats = c("rows"))
    expect_true(is.na(result$rows))
    expect_true(result$rows_suppressed)
  })
})

test_that("profileColumnStats returns statistics", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .profileColumnStats(handle, "measurement", "value_as_number")
  expect_true(!is.null(result$n_total))
  expect_true(!is.null(result$n_missing))
  expect_true(!is.null(result$n_distinct))
  expect_true(!is.null(result$min))
  expect_true(!is.null(result$max))
  expect_true(!is.null(result$mean))
})

test_that("profileColumnStats blocks sensitive columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .profileColumnStats(handle, "observation", "value_as_string"),
    "blocked"
  )
})

test_that("profileDomainCoverage returns coverage data", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    result <- .profileDomainCoverage(handle)
    expect_true(is.data.frame(result))
    expect_true("table_name" %in% names(result))
    expect_true("n_persons" %in% names(result))
    expect_true("person" %in% result$table_name)
  })
})

test_that("profileMissingness returns rates", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .profileMissingness(handle, "person")
  expect_true(is.data.frame(result))
  expect_true("column_name" %in% names(result))
  expect_true("missing_rate" %in% names(result))
  expect_true(all(result$missing_rate >= 0 & result$missing_rate <= 1))
})

test_that("profileMissingness excludes blocked columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  result <- .profileMissingness(handle, "observation")
  expect_false("value_as_string" %in% result$column_name)
})

test_that("profileValueCounts returns frequencies", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(
    nfilter.tab = 3,
    nfilter.levels.max = 40,
    nfilter.levels.density = 0.33
  ), {
    result <- .profileValueCounts(handle, "person", "gender_concept_id")
    expect_true(is.data.frame(result))
    expect_true("value" %in% names(result))
    expect_true("n" %in% names(result))
  })
})

test_that("profileValueCounts blocks sensitive columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .profileValueCounts(handle, "observation", "value_as_string"),
    "blocked"
  )
})
