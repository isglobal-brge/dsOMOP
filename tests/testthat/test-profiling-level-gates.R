# Counted releases follow the DataSHIELD table rule: values below nfilter.tab
# distinct persons are dropped or suppressed and released counts are banded.
# Since 2.10.6 the number of unreleased rare values never refuses the frequent
# ones (no level-count gate on value counts, cross-tabs or drilldowns).

.plg_frequent <- 4600001:4600045
.plg_rare <- 4600101:4600115

.plg_handle <- function(values = c(rep(.plg_frequent, each = 3L), .plg_rare)) {
  handle <- create_test_handle(n_persons = length(values))
  .buildBlueprint(handle)
  DBI::dbExecute(handle$conn, "DELETE FROM observation")
  DBI::dbWriteTable(handle$conn, "observation", data.frame(
    observation_id = seq_along(values), person_id = seq_along(values),
    observation_concept_id = 4500000L, observation_date = "2020-01-01",
    observation_type_concept_id = rep(c(32817L, 32818L), length.out = length(values)),
    value_as_concept_id = values), append = TRUE)
  handle
}

.plg_options <- list(nfilter.tab = 3, nfilter.subset = 3, nfilter.levels.max = 40,
                     nfilter.levels.density = 0.33)

test_that("value counts release frequent values whatever the number of rare ones", {
  handle <- .plg_handle()
  on.exit(cleanup_handle(handle))
  withr::with_options(.plg_options, {
    result <- .profileValueCounts(handle, "observation", "value_as_concept_id", top_n = 20)
    expect_identical(nrow(result), 20L)
    expect_true(all(as.integer(result$value) %in% .plg_frequent))
    expect_false(any(as.integer(result$value) %in% .plg_rare))
  })
})

test_that("value counts of a quasi-unique column release nothing instead of refusing", {
  handle <- .plg_handle(values = 4700001:4700120)
  on.exit(cleanup_handle(handle))
  withr::with_options(.plg_options, {
    result <- .profileValueCounts(handle, "observation", "value_as_concept_id")
    expect_identical(nrow(result), 0L)
  })
})

test_that("cross-tabs release only supported levels beyond nfilter.levels.max", {
  handle <- .plg_handle()
  on.exit(cleanup_handle(handle))
  withr::with_options(.plg_options, {
    result <- .profileCrossTab(handle, "observation", "value_as_concept_id",
                               "observation_type_concept_id")
    expect_setequal(as.integer(result$row_levels), .plg_frequent)
    expect_identical(dim(result$counts), c(length(.plg_frequent), 2L))
  })
})

test_that("drilldowns keep the categorical breakdown of supported values", {
  handle <- .plg_handle()
  on.exit(cleanup_handle(handle))
  withr::with_options(.plg_options, {
    result <- .profileConceptDrilldown(handle, "observation", 4500000L)
    values <- as.integer(result$categorical_values$value_as_concept_id)
    expect_setequal(values, .plg_frequent)
  })
})

test_that("legacy wide pivots name a rare concept's column under the level rule alone", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  df <- data.frame(measurement_id = 1:7, person_id = 1:7,
                   measurement_concept_id = c(rep(3004410L, 6L), 3004411L),
                   value_as_number = as.numeric(1:7))
  admitted <- function(levels_max) withr::with_options(list(nfilter.tab = 3, nfilter.levels.max = levels_max,
                                                            nfilter.levels.density = 0.5), {
    contract <- attr(.toWide(df, "measurement", handle), "omop_column_contract")
    named <- Filter(function(d) !is.null(attr(d, "concept_name")), contract)
    vapply(named, function(d) isTRUE(attr(d, "concept_name")$permitted), logical(1))
  })
  open <- admitted(40)
  expect_true(length(open) >= 2L && all(open))
  expect_false(any(admitted(1)))
})
