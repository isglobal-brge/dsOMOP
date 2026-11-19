# dsOMOP 2.10.9: drilldown missingness carries the banded counts behind each
# rate, so that clients pool it as a ratio of sums (as omopMissingnessDS).

test_that("drilldown missingness releases the banded counts behind each rate", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  result <- withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3),
    .profileConceptDrilldown(handle, "condition_occurrence", 201820L))
  m <- result$missingness
  band <- max(as.integer(withr::with_options(list(nfilter.tab = 3, nfilter.subset = 3),
    .omopDisclosureSettings()$nfilter_band)), 1L)
  expect_true(all(c("column_name", "missing_rate", "n_total", "n_missing") %in% names(m)))
  # The total is the summary's banded record count; nothing new is released.
  expect_true(all(m$n_total == result$summary$n_records))
  expect_identical(is.na(m$n_missing), is.na(m$missing_rate))
  released <- !is.na(m$missing_rate)
  expect_true(any(released))
  expect_true(all(m$n_missing[released] %% band == 0))
  expect_equal(m$missing_rate[released], round(m$n_missing[released] / m$n_total[released], 2))
})
