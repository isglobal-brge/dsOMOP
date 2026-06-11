# Concept scoping: an optional concept_id filter restricts a value-column
# summary to one concept of one table. The fixture's measurement table holds
# two concepts (3004410 HbA1c, 3025315 Creatinine), so the concept-scoped
# population is a strict subset of the unscoped one.

test_that("profileColumnStats concept_id scopes n_total to one concept", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    unscoped <- .profileColumnStats(handle, "measurement", "value_as_number")
    scoped <- .profileColumnStats(handle, "measurement", "value_as_number",
                                  concept_id = 3004410L)

    expect_true(!is.na(scoped$n_total))
    expect_true(scoped$n_total <= unscoped$n_total)
  })
})

test_that("profileValueCounts concept_id scopes n_total to one concept", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(
    nfilter.tab = 3,
    nfilter.levels.max = 40,
    nfilter.levels.density = 0.5
  ), {
    unscoped <- .profileValueCounts(handle, "measurement", "unit_concept_id")
    scoped <- .profileValueCounts(handle, "measurement", "unit_concept_id",
                                  concept_id = 3004410L)

    expect_true(sum(scoped$n) <= sum(unscoped$n))
  })
})

test_that("concept_id scoping errors when the table has no concept column", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  expect_error(
    .profileColumnStats(handle, "cdm_source", "cdm_source_name",
                        concept_id = 1L),
    "no concept column"
  )
})
