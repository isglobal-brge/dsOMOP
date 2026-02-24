# Extracted from test-profiling-drilldown.R:97

# test -------------------------------------------------------------------------
handle <- create_test_handle(n_persons = 2)
on.exit(cleanup_handle(handle))
.buildBlueprint(handle)
withr::with_options(list(nfilter.tab = 3, nfilter.subset = 5), {
    # Only 2 persons, need at least 5
    expect_error(
      .profileConceptDrilldown(handle, "condition_occurrence", 201820L),
      "Disclosive"
    )
  })
