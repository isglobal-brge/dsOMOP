# Exercise the matching client with actual protected server endpoint payloads.
# rtest.sh installs the sibling client through RTEST_WITH for this test.
test_that("one-site incidence pooling reproduces the canonical hierarchy display", {
  skip_if_not_installed("dsOMOPClient")
  ns <- asNamespace("dsOMOPClient")
  if (!exists(".dp_pool_c6_hierarchy", envir = ns, inherits = FALSE)) {
    skip("requires matching round-3 hierarchy client")
  }
  .release_state()
  h <- create_test_handle(n_persons = 800)
  on.exit(cleanup_handle(h), add = TRUE)
  .release_dated_cohort(h, "client_tree", 1:600, "2021-01-01", "2023-12-31")
  .release_add_condition(h, 1:60, 4000002L, "2021-06-01")
  pool <- get(".dp_pool_c6_hierarchy", envir = ns)
  for (params in list(list(), list(strat_by_gender = "1"),
                      list(strat_by_gender = "1", strat_by_age = "1"),
                      list(strat_by_age = "1", strat_by_year = "1"))) {
    x <- .release_run(h, "dsomop:incidence.rate", c(list(
      outcome_concept_id = "4000002", tar_end_offset = "365"), params),
      scope = "client_tree")
    owner <- attr(x, "dsomop_release")$owners[[1L]]
    y <- pool(list(owner))
    expect_equal(y$persons, x$persons_at_risk, tolerance = 1e-6)
    expect_equal(y$events, x$person_outcomes, tolerance = 1e-6)
    expect_equal(y$person_time, x$person_days, tolerance = 1e-6)
    expect_equal(y$recurrent, x$outcomes, tolerance = 1e-6)
  }
})
