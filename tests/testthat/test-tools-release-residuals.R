# dsOMOP 2.10.3 residual demonstrations (spec 10.2, item 10). They document
# inference the deterministic constructions do NOT prevent, so the
# documentation can never claim these attacks impossible. They are expected to
# hold as written and must never be "fixed" with a query cap or with noise that
# grows after repeated calls. The fixed-synopsis recombination invariant at the
# end, in contrast, must hold under every supported partition. (Per-person
# additive or multiplicative noise shares were rejected in the design and have
# no implementation to demonstrate.) Helpers in helper-tools-release.R.

.resid_table <- function(ids, producer = "test/residual") {
  value <- .testPseudonymize(data.frame(person_id = as.integer(ids),
                                        x = 1L, stringsAsFactors = FALSE))
  .dsomopDpSealPersonLocal(value, producer = producer, dataset_identity = list(
    resource_identity = "residual-test", target_dialect = "postgresql",
    cdm_schema = "main", vocabulary_schema = "main", results_schema = "main"))
}

.resid_count <- function(ids) {
  out <- suppressMessages(omopDpReleaseDS(.resid_table(ids),
                                          list(statistic = "count")))
  out$components$signed[[1L]]
}

test_that("residual: equal realised maps reveal set equality across scopes", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  a <- .release_cohort(h, "residual_a", 1:40)
  b <- .release_cohort(h, "residual_b", 1:40)
  c <- .release_cohort(h, "residual_c", c(1:39, 41))
  # Signed components (displayed values of a 40-person scope are mostly
  # clipped to zero at the reference calibration).
  run <- function(scope) {
    out <- .release_run(h, "dsomop:achilles.104", scope = scope)
    lapply(attr(out, "dsomop_release")$owners, `[[`, "components")
  }
  # The same people under another scope name replay (by design)...
  expect_identical(run(a), run(b))
  # ...and one swapped person redraws: two answers reveal whether two scopes
  # contain the same people (a data-dependent equality bit).
  expect_false(identical(run(a), run(c)))
})

test_that("residual: distinct supported splits give independent noisy totals", {
  .release_state()
  total <- .resid_count(1:60)
  splits <- list(list(1:30, 31:60), list(seq(1, 59, 2), seq(2, 60, 2)),
                 list(1:20, 21:60))
  sums <- vapply(splits, function(s) .resid_count(s[[1L]]) +
                   .resid_count(s[[2L]]), numeric(1L))
  # Each split is another realised question with fresh components, so the
  # split sums are independent noisy estimates of the same total: averaging
  # many such splits is a documented residual of the heuristic route.
  expect_length(unique(c(total, sums)), 4L)
  # Every repeat of one split still replays exactly.
  expect_identical(.resid_count(1:30) + .resid_count(31:60), sums[[1L]])
})

test_that("residual: a window change across a public grid boundary is a new question", {
  .release_state()
  h <- create_test_handle(n_persons = 200)
  on.exit(cleanup_handle(h), add = TRUE)
  .release_dated_cohort(h, "residual_w", 1:80, start = "2021-01-01",
                        end = "2023-12-31")
  # Signed components (displayed person-time is clipped to n * 3650 here).
  run <- function(end) {
    out <- .release_run(h, "dsomop:incidence.rate", list(
      outcome_concept_id = "4000002", tar_end_offset = as.character(end)),
      scope = "residual_w")
    lapply(attr(out, "dsomop_release")$owners, `[[`, "components")
  }
  # Inside one public cell: one effective window, one answer...
  expect_identical(run(20), run(28))
  # ...across a cell boundary: another effective window and other components.
  expect_false(identical(run(28), run(31)))
})

test_that("invariant: fixed-synopsis views recombine to one basis under every partition", {
  .release_state()
  h <- create_test_handle(n_persons = 150)
  on.exit(cleanup_handle(h), add = TRUE)
  a3 <- .release_run(h, "dsomop:achilles.3")
  a10 <- .release_run(h, "dsomop:achilles.10")
  by_decade <- .release_run(h, "dsomop:person.gender_by_age_decade")
  years <- .release_run(h, "dsomop:person.year_of_birth_distribution")
  for (x in list(a3, a10, by_decade, years)) {
    expect_identical(attr(x, "dsomop_protection")$protection, "fixed_synopsis")
  }
  total <- sum(a3$count_value)
  expect_equal(sum(a10$count_value), total, tolerance = 1e-9)
  expect_equal(sum(by_decade$n_persons), total, tolerance = 1e-9)
  expect_equal(sum(years$n_persons), total, tolerance = 1e-9)
  # The gender margins agree whether summed over birth years or decades.
  g10 <- tapply(a10$count_value, a10$gender_concept_id, sum)
  gdec <- tapply(by_decade$n_persons, by_decade$gender_concept_id, sum)
  common <- intersect(names(g10), names(gdec))
  expect_gt(length(common), 0L)
  expect_equal(as.numeric(g10[common]), as.numeric(gdec[common]),
               tolerance = 1e-9)
  # Birth-year margins agree between Achilles 3 and Achilles 10.
  y10 <- tapply(a10$count_value, ifelse(is.na(a10$year_of_birth), "unknown",
                                         a10$year_of_birth), sum)
  y3 <- stats::setNames(a3$count_value, ifelse(is.na(a3$year_of_birth),
                                               "unknown", a3$year_of_birth))
  expect_equal(as.numeric(y10[names(y3)]), as.numeric(y3), tolerance = 1e-9)
})
