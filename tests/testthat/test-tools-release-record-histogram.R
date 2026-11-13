# Record summaries without a registered scale keep a protected value
# distribution (Astra re-review section 6 / R07), their strata are cells of a
# declared public domain (section 4), displayed records stay within
# [N, K N] and labels report the allocation union of the components actually
# drawn. Helpers in helper-tools-release.R.

.rh_measurements <- function(h, values, unit = 999001L, persons = NULL,
                             concept = 3004410L, first_id = 1L) {
  persons <- persons %||% seq_along(values)
  DBI::dbWriteTable(h$conn, "measurement", data.frame(
    measurement_id = first_id - 1L + seq_along(values), person_id = persons,
    measurement_concept_id = concept, measurement_date = "2021-03-01",
    measurement_type_concept_id = 44818518L, value_as_number = values,
    value_as_concept_id = NA_integer_, unit_concept_id = unit,
    range_low = NA_real_, range_high = NA_real_,
    visit_occurrence_id = NA_integer_), append = TRUE)
}

.rh_owner <- function(df) attr(df, "dsomop_release")$owners[[1L]]

.rh_components <- function(df) {
  comps <- .rh_owner(df)$components
  stats::setNames(comps, vapply(comps, `[[`, character(1L), "stratum"))
}

# Exact (zero-noise) release: every component is its exact value.
.rh_without_noise <- function(.local_envir = parent.frame()) {
  testthat::local_mocked_bindings(
    .dsomopReleaseNoisyComponent = function(policy, keys, protection,
                                            component) component$value,
    .env = .local_envir)
}

test_that("Astra section6: unregistered record summaries retain protected histogram", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM measurement")
  .rh_measurements(h, 1:60)
  o <- .release_run(h, "dsomop:measurement.value_stats",
                    list(concept_id = "3004410"))
  owners <- attr(o, "dsomop_release")$owners
  expect_gt(nrow(o), 0L)
  expect_true("mean_value" %in% names(o))
  expect_true(all(is.na(o$mean_value)))
  expect_true(any(vapply(owners, function(x) any(vapply(x$components,
    function(z) !is.null(z$h), logical(1))), logical(1))))
})

test_that("value histograms are reconciled to the protected record total", {
  .release_state()
  # A large calibration so the displayed persons pass the distribution floor.
  withr::local_options(list(dsomop.dp.release_epsilon = 8))
  .release_restart()
  h <- create_test_handle(n_persons = 80)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM measurement")
  .rh_measurements(h, c(seq(0.5, 300, length.out = 80), -7, 250000),
                   persons = c(1:80, 1, 2))
  out <- .release_run(h, "dsomop:measurement.value_stats",
                      list(concept_id = "3004410"))
  owner <- .rh_owner(out)
  expect_identical(owner$value_histogram, "record-value-histogram-v1")
  expect_identical(owner$value_grid, "signed-1-2-5-v1")
  expect_identical(owner$value_reconciliation,
                   "cells_sum_to_signed_record_total_R")
  expect_length(owner$value_cell_keys, length(owner$value_breaks))
  comp <- owner$components[[1L]]
  expect_length(comp$h, length(owner$value_cell_keys))
  # Signed cells sum to the signed record total R, never to N.
  expect_equal(sum(comp$h), comp$r, tolerance = 1e-9)
  label <- attr(out, "dsomop_protection")
  hist <- label$value_histograms
  expect_identical(unique(hist$unit_concept_id), 999001)
  # Explicit tails of the scale-free grid.
  expect_identical(hist$bin_label[[1L]], "(-Inf, -50000)")
  expect_identical(hist$bin_label[[nrow(hist)]], "[100000, Inf)")
  # Displayed cells add up to the displayed records of the row.
  expect_equal(sum(hist$records), out$n_values[[1L]], tolerance = 1e-6)
  expect_true(all(hist$records >= 0))
  # The moments stay unavailable without a registered scale.
  expect_true(is.na(out$mean_value) && is.na(out$sd_value))
  expect_identical(label$components[[1L]]$moments_unavailable$reason,
                   "public_scale_unavailable")
  # Interval quantiles from the displayed cells (N02).
  q <- label$value_quantiles
  expect_identical(q$prob, c(0.1, 0.25, 0.5, 0.75, 0.9))
  expect_true(all(q$value >= q$bin_lower - 1e-9 & q$value <= q$bin_upper))
  expect_true(all(c("open_lower", "open_upper", "censored") %in% names(q)))
  # The same question replays the same answer.
  again <- .release_run(h, "dsomop:measurement.value_stats",
                        list(concept_id = "3004410"))
  expect_identical(attr(again, "dsomop_protection")$value_histograms, hist)
})

test_that("without noise the value histogram is the capped record count per cell", {
  .release_state()
  .rh_without_noise()
  h <- create_test_handle(n_persons = 40)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM measurement")
  # Person 1 has 12 records: the whole-person cap keeps its first 10 (by
  # date, then measurement id) across all strata.
  values <- c(rep(3, 12), seq(10, 390, by = 10))
  persons <- c(rep(1L, 12), 2:40)
  .rh_measurements(h, values, persons = persons)
  out <- .release_run(h, "dsomop:measurement.value_stats",
                      list(concept_id = "3004410"))
  expect_equal(out$n_persons, 40)
  expect_equal(out$n_values, 10 + 39)
  hist <- attr(out, "dsomop_protection")$value_histograms
  grid <- .dsomopReleaseGrid("signed_125")
  kept <- c(rep(3, 10), seq(10, 390, by = 10))
  exact <- tabulate(.dsomopReleaseBinIndex(kept, grid), length(grid$breaks))
  expect_equal(hist$records, exact)
  q <- attr(out, "dsomop_protection")$value_quantiles
  expect_equal(q$value[[3L]], .dsomopReleaseQuantiles(exact, grid)[[3L]])
  # A registered stratum of the same owner draws moments, never a histogram.
  DBI::dbExecute(h$conn, "DELETE FROM measurement")
  .rh_measurements(h, c(seq(5, 9, length.out = 30), seq(10, 300,
                   length.out = 30)), persons = c(1:30, 1:30),
                   unit = c(rep(8554L, 30), rep(999001L, 30)))
  mixed <- .release_run(h, "dsomop:measurement.value_stats",
                        list(concept_id = "3004410"))
  comps <- .rh_components(mixed)
  expect_false(is.null(comps[["concept=3004410|unit=8554"]]$s))
  expect_null(comps[["concept=3004410|unit=8554"]]$h)
  expect_null(comps[["concept=3004410|unit=999001"]]$s)
  expect_false(is.null(comps[["concept=3004410|unit=999001"]]$h))
  label <- attr(mixed, "dsomop_protection")
  comp <- label$components[[1L]]
  expect_identical(names(comp$allocation_fraction), c("N", "R", "S", "Q", "H"))
  # Moments and histogram share the whole-person record cap: the union is
  # N + R + max(S + Q, H), not their sum.
  expect_equal(unlist(comp$union_epsilon),
               c(N = 0.02, R = 0.03, record_values = 0.05))
  expect_true(is.na(label$mechanism_epsilon))
  expect_identical(label$protection, "realised_sticky")
  expect_match(comp$joint_cap, "max\\(S \\+ Q, H\\)")
})

test_that("labels report only the component families actually drawn", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM measurement")
  .rh_measurements(h, 1:60)
  unscaled <- .release_run(h, "dsomop:measurement.value_stats",
                           list(concept_id = "3004410"))
  comp <- attr(unscaled, "dsomop_protection")$components[[1L]]
  expect_identical(names(comp$allocation_fraction), c("N", "R", "H"))
  expect_false(any(c("S", "Q") %in% names(comp$sensitivity)))
  expect_equal(comp$sensitivity$H, 10)
  DBI::dbExecute(h$conn, "DELETE FROM measurement")
  .rh_measurements(h, seq(4, 12, length.out = 60), unit = 8554L)
  scaled <- .release_run(h, "dsomop:measurement.value_stats",
                         list(concept_id = "3004410"))
  comp <- attr(scaled, "dsomop_protection")$components[[1L]]
  expect_identical(names(comp$allocation_fraction), c("N", "R", "S", "Q"))
  expect_null(attr(scaled, "dsomop_protection")$value_histograms)
  expect_null(.rh_owner(scaled)$value_histogram)
  # The export keeps the owner's static calibration for every family.
  expect_identical(names(.rh_owner(scaled)$allocation),
                   c("N", "R", "S", "Q", "H"))
})

test_that("displayed records stay within [N, K N] before the moments", {
  .release_state()
  h <- create_test_handle(n_persons = 40)
  on.exit(cleanup_handle(h), add = TRUE)
  route <- .omopToolsReleaseRoute(
    h, .omopToolsReleaseContracts()[["dsomop:fe.continuous"]], scoped = TRUE)
  shift <- c(N = 0, R = 0)
  testthat::local_mocked_bindings(
    .dsomopReleaseNoisyComponent = function(policy, keys, protection,
                                            component) {
      component$value + (shift[component$family] %|% 0)
    })
  `%|%` <- function(a, b) if (is.na(a)) b else a
  sub <- data.frame(person_id = rep(1:20, each = 2), stratum = "a",
                    stratum_order = 1, v = rep(c(10, 20), 20),
                    stringsAsFactors = FALSE)
  owner <- .dsomopReleaseRecordOwner("t", "t", function(s) NULL)
  shown <- function(r_shift) {
    shift[["R"]] <<- r_shift
    st <- .dsomopReleaseBuildRecordMoments(route$policy, route$protection,
                                           owner, sub)$strata
    c(n = st$n_display, r = st$r_display)
  }
  expect_equal(shown(0), c(n = 20, r = 40))
  expect_equal(shown(-35), c(n = 20, r = 20))
  expect_equal(shown(500), c(n = 20, r = 200))
  expect_equal(.dsomopReleaseRecordDisplay(-3, 5, 10), list(n = 0, r = 0))
})

test_that("record strata are cells of declared public domains with tails", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  withr::local_options(list(dsomop.dp.reference_year = 2026L))
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  starts <- c(rep("1850-06-01", 20), rep("2031-01-01", 20),
              rep("2010-01-01", 20))
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = 1:60, person_id = 1:60,
    observation_period_start_date = starts,
    observation_period_end_date = as.character(as.Date(starts) + 400),
    period_type_concept_id = 44814724L), append = TRUE)
  .rh_without_noise()
  out <- .release_run(h, "dsomop:observation_period.length_stats")
  # Out-of-domain years are public tail cells, never their private values.
  expect_setequal(out$start_year, c(1899, 2010, 2027))
  expect_false(any(out$start_year %in% c(1850, 2031)))
  strata <- vapply(.rh_owner(out)$components, `[[`, character(1L), "stratum")
  expect_setequal(strata, c("year=before-1900", "year=2010",
                            "year=after-2026"))
  label <- attr(out, "dsomop_protection")
  expect_identical(label$public_domain$kind, "start year")
  expect_match(label$public_domain$cells, "1900 to 2026")
  expect_length(label$public_domain$tails, 2L)
  expect_match(label$displayed_strata, "data-enumerated realised-sticky")
  expect_identical(label$protection, "realised_sticky")
  for (id in c("condition_era.length_stats", "drug.quantity_stats")) {
    domain <- .omopReleaseRecordDomain(.omopReleaseRecordStrataKind(
      .dsomopToolsReviewedQueries()[[id]]$summary))
    expect_identical(domain$kind, "concept", info = id)
  }
  expect_identical(.omopReleaseRecordDomain("concept_unit")$kind,
                   "concept x unit")
  expect_identical(.omopReleaseRecordDomain("concept_currency")$kind,
                   "concept x currency")
})

test_that("a person changes each value-histogram vector by at most the cap", {
  owner <- .dsomopReleaseRecordOwner("t", "t", function(s) NULL)
  sub <- data.frame(person_id = c(rep(1L, 10), rep(2:30, each = 2)),
                    stratum = c(rep("a", 6), rep("b", 4),
                                rep(c("a", "b"), 29)),
                    v = c(1:10, seq_len(58) * 3), stringsAsFactors = FALSE)
  with <- .dsomopReleaseRecordExactComponents(owner, sub)
  without <- .dsomopReleaseRecordExactComponents(owner,
                                                 sub[sub$person_id != 1L, ])
  change <- sum(vapply(names(with), function(k) {
    sum(abs(with[[k]]$h - (without[[k]]$h %||% 0)))
  }, numeric(1L)))
  expect_lte(change, 10)
  expect_equal(change, 10)
})


test_that("observed record strata never claim a complete fixed public basis", {
  .release_state()
  contracts <- .omopReleaseRecordSummaryContracts()
  expect_true(all(vapply(contracts, function(x) identical(x$fixed_basis, FALSE),
                         logical(1))))
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  .release_state(mode = "dp")
  expect_error(.release_run(h, "dsomop:measurement.value_stats",
                           list(concept_id = "3004410")),
               "fixed|synopsis|unsupported|eligible|exclusive")
})
