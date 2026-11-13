# N02 (Astra re-review): every protected quantile carries the public bin
# interval that contains it and open/censored flags, derived from the
# displayed histogram (no new noise), so an interpolated point never looks
# like an exact value or extreme. Helpers in helper-tools-release.R.

test_that("quantile bounds name each quantile's bin and its open tails", {
  bounded <- .dsomopReleaseGrid("age_5y")
  counts <- c(0, 10, 20, 10, rep(0, length(bounded$breaks) - 4L))
  q <- .dsomopReleaseQuantileBounds(counts, bounded)
  expect_identical(names(q), c("prob", "value", "bin_lower", "bin_upper",
                               "open_lower", "open_upper", "censored"))
  expect_equal(q$value, .dsomopReleaseRound(
    .dsomopReleaseQuantiles(counts, bounded)))
  expect_true(all(q$value >= q$bin_lower & q$value <= q$bin_upper))
  expect_equal(q$bin_lower[q$prob == 0.5], 10)
  expect_equal(q$bin_upper[q$prob == 0.5], 15)
  expect_false(any(q$open_lower | q$open_upper | q$censored))
  # A scale without a public lower bound: the first bin is the open
  # underflow bin; mass in the open last bin is censored at its finite edge.
  free <- .dsomopReleaseGrid("signed_125")
  tails <- numeric(length(free$breaks))
  tails[[1L]] <- 30
  tails[[length(tails)]] <- 30
  tails[[17L]] <- 40
  q <- .dsomopReleaseQuantileBounds(tails, free)
  low <- q[q$prob == 0.1, ]
  expect_identical(low$bin_lower, -Inf)
  expect_true(low$open_lower && low$censored)
  expect_equal(low$value, free$breaks[[2L]])
  high <- q[q$prob == 0.9, ]
  expect_identical(high$bin_upper, Inf)
  expect_true(high$open_upper && high$censored)
  expect_equal(high$value, free$breaks[[length(free$breaks)]])
  mid <- q[q$prob == 0.5, ]
  expect_false(mid$censored)
  # An empty histogram has no quantiles.
  empty <- .dsomopReleaseQuantileBounds(numeric(length(free$breaks)), free)
  expect_true(all(is.na(empty$value) & is.na(empty$censored)))
})

test_that("protected distributions publish quantile intervals in the label", {
  .release_state()
  h <- create_test_handle(n_persons = 2000)
  on.exit(cleanup_handle(h), add = TRUE)
  out <- .release_run(h, "dsomop:achilles.103")
  label <- attr(out, "dsomop_protection")
  qi <- label$components[[1L]]$quantile_intervals
  expect_s3_class(qi, "data.frame")
  expect_identical(unique(qi$stratum), "root")
  expect_identical(qi$prob, c(0.1, 0.25, 0.5, 0.75, 0.9))
  # The displayed quantile columns are the interval values of the label.
  expect_equal(qi$value, unname(unlist(out[1L, c("p10_value", "p25_value",
                                                 "median_value", "p75_value",
                                                 "p90_value")])))
  expect_true(all(qi$value >= qi$bin_lower & qi$value <= qi$bin_upper))
  expect_match(label$quantile_method, "quantile_intervals")
  # Stratified releases carry one interval set per released stratum.
  by_gender <- .release_run(h, "dsomop:achilles.104")
  qi <- attr(by_gender, "dsomop_protection")$components[[1L]]$quantile_intervals
  expect_gt(length(unique(qi$stratum)), 0L)
  expect_true(all(table(qi$stratum) == 5L))
  # Unavailable quantiles have no interval rows.
  expect_true(all(is.finite(qi$value)))
})
