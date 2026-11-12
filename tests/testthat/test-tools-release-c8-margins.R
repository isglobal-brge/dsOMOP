test_that("C8 display repairs margins after a slow IPF convergence", {
  Y <- diag(1e4, 2)
  rows <- c(1, 1)
  cols <- c(1.0001, 0.9999)
  for (iterations in c(1L, 2000L)) {
    Z <- .dsomopReleaseTwoWayDisplay(Y, rows, cols, iterations = iterations)
    expect_true(all(Z >= 0))
    expect_equal(rowSums(Z), rows, tolerance = 1e-12)
    expect_equal(colSums(Z), cols, tolerance = 1e-12)
    expect_true(all(Z <= outer(rows, cols, pmin) + 1e-12))
  }
  # Empty displayed strata and status cells stay structurally zero.
  Y <- matrix(c(100, -100, 1, 2, -3, 100), 2, 3)
  Z <- .dsomopReleaseTwoWayDisplay(Y, c(3, 0), c(0, 1, 2), iterations = 1L)
  expect_true(all(Z >= 0))
  expect_equal(rowSums(Z), c(3, 0), tolerance = 1e-12)
  expect_equal(colSums(Z), c(0, 1, 2), tolerance = 1e-12)
  expect_identical(as.numeric(Z[2, ]), c(0, 0, 0))
  expect_identical(as.numeric(Z[, 1]), c(0, 0))
  expect_equal(.dsomopReleaseRepairColumns(Z, colSums(Z)), Z)
})
