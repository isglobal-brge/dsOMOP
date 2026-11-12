source(testthat::test_path("..", "..", "tools", "ws2", "ws2f-membership-results.R"), local = TRUE)

test_that("WS-2f B1 complete membership results retain the pristine baseline", {
  baseline <- readRDS(testthat::test_path("fixtures", "ws2f-membership-baseline.rds"))
  current <- .ws2f_membership_results()
  expect_identical(names(current), names(baseline))
  for (name in names(baseline)) expect_identical(current[[name]], baseline[[name]], info = name)
})

