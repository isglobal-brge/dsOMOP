test_that("harness contracts and changed R syntax hold", {
  withr::local_dir(testthat::test_path("..", ".."))
  source("scripts/tests/g-contract.R", local = TRUE)
  for (f in list.files("scripts", pattern = "[.]R$", recursive = TRUE, full.names = TRUE)) parse(f)
  expect_true(TRUE)
})
