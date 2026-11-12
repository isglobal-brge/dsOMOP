# WS9M1 B1: publication regressions for half-open integer64 ranges.
.between_frame <- function(v, name = "v") {
  x <- data.frame(person_id = seq_along(v))
  x[[name]] <- v
  .testPseudonymize(x)
}
.between_mut <- function(x, expr) omopMutateDS(x, list(version = 1L,
  assign = list(list(name = "answer", expr = expr))))$answer
.between_p <- function(var, op, value) list(var = var, op = op, value = value)

test_that("exclusive BETWEEN preserves bounds across integer storage and publication", {
  withr::local_options(list(nfilter.subset = 3, default.dsomop.dp.enabled = FALSE))
  raw <- c(1L, 1L, 1L, 2L, 2L, 2L, 3L, NA_integer_, 4L)
  pred <- .between_p("v", "between", list(lower = 1, upper = 2, upper_inclusive = FALSE))
  conjunction <- list(and = list(.between_p("v", ">=", 1), .between_p("v", "<", 2)))
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, NA, FALSE)
  for (storage in c("integer", "double", "integer64")) {
    value <- switch(storage, integer = raw, double = as.numeric(raw),
                    integer64 = bit64::as.integer64(raw))
    x <- .between_frame(value)
    actual <- .between_mut(x, list(pred = pred))
    expect_identical(actual, expected)
    expect_identical(actual, .between_mut(x, list(pred = conjunction)))
    rows <- omopFilterDS(x, expr = pred)
    expect_identical(rows$person_id, x$person_id[1:3])
    expect_identical(.between_mut(x, list(ifelse = list(test = pred,
      yes = list(lit = "inside"), no = list(lit = "outside")))),
      ifelse(expected, "inside", "outside"))
    expect_identical(.between_mut(x, list(case_when = list(cases = list(list(when = pred,
      then = list(lit = "inside"))), `else` = list(lit = "outside")))),
      ifelse(expected, "inside", "outside"))
    inclusive <- .between_p("v", "between", list(lower = 1, upper = 2, upper_inclusive = TRUE))
    expect_identical(.between_mut(x, list(pred = inclusive)), raw >= 1 & raw <= 2)
    inclusive$value$upper_inclusive <- NULL
    expect_identical(.between_mut(x, list(pred = inclusive)), raw >= 1 & raw <= 2)
  }
})

test_that("exclusive zero-width integer64 interval publishes no qualifying rows", {
  withr::local_options(list(nfilter.subset = 3, default.dsomop.dp.enabled = FALSE))
  x <- .between_frame(bit64::as.integer64(c(2L, 2L, 2L, 1L, NA_integer_)))
  pred <- .between_p("v", "between", list(lower = 2, upper = 2, upper_inclusive = FALSE))
  actual <- .between_mut(x, list(pred = pred))
  expect_identical(actual, c(FALSE, FALSE, FALSE, FALSE, NA))
  expect_error(omopFilterDS(x, expr = pred), "insufficient individuals",
               class = "dsomop_plan_rejection")
})

test_that("exclusive predicate on earlier exact integer assignment obeys its AST", {
  withr::local_options(list(nfilter.subset = 3, default.dsomop.dp.enabled = FALSE))
  x <- .between_frame(c(1L, 1L, 1L, 2L, 2L, 2L, NA_integer_))
  bound <- .Machine$integer.max * 2
  pred <- .between_p("big", "between", list(lower = .Machine$integer.max,
    upper = bound, upper_inclusive = FALSE))
  req <- list(version = 1L, assign = list(
    list(name = "big", expr = list(op = "*", args = list(list(col = "v"),
      list(lit = .Machine$integer.max, as = "integer")))),
    list(name = "answer", expr = list(pred = pred))))
  actual <- omopMutateDS(x, req)
  expect_s3_class(actual$big, "integer64")
  expect_identical(as.character(actual$big), c(rep("2147483647", 3), rep("4294967294", 3), NA))
  expect_identical(actual$answer, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA))
})

