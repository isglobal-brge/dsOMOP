# dsOMOP 2.10.9: a copy for tables where categories held by fewer than
# nfilter.tab people at this site are missing, with secondary suppression so
# that a total minus a table never gives a small count.

.small_frame <- function(person, ..., shared = TRUE) {
  columns <- list(...)
  frame <- data.frame(person_id = person)
  contracts <- list()
  for (col in names(columns)) {
    values <- columns[[col]]
    frame[[col]] <- .conceptFactor(values, levels = sort(unique(values[!is.na(values)])))
    contracts[[col]] <- list(ids = values, records = list(), declared = character())
  }
  class(frame) <- c("omop.table", "data.frame")
  attr(frame, "omop_concepts") <- contracts
  if (shared) attr(frame, "dsomop_levels_shared") <- TRUE
  frame
}
.small_counts <- function(f) as.list(table(f, useNA = "no"))

test_that("categories held by fewer than nfilter.tab people become missing", {
  withr::local_options(list(nfilter.tab = 3))
  cause <- c(rep("A", 5), rep("B", 2), "C", rep("D", 4))
  x <- .small_frame(seq_along(cause), cause = cause)
  out <- omopAsFactorColumnsDS(x, list(kind = "suppress_small", columns = list("cause")))
  expect_identical(levels(out$cause), levels(x$cause))
  expect_identical(class(out$cause), class(x$cause))
  expect_identical(unlist(.small_counts(out$cause)), c(A = 5L, B = 0L, C = 0L, D = 4L))
  expect_identical(sum(is.na(attr(out, "omop_concepts")$cause$ids)), 3L)
  expect_true(isTRUE(attr(out, "dsomop_levels_shared")))
})

test_that("secondary suppression keeps the suppressed total at nfilter.tab or more", {
  withr::local_options(list(nfilter.tab = 3))
  # Only B is small (one person): the next smallest category, D, goes too.
  cause <- c(rep("A", 5), "B", rep("D", 4))
  x <- .small_frame(seq_along(cause), cause = cause)
  out <- omopAsFactorColumnsDS(x, list(kind = "suppress_small", columns = list("cause")))
  expect_identical(unlist(.small_counts(out$cause)), c(A = 5L, B = 0L, D = 0L))
  # A tie goes to the first category in level order.
  cause <- c(rep("A", 4), "B", rep("D", 4))
  out <- omopAsFactorColumnsDS(.small_frame(seq_along(cause), cause = cause),
                               list(kind = "suppress_small", columns = list("cause")))
  expect_identical(unlist(.small_counts(out$cause)), c(A = 0L, B = 0L, D = 4L))
})

test_that("people are counted, not rows", {
  withr::local_options(list(nfilter.tab = 3))
  # E has five rows of one person: small. F has three people.
  person <- c(rep(1L, 5), 2:4, 5:9)
  cause <- c(rep("E", 5), rep("F", 3), rep("G", 5))
  out <- omopAsFactorColumnsDS(.small_frame(person, cause = cause),
                               list(kind = "suppress_small", columns = list("cause")))
  expect_identical(unlist(.small_counts(out$cause)), c(E = 0L, F = 0L, G = 5L))
})

test_that("joint suppression works on the cells of a two-way table", {
  withr::local_options(list(nfilter.tab = 3))
  x <- c(rep("a", 4), "a", rep("b", 3), rep("b", 2))
  y <- c(rep("u", 4), "v", rep("u", 3), rep("v", 2))
  frame <- .small_frame(seq_along(x), x = x, y = y)
  out <- omopAsFactorColumnsDS(frame, list(kind = "suppress_small", columns = list("x", "y"), joint = TRUE))
  cells <- table(out$x, out$y)
  expect_identical(as.vector(cells), c(4L, 3L, 0L, 0L))
  expect_identical(sum(is.na(out$x)), 3L)
  expect_identical(sum(is.na(out$y)), 3L)
  # Each column on its own keeps every value (no category is small).
  apart <- omopAsFactorColumnsDS(frame, list(kind = "suppress_small", columns = list("x", "y")))
  expect_false(anyNA(apart$x)); expect_false(anyNA(apart$y))
})

test_that("only reconciled outputs with a person key are suppressed", {
  withr::local_options(list(nfilter.tab = 3))
  cause <- c(rep("A", 5), "B")
  expect_error(omopAsFactorColumnsDS(.small_frame(seq_along(cause), cause = cause, shared = FALSE),
                                     list(kind = "suppress_small", columns = list("cause"))),
               "reconciled")
  expect_error(omopAsFactorColumnsDS(.small_frame(seq_along(cause), cause = cause),
                                     list(kind = "suppress_small", columns = list("cause")), "cause"),
               "no table column")
})
