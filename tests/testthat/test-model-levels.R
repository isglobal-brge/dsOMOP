# dsOMOP 2.10.7: levels in use of published factor columns, and the copy for
# models that keeps only them. Both follow the DataSHIELD level rule, so the
# copy can never answer, level by level, whether a level is in use.

.ml_frame <- function(used, levels, n = 200L) {
  values <- rep_len(used, n)
  frame <- data.frame(person_id = sprintf("p%03d", seq_len(n)), stringsAsFactors = FALSE)
  frame$cause <- .conceptFactor(values, levels)
  frame$age <- seq_len(n)
  class(frame) <- c("omop.table", class(frame))
  attr(frame, "omop_concepts") <- list(cause = list(ids = values))
  attr(frame, "omop_concept_cols") <- "cause"
  attr(frame, "dsomop_levels_shared") <- TRUE
  frame
}

test_that("levels in use are reported under the rule, without counts", {
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 0.33))
  frame <- .ml_frame(c("a", "b"), c("a", "b", "c", "d"))
  report <- omopFactorLevelsDS(frame, list(columns = list("*")))
  expect_identical(report$levels$cause, c("a", "b", "c", "d"))
  expect_identical(report$occurring$cause, c("a", "b"))
  expect_length(report$unsafe, 0L)
  expect_false(any(c("person_id", "age") %in% names(report$levels)))
  wide <- .ml_frame(sprintf("l%02d", 1:50), sprintf("l%02d", 1:60))
  report <- omopFactorLevelsDS(wide, list(columns = list("cause")))
  expect_identical(report$unsafe, "cause")
  expect_identical(report$unsafe_rules$cause, "nfilter.levels.max")
  expect_null(report$occurring$cause)
  expect_error(omopFactorLevelsDS(frame, list(columns = list("person_id"))), "factor columns only")
  expect_error(omopFactorLevelsDS(as.data.frame(unclass(frame)), list(columns = list("*"))),
               "dsOMOP outputs only")
})

test_that("the copy keeps only given levels, and refuses like the report would", {
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 0.33))
  frame <- .ml_frame(c("a", "b"), c("a", "b", "c", "d"))
  spec <- function(columns) list(kind = "drop_levels", columns = columns)
  copy <- omopAsFactorColumnsDS(frame, spec(list(cause = list("a", "b", "c"))))
  expect_identical(levels(copy$cause), c("a", "b", "c"))
  expect_s3_class(copy$cause, "dsomop_concept_factor")
  expect_identical(as.character(copy$cause), as.character(frame$cause))
  expect_null(attr(copy, "omop_concepts")$cause)
  expect_false(is.null(attr(copy, "omop_concepts")))
  expect_null(attr(copy, "omop_concept_cols"))
  expect_identical(copy$age, frame$age)
  expect_error(omopAsFactorColumnsDS(frame, spec(list(cause = list("a", "c")))), "hold every value")
  expect_error(omopAsFactorColumnsDS(frame, spec(list(cause = list("a", "b", "z")))), "existing levels")
  expect_error(omopAsFactorColumnsDS(frame, spec(list(person_id = list("p001")))), "factor columns only")
  # Levels in use that fail the rule are refused whatever the kept set, so a
  # refusal never tells which one level is in use.
  wide <- .ml_frame(sprintf("l%02d", 1:50), sprintf("l%02d", 1:60))
  for (keep in list(sprintf("l%02d", 1:60), sprintf("l%02d", 2:60)))
    expect_error(omopAsFactorColumnsDS(wide, spec(list(cause = as.list(keep)))), "factor-level rule")
  expect_identical(omopAsFactorColumnsDS(frame, spec(list()))$cause, frame$cause)
})

test_that("raw outputs before reconciliation release nothing, whatever is asked", {
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 0.33))
  raw <- .ml_frame(sprintf("l%02d", 1:50), sprintf("l%02d", 1:50))
  attr(raw, "dsomop_levels_shared") <- NULL
  expect_error(omopFactorLevelsDS(raw, list(columns = list("*"))), "reconciled")
  spec <- function(keep) list(kind = "drop_levels", columns = list(cause = as.list(keep)))
  # The same refusal whether a probed level exists or not: no oracle.
  for (keep in list(sprintf("l%02d", 1:50), c(sprintf("l%02d", 1:49), "zz")))
    expect_error(omopAsFactorColumnsDS(raw, spec(keep)), "reconciled")
})

test_that("a model copy carries no DP lineage of its source", {
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 0.33))
  frame <- .ml_frame(c("a", "b"), c("a", "b", "c"))
  attr(frame, "dsomop_dp_provenance") <- list(mac = "stale")
  attr(frame, "dsomop_dp_filter_state") <- list(state = 1)
  copy <- omopAsFactorColumnsDS(frame, list(kind = "drop_levels", columns = list(cause = list("a", "b"))))
  expect_null(attr(copy, "dsomop_dp_provenance"))
  expect_null(attr(copy, "dsomop_dp_filter_state"))
})
