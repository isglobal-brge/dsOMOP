# dsOMOP 2.10.7: the public vocabulary list a concept column declares when its
# observed levels fail the DataSHIELD rule. Only the kinds of the observed
# values (vocabulary, domain, class, standard flag) come from the data, and
# they pass the same rule; the list is read from the vocabulary tables.

.auto_vocabulary <- function(h, ids, vocabulary = "AutoFixture", standard = "S", invalid = NA_character_) {
  DBI::dbWriteTable(h$conn, "concept", data.frame(concept_id = ids, concept_name = paste("Fixture", ids),
    domain_id = "Condition", vocabulary_id = vocabulary, concept_class_id = "Cause",
    standard_concept = standard, concept_code = as.character(ids), valid_start_date = "2000-01-01",
    valid_end_date = "2099-12-31", invalid_reason = invalid), append = TRUE)
}

test_that("the declared list holds every valid concept of the observed kinds only", {
  h <- create_test_handle(); on.exit(cleanup_handle(h), add = TRUE)
  .auto_vocabulary(h, 930001:930076)
  .auto_vocabulary(h, 930077L, invalid = "D")
  .auto_vocabulary(h, 930078L, standard = NA_character_)
  .auto_vocabulary(h, 930079L, vocabulary = "OtherFixture")
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 0.33,
                            dsomop.auto_declared_levels_max = NULL))
  observed <- as.character(930001:930050)
  expect_setequal(.conceptAutoDeclared(h, observed, 500), as.character(930001:930076))
  # A second kind among the observed values brings its own valid concepts.
  expect_setequal(.conceptAutoDeclared(h, c(observed, "930079"), 500), as.character(c(930001:930076, 930079)))
  # Unknown, invalid and non-ID values add no kind; non-ID text declares nothing.
  expect_setequal(.conceptAutoDeclared(h, c(observed, "930077", "999999999"), 500), as.character(930001:930076))
  expect_length(.conceptAutoDeclared(h, c(observed, "Cause 1"), 500), 0L)
})

test_that("observed kinds pass the level rule, and long lists are reported, not declared", {
  h <- create_test_handle(); on.exit(cleanup_handle(h), add = TRUE)
  for (i in 1:45) .auto_vocabulary(h, 940000L + i, vocabulary = paste0("Fixture", i))
  .auto_vocabulary(h, 930001:930076)
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 0.33))
  many <- .conceptAutoDeclared(h, as.character(940001:940045), 500)
  expect_length(many, 0L)
  expect_null(attr(many, "too_long"))
  # Density: one kind over two rows is a level release the rule refuses.
  expect_length(.conceptAutoDeclared(h, as.character(930001:930050), 2), 0L)
  withr::local_options(list(dsomop.auto_declared_levels_max = 50))
  long <- .conceptAutoDeclared(h, as.character(930001:930050), 500)
  expect_length(long, 0L)
  expect_true(isTRUE(attr(long, "too_long")))
  withr::local_options(list(dsomop.auto_declared_levels_max = 0))
  off <- .conceptAutoDeclared(h, as.character(930001:930050), 500)
  expect_length(off, 0L)
  expect_null(attr(off, "too_long"))
  expect_identical(.omopDisclosureSettings()$auto_declared_levels_max, 0)
  withr::local_options(list(dsomop.auto_declared_levels_max = -1))
  expect_error(.omopDisclosureSettings(), "auto_declared_levels_max")
})

test_that("calendars declare every period of their observed years under the rule", {
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 0.33,
                            dsomop.auto_declared_levels_max = NULL))
  days <- as.Date("2015-01-10") + seq(0, 3 * 365, by = 20)
  spec <- list(precision = "month", representation = "factor")
  x <- .datePresent(list(lo = days, hi = days), spec)
  out <- .dateAutoDeclared(x, length(days))
  expect_s3_class(out, "dsomop_date_factor")
  expect_identical(levels(out), sprintf("%04d-%02d", rep(2015:2017, each = 12L), rep(1:12, 3L)))
  expect_identical(as.character(out), as.character(x))
  expect_setequal(attr(out, "omop_date_contract")$declared, levels(out))
  # The years themselves follow the rule: here over too few rows.
  expect_null(.dateAutoDeclared(x, 5L))
  # Weeks keep their own week-years, including week 53.
  spec$precision <- "iso_week"
  w <- .dateAutoDeclared(.datePresent(list(lo = days, hi = days), spec), length(days))
  expect_true(all(c("2015-W53", "2016-W01", "2017-W52") %in% levels(w)))
  expect_false(any(substr(levels(w), 1L, 4L) %in% c("2014", "2018")))
  withr::local_options(list(dsomop.auto_declared_levels_max = 100))
  expect_true(isTRUE(attr(.dateAutoDeclared(.datePresent(list(lo = days, hi = days), spec), length(days)), "too_long")))
  # Days and custom patterns are not completed.
  expect_null(.dateAutoDeclared(.datePresent(list(lo = days, hi = days),
    list(precision = "day", representation = "factor")), length(days)))
})
