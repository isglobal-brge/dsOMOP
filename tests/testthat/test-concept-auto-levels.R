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

# Review of 2.10.7 (19 Nov): each finding as a regression test.

.auto_plan_handle <- function(concepts, observed, n) {
  h <- create_test_handle(n_persons = n)
  .ws7_keys(list(h))
  DBI::dbWriteTable(h$conn, "concept", concepts, append = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  DBI::dbWriteTable(h$conn, "condition_occurrence", data.frame(condition_occurrence_id = seq_len(n), person_id = seq_len(n),
    condition_concept_id = rep_len(observed, n), condition_start_date = "2021-01-01", condition_end_date = "2021-01-02",
    condition_type_concept_id = 32817L, visit_occurrence_id = NA_integer_), append = TRUE)
  DBI::dbWriteTable(h$conn, "cohort", data.frame(cohort_definition_id = 77L, subject_id = seq_len(n),
    cohort_start_date = "2020-01-01", cohort_end_date = "2024-12-31"), append = TRUE)
  h$blueprint <- NULL
  h
}
.auto_concepts <- function(ids, vocabulary = "AutoFixture", invalid = NA_character_, standard = "S")
  data.frame(concept_id = ids, concept_name = paste("Fixture", ids), domain_id = "Condition",
             vocabulary_id = vocabulary, concept_class_id = "Cause", standard_concept = standard,
             concept_code = as.character(ids), valid_start_date = "2000-01-01", valid_end_date = "2099-12-31",
             invalid_reason = invalid)
.auto_events_plan <- function(dictionary = FALSE) list(
  cohort = list(type = "cohort_table", cohort_definition_id = 77),
  outputs = c(list(events = list(type = "event_level", table = "condition_occurrence",
                                 columns = c("condition_concept_id"), representation = list(format = "long"))),
              if (dictionary) list(dictionary = list(type = "concept_dictionary", source_outputs = "events"))),
  options = list(translate_concepts = FALSE, block_sensitive = TRUE))

test_that("kinds and remaining observed values fit the rule together, once", {
  # 30 valid concepts, each the only one of its kind, and 30 deprecated ones.
  single <- 950001:950030; invalid <- 960001:960030
  concepts <- rbind(.auto_concepts(single, vocabulary = paste0("V", seq_along(single))),
                    .auto_concepts(invalid, vocabulary = "Old", invalid = "D", standard = NA_character_))
  for (auto in c(0, 1000)) {
    h <- .auto_plan_handle(concepts, c(single, invalid), 100L)
    withr::local_options(list(nfilter.subset = 3, nfilter.tab = 3, nfilter.levels.max = 40,
                              nfilter.levels.density = 0.33, dsomop.auto_declared_levels_max = auto))
    result <- .planExecute(h, .auto_events_plan(), list(events = "E"))
    report <- .conceptLevelReport(result$events)
    expect_identical(report$unsafe, "condition_concept_id", info = auto)
    expect_null(report$levels$condition_concept_id)
    cleanup_handle(h)
  }
})

test_that("a dictionary names observed concepts only when they pass the rule on their own", {
  ids <- 930001:930070
  for (auto in c(1000, 0)) {
    h <- .auto_plan_handle(.auto_concepts(ids), ids[1:60], 200L)
    withr::local_options(list(nfilter.subset = 3, nfilter.tab = 3, nfilter.levels.max = 40,
                              nfilter.levels.density = 0.33, dsomop.auto_declared_levels_max = auto))
    result <- .planExecute(h, .auto_events_plan(dictionary = TRUE), list(events = "E", dictionary = "D"))
    events <- .conceptLevelReport(result$events)
    # With the automatic list the source publishes its 70 public levels; without
    # it the source is refused. Either way the dictionary must not list the 60.
    if (auto > 0) expect_length(events$levels$condition_concept_id, 70L) else
      expect_identical(events$unsafe, "condition_concept_id")
    dictionary <- .conceptLevelReport(result$dictionary)
    named <- as.character(unlist(dictionary$levels, use.names = FALSE))
    expect_false(any(as.character(ids[1:60]) %in% named), info = auto)
    expect_identical(nrow(result$dictionary), 0L)
    cleanup_handle(h)
  }
  # Few observed concepts still pass and are named, as before.
  h <- .auto_plan_handle(.auto_concepts(ids), ids[1:10], 200L)
  on.exit(cleanup_handle(h), add = TRUE)
  withr::local_options(list(nfilter.subset = 3, nfilter.tab = 3, nfilter.levels.max = 40, nfilter.levels.density = 0.33))
  result <- .planExecute(h, .auto_events_plan(dictionary = TRUE), list(events = "E", dictionary = "D"))
  expect_setequal(as.character(result$dictionary$concept_id), as.character(ids[1:10]))
})

test_that("level_range calendars and long vocabulary lookups", {
  withr::local_options(list(nfilter.levels.max = 40, nfilter.levels.density = 0.33,
                            dsomop.auto_declared_levels_max = NULL))
  days <- as.Date("2015-01-10") + seq(0, 3 * 365, by = 20)
  ranged <- .datePresent(list(lo = days, hi = days),
    list(precision = "month", representation = "factor", level_range = list("2016-01-01", "2016-12-31")))
  expect_null(.dateAutoDeclared(ranged, length(days)))
  h <- create_test_handle(); on.exit(cleanup_handle(h), add = TRUE)
  ids <- 970001:971500
  .auto_vocabulary(h, ids)
  records <- .conceptVocabulary(h, as.character(ids))
  expect_length(records, 1500L)
  expect_true(all(vapply(records, function(r) identical(r$origin, "vocabulary"), logical(1))))
})

test_that("encoded arguments longer than a million characters decode whole", {
  payload <- list(a = strrep("x", 2e6))
  json <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE))
  b64 <- gsub("=+$", "", gsub("/", "_", gsub("\\+", "-", gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json))))))
  expect_identical(nchar(.ds_arg(paste0("B64:", b64))$a), 2e6L)
})
