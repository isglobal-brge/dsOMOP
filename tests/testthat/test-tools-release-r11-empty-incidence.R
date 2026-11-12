# R11: an empty eligible substrate must take the same complete protected
# release path as one or two eligible people under a supported cohort parent.
# These tests use the exported endpoint, production fingerprints and sampler.

test_that("R11: empty incidence keys are zero-row character columns", {
  expect_identical(.omopReleaseGenderKey(numeric(0)), character(0))
  expect_identical(.omopReleaseGenderKey(NULL), character(0))
  expected <- data.frame(gender = character(0), age5 = character(0),
                          year = character(0), stringsAsFactors = FALSE)
  expect_identical(.dsomopReleaseC6TreeKeys(numeric(0), numeric(0),
                                           numeric(0)), expected)
  expect_identical(.dsomopReleaseC6TreeKeys(NULL, NULL, NULL), expected)
})

test_that("R11: public incidence releases every view for zero, one and two eligible people", {
  .release_state()
  h <- create_test_handle(n_persons = 24)
  on.exit(cleanup_handle(h), add = TRUE)
  assign("handle_r11_incidence", h, envir = .dsomop_env)
  on.exit(rm(list = "handle_r11_incidence", envir = .dsomop_env), add = TRUE)
  .release_dated_cohort(h, "r11_parent", 1:24, "2021-01-01", "2022-01-01")
  before_parent <- DBI::dbReadTable(h$conn, "r11_parent")
  before_temps <- h$temp_tables
  temp_objects <- function() DBI::dbGetQuery(h$conn,
    "SELECT name, type FROM sqlite_temp_master ORDER BY name, type")
  before_objects <- temp_objects()
  fields <- c("persons_at_risk", "person_days", "outcomes", "person_outcomes")
  columns <- c("stratum", fields, "proportion", "rate")
  dimensions <- expand.grid(gender = 0:1, age = 0:1, year = 0:1,
                             KEEP.OUT.ATTRS = FALSE)
  gender_labels <- .dsomopToolsGenders()$gender_name
  year_labels <- c("before 1970", as.character(1970:2026), "unknown")
  views <- list()
  for (i in seq_len(nrow(dimensions))) {
    d <- dimensions[i, ]
    for (band in if (d$age == 1L) c(5L, 10L, 20L) else 10L) {
      labels <- list()
      if (d$gender == 1L) labels$gender <- gender_labels
      if (d$age == 1L) labels$age <- c(as.character(seq(0L, 110L, by = band)),
                                       "unknown")
      if (d$year == 1L) labels$year <- year_labels
      strata <- if (length(labels)) {
        do.call(paste, c(expand.grid(labels, KEEP.OUT.ATTRS = FALSE,
                                      stringsAsFactors = FALSE), sep = "|"))
      } else "all"
      views[[length(views) + 1L]] <- list(
        params = list(outcome_concept_id = "4000002",
                      strat_by_gender = as.character(d$gender),
                      strat_by_age = as.character(d$age),
                      strat_by_year = as.character(d$year),
                      age_band_years = as.character(band)),
        strata = strata)
    }
  }
  expect_length(views, 16L)
  for (n in 0:2) {
    DBI::dbExecute(h$conn, "DELETE FROM observation_period")
    if (n > 0L) {
      DBI::dbWriteTable(h$conn, "observation_period", data.frame(
        observation_period_id = seq_len(n), person_id = seq_len(n),
        observation_period_start_date = "2020-01-01",
        observation_period_end_date = "2024-12-31",
        period_type_concept_id = 44814724L), append = TRUE)
    }
    root_values <- NULL
    root_component <- NULL
    for (i in seq_along(views)) {
      view <- views[[i]]
      info <- paste("eligible people", n, "view", i)
      answer <- suppressWarnings(suppressMessages(omopAnalysisRunDS(
        "r11_incidence", "dsomop:incidence.rate", view$params, "r11_parent")))
      expect_s3_class(answer, "data.frame")
      expect_identical(names(answer), columns, info = info)
      # Every public cell survives, including the unknown/tail strata, with
      # no eligible-cell emptiness or subthreshold support gate.
      expect_identical(answer$stratum, view$strata, info = info)
      expect_true(all(is.finite(as.matrix(answer[fields]))), info = info)
      expect_true(all(as.matrix(answer[fields]) >= 0), info = info)
      label <- attr(answer, "dsomop_protection")
      expect_identical(label$protection, "realised_sticky", info = info)
      owner <- attr(answer, "dsomop_release")$owners[[1L]]
      expect_equal(length(owner$components), nrow(answer), info = info)
      if (i == 1L) {
        root_values <- vapply(answer[fields], sum, numeric(1L))
        root_component <- owner$hierarchy_nodes$root
      } else {
        expect_equal(vapply(answer[fields], sum, numeric(1L)), root_values,
                     tolerance = 1e-6, info = info)
        expect_identical(owner$hierarchy_nodes$root, root_component, info = info)
      }
      expect_null(h$read_snapshot)
      expect_identical(h$temp_tables, before_temps, info = info)
      expect_identical(temp_objects(), before_objects, info = info)
      expect_identical(DBI::dbReadTable(h$conn, "r11_parent"), before_parent,
                       info = info)
    }
  }

  # The existing final cohort-parent gate still refuses fewer than three
  # people, and its error also releases the request and any temporary tables.
  .release_dated_cohort(h, "r11_small_parent", 1:2,
                        "2021-01-01", "2022-01-01")
  before_temps <- h$temp_tables
  before_objects <- temp_objects()
  expect_error(suppressWarnings(suppressMessages(omopAnalysisRunDS(
    "r11_incidence", "dsomop:incidence.rate",
    list(outcome_concept_id = "4000002"), "r11_small_parent"))),
    "insufficient individuals to meet disclosure threshold")
  expect_null(h$read_snapshot)
  expect_identical(h$temp_tables, before_temps)
  expect_identical(temp_objects(), before_objects)
  expect_identical(DBI::dbReadTable(h$conn, "r11_parent"), before_parent)
})
