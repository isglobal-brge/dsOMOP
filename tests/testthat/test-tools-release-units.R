# Unit-aware continuous measurements (dsOMOP 2.10.3, review B13).

# HbA1c records for persons 1..30: value 1 in percent (8554) and value 100 in
# an unregistered unit (999001) for every person.
.units_two_unit_fixture <- function(h) {
  DBI::dbExecute(h$conn, "DELETE FROM measurement")
  DBI::dbWriteTable(h$conn, "measurement", data.frame(
    measurement_id = 1:60, person_id = rep(1:30, 2),
    measurement_concept_id = 3004410L, measurement_date = "2021-03-01",
    measurement_type_concept_id = 44818518L,
    value_as_number = rep(c(1, 100), each = 30),
    value_as_concept_id = NA_integer_,
    unit_concept_id = rep(c(8554L, 999001L), each = 30),
    range_low = NA_real_, range_high = NA_real_,
    visit_occurrence_id = NA_integer_), append = TRUE)
}

.units_owners <- function(df) attr(df, "dsomop_release")$owners

.units_strata <- function(owner) {
  vapply(owner$components, `[[`, character(1L), "stratum")
}

test_that("protected values never average incompatible units (B13)", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  .units_two_unit_fixture(h)
  a <- .release_cohort(h, "units_scope", 1:30)
  # The reducer is per person within concept x unit: never the 50.5 mix.
  raw <- .omopReleaseConceptUnitValues(h, list(scoped_cohort = a), "3")
  expect_equal(nrow(raw), 60L)
  expect_setequal(unique(raw$unit_concept_id), c(8554, 999001))
  expect_true(all(raw$v[raw$unit_concept_id == 8554] == 1))
  expect_true(all(raw$v[raw$unit_concept_id == 999001] == 100))
  expect_false(any(raw$v == 50.5))

  params <- list(metric = "measurement_value", domain_code = "3")
  out <- .release_run(h, "dsomop:fe.continuous", params, scope = a)
  expect_identical(attr(out, "dsomop_protection")$protection,
                   "realised_sticky")
  expect_equal(nrow(out), 2L)
  expect_true(all(c("unit_concept_id", "unit_name") %in% names(out)))
  expect_true(all(out$covariate_id == 3004410))
  expect_setequal(out$unit_concept_id, c(8554, 999001))
  # The registered HbA1c percent scale owns bounded moments; the unknown unit
  # 999001 keeps a coarse histogram and no moment component at all.
  owners <- .units_owners(out)
  registered <- Filter(function(o) !is.null(o$bounds), owners)
  unregistered <- Filter(function(o) is.null(o$bounds), owners)
  expect_length(registered, 1L)
  expect_length(unregistered, 1L)
  expect_equal(registered[[1L]]$bounds, c(0, 20))
  expect_identical(.units_strata(registered[[1L]]),
                   "concept=3004410|unit=8554")
  expect_identical(.units_strata(unregistered[[1L]]),
                   "concept=3004410|unit=999001")
  comp <- registered[[1L]]$components[[1L]]
  expect_false(is.null(comp$s))
  expect_false(is.null(comp$q))
  expect_null(unregistered[[1L]]$components[[1L]]$s)
  expect_null(unregistered[[1L]]$components[[1L]]$q)
  unknown <- out[out$unit_concept_id == 999001, ]
  expect_true(is.na(unknown$avg_value) && is.na(unknown$stdev_value))
  # The displayed percent mean is post-processing of its components only.
  row <- out[out$unit_concept_id == 8554, ]
  if (!is.na(row$avg_value)) {
    p <- .dsomopReleaseProjectMoments(comp$s / 65535, comp$q / 65535,
                                      row$count_value)
    expect_equal(row$avg_value, 10 + 10 * p[["S"]] / row$count_value,
                 tolerance = 1e-6)
  }
  # The estimands name the unit discipline.
  label <- attr(out, "dsomop_protection")
  estimands <- vapply(label$components, `[[`, character(1L), "estimand")
  expect_true(any(grepl("in unit 8554", estimands, fixed = TRUE)))
  expect_true(any(grepl("no registered public scale", estimands,
                        fixed = TRUE)))

  # The standard kernel keeps the two units apart as well.
  std <- .omopCovariateContinuous(h, list(scoped_cohort = a),
                                  domain_code = "3", value_kind = "value",
                                  top_n = 50L)
  expect_setequal(std$unit_concept_id, c(8554, 999001))
  expect_equal(std$avg_value[std$unit_concept_id == 8554], 1)
  expect_equal(std$avg_value[std$unit_concept_id == 999001], 100)
})

test_that("only registry-declared conversions merge units (B13)", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM measurement")
  # HbA1c 7 % for persons 1..15 and 53 mmol/mol (9579) for persons 16..30.
  DBI::dbWriteTable(h$conn, "measurement", data.frame(
    measurement_id = 1:30, person_id = 1:30,
    measurement_concept_id = 3004410L, measurement_date = "2021-03-01",
    measurement_type_concept_id = 44818518L,
    value_as_number = c(rep(7, 15), rep(53, 15)),
    value_as_concept_id = NA_integer_,
    unit_concept_id = c(rep(8554L, 15), rep(9579L, 15)),
    range_low = NA_real_, range_high = NA_real_,
    visit_occurrence_id = NA_integer_), append = TRUE)
  a <- .release_cohort(h, "units_convert", 1:30)
  plain <- .omopReleaseConceptUnitValues(h, list(scoped_cohort = a), "3")
  expect_setequal(unique(plain$unit_concept_id), c(8554, 9579))
  builtin <- .dsomopReleaseMeasurementScales()[["concept=3004410|unit=8554"]]

  withr::local_options(list(dsomop.release.measurement_scales = list(list(
    concept_id = 3004410, unit_concept_id = 8554, lower = 0, upper = 20,
    label = "HbA1c (NGSP percent)",
    convert = list(list(unit_concept_id = 9579, factor = 0.09148,
                        offset = 2.152))))))
  scales <- .dsomopReleaseMeasurementScales()
  custom <- scales[["concept=3004410|unit=8554"]]
  expect_length(custom$convert, 1L)
  # A changed public scale is a new public scale identity.
  expect_false(identical(custom$id, builtin$id))
  converted <- .omopReleaseConceptUnitValues(h, list(scoped_cohort = a), "3")
  expect_equal(unique(converted$unit_concept_id), 8554)
  expect_equal(sort(converted$v),
               sort(c(rep(7, 15), rep(0.09148 * 53 + 2.152, 15))),
               tolerance = 1e-9)
  out <- .release_run(h, "dsomop:fe.continuous",
                      list(metric = "measurement_value", domain_code = "3"),
                      scope = a)
  expect_equal(nrow(out), 1L)
  expect_equal(out$unit_concept_id, 8554)
  owners <- .units_owners(out)
  expect_true(any(grepl(custom$id, vapply(owners, `[[`, character(1L),
                                          "owner"), fixed = TRUE)))
})

test_that("unknown units are one explicit separate stratum (B13)", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM measurement")
  # Persons 1..10 without a unit, 11..20 with unit 0, 21..30 in percent.
  DBI::dbWriteTable(h$conn, "measurement", data.frame(
    measurement_id = 1:30, person_id = 1:30,
    measurement_concept_id = 3004410L, measurement_date = "2021-03-01",
    measurement_type_concept_id = 44818518L, value_as_number = 7,
    value_as_concept_id = NA_integer_,
    unit_concept_id = c(rep(NA_integer_, 10), rep(0L, 10), rep(8554L, 10)),
    range_low = NA_real_, range_high = NA_real_,
    visit_occurrence_id = NA_integer_), append = TRUE)
  a <- .release_cohort(h, "units_unknown", 1:30)
  raw <- .omopReleaseConceptUnitValues(h, list(scoped_cohort = a), "3")
  sub <- .omopReleaseUnitSubstrate(raw)
  expect_setequal(unique(sub$stratum), c("concept=3004410|unit=unknown",
                                         "concept=3004410|unit=8554"))
  expect_equal(sum(sub$stratum == "concept=3004410|unit=unknown"), 20L)
  out <- .release_run(h, "dsomop:fe.continuous",
                      list(metric = "measurement_value", domain_code = "3"),
                      scope = a)
  expect_setequal(out$unit_concept_id, c(0, 8554))
  expect_identical(out$unit_name[out$unit_concept_id == 0], "unknown")
  expect_true(is.na(out$avg_value[out$unit_concept_id == 0]))
  std <- .omopCovariateContinuous(h, list(scoped_cohort = a),
                                  domain_code = "3", value_kind = "value",
                                  top_n = 50L)
  expect_setequal(std$unit_concept_id, c(0, 8554))
  expect_identical(std$unit_name[std$unit_concept_id == 0], "unknown")
})

test_that("the whole-person cap applies before strata are grouped by scale", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  route <- .omopToolsReleaseRoute(
    h, .omopToolsReleaseContracts()[["dsomop:fe.continuous"]], scoped = TRUE)
  # Every person has 12 concept x unit strata: six registered scales (one
  # owner each) and six unregistered units (one generic owner).
  strata <- data.frame(
    covariate_id = c(3004410, 3025315, 3036277, 3038553, 3004249, 3012888,
                     4000000 + 1:6),
    unit_concept_id = c(8554, 9529, 8582, 9531, 8876, 8876, rep(999001, 6)))
  raw <- do.call(rbind, lapply(1:20, function(p) {
    cbind(strata, person_id = p, v = 5, covariate_name = "c",
          unit_name = "u")
  }))
  seen <- list()
  testthat::local_mocked_bindings(
    .dsomopReleaseBuild = function(policy, protection, owner, sub, ...) {
      seen[[length(seen) + 1L]] <<- sub
      list(strata = data.frame(stratum = character(0), n_display = numeric(0)))
    })
  .omopReleaseUnitStrataBuild(route, raw, "fe/test", "values", top_n = 50)
  rows <- do.call(rbind, seen)
  expect_length(seen, 7L)
  # Ten strata per person in public concept order across all scale groups,
  # not ten per group.
  expect_true(all(table(rows$person_id) == 10L))
  expect_false(any(grepl("concept=4000005|", rows$stratum, fixed = TRUE)))
  expect_false(any(grepl("concept=4000006|", rows$stratum, fixed = TRUE)))
  expect_true(any(grepl("concept=4000004|", rows$stratum, fixed = TRUE)))
})

test_that("the measurement scale registry validates custodian entries", {
  scales <- .dsomopReleaseMeasurementScales()
  expect_true(all(c("concept=3004410|unit=8554", "concept=3025315|unit=9529")
                  %in% names(scales)))
  expect_equal(c(scales[["concept=3004410|unit=8554"]]$lower,
                 scales[["concept=3004410|unit=8554"]]$upper), c(0, 20))
  bad <- function(entry) {
    withr::with_options(list(dsomop.release.measurement_scales = list(entry)),
                        .dsomopReleaseMeasurementScales())
  }
  expect_error(bad(list(concept_id = 1, unit_concept_id = 2, lower = 5,
                        upper = 5)), "lower < upper")
  expect_error(bad(list(concept_id = 1.5, unit_concept_id = 2, lower = 0,
                        upper = 5)), "invalid concept_id")
  expect_error(bad(list(concept_id = 1, unit_concept_id = 2, lower = 0,
                        upper = 5, breaks = c(-1, 3))), "within its bounds")
  expect_error(bad(list(concept_id = 1, unit_concept_id = 2, lower = 0,
                        upper = 5, convert = list(list(unit_concept_id = 2,
                                                       factor = 1)))),
               "different source unit")
  withr::with_options(list(dsomop.release.measurement_scales = "x"),
                      expect_error(.dsomopReleaseMeasurementScales(),
                                   "list of scales"))
  one <- bad(list(concept_id = 1, unit_concept_id = 2, lower = 0, upper = 5))
  two <- bad(list(concept_id = 1, unit_concept_id = 2, lower = 0, upper = 6))
  expect_false(identical(one[["concept=1|unit=2"]]$id,
                         two[["concept=1|unit=2"]]$id))
  expect_match(one[["concept=1|unit=2"]]$id, "^measurement-scale/v1/")
})

test_that("target covariates and temporal values carry their units (B13)", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  .units_two_unit_fixture(h)
  a <- .release_cohort(h, "units_target", 1:30)
  target <- .release_run(h, "dsomop:char.target_covariates",
                         list(domain_code = "1"), scope = a)
  cont <- target[target$kind == "continuous", ]
  expect_true(all(c("unit_concept_id", "unit_name") %in% names(target)))
  expect_setequal(cont$unit_concept_id, c(8554, 999001))
  expect_true(is.na(cont$avg_value[cont$unit_concept_id == 999001]))
  expect_identical(attr(target, "dsomop_protection")$protection, "mixed")
  # The same concept x unit owners and draws as fe.continuous.
  fe <- .release_run(h, "dsomop:fe.continuous",
                     list(metric = "measurement_value", domain_code = "3"),
                     scope = a)
  expect_identical(cont$avg_value[cont$unit_concept_id == 8554],
                   fe$avg_value[fe$unit_concept_id == 8554])
  expect_identical(cont$median_value[cont$unit_concept_id == 8554],
                   fe$median_value[fe$unit_concept_id == 8554])

  temporal <- .release_run(
    h, "dsomop:ohdsi.cohort_diagnostics.temporal_covariate_value_dist",
    list(domain_code = "3", value_kind = "value", temporal_start_days = 0,
         temporal_end_days = 3650), scope = a)
  expect_true(all(c("unit_concept_id", "unit_name") %in% names(temporal)))
  expect_setequal(temporal$unit_concept_id, c(8554, 999001))
  counts <- .release_run(
    h, "dsomop:ohdsi.cohort_diagnostics.temporal_covariate_value_dist",
    list(domain_code = "3", value_kind = "count", temporal_start_days = 0,
         temporal_end_days = 3650), scope = a)
  expect_true(all(is.na(counts$unit_concept_id)))

  # Standard mode keeps the unit columns and the established masks.
  withr::local_options(list(dsomop.tools.mode = "standard"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  std <- .release_run(h, "dsomop:fe.continuous",
                      list(metric = "measurement_value", domain_code = "3"),
                      scope = a)
  expect_setequal(std$unit_concept_id, c(8554, 999001))
  expect_true(all(is.na(std$avg_value)))
})

test_that("without noise the unit strata are per-person means within unit", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM measurement")
  # Two HbA1c percent records per person (means 5.5 + p / 10) and one record
  # in an unregistered unit.
  DBI::dbWriteTable(h$conn, "measurement", data.frame(
    measurement_id = 1:90, person_id = c(rep(1:30, 2), 1:30),
    measurement_concept_id = 3004410L, measurement_date = "2021-03-01",
    measurement_type_concept_id = 44818518L,
    value_as_number = c(5 + (1:30) / 10, 6 + (1:30) / 10, rep(40, 30)),
    value_as_concept_id = NA_integer_,
    unit_concept_id = c(rep(8554L, 60), rep(999001L, 30)),
    range_low = NA_real_, range_high = NA_real_,
    visit_occurrence_id = NA_integer_), append = TRUE)
  a <- .release_cohort(h, "units_exact", 1:30)
  testthat::local_mocked_bindings(
    .dsomopReleaseNoisyComponent = function(policy, keys, protection,
                                            component) component$value)
  out <- .release_run(h, "dsomop:fe.continuous",
                      list(metric = "measurement_value", domain_code = "3"),
                      scope = a)
  row <- out[out$unit_concept_id == 8554, ]
  person_means <- 5.5 + (1:30) / 10
  expect_equal(row$count_value, 30)
  expect_equal(row$avg_value, mean(person_means), tolerance = 1e-4)
  expect_equal(row$stdev_value, stats::sd(person_means), tolerance = 1e-3)
  expect_true(is.na(out$avg_value[out$unit_concept_id == 999001]))
})
