# Capped record-weighted QueryLibrary summaries (dsOMOP 2.10.3, review s6).

# The test CDM plus the era and cost tables, and 20 more drug exposures (with
# cost rows) so the quantity and cost questions have nfilter.dist persons.
.records_tables <- function(h) {
  conn <- h$conn
  n <- DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM person")$n
  DBI::dbExecute(conn, paste(
    "CREATE TABLE condition_era (condition_era_id INTEGER, person_id INTEGER,",
    "condition_concept_id INTEGER, condition_era_start_date TEXT,",
    "condition_era_end_date TEXT, condition_occurrence_count INTEGER)"))
  persons <- rep(seq_len(n), times = rep_len(c(1L, 2L, 3L), n))
  DBI::dbWriteTable(conn, "condition_era", data.frame(
    condition_era_id = seq_along(persons), person_id = persons,
    condition_concept_id = rep_len(c(201820L, 255573L), length(persons)),
    condition_era_start_date = "2020-01-01",
    condition_era_end_date = as.character(
      as.Date("2020-01-01") + (seq_along(persons) * 37L) %% 900L),
    condition_occurrence_count = 1L), append = TRUE)
  DBI::dbExecute(conn, paste(
    "CREATE TABLE drug_era (drug_era_id INTEGER, person_id INTEGER,",
    "drug_concept_id INTEGER, drug_era_start_date TEXT,",
    "drug_era_end_date TEXT, drug_exposure_count INTEGER, gap_days INTEGER)"))
  DBI::dbWriteTable(conn, "drug_era", data.frame(
    drug_era_id = seq_len(n), person_id = seq_len(n),
    drug_concept_id = rep(c(1124300L, 1154029L), length.out = n),
    drug_era_start_date = "2021-01-01",
    drug_era_end_date = as.character(
      as.Date("2021-01-01") + 10L + seq_len(n) * 5L),
    drug_exposure_count = rep_len(1:12, n), gap_days = 0L), append = TRUE)
  DBI::dbWriteTable(conn, "drug_exposure", data.frame(
    drug_exposure_id = 100L + 1:20, person_id = 10L + 1:20,
    drug_concept_id = 1124300L, drug_exposure_start_date = "2021-01-01",
    drug_exposure_end_date = "2021-02-01", drug_type_concept_id = 44818518L,
    quantity = 30 + (1:20 %% 4) * 10, visit_occurrence_id = NA_integer_),
    append = TRUE)
  DBI::dbExecute(conn, paste(
    "CREATE TABLE cost (cost_id INTEGER, cost_event_id INTEGER,",
    "cost_domain_id TEXT, currency_concept_id INTEGER, total_paid REAL,",
    "paid_by_patient REAL)"))
  DBI::dbWriteTable(conn, "cost", data.frame(
    cost_id = c(1:6, 100L + 1:20), cost_event_id = c(1:6, 100L + 1:20),
    cost_domain_id = "Drug", currency_concept_id = 44818668L,
    total_paid = c(10, 20, 30, 40, 5, 6, 60 + 1:20),
    paid_by_patient = c(1, 2, 3, 4, 1, 1, rep(5, 20))), append = TRUE)
  h
}

.records_handle <- function(n_persons = 60, derived = FALSE) {
  h <- if (isTRUE(derived)) .release_derived_handle(n_persons) else
    create_test_handle(n_persons = n_persons)
  .records_tables(h)
}

.records_run <- function(h, id, params = list()) {
  suppressWarnings(.omopAnalysisRun(h, paste0("dsomop:", id), params))
}

.records_spec <- function(qid) .dsomopToolsReviewedQueries()[[qid]]

.records_owner <- function(df) attr(df, "dsomop_release")$owners[[1L]]

test_that("record-weighted summaries resolve protected per-server contracts", {
  .release_state()
  h <- .records_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  registry <- suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  for (qid in names(.DSOMOP_RELEASE_RECORD_SUMMARIES)) {
    entry <- registry[[paste0("dsomop:", qid)]]
    contract <- .omopToolsReleaseContract(entry)
    expect_identical(contract$class, "C7", info = qid)
    expect_null(contract$per_site_only, info = qid)
    expect_identical(entry$meta$tools_shape, "summary", info = qid)
    # dsOMOPClient 2.10.3 pools the record components (N, R, S, Q per
    # stratum); older clients see a not-poolable contract. Servers without
    # the DP service pool their standard summaries by the established one.
    rp <- entry$meta$release_pooling
    expect_identical(rp$strategy, "protected_components", info = qid)
    expect_identical(names(rp$columns), entry$meta$output_fields, info = qid)
    expect_identical(rp$standard_contract$strategy, "tabular", info = qid)
    expect_identical(rp$standard_contract$columns, rp$columns, info = qid)
    expect_null(rp$effect_estimate, info = qid)
    expect_identical(entry$meta$pooling_contract$strategy, "not_poolable",
                     info = qid)
  }
  withr::with_options(list(dsomop.tools.mode = "standard"), {
    registry <- suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
    entry <- registry[["dsomop:condition_era.length_stats"]]
    expect_null(entry$meta$release_pooling)
    expect_identical(entry$meta$pooling_contract$strategy, "tabular")
  })
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
})

test_that("duration summaries release capped record-weighted moments", {
  .release_state()
  h <- .records_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  out <- .records_run(h, "condition_era.length_stats")
  expect_identical(names(out), c("condition_concept_id", "concept_name",
                                 "n_persons", "n_records", "avg_days",
                                 "stddev_days"))
  expect_gt(nrow(out), 0L)
  label <- attr(out, "dsomop_protection")
  # Unscoped, over the CDM population, with a custodial snapshot.
  expect_identical(label$protection, "fixed_synopsis")
  expect_equal(label$mechanism_epsilon, 0.1)
  expect_identical(label$privacy_call_quota, "none")
  expect_false(label$history_based_admission)
  expect_match(label$estimand, "capped record-weighted mean and SD")
  expect_identical(label$weighting, "record")
  comp <- label$components[[1L]]
  expect_identical(comp$family, "RM")
  expect_identical(comp$cap_scope, "whole_person_records")
  expect_equal(comp$contribution_cap, 10L)
  expect_equal(comp$sensitivity$N, 10)
  expect_equal(comp$sensitivity$R, 10)
  expect_equal(comp$sensitivity$S, 10 * 65535)
  expect_equal(comp$sensitivity$Q, 10 * 65535)
  expect_equal(unlist(comp$allocation_fraction),
               c(N = 0.2, R = 0.3, S = 0.3, Q = 0.2))
  expect_identical(comp$scales[[1L]]$id, "record-scale/duration-days-v2")
  expect_equal(c(comp$scales[[1L]]$lower, comp$scales[[1L]]$upper),
               c(0, 10957))
  expect_identical(comp$record_order,
                   c("condition_era_start_date", "condition_era_id"))
  # Displayed values are post-processing of the exported components: persons
  # from N, records from R, the moments from R, S and Q only.
  owner <- .records_owner(out)
  expect_identical(owner$family, "RM")
  expect_identical(owner$weighting, "record")
  for (c in owner$components) {
    row <- out[paste0("concept=", out$condition_concept_id) == c$stratum, ]
    expect_equal(nrow(row), 1L)
    expect_equal(row$n_persons, max(0, c$n))
    expect_equal(row$n_records, max(0, c$r))
    if (!is.na(row$avg_days)) {
      R <- row$n_records
      Su <- c$s / 65535
      Qu <- c$q / 65535
      p <- .dsomopReleaseProjectMoments(2 * Su - R, 4 * Qu - 4 * Su + R, R)
      expect_equal(row$avg_days, 10957 / 2 * (1 + p[["S"]] / R),
                   tolerance = 1e-6)
      expect_equal(row$stddev_days, 10957 / 2 *
                     sqrt(max(0, (p[["Q"]] - p[["S"]]^2 / R) / (R - 1))),
                   tolerance = 1e-6)
    }
  }
  # Displayed by released persons, then the public concept id.
  expect_false(is.unsorted(-out$n_persons))
  # The pooling fields: column roles, display order and, per stratum, its
  # displayed key and label values and public ordering values (nothing that
  # is not displayed).
  expect_identical(owner$display, list(
    columns = names(out), persons = "n_persons", records = "n_records",
    mean = "avg_days", sd = "stddev_days", order = "n_persons"))
  for (c in owner$components) {
    row <- out[paste0("concept=", out$condition_concept_id) == c$stratum, ]
    expect_identical(names(c$display), c("condition_concept_id",
                                         "concept_name"))
    expect_equal(c$display$condition_concept_id, row$condition_concept_id)
    expect_identical(c$display$concept_name, row$concept_name)
    expect_equal(c$order_value, row$condition_concept_id)
    expect_equal(c$order_unit, 0)
  }
  expect_null(attr(out, "dsomop_record_strata"))
  again <- .records_run(h, "condition_era.length_stats")
  expect_identical(.release_values(again), .release_values(out))
  for (id in c("drug_era.length_stats", "observation_period.length_stats",
               "observation_period.length_months_stats")) {
    res <- .records_run(h, id, if (identical(id, "drug_era.length_stats"))
      list(top_n = 5) else list())
    expect_identical(attr(res, "dsomop_protection")$protection,
                     "fixed_synopsis", info = id)
    expect_identical(names(res),
                     .omopAnalysisResolve(h, paste0("dsomop:", id))$meta$output_fields,
                     info = id)
  }
  # A question without an SD column draws no Q component.
  op <- .records_run(h, "observation_period.length_stats")
  comp <- attr(op, "dsomop_protection")$components[[1L]]
  expect_equal(unlist(comp$allocation_fraction), c(N = 0.2, R = 0.4, S = 0.4))
  expect_true(all(vapply(.records_owner(op)$components,
                         function(c) is.null(c$q), logical(1L))))
})

test_that("the whole-person cap keeps the first K records in public order", {
  .release_state()
  h <- .records_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM condition_era WHERE person_id = 1")
  # Twelve eras of person 1, each of its own concept, in an order unrelated to
  # the concept ids: 2019 eras first, then 2020 (ties broken by era id).
  DBI::dbWriteTable(h$conn, "condition_era", data.frame(
    condition_era_id = 1000L + 1:12, person_id = 1L,
    condition_concept_id = 5000013L - 1:12,
    condition_era_start_date = c(rep("2020-01-01", 4), rep("2019-01-01", 4),
                                 rep("2021-01-01", 4)),
    condition_era_end_date = "2021-06-01", condition_occurrence_count = 1L),
    append = TRUE)
  spec <- .records_spec("condition_era.length_stats")
  sub <- .omopReleaseRecordSubstrate(h, spec, NULL, "concept")
  mine <- sub[sub$person_id == "1", ]
  expect_equal(nrow(mine), 10L)
  # Kept: eras 1005..1008, 1001..1004, 1009, 1010; dropped: 1011, 1012
  # (concepts 5000002 and 5000001), not the highest concept ids.
  expect_setequal(mine$concept_id, 5000013 - c(1:10))
  expect_false(any(c(5000001, 5000002) %in% mine$concept_id))
  expect_true(all(table(sub$person_id) <= 10L))
})

test_that("a person changes each record component vector by at most the cap", {
  .release_state()
  h <- .records_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbWriteTable(h$conn, "condition_era", data.frame(
    condition_era_id = 2000L + 1:15, person_id = 2L,
    condition_concept_id = 6000000L + 1:15,
    condition_era_start_date = "2019-01-01",
    condition_era_end_date = "2049-01-01", condition_occurrence_count = 1L),
    append = TRUE)
  spec <- .records_spec("condition_era.length_stats")
  sub <- .omopReleaseRecordSubstrate(h, spec, NULL, "concept")
  owner <- .dsomopReleaseRecordOwner(
    "test", "test", .omopReleaseRecordScaleOf(list(scale = "days")))
  full <- .dsomopReleaseRecordExactComponents(owner, sub)
  J <- 65535
  for (person in c("1", "2", "5", "9")) {
    without <- .dsomopReleaseRecordExactComponents(
      owner, sub[sub$person_id != person, ])
    keys <- union(names(full), names(without))
    diff <- function(field) sum(vapply(keys, function(k) {
      abs((full[[k]][[field]] %||% 0) - (without[[k]][[field]] %||% 0))
    }, numeric(1L)))
    expect_lte(diff("n"), 10, label = person)
    expect_lte(diff("r"), 10, label = person)
    expect_lte(diff("s"), 10 * J, label = person)
    expect_lte(diff("q"), 10 * J, label = person)
  }
})

test_that("calendar months are their own public scale, never days / 30", {
  .release_state()
  h <- .records_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = 1000L, person_id = 2L,
    observation_period_start_date = "2020-01-31",
    observation_period_end_date = "2020-02-01",
    period_type_concept_id = 44818518L), append = TRUE)
  spec <- .records_spec("observation_period.length_months_stats")
  sub <- .omopReleaseRecordSubstrate(h, spec, NULL, "year")
  # One day, but one calendar month.
  expect_true(1 %in% sub$v[sub$person_id == "2"])
  expect_true(all(sub$v[sub$person_id == "1"] == 59))
  out <- .records_run(h, "observation_period.length_months_stats")
  expect_identical(names(out), c("start_year", "n_persons", "avg_months",
                                 "stddev_months"))
  comp <- attr(out, "dsomop_protection")$components[[1L]]
  expect_identical(comp$scales[[1L]]$id, "record-scale/duration-months-v1")
  expect_equal(c(comp$scales[[1L]]$lower, comp$scales[[1L]]$upper), c(0, 360))
  expect_identical(.dsomopReleaseRecordDurationScale("months")$breaks,
                   c(0, 1, 2, 3, 4, 6, 9, 12, 18, 24, 36, 48, 60, 120, 240,
                     360))
  expect_equal(out$start_year, sort(out$start_year))
})

test_that("only registered scales give quantity, value and cost moments", {
  .release_state()
  h <- .records_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  has_moments <- function(df) {
    any(vapply(.records_owner(df)$components, function(c) !is.null(c$s),
               logical(1L)))
  }
  # Drug quantity: no built-in scale, so protected counts only.
  qty <- .records_run(h, "drug.quantity_stats", list(concept_id = 1124300))
  expect_identical(names(qty), c("drug_concept_id", "concept_name",
                                 "n_persons", "n_records", "avg_quantity",
                                 "sd_quantity"))
  expect_true(all(is.na(qty$avg_quantity) & is.na(qty$sd_quantity)))
  expect_false(has_moments(qty))
  label <- attr(qty, "dsomop_protection")
  expect_identical(label$components[[1L]]$moments_unavailable$reason,
                   "public_scale_unavailable")
  withr::with_options(list(dsomop.release.quantity_scales = list(list(
    concept_id = 1124300, lower = 0, upper = 1000))), {
    scaled <- .records_run(h, "drug.quantity_stats", list(concept_id = 1124300))
    expect_true(has_moments(scaled))
    expect_match(attr(scaled, "dsomop_protection")$components[[1L]]$scales[[1L]]$id,
                 "^quantity-scale/v1/")
  })

  # Measurement values: concept x unit, registered body weight in kilograms.
  DBI::dbWriteTable(h$conn, "measurement", data.frame(
    measurement_id = 500L + 1:12, person_id = 20L + 1:12,
    measurement_concept_id = 3025315L, measurement_date = "2021-05-01",
    measurement_type_concept_id = 44818518L, value_as_number = 150,
    value_as_concept_id = NA_integer_, unit_concept_id = 8739L,
    range_low = NA_real_, range_high = NA_real_,
    visit_occurrence_id = NA_integer_), append = TRUE)
  value <- .records_run(h, "measurement.value_stats",
                        list(concept_id = 3025315))
  expect_identical(names(value), c("measurement_concept_id", "concept_name",
                                   "unit_concept_id", "unit_name",
                                   "n_persons", "n_values", "mean_value",
                                   "sd_value"))
  expect_setequal(value$unit_concept_id, c(8739, 9529))
  comps <- .records_owner(value)$components
  by_stratum <- stats::setNames(comps, vapply(comps, `[[`, character(1L),
                                              "stratum"))
  expect_false(is.null(by_stratum[["concept=3025315|unit=9529"]]$s))
  # Pounds (8739) have no registered scale or declared conversion.
  expect_null(by_stratum[["concept=3025315|unit=8739"]]$s)
  expect_true(is.na(value$mean_value[value$unit_concept_id == 8739]))
  expect_identical(
    attr(value, "dsomop_protection")$components[[1L]]$moments_unavailable$strata,
    "concept=3025315|unit=8739")

  # Costs: per currency; a per-unit scale needs a declared valid denominator.
  cost <- .records_run(h, "drug_cost.cost_per_unit_stats",
                       list(concept_id = 1124300))
  expect_identical(names(cost), c("drug_concept_id", "concept_name",
                                  "currency_concept_id", "currency_name",
                                  "n_persons", "n_records",
                                  "avg_cost_per_unit", "sd_cost_per_unit"))
  expect_false(has_moments(cost))
  withr::with_options(list(dsomop.release.cost_scales = list(list(
    measure = "cost_per_unit", concept_id = 1124300,
    currency_concept_id = 44818668, lower = 0, upper = 100))), {
    expect_error(.records_run(h, "drug_cost.cost_per_unit_stats",
                              list(concept_id = 1124300)),
                 "per_unit_denominator")
  })
  withr::with_options(list(dsomop.release.cost_scales = list(
    list(measure = "cost_per_unit", concept_id = 1124300,
         currency_concept_id = 44818668, lower = 0, upper = 100,
         per_unit_denominator = TRUE),
    list(measure = "out_of_pocket", concept_id = 1124300,
         currency_concept_id = 44818668, lower = 0, upper = 500))), {
    per_unit <- .records_run(h, "drug_cost.cost_per_unit_stats",
                             list(concept_id = 1124300))
    expect_true(has_moments(per_unit))
    pocket <- .records_run(h, "drug_cost.out_of_pocket_stats",
                           list(concept_id = 1124300))
    expect_true(has_moments(pocket))
    expect_equal(pocket$currency_concept_id, 44818668)
  })
})

test_that("standard mode and auto without DP keep the standard summaries", {
  .release_state(enabled = FALSE)
  h <- .records_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  fallback <- .records_run(h, "condition_era.length_stats")
  expect_identical(attr(fallback, "dsomop_protection")$protection,
                   "standard_fallback")
  withr::local_options(list(dsomop.tools.mode = "standard"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  std <- .records_run(h, "condition_era.length_stats")
  expect_identical(attr(std, "dsomop_protection")$protection, "standard")
  expect_identical(.release_values(std), .release_values(fallback))
  eras <- DBI::dbGetQuery(h$conn, paste(
    "SELECT condition_concept_id AS c, julianday(condition_era_end_date) -",
    "julianday(condition_era_start_date) AS d FROM condition_era"))
  for (concept in std$condition_concept_id[!is.na(std$avg_days)]) {
    expect_equal(std$avg_days[std$condition_concept_id == concept],
                 mean(eras$d[eras$c == concept]), tolerance = 1e-9)
  }
})

test_that("DP-only mode answers them only with fixed synopses", {
  .release_state(mode = "dp")
  h <- .records_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  out <- .records_run(h, "drug_era.length_stats", list(top_n = 1))
  expect_identical(attr(out, "dsomop_protection")$protection,
                   "fixed_synopsis")
  expect_lte(nrow(out), 1L)
  expect_identical(.release_values(.records_run(h, "drug_era.length_stats",
                                                list(top_n = 1))),
                   .release_values(out))
})

test_that("without a custodial generation they are heuristic or refused", {
  .release_state(snapshot_id = NULL)
  h <- .records_handle(derived = TRUE)
  on.exit(cleanup_handle(h), add = TRUE)
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  out <- .records_run(h, "condition_era.length_stats")
  label <- attr(out, "dsomop_protection")
  expect_identical(label$protection, "realised_sticky")
  expect_true(is.na(label$mechanism_epsilon))
  withr::local_options(list(dsomop.tools.mode = "dp"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  expect_error(.records_run(h, "condition_era.length_stats"),
               "no fixed-synopsis protection")
})

test_that("the record substrate builds portable SQL for every dialect", {
  captured <- character(0)
  testthat::local_mocked_bindings(
    .executeQuery = function(handle, sql) {
      captured <<- c(captured, sql)
      data.frame()
    },
    .qualifyTable = function(handle, table, schema = NULL) {
      paste0("cdm.", table)
    }
  )
  for (dialect in .DSOMOP_TOOLS_DIALECTS) {
    captured <- character(0)
    h <- new.env(parent = emptyenv())
    h$target_dialect <- dialect
    for (qid in names(.DSOMOP_RELEASE_RECORD_SUMMARIES)) {
      spec <- .records_spec(qid)
      concept_id <- if (!is.null(spec$concept_param)) 201820L else NULL
      sub <- .omopReleaseRecordSubstrate(
        h, spec, concept_id, .omopReleaseRecordStrataKind(spec$summary))
      expect_equal(nrow(sub), 0L)
    }
    expect_length(captured, length(.DSOMOP_RELEASE_RECORD_SUMMARIES))
    sql <- paste(captured, collapse = "\n")
    expect_true(all(grepl("ROW_NUMBER() OVER (PARTITION BY x.person_id ORDER BY",
                          captured, fixed = TRUE)), info = dialect)
    expect_false(grepl("\\b(STDDEV|STDEV|AVG|LIMIT)\\b", sql,
                       ignore.case = TRUE, perl = TRUE), info = dialect)
    if (dialect %in% c("sqlite", "sql server", "mysql", "spark")) {
      expect_false(grepl("EXTRACT\\s*\\(", sql, ignore.case = TRUE),
                   info = dialect)
    }
  }
})

test_that("without noise the record moments are the record-weighted mean and SD", {
  .release_state()
  h <- .records_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  route <- .omopToolsReleaseRoute(
    h, .omopToolsReleaseContracts()[["dsomop:fe.continuous"]], scoped = TRUE)
  testthat::local_mocked_bindings(
    .dsomopReleaseNoisyComponent = function(policy, keys, protection,
                                            component) component$value)
  # Three records per person in stratum a, one in b: R = 3N in a, so a
  # person denominator would be visibly wrong.
  sub <- data.frame(
    person_id = c(rep(1:20, each = 3), 1:20),
    stratum = c(rep("a", 60), rep("b", 20)), stratum_order = 1,
    v = c(seq(1, 60) * 1.5, seq(1, 20) * 4), stringsAsFactors = FALSE)
  scale <- list(id = "test-scale", lower = 0, upper = 100)
  for (with_sd in c(TRUE, FALSE)) {
    owner <- .dsomopReleaseRecordOwner("test", "test", function(s) scale,
                                       with_sd = with_sd)
    built <- .dsomopReleaseBuildRecordMoments(route$policy, route$protection,
                                              owner, sub)
    st <- built$strata
    for (k in c("a", "b")) {
      v <- pmin(100, sub$v[sub$stratum == k])
      i <- match(k, st$stratum)
      expect_equal(st$n_display[[i]], 20)
      expect_equal(st$r_display[[i]], length(v))
      expect_equal(st$mean[[i]], mean(v), tolerance = 1e-4)
      if (with_sd) {
        expect_equal(st$sd[[i]], stats::sd(v), tolerance = 1e-3)
      } else {
        expect_true(is.na(st$sd[[i]]))
      }
    }
  }
  # End to end: with the cap inactive (at most three eras per person) the
  # protected estimand is the original record-weighted summary.
  out <- .records_run(h, "condition_era.length_stats")
  withr::local_options(list(dsomop.tools.mode = "standard"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  std <- .records_run(h, "condition_era.length_stats")
  exact <- DBI::dbGetQuery(h$conn, paste(
    "SELECT condition_concept_id, COUNT(*) AS n, COUNT(DISTINCT person_id)",
    "AS p FROM condition_era GROUP BY condition_concept_id"))
  both <- merge(merge(out, std, by = "condition_concept_id"), exact,
                by = "condition_concept_id")
  expect_gt(nrow(both), 0L)
  expect_equal(both$n_records.x, as.numeric(both$n))
  expect_equal(both$n_persons.x, as.numeric(both$p))
  expect_equal(both$avg_days.x, both$avg_days.y, tolerance = 1e-4)
  expect_equal(both$stddev_days.x, both$stddev_days.y, tolerance = 1e-3)
})

# --- Real engines: the record SQL gives the SQLite answers --------------------

.records_engine_questions <- list(
  list(id = "dsomop:observation_period.length_stats"),
  list(id = "dsomop:observation_period.length_months_stats"),
  list(id = "dsomop:measurement.value_stats",
       params = list(concept_id = "3004410")),
  list(id = "dsomop:drug.quantity_stats",
       params = list(concept_id = "1503297"))
)

.records_numeric_frame <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  for (col in names(df)) {
    v <- df[[col]]
    if (inherits(v, "integer64") || is.numeric(v)) df[[col]] <- as.numeric(v)
  }
  rownames(df) <- NULL
  df
}

.records_engine_answers <- function(handle) {
  suppressWarnings(.omopAnalysisRegistry(handle, force = TRUE))
  lapply(.records_engine_questions, function(q) {
    out <- suppressWarnings(.omopAnalysisRun(handle, q$id,
                                             q$params %||% list()))
    list(id = q$id,
         protection = attr(out, "dsomop_protection")$protection,
         values = .records_numeric_frame(out))
  })
}

# The live fixture on an embedded engine (one schema).
.records_engine_handle <- function(conn, dbms) {
  for (statement in .achillesLiveStatements(.achillesLiveFrames(), dbms)) {
    DBI::dbExecute(conn, statement)
  }
  handle <- new.env(parent = emptyenv())
  handle$conn <- conn
  handle$dbms <- dbms
  handle$target_dialect <- dbms
  handle$cdm_schema <- if (identical(dbms, "duckdb")) "main" else NULL
  handle$vocab_schema <- NULL
  handle$results_schema <- NULL
  handle$temp_schema <- NULL
  handle$resource_client <- NULL
  handle$config <- list()
  handle$blueprint <- NULL
  handle$temp_tables <- character(0)
  handle$temp_connection <- NULL
  handle$person_key <- .testPseudonymKey("records-engines")
  .buildBlueprint(handle)
  handle
}

.records_sqlite_answers <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
    cache <<- .records_engine_answers(.records_engine_handle(conn, "sqlite"))
    cache
  }
})

.expect_records_engine <- function(answers, label) {
  reference <- .records_sqlite_answers()
  for (i in seq_along(reference)) {
    info <- paste(label, reference[[i]]$id)
    expect_identical(answers[[i]]$protection, "fixed_synopsis", info = info)
    expect_gt(nrow(reference[[i]]$values), 0L, label = info)
    expect_equal(answers[[i]]$values, reference[[i]]$values,
                 tolerance = 1e-9, info = info)
  }
}

test_that("record summaries run on SQLite and DuckDB with equal answers", {
  .release_state()
  reference <- .records_sqlite_answers()
  expect_true(all(vapply(reference, `[[`, character(1L), "protection") ==
                    "fixed_synopsis"))
  value <- reference[[3L]]$values
  # The live fixture records three HbA1c values without a unit: their own
  # stratum, never merged with percent.
  expect_setequal(value$unit_concept_id, c(0, 8554))
  skip_if_not_installed("duckdb")
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  .expect_records_engine(.records_engine_answers(
    .records_engine_handle(conn, "duckdb")), "duckdb")
})

.expect_records_vendor <- function(dbms) {
  config <- .dsomopVendorTestConfig(dbms)
  testthat::skip_if(is.null(config), paste0(
    "Set DSOMOP_TEST_", toupper(if (identical(dbms, "postgresql")) {
      "POSTGRES"
    } else dbms), "_HOST to run this vendor integration test."))
  .release_state(.local_envir = parent.frame())
  frames <- .achillesLiveFrames()
  fixture <- .dsomopVendorCreateFixture(config, statements = function(
      cdm, vocab, results) {
    .achillesLiveStatements(frames, dbms, list(cdm = cdm, vocab = vocab,
                                               results = results))
  })
  on.exit(fixture$cleanup(), add = TRUE)
  .expect_records_engine(.records_engine_answers(fixture$handle), dbms)
}

test_that("PostgreSQL gives the SQLite record-summary answers", {
  .expect_records_vendor("postgresql")
})

test_that("MySQL gives the SQLite record-summary answers", {
  .expect_records_vendor("mysql")
})

test_that("MariaDB gives the SQLite record-summary answers", {
  .expect_records_vendor("mariadb")
})
