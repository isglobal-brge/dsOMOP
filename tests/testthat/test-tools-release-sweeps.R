# G-02 regressions (dsOMOP 2.10.3): sweeping free numeric parameters no longer
# rebuilds exact order statistics, birth years, arm sizes or event days. Ports
# of the audit's attack scripts (QueryLibrary min_days / reference_year sweeps,
# CohortMethod TAR sweep and day-scaling, SCCS window sweep) through the real
# run path. Helpers in helper-tools-release.R.

# A 200-person CDM whose observation periods have distinct lengths (so exact
# order statistics exist) and spread birth years.
.sweep_handle <- function(n = 200) {
  h <- create_test_handle(n_persons = n)
  lengths <- c(1L, 4598L, 4601L, 4605L, seq.int(40L, by = 9L,
                                                 length.out = n - 4L))
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = seq_len(n), person_id = seq_len(n),
    observation_period_start_date = "2008-01-01",
    observation_period_end_date = as.character(as.Date("2008-01-01") +
                                                 lengths),
    period_type_concept_id = 44814724L), append = TRUE)
  years <- c(1909L, 1911L, 1912L, 1950L + (seq_len(n - 3L) %% 50L))
  for (i in seq_len(n)) {
    DBI::dbExecute(h$conn, sprintf(
      "UPDATE person SET year_of_birth = %d WHERE person_id = %d",
      years[[i]], i))
  }
  attr(h, "lengths") <- lengths
  attr(h, "years") <- years
  h
}

test_that("min_days sweeps only see the canonical views of one public histogram", {
  .release_state()
  h <- .sweep_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  release <- function(d) {
    out <- .release_run(h, "dsomop:observation_period.long_period_count",
                        list(min_days = d))
    if (nrow(out)) out$n_persons else NA_real_
  }
  grid <- .dsomopReleaseGrid("duration_days")$breaks
  # Every request inside one quantization cell is the same effective estimand
  # and the same answer.
  for (pair in list(c(31, 44), c(61, 74), c(366, 455), c(4000, 5475))) {
    expect_identical(release(pair[[1L]]), release(pair[[2L]]))
  }
  # The audit's binary search for the minimum and third-largest period length
  # can only locate public grid midpoints, never the exact values (1, 4601).
  lo <- -1; hi <- max(attr(h, "lengths")) + 2; base <- release(-1)
  while (hi - lo > 1) {
    mid <- (lo + hi) %/% 2
    if (identical(release(mid), base)) lo <- mid else hi <- mid
  }
  midpoints <- (grid[-1L] + grid[-length(grid)]) / 2
  expect_true(any(abs((hi - 1) - midpoints) <= 1) || hi - 1 >= max(grid))
  expect_false(identical(as.integer(hi - 1), 1L))
  # Negative, zero and huge thresholds saturate to the public endpoints.
  expect_identical(release(-500), release(0))
  expect_identical(release(1e7), release(10957))
  # Standard mode answers the snapped threshold under the standard gate.
  withr::local_options(list(dsomop.tools.mode = "standard"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  std <- .release_run(h, "dsomop:observation_period.long_period_count",
                      list(min_days = 400))
  expect_identical(attr(std, "dsomop_protection")$effective_parameters$
                     min_days$effective, "365")
})

test_that("reference-year sweeps relabel one released birth-year basis", {
  .release_state()
  h <- .sweep_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  basis <- .release_run(h, "dsomop:achilles.3")
  for (year in 1995:2012) {
    out <- .release_run(h, "dsomop:person.age_distribution_decade",
                        list(reference_year = year))
    # The complete public domain is always released: the presence of the
    # '90+' group is no first-visible-year oracle.
    expect_true("90+" %in% out$age_group)
    expect_equal(sum(out$n_persons), sum(basis$count_value),
                 tolerance = 1e-9)
  }
  # Standard mode: the requested year snaps to the fixed reference year, so
  # every request is one effective estimand.
  withr::local_options(list(dsomop.tools.mode = "standard"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  answers <- lapply(c(1995, 2002, 2012), function(y) {
    .release_values(.release_run(h, "dsomop:person.age_distribution_decade",
                                 list(reference_year = y)))
  })
  expect_identical(answers[[1L]], answers[[2L]])
  expect_identical(answers[[2L]], answers[[3L]])
})

# The CohortMethod TAR sweep of the audit (exp12): exact SE changes located the
# target arm's event days 100, 107, 114 and 121.
.sweep_cm_handle <- function() {
  h <- create_test_handle(n_persons = 200)
  DBI::dbExecute(h$conn,
                 "DELETE FROM condition_occurrence WHERE condition_concept_id = 4000002")
  .release_dated_cohort(h, "sw_t", 1:60, start = "2021-01-01",
                        end = "2023-12-31")
  .release_dated_cohort(h, "sw_c", 101:160, start = "2021-01-01",
                        end = "2023-12-31")
  for (p in 1:20) {
    .release_add_condition(h, p, 4000002L,
                           format(as.Date("2021-01-01") + 30 + p * 7))
  }
  for (p in 101:110) {
    .release_add_condition(h, p, 4000002L,
                           format(as.Date("2021-01-01") + 20 + (p - 100) * 3))
  }
  h
}

test_that("a TAR sweep no longer locates event days through SE, counts or time", {
  .release_state()
  h <- .sweep_cm_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  sweep <- lapply(95:125, function(d) {
    .release_run(h, "dsomop:cm.effect_estimate", list(
      outcome_concept_id = "4000002", model_type = "poisson",
      tar_start_offset = "0", tar_end_offset = as.character(d),
      tar_anchor_start = "start", tar_anchor_end = "start"),
      scope = list("sw_t", "sw_c"))
  })
  effective <- vapply(seq_along(sweep), function(i) {
    attr(sweep[[i]], "dsomop_protection")$effective_parameters$
      tar_end_offset$effective %||% as.character((95:125)[[i]])
  }, character(1L))
  # 31 requests collapse to two public windows on the clinical grid:
  # 95..120 end at 120, 121..125 at 150 (review section 5).
  expect_identical(effective[(95:125) <= 120], rep("120", 26L))
  expect_identical(effective[(95:125) > 120], rep("150", 5L))
  key <- function(r) paste(unlist(r[, c("persons", "person_days", "outcomes",
                                        "se_log_estimate")]), collapse = "|")
  keys <- vapply(sweep, key, character(1L))
  # Equality inside each public cell: the event days 100, 107 and 114 create
  # no transition; 121 is the announced public boundary.
  expect_length(unique(keys[(95:125) <= 120]), 1L)
  expect_length(unique(keys[(95:125) > 120]), 1L)
  for (r in sweep) {
    expect_identical(attr(r, "dsomop_protection")$protection,
                     "realised_sticky")
  }
  # Standard mode releases no exact person-time, estimate or SE at all: with
  # the DP service its person-time block is the protected one of the same
  # effective window (a labelled mixed frame), without it the block is
  # unavailable.
  time_fields <- c("person_days", "estimate", "ci_lo", "ci_hi",
                   "log_estimate", "se_log_estimate")
  withr::local_options(list(dsomop.tools.mode = "standard"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  std_run <- function() .release_run(h, "dsomop:cm.effect_estimate", list(
    outcome_concept_id = "4000002", model_type = "poisson",
    tar_start_offset = "0", tar_end_offset = "110",
    tar_anchor_start = "start", tar_anchor_end = "start"),
    scope = list("sw_t", "sw_c"))
  std <- std_run()
  expect_identical(attr(std, "dsomop_protection")$protection, "mixed")
  expect_identical(attr(std, "dsomop_protection")$blocks$person_time,
                   "realised_sticky")
  expect_equal(std[, time_fields], sweep[[which(95:125 == 110)]][, time_fields],
               tolerance = 1e-9)
  withr::local_options(list(dsomop.dp.enabled = FALSE))
  .release_restart()
  off <- std_run()
  if (nrow(off)) expect_true(all(is.na(off[, time_fields])))
  expect_true("se_log_estimate" %in%
                attr(off, "dsomop_protection")$masked_fields)
})

test_that("day-scaling variants cannot recover exact arm sizes from person-time", {
  .release_state()
  h <- .sweep_cm_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  release <- function(end) {
    .release_run(h, "dsomop:cm.mdrr", list(
      outcome_concept_id = "4000002", tar_start_offset = "0",
      tar_end_offset = as.character(end), tar_anchor_start = "start",
      tar_anchor_end = "start"), scope = list("sw_t", "sw_c"))
  }
  base <- release(60)
  for (step in c(30, 365, 10000)) {
    shifted <- release(60 + step)
    # The released person-days are post-processing of noisy, horizon-clipped
    # components: the former algebra (difference / step = exact N) fails.
    implied <- (shifted$person_days[[1L]] - base$person_days[[1L]]) / step
    expect_false(isTRUE(all.equal(implied, 60)))
    comp <- attr(shifted, "dsomop_release")$owners[[1L]]$components[[1L]]
    expect_equal(shifted$person_days[[1L]],
                 min(max(comp$time, 0), shifted$persons[[1L]] * 3650),
                 tolerance = 1e-6)
  }
  # Standard mode: person-days and MDRR are the protected ones of the same
  # question (DP service), or unavailable (no DP service); never exact.
  withr::local_options(list(dsomop.tools.mode = "standard"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  std <- release(60)
  expect_equal(std$person_days, base$person_days, tolerance = 1e-9)
  expect_equal(std$mdrr, base$mdrr, tolerance = 1e-9)
  withr::local_options(list(dsomop.dp.enabled = FALSE))
  .release_restart()
  off <- release(60)
  if (nrow(off)) {
    expect_true(all(is.na(off$person_days)))
    expect_true(all(is.na(off$mdrr)))
  }
})

test_that("incidence person-time is protected or unavailable on every route", {
  .release_state()
  h <- .sweep_cm_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  out <- .release_run(h, "dsomop:incidence.rate", list(
    outcome_concept_id = "4000002", tar_end_offset = "365"), scope = "sw_t")
  auto365 <- out
  label <- attr(out, "dsomop_protection")
  expect_identical(label$protection, "realised_sticky")
  expect_true(all(is.na(out$outcomes)))
  # The same effective window (366..390 all end at 390) replays exactly.
  out <- .release_run(h, "dsomop:incidence.rate", list(
    outcome_concept_id = "4000002", tar_end_offset = "371"), scope = "sw_t")
  again <- .release_run(h, "dsomop:incidence.rate", list(
    outcome_concept_id = "4000002", tar_end_offset = "389"), scope = "sw_t")
  expect_identical(.release_values(out), .release_values(again))
  withr::local_options(list(dsomop.tools.mode = "standard"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  std_run <- function() .release_run(h, "dsomop:incidence.rate", list(
    outcome_concept_id = "4000002", tar_end_offset = "365"), scope = "sw_t")
  # With the DP service: the protected person-time of the same question.
  std <- std_run()
  expect_identical(attr(std, "dsomop_protection")$protection, "mixed")
  expect_null(attr(std, "dsomop_protection")$masked_fields)
  expect_equal(std$person_days, auto365$person_days, tolerance = 1e-9)
  expect_equal(std$rate, auto365$rate, tolerance = 1e-9)
  # Without it: unavailable.
  withr::local_options(list(dsomop.dp.enabled = FALSE))
  .release_restart()
  off <- std_run()
  if (nrow(off)) {
    expect_true(all(is.na(off$person_days)))
    expect_true(all(is.na(off$rate)))
  }
  expect_setequal(attr(off, "dsomop_protection")$masked_fields,
                  c("person_days", "rate"))
})

test_that("SCCS window sweeps never reach exact conditional-Poisson scalars", {
  for (mode in c("auto", "standard")) {
    .release_state(mode = mode)
    h <- create_test_handle(n_persons = 200)
    DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
    DBI::dbExecute(h$conn, "DELETE FROM drug_exposure")
    set.seed(2)
    for (p in 1:80) {
      ex <- as.Date("2021-01-01") + sample(0:200, 1)
      DBI::dbExecute(h$conn, sprintf(paste0(
        "INSERT INTO drug_exposure (drug_exposure_id, person_id, ",
        "drug_concept_id, drug_exposure_start_date, drug_exposure_end_date, ",
        "drug_type_concept_id) VALUES (%d, %d, 1112807, '%s', '%s', ",
        "38000177)"), 5000L + p, p, format(ex), format(ex)))
      for (o in sample(c(-300:-1, 0:60), 2)) {
        .release_add_condition(h, p, 4000002L, format(ex + o))
      }
    }
    # The released scalars are post-processing of the protected q-gridded
    # event histogram (auto) or its mixed standard frame, on the public window
    # grid (20 -> 21, 25 -> 28, 30/35/40 -> 30); never the exact fit.
    fields <- c("irr", "ci_lo", "ci_hi", "log_irr", "se_log_irr")
    windows <- c(20, 25, 30, 35, 40)
    sweep <- lapply(windows, function(w) {
      .release_run(h, "dsomop:sccs.incidence_rate_ratio", list(
        exposure_concept_id = "1112807", outcome_concept_id = "4000002",
        window = as.character(w)))
    })
    for (i in seq_along(windows)) {
      r <- sweep[[i]]
      expect_identical(attr(r, "dsomop_protection")$protection,
                       if (identical(mode, "auto")) "realised_sticky" else
                         "mixed", info = mode)
      exact <- .omopSccsIrrFn()(h, list(scoped_cohort = NULL), list(
        exposure_concept_id = "1112807", outcome_concept_id = "4000002",
        window = as.character(windows[[i]])))
      if (nrow(r) && is.finite(r$se_log_irr) && nrow(exact)) {
        expect_false(isTRUE(all.equal(r$se_log_irr, exact$se_log_irr)),
                     info = mode)
      }
    }
    expect_identical(.release_values(sweep[[3L]])[, fields],
                     .release_values(sweep[[5L]])[, fields], info = mode)
    cleanup_handle(h)
  }
})

test_that("the protected Poisson rate ratio is post-processing of the displayed arms", {
  .release_state()
  # A large reference epsilon keeps this small fixture's arms supported.
  withr::local_options(list(dsomop.dp.release_epsilon = 8))
  .release_restart()
  h <- .sweep_cm_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  params <- list(outcome_concept_id = "4000002", model_type = "poisson",
                 tar_start_offset = "0", tar_end_offset = "365",
                 tar_anchor_start = "start", tar_anchor_end = "start")
  r <- .release_run(h, "dsomop:cm.effect_estimate", params,
                    scope = list("sw_t", "sw_c"))
  expect_identical(attr(r, "dsomop_protection")$protection, "realised_sticky")
  t <- r[r$arm == "target", ]
  c <- r[r$arm == "comparator", ]
  expect_true(all(is.finite(c(r$estimate, r$ci_lo, r$ci_hi, r$log_estimate,
                              r$se_log_estimate))))
  expect_equal(t$estimate, (t$outcomes / t$person_days) /
                 (c$outcomes / c$person_days), tolerance = 1e-9)
  expect_equal(log(t$estimate), t$log_estimate, tolerance = 1e-9)
  expect_true(t$ci_lo < t$estimate && t$estimate < t$ci_hi)
  expect_identical(c$estimate, t$estimate)
  # A Cox request releases the grouped-time Cox hazard ratio of the protected
  # exit-time x status basis (test-tools-release-survival.R), never this rate
  # ratio relabelled.
  params$model_type <- "cox"
  cox <- .release_run(h, "dsomop:cm.effect_estimate", params,
                      scope = list("sw_t", "sw_c"))
  expect_match(attr(cox, "dsomop_protection")$estimand, "grouped-time Cox")
  expect_false(isTRUE(all.equal(cox$estimate, r$estimate)))
  expect_identical(cox$persons, r$persons)
})
