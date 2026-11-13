# Astra's literal original-size reproductions (astra-p0-review.md N03): the
# 12/13-person E1 adjacent-scope algebra and the original 1000-person
# min_days / reference-year / SCCS attacker algorithms, run on the actual
# endpoints. Their defect-witness assertions are inverted where the review
# required the fix (B09: no standard min_days transition; B13: protected
# measurement moments exist but the 7.85 equation has no solution). Fixtures
# in tests/fixtures/astra (Astra's scratch data). Helpers in
# helper-tools-release.R.

.astra_fixture <- function(name) {
  for (path in c(file.path("..", "fixtures", "astra", name),
                 file.path("tests", "fixtures", "astra", name))) {
    if (file.exists(path)) return(utils::read.csv(path))
  }
  stop("Missing Astra fixture ", name)
}

# The adjacent-scope solver of the review: persons n and value x that make two
# released (mean, SD) pairs consistent with one added person.
.astra_solve <- function(a, b) {
  if (anyNA(c(a$avg_value, a$stdev_value, b$avg_value, b$stdev_value))) {
    return(data.frame())
  }
  n <- 3:100
  x <- (n + 1) * b$avg_value - n * a$avg_value
  q1 <- (n - 1) * a$stdev_value^2 + n * a$avg_value^2
  q2 <- n * b$stdev_value^2 + (n + 1) * b$avg_value^2
  data.frame(n = n, x = x)[abs(q1 + x * x - q2) < 1e-6, , drop = FALSE]
}

test_that("E1 original 12/13-person age-34 reconstruction is closed", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  ages <- c(20, 24, 27, 29, 33, 36, 41, 44, 47, 51, 55, 60, 34)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = 1:60, person_id = 1:60,
    observation_period_start_date = "2020-01-01",
    observation_period_end_date = "2025-12-31",
    period_type_concept_id = 44814724L), append = TRUE)
  for (i in seq_along(ages)) {
    DBI::dbExecute(h$conn, paste0("UPDATE person SET year_of_birth=",
                                  2020 - ages[i], " WHERE person_id=", i))
  }
  exact <- function(x) data.frame(count_value = 5 * floor(length(x) / 5),
                                  avg_value = mean(x), stdev_value = sd(x))
  # The 2.10.2 algebra recovers the added person exactly.
  old <- .astra_solve(exact(ages[1:12]), exact(ages))
  expect_true(any(old$n == 12 & abs(old$x - 34) < 1e-8))
  a <- .release_cohort(h, "astra_e1_a", 1:12)
  b <- .release_cohort(h, "astra_e1_b", 1:13)
  ra <- .release_run(h, "dsomop:achilles.103", scope = a)
  rb <- .release_run(h, "dsomop:achilles.103", scope = b)
  expect_identical(attr(ra, "dsomop_protection")$protection, "realised_sticky")
  expect_identical(attr(rb, "dsomop_protection")$protection, "realised_sticky")
  now <- .astra_solve(ra, rb)
  expect_false(nrow(now) > 0 && any(now$n == 12 & abs(now$x - 34) < 1e-8))
  # Standard mode releases no exact scoped moments at all.
  withr::local_options(list(dsomop.tools.mode = "standard"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  sa <- .release_run(h, "dsomop:achilles.103", scope = a)
  sb <- .release_run(h, "dsomop:achilles.103", scope = b)
  expect_true(all(is.na(sa[, .DSOMOP_RELEASE_DIST_FIELDS])))
  expect_true(all(is.na(sb[, .DSOMOP_RELEASE_DIST_FIELDS])))
})

test_that("E2 original 29/30-person HbA1c-7.85 reconstruction is closed", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM measurement")
  values <- c(5 + (1:29 %% 7) * .45, 7.85)
  DBI::dbWriteTable(h$conn, "measurement", data.frame(
    measurement_id = 1:30, person_id = 1:30, measurement_concept_id = 3004410L,
    measurement_date = "2021-03-01", measurement_type_concept_id = 44818518L,
    value_as_number = values, value_as_concept_id = NA_integer_,
    unit_concept_id = 8554L, range_low = 4, range_high = 6,
    visit_occurrence_id = NA_integer_), append = TRUE)
  exact <- function(x) data.frame(avg_value = mean(x), stdev_value = sd(x))
  old <- .astra_solve(exact(values[1:29]), exact(values))
  expect_true(any(old$n == 29 & abs(old$x - 7.85) < 1e-8))
  a <- .release_cohort(h, "astra_e2_a", 1:29)
  b <- .release_cohort(h, "astra_e2_b", 1:30)
  run <- function(scope) .release_run(h, "dsomop:fe.continuous", list(
    metric = "measurement_value", domain_code = "3"), scope = scope)
  ra <- run(a)
  rb <- run(b)
  expect_equal(nrow(ra), 1L)
  expect_equal(nrow(rb), 1L)
  # B13: HbA1c in % is a registered scale, so protected moments are released
  # (from noisy S and Q components); the original equation has no solution.
  now <- .astra_solve(ra, rb)
  expect_false(nrow(now) > 0 && any(now$n == 29 & abs(now$x - 7.85) < 1e-6))
})

test_that("the original min_days and reference-year attacker algorithms fail", {
  lens <- .astra_fixture("observation_lengths.csv")$len
  years <- .astra_fixture("birth_years.csv")$yob
  for (mode in c("auto", "standard")) {
    .release_state(mode = mode)
    h <- create_test_handle(n_persons = length(lens))
    DBI::dbExecute(h$conn, "DELETE FROM observation_period")
    DBI::dbWriteTable(h$conn, "observation_period", data.frame(
      observation_period_id = seq_along(lens), person_id = seq_along(lens),
      observation_period_start_date = "2000-01-01",
      observation_period_end_date = as.character(as.Date("2000-01-01") + lens),
      period_type_concept_id = 44814724L), append = TRUE)
    DBI::dbWriteTable(h$conn, "astra_birth",
                      data.frame(person_id = seq_along(years), yob = years))
    DBI::dbExecute(h$conn, paste(
      "UPDATE person SET year_of_birth = (SELECT yob FROM astra_birth",
      "WHERE astra_birth.person_id = person.person_id)"))
    rel <- function(d) {
      r <- .release_run(h, "dsomop:observation_period.long_period_count",
                        list(min_days = as.character(d)))
      if (nrow(r)) r$n_persons[[1L]] else NA_real_
    }
    # Minimum by bisection on "unchanged answer".
    base <- rel(-1)
    lo <- -1
    hi <- max(lens) + 2
    while (hi - lo > 1) {
      mid <- (lo + hi) %/% 2
      if (identical(rel(mid), base)) lo <- mid else hi <- mid
    }
    expect_false(identical(as.integer(hi - 1), as.integer(min(lens))),
                 info = mode)
    # Third largest by bisection on "row present".
    lo <- 0
    hi <- max(lens) + 2
    while (hi - lo > 1) {
      mid <- (lo + hi) %/% 2
      if (!is.na(rel(mid))) lo <- mid else hi <- mid
    }
    expect_false(identical(as.integer(lo),
                           as.integer(sort(lens, decreasing = TRUE)[3])),
                 info = mode)
    # The review's extended bracket: on the reviewed head standard mode lost
    # its row at 5476 (the raw tail support oracle, B09); now the row is
    # present up to the end of the public scale in both modes.
    lo2 <- 0
    hi2 <- 10959
    while (hi2 - lo2 > 1) {
      mid <- (lo2 + hi2) %/% 2
      if (!is.na(rel(mid))) lo2 <- mid else hi2 <- mid
    }
    expect_equal(lo2, 10958, info = mode)
    # Third oldest by sweeping the reference year.
    first <- NA_real_
    outs <- list()
    for (y in 1980:2010) {
      r <- .release_run(h, "dsomop:person.age_distribution_decade",
                        list(reference_year = as.character(y)))
      outs[[as.character(y)]] <- .release_values(r)
      if (is.na(first) && "90+" %in% r$age_group) first <- y
    }
    expect_false(identical(as.integer(first - 90), as.integer(sort(years)[3])),
                 info = mode)
    expect_true(all(vapply(outs, function(r) "90+" %in% r$age_group,
                           logical(1))), info = mode)
    if (identical(mode, "standard")) {
      expect_true(all(vapply(outs, identical, logical(1), outs[[1L]])))
    }
    cleanup_handle(h)
  }
})

test_that("the original SCCS window sweep yields no exact log-IRR differences", {
  for (mode in c("auto", "standard")) {
    .release_state(mode = mode)
    h <- create_test_handle(n_persons = 200)
    DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
    DBI::dbExecute(h$conn, "DELETE FROM drug_exposure")
    set.seed(2)
    for (p in 1:80) {
      ex <- as.Date("2021-01-01") + sample(0:200, 1)
      DBI::dbExecute(h$conn, sprintf(paste(
        "INSERT INTO drug_exposure (drug_exposure_id, person_id,",
        "drug_concept_id, drug_exposure_start_date, drug_exposure_end_date,",
        "drug_type_concept_id) VALUES (%d, %d, 1112807, '%s', '%s',",
        "38000177)"), 5000 + p, p, format(ex), format(ex)))
      for (o in sample(c(-300:-1, 0:60), 2)) {
        .release_add_condition(h, p, 4000002L, format(ex + o))
      }
    }
    runs <- lapply(20:40, function(w) {
      .release_run(h, "dsomop:sccs.incidence_rate_ratio", list(
        exposure_concept_id = "1112807", outcome_concept_id = "4000002",
        window = as.character(w)))
    })
    lirr <- vapply(runs, function(r) {
      if (nrow(r)) r$log_irr[[1L]] else NA_real_
    }, numeric(1L))
    # No exact conditional-Poisson scalar is released: every finite value is
    # a protected release, and windows that snap to one public window replay
    # the same answer (so their differences carry no timing information).
    for (i in which(is.finite(lirr))) {
      label <- attr(runs[[i]], "dsomop_protection")
      expect_true(label$protection %in% c("realised_sticky", "fixed_synopsis",
                                          "mixed"), info = mode)
    }
    effective <- vapply(runs, function(r) {
      w <- attr(r, "dsomop_protection")$effective_parameters$window$effective
      if (is.null(w)) NA_character_ else as.character(w)
    }, character(1L))
    for (w in unique(effective[!is.na(effective)])) {
      same <- lirr[which(effective == w)]
      expect_true(length(unique(same)) <= 1L, info = paste(mode, w))
    }
    cleanup_handle(h)
  }
})
