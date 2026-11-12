# Protected char.time_to_event (Astra re-review R07, deviation 19): the
# capped recurrent estimand (first_occurrence_only = 0, the default) and the
# first-event estimand are views of one common signed-day basis, so the 1-,
# 30- and 365-day scales never redraw the same events. Helpers in
# helper-tools-release.R.

.tte_run <- function(h, scope, scale = "1", first = "0", outcome = "4000002") {
  .release_run(h, "dsomop:char.time_to_event",
               list(outcome_concept_id = outcome, time_scale = scale,
                    first_occurrence_only = first), scope = scope)
}

.tte_without_noise <- function(.local_envir = parent.frame()) {
  testthat::local_mocked_bindings(
    .dsomopReleaseNoisyComponent = function(policy, keys, protection,
                                            component) component$value,
    .env = .local_envir)
}

# A cohort of 60 persons with two episodes each and recurrent outcomes.
.tte_fixture <- function(n = 60) {
  h <- create_test_handle(n_persons = n)
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  DBI::dbExecute(h$conn, paste(
    "CREATE TEMP TABLE tte_cohort (cohort_definition_id INTEGER,",
    "subject_id INTEGER, cohort_start_date TEXT, cohort_end_date TEXT)"))
  DBI::dbWriteTable(h$conn, "tte_cohort", data.frame(
    cohort_definition_id = 1L, subject_id = rep(seq_len(n), 2),
    cohort_start_date = rep(c("2015-01-01", "2016-01-01"), each = n),
    cohort_end_date = rep(c("2015-12-31", "2016-12-31"), each = n)),
    append = TRUE)
  register_test_temp(h, "tte_cohort")
  id <- 70000L
  for (p in seq_len(n)) {
    days <- c(3L, 40L, 400L, -20L)[seq_len(1L + p %% 4L)]
    for (d in days) {
      id <- id + 1L
      DBI::dbExecute(h$conn, sprintf(paste0(
        "INSERT INTO condition_occurrence (condition_occurrence_id, ",
        "person_id, condition_concept_id, condition_start_date, ",
        "condition_end_date, condition_type_concept_id) VALUES (%d, %d, ",
        "4000002, '%s', '%s', 32020)"), id, p,
        format(as.Date("2015-01-01") + d), format(as.Date("2015-01-01") + d)))
    }
  }
  list(handle = h, scope = "tte_cohort")
}

# Exact capped occurrences: (event, episode) offsets in date/key/episode
# order, first K per person.
.tte_exact_occurrences <- function(h, K = 10L) {
  occ <- DBI::dbGetQuery(h$conn, paste(
    "SELECT DISTINCT c.subject_id AS person_id, o.condition_occurrence_id AS",
    "event_id, o.condition_start_date AS event_date, c.cohort_start_date AS",
    "episode_start, julianday(o.condition_start_date) -",
    "julianday(c.cohort_start_date) AS v FROM tte_cohort c INNER JOIN",
    "condition_occurrence o ON o.person_id = c.subject_id AND",
    "o.condition_concept_id = 4000002"))
  occ <- occ[order(occ$person_id, occ$event_date, occ$event_id,
                   occ$episode_start), ]
  occ$rank <- stats::ave(seq_len(nrow(occ)), occ$person_id, FUN = seq_along)
  occ[occ$rank <= K, , drop = FALSE]
}

.tte_bin_sums <- function(v, out) {
  lower <- out$day_offset
  upper <- c(out$day_offset[-1L], Inf)
  vapply(seq_along(lower), function(i) sum(v >= lower[[i]] & v < upper[[i]]),
         numeric(1L))
}

test_that("the common signed-day basis nests its three levels", {
  basis <- .dsomopReleaseTteBasis()
  expect_identical(basis$id, "tte-signed-day-basis-v1")
  expect_equal(nrow(basis$A), 22L)
  expect_identical(basis$A$key[[1L]], "A:(-Inf, -3650)")
  expect_identical(basis$A$key[[22L]], "A:[3650, Inf)")
  # Every multiple of 30 and of 365 inside [-3650, 3650] is a B boundary.
  b_edges <- c(basis$B$lower, basis$B$upper[[nrow(basis$B)]])
  expect_true(all(c(30 * (-121:121), 365 * (-10:10)) %in% b_edges))
  expect_equal(nrow(basis$C), 730L)
  expect_true(all(basis$B$lower >= basis$A$lower[basis$B$parent] &
                    basis$B$upper <= basis$A$upper[basis$B$parent]))
  expect_true(all(basis$C$lower >= basis$B$lower[basis$C$parent] &
                    basis$C$upper <= basis$B$upper[basis$C$parent]))
  for (scale in c(1L, 30L, 365L)) {
    view <- .dsomopReleaseTteView(basis, scale)
    # The bins partition the real line in increasing order.
    lower <- view$bins$day_offset
    upper <- view$bins$upper
    expect_identical(lower[[1L]], -Inf, info = scale)
    expect_identical(upper[[length(upper)]], Inf, info = scale)
    expect_identical(lower[-1L], upper[-length(upper)], info = scale)
  }
})

test_that("without noise the recurrent view is the capped occurrence count", {
  .release_state()
  .tte_without_noise()
  f <- .tte_fixture()
  on.exit(cleanup_handle(f$handle), add = TRUE)
  occ <- .tte_exact_occurrences(f$handle)
  for (scale in c("1", "30", "365")) {
    out <- .tte_run(f$handle, f$scope, scale)
    expect_identical(names(out), c("day_offset", "num_events", "persons"))
    expect_equal(out$num_events, .tte_bin_sums(occ$v, out), info = scale)
    expect_true(all(is.na(out$persons)), info = scale)
    label <- attr(out, "dsomop_protection")
    expect_identical(label$estimand_kind, "recurrent")
    expect_match(label$persons_unavailable, "not additive")
    expect_match(label$estimand, "capped recurrent outcome events")
  }
  # First events: one per person, from the first episode; persons = events.
  first <- DBI::dbGetQuery(f$handle$conn, paste(
    "SELECT o.person_id, MIN(julianday(o.condition_start_date)) -",
    "julianday('2015-01-01') AS v FROM condition_occurrence o WHERE",
    "o.condition_concept_id = 4000002 GROUP BY o.person_id"))
  for (scale in c("1", "30", "365")) {
    out <- .tte_run(f$handle, f$scope, scale, first = "1")
    expect_equal(out$num_events, .tte_bin_sums(first$v, out), info = scale)
    expect_identical(out$persons, out$num_events, info = scale)
    expect_identical(attr(out, "dsomop_protection")$estimand_kind, "first")
  }
})

test_that("the whole-person cap keeps the first K occurrences across episodes", {
  .release_state()
  .tte_without_noise()
  f <- .tte_fixture()
  on.exit(cleanup_handle(f$handle), add = TRUE)
  # Person 1 gets 8 more events (10 events x 2 episodes = 20 occurrences).
  for (k in 1:8) .release_add_condition(f$handle, 1L, 4000002L,
                                        format(as.Date("2017-01-01") + k))
  occ <- .tte_exact_occurrences(f$handle)
  expect_equal(sum(occ$person_id == 1L), 10)
  out <- .tte_run(f$handle, f$scope, "365")
  expect_equal(out$num_events, .tte_bin_sums(occ$v, out))
  expect_equal(sum(out$num_events), nrow(occ))
})

test_that("1-, 30- and 365-day views are sums of one draw of the common basis", {
  .release_state()
  withr::local_options(list(dsomop.dp.release_epsilon = 8))
  .release_restart()
  f <- .tte_fixture()
  on.exit(cleanup_handle(f$handle), add = TRUE)
  for (first in c("0", "1")) {
    d1 <- .tte_run(f$handle, f$scope, "1", first)
    d30 <- .tte_run(f$handle, f$scope, "30", first)
    d365 <- .tte_run(f$handle, f$scope, "365", first)
    total <- sum(d365$num_events)
    expect_gt(total, 0)
    expect_equal(sum(d30$num_events), total, tolerance = 1e-6, info = first)
    expect_equal(sum(d1$num_events), total, tolerance = 1e-6, info = first)
    # A 365-day bin inside [-365, 365) is the sum of its days.
    day_sum <- function(lo, hi) {
      sum(d1$num_events[d1$day_offset >= lo & d1$day_offset < hi])
    }
    expect_equal(d365$num_events[d365$day_offset == 0], day_sum(0, 365),
                 tolerance = 1e-6, info = first)
    expect_equal(d365$num_events[d365$day_offset == -365], day_sum(-365, 0),
                 tolerance = 1e-6, info = first)
    # A 30-day bin inside [-365, 365) is the sum of its days.
    for (lo in c(-360, -30, 0, 30, 330)) {
      expect_equal(d30$num_events[d30$day_offset == lo],
                   day_sum(lo, lo + 30), tolerance = 1e-6,
                   info = paste(first, lo))
    }
    # The day view's upper tail is the sum of the 365-day bins above 365.
    expect_equal(d1$num_events[d1$day_offset == 365],
                 sum(d365$num_events[d365$day_offset >= 365]),
                 tolerance = 1e-6, info = first)
  }
})

test_that("equal occurrence maps replay; the estimands are distinct owners", {
  .release_state()
  f <- .tte_fixture()
  on.exit(cleanup_handle(f$handle), add = TRUE)
  a <- .tte_run(f$handle, f$scope, "30")
  DBI::dbExecute(f$handle$conn,
                 "CREATE TEMP TABLE tte_copy AS SELECT * FROM tte_cohort")
  register_test_temp(f$handle, "tte_copy")
  b <- .tte_run(f$handle, "tte_copy", "30")
  expect_identical(.release_values(b), .release_values(a))
  first <- .tte_run(f$handle, f$scope, "30", first = "1")
  ra <- attr(a, "dsomop_release")$owners[[1L]]
  rf <- attr(first, "dsomop_release")$owners[[1L]]
  expect_false(identical(ra$owner, rf$owner))
  expect_identical(ra$estimand_kind, "recurrent")
  expect_identical(rf$estimand_kind, "first")
  expect_identical(ra$basis, rf$basis)
  # Recurrent cells are weighted: they are never exported as person cells.
  expect_null(ra$components[[1L]]$h)
  expect_false(is.null(rf$components[[1L]]$h))
  expect_equal(sum(ra$components[[1L]]$levels$A$signed),
               ra$components[[1L]]$r, tolerance = 1e-9)
  expect_equal(sum(rf$components[[1L]]$levels$A$signed),
               rf$components[[1L]]$n, tolerance = 1e-9)
  lab <- attr(a, "dsomop_protection")$components[[1L]]
  expect_identical(lab$cap_scope,
                   "whole_person_occurrences_across_episodes_and_bins")
  expect_equal(lab$sensitivity$A, 10)
  expect_equal(lab$sensitivity$N, 1)
  expect_identical(names(lab$allocation_fraction), c("N", "R", "A", "B"))
  # The node draws do not depend on the requested scale: the A level of a
  # 1-day request equals the A level of a 30-day request.
  d1 <- .tte_run(f$handle, f$scope, "1")
  expect_identical(attr(d1, "dsomop_release")$owners[[1L]]$components[[1L]]$levels$A,
                   ra$components[[1L]]$levels$A)
  expect_identical(attr(d1, "dsomop_release")$owners[[1L]]$components[[1L]]$levels$B,
                   ra$components[[1L]]$levels$B)
})

test_that("standard mode keeps the standard time-to-event histogram", {
  .release_state(mode = "standard")
  f <- .tte_fixture()
  on.exit(cleanup_handle(f$handle), add = TRUE)
  suppressWarnings(.omopAnalysisRegistry(f$handle, force = TRUE))
  out <- .tte_run(f$handle, f$scope, "30")
  expect_identical(attr(out, "dsomop_protection")$protection, "standard")
  expect_false(any(is.infinite(out$day_offset)))
})
