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
    lower <- out$day_offset; upper <- c(lower[-1L], Inf)
    person_truth <- vapply(seq_along(lower), function(i) {
      length(unique(occ$person_id[occ$v >= lower[[i]] & occ$v < upper[[i]]]))
    }, numeric(1L))
    expect_equal(out$persons, person_truth, info = scale)
    label <- attr(out, "dsomop_protection")
    expect_identical(label$estimand_kind, "recurrent")
    expect_match(label$persons_estimand, "same K-capped occurrences")
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
  expect_identical(names(lab$allocation_fraction), c("N", "R", "A", "B", "C", "P"))
  expect_equal(sum(unlist(lab$allocation_fraction)), 1)
  expect_equal(lab$sensitivity$P, 30)
  expect_match(lab$presence$projection, "radial")
  # The node draws do not depend on the requested scale: the A level of a
  # 1-day request equals the A level of a 30-day request.
  d1 <- .tte_run(f$handle, f$scope, "1")
  expect_identical(attr(d1, "dsomop_release")$owners[[1L]]$components[[1L]]$levels$A,
                   ra$components[[1L]]$levels$A)
  expect_identical(attr(d1, "dsomop_release")$owners[[1L]]$components[[1L]]$levels$B,
                   ra$components[[1L]]$levels$B)
  expect_identical(attr(d1, "dsomop_release")$owners[[1L]]$components[[1L]]$p,
                   ra$components[[1L]]$p)
  expect_length(ra$components[[1L]]$p, nrow(ra$presence_basis$intervals))
  field_names <- function(x) if (is.list(x)) {
    c(names(x), unlist(lapply(x, field_names), use.names = FALSE))
  } else character(0)
  expect_false(any(field_names(attr(d1, "dsomop_release")) %in%
    c("fingerprint", "tokens", "noise_root", "data_generation", "person_id")))
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

test_that("actual first and recurrent time-to-event frames preserve client pooling", {
  skip_if_not_installed("dsOMOPClient")
  cl <- asNamespace("dsOMOPClient")
  expect_true(exists(".pool_protected_tte", envir = cl))
  .release_state()
  fixtures <- list(a = .tte_fixture(60), b = .tte_fixture(80))
  on.exit(lapply(fixtures, function(f) cleanup_handle(f$handle)), add = TRUE)
  for (first in c("0", "1")) {
    views <- lapply(c("1", "30", "365"), function(scale) {
      frames <- lapply(seq_along(fixtures), function(i) {
        withr::with_options(list(
          dsomop.dp.domain = paste0("tte-pooling-site-", i),
          dsomop.dp.noise_root = paste(rep(if (i == 1L) "6a" else "7b", 32),
                                        collapse = "")
        ), {
          .release_restart()
          .tte_run(fixtures[[i]]$handle, fixtures[[i]]$scope, scale, first)
        })
      })
      names(frames) <- names(fixtures)
      rp <- .omopAnalysisGet(fixtures$a$handle,
                             "dsomop:char.time_to_event")$release_pooling
      for (i in seq_along(frames)) {
        one <- cl$.pool_protected_analysis(frames[i], rp)$result
        expect_identical(names(one), names(frames[[i]]))
        for (col in names(one)) {
          expect_equal(one[[col]], frames[[i]][[col]], tolerance = 1e-8,
                       info = paste(first, scale, names(frames)[[i]], col))
        }
      }
      pooled <- cl$.pool_protected_analysis(frames, rp)$result
      owners <- lapply(frames, function(x) attr(x, "dsomop_release")$owners[[1]])
      field <- if (first == "1") "n" else "r"
      total <- max(0, sum(vapply(owners, function(o) o$components[[1]][[field]],
                                 numeric(1))))
      N <- max(0, sum(vapply(owners, function(o) o$components[[1]]$n, numeric(1))))
      if (first == "0") total <- min(10 * N, max(N, total))
      expect_equal(sum(pooled$num_events), total, tolerance = 1e-8)
      if (first == "1") expect_equal(pooled$persons, pooled$num_events)
      else {
        expect_true(all(is.finite(pooled$persons)))
        expect_true(all(pooled$persons >= pooled$num_events / 10 - 1e-7))
        expect_true(all(pooled$persons <= pmin(N, pooled$num_events) + 1e-7))
        expect_true(sum(pooled$persons) >= N - 1e-7)
        expect_true(sum(pooled$persons) <= 10 * N + 1e-7)
      }
      pooled
    })
    if (first == "0") {
      for (lo in c(-365, 0)) {
        fine <- views[[1]]$persons[views[[1]]$day_offset >= lo &
                                    views[[1]]$day_offset < lo + 365]
        parent <- views[[3]]$persons[views[[3]]$day_offset == lo]
        expect_true(max(fine) <= parent + 1e-7)
        expect_true(parent <= sum(fine) + 1e-7)
        expect_true(sum(fine) <= 10 * parent + 1e-7)
      }
    }
    # The central 365-day bin is also the sum of its 365 one-day children.
    expect_equal(sum(views[[1]]$num_events[views[[1]]$day_offset >= 0 &
                                           views[[1]]$day_offset < 365]),
                 views[[3]]$num_events[views[[3]]$day_offset == 0],
                 tolerance = 1e-8)
  }
})

test_that("the complete recurrent presence basis has bounded feasible projections", {
  b <- .dsomopReleaseTtePresenceBasis(.dsomopReleaseTteBasis())
  expect_equal(nrow(b$intervals), 998L)
  expect_false(anyDuplicated(b$intervals$key) > 0)
  person <- c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L)
  value <- c(-370, 0, 1, 0, 40, 400, -20, 3)
  E <- vapply(seq_len(nrow(b$intervals)), function(i) {
    sum(value >= b$intervals$lower[[i]] & value < b$intervals$upper[[i]])
  }, numeric(1L))
  P <- vapply(seq_len(nrow(b$intervals)), function(i) {
    length(unique(person[value >= b$intervals$lower[[i]] &
                           value < b$intervals$upper[[i]]]))
  }, numeric(1L))
  project <- function(x) .dsomopReleaseTtePresenceProject(x, E, 3, 8, 10, b)
  expect_identical(project(P), P)
  # Distinct persons in a coarse bin are not sums of its daily counts.
  d <- b$views[["1"]]; y <- b$views[["365"]]
  parent <- y[b$intervals$lower[y] == 0]
  kids <- d[b$intervals$lower[d] >= 0 & b$intervals$upper[d] <= 365]
  expect_lt(P[parent], sum(P[kids]))
  noisy <- P + 100 * sin(seq_along(P))
  shown <- project(noisy)
  expect_identical(project(noisy), shown)
  expect_equal(project(shown), shown)
  expect_true(all(shown >= E / 10 - 1e-9 & shown <= pmin(3, E) + 1e-9))
  expect_true(all(shown[b$contains[, 1]] <= shown[b$contains[, 2]] + 1e-9))
  for (part in b$partitions) {
    parent <- if (part$parent == 0L) 3 else shown[[part$parent]]
    total <- sum(shown[part$children])
    expect_true(total >= parent - 1e-9 && total <= 10 * parent + 1e-9)
  }
  expect_identical(.dsomopReleaseTtePresenceProject(noisy, E * 0, 0, 0, 10, b),
                   numeric(length(P)))
  if (requireNamespace("dsOMOPClient", quietly = TRUE)) {
    cp <- get(".dp_tte_presence_project", asNamespace("dsOMOPClient"))
    expect_identical(cp(noisy, E, 3, 8, 10, b), shown)
  }
})
