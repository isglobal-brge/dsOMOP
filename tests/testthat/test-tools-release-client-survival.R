# Actual endpoint payloads exercise the matching client's signed-family path.
# Noise is a deterministic signed witness here; exact sampling and model SE
# have separate tests. This isolates display/export/pooling correctness.
.client_survival_handle <- function(site) {
  n <- 600L
  h <- create_test_handle(n_persons = n)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = seq_len(n), person_id = seq_len(n),
    observation_period_start_date = "2019-01-01",
    observation_period_end_date = "2022-12-31",
    period_type_concept_id = 44814724L), append = TRUE)
  DBI::dbExecute(h$conn, paste0("UPDATE person SET year_of_birth = CASE ",
    "WHEN person_id % 3 = 0 THEN 1990 WHEN person_id % 3 = 1 THEN 1970 ",
    "ELSE 1950 END, month_of_birth = 1, day_of_birth = 1"))
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  .release_dated_cohort(h, "pool_t", 1:300, "2020-01-01", "2021-12-31")
  .release_dated_cohort(h, "pool_c", 301:600, "2020-01-01", "2021-12-31")
  for (g in 0:2) {
    ids <- which(seq_len(n) %% 3 == g & seq_len(n) %% (site + 2L) != 0)
    .release_add_condition(h, ids, 4000002L,
                           format(as.Date("2020-01-01") + 40L + 50L * g))
  }
  .buildBlueprint(h, force = TRUE)
  h
}

.client_survival_expected <- function(owners) {
  o <- owners[[1L]]
  add <- function(parts) {
    z <- parts[[1L]]
    for (f in intersect(c("n", "status", "time", "h"), names(z))) {
      z[[f]] <- Reduce(`+`, lapply(parts, `[[`, f))
    }
    z
  }
  as_raw <- function(x) {
    ans <- list(n = x$n, e = x$status[[1L]], ne = x$status[[2L]], t = x$time)
    if (!is.null(x$h)) {
      ans$event <- x$h[o$status_of_cell == "event"]
      ans$nonevent <- x$h[o$status_of_cell != "event"]
    }
    ans
  }
  f <- o$family_basis
  if (is.null(f)) {
    comps <- lapply(seq_along(o$components), function(a) {
      add(lapply(owners, function(x) x$components[[a]]))
    })
    names(comps) <- vapply(comps, `[[`, character(1L), "stratum")
    return(lapply(comps, function(x) {
      N <- max(0, x$n); E <- .dsomopReleaseSimplex(x$status, N)[[1L]]
      list(n_display = N, events_display = E,
        cells = .dsomopReleaseC8Display(x$h[o$status_of_cell == "event"],
          x$h[o$status_of_cell != "event"], E, N, .dsomopReleaseExitMask(NULL)))
    }))
  }
  arm <- lapply(seq_along(f$arms), function(a) as_raw(add(lapply(owners,
    function(x) x$family_basis$arms[[a]]))))
  names(arm) <- names(f$arms)
  atoms <- lapply(seq_along(f$atoms), function(i) {
    ans <- lapply(seq_along(f$arms), function(a) as_raw(add(lapply(owners,
      function(x) x$family_basis$atoms[[i]][[a]]))))
    stats::setNames(ans, names(arm))
  })
  if (identical(o$family, "C8")) {
    return(.omopReleaseInteractionCoxPipeline(arm, atoms,
      .dsomopReleaseExitMask(NULL), f$view_of_atoms)$subgroups[[f$view_index]])
  }
  fam <- .omopReleaseFamilyC6(arm, atoms)
  idx <- f$view_of_atoms[[f$view_index]]
  stats::setNames(lapply(names(arm), function(a) {
    parts <- lapply(fam$subgroups[idx], `[[`, a)
    stats::setNames(lapply(names(parts[[1L]]), function(field) {
      Reduce(`+`, lapply(parts, `[[`, field))
    }), names(parts[[1L]]))
  }), names(arm))
}

test_that("actual C6/C8 endpoints pool complete signed interaction atoms before views", {
  skip_if_not_installed("dsOMOPClient")
  ns <- asNamespace("dsOMOPClient")
  if (!exists(".dp_pool_survival_owner", ns, inherits = FALSE)) {
    skip("requires matching signed interaction-family client")
  }
  pool <- get(".dp_pool_survival_owner", ns)
  .release_state()
  h <- lapply(1:2, .client_survival_handle)
  on.exit(lapply(h, cleanup_handle), add = TRUE)
  expect_true(suppressMessages(.dsomopToolsDpReady(h[[1L]]))$ready)
  local_mocked_bindings(
    .dsomopReleaseNoisyComponent = function(policy, keys, protection, component) {
      component$value + (strtoi(substr(component$fingerprint, 1L, 6L), 16L) %% 81L) - 40
    },
    .dsomopReleaseSimSd = function(label, values, epsilon, sensitivity, refit, ...) {
      rep(0, length(refit(values)))
    })
  params <- list(outcome_concept_id = "4000002", model_type = "cox",
    tar_start_offset = "0", tar_end_offset = "365", tar_anchor_start = "start",
    tar_anchor_end = "start")
  endpoint <- function(handle, breaks) .release_run(handle,
    "dsomop:cm.interaction_estimate", c(params, list(subgroup_kind = "age",
      age_breaks = breaks)), list("pool_t", "pool_c"))
  fine <- lapply(h, endpoint, breaks = "40,60")
  coarse <- lapply(h, endpoint, breaks = "60")
  owners_of <- function(x) attr(x, "dsomop_release")$owners
  child <- function(x, family) Filter(function(o) identical(o$family, family) &&
    !is.null(o$family_basis), owners_of(x))
  compare <- function(owners) {
    got <- pool(owners)
    want <- .client_survival_expected(owners)
    for (a in names(want)) {
      row <- got$frame[got$frame$stratum == a, , drop = FALSE]
      expect_equal(row$persons, want[[a]]$n_display, tolerance = 1e-6)
      expect_equal(row$events, want[[a]]$events_display, tolerance = 1e-6)
      if (identical(owners[[1L]]$family, "C6")) {
        expect_equal(row$person_time, want[[a]]$time_display, tolerance = 1e-6)
      } else {
        cells <- got$cells[got$cells$stratum == a, , drop = FALSE]
        flat <- as.vector(t(want[[a]]$cells))
        mask <- as.vector(t(.dsomopReleaseExitMask(NULL)))
        expect_equal(cells$count, flat[mask], tolerance = 1e-6)
        expect_equal(sum(cells$count[owners[[1L]]$status_of_cell == "event"]),
                     row$events, tolerance = 1e-6)
        expect_true(all(cells$count >= 0))
      }
    }
    got
  }
  for (family in c("C6", "C8")) {
    fo <- lapply(fine, child, family = family)
    co <- lapply(coarse, child, family = family)
    for (site in 1:2) {
      expect_identical(fo[[site]][[1L]]$family_basis$atoms,
                       co[[site]][[1L]]$family_basis$atoms)
      expect_identical(fo[[site]][[1L]]$family_basis$family_id, list(kind = "age5"))
      expect_identical(names(fo[[site]][[1L]]$family_basis$atoms),
        paste0("age5=", c("neg", seq(0, 110, 5), "unknown")))
      for (i in seq_along(fo[[site]])) {
        got <- compare(list(fo[[site]][[i]]))
        if (identical(family, "C6")) {
          for (a in c("target", "comparator")) {
            row <- got$frame[got$frame$stratum == paste0("arm=", a), ]
            expect_equal(row$persons, fine[[site]][[paste0(a, "_persons")]][i],
                         tolerance = 1e-6)
            expect_equal(row$events, fine[[site]][[paste0(a, "_outcomes")]][i],
                         tolerance = 1e-6)
          }
        }
      }
    }
    pf <- lapply(seq_along(fo[[1L]]), function(i) compare(lapply(fo, `[[`, i)))
    pc <- lapply(seq_along(co[[1L]]), function(i) compare(lapply(co, `[[`, i)))
    for (col in c("persons", "events", if (family == "C6") "person_time")) {
      expect_equal(pc[[1L]]$frame[[col]], pf[[1L]]$frame[[col]] + pf[[2L]]$frame[[col]],
                   tolerance = 1e-6)
    }
    if (family == "C8") expect_equal(pc[[1L]]$cells$count,
      pf[[1L]]$cells$count + pf[[2L]]$cells$count, tolerance = 1e-6)
  }
  ordinary <- .release_run(h[[1L]], "dsomop:cm.effect_estimate", params,
                           list("pool_t", "pool_c"))
  o8 <- Filter(function(o) identical(o$family, "C8"), owners_of(ordinary))[[1L]]
  result <- compare(list(o8))
  expect_equal(result$frame$events, ordinary$outcomes, tolerance = 1e-6)
  # Demonstrate that treating all statuses as a single simplex was wrong.
  naive <- vapply(o8$components, function(x) sum(.dsomopReleaseSimplex(x$h,
    max(0, x$n))[o8$status_of_cell == "event"]), numeric(1L))
  expect_gt(max(abs(naive - result$frame$events)), 1e-4)
  integrated <- get(".pool_protected_release_frames", ns)(list(site = fine[[1L]]))
  expect_true(nrow(integrated$result) > 0)
})
