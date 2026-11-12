# Regressions of Astra's 2.10.3 implementation review (astra-p0-review.md):
# B03 public marginal hierarchy, B06 scientific person-time owners, B09
# always-protected threshold view, B10 protected horizon, N01 view reference
# year, B12/B14 release source and delta claims. Each reproduction failed on
# the reviewed heads and must pass here. Helpers in helper-tools-release.R.

.review_components <- function(x, i = 1L) {
  attr(x, "dsomop_release")$owners[[i]]$components
}

test_that("B03: a public gender decomposition reconciles to the canonical root N", {
  for (route in c("fixed_synopsis", "realised_sticky")) {
    if (identical(route, "fixed_synopsis")) {
      .release_state()
      h <- create_test_handle(n_persons = 2000L)
    } else {
      .release_state(snapshot_id = NULL)
      h <- .release_derived_handle(n_persons = 2000L)
    }
    a <- .release_run(h, "dsomop:achilles.103")
    b <- .release_run(h, "dsomop:achilles.104")
    expect_identical(attr(a, "dsomop_protection")$protection, route)
    root <- sum(vapply(.review_components(a), `[[`, numeric(1L), "n"))
    child <- sum(vapply(.review_components(b), `[[`, numeric(1L), "n"))
    # The review's probe: one predeclared hierarchy for the same population.
    expect_equal(root, child, tolerance = 1e-9, info = route)
    expect_equal(attr(b, "dsomop_release")$owners[[1L]]$root$n, root,
                 tolerance = 1e-9, info = route)
    # Displayed children are projected onto the parent's displayed total.
    expect_equal(sum(b$count_value), a$count_value, tolerance = 1e-6,
                 info = route)
    # A child request never moves its parent.
    expect_identical(.release_values(.release_run(h, "dsomop:achilles.103")),
                     .release_values(a))
    # The C2 gender view of the same first-observation age shares the root.
    c2 <- .release_run(h, "dsomop:achilles.102")
    expect_equal(sum(vapply(.review_components(c2), `[[`, numeric(1L), "n")),
                 root, tolerance = 1e-9, info = route)
    cleanup_handle(h)
  }
})

test_that("B03: incidence strata reconcile to the persons-at-risk total", {
  .release_state()
  h <- create_test_handle(n_persons = 200)
  on.exit(cleanup_handle(h), add = TRUE)
  .release_dated_cohort(h, "review_inc", 1:150, start = "2021-01-01",
                        end = "2023-12-31")
  params <- list(outcome_concept_id = "4000002", tar_end_offset = "365")
  all <- .release_run(h, "dsomop:incidence.rate", params, scope = "review_inc")
  by_gender <- .release_run(h, "dsomop:incidence.rate",
                            c(params, list(strat_by_gender = "1")),
                            scope = "review_inc")
  root <- .review_components(all)[[1L]]$n
  owner <- attr(by_gender, "dsomop_release")$owners[[1L]]
  expect_equal(owner$root$n, root, tolerance = 1e-9)
  expect_equal(sum(vapply(owner$components, `[[`, numeric(1L), "n")), root,
               tolerance = 1e-9)
  expect_equal(sum(by_gender$persons_at_risk), all$persons_at_risk,
               tolerance = 1e-6)
  expect_true("N_strata" %in% names(attr(by_gender, "dsomop_protection")$
                                      components[[1L]]$allocation_fraction))
})

test_that("B06: equivalent interaction subgroups share their protected components", {
  .release_state()
  h <- create_test_handle(n_persons = 120)
  on.exit(cleanup_handle(h), add = TRUE)
  .setLegacyTestPersonKey(h, "review-interaction", .local_envir = environment())
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  .release_dated_cohort(h, "review_t", 1:60, "2020-06-01", "2023-12-31")
  .release_dated_cohort(h, "review_c", 61:120, "2020-06-01", "2023-12-31")
  .release_add_condition(h, c(1:20, 61:80), 4000002L, "2021-03-01")
  selected <- c(1:10, 31:40, 61:70, 91:100)
  .release_add_condition(h, selected, 317009L, "2020-01-01")
  .release_add_condition(h, selected, 201820L, "2020-01-01")
  .buildBlueprint(h, force = TRUE)
  params <- list(outcome_concept_id = "4000002", outcome_domain_code = "0",
                 model_type = "poisson", subgroup_kind = "concept",
                 subgroup_domain_code = "0")
  scope <- list("review_t", "review_c")
  a <- .release_run(h, "dsomop:cm.interaction_estimate",
                    c(params, list(subgroup_concept_id = "317009")), scope)
  b <- .release_run(h, "dsomop:cm.interaction_estimate",
                    c(params, list(subgroup_concept_id = "201820")), scope)
  expect_equal(nrow(a), 2L)
  # The review's probe: two subgroup definitions selecting the same people
  # now give the same status/time components (labels are presentation).
  oa <- attr(a, "dsomop_release")$owners
  ob <- attr(b, "dsomop_release")$owners
  expect_identical(oa[[1L]]$components, ob[[1L]]$components)
  expect_identical(oa[[2L]]$components, ob[[2L]]$components)
  expect_identical(a$target_outcomes, b$target_outcomes)
  expect_identical(a$target_person_days, b$target_person_days)
  # A window spelling with the same effective window replays exactly.
  wide <- .release_run(h, "dsomop:cm.interaction_estimate",
                       c(params, list(subgroup_concept_id = "317009",
                                      tar_end_offset = "100")), scope)
  wide2 <- .release_run(h, "dsomop:cm.interaction_estimate",
                        c(params, list(subgroup_concept_id = "317009",
                                       tar_end_offset = "110")), scope)
  expect_identical(attr(wide, "dsomop_release")$owners,
                   attr(wide2, "dsomop_release")$owners)
  # The MDRR arms of the same cohorts share the overall arms' components.
  mdrr <- .release_run(h, "dsomop:cm.mdrr", list(
    outcome_concept_id = "4000002", outcome_domain_code = "0"), scope)
  overall <- .release_run(h, "dsomop:cm.interaction_estimate",
                          c(params[setdiff(names(params), "subgroup_kind")],
                            list(subgroup_kind = "overall")), scope)
  if (nrow(overall)) {
    expect_identical(attr(mdrr, "dsomop_release")$owners[[1L]]$components,
                     attr(overall, "dsomop_release")$owners[[1L]]$components)
  }
})

.review_long_handle <- function() {
  h <- create_test_handle(n_persons = 200)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  lengths <- c(rep(400L, 197), 800L, 900L, 1000L)
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = 1:200, person_id = 1:200,
    observation_period_start_date = "2008-01-01",
    observation_period_end_date = as.character(as.Date("2008-01-01") +
                                                 lengths),
    period_type_concept_id = 44814724L), append = TRUE)
  h
}

test_that("B09: standard min_days never re-tests raw tail support", {
  # Standard mode with the DP service: the view always comes from the
  # protected longest-period histogram, so tail support changes no row.
  .release_state(mode = "standard")
  h <- .review_long_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  run <- function(d) .release_run(h, "dsomop:observation_period.long_period_count",
                                  list(min_days = as.character(d)))
  before <- run(730)
  expect_equal(nrow(before), 1L)
  expect_true(attr(before, "dsomop_protection")$protection %in%
                .DSOMOP_RELEASE_PROTECTED)
  DBI::dbExecute(h$conn, paste(
    "UPDATE observation_period SET observation_period_end_date = '2009-02-04'",
    "WHERE person_id = 200"))
  after <- run(730)
  expect_equal(nrow(after), 1L)
  # Every public threshold answers with one row.
  for (d in .dsomopReleaseDurationGrid()) expect_equal(nrow(run(d)), 1L)
  # Without the DP service: a stable unavailable row after parent admission.
  withr::local_options(list(dsomop.dp.enabled = FALSE))
  .release_restart()
  for (d in c(730, 3650, 10957)) {
    off <- run(d)
    expect_equal(nrow(off), 1L)
    expect_true(is.na(off$n_persons))
    expect_identical(attr(off, "dsomop_protection")$protection, "unavailable")
  }
})

test_that("B10: events beyond the protected horizon never count", {
  .release_state()
  h <- create_test_handle(n_persons = 200)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = 1:200, person_id = 1:200,
    observation_period_start_date = "1999-01-01",
    observation_period_end_date = "2025-12-31",
    period_type_concept_id = 44814724L), append = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  .release_dated_cohort(h, "review_ht", 1:60, "2000-01-01", "2020-01-01")
  .release_dated_cohort(h, "review_hc", 101:160, "2000-01-01", "2020-01-01")
  .release_add_condition(h, c(1:20, 101:120), 4000002L,
                         format(as.Date("2000-01-01") + 4000))
  # One event just inside the horizon (day 3649) for persons 21..25.
  .release_add_condition(h, 21:25, 4000002L,
                         format(as.Date("2000-01-01") + 3649))
  capture <- NULL
  real_build <- .dsomopReleaseBuildC6
  testthat::local_mocked_bindings(.dsomopReleaseBuildC6 = function(
      policy, protection, owner, sub) {
    capture <<- sub
    real_build(policy, protection, owner, sub)
  })
  pars <- list(outcome_concept_id = "4000002", tar_start_offset = "0",
               tar_end_offset = "0", tar_anchor_start = "start",
               tar_anchor_end = "end")
  .release_run(h, "dsomop:incidence.rate", pars, scope = "review_ht")
  expect_lte(max(capture$time), 3650)
  # Only the 5 persons with an event inside [0, 3650) have one.
  expect_equal(sum(capture$event), 5)
  .release_run(h, "dsomop:cm.effect_estimate",
               c(pars, list(model_type = "poisson")),
               scope = list("review_ht", "review_hc"))
  expect_lte(max(capture$time), 3650)
  expect_equal(sum(capture$event), 5)
  expect_true(all(capture$time[capture$event == 1] <= 3650))
})

test_that("N01: the age-decade view reports its basis and view years", {
  .release_state()
  h <- create_test_handle(n_persons = 100)
  on.exit(cleanup_handle(h), add = TRUE)
  out <- .release_run(h, "dsomop:person.age_distribution_decade",
                      list(reference_year = "1995"))
  label <- attr(out, "dsomop_protection")
  expect_equal(label$view_reference_year, 1995)
  expect_equal(label$basis_reference_year, 2026)
  expect_null(label$effective_parameters$reference_year)
  withr::local_options(list(dsomop.tools.mode = "standard"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  std <- .release_run(h, "dsomop:person.age_distribution_decade",
                      list(reference_year = "1995"))
  expect_identical(attr(std, "dsomop_protection")$effective_parameters$
                     reference_year$effective, "2026")
})

test_that("B12/B14: releases carry a logical source and only fixed vectors claim delta", {
  .release_state()
  h <- create_test_handle(n_persons = 120)
  on.exit(cleanup_handle(h), add = TRUE)
  fixed <- .release_run(h, "dsomop:achilles.103")
  cohort <- .release_cohort(h, "review_src", 1:80)
  realised <- .release_run(h, "dsomop:achilles.103", scope = cohort)
  lf <- attr(fixed, "dsomop_protection")
  lr <- attr(realised, "dsomop_protection")
  expect_identical(lf$protection, "fixed_synopsis")
  expect_identical(lf$delta, 0)
  expect_true(is.finite(lf$mechanism_epsilon) && lf$mechanism_epsilon > 0)
  expect_identical(lr$protection, "realised_sticky")
  expect_false("delta" %in% names(lr))
  expect_true(is.na(lr$mechanism_epsilon))
  for (x in list(fixed, realised)) {
    id <- attr(x, "dsomop_release")$source$logical_source_id
    expect_match(id, "^dsl_[0-9a-f]{40}$")
    expect_identical(attr(x, "dsomop_protection")$source$logical_source_id, id)
  }
  expect_identical(attr(fixed, "dsomop_release")$source,
                   attr(realised, "dsomop_release")$source)
  # Mixed frames claim no frame-level epsilon or delta.
  table1 <- .release_run(h, "dsomop:fe.table1", scope = cohort)
  lm <- attr(table1, "dsomop_protection")
  if (identical(lm$protection, "mixed")) {
    expect_false("delta" %in% names(lm))
    expect_true(is.na(lm$mechanism_epsilon))
  }
})

test_that("B04: framed identities are injective for delimiters and control characters", {
  cases <- list(
    list("a", "b"), list("a\u001fb"), list(c("a", "b")), list("a|b"),
    list("a", "|b"), list("a:b"), list("a", ":b"), list("a\nb"),
    list("a\n", "b"), list(NA_character_), list("NA"), list(NULL),
    list(character(0)), list(1), list("1"), list(TRUE), list("t;"),
    list(list("a", "b")), list(list(a = "b")), list(c("a", NA)),
    list("s1:a"), list(c("1:a", "b")), list("\u001e\u001f"), list(""),
    list(c("", "")), list(0), list(-0.0 + 1e-300))
  encoded <- vapply(cases, function(x) do.call(.dsomopFrame, x),
                    character(1L))
  expect_false(anyDuplicated(encoded) > 0L)
  # Outcome identities are canonical sets, not request spellings.
  expect_identical(.omopReleaseOutcomeKey("201820,4000002", "0"),
                   .omopReleaseOutcomeKey(" 4000002, 201820,201820", "0"))
  expect_false(identical(.omopReleaseOutcomeKey("201820", "0"),
                         .omopReleaseOutcomeKey("201820", "1")))
})
