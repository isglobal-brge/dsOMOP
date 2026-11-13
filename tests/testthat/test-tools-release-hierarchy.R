# The public marginal hierarchy of crossed strata (dsOMOP 2.10.3, Astra
# re-review B03): every stratification of one population is post-processing
# of one canonical set of components, so a crossed request never creates
# another estimate of a margin that a coarser request releases, signed or
# displayed; incidence also restores the capped recurrent outcome count.
# Helpers in helper-tools-release.R.

.h_comp <- function(x, i = 1L) {
  comps <- attr(x, "dsomop_release")$owners[[i]]$components
  stats::setNames(comps, vapply(comps, `[[`, character(1L), "stratum"))
}

.h_signed <- function(x, field = "n", i = 1L, k = 1L) {
  vapply(.h_comp(x, i), function(c) as.numeric(c[[field]])[[k]], numeric(1L))
}

# Sum a child view over the cells whose key (or label) has a parent prefix,
# suffix or interior part.
.h_sum_by <- function(values, keys, parent_keys, part) {
  vapply(parent_keys, function(p) {
    sum(values[vapply(strsplit(keys, "|", fixed = TRUE), function(k) {
      identical(k[part], strsplit(p, "|", fixed = TRUE)[[1L]])
    }, logical(1L))])
  }, numeric(1L))
}

.h_incidence_handle <- function(n = 1200L) {
  h <- create_test_handle(n_persons = n)
  .release_dated_cohort(h, "hier_inc", seq_len(round(n * 0.75)),
                        start = "2021-01-01", end = "2023-12-31")
  # Recurrent outcome events: up to 14 per person inside the window.
  for (p in seq_len(60L)) {
    .release_add_condition(h, rep(p, 1L + p %% 14L), 4000002L,
                           format(as.Date("2021-02-01") + p))
  }
  h
}

.h_run <- function(h, ...) {
  .release_run(h, "dsomop:incidence.rate", c(list(
    outcome_concept_id = "4000002", tar_end_offset = "365"), list(...)),
    scope = "hier_inc")
}

# --- Projections ---------------------------------------------------------------

test_that("the bounded simplex is the exact constrained projection", {
  y <- c(3.2, -1, 0.5, 7, 2)
  x <- .dsomopReleaseBoundedSimplex(y, 6)
  expect_equal(sum(x), 6, tolerance = 1e-12)
  expect_equal(x, .dsomopReleaseSimplex(y, 6), tolerance = 1e-12)
  lo <- c(0, 0, 1, 0, 0.5)
  up <- c(2, 1, 3, 2.5, 4)
  x <- .dsomopReleaseBoundedSimplex(y, 7, lo, up)
  expect_equal(sum(x), 7, tolerance = 1e-12)
  expect_true(all(x >= lo - 1e-12 & x <= up + 1e-12))
  # KKT: free entries share one shift.
  free <- x > lo + 1e-9 & x < up - 1e-9
  expect_lte(diff(range((y - x)[free])), 1e-9)
  # Infeasible totals clamp into [sum(lower), sum(upper)].
  expect_equal(sum(.dsomopReleaseBoundedSimplex(y, 100, lo, up)), sum(up))
  expect_equal(sum(.dsomopReleaseBoundedSimplex(y, -5, lo, up)), sum(lo))
})

test_that("the transportation projection meets both displayed margins", {
  set.seed(7)
  Y <- matrix(stats::rnorm(24 * 59, 2, 40), 24, 59)
  rows <- .dsomopReleaseSimplex(stats::rnorm(24, 30, 50), 400)
  cols <- .dsomopReleaseSimplex(stats::rnorm(59, 7, 30), 400)
  X <- .dsomopReleaseTransport(Y, rows, cols)
  expect_true(all(X >= 0))
  expect_equal(rowSums(X), rows, tolerance = 1e-8)
  expect_equal(colSums(X), cols, tolerance = 1e-8)
  # A feasible target is its own projection.
  F <- outer(rows, cols) / 400
  expect_equal(.dsomopReleaseTransport(F, rows, cols), F, tolerance = 1e-8)
  # Row and column effects of the target do not change the projection.
  expect_equal(.dsomopReleaseTransport(Y + outer(seq_len(24), rep(1, 59)) +
                                         outer(rep(1, 24), seq_len(59)),
                                       rows, cols), X, tolerance = 1e-6)
})

test_that("the two-way adjustment meets consistent signed margins", {
  X <- matrix(c(1, -2, 4, 0.5, 3, -1), 2, 3)
  r <- c(5, -1)
  cc <- c(2, 1, 1)
  Z <- .dsomopReleaseReconcileTwoWay(X, r, cc)
  expect_equal(rowSums(Z), r, tolerance = 1e-12)
  expect_equal(colSums(Z), cc, tolerance = 1e-12)
})

# --- Incidence ---------------------------------------------------------------------

test_that("Astra B03: adding a public age dimension preserves gender margins", {
  # Port of astra-scratch/rereview-readers test-astra-rereview-readers.R.
  .release_state()
  h <- create_test_handle(n_persons = 2000)
  on.exit(cleanup_handle(h), add = TRUE)
  .release_dated_cohort(h, "astra_inc", 1:1500, start = "2021-01-01",
                        end = "2023-12-31")
  p <- list(outcome_concept_id = "4000002", tar_end_offset = "365",
            strat_by_gender = "1")
  g <- .release_run(h, "dsomop:incidence.rate", p, scope = "astra_inc")
  ga <- .release_run(h, "dsomop:incidence.rate",
                     c(p, list(strat_by_age = "1")), scope = "astra_inc")
  og <- attr(g, "dsomop_release")$owners[[1]]
  oga <- attr(ga, "dsomop_release")$owners[[1]]
  getn <- function(o) stats::setNames(
    vapply(o$components, `[[`, numeric(1), "n"),
    vapply(o$components, `[[`, character(1), "stratum"))
  ng <- getn(og)
  nga <- getn(oga)
  recombined <- vapply(names(ng), function(s) {
    sum(nga[startsWith(names(nga), paste0(s, "|"))])
  }, numeric(1))
  expect_equal(recombined, ng, tolerance = 1e-9)
  expect_equal(vapply(g$stratum, function(s) {
    sum(ga$persons_at_risk[startsWith(ga$stratum, paste0(s, "|"))])
  }, numeric(1)), stats::setNames(g$persons_at_risk, g$stratum),
  tolerance = 1e-6)
})

test_that("every incidence view is post-processing of one tree, signed and displayed", {
  .release_state()
  h <- .h_incidence_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  all <- .h_run(h)
  g <- .h_run(h, strat_by_gender = "1")
  a10 <- .h_run(h, strat_by_age = "1", age_band_years = "10")
  a20 <- .h_run(h, strat_by_age = "1", age_band_years = "20")
  a5 <- .h_run(h, strat_by_age = "1", age_band_years = "5")
  ga <- .h_run(h, strat_by_gender = "1", strat_by_age = "1")
  y <- .h_run(h, strat_by_year = "1")
  gy <- .h_run(h, strat_by_gender = "1", strat_by_year = "1")
  ay <- .h_run(h, strat_by_age = "1", strat_by_year = "1")
  gay <- .h_run(h, strat_by_gender = "1", strat_by_age = "1",
                strat_by_year = "1")
  fields <- c("persons_at_risk", "person_outcomes", "person_days", "outcomes")
  # Root = every view's total (displayed).
  for (v in list(g, a10, a20, a5, ga, y, gy, ay, gay)) {
    for (f in fields) {
      expect_equal(sum(v[[f]]), all[[f]], tolerance = 1e-6, info = f)
    }
  }
  check <- function(parent, child, part) {
    for (f in fields) {
      expect_equal(.h_sum_by(child[[f]], child$stratum, parent$stratum, part),
                   stats::setNames(parent[[f]], parent$stratum),
                   tolerance = 1e-6, info = f)
    }
    # Signed components: persons, status pair, time, recurrent.
    ck <- names(.h_comp(child))
    pk <- names(.h_comp(parent))
    for (field in c("n", "time", "recurrent")) {
      expect_equal(.h_sum_by(.h_signed(child, field), ck, pk, part),
                   stats::setNames(.h_signed(parent, field), pk),
                   tolerance = 1e-6, info = field)
    }
    expect_equal(.h_sum_by(.h_signed(child, "status", k = 1L), ck, pk, part),
                 stats::setNames(.h_signed(parent, "status", k = 1L), pk),
                 tolerance = 1e-6, info = "events")
  }
  check(g, ga, 1L)          # gender vs gender x age
  check(g, gy, 1L)          # gender vs gender x year
  check(a10, ay, 1L)        # age vs age x year
  check(y, ay, 2L)          # year vs age x year
  check(y, gy, 2L)          # year vs gender x year
  check(ga, gay, 1:2)       # gender x age vs gender x age x year
  check(gy, gay, c(1L, 3L)) # gender x year vs gender x age x year
  # Coarsened age bands are sums of the 5-year atoms (displayed and signed).
  coarsen <- function(fine, coarse, band) {
    lo <- suppressWarnings(as.integer(fine$stratum))
    key <- ifelse(is.na(lo), "unknown",
                  as.character(pmin((lo %/% band) * band,
                                    max(seq(0L, 110L, by = band)))))
    for (f in fields) {
      expect_equal(vapply(coarse$stratum, function(s) sum(fine[[f]][key == s]),
                          numeric(1L)),
                   stats::setNames(coarse[[f]], coarse$stratum),
                   tolerance = 1e-6, info = paste(band, f))
    }
  }
  coarsen(a5, a10, 10L)
  coarsen(a5, a20, 20L)
  coarsen(a10, a20, 20L)
  # Displayed coherence at the tree levels.
  for (v in list(g, a10, ga, gy)) {
    expect_true(all(v$person_outcomes <= v$persons_at_risk + 1e-9))
    expect_true(all(v$person_days <= v$persons_at_risk * 3650 + 1e-6))
    expect_true(all(v$outcomes >= v$person_outcomes - 1e-9))
    expect_true(all(v$outcomes <= 10 * v$person_outcomes + 1e-9))
  }
  for (v in list(all, g, a10, a20, a5, ga, y, gy, ay, gay)) {
    expect_true(all(unlist(v[fields]) >= 0))
  }
})

test_that("the gender view's parents never move when children are requested", {
  .release_state()
  h <- .h_incidence_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  g1 <- .h_run(h, strat_by_gender = "1")
  invisible(.h_run(h, strat_by_gender = "1", strat_by_age = "1"))
  invisible(.h_run(h, strat_by_gender = "1", strat_by_year = "1",
                   strat_by_age = "1"))
  g2 <- .h_run(h, strat_by_gender = "1")
  expect_identical(.release_values(g2), .release_values(g1))
  expect_identical(.h_comp(g2), .h_comp(g1))
})

test_that("incidence outcomes are the capped recurrent events (first 10 per person)", {
  .release_state()
  h <- .h_incidence_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  # Exact capped count of the window: min(events, 10) per person.
  exact <- vapply(seq_len(60L), function(p) min(1L + p %% 14L, 10L),
                  numeric(1L))
  # Bootstrap the exact sampler before replacing its draws for sufficiency.
  expect_true(suppressMessages(.dsomopToolsDpReady(h))$ready)
  zero <- function(...) .dsomopBigInteger(0L)
  testthat::local_mocked_bindings(.dsomopDpDiscreteLaplace = zero)
  all <- .h_run(h)
  expect_equal(all$outcomes, sum(exact))
  expect_equal(all$person_outcomes, 60)
  g <- .h_run(h, strat_by_gender = "1")
  genders <- DBI::dbGetQuery(h$conn, "SELECT person_id, gender_concept_id
                                      FROM person WHERE person_id <= 60")
  by_g <- tapply(exact[genders$person_id], genders$gender_concept_id, sum)
  male <- g$outcomes[g$stratum == "MALE"]
  female <- g$outcomes[g$stratum == "FEMALE"]
  expect_equal(c(male, female), unname(c(by_g[["8507"]], by_g[["8532"]])))
  label <- attr(all, "dsomop_protection")
  comp <- label$components[[1L]]
  expect_identical(comp$recurrent_estimand,
                   "capped recurrent outcome events (first 10 per person)")
  expect_identical(comp$recurrent_cap, 10L)
  expect_identical(comp$hierarchy, "c6-tree-gender-age5-year-v1")
  expect_true(all(c("N", "status:root", "time:root", "recurrent:root") %in%
                    names(comp$allocation_fraction)))
  expect_equal(comp$sensitivity[["recurrent:root"]], 10)
  expect_match(label$components[[1L]]$estimand, "first 10 per person")
})

test_that("a stratified request reports the union of its drawn levels", {
  .release_state()
  h <- .h_incidence_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  gay <- .h_run(h, strat_by_gender = "1", strat_by_age = "1",
                strat_by_year = "1")
  comp <- attr(gay, "dsomop_protection")$components[[1L]]
  expect_setequal(comp$levels, c("root", "gender", "gender|age5",
                                 "gender|year", "gender|age5|year"))
  alloc <- comp$allocation_fraction
  expect_true(all(c("N_strata:gender", "N_strata:gender|age5",
                    "N_strata:gender|year", "status:gender|age5|year",
                    "recurrent:gender|age5|year") %in% names(alloc)))
  # The crossed level owns no person total (event plus no event).
  expect_false("N_strata:gender|age5|year" %in% names(alloc))
  # Realised route: no joint epsilon is claimed.
  expect_true(is.na(attr(gay, "dsomop_protection")$mechanism_epsilon))
})

test_that("equal contribution maps replay through any cohort name", {
  .release_state()
  h <- .h_incidence_handle()
  on.exit(cleanup_handle(h), add = TRUE)
  .release_dated_cohort(h, "hier_inc_twin", seq_len(900L),
                        start = "2021-01-01", end = "2023-12-31")
  a <- .h_run(h, strat_by_gender = "1", strat_by_age = "1")
  b <- .release_run(h, "dsomop:incidence.rate", list(
    outcome_concept_id = "4000002", tar_end_offset = "365",
    strat_by_gender = "1", strat_by_age = "1"), scope = "hier_inc_twin")
  expect_identical(.release_values(a), .release_values(b))
})

# --- Achilles first observation period ---------------------------------------------

.h_first_obs_checks <- function(h, scope = NULL, route) {
  run <- function(id) .release_run(h, id, scope = scope)
  a101 <- run("dsomop:achilles.101")
  a102 <- run("dsomop:achilles.102")
  expect_identical(attr(a102, "dsomop_protection")$protection, route)
  # Bands by gender add up to the root bands (displayed) ...
  expect_equal(vapply(a101$age_band, function(b) {
    sum(a102$count_value[a102$age_band == b])
  }, numeric(1L)), a101$count_value, tolerance = 1e-6)
  # ... and to the gender totals of 104.
  a104 <- run("dsomop:achilles.104")
  expect_equal(vapply(a104$gender_name, function(g) {
    sum(a102$count_value[a102$gender_name == g])
  }, numeric(1L)), stats::setNames(a104$count_value, a104$gender_name),
  tolerance = 1e-6)
  # Signed cells and moment sums reconcile to the root owner's.
  c101 <- .h_comp(a101)[[1L]]
  c102 <- .h_comp(a102)
  expect_equal(Reduce(`+`, lapply(c102, `[[`, "h")), c101$h,
               tolerance = 1e-8)
  a103 <- run("dsomop:achilles.103")
  expect_equal(sum(vapply(.h_comp(a104), `[[`, numeric(1L), "s")),
               .h_comp(a103)[[1L]]$s, tolerance = 1e-6)
  # The root owner is part of the stratified release, drawn as a root
  # request draws it.
  expect_identical(attr(a102, "dsomop_release")$owners[[2L]],
                   attr(a101, "dsomop_release")$owners[[1L]])
  a105 <- run("dsomop:achilles.105")
  a107 <- run("dsomop:achilles.107")
  expect_equal(sum(a107$count_value), a105$count_value, tolerance = 1e-6)
  expect_equal(Reduce(`+`, lapply(.h_comp(a107), `[[`, "h")),
               .h_comp(a105)[[1L]]$h, tolerance = 1e-8)
}

test_that("first-observation strata reconcile to the root release (fixed route)", {
  .release_state()
  h <- create_test_handle(n_persons = 600)
  on.exit(cleanup_handle(h), add = TRUE)
  .h_first_obs_checks(h, NULL, "fixed_synopsis")
})

test_that("first-observation strata reconcile to the root release (realised route)", {
  .release_state()
  h <- create_test_handle(n_persons = 600)
  on.exit(cleanup_handle(h), add = TRUE)
  cohort <- .release_cohort(h, "hier_fop", 1:400)
  .h_first_obs_checks(h, cohort, "realised_sticky")
})

# --- CohortMethod interaction families ---------------------------------------------

.h_interaction <- function(h, ...) {
  .release_run(h, "dsomop:cm.interaction_estimate", c(list(
    outcome_concept_id = "4000002", tar_start_offset = "0",
    tar_end_offset = "365", model_type = "poisson"), list(...)),
    scope = list("hier_it_t", "hier_it_c"))
}

test_that("interaction families are partitions of the arms' own components", {
  .release_state()
  h <- create_test_handle(n_persons = 1400)
  on.exit(cleanup_handle(h), add = TRUE)
  .release_dated_cohort(h, "hier_it_t", 1:600, "2020-01-01", "2023-12-31")
  .release_dated_cohort(h, "hier_it_c", 601:1200, "2020-01-01", "2023-12-31")
  .release_add_condition(h, c(1:80, 601:640), 4000002L, "2020-06-01")
  ov <- .h_interaction(h, subgroup_kind = "overall")
  sx <- .h_interaction(h, subgroup_kind = "sex")
  coarse <- .h_interaction(h, subgroup_kind = "age", age_breaks = "40,60")
  fine <- .h_interaction(h, subgroup_kind = "age", age_breaks = "40,50,60")
  cols <- c("target_persons", "comparator_persons", "target_outcomes",
            "comparator_outcomes", "target_person_days",
            "comparator_person_days")
  for (fam in list(sx, coarse, fine)) {
    for (f in cols) {
      expect_equal(sum(fam[[f]]), ov[[f]], tolerance = 1e-6, info = f)
    }
  }
  # Different public breaks are views of the same 5-year atoms.
  for (f in cols) {
    expect_equal(coarse[[f]][coarse$subgroup_label == "age: 40-59"],
                 sum(fine[[f]][fine$subgroup_label %in%
                                 c("age: 40-49", "age: 50-59")]),
                 tolerance = 1e-6, info = f)
    for (lv in c("age: <40", "age: >=60", "age: unknown")) {
      expect_equal(coarse[[f]][coarse$subgroup_label == lv],
                   fine[[f]][fine$subgroup_label == lv], tolerance = 1e-9,
                   info = paste(lv, f))
    }
  }
  # Signed components: each family's nodes add up to the arms' components,
  # which are the overall subgroup's (and the effect estimate's) components.
  arm <- attr(ov, "dsomop_release")$owners[[1L]]$components
  owners <- attr(sx, "dsomop_release")$owners
  expect_identical(owners[[length(owners)]]$components, arm)
  for (a in 1:2) {
    expect_equal(sum(vapply(owners[1:3], function(o) o$components[[a]]$n,
                            numeric(1L))), arm[[a]]$n, tolerance = 1e-9)
    expect_equal(Reduce(`+`, lapply(owners[1:3], function(o) {
      o$components[[a]]$status
    })), arm[[a]]$status, tolerance = 1e-9)
  }
  # The overall row is the arms themselves.
  eff <- .release_run(h, "dsomop:cm.effect_estimate", list(
    outcome_concept_id = "4000002", tar_start_offset = "0",
    tar_end_offset = "365", model_type = "poisson"),
    scope = list("hier_it_t", "hier_it_c"))
  expect_equal(ov$target_persons, eff$persons[eff$arm == "target"])
})
