# Protected releases of the OHDSI community tools (dsOMOP 2.10.3): canonical
# owners, public quantization, fixed-basis and realised-contribution keys, and
# the absence of any history-dependent admission.

.release_env_names <- c(
  "DSOMOP_DP_ENABLED", "DSOMOP_DP_DOMAIN", "DSOMOP_DP_SNAPSHOT_ID",
  "DSOMOP_DP_RELEASE_EPSILON", "DSOMOP_DP_PRIVACY_EPOCH",
  "DSOMOP_DP_MAX_LEVELS", "DSOMOP_DP_MAX_CONTRIBUTIONS",
  "DSOMOP_DP_NUMERIC_GRID", "DSOMOP_DP_NOISE_ROOT"
)
# A fixed test noise root: protected answers are reproducible test fixtures.
.release_test_root <- paste(rep("5a", 32L), collapse = "")

.release_state <- function(snapshot_id = "release-2026-11-12",
                           enabled = TRUE, mode = "auto",
                           .local_envir = parent.frame()) {
  state <- withr::local_tempdir(pattern = "dsomop-release-",
                                .local_envir = .local_envir)
  Sys.chmod(state, mode = "0700")
  state <- normalizePath(state, winslash = "/", mustWork = TRUE)
  environment <- stats::setNames(
    rep.int(NA_character_, length(.release_env_names)), .release_env_names)
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state, DSOMOP_MARKER_DIR = NA_character_,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1", environment
  ), .local_envir = .local_envir)
  withr::local_options(list(
    dsomop.state_dir = state, dsomop.dp.enabled = enabled,
    dsomop.dp.domain = "dsomop-release-test",
    dsomop.dp.snapshot_id = snapshot_id,
    dsomop.dp.release_epsilon = 0.1, dsomop.dp.privacy_epoch = 1L,
    dsomop.dp.noise_root = .release_test_root,
    dsomop.dp.reference_year = 2026L,
    dsomop.tools.mode = mode,
    nfilter.subset = 3, nfilter.tab = 3, dsomop.nfilter.dist = 10,
    dsomop.nfilter.band = 5
  ), .local_envir = .local_envir)
  .release_restart()
  withr::defer(.release_restart(), envir = .local_envir)
  state
}

.release_restart <- function() {
  .pkg_state$dp_status <- NULL
  .pkg_state$dp_runtime <- NULL
  .pkg_state$dp_bootstrap_in_progress <- FALSE
  invisible(NULL)
}

# A handle whose resource has persistent coordinates, so a DP identity can be
# derived without a custodial snapshot (the realised-contribution route for
# unscoped questions).
.release_derived_handle <- function(n_persons = 60) {
  h <- create_test_handle(n_persons = n_persons)
  h$resource_client <- list(getParsed = function() list(
    dbms = "postgresql", host = "db.example", port = 5432L,
    database = "release", user = "ignored", password = "ignored"
  ), getConnection = function() h$conn)
  DBI::dbExecute(h$conn, "ALTER TABLE cdm_source ADD COLUMN cdm_release_date TEXT")
  DBI::dbExecute(h$conn, "UPDATE cdm_source SET cdm_release_date = '2026-08-01'")
  h
}

.release_run <- function(h, id, params = list(), scope = NULL) {
  suppressWarnings(.omopAnalysisRun(h, id, params, scope = scope))
}

# A temporary cohort holding exactly the given persons.
.release_cohort <- function(h, name, person_ids) {
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE ", name, " (cohort_definition_id INTEGER, ",
    "subject_id INTEGER, cohort_start_date TEXT, cohort_end_date TEXT)"))
  DBI::dbWriteTable(h$conn, name, data.frame(
    cohort_definition_id = 1L, subject_id = as.integer(person_ids),
    cohort_start_date = "2015-01-01", cohort_end_date = "2016-01-01"),
    append = TRUE)
  register_test_temp(h, name)
  name
}

test_that("public quantizers take the nearest grid point, ties to the smaller", {
  grid <- c(0, 30, 60)
  expect_identical(.dsomopReleaseSnap(15, grid), 0)
  expect_identical(.dsomopReleaseSnap(15.0001, grid), 30)
  expect_identical(.dsomopReleaseSnap(44, grid), 30)
  expect_identical(.dsomopReleaseSnap(45, grid), 30)
  expect_identical(.dsomopReleaseSnap(-1e12, grid), 0)
  expect_identical(.dsomopReleaseSnap(1e12, grid), 60)
  expect_identical(.dsomopReleaseSnapFloor(31, grid), 30)
  expect_identical(.dsomopReleaseSnapCeil(31, grid), 60)
  expect_error(.dsomopReleaseSnap(NA_real_, grid), "finite")
  expect_error(.dsomopReleaseSnap(Inf, grid), "finite")
  signed <- .dsomopReleaseSignedDayGrid()
  expect_identical(range(signed), c(-3650, 3650))
  expect_true(all(signed[-c(1L, length(signed))] %% 30 == 0))
  expect_false(anyDuplicated(signed) > 0L)
})

test_that("windows snap outward and scalars to the nearest public point", {
  entry <- list(name = "x", params = list(
    list(name = "tar_start_offset", type = "int"),
    list(name = "tar_end_offset", type = "int"),
    list(name = "temporal_start_days", type = "int"),
    list(name = "temporal_end_days", type = "int"),
    list(name = "min_days", type = "int"),
    list(name = "min_age", type = "int"),
    list(name = "max_age", type = "int"),
    list(name = "age_band_years", type = "int"),
    list(name = "alpha", type = "number"),
    list(name = "age_breaks", type = "string"),
    list(name = "min_prior_observation", type = "int"),
    list(name = "reference_year", type = "int"),
    list(name = "top_n", type = "int"),
    list(name = "concept_id", type = "concept_id")))
  withr::local_options(list(dsomop.dp.reference_year = 2026L))
  snap <- .omopToolsSnapReleaseParams(entry, list(
    tar_start_offset = "1", tar_end_offset = "0",
    temporal_start_days = "-365", temporal_end_days = "-1",
    min_days = "400", min_age = "39", max_age = "39",
    age_band_years = "8", alpha = "0.051", age_breaks = "39,40",
    min_prior_observation = "365", reference_year = "2002", top_n = "7",
    concept_id = "201820"))
  e <- snap$effective
  expect_identical(e$tar_start_offset, "0")
  expect_identical(e$tar_end_offset, "29")
  expect_identical(e$temporal_start_days, "-390")
  expect_identical(e$temporal_end_days, "-1")
  expect_identical(e$min_days, "365")
  expect_identical(e$min_age, "35")
  expect_identical(e$max_age, "39")
  expect_identical(e$age_band_years, "10")
  expect_identical(e$alpha, "0.05")
  expect_identical(e$age_breaks, "40")
  expect_identical(e$min_prior_observation, "360")
  expect_identical(e$reference_year, "2026")
  expect_identical(e$top_n, "7")
  expect_identical(e$concept_id, "201820")
  # Every change is reported with the requested and effective value.
  expect_identical(snap$changes$tar_end_offset$requested, "0")
  expect_identical(snap$changes$tar_end_offset$effective, "29")
  expect_null(snap$changes$top_n)
  # Saturation of huge, negative and boundary values.
  far <- .omopToolsSnapReleaseParams(entry, list(
    tar_end_offset = "10000", tar_start_offset = "-99999",
    min_days = "-5", max_age = "200"))$effective
  expect_identical(far$tar_end_offset, "3649")
  expect_identical(far$tar_start_offset, "-3650")
  expect_identical(far$min_days, "0")
  expect_identical(far$max_age, "109")
  # Requests inside one quantization cell share one effective estimand.
  inside <- vapply(c("95", "100", "107", "114", "119"), function(x) {
    .omopToolsSnapReleaseParams(entry, list(tar_end_offset = x))$effective$
      tar_end_offset
  }, character(1L))
  expect_identical(unname(inside), rep("119", 5L))
})

test_that("every numeric parameter of every built-in aggregate entry is classified", {
  h <- create_test_handle(n_persons = 20)
  on.exit(cleanup_handle(h), add = TRUE)
  registry <- suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  missing <- character(0)
  for (entry in registry) {
    if (identical(entry$mode, "assign")) next
    for (p in entry$params %||% list()) {
      if (is.null(.dsomopReleaseParamClass(p))) {
        missing <- c(missing, paste(entry$name, p$name))
      }
    }
  }
  expect_identical(missing, character(0))
})

test_that("the simplex projection is deterministic, non-negative and exact", {
  y <- c(5, -3, 2, 2, 0, -10)
  p <- .dsomopReleaseSimplex(y, 6)
  expect_true(all(p >= 0))
  expect_equal(sum(p), 6, tolerance = 1e-12)
  expect_identical(.dsomopReleaseSimplex(y, 6), p)
  expect_equal(.dsomopReleaseSimplex(c(1, 1, 1), 1), rep(1 / 3, 3))
  expect_identical(.dsomopReleaseSimplex(c(4, -2), 0), c(0, 0))
  expect_equal(.dsomopReleaseSimplex(c(1, 2, 3), 6), c(1, 2, 3))
  # Empty cells never gain unconstrained positive mass: the total is fixed.
  sparse <- .dsomopReleaseSimplex(c(100, rnorm(100, sd = 14)), 100)
  expect_equal(sum(sparse), 100, tolerance = 1e-9)
})

test_that("the moment projection is the identity on feasible points", {
  feasible <- function(p, N) {
    abs(p[["S"]]) <= N + 1e-9 && p[["Q"]] >= -1e-12 && p[["Q"]] <= N + 1e-9 &&
      p[["S"]]^2 <= N * p[["Q"]] + 1e-6 * N
  }
  expect_equal(.dsomopReleaseProjectMoments(2, 5, 10), c(S = 2, Q = 5))
  set.seed(20261112)
  for (i in seq_len(300L)) {
    N <- stats::runif(1, 1, 500)
    S0 <- stats::rnorm(1, 0, N)
    Q0 <- stats::rnorm(1, N / 2, N)
    p <- .dsomopReleaseProjectMoments(S0, Q0, N)
    expect_true(feasible(p, N))
    # Idempotent and no farther than any feasible corner.
    expect_equal(.dsomopReleaseProjectMoments(p[["S"]], p[["Q"]], N), p,
                 tolerance = 1e-6)
    d <- (p[["S"]] - S0)^2 + (p[["Q"]] - Q0)^2
    for (corner in list(c(N, N), c(-N, N), c(0, 0))) {
      expect_lte(d, (corner[[1L]] - S0)^2 + (corner[[2L]] - Q0)^2 + 1e-6)
    }
  }
  expect_equal(.dsomopReleaseProjectMoments(5, 5, 0), c(S = 0, Q = 0))
})

test_that("quantiles invert the displayed histogram monotonically", {
  grid <- .dsomopReleaseGrid("age_5y")
  counts <- c(0, 0, 10, 20, 30, 40, rep(0, length(grid$breaks) - 6L))
  q <- .dsomopReleaseQuantiles(counts, grid, c(0.1, 0.25, 0.5, 0.75, 0.9))
  expect_false(is.unsorted(q))
  expect_true(all(q >= 10 & q <= 30))
  expect_true(all(is.na(.dsomopReleaseQuantiles(rep(0, length(grid$breaks)),
                                                grid))))
  open <- c(rep(0, length(grid$breaks) - 1L), 5)
  expect_identical(.dsomopReleaseQuantiles(open, grid, 0.5), 110)
})

test_that("contribution fingerprints are order-free and change with contributions", {
  keys <- list(fingerprint = as.raw(1:32))
  tokens <- c("aa", "bb", "cc")
  base <- .dsomopReleaseFingerprint(keys, "o", "H", "c1", tokens, c(1, 1, 1))
  expect_identical(.dsomopReleaseFingerprint(keys, "o", "H", "c1",
                                             rev(tokens), c(1, 1, 1)), base)
  expect_false(identical(.dsomopReleaseFingerprint(
    keys, "o", "H", "c1", tokens, c(1, 2, 1)), base))
  expect_false(identical(.dsomopReleaseFingerprint(
    keys, "o", "H", "c1", tokens[-1L], c(1, 1)), base))
  # Empty components keep distinct coordinate identities.
  e1 <- .dsomopReleaseFingerprint(keys, "o", "H", "c1")
  e2 <- .dsomopReleaseFingerprint(keys, "o", "H", "c2")
  expect_false(identical(e1, e2))
  expect_false(identical(.dsomopReleaseFingerprint(keys, "o2", "H", "c1"), e1))
  expect_error(.dsomopReleaseFingerprint(keys, "o", "H", "c1", c("aa", "aa"),
                                         c(1, 1)), "twice")
  expect_error(.dsomopReleaseFingerprint(keys, "o", "H", "c1", "aa", 0.5),
               "exact integer")
})

test_that("unscoped public-basis questions are fixed synopses; scopes are realised", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  wide <- .release_run(h, "dsomop:achilles.103")
  label <- attr(wide, "dsomop_protection")
  expect_identical(label$protection, "fixed_synopsis")
  expect_identical(label$guarantee_scope,
                   "fixed_vector_before_exact_support_gate")
  expect_equal(label$mechanism_epsilon, 0.1)
  cohort <- .release_cohort(h, "release_scope_all", 1:40)
  scoped <- .release_run(h, "dsomop:achilles.103", scope = cohort)
  label <- attr(scoped, "dsomop_protection")
  expect_identical(label$protection, "realised_sticky")
  expect_identical(label$guarantee_scope,
                   "heuristic_realised_contribution_reuse")
  expect_true(is.na(label$mechanism_epsilon))
  expect_true(is.na(label$api_transcript_epsilon))
  expect_equal(label$noise_reference_epsilon, 0.1)
  # DP-only mode accepts only the fixed synopsis, as a static distinction.
  withr::local_options(list(dsomop.tools.mode = "dp"))
  registry <- suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  expect_identical(attr(.release_run(h, "dsomop:achilles.103"),
                        "dsomop_protection")$protection, "fixed_synopsis")
  expect_error(.release_run(h, "dsomop:achilles.103", scope = cohort),
               "no fixed-synopsis protection")
})

test_that("without a custodial generation unscoped questions are realised", {
  .release_state(snapshot_id = "")
  h <- .release_derived_handle(60)
  on.exit(cleanup_handle(h), add = TRUE)
  out <- .release_run(h, "dsomop:achilles.105")
  expect_identical(attr(out, "dsomop_protection")$protection,
                   "realised_sticky")
  withr::local_options(list(dsomop.tools.mode = "dp"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  expect_error(.release_run(h, "dsomop:achilles.105"),
               "no fixed-synopsis protection")
})

test_that("answers never depend on the number or order of earlier questions", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  cohort <- .release_cohort(h, "release_history", 1:45)
  first <- .release_run(h, "dsomop:achilles.104", scope = cohort)
  fixed_first <- .release_run(h, "dsomop:achilles.3")
  for (i in seq_len(25L)) {
    .release_run(h, "dsomop:achilles.105")
    .release_run(h, "dsomop:achilles.104")
    .release_run(h, "dsomop:observation_period.long_period_count",
                 list(min_days = as.character(i * 30)))
  }
  expect_identical(.release_run(h, "dsomop:achilles.104", scope = cohort),
                   first)
  # A worker restart (new runtime, same root) and a new handle replay too.
  .release_restart()
  h2 <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h2), add = TRUE)
  expect_identical(.release_run(h2, "dsomop:achilles.3"), fixed_first)
  cohort2 <- .release_cohort(h2, "release_history_copy", 1:45)
  expect_identical(.release_run(h2, "dsomop:achilles.104", scope = cohort2),
                   first)
  # No accountant, budget, quota or counter exists in configuration or status.
  status <- .dsomopDpPublicStatus(initialize = TRUE)
  expect_identical(status$privacy_call_quota, "none")
  expect_false(isTRUE(status$history_dependent))
  forbidden <- c("total_epsilon", "remaining_epsilon", "spent_epsilon",
                 "budget", "quota_remaining", "release_count")
  expect_false(any(forbidden %in% names(status)))
  expect_false(any(grepl("total_epsilon|budget", names(options()))))
})

test_that("Achilles 3 and 10 and the QueryLibrary birth-year questions share one basis", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  a3 <- .release_run(h, "dsomop:achilles.3")
  a10 <- .release_run(h, "dsomop:achilles.10")
  ql <- .release_run(h, "dsomop:person.year_of_birth_distribution")
  decade <- .release_run(h, "dsomop:person.gender_by_age_decade")
  expect_identical(attr(a3, "dsomop_protection")$owner,
                   attr(ql, "dsomop_protection")$owner)
  expect_identical(attr(a10, "dsomop_protection")$owner,
                   attr(decade, "dsomop_protection")$owner)
  expect_equal(a3$count_value, ql$n_persons)
  by_year <- tapply(a10$count_value,
                    ifelse(is.na(a10$year_of_birth), "unknown",
                           a10$year_of_birth), sum)
  expect_equal(as.numeric(by_year[ifelse(is.na(a3$year_of_birth), "unknown",
                                         as.character(a3$year_of_birth))]),
               a3$count_value, tolerance = 1e-9)
  # A requested reference year only relabels the released basis.
  r2000 <- .release_run(h, "dsomop:person.age_distribution_decade",
                        list(reference_year = 2000))
  r1990 <- .release_run(h, "dsomop:person.age_distribution_decade",
                        list(reference_year = 1990))
  expect_equal(sum(r2000$n_persons), sum(a3$count_value), tolerance = 1e-9)
  expect_equal(sum(r1990$n_persons), sum(a3$count_value), tolerance = 1e-9)
})

test_that("two scopes differing by one person no longer expose the exact mean algebra (E1)", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  a <- .release_cohort(h, "release_e1_a", 1:30)
  b <- .release_cohort(h, "release_e1_b", 1:31)
  ra <- .release_run(h, "dsomop:achilles.103", scope = a)
  rb <- .release_run(h, "dsomop:achilles.103", scope = b)
  for (r in list(ra, rb)) {
    label <- attr(r, "dsomop_protection")
    expect_identical(label$protection, "realised_sticky")
    expect_false(any(c("min_value", "max_value") %in% names(r)))
  }
  # The released moments are post-processing of noisy components only: the
  # displayed mean is reproduced from the exported signed components.
  comp <- attr(rb, "dsomop_release")$owners[[1L]]$components[[1L]]
  N <- rb$count_value
  if (N >= 10) {
    p <- .dsomopReleaseProjectMoments(comp$s / 65535, comp$q / 65535, N)
    expect_equal(rb$avg_value, 55 + 55 * p[["S"]] / N, tolerance = 1e-6)
  }
  # The exact difference equation of the attack does not hold.
  person <- DBI::dbGetQuery(h$conn, paste(
    "SELECT p.year_of_birth, MIN(o.observation_period_start_date) AS s",
    "FROM person p JOIN observation_period o ON o.person_id = p.person_id",
    "WHERE p.person_id = 31 GROUP BY p.year_of_birth"))
  if (nrow(person) == 1L && !anyNA(c(ra$avg_value, rb$avg_value))) {
    age <- as.integer(substr(person$s, 1, 4)) - person$year_of_birth
    recovered <- rb$count_value * rb$avg_value - ra$count_value * ra$avg_value
    expect_false(isTRUE(all.equal(recovered, age)))
  }
  # Standard mode: the exact scoped moments are unavailable.
  withr::local_options(list(dsomop.tools.mode = "standard"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  std <- .release_run(h, "dsomop:achilles.103", scope = b)
  expect_true(all(is.na(std[, .DSOMOP_RELEASE_DIST_FIELDS])))
  label <- attr(std, "dsomop_protection")
  expect_identical(label$masked_reason, "protected_output_required")
  expect_setequal(label$masked_fields, .DSOMOP_RELEASE_DIST_FIELDS)
  # Unscoped standard summaries keep their established controls.
  wide <- .release_run(h, "dsomop:achilles.103")
  expect_false(all(is.na(wide$avg_value)))
})

test_that("realised components replay for equal maps whatever the program", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  one <- .release_cohort(h, "release_map_one", 1:40)
  two <- .release_cohort(h, "release_map_two", rev(1:40))
  expect_identical(.release_run(h, "dsomop:achilles.10", scope = one),
                   .release_run(h, "dsomop:achilles.10", scope = two))
  # Adding one person changes only the components that person is in.
  wider <- .release_cohort(h, "release_map_wider", 1:41)
  base <- attr(.release_run(h, "dsomop:achilles.10", scope = one),
               "dsomop_release")$owners[[1L]]$components
  plus <- attr(.release_run(h, "dsomop:achilles.10", scope = wider),
               "dsomop_release")$owners[[1L]]$components
  person <- DBI::dbGetQuery(h$conn, paste(
    "SELECT gender_concept_id, year_of_birth FROM person WHERE person_id = 41"))
  stratum <- .omopReleaseGenderKey(person$gender_concept_id)
  for (i in seq_along(base)) {
    if (identical(base[[i]]$stratum, stratum)) {
      expect_false(identical(base[[i]]$n, plus[[i]]$n))
    } else {
      expect_identical(base[[i]]$n, plus[[i]]$n)
      expect_identical(base[[i]]$h, plus[[i]]$h)
    }
  }
})

test_that("each person changes a bounded number of exact components", {
  h <- create_test_handle(n_persons = 40)
  on.exit(cleanup_handle(h), add = TRUE)
  ctx <- list(scoped_cohort = NULL)
  owners <- list(
    list(owner = .omopReleaseBirthYearOwner(),
         sub = function() .omopReleaseBirthYearSubstrate(h, ctx), cap = 1),
    list(owner = .omopReleaseFirstObsOwner("age", "gender"),
         sub = function() .omopReleaseFirstObsSubstrate(h, ctx, "age",
                                                        "gender"), cap = 1),
    list(owner = .omopReleaseFirstObsOwner("length", "age_decile"),
         sub = function() .omopReleaseFirstObsSubstrate(h, ctx, "length",
                                                        "age_decile"), cap = 1))
  tables <- c("person", "observation_period", "condition_occurrence",
              "drug_exposure", "measurement", "visit_occurrence")
  tables <- intersect(tables, DBI::dbListTables(h$conn))
  for (spec in owners) {
    full <- .dsomopReleaseExactComponents(spec$owner, spec$sub())
    for (person in c(1L, 2L, 7L)) {
      backup <- lapply(tables, function(t) DBI::dbGetQuery(
        h$conn, paste0("SELECT * FROM ", t, " WHERE person_id = ", person)))
      for (t in tables) {
        DBI::dbExecute(h$conn, paste0("DELETE FROM ", t,
                                      " WHERE person_id = ", person))
      }
      without <- .dsomopReleaseExactComponents(spec$owner, spec$sub())
      for (i in seq_along(tables)) {
        DBI::dbWriteTable(h$conn, tables[[i]], backup[[i]], append = TRUE)
      }
      dn <- sum(abs(vapply(full, `[[`, numeric(1L), "n") -
                      vapply(without, `[[`, numeric(1L), "n")))
      dh <- sum(abs(unlist(lapply(full, `[[`, "h")) -
                      unlist(lapply(without, `[[`, "h"))))
      ds <- sum(abs(vapply(full, `[[`, numeric(1L), "s") -
                      vapply(without, `[[`, numeric(1L), "s")))
      dq <- sum(abs(vapply(full, `[[`, numeric(1L), "q") -
                      vapply(without, `[[`, numeric(1L), "q")))
      expect_lte(dn, spec$cap)
      expect_lte(dh, spec$cap)
      expect_lte(ds, spec$cap * 65535)
      expect_lte(dq, spec$cap * 65535)
    }
  }
})

test_that("multi-concept strata cap each person at K concepts in public order", {
  owner <- .dsomopReleaseOwner(id = "test/multi", family = "HM",
                               grid = .dsomopReleaseGrid("age_5y"),
                               strata = list(kind = "data"), cap = 2L,
                               bounds = c(0, 110))
  sub <- data.frame(person_id = c(1, 1, 1, 2), stratum = c("concept=30",
                                                            "concept=10",
                                                            "concept=20",
                                                            "concept=30"),
                    stratum_order = c(30, 10, 20, 30), v = c(50, 50, 50, 60),
                    stringsAsFactors = FALSE)
  capped <- .dsomopReleaseCapSubstrate(sub, owner)
  expect_identical(sort(capped$stratum[capped$person_id == "1"]),
                   c("concept=10", "concept=20"))
  expect_identical(capped$stratum[capped$person_id == "2"], "concept=30")
  partition <- .dsomopReleaseOwner(id = "test/partition", family = "H",
                                   cells = c("a", "b"))
  expect_error(.dsomopReleaseCapSubstrate(
    data.frame(person_id = c(1, 1), stratum = c("root", "root"),
               cell = c("a", "b")), partition), "twice")
})

test_that("protected labels and payloads carry no secret material", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  cohort <- .release_cohort(h, "release_secret", 1:40)
  out <- .release_run(h, "dsomop:achilles.406", scope = cohort)
  text <- paste(capture.output(str(attributes(out))), collapse = "\n")
  expect_false(grepl(.release_test_root, text, fixed = TRUE))
  expect_false(grepl("fingerprint|token|seed", text, ignore.case = TRUE))
  # Data-dependent concept strata still need their own support.
  expect_true(all(out$count_value >= 0))
})
