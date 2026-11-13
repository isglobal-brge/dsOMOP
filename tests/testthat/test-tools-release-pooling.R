# Live pooling of dsOMOP 2.10.3 protected releases by the sibling
# dsOMOPClient: real frames of two independent sites (own DSLite-style
# session, own corpus domain, own data) pooled by the client's own
# functions. Record-weighted summaries pool their signed N, R, S and Q;
# protected effects pool by inverse-variance meta-analysis of the per-site
# protected estimates; standard-mode mixed frames are labelled as sums of
# protected values. Skips when the working-tree client is not loadable.

.pool_client_ready <- function() {
  tp <- tryCatch(testthat::test_path(), error = function(e) ".")
  cand <- c(file.path(tp, "..", "..", "..", "dsOMOPClient"),
            file.path(tp, "..", "..", "dsOMOPClient"),
            "../dsOMOPClient", "../../dsOMOPClient")
  cp <- Find(dir.exists, cand)
  if (is.null(cp)) return(FALSE)
  tryCatch({
    suppressMessages(pkgload::load_all(cp, quiet = TRUE, export_all = FALSE))
    exists(".pool_protected_analysis", envir = asNamespace("dsOMOPClient"))
  }, error = function(e) FALSE)
}

# Run one site's code in its own DP session with its own corpus domain (so
# the sites are distinct logical sources with independent noise).
.pool_sites <- function(handles, code) {
  old <- .pkg_state$dp_sessions
  on.exit(.pkg_state$dp_sessions <- old, add = TRUE)
  .pkg_state$dp_sessions <- NULL
  out <- lapply(seq_along(handles), function(i) {
    env <- new.env()
    attr(env, "name") <- sprintf("DSLiteEnv_%04d", 9100L + i)
    env$h <- handles[[i]]
    env$code <- code
    withr::with_options(list(dsomop.dp.domain = sprintf("pool-site-%d", i)),
                        evalq(code(h), env))
  })
  stats::setNames(out, names(handles))
}

# Condition eras on a test CDM (two concepts, one to three eras per person).
.pool_records_handle <- function(n_persons) {
  h <- create_test_handle(n_persons = n_persons)
  DBI::dbExecute(h$conn, paste(
    "CREATE TABLE condition_era (condition_era_id INTEGER, person_id INTEGER,",
    "condition_concept_id INTEGER, condition_era_start_date TEXT,",
    "condition_era_end_date TEXT, condition_occurrence_count INTEGER)"))
  persons <- rep(seq_len(n_persons),
                 times = rep_len(c(1L, 2L, 3L), n_persons))
  DBI::dbWriteTable(h$conn, "condition_era", data.frame(
    condition_era_id = seq_along(persons), person_id = persons,
    condition_concept_id = rep_len(c(201820L, 255573L), length(persons)),
    condition_era_start_date = "2020-01-01",
    condition_era_end_date = as.character(
      as.Date("2020-01-01") + (seq_along(persons) * 37L) %% 900L),
    condition_occurrence_count = 1L), append = TRUE)
  h
}

.pool_survival_handle <- function(n, seed) {
  h <- create_test_handle(n_persons = n)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = seq_len(n), person_id = seq_len(n),
    observation_period_start_date = "2015-01-01",
    observation_period_end_date = "2024-12-31",
    period_type_concept_id = 44814724L), append = TRUE)
  DBI::dbExecute(h$conn, paste0("DELETE FROM condition_occurrence WHERE ",
                                "condition_concept_id = 4000002"))
  half <- n / 2
  .release_dated_cohort(h, "pool_t", seq_len(half), start = "2020-01-01",
                        end = "2023-12-31")
  .release_dated_cohort(h, "pool_c", half + seq_len(half),
                        start = "2020-01-01", end = "2023-12-31")
  set.seed(seed)
  for (p in seq_len(n)) {
    if (stats::runif(1) < if (p <= half) 0.5 else 0.25) {
      .release_add_condition(h, p, 4000002L, format(
        as.Date("2020-01-01") + sample.int(360L, 1L)))
    }
  }
  h
}

.pool_sccs_handle <- function(n, seed) {
  h <- create_test_handle(n_persons = n)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = seq_len(n), person_id = seq_len(n),
    observation_period_start_date = "2020-01-01",
    observation_period_end_date = "2021-12-31",
    period_type_concept_id = 44814724L), append = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  DBI::dbExecute(h$conn, "DELETE FROM drug_exposure")
  set.seed(seed)
  for (p in seq_len(n)) {
    ex <- as.Date("2020-03-01") + sample(0:300, 1)
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO drug_exposure (drug_exposure_id, person_id, ",
      "drug_concept_id, drug_exposure_start_date, drug_exposure_end_date, ",
      "drug_type_concept_id) VALUES (%d, %d, 1112807, '%s', '%s', ",
      "38000177)"), 5000L + p, p, format(ex), format(ex)))
    days <- sample(0:729, 1 + stats::rpois(1, 1))
    if (stats::runif(1) < 0.6) {
      days <- c(days, as.numeric(ex - as.Date("2020-01-01")) + sample(0:29, 1))
    }
    for (d in days) {
      .release_add_condition(h, p, 4000002L,
                             format(as.Date("2020-01-01") + d))
    }
  }
  h
}

# Inverse-variance fixed-effect pool of per-site log estimates.
.pool_iv <- function(le, se) {
  w <- 1 / se^2
  exp(sum(w * le) / sum(w))
}

test_that("record-weighted summaries of two live sites pool their components", {
  skip_if_not(.pool_client_ready(), "working-tree dsOMOPClient not loadable")
  cl <- asNamespace("dsOMOPClient")
  .release_state()
  handles <- list(a = .pool_records_handle(60), b = .pool_records_handle(80))
  on.exit(lapply(handles, cleanup_handle), add = TRUE)
  id <- "dsomop:condition_era.length_stats"
  frames <- .pool_sites(handles, function(h) .release_run(h, id))
  meta <- .omopAnalysisGet(handles$a, id)
  rp <- meta$release_pooling
  expect_identical(rp$strategy, "protected_components")
  sources <- vapply(frames, function(df) {
    attr(df, "dsomop_release")$source$logical_source_id
  }, character(1L))
  expect_false(identical(sources[[1L]], sources[[2L]]))
  pooled <- cl$.pool_protected_analysis(frames, rp, policy = "strict")
  out <- pooled$result
  expect_identical(names(out), names(rp$columns))
  expect_gt(nrow(out), 0L)
  owners <- lapply(frames, function(df) attr(df, "dsomop_release")$owners[[1L]])
  J <- 65535
  for (i in seq_len(nrow(out))) {
    key <- paste0("concept=", out$condition_concept_id[[i]])
    comps <- lapply(owners, function(o) {
      Filter(function(c) identical(c$stratum, key), o$components)[[1L]]
    })
    total <- function(f) sum(vapply(comps, function(c) c[[f]], numeric(1L)))
    expect_equal(out$n_persons[[i]], max(0, total("n")))
    N <- max(0, total("n"))
    R <- min(10 * N, max(N, total("r")))
    expect_equal(out$n_records[[i]], R)
    # The moments are the server's derivation from the pooled R, S and Q,
    # shown once pooled persons reach the public distribution floor (10)
    # and records reach 2.
    if (max(0, total("n")) < 10 || R < 2) {
      expect_true(is.na(out$avg_days[[i]]))
      expect_true(is.na(out$stddev_days[[i]]))
    } else {
      p <- .dsomopReleaseProjectMoments(2 * total("s") / J - R,
                                        4 * total("q") / J -
                                          4 * total("s") / J + R, R)
      expect_equal(out$avg_days[[i]], 10957 / 2 * (1 + p[["S"]] / R),
                   tolerance = 1e-9)
      expect_equal(out$stddev_days[[i]], 10957 / 2 *
                     sqrt(max(0, (p[["Q"]] - p[["S"]]^2 / R) / (R - 1))),
                   tolerance = 1e-9)
    }
    expect_identical(out$concept_name[[i]],
                     frames$a$concept_name[frames$a$condition_concept_id ==
                                             out$condition_concept_id[[i]]])
  }
  expect_false(is.unsorted(-out$n_persons))
  # One node under two connection names is refused, never counted twice.
  expect_error(cl$.pool_protected_analysis(list(a = frames$a,
                                                again = frames$a), rp),
               "Duplicate logical source")
})

test_that("protected effects of two live sites pool by per-site meta-analysis", {
  skip_if_not(.pool_client_ready(), "working-tree dsOMOPClient not loadable")
  cl <- asNamespace("dsOMOPClient")
  .release_state()
  withr::local_options(list(dsomop.dp.release_epsilon = 8))
  .release_restart()
  handles <- list(a = .pool_survival_handle(200, 7),
                  b = .pool_survival_handle(240, 8))
  on.exit(lapply(handles, cleanup_handle), add = TRUE)
  id <- "dsomop:cm.effect_estimate"
  # This focused CDM fixture intentionally omits unrelated catalogue tables.
  expect_warning(rp <- .omopAnalysisGet(handles$a, id)$release_pooling,
    "analysis catalog: some entries depend on resources not present: tables",
    fixed = TRUE)
  expect_identical(rp$effect_estimate$strategy, "effect_estimate")
  for (model in c("poisson", "cox")) {
    params <- list(outcome_concept_id = "4000002", model_type = model,
                   tar_start_offset = "0", tar_end_offset = "365",
                   tar_anchor_start = "start", tar_anchor_end = "start")
    frames <- .pool_sites(handles, function(h) {
      .release_run(h, id, params, scope = list("pool_t", "pool_c"))
    })
    le <- vapply(frames, function(df) df$log_estimate[[1L]], numeric(1L))
    se <- vapply(frames, function(df) df$se_log_estimate[[1L]], numeric(1L))
    expect_true(all(is.finite(c(le, se))), info = model)
    pooled <- cl$.pool_protected_analysis(frames, rp, policy = "strict")
    out <- pooled$result
    expect_true(all(out$n_databases == 2L), info = model)
    expect_equal(out$estimate_fixed, rep(.pool_iv(le, se), 2L),
                 tolerance = 1e-9, info = model)
    expect_match(paste(pooled$warnings, collapse = " "),
                 "meta-analysis of the per-site protected", info = model)
    # The signed C6 components pool alongside (persons per arm).
    comps <- attr(out, "protected_components")
    c6 <- lapply(frames, function(df) {
      Filter(function(o) identical(o$family, "C6"),
             attr(df, "dsomop_release")$owners)[[1L]]
    })
    n_target <- sum(vapply(c6, function(o) {
      Filter(function(c) identical(c$stratum, "arm=target"),
             o$components)[[1L]]$n
    }, numeric(1L)))
    expect_equal(comps$persons[comps$stratum == "arm=target" &
                                 comps$owner == c6$a$owner][[1L]],
                 max(0, n_target), info = model)
    if (identical(model, "cox")) {
      expect_match(paste(pooled$warnings, collapse = " "),
                   "No hazard ratio is refitted on them.", fixed = TRUE)
    }
  }
})

test_that("the protected SCCS IRR of two live sites pools by meta-analysis", {
  skip_if_not(.pool_client_ready(), "working-tree dsOMOPClient not loadable")
  cl <- asNamespace("dsOMOPClient")
  .release_state()
  withr::local_options(list(dsomop.dp.release_epsilon = 8))
  .release_restart()
  handles <- list(a = .pool_sccs_handle(220, 11), b = .pool_sccs_handle(260, 12))
  on.exit(lapply(handles, cleanup_handle), add = TRUE)
  id <- "dsomop:sccs.incidence_rate_ratio"
  # The missing resources concern other catalogue entries, not this SCCS input.
  expect_warning(rp <- .omopAnalysisGet(handles$a, id)$release_pooling,
    "analysis catalog: some entries depend on resources not present: tables",
    fixed = TRUE)
  params <- list(exposure_concept_id = "1112807",
                 outcome_concept_id = "4000002", window = "30")
  frames <- .pool_sites(handles, function(h) .release_run(h, id, params))
  le <- vapply(frames, function(df) df$log_irr[[1L]], numeric(1L))
  se <- vapply(frames, function(df) df$se_log_irr[[1L]], numeric(1L))
  expect_true(all(is.finite(c(le, se))))
  pooled <- cl$.pool_protected_analysis(frames, rp, policy = "strict")
  expect_equal(pooled$result$n_databases, 2L)
  expect_equal(pooled$result$estimate_fixed, .pool_iv(le, se),
               tolerance = 1e-9)
  # Only the case totals pool as components; the event-weighted cells never.
  comps <- attr(pooled$result, "protected_components")
  expect_null(attr(comps, "cells"))
  expect_match(paste(pooled$warnings, collapse = " "),
               "SCCS q x exposure cells are not pooled")
})

test_that("standard-mode mixed frames of live sites are labelled when pooled", {
  skip_if_not(.pool_client_ready(), "working-tree dsOMOPClient not loadable")
  cl <- asNamespace("dsOMOPClient")
  .release_state(mode = "standard")
  handles <- list(a = .pool_survival_handle(200, 7),
                  b = .pool_survival_handle(240, 8))
  on.exit(lapply(handles, cleanup_handle), add = TRUE)
  lapply(handles, function(h) {
    suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  })
  id <- "dsomop:cm.mdrr"
  params <- list(outcome_concept_id = "4000002", tar_start_offset = "0",
                 tar_end_offset = "365", tar_anchor_start = "start",
                 tar_anchor_end = "start")
  frames <- .pool_sites(handles, function(h) {
    .release_run(h, id, params, scope = list("pool_t", "pool_c"))
  })
  expect_identical(attr(frames$a, "dsomop_protection")$protection, "mixed")
  meta <- .omopAnalysisGet(handles$a, id)
  expect_null(meta$release_pooling)
  out <- cl$.pool_analysis_contract(frames, meta$pooling_contract)
  expect_equal(out$result$person_days,
               frames$a$person_days + frames$b$person_days)
  expect_match(cl$.analysis_mixed_pool_warnings(frames),
               "person_days.*not exact sums")
})


test_that("one-site record histogram pooling replays its server display", {
  skip_if_not(.pool_client_ready(), "working-tree dsOMOPClient not loadable")
  cl <- asNamespace("dsOMOPClient")
  .release_state()
  h <- create_test_handle(n_persons = 2000)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM measurement")
  DBI::dbWriteTable(h$conn, "measurement", data.frame(
    measurement_id = 1:4000, person_id = rep(1:2000, each = 2),
    measurement_concept_id = 3004410L, measurement_date = "2021-01-01",
    measurement_type_concept_id = 44818518L,
    value_as_number = rep(c(10, 50), 2000),
    value_as_concept_id = NA_integer_, unit_concept_id = 999001L,
    range_low = NA_real_, range_high = NA_real_,
    visit_occurrence_id = NA_integer_), append = TRUE)
  id <- "dsomop:measurement.value_stats"
  frame <- .release_run(h, id, list(concept_id = "3004410"))
  rp <- .omopAnalysisGet(h, id)$release_pooling
  pooled <- cl$.pool_protected_analysis(list(site = frame), rp)$result
  expect_equal(.release_values(pooled), .release_values(frame), tolerance=1e-6)
  # Additional label/payload fields are protected post-processing only.
  hist <- attr(pooled, "value_histograms")
  expect_true(is.data.frame(hist))
  expect_equal(sum(hist$records), frame$n_values, tolerance=1e-6)
  expect_true(all(hist$records >= 0))
  expected <- attr(frame, "dsomop_protection")$value_histograms
  expect_equal(hist$records, expected$records, tolerance=1e-6)
  expect_equal(attr(pooled, "value_quantiles")$value,
               attr(frame, "dsomop_protection")$value_quantiles$value,
               tolerance=1e-6)
})
