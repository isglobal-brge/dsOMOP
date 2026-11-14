# Regression counterexamples from the independent WS-7 review, using real
# population compilation and every configured backend, including MySQL.
.ws7f_output_backends <- .ws7_scenario_backends()
withr::defer(.ws4_cleanup(.ws7f_output_backends), testthat::teardown_env())

test_that("configured B1-B3 regression vendors are exercised", {
  expected <- c(postgresql = "DSOMOP_TEST_POSTGRES_HOST", mariadb = "DSOMOP_TEST_MARIADB_HOST",
                mysql = "DSOMOP_TEST_MYSQL_HOST")
  required <- names(expected)[nzchar(Sys.getenv(expected))]
  expect_true(all(c("sqlite", required) %in% names(.ws7f_output_backends)))
  if (requireNamespace("duckdb", quietly = TRUE)) expect_true("duckdb" %in% names(.ws7f_output_backends))
})

.ws7f_panel_plan <- function() {
  p <- .ws7_scenario_plan(list(), output = list(format = "person_period", options = list(
    sources = list(list(event = "dx"), list(event = "dx_92200002")),
    window = list(start = -10L, end = 20L, anchor = "index"),
    bins = list(width = 100L), analyses = list("count"))),
    events = list(dx_92200002 = list(table = "measurement",
      concept_set = list(concepts = list(.WS7$LAB)))))
  p$variables <- NULL
  p
}

test_that("B1 panel generated, outcome, exposure and carry collisions precede value fetches", {
  generated <- .ws7f_panel_plan()
  outcome <- generated
  outcome$events$n_dx <- outcome$events$dx_92200002
  outcome$outputs$o$options$sources <- list(list(event = "n_dx"))
  outcome$outputs$o$options$outcomes <- list(dx_count = list(event = "dx"))
  carry <- outcome
  carry$outputs$o$options$outcomes <- list(dx_carry = list(event = "dx"))
  carry$outputs$o$options$carry_forward <- list(n_dx = list(lookback = 30L))
  source_carry <- generated
  source_carry$events$lab$concept_set <- list(concepts = list(.WS7$LAB, .WS7$DX))
  source_carry$events$lab_92200001 <- source_carry$events$dx_92200002
  source_carry$outputs$o$options$sources <- list(list(event = "lab"), list(event = "lab_92200001"))
  source_carry$outputs$o$options$analyses <- list("mean", "last")
  source_carry$outputs$o$options$source_analyses <- list(lab = list("mean"), lab_92200001 = list("last"))
  source_carry$outputs$o$options$carry_forward <- list(lab = list(lookback = 30L),
                                                      lab_92200001 = list(lookback = 30L))
  exposure <- generated
  exposure$events$active_rx <- exposure$events$dx_92200002
  exposure$intervals <- list(rx = list(source = list(event = "drug"), gap = 0L))
  exposure$outputs$o$options$sources <- list(list(event = "active_rx"))
  exposure$outputs$o$options$exposures <- list(rx_count = list(interval = "rx"))
  ambiguous <- generated
  ambiguous$intervals <- list(dx = list(source = list(event = "drug"), gap = 0L))
  ambiguous$outputs$o$options$sources <- list(list(event = "dx"), list(interval = "dx"))
  duplicate <- generated
  duplicate$outputs$o$sources <- list(list(event = "dx"))
  cases <- list(generated = generated, outcome = outcome, carry = carry,
                source_carry = source_carry, exposure = exposure, ambiguous = ambiguous, duplicate = duplicate)
  testthat::local_mocked_bindings(.outputSourceRows = function(...) stop("patient rows were fetched"))
  for (b in names(.ws7f_output_backends)) for (case in names(cases)) {
    h <- .ws7f_output_backends[[b]]$handle
    p <- cases[[case]]
    .irValidate(.irNormalize(.irJsonForm(p)), h)
    err <- tryCatch(.ws7_scenario_run(h, p, .ws7_scenario_frame(7001)), error = identity)
    expect_true(inherits(err, "dsomop_plan_rejection"), info = paste(b, case))
    expect_match(conditionMessage(err), if (case %in% c("ambiguous", "duplicate")) "ambiguous" else "twice",
                 info = paste(b, case))
  }
})

test_that("B1 public panel collision refuses both publication modes and cleans resources", {
  p <- .ws7f_panel_plan()
  for (b in names(.ws7f_output_backends)) local({
    h <- .ws7f_output_backends[[b]]$handle
    .ws7_keys(list(h))
    handle_name <- paste0("ws7f_collision_", b)
    .setHandle(handle_name, h)
    on.exit(rm(list = paste0("handle_", handle_name), envir = .dsomop_env), add = TRUE)
    restore <- .ws7_seed_cohort(h, .ws7_scenario_frame(c(7001, 7003, 7004, 7005, 7006)))
    on.exit(restore(), add = TRUE)
    before <- h$temp_tables
    for (mode in c("memory", "staged")) local({
      target <- new.env(parent = emptyenv())
      target$sentinel <- "caller-owned"
      status <- withr::with_options(list(nfilter.subset = 5L),
        .omopPlanExecuteDS(handle_name, p, list(o = "D"), output_mode = mode, target_env = target))
      info <- paste(b, mode)
      expect_identical(status$status, "refused", info = info)
      expect_identical(status$rejected$o$code, "validation_error", info = info)
      expect_length(status$published, 0L)
      expect_identical(ls(target), "sentinel", info = info)
      expect_identical(target$sentinel, "caller-owned", info = info)
      expect_setequal(h$temp_tables, before)
      ctx <- get(attr(status, "execution_id"), .publicationRegistry)
      on.exit(.publishAbort(ctx), add = TRUE)
      expect_true(ctx$closed, info = info)
      expect_null(ctx$ir)
      expect_false(exists(ctx$id, .planContextRegistry, inherits = FALSE), info = info)
      expect_length(ctx$compile_context$compile_temp_tables, 0L)
      expect_length(ls(ctx$staging_env, all.names = TRUE), 0L)
      expect_true(.validatePlanStatus(unclass(status)))
    })
  })
})

test_that("B1 collision-free panel names retain A-3 values and order", {
  p <- .ws7f_panel_plan()
  names(p$events)[names(p$events) == "dx_92200002"] <- "measurement"
  p$outputs$o$options$sources[[2L]] <- list(event = "measurement")
  for (b in names(.ws7f_output_backends)) {
    f <- .ws7_scenario_run(.ws7f_output_backends[[b]]$handle, p, .ws7_scenario_frame(7001))$frame
    expect_identical(names(f), c("row_id", "cohort_row_id", "person_id", "period_start_days",
      "period_end_days", "days", "dx_92200002_count", "dx_92200003_count", "measurement_count", "pid"),
      info = b)
    expect_identical(unname(unlist(f[c("dx_92200002_count", "dx_92200003_count", "measurement_count")])),
                     c(4L, 1L, 5L), info = b)
  }
})

test_that("B2 sparse first, last and any use real complete visit rosters", {
  p <- .ws7_scenario_plan(list(), output = list(format = "sparse", grain = "person",
    collapse = "last_episode", options = list(sources = list(list(event = "visit_dx")),
      analyses = list("binary", "count"))), events = list(
        vis = list(table = "visit_occurrence", concept_set = list(concepts = list(9202L))),
        visit_dx = list(table = "condition_occurrence", concept_set = list(concepts = list(.WS7$DX)),
                        visit = list(index = TRUE))))
  p$variables <- NULL
  p$populations$t$index$source <- list(event = "vis")
  for (b in names(.ws7f_output_backends)) for (selected_zero in c("none", "first", "last")) {
    plan <- p
    if (selected_zero == "first") plan$events$visit_dx$concept_set <- list(concepts = list(.WS7$DX2))
    if (selected_zero == "last") plan$events$visit_dx$where <-
      list(var = "start_date", op = "<", value = "2020-01-03")
    for (collapse in c("first_episode", "last_episode", "any")) {
      plan$outputs$o$collapse <- collapse
      .irValidate(.irNormalize(.irJsonForm(plan)), .ws7f_output_backends[[b]]$handle)
      res <- .ws7_scenario_run(.ws7f_output_backends[[b]]$handle, plan, frame = NULL)
      f <- res$frame[as.numeric(res$frame$person_id) == 7001, , drop = FALSE]
      ref <- res$compiled$components$covariateRef$frame
      expected <- if (selected_zero == "first") {
        if (collapse == "first_episode") 0 else 1
      } else if (selected_zero == "last" && collapse == "last_episode") 0 else {
        if (collapse == "last_episode") 1 else 2
      }
      info <- paste(b, selected_zero, collapse)
      expect_identical(nrow(f), if (expected == 0) 0L else 2L, info = info)
      if (expected > 0) {
        expect_identical(f$covariate_value[match(c("binary", "count"),
          ref$analysis[match(f$covariate_id, ref$covariate_id)])], c(1, expected), info = info)
      }
      expect_identical(sum(as.numeric(res$roster$person_id) == 7001), 2L, info = info)
    }
  }
})

test_that("B2 sparse collapse orders adjacent BIGINT episode keys losslessly", {
  roster <- data.frame(person_id = c("p", "p", "q", "q"),
    cohort_row_id = c("9007199254740993", "9007199254740992", "10", "2"))
  cells <- data.frame(ep = c(1L, 2L, 4L), covariate_id = 1L, covariate_value = c(1, 2, 3))
  first <- .outputSparseCollapse(cells, roster, "first_episode")
  last <- .outputSparseCollapse(cells, roster, "last_episode")
  any <- .outputSparseCollapse(cells, roster, "any")
  expect_identical(first$ep, c(2L, 4L))
  expect_identical(first$covariate_value, c(2, 3))
  expect_identical(last$ep, 1L) # q's last episode has no cells: no fallback.
  expect_identical(last$covariate_value, 1)
  expect_identical(any$covariate_value, c(2, 3))
})

.ws7f_cost_plan <- function(format) {
  opts <- list(sources = list(list(event = "costs")),
               analyses = if (format == "sparse") list("count") else list("count", "mean", "last"))
  if (format != "sparse") opts$window <- list(start = -10L, end = 60L, anchor = "index")
  if (format == "person_period") opts$bins <- list(width = 100L)
  if (format == "temporal_covariates") opts$bin_width <- 100L
  p <- .ws7_scenario_plan(list(), output = list(format = format, options = opts), events = list(
    metf = list(table = "drug_exposure", concept_set = "metformin", end = list(fallback = "days_supply")),
    costs = list(cost = list(event = list(event = "metf"), field = "total_cost", currency = 44818668L,
                             on_missing_table = "unavailable"))))
  p$concept_sets <- list(metformin = list(concepts = list(1503297L)))
  p$variables <- NULL
  p
}

test_that("B3 bounded costs keep cost values, empty identities and unavailable numeric schemas", {
  frame <- .ws7_scenario_frame(c(1007, 1017))
  formats <- c("sparse", "temporal_covariates", "person_period")
  for (b in names(.ws7f_output_backends)) {
    h <- .ws7f_output_backends[[b]]$handle
    available <- list()
    for (format in formats) for (state in c("available", "empty", "no_identities")) {
      p <- .ws7f_cost_plan(format)
      if (state == "empty") p$events$costs$cost$where <- list(var = "value", op = ">", value = 10000)
      if (state == "no_identities") p$concept_sets$metformin$exclude <- list(1503297L)
      .irValidate(.irNormalize(.irJsonForm(p)), h)
      res <- .ws7_scenario_run(h, p, frame)
      f <- res$compiled$components$o$frame
      info <- paste(b, format, state)
      expect_identical(res$compiled$status, "ok", info = info)
      if (state == "available") available[[format]] <- res$compiled
      if (format == "person_period") {
        if (state == "no_identities") {
          expect_false(any(startsWith(names(f), "costs_")), info = info)
        } else {
          expect_identical(f$costs_count, if (state == "empty") c(0L, 0L) else c(0L, 1L), info = info)
          expect_identical(f$costs_mean, if (state == "empty") c(NA_real_, NA_real_) else c(NA_real_, 20), info = info)
          expect_identical(f$costs_last, f$costs_mean, info = info)
        }
      } else {
        ref <- res$compiled$components$covariateRef$frame
        expect_identical(ref$source, rep("costs", nrow(ref)), info = info)
        expect_identical(ref$concept_id, rep(1503297L, nrow(ref)), info = info)
        expect_identical(nrow(ref), if (state == "no_identities") 0L else if (format == "sparse") 1L else 3L,
                         info = info)
        expect_identical(f$covariate_value, if (state != "available") numeric(0) else
          if (format == "sparse") 1 else c(1, 20, 20), info = info)
        if (state == "available") expect_true(all(as.numeric(f$person_id) == 1017), info = info)
      }
    }
    for (format in formats) {
      p <- .ws7f_cost_plan(format)
      p$events$metf$concept_set <- NULL
      err <- tryCatch(.ws7_scenario_run(h, p, frame), error = identity)
      expect_true(inherits(err, "dsomop_plan_rejection"), info = paste(b, format))
      expect_match(conditionMessage(err), "unrestricted", info = paste(b, format))
    }
    # Exercise the actual missing-table adapter path, preserving its bounded
    # parent identities and own numeric role without metadata fabrication.
    DBI::dbExecute(h$conn, paste0("DROP TABLE ", .qualifyTable(h, "cost", h$cdm_schema)))
    h$blueprint <- NULL
    h$role_registry <- NULL
    for (format in formats) {
      res <- .ws7_scenario_run(h, .ws7f_cost_plan(format), frame)$compiled
      f <- res$components$o$frame
      info <- paste(b, format, "unavailable")
      expect_identical(res$status, "unavailable", info = info)
      expect_identical(res$components$o$fragment$columns,
                       available[[format]]$components$o$fragment$columns, info = info)
      expect_identical(names(f), names(available[[format]]$components$o$frame), info = info)
      if (format == "person_period") {
        expect_true(all(is.na(f[c("costs_count", "costs_mean", "costs_last")])), info = info)
        expect_type(f$costs_count, "integer")
        expect_type(f$costs_mean, "double")
        expect_type(f$costs_last, "double")
      } else {
        expect_identical(nrow(f), 0L, info = info)
        expect_identical(res$components$covariateRef$frame,
                         available[[format]]$components$covariateRef$frame, info = info)
      }
    }
  }
})
