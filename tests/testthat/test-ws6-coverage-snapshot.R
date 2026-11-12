# Coverage component numbers and their recorded union belong to one native
# population operation. A later CDM observation insertion must not reassign
# the component transferred into an already frozen final roster.

test_that("native roster coverage remains bound to its original recorded component", {
  for (precompiled in c(FALSE, TRUE)) {
    h <- .ws4_reach_handle()
    .ws6_population_extend(h$conn)
    h$blueprint <- NULL
    h$role_registry <- NULL
    plan <- .ws6_plan(events = list(idx = .ws6_event("condition_occurrence", .WS6$IDX)),
      populations = list(p = list(kind = "index", coverage = list(policy = "union", gap = 7L),
                                  index = list(source = list(event = "idx"), select = "all"))))
    ctx <- .ws4_ctx(h, plan)
    tryCatch({
      if (precompiled) {
        live <- .coverageSource(ctx)
        expect_false(isTRUE(attr(live, "coverage_snapshot", exact = TRUE)))
        expect_length(ctx$compile_temp_tables, 0L)
      }
      result <- .compilePopulationByName(ctx, "p")
      original <- .fetchFragment(ctx, result$segments, c("row_key", "start_date", "end_date"))
      roster <- .fetchFragment(ctx, result$roster, "cohort_row_id")
      ids <- roster$cohort_row_id[roster$subject_id == 6001]
      expect_true(length(ids) > 0L)
      expect_true(all(as.character(original$start_date[original$row_key %in% ids]) == "2010-01-01"))
      frozen <- .coverageSource(ctx)
      expect_true(isTRUE(attr(frozen, "coverage_snapshot", exact = TRUE)))
      before <- ctx$compile_temp_tables
      expect_identical(.coverageSource(ctx), frozen)
      expect_identical(ctx$compile_temp_tables, before)
      # This new disjoint component would renumber the selected component
      # from 1 to 2 in a live relation, binding the old id to 1900 instead.
      DBI::dbExecute(h$conn, paste0(
        "INSERT INTO observation_period (observation_period_id, person_id, observation_period_start_date, ",
        "observation_period_end_date, period_type_concept_id) VALUES (999001, 6001, '1900-01-01', '1900-01-10', 44814724)"))
      after <- .fetchFragment(ctx, result$segments, c("row_key", "start_date", "end_date"))
      expect_identical(after, original)
      expect_identical(.fetchFragment(ctx, result$roster, "cohort_row_id"), roster)
    }, finally = {
      .planContextClose(ctx)
      expect_length(h$temp_tables, 0L)
      DBI::dbDisconnect(h$conn)
    })
  }
})

test_that("a failed live-to-native coverage snapshot leaves the registered source intact", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  plan <- .ws6_plan(populations = list(p = list(kind = "criteria")))
  ctx <- .ws4_ctx(h, plan)
  original <- .coverageSource(ctx)
  assign("population_snapshots", TRUE, envir = ctx)
  testthat::with_mocked_bindings({
    expect_error(.coverageSource(ctx), "injected coverage materialisation failure")
  }, .populationFreezeFragment = function(...) stop("injected coverage materialisation failure"))
  expect_identical(.fragmentGet(ctx, original$name), original)
  frozen <- .coverageSource(ctx)
  expect_true(isTRUE(attr(frozen, "coverage_snapshot", exact = TRUE)))
  expect_identical(frozen$name, original$name)
  .planContextClose(ctx)
  expect_length(h$temp_tables, 0L)
})
