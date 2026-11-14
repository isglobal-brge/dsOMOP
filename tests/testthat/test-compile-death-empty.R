test_that("death frames preserve empty rosters with available and unavailable sources", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  plan <- .ws7_scenario_plan(list())
  plan$variables <- NULL
  empty <- .ws7_scenario_frame(1007L)[FALSE, , drop = FALSE]
  for (available in c(TRUE, FALSE)) {
    if (!available) {
      DBI::dbExecute(h$conn, "DROP TABLE death")
      h$blueprint <- NULL
      h$role_registry <- NULL
    }
    local({
      run <- .ws7_ctx(h, plan)
      on.exit(.ws7_close(run))
      pop <- .ws7_population(run, "t", empty)
      cons <- .reduceConsumer(run$cctx, "o", pop$roster, pop$coverage, "episode")
      rows <- .reduceDeathFrame(run$cctx, cons)
      expect_identical(nrow(rows), 0L)
      expect_named(rows, c("row_key", "death_date", "available"))
      expect_identical(rows$available, logical())
      expect_s3_class(rows$death_date, "Date")
      expect_identical(.reduceDeathFrame(run$cctx, cons), rows)
    })
  }
})

test_that("death-censored empty survival populations reach the final support gate", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  .ws7_keys(list(h))
  withr::local_options(list(nfilter.subset = 5L))
  for (mode in c("memory", "staged")) {
    for (cause in c("no_candidates", "eligibility")) local({
      plan <- .ws7_scenario_plan(list(), output = list(format = "survival",
        options = list(outcomes = list(outcome = list(event = "dx")),
          tar = list(start = 1L, end = 365L, anchor = "index"),
          censor = list("observation_end", "death"))))
      plan$variables <- NULL
      frame <- .ws7_scenario_frame(1007L, index = "2020-01-01")
      if (cause == "no_candidates") frame <- frame[FALSE, , drop = FALSE] else
        plan$populations$t$where <- list(observed = list(before = 100000L))
      run <- .ws7_ctx(h, plan, mode)
      on.exit(.ws7_close(run))
      pop <- .ws7_population(run, "t", frame)
      expect_identical(nrow(.reduceRosterFrame(run$cctx, pop$roster)), 0L)
      compiled <- .planCompileOutputs(run$cctx, list(o = "D_empty"), populations = list(t = pop))$o
      expect_identical(compiled$status, "ok")
      expect_identical(nrow(compiled$components$o$frame), 0L)
      .publishCompiled(run$ctx, compiled)
      status <- .publishCommit(run$ctx)
      expect_identical(status$status, "refused")
      expect_identical(status$rejected$o$code, "insufficient_support")
      expect_length(status$published, 0L)
      expect_identical(ls(run$ctx$target_env), character())
      if (mode == "staged") expect_false(dir.exists(run$ctx$staging_dir))
    })
  }
})
