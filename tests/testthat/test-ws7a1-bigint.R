# B4 rereview regressions: exact SQL-to-R BIGINT transport on all five drivers.
# Adapted from the independent ws7r2 compiler/publication counterexamples.

test_that("compiled modes retain full signed BIGINT domain across vendors", {
  withr::local_options(list(dsomop.allowed_cdm_extensions = list(measurement = c("r2_amount", "r2_text"))))
  backends <- .ws7_scenario_backends()
  on.exit(.ws4_cleanup(backends), add = TRUE)
  expect_setequal(names(backends), c("sqlite", "duckdb", "postgresql", "mariadb", "mysql"))
  for (b in names(backends)) {
    h <- backends[[b]]$handle
    table <- .qualifyTable(h, "measurement", h$cdm_schema)
    DBI::dbExecute(h$conn, paste0("ALTER TABLE ", table, " ADD COLUMN r2_amount BIGINT"))
    DBI::dbExecute(h$conn, paste0("ALTER TABLE ", table, " ADD COLUMN r2_text VARCHAR(64)"))
    h$blueprint <- h$role_registry <- NULL
    plan <- .ws7_scenario_plan(list(
      m = list(source = list(event = "lab"), reducer = list(fn = "mode", value = "r2_amount")),
      nd = list(source = list(event = "lab"), reducer = list(fn = "n_distinct", value = "r2_amount")),
      txt = list(source = list(event = "lab"), reducer = list(fn = "mode", value = "r2_text"))))
    cases <- list(small = c("10", "2"), positive_adjacent = c("9007199254740993", "9007199254740992"),
                  negative_adjacent = c("-9007199254740992", "-9007199254740993"),
                  signed_max = c("9223372036854775807", "9223372036854775806"),
                  signed_min = c("-9223372036854775807", "-9223372036854775808"))
    for (case in names(cases)) {
      v <- cases[[case]]
      DBI::dbExecute(h$conn, paste0("UPDATE ", table, " SET r2_amount = CASE measurement_id WHEN 970001 THEN ", v[[1L]],
        " WHEN 970002 THEN ", v[[2L]], " WHEN 970006 THEN 2147483648 ELSE NULL END, r2_text = CASE measurement_id ",
        "WHEN 970001 THEN '2' WHEN 970002 THEN '10' ELSE NULL END"))
      result <- .ws7_scenario_run(h, plan)
      got <- result$frame
      expect_identical(result$compiled$components$o$fragment$columns$m$type, "bigint")
      expect_identical(result$compiled$components$o$fragment$columns$nd$type, "integer")
      expect_identical(.ws7_value(got, 7001, "nd"), 2L)
      expect_identical(.ws7_value(got, 7004, "m"), NA_character_)
      expect_identical(.ws7_value(got, 7004, "nd"), 0L)
      local({
        run <- .ws7_ctx(h, plan)
        on.exit(.ws7_close(run), add = TRUE)
        columns <- list(event_id = .fragmentColumn("bigint", FALSE, "private"),
          amount = .fragmentColumn("bigint", TRUE, "measure"),
          num = .fragmentColumn("numeric", TRUE, "measure"),
          day = .fragmentColumn("date", TRUE, "start"),
          label = .fragmentColumn("text", TRUE, "measure"))
        f <- .fragment("ws7a1_fetch", "stream", "none", paste0(
          "SELECT measurement_id AS event_id, r2_amount AS amount, ",
          "value_as_number AS num, measurement_date AS day, r2_text AS label FROM ", table,
          " WHERE measurement_id IN (970001, 970002, 970003)"), columns)
        .fragmentRegister(run$cctx, f)
        rows <- .reduceFetch(run$cctx, f, order = "event_id")
        expect_identical(names(rows), names(columns))
        expect_identical(rows$event_id, c("970001", "970002", "970003"))
        expect_identical(rows$amount, c(v, NA_character_), info = paste(b, case))
        expect_identical(rows$num, c(5, NA_real_, 7))
        expect_identical(rows$day, as.Date(c("2020-01-01", "2020-01-05", "2020-01-05")))
        expect_identical(rows$label, c("2", "10", NA_character_))
        expect_identical(.fragmentGet(run$cctx, f$name)$columns, columns)
        ordered <- .reduceFetch(run$cctx, f, order = c("amount", "event_id"))
        expect_identical(ordered$amount[!is.na(ordered$amount)], rev(v), info = paste(b, case, "numeric order"))
        # A dependent SQL fragment still compares, joins and adds numeric BIGINTs.
        numeric <- .fragment("ws7a1_numeric", "stream", "none", paste0(
          "SELECT a.event_id, a.amount + 1 AS amount, a.num, a.day, a.label FROM ",
          f$name, " a INNER JOIN ", f$name, " z ON a.amount = z.amount ",
          "WHERE a.amount < ", v[[1L]]), columns, dependencies = f$name)
        .fragmentRegister(run$cctx, numeric)
        calculated <- .reduceFetch(run$cctx, numeric, order = "amount")
        expect_identical(calculated$event_id, "970002")
        expect_identical(calculated$amount, if (case == "small") "3" else v[[1L]])
        empty <- f
        empty$name <- "ws7a1_empty"
        empty$select_body <- paste0(f$select_body, " AND 1 = 0")
        .fragmentRegister(run$cctx, empty)
        empty_rows <- .reduceFetch(run$cctx, empty)
        expect_identical(names(empty_rows), names(rows))
        expect_identical(vapply(empty_rows, typeof, ""), vapply(rows, typeof, ""))
        expect_equal(nrow(empty_rows), 0L)
      })
      actual <- as.character(.ws7_value(got, 7001, "m"))
      message("FULL_BIGINT backend=", b, " case=", case, " expected=", v[[2L]], " actual=", actual)
      expect_identical(actual, v[[2L]], info = paste(b, case))
      expect_identical(.ws7_value(got, 7001, "txt"), "10", info = paste(b, case))
    }
  }
})

test_that("public BIGINT mode agrees with stored SQL integers in both publication modes", {
  withr::local_options(list(dsomop.allowed_cdm_extensions = list(measurement = "r2_amount")))
  backends <- .ws7_scenario_backends()
  on.exit(.ws4_cleanup(backends), add = TRUE)
  expect_setequal(names(backends), c("sqlite", "duckdb", "postgresql", "mariadb", "mysql"))
  issues <- list()
  for (b in names(backends)) local({
    h <- backends[[b]]$handle
    .ws7_keys(list(h))
    table <- .qualifyTable(h, "measurement", h$cdm_schema)
    DBI::dbExecute(h$conn, paste0("ALTER TABLE ", table, " ADD COLUMN r2_amount BIGINT"))
    h$blueprint <- h$role_registry <- NULL
    plan <- .ws7_scenario_plan(list(
      m = list(source = list(event = "lab"), reducer = list(fn = "mode", value = "r2_amount")),
      nd = list(source = list(event = "lab"), reducer = list(fn = "n_distinct", value = "r2_amount"))))
    handle_name <- paste0("astra_r2_bigint_", b)
    .setHandle(handle_name, h)
    on.exit(rm(list = paste0("handle_", handle_name), envir = .dsomop_env), add = TRUE)
    restore <- .ws7_seed_cohort(h, .ws7_scenario_frame(c(7001, 7003, 7004, 7005, 7006)))
    on.exit(restore(), add = TRUE)
    cases <- list(
      negative_adjacent = c("-9007199254740992", "-9007199254740993"),
      signed_max = c("9223372036854775807", "9223372036854775806"),
      signed_min = c("-9223372036854775807", "-9223372036854775808"))
    for (case in names(cases)) {
      values <- cases[[case]]
      DBI::dbExecute(h$conn, paste0("UPDATE ", table, " SET r2_amount = CASE measurement_id WHEN 970001 THEN ", values[[1L]],
        " WHEN 970002 THEN ", values[[2L]], " WHEN 970006 THEN 2147483648 ELSE NULL END"))
      cast <- if (b %in% c("mysql", "mariadb")) "CHAR(64)" else "VARCHAR(64)"
      witness <- DBI::dbGetQuery(h$conn, paste0("SELECT CAST(r2_amount AS ", cast,
        ") AS exact_amount FROM ", table, " WHERE measurement_id IN (970001, 970002) ORDER BY measurement_id"))
      expect_identical(witness$exact_amount, values, info = paste(b, case, "stored SQL integers"))
      message("SQL_WITNESS backend=", b, " case=", case, " exact=", paste(witness$exact_amount, collapse = ","))
      for (mode in c("memory", "staged")) local({
        target <- new.env(parent = emptyenv())
        status <- withr::with_options(list(nfilter.subset = 5L),
          .omopPlanExecuteDS(handle_name, plan, list(o = "released"), output_mode = mode, target_env = target))
        expect_identical(status$status, "ok", info = paste(b, case, mode))
        ctx <- get(attr(status, "execution_id"), .publicationRegistry)
        on.exit(.publishAbort(ctx), add = TRUE)
        got <- target$released
        if (inherits(got, "FlowerDatasetDescriptor")) {
          chunks <- list()
          .publishReadChunks(got, function(x) chunks[[length(chunks) + 1L]] <<- x)
          got <- do.call(rbind, chunks)
        }
        expect_equal(nrow(got), 5L)
        expect_length(unique(got$person_id), 5L)
        expect_true(all(grepl("^p2", got$person_id)))
        expect_identical(as.character(got$cohort_row_id[[1L]]), "1")
        expect_type(got$m, "character")
        expect_identical(got$m, c(values[[2L]], "2147483648", rep(NA_character_, 3L)))
        expect_identical(got$nd, c(2L, 1L, 0L, 0L, 0L))
        actual <- as.character(got$m[[1L]])
        message("PUBLIC_MODE backend=", b, " case=", case, " mode=", mode,
                " status=", status$status, " support=", length(unique(got$person_id)),
                " expected=", values[[2L]], " actual=", actual, " type=", typeof(got$m), " distinct_expected=2 distinct_actual=", got$nd[[1L]])
        if (!identical(actual, values[[2L]])) issues[[paste(b, case, mode)]] <<- c(expected = values[[2L]], actual = actual)
        if (!identical(got$nd[[1L]], 2L)) issues[[paste(b, case, mode, "distinct")]] <<- c(expected = "2", actual = as.character(got$nd[[1L]]))
        expect_true(ctx$closed)
        expect_length(ctx$compile_context$compile_temp_tables, 0L)
      })
    }
  })
  expect_identical(length(issues), 0L, info = paste(capture.output(str(issues)), collapse = "\n"))
})
