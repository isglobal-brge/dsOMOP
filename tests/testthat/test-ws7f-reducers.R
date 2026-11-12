# Review regressions B4 / N1: declared reducer types survive transport and
# unavailable tables. Scenarios compile real WS-6 populations and outputs.

test_that("mode orders integer ties exactly without reinterpreting text", {
  for (type in c("integer", "bigint")) {
    expect_identical(.reduceMode(c("10", "2", NA_character_), type), "2")
    expect_identical(.reduceMode(c("-2", "-10", "2"), type), "-10")
    expect_identical(.reduceMode(c("9007199254740993", "9007199254740992"), type),
                     "9007199254740992")
    expect_identical(.reduceMode(c("-9007199254740992", "-9007199254740993"), type),
                     "-9007199254740993")
    expect_identical(.reduceMode(c("9223372036854775807", "9223372036854775806"), type),
                     "9223372036854775806")
    expect_identical(.reduceMode(c("2", "10", "10", NA_character_), type), "10")
  }
  expect_identical(.reduceMode(c("2", "10"), "text"), "10")
  expect_identical(.reduceMode(c("-10", "-2"), "text"), "-10")
  expect_identical(.reduceMode(c("9007199254740993", "10"), "text"), "10")
  expect_identical(.reduceMode(c(2.5, 10.1), "numeric"), 2.5)
  expect_identical(.reduceMode(as.Date(c("2020-02-01", "2020-01-01")), "date"),
                   as.Date("2020-01-01"))
  expect_identical(.reduceMode(c(NA_character_, NA_character_), "bigint"), NA_character_)
})

test_that("compiled BIGINT mode is stable when another person's values change transport", {
  withr::local_options(list(dsomop.allowed_cdm_extensions = list(measurement = c("astra_amount", "astra_text"))))
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  .ws7_extend(h$conn)
  DBI::dbExecute(h$conn, "ALTER TABLE measurement ADD COLUMN astra_amount BIGINT")
  DBI::dbExecute(h$conn, "ALTER TABLE measurement ADD COLUMN astra_text VARCHAR(64)")
  DBI::dbExecute(h$conn, paste0("UPDATE measurement SET astra_amount = CASE measurement_id ",
    "WHEN 970001 THEN 2 WHEN 970002 THEN 10 WHEN 970006 THEN 2147483648 ELSE NULL END, ",
    "astra_text = CASE measurement_id WHEN 970001 THEN '2' WHEN 970002 THEN '10' ELSE NULL END"))
  h$blueprint <- NULL
  h$role_registry <- NULL
  plan <- .ws7_scenario_plan(list(
    amount_mode = list(source = list(event = "lab"), reducer = list(fn = "mode", value = "astra_amount")),
    text_mode = list(source = list(event = "lab"), reducer = list(fn = "mode", value = "astra_text"))))
  before <- .ws7_scenario_run(h, plan)$frame
  expect_identical(as.character(.ws7_value(before, 7001, "amount_mode")), "2")
  expect_identical(.ws7_value(before, 7001, "text_mode"), "10")
  DBI::dbExecute(h$conn, "UPDATE measurement SET astra_amount = NULL WHERE person_id = 7003")
  after <- .ws7_scenario_run(h, plan)$frame
  expect_identical(as.character(.ws7_value(after, 7001, "amount_mode")), "2")
  expect_identical(as.character(.ws7_value(before, 7001, "amount_mode")),
                   as.character(.ws7_value(after, 7001, "amount_mode")))
  for (values in list(c("-2", "-10"),
                      c("9007199254740993", "9007199254740992"),
                      c("-9007199254740992", "-9007199254740993"))) {
    DBI::dbExecute(h$conn, paste0("UPDATE measurement SET astra_amount = CASE measurement_id ",
      "WHEN 970001 THEN ", values[[1L]], " WHEN 970002 THEN ", values[[2L]], " ELSE NULL END"))
    got <- .ws7_scenario_run(h, plan)$frame
    expect_identical(as.character(.ws7_value(got, 7001, "amount_mode")), values[[2L]])
    expect_identical(.ws7_value(got, 7001, "text_mode"), "10")
  }
})

test_that("unavailable raw mode aliases preserve declared types and date policies on every backend", {
  backends <- .ws7_scenario_backends()
  on.exit(.ws4_cleanup(backends), add = TRUE)
  plan <- .ws7_scenario_plan(list(
    canonical_date = list(source = list(event = "lab"), reducer = list(fn = "mode", value = "start_date")),
    alias_date = list(source = list(event = "lab"), reducer = list(fn = "mode", value = "measurement_date")),
    canonical_concept = list(source = list(event = "lab"), reducer = list(fn = "mode", value = "concept_id")),
    alias_concept = list(source = list(event = "lab"), reducer = list(fn = "mode", value = "measurement_concept_id")),
    raw_integer = list(source = list(event = "lab"), reducer = list(fn = "mode", value = "measurement_type_concept_id"))))
  plan$events$lab$on_missing_table <- "unavailable"
  for (b in names(backends)) {
    h <- backends[[b]]$handle
    available <- list()
    for (policy in c("relative", "remove", "binned")) {
      p <- plan
      p$outputs$o$options <- list(date_handling = list(mode = policy))
      if (policy == "binned") p$outputs$o$options$date_handling$bin_width <- "month"
      got <- .ws7_scenario_run(h, p)
      available[[policy]] <- got
      expect_identical(got$frame$canonical_date, got$frame$alias_date, info = paste(b, policy))
      expect_identical(got$frame$canonical_concept, got$frame$alias_concept, info = paste(b, policy))
      if (policy == "remove") expect_false("alias_date" %in% names(got$frame), info = b)
    }
    DBI::dbExecute(h$conn, paste0("DROP TABLE ", .qualifyTable(h, "measurement", h$cdm_schema)))
    h$blueprint <- NULL
    h$role_registry <- NULL
    for (policy in names(available)) {
      p <- plan
      p$outputs$o$options <- list(date_handling = list(mode = policy))
      if (policy == "binned") p$outputs$o$options$date_handling$bin_width <- "month"
      got <- .ws7_scenario_run(h, p)
      info <- paste(b, policy)
      expect_identical(names(got$frame), names(available[[policy]]$frame), info = info)
      expect_identical(vapply(got$frame, typeof, ""), vapply(available[[policy]]$frame, typeof, ""), info = info)
      expect_identical(got$compiled$components$o$fragment$columns,
                       available[[policy]]$compiled$components$o$fragment$columns, info = info)
      expect_identical(got$frame$canonical_date, got$frame$alias_date, info = info)
      expect_identical(got$frame$canonical_concept, got$frame$alias_concept, info = info)
      expect_true(all(is.na(got$frame$alias_concept)), info = info)
      expect_type(got$frame$raw_integer, "integer")
      expect_true(all(is.na(got$frame$raw_integer)), info = info)
      if (policy == "remove") {
        expect_false("alias_date" %in% names(got$frame), info = info)
      } else {
        expect_type(got$frame$alias_date, if (policy == "relative") "integer" else "character")
        expect_true(all(is.na(got$frame$alias_date)), info = info)
      }
    }
  }
})

test_that("unavailable extension fallback preserves known date and blocked column contracts", {
  withr::local_options(list(dsomop.allowed_cdm_extensions = list(note = "astra_amount")))
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  .ws7_extend(h$conn)
  plan <- .ws7_scenario_plan(list(
    amount = list(source = list(event = "notes"), reducer = list(fn = "mode", value = "astra_amount")),
    day = list(source = list(event = "notes"), reducer = list(fn = "mode", value = "note_date"))),
    events = list(notes = list(table = "note", on_missing_table = "unavailable")))
  got <- .ws7_scenario_run(h, plan)$frame
  expect_type(got$amount, "double")
  expect_true(all(is.na(got$amount)))
  expect_type(got$day, "integer")
  expect_true(all(is.na(got$day)))
  for (column in c("note_text", "person_id")) {
    refused <- plan
    refused$variables$amount$reducer$value <- column
    .ws4_expect_rejection(.ws7_scenario_run(h, refused), pattern = "never")
  }
})
