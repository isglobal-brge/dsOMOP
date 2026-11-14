.ws2f_explain_plan <- function() {
  path <- system.file("schema", "examples", "plan-v2-example-A.json", package = "dsOMOP")
  plan <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  plan$events$metf_rx$end <- NULL
  plan$outputs$course_rows <- list(rows = "new_users", format = "long",
                                  sources = list(list(interval = "metf_era")))
  plan$publish <- NULL
  plan
}

test_that("N1: interval long projections use the course contract and effective date policy", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  plan <- .ws2f_explain_plan()
  for (mode in c("relative", "absolute", "remove", "binned")) {
    policy <- list(mode = mode)
    if (mode == "binned") policy$bin_width <- "month"
    plan$outputs$course_rows$options <- list(date_handling = policy)
    ir <- .irPrepare(plan, handle)
    for (classes in list(.irExplain(ir, handle)$column_classes$course_rows,
                         .irExplain(ir)$column_classes$course_rows)) {
      expect_identical(classes$metf_era.concept_id$class, 3L)
      for (column in c("start_date", "end_date", "end_recorded")) {
        entry <- classes[[paste0("metf_era.", column)]]
        if (mode == "remove") expect_null(entry) else {
          expect_identical(entry$class, if (mode == "binned") 3L else 2L)
          expect_identical(entry$source, paste0("interval:metf_era.", column))
        }
      }
      expect_false(any(paste0("metf_era.", c("event_id", "source_ns", "visit_id",
        "group_id", "era_no", "era_key", "n_events")) %in% names(classes)))
      expect_false(any(grepl("drug_exposure_date|drug_source_value", names(classes))))
    }
  }
  # A specified subset transforms only those dates; the remaining exact
  # dates retain their class even when the selected dates are removed/binned.
  plan$outputs$course_rows$options <- list(date_handling = list(
    mode = "remove", date_columns = list("start_date")))
  classes <- .irExplain(.irPrepare(plan, handle), handle)$column_classes$course_rows
  expect_null(classes$metf_era.start_date)
  expect_identical(classes$metf_era.end_date$class, 2L)
  expect_identical(classes$metf_era.end_recorded$class, 2L)
})

test_that("N1: interval outputs resolve their options sources despite default empty sources", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  plan <- .ws2f_explain_plan()
  plan$outputs$course_rows <- list(rows = "new_users", format = "intervals",
    options = list(sources = list(list(interval = "metf_era"))))
  classes <- .irExplain(.irPrepare(plan, handle), handle)$column_classes$course_rows
  expect_identical(classes$metf_era.start_date$class, 2L)
  expect_identical(classes$metf_era.end_date$class, 2L)
  expect_identical(classes$metf_era.concept_id$class, 3L)
})

test_that("N1: long adapters project their safe contracts and the common event log", {
  handle <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(handle$conn))
  plan <- .ws4_plan(events = list(
    episodes = list(table = "episode"),
    members = list(episode_members = list(episode = list(event = "episodes"),
                                         tables = list("drug_exposure"))),
    visits = list(table = "visit_occurrence"),
    costs = list(cost = list(event = list(event = "visits"), field = "total_cost"))))
  plan$outputs <- list(
    members = list(rows = "all", format = "long", sources = list(list(event = "members")),
                   options = list(date_handling = list(mode = "absolute"))),
    costs = list(rows = "all", format = "long", sources = list(list(event = "costs")),
                 options = list(date_handling = list(mode = "remove"))))
  ir <- .irPrepare(plan, handle)
  for (classes in list(.irExplain(ir, handle)$column_classes, .irExplain(ir)$column_classes)) {
    expect_identical(classes$members$members.start_date$class, 2L)
    expect_identical(classes$members$members.end_recorded$class, 2L)
    expect_identical(classes$members$members.member_table$class, 3L)
    expect_identical(classes$members$members.episode_concept_id$class, 3L)
    expect_null(classes$costs$costs.start_date)
    expect_identical(classes$costs$costs.currency_concept_id$class, 3L)
    expect_identical(classes$costs$costs.cost_type_concept_id$class, 3L)
    for (name in names(classes)) {
      expect_false(any(grepl("[.](event_id|source_ns|episode_id|parent_event_id|parent_source_ns)$",
                            names(classes[[name]]))))
    }
  }
  plan$outputs <- list(log = list(rows = "all", format = "long",
    sources = list(list(event = "members"), list(event = "costs")), options = list(event_log = TRUE)))
  classes <- .irExplain(.irPrepare(plan, handle), handle)$column_classes$log
  expect_setequal(names(classes), c("source", "concept_id", "start_days", "end_days",
                                    "value", "unit_concept_id", "event_number"))
  expect_identical(classes$start_days$class, 2L)
  expect_identical(classes$end_days$class, 2L)
  expect_identical(classes$value$class, 3L)
})

test_that("N1: table events use normalized columns and requested derived projections", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  plan <- .ws4_plan(events = list(labs = list(table = "measurement")))
  plan$outputs$o <- list(rows = "all", format = "long", sources = list(list(event = "labs")),
    options = list(date_handling = list(mode = "remove"),
      columns_derived = list("days_from_index", "days_from_anchor", "event_number", "unit"),
      anchors = list(prior = list(source = list(event = "labs"))),
      relations = list(next_lab = list(source = list(event = "labs"), within = list(0L, 30L)))))
  classes <- .irExplain(.irPrepare(plan, handle), handle)$column_classes$o
  expect_identical(classes$labs.value$class, 3L)
  expect_identical(classes$labs.concept_id$class, 3L)
  expect_null(classes$labs.measurement_date)
  expect_null(classes$labs.measurement_id)
  expect_null(classes$labs.value_as_number)
  expect_null(classes$labs.start_date)
  expect_identical(classes$days_from_index$class, 2L)
  expect_identical(classes$days_from_prior$class, 2L)
  expect_identical(classes$event_number$class, 3L)
  expect_identical(classes$unit_concept_id$class, 3L)
  expect_identical(classes$days_to_next_lab$class, 3L)
})

test_that("N1: lowered long outputs retain only requested physical columns and date results", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  plan <- list(outputs = list(labs = list(type = "event_level", table = "measurement",
    columns = list("measurement_date", "value_as_number"), date_handling = "absolute",
    representation = list(format = "long"))))
  ir <- .irPrepare(plan, handle)
  source <- ir$outputs$labs$sources[[1L]]$event
  classes <- .irExplain(ir, handle)$column_classes$labs
  expect_identical(classes[[paste0(source, ".measurement_date")]]$class, 2L)
  expect_identical(classes[[paste0(source, ".value_as_number")]]$class, 3L)
  expect_null(classes[[paste0(source, ".measurement_concept_id")]])
  expect_null(classes[[paste0(source, ".measurement_id")]])
  expect_null(classes[[paste0(source, ".start_date")]])
  plan$outputs$labs$date_handling <- "remove"
  classes <- .irExplain(.irPrepare(plan, handle), handle)$column_classes$labs
  expect_null(classes[[paste0(source, ".measurement_date")]])
  expect_identical(classes[[paste0(source, ".value_as_number")]]$class, 3L)
  plan$outputs$labs$columns <- list()
  classes <- .irExplain(.irPrepare(plan, handle), handle)$column_classes$labs
  expect_false(any(grepl("measurement_date|value_as_number", names(classes))))
})

test_that("N1: table contracts without a domain concept retain safe physical extras", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  plan <- .ws4_plan(events = list(observed = list(table = "observation_period")))
  plan$outputs$o <- list(rows = "all", format = "long",
    sources = list(list(event = "observed")), options = list(date_handling = list(mode = "absolute")))
  ir <- .irPrepare(plan, handle)
  expect_null(.roleOf(handle, "observation_period")$domain_concept)
  classes <- .irExplain(ir, handle)$column_classes$o
  expect_identical(classes$observed.period_type_concept_id$class, 3L)
  expect_identical(classes$observed.start_date$class, 2L)
  expect_identical(classes$observed.end_date$class, 2L)
  expect_null(classes$observed.observation_period_id)
})

test_that("N1: filtered surfacing concept columns remain in the effective v1 projection", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  withr::local_options(list(nfilter.subset = 3L))
  plan <- list(outputs = list(labs = list(type = "event_level", table = "measurement",
    columns = list("value_as_number"), concept_set = 3004410L, concept_col = "unit_concept_id",
    filters = list(custom = list(var = "unit_concept_id", op = "==", value = 8554L)),
    representation = list(format = "long"))), options = list(translate_concepts = FALSE))
  for (override_in_filter in c(FALSE, TRUE)) {
    if (override_in_filter) {
      plan$outputs$labs$filters$concept_col <- plan$outputs$labs$concept_col
      plan$outputs$labs$concept_col <- NULL
    }
    ir <- .irPrepare(plan, handle)
    source <- ir$outputs$labs$sources[[1L]]$event
    expect_null(ir$events[[source]]$concept_col)
    expect_identical(unlist(ir$outputs$labs$legacy_columns, use.names = FALSE),
                     c("unit_concept_id", "value_as_number"))
    result <- .planExecute(handle, plan, list(labs = "ws2f_surfaced"))$labs
    expect_gt(nrow(result), 0L)
    expect_true(all(c("unit_concept_id", "measurement_concept_id", "value_as_number") %in% names(result)))
    classes <- .irExplain(ir, handle)$column_classes$labs
    projected <- sub(paste0("^", source, "[.]"), "", names(classes))
    expect_setequal(projected, setdiff(names(result), "row_id"))
    expect_identical(classes[[paste0(source, ".unit_concept_id")]]$class, 3L)
  }
  # Omitting a requested projection still means the historical default;
  # without a concept filter a predicate alone does not add its column.
  plan$outputs$labs$columns <- NULL
  expect_null(.irPrepare(plan, handle)$outputs$labs$legacy_columns)
  plan$outputs$labs$columns <- list("value_as_number")
  plan$outputs$labs$concept_set <- NULL
  ir <- .irPrepare(plan, handle)
  expect_identical(unlist(ir$outputs$labs$legacy_columns, use.names = FALSE), "value_as_number")
})

test_that("N2: role extensions use physical datatypes with CDM datatype priority", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  row <- .buildBlueprint(handle)$columns$measurement[1L, , drop = FALSE]
  row$cdm_datatype <- ""
  row$is_date <- FALSE
  row$is_extension <- TRUE
  row$is_untyped_identifier <- FALSE
  row$is_sensitive <- FALSE
  row$is_blocked <- FALSE
  row$is_quasi_identifier <- FALSE
  row$concept_role <- ""
  types <- c(extension_numeric = "numeric", extension_text = "text", extension_boolean = "boolean")
  for (column in names(types)) {
    row$column_name <- column
    row$db_datatype <- types[[column]]
    frame <- .roleColumnFrame(row, column)
    expect_identical(frame$value_type, types[[column]])
    expect_true(frame$filterable)
    expect_identical(frame$assignment_class, 3L)
  }
  row$cdm_datatype <- "integer"
  row$db_datatype <- "text"
  expect_identical(.roleColumnFrame(row, row$column_name)$value_type, "integer")
  row$cdm_datatype <- NA_character_
  expect_identical(.roleColumnFrame(row, row$column_name)$value_type, "text")
})
