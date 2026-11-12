# WS-2: the role registry (design-2.10.3 section 6.2, defect B-20).

.roles_field_metadata <- function(version) {
  path <- system.file("ohdsi", paste0("OMOP_CDMv", version, "_Field_Level.csv"),
                      package = "dsOMOP")
  meta <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "latin1")
  split(tolower(meta$cdmFieldName), tolower(meta$cdmTableName))
}

test_that("the packaged role records name only fields of their CDM version", {
  for (version in c("5.3", "5.4")) {
    records <- .roleRecordsCsv(version)
    expect_identical(names(records), .ROLE_CSV_COLUMNS)
    fields <- .roles_field_metadata(version)
    for (i in seq_len(nrow(records))) {
      row <- records[i, ]
      expect_true(row$table_name %in% names(fields), info = paste(version, row$table_name))
      role_cols <- setdiff(.ROLE_CSV_COLUMNS, c("table_name", "event_source", "notes",
                                                "role_concept_cols", "end_fallback"))
      for (col in role_cols) {
        value <- row[[col]]
        if (nzchar(value)) {
          expect_true(value %in% fields[[row$table_name]],
                      info = paste(version, row$table_name, col, value))
        }
      }
      pairs <- .roleConceptPairs(row$role_concept_cols)
      expect_true(all(pairs %in% fields[[row$table_name]]), info = paste(version, row$table_name))
    }
    # every clinical event table of the version is covered or excluded
    clinical <- c("condition_occurrence", "drug_exposure", "procedure_occurrence",
                  "device_exposure", "measurement", "observation", "visit_occurrence",
                  "visit_detail", "observation_period", "death", "specimen", "note",
                  "payer_plan_period", "condition_era", "drug_era", "dose_era")
    expect_true(all(clinical %in% records$table_name), info = version)
  }
  v53 <- .roleRecordsCsv("5.3")
  v54 <- .roleRecordsCsv("5.4")
  expect_identical(v53$end_col[v53$table_name == "procedure_occurrence"], "")
  expect_identical(v54$end_col[v54$table_name == "procedure_occurrence"], "procedure_end_date")
  expect_true("episode" %in% v54$table_name)
  expect_false("episode" %in% v53$table_name)
  expect_null(.roleRecordsCsv("6.0"))
  expect_identical(.roleRecordsCsv("v5.3.1"), v53)
})

test_that(".roleOf returns the section 6.2 record of an event table", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  r <- .roleOf(handle, "condition_occurrence")
  expect_s3_class(r, "dsomop_role_record")
  expect_identical(r$cdm_version, "5.4")
  expect_identical(r$person, "person_id")
  expect_identical(r$event_key, "condition_occurrence_id")
  expect_identical(r$start, "condition_start_date")
  expect_identical(r$end, "condition_end_date")
  expect_identical(r$domain_concept, "condition_concept_id")
  expect_false(r$point_event)
  m <- .roleOf(handle, "MEASUREMENT")
  expect_identical(m$value, "value_as_number")
  expect_null(m$end)
  expect_true(m$point_event)
  # a role whose column the site lacks is NULL, never guessed (the unit test
  # CDM has no measurement.operator_concept_id and no drug days_supply)
  has <- function(t, c) c %in% DBI::dbListFields(handle$conn, t)
  expect_identical(m$operator, if (has("measurement", "operator_concept_id")) "operator_concept_id")
  d <- .roleOf(handle, "drug_exposure")
  expect_identical(d$supply, if (has("drug_exposure", "days_supply")) "days_supply")
  expect_identical(d$end_fallback, d$supply)
  v54 <- .roleRecordsCsv("5.4")
  expect_identical(v54$supply_col[v54$table_name == "drug_exposure"], "days_supply")
  expect_identical(v54$operator_col[v54$table_name == "measurement"], "operator_concept_id")
  death <- .roleOf(handle, "death")
  expect_identical(death$event_key, "person_id")
  expect_identical(death$domain_concept, "cause_concept_id")
  p <- .roleOf(handle, "procedure_occurrence")
  expect_identical(p$end, "procedure_end_date")
  # column classes travel with the record (section 7.6)
  cols <- r$columns
  expect_true(all(c("column_name", "is_date", "filterable", "assignment_class") %in% names(cols)))
  expect_identical(cols$assignment_class[cols$column_name == "condition_start_date"], 2L)
  expect_identical(cols$assignment_class[cols$column_name == "condition_concept_id"], 3L)
})

test_that(".roleOf is NULL for non-sources, absent tables and junk", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  expect_null(.roleOf(handle, "episode_event"))
  expect_null(.roleOf(handle, "cost"))
  expect_null(.roleOf(handle, "person"))
  expect_null(.roleOf(handle, "concept"))
  expect_null(.roleOf(handle, "specimen"))      # not present in the test CDM
  expect_null(.roleOf(handle, NA_character_))
  expect_null(.roleOf(handle, c("a", "b")))
  expect_false(is.null(.roleOf(handle, "episode")))
})

test_that("the registry follows the handle's CDM version", {
  handle <- create_test_handle_v53()
  on.exit(cleanup_handle(handle))
  p <- .roleOf(handle, "procedure_occurrence")
  expect_identical(p$cdm_version, "5.3")
  expect_null(p$end)
  expect_true(p$point_event)
  expect_null(.roleOf(handle, "episode"))
})

test_that("the registry is cached per blueprint and rebuilt when it changes", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  first <- .roleRegistry(handle)
  expect_identical(.roleRegistry(handle)$blueprint_key, first$blueprint_key)
  DBI::dbExecute(handle$conn, "DROP TABLE death")
  handle$blueprint <- NULL
  expect_null(.roleOf(handle, "death"))
})

test_that("B-20: index events come from any table with an event role record", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  bp <- .buildBlueprint(handle)
  # death was outside the 2.10.1 allowlist; it has a role record now
  ct <- withr::with_options(list(nfilter.subset = 0),
    .materializeIndexEventCohort(handle, bp, list(table = "death", primary_limit = "first"),
                                 "dsomop_test_role_index"))
  n <- DBI::dbGetQuery(handle$conn, paste0("SELECT COUNT(*) AS n FROM ", ct))$n
  expect_true(n >= 0)
  # a table without an event role record stays refused
  expect_error(
    .materializeIndexEventCohort(handle, bp, list(table = "concept"), "dsomop_test_role_x"),
    "no event role record")
  expect_error(
    .materializeIndexEventCohort(handle, bp, list(table = "episode_event"), "dsomop_test_role_y"),
    "no event role record")
})

test_that("A-E3: index-event concept sets accept exclude", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  bp <- .buildBlueprint(handle)
  ids <- DBI::dbGetQuery(handle$conn,
    "SELECT DISTINCT condition_concept_id AS id FROM condition_occurrence ORDER BY 1")$id
  skip_if(length(ids) < 2L, "test CDM needs two condition concepts")
  ct <- withr::with_options(list(nfilter.subset = 0),
    .materializeIndexEventCohort(handle, bp, list(
      table = "condition_occurrence", primary_limit = "all",
      concept_set = list(concepts = as.list(ids[1:2]), exclude = list(ids[[2]]))),
      "dsomop_test_exclude_index"))
  got <- DBI::dbGetQuery(handle$conn, paste0(
    "SELECT COUNT(*) AS n FROM ", ct, " c JOIN condition_occurrence t ",
    "ON t.condition_occurrence_id = c.index_event_id WHERE t.condition_concept_id = ", ids[[2]]))$n
  expect_identical(as.integer(got), 0L)
  expect_error(.materializeIndexEventCohort(handle, bp, list(
    table = "condition_occurrence", concept_set = list(concepts = list(ids[[1]]), forged = TRUE)),
    "dsomop_test_exclude_bad"), "Unknown index-event concept-set field")
})
