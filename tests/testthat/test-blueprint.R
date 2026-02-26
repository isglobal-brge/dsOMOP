test_that("loadCdmSpec returns spec for version 5.4", {
  spec <- .loadCdmSpec("5.4")
  expect_true(is.list(spec))
  expect_true("table_level" %in% names(spec))
  expect_true("field_level" %in% names(spec))
  expect_true(nrow(spec$table_level) > 0)
  expect_true(nrow(spec$field_level) > 0)
  expect_true("cdmTableName" %in% names(spec$table_level))
  expect_true("schema" %in% names(spec$table_level))
  expect_true("conceptPrefix" %in% names(spec$table_level))
  expect_true("cdmFieldName" %in% names(spec$field_level))
  expect_true(spec$version %in% c("5.4"))
  expect_true(spec$source %in% c("CommonDataModel", "vendored"))
})

test_that("loadCdmSpec falls back for unsupported version", {
  spec <- .loadCdmSpec("99.9")
  # Should fall back to vendored
  expect_true(is.list(spec) || is.null(spec))
  if (is.list(spec)) {
    expect_equal(spec$source, "vendored")
  }
})

test_that("loadCdmSpec normalizes version strings", {
  spec_a <- .loadCdmSpec("v5.4")
  spec_b <- .loadCdmSpec("5.4.0")
  expect_equal(spec_a$version, "5.4")
  expect_equal(spec_b$version, "5.4")
})

test_that("loadCdmSpec returns spec with NULL version", {
  spec <- .loadCdmSpec(NULL)
  # Should fall back to vendored
  expect_true(is.list(spec))
  expect_equal(spec$version, "5.4")
  expect_equal(spec$source, "vendored")
})

test_that("loadVendoredSpec returns vendored spec", {
  spec <- .loadVendoredSpec()
  expect_true(is.list(spec))
  expect_equal(spec$version, "5.4")
  expect_equal(spec$source, "vendored")
})

test_that("listSupportedVersions is accessible", {
  skip_if_not_installed("CommonDataModel")
  versions <- CommonDataModel::listSupportedVersions()
  expect_true(is.character(versions))
  expect_true("5.4" %in% versions)
})

test_that("classifyConceptRoleHeuristic works correctly", {
  expect_equal(.classifyConceptRoleHeuristic("test", "condition_concept_id"),
               "domain_concept")
  expect_equal(.classifyConceptRoleHeuristic("test", "condition_source_concept_id"),
               "source_concept")
  expect_equal(.classifyConceptRoleHeuristic("test", "condition_type_concept_id"),
               "type_concept")
  expect_equal(.classifyConceptRoleHeuristic("test", "person_id"),
               "non_concept")
})

test_that("blueprint builds from test handle", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  bp <- .buildBlueprint(handle)

  expect_true(is.environment(bp))
  expect_true(is.data.frame(bp$tables))
  expect_true(is.list(bp$columns))
  expect_true(is.data.frame(bp$join_graph))

  # Tables present in test DB
  present <- bp$tables$table_name[bp$tables$present_in_db]
  expect_true("person" %in% present)
  expect_true("condition_occurrence" %in% present)
  expect_true("measurement" %in% present)
  expect_true("concept" %in% present)
  expect_true("concept_ancestor" %in% present)
})

test_that("blueprint caches and reuses", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  bp1 <- .buildBlueprint(handle)
  bp2 <- .buildBlueprint(handle)
  expect_identical(bp1, bp2)

  # Force rebuild
  bp3 <- .buildBlueprint(handle, force = TRUE)
  expect_true(is.environment(bp3))
})

test_that("blueprint columns have correct metadata", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  bp <- .buildBlueprint(handle)

  person_cols <- bp$columns[["person"]]
  expect_true(is.data.frame(person_cols))
  expect_true("person_id" %in% person_cols$column_name)
  expect_true("gender_concept_id" %in% person_cols$column_name)

  # Check column metadata fields
  expect_true(all(c("column_name", "concept_role", "is_date",
                     "is_sensitive", "is_blocked") %in% names(person_cols)))
})

test_that("concept role classification works correctly", {
  # Domain concept: matches conceptPrefix
  expect_equal(
    .classifyConceptRole("condition_occurrence", "condition_concept_id",
                          "condition", "", is_fk = TRUE, fk_table = "CONCEPT"),
    "domain_concept"
  )

  # Type concept: fk_domain is "Type Concept"
  expect_equal(
    .classifyConceptRole("condition_occurrence", "condition_type_concept_id",
                          "condition", "Type Concept", is_fk = TRUE, fk_table = "CONCEPT"),
    "type_concept"
  )

  # Source concept: ends in _source_concept_id
  expect_equal(
    .classifyConceptRole("condition_occurrence", "condition_source_concept_id",
                          "condition", "", is_fk = TRUE, fk_table = "CONCEPT"),
    "source_concept"
  )

  # Attribute concept: FK to CONCEPT but not domain/type/source
  expect_equal(
    .classifyConceptRole("measurement", "unit_concept_id",
                          "measurement", "", is_fk = TRUE, fk_table = "CONCEPT"),
    "attribute_concept"
  )

  # Non-concept: doesn't end in _concept_id
  expect_equal(
    .classifyConceptRole("person", "person_id", "gender", "",
                          is_fk = FALSE, fk_table = ""),
    "non_concept"
  )
})

test_that("sensitive column detection works", {
  expect_true(.detectSensitiveColumns("condition_source_value"))
  expect_true(.detectSensitiveColumns("drug_source_value"))
  expect_true(.detectSensitiveColumns("value_as_string"))
  expect_true(.detectSensitiveColumns("note_text"))
  expect_true(.detectSensitiveColumns("condition_source_concept_id"))

  expect_false(.detectSensitiveColumns("person_id"))
  expect_false(.detectSensitiveColumns("condition_concept_id"))
  expect_false(.detectSensitiveColumns("value_as_number"))
  expect_false(.detectSensitiveColumns("measurement_date"))
})

test_that("getDomainConceptColumn returns correct columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  bp <- .buildBlueprint(handle)

  expect_equal(.getDomainConceptColumn(bp, "condition_occurrence"), "condition_concept_id")
  expect_equal(.getDomainConceptColumn(bp, "measurement"), "measurement_concept_id")
  expect_equal(.getDomainConceptColumn(bp, "drug_exposure"), "drug_concept_id")
  expect_equal(.getDomainConceptColumn(bp, "observation"), "observation_concept_id")
  expect_equal(.getDomainConceptColumn(bp, "person"), "gender_concept_id")
})

test_that("getDatePair returns correct pairs for interval tables", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  bp <- .buildBlueprint(handle)

  # condition_occurrence has start/end pair
  pair <- .getDatePair(bp, "condition_occurrence")
  expect_true(!is.null(pair))
  expect_equal(pair$start, "condition_start_date")
  expect_equal(pair$end, "condition_end_date")

  # drug_exposure has start/end pair
  pair <- .getDatePair(bp, "drug_exposure")
  expect_true(!is.null(pair))
  expect_equal(pair$start, "drug_exposure_start_date")
  expect_equal(pair$end, "drug_exposure_end_date")

  # observation_period has start/end pair
  pair <- .getDatePair(bp, "observation_period")
  expect_true(!is.null(pair))
  expect_equal(pair$start, "observation_period_start_date")
  expect_equal(pair$end, "observation_period_end_date")

  # visit_occurrence has start/end pair
  pair <- .getDatePair(bp, "visit_occurrence")
  expect_true(!is.null(pair))
  expect_equal(pair$start, "visit_start_date")
  expect_equal(pair$end, "visit_end_date")
})

test_that("getDatePair returns NULL for single-date tables", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  bp <- .buildBlueprint(handle)

  # measurement has measurement_date but no end_date
  pair <- .getDatePair(bp, "measurement")
  expect_null(pair)

  # procedure_occurrence has procedure_date but no end_date
  pair <- .getDatePair(bp, "procedure_occurrence")
  expect_null(pair)
})

test_that("getDateColumn returns correct columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  bp <- .buildBlueprint(handle)

  expect_equal(.getDateColumn(bp, "condition_occurrence"), "condition_start_date")
  expect_equal(.getDateColumn(bp, "measurement"), "measurement_date")
  expect_equal(.getDateColumn(bp, "drug_exposure"), "drug_exposure_start_date")
  expect_equal(.getDateColumn(bp, "observation_period"), "observation_period_start_date")
})

test_that("findJoinPath finds person_id directly", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  bp <- .buildBlueprint(handle)

  # person table has person_id directly
  path <- .findJoinPath(bp, "person", "person_id")
  expect_equal(path$path, "person")
  expect_equal(length(path$joins), 0)

  # condition_occurrence has person_id directly
  path2 <- .findJoinPath(bp, "condition_occurrence", "person_id")
  expect_equal(path2$path, "condition_occurrence")
})

test_that("getCapabilities returns valid structure", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  .buildBlueprint(handle)
  caps <- .getCapabilities(handle)

  expect_true(is.list(caps))
  expect_true(!is.null(caps$hash))
  expect_true(caps$n_tables > 0)
  expect_true("person" %in% caps$tables)
  expect_equal(caps$dbms, "sqlite")
  expect_true(is.list(caps$cdm_info))
})

test_that("CDM info is detected from cdm_source table", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  bp <- .buildBlueprint(handle)

  expect_true(!is.null(bp$cdm_info))
  expect_equal(bp$cdm_info$source_name, "dsOMOP Test")
  expect_equal(bp$cdm_info$cdm_version, "v5.4")
})

test_that("blueprint stores spec metadata", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  bp <- .buildBlueprint(handle)

  expect_true(!is.null(bp$spec_version))
  expect_true(!is.null(bp$spec_source))
  expect_true(bp$spec_source %in% c("CommonDataModel", "vendored"))
})

test_that("blueprint tables have schema categories", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  bp <- .buildBlueprint(handle)

  # Check schema categories from OHDSI metadata
  person_row <- bp$tables[bp$tables$table_name == "person", ]
  expect_equal(person_row$schema_category, "CDM")

  concept_row <- bp$tables[bp$tables$table_name == "concept", ]
  expect_equal(concept_row$schema_category, "Vocabulary")
})

test_that("blueprint has_person_id is set correctly", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  bp <- .buildBlueprint(handle)

  expect_true(bp$tables$has_person_id[bp$tables$table_name == "condition_occurrence"])
  expect_true(bp$tables$has_person_id[bp$tables$table_name == "measurement"])
  expect_false(bp$tables$has_person_id[bp$tables$table_name == "concept"])
})

test_that("join graph is built from OHDSI FK metadata", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  bp <- .buildBlueprint(handle)

  expect_true(is.data.frame(bp$join_graph))
  expect_true(nrow(bp$join_graph) > 0)
  expect_true(all(c("from_table", "from_column", "to_table", "to_column") %in%
                    names(bp$join_graph)))

  # condition_occurrence should join to person via person_id
  co_joins <- bp$join_graph[bp$join_graph$from_table == "condition_occurrence", ]
  person_join <- co_joins[co_joins$to_table == "person", ]
  expect_true(nrow(person_join) > 0)
})

# --- Structural CDM Version Detection Tests ---

test_that("structural detection identifies v5.4 DB", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  db_tables <- tolower(DBI::dbGetQuery(handle$conn,
    "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")$name)

  result <- .detectCDMVersionFromStructure(handle, db_tables)

  expect_true(!is.null(result))
  expect_equal(result$version, "5.4")
  expect_true(result$evidence_54 > result$evidence_53)
  # Should have detected episode table
  expect_equal(result$checks$episode_table, "5.4")
  # Should have detected procedure_end_date
  expect_equal(result$checks$procedure_end_date, "5.4")
})

test_that("structural detection identifies v5.3 DB", {
  handle <- create_test_handle_v53()
  on.exit(cleanup_handle(handle))

  db_tables <- tolower(DBI::dbGetQuery(handle$conn,
    "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")$name)

  result <- .detectCDMVersionFromStructure(handle, db_tables)

  expect_true(!is.null(result))
  expect_equal(result$version, "5.3")
  expect_true(result$evidence_53 > result$evidence_54)
  # No episode table
  expect_null(result$checks$episode_table)
  # No procedure_end_date
  expect_equal(result$checks$procedure_end_date, "5.3")
})

test_that("structural detection used as fallback when cdm_source missing", {
  handle <- create_test_handle_no_source()
  on.exit(cleanup_handle(handle))

  # cdm_source was dropped, so .detectCDMInfo returns NULL
  db_tables <- tolower(DBI::dbGetQuery(handle$conn,
    "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")$name)
  cdm_info <- .detectCDMInfo(handle, db_tables)
  expect_null(cdm_info)

  # buildBlueprint should use structural detection as fallback
  expect_message(
    bp <- .buildBlueprint(handle),
    "CDM version .* inferred from table structure"
  )

  # Should have picked up the version from structure
  expect_true(!is.null(bp$spec_version))
})

test_that("warning emitted when cdm_source disagrees with structure", {
  handle <- create_test_handle_mismatch()
  on.exit(cleanup_handle(handle))

  expect_warning(
    bp <- .buildBlueprint(handle),
    "cdm_source reports version.*but table structure suggests"
  )
})

test_that("structural detection returns NULL for empty DB", {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conn))

  # Create a minimal DB with no CDM-specific tables
  DBI::dbExecute(conn, "CREATE TABLE some_random_table (id INTEGER)")

  handle <- new.env(parent = emptyenv())
  handle$conn <- conn
  handle$dbms <- "sqlite"
  handle$target_dialect <- "sqlite"
  handle$cdm_schema <- NULL
  handle$blueprint <- NULL

  result <- .detectCDMVersionFromStructure(handle, c("some_random_table"))
  expect_null(result)
})
