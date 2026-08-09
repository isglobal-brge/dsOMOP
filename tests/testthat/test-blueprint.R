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

test_that("loadCdmSpec fails closed for an explicitly unsupported version", {
  spec <- .loadCdmSpec("99.9")
  expect_null(spec)
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
  expect_equal(spec$upstream_source, "OHDSI/CommonDataModel")
  expect_equal(spec$upstream_release, "v5.4.2")
  expect_equal(spec$upstream_commit,
               "aa047a3c620b5c842b4370a0c965e2aa72203b1d")
})

test_that("official OHDSI table metadata preserves the blueprint API", {
  spec <- .loadVendoredSpec("5.4")
  tables <- spec$table_level

  expect_setequal(unique(tables$schema), c("CDM", "Vocabulary", "Results"))
  expect_equal(
    tables$conceptPrefix[tables$cdmTableName == "condition_occurrence"],
    "condition"
  )
  expect_equal(tables$schema[tables$cdmTableName == "concept"], "Vocabulary")
  expect_equal(tables$schema[tables$cdmTableName == "cohort"], "Results")
})

test_that("vendored CDM metadata matches pinned OHDSI release bytes", {
  root <- system.file("ohdsi", package = "dsOMOP")
  if (!nzchar(root)) {
    root <- testthat::test_path("..", "..", "inst", "ohdsi")
  }
  manifest <- jsonlite::fromJSON(
    file.path(root, "UPSTREAM_METADATA.json"), simplifyVector = FALSE
  )
  pinned <- .DSOMOP_VENDORED_OHDSI_METADATA
  expect_setequal(names(manifest), c("contract_version", "source", "files"))
  expect_identical(as.integer(manifest$contract_version),
                   pinned$contract_version)
  expect_identical(manifest$source, pinned$source)
  expect_setequal(names(manifest$files), names(pinned$files))
  disk_files <- list.files(
    root,
    pattern = "^OMOP_CDMv[0-9]+\\.[0-9]+_(Field|Table)_Level\\.csv$"
  )
  expect_setequal(disk_files, names(pinned$files))
  for (name in names(manifest$files)) {
    path <- file.path(root, name)
    expect_true(file.exists(path), info = name)
    expect_setequal(names(manifest$files[[name]]),
                    c("release", "commit", "sha256"))
    expect_identical(manifest$files[[name]], pinned$files[[name]], info = name)
    bytes <- readBin(path, what = "raw", n = file.info(path)$size)
    actual <- unclass(as.character(openssl::sha256(bytes)))
    expect_identical(actual, manifest$files[[name]]$sha256, info = name)
  }
  expect_silent(.loadVerifiedVendoredMetadata(root))
})

test_that("vendored OHDSI verification rejects manifest and byte tampering", {
  root <- system.file("ohdsi", package = "dsOMOP")
  if (!nzchar(root)) {
    root <- testthat::test_path("..", "..", "inst", "ohdsi")
  }
  copy_fixture <- function() {
    target <- tempfile("dsomop-ohdsi-")
    dir.create(target, mode = "0700")
    expect_true(all(file.copy(
      list.files(root, full.names = TRUE), target, copy.mode = TRUE
    )))
    target
  }

  bytes_dir <- copy_fixture()
  on.exit(unlink(bytes_dir, recursive = TRUE, force = TRUE), add = TRUE)
  field_path <- file.path(bytes_dir, "OMOP_CDMv5.4_Field_Level.csv")
  field_bytes <- readBin(
    field_path, what = "raw", n = file.info(field_path)$size
  )
  writeBin(c(field_bytes, charToRaw("\n")), field_path)
  expect_error(
    .loadVerifiedVendoredMetadata(bytes_dir),
    "content hash does not match"
  )

  manifest_dir <- copy_fixture()
  on.exit(unlink(manifest_dir, recursive = TRUE, force = TRUE), add = TRUE)
  manifest_path <- file.path(manifest_dir, "UPSTREAM_METADATA.json")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  manifest$files[["OMOP_CDMv5.4_Field_Level.csv"]]$release <- "v5.4.999"
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)
  expect_error(
    .loadVerifiedVendoredMetadata(manifest_dir),
    "manifest entry is invalid"
  )
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
  expect_equal(
    .classifyConceptRole("condition_occurrence", "condition_concept_id",
                          "CONDITION_", "", is_fk = TRUE, fk_table = "CONCEPT"),
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

test_that("local identifiers and narrative extension fields fail closed", {
  sensitive <- c(
    "patient_id", "enterprise_patient_identifier", "patient_id_raw", "mrn",
    "medical_record_number", "EMAIL", "patient_email_address",
    "phone", "contact_phone_number", "ssn", "date_of_birth", "dob",
    "clinical_comments", "result_free_text", "term_modifiers",
    "note_nlp_term_modifiers"
  )
  expect_true(all(vapply(sensitive, .detectSensitiveColumns, logical(1))))
  expect_true(.detectSensitiveColumns(NA_character_))

  expect_false(.detectSensitiveColumns("person_id"))
  expect_false(.detectSensitiveColumns("measurement_concept_id"))
  expect_false(.detectSensitiveColumns("value_as_number"))
})

test_that("extension contract requires explicit bare table and column names", {
  withr::local_options(list(dsomop.allowed_cdm_extensions = list()))
  expect_equal(.allowedCdmExtensionContract(), list())

  withr::local_options(list(dsomop.allowed_cdm_extensions = list("person_id")))
  expect_error(.allowedCdmExtensionContract(), "named list")

  withr::local_options(list(
    dsomop.allowed_cdm_extensions = list(site_event = "*")
  ))
  expect_error(.allowedCdmExtensionContract(), "wildcards|invalid column")

  withr::local_options(list(
    dsomop.allowed_cdm_extensions = list("site.event" = "person_id")
  ))
  expect_error(.allowedCdmExtensionContract(), "invalid table")
})

test_that("unknown tables and standard-table columns are invisible by default", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  DBI::dbExecute(handle$conn,
    paste(
      "CREATE TABLE site_event (person_id INTEGER,",
      "event_concept_id INTEGER, event_date DATE, research_score REAL)"
    ))
  DBI::dbExecute(handle$conn,
    "ALTER TABLE measurement ADD COLUMN site_quality_score REAL")

  bp <- .buildBlueprint(handle, force = TRUE)
  expect_false("site_event" %in% bp$tables$table_name)
  expect_false("site_quality_score" %in%
                 bp$columns[["measurement"]]$column_name)
  expect_error(
    .compileSelect(handle, "site_event"),
    "not found in CDM schema"
  )
  expect_error(
    .compileSelect(handle, "measurement", columns = "site_quality_score"),
    "Column.*not found"
  )
})

test_that("server contract exposes only named extension columns", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  DBI::dbExecute(handle$conn,
    paste(
      "CREATE TABLE site_event (person_id INTEGER,",
      "event_concept_id INTEGER, event_date DATE, research_score REAL,",
      "comments TEXT, hidden_local_code TEXT)"
    ))
  DBI::dbExecute(handle$conn,
    "ALTER TABLE measurement ADD COLUMN site_quality_score REAL")
  withr::local_options(list(dsomop.allowed_cdm_extensions = list(
    site_event = c("person_id", "event_concept_id", "event_date",
                   "research_score", "comments"),
    measurement = "site_quality_score"
  )))

  bp <- .buildBlueprint(handle, force = TRUE)
  expect_true("site_event" %in%
                bp$tables$table_name[bp$tables$present_in_db])
  expect_setequal(
    bp$columns[["site_event"]]$column_name,
    c("person_id", "event_concept_id", "event_date", "research_score",
      "comments")
  )
  expect_false("hidden_local_code" %in%
                 bp$columns[["site_event"]]$column_name)
  expect_true(bp$columns[["site_event"]]$is_blocked[
    bp$columns[["site_event"]]$column_name == "comments"
  ])
  expect_true("site_quality_score" %in%
                bp$columns[["measurement"]]$column_name)

  sql <- .compileSelect(handle, "site_event")
  expect_true(grepl("research_score", sql, fixed = TRUE))
  expect_false(grepl("comments", sql, fixed = TRUE))
  expect_error(
    .compileSelect(handle, "site_event", columns = "comments"),
    "blocked"
  )
})

test_that("untyped extension identifiers are denied independently of PII access", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))

  DBI::dbExecute(handle$conn, paste(
    "CREATE TABLE site_identity_extension (person_id INTEGER,",
    "event_concept_id INTEGER, research_score REAL, encounter_id TEXT,",
    "member_id TEXT, account_id TEXT, local_record_id INTEGER,",
    "external_member_key TEXT, source_identifier TEXT)"
  ))
  DBI::dbExecute(handle$conn, paste(
    "INSERT INTO site_identity_extension VALUES",
    "(1, 201820, 1.1, 'e1', 'm1', 'a1', 101, 'k1', 's1'),",
    "(2, 201820, 2.2, 'e2', 'm2', 'a2', 102, 'k2', 's2'),",
    "(3, 201820, 3.3, 'e3', 'm3', 'a3', 103, 'k3', 's3')"
  ))
  extension_ids <- c(
    "encounter_id", "member_id", "account_id", "local_record_id",
    "external_member_key", "source_identifier"
  )
  withr::local_options(list(dsomop.allowed_cdm_extensions = list(
    site_identity_extension = c(
      "person_id", "event_concept_id", "research_score", extension_ids
    )
  )))

  bp <- .buildBlueprint(handle, force = TRUE)
  cols <- bp$columns[["site_identity_extension"]]
  expect_true(all(cols$is_extension))
  expect_true(all(cols$is_untyped_identifier[match(
    extension_ids, cols$column_name
  )]))
  expect_true(all(cols$is_blocked[match(extension_ids, cols$column_name)]))
  expect_false(cols$is_untyped_identifier[
    cols$column_name == "event_concept_id"
  ])
  expect_false(cols$is_untyped_identifier[cols$column_name == "person_id"])

  sql <- .compileSelect(handle, "site_identity_extension")
  expect_true(grepl("event_concept_id", sql, fixed = TRUE))
  expect_true(grepl("research_score", sql, fixed = TRUE))
  expect_false(any(vapply(extension_ids, grepl, logical(1), x = sql,
                          fixed = TRUE)))
  expect_false(any(extension_ids %in% .filterableColumns(
    bp, "site_identity_extension"
  )))

  expect_error(
    .compileSelect(
      handle, "site_identity_extension", columns = "encounter_id"
    ),
    "untyped identifiers"
  )
  withr::local_options(list(dsomop.allow_sensitive_columns = TRUE))
  expect_error(
    .compileSelect(
      handle, "site_identity_extension", columns = "account_id",
      block_sensitive = FALSE
    ),
    "untyped identifiers"
  )

  withr::local_options(list(nfilter.subset = 3L))
  extracted <- .extractTable(
    handle, "site_identity_extension", translate_concepts = FALSE
  )
  expect_false(any(extension_ids %in% names(extracted)))
  expect_true(all(c("person_id", "event_concept_id", "research_score") %in%
                    names(extracted)))
})

test_that("column aliases cannot disguise untyped or fake concept identifiers", {
  frame <- data.frame(
    research_score = 1:3,
    event_concept_id = c(1L, 2L, 3L)
  )

  untyped <- .applyColumnAliases(
    frame, .colSpec(c(encounter_id = "research_score"))
  )
  expect_true("research_score" %in% names(untyped))
  expect_false("encounter_id" %in% names(untyped))

  fake_concept <- .applyColumnAliases(
    frame, .colSpec(c(local_record_concept_id = "research_score"))
  )
  expect_true("research_score" %in% names(fake_concept))
  expect_false("local_record_concept_id" %in% names(fake_concept))

  reviewed_concept <- .applyColumnAliases(
    frame, .colSpec(c(diagnosis = "event_concept_id"))
  )
  expect_true("diagnosis" %in% names(reviewed_concept))
  expect_false("event_concept_id" %in% names(reviewed_concept))
})

test_that("blueprint refuses unclassified introspection without an OHDSI spec", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  local_mocked_bindings(
    .loadCdmSpec = function(...) NULL,
    .package = "dsOMOP"
  )
  expect_error(
    .buildBlueprint(handle, force = TRUE),
    "Refusing schema introspection"
  )
})

test_that("temporary-table creation enforces the server-owned per-handle cap", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  withr::local_options(list(dsomop.max_temp_tables_per_handle = 1L))

  expect_equal(
    .createTempTable(handle, "dsomop_cap_one", "SELECT 1 AS value"),
    "dsomop_cap_one"
  )
  expect_error(
    .createTempTable(handle, "dsomop_cap_two", "SELECT 2 AS value"),
    "temporary-table cap"
  )
  expect_identical(handle$temp_tables, "dsomop_cap_one")
  expect_true(DBI::dbExistsTable(handle$conn, "dsomop_cap_one"))
  expect_false(DBI::dbExistsTable(handle$conn, "dsomop_cap_two"))

  .dropTempTable(handle, "dsomop_cap_one")
  expect_equal(
    .createTempTable(handle, "dsomop_cap_two", "SELECT 2 AS value"),
    "dsomop_cap_two"
  )
  .dropTempTable(handle, "dsomop_cap_two")
})

test_that("temporary-table creation fails closed on an invalid server cap", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  withr::local_options(list(dsomop.max_temp_tables_per_handle = 0L))

  expect_error(
    .createTempTable(handle, "dsomop_bad_cap", "SELECT 1 AS value"),
    "max_temp_tables_per_handle"
  )
  expect_false(DBI::dbExistsTable(handle$conn, "dsomop_bad_cap"))
  expect_length(handle$temp_tables, 0L)
})

test_that("failed temporary-table drops retain ownership for a safe retry", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  name <- .createTempTable(
    handle, "dsomop_drop_retry", "SELECT 1 AS value"
  )

  local_mocked_bindings(
    .tempDropSql = function(...) "DROP TABLE malformed syntax",
    .package = "dsOMOP"
  )
  expect_error(.dropTempTable(handle, name), "syntax|near")
  expect_true(name %in% handle$temp_tables)
  expect_true(DBI::dbExistsTable(handle$conn, name))

  # Leave the fixture consistent even while the drop-SQL binding is mocked.
  DBI::dbExecute(handle$conn, paste0("DROP TABLE ", name))
  handle$temp_tables <- setdiff(handle$temp_tables, name)
  handle$temp_connection <- NULL
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
  expect_true(is.list(caps$pseudonymization))
  expect_false(caps$pseudonymization$available)
  expect_false(caps$pseudonymization$legacy_global_opt_in)
})

test_that("getCapabilities never releases an exact unbanded population", {
  handle <- create_test_handle(n_persons = 17)
  on.exit(cleanup_handle(handle))

  withr::with_options(list(nfilter.subset = 3,
                           dsomop.nfilter.band = 5), {
    expect_equal(.getCapabilities(handle)$total_persons, 15)
  })
  withr::with_options(list(nfilter.subset = 20,
                           dsomop.nfilter.band = 5), {
    expect_null(.getCapabilities(handle)$total_persons)
  })
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

test_that("blueprint presence is scoped to each configured OHDSI daimon", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  DBI::dbExecute(handle$conn, "ATTACH DATABASE ':memory:' AS vocab")
  DBI::dbExecute(handle$conn, "ATTACH DATABASE ':memory:' AS results")
  handle$cdm_schema <- "main"
  handle$vocab_schema <- "vocab"
  handle$results_schema <- "results"

  first <- .buildBlueprint(handle)
  expect_false(first$tables$present_in_db[
    first$tables$table_name == "concept"
  ])
  expect_false(any(
    first$tables$table_name == "achilles_results" &
      first$tables$present_in_db
  ))

  DBI::dbExecute(
    handle$conn,
    "CREATE TABLE vocab.concept (concept_id INTEGER, concept_name TEXT)"
  )
  DBI::dbExecute(
    handle$conn,
    "CREATE TABLE results.achilles_results (analysis_id INTEGER, count_value INTEGER)"
  )
  handle$blueprint <- NULL
  handle$results_schema_resolved_done <- FALSE
  second <- .buildBlueprint(handle)
  concept <- second$tables[second$tables$table_name == "concept", ]
  achilles <- second$tables[second$tables$table_name == "achilles_results", ]
  expect_true(concept$present_in_db)
  expect_equal(concept$qualified_name, "vocab.concept")
  expect_true(achilles$present_in_db)
  expect_equal(achilles$qualified_name, "results.achilles_results")
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
