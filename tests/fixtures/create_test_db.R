# ==============================================================================
# dsOMOP v2 - SQLite Test Database Creator
# ==============================================================================
# Creates an in-memory or file-based SQLite database with OMOP CDM tables
# and deterministic test data. Used for unit tests and DSLite integration.
# ==============================================================================

#' Create a SQLite OMOP CDM test database
#'
#' @param path Character; file path for the SQLite DB. Use ":memory:" for
#'   in-memory database (default).
#' @param n_persons Integer; number of test persons (default: 15).
#' @return A DBI connection to the test database.
#' @export
create_test_omop_db <- function(path = ":memory:", n_persons = 15) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("RSQLite required for test database.", call. = FALSE)
  }

  conn <- DBI::dbConnect(RSQLite::SQLite(), path)

  # Create tables
  .create_test_tables(conn)

  # Insert vocabulary
  .insert_test_vocabulary(conn)

  # Insert persons and clinical data
  .insert_test_persons(conn, n_persons)

  # Insert conditions, measurements, etc.
  .insert_test_conditions(conn)
  .insert_test_measurements(conn)
  .insert_test_observations(conn)
  .insert_test_drug_exposures(conn)
  .insert_test_visits(conn)
  .insert_test_procedures(conn)
  .insert_test_death(conn)
  .insert_test_cohorts(conn)
  .insert_test_outcome_conditions(conn)
  .insert_test_repeated_measurements(conn)
  .insert_test_achilles(conn)
  .insert_test_dcsi_hfrs_vocab(conn)
  .insert_test_vocab_extras(conn)
  .insert_test_ohdsi_results(conn)

  conn
}

#' Create a SQLite OMOP CDM v5.3 test database
#'
#' Creates a v5.3-style database: no episode/episode_event tables, no
#' procedure_end_date column, and cdm_source.cdm_version = "v5.3".
#'
#' @param path Character; file path for the SQLite DB. Use ":memory:" for
#'   in-memory database (default).
#' @param n_persons Integer; number of test persons (default: 15).
#' @return A DBI connection to the test database.
#' @export
create_test_omop_db_v53 <- function(path = ":memory:", n_persons = 15) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("RSQLite required for test database.", call. = FALSE)
  }

  conn <- DBI::dbConnect(RSQLite::SQLite(), path)

  # Create v5.3 tables (no episode/episode_event, no procedure_end_date)
  .create_test_tables_v53(conn)

  # Insert vocabulary (reuse, but with v5.3 cdm_source)
  .insert_test_vocabulary_v53(conn)

  # Insert persons and clinical data (reuse existing helpers)
  .insert_test_persons(conn, n_persons)
  .insert_test_conditions(conn)
  .insert_test_measurements(conn)
  .insert_test_observations(conn)
  .insert_test_drug_exposures(conn)
  .insert_test_visits(conn)
  .insert_test_procedures(conn)
  .insert_test_death(conn)
  .insert_test_dcsi_hfrs_vocab(conn)
  .insert_test_vocab_extras(conn)

  conn
}

.create_test_tables_v53 <- function(conn) {
  DBI::dbExecute(conn, "
    CREATE TABLE person (
      person_id INTEGER PRIMARY KEY,
      gender_concept_id INTEGER,
      year_of_birth INTEGER,
      month_of_birth INTEGER,
      day_of_birth INTEGER,
      race_concept_id INTEGER,
      ethnicity_concept_id INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE observation_period (
      observation_period_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      observation_period_start_date TEXT,
      observation_period_end_date TEXT,
      period_type_concept_id INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE condition_occurrence (
      condition_occurrence_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      condition_concept_id INTEGER,
      condition_start_date TEXT,
      condition_end_date TEXT,
      condition_type_concept_id INTEGER,
      visit_occurrence_id INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE drug_exposure (
      drug_exposure_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      drug_concept_id INTEGER,
      drug_exposure_start_date TEXT,
      drug_exposure_end_date TEXT,
      drug_type_concept_id INTEGER,
      quantity REAL,
      visit_occurrence_id INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE measurement (
      measurement_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      measurement_concept_id INTEGER,
      measurement_date TEXT,
      measurement_type_concept_id INTEGER,
      value_as_number REAL,
      value_as_concept_id INTEGER,
      unit_concept_id INTEGER,
      range_low REAL,
      range_high REAL,
      visit_occurrence_id INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE observation (
      observation_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      observation_concept_id INTEGER,
      observation_date TEXT,
      observation_type_concept_id INTEGER,
      value_as_number REAL,
      value_as_string TEXT,
      value_as_concept_id INTEGER,
      visit_occurrence_id INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE concept (
      concept_id INTEGER PRIMARY KEY,
      concept_name TEXT,
      domain_id TEXT,
      vocabulary_id TEXT,
      concept_class_id TEXT,
      standard_concept TEXT,
      concept_code TEXT,
      valid_start_date TEXT,
      valid_end_date TEXT,
      invalid_reason TEXT
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE cdm_source (
      cdm_source_name TEXT,
      cdm_source_abbreviation TEXT,
      cdm_holder TEXT,
      source_description TEXT,
      cdm_version TEXT,
      vocabulary_version TEXT
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE concept_ancestor (
      ancestor_concept_id INTEGER,
      descendant_concept_id INTEGER,
      min_levels_of_separation INTEGER,
      max_levels_of_separation INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE concept_relationship (
      concept_id_1 INTEGER,
      concept_id_2 INTEGER,
      relationship_id TEXT,
      valid_start_date TEXT,
      valid_end_date TEXT,
      invalid_reason TEXT
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE visit_occurrence (
      visit_occurrence_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      visit_concept_id INTEGER,
      visit_start_date TEXT,
      visit_end_date TEXT,
      visit_type_concept_id INTEGER
    )")

  # v5.3: procedure_occurrence has NO procedure_end_date / procedure_end_datetime
  DBI::dbExecute(conn, "
    CREATE TABLE procedure_occurrence (
      procedure_occurrence_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      procedure_concept_id INTEGER,
      procedure_date TEXT,
      procedure_type_concept_id INTEGER,
      visit_occurrence_id INTEGER
    )")

  # death (same shape in 5.3 / 5.4: cause_concept_id is the domain concept)
  DBI::dbExecute(conn, "
    CREATE TABLE death (
      person_id INTEGER,
      death_date TEXT,
      death_type_concept_id INTEGER,
      cause_concept_id INTEGER,
      cause_source_value TEXT,
      cause_source_concept_id INTEGER
    )")

  # v5.3: NO episode or episode_event tables
}

.insert_test_vocabulary_v53 <- function(conn) {
  # Reuse the same vocabulary concepts
  .insert_test_vocabulary(conn)
  # Overwrite cdm_source with v5.3 version
  DBI::dbExecute(conn, "DELETE FROM cdm_source")
  DBI::dbWriteTable(conn, "cdm_source", data.frame(
    cdm_source_name = "dsOMOP Test v5.3",
    cdm_source_abbreviation = "TEST53",
    cdm_holder = "ISGlobal",
    source_description = "Test data (v5.3)",
    cdm_version = "v5.3",
    vocabulary_version = "v5.0",
    stringsAsFactors = FALSE
  ), append = TRUE)
}

.create_test_tables <- function(conn) {
  DBI::dbExecute(conn, "
    CREATE TABLE person (
      person_id INTEGER PRIMARY KEY,
      gender_concept_id INTEGER,
      year_of_birth INTEGER,
      month_of_birth INTEGER,
      day_of_birth INTEGER,
      race_concept_id INTEGER,
      ethnicity_concept_id INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE observation_period (
      observation_period_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      observation_period_start_date TEXT,
      observation_period_end_date TEXT,
      period_type_concept_id INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE condition_occurrence (
      condition_occurrence_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      condition_concept_id INTEGER,
      condition_start_date TEXT,
      condition_end_date TEXT,
      condition_type_concept_id INTEGER,
      visit_occurrence_id INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE drug_exposure (
      drug_exposure_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      drug_concept_id INTEGER,
      drug_exposure_start_date TEXT,
      drug_exposure_end_date TEXT,
      drug_type_concept_id INTEGER,
      quantity REAL,
      visit_occurrence_id INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE measurement (
      measurement_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      measurement_concept_id INTEGER,
      measurement_date TEXT,
      measurement_type_concept_id INTEGER,
      value_as_number REAL,
      value_as_concept_id INTEGER,
      unit_concept_id INTEGER,
      range_low REAL,
      range_high REAL,
      visit_occurrence_id INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE observation (
      observation_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      observation_concept_id INTEGER,
      observation_date TEXT,
      observation_type_concept_id INTEGER,
      value_as_number REAL,
      value_as_string TEXT,
      value_as_concept_id INTEGER,
      visit_occurrence_id INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE concept (
      concept_id INTEGER PRIMARY KEY,
      concept_name TEXT,
      domain_id TEXT,
      vocabulary_id TEXT,
      concept_class_id TEXT,
      standard_concept TEXT,
      concept_code TEXT,
      valid_start_date TEXT,
      valid_end_date TEXT,
      invalid_reason TEXT
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE cdm_source (
      cdm_source_name TEXT,
      cdm_source_abbreviation TEXT,
      cdm_holder TEXT,
      source_description TEXT,
      cdm_version TEXT,
      vocabulary_version TEXT
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE cohort (
      cohort_definition_id INTEGER,
      subject_id INTEGER,
      cohort_start_date TEXT,
      cohort_end_date TEXT
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE cohort_definition (
      cohort_definition_id INTEGER PRIMARY KEY,
      cohort_definition_name TEXT,
      cohort_definition_description TEXT,
      cohort_definition_syntax TEXT
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE concept_ancestor (
      ancestor_concept_id INTEGER,
      descendant_concept_id INTEGER,
      min_levels_of_separation INTEGER,
      max_levels_of_separation INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE concept_relationship (
      concept_id_1 INTEGER,
      concept_id_2 INTEGER,
      relationship_id TEXT,
      valid_start_date TEXT,
      valid_end_date TEXT,
      invalid_reason TEXT
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE visit_occurrence (
      visit_occurrence_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      visit_concept_id INTEGER,
      visit_start_date TEXT,
      visit_end_date TEXT,
      visit_type_concept_id INTEGER
    )")

  # v5.4: procedure_occurrence includes procedure_end_date
  DBI::dbExecute(conn, "
    CREATE TABLE procedure_occurrence (
      procedure_occurrence_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      procedure_concept_id INTEGER,
      procedure_date TEXT,
      procedure_end_date TEXT,
      procedure_type_concept_id INTEGER,
      visit_occurrence_id INTEGER
    )")

  # death: CDM 5.4 spells the domain concept cause_concept_id (NO death_concept_id)
  DBI::dbExecute(conn, "
    CREATE TABLE death (
      person_id INTEGER,
      death_date TEXT,
      death_type_concept_id INTEGER,
      cause_concept_id INTEGER,
      cause_source_value TEXT,
      cause_source_concept_id INTEGER
    )")

  # v5.4: episode table
  DBI::dbExecute(conn, "
    CREATE TABLE episode (
      episode_id INTEGER PRIMARY KEY,
      person_id INTEGER,
      episode_concept_id INTEGER,
      episode_start_date TEXT,
      episode_end_date TEXT,
      episode_parent_id INTEGER,
      episode_number INTEGER,
      episode_object_concept_id INTEGER,
      episode_type_concept_id INTEGER,
      episode_source_value TEXT,
      episode_source_concept_id INTEGER
    )")

  # v5.4: episode_event table
  DBI::dbExecute(conn, "
    CREATE TABLE episode_event (
      episode_id INTEGER,
      event_id INTEGER,
      episode_event_field_concept_id INTEGER
    )")
}

.insert_test_vocabulary <- function(conn) {
  concepts <- data.frame(
    concept_id = c(8507, 8532, 8527, 8516, 201820, 255573,
                   316139, 317009, 1124300, 1154029, 3004410,
                   3025315, 3027018, 3012888, 4005823,
                   8554, 9529, 44818518,
                   4000000, 4000001,
                   9201, 4019824, 4301351, 4000002, 38003563),
    concept_name = c("MALE", "FEMALE", "White",
                     "Black or African American",
                     "Diabetes mellitus",
                     "Chronic obstructive lung disease",
                     "Heart failure", "Asthma",
                     "Metformin", "Albuterol",
                     "Hemoglobin A1c", "Body weight",
                     "Heart rate", "Diastolic blood pressure",
                     "Tobacco user",
                     "percent", "kilogram", "EHR",
                     "Metabolic disease", "Respiratory disease",
                     "Inpatient Visit", "Blood glucose measurement",
                     "Electrocardiogram", "Myocardial infarction",
                     "Hispanic"),
    domain_id = c("Gender", "Gender", "Race", "Race",
                  "Condition", "Condition", "Condition",
                  "Condition", "Drug", "Drug",
                  "Measurement", "Measurement", "Measurement",
                  "Measurement", "Observation",
                  "Unit", "Unit", "Type Concept",
                  "Condition", "Condition",
                  "Visit", "Procedure", "Procedure",
                  "Condition", "Ethnicity"),
    vocabulary_id = c("Gender", "Gender", "Race", "Race",
                      "SNOMED", "SNOMED", "SNOMED", "SNOMED",
                      "RxNorm", "RxNorm",
                      "LOINC", "LOINC", "LOINC", "LOINC",
                      "SNOMED", "UCUM", "UCUM", "Type Concept",
                      "SNOMED", "SNOMED",
                      "Visit", "SNOMED", "SNOMED",
                      "SNOMED", "Ethnicity"),
    concept_class_id = c("Gender", "Gender", "Race", "Race",
                         rep("Clinical Finding", 4),
                         rep("Ingredient", 2),
                         rep("Lab Test", 2), "Clinical Observation",
                         "Clinical Observation",
                         "Clinical Finding",
                         "Unit", "Unit", "Type Concept",
                         "Clinical Finding", "Clinical Finding",
                         "Visit", "Procedure", "Procedure",
                         "Clinical Finding", "Ethnicity"),
    standard_concept = rep("S", 25),
    concept_code = c("M", "F", "1", "2",
                     "73211009", "13645005", "84114007",
                     "195967001", "6809", "435",
                     "4548-4", "29463-7", "8867-4",
                     "8462-4", "110483000",
                     "%", "kg", "EHR",
                     "TEST001", "TEST002",
                     "IP", "33747003", "29303009",
                     "22298006", "Hispanic"),
    valid_start_date = rep("1970-01-01", 25),
    valid_end_date = rep("2099-12-31", 25),
    invalid_reason = rep(NA_character_, 25),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "concept", concepts, append = TRUE)

  # Concept ancestor relationships
  concept_ancestor <- data.frame(
    ancestor_concept_id = c(
      201820, 255573, 316139, 317009,       # Self-ancestry
      4000000, 4000001,                      # Self-ancestry for parent concepts
      4000000,                               # Metabolic disease -> Diabetes
      4000001, 4000001,                      # Respiratory disease -> COPD, Asthma
      4000002                                # MI self-ancestry
    ),
    descendant_concept_id = c(
      201820, 255573, 316139, 317009,
      4000000, 4000001,
      201820,
      255573, 317009,
      4000002
    ),
    min_levels_of_separation = c(
      0L, 0L, 0L, 0L,
      0L, 0L,
      1L,
      1L, 1L,
      0L
    ),
    max_levels_of_separation = c(
      0L, 0L, 0L, 0L,
      0L, 0L,
      2L,
      1L, 1L,
      0L
    ),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "concept_ancestor", concept_ancestor, append = TRUE)

  # CDM Source
  DBI::dbWriteTable(conn, "cdm_source", data.frame(
    cdm_source_name = "dsOMOP Test",
    cdm_source_abbreviation = "TEST",
    cdm_holder = "ISGlobal",
    source_description = "Test data",
    cdm_version = "v5.4",
    vocabulary_version = "v5.0",
    stringsAsFactors = FALSE
  ), append = TRUE)
}

.insert_test_persons <- function(conn, n) {
  persons <- data.frame(
    person_id = 1:n,
    gender_concept_id = rep(c(8507, 8532), length.out = n),
    year_of_birth = 1960 + seq_len(n) * 2,
    month_of_birth = rep(1:12, length.out = n),
    day_of_birth = rep(c(5, 15, 25), length.out = n),
    race_concept_id = rep(c(8527, 8516), length.out = n),
    ethnicity_concept_id = rep(c(NA_integer_, 38003563),
                               length.out = n),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "person", persons, append = TRUE)

  # Observation periods
  obs_periods <- data.frame(
    observation_period_id = 1:n,
    person_id = 1:n,
    observation_period_start_date = rep("2020-01-01", n),
    observation_period_end_date = rep("2024-12-31", n),
    period_type_concept_id = rep(44818518L, n),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "observation_period", obs_periods,
                    append = TRUE)
}

.insert_test_conditions <- function(conn) {
  conditions <- data.frame(
    condition_occurrence_id = 1:12,
    person_id = c(1, 3, 5, 7, 9, 11,    # Diabetes
                  5, 7, 9, 13,           # COPD
                  2, 4),                 # Asthma
    condition_concept_id = c(rep(201820, 6), rep(255573, 4),
                             rep(317009, 2)),
    condition_start_date = c(
      "2019-06-01", "2019-06-01", "2019-06-01",
      "2019-06-01", "2019-06-01", "2019-06-01",
      "2019-08-01", "2019-08-01", "2019-08-01", "2019-08-01",
      "2020-04-10", "2021-07-15"),
    condition_end_date = rep("2024-12-31", 12),
    condition_type_concept_id = rep(44818518L, 12),
    visit_occurrence_id = rep(NA_integer_, 12),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "condition_occurrence", conditions,
                    append = TRUE)
}

.insert_test_measurements <- function(conn) {
  # HbA1c for diabetes patients
  hba1c <- data.frame(
    measurement_id = 1:5,
    person_id = c(1, 3, 5, 7, 9),
    measurement_concept_id = rep(3004410, 5),
    measurement_date = c("2020-06-01", "2020-07-15",
                         "2020-03-20", "2019-12-01",
                         "2021-02-10"),
    measurement_type_concept_id = rep(44818518L, 5),
    value_as_number = c(7.2, 6.5, 8.1, 9.0, 7.5),
    value_as_concept_id = rep(NA_integer_, 5),
    unit_concept_id = rep(8554L, 5),
    range_low = rep(4.0, 5),
    range_high = rep(6.0, 5),
    visit_occurrence_id = rep(NA_integer_, 5),
    stringsAsFactors = FALSE
  )

  # Body weight for most persons
  bw <- data.frame(
    measurement_id = 6:18,
    person_id = 1:13,
    measurement_concept_id = rep(3025315, 13),
    measurement_date = paste0("2021-", sprintf("%02d", 1:13 %% 12 + 1), "-15"),
    measurement_type_concept_id = rep(44818518L, 13),
    value_as_number = c(85.2, 62.1, 78.5, 55.3, 92.0, 68.7, 75.0,
                        58.9, 81.3, 71.2, 89.5, 66.8, 73.4),
    value_as_concept_id = rep(NA_integer_, 13),
    unit_concept_id = rep(9529L, 13),
    range_low = rep(NA_real_, 13),
    range_high = rep(NA_real_, 13),
    visit_occurrence_id = rep(NA_integer_, 13),
    stringsAsFactors = FALSE
  )

  measurements <- rbind(hba1c, bw)
  DBI::dbWriteTable(conn, "measurement", measurements, append = TRUE)
}

.insert_test_observations <- function(conn) {
  obs <- data.frame(
    observation_id = 1:5,
    person_id = c(1, 5, 7, 9, 11),
    observation_concept_id = rep(4005823, 5),
    observation_date = c("2020-01-15", "2019-06-01",
                         "2018-03-10", "2020-11-20",
                         "2022-03-01"),
    observation_type_concept_id = rep(44818518L, 5),
    value_as_number = rep(NA_real_, 5),
    value_as_string = c("Current smoker", "Former smoker",
                        "Current smoker", "Current smoker",
                        "Former smoker"),
    value_as_concept_id = rep(NA_integer_, 5),
    visit_occurrence_id = rep(NA_integer_, 5),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "observation", obs, append = TRUE)
}

.insert_test_drug_exposures <- function(conn) {
  drugs <- data.frame(
    drug_exposure_id = 1:6,
    person_id = c(1, 5, 7, 9, 2, 4),
    drug_concept_id = c(rep(1124300, 4), rep(1154029, 2)),
    drug_exposure_start_date = c("2020-04-01", "2019-02-01",
                                 "2018-07-15", "2020-12-01",
                                 "2020-04-15", "2021-08-01"),
    drug_exposure_end_date = c("2024-12-31", "2024-12-31",
                               "2024-12-31", "2024-12-31",
                               "2020-05-15", "2021-09-01"),
    drug_type_concept_id = rep(44818518L, 6),
    quantity = c(500, 1000, 850, 500, 1, 1),
    visit_occurrence_id = rep(NA_integer_, 6),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "drug_exposure", drugs, append = TRUE)
}

.insert_test_visits <- function(conn) {
  visits <- data.frame(
    visit_occurrence_id = 1:10,
    person_id = 1:10,
    visit_concept_id = rep(9201L, 10),   # Inpatient visit
    visit_start_date = c(
      "2020-03-15", "2020-04-10", "2020-06-01", "2021-07-15",
      "2019-01-10", "2020-02-14", "2018-06-22", "2019-09-01",
      "2020-11-05", "2021-03-20"
    ),
    visit_end_date = c(
      "2020-03-18", "2020-04-12", "2020-06-05", "2021-07-18",
      "2019-01-15", "2020-02-17", "2018-06-28", "2019-09-05",
      "2020-11-09", "2021-03-24"
    ),
    visit_type_concept_id = rep(44818518L, 10),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "visit_occurrence", visits, append = TRUE)
}

.insert_test_procedures <- function(conn) {
  procedures <- data.frame(
    procedure_occurrence_id = 1:5,
    person_id = c(1, 3, 5, 7, 9),
    procedure_concept_id = c(4019824L, 4019824L, 4301351L,
                              4301351L, 4019824L),
    procedure_date = c(
      "2020-03-16", "2020-06-02", "2019-01-11",
      "2018-06-23", "2020-11-06"
    ),
    procedure_type_concept_id = rep(44818518L, 5),
    visit_occurrence_id = c(1L, 3L, 5L, 7L, 9L),
    stringsAsFactors = FALSE
  )
  # Check if table has procedure_end_date (v5.4) and add it if so
  cols <- DBI::dbGetQuery(conn, "PRAGMA table_info('procedure_occurrence')")
  if ("procedure_end_date" %in% cols$name) {
    procedures$procedure_end_date <- c(
      "2020-03-16", "2020-06-02", "2019-01-11",
      "2018-06-23", "2020-11-06"
    )
  }
  DBI::dbWriteTable(conn, "procedure_occurrence", procedures, append = TRUE)
}

.insert_test_death <- function(conn) {
  # A fixed set of decedents (persons 1..12, all present for n_persons >= 12),
  # enough to clear the small-sample floors when exercising death exploration.
  # cause_concept_id references an existing vocab concept (Myocardial infarction).
  deaths <- data.frame(
    person_id = 1:12,
    death_date = rep("2021-03-15", 12),
    death_type_concept_id = rep(32817L, 12),
    cause_concept_id = rep(4000002L, 12),
    cause_source_value = rep("I21", 12),
    cause_source_concept_id = rep(0L, 12),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "death", deaths, append = TRUE)
}

.insert_test_cohorts <- function(conn) {
  # --- Disclosure-audit cohorts of varying sizes -----------------------------
  # Predefined cohorts used to adversarially test the cohort disclosure gate:
  #   id 1  : Diabetes Cohort (6 members) -- pre-existing, kept.
  #   id 2  : COPD Cohort     (4 members) -- pre-existing, kept.
  #   id 10 : LARGE  -- 12 members, comfortably >= nfilter_subset.
  #   id 20 : MEDIUM -- 6  members, >= nfilter_subset.
  #   id 30 : SMALL  -- exactly 1 member, < nfilter_subset (must be invisible).
  #   id 40 : CLEARING-BUT-SMALL -- 4 members: >= nfilter_subset (3) yet below a
  #           band of 5, so it lists but its banded size floors to 0.
  #   id 999: ABSENT -- deliberately NEVER inserted; the "absent" probe id.
  # Cohort definitions (with a distinct SECRET syntax per audit cohort so a
  # below-threshold metadata leak is detectable).
  DBI::dbWriteTable(conn, "cohort_definition", data.frame(
    cohort_definition_id = c(1L, 2L, 10L, 20L, 30L, 40L),
    cohort_definition_name = c(
      "Diabetes Cohort", "COPD Cohort", "Large audit cohort",
      "Medium audit cohort", "Small audit cohort",
      "Clearing-but-small audit cohort"),
    cohort_definition_description = c(
      "Persons with diabetes diagnosis",
      "Persons with COPD diagnosis",
      "LARGE cohort, well above threshold",
      "MEDIUM cohort, above threshold",
      "SMALL secret cohort, single subject",
      "Clearing cohort, below one band"),
    cohort_definition_syntax = c(
      "", "", "SECRET-SYNTAX-10", "SECRET-SYNTAX-20",
      "SECRET-SYNTAX-30", "SECRET-SYNTAX-40"),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # Cohort members. id 1/2 unchanged; audit cohorts reference persons that exist
  # at the default n_persons (15): LARGE = 1..12, MEDIUM = 1..6, SMALL = {7},
  # CLEARING = {2,4,6,8}.
  cohort_member_sets <- list(
    "1"  = c(1L, 3L, 5L, 7L, 9L, 11L),
    "2"  = c(5L, 7L, 9L, 13L),
    "10" = 1:12,
    "20" = c(1L, 2L, 3L, 4L, 5L, 6L),
    "30" = c(7L),
    "40" = c(2L, 4L, 6L, 8L)
  )
  rows <- do.call(rbind, lapply(names(cohort_member_sets), function(id) {
    members <- cohort_member_sets[[id]]
    data.frame(
      cohort_definition_id = rep(as.integer(id), length(members)),
      subject_id = as.integer(members),
      cohort_start_date = rep("2020-01-01", length(members)),
      cohort_end_date = rep("2024-12-31", length(members)),
      stringsAsFactors = FALSE
    )
  }))
  DBI::dbWriteTable(conn, "cohort", rows, append = TRUE)
}

.insert_test_outcome_conditions <- function(conn) {
  # MI events for persons 1, 5, 9 (in diabetes cohort 1)
  # Persons 3, 7, 11 in the cohort do NOT get MI (censored)
  outcomes <- data.frame(
    condition_occurrence_id = 13:15,
    person_id = c(1L, 5L, 9L),
    condition_concept_id = rep(4000002L, 3),
    condition_start_date = c("2020-09-15", "2020-06-10", "2021-05-20"),
    condition_end_date = c("2020-09-30", "2020-06-25", "2021-06-05"),
    condition_type_concept_id = rep(44818518L, 3),
    visit_occurrence_id = rep(NA_integer_, 3),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "condition_occurrence", outcomes, append = TRUE)
}

.insert_test_repeated_measurements <- function(conn) {
  # Second body weight measurement for persons 1-8 on 2022-06-15
  bw2 <- data.frame(
    measurement_id = 19:26,
    person_id = 1:8,
    measurement_concept_id = rep(3025315L, 8),
    measurement_date = rep("2022-06-15", 8),
    measurement_type_concept_id = rep(44818518L, 8),
    value_as_number = c(86.0, 63.5, 79.0, 56.1, 93.2, 69.5, 76.3, 59.8),
    value_as_concept_id = rep(NA_integer_, 8),
    unit_concept_id = rep(9529L, 8),
    range_low = rep(NA_real_, 8),
    range_high = rep(NA_real_, 8),
    visit_occurrence_id = rep(NA_integer_, 8),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "measurement", bw2, append = TRUE)
}

.insert_test_achilles <- function(conn) {
  # Create Achilles tables
  DBI::dbExecute(conn, "
    CREATE TABLE achilles_analysis (
      analysis_id INTEGER NOT NULL,
      analysis_name TEXT,
      stratum_1_name TEXT,
      stratum_2_name TEXT,
      stratum_3_name TEXT,
      stratum_4_name TEXT,
      stratum_5_name TEXT
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE achilles_results (
      analysis_id INTEGER NOT NULL,
      stratum_1 TEXT,
      stratum_2 TEXT,
      stratum_3 TEXT,
      stratum_4 TEXT,
      stratum_5 TEXT,
      count_value INTEGER
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE achilles_results_dist (
      analysis_id INTEGER NOT NULL,
      stratum_1 TEXT,
      stratum_2 TEXT,
      stratum_3 TEXT,
      stratum_4 TEXT,
      stratum_5 TEXT,
      count_value INTEGER,
      min_value REAL,
      max_value REAL,
      avg_value REAL,
      stdev_value REAL,
      median_value REAL,
      p10_value REAL,
      p25_value REAL,
      p75_value REAL,
      p90_value REAL
    )")

  DBI::dbExecute(conn, "
    CREATE TABLE achilles_heel_results (
      analysis_id INTEGER,
      achilles_heel_warning TEXT,
      rule_id INTEGER,
      record_count INTEGER
    )")

  # Seed achilles_results
  ar <- data.frame(
    analysis_id = c(
      1L,
      2L, 2L,
      3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
      5L, 5L,
      4L, 4L,
      109L, 109L, 109L,
      116L, 117L,
      200L,
      400L, 400L, 400L,
      700L, 700L,
      800L,
      1800L, 1800L,
      600L, 600L
    ),
    stratum_1 = c(
      NA,
      "8507", "8532",
      as.character(1960 + seq_len(15) * 2),
      "38003563", "0",
      "8527", "8516",
      "2020", "2021", "2022",
      "2020-01", "2024-12",
      "9201",
      "201820", "255573", "317009",
      "1124300", "1154029",
      "4005823",
      "3004410", "3025315",
      "4019824", "4301351"
    ),
    stratum_2 = NA_character_,
    stratum_3 = NA_character_,
    stratum_4 = NA_character_,
    stratum_5 = NA_character_,
    count_value = c(
      15L,
      8L, 7L,
      rep(1L, 15),
      8L, 7L,
      8L, 7L,
      15L, 15L, 15L,
      15L, 15L,
      10L,
      6L, 4L, 5L,
      4L, 2L,
      5L,
      5L, 13L,
      5L, 5L
    ),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "achilles_results", ar, append = TRUE)

  # Seed some trend data (analysis 401: conditions by month)
  ar_trends <- data.frame(
    analysis_id = c(401L, 401L, 401L, 401L),
    stratum_1 = c("201820", "201820", "255573", "317009"),
    stratum_2 = c("2020-03", "2020-11", "2020-02", "2020-04"),
    stratum_3 = NA_character_,
    stratum_4 = NA_character_,
    stratum_5 = NA_character_,
    count_value = c(2L, 1L, 1L, 1L),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "achilles_results", ar_trends, append = TRUE)

  # Seed achilles_results_dist
  ard <- data.frame(
    analysis_id = c(3L, 105L, 103L),
    stratum_1 = NA_character_,
    stratum_2 = NA_character_,
    stratum_3 = NA_character_,
    stratum_4 = NA_character_,
    stratum_5 = NA_character_,
    count_value = c(15L, 15L, 15L),
    min_value = c(1962, 1096, 22),
    max_value = c(1992, 2922, 62),
    avg_value = c(1977.0, 1826.0, 42.0),
    stdev_value = c(9.5, 523.0, 12.5),
    median_value = c(1978, 1826, 42),
    p10_value = c(1964, 1278, 26),
    p25_value = c(1970, 1643, 32),
    p75_value = c(1986, 2192, 52),
    p90_value = c(1990, 2557, 58),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "achilles_results_dist", ard, append = TRUE)

  # Seed condition age dist (analysis 404)
  ard2 <- data.frame(
    analysis_id = c(404L, 404L),
    stratum_1 = c("201820", "255573"),
    stratum_2 = NA_character_,
    stratum_3 = NA_character_,
    stratum_4 = NA_character_,
    stratum_5 = NA_character_,
    count_value = c(6L, 4L),
    min_value = c(30, 35),
    max_value = c(65, 72),
    avg_value = c(52.5, 55.0),
    stdev_value = c(12.8, 15.1),
    median_value = c(55, 56),
    p10_value = c(38, 40),
    p25_value = c(45, 48),
    p75_value = c(60, 65),
    p90_value = c(63, 70),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "achilles_results_dist", ard2, append = TRUE)

  # Seed achilles_heel_results
  heel <- data.frame(
    analysis_id = c(1L, 2L, NA_integer_),
    achilles_heel_warning = c(
      "WARNING: Small sample size (n=15)",
      "WARNING: Gender concept 8532 has only 7 persons",
      "WARNING: No device records found"
    ),
    rule_id = c(1L, 2L, 3L),
    record_count = c(15L, 7L, 0L),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "achilles_heel_results", heel, append = TRUE)

  # Seed achilles_analysis catalog
  achilles_analysis <- data.frame(
    analysis_id = c(1L, 2L, 3L, 4L, 5L,
                    103L, 105L, 109L, 116L, 117L,
                    200L, 201L,
                    400L, 401L, 403L, 404L,
                    600L, 601L,
                    700L, 701L,
                    800L,
                    1800L, 1801L),
    analysis_name = c(
      "Number of persons", "Number of persons by gender",
      "Number of persons by year of birth",
      "Number of persons by race",
      "Number of persons by ethnicity",
      "Distribution of age at first observation period",
      "Length of observation (days) of first observation period",
      "Number of persons with continuous observation in each year",
      "Number of persons with observation period start by month",
      "Number of persons with observation period end by month",
      "Number of visits by visit concept",
      "Number of visits by visit concept and month",
      "Number of persons with condition by concept",
      "Number of condition records by concept and month",
      "Number of persons with condition by concept and gender",
      "Condition age at first diagnosis distribution",
      "Number of persons with procedure by concept",
      "Number of procedure records by concept and month",
      "Number of persons with drug by concept",
      "Number of drug records by concept and month",
      "Number of persons with observation by concept",
      "Number of persons with measurement by concept",
      "Number of measurement records by concept and month"
    ),
    stratum_1_name = c(
      NA, "gender_concept_id", "year_of_birth",
      "race_concept_id", "ethnicity_concept_id",
      NA, NA, "calendar_year", "calendar_month", "calendar_month",
      "visit_concept_id", "visit_concept_id",
      "concept_id", "concept_id", "concept_id", "concept_id",
      "concept_id", "concept_id",
      "concept_id", "concept_id",
      "concept_id",
      "concept_id", "concept_id"
    ),
    stratum_2_name = c(
      NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA,
      NA, "calendar_month",
      NA, "calendar_month", "gender_concept_id", NA,
      NA, "calendar_month",
      NA, "calendar_month",
      NA,
      NA, "calendar_month"
    ),
    stratum_3_name = NA_character_,
    stratum_4_name = NA_character_,
    stratum_5_name = NA_character_,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "achilles_analysis", achilles_analysis, append = TRUE)
}

.insert_test_dcsi_hfrs_vocab <- function(conn) {
  # ICD9CM source concepts for DCSI testing
  # ICD10CM source concepts for HFRS testing
  # SNOMED target concepts they map to
  icd_concepts <- data.frame(
    concept_id = c(
      # ICD9CM sources (DCSI)
      44826430L,  # 250.50 - Diabetic retinopathy
      44831686L,  # 585.1  - CKD stage 1 (nephropathy tier 2)
      44830809L,  # 356.9  - Neuropathy unspecified
      44829750L,  # 435.9  - TIA unspecified (cerebrovascular tier 1)
      44827319L,  # 410.9  - MI (cardiovascular tier 2)
      44836914L,  # 250.10 - DKA (metabolic tier 2)
      # ICD10CM sources (HFRS)
      35208101L,  # F05 - Delirium
      35207981L,  # F00 - Dementia in Alzheimer's
      35208689L   # G81 - Hemiplegia
    ),
    concept_name = c(
      "Diabetes with ophthalmic manifestations type II",
      "Chronic kidney disease Stage I",
      "Unspecified idiopathic peripheral neuropathy",
      "Unspecified transient cerebral ischemia",
      "Acute myocardial infarction unspecified site",
      "Diabetes with ketoacidosis type I",
      "Delirium not induced by alcohol and other psychoactive substances",
      "Dementia in Alzheimers disease",
      "Hemiplegia"
    ),
    domain_id = rep("Condition", 9),
    vocabulary_id = c(rep("ICD9CM", 6), rep("ICD10CM", 3)),
    concept_class_id = c(rep("5-dig billing code", 6), rep("3-char nonbill code", 3)),
    standard_concept = rep(NA_character_, 9),
    concept_code = c("250.50", "585.1", "356.9", "435.9", "410.9", "250.10",
                     "F05", "F00", "G81"),
    valid_start_date = rep("1970-01-01", 9),
    valid_end_date = rep("2099-12-31", 9),
    invalid_reason = rep(NA_character_, 9),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "concept", icd_concepts, append = TRUE)

  # SNOMED target concepts for the mappings
  snomed_targets <- data.frame(
    concept_id = c(
      4174977L,  # Diabetic retinopathy (SNOMED)
      443611L,   # CKD stage 1 (SNOMED)
      4012159L,  # Peripheral neuropathy (SNOMED)
      373503L,   # TIA (SNOMED)
      312327L,   # MI acute (SNOMED)
      443727L,   # DKA (SNOMED)
      373995L,   # Delirium (SNOMED)
      378419L,   # Dementia in Alzheimer's (SNOMED)
      372924L    # Hemiplegia (SNOMED)
    ),
    concept_name = c(
      "Diabetic retinopathy", "Chronic kidney disease stage 1",
      "Peripheral neuropathy", "Transient cerebral ischemia",
      "Acute myocardial infarction", "Diabetic ketoacidosis",
      "Delirium", "Dementia in Alzheimers disease", "Hemiplegia"
    ),
    domain_id = rep("Condition", 9),
    vocabulary_id = rep("SNOMED", 9),
    concept_class_id = rep("Clinical Finding", 9),
    standard_concept = rep("S", 9),
    concept_code = c("4855003", "431855005", "302226006", "266257000",
                     "57054005", "420422005", "2776000", "26929004",
                     "50582007"),
    valid_start_date = rep("1970-01-01", 9),
    valid_end_date = rep("2099-12-31", 9),
    invalid_reason = rep(NA_character_, 9),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "concept", snomed_targets, append = TRUE)

  # concept_relationship: ICD -> SNOMED "Maps to"
  cr <- data.frame(
    concept_id_1 = c(44826430L, 44831686L, 44830809L, 44829750L,
                     44827319L, 44836914L,
                     35208101L, 35207981L, 35208689L),
    concept_id_2 = c(4174977L, 443611L, 4012159L, 373503L,
                     312327L, 443727L,
                     373995L, 378419L, 372924L),
    relationship_id = rep("Maps to", 9),
    valid_start_date = rep("1970-01-01", 9),
    valid_end_date = rep("2099-12-31", 9),
    invalid_reason = rep(NA_character_, 9),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "concept_relationship", cr, append = TRUE)

  # Condition records using SNOMED targets for DCSI/HFRS test persons
  # Start IDs after existing (15 conditions + 3 outcomes = 18 max used earlier)
  dcsi_hfrs_conditions <- data.frame(
    condition_occurrence_id = 100:108,
    person_id = c(
      # DCSI: person 1 = retinopathy(t1) + CKD(t2) -> score 3
      1L, 1L,
      # DCSI: person 5 = neuropathy(t1) + TIA(t1) -> score 2
      5L, 5L,
      # DCSI: person 7 = MI(t2) + DKA(t2) -> score 4
      7L, 7L,
      # HFRS: person 3 = delirium(3.2)
      3L,
      # HFRS: person 9 = dementia(7.1) + hemiplegia(4.4) -> 11.5
      9L, 9L
    ),
    condition_concept_id = c(
      4174977L, 443611L,    # retinopathy, CKD
      4012159L, 373503L,    # neuropathy, TIA
      312327L, 443727L,     # MI, DKA
      373995L,              # delirium
      378419L, 372924L      # dementia, hemiplegia
    ),
    condition_start_date = rep("2020-01-15", 9),
    condition_end_date = rep("2024-12-31", 9),
    condition_type_concept_id = rep(44818518L, 9),
    visit_occurrence_id = rep(NA_integer_, 9),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "condition_occurrence", dcsi_hfrs_conditions,
                    append = TRUE)
}

# Phase 5 vocabulary extras: concept_synonym + concept_class. These OMOP
# vocabulary tables are not part of the base schema; the vocab-exploration
# aggregates read them, so they are populated here after all concepts exist.
.insert_test_vocab_extras <- function(conn) {
  # concept_synonym: alternative names for a few existing concepts, mirroring
  # the Athena concept "synonyms" panel.
  DBI::dbWriteTable(conn, "concept_synonym", data.frame(
    concept_id = c(201820L, 201820L, 255573L, 4174977L),
    concept_synonym_name = c(
      "Diabetes mellitus (disorder)", "DM",
      "COPD", "Diabetic retinopathy (disorder)"
    ),
    language_concept_id = rep(4180186L, 4),
    stringsAsFactors = FALSE
  ), overwrite = TRUE)

  # concept_class: the dedicated descriptive table, derived from the distinct
  # concept_class_id values already loaded into concept.
  classes <- DBI::dbGetQuery(conn, paste0(
    "SELECT DISTINCT concept_class_id FROM concept ",
    "WHERE concept_class_id IS NOT NULL ORDER BY concept_class_id"))
  DBI::dbWriteTable(conn, "concept_class", data.frame(
    concept_class_id = classes$concept_class_id,
    concept_class_name = classes$concept_class_id,
    concept_class_concept_id = seq_len(nrow(classes)),
    stringsAsFactors = FALSE
  ), overwrite = TRUE)
}

# --- OHDSI Result Tables Fixture Data ---

.insert_test_ohdsi_results <- function(conn) {

  # --- DQD: dqdashboard_results ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dqdashboard_results (
      check_name TEXT,
      cdm_table_name TEXT,
      cdm_field_name TEXT,
      category TEXT,
      subcategory TEXT,
      num_violated_rows INTEGER,
      num_denominator_rows INTEGER,
      pct_violated_rows REAL,
      passed INTEGER,
      failed INTEGER,
      query_text TEXT
    )")

  dqd <- data.frame(
    check_name = c("measurePersonCompleteness", "measurePersonCompleteness",
                    "measureValueCompleteness", "measureValueCompleteness",
                    "isRequired", "fkDomain",
                    "plausibleValueLow", "plausibleValueHigh"),
    cdm_table_name = c("person", "person",
                        "condition_occurrence", "drug_exposure",
                        "observation_period", "condition_occurrence",
                        "measurement", "measurement"),
    cdm_field_name = c("year_of_birth", "gender_concept_id",
                        "condition_concept_id", "drug_concept_id",
                        "observation_period_start_date", "condition_concept_id",
                        "value_as_number", "value_as_number"),
    category = c("Completeness", "Completeness",
                  "Completeness", "Completeness",
                  "Conformance", "Conformance",
                  "Plausibility", "Plausibility"),
    subcategory = c("", "", "", "", "Relational", "Value",
                     "Atemporal", "Atemporal"),
    num_violated_rows = c(5L, 3L, 1L, 2L, 4L, 3L, 5L, 1L),
    num_denominator_rows = c(15L, 15L, 20L, 10L, 15L, 20L, 50L, 50L),
    pct_violated_rows = c(33.3, 20.0, 5.0, 20.0, 26.7, 15.0, 10.0, 2.0),
    passed = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L),
    failed = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L),
    query_text = c(
      "SELECT COUNT(*) FROM person WHERE year_of_birth IS NULL",
      "SELECT COUNT(*) FROM person WHERE gender_concept_id IS NULL",
      "SELECT COUNT(*) FROM condition_occurrence WHERE condition_concept_id = 0",
      "SELECT COUNT(*) FROM drug_exposure WHERE drug_concept_id = 0",
      "SELECT COUNT(*) FROM observation_period WHERE observation_period_start_date IS NULL",
      "SELECT COUNT(*) FROM condition_occurrence WHERE condition_concept_id NOT IN (SELECT concept_id FROM concept)",
      "SELECT COUNT(*) FROM measurement WHERE value_as_number < 0",
      "SELECT COUNT(*) FROM measurement WHERE value_as_number > 1000000"
    ),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "dqdashboard_results", dqd, append = TRUE)

  # --- CohortDiagnostics: cohort_count ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS cohort_count (
      cohort_id INTEGER,
      cohort_entries INTEGER,
      cohort_subjects INTEGER,
      database_id TEXT
    )")

  cc <- data.frame(
    cohort_id = c(1L, 2L),
    cohort_entries = c(120L, 2L),
    cohort_subjects = c(85L, 2L),
    database_id = c("test_db", "test_db"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "cohort_count", cc, append = TRUE)

  # --- CohortDiagnostics: incidence_rate ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS incidence_rate (
      cohort_id INTEGER,
      gender TEXT,
      age_group TEXT,
      calendar_year INTEGER,
      cohort_count INTEGER,
      person_years REAL,
      incidence_rate REAL,
      database_id TEXT
    )")

  ir <- data.frame(
    cohort_id = c(1L, 1L, 1L, 1L),
    gender = c("MALE", "FEMALE", "MALE", "FEMALE"),
    age_group = c("30-39", "30-39", "40-49", "40-49"),
    calendar_year = c(2020L, 2020L, 2020L, 2020L),
    cohort_count = c(45L, 40L, 20L, 15L),
    person_years = c(320.5, 290.0, 150.2, 125.8),
    incidence_rate = c(14.04, 13.79, 13.32, 11.92),
    database_id = c("test_db", "test_db", "test_db", "test_db"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "incidence_rate", ir, append = TRUE)

  # --- CohortIncidence: incidence_summary ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS incidence_summary (
      target_cohort_definition_id INTEGER,
      outcome_id INTEGER,
      persons_at_risk INTEGER,
      person_days REAL,
      person_outcomes INTEGER,
      outcomes INTEGER,
      incidence_proportion_p100p REAL,
      incidence_rate_p100py REAL
    )")

  is_data <- data.frame(
    target_cohort_definition_id = c(1L, 1L),
    outcome_id = c(10L, 20L),
    persons_at_risk = c(500L, 500L),
    person_days = c(182500.0, 182500.0),
    person_outcomes = c(50L, 1L),
    outcomes = c(75L, 1L),
    incidence_proportion_p100p = c(10.0, 0.2),
    incidence_rate_p100py = c(15.0, 0.2),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "incidence_summary", is_data, append = TRUE)

  # --- Characterization: c_cohort_counts ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS c_cohort_counts (
      cohort_id INTEGER,
      setting_id INTEGER,
      num_persons INTEGER,
      database_id TEXT
    )")

  ccc <- data.frame(
    cohort_id = c(1L, 2L),
    setting_id = c(1L, 1L),
    num_persons = c(200L, 1L),
    database_id = c("test_db", "test_db"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "c_cohort_counts", ccc, append = TRUE)

  # --- Characterization: c_covariates ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS c_covariates (
      cohort_id INTEGER,
      covariate_id INTEGER,
      covariate_name TEXT,
      sum_value INTEGER,
      average_value REAL,
      database_id TEXT
    )")

  cv <- data.frame(
    cohort_id = c(1L, 1L, 2L),
    covariate_id = c(1001L, 1002L, 1001L),
    covariate_name = c("Age group: 30-39", "Gender: Female", "Age group: 30-39"),
    sum_value = c(80L, 110L, 2L),
    average_value = c(0.4, 0.55, 0.5),
    database_id = c("test_db", "test_db", "test_db"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "c_covariates", cv, append = TRUE)

  # --- CohortDiagnostics: index_event_breakdown ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS index_event_breakdown (
      cohort_id INTEGER, concept_id INTEGER, concept_name TEXT,
      domain_field TEXT, concept_count INTEGER, subject_count INTEGER,
      database_id TEXT
    )")
  DBI::dbWriteTable(conn, "index_event_breakdown", data.frame(
    cohort_id = c(1L, 1L, 1L, 2L),
    concept_id = c(201820L, 316139L, 255573L, 317009L),
    concept_name = c("Diabetes mellitus", "Heart failure",
                     "Chronic obstructive lung disease", "Asthma"),
    domain_field = rep("condition_concept_id", 4),
    concept_count = c(120L, 45L, 38L, 60L),
    subject_count = c(85L, 30L, 25L, 42L),
    database_id = rep("test_db", 4),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- CohortDiagnostics: visit_context ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS visit_context (
      cohort_id INTEGER, visit_concept_id INTEGER, visit_context TEXT,
      subjects INTEGER, database_id TEXT
    )")
  DBI::dbWriteTable(conn, "visit_context", data.frame(
    cohort_id = c(1L, 1L, 1L, 2L),
    visit_concept_id = c(9201L, 9202L, 9203L, 9201L),
    visit_context = c("Inpatient Visit", "Outpatient Visit",
                      "Emergency Room Visit", "Inpatient Visit"),
    subjects = c(65L, 40L, 18L, 28L),
    database_id = rep("test_db", 4),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- CohortDiagnostics: temporal_covariate_value ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS temporal_covariate_value (
      cohort_id INTEGER, covariate_id INTEGER, covariate_name TEXT,
      time_id INTEGER, mean REAL, sd REAL, database_id TEXT
    )")
  DBI::dbWriteTable(conn, "temporal_covariate_value", data.frame(
    cohort_id = rep(1L, 8),
    covariate_id = c(rep(2001L, 5), rep(2002L, 3)),
    covariate_name = c(rep("Diabetes diagnosis", 5),
                       rep("Heart failure diagnosis", 3)),
    time_id = c(-365L, -180L, -30L, 0L, 30L, -365L, -180L, 0L),
    mean = c(0.12, 0.18, 0.35, 0.85, 0.42, 0.05, 0.08, 0.25),
    sd = c(0.05, 0.06, 0.08, 0.10, 0.09, 0.03, 0.04, 0.07),
    database_id = rep("test_db", 8),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- CohortDiagnostics: time_series ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS time_series (
      cohort_id INTEGER, calendar_year INTEGER, calendar_month INTEGER,
      series_type TEXT, records INTEGER, subjects INTEGER, database_id TEXT
    )")
  DBI::dbWriteTable(conn, "time_series", data.frame(
    cohort_id = rep(1L, 6),
    calendar_year = rep(2020L, 6),
    calendar_month = 1:6,
    series_type = rep("cohort_entries", 6),
    records = c(15L, 22L, 30L, 28L, 35L, 40L),
    subjects = c(12L, 18L, 25L, 20L, 28L, 32L),
    database_id = rep("test_db", 6),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- CohortDiagnostics: included_source_concept ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS included_source_concept (
      cohort_id INTEGER, concept_id INTEGER, concept_name TEXT,
      concept_count INTEGER, source_concept_id INTEGER,
      source_concept_name TEXT, database_id TEXT
    )")
  DBI::dbWriteTable(conn, "included_source_concept", data.frame(
    cohort_id = rep(1L, 3),
    concept_id = c(201820L, 316139L, 255573L),
    concept_name = c("Diabetes mellitus", "Heart failure", "COPD"),
    concept_count = c(85L, 30L, 25L),
    source_concept_id = c(44826430L, 44827319L, 44830809L),
    source_concept_name = c("E11.9 Type 2 diabetes",
                            "I50.9 Heart failure unspecified",
                            "J44.1 COPD with acute exacerbation"),
    database_id = rep("test_db", 3),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- CohortDiagnostics: orphan_concept ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS orphan_concept (
      cohort_id INTEGER, concept_id INTEGER, concept_name TEXT,
      concept_count INTEGER, standard_concept TEXT, database_id TEXT
    )")
  DBI::dbWriteTable(conn, "orphan_concept", data.frame(
    cohort_id = rep(1L, 3),
    concept_id = c(999001L, 999002L, 999003L),
    concept_name = c("Orphan condition A", "Orphan condition B",
                     "Orphan condition C"),
    concept_count = c(12L, 8L, 5L),
    standard_concept = c("S", "S", NA_character_),
    database_id = rep("test_db", 3),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- Characterization: c_covariates_continuous ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS c_covariates_continuous (
      cohort_id INTEGER, covariate_id INTEGER, covariate_name TEXT,
      count_value INTEGER, min_value REAL, p25_value REAL,
      median_value REAL, p75_value REAL, max_value REAL, database_id TEXT
    )")
  DBI::dbWriteTable(conn, "c_covariates_continuous", data.frame(
    cohort_id = rep(1L, 5),
    covariate_id = 3001:3005,
    covariate_name = c("Age at index", "BMI", "Systolic blood pressure",
                       "HbA1c", "Charlson comorbidity index"),
    count_value = c(200L, 180L, 175L, 160L, 200L),
    min_value = c(18.0, 16.5, 85.0, 4.2, 0.0),
    p25_value = c(35.0, 22.8, 115.0, 5.8, 1.0),
    median_value = c(48.0, 27.5, 130.0, 7.1, 2.0),
    p75_value = c(62.0, 33.1, 148.0, 8.5, 4.0),
    max_value = c(89.0, 52.0, 210.0, 14.0, 12.0),
    database_id = rep("test_db", 5),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- Characterization: c_time_to_event ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS c_time_to_event (
      cohort_id INTEGER, outcome_id INTEGER, time_metric TEXT,
      time_value REAL, value REAL, database_id TEXT
    )")
  DBI::dbWriteTable(conn, "c_time_to_event", data.frame(
    cohort_id = rep(1L, 8),
    outcome_id = c(rep(10L, 4), rep(20L, 4)),
    time_metric = rep("time_to_first", 8),
    time_value = rep(c(30.0, 90.0, 180.0, 365.0), 2),
    value = c(0.92, 0.85, 0.78, 0.65, 0.95, 0.90, 0.82, 0.71),
    database_id = rep("test_db", 8),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- Characterization: c_dechallenge_rechallenge ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS c_dechallenge_rechallenge (
      cohort_id INTEGER, dechallenge_stop_interval INTEGER,
      dechallenge_evaluation_window INTEGER,
      num_cases INTEGER, num_dechallenge_attempt INTEGER,
      num_dechallenge_success INTEGER, num_rechallenge_attempt INTEGER,
      num_rechallenge_success INTEGER, database_id TEXT
    )")
  DBI::dbWriteTable(conn, "c_dechallenge_rechallenge", data.frame(
    cohort_id = c(1L, 2L),
    dechallenge_stop_interval = c(30L, 30L),
    dechallenge_evaluation_window = c(90L, 90L),
    num_cases = c(50L, 30L),
    num_dechallenge_attempt = c(35L, 20L),
    num_dechallenge_success = c(25L, 12L),
    num_rechallenge_attempt = c(10L, 8L),
    num_rechallenge_success = c(6L, 5L),
    database_id = rep("test_db", 2),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- CohortMethod: cm_result ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS cm_result (
      analysis_id INTEGER, target_id INTEGER, comparator_id INTEGER,
      outcome_id INTEGER, rr REAL, ci_95_lb REAL, ci_95_ub REAL,
      p REAL, log_rr REAL, se_log_rr REAL,
      target_subjects INTEGER, comparator_subjects INTEGER,
      target_outcomes INTEGER, comparator_outcomes INTEGER
    )")
  DBI::dbWriteTable(conn, "cm_result", data.frame(
    analysis_id = c(1L, 1L, 2L, 2L),
    target_id = rep(1L, 4),
    comparator_id = c(2L, 2L, 3L, 3L),
    outcome_id = c(10L, 20L, 10L, 20L),
    rr = c(0.85, 1.15, 0.72, 1.05),
    ci_95_lb = c(0.72, 0.95, 0.58, 0.88),
    ci_95_ub = c(1.01, 1.39, 0.89, 1.25),
    p = c(0.06, 0.15, 0.003, 0.60),
    log_rr = c(-0.163, 0.140, -0.329, 0.049),
    se_log_rr = c(0.086, 0.097, 0.110, 0.090),
    target_subjects = c(500L, 500L, 500L, 500L),
    comparator_subjects = c(480L, 480L, 450L, 450L),
    target_outcomes = c(45L, 60L, 35L, 55L),
    comparator_outcomes = c(52L, 53L, 48L, 52L),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- CohortMethod: cm_diagnostics_summary ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS cm_diagnostics_summary (
      analysis_id INTEGER, target_id INTEGER, comparator_id INTEGER,
      outcome_id INTEGER, max_sdm REAL, shared_max_sdm REAL,
      equipoise REAL, mdrr REAL, ease REAL
    )")
  DBI::dbWriteTable(conn, "cm_diagnostics_summary", data.frame(
    analysis_id = c(1L, 1L, 2L, 2L),
    target_id = rep(1L, 4),
    comparator_id = c(2L, 2L, 3L, 3L),
    outcome_id = c(10L, 20L, 10L, 20L),
    max_sdm = c(0.08, 0.10, 0.12, 0.06),
    shared_max_sdm = c(0.05, 0.07, 0.09, 0.04),
    equipoise = c(0.72, 0.68, 0.65, 0.75),
    mdrr = c(1.25, 1.30, 1.35, 1.20),
    ease = c(0.45, 0.50, 0.55, 0.40),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- SCCS: sccs_result ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS sccs_result (
      analysis_id INTEGER, exposures_outcome_set_id INTEGER,
      covariate_id INTEGER, rr REAL, ci_95_lb REAL, ci_95_ub REAL,
      p REAL, log_rr REAL, se_log_rr REAL,
      outcome_subjects INTEGER, outcome_events INTEGER
    )")
  DBI::dbWriteTable(conn, "sccs_result", data.frame(
    analysis_id = c(1L, 1L, 2L, 2L),
    exposures_outcome_set_id = c(1L, 2L, 1L, 2L),
    covariate_id = c(100L, 100L, 200L, 200L),
    rr = c(2.50, 1.85, 1.20, 3.10),
    ci_95_lb = c(1.80, 1.25, 0.90, 2.10),
    ci_95_ub = c(3.47, 2.74, 1.60, 4.57),
    p = c(0.001, 0.002, 0.22, 0.001),
    log_rr = c(0.916, 0.615, 0.182, 1.131),
    se_log_rr = c(0.167, 0.200, 0.147, 0.199),
    outcome_subjects = c(120L, 95L, 200L, 80L),
    outcome_events = c(180L, 130L, 250L, 110L),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- SCCS: sccs_diagnostics_summary ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS sccs_diagnostics_summary (
      analysis_id INTEGER, exposures_outcome_set_id INTEGER,
      covariate_id INTEGER, mdrr REAL, ease REAL,
      time_trend_p REAL, pre_exposure_p REAL
    )")
  DBI::dbWriteTable(conn, "sccs_diagnostics_summary", data.frame(
    analysis_id = c(1L, 1L, 2L, 2L),
    exposures_outcome_set_id = c(1L, 2L, 1L, 2L),
    covariate_id = c(100L, 100L, 200L, 200L),
    mdrr = c(1.50, 1.60, 1.40, 1.70),
    ease = c(0.35, 0.40, 0.30, 0.45),
    time_trend_p = c(0.45, 0.52, 0.60, 0.38),
    pre_exposure_p = c(0.80, 0.75, 0.85, 0.70),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- PLP: plp_performances ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS plp_performances (
      model_design_id INTEGER, development_database_id TEXT,
      validation_database_id TEXT, auc REAL, auprc REAL,
      population_size INTEGER, outcome_count INTEGER,
      calibration_in_large REAL, calibration_intercept REAL,
      calibration_slope REAL
    )")
  DBI::dbWriteTable(conn, "plp_performances", data.frame(
    model_design_id = c(1L, 2L, 3L, 1L),
    development_database_id = rep("test_db", 4),
    validation_database_id = c("test_db", "test_db", "test_db", "ext_db"),
    auc = c(0.82, 0.78, 0.85, 0.75),
    auprc = c(0.45, 0.38, 0.52, 0.35),
    population_size = c(1000L, 1000L, 800L, 500L),
    outcome_count = c(150L, 150L, 120L, 80L),
    calibration_in_large = c(1.02, 0.98, 1.05, 0.92),
    calibration_intercept = c(0.05, -0.08, 0.02, -0.15),
    calibration_slope = c(0.95, 0.90, 1.02, 0.82),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- Evidence Synthesis: es_cm_result ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS es_cm_result (
      analysis_id INTEGER, target_id INTEGER, comparator_id INTEGER,
      outcome_id INTEGER, rr REAL, ci_95_lb REAL, ci_95_ub REAL,
      p REAL, log_rr REAL, se_log_rr REAL, i2 REAL, n_databases INTEGER
    )")
  DBI::dbWriteTable(conn, "es_cm_result", data.frame(
    analysis_id = c(1L, 1L, 2L),
    target_id = rep(1L, 3),
    comparator_id = c(2L, 2L, 3L),
    outcome_id = c(10L, 20L, 10L),
    rr = c(0.80, 1.10, 0.75),
    ci_95_lb = c(0.70, 0.96, 0.63),
    ci_95_ub = c(0.91, 1.26, 0.89),
    p = c(0.001, 0.18, 0.001),
    log_rr = c(-0.223, 0.095, -0.288),
    se_log_rr = c(0.067, 0.070, 0.088),
    i2 = c(25.0, 15.0, 30.0),
    n_databases = c(5L, 5L, 5L),
    stringsAsFactors = FALSE
  ), append = TRUE)

  # --- Evidence Synthesis: es_sccs_result ---
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS es_sccs_result (
      analysis_id INTEGER, exposures_outcome_set_id INTEGER,
      covariate_id INTEGER, rr REAL, ci_95_lb REAL, ci_95_ub REAL,
      p REAL, log_rr REAL, se_log_rr REAL, i2 REAL, n_databases INTEGER
    )")
  DBI::dbWriteTable(conn, "es_sccs_result", data.frame(
    analysis_id = rep(1L, 2),
    exposures_outcome_set_id = c(1L, 2L),
    covariate_id = rep(100L, 2),
    rr = c(2.30, 1.75),
    ci_95_lb = c(1.75, 1.28),
    ci_95_ub = c(3.02, 2.39),
    p = c(0.001, 0.001),
    log_rr = c(0.833, 0.559),
    se_log_rr = c(0.140, 0.160),
    i2 = c(20.0, 18.0),
    n_databases = c(5L, 5L),
    stringsAsFactors = FALSE
  ), append = TRUE)
}
