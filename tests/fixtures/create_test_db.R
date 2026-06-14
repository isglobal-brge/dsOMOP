create_test_omop_db <- function(n_persons = 15) {
  .create_test_omop_db(version = "5.4", n_persons = n_persons)
}

create_test_omop_db_v53 <- function(n_persons = 15) {
  .create_test_omop_db(version = "5.3", n_persons = n_persons)
}

.create_test_omop_db <- function(version = "5.4", n_persons = 15) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  n_persons <- as.integer(n_persons)

  write <- function(name, df) {
    DBI::dbWriteTable(conn, name, df, overwrite = TRUE)
  }

  persons <- seq_len(n_persons)
  cohort_members <- intersect(c(1L, 3L, 5L, 7L, 9L, 11L), persons)

  write("person", data.frame(
    person_id = persons,
    gender_concept_id = ifelse(persons %% 2L == 0L, 8532L, 8507L),
    year_of_birth = 1940L + persons,
    month_of_birth = rep(1L, n_persons),
    day_of_birth = rep(1L, n_persons),
    race_concept_id = rep(8527L, n_persons),
    ethnicity_concept_id = rep(38003564L, n_persons),
    location_id = persons,
    provider_id = persons,
    care_site_id = persons,
    person_source_value = paste0("SRC", persons),
    gender_source_value = ifelse(persons %% 2L == 0L, "F", "M"),
    stringsAsFactors = FALSE
  ))

  write("observation_period", data.frame(
    observation_period_id = persons,
    person_id = persons,
    observation_period_start_date = rep("2018-01-01", n_persons),
    observation_period_end_date = rep("2030-12-31", n_persons),
    period_type_concept_id = rep(44814724L, n_persons),
    stringsAsFactors = FALSE
  ))

  write("cohort", data.frame(
    cohort_definition_id = rep(1L, length(cohort_members)),
    subject_id = cohort_members,
    cohort_start_date = rep("2020-01-01", length(cohort_members)),
    cohort_end_date = rep("2022-12-31", length(cohort_members)),
    stringsAsFactors = FALSE
  ))

  write("cohort_definition", data.frame(
    cohort_definition_id = 1L,
    cohort_definition_name = "Test cohort",
    cohort_definition_description = "Synthetic cohort for dsOMOP tests",
    definition_type_concept_id = 0L,
    cohort_definition_syntax = "",
    subject_concept_id = 0L,
    cohort_initiation_date = "2020-01-01",
    stringsAsFactors = FALSE
  ))

  .write_vocabulary_tables(conn, write)
  .write_clinical_event_tables(write, persons, version)
  .write_achilles_tables(write, n_persons)
  .write_ohdsi_result_tables(write)

  write("cdm_source", data.frame(
    cdm_source_name = "dsOMOP Test",
    cdm_source_abbreviation = "DSOT",
    cdm_holder = "ISGlobal",
    source_description = "Synthetic OMOP CDM fixture for tests",
    cdm_version = paste0("v", version),
    vocabulary_version = "v5.0",
    stringsAsFactors = FALSE
  ))

  if (identical(version, "5.4")) {
    write("episode", data.frame(
      episode_id = integer(0),
      person_id = integer(0),
      episode_concept_id = integer(0),
      episode_start_date = character(0),
      episode_end_date = character(0),
      stringsAsFactors = FALSE
    ))
    write("episode_event", data.frame(
      episode_id = integer(0),
      event_id = integer(0),
      episode_event_field_concept_id = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  conn
}

.write_vocabulary_tables <- function(conn, write) {
  concept_rows <- data.frame(
    concept_id = c(
      8507L, 8532L, 8527L, 38003564L, 44814724L, 44818518L,
      9202L, 201820L, 255573L, 317009L, 3004410L, 3025315L,
      1124300L, 4000000L, 4000001L, 4000002L,
      900001L, 900002L, 900003L, 900004L, 900005L, 900006L,
      910001L, 910002L, 910003L,
      800001L, 800002L, 800003L, 800004L, 800005L, 800006L,
      810001L, 810002L, 810003L
    ),
    concept_name = c(
      "male", "female", "White", "Not Hispanic or Latino",
      "Period while enrolled in insurance", "EHR encounter record",
      "Outpatient Visit", "Diabetes mellitus", "COPD", "Asthma",
      "Hemoglobin A1c", "Creatinine measurement", "Aspirin",
      "Metabolic disease", "Respiratory disease", "Myocardial infarction",
      "Diabetic retinopathy", "Chronic kidney disease",
      "Diabetic neuropathy", "Transient ischemic attack",
      "Acute myocardial infarction", "Diabetic ketoacidosis",
      "Delirium", "Alzheimer dementia", "Hemiplegia",
      "ICD9 retinopathy", "ICD9 chronic kidney disease",
      "ICD9 neuropathy", "ICD9 TIA", "ICD9 myocardial infarction",
      "ICD9 ketoacidosis", "ICD10 delirium", "ICD10 dementia",
      "ICD10 hemiplegia"
    ),
    domain_id = c(
      "Gender", "Gender", "Race", "Ethnicity", "Type Concept",
      "Type Concept", "Visit", "Condition", "Condition", "Condition",
      "Measurement", "Measurement", "Drug", "Condition", "Condition",
      "Condition", rep("Condition", 15), rep("Condition", 3)
    ),
    vocabulary_id = c(
      rep("Gender", 2), "Race", "Ethnicity", rep("Type Concept", 2),
      "Visit", rep("SNOMED", 18), rep("ICD9CM", 6), rep("ICD10CM", 3)
    ),
    concept_class_id = c(
      rep("Concept", 16), rep("Clinical Finding", 9),
      rep("ICD9CM", 6), rep("ICD10CM", 3)
    ),
    standard_concept = c(
      rep("S", 25), rep(NA_character_, 9)
    ),
    concept_code = c(
      "M", "F", "8527", "38003564", "44814724", "44818518",
      "9202", "201820", "255573", "317009", "3004410", "3025315",
      "1124300", "4000000", "4000001", "4000002",
      "900001", "900002", "900003", "900004", "900005", "900006",
      "910001", "910002", "910003",
      "250.50", "585.1", "250.60", "435.0", "410.0", "250.10",
      "F05", "F00", "G81"
    ),
    valid_start_date = rep("1970-01-01", 34),
    valid_end_date = rep("2099-12-31", 34),
    invalid_reason = rep(NA_character_, 34),
    stringsAsFactors = FALSE
  )
  write("concept", concept_rows)

  ancestor_pairs <- data.frame(
    ancestor_concept_id = c(
      201820L, 255573L, 317009L, 3004410L, 3025315L, 4000002L,
      4000000L, 4000000L, 4000001L, 4000001L, 4000001L
    ),
    descendant_concept_id = c(
      201820L, 255573L, 317009L, 3004410L, 3025315L, 4000002L,
      4000000L, 201820L, 4000001L, 255573L, 317009L
    ),
    min_levels_of_separation = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 1L),
    max_levels_of_separation = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 1L)
  )
  write("concept_ancestor", ancestor_pairs)

  relationships <- data.frame(
    concept_id_1 = c(800001L, 800002L, 800003L, 800004L, 800005L, 800006L,
                     810001L, 810002L, 810003L),
    concept_id_2 = c(900001L, 900002L, 900003L, 900004L, 900005L, 900006L,
                     910001L, 910002L, 910003L),
    relationship_id = rep("Maps to", 9),
    valid_start_date = rep("1970-01-01", 9),
    valid_end_date = rep("2099-12-31", 9),
    invalid_reason = rep(NA_character_, 9),
    stringsAsFactors = FALSE
  )
  write("concept_relationship", relationships)

  write("domain", data.frame(
    domain_id = unique(concept_rows$domain_id),
    domain_name = unique(concept_rows$domain_id),
    domain_concept_id = seq_along(unique(concept_rows$domain_id)),
    stringsAsFactors = FALSE
  ))

  write("vocabulary", data.frame(
    vocabulary_id = unique(concept_rows$vocabulary_id),
    vocabulary_name = unique(concept_rows$vocabulary_id),
    vocabulary_reference = "",
    vocabulary_version = "test",
    vocabulary_concept_id = seq_along(unique(concept_rows$vocabulary_id)),
    stringsAsFactors = FALSE
  ))

  DBI::dbExecute(conn, "CREATE TABLE concept_class (
    concept_class_id TEXT,
    concept_class_name TEXT,
    concept_class_concept_id INTEGER
  )")
}

.write_clinical_event_tables <- function(write, persons, version) {
  keep <- function(ids) ids[ids %in% persons]
  row_id <- 1L
  condition_rows <- list()
  add_condition <- function(pid, concept_id, start, end = start) {
    condition_rows[[length(condition_rows) + 1L]] <<- data.frame(
      condition_occurrence_id = row_id,
      person_id = pid,
      condition_concept_id = concept_id,
      condition_start_date = start,
      condition_end_date = end,
      condition_type_concept_id = 44818518L,
      stop_reason = NA_character_,
      provider_id = pid,
      visit_occurrence_id = pid,
      condition_source_value = paste0("COND", concept_id),
      condition_source_concept_id = 0L,
      stringsAsFactors = FALSE
    )
    row_id <<- row_id + 1L
  }

  for (pid in keep(c(1L, 3L, 5L, 7L, 9L, 11L))) {
    add_condition(pid, 201820L, "2019-06-01")
  }
  for (pid in keep(c(1L, 3L, 5L))) {
    add_condition(pid, 255573L, "2019-08-01")
  }
  for (pid in keep(c(2L, 4L, 6L))) {
    add_condition(pid, 317009L, "2019-09-01")
  }
  for (pid in keep(c(1L, 5L, 9L))) {
    add_condition(pid, 4000002L, "2020-06-01")
  }
  if (1L %in% persons) {
    add_condition(1L, 900001L, "2019-02-01")
    add_condition(1L, 900002L, "2019-03-01")
  }
  if (5L %in% persons) {
    add_condition(5L, 900003L, "2019-02-01")
    add_condition(5L, 900004L, "2019-03-01")
  }
  if (7L %in% persons) {
    add_condition(7L, 900005L, "2019-02-01")
    add_condition(7L, 900006L, "2019-03-01")
  }
  if (3L %in% persons) add_condition(3L, 910001L, "2019-02-01")
  if (9L %in% persons) {
    add_condition(9L, 910002L, "2019-02-01")
    add_condition(9L, 910003L, "2019-03-01")
  }

  condition_df <- if (length(condition_rows) > 0) {
    do.call(rbind, condition_rows)
  } else {
    data.frame(
      condition_occurrence_id = integer(0),
      person_id = integer(0),
      condition_concept_id = integer(0),
      condition_start_date = character(0),
      condition_end_date = character(0),
      condition_type_concept_id = integer(0),
      stop_reason = character(0),
      provider_id = integer(0),
      visit_occurrence_id = integer(0),
      condition_source_value = character(0),
      condition_source_concept_id = integer(0),
      stringsAsFactors = FALSE
    )
  }
  write("condition_occurrence", condition_df)

  meas_persons <- keep(c(1L, 3L, 5L, 7L, 9L, 11L))
  meas <- data.frame(
    measurement_id = seq_along(meas_persons),
    person_id = meas_persons,
    measurement_concept_id = rep(3004410L, length(meas_persons)),
    measurement_date = rep("2019-12-15", length(meas_persons)),
    measurement_type_concept_id = rep(44818702L, length(meas_persons)),
    value_as_number = seq(6.1, by = 0.2, length.out = length(meas_persons)),
    value_as_concept_id = rep(0L, length(meas_persons)),
    unit_concept_id = rep(8840L, length(meas_persons)),
    range_low = rep(4.0, length(meas_persons)),
    range_high = rep(6.0, length(meas_persons)),
    measurement_source_value = rep("A1C", length(meas_persons)),
    measurement_source_concept_id = rep(0L, length(meas_persons)),
    stringsAsFactors = FALSE
  )
  if (length(meas_persons) > 0) {
    extra_meas <- data.frame(
      measurement_id = length(meas_persons) + seq_along(meas_persons),
      person_id = meas_persons,
      measurement_concept_id = rep(3025315L, length(meas_persons)),
      measurement_date = rep("2019-11-15", length(meas_persons)),
      measurement_type_concept_id = rep(44818702L, length(meas_persons)),
      value_as_number = seq(1.0, by = 0.1, length.out = length(meas_persons)),
      value_as_concept_id = rep(0L, length(meas_persons)),
      unit_concept_id = rep(8840L, length(meas_persons)),
      range_low = rep(0.5, length(meas_persons)),
      range_high = rep(1.5, length(meas_persons)),
      measurement_source_value = rep("CREAT", length(meas_persons)),
      measurement_source_concept_id = rep(0L, length(meas_persons)),
      stringsAsFactors = FALSE
    )
    meas <- rbind(meas, extra_meas)
  }
  write("measurement", meas)

  write("observation", data.frame(
    observation_id = persons,
    person_id = persons,
    observation_concept_id = rep(100000L, length(persons)),
    observation_date = rep("2019-10-01", length(persons)),
    observation_type_concept_id = rep(44814721L, length(persons)),
    value_as_number = rep(1, length(persons)),
    value_as_string = paste0("free text ", persons),
    value_as_concept_id = rep(0L, length(persons)),
    observation_source_value = paste0("OBS", persons),
    observation_source_concept_id = rep(0L, length(persons)),
    stringsAsFactors = FALSE
  ))

  write("drug_exposure", data.frame(
    drug_exposure_id = persons,
    person_id = persons,
    drug_concept_id = rep(1124300L, length(persons)),
    drug_exposure_start_date = rep("2019-07-01", length(persons)),
    drug_exposure_end_date = rep("2019-07-31", length(persons)),
    drug_type_concept_id = rep(38000177L, length(persons)),
    stop_reason = NA_character_,
    sig = paste0("take ", persons),
    drug_source_value = paste0("DRUG", persons),
    drug_source_concept_id = rep(0L, length(persons)),
    stringsAsFactors = FALSE
  ))

  write("visit_occurrence", data.frame(
    visit_occurrence_id = persons,
    person_id = persons,
    visit_concept_id = rep(9202L, length(persons)),
    visit_start_date = rep("2019-05-01", length(persons)),
    visit_end_date = rep("2019-05-02", length(persons)),
    visit_type_concept_id = rep(44818518L, length(persons)),
    visit_source_value = paste0("VIS", persons),
    visit_source_concept_id = rep(0L, length(persons)),
    stringsAsFactors = FALSE
  ))

  procedure_cols <- data.frame(
    procedure_occurrence_id = persons,
    person_id = persons,
    procedure_concept_id = rep(2213316L, length(persons)),
    procedure_date = rep("2019-04-01", length(persons)),
    procedure_type_concept_id = rep(44818518L, length(persons)),
    procedure_source_value = paste0("PROC", persons),
    procedure_source_concept_id = rep(0L, length(persons)),
    stringsAsFactors = FALSE
  )
  if (identical(version, "5.4")) {
    procedure_cols$procedure_end_date <- rep("2019-04-01", length(persons))
  }
  write("procedure_occurrence", procedure_cols)

  # death: a subset of persons die. CDM 5.4 spells the domain concept
  # cause_concept_id (there is NO death_concept_id); used to exercise the
  # death auto-detect fix and death prevalence/value-counts/quantiles.
  death_persons <- keep(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L))
  if (length(death_persons) > 0) {
    write("death", data.frame(
      person_id = death_persons,
      death_date = rep("2021-03-15", length(death_persons)),
      death_type_concept_id = rep(32817L, length(death_persons)),
      cause_concept_id = rep(4000002L, length(death_persons)),
      cause_source_value = rep("I21", length(death_persons)),
      cause_source_concept_id = rep(0L, length(death_persons)),
      stringsAsFactors = FALSE
    ))
  }

  if (identical(version, "5.4")) {
    write("location", data.frame(
      location_id = persons,
      address_1 = paste0("street ", persons),
      city = "Barcelona",
      state = "CT",
      zip = "08000",
      county = "Barcelona",
      latitude = rep(41.38, length(persons)),
      longitude = rep(2.17, length(persons)),
      stringsAsFactors = FALSE
    ))
    write("visit_detail", data.frame(
      visit_detail_id = persons,
      person_id = persons,
      visit_detail_concept_id = rep(9202L, length(persons)),
      visit_detail_start_date = rep("2019-05-01", length(persons)),
      visit_detail_end_date = rep("2019-05-02", length(persons)),
      parent_visit_detail_id = NA_integer_,
      stringsAsFactors = FALSE
    ))
  } else {
    write("location", data.frame(
      location_id = persons,
      address_1 = paste0("street ", persons),
      city = "Barcelona",
      state = "CT",
      zip = "08000",
      county = "Barcelona",
      stringsAsFactors = FALSE
    ))
    write("visit_detail", data.frame(
      visit_detail_id = persons,
      person_id = persons,
      visit_detail_concept_id = rep(9202L, length(persons)),
      visit_detail_start_date = rep("2019-05-01", length(persons)),
      visit_detail_end_date = rep("2019-05-02", length(persons)),
      visit_detail_parent_id = NA_integer_,
      stringsAsFactors = FALSE
    ))
  }
}

.write_achilles_tables <- function(write, n_persons) {
  write("achilles_analysis", data.frame(
    analysis_id = c(1L, 2L, 103L, 400L, 700L),
    analysis_name = c("Number of persons", "Gender distribution",
                      "Age at first observation", "Condition persons",
                      "Drug persons"),
    stratum_1_name = c(NA, "gender_concept_id", NA, "concept_id", "concept_id"),
    stratum_2_name = NA_character_,
    stratum_3_name = NA_character_,
    stratum_4_name = NA_character_,
    stratum_5_name = NA_character_,
    stringsAsFactors = FALSE
  ))

  male_n <- ceiling(n_persons / 2)
  female_n <- floor(n_persons / 2)
  write("achilles_results", data.frame(
    analysis_id = c(1L, 2L, 2L, 400L, 700L, 700L),
    stratum_1 = c(NA, "8507", "8532", "201820", "1154029", "1124300"),
    stratum_2 = NA_character_,
    stratum_3 = NA_character_,
    stratum_4 = NA_character_,
    stratum_5 = NA_character_,
    count_value = c(n_persons, male_n, female_n, 6, 2, 6),
    stringsAsFactors = FALSE
  ))

  write("achilles_results_dist", data.frame(
    analysis_id = c(3L, 103L),
    stratum_1 = c("year_of_birth", "age"),
    stratum_2 = NA_character_,
    stratum_3 = NA_character_,
    stratum_4 = NA_character_,
    stratum_5 = NA_character_,
    count_value = c(n_persons, n_persons),
    min_value = c(1941, 50),
    max_value = c(1955, 90),
    avg_value = c(1948, 70),
    stdev_value = c(4, 10),
    median_value = c(1948, 70),
    p10_value = c(1942, 55),
    p25_value = c(1944, 60),
    p75_value = c(1952, 80),
    p90_value = c(1954, 85),
    stringsAsFactors = FALSE
  ))

  write("achilles_heel_results", data.frame(
    analysis_id = c(1L, 2L, 400L),
    achilles_heel_warning = c("Warning A", "Warning B", "Warning C"),
    rule_id = c(101L, 102L, 103L),
    record_count = c(5L, 6L, 7L),
    stringsAsFactors = FALSE
  ))
}

.write_ohdsi_result_tables <- function(write) {
  write("dqdashboard_results", data.frame(
    check_name = paste0("check_", 1:8),
    category = c("Completeness", "Completeness", "Completeness",
                 "Conformance", "Conformance", "Plausibility",
                 "Plausibility", "Completeness"),
    num_violated_rows = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L),
    num_denominator_rows = rep(15L, 8),
    query_text = paste0("SELECT ", 1:8),
    stringsAsFactors = FALSE
  ))

  write("cohort_count", data.frame(
    cohort_id = c(1L, 2L),
    cohort_name = c("Main", "Small"),
    cohort_entries = c(6L, 2L),
    cohort_subjects = c(6L, 2L),
    stringsAsFactors = FALSE
  ))

  write("incidence_rate", data.frame(
    cohort_id = c(1L, 3L),
    outcome_id = c(10L, 11L),
    cohort_count = c(4L, 8L),
    person_years = c(30, 40),
    incidence_rate = c(0.1, 0.2),
    stringsAsFactors = FALSE
  ))

  write("incidence_summary", data.frame(
    target_id = c(1L, 1L),
    outcome_id = c(10L, 20L),
    persons_at_risk = c(6L, 6L),
    person_outcomes = c(4L, 1L),
    outcomes = c(4L, 1L),
    stringsAsFactors = FALSE
  ))

  write("c_cohort_counts", data.frame(
    cohort_id = c(1L, 2L),
    num_persons = c(6L, 1L),
    num_records = c(9L, 1L),
    stringsAsFactors = FALSE
  ))

  write("c_covariates", data.frame(
    cohort_id = c(1L, 1L),
    covariate_id = c(201820001, 255573001),
    covariate_name = c("Diabetes", "COPD"),
    num_persons = c(6L, 3L),
    sum_value = c(6L, 3L),
    count_value = c(6L, 3L),
    stringsAsFactors = FALSE
  ))

  write("index_event_breakdown", data.frame(
    cohort_id = 1L, concept_id = 201820L,
    concept_name = "Diabetes mellitus", subjects = 6L, records = 6L,
    stringsAsFactors = FALSE
  ))
  write("visit_context", data.frame(
    cohort_id = 1L, visit_concept_id = 9202L, subjects = 6L,
    stringsAsFactors = FALSE
  ))
  write("temporal_covariate_value", data.frame(
    time_id = 1L, covariate_id = 201820001, mean = 0.5, subjects = 6L,
    stringsAsFactors = FALSE
  ))
  write("time_series", data.frame(
    calendar_year = 2020L, subjects = 6L, count_value = 6L,
    stringsAsFactors = FALSE
  ))
  write("included_source_concept", data.frame(
    concept_id = 201820L, concept_name = "Diabetes mellitus",
    concept_count = 6L, concept_subjects = 6L,
    stringsAsFactors = FALSE
  ))
  write("orphan_concept", data.frame(
    concept_id = 999L, concept_name = "Orphan", concept_count = 6L,
    stringsAsFactors = FALSE
  ))
  write("c_covariates_continuous", data.frame(
    covariate_id = 3004410003, covariate_name = "HbA1c mean",
    median_value = 6.5, p25_value = 6.1, p75_value = 6.9,
    num_persons = 6L,
    stringsAsFactors = FALSE
  ))
  write("c_time_to_event", data.frame(
    cohort_id = 1L, time_value = 365L, value = 0.25, num_persons = 6L,
    stringsAsFactors = FALSE
  ))
  write("c_dechallenge_rechallenge", data.frame(
    cohort_id = 1L, num_cases = 6L, num_dechallenge_attempt = 4L,
    num_dechallenge_success = 3L, num_rechallenge_attempt = 3L,
    num_rechallenge_success = 3L,
    stringsAsFactors = FALSE
  ))

  write("cm_result", data.frame(
    analysis_id = 1L, target_subjects = 6L, comparator_subjects = 6L,
    target_outcomes = 4L, comparator_outcomes = 3L,
    rr = 1.2, ci_95_lb = 0.8, ci_95_ub = 1.8,
    stringsAsFactors = FALSE
  ))
  write("cm_diagnostics_summary", data.frame(
    analysis_id = 1L, target_subjects = 6L, comparator_subjects = 6L,
    subjects = 6L,
    stringsAsFactors = FALSE
  ))
  write("sccs_result", data.frame(
    analysis_id = 1L, outcome_subjects = 6L, outcome_events = 6L,
    rr = 1.1, ci_95_lb = 0.9, ci_95_ub = 1.4,
    stringsAsFactors = FALSE
  ))
  write("sccs_diagnostics_summary", data.frame(
    analysis_id = 1L, subjects = 6L, count_value = 6L,
    stringsAsFactors = FALSE
  ))
  write("plp_performances", data.frame(
    analysis_id = 1L, population_size = 6L, outcome_count = 3L,
    auc = 0.71,
    stringsAsFactors = FALSE
  ))
  write("es_cm_result", data.frame(
    analysis_id = 1L, n_databases = 3L, rr = 1.15, ci_95_lb = 0.9,
    ci_95_ub = 1.5,
    stringsAsFactors = FALSE
  ))
  write("es_sccs_result", data.frame(
    analysis_id = 1L, n_databases = 3L, rr = 1.05, ci_95_lb = 0.8,
    ci_95_ub = 1.3,
    stringsAsFactors = FALSE
  ))
}
