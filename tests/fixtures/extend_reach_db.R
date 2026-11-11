# =============================================================================
# WS-0 (design-2.10.3 section 10.2): adversarial extension of the REACH fixture.
#
# extend_reach_handle(handle) appends deterministic rows for persons 1001..1030
# to the in-memory SQLite fixture built by build_reach_handle() (create_reach_db.R)
# and creates the `cost` table. Persons 1..100 are untouched, so every existing
# test keeps its counts; new tests and the 2.10.3 acceptance corpus
# (inst/plans/acceptance/) run on the EXTENDED fixture.
#
# Scenario map (index date 2020-01-01 unless stated; "cohort 1" = the reach
# fixture's all-persons cohort, extended here with the new persons):
#   1001  two overlapping observation periods covering the index (ambiguous coverage)
#   1002  two observation periods with exactly 7 uncovered days (2019-12-25..31)
#   1003  PDC fixture (cohort 1001, index 2019-12-31): observation [..2020-01-02] and
#         [2020-01-09..], one metformin fill 2020-01-01 (NULL end, supply 10)
#   1004  overlapping fills (cohort 1001): metformin 2020-01-01 and 2020-01-15, NULL
#         ends, supply 30 each (recorded coverage 44 days, effective supply 60)
#   1005  data-quality rows (cohort 1001): NULL end + NULL supply; end before start
#   1006  measurement normalisation: HbA1c in %, mmol/mol, NULL unit, operators < and >,
#         NULL value with a value concept
#   1007  same-day index events (two MI rows + an MI alternative code on 2020-01-01),
#         inpatient visit 2020-01-01..05 with a same-visit procedure and condition,
#         a procedure on another visit
#   1008  window boundaries: diabetes on 2019-01-01 (index-365), 2020-01-01 (index) and
#         2018-12-31 (outside)
#   1009  NULL year_of_birth;  1010  future year_of_birth (2030)
#   1011  death before index (2019-06-01) and an outcome after death
#   1012  matching case with a unique age band (yob 1934): zero eligible controls
#   1013  1014  covariate 4000777 present in exactly these two persons (2019-10-01)
#   1015  1016  oncology episodes (concept 32531) with episode_event links; one drug
#         exposure linked to two episodes; specimens; with_defects = TRUE adds a
#         member row whose person disagrees with the episode (refusal fixture)
#   1017  cost rows in USD (44818568) on a visit and a drug, one EUR (44818570) row
#   1018  two inpatient visits each with a respiratory-infection condition on the
#         visit; ICU visit_detail (32037) under the first visit
#   1019..1022  SCCS set (cohort 1002, index 2019-07-01): aspirin exposure across the
#         1 January boundary, MI outcomes before and after it
#   1023..1026  monthly weight panel: 1023 three weights, 1024 none, 1025 one weight
#         before the window only (carry-forward), 1026 two weights in one month
#   1027  recurrent respiratory infections (3 events); 1028 no outcome (sentinel)
#   1029  Circe import: diabetes 2019-03-01 with >= 365 prior observation
#   1030  Circe import: diabetes 2015-06-01 with < 365 prior observation
# Every new concept id is registered in `concept` so translation works.
# =============================================================================

extend_reach_handle <- function(handle, with_defects = FALSE) {
  conn <- handle$conn
  app <- function(name, df) DBI::dbWriteTable(conn, name, df, append = TRUE)
  d <- function(x) as.character(x)
  DM <- 201820L; MI <- 4329847L; MI2 <- 312327L; CHESTPAIN <- 77670L; RESP <- 320136L
  METF <- 1503297L; ASPIRIN <- 1124300L; CHEMO <- 1718850L; PCI <- 4283095L
  CARDIO_PROC <- 4044013L; A1C <- 3004410L; WT <- 3025315L; SPEC_TUMOR <- 4001225L
  VINP <- 9201L; VOUT <- 9202L; VICU_DETAIL <- 32037L
  U_PCT <- 8554L; U_MMOLMOL <- 8753L; U_KG <- 9529L; VAL_HIGH <- 4328749L
  OP_LT <- 4171756L; OP_GT <- 4172704L; OP_EQ <- 4172703L
  EPISODE <- 32531L; USD <- 44818568L; EUR <- 44818570L
  FIELD_DRUG <- 1147094L; FIELD_PROC <- 1147082L; COVAR2 <- 4000777L
  COND_TYPE <- 44818518L; DRUG_TYPE <- 38000177L; MEAS_TYPE <- 44818702L
  OP_TYPE <- 44814724L; VISIT_TYPE <- 44818518L; PROC_TYPE <- 44818518L
  TYPE_EHR <- 32817L; COST_TYPE <- 32817L; SPECTYPE <- 581378L

  pids <- 1001:1030
  yob <- c(1960L, 1961L, 1962L, 1963L, 1964L, 1965L, 1966L, 1967L, NA_integer_, 2030L,
           1968L, 1934L, 1970L, 1970L, 1955L, 1955L, 1972L, 1973L,
           1960L, 1961L, 1962L, 1963L, 1980L, 1981L, 1982L, 1983L, 1975L, 1976L, 1950L, 1951L)
  gender <- ifelse(pids %% 2L == 0L, 8532L, 8507L)
  app("person", data.frame(
    person_id = pids, gender_concept_id = gender, year_of_birth = yob,
    month_of_birth = ifelse(is.na(yob), NA_integer_, 6L),
    day_of_birth = ifelse(is.na(yob), NA_integer_, 15L),
    race_concept_id = 8527L, ethnicity_concept_id = 38003564L,
    location_id = pids, provider_id = pids, care_site_id = pids,
    person_source_value = paste0("EXT", pids),
    gender_source_value = ifelse(pids %% 2L == 0L, "F", "M"),
    stringsAsFactors = FALSE))

  # ---- observation periods ------------------------------------------------
  op <- function(id, pid, start, end) data.frame(
    observation_period_id = id, person_id = pid,
    observation_period_start_date = start, observation_period_end_date = end,
    period_type_concept_id = OP_TYPE, stringsAsFactors = FALSE)
  ops <- rbind(
    op(100101L, 1001L, "2015-01-01", "2020-06-30"), op(100102L, 1001L, "2020-01-01", "2030-12-31"),
    op(100201L, 1002L, "2015-01-01", "2019-12-24"), op(100202L, 1002L, "2020-01-01", "2030-12-31"),
    op(100301L, 1003L, "2015-01-01", "2020-01-02"), op(100302L, 1003L, "2020-01-09", "2030-12-31"),
    op(101901L, 1019L, "2019-01-01", "2020-12-31"), op(102001L, 1020L, "2019-01-01", "2020-12-31"),
    op(102101L, 1021L, "2019-01-01", "2020-12-31"), op(102201L, 1022L, "2019-01-01", "2020-12-31"),
    op(103001L, 1030L, "2015-01-01", "2030-12-31"))
  single <- setdiff(pids, c(1001L, 1002L, 1003L, 1019:1022, 1030L))
  ops <- rbind(ops, op(100000L + single, single, "2015-01-01", "2030-12-31"))
  app("observation_period", ops)

  # ---- cohorts --------------------------------------------------------------
  c1 <- c(1001L, 1002L, 1006:1014, 1015L, 1016L, 1017L, 1018L, 1023:1028, 1029L, 1030L)
  app("cohort", rbind(
    data.frame(cohort_definition_id = 1L, subject_id = c1, cohort_start_date = "2020-01-01",
               cohort_end_date = "2025-12-31", stringsAsFactors = FALSE),
    data.frame(cohort_definition_id = 1001L, subject_id = c(1003L, 1004L, 1005L),
               cohort_start_date = "2019-12-31", cohort_end_date = "2020-12-31", stringsAsFactors = FALSE),
    data.frame(cohort_definition_id = 1002L, subject_id = 1019:1022,
               cohort_start_date = "2019-07-01", cohort_end_date = "2020-06-30", stringsAsFactors = FALSE)))
  app("cohort_definition", data.frame(
    cohort_definition_id = c(1001L, 1002L),
    cohort_definition_name = c("ext_adherence", "ext_sccs"),
    cohort_definition_description = "reach extension cohort",
    definition_type_concept_id = 0L, cohort_definition_syntax = "",
    subject_concept_id = 0L, cohort_initiation_date = "2020-01-01", stringsAsFactors = FALSE))

  # ---- condition_occurrence --------------------------------------------------
  co_id <- 500000L
  co <- function(pid, concept, start, visit = NA_integer_, end = start) {
    co_id <<- co_id + 1L
    data.frame(condition_occurrence_id = co_id, person_id = pid, condition_concept_id = concept,
               condition_start_date = start, condition_end_date = end,
               condition_type_concept_id = COND_TYPE, stop_reason = NA_character_,
               provider_id = pid, visit_occurrence_id = visit,
               condition_source_value = paste0("C", concept), condition_source_concept_id = 0L,
               stringsAsFactors = FALSE)
  }
  conds <- rbind(
    co(1001L, DM, "2020-01-01"), co(1002L, DM, "2020-01-01"),
    co(1003L, DM, "2019-12-31"), co(1004L, DM, "2019-12-31"), co(1005L, DM, "2019-12-31"),
    co(1006L, DM, "2020-01-01"),
    co(1007L, MI, "2020-01-01", visit = 700701L), co(1007L, MI, "2020-01-01", visit = 700701L),
    co(1007L, MI2, "2020-01-01", visit = 700701L), co(1007L, CHESTPAIN, "2020-01-02", visit = 700701L),
    co(1008L, DM, "2019-01-01"), co(1008L, DM, "2020-01-01"), co(1008L, DM, "2018-12-31"),
    co(1009L, DM, "2020-01-01"), co(1010L, DM, "2020-01-01"),
    co(1011L, DM, "2020-01-01"), co(1011L, MI, "2020-02-01"),
    co(1012L, MI, "2019-07-01"), co(1012L, DM, "2019-01-15"),
    co(1013L, COVAR2, "2019-10-01"), co(1014L, COVAR2, "2019-10-01"),
    co(1013L, DM, "2019-10-01"), co(1014L, DM, "2019-10-01"),
    co(1018L, RESP, "2020-01-02", visit = 701801L), co(1018L, RESP, "2020-03-02", visit = 701802L),
    co(1019L, MI, "2019-12-20"), co(1019L, MI, "2020-01-20"),
    co(1020L, MI, "2020-02-10"), co(1021L, MI, "2019-10-05"), co(1022L, MI, "2020-05-15"),
    co(1027L, RESP, "2020-01-10"), co(1027L, RESP, "2020-01-20"), co(1027L, RESP, "2020-02-15"),
    co(1029L, DM, "2019-03-01"), co(1030L, DM, "2015-06-01"))
  app("condition_occurrence", conds)

  # ---- drug_exposure ----------------------------------------------------------
  de_id <- 500000L
  de <- function(pid, concept, start, end = NA_character_, supply = NA_integer_, visit = NA_integer_) {
    de_id <<- de_id + 1L
    data.frame(drug_exposure_id = de_id, person_id = pid, drug_concept_id = concept,
               drug_exposure_start_date = start, drug_exposure_end_date = end,
               drug_type_concept_id = DRUG_TYPE, stop_reason = NA_character_, refills = 0L,
               days_supply = supply, sig = NA_character_, route_concept_id = 0L,
               lot_number = NA_character_, dose_unit_concept_id = 0L,
               drug_source_value = paste0("D", concept), drug_source_concept_id = 0L,
               stringsAsFactors = FALSE)
  }
  drugs <- rbind(
    de(1003L, METF, "2020-01-01", supply = 10L),
    de(1004L, METF, "2020-01-01", supply = 30L), de(1004L, METF, "2020-01-15", supply = 30L),
    de(1005L, METF, "2020-02-01"),                                   # NULL end, NULL supply
    de(1005L, METF, "2020-03-10", end = "2020-03-01", supply = 30L), # end before start
    de(1015L, CHEMO, "2019-03-01", supply = 1L), de(1015L, CHEMO, "2019-03-22", supply = 1L),
    de(1015L, CHEMO, "2019-04-12", supply = 1L), de(1015L, CHEMO, "2019-10-01", supply = 1L),
    de(1016L, CHEMO, "2019-05-01", supply = 1L),
    de(1017L, METF, "2020-01-05", supply = 30L),
    de(1019L, ASPIRIN, "2019-11-15", supply = 93L),
    de(1020L, ASPIRIN, "2020-01-10", supply = 60L),
    de(1021L, ASPIRIN, "2019-09-01", supply = 30L), de(1021L, ASPIRIN, "2019-10-01", supply = 30L),
    de(1022L, ASPIRIN, "2020-04-01", supply = 90L))
  app("drug_exposure", drugs)
  # ids needed for episode_event links
  chemo_ids <- drugs$drug_exposure_id[drugs$person_id == 1015L]      # 4 fills in date order
  chemo_1016 <- drugs$drug_exposure_id[drugs$person_id == 1016L]
  metf_1017 <- drugs$drug_exposure_id[drugs$person_id == 1017L]

  # ---- measurement ----------------------------------------------------------
  me_id <- 500000L
  me <- function(pid, concept, date, value, unit = U_PCT, op = 0L, vac = 0L) {
    me_id <<- me_id + 1L
    data.frame(measurement_id = me_id, person_id = pid, measurement_concept_id = concept,
               measurement_date = date, measurement_type_concept_id = MEAS_TYPE,
               operator_concept_id = op, value_as_number = value, value_as_concept_id = vac,
               unit_concept_id = unit, range_low = NA_real_, range_high = NA_real_,
               measurement_source_value = paste0("M", concept), measurement_source_concept_id = 0L,
               unit_source_value = "u", stringsAsFactors = FALSE)
  }
  meas <- rbind(
    me(1006L, A1C, "2019-12-20", 52, unit = U_MMOLMOL),
    me(1006L, A1C, "2019-12-10", 7.5, unit = U_PCT),
    me(1006L, A1C, "2019-12-15", 6.0, unit = NA_integer_),
    me(1006L, A1C, "2019-12-01", 9.0, unit = U_PCT, op = OP_LT),
    me(1006L, A1C, "2019-11-20", 8.0, unit = U_PCT, op = OP_GT),
    me(1006L, A1C, "2019-12-28", NA_real_, unit = U_PCT, vac = VAL_HIGH),
    me(1009L, A1C, "2019-12-15", 7.0, unit = U_PCT, op = OP_EQ),
    me(1023L, WT, "2019-10-15", 80, unit = U_KG), me(1023L, WT, "2019-12-20", 82, unit = U_KG),
    me(1023L, WT, "2020-02-05", 85, unit = U_KG),
    me(1025L, WT, "2019-09-01", 70, unit = U_KG),
    me(1026L, WT, "2020-01-03", 90, unit = U_KG), me(1026L, WT, "2020-01-25", 91, unit = U_KG))
  app("measurement", meas)

  # ---- visits ---------------------------------------------------------------
  vo <- function(id, pid, concept, start, end) data.frame(
    visit_occurrence_id = id, person_id = pid, visit_concept_id = concept,
    visit_start_date = start, visit_end_date = end, visit_type_concept_id = VISIT_TYPE,
    visit_source_value = "V", visit_source_concept_id = 0L, stringsAsFactors = FALSE)
  app("visit_occurrence", rbind(
    vo(700701L, 1007L, VINP, "2020-01-01", "2020-01-05"),
    vo(700702L, 1007L, VOUT, "2020-02-01", "2020-02-01"),
    vo(701701L, 1017L, VOUT, "2020-01-10", "2020-01-10"),
    vo(701702L, 1017L, VOUT, "2020-02-01", "2020-02-01"),
    vo(701801L, 1018L, VINP, "2020-01-01", "2020-01-06"),
    vo(701802L, 1018L, VINP, "2020-03-01", "2020-03-04")))
  app("visit_detail", data.frame(
    visit_detail_id = 701801L, person_id = 1018L, visit_detail_concept_id = VICU_DETAIL,
    visit_detail_start_date = "2020-01-01", visit_detail_end_date = "2020-01-03",
    visit_occurrence_id = 701801L, parent_visit_detail_id = NA_integer_, stringsAsFactors = FALSE))

  # ---- procedures -------------------------------------------------------------
  po_id <- 500000L
  po <- function(pid, concept, date, visit = NA_integer_) {
    po_id <<- po_id + 1L
    data.frame(procedure_occurrence_id = po_id, person_id = pid, procedure_concept_id = concept,
               procedure_date = date, procedure_end_date = date, procedure_type_concept_id = PROC_TYPE,
               procedure_source_value = "P", procedure_source_concept_id = 0L,
               visit_occurrence_id = visit, stringsAsFactors = FALSE)
  }
  procs <- rbind(
    po(1007L, PCI, "2020-01-02", visit = 700701L), po(1007L, CARDIO_PROC, "2020-02-01", visit = 700702L),
    po(1015L, CARDIO_PROC, "2019-03-05"))
  if (!"visit_occurrence_id" %in% DBI::dbListFields(conn, "procedure_occurrence")) {
    DBI::dbExecute(conn, "ALTER TABLE procedure_occurrence ADD COLUMN visit_occurrence_id INTEGER")
  }
  app("procedure_occurrence", procs)
  proc_1015 <- procs$procedure_occurrence_id[procs$person_id == 1015L]

  # ---- specimens, death ---------------------------------------------------------
  app("specimen", data.frame(
    specimen_id = c(500001L, 500002L), person_id = 1015L, specimen_concept_id = SPEC_TUMOR,
    specimen_type_concept_id = SPECTYPE, specimen_date = c("2019-03-01", "2019-10-01"),
    quantity = 5, unit_concept_id = 0L, disease_status_concept_id = 0L,
    specimen_source_id = "S", specimen_source_value = "S", stringsAsFactors = FALSE))
  app("death", data.frame(
    person_id = 1011L, death_date = "2019-06-01", death_type_concept_id = TYPE_EHR,
    cause_concept_id = MI, cause_source_value = "I21", cause_source_concept_id = 0L,
    stringsAsFactors = FALSE))

  # ---- oncology episodes ----------------------------------------------------------
  app("episode", data.frame(
    episode_id = c(1L, 2L, 3L), person_id = c(1015L, 1015L, 1016L), episode_concept_id = EPISODE,
    episode_start_date = c("2019-03-01", "2019-10-01", "2019-05-01"),
    episode_end_date = c("2019-09-30", "2020-03-31", "2019-08-31"), stringsAsFactors = FALSE))
  links <- data.frame(
    episode_id = c(1L, 1L, 1L, 1L, 2L, 2L, 3L),
    event_id = c(chemo_ids[1:3], proc_1015, chemo_ids[4], chemo_ids[3], chemo_1016),
    episode_event_field_concept_id = c(FIELD_DRUG, FIELD_DRUG, FIELD_DRUG, FIELD_PROC,
                                       FIELD_DRUG, FIELD_DRUG, FIELD_DRUG),
    stringsAsFactors = FALSE)
  if (isTRUE(with_defects)) {
    # a member row whose person disagrees with the episode's person (1016 vs 1015)
    links <- rbind(links, data.frame(episode_id = 3L, event_id = chemo_ids[1],
                                     episode_event_field_concept_id = FIELD_DRUG))
  }
  app("episode_event", links)

  # ---- cost (CDM 5.4 columns) -------------------------------------------------------
  cost <- data.frame(
    cost_id = 1:5,
    cost_event_id = c(701701L, 701701L, metf_1017, 701702L, 700701L),
    cost_domain_id = c("Visit", "Visit", "Drug", "Visit", "Visit"),
    cost_type_concept_id = COST_TYPE,
    currency_concept_id = c(USD, USD, USD, EUR, USD),
    total_charge = c(150, 60, 25, 80, 1200), total_cost = c(100, 50, 20, 70, 1000),
    total_paid = c(90, 45, 18, 60, 900), paid_by_payer = NA_real_, paid_by_patient = NA_real_,
    paid_patient_copay = NA_real_, paid_patient_coinsurance = NA_real_,
    paid_patient_deductible = NA_real_, paid_by_primary = NA_real_,
    paid_ingredient_cost = NA_real_, paid_dispensing_fee = NA_real_,
    payer_plan_period_id = NA_integer_, amount_allowed = NA_real_,
    revenue_code_concept_id = 0L, revenue_code_source_value = NA_character_,
    drg_concept_id = 0L, drg_source_value = NA_character_, stringsAsFactors = FALSE)
  DBI::dbWriteTable(conn, "cost", cost, overwrite = TRUE)

  # ---- vocabulary rows for the new concepts -------------------------------------------
  new_concepts <- c(CHEMO, EPISODE, USD, EUR, OP_LT, OP_GT, FIELD_DRUG, FIELD_PROC, 1147127L, COVAR2)
  have <- DBI::dbGetQuery(conn, paste0("SELECT concept_id FROM concept WHERE concept_id IN (",
                                       paste(new_concepts, collapse = ","), ")"))$concept_id
  add <- setdiff(new_concepts, have)
  if (length(add) > 0L) {
    app("concept", data.frame(
      concept_id = add, concept_name = paste0("concept_", add),
      domain_id = "Condition", vocabulary_id = "SNOMED", concept_class_id = "Clinical Finding",
      standard_concept = "S", concept_code = as.character(add),
      valid_start_date = "1970-01-01", valid_end_date = "2099-12-31",
      invalid_reason = NA_character_, stringsAsFactors = FALSE))
    app("concept_ancestor", data.frame(
      ancestor_concept_id = add, descendant_concept_id = add,
      min_levels_of_separation = 0L, max_levels_of_separation = 0L))
  }
  handle$blueprint <- NULL   # the cost table and new columns must be rediscovered
  invisible(handle)
}

# Convenience: a fresh extended handle (used by tests and the acceptance corpus).
build_extended_reach_handle <- function(n_persons = 100L, with_defects = FALSE) {
  h <- build_reach_handle(n_persons)
  extend_reach_handle(h, with_defects = with_defects)
}
