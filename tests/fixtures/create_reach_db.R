# =============================================================================
# Comprehensive OMOP-CDM SQLite REACH FIXTURE for Phase 7b limit-test
# Builds ALL clinical + era + vocab tables, seeded with the corpus concept_ids
# and ~100 synthetic persons exhibiting the set-op / temporal / value patterns.
# Disclosure gate is meant to be turned OFF by the caller (options(nfilter.subset=0)).
# Returns a dsOMOP "handle" (env wrapping a SQLite conn), ready for .planExecute.
# =============================================================================

build_reach_handle <- function(n_persons = 100L) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  n_persons <- as.integer(n_persons)
  persons <- seq_len(n_persons)
  write <- function(name, df) DBI::dbWriteTable(conn, name, df, overwrite = TRUE)

  # ---- Concept IDs from the corpus -----------------------------------------
  # conditions
  DM <- 201820L; PRE <- 201826L; HTN1 <- 320128L; HTN2 <- 316866L
  MI <- 4329847L; MI2 <- 312327L; CKD <- 46271022L; AF <- 313217L
  HF <- 316139L; HLD <- 432867L; OBES <- 433736L; PREG <- 4299535L
  STROKE <- 381316L; STROKE2 <- 372924L; SEPSIS <- 132797L; CIRR <- 4064161L
  PAINc <- 436096L; ASTHMA <- 317009L; COPD <- 255573L; CHESTPAIN <- 77670L
  RESP <- 320136L; PRIOR_BLEED <- 192671L; MAJOR_BLEED <- 4108832L
  MALIG <- 4112853L; CHRONICPAIN <- 436096L; HTNCOMP <- 321318L
  # drugs
  METF <- 1503297L; INS <- 1596977L; STATIN <- 1539403L; WARF <- 1310149L
  ACEI <- 1308216L; ARB <- 1367500L; BB <- 1346823L; DIUR <- 974166L
  DOAC <- 40228152L; SGLT2 <- 1488000L; SPIRO <- 961047L; LOOPD <- 956874L
  OPIOID <- 1124957L; OXY <- 1124957L; SABA <- 1154343L; PRED <- 1551099L
  VASO1 <- 1343916L; VASO2 <- 1326303L; PROPOFOL <- 19078318L; ASPIRIN <- 1124300L
  # measurements
  A1C <- 3004410L; EGFR <- 3049187L; EGFR2 <- 3029829L; LDL <- 3028288L
  LDL2 <- 3028437L; SBP <- 3004249L; CREAT <- 3016723L; GLUC <- 3004501L
  K <- 3023103L; NA_ <- 3019550L; CA <- 3006906L; TROP <- 3033745L
  INR <- 3032080L; BNP <- 3031569L; LACT <- 3047181L; CHOL <- 3027114L
  ALB <- 3024561L; KI67 <- 44793318L; SIRS1 <- 3020891L; SIRS2 <- 3027018L
  SERO1 <- 3020416L; SERO2 <- 3019198L; WT <- 3025315L; PAINSCORE <- 4291306L
  HEMO <- 3020138L; DRUGCONC <- 3024561L
  # procedures/devices/specimens/observations
  DIALYSIS <- 4032243L; PCI <- 4283095L; BARIATRIC <- 4322471L; CATH <- 4139525L
  CARDIO_PROC <- 4044013L; VENT <- 4097216L; STENT <- 4038664L
  CATHETER <- 4145529L; DEV1 <- 4206863L; DEV2 <- 4145806L
  SPEC_BLOOD <- 4046221L; SPEC_TUMOR <- 4001225L; SPEC2 <- 4122249L
  SMOKING <- 4275495L; FRAILTY <- 4275495L
  # visits
  VOUT <- 9202L; VINP <- 9201L; VER <- 9203L; VICU_DETAIL <- 32037L
  # units / types
  U_PCT <- 8554L; U_MMOLMOL <- 8753L; U_MGDL <- 8840L; U_KG <- 9529L
  U_LB <- 8739L; U_INSULIN <- 8554L
  TYPE_EHR <- 32817L; TYPE_CLAIMS <- 32810L
  VAL_HIGH <- 4328749L; VAL_POS <- 9191L; VAL_REACTIVE <- 4126681L
  VAL_NEG <- 45877985L; OP_EQ <- 4172703L
  DRUGTYPE_PHARM <- 38000175L; DEVTYPE <- 32856L
  SPECTYPE <- 581378L; DISEASE_STATUS <- 4204311L
  COND_PROBLIST <- 32840L; SMOKE_VAL <- 45877994L

  # =========================================================================
  # PERSON  (ages chosen so age filters bite: born 1935..1990 cycling)
  # =========================================================================
  yob <- 1990L - ((persons * 7L) %% 56L)   # spread 1934..1990
  gender <- ifelse(persons %% 2L == 0L, 8532L, 8507L)  # even=F, odd=M
  write("person", data.frame(
    person_id = persons, gender_concept_id = gender, year_of_birth = yob,
    month_of_birth = rep(6L, n_persons), day_of_birth = rep(15L, n_persons),
    race_concept_id = rep(8527L, n_persons),
    ethnicity_concept_id = rep(38003564L, n_persons),
    location_id = persons, provider_id = persons, care_site_id = persons,
    person_source_value = paste0("SRC", persons),
    gender_source_value = ifelse(persons %% 2L == 0L, "F", "M"),
    stringsAsFactors = FALSE))

  write("observation_period", data.frame(
    observation_period_id = persons, person_id = persons,
    observation_period_start_date = rep("2015-01-01", n_persons),
    observation_period_end_date = rep("2030-12-31", n_persons),
    period_type_concept_id = rep(44814724L, n_persons),
    stringsAsFactors = FALSE))

  # =========================================================================
  # COHORTS — several cohort_definition_ids referenced by scoping variants
  # cohort 1: all; 3,5,7,9,11,17,42: assorted; 200/201 for A32; 101/202 for A10
  # =========================================================================
  mk_cohort <- function(cid, members) data.frame(
    cohort_definition_id = rep(cid, length(members)), subject_id = members,
    cohort_start_date = rep("2020-01-01", length(members)),
    cohort_end_date = rep("2025-12-31", length(members)), stringsAsFactors = FALSE)
  ev <- persons[persons %% 2L == 0L]
  cohorts <- rbind(
    mk_cohort(1L, persons),
    mk_cohort(2L, persons[1:60]),
    mk_cohort(3L, persons[1:70]),
    mk_cohort(4L, persons[seq(1, n_persons, 2)]),
    mk_cohort(5L, persons[1:50]),
    mk_cohort(6L, persons[1:55]),
    mk_cohort(7L, persons[1:65]),
    mk_cohort(8L, persons[10:80]),
    mk_cohort(9L, persons[1:75]),
    mk_cohort(10L, persons[1:40]),
    mk_cohort(11L, persons[1:80]),
    mk_cohort(12L, persons[1:60]),
    mk_cohort(13L, persons[1:70]),
    mk_cohort(14L, persons[1:50]),
    mk_cohort(17L, persons[1:90]),
    mk_cohort(21L, persons[1:30]),
    mk_cohort(30L, persons[1:85]),
    mk_cohort(42L, persons[1:88]),
    mk_cohort(50L, persons[1:60]),
    mk_cohort(55L, persons[1:70]),
    mk_cohort(88L, persons[1:80]),
    mk_cohort(99L, persons[1:95]),
    mk_cohort(101L, persons[1:30]),
    mk_cohort(202L, persons[20:55]),
    mk_cohort(200L, persons[1:50]),
    mk_cohort(201L, persons[40:90]),
    mk_cohort(313217L, persons[1:60]),
    mk_cohort(1820L, persons[1:70]))
  write("cohort", cohorts)
  write("cohort_definition", data.frame(
    cohort_definition_id = unique(cohorts$cohort_definition_id),
    cohort_definition_name = paste0("cohort_", unique(cohorts$cohort_definition_id)),
    cohort_definition_description = "reach fixture cohort",
    definition_type_concept_id = 0L, cohort_definition_syntax = "",
    subject_concept_id = 0L, cohort_initiation_date = "2020-01-01",
    stringsAsFactors = FALSE))

  # =========================================================================
  # CONDITION_OCCURRENCE — pattern-rich seeding by person-id modulo classes
  # index date ~ 2020-01-01 anchored cohort; events spread before/after
  # =========================================================================
  co <- list(); cid_seq <- 1L
  add_co <- function(pids, concept, start, end = start, type = 44818518L) {
    for (p in pids) {
      co[[length(co)+1L]] <<- data.frame(
        condition_occurrence_id = cid_seq, person_id = p,
        condition_concept_id = concept, condition_start_date = start,
        condition_end_date = end, condition_type_concept_id = type,
        stop_reason = NA_character_, provider_id = p, visit_occurrence_id = p,
        condition_source_value = paste0("C", concept),
        condition_source_concept_id = 0L, stringsAsFactors = FALSE)
      cid_seq <<- cid_seq + 1L
    }
  }
  P <- function(mod, r=0L) persons[persons %% mod == r]
  add_co(persons[1:50], DM, "2019-06-01")           # ~half diabetic
  add_co(persons[1:25], DM, "2019-06-05", type = COND_PROBLIST)  # DM problem-list rows (C27)
  add_co(persons[1:25], DM, "2019-06-08", type = TYPE_EHR)       # DM EHR-claim rows (D14, type 32817)
  add_co(persons[1:30], PRE, "2019-05-01")          # prediabetes
  add_co(persons[20:70], HTN1, "2019-06-15")        # hypertensive (320128)
  add_co(persons[20:70], HTN2, "2019-06-15")        # HTN alt code (316866)
  add_co(persons[1:25], MI, "2019-07-01")           # MI
  add_co(persons[1:20], CKD, "2019-08-01")          # CKD
  add_co(persons[10:40], AF, "2019-06-20")          # AFib
  add_co(persons[5:35], HF, "2019-07-10")           # HF
  add_co(persons[1:30], HLD, "2019-05-20")          # hyperlipidemia
  add_co(persons[40:60], OBES, "2019-04-01")        # obesity
  add_co(ev[ev <= 30], PREG, "2019-09-01")          # pregnancy (women)
  add_co(persons[1:25], STROKE, "2019-03-01")       # stroke 381316
  add_co(persons[1:25], STROKE2, "2019-03-05")      # stroke 372924
  add_co(persons[1:15], SEPSIS, "2020-02-01")       # sepsis
  add_co(persons[1:12], CIRR, "2019-02-01")         # cirrhosis
  add_co(persons[1:30], ASTHMA, "2019-06-01")       # asthma
  add_co(persons[1:30], ASTHMA, "2019-09-01")       # asthma 2nd occurrence
  add_co(persons[1:25], COPD, "2019-05-01")         # COPD
  add_co(persons[1:20], CHESTPAIN, "2020-01-05")    # chest pain
  add_co(persons[1:30], RESP, "2020-04-01")         # respiratory infection (covid era)
  add_co(persons[1:10], PRIOR_BLEED, "2017-01-01")  # prior bleed (>1yr before)
  add_co(persons[1:8], MAJOR_BLEED, "2019-09-01")   # major bleed within yr
  add_co(persons[1:12], MALIG, "2019-01-01")        # malignancy
  add_co(persons[1:20], CHRONICPAIN, "2019-01-15")  # chronic pain
  add_co(persons[1:20], HTNCOMP, "2019-06-01")      # htn comp
  add_co(persons[1:18], MI2, "2019-07-02")          # MI alt
  # Synthetic panel concepts for covariates_sparse blocks (E3: 4000000-4000049,
  # E21: 4000000-4000499). Seed a handful so >=1 block column is non-empty.
  for (cc in c(4000000L, 4000001L, 4000002L, 4000025L, 4000049L, 4000100L, 4000300L, 4000499L))
    add_co(persons[1:20], cc, "2019-06-01")
  write("condition_occurrence", do.call(rbind, co))

  # =========================================================================
  # DRUG_EXPOSURE
  # =========================================================================
  de <- list(); de_seq <- 1L
  add_de <- function(pids, concept, start, end = start, type = 38000177L,
                     days_supply = 30L, route = 0L, dose_unit = 0L) {
    for (p in pids) {
      de[[length(de)+1L]] <<- data.frame(
        drug_exposure_id = de_seq, person_id = p, drug_concept_id = concept,
        drug_exposure_start_date = start, drug_exposure_end_date = end,
        drug_type_concept_id = type, stop_reason = NA_character_,
        refills = 0L, days_supply = days_supply, sig = paste0("sig", p),
        route_concept_id = route, lot_number = "L1",
        dose_unit_concept_id = dose_unit,
        drug_source_value = paste0("D", concept), drug_source_concept_id = 0L,
        stringsAsFactors = FALSE)
      de_seq <<- de_seq + 1L
    }
  }
  add_de(persons[1:40], METF, "2019-07-01", days_supply = 90L, type = DRUGTYPE_PHARM)
  add_de(persons[1:40], METF, "2019-10-15", days_supply = 90L, type = DRUGTYPE_PHARM)  # 2nd fill (gap)
  add_de(persons[5:25], INS, "2019-08-01", days_supply = 30L)
  add_de(persons[10:35], STATIN, "2020-01-10", days_supply = 30L)   # post-index [0,30]
  add_de(persons[1:20], WARF, "2019-09-01", days_supply = 30L)
  add_de(persons[20:45], ACEI, "2019-06-01", days_supply = 90L)
  add_de(persons[15:40], ARB, "2020-01-05", days_supply = 90L)
  add_de(persons[5:30], BB, "2019-07-01", days_supply = 90L)
  add_de(persons[10:30], DIUR, "2019-07-15", days_supply = 90L)
  add_de(persons[10:30], DOAC, "2020-01-15", days_supply = 30L)     # within [0,90]
  add_de(persons[1:15], SGLT2, "2019-08-01", days_supply = 30L)
  add_de(persons[1:10], SPIRO, "2019-03-01", days_supply = 30L)
  add_de(persons[5:25], LOOPD, "2019-07-01", days_supply = 30L)
  add_de(persons[1:20], OPIOID, "2020-01-05", days_supply = 7L, route = 4132161L, dose_unit = 8576L)
  add_de(persons[1:20], OPIOID, "2020-01-20", days_supply = 7L, route = 4132161L, dose_unit = 8576L)
  add_de(persons[1:20], OPIOID, "2020-02-10", days_supply = 7L, route = 4132161L, dose_unit = 8576L)  # >=3 fills
  add_de(persons[1:25], SABA, "2019-06-01", days_supply = 30L)
  add_de(persons[1:15], PRED, "2019-05-01", days_supply = 14L)
  add_de(persons[1:10], VASO1, "2020-02-02", days_supply = 1L)
  add_de(persons[1:10], PROPOFOL, "2020-02-02", days_supply = 1L)
  add_de(persons[1:30], ASPIRIN, "2019-07-01", days_supply = 30L)
  write("drug_exposure", do.call(rbind, de))

  # =========================================================================
  # MEASUREMENT — value-rich for value/unit/type scenarios
  # =========================================================================
  me <- list(); me_seq <- 1L
  add_me <- function(pids, concept, date, value, unit = U_MGDL, vac = 0L,
                     mtype = 44818702L, rlow = NA_real_, rhigh = NA_real_,
                     op = 0L) {
    for (p in pids) {
      me[[length(me)+1L]] <<- data.frame(
        measurement_id = me_seq, person_id = p, measurement_concept_id = concept,
        measurement_date = date, measurement_type_concept_id = mtype,
        operator_concept_id = op, value_as_number = value, value_as_concept_id = vac,
        unit_concept_id = unit, range_low = rlow, range_high = rhigh,
        measurement_source_value = paste0("M", concept), measurement_source_concept_id = 0L,
        unit_source_value = "u", stringsAsFactors = FALSE)
      me_seq <<- me_seq + 1L
    }
  }
  # HbA1c — multiple per person + unit variants + values straddling 6.5
  add_me(persons[1:50], A1C, "2019-12-15", 7.5, unit = U_PCT, rlow=4, rhigh=6)
  add_me(persons[1:50], A1C, "2020-03-15", 8.2, unit = U_PCT, rlow=4, rhigh=6)
  add_me(persons[1:20], A1C, "2020-02-01", 52, unit = U_MMOLMOL)
  add_me(persons[1:10], A1C, "2020-02-15", 6.0, unit = 0L)
  add_me(persons[1:30], EGFR, "2019-12-01", 55, rlow=60, rhigh=120)  # <60 lots
  add_me(persons[1:30], EGFR, "2020-06-01", 48)
  add_me(persons[1:30], EGFR2, "2019-12-01", 50)
  add_me(persons[1:30], EGFR2, "2020-06-01", 45)
  add_me(persons[1:25], LDL, "2019-11-01", 170)   # >=160
  add_me(persons[1:25], LDL2, "2019-11-01", 130)
  add_me(persons[20:70], SBP, "2019-12-01", 150, rlow=90, rhigh=140)  # >=140
  add_me(persons[20:70], SBP, "2020-03-01", 135)
  add_me(persons[1:30], CREAT, "2019-12-01", 2.2, mtype = TYPE_EHR, rlow=0.6, rhigh=1.3)  # >=2.0
  add_me(persons[1:30], CREAT, "2020-02-01", 1.1, mtype = TYPE_CLAIMS, rlow=0.6, rhigh=1.3)
  add_me(persons[1:40], GLUC, "2019-12-01", 180, unit = U_MGDL)
  add_me(persons[1:40], GLUC, "2020-01-15", 9.0, unit = U_MMOLMOL)
  add_me(persons[1:30], K, "2019-12-01", 6.5, vac = VAL_HIGH, rlow=3.5, rhigh=5.0)  # high
  add_me(persons[1:30], K, "2020-02-01", 3.0, rlow=3.5, rhigh=5.0)                  # low
  add_me(persons[1:30], K, "2020-04-01", 4.2, rlow=3.5, rhigh=5.0)                  # in-band [3.5,5.0] (C21)
  add_me(persons[1:30], NA_, "2019-12-01", 128, rlow=135, rhigh=145)               # low Na
  add_me(persons[1:25], CA, "2019-12-01", 11.0, rlow=8.5, rhigh=10.5)              # high Ca
  add_me(persons[1:20], TROP, "2020-01-03", 0.08, rlow=0, rhigh=0.04)             # high trop within wk
  add_me(persons[1:20], INR, "2020-01-10", 2.5)
  add_me(persons[1:15], BNP, "2020-01-01", 500)
  add_me(persons[1:12], LACT, "2020-02-01", 4.0)
  add_me(persons[1:20], CHOL, "2019-12-01", 200, op = OP_EQ)
  add_me(persons[1:15], ALB, "2019-12-01", 3.0)
  add_me(persons[1:12], KI67, "2019-12-01", 25)
  add_me(persons[1:15], SIRS1, "2020-02-01", 13000)
  add_me(persons[1:15], SIRS2, "2020-02-01", 39)
  add_me(persons[1:20], SERO1, "2019-12-01", 1, vac = VAL_POS)
  add_me(persons[1:20], SERO2, "2019-12-01", 1, vac = VAL_REACTIVE)
  add_me(persons[1:40], WT, "2019-12-01", 80, unit = U_KG, mtype = TYPE_EHR)
  add_me(persons[1:20], WT, "2020-02-01", 176, unit = U_LB, mtype = TYPE_CLAIMS)
  add_me(persons[1:20], HEMO, "2020-02-01", 10)
  add_me(persons[1:25], CREAT, "2020-04-01", 2.5)   # extra creat for survival/AKI
  write("measurement", do.call(rbind, me))

  # =========================================================================
  # OBSERVATION
  # =========================================================================
  write("observation", data.frame(
    observation_id = persons, person_id = persons,
    observation_concept_id = rep(SMOKING, n_persons),
    observation_date = rep("2019-10-01", n_persons),
    observation_type_concept_id = rep(44814721L, n_persons),
    value_as_number = ifelse(persons %% 3L == 0L, 5, 2),
    value_as_string = paste0("freetext", persons),
    value_as_concept_id = rep(SMOKE_VAL, n_persons),
    observation_source_value = paste0("O", persons),
    observation_source_concept_id = 0L, unit_concept_id = 0L,
    stringsAsFactors = FALSE))
  # extra numeric observation (pain score 4291306)
  pain <- data.frame(
    observation_id = n_persons + persons[1:30], person_id = persons[1:30],
    observation_concept_id = rep(PAINSCORE, 30),
    observation_date = rep("2020-01-10", 30),
    observation_type_concept_id = rep(44814721L, 30),
    value_as_number = rep(6, 30), value_as_string = NA_character_,
    value_as_concept_id = 0L, observation_source_value = "PAIN",
    observation_source_concept_id = 0L, unit_concept_id = 0L,
    stringsAsFactors = FALSE)
  DBI::dbWriteTable(conn, "observation", pain, append = TRUE)

  # =========================================================================
  # VISIT_OCCURRENCE — inpatient/outpatient/ER, multiple per person
  # =========================================================================
  vo <- list(); vo_seq <- 1L
  add_vo <- function(pids, vconcept, start, end = start) {
    for (p in pids) {
      vo[[length(vo)+1L]] <<- data.frame(
        visit_occurrence_id = vo_seq, person_id = p, visit_concept_id = vconcept,
        visit_start_date = start, visit_end_date = end,
        visit_type_concept_id = 44818518L, visit_source_value = "V",
        visit_source_concept_id = 0L, stringsAsFactors = FALSE)
      vo_seq <<- vo_seq + 1L
    }
  }
  add_vo(persons, VOUT, "2019-05-01", "2019-05-02")
  add_vo(persons, VOUT, "2019-08-01", "2019-08-02")
  add_vo(persons[1:60], VOUT, "2019-11-01", "2019-11-02")
  add_vo(persons[1:60], VOUT, "2020-02-01", "2020-02-02")
  add_vo(persons[1:50], VINP, "2020-01-01", "2020-01-05")
  add_vo(persons[1:40], VINP, "2020-03-01", "2020-03-04")  # 2nd inpatient
  add_vo(persons[1:30], VER, "2020-01-03", "2020-01-03")
  add_vo(persons[1:30], VER, "2020-02-15", "2020-02-15")
  write("visit_occurrence", do.call(rbind, vo))

  # visit_detail (ICU sub-stays use concept 32037)
  write("visit_detail", data.frame(
    visit_detail_id = persons, person_id = persons,
    visit_detail_concept_id = ifelse(persons <= 20L, VICU_DETAIL, VOUT),
    visit_detail_start_date = rep("2020-01-01", n_persons),
    visit_detail_end_date = rep("2020-01-03", n_persons),
    visit_occurrence_id = persons, parent_visit_detail_id = NA_integer_,
    stringsAsFactors = FALSE))

  # =========================================================================
  # PROCEDURE_OCCURRENCE
  # =========================================================================
  po <- list(); po_seq <- 1L
  add_po <- function(pids, concept, date) {
    for (p in pids) {
      po[[length(po)+1L]] <<- data.frame(
        procedure_occurrence_id = po_seq, person_id = p,
        procedure_concept_id = concept, procedure_date = date,
        procedure_end_date = date, procedure_type_concept_id = 44818518L,
        procedure_source_value = "P", procedure_source_concept_id = 0L,
        stringsAsFactors = FALSE)
      po_seq <<- po_seq + 1L
    }
  }
  add_po(persons[1:15], DIALYSIS, "2019-09-01")
  add_po(persons[1:12], PCI, "2020-01-01")
  add_po(persons[40:50], BARIATRIC, "2019-09-01")
  add_po(persons[1:20], CATH, "2019-12-01")
  add_po(persons[1:25], CARDIO_PROC, "2019-08-01")
  add_po(persons[1:18], CARDIO_PROC, "2019-09-01")  # for concept_count>=1
  write("procedure_occurrence", do.call(rbind, po))

  # =========================================================================
  # DEVICE_EXPOSURE
  # =========================================================================
  dev <- list(); dev_seq <- 1L
  add_dev <- function(pids, concept, start, end, type = 44818518L) {
    for (p in pids) {
      dev[[length(dev)+1L]] <<- data.frame(
        device_exposure_id = dev_seq, person_id = p, device_concept_id = concept,
        device_exposure_start_date = start, device_exposure_end_date = end,
        device_type_concept_id = type, unique_device_id = "UDI",
        device_source_value = "DEV", device_source_concept_id = 0L,
        stringsAsFactors = FALSE)
      dev_seq <<- dev_seq + 1L
    }
  }
  add_dev(persons[1:12], CATHETER, "2020-01-01", "2020-01-10")
  add_dev(persons[1:10], STENT, "2020-01-01", "2020-01-01")
  add_dev(persons[1:10], VENT, "2020-02-02", "2020-02-08", type = DEVTYPE)
  add_dev(persons[1:8], DEV1, "2019-06-01", "2019-06-01", type = DEVTYPE)
  add_dev(persons[1:8], DEV2, "2019-06-01", "2019-06-01", type = DEVTYPE)
  write("device_exposure", do.call(rbind, dev))

  # =========================================================================
  # SPECIMEN
  # =========================================================================
  sp <- list(); sp_seq <- 1L
  add_sp <- function(pids, concept, date, qty = 5, stype = SPECTYPE, dstatus = 0L) {
    for (p in pids) {
      sp[[length(sp)+1L]] <<- data.frame(
        specimen_id = sp_seq, person_id = p, specimen_concept_id = concept,
        specimen_type_concept_id = stype, specimen_date = date, quantity = qty,
        unit_concept_id = 0L, disease_status_concept_id = dstatus,
        specimen_source_id = "S", specimen_source_value = "S",
        stringsAsFactors = FALSE)
      sp_seq <<- sp_seq + 1L
    }
  }
  add_sp(persons[1:15], SPEC_BLOOD, "2020-01-02")
  add_sp(persons[1:12], SPEC_TUMOR, "2019-12-01", dstatus = DISEASE_STATUS)
  add_sp(persons[1:12], SPEC2, "2019-12-01")
  write("specimen", do.call(rbind, sp))

  # =========================================================================
  # ERA TABLES
  # =========================================================================
  mk_era <- function(pids, concept, start, end) data.frame(
    person_id = pids, concept = concept, start = start, end = end,
    stringsAsFactors = FALSE)
  # drug_era
  dre <- list(); dre_seq <- 1L
  add_dre <- function(pids, concept, start, end) {
    for (p in pids) {
      dre[[length(dre)+1L]] <<- data.frame(
        drug_era_id = dre_seq, person_id = p, drug_concept_id = concept,
        drug_era_start_date = start, drug_era_end_date = end,
        drug_exposure_count = 3L, gap_days = 0L, stringsAsFactors = FALSE)
      dre_seq <<- dre_seq + 1L
    }
  }
  add_dre(persons[1:40], METF, "2019-06-01", "2020-06-01")
  add_dre(persons[5:25], INS, "2019-07-01", "2020-05-01")
  add_dre(persons[15:40], ARB, "2020-01-01", "2020-08-01")
  add_dre(persons[20:45], ACEI, "2019-06-01", "2020-06-01")
  add_dre(persons[1:20], OPIOID, "2020-01-01", "2020-03-01")
  add_dre(persons[1:15], PRED, "2019-05-01", "2019-06-01")
  add_dre(persons[1:15], PRED, "2019-08-01", "2019-09-01")
  add_dre(persons[1:15], PRED, "2019-11-01", "2019-12-01")  # >=3 eras
  add_dre(persons[1:30], ASPIRIN, "2019-06-01", "2020-06-01")
  write("drug_era", do.call(rbind, dre))
  # condition_era
  cre <- list(); cre_seq <- 1L
  add_cre <- function(pids, concept, start, end) {
    for (p in pids) {
      cre[[length(cre)+1L]] <<- data.frame(
        condition_era_id = cre_seq, person_id = p, condition_concept_id = concept,
        condition_era_start_date = start, condition_era_end_date = end,
        condition_occurrence_count = 2L, stringsAsFactors = FALSE)
      cre_seq <<- cre_seq + 1L
    }
  }
  add_cre(persons[5:35], HF, "2019-07-10", "2020-07-10")
  add_cre(persons[1:30], ASTHMA, "2019-06-01", "2020-01-01")
  add_cre(persons[1:20], CHRONICPAIN, "2019-01-15", "2020-01-15")
  add_cre(persons[1:50], DM, "2019-06-01", "2020-06-01")
  write("condition_era", do.call(rbind, cre))
  # dose_era
  doe <- list(); doe_seq <- 1L
  add_doe <- function(pids, concept, start, end, unit = U_INSULIN, dose = 50) {
    for (p in pids) {
      doe[[length(doe)+1L]] <<- data.frame(
        dose_era_id = doe_seq, person_id = p, drug_concept_id = concept,
        unit_concept_id = unit, dose_value = dose,
        dose_era_start_date = start, dose_era_end_date = end,
        stringsAsFactors = FALSE)
      doe_seq <<- doe_seq + 1L
    }
  }
  add_doe(persons[5:25], INS, "2019-07-01", "2020-05-01")
  add_doe(persons[1:15], PRED, "2019-05-01", "2019-06-01")
  add_doe(persons[1:40], METF, "2019-06-01", "2020-06-01")
  write("dose_era", do.call(rbind, doe))

  # =========================================================================
  # DEATH — subset dies; death_date within [index, index+365]
  # =========================================================================
  death_persons <- persons[persons %% 4L == 0L]   # 25 deaths
  write("death", data.frame(
    person_id = death_persons, death_date = rep("2020-06-15", length(death_persons)),
    death_type_concept_id = rep(TYPE_EHR, length(death_persons)),
    cause_concept_id = rep(MI, length(death_persons)),
    cause_source_value = "I21", cause_source_concept_id = 0L,
    stringsAsFactors = FALSE))

  # =========================================================================
  # VOCABULARY — concept, concept_ancestor, concept_relationship, etc.
  # Register EVERY concept_id used above so translate/expand find names.
  # =========================================================================
  all_concepts <- sort(unique(c(
    8507L,8532L,8527L,38003564L,44814724L,44818518L,44818702L,44814721L,
    DM,PRE,HTN1,HTN2,MI,MI2,CKD,AF,HF,HLD,OBES,PREG,STROKE,STROKE2,SEPSIS,
    CIRR,PAINc,ASTHMA,COPD,CHESTPAIN,RESP,PRIOR_BLEED,MAJOR_BLEED,MALIG,
    CHRONICPAIN,HTNCOMP,
    METF,INS,STATIN,WARF,ACEI,ARB,BB,DIUR,DOAC,SGLT2,SPIRO,LOOPD,OPIOID,
    SABA,PRED,VASO1,VASO2,PROPOFOL,ASPIRIN,
    A1C,EGFR,EGFR2,LDL,LDL2,SBP,CREAT,GLUC,K,NA_,CA,TROP,INR,BNP,LACT,CHOL,
    ALB,KI67,SIRS1,SIRS2,SERO1,SERO2,WT,PAINSCORE,HEMO,DRUGCONC,
    DIALYSIS,PCI,BARIATRIC,CATH,CARDIO_PROC,VENT,STENT,CATHETER,DEV1,DEV2,
    SPEC_BLOOD,SPEC_TUMOR,SPEC2,SMOKING,
    VOUT,VINP,VER,VICU_DETAIL,
    U_PCT,U_MMOLMOL,U_MGDL,U_KG,U_LB,TYPE_EHR,TYPE_CLAIMS,VAL_HIGH,VAL_POS,
    VAL_REACTIVE,VAL_NEG,OP_EQ,DRUGTYPE_PHARM,DEVTYPE,SPECTYPE,DISEASE_STATUS,
    COND_PROBLIST,SMOKE_VAL,
    # route / dose-unit qualifier concepts (D28), synthetic panel (E3/E21)
    4132161L, 8576L, 4000000L,4000001L,4000002L,4000025L,4000049L,4000100L,4000300L,4000499L,
    # descendant concepts for expand= tests
    201821L,201822L,1596978L)))
  write("concept", data.frame(
    concept_id = all_concepts,
    concept_name = paste0("concept_", all_concepts),
    domain_id = rep("Condition", length(all_concepts)),
    vocabulary_id = rep("SNOMED", length(all_concepts)),
    concept_class_id = rep("Clinical Finding", length(all_concepts)),
    standard_concept = rep("S", length(all_concepts)),
    concept_code = as.character(all_concepts),
    valid_start_date = rep("1970-01-01", length(all_concepts)),
    valid_end_date = rep("2099-12-31", length(all_concepts)),
    invalid_reason = rep(NA_character_, length(all_concepts)),
    stringsAsFactors = FALSE))

  # concept_ancestor: self-rows for all + a few real descendant chains
  anc <- data.frame(
    ancestor_concept_id = all_concepts, descendant_concept_id = all_concepts,
    min_levels_of_separation = 0L, max_levels_of_separation = 0L)
  extra_anc <- data.frame(
    ancestor_concept_id = c(DM, DM, INS),
    descendant_concept_id = c(201821L, 201822L, 1596978L),
    min_levels_of_separation = 1L, max_levels_of_separation = 1L)
  write("concept_ancestor", rbind(anc, extra_anc))

  write("concept_relationship", data.frame(
    concept_id_1 = integer(0), concept_id_2 = integer(0),
    relationship_id = character(0), valid_start_date = character(0),
    valid_end_date = character(0), invalid_reason = character(0),
    stringsAsFactors = FALSE))
  write("domain", data.frame(
    domain_id = c("Condition","Drug","Measurement","Observation","Gender",
                  "Race","Ethnicity","Type Concept","Visit","Unit","Device",
                  "Specimen","Procedure"),
    domain_name = c("Condition","Drug","Measurement","Observation","Gender",
                  "Race","Ethnicity","Type Concept","Visit","Unit","Device",
                  "Specimen","Procedure"),
    domain_concept_id = 1:13, stringsAsFactors = FALSE))
  write("vocabulary", data.frame(
    vocabulary_id = c("SNOMED","Gender","Race","Ethnicity","Type Concept","Visit"),
    vocabulary_name = c("SNOMED","Gender","Race","Ethnicity","Type Concept","Visit"),
    vocabulary_reference = "", vocabulary_version = "test",
    vocabulary_concept_id = 1:6, stringsAsFactors = FALSE))
  DBI::dbExecute(conn, "CREATE TABLE concept_class (concept_class_id TEXT, concept_class_name TEXT, concept_class_concept_id INTEGER)")

  write("cdm_source", data.frame(
    cdm_source_name = "Reach Fixture", cdm_source_abbreviation = "REACH",
    cdm_holder = "test", source_description = "reach", cdm_version = "v5.4",
    vocabulary_version = "v5.0", stringsAsFactors = FALSE))
  write("episode", data.frame(episode_id=integer(0), person_id=integer(0),
    episode_concept_id=integer(0), episode_start_date=character(0),
    episode_end_date=character(0), stringsAsFactors=FALSE))
  write("episode_event", data.frame(episode_id=integer(0), event_id=integer(0),
    episode_event_field_concept_id=integer(0), stringsAsFactors=FALSE))
  write("location", data.frame(location_id=persons, address_1="x", city="BCN",
    state="CT", zip="08000", county="BCN", latitude=rep(41.4,n_persons),
    longitude=rep(2.1,n_persons), stringsAsFactors=FALSE))

  # =========================================================================
  # Phase 7b FixToSummit augmentation: seed the exact temporal / set-overlap
  # patterns the windowed + nested set-op scenarios need so they resolve to a
  # NON-empty population on this fixture (cohort index date = 2020-01-01).
  # Each block is tied to a specific scenario; all rows APPEND to base tables.
  # =========================================================================
  append_tbl <- function(name, df) DBI::dbWriteTable(conn, name, df, append = TRUE)

  # --- Condition overlaps for nested set-ops (A5, A8) ----------------------
  # A5 final = (DM|prediab) & (CKD|AF) minus (SGLT2|metformin). DM=1:50 &
  # metformin=1:40, so 41:50 are DM-not-metformin; give 41:50 AF so they enter
  # cardiorenal and survive the metformin/SGLT2 setdiff.
  # A8 final = untreated_metab & untreated_cardiorenal; 46:50 are HTN-not-ACEi
  # AND DM-not-metformin — give them HF (not on diuretic 10:30) so they also
  # land in untreated_cardiorenal.
  co_seq2 <- max(DBI::dbGetQuery(conn,
    "SELECT MAX(condition_occurrence_id) m FROM condition_occurrence")$m, 0L)
  add_co_app <- function(pids, concept, start, type = 44818518L) {
    rows <- lapply(pids, function(p) {
      co_seq2 <<- co_seq2 + 1L
      data.frame(condition_occurrence_id = co_seq2, person_id = p,
        condition_concept_id = concept, condition_start_date = start,
        condition_end_date = start, condition_type_concept_id = type,
        stop_reason = NA_character_, provider_id = p, visit_occurrence_id = p,
        condition_source_value = paste0("C", concept),
        condition_source_concept_id = 0L, stringsAsFactors = FALSE)
    })
    append_tbl("condition_occurrence", do.call(rbind, rows))
  }
  add_co_app(persons[41:50], AF, "2019-06-20")   # A5: DM-not-metf gain AF
  add_co_app(persons[46:50], HF, "2019-07-10")   # A8: untreated_metab gain HF

  # --- Post-/pre-index drug windows (A26, A31, B16) ------------------------
  de_seq2 <- max(DBI::dbGetQuery(conn,
    "SELECT MAX(drug_exposure_id) m FROM drug_exposure")$m, 0L)
  add_de_app <- function(pids, concept, start, days = 30L, type = 38000177L) {
    rows <- lapply(pids, function(p) {
      de_seq2 <<- de_seq2 + 1L
      data.frame(drug_exposure_id = de_seq2, person_id = p,
        drug_concept_id = concept, drug_exposure_start_date = start,
        drug_exposure_end_date = start, drug_type_concept_id = type,
        stop_reason = NA_character_, refills = 0L, days_supply = days,
        sig = paste0("sig", p), route_concept_id = 0L, lot_number = "L1",
        dose_unit_concept_id = 0L, drug_source_value = paste0("D", concept),
        drug_source_concept_id = 0L, stringsAsFactors = FALSE)
    })
    append_tbl("drug_exposure", do.call(rbind, rows))
  }
  # A26: delayed DOAC starters — DOAC in [30,365] only (day +91), persons 31:45
  # are NOT in the early DOAC set (10:30), so delayed_doac is non-empty.
  add_de_app(persons[31:45], DOAC, "2020-04-01")
  # A31: anticoag (DOAC) in [-365,0] for DM/HTN persons (treated_cm), pre-index.
  add_de_app(persons[1:30], DOAC, "2019-09-01")
  # B16: metformin in [0,90] post-index (day +31) for DM persons, so dm_rx
  # (intersect with dm_dx + HbA1c) is non-empty.
  add_de_app(persons[1:40], METF, "2020-02-01", days = 90L,
             type = DRUGTYPE_PHARM)

  # --- Peri-index lactate window (B20) -------------------------------------
  # Lactate (3047181) around index for cohort 99 (1:95): pre [-7,-1], intra
  # [0,0] during an inpatient visit, post [1,7].
  me_seq2 <- max(DBI::dbGetQuery(conn,
    "SELECT MAX(measurement_id) m FROM measurement")$m, 0L)
  add_me_app <- function(pids, concept, date, value, unit = U_MGDL,
                         mtype = 44818702L) {
    rows <- lapply(pids, function(p) {
      me_seq2 <<- me_seq2 + 1L
      data.frame(measurement_id = me_seq2, person_id = p,
        measurement_concept_id = concept, measurement_date = date,
        measurement_type_concept_id = mtype, operator_concept_id = 0L,
        value_as_number = value, value_as_concept_id = 0L,
        unit_concept_id = unit, range_low = NA_real_, range_high = NA_real_,
        measurement_source_value = paste0("M", concept),
        measurement_source_concept_id = 0L, unit_source_value = "u",
        stringsAsFactors = FALSE)
    })
    append_tbl("measurement", do.call(rbind, rows))
  }
  add_me_app(persons[1:12], LACT, "2019-12-28", 3.5)   # B20 pre  [-7,-1]
  add_me_app(persons[1:12], LACT, "2020-01-01", 4.5)   # B20 intra [0,0]
  add_me_app(persons[1:12], LACT, "2020-01-05", 3.8)   # B20 post [1,7]
  # B20 intra requires an inpatient (9201) visit on the index day; visits at
  # 2020-01-01 (VINP 1:50) already cover persons 1:12.

  # ---- Build the handle env (mirrors create_test_handle) -------------------
  handle <- new.env(parent = emptyenv())
  handle$conn            <- conn
  handle$dbms            <- "sqlite"
  handle$target_dialect  <- "sqlite"
  handle$cdm_schema      <- NULL
  handle$vocab_schema    <- NULL
  handle$results_schema  <- NULL
  handle$temp_schema     <- NULL
  handle$resource_client <- NULL
  handle$config          <- list()
  handle$blueprint       <- NULL
  handle$temp_tables     <- character(0)
  handle$person_key      <- as.raw(1:16)   # enables omop.table token scope
  handle
}
