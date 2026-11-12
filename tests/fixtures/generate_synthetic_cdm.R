# =============================================================================
# WS-0 (design-2.10.3 section 10.2): deterministic synthetic OMOP CDM generator.
#
# generate_synthetic_cdm() builds 23 GiBleed-shaped CDM tables, with the full
# column lists for those tables from the vendored CDM 5.3 field metadata (the
# version Eunomia GiBleed uses; it does not generate every CDM table), filled
# deterministically from a seed for n_persons persons. It writes one
# CSV per table into `dir` and/or loads the tables into a DBI connection. The
# 10k-person set of the test plan is `generate_synthetic_cdm(10000L)`; nothing
# generated is checked in (design: generator script, not a blob).
#
#   generate_synthetic_cdm(n_persons = 10000L, seed = 20261112L,
#                          dir = NULL, conn = NULL, field_level_csv = NULL)
#
# Returns (invisibly) the named list of data frames. Row counts scale linearly
# with n_persons: about 3 visits, 4 conditions, 3 drug exposures, 5
# measurements and 1 observation per person over the observation window, 5%
# deaths, eras coalesced with a 30-day gap.
# =============================================================================

generate_synthetic_cdm <- function(n_persons = 10000L, seed = 20261112L,
                                   dir = NULL, conn = NULL,
                                   field_level_csv = NULL, schema = NULL) {
  n_persons <- as.integer(n_persons)
  if (is.null(field_level_csv)) {
    field_level_csv <- system.file("ohdsi", "OMOP_CDMv5.3_Field_Level.csv", package = "dsOMOP")
    if (!nzchar(field_level_csv)) {
      field_level_csv <- Find(file.exists, c("inst/ohdsi/OMOP_CDMv5.3_Field_Level.csv",
                                             "../../inst/ohdsi/OMOP_CDMv5.3_Field_Level.csv"))
    }
  }
  fields <- utils::read.csv(field_level_csv, stringsAsFactors = FALSE)
  fields$table <- tolower(fields$cdmTableName); fields$field <- tolower(fields$cdmFieldName)
  set.seed(seed)
  pid <- seq_len(n_persons)
  as_date <- function(x) as.Date(x, origin = "1970-01-01")
  fmt <- function(x) format(as_date(x), "%Y-%m-%d")

  # ---- a small reviewed concept dictionary --------------------------------
  concepts <- data.frame(
    concept_id = c(8507L, 8532L, 8527L, 8516L, 38003564L, 38003563L, 44814724L, 9201L, 9202L, 9203L,
                   44818518L, 38000177L, 44818702L, 44814721L, 32817L,
                   192671L, 201820L, 316866L, 4329847L, 255848L, 320128L, 433736L, 81893L, 4112853L,
                   1118084L, 1124300L, 1503297L, 1539403L, 1310149L, 19078106L,
                   4283095L, 4032243L, 4139525L,
                   3004410L, 3016723L, 3004249L, 3028288L, 3025315L, 3013682L,
                   4275495L, 8554L, 8840L, 9529L, 8753L, 4172703L, 4171756L, 4172704L),
    concept_name = c("MALE", "FEMALE", "White", "Asian", "Not Hispanic or Latino", "Hispanic or Latino",
                     "Period covering healthcare encounters", "Inpatient Visit", "Outpatient Visit",
                     "Emergency Room Visit", "EHR encounter record", "Prescription written",
                     "Lab result", "Patient reported", "EHR",
                     "Gastrointestinal hemorrhage", "Type 2 diabetes mellitus", "Essential hypertension",
                     "Myocardial infarction", "Pneumonia", "Hypertensive disorder", "Obesity",
                     "Osteoarthritis", "Primary malignant neoplasm",
                     "celecoxib", "aspirin", "metformin", "simvastatin", "warfarin", "ibuprofen",
                     "Percutaneous coronary intervention", "Renal dialysis", "Cardiac catheterization",
                     "Hemoglobin A1c", "Creatinine", "Systolic blood pressure", "LDL cholesterol",
                     "Body weight", "Urea nitrogen", "Tobacco smoking status",
                     "percent", "milligram per deciliter", "kilogram", "millimole per mole",
                     "=", "<", ">"),
    domain_id = c(rep("Gender", 2), rep("Race", 2), rep("Ethnicity", 2), "Type Concept", rep("Visit", 3),
                  rep("Type Concept", 5), rep("Condition", 9), rep("Drug", 6), rep("Procedure", 3),
                  rep("Measurement", 6), "Observation", rep("Unit", 4), rep("Meas Value Operator", 3)),
    vocabulary_id = c(rep("Gender", 2), rep("Race", 2), rep("Ethnicity", 2), "Type Concept", rep("Visit", 3),
                      rep("Type Concept", 5), rep("SNOMED", 9), rep("RxNorm", 6), rep("SNOMED", 3),
                      rep("LOINC", 6), "SNOMED", rep("UCUM", 4), rep("SNOMED", 3)),
    stringsAsFactors = FALSE)
  cond_ids <- c(192671L, 201820L, 316866L, 4329847L, 255848L, 320128L, 433736L, 81893L, 4112853L)
  drug_ids <- c(1118084L, 1124300L, 1503297L, 1539403L, 1310149L, 19078106L)
  proc_ids <- c(4283095L, 4032243L, 4139525L)
  meas_ids <- c(3004410L, 3016723L, 3004249L, 3028288L, 3025315L, 3013682L)
  meas_units <- c(8554L, 8840L, 8840L, 8840L, 9529L, 8840L)
  meas_mean <- c(6.5, 1.0, 130, 120, 78, 15); meas_sd <- c(1.2, 0.3, 18, 30, 15, 5)

  # ---- person, observation_period -------------------------------------------
  yob <- 2005L - as.integer(round(stats::rbeta(n_persons, 2, 2) * 75))
  gender <- ifelse(stats::runif(n_persons) < 0.52, 8532L, 8507L)
  op_start <- as.integer(as.Date("2010-01-01")) + as.integer(round(stats::runif(n_persons, 0, 365 * 6)))
  op_end <- pmin(op_start + as.integer(round(stats::runif(n_persons, 365, 365 * 10))),
                 as.integer(as.Date("2022-12-31")))
  dead <- stats::runif(n_persons) < 0.05
  death_day <- ifelse(dead, op_start + as.integer(round(stats::runif(n_persons, 0.5, 1) * (op_end - op_start))), NA_integer_)
  op_end <- ifelse(dead, death_day, op_end)

  rows_per <- function(rate) stats::rpois(n_persons, rate * pmax(1, (op_end - op_start) / 365.25))
  expand <- function(k) rep.int(pid, k)
  day_in_op <- function(p) op_start[p] + as.integer(floor(stats::runif(length(p)) * (op_end[p] - op_start[p] + 1)))

  k_visit <- rows_per(1.5); vp <- expand(k_visit); nv <- length(vp)
  v_start <- day_in_op(vp); v_type <- sample(c(9202L, 9201L, 9203L), nv, TRUE, c(0.8, 0.12, 0.08))
  v_end <- ifelse(v_type == 9201L, v_start + as.integer(round(stats::runif(nv, 1, 7))), v_start)
  k_cond <- rows_per(1.3); cp <- expand(k_cond); nc <- length(cp)
  c_start <- day_in_op(cp)
  k_drug <- rows_per(1.0); dp <- expand(k_drug); nd <- length(dp)
  d_start <- day_in_op(dp); d_supply <- sample(c(7L, 14L, 30L, 90L), nd, TRUE, c(0.1, 0.1, 0.6, 0.2))
  d_end <- ifelse(stats::runif(nd) < 0.85, d_start + d_supply - 1L, NA_integer_)
  k_meas <- rows_per(1.7); mp <- expand(k_meas); nm <- length(mp)
  m_which <- sample(seq_along(meas_ids), nm, TRUE); m_start <- day_in_op(mp)
  m_value <- round(stats::rnorm(nm, meas_mean[m_which], meas_sd[m_which]), 2)
  k_proc <- rows_per(0.3); pp <- expand(k_proc); np <- length(pp)

  typed_frame <- function(table, values) {
    spec <- fields[fields$table == table, , drop = FALSE]
    n <- if (length(values) > 0L) length(values[[1L]]) else 0L
    if (nrow(spec) == 0L) {   # a table outside the field metadata (the 5.3 results cohort): keep the given columns
      return(as.data.frame(values, stringsAsFactors = FALSE))
    }
    out <- lapply(seq_len(nrow(spec)), function(i) {
      f <- spec$field[i]
      if (!is.null(values[[f]])) return(values[[f]])
      dt <- tolower(spec$cdmDatatype[i])
      if (grepl("^integer", dt)) rep(NA_integer_, n)
      else if (grepl("^float", dt)) rep(NA_real_, n)
      else rep(NA_character_, n)   # date, datetime, varchar
    })
    names(out) <- spec$field
    as.data.frame(out, stringsAsFactors = FALSE)
  }

  cdm <- list()
  cdm$person <- typed_frame("person", list(
    person_id = pid, gender_concept_id = gender, year_of_birth = yob,
    month_of_birth = sample(1:12, n_persons, TRUE), day_of_birth = sample(1:28, n_persons, TRUE),
    race_concept_id = sample(c(8527L, 8516L), n_persons, TRUE, c(0.85, 0.15)),
    ethnicity_concept_id = sample(c(38003564L, 38003563L), n_persons, TRUE, c(0.9, 0.1)),
    location_id = pid, provider_id = NA_integer_, care_site_id = 1L + (pid %% 20L),
    person_source_value = paste0("P", pid), gender_source_value = ifelse(gender == 8532L, "F", "M")))
  cdm$observation_period <- typed_frame("observation_period", list(
    observation_period_id = pid, person_id = pid, observation_period_start_date = fmt(op_start),
    observation_period_end_date = fmt(op_end), period_type_concept_id = rep(44814724L, n_persons)))
  cdm$visit_occurrence <- typed_frame("visit_occurrence", list(
    visit_occurrence_id = seq_len(nv), person_id = vp, visit_concept_id = v_type,
    visit_start_date = fmt(v_start), visit_end_date = fmt(v_end),
    visit_type_concept_id = rep(44818518L, nv), care_site_id = 1L + (vp %% 20L)))
  cdm$condition_occurrence <- typed_frame("condition_occurrence", list(
    condition_occurrence_id = seq_len(nc), person_id = cp,
    condition_concept_id = sample(cond_ids, nc, TRUE, c(0.08, 0.2, 0.25, 0.05, 0.1, 0.12, 0.1, 0.07, 0.03)),
    condition_start_date = fmt(c_start), condition_type_concept_id = rep(32817L, nc)))
  cdm$drug_exposure <- typed_frame("drug_exposure", list(
    drug_exposure_id = seq_len(nd), person_id = dp,
    drug_concept_id = sample(drug_ids, nd, TRUE, c(0.15, 0.25, 0.25, 0.2, 0.05, 0.1)),
    drug_exposure_start_date = fmt(d_start), drug_exposure_end_date = ifelse(is.na(d_end), NA_character_, fmt(d_end)),
    drug_type_concept_id = rep(38000177L, nd), days_supply = d_supply))
  cdm$procedure_occurrence <- typed_frame("procedure_occurrence", list(
    procedure_occurrence_id = seq_len(np), person_id = pp,
    procedure_concept_id = sample(proc_ids, np, TRUE), procedure_date = fmt(day_in_op(pp)),
    procedure_type_concept_id = rep(44818518L, np)))
  cdm$measurement <- typed_frame("measurement", list(
    measurement_id = seq_len(nm), person_id = mp, measurement_concept_id = meas_ids[m_which],
    measurement_date = fmt(m_start), measurement_type_concept_id = rep(44818702L, nm),
    operator_concept_id = sample(c(4172703L, 4171756L, 4172704L), nm, TRUE, c(0.96, 0.02, 0.02)),
    value_as_number = m_value, unit_concept_id = meas_units[m_which]))
  cdm$observation <- typed_frame("observation", list(
    observation_id = pid, person_id = pid, observation_concept_id = rep(4275495L, n_persons),
    observation_date = fmt(op_start + 1L), observation_type_concept_id = rep(44814721L, n_persons),
    value_as_number = sample(c(0, 1, 2), n_persons, TRUE)))
  dpid <- pid[dead]
  cdm$death <- typed_frame("death", list(
    person_id = dpid, death_date = fmt(death_day[dead]), death_type_concept_id = rep(32817L, length(dpid)),
    cause_concept_id = sample(c(4329847L, 255848L, 4112853L), length(dpid), TRUE)))

  # eras: coalesce exposures per (person, concept) with a 30-day gap
  era_of <- function(p, cid, s, e, gap = 30L) {
    e <- ifelse(is.na(e), s, e)
    o <- order(p, cid, s); p <- p[o]; cid <- cid[o]; s <- s[o]; e <- e[o]
    key <- paste(p, cid); new <- c(TRUE, key[-1] != key[-length(key)])
    run_end <- e; grp <- integer(length(p)); g <- 0L; cur_end <- -Inf
    for (i in seq_along(p)) {
      if (new[i] || s[i] > cur_end + gap + 1L) { g <- g + 1L; cur_end <- e[i] } else cur_end <- max(cur_end, e[i])
      grp[i] <- g
    }
    data.frame(person_id = tapply(p, grp, `[`, 1L), concept_id = tapply(cid, grp, `[`, 1L),
               start = tapply(s, grp, min), end = tapply(e, grp, max), n = tapply(s, grp, length))
  }
  de <- era_of(dp, cdm$drug_exposure$drug_concept_id, d_start, d_end)
  cdm$drug_era <- typed_frame("drug_era", list(
    drug_era_id = seq_len(nrow(de)), person_id = as.integer(de$person_id), drug_concept_id = as.integer(de$concept_id),
    drug_era_start_date = fmt(de$start), drug_era_end_date = fmt(de$end),
    drug_exposure_count = as.integer(de$n), gap_days = rep(0L, nrow(de))))
  ce <- era_of(cp, cdm$condition_occurrence$condition_concept_id, c_start, c_start)
  cdm$condition_era <- typed_frame("condition_era", list(
    condition_era_id = seq_len(nrow(ce)), person_id = as.integer(ce$person_id),
    condition_concept_id = as.integer(ce$concept_id), condition_era_start_date = fmt(ce$start),
    condition_era_end_date = fmt(ce$end), condition_occurrence_count = as.integer(ce$n)))
  cdm$location <- typed_frame("location", list(location_id = pid, city = rep("Synthetic", n_persons),
                                                zip = sprintf("%05d", 1000L + pid %% 900L)))
  cdm$care_site <- typed_frame("care_site", list(care_site_id = 1:20, care_site_name = paste0("Site ", 1:20),
                                                  place_of_service_concept_id = rep(9202L, 20)))
  cdm$provider <- typed_frame("provider", list(provider_id = 1:50, provider_name = paste0("Provider ", 1:50)))
  cdm$cdm_source <- typed_frame("cdm_source", list(
    cdm_source_name = "dsOMOP synthetic CDM", cdm_source_abbreviation = "SYN", cdm_holder = "dsOMOP tests",
    source_description = paste0("generate_synthetic_cdm(n_persons = ", n_persons, ", seed = ", seed, ")"),
    cdm_version = "v5.3.1", vocabulary_version = "synthetic"))
  cdm$concept <- typed_frame("concept", list(
    concept_id = concepts$concept_id, concept_name = concepts$concept_name, domain_id = concepts$domain_id,
    vocabulary_id = concepts$vocabulary_id, concept_class_id = rep("Clinical Finding", nrow(concepts)),
    standard_concept = rep("S", nrow(concepts)), concept_code = as.character(concepts$concept_id),
    valid_start_date = rep("1970-01-01", nrow(concepts)), valid_end_date = rep("2099-12-31", nrow(concepts))))
  cdm$concept_ancestor <- typed_frame("concept_ancestor", list(
    ancestor_concept_id = concepts$concept_id, descendant_concept_id = concepts$concept_id,
    min_levels_of_separation = rep(0L, nrow(concepts)), max_levels_of_separation = rep(0L, nrow(concepts))))
  cdm$concept_relationship <- typed_frame("concept_relationship", list(concept_id_1 = integer(0)))
  cdm$vocabulary <- typed_frame("vocabulary", list(
    vocabulary_id = unique(concepts$vocabulary_id), vocabulary_name = unique(concepts$vocabulary_id),
    vocabulary_reference = rep("synthetic", length(unique(concepts$vocabulary_id))),
    vocabulary_version = rep("synthetic", length(unique(concepts$vocabulary_id))),
    vocabulary_concept_id = seq_along(unique(concepts$vocabulary_id))))
  cdm$domain <- typed_frame("domain", list(
    domain_id = unique(concepts$domain_id), domain_name = unique(concepts$domain_id),
    domain_concept_id = seq_along(unique(concepts$domain_id))))
  cdm$concept_class <- typed_frame("concept_class", list(
    concept_class_id = "Clinical Finding", concept_class_name = "Clinical Finding", concept_class_concept_id = 1L))
  cdm$cohort_definition <- typed_frame("cohort_definition", list(cohort_definition_id = integer(0)))
  cdm$cohort <- typed_frame("cohort", list(cohort_definition_id = integer(0), subject_id = integer(0),
                                           cohort_start_date = character(0), cohort_end_date = character(0)))

  if (!is.null(dir)) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    for (nm in names(cdm)) utils::write.csv(cdm[[nm]], file.path(dir, paste0(nm, ".csv")), row.names = FALSE, na = "")
  }
  if (!is.null(conn)) {
    # SQLite keeps ISO-8601 text (the reach fixture convention); every other
    # DBMS gets typed DATE / TIMESTAMP columns, as load_reach_fixture_pg does.
    typed <- !inherits(conn, "SQLiteConnection")
    for (nm in names(cdm)) {
      df <- cdm[[nm]]
      if (typed) for (col in names(df)) {
        if (grepl("_date$", col) && is.character(df[[col]])) df[[col]] <- as.Date(df[[col]])
        if (grepl("_datetime$", col) && is.character(df[[col]])) df[[col]] <- as.POSIXct(df[[col]], tz = "UTC")
      }
      target <- if (is.null(schema)) nm else DBI::Id(schema = schema, table = nm)
      DBI::dbWriteTable(conn, target, df, overwrite = TRUE)
    }
  }
  invisible(cdm)
}

# Backend-independent cardinality / scenario signature of a loaded synthetic CDM
# (row counts per table plus a few typed-date scenario queries), used to prove
# that a PostgreSQL load equals the in-memory frames and a SQLite load.
synthetic_cdm_signature <- function(conn, schema = NULL, tables = NULL) {
  q <- function(sql) {
    if (!is.null(schema)) sql <- gsub("\\bFROM ([a-z_]+)", paste0("FROM ", schema, ".\\1"), sql, perl = TRUE)
    DBI::dbGetQuery(conn, sql)
  }
  if (is.null(tables)) tables <- c("person", "observation_period", "visit_occurrence", "condition_occurrence", "drug_exposure",
                                   "procedure_occurrence", "measurement", "observation", "death", "drug_era", "condition_era",
                                   "location", "care_site", "provider", "cdm_source", "concept", "concept_ancestor",
                                   "concept_relationship", "vocabulary", "domain", "concept_class", "cohort_definition", "cohort")
  counts <- vapply(tables, function(t) as.integer(q(paste0("SELECT COUNT(*) AS n FROM ", t))$n), integer(1))
  list(counts = counts, scenarios = list(
    persons_dead = as.integer(q("SELECT COUNT(*) AS n FROM death")$n),
    op_end_after_start = as.integer(q("SELECT COUNT(*) AS n FROM observation_period WHERE observation_period_end_date >= observation_period_start_date")$n),
    drugs_null_end = as.integer(q("SELECT COUNT(*) AS n FROM drug_exposure WHERE drug_exposure_end_date IS NULL")$n),
    eras_per_person = as.integer(q("SELECT COUNT(DISTINCT person_id) AS n FROM drug_era")$n),
    inpatient = as.integer(q("SELECT COUNT(*) AS n FROM visit_occurrence WHERE visit_concept_id = 9201")$n),
    cdm_version = as.character(q("SELECT cdm_version FROM cdm_source")$cdm_version),
    max_person = as.integer(q("SELECT MAX(person_id) AS n FROM person")$n)))
}
