# Shared deterministic OMOP CDM for executing the live Achilles catalogue on
# every engine (SQLite, DuckDB, PostgreSQL, MariaDB) through the real run path.
#
# The population is large enough for most live entries to return rows (40
# persons, every clinical domain populated) and embeds the small-support cases
# of the Achilles reader (G-03):
#   * 2016-06 condition records: person 1 (ten records) and person 2 (one) are
#     inside an observation period; persons 39 and 40 have records that month
#     but their only period starts in 2018, so upstream 420 excludes them;
#   * person 1 has an extra period overlapping their first one and person 2 an
#     exact duplicate period: neither may inflate a person basis;
#   * analysis 2102 rows by device x month, with one month shared by two
#     devices;
#   * 1818 measurements on both range edges, with NULL unit / NULL range rows
#     that upstream excludes;
#   * a NULL death type and an out-of-period death.
# Concept 0 (gender) and 8515 (race) are deliberately absent from the
# vocabulary, so a missing concept label is part of the grouping checks.

.achillesLiveFrames <- function() {
  d <- function(x) as.Date(x)
  pid <- 1:40
  person <- data.frame(
    person_id = pid,
    gender_concept_id = ifelse(pid <= 18L, 8507L,
                               ifelse(pid <= 36L, 8532L, 0L)),
    year_of_birth = 1950L + ((pid - 1L) %% 8L) * 5L,
    month_of_birth = ((pid - 1L) %% 12L) + 1L,
    race_concept_id = ifelse(pid <= 24L, 8527L,
                             ifelse(pid <= 36L, 8516L, 8515L)),
    ethnicity_concept_id = ifelse(pid %% 2L == 1L, 38003564L, 38003563L),
    stringsAsFactors = FALSE
  )

  op <- data.frame(
    observation_period_id = pid, person_id = pid,
    observation_period_start_date = d(ifelse(pid %in% 39:40, "2018-01-01",
                                             "2015-01-01")),
    observation_period_end_date = d("2022-12-31"),
    period_type_concept_id = 44814724L,
    stringsAsFactors = FALSE
  )
  op <- rbind(op, data.frame(
    observation_period_id = 41:47,
    person_id = c(31:35, 1L, 2L),
    observation_period_start_date = d(c(rep("2023-06-01", 5), "2016-01-01",
                                        "2015-01-01")),
    observation_period_end_date = d(c(rep("2024-06-30", 5), "2016-12-31",
                                      "2022-12-31")),
    period_type_concept_id = 44814724L,
    stringsAsFactors = FALSE
  ))

  cond_rows <- list(
    data.frame(person_id = 1:20, concept = 201820L,
               date = d(sprintf("2020-%02d-15", ((1:20) %% 4L) + 1L))),
    data.frame(person_id = 10:30, concept = 320128L, date = d("2019-03-10")),
    data.frame(person_id = 1L, concept = 201820L,
               date = d(sprintf("2016-06-%02d", 1:10))),
    data.frame(person_id = c(2L, 39L, 40L), concept = 201820L,
               date = d(c("2016-06-15", "2016-06-20", "2016-06-20")))
  )
  cond <- do.call(rbind, cond_rows)
  condition_occurrence <- data.frame(
    condition_occurrence_id = seq_len(nrow(cond)),
    person_id = cond$person_id,
    condition_concept_id = cond$concept,
    condition_start_date = cond$date,
    condition_end_date = cond$date + 2L,
    condition_type_concept_id = 32020L,
    visit_occurrence_id = NA_integer_,
    stringsAsFactors = FALSE
  )

  drug_exposure <- data.frame(
    drug_exposure_id = 1:36,
    person_id = c(1:20, 10:25),
    drug_concept_id = c(rep(1503297L, 20), rep(1308216L, 16)),
    drug_exposure_start_date = d(c(rep("2020-02-01", 20),
                                   rep("2019-05-01", 16))),
    drug_exposure_end_date = d(c(rep("2020-03-01", 20),
                                 rep("2019-07-30", 16))),
    drug_type_concept_id = 38000177L,
    days_supply = c(rep(30L, 20), rep(90L, 16)),
    refills = c(rep(2L, 20), rep(1L, 16)),
    quantity = c(rep(60, 20), rep(90, 16)),
    visit_occurrence_id = NA_integer_,
    stringsAsFactors = FALSE
  )

  procedure_occurrence <- data.frame(
    procedure_occurrence_id = 1:29,
    person_id = c(1:15, 5:18),
    procedure_concept_id = c(rep(4301351L, 15), rep(4019824L, 14)),
    procedure_date = d(c(rep("2020-04-05", 15), rep("2021-01-20", 14))),
    procedure_end_date = d(c(rep("2020-04-05", 15), rep("2021-01-20", 14))),
    procedure_type_concept_id = 32817L,
    visit_occurrence_id = NA_integer_,
    stringsAsFactors = FALSE
  )

  hba1c <- c(rep(3.9, 5), 4.0, 4.0, rep(5.0, 5), 5.6, 5.6, rep(6.5, 6))
  measurement <- data.frame(
    measurement_id = 1:37,
    person_id = c(1:20, 21:23, 24:25, 1:12),
    measurement_concept_id = c(rep(3004410L, 25), rep(3025315L, 12)),
    measurement_date = d(c(sprintf("2020-%02d-10", ((1:20) %% 6L) + 1L),
                           rep("2020-08-10", 5), rep("2020-09-01", 12))),
    measurement_type_concept_id = 32817L,
    value_as_number = c(hba1c, rep(5.0, 5), 70 + (1:12)),
    value_as_concept_id = NA_integer_,
    unit_concept_id = c(rep(8554L, 20), rep(NA_integer_, 3),
                        rep(8554L, 2), rep(9529L, 12)),
    range_low = c(rep(4.0, 23), NA, NA, rep(NA, 12)),
    range_high = c(rep(5.6, 23), 5.6, 5.6, rep(NA, 12)),
    visit_occurrence_id = NA_integer_,
    stringsAsFactors = FALSE
  )

  observation <- data.frame(
    observation_id = 1:12, person_id = 1:12,
    observation_concept_id = 4005823L,
    observation_date = d("2020-05-20"),
    observation_type_concept_id = 32817L,
    value_as_number = NA_real_,
    value_as_string = NA_character_,
    value_as_concept_id = NA_integer_,
    visit_occurrence_id = NA_integer_,
    stringsAsFactors = FALSE
  )

  visit_occurrence <- data.frame(
    visit_occurrence_id = 1:55,
    person_id = c(1:40, 1:15),
    visit_concept_id = c(rep(9202L, 40), rep(9201L, 15)),
    visit_start_date = d(c(rep("2020-03-03", 40), rep("2020-01-10", 15))),
    visit_end_date = d(c(rep("2020-03-03", 40), rep("2020-01-15", 15))),
    visit_type_concept_id = 32817L,
    stringsAsFactors = FALSE
  )

  device_exposure <- data.frame(
    device_exposure_id = 1:24,
    person_id = c(1:10, 1:6, 5:12),
    device_concept_id = c(rep(4206863L, 16), rep(4044008L, 8)),
    device_exposure_start_date = d(c(rep("2020-06-10", 10),
                                     rep("2020-07-15", 6),
                                     rep("2020-06-20", 8))),
    device_exposure_end_date = d(c(rep("2020-06-10", 10),
                                   rep("2020-07-15", 6),
                                   rep("2020-06-20", 8))),
    device_type_concept_id = 32817L,
    stringsAsFactors = FALSE
  )

  death <- data.frame(
    person_id = c(33L, 34L, 36L, 37L, 38L, 35L, 40L),
    death_date = d(c(rep("2022-06-15", 5), "2023-01-15", "2022-07-01")),
    death_datetime = as.POSIXct(NA, tz = "UTC"),
    death_type_concept_id = c(rep(32817L, 6), NA_integer_),
    cause_concept_id = 320128L,
    cause_source_value = NA_character_,
    cause_source_concept_id = NA_integer_,
    stringsAsFactors = FALSE
  )

  vocab <- data.frame(
    concept_id = c(8507L, 8532L, 8527L, 8516L, 38003564L, 38003563L,
                   201820L, 320128L, 1503297L, 1308216L, 4301351L, 4019824L,
                   3004410L, 3025315L, 8554L, 9529L, 4005823L, 9201L, 9202L,
                   4206863L, 4044008L, 32817L, 32020L, 38000177L, 44814724L),
    concept_name = c("MALE", "FEMALE", "White", "Black or African American",
                     "Not Hispanic or Latino", "Hispanic or Latino",
                     "Type 2 diabetes mellitus", "Essential hypertension",
                     "metformin", "lisinopril", "Surgical procedure",
                     "Imaging of lung", "Hemoglobin A1c", "Body weight",
                     "percent", "kilogram", "Tobacco user",
                     "Inpatient Visit", "Outpatient Visit", "Insulin pump",
                     "Urinary catheter", "EHR", "EHR encounter diagnosis",
                     "Prescription written",
                     "Period covering healthcare encounters"),
    domain_id = c("Gender", "Gender", "Race", "Race", "Ethnicity",
                  "Ethnicity", "Condition", "Condition", "Drug", "Drug",
                  "Procedure", "Procedure", "Measurement", "Measurement",
                  "Unit", "Unit", "Observation", "Visit", "Visit", "Device",
                  "Device", "Type Concept", "Type Concept", "Type Concept",
                  "Type Concept"),
    vocabulary_id = "Test",
    concept_class_id = "Test",
    standard_concept = "S",
    concept_code = "x",
    valid_start_date = d("1970-01-01"),
    valid_end_date = d("2099-12-31"),
    invalid_reason = NA_character_,
    stringsAsFactors = FALSE
  )
  vocab$concept_code <- as.character(vocab$concept_id)
  clinical <- c(201820L, 320128L, 1503297L, 1308216L, 4301351L, 4019824L,
                3004410L, 3025315L, 4005823L, 9201L, 9202L, 4206863L,
                4044008L)
  concept_ancestor <- data.frame(
    ancestor_concept_id = clinical, descendant_concept_id = clinical,
    min_levels_of_separation = 0L, max_levels_of_separation = 0L
  )

  cdm_source <- data.frame(
    cdm_source_name = "Achilles live fixture",
    cdm_source_abbreviation = "ACH_LIVE",
    cdm_version = "5.4", vocabulary_version = "test",
    stringsAsFactors = FALSE
  )

  # Precomputed reader rows (upstream-shaped) for the G-03 reader checks.
  ar <- function(id, s1, s2 = NA_character_, s3 = NA_character_, n) {
    data.frame(analysis_id = as.integer(id), stratum_1 = s1, stratum_2 = s2,
               stratum_3 = s3, stratum_4 = NA_character_,
               stratum_5 = NA_character_, count_value = as.integer(n),
               stringsAsFactors = FALSE)
  }
  achilles_results <- rbind(
    ar(420, "201606", n = 11),
    ar(420, "202001", n = 5),
    ar(2102, "4206863", "202006", n = 10),
    ar(2102, "4044008", "202006", n = 8),
    ar(2102, "4206863", "202007", n = 6),
    ar(1818, "3004410", "8554", "Below Range Low", n = 5),
    ar(1818, "3004410", "8554", "Within Range", n = 9),
    ar(1818, "3004410", "8554", "Above Range High", n = 6),
    ar(1818, "3004410", NA_character_, "Within Range", n = 3),
    ar(505, "32817", n = 5),
    ar(505, NA_character_, n = 1)
  )

  list(
    cdm = list(cdm_source = cdm_source, person = person,
               observation_period = op,
               condition_occurrence = condition_occurrence,
               drug_exposure = drug_exposure,
               procedure_occurrence = procedure_occurrence,
               measurement = measurement, observation = observation,
               visit_occurrence = visit_occurrence,
               device_exposure = device_exposure, death = death),
    vocab = list(concept = vocab, concept_ancestor = concept_ancestor),
    results = list(achilles_results = achilles_results)
  )
}

# SQL type of an R column for one engine.
.achillesLiveSqlType <- function(column, value, dbms) {
  integer_like <- is.integer(value)
  id_like <- grepl("(^|_)id$", column) && integer_like
  if (inherits(value, "POSIXct")) {
    return(switch(dbms, sqlite = "TEXT", mysql = , mariadb = "DATETIME",
                  "TIMESTAMP"))
  }
  if (inherits(value, "Date")) {
    return(if (identical(dbms, "sqlite")) "TEXT" else "DATE")
  }
  if (id_like) return(if (identical(dbms, "sqlite")) "INTEGER" else "BIGINT")
  if (integer_like) return("INTEGER")
  if (is.numeric(value)) {
    return(switch(dbms, sqlite = "REAL", postgresql = "DOUBLE PRECISION",
                  "DOUBLE"))
  }
  if (identical(dbms, "sqlite")) "TEXT" else "VARCHAR(255)"
}

.achillesLiveLiteral <- function(value) {
  if (is.na(value)) return("NULL")
  if (inherits(value, "Date")) return(paste0("'", format(value, "%Y-%m-%d"),
                                             "'"))
  if (inherits(value, "POSIXct")) {
    return(paste0("'", format(value, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "'"))
  }
  if (is.numeric(value)) return(format(value, scientific = FALSE, trim = TRUE))
  paste0("'", gsub("'", "''", as.character(value), fixed = TRUE), "'")
}

# CREATE TABLE + INSERT statements of the fixture for one engine. `schemas`
# names the cdm/vocab/results namespaces (NULL for a single-schema engine).
.achillesLiveStatements <- function(frames, dbms,
                                    schemas = list(cdm = NULL, vocab = NULL,
                                                   results = NULL)) {
  out <- character(0)
  for (group in names(frames)) {
    schema <- schemas[[group]]
    for (table in names(frames[[group]])) {
      df <- frames[[group]][[table]]
      qualified <- if (is.null(schema)) table else paste0(schema, ".", table)
      types <- vapply(names(df), function(col) {
        .achillesLiveSqlType(col, df[[col]], dbms)
      }, character(1L))
      out <- c(out, paste0("CREATE TABLE ", qualified, " (",
                           paste(names(df), types, collapse = ", "), ")"))
      rows <- vapply(seq_len(nrow(df)), function(i) {
        paste0("(", paste(vapply(names(df), function(col) {
          .achillesLiveLiteral(df[[col]][i])
        }, character(1L)), collapse = ", "), ")")
      }, character(1L))
      for (chunk in split(rows, ceiling(seq_along(rows) / 50L))) {
        out <- c(out, paste0("INSERT INTO ", qualified, " (",
                             paste(names(df), collapse = ", "), ") VALUES ",
                             paste(chunk, collapse = ", ")))
      }
    }
  }
  out
}

# Exact multisets the Achilles person analyses must return (thresholds 1).
.achillesLivePersonExpected <- function(frames) {
  person <- frames$cdm$person
  vocab <- frames$vocab$concept
  name_of <- function(id) vocab$concept_name[match(id, vocab$concept_id)]
  count_by <- function(cols) {
    key <- do.call(paste, c(lapply(cols, function(col) person[[col]]),
                            list(sep = "\r")))
    tab <- table(key)
    parts <- strsplit(names(tab), "\r", fixed = TRUE)
    out <- as.data.frame(stats::setNames(lapply(seq_along(cols), function(j) {
      as.integer(vapply(parts, `[[`, character(1L), j))
    }), cols))
    out$count_value <- as.numeric(tab)
    out
  }
  list(
    "1" = data.frame(count_value = nrow(person)),
    "2" = transform(count_by("gender_concept_id"),
                    gender_name = name_of(gender_concept_id)),
    "3" = count_by("year_of_birth"),
    "4" = transform(count_by("race_concept_id"),
                    race_name = name_of(race_concept_id)),
    "5" = transform(count_by("ethnicity_concept_id"),
                    ethnicity_name = name_of(ethnicity_concept_id)),
    "10" = transform(count_by(c("year_of_birth", "gender_concept_id")),
                     gender_name = name_of(gender_concept_id)),
    "12" = transform(count_by(c("race_concept_id", "ethnicity_concept_id")),
                     race_name = name_of(race_concept_id),
                     ethnicity_name = name_of(ethnicity_concept_id))
  )
}

# Canonical (sorted, typed) form of a result frame for multiset comparison:
# databases return ties in arbitrary order.
.achillesLiveCanonical <- function(df, columns) {
  df <- as.data.frame(df)[, columns, drop = FALSE]
  for (col in columns) {
    df[[col]] <- if (grepl("_name$", col)) as.character(df[[col]]) else
      as.numeric(df[[col]])
  }
  ord <- do.call(order, c(unname(as.list(df)),
                          list(na.last = TRUE, method = "radix")))
  df <- df[ord, , drop = FALSE]
  rownames(df) <- NULL
  df
}

# Run EVERY live Achilles catalogue entry on `handle` through the real run path
# with thresholds of 1 (so every result-processing branch executes), failing
# with the entry and the last SQL it sent when one errors. Then check the
# grouped person analyses (G-16: 2/4/5/10/12, controls 1/3) against the exact
# multisets of the fixture. Returns the row count per entry.
.expectAchillesLiveCatalogue <- function(handle, label,
                                         frames = .achillesLiveFrames()) {
  registry <- suppressWarnings(.omopAnalysisRegistry(handle, force = TRUE))
  ids <- sort(grep("^dsomop:achilles\\.", names(registry), value = TRUE))
  testthat::expect_length(ids, 70L)
  last_sql <- NULL
  original <- .executeQuery
  testthat::local_mocked_bindings(
    .executeQuery = function(handle, sql) {
      last_sql <<- sql
      original(handle, sql)
    },
    .package = "dsOMOP"
  )
  withr::local_options(list(
    nfilter.subset = 1, nfilter.tab = 1, dsomop.nfilter.band = 1,
    dsomop.nfilter.dist = 1
  ))
  rows <- stats::setNames(rep(NA_integer_, length(ids)), ids)
  outputs <- list()
  for (id in ids) {
    last_sql <- NULL
    out <- tryCatch(
      suppressWarnings(suppressMessages(.omopAnalysisRun(handle, id))),
      error = function(e) e
    )
    if (inherits(out, "error")) {
      testthat::fail(paste0(label, " ", id, " failed: ",
                            conditionMessage(out), "\nlast SQL: ",
                            last_sql %||% "<none>"))
      next
    }
    testthat::expect_s3_class(out, "data.frame")
    rows[[id]] <- nrow(out)
    outputs[[id]] <- out
  }

  expected <- .achillesLivePersonExpected(frames)
  for (aid in names(expected)) {
    id <- paste0("dsomop:achilles.", aid)
    got <- outputs[[id]]
    if (is.null(got)) next
    want <- expected[[aid]]
    columns <- names(want)
    testthat::expect_true(all(columns %in% names(got)),
                          info = paste(label, id, "columns"))
    if (!all(columns %in% names(got))) next
    testthat::expect_equal(
      .achillesLiveCanonical(got, columns),
      .achillesLiveCanonical(want, columns),
      info = paste(label, id, "grouped multiset")
    )
  }
  # A missing label still forms its own group (concept 0 gender, 8515 race).
  gender <- outputs[["dsomop:achilles.2"]]
  if (!is.null(gender)) {
    testthat::expect_true(any(as.numeric(gender$gender_concept_id) == 0 &
                                is.na(gender$gender_name)),
                          info = paste(label, "missing gender label"))
  }
  # Every populated domain answers: the entries below must return rows.
  must_have_rows <- paste0("dsomop:achilles.", c(
    1, 2, 3, 4, 5, 10, 12, 101, 102, 103, 104, 105, 106, 107, 108, 109, 113,
    200, 201, 202, 203, 206, 220, 221, 400, 401, 402, 403, 406, 420, 500,
    502, 505, 506, 600, 601, 602, 603, 606, 620, 700, 701, 702, 703, 706,
    715, 716, 717, 720, 800, 801, 802, 803, 806, 820, 1800, 1801, 1802, 1803,
    1806, 1818, 1820, 2000, 2001, 2002, 2003, 2100, 2101, 2102, 2106
  ))
  for (id in intersect(must_have_rows, ids)) {
    testthat::expect_gt(rows[[id]] %||% 0L, 0L, label = paste(label, id))
  }
  invisible(rows)
}

# G-03 on a real engine: the precomputed Achilles reader serves exactly the
# cells whose upstream person basis clears max(nfilter.tab, nfilter.subset),
# with default thresholds.
.expectAchillesLiveReader <- function(handle, label) {
  withr::local_options(list(nfilter.subset = 3, nfilter.tab = 3,
                            dsomop.nfilter.band = 5))
  # The 2016-06 cell: 11 in-period records from persons 1 and 2; persons 39
  # and 40 have records that month outside every observation period, and
  # person 1's overlapping period must not count them twice.
  companion <- .achillesCompanionPersonCounts(handle, 420L)
  testthat::expect_equal(
    as.numeric(companion$n_persons[companion$stratum_1 == "201606"]), 2,
    info = paste(label, "420 in-period person basis")
  )
  testthat::expect_equal(
    as.numeric(companion$n_persons[companion$stratum_1 == "202001"]), 5,
    info = paste(label, "420 supported month")
  )
  res <- .achillesGetResults(handle, c(420L, 505L, 1818L, 2102L))
  key <- paste(res$analysis_id, res$stratum_1, res$stratum_2, res$stratum_3,
               sep = "|")
  released <- stats::setNames(as.numeric(res$count_value), key)
  expected <- c(
    "420|202001|NA|NA" = 5,
    "505|32817|NA|NA" = 5,
    "1818|3004410|8554|Below Range Low" = 5,
    "1818|3004410|8554|Within Range" = 5,
    "1818|3004410|8554|Above Range High" = 5,
    "2102|4044008|202006|NA" = 5,
    "2102|4206863|202006|NA" = 10,
    "2102|4206863|202007|NA" = 5
  )
  testthat::expect_setequal(names(released), names(expected))
  testthat::expect_equal(released[names(expected)], expected,
                         info = paste(label, "reader cells"))
  invisible(res)
}
