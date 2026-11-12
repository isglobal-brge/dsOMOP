# WS-0 (design-2.10.3 section 10.2): the adversarial extension of the reach fixture.

.ws0_reach_path <- function(name) {
  tp <- tryCatch(testthat::test_path(), error = function(e) ".")
  Find(file.exists, c(file.path(tp, "..", "fixtures", name), file.path("tests", "fixtures", name)))
}

test_that("the extension appends the documented persons and tables without touching 1..100", {
  skip_if_not_installed("RSQLite")
  src <- .ws0_reach_path("create_reach_db.R"); ext <- .ws0_reach_path("extend_reach_db.R")
  skip_if(is.null(src) || is.null(ext), "reach fixture files not found")
  source(src, local = TRUE); source(ext, local = TRUE)
  h <- build_reach_handle(100L)
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  before <- list(
    persons = DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM person WHERE person_id <= 100")$n,
    cond = DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM condition_occurrence WHERE person_id <= 100")$n,
    drug = DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM drug_exposure WHERE person_id <= 100")$n,
    meas = DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM measurement WHERE person_id <= 100")$n,
    c1 = DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM cohort WHERE cohort_definition_id = 1 AND subject_id <= 100")$n)
  expect_false("cost" %in% DBI::dbListTables(h$conn))
  extend_reach_handle(h)
  after <- list(
    persons = DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM person WHERE person_id <= 100")$n,
    cond = DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM condition_occurrence WHERE person_id <= 100")$n,
    drug = DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM drug_exposure WHERE person_id <= 100")$n,
    meas = DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM measurement WHERE person_id <= 100")$n,
    c1 = DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM cohort WHERE cohort_definition_id = 1 AND subject_id <= 100")$n)
  expect_identical(after, before)
  expect_identical(DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM person WHERE person_id BETWEEN 1001 AND 1032")$n, 32L)
  expect_true("cost" %in% DBI::dbListTables(h$conn))
  expect_true(all(c("cost_event_id", "cost_domain_id", "currency_concept_id", "total_cost") %in% DBI::dbListFields(h$conn, "cost")))
  # scenario checks
  ops <- function(pid) DBI::dbGetQuery(h$conn, paste0("SELECT observation_period_start_date AS s, observation_period_end_date AS e FROM observation_period WHERE person_id = ", pid, " ORDER BY s"))
  expect_identical(nrow(ops(1001L)), 2L)                                   # overlapping periods
  expect_identical(as.integer(as.Date("2020-01-01") - as.Date(ops(1002L)$e[1])) - 1L, 7L)   # exactly 7 uncovered days
  expect_identical(ops(1003L)$e[1], "2020-01-02"); expect_identical(ops(1003L)$s[2], "2020-01-09")
  fills <- DBI::dbGetQuery(h$conn, "SELECT drug_exposure_start_date AS s, drug_exposure_end_date AS e, days_supply FROM drug_exposure WHERE person_id = 1004 ORDER BY s")
  expect_identical(nrow(fills), 2L); expect_true(all(is.na(fills$e))); expect_identical(fills$days_supply, c(30L, 30L))
  dq <- DBI::dbGetQuery(h$conn, "SELECT drug_exposure_start_date AS s, drug_exposure_end_date AS e, days_supply FROM drug_exposure WHERE person_id = 1005 ORDER BY s")
  expect_true(is.na(dq$e[1]) && is.na(dq$days_supply[1])); expect_true(dq$e[2] < dq$s[2])
  a1c <- DBI::dbGetQuery(h$conn, "SELECT unit_concept_id AS u, operator_concept_id AS op, value_as_number AS v FROM measurement WHERE person_id = 1006 AND measurement_concept_id = 3004410")
  expect_true(any(a1c$u == 8753L, na.rm = TRUE) && any(is.na(a1c$u)) && any(a1c$op == 4171756L) && any(is.na(a1c$v)))
  expect_identical(DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM condition_occurrence WHERE person_id = 1007 AND condition_concept_id = 4329847 AND condition_start_date = '2020-01-01'")$n, 2L)
  expect_true(is.na(DBI::dbGetQuery(h$conn, "SELECT year_of_birth FROM person WHERE person_id = 1009")$year_of_birth))
  expect_identical(DBI::dbGetQuery(h$conn, "SELECT year_of_birth FROM person WHERE person_id = 1010")$year_of_birth, 2030L)
  expect_identical(DBI::dbGetQuery(h$conn, "SELECT death_date FROM death WHERE person_id = 1011")$death_date, "2019-06-01")
  expect_identical(sort(DBI::dbGetQuery(h$conn, "SELECT DISTINCT person_id FROM condition_occurrence WHERE condition_concept_id = 4000777")$person_id), c(1013L, 1014L))
  links <- DBI::dbGetQuery(h$conn, "SELECT event_id, COUNT(*) AS n FROM episode_event GROUP BY event_id HAVING COUNT(*) > 1")
  expect_identical(nrow(links), 1L)                                        # one fill linked to two episodes
  expect_gte(DBI::dbGetQuery(h$conn, "SELECT COUNT(DISTINCT person_id) AS n FROM episode WHERE episode_concept_id = 32531")$n, 5L)
  cur <- DBI::dbGetQuery(h$conn, "SELECT DISTINCT currency_concept_id FROM cost")$currency_concept_id
  expect_setequal(cur, c(44818668L, 44818568L))                           # USD and EUR by their real ids (B6)
  expect_identical(DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM cost WHERE currency_concept_id = 44818668")$n, 4L)
  vocab <- DBI::dbGetQuery(h$conn, "SELECT concept_id, concept_name, vocabulary_id, concept_class_id, concept_code FROM concept WHERE concept_id IN (44818668, 44818568, 1147094, 1147082, 4171756)")
  expect_identical(vocab$concept_code[vocab$concept_id == 44818668L], "USD")
  expect_identical(vocab$concept_code[vocab$concept_id == 44818568L], "EUR")
  expect_true(all(vocab$vocabulary_id[vocab$concept_id %in% c(1147094L, 1147082L)] == "CDM"))
  expect_true(all(vocab$concept_class_id[vocab$concept_id %in% c(1147094L, 1147082L)] == "Field"))
  expect_identical(vocab$concept_name[vocab$concept_id == 1147094L], "drug_exposure.drug_exposure_id")
  # active-comparator scenario persons and the closest-value tie rows
  expect_identical(DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM drug_era WHERE person_id = 1031 AND drug_concept_id = 1308216")$n, 2L)
  expect_identical(DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM drug_era WHERE person_id = 1032 AND drug_concept_id = 1308216")$n, 2L)
  expect_identical(DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM drug_era WHERE person_id = 1032 AND drug_concept_id = 1367500")$n, 1L)
  expect_identical(DBI::dbGetQuery(h$conn, "SELECT observation_period_start_date AS s FROM observation_period WHERE person_id = 1031")$s, "2019-01-01")
  expect_identical(DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM measurement WHERE person_id = 1006 AND measurement_date = '2019-12-20'")$n, 2L)
  expect_identical(DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM episode_event ee JOIN episode e ON e.episode_id = ee.episode_id JOIN drug_exposure d ON d.drug_exposure_id = ee.event_id WHERE ee.episode_event_field_concept_id = 1147094 AND d.person_id <> e.person_id")$n, 0L)
  # every acceptance cohort has at least nfilter.subset = 5 persons
  for (cid in c(1L, 1001L, 1002L)) {
    expect_gte(DBI::dbGetQuery(h$conn, paste0("SELECT COUNT(DISTINCT subject_id) AS n FROM cohort WHERE cohort_definition_id = ", cid))$n, 5L)
  }
})

test_that("with_defects adds the person-mismatch link only", {
  skip_if_not_installed("RSQLite")
  src <- .ws0_reach_path("create_reach_db.R"); ext <- .ws0_reach_path("extend_reach_db.R")
  skip_if(is.null(src) || is.null(ext), "reach fixture files not found")
  source(src, local = TRUE); source(ext, local = TRUE)
  h <- build_extended_reach_handle(100L, with_defects = TRUE)
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  expect_identical(DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM episode_event ee JOIN episode e ON e.episode_id = ee.episode_id JOIN drug_exposure d ON d.drug_exposure_id = ee.event_id WHERE ee.episode_event_field_concept_id = 1147094 AND d.person_id <> e.person_id")$n, 1L)
})

test_that("the synthetic CDM generator is deterministic and GiBleed-shaped", {
  gen <- .ws0_reach_path("generate_synthetic_cdm.R")
  skip_if(is.null(gen), "generator not found")
  source(gen, local = TRUE)
  csv <- Find(file.exists, c(system.file("ohdsi", "OMOP_CDMv5.3_Field_Level.csv", package = "dsOMOP"),
                             "inst/ohdsi/OMOP_CDMv5.3_Field_Level.csv", "../../inst/ohdsi/OMOP_CDMv5.3_Field_Level.csv"))
  a <- generate_synthetic_cdm(200L, field_level_csv = csv)
  b <- generate_synthetic_cdm(200L, field_level_csv = csv)
  expect_identical(a, b)
  fields <- read.csv(csv, stringsAsFactors = FALSE)
  for (tbl in c("person", "observation_period", "visit_occurrence", "condition_occurrence", "drug_exposure",
                "measurement", "death", "drug_era", "condition_era", "cdm_source", "concept")) {
    expect_identical(names(a[[tbl]]), tolower(fields$cdmFieldName[fields$cdmTableName == toupper(tbl)]), info = tbl)
  }
  expect_identical(nrow(a$person), 200L)
  expect_true(nrow(a$condition_occurrence) > 200L)
  expect_true(all(a$observation_period$observation_period_end_date >= a$observation_period$observation_period_start_date))
  expect_true(all(a$drug_era$drug_era_end_date >= a$drug_era$drug_era_start_date))
  skip_if_not_installed("RSQLite")
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  generate_synthetic_cdm(50L, field_level_csv = csv, conn = conn)
  expect_true(all(c("person", "cdm_source", "drug_era") %in% DBI::dbListTables(conn)))
  expect_identical(DBI::dbGetQuery(conn, "SELECT cdm_version FROM cdm_source")$cdm_version, "v5.3.1")
})
