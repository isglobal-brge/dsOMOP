# =============================================================================
# WS-0: load the (extended) reach fixture into PostgreSQL and compare it with
# the SQLite original (design-2.10.3 section 12.2, WS-0 loading criterion).
#
#   load_reach_fixture_pg(pg_conn, schema = "ws0_reach", handle = NULL)
#     copies every table of the in-memory SQLite fixture (build_reach_handle()
#     + extend_reach_handle()) into `schema` of a PostgreSQL connection, with
#     *_date columns as DATE and *_datetime columns as TIMESTAMP, and returns a
#     dsOMOP-style handle on the PostgreSQL connection.
#   reach_fixture_signature(conn, schema = NULL)
#     the backend-independent scenario/cardinality signature used to prove the
#     two copies are equivalent: row counts per table plus the fixture-scenario
#     queries of extend_reach_db.R (overlapping and gapped periods, the PDC and
#     stockpiling fills, episode links, cost currencies, cohort sizes).
# Connection on the test pod: host omopdb, database omop, user/password
# omop/omop, no TLS (tests skip when the host is unreachable).
# =============================================================================

.reach_date_columns <- function(df) {
  nm <- names(df)
  nm[grepl("_date$", nm) & vapply(df, is.character, logical(1))]
}
.reach_datetime_columns <- function(df) {
  nm <- names(df)
  nm[grepl("_datetime$", nm) & vapply(df, is.character, logical(1))]
}

load_reach_fixture_pg <- function(pg_conn, schema = "ws0_reach", handle = NULL,
                                  n_persons = 100L, with_defects = FALSE) {
  if (is.null(handle)) handle <- build_extended_reach_handle(n_persons, with_defects = with_defects)
  src <- handle$conn
  DBI::dbExecute(pg_conn, paste0("DROP SCHEMA IF EXISTS ", schema, " CASCADE"))
  DBI::dbExecute(pg_conn, paste0("CREATE SCHEMA ", schema))
  tables <- sort(DBI::dbListTables(src))
  tables <- tables[!grepl("^sqlite_", tables)]
  for (tbl in tables) {
    df <- DBI::dbGetQuery(src, paste0("SELECT * FROM ", tbl))
    for (col in .reach_date_columns(df)) df[[col]] <- as.Date(df[[col]])
    for (col in .reach_datetime_columns(df)) df[[col]] <- as.POSIXct(df[[col]], tz = "UTC")
    DBI::dbWriteTable(pg_conn, DBI::Id(schema = schema, table = tbl), df, overwrite = TRUE)
  }
  pg <- new.env(parent = emptyenv())
  pg$conn <- pg_conn; pg$dbms <- "postgresql"; pg$target_dialect <- "postgresql"
  pg$cdm_schema <- schema; pg$vocab_schema <- schema; pg$results_schema <- schema
  pg$temp_schema <- NULL; pg$resource_client <- NULL; pg$config <- list()
  pg$blueprint <- NULL; pg$temp_tables <- character(0); pg$person_key <- handle$person_key
  pg$tables_loaded <- tables
  pg
}

reach_fixture_signature <- function(conn, schema = NULL) {
  q <- function(sql) {
    if (!is.null(schema)) sql <- gsub("\\bFROM ([a-z_]+)", paste0("FROM ", schema, ".\\1"), sql, perl = TRUE)
    if (!is.null(schema)) sql <- gsub("\\bJOIN ([a-z_]+)", paste0("JOIN ", schema, ".\\1"), sql, perl = TRUE)
    DBI::dbGetQuery(conn, sql)
  }
  tables <- c("person", "observation_period", "visit_occurrence", "visit_detail", "condition_occurrence",
              "drug_exposure", "procedure_occurrence", "device_exposure", "measurement", "observation",
              "death", "specimen", "drug_era", "condition_era", "dose_era", "cohort", "cohort_definition",
              "concept", "concept_ancestor", "episode", "episode_event", "cost", "location")
  counts <- vapply(tables, function(t) as.integer(q(paste0("SELECT COUNT(*) AS n FROM ", t))$n), integer(1))
  scen <- list(
    persons_ext = as.integer(q("SELECT COUNT(*) AS n FROM person WHERE person_id >= 1001")$n),
    ops_1001 = as.integer(q("SELECT COUNT(*) AS n FROM observation_period WHERE person_id = 1001")$n),
    gap_1002 = as.integer(q("SELECT COUNT(*) AS n FROM observation_period WHERE person_id = 1002")$n),
    fills_1004_null_end = as.integer(q("SELECT COUNT(*) AS n FROM drug_exposure WHERE person_id = 1004 AND drug_exposure_end_date IS NULL")$n),
    fills_1005_bad = as.integer(q("SELECT COUNT(*) AS n FROM drug_exposure WHERE person_id = 1005 AND drug_exposure_end_date < drug_exposure_start_date")$n),
    a1c_1006 = as.integer(q("SELECT COUNT(*) AS n FROM measurement WHERE person_id = 1006 AND measurement_concept_id = 3004410")$n),
    a1c_1006_null_unit = as.integer(q("SELECT COUNT(*) AS n FROM measurement WHERE person_id = 1006 AND unit_concept_id IS NULL")$n),
    mi_1007_same_day = as.integer(q("SELECT COUNT(*) AS n FROM condition_occurrence WHERE person_id = 1007 AND condition_concept_id = 4329847")$n),
    yob_null = as.integer(q("SELECT COUNT(*) AS n FROM person WHERE year_of_birth IS NULL")$n),
    death_1011 = as.character(q("SELECT death_date FROM death WHERE person_id = 1011")$death_date),
    covar2 = sort(as.integer(q("SELECT DISTINCT person_id FROM condition_occurrence WHERE condition_concept_id = 4000777")$person_id)),
    episode_links = as.integer(q("SELECT COUNT(*) AS n FROM episode_event")$n),
    shared_link = as.integer(q("SELECT COUNT(*) AS n FROM (SELECT event_id FROM episode_event GROUP BY event_id HAVING COUNT(*) > 1) t")$n),
    cost_usd = as.integer(q("SELECT COUNT(*) AS n FROM cost WHERE currency_concept_id = 44818668")$n),
    cost_eur = as.integer(q("SELECT COUNT(*) AS n FROM cost WHERE currency_concept_id = 44818568")$n),
    cohort_sizes = {
      cs <- q("SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS n FROM cohort WHERE cohort_definition_id IN (1, 1001, 1002) GROUP BY cohort_definition_id ORDER BY cohort_definition_id")
      stats::setNames(as.integer(cs$n), as.character(cs$cohort_definition_id))
    },
    acei_eras_1032 = as.integer(q("SELECT COUNT(*) AS n FROM drug_era WHERE person_id = 1032 AND drug_concept_id = 1308216")$n),
    field_concepts = sort(as.integer(q("SELECT concept_id FROM concept WHERE vocabulary_id = 'CDM' AND concept_class_id = 'Field'")$concept_id)))
  list(counts = counts, scenarios = scen)
}
