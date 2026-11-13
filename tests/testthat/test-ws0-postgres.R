# WS-0 (design-2.10.3 section 12.2 loading criterion): the extended reach
# fixture loads into PostgreSQL and is equivalent to the SQLite original.
# Pod connection: host omopdb, database omop, user/password omop/omop, no TLS.

.ws0_pg_connect <- function() {
  if (!requireNamespace("RPostgres", quietly = TRUE)) return(NULL)
  host <- Sys.getenv("DSOMOP_TEST_PG_HOST", "omopdb")
  tryCatch(DBI::dbConnect(RPostgres::Postgres(), host = host, port = as.integer(Sys.getenv("DSOMOP_TEST_PG_PORT", "5432")),
                          dbname = Sys.getenv("DSOMOP_TEST_PG_DB", "omop"), user = Sys.getenv("DSOMOP_TEST_PG_USER", "omop"),
                          password = Sys.getenv("DSOMOP_TEST_PG_PASSWORD", "omop"), sslmode = "disable",
                          connect_timeout = 5L),
           error = function(e) NULL)
}
.ws0_fixture_file <- function(name) {
  tp <- tryCatch(testthat::test_path(), error = function(e) ".")
  Find(file.exists, c(file.path(tp, "..", "fixtures", name), file.path("tests", "fixtures", name)))
}

test_that("the extended reach fixture loads into PostgreSQL with the same scenarios and cardinalities", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("RPostgres")
  pg <- .ws0_pg_connect()
  skip_if(is.null(pg), "PostgreSQL test host not reachable")
  on.exit(DBI::dbDisconnect(pg), add = TRUE)
  for (f in c("create_reach_db.R", "extend_reach_db.R", "load_reach_pg.R")) source(.ws0_fixture_file(f), local = TRUE)
  h <- build_extended_reach_handle(100L)
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  schema <- paste0("ws0_reach_", as.integer(Sys.getpid()) %% 10000L)
  DBI::dbExecute(pg, "SET client_min_messages TO WARNING")
  pgh <- load_reach_fixture_pg(pg, schema = schema, handle = h)
  on.exit(try(DBI::dbExecute(pg, paste0("DROP SCHEMA ", schema, " CASCADE")), silent = TRUE), add = TRUE)
  expect_identical(pgh$dbms, "postgresql")
  expect_true(all(c("person", "cost", "episode_event", "drug_era") %in% pgh$tables_loaded))
  # typed dates on PostgreSQL
  types <- DBI::dbGetQuery(pg, paste0("SELECT column_name, data_type FROM information_schema.columns WHERE table_schema = '", schema,
                                      "' AND table_name = 'observation_period'"))
  expect_identical(types$data_type[types$column_name == "observation_period_start_date"], "date")
  # equivalent scenario/cardinality signature on both backends
  sig_sqlite <- reach_fixture_signature(h$conn)
  sig_pg <- reach_fixture_signature(pg, schema = schema)
  expect_identical(sig_pg$counts, sig_sqlite$counts)
  expect_identical(sig_pg$scenarios, sig_sqlite$scenarios)
  # a date comparison that only works on real DATE columns
  n <- DBI::dbGetQuery(pg, paste0("SELECT COUNT(*) AS n FROM ", schema, ".drug_exposure WHERE drug_exposure_end_date < drug_exposure_start_date"))$n
  expect_identical(as.integer(n), 1L)
  # the blueprint discovers the PostgreSQL schema
  bp <- dsOMOP:::.buildBlueprint(pgh)
  expect_true(all(c("person", "cost", "episode_event") %in% bp$tables$table_name[bp$tables$present_in_db]))
})


test_that("the synthetic GiBleed-shaped CDM loads into PostgreSQL with typed dates and identical cardinalities", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("RPostgres")
  pg <- .ws0_pg_connect()
  skip_if(is.null(pg), "PostgreSQL test host not reachable")
  on.exit(DBI::dbDisconnect(pg), add = TRUE)
  source(.ws0_fixture_file("generate_synthetic_cdm.R"), local = TRUE)
  csv <- Find(file.exists, c(system.file("ohdsi", "OMOP_CDMv5.3_Field_Level.csv", package = "dsOMOP"),
                             "inst/ohdsi/OMOP_CDMv5.3_Field_Level.csv", "../../inst/ohdsi/OMOP_CDMv5.3_Field_Level.csv"))
  n <- as.integer(Sys.getenv("DSOMOP_TEST_SYNTHETIC_N", "500"))
  schema <- paste0("ws0_synth_", as.integer(Sys.getpid()) %% 10000L)
  DBI::dbExecute(pg, "SET client_min_messages TO WARNING")
  if (as.integer(DBI::dbGetQuery(pg, paste0("SELECT COUNT(*) AS n FROM information_schema.schemata WHERE schema_name = '", schema, "'"))$n) > 0L) {
    DBI::dbExecute(pg, paste0("DROP SCHEMA ", schema, " CASCADE"))
  }
  DBI::dbExecute(pg, paste0("CREATE SCHEMA ", schema))
  on.exit(try(DBI::dbExecute(pg, paste0("DROP SCHEMA ", schema, " CASCADE")), silent = TRUE), add = TRUE)
  cdm <- generate_synthetic_cdm(n, field_level_csv = csv, conn = pg, schema = schema)
  lite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(lite), add = TRUE)
  generate_synthetic_cdm(n, field_level_csv = csv, conn = lite)
  # every table loaded with the full CDM 5.3 column list; dates typed on PostgreSQL
  expect_identical(nrow(cdm$person), n)
  cols <- DBI::dbGetQuery(pg, paste0("SELECT table_name, column_name, data_type FROM information_schema.columns WHERE table_schema = '", schema, "'"))
  for (tbl in names(cdm)) {
    expect_setequal(cols$column_name[cols$table_name == tbl], names(cdm[[tbl]]))
  }
  expect_identical(cols$data_type[cols$table_name == "condition_occurrence" & cols$column_name == "condition_start_date"], "date")
  expect_identical(cols$data_type[cols$table_name == "drug_exposure" & cols$column_name == "drug_exposure_end_date"], "date")
  # cardinalities and typed-date scenarios: in-memory frames == PostgreSQL == SQLite
  sig_pg <- synthetic_cdm_signature(pg, schema = schema)
  sig_lite <- synthetic_cdm_signature(lite)
  expect_identical(sig_pg$counts, vapply(names(sig_pg$counts), function(t) nrow(cdm[[t]]), integer(1)))
  expect_identical(sig_pg$counts, sig_lite$counts)
  expect_identical(sig_pg$scenarios, sig_lite$scenarios)
  expect_identical(sig_pg$scenarios$persons_dead, sum(!is.na(cdm$death$person_id)))
  expect_identical(sig_pg$scenarios$cdm_version, "v5.3.1")
  # a date comparison that needs real DATE columns on PostgreSQL
  n_bad <- DBI::dbGetQuery(pg, paste0("SELECT COUNT(*) AS n FROM ", schema, ".drug_exposure WHERE drug_exposure_end_date < drug_exposure_start_date"))$n
  expect_identical(as.integer(n_bad), 0L)
  # the blueprint discovers the synthetic schema and its CDM version
  h <- new.env(parent = emptyenv()); h$conn <- pg; h$dbms <- "postgresql"; h$target_dialect <- "postgresql"
  h$cdm_schema <- schema; h$vocab_schema <- schema; h$results_schema <- schema; h$temp_schema <- NULL
  h$resource_client <- NULL; h$config <- list(); h$blueprint <- NULL; h$temp_tables <- character(0); h$person_key <- as.raw(1:16)
  bp <- dsOMOP:::.buildBlueprint(h)
  expect_true(all(c("person", "drug_era", "cdm_source") %in% bp$tables$table_name[bp$tables$present_in_db]))
})
