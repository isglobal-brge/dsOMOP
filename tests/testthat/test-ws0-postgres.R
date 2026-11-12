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
  pgh <- load_reach_fixture_pg(pg, schema = schema, handle = h)
  on.exit(DBI::dbExecute(pg, paste0("DROP SCHEMA IF EXISTS ", schema, " CASCADE")), add = TRUE)
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
