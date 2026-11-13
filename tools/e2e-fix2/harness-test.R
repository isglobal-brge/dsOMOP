test_that("pure harness contracts and syntax hold", {
  withr::local_dir(testthat::test_path("..", ".."))
  source("scripts/tests/g-contract.R", local = TRUE)
  for (f in list.files("scripts", pattern = "[.]R$", recursive = TRUE, full.names = TRUE)) parse(f)
  expect_true(TRUE)
})

test_that("large fixture rekeys duplicate surrogate IDs without removing clinical rows", {
  sql <- readLines(testthat::test_path("..", "..", "scripts", "omopbig.sh"))
  sql <- paste(sql[seq(grep("^WITH numbered", sql), grep("^UPDATE cdm.drug_exposure", sql))], collapse = "\n")
  sql <- gsub("cdm.drug_exposure", "e2e_drugs", sql, fixed = TRUE)
  con <- DBI::dbConnect(RPostgres::Postgres(), host = "omopdb", dbname = "omop", user = "omop", password = "omop")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TEMP TABLE e2e_drugs (drug_exposure_id integer, person_id integer, quantity integer)")
  DBI::dbExecute(con, "INSERT INTO e2e_drugs VALUES (62142,1,2), (62142,1,2), (62142,2,3), (100000001,3,4)")
  DBI::dbExecute(con, sql)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n, COUNT(DISTINCT drug_exposure_id) AS ids, SUM(quantity) AS q FROM e2e_drugs"), data.frame(n=4,ids=4,q=11), ignore_attr = TRUE)
  expect_error(DBI::dbExecute(con, "ALTER TABLE e2e_drugs ADD PRIMARY KEY (drug_exposure_id)"), NA)
})
