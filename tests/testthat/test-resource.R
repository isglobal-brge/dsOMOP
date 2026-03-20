# --- Resource URL B64 Parsing Tests ---

test_that("B64 resource URL round-trips correctly", {
  # Simulate what resource.js produces
  config <- list(
    dbms = "postgresql", host = "db.hospital.org",
    port = 5432L, database = "omop_cdm",
    cdm_schema = "cdm", vocabulary_schema = "vocab"
  )
  json <- as.character(jsonlite::toJSON(config, auto_unbox = TRUE))
  b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json)))
  b64 <- gsub("+", "-", b64, fixed = TRUE)
  b64 <- gsub("/", "_", b64, fixed = TRUE)
  b64 <- gsub("=+$", "", b64)
  url <- paste0("omop+dbi:///B64:", b64)

  # Parse using the same logic as resource.R
  body <- sub("^omop[+]dbi:///", "", url)
  expect_true(startsWith(body, "B64:"))
  b64r <- substring(body, 5)
  b64r <- gsub("-", "+", b64r, fixed = TRUE)
  b64r <- gsub("_", "/", b64r, fixed = TRUE)
  pad <- (4 - nchar(b64r) %% 4) %% 4
  if (pad > 0) b64r <- paste0(b64r, strrep("=", pad))
  params <- jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(b64r)),
                                simplifyVector = FALSE)

  expect_equal(params$dbms, "postgresql")
  expect_equal(params$host, "db.hospital.org")
  expect_equal(params$port, 5432)
  expect_equal(params$database, "omop_cdm")
  expect_equal(params$cdm_schema, "cdm")
  expect_equal(params$vocabulary_schema, "vocab")
})

test_that("B64 URL contains only parser-safe characters", {
  config <- list(dbms = "sql_server", host = "10.0.0.1",
                 port = 1433L, database = "my_db",
                 cdm_schema = "dbo", results_schema = "results")
  json <- as.character(jsonlite::toJSON(config, auto_unbox = TRUE))
  b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json)))
  b64 <- gsub("+", "-", b64, fixed = TRUE)
  b64 <- gsub("/", "_", b64, fixed = TRUE)
  b64 <- gsub("=+$", "", b64)
  url <- paste0("omop+dbi:///B64:", b64)

  # Must not contain ?, &, +, or = (Opal parser-unsafe)
  body <- sub("^omop[+]dbi:///B64:", "", url)
  expect_false(grepl("[?&+=]", body))
  # Only alphanumeric, -, _
  expect_true(grepl("^[A-Za-z0-9_-]+$", body))
})

test_that("B64 URL handles special chars in schema names", {
  config <- list(dbms = "postgresql", host = "db.example.com",
                 port = 5432L, database = "prod",
                 cdm_schema = "omop-v5.4_data")
  json <- as.character(jsonlite::toJSON(config, auto_unbox = TRUE))
  b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json)))
  b64 <- gsub("+", "-", b64, fixed = TRUE)
  b64 <- gsub("/", "_", b64, fixed = TRUE)
  b64 <- gsub("=+$", "", b64)

  # Decode back
  b64r <- gsub("-", "+", b64, fixed = TRUE)
  b64r <- gsub("_", "/", b64r, fixed = TRUE)
  pad <- (4 - nchar(b64r) %% 4) %% 4
  if (pad > 0) b64r <- paste0(b64r, strrep("=", pad))
  params <- jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(b64r)),
                                simplifyVector = FALSE)

  expect_equal(params$cdm_schema, "omop-v5.4_data")
})

# --- DATEADD translation for MySQL ---

test_that(".sql_translate converts DATEADD for mysql", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "mysql")
  expect_true(grepl("DATE_ADD", sql))
  expect_true(grepl("INTERVAL 30 DAY", sql))
  expect_false(grepl("DATEADD", sql))
})

test_that(".sql_translate converts DATEADD with negative days for mysql", {
  sql <- .sql_translate("DATEADD(day, -7, end_date)", "mysql")
  expect_true(grepl("DATE_ADD", sql))
  expect_true(grepl("INTERVAL -7 DAY", sql))
})

# --- TOP/LIMIT translation for MySQL and Oracle ---

test_that(".sql_translate converts TOP to LIMIT for mysql", {
  sql <- .sql_translate("SELECT TOP 10 * FROM person", "mysql")
  expect_true(grepl("LIMIT 10", sql))
  expect_false(grepl("TOP", sql))
})

test_that(".sql_translate converts TOP to FETCH FIRST for oracle", {
  sql <- .sql_translate("SELECT TOP 10 * FROM person", "oracle")
  expect_true(grepl("FETCH FIRST 10 ROWS ONLY", sql))
  expect_false(grepl("TOP", sql))
  expect_false(grepl("LIMIT", sql))
})

# --- Dialect mapping completeness ---

test_that("all DBMS in resource.js enum are mapped in dialect resolver", {
  # These match the enum in resource.js
  dbms_list <- c("postgresql", "sql_server", "oracle", "redshift",
                 "bigquery", "snowflake", "spark", "databricks",
                 "duckdb", "sqlite")
  for (dbms in dbms_list) {
    expect_silent(.resolve_target_dialect(dbms))
  }
})

test_that("duckdb maps to sqlite dialect", {
  expect_equal(.resolve_target_dialect("duckdb"), "sqlite")
})

test_that("databricks maps to spark dialect", {
  expect_equal(.resolve_target_dialect("databricks"), "spark")
})

test_that("mysql and mariadb are mapped in dialect resolver", {
  expect_equal(.resolve_target_dialect("mysql"), "mysql")
  expect_equal(.resolve_target_dialect("mariadb"), "mysql")
})
