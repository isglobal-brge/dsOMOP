# Public synthetic Eunomia CDM; run with Rscript tests/fixtures/numeric/load.R DIR.
# Requires DBI, RPostgres, duckdb, openssl, jsonlite. No source rows are altered.
# PGHOST/PGPORT/PGDATABASE/PGUSER/PGPASSWORD override the test-pod defaults.
# NUMERIC_POSTGRES=0 loads DuckDB only; NUMERIC_DUCKDB=0 loads PostgreSQL only.
load_numeric_cdm <- function(directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  revision <- "3d349dcdd79cc346b8e36eb837e49feffe7f7497"
  sha <- "cdd3971be325e17b0e872bf4560711ab4c346917ecb079e2faf32badb55cb693"
  url <- paste0("https://raw.githubusercontent.com/OHDSI/EunomiaDatasets/", revision,
                "/datasets/Synthea27Nj/Synthea27Nj_5.4.zip")
  archive <- file.path(directory, "Synthea27Nj_5.4.zip")
  if (!file.exists(archive)) utils::download.file(url, archive, mode = "wb", quiet = TRUE)
  actual <- local({ con <- file(archive, "rb"); on.exit(close(con)); as.character(openssl::sha256(con)) })
  if (length(actual) != 1L || is.na(actual) || actual != sha) stop("Numeric CDM archive checksum mismatch")
  csvdir <- tempfile("csv-", tmpdir = directory)
  dir.create(csvdir)
  on.exit(unlink(csvdir, recursive = TRUE), add = TRUE)
  utils::unzip(archive, exdir = csvdir)
  connections <- list()
  on.exit(for (con in connections) DBI::dbDisconnect(con), add = TRUE)
  if (Sys.getenv("NUMERIC_POSTGRES", "1") != "0") {
    connections$postgresql <- DBI::dbConnect(RPostgres::Postgres(),
      host = Sys.getenv("PGHOST", "omopdb"), port = as.integer(Sys.getenv("PGPORT", "5432")),
      dbname = Sys.getenv("PGDATABASE", "omop"), user = Sys.getenv("PGUSER", "omop"),
      password = Sys.getenv("PGPASSWORD", "omop"), sslmode = "disable")
    DBI::dbExecute(connections$postgresql, "CREATE SCHEMA IF NOT EXISTS numeric_cdm")
    DBI::dbBegin(connections$postgresql)
  }
  if (Sys.getenv("NUMERIC_DUCKDB", "1") != "0") {
    connections$duckdb <- DBI::dbConnect(duckdb::duckdb(), dbdir = file.path(directory, "numeric_cdm.duckdb"))
    DBI::dbBegin(connections$duckdb)
  }
  counts <- list()
  for (path in sort(list.files(csvdir, pattern = "[.]csv$", full.names = TRUE, ignore.case = TRUE))) {
    table <- tolower(tools::file_path_sans_ext(basename(path)))
    d <- utils::read.csv(path, colClasses = "character", na.strings = c("", "NA"), check.names = FALSE)
    names(d) <- tolower(names(d))
    # Explicit CDM scalar types, including all-NULL and header-only columns.
    # vocabulary/domain/class/relationship identifiers are strings in the CDM.
    text_ids <- c("vocabulary_id", "domain_id", "concept_class_id", "relationship_id",
                  "source_vocabulary_id", "target_vocabulary_id", "unique_device_id")
    for (col in names(d)) {
      x <- d[[col]]
      if (grepl("_date$", col)) d[[col]] <- as.Date(x)
      else if (grepl("_datetime$", col)) d[[col]] <- as.POSIXct(x, tz = "UTC")
      else if ((grepl("_id$", col) && !col %in% text_ids) || col %in%
               c("year_of_birth", "month_of_birth", "day_of_birth", "days_supply", "refills",
                 "stop_reason_concept_id", "min_levels_of_separation", "max_levels_of_separation")) {
        d[[col]] <- as.integer(x)
      } else if (col %in% c("value_as_number", "range_low", "range_high", "quantity", "numerator_value",
                            "denominator_value", "amount_value", "total_charge", "total_cost", "total_paid",
                            "paid_by_payer", "paid_by_patient", "paid_patient_copay", "paid_patient_coinsurance",
                            "paid_patient_deductible", "paid_by_primary", "paid_ingredient_cost",
                            "paid_dispensing_fee", "amount_allowed", "revenue_code_concept_id")) {
        d[[col]] <- as.numeric(x)
      }
    }
    counts[[table]] <- nrow(d)
    for (backend in names(connections)) {
      id <- if (backend == "postgresql") DBI::Id(schema = "numeric_cdm", table = table) else table
      DBI::dbWriteTable(connections[[backend]], id, d, overwrite = TRUE)
    }
    if (table == "measurement") {
      has_value <- !is.na(d$value_as_number)
      has_unit <- !is.na(d$unit_concept_id) & d$unit_concept_id != 0L
      measurements <- list(rows = nrow(d), numeric = sum(has_value), mapped_unit = sum(has_unit),
        numeric_and_mapped_unit = sum(has_value & has_unit),
        numeric_fraction = mean(has_value), mapped_unit_fraction = mean(has_unit),
        both_fraction = mean(has_value & has_unit), reference_ranges = sum(!is.na(d$range_high)))
    }
  }
  for (con in connections) DBI::dbCommit(con)
  manifest <- list(dataset = "Synthea27Nj", revision = revision, sha256 = sha, url = url,
    license = paste0("https://github.com/OHDSI/EunomiaDatasets/blob/", revision, "/LICENSE"),
    archive_bytes = unname(file.info(archive)$size), tables = counts, measurements = measurements)
  jsonlite::write_json(manifest, file.path(directory, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  print(manifest[c("dataset", "archive_bytes", "measurements")])
  invisible(manifest)
}
if (sys.nframe() == 0L) {
  args <- commandArgs(TRUE)
  if (length(args) != 1L) stop("Usage: Rscript load.R DIRECTORY")
  load_numeric_cdm(args[[1L]])
}
