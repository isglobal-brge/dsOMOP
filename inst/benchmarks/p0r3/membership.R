# Runnable on the test pod from the package directory:
# Rscript inst/benchmarks/p0r3/membership.R [output.csv]
# Synthetic deterministic IDs 1..10,000,000; no random inputs, no private data.
# Cold = first digest in a fresh connection after fixture construction; caches
# are NOT forcibly dropped (shared pod). Warm = next two complete requests.
# Both perform full reads; neither uses a retained generation or answer cache.
# Uses dedicated generated schemas / an owned DuckDB file and removes them.
suppressPackageStartupMessages(pkgload::load_all('.', quiet = TRUE))
source('tests/testthat/helper-vendor-db.R')
ns <- asNamespace('dsOMOP')
fn <- function(name) get(name, envir = ns)
args <- commandArgs(TRUE)
outfile <- if (length(args)) args[[1L]] else 'inst/benchmarks/p0r3/membership-raw.csv'
Sys.setenv(DSOMOP_TEST_POSTGRES_HOST = 'omopdb',
           DSOMOP_TEST_MARIADB_HOST = '127.0.0.1',
           DSOMOP_TEST_MYSQL_HOST = '127.0.0.1', DSOMOP_TEST_MYSQL_PORT = '3307')
N <- 10000000L
rows <- list()
run <- function(dbms) {
  if (dbms == 'duckdb') {
    path <- tempfile(fileext = '.duckdb')
    conn <- DBI::dbConnect(duckdb::duckdb(), path)
    DBI::dbExecute(conn, paste0('CREATE TABLE person AS SELECT i::BIGINT AS person_id FROM range(1, ', N + 1, ') t(i)'))
    DBI::dbDisconnect(conn, shutdown = TRUE)
    conn <- DBI::dbConnect(duckdb::duckdb(), path)
    on.exit({DBI::dbDisconnect(conn, shutdown = TRUE); unlink(path)}, add = TRUE)
    h <- new.env(parent = emptyenv()); h$conn <- conn
    h$dbms <- h$target_dialect <- dbms; h$cdm_schema <- 'main'
    version <- DBI::dbGetQuery(conn, 'SELECT version()')[[1L]][[1L]]
  } else {
    config <- .dsomopVendorTestConfig(dbms)
    fixture <- .dsomopVendorCreateFixture(config, function(cdm, vocab, results) {
      table <- paste0(cdm, '.person')
      create <- paste0('CREATE TABLE ', table, ' (person_id BIGINT PRIMARY KEY)')
      if (dbms == 'postgresql') {
        insert <- paste0('INSERT INTO ', table, ' SELECT generate_series(1, ', N, ')')
      } else if (dbms == 'mariadb') {
        insert <- paste0('INSERT INTO ', table, ' SELECT seq FROM seq_1_to_', N)
      } else {
        digit <- '(SELECT 0 AS n UNION ALL SELECT 1 UNION ALL SELECT 2 UNION ALL SELECT 3 UNION ALL SELECT 4 UNION ALL SELECT 5 UNION ALL SELECT 6 UNION ALL SELECT 7 UNION ALL SELECT 8 UNION ALL SELECT 9)'
        terms <- paste0('d', 0:6, '.n * ', 10^(0:6))
        sources <- paste0(digit, ' d', 0:6)
        insert <- paste0('INSERT INTO ', table, ' SELECT 1 + ', paste(terms, collapse = ' + '),
                         ' FROM ', paste(sources, collapse = ' CROSS JOIN '))
      }
      c(create, insert)
    })
    on.exit(fixture$cleanup(), add = TRUE)
    h <- fixture$handle; conn <- h$conn
    version <- DBI::dbGetQuery(conn, 'SELECT VERSION()')[[1L]][[1L]]
  }
  h$temp_tables <- character()
  sql <- fn('.dsomopDpMembershipSql')(h)
  # Save the concrete SQL and explain plan as evidence of the scan used.
  writeLines(sql, paste0(dirname(outfile), '/', dbms, '-query.sql'))
  explain <- tryCatch(DBI::dbGetQuery(conn, paste('EXPLAIN', sql)), error = function(e) conditionMessage(e))
  capture.output(print(explain), file = paste0(dirname(outfile), '/', dbms, '-explain.txt'))
  expected <- NULL
  for (iteration in 1:3) {
    gc()
    timing <- system.time(value <- fn('.dsomopWithReadSnapshot')(h,
      fn('.dsomopDpMembershipHash')(h)))
    stopifnot(value$n == as.character(N), is.null(h$read_snapshot))
    if (is.null(expected)) expected <- value$digest else stopifnot(identical(expected, value$digest))
    row <- data.frame(engine = dbms, version = version, persons = N,
      cache = if (iteration == 1L) 'cold_connection_first_digest' else 'warm',
      iteration = iteration, elapsed_seconds = unname(timing[['elapsed']]),
      r_user_seconds = unname(timing[['user.self']]),
      r_system_seconds = unname(timing[['sys.self']]), method = value$method,
      digest = value$digest, timestamp_utc = format(Sys.time(), tz = 'UTC'))
    rows[[length(rows) + 1L]] <<- row
    write.csv(do.call(rbind, rows), outfile, row.names = FALSE)
    print(row)
    flush.console()
  }
}
for (dbms in c('postgresql', 'mariadb', 'mysql', 'duckdb')) run(dbms)
stopifnot(length(unique(vapply(rows, function(x) x$digest, character(1L)))) == 1L)
