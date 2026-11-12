# Shared fixtures for the WS-6 tests (design-2.10.3 sections 2.5, 2.8, 6.9,
# 6.10): populations and matching on every executable backend available here
# -- SQLite always, DuckDB when installed, PostgreSQL, MariaDB and MySQL
# through the opt-in vendor variables (DSOMOP_TEST_POSTGRES_HOST,
# DSOMOP_TEST_MARIADB_HOST, DSOMOP_TEST_MYSQL_HOST / _PORT) -- on the extended
# reach fixture, optionally extended by a test's own rows.

#' The extended reach fixture on every backend available
#'
#' @param extend NULL or function(conn) adding rows to the SQLite source
#'   before it is copied.
#' @param vendors Backends to try besides SQLite.
#' @return Named list of list(handle, cleanup).
.ws6_backends <- function(extend = NULL,
                          vendors = c("duckdb", "postgresql", "mariadb", "mysql")) {
  sqlite <- .ws4_reach_handle()
  if (!is.null(extend)) extend(sqlite$conn)
  sqlite$blueprint <- NULL
  sqlite$role_registry <- NULL
  out <- list(sqlite = list(handle = sqlite, cleanup = function() {
    try(DBI::dbDisconnect(sqlite$conn), silent = TRUE)
  }))
  if ("duckdb" %in% vendors && requireNamespace("duckdb", quietly = TRUE)) {
    dk <- tryCatch(DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:"), error = function(e) NULL)
    if (!is.null(dk)) {
      .ws4_copy_tables(sqlite$conn, dk)
      h <- .ws4_handle_on(dk, "duckdb", "duckdb", "main")
      h$person_key <- sqlite$person_key
      out$duckdb <- list(handle = h, cleanup = function() {
        try(DBI::dbDisconnect(dk, shutdown = TRUE), silent = TRUE)
      })
    }
  }
  pg <- if ("postgresql" %in% vendors) .ws4_pg_connect()
  if (!is.null(pg)) {
    schema <- .ws4_namespace("ws6")
    ok <- tryCatch({
      DBI::dbExecute(pg, "SET client_min_messages TO WARNING")
      DBI::dbExecute(pg, paste0("CREATE SCHEMA ", schema))
      .ws4_copy_tables(sqlite$conn, pg, schema)
      TRUE
    }, error = function(e) FALSE)
    if (ok) {
      h <- .ws4_handle_on(pg, "postgresql", "postgresql", schema)
      h$person_key <- sqlite$person_key
      out$postgresql <- list(handle = h, cleanup = function() {
        try(DBI::dbExecute(pg, paste0("DROP SCHEMA ", schema, " CASCADE")), silent = TRUE)
        try(DBI::dbDisconnect(pg), silent = TRUE)
      })
    } else {
      try(DBI::dbDisconnect(pg), silent = TRUE)
    }
  }
  for (dbms in intersect(c("mariadb", "mysql"), vendors)) {
    config <- .dsomopVendorTestConfig(dbms)
    if (is.null(config) || !requireNamespace("RMariaDB", quietly = TRUE)) next
    my <- tryCatch(DBI::dbConnect(RMariaDB::MariaDB(), host = config$host, port = config$port,
                                  dbname = config$database, user = config$admin_user,
                                  password = config$admin_password),
                   error = function(e) NULL)
    if (is.null(my)) next
    db <- .ws4_namespace("ws6")
    ok <- tryCatch({
      .ensureMariaSession(my, require_tls = FALSE)
      DBI::dbExecute(my, paste0("CREATE DATABASE ", db))
      .ws4_copy_tables(sqlite$conn, my, db)
      TRUE
    }, error = function(e) FALSE)
    if (ok) {
      h <- .ws4_handle_on(my, dbms, "mysql", db)
      h$person_key <- sqlite$person_key
      out[[dbms]] <- local({
        conn <- my
        name <- db
        list(handle = h, cleanup = function() {
          try(DBI::dbExecute(conn, paste0("DROP DATABASE IF EXISTS ", name)), silent = TRUE)
          try(DBI::dbDisconnect(conn), silent = TRUE)
        })
      })
    } else {
      try(DBI::dbExecute(my, paste0("DROP DATABASE IF EXISTS ", db)), silent = TRUE)
      try(DBI::dbDisconnect(my), silent = TRUE)
    }
  }
  out
}

# Rows of a population's final roster (fixture keys as the oracle writes them).
.ws6_roster <- function(ctx, pop) {
  res <- .compilePopulationByName(ctx, pop)
  r <- .fetchFragment(ctx, res$roster, "cohort_row_id")
  data.frame(cohort_row_id = as.numeric(r$cohort_row_id), person_id = as.numeric(r$subject_id),
             index_date = format(r$cohort_start_date, "%Y-%m-%d"),
             end_date = format(r$cohort_end_date, "%Y-%m-%d"),
             index_source = as.character(r$index_source), index_key = as.character(r$index_key),
             set_id = as.numeric(r$set_id), is_case = as.numeric(r$is_case),
             w = as.numeric(r$sampling_weight),
             op_start = format(r$op_start, "%Y-%m-%d"), op_end = format(r$op_end, "%Y-%m-%d"),
             stringsAsFactors = FALSE)
}

.ws6_key <- function(df, cols = c("person_id", "index_date")) do.call(paste, df[cols])

# A language-2.0 plan for population tests: events, intervals and the given
# populations, with one wide output per population (outputs make every
# population an ordinary roster; matching templates get none).
.ws6_plan <- function(events = list(), populations = list(), intervals = list(),
                      outputs = NULL, scope = NULL) {
  pops <- populations
  if (is.null(outputs)) {
    templates <- unlist(lapply(pops, function(p) if (identical(p$kind, "match")) p$match$controls))
    outputs <- list()
    for (p in setdiff(names(pops), templates)) {
      outputs[[paste0("o_", p)]] <- list(rows = p, format = "wide")
    }
  }
  plan <- list(version = 2L,
               meta = list(language_version = "2.0", semantic_version = "2.0.0", generated_by = "ws6-test"),
               events = events, populations = pops, outputs = outputs)
  if (length(intervals)) plan$intervals <- intervals
  if (!is.null(scope)) plan$scope <- scope
  plan
}

.ws6_event <- function(table, concepts = NULL, ...) {
  ev <- list(table = table, ...)
  if (!is.null(concepts)) ev$concept_set <- list(concepts = as.list(concepts), include_descendants = TRUE)
  ev
}
