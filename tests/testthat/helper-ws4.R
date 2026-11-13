# Shared fixtures for the WS-4 compiler tests (design-2.10.3 sections 6.4-6.6
# and 11): the extended reach fixture (create_reach_db.R + extend_reach_db.R,
# CDM 5.4 with episodes, episode_event and cost) on every executable backend
# available here -- SQLite always, DuckDB when installed, PostgreSQL through
# the opt-in vendor variables (DSOMOP_TEST_POSTGRES_HOST, as the vendor
# integration tests) or the pod's default WS-0 host, MariaDB through
# DSOMOP_TEST_MARIADB_HOST. Each backend copy carries the same rows with typed
# DATE columns, so one assertion runs unchanged everywhere.

.ws4_fixture_file <- function(name) {
  tp <- tryCatch(testthat::test_path(), error = function(e) ".")
  Find(file.exists, c(file.path(tp, "..", "fixtures", name),
                      file.path("tests", "fixtures", name)))
}

.ws4_reach_handle <- function(with_defects = FALSE) {
  testthat::skip_if_not_installed("RSQLite")
  env <- new.env()
  for (f in c("create_reach_db.R", "extend_reach_db.R")) {
    path <- .ws4_fixture_file(f)
    testthat::skip_if(is.null(path), paste("fixture", f, "not found"))
    sys.source(path, envir = env)
  }
  env$build_extended_reach_handle(100L, with_defects = with_defects)
}

# Copy every table of a SQLite fixture into another connection, with *_date
# columns as DATE and *_datetime as TIMESTAMP (as load_reach_pg.R does).
.ws4_copy_tables <- function(src, dest, schema = NULL) {
  tables <- sort(DBI::dbListTables(src))
  tables <- tables[!grepl("^sqlite_", tables)]
  for (tbl in tables) {
    df <- DBI::dbGetQuery(src, paste0("SELECT * FROM ", tbl))
    for (col in names(df)[grepl("_date$", names(df)) & vapply(df, is.character, logical(1))]) {
      df[[col]] <- as.Date(df[[col]])
    }
    for (col in names(df)[grepl("_datetime$", names(df)) & vapply(df, is.character, logical(1))]) {
      df[[col]] <- as.POSIXct(df[[col]], tz = "UTC")
    }
    for (col in names(df)[vapply(df, is.logical, logical(1)) & vapply(df, function(x) all(is.na(x)), logical(1))]) {
      # all-NULL columns arrive as logical: keep them numeric-compatible
      df[[col]] <- as.numeric(df[[col]])
    }
    id <- if (is.null(schema)) tbl else DBI::Id(schema = schema, table = tbl)
    text_cols <- names(df)[vapply(df, is.character, logical(1))]
    if (inherits(dest, "MariaDBConnection") && length(text_cols)) {
      # an empty character column would be created as VARCHAR(0)
      types <- stats::setNames(rep("VARCHAR(255)", length(text_cols)), text_cols)
      DBI::dbWriteTable(dest, id, df, overwrite = TRUE, field.types = types)
    } else {
      DBI::dbWriteTable(dest, id, df, overwrite = TRUE)
    }
  }
  invisible(tables)
}

.ws4_handle_on <- function(conn, dbms, target_dialect, schema) {
  h <- new.env(parent = emptyenv())
  h$conn <- conn
  h$dbms <- dbms
  h$target_dialect <- target_dialect
  h$cdm_schema <- schema
  h$vocab_schema <- schema
  h$results_schema <- schema
  h$temp_schema <- NULL
  h$resource_client <- NULL
  h$config <- list()
  h$blueprint <- NULL
  h$temp_tables <- character(0)
  h$temp_connection <- NULL
  h
}

.ws4_pg_connect <- function() {
  if (!requireNamespace("RPostgres", quietly = TRUE)) return(NULL)
  config <- .dsomopVendorTestConfig("postgresql")
  if (!is.null(config)) {
    return(tryCatch(DBI::dbConnect(
      RPostgres::Postgres(), host = config$host, port = config$port,
      dbname = config$database, user = config$admin_user,
      password = config$admin_password, sslmode = "disable",
      connect_timeout = 5L), error = function(e) NULL))
  }
  host <- Sys.getenv("DSOMOP_TEST_PG_HOST", "omopdb")
  tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = host,
    port = as.integer(Sys.getenv("DSOMOP_TEST_PG_PORT", "5432")),
    dbname = Sys.getenv("DSOMOP_TEST_PG_DB", "omop"),
    user = Sys.getenv("DSOMOP_TEST_PG_USER", "omop"),
    password = Sys.getenv("DSOMOP_TEST_PG_PASSWORD", "omop"),
    sslmode = "disable", connect_timeout = 5L), error = function(e) NULL)
}

.ws4_namespace <- function(prefix) {
  paste0(prefix, "_", Sys.getpid(), "_",
         paste(format(openssl::rand_bytes(3L)), collapse = ""))
}

#' The reach fixture on every executable backend available
#'
#' @return Named list of list(handle, cleanup) per backend (sqlite always).
.ws4_backends <- function(with_defects = FALSE) {
  sqlite <- .ws4_reach_handle(with_defects = with_defects)
  out <- list(sqlite = list(handle = sqlite, cleanup = function() {
    try(DBI::dbDisconnect(sqlite$conn), silent = TRUE)
  }))
  if (requireNamespace("duckdb", quietly = TRUE)) {
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
  pg <- .ws4_pg_connect()
  if (!is.null(pg)) {
    schema <- .ws4_namespace("ws4")
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
  config <- .dsomopVendorTestConfig("mariadb")
  if (!is.null(config) && requireNamespace("RMariaDB", quietly = TRUE)) {
    my <- tryCatch(DBI::dbConnect(RMariaDB::MariaDB(), host = config$host, port = config$port,
                                  dbname = config$database, user = config$admin_user,
                                  password = config$admin_password),
                   error = function(e) NULL)
    if (!is.null(my)) {
      db <- .ws4_namespace("ws4")
      ok <- tryCatch({
        .ensureMariaSession(my, require_tls = FALSE)
        DBI::dbExecute(my, paste0("CREATE DATABASE ", db))
        .ws4_copy_tables(sqlite$conn, my, db)
        TRUE
      }, error = function(e) FALSE)
      if (ok) {
        h <- .ws4_handle_on(my, "mariadb", "mysql", db)
        h$person_key <- sqlite$person_key
        out$mariadb <- list(handle = h, cleanup = function() {
          try(DBI::dbExecute(my, paste0("DROP DATABASE IF EXISTS ", db)), silent = TRUE)
          try(DBI::dbDisconnect(my), silent = TRUE)
        })
      } else {
        try(DBI::dbExecute(my, paste0("DROP DATABASE IF EXISTS ", db)), silent = TRUE)
        try(DBI::dbDisconnect(my), silent = TRUE)
      }
    }
  }
  out
}

.ws4_cleanup <- function(backends) {
  for (b in backends) try(b$cleanup(), silent = TRUE)
  invisible(NULL)
}

# A minimal language-2.0 plan around a set of events (one wide output over a
# criteria population so the plan is schema-valid).
.ws4_plan <- function(events = list(), concept_sets = NULL) {
  plan <- list(
    version = 2L,
    meta = list(language_version = "2.0", semantic_version = "2.0.0",
                generated_by = "ws4-test"),
    events = events,
    populations = list(all = list(kind = "criteria")),
    outputs = list(o = list(rows = "all", format = "wide")))
  if (!is.null(concept_sets)) plan$concept_sets <- concept_sets
  plan
}

# An execution context over a plan, through the frozen IR normaliser. The
# plan is validated against the wire schema first.
.ws4_ctx <- function(handle, plan, validate = TRUE) {
  plan <- .irJsonForm(plan)
  if (validate) .irValidateWire(plan)
  ir <- .irNormalize(plan)
  .planContext(handle, ir)
}

# Rows of a compiled event, ordered deterministically.
.ws4_rows <- function(ctx, name, where = NULL,
                      order = "person_id, start_date, source_ns, event_id") {
  f <- .compileEventByName(ctx, name)
  select <- paste0("SELECT * FROM ", f$name, " x",
                   if (!is.null(where)) paste0(" WHERE ", where),
                   " ORDER BY ", order)
  sql <- .buildStatement(ctx, f, select = select, dialect = ctx)
  rows <- .executeQuery(ctx$handle, sql)
  for (col in intersect(c("start_date", "end_date", "end_recorded"), names(rows))) {
    rows[[col]] <- as.character(as.Date(rows[[col]]))
  }
  rows
}

# Expect a typed plan rejection with the given code (and reason).
.ws4_expect_rejection <- function(expr, code = "validation_error", reason = NULL,
                                  pattern = NULL) {
  err <- tryCatch({ force(expr); NULL }, dsomop_plan_rejection = function(e) e)
  testthat::expect_s3_class(err, "dsomop_plan_rejection")
  if (!is.null(err)) {
    testthat::expect_identical(err$code, code)
    if (!is.null(reason)) testthat::expect_identical(err$reason, reason)
    if (!is.null(pattern)) testthat::expect_match(conditionMessage(err), pattern)
  }
  invisible(err)
}
