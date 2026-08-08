# Helpers for opt-in integration tests against real PostgreSQL, MySQL and
# MariaDB services. A host environment variable is the explicit opt-in; normal
# package checks never attempt a network connection. The configured credential
# is setup-only; every fixture creates a distinct least-privilege runtime login.

.dsomopVendorTestConfig <- function(dbms) {
  dbms <- match.arg(dbms, c("postgresql", "mysql", "mariadb"))
  prefix <- switch(dbms,
    postgresql = "DSOMOP_TEST_POSTGRES",
    mysql = "DSOMOP_TEST_MYSQL",
    mariadb = "DSOMOP_TEST_MARIADB"
  )
  env <- function(suffix, unset = "") {
    Sys.getenv(paste0(prefix, "_", suffix), unset = unset)
  }
  host <- env("HOST")
  if (!nzchar(host)) return(NULL)

  default_port <- if (identical(dbms, "postgresql")) "5432" else "3306"
  port <- suppressWarnings(as.integer(env("PORT", default_port)))
  if (length(port) != 1L || is.na(port) || port < 1L || port > 65535L) {
    stop(prefix, "_PORT must be an integer between 1 and 65535.",
         call. = FALSE)
  }
  database <- env("DATABASE", "dsomop")
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", database)) {
    stop(prefix, "_DATABASE must be a simple SQL identifier.", call. = FALSE)
  }
  default_admin <- if (identical(dbms, "postgresql")) "dsomop_admin" else "root"

  list(
    dbms = dbms,
    host = host,
    port = port,
    database = database,
    admin_user = env("ADMIN_USER", env("USER", default_admin)),
    admin_password = env("ADMIN_PASSWORD", env("PASSWORD", "dsomop"))
  )
}

.dsomopVendorAdminConnection <- function(config) {
  if (identical(config$dbms, "postgresql")) {
    testthat::skip_if_not_installed("RPostgres")
    return(DBI::dbConnect(
      RPostgres::Postgres(), host = config$host, port = config$port,
      dbname = config$database, user = config$admin_user,
      password = config$admin_password
    ))
  }

  testthat::skip_if_not_installed("RMariaDB")
  DBI::dbConnect(
    RMariaDB::MariaDB(), host = config$host, port = config$port,
    dbname = config$database, user = config$admin_user,
    password = config$admin_password
  )
}

.dsomopVendorNamespaceToken <- function() {
  random <- paste0(format(openssl::rand_bytes(4L)), collapse = "")
  token <- paste0("dsomop_it_", Sys.getpid(), "_", random)
  if (!grepl("^dsomop_it_[0-9]+_[0-9a-f]{8}$", token)) {
    stop("Could not generate a safe integration-test namespace.", call. = FALSE)
  }
  token
}

.dsomopVendorRuntimeCredentials <- function() {
  username_random <- paste0(format(openssl::rand_bytes(6L)), collapse = "")
  password_random <- paste0(format(openssl::rand_bytes(12L)), collapse = "")
  username <- paste0("dsrt_", Sys.getpid(), "_", username_random)
  password <- paste0("Ds0_", password_random)
  if (nchar(username) > 32L ||
      !grepl("^dsrt_[0-9]+_[0-9a-f]{12}$", username) ||
      !grepl("^Ds0_[0-9a-f]{24}$", password)) {
    stop("Could not generate safe vendor-test runtime credentials.",
         call. = FALSE)
  }
  list(username = username, password = password)
}

.dsomopVendorExecuteAll <- function(conn, statements) {
  for (statement in statements) DBI::dbExecute(conn, statement)
  invisible(NULL)
}

.dsomopVendorCreateFixture <- function(config) {
  context <- new.env(parent = emptyenv())
  context$config <- config
  context$admin <- .dsomopVendorAdminConnection(config)
  context$handle <- NULL
  context$closed <- FALSE
  context$runtime_created <- FALSE

  runtime <- .dsomopVendorRuntimeCredentials()
  context$runtime_user <- runtime$username
  context$runtime_password <- runtime$password
  context$runtime_principal <- if (identical(config$dbms, "postgresql")) {
    context$runtime_user
  } else {
    paste0("'", context$runtime_user, "'@'%'")
  }

  context$grant_select <- function(qualified_table) {
    if (!isTRUE(context$runtime_created)) {
      stop("Runtime principal has not been created.", call. = FALSE)
    }
    DBI::dbExecute(context$admin, paste0(
      "GRANT SELECT ON ", qualified_table, " TO ",
      context$runtime_principal
    ))
    invisible(NULL)
  }

  token <- .dsomopVendorNamespaceToken()
  context$schemas <- c(
    cdm = paste0(token, "_cdm"),
    vocab = paste0(token, "_vocab"),
    results = paste0(token, "_results")
  )

  # Cleanup is intentionally limited to names generated and validated above.
  context$cleanup <- function() {
    if (isTRUE(context$closed)) return(invisible(NULL))
    context$closed <- TRUE

    if (!is.null(context$handle)) {
      try(.closeHandle(context$handle), silent = TRUE)
      context$handle <- NULL
    }
    if (DBI::dbIsValid(context$admin)) {
      if (identical(config$dbms, "postgresql")) {
        for (schema in rev(context$schemas)) {
          try(DBI::dbExecute(
            context$admin, paste0("DROP SCHEMA IF EXISTS ", schema, " CASCADE")
          ), silent = TRUE)
        }
        if (isTRUE(context$runtime_created)) {
          try(DBI::dbExecute(context$admin, paste0(
            "REVOKE CONNECT, TEMPORARY ON DATABASE ", config$database,
            " FROM ", context$runtime_user
          )), silent = TRUE)
          try(DBI::dbExecute(context$admin, paste0(
            "DROP ROLE IF EXISTS ", context$runtime_user
          )), silent = TRUE)
        }
      } else {
        for (schema in rev(context$schemas)) {
          try(DBI::dbExecute(
            context$admin, paste0("DROP DATABASE IF EXISTS ", schema)
          ), silent = TRUE)
        }
        if (isTRUE(context$runtime_created)) {
          try(DBI::dbExecute(context$admin, paste0(
            "DROP USER IF EXISTS ", context$runtime_principal
          )), silent = TRUE)
        }
      }
      try(DBI::dbDisconnect(context$admin), silent = TRUE)
    }
    invisible(NULL)
  }

  tryCatch({
    namespace_kind <- if (identical(config$dbms, "postgresql")) {
      "SCHEMA"
    } else {
      "DATABASE"
    }
    .dsomopVendorExecuteAll(context$admin, paste0(
      "CREATE ", namespace_kind, " ", context$schemas
    ))

    cdm <- context$schemas[["cdm"]]
    vocab <- context$schemas[["vocab"]]
    results <- context$schemas[["results"]]

    .dsomopVendorExecuteAll(context$admin, c(
      paste0("CREATE TABLE ", cdm, ".cdm_source (",
             "cdm_source_name VARCHAR(255), ",
             "cdm_source_abbreviation VARCHAR(25), ",
             "cdm_version VARCHAR(10), vocabulary_version VARCHAR(255))"),
      paste0("CREATE TABLE ", cdm, ".person (",
             "person_id BIGINT NOT NULL, gender_concept_id INTEGER NOT NULL, ",
             "year_of_birth INTEGER NOT NULL, race_concept_id INTEGER NOT NULL, ",
             "ethnicity_concept_id INTEGER NOT NULL)"),
      paste0("CREATE TABLE ", cdm, ".observation_period (",
             "observation_period_id BIGINT NOT NULL, person_id BIGINT NOT NULL, ",
             "observation_period_start_date DATE NOT NULL, ",
             "observation_period_end_date DATE NOT NULL, ",
             "period_type_concept_id INTEGER NOT NULL)"),
      paste0("CREATE TABLE ", cdm, ".condition_occurrence (",
             "condition_occurrence_id BIGINT NOT NULL, person_id BIGINT NOT NULL, ",
             "condition_concept_id INTEGER NOT NULL, ",
             "condition_start_date DATE NOT NULL, condition_end_date DATE, ",
             "condition_type_concept_id INTEGER NOT NULL, ",
             "visit_occurrence_id BIGINT)"),
      paste0("CREATE TABLE ", vocab, ".concept (",
             "concept_id INTEGER NOT NULL, concept_name VARCHAR(255) NOT NULL, ",
             "domain_id VARCHAR(20) NOT NULL, vocabulary_id VARCHAR(20) NOT NULL, ",
             "concept_class_id VARCHAR(20) NOT NULL, standard_concept VARCHAR(1), ",
             "concept_code VARCHAR(50) NOT NULL, valid_start_date DATE NOT NULL, ",
             "valid_end_date DATE NOT NULL, invalid_reason VARCHAR(1))"),
      paste0("CREATE TABLE ", vocab, ".concept_ancestor (",
             "ancestor_concept_id INTEGER NOT NULL, ",
             "descendant_concept_id INTEGER NOT NULL, ",
             "min_levels_of_separation INTEGER NOT NULL, ",
             "max_levels_of_separation INTEGER NOT NULL)"),
      paste0("CREATE TABLE ", results, ".cohort_definition (",
             "cohort_definition_id INTEGER NOT NULL, ",
             "cohort_definition_name VARCHAR(255) NOT NULL, ",
             "cohort_definition_description VARCHAR(1000), ",
             "definition_type_concept_id INTEGER NOT NULL, ",
             "cohort_definition_syntax VARCHAR(1000), ",
             "subject_concept_id INTEGER NOT NULL, cohort_initiation_date DATE)"),
      paste0("CREATE TABLE ", results, ".cohort (",
             "cohort_definition_id INTEGER NOT NULL, subject_id BIGINT NOT NULL, ",
             "cohort_start_date DATE NOT NULL, cohort_end_date DATE NOT NULL)"),

      # Same-named decoys make namespace bleed observable. Correct routing must
      # never read either object.
      paste0("CREATE TABLE ", cdm, ".concept (",
             "concept_id INTEGER, concept_name VARCHAR(255))"),
      paste0("CREATE TABLE ", cdm, ".cohort (",
             "cohort_definition_id INTEGER, subject_id BIGINT, ",
             "cohort_start_date DATE, cohort_end_date DATE)"),

      paste0("INSERT INTO ", cdm, ".cdm_source VALUES ",
             "('Vendor integration fixture', 'DSOMOP_IT', '5.4', 'test')"),
      paste0("INSERT INTO ", cdm, ".person VALUES ",
             "(1, 8507, 1970, 8527, 38003564), ",
             "(2, 8532, 1980, 8527, 38003564), ",
             "(3, 8507, 1990, 8527, 38003564)"),
      paste0("INSERT INTO ", cdm, ".observation_period VALUES ",
             "(1, 1, '2019-01-01', '2022-12-31', 44814724), ",
             "(2, 2, '2019-01-01', '2022-12-31', 44814724), ",
             "(3, 3, '2019-01-01', '2022-12-31', 44814724)"),
      paste0("INSERT INTO ", cdm, ".condition_occurrence VALUES ",
             "(1, 1, 201820, '2020-01-01', '2020-01-03', 32020, NULL), ",
             "(2, 1, 201820, '2020-02-01', '2020-02-03', 32020, NULL), ",
             "(3, 2, 201820, '2020-03-01', '2020-03-03', 32020, NULL), ",
             "(4, 3, 201820, '2021-01-01', '2021-01-03', 32020, NULL)"),
      paste0("INSERT INTO ", vocab, ".concept VALUES ",
             "(201820, 'Type 2 diabetes mellitus', 'Condition', 'SNOMED', ",
             "'Clinical Finding', 'S', '44054006', '1970-01-01', ",
             "'2099-12-31', NULL)"),
      paste0("INSERT INTO ", vocab, ".concept_ancestor VALUES ",
             "(201820, 201820, 0, 0)"),
      paste0("INSERT INTO ", results, ".cohort_definition VALUES ",
             "(7, 'All fixture persons', 'Integration test cohort', 0, ",
             "'{}', 1147314, '2022-01-01'), ",
             "(8, 'Recurrent fixture episodes', 'Longitudinal test cohort', 0, ",
             "'{}', 1147314, '2022-01-01')"),
      paste0("INSERT INTO ", results, ".cohort VALUES ",
             "(7, 1, '2020-01-01', '2020-12-31'), ",
             "(7, 2, '2020-01-01', '2020-12-31'), ",
             "(7, 3, '2020-01-01', '2020-12-31'), ",
             "(8, 1, '2020-01-01', '2020-01-31'), ",
             "(8, 1, '2020-02-01', '2020-02-29'), ",
             "(8, 2, '2020-01-01', '2020-01-31'), ",
             "(8, 2, '2020-03-01', '2020-03-31'), ",
             "(8, 3, '2020-01-01', '2020-01-31'), ",
             "(8, 3, '2021-01-01', '2021-01-31')"),
      paste0("CREATE VIEW ", results, ".longitudinal_cohort AS ",
             "SELECT subject_id, cohort_start_date, cohort_end_date FROM ",
             results, ".cohort WHERE cohort_definition_id = 8"),
      paste0("CREATE VIEW ", results, ".full_cohort AS ",
             "SELECT subject_id, cohort_start_date, cohort_end_date FROM ",
             results, ".cohort WHERE cohort_definition_id = 7"),
      paste0("INSERT INTO ", cdm, ".concept VALUES (999, 'WRONG SCHEMA')"),
      paste0("INSERT INTO ", cdm, ".cohort VALUES ",
             "(99, 999, '2020-01-01', '2020-12-31')")
    ))

    if (identical(config$dbms, "postgresql")) {
      DBI::dbExecute(context$admin, paste0(
        "CREATE ROLE ", context$runtime_user,
        " LOGIN PASSWORD '", context$runtime_password,
        "' NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION"
      ))
      context$runtime_created <- TRUE
      .dsomopVendorExecuteAll(context$admin, c(
        paste0(
          "GRANT CONNECT, TEMPORARY ON DATABASE ", config$database,
          " TO ", context$runtime_user
        ),
        paste0(
          "GRANT USAGE ON SCHEMA ", paste(context$schemas, collapse = ", "),
          " TO ", context$runtime_user
        ),
        paste0(
          "GRANT SELECT ON ALL TABLES IN SCHEMA ",
          paste(context$schemas, collapse = ", "),
          " TO ", context$runtime_user
        )
      ))
    } else {
      DBI::dbExecute(context$admin, paste0(
        "CREATE USER ", context$runtime_principal,
        " IDENTIFIED BY '", context$runtime_password, "'"
      ))
      context$runtime_created <- TRUE
      .dsomopVendorExecuteAll(context$admin, c(
        vapply(context$schemas, function(schema) paste0(
          "GRANT SELECT ON `", schema, "`.* TO ",
          context$runtime_principal
        ), character(1L)),
        paste0(
          "GRANT CREATE TEMPORARY TABLES ON `", config$database, "`.* TO ",
          context$runtime_principal
        )
      ))
    }

    query <- paste0(
      "cdm_schema=", cdm,
      "&vocabulary_schema=", vocab,
      "&results_schema=", results
    )
    resource <- resourcer::newResource(
      name = paste0("vendor-integration-", config$dbms),
      url = paste0("omop+dbi:", config$dbms, "://", config$host, ":",
                   config$port, "/", config$database, "?", query),
      identity = context$runtime_user,
      secret = context$runtime_password,
      format = "omop.dbi.db"
    )
    context$handle <- .createHandle(OMOPResourceClient$new(resource))
    context
  }, error = function(e) {
    context$cleanup()
    stop(e)
  })
}
