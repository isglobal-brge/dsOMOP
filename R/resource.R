# Module: Resource Resolver
# ResourceR integration for OMOP CDM database connections.

#' OMOP CDM Resource Client
#'
#' R6 class that wraps a DataSHIELD resource pointing to an OMOP CDM database.
#' Manages the DBI connection and extracts configuration from the resource URL.
#'
#' @importFrom R6 R6Class
#' @importFrom DBI dbConnect dbDisconnect dbIsValid
#' @keywords internal
OMOPResourceClient <- R6::R6Class(

  "OMOPResourceClient",
  inherit = resourcer::ResourceClient,

  private = list(
    .connection = NULL,
    .parsed = NULL,

    parse_url = function() {
      url <- self$getResource()$url

      # Strip scheme
      body <- sub("^omop\\+dbi:///", "", url)

      # B64-encoded JSON (safe for Opal R parser: no ?, &, = in URL body)
      if (!startsWith(body, "B64:"))
        stop("Unsupported OMOP resource URL format: ", url, call. = FALSE)

      b64 <- substring(body, 5)
      # base64url → standard base64
      b64 <- gsub("-", "+", b64, fixed = TRUE)
      b64 <- gsub("_", "/", b64, fixed = TRUE)
      pad <- (4 - nchar(b64) %% 4) %% 4
      if (pad > 0) b64 <- paste0(b64, strrep("=", pad))
      params <- jsonlite::fromJSON(
        rawToChar(jsonlite::base64_dec(b64)),
        simplifyVector = FALSE
      )

      parsed <- list(
        dbms              = params$dbms,
        host              = params$host,
        port              = as.integer(params$port),
        database          = params$database %||% "",
        server            = paste0(params$host, "/", params$database %||% ""),
        cdm_schema        = params$cdm_schema,
        vocabulary_schema = params$vocabulary_schema,
        results_schema    = params$results_schema,
        temp_schema       = params$temp_schema
      )
      private$.parsed <- parsed
    },

    #' Create a DBI connection directly from parsed URL parameters
    connect_dbi = function() {
      res <- self$getResource()
      p <- private$.parsed
      user <- res$identity
      pass <- res$secret

      dbms <- tolower(p$dbms %||% "")

      if (dbms == "postgresql") {
        if (requireNamespace("RPostgres", quietly = TRUE)) {
          return(DBI::dbConnect(RPostgres::Postgres(),
                                host = p$host, port = p$port,
                                dbname = p$database,
                                user = user, password = pass))
        }
        if (requireNamespace("RPostgreSQL", quietly = TRUE)) {
          return(DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                                host = p$host, port = p$port,
                                dbname = p$database,
                                user = user, password = pass))
        }
        stop("No PostgreSQL driver found. Install RPostgres or RPostgreSQL.",
             call. = FALSE)
      }

      if (dbms == "sqlite") {
        if (!requireNamespace("RSQLite", quietly = TRUE))
          stop("RSQLite package required for SQLite connections.", call. = FALSE)
        return(DBI::dbConnect(RSQLite::SQLite(), dbname = p$database))
      }

      if (dbms == "duckdb") {
        if (!requireNamespace("duckdb", quietly = TRUE))
          stop("duckdb package required for DuckDB connections.", call. = FALSE)
        dbpath <- if (nchar(p$database) > 0) p$database else ":memory:"
        return(DBI::dbConnect(duckdb::duckdb(), dbdir = dbpath, read_only = FALSE))
      }

      if (dbms %in% c("sql server", "sqlserver", "mssql")) {
        if (!requireNamespace("odbc", quietly = TRUE))
          stop("odbc package required for SQL Server connections.", call. = FALSE)
        return(DBI::dbConnect(odbc::odbc(),
                              driver = "ODBC Driver 17 for SQL Server",
                              server = paste0(p$host, ",", p$port),
                              database = p$database,
                              uid = user, pwd = pass))
      }

      if (dbms %in% c("mysql", "mariadb")) {
        if (!requireNamespace("RMariaDB", quietly = TRUE))
          stop("RMariaDB package required for MySQL/MariaDB.", call. = FALSE)
        return(DBI::dbConnect(RMariaDB::MariaDB(),
                              host = p$host, port = p$port,
                              dbname = p$database,
                              user = user, password = pass))
      }

      if (dbms == "oracle") {
        # Prefer ROracle (requires Oracle Instant Client), fallback to odbc
        if (requireNamespace("ROracle", quietly = TRUE)) {
          drv <- DBI::dbDriver("Oracle")
          connect_string <- paste0(
            "(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)(HOST=", p$host,
            ")(PORT=", p$port, "))(CONNECT_DATA=(SID=", p$database, ")))"
          )
          return(DBI::dbConnect(drv, username = user, password = pass,
                                dbname = connect_string))
        }
        if (requireNamespace("odbc", quietly = TRUE)) {
          return(DBI::dbConnect(odbc::odbc(),
                                driver = "Oracle",
                                DBQ = paste0(p$host, ":", p$port, "/", p$database),
                                UID = user, PWD = pass))
        }
        stop("Oracle requires ROracle (with Oracle Instant Client) or odbc package.",
             call. = FALSE)
      }

      if (dbms == "redshift") {
        # Redshift is PostgreSQL wire-compatible
        if (!requireNamespace("RPostgres", quietly = TRUE))
          stop("RPostgres package required for Redshift connections.", call. = FALSE)
        return(DBI::dbConnect(RPostgres::Postgres(),
                              host = p$host, port = p$port,
                              dbname = p$database,
                              user = user, password = pass))
      }

      if (dbms == "bigquery") {
        if (!requireNamespace("bigrquery", quietly = TRUE))
          stop("bigrquery package required for BigQuery connections.", call. = FALSE)
        project <- p$host  # use host field for GCP project ID
        return(DBI::dbConnect(bigrquery::bigquery(),
                              project = project,
                              dataset = p$database))
      }

      if (dbms == "snowflake") {
        if (!requireNamespace("odbc", quietly = TRUE))
          stop("odbc package required for Snowflake connections.", call. = FALSE)
        return(DBI::dbConnect(odbc::odbc(),
                              driver = "Snowflake",
                              server = paste0(p$host, ".snowflakecomputing.com"),
                              database = p$database,
                              uid = user, pwd = pass,
                              warehouse = p$warehouse %||% "COMPUTE_WH"))
      }

      if (dbms %in% c("spark", "databricks")) {
        if (!requireNamespace("odbc", quietly = TRUE))
          stop("odbc package required for Spark/Databricks connections.", call. = FALSE)
        # Databricks uses its own ODBC driver; classic Spark uses Simba
        driver <- if (dbms == "databricks") "Databricks" else "Simba Spark ODBC Driver"
        return(DBI::dbConnect(odbc::odbc(),
                              driver = driver,
                              host = p$host, port = p$port,
                              database = p$database,
                              uid = user, pwd = pass))
      }

      stop("Unsupported DBMS: '", dbms, "'. Supported: postgresql, sqlite, duckdb, ",
           "sql server, oracle, mysql, mariadb, redshift, bigquery, snowflake, ",
           "spark, databricks.",
           call. = FALSE)
    }
  ),

  public = list(
    #' @description Create a new OMOP resource client
    #' @param resource A resourcer resource object
    initialize = function(resource, ...) {
      super$initialize(resource)
      private$parse_url()
    },

    #' @description Get or create a DBI connection
    #' @return A DBI connection object
    getConnection = function() {
      if (is.null(private$.connection) || !DBI::dbIsValid(private$.connection)) {
        private$.connection <- private$connect_dbi()
      }
      private$.connection
    },

    #' @description Get parsed URL parameters
    #' @return Named list with dbms, host, port, database, schemas
    getParsed = function() {
      private$.parsed
    },

    #' @description Get the DBMS type
    #' @return Character string
    getDBMS = function() {
      private$.parsed$dbms
    },

    #' @description Get the CDM schema name
    #' @return Character or NULL
    getCDMSchema = function() {
      private$.parsed$cdm_schema
    },

    #' @description Get the vocabulary schema name
    #' @return Character or NULL
    getVocabularySchema = function() {
      private$.parsed$vocabulary_schema
    },

    #' @description Get the results schema name
    #' @return Character or NULL
    getResultsSchema = function() {
      private$.parsed$results_schema
    },

    #' @description Get the temp schema name
    #' @return Character or NULL
    getTempSchema = function() {
      private$.parsed$temp_schema
    },

    #' @description Close the connection
    close = function() {
      if (!is.null(private$.connection)) {
        try(DBI::dbDisconnect(private$.connection), silent = TRUE)
        private$.connection <- NULL
      }
    }
  )
)


#' OMOP CDM Resource Resolver
#'
#' A \code{resourcer::ResourceResolver} subclass that creates database
#' connections from DataSHIELD resource descriptors pointing to OMOP CDM
#' databases. Matches resources whose format is \code{"omop.dbi.db"} and
#' delegates connection setup to \code{\link{OMOPResourceClient}}, which
#' supports PostgreSQL, SQL Server, SQLite, and MySQL/MariaDB backends.
#'
#' @importFrom R6 R6Class
#' @keywords internal
OMOPResourceResolver <- R6::R6Class(
  "OMOPResourceResolver",
  inherit = resourcer::ResourceResolver,

  public = list(
    #' @description Check if this resolver can handle the given resource
    #' @param resource A resourcer resource object
    #' @return Logical
    isFor = function(resource) {
      if (!super$isFor(resource)) return(FALSE)
      fmt <- resource$format
      if (is.null(fmt)) return(FALSE)
      tolower(fmt) == "omop.dbi.db"
    },

    #' @description Create a new client for the given resource
    #' @param resource A resourcer resource object
    #' @return An OMOPResourceClient, or NULL
    newClient = function(resource) {
      tryCatch(
        OMOPResourceClient$new(resource),
        error = function(e) {
          warning("Failed to create OMOP resource client: ", e$message)
          NULL
        }
      )
    }
  )
)
