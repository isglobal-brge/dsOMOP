# ==============================================================================
# dsOMOP v2 - Resource Resolver + Client (R6 Classes)
# ==============================================================================
# Handles DataSHIELD resource resolution for OMOP CDM databases.
# Parses resource URL to extract DBMS, host, port, database, schemas.
# ==============================================================================

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
      # URL format: omop+hades:///dbms=X;server=host/db;port=N;key=value;...
      body <- sub("^omop\\+hades:///", "", url)
      pairs <- strsplit(body, ";", fixed = TRUE)[[1]]
      params <- list()
      for (pair in pairs) {
        eq_pos <- regexpr("=", pair, fixed = TRUE)
        if (eq_pos > 0) {
          key <- substr(pair, 1, eq_pos - 1)
          val <- substr(pair, eq_pos + 1, nchar(pair))
          params[[key]] <- val
        }
      }

      # Parse server into host + database
      server_parts <- strsplit(params$server %||% "", "/", fixed = TRUE)[[1]]

      parsed <- list(
        dbms              = params$dbms,
        host              = server_parts[1],
        port              = as.integer(params$port),
        database          = if (length(server_parts) > 1) server_parts[2] else "",
        server            = params$server,
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

      stop("Unsupported DBMS: '", dbms, "'. ",
           "Supported: postgresql, sqlite, sql server, mysql, mariadb.",
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
      tolower(fmt) %in% c("omop.hades.db", "omop.cdm.db")
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
