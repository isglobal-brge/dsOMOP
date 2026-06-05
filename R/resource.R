# Module: Resource Resolver
# ResourceR integration for OMOP CDM database connections.

# --- URL Parsing Helpers ---

#' Decode percent-encoded URL components safely
#'
#' @param x Character; a possibly percent-encoded string.
#' @return The decoded string, or the input unchanged if decoding fails or is empty.
#' @keywords internal
.urlDecode <- function(x) {
  if (is.null(x) || !nzchar(x)) return(x)
  tryCatch(utils::URLdecode(x), error = function(e) x)
}

#' Normalize a DBMS name to a canonical token
#'
#' Maps the many spellings of each supported backend (underscores, spaces,
#' vendor aliases) to a single canonical token accepted by BOTH
#' \code{\link{.resolve_target_dialect}} and \code{connect_dbi}. Unknown values
#' are returned cleaned (lower-cased, separators collapsed) so the downstream
#' dialect resolver raises one consistent "Unsupported DBMS" error.
#'
#' @param x Character; a raw DBMS name.
#' @return Character canonical token, or NULL if input is NULL.
#' @keywords internal
.normalizeDBMS <- function(x) {
  if (is.null(x)) return(NULL)
  s <- tolower(trimws(x))
  if (!nzchar(s)) return(s)
  s <- trimws(gsub("[_[:space:]]+", " ", s))   # "sql_server" / "sql  server" -> "sql server"
  aliases <- c(
    "postgresql"      = "postgresql",
    "postgres"        = "postgresql",
    "postgre"         = "postgresql",
    "pg"              = "postgresql",
    "mysql"           = "mysql",
    "mariadb"         = "mariadb",
    "maria"           = "mariadb",
    "sql server"      = "sqlserver",
    "sqlserver"       = "sqlserver",
    "mssql"           = "sqlserver",
    "ms sql"          = "sqlserver",
    "ms sql server"   = "sqlserver",
    "synapse"         = "synapse",
    "azure synapse"   = "synapse",
    "pdw"             = "pdw",
    "oracle"          = "oracle",
    "redshift"        = "redshift",
    "amazon redshift" = "redshift",
    "bigquery"        = "bigquery",
    "big query"       = "bigquery",
    "snowflake"       = "snowflake",
    "spark"           = "spark",
    "spark sql"       = "spark",
    "sparksql"        = "spark",
    "hive"            = "spark",
    "databricks"      = "databricks",
    "sqlite"          = "sqlite",
    "sqlite3"         = "sqlite",
    "duckdb"          = "duckdb"
  )
  out <- aliases[[s]]
  if (is.null(out)) s else out
}

#' Default schema for a DBMS when none is supplied
#'
#' Engines expose their default namespace differently: PostgreSQL and Redshift
#' use \code{public}; SQL Server / Synapse / PDW use \code{dbo}; MySQL/MariaDB
#' and BigQuery treat the database/dataset itself as the schema; Oracle uses the
#' connecting user's (upper-cased) schema; SQLite/DuckDB use \code{main};
#' Snowflake uses \code{PUBLIC}; Spark/Databricks use \code{default}.
#'
#' @param dbms Character; DBMS name (any spelling; normalized internally).
#' @param database Character; database/dataset name (where it doubles as schema).
#' @param user Character; connecting user (used for Oracle).
#' @return Character schema name, or NULL when no sensible default exists.
#' @keywords internal
.dbmsDefaultSchema <- function(dbms, database = NULL, user = NULL) {
  d <- .normalizeDBMS(dbms)
  if (is.null(d) || !nzchar(d)) return(NULL)
  nz <- function(v) !is.null(v) && nzchar(v)
  switch(d,
    postgresql = "public",
    redshift   = "public",
    sqlserver  = "dbo",
    synapse    = "dbo",
    pdw        = "dbo",
    mysql      = if (nz(database)) database else NULL,
    mariadb    = if (nz(database)) database else NULL,
    bigquery   = if (nz(database)) database else NULL,
    oracle     = if (nz(user)) toupper(user) else NULL,
    sqlite     = "main",
    duckdb     = "main",
    snowflake  = "PUBLIC",
    spark      = "default",
    databricks = "default",
    NULL
  )
}

#' Parse a readable OMOP CDM resource URL
#'
#' Parses URLs of the form
#' \code{omop+dbi:<dbms>://<host>[:<port>]/<database>?cdm_schema=...&vocabulary_schema=...}.
#' The \code{omop+dbi:} wrapper is optional (a bare \code{<dbms>://...} is
#' accepted). File-backed engines use an empty authority and an absolute path,
#' e.g. \code{omop+dbi:sqlite:///srv/data/omop.sqlite}. Recognized query keys:
#' \code{cdm_schema}, \code{vocabulary_schema}, \code{results_schema},
#' \code{temp_schema}, \code{warehouse}, \code{driver} (plus a few aliases).
#'
#' @param url Character; the resource URL.
#' @return Named list with dbms, host, port, database, server and schema/extra fields.
#' @keywords internal
.parseOmopUrl <- function(url) {
  if (is.null(url) || !nzchar(trimws(url)))
    stop("OMOP resource URL is empty.", call. = FALSE)

  raw <- trimws(url)

  # The dsOMOP wrapper scheme "omop+dbi:" is optional.
  body <- sub("^omop\\+dbi:", "", raw, ignore.case = TRUE)

  # Refuse the retired base64 format with an actionable message.
  if (grepl("^/*B64:", body))
    stop("This OMOP resource uses the retired base64 URL format. Re-create it ",
         "with a readable URL, e.g. ",
         "'omop+dbi:postgresql://host:5432/db?cdm_schema=cdm'.", call. = FALSE)

  # Split off the query string (everything after the first '?').
  query <- ""
  qpos <- regexpr("?", body, fixed = TRUE)
  if (qpos > 0L) {
    query <- substring(body, qpos + 1L)
    body  <- substring(body, 1L, qpos - 1L)
  }

  # Scheme (the DBMS) precedes "://".
  sep <- regexpr("://", body, fixed = TRUE)
  if (sep < 1L)
    stop("Malformed OMOP resource URL (expected '<dbms>://...'): ", url,
         call. = FALSE)
  dbms_raw <- substring(body, 1L, sep - 1L)
  rest     <- substring(body, sep + 3L)

  # Authority is up to the first '/'; the remainder is the database/file path.
  slash <- regexpr("/", rest, fixed = TRUE)
  if (slash < 1L) {
    authority <- rest
    path <- ""
  } else {
    authority <- substring(rest, 1L, slash - 1L)
    path      <- substring(rest, slash + 1L)
  }

  # Host and optional port from the authority (split on the last ':').
  host <- authority
  port <- NULL
  if (nzchar(authority)) {
    cpos <- regexpr(":[^:]*$", authority)
    if (cpos > 0L) {
      host <- substring(authority, 1L, cpos - 1L)
      port_str <- substring(authority, cpos + 1L)
      if (nzchar(port_str)) port <- suppressWarnings(as.integer(port_str))
    }
  }
  host <- .urlDecode(host)

  # Database / file path. With an empty authority the path is absolute (file
  # engines): omop+dbi:sqlite:///srv/x.db -> "/srv/x.db". Kept as "" when absent.
  database <- if (!nzchar(authority) && nzchar(path)) paste0("/", path) else path
  database <- .urlDecode(database)

  # Parse the query string into a named list of decoded values.
  q <- list()
  if (nzchar(query)) {
    for (kv in strsplit(query, "&", fixed = TRUE)[[1]]) {
      if (!nzchar(kv)) next
      eq <- regexpr("=", kv, fixed = TRUE)
      if (eq > 0L) {
        k <- substring(kv, 1L, eq - 1L); v <- substring(kv, eq + 1L)
      } else {
        k <- kv; v <- ""
      }
      q[[.urlDecode(k)]] <- .urlDecode(v)
    }
  }
  pick <- function(...) {
    for (nm in c(...)) {
      val <- q[[nm]]
      if (!is.null(val) && nzchar(val)) return(val)
    }
    NULL
  }

  list(
    dbms              = .normalizeDBMS(dbms_raw),
    host              = if (nzchar(host)) host else NULL,
    port              = port,
    database          = database,                  # "" when absent (connect_dbi compat)
    server            = paste0(host, "/", database),
    cdm_schema        = pick("cdm_schema", "schema"),
    vocabulary_schema = pick("vocabulary_schema", "vocab_schema", "vocabulary"),
    results_schema    = pick("results_schema", "results"),
    temp_schema       = pick("temp_schema", "temp"),
    warehouse         = pick("warehouse"),
    driver            = pick("driver")
  )
}

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
      private$.parsed <- .parseOmopUrl(self$getResource()$url)
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
        dbpath <- if (!is.null(p$database) && nzchar(p$database)) p$database else ":memory:"
        return(DBI::dbConnect(duckdb::duckdb(), dbdir = dbpath, read_only = FALSE))
      }

      if (dbms %in% c("sql server", "sqlserver", "mssql",
                      "synapse", "pdw")) {
        if (!requireNamespace("odbc", quietly = TRUE))
          stop("odbc package required for SQL Server connections.", call. = FALSE)
        return(DBI::dbConnect(odbc::odbc(),
                              driver = p$driver %||% "ODBC Driver 17 for SQL Server",
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
                                driver = p$driver %||% "Oracle",
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
                              driver = p$driver %||% "Snowflake",
                              server = paste0(p$host, ".snowflakecomputing.com"),
                              database = p$database,
                              uid = user, pwd = pass,
                              warehouse = p$warehouse %||% "COMPUTE_WH"))
      }

      if (dbms %in% c("spark", "databricks")) {
        if (!requireNamespace("odbc", quietly = TRUE))
          stop("odbc package required for Spark/Databricks connections.", call. = FALSE)
        # Databricks uses its own ODBC driver; classic Spark uses Simba
        driver <- p$driver %||% (if (dbms == "databricks") "Databricks" else "Simba Spark ODBC Driver")
        return(DBI::dbConnect(odbc::odbc(),
                              driver = driver,
                              host = p$host, port = p$port,
                              database = p$database,
                              uid = user, pwd = pass))
      }

      stop("Unsupported DBMS: '", dbms, "'. Supported: postgresql, sqlite, duckdb, ",
           "sql server, synapse, pdw, oracle, mysql, mariadb, redshift, ",
           "bigquery, snowflake, spark, databricks.",
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
#' supports PostgreSQL, SQLite/DuckDB, MySQL/MariaDB, SQL Server/Synapse/PDW,
#' Oracle, Redshift, BigQuery, Snowflake, Spark, and Databricks backends.
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
