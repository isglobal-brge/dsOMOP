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
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("dbms must be one character value.", call. = FALSE)
  }
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
  out <- unname(aliases[s])
  if (length(out) == 0L || is.na(out)) s else out
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

#' Validate a DBMS namespace used to qualify OMOP tables
#'
#' Most supported engines use one schema/database component. Engines whose SQL
#' grammar and metadata APIs support catalog.schema accept at most two. This
#' rejects a resource configuration at bootstrap instead of letting it produce
#' invalid or misrouted SQL during a private request.
#'
#' @param dbms Character DBMS name.
#' @param namespace Character namespace.
#' @param label Character field label for errors.
#' @return The validated namespace.
#' @keywords internal
.validateSchemaNamespace <- function(dbms, namespace, label = "schema") {
  if (is.null(namespace) || length(namespace) == 0L) return(NULL)
  if (!is.character(namespace) || length(namespace) != 1L ||
      is.na(namespace)) {
    stop(label, " must be one non-missing namespace.", call. = FALSE)
  }
  namespace <- trimws(namespace)
  if (!nzchar(namespace)) {
    stop(label, " must be one non-empty namespace.", call. = FALSE)
  }
  canonical <- .normalizeDBMS(dbms)
  parts <- strsplit(namespace, ".", fixed = TRUE)[[1L]]
  if (startsWith(namespace, ".") || endsWith(namespace, ".") ||
      any(!nzchar(parts)) ||
      any(!grepl("^[A-Za-z_][A-Za-z0-9_-]*$", parts))) {
    stop("Invalid ", label, " namespace '", namespace, "'.", call. = FALSE)
  }
  catalog_schema <- c(
    "sqlserver", "synapse", "pdw", "snowflake", "bigquery", "spark",
    "databricks", "duckdb"
  )
  maximum <- if (canonical %in% catalog_schema) 2L else 1L
  if (length(parts) > maximum) {
    stop(label, " for ", canonical, " accepts at most ", maximum,
         " namespace component", if (maximum == 1L) "" else "s", ".",
         call. = FALSE)
  }
  namespace
}

#' Describe the database adapter surface honestly
#'
#' This is a static implementation profile, not a claim of feature parity on
#' every vendor version. PostgreSQL, MySQL and MariaDB have executable CI smoke
#' tests with separate CDM, vocabulary and results namespaces; every deployment
#' still requires site-specific testing of authentication and permissions.
#'
#' @param dbms Character DBMS name.
#' @return Named list describing translation, metadata and temporary-object
#'   support.
#' @keywords internal
.databaseSupportProfile <- function(dbms) {
  canonical <- .normalizeDBMS(dbms)
  dialect <- .resolve_target_dialect(canonical)
  sqlrender <- .sqlRenderRuntimeInfo(canonical, inspect_runtime = FALSE)

  temp_mode <- switch(canonical,
    postgresql =, sqlite =, duckdb =, mysql =, mariadb = "session_table",
    redshift =, snowflake =, spark =, databricks =
      "unavailable_cross_statement",
    sqlserver =, synapse =, pdw =, oracle =, bigquery =
      "unavailable_cross_statement",
    "unavailable"
  )
  metadata_mode <- switch(canonical,
    postgresql = "postgresql_catalog",
    sqlite = "sqlite_catalog",
    oracle = "oracle_catalog",
    bigquery = "dataset_information_schema",
    spark =, databricks = "show_describe",
    "information_schema"
  )
  verification <- switch(canonical,
    postgresql =, mysql =, mariadb = "vendor_integration_tests",
    sqlite = "embedded_integration_tests",
    duckdb = "optional_embedded_integration_tests",
    "sql_contract_tests_only"
  )
  ohdsi_temp_mode <- if (is.null(sqlrender$target_dialect)) {
    "unsupported_mysql_extension"
  } else if (canonical %in% c(
    "oracle", "bigquery", "redshift", "snowflake", "spark", "databricks"
  )) {
    "unsafe_lifecycle_blocked"
  } else {
    "session_scoped_when_explicitly_allowed"
  }

  list(
    dbms = canonical,
    target_dialect = dialect,
    sql_translation = "builtin_reviewed_subset",
    sql_translation_patterns = c(
      "select_top_integer", "dateadd_day_integer", "datediff_day_integer",
      "cast_target_normalization", "sample_stddev_mysql", "oracle_bare_alias"
    ),
    ohdsi_sql_translation = if (is.null(sqlrender$target_dialect)) {
      "reviewed_mysql_extension_only"
    } else {
      "optional_sqlrender_fail_closed"
    },
    sqlrender_target_dialect = sqlrender$target_dialect,
    sqlrender_installed = sqlrender$installed,
    sqlrender_version = sqlrender$version,
    sqlrender_runtime = sqlrender$target_available,
    ohdsi_temporary_objects = ohdsi_temp_mode,
    metadata_discovery = metadata_mode,
    temporary_materialization = temp_mode,
    support_tier = verification,
    verification = verification,
    live_vendor_ci = canonical %in% c("postgresql", "mysql", "mariadb")
  )
}

#' Read and cache the connected database server version
#' @param handle CDM handle.
#' @return Character version string, or \code{NULL} when unavailable.
#' @keywords internal
.databaseServerVersion <- function(handle) {
  configured <- handle$dbms_version %||% NULL
  if (!is.null(configured)) return(as.character(configured)[[1L]])
  version <- tryCatch({
    result <- .withDbReconnect(handle, function(conn) {
      DBI::dbGetQuery(conn, "SELECT VERSION() AS dsomop_server_version")
    })
    if (!is.data.frame(result) || nrow(result) != 1L || ncol(result) != 1L ||
        is.na(result[[1L]][[1L]])) NULL else as.character(result[[1L]][[1L]])
  }, error = function(e) NULL)
  if (!is.null(version) && nzchar(version)) handle$dbms_version <- version
  version
}

#' Extract the server version number from a vendor version string
#'
#' MariaDB reports its own version tagged \code{-MariaDB}, possibly after the
#' replication prefix \code{5.5.5-} and before a distribution suffix with
#' dotted numbers of its own (Ubuntu: \code{10.6.23-MariaDB-0ubuntu0.22.04.1});
#' the tagged number is the server version. Other families report it first.
#'
#' @param version Character vendor version string.
#' @param family \code{"mariadb"} or any other family name.
#' @return The version number as a string, or \code{NULL}.
#' @keywords internal
.databaseVersionNumber <- function(version, family) {
  if (!is.character(version) || length(version) != 1L || is.na(version) ||
      !nzchar(version)) return(NULL)
  pattern <- "[0-9]+\\.[0-9]+(?:\\.[0-9]+)?"
  if (identical(family, "mariadb")) {
    tagged <- regmatches(version, regexec(
      paste0("(", pattern, ")-mariadb"), version, ignore.case = TRUE,
      perl = TRUE))[[1L]]
    if (length(tagged) == 2L) return(tagged[[2L]])
    version <- sub("^5\\.5\\.5-", "", version)
  }
  found <- regmatches(version, regexpr(pattern, version, perl = TRUE))
  if (length(found) == 1L && nzchar(found)) found else NULL
}

#' Compare a vendor version string with a minimum version
#'
#' MariaDB reports its version either directly (\code{"10.6.23-MariaDB"},
#' distribution builds append a package suffix such as
#' \code{"-0ubuntu0.22.04.1"} or \code{"-0+deb12u1"}) or behind the
#' \code{"5.5.5-"} replication-protocol prefix that MySQL client libraries
#' show (\code{"5.5.5-10.11.6-MariaDB"}). The server version is the token
#' immediately before \code{"-MariaDB"}; without that tag the protocol prefix
#' is dropped and the first version is read. A package suffix such as
#' \code{"0.22.04"} is never taken for the server version.
#'
#' @param version Version string reported by the server.
#' @param minimum Minimum version, e.g. \code{"10.2.0"}.
#' @param family \code{"mariadb"} or another engine family.
#' @return \code{TRUE} when the version is verified to be at least
#'   \code{minimum}.
#' @keywords internal
.databaseVersionAtLeast <- function(version, minimum, family) {
  candidate <- .databaseVersionNumber(version, family)
  if (is.null(candidate)) return(FALSE)
  parsed <- tryCatch(numeric_version(candidate), error = function(e) NULL)
  !is.null(parsed) && parsed >= numeric_version(minimum)
}

#' Enforce the common analytic SQL baseline for MySQL-family engines
#'
#' dsOMOP's longitudinal and recipe contracts use CTEs and window functions.
#' Verify their vendor minimum once, at connection bootstrap and again in SQL
#' compilers used by synthetic test handles, so an unsupported server never
#' fails halfway through a private request.
#'
#' @param handle CDM handle.
#' @param context User-facing operation label.
#' @return \code{TRUE}, invisibly.
#' @keywords internal
.assertAnalyticDbmsSupport <- function(handle, context = "dsOMOP analytic SQL") {
  dbms <- .normalizeDBMS(handle$dbms %||% handle$target_dialect %||% "")
  if (!dbms %in% c("mysql", "mariadb")) return(invisible(TRUE))

  version <- .databaseServerVersion(handle)
  family <- if (identical(dbms, "mariadb") ||
                (!is.null(version) && grepl("mariadb", version,
                                           ignore.case = TRUE))) {
    "mariadb"
  } else {
    "mysql"
  }
  minimum <- if (identical(family, "mariadb")) "10.2.0" else "8.0.0"
  if (!.databaseVersionAtLeast(version, minimum, family)) {
    stop(context, " requires a verified ",
         if (identical(family, "mariadb")) "MariaDB >= 10.2" else
           "MySQL >= 8.0",
         " server (CTE and window-function baseline); the connected version ",
         "could not be verified as compatible.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Remove NULL DBI connection arguments
#'
#' Passing an explicit NULL is not equivalent to omitting an argument for all
#' DBI drivers (notably their port and credential validators).
#' @param args Named list of DBI connection arguments.
#' @return Named list without NULL entries.
#' @keywords internal
.compactConnectionArgs <- function(args) {
  args[!vapply(args, is.null, logical(1))]
}

#' Format a host and optional port for driver connection strings
#' @param host Host name.
#' @param port Optional integer port.
#' @param separator Separator used by the driver.
#' @return Formatted endpoint.
#' @keywords internal
.hostWithOptionalPort <- function(host, port = NULL, separator = ":") {
  if (is.null(host) || !nzchar(host)) return(host)
  if (is.null(port)) return(host)
  endpoint_host <- if (grepl(":", host, fixed = TRUE) &&
                       !startsWith(host, "[")) paste0("[", host, "]") else host
  paste0(endpoint_host, separator, port)
}

# Transport security is opt-in for every database: dsOMOP never requires TLS
# on its own. The DataSHIELD server and the database both sit inside the data
# owner's domain, often on one machine or one Docker network, and most OMOP
# databases there (OHDSI Broadsea, a plain postgres container) do not offer TLS.
# A resource that leaves its TLS settings empty connects the way the driver
# does by default, as dsOMOP 1.x did; a resource that names a TLS mode gets
# exactly that mode.

#' Whether a DBI connection or result is still usable
#'
#' \code{DBI::dbIsValid()} for drivers that implement it. RPostgreSQL, the
#' PostgreSQL driver some DataSHIELD R servers carry instead of RPostgres, does
#' not: its connections are probed with a trivial query, and its results count
#' as valid until cleared.
#' @param x A DBI connection or result.
#' @return Logical.
#' @keywords internal
.dbIsValid <- function(x) {
  tryCatch(
    isTRUE(DBI::dbIsValid(x)),
    error = function(e) {
      if (!grepl("unable to find an inherited method", conditionMessage(e),
                 fixed = TRUE)) {
        stop(e)
      }
      if (!methods::is(x, "DBIConnection")) return(TRUE)
      isTRUE(tryCatch({
        DBI::dbGetQuery(x, "SELECT 1")
        TRUE
      }, error = function(probe) FALSE))
    }
  )
}

#' PostgreSQL sslmode for a resource
#'
#' The resource's own mode, or NULL when it sets none: no sslmode then reaches
#' libpq, whose default ("prefer", or PGSSLMODE when the server sets it) uses
#' TLS only when the database offers it.
#' @param mode The resource's sslmode, or NULL.
#' @param host Unused; kept for callers that pass the host.
#' @return Character mode or NULL.
#' @keywords internal
.effectivePostgresSslMode <- function(mode, host = NULL) {
  mode
}

#' Whether a MySQL/MariaDB connection must use authenticated TLS
#'
#' Only when the resource asks for it (ssl_required = true) or supplies TLS
#' material (a CA, client certificate or key, or cipher).
#' @param required The resource's ssl_required, or NULL.
#' @param host Unused; kept for callers that pass the host.
#' @param tls_material List of the resource's TLS settings.
#' @return Logical.
#' @keywords internal
.effectiveMariaTlsRequired <- function(required, host = NULL,
                                       tls_material = list()) {
  isTRUE(required) || any(!vapply(tls_material, is.null, logical(1L)))
}

.mariaTlsClientFlag <- function() {
  if (utils::packageVersion("RMariaDB") < "1.3.2") {
    stop("Authenticated MySQL/MariaDB TLS requires RMariaDB >= 1.3.2.",
         call. = FALSE)
  }
  exports <- getNamespaceExports("RMariaDB")
  required <- c("CLIENT_SSL", "CLIENT_SSL_VERIFY_SERVER_CERT")
  if (!all(required %in% exports)) {
    stop("The installed RMariaDB version cannot verify MySQL/MariaDB server ",
         "certificates. Upgrade RMariaDB before enabling TLS.", call. = FALSE)
  }
  bitwOr(
    as.integer(getExportedValue("RMariaDB", "CLIENT_SSL")),
    as.integer(getExportedValue("RMariaDB", "CLIENT_SSL_VERIFY_SERVER_CERT"))
  )
}

.mariaConnectionCallArgs <- function(driver, args) {
  # RMariaDB otherwise reads the [rs-dbi] option-file group. A local option
  # file must not be able to weaken the TLS policy assembled above.
  c(list(driver), list(group = NULL), args)
}

# Keep timestamp decoding and SQL literal parsing independent of PostgreSQL
# server defaults. The resource client calls this for every newly opened
# connection, including reconnects.
.ensurePostgresUtc <- function(connection) {
  ok <- tryCatch({
    DBI::dbExecute(connection, "SET TIME ZONE 'UTC'")
    DBI::dbExecute(connection, "SET standard_conforming_strings TO on")
    timezone <- DBI::dbGetQuery(connection, "SHOW TIME ZONE")
    strings <- DBI::dbGetQuery(connection, "SHOW standard_conforming_strings")
    value <- if (is.data.frame(timezone) && nrow(timezone) == 1L &&
                 ncol(timezone) == 1L) {
      as.character(timezone[[1L]][[1L]])
    } else {
      NA_character_
    }
    string_mode <- if (is.data.frame(strings) && nrow(strings) == 1L &&
                       ncol(strings) == 1L) {
      tolower(as.character(strings[[1L]][[1L]]))
    } else {
      NA_character_
    }
    is.character(value) && length(value) == 1L && !is.na(value) &&
      toupper(value) %in% c("UTC", "ETC/UTC") &&
      identical(string_mode, "on")
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    try(DBI::dbDisconnect(connection), silent = TRUE)
    stop("PostgreSQL connection could not establish a verified UTC session.",
         call. = FALSE)
  }
  connection
}

.ensureMariaSession <- function(connection, require_tls) {
  mode_ok <- tryCatch({
    current <- DBI::dbGetQuery(
      connection, "SELECT @@SESSION.sql_mode AS sql_mode"
    )
    value <- as.character(current[[1L]][[1L]] %||% "")
    modes <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
    modes <- modes[nzchar(modes) & modes != "NO_BACKSLASH_ESCAPES"]
    modes <- unique(c(modes, "ANSI_QUOTES"))
    if (any(!grepl("^[A-Z0-9_]+$", modes))) {
      stop("The MySQL/MariaDB session reported an invalid sql_mode.")
    }
    literal <- as.character(DBI::dbQuoteString(
      connection, paste(modes, collapse = ",")
    ))
    DBI::dbExecute(connection, paste0("SET SESSION sql_mode = ", literal))
    verified <- DBI::dbGetQuery(
      connection, "SELECT @@SESSION.sql_mode AS sql_mode"
    )
    active <- trimws(strsplit(
      as.character(verified[[1L]][[1L]] %||% ""), ",", fixed = TRUE
    )[[1L]])
    "ANSI_QUOTES" %in% active && !"NO_BACKSLASH_ESCAPES" %in% active
  }, error = function(e) FALSE)
  if (!isTRUE(mode_ok)) {
    try(DBI::dbDisconnect(connection), silent = TRUE)
    stop("MySQL/MariaDB connection could not establish the required SQL ",
         "literal and identifier modes.", call. = FALSE)
  }
  if (isTRUE(require_tls)) {
    tls_ok <- tryCatch({
      read_status <- function(name) {
        status <- DBI::dbGetQuery(
          connection, paste0("SHOW STATUS LIKE '", name, "'")
        )
        value_column <- match("value", tolower(names(status)))
        if (nrow(status) != 1L || is.na(value_column)) return(NA_character_)
        as.character(status[[value_column]][[1L]])
      }
      cipher <- read_status("Ssl_cipher")
      version <- read_status("Ssl_version")
      !is.na(cipher) && nzchar(cipher) &&
        version %in% c("TLSv1.2", "TLSv1.3")
    }, error = function(e) FALSE)
    if (!isTRUE(tls_ok)) {
      try(DBI::dbDisconnect(connection), silent = TRUE)
      stop("MySQL/MariaDB resource requires authenticated TLS 1.2 or newer, ",
           "but the session did not verify that transport contract.",
           call. = FALSE)
    }
  }
  connection
}

#' Normalize a Snowflake server name
#' @param host Account identifier or complete Snowflake host name.
#' @return Complete Snowflake host name.
#' @keywords internal
.snowflakeServer <- function(host) {
  if (is.null(host) || !nzchar(host)) return(host)
  if (grepl("\\.snowflakecomputing\\.com$", host, ignore.case = TRUE)) host else
    paste0(host, ".snowflakecomputing.com")
}

#' Parse a readable OMOP CDM resource URL
#'
#' Parses URLs of the form
#' \code{omop+dbi:<dbms>://<host>[:<port>]/<database>?cdm_schema=...&vocabulary_schema=...}.
#' The \code{omop+dbi:} wrapper is optional (a bare \code{<dbms>://...} is
#' accepted). File-backed engines use an empty authority and an absolute path,
#' e.g. \code{omop+dbi:sqlite:///srv/data/omop.sqlite}. Recognized query keys:
#' \code{cdm_schema}, \code{vocabulary_schema}, \code{results_schema},
#' \code{temp_schema}, \code{warehouse}, \code{driver}, and optional
#' driver-native TLS settings (plus a few aliases).
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

  # Host and optional port. Bracketed IPv6 is unambiguous; an unbracketed
  # multi-colon authority is rejected rather than silently treating part of the
  # address as a port.
  host <- authority
  port <- NULL
  if (nzchar(authority)) {
    port_str <- NULL
    if (startsWith(authority, "[")) {
      close <- regexpr("]", authority, fixed = TRUE)
      if (close < 2L) {
        stop("Malformed bracketed IPv6 host in OMOP resource URL.",
             call. = FALSE)
      }
      host <- substring(authority, 2L, close - 1L)
      tail <- substring(authority, close + 1L)
      if (nzchar(tail)) {
        if (!startsWith(tail, ":")) {
          stop("Malformed authority after bracketed IPv6 host.", call. = FALSE)
        }
        port_str <- substring(tail, 2L)
      }
    } else {
      colons <- gregexpr(":", authority, fixed = TRUE)[[1]]
      n_colons <- if (length(colons) == 1L && colons[[1]] == -1L) {
        0L
      } else {
        length(colons)
      }
      if (n_colons > 1L) {
        stop("IPv6 hosts in OMOP resource URLs must use brackets.",
             call. = FALSE)
      }
      if (n_colons == 1L) {
        cpos <- colons[[1]]
        host <- substring(authority, 1L, cpos - 1L)
        port_str <- substring(authority, cpos + 1L)
      }
    }
    if (!is.null(port_str)) {
      if (!grepl("^[0-9]+$", port_str)) {
        stop("OMOP resource URL port must be an integer between 1 and 65535.",
             call. = FALSE)
      }
      port <- suppressWarnings(as.integer(port_str))
      if (is.na(port) || port < 1L || port > 65535L) {
        stop("OMOP resource URL port must be an integer between 1 and 65535.",
             call. = FALSE)
      }
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
  option_value <- function(name, ...) {
    value <- pick(name, ...)
    if (is.null(value)) return(NULL)
    if (grepl("[\\r\\n]", value, perl = TRUE)) {
      stop("OMOP resource URL option '", name,
           "' contains unsupported control characters.", call. = FALSE)
    }
    value
  }
  sslmode <- option_value("sslmode", "ssl_mode")
  if (!is.null(sslmode)) {
    sslmode <- tolower(sslmode)
    allowed_sslmode <- c(
      "disable", "allow", "prefer", "require", "verify-ca", "verify-full"
    )
    if (!sslmode %in% allowed_sslmode) {
      stop("sslmode must be one of: ", paste(allowed_sslmode, collapse = ", "),
           ".", call. = FALSE)
    }
  }
  ssl_required <- option_value("ssl_required", "tls_required")
  if (!is.null(ssl_required)) {
    normalized <- tolower(ssl_required)
    if (!normalized %in% c("true", "false", "1", "0")) {
      stop("ssl_required must be true or false.", call. = FALSE)
    }
    ssl_required <- normalized %in% c("true", "1")
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
    driver            = pick("driver"),
    sslmode           = sslmode,
    sslrootcert       = option_value("sslrootcert", "ssl_root_cert"),
    sslcert           = option_value("sslcert", "ssl_cert"),
    sslkey            = option_value("sslkey", "ssl_key"),
    ssl_required      = ssl_required,
    ssl_ca            = option_value("ssl_ca"),
    ssl_capath        = option_value("ssl_capath"),
    ssl_cipher        = option_value("ssl_cipher")
  )
}

# --- dsOMOP 1.x resources (read-only legacy support) ---
#
# Resources created with the dsOMOP 1.x form (format "omop.cdm.db") keep
# working after an upgrade: their URL is rewritten into the 2.x form, then
# parsed, validated and connected exactly like a 2.x resource. Nothing creates
# them any more: the 2.x form (inst/resources/resource.js) only writes
# "omop.dbi.db" (David, 11 Nov 2026: read the old resources, never declare new
# ones). The 1.x form wrote <driver>://<host>:<port>/<database> for driver
# mariadb, mysql or postgresql, followed, when a schema was given, by
# //dsomop:: and /schema:<cdm> and/or /vocabulary_schema:<vocab>; dsOMOP 1.x
# read each schema with "/schema:([^/]+)" or "/vocabulary_schema:([^/]+)" after
# //dsomop::, and set no TLS option.

.LEGACY_OMOP_FORMAT <- "omop.cdm.db"

#' Whether a resource format is the dsOMOP 1.x one
#' @param format The resource's format.
#' @return Logical.
#' @keywords internal
.isLegacyOmopFormat <- function(format) {
  is.character(format) && length(format) == 1L && !is.na(format) &&
    identical(tolower(trimws(format)), .LEGACY_OMOP_FORMAT)
}

#' Rewrite a dsOMOP 1.x resource URL into the 2.x form
#'
#' Every part is percent-encoded, so no value can add query options of its own
#' (TLS, driver) to the 2.x URL.
#' @param url The 1.x resource URL.
#' @return The equivalent 2.x URL.
#' @keywords internal
.legacyOmopUrl <- function(url) {
  unreadable <- function(why) {
    stop("This dsOMOP 1.x resource (format 'omop.cdm.db') could not be read: ",
         why, ". Re-create it with the dsOMOP 2.x resource form.",
         call. = FALSE)
  }
  if (!is.character(url) || length(url) != 1L || is.na(url) ||
      !nzchar(trimws(url))) {
    unreadable("its URL is empty")
  }
  parts <- strsplit(trimws(url), "//dsomop::", fixed = TRUE)[[1L]]
  if (length(parts) > 2L) unreadable("its URL holds more than one //dsomop::")
  base <- regmatches(parts[[1L]], regexec(
    "^(postgresql|mysql|mariadb)://([^/:?#@[:space:]]+)(:([0-9]+))?/([^/?#]+)$",
    parts[[1L]], ignore.case = TRUE
  ))[[1L]]
  if (length(base) == 0L) {
    unreadable(paste0("expected postgresql://, mysql:// or mariadb:// ",
                      "<host>:<port>/<database>"))
  }
  config <- if (length(parts) == 2L) parts[[2L]] else ""
  setting <- function(key) {
    m <- regmatches(config, regexec(paste0("/", key, ":([^/]+)"), config))[[1L]]
    if (length(m) == 2L) m[[2L]] else NULL
  }
  enc <- function(x) utils::URLencode(x, reserved = TRUE)
  query <- c(cdm_schema = setting("schema"),
             vocabulary_schema = setting("vocabulary_schema"))
  paste0(
    "omop+dbi:", tolower(base[[2L]]), "://", enc(base[[3L]]),
    if (nzchar(base[[5L]])) paste0(":", base[[5L]]) else "",
    "/", enc(base[[6L]]),
    if (length(query) > 0L) {
      paste0("?", paste0(names(query), "=", vapply(query, enc, ""),
                         collapse = "&"))
    } else ""
  )
}

#' OMOP CDM Resource Client
#'
#' R6 class that wraps a DataSHIELD resource pointing to an OMOP CDM database.
#' Manages the DBI connection and extracts configuration from the resource URL.
#'
#' @importFrom R6 R6Class
#' @importFrom DBI dbConnect dbDisconnect dbIsValid
#' @importFrom RSQLite SQLite
#' @keywords internal
OMOPResourceClient <- R6::R6Class(

  "OMOPResourceClient",
  inherit = resourcer::ResourceClient,

  private = list(
    .connection = NULL,
    .parsed = NULL,

    parse_url = function() {
      resource <- self$getResource()
      url <- if (.isLegacyOmopFormat(resource$format)) {
        .legacyOmopUrl(resource$url)
      } else {
        resource$url
      }
      private$.parsed <- .parseOmopUrl(url)
    },

    #' Create a DBI connection directly from parsed URL parameters
    connect_dbi = function() {
      res <- self$getResource()
      p <- private$.parsed
      user <- res$identity
      pass <- res$secret

      dbms <- tolower(p$dbms %||% "")

      if (dbms == "postgresql") {
        sslmode <- .effectivePostgresSslMode(p$sslmode, p$host)
        if (requireNamespace("RPostgres", quietly = TRUE)) {
          args <- .compactConnectionArgs(list(
            host = p$host, port = p$port, dbname = p$database,
            user = user, password = pass, timezone = "UTC",
            sslmode = sslmode, sslrootcert = p$sslrootcert,
            sslcert = p$sslcert, sslkey = p$sslkey))
          connection <- do.call(
            DBI::dbConnect, c(list(RPostgres::Postgres()), args)
          )
          return(.ensurePostgresUtc(connection))
        }
        if (requireNamespace("RPostgreSQL", quietly = TRUE)) {
          # RPostgreSQL cannot pass TLS settings: it serves resources that set
          # none (libpq's default) or disable TLS.
          if (!(is.null(sslmode) || identical(sslmode, "disable")) || any(!vapply(
              list(p$sslrootcert, p$sslcert, p$sslkey),
              is.null, logical(1L)))) {
            stop("Resource-level PostgreSQL TLS options require RPostgres; ",
                 "the legacy RPostgreSQL fallback cannot apply them safely.",
                 call. = FALSE)
          }
          args <- .compactConnectionArgs(list(
            host = p$host, port = p$port, dbname = p$database,
            user = user, password = pass,
            options = "-c timezone=UTC"))
          connection <- do.call(
            DBI::dbConnect, c(list(RPostgreSQL::PostgreSQL()), args)
          )
          return(.ensurePostgresUtc(connection))
        }
        stop("No PostgreSQL driver found. Install RPostgres or RPostgreSQL.",
             call. = FALSE)
      }

      if (dbms == "sqlite") {
        if (!requireNamespace("RSQLite", quietly = TRUE))
          stop("RSQLite package required for SQLite connections.", call. = FALSE)
        return(DBI::dbConnect(SQLite(), dbname = p$database))
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
        driver <- p$driver %||% "ODBC Driver 17 for SQL Server"
        # Microsoft's ODBC Driver 18 encrypts and validates the server
        # certificate unless told otherwise; dsOMOP requires neither.
        microsoft <- grepl("^ODBC Driver [0-9]+ for SQL Server$", driver,
                           ignore.case = TRUE)
        args <- .compactConnectionArgs(list(
          driver = driver,
          server = .hostWithOptionalPort(p$host, p$port, ","),
          database = p$database, uid = user, pwd = pass,
          Encrypt = if (microsoft) "no" else NULL,
          TrustServerCertificate = if (microsoft) "yes" else NULL))
        return(do.call(DBI::dbConnect, c(list(odbc::odbc()), args)))
      }

      if (dbms %in% c("mysql", "mariadb")) {
        if (!requireNamespace("RMariaDB", quietly = TRUE))
          stop("RMariaDB package required for MySQL/MariaDB.", call. = FALSE)
        require_tls <- .effectiveMariaTlsRequired(
          p$ssl_required, p$host,
          list(p$ssl_ca, p$ssl_capath, p$sslcert, p$sslkey, p$ssl_cipher)
        )
        args <- .compactConnectionArgs(list(
          host = p$host, port = p$port, dbname = p$database,
          username = user, password = pass, timezone = "+00:00",
          client.flag = if (require_tls) .mariaTlsClientFlag() else NULL,
          ssl.key = p$sslkey, ssl.cert = p$sslcert, ssl.ca = p$ssl_ca,
          ssl.capath = p$ssl_capath, ssl.cipher = p$ssl_cipher))
        connection <- do.call(
          DBI::dbConnect,
          .mariaConnectionCallArgs(RMariaDB::MariaDB(), args)
        )
        return(.ensureMariaSession(connection, require_tls))
      }

      if (dbms == "oracle") {
        # Prefer ROracle (requires Oracle Instant Client), fallback to odbc
        if (requireNamespace("ROracle", quietly = TRUE)) {
          drv <- DBI::dbDriver("Oracle")
          connect_string <- paste0(
            "(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)(HOST=", p$host,
            ")(PORT=", p$port %||% 1521L,
            "))(CONNECT_DATA=(SID=", p$database, ")))"
          )
          args <- .compactConnectionArgs(list(
            username = user, password = pass, dbname = connect_string))
          return(do.call(DBI::dbConnect, c(list(drv), args)))
        }
        if (requireNamespace("odbc", quietly = TRUE)) {
          args <- .compactConnectionArgs(list(
            driver = p$driver %||% "Oracle",
            DBQ = paste0(.hostWithOptionalPort(p$host, p$port),
                         "/", p$database),
            UID = user, PWD = pass))
          return(do.call(DBI::dbConnect, c(list(odbc::odbc()), args)))
        }
        stop("Oracle requires ROracle (with Oracle Instant Client) or odbc package.",
             call. = FALSE)
      }

      if (dbms == "redshift") {
        # Redshift is PostgreSQL wire-compatible
        if (!requireNamespace("RPostgres", quietly = TRUE))
          stop("RPostgres package required for Redshift connections.", call. = FALSE)
        args <- .compactConnectionArgs(list(
          host = p$host, port = p$port, dbname = p$database,
          user = user, password = pass))
        return(do.call(DBI::dbConnect, c(list(RPostgres::Postgres()), args)))
      }

      if (dbms == "bigquery") {
        if (!requireNamespace("bigrquery", quietly = TRUE))
          stop("bigrquery package required for BigQuery connections.", call. = FALSE)
        project <- p$host  # use host field for GCP project ID
        args <- .compactConnectionArgs(list(
          project = project, dataset = p$database))
        return(do.call(DBI::dbConnect, c(list(bigrquery::bigquery()), args)))
      }

      if (dbms == "snowflake") {
        if (!requireNamespace("odbc", quietly = TRUE))
          stop("odbc package required for Snowflake connections.", call. = FALSE)
        args <- .compactConnectionArgs(list(
          driver = p$driver %||% "Snowflake",
          server = .snowflakeServer(p$host), database = p$database,
          uid = user, pwd = pass,
          warehouse = p$warehouse %||% "COMPUTE_WH"))
        return(do.call(DBI::dbConnect, c(list(odbc::odbc()), args)))
      }

      if (dbms %in% c("spark", "databricks")) {
        if (!requireNamespace("odbc", quietly = TRUE))
          stop("odbc package required for Spark/Databricks connections.", call. = FALSE)
        # Databricks uses its own ODBC driver; classic Spark uses Simba
        driver <- p$driver %||% (if (dbms == "databricks") "Databricks" else "Simba Spark ODBC Driver")
        args <- .compactConnectionArgs(list(
          driver = driver, host = p$host, port = p$port,
          database = p$database, uid = user, pwd = pass))
        return(do.call(DBI::dbConnect, c(list(odbc::odbc()), args)))
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
    #' @param ... Reserved for compatibility with the parent resource client.
    initialize = function(resource, ...) {
      super$initialize(resource)
      private$parse_url()
    },

    #' @description Get or create a DBI connection
    #' @return A DBI connection object
    getConnection = function() {
      if (is.null(private$.connection) || !.dbIsValid(private$.connection)) {
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
#' databases. Matches resources whose format is \code{"omop.dbi.db"}, or the
#' dsOMOP 1.x \code{"omop.cdm.db"} read through \code{.legacyOmopUrl()}, and
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
      tolower(fmt) == "omop.dbi.db" || .isLegacyOmopFormat(fmt)
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
