# Module: SQL Rendering
# SQL rendering utilities for database query execution.

# --- DBMS Dialect Mapping ---

#' Resolve a DBMS name to a target dialect
#'
#' @param dbms Character; DBMS name from resource driver
#' @return Character; target dialect string
#' @keywords internal
.resolve_target_dialect <- function(dbms) {
  if (!is.character(dbms) || length(dbms) != 1L || is.na(dbms) ||
      !nzchar(trimws(dbms))) {
    stop("dbms must be one non-empty character value.", call. = FALSE)
  }
  dbms <- tolower(trimws(dbms))
  mapping <- list(
    postgresql = "postgresql",
    postgres   = "postgresql",
    `sql server` = "sql server",
    sql_server = "sql server",
    sqlserver  = "sql server",
    synapse    = "sql server",
    pdw        = "sql server",
    oracle     = "oracle",
    redshift   = "redshift",
    bigquery   = "bigquery",
    snowflake  = "snowflake",
    spark      = "spark",
    databricks = "spark",
    sqlite     = "sqlite",
    duckdb     = "duckdb",
    mysql      = "mysql",
    mariadb    = "mysql"
  )
  dialect <- mapping[[dbms]]
  if (is.null(dialect)) {
    stop("Unsupported DBMS: '", dbms, "'. Supported: ",
         paste(unique(unlist(mapping)), collapse = ", "), call. = FALSE)
  }
  dialect
}

#' Resolve a DBMS name to OHDSI SqlRender's exact target dialect
#'
#' This mapping is intentionally separate from
#' \code{\link{.resolve_target_dialect}}. The latter preserves dsOMOP's legacy
#' built-in translation groups (for example, Synapse and PDW use the SQL Server
#' subset), whereas SqlRender has distinct targets for those platforms.
#' MySQL and MariaDB are deliberately absent because they are not OHDSI
#' SqlRender target dialects; dsOMOP supports them only through reviewed local
#' SQL contracts.
#'
#' @param dbms Character; DBMS name or alias.
#' @return A SqlRender target dialect, or \code{NULL} for the reviewed
#'   MySQL/MariaDB extension.
#' @keywords internal
.resolveSqlRenderDialect <- function(dbms) {
  canonical <- .normalizeDBMS(dbms)
  if (is.null(canonical) || !nzchar(canonical)) {
    stop("dbms must be one non-empty character value.", call. = FALSE)
  }
  mapping <- list(
    postgresql = "postgresql",
    sqlserver  = "sql server",
    synapse    = "synapse",
    pdw        = "pdw",
    oracle     = "oracle",
    redshift   = "redshift",
    bigquery   = "bigquery",
    snowflake  = "snowflake",
    spark      = "spark",
    databricks = "spark",
    sqlite     = "sqlite",
    duckdb     = "duckdb",
    mysql      = NULL,
    mariadb    = NULL
  )
  if (!canonical %in% names(mapping)) {
    stop("Unsupported DBMS: '", canonical, "'.", call. = FALSE)
  }
  mapping[[canonical]]
}

#' Inspect the optional SqlRender runtime without overstating support
#'
#' @param dbms Character DBMS name.
#' @param inspect_runtime Logical; load SqlRender and inspect its live dialect
#'   registry. Static capability reporting sets this to FALSE so unrelated
#'   database paths never initialize rJava.
#' @return Named list with the declared target and actual runtime availability.
#' @keywords internal
.sqlRenderRuntimeInfo <- function(dbms, inspect_runtime = TRUE) {
  if (!is.logical(inspect_runtime) || length(inspect_runtime) != 1L ||
      is.na(inspect_runtime)) {
    stop("inspect_runtime must be TRUE or FALSE.", call. = FALSE)
  }
  target <- .resolveSqlRenderDialect(dbms)
  package_path <- suppressWarnings(tryCatch(
    find.package("SqlRender", quiet = TRUE), error = function(e) ""
  ))
  installed <- is.character(package_path) && length(package_path) == 1L &&
    nzchar(package_path)
  loadable <- if (installed && inspect_runtime) {
    suppressMessages(withCallingHandlers(
      tryCatch(requireNamespace("SqlRender", quietly = TRUE),
               error = function(e) FALSE),
      warning = function(w) invokeRestart("muffleWarning")
    ))
  } else {
    FALSE
  }
  dialects <- if (loadable) {
    tryCatch(
      tolower(as.character(SqlRender::listSupportedDialects()$dialect)),
      error = function(e) character(0)
    )
  } else {
    character(0)
  }
  list(
    target_dialect = target,
    installed = installed,
    version = if (installed) {
      tryCatch(as.character(utils::packageVersion("SqlRender")),
               error = function(e) NULL)
    } else {
      NULL
    },
    target_available = if (inspect_runtime) {
      loadable && !is.null(target) && target %in% dialects
    } else {
      NA
    },
    runtime_inspected = inspect_runtime,
    supported_dialects = dialects
  )
}

#' Detect OHDSI temporary-object tokens
#'
#' OHDSI SQL identifies temporary tables with a leading hash. This deliberately
#' conservative check may reject a hash token in controller-owned SQL rather
#' than risk translating it to a persistent table on an emulating backend.
#'
#' @param sql Character SQL string.
#' @return Logical scalar.
#' @keywords internal
.hasOhdsiTempObject <- function(sql) {
  grepl("(^|[^#A-Za-z0-9_])#[A-Za-z_][A-Za-z0-9_]*",
        sql, perl = TRUE)
}

#' Translate canonical OHDSI SQL with the official SqlRender package
#'
#' This is an explicit, fail-closed path. It never falls back to dsOMOP's small
#' built-in translator when SqlRender is absent or lacks the requested target.
#' MySQL/MariaDB therefore remain a separate reviewed extension. OHDSI temp
#' objects are disabled by default and remain blocked on backends where their
#' translated create/drop lifecycle is persistent or cannot be constrained to
#' the session object (Oracle, BigQuery, Redshift, Snowflake and
#' Spark/Databricks).
#'
#' @param sql One canonical OHDSI SQL string (SQL Server source convention).
#' @param dbms Character target DBMS name.
#' @param allow_temp_objects Logical; allow hash-prefixed OHDSI temp objects on
#'   targets where SqlRender keeps them session-scoped.
#' @return One translated SQL string without SqlRender-specific attributes.
#' @keywords internal
.translateOhdsiSql <- function(sql, dbms, allow_temp_objects = FALSE) {
  if (!is.character(sql) || length(sql) != 1L || is.na(sql) ||
      !nzchar(trimws(sql))) {
    stop("sql must be one non-empty character value.", call. = FALSE)
  }
  if (!is.logical(allow_temp_objects) || length(allow_temp_objects) != 1L ||
      is.na(allow_temp_objects)) {
    stop("allow_temp_objects must be TRUE or FALSE.", call. = FALSE)
  }

  canonical <- .normalizeDBMS(dbms)
  runtime <- .sqlRenderRuntimeInfo(canonical)
  if (is.null(runtime$target_dialect)) {
    stop("OHDSI SqlRender does not support MySQL/MariaDB. Use only dsOMOP's ",
         "reviewed MySQL extension templates for this backend.", call. = FALSE)
  }
  if (!runtime$installed) {
    stop("Canonical OHDSI SQL requires the optional SqlRender package; no ",
         "built-in fallback is used.", call. = FALSE)
  }
  if (!runtime$target_available) {
    stop("The installed SqlRender does not support target dialect '",
         runtime$target_dialect, "'. Upgrade SqlRender or use a reviewed ",
         "backend-specific dsOMOP query.", call. = FALSE)
  }

  has_temp <- .hasOhdsiTempObject(sql)
  if (has_temp && !allow_temp_objects) {
    stop("OHDSI temporary objects are disabled for this translation. Use ",
         "dsOMOP-owned temporary materialization or explicitly allow a ",
         "session-scoped OHDSI temp workflow.", call. = FALSE)
  }
  if (has_temp && canonical %in% c(
    "oracle", "bigquery", "redshift", "snowflake", "spark", "databricks"
  )) {
    stop("dsOMOP cannot guarantee a temp-only create/drop lifecycle for OHDSI ",
         "temporary objects on DBMS '", canonical, "'; the unsafe lifecycle ",
         "is blocked.", call. = FALSE)
  }

  translated <- tryCatch(
    withCallingHandlers(
      SqlRender::translate(
        sql = sql,
        targetDialect = runtime$target_dialect
      ),
      warning = function(w) {
        stop("SqlRender warning: ", conditionMessage(w), call. = FALSE)
      }
    ),
    error = function(e) {
      stop("SqlRender translation failed for '", runtime$target_dialect,
           "': ", conditionMessage(e), call. = FALSE)
    }
  )
  translated <- as.character(translated)
  if (length(translated) != 1L || is.na(translated) ||
      !nzchar(trimws(translated))) {
    stop("SqlRender returned an invalid SQL translation.", call. = FALSE)
  }
  translated
}

#' Render and translate controller-owned canonical OHDSI SQL
#'
#' Parameter values must already have been validated or quoted by the typed
#' caller. Any unresolved \code{@parameter} fails closed.
#'
#' @param handle CDM handle.
#' @param sql Canonical OHDSI SQL template.
#' @param parameters Named list of reviewed template values.
#' @param allow_temp_objects See \code{\link{.translateOhdsiSql}}.
#' @return One translated SQL string.
#' @keywords internal
.renderOhdsiSql <- function(handle, sql, parameters = list(),
                            allow_temp_objects = FALSE) {
  if (!is.list(parameters) ||
      (length(parameters) > 0L && is.null(names(parameters)))) {
    stop("parameters must be a named list.", call. = FALSE)
  }
  if (length(parameters) &&
      (any(!nzchar(names(parameters))) || anyDuplicated(names(parameters)))) {
    stop("parameters must have unique, non-empty names.", call. = FALSE)
  }
  runtime <- .sqlRenderRuntimeInfo(handle$dbms)
  if (is.null(runtime$target_dialect)) {
    stop("OHDSI SqlRender does not support MySQL/MariaDB. Use only dsOMOP's ",
         "reviewed MySQL extension templates for this backend.", call. = FALSE)
  }
  if (!runtime$installed) {
    stop("Canonical OHDSI SQL requires the optional SqlRender package; no ",
         "built-in fallback is used.", call. = FALSE)
  }

  rendered <- tryCatch(
    withCallingHandlers(
      do.call(
        SqlRender::render,
        c(list(sql = sql, warnOnMissingParameters = TRUE), parameters)
      ),
      warning = function(w) {
        stop("SqlRender warning: ", conditionMessage(w), call. = FALSE)
      }
    ),
    error = function(e) {
      stop("SqlRender rendering failed: ", conditionMessage(e), call. = FALSE)
    }
  )
  rendered <- as.character(rendered)
  if (length(rendered) != 1L || is.na(rendered) ||
      grepl("@[A-Za-z_][A-Za-z0-9_]*", rendered, perl = TRUE)) {
    stop("OHDSI SQL contains an unresolved @parameter.", call. = FALSE)
  }
  .translateOhdsiSql(
    sql = rendered,
    dbms = handle$dbms,
    allow_temp_objects = allow_temp_objects
  )
}

# Return unquoted SQL words and identifier-based calls with their parenthesis
# depth. String/comment contents stay opaque, while quoted calls are retained
# so quoting cannot bypass the call policy. Cross-dialect ambiguous syntax and
# malformed delimiters fail closed before the database sees the statement.
.lexOhdsiReadOnlySql <- function(statement, dbms = NULL) {
  if (!is.character(statement) || length(statement) != 1L ||
      is.na(statement) || !nzchar(trimws(statement))) {
    stop("Canonical OHDSI read queries require one non-empty SQL statement.",
         call. = FALSE)
  }

  chars <- strsplit(enc2utf8(statement), "", fixed = TRUE)[[1L]]
  mysql_dash_comments <- !is.null(dbms) && length(dbms) == 1L &&
    !is.na(dbms) && tolower(trimws(dbms)) %in% c("mysql", "mariadb")
  n <- length(chars)
  i <- 1L
  depth <- 0L
  words <- character(0)
  depths <- integer(0)
  positions <- integer(0)
  call_names <- character(0)
  call_bases <- character(0)
  call_depths <- integer(0)
  call_positions <- integer(0)
  call_word_positions <- integer(0)
  call_quoted <- logical(0)
  call_qualified <- logical(0)
  has_assignment <- FALSE
  identifier_parts <- character(0)
  identifier_positions <- integer(0)
  identifier_quoted <- logical(0)
  last_significant <- "other"
  malformed <- function(what) {
    stop("Canonical OHDSI read query contains ", what, ".", call. = FALSE)
  }
  reset_identifier <- function(kind = "other") {
    identifier_parts <<- character(0)
    identifier_positions <<- integer(0)
    identifier_quoted <<- logical(0)
    last_significant <<- kind
  }
  note_identifier <- function(value, quoted, position) {
    if (!identical(last_significant, "dot")) {
      identifier_parts <<- character(0)
      identifier_positions <<- integer(0)
      identifier_quoted <<- logical(0)
    }
    identifier_parts <<- c(identifier_parts, toupper(value))
    identifier_positions <<- c(identifier_positions, position)
    identifier_quoted <<- c(identifier_quoted, quoted)
    last_significant <<- "identifier"
  }
  note_call <- function(position) {
    if (!identical(last_significant, "identifier") ||
        length(identifier_parts) == 0L) {
      return(invisible(NULL))
    }
    call_names <<- c(call_names, paste(identifier_parts, collapse = "."))
    call_bases <<- c(call_bases, identifier_parts[[length(identifier_parts)]])
    call_depths <<- c(call_depths, depth)
    call_positions <<- c(call_positions, position)
    call_word_positions <<- c(
      call_word_positions,
      identifier_positions[[length(identifier_positions)]]
    )
    call_quoted <<- c(call_quoted, any(identifier_quoted))
    call_qualified <<- c(call_qualified, length(identifier_parts) > 1L)
    invisible(NULL)
  }
  skip_quoted <- function(start, close, what) {
    cursor <- start + 1L
    while (cursor <= n) {
      if (identical(close, "'") && identical(chars[[cursor]], "\\")) {
        malformed("a cross-dialect ambiguous backslash in a string literal")
      }
      if (identical(chars[[cursor]], close)) {
        if (cursor < n && identical(chars[[cursor + 1L]], close)) {
          cursor <- cursor + 2L
        } else {
          return(cursor + 1L)
        }
      } else {
        cursor <- cursor + 1L
      }
    }
    malformed(paste0("an unterminated ", what))
  }
  read_quoted_identifier <- function(start, close, what) {
    cursor <- start + 1L
    value <- character(0)
    while (cursor <= n) {
      if (identical(chars[[cursor]], close)) {
        if (cursor < n && identical(chars[[cursor + 1L]], close)) {
          value <- c(value, close)
          cursor <- cursor + 2L
        } else {
          return(list(
            next_position = cursor + 1L,
            value = paste0(value, collapse = "")
          ))
        }
      } else {
        value <- c(value, chars[[cursor]])
        cursor <- cursor + 1L
      }
    }
    malformed(paste0("an unterminated ", what))
  }

  while (i <= n) {
    current <- chars[[i]]
    following <- if (i < n) chars[[i + 1L]] else ""

    if (current %in% c(" ", "\t", "\r", "\n", "\f")) {
      i <- i + 1L
      next
    }
    if (identical(current, "-") && identical(following, "-")) {
      third <- if (i + 2L <= n) chars[[i + 2L]] else ""
      if (mysql_dash_comments && nzchar(third) &&
          !third %in% c(" ", "\t", "\r", "\n", "\f")) {
        malformed("a cross-dialect ambiguous dash comment")
      }
      i <- i + 2L
      while (i <= n && !chars[[i]] %in% c("\r", "\n")) i <- i + 1L
      next
    }
    if (identical(current, "/") && identical(following, "*")) {
      executable_comment <-
        (i + 2L <= n && identical(chars[[i + 2L]], "!")) ||
        (i + 3L <= n && toupper(chars[[i + 2L]]) == "M" &&
         identical(chars[[i + 3L]], "!"))
      if (executable_comment) malformed("an executable block comment")
      comment_depth <- 1L
      i <- i + 2L
      while (i <= n && comment_depth > 0L) {
        following <- if (i < n) chars[[i + 1L]] else ""
        if (identical(chars[[i]], "/") && identical(following, "*")) {
          malformed("a cross-dialect ambiguous nested block comment")
        } else if (identical(chars[[i]], "*") &&
                   identical(following, "/")) {
          comment_depth <- comment_depth - 1L
          i <- i + 2L
        } else {
          i <- i + 1L
        }
      }
      if (comment_depth != 0L) malformed("an unterminated block comment")
      next
    }
    if (identical(current, "'")) {
      reset_identifier()
      i <- skip_quoted(i, "'", "string literal")
      next
    }
    if (identical(current, '"')) {
      unicode_escape <- i >= 3L && identical(chars[[i - 1L]], "&") &&
        toupper(chars[[i - 2L]]) == "U"
      if (unicode_escape) malformed("a Unicode-escaped identifier")
      identifier <- read_quoted_identifier(
        i, '"', "quoted identifier"
      )
      note_identifier(identifier$value, quoted = TRUE, position = i)
      i <- identifier$next_position
      next
    }
    if (identical(current, "[")) {
      identifier <- read_quoted_identifier(
        i, "]", "bracket-quoted identifier"
      )
      note_identifier(identifier$value, quoted = TRUE, position = i)
      i <- identifier$next_position
      next
    }
    if (identical(current, "`")) {
      identifier <- read_quoted_identifier(
        i, "`", "backtick-quoted identifier"
      )
      note_identifier(identifier$value, quoted = TRUE, position = i)
      i <- identifier$next_position
      next
    }
    if (identical(current, "$")) {
      delimiter_end <- i + 1L
      valid_tag <- delimiter_end <= n &&
        (identical(chars[[delimiter_end]], "$") ||
         grepl("^[A-Za-z_]$", chars[[delimiter_end]]))
      if (valid_tag && !identical(chars[[delimiter_end]], "$")) {
        delimiter_end <- delimiter_end + 1L
        while (delimiter_end <= n &&
               grepl("^[A-Za-z0-9_]$", chars[[delimiter_end]])) {
          delimiter_end <- delimiter_end + 1L
        }
      }
      if (valid_tag && delimiter_end <= n &&
          identical(chars[[delimiter_end]], "$")) {
        delimiter <- paste0(chars[i:delimiter_end], collapse = "")
        remainder <- if (delimiter_end < n) {
          paste0(chars[(delimiter_end + 1L):n], collapse = "")
        } else {
          ""
        }
        closing <- regexpr(delimiter, remainder, fixed = TRUE)[[1L]]
        if (closing < 1L) malformed("an unterminated dollar-quoted literal")
        reset_identifier()
        i <- delimiter_end + as.integer(closing) + nchar(delimiter)
        next
      }
    }
    if (identical(current, "(")) {
      note_call(i)
      reset_identifier()
      depth <- depth + 1L
      i <- i + 1L
      next
    }
    if (identical(current, ")")) {
      depth <- depth - 1L
      if (depth < 0L) malformed("unbalanced parentheses")
      reset_identifier()
      i <- i + 1L
      next
    }
    if (identical(current, ".")) {
      if (identical(last_significant, "identifier")) {
        last_significant <- "dot"
      } else {
        reset_identifier()
      }
      i <- i + 1L
      next
    }
    if (identical(current, ":") && identical(following, "=")) {
      has_assignment <- TRUE
      reset_identifier()
      i <- i + 2L
      next
    }
    if (grepl("^[A-Za-z_]$", current)) {
      end <- i + 1L
      while (end <= n && grepl("^[A-Za-z0-9_$]$", chars[[end]])) {
        end <- end + 1L
      }
      word <- toupper(paste0(chars[i:(end - 1L)], collapse = ""))
      words <- c(words, word)
      depths <- c(depths, depth)
      positions <- c(positions, i)
      note_identifier(word, quoted = FALSE, position = i)
      i <- end
      next
    }
    reset_identifier()
    i <- i + 1L
  }
  if (depth != 0L) malformed("unbalanced parentheses")
  tokens <- data.frame(
    word = words, depth = depths, position = positions,
    stringsAsFactors = FALSE
  )
  attr(tokens, "calls") <- data.frame(
    name = call_names, base = call_bases, depth = call_depths,
    position = call_positions, word_position = call_word_positions,
    quoted = call_quoted,
    qualified = call_qualified, stringsAsFactors = FALSE
  )
  attr(tokens, "has_assignment") <- has_assignment
  tokens
}

# This is deliberately a deny-list of database-native calls that can mutate
# state, hold resources, execute dynamic SQL or reach outside the CDM. It is a
# defence-in-depth check for reviewed templates, not a UDF sandbox: arbitrary
# user-defined routines can only be contained by the database principal and
# its EXECUTE/external-access privileges.
.blockedOhdsiReadCall <- function(calls) {
  if (!is.data.frame(calls) || nrow(calls) == 0L) return(character(0))

  exact_bases <- c(
    # PostgreSQL/session and sequence state
    "SET_CONFIG", "SETSEED", "NEXTVAL", "SETVAL", "PG_NOTIFY",
    "PG_EXPORT_SNAPSHOT", "PG_IMPORT_SYSTEM_COLLATIONS",
    # MySQL/MariaDB locks, waits, session state and server-file access
    "GET_LOCK", "RELEASE_LOCK", "RELEASE_ALL_LOCKS", "SLEEP",
    "BENCHMARK", "LOAD_FILE", "MASTER_POS_WAIT", "MASTER_GTID_WAIT",
    "SOURCE_POS_WAIT", "WAIT_FOR_EXECUTED_GTID_SET", "LAST_INSERT_ID",
    # SQL Server external rowsets and server-file readers
    "OPENROWSET", "OPENQUERY", "OPENDATASOURCE", "OPENXML",
    "FN_GET_AUDIT_FILE", "FN_XE_FILE_TARGET_READ_FILE",
    "FN_TRACE_GETTABLE",
    # SQLite extension loading and shell-extension file helpers
    "LOAD_EXTENSION", "READFILE", "WRITEFILE", "FTS3_TOKENIZER",
    # Dynamic/external execution surfaces shared by supported dialects
    "QUERY", "TO_QUERY", "EXTERNAL_QUERY", "EVAL", "SYSTEM",
    "EXECUTE_IMMEDIATE", "SYS_EXEC", "SYS_EVAL", "REFLECT",
    "JAVA_METHOD", "HTTPURITYPE", "DBURITYPE"
  )
  base_patterns <- c(
    paste0(
      "^PG_(TERMINATE_BACKEND|CANCEL_BACKEND|RELOAD_CONF|",
      "ROTATE_LOGFILE|SWITCH_WAL|CREATE_RESTORE_POINT|PROMOTE|",
      "WAL_REPLAY_(PAUSE|RESUME)|LOGICAL_EMIT_MESSAGE|",
      "BACKUP_(START|STOP))$"
    ),
    "^PG_(TRY_)?ADVISORY_",
    "^PG_(SLEEP($|_)|READ_FILE$|READ_BINARY_FILE$|STAT_FILE$|LS_)",
    "^PG_(CREATE|DROP|COPY)_(PHYSICAL_|LOGICAL_)?REPLICATION_SLOT",
    "^PG_(LOGICAL_SLOT|REPLICATION_SLOT|REPLICATION_ORIGIN)_",
    "^DBLINK($|_)",
    "^(LO_|LOREAD$|LOWRITE$)",
    "^(XP_|SP_)",
    "^SYSTEM\\$",
    paste0(
      "^(READ_(CSV.*|JSON.*|NDJSON.*|PARQUET|TEXT|BLOB|XLSX)|",
      "PARQUET_(SCAN|METADATA|SCHEMA|FILE_METADATA|KV_METADATA)|",
      "CSV_SCAN|JSON_SCAN|GLOB|SQLITE_(SCAN|QUERY)|",
      "POSTGRES_(SCAN|SCAN_PUSHDOWN|QUERY)|MYSQL_(SCAN|QUERY)|",
      "DELTA_SCAN|ICEBERG_SCAN)$"
    )
  )
  full_patterns <- c(
    paste0(
      "(^|\\.)(UTL_(FILE|HTTP|TCP|SMTP|INADDR)|",
      "DBMS_(LOCK|PIPE|SESSION|SCHEDULER|JOB|SQL|SYSTEM|ALERT|",
      "AQ|AQADM|LDAP|XSLPROCESSOR))\\."
    ),
    "(^|\\.)DBMS_RANDOM\\.SEED$",
    paste0(
      "(^|\\.)DBMS_LOB\\.(WRITE|WRITEAPPEND|APPEND|COPY|ERASE|",
      "TRIM|CREATETEMPORARY|FREETEMPORARY|OPEN|CLOSE|FILEOPEN|",
      "FILECLOSE|LOADFROMFILE|FRAGMENT_.*)$"
    )
  )

  blocked <- calls$base %in% exact_bases
  for (pattern in base_patterns) {
    blocked <- blocked | grepl(pattern, calls$base, perl = TRUE)
  }
  for (pattern in full_patterns) {
    blocked <- blocked | grepl(pattern, calls$name, perl = TRUE)
  }
  hit <- which(blocked)
  if (!length(hit)) character(0) else calls$name[[hit[[1L]]]]
}

.assertOhdsiReadOnlySql <- function(statement, dbms = NULL) {
  tokens <- .lexOhdsiReadOnlySql(statement, dbms = dbms)
  calls <- attr(tokens, "calls")
  first <- if (nrow(tokens)) tokens$word[[1L]] else ""
  if (!first %in% c("SELECT", "WITH")) {
    stop("Canonical OHDSI read queries may contain only SELECT or WITH.",
         call. = FALSE)
  }

  forbidden <- c(
    "INSERT", "UPDATE", "DELETE", "MERGE", "UPSERT", "REPLACE",
    "CREATE", "ALTER", "DROP", "TRUNCATE", "RENAME", "COMMENT",
    "GRANT", "REVOKE", "CALL", "EXEC", "EXECUTE", "DO",
    "COPY", "LOAD", "UNLOAD", "IMPORT", "EXPORT", "INTO",
    "ATTACH", "DETACH", "PRAGMA", "VACUUM"
  )
  blocked_index <- tokens$word %in% forbidden
  replace_calls <- calls$word_position[calls$base == "REPLACE"]
  blocked_index <- blocked_index & !(
    tokens$word == "REPLACE" & tokens$position %in% replace_calls
  )
  blocked <- tokens$word[blocked_index]
  if (length(blocked)) {
    stop("Canonical OHDSI read queries may not contain DML, DDL, SELECT INTO ",
         "or data-transfer commands (found '", blocked[[1L]], "').",
         call. = FALSE)
  }
  if (isTRUE(attr(tokens, "has_assignment"))) {
    stop("Canonical OHDSI read queries may not assign session variables.",
         call. = FALSE)
  }
  token_stream <- paste(tokens$word, collapse = " ")
  if (any(tokens$word == "NEXTVAL") ||
      grepl("(^| )NEXT VALUE FOR( |$)", token_stream, perl = TRUE)) {
    stop("Canonical OHDSI read queries may not advance database sequences.",
         call. = FALSE)
  }
  blocked_call <- .blockedOhdsiReadCall(calls)
  if (length(blocked_call)) {
    stop("Canonical OHDSI read queries may not call side-effecting, dynamic, ",
         "resource-control or external-access functions (found '",
         blocked_call[[1L]], "').", call. = FALSE)
  }
  if (identical(first, "WITH") &&
      !any(tokens$word == "SELECT" & tokens$depth == 0L)) {
    stop("Canonical OHDSI WITH queries must terminate in a top-level SELECT.",
         call. = FALSE)
  }
  invisible(statement)
}

#' Execute one read-only canonical OHDSI query
#'
#' Multiple statements, non-query statements and known side-effecting calls are
#' rejected. DDL and temporary-object lifecycles must go through dsOMOP's owned
#' materialization helpers instead. This internal helper accepts only installed
#' or controller-owned reviewed templates; the resource's database principal
#' must be read-only and unable to execute untrusted routines or external-I/O
#' functions because textual SQL inspection cannot prove arbitrary UDF purity.
#'
#' @param handle CDM handle.
#' @param sql Canonical OHDSI SQL template.
#' @param parameters Named list of reviewed template values.
#' @return Data frame returned by DBI.
#' @keywords internal
.queryOhdsiSql <- function(handle, sql, parameters = list()) {
  translated <- .renderOhdsiSql(
    handle = handle,
    sql = sql,
    parameters = parameters,
    allow_temp_objects = FALSE
  )
  statements <- SqlRender::splitSql(translated)
  if (length(statements) != 1L) {
    stop("Canonical OHDSI read queries must translate to exactly one SQL ",
         "statement.", call. = FALSE)
  }
  statement <- trimws(as.character(statements[[1L]]))
  .assertOhdsiReadOnlySql(statement, dbms = handle$dbms)
  .coerce_integer64(
    .withDbReconnect(
      handle,
      function(conn) DBI::dbGetQuery(conn, statement)
    )
  )
}

# --- Core SQL Execution ---

#' Render, translate, and execute SQL (no result set)
#'
#' @param handle CDM handle
#' @param sql Character; OHDSI SQL with \code{@param} placeholders
#' @param ... Named parameters for substitution
#' @return Invisible \code{NULL}; called for side effects.
#' @keywords internal
.execSql <- function(handle, sql, ...) {
  rendered <- .sql_render(sql, ...)
  translated <- .sql_translate(rendered, handle$target_dialect)
  statements <- .sql_split(translated)
  conn <- .conn(handle)
  for (stmt in statements) {
    DBI::dbExecute(conn, stmt)
  }
  invisible(NULL)
}

#' Render, translate, and query SQL (returns data.frame)
#'
#' @param handle CDM handle
#' @param sql Character; OHDSI SQL with \code{@param} placeholders
#' @param ... Named parameters for substitution
#' @return Data frame
#' @keywords internal
.querySql <- function(handle, sql, ...) {
  rendered <- .sql_render(sql, ...)
  translated <- .sql_translate(rendered, handle$target_dialect)
  .coerce_integer64(
    .withDbReconnect(handle, function(conn) DBI::dbGetQuery(conn, translated)))
}

#' Render and translate SQL (returns SQL string, no execution)
#'
#' @param handle CDM handle
#' @param sql Character; OHDSI SQL with \code{@param} placeholders
#' @param ... Named parameters for substitution
#' @return Character; translated SQL string
#' @keywords internal
.renderSql <- function(handle, sql, ...) {
  rendered <- .sql_render(sql, ...)
  .sql_translate(rendered, handle$target_dialect)
}
