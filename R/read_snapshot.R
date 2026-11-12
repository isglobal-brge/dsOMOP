# Module: one database read snapshot per request (dsOMOP 2.10.3, review R03)
#
# Every final request that reads the CDM or the results schema (catalogue and
# QueryLibrary runs, Achilles and OHDSI readers with their companion support,
# the exploration releases) and every loader whose output is sealed for later
# releases runs all of its source reads inside ONE database snapshot, so its
# support checks, owners, overlays and the fixed-synopsis generation describe
# one data state. Nothing is retained between requests: the context lives on
# the handle only while the request runs.
#
#   * PostgreSQL: a transaction-level REPEATABLE READ transaction (not READ
#     ONLY, so the request's temporary tables still work). Every statement runs
#     under a SAVEPOINT, so a deliberately failing probe rolls back to it
#     instead of aborting the snapshot.
#   * MySQL and MariaDB: InnoDB sources use REPEATABLE READ with START
#     TRANSACTION WITH CONSISTENT SNAPSHOT. InnoDB evaluates the SELECT of
#     CREATE TABLE ... SELECT as a
#     locking read of the latest committed rows, not of the snapshot, so inside
#     a snapshot a temporary table is filled from a consistent read through R.
#     Nontransactional source tables and statements that would commit
#     implicitly are refused. Actual source graphs (including cross-schema
#     nested/definer views) are certified under transaction metadata locks,
#     with current definitions and engines rechecked before release.
#   * DuckDB and SQLite: an explicit transaction (DuckDB has no savepoints: a
#     failing statement aborts its transaction, and the request then fails).
#   * SQL Server: SERIALIZABLE, with range locks; Oracle: SERIALIZABLE,
#     refusing implicit-commit DDL; Redshift: its native transaction snapshot.
#     These adapters require no database option; vendor execution remains an
#     explicit deployment check when those services are unavailable.
#   * Engines without a native adapter: no snapshot is opened; the connection
#     is still pinned and labels report read_snapshot = "not_isolated".
#
# The physical connection is pinned for the whole request; a lost connection
# never resumes a single statement on a new snapshot: the request fails with
# one stable error and can be retried as a whole. Nested calls reuse the
# established snapshot.

# Snapshot kind per canonical DBMS (resource.R's .normalizeDBMS names).
.DSOMOP_READ_SNAPSHOT_KINDS <- c(
  postgresql = "repeatable_read_transaction",
  mysql = "consistent_snapshot_transaction",
  mariadb = "consistent_snapshot_transaction",
  duckdb = "snapshot_transaction",
  sqlite = "serializable_transaction",
  sqlserver = "sqlserver_serializable_transaction",
  oracle = "oracle_serializable_transaction",
  redshift = "redshift_snapshot_transaction"
)

.DSOMOP_READ_SNAPSHOT_ERROR <- paste0(
  "The database could not be read consistently for this request (its ",
  "connection or read snapshot was lost); no result was released. Retry the ",
  "request.")

#' Read-snapshot capability of a handle's database engine
#'
#' @param handle CDM handle.
#' @return One of \code{"repeatable_read_transaction"} (PostgreSQL),
#'   \code{"consistent_snapshot_transaction"} (MySQL, MariaDB),
#'   \code{"snapshot_transaction"} (DuckDB), \code{"serializable_transaction"}
#'   (SQLite), \code{"sqlserver_serializable_transaction"},
#'   \code{"oracle_serializable_transaction"},
#'   \code{"redshift_snapshot_transaction"}, or \code{"not_isolated"}
#'   (an engine without a native adapter: statements can see different states).
#' @keywords internal
.dsomopReadSnapshotKind <- function(handle) {
  dbms <- tryCatch(.normalizeDBMS(handle$dbms %||% ""),
                   error = function(e) "")
  kind <- unname(.DSOMOP_READ_SNAPSHOT_KINDS[dbms %||% ""])
  if (length(kind) != 1L || is.na(kind)) "not_isolated" else kind
}

# Is a request snapshot established on this handle?
.dsomopReadSnapshotActive <- function(handle) {
  is.environment(handle) && is.environment(handle$read_snapshot)
}

#' Public read-consistency label of the current request
#'
#' @param handle CDM handle.
#' @return The snapshot kind of the active request snapshot, or the engine's
#'   capability when called outside one.
#' @keywords internal
.dsomopReadSnapshotLabel <- function(handle) {
  if (.dsomopReadSnapshotActive(handle)) {
    return(handle$read_snapshot$kind)
  }
  if (is.environment(handle)) .dsomopReadSnapshotKind(handle) else
    "not_isolated"
}

# One stable analyst error; the reason goes to the server log only.
.dsomopSnapshotLost <- function(reason) {
  message("[dsomop-read-snapshot] ", reason)
  stop(structure(
    list(message = .DSOMOP_READ_SNAPSHOT_ERROR, call = NULL),
    class = c("dsomop_snapshot_lost", "error", "condition")))
}

#' Run one request inside a single database read snapshot
#'
#' Opens the engine's snapshot on the handle's live connection, pins that
#' connection, evaluates \code{expr} and rolls back on every exit. A nested call evaluates \code{expr} inside the established
#' snapshot. Nothing outlives the call.
#'
#' @param handle CDM handle.
#' @param expr The request body (evaluated lazily, once).
#' @return The value of \code{expr}.
#' @keywords internal
.dsomopWithReadSnapshot <- function(handle, expr) {
  if (!is.environment(handle) || .dsomopReadSnapshotActive(handle)) {
    return(expr)
  }
  kind <- .dsomopReadSnapshotKind(handle)
  ctx <- new.env(parent = emptyenv())
  ctx$kind <- kind
  ctx$dbms <- tryCatch(.normalizeDBMS(handle$dbms), error = function(e) "")
  ctx$schemas <- unique(c(handle$cdm_schema, handle$vocab_schema, handle$results_schema))
  ctx$memo <- new.env(parent = emptyenv())
  ctx$sources <- new.env(parent = emptyenv())
  ctx$unavailable <- FALSE
  ctx$finalizers <- list()
  success <- FALSE
  ctx$savepoints <- 0L
  ctx$aborted <- FALSE
  ctx$conn <- .conn(handle)
  ctx$transaction <- !identical(kind, "not_isolated")
  if (ctx$transaction) {
    # Resolve (and if necessary reconnect) BEFORE the snapshot starts; from
    # here on this physical connection serves every statement.
    .dsomopSnapshotBegin(ctx)
  }
  handle$read_snapshot <- ctx
  on.exit({
    if (identical(handle$read_snapshot, ctx)) handle$read_snapshot <- NULL
    if (ctx$transaction) {
      try(.dsomopSnapshotRollback(ctx), silent = TRUE)
    }
    if (!is.null(ctx$restore_isolation_sql)) {
      try(DBI::dbExecute(ctx$conn, ctx$restore_isolation_sql), silent = TRUE)
    }
    cleanup_error <- NULL
    for (finalizer in ctx$finalizers) {
      tryCatch(finalizer(success), error = function(e) {
        if (is.null(cleanup_error)) cleanup_error <<- e
      })
    }
    if (!is.null(cleanup_error)) {
      if (success) {
        for (finalizer in ctx$finalizers) try(finalizer(FALSE), silent = TRUE)
      }
      stop(cleanup_error)
    }
  }, add = TRUE)
  value <- tryCatch(expr, error = function(e) {
    if (isTRUE(ctx$unavailable)) {
      .dsomopSnapshotUnavailable(ctx, "source certification failed inside the request")
    }
    stop(e)
  })
  if (!is.null(ctx$conn)) .dsomopSnapshotValidate(ctx)
  if (ctx$transaction) {
    tryCatch(.dsomopSnapshotRollback(ctx), error = function(e) {
      .dsomopSnapshotLost("the request snapshot could not be rolled back")
    })
    ctx$transaction <- FALSE
  }
  success <- TRUE
  label <- attr(value, "dsomop_protection", exact = TRUE)
  if (is.list(label)) {
    label$read_snapshot <- ctx$kind
    attr(value, "dsomop_protection") <- label
  }
  value
}

# Open the engine's snapshot on ctx$conn.
.dsomopSnapshotBegin <- function(ctx) {
  conn <- ctx$conn
  tryCatch({
    switch(ctx$kind,
      repeatable_read_transaction = {
        DBI::dbBegin(conn)
        DBI::dbExecute(conn,
                       "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ")
        # The first statement takes the transaction snapshot; its timestamp
        # later proves that no inner COMMIT ended the transaction.
        ctx$token <- DBI::dbGetQuery(conn, paste(
          "SELECT CAST(transaction_timestamp() AS TEXT) AS t"))$t[[1L]]
      },
      consistent_snapshot_transaction = {
        session <- DBI::dbGetQuery(conn, "SELECT DATABASE() AS db, @@session.sql_mode AS sql_mode")
        ctx$default_schema <- session[[1L]][[1L]]
        ctx$sql_mode <- session[[2L]][[1L]]
        # The session isolation applies to the consistent-snapshot
        # transaction below (a session setting, never a database option).
        variable <- if (identical(ctx$dbms, "mariadb")) "tx_isolation" else "transaction_isolation"
        prior <- DBI::dbGetQuery(conn, paste0("SELECT @@session.", variable, " AS isolation"))[[1L]][[1L]]
        prior <- gsub("-", " ", toupper(prior), fixed = TRUE)
        if (!prior %in% c("READ UNCOMMITTED", "READ COMMITTED", "REPEATABLE READ", "SERIALIZABLE")) {
          stop("Unknown connection isolation level.")
        }
        ctx$restore_isolation_sql <- paste0("SET SESSION TRANSACTION ISOLATION LEVEL ", prior)
        DBI::dbExecute(conn,
                       "SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ")
        # RMariaDB commits after every statement unless it opened the
        # transaction itself, so it opens one, which the consistent-snapshot
        # start then replaces (an empty transaction commits implicitly).
        DBI::dbBegin(conn)
        DBI::dbExecute(conn, "START TRANSACTION WITH CONSISTENT SNAPSHOT")
        DBI::dbExecute(conn, "SAVEPOINT dsomop_request_snapshot")
      },
      sqlserver_serializable_transaction = {
        prior <- as.integer(DBI::dbGetQuery(conn, paste(
          "SELECT transaction_isolation_level AS isolation FROM sys.dm_exec_sessions",
          "WHERE session_id = @@SPID"))[[1L]][[1L]])
        levels <- c("READ UNCOMMITTED", "READ COMMITTED", "REPEATABLE READ", "SERIALIZABLE", "SNAPSHOT")
        if (is.na(prior) || !prior %in% seq_along(levels)) stop("Unknown connection isolation level.")
        ctx$restore_isolation_sql <- paste0("SET TRANSACTION ISOLATION LEVEL ", levels[[prior]])
        DBI::dbBegin(conn)
        DBI::dbExecute(conn, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
        DBI::dbExecute(conn, "SAVE TRANSACTION dsomop_request_snapshot")
      },
      oracle_serializable_transaction = {
        DBI::dbBegin(conn)
        DBI::dbExecute(conn, "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
        DBI::dbExecute(conn, "SAVEPOINT dsomop_request_snapshot")
      },
      # Redshift uses native transaction-wide SNAPSHOT or SERIALIZABLE
      # isolation, both sufficient here; no database option needs changing.
      redshift_snapshot_transaction = DBI::dbBegin(conn),
      snapshot_transaction = DBI::dbBegin(conn),
      serializable_transaction = {
        DBI::dbBegin(conn)
        # A first read takes SQLite's shared lock / WAL snapshot now.
        DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM sqlite_master")
      },
      stop("Unknown read-snapshot kind.")
    )
  }, error = function(e) {
    try(DBI::dbRollback(conn), silent = TRUE)
    if (!is.null(ctx$restore_isolation_sql)) {
      try(DBI::dbExecute(conn, ctx$restore_isolation_sql), silent = TRUE)
    }
    if (inherits(e, "dsomop_snapshot_unavailable")) stop(e)
    .dsomopSnapshotLost("the request snapshot could not be opened")
  })
  invisible(TRUE)
}

# A capability refusal is request-wide, even if a caller catches the first
# error while probing a source. Metadata is never cached across requests.
.dsomopSnapshotUnavailable <- function(ctx, reason) {
  ctx$unavailable <- TRUE
  message("[dsomop-read-snapshot] ", reason)
  stop(structure(list(message = paste0(
    "This source has nontransactional tables or unverifiable dependencies and ",
    "cannot provide a request-consistent read snapshot; no result was released."),
    call = NULL), class = c("dsomop_snapshot_unavailable", "error", "condition")))
}

# Tokenize server-normalized view definitions and our generated read SQL.
# Strings and quoted identifiers remain opaque; executable comments and
# ambiguous quoting cannot hide a dependency. No regex is applied to strings.
.dsomopSnapshotSqlTokens <- function(sql, sql_mode = "") {
  modes <- strsplit(toupper(sql_mode), ",", fixed = TRUE)[[1L]]
  no_escape <- "NO_BACKSLASH_ESCAPES" %in% modes
  ansi <- "ANSI_QUOTES" %in% modes
  single <- if (no_escape) "'(?:''|[^'])*'" else "'(?:''|\\\\.|[^'\\\\])*'"
  double <- if (no_escape || ansi) '\"(?:\"\"|[^\"])*\"' else '\"(?:\"\"|\\\\.|[^\"\\\\])*\"'
  pattern <- paste0("(?s)\\s+|--(?=\\s|$)[^\\r\\n]*|#[^\\r\\n]*|/\\*.*?\\*/|",
    single, "|`(?:``|[^`])*`|", double, "|[A-Za-z_][A-Za-z_0-9$]*|[0-9]+|.")
  tokens <- regmatches(sql, gregexpr(pattern, sql, perl = TRUE))[[1L]]
  if (any(grepl("^/\\*(?:!|[Mm]!)", tokens, perl = TRUE))) {
    stop("Executable SQL comments cannot certify source dependencies.")
  }
  tokens <- tokens[!grepl("^(?:\\s|--|#|/\\*)", tokens, perl = TRUE)]
  if (any(tokens %in% c("'", "`", '"'))) stop("Unterminated SQL quoting.")
  quoted <- startsWith(tokens, "`") | (ansi & startsWith(tokens, '"'))
  string <- startsWith(tokens, "'") | (!ansi & startsWith(tokens, '"'))
  id <- quoted | grepl("^[A-Za-z_][A-Za-z_0-9$]*$", tokens)
  value <- tokens
  value[quoted] <- substring(value[quoted], 2L, nchar(value[quoted]) - 1L)
  value[quoted] <- gsub("``", "`", value[quoted], fixed = TRUE)
  value[quoted] <- gsub('""', '"', value[quoted], fixed = TRUE)
  word <- ifelse(id & !quoted, toupper(value), "")
  syntax <- ifelse(!id & !string, value, "")
  data.frame(value = value, word = word, id = id, syntax = syntax, stringsAsFactors = FALSE)
}

# Audited scalar/aggregate/window constructors for view expressions. Unknown
# calls (including native UDFs with hidden file/table reads) are unverifiable.
# SQL syntax and type constructors are included because they also precede '('.
.DSOMOP_SNAPSHOT_VIEW_CALLS <- c(
  "ABS", "ACOS", "ADDDATE", "ADDTIME", "ASCII", "ASIN", "ATAN", "ATAN2", "AVG",
  "BIN", "BIT_AND", "BIT_COUNT", "BIT_LENGTH", "BIT_OR", "BIT_XOR", "CAST",
  "CEIL", "CEILING", "CHAR", "CHAR_LENGTH", "CHARACTER_LENGTH", "COALESCE",
  "COMPRESS", "CONCAT", "CONCAT_WS", "CONV", "CONVERT", "COS", "COT", "COUNT",
  "CRC32", "CUME_DIST", "DATE", "DATEDIFF", "DATE_ADD", "DATE_FORMAT", "DATE_SUB",
  "DAY", "DAYNAME", "DAYOFMONTH", "DAYOFWEEK", "DAYOFYEAR", "DEGREES", "DENSE_RANK",
  "ELT", "EXP", "EXTRACT", "FIELD", "FIND_IN_SET", "FIRST_VALUE", "FLOOR", "FORMAT",
  "FROM_BASE64", "FROM_DAYS", "FROM_UNIXTIME", "GREATEST", "GROUP_CONCAT", "HEX",
  "HOUR", "IF", "IFNULL", "INSTR", "ISNULL", "LAG", "LAST_DAY", "LAST_VALUE",
  "LCASE", "LEAD", "LEAST", "LEFT", "LENGTH", "LN", "LOCATE", "LOG", "LOG10",
  "LOG2", "LOWER", "LPAD", "LTRIM", "MAKEDATE", "MAKETIME", "MAX", "MD5",
  "MICROSECOND", "MID", "MIN", "MOD", "MONTH", "MONTHNAME", "NTH_VALUE", "NTILE",
  "NULLIF", "OCT", "OCTET_LENGTH", "ORD", "PERCENT_RANK", "PERIOD_ADD", "PERIOD_DIFF",
  "PI", "POSITION", "POW", "POWER", "QUARTER", "RADIANS", "RANK", "REGEXP_INSTR",
  "REGEXP_LIKE", "REGEXP_REPLACE", "REGEXP_SUBSTR", "REPEAT", "REPLACE", "REVERSE",
  "RIGHT", "ROUND", "ROW_NUMBER", "RPAD", "RTRIM", "SECOND", "SEC_TO_TIME",
  "SHA", "SHA1", "SHA2", "SIGN", "SIN", "SOUNDEX", "SPACE", "SQRT", "STD",
  "STDDEV", "STDDEV_POP", "STDDEV_SAMP", "STRCMP", "STR_TO_DATE", "SUBDATE",
  "SUBSTR", "SUBSTRING", "SUBSTRING_INDEX", "SUM", "TAN", "TIME", "TIMEDIFF",
  "TIMESTAMP", "TIMESTAMPADD", "TIMESTAMPDIFF", "TIME_FORMAT", "TIME_TO_SEC",
  "TO_BASE64", "TO_DAYS", "TO_SECONDS", "TRIM", "TRUNCATE", "UCASE", "UNCOMPRESS",
  "UNCOMPRESSED_LENGTH", "UNHEX", "UNIX_TIMESTAMP", "UPPER", "VARIANCE", "VAR_POP",
  "VAR_SAMP", "WEEK", "WEEKDAY", "WEEKOFYEAR", "YEAR", "YEARWEEK",
  "JSON_ARRAY", "JSON_ARRAYAGG", "JSON_ARRAY_APPEND", "JSON_ARRAY_INSERT",
  "JSON_CONTAINS", "JSON_CONTAINS_PATH", "JSON_DEPTH", "JSON_EXTRACT", "JSON_INSERT",
  "JSON_KEYS", "JSON_LENGTH", "JSON_MERGE", "JSON_MERGE_PATCH", "JSON_MERGE_PRESERVE",
  "JSON_OBJECT", "JSON_OBJECTAGG", "JSON_OVERLAPS", "JSON_PRETTY", "JSON_QUOTE",
  "JSON_REMOVE", "JSON_REPLACE", "JSON_SEARCH", "JSON_SET", "JSON_TABLE", "JSON_TYPE",
  "JSON_UNQUOTE", "JSON_VALID", "JSON_VALUE",
  "AS", "IN", "EXISTS", "NOT", "ANY", "ALL", "SOME", "OVER", "PARTITION",
  "VALUES", "ROW", "CASE", "WHEN", "THEN", "ELSE", "ON", "USING", "JOIN", "SELECT",
  "UNION", "WITH", "FROM", "WHERE", "HAVING", "BY", "COLUMNS", "CHECK",
  "TINYINT", "SMALLINT", "MEDIUMINT", "INT", "INTEGER", "BIGINT", "DECIMAL", "DEC",
  "FIXED", "NUMERIC", "FLOAT", "DOUBLE", "REAL", "VARCHAR", "NCHAR", "NVARCHAR",
  "BINARY", "VARBINARY", "BIT", "DATETIME", "AND", "OR", "XOR", "IS", "LIKE",
  "REGEXP", "RLIKE", "BETWEEN", "DISTINCT", "LIMIT", "OFFSET", "ORDER", "GROUP",
  "WINDOW", "INDEX", "KEY", "INTERVAL", "GROUPING")

.DSOMOP_SNAPSHOT_GENERATED_CALLS <- c(.DSOMOP_SNAPSHOT_VIEW_CALLS,
  "RAND", "UUID", "UUID_SHORT", "NOW", "SYSDATE", "CURRENT_TIMESTAMP",
  "CURRENT_DATE", "CURRENT_TIME", "CURDATE", "CURTIME", "UTC_DATE", "UTC_TIME",
  "UTC_TIMESTAMP", "LOCALTIME", "LOCALTIMESTAMP", "DATABASE", "SCHEMA", "VERSION",
  "USER", "CURRENT_USER", "SYSTEM_USER", "SESSION_USER", "CONNECTION_ID",
  "FOUND_ROWS", "ROW_COUNT", "LAST_INSERT_ID")

# Enumerate relational references, including comma/parenthesized joins,
# subqueries, UNION arms and CTEs. CTE bindings apply only within their SQL
# scope; only an unqualified reference can resolve to one. In a server view,
# stored routines are schema-qualified and cannot be certified as tables.
.dsomopSnapshotSqlSources <- function(sql, schema, view = FALSE, sql_mode = "") {
  t <- .dsomopSnapshotSqlTokens(sql, sql_mode)
  n <- nrow(t)
  if (view) {
    volatile <- c("RAND", "RANDOM", "UUID", "UUID_SHORT", "SYSDATE", "NOW",
      "CURRENT_TIMESTAMP", "CURRENT_DATE", "CURRENT_TIME", "CURDATE", "CURTIME",
      "UTC_DATE", "UTC_TIME", "UTC_TIMESTAMP", "LOCALTIME", "LOCALTIMESTAMP",
      "LOAD_FILE", "SLEEP", "BENCHMARK", "GET_LOCK", "IS_FREE_LOCK", "IS_USED_LOCK",
      "RELEASE_LOCK", "RELEASE_ALL_LOCKS", "FOUND_ROWS", "ROW_COUNT", "LAST_INSERT_ID")
    if (any(t$word %in% volatile)) stop("Volatile or external view inputs cannot be certified.")
  }
  empty <- data.frame(schema = character(), table = character())
  if (!n) return(empty)
  depth <- integer(n)
  parents <- integer()
  closing <- rep.int(n + 1L, n)
  for (i in seq_len(n)) {
    depth[[i]] <- length(parents)
    if (t$syntax[[i]] == "(") parents <- c(parents, i)
    if (t$syntax[[i]] == ")") {
      if (!length(parents)) stop("Unbalanced source SQL.")
      closing[[tail(parents, 1L)]] <- i
      parents <- head(parents, -1L)
      depth[[i]] <- length(parents)
    }
  }
  if (length(parents)) stop("Unbalanced source SQL.")
  target <- function(position) {
    begin <- position
    while (begin > 2L && t$syntax[[begin - 1L]] == "." && t$id[[begin - 2L]]) begin <- begin - 2L
    begin > 1L && t$word[[begin - 1L]] %in% c("TABLE", "INTO") &&
      t$word[[1L]] %in% c("CREATE", "INSERT", "REPLACE", "ALTER")
  }
  if (n > 2L) {
    for (i in seq_len(n - 2L)) {
      if (t$syntax[[i]] == "." && t$id[[i + 1L]] && t$syntax[[i + 2L]] == "(" && !target(i + 1L)) {
        stop("Stored-routine source dependencies cannot be certified.")
      }
    }
  }
  ctes <- list()
  for (w in which(t$word == "WITH")) {
    j <- w + 1L
    if (j <= n && t$word[[j]] %in% c("CHECK", "LOCAL", "CASCADED", "ROLLUP")) next
    recursive <- j <= n && t$word[[j]] == "RECURSIVE"
    if (recursive) j <- j + 1L
    end <- which(seq_len(n) > w & depth < depth[[w]])
    end <- if (length(end)) end[[1L]] else n + 1L
    repeat {
      if (j > n || !t$id[[j]]) stop("Unverifiable CTE source.")
      name <- t$value[[j]]; declaration <- j; j <- j + 1L
      if (j <= n && t$syntax[[j]] == "(") j <- closing[[j]] + 1L
      if (j + 1L > n || t$word[[j]] != "AS" || t$syntax[[j + 1L]] != "(") {
        stop("Unverifiable CTE source.")
      }
      ctes[[length(ctes) + 1L]] <- list(name = name, declaration = declaration,
        start = if (recursive) j + 1L else closing[[j + 1L]], end = end)
      j <- closing[[j + 1L]] + 1L
      if (j > n || t$syntax[[j]] != ",") break
      j <- j + 1L
    }
  }
  if (n > 1L) {
    allowed <- if (view) .DSOMOP_SNAPSHOT_VIEW_CALLS else .DSOMOP_SNAPSHOT_GENERATED_CALLS
    declarations <- vapply(ctes, `[[`, integer(1L), "declaration")
    for (i in seq_len(n - 1L)) {
      if (!t$id[[i]] || t$syntax[[i + 1L]] != "(" || i %in% declarations || target(i)) next
      # LATERAL introduces a derived SELECT/WITH after a relational keyword.
      # It is syntax here, not an admitted scalar or table-valued routine.
      if (t$word[[i]] == "LATERAL" && i > 1L && i + 2L <= n &&
          (t$word[[i - 1L]] %in% c("FROM", "JOIN", "STRAIGHT_JOIN") ||
           t$syntax[[i - 1L]] == ",") && t$word[[i + 2L]] %in% c("SELECT", "WITH")) next
      # A derived-table alias can carry a column-name list.
      if ((i > 1L && t$syntax[[i - 1L]] == ")") ||
          (i > 2L && t$word[[i - 1L]] == "AS" && t$syntax[[i - 2L]] == ")")) next
      call <- toupper(t$value[[i]])
      if (!call %in% allowed ||
          (view && call == "UNIX_TIMESTAMP" && i + 2L <= n && t$syntax[[i + 2L]] == ")")) {
        stop("An unknown or stateful source function cannot be certified.")
      }
    }
  }
  contexts <- list(list(select = FALSE, from = FALSE, expect = FALSE))
  out <- list()
  i <- 1L
  while (i <= n) {
    k <- length(contexts); state <- contexts[[k]]
    v <- t$value[[i]]; w <- t$word[[i]]; op <- t$syntax[[i]]
    if (op == "(") {
      # A parenthesized table group inherits FROM. A derived query starts
      # its own scope; in particular WITH is syntax, never a physical source.
      derived <- i < n && t$word[[i + 1L]] %in% c("SELECT", "WITH")
      inherit_from <- state$expect && !derived
      contexts[[k]]$expect <- FALSE
      contexts[[k + 1L]] <- list(select = FALSE, from = inherit_from,
                                 expect = inherit_from)
    } else if (op == ")") {
      contexts <- head(contexts, -1L)
    } else if (w == "SELECT") {
      contexts[[k]] <- list(select = TRUE, from = FALSE, expect = FALSE)
    } else if ((w == "FROM" && state$select) || w %in% c("JOIN", "STRAIGHT_JOIN") ||
               (w == "TABLE" && (i == 1L || t$syntax[[i - 1L]] == "(" ||
                  t$word[[i - 1L]] %in% c("UNION", "ALL", "DISTINCT", "AS")))) {
      contexts[[k]]$from <- TRUE; contexts[[k]]$expect <- TRUE
    } else if (w %in% c("WHERE", "GROUP", "HAVING", "ORDER", "LIMIT", "UNION",
                       "EXCEPT", "INTERSECT", "WINDOW", "FOR", "LOCK")) {
      contexts[[k]]$from <- FALSE; contexts[[k]]$expect <- FALSE
    } else if (op == "," && state$from) {
      contexts[[k]]$expect <- TRUE
    } else if (state$expect && w == "LATERAL" && i + 2L <= n &&
               t$syntax[[i + 1L]] == "(" && t$word[[i + 2L]] %in% c("SELECT", "WITH")) {
      # The next parenthesis opens the derived query; scan all its sources.
    } else if (state$expect) {
      if (!t$id[[i]]) stop("Unverifiable relational source.")
      parts <- v
      while (i + 2L <= n && t$syntax[[i + 1L]] == "." && t$id[[i + 2L]]) {
        parts <- c(parts, t$value[[i + 2L]]); i <- i + 2L
      }
      table_function <- i < n && t$syntax[[i + 1L]] == "("
      json_table <- table_function && length(parts) == 1L && identical(w, "JSON_TABLE")
      if (table_function && !json_table) stop("Unverifiable table-valued source.")
      # JSON_TABLE is a native table constructor; its argument expressions and
      # any nested SELECTs are still scanned by the parenthesis walk below.
      virtual <- json_table || (length(parts) == 1L && identical(w, "DUAL"))
      local <- length(parts) == 1L && any(vapply(ctes, function(cte) {
        identical(tolower(parts[[1L]]), tolower(cte$name)) && i > cte$start && i < cte$end
      }, logical(1L)))
      if (!local && !virtual) {
        if (length(parts) > 2L) stop("Unverifiable qualified source.")
        out[[length(out) + 1L]] <- c(if (length(parts) == 1L) schema else parts[[1L]], tail(parts, 1L))
      }
      contexts[[k]]$expect <- FALSE
    }
    i <- i + 1L
  }
  if (!length(out)) return(empty)
  out <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
  names(out) <- c("schema", "table")
  unique(out)
}

.dsomopSnapshotSourceDefinition <- function(ctx, schema, table) {
  qualified <- paste(DBI::dbQuoteIdentifier(ctx$conn, schema),
                     DBI::dbQuoteIdentifier(ctx$conn, table), sep = ".")
  # SHOW reads current metadata even when MySQL INFORMATION_SCHEMA sees the
  # dictionary as it stood at START TRANSACTION.
  definition <- DBI::dbGetQuery(ctx$conn, paste("SHOW CREATE TABLE", qualified))
  if (nrow(definition) != 1L || ncol(definition) < 2L ||
      is.na(definition[[2L]][[1L]])) stop("Missing source definition.")
  ddl <- definition[[2L]][[1L]]
  if (identical(tolower(names(definition)[[2L]]), "create view")) {
    sources <- .dsomopSnapshotSqlSources(ddl, schema, view = TRUE, sql_mode = ctx$sql_mode)
    return(list(schema = schema, table = table, view = TRUE, ddl = ddl, sources = sources))
  }
  engine <- regmatches(ddl, regexec("(?m)^\\)\\s+ENGINE\\s*=\\s*([A-Za-z0-9_]+)\\b", ddl, perl = TRUE))[[1L]]
  if (length(engine) != 2L || toupper(engine[[2L]]) != "INNODB") {
    stop("Nontransactional or unverifiable source engine.")
  }
  list(schema = schema, table = table, view = FALSE, engine = toupper(engine[[2L]]))
}

.dsomopSnapshotCertifySource <- function(ctx, schema, table, path = character()) {
  key <- paste0(nchar(schema), ":", schema, nchar(table), ":", table)
  if (key %in% path) stop("Cyclic view dependencies.")
  if (exists(key, ctx$sources, inherits = FALSE)) return(invisible(TRUE))
  definition <- .dsomopSnapshotSourceDefinition(ctx, schema, table)
  if (definition$view) {
    for (i in seq_len(nrow(definition$sources))) {
      .dsomopSnapshotCertifySource(ctx, definition$sources$schema[[i]],
        definition$sources$table[[i]], c(path, key))
    }
  }
  assign(key, definition, ctx$sources)
  invisible(TRUE)
}

.dsomopSnapshotReadSql <- function(handle, sql) {
  if (!.dsomopReadSnapshotActive(handle) ||
      !identical(handle$read_snapshot$kind, "consistent_snapshot_transaction")) return(invisible(TRUE))
  ctx <- handle$read_snapshot
  if (isTRUE(ctx$unavailable)) .dsomopSnapshotUnavailable(ctx, "a source was not certifiable")
  tryCatch({
    .dsomopSnapshotGuard(handle, sql)
    .dsomopSnapshotAssertSession(ctx)
    sources <- .dsomopSnapshotSqlSources(sql, ctx$default_schema, sql_mode = ctx$sql_mode)
    for (i in seq_len(nrow(sources))) {
      schema <- sources$schema[[i]]; table <- sources$table[[i]]
      if (tolower(schema) %in% c("information_schema", "performance_schema")) next
      if (identical(schema, ctx$default_schema) && table %in% (handle$temp_tables %||% character())) next
      key <- paste0(nchar(schema), ":", schema, nchar(table), ":", table)
      if (exists(key, ctx$sources, inherits = FALSE)) next
      qualified <- paste(DBI::dbQuoteIdentifier(ctx$conn, schema),
                         DBI::dbQuoteIdentifier(ctx$conn, table), sep = ".")
      # This reads no rows, but pins the source and its transitive metadata
      # until rollback. A concurrent engine/view replacement cannot race the
      # subsequent certification or the request's source reads.
      DBI::dbGetQuery(ctx$conn, paste("SELECT 1 FROM", qualified, "LIMIT 0"))
      .dsomopSnapshotCertifySource(ctx, schema, table)
    }
  }, error = function(e) .dsomopSnapshotUnavailable(ctx, conditionMessage(e)))
  invisible(TRUE)
}

.dsomopSnapshotAssertSession <- function(ctx) {
  session <- DBI::dbGetQuery(ctx$conn, "SELECT DATABASE() AS db, @@session.sql_mode AS sql_mode")
  if (!identical(session[[1L]][[1L]], ctx$default_schema) ||
      !identical(session[[2L]][[1L]], ctx$sql_mode)) {
    stop("The source namespace or SQL interpretation changed during the request.")
  }
  invisible(TRUE)
}

.dsomopSnapshotValidateSources <- function(ctx) {
  if (isTRUE(ctx$unavailable)) .dsomopSnapshotUnavailable(ctx, "a source was not certifiable")
  tryCatch({
    .dsomopSnapshotAssertSession(ctx)
    for (key in ls(ctx$sources, all.names = TRUE)) {
      prior <- get(key, ctx$sources, inherits = FALSE)
      current <- .dsomopSnapshotSourceDefinition(ctx, prior$schema, prior$table)
      if (!identical(prior, current)) stop("Source dependencies changed during the request.")
    }
  }, error = function(e) .dsomopSnapshotUnavailable(ctx, conditionMessage(e)))
  invisible(TRUE)
}

.dsomopSnapshotValidate <- function(ctx) {
  conn <- ctx$conn
  if (isTRUE(ctx$aborted) || !isTRUE(tryCatch(.dbIsValid(conn), error = function(e) FALSE))) {
    .dsomopSnapshotLost("a statement or connection loss aborted the request snapshot")
  }
  if (identical(ctx$kind, "repeatable_read_transaction")) {
    token <- tryCatch(DBI::dbGetQuery(conn, paste(
      "SELECT CAST(transaction_timestamp() AS TEXT) AS t"))$t[[1L]],
      error = function(e) NA_character_)
    if (!identical(token, ctx$token)) {
      .dsomopSnapshotLost("the request snapshot ended inside the request")
    }
  }
  if (identical(ctx$kind, "consistent_snapshot_transaction")) {
    .dsomopSnapshotValidateSources(ctx)
    tryCatch(DBI::dbExecute(conn, "ROLLBACK TO SAVEPOINT dsomop_request_snapshot"),
             error = function(e) .dsomopSnapshotLost("the request transaction ended"))
  }
  rollback_to <- switch(ctx$kind,
    sqlserver_serializable_transaction = "ROLLBACK TRANSACTION dsomop_request_snapshot",
    oracle_serializable_transaction = "ROLLBACK TO dsomop_request_snapshot", NULL)
  if (!is.null(rollback_to)) {
    tryCatch(DBI::dbExecute(conn, rollback_to), error = function(e) {
      .dsomopSnapshotLost("the request transaction ended")
    })
  }
  invisible(TRUE)
}

.dsomopSnapshotRollback <- function(ctx) {
  DBI::dbRollback(ctx$conn)
  invisible(TRUE)
}

#' The connection pinned by the active request snapshot
#'
#' Never reconnects: a pinned connection that is no longer valid ends the
#' request with the stable snapshot error.
#'
#' @param handle CDM handle with an active snapshot.
#' @return DBI connection, or NULL when the engine has no snapshot.
#' @keywords internal
.dsomopSnapshotConn <- function(handle) {
  ctx <- handle$read_snapshot
  conn <- ctx$conn
  if (is.null(conn)) return(NULL)
  if (!isTRUE(tryCatch(.dbIsValid(conn), error = function(e) FALSE))) {
    ctx$aborted <- TRUE
    .dsomopSnapshotLost("the pinned connection of the request was lost")
  }
  conn
}

#' Statements a request snapshot must never run
#'
#' Transaction control on every engine; on MySQL and MariaDB also every
#' statement that commits implicitly (DDL other than on TEMPORARY tables,
#' locking statements, account management).
#'
#' @param handle CDM handle with an active snapshot.
#' @param sql One SQL statement.
#' @return Invisible TRUE, or a stable error.
#' @keywords internal
.dsomopSnapshotGuard <- function(handle, sql) {
  if (!.dsomopReadSnapshotActive(handle)) return(invisible(TRUE))
  text <- toupper(trimws(gsub("--[^\n]*|/\\*.*?\\*/", " ", paste(sql,
                                                                collapse = " "),
                              perl = TRUE)))
  control <- grepl(paste0("^(BEGIN|START\\s+TRANSACTION|COMMIT|ROLLBACK|END",
                          "|SET\\s+AUTOCOMMIT|SET\\s+TRANSACTION|SAVEPOINT",
                          "|RELEASE\\s+SAVEPOINT)\\b"), text, perl = TRUE)
  implicit <- identical(handle$read_snapshot$kind,
                        "consistent_snapshot_transaction") &&
    grepl("^(CREATE|DROP|ALTER|RENAME|TRUNCATE|LOCK|UNLOCK|GRANT|REVOKE|ANALYZE|OPTIMIZE|REPAIR|FLUSH|INSTALL|UNINSTALL|LOAD)\\b",
          text, perl = TRUE) &&
    !grepl("^(CREATE|DROP)\\s+TEMPORARY\\s+TABLE\\b", text, perl = TRUE)
  implicit <- implicit ||
    (identical(handle$read_snapshot$kind, "oracle_serializable_transaction") &&
       grepl("^(CREATE|DROP|ALTER|TRUNCATE|GRANT|REVOKE|COMMENT|ANALYZE)\\b", text, perl = TRUE)) ||
    (identical(handle$read_snapshot$kind, "redshift_snapshot_transaction") &&
       grepl("^(TRUNCATE|VACUUM)\\b", text, perl = TRUE))
  syntax_change <- identical(handle$read_snapshot$kind, "consistent_snapshot_transaction") &&
    grepl("^(USE|SET(?:\\s+(?:SESSION|LOCAL))?\\s+(?:SQL_MODE|NAMES|CHARACTER\\s+SET))\\b", text, perl = TRUE)
  if (control || implicit || syntax_change) {
    handle$read_snapshot$aborted <- TRUE
    .dsomopSnapshotLost("a statement that would end the request snapshot was refused")
  }
  invisible(TRUE)
}

#' Run one statement of a request inside its snapshot
#'
#' On PostgreSQL the statement runs under its own SAVEPOINT, so a failing
#' (for example deliberately probing) statement rolls back to it and the
#' snapshot survives; on DuckDB a failure aborts the transaction, which is
#' recorded so that the request fails instead of reading from a new snapshot.
#'
#' @param handle CDM handle with an active snapshot.
#' @param fn Function of the pinned connection.
#' @return The value of \code{fn}.
#' @keywords internal
.dsomopSnapshotRun <- function(handle, fn) {
  ctx <- handle$read_snapshot
  conn <- .dsomopSnapshotConn(handle)
  if (is.null(conn)) return(fn(.conn(handle)))
  if (isTRUE(ctx$aborted)) {
    .dsomopSnapshotLost("a statement aborted the request snapshot")
  }
  failed <- function(e) {
    if (.isConnectionError(e)) {
      ctx$aborted <- TRUE
      .dsomopSnapshotLost("the connection failed inside the request snapshot")
    }
    stop(e)
  }
  if (identical(ctx$kind, "repeatable_read_transaction")) {
    ctx$savepoints <- ctx$savepoints + 1L
    name <- paste0("dsomop_rs_", ctx$savepoints)
    DBI::dbExecute(conn, paste0("SAVEPOINT ", name))
    return(tryCatch({
      value <- fn(conn)
      DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", name))
      value
    }, error = function(e) {
      restored <- tryCatch({
        DBI::dbExecute(conn, paste0("ROLLBACK TO SAVEPOINT ", name))
        DBI::dbExecute(conn, paste0("RELEASE SAVEPOINT ", name))
        TRUE
      }, error = function(e2) FALSE)
      if (!restored) ctx$aborted <- TRUE
      failed(e)
    }))
  }
  tryCatch(fn(conn), error = function(e) {
    if (ctx$kind %in% c("snapshot_transaction", "redshift_snapshot_transaction")) {
      # DuckDB aborts its transaction on a failing statement: record it, so
      # the request fails instead of continuing on another snapshot.
      alive <- tryCatch({
        DBI::dbGetQuery(conn, "SELECT 1 AS ok")
        TRUE
      }, error = function(e2) FALSE)
      if (!alive) ctx$aborted <- TRUE
    }
    failed(e)
  })
}

#' Request-local memo inside the active snapshot
#'
#' Computes a value once per request (for example the person-membership
#' generation shared by every builder of one request) and forgets it when the
#' request ends. Outside a snapshot the value is computed every time.
#'
#' @param handle CDM handle.
#' @param key Memo key.
#' @param compute Function computing the value.
#' @return The value.
#' @keywords internal
.dsomopSnapshotMemo <- function(handle, key, compute) {
  if (!.dsomopReadSnapshotActive(handle)) return(compute())
  memo <- handle$read_snapshot$memo
  if (!exists(key, envir = memo, inherits = FALSE)) {
    value <- compute()
    assign(key, value, envir = memo)
  }
  get(key, envir = memo, inherits = FALSE)
}

# Column type of an R vector for a temporary table on MySQL or MariaDB.
.dsomopSnapshotSqlType <- function(x) {
  if (inherits(x, "integer64")) return("BIGINT")
  if (inherits(x, "Date")) return("DATE")
  if (inherits(x, "POSIXt")) return("DATETIME(6)")
  if (is.logical(x)) return("BOOLEAN")
  if (is.integer(x)) return("BIGINT")
  if (is.numeric(x)) return("DOUBLE")
  if (is.raw(x) || is.list(x)) return("LONGBLOB")
  "LONGTEXT CHARACTER SET utf8mb4"
}

#' Materialize a temporary table from a consistent read (MySQL, MariaDB)
#'
#' InnoDB reads the SELECT of \code{CREATE TEMPORARY TABLE ... SELECT} with
#' locking reads of the latest committed rows, so inside a request snapshot
#' the rows are read with an ordinary consistent SELECT and appended to a
#' temporary table of the matching column types.
#'
#' @param handle CDM handle with an active consistent-snapshot transaction.
#' @param name Validated temporary table name.
#' @param select_sql The SELECT to materialize.
#' @return Invisible NULL.
#' @keywords internal
.dsomopSnapshotMaterializeTemp <- function(handle, name, select_sql) {
  .dsomopSnapshotReadSql(handle, select_sql)
  rows <- .dsomopSnapshotRun(handle, function(conn) {
    DBI::dbGetQuery(conn, select_sql)
  })
  names(rows) <- tolower(names(rows))
  if (!ncol(rows)) {
    stop("A temporary table needs at least one column.", call. = FALSE)
  }
  conn <- .dsomopSnapshotConn(handle)
  columns <- paste(vapply(names(rows), function(col) {
    paste(DBI::dbQuoteIdentifier(conn, col), .dsomopSnapshotSqlType(rows[[col]]))
  }, character(1L)), collapse = ", ")
  create <- paste0("CREATE TEMPORARY TABLE ", name, " (", columns, ")")
  .dsomopSnapshotGuard(handle, create)
  .dsomopSnapshotRun(handle, function(conn) DBI::dbExecute(conn, create))
  # CREATE TEMPORARY TABLE survives rollback. Own it before appending so a
  # failed transfer remains visible to compiler/operation cleanup and retries.
  handle$temp_connection <- conn
  handle$temp_tables <- unique(c(handle$temp_tables, name))
  if (nrow(rows)) {
    .dsomopSnapshotRun(handle, function(conn) {
      DBI::dbAppendTable(conn, name, rows)
    })
  }
  invisible(NULL)
}
