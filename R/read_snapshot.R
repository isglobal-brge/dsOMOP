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
#   * MySQL and MariaDB: REPEATABLE READ with START TRANSACTION WITH CONSISTENT
#     SNAPSHOT. Visible source base tables must use InnoDB; unsupported engines
#     fail closed. InnoDB evaluates the SELECT of CREATE TABLE ... SELECT as a
#     locking read of the latest committed rows, not of the snapshot, so inside
#     a snapshot a temporary table is filled from a consistent read through R.
#     Statements that would commit implicitly are refused.
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
  }, add = TRUE)
  value <- expr
  if (!is.null(ctx$conn)) .dsomopSnapshotValidate(ctx)
  if (ctx$transaction) {
    tryCatch(.dsomopSnapshotRollback(ctx), error = function(e) {
      .dsomopSnapshotLost("the request snapshot could not be rolled back")
    })
    ctx$transaction <- FALSE
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
        schemas <- ctx$schemas
        schemas <- schemas[!is.na(schemas) & nzchar(schemas)]
        if (length(schemas)) {
          schema_sql <- paste(DBI::dbQuoteString(conn, schemas), collapse = ", ")
          nontransactional <- DBI::dbGetQuery(conn, paste0(
            "SELECT COUNT(*) AS n FROM information_schema.tables WHERE table_schema IN (",
            schema_sql, ") AND table_type = 'BASE TABLE' AND UPPER(engine) <> 'INNODB'"))$n[[1L]]
          if (as.numeric(nontransactional) > 0) {
            stop(structure(list(message = paste0(
              "This source contains nontransactional tables and cannot provide ",
              "a request-consistent read snapshot; no result was released."), call = NULL),
              class = c("dsomop_snapshot_unavailable", "error", "condition")))
          }
        }
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
  if (control || implicit) {
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
  "TEXT"
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
  if (nrow(rows)) {
    .dsomopSnapshotRun(handle, function(conn) {
      DBI::dbAppendTable(conn, name, rows)
    })
  }
  invisible(NULL)
}
