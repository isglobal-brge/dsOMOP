# Module: SQL Rendering
# SQL rendering utilities for database query execution.

# --- DBMS Dialect Mapping ---

#' Resolve a DBMS name to a target dialect
#'
#' @param dbms Character; DBMS name from resource driver
#' @return Character; target dialect string
#' @keywords internal
.resolve_target_dialect <- function(dbms) {
  dbms <- tolower(dbms)
  mapping <- list(
    postgresql = "postgresql",
    postgres   = "postgresql",
    sql_server = "sql server",
    sqlserver  = "sql server",
    oracle     = "oracle",
    redshift   = "redshift",
    bigquery   = "bigquery",
    snowflake  = "snowflake",
    spark      = "spark",
    databricks = "spark",
    sqlite     = "sqlite",
    duckdb     = "sqlite",
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
  for (stmt in statements) {
    DBI::dbExecute(handle$conn, stmt)
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
  .coerce_integer64(DBI::dbGetQuery(handle$conn, translated))
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
