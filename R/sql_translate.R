# ==============================================================================
# dsOMOP v2 - Pure R SQL Rendering Engine
# ==============================================================================
# Replaces SqlRender dependency. Implements the subset of SqlRender features
# actually used by dsOMOP: @param substitution, TOP/LIMIT translation,
# DATEADD translation, and statement splitting.
# ==============================================================================

# --- Parameter Substitution ---

#' Substitute @param placeholders in SQL
#'
#' Replaces \code{@paramName} tokens with their values. Parameters are sorted
#' longest-first to avoid substring collisions (e.g., \code{@schema} before
#' \code{@s}).
#'
#' @param sql Character; SQL template with \code{@param} placeholders
#' @param ... Named parameter values
#' @return Character; SQL with parameters substituted
#' @keywords internal
.sql_render <- function(sql, ...) {
  params <- list(...)
  if (length(params) == 0L) return(sql)

  keys <- names(params)
  keys <- keys[order(nchar(keys), decreasing = TRUE)]
  for (key in keys) {
    val <- as.character(params[[key]])
    sql <- gsub(paste0("@", key), val, sql, fixed = TRUE)
  }
  sql
}

# --- Dialect Translation ---

#' Translate OHDSI SQL (SQL Server dialect) to target dialect
#'
#' Handles two transformations:
#' \enumerate{
#'   \item \code{SELECT TOP n ...} to \code{SELECT ... LIMIT n} (non-SQL Server)
#'   \item \code{DATEADD(day, n, expr)} to dialect-specific date arithmetic
#' }
#'
#' @param sql Character; SQL in OHDSI/SQL Server convention
#' @param target_dialect Character; target dialect name
#' @return Character; translated SQL
#' @keywords internal
.sql_translate <- function(sql, target_dialect) {
  if (is.null(target_dialect) || nchar(target_dialect) == 0L) return(sql)

  sql <- .translate_dateadd(sql, target_dialect)
  sql <- .translate_top(sql, target_dialect)
  sql
}

# --- DATEADD Translation ---

#' Translate DATEADD(day, n, expr) to dialect-specific form
#'
#' @param sql Character; SQL containing DATEADD expressions
#' @param dialect Character; target dialect
#' @return Character; SQL with DATEADD translated
#' @keywords internal
.translate_dateadd <- function(sql, dialect) {
  if (!grepl("DATEADD", sql, ignore.case = TRUE)) return(sql)

  # Dialects with native DATEADD: no translation needed
  if (dialect %in% c("sql server", "redshift", "snowflake")) return(sql)

  # Match DATEADD(day, <number>, <expression>)
  # The expression can contain nested parentheses, identifiers, etc.
  # We use a careful approach: match DATEADD(day, then capture the number,
  # then capture everything up to the matching closing paren.
  pattern <- "DATEADD\\(\\s*day\\s*,\\s*(-?\\d+)\\s*,\\s*"

  repeat {
    m <- regexpr(pattern, sql, ignore.case = TRUE, perl = TRUE)
    if (m == -1L) break

    # Find the start of the expression (after the matched prefix)
    prefix_end <- m + attr(m, "match.length")

    # Extract the number
    num_match <- regmatches(sql, regexec(pattern, sql, ignore.case = TRUE, perl = TRUE))[[1]]
    n <- num_match[2]

    # Find the matching closing parenthesis
    expr_start <- prefix_end
    depth <- 1L
    pos <- expr_start
    while (pos <= nchar(sql) && depth > 0L) {
      ch <- substr(sql, pos, pos)
      if (ch == "(") depth <- depth + 1L
      else if (ch == ")") depth <- depth - 1L
      pos <- pos + 1L
    }

    if (depth != 0L) break  # unmatched parens, bail out

    # expr is from expr_start to pos-2 (before the closing paren)
    expr <- substr(sql, expr_start, pos - 2L)

    # Build replacement based on dialect
    replacement <- switch(dialect,
      "postgresql" = paste0("(", expr, " + ", n, " * INTERVAL '1 day')"),
      "sqlite"     = paste0("DATE(", expr, ", '", n, " days')"),
      "oracle"     = paste0("(", expr, " + ", n, ")"),
      "bigquery"   = paste0("DATE_ADD(", expr, ", INTERVAL ", n, " DAY)"),
      "spark"      = paste0("DATE_ADD(", expr, ", ", n, ")")
    )

    # Replace the matched DATEADD(...) with the translation
    sql <- paste0(
      substr(sql, 1L, m - 1L),
      replacement,
      substr(sql, pos, nchar(sql))
    )
  }

  sql
}

# --- TOP/LIMIT Translation ---

#' Translate SELECT TOP n to SELECT ... LIMIT n
#'
#' For non-SQL Server dialects, converts \code{SELECT TOP n columns FROM ...}
#' to \code{SELECT columns FROM ... LIMIT n}. Only transforms when a FROM
#' clause is present.
#'
#' @param sql Character; SQL containing SELECT TOP patterns
#' @param dialect Character; target dialect
#' @return Character; SQL with TOP translated to LIMIT
#' @keywords internal
.translate_top <- function(sql, dialect) {
  # SQL Server has native TOP support
  if (dialect == "sql server") return(sql)

  if (!grepl("\\bTOP\\b", sql, ignore.case = TRUE)) return(sql)

  # Pattern: SELECT TOP <n> ... FROM ...
  # Capture the number and the rest of the SELECT list + FROM onwards
  pattern <- "(?i)\\bSELECT\\s+TOP\\s+(\\d+)\\b"

  while (grepl(pattern, sql, perl = TRUE)) {
    m <- regexec(pattern, sql, perl = TRUE)[[1]]
    n <- regmatches(sql, list(m))[[1]][2]

    match_start <- m[1]
    match_end <- match_start + attr(m, "match.length")[1] - 1L

    # Replace "SELECT TOP n" with "SELECT"
    before <- substr(sql, 1L, match_start - 1L)
    after <- substr(sql, match_end + 1L, nchar(sql))

    # Find where to insert LIMIT: before any trailing semicolon, after the
    # statement body. We look for the end of this statement.
    # Handle nested subqueries by tracking parenthesis depth.
    insert_pos <- nchar(after)
    depth <- 0L
    for (i in seq_len(nchar(after))) {
      ch <- substr(after, i, i)
      if (ch == "(") depth <- depth + 1L
      else if (ch == ")") depth <- depth - 1L
      else if (ch == ";" && depth == 0L) {
        insert_pos <- i - 1L
        break
      }
    }

    stmt_body <- trimws(substr(after, 1L, insert_pos))
    rest <- substr(after, insert_pos + 1L, nchar(after))

    sql <- paste0(before, "SELECT ", stmt_body, " LIMIT ", n, rest)
  }

  sql
}

# --- Statement Splitting ---

#' Split SQL into individual statements on semicolons
#'
#' Splits on semicolons that are not inside single-quoted strings.
#' Trims whitespace and drops empty statements.
#'
#' @param sql Character; one or more SQL statements separated by semicolons
#' @return Character vector of individual SQL statements
#' @keywords internal
.sql_split <- function(sql) {
  # Walk through the string, split on ; outside single quotes
  chars <- strsplit(sql, "")[[1]]
  stmts <- character(0)
  current <- character(0)
  in_quote <- FALSE

  for (ch in chars) {
    if (ch == "'" && !in_quote) {
      in_quote <- TRUE
      current <- c(current, ch)
    } else if (ch == "'" && in_quote) {
      in_quote <- FALSE
      current <- c(current, ch)
    } else if (ch == ";" && !in_quote) {
      stmt <- trimws(paste(current, collapse = ""))
      if (nchar(stmt) > 0L) stmts <- c(stmts, stmt)
      current <- character(0)
    } else {
      current <- c(current, ch)
    }
  }

  # Last segment (no trailing semicolon)
  stmt <- trimws(paste(current, collapse = ""))
  if (nchar(stmt) > 0L) stmts <- c(stmts, stmt)

  stmts
}
