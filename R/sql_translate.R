# Module: SQL Translation
# DBMS-specific SQL translation and schema qualification utilities.

# --- Parameter Substitution ---

#' Substitute @param placeholders in SQL
#'
#' Replaces \code{@paramName} tokens with their values. Parameters are sorted
#' longest-first to avoid substring collisions (e.g., \code{@schema} before
#' \code{@s}).
#'
#' This is literal template substitution, not DBI parameter binding, identifier
#' validation, or SQL sanitization. Callers must pass only controller-owned
#' templates and values already validated or quoted by the typed query layer.
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

#' Build a portable floored integer quotient
#'
#' SQL engines disagree on integer division and on whether casting a fractional
#' value to INTEGER truncates or rounds.  Force decimal division, apply FLOOR,
#' and only then cast to INTEGER so age/day calculations have identical
#' semantics across dialects.
#'
#' @param expr Character SQL numeric expression.
#' @param divisor Positive integer divisor.
#' @return Character SQL expression.
#' @keywords internal
.omopFloorDivideSql <- function(expr, divisor) {
  divisor <- suppressWarnings(as.integer(divisor))
  if (length(divisor) != 1L || is.na(divisor) || divisor <= 0L) {
    stop("divisor must be one positive integer.", call. = FALSE)
  }
  paste0("CAST(FLOOR((", expr, ") / ", divisor,
         ".0) AS INTEGER)")
}

#' Build a portable fixed-width floor bin
#'
#' @param expr Character SQL numeric expression.
#' @param width Positive integer bin width.
#' @return Character SQL expression yielding the lower integer bin boundary.
#' @keywords internal
.omopFloorBinSql <- function(expr, width) {
  width <- suppressWarnings(as.integer(width))
  if (length(width) != 1L || is.na(width) || width <= 0L) {
    stop("width must be one positive integer.", call. = FALSE)
  }
  paste0("CAST(FLOOR((", expr, ") / ", width, ".0) * ", width,
         " AS INTEGER)")
}

# Dialect-aware scalar casts and concatenation used by controller-owned SQL.
# Keeping these here prevents individual OHDSI adapters from relying on session
# modes such as MySQL PIPES_AS_CONCAT or on non-portable CAST aliases.
.omopIntegerCastSql <- function(handle, expression) {
  dialect <- tolower(handle$target_dialect %||% "")
  type <- switch(dialect,
    mysql = "SIGNED",
    bigquery = "INT64",
    oracle = "NUMBER(19)",
    "INTEGER"
  )
  paste0("CAST(", expression, " AS ", type, ")")
}

.omopBigIntegerCastSql <- function(handle, expression) {
  dialect <- tolower(handle$target_dialect %||% "")
  type <- switch(dialect,
    mysql = "SIGNED",
    bigquery = "INT64",
    oracle = "NUMBER(19)",
    "BIGINT"
  )
  paste0("CAST(", expression, " AS ", type, ")")
}

.omopTextCastSql <- function(handle, expression, width = 64L) {
  width <- suppressWarnings(as.integer(width))
  if (length(width) != 1L || is.na(width) || width < 1L || width > 4000L) {
    stop("text cast width must be between 1 and 4000.", call. = FALSE)
  }
  dialect <- tolower(handle$target_dialect %||% "")
  type <- switch(dialect,
    mysql = paste0("CHAR(", width, ")"),
    bigquery = "STRING",
    spark = "STRING",
    oracle = paste0("VARCHAR2(", width, ")"),
    paste0("VARCHAR(", width, ")")
  )
  paste0("CAST(", expression, " AS ", type, ")")
}

.omopConcatSql <- function(handle, ...) {
  parts <- as.character(unlist(list(...), use.names = FALSE))
  if (length(parts) < 1L || anyNA(parts) || any(!nzchar(parts))) {
    stop("SQL concatenation requires non-empty expressions.", call. = FALSE)
  }
  dialect <- tolower(handle$target_dialect %||% "")
  if (dialect %in% c("mysql", "bigquery", "spark")) {
    paste0("CONCAT(", paste(parts, collapse = ", "), ")")
  } else if (identical(dialect, "sql server")) {
    paste(parts, collapse = " + ")
  } else {
    paste(parts, collapse = " || ")
  }
}

.omopYearStartDateSql <- function(handle, year_expression) {
  joined <- .omopConcatSql(
    handle,
    .omopTextCastSql(handle, year_expression, 4L),
    "'-01-01'"
  )
  if (identical(tolower(handle$target_dialect %||% ""), "sqlite")) {
    paste0("(", joined, ")")
  } else {
    paste0("CAST(", joined, " AS DATE)")
  }
}

.omopMonthKeySql <- function(handle, date_expression) {
  dialect <- tolower(handle$target_dialect %||% "")
  if (identical(dialect, "sqlite")) {
    paste0("CAST(strftime('%Y%m', ", date_expression, ") AS INTEGER)")
  } else if (dialect %in% c("mysql", "sql server", "spark")) {
    paste0("YEAR(", date_expression, ") * 100 + MONTH(",
           date_expression, ")")
  } else {
    paste0("EXTRACT(YEAR FROM ", date_expression, ") * 100 + ",
           "EXTRACT(MONTH FROM ", date_expression, ")")
  }
}

# --- Dialect Translation ---

#' Translate OHDSI SQL (SQL Server dialect) to target dialect
#'
#' Handles three transformations:
#' \enumerate{
#'   \item \code{SELECT TOP n ...} to \code{SELECT ... LIMIT n} (non-SQL Server)
#'   \item \code{DATEDIFF(day, start, end)} to dialect-specific date arithmetic
#'   \item \code{DATEADD(day, n, expr)} to dialect-specific date arithmetic
#' }
#'
#' This deliberately small built-in translator is not OHDSI SqlRender and does
#' not claim its full grammar. PostgreSQL and the MySQL-family paths have live
#' vendor smoke tests; other network dialect outputs remain contract-tested.
#'
#' @param sql Character; SQL in OHDSI/SQL Server convention
#' @param target_dialect Character; target dialect name
#' @return Character; translated SQL
#' @keywords internal
.sql_translate <- function(sql, target_dialect) {
  if (is.null(target_dialect)) return(sql)
  if (!is.character(target_dialect) || length(target_dialect) != 1L ||
      is.na(target_dialect)) {
    stop("target_dialect must be one character value.", call. = FALSE)
  }
  target_dialect <- tolower(trimws(target_dialect))
  if (!nzchar(target_dialect)) return(sql)
  supported <- c("postgresql", "sql server", "oracle", "redshift",
                 "bigquery", "snowflake", "spark", "sqlite", "duckdb",
                 "mysql")
  if (!target_dialect %in% supported) {
    stop("Unsupported target SQL dialect: '", target_dialect, "'.",
         call. = FALSE)
  }

  protected <- .protectSqlTranslationSegments(sql, target_dialect)
  sql <- protected$sql
  sql <- .translate_datediff(sql, target_dialect)
  sql <- .translate_dateadd(sql, target_dialect)
  sql <- .translate_top(
    sql, target_dialect, line_comment_tokens = protected$line_comment_tokens
  )
  sql <- .translate_cast_types(sql, target_dialect)
  sql <- .translate_statistical_functions(sql, target_dialect)
  if (identical(target_dialect, "oracle")) {
    sql <- .translate_oracle_aliases(sql)
  }
  .restoreSqlTranslationSegments(sql, protected)
}

# Protect values, quoted identifiers and comments before applying the small
# built-in SQL translator. Query parameters have already been rendered at this
# point, so scanning the whole string would otherwise turn public text such as
# "AS INTEGER" or "SELECT TOP 1" into executable syntax changes.
.protectSqlTranslationSegments <- function(sql, dialect = "") {
  if (!is.character(sql) || length(sql) != 1L || is.na(sql)) {
    stop("sql must be one non-missing character value.", call. = FALSE)
  }
  dialect <- tolower(dialect)
  prefix <- "DSOMOPPROTECTEDSQLSEGMENT"
  while (grepl(prefix, sql, fixed = TRUE)) prefix <- paste0(prefix, "X")

  n <- nchar(sql)
  i <- 1L
  cursor <- 1L
  pieces <- character(0)
  tokens <- character(0)
  values <- character(0)
  line_comment_tokens <- character(0)
  protect <- function(start, end, line_comment = FALSE) {
    if (start > cursor) {
      pieces <<- c(pieces, substr(sql, cursor, start - 1L))
    }
    token <- paste0(prefix, sprintf("%08d", length(values) + 1L), "X")
    pieces <<- c(pieces, token)
    tokens <<- c(tokens, token)
    values <<- c(values, substr(sql, start, end))
    if (isTRUE(line_comment)) {
      line_comment_tokens <<- c(line_comment_tokens, token)
    }
    cursor <<- end + 1L
  }

  while (i <= n) {
    ch <- substr(sql, i, i)
    nxt <- if (i < n) substr(sql, i + 1L, i + 1L) else ""
    prev <- if (i > 1L) substr(sql, i - 1L, i - 1L) else ""
    end <- NA_integer_
    line_comment <- FALSE

    pg_escape_string <- identical(dialect, "postgresql") &&
      ch %in% c("E", "e") && identical(nxt, "'") &&
      (i == 1L || !grepl("[A-Za-z0-9_$]", prev))
    if (pg_escape_string || ch %in% c("'", "\"", "`")) {
      closing <- if (pg_escape_string) "'" else ch
      j <- if (pg_escape_string) i + 2L else i + 1L
      backslash_escapes <- if (identical(dialect, "postgresql")) {
        pg_escape_string
      } else {
        # MySQL-family string literals use backslash escapes unless the server
        # explicitly enables NO_BACKSLASH_ESCAPES. Retain that compatible
        # interpretation, and preserve the translator's legacy behaviour for
        # other dialects.
        TRUE
      }
      while (j <= n) {
        current <- substr(sql, j, j)
        following <- if (j < n) substr(sql, j + 1L, j + 1L) else ""
        if (backslash_escapes && current == "\\" && j < n) {
          j <- j + 2L
          next
        }
        if (current == closing && following == closing) {
          j <- j + 2L
          next
        }
        if (current == closing) {
          end <- j
          break
        }
        j <- j + 1L
      }
      if (is.na(end)) {
        stop("SQL contains an unterminated quoted segment.", call. = FALSE)
      }
    } else if (ch == "[") {
      j <- i + 1L
      while (j <= n) {
        current <- substr(sql, j, j)
        following <- if (j < n) substr(sql, j + 1L, j + 1L) else ""
        if (current == "]" && following == "]") {
          j <- j + 2L
          next
        }
        if (current == "]") {
          end <- j
          break
        }
        j <- j + 1L
      }
      if (is.na(end)) {
        stop("SQL contains an unterminated quoted identifier.", call. = FALSE)
      }
    } else if ((ch == "-" && nxt == "-") ||
               # Input uses OHDSI's SQL Server convention, where #name is a
               # temporary-table identifier. Only the unambiguous MySQL
               # "# comment" form is treated as a line comment here.
               (identical(dialect, "mysql") && ch == "#" &&
                (nxt == "" || grepl("[[:space:]]", nxt)))) {
      newline <- regexpr("[\\r\\n]", substr(sql, i, n), perl = TRUE)[[1L]]
      end <- if (newline == -1L) n else i + newline - 2L
      line_comment <- TRUE
    } else if (ch == "/" && nxt == "*") {
      depth <- 1L
      j <- i + 2L
      while (j <= n && depth > 0L) {
        current <- substr(sql, j, j)
        following <- if (j < n) substr(sql, j + 1L, j + 1L) else ""
        if (current == "/" && following == "*") {
          depth <- depth + 1L
          j <- j + 2L
        } else if (current == "*" && following == "/") {
          depth <- depth - 1L
          j <- j + 2L
        } else {
          j <- j + 1L
        }
      }
      if (depth != 0L) {
        stop("SQL contains an unterminated block comment.", call. = FALSE)
      }
      end <- j - 1L
    } else if (ch == "$") {
      remainder <- substr(sql, i, n)
      delimiter_match <- regexpr(
        "^\\$(?:[A-Za-z_][A-Za-z0-9_]*)?\\$", remainder, perl = TRUE
      )
      if (delimiter_match[[1L]] == 1L) {
        delimiter <- substr(
          remainder, 1L, attr(delimiter_match, "match.length")[[1L]]
        )
        content_start <- i + nchar(delimiter)
        closing <- regexpr(
          delimiter, substr(sql, content_start, n), fixed = TRUE
        )[[1L]]
        if (closing == -1L) {
          stop("SQL contains an unterminated dollar-quoted segment.",
               call. = FALSE)
        }
        end <- content_start + closing + nchar(delimiter) - 2L
      }
    }

    if (!is.na(end)) {
      protect(i, end, line_comment = line_comment)
      i <- end + 1L
    } else {
      i <- i + 1L
    }
  }

  if (cursor <= n) pieces <- c(pieces, substr(sql, cursor, n))
  list(
    sql = paste0(pieces, collapse = ""),
    tokens = tokens,
    values = values,
    line_comment_tokens = line_comment_tokens
  )
}

.restoreSqlTranslationSegments <- function(sql, protected) {
  if (length(protected$tokens) == 0L) return(sql)
  for (i in seq_along(protected$tokens)) {
    sql <- gsub(protected$tokens[[i]], protected$values[[i]], sql, fixed = TRUE)
  }
  sql
}

# Normalize only CAST target tokens in controller-owned SQL. This deliberately
# does not attempt general SQL parsing; every replacement is a type spelling
# with equivalent numeric/text semantics on the target engine.
.translate_cast_types <- function(sql, dialect) {
  dialect <- tolower(dialect)
  if (identical(dialect, "mysql")) {
    sql <- gsub("\\bAS\\s+VARCHAR\\b", "AS CHAR", sql,
                ignore.case = TRUE, perl = TRUE)
    sql <- gsub("\\bAS\\s+TEXT\\b", "AS CHAR", sql,
                ignore.case = TRUE, perl = TRUE)
    sql <- gsub("\\bAS\\s+(?:INTEGER|BIGINT)\\b", "AS SIGNED", sql,
                ignore.case = TRUE, perl = TRUE)
    sql <- gsub("\\bAS\\s+FLOAT\\b", "AS DECIMAL(38,10)", sql,
                ignore.case = TRUE, perl = TRUE)
  } else if (identical(dialect, "bigquery")) {
    sql <- gsub("\\bAS\\s+VARCHAR(?:\\s*\\([0-9]+\\))?", "AS STRING", sql,
                ignore.case = TRUE, perl = TRUE)
    sql <- gsub("\\bAS\\s+TEXT\\b", "AS STRING", sql,
                ignore.case = TRUE, perl = TRUE)
    sql <- gsub("\\bAS\\s+(?:INTEGER|BIGINT)\\b", "AS INT64", sql,
                ignore.case = TRUE, perl = TRUE)
    sql <- gsub("\\bAS\\s+FLOAT\\b", "AS FLOAT64", sql,
                ignore.case = TRUE, perl = TRUE)
  } else if (identical(dialect, "spark")) {
    sql <- gsub("\\bAS\\s+VARCHAR(?:\\s*\\([0-9]+\\))?", "AS STRING", sql,
                ignore.case = TRUE, perl = TRUE)
    sql <- gsub("\\bAS\\s+TEXT\\b", "AS STRING", sql,
                ignore.case = TRUE, perl = TRUE)
  } else if (identical(dialect, "oracle")) {
    sql <- gsub("\\bAS\\s+BIGINT\\b", "AS NUMBER(19)", sql,
                ignore.case = TRUE, perl = TRUE)
    sql <- gsub("\\bAS\\s+VARCHAR\\s*\\(", "AS VARCHAR2(", sql,
                ignore.case = TRUE, perl = TRUE)
    sql <- gsub("\\bAS\\s+VARCHAR\\b(?!\\s*\\()", "AS VARCHAR2(4000)", sql,
                ignore.case = TRUE, perl = TRUE)
  }
  sql
}

# PostgreSQL defines bare STDDEV as the sample standard deviation, whereas
# MySQL and MariaDB define it as the population standard deviation. Use the
# explicit sample form on the MySQL-family path so federated results have the
# same statistic.
.translate_statistical_functions <- function(sql, dialect) {
  if (identical(tolower(dialect), "mysql")) {
    sql <- gsub("\\bSTDDEV\\s*\\(", "STDDEV_SAMP(", sql,
                ignore.case = TRUE, perl = TRUE)
  }
  sql
}

#' Remove Oracle-incompatible AS keywords from generated aliases
#'
#' Oracle accepts \code{AS} for neither table nor subquery aliases, while the
#' controller-owned SQL consistently emits bare aliases. Removing \code{AS}
#' from ordinary aliases is also valid for selected-column aliases. The
#' negative look-ahead deliberately preserves CTE/view/table \code{AS SELECT}
#' clauses and SQL type casts.
#'
#' @param sql Controller-owned SQL.
#' @return Oracle-compatible SQL.
#' @keywords internal
.translate_oracle_aliases <- function(sql) {
  preserved <- paste(c(
    "SELECT", "WITH", "MATERIALIZED", "OF",
    "CHAR", "VARCHAR", "VARCHAR2", "NCHAR", "NVARCHAR", "NVARCHAR2",
    "NUMBER", "NUMERIC", "DECIMAL", "INTEGER", "INT", "SMALLINT",
    "BIGINT", "FLOAT", "REAL", "DOUBLE", "DATE", "TIMESTAMP",
    "INTERVAL", "RAW", "CLOB", "NCLOB", "BLOB", "BOOLEAN"
  ), collapse = "|")
  pattern <- paste0(
    "\\bAS\\s+(?!(?:", preserved, ")\\b)",
    "([A-Za-z_][A-Za-z0-9_]*)"
  )
  translate <- function(value) {
    gsub(pattern, "\\1", value, ignore.case = TRUE, perl = TRUE)
  }

  # Filter values have already been quoted when translation runs. Never alter
  # their bytes: a public value such as "AS treatment" is data, not syntax.
  literals <- gregexpr("'(?:''|[^'])*'", sql, perl = TRUE)[[1L]]
  if (identical(literals[[1L]], -1L)) return(translate(sql))
  lengths <- attr(literals, "match.length")
  pieces <- character(0)
  cursor <- 1L
  for (i in seq_along(literals)) {
    start <- literals[[i]]
    if (start > cursor) {
      pieces <- c(pieces, translate(substr(sql, cursor, start - 1L)))
    }
    end <- start + lengths[[i]] - 1L
    pieces <- c(pieces, substr(sql, start, end))
    cursor <- end + 1L
  }
  if (cursor <= nchar(sql)) {
    pieces <- c(pieces, translate(substr(sql, cursor, nchar(sql))))
  }
  paste0(pieces, collapse = "")
}

# --- DATEDIFF Translation ---

#' Translate DATEDIFF(day, start, end) to dialect-specific form
#'
#' The built-in contract intentionally accepts only column identifiers (with
#' optional qualification) as the start and end expressions. This covers the
#' curated QueryLibrary duration templates without claiming a general SQL
#' parser. Quoted identifiers are represented by protected identifier tokens
#' before this function runs and therefore follow the same path.
#'
#' @param sql Character; SQL containing DATEDIFF expressions.
#' @param dialect Character; target dialect.
#' @return Character; SQL with DATEDIFF translated.
#' @keywords internal
.translate_datediff <- function(sql, dialect) {
  if (!grepl("DATEDIFF", sql, ignore.case = TRUE)) return(sql)

  # These dialects implement the canonical OHDSI/SQL Server spelling.
  if (dialect %in% c("sql server", "redshift", "snowflake")) return(sql)

  identifier <- paste0(
    "[A-Za-z_][A-Za-z0-9_]*",
    "(?:\\.[A-Za-z_][A-Za-z0-9_]*)*"
  )
  pattern <- paste0(
    "\\bDATEDIFF\\s*\\(\\s*day\\s*,\\s*(", identifier, ")",
    "\\s*,\\s*(", identifier, ")\\s*\\)"
  )

  repeat {
    match <- regexec(pattern, sql, ignore.case = TRUE, perl = TRUE)[[1L]]
    if (identical(match[[1L]], -1L)) break
    parts <- regmatches(sql, list(match))[[1L]]
    start <- parts[[2L]]
    end <- parts[[3L]]
    replacement <- switch(dialect,
      postgresql = paste0(
        "(CAST(", end, " AS DATE) - CAST(", start, " AS DATE))"
      ),
      mysql = paste0("DATEDIFF(", end, ", ", start, ")"),
      sqlite = paste0(
        "CAST(julianday(DATE(", end, ")) - julianday(DATE(", start,
        ")) AS INTEGER)"
      ),
      oracle = paste0(
        "CAST(TRUNC(", end, ") - TRUNC(", start, ") AS INTEGER)"
      ),
      bigquery = paste0(
        "DATE_DIFF(CAST(", end, " AS DATE), CAST(", start,
        " AS DATE), DAY)"
      ),
      spark = paste0(
        "DATEDIFF(CAST(", end, " AS DATE), CAST(", start, " AS DATE))"
      ),
      duckdb = paste0(
        "DATE_DIFF('day', CAST(", start, " AS DATE), CAST(", end,
        " AS DATE))"
      )
    )

    match_start <- match[[1L]]
    match_end <- match_start + attr(match, "match.length")[[1L]] - 1L
    sql <- paste0(
      substr(sql, 1L, match_start - 1L), replacement,
      substr(sql, match_end + 1L, nchar(sql))
    )
  }

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
      "duckdb"     = paste0("(", expr, " + ", n, " * INTERVAL '1 day')"),
      "sqlite"     = paste0("DATE(", expr, ", '", n, " days')"),
      "oracle"     = paste0("(", expr, " + ", n, ")"),
      "bigquery"   = paste0("DATE_ADD(", expr, ", INTERVAL ", n, " DAY)"),
      "spark"      = paste0("DATE_ADD(", expr, ", ", n, ")"),
      "mysql"      = paste0("DATE_ADD(", expr, ", INTERVAL ", n, " DAY)")
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
#' @param line_comment_tokens Character vector of protected terminal line
#'   comment placeholders.
#' @return Character; SQL with TOP translated to LIMIT
#' @keywords internal
.translate_top <- function(sql, dialect, line_comment_tokens = character(0)) {
  # SQL Server has native TOP support
  if (dialect == "sql server") return(sql)

  if (!grepl("\\bTOP\\b", sql, ignore.case = TRUE)) return(sql)
  # LIMIT/FETCH binds to a compound query differently from TOP on an
  # individual SELECT branch. Without a full parser, moving TOP across a set
  # operator can silently change the requested population. Protected literals
  # and comments have already been replaced by placeholders by .sql_translate,
  # so fail closed on any remaining set operator.
  if (grepl("\\b(?:UNION|EXCEPT|INTERSECT)\\b", sql,
            ignore.case = TRUE, perl = TRUE)) {
    stop("Cannot safely translate SELECT TOP in SQL containing UNION, EXCEPT, ",
         "or INTERSECT; use canonical OHDSI SqlRender SQL or an explicitly ",
         "reviewed backend-specific query without TOP.", call. = FALSE)
  }

  split_terminal_comments <- function(statement) {
    if (length(line_comment_tokens) == 0L || !nzchar(statement)) {
      return(list(body = statement, comments = ""))
    }
    starts <- vapply(line_comment_tokens, function(token) {
      regexpr(token, statement, fixed = TRUE)[[1L]]
    }, integer(1))
    starts <- sort(starts[starts > 0L])
    for (start in starts) {
      tail <- substr(statement, start, nchar(statement))
      remainder <- tail
      for (token in line_comment_tokens) {
        remainder <- gsub(token, "", remainder, fixed = TRUE)
      }
      if (!nzchar(trimws(remainder))) {
        suffix_start <- start
        while (suffix_start > 1L && grepl(
          "[[:space:]]", substr(statement, suffix_start - 1L,
                                suffix_start - 1L)
        )) {
          suffix_start <- suffix_start - 1L
        }
        return(list(
          body = substr(statement, 1L, suffix_start - 1L),
          comments = substr(statement, suffix_start, nchar(statement))
        ))
      }
    }
    list(body = statement, comments = "")
  }

  # Pattern: SELECT TOP <n> ... FROM ...
  pattern <- "(?i)\\bSELECT\\s+TOP\\s+(\\d+)\\b"

  while (grepl(pattern, sql, perl = TRUE)) {
    m <- regexec(pattern, sql, perl = TRUE)[[1]]
    n <- regmatches(sql, list(m))[[1]][2]

    match_start <- m[1]
    match_end <- match_start + attr(m, "match.length")[1] - 1L

    # Replace "SELECT TOP n" with "SELECT"
    before <- substr(sql, 1L, match_start - 1L)
    after <- substr(sql, match_end + 1L, nchar(sql))

    # Find the end of THIS SELECT. For a TOP inside a CTE/subquery that is the
    # closing parenthesis, not the end of the outer statement. The previous
    # implementation always appended LIMIT at the statement end and silently
    # changed the meaning of nested queries.
    insert_pos <- nchar(after)
    depth <- 0L
    in_single <- FALSE
    in_double <- FALSE
    in_backtick <- FALSE
    i <- 1L
    while (i <= nchar(after)) {
      ch <- substr(after, i, i)
      nxt <- if (i < nchar(after)) substr(after, i + 1L, i + 1L) else ""

      if (in_single) {
        if (ch == "'" && nxt == "'") {
          i <- i + 2L
          next
        }
        if (ch == "'") in_single <- FALSE
      } else if (in_double) {
        if (ch == '"' && nxt == '"') {
          i <- i + 2L
          next
        }
        if (ch == '"') in_double <- FALSE
      } else if (in_backtick) {
        if (ch == "`") in_backtick <- FALSE
      } else if (ch == "'") {
        in_single <- TRUE
      } else if (ch == '"') {
        in_double <- TRUE
      } else if (ch == "`") {
        in_backtick <- TRUE
      } else if (ch == "(") {
        depth <- depth + 1L
      } else if (ch == ")") {
        if (depth == 0L) {
          insert_pos <- i - 1L
          break
        }
        depth <- depth - 1L
      } else if (ch == ";" && depth == 0L) {
        insert_pos <- i - 1L
        break
      }
      i <- i + 1L
    }

    statement <- split_terminal_comments(substr(after, 1L, insert_pos))
    stmt_body <- trimws(statement$body)
    rest <- substr(after, insert_pos + 1L, nchar(after))

    suffix <- if (dialect == "oracle") {
      paste0(" FETCH FIRST ", n, " ROWS ONLY")
    } else {
      paste0(" LIMIT ", n)
    }
    sql <- paste0(
      before, "SELECT ", stmt_body, suffix, statement$comments, rest
    )
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
