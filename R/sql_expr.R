# Module: Typed SQL expression helpers (design-2.10.3 sections 6.3 and 9)
#
# Every NEW emitter of the 2.10.3 recipe engine builds dialect-specific SQL
# through these helpers instead of writing OHDSI/SQL Server spellings
# (DATEADD/DATEDIFF) and relying on the regex translator of sql_translate.R,
# which handles only literal DATEADD offsets and identifier DATEDIFF operands
# and passes dynamic forms through untranslated (defect A-E8). The legacy v1
# paths keep their own helpers and the translator unchanged.
#
# Contract shared by every helper:
#   * `h` is a CDM handle, an execution context carrying `$dialect` or
#     `$handle`, a dialect profile (`.sqlDialectProfile()`), or one DBMS /
#     dialect / family name ("postgresql", "sql server", "mariadb", ...).
#   * Operands are controller-owned SQL expressions (character scalars), never
#     user text; R whole numbers and `Date` values are accepted where a
#     literal makes sense and are rendered with `.sqlIntegerLiteral()` /
#     `.sqlDateLiteral()`. Operands may be columns, arithmetic or the output
#     of another helper ("dynamic forms").
#   * The result is one atomic SQL expression (a call, a CASE or a
#     parenthesised expression) that can be composed with operators, and that
#     the legacy translator leaves byte-identical (`.sql_translate()` is a
#     no-op on it), so a statement mixing legacy and new parts stays correct.
#   * Semantics are identical in every family (NULL in -> NULL out unless a
#     helper documents otherwise); a helper returns NULL when the family
#     cannot express the construct, and `.sqlRequire()` /
#     `.sqlDialectPreflight()` turn that into the typed `dialect_unsupported`
#     rejection naming the construct (never an untranslated statement).
#   * SQLite stores OMOP dates as ISO-8601 text ('YYYY-MM-DD'), as dsOMOP's
#     fixtures and loaders do; SQLite date helpers read and return that text.

# --- Families, constructs and the availability matrix -------------------------

.SQL_FAMILIES <- c("postgresql", "sqlite", "duckdb", "mysql", "sqlserver",
                   "oracle", "redshift", "bigquery", "snowflake", "spark")

# Families with required executable verification in this repository
# (design section 9: PostgreSQL in live CI, SQLite embedded, DuckDB optional
# embedded, MySQL >= 8.0 / MariaDB >= 10.2 in live CI). The other families are
# implemented and verified by rendered-SQL snapshots only ("SQL contract only").
.SQL_EXECUTABLE_FAMILIES <- c("postgresql", "sqlite", "duckdb", "mysql")

# Constructs named by typed rejections and by the preflight. One per row of the
# section 9 matrix, plus the literal/ordering helpers of section 6.3.
.SQL_CONSTRUCTS <- c(
  "date_add_days",    # .sqlDateAddDays with expression operands
  "date_diff_days",   # .sqlDateDiffDays with expression operands
  "calendar",         # .sqlYear, .sqlMonth, .sqlMonthKey, .sqlCalendarFloor/Advance, .sqlMakeDate
  "date_literal",     # .sqlDateLiteral (typed)
  "integer_literal",  # .sqlIntegerLiteral
  "least_greatest",   # .sqlLeast, .sqlGreatest (NULL-ignoring)
  "nulls_last",       # .sqlNullsLast
  "window_functions", # .sqlRowNumber, LAG/LEAD through .sqlWindow
  "window_frames",    # SUM/MAX OVER (ROWS ...) through .sqlWindow(frame = )
  "lossless_key",     # .sqlLosslessKey, .sqlLosslessKeyText
  "date_text",        # .sqlDateText
  "sha256_sql",       # .sqlSha256Hex (optional push-down of the matching order)
  "subqueries_cte",   # EXISTS / NOT EXISTS, correlated COUNT, CTEs
  "session_table"     # session temp tables (a roster reused by several statements)
)

#' Required support of every typed SQL construct per dialect family
#'
#' The design section 9 matrix as data: \code{"executable"} (implemented and
#' executable-tested before release: PostgreSQL, SQLite, DuckDB, MySQL >= 8.0
#' and MariaDB >= 10.2), \code{"snapshot"} (implemented, verified by a rendered
#' SQL snapshot per family), \code{"unsupported"} (the helper returns NULL; a
#' plan that needs the construct is refused at preflight with
#' \code{dialect_unsupported}). These marks are required release support; the
#' runtime versions actually executed are recorded by the conformance tests.
#'
#' @return Character matrix, constructs x families.
#' @keywords internal
.sqlHelperMatrix <- function() {
  m <- matrix("snapshot", nrow = length(.SQL_CONSTRUCTS),
              ncol = length(.SQL_FAMILIES),
              dimnames = list(.SQL_CONSTRUCTS, .SQL_FAMILIES))
  m[, .SQL_EXECUTABLE_FAMILIES] <- "executable"
  m["sha256_sql", "sqlite"] <- "unsupported"
  m["session_table", setdiff(.SQL_FAMILIES, .SQL_EXECUTABLE_FAMILIES)] <-
    "unsupported"
  m
}

# Operation -> constructs used by its compiled SQL (section 6). Advisory input
# for the IR validator's dialect preflight (section 6.1); every plan also needs
# the baseline below. Compilers that start using another construct extend it.
.SQL_BASELINE_CONSTRUCTS <- c("subqueries_cte", "date_literal",
                              "integer_literal", "window_functions",
                              "lossless_key", "nulls_last")
.SQL_OPERATION_CONSTRUCTS <- list(
  concept_set = character(0),
  event = c("date_add_days"),
  event.unit = character(0),
  event.on_missing_table = character(0),
  interval = c("date_add_days", "date_diff_days", "window_frames"),
  interval.ingredient = character(0),
  interval.stockpile = character(0),
  population.index = c("date_add_days", "least_greatest", "date_text"),
  population.first_eligible = character(0),
  population.any_of = character(0),
  population.criteria = c("date_add_days"),
  population.parent = character(0),
  population.setop = character(0),
  population.match = c("date_add_days", "calendar", "date_text"),
  population.coverage.union = c("date_add_days", "window_frames"),
  criteria.not = character(0),
  criteria.age = c("calendar"),
  window.anchor.source = c("date_add_days"),
  variable.reducer = c("date_add_days", "date_diff_days"),
  variable.derived = c("calendar", "date_diff_days"),
  variable.raw = character(0),
  variable.population_column = character(0),
  output.wide = character(0),
  output.long = c("date_diff_days"),
  output.long.event_log = c("date_diff_days"),
  output.survival = c("date_add_days", "date_diff_days", "least_greatest"),
  output.survival.censor_source = c("least_greatest"),
  output.intervals = c("date_diff_days"),
  output.temporal_covariates = c("date_diff_days"),
  output.person_period = c("calendar", "least_greatest", "date_diff_days",
                           "date_add_days"),
  output.person_period.partition = c("calendar", "least_greatest",
                                     "window_frames"),
  output.sparse = character(0),
  output.membership = character(0),
  output.dictionary = character(0),
  output.joined = character(0),
  publish.roots = character(0),
  event.episode_members = character(0),
  event.cost = character(0),
  reducer.supply_days = c("date_add_days", "date_diff_days"),
  reducer.covered_days.effective_supply = character(0),
  status.accessor = character(0),
  verb.mutate = character(0),
  verb.filter_expr = character(0)
)

#' Constructs a plan needs, from the operations it uses
#'
#' @param operations Character vector of section 4.4 operation names.
#' @return Character vector of construct ids (always including the baseline).
#' @keywords internal
.sqlPlanConstructs <- function(operations = character(0)) {
  if (is.null(operations)) operations <- character(0)
  if (!is.character(operations) || anyNA(operations)) {
    stop("operations must be a character vector.", call. = FALSE)
  }
  unknown <- setdiff(operations, names(.SQL_OPERATION_CONSTRUCTS))
  if (length(unknown)) {
    stop("No SQL construct mapping for operation(s): ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  used <- unlist(.SQL_OPERATION_CONSTRUCTS[unique(operations)],
                 use.names = FALSE)
  .SQL_CONSTRUCTS[.SQL_CONSTRUCTS %in% c(.SQL_BASELINE_CONSTRUCTS, used)]
}

# --- Family resolution --------------------------------------------------------

.SQL_FAMILY_OF_TOKEN <- c(
  postgresql = "postgresql", sqlite = "sqlite", duckdb = "duckdb",
  mysql = "mysql", mariadb = "mysql",
  sqlserver = "sqlserver", synapse = "sqlserver", pdw = "sqlserver",
  oracle = "oracle", redshift = "redshift", bigquery = "bigquery",
  snowflake = "snowflake", spark = "spark", databricks = "spark"
)

.SQL_TARGET_DIALECT_OF_FAMILY <- c(
  postgresql = "postgresql", sqlite = "sqlite", duckdb = "duckdb",
  mysql = "mysql", sqlserver = "sql server", oracle = "oracle",
  redshift = "redshift", bigquery = "bigquery", snowflake = "snowflake",
  spark = "spark"
)

# Map one DBMS / dialect / family spelling to c(family, engine).
.sqlFamilyToken <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !nzchar(trimws(name))) {
    stop("SQL dialect must be one non-empty name.", call. = FALSE)
  }
  token <- .normalizeDBMS(name)
  family <- unname(.SQL_FAMILY_OF_TOKEN[token])
  if (length(family) != 1L || is.na(family)) {
    stop("Unsupported SQL dialect family: '", name, "'.", call. = FALSE)
  }
  c(family = family, engine = token)
}

#' Resolve the dialect family of a handle, context, profile or name
#'
#' Families: postgresql, sqlite, duckdb, mysql (MySQL and MariaDB), sqlserver
#' (SQL Server, Synapse, PDW), oracle, redshift, bigquery, snowflake, spark
#' (Spark, Databricks). A handle's \code{target_dialect} (the dialect the
#' legacy translator also uses) decides; its \code{dbms} is the fallback and
#' must not name another family.
#'
#' @param h Handle, execution context, \code{dsomop_sql_dialect} profile or a
#'   DBMS/dialect name.
#' @return One family token.
#' @keywords internal
.sqlFamily <- function(h) .sqlResolve(h)[["family"]]

#' Resolve the engine (finer than the family) of a handle or name
#'
#' @inheritParams .sqlFamily
#' @return One engine token, e.g. \code{"mariadb"} within family \code{mysql}.
#' @keywords internal
.sqlEngine <- function(h) .sqlResolve(h)[["engine"]]

.sqlResolve <- function(h) {
  if (inherits(h, "dsomop_sql_dialect")) {
    return(c(family = h$family, engine = h$engine))
  }
  if (is.character(h)) return(.sqlFamilyToken(h))
  if (is.environment(h) || is.list(h)) {
    dialect <- h[["dialect"]]
    if (inherits(dialect, "dsomop_sql_dialect")) {
      return(c(family = dialect$family, engine = dialect$engine))
    }
    inner <- h[["handle"]]
    if (!is.null(inner) && (is.environment(inner) || is.list(inner))) {
      return(.sqlResolve(inner))
    }
    target <- h[["target_dialect"]]
    dbms <- h[["dbms"]]
    if (is.null(target) && is.null(dbms)) {
      stop("The handle declares neither target_dialect nor dbms.",
           call. = FALSE)
    }
    from_target <- if (!is.null(target)) .sqlFamilyToken(target) else NULL
    from_dbms <- if (!is.null(dbms)) .sqlFamilyToken(dbms) else NULL
    if (!is.null(from_target) && !is.null(from_dbms) &&
        !identical(from_target[["family"]], from_dbms[["family"]])) {
      stop("The handle's target_dialect ('", target, "') and dbms ('", dbms,
           "') name different SQL families.", call. = FALSE)
    }
    if (is.null(from_target)) return(from_dbms)
    if (is.null(from_dbms)) return(from_target)
    return(c(family = from_target[["family"]], engine = from_dbms[["engine"]]))
  }
  stop("Cannot resolve an SQL dialect from an object of class '",
       class(h)[[1L]], "'.", call. = FALSE)
}

# The legacy helpers of sql_translate.R take a handle-like object; give them
# the family's target dialect.
.sqlLegacyHandle <- function(family) {
  list(target_dialect = unname(.SQL_TARGET_DIALECT_OF_FAMILY[[family]]))
}

# --- Operand validation ---------------------------------------------------------

# Defence in depth for controller-owned expressions: one non-empty string with
# balanced parentheses and quotes and no statement separator or comment outside
# quoted segments. This is not a parser; it catches composition bugs such as
# "a--1" (a comment) or a stray ";" before they reach a database.
.sqlOperand <- function(x, what = "SQL expression", family = NULL) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(what, " must be one non-empty SQL expression.", call. = FALSE)
  }
  chars <- strsplit(x, "", fixed = TRUE)[[1L]]
  n <- length(chars)
  backslash <- identical(family, "mysql")
  depth <- 0L
  i <- 1L
  bad <- function(reason) {
    stop(what, " is not a well-formed SQL expression (", reason, ").",
         call. = FALSE)
  }
  while (i <= n) {
    ch <- chars[[i]]
    nxt <- if (i < n) chars[[i + 1L]] else ""
    if (ch %in% c("'", "\"", "`")) {
      j <- i + 1L
      closed <- FALSE
      while (j <= n) {
        cj <- chars[[j]]
        if (backslash && identical(ch, "'") && identical(cj, "\\") && j < n) {
          j <- j + 2L
          next
        }
        if (identical(cj, ch)) {
          if (j < n && identical(chars[[j + 1L]], ch)) {
            j <- j + 2L
            next
          }
          closed <- TRUE
          break
        }
        j <- j + 1L
      }
      if (!closed) bad("unterminated quoted segment")
      i <- j + 1L
      next
    }
    if (identical(ch, "[")) {
      close <- which(chars[seq.int(i, n)] == "]")
      if (!length(close)) bad("unterminated bracket identifier")
      i <- i + close[[1L]]
      next
    }
    if (identical(ch, ";")) bad("statement separator")
    if (identical(ch, "-") && identical(nxt, "-")) bad("line comment")
    if (identical(ch, "/") && identical(nxt, "*")) bad("block comment")
    if (identical(ch, "*") && identical(nxt, "/")) bad("block comment")
    if (identical(ch, "#") && backslash) bad("line comment")
    if (identical(ch, "(")) depth <- depth + 1L
    if (identical(ch, ")")) {
      depth <- depth - 1L
      if (depth < 0L) bad("unbalanced parentheses")
    }
    i <- i + 1L
  }
  if (depth != 0L) bad("unbalanced parentheses")
  x
}

.sqlIsWholeNumber <- function(x) {
  (is.integer(x) || is.double(x) || inherits(x, "integer64")) &&
    !inherits(x, c("Date", "POSIXt", "difftime", "factor"))
}

# An operand that may be an R literal: whole numbers -> integer literal, Date ->
# date literal, character -> validated expression.
.sqlIntegerOperand <- function(h, x, what) {
  if (is.character(x)) return(.sqlOperand(x, what, .sqlFamily(h)))
  if (.sqlIsWholeNumber(x)) return(.sqlIntegerLiteral(h, x))
  stop(what, " must be an SQL expression or one whole number.", call. = FALSE)
}

.sqlDateOperand <- function(h, x, what) {
  if (inherits(x, "Date")) return(.sqlDateLiteral(h, x))
  if (is.character(x)) return(.sqlOperand(x, what, .sqlFamily(h)))
  stop(what, " must be an SQL date expression or one Date.", call. = FALSE)
}

# Integer value of an R literal operand (NULL when the operand is SQL).
.sqlLiteralInteger <- function(x) {
  if (is.character(x) || !.sqlIsWholeNumber(x)) return(NULL)
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1L || is.na(value) || !is.finite(value) ||
      value != trunc(value) || abs(value) > 2^31 - 1) {
    stop("An integer literal operand must be one whole number within the ",
         "32-bit range.", call. = FALSE)
  }
  as.integer(value)
}

.sqlIntegerCast <- function(h, expr) {
  .omopIntegerCastSql(.sqlLegacyHandle(.sqlFamily(h)), expr)
}

# --- Literals -------------------------------------------------------------------

#' Typed SQL integer literal
#'
#' Renders one whole number (integer, whole double up to 2^53, integer64, or a
#' decimal string within the signed 64-bit range) as an SQL literal; negative
#' values are parenthesised so that composition can never form a comment
#' (\code{"a - (-1)"}, never \code{"a--1"}).
#'
#' @param h Handle, context, profile or dialect name (validated; integer
#'   literals are spelled identically in every family).
#' @param x One whole number.
#' @return Character SQL literal.
#' @keywords internal
.sqlIntegerLiteral <- function(h, x) {
  .sqlFamily(h)
  if (length(x) != 1L || is.na(x) || is.logical(x) || is.factor(x)) {
    stop("An SQL integer literal needs one non-missing whole number.",
         call. = FALSE)
  }
  digits <- if (is.character(x)) {
    value <- trimws(x)
    if (!grepl("^-?[0-9]{1,19}$", value)) {
      stop("An SQL integer literal string must be a decimal integer.",
           call. = FALSE)
    }
    negative <- startsWith(value, "-")
    body <- sub("^-?0*([0-9])", "\\1", value)
    limit <- if (negative) "9223372036854775808" else "9223372036854775807"
    if (nchar(body) == 19L && .sqlDigitsGreater(body, limit)) {
      stop("An SQL integer literal must be within the signed 64-bit range.",
           call. = FALSE)
    }
    if (identical(body, "0")) "0" else paste0(if (negative) "-", body)
  } else if (inherits(x, "integer64")) {
    value <- as.character(x)
    if (!grepl("^-?[0-9]+$", value)) {
      stop("An SQL integer literal needs one whole number.", call. = FALSE)
    }
    value
  } else if (is.integer(x)) {
    as.character(x)
  } else if (is.double(x)) {
    if (!is.finite(x) || x != trunc(x) || abs(x) > 2^53) {
      stop("An SQL integer literal needs one finite whole number of at most ",
           "2^53 in magnitude (pass larger keys as decimal strings).",
           call. = FALSE)
    }
    if (x == 0) "0" else formatC(x, format = "f", digits = 0L)
  } else {
    stop("An SQL integer literal needs one whole number.", call. = FALSE)
  }
  if (startsWith(digits, "-")) paste0("(", digits, ")") else digits
}

# Compare two equal-length decimal digit strings without locale collation.
.sqlDigitsGreater <- function(a, b) {
  da <- utf8ToInt(a) - 48L
  db <- utf8ToInt(b) - 48L
  differ <- which(da != db)
  length(differ) > 0L && da[[differ[[1L]]]] > db[[differ[[1L]]]]
}

.sqlParseIsoDate <- function(x) {
  if (inherits(x, "Date")) {
    if (length(x) != 1L || is.na(x)) {
      stop("An SQL date literal needs one non-missing date.", call. = FALSE)
    }
    x <- format(x, "%Y-%m-%d")
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) {
    stop("An SQL date literal needs one ISO date (YYYY-MM-DD).", call. = FALSE)
  }
  if (as.integer(substr(x, 1L, 4L)) < 1000L) {
    stop("Portable SQL dates are between 1000-01-01 and 9999-12-31.",
         call. = FALSE)
  }
  parsed <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  if (is.na(parsed) || !identical(format(parsed, "%Y-%m-%d"), x)) {
    stop("An SQL date literal needs one valid calendar date (YYYY-MM-DD).",
         call. = FALSE)
  }
  x
}

#' Typed SQL date literal
#'
#' \code{DATE 'YYYY-MM-DD'} where the family accepts the standard typed
#' literal, \code{CAST('YYYY-MM-DD' AS DATE)} on SQL Server and Redshift, and
#' the ISO text itself on SQLite (dsOMOP stores SQLite dates as ISO text).
#' Portable dates are 1000-01-01 to 9999-12-31 (the MySQL/MariaDB DATE range).
#'
#' @param h Handle, context, profile or dialect name.
#' @param x One ISO date string or one \code{Date}.
#' @return Character SQL literal of type DATE.
#' @keywords internal
.sqlDateLiteral <- function(h, x) {
  family <- .sqlFamily(h)
  iso <- .sqlParseIsoDate(x)
  quoted <- paste0("'", iso, "'")
  switch(family,
    sqlite = quoted,
    sqlserver =, redshift = paste0("CAST(", quoted, " AS DATE)"),
    paste0("DATE ", quoted)
  )
}

# A typed NULL date (used when a literal construction is invalid).
.sqlNullDate <- function(h) {
  if (identical(.sqlFamily(h), "sqlite")) "NULL" else "CAST(NULL AS DATE)"
}

# --- Date arithmetic --------------------------------------------------------------

#' Add a whole number of days to a date
#'
#' Both operands may be expressions (columns, arithmetic, other helpers); an
#' R whole number is rendered as a literal. The day count must be integer
#' valued (callers reject fractional supplies before arithmetic, section 6.7).
#' Returns a DATE (ISO text on SQLite); NULL when either operand is NULL.
#'
#' @param h Handle, context, profile or dialect name.
#' @param date_expr SQL date expression (or one \code{Date}).
#' @param days_expr SQL integer expression or one whole number.
#' @return Character SQL expression.
#' @keywords internal
.sqlDateAddDays <- function(h, date_expr, days_expr) {
  family <- .sqlFamily(h)
  d <- .sqlDateOperand(h, date_expr, "date_expr")
  literal <- .sqlLiteralInteger(days_expr)
  n <- if (!is.null(literal)) {
    .sqlIntegerLiteral(h, literal)
  } else {
    .sqlIntegerCast(h, .sqlIntegerOperand(h, days_expr, "days_expr"))
  }
  switch(family,
    postgresql = paste0("(CAST(", d, " AS DATE) + ", n, ")"),
    duckdb = paste0("CAST(CAST(", d, " AS DATE) + ", n, " AS DATE)"),
    sqlite = {
      modifier <- if (!is.null(literal)) {
        paste0("'", if (literal >= 0L) "+", literal, " days'")
      } else {
        paste0("CAST(", n, " AS TEXT) || ' days'")
      }
      paste0("date(", d, ", ", modifier, ")")
    },
    mysql = paste0("DATE_ADD(CAST(", d, " AS DATE), INTERVAL ", n, " DAY)"),
    sqlserver = paste0("DATEADD(day, ", n, ", CAST(", d, " AS DATE))"),
    oracle = paste0("(CAST(", d, " AS DATE) + ", n, ")"),
    redshift = paste0("CAST(DATEADD(day, ", n, ", CAST(", d,
                      " AS DATE)) AS DATE)"),
    bigquery = paste0("DATE_ADD(CAST(", d, " AS DATE), INTERVAL ", n, " DAY)"),
    snowflake = paste0("DATEADD(day, ", n, ", CAST(", d, " AS DATE))"),
    spark = paste0("DATE_ADD(CAST(", d, " AS DATE), ", n, ")")
  )
}

#' Whole days between two dates (to - from)
#'
#' Calendar-day difference of the two dates (time parts ignored), as an
#' integer; NULL when either operand is NULL.
#'
#' @param h Handle, context, profile or dialect name.
#' @param from_expr SQL date expression of the earlier bound (or a \code{Date}).
#' @param to_expr SQL date expression of the later bound (or a \code{Date}).
#' @return Character SQL integer expression.
#' @keywords internal
.sqlDateDiffDays <- function(h, from_expr, to_expr) {
  family <- .sqlFamily(h)
  a <- .sqlDateOperand(h, from_expr, "from_expr")
  b <- .sqlDateOperand(h, to_expr, "to_expr")
  ca <- paste0("CAST(", a, " AS DATE)")
  cb <- paste0("CAST(", b, " AS DATE)")
  switch(family,
    postgresql = paste0("(", cb, " - ", ca, ")"),
    duckdb = paste0("CAST(DATE_DIFF('day', ", ca, ", ", cb, ") AS INTEGER)"),
    sqlite = paste0("CAST(julianday(date(", b, ")) - julianday(date(", a,
                    ")) AS INTEGER)"),
    mysql = paste0("DATEDIFF(", cb, ", ", ca, ")"),
    sqlserver = paste0("DATEDIFF(day, ", ca, ", ", cb, ")"),
    oracle = paste0("CAST(TRUNC(", cb, ") - TRUNC(", ca, ") AS INTEGER)"),
    redshift = paste0("CAST(DATEDIFF(day, ", ca, ", ", cb, ") AS INTEGER)"),
    bigquery = paste0("DATE_DIFF(", cb, ", ", ca, ", DAY)"),
    snowflake = paste0("CAST(DATEDIFF(day, ", ca, ", ", cb, ") AS INTEGER)"),
    spark = paste0("DATEDIFF(", cb, ", ", ca, ")")
  )
}

# --- Calendar ---------------------------------------------------------------------

.sqlDatePart <- function(h, date_expr, part) {
  family <- .sqlFamily(h)
  d <- .sqlDateOperand(h, date_expr, "date_expr")
  word <- switch(part, year = "YEAR", month = "MONTH")
  switch(family,
    sqlite = paste0("CAST(strftime('", switch(part, year = "%Y", month = "%m"),
                    "', ", d, ") AS INTEGER)"),
    mysql =, sqlserver =, spark = paste0(word, "(", d, ")"),
    bigquery = paste0("EXTRACT(", word, " FROM ", d, ")"),
    .sqlIntegerCast(h, paste0("EXTRACT(", word, " FROM ", d, ")"))
  )
}

#' Calendar year of a date, as an integer
#'
#' @param h Handle, context, profile or dialect name.
#' @param date_expr SQL date expression (or a \code{Date}).
#' @return Character SQL integer expression.
#' @keywords internal
.sqlYear <- function(h, date_expr) .sqlDatePart(h, date_expr, "year")

#' Calendar month (1-12) of a date, as an integer
#'
#' @inheritParams .sqlYear
#' @return Character SQL integer expression.
#' @keywords internal
.sqlMonth <- function(h, date_expr) .sqlDatePart(h, date_expr, "month")

#' Integer month key YYYYMM of a date
#'
#' @inheritParams .sqlYear
#' @return Character SQL integer expression (\code{year * 100 + month}).
#' @keywords internal
.sqlMonthKey <- function(h, date_expr) {
  paste0("(", .sqlYear(h, date_expr), " * 100 + ", .sqlMonth(h, date_expr),
         ")")
}

.SQL_CALENDAR_UNITS <- c("month", "quarter", "year")

#' First day of the calendar period containing a date
#'
#' @param h Handle, context, profile or dialect name.
#' @param date_expr SQL date expression (or a \code{Date}).
#' @param unit One of \code{"month"}, \code{"quarter"}, \code{"year"}.
#' @return Character SQL DATE expression (ISO text on SQLite).
#' @keywords internal
.sqlCalendarFloor <- function(h, date_expr, unit = c("month", "quarter",
                                                      "year")) {
  unit <- match.arg(unit)
  family <- .sqlFamily(h)
  d <- .sqlDateOperand(h, date_expr, "date_expr")
  cd <- paste0("CAST(", d, " AS DATE)")
  switch(family,
    postgresql =, duckdb =, redshift =, snowflake =, spark =
      paste0("CAST(DATE_TRUNC('", unit, "', ", cd, ") AS DATE)"),
    sqlite = switch(unit,
      month = paste0("date(", d, ", 'start of month')"),
      year = paste0("date(", d, ", 'start of year')"),
      quarter = paste0(
        "date(", d, ", 'start of month', '-' || ((CAST(strftime('%m', ", d,
        ") AS INTEGER) - 1) % 3) || ' months')"
      )
    ),
    mysql = switch(unit,
      year = paste0("MAKEDATE(YEAR(", d, "), 1)"),
      month = paste0("DATE_ADD(MAKEDATE(YEAR(", d, "), 1), INTERVAL (MONTH(",
                     d, ") - 1) MONTH)"),
      quarter = paste0("DATE_ADD(MAKEDATE(YEAR(", d, "), 1), INTERVAL ",
                       "((QUARTER(", d, ") - 1) * 3) MONTH)")
    ),
    sqlserver = switch(unit,
      year = paste0("DATEFROMPARTS(YEAR(", d, "), 1, 1)"),
      month = paste0("DATEFROMPARTS(YEAR(", d, "), MONTH(", d, "), 1)"),
      quarter = paste0("DATEFROMPARTS(YEAR(", d, "), (DATEPART(quarter, ", d,
                       ") - 1) * 3 + 1, 1)")
    ),
    oracle = paste0("TRUNC(", cd, ", '",
                    switch(unit, month = "MM", quarter = "Q", year = "YYYY"),
                    "')"),
    bigquery = paste0("DATE_TRUNC(", cd, ", ", toupper(unit), ")")
  )
}

#' First day of the n-th calendar period after the one containing a date
#'
#' \code{n_expr = 0} is the floor of \code{date_expr}; negative values move
#' backwards. Because the result is built from the first day of a period, no
#' end-of-month clipping occurs.
#'
#' @param h Handle, context, profile or dialect name.
#' @param date_expr SQL date expression (or a \code{Date}).
#' @param n_expr SQL integer expression or one whole number.
#' @param unit One of \code{"month"}, \code{"quarter"}, \code{"year"}.
#' @return Character SQL DATE expression (ISO text on SQLite).
#' @keywords internal
.sqlCalendarAdvance <- function(h, date_expr, n_expr, unit) {
  if (!is.character(unit) || length(unit) != 1L ||
      !unit %in% .SQL_CALENDAR_UNITS) {
    stop("unit must be one of month, quarter, year.", call. = FALSE)
  }
  family <- .sqlFamily(h)
  floor <- .sqlCalendarFloor(h, date_expr, unit)
  per <- switch(unit, month = 1L, quarter = 3L, year = 12L)
  literal <- .sqlLiteralInteger(n_expr)
  months <- if (!is.null(literal)) {
    .sqlIntegerLiteral(h, literal * per)
  } else {
    n <- .sqlIntegerCast(h, .sqlIntegerOperand(h, n_expr, "n_expr"))
    if (per == 1L) n else paste0("(", n, " * ", per, ")")
  }
  switch(family,
    postgresql =, duckdb =
      paste0("CAST(", floor, " + ", months, " * INTERVAL '1 month' AS DATE)"),
    redshift = paste0("CAST(DATEADD(month, ", months, ", ", floor,
                      ") AS DATE)"),
    sqlite = {
      modifier <- if (!is.null(literal)) {
        value <- literal * per
        paste0("'", if (value >= 0L) "+", value, " months'")
      } else {
        paste0("CAST(", months, " AS TEXT) || ' months'")
      }
      paste0("date(", floor, ", ", modifier, ")")
    },
    mysql = paste0("DATE_ADD(", floor, ", INTERVAL ", months, " MONTH)"),
    sqlserver =, snowflake = paste0("DATEADD(month, ", months, ", ", floor,
                                    ")"),
    oracle =, spark = paste0("ADD_MONTHS(", floor, ", ", months, ")"),
    bigquery = paste0("DATE_ADD(", floor, ", INTERVAL ", months, " MONTH)")
  )
}

# First day of month y-m for integer expressions with 1000 <= y <= 9999 and
# 1 <= m <= 12 (guarded by the caller).
.sqlFirstOfMonth <- function(h, yi, mi) {
  switch(.sqlFamily(h),
    postgresql =, duckdb =, spark = paste0("MAKE_DATE(", yi, ", ", mi, ", 1)"),
    sqlite = paste0("date(printf('%04d-%02d-01', ", yi, ", ", mi, "))"),
    mysql = paste0("DATE_ADD(MAKEDATE(", yi, ", 1), INTERVAL (", mi,
                   " - 1) MONTH)"),
    sqlserver = paste0("DATEFROMPARTS(", yi, ", ", mi, ", 1)"),
    oracle = paste0("ADD_MONTHS(TO_DATE(TO_CHAR(", yi,
                    ", 'FM0000') || '-01-01', 'YYYY-MM-DD'), ", mi, " - 1)"),
    redshift = paste0("CAST(DATEADD(month, ", mi, " - 1, TO_DATE(CAST(", yi,
                      " AS VARCHAR(4)) || '-01-01', 'YYYY-MM-DD')) AS DATE)"),
    bigquery = paste0("DATE(", yi, ", ", mi, ", 1)"),
    snowflake = paste0("DATE_FROM_PARTS(", yi, ", ", mi, ", 1)")
  )
}

.sqlDaysInMonth <- function(year, month) {
  first <- as.Date(sprintf("%04d-%02d-01", year, month))
  next_first <- seq(first, by = "1 month", length.out = 2L)[[2L]]
  as.integer(next_first - first)
}

#' Build a date from year, month and day
#'
#' Returns the DATE when the components are integers with year 1000-9999,
#' month 1-12 and day 1 to the length of that month; any other combination,
#' and any NULL component, gives NULL (never a database error and never a
#' silently normalised date such as 2019-02-29 -> 2019-03-01). The check is a
#' nested CASE so that no family evaluates an invalid constructor; when all
#' three components are R literals the date is computed in R.
#'
#' @param h Handle, context, profile or dialect name.
#' @param year_expr,month_expr,day_expr SQL integer expressions or whole
#'   numbers.
#' @return Character SQL DATE expression (ISO text on SQLite).
#' @keywords internal
.sqlMakeDate <- function(h, year_expr, month_expr, day_expr) {
  .sqlFamily(h)
  literals <- list(.sqlLiteralInteger(year_expr), .sqlLiteralInteger(month_expr),
                   .sqlLiteralInteger(day_expr))
  if (!any(vapply(literals, is.null, logical(1)))) {
    y <- literals[[1L]]
    m <- literals[[2L]]
    d <- literals[[3L]]
    valid <- y >= 1000L && y <= 9999L && m >= 1L && m <= 12L && d >= 1L &&
      d <= .sqlDaysInMonth(y, m)
    if (!valid) return(.sqlNullDate(h))
    return(.sqlDateLiteral(h, sprintf("%04d-%02d-%02d", y, m, d)))
  }
  yi <- .sqlIntegerCast(h, .sqlIntegerOperand(h, year_expr, "year_expr"))
  mi <- .sqlIntegerCast(h, .sqlIntegerOperand(h, month_expr, "month_expr"))
  di <- .sqlIntegerCast(h, .sqlIntegerOperand(h, day_expr, "day_expr"))
  built <- .sqlDateAddDays(h, .sqlFirstOfMonth(h, yi, mi), paste0(di, " - 1"))
  paste0(
    "CASE WHEN ", yi, " BETWEEN 1000 AND 9999 AND ", mi, " BETWEEN 1 AND 12",
    " AND ", di, " BETWEEN 1 AND 31 THEN CASE WHEN ", .sqlMonth(h, built),
    " = ", mi, " THEN ", built, " END END"
  )
}

#' ISO text ('YYYY-MM-DD') of a date
#'
#' @inheritParams .sqlYear
#' @return Character SQL text expression; NULL for a NULL date.
#' @keywords internal
.sqlDateText <- function(h, date_expr) {
  family <- .sqlFamily(h)
  d <- .sqlDateOperand(h, date_expr, "date_expr")
  cd <- paste0("CAST(", d, " AS DATE)")
  switch(family,
    postgresql =, redshift =, oracle =, snowflake =
      paste0("TO_CHAR(", cd, ", 'YYYY-MM-DD')"),
    duckdb = paste0("STRFTIME(", cd, ", '%Y-%m-%d')"),
    sqlite = paste0("date(", d, ")"),
    mysql = paste0("DATE_FORMAT(", cd, ", '%Y-%m-%d')"),
    sqlserver = paste0("CONVERT(VARCHAR(10), ", cd, ", 23)"),
    bigquery = paste0("FORMAT_DATE('%Y-%m-%d', ", cd, ")"),
    spark = paste0("DATE_FORMAT(", cd, ", 'yyyy-MM-dd')")
  )
}

# --- Comparison and ordering --------------------------------------------------------

.sqlExtremum <- function(h, operands, fn) {
  family <- .sqlFamily(h)
  if (length(operands) == 0L) {
    stop(".sql", if (fn == "LEAST") "Least" else "Greatest",
         " needs at least one operand.", call. = FALSE)
  }
  ops <- vapply(operands, function(x) {
    if (inherits(x, "Date")) return(.sqlDateLiteral(h, x))
    .sqlIntegerOperand(h, x, "operand")
  }, character(1), USE.NAMES = FALSE)
  if (length(ops) == 1L) return(paste0("(", ops[[1L]], ")"))
  n <- length(ops)
  if (family %in% c("postgresql", "duckdb")) {
    # Native LEAST/GREATEST ignore NULL operands here (executable-tested).
    return(paste0(fn, "(", paste(ops, collapse = ", "), ")"))
  }
  if (identical(family, "sqlserver")) {
    # No LEAST/GREATEST on Synapse/PDW and before SQL Server 2022: a CASE that
    # picks the first operand not beaten by any later non-NULL operand.
    cmp <- if (fn == "LEAST") " <= " else " >= "
    arms <- vapply(seq_len(n - 1L), function(i) {
      later <- vapply(seq.int(i + 1L, n), function(j) {
        paste0("((", ops[[j]], ") IS NULL OR (", ops[[i]], ")", cmp, "(",
               ops[[j]], "))")
      }, character(1))
      paste0("WHEN (", ops[[i]], ") IS NOT NULL AND ",
             paste(later, collapse = " AND "), " THEN (", ops[[i]], ")")
    }, character(1))
    return(paste0("CASE ", paste(arms, collapse = " "), " ELSE (", ops[[n]],
                  ") END"))
  }
  # NULL-propagating LEAST/GREATEST (SQLite scalar MIN/MAX, MySQL, MariaDB,
  # Oracle, BigQuery, Snowflake) or vendor-dependent semantics: give every
  # argument a non-NULL substitute taken from the other operands, so the
  # result ignores NULLs and is NULL only when every operand is NULL.
  native <- if (identical(family, "sqlite")) {
    if (fn == "LEAST") "MIN" else "MAX"
  } else {
    fn
  }
  coalesced <- vapply(seq_len(n), function(i) {
    paste0("COALESCE(", paste(c(ops[[i]], ops[-i]), collapse = ", "), ")")
  }, character(1))
  paste0(native, "(", paste(coalesced, collapse = ", "), ")")
}

#' Smallest non-NULL operand
#'
#' NULL operands are ignored; the result is NULL only when every operand is
#' NULL (PostgreSQL's LEAST semantics, used for capped ends and censoring) in
#' every family.
#'
#' @param h Handle, context, profile or dialect name.
#' @param ... SQL expressions, whole numbers or \code{Date}s.
#' @return Character SQL expression.
#' @keywords internal
.sqlLeast <- function(h, ...) .sqlExtremum(h, list(...), "LEAST")

#' Largest non-NULL operand
#'
#' @inheritParams .sqlLeast
#' @return Character SQL expression.
#' @keywords internal
.sqlGreatest <- function(h, ...) .sqlExtremum(h, list(...), "GREATEST")

#' ORDER BY item(s) sorting NULLs last
#'
#' @param h Handle, context, profile or dialect name.
#' @param order_expr SQL expression to order by.
#' @param descending Logical; descending order (NULLs still last).
#' @return Character ORDER BY fragment (one or two comma-separated items).
#' @keywords internal
.sqlNullsLast <- function(h, order_expr, descending = FALSE) {
  family <- .sqlFamily(h)
  x <- .sqlOperand(order_expr, "order_expr", family)
  if (!is.logical(descending) || length(descending) != 1L ||
      is.na(descending)) {
    stop("descending must be TRUE or FALSE.", call. = FALSE)
  }
  direction <- if (descending) "DESC" else "ASC"
  if (family %in% c("sqlite", "mysql", "sqlserver")) {
    return(paste0("CASE WHEN (", x, ") IS NULL THEN 1 ELSE 0 END, ", x, " ",
                  direction))
  }
  paste0(x, " ", direction, " NULLS LAST")
}

# --- Window functions -----------------------------------------------------------------

.SQL_WINDOW_FRAMES <- c(
  preceding = "ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING",
  cumulative = "ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
  partition = "ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING"
)
.SQL_RANKING_FUNCTIONS <- c("ROW_NUMBER", "RANK", "DENSE_RANK", "PERCENT_RANK",
                            "CUME_DIST", "NTILE", "LAG", "LEAD")

.sqlExpressionList <- function(x, what, family) {
  if (is.null(x)) return(character(0))
  if (!is.character(x) || anyNA(x)) {
    stop(what, " must be a character vector of SQL expressions.",
         call. = FALSE)
  }
  vapply(x, .sqlOperand, character(1), what = what, family = family,
         USE.NAMES = FALSE)
}

#' Window function call with an explicit, closed frame vocabulary
#'
#' Ranking and navigation functions (ROW_NUMBER, RANK, LAG, LEAD, ...) take no
#' frame and need an ORDER BY. Every other function with an ORDER BY must name
#' its frame: \code{"preceding"} (rows before the current one, as the era
#' algorithm's 1-PRECEDING frame), \code{"cumulative"} (up to the current row)
#' or \code{"partition"} (the whole partition). The implicit RANGE frame, whose
#' peers make results depend on ties, is never emitted.
#'
#' @param h Handle, context, profile or dialect name.
#' @param call SQL function call, e.g. \code{"MAX(r.end_date)"}.
#' @param partition Character vector of PARTITION BY expressions.
#' @param order Character vector of ORDER BY items (e.g. from
#'   \code{.sqlNullsLast}); must make the order deterministic.
#' @param frame NULL or one of \code{"preceding"}, \code{"cumulative"},
#'   \code{"partition"}.
#' @return Character SQL expression.
#' @keywords internal
.sqlWindow <- function(h, call, partition = character(0),
                       order = character(0), frame = NULL) {
  family <- .sqlFamily(h)
  fn <- .sqlOperand(call, "window call", family)
  partition <- .sqlExpressionList(partition, "partition", family)
  order <- .sqlExpressionList(order, "order", family)
  name <- toupper(sub("^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\(.*$", "\\1", fn))
  ranking <- name %in% .SQL_RANKING_FUNCTIONS
  if (!is.null(frame)) {
    if (!is.character(frame) || length(frame) != 1L ||
        !frame %in% names(.SQL_WINDOW_FRAMES)) {
      stop("frame must be NULL or one of ",
           paste(names(.SQL_WINDOW_FRAMES), collapse = ", "), ".",
           call. = FALSE)
    }
    if (ranking) {
      stop(name, "() takes no window frame.", call. = FALSE)
    }
  }
  if (ranking && length(order) == 0L) {
    stop(name, "() needs a deterministic ORDER BY.", call. = FALSE)
  }
  if (!ranking && length(order) > 0L && is.null(frame)) {
    stop("A window aggregate with ORDER BY must declare its frame.",
         call. = FALSE)
  }
  if (!is.null(frame) && !identical(frame, "partition") &&
      length(order) == 0L) {
    stop("The '", frame, "' frame needs a deterministic ORDER BY.",
         call. = FALSE)
  }
  over <- c(
    if (length(partition)) paste0("PARTITION BY ", paste(partition,
                                                          collapse = ", ")),
    if (length(order)) paste0("ORDER BY ", paste(order, collapse = ", ")),
    if (!is.null(frame)) unname(.SQL_WINDOW_FRAMES[[frame]])
  )
  paste0(fn, " OVER (", paste(over, collapse = " "), ")")
}

#' ROW_NUMBER() over a partition and a deterministic order
#'
#' @param h Handle, context, profile or dialect name.
#' @param partition Character vector of PARTITION BY expressions (may be
#'   empty).
#' @param order Character vector of ORDER BY items (required).
#' @return Character SQL expression.
#' @keywords internal
.sqlRowNumber <- function(h, partition, order) {
  .sqlWindow(h, "ROW_NUMBER()", partition = partition, order = order)
}

# --- Keys and hashing -------------------------------------------------------------------

#' Lossless 64-bit integer key
#'
#' The family's BIGINT cast (\code{.omopBigIntegerCastSql}): \code{BIGINT},
#' \code{SIGNED} on MySQL/MariaDB, \code{INT64} on BigQuery,
#' \code{NUMBER(19)} on Oracle.
#'
#' @param h Handle, context, profile or dialect name.
#' @param expr SQL integer expression (a source row key).
#' @return Character SQL expression.
#' @keywords internal
.sqlLosslessKey <- function(h, expr) {
  family <- .sqlFamily(h)
  .omopBigIntegerCastSql(.sqlLegacyHandle(family),
                         .sqlOperand(expr, "key expression", family))
}

#' Decimal text of a lossless 64-bit integer key
#'
#' Text form used in origin identities (\code{index_key}, section 6.9): the
#' decimal digits of the BIGINT value, with no exponent or fraction.
#'
#' @inheritParams .sqlLosslessKey
#' @return Character SQL text expression.
#' @keywords internal
.sqlLosslessKeyText <- function(h, expr) {
  family <- .sqlFamily(h)
  .omopTextCastSql(.sqlLegacyHandle(family), .sqlLosslessKey(h, expr), 20L)
}

#' Lowercase hexadecimal SHA-256 digest
#'
#' \code{input = "text"} hashes the UTF-8 bytes of a text expression;
#' \code{input = "hex"} hashes the bytes spelled by a hexadecimal text
#' expression (for binary encodings such as the matching order of section
#' 6.10). Returns NULL where the family has no SQL SHA-256 (SQLite: matching
#' then computes the order in R) or where a probed runtime lacks it.
#'
#' @param h Handle, context, profile or dialect name.
#' @param expr SQL text expression.
#' @param input \code{"text"} or \code{"hex"}.
#' @return Character SQL expression yielding 64 lowercase hex characters, or
#'   NULL when unavailable.
#' @keywords internal
.sqlSha256Hex <- function(h, expr, input = c("text", "hex")) {
  input <- match.arg(input)
  family <- .sqlFamily(h)
  x <- .sqlOperand(expr, "hash input", family)
  profile <- .sqlCachedProfile(h)
  if (!is.null(profile) && isFALSE(profile$sha256_sql)) return(NULL)
  text <- identical(input, "text")
  switch(family,
    sqlite = NULL,
    postgresql = if (text) {
      paste0("ENCODE(SHA256(CONVERT_TO(", x, ", 'UTF8')), 'hex')")
    } else {
      paste0("ENCODE(SHA256(DECODE(", x, ", 'hex')), 'hex')")
    },
    duckdb = if (text) {
      paste0("SHA256(", x, ")")
    } else {
      paste0("SHA256(UNHEX(", x, "))")
    },
    mysql = if (text) {
      paste0("LOWER(SHA2(CONVERT(", x, " USING utf8mb4), 256))")
    } else {
      paste0("LOWER(SHA2(UNHEX(", x, "), 256))")
    },
    sqlserver = if (text) {
      paste0("LOWER(CONVERT(VARCHAR(64), HASHBYTES('SHA2_256', CAST(", x,
             " AS VARCHAR(8000))), 2))")
    } else {
      paste0("LOWER(CONVERT(VARCHAR(64), HASHBYTES('SHA2_256', ",
             "CONVERT(VARBINARY(8000), ", x, ", 2)), 2))")
    },
    oracle = if (text) {
      paste0("LOWER(RAWTOHEX(STANDARD_HASH(", x, ", 'SHA256')))")
    } else {
      paste0("LOWER(RAWTOHEX(STANDARD_HASH(HEXTORAW(", x, "), 'SHA256')))")
    },
    redshift = if (text) {
      paste0("SHA2(", x, ", 256)")
    } else {
      paste0("SHA2(FROM_HEX(", x, "), 256)")
    },
    bigquery = if (text) {
      paste0("TO_HEX(SHA256(", x, "))")
    } else {
      paste0("TO_HEX(SHA256(FROM_HEX(", x, ")))")
    },
    snowflake = if (text) {
      paste0("SHA2(", x, ", 256)")
    } else {
      paste0("SHA2(TO_BINARY(", x, ", 'HEX'), 256)")
    },
    spark = if (text) {
      paste0("SHA2(", x, ", 256)")
    } else {
      paste0("SHA2(UNHEX(", x, "), 256)")
    }
  )
}

# --- Typed rejection, availability and preflight ------------------------------------

#' Signal the typed dialect_unsupported rejection
#'
#' The condition has classes \code{dsomop_dialect_unsupported},
#' \code{dsomop_plan_rejection}, \code{error}, \code{condition} and fields
#' \code{code = "dialect_unsupported"}, \code{construct}, \code{family} and
#' \code{output}. Its message names the output (when given), the construct and
#' the family, never data values or counts (section 7.3).
#'
#' @param h Handle, context, profile or dialect name.
#' @param construct One construct id of \code{.SQL_CONSTRUCTS}.
#' @param output Optional output name the construct belongs to.
#' @keywords internal
.sqlDialectUnsupported <- function(h, construct, output = NULL) {
  if (!is.character(construct) || length(construct) != 1L ||
      !construct %in% .SQL_CONSTRUCTS) {
    stop("Unknown SQL construct.", call. = FALSE)
  }
  family <- .sqlFamily(h)
  if (!is.null(output) && (!is.character(output) || length(output) != 1L ||
                           is.na(output) || !nzchar(output))) {
    output <- NULL
  }
  message <- paste0(
    "dialect_unsupported: ",
    if (!is.null(output)) paste0("output '", output, "' needs "),
    if (is.null(output)) "the plan needs ",
    "the SQL construct '", construct, "', which is not available for the ",
    family, " SQL family of this server."
  )
  stop(structure(
    list(message = message, call = NULL, code = "dialect_unsupported",
         construct = construct, family = family, output = output),
    class = c("dsomop_dialect_unsupported", "dsomop_plan_rejection", "error",
              "condition")
  ))
}

#' Is a construct available for a family (static matrix plus probed runtime)?
#'
#' @param h Handle, context, profile or dialect name.
#' @param construct One construct id.
#' @return \code{TRUE} or \code{FALSE}.
#' @keywords internal
.sqlHelperAvailable <- function(h, construct) {
  if (!is.character(construct) || length(construct) != 1L ||
      !construct %in% .SQL_CONSTRUCTS) {
    stop("Unknown SQL construct '", paste(construct, collapse = ", "), "'.",
         call. = FALSE)
  }
  family <- .sqlFamily(h)
  if (identical(.sqlHelperMatrix()[construct, family], "unsupported")) {
    return(FALSE)
  }
  profile <- if (inherits(h, "dsomop_sql_dialect")) h else .sqlCachedProfile(h)
  if (!is.null(profile)) {
    if (construct %in% c("window_functions", "window_frames") &&
        isFALSE(profile$window_frames)) return(FALSE)
    if (identical(construct, "sha256_sql") && isFALSE(profile$sha256_sql)) {
      return(FALSE)
    }
    if (identical(construct, "session_table") &&
        !identical(profile$temporary_materialization, "session_table")) {
      return(FALSE)
    }
    if (isFALSE(profile$analytic_baseline)) return(FALSE)
  }
  TRUE
}

#' Require a construct, or refuse with dialect_unsupported
#'
#' With \code{expr} given (the result of a helper), returns it when it is not
#' NULL; otherwise checks the construct's availability.
#'
#' @param h Handle, context, profile or dialect name.
#' @param construct One construct id.
#' @param expr Optional helper result to pass through.
#' @param output Optional output name for the rejection.
#' @return \code{expr} (or \code{TRUE}) invisibly when available.
#' @keywords internal
.sqlRequire <- function(h, construct, expr = NULL, output = NULL) {
  if (!missing(expr)) {
    if (is.null(expr)) .sqlDialectUnsupported(h, construct, output)
    return(expr)
  }
  if (!.sqlHelperAvailable(h, construct)) {
    .sqlDialectUnsupported(h, construct, output)
  }
  invisible(TRUE)
}

#' Dialect preflight of a plan (design sections 6.1 and 9)
#'
#' Called by the IR validator before any query of the plan runs: every
#' construct the plan needs must be available for the server's family, a
#' plan that reuses a roster across statements needs session temp tables,
#' and the runtime facts that matter are read from the live connection (the
#' SQLite version and its window-frame support are executed, never inferred
#' from a package version). The first missing construct is refused with the
#' typed \code{dialect_unsupported} rejection.
#'
#' @param h Handle (probed when it has a connection), context, profile or
#'   dialect name.
#' @param constructs Character vector of construct ids (e.g. from
#'   \code{.sqlPlanConstructs()}).
#' @param session_tables Logical; does the plan reuse a roster in more than one
#'   statement.
#' @param output Optional output name for the rejection.
#' @return The dialect profile, invisibly.
#' @keywords internal
.sqlDialectPreflight <- function(h, constructs = character(0),
                                 session_tables = FALSE, output = NULL) {
  if (is.null(constructs)) constructs <- character(0)
  if (!is.character(constructs) || anyNA(constructs)) {
    stop("constructs must be a character vector.", call. = FALSE)
  }
  unknown <- setdiff(constructs, .SQL_CONSTRUCTS)
  if (length(unknown)) {
    stop("Unknown SQL construct(s): ", paste(unknown, collapse = ", "), ".",
         call. = FALSE)
  }
  if (!is.logical(session_tables) || length(session_tables) != 1L ||
      is.na(session_tables)) {
    stop("session_tables must be TRUE or FALSE.", call. = FALSE)
  }
  profile <- .sqlDialectProfile(h, probe = TRUE)
  if (isFALSE(profile$analytic_baseline)) {
    .sqlDialectUnsupported(profile, "subqueries_cte", output)
  }
  needed <- unique(c(constructs, if (session_tables) "session_table"))
  for (construct in .SQL_CONSTRUCTS[.SQL_CONSTRUCTS %in% needed]) {
    if (!.sqlHelperAvailable(profile, construct)) {
      .sqlDialectUnsupported(profile, construct, output)
    }
  }
  invisible(profile)
}

#' Refuse legacy OHDSI date spellings in new SQL
#'
#' New statements must build date arithmetic with the typed helpers. Outside
#' quoted segments and comments, \code{DATEADD(} and \code{DATEDIFF(day, }
#' (the forms the regex translator rewrites) may appear only in families whose
#' helpers emit them natively (SQL Server, Redshift, Snowflake).
#'
#' @param h Handle, context, profile or dialect name.
#' @param sql One SQL statement or fragment.
#' @return \code{sql}, invisibly.
#' @keywords internal
.sqlAssertTypedSql <- function(h, sql) {
  family <- .sqlFamily(h)
  if (!is.character(sql) || length(sql) != 1L || is.na(sql)) {
    stop("sql must be one SQL string.", call. = FALSE)
  }
  if (family %in% c("sqlserver", "redshift", "snowflake")) {
    return(invisible(sql))
  }
  protected <- .protectSqlTranslationSegments(
    sql, unname(.SQL_TARGET_DIALECT_OF_FAMILY[[family]])
  )$sql
  if (grepl("\\bDATEADD\\s*\\(", protected, ignore.case = TRUE, perl = TRUE) ||
      grepl("\\bDATEDIFF\\s*\\(\\s*day\\s*,", protected, ignore.case = TRUE,
            perl = TRUE)) {
    stop("New SQL for the ", family, " family contains an OHDSI DATEADD/",
         "DATEDIFF spelling; build date arithmetic with the typed helpers of ",
         "R/sql_expr.R (.sqlDateAddDays, .sqlDateDiffDays).", call. = FALSE)
  }
  invisible(sql)
}

# --- Dialect profile and capability -------------------------------------------------------

.sqlCachedProfile <- function(h) {
  if (inherits(h, "dsomop_sql_dialect")) return(h)
  if (is.environment(h) || is.list(h)) {
    dialect <- h[["dialect"]]
    if (inherits(dialect, "dsomop_sql_dialect")) return(dialect)
    cached <- h[["sql_dialect_profile"]]
    if (inherits(cached, "dsomop_sql_dialect") &&
        identical(cached$family, tryCatch(.sqlFamily(h),
                                          error = function(e) NA))) {
      return(cached)
    }
    inner <- h[["handle"]]
    if (!is.null(inner) && (is.environment(inner) || is.list(inner))) {
      return(.sqlCachedProfile(inner))
    }
  }
  NULL
}

.sqlHasConnection <- function(h) {
  (is.environment(h) || is.list(h)) &&
    (!is.null(h[["conn"]]) || !is.null(h[["resource_client"]]))
}

.sqlQueryOne <- function(handle, sql) {
  result <- .withDbReconnect(handle, function(conn) DBI::dbGetQuery(conn, sql))
  if (!is.data.frame(result) || nrow(result) < 1L || ncol(result) < 1L) {
    return(NULL)
  }
  result
}

.sqlDriverLabel <- function(handle) {
  conn <- tryCatch(.conn(handle), error = function(e) NULL)
  if (is.null(conn)) return(NULL)
  package <- attr(class(conn), "package")
  if (is.null(package)) {
    package <- switch(class(conn)[[1L]],
      duckdb_connection = "duckdb", NULL)
  }
  if (is.null(package) || !nzchar(package)) return(NULL)
  version <- tryCatch(as.character(utils::packageVersion(package)),
                      error = function(e) NULL)
  paste(c(package, version), collapse = " ")
}

.sqlRuntimeVersion <- function(handle, family) {
  tryCatch(switch(family,
    sqlite = {
      r <- .sqlQueryOne(handle, "SELECT sqlite_version() AS v")
      if (is.null(r)) NULL else paste("SQLite", as.character(r[[1L]][[1L]]))
    },
    duckdb = {
      r <- .sqlQueryOne(handle, "SELECT version() AS v")
      if (is.null(r)) NULL else
        paste("DuckDB", sub("^v", "", as.character(r[[1L]][[1L]])))
    },
    postgresql = {
      v <- .databaseServerVersion(handle)
      number <- if (is.null(v)) character(0) else
        regmatches(v, regexpr("[0-9]+(\\.[0-9]+)*", v))
      if (length(number)) paste("PostgreSQL", number) else NULL
    },
    mysql = {
      v <- .databaseServerVersion(handle)
      if (is.null(v)) NULL else if (grepl("mariadb", v, ignore.case = TRUE)) {
        paste("MariaDB", sub("^([0-9][0-9.]*).*$", "\\1",
                             sub("^.*?([0-9]+\\.[0-9]+\\.[0-9]+)-MariaDB.*$",
                                 "\\1", v, perl = TRUE)))
      } else {
        paste("MySQL", sub("^([0-9][0-9.]*).*$", "\\1", v))
      }
    },
    {
      v <- .databaseServerVersion(handle)
      if (is.null(v)) NULL else as.character(v)
    }
  ), error = function(e) NULL)
}

.sqlVersionNumber <- function(runtime) {
  if (is.null(runtime)) return(NULL)
  number <- regmatches(runtime, regexpr("[0-9]+(\\.[0-9]+)+", runtime))
  if (!length(number)) return(NULL)
  tryCatch(numeric_version(number), error = function(e) NULL)
}

# Execute the 1-PRECEDING frame (and ROW_NUMBER) once on the live connection.
.sqlProbeWindowFrames <- function(handle) {
  sql <- paste0(
    "SELECT x, ROW_NUMBER() OVER (ORDER BY x) AS rn, ",
    "MAX(x) OVER (ORDER BY x ROWS BETWEEN UNBOUNDED PRECEDING AND 1 ",
    "PRECEDING) AS prev_max FROM (SELECT 1 AS x UNION ALL SELECT 2 UNION ",
    "ALL SELECT 3) t ORDER BY x"
  )
  result <- tryCatch(.sqlQueryOne(handle, sql), error = function(e) NULL)
  !is.null(result) && nrow(result) == 3L &&
    identical(as.integer(result$rn), 1:3) &&
    identical(as.integer(result$prev_max), c(NA, 1L, 2L))
}

#' Dialect profile of a handle (family, materialisation, runtime facts)
#'
#' Static facts come from the family; with \code{probe = TRUE} and a live
#' connection the runtime version and driver are read and, on SQLite, the
#' window-frame feature is executed. A probed profile is cached on an
#' environment handle (field \code{sql_dialect_profile}).
#'
#' @param h Handle, context, profile or dialect name.
#' @param probe Logical; read runtime facts from the connection.
#' @return A \code{dsomop_sql_dialect} list: \code{family}, \code{engine},
#'   \code{target_dialect}, \code{temporary_materialization},
#'   \code{typed_sql}, \code{sha256_sql}, \code{window_frames} (NA on an
#'   unprobed SQLite), \code{analytic_baseline}, \code{runtime},
#'   \code{driver}, \code{verification}, \code{constructs}.
#' @keywords internal
.sqlDialectProfile <- function(h, probe = FALSE) {
  if (!is.logical(probe) || length(probe) != 1L || is.na(probe)) {
    stop("probe must be TRUE or FALSE.", call. = FALSE)
  }
  # A profile is final: it has no connection to probe.
  if (inherits(h, "dsomop_sql_dialect")) return(h)
  cached <- .sqlCachedProfile(h)
  if (!is.null(cached) && (!probe || isTRUE(cached$probed))) return(cached)
  resolved <- .sqlResolve(h)
  family <- resolved[["family"]]
  engine <- resolved[["engine"]]
  handle <- if ((is.environment(h) || is.list(h)) &&
                !inherits(h, "dsomop_sql_dialect")) {
    if (!is.null(h[["handle"]])) h[["handle"]] else h
  } else {
    NULL
  }
  probed <- isTRUE(probe) && !is.null(handle) && .sqlHasConnection(handle)
  runtime <- if (probed) .sqlRuntimeVersion(handle, family) else NULL
  if (identical(family, "mysql") && !is.null(runtime)) {
    engine <- if (startsWith(runtime, "MariaDB")) "mariadb" else "mysql"
  }
  version <- .sqlVersionNumber(runtime)
  # MySQL/MariaDB: the CTE and window-function floor of resource.R.
  analytic <- TRUE
  if (identical(family, "mysql") && !is.null(version)) {
    analytic <- version >= numeric_version(
      if (identical(engine, "mariadb")) "10.2.0" else "8.0.0"
    )
  }
  window_frames <- if (!analytic) {
    FALSE
  } else if (identical(family, "sqlite")) {
    if (probed) .sqlProbeWindowFrames(handle) else NA
  } else {
    TRUE
  }
  # PostgreSQL has a built-in sha256() from version 11.
  sha256 <- !identical(family, "sqlite")
  if (identical(family, "postgresql") && !is.null(version) &&
      version < numeric_version("11")) {
    sha256 <- FALSE
  }
  matrix <- .sqlHelperMatrix()
  temp_mode <- .databaseSupportProfile(engine)$temporary_materialization
  profile <- structure(list(
    family = family,
    engine = engine,
    target_dialect = unname(.SQL_TARGET_DIALECT_OF_FAMILY[[family]]),
    temporary_materialization = temp_mode,
    typed_sql = TRUE,
    sha256_sql = sha256,
    window_frames = window_frames,
    analytic_baseline = analytic,
    runtime = runtime,
    driver = if (probed) .sqlDriverLabel(handle) else NULL,
    verification = if (family %in% .SQL_EXECUTABLE_FAMILIES) "executable"
                   else "snapshot",
    constructs = matrix[, family],
    probed = probed
  ), class = "dsomop_sql_dialect")
  if (probed && is.environment(handle)) handle$sql_dialect_profile <- profile
  profile
}

#' The capability \code{dialect} block (design section 4.4)
#'
#' @param handle CDM handle (probed).
#' @return Named list with \code{family}, \code{temporary_materialization},
#'   \code{typed_sql}, \code{sha256_sql}, \code{window_frames},
#'   \code{runtime}.
#' @keywords internal
.sqlDialectCapability <- function(handle) {
  p <- .sqlDialectProfile(handle, probe = TRUE)
  list(
    family = p$family,
    temporary_materialization = p$temporary_materialization,
    typed_sql = isTRUE(p$typed_sql),
    sha256_sql = isTRUE(p$sha256_sql),
    window_frames = isTRUE(p$window_frames),
    runtime = p$runtime
  )
}
