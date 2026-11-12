# Module: Typed row predicates (design-2.10.3 sections 2.2, 4.1 `predicate`
# and 6.6)
#
# The language 2.0 predicate grammar -- and / or / not over leaves
# {var, op, value} with op in == != < <= > >= in not_in is_null not_null
# between -- compiles to SQL with native three-valued logic: a comparison with
# a NULL operand is UNKNOWN, NOT UNKNOWN is UNKNOWN, AND/OR are Kleene, and
# only the final WHERE drops UNKNOWN rows. `x IN (v...)` is TRUE when x equals
# a listed value, FALSE when x is non-NULL and equals none, UNKNOWN when x is
# NULL (list literals are never NULL); NOT IN is its negation. is_null and
# not_null are the only leaves that turn NULL into TRUE/FALSE. The R mirror is
# .evalFilterMask() (R/extraction.R), NA-preserving with the same table; the
# truth tables of both are pinned by test-predicate-truth-tables.R.
#
# A predicate is resolved against a column catalogue: the typed columns a
# fragment exposes to predicates, the raw column names that bind to them (the
# configured value column binds to the NORMALISED value, the unit column to
# the normalised unit, ...), and the names that are refused with a reason
# (identifiers, class-1 columns, roles the source does not have). Literals are
# typed against the column: dates take ISO strings, concept columns integers,
# numeric columns finite numbers, text columns strings in equality and IN
# predicates only.

.PREDICATE_COMPARISONS <- c("==", "!=", "<", "<=", ">", ">=")
.PREDICATE_OPS <- c(.PREDICATE_COMPARISONS, "in", "not_in", "is_null",
                    "not_null", "between")
.PREDICATE_SQL_OPS <- c(`==` = "=", `!=` = "<>", `<` = "<", `<=` = "<=",
                        `>` = ">", `>=` = ">=")

#' Build a predicate column catalogue
#'
#' @param columns Named list: predicate column -> list(type, concept = FALSE).
#' @param aliases Named character: raw name -> catalogue column it binds to.
#' @param refused Named character: name -> reason it cannot be used.
#' @param what Owner label used in rejections (e.g. "event 'hba1c'").
#' @return A \code{dsomop_predicate_catalogue}.
#' @keywords internal
.predicateCatalogue <- function(columns, aliases = character(0),
                                refused = character(0), what = "source") {
  structure(list(columns = columns, aliases = aliases, refused = refused,
                 what = what),
            class = "dsomop_predicate_catalogue")
}

#' Resolve one predicate variable against a catalogue
#'
#' @return list(column, type, concept) or a typed rejection.
#' @keywords internal
.predicateResolve <- function(catalogue, var, path) {
  if (!is.character(var) || length(var) != 1L || is.na(var) || !nzchar(var)) {
    .irReject("a predicate leaf needs one column name", field = path)
  }
  v <- tolower(var)
  if (v %in% names(catalogue$refused)) {
    .irReject(paste0("column '", var, "' cannot be used in a predicate of ",
                     catalogue$what, ": ", catalogue$refused[[v]]), field = path)
  }
  if (v %in% names(catalogue$aliases)) v <- catalogue$aliases[[v]]
  col <- catalogue$columns[[v]]
  if (is.null(col)) {
    .irReject(paste0("column '", var, "' is not a predicate column of ",
                     catalogue$what), field = path)
  }
  list(column = v, type = col$type, concept = isTRUE(col$concept))
}

#' Every variable a predicate references
#' @keywords internal
.predicateVars <- function(pred) {
  if (!is.list(pred) || is.null(names(pred))) return(character(0))
  if (!is.null(pred[["and"]])) return(unique(unlist(lapply(.jsonArrayItems(pred[["and"]]), .predicateVars))))
  if (!is.null(pred[["or"]])) return(unique(unlist(lapply(.jsonArrayItems(pred[["or"]]), .predicateVars))))
  if (!is.null(pred[["not"]])) return(.predicateVars(pred[["not"]]))
  v <- pred[["var"]]
  if (is.character(v) && length(v) == 1L) tolower(v) else character(0)
}

#' Shortest round-trip decimal literal of a finite number
#'
#' Never scientific notation; negative values are parenthesised so that
#' composition cannot form a comment (\code{a - (-1)}).
#' @keywords internal
.predicateNumericLiteral <- function(x, what = "numeric literal") {
  if (is.list(x) && length(x) == 1L) x <- x[[1L]]
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop(what, " must be one finite number.", call. = FALSE)
  }
  x <- as.numeric(x)
  if (x == trunc(x) && abs(x) <= 2^53) {
    s <- formatC(x, format = "f", digits = 0L)
  } else {
    s <- NULL
    for (d in 15:17) {
      cand <- trimws(format(x, digits = d, scientific = FALSE))
      if (identical(as.numeric(cand), x)) { s <- cand; break }
    }
    if (is.null(s)) s <- trimws(format(x, digits = 17, scientific = FALSE))
  }
  if (identical(s, "-0")) s <- "0"
  if (startsWith(s, "-")) paste0("(", s, ")") else s
}

# Typed SQL literal of one predicate operand.
.predicateLiteral <- function(h, value, col, op, var, path) {
  if (is.list(value) && length(value) == 1L && is.null(names(value))) value <- value[[1L]]
  bad <- function(reason) {
    .irReject(paste0("column '", var, "' ", reason), field = path)
  }
  if (is.null(value) || length(value) != 1L || (is.atomic(value) && is.na(value))) {
    bad("compares with a missing literal; use is_null / not_null")
  }
  type <- col$type
  if (identical(type, "date")) {
    if (!is.character(value)) bad("is a date column and compares with ISO dates only")
    ok <- tryCatch(.sqlDateLiteral(h, value), error = function(e) NULL)
    if (is.null(ok)) bad("is a date column and compares with valid ISO dates only")
    return(ok)
  }
  if (type %in% c("integer", "bigint", "numeric", "boolean")) {
    if (is.logical(value)) {
      if (identical(type, "numeric")) bad("is numeric and does not compare with booleans")
      return(if (isTRUE(value)) "1" else "0")
    }
    if (!is.numeric(value) || !is.finite(value)) {
      bad("is numeric and compares with finite numbers only")
    }
    if (isTRUE(col$concept) && value != trunc(value)) {
      bad("is a concept column and compares with integer concept ids only")
    }
    return(.predicateNumericLiteral(value, paste0("the literal of '", var, "'")))
  }
  if (identical(type, "text")) {
    if (!op %in% c("==", "!=", "in", "not_in")) {
      bad("is a text column: only equality and IN predicates apply")
    }
    if (!is.character(value)) bad("is a text column and compares with strings only")
    return(paste0("'", gsub("'", "''", enc2utf8(value), fixed = TRUE), "'"))
  }
  bad(paste0("has an unsupported predicate type '", type, "'"))
}

.predicateCaps <- function(state, depth, values, path) {
  tryCatch(.filterComplexityVisit(state, depth, values),
           error = function(e) .irReject(conditionMessage(e), field = path))
}

#' Compile a language 2.0 predicate to SQL (three-valued)
#'
#' @param h Handle, context, profile or dialect name.
#' @param pred Predicate tree (parsed JSON form).
#' @param catalogue A \code{.predicateCatalogue()}.
#' @param alias Table alias of the catalogue columns.
#' @param path JSON path for rejections.
#' @return SQL predicate (parenthesised).
#' @keywords internal
.predicateSql <- function(h, pred, catalogue, alias, path = "/where") {
  state <- NULL
  walk <- function(node, depth, p) {
    if (!is.list(node) || is.null(names(node)) || length(node) == 0L) {
      .irReject("a predicate node is an object", field = p)
    }
    keys <- names(node)
    group <- intersect(keys, c("and", "or", "not"))
    if (length(group)) {
      if (length(keys) != 1L) .irReject("a predicate group carries exactly one of and/or/not", field = p)
      state <<- .predicateCaps(state, depth, 0L, p)
      if (identical(group, "not")) {
        return(paste0("(NOT ", walk(node[["not"]], depth + 1L, paste0(p, "/not")), ")"))
      }
      items <- .jsonArrayItems(node[[group]])
      if (!length(items)) .irReject(paste0("an ", group, " group needs at least one predicate"), field = p)
      parts <- vapply(seq_along(items), function(i) {
        walk(items[[i]], depth + 1L, paste0(p, "/", group, "/", i - 1L))
      }, "")
      if (length(parts) == 1L) return(parts)
      return(paste0("(", paste(parts, collapse = if (identical(group, "and")) " AND " else " OR "), ")"))
    }
    unknown <- setdiff(keys, c("var", "op", "value"))
    if (length(unknown)) .irReject(paste0("unknown predicate field(s): ", paste(unknown, collapse = ", ")), field = p)
    op <- node[["op"]]
    if (!is.character(op) || length(op) != 1L || !op %in% .PREDICATE_OPS) {
      .irReject("unknown predicate operator", field = paste0(p, "/op"))
    }
    value <- node[["value"]]
    n_values <- if (op %in% c("in", "not_in")) length(.jsonArrayItems(value)) else if (op == "between") 2L else if (op %in% c("is_null", "not_null")) 0L else 1L
    state <<- .predicateCaps(state, depth, n_values, p)
    col <- .predicateResolve(catalogue, node[["var"]], paste0(p, "/var"))
    ref <- paste0(alias, ".", col$column)
    var <- node[["var"]]
    if (op %in% c("is_null", "not_null")) {
      if (!is.null(value)) .irReject("is_null / not_null take no value", field = paste0(p, "/value"))
      return(paste0("(", ref, if (op == "is_null") " IS NULL)" else " IS NOT NULL)"))
    }
    if (op %in% .PREDICATE_COMPARISONS) {
      if (is.list(value) && (length(value) != 1L || !is.null(names(value)))) {
        .irReject("a comparison takes one scalar", field = paste0(p, "/value"))
      }
      if (identical(col$type, "text") && !op %in% c("==", "!=")) {
        .irReject(paste0("column '", var, "' is a text column: only equality and IN predicates apply"),
                  field = p)
      }
      lit <- .predicateLiteral(h, value, col, op, var, paste0(p, "/value"))
      return(paste0("(", ref, " ", .PREDICATE_SQL_OPS[[op]], " ", lit, ")"))
    }
    if (op %in% c("in", "not_in")) {
      items <- .jsonArrayItems(value)
      if (!length(items)) .irReject("in / not_in need at least one value", field = paste0(p, "/value"))
      lits <- vapply(seq_along(items), function(i) {
        .predicateLiteral(h, items[[i]], col, op, var, paste0(p, "/value/", i - 1L))
      }, "")
      lits <- unique(lits)
      chunks <- split(lits, ceiling(seq_along(lits) / 1000L))
      parts <- vapply(chunks, function(chunk) {
        paste0(ref, if (op == "in") " IN (" else " NOT IN (", paste(chunk, collapse = ", "), ")")
      }, "")
      if (length(parts) == 1L) return(paste0("(", parts, ")"))
      return(paste0("(", paste(parts, collapse = if (op == "in") " OR " else " AND "), ")"))
    }
    # between
    if (!is.list(value) || is.null(names(value)) ||
        length(setdiff(names(value), c("lower", "upper", "upper_inclusive")))) {
      .irReject("between takes {lower, upper, upper_inclusive}", field = paste0(p, "/value"))
    }
    if (identical(col$type, "text")) {
      .irReject(paste0("column '", var, "' is a text column: only equality and IN predicates apply"), field = p)
    }
    lo <- value[["lower"]]; hi <- value[["upper"]]
    lo_sql <- .predicateLiteral(h, lo, col, op, var, paste0(p, "/value/lower"))
    hi_sql <- .predicateLiteral(h, hi, col, op, var, paste0(p, "/value/upper"))
    lo_v <- if (is.list(lo)) lo[[1L]] else lo
    hi_v <- if (is.list(hi)) hi[[1L]] else hi
    if ((is.numeric(lo_v) && is.numeric(hi_v) && lo_v > hi_v) ||
        (is.character(lo_v) && is.character(hi_v) && lo_v > hi_v)) {
      .irReject("between bounds must be ordered (lower <= upper)", field = paste0(p, "/value"))
    }
    inclusive <- value[["upper_inclusive"]]
    if (is.list(inclusive)) inclusive <- inclusive[[1L]]
    if (is.null(inclusive)) inclusive <- TRUE
    if (!is.logical(inclusive) || length(inclusive) != 1L || is.na(inclusive)) {
      .irReject("upper_inclusive must be TRUE or FALSE", field = paste0(p, "/value/upper_inclusive"))
    }
    paste0("(", ref, " >= ", lo_sql, " AND ", ref, if (inclusive) " <= " else " < ", hi_sql, ")")
  }
  walk(pred, 1L, path)
}

#' Evaluate a language 2.0 predicate in R over catalogue columns
#'
#' The R mirror of \code{.predicateSql()} for the same catalogue: variables
#' are bound through the catalogue (aliases, refusals) and the tree is
#' evaluated by the NA-preserving \code{.evalFilterMask()}.
#'
#' @param pred Predicate tree.
#' @param df Data frame whose columns are the catalogue's columns.
#' @param catalogue A \code{.predicateCatalogue()}.
#' @return Logical vector with NA for UNKNOWN.
#' @keywords internal
.predicateMask <- function(pred, df, catalogue = NULL) {
  bind <- function(node, p) {
    if (!is.list(node) || is.null(names(node))) return(node)
    if (!is.null(node[["and"]])) return(list(and = lapply(.jsonArrayItems(node[["and"]]), bind, p = p)))
    if (!is.null(node[["or"]])) return(list(or = lapply(.jsonArrayItems(node[["or"]]), bind, p = p)))
    if (!is.null(node[["not"]])) return(list(not = bind(node[["not"]], p)))
    if (!is.null(catalogue)) node[["var"]] <- .predicateResolve(catalogue, node[["var"]], p)$column
    node
  }
  .evalFilterMask(bind(pred, "/where"), df)
}

#' Kleene ANY over a window's predicate values (design sections 2.7, 6.6)
#'
#' TRUE if any row is TRUE, FALSE if every row is FALSE, UNKNOWN (NA) if no
#' row is TRUE and at least one is UNKNOWN; an empty window is 0.
#'
#' @param x Logical vector (NA = UNKNOWN) of the window's rows.
#' @return Integer 1, 0 or NA.
#' @keywords internal
.kleeneAny <- function(x) {
  if (!length(x)) return(0L)
  if (any(x %in% TRUE)) return(1L)
  if (all(x %in% FALSE)) return(0L)
  NA_integer_
}

#' Kleene ALL over a window's predicate values (design sections 2.7, 6.6)
#'
#' FALSE if any row is FALSE, TRUE if every row is TRUE, UNKNOWN (NA) if no
#' row is FALSE and at least one is UNKNOWN; an empty window is NA (a person
#' without rows is not "all normal": documented deviation from vacuous truth).
#'
#' @param x Logical vector (NA = UNKNOWN) of the window's rows.
#' @return Integer 1, 0 or NA.
#' @keywords internal
.kleeneAll <- function(x) {
  if (!length(x)) return(NA_integer_)
  if (any(x %in% FALSE)) return(0L)
  if (all(x %in% TRUE)) return(1L)
  NA_integer_
}
