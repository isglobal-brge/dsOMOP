# Module: language-2.0 standalone row verbs (design-2.10.3 sections 2.11, 4.6)
#
# omopMutateDS() evaluates the closed mutation AST of inst/schema/mutate-v1.json
# and omopFilterDS(expr =) the closed predicate grammar of plan-v2.json on a
# server-side omop.table. Neither ever evaluates R code: a request is parsed
# JSON (data), validated against the frozen schema, then type-checked against
# the frame's columns before any row is touched, and only the operators the
# schema lists are interpreted by the small evaluator below. Every result is a
# released patient-level object and goes through the standalone publication
# boundary (.publishStandalone(), design 7.3): its distinct-person support must
# reach nfilter.subset; intermediate values stay private. Person, episode and
# set keys and any dsomop_protected column can be neither read nor written,
# class-1 names are refused, and DP provenance is invalidated (design 7.5).

# --- column types --------------------------------------------------------------------

.verbColumnType <- function(v) {
  if (is.factor(v) || is.character(v)) return("text")
  if (inherits(v, "Date")) return("date")
  if (inherits(v, c("POSIXct", "POSIXlt"))) return("datetime")
  if (is.logical(v)) return("boolean")
  if (inherits(v, "integer64") || is.integer(v)) return("integer")
  if (is.numeric(v)) return("numeric")
  "unsupported"
}

# Columns no verb expression may read or write: person, episode and set keys
# and the frame's protected columns.
.verbProtectedColumns <- function(x) {
  unique(c(.PERSON_KEY_COLS(), .EPISODE_KEY_COLS(),
           as.character(attr(x, "dsomop_protected") %||% character(0))))
}

.verbReject <- function(caller, message, field = NULL) {
  .irReject(paste0(caller, ": ", message), field = field)
}

# Resolve one column reference against the typed environment.
.verbColumn <- function(name, types, protected, caller, path) {
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    .verbReject(caller, "a column reference needs one name", path)
  }
  if (name %in% protected) {
    .verbReject(caller, paste0("column '", name, "' is a protected identifier and cannot be ",
                               "used in an expression"), path)
  }
  type <- types[name]
  if (is.na(type)) {
    .verbReject(caller, paste0("column '", name, "' is not a column of the input or an earlier ",
                               "assignment"), path)
  }
  if (type %in% c("unsupported", "null")) {
    .verbReject(caller, paste0("column '", name, "' has no type an expression can use"), path)
  }
  unname(type)
}

# --- predicates (plan-v2 predicate grammar, three-valued) -------------------------------------

.verbScalar <- function(v) if (is.list(v) && length(v) == 1L && is.null(names(v))) v[[1L]] else v

# A literal compared with a column of `type` (the rules of .predicateLiteral()).
.verbCheckLiteral <- function(value, type, op, name, caller, path) {
  v <- .verbScalar(value)
  bad <- function(reason) .verbReject(caller, paste0("column '", name, "' ", reason), path)
  if (is.null(v) || length(v) != 1L || (is.atomic(v) && is.na(v))) {
    bad("compares with a missing literal; use is_null / not_null")
  }
  switch(type,
    date = , datetime = {
      if (!is.character(v)) bad("is a date column and compares with ISO dates only")
      ok <- tryCatch(.isoDate(v, "literal"), error = function(e) NULL)
      if (is.null(ok)) bad("is a date column and compares with valid ISO dates only")
    },
    numeric = {
      if (!is.numeric(v) || !is.finite(v)) bad("is numeric and compares with finite numbers only")
    },
    integer = , boolean = {
      if (!(is.logical(v) || (is.numeric(v) && is.finite(v)))) {
        bad(paste0("is ", type, " and compares with numbers or booleans only"))
      }
    },
    text = {
      if (!op %in% c("==", "!=", "in", "not_in")) {
        bad("is a text column: only equality and IN predicates apply")
      }
      if (!is.character(v)) bad("is a text column and compares with strings only")
      .validateString(v)
    },
    bad(paste0("has the unsupported type '", type, "'")))
  invisible(TRUE)
}

#' Type-check a predicate against a typed column environment
#' @keywords internal
.verbCheckPredicate <- function(pred, types, protected, caller, path = "/pred",
                                state = NULL, depth = 1L) {
  if (!is.list(pred) || is.null(names(pred)) || !length(pred)) {
    .verbReject(caller, "a predicate node is an object", path)
  }
  group <- intersect(names(pred), c("and", "or", "not"))
  if (length(group)) {
    state <- tryCatch(.filterComplexityVisit(state, depth, 0L),
                      error = function(e) .verbReject(caller, conditionMessage(e), path))
    if (identical(group, "not")) {
      return(.verbCheckPredicate(pred[["not"]], types, protected, caller,
                                 paste0(path, "/not"), state, depth + 1L))
    }
    items <- .jsonArrayItems(pred[[group]])
    for (i in seq_along(items)) {
      state <- .verbCheckPredicate(items[[i]], types, protected, caller,
                                   paste0(path, "/", group, "/", i - 1L), state, depth + 1L)
    }
    return(state)
  }
  op <- pred[["op"]]
  n_values <- if (op %in% c("in", "not_in")) length(.jsonArrayItems(pred[["value"]])) else
    if (identical(op, "between")) 2L else if (op %in% c("is_null", "not_null")) 0L else 1L
  state <- tryCatch(.filterComplexityVisit(state, depth, n_values),
                    error = function(e) .verbReject(caller, conditionMessage(e), path))
  name <- pred[["var"]]
  type <- .verbColumn(name, types, protected, caller, paste0(path, "/var"))
  if (op %in% c("is_null", "not_null")) return(state)
  if (op %in% c("in", "not_in")) {
    items <- .jsonArrayItems(pred[["value"]])
    for (i in seq_along(items)) {
      .verbCheckLiteral(items[[i]], type, op, name, caller, paste0(path, "/value/", i - 1L))
    }
    return(state)
  }
  if (identical(op, "between")) {
    if (identical(type, "text")) {
      .verbReject(caller, paste0("column '", name, "' is a text column: only equality and IN ",
                                 "predicates apply"), path)
    }
    lo <- .verbScalar(pred[["value"]][["lower"]])
    hi <- .verbScalar(pred[["value"]][["upper"]])
    .verbCheckLiteral(lo, type, op, name, caller, paste0(path, "/value/lower"))
    .verbCheckLiteral(hi, type, op, name, caller, paste0(path, "/value/upper"))
    if ((is.numeric(lo) && is.numeric(hi) && lo > hi) ||
        (is.character(lo) && is.character(hi) && lo > hi)) {
      .verbReject(caller, "between bounds must be ordered (lower <= upper)", paste0(path, "/value"))
    }
    return(state)
  }
  .verbCheckLiteral(pred[["value"]], type, op, name, caller, paste0(path, "/value"))
  state
}

#' Evaluate a type-checked predicate over a frame (three-valued: NA = UNKNOWN)
#'
#' The NA-preserving R mask of the predicate grammar, .evalFilterMask(), on a
#' copy of the referenced columns under private lower-case aliases (assigned
#' names may carry upper-case letters, amendment A-6).
#' @keywords internal
.verbPredicateMask <- function(pred, df) {
  refs <- unique(unlist(.verbPredicateRefs(pred)))
  aliases <- stats::setNames(paste0("v", seq_along(refs)), refs)
  frame <- as.data.frame(lapply(refs, function(r) df[[r]]), stringsAsFactors = FALSE,
                         col.names = unname(aliases), check.names = FALSE)
  if (!length(refs)) frame <- data.frame(row.names = seq_len(nrow(df)))
  rename <- function(node) {
    if (!is.list(node) || is.null(names(node))) return(node)
    if (!is.null(node[["and"]])) return(list(and = lapply(.jsonArrayItems(node[["and"]]), rename)))
    if (!is.null(node[["or"]])) return(list(or = lapply(.jsonArrayItems(node[["or"]]), rename)))
    if (!is.null(node[["not"]])) return(list(not = rename(node[["not"]])))
    node[["var"]] <- unname(aliases[[node[["var"]]]])
    node
  }
  mask <- .evalFilterMask(rename(pred), frame)
  if (length(mask) == 1L && nrow(df) != 1L) mask <- rep(mask, nrow(df))
  as.logical(mask)
}

# Exact (case-preserving) variables of a predicate.
.verbPredicateRefs <- function(pred) {
  if (!is.list(pred) || is.null(names(pred))) return(list())
  if (!is.null(pred[["and"]])) return(unlist(lapply(.jsonArrayItems(pred[["and"]]), .verbPredicateRefs), recursive = FALSE))
  if (!is.null(pred[["or"]])) return(unlist(lapply(.jsonArrayItems(pred[["or"]]), .verbPredicateRefs), recursive = FALSE))
  if (!is.null(pred[["not"]])) return(.verbPredicateRefs(pred[["not"]]))
  list(pred[["var"]])
}

# --- mutation expressions (mutate-v1.json) ---------------------------------------------------

# One result type for operands that must agree (ifelse/case_when branches,
# coalesce): an untyped null adapts; integer and numeric unify to numeric.
.mutateUnify <- function(types, caller, path, what) {
  t <- unique(setdiff(types, "null"))
  if (!length(t)) .verbReject(caller, paste0(what, " has no typed operand"), path)
  if (all(t %in% c("integer", "numeric"))) return(if ("numeric" %in% t) "numeric" else "integer")
  if (length(t) != 1L) {
    .verbReject(caller, paste0(what, " takes operands of one type (got ",
                               paste(sort(t), collapse = ", "), ")"), path)
  }
  t
}

# Operand and result types of one arithmetic node (design 4.6), or NULL when
# the operation is not defined: two numeric or integer operands (division and
# any numeric operand give numeric), date - date (integer days) and date +/-
# integer (a date). An untyped null operand takes the type that makes the
# operation defined.
.mutateOpTypes <- function(op, a, b) {
  num <- c("integer", "numeric")
  if (identical(a, "null") && identical(b, "null")) return(NULL)
  if (identical(a, "null")) a <- if (identical(b, "date") && identical(op, "-")) "date" else
    if (identical(b, "date")) "integer" else b
  if (identical(b, "null")) b <- if (identical(a, "date")) "integer" else a
  result <- if (a %in% num && b %in% num) {
    if (identical(op, "/") || "numeric" %in% c(a, b)) "numeric" else "integer"
  } else if (identical(a, "date") && identical(b, "date") && identical(op, "-")) {
    "integer"
  } else if (identical(a, "date") && identical(b, "integer") && op %in% c("+", "-")) {
    "date"
  } else if (identical(a, "integer") && identical(b, "date") && identical(op, "+")) {
    "date"
  } else NULL
  if (is.null(result)) return(NULL)
  list(a = a, b = b, result = result)
}

# Static type of an expression (design 4.6 type rules); refuses before any
# evaluation.
.mutateType <- function(expr, types, protected, caller, path, depth = 1L) {
  if (depth > .extractionCap("dsomop.max_filter_depth", 32L)) {
    .verbReject(caller, "the expression is nested deeper than the server max_filter_depth cap",
                path)
  }
  key <- names(expr)[[1L]]
  sub <- function(e, p) .mutateType(e, types, protected, caller, p, depth + 1L)
  switch(key,
    col = .verbColumn(expr[["col"]], types, protected, caller, paste0(path, "/col")),
    lit = {
      v <- expr[["lit"]]
      as <- expr[["as"]]
      if (is.null(as)) {
        if (is.null(v)) return("null")
        if (is.logical(v)) return("boolean")
        if (is.numeric(v)) return("numeric")
        .validateString(v)
        return("text")
      }
      if (!is.null(v)) {
        ok <- switch(as,
          numeric = is.numeric(v),
          integer = is.numeric(v) && v == trunc(v) && abs(v) <= .Machine$integer.max,
          text = is.character(v),
          boolean = is.logical(v),
          date = is.character(v) && !is.null(tryCatch(.isoDate(v, "literal"),
                                                      error = function(e) NULL)),
          FALSE)
        if (!isTRUE(ok)) {
          .verbReject(caller, paste0("the literal does not have its declared type '", as, "'"),
                      paste0(path, "/lit"))
        }
        if (identical(as, "text")) .validateString(v)
      }
      as
    },
    op = {
      args <- .jsonArrayItems(expr[["args"]])
      a <- sub(args[[1L]], paste0(path, "/args/0"))
      b <- sub(args[[2L]], paste0(path, "/args/1"))
      typed <- .mutateOpTypes(expr[["op"]], a, b)
      if (is.null(typed)) {
        .verbReject(caller, paste0("arithmetic '", expr[["op"]], "' needs two numeric or integer ",
                                   "operands (date - date gives days, date +/- integer a date); ",
                                   "got ", a, " and ", b), path)
      }
      typed$result
    },
    pred = {
      .verbCheckPredicate(expr[["pred"]], types, protected, caller, paste0(path, "/pred"))
      "boolean"
    },
    ifelse = {
      ie <- expr[["ifelse"]]
      .verbCheckPredicate(ie[["test"]], types, protected, caller, paste0(path, "/ifelse/test"))
      .mutateUnify(c(sub(ie[["yes"]], paste0(path, "/ifelse/yes")),
                     sub(ie[["no"]], paste0(path, "/ifelse/no"))),
                   caller, paste0(path, "/ifelse"), "ifelse")
    },
    case_when = {
      cw <- expr[["case_when"]]
      cases <- .jsonArrayItems(cw[["cases"]])
      out <- vapply(seq_along(cases), function(i) {
        p <- paste0(path, "/case_when/cases/", i - 1L)
        .verbCheckPredicate(cases[[i]][["when"]], types, protected, caller, paste0(p, "/when"))
        sub(cases[[i]][["then"]], paste0(p, "/then"))
      }, "")
      if (!is.null(cw[["else"]])) out <- c(out, sub(cw[["else"]], paste0(path, "/case_when/else")))
      .mutateUnify(out, caller, paste0(path, "/case_when"), "case_when")
    },
    coalesce = {
      items <- .jsonArrayItems(expr[["coalesce"]])
      .mutateUnify(vapply(seq_along(items), function(i) {
        sub(items[[i]], paste0(path, "/coalesce/", i - 1L))
      }, ""), caller, paste0(path, "/coalesce"), "coalesce")
    },
    cut = {
      ct <- expr[["cut"]]
      a <- sub(ct[["arg"]], paste0(path, "/cut/arg"))
      if (!a %in% c("integer", "numeric")) {
        .verbReject(caller, "cut needs a numeric argument", paste0(path, "/cut/arg"))
      }
      breaks <- vapply(.jsonArrayItems(ct[["breaks"]]), function(b) as.numeric(b), numeric(1))
      if (any(!is.finite(breaks)) || any(diff(breaks) <= 0)) {
        .verbReject(caller, "cut breaks must be strictly increasing finite numbers",
                    paste0(path, "/cut/breaks"))
      }
      labels <- ct[["labels"]]
      if (!is.null(labels)) {
        labels <- vapply(.jsonArrayItems(labels), as.character, "")
        if (length(labels) != length(breaks) - 1L || anyDuplicated(labels)) {
          .verbReject(caller, "cut labels must be unique, one per interval",
                      paste0(path, "/cut/labels"))
        }
        for (l in labels) .validateString(l)
      }
      "text"
    },
    .verbReject(caller, paste0("unknown expression '", key, "'"), path))
}

# R value of a typed scalar literal, recycled to n rows.
.mutateLiteral <- function(v, as, n) {
  if (is.null(v)) {
    proto <- switch(as %||% "null", numeric = NA_real_, integer = NA_integer_,
                    text = NA_character_, date = as.Date(NA), boolean = NA, NA)
    return(rep(proto, n))
  }
  val <- switch(as %||% "",
    numeric = as.numeric(v), integer = as.integer(v), text = as.character(v),
    boolean = as.logical(v), date = .isoDate(v, "literal"),
    if (is.logical(v)) v else if (is.numeric(v)) as.numeric(v) else as.character(v))
  rep(val, n)
}

# A value vector in the unified type of its expression.
.mutateCoerce <- function(x, type) {
  switch(type,
    numeric = { x <- suppressWarnings(as.numeric(x)); x },
    integer = if (inherits(x, "integer64")) x else suppressWarnings(as.integer(x)),
    text = as.character(x),
    boolean = as.logical(x),
    date = if (inherits(x, "Date")) x else as.Date(x),
    datetime = x,
    x)
}

#' Evaluate a type-checked mutation expression over a frame
#' @keywords internal
.mutateEval <- function(expr, df, types) {
  n <- nrow(df)
  key <- names(expr)[[1L]]
  sub <- function(e) .mutateEval(e, df, types)
  type_of <- function(e) .mutateType(e, types, character(0), "omopMutateDS", "")
  switch(key,
    col = df[[expr[["col"]]]],
    lit = .mutateLiteral(expr[["lit"]], expr[["as"]], n),
    op = {
      args <- .jsonArrayItems(expr[["args"]])
      op <- expr[["op"]]
      typed <- .mutateOpTypes(op, type_of(args[[1L]]), type_of(args[[2L]]))
      a <- sub(args[[1L]])
      b <- sub(args[[2L]])
      if (identical(typed$a, "date") && identical(typed$b, "date")) {
        a <- .mutateCoerce(a, "date")
        b <- .mutateCoerce(b, "date")
        return(as.integer(round(as.numeric(difftime(a, b, units = "days")))))
      }
      if (identical(typed$result, "date")) {
        first <- identical(typed$a, "date")
        d <- .mutateCoerce(if (first) a else b, "date")
        k <- suppressWarnings(as.integer(if (first) b else a))
        return(as.Date(if (identical(op, "-")) d - k else d + k))
      }
      a <- suppressWarnings(as.numeric(a))
      b <- suppressWarnings(as.numeric(b))
      out <- switch(op, "+" = a + b, "-" = a - b, "*" = a * b, "/" = a / b)
      # Division by zero or any non-finite result is NA (design 4.6).
      out[!is.finite(out)] <- NA
      if (identical(typed$result, "integer")) {
        # Outside the 32-bit integer range an integer result is not finite.
        out[!is.na(out) & abs(out) > .Machine$integer.max] <- NA
        out <- as.integer(out)
      }
      out
    },
    pred = .verbPredicateMask(expr[["pred"]], df),
    ifelse = {
      ie <- expr[["ifelse"]]
      type <- type_of(expr)
      test <- .verbPredicateMask(ie[["test"]], df)
      yes <- .mutateCoerce(sub(ie[["yes"]]), type)
      no <- .mutateCoerce(sub(ie[["no"]]), type)
      out <- yes
      pick <- test %in% FALSE
      out[pick] <- no[pick]
      # An UNKNOWN test yields NA (three-valued).
      out[is.na(test)] <- NA
      out
    },
    case_when = {
      cw <- expr[["case_when"]]
      type <- type_of(expr)
      cases <- .jsonArrayItems(cw[["cases"]])
      out <- .mutateCoerce(if (!is.null(cw[["else"]])) sub(cw[["else"]]) else
        .mutateLiteral(NULL, NULL, n), type)
      decided <- rep(FALSE, n)
      for (cs in cases) {
        test <- .verbPredicateMask(cs[["when"]], df)
        value <- .mutateCoerce(sub(cs[["then"]]), type)
        hit <- !decided & test %in% TRUE
        unknown <- !decided & is.na(test)
        out[hit] <- value[hit]
        # The first non-FALSE test decides: TRUE takes its value, UNKNOWN is NA.
        out[unknown] <- NA
        decided <- decided | hit | unknown
      }
      out
    },
    coalesce = {
      type <- type_of(expr)
      items <- .jsonArrayItems(expr[["coalesce"]])
      out <- .mutateCoerce(sub(items[[1L]]), type)
      for (it in items[-1L]) {
        fill <- is.na(out)
        if (!any(fill)) break
        value <- .mutateCoerce(sub(it), type)
        out[fill] <- value[fill]
      }
      out
    },
    cut = {
      ct <- expr[["cut"]]
      x <- as.numeric(sub(ct[["arg"]]))
      breaks <- vapply(.jsonArrayItems(ct[["breaks"]]), function(b) as.numeric(b), numeric(1))
      right <- isTRUE(.verbScalar(ct[["right"]]))
      labels <- if (is.null(ct[["labels"]])) NULL else
        vapply(.jsonArrayItems(ct[["labels"]]), as.character, "")
      out <- cut(x, breaks = breaks, labels = labels %||% NULL, right = right,
                 include.lowest = FALSE)
      as.character(out)
    })
}

#' Validate a mutation request: schema, references, types, protected writes
#'
#' @return The request's assignments with their result types.
#' @keywords internal
.mutateValidate <- function(req, x, caller = "omopMutateDS") {
  res <- .jsonSchemaValidate(.irSchema("mutate-v1"), req)
  if (!isTRUE(res$valid)) .irRejectAll(paste0(caller, ": invalid mutation request"), res$errors)
  protected <- .verbProtectedColumns(x)
  types <- vapply(x, .verbColumnType, "")
  types <- types[!names(types) %in% protected]
  out <- list()
  items <- .jsonArrayItems(req[["assign"]])
  for (i in seq_along(items)) {
    path <- paste0("/assign/", i - 1L)
    name <- items[[i]][["name"]]
    if (name %in% protected) {
      .verbReject(caller, paste0("'", name, "' is a protected identifier and cannot be written"),
                  paste0(path, "/name"))
    }
    if (isTRUE(.assignmentBlockedColumn(name))) {
      .verbReject(caller, paste0("'", name, "' is a blocked (class-1) column name; choose ",
                                 "another name"), paste0(path, "/name"))
    }
    type <- .mutateType(items[[i]][["expr"]], types, protected, caller, paste0(path, "/expr"))
    if (identical(type, "null")) {
      .verbReject(caller, paste0("'", name, "' has no type: give the null literal a type ",
                                 "(as = ...)"), paste0(path, "/expr"))
    }
    # Later expressions read the new column with its static type.
    types[[name]] <- type
    out[[i]] <- list(name = name, expr = items[[i]][["expr"]], type = type)
  }
  out
}

#' Derive columns of an omop.table from closed expressions (Assign)
#'
#' @description
#' Evaluates the closed mutation AST of \code{mutate-v1.json} (design section
#' 4.6) on a server-side \code{omop.table}: column references, typed literals,
#' arithmetic (division by zero or a non-finite result is \code{NA}), the
#' three-valued predicate grammar, \code{ifelse}/\code{case_when} (an UNKNOWN
#' test yields \code{NA}), \code{coalesce}, \code{cut} with public breaks, and
#' date arithmetic. The request is data: it is validated against the frozen
#' schema and type-checked against the frame before any row is evaluated, and
#' nothing in it is evaluated as R code. New columns may use earlier ones; an
#' existing non-protected column may be overwritten; person, episode and set
#' keys and protected columns are never read or written. The result is
#' published through the standalone publication boundary (its distinct-person
#' support must reach \code{nfilter.subset}) and carries no DP provenance.
#'
#' @param x A server-side omop.table data.frame.
#' @param exprs The mutation request (\code{list(version = 1, assign = ...)}),
#'   JSON-encoded by the client.
#' @return The mutated omop.table (assigned to the caller's symbol).
#' @export
omopMutateDS <- function(x, exprs) {
  if (!.is_omop.table(x)) {
    stop("omopMutateDS: input must be a dsOMOP table (omop.table).", call. = FALSE)
  }
  .omopTablePseudonymization(x, caller = "omopMutateDS")
  req <- .ds_arg(exprs)
  if (!is.list(req) || is.null(names(req))) {
    .verbReject("omopMutateDS", "the mutation request must be a JSON object")
  }
  assigns <- .mutateValidate(req, x)
  .omopAuditLog("omopMutateDS", list(columns = vapply(assigns, `[[`, "", "name")))
  result <- as.data.frame(x, stringsAsFactors = FALSE)
  attributes(result) <- attributes(x)[c("names", "row.names", "class")]
  class(result) <- "data.frame"
  types <- vapply(result, .verbColumnType, "")
  for (a in assigns) {
    value <- .mutateEval(a$expr, result, types)
    if (length(value) == 1L && nrow(result) != 1L) value <- rep(value, nrow(result))
    result[[a$name]] <- value
    types[[a$name]] <- a$type
  }
  rownames(result) <- NULL
  written <- unique(vapply(assigns, `[[`, "", "name"))
  result <- .retagOmopTable(result, x)
  # A rewritten column is no longer the concept column it may have been.
  concept_cols <- setdiff(attr(result, "omop_concept_cols") %||% character(0), written)
  attr(result, "omop_concept_cols") <- if (length(concept_cols)) concept_cols else NULL
  .publishStandalone(result, caller = "omopMutateDS", already_tokenized = TRUE)
}

#' Rows of an omop.table that satisfy a closed predicate
#'
#' The \code{expr} form of \code{\link{omopFilterDS}}: the plan-v2 predicate
#' grammar (design sections 2.11, 4.1), validated and type-checked before any
#' row is read, evaluated with three-valued logic; only rows where it is TRUE
#' are kept.
#' @keywords internal
.omopFilterExpr <- function(x, expr) {
  pred <- .ds_arg(expr)
  schema <- .irSchema("plan-v2")
  res <- .jsonSchemaValidate(schema[["$defs"]][["predicate"]], pred, root = schema)
  if (!isTRUE(res$valid)) .irRejectAll("omopFilterDS: invalid predicate", res$errors)
  protected <- .verbProtectedColumns(x)
  types <- vapply(x, .verbColumnType, "")
  types <- types[!names(types) %in% protected]
  .verbCheckPredicate(pred, types, protected, "omopFilterDS", "/expr")
  .omopAuditLog("omopFilterDS", list(expr = TRUE, columns = unique(unlist(.verbPredicateRefs(pred)))))
  frame <- as.data.frame(x, stringsAsFactors = FALSE)
  keep <- .verbPredicateMask(pred, frame) %in% TRUE
  result <- x[keep, , drop = FALSE]
  rownames(result) <- NULL
  result
}
