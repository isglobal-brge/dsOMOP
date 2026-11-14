# Generated from the existing mutate-v1 checker by tools/numbers/vendor_expression_types.py.
# Only admission adapters differ: all initial columns are declared private aliases.
.numberAstCheck <- local({
.verbReject <- function(caller,message,field=NULL) .numberReject(message,"number_expression_invalid")
.assignmentBlockedColumn <- function(name) name %in% c("person_id","subject_id","row_id","cohort_row_id","event_id")
.validateString <- function(x,short=FALSE) {if(nchar(x)>4096L) .numberReject("Expression string exceeds the existing bound.");x}
.verbScalar <- function(v) if(is.list(v)&&length(v)==1L&&is.null(names(v)))v[[1L]] else v
.isoDate <- function(x, label) {
  if (length(x) != 1L || is.na(x)) {
    stop(label, " must be one ISO date (YYYY-MM-DD).", call. = FALSE)
  }
  value <- as.character(x)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)) {
    stop(label, " must be an ISO date (YYYY-MM-DD).", call. = FALSE)
  }
  parsed <- suppressWarnings(as.Date(value, format = "%Y-%m-%d"))
  if (is.na(parsed) || format(parsed, "%Y-%m-%d") != value) {
    stop(label, " is not a valid date.", call. = FALSE)
  }
  parsed
}
.extractionCap <- function(option, default) {
  value <- suppressWarnings(as.numeric(getOption(
    option, getOption(paste0("default.", option), default)
  )))
  if (length(value) != 1L || is.na(value) || !is.finite(value) ||
      value != floor(value) || value < 1L) {
    stop(option, " must be one positive integer.", call. = FALSE)
  }
  value
}
.filterComplexityVisit <- function(state = NULL, depth = 1L,
                                   values = 0L) {
  if (is.null(state)) {
    state <- new.env(parent = emptyenv())
    state$nodes <- 0
    state$values <- 0
    state$max_depth <- .extractionCap("dsomop.max_filter_depth", 32L)
    state$max_nodes <- .extractionCap("dsomop.max_filter_nodes", 1024L)
    state$max_values <- .extractionCap("dsomop.max_filter_values", 10000L)
  }
  depth <- suppressWarnings(as.numeric(depth))
  values <- suppressWarnings(as.numeric(values))
  if (length(depth) != 1L || !is.finite(depth) || depth < 1L ||
      depth != floor(depth) || length(values) != 1L || !is.finite(values) ||
      values < 0L || values != floor(values)) {
    stop("Invalid filter complexity accounting state.", call. = FALSE)
  }
  state$nodes <- state$nodes + 1
  state$values <- state$values + values
  if (depth > state$max_depth) {
    stop("Filter tree exceeds the server max_filter_depth cap of ",
         state$max_depth, ".", call. = FALSE)
  }
  if (state$nodes > state$max_nodes) {
    stop("Filter tree exceeds the server max_filter_nodes cap of ",
         state$max_nodes, ".", call. = FALSE)
  }
  if (state$values > state$max_values) {
    stop("Filter tree exceeds the server max_filter_values cap of ",
         state$max_values, ".", call. = FALSE)
  }
  state
}
.verbColumn <- function(name, types, protected, caller, path) {
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    .verbReject(caller, "a column reference needs one name", path)
  }
  if (name %in% protected) {
    .verbReject(caller, paste0("column '", name, "' is a protected identifier and cannot be ",
                               "used in an expression"), path)
  }
  if (isTRUE(.assignmentBlockedColumn(name))) {
    .verbReject(caller, paste0("column '", name, "' is a blocked (class-1) source and cannot be ",
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

.mutateVariant <- function(expr) {
  keys <- intersect(names(expr), c("col", "lit", "op", "pred", "ifelse",
                                   "case_when", "coalesce", "cut"))
  if (length(keys) != 1L) .verbReject("omopMutateDS", "an expression needs exactly one variant")
  keys[[1L]]
}

.mutateType <- function(expr, types, protected, caller, path, depth = 1L) {
  if (depth > .extractionCap("dsomop.max_filter_depth", 32L)) {
    .verbReject(caller, "the expression is nested deeper than the server max_filter_depth cap",
                path)
  }
  key <- .mutateVariant(expr)
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
          integer = is.numeric(v) && is.finite(v) && v == trunc(v) &&
            v > -2^63 && v < 2^63,
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
        if (length(labels) != length(breaks) - 1L) {
          .verbReject(caller, "cut labels must have one per interval",
                      paste0(path, "/cut/labels"))
        }
        for (l in labels) .validateString(l)
      }
      "text"
    },
    .verbReject(caller, paste0("unknown expression '", key, "'"), path))
}

function(exprs,aliases,result) {
  types<-setNames(rep("numeric",length(aliases)),aliases)
  for(a in exprs$assign) types[[a$name]]<-.mutateType(a$expr,types,character(),"numeric expression","/exprs")
  if(!types[[result]] %in% c("numeric","integer")) .numberReject("Expression result must be numeric; use native bins/flags.","number_expression_invalid")
  invisible(types[[result]])
}
})
