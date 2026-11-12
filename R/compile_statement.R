# Module: Fragment registry and statement builder (design-2.10.3 section 6.4)
#
# Every 2.10.3 compiler returns typed fragments (R/types.R): one SELECT body
# without WITH, naming the fragments it reads. This module keeps the fragments
# of one plan execution in the execution context (`ctx$fragments`, an
# environment keyed by fragment name), gives them deterministic names
# (cs_<name>, ev_<name>, iv_<name>, ...), and assembles a statement: every
# dependency hoisted into one WITH chain in dependency order. Fragments are
# built through the typed SQL helpers of R/sql_expr.R, so the builder never
# translates SQL; the finished statement passes .sqlAssertTypedSql().

#' Deterministic fragment name within the 64-character name grammar
#'
#' \code{<prefix>_<name>[_<suffix>]}. A name that would exceed the grammar is
#' shortened deterministically: the stem keeps its leading characters and
#' gains the first ten hexadecimal digits of the SHA-256 of the full name, so
#' two distinct node names never share a fragment name.
#'
#' @param prefix Fragment prefix (e.g. \code{"ev"}, \code{"cs"}).
#' @param name Node name (plan grammar).
#' @param suffix Optional stage suffix (e.g. \code{"src"}, \code{"norm"}).
#' @return One fragment name.
#' @keywords internal
.fragmentName <- function(prefix, name, suffix = NULL) {
  if (!.isScalarString(prefix) || !.isScalarString(name)) {
    stop("A fragment name needs a prefix and a node name.", call. = FALSE)
  }
  full <- paste(c(prefix, name, suffix), collapse = "_")
  if (nchar(full) <= 64L && grepl(.NAME_PATTERN, full)) return(full)
  hash <- substr(paste(as.character(openssl::sha256(charToRaw(enc2utf8(full)))),
                       collapse = ""), 1L, 10L)
  tail <- paste(c(hash, suffix), collapse = "_")
  room <- 64L - nchar(prefix) - nchar(tail) - 2L
  stem <- gsub("[^A-Za-z0-9_]", "_", name)
  out <- paste0(prefix, "_", substr(stem, 1L, max(1L, room)), "_", tail)
  if (!grepl(.NAME_PATTERN, out)) {
    stop("Cannot derive a fragment name for '", name, "'.", call. = FALSE)
  }
  out
}

# The fragment store of an execution context (created on first use).
.fragmentStore <- function(ctx) {
  if (is.environment(ctx)) {
    if (is.null(ctx[["fragments"]]) || !is.environment(ctx[["fragments"]])) {
      assign("fragments", new.env(parent = emptyenv()), envir = ctx)
    }
    return(ctx[["fragments"]])
  }
  stop("Fragments are kept in an execution context environment.", call. = FALSE)
}

#' Register a compiled fragment in its execution context
#'
#' The fragment is validated (\code{.assertFragment}). Registering the same
#' name twice is allowed only for an identical fragment (memoised
#' compilation); a different fragment under an existing name is a compiler
#' defect and fails closed.
#'
#' @param ctx Execution context (environment).
#' @param f A \code{dsomop_fragment}.
#' @return The fragment, invisibly.
#' @keywords internal
.fragmentRegister <- function(ctx, f) {
  .assertFragment(f)
  store <- .fragmentStore(ctx)
  if (exists(f$name, envir = store, inherits = FALSE)) {
    old <- get(f$name, envir = store, inherits = FALSE)
    if (!identical(unclass(old), unclass(f))) {
      stop("Fragment name collision: '", f$name, "' already names another ",
           "fragment in this plan.", call. = FALSE)
    }
    return(invisible(old))
  }
  assign(f$name, f, envir = store)
  invisible(f)
}

#' A registered fragment by name (NULL when absent)
#' @keywords internal
.fragmentGet <- function(ctx, name) {
  if (!.isScalarString(name)) return(NULL)
  store <- if (is.environment(ctx) && !is.null(ctx[["fragments"]])) ctx[["fragments"]] else NULL
  if (is.null(store) || !exists(name, envir = store, inherits = FALSE)) return(NULL)
  get(name, envir = store, inherits = FALSE)
}

# Normalise the `fragments` argument of the builder to a lookup function.
.fragmentLookup <- function(fragments) {
  if (is.environment(fragments)) {
    store <- if (!is.null(fragments[["fragments"]]) && is.environment(fragments[["fragments"]])) {
      fragments[["fragments"]]
    } else fragments
    return(function(name) {
      if (exists(name, envir = store, inherits = FALSE)) get(name, envir = store, inherits = FALSE)
    })
  }
  if (inherits(fragments, "dsomop_fragment")) fragments <- list(fragments)
  if (is.list(fragments)) {
    by_name <- list()
    for (f in fragments) {
      if (!inherits(f, "dsomop_fragment")) {
        stop("The statement builder takes dsomop_fragment records.", call. = FALSE)
      }
      if (!is.null(by_name[[f$name]]) && !identical(unclass(by_name[[f$name]]), unclass(f))) {
        stop("Fragment name collision: '", f$name, "'.", call. = FALSE)
      }
      by_name[[f$name]] <- f
    }
    return(function(name) by_name[[name]])
  }
  stop("fragments must be a list of fragments or an execution context.", call. = FALSE)
}

#' The dependency closure of a fragment, in dependency order
#'
#' Depth-first over \code{dependencies} (each fragment's dependencies in their
#' declared order), so every fragment comes after the fragments it reads. A
#' missing dependency or a cycle fails closed.
#'
#' @param fragments List of fragments, or an execution context / store.
#' @param final The final fragment.
#' @return List of fragments (the dependencies of \code{final}, excluding it).
#' @keywords internal
.fragmentClosure <- function(fragments, final) {
  lookup <- .fragmentLookup(fragments)
  state <- new.env(parent = emptyenv())
  state$order <- list()
  state$seen <- character(0)
  visit <- function(name, stack) {
    if (name %in% stack) {
      stop("Fragment dependency cycle: ", paste(c(stack, name), collapse = " -> "),
           call. = FALSE)
    }
    if (name %in% state$seen) return(invisible(NULL))
    f <- lookup(name)
    if (is.null(f)) {
      stop("Fragment '", name, "' is referenced but was never compiled.", call. = FALSE)
    }
    .assertFragment(f)
    for (d in f$dependencies) visit(d, c(stack, name))
    state$seen <- c(state$seen, name)
    state$order[[length(state$order) + 1L]] <- f
    invisible(NULL)
  }
  .assertFragment(final)
  for (d in final$dependencies) visit(d, final$name)
  state$order
}

#' Assemble one statement from fragments (design section 6.4)
#'
#' Hoists every dependency of \code{final} into one \code{WITH} chain in
#' dependency order, each CTE named by its fragment, followed by the final
#' fragment's SELECT body. With \code{select} given, the final fragment is
#' itself hoisted and \code{select} (a SELECT reading it by name) becomes the
#' main query. The statement is checked with \code{.sqlAssertTypedSql()} when
#' a dialect is given.
#'
#' @param fragments List of fragments, or an execution context holding them.
#' @param final The final fragment (or the name of a registered one).
#' @param select Optional main SELECT reading \code{final} (and its
#'   dependencies) by name.
#' @param dialect Optional handle, context, profile or dialect name for the
#'   typed-SQL assertion.
#' @return One SQL statement.
#' @keywords internal
.buildStatement <- function(fragments, final, select = NULL, dialect = NULL) {
  if (is.character(final)) {
    name <- final
    final <- .fragmentLookup(fragments)(name)
    if (is.null(final)) stop("Unknown final fragment '", name, "'.", call. = FALSE)
  }
  .assertFragment(final)
  deps <- .fragmentClosure(fragments, final)
  main <- final$select_body
  if (!is.null(select)) {
    if (!.isScalarString(select)) stop("select must be one SQL SELECT.", call. = FALSE)
    deps <- c(deps, list(final))
    main <- select
  }
  sql <- if (length(deps)) {
    ctes <- vapply(deps, function(f) paste0(f$name, " AS (", f$select_body, ")"), "")
    paste0("WITH ", paste(ctes, collapse = ", "), " ", main)
  } else main
  if (!is.null(dialect)) .sqlAssertTypedSql(dialect, sql)
  sql
}

#' Dialect operand of an execution context
#'
#' The probed dialect profile of the context when present, otherwise its
#' handle (helpers resolve the family from either).
#' @keywords internal
.compileDialect <- function(ctx) {
  if (is.environment(ctx) || is.list(ctx)) {
    if (inherits(ctx[["dialect"]], "dsomop_sql_dialect")) return(ctx[["dialect"]])
    if (!is.null(ctx[["handle"]])) return(ctx[["handle"]])
  }
  stop("The execution context carries neither a dialect nor a handle.", call. = FALSE)
}

#' Per-context memo environment of one compiler
#' @keywords internal
.compileMemo <- function(ctx, slot) {
  if (!is.environment(ctx)) {
    stop("Compilers memoise in an execution context environment.", call. = FALSE)
  }
  if (is.null(ctx[[slot]]) || !is.environment(ctx[[slot]])) {
    assign(slot, new.env(parent = emptyenv()), envir = ctx)
  }
  ctx[[slot]]
}

#' Signal a typed compiler rejection
#'
#' A \code{dsomop_plan_rejection} with the status code of section 7.3 it is
#' transported under (\code{validation_error} unless stated) and a
#' \code{reason} naming the construct, e.g. \code{cost_currency_mixed} or
#' \code{adapter_person_mismatch} (section 11). The message names fields and
#' constructs only, never data values or counts.
#'
#' @param message Reason text.
#' @param reason Construct-level reason code.
#' @param field Optional JSON-pointer-like path of the offending field.
#' @param code Status code (section 7.3).
#' @param output Optional output (publication root).
#' @keywords internal
.compileReject <- function(message, reason = "validation_error", field = NULL,
                           code = "validation_error", output = NULL) {
  stop(structure(
    list(message = paste0(code, ": ", if (!identical(reason, code)) paste0(reason, ": "),
                          message),
         call = NULL, code = code, reason = reason, output = output, field = field,
         errors = NULL),
    class = c("dsomop_compile_rejection", "dsomop_ir_validation_error",
              "dsomop_plan_rejection", "error", "condition")
  ))
}

#' Typed NULL of a fragment column type
#'
#' SQLite stores everything dynamically and takes a bare NULL; other families
#' get a CAST so UNION branches and CASE expressions keep their types.
#'
#' @param h Handle, context, profile or dialect name.
#' @param type One of integer, bigint, numeric, date, text, boolean.
#' @return SQL expression.
#' @keywords internal
.compileTypedNull <- function(h, type) {
  family <- .sqlFamily(h)
  if (identical(type, "date")) return(.sqlNullDate(h))
  if (identical(family, "sqlite")) return("NULL")
  if (identical(type, "integer")) return(.sqlIntegerCast(h, "NULL"))
  if (identical(type, "bigint")) return(.sqlLosslessKey(h, "NULL"))
  sql_type <- switch(type,
    numeric = switch(family, postgresql = "NUMERIC", duckdb = "DOUBLE",
                     mysql = "DECIMAL(38,10)", sqlserver = "FLOAT",
                     oracle = "NUMBER", redshift = "DOUBLE PRECISION",
                     bigquery = "FLOAT64", snowflake = "DOUBLE",
                     spark = "DOUBLE"),
    text = switch(family, mysql = "CHAR(255)", oracle = "VARCHAR2(255)",
                  bigquery = "STRING", spark = "STRING", "VARCHAR(255)"),
    boolean = switch(family, mysql = "SIGNED", sqlserver = "BIT",
                     oracle = "NUMBER(1)", bigquery = "BOOL", "BOOLEAN"),
    stop("Unknown fragment column type '", type, "'.", call. = FALSE)
  )
  paste0("CAST(NULL AS ", sql_type, ")")
}

#' SQL text literal of a controller-owned token
#'
#' For names that already match the plan grammar (source namespaces, member
#' table names, domain ids); quotes are doubled.
#' @keywords internal
.compileTextLiteral <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("A text literal needs one string.", call. = FALSE)
  }
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

#' A FROM clause for a SELECT without a table
#'
#' Oracle and MySQL/MariaDB need \code{FROM DUAL} for a SELECT carrying a
#' WHERE clause; other families accept none.
#' @keywords internal
.compileNoTableFrom <- function(h) {
  if (.sqlFamily(h) %in% c("oracle", "mysql")) " FROM DUAL" else ""
}
