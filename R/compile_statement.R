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

# --- Fragment SQL identifiers (amendment A-17) ---------------------------------
#
# A fragment's identity is structured: (prefix, name, suffix), where suffix
# names a stage ("src", "norm", ...) and NULL marks the final fragment. SQL
# identifiers are allocated per execution context from these identities,
# never by concatenating strings at the call site (the concatenation collides:
# ("ev", "a", "src") and ("ev", "a_src") both read "ev_a_src"), so:
#   * every identity the plan can need is preallocated at once, in canonical
#     order (radix order of an unambiguous encoding), from the IR through the
#     identity generators of .fragmentIdentityGenerators(): the allocation is
#     deterministic for the plan and independent of compilation order;
#   * a readable name <prefix>_<name>[_<suffix>] is reserved when no other
#     identity of the plan reads the same under the dialect's identifier
#     comparison (case-folded), it fits .NAME_PATTERN and the dialect's
#     identifier limit (PostgreSQL 63 bytes, Oracle 30, ...), and it is not a
#     CDM table name a CTE could shadow;
#   * every other identity gets a readable stem plus the first ten
#     hexadecimal digits of SHA-256 over its encoding, then the first free
#     _2, _3, ... on a residual collision, the stem shortened so the hash and
#     the counter are never truncated;
#   * the completed allocation is checked again under the dialect equivalence
#     before any SQL is emitted. Distinct identities are never merged or
#     refused because their readable names collide; an identity no generator
#     declared is a compiler defect and fails closed.

#' A structured fragment identity
#'
#' @param prefix Fragment family (e.g. \code{"ev"} for events and adapters).
#' @param name Node name (plan grammar).
#' @param suffix Stage name, or NULL for the final fragment.
#' @return list(prefix, name, suffix).
#' @keywords internal
.fragmentIdentity <- function(prefix, name, suffix = NULL) {
  ok <- function(x) is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl("^[A-Za-z][A-Za-z0-9_]*$", x)
  if (!ok(prefix) || !ok(name) || (!is.null(suffix) && !ok(suffix))) {
    stop("A fragment identity is (prefix, name, suffix) in the plan name grammar.",
         call. = FALSE)
  }
  list(prefix = prefix, name = name, suffix = suffix)
}

# Unambiguous encoding of an identity (canonical JSON of the triple).
.fragmentIdentityKey <- function(id) {
  .jcsCanonical(list(id$prefix, id$name, id$suffix))
}

#' Identity generators of the fragment families (amendment A-17)
#'
#' One function per prefix, mapping an IR to the identities its compiler can
#' register for that plan. WS-4: \code{ev} (table events: stages
#' \code{src}, \code{norm} and the final fragment; adapters: \code{src} and
#' the final fragment) and \code{map} (vocabulary maps). WS-5: \code{iv}
#' (intervals), \code{op} / \code{opc} / \code{seg} (coverage) and
#' \code{anc} (anchors). Later compilers add their families here (e.g.
#' populations, output rosters); a fragment family without a generator cannot
#' be named.
#'
#' @return Named list of functions(ir) -> list of identities.
#' @keywords internal
.fragmentIdentityGenerators <- function() {
  list(ev = .eventFragmentIdentities, map = .mapFragmentIdentities,
       iv = .intervalFragmentIdentities, coverage = .coverageFragmentIdentities,
       anc = .anchorFragmentIdentities)
}

#' Identities of the event and adapter fragments of a plan
#' @keywords internal
.eventFragmentIdentities <- function(ir) {
  out <- list()
  for (e in names(ir[["events"]] %||% list())) {
    node <- ir[["events"]][[e]]
    adapter <- !is.null(node[["episode_members"]]) || !is.null(node[["cost"]])
    for (stage in if (adapter) "src" else c("src", "norm")) {
      out[[length(out) + 1L]] <- .fragmentIdentity("ev", e, stage)
    }
    out[[length(out) + 1L]] <- .fragmentIdentity("ev", e)
  }
  out
}

#' Identities of the vocabulary-map fragments (the RxNorm ingredient map)
#' @keywords internal
.mapFragmentIdentities <- function(ir) {
  list(.fragmentIdentity("map", "ingredient"))
}

#' Identifier length limit of a SQL family
#'
#' The dialect's limit on unquoted identifiers, capped by the 64-character
#' plan name grammar every fragment name also satisfies.
#' @keywords internal
.fragmentIdentifierLimit <- function(family) {
  limit <- switch(family,
    postgresql = 63L, redshift = 127L, oracle = 30L, mysql = 64L,
    sqlserver = 128L, bigquery = 300L, snowflake = 255L, spark = 128L,
    sqlite = 64L, duckdb = 64L, 64L)
  min(limit, 64L)
}

# Names a CTE must never take (a CTE named like a base table would shadow it
# inside the statement): every CDM/vocabulary table and the results cohort
# tables, case-folded.
.fragmentReservedNames <- function() {
  cached <- .irCache[["fragment_reserved_names"]]
  if (!is.null(cached)) return(cached)
  path <- system.file("ohdsi", "cdm_columns.csv", package = "dsOMOP")
  tables <- if (nzchar(path)) {
    unique(utils::read.csv(path, colClasses = "character")$table_name)
  } else character(0)
  out <- tolower(unique(c(tables, "cohort", "cohort_definition", "cohort_attribute")))
  assign("fragment_reserved_names", out, envir = .irCache)
  out
}

#' Allocate SQL identifiers for a set of fragment identities
#'
#' Pure and deterministic: the result depends only on the set of identities
#' and the SQL family (see the module notes above, amendment A-17).
#'
#' @param identities List of \code{.fragmentIdentity()} records.
#' @param family SQL family (\code{.sqlFamily()}).
#' @return Named character vector: identity key -> allocated identifier.
#' @keywords internal
.fragmentAllocate <- function(identities, family) {
  if (!length(identities)) return(stats::setNames(character(0), character(0)))
  keys <- vapply(identities, .fragmentIdentityKey, "")
  keep <- !duplicated(keys)
  identities <- identities[keep]
  keys <- keys[keep]
  ord <- order(keys, method = "radix")
  identities <- identities[ord]
  keys <- keys[ord]
  limit <- .fragmentIdentifierLimit(family)
  reserved <- .fragmentReservedNames()
  readable <- vapply(identities, function(id) {
    paste(c(id$prefix, id$name, id$suffix), collapse = "_")
  }, "")
  folded <- tolower(readable)
  clash <- duplicated(folded) | duplicated(folded, fromLast = TRUE)
  ok <- !clash & nchar(readable) <= limit & grepl(.NAME_PATTERN, readable) &
    !folded %in% reserved
  out <- stats::setNames(rep(NA_character_, length(keys)), keys)
  out[ok] <- readable[ok]
  used <- folded[ok]
  for (i in which(!ok)) {
    hash <- .fragmentHash(keys[[i]])
    stem <- gsub("[^A-Za-z0-9_]", "_", readable[[i]])
    k <- 1L
    repeat {
      tail <- paste0("_", hash, if (k > 1L) paste0("_", k))
      cand <- paste0(substr(stem, 1L, limit - nchar(tail)), tail)
      if (!tolower(cand) %in% c(used, reserved)) break
      k <- k + 1L
    }
    out[[i]] <- cand
    used <- c(used, tolower(cand))
  }
  # The completed allocation, checked under the dialect's identifier comparison.
  if (anyNA(out) || anyDuplicated(tolower(out)) || any(nchar(out) > limit) ||
      !all(grepl(.NAME_PATTERN, out)) || any(tolower(out) %in% reserved)) {
    stop("Fragment identifier allocation failed its distinctness check.", call. = FALSE)
  }
  out
}

# First ten hexadecimal digits of SHA-256 over an identity's encoding.
.fragmentHash <- function(key) {
  substr(paste(as.character(openssl::sha256(charToRaw(enc2utf8(key)))), collapse = ""),
         1L, 10L)
}

# The allocation of one execution context (preallocated from its IR on first
# use, memoised).
.fragmentAllocation <- function(ctx) {
  if (!is.environment(ctx)) {
    stop("Fragment identifiers are allocated per execution context.", call. = FALSE)
  }
  alloc <- ctx[["fragment_allocation"]]
  if (!is.null(alloc)) return(alloc)
  ir <- ctx[["ir"]]
  identities <- unlist(lapply(.fragmentIdentityGenerators(), function(gen) gen(ir)),
                       recursive = FALSE)
  alloc <- .fragmentAllocate(identities, .sqlFamily(.compileDialect(ctx)))
  assign("fragment_allocation", alloc, envir = ctx)
  alloc
}

#' The SQL identifier of a fragment identity
#'
#' With an execution context (every compiler call), the identifier allocated
#' for \code{(prefix, name, suffix)} in that context (amendment A-17). Without
#' one, the context-free form for tests and documentation: the readable name
#' when it fits the name grammar, otherwise a stem plus the first ten
#' hexadecimal digits of the identity's hash.
#'
#' @param prefix Fragment family.
#' @param name Node name (plan grammar).
#' @param suffix Stage name, or NULL for the final fragment.
#' @param ctx Optional execution context.
#' @return One fragment identifier.
#' @keywords internal
.fragmentName <- function(prefix, name, suffix = NULL, ctx = NULL) {
  id <- .fragmentIdentity(prefix, name, suffix)
  key <- .fragmentIdentityKey(id)
  if (is.null(ctx)) return(unname(.fragmentAllocate(list(id), "sqlite")[[key]]))
  alloc <- .fragmentAllocation(ctx)
  hit <- alloc[key]
  if (length(hit) != 1L || is.na(hit)) {
    stop("Fragment identity ", key, " was not declared by an identity generator ",
         "(.fragmentIdentityGenerators()); it cannot be named.", call. = FALSE)
  }
  unname(hit)
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
#' a dialect is given. On actual MySQL, stockpile fragments carrying the
#' private \code{mysql_stockpile_source} attribute are read once into a
#' statement-local JSON payload and reconstructed with typed JSON_TABLE rows
#' inside a lateral query. This permits repeated consumers without reopening
#' a session temporary table; final ordering remains explicit in the outer
#' SELECT. Payloads never leave the database or become persistent state.
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
  sources <- c(deps, if (is.null(select)) list(final))
  if (any(vapply(sources, function(f) !is.null(attr(f, "mysql_stockpile_source", exact = TRUE)), logical(1)))) {
    sql <- .mysqlStockpileStatement(deps, main, final, source_final = is.null(select))
    if (!is.null(dialect)) .sqlAssertTypedSql(dialect, sql)
    return(sql)
  }
  sql <- if (length(deps)) {
    ctes <- vapply(deps, function(f) paste0(f$name, " AS (", f$select_body, ")"), "")
    paste0("WITH ", paste(ctes, collapse = ", "), " ", main)
  } else main
  if (!is.null(dialect)) .sqlAssertTypedSql(dialect, sql)
  sql
}

# Mark a bounded private stockpile table for MySQL's statement-local transport.
# The attribute is compiler metadata, outside the frozen fragment record. Only
# actual MySQL uses it: MariaDB shares the SQL family but supports temp reuse.
.mysqlStockpileSource <- function(fragment, table, dialect) {
  if (identical(.sqlDialectProfile(dialect, probe = TRUE)$engine, "mysql")) {
    types <- vapply(fragment$columns, function(column) column$type, "")
    if (!all(types %in% c("integer", "bigint", "date", "text", "boolean"))) {
      stop("A MySQL stockpile payload needs exact integer, date, text or boolean columns.", call. = FALSE)
    }
    attr(fragment, "mysql_stockpile_source") <- list(table = table)
  }
  fragment
}

# Mask quoted strings, comments and parenthesised expressions while preserving
# character offsets. This identifies top-level compiler SELECT/ORDER clauses
# without mistaking a window ORDER BY or a literal for the final row order.
.mysqlTopLevelSql <- function(sql, top_only = TRUE) {
  chars <- strsplit(sql, "", fixed = TRUE)[[1L]]
  out <- chars
  n <- length(chars)
  depth <- 0L
  quote <- NULL
  comment <- NULL
  i <- 1L
  while (i <= n) {
    ch <- chars[[i]]
    next_ch <- if (i < n) chars[[i + 1L]] else ""
    if (!is.null(comment)) {
      out[[i]] <- " "
      if (comment == "line" && ch == "\n") comment <- NULL
      if (identical(comment, "block") && ch == "*" && next_ch == "/") {
        out[[i + 1L]] <- " "; i <- i + 1L; comment <- NULL
      }
    } else if (!is.null(quote)) {
      out[[i]] <- " "
      if (ch == "\\" && i < n) {
        out[[i + 1L]] <- " "; i <- i + 1L
      } else if (ch == quote) {
        if (next_ch == quote) {
          out[[i + 1L]] <- " "; i <- i + 1L
        } else quote <- NULL
      }
    } else if (ch %in% c("'", '"', "`")) {
      quote <- ch; out[[i]] <- " "
    } else if ((ch == "-" && next_ch == "-") || ch == "#") {
      comment <- "line"; out[[i]] <- " "
    } else if (ch == "/" && next_ch == "*") {
      comment <- "block"; out[[i]] <- " "
    } else if (ch == "(") {
      depth <- depth + 1L; if (top_only) out[[i]] <- " "
    } else if (ch == ")") {
      depth <- depth - 1L; if (top_only) out[[i]] <- " "
    } else if (top_only && depth > 0L) out[[i]] <- " "
    i <- i + 1L
  }
  paste(out, collapse = "")
}

.mysqlLocalSqlName <- function(base, sql) {
  name <- base
  counter <- 2
  while (grepl(paste0("\\b", name, "\\b"), sql, ignore.case = TRUE, perl = TRUE)) {
    name <- paste0(substr(base, 1L, 48L), "_", format(counter, scientific = FALSE, trim = TRUE))
    counter <- counter + 1
  }
  name
}

# Substitute only original unqualified identifier tokens, right to left:
# literals/comments are unchanged and inserted expressions are never rescanned.
.mysqlExpandOrderAliases <- function(expr, expressions, qualified = FALSE) {
  mask <- .mysqlTopLevelSql(expr, top_only = FALSE)
  pattern <- if (qualified) "(?<![A-Za-z0-9_.])[A-Za-z][A-Za-z0-9_]*(?:\\.[A-Za-z][A-Za-z0-9_]*)*(?![A-Za-z0-9_.])" else
    "(?<![A-Za-z0-9_.])[A-Za-z][A-Za-z0-9_]*(?![A-Za-z0-9_.])"
  tokens <- gregexpr(pattern, mask, perl = TRUE)[[1L]]
  if (tokens[[1L]] < 0L) return(expr)
  sizes <- attr(tokens, "match.length")
  for (i in rev(seq_along(tokens))) {
    token <- substr(expr, tokens[[i]], tokens[[i]] + sizes[[i]] - 1L)
    tail <- substring(mask, tokens[[i]] + sizes[[i]])
    if (grepl("^\\s*\\(", tail, perl = TRUE)) next
    k <- match(tolower(token), tolower(names(expressions)))
    if (!is.na(k)) expr <- paste0(substr(expr, 1L, tokens[[i]] - 1L), "(", expressions[[k]], ")",
                                  substring(expr, tokens[[i]] + sizes[[i]]))
  }
  expr
}

.mysqlSqlList <- function(sql) {
  commas <- gregexpr(",", .mysqlTopLevelSql(sql), fixed = TRUE)[[1L]]
  if (commas[[1L]] < 0L) return(trimws(sql))
  starts <- c(1L, commas + 1L)
  ends <- c(commas - 1L, nchar(sql))
  trimws(substring(sql, starts, ends))
}

# Preserve final ordering outside the lateral query. Exposed expressions map
# to their output aliases; otherwise a private sort expression is projected
# inside and excluded from the outer SELECT. LIMIT stays inside, where the
# original ORDER BY continues to select the same subset of rows.
.mysqlStockpileOrder <- function(main, final, result_alias = "dsomop_result") {
  top <- .mysqlTopLevelSql(main)
  at <- regexpr("\\bORDER\\s+BY\\b", top, ignore.case = TRUE, perl = TRUE)
  out <- list(main = main, projection = paste0(result_alias, ".*"), order = "")
  if (at[[1L]] < 0L) return(out)
  from <- regexpr("\\bFROM\\b", top, ignore.case = TRUE, perl = TRUE)[[1L]]
  select <- regexpr("\\bSELECT\\s+(?:DISTINCT\\s+)?", top, ignore.case = TRUE, perl = TRUE)
  projection_start <- select[[1L]] + attr(select, "match.length")
  projection_end <- from - 1L
  projection <- .mysqlSqlList(substr(main, projection_start, projection_end))
  expressions <- character(0)
  star_references <- character(0)
  for (part in projection) {
    if (grepl("^(?:[A-Za-z][A-Za-z0-9_]*\\.)?\\*$", part, perl = TRUE)) {
      cols <- names(final$columns)
      qualifier <- if (identical(part, "*")) final$name else sub("\\.\\*$", "", part)
      refs <- paste0(qualifier, ".", cols)
      star_references <- unique(c(star_references, refs))
      expressions <- c(expressions, stats::setNames(if (identical(part, "*")) cols else refs, cols))
    } else {
      alias <- regexpr("\\s+AS\\s+([A-Za-z][A-Za-z0-9_]*)\\s*$", part, ignore.case = TRUE, perl = TRUE)
      if (alias[[1L]] > 0L) {
        name <- sub("^\\s+AS\\s+", "", regmatches(part, alias), ignore.case = TRUE)
        expressions[[trimws(name)]] <- trimws(substr(part, 1L, alias[[1L]] - 1L))
      } else {
        name <- sub("^.*\\.", "", part)
        expressions[[name]] <- part
      }
    }
  }
  rest <- substring(main, at[[1L]] + attr(at, "match.length"))
  limit <- regexpr("\\bLIMIT\\b", .mysqlTopLevelSql(rest), ignore.case = TRUE, perl = TRUE)[[1L]]
  order_sql <- if (limit > 0L) substr(rest, 1L, limit - 1L) else rest
  terms <- .mysqlSqlList(order_sql)
  added <- character(0)
  added_output <- character(0)
  input_alias <- .mysqlLocalSqlName("dsomop_order_input", main)
  output_map <- stats::setNames(paste0(input_alias, ".", names(expressions)), names(expressions))
  simple_sources <- grepl("^[A-Za-z][A-Za-z0-9_]*(\\.[A-Za-z][A-Za-z0-9_]*)?$", expressions)
  output_map <- c(output_map, stats::setNames(paste0(input_alias, ".", names(expressions)[simple_sources]),
                                             expressions[simple_sources]))
  order <- vapply(seq_along(terms), function(i) {
    term <- terms[[i]]
    direction <- regmatches(term, regexpr("\\s+(?:ASC|DESC)\\s*$", term, ignore.case = TRUE, perl = TRUE))
    expr <- trimws(sub("\\s+(ASC|DESC)\\s*$", "", term, ignore.case = TRUE))
    if (grepl("^[0-9]+$", expr)) return(paste0(expr, direction))
    matching <- names(expressions)[expressions == expr]
    alias_match <- match(tolower(expr), tolower(names(expressions)))
    if (!is.na(alias_match)) matching <- names(expressions)[[alias_match]]
    # A qualified reference to a star-projected column retains its name.
    simple <- sub("^[A-Za-z][A-Za-z0-9_]*\\.", "", expr)
    if (!length(matching) && expr %in% star_references &&
        grepl("^[A-Za-z][A-Za-z0-9_]*(\\.[A-Za-z][A-Za-z0-9_]*)?$", expr)) matching <- simple
    if (length(matching)) return(paste0(result_alias, ".", matching[[1L]], direction))
    name <- .mysqlLocalSqlName(paste0("dsomop_sort_", i), paste(c(main, names(added)), collapse = " "))
    # ORDER BY may refer to a SELECT alias; a sibling projection cannot, so
    # expand aliases to the source expression before adding the private key.
    added_output[[name]] <<- .mysqlExpandOrderAliases(expr, output_map, qualified = TRUE)
    added[[name]] <<- .mysqlExpandOrderAliases(expr, expressions)
    paste0(result_alias, ".", name, direction)
  }, "")
  if (length(added)) {
    distinct_or_union <- grepl("^\\s*SELECT\\s+DISTINCT\\b|\\bUNION\\b", top, ignore.case = TRUE, perl = TRUE)
    if (distinct_or_union) {
      # Compute expression sort keys after DISTINCT/UNION, so even a volatile
      # key cannot turn duplicate input rows into distinct output records.
      inner_order <- gsub(paste0(result_alias, "."), paste0(input_alias, "."), order, fixed = TRUE)
      for (name in names(added)) inner_order <- gsub(paste0(input_alias, ".", name), name, inner_order, fixed = TRUE)
      out$main <- paste0("SELECT ", input_alias, ".*, ",
                         paste(paste0(added_output, " AS ", names(added_output)), collapse = ", "),
                         " FROM (", substr(main, 1L, at[[1L]] - 1L), ") ", input_alias,
                         " ORDER BY ", paste(inner_order, collapse = ", "),
                         if (limit > 0L) paste0(" ", substring(rest, limit)))
    } else {
      if (sum(grepl("^(?:[A-Za-z][A-Za-z0-9_]*\\.)?\\*$", projection, perl = TRUE)) > 1L) {
        stop("Private MySQL sort keys need an explicit projection or one fragment star.", call. = FALSE)
      }
      out$main <- paste0(substr(main, 1L, projection_end), ", ",
                         paste(paste0(added, " AS ", names(added)), collapse = ", "),
                         " ", substring(main, from))
    }
    out$projection <- paste(paste0(result_alias, ".", names(expressions)), collapse = ", ")
  }
  out$order <- paste0(" ORDER BY ", paste(order, collapse = ", "))
  out
}

# MySQL 8.4 resolves a TEMPORARY table repeatedly through shared CTEs before
# optimizer materialization; DISTINCT, NO_MERGE and recursive wrappers do not
# avoid ERROR 1137. Capture each bounded stockpile once as a private JSON
# payload in the outer FROM, then expose typed rows through JSON_TABLE inside
# a correlated lateral statement. JSON never crosses the database boundary.
.mysqlStockpileStatement <- function(deps, main, final, source_final) {
  all <- c(deps, if (source_final) list(final))
  sources <- Filter(function(f) !is.null(attr(f, "mysql_stockpile_source", exact = TRUE)), all)
  tables <- unique(vapply(sources, function(f) attr(f, "mysql_stockpile_source")$table, ""))
  payloads <- character(0)
  replacements <- list()
  original_sql <- paste(c(main, vapply(all, function(f) paste(f$name, f$select_body), "")), collapse = " ")
  result_alias <- .mysqlLocalSqlName("dsomop_result", original_sql)
  sql_types <- c(integer = "INT", bigint = "BIGINT", date = "DATE",
                 text = "LONGTEXT", boolean = "BOOLEAN")
  for (k in seq_along(tables)) {
    table <- tables[[k]]
    fs <- Filter(function(f) identical(attr(f, "mysql_stockpile_source")$table, table), sources)
    f <- fs[[1L]]
    cols <- names(f$columns)
    if (any(!vapply(fs, function(source) identical(source$columns, f$columns), logical(1)))) {
      stop("Aliases of one MySQL stockpile table must share its column contract.", call. = FALSE)
    }
    alias <- .mysqlLocalSqlName(paste0("dsomop_payload_", k), original_sql)
    # JSON_ARRAYAGG returns SQL NULL for an empty input and can also return
    # NULL on error. Count in the same scan and reject a failed/truncated
    # payload instead of turning nonempty data into an empty stream. The
    # invalid-JSON branch is dynamic, and its message contains no row count.
    payloads <- c(payloads, paste0(
      "(SELECT CASE WHEN p.n = 0 THEN JSON_ARRAY() ",
      "WHEN p.raw_payload IS NULL OR JSON_LENGTH(p.raw_payload) <> p.n ",
      "THEN CAST(CONCAT('invalid_stockpile_payload', LEFT(CAST(p.n AS CHAR), 0)) AS JSON) ",
      "ELSE p.raw_payload END AS payload FROM ",
      "(SELECT JSON_ARRAYAGG(JSON_ARRAY(", paste(cols, collapse = ", "),
      ")) AS raw_payload, COUNT(*) AS n FROM ", table, ") p) ", alias))
    typed <- vapply(seq_along(cols), function(i) {
      paste0(cols[[i]], " ", sql_types[[f$columns[[cols[[i]]]]$type]], " PATH '$[", i - 1L,
             "]' ERROR ON EMPTY ERROR ON ERROR")
    }, "")
    body <- paste0("SELECT ", paste(paste0("j.", cols), collapse = ", "),
                    " FROM JSON_TABLE(", alias, ".payload, '$[*]' COLUMNS (",
                    paste(typed, collapse = ", "), ")) j")
    for (source in fs) replacements[[source$name]] <- body
  }
  for (i in seq_along(deps)) {
    if (!is.null(replacements[[deps[[i]]$name]])) deps[[i]]$select_body <- replacements[[deps[[i]]$name]]
  }
  if (source_final && !is.null(replacements[[final$name]])) main <- replacements[[final$name]]
  ordered <- .mysqlStockpileOrder(main, final, result_alias)
  inner <- if (length(deps)) paste0("WITH ", paste(vapply(deps, function(f) {
    paste0(f$name, " AS (", f$select_body, ")")
  }, ""), collapse = ", "), " ", ordered$main) else ordered$main
  paste0("SELECT ", ordered$projection, " FROM ", paste(payloads, collapse = " CROSS JOIN "),
         " JOIN LATERAL (", inner, ") ", result_alias, " ON TRUE", ordered$order)
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

#' The compilation context of a plan execution
#'
#' Compilers read the validated IR from \code{ctx$ir}, register fragments and
#' memos in the context and allocate identifiers per context. A publication
#' context (\code{.planContext()}) keeps the incoming plan as it was received,
#' so the compilers work in a child context bound to it: the handle and the
#' probed dialect of the execution, the normalised IR, its own fragment store.
#' Memoised in the execution context.
#'
#' @param ctx The execution context (\code{.planContext()}).
#' @param ir The validated IR (\code{.irPrepare()}).
#' @return The compilation context (environment).
#' @keywords internal
.compileContext <- function(ctx, ir) {
  existing <- ctx[["compile_context"]]
  if (is.environment(existing) && identical(existing[["ir"]], ir)) return(existing)
  cctx <- new.env(parent = emptyenv())
  cctx$handle <- ctx[["handle"]]
  cctx$ir <- ir
  cctx$dialect <- ctx[["dialect"]] %||% .sqlDialectProfile(ctx[["handle"]], probe = TRUE)
  cctx$execution <- ctx
  assign("compile_context", cctx, envir = ctx)
  cctx
}
