# Module: Disclosure-Gated Data Manipulation (Assign)
#
# Client-driven data-manipulation verbs that operate on the token-keyed,
# disclosure-controlled data.frames produced by the dsOMOP assign methods
# (every such frame carries the `omop.table` class, stamped by
# .pseudonymizeIdentifiers). Each verb:
#   (a) admits ONLY omop.table inputs (.is_omop.table), so a client cannot
#       smuggle an arbitrary data.frame into the gated path;
#   (b) re-gates the RESULT on the number of DISTINCT person tokens via the
#       shared, fail-closed .assertMinPersons (it stop()s below
#       nfilter.subset and never returns a too-small frame);
#   (c) re-stamps the omop.table class additively and restores the
#       `dsomop_protected` / `omop_concept_cols` attributes that base-R
#       subsetting (`[`) and `merge()` drop.
#
# Gating on DISTINCT person tokens — never on row counts — is what defeats the
# rbind-doubling bypass: duplicating rows cannot raise the distinct-person
# count, so a 1-person frame stays a 1-person frame and is blocked.

#' The person/subject token column(s) present in a frame
#'
#' @param x A data.frame.
#' @return Character vector of person-key columns present (subset of
#'   \code{.PERSON_KEY_COLS()}).
#' @keywords internal
.omopPersonKeys <- function(x) intersect(.PERSON_KEY_COLS(), names(x))

#' Distinct-person token count used for gating
#'
#' The gate is always on \code{person_id} when present (the canonical person
#' token), falling back to \code{subject_id} (cohort frames key the person on
#' \code{subject_id}). Counting distinct tokens — not rows — is the property
#' that makes the rbind/union path non-bypassable.
#'
#' @param x A data.frame carrying a person/subject token column.
#' @return Integer count of distinct non-NA person tokens.
#' @keywords internal
.omopDistinctPersons <- function(x) {
  keys <- .omopPersonKeys(x)
  if (length(keys) == 0L) {
    stop("Disclosive: object has no person key; refusing to operate.",
         call. = FALSE)
  }
  key <- if ("person_id" %in% keys) "person_id" else keys[[1]]
  vals <- x[[key]]
  length(unique(vals[!is.na(vals)]))
}

#' Re-stamp the omop.table class and restore protected attributes
#'
#' \code{merge()} drops both the class and the \code{dsomop_protected} /
#' \code{omop_concept_cols} attributes; \code{[}-subsetting drops the
#' attributes. This restores them so a manipulated frame stays a fully tagged,
#' guard-carrying omop.table (the factor/level layer keys off
#' \code{dsomop_protected}).
#'
#' @param result The manipulated data.frame to tag.
#' @param ... One or more source omop.table frames whose protected/concept
#'   attributes should carry over (only columns still present in
#'   \code{result} are kept).
#' @return \code{result} with the omop.table class and restored attributes.
#' @keywords internal
.retagOmopTable <- function(result, ...) {
  sources <- list(...)
  protected <- character(0)
  concept_cols <- character(0)
  for (s in sources) {
    protected <- union(protected, attr(s, "dsomop_protected"))
    concept_cols <- union(concept_cols, attr(s, "omop_concept_cols"))
  }
  protected <- intersect(protected, names(result))
  # The person key is always protected, even if a source lost the attribute.
  protected <- union(protected, .omopPersonKeys(result))
  concept_cols <- intersect(concept_cols, names(result))

  if (length(protected) > 0L) {
    attr(result, "dsomop_protected") <- protected
  }
  if (length(concept_cols) > 0L) {
    attr(result, "omop_concept_cols") <- concept_cols
  }
  class(result) <- union("omop.table", class(result))
  result
}

#' Merge two omop.table frames on the person key (Assign)
#'
#' @description
#' Inner or left join of two server-side, token-keyed omop.table frames. Joining
#' is restricted to the person/subject token so a client cannot use \code{by}
#' to pivot on a protected identifier other than the person key. The joined
#' result is re-gated on its distinct-person token count (fail-closed) before it
#' can become a symbol.
#'
#' @param x,y Server-side omop.table data.frames (resolved from symbols by
#'   DataSHIELD).
#' @param by Character vector of join columns. Must be the person key
#'   (\code{"person_id"} / \code{"subject_id"}); may not include any other
#'   protected/identifier column.
#' @param type Character; \code{"inner"} (default) or \code{"left"}.
#' @return The joined omop.table (re-assigned to the caller's symbol).
#' @export
omopMergeDS <- function(x, y, by = "person_id", type = "inner") {
  if (!.is_omop.table(x) || !.is_omop.table(y)) {
    stop("omopMergeDS: both inputs must be dsOMOP tables (omop.table).",
         call. = FALSE)
  }
  by <- .ds_arg(by)
  type <- .ds_arg(type)
  type <- match.arg(as.character(type), c("inner", "left"))
  by <- as.character(by)
  for (b in by) .validateIdentifier(b, "join column")
  .omopAuditLog("omopMergeDS", list(by = by, type = type))

  if (length(by) == 0L) {
    stop("omopMergeDS: 'by' must name the person key.", call. = FALSE)
  }
  if (!all(by %in% names(x)) || !all(by %in% names(y))) {
    stop("omopMergeDS: every 'by' column must exist in both inputs.",
         call. = FALSE)
  }
  # The only legal join key is the person/subject token. Any other column that
  # is protected OR a known identifier would let a client correlate on a
  # quasi-identifier, so restrict `by` to the person key.
  person_keys <- .PERSON_KEY_COLS()
  if (!all(by %in% person_keys)) {
    stop("omopMergeDS: 'by' may only be the person key, not another ",
         "protected/identifier column.", call. = FALSE)
  }

  result <- merge(x, y, by = by, all.x = (type == "left"), all.y = FALSE)

  # Fail-closed re-gate on DISTINCT person tokens of the join result.
  .assertMinPersons(n_persons = .omopDistinctPersons(result))
  .retagOmopTable(result, x, y)
}

#' Filter the rows of an omop.table (Assign)
#'
#' @description
#' Applies a single comparison filter to a server-side omop.table and re-gates
#' the filtered result on its distinct-person token count (fail-closed): a
#' filter that narrows the population below the disclosure threshold errors and
#' never returns. Filtering on a protected/identifier column (the person token,
#' \code{subject_id}, or any \code{dsomop_protected} column) is rejected, so a
#' client cannot probe individual identities.
#'
#' @param x A server-side omop.table data.frame.
#' @param var Character; an existing, non-protected column to filter on.
#' @param op Character; one of \code{"=="}, \code{"!="}, \code{">"},
#'   \code{">="}, \code{"<"}, \code{"<="}, \code{"in"}, \code{"not_in"}.
#' @param value Comparison value(s). Scalar for the relational operators; a
#'   vector for \code{"in"} / \code{"not_in"}.
#' @return The filtered omop.table (re-assigned to the caller's symbol).
#' @export
omopFilterDS <- function(x, var, op, value) {
  if (!.is_omop.table(x)) {
    stop("omopFilterDS: input must be a dsOMOP table (omop.table).",
         call. = FALSE)
  }
  var <- .validateIdentifier(.ds_arg(var), "filter column")
  op <- .ds_arg(op)
  op <- match.arg(as.character(op),
                  c("==", "!=", ">", ">=", "<", "<=", "in", "not_in"))
  value <- .ds_arg(value)
  .omopAuditLog("omopFilterDS", list(var = var, op = op))

  if (!var %in% names(x)) {
    stop("omopFilterDS: column '", var, "' not found.", call. = FALSE)
  }
  protected <- union(attr(x, "dsomop_protected"), .PERSON_KEY_COLS())
  if (var %in% protected) {
    stop("omopFilterDS: cannot filter on a protected/identifier column ('",
         var, "').", call. = FALSE)
  }

  col <- x[[var]]
  # Coerce the comparison value to the column's type so a string-typed value
  # cannot silently coerce a numeric column to character (which would change
  # comparison semantics). Character/factor columns validate value length.
  value <- .omopCoerceFilterValue(col, value, var)

  keep <- switch(op,
    "=="     = col == value,
    "!="     = col != value,
    ">"      = col >  value,
    ">="     = col >= value,
    "<"      = col <  value,
    "<="     = col <= value,
    "in"     = col %in% value,
    "not_in" = !(col %in% value)
  )
  # NA comparisons are dropped (treated as non-matching), consistent with the
  # fail-closed "no hints" policy elsewhere in the disclosure layer.
  keep[is.na(keep)] <- FALSE
  result <- x[keep, , drop = FALSE]
  rownames(result) <- NULL

  # Fail-closed re-gate: a filter that isolates < nfilter.subset persons errors.
  .assertMinPersons(n_persons = .omopDistinctPersons(result))
  .retagOmopTable(result, x)
}

#' Coerce/validate a filter value against the target column type
#'
#' @param col The column vector being filtered.
#' @param value The user-supplied comparison value(s).
#' @param var Column name (for error messages).
#' @return The value coerced to a type compatible with \code{col}.
#' @keywords internal
.omopCoerceFilterValue <- function(col, value, var) {
  if (is.numeric(col)) {
    out <- suppressWarnings(as.numeric(unlist(value, use.names = FALSE)))
    if (any(is.na(out)) && !all(is.na(unlist(value)))) {
      stop("omopFilterDS: value for numeric column '", var,
           "' is not numeric.", call. = FALSE)
    }
    return(out)
  }
  if (is.logical(col)) {
    return(as.logical(unlist(value, use.names = FALSE)))
  }
  # Character/factor: validate each value against the string-length limit.
  vals <- as.character(unlist(value, use.names = FALSE))
  vapply(vals, .validateString, character(1), USE.NAMES = FALSE)
}

#' Keep a subset of columns of an omop.table (Assign)
#'
#' @description
#' Projects a server-side omop.table to \code{cols} (intersected with the
#' frame's columns), always retaining the person/subject token so the result
#' stays a valid, joinable omop.table. Selecting columns never lowers the
#' person count; the gate is re-run only for uniformity with the other verbs.
#'
#' @param x A server-side omop.table data.frame.
#' @param cols Character vector of columns to keep.
#' @return The projected omop.table (re-assigned to the caller's symbol).
#' @export
omopSelectDS <- function(x, cols) {
  if (!.is_omop.table(x)) {
    stop("omopSelectDS: input must be a dsOMOP table (omop.table).",
         call. = FALSE)
  }
  cols <- as.character(.ds_arg(cols))
  for (c in cols) .validateIdentifier(c, "column")
  .omopAuditLog("omopSelectDS", list(cols = cols))

  # Always retain the person key so the projection remains a joinable,
  # gate-able omop.table.
  keep <- union(.omopPersonKeys(x), intersect(cols, names(x)))
  if (length(keep) == 0L) {
    stop("omopSelectDS: no requested column exists.", call. = FALSE)
  }
  result <- x[, keep, drop = FALSE]

  # Defensive re-gate (projection cannot lower the person count, but keep the
  # gate so every verb fails closed identically).
  .assertMinPersons(n_persons = .omopDistinctPersons(result))
  .retagOmopTable(result, x)
}

#' Row-bind two schema-identical omop.table frames (Assign)
#'
#' @description
#' Stacks two server-side omop.table frames that share an identical set of
#' column names, then re-gates the result on its DISTINCT person token count.
#' Because the gate counts distinct persons (not rows), binding a frame to
#' itself — or any duplicate-row inflation — cannot raise the count, so the
#' classic rbind-doubling bypass of a per-row count gate is defeated here.
#'
#' @param x,y Server-side omop.table data.frames with identical column names.
#' @return The row-bound omop.table (re-assigned to the caller's symbol).
#' @export
omopBindRowsDS <- function(x, y) {
  if (!.is_omop.table(x) || !.is_omop.table(y)) {
    stop("omopBindRowsDS: both inputs must be dsOMOP tables (omop.table).",
         call. = FALSE)
  }
  .omopAuditLog("omopBindRowsDS", list(nx = nrow(x), ny = nrow(y)))

  # Safe simple option: require identical column NAMES (set-equal). Aligning
  # mismatched schemas would risk silently introducing NA columns that change
  # the person/identifier surface.
  if (!setequal(names(x), names(y))) {
    stop("omopBindRowsDS: inputs must have identical column names.",
         call. = FALSE)
  }
  # Both must carry the same person key so the bound frame has a single,
  # well-defined person token to gate on.
  if (!setequal(.omopPersonKeys(x), .omopPersonKeys(y))) {
    stop("omopBindRowsDS: inputs must share the same person key.",
         call. = FALSE)
  }
  # Align y's column order to x before binding.
  y <- y[, names(x), drop = FALSE]
  result <- rbind(x, y)
  rownames(result) <- NULL

  # Fail-closed re-gate on DISTINCT person tokens: this is the check that blocks
  # the doubling bypass (duplicates collapse to the same distinct count).
  .assertMinPersons(n_persons = .omopDistinctPersons(result))
  .retagOmopTable(result, x, y)
}
