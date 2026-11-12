# Typed compiler and publication records of the 2.10.3 recipe engine
# (design-2.10.3 sections 6.4 and 7.3). WS-0 frozen artefact.
#
# Three record types are constructed and validated here:
#   .fragment()             one compiled relation (a SELECT body without WITH)
#   .compiledOutput()       what an output executor returns
#   .publicationCandidate() one independently accessible patient component
#                           with its exact person lineage
# Every workstream builds these records through the constructors below and
# passes them through the validators; a record that does not validate is a
# structural failure, never a silent default. The validators re-check every
# nested payload (an S3 class tag is never trusted on its own) and
# .assertCompiledOutput() cross-checks each candidate against its fragment
# (B4 of the WS-0 review). Runtime roster uniqueness, row-to-person coverage
# and genuine support counting stay in WS-1's .publishPrepare().

.FRAGMENT_KINDS <- c("concept", "event", "interval", "anchor", "coverage",
                     "roster", "candidates", "stream", "result")
.FRAGMENT_GRAINS <- c("row", "person", "episode", "none")
.FRAGMENT_COLUMN_TYPES <- c("integer", "bigint", "numeric", "date", "text",
                            "boolean")
.FRAGMENT_COLUMN_ROLES <- c("person_key", "episode_key", "origin_identity",
                            "start", "end", "end_recorded", "supply",
                            "concept", "group", "era_no", "value", "unit",
                            "operator", "range", "visit", "analytic_key",
                            "measure", "private")
.FRAGMENT_AVAILABILITY <- c("available", "unavailable")
.CANDIDATE_KINDS <- c("frame", "staged")
.LINEAGE_MODES <- c("direct", "roster", "reference")
.RECORD_STATUS <- c("ok", "unavailable")
.OUTPUT_STATUS <- c("ok", "unavailable")
.ROSTER_KEY_SETS <- list(c("person_id", "cohort_row_id"), "rowId")
.PERSON_KEY_NAMES <- c("person_id", "subject_id")
.NAME_PATTERN <- "^[A-Za-z][A-Za-z0-9_]{0,63}$"

.typesFail <- function(what, ...) {
  stop(what, ": ", paste0(..., collapse = ""), call. = FALSE)
}

.isScalarString <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)

.assertEnum <- function(value, allowed, field, what) {
  if (!.isScalarString(value) || !value %in% allowed) {
    .typesFail(what, field, " must be one of ", paste(allowed, collapse = ", "),
               if (.isScalarString(value)) paste0(", got '", value, "'") else "")
  }
  value
}

#' Describe one typed column of a fragment
#'
#' @param type One of integer, bigint, numeric, date, text, boolean.
#' @param nullable Logical; may the column be NULL.
#' @param role Semantic role (see .FRAGMENT_COLUMN_ROLES).
#' @keywords internal
.fragmentColumn <- function(type, nullable = TRUE, role = "measure") {
  col <- structure(list(type = type, nullable = nullable, role = role),
                   class = "dsomop_fragment_column")
  .assertFragmentColumn(col, "<column>")
}

#' Validate one column payload (shared by construction and validation)
#' @keywords internal
.assertFragmentColumn <- function(col, name) {
  what <- paste0("fragment column '", name, "'")
  if (!inherits(col, "dsomop_fragment_column") || !is.list(col) ||
      !setequal(names(col), c("type", "nullable", "role"))) {
    .typesFail(what, "must be built with .fragmentColumn() (type, nullable, role)")
  }
  .assertEnum(col$type, .FRAGMENT_COLUMN_TYPES, "column type", what)
  .assertEnum(col$role, .FRAGMENT_COLUMN_ROLES, "column role", what)
  if (!is.logical(col$nullable) || length(col$nullable) != 1L || is.na(col$nullable)) {
    .typesFail(what, "nullable must be TRUE or FALSE")
  }
  invisible(col)
}

#' Construct a compiled fragment (design section 6.4)
#'
#' @param name Fragment name (CTE name).
#' @param kind One of .FRAGMENT_KINDS.
#' @param grain One of .FRAGMENT_GRAINS.
#' @param select_body One SELECT statement without a leading WITH; it may
#'   reference other fragments by name.
#' @param columns Named list of .fragmentColumn() records.
#' @param dependencies Character vector of fragment names referenced.
#' @param unique_key Character vector of columns identifying a row.
#' @param person_lineage list(person_col = <column or NULL>,
#'   via_roster = <roster fragment name or NULL>).
#' @param roster_namespace The final roster whose cohort_row_id this
#'   fragment uses, or NULL.
#' @param source_identity_columns Columns carrying the origin identity.
#' @param private_columns Columns stripped at publication.
#' @param availability "available" or "unavailable".
#' @param ordering Deterministic ORDER BY columns, when a consumer needs
#'   order.
#' @keywords internal
.fragment <- function(name, kind, grain, select_body, columns,
                      dependencies = character(0), unique_key = character(0),
                      person_lineage = list(person_col = NULL, via_roster = NULL),
                      roster_namespace = NULL,
                      source_identity_columns = character(0),
                      private_columns = character(0),
                      availability = "available", ordering = character(0)) {
  f <- structure(list(
    name = name, kind = kind, grain = grain, select_body = select_body,
    dependencies = dependencies, columns = columns, unique_key = unique_key,
    person_lineage = person_lineage, roster_namespace = roster_namespace,
    source_identity_columns = source_identity_columns,
    private_columns = private_columns, availability = availability,
    ordering = ordering), class = "dsomop_fragment")
  .assertFragment(f)
}

#' Validate a fragment record
#' @return The fragment, invisibly.
#' @keywords internal
.assertFragment <- function(f) {
  what <- "fragment"
  if (!inherits(f, "dsomop_fragment")) .typesFail(what, "not a dsomop_fragment")
  if (!.isScalarString(f$name) || !grepl(.NAME_PATTERN, f$name)) {
    .typesFail(what, "name must match ", .NAME_PATTERN)
  }
  what <- paste0("fragment '", f$name, "'")
  .assertEnum(f$kind, .FRAGMENT_KINDS, "kind", what)
  .assertEnum(f$grain, .FRAGMENT_GRAINS, "grain", what)
  .assertEnum(f$availability, .FRAGMENT_AVAILABILITY, "availability", what)
  if (!.isScalarString(f$select_body)) .typesFail(what, "select_body must be one SQL string")
  if (grepl("^\\s*WITH\\b", f$select_body, ignore.case = TRUE, perl = TRUE)) {
    .typesFail(what, "select_body must not start with WITH (the statement builder hoists CTEs)")
  }
  if (!is.character(f$dependencies) || anyNA(f$dependencies) ||
      (length(f$dependencies) > 0L && !all(grepl(.NAME_PATTERN, f$dependencies)))) {
    .typesFail(what, "dependencies must be fragment names")
  }
  if (!is.list(f$columns) || length(f$columns) == 0L || is.null(names(f$columns)) ||
      any(!nzchar(names(f$columns))) || anyDuplicated(names(f$columns))) {
    .typesFail(what, "columns must be a non-empty uniquely named list")
  }
  for (nm in names(f$columns)) .assertFragmentColumn(f$columns[[nm]], nm)
  cols <- names(f$columns)
  roles <- vapply(f$columns, function(c) c$role, "")
  check_subset <- function(x, field) {
    if (!is.character(x) || anyNA(x) || !all(x %in% cols)) {
      .typesFail(what, field, " must name existing columns")
    }
  }
  check_subset(f$unique_key, "unique_key")
  check_subset(f$source_identity_columns, "source_identity_columns")
  check_subset(f$private_columns, "private_columns")
  check_subset(f$ordering, "ordering")
  pl <- f$person_lineage
  if (!is.list(pl) || !all(names(pl) %in% c("person_col", "via_roster")) ||
      (!is.null(pl$person_col) && !.isScalarString(pl$person_col)) ||
      (!is.null(pl$via_roster) && !.isScalarString(pl$via_roster))) {
    .typesFail(what, "person_lineage must be list(person_col, via_roster)")
  }
  if (!is.null(pl$person_col)) {
    if (!pl$person_col %in% cols) .typesFail(what, "person_lineage$person_col must be a column")
    if (roles[[pl$person_col]] != "person_key") {
      .typesFail(what, "person_lineage$person_col must carry the person_key role")
    }
  }
  if (!is.null(f$roster_namespace) && !.isScalarString(f$roster_namespace)) {
    .typesFail(what, "roster_namespace must be NULL or one population id")
  }
  if (f$kind %in% c("roster", "candidates", "stream", "coverage", "anchor") &&
      f$grain != "none" && is.null(pl$person_col) && is.null(pl$via_roster)) {
    .typesFail(what, "a ", f$kind, " fragment must declare its person lineage")
  }
  if (f$kind == "roster") {
    if (!"origin_identity" %in% roles || !"person_key" %in% roles) {
      .typesFail(what, "a roster must carry person_key and origin_identity columns")
    }
    if (length(f$unique_key) == 0L) .typesFail(what, "a roster must declare its unique_key")
  }
  if (f$grain == "none") {
    if (!is.null(pl$person_col) || !is.null(pl$via_roster)) {
      .typesFail(what, "a reference fragment (grain none) cannot carry person lineage")
    }
    if (any(roles %in% c("person_key", "episode_key"))) {
      .typesFail(what, "a reference fragment (grain none) cannot carry person or episode keys")
    }
  }
  if (f$grain != "none" && is.null(pl$person_col) && is.null(pl$via_roster) &&
      any(roles == "person_key")) {
    .typesFail(what, "a fragment with a person_key column must declare its person lineage")
  }
  if (any(roles == "private") && !all(cols[roles == "private"] %in% f$private_columns)) {
    .typesFail(what, "every column with the private role must be listed in private_columns")
  }
  invisible(f)
}

#' Construct a publication candidate (design section 7.3)
#'
#' @param output Output name.
#' @param component Component name (the output itself, a TAR, a survival
#'   component, ...).
#' @param kind "frame" (assigned symbol) or "staged" (file component).
#' @param lineage list(mode, key_columns, roster_id, roster_key_columns,
#'   roster_person_col). Rules: `direct` names one person column;
#'   `roster` names an origin-qualified participant/episode key
#'   (c("person_id", "cohort_row_id") or "rowId") resolved through the
#'   roster namespace `roster_id`; `reference` carries no keys. `set_id` is
#'   never person lineage in any mode.
#' @param private_columns Columns dropped at publication.
#' @param requested_symbol Symbol requested for a frame component.
#' @param requested_path Relative path requested for a staged component.
#' @param grain "person" or "episode" ("none" for a reference component).
#' @param status "ok" or "unavailable".
#' @keywords internal
.publicationCandidate <- function(output, component, kind, lineage,
                                  private_columns = character(0),
                                  requested_symbol = NULL,
                                  requested_path = NULL,
                                  grain = "episode", status = "ok") {
  pc <- structure(list(
    output = output, component = component, kind = kind, lineage = lineage,
    private_columns = private_columns, requested_symbol = requested_symbol,
    requested_path = requested_path, grain = grain, status = status),
    class = "dsomop_publication_candidate")
  .assertPublicationCandidate(pc)
}

#' Build a lineage declaration for a publication candidate
#' @keywords internal
.lineage <- function(mode, key_columns = NULL, roster_id = NULL,
                     roster_key_columns = NULL, roster_person_col = NULL) {
  list(mode = mode, key_columns = key_columns, roster_id = roster_id,
       roster_key_columns = roster_key_columns,
       roster_person_col = roster_person_col)
}

#' Validate a publication candidate
#' @return The candidate, invisibly.
#' @keywords internal
.assertPublicationCandidate <- function(pc) {
  what <- "publication candidate"
  if (!inherits(pc, "dsomop_publication_candidate")) .typesFail(what, "not a dsomop_publication_candidate")
  if (!.isScalarString(pc$output) || !grepl(.NAME_PATTERN, pc$output)) .typesFail(what, "output must be a name")
  if (!.isScalarString(pc$component) || !grepl(.NAME_PATTERN, pc$component)) .typesFail(what, "component must be a name")
  what <- paste0("publication candidate ", pc$output, "/", pc$component)
  .assertEnum(pc$kind, .CANDIDATE_KINDS, "kind", what)
  .assertEnum(pc$status, .RECORD_STATUS, "status", what)
  .assertEnum(pc$grain, c("person", "episode", "none"), "grain", what)
  if (!is.character(pc$private_columns) || anyNA(pc$private_columns)) {
    .typesFail(what, "private_columns must be a character vector")
  }
  if (pc$kind == "frame") {
    if (!.isScalarString(pc$requested_symbol) || !is.null(pc$requested_path)) {
      .typesFail(what, "a frame component requests exactly a symbol")
    }
  } else {
    if (!.isScalarString(pc$requested_path) || !is.null(pc$requested_symbol)) {
      .typesFail(what, "a staged component requests exactly a path")
    }
    if (startsWith(pc$requested_path, "/") || grepl("\\.\\.", pc$requested_path)) {
      .typesFail(what, "requested_path must be relative and must not climb")
    }
  }
  ln <- pc$lineage
  if (!is.list(ln) || !.isScalarString(ln$mode) ||
      !all(names(ln) %in% c("mode", "key_columns", "roster_id",
                            "roster_key_columns", "roster_person_col"))) {
    .typesFail(what, "lineage must be built with .lineage()")
  }
  .assertEnum(ln$mode, .LINEAGE_MODES, "lineage mode", what)
  keys <- ln$key_columns
  if ("set_id" %in% keys || "set_id" %in% ln$roster_key_columns) {
    .typesFail(what, "set_id is never person lineage")
  }
  if (ln$mode == "direct") {
    if (!.isScalarString(keys) || !keys %in% .PERSON_KEY_NAMES) {
      .typesFail(what, "direct lineage names one person column (",
                 paste(.PERSON_KEY_NAMES, collapse = " or "), ")")
    }
    if (!is.null(ln$roster_id) || !is.null(ln$roster_key_columns) || !is.null(ln$roster_person_col)) {
      .typesFail(what, "direct lineage carries no roster mapping")
    }
    if (pc$grain == "none") .typesFail(what, "a patient component cannot have grain none")
  } else if (ln$mode == "roster") {
    ok_keys <- any(vapply(.ROSTER_KEY_SETS, function(s) identical(as.character(keys), s), logical(1)))
    if (!ok_keys) {
      .typesFail(what, "roster lineage keys must be c(\"person_id\", \"cohort_row_id\") or \"rowId\"")
    }
    if (!.isScalarString(ln$roster_id) || !grepl(.NAME_PATTERN, ln$roster_id)) {
      .typesFail(what, "roster lineage must name its roster namespace")
    }
    if (!is.character(ln$roster_key_columns) || length(ln$roster_key_columns) != length(keys) ||
        anyNA(ln$roster_key_columns) || any(!nzchar(ln$roster_key_columns))) {
      .typesFail(what, "roster_key_columns must match key_columns one to one")
    }
    if (!.isScalarString(ln$roster_person_col)) {
      .typesFail(what, "roster lineage must name the roster's person column")
    }
    if (pc$grain == "none") .typesFail(what, "a patient component cannot have grain none")
  } else {
    if (!is.null(keys) || !is.null(ln$roster_id) || !is.null(ln$roster_key_columns) ||
        !is.null(ln$roster_person_col)) {
      .typesFail(what, "reference lineage carries no keys")
    }
    if (pc$grain != "none") .typesFail(what, "a reference component has grain none")
  }
  invisible(pc)
}

#' Cross-check a publication candidate against the fragment of its component
#' @keywords internal
.assertCandidateMatchesFragment <- function(pc, f, what) {
  cols <- names(f$columns)
  roles <- vapply(f$columns, function(c) c$role, "")
  ln <- pc$lineage
  if (ln$mode == "reference") {
    if (f$grain != "none") .typesFail(what, "reference component '", pc$component, "' has a fragment of grain ", f$grain)
    return(invisible(TRUE))
  }
  if (f$grain == "none") .typesFail(what, "patient component '", pc$component, "' has a reference fragment (grain none)")
  if (!identical(pc$grain, f$grain) && !(f$grain == "row" && pc$grain %in% c("person", "episode"))) {
    .typesFail(what, "candidate grain '", pc$grain, "' disagrees with fragment grain '", f$grain, "'")
  }
  missing <- setdiff(ln$key_columns, cols)
  if (length(missing) > 0L) {
    .typesFail(what, "candidate key column(s) ", paste(missing, collapse = ", "), " do not exist in the fragment")
  }
  if (ln$mode == "direct") {
    if (roles[[ln$key_columns]] != "person_key") {
      .typesFail(what, "direct key '", ln$key_columns, "' does not carry the person_key role")
    }
    if (is.null(f$person_lineage$person_col) || !identical(f$person_lineage$person_col, ln$key_columns)) {
      .typesFail(what, "direct key '", ln$key_columns, "' is not the fragment's declared person column")
    }
  } else {
    if (is.null(f$roster_namespace) || !identical(f$roster_namespace, ln$roster_id)) {
      .typesFail(what, "roster lineage names namespace '", ln$roster_id, "' but the fragment uses '",
                 f$roster_namespace %||% "<none>", "'")
    }
    pl <- f$person_lineage
    if (is.null(pl$person_col) && is.null(pl$via_roster)) {
      .typesFail(what, "roster lineage on a fragment that declares no person lineage")
    }
    if (!is.null(pl$via_roster) && !identical(pl$via_roster, ln$roster_id)) {
      .typesFail(what, "the fragment is mediated by roster '", pl$via_roster, "' but the candidate names '", ln$roster_id, "'")
    }
    if (!ln$roster_person_col %in% .PERSON_KEY_NAMES) {
      .typesFail(what, "roster_person_col must be a person key column (", paste(.PERSON_KEY_NAMES, collapse = " or "), ")")
    }
    if ("person_id" %in% ln$key_columns) {
      if (roles[["person_id"]] != "person_key") {
        .typesFail(what, "roster key 'person_id' does not carry the person_key role")
      }
      if (!is.null(pl$person_col) && !identical(pl$person_col, "person_id")) {
        .typesFail(what, "the fragment declares person lineage through '", pl$person_col, "' but the candidate keys person_id")
      }
    }
    if ("cohort_row_id" %in% ln$key_columns && roles[["cohort_row_id"]] != "episode_key") {
      .typesFail(what, "cohort_row_id must carry the episode_key role")
    }
    if (identical(as.character(ln$key_columns), "rowId")) {
      if (roles[["rowId"]] != "episode_key") .typesFail(what, "roster key 'rowId' must carry the episode_key role")
      if (is.null(pl$via_roster)) .typesFail(what, "a rowId-keyed component must be mediated by its roster (person_lineage$via_roster)")
      if (any(roles == "person_key") && is.null(pl$person_col)) {
        .typesFail(what, "the fragment carries a person_key column that its lineage does not declare")
      }
    }
    if (!is.null(pl$person_col) && roles[[pl$person_col]] != "person_key") {
      .typesFail(what, "the declared person column '", pl$person_col, "' does not carry the person_key role")
    }
  }
  missing_private <- setdiff(pc$private_columns, cols)
  if (length(missing_private) > 0L) {
    .typesFail(what, "candidate private column(s) ", paste(missing_private, collapse = ", "), " do not exist in the fragment")
  }
  if (!all(f$private_columns %in% pc$private_columns)) {
    .typesFail(what, "the fragment's private columns must all be dropped by the candidate")
  }
  invisible(TRUE)
}

#' Construct a compiled output (design section 6.4)
#'
#' @param output Output name.
#' @param components Named list; each element is list(statement = <SQL> or
#'   frame = <data.frame>, fragment = <dsomop_fragment>).
#' @param publication_candidates List of publication candidates, exactly one
#'   per component (reference components carry a reference-mode candidate).
#' @param reference_components Character vector of component names without
#'   person lineage.
#' @param status "ok" or "unavailable".
#' @keywords internal
.compiledOutput <- function(output, components, publication_candidates,
                            reference_components = character(0), status = "ok") {
  co <- structure(list(
    output = output, components = components,
    publication_candidates = publication_candidates,
    reference_components = reference_components, status = status),
    class = "dsomop_compiled_output")
  .assertCompiledOutput(co)
}

#' Validate a compiled output
#' @return The record, invisibly.
#' @keywords internal
.assertCompiledOutput <- function(co) {
  what <- "compiled output"
  if (!inherits(co, "dsomop_compiled_output")) .typesFail(what, "not a dsomop_compiled_output")
  if (!.isScalarString(co$output) || !grepl(.NAME_PATTERN, co$output)) .typesFail(what, "output must be a name")
  what <- paste0("compiled output '", co$output, "'")
  .assertEnum(co$status, .OUTPUT_STATUS, "status", what)
  comps <- co$components
  if (!is.list(comps) || length(comps) == 0L || is.null(names(comps)) ||
      any(!nzchar(names(comps))) || anyDuplicated(names(comps))) {
    .typesFail(what, "components must be a non-empty uniquely named list")
  }
  for (nm in names(comps)) {
    comp <- comps[[nm]]
    has_stmt <- .isScalarString(comp$statement)
    has_frame <- is.data.frame(comp$frame)
    if (!is.list(comp) || (has_stmt == has_frame)) {
      .typesFail(what, "component '", nm, "' must carry exactly one of statement or frame")
    }
    if (!inherits(comp$fragment, "dsomop_fragment")) {
      .typesFail(what, "component '", nm, "' must carry its fragment")
    }
    .assertFragment(comp$fragment)
  }
  refs <- co$reference_components
  if (!is.character(refs) || anyNA(refs) || !all(refs %in% names(comps))) {
    .typesFail(what, "reference_components must name existing components")
  }
  for (nm in refs) {
    f <- comps[[nm]]$fragment
    roles <- vapply(f$columns, function(c) c$role, "")
    if (f$grain != "none" || !is.null(f$person_lineage$person_col) || !is.null(f$person_lineage$via_roster) ||
        any(roles %in% c("person_key", "episode_key"))) {
      .typesFail(what, "reference component '", nm, "' carries person lineage or keys; it is a patient component")
    }
  }
  pcs <- co$publication_candidates
  if (!is.list(pcs)) .typesFail(what, "publication_candidates must be a list")
  covered <- character(0)
  for (pc in pcs) {
    .assertPublicationCandidate(pc)
    if (!identical(pc$output, co$output)) {
      .typesFail(what, "candidate for output '", pc$output, "' does not belong here")
    }
    if (!pc$component %in% names(comps)) {
      .typesFail(what, "candidate names unknown component '", pc$component, "'")
    }
    if (pc$lineage$mode == "reference" && !pc$component %in% refs) {
      .typesFail(what, "component '", pc$component, "' declares reference lineage but is not a reference component")
    }
    if (pc$lineage$mode != "reference" && pc$component %in% refs) {
      .typesFail(what, "reference component '", pc$component, "' cannot carry person lineage")
    }
    if (pc$component %in% covered) {
      .typesFail(what, "component '", pc$component, "' has more than one candidate")
    }
    .assertCandidateMatchesFragment(pc, comps[[pc$component]]$fragment, what)
    covered <- c(covered, pc$component)
  }
  uncovered <- setdiff(names(comps), covered)
  if (length(uncovered) > 0L) {
    .typesFail(what, "component(s) without a publication candidate: ",
               paste(uncovered, collapse = ", "), " (fail closed)")
  }
  invisible(co)
}
