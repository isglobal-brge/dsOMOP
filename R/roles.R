# Module: Role registry (design-2.10.3 section 6.2)
#
# One record per OMOP table and CDM version stating which physical column
# plays each semantic role (person, event key, start, end, domain concept,
# value, unit, operator, ranges, supply, visit links). The records ship as
# inst/ohdsi/roles-5.3.csv and roles-5.4.csv: an explicit per-table,
# per-version map of what the vendored OHDSI field metadata cannot express,
# checked offline against that metadata (test-roles.R). At run time a record is
# joined with the handle's blueprint (the columns actually present, their type,
# concept role and blocking flags), so a role whose column is absent at a site
# is NULL rather than guessed.
#
# Every 2.10.3 compiler reads roles through .roleOf(handle, table) instead of
# the .getDateColumn()/.getDatePair()/.eventPrimaryKeyColumn() heuristics; the
# executable index-event table allowlist is replaced by "has an event role
# record" (defect B-20).

.ROLE_VERSIONS <- c("5.3", "5.4")

.ROLE_CSV_COLUMNS <- c(
  "table_name", "event_source", "person_col", "event_key", "start_col",
  "end_col", "end_fallback", "domain_concept_col", "alt_domain_concept_col",
  "type_concept_col", "value_col", "value_concept_col", "unit_col",
  "operator_col", "range_low_col", "range_high_col", "quantity_col",
  "supply_col", "qualifier_col", "role_concept_cols", "visit_occurrence_col",
  "visit_detail_col", "notes"
)

# Roles that must exist for a table to be an event source at a site.
.ROLE_REQUIRED <- c("person", "event_key", "start")

.roleCache <- new.env(parent = emptyenv())

#' Packaged role records of one CDM version
#'
#' @param cdm_version CDM version label ("5.3", "5.4", "v5.3.1", ...).
#' @return A character data frame with one row per table (empty strings for
#'   absent roles), or NULL when no records ship for that version.
#' @keywords internal
.roleRecordsCsv <- function(cdm_version) {
  version <- .normalizeCdmVersion(cdm_version)
  if (is.null(version) || length(version) != 1L || !version %in% .ROLE_VERSIONS) {
    return(NULL)
  }
  key <- paste0("csv_", version)
  cached <- .roleCache[[key]]
  if (!is.null(cached)) return(cached)
  path <- system.file("ohdsi", paste0("roles-", version, ".csv"),
                      package = "dsOMOP")
  if (!nzchar(path)) {
    stop("Packaged role registry for CDM ", version, " is missing.",
         call. = FALSE)
  }
  records <- utils::read.csv(path, colClasses = "character",
                             na.strings = character(0),
                             stringsAsFactors = FALSE, check.names = FALSE)
  if (!identical(names(records), .ROLE_CSV_COLUMNS) ||
      anyDuplicated(records[["table_name"]]) ||
      !all(records[["event_source"]] %in% c("TRUE", "FALSE"))) {
    stop("Packaged role registry for CDM ", version, " is malformed.",
         call. = FALSE)
  }
  assign(key, records, envir = .roleCache)
  records
}

# Split "status:condition_status_concept_id;route:route_concept_id".
.roleConceptPairs <- function(x) {
  if (!nzchar(x)) return(stats::setNames(character(0), character(0)))
  parts <- strsplit(strsplit(x, ";", fixed = TRUE)[[1L]], ":", fixed = TRUE)
  stats::setNames(vapply(parts, `[[`, "", 2L), vapply(parts, `[[`, "", 1L))
}

#' Assignment class of a column (design section 7.6), for explain and lint
#'
#' Reporting only: class 1 (never released), 2 (quasi-identifier, permitted on
#' assign paths) or 3 (ordinary). Class 1 is the blueprint's blocking flag
#' minus the birth-component patterns; the assignment-policy predicate
#' \code{.assignmentBlockedColumn()} (WS-4) is used when it is installed so the
#' two can never disagree.
#'
#' @param column Column name.
#' @param col_df Optional blueprint column frame of the column's table.
#' @return Integer 1, 2 or 3.
#' @keywords internal
.roleColumnClass <- function(column, col_df = NULL) {
  column <- tolower(as.character(column))
  birth <- grepl("(^|_)(dob|date_of_birth|birth_date|birth_datetime)($|_)",
                 column) || grepl("^(year|month|day)_of_birth$", column)
  in_bp <- !is.null(col_df) && column %in% col_df[["column_name"]]
  predicate <- get0(".assignmentBlockedColumn", envir = asNamespace("dsOMOP"),
                    inherits = FALSE)
  blocked <- if (is.function(predicate)) {
    isTRUE(tryCatch(predicate(column, col_df), error = function(e) TRUE))
  } else if (in_bp) {
    isTRUE(col_df[["is_blocked"]][match(column, col_df[["column_name"]])]) && !birth
  } else {
    .detectSensitiveColumns(column) && !birth
  }
  if (blocked) return(1L)
  is_date <- if (in_bp) {
    isTRUE(col_df[["is_date"]][match(column, col_df[["column_name"]])])
  } else {
    grepl("_date$|_datetime$", column)
  }
  if (birth || is_date || column %in% c("age", "age_group", "birth_year_band")) {
    return(2L)
  }
  3L
}

# A cheap signature of the blueprint parts a role record depends on.
.roleBlueprintKey <- function(bp) {
  cols <- vapply(names(bp[["columns"]]), function(t) {
    paste(t, paste(bp[["columns"]][[t]][["column_name"]], collapse = ","), sep = ":")
  }, "")
  present <- bp[["tables"]][["table_name"]][bp[["tables"]][["present_in_db"]]]
  paste(.normalizeCdmVersion(bp[["spec_version"]]) %||% "",
        paste(sort(present), collapse = ","), paste(cols, collapse = ";"),
        sep = "|")
}

#' Build one run-time role record from a packaged row and the blueprint
#' @keywords internal
.roleBuildRecord <- function(row, version, bp) {
  table <- row[["table_name"]]
  col_df <- bp[["columns"]][[table]]
  present <- table %in% bp[["tables"]][["table_name"]][bp[["tables"]][["present_in_db"]]]
  if (!present || is.null(col_df) || nrow(col_df) == 0L) return(NULL)
  have <- col_df[["column_name"]]
  pick <- function(field) {
    value <- row[[field]]
    if (!nzchar(value) || !value %in% have) NULL else value
  }
  record <- list(
    table = table,
    cdm_version = version,
    person = pick("person_col"),
    event_key = pick("event_key"),
    start = pick("start_col"),
    end = pick("end_col"),
    end_fallback = if (nzchar(row[["end_fallback"]]) &&
                       row[["end_fallback"]] %in% have) row[["end_fallback"]],
    domain_concept = pick("domain_concept_col"),
    alt_domain_concept = pick("alt_domain_concept_col"),
    type_concept = pick("type_concept_col"),
    value = pick("value_col"),
    value_concept = pick("value_concept_col"),
    unit = pick("unit_col"),
    operator = pick("operator_col"),
    range_low = pick("range_low_col"),
    range_high = pick("range_high_col"),
    quantity = pick("quantity_col"),
    supply = pick("supply_col"),
    qualifier = pick("qualifier_col"),
    role_concepts = {
      pairs <- .roleConceptPairs(row[["role_concept_cols"]])
      as.list(pairs[pairs %in% have])
    },
    visit_occurrence = pick("visit_occurrence_col"),
    visit_detail = pick("visit_detail_col"),
    point_event = !nzchar(row[["end_col"]]),
    notes = row[["notes"]]
  )
  if (any(vapply(record[.ROLE_REQUIRED], is.null, logical(1)))) return(NULL)
  # The packaged role columns plus every concept and filterable column the
  # site actually has, with its type and assignment class (section 7.6).
  record[["columns"]] <- .roleColumnFrame(col_df, .filterableColumns(bp, table))
  structure(record, class = "dsomop_role_record")
}

#' Value type of a blueprint column
#'
#' From the CDM specification datatype when the blueprint has one (the
#' physical type otherwise): \code{"date"}, \code{"integer"} (concept ids
#' included), \code{"numeric"}, \code{"text"}, \code{"boolean"} or
#' \code{"unknown"} (no type information: literal typing is skipped).
#' @keywords internal
.roleValueType <- function(column, cdm_datatype = NULL, data_type = NULL) {
  pick <- function(x) if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) tolower(x) else ""
  t <- pick(cdm_datatype)
  if (!nzchar(t)) t <- pick(data_type)
  if (grepl("_date$|_datetime$", column) || grepl("^(date|datetime|timestamp)", t)) return("date")
  if (grepl("_concept_id$", column) || grepl("int", t)) return("integer")
  if (grepl("float|double|numeric|decimal|real|number", t)) return("numeric")
  if (grepl("char|text|string|clob", t)) return("text")
  if (grepl("bool", t)) return("boolean")
  "unknown"
}

#' Typed column frame of one blueprint table (role records, site checks)
#'
#' @param col_df The blueprint's column frame of the table.
#' @param filterable The table's filterable columns (\code{.filterableColumns()}).
#' @return data.frame(column_name, is_date, concept_role, is_concept,
#'   value_type, filterable, assignment_class).
#' @keywords internal
.roleColumnFrame <- function(col_df, filterable) {
  have <- as.character(col_df[["column_name"]])
  field <- function(name) {
    x <- col_df[[name]]
    if (is.null(x)) rep(NA_character_, length(have)) else as.character(x)
  }
  concept_role <- field("concept_role")
  cdm_type <- field("cdm_datatype")
  data_type <- field("db_datatype")
  data.frame(
    column_name = have,
    is_date = as.logical(col_df[["is_date"]] %||% grepl("_date$|_datetime$", have)),
    concept_role = concept_role,
    is_concept = grepl("_concept_id$", have) |
      (!is.na(concept_role) & nzchar(concept_role) & concept_role != "non_concept"),
    value_type = vapply(seq_along(have), function(i) {
      .roleValueType(have[[i]], cdm_type[[i]], data_type[[i]])
    }, ""),
    filterable = have %in% filterable,
    assignment_class = vapply(have, .roleColumnClass, integer(1), col_df = col_df),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' The role registry of a handle (built once per blueprint)
#'
#' @param handle CDM handle.
#' @return list(cdm_version, records = named list of role records (event
#'   sources present at the site), excluded = tables that are never event
#'   sources).
#' @keywords internal
.roleRegistry <- function(handle) {
  bp <- .buildBlueprint(handle)
  key <- .roleBlueprintKey(bp)
  cached <- if (is.environment(handle)) handle[["role_registry"]] else NULL
  if (!is.null(cached) && identical(cached[["blueprint_key"]], key)) return(cached)
  version <- .normalizeCdmVersion(bp[["spec_version"]])
  rows <- .roleRecordsCsv(version)
  records <- list()
  excluded <- character(0)
  if (!is.null(rows)) {
    for (i in seq_len(nrow(rows))) {
      row <- as.list(rows[i, , drop = FALSE])
      if (!identical(row[["event_source"]], "TRUE")) {
        excluded <- c(excluded, row[["table_name"]])
        next
      }
      record <- .roleBuildRecord(row, version, bp)
      if (!is.null(record)) records[[record[["table"]]]] <- record
    }
  }
  registry <- list(cdm_version = version, records = records,
                   excluded = excluded, blueprint_key = key)
  if (is.environment(handle)) handle[["role_registry"]] <- registry
  registry
}

#' Role record of one table (design section 6.2)
#'
#' @param handle CDM handle.
#' @param table Table name (case-insensitive).
#' @return The \code{dsomop_role_record} of an event-source table present at
#'   this site, or NULL (absent table, a table that is never an event source
#'   such as cost or episode_event, or a site table lacking a required role
#'   column).
#' @keywords internal
.roleOf <- function(handle, table) {
  if (!is.character(table) || length(table) != 1L || is.na(table) ||
      !nzchar(table)) {
    return(NULL)
  }
  .roleRegistry(handle)[["records"]][[tolower(table)]]
}

#' Why a table has, or lacks, a role record at this site
#'
#' @param handle CDM handle.
#' @param table Table name (case-insensitive).
#' @return \code{"available"} (a role record exists); \code{"absent"} (an
#'   event source of some CDM version that this site cannot serve: the table
#'   is missing, lacks a required role column, or belongs to another CDM
#'   version -- a site resource, \code{on_missing_table} applies); or
#'   \code{"not_event_source"} (a table that is never an event source, such as
#'   cost or episode_event, or an unknown table -- a plan error).
#' @keywords internal
.roleAvailability <- function(handle, table) {
  if (!is.null(.roleOf(handle, table))) return("available")
  if (!is.character(table) || length(table) != 1L || is.na(table)) {
    return("not_event_source")
  }
  t <- tolower(table)
  sources <- unlist(lapply(.ROLE_VERSIONS, function(v) {
    rows <- .roleRecordsCsv(v)
    rows[["table_name"]][rows[["event_source"]] == "TRUE"]
  }), use.names = FALSE)
  if (t %in% sources) "absent" else "not_event_source"
}

#' Public, count-free summary of a role record (explain payload)
#' @keywords internal
.roleRecordSummary <- function(record) {
  if (is.null(record)) return(NULL)
  roles <- c("person", "event_key", "start", "end", "end_fallback",
             "domain_concept", "alt_domain_concept", "type_concept", "value",
             "value_concept", "unit", "operator", "range_low", "range_high",
             "quantity", "supply", "qualifier", "visit_occurrence",
             "visit_detail")
  out <- list(table = record[["table"]], cdm_version = record[["cdm_version"]])
  for (r in roles) out[[r]] <- record[[r]]
  out[["role_concepts"]] <- record[["role_concepts"]]
  out[["point_event"]] <- record[["point_event"]]
  out
}
