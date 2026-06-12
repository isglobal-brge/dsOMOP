# Module: Achilles Person-Gating
#
# Some Achilles analyses count RECORDS, not persons (e.g. analysis 401 =
# "Number of condition records, by condition_concept_id"). A record-count cell
# is disclosive when the number of DISTINCT PERSONS underlying that exact
# stratum is below nfilter.tab, even if the record count itself is large (e.g.
# 50 records all belonging to 1 person). .achillesGetResults() already drops
# cells whose count_value < nfilter.tab; person-gating is the orthogonal control
# that ADDITIONALLY drops record cells whose underlying PERSON count is below
# the threshold. This lets dsOMOP expose record-level volume safely instead of
# excluding it.
#
# Two gating strategies, chosen per analysis_id by .achilles_record_gate_spec():
#   * "sibling": the person count by the SAME concept_id already exists in
#     achilles_results as the sibling x00 analysis (e.g. 401 -> 400). Gate by
#     joining record rows to the sibling person rows on the concept stratum and
#     dropping record cells whose sibling person count_value < nfilter.tab. No
#     CDM query needed.
#   * "companion": no precomputed person sibling exists (monthly analyses, 505,
#     1818, 2102). Compute a targeted, cached COUNT(DISTINCT person_id) grouped
#     by the same strata directly from the CDM domain table, then drop record
#     cells where persons < nfilter.tab.

#' Gating specification for record-unit Achilles analyses
#'
#' Each entry describes one record-count analysis that must be person-gated.
#' \code{strategy} is "sibling" or "companion"; \code{sibling} is the x00
#' analysis_id holding the precomputed person count (sibling strategy only);
#' \code{join_strata} names the stratum columns that form the join key between
#' record rows and the person counts.
#'
#' @return Named list keyed by character analysis_id.
#' @keywords internal
.achilles_record_gate_spec <- function() {
  list(
    # Sibling-gated: x01 record-by-concept -> x00 person-by-concept,
    # joined on the concept stratum (stratum_1).
    "201"  = list(strategy = "sibling", sibling = 200L,  join_strata = "stratum_1"),
    "401"  = list(strategy = "sibling", sibling = 400L,  join_strata = "stratum_1"),
    "601"  = list(strategy = "sibling", sibling = 600L,  join_strata = "stratum_1"),
    "701"  = list(strategy = "sibling", sibling = 700L,  join_strata = "stratum_1"),
    "801"  = list(strategy = "sibling", sibling = 800L,  join_strata = "stratum_1"),
    "1801" = list(strategy = "sibling", sibling = 1800L, join_strata = "stratum_1"),
    "2101" = list(strategy = "sibling", sibling = 2100L, join_strata = "stratum_1"),
    # Companion-gated: no precomputed person sibling. Person counts come from
    # .achillesCompanionPersonCounts(), keyed by the same stratum columns.
    "505"  = list(strategy = "companion", join_strata = "stratum_1"),
    "220"  = list(strategy = "companion", join_strata = "stratum_1"),
    "420"  = list(strategy = "companion", join_strata = "stratum_1"),
    "620"  = list(strategy = "companion", join_strata = "stratum_1"),
    "720"  = list(strategy = "companion", join_strata = "stratum_1"),
    "820"  = list(strategy = "companion", join_strata = "stratum_1"),
    "1820" = list(strategy = "companion", join_strata = "stratum_1"),
    "2102" = list(strategy = "companion", join_strata = "stratum_1"),
    "1818" = list(strategy = "companion",
                  join_strata = c("stratum_1", "stratum_2", "stratum_3"))
  )
}

#' Record-unit analysis IDs that require person-gating
#'
#' @return Integer vector of analysis IDs.
#' @keywords internal
.achillesRecordGatedIds <- function() {
  as.integer(names(.achilles_record_gate_spec()))
}

#' Build a composite stratum join key
#'
#' Normalizes one or more stratum columns into a single comparable character
#' key so record rows and person-count rows can be matched. Missing strata
#' become the empty string; parts are "\\x1f"-joined (a delimiter that cannot
#' occur in a concept id or YYYYMM month) to avoid collisions between, e.g.,
#' ("1","23") and ("12","3").
#'
#' @param df Data frame containing the stratum columns.
#' @param cols Character vector of stratum column names.
#' @return Character vector, one composite key per row.
#' @keywords internal
.achillesStratumKey <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) return(rep("", nrow(df)))
  parts <- lapply(cols, function(col) {
    v <- as.character(df[[col]])
    v[is.na(v)] <- ""
    v
  })
  do.call(paste, c(parts, list(sep = "\x1f")))
}

#' Compute companion distinct-person counts for a record-count analysis
#'
#' For record-count analyses with NO precomputed person sibling (monthly
#' 220/420/620/720/820/1820/2102; death 505; measurement range 1818), compute a
#' targeted COUNT(DISTINCT person_id) grouped by the same gate strata directly
#' from the CDM domain table. Returned strata are emitted as text and built to
#' be join-compatible with the record analysis's stratum_1 (and stratum_2/3 for
#' 1818): concept ids and YYYYMM month keys match achilles_results.stratum_*
#' storage.
#'
#' @param handle CDM handle.
#' @param analysis_id Integer; a companion-gated analysis id.
#' @return data.frame with the join-stratum columns + \code{n_persons}; empty
#'   frame if not companion-gated or the domain table is absent.
#' @keywords internal
.achillesCompanionPersonCounts <- function(handle, analysis_id) {
  analysis_id <- as.integer(analysis_id)
  dialect <- handle$target_dialect

  companion_specs <- list(
    "220"  = list(table = "visit_occurrence",     kind = "month", date = "visit_start_date"),
    "420"  = list(table = "condition_occurrence", kind = "month", date = "condition_start_date"),
    "620"  = list(table = "procedure_occurrence", kind = "month", date = "procedure_date"),
    "720"  = list(table = "drug_exposure",        kind = "month", date = "drug_exposure_start_date"),
    "820"  = list(table = "observation",          kind = "month", date = "observation_date"),
    "1820" = list(table = "measurement",          kind = "month", date = "measurement_date"),
    "2102" = list(table = "device_exposure",      kind = "month", date = "device_exposure_start_date"),
    "505"  = list(table = "death",                kind = "concept", col = "death_type_concept_id"),
    "1818" = list(table = "measurement",          kind = "range")
  )

  spec <- companion_specs[[as.character(analysis_id)]]
  empty <- data.frame(stratum_1 = character(0), n_persons = numeric(0),
                      stringsAsFactors = FALSE)
  if (is.null(spec)) return(empty)

  bp <- .buildBlueprint(handle)
  if (!spec$table %in% bp$tables$table_name[bp$tables$present_in_db]) {
    return(empty)
  }

  qualified <- .qualifyTable(handle, spec$table, handle$cdm_schema)
  txt_cast <- if (identical(dialect, "postgresql")) "VARCHAR" else "TEXT"
  month_key <- if (identical(dialect, "postgresql")) {
    paste0("EXTRACT(YEAR FROM ", spec$date, ") * 100 + ",
           "EXTRACT(MONTH FROM ", spec$date, ")")
  } else {
    paste0("CAST(strftime('%Y%m', ", spec$date, ") AS INTEGER)")
  }

  sql <- switch(spec$kind,
    "month" = paste0(
      "SELECT CAST(", month_key, " AS ", txt_cast, ") AS stratum_1, ",
      "COUNT(DISTINCT person_id) AS n_persons FROM ", qualified,
      " WHERE ", spec$date, " IS NOT NULL",
      " GROUP BY ", month_key
    ),
    "concept" = paste0(
      "SELECT CAST(", spec$col, " AS ", txt_cast, ") AS stratum_1, ",
      "COUNT(DISTINCT person_id) AS n_persons FROM ", qualified,
      " WHERE ", spec$col, " IS NOT NULL",
      " GROUP BY ", spec$col
    ),
    "range" = {
      # Labels MUST match what real OHDSI Achilles analysis 1818 stores in
      # stratum_3: 'Below Range Low' / 'Within Range' / 'Above Range High'
      # (verified against OHDSI/Achilles inst/sql .../1818.sql). Any mismatch
      # makes the gate join fail and fail-closed-drop every 1818 cell.
      bucket <- paste0(
        "CASE WHEN value_as_number < range_low THEN 'Below Range Low' ",
        "WHEN value_as_number > range_high THEN 'Above Range High' ",
        "ELSE 'Within Range' END"
      )
      paste0(
        "SELECT CAST(measurement_concept_id AS ", txt_cast, ") AS stratum_1, ",
        "CAST(unit_concept_id AS ", txt_cast, ") AS stratum_2, ",
        bucket, " AS stratum_3, ",
        "COUNT(DISTINCT person_id) AS n_persons FROM ", qualified,
        " WHERE value_as_number IS NOT NULL",
        " AND range_low IS NOT NULL AND range_high IS NOT NULL",
        " GROUP BY measurement_concept_id, unit_concept_id, ", bucket
      )
    }
  )

  result <- tryCatch(.executeQuery(handle, sql), error = function(e) empty)
  for (col in intersect(c("stratum_1", "stratum_2", "stratum_3"), names(result))) {
    result[[col]] <- as.character(result[[col]])
  }
  result
}

#' Fetch raw (un-suppressed) achilles_results rows for given analysis IDs
#'
#' Used to retrieve a sibling person analysis that the client did not request.
#' Deliberately does NOT apply \code{.suppressSmallCounts}: a sibling person
#' count of, say, 2 is exactly the signal that must DROP the matching record
#' cell, so it has to survive to be read as the gate value. These rows are never
#' returned to the client — they are consumed only by .achillesGatePersonCounts.
#'
#' @param handle CDM handle.
#' @param analysis_ids Integer vector.
#' @return Data frame with analysis_id, stratum_1..5, count_value.
#' @keywords internal
.achillesFetchRawRows <- function(handle, analysis_ids) {
  empty <- data.frame(analysis_id = integer(0), stratum_1 = character(0),
                      stratum_2 = character(0), stratum_3 = character(0),
                      stratum_4 = character(0), stratum_5 = character(0),
                      count_value = numeric(0), stringsAsFactors = FALSE)
  qualified <- .qualifyTable(handle, "achilles_results", .resolveResultsSchema(handle))
  id_list <- paste(as.integer(analysis_ids), collapse = ", ")
  sql <- paste0(
    "SELECT analysis_id, stratum_1, stratum_2, stratum_3, stratum_4, ",
    "stratum_5, count_value FROM ", qualified,
    " WHERE analysis_id IN (", id_list, ")"
  )
  tryCatch(.executeQuery(handle, sql), error = function(e) empty)
}

#' Resolve distinct-person counts for one record-gated analysis (cached)
#'
#' Returns a data frame with the analysis's join-stratum columns plus an
#' \code{n_persons} column. For "sibling" analyses the counts are read from the
#' sibling x00 rows (fetched if absent) — no CDM query. For "companion"
#' analyses they come from \code{.achillesCompanionPersonCounts}. Cached per
#' analysis_id in \code{handle$achilles_gate_cache}.
#'
#' @param handle CDM handle.
#' @param analysis_id Integer; the record-unit analysis being gated.
#' @param spec Gating spec entry for \code{analysis_id}.
#' @param rows Data frame of already-read achilles_results rows.
#' @return Data frame with \code{spec$join_strata} columns and \code{n_persons}.
#' @keywords internal
.achillesGatePersonCounts <- function(handle, analysis_id, spec, rows) {
  if (is.null(handle$achilles_gate_cache)) handle$achilles_gate_cache <- list()
  cache_key <- as.character(analysis_id)
  cached <- handle$achilles_gate_cache[[cache_key]]
  if (!is.null(cached)) return(cached)

  if (identical(spec$strategy, "sibling")) {
    sib <- spec$sibling
    sib_rows <- rows[!is.na(rows$analysis_id) & rows$analysis_id == sib, ,
                     drop = FALSE]
    if (nrow(sib_rows) == 0) sib_rows <- .achillesFetchRawRows(handle, sib)
    counts <- sib_rows[, c(spec$join_strata, "count_value"), drop = FALSE]
    names(counts)[names(counts) == "count_value"] <- "n_persons"
  } else {
    counts <- .achillesCompanionPersonCounts(handle, analysis_id)
  }

  counts$n_persons <- suppressWarnings(as.numeric(counts$n_persons))
  handle$achilles_gate_cache[[cache_key]] <- counts
  counts
}

#' Drop record-unit cells backed by too few distinct persons
#'
#' Given the data frame of read \code{achilles_results} rows, drops every
#' record-unit cell (one of the analyses in \code{.achilles_record_gate_spec()})
#' whose underlying distinct-person count is below \code{nfilter.tab}. Rows for
#' all OTHER analyses (person-unit, distribution) pass through untouched — those
#' are gated by \code{.suppressSmallCounts()} on their own count value.
#'
#' Consistent with the "no hints" policy, a record cell with NO matching person
#' count (no sibling/companion row, hence effectively zero persons) is dropped
#' (fail-closed), exactly as a sub-threshold cell would be.
#'
#' @param handle CDM handle.
#' @param rows Data frame with analysis_id, stratum_1..5, count_value.
#' @return \code{rows} with disclosive record cells removed.
#' @keywords internal
.achillesPersonGate <- function(handle, rows) {
  if (nrow(rows) == 0) return(rows)
  spec_all <- .achilles_record_gate_spec()
  threshold <- .omopDisclosureSettings()$nfilter_tab

  gated_ids <- intersect(unique(rows$analysis_id), .achillesRecordGatedIds())
  if (length(gated_ids) == 0) return(rows)

  keep <- rep(TRUE, nrow(rows))
  for (aid in gated_ids) {
    spec <- spec_all[[as.character(aid)]]
    counts <- .achillesGatePersonCounts(handle, aid, spec, rows)

    is_rec <- !is.na(rows$analysis_id) & rows$analysis_id == aid
    rec_key <- .achillesStratumKey(rows[is_rec, , drop = FALSE], spec$join_strata)
    cnt_key <- .achillesStratumKey(counts, spec$join_strata)

    n_persons <- counts$n_persons[match(rec_key, cnt_key)]
    keep[is_rec] <- !is.na(n_persons) & n_persons >= threshold
  }

  result <- rows[keep, , drop = FALSE]
  rownames(result) <- NULL
  result
}
