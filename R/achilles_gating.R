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
#     1818). Compute a targeted, cached COUNT(DISTINCT person_id) grouped by
#     the same strata directly from the CDM domain table, over exactly the
#     records the upstream analysis counts (events inside an observation
#     period, with the analysis's own event restrictions), then drop record
#     cells whose person basis is below max(nfilter.tab, nfilter.subset).
#
# Analysis 2102 is not a record analysis: upstream it counts distinct PERSONS
# by device_concept_id and calendar month, so it is gated as a person-unit
# analysis on its own count (see .achilles_catalog_static()).

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
#' Normalizes the contracted stratum columns into a single comparable character
#' key so record rows and person-count rows can be matched. A NULL stratum value
#' (a legitimate upstream group, e.g. a missing death type) becomes the empty
#' string; parts are "\\x1f"-joined (a delimiter that cannot occur in a concept
#' id or YYYYMM month) to avoid collisions between, e.g., ("1","23") and
#' ("12","3").
#'
#' Every contracted column is required. A frame that lacks one of them (or a
#' contract with no stratum columns at all) yields \code{NA} keys, which never
#' match, so the affected record cells fail closed instead of being joined on a
#' silently shortened key (for 1818, a key without the unit and range bucket
#' would let one bucket borrow another bucket's person count).
#'
#' @param df Data frame containing the stratum columns.
#' @param cols Character vector of contracted stratum column names.
#' @return Character vector, one composite key per row (\code{NA} when the
#'   contract cannot be evaluated).
#' @keywords internal
.achillesStratumKey <- function(df, cols) {
  n <- if (is.data.frame(df)) nrow(df) else 0L
  if (length(cols) == 0L || !is.data.frame(df) ||
      !all(cols %in% names(df))) {
    return(rep(NA_character_, n))
  }
  parts <- lapply(cols, function(col) {
    v <- as.character(df[[col]])
    v[is.na(v)] <- ""
    v
  })
  do.call(paste, c(parts, list(sep = "\x1f")))
}

#' Companion person-basis contracts of the record-unit Achilles analyses
#'
#' One entry per companion-gated analysis: the CDM domain table, the event date
#' that places a record inside an observation period, the stratum expressions
#' (one per contracted \code{stratum_*} column, built on the domain alias
#' \code{d}) and any further upstream event restriction. The records counted
#' are exactly those of the upstream OHDSI Achilles SQL (OHDSI/Achilles
#' \code{inst/sql/sql_server/analyses/<id>.sql}): events dated inside one of the
#' person's observation periods, endpoints included.
#'
#' @param handle CDM handle (selects the dialect of the month key).
#' @return Named list keyed by character analysis_id.
#' @keywords internal
.achillesCompanionSpecs <- function(handle) {
  month <- function(table, date) {
    list(table = table, date = date,
         strata = list(stratum_1 = .omopMonthKeySql(handle, paste0("d.", date))),
         where = character(0), columns = date)
  }
  list(
    "220"  = month("visit_occurrence", "visit_start_date"),
    "420"  = month("condition_occurrence", "condition_start_date"),
    "620"  = month("procedure_occurrence", "procedure_date"),
    "720"  = month("drug_exposure", "drug_exposure_start_date"),
    "820"  = month("observation", "observation_date"),
    "1820" = month("measurement", "measurement_date"),
    # 505: death records by death type; a NULL type is its own upstream group.
    "505"  = list(table = "death", date = "death_date",
                  strata = list(stratum_1 = "d.death_type_concept_id"),
                  where = character(0),
                  columns = c("death_date", "death_type_concept_id")),
    # 1818: measurement records by concept x unit x normal-range bucket, over
    # records with a numeric value, a unit and both range bounds.
    "1818" = list(table = "measurement", date = "measurement_date",
                  strata = list(stratum_1 = "d.measurement_concept_id",
                                stratum_2 = "d.unit_concept_id",
                                stratum_3 = .achillesRangeBucketSql("d")),
                  where = c("d.value_as_number IS NOT NULL",
                            "d.unit_concept_id IS NOT NULL",
                            "d.range_low IS NOT NULL",
                            "d.range_high IS NOT NULL"),
                  columns = c("measurement_date", "measurement_concept_id",
                              "unit_concept_id", "value_as_number",
                              "range_low", "range_high"))
  )
}

#' Normal-range bucket of Achilles analysis 1818
#'
#' The exact CASE of OHDSI/Achilles analysis 1818 (stratum_3 labels are
#' load-bearing: they are the strata stored in \code{achilles_results}). A
#' companion that bucketed records differently would join a cell to another
#' bucket's person count.
#'
#' @param alias Alias of the measurement table.
#' @return Character; a SQL CASE expression.
#' @keywords internal
.achillesRangeBucketSql <- function(alias) {
  v <- paste0(alias, ".value_as_number")
  lo <- paste0(alias, ".range_low")
  hi <- paste0(alias, ".range_high")
  paste0(
    "CASE WHEN ", v, " < ", lo, " THEN 'Below Range Low' ",
    "WHEN ", v, " >= ", lo, " AND ", v, " <= ", hi, " THEN 'Within Range' ",
    "WHEN ", v, " > ", hi, " THEN 'Above Range High' ",
    "ELSE 'Other' END"
  )
}

#' Compute companion distinct-person counts for a record-count analysis
#'
#' For record-count analyses with NO precomputed person sibling (monthly
#' 220/420/620/720/820/1820; death 505; measurement range 1818), compute a
#' targeted \code{COUNT(DISTINCT d.person_id)} grouped by the same gate strata
#' directly from the CDM domain table, over the records the upstream analysis
#' counts: a correlated \code{EXISTS} keeps an event only when its date lies
#' inside one of the person's observation periods (endpoints included). EXISTS
#' rather than a join means duplicate or overlapping observation periods can
#' never inflate the person basis, and out-of-period events (which the stored
#' record count excludes) can never lend support to an in-period cell. Returned
#' strata are emitted as text and built to be join-compatible with the record
#' analysis's stratum columns: concept ids and YYYYMM month keys match
#' \code{achilles_results.stratum_*} storage.
#'
#' Fail closed: a missing domain table, observation_period table or required
#' column, or a failing query, returns an empty frame with the contracted
#' stratum columns, so every record cell of the analysis is withheld; there is
#' never a fallback to an un-restricted count.
#'
#' @param handle CDM handle.
#' @param analysis_id Integer; a companion-gated analysis id.
#' @return data.frame with the join-stratum columns + \code{n_persons}; empty
#'   frame if the analysis is not companion-gated or its basis is unavailable.
#' @keywords internal
.achillesCompanionPersonCounts <- function(handle, analysis_id) {
  analysis_id <- as.integer(analysis_id)
  spec <- .achillesCompanionSpecs(handle)[[as.character(analysis_id)]]
  strata_cols <- if (is.null(spec)) "stratum_1" else names(spec$strata)
  empty <- as.data.frame(
    c(stats::setNames(rep(list(character(0)), length(strata_cols)),
                      strata_cols),
      list(n_persons = numeric(0))),
    stringsAsFactors = FALSE
  )
  if (is.null(spec)) return(empty)

  bp <- .buildBlueprint(handle)
  present <- bp$tables$table_name[bp$tables$present_in_db]
  if (!all(c(spec$table, "observation_period") %in% present)) return(empty)
  has_columns <- function(table, columns) {
    cols <- bp$columns[[table]]
    is.data.frame(cols) && all(columns %in% tolower(cols$column_name))
  }
  if (!has_columns(spec$table, unique(c("person_id", spec$columns))) ||
      !has_columns("observation_period",
                   c("person_id", "observation_period_start_date",
                     "observation_period_end_date"))) {
    return(empty)
  }

  qualified <- .qualifyTable(handle, spec$table, handle$cdm_schema)
  observation <- .qualifyTable(handle, "observation_period", handle$cdm_schema)
  text_cast <- function(expression) .omopTextCastSql(handle, expression, 64L)
  event_date <- paste0("d.", spec$date)
  in_period <- paste0(
    "EXISTS (SELECT 1 FROM ", observation, " op",
    " WHERE op.person_id = d.person_id",
    " AND ", event_date, " >= op.observation_period_start_date",
    " AND ", event_date, " <= op.observation_period_end_date)"
  )
  select <- c(
    vapply(names(spec$strata), function(col) {
      paste0(text_cast(spec$strata[[col]]), " AS ", col)
    }, character(1L)),
    "COUNT(DISTINCT d.person_id) AS n_persons"
  )
  where <- c(paste0(event_date, " IS NOT NULL"), spec$where, in_period)
  sql <- .sql_translate(paste0(
    "SELECT ", paste(select, collapse = ", "),
    " FROM ", qualified, " d",
    " WHERE ", paste(where, collapse = " AND "),
    " GROUP BY ", paste(unlist(spec$strata, use.names = FALSE),
                        collapse = ", ")
  ), handle$target_dialect)

  result <- tryCatch(.executeQuery(handle, sql), error = function(e) empty)
  if (!is.data.frame(result) ||
      !all(c(strata_cols, "n_persons") %in% names(result))) {
    return(empty)
  }
  for (col in strata_cols) {
    result[[col]] <- as.character(result[[col]])
  }
  result[, c(strata_cols, "n_persons"), drop = FALSE]
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

#' Resolve distinct-person counts for one record-gated analysis
#'
#' Returns a data frame with the analysis's join-stratum columns plus an
#' \code{n_persons} column. For "sibling" analyses the counts are read from the
#' sibling x00 rows (fetched if absent) — no CDM query. For "companion"
#' analyses they come from \code{.achillesCompanionPersonCounts}.
#'
#' The basis is recomputed on every read (dsOMOP 2.10.3): a release decision
#' never rests on support remembered from an earlier read, so a CDM or results
#' refresh that drops a cell below the person threshold withholds it at once,
#' on cached and new handles alike. There is no per-handle support cache.
#'
#' @param handle CDM handle.
#' @param analysis_id Integer; the record-unit analysis being gated.
#' @param spec Gating spec entry for \code{analysis_id}.
#' @param rows Data frame of already-read achilles_results rows.
#' @return Data frame with \code{spec$join_strata} columns and \code{n_persons}.
#' @keywords internal
.achillesGatePersonCounts <- function(handle, analysis_id, spec, rows) {
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
  settings <- .omopDisclosureSettings()
  threshold <- max(settings$nfilter_tab, settings$nfilter_subset)

  gated_ids <- intersect(unique(rows$analysis_id), .achillesRecordGatedIds())
  if (length(gated_ids) == 0) return(rows)

  keep <- rep(TRUE, nrow(rows))
  for (aid in gated_ids) {
    spec <- spec_all[[as.character(aid)]]
    counts <- .achillesGatePersonCounts(handle, aid, spec, rows)

    is_rec <- !is.na(rows$analysis_id) & rows$analysis_id == aid
    rec_key <- .achillesStratumKey(rows[is_rec, , drop = FALSE], spec$join_strata)
    cnt_key <- .achillesStratumKey(counts, spec$join_strata)

    # An unevaluable key (NA: a contracted stratum column is missing on either
    # side) never matches, so its record cells are withheld.
    basis <- if (is.data.frame(counts) && "n_persons" %in% names(counts)) {
      suppressWarnings(as.numeric(counts$n_persons))
    } else rep(NA_real_, length(cnt_key))
    n_persons <- basis[match(rec_key, cnt_key, incomparables = NA)]
    keep[is_rec] <- !is.na(n_persons) & n_persons >= threshold
  }

  result <- rows[keep, , drop = FALSE]
  rownames(result) <- NULL
  result
}
