# Module: Profiling Engine
# Data profiling functions for table stats, column stats, distributions, and concept analysis.

#' Resolve the concept column a profiler scopes its concept_id filter on
#'
#' By default a concept scope (\code{concept_id}) restricts to the table's
#' DOMAIN concept column (e.g. \code{measurement_concept_id}). Supplying
#' \code{concept_col} lets the caller scope by another concept column on the
#' same table instead - \code{unit_concept_id}, a \code{*_type_concept_id}, or
#' \code{value_as_concept_id} - which enables unit-aware value distributions and
#' value-by-type profiling. This is the single authoritative chokepoint for
#' concept scoping: every profiler that turns \code{concept_col} into a WHERE
#' filter resolves it here, so the override is gated fail-closed and CANNOT be
#' used to filter on a forbidden column.
#'
#' An explicit override must be a genuine, releasable concept column:
#' \itemize{
#'   \item it must EXIST on this table (so it cannot reach another table);
#'   \item it must NOT be blocked (\code{is_blocked}) - this rejects every
#'     \code{*_source_value} / \code{*_source_concept_id} column (which must be
#'     treated as if it does not exist) and all other PII, closing the
#'     source-value filter leak;
#'   \item it must be a concept column (name ends in \code{_concept_id}) that is
#'     not a source concept - this additionally rejects identifier / person-key
#'     columns (\code{person_id}, \code{*_occurrence_id}, foreign keys), which
#'     are not concepts and must never be used as a scope filter.
#' }
#' Anything else stops with a generic error. Returns NULL only when NO override
#' is given and the table has no default domain concept column.
#'
#' @param bp Blueprint
#' @param table Character; lower-cased table name
#' @param concept_col Character or NULL; explicit concept column override
#' @return Character column name, or NULL if neither default nor override resolves
#' @keywords internal
.resolveConceptScopeColumn <- function(bp, table, concept_col = NULL) {
  if (is.null(concept_col)) {
    return(.getDomainConceptColumn(bp, table))
  }
  concept_col <- tolower(.validateIdentifier(concept_col, "concept column"))
  cols <- bp$columns[[table]]
  if (is.null(cols) || !concept_col %in% cols$column_name) {
    stop("Concept column '", concept_col, "' not found in '", table, "'.",
         call. = FALSE)
  }
  crow <- cols[cols$column_name == concept_col, , drop = FALSE]
  # Fail-closed: a blocked column (any *_source_value / *_source_concept_id or
  # other PII) is never a valid scope filter - treat it as if it does not exist.
  if (isTRUE(crow$is_blocked[1])) {
    stop("Concept column '", concept_col, "' is not a valid scope column.",
         call. = FALSE)
  }
  # Fail-closed: only true concept columns may scope, and never source concepts.
  # This also rejects identifier / person-key columns (they are non_concept).
  if (!grepl("_concept_id$", concept_col) ||
      identical(crow$concept_role[1], "source_concept")) {
    stop("Concept column '", concept_col, "' is not a valid scope column.",
         call. = FALSE)
  }
  concept_col
}

#' Validate a column before a profiler can emit information about its values
#'
#' This is the common release-policy gate for column statistics, value counts,
#' numeric distributions and cross-tab axes.  Blueprint `is_blocked` alone is
#' not sufficient: OMOP primary/foreign keys such as `person_id` and
#' `visit_occurrence_id` are intentionally usable inside server-side joins
#' and therefore are not marked blocked, but their raw values must never become
#' profiler output.  Standard concept foreign keys remain valid categorical
#' dimensions; numeric profilers reject them because their integer storage does
#' not make concept codes continuous measures.
#'
#' @param bp Schema blueprint.
#' @param table Character; lower-case table name.
#' @param column Character; lower-case column name.
#' @param require_numeric Logical; require a genuine numeric measure.
#' @param allow_identifiers Logical; allow identifiers for operations that never
#'   emit their values (currently missingness rates only).
#' @return List with the blueprint row and `is_numeric_measure`.
#' @keywords internal
.profilerColumnInfo <- function(bp, table, column, require_numeric = FALSE,
                                allow_identifiers = FALSE) {
  col_df <- bp$columns[[table]]
  if (is.null(col_df) || !column %in% col_df$column_name) {
    stop("Column '", column, "' not found in '", table, "'.", call. = FALSE)
  }
  crow <- col_df[col_df$column_name == column, , drop = FALSE][1, , drop = FALSE]

  blocked <- isTRUE(crow$is_blocked[1]) || isTRUE(crow$is_sensitive[1]) ||
    .detectSensitiveColumns(column)
  if (blocked) {
    stop("Column '", column,
         "' is blocked (sensitive) and cannot be profiled.", call. = FALSE)
  }

  # Prefer the vendored OHDSI field metadata.  It precisely distinguishes a
  # clinical concept FK (safe as a categorical code) from row/entity keys.  For
  # introspection-only extension tables, fall back to conservative name rules.
  spec_row <- NULL
  if (!is.null(bp$spec_version)) {
    spec <- tryCatch(.loadCdmSpec(bp$spec_version), error = function(e) NULL)
    if (!is.null(spec) && !is.null(spec$field_level)) {
      fields <- spec$field_level
      spec_row <- fields[
        tolower(fields$cdmTableName) == table &
          tolower(fields$cdmFieldName) == column,
        , drop = FALSE
      ]
    }
  }

  yes <- function(x) {
    length(x) > 0L && !is.na(x[1]) &&
      tolower(trimws(as.character(x[1]))) %in% c("yes", "y", "true", "1")
  }
  is_identifier <- column %in% .identifierColumns()
  if (!is.null(spec_row) && nrow(spec_row) > 0L) {
    is_pk <- yes(spec_row$isPrimaryKey)
    is_fk <- yes(spec_row$isForeignKey)
    fk_table <- toupper(trimws(as.character(spec_row$fkTableName[1] %||% "")))
    # CONCEPT foreign keys are clinical categorical codes, not entity keys.
    is_identifier <- is_identifier || is_pk ||
      (is_fk && !identical(fk_table, "CONCEPT"))
  } else {
    is_identifier <- is_identifier ||
      (grepl("(^id$|_(id|key|identifier)$)", column) &&
         !grepl("_concept_id$", column))
  }
  if (is_identifier && !isTRUE(allow_identifiers)) {
    stop("Identifier column '", column,
         "' is not permitted for profiling.", call. = FALSE)
  }

  types <- tolower(trimws(as.character(c(
    crow$cdm_datatype[1] %||% "", crow$db_datatype[1] %||% ""
  ))))
  types <- types[!is.na(types) & nzchar(types)]
  numeric_type <- any(grepl(
    "(^|\\b)(tinyint|smallint|mediumint|integer|int[0-9]*|bigint|hugeint|utinyint|usmallint|uinteger|ubigint|uhugeint|decimal|numeric|bignumeric|number|real|float[0-9]*|double|double precision|binary_float|binary_double|money|smallmoney)(\\b|\\s*\\()",
    types
  ))
  is_concept <- grepl("_concept_id$", column) ||
    (!is.na(crow$concept_role[1]) &&
       !identical(crow$concept_role[1], "non_concept"))
  is_numeric_measure <- numeric_type && !is_concept

  if (isTRUE(require_numeric) && !is_numeric_measure) {
    stop("Column '", column,
         "' is not a numeric measure and cannot be used by numeric profilers.",
         call. = FALSE)
  }

  list(row = crow, is_numeric_measure = is_numeric_measure)
}

#' Require a reviewed population unit before profiling a table
#'
#' Clinical and results tables may only be profiled when they expose the reviewed
#' direct `person_id` route used by the disclosure gates below. Do not infer
#' event-domain joins here: an unreviewed join can change the population unit and
#' a missing join would fall back to unsafe record counts. A small explicit set
#' of public OMOP Vocabulary/source-metadata tables remains record-based.
#'
#' @keywords internal
.profilerHasReviewedPersonScope <- function(bp, table, cohort_table = NULL) {
  columns <- bp$columns[[table]]$column_name %||% character(0)
  has_person <- "person_id" %in% columns
  if (has_person) return(TRUE)

  if (!is.null(cohort_table)) {
    stop("Profiling cohort scope cannot be applied to table '", table,
         "': no reviewed path to person_id is available.", call. = FALSE)
  }

  table_row <- bp$tables[bp$tables$table_name == table, , drop = FALSE]
  category <- if (nrow(table_row) == 1L) {
    tolower(table_row$schema_category[[1L]])
  } else {
    ""
  }
  if (length(category) != 1L || is.na(category)) category <- ""
  public_vocabulary <- c(
    "concept", "concept_ancestor", "concept_class", "concept_relationship",
    "concept_synonym", "domain", "drug_strength", "relationship",
    "vocabulary"
  )
  public_metadata <- c("cdm_source", "metadata")
  is_public <- (category == "vocabulary" && table %in% public_vocabulary) ||
    (category == "cdm" && table %in% public_metadata)
  if (!is_public) {
    stop("Profiling table '", table,
         "' requires a reviewed path to person_id; none is available.",
         call. = FALSE)
  }

  FALSE
}

#' Fail-closed distinct-person gate for a scoped numeric-distribution query
#'
#' The numeric-distribution profilers (range / quantiles / histogram / safe
#' cutpoints) summarise the VALUE distribution of a (possibly concept- or
#' cohort-scoped) relation. Gating those summaries on the RECORD count is not
#' enough: one individual can contribute many records (e.g. a single patient
#' with 20 lab measurements of the same concept), so a record count can clear
#' \code{nfilter_subset}/\code{nfilter_dist} while only one or two PEOPLE are
#' described - and p05/p95/quantiles/bin-edges then sit at that handful of
#' individuals' values (min/max). This mirrors the distinct-person gate already
#' enforced in \code{\link{.profileColumnStats}} / \code{\link{.profileValueCounts}}:
#' for a person-bearing table it counts \code{DISTINCT person_id} over EXACTLY
#' the same scoped relation (\code{from_clause} + \code{where_sql}) the statistic
#' describes and calls \code{\link{.assertMinPersons}} (which stops below the
#' threshold). Person-less tables (no \code{person_id}) have nothing to count, so
#' they fall through unchanged.
#'
#' @param handle CDM handle.
#' @param from_clause Character; the FROM clause (incl. any cohort INNER JOIN).
#' @param where_sql Character; the leading-space WHERE clause (may be "").
#' @param tbl_cols Character vector; the table's column names.
#' @return TRUE invisibly; stops (fail-closed) when the scoped distinct-person
#'   count is below \code{nfilter_subset}.
#' @keywords internal
.assertNumericDistPersons <- function(handle, from_clause, where_sql, tbl_cols) {
  if (!"person_id" %in% tbl_cols) return(invisible(TRUE))
  sql <- paste0("SELECT COUNT(DISTINCT t.person_id) AS n FROM ",
                from_clause, where_sql)
  n_persons <- .executeQuery(handle, .renderSql(handle, sql))$n[1]
  .assertMinPersons(n_persons = n_persons)
}

#' Build a disclosure-safe numeric distribution relation
#'
#' Person-bearing longitudinal tables default to one mean value per person.
#' Record-level mode is accepted only when the scoped relation is already 1:1
#' by person; otherwise it would let repeated records from a few people dominate
#' quantiles and histogram cells. Person-less reference tables remain
#' record-based because there is no individual contribution to protect.
#'
#' @keywords internal
.numericDistributionSql <- function(handle, from_clause, where_sql, value_col,
                                    tbl_cols, unit = "person") {
  unit <- match.arg(unit, c("person", "record"))
  has_person <- "person_id" %in% tbl_cols
  if (!has_person) {
    return(paste0("SELECT CAST(t.", value_col, " AS REAL) AS value FROM ",
                  from_clause, where_sql))
  }

  if (identical(unit, "record")) {
    multiplicity_sql <- paste0(
      "SELECT MAX(per_person.n) AS max_records FROM (",
      "SELECT t.person_id, COUNT(*) AS n FROM ", from_clause, where_sql,
      " GROUP BY t.person_id) AS per_person"
    )
    max_records <- .executeQuery(
      handle, .renderSql(handle, multiplicity_sql))$max_records[1]
    if (is.na(max_records) || max_records > 1L) {
      stop("unit='record' is not disclosure-safe when a person has multiple ",
           "scoped records; use unit='person'.", call. = FALSE)
    }
    return(paste0("SELECT CAST(t.", value_col, " AS REAL) AS value FROM ",
                  from_clause, where_sql))
  }

  paste0(
    "SELECT AVG(CAST(t.", value_col, " AS REAL)) AS value FROM ",
    from_clause, where_sql, " GROUP BY t.person_id"
  )
}

#' Select a quantile row while protecting both distribution tails
#'
#' @keywords internal
.protectedQuantileOffset <- function(n, probability, settings) {
  tail_min <- max(as.integer(settings$nfilter_tab),
                  as.integer(settings$nfilter_subset), 1L)
  if (is.na(n) || n < 2L * tail_min) {
    stop("Disclosive: insufficient contributions for protected distribution ",
         "tails.", call. = FALSE)
  }
  raw <- as.integer(floor(n * probability)) - 1L
  max(tail_min - 1L, min(raw, as.integer(n) - tail_min))
}

#' Get safe table-level statistics
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param stats Character vector; which stats to include
#' @return Named list with requested statistics
#' @keywords internal
.profileTableStats <- function(handle, table, stats = c("rows", "persons")) {
  table <- tolower(.validateIdentifier(table, "table"))
  bp <- .buildBlueprint(handle)

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) {
    stop("Table '", table, "' not found.", call. = FALSE)
  }

  qualified <- tbl_row$qualified_name[1]
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name
  result <- list()
  settings <- .omopDisclosureSettings()
  has_person <- .profilerHasReviewedPersonScope(bp, table)

  # Every statistic from a person-bearing table describes a population, even
  # when the requested output is only a row count or a date range. Gate that
  # population once on exact distinct persons so repeated longitudinal records
  # from one or two people cannot make any branch releasable.
  n_persons <- NULL
  person_gate_ok <- TRUE
  if (has_person) {
    person_sql <- paste0("SELECT COUNT(DISTINCT person_id) AS n FROM ",
                         qualified)
    n_persons <- .executeQuery(handle, person_sql)$n[1]
    person_gate_ok <- !is.na(n_persons) &&
      n_persons >= settings$nfilter_subset
  }

  # Surviving counts are banded down (floor to nfilter_band) at the return
  # boundary so an exact supra-threshold count is never released; the gate is
  # still the exact count compared against nfilter_subset.
  if ("rows" %in% stats) {
    sql <- paste0("SELECT COUNT(*) AS n FROM ", qualified)
    n_rows <- .executeQuery(handle, sql)$n[1]
    row_gate_ok <- !is.na(n_rows) && n_rows >= settings$nfilter_subset &&
      (!has_person || person_gate_ok)
    if (row_gate_ok) {
      result$rows <- .bandCount(n_rows, settings$nfilter_band)
    }
  }

  if ("persons" %in% stats && has_person) {
    if (person_gate_ok) {
      result$persons <- .bandCount(n_persons, settings$nfilter_band)
    }
  }

  if ("date_range" %in% stats && (!has_person || person_gate_ok)) {
    date_col <- .getDateColumn(bp, table)
    if (!is.null(date_col)) {
      # Never publish MIN/MAX(month) directly: either endpoint may be supported
      # by one person. Reuse the period profiler, which drops months below the
      # exact distinct-person threshold, and derive the range only from the
      # surviving supported periods. If none survive, omit the range entirely.
      supported <- tryCatch(
        .profileDateCounts(handle, table, date_col = date_col,
                           granularity = "month"),
        error = function(e) NULL
      )
      if (!is.null(supported) && nrow(supported) > 0L) {
        periods <- sort(as.character(supported$period))
        result$date_range <- list(
          column = date_col,
          min_month = periods[1],
          max_month = periods[length(periods)]
        )
      }
    }
  }

  result
}

#' Get column-level statistics
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param column Character; column name
#' @param concept_id Integer or NULL; restrict to rows of this concept
#' @param cohort_table Character; cohort temp table name to scope the
#'   population (INNER JOIN on subject_id), or NULL.
#' @return Named list with column statistics
#' @keywords internal
.profileColumnStats <- function(handle, table, column, concept_id = NULL,
                                concept_col = NULL, cohort_table = NULL) {
  table <- tolower(.validateIdentifier(table, "table"))
  column <- tolower(.validateIdentifier(column, "column"))
  bp <- .buildBlueprint(handle)

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)

  col_df <- bp$columns[[table]]
  has_person <- .profilerHasReviewedPersonScope(
    bp, table, cohort_table = cohort_table
  )
  column_info <- .profilerColumnInfo(bp, table, column)

  qualified <- tbl_row$qualified_name[1]
  settings <- .omopDisclosureSettings()

  # FROM + optional cohort scope (INNER JOIN on subject_id, as in prevalence).
  # Everything is computed over this scoped relation so the distinct-person gate
  # below applies to exactly the population the statistics describe.
  from_clause <- paste0(qualified, " AS t")
  if (!is.null(cohort_table) && has_person) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN (SELECT DISTINCT subject_id FROM ",
                          cohort_table, ") AS coh",
                          " ON t.person_id = coh.subject_id")
  }

  # Optional concept scope: restrict every query to one concept of this table.
  # concept_col defaults to the domain concept but may override to scope by
  # unit_concept_id / *_type_concept_id / value_as_concept_id.
  concept_filter <- NULL
  if (!is.null(concept_id)) {
    ccol <- .resolveConceptScopeColumn(bp, table, concept_col)
    if (is.null(ccol)) {
      stop("Table '", table, "' has no concept column to scope by.",
           call. = FALSE)
    }
    concept_filter <- paste0("t.", ccol, " = ", as.integer(concept_id))
  }

  # For person-bearing tables the disclosure gate must count DISTINCT persons,
  # not records: one person can contribute many rows, so a record count can sail
  # past the threshold while only a handful of individuals are involved.
  sql <- paste0(
    "SELECT ",
    "COUNT(*) AS n_total, ",
    if (has_person) "COUNT(DISTINCT t.person_id) AS n_persons, " else "",
    "SUM(CASE WHEN t.", column, " IS NULL THEN 1 ELSE 0 END) AS n_missing, ",
    "COUNT(DISTINCT t.", column, ") AS n_distinct ",
    "FROM ", from_clause,
    if (!is.null(concept_filter)) paste0(" WHERE ", concept_filter) else ""
  )
  stats_result <- .executeQuery(handle, .renderSql(handle, sql))

  if (has_person) {
    person_support_sql <- paste0(
      "SELECT COUNT(*) AS n_persons, ",
      "SUM(CASE WHEN per_person.has_value = 0 THEN 1 ELSE 0 END) ",
      "AS n_missing_persons, ",
      "SUM(CASE WHEN per_person.has_value = 1 THEN 1 ELSE 0 END) ",
      "AS n_value_persons FROM (",
      "SELECT t.person_id, MAX(CASE WHEN t.", column,
      " IS NOT NULL THEN 1 ELSE 0 END) AS has_value FROM ", from_clause,
      if (!is.null(concept_filter)) paste0(" WHERE ", concept_filter) else "",
      " GROUP BY t.person_id) AS per_person"
    )
    person_support <- .executeQuery(
      handle, .renderSql(handle, person_support_sql))
  }

  result <- list(
    n_total = stats_result$n_total[1],
    n_missing = if (has_person) person_support$n_missing_persons[1]
                else stats_result$n_missing[1],
    n_distinct = stats_result$n_distinct[1]
  )
  if (has_person) result$n_persons <- stats_result$n_persons[1]

  # Gate on distinct persons for person-bearing tables (fail-closed), falling
  # back to the record count only for tables with no person_id (e.g. vocabulary).
  gate_n <- if (has_person) result$n_persons else result$n_total
  if (is.na(gate_n) || gate_n < settings$nfilter_subset) {
    stop("Disclosive: insufficient individuals.", call. = FALSE)
  }

  # Missingness can be differenced into rows with and without values. On
  # longitudinal tables, gate BOTH sides on distinct contributing persons;
  # record counts alone are not population support.
  if (has_person) {
    n_missing_persons <- person_support$n_missing_persons[1]
    n_value_persons <- person_support$n_value_persons[1]
    n_value_unit <- result$n_persons - result$n_missing
    missing_safe <- isTRUE(result$n_missing == 0) ||
      (!is.na(n_missing_persons) &&
         n_missing_persons >= settings$nfilter_tab)
    value_safe <- isTRUE(n_value_unit == 0) ||
      (!is.na(n_value_persons) && n_value_persons >= settings$nfilter_tab)
    if (!missing_safe || !value_safe) result$n_missing <- NA_real_
  } else if (!is.na(result$n_missing) && result$n_missing > 0 &&
             result$n_missing < settings$nfilter_tab) {
    result$n_missing <- NA_real_
  }

  # Suppress n_distinct when it fails the high-cardinality gate (quasi-unique
  # columns enable re-identification). Reuse .assertSafeLevels non-fatally.
  if (!is.na(result$n_distinct)) {
    safe_distinct <- tryCatch({
      .assertSafeLevels(result$n_distinct, result$n_total)
      TRUE
    }, error = function(e) FALSE)
    if (!safe_distinct) result$n_distinct <- NA_real_
  }

  # Band the record/person counts at the return boundary (after the person gate
  # and after the n_missing / n_distinct suppression, both of which depend on the
  # EXACT totals). n_distinct is a distinct-value cardinality, not a person/record
  # count, so it is not banded.
  result$n_total <- .bandCount(result$n_total, settings$nfilter_band)
  if (!is.na(result$n_missing)) {
    result$n_missing <- .bandCount(result$n_missing, settings$nfilter_band)
  }
  if (has_person) {
    result$n_persons <- .bandCount(result$n_persons, settings$nfilter_band)
  }

  # Numeric stats if applicable
  if (isTRUE(column_info$is_numeric_measure)) {
    # Disclosure-safe numeric summary: mean requires nfilter_subset DISTINCT
    # contributors with a value, while SD requires nfilter_dist contributors.
    # Both summarise one within-person mean per contributor, so repeated values
    # cannot dominate the result. Min/max remain forbidden because they identify
    # tail individuals.
    value_where <- paste0(" WHERE t.", column, " IS NOT NULL",
      if (!is.null(concept_filter)) paste0(" AND ", concept_filter) else "")
    value_distribution <- .numericDistributionSql(
      handle, from_clause, value_where, column, col_df$column_name,
      unit = "person"
    )
    num_sql <- paste0(
      "SELECT AVG(distribution_values.value) AS mean_val, ",
      "COUNT(*) AS n_val, ",
      "SUM(distribution_values.value * distribution_values.value) AS sumsq, ",
      "SUM(distribution_values.value) AS sumval FROM (", value_distribution,
      ") AS distribution_values"
    )
    num_stats <- tryCatch(.executeQuery(handle, .renderSql(handle, num_sql)),
                          error = function(e) NULL)
    result$mean <- NA_real_
    result$sd <- NA_real_
    if (!is.null(num_stats) && nrow(num_stats) > 0) {
      nfilter_dist <- settings$nfilter_dist %||% 10L
      n_val <- num_stats$n_val[1]
      n_value_people <- n_val
      if (!is.na(n_value_people) &&
          n_value_people >= settings$nfilter_subset) {
        result$mean <- round(num_stats$mean_val[1], 4)
      }
      # Sample SD over the one-value-per-person relation.
      if (!is.na(n_value_people) && n_value_people >= nfilter_dist &&
          !is.na(n_val) && n_val > 1) {
        mu <- num_stats$sumval[1] / n_val
        var_num <- (num_stats$sumsq[1] - n_val * mu * mu) / (n_val - 1)
        # Guard tiny negative variance from floating-point cancellation.
        if (!is.na(var_num)) {
          result$sd <- round(sqrt(max(var_num, 0)), 4)
        }
      }
    }
  }

  result
}

#' Get cross-table domain coverage
#'
#' @param handle CDM handle
#' @return Data frame with table, n_persons, schema_category
#' @keywords internal
.profileDomainCoverage <- function(handle) {
  bp <- .buildBlueprint(handle)
  settings <- .omopDisclosureSettings()

  # Only clinical tables with person_id
  clinical <- bp$tables[bp$tables$present_in_db &
                          bp$tables$has_person_id &
                          bp$tables$schema_category == "CDM", , drop = FALSE]

  results <- data.frame(
    table_name = character(0),
    n_persons = numeric(0),
    suppressed = logical(0),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(clinical))) {
    tbl_name <- clinical$table_name[i]
    qualified <- clinical$qualified_name[i]
    sql <- paste0("SELECT COUNT(DISTINCT person_id) AS n FROM ", qualified)
    n <- tryCatch(.executeQuery(handle, sql)$n[1], error = function(e) NA_real_)

    suppressed <- !is.na(n) && n < settings$nfilter_subset
    results <- rbind(results, data.frame(
      table_name = tbl_name,
      # Band the surviving per-table person count at the return boundary; the
      # gate above uses the exact count.
      n_persons = if (suppressed) NA_real_ else .bandCount(n, settings$nfilter_band),
      suppressed = suppressed,
      stringsAsFactors = FALSE
    ))
  }

  .dropSuppressed(results)
}

#' Get missingness rates for columns
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param columns Character vector; columns to check (NULL = all)
#' @param cohort_table Character; cohort temp table name to scope the
#'   population (INNER JOIN on subject_id), or NULL.
#' @return Data frame with column_name and missing_rate
#' @keywords internal
.profileMissingness <- function(handle, table, columns = NULL,
                                cohort_table = NULL) {
  table <- tolower(.validateIdentifier(table, "table"))
  bp <- .buildBlueprint(handle)

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)

  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name
  has_person <- .profilerHasReviewedPersonScope(
    bp, table, cohort_table = cohort_table
  )

  if (!is.null(columns)) {
    columns <- tolower(columns)
    columns <- intersect(columns, tbl_cols)
    for (column in columns) {
      .profilerColumnInfo(bp, table, column, allow_identifiers = TRUE)
    }
  } else {
    # Exclude blocked/free-text columns. Identifier missingness is safe because
    # this endpoint emits only a population-gated rate, never identifier values.
    safe <- !col_df$is_blocked & !col_df$is_sensitive &
      !vapply(col_df$column_name, .detectSensitiveColumns, logical(1))
    columns <- col_df$column_name[safe]
  }

  qualified <- tbl_row$qualified_name[1]
  settings <- .omopDisclosureSettings()

  # FROM + optional cohort scope (INNER JOIN on subject_id, as in prevalence).
  from_clause <- paste0(qualified, " AS t")
  if (!is.null(cohort_table) && has_person) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN (SELECT DISTINCT subject_id FROM ",
                          cohort_table, ") AS coh",
                          " ON t.person_id = coh.subject_id")
  }

  if (has_person) {
    persons_sql <- paste0("SELECT COUNT(DISTINCT t.person_id) AS n FROM ",
                          from_clause)
    gate_n <- .executeQuery(handle, .renderSql(handle, persons_sql))$n[1]
  } else {
    total_sql <- paste0("SELECT COUNT(*) AS n FROM ", from_clause)
    total <- .executeQuery(handle, .renderSql(handle, total_sql))$n[1]
    gate_n <- total
  }
  if (is.na(gate_n) || gate_n < settings$nfilter_subset) {
    stop("Disclosive: insufficient individuals.", call. = FALSE)
  }

  results <- data.frame(
    column_name = character(0),
    missing_rate = numeric(0),
    stringsAsFactors = FALSE
  )

  for (col in columns) {
    if (has_person) {
      # One contribution per person: a person is missing only when NONE of
      # their scoped longitudinal records carries a value for this column.
      sql <- paste0(
        "SELECT COUNT(*) AS n_total, ",
        "SUM(CASE WHEN per_person.has_value = 0 THEN 1 ELSE 0 END) AS n_missing ",
        "FROM (SELECT t.person_id, MAX(CASE WHEN t.", col,
        " IS NOT NULL THEN 1 ELSE 0 END) AS has_value FROM ", from_clause,
        " GROUP BY t.person_id) AS per_person"
      )
      counts <- .executeQuery(handle, .renderSql(handle, sql))
      total_unit <- counts$n_total[1]
      n_missing <- counts$n_missing[1]
    } else {
      sql <- paste0(
        "SELECT COUNT(*) AS n_total, ",
        "SUM(CASE WHEN t.", col,
        " IS NULL THEN 1 ELSE 0 END) AS n_missing FROM ", from_clause)
      counts <- .executeQuery(handle, .renderSql(handle, sql))
      total_unit <- counts$n_total[1]
      n_missing <- counts$n_missing[1]
    }
    n_value <- total_unit - n_missing
    groups_safe <- (n_missing == 0 || n_missing >= settings$nfilter_tab) &&
      (n_value == 0 || n_value >= settings$nfilter_tab)
    total_banded <- .bandCount(total_unit, settings$nfilter_band)
    missing_banded <- .bandCount(n_missing, settings$nfilter_band)
    rate <- NA_real_
    if (groups_safe && total_banded > 0 &&
        (n_missing == 0 || missing_banded > 0)) {
      rate <- round(missing_banded / total_banded, 2)
    }
    results <- rbind(results, data.frame(
      column_name = col,
      missing_rate = rate,
      stringsAsFactors = FALSE
    ))
  }

  results
}

#' Get value counts for a column (with suppression)
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param column Character; column name
#' @param top_n Integer; number of top values to return
#' @param suppress_small Logical; suppress counts below nfilter.tab
#' @param concept_id Integer or NULL; restrict to rows of this concept
#' @param cohort_table Character; cohort temp table name to scope the
#'   population (INNER JOIN on subject_id), or NULL.
#' @return Data frame with value and count columns
#' @keywords internal
.profileValueCounts <- function(handle, table, column, top_n = 20,
                                 suppress_small = TRUE, concept_id = NULL,
                                 concept_col = NULL, cohort_table = NULL) {
  table <- tolower(.validateIdentifier(table, "table"))
  column <- tolower(.validateIdentifier(column, "column"))
  bp <- .buildBlueprint(handle)

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)

  col_df <- bp$columns[[table]]
  has_person <- .profilerHasReviewedPersonScope(
    bp, table, cohort_table = cohort_table
  )
  column_info <- .profilerColumnInfo(bp, table, column)
  if (isTRUE(column_info$is_numeric_measure)) {
    stop("Column '", column, "' is continuous; use a protected numeric range, ",
         "histogram, quantile, or cutpoint profiler instead of value counts.",
         call. = FALSE)
  }

  qualified <- tbl_row$qualified_name[1]

  # FROM + optional cohort scope (INNER JOIN on subject_id, as in prevalence).
  from_clause <- paste0(qualified, " AS t")
  if (!is.null(cohort_table) && has_person) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN (SELECT DISTINCT subject_id FROM ",
                          cohort_table, ") AS coh",
                          " ON t.person_id = coh.subject_id")
  }

  # Optional concept scope: restrict every query to one concept of this table.
  # concept_col defaults to the domain concept but may override to scope by
  # unit_concept_id / *_type_concept_id / value_as_concept_id.
  concept_filter <- NULL
  if (!is.null(concept_id)) {
    ccol <- .resolveConceptScopeColumn(bp, table, concept_col)
    if (is.null(ccol)) {
      stop("Table '", table, "' has no concept column to scope by.",
           call. = FALSE)
    }
    concept_filter <- paste0(" AND t.", ccol, " = ", as.integer(concept_id))
  }
  concept_clause <- concept_filter %||% ""

  # Distinct-person gate over the SCOPED population (fail-closed) for
  # person-bearing tables, so a too-small (e.g. tightly cohort-scoped) population
  # cannot leak its value distribution at all.
  if (has_person) {
    persons_sql <- paste0(
      "SELECT COUNT(DISTINCT t.person_id) AS n FROM ", from_clause,
      " WHERE t.", column, " IS NOT NULL", concept_clause)
    n_persons_scoped <- .executeQuery(handle, .renderSql(handle, persons_sql))$n[1]
    .assertMinPersons(n_persons = n_persons_scoped)
  }

  n_total_sql <- paste0("SELECT COUNT(*) AS n FROM ", from_clause,
                        " WHERE t.", column, " IS NOT NULL", concept_clause)
  n_total <- .executeQuery(handle, .renderSql(handle, n_total_sql))$n[1]

  n_levels_sql <- paste0(
    "SELECT COUNT(DISTINCT t.", column, ") AS n FROM ", from_clause,
    " WHERE t.", column, " IS NOT NULL", concept_clause
  )
  n_levels <- .executeQuery(handle, .renderSql(handle, n_levels_sql))$n[1]

  .assertSafeLevels(n_levels, n_total)

  # For person-bearing tables, compute the number of DISTINCT persons behind
  # each value and suppress on THAT, not on the record count: a value backed by
  # many records but only one or two individuals is disclosive and must be
  # dropped. The record count (n) is retained as a separate column.
  effective_limit <- min(as.integer(top_n), 500L)
  sql <- paste0(
    "SELECT CAST(t.", column, " AS VARCHAR) AS value, ",
    "COUNT(*) AS n",
    if (has_person) ", COUNT(DISTINCT t.person_id) AS n_persons " else " ",
    "FROM ", from_clause, " ",
    "WHERE t.", column, " IS NOT NULL", concept_clause, " ",
    "GROUP BY t.", column
  )

  # The level gate above bounds this aggregate, so fetch all admissible groups.
  # Selecting TOP/LIMIT on the exact COUNT(*) would expose within-band rank when
  # callers vary top_n. Suppress first, then rank only on the released count band
  # with the public categorical value as deterministic tie-breaker.
  translated <- .renderSql(handle, sql)
  result <- .withDbReconnect(handle, function(conn) DBI::dbGetQuery(conn, translated))
  names(result) <- tolower(names(result))
  result <- .coerce_integer64(result)

  if (suppress_small) {
    # Person-based suppression for person-bearing tables (fail-closed row-drop
    # on distinct persons); record-count suppression only for tables that have
    # no person_id to count.
    result <- .suppressSmallCounts(result,
                                   if (has_person) "n_persons" else "n")
  }
  result <- .omopBandedTopN(
    result, support_cols = "n", top_n = effective_limit, key_cols = "value"
  )

  # Band the surviving record/person counts at the return boundary so the exact
  # per-value count (a differencing primitive) is never released. Suppression
  # above (which drops rows on the EXACT count) and the level/person gates have
  # already run on exact values.
  band_width <- .omopDisclosureSettings()$nfilter_band
  if (nrow(result) > 0) {
    if ("n" %in% names(result)) {
      result$n <- vapply(result$n, .bandCount, numeric(1), band_width = band_width)
    }
    if ("n_persons" %in% names(result)) {
      result$n_persons <- vapply(result$n_persons, .bandCount, numeric(1),
                                 band_width = band_width)
    }
  }

  # Decorate categorical concept VALUES with human-readable names, so the row
  # values themselves are translated (e.g. 8532 -> "Female"), not just labelled
  # by column. Mirrors the prevalence path (.profileConceptPrevalence).
  if (nrow(result) > 0 &&
      (grepl("_concept_id$", column) || identical(column, "value_as_concept_id"))) {
    ids <- suppressWarnings(as.integer(result$value))
    concepts <- tryCatch(.vocabLookupConcepts(handle, ids[!is.na(ids)]),
                         error = function(e) NULL)
    if (!is.null(concepts) && nrow(concepts) > 0) {
      cmap <- stats::setNames(concepts$concept_name,
                              as.character(concepts$concept_id))
      result$concept_name <- unname(cmap[as.character(result$value)])
      miss <- is.na(result$concept_name)
      result$concept_name[miss] <- paste0("concept_", result$value[miss])
    }
  }

  result
}

# --- Safe Numeric Cutpoints ---

#' Resolve a public, server-configured numeric grid
#'
#' Numeric boundaries must not be estimated from the protected data. Server
#' administrators configure public grids through `dsomop.safe_numeric_grids`
#' (or its `default.*` fallback). Each entry is a list with `table`, `column`,
#' optional `concept_id`/`concept_col`, finite `lower`/`upper`, strictly
#' increasing `breaks` spanning that range, and `clipping = "winsorize"`.
#' Multiple entries for one scope are allowed when they have different numbers
#' of bins. The requested `n_bins` must match one entry exactly.
#'
#' @param domain_concept_col The table's resolved domain concept column. A
#'   configured `concept_col = NULL` means this column, never a wildcard.
#' @keywords internal
.configuredSafeNumericGrid <- function(table, column, concept_id, concept_col,
                                       n_bins, domain_concept_col = NULL) {
  configs <- getOption(
    "dsomop.safe_numeric_grids",
    getOption("default.dsomop.safe_numeric_grids", list())
  )
  if (!is.list(configs) || length(configs) == 0L) {
    stop("Safe cutpoints are disabled for this scope: the server administrator ",
         "must configure a public numeric grid.", call. = FALSE)
  }
  if (all(c("table", "column", "breaks") %in% names(configs))) {
    configs <- list(configs)
  }

  normalize <- function(entry) {
    fail <- function() {
      stop("Invalid server option 'dsomop.safe_numeric_grids': each entry must ",
           "define one finite public grid and explicit winsorizing range.",
           call. = FALSE)
    }
    if (!is.list(entry) || is.null(names(entry)) || anyNA(names(entry)) ||
        any(!nzchar(names(entry))) || anyDuplicated(names(entry))) fail()
    allowed <- c("table", "column", "concept_id", "concept_col", "lower",
                 "upper", "breaks", "clipping")
    required <- c("table", "column", "lower", "upper", "breaks", "clipping")
    if (length(setdiff(names(entry), allowed)) > 0L ||
        !all(required %in% names(entry))) fail()

    entry_table <- tryCatch(
      tolower(.validateIdentifier(entry$table, "safe-grid table")),
      error = function(e) fail()
    )
    entry_column <- tryCatch(
      tolower(.validateIdentifier(entry$column, "safe-grid column")),
      error = function(e) fail()
    )
    entry_concept_col <- entry$concept_col %||% NULL
    if (!is.null(entry_concept_col)) {
      entry_concept_col <- tryCatch(
        tolower(.validateIdentifier(entry_concept_col,
                                    "safe-grid concept column")),
        error = function(e) fail()
      )
    }
    entry_concept_id <- entry$concept_id %||% NULL
    if (!is.null(entry_concept_id)) {
      concept_num <- suppressWarnings(as.numeric(entry_concept_id))
      concept_int <- suppressWarnings(as.integer(entry_concept_id))
      if (length(entry_concept_id) != 1L || length(concept_num) != 1L ||
          !is.finite(concept_num) || length(concept_int) != 1L ||
          is.na(concept_int) || concept_num != concept_int) fail()
      entry_concept_id <- concept_int
    }

    lower <- suppressWarnings(as.numeric(entry$lower))
    upper <- suppressWarnings(as.numeric(entry$upper))
    if (!is.numeric(entry$breaks) || length(lower) != 1L ||
        length(upper) != 1L || !is.finite(lower) || !is.finite(upper) ||
        lower >= upper) fail()
    breaks <- as.numeric(entry$breaks)
    if (length(breaks) < 3L || any(!is.finite(breaks)) ||
        any(diff(breaks) <= 0)) fail()
    near <- function(x, y) {
      abs(x - y) <= 1e-12 * max(1, abs(x), abs(y))
    }
    if (!near(breaks[1], lower) ||
        !near(breaks[length(breaks)], upper)) fail()
    if (!is.character(entry$clipping) || length(entry$clipping) != 1L ||
        !identical(tolower(entry$clipping), "winsorize")) fail()

    list(
      table = entry_table, column = entry_column,
      concept_id = entry_concept_id, concept_col = entry_concept_col,
      lower = lower, upper = upper, breaks = breaks,
      clipping = "winsorize", n_bins = length(breaks) - 1L
    )
  }

  normalized <- lapply(configs, normalize)
  same_nullable <- function(x, y) {
    if (is.null(x) && is.null(y)) return(TRUE)
    if (is.null(x) || is.null(y)) return(FALSE)
    identical(x, y)
  }
  matches <- vapply(normalized, function(entry) {
    entry_concept_col <- entry$concept_col
    if (!is.null(entry$concept_id) && is.null(entry_concept_col)) {
      entry_concept_col <- domain_concept_col
    }
    identical(entry$table, table) && identical(entry$column, column) &&
      identical(entry$n_bins, n_bins) &&
      same_nullable(entry$concept_id, concept_id) &&
      same_nullable(entry_concept_col, concept_col)
  }, logical(1))
  if (sum(matches) != 1L) {
    stop(if (any(matches)) {
      "Invalid server option 'dsomop.safe_numeric_grids': the requested scope is ambiguous."
    } else {
      "Safe cutpoints are disabled for this scope and n_bins: the server administrator must configure an exact public numeric grid."
    }, call. = FALSE)
  }
  normalized[[which(matches)]]
}

#' Remember a server-issued numeric-bin contract for this resource session
#'
#' Client-provided numeric edges are not self-authenticating. Keeping the
#' reviewed edges in the handle lets extraction validate that a later
#' `value_bin` really came from this server, for the same table/column/scope,
#' rather than trusting a forgeable client list.
#'
#' @param grid Public grid metadata retained for auditability.
#' @keywords internal
.rememberSafeNumericBins <- function(handle, scope, breaks, grid = NULL) {
  allowed_scope <- c("table", "column", "concept_id", "concept_col", "n_bins")
  if (!is.list(scope) || is.null(names(scope)) || anyNA(names(scope)) ||
      any(!nzchar(names(scope))) || anyDuplicated(names(scope)) ||
      length(setdiff(names(scope), allowed_scope)) > 0L ||
      !all(c("table", "column", "n_bins") %in% names(scope))) {
    stop("Invalid internal safe numeric-bin contract.", call. = FALSE)
  }
  n_bins_numeric <- suppressWarnings(as.numeric(scope$n_bins))
  n_bins <- suppressWarnings(as.integer(scope$n_bins))
  if (length(n_bins_numeric) != 1L || !is.finite(n_bins_numeric) ||
      length(n_bins) != 1L || is.na(n_bins) || n_bins_numeric != n_bins ||
      n_bins < 2L || n_bins > 100L ||
      !is.numeric(breaks) || length(breaks) != n_bins + 1L ||
      any(!is.finite(breaks)) || any(diff(breaks) <= 0)) {
    stop("Invalid internal safe numeric-bin contract.", call. = FALSE)
  }
  ttl <- suppressWarnings(as.numeric(
    getOption("dsomop.safe_bin_ttl_seconds", 900)
  ))
  if (length(ttl) != 1L || is.na(ttl) || !is.finite(ttl) || ttl <= 0) {
    stop("dsomop.safe_bin_ttl_seconds must be one positive finite number.",
         call. = FALSE)
  }
  now <- as.numeric(Sys.time())
  cache <- handle$safe_numeric_bins %||% list()
  if (length(cache) > 0L) {
    fresh <- vapply(cache, function(x) {
      is.list(x) && is.numeric(x$expires_at) && length(x$expires_at) == 1L &&
        is.finite(x$expires_at) && x$expires_at > now
    }, logical(1))
    cache <- cache[fresh]
  }
  entry <- c(scope, list(
    breaks = as.numeric(breaks),
    grid = grid,
    expires_at = now + ttl
  ))
  key <- paste0("bin_", paste0(format(openssl::rand_bytes(12L)), collapse = ""))
  cache[[key]] <- entry
  # Bound per-handle state even if an analyst requests many cutpoint variants.
  if (length(cache) > 128L) cache <- utils::tail(cache, 128L)
  handle$safe_numeric_bins <- cache
  invisible(scope)
}

#' Compute safe histogram bin edges for a numeric column
#'
#' Returns a public, server-configured grid over one mean value per person.
#' Contributions outside its declared range are winsorized to the nearest
#' endpoint. No edge is estimated from protected values. The complete grid is
#' released only when every bin contains at least \code{nfilter.tab} persons;
#' counts are banded only after that exact internal gate.
#'
#' @section Server configuration:
#' `dsomop.safe_numeric_grids` must contain an exact entry for the requested
#' table, column, concept scope and number of bins. Its range and breaks must be
#' fixed from public clinical knowledge or governance policy before inspecting
#' the protected data. Configuring observed minima, maxima or quantiles as a
#' supposedly public grid would defeat this contract. Winsorization bounds each
#' person's contribution for bin support; it does not modify the source table.
#' Count banding and `nfilter.noise` are not claimed as differential privacy.
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param column Character; numeric column name
#' @param concept_id Integer or NULL; concept filter
#' @param n_bins Integer; target number of bins (default 10)
#' @param concept_col Character or NULL; reviewed concept column used to scope
#'   `concept_id`. The table's domain concept column is used by default.
#' @return List with public breaks, banded person counts, the session scope
#'   contract, and public grid/clipping metadata.
#' @keywords internal
.profileSafeCutpoints <- function(handle, table, column, concept_id = NULL,
                                   n_bins = 10L, concept_col = NULL) {
  table <- tolower(.validateIdentifier(table, "table"))
  column <- tolower(.validateIdentifier(column, "column"))
  bp <- .buildBlueprint(handle)
  settings <- .omopDisclosureSettings()

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)

  qualified <- tbl_row$qualified_name[1]
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name
  has_person <- .profilerHasReviewedPersonScope(bp, table)

  .profilerColumnInfo(bp, table, column, require_numeric = TRUE)

  n_bins_numeric <- suppressWarnings(as.numeric(n_bins))
  n_bins_integer <- suppressWarnings(as.integer(n_bins))
  if (length(n_bins) != 1L || length(n_bins_numeric) != 1L ||
      !is.finite(n_bins_numeric) || length(n_bins_integer) != 1L ||
      is.na(n_bins_integer) || n_bins_numeric != n_bins_integer ||
      n_bins_integer < 2L || n_bins_integer > 100L) {
    stop("n_bins must be one integer between 2 and 100.", call. = FALSE)
  }
  n_bins <- n_bins_integer

  if (!has_person) {
    stop("Safe cutpoints require a person-bearing OMOP table.", call. = FALSE)
  }

  # Build WHERE clauses. concept_col defaults to the domain concept but may
  # override to scope by unit_concept_id / *_type_concept_id / value_as_concept_id.
  where_parts <- paste0("t.", column, " IS NOT NULL")
  scope_col <- NULL
  if (!is.null(concept_id)) {
    concept_num <- suppressWarnings(as.numeric(concept_id))
    concept_int <- suppressWarnings(as.integer(concept_id))
    if (length(concept_id) != 1L || length(concept_num) != 1L ||
        !is.finite(concept_num) || length(concept_int) != 1L ||
        is.na(concept_int) || concept_num != concept_int) {
      stop("concept_id must be one finite integer.", call. = FALSE)
    }
    concept_id <- concept_int
    scope_col <- .resolveConceptScopeColumn(bp, table, concept_col)
    if (is.null(scope_col)) {
      stop("Table '", table,
           "' has no reviewed concept column for concept_id scoping.",
           call. = FALSE)
    }
    where_parts <- c(where_parts,
                     paste0("t.", scope_col, " = ", concept_id))
  } else if (!is.null(concept_col)) {
    stop("concept_col requires concept_id.", call. = FALSE)
  }
  where_sql <- paste0(" WHERE ", paste(where_parts, collapse = " AND "))
  contract <- list(
    table = table,
    column = column,
    concept_id = concept_id,
    concept_col = scope_col,
    n_bins = n_bins
  )

  # Collapse longitudinal records before computing the distribution. This gives
  # every person exactly one contribution, so a patient with many measurements
  # cannot make a configured grid cell appear well supported by repetition.
  person_value_sql <- paste0(
    "SELECT t.person_id AS person_id, ",
    "AVG(CAST(t.", column, " AS REAL)) AS value ",
    "FROM ", qualified, " AS t", where_sql,
    " GROUP BY t.person_id"
  )
  count_sql <- paste0("SELECT COUNT(*) AS n FROM (", person_value_sql,
                      ") AS person_values")
  n_total <- .executeQuery(handle, .renderSql(handle, count_sql))$n[1]
  .assertMinPersons(n_persons = n_total)

  if (is.na(n_total) || n_total < as.integer(settings$nfilter_dist %||% 10L)) {
    stop("Disclosive: operation blocked - insufficient individuals for ",
         "a numeric distribution.", call. = FALSE)
  }

  # The released edges are selected exclusively from server-owned public
  # configuration. `nfilter.noise` is not a DP mechanism and is intentionally
  # not used to sanitize data-derived order statistics here.
  grid <- .configuredSafeNumericGrid(
    table, column, concept_id, scope_col, n_bins,
    domain_concept_col = .getDomainConceptColumn(bp, table)
  )
  breaks <- grid$breaks
  lower_literal <- .quoteLiteral(grid$lower, handle)
  upper_literal <- .quoteLiteral(grid$upper, handle)
  clipped_value_sql <- paste0(
    "SELECT raw_values.person_id AS person_id, CASE ",
    "WHEN raw_values.value < ", lower_literal, " THEN ", lower_literal, " ",
    "WHEN raw_values.value > ", upper_literal, " THEN ", upper_literal, " ",
    "ELSE raw_values.value END AS value FROM (", person_value_sql,
    ") AS raw_values"
  )

  # Compute counts per bin
  n_result_bins <- length(breaks) - 1L
  counts <- numeric(n_result_bins)

  for (i in seq_len(n_result_bins)) {
    lo <- breaks[i]
    hi <- breaks[i + 1L]
    op <- if (i == n_result_bins) " <= " else " < "
    bin_sql <- paste0(
      "SELECT COUNT(*) AS n FROM (", clipped_value_sql, ") AS person_values",
      " WHERE person_values.value >= ", .quoteLiteral(lo, handle),
      " AND person_values.value", op, .quoteLiteral(hi, handle)
    )
    cnt <- tryCatch(.executeQuery(handle, .renderSql(handle, bin_sql))$n[1],
                    error = function(e) NA_real_)
    counts[i] <- as.numeric(cnt)
  }

  # Never merge or omit bins based on protected counts: that would make the
  # returned edge set itself data-dependent. Fail closed unless the complete
  # configured grid is supported.
  min_cell <- settings$nfilter_tab
  if (anyNA(counts) || sum(counts) != as.numeric(n_total) ||
      any(counts < min_cell)) {
    stop("Disclosive: configured public numeric grid is not supported by ",
         "enough individuals in every bin.", call. = FALSE)
  }
  counts <- vapply(counts, .bandCount, numeric(1),
                   band_width = settings$nfilter_band)

  grid_metadata <- list(
    lower = grid$lower, upper = grid$upper,
    clipping = grid$clipping, source = "server_configured_public_grid"
  )
  .rememberSafeNumericBins(handle, contract, breaks, grid = grid_metadata)
  list(breaks = breaks, counts = counts, contract = contract,
       grid = grid_metadata)
}

# --- Exploration Profiling ---

#' Build a dialect-aware LIMIT ... OFFSET ... suffix for paginated reads
#'
#' The package's bespoke \code{.translate_top} converts \code{SELECT TOP n} to a
#' trailing \code{LIMIT n}, which cannot express an OFFSET (and would emit an
#' invalid \code{OFFSET m LIMIT n} ordering if one were spliced in earlier). For
#' the paginated prevalence path we therefore bypass \code{TOP} and append the
#' page window ourselves AFTER rendering. Only \code{limit}/\code{offset} are
#' interpolated and both are coerced to non-negative integers, so this never
#' carries user text into SQL.
#'
#' @param dialect Character; \code{handle$target_dialect}.
#' @param limit Integer; page size.
#' @param offset Integer; rows to skip.
#' @return Character SQL suffix (leading space included).
#' @keywords internal
.paginationClause <- function(dialect, limit, offset) {
  limit <- max(as.integer(limit), 0L)
  offset <- max(as.integer(offset), 0L)
  if (identical(dialect, "sql server") || identical(dialect, "oracle")) {
    # ANSI offset-fetch (SQL Server 2012+, Oracle 12c+). Requires an ORDER BY,
    # which the prevalence query always supplies.
    return(paste0(" OFFSET ", offset, " ROWS FETCH NEXT ", limit,
                  " ROWS ONLY"))
  }
  # sqlite / postgresql / redshift / spark / mysql / bigquery: LIMIT n OFFSET m.
  paste0(" LIMIT ", limit, " OFFSET ", offset)
}

#' Aggregate concept prevalence for ONE clinical table (engine)
#'
#' Shared core used by both single-table and GLOBAL prevalence. Returns the raw,
#' un-decorated aggregate (concept_id, n_persons?, n_records) for one table with
#' the page window applied, plus a \code{source_table} tag. It performs no concept
#' decoration or final count banding; the caller owns those so a global run does
#' them once over the merged set. Small cells are excluded in SQL
#' before pagination, and pages are ranked on the release band with concept_id as
#' the sole tie-breaker. The per-table population gate (\code{.assertMinPersons}
#' on the table's distinct persons) still runs here so a too-small table never
#' contributes rows.
#'
#' @keywords internal
.prevalenceOneTable <- function(handle, bp, table, concept_col, metric,
                                 limit, offset, cohort_table, window,
                                 gate = TRUE) {
  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) return(NULL)
  qualified <- tbl_row$qualified_name[1]
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name
  has_person <- .profilerHasReviewedPersonScope(
    bp, table, cohort_table = cohort_table
  )

  if (is.null(concept_col)) {
    concept_col <- .getDomainConceptColumn(bp, table)
    if (is.null(concept_col)) return(NULL)
  }
  if (!concept_col %in% tbl_cols) return(NULL)

  # FROM / cohort join / window (same shape as the legacy single-table path).
  from_clause <- paste0(qualified, " AS t")
  if (!is.null(cohort_table) && has_person) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN (SELECT DISTINCT subject_id FROM ",
                          cohort_table, ") AS coh",
                          " ON t.person_id = coh.subject_id")
  }
  where_parts <- character(0)
  if (!is.null(window)) {
    if (!is.list(window)) {
      stop("window must be a list with optional start/end dates.", call. = FALSE)
    }
    window <- .validateDateBounds(window$start, window$end,
                                  "profiling window")
    date_col <- .getDateColumn(bp, table)
    if (!is.null(date_col)) {
      if (!is.null(window$start)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " >= ",
                                .quoteLiteral(window$start, handle)))
      }
      if (!is.null(window$end)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " <= ",
                                .quoteLiteral(window$end, handle)))
      }
    }
  }
  where_sql <- if (length(where_parts) > 0) {
    paste0(" WHERE ", paste(where_parts, collapse = " AND "))
  } else ""

  # Per-table population gate on the SCOPED population (fail-closed).
  if (gate && has_person) {
    pc_sql <- paste0("SELECT COUNT(DISTINCT t.person_id) AS n FROM ",
                     from_clause, where_sql)
    n_total_persons <- .executeQuery(handle, .renderSql(handle, pc_sql))$n[1]
    .assertMinPersons(n_persons = n_total_persons)
  }

  settings <- .omopDisclosureSettings()
  order_col <- if (metric == "persons") "n_persons" else "n_records"
  person_count_expr <- "COUNT(DISTINCT t.person_id)"
  record_count_expr <- "COUNT(*)"
  if (has_person) {
    select_expr <- paste0(
      "SELECT t.", concept_col, " AS concept_id, ",
      person_count_expr, " AS n_persons, ",
      record_count_expr, " AS n_records")
  } else {
    order_col <- "n_records"
    select_expr <- paste0(
      "SELECT t.", concept_col, " AS concept_id, ",
      record_count_expr, " AS n_records")
  }
  support_expr <- if (order_col == "n_persons") {
    person_count_expr
  } else {
    record_count_expr
  }
  banded_support_expr <- paste0(
    "FLOOR((", support_expr, ") / ", as.integer(settings$nfilter_band), ".0)"
  )
  having_sql <- if (has_person) {
    paste0(" HAVING ", person_count_expr, " >= ",
           as.integer(settings$nfilter_tab), " AND ", record_count_expr,
           " >= ", as.integer(settings$nfilter_tab))
  } else {
    paste0(" HAVING ", record_count_expr, " >= ",
           as.integer(settings$nfilter_tab))
  }

  sql <- paste0(
    select_expr,
    " FROM ", from_clause,
    where_sql,
    " GROUP BY t.", concept_col,
    having_sql,
    " ORDER BY ", banded_support_expr, " DESC, ",
    "CASE WHEN t.", concept_col, " IS NULL THEN 1 ELSE 0 END ASC, ",
    "t.", concept_col, " ASC")

  # Pagination is applied AFTER suppression and band-based ordering. Therefore
  # top_n/offset cannot reveal the exact order of concepts within one count band.
  translated <- paste0(.renderSql(handle, sql),
                       .paginationClause(handle$target_dialect, limit, offset))
  result <- .withDbReconnect(handle, function(conn) DBI::dbGetQuery(conn, translated))
  names(result) <- tolower(names(result))
  result <- .coerce_integer64(result)
  if (nrow(result) == 0) return(NULL)
  if (!"n_persons" %in% names(result)) result$n_persons <- NA_real_
  result$source_table <- table
  result
}

#' Get top concepts in a table by person count or record count
#'
#' @param handle CDM handle
#' @param table Character; table name. Ignored when \code{global = TRUE}.
#' @param concept_col Character; concept column name (NULL = auto-detect)
#' @param metric Character; "persons" or "records"
#' @param top_n Integer; page size (number of top concepts to return)
#' @param cohort_table Character; cohort temp table name for filtering (NULL)
#' @param window List with start and end dates for filtering (NULL)
#' @param offset Integer; number of leading concepts to skip (pagination). The
#'   page is \code{[offset+1 .. offset+top_n]} of the descending-prevalence
#'   ranking; lift the legacy 500-row hard cap by walking pages.
#' @param global Logical; when TRUE, rank concepts across ALL clinical tables
#'   (every table with a domain concept column), person-gated per table and
#'   suppressed over the merged set, rather than a single table.
#' @return Data frame with concept_id, concept_name, n_persons, n_records (plus
#'   source_table when \code{global = TRUE}).
#' @keywords internal
.profileConceptPrevalence <- function(handle, table, concept_col = NULL,
                                       metric = "persons", top_n = 50L,
                                       cohort_table = NULL, window = NULL,
                                       offset = 0L, global = FALSE) {
  bp <- .buildBlueprint(handle)
  settings <- .omopDisclosureSettings()
  metric <- match.arg(metric, c("persons", "records"))
  effective_top_n <- min(as.integer(top_n), 500L)
  offset <- max(as.integer(offset %||% 0L), 0L)

  if (!is.null(cohort_table)) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
  }

  # --- GLOBAL mode: rank across all clinical (person-bearing CDM) tables ------
  # Each table is gated and paged independently, then the union is re-ranked and
  # the requested page is taken. To make the global page correct we must pull
  # enough rows per table to cover offset+top_n of the merged ranking, so we read
  # the top (offset+top_n) of EACH table (still bounded), merge, re-rank, slice.
  if (isTRUE(global)) {
    clinical <- bp$tables[bp$tables$present_in_db & bp$tables$has_person_id &
                            bp$tables$schema_category == "CDM", , drop = FALSE]
    page_each <- min(offset + effective_top_n, 500L)
    parts <- list()
    for (tn in clinical$table_name) {
      cc <- .getDomainConceptColumn(bp, tn)
      if (is.null(cc)) next
      one <- tryCatch(
        .prevalenceOneTable(handle, bp, tn, cc, metric,
                            limit = page_each, offset = 0L,
                            cohort_table = cohort_table, window = window,
                            gate = TRUE),
        error = function(e) NULL)  # a too-small table is omitted, not fatal
      if (!is.null(one)) parts[[tn]] <- one
    }
    if (length(parts) == 0) {
      return(data.frame(concept_id = integer(0), concept_name = character(0),
                        n_persons = numeric(0), n_records = numeric(0),
                        source_table = character(0), stringsAsFactors = FALSE))
    }
    result <- do.call(rbind, parts)
    # Suppress small cells over the MERGED set (drops rows), then re-rank + page.
    result <- .suppressSmallCounts(result, c("n_persons", "n_records"))
    if (nrow(result) == 0) {
      return(data.frame(concept_id = integer(0), concept_name = character(0),
                        n_persons = numeric(0), n_records = numeric(0),
                        source_table = character(0), stringsAsFactors = FALSE))
    }
    ord_col <- if (metric == "persons") "n_persons" else "n_records"
    result <- .omopBandedTopN(
      result, support_cols = ord_col,
      top_n = min(nrow(result), offset + effective_top_n),
      key_cols = c("source_table", "concept_id")
    )
    take <- seq_len(min(nrow(result), effective_top_n)) + offset
    take <- take[take <= nrow(result)]
    result <- result[take, , drop = FALSE]
    rownames(result) <- NULL
    return(.decoratePrevalence(handle, result, include_source = TRUE))
  }

  # --- Single-table mode -----------------------------------------------------
  table <- tolower(.validateIdentifier(table, "table"))
  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name

  concept_col <- .resolveConceptScopeColumn(bp, table, concept_col)
  if (is.null(concept_col)) {
    stop("No releasable concept column found for table '", table,
         "'.", call. = FALSE)
  }

  result <- .prevalenceOneTable(handle, bp, table, concept_col, metric,
                                limit = effective_top_n, offset = offset,
                                cohort_table = cohort_table, window = window,
                                gate = TRUE)
  if (is.null(result) || nrow(result) == 0) {
    return(data.frame(concept_id = integer(0), concept_name = character(0),
                      n_persons = numeric(0), n_records = numeric(0),
                      stringsAsFactors = FALSE))
  }
  result$source_table <- NULL

  # Suppress small counts (drops rows)
  result <- .suppressSmallCounts(result, c("n_persons", "n_records"))

  .decoratePrevalence(handle, result, include_source = FALSE)
}

#' Decorate a prevalence aggregate with concept names + fix column order
#'
#' Shared tail of \code{\link{.profileConceptPrevalence}} for both single-table
#' and global modes: looks up human-readable concept names from the vocabulary
#' and returns the canonical column order. \code{include_source} keeps the
#' \code{source_table} column for global runs.
#'
#' @keywords internal
.decoratePrevalence <- function(handle, result, include_source = FALSE) {
  if (is.null(result) || nrow(result) == 0) {
    base <- data.frame(concept_id = integer(0), concept_name = character(0),
                       n_persons = numeric(0), n_records = numeric(0),
                       stringsAsFactors = FALSE)
    if (include_source) base$source_table <- character(0)
    return(base)
  }
  concept_ids <- result$concept_id[!is.na(result$concept_id)]
  if (length(concept_ids) > 0) {
    concepts <- tryCatch(
      .vocabLookupConcepts(handle, concept_ids),
      error = function(e) data.frame(concept_id = integer(0),
                                      concept_name = character(0),
                                      stringsAsFactors = FALSE)
    )
    if (nrow(concepts) > 0) {
      concept_map <- stats::setNames(concepts$concept_name,
                                      as.character(concepts$concept_id))
      result$concept_name <- concept_map[as.character(result$concept_id)]
      result$concept_name[is.na(result$concept_name)] <- ""
    } else {
      result$concept_name <- ""
    }
  } else {
    result$concept_name <- ""
  }

  out_cols <- intersect(
    c("concept_id", "concept_name", "n_persons", "n_records",
      if (include_source) "source_table"),
    names(result))
  out <- result[, out_cols, drop = FALSE]
  rownames(out) <- NULL

  # Band the surviving per-concept counts at the return boundary (shared tail of
  # both single-table and global prevalence). Small-cell suppression has already
  # dropped rows on the EXACT counts; banding only the reported numbers destroys
  # the 1-person resolution a differencing attack would read off the funnel.
  band_width <- .omopDisclosureSettings()$nfilter_band
  for (cc in intersect(c("n_persons", "n_records"), names(out))) {
    out[[cc]] <- vapply(out[[cc]], .bandCount, numeric(1), band_width = band_width)
  }
  out
}

#' Get the numeric range (p05/p95) for a column
#'
#' Returns the 5th and 95th percentile approximations and total count.
#' Used as pass 1 of two-pass histogram pooling.
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param cohort_table Character; cohort temp table name (NULL)
#' @param window List with start/end dates (NULL)
#' @param concept_id Integer or NULL; optional concept scope.
#' @param concept_col Character or NULL; concept column for that scope.
#' @param unit Distribution unit. \code{"person"} (default) contributes one
#'   within-person mean; \code{"record"} is allowed only for a 1:1 relation.
#' @return List with p05, p95, n_total
#' @keywords internal
.profileNumericRange <- function(handle, table, value_col,
                                  cohort_table = NULL, window = NULL,
                                  concept_id = NULL, concept_col = NULL,
                                  unit = "person") {
  table <- tolower(.validateIdentifier(table, "table"))
  value_col <- tolower(.validateIdentifier(value_col, "column"))
  bp <- .buildBlueprint(handle)

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)

  qualified <- tbl_row$qualified_name[1]
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name
  has_person <- .profilerHasReviewedPersonScope(
    bp, table, cohort_table = cohort_table
  )

  .profilerColumnInfo(bp, table, value_col, require_numeric = TRUE)

  from_clause <- paste0(qualified, " AS t")
  where_parts <- paste0("t.", value_col, " IS NOT NULL")

  if (!is.null(cohort_table) && has_person) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN (SELECT DISTINCT subject_id FROM ",
                          cohort_table, ") AS coh",
                          " ON t.person_id = coh.subject_id")
  }

  if (!is.null(window)) {
    if (!is.list(window)) {
      stop("window must be a list with optional start/end dates.", call. = FALSE)
    }
    window <- .validateDateBounds(window$start, window$end,
                                  "profiling window")
    date_col <- .getDateColumn(bp, table)
    if (!is.null(date_col)) {
      if (!is.null(window$start)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " >= ",
                                .quoteLiteral(window$start, handle)))
      }
      if (!is.null(window$end)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " <= ",
                                .quoteLiteral(window$end, handle)))
      }
    }
  }

  # Optional concept scope: restrict to one concept of this table. concept_col
  # defaults to the domain concept but may override to scope by unit_concept_id /
  # *_type_concept_id / value_as_concept_id.
  if (!is.null(concept_id)) {
    ccol <- .resolveConceptScopeColumn(bp, table, concept_col)
    if (is.null(ccol)) {
      stop("Table '", table, "' has no concept column to scope by.",
           call. = FALSE)
    }
    where_parts <- c(where_parts,
                     paste0("t.", ccol, " = ", as.integer(concept_id)))
  }

  where_sql <- paste0(" WHERE ", paste(where_parts, collapse = " AND "))

  # Fail-closed distinct-person gate over the scoped relation: a value range is
  # disclosive when it describes < nfilter_subset PEOPLE, regardless of how many
  # records they contribute (one person with many measurements must not leak).
  .assertNumericDistPersons(handle, from_clause, where_sql, tbl_cols)

  distribution_sql <- .numericDistributionSql(
    handle, from_clause, where_sql, value_col, tbl_cols, unit = unit)
  count_sql <- paste0("SELECT COUNT(*) AS n FROM (", distribution_sql,
                      ") AS distribution_values")
  n_total <- .executeQuery(handle, .renderSql(handle, count_sql))$n[1]

  if (is.na(n_total) || n_total == 0) {
    return(list(p05 = NA_real_, p95 = NA_real_, n_total = 0L))
  }

  # PERCENTILE LEAKAGE GUARD: With small samples, even clamped percentiles
  # (p05/p95) return values near min/max, identifying individuals at the
  # extremes of the distribution. E.g., with n=5, p05 is approximately the minimum.
  # nfilter_dist (default 10) ensures enough data points for safe estimation.
  settings <- .omopDisclosureSettings()
  nfilter_dist <- settings$nfilter_dist %||% 10L
  tail_min <- max(as.integer(settings$nfilter_tab),
                  as.integer(settings$nfilter_subset), 1L)
  if (n_total < max(nfilter_dist, 2L * tail_min)) {
    return(list(
      p05 = NA_real_, p95 = NA_real_,
      n_total = .bandCount(n_total, settings$nfilter_band)
    ))
  }

  offset_p05 <- .protectedQuantileOffset(n_total, 0.05, settings)
  offset_p95 <- .protectedQuantileOffset(n_total, 0.95, settings)

  ordered_sql <- paste0(
    "SELECT distribution_values.value AS val FROM (", distribution_sql,
    ") AS distribution_values ORDER BY distribution_values.value ASC"
  )
  ordered_sql <- .renderSql(handle, ordered_sql)
  p05_sql <- paste0(ordered_sql,
                    .paginationClause(handle$target_dialect, 1L, offset_p05))
  p95_sql <- paste0(ordered_sql,
                    .paginationClause(handle$target_dialect, 1L, offset_p95))

  p05_val <- tryCatch(.executeQuery(handle, p05_sql)$val[1], error = function(e) NA_real_)
  p95_val <- tryCatch(.executeQuery(handle, p95_sql)$val[1], error = function(e) NA_real_)

  list(
    p05 = p05_val, p95 = p95_val,
    n_total = .bandCount(n_total, settings$nfilter_band)
  )
}

#' Compute a safe histogram with suppressed low-count bins
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param bins Integer; number of bins
#' @param cohort_table Character; cohort temp table name (NULL)
#' @param window List with start/end dates (NULL)
#' @param breaks Numeric vector; shared bin edges from two-pass pooling (NULL = compute locally)
#' @param unit Distribution unit. \code{"person"} (default) contributes one
#'   within-person mean; \code{"record"} is allowed only for a 1:1 relation.
#' @return Data frame with bin_start, bin_end, count, suppressed
#' @keywords internal
.profileNumericHistogram <- function(handle, table, value_col,
                                      bins = 20L, cohort_table = NULL,
                                      window = NULL, breaks = NULL,
                                      concept_id = NULL, concept_col = NULL,
                                      unit = "person") {
  table <- tolower(.validateIdentifier(table, "table"))
  value_col <- tolower(.validateIdentifier(value_col, "column"))

  bins_numeric <- suppressWarnings(as.numeric(bins))
  bins_integer <- suppressWarnings(as.integer(bins))
  if (length(bins) != 1L || length(bins_numeric) != 1L ||
      !is.finite(bins_numeric) || length(bins_integer) != 1L ||
      is.na(bins_integer) || bins_numeric != bins_integer ||
      bins_integer < 2L || bins_integer > 200L) {
    stop("bins must be one integer between 2 and 200.", call. = FALSE)
  }
  bins <- bins_integer

  if (!is.null(breaks)) {
    raw_breaks <- unlist(breaks, use.names = FALSE)
    numeric_breaks <- suppressWarnings(as.numeric(raw_breaks))
    if (length(raw_breaks) < 2L || length(raw_breaks) > 201L ||
        length(numeric_breaks) != length(raw_breaks) ||
        any(!is.finite(numeric_breaks)) || any(diff(numeric_breaks) <= 0)) {
      stop("breaks must contain 2 to 201 finite, strictly increasing numeric ",
           "values.", call. = FALSE)
    }
    # From this point only validated numeric values reach SQL interpolation.
    breaks <- numeric_breaks
  }

  bp <- .buildBlueprint(handle)
  settings <- .omopDisclosureSettings()

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)

  qualified <- tbl_row$qualified_name[1]
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name
  has_person <- .profilerHasReviewedPersonScope(
    bp, table, cohort_table = cohort_table
  )

  .profilerColumnInfo(bp, table, value_col, require_numeric = TRUE)

  # Build FROM / WHERE clauses
  from_clause <- paste0(qualified, " AS t")
  where_parts <- paste0("t.", value_col, " IS NOT NULL")

  if (!is.null(cohort_table) && has_person) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN (SELECT DISTINCT subject_id FROM ",
                          cohort_table, ") AS coh",
                          " ON t.person_id = coh.subject_id")
  }

  if (!is.null(window)) {
    if (!is.list(window)) {
      stop("window must be a list with optional start/end dates.", call. = FALSE)
    }
    window <- .validateDateBounds(window$start, window$end,
                                  "profiling window")
    date_col <- .getDateColumn(bp, table)
    if (!is.null(date_col)) {
      if (!is.null(window$start)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " >= ",
                                .quoteLiteral(window$start, handle)))
      }
      if (!is.null(window$end)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " <= ",
                                .quoteLiteral(window$end, handle)))
      }
    }
  }

  # Optional concept scope: restrict to one concept of this table. concept_col
  # defaults to the domain concept but may override to scope by unit_concept_id /
  # *_type_concept_id / value_as_concept_id.
  if (!is.null(concept_id)) {
    ccol <- .resolveConceptScopeColumn(bp, table, concept_col)
    if (is.null(ccol)) {
      stop("Table '", table, "' has no concept column to scope by.",
           call. = FALSE)
    }
    where_parts <- c(where_parts,
                     paste0("t.", ccol, " = ", as.integer(concept_id)))
  }

  where_sql <- paste0(" WHERE ", paste(where_parts, collapse = " AND "))

  # Fail-closed distinct-person gate over the scoped relation: a histogram (its
  # bin counts AND bin edges) is disclosive when it describes < nfilter_subset
  # PEOPLE, no matter how many records they contribute.
  .assertNumericDistPersons(handle, from_clause, where_sql, tbl_cols)

  distribution_sql <- .numericDistributionSql(
    handle, from_clause, where_sql, value_col, tbl_cols, unit = unit)
  count_sql <- paste0("SELECT COUNT(*) AS n FROM (", distribution_sql,
                      ") AS distribution_values")
  n_total <- .executeQuery(handle, .renderSql(handle, count_sql))$n[1]

  if (is.na(n_total) || n_total == 0) {
    return(.dropSuppressed(data.frame(bin_start = numeric(0), bin_end = numeric(0),
                      count = integer(0), suppressed = logical(0),
                      stringsAsFactors = FALSE)))
  }

  # Use provided breaks (from two-pass pooling) or compute locally
  if (!is.null(breaks)) {
    # Shared breaks provided: use them directly
    bins <- length(breaks) - 1L
  } else {
    nfilter_dist <- settings$nfilter_dist %||% 10L
    tail_min <- max(as.integer(settings$nfilter_tab),
                    as.integer(settings$nfilter_subset), 1L)
    if (n_total < max(nfilter_dist, 2L * tail_min)) {
      stop("Disclosive: sample size too small for safe histogram edges. ",
           "Protected tails require more contributions.", call. = FALSE)
    }
    # Compute safe range using 5th and 95th percentile approximations
    offset_p05 <- .protectedQuantileOffset(n_total, 0.05, settings)
    offset_p95 <- .protectedQuantileOffset(n_total, 0.95, settings)

    ordered_sql <- paste0(
      "SELECT distribution_values.value AS val FROM (", distribution_sql,
      ") AS distribution_values ORDER BY distribution_values.value ASC"
    )
    ordered_sql <- .renderSql(handle, ordered_sql)
    p05_sql <- paste0(ordered_sql,
                      .paginationClause(handle$target_dialect, 1L, offset_p05))
    p95_sql <- paste0(ordered_sql,
                      .paginationClause(handle$target_dialect, 1L, offset_p95))

    p05_val <- tryCatch(.executeQuery(handle, p05_sql)$val[1], error = function(e) NA_real_)
    p95_val <- tryCatch(.executeQuery(handle, p95_sql)$val[1], error = function(e) NA_real_)

    if (is.na(p05_val) || is.na(p95_val) || p05_val == p95_val) {
      # Degenerate spread (>~90% identical values, or too few rows): a single
      # bin would have bin_start == bin_end == the exact value -- an exact-value
      # disclosure. Return an EMPTY histogram instead (matching the concept
      # drilldown path), so no zero-width bin carrying a real measurement leaks.
      return(.dropSuppressed(data.frame(
        bin_start = numeric(0),
        bin_end = numeric(0),
        count = integer(0),
        suppressed = logical(0),
        stringsAsFactors = FALSE
      )))
    }

    bin_width <- (p95_val - p05_val) / bins
    breaks <- seq(p05_val, p95_val, by = bin_width)
    if (length(breaks) < bins + 1L) {
      breaks <- c(breaks, p95_val)
    }
    breaks <- breaks[seq_len(bins + 1L)]
  }

  # Build CASE WHEN for each bin
  case_parts <- character(bins)
  for (i in seq_len(bins)) {
    lo <- breaks[i]
    hi <- breaks[i + 1L]
    if (i == bins) {
      # Last bin includes the upper bound
      case_parts[i] <- paste0(
        "SUM(CASE WHEN distribution_values.value >= ", lo,
        " AND distribution_values.value <= ", hi,
        " THEN 1 ELSE 0 END) AS bin_", i
      )
    } else {
      case_parts[i] <- paste0(
        "SUM(CASE WHEN distribution_values.value >= ", lo,
        " AND distribution_values.value < ", hi,
        " THEN 1 ELSE 0 END) AS bin_", i
      )
    }
  }

  bin_sql <- paste0(
    "SELECT ", paste(case_parts, collapse = ", "),
    " FROM (", distribution_sql, ") AS distribution_values"
  )

  bin_result <- .executeQuery(handle, .renderSql(handle, bin_sql))

  # Assemble result data frame
  result <- data.frame(
    bin_start = breaks[seq_len(bins)],
    bin_end = breaks[seq_len(bins) + 1L],
    count = integer(bins),
    suppressed = logical(bins),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(bins)) {
    col_name <- paste0("bin_", i)
    cnt <- if (col_name %in% names(bin_result)) bin_result[[col_name]][1] else 0L
    result$count[i] <- as.integer(cnt)
    result$suppressed[i] <- FALSE
  }

  # Drop bins with small counts (no hints), then drop the now-redundant flag
  # column so no `suppressed` marker is ever returned.
  result <- .suppressSmallCounts(result, "count")
  if (nrow(result) > 0L) {
    result$count <- vapply(result$count, .bandCount, numeric(1),
                           band_width = settings$nfilter_band)
  }

  .dropSuppressed(result)
}

#' Compute quantiles at specified probabilities
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param probs Numeric vector; probabilities
#' @param cohort_table Character; cohort temp table name (NULL)
#' @param window List with start/end dates (NULL)
#' @param rounding Integer; decimal places for rounding
#' @param concept_id Integer or NULL; restrict to rows of this concept
#' @param unit Distribution unit. \code{"person"} (default) contributes one
#'   within-person mean; \code{"record"} is allowed only for a 1:1 relation.
#' @return Data frame with probability and value
#' @keywords internal
.profileNumericQuantiles <- function(handle, table, value_col,
                                      probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                      cohort_table = NULL, window = NULL,
                                      rounding = 2L, concept_id = NULL,
                                      concept_col = NULL, unit = "person") {
  table <- tolower(.validateIdentifier(table, "table"))
  value_col <- tolower(.validateIdentifier(value_col, "column"))

  # Bound both tail reach and query multiplicity. Clamping malformed/extreme
  # requests would silently create duplicate queries and still reveal that an
  # extreme was requested; reject them fail-closed instead.
  probs_numeric <- suppressWarnings(as.numeric(probs))
  if (length(probs) < 1L || length(probs) > 9L ||
      length(probs_numeric) != length(probs) ||
      any(!is.finite(probs_numeric)) || any(probs_numeric < 0.05) ||
      any(probs_numeric > 0.95) || anyDuplicated(probs_numeric)) {
    stop("probs must contain 1 to 9 unique finite probabilities between ",
         "0.05 and 0.95.", call. = FALSE)
  }
  probs <- probs_numeric
  bp <- .buildBlueprint(handle)
  settings <- .omopDisclosureSettings()

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)

  qualified <- tbl_row$qualified_name[1]
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name
  has_person <- .profilerHasReviewedPersonScope(
    bp, table, cohort_table = cohort_table
  )

  .profilerColumnInfo(bp, table, value_col, require_numeric = TRUE)

  rounding_numeric <- suppressWarnings(as.numeric(rounding))
  rounding_integer <- suppressWarnings(as.integer(rounding))
  if (length(rounding) != 1L || length(rounding_numeric) != 1L ||
      !is.finite(rounding_numeric) || length(rounding_integer) != 1L ||
      is.na(rounding_integer) || rounding_numeric != rounding_integer ||
      rounding_integer < 0L || rounding_integer > 4L) {
    stop("rounding must be one integer between 0 and 4.", call. = FALSE)
  }
  rounding <- rounding_integer

  # Build FROM / WHERE clauses
  from_clause <- paste0(qualified, " AS t")
  where_parts <- paste0("t.", value_col, " IS NOT NULL")

  if (!is.null(cohort_table) && has_person) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN (SELECT DISTINCT subject_id FROM ",
                          cohort_table, ") AS coh",
                          " ON t.person_id = coh.subject_id")
  }

  if (!is.null(window)) {
    if (!is.list(window)) {
      stop("window must be a list with optional start/end dates.", call. = FALSE)
    }
    window <- .validateDateBounds(window$start, window$end,
                                  "profiling window")
    date_col <- .getDateColumn(bp, table)
    if (!is.null(date_col)) {
      if (!is.null(window$start)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " >= ",
                                .quoteLiteral(window$start, handle)))
      }
      if (!is.null(window$end)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " <= ",
                                .quoteLiteral(window$end, handle)))
      }
    }
  }

  # Optional concept scope: restrict to one concept of this table. concept_col
  # defaults to the domain concept but may override to scope by unit_concept_id /
  # *_type_concept_id / value_as_concept_id.
  if (!is.null(concept_id)) {
    ccol <- .resolveConceptScopeColumn(bp, table, concept_col)
    if (is.null(ccol)) {
      stop("Table '", table, "' has no concept column to scope by.",
           call. = FALSE)
    }
    where_parts <- c(where_parts,
                     paste0("t.", ccol, " = ", as.integer(concept_id)))
  }

  where_sql <- paste0(" WHERE ", paste(where_parts, collapse = " AND "))

  # Fail-closed distinct-person gate over the scoped relation: quantiles are
  # disclosive when they describe < nfilter_subset PEOPLE, no matter how many
  # records they contribute (the record-count gate below is necessary but not
  # sufficient - one person with many values would otherwise pass it).
  .assertNumericDistPersons(handle, from_clause, where_sql, tbl_cols)

  distribution_sql <- .numericDistributionSql(
    handle, from_clause, where_sql, value_col, tbl_cols, unit = unit)
  count_sql <- paste0("SELECT COUNT(*) AS n FROM (", distribution_sql,
                      ") AS distribution_values")
  n_total <- .executeQuery(handle, .renderSql(handle, count_sql))$n[1]

  # Block if total non-NULL values < nfilter_subset
  if (is.na(n_total) || n_total < settings$nfilter_subset) {
    stop("Disclosive: non-NULL value count below disclosure threshold. ",
         "Operation blocked.", call. = FALSE)
  }

  # Block quantile output if sample too small for safe percentile estimation.
  # With small n, even clamped probs can return values close to min/max.
  nfilter_dist <- settings$nfilter_dist %||% 10L
  tail_min <- max(as.integer(settings$nfilter_tab),
                  as.integer(settings$nfilter_subset), 1L)
  if (n_total < max(nfilter_dist, 2L * tail_min)) {
    stop("Disclosive: sample size too small for safe quantile estimation. ",
         "Protected tails require more contributions.", call. = FALSE)
  }

  # Compute quantiles using SQL ORDER BY + OFFSET approximation
  result <- data.frame(
    probability = probs,
    value = numeric(length(probs)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(probs)) {
    offset_val <- .protectedQuantileOffset(n_total, probs[i], settings)

    q_sql <- paste0(
      "SELECT distribution_values.value AS val FROM (", distribution_sql,
      ") AS distribution_values ORDER BY distribution_values.value ASC"
    )
    q_sql <- paste0(.renderSql(handle, q_sql),
                    .paginationClause(handle$target_dialect, 1L, offset_val))

    val <- tryCatch(.executeQuery(handle, q_sql)$val[1], error = function(e) NA_real_)
    result$value[i] <- if (!is.na(val)) round(val, rounding) else NA_real_
  }

  result
}

#' Count records by time bin (year, quarter, month)
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param date_col Character; date column (NULL = auto-detect)
#' @param granularity Character; "year", "quarter", or "month"
#' @param cohort_table Character; cohort temp table name (NULL)
#' @param window List with start/end dates (NULL)
#' @param concept_id Integer or NULL; optional concept scope.
#' @param concept_col Character or NULL; concept column for that scope.
#' @return Data frame with period, banded n_records and, for person-bearing
#'   tables, banded n_persons. Unsafe periods are omitted.
#' @keywords internal
.profileDateCounts <- function(handle, table, date_col = NULL,
                                granularity = "year", cohort_table = NULL,
                                window = NULL, concept_id = NULL,
                                concept_col = NULL) {
  table <- tolower(.validateIdentifier(table, "table"))
  bp <- .buildBlueprint(handle)
  settings <- .omopDisclosureSettings()

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)

  qualified <- tbl_row$qualified_name[1]
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name
  has_person <- .profilerHasReviewedPersonScope(
    bp, table, cohort_table = cohort_table
  )

  # Auto-detect date column if not provided
  if (is.null(date_col)) {
    date_col <- .getDateColumn(bp, table)
    if (is.null(date_col)) {
      stop("No date column found for table '", table,
           "'. Provide date_col explicitly.", call. = FALSE)
    }
  } else {
    date_col <- tolower(.validateIdentifier(date_col, "column"))
  }

  if (!date_col %in% tbl_cols) {
    stop("Column '", date_col, "' not found in '", table, "'.", call. = FALSE)
  }
  date_info <- .profilerColumnInfo(bp, table, date_col)
  if (!isTRUE(date_info$row$is_date[1])) {
    stop("Column '", date_col, "' is not a declared OMOP date field.",
         call. = FALSE)
  }

  granularity <- match.arg(granularity, c("year", "quarter", "month"))

  # Build date extraction expression based on dialect
  if (handle$target_dialect == "sqlite") {
    quarter_number <- .omopFloorDivideSql(
      paste0("CAST(strftime('%m', t.", date_col, ") AS INTEGER) + 2"),
      3L
    )
    date_expr <- switch(granularity,
      "year"    = paste0("strftime('%Y', t.", date_col, ")"),
      "quarter" = paste0("strftime('%Y', t.", date_col, ") || '-Q' || ",
                         quarter_number),
      "month"   = paste0("strftime('%Y-%m', t.", date_col, ")")
    )
  } else if (handle$target_dialect == "mysql") {
    date_expr <- switch(granularity,
      "year"    = paste0("CAST(YEAR(t.", date_col, ") AS CHAR)"),
      "quarter" = paste0("CONCAT(YEAR(t.", date_col, "), '-Q', QUARTER(t.", date_col, "))"),
      "month"   = paste0("DATE_FORMAT(t.", date_col, ", '%Y-%m')")
    )
  } else if (handle$target_dialect == "sql server") {
    date_expr <- switch(granularity,
      "year" = paste0("CAST(YEAR(t.", date_col, ") AS VARCHAR(4))"),
      "quarter" = paste0("CONCAT(YEAR(t.", date_col,
                         "), '-Q', DATEPART(quarter, t.", date_col, "))"),
      "month" = paste0("CONCAT(YEAR(t.", date_col,
                       "), '-', RIGHT('0' + CAST(MONTH(t.", date_col,
                       ") AS VARCHAR(2)), 2))")
    )
  } else {
    # PostgreSQL and other dialects: use EXTRACT
    date_expr <- switch(granularity,
      "year"    = paste0("CAST(EXTRACT(YEAR FROM t.", date_col, ") AS VARCHAR)"),
      "quarter" = paste0("CAST(EXTRACT(YEAR FROM t.", date_col, ") AS VARCHAR) || '-Q' || ",
                         "CAST(EXTRACT(QUARTER FROM t.", date_col, ") AS VARCHAR)"),
      "month"   = paste0("CAST(EXTRACT(YEAR FROM t.", date_col, ") AS VARCHAR) || '-' || ",
                         "LPAD(CAST(EXTRACT(MONTH FROM t.", date_col, ") AS VARCHAR), 2, '0')")
    )
  }

  # Build FROM / WHERE clauses
  from_clause <- paste0(qualified, " AS t")
  where_parts <- paste0("t.", date_col, " IS NOT NULL")

  if (!is.null(cohort_table) && has_person) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN (SELECT DISTINCT subject_id FROM ",
                          cohort_table, ") AS coh",
                          " ON t.person_id = coh.subject_id")
  }

  if (!is.null(window)) {
    if (!is.list(window)) {
      stop("window must be a list with start/end dates.", call. = FALSE)
    }
    if (is.null(window$start) || is.null(window$end)) {
      stop("Date-count windows require both start and end dates.",
           call. = FALSE)
    }
    window <- .validateDateBounds(window$start, window$end,
                                  "profiling window")
    if (!is.null(window$start)) {
      where_parts <- c(where_parts,
                       paste0("t.", date_col, " >= ",
                              .quoteLiteral(window$start, handle)))
    }
    if (!is.null(window$end)) {
      where_parts <- c(where_parts,
                       paste0("t.", date_col, " <= ",
                              .quoteLiteral(window$end, handle)))
    }
  }

  if (!is.null(concept_id)) {
    ccol <- .resolveConceptScopeColumn(bp, table, concept_col)
    if (is.null(ccol)) {
      stop("Table '", table, "' has no concept column to scope by.",
           call. = FALSE)
    }
    where_parts <- c(where_parts,
                     paste0("t.", ccol, " = ", as.integer(concept_id)))
  }

  where_sql <- paste0(" WHERE ", paste(where_parts, collapse = " AND "))

  sql <- paste0(
    "SELECT ", date_expr, " AS period, COUNT(*) AS n_records",
    if (has_person) ", COUNT(DISTINCT t.person_id) AS n_persons" else "",
    " FROM ", from_clause,
    where_sql,
    " GROUP BY ", date_expr,
    " ORDER BY period ASC"
  )

  result <- .executeQuery(handle, sql)

  if (nrow(result) == 0) {
    empty <- data.frame(period = character(0), n_records = numeric(0),
                        suppressed = logical(0), stringsAsFactors = FALSE)
    if (has_person) empty$n_persons <- numeric(0)
    return(.dropSuppressed(empty))
  }

  # For person-bearing tables, a period is gated on distinct PEOPLE as well as
  # records. Many longitudinal rows from one person therefore cannot make that
  # period releasable. Only after the exact gates pass are counts banded.
  result$suppressed <- FALSE
  count_cols <- c("n_records", if (has_person) "n_persons")
  result <- .suppressSmallCounts(result, count_cols)
  if (nrow(result) > 0L) {
    for (cc in count_cols) {
      result[[cc]] <- vapply(result[[cc]], .bandCount, numeric(1),
                             band_width = settings$nfilter_band)
    }
  }

  .dropSuppressed(result)
}

# --- Concept Drilldown & Locator ---

#' Full drilldown profile for a single concept within a table
#'
#' Returns summary stats, numeric distribution, categorical values, date
#' coverage, and missingness - all disclosure-controlled - for records
#' matching a given concept_id.
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param concept_id Integer; concept ID to drill into
#' @param concept_col Character; concept column (NULL = auto-detect)
#' @return Named list with summary, numeric_summary, categorical_values,
#'   date_range, missingness
#' @keywords internal
.profileConceptDrilldown <- function(handle, table, concept_id,
                                      concept_col = NULL) {
  table <- tolower(.validateIdentifier(table, "table"))
  concept_id <- as.integer(concept_id)
  bp <- .buildBlueprint(handle)
  settings <- .omopDisclosureSettings()

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)

  qualified <- tbl_row$qualified_name[1]
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name
  has_person <- .profilerHasReviewedPersonScope(bp, table)

  # Resolve the concept column through the single authoritative chokepoint:
  # auto-detect the domain concept when concept_col is NULL, otherwise validate
  # the override fail-closed (rejects blocked / source-value / identifier
  # columns) so it cannot be used as a forbidden WHERE filter below.
  concept_col <- .resolveConceptScopeColumn(bp, table, concept_col)
  if (is.null(concept_col)) {
    stop("No domain concept column found for table '", table,
         "'. Provide concept_col explicitly.", call. = FALSE)
  }

  where_concept <- paste0(concept_col, " = ", concept_id)

  # --- 1. Summary statistics ---

  if (has_person) {
    summary_sql <- paste0(
      "SELECT COUNT(*) AS n_records, ",
      "COUNT(DISTINCT person_id) AS n_persons ",
      "FROM ", qualified,
      " WHERE ", where_concept
    )
  } else {
    summary_sql <- paste0(
      "SELECT COUNT(*) AS n_records ",
      "FROM ", qualified,
      " WHERE ", where_concept
    )
  }
  summary_raw <- .executeQuery(handle, summary_sql)

  n_records <- summary_raw$n_records[1]
  n_persons <- if (has_person) summary_raw$n_persons[1] else NA_real_

  # Disclosure check on persons
  if (has_person) {
    .assertMinPersons(n_persons = n_persons)
  }

  # Suppress small counts
  if (!is.na(n_records) && n_records < settings$nfilter_tab) {
    n_records <- NA_real_
  }
  if (has_person && !is.na(n_persons) && n_persons < settings$nfilter_tab) {
    n_persons <- NA_real_
  }

  # Longitudinal: % persons with >1 record
  pct_persons_multi <- NA_real_
  if (has_person && !is.na(summary_raw$n_persons[1]) &&
      summary_raw$n_persons[1] >= settings$nfilter_tab) {
    multi_sql <- paste0(
      "SELECT COUNT(*) AS n_multi FROM (",
      "SELECT person_id FROM ", qualified,
      " WHERE ", where_concept,
      " GROUP BY person_id HAVING COUNT(*) > 1)"
    )
    n_multi <- tryCatch(.executeQuery(handle, multi_sql)$n_multi[1],
                        error = function(e) NA_real_)
    n_nonmulti <- summary_raw$n_persons[1] - n_multi
    groups_safe <- !is.na(n_multi) &&
      (n_multi == 0 || n_multi >= settings$nfilter_tab) &&
      (n_nonmulti == 0 || n_nonmulti >= settings$nfilter_tab)
    if (groups_safe) {
      multi_banded <- .bandCount(n_multi, settings$nfilter_band)
      persons_banded <- .bandCount(summary_raw$n_persons[1],
                                   settings$nfilter_band)
      if (persons_banded > 0 && (n_multi == 0 || multi_banded > 0)) {
        pct_persons_multi <- round(multi_banded / persons_banded * 100, 0)
      }
    }
  }

  # Look up concept name
  concept_name <- ""
  cinfo <- tryCatch(.vocabLookupConcepts(handle, concept_id),
                    error = function(e) NULL)
  if (!is.null(cinfo) && nrow(cinfo) > 0) {
    concept_name <- cinfo$concept_name[1]
  }

  # Band counts before deriving any returned ratio. Exact counts are used only
  # for suppression decisions and never become reconstructable through a mean
  # or percentage calculated at full precision.
  n_records <- .bandCount(n_records, settings$nfilter_band)
  if (has_person) n_persons <- .bandCount(n_persons, settings$nfilter_band)
  rpm <- if (has_person && !is.na(n_records) && !is.na(n_persons) &&
             n_persons > 0) round(n_records / n_persons, 2) else NA_real_

  summary_out <- list(
    concept_id = concept_id,
    concept_name = concept_name,
    n_records = n_records,
    n_persons = n_persons,
    records_per_person_mean = rpm,
    pct_persons_multi = pct_persons_multi
  )

  # --- 2. Numeric summary (only if value_as_number exists) ---
  numeric_summary <- NULL
  if ("value_as_number" %in% tbl_cols) {
    # Reuse the central person-unit distribution paths. This keeps drilldown
    # semantics identical to the public profilers and prevents repeated records
    # from a few patients from dominating either edges or bin counts.
    quantiles <- tryCatch(
      .profileNumericQuantiles(
        handle, table, "value_as_number",
        concept_id = concept_id, concept_col = concept_col, unit = "person"
      ),
      error = function(e) NULL
    )
    histogram <- tryCatch(
      .profileNumericHistogram(
        handle, table, "value_as_number", bins = 20L,
        concept_id = concept_id, concept_col = concept_col, unit = "person"
      ),
      error = function(e) NULL
    )
    if (!is.null(quantiles) || !is.null(histogram)) {
      numeric_summary <- list(quantiles = quantiles, histogram = histogram)
    }
  }

  # --- 3. Categorical values (only if value_as_concept_id exists) ---
  categorical_values <- NULL
  if ("value_as_concept_id" %in% tbl_cols) {
    has_person_col <- "person_id" %in% tbl_cols
    cat_sql <- paste0(
      "SELECT value_as_concept_id, COUNT(*) AS n",
      if (has_person_col) ", COUNT(DISTINCT person_id) AS n_persons " else " ",
      "FROM ", qualified,
      " WHERE ", where_concept,
      " AND value_as_concept_id IS NOT NULL ",
      "GROUP BY value_as_concept_id"
    )
    cat_result <- tryCatch(.executeQuery(handle, cat_sql),
                           error = function(e) NULL)

    if (!is.null(cat_result) && nrow(cat_result) > 0) {
      # Check safe levels
      safe <- tryCatch({
        n_cat_total_sql <- paste0(
          "SELECT COUNT(*) AS n FROM ", qualified,
          " WHERE ", where_concept,
          " AND value_as_concept_id IS NOT NULL")
        n_cat_total <- .executeQuery(handle, n_cat_total_sql)$n[1]
        .assertSafeLevels(nrow(cat_result), n_cat_total)
        TRUE
      }, error = function(e) FALSE)

      if (safe) {
        # Suppress on distinct PERSONS for this person-bearing table (a value
        # backed by many records but few people is disclosive), then band the
        # surviving counts so exact category sizes aren't released. No secondary
        # suppression: value_as_concept_id is multi-valued per person (one
        # patient can have several values over time), so the levels do not
        # partition persons and a hidden level is not recoverable from a total.
        cat_result <- .suppressSmallCounts(
          cat_result, if (has_person_col) "n_persons" else "n")
        cat_result <- .omopBandedTopN(
          cat_result, support_cols = "n", top_n = nrow(cat_result),
          key_cols = "value_as_concept_id"
        )
        if (nrow(cat_result) > 0) {
          band_width <- settings$nfilter_band
          cat_result$n <- vapply(cat_result$n, .bandCount, numeric(1),
                                 band_width = band_width)
          if ("n_persons" %in% names(cat_result)) {
            cat_result$n_persons <- vapply(cat_result$n_persons, .bandCount,
                                           numeric(1), band_width = band_width)
          }
        }

        # Decorate with concept names
        cat_ids <- cat_result$value_as_concept_id[!is.na(cat_result$value_as_concept_id)]
        if (length(cat_ids) > 0) {
          cat_concepts <- tryCatch(
            .vocabLookupConcepts(handle, cat_ids),
            error = function(e) data.frame(concept_id = integer(0),
                                           concept_name = character(0),
                                           stringsAsFactors = FALSE)
          )
          if (nrow(cat_concepts) > 0) {
            cmap <- stats::setNames(cat_concepts$concept_name,
                                    as.character(cat_concepts$concept_id))
            cat_result$concept_name <- cmap[as.character(cat_result$value_as_concept_id)]
            cat_result$concept_name[is.na(cat_result$concept_name)] <- ""
          } else {
            cat_result$concept_name <- ""
          }
        } else {
          cat_result$concept_name <- ""
        }
        categorical_values <- cat_result[, c("value_as_concept_id",
                                             "concept_name", "n"),
                                         drop = FALSE]
      }
    }
  }

  # --- 4. Date coverage ---
  date_range <- NULL
  date_col <- .getDateColumn(bp, table)
  if (!is.null(date_col)) {
    month_counts <- tryCatch(
      .profileDateCounts(
        handle, table, date_col = date_col, granularity = "month",
        concept_id = concept_id, concept_col = concept_col
      ),
      error = function(e) NULL
    )
    year_counts <- tryCatch(
      .profileDateCounts(
        handle, table, date_col = date_col, granularity = "year",
        concept_id = concept_id, concept_col = concept_col
      ),
      error = function(e) NULL
    )
    if (!is.null(month_counts) && nrow(month_counts) > 0L) {
      supported_months <- sort(as.character(month_counts$period))
      date_range <- list(
        column = date_col,
        min_month_safe = supported_months[1],
        max_month_safe = supported_months[length(supported_months)],
        date_counts = year_counts
      )
    }
  }

  # --- 5. Missingness within concept-filtered rows ---
  safe_cols <- !col_df$is_blocked & !col_df$is_sensitive &
    !vapply(col_df$column_name, .detectSensitiveColumns, logical(1))
  check_cols <- col_df$column_name[safe_cols]
  total <- summary_raw$n_records[1]
  total_banded <- .bandCount(total, settings$nfilter_band)

  missingness <- data.frame(column_name = character(0),
                            missing_rate = numeric(0),
                            stringsAsFactors = FALSE)

  if (!is.na(total) && total > 0) {
    for (col in check_cols) {
      miss_sql <- paste0("SELECT ",
        "SUM(CASE WHEN ", col, " IS NULL THEN 1 ELSE 0 END) AS n_missing",
        if (has_person) paste0(
          ", COUNT(DISTINCT CASE WHEN ", col,
          " IS NULL THEN person_id END) AS n_missing_persons",
          ", COUNT(DISTINCT CASE WHEN ", col,
          " IS NOT NULL THEN person_id END) AS n_value_persons"
        ) else "",
        " FROM ", qualified,
        " WHERE ", where_concept
      )
      miss <- tryCatch(.executeQuery(handle, miss_sql),
                       error = function(e) NULL)
      n_missing <- if (!is.null(miss)) miss$n_missing[1] else NA_real_
      n_value <- total - n_missing
      if (has_person && !is.null(miss)) {
        missing_safe <- isTRUE(n_missing == 0) ||
          (!is.na(miss$n_missing_persons[1]) &&
             miss$n_missing_persons[1] >= settings$nfilter_tab)
        value_safe <- isTRUE(n_value == 0) ||
          (!is.na(miss$n_value_persons[1]) &&
             miss$n_value_persons[1] >= settings$nfilter_tab)
      } else {
        missing_safe <- isTRUE(n_missing == 0) ||
          (!is.na(n_missing) && n_missing >= settings$nfilter_tab)
        value_safe <- isTRUE(n_value == 0) ||
          (!is.na(n_value) && n_value >= settings$nfilter_tab)
      }
      missing_banded <- .bandCount(n_missing, settings$nfilter_band)
      rate <- NA_real_
      if (missing_safe && value_safe && !is.na(total_banded) &&
          total_banded > 0 && (n_missing == 0 || missing_banded > 0)) {
        rate <- round(missing_banded / total_banded, 2)
      }
      missingness <- rbind(missingness, data.frame(
        column_name = col,
        missing_rate = rate,
        stringsAsFactors = FALSE
      ))
    }
  }

  list(
    summary = summary_out,
    numeric_summary = numeric_summary,
    categorical_values = categorical_values,
    date_range = date_range,
    missingness = missingness
  )
}

#' Locate a concept across all CDM tables
#'
#' Searches all clinical tables with concept columns and returns a presence
#' matrix showing where the given concept IDs appear, with record and person
#' counts (disclosure-controlled).
#'
#' @param handle CDM handle
#' @param concept_ids Integer vector; concept IDs to locate
#' @return Data frame with table_name, concept_column, concept_id, n_records,
#'   n_persons
#' @keywords internal
.profileLocateConcept <- function(handle, concept_ids) {
  concept_ids <- .conceptIdList(concept_ids)
  if (length(concept_ids) == 0) {
    return(data.frame(table_name = character(0), concept_column = character(0),
                      concept_id = integer(0), n_records = numeric(0),
                      n_persons = numeric(0), stringsAsFactors = FALSE))
  }

  bp <- .buildBlueprint(handle)
  ids_csv <- paste(concept_ids, collapse = ", ")

  results <- data.frame(table_name = character(0),
                        concept_column = character(0),
                        concept_id = integer(0),
                        n_records = numeric(0),
                        n_persons = numeric(0),
                        stringsAsFactors = FALSE)

  # Iterate over present CDM tables
  present <- bp$tables[bp$tables$present_in_db &
                         bp$tables$schema_category == "CDM", , drop = FALSE]

  for (i in seq_len(nrow(present))) {
    tbl_name <- present$table_name[i]
    qualified <- present$qualified_name[i]
    col_df <- bp$columns[[tbl_name]]
    if (is.null(col_df)) next

    tbl_cols <- col_df$column_name
    has_person <- "person_id" %in% tbl_cols

    # Only genuine releasable concept fields may become output dimensions.
    candidates <- col_df$column_name[col_df$concept_role != "non_concept"]
    concept_cols <- candidates[vapply(candidates, function(ccol) {
      !is.null(tryCatch(
        .resolveConceptScopeColumn(bp, tbl_name, ccol),
        error = function(e) NULL
      ))
    }, logical(1))]
    if (length(concept_cols) == 0) next

    for (ccol in concept_cols) {
      if (has_person) {
        sql <- paste0(
          "SELECT ", ccol, " AS concept_id, ",
          "COUNT(*) AS n_records, ",
          "COUNT(DISTINCT person_id) AS n_persons ",
          "FROM ", qualified,
          " WHERE ", ccol, " IN (", ids_csv, ") ",
          "GROUP BY ", ccol
        )
      } else {
        sql <- paste0(
          "SELECT ", ccol, " AS concept_id, ",
          "COUNT(*) AS n_records ",
          "FROM ", qualified,
          " WHERE ", ccol, " IN (", ids_csv, ") ",
          "GROUP BY ", ccol
        )
      }

      res <- tryCatch(.executeQuery(handle, sql), error = function(e) NULL)
      if (is.null(res) || nrow(res) == 0) next

      res$table_name <- tbl_name
      res$concept_column <- ccol
      if (!has_person) res$n_persons <- NA_real_

      results <- rbind(results, res[, c("table_name", "concept_column",
                                         "concept_id", "n_records",
                                         "n_persons"),
                                    drop = FALSE])
    }
  }

  # Suppress small counts (drops rows)
  if (nrow(results) > 0) {
    results <- .suppressSmallCounts(results, c("n_records", "n_persons"))
    band_width <- .omopDisclosureSettings()$nfilter_band
    for (cc in c("n_records", "n_persons")) {
      results[[cc]] <- vapply(results[[cc]], .bandCount, numeric(1),
                              band_width = band_width)
    }
  }

  results
}

# --- Disclosure-safe 2-way cross-tabulation ---------------------------------

#' Iterative complementary (secondary) suppression to a fixpoint
#'
#' Operates on a dense integer matrix \code{M} and its logical suppression mask
#' \code{S} (TRUE = cell hidden). Primary small-cell suppression alone is
#' recoverable: if a row (or column) has exactly ONE hidden non-zero cell and
#' every other cell in that line is visible, an attacker who also knows the line
#' total (or can subtract the visible cells from any published margin) recovers
#' the hidden value by arithmetic. Even WITHOUT published margins, a single
#' hidden non-zero cell in an otherwise-visible line is a one-unknown linear
#' equation the moment any external total is known, so we close it
#' defensively. This routine repeatedly scans every row and column; whenever a
#' line contains exactly one hidden NON-ZERO cell, it additionally suppresses
#' the smallest visible NON-ZERO cell in that line (structural zeros do not
#' participate in the arithmetic suppression pass). Suppressing a second cell turns the
#' line into a two-unknown equation, which is not uniquely solvable. The grid is
#' finite and each pass only ever adds suppressions, so the process is monotone
#' and converges.
#'
#' @param M Integer matrix of true counts (>= 0).
#' @param S Logical matrix, same dims as \code{M}; TRUE where already suppressed.
#' @return Updated logical matrix \code{S} at the suppression fixpoint.
#' @keywords internal
.complementarySuppress <- function(M, S) {
  nz <- M > 0  # cells that carry at least one individual (non-structural-zero)

  repeat {
    changed <- FALSE

    # Helper: for one line (row or column), if it has exactly one suppressed
    # non-zero cell and at least one visible non-zero cell, suppress the
    # smallest visible non-zero cell. Returns the (possibly mutated) mask line.
    close_line <- function(m_line, s_line, nz_line) {
      hidden_nz <- s_line & nz_line
      if (sum(hidden_nz) == 1L) {
        visible_nz <- nz_line & !s_line
        if (any(visible_nz)) {
          # smallest visible non-zero value
          vals <- m_line
          vals[!visible_nz] <- NA_integer_
          j <- which.min(vals)
          if (length(j) == 1L && !s_line[j]) {
            s_line[j] <- TRUE
            return(list(s = s_line, changed = TRUE))
          }
        }
      }
      list(s = s_line, changed = FALSE)
    }

    # Rows
    for (i in seq_len(nrow(M))) {
      res <- close_line(M[i, ], S[i, ], nz[i, ])
      if (res$changed) { S[i, ] <- res$s; changed <- TRUE }
    }
    # Columns
    for (j in seq_len(ncol(M))) {
      res <- close_line(M[, j], S[, j], nz[, j])
      if (res$changed) { S[, j] <- res$s; changed <- TRUE }
    }

    if (!changed) break
  }
  S
}

#' Apply primary + complementary suppression to a dense count matrix
#'
#' Implements the disclosure algorithm for a 2-way contingency table operating
#' entirely on a dense matrix (never row-dropping, which would itself leak the
#' table's structure). Steps:
#' \enumerate{
#'   \item Primary: cells with \code{0 < M < t} are suppressed.
#'   \item Complementary: \code{\link{.complementarySuppress}} runs to a fixpoint.
#'   \item Render: suppressed cells and structural zeros both become \code{NA};
#'     visible cells keep their value. This avoids a zero-versus-small hint.
#'   \item Margins: OMITTED by default. If \code{band_margins = TRUE}, row/col/
#'     grand totals are returned banded down via \code{\link{.bandCount}} only -
#'     exact margins are never returned.
#' }
#'
#' @param M Integer matrix of true counts (rows = row_col levels, cols =
#'   col_col levels), missing combos already filled with 0.
#' @param t Numeric; \code{nfilter_tab} threshold.
#' @param band_margins Logical; when TRUE, attach banded margins.
#' @param band_width Integer; band granularity for margins (default 5).
#' @param support Optional matrix of distinct-person support for record counts.
#' @return Named list with \code{matrix} (numeric, NA-masked) and optionally
#'   \code{row_margins}/\code{col_margins}/\code{grand_total} (banded) when
#'   \code{band_margins = TRUE}.
#' @keywords internal
.crossTabSuppress <- function(M, t, band_margins = FALSE, band_width = 5L,
                              support = NULL) {
  M <- matrix(as.integer(M), nrow = nrow(M), ncol = ncol(M),
              dimnames = dimnames(M))

  # Step A: primary small-cell suppression (only non-zero cells below threshold)
  S <- (M > 0) & (M < t)
  if (!is.null(support)) {
    support <- matrix(as.numeric(support), nrow = nrow(M), ncol = ncol(M),
                      dimnames = dimnames(M))
    # A record cell is releasable only if enough distinct people support it.
    S <- S | ((M > 0) & (is.na(support) | support < t))
  }

  # Step B: iterative complementary suppression to a fixpoint
  S <- .complementarySuppress(M, S)

  # Step C: mask both rare cells and structural zeros. Returning 0 for one and
  # NA for the other would reveal that a suppressed combination exists.
  out <- matrix(as.numeric(M), nrow = nrow(M), ncol = ncol(M),
                dimnames = dimnames(M))
  out[S | M == 0] <- NA_real_

  result <- list(matrix = out)

  # Step D: margins omitted by default; banded only on explicit opt-in.
  if (isTRUE(band_margins)) {
    row_tot <- rowSums(M)
    col_tot <- colSums(M)
    result$row_margins <- stats::setNames(
      vapply(row_tot, .bandCount, numeric(1), band_width = band_width),
      rownames(M))
    result$col_margins <- stats::setNames(
      vapply(col_tot, .bandCount, numeric(1), band_width = band_width),
      colnames(M))
    result$grand_total <- .bandCount(sum(M), band_width = band_width)
  }

  result
}

#' Build a disclosure-safe 2-way (optionally stratified) cross-tabulation
#'
#' Server-side engine for \code{\link{omopCrossTabDS}}. Cross-tabulates two
#' categorical columns of an OMOP table, counting either distinct persons
#' (default) or records, then applies primary + iterative complementary
#' small-cell suppression on the dense matrix (see \code{\link{.crossTabSuppress}}).
#' Exact margins are never returned. When \code{stratify_by} is supplied, an
#' INDEPENDENT protected 2-way table is produced for each stratum level (see
#' section 7 of the disclosure spec); the unstratified total is never returned.
#'
#' @param handle CDM handle.
#' @param table Character; table name.
#' @param row_col Character; row categorical column.
#' @param col_col Character; column categorical column.
#' @param count_mode Character; "persons" (distinct person_id) or "records".
#' @param row_concept_ids,col_concept_ids Optional integer vectors restricting
#'   the levels of the row/column axes.
#' @param cohort_table Character; cohort temp table to scope the population.
#' @param stratify_by Character; optional 3rd categorical column for stratified
#'   chained 2-way tables.
#' @param band_margins Logical; attach banded (never exact) margins.
#' @return For a plain call: a named list \code{{row_col, col_col, count_mode,
#'   row_levels, col_levels, counts (NA-masked matrix)}}. For a
#'   stratified call: \code{{stratified = TRUE, stratify_by, strata = <named
#'   list of per-level protected tables>}}.
#' @keywords internal
.profileCrossTab <- function(handle, table, row_col, col_col,
                             count_mode = "persons",
                             row_concept_ids = NULL, col_concept_ids = NULL,
                             cohort_table = NULL, stratify_by = NULL,
                             band_margins = FALSE) {
  table <- tolower(.validateIdentifier(table, "table"))
  row_col <- tolower(.validateIdentifier(row_col, "row_col"))
  col_col <- tolower(.validateIdentifier(col_col, "col_col"))
  if (!is.null(stratify_by)) {
    stratify_by <- tolower(.validateIdentifier(stratify_by, "stratify_by"))
  }
  if (!is.null(cohort_table)) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
  }

  count_mode <- match.arg(count_mode, c("persons", "records"))

  bp <- .buildBlueprint(handle)
  settings <- .omopDisclosureSettings()
  t <- settings$nfilter_tab

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)
  qualified <- tbl_row$qualified_name[1]
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name
  has_person <- .profilerHasReviewedPersonScope(
    bp, table, cohort_table = cohort_table
  )

  # Every axis / stratifier is both WHERE-filtered and emitted as raw GROUP BY
  # level VALUES. Route all three through the same central release-policy gate;
  # in particular, person/entity keys are not blueprint-blocked but are still
  # forbidden as dimensions.
  for (cc in c(row_col, col_col, stratify_by)) {
    if (!is.null(cc)) {
      info <- .profilerColumnInfo(bp, table, cc)
      if (isTRUE(info$is_numeric_measure)) {
        stop("Cross-tab axes must be categorical; continuous column '", cc,
             "' requires protected binning first.", call. = FALSE)
      }
    }
  }
  if (count_mode == "persons" && !has_person) {
    stop("Table '", table, "' has no person_id; use count_mode='records'.",
         call. = FALSE)
  }

  # FROM / cohort scoping (cohort INNER JOIN on subject_id, as in prevalence).
  from_clause <- paste0(qualified, " AS t")
  if (!is.null(cohort_table) && has_person) {
    from_clause <- paste0(from_clause,
                          " INNER JOIN (SELECT DISTINCT subject_id FROM ",
                          cohort_table, ") AS coh",
                          " ON t.person_id = coh.subject_id")
  }

  where_parts <- c(paste0("t.", row_col, " IS NOT NULL"),
                   paste0("t.", col_col, " IS NOT NULL"))
  if (!is.null(stratify_by)) {
    where_parts <- c(where_parts, paste0("t.", stratify_by, " IS NOT NULL"))
  }
  if (!is.null(row_concept_ids) && length(row_concept_ids) > 0) {
    where_parts <- c(where_parts,
      paste0("t.", row_col, " IN (", .sqlIdList(as.integer(row_concept_ids)), ")"))
  }
  if (!is.null(col_concept_ids) && length(col_concept_ids) > 0) {
    where_parts <- c(where_parts,
      paste0("t.", col_col, " IN (", .sqlIdList(as.integer(col_concept_ids)), ")"))
  }
  where_sql <- paste0(" WHERE ", paste(where_parts, collapse = " AND "))

  count_expr <- if (count_mode == "persons") {
    "COUNT(DISTINCT t.person_id)"
  } else {
    "COUNT(*)"
  }
  person_cell_support <- count_mode == "records" && has_person

  # Gate A (persons): distinct persons over the scoped population. For records
  # mode on a person-bearing table we still gate on distinct persons; on a
  # person-less table we cannot, so the build itself must remain safe.
  if (has_person) {
    n_sql <- paste0("SELECT COUNT(DISTINCT t.person_id) AS n FROM ",
                    from_clause, where_sql)
    n_persons <- .executeQuery(handle, .renderSql(handle, n_sql))$n[1]
    .assertMinPersons(n_persons = n_persons)
  }

  if (is.null(stratify_by)) {
    return(.crossTabOneSlice(handle, from_clause, where_sql, row_col, col_col,
                             count_expr, count_mode, t, band_margins, bp, table,
                             person_cell_support = person_cell_support))
  }

  # --- Stratified (section 7): independent protected 2-way per stratum ---
  max_strata <- 6L
  lv_sql <- if (has_person) {
    paste0("SELECT t.", stratify_by, " AS s FROM ", from_clause, where_sql,
           " GROUP BY t.", stratify_by,
           " HAVING COUNT(DISTINCT t.person_id) >= ",
           as.integer(settings$nfilter_subset),
           " ORDER BY t.", stratify_by)
  } else {
    paste0("SELECT DISTINCT t.", stratify_by, " AS s FROM ",
           from_clause, where_sql, " ORDER BY t.", stratify_by)
  }
  strata_levels <- .executeQuery(handle, .renderSql(handle, lv_sql))$s
  strata_levels <- strata_levels[!is.na(strata_levels)]

  # Cap strata: extra levels are not returned (slice suppressed by omission).
  if (length(strata_levels) > max_strata) {
    strata_levels <- strata_levels[seq_len(max_strata)]
  }

  strata_out <- list()
  for (lv in strata_levels) {
    lv_where <- paste0(where_sql, " AND t.", stratify_by, " = ",
                       if (is.numeric(lv)) as.integer(lv) else
                         .quoteLiteral(lv, handle))

    slice <- tryCatch(
      .crossTabOneSlice(handle, from_clause, lv_where, row_col, col_col,
                        count_expr, count_mode, t, band_margins, bp, table,
                        person_cell_support = person_cell_support),
      error = function(e) NULL
    )
    if (!is.null(slice)) strata_out[[as.character(lv)]] <- slice
  }

  list(stratified = TRUE, stratify_by = stratify_by,
       strata = strata_out)
}

#' Build one protected 2-way slice (dense matrix + suppression + names)
#'
#' @keywords internal
.crossTabOneSlice <- function(handle, from_clause, where_sql, row_col, col_col,
                              count_expr, count_mode, t, band_margins,
                              bp, table, person_cell_support = FALSE) {
  settings <- .omopDisclosureSettings()

  # Gate B (dimensions): distinct level counts on each axis, NULLs dropped.
  rl_sql <- paste0("SELECT COUNT(DISTINCT t.", row_col, ") AS n FROM ",
                   from_clause, where_sql)
  cl_sql <- paste0("SELECT COUNT(DISTINCT t.", col_col, ") AS n FROM ",
                   from_clause, where_sql)
  total_expr <- if (count_mode == "persons" || person_cell_support) {
    "COUNT(DISTINCT t.person_id)"
  } else {
    "COUNT(*)"
  }
  nt_sql <- paste0("SELECT ", total_expr, " AS n FROM ",
                   from_clause, where_sql)
  n_rows_lv <- .executeQuery(handle, .renderSql(handle, rl_sql))$n[1]
  n_cols_lv <- .executeQuery(handle, .renderSql(handle, cl_sql))$n[1]
  n_total   <- .executeQuery(handle, .renderSql(handle, nt_sql))$n[1]
  .assertSafeLevels(n_rows_lv, n_total)
  .assertSafeLevels(n_cols_lv, n_total)

  # Gate F: reject degenerate (1xN / Nx1) axes - that is a 1-way distribution.
  if (is.na(n_rows_lv) || is.na(n_cols_lv) || n_rows_lv < 2 || n_cols_lv < 2) {
    stop("Disclosive: cross-tab requires at least 2 levels on each axis ",
         "(a 1xN table is a one-way distribution).", call. = FALSE)
  }

  # No-hints axis policy: a level name is itself output. Keep only levels whose
  # marginal support clears nfilter_tab (distinct people for person-bearing
  # tables), so an all-suppressed rare row/column cannot reveal its label.
  marginal_support_expr <- if (count_mode == "persons" || person_cell_support) {
    "COUNT(DISTINCT t.person_id)"
  } else {
    "COUNT(*)"
  }
  safe_row_sql <- paste0(
    "SELECT t.", row_col, " AS level_v FROM ", from_clause, where_sql,
    " GROUP BY t.", row_col, " HAVING ", marginal_support_expr,
    " >= ", as.integer(t))
  safe_col_sql <- paste0(
    "SELECT t.", col_col, " AS level_v FROM ", from_clause, where_sql,
    " GROUP BY t.", col_col, " HAVING ", marginal_support_expr,
    " >= ", as.integer(t))
  safe_rows <- .executeQuery(handle, .renderSql(handle, safe_row_sql))$level_v
  safe_cols <- .executeQuery(handle, .renderSql(handle, safe_col_sql))$level_v
  if (length(safe_rows) < 2L || length(safe_cols) < 2L) {
    stop("Disclosive: cross-tab requires at least 2 supported levels on each ",
         "axis.", call. = FALSE)
  }

  # Build the dense long-form counts.
  agg_sql <- paste0(
    "SELECT t.", row_col, " AS row_v, t.", col_col, " AS col_v, ",
    count_expr, " AS n",
    if (person_cell_support)
      ", COUNT(DISTINCT t.person_id) AS n_persons" else "",
    " FROM ", from_clause, where_sql,
    " GROUP BY t.", row_col, ", t.", col_col)
  long <- .executeQuery(handle, .renderSql(handle, agg_sql))
  names(long) <- tolower(names(long))
  long <- long[long$row_v %in% safe_rows & long$col_v %in% safe_cols, ,
               drop = FALSE]

  row_levels <- sort(unique(long$row_v))
  col_levels <- sort(unique(long$col_v))
  if (length(row_levels) < 2L || length(col_levels) < 2L) {
    stop("Disclosive: cross-tab requires at least 2 supported levels on each ",
         "axis.", call. = FALSE)
  }

  M <- matrix(0L, nrow = length(row_levels), ncol = length(col_levels),
              dimnames = list(as.character(row_levels), as.character(col_levels)))
  P <- if (person_cell_support) {
    matrix(0L, nrow = length(row_levels), ncol = length(col_levels),
           dimnames = dimnames(M))
  } else NULL
  if (nrow(long) > 0) {
    ri <- match(long$row_v, row_levels)
    ci <- match(long$col_v, col_levels)
    for (k in seq_len(nrow(long))) {
      M[ri[k], ci[k]] <- as.integer(long$n[k])
      if (person_cell_support) {
        P[ri[k], ci[k]] <- as.integer(long$n_persons[k])
      }
    }
  }

  sup <- .crossTabSuppress(M, t, band_margins = band_margins, support = P)

  # Suppression decisions use exact cells; only surviving cells cross the
  # release boundary, banded to remove one-person differencing resolution.
  visible <- !is.na(sup$matrix)
  sup$matrix[visible] <- vapply(sup$matrix[visible], .bandCount, numeric(1),
                                band_width = settings$nfilter_band)

  # Decorate axis labels with concept names when the axis is a concept-id column.
  row_labels <- .crossTabLabels(handle, row_col, row_levels)
  col_labels <- .crossTabLabels(handle, col_col, col_levels)
  dimnames(sup$matrix) <- list(row_labels, col_labels)

  out <- list(
    row_col    = row_col,
    col_col    = col_col,
    count_mode = count_mode,
    row_levels = row_labels,
    col_levels = col_labels,
    counts     = sup$matrix
  )
  if (isTRUE(band_margins)) {
    out$row_margins <- stats::setNames(sup$row_margins, row_labels)
    out$col_margins <- stats::setNames(sup$col_margins, col_labels)
    out$grand_total <- sup$grand_total
  }
  out
}

#' Map axis levels to human-readable labels for concept-id columns
#'
#' @keywords internal
.crossTabLabels <- function(handle, col, levels) {
  labs <- as.character(levels)
  if (length(levels) == 0) return(labs)
  if (grepl("_concept_id$", col) || identical(col, "value_as_concept_id")) {
    ids <- suppressWarnings(as.integer(levels))
    concepts <- tryCatch(.vocabLookupConcepts(handle, ids[!is.na(ids)]),
                         error = function(e) NULL)
    if (!is.null(concepts) && nrow(concepts) > 0) {
      cmap <- stats::setNames(concepts$concept_name,
                              as.character(concepts$concept_id))
      named <- unname(cmap[as.character(levels)])
      miss <- is.na(named) | !nzchar(named)
      named[miss] <- labs[miss]
      labs <- named
    }
  }
  labs
}
