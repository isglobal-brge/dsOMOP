# Module: Profiling Engine
# Data profiling functions for table stats, column stats, distributions, and concept analysis.

#' Resolve the concept column a profiler scopes its concept_id filter on
#'
#' By default a concept scope (\code{concept_id}) restricts to the table's
#' DOMAIN concept column (e.g. \code{measurement_concept_id}). Supplying
#' \code{concept_col} lets the caller scope by another concept column on the
#' same table instead — \code{unit_concept_id}, a \code{*_type_concept_id}, or
#' \code{value_as_concept_id} — which enables unit-aware value distributions and
#' value-by-type profiling. This is the single authoritative chokepoint for
#' concept scoping: every profiler that turns \code{concept_col} into a WHERE
#' filter resolves it here, so the override is gated fail-closed and CANNOT be
#' used to filter on a forbidden column.
#'
#' An explicit override must be a genuine, releasable concept column:
#' \itemize{
#'   \item it must EXIST on this table (so it cannot reach another table);
#'   \item it must NOT be blocked (\code{is_blocked}) — this rejects every
#'     \code{*_source_value} / \code{*_source_concept_id} column (which must be
#'     treated as if it does not exist) and all other PII, closing the
#'     source-value filter leak;
#'   \item it must be a concept column (name ends in \code{_concept_id}) that is
#'     not a source concept — this additionally rejects identifier / person-key
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
  # other PII) is never a valid scope filter — treat it as if it does not exist.
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

#' Fail-closed distinct-person gate for a scoped numeric-distribution query
#'
#' The numeric-distribution profilers (range / quantiles / histogram / safe
#' cutpoints) summarise the VALUE distribution of a (possibly concept- or
#' cohort-scoped) relation. Gating those summaries on the RECORD count is not
#' enough: one individual can contribute many records (e.g. a single patient
#' with 20 lab measurements of the same concept), so a record count can clear
#' \code{nfilter_subset}/\code{nfilter_dist} while only one or two PEOPLE are
#' described — and p05/p95/quantiles/bin-edges then sit at that handful of
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

  # Surviving counts are banded down (floor to nfilter_band) at the return
  # boundary so an exact supra-threshold count is never released; the gate is
  # still the exact count compared against nfilter_subset.
  if ("rows" %in% stats) {
    sql <- paste0("SELECT COUNT(*) AS n FROM ", qualified)
    n_rows <- .executeQuery(handle, sql)$n[1]
    if (!is.na(n_rows) && n_rows < settings$nfilter_subset) {
      result$rows <- NA_real_
      result$rows_suppressed <- TRUE
    } else {
      result$rows <- .bandCount(n_rows, settings$nfilter_band)
      result$rows_suppressed <- FALSE
    }
  }

  if ("persons" %in% stats && "person_id" %in% tbl_cols) {
    sql <- paste0("SELECT COUNT(DISTINCT person_id) AS n FROM ", qualified)
    n_persons <- .executeQuery(handle, sql)$n[1]
    if (!is.na(n_persons) && n_persons < settings$nfilter_subset) {
      result$persons <- NA_real_
      result$persons_suppressed <- TRUE
    } else {
      result$persons <- .bandCount(n_persons, settings$nfilter_band)
      result$persons_suppressed <- FALSE
    }
  }

  if ("date_range" %in% stats) {
    date_col <- .getDateColumn(bp, table)
    if (!is.null(date_col)) {
      # Bin to year-month to prevent exact date disclosure
      dialect <- handle$target_dialect %||% "sql server"
      if (dialect == "sqlite") {
        min_expr <- paste0("strftime('%Y-%m', MIN(", date_col, "))")
        max_expr <- paste0("strftime('%Y-%m', MAX(", date_col, "))")
      } else {
        min_expr <- paste0("TO_CHAR(MIN(", date_col, "), 'YYYY-MM')")
        max_expr <- paste0("TO_CHAR(MAX(", date_col, "), 'YYYY-MM')")
      }
      sql <- paste0(
        "SELECT ", min_expr, " AS min_month, ",
        max_expr, " AS max_month ",
        "FROM ", qualified,
        " WHERE ", date_col, " IS NOT NULL"
      )
      date_result <- .executeQuery(handle, sql)
      result$date_range <- list(
        column    = date_col,
        min_month = date_result$min_month[1],
        max_month = date_result$max_month[1]
      )
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
  if (!column %in% col_df$column_name) {
    stop("Column '", column, "' not found in '", table, "'.", call. = FALSE)
  }

  # Check if sensitive
  if (any(col_df$is_blocked[col_df$column_name == column])) {
    stop("Column '", column, "' is blocked (sensitive).", call. = FALSE)
  }

  qualified <- tbl_row$qualified_name[1]
  settings <- .omopDisclosureSettings()
  has_person <- "person_id" %in% col_df$column_name

  # FROM + optional cohort scope (INNER JOIN on subject_id, as in prevalence).
  # Everything is computed over this scoped relation so the distinct-person gate
  # below applies to exactly the population the statistics describe.
  from_clause <- paste0(qualified, " AS t")
  if (!is.null(cohort_table) && has_person) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN ", cohort_table, " AS coh",
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

  result <- list(
    n_total = stats_result$n_total[1],
    n_missing = stats_result$n_missing[1],
    n_distinct = stats_result$n_distinct[1]
  )
  if (has_person) result$n_persons <- stats_result$n_persons[1]

  # Gate on distinct persons for person-bearing tables (fail-closed), falling
  # back to the record count only for tables with no person_id (e.g. vocabulary).
  gate_n <- if (has_person) result$n_persons else result$n_total
  if (is.na(gate_n) || gate_n < settings$nfilter_subset) {
    stop("Disclosive: insufficient individuals.", call. = FALSE)
  }

  # Suppress a tiny number of NULL rows (pinpoints the few missing individuals)
  if (!is.na(result$n_missing) &&
      result$n_missing > 0 && result$n_missing < settings$nfilter_tab) {
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
  col_type <- col_df$db_datatype[col_df$column_name == column][1]
  if (grepl("int|float|real|numeric|double|decimal", col_type) ||
      grepl("_as_number$|^quantity$|^range_|^dose_value$", column)) {
    # Disclosure-safe numeric summary: mean is always safe to release once the
    # population gate above has passed; SD is added so the canonical summary is
    # n, n_persons, %missing, mean, SD, clamped p05-p95 (still NO min/max, which
    # would identify outlier individuals — if min==max a single person is
    # identified). SD is computed over the same scoped, non-NULL relation as the
    # mean and is only released when the non-NULL count clears nfilter_dist (the
    # SAME small-sample floor that gates the quantiles): with a handful of values
    # the spread is itself near-identifying, so it fails closed to NA.
    num_sql <- paste0(
      "SELECT AVG(CAST(t.", column, " AS REAL)) AS mean_val, ",
      "COUNT(t.", column, ") AS n_val, ",
      "SUM(CAST(t.", column, " AS REAL) * CAST(t.", column, " AS REAL)) AS sumsq, ",
      "SUM(CAST(t.", column, " AS REAL)) AS sumval ",
      "FROM ", from_clause,
      " WHERE t.", column, " IS NOT NULL",
      if (!is.null(concept_filter)) paste0(" AND ", concept_filter) else ""
    )
    num_stats <- tryCatch(.executeQuery(handle, .renderSql(handle, num_sql)),
                          error = function(e) NULL)
    if (!is.null(num_stats) && nrow(num_stats) > 0) {
      result$mean <- round(num_stats$mean_val[1], 4)
      nfilter_dist <- settings$nfilter_dist %||% 10L
      n_val <- num_stats$n_val[1]
      # Sample SD from sums (sqrt((sumsq - n*mean^2)/(n-1))), gated by nfilter_dist.
      if (!is.na(n_val) && n_val >= nfilter_dist && n_val > 1) {
        mu <- num_stats$sumval[1] / n_val
        var_num <- (num_stats$sumsq[1] - n_val * mu * mu) / (n_val - 1)
        # Guard tiny negative variance from floating-point cancellation.
        if (!is.na(var_num)) {
          result$sd <- round(sqrt(max(var_num, 0)), 4)
        }
      } else {
        result$sd <- NA_real_
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

  results
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

  if (!is.null(columns)) {
    columns <- tolower(columns)
    columns <- intersect(columns, tbl_cols)
  } else {
    # Exclude blocked columns
    columns <- col_df$column_name[!col_df$is_blocked]
  }

  qualified <- tbl_row$qualified_name[1]
  settings <- .omopDisclosureSettings()
  has_person <- "person_id" %in% col_df$column_name

  # FROM + optional cohort scope (INNER JOIN on subject_id, as in prevalence).
  from_clause <- paste0(qualified, " AS t")
  if (!is.null(cohort_table) && has_person) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN ", cohort_table, " AS coh",
                          " ON t.person_id = coh.subject_id")
  }

  total_sql <- paste0("SELECT COUNT(*) AS n FROM ", from_clause)
  total <- .executeQuery(handle, .renderSql(handle, total_sql))$n[1]

  # Disclosure gate on DISTINCT persons (fail-closed) for person-bearing tables:
  # a record count can clear the threshold while only a few individuals exist.
  # The per-record missing rate below is still record-based (that is what the
  # statistic means), but it is only released once the population is large enough.
  if (has_person) {
    persons_sql <- paste0("SELECT COUNT(DISTINCT t.person_id) AS n FROM ",
                          from_clause)
    gate_n <- .executeQuery(handle, .renderSql(handle, persons_sql))$n[1]
  } else {
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
    sql <- paste0(
      "SELECT COUNT(*) AS n_missing FROM ", from_clause,
      " WHERE t.", col, " IS NULL"
    )
    n_missing <- .executeQuery(handle, .renderSql(handle, sql))$n_missing[1]
    # Suppress near-0/near-1 rates: a tiny number of NULL or non-NULL rows
    # pinpoints the few individuals in that column.
    if (!is.na(n_missing) &&
        ((n_missing > 0 && n_missing < settings$nfilter_tab) ||
         ((total - n_missing) > 0 && (total - n_missing) < settings$nfilter_tab))) {
      rate <- NA_real_
    } else {
      rate <- round(n_missing / total, 2)
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
  if (!column %in% col_df$column_name) {
    stop("Column '", column, "' not found in '", table, "'.", call. = FALSE)
  }

  if (any(col_df$is_blocked[col_df$column_name == column])) {
    stop("Column '", column, "' is blocked (sensitive).", call. = FALSE)
  }

  qualified <- tbl_row$qualified_name[1]
  has_person <- "person_id" %in% col_df$column_name

  # FROM + optional cohort scope (INNER JOIN on subject_id, as in prevalence).
  from_clause <- paste0(qualified, " AS t")
  if (!is.null(cohort_table) && has_person) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN ", cohort_table, " AS coh",
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
    "SELECT TOP ", effective_limit,
    " CAST(t.", column, " AS VARCHAR) AS value, ",
    "COUNT(*) AS n",
    if (has_person) ", COUNT(DISTINCT t.person_id) AS n_persons " else " ",
    "FROM ", from_clause, " ",
    "WHERE t.", column, " IS NOT NULL", concept_clause, " ",
    "GROUP BY t.", column, " ",
    "ORDER BY COUNT(*) DESC"
  )

  # Translate TOP to LIMIT for sqlite/postgresql
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

#' Compute safe histogram bin edges for a numeric column
#'
#' Returns quantile-based bin edges that can be safely used as filter
#' thresholds. Each bin is guaranteed to contain >= nfilter.tab persons.
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param column Character; numeric column name
#' @param concept_id Integer or NULL; concept filter
#' @param n_bins Integer; target number of bins (default 10)
#' @return List with breaks (numeric vector) and counts (integer vector)
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

  if (!column %in% tbl_cols) {
    stop("Column '", column, "' not found in '", table, "'.", call. = FALSE)
  }

  n_bins <- max(as.integer(n_bins), 2L)
  n_bins <- min(n_bins, 100L)

  # Build WHERE clauses. concept_col defaults to the domain concept but may
  # override to scope by unit_concept_id / *_type_concept_id / value_as_concept_id.
  where_parts <- paste0(column, " IS NOT NULL")
  if (!is.null(concept_id)) {
    scope_col <- .resolveConceptScopeColumn(bp, table, concept_col)
    if (!is.null(scope_col) && scope_col %in% tbl_cols) {
      where_parts <- c(where_parts,
                       paste0(scope_col, " = ", as.integer(concept_id)))
    }
  }
  where_sql <- paste0(" WHERE ", paste(where_parts, collapse = " AND "))

  # Fail-closed distinct-person gate over the scoped relation: cutpoint edges
  # (used as filter thresholds) are disclosive when they describe
  # < nfilter_subset PEOPLE, no matter how many records they contribute. This
  # query references columns un-aliased, so gate inline rather than via the
  # t-aliased .assertNumericDistPersons helper.
  if ("person_id" %in% tbl_cols) {
    pc_sql <- paste0("SELECT COUNT(DISTINCT person_id) AS n FROM ",
                     qualified, where_sql)
    .assertMinPersons(n_persons = .executeQuery(handle, pc_sql)$n[1])
  }

  # Get total count
  count_sql <- paste0("SELECT COUNT(*) AS n FROM ", qualified, where_sql)
  n_total <- .executeQuery(handle, count_sql)$n[1]

  if (is.na(n_total) || n_total < settings$nfilter_subset) {
    stop("Disclosive: operation blocked — insufficient individuals to meet ",
         "disclosure threshold. No further details available.",
         call. = FALSE)
  }

  # Compute quantile-based breaks
  probs <- seq(0, 1, length.out = n_bins + 1L)
  breaks <- numeric(length(probs))

  for (i in seq_along(probs)) {
    offset_val <- max(0L, as.integer(floor(n_total * probs[i])) - 1L)
    if (offset_val >= n_total) offset_val <- n_total - 1L

    q_sql <- paste0(
      "SELECT CAST(", column, " AS REAL) AS val FROM ", qualified,
      where_sql,
      " ORDER BY ", column, " ASC LIMIT 1 OFFSET ", offset_val
    )
    val <- tryCatch(.executeQuery(handle, q_sql)$val[1],
                    error = function(e) NA_real_)
    breaks[i] <- if (!is.na(val)) round(val, 4) else NA_real_
  }

  # Remove NAs and duplicates while preserving order
  breaks <- unique(breaks[!is.na(breaks)])

  if (length(breaks) < 2) {
    return(list(breaks = breaks, counts = integer(0)))
  }

  # Compute counts per bin
  n_result_bins <- length(breaks) - 1L
  counts <- integer(n_result_bins)

  for (i in seq_len(n_result_bins)) {
    lo <- breaks[i]
    hi <- breaks[i + 1L]
    op <- if (i == n_result_bins) " <= " else " < "
    bin_sql <- paste0(
      "SELECT COUNT(*) AS n FROM ", qualified, where_sql,
      " AND CAST(", column, " AS REAL) >= ", lo,
      " AND CAST(", column, " AS REAL)", op, hi
    )
    cnt <- tryCatch(.executeQuery(handle, bin_sql)$n[1],
                    error = function(e) 0L)
    counts[i] <- as.integer(cnt)
  }

  # Merge small bins until all >= min_cell
  min_cell <- settings$nfilter_tab
  while (length(counts) > 1) {
    small_idx <- which(counts < min_cell)
    if (length(small_idx) == 0) break
    # Merge smallest with its neighbor
    idx <- small_idx[1]
    if (idx == length(counts)) {
      # Merge with previous
      counts[idx - 1L] <- counts[idx - 1L] + counts[idx]
      counts <- counts[-idx]
      breaks <- breaks[-(idx + 1L)]
    } else {
      # Merge with next
      counts[idx] <- counts[idx] + counts[idx + 1L]
      counts <- counts[-(idx + 1L)]
      breaks <- breaks[-(idx + 1L)]
    }
  }

  # Suppress counts below threshold
  counts[counts < min_cell] <- NA_integer_

  list(breaks = breaks, counts = counts)
}

# --- Exploration Profiling (OMOP Studio) ---

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
#' the page window applied, plus a \code{source_table} tag. Performs NO concept
#' decoration and NO small-cell suppression — the caller owns those so a global
#' run decorates/suppresses ONCE over the merged set. The per-table population
#' gate (\code{.assertMinPersons} on the table's distinct persons) still runs
#' here so a too-small table never contributes rows.
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

  if (is.null(concept_col)) {
    concept_col <- .getDomainConceptColumn(bp, table)
    if (is.null(concept_col)) return(NULL)
  }
  if (!concept_col %in% tbl_cols) return(NULL)

  has_person <- "person_id" %in% tbl_cols

  # FROM / cohort join / window (same shape as the legacy single-table path).
  from_clause <- paste0(qualified, " AS t")
  if (!is.null(cohort_table) && has_person) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN ", cohort_table, " AS coh",
                          " ON t.person_id = coh.subject_id")
  }
  where_parts <- character(0)
  if (!is.null(window)) {
    date_col <- .getDateColumn(bp, table)
    if (!is.null(date_col)) {
      if (!is.null(window$start)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " >= ", .quoteLiteral(window$start)))
      }
      if (!is.null(window$end)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " <= ", .quoteLiteral(window$end)))
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

  order_col <- if (metric == "persons") "n_persons" else "n_records"
  if (has_person) {
    select_expr <- paste0(
      "SELECT t.", concept_col, " AS concept_id, ",
      "COUNT(DISTINCT t.person_id) AS n_persons, ",
      "COUNT(*) AS n_records")
  } else {
    order_col <- "n_records"
    select_expr <- paste0(
      "SELECT t.", concept_col, " AS concept_id, ",
      "COUNT(*) AS n_records")
  }

  sql <- paste0(
    select_expr,
    " FROM ", from_clause,
    where_sql,
    " GROUP BY t.", concept_col,
    " ORDER BY ", order_col, " DESC")

  # Pagination is applied AFTER rendering (see .paginationClause): bypass TOP so
  # offset is expressible. The window is bounded by effective_top_n upstream.
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

  if (!is.null(concept_col)) {
    concept_col <- tolower(.validateIdentifier(concept_col, "column"))
  }
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
    result <- result[order(-result[[ord_col]]), , drop = FALSE]
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

  if (is.null(concept_col)) {
    concept_col <- .getDomainConceptColumn(bp, table)
    if (is.null(concept_col)) {
      stop("No domain concept column found for table '", table,
           "'. Provide concept_col explicitly.", call. = FALSE)
    }
  }
  if (!concept_col %in% tbl_cols) {
    stop("Column '", concept_col, "' not found in '", table, "'.", call. = FALSE)
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
#' @return List with p05, p95, n_total
#' @keywords internal
.profileNumericRange <- function(handle, table, value_col,
                                  cohort_table = NULL, window = NULL,
                                  concept_id = NULL, concept_col = NULL) {
  table <- tolower(.validateIdentifier(table, "table"))
  value_col <- tolower(.validateIdentifier(value_col, "column"))
  bp <- .buildBlueprint(handle)

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)

  qualified <- tbl_row$qualified_name[1]
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name

  if (!value_col %in% tbl_cols) {
    stop("Column '", value_col, "' not found in '", table, "'.", call. = FALSE)
  }

  from_clause <- paste0(qualified, " AS t")
  where_parts <- paste0("t.", value_col, " IS NOT NULL")

  if (!is.null(cohort_table) && "person_id" %in% tbl_cols) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN ", cohort_table, " AS coh",
                          " ON t.person_id = coh.subject_id")
  }

  if (!is.null(window)) {
    date_col <- .getDateColumn(bp, table)
    if (!is.null(date_col)) {
      if (!is.null(window$start)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " >= ", .quoteLiteral(window$start)))
      }
      if (!is.null(window$end)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " <= ", .quoteLiteral(window$end)))
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

  count_sql <- paste0("SELECT COUNT(*) AS n FROM ", from_clause, where_sql)
  n_total <- .executeQuery(handle, count_sql)$n[1]

  if (is.na(n_total) || n_total == 0) {
    return(list(p05 = NA_real_, p95 = NA_real_, n_total = 0L))
  }

  # PERCENTILE LEAKAGE GUARD: With small samples, even clamped percentiles
  # (p05/p95) return values near min/max, identifying individuals at the
  # extremes of the distribution. E.g., with n=5, p05 ≈ 1st of 5 ≈ min.
  # nfilter_dist (default 10) ensures enough data points for safe estimation.
  settings <- .omopDisclosureSettings()
  nfilter_dist <- settings$nfilter_dist %||% 10L
  if (n_total < nfilter_dist) {
    return(list(p05 = NA_real_, p95 = NA_real_, n_total = as.integer(n_total)))
  }

  offset_p05 <- max(0L, as.integer(floor(n_total * 0.05)) - 1L)
  offset_p95 <- max(0L, as.integer(floor(n_total * 0.95)) - 1L)

  if (handle$target_dialect == "sqlite") {
    p05_sql <- paste0(
      "SELECT CAST(t.", value_col, " AS REAL) AS val FROM ", from_clause,
      where_sql,
      " ORDER BY t.", value_col, " ASC LIMIT 1 OFFSET ", offset_p05
    )
    p95_sql <- paste0(
      "SELECT CAST(t.", value_col, " AS REAL) AS val FROM ", from_clause,
      where_sql,
      " ORDER BY t.", value_col, " ASC LIMIT 1 OFFSET ", offset_p95
    )
  } else {
    p05_sql <- .renderSql(handle, paste0(
      "SELECT CAST(t.", value_col, " AS FLOAT) AS val FROM ", from_clause,
      where_sql,
      " ORDER BY t.", value_col, " ASC LIMIT 1 OFFSET ", offset_p05
    ))
    p95_sql <- .renderSql(handle, paste0(
      "SELECT CAST(t.", value_col, " AS FLOAT) AS val FROM ", from_clause,
      where_sql,
      " ORDER BY t.", value_col, " ASC LIMIT 1 OFFSET ", offset_p95
    ))
  }

  p05_val <- tryCatch(.executeQuery(handle, p05_sql)$val[1], error = function(e) NA_real_)
  p95_val <- tryCatch(.executeQuery(handle, p95_sql)$val[1], error = function(e) NA_real_)

  list(p05 = p05_val, p95 = p95_val, n_total = as.integer(n_total))
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
#' @return Data frame with bin_start, bin_end, count, suppressed
#' @keywords internal
.profileNumericHistogram <- function(handle, table, value_col,
                                      bins = 20L, cohort_table = NULL,
                                      window = NULL, breaks = NULL,
                                      concept_id = NULL, concept_col = NULL) {
  table <- tolower(.validateIdentifier(table, "table"))
  value_col <- tolower(.validateIdentifier(value_col, "column"))
  bp <- .buildBlueprint(handle)
  settings <- .omopDisclosureSettings()

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)

  qualified <- tbl_row$qualified_name[1]
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name

  if (!value_col %in% tbl_cols) {
    stop("Column '", value_col, "' not found in '", table, "'.", call. = FALSE)
  }

  bins <- as.integer(bins)
  if (bins < 2L || bins > 200L) {
    stop("bins must be between 2 and 200.", call. = FALSE)
  }

  # Build FROM / WHERE clauses
  from_clause <- paste0(qualified, " AS t")
  where_parts <- paste0("t.", value_col, " IS NOT NULL")

  if (!is.null(cohort_table) && "person_id" %in% tbl_cols) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN ", cohort_table, " AS coh",
                          " ON t.person_id = coh.subject_id")
  }

  if (!is.null(window)) {
    date_col <- .getDateColumn(bp, table)
    if (!is.null(date_col)) {
      if (!is.null(window$start)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " >= ", .quoteLiteral(window$start)))
      }
      if (!is.null(window$end)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " <= ", .quoteLiteral(window$end)))
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

  # Get total non-NULL count first
  count_sql <- paste0("SELECT COUNT(*) AS n FROM ", from_clause, where_sql)
  n_total <- .executeQuery(handle, count_sql)$n[1]

  if (is.na(n_total) || n_total == 0) {
    return(data.frame(bin_start = numeric(0), bin_end = numeric(0),
                      count = integer(0), suppressed = logical(0),
                      stringsAsFactors = FALSE))
  }

  # Use provided breaks (from two-pass pooling) or compute locally
  if (!is.null(breaks)) {
    # Shared breaks provided: use them directly
    bins <- length(breaks) - 1L
  } else {
    # Compute safe range using 5th and 95th percentile approximations
    offset_p05 <- max(0L, as.integer(floor(n_total * 0.05)) - 1L)
    offset_p95 <- max(0L, as.integer(floor(n_total * 0.95)) - 1L)

    if (handle$target_dialect == "sqlite") {
      p05_sql <- paste0(
        "SELECT CAST(t.", value_col, " AS REAL) AS val FROM ", from_clause,
        where_sql,
        " ORDER BY t.", value_col, " ASC LIMIT 1 OFFSET ", offset_p05
      )
      p95_sql <- paste0(
        "SELECT CAST(t.", value_col, " AS REAL) AS val FROM ", from_clause,
        where_sql,
        " ORDER BY t.", value_col, " ASC LIMIT 1 OFFSET ", offset_p95
      )
    } else {
      p05_sql <- .renderSql(handle, paste0(
        "SELECT CAST(t.", value_col, " AS FLOAT) AS val FROM ", from_clause,
        where_sql,
        " ORDER BY t.", value_col, " ASC LIMIT 1 OFFSET ", offset_p05
      ))
      p95_sql <- .renderSql(handle, paste0(
        "SELECT CAST(t.", value_col, " AS FLOAT) AS val FROM ", from_clause,
        where_sql,
        " ORDER BY t.", value_col, " ASC LIMIT 1 OFFSET ", offset_p95
      ))
    }

    p05_val <- tryCatch(.executeQuery(handle, p05_sql)$val[1], error = function(e) NA_real_)
    p95_val <- tryCatch(.executeQuery(handle, p95_sql)$val[1], error = function(e) NA_real_)

    if (is.na(p05_val) || is.na(p95_val) || p05_val == p95_val) {
      return(data.frame(
        bin_start = p05_val %||% 0,
        bin_end = p95_val %||% 0,
        count = n_total,
        suppressed = n_total < settings$nfilter_tab,
        stringsAsFactors = FALSE
      ))
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
        "SUM(CASE WHEN CAST(t.", value_col, " AS REAL) >= ", lo,
        " AND CAST(t.", value_col, " AS REAL) <= ", hi,
        " THEN 1 ELSE 0 END) AS bin_", i
      )
    } else {
      case_parts[i] <- paste0(
        "SUM(CASE WHEN CAST(t.", value_col, " AS REAL) >= ", lo,
        " AND CAST(t.", value_col, " AS REAL) < ", hi,
        " THEN 1 ELSE 0 END) AS bin_", i
      )
    }
  }

  bin_sql <- paste0(
    "SELECT ", paste(case_parts, collapse = ", "),
    " FROM ", from_clause,
    where_sql
  )

  bin_result <- .executeQuery(handle, bin_sql)

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

  # Drop bins with small counts (no hints)
  result <- .suppressSmallCounts(result, "count")

  result
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
#' @return Data frame with probability and value
#' @keywords internal
.profileNumericQuantiles <- function(handle, table, value_col,
                                      probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                      cohort_table = NULL, window = NULL,
                                      rounding = 2L, concept_id = NULL,
                                      concept_col = NULL) {
  table <- tolower(.validateIdentifier(table, "table"))
  value_col <- tolower(.validateIdentifier(value_col, "column"))

  # EXTREME VALUE GUARD: Clamp probabilities to [0.05, 0.95] to prevent
  # min/max extraction via extreme quantiles. Without this, a request for
  # probs = c(0.001, 0.999) would return values nearly identical to MIN/MAX,
  # potentially identifying individuals at the tails of the distribution.
  probs <- pmax(0.05, pmin(0.95, as.numeric(probs)))
  bp <- .buildBlueprint(handle)
  settings <- .omopDisclosureSettings()

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)

  qualified <- tbl_row$qualified_name[1]
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name

  if (!value_col %in% tbl_cols) {
    stop("Column '", value_col, "' not found in '", table, "'.", call. = FALSE)
  }

  rounding <- as.integer(rounding)

  # Build FROM / WHERE clauses
  from_clause <- paste0(qualified, " AS t")
  where_parts <- paste0("t.", value_col, " IS NOT NULL")

  if (!is.null(cohort_table) && "person_id" %in% tbl_cols) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN ", cohort_table, " AS coh",
                          " ON t.person_id = coh.subject_id")
  }

  if (!is.null(window)) {
    date_col <- .getDateColumn(bp, table)
    if (!is.null(date_col)) {
      if (!is.null(window$start)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " >= ", .quoteLiteral(window$start)))
      }
      if (!is.null(window$end)) {
        where_parts <- c(where_parts,
                         paste0("t.", date_col, " <= ", .quoteLiteral(window$end)))
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
  # sufficient — one person with many values would otherwise pass it).
  .assertNumericDistPersons(handle, from_clause, where_sql, tbl_cols)

  # Get total non-NULL count
  count_sql <- paste0("SELECT COUNT(*) AS n FROM ", from_clause, where_sql)
  n_total <- .executeQuery(handle, count_sql)$n[1]

  # Block if total non-NULL values < nfilter_subset
  if (is.na(n_total) || n_total < settings$nfilter_subset) {
    stop("Disclosive: non-NULL value count below disclosure threshold. ",
         "Operation blocked.", call. = FALSE)
  }

  # Block quantile output if sample too small for safe percentile estimation.
  # With small n, even clamped probs can return values close to min/max.
  nfilter_dist <- settings$nfilter_dist %||% 10L
  if (n_total < nfilter_dist) {
    stop("Disclosive: sample size too small for safe quantile estimation. ",
         "Minimum ", nfilter_dist, " non-NULL values required.", call. = FALSE)
  }

  # Compute quantiles using SQL ORDER BY + OFFSET approximation
  result <- data.frame(
    probability = probs,
    value = numeric(length(probs)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(probs)) {
    offset_val <- max(0L, as.integer(floor(n_total * probs[i])) - 1L)

    q_sql <- paste0(
      "SELECT CAST(t.", value_col, " AS REAL) AS val FROM ", from_clause,
      where_sql,
      " ORDER BY t.", value_col, " ASC LIMIT 1 OFFSET ", offset_val
    )

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
#' @return Data frame with period, n_records, suppressed
#' @keywords internal
.profileDateCounts <- function(handle, table, date_col = NULL,
                                granularity = "year", cohort_table = NULL,
                                window = NULL) {
  table <- tolower(.validateIdentifier(table, "table"))
  bp <- .buildBlueprint(handle)
  settings <- .omopDisclosureSettings()

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)

  qualified <- tbl_row$qualified_name[1]
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name

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

  granularity <- match.arg(granularity, c("year", "quarter", "month"))

  # Build date extraction expression based on dialect
  if (handle$target_dialect == "sqlite") {
    date_expr <- switch(granularity,
      "year"    = paste0("strftime('%Y', t.", date_col, ")"),
      "quarter" = paste0("strftime('%Y', t.", date_col, ") || '-Q' || ",
                         "((CAST(strftime('%m', t.", date_col, ") AS INTEGER) + 2) / 3)"),
      "month"   = paste0("strftime('%Y-%m', t.", date_col, ")")
    )
  } else if (handle$target_dialect == "mysql") {
    date_expr <- switch(granularity,
      "year"    = paste0("CAST(YEAR(t.", date_col, ") AS CHAR)"),
      "quarter" = paste0("CONCAT(YEAR(t.", date_col, "), '-Q', QUARTER(t.", date_col, "))"),
      "month"   = paste0("DATE_FORMAT(t.", date_col, ", '%Y-%m')")
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

  if (!is.null(cohort_table) && "person_id" %in% tbl_cols) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN ", cohort_table, " AS coh",
                          " ON t.person_id = coh.subject_id")
  }

  if (!is.null(window)) {
    if (!is.null(window$start)) {
      where_parts <- c(where_parts,
                       paste0("t.", date_col, " >= ", .quoteLiteral(window$start)))
    }
    if (!is.null(window$end)) {
      where_parts <- c(where_parts,
                       paste0("t.", date_col, " <= ", .quoteLiteral(window$end)))
    }
  }

  where_sql <- paste0(" WHERE ", paste(where_parts, collapse = " AND "))

  sql <- paste0(
    "SELECT ", date_expr, " AS period, COUNT(*) AS n_records",
    " FROM ", from_clause,
    where_sql,
    " GROUP BY ", date_expr,
    " ORDER BY period ASC"
  )

  result <- .executeQuery(handle, sql)

  if (nrow(result) == 0) {
    return(data.frame(period = character(0), n_records = integer(0),
                      suppressed = logical(0), stringsAsFactors = FALSE))
  }

  # Drop bins with small counts (no hints)
  result$suppressed <- FALSE
  result <- .suppressSmallCounts(result, "n_records")

  result
}

# --- Concept Drilldown & Locator ---

#' Full drilldown profile for a single concept within a table
#'
#' Returns summary stats, numeric distribution, categorical values, date
#' coverage, and missingness — all disclosure-controlled — for records
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
  has_person <- "person_id" %in% tbl_cols

  if (has_person) {
    summary_sql <- paste0(
      "SELECT COUNT(*) AS n_records, ",
      "COUNT(DISTINCT person_id) AS n_persons, ",
      "CAST(COUNT(*) AS REAL) / NULLIF(COUNT(DISTINCT person_id), 0) AS records_per_person_mean ",
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

  rpm <- if (has_person) summary_raw$records_per_person_mean[1] else NA_real_

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
    if (!is.na(n_multi) && n_multi >= settings$nfilter_tab) {
      pct_persons_multi <- round(n_multi / summary_raw$n_persons[1] * 100, 2)
    }
  }

  # Look up concept name
  concept_name <- ""
  cinfo <- tryCatch(.vocabLookupConcepts(handle, concept_id),
                    error = function(e) NULL)
  if (!is.null(cinfo) && nrow(cinfo) > 0) {
    concept_name <- cinfo$concept_name[1]
  }

  # Band the reported record/person counts at the return boundary. The person
  # gate, the small-cell suppression, and the ratios above (records_per_person,
  # pct_persons_multi) were all computed from the EXACT counts (summary_raw);
  # only the two reported counts are banded.
  n_records <- .bandCount(n_records, settings$nfilter_band)
  if (has_person) n_persons <- .bandCount(n_persons, settings$nfilter_band)

  summary_out <- list(
    concept_id = concept_id,
    concept_name = concept_name,
    n_records = n_records,
    n_persons = n_persons,
    records_per_person_mean = if (!is.na(rpm)) round(rpm, 2) else NA_real_,
    pct_persons_multi = pct_persons_multi
  )

  # --- 2. Numeric summary (only if value_as_number exists) ---
  numeric_summary <- NULL
  if ("value_as_number" %in% tbl_cols) {
    val_col <- "value_as_number"
    from_clause <- paste0(qualified, " AS t")
    where_parts <- paste0("t.", where_concept,
                          " AND t.", val_col, " IS NOT NULL")
    where_sql <- paste0(" WHERE ", where_parts)

    count_sql <- paste0("SELECT COUNT(*) AS n FROM ", from_clause, where_sql)
    n_vals <- tryCatch(.executeQuery(handle, count_sql)$n[1],
                       error = function(e) 0L)

    if (!is.na(n_vals) && n_vals >= settings$nfilter_subset) {
      # Quantiles
      probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
      quantiles <- data.frame(probability = probs, value = numeric(length(probs)),
                              stringsAsFactors = FALSE)
      for (i in seq_along(probs)) {
        offset_val <- max(0L, as.integer(floor(n_vals * probs[i])) - 1L)
        q_sql <- paste0(
          "SELECT CAST(t.", val_col, " AS REAL) AS val FROM ", from_clause,
          where_sql,
          " ORDER BY t.", val_col, " ASC LIMIT 1 OFFSET ", offset_val
        )
        val <- tryCatch(.executeQuery(handle, q_sql)$val[1],
                        error = function(e) NA_real_)
        quantiles$value[i] <- if (!is.na(val)) round(val, 2) else NA_real_
      }

      # Histogram using 5th/95th from quantiles as range
      p05 <- quantiles$value[1]
      p95 <- quantiles$value[5]
      histogram <- NULL

      if (!is.na(p05) && !is.na(p95) && p05 != p95) {
        bins <- 20L
        bin_width <- (p95 - p05) / bins
        breaks <- seq(p05, p95, by = bin_width)
        if (length(breaks) < bins + 1L) breaks <- c(breaks, p95)
        breaks <- breaks[seq_len(bins + 1L)]

        case_parts <- character(bins)
        for (j in seq_len(bins)) {
          lo <- breaks[j]; hi <- breaks[j + 1L]
          op <- if (j == bins) " <= " else " < "
          case_parts[j] <- paste0(
            "SUM(CASE WHEN CAST(t.", val_col, " AS REAL) >= ", lo,
            " AND CAST(t.", val_col, " AS REAL)", op, hi,
            " THEN 1 ELSE 0 END) AS bin_", j
          )
        }
        bin_sql <- paste0("SELECT ", paste(case_parts, collapse = ", "),
                          " FROM ", from_clause, where_sql)
        bin_result <- tryCatch(.executeQuery(handle, bin_sql),
                               error = function(e) NULL)

        if (!is.null(bin_result)) {
          histogram <- data.frame(
            bin_start = breaks[seq_len(bins)],
            bin_end = breaks[seq_len(bins) + 1L],
            count = integer(bins), suppressed = logical(bins),
            stringsAsFactors = FALSE
          )
          for (j in seq_len(bins)) {
            col_name <- paste0("bin_", j)
            cnt <- if (col_name %in% names(bin_result))
              as.integer(bin_result[[col_name]][1]) else 0L
            histogram$count[j] <- cnt
            histogram$suppressed[j] <- FALSE
          }
          # Drop bins with small counts (no hints)
          histogram <- .suppressSmallCounts(histogram, "count")
        }
      }

      numeric_summary <- list(quantiles = quantiles, histogram = histogram)
    }
  }

  # --- 3. Categorical values (only if value_as_concept_id exists) ---
  categorical_values <- NULL
  if ("value_as_concept_id" %in% tbl_cols) {
    cat_sql <- paste0(
      "SELECT value_as_concept_id, COUNT(*) AS n ",
      "FROM ", qualified,
      " WHERE ", where_concept,
      " AND value_as_concept_id IS NOT NULL ",
      "GROUP BY value_as_concept_id ",
      "ORDER BY COUNT(*) DESC"
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
        cat_result <- .suppressSmallCounts(cat_result, "n")

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
    # Use 5th/95th percentile for safe date range
    date_count_sql <- paste0(
      "SELECT COUNT(*) AS n FROM ", qualified,
      " WHERE ", where_concept,
      " AND ", date_col, " IS NOT NULL"
    )
    n_dates <- tryCatch(.executeQuery(handle, date_count_sql)$n[1],
                        error = function(e) 0L)

    if (!is.na(n_dates) && n_dates >= settings$nfilter_subset) {
      # Bin p05/p95 to year-month to prevent exact date disclosure
      off_p05 <- max(0L, as.integer(floor(n_dates * 0.05)) - 1L)
      off_p95 <- max(0L, as.integer(floor(n_dates * 0.95)) - 1L)

      if (handle$target_dialect == "sqlite") {
        ym_expr <- paste0("strftime('%Y-%m', ", date_col, ")")
      } else if (handle$target_dialect == "mysql") {
        ym_expr <- paste0("DATE_FORMAT(", date_col, ", '%Y-%m')")
      } else {
        ym_expr <- paste0("TO_CHAR(", date_col, ", 'YYYY-MM')")
      }

      p05_sql <- paste0(
        "SELECT ", ym_expr, " AS val FROM ", qualified,
        " WHERE ", where_concept,
        " AND ", date_col, " IS NOT NULL",
        " ORDER BY ", date_col, " ASC LIMIT 1 OFFSET ", off_p05
      )
      p95_sql <- paste0(
        "SELECT ", ym_expr, " AS val FROM ", qualified,
        " WHERE ", where_concept,
        " AND ", date_col, " IS NOT NULL",
        " ORDER BY ", date_col, " ASC LIMIT 1 OFFSET ", off_p95
      )

      min_month_safe <- tryCatch(.executeQuery(handle, p05_sql)$val[1],
                                error = function(e) NA_character_)
      max_month_safe <- tryCatch(.executeQuery(handle, p95_sql)$val[1],
                                error = function(e) NA_character_)

      # Date counts by year for concept-filtered records
      if (handle$target_dialect == "sqlite") {
        date_expr <- paste0("strftime('%Y', ", date_col, ")")
      } else if (handle$target_dialect == "mysql") {
        date_expr <- paste0("CAST(YEAR(", date_col, ") AS CHAR)")
      } else {
        date_expr <- paste0("CAST(EXTRACT(YEAR FROM ", date_col, ") AS VARCHAR)")
      }
      dc_sql <- paste0(
        "SELECT ", date_expr, " AS period, COUNT(*) AS n_records",
        " FROM ", qualified,
        " WHERE ", where_concept,
        " AND ", date_col, " IS NOT NULL",
        " GROUP BY ", date_expr,
        " ORDER BY period ASC"
      )
      dc_result <- tryCatch(.executeQuery(handle, dc_sql),
                            error = function(e) NULL)
      if (!is.null(dc_result) && nrow(dc_result) > 0) {
        # "No hints" policy: drop small periods entirely (keeping them with
        # NA would reveal which years had 1-2 records via subtraction).
        dc_result <- .suppressSmallCounts(dc_result, "n_records")
      }

      date_range <- list(
        column = date_col,
        min_month_safe = min_month_safe,
        max_month_safe = max_month_safe,
        date_counts = dc_result
      )
    }
  }

  # --- 5. Missingness within concept-filtered rows ---
  check_cols <- col_df$column_name[!col_df$is_blocked]
  total_sql <- paste0("SELECT COUNT(*) AS n FROM ", qualified,
                      " WHERE ", where_concept)
  total <- tryCatch(.executeQuery(handle, total_sql)$n[1],
                    error = function(e) 0L)

  missingness <- data.frame(column_name = character(0),
                            missing_rate = numeric(0),
                            stringsAsFactors = FALSE)

  if (!is.na(total) && total > 0) {
    for (col in check_cols) {
      miss_sql <- paste0(
        "SELECT COUNT(*) AS n_missing FROM ", qualified,
        " WHERE ", where_concept,
        " AND ", col, " IS NULL"
      )
      n_missing <- tryCatch(.executeQuery(handle, miss_sql)$n_missing[1],
                            error = function(e) NA_real_)
      missingness <- rbind(missingness, data.frame(
        column_name = col,
        missing_rate = if (!is.na(n_missing)) round(n_missing / total, 4)
                       else NA_real_,
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
  concept_ids <- as.integer(concept_ids)
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

    # Find concept columns (concept_role != "non_concept")
    concept_cols <- col_df$column_name[col_df$concept_role != "non_concept"]
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
#' the smallest visible NON-ZERO cell in that line (structural zeros are never
#' suppressed — they carry no individual). Suppressing a second cell turns the
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
#'   \item Render: suppressed cells become \code{NA}; structural zeros stay
#'     \code{0}; visible cells keep their value.
#'   \item Margins: OMITTED by default. If \code{band_margins = TRUE}, row/col/
#'     grand totals are returned banded down via \code{\link{.bandCount}} only —
#'     exact margins are never returned.
#' }
#'
#' @param M Integer matrix of true counts (rows = row_col levels, cols =
#'   col_col levels), missing combos already filled with 0.
#' @param t Numeric; \code{nfilter_tab} threshold.
#' @param band_margins Logical; when TRUE, attach banded margins.
#' @param band_width Integer; band granularity for margins (default 5).
#' @return Named list with \code{matrix} (numeric, NA-masked), \code{suppressed}
#'   (logical, TRUE if every non-zero cell ended up hidden), and optionally
#'   \code{row_margins}/\code{col_margins}/\code{grand_total} (banded) when
#'   \code{band_margins = TRUE}.
#' @keywords internal
.crossTabSuppress <- function(M, t, band_margins = FALSE, band_width = 5L) {
  M <- matrix(as.integer(M), nrow = nrow(M), ncol = ncol(M),
              dimnames = dimnames(M))

  # Step A: primary small-cell suppression (only non-zero cells below threshold)
  S <- (M > 0) & (M < t)

  # Step B: iterative complementary suppression to a fixpoint
  S <- .complementarySuppress(M, S)

  # Step C: render NA-masked matrix (structural zeros preserved as 0)
  out <- matrix(as.numeric(M), nrow = nrow(M), ncol = ncol(M),
                dimnames = dimnames(M))
  out[S] <- NA_real_

  result <- list(
    matrix     = out,
    suppressed = all(S[M > 0]) && any(M > 0)
  )

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
#'   row_levels, col_levels, counts (NA-masked matrix), suppressed}}. For a
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

  # Every axis / stratifier is both WHERE-filtered and emitted as raw GROUP BY
  # level VALUES, so a blocked column here would leak (e.g. value_as_string or a
  # *_source_value). Reject fail-closed, mirroring .resolveConceptScopeColumn —
  # crosstab cannot route through that chokepoint (two axes, no domain default).
  for (cc in c(row_col, col_col, stratify_by)) {
    if (!is.null(cc) && !cc %in% tbl_cols) {
      stop("Column '", cc, "' not found in '", table, "'.", call. = FALSE)
    }
    if (!is.null(cc) && any(col_df$is_blocked[col_df$column_name == cc])) {
      stop("Column '", cc, "' is blocked (sensitive).", call. = FALSE)
    }
  }
  if (count_mode == "persons" && !"person_id" %in% tbl_cols) {
    stop("Table '", table, "' has no person_id; use count_mode='records'.",
         call. = FALSE)
  }

  # FROM / cohort scoping (cohort INNER JOIN on subject_id, as in prevalence).
  from_clause <- paste0(qualified, " AS t")
  if (!is.null(cohort_table) && "person_id" %in% tbl_cols) {
    from_clause <- paste0(from_clause,
                          " INNER JOIN ", cohort_table, " AS coh",
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

  # Gate A (persons): distinct persons over the scoped population. For records
  # mode on a person-bearing table we still gate on distinct persons; on a
  # person-less table we cannot, so the build itself must remain safe.
  if ("person_id" %in% tbl_cols) {
    n_sql <- paste0("SELECT COUNT(DISTINCT t.person_id) AS n FROM ",
                    from_clause, where_sql)
    n_persons <- .executeQuery(handle, .renderSql(handle, n_sql))$n[1]
    .assertMinPersons(n_persons = n_persons)
  }

  if (is.null(stratify_by)) {
    return(.crossTabOneSlice(handle, from_clause, where_sql, row_col, col_col,
                             count_expr, count_mode, t, band_margins, bp, table))
  }

  # --- Stratified (section 7): independent protected 2-way per stratum ---
  max_strata <- 6L
  lv_sql <- paste0("SELECT DISTINCT t.", stratify_by, " AS s FROM ",
                   from_clause, where_sql, " ORDER BY t.", stratify_by)
  strata_levels <- .executeQuery(handle, .renderSql(handle, lv_sql))$s
  strata_levels <- strata_levels[!is.na(strata_levels)]

  # Cap strata: extra levels are not returned (slice suppressed by omission).
  capped <- length(strata_levels) > max_strata
  if (capped) strata_levels <- strata_levels[seq_len(max_strata)]

  strata_out <- list()
  for (lv in strata_levels) {
    lv_where <- paste0(where_sql, " AND t.", stratify_by, " = ",
                       if (is.numeric(lv)) as.integer(lv) else .quoteLiteral(lv))

    # Per-stratum person gate: a stratum below nfilter_subset is fully
    # suppressed (generic block -> all-NA slice), never partially exposed.
    slice_ok <- TRUE
    if ("person_id" %in% tbl_cols) {
      sp_sql <- paste0("SELECT COUNT(DISTINCT t.person_id) AS n FROM ",
                       from_clause, lv_where)
      n_sp <- .executeQuery(handle, .renderSql(handle, sp_sql))$n[1]
      slice_ok <- !is.na(n_sp) && n_sp >= settings$nfilter_subset
    }

    slice <- tryCatch(
      .crossTabOneSlice(handle, from_clause, lv_where, row_col, col_col,
                        count_expr, count_mode, t, band_margins, bp, table),
      error = function(e) NULL
    )
    if (!slice_ok || is.null(slice)) {
      slice <- list(row_col = row_col, col_col = col_col,
                    count_mode = count_mode, suppressed = TRUE,
                    counts = matrix(numeric(0), 0, 0))
    }
    strata_out[[as.character(lv)]] <- slice
  }

  list(stratified = TRUE, stratify_by = stratify_by,
       strata = strata_out, capped = capped)
}

#' Build one protected 2-way slice (dense matrix + suppression + names)
#'
#' @keywords internal
.crossTabOneSlice <- function(handle, from_clause, where_sql, row_col, col_col,
                              count_expr, count_mode, t, band_margins,
                              bp, table) {
  settings <- .omopDisclosureSettings()

  # Gate B (dimensions): distinct level counts on each axis, NULLs dropped.
  rl_sql <- paste0("SELECT COUNT(DISTINCT t.", row_col, ") AS n FROM ",
                   from_clause, where_sql)
  cl_sql <- paste0("SELECT COUNT(DISTINCT t.", col_col, ") AS n FROM ",
                   from_clause, where_sql)
  nt_sql <- paste0("SELECT COUNT(*) AS n FROM ", from_clause, where_sql)
  n_rows_lv <- .executeQuery(handle, .renderSql(handle, rl_sql))$n[1]
  n_cols_lv <- .executeQuery(handle, .renderSql(handle, cl_sql))$n[1]
  n_total   <- .executeQuery(handle, .renderSql(handle, nt_sql))$n[1]
  .assertSafeLevels(n_rows_lv, n_total)
  .assertSafeLevels(n_cols_lv, n_total)

  # Gate F: reject degenerate (1xN / Nx1) axes — that is a 1-way distribution.
  if (is.na(n_rows_lv) || is.na(n_cols_lv) || n_rows_lv < 2 || n_cols_lv < 2) {
    stop("Disclosive: cross-tab requires at least 2 levels on each axis ",
         "(a 1xN table is a one-way distribution).", call. = FALSE)
  }

  # Build the dense long-form counts.
  agg_sql <- paste0(
    "SELECT t.", row_col, " AS row_v, t.", col_col, " AS col_v, ",
    count_expr, " AS n FROM ", from_clause, where_sql,
    " GROUP BY t.", row_col, ", t.", col_col)
  long <- .executeQuery(handle, .renderSql(handle, agg_sql))
  names(long) <- tolower(names(long))

  row_levels <- sort(unique(long$row_v))
  col_levels <- sort(unique(long$col_v))

  M <- matrix(0L, nrow = length(row_levels), ncol = length(col_levels),
              dimnames = list(as.character(row_levels), as.character(col_levels)))
  if (nrow(long) > 0) {
    ri <- match(long$row_v, row_levels)
    ci <- match(long$col_v, col_levels)
    for (k in seq_len(nrow(long))) {
      M[ri[k], ci[k]] <- as.integer(long$n[k])
    }
  }

  sup <- .crossTabSuppress(M, t, band_margins = band_margins)

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
    counts     = sup$matrix,
    suppressed = sup$suppressed
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
