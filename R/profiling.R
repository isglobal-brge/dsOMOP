# ==============================================================================
# dsOMOP v2 - Safe Aggregate Profiling
# ==============================================================================
# Dedicated profiling module. All disclosure-controlled.
# ==============================================================================

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

  if ("rows" %in% stats) {
    sql <- paste0("SELECT COUNT(*) AS n FROM ", qualified)
    n_rows <- .executeQuery(handle, sql)$n[1]
    if (!is.na(n_rows) && n_rows < settings$nfilter_subset) {
      result$rows <- NA_real_
      result$rows_suppressed <- TRUE
    } else {
      result$rows <- n_rows
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
      result$persons <- n_persons
      result$persons_suppressed <- FALSE
    }
  }

  if ("date_range" %in% stats) {
    date_col <- .getDateColumn(bp, table)
    if (!is.null(date_col)) {
      sql <- paste0(
        "SELECT MIN(", date_col, ") AS min_date, ",
        "MAX(", date_col, ") AS max_date ",
        "FROM ", qualified,
        " WHERE ", date_col, " IS NOT NULL"
      )
      date_result <- .executeQuery(handle, sql)
      result$date_range <- list(
        column   = date_col,
        min_date = date_result$min_date[1],
        max_date = date_result$max_date[1]
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
#' @return Named list with column statistics
#' @keywords internal
.profileColumnStats <- function(handle, table, column) {
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
  if (col_df$is_blocked[col_df$column_name == column]) {
    stop("Column '", column, "' is blocked (sensitive).", call. = FALSE)
  }

  qualified <- tbl_row$qualified_name[1]

  sql <- paste0(
    "SELECT ",
    "COUNT(*) AS n_total, ",
    "SUM(CASE WHEN ", column, " IS NULL THEN 1 ELSE 0 END) AS n_missing, ",
    "COUNT(DISTINCT ", column, ") AS n_distinct ",
    "FROM ", qualified
  )
  stats_result <- .executeQuery(handle, sql)

  result <- list(
    n_total = stats_result$n_total[1],
    n_missing = stats_result$n_missing[1],
    n_distinct = stats_result$n_distinct[1]
  )

  # Numeric stats if applicable
  col_type <- col_df$db_datatype[col_df$column_name == column]
  if (grepl("int|float|real|numeric|double|decimal", col_type) ||
      grepl("_as_number$|^quantity$|^range_|^dose_value$", column)) {
    num_sql <- paste0(
      "SELECT MIN(CAST(", column, " AS REAL)) AS min_val, ",
      "MAX(CAST(", column, " AS REAL)) AS max_val, ",
      "AVG(CAST(", column, " AS REAL)) AS mean_val ",
      "FROM ", qualified,
      " WHERE ", column, " IS NOT NULL"
    )
    num_stats <- tryCatch(.executeQuery(handle, num_sql), error = function(e) NULL)
    if (!is.null(num_stats) && nrow(num_stats) > 0) {
      result$min <- num_stats$min_val[1]
      result$max <- num_stats$max_val[1]
      result$mean <- round(num_stats$mean_val[1], 4)
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
      n_persons = if (suppressed) NA_real_ else n,
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
#' @return Data frame with column_name and missing_rate
#' @keywords internal
.profileMissingness <- function(handle, table, columns = NULL) {
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

  total_sql <- paste0("SELECT COUNT(*) AS n FROM ", qualified)
  total <- .executeQuery(handle, total_sql)$n[1]

  if (total == 0) {
    return(data.frame(
      column_name = columns,
      missing_rate = rep(NA_real_, length(columns)),
      stringsAsFactors = FALSE
    ))
  }

  results <- data.frame(
    column_name = character(0),
    missing_rate = numeric(0),
    stringsAsFactors = FALSE
  )

  for (col in columns) {
    sql <- paste0(
      "SELECT COUNT(*) AS n_missing FROM ", qualified,
      " WHERE ", col, " IS NULL"
    )
    n_missing <- .executeQuery(handle, sql)$n_missing[1]
    results <- rbind(results, data.frame(
      column_name = col,
      missing_rate = round(n_missing / total, 4),
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
#' @return Data frame with value and count columns
#' @keywords internal
.profileValueCounts <- function(handle, table, column, top_n = 20,
                                 suppress_small = TRUE) {
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

  if (col_df$is_blocked[col_df$column_name == column]) {
    stop("Column '", column, "' is blocked (sensitive).", call. = FALSE)
  }

  qualified <- tbl_row$qualified_name[1]

  n_total_sql <- paste0("SELECT COUNT(*) AS n FROM ", qualified,
                        " WHERE ", column, " IS NOT NULL")
  n_total <- .executeQuery(handle, n_total_sql)$n[1]

  n_levels_sql <- paste0(
    "SELECT COUNT(DISTINCT ", column, ") AS n FROM ", qualified,
    " WHERE ", column, " IS NOT NULL"
  )
  n_levels <- .executeQuery(handle, n_levels_sql)$n[1]

  .assertSafeLevels(n_levels, n_total)

  effective_limit <- min(as.integer(top_n), 500L)
  sql <- paste0(
    "SELECT TOP ", effective_limit,
    " CAST(", column, " AS VARCHAR) AS value, ",
    "COUNT(*) AS n ",
    "FROM ", qualified, " ",
    "WHERE ", column, " IS NOT NULL ",
    "GROUP BY ", column, " ",
    "ORDER BY COUNT(*) DESC"
  )

  # Translate TOP to LIMIT for sqlite/postgresql
  translated <- .renderSql(handle, sql)
  result <- DBI::dbGetQuery(handle$conn, translated)
  names(result) <- tolower(names(result))

  if (suppress_small) {
    result <- .suppressSmallCounts(result, "n")
  }

  result
}

# ==============================================================================
# Safe Numeric Cutpoints
# ==============================================================================

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
                                   n_bins = 10L) {
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

  # Build WHERE clauses
  where_parts <- paste0(column, " IS NOT NULL")
  if (!is.null(concept_id)) {
    concept_col <- .getDomainConceptColumn(bp, table)
    if (!is.null(concept_col) && concept_col %in% tbl_cols) {
      where_parts <- c(where_parts,
                       paste0(concept_col, " = ", as.integer(concept_id)))
    }
  }
  where_sql <- paste0(" WHERE ", paste(where_parts, collapse = " AND "))

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

# ==============================================================================
# Exploration Profiling (OMOP Studio)
# ==============================================================================

#' Get top concepts in a table by person count or record count
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param concept_col Character; concept column name (NULL = auto-detect)
#' @param metric Character; "persons" or "records"
#' @param top_n Integer; number of top concepts to return
#' @param cohort_table Character; cohort temp table name for filtering (NULL)
#' @param window List with start and end dates for filtering (NULL)
#' @return Data frame with concept_id, concept_name, n_persons, n_records
#' @keywords internal
.profileConceptPrevalence <- function(handle, table, concept_col = NULL,
                                       metric = "persons", top_n = 50L,
                                       cohort_table = NULL, window = NULL) {
  table <- tolower(.validateIdentifier(table, "table"))
  bp <- .buildBlueprint(handle)
  settings <- .omopDisclosureSettings()

  tbl_row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db, ,
                       drop = FALSE]
  if (nrow(tbl_row) == 0) stop("Table '", table, "' not found.", call. = FALSE)

  qualified <- tbl_row$qualified_name[1]
  col_df <- bp$columns[[table]]
  tbl_cols <- col_df$column_name

  # Auto-detect concept column if not provided
  if (is.null(concept_col)) {
    concept_col <- .getDomainConceptColumn(bp, table)
    if (is.null(concept_col)) {
      stop("No domain concept column found for table '", table,
           "'. Provide concept_col explicitly.", call. = FALSE)
    }
  } else {
    concept_col <- tolower(.validateIdentifier(concept_col, "column"))
  }

  if (!concept_col %in% tbl_cols) {
    stop("Column '", concept_col, "' not found in '", table, "'.", call. = FALSE)
  }

  metric <- match.arg(metric, c("persons", "records"))
  effective_top_n <- min(as.integer(top_n), 500L)

  # Check minimum person count before returning any data
  if ("person_id" %in% tbl_cols) {
    person_count_sql <- paste0("SELECT COUNT(DISTINCT person_id) AS n FROM ", qualified)
    n_total_persons <- .executeQuery(handle, person_count_sql)$n[1]
    .assertMinPersons(n_persons = n_total_persons)
  }

  # Build FROM / JOIN clauses
  from_clause <- paste0(qualified, " AS t")
  where_parts <- character(0)

  # Cohort join
  if (!is.null(cohort_table) && "person_id" %in% tbl_cols) {
    cohort_table <- .validateIdentifier(cohort_table, "cohort_table")
    from_clause <- paste0(from_clause,
                          " INNER JOIN ", cohort_table, " AS coh",
                          " ON t.person_id = coh.subject_id")
  }

  # Time window filter
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

  where_sql <- ""
  if (length(where_parts) > 0) {
    where_sql <- paste0(" WHERE ", paste(where_parts, collapse = " AND "))
  }

  # Build main aggregation query
  order_col <- if (metric == "persons") "n_persons" else "n_records"

  if ("person_id" %in% tbl_cols) {
    select_expr <- paste0(
      "SELECT TOP ", effective_top_n, " t.", concept_col, " AS concept_id, ",
      "COUNT(DISTINCT t.person_id) AS n_persons, ",
      "COUNT(*) AS n_records"
    )
  } else {
    select_expr <- paste0(
      "SELECT TOP ", effective_top_n, " t.", concept_col, " AS concept_id, ",
      "COUNT(*) AS n_records"
    )
  }

  sql <- paste0(
    select_expr,
    " FROM ", from_clause,
    where_sql,
    " GROUP BY t.", concept_col,
    " ORDER BY ", order_col, " DESC"
  )

  translated <- .renderSql(handle, sql)
  result <- DBI::dbGetQuery(handle$conn, translated)
  names(result) <- tolower(names(result))

  if (nrow(result) == 0) {
    return(data.frame(concept_id = integer(0), concept_name = character(0),
                      n_persons = numeric(0), n_records = numeric(0),
                      stringsAsFactors = FALSE))
  }

  # Suppress small counts
  if ("n_persons" %in% names(result)) {
    result <- .suppressSmallCounts(result, "n_persons")
  }
  result <- .suppressSmallCounts(result, "n_records")

  # Decorate with concept names from vocabulary
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

  # Ensure consistent column order
  out_cols <- intersect(c("concept_id", "concept_name", "n_persons", "n_records"),
                        names(result))
  result[, out_cols, drop = FALSE]
}

#' Compute a safe histogram with suppressed low-count bins
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param bins Integer; number of bins
#' @param cohort_table Character; cohort temp table name (NULL)
#' @param window List with start/end dates (NULL)
#' @return Data frame with bin_start, bin_end, count, suppressed
#' @keywords internal
.profileNumericHistogram <- function(handle, table, value_col,
                                      bins = 20L, cohort_table = NULL,
                                      window = NULL) {
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

  where_sql <- paste0(" WHERE ", paste(where_parts, collapse = " AND "))

  # Get total non-NULL count first
  count_sql <- paste0("SELECT COUNT(*) AS n FROM ", from_clause, where_sql)
  n_total <- .executeQuery(handle, count_sql)$n[1]

  if (is.na(n_total) || n_total == 0) {
    return(data.frame(bin_start = numeric(0), bin_end = numeric(0),
                      count = integer(0), suppressed = logical(0),
                      stringsAsFactors = FALSE))
  }

  # Compute safe range using 5th and 95th percentile approximations
  # (avoid exact min/max for privacy)
  offset_p05 <- max(0L, as.integer(floor(n_total * 0.05)) - 1L)
  offset_p95 <- max(0L, as.integer(floor(n_total * 0.95)) - 1L)

  if (handle$target_dialect == "sqlite") {
    # SQLite: ORDER BY + LIMIT + OFFSET to approximate percentiles
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
    # PostgreSQL / others: use OHDSI SQL TOP convention, rendered via .renderSql
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
    # Fallback: if percentiles are equal, return single bin
    return(data.frame(
      bin_start = p05_val %||% 0,
      bin_end = p95_val %||% 0,
      count = n_total,
      suppressed = n_total < settings$nfilter_tab,
      stringsAsFactors = FALSE
    ))
  }

  # Build equal-width bins across the safe range
  bin_width <- (p95_val - p05_val) / bins
  breaks <- seq(p05_val, p95_val, by = bin_width)
  # Ensure we have exactly bins + 1 breaks
  if (length(breaks) < bins + 1L) {
    breaks <- c(breaks, p95_val)
  }
  breaks <- breaks[seq_len(bins + 1L)]

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
    cnt <- as.integer(cnt)
    if (!is.na(cnt) && cnt < settings$nfilter_tab) {
      result$count[i] <- NA_integer_
      result$suppressed[i] <- TRUE
    } else {
      result$count[i] <- cnt
      result$suppressed[i] <- FALSE
    }
  }

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
#' @return Data frame with probability and value
#' @keywords internal
.profileNumericQuantiles <- function(handle, table, value_col,
                                      probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                      cohort_table = NULL, window = NULL,
                                      rounding = 2L) {
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

  where_sql <- paste0(" WHERE ", paste(where_parts, collapse = " AND "))

  # Get total non-NULL count
  count_sql <- paste0("SELECT COUNT(*) AS n FROM ", from_clause, where_sql)
  n_total <- .executeQuery(handle, count_sql)$n[1]

  # Block if total non-NULL values < nfilter_subset
  if (is.na(n_total) || n_total < settings$nfilter_subset) {
    stop("Disclosive: non-NULL value count (", ifelse(is.na(n_total), "NA", n_total),
         ") < nfilter.subset (", settings$nfilter_subset,
         "). Operation blocked.", call. = FALSE)
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

  # Suppress bins with count < nfilter_tab
  result$suppressed <- !is.na(result$n_records) &
    result$n_records < settings$nfilter_tab
  result$n_records[result$suppressed] <- NA_integer_

  result
}

# ==============================================================================
# Concept Drilldown & Locator
# ==============================================================================

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

  # Auto-detect concept column
  if (is.null(concept_col)) {
    concept_col <- .getDomainConceptColumn(bp, table)
    if (is.null(concept_col)) {
      stop("No domain concept column found for table '", table,
           "'. Provide concept_col explicitly.", call. = FALSE)
    }
  } else {
    concept_col <- tolower(.validateIdentifier(concept_col, "column"))
  }
  if (!concept_col %in% tbl_cols) {
    stop("Column '", concept_col, "' not found in '", table, "'.",
         call. = FALSE)
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
            if (!is.na(cnt) && cnt < settings$nfilter_tab) {
              histogram$count[j] <- NA_integer_
              histogram$suppressed[j] <- TRUE
            } else {
              histogram$count[j] <- cnt
              histogram$suppressed[j] <- FALSE
            }
          }
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
      off_p05 <- max(0L, as.integer(floor(n_dates * 0.05)) - 1L)
      off_p95 <- max(0L, as.integer(floor(n_dates * 0.95)) - 1L)

      p05_sql <- paste0(
        "SELECT ", date_col, " AS val FROM ", qualified,
        " WHERE ", where_concept,
        " AND ", date_col, " IS NOT NULL",
        " ORDER BY ", date_col, " ASC LIMIT 1 OFFSET ", off_p05
      )
      p95_sql <- paste0(
        "SELECT ", date_col, " AS val FROM ", qualified,
        " WHERE ", where_concept,
        " AND ", date_col, " IS NOT NULL",
        " ORDER BY ", date_col, " ASC LIMIT 1 OFFSET ", off_p95
      )

      min_date_safe <- tryCatch(.executeQuery(handle, p05_sql)$val[1],
                                error = function(e) NA_character_)
      max_date_safe <- tryCatch(.executeQuery(handle, p95_sql)$val[1],
                                error = function(e) NA_character_)

      # Date counts by year for concept-filtered records
      if (handle$target_dialect == "sqlite") {
        date_expr <- paste0("strftime('%Y', ", date_col, ")")
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
        dc_result$suppressed <- !is.na(dc_result$n_records) &
          dc_result$n_records < settings$nfilter_tab
        dc_result$n_records[dc_result$suppressed] <- NA_integer_
      }

      date_range <- list(
        column = date_col,
        min_date_safe = min_date_safe,
        max_date_safe = max_date_safe,
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

  # Suppress small counts
  if (nrow(results) > 0) {
    results <- .suppressSmallCounts(results, "n_records")
    results <- .suppressSmallCounts(results, "n_persons")
    # Filter out rows where n_records == 0 or is NA after suppression
    results <- results[is.na(results$n_records) | results$n_records > 0, ,
                       drop = FALSE]
  }

  results
}
