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
