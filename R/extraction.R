# ==============================================================================
# dsOMOP v2 - SQL Compilation + Execution + Representations
# ==============================================================================
# Replaces v2's query.R + execution.R. Uses blueprint throughout.
# ==============================================================================

# --- Temporal Filtering ---

#' Compile temporal spec into SQL WHERE fragments
#'
#' @param handle CDM handle
#' @param temporal List; temporal filtering specification
#' @param alias Character; table alias
#' @param date_col Character; date column name
#' @return Character vector of WHERE clause fragments
#' @keywords internal
.compileTemporalWhere <- function(handle, temporal, alias = "t",
                                  date_col = NULL) {
  if (is.null(temporal) || length(temporal) == 0) return(character(0))
  where <- character(0)

  # Index-relative window: days relative to cohort_start_date
  if (!is.null(temporal$index_window) && !is.null(date_col)) {
    iw <- temporal$index_window
    if (!is.null(iw$start)) {
      where <- c(where, paste0(
        alias, ".", date_col, " >= ",
        .renderSql(handle, "DATEADD(day, @days, c.cohort_start_date)",
                   days = as.integer(iw$start))
      ))
    }
    if (!is.null(iw$end)) {
      where <- c(where, paste0(
        alias, ".", date_col, " <= ",
        .renderSql(handle, "DATEADD(day, @days, c.cohort_start_date)",
                   days = as.integer(iw$end))
      ))
    }
  }

  # Calendar time filter
  if (!is.null(temporal$calendar) && !is.null(date_col)) {
    cal <- temporal$calendar
    if (!is.null(cal$start)) {
      where <- c(where, paste0(
        alias, ".", date_col, " >= '", cal$start, "'"
      ))
    }
    if (!is.null(cal$end)) {
      where <- c(where, paste0(
        alias, ".", date_col, " <= '", cal$end, "'"
      ))
    }
  }

  where
}

#' Wrap SQL in ROW_NUMBER CTE for event selection
#'
#' @param handle CDM handle
#' @param sql Character; base SQL query
#' @param temporal List; temporal spec with event_select
#' @param date_col Character; date column for ordering
#' @return Character; possibly wrapped SQL
#' @keywords internal
.wrapEventSelect <- function(handle, sql, temporal, date_col = NULL) {
  if (is.null(temporal$event_select) || is.null(date_col)) return(sql)

  es <- temporal$event_select
  order_dir <- if (identical(es$order, "last")) "DESC" else "ASC"
  n <- as.integer(es$n %||% 1L)

  rn_expr <- paste0("ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY ",
                     date_col, " ", order_dir, ")")

  paste0(
    "SELECT * FROM (SELECT sub.*, ", rn_expr, " AS rn ",
    "FROM (", sql, ") AS sub) AS ranked WHERE ranked.rn <= ", n
  )
}

#' Apply date handling transforms to a result data frame
#'
#' @param df Data frame
#' @param date_handling List; date handling specification
#' @param index_date_col Character; column with index dates (for relative mode)
#' @return Transformed data frame
#' @keywords internal
.applyDateHandling <- function(df, date_handling, index_date_col = NULL) {
  if (is.null(date_handling) || nrow(df) == 0) return(df)

  mode <- date_handling$mode %||% "absolute"
  if (mode == "absolute") return(df)

  # Identify date columns to transform
  date_columns <- date_handling$date_columns
  if (is.null(date_columns)) {
    date_columns <- grep("_date$|_datetime$", names(df), value = TRUE)
  }
  date_columns <- intersect(date_columns, names(df))
  if (length(date_columns) == 0) return(df)

  if (mode == "remove") {
    df <- df[, setdiff(names(df), date_columns), drop = FALSE]
    return(df)
  }

  if (mode == "relative") {
    ref_col <- NULL
    if (!is.null(index_date_col) && index_date_col %in% names(df)) {
      ref_col <- index_date_col
    } else if ("cohort_start_date" %in% names(df)) {
      ref_col <- "cohort_start_date"
    }
    if (!is.null(ref_col)) {
      ref_dates <- as.Date(df[[ref_col]])
      for (col in date_columns) {
        if (col == ref_col) next
        col_dates <- tryCatch(as.Date(df[[col]]), error = function(e) NULL)
        if (!is.null(col_dates)) {
          df[[col]] <- as.integer(col_dates - ref_dates)
        }
      }
      # Remove the reference column itself after computing relative days
      if (ref_col %in% date_columns) {
        df[[ref_col]] <- 0L
      }
    }
    return(df)
  }

  if (mode == "binned") {
    bin_width <- date_handling$bin_width %||% "month"
    for (col in date_columns) {
      col_dates <- tryCatch(as.Date(df[[col]]), error = function(e) NULL)
      if (!is.null(col_dates)) {
        df[[col]] <- switch(bin_width,
          "year"  = format(col_dates, "%Y-01-01"),
          "month" = format(col_dates, "%Y-%m-01"),
          "week"  = {
            # Truncate to start of week (Monday)
            wday <- as.integer(format(col_dates, "%u"))
            as.character(col_dates - (wday - 1L))
          },
          as.character(col_dates)
        )
      }
    }
    return(df)
  }

  df
}

# --- SQL Compilation ---

#' Quote a SQL literal value safely
#'
#' @param x Value to quote
#' @return Character; quoted SQL literal
#' @keywords internal
.quoteLiteral <- function(x) {
  if (is.numeric(x)) return(as.character(x))
  paste0("'", gsub("'", "''", as.character(x)), "'")
}

#' Compile a SQL SELECT for a table extraction
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param columns Character vector; columns to select (NULL = all)
#' @param concept_filter Numeric vector; concept IDs to filter on
#' @param person_ids Numeric vector; person IDs to restrict to
#' @param time_window Named list with start_date, end_date, date_column
#' @param cohort_table Character; name of cohort temp table
#' @param limit Integer; max rows
#' @param block_sensitive Logical; block sensitive columns (default TRUE)
#' @param temporal List; temporal filtering spec (index_window, calendar, etc.)
#' @return Character; compiled SQL statement
#' @keywords internal
.compileSelect <- function(handle, table, columns = NULL,
                           concept_filter = NULL, person_ids = NULL,
                           time_window = NULL, cohort_table = NULL,
                           limit = NULL, block_sensitive = TRUE,
                           temporal = NULL, add_cohort_date = FALSE) {
  bp <- .buildBlueprint(handle)

  table_lower <- tolower(table)
  tbl_row <- bp$tables[bp$tables$table_name == table_lower, , drop = FALSE]
  if (nrow(tbl_row) == 0 || !tbl_row$present_in_db[1]) {
    stop("Table '", table, "' not found in CDM schema.", call. = FALSE)
  }

  col_df <- bp$columns[[table_lower]]
  if (is.null(col_df) || nrow(col_df) == 0) {
    stop("No columns found for table '", table, "'.", call. = FALSE)
  }

  concept_col <- .getDomainConceptColumn(bp, table_lower)
  has_concept_col <- !is.null(concept_col) && concept_col %in% col_df$column_name
  has_person_id <- "person_id" %in% col_df$column_name

  # Determine columns to select
  if (is.null(columns)) {
    select_cols <- col_df$column_name
  } else {
    columns <- tolower(columns)
    must_keep <- character(0)
    if (has_person_id) must_keep <- c(must_keep, "person_id")
    if (has_concept_col && !is.null(concept_filter)) {
      must_keep <- c(must_keep, concept_col)
    }
    select_cols <- unique(c(must_keep, intersect(columns, col_df$column_name)))
  }

  # Block sensitive columns
  if (block_sensitive) {
    blocked <- col_df$column_name[col_df$is_blocked]
    select_cols <- setdiff(select_cols, blocked)
  }

  if (length(select_cols) == 0) {
    stop("No columns available to select after filtering.", call. = FALSE)
  }

  # Resolve qualified table name
  schema <- .resolveTableSchema(handle, table_lower, tbl_row$schema_category[1])
  qualified_table <- .qualifyTable(handle, table_lower, schema)
  t_alias <- "t"

  select_parts <- paste(
    vapply(select_cols, function(c) paste0(t_alias, ".", c), character(1)),
    collapse = ", "
  )

  # Determine if we need a JOIN (for index_window) or EXISTS (default)
  needs_cohort_join <- !is.null(temporal$index_window) &&
    !is.null(cohort_table) && has_person_id

  # Add cohort_start_date to SELECT when requested and cohort join is active
  if (add_cohort_date && needs_cohort_join) {
    select_parts <- paste0(select_parts, ", c.cohort_start_date")
  }

  # Use TOP for limit (OHDSI SQL convention, translated to LIMIT by .sql_translate)
  if (!is.null(limit)) {
    sql <- paste0("SELECT TOP ", as.integer(limit), " ", select_parts,
                  " FROM ", qualified_table, " AS ", t_alias)
  } else {
    sql <- paste0("SELECT ", select_parts, " FROM ", qualified_table, " AS ", t_alias)
  }

  # Build WHERE clauses
  where <- character(0)

  if (!is.null(concept_filter) && has_concept_col) {
    ids <- paste(as.integer(concept_filter), collapse = ", ")
    where <- c(where, paste0(t_alias, ".", concept_col, " IN (", ids, ")"))
  }

  if (!is.null(person_ids) && has_person_id) {
    ids <- paste(as.integer(person_ids), collapse = ", ")
    where <- c(where, paste0(t_alias, ".person_id IN (", ids, ")"))
  }

  if (needs_cohort_join) {
    sql <- paste0(sql, " INNER JOIN ", cohort_table,
                  " AS c ON c.subject_id = ", t_alias, ".person_id")
  } else if (!is.null(cohort_table) && has_person_id) {
    where <- c(where, paste0(
      "EXISTS (SELECT 1 FROM ", cohort_table,
      " AS c WHERE c.subject_id = ", t_alias, ".person_id)"
    ))
  }

  # Temporal WHERE clauses
  if (!is.null(temporal) && has_person_id) {
    date_col_temporal <- .getDateColumn(bp, table_lower)
    temporal_where <- .compileTemporalWhere(
      handle, temporal, t_alias, date_col_temporal
    )
    where <- c(where, temporal_where)
  }

  if (!is.null(time_window)) {
    date_col <- time_window$date_column %||% .getDateColumn(bp, table_lower)
    if (!is.null(date_col) && date_col %in% col_df$column_name) {
      if (!is.null(time_window$start_date)) {
        where <- c(where, paste0(
          t_alias, ".", date_col, " >= ", .quoteLiteral(time_window$start_date)
        ))
      }
      if (!is.null(time_window$end_date)) {
        where <- c(where, paste0(
          t_alias, ".", date_col, " <= ", .quoteLiteral(time_window$end_date)
        ))
      }
    }
  }

  if (length(where) > 0) {
    sql <- paste0(sql, " WHERE ", paste(where, collapse = " AND "))
  }

  # Translate complete SQL to target dialect (TOP -> LIMIT, etc.)
  .sql_translate(sql, handle$target_dialect)
}

#' Compile SQL to count distinct persons
#'
#' @param handle CDM handle
#' @param from_sql Character; the full SELECT query
#' @return Character; SQL returning single count
#' @keywords internal
.compilePersonCount <- function(handle, from_sql) {
  paste0("SELECT COUNT(DISTINCT person_id) AS n_persons FROM (", from_sql, ") AS sub")
}

#' Compile SQL for concept lookup
#'
#' @param handle CDM handle
#' @param concept_ids Numeric vector
#' @return Character; SQL returning concept_id, concept_name
#' @keywords internal
.compileConceptLookup <- function(handle, concept_ids) {
  bp <- .buildBlueprint(handle)

  if (!"concept" %in% bp$tables$table_name[bp$tables$present_in_db]) {
    return(NULL)
  }

  schema <- .resolveTableSchema(handle, "concept", "Vocabulary")
  concept_table <- .qualifyTable(handle, "concept", schema)
  ids <- paste(as.integer(concept_ids), collapse = ", ")

  paste0(
    "SELECT concept_id, concept_name",
    " FROM ", concept_table,
    " WHERE concept_id IN (", ids, ")"
  )
}

#' Compile a filter DSL structure into SQL WHERE fragments
#'
#' @param handle CDM handle
#' @param filter List; the filter structure
#' @param table_alias Character; table alias
#' @param valid_columns Character vector; whitelist
#' @return Character; SQL WHERE fragment
#' @keywords internal
.compileFilter <- function(handle, filter, table_alias = "t",
                           valid_columns = NULL) {
  if (is.null(filter) || length(filter) == 0) return(NULL)

  if ("and" %in% names(filter)) {
    parts <- vapply(filter$and, function(f) {
      .compileFilter(handle, f, table_alias, valid_columns)
    }, character(1))
    parts <- parts[nchar(parts) > 0]
    if (length(parts) == 0) return("")
    return(paste0("(", paste(parts, collapse = " AND "), ")"))
  }

  if ("or" %in% names(filter)) {
    parts <- vapply(filter$or, function(f) {
      .compileFilter(handle, f, table_alias, valid_columns)
    }, character(1))
    parts <- parts[nchar(parts) > 0]
    if (length(parts) == 0) return("")
    return(paste0("(", paste(parts, collapse = " OR "), ")"))
  }

  var <- tolower(filter$var)
  op <- tolower(filter$op)
  value <- filter$value

  .validateIdentifier(var, "filter column")
  if (!is.null(valid_columns) && !var %in% valid_columns) {
    stop("Filter column '", var, "' not in table.", call. = FALSE)
  }

  col_ref <- paste0(table_alias, ".", var)

  switch(op,
    "==" =, "eq" = paste0(col_ref, " = ", .quoteLiteral(value)),
    "!=" =, "ne" = paste0(col_ref, " != ", .quoteLiteral(value)),
    ">=" =, "gte" = paste0(col_ref, " >= ", .quoteLiteral(value)),
    "<=" =, "lte" = paste0(col_ref, " <= ", .quoteLiteral(value)),
    ">"  =, "gt"  = paste0(col_ref, " > ", .quoteLiteral(value)),
    "<"  =, "lt"  = paste0(col_ref, " < ", .quoteLiteral(value)),
    "in" = {
      vals <- paste(vapply(value, .quoteLiteral, character(1)), collapse = ", ")
      paste0(col_ref, " IN (", vals, ")")
    },
    "not_in" = {
      vals <- paste(vapply(value, .quoteLiteral, character(1)), collapse = ", ")
      paste0(col_ref, " NOT IN (", vals, ")")
    },
    "between" = {
      paste0(col_ref, " BETWEEN ", .quoteLiteral(value[1]),
             " AND ", .quoteLiteral(value[2]))
    },
    "is_null"  = paste0(col_ref, " IS NULL"),
    "not_null" = paste0(col_ref, " IS NOT NULL"),
    stop("Unknown filter op: '", op, "'", call. = FALSE)
  )
}

# --- Query Execution ---

#' Execute a SQL query and return a data frame
#'
#' @param handle CDM handle
#' @param sql Character; SQL to execute
#' @return Data frame
#' @keywords internal
.executeQuery <- function(handle, sql) {
  result <- DBI::dbGetQuery(handle$conn, sql)
  names(result) <- tolower(names(result))
  result
}

#' Execute a SQL statement (DDL/DML, no result set)
#'
#' @param handle CDM handle
#' @param sql Character; SQL to execute
#' @return Number of affected rows (invisible)
#' @keywords internal
.executeStatement <- function(handle, sql) {
  invisible(DBI::dbExecute(handle$conn, sql))
}

# --- Main Extraction ---

#' Extract a table as a data frame with all filters applied
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param columns Character vector; columns to select
#' @param concept_filter Numeric vector; concept IDs
#' @param person_ids Numeric vector; person IDs
#' @param time_window Named list
#' @param cohort_table Character; cohort temp table name
#' @param translate_concepts Logical; replace concept IDs with names
#' @param representation Character; "long", "wide", or "features"
#' @param feature_specs Named list; for features mode
#' @param block_sensitive Logical; block sensitive columns
#' @param temporal List; temporal filtering spec
#' @param date_handling List; date handling spec
#' @return Data frame
#' @keywords internal
.extractTable <- function(handle, table, columns = NULL,
                          concept_filter = NULL, person_ids = NULL,
                          time_window = NULL, cohort_table = NULL,
                          translate_concepts = FALSE,
                          representation = "long",
                          feature_specs = NULL,
                          block_sensitive = TRUE,
                          temporal = NULL,
                          date_handling = NULL,
                          add_cohort_date = FALSE) {

  sql <- .compileSelect(
    handle, table,
    columns = columns,
    concept_filter = concept_filter,
    person_ids = person_ids,
    time_window = time_window,
    cohort_table = cohort_table,
    block_sensitive = block_sensitive,
    temporal = temporal,
    add_cohort_date = add_cohort_date
  )

  # Wrap with event selection (ROW_NUMBER) if requested
  if (!is.null(temporal$event_select)) {
    bp <- .buildBlueprint(handle)
    ev_date_col <- .getDateColumn(bp, tolower(table))
    sql <- .wrapEventSelect(handle, sql, temporal, ev_date_col)
  }

  bp <- .buildBlueprint(handle)
  col_df <- bp$columns[[tolower(table)]]
  has_person_id <- "person_id" %in% col_df$column_name

  if (has_person_id) {
    count_sql <- .compilePersonCount(handle, sql)
    .assertMinPersons(conn = handle$conn, sql = count_sql)
  }

  result <- .executeQuery(handle, sql)

  if (nrow(result) == 0) return(result)

  # Compute days_from_index when cohort_start_date is present
  if ("cohort_start_date" %in% names(result)) {
    date_col_for_index <- .getDateColumn(bp, tolower(table))
    if (!is.null(date_col_for_index) && date_col_for_index %in% names(result)) {
      result$days_from_index <- as.integer(
        as.Date(result[[date_col_for_index]]) -
        as.Date(result$cohort_start_date)
      )
    }
    # Remove cohort_start_date from output (internal use only)
    result$cohort_start_date <- NULL
  }

  if (translate_concepts) {
    result <- .vocabTranslateColumns(handle, result)
  }

  result <- .convertTypes(result)

  # Apply date handling transforms
  if (!is.null(date_handling)) {
    result <- .applyDateHandling(result, date_handling)
  }

  result <- switch(representation,
    "long" = result,
    "wide" = .toWide(result, table),
    "features" = .toFeatures(result, table, feature_specs),
    "sparse" = .toSparse(result, table),
    result
  )

  result
}

# --- Type Conversion ---

#' Convert data types in a result data frame
#'
#' @param df Data frame
#' @return Data frame with converted types
#' @keywords internal
.convertTypes <- function(df) {
  if (nrow(df) == 0) return(df)

  for (col in names(df)) {
    if (grepl("_date$", col) && !grepl("_datetime$", col)) {
      df[[col]] <- tryCatch(as.Date(df[[col]]), error = function(e) df[[col]])
    }
    if (grepl("_datetime$", col)) {
      df[[col]] <- tryCatch(as.POSIXct(df[[col]]), error = function(e) df[[col]])
    }
    if (grepl("_as_number$|^range_low$|^range_high$|^quantity$|^dose_value$", col)) {
      df[[col]] <- tryCatch(as.numeric(df[[col]]), error = function(e) df[[col]])
    }
  }
  df
}

#' Standardize a concept name for column names
#'
#' @param name Character
#' @return Character; standardized name
#' @keywords internal
.standardizeName <- function(name) {
  if (is.na(name) || is.null(name)) return(NA_character_)
  name <- make.names(name)
  name <- tolower(name)
  name <- gsub("\\.", "_", name)
  name <- gsub("_+", "_", name)
  name <- gsub("^_|_$", "", name)
  name
}

# --- Representations ---

#' Transform a long result to wide format
#'
#' Pivots on the domain concept column (from blueprint).
#'
#' @param df Data frame in long format
#' @param table Character; source table name
#' @return Data frame in wide format
#' @keywords internal
.toWide <- function(df, table) {
  bp <- .buildBlueprint(get(".current_handle", envir = environment()) %||% NULL)
  concept_col <- NULL

  # Try to find concept column from column names
  possible <- grep("_concept_id$", names(df), value = TRUE)
  # Filter out type and source concepts
  possible <- possible[!grepl("_type_concept_id$|_source_concept_id$", possible)]
  if (length(possible) > 0) concept_col <- possible[1]

  if (is.null(concept_col) || !concept_col %in% names(df)) return(df)
  if (!"person_id" %in% names(df)) return(df)

  value_cols <- setdiff(names(df), c("person_id", concept_col))
  if (length(value_cols) == 0) return(df)

  df$.seq <- ave(seq_len(nrow(df)),
                 df$person_id, df[[concept_col]],
                 FUN = seq_along)

  concepts <- unique(df[[concept_col]])
  persons <- unique(df$person_id)

  wide <- data.frame(person_id = persons, stringsAsFactors = FALSE)

  for (concept in concepts) {
    concept_data <- df[df[[concept_col]] == concept, , drop = FALSE]
    concept_label <- .standardizeName(as.character(concept))
    if (is.na(concept_label) || concept_label == "") {
      concept_label <- paste0("concept_", concept)
    }

    for (vcol in value_cols) {
      if (vcol == ".seq") next
      col_name <- paste0(concept_label, ".", vcol)
      first_occ <- concept_data[concept_data$.seq == 1, c("person_id", vcol),
                                drop = FALSE]
      names(first_occ)[2] <- col_name
      wide <- merge(wide, first_occ, by = "person_id", all.x = TRUE)
    }
  }

  wide
}

#' Compute person-level features from event data
#'
#' @param df Data frame in long format
#' @param table Character; source table name
#' @param specs Named list of feature specifications
#' @return Data frame with one row per person
#' @keywords internal
.toFeatures <- function(df, table, specs = NULL) {
  if (!"person_id" %in% names(df)) return(df)

  # Find concept column
  possible <- grep("_concept_id$", names(df), value = TRUE)
  possible <- possible[!grepl("_type_concept_id$|_source_concept_id$", possible)]
  concept_col <- if (length(possible) > 0) possible[1] else NULL

  persons <- unique(df$person_id)
  features <- data.frame(person_id = persons, stringsAsFactors = FALSE)

  if (!is.null(concept_col) && concept_col %in% names(df)) {
    concepts <- unique(df[[concept_col]])

    for (concept in concepts) {
      concept_label <- .standardizeName(as.character(concept))
      if (is.na(concept_label) || concept_label == "") {
        concept_label <- paste0("concept_", concept)
      }

      concept_data <- df[df[[concept_col]] == concept, , drop = FALSE]

      # Count feature
      count_df <- stats::aggregate(
        concept_data[[concept_col]],
        by = list(person_id = concept_data$person_id),
        FUN = length
      )
      names(count_df)[2] <- paste0(concept_label, ".count")
      features <- merge(features, count_df, by = "person_id", all.x = TRUE)
      feat_col <- paste0(concept_label, ".count")
      features[[feat_col]][is.na(features[[feat_col]])] <- 0L

      # Boolean feature
      features[[paste0(concept_label, ".ever")]] <-
        as.integer(features$person_id %in% concept_data$person_id)

      # Numeric features
      if ("value_as_number" %in% names(concept_data)) {
        num_data <- concept_data[!is.na(concept_data$value_as_number), ,
                                 drop = FALSE]
        if (nrow(num_data) > 0) {
          for (stat in c("mean", "min", "max")) {
            stat_df <- stats::aggregate(
              num_data$value_as_number,
              by = list(person_id = num_data$person_id),
              FUN = stat
            )
            names(stat_df)[2] <- paste0(concept_label, ".value_", stat)
            features <- merge(features, stat_df, by = "person_id", all.x = TRUE)
          }
        }
      }
    }
  } else {
    count_df <- stats::aggregate(
      rep(1, nrow(df)),
      by = list(person_id = df$person_id),
      FUN = sum
    )
    names(count_df)[2] <- "n_records"
    features <- merge(features, count_df, by = "person_id", all.x = TRUE)
    features$n_records[is.na(features$n_records)] <- 0L
  }

  features
}

#' Transform event data to FeatureExtraction-compatible sparse format
#'
#' Produces a named list with \code{covariates} (sparse triplet) and
#' \code{covariateRef} (reference table). CovariateId scheme:
#' \code{conceptId * 1000 + analysisId} where analysisId 1=binary,
#' 2=count, 3=mean, 4=min, 5=max.
#'
#' @param df Data frame in long format
#' @param table Character; source table name
#' @return Named list with \code{covariates} and \code{covariateRef}
#' @keywords internal
.toSparse <- function(df, table) {
  if (!"person_id" %in% names(df)) {
    return(list(
      covariates = data.frame(rowId = integer(0), covariateId = numeric(0),
                              covariateValue = numeric(0),
                              stringsAsFactors = FALSE),
      covariateRef = data.frame(covariateId = numeric(0),
                                covariateName = character(0),
                                analysisId = integer(0),
                                conceptId = integer(0),
                                stringsAsFactors = FALSE)
    ))
  }

  # Find concept column
  possible <- grep("_concept_id$", names(df), value = TRUE)
  possible <- possible[!grepl("_type_concept_id$|_source_concept_id$", possible)]
  concept_col <- if (length(possible) > 0) possible[1] else NULL

  # Build row_id mapping (unique person_id -> rowId)
  persons <- sort(unique(df$person_id))
  row_map <- stats::setNames(seq_along(persons), as.character(persons))

  covariates <- data.frame(rowId = integer(0), covariateId = numeric(0),
                           covariateValue = numeric(0),
                           stringsAsFactors = FALSE)
  covariateRef <- data.frame(covariateId = numeric(0),
                             covariateName = character(0),
                             analysisId = integer(0),
                             conceptId = integer(0),
                             stringsAsFactors = FALSE)

  if (!is.null(concept_col) && concept_col %in% names(df)) {
    concepts <- sort(unique(df[[concept_col]]))

    for (cid in concepts) {
      concept_label <- .standardizeName(as.character(cid))
      if (is.na(concept_label) || concept_label == "") {
        concept_label <- paste0("concept_", cid)
      }
      concept_data <- df[df[[concept_col]] == cid, , drop = FALSE]

      # Analysis 1: binary (ever/never)
      binary_persons <- unique(concept_data$person_id)
      if (length(binary_persons) > 0) {
        cov_id <- as.numeric(cid) * 1000 + 1
        covariates <- rbind(covariates, data.frame(
          rowId = as.integer(row_map[as.character(binary_persons)]),
          covariateId = rep(cov_id, length(binary_persons)),
          covariateValue = rep(1, length(binary_persons)),
          stringsAsFactors = FALSE
        ))
        covariateRef <- rbind(covariateRef, data.frame(
          covariateId = cov_id,
          covariateName = paste0(concept_label, "_binary"),
          analysisId = 1L,
          conceptId = as.integer(cid),
          stringsAsFactors = FALSE
        ))
      }

      # Analysis 2: count
      count_agg <- stats::aggregate(
        concept_data[[concept_col]],
        by = list(person_id = concept_data$person_id),
        FUN = length
      )
      if (nrow(count_agg) > 0) {
        cov_id <- as.numeric(cid) * 1000 + 2
        covariates <- rbind(covariates, data.frame(
          rowId = as.integer(row_map[as.character(count_agg$person_id)]),
          covariateId = rep(cov_id, nrow(count_agg)),
          covariateValue = as.numeric(count_agg$x),
          stringsAsFactors = FALSE
        ))
        covariateRef <- rbind(covariateRef, data.frame(
          covariateId = cov_id,
          covariateName = paste0(concept_label, "_count"),
          analysisId = 2L,
          conceptId = as.integer(cid),
          stringsAsFactors = FALSE
        ))
      }

      # Numeric analyses (3=mean, 4=min, 5=max)
      if ("value_as_number" %in% names(concept_data)) {
        num_data <- concept_data[!is.na(concept_data$value_as_number), ,
                                 drop = FALSE]
        if (nrow(num_data) > 0) {
          for (analysis in list(list(id = 3L, fn = mean, nm = "mean"),
                                list(id = 4L, fn = min, nm = "min"),
                                list(id = 5L, fn = max, nm = "max"))) {
            stat_agg <- stats::aggregate(
              num_data$value_as_number,
              by = list(person_id = num_data$person_id),
              FUN = analysis$fn
            )
            if (nrow(stat_agg) > 0) {
              cov_id <- as.numeric(cid) * 1000 + analysis$id
              covariates <- rbind(covariates, data.frame(
                rowId = as.integer(row_map[as.character(stat_agg$person_id)]),
                covariateId = rep(cov_id, nrow(stat_agg)),
                covariateValue = as.numeric(stat_agg$x),
                stringsAsFactors = FALSE
              ))
              covariateRef <- rbind(covariateRef, data.frame(
                covariateId = cov_id,
                covariateName = paste0(concept_label, "_", analysis$nm),
                analysisId = analysis$id,
                conceptId = as.integer(cid),
                stringsAsFactors = FALSE
              ))
            }
          }
        }
      }
    }
  }

  list(covariates = covariates, covariateRef = covariateRef)
}

# --- Baseline Extraction ---

#' Extract baseline demographics for cohort members
#'
#' Joins cohort to person and observation_period to produce one row per
#' cohort member with demographics and optional derived fields.
#'
#' @param handle CDM handle
#' @param cohort_table Character; temp table name with cohort members
#' @param columns Character vector; person columns to include
#' @param derived Character vector; derived fields to compute
#' @param translate_concepts Logical; translate concept IDs to names
#' @return Data frame with one row per cohort member
#' @keywords internal
.extractBaseline <- function(handle, cohort_table, columns = NULL,
                              derived = NULL, translate_concepts = FALSE) {
  if (is.null(cohort_table)) {
    warning("Baseline output requires a cohort; returning NULL.", call. = FALSE)
    return(NULL)
  }

  bp <- .buildBlueprint(handle)

  person_schema <- .resolveTableSchema(handle, "person", "Clinical")
  person_table <- .qualifyTable(handle, "person", person_schema)

  op_schema <- .resolveTableSchema(handle, "observation_period", "Clinical")
  op_table <- .qualifyTable(handle, "observation_period", op_schema)

  # Default columns if not specified
  if (is.null(columns)) {
    columns <- c("gender_concept_id", "year_of_birth", "race_concept_id")
  }
  columns <- tolower(columns)

  # Build person column selects (only those available)
  person_cols <- bp$columns[["person"]]
  if (!is.null(person_cols)) {
    avail_person <- person_cols$column_name
    select_person_cols <- intersect(columns, avail_person)
  } else {
    select_person_cols <- columns
  }

  person_select <- if (length(select_person_cols) > 0) {
    paste0(", ", paste(paste0("p.", select_person_cols), collapse = ", "))
  } else {
    ""
  }

  sql <- paste0(
    "SELECT c.subject_id AS person_id, ",
    "c.cohort_start_date, c.cohort_end_date",
    person_select, ", ",
    "op.observation_period_start_date, op.observation_period_end_date",
    " FROM ", cohort_table, " AS c",
    " INNER JOIN ", person_table, " AS p ON p.person_id = c.subject_id",
    " LEFT JOIN ", op_table, " AS op",
    " ON op.person_id = c.subject_id",
    " AND c.cohort_start_date >= op.observation_period_start_date",
    " AND c.cohort_start_date <= op.observation_period_end_date",
    " ORDER BY c.subject_id, c.cohort_start_date"
  )

  .assertMinPersons(conn = handle$conn,
    sql = paste0("SELECT COUNT(DISTINCT subject_id) AS n_persons FROM ",
                 cohort_table))

  result <- .executeQuery(handle, sql)
  if (nrow(result) == 0) return(result)

  # Add row_id
  result$row_id <- seq_len(nrow(result))

  # Compute derived fields
  derived <- tolower(derived %||% character(0))

  if ("age_at_index" %in% derived && "year_of_birth" %in% names(result)) {
    index_year <- as.integer(format(as.Date(result$cohort_start_date), "%Y"))
    result$age_at_index <- index_year - as.integer(result$year_of_birth)
  }

  if ("prior_observation" %in% derived &&
      "observation_period_start_date" %in% names(result)) {
    result$prior_observation <- as.integer(
      as.Date(result$cohort_start_date) -
      as.Date(result$observation_period_start_date)
    )
  }

  if ("future_observation" %in% derived &&
      "observation_period_end_date" %in% names(result)) {
    result$future_observation <- as.integer(
      as.Date(result$observation_period_end_date) -
      as.Date(result$cohort_start_date)
    )
  }

  # Select final columns: row_id, person_id, requested columns, derived
  keep <- c("row_id", "person_id", select_person_cols, derived)
  keep <- intersect(keep, names(result))
  result <- result[, keep, drop = FALSE]

  if (translate_concepts) {
    result <- .vocabTranslateColumns(handle, result)
  }

  result
}

# --- Survival Extraction ---

#' Extract survival (time-to-event) data for cohort members
#'
#' Two-query approach: gets cohort members and outcome events separately,
#' then computes time-to-event on the R side.
#'
#' @param handle CDM handle
#' @param cohort_table Character; temp table name with cohort members
#' @param outcome List with \code{table} and \code{concept_set}
#' @param tar List with \code{start_offset} and \code{end_offset}
#' @param event_order Character; "first" or "last"
#' @return Data frame with row_id, person_id, event (0/1), time_to_event_days
#' @keywords internal
.extractSurvival <- function(handle, cohort_table, outcome, tar = NULL,
                              event_order = "first") {
  if (is.null(cohort_table)) {
    warning("Survival output requires a cohort; returning NULL.", call. = FALSE)
    return(NULL)
  }

  bp <- .buildBlueprint(handle)

  # Query 1: cohort members
  cohort_sql <- paste0(
    "SELECT subject_id AS person_id, cohort_start_date, cohort_end_date",
    " FROM ", cohort_table,
    " ORDER BY subject_id"
  )

  .assertMinPersons(conn = handle$conn,
    sql = paste0("SELECT COUNT(DISTINCT subject_id) AS n_persons FROM ",
                 cohort_table))

  cohort_df <- .executeQuery(handle, cohort_sql)
  if (nrow(cohort_df) == 0) return(cohort_df)

  cohort_df$row_id <- seq_len(nrow(cohort_df))

  # Query 2: outcome events
  outcome_table <- tolower(outcome$table)
  outcome_concepts <- as.integer(outcome$concept_set)

  tbl_row <- bp$tables[bp$tables$table_name == outcome_table, , drop = FALSE]
  if (nrow(tbl_row) == 0 || !tbl_row$present_in_db[1]) {
    stop("Outcome table '", outcome$table, "' not found.", call. = FALSE)
  }

  date_col <- .getDateColumn(bp, outcome_table)
  concept_col <- .getDomainConceptColumn(bp, outcome_table)
  schema <- .resolveTableSchema(handle, outcome_table,
                                 tbl_row$schema_category[1])
  qualified <- .qualifyTable(handle, outcome_table, schema)

  person_ids_str <- paste(as.integer(cohort_df$person_id), collapse = ", ")
  concept_ids_str <- paste(outcome_concepts, collapse = ", ")

  outcome_sql <- paste0(
    "SELECT t.person_id, t.", date_col, " AS outcome_date",
    " FROM ", qualified, " AS t",
    " WHERE t.", concept_col, " IN (", concept_ids_str, ")",
    " AND t.person_id IN (", person_ids_str, ")"
  )
  outcome_df <- .executeQuery(handle, outcome_sql)

  # TAR boundaries
  start_offset <- as.integer(tar$start_offset %||% 0L)
  end_offset <- tar$end_offset

  cohort_df$tar_start <- as.Date(cohort_df$cohort_start_date) + start_offset
  if (!is.null(end_offset)) {
    cohort_df$tar_end <- as.Date(cohort_df$cohort_start_date) +
      as.integer(end_offset)
  } else {
    cohort_df$tar_end <- as.Date(cohort_df$cohort_end_date)
  }

  # Merge and process outcomes
  if (nrow(outcome_df) > 0) {
    outcome_df$outcome_date <- as.Date(outcome_df$outcome_date)
    merged <- merge(cohort_df, outcome_df, by = "person_id", all.x = TRUE)
  } else {
    merged <- cohort_df
    merged$outcome_date <- as.Date(NA)
  }

  # Filter outcomes within TAR window
  merged$in_tar <- !is.na(merged$outcome_date) &
    merged$outcome_date >= merged$tar_start &
    merged$outcome_date <= merged$tar_end

  # For each person (row_id), find first/last outcome in TAR
  result_list <- lapply(split(merged, merged$row_id), function(sub) {
    tar_events <- sub[sub$in_tar, , drop = FALSE]
    if (nrow(tar_events) > 0) {
      if (event_order == "last") {
        chosen <- tar_events[which.max(tar_events$outcome_date), , drop = FALSE]
      } else {
        chosen <- tar_events[which.min(tar_events$outcome_date), , drop = FALSE]
      }
      data.frame(
        row_id = sub$row_id[1],
        person_id = sub$person_id[1],
        event = 1L,
        time_to_event_days = as.integer(chosen$outcome_date[1] -
                                         sub$tar_start[1]),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        row_id = sub$row_id[1],
        person_id = sub$person_id[1],
        event = 0L,
        time_to_event_days = as.integer(sub$tar_end[1] - sub$tar_start[1]),
        stringsAsFactors = FALSE
      )
    }
  })

  result <- do.call(rbind, result_list)
  rownames(result) <- NULL
  result
}

# --- Concept Dictionary ---

#' Build a concept dictionary from extracted results
#'
#' Scans result data.frames for \code{_concept_id} columns, collects all
#' unique IDs, looks them up, and adds a \code{used_in_outputs} column.
#'
#' @param handle CDM handle
#' @param results Named list of extracted results (data.frames or sparse lists)
#' @param source_outputs Character vector; which output names to scan
#' @return Data frame with concept_id, concept_name, domain_id, used_in_outputs
#' @keywords internal
.buildConceptDictionary <- function(handle, results, source_outputs = NULL) {
  if (is.null(source_outputs)) {
    source_outputs <- names(results)
  }
  source_outputs <- intersect(source_outputs, names(results))

  # Collect concept IDs per output
  concept_by_output <- list()
  for (out_name in source_outputs) {
    res <- results[[out_name]]
    if (is.null(res)) next

    ids <- integer(0)
    if (is.data.frame(res)) {
      concept_cols <- grep("_concept_id$", names(res), value = TRUE)
      for (col in concept_cols) {
        vals <- res[[col]]
        vals <- vals[!is.na(vals)]
        ids <- c(ids, as.integer(vals))
      }
    } else if (is.list(res) && "covariateRef" %in% names(res)) {
      if ("conceptId" %in% names(res$covariateRef)) {
        ids <- as.integer(res$covariateRef$conceptId)
      }
    }

    if (length(ids) > 0) {
      concept_by_output[[out_name]] <- unique(ids)
    }
  }

  all_ids <- unique(unlist(concept_by_output, use.names = FALSE))
  if (length(all_ids) == 0) {
    return(data.frame(concept_id = integer(0), concept_name = character(0),
                      domain_id = character(0), used_in_outputs = character(0),
                      stringsAsFactors = FALSE))
  }

  # Look up concepts
  concept_df <- .vocabLookupConcepts(handle, all_ids)

  # Build used_in_outputs mapping
  usage_map <- character(length(all_ids))
  names(usage_map) <- as.character(all_ids)
  for (out_name in names(concept_by_output)) {
    for (cid in concept_by_output[[out_name]]) {
      key <- as.character(cid)
      if (usage_map[key] == "") {
        usage_map[key] <- out_name
      } else {
        usage_map[key] <- paste0(usage_map[key], ", ", out_name)
      }
    }
  }

  if (nrow(concept_df) > 0) {
    concept_df$used_in_outputs <- usage_map[as.character(concept_df$concept_id)]
  }

  # Keep only essential columns
  keep <- intersect(c("concept_id", "concept_name", "domain_id",
                       "vocabulary_id", "used_in_outputs"), names(concept_df))
  concept_df[, keep, drop = FALSE]
}
