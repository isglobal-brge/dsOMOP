# Module: Extraction Engine
# SQL generation, data extraction, and feature engineering for OMOP CDM tables.

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
#' @param add_cohort_date Logical; if TRUE, add cohort start/end date columns
#'   from the cohort table to the output.
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
    # Always block exact birth dates/times (quasi-identifiers)
    always_block <- c("day_of_birth", "birth_datetime")
    blocked <- union(blocked, intersect(always_block, col_df$column_name))
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

  if (!is.null(concept_filter) && length(concept_filter) > 0 && has_concept_col) {
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
    "value_bin" = {
      lo <- as.numeric(value$lower)
      hi <- as.numeric(value$upper)
      paste0(col_ref, " >= ", lo, " AND ", col_ref, " < ", hi)
    },
    stop("Unknown filter op: '", op, "'", call. = FALSE)
  )
}

# --- Query Execution ---

#' Coerce integer64 columns to standard integer or numeric
#'
#' Converts bit64::integer64 columns in a data.frame to regular R integers
#' (if values fit) or doubles. Required because DataSHIELD serialization
#' does not support integer64.
#'
#' @param df A data.frame potentially containing integer64 columns.
#' @return The data.frame with integer64 columns converted.
#' @keywords internal
.coerce_integer64 <- function(df) {
  for (col in names(df)) {
    if (inherits(df[[col]], "integer64")) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  df
}

#' Execute a SQL query and return a data frame
#'
#' @param handle CDM handle
#' @param sql Character; SQL to execute
#' @return Data frame
#' @keywords internal
.executeQuery <- function(handle, sql) {
  result <- DBI::dbGetQuery(handle$conn, sql)
  names(result) <- tolower(names(result))
  .coerce_integer64(result)
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

  # Build spec lookup: concept_id -> list of specs (supports multiple formats per concept)
  specs_by_concept <- list()
  if (!is.null(specs) && length(specs) > 0) {
    for (s in specs) {
      cs <- s$concept_set
      if (is.list(cs) && !is.null(cs$concepts)) cs <- cs$concepts
      cs <- as.integer(cs)
      for (cid in cs) {
        key <- as.character(cid)
        if (is.null(specs_by_concept[[key]])) specs_by_concept[[key]] <- list()
        specs_by_concept[[key]] <- c(specs_by_concept[[key]], list(s))
      }
    }
  }

  persons <- unique(df$person_id)
  features <- data.frame(person_id = persons, stringsAsFactors = FALSE)

  # Helper: generate one feature column
  .add_feature <- function(features, spec, concept_data, concept_str, df) {
    # Determine label
    if (!is.null(spec) && !is.null(spec$name) && nchar(spec$name) > 0) {
      label <- spec$name
    } else {
      label <- .standardizeName(concept_str)
      if (is.na(label) || label == "") label <- paste0("concept_", concept_str)
    }

    spec_type <- if (!is.null(spec)) (spec$type %||% "boolean") else NULL

    # Boolean feature
    if (is.null(spec_type) || spec_type == "boolean") {
      features[[label]] <-
        as.integer(features$person_id %in% concept_data$person_id)
    }

    # Count feature
    if (is.null(spec_type) || spec_type == "count") {
      count_df <- stats::aggregate(
        concept_data[[names(concept_data)[2]]],
        by = list(person_id = concept_data$person_id),
        FUN = length
      )
      col_name <- if (identical(spec_type, "count")) label
                  else paste0(label, "_count")
      names(count_df)[2] <- col_name
      features <- merge(features, count_df, by = "person_id", all.x = TRUE)
      features[[col_name]][is.na(features[[col_name]])] <- 0L
    }

    # Value-based features
    val_col <- if (!is.null(spec)) (spec$value_column %||% "value_as_number")
               else "value_as_number"
    if (val_col %in% names(concept_data)) {
      num_data <- concept_data[!is.na(concept_data[[val_col]]), , drop = FALSE]
      if (nrow(num_data) > 0) {
        if (is.null(spec_type) || spec_type == "mean_value") {
          stat_df <- stats::aggregate(
            num_data[[val_col]],
            by = list(person_id = num_data$person_id), FUN = mean)
          col_name <- if (identical(spec_type, "mean_value")) label
                      else paste0(label, "_mean")
          names(stat_df)[2] <- col_name
          features <- merge(features, stat_df, by = "person_id", all.x = TRUE)
        }
        if (is.null(spec_type) || spec_type == "min_value") {
          stat_df <- stats::aggregate(
            num_data[[val_col]],
            by = list(person_id = num_data$person_id), FUN = min)
          col_name <- if (identical(spec_type, "min_value")) label
                      else paste0(label, "_min")
          names(stat_df)[2] <- col_name
          features <- merge(features, stat_df, by = "person_id", all.x = TRUE)
        }
        if (is.null(spec_type) || spec_type == "max_value") {
          stat_df <- stats::aggregate(
            num_data[[val_col]],
            by = list(person_id = num_data$person_id), FUN = max)
          col_name <- if (identical(spec_type, "max_value")) label
                      else paste0(label, "_max")
          names(stat_df)[2] <- col_name
          features <- merge(features, stat_df, by = "person_id", all.x = TRUE)
        }
        if (identical(spec_type, "first_value")) {
          first_df <- do.call(rbind, lapply(
            split(num_data, num_data$person_id),
            function(x) x[which.min(seq_len(nrow(x))), , drop = FALSE]))
          first_vals <- data.frame(
            person_id = first_df$person_id,
            val = first_df[[val_col]], stringsAsFactors = FALSE)
          names(first_vals)[2] <- label
          features <- merge(features, first_vals, by = "person_id", all.x = TRUE)
        }
        if (identical(spec_type, "latest_value")) {
          last_df <- do.call(rbind, lapply(
            split(num_data, num_data$person_id),
            function(x) x[nrow(x), , drop = FALSE]))
          last_vals <- data.frame(
            person_id = last_df$person_id,
            val = last_df[[val_col]], stringsAsFactors = FALSE)
          names(last_vals)[2] <- label
          features <- merge(features, last_vals, by = "person_id", all.x = TRUE)
        }
        if (identical(spec_type, "sum_value")) {
          stat_df <- stats::aggregate(
            num_data[[val_col]],
            by = list(person_id = num_data$person_id), FUN = sum)
          names(stat_df)[2] <- label
          features <- merge(features, stat_df, by = "person_id", all.x = TRUE)
        }
      }
    }

    # Drug duration feature
    if (identical(spec_type, "drug_duration")) {
      start_col <- intersect(
        c("drug_exposure_start_date", "drug_era_start_date"),
        names(concept_data))
      end_col <- intersect(
        c("drug_exposure_end_date", "drug_era_end_date"),
        names(concept_data))
      if (length(start_col) > 0 && length(end_col) > 0) {
        dur <- as.integer(
          as.Date(concept_data[[end_col[1]]]) -
          as.Date(concept_data[[start_col[1]]]))
        dur_data <- data.frame(
          person_id = concept_data$person_id,
          dur = dur, stringsAsFactors = FALSE)
        dur_data <- dur_data[!is.na(dur_data$dur), , drop = FALSE]
        if (nrow(dur_data) > 0) {
          agg_fn <- switch(spec$agg %||% "mean",
            mean = mean, sum = sum, max = max, mean)
          stat_df <- stats::aggregate(
            dur_data$dur,
            by = list(person_id = dur_data$person_id), FUN = agg_fn)
          names(stat_df)[2] <- label
          features <- merge(features, stat_df, by = "person_id", all.x = TRUE)
        }
      }
    }

    # Standard deviation feature
    if (identical(spec_type, "sd_value") && val_col %in% names(concept_data)) {
      num_data <- concept_data[!is.na(concept_data[[val_col]]), , drop = FALSE]
      if (nrow(num_data) > 0) {
        stat_df <- stats::aggregate(
          num_data[[val_col]],
          by = list(person_id = num_data$person_id),
          FUN = function(x) if (length(x) >= 2) stats::sd(x) else NA_real_)
        names(stat_df)[2] <- label
        features <- merge(features, stat_df, by = "person_id", all.x = TRUE)
      }
    }

    # Coefficient of variation feature
    if (identical(spec_type, "cv_value") && val_col %in% names(concept_data)) {
      num_data <- concept_data[!is.na(concept_data[[val_col]]), , drop = FALSE]
      if (nrow(num_data) > 0) {
        stat_df <- stats::aggregate(
          num_data[[val_col]],
          by = list(person_id = num_data$person_id),
          FUN = function(x) {
            if (length(x) >= 2) {
              m <- mean(x)
              if (m != 0) stats::sd(x) / m * 100 else NA_real_
            } else NA_real_
          })
        names(stat_df)[2] <- label
        features <- merge(features, stat_df, by = "person_id", all.x = TRUE)
      }
    }

    # Slope (linear trend) feature
    if (identical(spec_type, "slope_value") && val_col %in% names(concept_data)) {
      # Detect date column
      date_cols <- intersect(
        c("measurement_date", "condition_start_date", "drug_exposure_start_date",
          "observation_date", "procedure_date", "visit_start_date",
          "condition_era_start_date", "drug_era_start_date"),
        names(concept_data))
      if (length(date_cols) > 0) {
        dcol <- date_cols[1]
        num_data <- concept_data[
          !is.na(concept_data[[val_col]]) & !is.na(concept_data[[dcol]]),
          , drop = FALSE]
        if (nrow(num_data) > 0) {
          num_data$.date_num <- as.numeric(as.Date(num_data[[dcol]]))
          slope_list <- lapply(split(num_data, num_data$person_id), function(x) {
            if (nrow(x) >= 2 && length(unique(x$.date_num)) >= 2) {
              coef(stats::lm(x[[val_col]] ~ x$.date_num))[2]
            } else NA_real_
          })
          stat_df <- data.frame(
            person_id = as.integer(names(slope_list)),
            val = unlist(slope_list, use.names = FALSE),
            stringsAsFactors = FALSE)
          names(stat_df)[2] <- label
          features <- merge(features, stat_df, by = "person_id", all.x = TRUE)
        }
      }
    }

    # Abnormal high count
    if (identical(spec_type, "abnormal_high")) {
      if ("value_as_number" %in% names(concept_data) &&
          "range_high" %in% names(concept_data)) {
        abn <- concept_data[
          !is.na(concept_data$value_as_number) &
          !is.na(concept_data$range_high) &
          concept_data$value_as_number > concept_data$range_high,
          , drop = FALSE]
        if (nrow(abn) > 0) {
          cnt_df <- stats::aggregate(
            abn$value_as_number,
            by = list(person_id = abn$person_id), FUN = length)
          names(cnt_df)[2] <- label
          features <- merge(features, cnt_df, by = "person_id", all.x = TRUE)
        }
        if (!label %in% names(features)) features[[label]] <- NA_integer_
        features[[label]][is.na(features[[label]])] <- 0L
      }
    }

    # Abnormal low count
    if (identical(spec_type, "abnormal_low")) {
      if ("value_as_number" %in% names(concept_data) &&
          "range_low" %in% names(concept_data)) {
        abn <- concept_data[
          !is.na(concept_data$value_as_number) &
          !is.na(concept_data$range_low) &
          concept_data$value_as_number < concept_data$range_low,
          , drop = FALSE]
        if (nrow(abn) > 0) {
          cnt_df <- stats::aggregate(
            abn$value_as_number,
            by = list(person_id = abn$person_id), FUN = length)
          names(cnt_df)[2] <- label
          features <- merge(features, cnt_df, by = "person_id", all.x = TRUE)
        }
        if (!label %in% names(features)) features[[label]] <- NA_integer_
        features[[label]][is.na(features[[label]])] <- 0L
      }
    }

    # Gap max days / gap mean days
    if (identical(spec_type, "gap_max_days") ||
        identical(spec_type, "gap_mean_days")) {
      date_cols <- intersect(
        c("visit_start_date", "measurement_date", "condition_start_date",
          "drug_exposure_start_date", "observation_date", "procedure_date",
          "condition_era_start_date", "drug_era_start_date"),
        names(concept_data))
      if (length(date_cols) > 0) {
        dcol <- date_cols[1]
        date_data <- concept_data[!is.na(concept_data[[dcol]]), , drop = FALSE]
        if (nrow(date_data) > 0) {
          gap_fn <- if (identical(spec_type, "gap_max_days")) max else mean
          gap_list <- lapply(split(date_data, date_data$person_id), function(x) {
            dates <- sort(as.Date(x[[dcol]]))
            if (length(dates) >= 2) {
              gaps <- as.integer(diff(dates))
              gap_fn(gaps)
            } else NA_real_
          })
          stat_df <- data.frame(
            person_id = as.integer(names(gap_list)),
            val = unlist(gap_list, use.names = FALSE),
            stringsAsFactors = FALSE)
          names(stat_df)[2] <- label
          features <- merge(features, stat_df, by = "person_id", all.x = TRUE)
        }
      }
    }

    # Duration sum (sum of end_date - start_date for any table)
    if (identical(spec_type, "duration_sum")) {
      start_col <- intersect(
        c("drug_exposure_start_date", "drug_era_start_date",
          "condition_era_start_date", "condition_start_date",
          "visit_start_date", "observation_period_start_date"),
        names(concept_data))
      end_col <- intersect(
        c("drug_exposure_end_date", "drug_era_end_date",
          "condition_era_end_date", "condition_end_date",
          "visit_end_date", "observation_period_end_date"),
        names(concept_data))
      if (length(start_col) > 0 && length(end_col) > 0) {
        dur <- as.integer(
          as.Date(concept_data[[end_col[1]]]) -
          as.Date(concept_data[[start_col[1]]]))
        dur_data <- data.frame(
          person_id = concept_data$person_id,
          dur = dur, stringsAsFactors = FALSE)
        dur_data <- dur_data[!is.na(dur_data$dur), , drop = FALSE]
        if (nrow(dur_data) > 0) {
          stat_df <- stats::aggregate(
            dur_data$dur,
            by = list(person_id = dur_data$person_id), FUN = sum)
          names(stat_df)[2] <- label
          features <- merge(features, stat_df, by = "person_id", all.x = TRUE)
        }
      }
    }

    features
  }

  # Handle n_distinct specs (cross-concept, computed before per-concept loop)
  if (!is.null(specs) && length(specs) > 0 &&
      !is.null(concept_col) && concept_col %in% names(df)) {
    for (s in specs) {
      if (identical(s$type, "n_distinct")) {
        nd_label <- if (!is.null(s$name) && nchar(s$name) > 0) s$name
                    else "n_distinct"
        nd_df <- stats::aggregate(
          df[[concept_col]],
          by = list(person_id = df$person_id),
          FUN = function(x) length(unique(x))
        )
        names(nd_df)[2] <- nd_label
        features <- merge(features, nd_df, by = "person_id", all.x = TRUE)
        features[[nd_label]][is.na(features[[nd_label]])] <- 0L
      }
    }
  }

  if (!is.null(concept_col) && concept_col %in% names(df)) {
    concepts <- unique(df[[concept_col]])

    for (concept in concepts) {
      concept_str <- as.character(concept)
      concept_specs <- specs_by_concept[[concept_str]]
      concept_data <- df[df[[concept_col]] == concept, , drop = FALSE]

      if (is.null(concept_specs) || length(concept_specs) == 0) {
        # No spec: generate all features (backward compat)
        features <- .add_feature(features, NULL, concept_data, concept_str, df)
      } else {
        # Generate one feature per spec (supports multiple formats per concept)
        for (spec in concept_specs) {
          features <- .add_feature(features, spec, concept_data, concept_str, df)
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

# --- Derived Columns ---

#' Compute person-level derived columns (age, sex, observation duration)
#'
#' Fetches data from the \code{person} table (and \code{observation_period}
#' if needed) and computes derived columns for each person. Supports three
#' kinds of derived columns:
#' \describe{
#'   \item{age}{Numeric age computed as \code{reference_year - year_of_birth}.}
#'   \item{sex_mf}{Character "M" or "F" mapped from \code{gender_concept_id}.}
#'   \item{obs_duration}{Integer days between observation period start and end.}
#' }
#'
#' @param handle CDM handle.
#' @param derived_specs List of derived column specs, each with at least
#'   \code{$kind} and \code{$name}.
#' @param person_ids Integer vector of person IDs to restrict to, or
#'   \code{NULL} for all persons.
#' @param cohort_table Character; name of cohort temp table (used for
#'   index-date age computation), or \code{NULL}.
#' @return A \code{data.frame} with \code{person_id} and one column per
#'   derived spec, or \code{NULL} if no specs or no data.
#' @keywords internal
.computeDerivedColumns <- function(handle, derived_specs,
                                    person_ids = NULL,
                                    cohort_table = NULL) {
  if (is.null(derived_specs) || length(derived_specs) == 0) return(NULL)

  bp <- .buildBlueprint(handle)
  kinds <- vapply(derived_specs, function(s) s$kind, character(1))

  needs_person <- any(kinds %in% c("age", "sex_mf", "demo_missingness"))
  needs_obs <- any(kinds %in% c("obs_duration", "prior_obs", "followup")) ||
    any(kinds == "age" & vapply(derived_specs, function(s) {
      identical(s$reference, "index")
    }, logical(1)))
  needs_comorbidity <- any(kinds %in% c("charlson", "chads2", "chadsvasc",
                                         "dcsi", "hfrs"))

  if (!needs_person && !needs_obs && !needs_comorbidity) return(NULL)

  # Build SQL to fetch required data
  person_schema <- .resolveTableSchema(handle, "person", "Clinical")
  person_table <- .qualifyTable(handle, "person", person_schema)

  select_parts <- "p.person_id"
  if (any(kinds %in% c("age", "demo_missingness"))) {
    select_parts <- paste0(select_parts, ", p.year_of_birth")
  }
  if (any(kinds %in% c("sex_mf", "demo_missingness"))) {
    select_parts <- paste0(select_parts, ", p.gender_concept_id")
  }
  if (any(kinds == "demo_missingness")) {
    select_parts <- paste0(select_parts,
      ", p.month_of_birth, p.day_of_birth",
      ", p.race_concept_id, p.ethnicity_concept_id")
  }

  sql <- paste0("SELECT DISTINCT ", select_parts, " FROM ", person_table, " p")

  if (needs_obs) {
    op_schema <- .resolveTableSchema(handle, "observation_period", "Clinical")
    op_table <- .qualifyTable(handle, "observation_period", op_schema)
    sql <- paste0(sql,
      " LEFT JOIN ", op_table, " op ON op.person_id = p.person_id")
    if (any(kinds %in% c("obs_duration", "prior_obs", "followup"))) {
      # Add observation period columns
      sql <- sub("FROM", paste0(
        ", op.observation_period_start_date, op.observation_period_end_date FROM"
      ), sql)
    }
  }

  # Filter to cohort person IDs
  where <- character(0)
  if (!is.null(person_ids) && length(person_ids) > 0) {
    ids_str <- paste(as.integer(person_ids), collapse = ", ")
    where <- c(where, paste0("p.person_id IN (", ids_str, ")"))
  } else if (!is.null(cohort_table)) {
    where <- c(where, paste0(
      "EXISTS (SELECT 1 FROM ", cohort_table,
      " c WHERE c.subject_id = p.person_id)"))
  }
  if (length(where) > 0) {
    sql <- paste0(sql, " WHERE ", paste(where, collapse = " AND "))
  }

  # Disclosure check
  count_sql <- paste0(
    "SELECT COUNT(DISTINCT person_id) AS n_persons FROM (",
    sql, ") AS sub"
  )
  .assertMinPersons(conn = handle$conn, sql = count_sql)

  df <- .executeQuery(handle, sql)
  if (nrow(df) == 0) return(NULL)

  result <- data.frame(person_id = df$person_id, stringsAsFactors = FALSE)

  for (spec in derived_specs) {
    col_name <- spec$name

    if (spec$kind == "age") {
      if ("year_of_birth" %in% names(df)) {
        ref <- spec$reference %||% "today"
        if (ref == "today" || is.null(cohort_table)) {
          ref_year <- as.integer(format(Sys.Date(), "%Y"))
          if (!is.null(spec$reference_date)) {
            ref_year <- as.integer(format(
              as.Date(spec$reference_date), "%Y"))
          }
        } else {
          # index: use cohort_start_date — fetch from cohort table
          idx_sql <- paste0(
            "SELECT subject_id AS person_id, cohort_start_date FROM ",
            cohort_table)
          idx_df <- .executeQuery(handle, idx_sql)
          idx_df$index_year <- as.integer(
            format(as.Date(idx_df$cohort_start_date), "%Y"))
          df <- merge(df, idx_df[, c("person_id", "index_year")],
                      by = "person_id", all.x = TRUE)
          ref_year <- df$index_year
        }
        result[[col_name]] <- as.integer(ref_year - df$year_of_birth)
      } else {
        result[[col_name]] <- NA_integer_
      }

    } else if (spec$kind == "sex_mf") {
      if ("gender_concept_id" %in% names(df)) {
        result[[col_name]] <- ifelse(
          df$gender_concept_id == 8507L, "M",
          ifelse(df$gender_concept_id == 8532L, "F", NA_character_)
        )
      } else {
        result[[col_name]] <- NA_character_
      }

    } else if (spec$kind == "obs_duration") {
      if ("observation_period_start_date" %in% names(df) &&
          "observation_period_end_date" %in% names(df)) {
        start_d <- as.Date(df$observation_period_start_date)
        end_d <- as.Date(df$observation_period_end_date)
        result[[col_name]] <- as.integer(end_d - start_d)
      } else {
        result[[col_name]] <- NA_integer_
      }

    } else if (spec$kind == "prior_obs") {
      if ("observation_period_start_date" %in% names(df)) {
        ref_date <- if (!is.null(spec$reference_date)) {
          as.Date(spec$reference_date)
        } else {
          Sys.Date()
        }
        start_d <- as.Date(df$observation_period_start_date)
        result[[col_name]] <- as.integer(ref_date - start_d)
      } else {
        result[[col_name]] <- NA_integer_
      }

    } else if (spec$kind == "followup") {
      if ("observation_period_end_date" %in% names(df)) {
        ref_date <- if (!is.null(spec$reference_date)) {
          as.Date(spec$reference_date)
        } else {
          Sys.Date()
        }
        end_d <- as.Date(df$observation_period_end_date)
        result[[col_name]] <- as.integer(end_d - ref_date)
      } else {
        result[[col_name]] <- NA_integer_
      }

    } else if (spec$kind == "demo_missingness") {
      demo_cols <- c("year_of_birth", "month_of_birth", "day_of_birth",
                     "race_concept_id", "ethnicity_concept_id",
                     "gender_concept_id")
      present <- intersect(demo_cols, names(df))
      if (length(present) > 0) {
        miss_count <- rowSums(is.na(df[, present, drop = FALSE]))
        # Also count 0-valued concept IDs as "missing"
        concept_demo <- intersect(
          c("race_concept_id", "ethnicity_concept_id", "gender_concept_id"),
          present)
        for (cc in concept_demo) {
          miss_count <- miss_count +
            as.integer(!is.na(df[[cc]]) & df[[cc]] == 0L)
        }
        result[[col_name]] <- as.integer(miss_count)
      } else {
        result[[col_name]] <- NA_integer_
      }

    } else if (spec$kind %in% c("charlson", "chads2", "chadsvasc",
                                  "dcsi", "hfrs")) {
      score_df <- .computeComorbidityScore(
        handle, spec$kind, result$person_id)
      zero_val <- if (spec$kind %in% c("hfrs", "dcsi")) 0 else 0L
      if (!is.null(score_df) && nrow(score_df) > 0) {
        names(score_df)[2] <- col_name
        result <- merge(result, score_df, by = "person_id", all.x = TRUE)
        result[[col_name]][is.na(result[[col_name]])] <- zero_val
      } else {
        result[[col_name]] <- zero_val
      }
    }
  }

  result
}

#' Get ICD code patterns and scoring definitions for vocabulary-resolved scores
#'
#' Returns category definitions with ICD code patterns, tiers/weights for
#' DCSI (ICD9CM) and HFRS (ICD10CM). Pure function, no DB access.
#'
#' @param score_type Character; "dcsi" or "hfrs"
#' @return List with source_vocabulary and categories
#' @keywords internal
.getScoreDefinitions <- function(score_type) {
  if (score_type == "dcsi") {
    list(
      source_vocabulary = "ICD9CM",
      scoring_mode = "tiered",
      categories = list(
        list(category = "retinopathy", tier = 1L,
             patterns = c("250.5%", "362.01", "362.1%", "362.83",
                          "362.53", "362.81", "362.82")),
        list(category = "retinopathy", tier = 2L,
             patterns = c("361%", "369%", "362.02", "379.23")),
        list(category = "nephropathy", tier = 1L,
             patterns = c("250.4%", "580%", "581%", "582%", "583%")),
        list(category = "nephropathy", tier = 2L,
             patterns = c("585%", "586%", "593.9%")),
        list(category = "neuropathy", tier = 1L,
             patterns = c("356.9%", "250.6%", "358.1%", "951.0%",
                          "951.1%", "951.3%", "713.5%", "357.2%",
                          "596.54", "337.0%", "337.1%", "564.5%",
                          "536.3%", "458.0%", "354%", "355%")),
        list(category = "cerebrovascular", tier = 1L,
             patterns = c("435%")),
        list(category = "cerebrovascular", tier = 2L,
             patterns = c("431%", "433%", "434%", "436%")),
        list(category = "cardiovascular", tier = 1L,
             patterns = c("440%", "411%", "413%", "414%", "429.2%")),
        list(category = "cardiovascular", tier = 2L,
             patterns = c("410%", "427.1%", "427.3%", "427.4%",
                          "427.5%", "412%", "428%", "441%",
                          "440.23", "440.24")),
        list(category = "pvd", tier = 1L,
             patterns = c("250.7%", "442.3%", "892.1%", "443.9%",
                          "443.81")),
        list(category = "pvd", tier = 2L,
             patterns = c("785.4%", "707.1%", "040.0%", "444.22")),
        list(category = "metabolic", tier = 2L,
             patterns = c("250.1%", "250.2%", "250.3%"))
      )
    )
  } else if (score_type == "hfrs") {
    list(
      source_vocabulary = c("ICD10CM", "ICD10"),
      scoring_mode = "weighted_binary",
      categories = list(
        list(category = "dementia_alzheimers", patterns = c("F00%"), weight = 7.1),
        list(category = "hemiplegia", patterns = c("G81%"), weight = 4.4),
        list(category = "alzheimers_disease", patterns = c("G30%"), weight = 4.0),
        list(category = "sequelae_cvd", patterns = c("I69%"), weight = 3.7),
        list(category = "nervous_musculoskeletal", patterns = c("R29%"), weight = 3.6),
        list(category = "urinary_disorders", patterns = c("N39%"), weight = 3.2),
        list(category = "delirium", patterns = c("F05%"), weight = 3.2),
        list(category = "unspecified_fall", patterns = c("W19%"), weight = 3.2),
        list(category = "superficial_head", patterns = c("S00%"), weight = 3.2),
        list(category = "haematuria", patterns = c("R31%"), weight = 3.0),
        list(category = "bacterial_agents", patterns = c("B96%"), weight = 2.9),
        list(category = "cognitive_functions", patterns = c("R41%"), weight = 2.7),
        list(category = "gait_mobility", patterns = c("R26%"), weight = 2.6),
        list(category = "other_cerebrovascular", patterns = c("I67%"), weight = 2.6),
        list(category = "convulsions", patterns = c("R56%"), weight = 2.6),
        list(category = "somnolence_stupor_coma", patterns = c("R40%"), weight = 2.5),
        list(category = "intracranial_injury", patterns = c("S06%"), weight = 2.5),
        list(category = "gu_prosthesis_complication", patterns = c("T83%"), weight = 2.4),
        list(category = "fracture_shoulder", patterns = c("S42%"), weight = 2.3),
        list(category = "fluid_electrolyte", patterns = c("E87%"), weight = 2.3),
        list(category = "other_joint_disorders", patterns = c("M25%"), weight = 2.3),
        list(category = "volume_depletion", patterns = c("E86%"), weight = 2.3),
        list(category = "senility", patterns = c("R54%"), weight = 2.2),
        list(category = "rehabilitation", patterns = c("Z50%"), weight = 2.1),
        list(category = "unspecified_dementia", patterns = c("F03%"), weight = 2.1),
        list(category = "other_fall_same_level", patterns = c("W18%"), weight = 2.1),
        list(category = "medical_facility_problems", patterns = c("Z75%"), weight = 2.0),
        list(category = "vascular_dementia", patterns = c("F01%"), weight = 2.0),
        list(category = "superficial_lower_leg", patterns = c("S80%"), weight = 2.0),
        list(category = "cellulitis", patterns = c("L03%"), weight = 2.0),
        list(category = "blindness_low_vision", patterns = c("H54%"), weight = 1.9),
        list(category = "b_vitamin_deficiency", patterns = c("E53%"), weight = 1.9),
        list(category = "social_environment", patterns = c("Z60%"), weight = 1.8),
        list(category = "parkinsons", patterns = c("G20%"), weight = 1.8),
        list(category = "syncope", patterns = c("R55%"), weight = 1.8),
        list(category = "fracture_rib_sternum", patterns = c("S22%"), weight = 1.8),
        list(category = "functional_intestinal", patterns = c("K59%"), weight = 1.8),
        list(category = "acute_renal_failure", patterns = c("N17%"), weight = 1.8),
        list(category = "pressure_ulcer", patterns = c("L89%"), weight = 1.7),
        list(category = "carrier_infectious", patterns = c("Z22%"), weight = 1.7),
        list(category = "strep_staph", patterns = c("B95%"), weight = 1.7),
        list(category = "ulcer_lower_limb", patterns = c("L97%"), weight = 1.6),
        list(category = "perception_symptoms", patterns = c("R44%"), weight = 1.6),
        list(category = "duodenal_ulcer", patterns = c("K26%"), weight = 1.6),
        list(category = "hypotension", patterns = c("I95%"), weight = 1.6),
        list(category = "unspecified_renal_failure", patterns = c("N19%"), weight = 1.6),
        list(category = "other_septicaemia", patterns = c("A41%"), weight = 1.6),
        list(category = "personal_history_diseases", patterns = c("Z87%"), weight = 1.5),
        list(category = "respiratory_failure", patterns = c("J96%"), weight = 1.5),
        list(category = "exposure_unspecified", patterns = c("X59%"), weight = 1.5),
        list(category = "other_arthrosis", patterns = c("M19%"), weight = 1.5),
        list(category = "epilepsy", patterns = c("G40%"), weight = 1.5),
        list(category = "osteoporosis_no_fracture", patterns = c("M81%"), weight = 1.4),
        list(category = "fracture_femur", patterns = c("S72%"), weight = 1.4),
        list(category = "fracture_lumbar_pelvis", patterns = c("S32%"), weight = 1.4),
        list(category = "pancreatic_secretion", patterns = c("E16%"), weight = 1.4),
        list(category = "abnormal_function_study", patterns = c("R94%"), weight = 1.4),
        list(category = "chronic_renal_failure", patterns = c("N18%"), weight = 1.4),
        list(category = "urinary_retention", patterns = c("R33%"), weight = 1.3),
        list(category = "unknown_morbidity", patterns = c("R69%"), weight = 1.3),
        list(category = "other_kidney_ureter", patterns = c("N28%"), weight = 1.3),
        list(category = "urinary_incontinence", patterns = c("R32%"), weight = 1.2),
        list(category = "other_degenerative_nervous", patterns = c("G31%"), weight = 1.2),
        list(category = "nosocomial_condition", patterns = c("Y95%"), weight = 1.2),
        list(category = "other_head_injuries", patterns = c("S09%"), weight = 1.2),
        list(category = "emotional_state", patterns = c("R45%"), weight = 1.2),
        list(category = "transient_cerebral_ischaemia", patterns = c("G45%"), weight = 1.2),
        list(category = "care_provider_dependency", patterns = c("Z74%"), weight = 1.1),
        list(category = "other_soft_tissue", patterns = c("M79%"), weight = 1.1),
        list(category = "fall_involving_bed", patterns = c("W06%"), weight = 1.1),
        list(category = "open_wound_head", patterns = c("S01%"), weight = 1.1),
        list(category = "bacterial_intestinal", patterns = c("A04%"), weight = 1.1),
        list(category = "infectious_diarrhoea", patterns = c("A09%"), weight = 1.1),
        list(category = "pneumonia_unspecified", patterns = c("J18%"), weight = 1.1),
        list(category = "pneumonitis_solids_liquids", patterns = c("J69%"), weight = 1.0),
        list(category = "speech_disturbances", patterns = c("R47%"), weight = 1.0),
        list(category = "vitamin_d_deficiency", patterns = c("E55%"), weight = 1.0),
        list(category = "artificial_opening", patterns = c("Z93%"), weight = 1.0),
        list(category = "gangrene", patterns = c("R02%"), weight = 1.0),
        list(category = "food_fluid_intake", patterns = c("R63%"), weight = 0.9),
        list(category = "other_hearing_loss", patterns = c("H91%"), weight = 0.9),
        list(category = "fall_on_stairs", patterns = c("W10%"), weight = 0.9),
        list(category = "fall_slipping_tripping", patterns = c("W01%"), weight = 0.9),
        list(category = "thyrotoxicosis", patterns = c("E05%"), weight = 0.9),
        list(category = "scoliosis", patterns = c("M41%"), weight = 0.9),
        list(category = "dysphagia", patterns = c("R13%"), weight = 0.8),
        list(category = "dependence_machines", patterns = c("Z99%"), weight = 0.8),
        list(category = "penicillin_resistant", patterns = c("U80%"), weight = 0.8),
        list(category = "osteoporosis_fracture", patterns = c("M80%"), weight = 0.8),
        list(category = "other_digestive", patterns = c("K92%"), weight = 0.8),
        list(category = "cerebral_infarction", patterns = c("I63%"), weight = 0.8),
        list(category = "kidney_ureter_calculus", patterns = c("N20%"), weight = 0.7),
        list(category = "alcohol_mental", patterns = c("F10%"), weight = 0.7),
        list(category = "medical_procedure_reaction", patterns = c("Y84%"), weight = 0.7),
        list(category = "heartbeat_abnormalities", patterns = c("R00%"), weight = 0.7),
        list(category = "acute_lower_respiratory", patterns = c("J22%"), weight = 0.7),
        list(category = "life_management_difficulty", patterns = c("Z73%"), weight = 0.6),
        list(category = "abnormal_blood_chemistry", patterns = c("R79%"), weight = 0.6),
        list(category = "personal_history_risk", patterns = c("Z91%"), weight = 0.5),
        list(category = "open_wound_forearm", patterns = c("S51%"), weight = 0.5),
        list(category = "depressive_episode", patterns = c("F32%"), weight = 0.5),
        list(category = "spinal_stenosis", patterns = c("M48%"), weight = 0.5),
        list(category = "mineral_metabolism", patterns = c("E83%"), weight = 0.4),
        list(category = "polyarthrosis", patterns = c("M15%"), weight = 0.4),
        list(category = "other_anaemias", patterns = c("D64%"), weight = 0.4),
        list(category = "other_skin_infections", patterns = c("L08%"), weight = 0.4),
        list(category = "nausea_vomiting", patterns = c("R11%"), weight = 0.3),
        list(category = "noninfective_gastroenteritis", patterns = c("K52%"), weight = 0.3),
        list(category = "fever_unknown", patterns = c("R50%"), weight = 0.1)
      )
    )
  } else {
    stop("Unknown score type for definitions: '", score_type,
         "'. Supported: dcsi, hfrs.", call. = FALSE)
  }
}

#' Resolve ICD code patterns to SNOMED concept IDs via concept_relationship
#'
#' Queries the vocabulary's concept_relationship table to map ICD source codes
#' to standard SNOMED targets, then assigns each resolved concept to its
#' scoring category. Results are cached on the handle.
#'
#' @param handle CDM handle
#' @param score_type Character; "dcsi" or "hfrs"
#' @return Named list: category -> list(concepts, tier/weight)
#' @keywords internal
.resolveScoreConcepts <- function(handle, score_type) {
  # Check cache

  cache_key <- paste0("resolved_", score_type)
  if (!is.null(handle[[cache_key]])) return(handle[[cache_key]])

  defs <- .getScoreDefinitions(score_type)
  bp <- .buildBlueprint(handle)

  # Check if concept_relationship table exists
  cr_row <- bp$tables[bp$tables$table_name == "concept_relationship" &
                        bp$tables$present_in_db, , drop = FALSE]
  if (nrow(cr_row) == 0) {
    warning(score_type, " score requires concept_relationship table ",
            "but it is not present in the database. ",
            "All scores will be 0.", call. = FALSE)
    handle[[cache_key]] <- list()
    return(list())
  }

  # Resolve table names
  cr_table <- cr_row$qualified_name[1]
  concept_row <- bp$tables[bp$tables$table_name == "concept" &
                             bp$tables$present_in_db, , drop = FALSE]
  if (nrow(concept_row) == 0) {
    handle[[cache_key]] <- list()
    return(list())
  }
  concept_table <- concept_row$qualified_name[1]

  # Build LIKE clauses for all patterns across all categories
  all_patterns <- character(0)
  for (cat in defs$categories) {
    all_patterns <- c(all_patterns, cat$patterns)
  }
  all_patterns <- unique(all_patterns)

  if (length(all_patterns) == 0) {
    handle[[cache_key]] <- list()
    return(list())
  }

  # Build SQL LIKE conditions — convert % wildcard to SQL LIKE
  like_clauses <- vapply(all_patterns, function(p) {
    paste0("source.concept_code LIKE '", p, "'")
  }, character(1), USE.NAMES = FALSE)
  like_sql <- paste(like_clauses, collapse = " OR ")

  # Build vocabulary filter — HFRS accepts both ICD10CM and ICD10
  src_vocabs <- defs$source_vocabulary
  if (length(src_vocabs) == 1) {
    vocab_filter <- paste0("source.vocabulary_id = '", src_vocabs, "'")
  } else {
    vocab_filter <- paste0("source.vocabulary_id IN (",
      paste0("'", src_vocabs, "'", collapse = ", "), ")")
  }

  sql <- paste0(
    "SELECT source.concept_code AS source_code, ",
    "target.concept_id AS target_concept_id ",
    "FROM ", cr_table, " cr ",
    "JOIN ", concept_table, " source ON cr.concept_id_1 = source.concept_id ",
    "JOIN ", concept_table, " target ON cr.concept_id_2 = target.concept_id ",
    "WHERE ", vocab_filter, " ",
    "AND target.vocabulary_id = 'SNOMED' ",
    "AND target.standard_concept = 'S' ",
    "AND cr.relationship_id = 'Maps to' ",
    "AND cr.invalid_reason IS NULL ",
    "AND source.invalid_reason IS NULL ",
    "AND target.invalid_reason IS NULL ",
    "AND (", like_sql, ")"
  )

  resolved_df <- .executeQuery(handle, sql)

  if (nrow(resolved_df) == 0) {
    warning(score_type, " score: no ICD-to-SNOMED mappings found in ",
            "concept_relationship (vocabulary may not be loaded). ",
            "All scores will be 0.", call. = FALSE)
    handle[[cache_key]] <- list()
    return(list())
  }

  # Assign resolved concepts to categories by matching patterns
  result <- list()
  for (cat in defs$categories) {
    cat_name <- cat$category
    matched_ids <- integer(0)
    for (p in cat$patterns) {
      # Convert SQL LIKE pattern to regex
      regex <- paste0("^", gsub("%", ".*", gsub("\\.", "\\\\.", p)), "$")
      matching_rows <- grepl(regex, resolved_df$source_code)
      matched_ids <- c(matched_ids,
                       resolved_df$target_concept_id[matching_rows])
    }
    matched_ids <- unique(as.integer(matched_ids))
    if (length(matched_ids) > 0) {
      entry <- list(category = cat_name, concepts = matched_ids)
      if (!is.null(cat$tier)) entry$tier <- cat$tier
      if (!is.null(cat$weight)) entry$weight <- cat$weight
      result <- c(result, list(entry))
    }
  }

  handle[[cache_key]] <- result
  result
}

#' Compute a comorbidity score
#'
#' Checks condition tables for the presence of category-defining concepts
#' and computes a weighted sum per person. Supports simple weighted (Charlson,
#' CHADS2, CHA2DS2-VASc), tiered (DCSI), and weighted binary (HFRS) modes.
#'
#' @param handle CDM handle
#' @param score_type Character; "charlson", "chads2", "chadsvasc", "dcsi", "hfrs"
#' @param person_ids Integer vector of person IDs
#' @return Data frame with person_id and score columns
#' @keywords internal
.computeComorbidityScore <- function(handle, score_type, person_ids = NULL) {
  if (is.null(person_ids) || length(person_ids) == 0) return(NULL)

  # --- Score definitions ---
  # All concept IDs are ancestor concept IDs from OHDSI FeatureExtraction.
  # Matching is exact (no concept_ancestor descendant lookup).
  # Version-pinned to FeatureExtraction v3.5.x / OMOP CDM v5.4.
  #
  # IMPORTANT: These scores use ancestor concept IDs directly. In the official

  # FeatureExtraction, descendant concepts are resolved via concept_ancestor.
  # Our implementation matches on the ancestor IDs themselves, which covers
  # the most common codings but may miss descendant-only records.

  # Charlson Comorbidity Index (analysis_id = 901)
  # Source: FeatureExtraction/inst/sql/sql_server/CharlsonIndex.sql
  if (score_type == "charlson") {
    categories <- list(
      mi         = list(concepts = c(4329847L),                    weight = 1L),
      chf        = list(concepts = c(316139L),                     weight = 1L),
      pvd        = list(concepts = c(321052L),                     weight = 1L),
      cvd        = list(concepts = c(381591L, 434056L),            weight = 1L),
      dementia   = list(concepts = c(4182210L),                    weight = 1L),
      copd       = list(concepts = c(4063381L),                    weight = 1L),
      rheumatic  = list(concepts = c(257628L, 134442L, 80800L,
                                      80809L, 256197L, 255348L),   weight = 1L),
      pud        = list(concepts = c(4247120L),                    weight = 1L),
      mild_liver = list(concepts = c(4212540L, 4064161L),          weight = 1L),
      dm_uncomp  = list(concepts = c(201820L),                     weight = 1L),
      dm_comp    = list(concepts = c(443767L, 442793L),            weight = 2L),
      hemiplegia = list(concepts = c(192606L, 374022L),            weight = 2L),
      renal      = list(concepts = c(4030518L),                    weight = 2L),
      malignancy = list(concepts = c(443392L),                     weight = 2L),
      mod_liver  = list(concepts = c(4245975L, 4029488L,
                                      192680L, 24966L),            weight = 3L),
      metastatic = list(concepts = c(432851L),                     weight = 6L),
      aids       = list(concepts = c(439727L),                     weight = 6L)
    )

  # CHADS2 (analysis_id = 903)
  # Source: FeatureExtraction/inst/sql/sql_server/Chads2.sql
  } else if (score_type == "chads2") {
    categories <- list(
      chf          = list(concepts = c(316139L),            weight = 1L),
      hypertension = list(concepts = c(316866L),            weight = 1L),
      diabetes     = list(concepts = c(201820L),            weight = 1L),
      stroke       = list(concepts = c(381591L, 434056L),   weight = 2L)
    )

  # CHA2DS2-VASc (analysis_id = 904)
  # Source: FeatureExtraction/inst/sql/sql_server/Chads2Vasc.sql
  # Uses expanded concept sets from the official SQL definition.
  } else if (score_type == "chadsvasc") {
    categories <- list(
      chf          = list(concepts = c(316139L, 314378L, 318773L,
                                        321319L),                   weight = 1L),
      hypertension = list(concepts = c(320128L, 442604L, 201313L), weight = 1L),
      diabetes     = list(concepts = c(201820L, 442793L),          weight = 1L),
      stroke       = list(concepts = c(4043731L, 4110192L, 375557L,
                                        4108356L, 373503L, 434656L,
                                        433505L, 376714L, 312337L), weight = 2L),
      vascular     = list(concepts = c(312327L, 43020432L, 314962L,
                                        312939L, 315288L, 317309L,
                                        134380L, 196438L, 200138L,
                                        194393L, 319047L, 40486130L,
                                        317003L, 4313767L, 321596L,
                                        317305L, 321886L, 314659L,
                                        321887L, 437312L, 134057L), weight = 1L)
    )

  # DCSI (analysis_id = 902) — vocabulary-resolved via concept_relationship
  } else if (score_type == "dcsi") {
    resolved <- .resolveScoreConcepts(handle, "dcsi")
    scoring_mode <- "tiered"

  # HFRS (analysis_id = 926) — vocabulary-resolved via concept_relationship
  } else if (score_type == "hfrs") {
    resolved <- .resolveScoreConcepts(handle, "hfrs")
    scoring_mode <- "weighted_binary"

  } else {
    stop("Unknown score type: '", score_type, "'. Supported: charlson, ",
         "chads2, chadsvasc, dcsi, hfrs.", call. = FALSE)
  }

  # For vocabulary-resolved scores, check if resolution returned anything
  if (score_type %in% c("dcsi", "hfrs")) {
    if (length(resolved) == 0) {
      # Graceful fallback: no vocab mappings -> score 0 for everyone
      score_result <- data.frame(
        person_id = person_ids, score = 0, stringsAsFactors = FALSE)
      analysis_ids <- list(dcsi = 902L, hfrs = 926L)
      attr(score_result, "score_meta") <- list(
        score_type = score_type,
        analysis_id = analysis_ids[[score_type]],
        upstream = "FeatureExtraction",
        matching = "vocabulary_resolved",
        note = "No concept_relationship mappings found; all scores 0"
      )
      return(score_result)
    }
  } else {
    scoring_mode <- "simple_weighted"
  }

  bp <- .buildBlueprint(handle)

  # Find condition tables: condition_occurrence, condition_era
  cond_tables <- c("condition_occurrence", "condition_era")
  available_tables <- character(0)
  for (ct in cond_tables) {
    tbl_row <- bp$tables[bp$tables$table_name == ct &
                           bp$tables$present_in_db, , drop = FALSE]
    if (nrow(tbl_row) > 0) {
      available_tables <- c(available_tables, ct)
    }
  }
  if (length(available_tables) == 0) {
    return(data.frame(person_id = person_ids, score = 0,
                      stringsAsFactors = FALSE))
  }

  ids_str <- paste(as.integer(person_ids), collapse = ", ")

  # Build UNION ALL across available condition tables
  union_parts <- character(0)
  for (ct in available_tables) {
    tbl_row <- bp$tables[bp$tables$table_name == ct &
                           bp$tables$present_in_db, , drop = FALSE]
    concept_col <- .getDomainConceptColumn(bp, ct)
    # Fallback for v5.3 where concept_prefix has trailing underscore
    if (is.null(concept_col)) {
      fallback <- paste0(sub("_occurrence$|_era$|_exposure$", "", ct),
                          "_concept_id")
      cols <- bp$columns[[ct]]
      if (!is.null(cols) && fallback %in% cols$column_name) {
        concept_col <- fallback
      } else {
        next
      }
    }
    qualified <- tbl_row$qualified_name[1]
    union_parts <- c(union_parts, paste0(
      "SELECT DISTINCT person_id, ", concept_col, " AS concept_id FROM ",
      qualified, " WHERE person_id IN (", ids_str, ")"))
  }
  union_sql <- paste(union_parts, collapse = " UNION ALL ")

  cond_df <- .executeQuery(handle, union_sql)

  # Compute score per person
  use_numeric <- scoring_mode %in% c("tiered", "weighted_binary")
  score_result <- data.frame(
    person_id = person_ids,
    score = if (use_numeric) 0 else 0L,
    stringsAsFactors = FALSE)

  if (nrow(cond_df) > 0) {
    if (scoring_mode == "simple_weighted") {
      # Charlson/CHADS2/CHA2DS2-VASc: binary presence x integer weight
      for (cat in categories) {
        matched_pids <- unique(
          cond_df$person_id[cond_df$concept_id %in% cat$concepts])
        idx <- score_result$person_id %in% matched_pids
        score_result$score[idx] <- score_result$score[idx] + cat$weight
      }

    } else if (scoring_mode == "tiered") {
      # DCSI: MAX(tier) within each category, then SUM across categories
      # Group resolved entries by category
      cat_names <- unique(vapply(resolved, `[[`, character(1), "category"))
      for (cn in cat_names) {
        entries <- Filter(function(e) e$category == cn, resolved)
        # For each person, find the max tier they match
        person_max_tier <- setNames(rep(0, length(person_ids)), person_ids)
        for (entry in entries) {
          matched_pids <- unique(
            cond_df$person_id[cond_df$concept_id %in% entry$concepts])
          for (pid in as.character(matched_pids)) {
            if (pid %in% names(person_max_tier)) {
              person_max_tier[pid] <- max(person_max_tier[pid], entry$tier)
            }
          }
        }
        # Add max tier to score
        for (i in seq_along(person_ids)) {
          pid_chr <- as.character(person_ids[i])
          score_result$score[i] <- score_result$score[i] +
            person_max_tier[pid_chr]
        }
      }

    } else if (scoring_mode == "weighted_binary") {
      # HFRS: binary presence x decimal weight per category
      for (entry in resolved) {
        matched_pids <- unique(
          cond_df$person_id[cond_df$concept_id %in% entry$concepts])
        idx <- score_result$person_id %in% matched_pids
        score_result$score[idx] <- score_result$score[idx] + entry$weight
      }
    }
  }

  # CHADS2: add age-based points (age >= 75: +1)
  # CHA2DS2-VASc: add age-based and sex-based points
  if (score_type %in% c("chads2", "chadsvasc")) {
    person_schema <- .resolveTableSchema(handle, "person", "Clinical")
    person_table <- .qualifyTable(handle, "person", person_schema)
    person_sql <- paste0(
      "SELECT person_id, year_of_birth, gender_concept_id FROM ",
      person_table, " WHERE person_id IN (", ids_str, ")")
    person_df <- .executeQuery(handle, person_sql)
    if (nrow(person_df) > 0) {
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      person_df$age <- current_year - person_df$year_of_birth
      for (i in seq_len(nrow(person_df))) {
        pid <- person_df$person_id[i]
        idx <- which(score_result$person_id == pid)
        if (length(idx) > 0 && !is.na(person_df$age[i])) {
          if (score_type == "chads2") {
            # CHADS2: age >= 75 = +1
            if (person_df$age[i] >= 75) {
              score_result$score[idx] <- score_result$score[idx] + 1L
            }
          } else {
            # CHA2DS2-VASc: age >= 75 = +2, age 65-74 = +1
            if (person_df$age[i] >= 75) {
              score_result$score[idx] <- score_result$score[idx] + 2L
            } else if (person_df$age[i] >= 65) {
              score_result$score[idx] <- score_result$score[idx] + 1L
            }
          }
        }
        # CHA2DS2-VASc only: female sex = +1
        if (score_type == "chadsvasc" && length(idx) > 0 &&
            !is.na(person_df$gender_concept_id[i]) &&
            person_df$gender_concept_id[i] == 8532L) {
          score_result$score[idx] <- score_result$score[idx] + 1L
        }
      }
    }
  }

  # Attach score metadata for provenance tracking
  analysis_ids <- list(charlson = 901L, chads2 = 903L, chadsvasc = 904L,
                       dcsi = 902L, hfrs = 926L)
  matching_type <- if (score_type %in% c("dcsi", "hfrs")) {
    "vocabulary_resolved"
  } else {
    "exact_ancestor"
  }
  attr(score_result, "score_meta") <- list(
    score_type = score_type,
    analysis_id = analysis_ids[[score_type]],
    upstream = "FeatureExtraction",
    matching = matching_type,
    note = if (matching_type == "vocabulary_resolved") {
      "ICD codes resolved to SNOMED via concept_relationship"
    } else {
      "Ancestor concept IDs matched directly (no concept_ancestor descendant resolution)"
    }
  )

  score_result
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
  # Note: year_of_birth is included for age_group computation but removed
  # from final output when age_at_index is a derived field
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
    result$age_group <- .computeAgeGroups(result$year_of_birth, index_year)
    # Remove exact year_of_birth from output (quasi-identifier)
    result$year_of_birth <- NULL
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
  # Replace "age_at_index" with "age_group" in the keep list
  derived_keep <- derived
  if ("age_at_index" %in% derived_keep) {
    derived_keep <- setdiff(derived_keep, "age_at_index")
    derived_keep <- c(derived_keep, "age_group")
  }
  keep <- c("row_id", "person_id", select_person_cols, derived_keep)
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

  concept_ids_str <- paste(outcome_concepts, collapse = ", ")

  outcome_sql <- paste0(
    "SELECT t.person_id, t.", date_col, " AS outcome_date",
    " FROM ", qualified, " AS t",
    " INNER JOIN ", cohort_table, " AS c ON c.subject_id = t.person_id",
    " WHERE t.", concept_col, " IN (", concept_ids_str, ")"
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

# --- Cohort Membership Extraction ---

#' Extract standard OHDSI cohort membership table
#'
#' Simple query on the materialized cohort temp table. Produces output
#' with the standard OHDSI cohort columns.
#'
#' @param handle CDM handle
#' @param cohort_table Character; temp table name with cohort members
#' @param cohort_definition_id Integer; cohort definition ID
#' @return Data frame with row_id, subject_id, cohort_definition_id,
#'   cohort_start_date, cohort_end_date
#' @keywords internal
.extractCohortMembership <- function(handle, cohort_table,
                                      cohort_definition_id) {
  if (is.null(cohort_table)) {
    warning("Cohort membership output requires a cohort; returning NULL.",
            call. = FALSE)
    return(NULL)
  }

  .assertMinPersons(
    conn = handle$conn,
    sql = paste0("SELECT COUNT(DISTINCT subject_id) AS n_persons FROM ",
                 cohort_table)
  )

  sql <- paste0(
    "SELECT subject_id, cohort_start_date, cohort_end_date",
    " FROM ", cohort_table,
    " ORDER BY subject_id, cohort_start_date"
  )

  result <- .executeQuery(handle, sql)
  if (nrow(result) == 0) return(result)

  result$row_id <- seq_len(nrow(result))
  result$cohort_definition_id <- as.integer(cohort_definition_id)

  # Reorder columns: row_id, subject_id, cohort_definition_id, dates
  result[, c("row_id", "subject_id", "cohort_definition_id",
             "cohort_start_date", "cohort_end_date")]
}

# --- Intervals Long Extraction ---

#' Extract interval data from multiple tables relative to index date
#'
#' For each table, joins to the cohort, computes start/end days relative
#' to cohort_start_date, and combines into a single long data frame.
#'
#' @param handle CDM handle
#' @param cohort_table Character; temp table name with cohort members
#' @param tables Character vector; table names to extract intervals from
#' @param concept_filter Named list; per-table concept ID filters
#' @return Data frame with row_id, subject_id, interval_type, concept_id,
#'   start_days_from_index, end_days_from_index
#' @keywords internal
.extractIntervalsLong <- function(handle, cohort_table, tables,
                                   concept_filter = NULL) {
  if (is.null(cohort_table)) {
    warning("Intervals output requires a cohort; returning NULL.",
            call. = FALSE)
    return(NULL)
  }

  .assertMinPersons(
    conn = handle$conn,
    sql = paste0("SELECT COUNT(DISTINCT subject_id) AS n_persons FROM ",
                 cohort_table)
  )

  bp <- .buildBlueprint(handle)
  all_intervals <- list()

  for (tbl_name in tables) {
    tbl_lower <- tolower(tbl_name)

    # Skip if not present in DB
    tbl_row <- bp$tables[bp$tables$table_name == tbl_lower, , drop = FALSE]
    if (nrow(tbl_row) == 0 || !tbl_row$present_in_db[1]) next

    # Get date pair — skip if no start/end pair
    date_pair <- .getDatePair(bp, tbl_lower)
    if (is.null(date_pair)) next

    # Get domain concept column — use concept_role to avoid
    # returning type_concept columns (e.g. period_type_concept_id)
    col_df <- bp$columns[[tbl_lower]]
    domain_cols <- col_df$column_name[col_df$concept_role == "domain_concept"]
    concept_col <- if (length(domain_cols) > 0) domain_cols[1] else NULL

    # Resolve qualified table name
    schema <- .resolveTableSchema(
      handle, tbl_lower, tbl_row$schema_category[1]
    )
    qualified <- .qualifyTable(handle, tbl_lower, schema)

    # Build SELECT
    select_parts <- paste0(
      "t.person_id, t.", date_pair$start, ", t.", date_pair$end
    )
    if (!is.null(concept_col)) {
      select_parts <- paste0(select_parts, ", t.", concept_col)
    }
    select_parts <- paste0(select_parts, ", c.cohort_start_date")

    sql <- paste0(
      "SELECT ", select_parts,
      " FROM ", qualified, " AS t",
      " INNER JOIN ", cohort_table,
      " AS c ON c.subject_id = t.person_id"
    )

    # Apply per-table concept filter
    tbl_concepts <- concept_filter[[tbl_lower]] %||%
      concept_filter[[tbl_name]]
    if (!is.null(tbl_concepts) && !is.null(concept_col)) {
      ids_str <- paste(as.integer(tbl_concepts), collapse = ", ")
      sql <- paste0(sql, " WHERE t.", concept_col, " IN (", ids_str, ")")
    }

    tbl_df <- .executeQuery(handle, sql)
    if (nrow(tbl_df) == 0) next

    # Compute relative days
    start_dates <- as.Date(tbl_df[[date_pair$start]])
    end_dates <- as.Date(tbl_df[[date_pair$end]])
    index_dates <- as.Date(tbl_df$cohort_start_date)

    interval_df <- data.frame(
      subject_id = as.integer(tbl_df$person_id),
      interval_type = rep(tbl_lower, nrow(tbl_df)),
      concept_id = if (!is.null(concept_col))
        as.integer(tbl_df[[concept_col]])
      else
        rep(NA_integer_, nrow(tbl_df)),
      start_days_from_index = as.integer(start_dates - index_dates),
      end_days_from_index = as.integer(end_dates - index_dates),
      stringsAsFactors = FALSE
    )

    all_intervals[[tbl_lower]] <- interval_df
  }

  if (length(all_intervals) == 0) {
    return(data.frame(
      row_id = integer(0), subject_id = integer(0),
      interval_type = character(0), concept_id = integer(0),
      start_days_from_index = integer(0),
      end_days_from_index = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  result <- do.call(rbind, all_intervals)
  rownames(result) <- NULL
  result$row_id <- seq_len(nrow(result))

  result[, c("row_id", "subject_id", "interval_type", "concept_id",
             "start_days_from_index", "end_days_from_index")]
}

# --- Temporal Covariates ---

#' Generate time window bins
#'
#' @param bin_width Integer; bin width in days
#' @param window_start Integer; start of window (days from index)
#' @param window_end Integer; end of window (days from index)
#' @return Data frame with timeId, startDay, endDay
#' @keywords internal
.generateTimeWindows <- function(bin_width, window_start, window_end) {
  starts <- seq(as.integer(window_start),
                as.integer(window_end) - 1L,
                by = as.integer(bin_width))
  ends <- pmin(starts + as.integer(bin_width) - 1L,
               as.integer(window_end))

  data.frame(
    timeId = seq_along(starts),
    startDay = as.integer(starts),
    endDay = as.integer(ends),
    stringsAsFactors = FALSE
  )
}

#' Extract temporal (time-binned) covariates in sparse format
#'
#' R-side binning approach matching the existing \code{.toSparse()} pattern.
#' Extracts events within the time window, assigns to bins, and computes
#' binary/count analyses per concept x time bin.
#'
#' @param handle CDM handle
#' @param cohort_table Character; temp table name with cohort members
#' @param table Character; source OMOP table
#' @param concept_filter Numeric vector; concept IDs to include
#' @param bin_width Integer; bin width in days
#' @param window_start Integer; start of window (days from index)
#' @param window_end Integer; end of window (days from index)
#' @param analyses Character vector; analyses to compute
#' @return Named list with temporalCovariates, covariateRef, timeRef
#' @keywords internal
.extractTemporalCovariates <- function(handle, cohort_table, table,
                                        concept_filter = NULL,
                                        bin_width = 30L,
                                        window_start = -365L,
                                        window_end = 0L,
                                        analyses = c("binary")) {
  if (is.null(cohort_table)) {
    warning("Temporal covariates output requires a cohort; returning NULL.",
            call. = FALSE)
    return(NULL)
  }

  bin_width <- as.integer(bin_width)
  window_start <- as.integer(window_start)
  window_end <- as.integer(window_end)

  # Extract events with days_from_index via .extractTable
  events <- .extractTable(
    handle,
    table = table,
    concept_filter = concept_filter,
    cohort_table = cohort_table,
    add_cohort_date = TRUE,
    temporal = list(
      index_window = list(start = window_start, end = window_end)
    ),
    block_sensitive = TRUE
  )

  # Generate time windows
  time_ref <- .generateTimeWindows(bin_width, window_start, window_end)

  # Empty result template
  empty_result <- list(
    temporalCovariates = data.frame(
      rowId = integer(0), timeId = integer(0),
      covariateId = numeric(0), covariateValue = numeric(0),
      stringsAsFactors = FALSE
    ),
    covariateRef = data.frame(
      covariateId = numeric(0), covariateName = character(0),
      analysisId = integer(0), conceptId = integer(0),
      stringsAsFactors = FALSE
    ),
    timeRef = time_ref
  )

  if (nrow(events) == 0 || !"days_from_index" %in% names(events)) {
    return(empty_result)
  }

  # Find concept column
  possible <- grep("_concept_id$", names(events), value = TRUE)
  possible <- possible[!grepl("_type_concept_id$|_source_concept_id$",
                               possible)]
  concept_col <- if (length(possible) > 0) possible[1] else NULL

  if (is.null(concept_col)) return(empty_result)

  # Assign each event to a time bin
  dfi <- events$days_from_index
  time_ids <- floor((dfi - window_start) / bin_width) + 1L
  time_ids <- as.integer(time_ids)
  # Clamp to valid range
  time_ids <- pmin(pmax(time_ids, 1L), nrow(time_ref))
  events$.timeId <- time_ids

  # Build row_id mapping
  persons <- sort(unique(events$person_id))
  row_map <- stats::setNames(seq_along(persons),
                              as.character(persons))

  concepts <- sort(unique(events[[concept_col]]))

  covariates <- data.frame(
    rowId = integer(0), timeId = integer(0),
    covariateId = numeric(0), covariateValue = numeric(0),
    stringsAsFactors = FALSE
  )
  covariate_ref <- data.frame(
    covariateId = numeric(0), covariateName = character(0),
    analysisId = integer(0), conceptId = integer(0),
    stringsAsFactors = FALSE
  )

  analysis_map <- list(binary = 1L, count = 2L)

  for (cid in concepts) {
    concept_label <- .standardizeName(as.character(cid))
    if (is.na(concept_label) || concept_label == "") {
      concept_label <- paste0("concept_", cid)
    }

    c_events <- events[events[[concept_col]] == cid, , drop = FALSE]

    for (analysis_name in analyses) {
      aid <- analysis_map[[analysis_name]]
      if (is.null(aid)) next

      cov_id <- as.numeric(cid) * 1000 + aid

      if (analysis_name == "binary") {
        # Unique (person, time_bin) pairs
        uniq <- unique(c_events[, c("person_id", ".timeId"),
                                 drop = FALSE])
        if (nrow(uniq) > 0) {
          covariates <- rbind(covariates, data.frame(
            rowId = as.integer(
              row_map[as.character(uniq$person_id)]
            ),
            timeId = uniq$.timeId,
            covariateId = rep(cov_id, nrow(uniq)),
            covariateValue = rep(1, nrow(uniq)),
            stringsAsFactors = FALSE
          ))
        }
      } else if (analysis_name == "count") {
        # Count events per (person, time_bin)
        count_agg <- stats::aggregate(
          c_events[[concept_col]],
          by = list(person_id = c_events$person_id,
                    timeId = c_events$.timeId),
          FUN = length
        )
        if (nrow(count_agg) > 0) {
          covariates <- rbind(covariates, data.frame(
            rowId = as.integer(
              row_map[as.character(count_agg$person_id)]
            ),
            timeId = count_agg$timeId,
            covariateId = rep(cov_id, nrow(count_agg)),
            covariateValue = as.numeric(count_agg$x),
            stringsAsFactors = FALSE
          ))
        }
      }

      covariate_ref <- rbind(covariate_ref, data.frame(
        covariateId = cov_id,
        covariateName = paste0(concept_label, "_", analysis_name),
        analysisId = aid,
        conceptId = as.integer(cid),
        stringsAsFactors = FALSE
      ))
    }
  }

  list(
    temporalCovariates = covariates,
    covariateRef = covariate_ref,
    timeRef = time_ref
  )
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
      # intervals_long: bare concept_id column
      if ("concept_id" %in% names(res)) {
        vals <- res[["concept_id"]]
        ids <- c(ids, as.integer(vals[!is.na(vals)]))
      }
    } else if (is.list(res) &&
               "temporalCovariates" %in% names(res)) {
      # temporal_covariates: extract from covariateRef
      if ("covariateRef" %in% names(res) &&
          "conceptId" %in% names(res$covariateRef)) {
        ids <- as.integer(res$covariateRef$conceptId)
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
