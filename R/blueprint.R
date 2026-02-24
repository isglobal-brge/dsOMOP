# ==============================================================================
# dsOMOP v2 - SchemaBlueprint
# ==============================================================================
# Fuses vendored OHDSI Table_Level + Field_Level CSVs with runtime DB
# introspection. Replaces the legacy introspection.R + handle.R.
# ==============================================================================

# --- OHDSI Metadata Loading ---

#' Load vendored OHDSI CDM metadata CSVs
#'
#' @return List with table_level and field_level data frames
#' @keywords internal
.loadOHDSIMetadata <- function() {
  pkg_dir <- system.file("ohdsi", package = "dsOMOP")
  if (pkg_dir == "") {
    pkg_dir <- system.file("ohdsi", package = "dsOMOP", lib.loc = .libPaths())
  }

  tbl_file <- file.path(pkg_dir, "OMOP_CDMv5.4_Table_Level.csv")
  fld_file <- file.path(pkg_dir, "OMOP_CDMv5.4_Field_Level.csv")

  if (!file.exists(tbl_file) || !file.exists(fld_file)) {
    stop("OHDSI metadata files not found in inst/ohdsi/. ",
         "Ensure package is properly installed.", call. = FALSE)
  }

  table_level <- utils::read.csv(tbl_file, stringsAsFactors = FALSE)
  field_level <- utils::read.csv(fld_file, stringsAsFactors = FALSE)

  list(table_level = table_level, field_level = field_level)
}

# --- Handle Creation ---

#' Create a CDM handle from a resource client
#'
#' @param resource_client An OMOPResourceClient instance
#' @param cdm_schema Character; override CDM schema
#' @param vocab_schema Character; override vocabulary schema
#' @param results_schema Character; override results schema
#' @param temp_schema Character; override temp schema
#' @param config Named list; additional configuration
#' @return A CDM handle (environment)
#' @keywords internal
.createHandle <- function(resource_client,
                          cdm_schema = NULL,
                          vocab_schema = NULL,
                          results_schema = NULL,
                          temp_schema = NULL,
                          config = list()) {
  conn <- resource_client$getConnection()
  parsed <- resource_client$getParsed()
  dbms <- parsed$dbms

  cdm_schema     <- cdm_schema     %||% parsed$cdm_schema
  vocab_schema   <- vocab_schema   %||% parsed$vocabulary_schema %||% cdm_schema
  results_schema <- results_schema %||% parsed$results_schema
  temp_schema    <- temp_schema    %||% parsed$temp_schema

  handle <- new.env(parent = emptyenv())
  handle$conn            <- conn
  handle$dbms            <- dbms
  handle$target_dialect  <- .resolve_target_dialect(dbms)
  handle$cdm_schema      <- cdm_schema
  handle$vocab_schema    <- vocab_schema
  handle$results_schema  <- results_schema
  handle$temp_schema     <- temp_schema
  handle$resource_client <- resource_client
  handle$config          <- config
  handle$blueprint       <- NULL
  handle$temp_tables     <- character(0)

  handle
}

#' Close a CDM handle
#'
#' @param handle A CDM handle
#' @keywords internal
.closeHandle <- function(handle) {
  if (is.null(handle)) return(invisible(NULL))

  conn <- handle$conn

  if (length(handle$temp_tables) > 0 && DBI::dbIsValid(conn)) {
    for (tbl in handle$temp_tables) {
      .dropTempTable(handle, tbl)
    }
  }

  handle$resource_client$close()
  invisible(NULL)
}

# --- Blueprint Construction ---

#' Build the SchemaBlueprint for a handle
#'
#' Fuses vendored OHDSI metadata with runtime DB introspection.
#'
#' @param handle CDM handle
#' @param force Logical; rebuild even if already cached
#' @return The blueprint (also stored in handle$blueprint)
#' @keywords internal
.buildBlueprint <- function(handle, force = FALSE) {
  if (!is.null(handle$blueprint) && !force) {
    return(handle$blueprint)
  }

  ohdsi <- .loadOHDSIMetadata()
  tbl_meta <- ohdsi$table_level
  fld_meta <- ohdsi$field_level

  # Discover tables actually present in the DB
  db_tables_cdm <- .listTablesRaw(handle, handle$cdm_schema)

  db_tables_vocab <- character(0)
  if (!is.null(handle$vocab_schema) && handle$vocab_schema != handle$cdm_schema) {
    db_tables_vocab <- .listTablesRaw(handle, handle$vocab_schema)
  }

  db_tables_results <- character(0)
  if (!is.null(handle$results_schema) && handle$results_schema != handle$cdm_schema) {
    db_tables_results <- .listTablesRaw(handle, handle$results_schema)
  }

  all_db_tables <- unique(c(db_tables_cdm, db_tables_vocab, db_tables_results))

  # Build tables data.frame
  tables <- data.frame(
    table_name      = tolower(tbl_meta$cdmTableName),
    schema_category = tbl_meta$schema,
    concept_prefix  = tbl_meta$conceptPrefix,
    has_person_id   = logical(nrow(tbl_meta)),
    present_in_db   = logical(nrow(tbl_meta)),
    qualified_name  = character(nrow(tbl_meta)),
    stringsAsFactors = FALSE
  )

  # Determine which tables exist and where
  for (i in seq_len(nrow(tables))) {
    tbl_name <- tables$table_name[i]
    category <- tables$schema_category[i]
    tables$present_in_db[i] <- tbl_name %in% all_db_tables

    schema <- .resolveTableSchema(handle, tbl_name, category)
    tables$qualified_name[i] <- .qualifyTable(handle, tbl_name, schema)
  }

  # Build columns: named list of data.frames per table
  columns <- list()
  for (tbl_name in tables$table_name[tables$present_in_db]) {
    tbl_flds <- fld_meta[tolower(fld_meta$cdmTableName) == tbl_name, , drop = FALSE]
    category <- tables$schema_category[tables$table_name == tbl_name]
    schema <- .resolveTableSchema(handle, tbl_name, category)

    # Get actual DB columns
    db_cols <- .listColumnsRaw(handle, tbl_name, schema)
    concept_prefix <- tables$concept_prefix[tables$table_name == tbl_name]

    if (nrow(db_cols) == 0) next

    # Build column metadata by merging OHDSI + DB
    col_df <- data.frame(
      column_name  = db_cols$column_name,
      cdm_datatype = character(nrow(db_cols)),
      db_datatype  = db_cols$data_type,
      concept_role = character(nrow(db_cols)),
      fk_domain    = character(nrow(db_cols)),
      is_date      = logical(nrow(db_cols)),
      is_sensitive  = logical(nrow(db_cols)),
      is_blocked   = logical(nrow(db_cols)),
      stringsAsFactors = FALSE
    )

    for (j in seq_len(nrow(col_df))) {
      col_name <- col_df$column_name[j]
      ohdsi_row <- tbl_flds[tolower(tbl_flds$cdmFieldName) == col_name, , drop = FALSE]

      if (nrow(ohdsi_row) > 0) {
        col_df$cdm_datatype[j] <- ohdsi_row$cdmDatatype[1]
        fk_domain <- ohdsi_row$fkDomain[1]
        col_df$fk_domain[j] <- if (is.na(fk_domain)) "" else fk_domain
        is_fk <- ohdsi_row$isForeignKey[1]
        fk_table <- ohdsi_row$fkTableName[1]

        col_df$concept_role[j] <- .classifyConceptRole(
          tbl_name, col_name, concept_prefix,
          col_df$fk_domain[j],
          is_fk = (!is.na(is_fk) && toupper(is_fk) == "YES"),
          fk_table = if (is.na(fk_table)) "" else fk_table
        )
      } else {
        col_df$concept_role[j] <- "non_concept"
      }

      col_df$is_date[j] <- grepl("_date$|_datetime$", col_name) ||
        grepl("^date$|^datetime$", tolower(col_df$cdm_datatype[j]))

      col_df$is_sensitive[j] <- .detectSensitiveColumns(col_name)
      col_df$is_blocked[j] <- col_df$is_sensitive[j]
    }

    columns[[tbl_name]] <- col_df

    # Update has_person_id in tables
    tables$has_person_id[tables$table_name == tbl_name] <-
      "person_id" %in% col_df$column_name
  }

  # Discover Achilles tables in results_schema (not in vendored OHDSI CSVs)
  achilles_table_names <- c("achilles_results", "achilles_results_dist",
                             "achilles_heel_results")
  found_achilles <- intersect(tolower(db_tables_results), achilles_table_names)
  # Also check CDM tables for SQLite (no separate schemas)
  if (length(found_achilles) == 0) {
    found_achilles <- intersect(tolower(db_tables_cdm), achilles_table_names)
  }
  if (length(found_achilles) > 0) {
    achilles_schema <- if (length(intersect(tolower(db_tables_results),
                                             achilles_table_names)) > 0) {
      handle$results_schema
    } else {
      handle$cdm_schema
    }
    achilles_rows <- data.frame(
      table_name      = found_achilles,
      schema_category = rep("Results", length(found_achilles)),
      concept_prefix  = rep(NA_character_, length(found_achilles)),
      has_person_id   = rep(FALSE, length(found_achilles)),
      present_in_db   = rep(TRUE, length(found_achilles)),
      qualified_name  = vapply(found_achilles, function(t) {
        .qualifyTable(handle, t, achilles_schema)
      }, character(1)),
      stringsAsFactors = FALSE
    )
    tables <- rbind(tables, achilles_rows)
  }
  handle$has_achilles <- length(found_achilles) > 0

  # Build join graph from OHDSI FK metadata
  join_graph <- .buildJoinGraph(fld_meta, tables$table_name[tables$present_in_db])

  # Detect CDM info
  cdm_info <- .detectCDMInfo(handle, tables$table_name[tables$present_in_db])

  blueprint <- new.env(parent = emptyenv())
  blueprint$tables     <- tables
  blueprint$columns    <- columns
  blueprint$join_graph <- join_graph
  blueprint$cdm_info   <- cdm_info

  handle$blueprint <- blueprint
  blueprint
}

# --- Concept Role Classification ---

#' Classify a concept column's role
#'
#' @param table Table name
#' @param field Field name
#' @param concept_prefix Table's concept prefix from OHDSI metadata
#' @param fk_domain FK domain from OHDSI metadata
#' @param is_fk Whether the column is a foreign key
#' @param fk_table Target table of the FK
#' @return Character: domain_concept, type_concept, source_concept, attribute_concept, or non_concept
#' @keywords internal
.classifyConceptRole <- function(table, field, concept_prefix, fk_domain,
                                  is_fk = FALSE, fk_table = "") {
  if (!grepl("_concept_id$", field)) return("non_concept")

  # Source concept IDs
  if (grepl("_source_concept_id$", field)) return("source_concept")

  # Type concepts
  if (!is.na(fk_domain) && tolower(fk_domain) == "type concept") return("type_concept")

  # Domain concept: matches the table's conceptPrefix
  if (!is.na(concept_prefix) && nchar(concept_prefix) > 0) {
    expected_col <- paste0(tolower(concept_prefix), "_concept_id")
    if (field == expected_col) return("domain_concept")
  }

  # If it's a FK to CONCEPT table, it's an attribute concept
  if (is_fk && toupper(fk_table) == "CONCEPT") return("attribute_concept")

  # Default: if it ends in _concept_id, treat as attribute
  "attribute_concept"
}

# --- Sensitive Column Detection ---

#' Detect whether a column contains PII / sensitive data
#'
#' @param column_name Character; column name
#' @return Logical
#' @keywords internal
.detectSensitiveColumns <- function(column_name) {
  sensitive_patterns <- c(
    "_source_value$",
    "^value_as_string$",
    "^note_text$",
    "^note_source_value$",
    "_source_concept_id$"
  )
  any(vapply(sensitive_patterns, function(p) grepl(p, column_name), logical(1)))
}

# --- Schema Resolution ---

#' Resolve which DB schema a table should be queried from
#'
#' @param handle CDM handle
#' @param table Table name
#' @param schema_category Character; CDM, Vocabulary, or Results
#' @return Character; schema name or NULL
#' @keywords internal
.resolveTableSchema <- function(handle, table, schema_category) {
  category <- tolower(schema_category)
  if (category == "vocabulary" && !is.null(handle$vocab_schema)) {
    return(handle$vocab_schema)
  }
  if (category == "results" && !is.null(handle$results_schema)) {
    return(handle$results_schema)
  }
  handle$cdm_schema
}

#' Build a schema-qualified table reference
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param schema Character; schema name
#' @return Character; qualified table name for SQL
#' @keywords internal
.qualifyTable <- function(handle, table, schema = NULL) {
  schema <- schema %||% handle$cdm_schema
  if (is.null(schema) || schema == "" || handle$target_dialect == "sqlite") {
    return(table)
  }
  paste0(schema, ".", table)
}

# --- Blueprint Query Helpers ---

#' Get the domain concept column for a table
#'
#' Uses OHDSI conceptPrefix metadata instead of heuristic suffix stripping.
#'
#' @param blueprint The schema blueprint
#' @param table Character; table name
#' @return Character; domain concept column name, or NULL
#' @keywords internal
.getDomainConceptColumn <- function(blueprint, table) {
  table <- tolower(table)
  tbl_row <- blueprint$tables[blueprint$tables$table_name == table, , drop = FALSE]
  if (nrow(tbl_row) == 0) return(NULL)

  prefix <- tbl_row$concept_prefix[1]
  if (is.na(prefix) || nchar(prefix) == 0) return(NULL)

  expected_col <- paste0(tolower(prefix), "_concept_id")

  # Verify it exists
  cols <- blueprint$columns[[table]]
  if (!is.null(cols) && expected_col %in% cols$column_name) {
    return(expected_col)
  }

  # Fallback: look for the domain_concept role in columns
  if (!is.null(cols)) {
    domain_cols <- cols$column_name[cols$concept_role == "domain_concept"]
    if (length(domain_cols) > 0) return(domain_cols[1])
  }

  NULL
}

#' Get the primary date column for a table
#'
#' @param blueprint The schema blueprint
#' @param table Character; table name
#' @return Character; date column name, or NULL
#' @keywords internal
.getDateColumn <- function(blueprint, table) {
  table <- tolower(table)
  cols <- blueprint$columns[[table]]
  if (is.null(cols)) return(NULL)

  date_cols <- cols$column_name[cols$is_date]
  if (length(date_cols) == 0) return(NULL)

  # Prefer _start_date columns
  start_dates <- grep("_start_date$", date_cols, value = TRUE)
  if (length(start_dates) > 0) return(start_dates[1])

  # Then prefer _date columns (not _end_date)
  plain_dates <- grep("_date$", date_cols, value = TRUE)
  plain_dates <- setdiff(plain_dates, grep("_end_date$", plain_dates, value = TRUE))
  if (length(plain_dates) > 0) return(plain_dates[1])

  date_cols[1]
}

#' Get start/end date column pair for interval tables
#'
#' Returns a list with \code{start} and \code{end} date column names for
#' tables that have interval data (e.g. observation_period, visit_occurrence,
#' condition_occurrence, drug_exposure, drug_era, condition_era).
#' Returns NULL for single-date tables (measurement, procedure_occurrence).
#'
#' @param blueprint The schema blueprint
#' @param table Character; table name
#' @return Named list with \code{start} and \code{end}, or NULL
#' @keywords internal
.getDatePair <- function(blueprint, table) {
  table <- tolower(table)
  cols <- blueprint$columns[[table]]
  if (is.null(cols)) return(NULL)

  date_cols <- cols$column_name[cols$is_date]
  if (length(date_cols) == 0) return(NULL)

  # Find a _start_date column
  start_cols <- grep("_start_date$", date_cols, value = TRUE)
  if (length(start_cols) == 0) return(NULL)

  start_col <- start_cols[1]

  # Derive the _end_date column by substitution

  end_col <- sub("_start_date$", "_end_date", start_col)

  # Verify the end column exists
  if (!end_col %in% cols$column_name) return(NULL)

  list(start = start_col, end = end_col)
}

#' Find a join path between tables using BFS
#'
#' @param blueprint The schema blueprint
#' @param from_table Character; starting table
#' @param to_col Character; target column to reach
#' @return List with path and joins, or NULL
#' @keywords internal
.findJoinPath <- function(blueprint, from_table, to_col = "person_id") {
  edges <- blueprint$join_graph
  columns <- blueprint$columns

  from_table <- tolower(from_table)

  # Check if start table has target column
  if (!is.null(columns[[from_table]]) &&
      to_col %in% columns[[from_table]]$column_name) {
    return(list(path = from_table, joins = list()))
  }

  # BFS
  visited <- character(0)
  queue <- list(list(table = from_table, path = from_table, joins = list()))

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]

    if (current$table %in% visited) next
    visited <- c(visited, current$table)

    out_edges <- edges[edges$from_table == current$table, , drop = FALSE]

    for (i in seq_len(nrow(out_edges))) {
      next_table <- out_edges$to_table[i]
      if (next_table %in% visited) next

      new_join <- list(
        from_table  = current$table,
        from_column = out_edges$from_column[i],
        to_table    = next_table,
        to_column   = out_edges$to_column[i]
      )
      new_joins <- c(current$joins, list(new_join))
      new_path <- c(current$path, next_table)

      if (!is.null(columns[[next_table]]) &&
          to_col %in% columns[[next_table]]$column_name) {
        return(list(path = new_path, joins = new_joins))
      }

      queue <- c(queue, list(list(
        table = next_table, path = new_path, joins = new_joins
      )))
    }
  }

  NULL
}

#' Get capabilities signature for client validation
#'
#' @param handle CDM handle
#' @return Named list with schema summary
#' @keywords internal
.getCapabilities <- function(handle) {
  bp <- .buildBlueprint(handle)
  present <- bp$tables[bp$tables$present_in_db, ]

  tbl_sig <- paste(sort(present$table_name), collapse = ",")
  col_counts <- vapply(bp$columns, nrow, integer(1))
  col_sig <- paste(names(col_counts), col_counts, sep = ":", collapse = ",")

  sig_string <- paste(tbl_sig, col_sig, sep = "|")
  sig_hash <- substr(
    paste(as.character(charToRaw(sig_string)), collapse = ""),
    1, 32
  )

  list(
    hash = sig_hash,
    n_tables = nrow(present),
    tables = present$table_name,
    schema_categories = stats::setNames(present$schema_category, present$table_name),
    cdm_info = bp$cdm_info,
    dbms = handle$dbms,
    cdm_schema = handle$cdm_schema,
    vocab_schema = handle$vocab_schema,
    results_schema = handle$results_schema
  )
}

# --- Internal Helpers ---

#' List tables in a schema (raw DB query)
#'
#' @param handle CDM handle
#' @param schema Character; schema name
#' @return Character vector of table names (lowercase)
#' @keywords internal
.listTablesRaw <- function(handle, schema = NULL) {
  conn <- handle$conn

  if (handle$target_dialect == "sqlite") {
    result <- DBI::dbGetQuery(conn,
      "SELECT name AS table_name FROM sqlite_master WHERE type='table' ORDER BY name")
    tables <- result$table_name %||% result$name %||% character(0)
  } else {
    schema_to_use <- schema %||% handle$cdm_schema %||% "public"
    sql <- .renderSql(handle,
      "SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES
       WHERE TABLE_SCHEMA = '@schema' AND TABLE_TYPE = 'BASE TABLE'
       ORDER BY TABLE_NAME",
      schema = schema_to_use)
    result <- DBI::dbGetQuery(conn, sql)
    tables <- result$table_name %||% result$TABLE_NAME %||% character(0)
  }

  tolower(tables)
}

#' List columns in a table (raw DB query)
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param schema Character; schema name
#' @return Data frame with column_name, data_type, is_nullable
#' @keywords internal
.listColumnsRaw <- function(handle, table, schema = NULL) {
  conn <- handle$conn
  empty <- data.frame(column_name = character(0), data_type = character(0),
                      is_nullable = character(0), stringsAsFactors = FALSE)

  if (handle$target_dialect == "sqlite") {
    result <- DBI::dbGetQuery(conn, paste0("PRAGMA table_info('", table, "')"))
    if (nrow(result) > 0) {
      data.frame(
        column_name = tolower(result$name),
        data_type   = tolower(result$type),
        is_nullable = ifelse(result$notnull == 0, "YES", "NO"),
        stringsAsFactors = FALSE
      )
    } else {
      empty
    }
  } else {
    schema_to_use <- schema %||% handle$cdm_schema %||% "public"
    sql <- .renderSql(handle,
      "SELECT COLUMN_NAME, DATA_TYPE, IS_NULLABLE
       FROM INFORMATION_SCHEMA.COLUMNS
       WHERE TABLE_SCHEMA = '@schema' AND TABLE_NAME = '@table'
       ORDER BY ORDINAL_POSITION",
      schema = schema_to_use, table = table)
    result <- DBI::dbGetQuery(conn, sql)
    if (nrow(result) > 0) {
      data.frame(
        column_name = tolower(result$column_name %||% result$COLUMN_NAME),
        data_type   = tolower(result$data_type %||% result$DATA_TYPE),
        is_nullable = result$is_nullable %||% result$IS_NULLABLE,
        stringsAsFactors = FALSE
      )
    } else {
      empty
    }
  }
}

#' Build join graph from OHDSI FK metadata
#'
#' @param field_level Field-level metadata data.frame
#' @param present_tables Character vector of tables present in DB
#' @return Data frame with from_table, from_column, to_table, to_column
#' @keywords internal
.buildJoinGraph <- function(field_level, present_tables) {
  edges <- data.frame(
    from_table  = character(0),
    from_column = character(0),
    to_table    = character(0),
    to_column   = character(0),
    stringsAsFactors = FALSE
  )

  # Known FK targets: column -> (table, pk)
  known_targets <- list(
    person_id             = list(table = "person",              pk = "person_id"),
    visit_occurrence_id   = list(table = "visit_occurrence",    pk = "visit_occurrence_id"),
    visit_detail_id       = list(table = "visit_detail",        pk = "visit_detail_id"),
    provider_id           = list(table = "provider",            pk = "provider_id"),
    care_site_id          = list(table = "care_site",           pk = "care_site_id"),
    location_id           = list(table = "location",            pk = "location_id"),
    observation_period_id = list(table = "observation_period",  pk = "observation_period_id"),
    episode_id            = list(table = "episode",             pk = "episode_id")
  )

  fk_rows <- field_level[
    !is.na(field_level$isForeignKey) & toupper(field_level$isForeignKey) == "YES",
    , drop = FALSE
  ]

  for (i in seq_len(nrow(fk_rows))) {
    from_tbl <- tolower(fk_rows$cdmTableName[i])
    from_col <- tolower(fk_rows$cdmFieldName[i])
    fk_table_raw <- fk_rows$fkTableName[i]

    if (is.na(fk_table_raw) || nchar(fk_table_raw) == 0) next
    if (!from_tbl %in% present_tables) next

    to_tbl <- tolower(fk_table_raw)

    # Skip concept FKs for the join graph (too many)
    if (to_tbl == "concept") next
    if (!to_tbl %in% present_tables) next

    # Use known targets for PK resolution
    if (from_col %in% names(known_targets)) {
      target <- known_targets[[from_col]]
      if (target$table == to_tbl) {
        edges <- rbind(edges, data.frame(
          from_table = from_tbl, from_column = from_col,
          to_table = to_tbl, to_column = target$pk,
          stringsAsFactors = FALSE
        ))
        next
      }
    }

    # Generic: assume PK is table_name + "_id"
    to_pk <- paste0(to_tbl, "_id")
    edges <- rbind(edges, data.frame(
      from_table = from_tbl, from_column = from_col,
      to_table = to_tbl, to_column = to_pk,
      stringsAsFactors = FALSE
    ))
  }

  if (nrow(edges) > 0) edges <- unique(edges)
  edges
}

#' Detect CDM info from cdm_source table
#'
#' @param handle CDM handle
#' @param tables Character vector of available tables
#' @return Named list or NULL
#' @keywords internal
.detectCDMInfo <- function(handle, tables) {
  if (!"cdm_source" %in% tables) return(NULL)

  tryCatch({
    qualified <- .qualifyTable(handle, "cdm_source")
    sql <- .renderSql(handle,
      "SELECT TOP 1 * FROM @qualified",
      qualified = qualified)
    result <- DBI::dbGetQuery(handle$conn, sql)
    if (nrow(result) == 0) return(NULL)

    names(result) <- tolower(names(result))
    list(
      cdm_version         = result$cdm_version[1] %||% NULL,
      vocabulary_version  = result$vocabulary_version[1] %||% NULL,
      source_name         = result$cdm_source_name[1] %||% NULL,
      source_abbreviation = result$cdm_source_abbreviation[1] %||% NULL
    )
  }, error = function(e) NULL)
}

# --- Temp Table Helpers ---

#' Create a temporary table in the database
#'
#' @param handle CDM handle
#' @param name Character; temp table name
#' @param select_sql Character; SELECT statement for contents
#' @return The temp table name
#' @keywords internal
.createTempTable <- function(handle, name, select_sql) {
  sql <- paste0("CREATE TEMP TABLE ", name, " AS ", select_sql)
  DBI::dbExecute(handle$conn, sql)
  handle$temp_tables <- c(handle$temp_tables, name)
  name
}

#' Drop a temporary table
#'
#' @param handle CDM handle
#' @param name Character; temp table name
#' @keywords internal
.dropTempTable <- function(handle, name) {
  tryCatch(
    DBI::dbExecute(handle$conn, paste0("DROP TABLE IF EXISTS ", name)),
    error = function(e) NULL
  )
  handle$temp_tables <- setdiff(handle$temp_tables, name)
  invisible(NULL)
}
