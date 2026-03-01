# Module: Blueprint System
# CDM schema introspection, handle creation, vendored spec management.

# --- CDM Spec Loading ---

#' Load CDM spec for a given version using CommonDataModel package
#'
#' @param cdm_version Character; CDM version string (e.g. "5.4", "5.3")
#' @return List with table_level and field_level data.frames, or NULL if unsupported
#' @keywords internal
.loadCdmSpec <- function(cdm_version = NULL) {
  # Normalize version (e.g. "v5.4" -> "5.4", "5.4.0" -> "5.4")
  if (!is.null(cdm_version)) {
    cdm_version <- sub("^[vV]", "", trimws(cdm_version))
    cdm_version <- sub("\\.0$", "", cdm_version)
  }

  # Try vendored first (no Java dependency)
  vendored <- .loadVendoredSpec(cdm_version)
  if (!is.null(vendored)) return(vendored)

  # Fall back to CommonDataModel package (may need Java)
  has_cdm_pkg <- tryCatch(
    requireNamespace("CommonDataModel", quietly = TRUE),
    warning = function(w) FALSE
  )
  if (!has_cdm_pkg) return(.loadVendoredSpec("5.4"))

  supported <- tryCatch(
    CommonDataModel::listSupportedVersions(),
    error = function(e) character(0)
  )

  # Find matching version
  version_to_load <- NULL
  if (!is.null(cdm_version) && cdm_version %in% supported) {
    version_to_load <- cdm_version
  } else if (!is.null(cdm_version) && length(supported) > 0) {
    for (sv in supported) {
      if (startsWith(cdm_version, sv)) { version_to_load <- sv; break }
    }
  }

  if (is.null(version_to_load)) return(.loadVendoredSpec("5.4"))

  pkg_csv <- system.file("csv", package = "CommonDataModel")
  tbl_file <- file.path(pkg_csv, paste0("OMOP_CDMv", version_to_load, "_Table_Level.csv"))
  fld_file <- file.path(pkg_csv, paste0("OMOP_CDMv", version_to_load, "_Field_Level.csv"))

  if (!file.exists(tbl_file) || !file.exists(fld_file)) {
    warning("CDM v", version_to_load, " spec files not found in CommonDataModel package. ",
            "Falling back to vendored spec.", call. = FALSE)
    return(.loadVendoredSpec("5.4"))
  }

  list(
    table_level = utils::read.csv(tbl_file, stringsAsFactors = FALSE),
    field_level = utils::read.csv(fld_file, stringsAsFactors = FALSE),
    version     = version_to_load,
    source      = "CommonDataModel"
  )
}

#' Load vendored OHDSI metadata as fallback
#'
#' @param version Character; CDM version to load (e.g. "5.3", "5.4"). Defaults to "5.4".
#' @return data.frame with the CDM specification.
#' @keywords internal
.loadVendoredSpec <- function(version = NULL) {
  pkg_dir <- system.file("ohdsi", package = "dsOMOP")
  if (pkg_dir == "") {
    pkg_dir <- system.file("ohdsi", package = "dsOMOP", lib.loc = .libPaths())
  }
  version <- sub("^[vV]", "", trimws(version %||% "5.4"))
  version <- sub("\\.0$", "", version)
  for (v in unique(c(version, "5.4"))) {
    tbl_file <- file.path(pkg_dir, paste0("OMOP_CDMv", v, "_Table_Level.csv"))
    fld_file <- file.path(pkg_dir, paste0("OMOP_CDMv", v, "_Field_Level.csv"))
    if (file.exists(tbl_file) && file.exists(fld_file)) {
      return(list(
        table_level = utils::read.csv(tbl_file, stringsAsFactors = FALSE),
        field_level = utils::read.csv(fld_file, stringsAsFactors = FALSE),
        version     = v,
        source      = "vendored"
      ))
    }
  }
  NULL
}

#' Heuristic concept role classification (no spec)
#'
#' @param table Character; the table name.
#' @param column_name Character; the column name to classify.
#' @return Character; one of "primary", "type", "source", "qualifier", or "other".
#' @keywords internal
.classifyConceptRoleHeuristic <- function(table, column) {
  if (grepl("_concept_id$", column)) {
    if (grepl("_source_concept_id$", column)) return("source_concept")
    if (grepl("_type_concept_id$", column)) return("type_concept")
    return("domain_concept")
  }
  "non_concept"
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
#' @return NULL, called for side effect of closing the database connection.
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

  # Step 1: Discover tables actually present in the DB
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

  # Step 2: Detect CDM version from cdm_source (before spec loading)
  cdm_info <- .detectCDMInfo(handle, all_db_tables)
  cdm_version <- cdm_info$cdm_version  # may be NULL

  # Step 2b: Structural version detection (fallback / cross-validation)
  # Wrapped in tryCatch so structural detection can never crash blueprint build.
  struct <- tryCatch(
    .detectCDMVersionFromStructure(handle, all_db_tables),
    error = function(e) NULL
  )

  if (is.null(cdm_version) && !is.null(struct)) {
    cdm_version <- struct$version
    message("CDM version ", cdm_version, " inferred from table structure",
            " (evidence: v5.4=", struct$evidence_54,
            ", v5.3=", struct$evidence_53, ")")
  } else if (!is.null(cdm_version) && !is.null(struct)) {
    # cdm_source is authoritative; structural detection is advisory only
    normalized <- sub("^[vV]", "", trimws(cdm_version))
    normalized <- sub("\\.0$", "", normalized)
    if (normalized != struct$version) {
      message("Note: cdm_source reports version '", cdm_version,
              "' but table structure suggests '", struct$version,
              "' (evidence: v5.4=", struct$evidence_54,
              ", v5.3=", struct$evidence_53, "). Using cdm_source version.")
    }
  }

  # Step 3: Load spec for detected version
  spec <- .loadCdmSpec(cdm_version)
  has_spec <- !is.null(spec)
  if (!has_spec) {
    warning("No CDM spec available for version '", cdm_version %||% "unknown",
            "'. Running in introspection-only mode.", call. = FALSE)
  }

  tbl_meta <- if (has_spec) spec$table_level else NULL
  fld_meta <- if (has_spec) spec$field_level else NULL

  # Build tables data.frame
  if (has_spec) {
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

    # Add DB tables not in spec (introspection discovers extra tables)
    extra_db <- setdiff(all_db_tables, tables$table_name)
    if (length(extra_db) > 0) {
      extra_rows <- data.frame(
        table_name      = extra_db,
        schema_category = rep("CDM", length(extra_db)),
        concept_prefix  = rep(NA_character_, length(extra_db)),
        has_person_id   = rep(FALSE, length(extra_db)),
        present_in_db   = rep(TRUE, length(extra_db)),
        qualified_name  = vapply(extra_db, function(t) {
          .qualifyTable(handle, t, handle$cdm_schema)
        }, character(1)),
        stringsAsFactors = FALSE
      )
      tables <- rbind(tables, extra_rows)
    }
  } else {
    # Introspection-only mode: build from DB tables
    tables <- data.frame(
      table_name      = all_db_tables,
      schema_category = rep("CDM", length(all_db_tables)),
      concept_prefix  = rep(NA_character_, length(all_db_tables)),
      has_person_id   = rep(FALSE, length(all_db_tables)),
      present_in_db   = rep(TRUE, length(all_db_tables)),
      qualified_name  = vapply(all_db_tables, function(t) {
        .qualifyTable(handle, t, handle$cdm_schema)
      }, character(1)),
      stringsAsFactors = FALSE
    )
  }

  # Build columns: named list of data.frames per table
  columns <- list()
  for (tbl_name in tables$table_name[tables$present_in_db]) {
    tbl_flds <- if (has_spec) {
      fld_meta[tolower(fld_meta$cdmTableName) == tbl_name, , drop = FALSE]
    } else {
      data.frame(cdmFieldName = character(0), stringsAsFactors = FALSE)
    }
    category <- tables$schema_category[tables$table_name == tbl_name]
    schema <- .resolveTableSchema(handle, tbl_name, category)

    # Get actual DB columns
    db_cols <- .listColumnsRaw(handle, tbl_name, schema)
    concept_prefix <- tables$concept_prefix[tables$table_name == tbl_name]

    if (nrow(db_cols) == 0) next

    # Build column metadata by merging spec + DB
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

      if (has_spec && nrow(tbl_flds) > 0) {
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
          col_df$concept_role[j] <- .classifyConceptRoleHeuristic(tbl_name, col_name)
        }
      } else {
        # Introspection-only: use heuristic classification
        col_df$concept_role[j] <- .classifyConceptRoleHeuristic(tbl_name, col_name)
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

  # Discover Achilles tables in results_schema (not in OHDSI spec CSVs)
  achilles_table_names <- c("achilles_analysis", "achilles_results",
                             "achilles_results_dist", "achilles_heel_results")
  found_achilles <- intersect(tolower(db_tables_results), achilles_table_names)
  # Also check CDM tables for SQLite (no separate schemas)
  if (length(found_achilles) == 0) {
    found_achilles <- intersect(tolower(db_tables_cdm), achilles_table_names)
  }
  # Avoid duplicating tables already in the tables data.frame
  new_achilles <- setdiff(found_achilles, tables$table_name)
  if (length(new_achilles) > 0) {
    achilles_schema <- if (length(intersect(tolower(db_tables_results),
                                             achilles_table_names)) > 0) {
      handle$results_schema
    } else {
      handle$cdm_schema
    }
    achilles_rows <- data.frame(
      table_name      = new_achilles,
      schema_category = rep("Results", length(new_achilles)),
      concept_prefix  = rep(NA_character_, length(new_achilles)),
      has_person_id   = rep(FALSE, length(new_achilles)),
      present_in_db   = rep(TRUE, length(new_achilles)),
      qualified_name  = vapply(new_achilles, function(t) {
        .qualifyTable(handle, t, achilles_schema)
      }, character(1)),
      stringsAsFactors = FALSE
    )
    tables <- rbind(tables, achilles_rows)
  }
  # Also mark already-present achilles tables as present_in_db
  existing_achilles <- intersect(found_achilles, tables$table_name)
  if (length(existing_achilles) > 0) {
    achilles_schema <- if (length(intersect(tolower(db_tables_results),
                                             achilles_table_names)) > 0) {
      handle$results_schema
    } else {
      handle$cdm_schema
    }
    mask <- tables$table_name %in% existing_achilles
    tables$present_in_db[mask] <- TRUE
    tables$schema_category[mask & tables$schema_category == "CDM"] <- "Results"
    tables$qualified_name[mask] <- vapply(tables$table_name[mask], function(t) {
      .qualifyTable(handle, t, achilles_schema)
    }, character(1))
  }
  handle$has_achilles <- length(found_achilles) > 0

  # Discover OHDSI result tables (DQD, CohortDiagnostics, etc.)
  registry <- .ohdsi_tool_registry()
  all_ohdsi_names <- unlist(lapply(registry, function(t) t$table_names),
                             use.names = FALSE)
  found_ohdsi <- intersect(tolower(db_tables_results), all_ohdsi_names)
  if (length(found_ohdsi) == 0) {
    found_ohdsi <- intersect(tolower(db_tables_cdm), all_ohdsi_names)
  }
  new_ohdsi <- setdiff(found_ohdsi, tables$table_name)
  if (length(new_ohdsi) > 0) {
    ohdsi_schema <- if (length(intersect(tolower(db_tables_results),
                                          all_ohdsi_names)) > 0) {
      handle$results_schema
    } else {
      handle$cdm_schema
    }
    ohdsi_rows <- data.frame(
      table_name      = new_ohdsi,
      schema_category = rep("Results", length(new_ohdsi)),
      concept_prefix  = rep(NA_character_, length(new_ohdsi)),
      has_person_id   = rep(FALSE, length(new_ohdsi)),
      present_in_db   = rep(TRUE, length(new_ohdsi)),
      qualified_name  = vapply(new_ohdsi, function(t) {
        .qualifyTable(handle, t, ohdsi_schema)
      }, character(1)),
      stringsAsFactors = FALSE
    )
    tables <- rbind(tables, ohdsi_rows)
  }
  # Mark already-present OHDSI tables
  existing_ohdsi <- intersect(found_ohdsi, tables$table_name)
  if (length(existing_ohdsi) > 0) {
    ohdsi_schema <- if (length(intersect(tolower(db_tables_results),
                                          all_ohdsi_names)) > 0) {
      handle$results_schema
    } else {
      handle$cdm_schema
    }
    mask <- tables$table_name %in% existing_ohdsi
    tables$present_in_db[mask] <- TRUE
    tables$schema_category[mask & tables$schema_category == "CDM"] <- "Results"
    tables$qualified_name[mask] <- vapply(tables$table_name[mask], function(t) {
      .qualifyTable(handle, t, ohdsi_schema)
    }, character(1))
  }
  handle$has_ohdsi_results <- length(found_ohdsi) > 0

  # Build join graph from spec FK metadata (if available)
  if (has_spec) {
    join_graph <- .buildJoinGraph(fld_meta, tables$table_name[tables$present_in_db])
  } else {
    join_graph <- data.frame(
      from_table = character(0), from_column = character(0),
      to_table = character(0), to_column = character(0),
      stringsAsFactors = FALSE
    )
  }

  blueprint <- new.env(parent = emptyenv())
  blueprint$tables       <- tables
  blueprint$columns      <- columns
  blueprint$join_graph   <- join_graph
  blueprint$cdm_info     <- cdm_info
  blueprint$spec_version <- if (has_spec) spec$version else NULL
  blueprint$spec_source  <- if (has_spec) spec$source else "introspection_only"

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
  if (category %in% c("vocabulary", "vocab") && !is.null(handle$vocab_schema)) {
    return(handle$vocab_schema)
  }
  if (category %in% c("results", "result") && !is.null(handle$results_schema)) {
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

  achilles_table_names <- c("achilles_analysis", "achilles_results",
                              "achilles_results_dist", "achilles_heel_results")

  supported_versions <- if (tryCatch(requireNamespace("CommonDataModel", quietly = TRUE),
                                      warning = function(w) FALSE)) {
    tryCatch(CommonDataModel::listSupportedVersions(), error = function(e) character(0))
  } else {
    character(0)
  }

  # Count total persons (privacy-safe: only the count, no individual data)
  total_persons <- tryCatch({
    if ("person" %in% present$table_name) {
      person_qualified <- .qualifyTable(handle, "person")
      sql <- paste0("SELECT COUNT(*) AS n FROM ", person_qualified)
      res <- .executeQuery(handle, sql)
      as.numeric(res$n[1])
    } else {
      NULL
    }
  }, error = function(e) NULL)

  # Disclosure settings (from DataSHIELD server options)
  disclosure <- tryCatch(.omopDisclosureSettings(), error = function(e) NULL)

  # Filter tables to only those recognized by CDM spec
  spec_tables <- if (!is.null(bp$spec_version)) {
    spec <- .loadCdmSpec(bp$spec_version)
    if (!is.null(spec)) tolower(spec$table_level$cdmTableName) else NULL
  } else NULL

  list(
    hash = sig_hash,
    n_tables = nrow(present),
    total_persons = total_persons,
    tables = present$table_name,
    cdm_tables = if (!is.null(spec_tables)) {
      intersect(present$table_name, spec_tables)
    } else present$table_name,
    schema_categories = stats::setNames(present$schema_category, present$table_name),
    cdm_info = bp$cdm_info,
    spec_version = bp$spec_version,
    spec_source = bp$spec_source,
    supported_versions = supported_versions,
    achilles_available = isTRUE(handle$has_achilles),
    achilles_tables = intersect(achilles_table_names,
                                 bp$tables$table_name[bp$tables$present_in_db]),
    disclosure = disclosure
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

#' Detect CDM version from table/column structure using weighted evidence scoring
#'
#' Uses multiple structural signals to infer whether the database matches
#' CDM v5.3 or v5.4. Table-level checks (episode, episode_event) are free
#' since the table list is already discovered. Column-level checks query
#' only tables that exist.
#'
#' @param handle CDM handle
#' @param db_tables Character vector of table names present in the database
#' @return List with \code{version}, \code{evidence_54}, \code{evidence_53},
#'   and \code{checks} (named list of individual check results), or NULL if
#'   inconclusive (zero evidence or tie).
#' @keywords internal
.detectCDMVersionFromStructure <- function(handle, db_tables) {
  evidence_54 <- 0
  evidence_53 <- 0
  checks <- list()


  # Check 1: episode table exists (+3 for 5.4)
  if ("episode" %in% db_tables) {
    evidence_54 <- evidence_54 + 3
    checks$episode_table <- "5.4"
  }

  # Check 2: episode_event table exists (+3 for 5.4)
  if ("episode_event" %in% db_tables) {
    evidence_54 <- evidence_54 + 3
    checks$episode_event_table <- "5.4"
  }

  # Note: this function runs BEFORE the blueprint is built, so

  # handle$blueprint is NULL. Use handle$cdm_schema directly for column queries.
  schema <- handle$cdm_schema

  # Check 3: procedure_occurrence columns (+2 for winner)
  if ("procedure_occurrence" %in% db_tables) {
    proc_cols <- .listColumnsRaw(handle, "procedure_occurrence", schema)
    if (nrow(proc_cols) > 0) {
      if ("procedure_end_date" %in% proc_cols$column_name) {
        evidence_54 <- evidence_54 + 2
        checks$procedure_end_date <- "5.4"
      } else {
        evidence_53 <- evidence_53 + 2
        checks$procedure_end_date <- "5.3"
      }
    }
  }

  # Check 4: location columns (+1 for winner)
  if ("location" %in% db_tables) {
    loc_cols <- .listColumnsRaw(handle, "location", schema)
    if (nrow(loc_cols) > 0) {
      if ("latitude" %in% loc_cols$column_name) {
        evidence_54 <- evidence_54 + 1
        checks$location_latitude <- "5.4"
      } else {
        evidence_53 <- evidence_53 + 1
        checks$location_latitude <- "5.3"
      }
    }
  }

  # Check 5: visit_detail columns (+2 for winner)
  if ("visit_detail" %in% db_tables) {
    vd_cols <- .listColumnsRaw(handle, "visit_detail", schema)
    if (nrow(vd_cols) > 0) {
      if ("parent_visit_detail_id" %in% vd_cols$column_name) {
        evidence_54 <- evidence_54 + 2
        checks$visit_detail_parent <- "5.4"
      } else if ("visit_detail_parent_id" %in% vd_cols$column_name) {
        evidence_53 <- evidence_53 + 2
        checks$visit_detail_parent <- "5.3"
      }
    }
  }

  # Decision: higher score wins; tie or zero evidence -> NULL
  if (evidence_54 == 0 && evidence_53 == 0) return(NULL)
  if (evidence_54 == evidence_53) return(NULL)

  version <- if (evidence_54 > evidence_53) "5.4" else "5.3"

  list(
    version     = version,
    evidence_54 = evidence_54,
    evidence_53 = evidence_53,
    checks      = checks
  )
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
    result <- .coerce_integer64(DBI::dbGetQuery(handle$conn, sql))
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
#' @return NULL, called for side effect.
#' @keywords internal
.dropTempTable <- function(handle, name) {
  tryCatch(
    DBI::dbExecute(handle$conn, paste0("DROP TABLE IF EXISTS ", name)),
    error = function(e) NULL
  )
  handle$temp_tables <- setdiff(handle$temp_tables, name)
  invisible(NULL)
}
