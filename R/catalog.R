# ==============================================================================
# dsOMOP v2 - Query Catalog System
# ==============================================================================
# Provides a curated, disclosure-safe catalog of SQL query templates for
# OMOP CDM exploration. Inspired by OHDSI QueryLibrary, with DataSHIELD-
# aligned SDC enforcement at runtime.
#
# Architecture:
#   - Queries stored as Markdown files in inst/catalog/queries/
#   - Static safety classification (SAFE_AGGREGATE / SAFE_ASSIGN / BLOCKED)
#   - CI-generated allowlist in inst/catalog/query_allowlist.json
#   - Runtime SDC enforcement via DataSHIELD nfilter settings
#   - Provider interface for extensibility (native, QueryLibrary, Achilles)
# ==============================================================================

# --- Query Markdown Parser ---------------------------------------------------

#' Parse a query template from Markdown format
#'
#' Parses a QueryLibrary-style Markdown file into a structured list containing
#' metadata, SQL template, input parameters, output schema, and sensitive
#' field annotations.
#'
#' @param text Character; Markdown text content
#' @return Named list with id, group, name, description, sql, inputs, outputs,
#'   sensitive_fields, cdm_version, mode
#' @keywords internal
.ql_parse_markdown <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

  # Parse YAML front matter (between --- lines)
  fm_start <- which(trimws(lines) == "---")
  meta <- list()
  if (length(fm_start) >= 2) {
    fm_lines <- lines[(fm_start[1] + 1L):(fm_start[2] - 1L)]
    for (fl in fm_lines) {
      fl <- trimws(fl)
      if (nchar(fl) == 0 || !grepl(":", fl)) next
      key <- trimws(sub(":.*", "", fl))
      val <- trimws(sub("^[^:]+:\\s*", "", fl))
      meta[[tolower(key)]] <- val
    }
    lines <- lines[-(1:fm_start[2])]
  }

  # Parse sections
  sections <- list()
  current_section <- NULL
  current_content <- character(0)

  for (line in lines) {
    if (grepl("^##\\s+", line)) {
      if (!is.null(current_section)) {
        sections[[current_section]] <- paste(current_content, collapse = "\n")
      }
      current_section <- tolower(trimws(sub("^##\\s+", "", line)))
      current_content <- character(0)
    } else {
      current_content <- c(current_content, line)
    }
  }
  if (!is.null(current_section)) {
    sections[[current_section]] <- paste(current_content, collapse = "\n")
  }

  # Extract SQL from code block
  sql <- NULL
  sql_section <- sections[["query"]] %||% sections[["sql"]]
  if (!is.null(sql_section)) {
    sql_lines <- strsplit(sql_section, "\n")[[1]]
    in_block <- FALSE
    sql_parts <- character(0)
    for (sl in sql_lines) {
      if (grepl("^```", sl)) {
        in_block <- !in_block
        next
      }
      if (in_block) sql_parts <- c(sql_parts, sl)
    }
    sql <- trimws(paste(sql_parts, collapse = "\n"))
    if (nchar(sql) == 0) sql <- NULL
  }

  # Parse input table
  inputs <- .ql_parse_table(sections[["input"]] %||% sections[["inputs"]])

  # Parse output table
  outputs <- .ql_parse_table(sections[["output"]] %||% sections[["outputs"]])

  # Parse sensitive fields
  sensitive_text <- sections[["sensitive fields"]] %||%
    sections[["sensitive"]] %||% ""
  sensitive_fields <- trimws(strsplit(sensitive_text, "[,\n]")[[1]])
  sensitive_fields <- sensitive_fields[nchar(sensitive_fields) > 0]

  list(
    id          = meta[["id"]] %||% .ql_derive_id(meta),
    group       = meta[["group"]] %||% "general",
    name        = meta[["name"]] %||% "Unnamed Query",
    description = trimws(sections[["description"]] %||% ""),
    sql         = sql,
    inputs      = inputs,
    outputs     = outputs,
    sensitive_fields = sensitive_fields,
    cdm_version = meta[["cdm version"]] %||% "5.3+",
    mode        = tolower(meta[["mode"]] %||% "aggregate"),
    author      = meta[["author"]] %||% "dsOMOP"
  )
}

#' Parse a Markdown table into a data frame
#' @keywords internal
.ql_parse_table <- function(text) {
  if (is.null(text) || nchar(trimws(text)) == 0) return(NULL)
  lines <- strsplit(text, "\n")[[1]]
  lines <- trimws(lines)
  # Keep lines that look like table rows (contain |)
  tbl_lines <- lines[grepl("\\|", lines)]
  if (length(tbl_lines) < 2) return(NULL)

  # Remove separator line (contains ---)
  tbl_lines <- tbl_lines[!grepl("^[|\\s-]+$", tbl_lines)]
  if (length(tbl_lines) < 1) return(NULL)

  # Parse header
  header <- trimws(strsplit(tbl_lines[1], "\\|")[[1]])
  header <- header[nchar(header) > 0]

  if (length(tbl_lines) < 2) {
    return(data.frame(matrix(nrow = 0, ncol = length(header),
                             dimnames = list(NULL, tolower(header))),
                      stringsAsFactors = FALSE))
  }

  rows <- lapply(tbl_lines[-1], function(r) {
    vals <- trimws(strsplit(r, "\\|")[[1]])
    vals <- vals[nchar(vals) > 0]
    vals <- vals[seq_len(min(length(vals), length(header)))]
    if (length(vals) < length(header)) {
      vals <- c(vals, rep("", length(header) - length(vals)))
    }
    vals
  })

  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- tolower(header)
  df
}

#' Derive a query ID from metadata
#' @keywords internal
.ql_derive_id <- function(meta) {
  group <- gsub("[^a-z0-9]", "_", tolower(meta[["group"]] %||% "general"))
  group <- gsub("_+", "_", group)
  group <- gsub("^_|_$", "", group)
  name <- gsub("[^a-z0-9]", "_", tolower(meta[["name"]] %||% "query"))
  name <- gsub("_+", "_", name)
  name <- gsub("^_|_$", "", name)
  paste0(group, ".", name)
}

# --- Static Safety Classifier -----------------------------------------------

#' Classify a SQL query template by safety level
#'
#' Performs static analysis on rendered SQL to classify it as:
#' \itemize{
#'   \item \code{SAFE_AGGREGATE} - Safe to return results to client
#'   \item \code{SAFE_ASSIGN} - Safe for server-side assignment only
#'   \item \code{BLOCKED} - Cannot be executed (returns identifiers/free-text)
#' }
#'
#' @param sql Character; SQL query text (rendered, before dialect translation)
#' @param declared_mode Character; "aggregate" or "assign" from metadata
#' @return Named list with class, reason, sensitive_fields_detected, poolable
#' @keywords internal
.ql_classify <- function(sql, declared_mode = "aggregate") {
  if (is.null(sql) || nchar(trimws(sql)) == 0) {
    return(list(class = "BLOCKED", reason = "empty SQL",
                sensitive_fields_detected = character(0), poolable = FALSE))
  }

  sql_lower <- tolower(sql)

  # --- BLOCKED checks ---

  # Direct identifier columns
  identifier_cols <- c(
    "person_id", "visit_occurrence_id", "drug_exposure_id",
    "measurement_id", "condition_occurrence_id", "procedure_occurrence_id",
    "observation_id", "device_exposure_id", "specimen_id",
    "note_id", "episode_id"
  )

  # For aggregate mode, identifiers in SELECT are blocked
  if (declared_mode == "aggregate") {
    # Extract SELECT columns (crude but effective)
    select_match <- regmatches(sql_lower,
      regexpr("select\\s+(top\\s+\\d+\\s+)?(.+?)\\s+from\\s+",
              sql_lower, perl = TRUE))
    if (length(select_match) > 0) {
      select_part <- select_match[1]
      for (id_col in identifier_cols) {
        # Match column reference (not inside function like COUNT(DISTINCT person_id))
        # Pattern: the column appears as a bare select item, not inside an aggregate
        pattern <- paste0("\\b", id_col, "\\b")
        if (grepl(pattern, select_part)) {
          # Check if it's inside an aggregate function
          agg_pattern <- paste0(
            "(count|sum|avg|min|max)\\s*\\(\\s*(distinct\\s+)?",
            id_col)
          if (!grepl(agg_pattern, select_part)) {
            return(list(
              class = "BLOCKED",
              reason = paste0("returns identifier column: ", id_col),
              sensitive_fields_detected = character(0),
              poolable = FALSE
            ))
          }
        }
      }
    }
  }

  # Free-text fields
  freetext_cols <- c("note_text", "note_source_value", "value_source_value",
                     "stop_reason", "sig", "route_source_value")
  for (ft_col in freetext_cols) {
    if (grepl(paste0("\\b", ft_col, "\\b"), sql_lower)) {
      return(list(
        class = "BLOCKED",
        reason = paste0("returns free-text field: ", ft_col),
        sensitive_fields_detected = character(0),
        poolable = FALSE
      ))
    }
  }

  # SELECT * from person-level tables in aggregate mode
  if (declared_mode == "aggregate" &&
      grepl("select\\s+\\*", sql_lower)) {
    return(list(
      class = "BLOCKED",
      reason = "SELECT * not allowed in aggregate mode",
      sensitive_fields_detected = character(0),
      poolable = FALSE
    ))
  }

  # Unbinned raw dates in aggregate mode
  if (declared_mode == "aggregate") {
    date_patterns <- c("_date\\b", "_datetime\\b")
    has_raw_date <- any(vapply(date_patterns, function(p) {
      grepl(p, sql_lower)
    }, logical(1)))

    has_date_binning <- grepl(
      "date_trunc|extract\\s*\\(|year\\s*\\(|month\\s*\\(|to_char|format\\(",
      sql_lower)

    if (has_raw_date && !has_date_binning) {
      # Check if dates are in SELECT (not just WHERE)
      select_part <- sub("\\bfrom\\b.*", "", sql_lower)
      if (any(vapply(date_patterns, function(p) grepl(p, select_part), logical(1)))) {
        return(list(
          class = "BLOCKED",
          reason = "unbinned dates in SELECT for aggregate mode",
          sensitive_fields_detected = character(0),
          poolable = FALSE
        ))
      }
    }
  }

  # --- SAFE_AGGREGATE checks ---

  has_group_by <- grepl("\\bgroup\\s+by\\b", sql_lower)
  has_agg <- grepl("\\b(count|sum|avg|min|max)\\s*\\(", sql_lower)

  # Detect sensitive fields (count-like columns in output)
  sensitive_detected <- character(0)
  count_aliases <- regmatches(sql_lower,
    gregexpr("(count|sum)\\s*\\([^)]*\\)\\s+as\\s+(\\w+)", sql_lower, perl = TRUE))
  if (length(count_aliases) > 0) {
    for (match_str in count_aliases[[1]]) {
      alias <- sub(".*\\bas\\s+(\\w+)$", "\\1", match_str)
      sensitive_detected <- c(sensitive_detected, alias)
    }
  }
  # Also check for common count column names
  common_count_cols <- c("n_persons", "n_records", "count_value",
                         "person_count", "record_count", "num_persons")
  for (cc in common_count_cols) {
    if (grepl(paste0("\\b", cc, "\\b"), sql_lower)) {
      sensitive_detected <- c(sensitive_detected, cc)
    }
  }
  sensitive_detected <- unique(sensitive_detected)

  if (declared_mode == "aggregate" && has_group_by && has_agg) {
    return(list(
      class = "SAFE_AGGREGATE",
      reason = "aggregated + grouped query",
      sensitive_fields_detected = sensitive_detected,
      poolable = length(sensitive_detected) > 0
    ))
  }

  # Simple aggregate without GROUP BY (e.g., SELECT COUNT(*))
  if (declared_mode == "aggregate" && has_agg && !has_group_by) {
    return(list(
      class = "SAFE_AGGREGATE",
      reason = "simple aggregate query",
      sensitive_fields_detected = sensitive_detected,
      poolable = TRUE
    ))
  }

  # --- SAFE_ASSIGN ---
  if (declared_mode == "assign") {
    return(list(
      class = "SAFE_ASSIGN",
      reason = "assign mode (data stays server-side)",
      sensitive_fields_detected = sensitive_detected,
      poolable = FALSE
    ))
  }

  # Default: if aggregate mode but no aggregates detected, block
  if (declared_mode == "aggregate") {
    return(list(
      class = "BLOCKED",
      reason = "aggregate mode but no aggregate functions detected",
      sensitive_fields_detected = character(0),
      poolable = FALSE
    ))
  }

  list(
    class = "SAFE_ASSIGN",
    reason = "default classification for non-aggregate mode",
    sensitive_fields_detected = sensitive_detected,
    poolable = FALSE
  )
}

# --- Allowlist Management ----------------------------------------------------

#' Load the query allowlist from inst/catalog/
#'
#' Reads the curated allowlist JSON file that maps query IDs to their safety
#' classification and metadata. Falls back to an empty list if not found.
#'
#' @param package Character; package name to search for allowlist
#' @return Named list keyed by query ID
#' @keywords internal
.ql_load_allowlist <- function(package = "dsOMOP") {
  path <- system.file("catalog", "query_allowlist.json", package = package)
  if (path == "" || !file.exists(path)) return(list())

  tryCatch({
    raw <- readLines(path, warn = FALSE)
    json_text <- paste(raw, collapse = "\n")
    entries <- .ql_parse_json(json_text)
    # Convert list of entries to named list keyed by id
    result <- list()
    for (entry in entries) {
      if (!is.null(entry$id)) {
        result[[entry$id]] <- entry
      }
    }
    result
  }, error = function(e) {
    warning("Failed to load query allowlist: ", e$message, call. = FALSE)
    list()
  })
}

#' Minimal JSON parser for allowlist (avoids jsonlite dependency)
#' @keywords internal
.ql_parse_json <- function(text) {
  # Use R's built-in JSON parsing if available, otherwise basic parsing
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    return(jsonlite::fromJSON(text, simplifyVector = FALSE))
  }
  # Fallback: empty list
  warning("jsonlite not available; allowlist not loaded", call. = FALSE)
  list()
}

# --- Query Template Loading --------------------------------------------------

#' Load all query templates from inst/catalog/queries/
#'
#' Reads all .md files in the queries directory and parses them into
#' structured query objects.
#'
#' @param package Character; package name
#' @return Named list of parsed query objects, keyed by query ID
#' @keywords internal
.ql_load_queries <- function(package = "dsOMOP") {
  queries_dir <- system.file("catalog", "queries", package = package)
  if (queries_dir == "" || !dir.exists(queries_dir)) return(list())

  md_files <- list.files(queries_dir, pattern = "\\.md$",
                         full.names = TRUE, recursive = TRUE)
  if (length(md_files) == 0) return(list())

  result <- list()
  for (f in md_files) {
    tryCatch({
      text <- paste(readLines(f, warn = FALSE), collapse = "\n")
      parsed <- .ql_parse_markdown(text)
      if (!is.null(parsed$sql)) {
        result[[parsed$id]] <- parsed
      }
    }, error = function(e) {
      # Skip invalid files silently
    })
  }
  result
}

# --- Catalog Operations (Internal) -------------------------------------------

#' List available catalog queries
#'
#' Returns metadata for all queries that are on the allowlist and match
#' the requested filters.
#'
#' @param handle CDM handle
#' @param domain Character; optional domain/group filter
#' @param provider Character; "native" (default) or "all"
#' @return Data frame with query metadata
#' @keywords internal
.catalog_list <- function(handle, domain = NULL, provider = "native") {
  queries <- .ql_load_queries()
  allowlist <- .ql_load_allowlist()

  # Build metadata data frame
  entries <- list()
  for (qid in names(queries)) {
    q <- queries[[qid]]
    al <- allowlist[[qid]]

    # Only include queries on the allowlist (or all if no allowlist)
    safety_class <- if (!is.null(al)) al$class else {
      # Classify on the fly if not in allowlist
      cl <- .ql_classify(q$sql, q$mode)
      cl$class
    }

    # Skip BLOCKED queries
    if (safety_class == "BLOCKED") next

    # Apply domain filter
    if (!is.null(domain)) {
      if (tolower(q$group) != tolower(domain)) next
    }

    poolable <- if (!is.null(al)) (al$poolable %||% FALSE) else FALSE

    entries[[length(entries) + 1L]] <- data.frame(
      id = qid,
      group = q$group,
      name = q$name,
      description = substr(q$description, 1, 200),
      mode = q$mode,
      class = safety_class,
      poolable = poolable,
      cdm_version = q$cdm_version,
      n_inputs = if (!is.null(q$inputs)) nrow(q$inputs) else 0L,
      stringsAsFactors = FALSE
    )
  }

  if (length(entries) == 0) {
    return(data.frame(
      id = character(0), group = character(0), name = character(0),
      description = character(0), mode = character(0),
      class = character(0), poolable = logical(0),
      cdm_version = character(0), n_inputs = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, entries)
}

#' Get detailed metadata for a specific catalog query
#'
#' @param handle CDM handle (unused, for future schema validation)
#' @param query_id Character; query ID
#' @return Named list with full query metadata (excluding SQL)
#' @keywords internal
.catalog_get <- function(handle, query_id) {
  queries <- .ql_load_queries()
  q <- queries[[query_id]]
  if (is.null(q)) {
    stop("Query '", query_id, "' not found in catalog.", call. = FALSE)
  }

  allowlist <- .ql_load_allowlist()
  al <- allowlist[[query_id]]
  safety <- if (!is.null(al)) al else .ql_classify(q$sql, q$mode)

  list(
    id = q$id,
    group = q$group,
    name = q$name,
    description = q$description,
    mode = q$mode,
    class = safety$class %||% safety[["class"]],
    poolable = safety$poolable %||% FALSE,
    cdm_version = q$cdm_version,
    inputs = q$inputs,
    outputs = q$outputs,
    sensitive_fields = q$sensitive_fields
  )
}

#' Execute a catalog query with SDC enforcement
#'
#' Renders the SQL template with user inputs, translates to target dialect,
#' executes against the database, and applies disclosure controls.
#'
#' @param handle CDM handle
#' @param query_id Character; query ID from catalog
#' @param inputs Named list; parameter values for the query template
#' @param mode Character; "aggregate" or "assign"
#' @return For aggregate: disclosure-controlled data frame.
#'   For assign: invisible TRUE (data assigned server-side).
#' @keywords internal
.catalog_exec <- function(handle, query_id, inputs = list(),
                          mode = "aggregate") {
  queries <- .ql_load_queries()
  q <- queries[[query_id]]
  if (is.null(q)) {
    stop("Query '", query_id, "' not found in catalog.", call. = FALSE)
  }

  # Check allowlist
  allowlist <- .ql_load_allowlist()
  al <- allowlist[[query_id]]
  safety <- if (!is.null(al)) al else .ql_classify(q$sql, q$mode)
  safety_class <- safety$class %||% safety[["class"]]

  # Enforce safety

  if (safety_class == "BLOCKED") {
    stop("Query '", query_id, "' is blocked by safety classification.",
         call. = FALSE)
  }

  if (mode == "aggregate" && safety_class == "SAFE_ASSIGN") {
    stop("Query '", query_id, "' is only safe for assign mode, ",
         "not aggregate.", call. = FALSE)
  }

  # Render SQL with schema placeholders and user inputs
  sql <- q$sql

  # Substitute standard schema placeholders
  schema_params <- list(
    cdm = handle$cdm_schema %||% "public",
    vocab = handle$vocab_schema %||% handle$cdm_schema %||% "public",
    results = handle$results_schema %||% handle$cdm_schema %||% "public"
  )

  # Add schema qualification patterns (@cdm.table -> schema.table)
  for (schema_name in names(schema_params)) {
    schema_val <- schema_params[[schema_name]]
    if (!is.null(schema_val) && nchar(schema_val) > 0) {
      sql <- gsub(paste0("@", schema_name, "\\."),
                   paste0(schema_val, "."),
                   sql, fixed = FALSE)
    }
  }

  # Build effective inputs: user-provided values + defaults from template
  effective_inputs <- inputs
  if (!is.null(q$inputs) && is.data.frame(q$inputs) && nrow(q$inputs) > 0) {
    param_col <- intersect(c("parameter", "param", "name"), names(q$inputs))
    example_col <- intersect(c("example", "default"), names(q$inputs))
    if (length(param_col) > 0 && length(example_col) > 0) {
      for (i in seq_len(nrow(q$inputs))) {
        pname <- q$inputs[[param_col[1]]][i]
        if (!is.null(pname) && nchar(pname) > 0 &&
            !(pname %in% names(effective_inputs))) {
          default_val <- q$inputs[[example_col[1]]][i]
          if (!is.null(default_val) && nchar(default_val) > 0) {
            effective_inputs[[pname]] <- default_val
          }
        }
      }
    }
  }

  # Substitute input parameters (user-provided + defaults)
  for (param_name in names(effective_inputs)) {
    param_val <- effective_inputs[[param_name]]
    # Validate input parameter
    .validateString(as.character(param_val))
    sql <- gsub(paste0("@", param_name),
                 as.character(param_val),
                 sql, fixed = TRUE)
  }

  # Translate to target dialect
  sql <- .sql_translate(sql, handle$target_dialect)

  # Execute
  if (mode == "assign") {
    result <- .executeQuery(handle, sql)
    return(result)
  }

  # Aggregate mode: execute + apply SDC
  result <- .executeQuery(handle, sql)

  if (nrow(result) == 0) return(result)

  # Apply disclosure controls
  settings <- .omopDisclosureSettings()

  # Determine sensitive fields
  sensitive <- q$sensitive_fields
  if (length(sensitive) == 0) {
    # Auto-detect from classifier
    cl <- .ql_classify(q$sql, q$mode)
    sensitive <- cl$sensitive_fields_detected
  }

  # Suppress small counts in sensitive columns
  for (field in sensitive) {
    field_lower <- tolower(field)
    matching_col <- intersect(field_lower, tolower(names(result)))
    if (length(matching_col) > 0) {
      col_idx <- which(tolower(names(result)) == matching_col[1])
      col_name <- names(result)[col_idx[1]]
      mask <- !is.na(result[[col_name]]) &
              result[[col_name]] < settings$nfilter_tab
      result[[col_name]][mask] <- NA_real_
    }
  }

  # Cap rows to prevent long-tail disclosure
  max_rows <- 5000L
  if (nrow(result) > max_rows) {
    result <- result[seq_len(max_rows), , drop = FALSE]
    attr(result, "truncated") <- TRUE
    attr(result, "total_rows") <- nrow(result)
  }

  result
}

# --- SDC Helpers -------------------------------------------------------------

#' Suppress counts below threshold in specified columns
#'
#' @param df Data frame
#' @param sensitive_cols Character vector; column names to suppress
#' @param threshold Numeric; minimum count threshold
#' @return Data frame with small values replaced by NA
#' @keywords internal
.catalog_suppress_sensitive <- function(df, sensitive_cols, threshold = NULL) {
  if (is.null(threshold)) {
    threshold <- .omopDisclosureSettings()$nfilter_tab
  }

  for (col in sensitive_cols) {
    if (col %in% names(df)) {
      mask <- !is.na(df[[col]]) & is.numeric(df[[col]]) &
              df[[col]] < threshold
      df[[col]][mask] <- NA_real_
    }
  }
  df
}
