# Closed legacy physical-result identity contracts. Upstream versions are
# unknown for historical inline layouts; never infer a package version from a
# table name. No lookup adapter or live-analysis fallback is implemented here.
.ohdsiIdentityManifest <- function() {
  list(
    cohort_count = c("cohort_id", "database_id"),
    index_event_breakdown = c("cohort_id", "concept_id", "domain_field", "database_id"),
    visit_context = c("cohort_id", "visit_concept_id", "database_id"),
    time_series = c("cohort_id", "calendar_year", "calendar_month", "series_type", "database_id"),
    incidence_summary = c("target_cohort_definition_id", "outcome_id"),
    c_cohort_counts = c("cohort_id", "setting_id", "database_id"),
    c_dechallenge_rechallenge = c("cohort_id", "dechallenge_stop_interval", "dechallenge_evaluation_window", "database_id"),
    cm_result = c("analysis_id", "target_id", "comparator_id", "outcome_id"),
    sccs_result = c("analysis_id", "exposures_outcome_set_id", "covariate_id"),
    plp_performances = c("model_design_id", "development_database_id", "validation_database_id")
  )
}

.ohdsiIdentitySchema <- function(resolved, actual_cols) {
  keys <- .ohdsiIdentityManifest()[[resolved$canonical_table]]
  if (!isTRUE(resolved$strict)) return(character(0))
  if (identical(resolved$canonical_table, "cm_result") &&
      "target_comparator_id" %in% actual_cols) {
    stop("unsupported_results_schema: CohortMethod lookup identity layout is recognized ",
         "but not supported; requires a versioned cm_target_comparator adapter. ",
         "Use a listed supported inline identity layout or the separately documented ",
         "live analysis.", call. = FALSE)
  }
  if (is.null(keys) || length(setdiff(keys, actual_cols))) {
    stop("unsupported_results_schema: mandatory physical-result identity columns are missing; ",
         "use the documented inline identity layout. Optional-column completion ",
         "cannot supply analysis, cohort, arm, outcome or reference identities.", call. = FALSE)
  }
  keys
}

.ohdsiTextIdentityKeys <- function() {
  c("database_id", "development_database_id", "validation_database_id",
    "domain_field", "series_type")
}

# Exact signed BIGINT decimal spelling, checked without a double conversion.
# Leading zeros, plus signs, exponents and whitespace are not wire identities.
.ohdsiExactIntegerIdentity <- function(x) {
  if (anyNA(x)) return(FALSE)
  if (inherits(x, "integer64")) x <- as.character(x)
  if (is.object(x)) return(FALSE)
  if (is.integer(x)) return(TRUE)
  if (is.double(x)) {
    return(all(is.finite(x) & x == trunc(x) & abs(x) <= 9007199254740991))
  }
  if (!is.character(x) || any(!grepl("^(0|[1-9][0-9]*|-[1-9][0-9]*)$", x))) {
    return(FALSE)
  }
  magnitude <- sub("^-", "", x)
  bound <- ifelse(startsWith(x, "-"), "9223372036854775808", "9223372036854775807")
  # Compare equal-length decimal strings digit by digit, independent of locale.
  all(vapply(seq_along(x), function(i) {
    n <- nchar(magnitude[[i]], type = "bytes")
    if (n != 19L) return(n < 19L)
    delta <- utf8ToInt(magnitude[[i]]) - utf8ToInt(bound[[i]])
    first <- which(delta != 0L)
    !length(first) || delta[[first[[1L]]]] < 0L
  }, logical(1)))
}

.ohdsiValidateResultIdentity <- function(result, keys, count_columns) {
  if (!length(keys)) return(invisible(NULL))
  bad <- !all(keys %in% names(result)) || any(vapply(result[keys], anyNA, logical(1)))
  if (!bad) {
    bad <- any(vapply(keys, function(key) {
      x <- result[[key]]
      if (key %in% .ohdsiTextIdentityKeys()) {
        !is.character(x) || is.object(x) || any(!nzchar(x))
      } else {
        !.ohdsiExactIntegerIdentity(x)
      }
    }, logical(1)))
  }
  if (bad) {
    stop("results_schema_integrity: required row identities are missing or invalid; ",
         "use a complete supported physical export.", call. = FALSE)
  }
  # No negative-count sentinel has been established for the historical inline
  # contracts. Refuse unknown encodings instead of treating them as a count/zero.
  if (any(vapply(result[intersect(count_columns, names(result))], function(x) {
    is.numeric(x) && any(x < 0, na.rm = TRUE)
  }, logical(1)))) {
    stop("unsupported_results_suppression: negative count encoding is not contracted; ",
         "use a supported export with explicit missing counts.", call. = FALSE)
  }
  invisible(NULL)
}

# Metadata type validation is independent of rows, including empty tables.
.ohdsiIdentityTypes <- function(handle, physical, keys) {
  if (!length(keys)) return(invisible(NULL))
  bp <- physical$blueprint
  idx <- match(tolower(physical$table_name), tolower(names(bp$columns)))
  info <- if (!is.na(idx)) bp$columns[[idx]] else NULL
  if (is.null(info) || !all(c("column_name", "data_type") %in% names(info))) {
    info <- .listColumnsRaw(handle, physical$table_name, .effectiveResultsSchema(handle))
  }
  kinds <- tolower(as.character(info$data_type[match(keys, tolower(info$column_name))]))
  text_keys <- .ohdsiTextIdentityKeys()
  for (i in seq_along(keys)) {
    valid <- if (keys[[i]] %in% text_keys) {
      grepl("^(text|varchar|char|character|nvarchar|nchar|string)(\\(| |$)", kinds[[i]])
    } else {
      grepl("^(int|integer|bigint|smallint|tinyint|int2|int4|int8)(\\(| |$)", kinds[[i]])
    }
    if (is.na(valid) || !valid) {
      stop("unsupported_results_schema: required identity column has an unsupported physical type; ",
           "use exact integer identities and declared text reference keys.", call. = FALSE)
    }
  }
  invisible(NULL)
}

# Structural contracts describe the reader's historical inline layouts, not a
# claimed upstream release. Fingerprints contain schema metadata only. Optional
# columns are a closed set and their absence does not change the layout ID.
.ohdsiResultSchemaManifest <- function() {
  registry <- .ohdsi_tool_registry()
  identities <- .ohdsiIdentityManifest()
  # Known legacy scalar measures excluded from the public release contract.
  # These fields do not add dimensions or provide alternative person support.
  nonreleased <- list(
    incidence_summary = c(person_days = "number", incidence_proportion_p100p = "number",
      incidence_rate_p100py = "number"),
    sccs_result = c(covariate_eras = "number", covariate_days = "number",
      outcome_observation_periods = "number"))
  out <- list()
  for (family in names(registry)) for (table in registry[[family]]$table_names) {
    contract <- registry[[family]]$contracts[[table]]
    keys <- identities[[table]] %||% character()
    required <- unique(c(keys, contract$count_columns, contract$person_columns))
    types <- stats::setNames(ifelse(required %in% .ohdsiTextIdentityKeys(), "text",
      ifelse(required %in% keys, "integer", "number")), required)
    out[[table]] <- list(
      family = family, layout_id = paste0("legacy-inline-", table, "-1"),
      upstream_version = "unknown", contract_version = "physical-results-1",
      table = table, prefix_patterns = registry[[family]]$prefix_patterns,
      required_columns = types, identity = keys,
      relationships = list(unique_row = keys, reference_layout = "inline"),
      optional_columns = contract$optional_types,
      nonreleased_columns = nonreleased[[table]] %||% character(),
      private_basis = .ohdsiStatisticBasisColumns(contract$statistic_basis),
      suppression = "missing-only; negative counts unsupported",
      statistic_basis = contract$statistic_basis, person_basis = contract$person_columns,
      public_release = contract$release,
      status = if (identical(contract$release, "public")) "supported" else "admin_only")
  }
  out$cm_lookup <- list(family = "cohort_method", layout_id = "cm-target-comparator-lookup-1",
    upstream_version = "6.0.3", contract_version = "physical-results-1",
    table = "cm_result", reference_table = "cm_target_comparator",
    identity = c("analysis_id", "target_comparator_id", "outcome_id"),
    relationships = list(reference = "cm_target_comparator.target_comparator_id"),
    public_release = "none", status = "recognized_unsupported",
    evidence = "Pinned upstream schema only; physical export acceptance pending")
  out
}

.ohdsiSchemaType <- function(x) {
  x <- tolower(as.character(x))
  ifelse(grepl("^(int|integer|bigint|smallint|tinyint|int2|int4|int8)(\\(| |$)", x), "integer",
    ifelse(grepl("^(text|varchar|char|character|nvarchar|nchar|string)(\\(| |$)", x), "text",
      ifelse(grepl("^(real|double|float|numeric|decimal)(\\(| |$)", x), "number", "unknown")))
}

.ohdsiInspectResultSchema <- function(handle, table_name, tool_id = NULL) {
  registry <- .ohdsi_tool_registry()
  tool_id <- tool_id %||% .ohdsi_table_to_tool(table_name)
  canonical <- if (!is.null(tool_id)) .ohdsi_table_matches_tool(table_name, registry[[tool_id]]) else character()
  result <- list(status = "unknown", layout_id = "unknown", upstream_version = "unknown",
                 contract_version = "physical-results-1", structural_fingerprint = NA_character_)
  if (length(canonical) != 1L) return(result)
  info <- tryCatch(.listColumnsRaw(handle, table_name, .effectiveResultsSchema(handle)),
                   error = function(e) NULL)
  if (is.null(info) || !all(c("column_name", "data_type") %in% names(info))) return(result)
  columns <- tolower(as.character(info$column_name))
  if (anyNA(columns) || anyDuplicated(columns)) return(result)
  contract <- .ohdsiResultSchemaManifest()[[canonical]]
  # Both declared SQL types and contracted keys are included; row values never
  # enter this fingerprint. Column order is immaterial.
  metadata <- list(table = canonical,
    columns = stats::setNames(as.list(tolower(as.character(info$data_type))), columns),
    keys = as.list(sort(contract$identity)))
  result$structural_fingerprint <- unclass(as.character(openssl::sha256(charToRaw(.jcsCanonical(metadata)))))
  if (identical(canonical, "cm_result") && "target_comparator_id" %in% columns) {
    result$status <- "recognized_unsupported"
    result$layout_id <- "cm-target-comparator-lookup-1"
    # Recognizing a structural variant does not prove which upstream release
    # produced this database; the pinned schema version is not observed here.
    return(result)
  }
  if (identical(contract$status, "admin_only")) {
    result$status <- "admin_only"
    result$layout_id <- contract$layout_id
    return(result)
  }
  # A known subset is not a known layout. An extra dimension can change the
  # identity/meaning of every row, even when callers project that column away.
  # Private statistic support columns are explicitly contracted numeric fields.
  allowed <- unique(c(names(contract$required_columns),
    names(contract$optional_columns), names(contract$nonreleased_columns), contract$private_basis))
  if (length(setdiff(columns, allowed))) return(result)
  types <- stats::setNames(.ohdsiSchemaType(info$data_type), columns)
  basis <- intersect(c(contract$private_basis, names(contract$nonreleased_columns)), columns)
  if (length(basis) && any(!types[basis] %in% c("integer", "number"))) return(result)
  required <- contract$required_columns
  if (length(setdiff(names(required), columns)) ||
      !all(ifelse(required == "number", types[names(required)] %in% c("integer", "number"),
                  types[names(required)] == required))) return(result)
  optional <- intersect(names(contract$optional_columns), columns)
  if (length(optional)) {
    expected <- ifelse(contract$optional_columns[optional] == "character", "text", "number")
    observed <- types[optional]
    if (any(ifelse(expected == "number", !observed %in% c("number", "integer"), observed != expected))) return(result)
  }
  result$status <- "supported"
  result$layout_id <- contract$layout_id
  result
}

# Validate the complete key relation before caller filtering, projection and
# limits. A duplicate outside the requested slice must not make a seemingly
# valid export. Only a constant reaches R; no keys/counts enter a diagnostic.
.ohdsiValidateUniqueResultKeys <- function(handle, physical, keys) {
  if (!length(keys)) return(invisible(NULL))
  invalid <- paste0(keys, " IS NULL")
  text_keys <- intersect(keys, .ohdsiTextIdentityKeys())
  invalid <- c(invalid, paste0(text_keys, " = ''")[seq_along(text_keys)])
  if (identical(handle$target_dialect, "sqlite")) {
    kinds <- ifelse(keys %in% text_keys, "text", "integer")
    invalid <- c(invalid, paste0("TYPEOF(", keys, ") <> '", kinds, "'"))
  }
  checks <- c(paste0("SELECT TOP 1 1 AS invalid FROM ", physical$qualified_name,
      " WHERE ", paste(invalid, collapse = " OR ")),
    paste0("SELECT TOP 1 1 AS invalid FROM ", physical$qualified_name,
      " GROUP BY ", paste(keys, collapse = ", "), " HAVING COUNT(*) > 1"))
  bad <- tryCatch(any(vapply(checks, function(sql) {
    nrow(.executeQuery(handle, .sql_translate(sql, handle$target_dialect))) > 0L
  }, logical(1))), error = function(e) TRUE)
  if (bad) stop("results_schema_integrity: required row identities are missing or invalid; ",
                "use a complete supported physical export.", call. = FALSE)
  invisible(NULL)
}
