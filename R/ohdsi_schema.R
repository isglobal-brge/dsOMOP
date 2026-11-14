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
