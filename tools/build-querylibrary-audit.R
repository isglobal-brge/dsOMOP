#!/usr/bin/env Rscript

# Build the non-executable audit inventory for the pinned OHDSI QueryLibrary
# snapshot. Usage, from the dsOMOP source root:
#
#   Rscript tools/build-querylibrary-audit.R /path/to/QueryLibrary \
#     inst/queries/upstream_querylibrary_audit.json

EXPECTED_COMMIT <- "df8a21074b08519e581ca1afb7510468538117a4"
EXPECTED_QUERY_COUNT <- 201L
EXPECTED_MANIFEST_SHA256 <-
  "07b718badf25c485a7ac12f035e6f158b28d034ea6fa176598d6f52229c9ac5f"
AUDITED_AT <- "2026-08-01"
QUERY_ROOT_REL <- "inst/shinyApps/QueryLibrary/queries"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 2L) {
  stop(
    "Usage: build-querylibrary-audit.R QUERYLIBRARY_REPO [OUTPUT_JSON]",
    call. = FALSE
  )
}

if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("openssl", quietly = TRUE)) {
  stop("jsonlite and openssl are required", call. = FALSE)
}

repo <- normalizePath(args[[1L]], mustWork = TRUE)
output <- if (length(args) == 2L) {
  args[[2L]]
} else {
  file.path("inst", "queries", "upstream_querylibrary_audit.json")
}
query_root <- file.path(repo, QUERY_ROOT_REL)
if (!dir.exists(query_root)) {
  stop("QueryLibrary query root was not found: ", query_root, call. = FALSE)
}

git_commit <- suppressWarnings(system2(
  "git",
  c("-C", shQuote(repo), "rev-parse", "HEAD"),
  stdout = TRUE,
  stderr = TRUE
))
if (!identical(length(git_commit), 1L) ||
    !identical(unname(git_commit), EXPECTED_COMMIT)) {
  stop(
    "Expected QueryLibrary commit ", EXPECTED_COMMIT,
    "; found ", paste(git_commit, collapse = " "),
    call. = FALSE
  )
}

read_raw <- function(path) {
  size <- file.info(path)$size
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, what = "raw", n = size)
}

sha256_file <- function(path) {
  paste0(openssl::sha256(read_raw(path)))
}

extract_sql <- function(lines, path) {
  start <- grep("^```sql\\s*$", lines, ignore.case = TRUE)
  if (length(start) != 1L) {
    stop("Expected one SQL fence in ", path, call. = FALSE)
  }
  finish <- which(seq_along(lines) > start[[1L]] & grepl("^```\\s*$", lines))
  if (!length(finish)) {
    stop("Unclosed SQL fence in ", path, call. = FALSE)
  }
  paste(lines[(start[[1L]] + 1L):(finish[[1L]] - 1L)], collapse = "\n")
}

extract_title <- function(lines, path) {
  heading <- grep("^#\\s+", lines)
  if (!length(heading)) {
    stop("Missing title in ", path, call. = FALSE)
  }
  sub("^#\\s+", "", lines[[heading[[1L]]]])
}

extract_upstream_id <- function(title, path) {
  match <- regexec("^([A-Za-z0-9]+)\\s*:", title, perl = TRUE)
  parts <- regmatches(title, match)[[1L]]
  if (length(parts) < 2L) {
    stop("Cannot derive the published query ID from the title in ", path,
         call. = FALSE)
  }
  parts[[2L]]
}

extract_output_fields <- function(lines, path) {
  start <- grep("^##\\s+Output\\s*$", lines, ignore.case = TRUE)
  if (length(start) != 1L) {
    stop("Expected one Output section in ", path, call. = FALSE)
  }
  finish <- which(
    seq_along(lines) > start[[1L]] & grepl("^##\\s+", lines)
  )
  finish <- if (length(finish)) finish[[1L]] else length(lines) + 1L
  rows <- lines[(start[[1L]] + 1L):(finish - 1L)]
  rows <- rows[grepl("^\\s*\\|", rows)]
  fields <- trimws(vapply(strsplit(rows, "\\|"), function(parts) {
    if (length(parts) >= 2L) parts[[2L]] else ""
  }, character(1L)))
  fields <- fields[
    nzchar(fields) &
      !tolower(fields) %in% c("field", "---") &
      !grepl("^:?-+:?$", fields)
  ]
  if (!length(fields)) {
    stop("No documented output fields in ", path, call. = FALSE)
  }
  unname(fields)
}

extract_dependencies <- function(sql) {
  hits <- regmatches(
    sql,
    gregexpr(
      "@(?:cdm|vocab)\\.[A-Za-z0-9_]+",
      sql,
      ignore.case = TRUE,
      perl = TRUE
    )
  )[[1L]]
  if (identical(hits, "")) hits <- character()
  sort(unique(tolower(hits)), method = "radix")
}

as_json_array <- function(x) unname(as.list(x))

vocabulary_tables <- c(
  "concept", "concept_ancestor", "concept_relationship",
  "concept_synonym", "relationship", "vocabulary"
)
person_level_tables <- c(
  "condition_era", "condition_occurrence", "cost", "death", "drug_era",
  "drug_exposure", "measurement", "observation_period", "payer_plan_period",
  "person", "procedure_occurrence", "visit_occurrence"
)

# These exceptions come from the documented output grain and selectors, not
# from a target class count. DER02 selects one unique drug-era record even
# though its final SELECT is an aggregate; CO06 and DER04 disclose one person.
single_subject_ids <- c("CO06", "DER04")
single_event_ids <- "DER02"

# Each of these returns the exact numeric support value plus its frequency.
# They require public clipping/binning in addition to small-cell handling.
numeric_histogram_ids <- c(
  "CO01", "CO07", "CO25", "DEX15", "DEX24", "DEX35", "DEX37",
  "OP02", "PP01"
)

# DER23 documents date statistics with generic *_value output names, so output
# names alone are insufficient to detect its exact-date semantics.
exact_date_ids <- c(
  "CE11", "CE12", "CO02", "CO12", "CO13", "DER10", "DER11", "DER23",
  "DEX26", "DEX27", "DEX41", "OP11", "OP13"
)

# Their documented output names hide that the result is a raw mean or maximum.
implicit_statistic_ids <- c("DER07", "DEX07", "DEX08", "OP05", "OP06")

summary_field_pattern <- paste0(
  "(^|_)(min|minimum|max|maximum|avg|average|mean|stdev|stddev|std_dev|",
  "percentile|median|proportion|percent|percentage|percentages|perc|pct)",
  "(_|$)"
)

classify_query <- function(id, sql, dependencies, output_fields) {
  tables <- sub("^@(cdm|vocab)\\.", "", dependencies)
  fields <- tolower(output_fields)

  vocabulary_only <- length(tables) > 0L &&
    all(tables %in% vocabulary_tables)
  reference_metadata <- identical(id, "CS01")
  direct_person_output <- any(fields == "person_id")
  single_subject <- id %in% single_subject_ids
  single_event <- id %in% single_event_ids
  uncontrolled_label <- any(fields %in% c("stop_reason", "plan_source_value"))
  fine_geography <- any(fields == "zip")
  exact_date <- any(grepl("(^|_)date($|_)", fields)) || id %in% exact_date_ids
  exact_demographic <- any(fields == "age") ||
    any(grepl("year_of_birth|day_of_birth", fields))
  summary_statistic <- any(grepl(summary_field_pattern, fields, perl = TRUE)) ||
    any(grepl("adherence", fields))
  numeric_histogram <- id %in% numeric_histogram_ids
  implicit_statistic <- id %in% implicit_statistic_ids

  triage_class <- if (vocabulary_only || reference_metadata) {
    "vocabulary_reference_metadata"
  } else if (direct_person_output || single_subject || single_event) {
    "patient_rows_assignment_only"
  } else if (uncontrolled_label || fine_geography) {
    "unsafe_as_written"
  } else if (exact_date || exact_demographic || summary_statistic ||
             numeric_histogram || implicit_statistic) {
    "statistical_needs_redesign"
  } else {
    "rewritable_patient_aggregate"
  }

  list(
    triage_class = triage_class,
    vocabulary_only = vocabulary_only,
    reference_metadata = reference_metadata,
    direct_person_output = direct_person_output,
    single_subject = single_subject,
    single_event = single_event,
    uncontrolled_label = uncontrolled_label,
    fine_geography = fine_geography,
    exact_date = exact_date,
    exact_demographic = exact_demographic,
    summary_statistic = summary_statistic,
    numeric_histogram = numeric_histogram,
    has_patient_data = any(tables %in% person_level_tables),
    has_event_data = any(tables %in% setdiff(person_level_tables, "person"))
  )
}

output_type <- function(id, sql, output_fields, flags) {
  fields <- tolower(output_fields)
  has_aggregate <- grepl(
    "\\b(count|count_big|sum|min|max|avg|stdev)\\s*\\(",
    tolower(sql),
    perl = TRUE
  )
  if (identical(flags$triage_class, "vocabulary_reference_metadata")) {
    if (flags$reference_metadata) return("non_patient_metadata_count")
    return(if (has_aggregate) "vocabulary_summary" else "vocabulary_rows")
  }
  if (identical(flags$triage_class, "patient_rows_assignment_only")) {
    if (flags$single_event) return("single_event_result")
    if (flags$single_subject) return("single_patient_result")
    return("patient_or_event_rows")
  }
  if (identical(flags$triage_class, "unsafe_as_written")) {
    return(if (flags$fine_geography) {
      "fine_geography_count"
    } else {
      "source_value_frequency"
    })
  }
  if (identical(flags$triage_class, "statistical_needs_redesign")) {
    if (flags$exact_date) return("exact_date_statistic")
    if (flags$exact_demographic) return("exact_demographic_statistic")
    if (flags$numeric_histogram) return("numeric_histogram")
    if (any(grepl(
      paste0(
        "(^|_)(proportion|percent|percentage|percentages|perc|adherence)",
        "(_|$)"
      ),
      fields,
      perl = TRUE
    ))) return("rate_or_proportion")
    return("continuous_summary")
  }
  if (identical(id, "PE07")) return("coarse_geography_count")
  if (any(grepl("month|year|season", fields))) return("time_bucket_count")
  if (length(fields) == 1L && !grepl("\\bgroup\\s+by\\b", tolower(sql))) {
    return("scalar_count")
  }
  "categorical_count"
}

risk_signals <- function(id, sql, output_fields, flags) {
  fields <- tolower(output_fields)
  sql_lower <- tolower(sql)
  signals <- character()

  if (flags$vocabulary_only) signals <- c(signals, "vocabulary_only")
  if (flags$reference_metadata) {
    signals <- c(signals, "non_patient_reference_metadata")
  }
  if (flags$direct_person_output) {
    signals <- c(signals, "person_identifier_output", "row_level_clinical_data")
  }
  if (any(grepl("(^|_)drug_exposure_id($|_)", fields))) {
    signals <- c(signals, "event_identifier_output")
  }
  if (flags$single_subject) signals <- c(signals, "single_person_selector")
  if (flags$single_event) signals <- c(signals, "single_event_selector")
  if (flags$uncontrolled_label) signals <- c(signals, "uncontrolled_source_value")
  if (flags$fine_geography) signals <- c(signals, "fine_geography")
  if (flags$exact_date) signals <- c(signals, "exact_date_output")
  if (flags$exact_demographic) {
    signals <- c(signals, "exact_age_or_birth_component")
  }
  if (!flags$vocabulary_only && flags$summary_statistic) {
    signals <- c(signals, "raw_extreme_quantile_mean_or_rate")
  }
  if (flags$numeric_histogram) signals <- c(signals, "unbounded_numeric_support")

  patient_aggregate <- flags$has_patient_data &&
    flags$triage_class %in% c(
      "rewritable_patient_aggregate", "statistical_needs_redesign",
      "unsafe_as_written"
    )
  if (patient_aggregate) signals <- c(signals, "small_cohort_or_cell")
  if (patient_aggregate && flags$has_event_data &&
      !grepl(
        "count\\s*\\(\\s*distinct\\s+[a-z0-9_.]*person_id",
        sql_lower,
        perl = TRUE
      )) {
    signals <- c(signals, "missing_distinct_person_support")
  }
  if (patient_aggregate && flags$has_event_data &&
      grepl("count\\s*\\(\\s*(\\*|1|[a-z0-9_.]+)\\s*\\)", sql_lower)) {
    signals <- c(signals, "repeated_record_contribution")
  }
  unique(signals)
}

portability_signals <- function(sql, dependencies, id, path_id) {
  sql_lower <- tolower(sql)
  tables <- sub("^@(cdm|vocab)\\.", "", dependencies)
  schema <- sub("^@([^.]*)\\..*$", "\\1", dependencies)
  mismatch <- any(schema == "cdm" & tables %in% vocabulary_tables) ||
    any(schema == "vocab" & !tables %in% vocabulary_tables)
  signals <- character()
  if (!identical(id, path_id)) signals <- c(signals, "id_path_mismatch")
  if (mismatch) signals <- c(signals, "schema_placeholder_mismatch")
  if (grepl(
    "\\b(dateadd|datediff|datefromparts|isnull|count_big|convert|top)\\b",
    sql_lower,
    perl = TRUE
  )) signals <- c(signals, "sql_server_specific_syntax")
  if (grepl("\\bselect\\s+\\*", sql_lower, perl = TRUE)) {
    signals <- c(signals, "select_star")
  }
  signals
}

triage_reason <- function(id, flags) {
  if (flags$reference_metadata) {
    return(paste(
      "This is a non-patient care-site metadata count; keep public categories,",
      "small outputs and institutional policy controls."
    ))
  }
  if (identical(flags$triage_class, "vocabulary_reference_metadata")) {
    return(paste(
      "Only OMOP vocabulary tables are detected; no patient-level CDM table",
      "is read."
    ))
  }
  if (flags$single_event) {
    return(paste(
      "A unique drug_era_id selects one person's episode and returns its cost;",
      "direct client return is not safe."
    ))
  }
  if (flags$single_subject) {
    return(paste(
      "The query selects one person and returns that person's clinical result;",
      "only protected internal assignment is conceivable."
    ))
  }
  if (identical(flags$triage_class, "patient_rows_assignment_only")) {
    return(paste(
      "The documented output contains a person identifier and patient/event",
      "rows; it must never be returned directly."
    ))
  }
  if (flags$uncontrolled_label) {
    return(paste(
      "The output groups by an uncontrolled OMOP source/free-text value;",
      "protecting counts does not sanitize the labels."
    ))
  }
  if (flags$fine_geography) {
    return(paste(
      "Exact ZIP combined with counts is a high-risk geographic release and",
      "is blocked without a separate geography policy."
    ))
  }
  if (flags$exact_date) {
    return(paste(
      "Exact date extrema/quantiles or dates are returned; replace them with",
      "public temporal bins or a protected temporal mechanism."
    ))
  }
  if (flags$exact_demographic) {
    return(paste(
      "Exact age or birth components are returned; common public age/date",
      "bands are required before release."
    ))
  }
  if (identical(flags$triage_class, "statistical_needs_redesign")) {
    return(paste(
      "The statistic lacks public clipping/binning and bounded per-person",
      "contributions; raw extrema, quantiles, means or rates are not releasable."
    ))
  }
  paste(
    "Rewrite around distinct-person support, public grouping values, small-cell",
    "controls and bounded per-person contributions."
  )
}

dp_assessment <- function(id, type, flags) {
  candidate <- identical(flags$triage_class, "statistical_needs_redesign") ||
    (identical(flags$triage_class, "rewritable_patient_aggregate") &&
       flags$has_patient_data) ||
    identical(id, "PE08")
  if (!candidate) return(list(candidate = FALSE, reason = NA_character_))

  reason <- switch(
    type,
    exact_date_statistic = paste(
      "Replace exact dates with public bins and use a bounded sticky-DP",
      "histogram/quantile mechanism with composition accounting."
    ),
    exact_demographic_statistic = paste(
      "Use common public age/date bands, one bounded contribution per person",
      "and sticky-DP counts with composition accounting."
    ),
    numeric_histogram = paste(
      "Apply public clipping and bins plus a per-person contribution cap, then",
      "release a sticky-DP histogram with composition accounting."
    ),
    continuous_summary = paste(
      "Use public clipping and contribution bounds; derive the result from",
      "sticky-DP sums/counts or a DP histogram, never noisy raw extrema."
    ),
    rate_or_proportion = paste(
      "Release bounded sticky-DP numerator and denominator counts under one",
      "ledger, then derive the rate by post-processing."
    ),
    fine_geography_count = paste(
      "A candidate only with a complete public ZIP domain, one residence per",
      "person, sticky-DP counts and a site-approved geography policy."
    ),
    paste(
      "After enforcing public categories and bounded per-person contributions,",
      "sticky-DP counts are possible with composition accounting."
    )
  )
  list(candidate = TRUE, reason = reason)
}

files <- list.files(
  query_root,
  pattern = "\\.md$",
  recursive = TRUE,
  full.names = TRUE
)
relative_paths <- substring(files, nchar(query_root) + 2L)
order_index <- order(relative_paths, method = "radix")
files <- files[order_index]
relative_paths <- relative_paths[order_index]

if (!identical(length(files), EXPECTED_QUERY_COUNT)) {
  stop(
    "Expected ", EXPECTED_QUERY_COUNT, " Markdown queries; found ",
    length(files),
    call. = FALSE
  )
}

file_hashes <- vapply(files, sha256_file, character(1L))
manifest <- paste0(file_hashes, "  ", relative_paths, "\n", collapse = "")
manifest_sha256 <- paste0(openssl::sha256(charToRaw(manifest)))
if (!identical(manifest_sha256, EXPECTED_MANIFEST_SHA256)) {
  stop(
    "Pinned corpus digest mismatch: expected ", EXPECTED_MANIFEST_SHA256,
    "; found ", manifest_sha256,
    call. = FALSE
  )
}

queries <- Map(function(path, relative_path, hash) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  path_id <- sub("\\.md$", "", basename(relative_path))
  title <- extract_title(lines, relative_path)
  id <- extract_upstream_id(title, relative_path)
  sql <- extract_sql(lines, relative_path)
  dependencies <- extract_dependencies(sql)
  fields <- extract_output_fields(lines, relative_path)
  flags <- classify_query(id, sql, dependencies, fields)
  type <- output_type(id, sql, fields, flags)
  dp <- dp_assessment(id, type, flags)

  list(
    upstream_id = id,
    path_id = path_id,
    path = relative_path,
    sha256 = hash,
    title = title,
    dependencies = as_json_array(dependencies),
    tables = as_json_array(sort(unique(sub(
      "^@(cdm|vocab)\\.", "", dependencies
    )), method = "radix")),
    output_fields = as_json_array(fields),
    output_type = type,
    risk_signals = as_json_array(risk_signals(id, sql, fields, flags)),
    portability_signals = as_json_array(portability_signals(
      sql, dependencies, id, path_id
    )),
    triage_class = flags$triage_class,
    triage_reason = triage_reason(id, flags),
    dp_candidate = dp$candidate,
    dp_justification = dp$reason
  )
}, files, relative_paths, file_hashes)

ids <- vapply(queries, `[[`, character(1L), "upstream_id")
if (anyDuplicated(tolower(ids))) {
  stop("Upstream IDs are not unique (case-insensitive)", call. = FALSE)
}

class_levels <- c(
  "vocabulary_reference_metadata", "rewritable_patient_aggregate",
  "statistical_needs_redesign", "patient_rows_assignment_only",
  "unsafe_as_written"
)
classes <- vapply(queries, `[[`, character(1L), "triage_class")
triage_counts <- table(factor(classes, levels = class_levels))
domains <- table(dirname(relative_paths))
types <- table(vapply(queries, `[[`, character(1L), "output_type"))
dp_values <- vapply(queries, `[[`, logical(1L), "dp_candidate")

named_integer_list <- function(x) {
  stats::setNames(as.list(as.integer(x)), names(x))
}

inventory <- list(
  schema_version = 2L,
  source = "https://github.com/OHDSI/QueryLibrary",
  commit = EXPECTED_COMMIT,
  audited_at = AUDITED_AT,
  query_root = QUERY_ROOT_REL,
  query_count = length(queries),
  domain_count = length(domains),
  manifest_sha256 = manifest_sha256,
  manifest_algorithm = paste(
    "For every *.md file below query_root, sort relative POSIX paths bytewise;",
    "write lowercase SHA-256, two spaces, relative path, and LF; SHA-256 the",
    "resulting 201-line byte stream."
  ),
  classification_method = list(
    precedence = as_json_array(c(
      "vocabulary_reference_metadata", "patient_rows_assignment_only",
      "unsafe_as_written", "statistical_needs_redesign",
      "rewritable_patient_aggregate"
    )),
    rules = as_json_array(c(
      paste(
        "Vocabulary/reference metadata is detected from dependencies whose",
        "table names are all OMOP vocabulary tables, regardless of folder or",
        "an upstream @cdm/@vocab placeholder mismatch; CS01 is the sole",
        "non-patient structural-metadata count."
      ),
      paste(
        "Assignment-only covers documented person_id outputs plus CO06 and",
        "DER04 single-person selectors and DER02's unique drug-era selector."
      ),
      paste(
        "Unsafe-as-written covers uncontrolled stop_reason/plan_source_value",
        "labels and exact ZIP output; noise on counts alone does not sanitize",
        "those labels or geography."
      ),
      paste(
        "Statistical redesign covers exact dates/ages/birth components, raw",
        "extrema/means/quantiles/rates, and raw numeric histograms requiring",
        "public clipping or bins and contribution bounds."
      ),
      paste(
        "Remaining patient aggregates require distinct-person support, public",
        "groups, small-cell controls and bounded per-person contributions."
      )
    )),
    note = paste(
      "This static audit is evidence and backlog metadata only. A class or DP",
      "candidate flag does not authorize execution and is not read by the",
      "runtime query allowlist."
    )
  ),
  runtime_status = list(
    authorizes_execution = FALSE,
    runtime_allowlist = "inst/queries/query_allowlist.json",
    queries_enabled_by_this_inventory = 0L
  ),
  dp_semantics = list(
    candidate_means = paste(
      "A redesigned aggregate can have finite sensitivity after the stated",
      "public bins/clips and per-person contribution bounds, using sticky",
      "noise and durable composition accounting."
    ),
    candidate_does_not_mean = paste(
      "The upstream SQL is safe as written, that nfilter.noise is formal DP,",
      "or that the query is currently executable."
    )
  ),
  domains = named_integer_list(domains),
  triage = named_integer_list(triage_counts),
  output_types = named_integer_list(types),
  dp_candidates = list(
    candidate = sum(dp_values),
    not_candidate = sum(!dp_values)
  ),
  queries = unname(queries)
)

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
json <- jsonlite::toJSON(
  inventory,
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null",
  na = "null"
)
writeLines(json, output, useBytes = TRUE)

message(
  "Wrote ", length(queries), " audited queries to ", output,
  " (triage: ",
  paste(names(triage_counts), as.integer(triage_counts), collapse = ", "),
  ")"
)
