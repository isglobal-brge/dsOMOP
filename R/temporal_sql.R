# SQL-first temporal covariate and person-period output helpers.

.temporalSqlInteger <- function(value, name) {
  number <- suppressWarnings(as.numeric(value))
  integer <- suppressWarnings(as.integer(value))
  if (length(value) != 1L || length(number) != 1L || is.na(number) ||
      !is.finite(number) || length(integer) != 1L || is.na(integer) ||
      number != integer) {
    stop(name, " must be one finite integer.", call. = FALSE)
  }
  integer
}

.normalizeTemporalSqlAnalyses <- function(analyses) {
  if (!is.character(analyses) || length(analyses) == 0L || anyNA(analyses)) {
    stop("analyses must be a non-empty subset of binary and count.",
         call. = FALSE)
  }
  analyses <- tolower(analyses)
  if (any(!analyses %in% c("binary", "count"))) {
    stop("analyses must be a non-empty subset of binary and count.",
         call. = FALSE)
  }
  unique(analyses)
}

.normalizeTemporalSqlConcepts <- function(handle, concept_filter,
                                          max_concepts) {
  if (is.null(concept_filter)) return(NULL)
  raw <- .resolveConceptSet(handle, concept_filter)
  number <- suppressWarnings(as.numeric(raw))
  integer <- suppressWarnings(as.integer(raw))
  if (length(raw) == 0L || anyNA(number) || any(!is.finite(number)) ||
      anyNA(integer) || any(number != integer) || any(integer < 0L)) {
    stop("concept_filter must contain finite non-negative integer concept IDs.",
         call. = FALSE)
  }
  concepts <- sort(unique(integer))
  if (length(concepts) > max_concepts) {
    stop("Temporal covariates exceed the server concept cap of ",
         max_concepts, ".", call. = FALSE)
  }
  concepts
}

.temporalSqlBigInteger <- function(expression, handle) {
  dialect <- tolower(handle$target_dialect %||% "")
  type <- switch(dialect,
    bigquery = "INT64",
    oracle = "NUMBER(19)",
    mysql = "SIGNED",
    "BIGINT"
  )
  paste0("CAST(", expression, " AS ", type, ")")
}

.temporalSqlFloorDivide <- function(expression, divisor, handle) {
  divisor <- .temporalSqlInteger(divisor, "temporal divisor")
  if (divisor <= 0L) {
    stop("temporal divisor must be greater than zero.", call. = FALSE)
  }
  .temporalSqlBigInteger(
    paste0("FLOOR((", expression, ") / ", divisor, ".0)"),
    handle
  )
}

.temporalSqlDateDiffDays <- function(handle, end_expression,
                                     start_expression) {
  # Start with the shared OHDSI-aligned helper, overriding only engines where
  # date subtraction is not a day-count expression.
  shared <- .omopDateDiffDays(handle, end_expression, start_expression)
  switch(tolower(handle$target_dialect %||% ""),
    mysql = paste0("DATEDIFF(", end_expression, ", ", start_expression, ")"),
    spark = paste0("DATEDIFF(", end_expression, ", ", start_expression, ")"),
    shared
  )
}

.temporalSqlConceptName <- function(concept_expression, analysis, handle) {
  dialect <- tolower(handle$target_dialect %||% "")
  character_expression <- switch(dialect,
    bigquery = paste0("CAST(", concept_expression, " AS STRING)"),
    spark = paste0("CAST(", concept_expression, " AS STRING)"),
    mysql = paste0("CAST(", concept_expression, " AS CHAR)"),
    oracle = paste0("CAST(", concept_expression, " AS VARCHAR2(64))"),
    paste0("CAST(", concept_expression, " AS VARCHAR(64))")
  )
  prefix <- .quoteLiteral("x", handle)
  suffix <- .quoteLiteral(paste0("_", analysis), handle)
  if (dialect %in% c("sql server")) {
    paste0(prefix, " + ", character_expression, " + ", suffix)
  } else if (dialect %in% c("bigquery", "spark", "mysql")) {
    paste0("CONCAT(", prefix, ", ", character_expression, ", ", suffix,
           ")")
  } else {
    paste0(prefix, " || ", character_expression, " || ", suffix)
  }
}

.temporalCovariateRefData <- function(concepts, analyses) {
  analysis_ids <- c(binary = 1L, count = 2L)
  rows <- lapply(concepts, function(concept_id) {
    label <- .standardizeName(as.character(concept_id))
    if (is.na(label) || !nzchar(label)) label <- paste0("concept_", concept_id)
    do.call(rbind, lapply(analyses, function(analysis) {
      analysis_id <- analysis_ids[[analysis]]
      data.frame(
        covariateId = as.numeric(concept_id) * 1000 + analysis_id,
        covariateName = paste0(label, "_", analysis),
        analysisId = analysis_id,
        conceptId = as.integer(concept_id),
        stringsAsFactors = FALSE
      )
    }))
  })
  if (length(rows) == 0L) {
    return(data.frame(
      covariateId = numeric(0), covariateName = character(0),
      analysisId = integer(0), conceptId = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

.temporalLiteralSelect <- function(value, alias, handle) {
  suffix <- if (identical(tolower(handle$target_dialect %||% ""), "oracle")) {
    " FROM DUAL"
  } else {
    ""
  }
  paste0("SELECT ", value, " AS ", alias, suffix)
}

# Produce a compact, portable integer grid without embedding one UNION branch
# per time bin. Four ten-row digit relations cover the default 10,000-bin cap.
.temporalNumberGridSql <- function(n, handle) {
  if (length(n) != 1L || is.na(n) || !is.finite(n) || n < 1 || n != floor(n)) {
    stop("Temporal number-grid size must be one positive integer.",
         call. = FALSE)
  }
  digits <- max(1L, nchar(as.character(as.integer(n - 1))))
  digit_sql <- paste(vapply(0:9, function(value) {
    .temporalLiteralSelect(value, "n", handle)
  }, character(1)), collapse = " UNION ALL ")
  aliases <- paste0("d", seq_len(digits) - 1L)
  terms <- paste0(aliases, ".n * ", 10 ^ (seq_len(digits) - 1L))
  list(
    from = paste0(
      "(", digit_sql, ") ", aliases,
      collapse = " CROSS JOIN "
    ),
    index = paste(terms, collapse = " + ")
  )
}

.temporalSqlComponent <- function(sql, columns) {
  list(
    kind = "sql",
    sql = sql,
    columns = columns,
    chunk_transform = "temporal_component_columns"
  )
}

.temporalDataComponent <- function(data) {
  list(kind = "data", data = data, columns = names(data))
}

# Build the one-observation-period-per-episode relation used by every temporal
# panel component. OMOP permits multiple non-overlapping observation periods
# per person, but an index date must be covered by exactly one of them before
# absence of an event can be interpreted as absence during follow-up.
.temporalObservationSqlContract <- function(handle, cohort_table,
                                             blueprint = NULL) {
  blueprint <- blueprint %||% .buildBlueprint(handle)
  op_row <- blueprint$tables[
    blueprint$tables$table_name == "observation_period" &
      blueprint$tables$present_in_db,
    , drop = FALSE
  ]
  required <- c(
    "person_id", "observation_period_start_date",
    "observation_period_end_date"
  )
  op_columns <- blueprint$columns[["observation_period"]]$column_name %||%
    character(0)
  if (nrow(op_row) != 1L || !all(required %in% op_columns)) {
    stop("Temporal panels require the OMOP observation_period table with ",
         "person_id and start/end dates.", call. = FALSE)
  }

  op_schema <- .resolveTableSchema(
    handle, "observation_period", op_row$schema_category[[1L]]
  )
  op_table <- .qualifyTable(handle, "observation_period", op_schema)
  ranked_cohort <- .rankedCohortSql(cohort_table, handle)
  matches_sql <- paste0(
    "SELECT c.cohort_row_id, c.subject_id AS person_id, ",
    "c.cohort_start_date, ",
    "MIN(op.observation_period_start_date) AS observation_period_start_date, ",
    "MAX(op.observation_period_end_date) AS observation_period_end_date, ",
    "COUNT(op.person_id) AS dsomop_observation_matches FROM ",
    ranked_cohort, " c LEFT JOIN ", op_table, " op",
    " ON op.person_id = c.subject_id",
    " AND c.cohort_start_date >= op.observation_period_start_date",
    " AND c.cohort_start_date <= op.observation_period_end_date",
    " GROUP BY c.cohort_row_id, c.subject_id, c.cohort_start_date"
  )
  observed_sql <- paste0(
    "SELECT m.cohort_row_id, m.person_id, m.cohort_start_date, ",
    "m.observation_period_start_date, m.observation_period_end_date FROM (",
    matches_sql, ") m WHERE m.dsomop_observation_matches = 1"
  )
  invalid_sql <- paste0(
    "SELECT COUNT(*) AS value FROM (", matches_sql,
    ") m WHERE m.dsomop_observation_matches <> 1"
  )

  list(
    ranked_cohort = ranked_cohort,
    observed = observed_sql,
    invalid = invalid_sql
  )
}

.loadTemporalObservationRoster <- function(handle, cohort_table,
                                             blueprint = NULL) {
  contract <- .temporalObservationSqlContract(
    handle, cohort_table, blueprint = blueprint
  )
  # Do not reveal a data-quality diagnosis about a below-threshold cohort.
  # Event-specific qualification is gated again after observability filtering.
  .assertMinPersons(handle = handle, sql = paste0(
    "SELECT COUNT(DISTINCT c.subject_id) AS n_persons FROM ",
    contract$ranked_cohort, " c"
  ))
  invalid <- suppressWarnings(as.numeric(
    .executeQuery(handle, contract$invalid)[[1L]][[1L]]
  ))
  if (length(invalid) != 1L || is.na(invalid) || !is.finite(invalid) ||
      invalid != 0) {
    stop("Every cohort episode must have exactly one observation_period ",
         "covering its index date.", call. = FALSE)
  }

  roster <- .executeQuery(handle, paste0(
    "SELECT o.cohort_row_id, o.person_id, o.cohort_start_date, ",
    "o.observation_period_start_date, o.observation_period_end_date FROM (",
    contract$observed, ") o ORDER BY o.cohort_row_id"
  ))
  expected <- c(
    "cohort_row_id", "person_id", "cohort_start_date",
    "observation_period_start_date", "observation_period_end_date"
  )
  actual <- tolower(names(roster))
  if (anyDuplicated(actual) || !setequal(actual, expected)) {
    stop("Observation-period roster returned an unexpected column shape.",
         call. = FALSE)
  }
  roster <- roster[, match(expected, actual), drop = FALSE]
  names(roster) <- expected
  roster$cohort_row_id <- as.integer(roster$cohort_row_id)
  index_date <- as.Date(roster$cohort_start_date)
  observation_start <- as.Date(roster$observation_period_start_date)
  observation_end <- as.Date(roster$observation_period_end_date)
  if (anyNA(roster$cohort_row_id) || anyDuplicated(roster$cohort_row_id) ||
      anyNA(roster$person_id) || anyNA(index_date) ||
      anyNA(observation_start) || anyNA(observation_end)) {
    stop("Observation-period roster is incomplete or ambiguous.",
         call. = FALSE)
  }
  roster$observation_start_day <- as.integer(observation_start - index_date)
  roster$observation_end_day <- as.integer(observation_end - index_date)
  if (any(roster$observation_start_day > 0L) ||
      any(roster$observation_end_day < 0L)) {
    stop("Observation-period roster does not cover every index date.",
         call. = FALSE)
  }
  roster[, c(
    "cohort_row_id", "person_id", "observation_start_day",
    "observation_end_day"
  ), drop = FALSE]
}

# DBI drivers and .executeQuery() may normalize aliases to lower-case. Restore
# the public FeatureExtraction spelling on each bounded fetch, and fail closed
# if a backend returns a different shape than the compiler declared.
.normalizeTemporalSqlChunk <- function(data, component) {
  if (!is.data.frame(data) || !is.list(component) ||
      !identical(component$kind, "sql") ||
      !is.character(component$columns) || length(component$columns) == 0L) {
    stop("Invalid temporal SQL chunk or component contract.", call. = FALSE)
  }
  actual <- tolower(names(data))
  expected <- tolower(component$columns)
  if (anyDuplicated(actual) || anyDuplicated(expected) ||
      !setequal(actual, expected)) {
    stop("Temporal SQL component returned an unexpected column shape.",
         call. = FALSE)
  }
  data <- data[, match(expected, actual), drop = FALSE]
  names(data) <- component$columns
  data
}

#' Compile SQL-first temporal output components
#'
#' Compiles the large components of \code{temporal_covariates} and
#' \code{person_period} without executing or materializing them. The returned
#' queries expose only generated episode/time/covariate identifiers and relative
#' day bins. Every cohort episode must have exactly one OMOP observation period
#' covering its index date. Events outside that period are excluded. Absolute
#' dates and OMOP source-row identifiers remain inside nested queries and are
#' never projected by a component query.
#'
#' The caller must execute every entry in \code{validations} before streaming a
#' component. In particular, \code{min_persons} preserves DataSHIELD's
#' qualifying-population gate, and \code{max_concepts} bounds a data-derived
#' covariate dictionary when no public concept filter was supplied.
#'
#' @param handle CDM handle.
#' @param cohort_table Cohort table with subject/start/end columns.
#' @param table Person-keyed OMOP event table.
#' @param concept_filter Optional public concept-ID set.
#' @param bin_width Positive integer bin width in days.
#' @param window_start,window_end Inclusive integer offsets from index.
#' @param analyses Non-empty subset of \code{binary} and \code{count}.
#' @param filters Optional reviewed row-filter tree.
#' @param output_type \code{temporal_covariates} or \code{person_period}.
#' @param grain,time_origin Required as \code{episode}/\code{index} for a
#'   person-period output.
#' @return An internal component plan containing SQL or small public data frames.
#' @keywords internal
.compileTemporalSqlComponents <- function(
    handle, cohort_table, table, concept_filter = NULL,
    bin_width = 30L, window_start = -365L, window_end = 0L,
    analyses = "binary", filters = NULL,
    output_type = "temporal_covariates", grain = NULL,
    time_origin = NULL) {
  .assertAnalyticDbmsSupport(handle, "Temporal covariate SQL")
  if (!is.character(output_type) || length(output_type) != 1L ||
      is.na(output_type)) {
    stop("output_type must be temporal_covariates or person_period.",
         call. = FALSE)
  }
  output_type <- tolower(output_type)
  if (!output_type %in% c("temporal_covariates", "person_period")) {
    stop("output_type must be temporal_covariates or person_period.",
         call. = FALSE)
  }
  if (identical(output_type, "person_period")) {
    if (!is.character(grain) || length(grain) != 1L || is.na(grain) ||
        !identical(tolower(grain), "episode")) {
      stop("person_period grain must be explicitly 'episode'.", call. = FALSE)
    }
    if (!is.character(time_origin) || length(time_origin) != 1L ||
        is.na(time_origin) || !identical(tolower(time_origin), "index")) {
      stop("person_period time_origin must be explicitly 'index'.",
           call. = FALSE)
    }
  }
  if (is.null(cohort_table)) {
    stop(output_type, " requires a cohort.", call. = FALSE)
  }
  if (!is.character(cohort_table) || length(cohort_table) != 1L ||
      is.na(cohort_table) || !nzchar(cohort_table)) {
    stop("cohort_table must be one table identifier.", call. = FALSE)
  }
  if (!is.character(table) || length(table) != 1L || is.na(table) ||
      !nzchar(table)) {
    stop("table must be one OMOP table identifier.", call. = FALSE)
  }

  cohort_table <- .validateIdentifier(cohort_table, "cohort table")
  table <- tolower(.validateIdentifier(table, "temporal table"))
  bin_width <- .temporalSqlInteger(bin_width, "bin_width")
  window_start <- .temporalSqlInteger(window_start, "window_start")
  window_end <- .temporalSqlInteger(window_end, "window_end")
  if (bin_width <= 0L) {
    stop("bin_width must be greater than zero.", call. = FALSE)
  }
  if (window_start > window_end) {
    stop("window_start must not be after window_end.", call. = FALSE)
  }
  n_bins <- floor(
    (as.double(window_end) - as.double(window_start)) / bin_width
  ) + 1
  max_bins <- .extractionCap("dsomop.max_temporal_bins", 10000L)
  if (!is.finite(n_bins) || n_bins > max_bins) {
    stop("Temporal covariates would create ", n_bins,
         " bins, exceeding the server cap of ", max_bins, ".",
         call. = FALSE)
  }
  analyses <- .normalizeTemporalSqlAnalyses(analyses)
  max_concepts <- .extractionCap("dsomop.max_pivot_concepts", 1000L)
  concepts <- .normalizeTemporalSqlConcepts(
    handle, concept_filter, max_concepts
  )

  blueprint <- .buildBlueprint(handle)
  table_row <- blueprint$tables[
    blueprint$tables$table_name == table & blueprint$tables$present_in_db,
    , drop = FALSE
  ]
  if (nrow(table_row) != 1L) {
    stop("Table '", table, "' not found in CDM schema.", call. = FALSE)
  }
  table_columns <- blueprint$columns[[table]]$column_name %||% character(0)
  if (!"person_id" %in% table_columns) {
    stop("Temporal SQL requires a person-keyed OMOP table.", call. = FALSE)
  }
  date_column <- .getDateColumn(blueprint, table)
  concept_column <- .getDomainConceptColumn(blueprint, table)
  if (is.null(date_column)) {
    stop("Temporal SQL requires a reviewed OMOP event date column.",
         call. = FALSE)
  }
  if (is.null(concept_column)) {
    stop("Temporal SQL requires a reviewed OMOP domain concept column.",
         call. = FALSE)
  }

  observation <- .temporalObservationSqlContract(
    handle, cohort_table, blueprint = blueprint
  )
  event_sql <- .compileSelect(
    handle = handle,
    table = table,
    columns = c(date_column, concept_column),
    concept_filter = concepts,
    cohort_table = cohort_table,
    temporal = list(index_window = list(
      start = window_start, end = window_end
    )),
    add_cohort_date = TRUE,
    filters = filters,
    block_sensitive = TRUE
  )
  observed_event_sql <- paste0(
    "SELECT q.* FROM (", event_sql, ") q INNER JOIN (",
    observation$observed, ") o",
    " ON o.cohort_row_id = q.cohort_row_id",
    " AND o.person_id = q.person_id",
    " AND q.", date_column, " >= o.observation_period_start_date",
    " AND q.", date_column, " <= o.observation_period_end_date"
  )
  day_expression <- .temporalSqlDateDiffDays(
    handle, paste0("q.", date_column), "q.cohort_start_date"
  )
  time_expression <- paste0(
    .temporalSqlFloorDivide(
      paste0("(", day_expression, ") - (", window_start, ")"),
      bin_width,
      handle
    ),
    " + 1"
  )
  binned_sql <- paste0(
    "SELECT q.cohort_row_id AS rowId, q.person_id AS person_id, ",
    time_expression, " AS timeId, q.", concept_column, " AS conceptId",
    " FROM (", observed_event_sql, ") q",
    " WHERE q.", date_column, " IS NOT NULL",
    " AND q.", concept_column, " IS NOT NULL"
  )

  analysis_ids <- c(binary = 1L, count = 2L)
  covariate_queries <- vapply(analyses, function(analysis) {
    analysis_id <- analysis_ids[[analysis]]
    covariate_id <- paste0(
      .temporalSqlBigInteger("b.conceptId", handle),
      " * 1000 + ", analysis_id
    )
    value <- if (identical(analysis, "binary")) "1" else "COUNT(*)"
    paste0(
      "SELECT b.rowId AS rowId, b.timeId AS timeId, ",
      covariate_id, " AS covariateId, ", value,
      " AS covariateValue FROM dsomop_binned b",
      " GROUP BY b.rowId, b.timeId, b.conceptId"
    )
  }, character(1))
  temporal_covariates_sql <- paste0(
    "WITH dsomop_binned AS (", binned_sql, ") ",
    paste(covariate_queries, collapse = " UNION ALL "),
    " ORDER BY rowId, timeId, covariateId"
  )

  person_ref_sql <- paste0(
    "SELECT o.cohort_row_id AS rowId, o.person_id AS person_id FROM (",
    observation$observed, ") o ORDER BY o.cohort_row_id"
  )
  time_ref <- .generateTimeWindows(bin_width, window_start, window_end)
  if (nrow(time_ref) != n_bins || anyNA(time_ref)) {
    stop("Could not construct the requested temporal bins safely.",
         call. = FALSE)
  }

  if (is.null(concepts)) {
    ref_queries <- vapply(analyses, function(analysis) {
      analysis_id <- analysis_ids[[analysis]]
      covariate_id <- paste0(
        .temporalSqlBigInteger("b.conceptId", handle),
        " * 1000 + ", analysis_id
      )
      paste0(
        "SELECT DISTINCT ", covariate_id, " AS covariateId, ",
        .temporalSqlConceptName("b.conceptId", analysis, handle),
        " AS covariateName, ", analysis_id, " AS analysisId, ",
        "b.conceptId AS conceptId FROM dsomop_binned b"
      )
    }, character(1))
    covariate_ref <- .temporalSqlComponent(
      paste0(
        "WITH dsomop_binned AS (", binned_sql, ") ",
        paste(ref_queries, collapse = " UNION ALL "),
        " ORDER BY conceptId, analysisId"
      ),
      c("covariateId", "covariateName", "analysisId", "conceptId")
    )
  } else {
    covariate_ref <- .temporalDataComponent(
      .temporalCovariateRefData(concepts, analyses)
    )
  }

  components <- list(
    temporalCovariates = .temporalSqlComponent(
      temporal_covariates_sql,
      c("rowId", "timeId", "covariateId", "covariateValue")
    ),
    covariateRef = covariate_ref,
    timeRef = .temporalDataComponent(time_ref),
    personRef = .temporalSqlComponent(
      person_ref_sql, c("rowId", "person_id")
    )
  )

  if (identical(output_type, "person_period")) {
    number_grid <- .temporalNumberGridSql(n_bins, handle)
    start_expression <- paste0(
      "(", window_start, " + (", number_grid$index, ") * ", bin_width, ")"
    )
    proposed_end <- paste0("(", start_expression, " + ", bin_width - 1L,
                           ")")
    end_expression <- paste0(
      "CASE WHEN ", proposed_end, " > ", window_end, " THEN ", window_end,
      " ELSE ", proposed_end, " END"
    )
    observed_cohort_sql <- paste0(
      "SELECT o.cohort_row_id, ",
      .temporalSqlDateDiffDays(
        handle, "o.observation_period_start_date", "o.cohort_start_date"
      ), " AS observationStartDay, ",
      .temporalSqlDateDiffDays(
        handle, "o.observation_period_end_date", "o.cohort_start_date"
      ), " AS observationEndDay FROM (", observation$observed, ") o"
    )
    clipped_start <- paste0(
      "CASE WHEN ", start_expression, " > c.observationStartDay THEN ",
      start_expression, " ELSE c.observationStartDay END"
    )
    clipped_end <- paste0(
      "CASE WHEN ", end_expression, " < c.observationEndDay THEN ",
      end_expression, " ELSE c.observationEndDay END"
    )
    person_period_sql <- paste0(
      "SELECT c.cohort_row_id AS rowId, (", number_grid$index,
      ") + 1 AS timeId, ", start_expression, " AS startDay, ",
      end_expression, " AS endDay, ", clipped_start,
      " AS observationStartDay, ", clipped_end,
      " AS observationEndDay, (", clipped_end, ") - (", clipped_start,
      ") + 1 AS daysObserved FROM (", observed_cohort_sql,
      ") c CROSS JOIN ",
      number_grid$from, " WHERE (", number_grid$index, ") < ", n_bins,
      " AND ", end_expression, " >= c.observationStartDay",
      " AND ", start_expression, " <= c.observationEndDay",
      " ORDER BY c.cohort_row_id, timeId"
    )
    components$personPeriods <- .temporalSqlComponent(
      person_period_sql,
      c(
        "rowId", "timeId", "startDay", "endDay",
        "observationStartDay", "observationEndDay", "daysObserved"
      )
    )
  }

  validations <- list(
    min_persons = list(
      kind = "min_persons",
      sql = paste0(
        "SELECT COUNT(DISTINCT person_id) AS n_persons FROM (",
        binned_sql, ") dsomop_temporal_gate"
      )
    ),
    observation_period = list(
      kind = "max_value",
      sql = observation$invalid,
      max = 0,
      label = paste0(
        "cohort episodes without exactly one covering observation period"
      )
    )
  )
  if (is.null(concepts)) {
    validations$max_concepts <- list(
      kind = "max_value",
      sql = paste0(
        "SELECT COUNT(DISTINCT conceptId) AS value FROM (", binned_sql,
        ") dsomop_temporal_concept_cap"
      ),
      max = max_concepts,
      label = "temporal concepts"
    )
  }

  structure(list(
    output_type = output_type,
    grain = "episode",
    time_origin = "index",
    components = components,
    validations = validations,
    source = list(table = table, date_column = date_column,
                  concept_column = concept_column),
    window = list(start = window_start, end = window_end,
                  bin_width = bin_width, n_bins = as.integer(n_bins)),
    analyses = analyses
  ), class = c("omop_temporal_sql_components", "list"))
}
