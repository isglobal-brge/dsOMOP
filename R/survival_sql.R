# SQL-first longitudinal risk-set and survival output contracts.

.survivalExactInteger <- function(value, name, default = NULL) {
  if (is.null(value)) return(default)
  number <- suppressWarnings(as.numeric(value))
  integer <- suppressWarnings(as.integer(value))
  if (length(value) != 1L || length(number) != 1L || is.na(number) ||
      !is.finite(number) || length(integer) != 1L || is.na(integer) ||
      number != integer) {
    stop(name, " must be one finite exact integer day offset.",
         call. = FALSE)
  }
  integer
}

.survivalLogical <- function(value, name, default) {
  if (is.null(value)) return(default)
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop(name, " must be TRUE or FALSE.", call. = FALSE)
  }
  value
}

.survivalServerVersion <- function(handle) {
  .databaseServerVersion(handle)
}

.survivalVersionAtLeast <- function(version, minimum, family) {
  .databaseVersionAtLeast(version, minimum, family)
}

.survivalSupportedDbms <- function(handle) {
  dbms <- .normalizeDBMS(handle$dbms %||% handle$target_dialect %||% "")
  supported <- c(
    "sqlite", "duckdb", "postgresql", "sqlserver", "synapse", "pdw",
    "oracle", "redshift", "bigquery", "snowflake", "spark", "databricks",
    "mysql", "mariadb"
  )
  if (!dbms %in% supported) {
    stop("Longitudinal risk-set SQL is not implemented for DBMS '", dbms,
         "'.", call. = FALSE)
  }
  .assertAnalyticDbmsSupport(handle, "Longitudinal risk-set SQL")
  dbms
}

.survivalDateAdd <- function(handle, days, expression) {
  .renderSql(
    handle,
    "DATEADD(day, @days, @expression)",
    days = .survivalExactInteger(days, "date offset"),
    expression = expression
  )
}

.survivalDateDiff <- function(handle, end_expression, start_expression) {
  dbms <- .normalizeDBMS(handle$dbms %||% handle$target_dialect %||% "")
  if (identical(dbms, "sqlite")) {
    return(paste0(
      "CAST(julianday(", end_expression, ") - julianday(",
      start_expression, ") AS INTEGER)"
    ))
  }
  if (dbms %in% c("sqlserver", "synapse", "pdw", "redshift", "snowflake")) {
    return(paste0(
      "DATEDIFF(day, ", start_expression, ", ", end_expression, ")"
    ))
  }
  if (identical(dbms, "bigquery")) {
    return(paste0(
      "DATE_DIFF(CAST(", end_expression, " AS DATE), CAST(",
      start_expression, " AS DATE), DAY)"
    ))
  }
  if (dbms %in% c("spark", "databricks")) {
    return(paste0("DATEDIFF(", end_expression, ", ", start_expression, ")"))
  }
  if (dbms %in% c("mysql", "mariadb")) {
    return(paste0("DATEDIFF(", end_expression, ", ", start_expression, ")"))
  }
  if (identical(dbms, "oracle")) {
    return(paste0(
      "CAST(TRUNC(", end_expression, ") - TRUNC(", start_expression,
      ") AS INTEGER)"
    ))
  }
  paste0(
    "CAST(CAST(", end_expression, " AS DATE) - CAST(", start_expression,
    " AS DATE) AS INTEGER)"
  )
}

.survivalDateLiteral <- function(handle, value, name = "admin_date") {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)) {
    stop(name, " must be one ISO date (YYYY-MM-DD).", call. = FALSE)
  }
  parsed <- suppressWarnings(as.Date(value))
  if (is.na(parsed) || !identical(format(parsed, "%Y-%m-%d"), value)) {
    stop(name, " must be one valid ISO date (YYYY-MM-DD).", call. = FALSE)
  }
  literal <- .quoteLiteral(value, handle)
  dbms <- .normalizeDBMS(handle$dbms %||% handle$target_dialect %||% "")
  if (identical(dbms, "sqlite")) {
    paste0("DATE(", literal, ")")
  } else {
    paste0("CAST(", literal, " AS DATE)")
  }
}

.survivalLeastDate <- function(expressions) {
  expressions <- expressions[!vapply(expressions, is.null, logical(1))]
  if (length(expressions) == 0L) {
    stop("At least one censoring boundary is required.", call. = FALSE)
  }
  Reduce(function(left, right) {
    paste0(
      "(CASE WHEN ", left, " IS NULL THEN ", right,
      " WHEN ", right, " IS NULL THEN ", left,
      " WHEN ", left, " <= ", right, " THEN ", left,
      " ELSE ", right, " END)"
    )
  }, expressions)
}

.normalizeSurvivalTar <- function(tar) {
  tar <- tar %||% list()
  if (!is.list(tar) ||
      (length(tar) > 0L && (is.null(names(tar)) || any(!nzchar(names(tar))) ||
                            anyDuplicated(names(tar))))) {
    stop("tar must be a uniquely named list.", call. = FALSE)
  }
  unknown <- setdiff(names(tar), c("start_offset", "end_offset"))
  if (length(unknown) > 0L) {
    stop("Unknown TAR field(s): ", paste(unknown, collapse = ", "), ".",
         call. = FALSE)
  }
  start <- .survivalExactInteger(tar$start_offset, "tar$start_offset", 0L)
  end <- .survivalExactInteger(tar$end_offset, "tar$end_offset", NULL)
  if (!is.null(end) && end < start) {
    stop("tar$end_offset must not be before tar$start_offset.",
         call. = FALSE)
  }
  list(start_offset = start, end_offset = end)
}

.normalizeSurvivalCensoring <- function(censoring) {
  censoring <- censoring %||% list()
  if (!is.list(censoring) ||
      (length(censoring) > 0L &&
       (is.null(names(censoring)) || any(!nzchar(names(censoring))) ||
        anyDuplicated(names(censoring))))) {
    stop("censoring must be a uniquely named list.", call. = FALSE)
  }
  allowed <- c("cohort_end", "observation_period_end", "death", "admin_date")
  unknown <- setdiff(names(censoring), allowed)
  if (length(unknown) > 0L) {
    stop("Unknown censoring field(s): ", paste(unknown, collapse = ", "), ".",
         call. = FALSE)
  }
  cohort_end <- .survivalLogical(censoring$cohort_end, "censoring$cohort_end",
                                 TRUE)
  if (!cohort_end) {
    stop("censoring$cohort_end must remain TRUE for this conservative risk-set contract.",
         call. = FALSE)
  }
  list(
    cohort_end = TRUE,
    observation_period_end = .survivalLogical(
      censoring$observation_period_end,
      "censoring$observation_period_end", TRUE
    ),
    death = .survivalLogical(censoring$death, "censoring$death", TRUE),
    admin_date = censoring$admin_date %||% NULL
  )
}

.normalizeSurvivalOutcomes <- function(handle, outcomes, blueprint) {
  if (!is.list(outcomes) || length(outcomes) == 0L || is.null(names(outcomes)) ||
      any(!nzchar(names(outcomes))) || anyDuplicated(names(outcomes))) {
    stop("outcomes must be a non-empty, uniquely named list.",
         call. = FALSE)
  }
  max_outcomes <- suppressWarnings(as.integer(getOption(
    "dsomop.max_plan_outputs",
    getOption("default.dsomop.max_plan_outputs", 100L)
  )))
  if (is.na(max_outcomes) || max_outcomes < 1L || length(outcomes) > max_outcomes) {
    stop("outcomes exceeds the server longitudinal output cap.",
         call. = FALSE)
  }
  name_limit <- .omopDisclosureSettings()$nfilter_string
  bad_name <- !grepl("^[A-Za-z][A-Za-z0-9_.-]*$", names(outcomes)) |
    nchar(names(outcomes), type = "bytes") > name_limit
  if (any(bad_name)) {
    stop("Outcome names must start with a letter, contain only letters, digits, ",
         "underscore, dot or dash, and respect nfilter.string.",
         call. = FALSE)
  }

  normalized <- vector("list", length(outcomes))
  for (index in seq_along(outcomes)) {
    outcome <- outcomes[[index]]
    if (!is.list(outcome) || is.null(names(outcome)) ||
        any(!nzchar(names(outcome))) || anyDuplicated(names(outcome))) {
      stop("Outcome '", names(outcomes)[index],
           "' must be a uniquely named list.", call. = FALSE)
    }
    unknown <- setdiff(names(outcome), c("table", "concept_set", "filters"))
    if (length(unknown) > 0L ||
        !all(c("table", "concept_set") %in% names(outcome))) {
      stop("Outcome '", names(outcomes)[index],
           "' must contain table and concept_set, with optional filters only.",
           call. = FALSE)
    }
    table <- tolower(.validateIdentifier(outcome$table, "outcome table"))
    table_row <- blueprint$tables[
      blueprint$tables$table_name == table & blueprint$tables$present_in_db,
      , drop = FALSE
    ]
    if (nrow(table_row) != 1L) {
      stop("Outcome table '", table, "' is unavailable.", call. = FALSE)
    }
    columns <- blueprint$columns[[table]]$column_name %||% character(0)
    required <- c("person_id")
    if (!all(required %in% columns)) {
      stop("Outcome table '", table, "' has no person_id linkage.",
           call. = FALSE)
    }
    date_column <- .getDateColumn(blueprint, table)
    concept_column <- .getDomainConceptColumn(blueprint, table)
    primary_key <- .eventPrimaryKeyColumn(blueprint, table)
    if (is.null(date_column) || is.null(concept_column) || is.null(primary_key) ||
        !all(c(date_column, concept_column, primary_key) %in% columns)) {
      stop("Outcome table '", table, "' requires reviewed event-date, domain-",
           "concept and primary-key columns.", call. = FALSE)
    }
    concepts <- .resolveConceptSet(handle, outcome$concept_set)
    if (length(concepts) == 0L) {
      stop("Outcome '", names(outcomes)[index],
           "' concept_set resolved to no concepts.", call. = FALSE)
    }
    filter_sql <- NULL
    if (!is.null(outcome$filters) && length(outcome$filters) > 0L) {
      valid_columns <- .filterableColumns(blueprint, table)
      .assertCustomFilterSafe(outcome$filters, valid_columns, handle = handle,
                              table = table)
      filter_sql <- .compileFilter(handle, outcome$filters, "t", valid_columns)
    }
    schema <- .resolveTableSchema(handle, table, table_row$schema_category[1L])
    normalized[[index]] <- list(
      name = names(outcomes)[index],
      priority = as.integer(index),
      table = table,
      qualified = .qualifyTable(handle, table, schema),
      date_column = date_column,
      concept_column = concept_column,
      primary_key = primary_key,
      concepts = concepts,
      filter_sql = filter_sql
    )
  }
  normalized
}

.survivalRequireTable <- function(handle, blueprint, table, columns) {
  row <- blueprint$tables[
    blueprint$tables$table_name == table & blueprint$tables$present_in_db,
    , drop = FALSE
  ]
  available <- blueprint$columns[[table]]$column_name %||% character(0)
  if (nrow(row) != 1L || !all(columns %in% available)) {
    stop("Censoring requires OMOP table '", table, "' with columns: ",
         paste(columns, collapse = ", "), ".", call. = FALSE)
  }
  .qualifyTable(
    handle, table,
    .resolveTableSchema(handle, table, row$schema_category[1L])
  )
}

.survivalRankedCohortSql <- function(handle, cohort_table) {
  fields <- names(.executeQuery(
    handle, paste0("SELECT * FROM ", cohort_table, " WHERE 1 = 0")
  ))
  required <- c("subject_id", "cohort_start_date", "cohort_end_date")
  if (!all(required %in% fields)) {
    stop("Longitudinal survival cohort requires subject_id, ",
         "cohort_start_date and cohort_end_date.", call. = FALSE)
  }
  episode_key <- if ("dsomop_episode_key" %in% fields) {
    "dsomop_episode_key"
  } else if ("index_event_id" %in% fields) {
    "index_event_id"
  } else {
    NULL
  }
  source_key <- if (is.null(episode_key)) "" else paste0(", ", episode_key)
  projected_key <- if (is.null(episode_key)) {
    ""
  } else {
    paste0(", cohort_base.", episode_key, " AS dsomop_episode_key")
  }
  order_key <- if (is.null(episode_key)) {
    ""
  } else {
    paste0(", cohort_base.", episode_key)
  }
  # Table aliases intentionally omit AS: Oracle rejects AS for table aliases,
  # while every other supported backend accepts the portable form below.
  paste0(
    "(SELECT cohort_base.subject_id, cohort_base.cohort_start_date, ",
    "cohort_base.cohort_end_date", projected_key,
    ", ROW_NUMBER() OVER (ORDER BY cohort_base.subject_id, ",
    "cohort_base.cohort_start_date, cohort_base.cohort_end_date", order_key,
    ") AS cohort_row_id FROM (SELECT DISTINCT subject_id, ",
    "cohort_start_date, cohort_end_date", source_key, " FROM ",
    cohort_table, ") cohort_base)"
  )
}

.survivalEventSourceSql <- function(handle, outcome, washout_days) {
  concept_predicate <- .sqlIdInPredicate(
    paste0("t.", outcome$concept_column), outcome$concepts
  )
  filter_predicate <- if (!is.null(outcome$filter_sql) &&
                          nzchar(outcome$filter_sql)) {
    paste0(" AND ", outcome$filter_sql)
  } else {
    ""
  }
  lower_bound <- .survivalDateAdd(
    handle, -washout_days, "e.tar_start_date"
  )
  paste0(
    "SELECT e.cohort_row_id, e.subject_id, e.cohort_start_date, ",
    "e.tar_start_date, e.censor_date, e.risk_start_days, e.risk_end_days, ",
    .quoteLiteral(outcome$name, handle), " AS outcome_name, ",
    outcome$priority, " AS outcome_priority, t.", outcome$date_column,
    " AS event_date, t.", outcome$primary_key, " AS event_key FROM ",
    "risk_episodes e INNER JOIN ", outcome$qualified,
    " t ON t.person_id = e.subject_id AND t.", outcome$date_column,
    " >= ", lower_bound, " AND t.", outcome$date_column,
    " <= e.censor_date WHERE ", concept_predicate, filter_predicate
  )
}

.survivalEventValidationSql <- function(handle, cohort_table, outcome) {
  concept_predicate <- .sqlIdInPredicate(
    paste0("t.", outcome$concept_column), outcome$concepts
  )
  filter_predicate <- if (!is.null(outcome$filter_sql) &&
                          nzchar(outcome$filter_sql)) {
    paste0(" AND ", outcome$filter_sql)
  } else {
    ""
  }
  scoped <- paste0(
    concept_predicate, filter_predicate,
    " AND EXISTS (SELECT 1 FROM ", cohort_table,
    " c WHERE c.subject_id = t.person_id)"
  )
  list(
    null_event_fields = paste0(
      "SELECT COUNT(*) AS n_violations FROM ", outcome$qualified,
      " t WHERE ", scoped, " AND (t.", outcome$date_column,
      " IS NULL OR t.", outcome$primary_key, " IS NULL)"
    ),
    duplicate_event_key = paste0(
      "SELECT COUNT(*) AS n_violations FROM (SELECT t.", outcome$primary_key,
      " FROM ", outcome$qualified, " t WHERE ", scoped,
      " GROUP BY t.", outcome$primary_key,
      " HAVING COUNT(*) > 1) duplicate_keys"
    )
  )
}

#' Compile a streamable longitudinal survival/risk-set query
#'
#' The returned SQL projects person_id only as a trusted internal transport
#' column: the plan executor must pseudonymize it before assignment or staging.
#' It never projects source event identifiers or calendar dates. Public fields
#' otherwise contain the query-local cohort row key, outcome labels, event
#' indicators and integer day offsets. A qualifying event is attached
#' independently to each recurrent episode whose time-at-risk contains it.
#'
#' `format = "survival"` returns one outcome-specific row per episode and
#' named endpoint. `format = "competing_risk"` returns the first event of any
#' outcome, with input order providing deterministic cause priority.
#' `format = "recurrent_events"` returns an event stream plus a separate
#' episode risk-set component. `format = "counting_process"` returns
#' non-overlapping start/stop rows under the declared daily tie policy.
#'
#' @param handle CDM handle.
#' @param cohort_table Cohort table with subject_id/start/end columns.
#' @param outcomes Uniquely named list of table/concept_set specifications.
#' @param tar Index-relative start/end day offsets.
#' @param censoring Clinical censoring choices. Cohort end is mandatory;
#'   observation-period end and death default to enabled. admin_date is an
#'   optional controller-provided ISO date.
#' @param format survival, competing_risk, recurrent_events, or
#'   counting_process.
#' @param event_order first, last, or all. last is retained for legacy survival
#'   plans; all is available for recurrent/counting formats only.
#' @param washout_days Minimum clean interval between raw events of the same
#'   named outcome. Events in the pre-entry lookback participate in washout.
#' @param tie_policy priority, error, or all. all is restricted to the event-log
#'   format because daily start/stop and competing-risk outputs require one
#'   deterministic event at a time point.
#' @param legacy Emit the historical single-outcome survival columns and
#'   time-to-event semantics. Intended only for old plan shapes.
#' @return A `dsomop_longitudinal_sql` internal execution contract.
#' @keywords internal
.compileLongitudinalSurvivalSql <- function(
    handle, cohort_table, outcomes, tar = NULL, censoring = NULL,
    format = "survival", event_order = NULL, washout_days = 0L,
    tie_policy = "priority", legacy = FALSE) {
  dbms <- .survivalSupportedDbms(handle)
  if (is.null(cohort_table)) {
    stop("Longitudinal survival output requires a cohort.", call. = FALSE)
  }
  cohort_table <- .validateIdentifier(cohort_table, "survival cohort table")
  if (!is.character(format) || length(format) != 1L || is.na(format)) {
    stop("format must be one longitudinal survival format.", call. = FALSE)
  }
  format <- tolower(format)
  allowed_formats <- c(
    "survival", "competing_risk", "recurrent_events", "counting_process"
  )
  if (!format %in% allowed_formats) {
    stop("format must be survival, competing_risk, recurrent_events, or ",
         "counting_process.", call. = FALSE)
  }
  if (!is.logical(legacy) || length(legacy) != 1L || is.na(legacy)) {
    stop("legacy must be TRUE or FALSE.", call. = FALSE)
  }
  if (legacy && (format != "survival" || length(outcomes) != 1L)) {
    stop("legacy survival requires format='survival' and exactly one outcome.",
         call. = FALSE)
  }
  event_order <- event_order %||% if (format %in%
    c("recurrent_events", "counting_process")) "all" else "first"
  if (!is.character(event_order) || length(event_order) != 1L ||
      is.na(event_order) ||
      !tolower(event_order) %in% c("first", "last", "all")) {
    stop("event_order must be first, last, or all.", call. = FALSE)
  }
  event_order <- tolower(event_order)
  if (format == "survival" && event_order == "all") {
    stop("survival requires event_order first or last.", call. = FALSE)
  }
  if (format == "competing_risk" && event_order != "first") {
    stop("competing_risk requires event_order='first'.", call. = FALSE)
  }
  if (format %in% c("recurrent_events", "counting_process") &&
      event_order == "last") {
    stop("recurrent_events and counting_process require event_order first or all.",
         call. = FALSE)
  }
  if (!is.character(tie_policy) || length(tie_policy) != 1L ||
      is.na(tie_policy) ||
      !tolower(tie_policy) %in% c("priority", "error", "all")) {
    stop("tie_policy must be priority, error, or all.", call. = FALSE)
  }
  tie_policy <- tolower(tie_policy)
  if (tie_policy == "all" && format != "recurrent_events") {
    stop("tie_policy='all' is supported only for recurrent_events; other ",
         "formats require one daily event chosen by priority or checked with ",
         "tie_policy='error'.", call. = FALSE)
  }
  washout_days <- .survivalExactInteger(
    washout_days, "washout_days", 0L
  )
  if (washout_days < 0L) {
    stop("washout_days must not be negative.", call. = FALSE)
  }

  tar <- .normalizeSurvivalTar(tar)
  censoring <- .normalizeSurvivalCensoring(censoring)
  blueprint <- .buildBlueprint(handle)
  outcomes <- .normalizeSurvivalOutcomes(handle, outcomes, blueprint)

  # Observation coverage is part of the estimand even when OP end is not one
  # of the requested censoring candidates: delayed entry and washout are not
  # interpretable when their lookback starts before observable data.
  observation_table <- .survivalRequireTable(
    handle, blueprint, "observation_period",
    c("person_id", "observation_period_start_date",
      "observation_period_end_date")
  )
  death_table <- NULL
  if (censoring$death) {
    death_table <- .survivalRequireTable(
      handle, blueprint, "death", c("person_id", "death_date")
    )
  }
  admin_expression <- if (is.null(censoring$admin_date)) {
    NULL
  } else {
    .survivalDateLiteral(handle, censoring$admin_date)
  }

  ranked <- .survivalRankedCohortSql(handle, cohort_table)
  episode_cte <- paste0(
    "episode_base AS (SELECT c.cohort_row_id, c.subject_id, ",
    "c.cohort_start_date, c.cohort_end_date FROM ", ranked, " c)"
  )
  ctes <- episode_cte
  joins <- character(0)
  censor_candidates <- list("eb.cohort_end_date")

  op_cte <- paste0(
    "op_cover AS (SELECT eb.cohort_row_id, COUNT(op.person_id) AS op_count, ",
    "MIN(op.observation_period_start_date) AS observation_period_start_date, ",
    "MIN(op.observation_period_end_date) AS observation_period_end_date ",
    "FROM episode_base eb LEFT JOIN ", observation_table,
    " op ON op.person_id = eb.subject_id AND eb.cohort_start_date >= ",
    "op.observation_period_start_date AND eb.cohort_start_date <= ",
    "op.observation_period_end_date GROUP BY eb.cohort_row_id)"
  )
  ctes <- c(ctes, op_cte)
  joins <- c(joins,
    " LEFT JOIN op_cover op ON op.cohort_row_id = eb.cohort_row_id")
  if (censoring$observation_period_end) {
    censor_candidates <- c(censor_candidates,
                           list("op.observation_period_end_date"))
  }
  if (censoring$death) {
    death_cte <- paste0(
      "death_one AS (SELECT d.person_id, MIN(d.death_date) AS death_date, ",
      "COUNT(*) AS death_count FROM ", death_table,
      " d WHERE EXISTS (SELECT 1 FROM episode_base eb ",
      "WHERE eb.subject_id = d.person_id) GROUP BY d.person_id)"
    )
    ctes <- c(ctes, death_cte)
    joins <- c(joins,
      " LEFT JOIN death_one d ON d.person_id = eb.subject_id")
    censor_candidates <- c(censor_candidates, list("d.death_date"))
  }
  if (!is.null(tar$end_offset)) {
    censor_candidates <- c(censor_candidates, list(.survivalDateAdd(
      handle, tar$end_offset, "eb.cohort_start_date"
    )))
  }
  if (!is.null(admin_expression)) {
    censor_candidates <- c(censor_candidates, list(admin_expression))
  }
  tar_start_expression <- .survivalDateAdd(
    handle, tar$start_offset, "eb.cohort_start_date"
  )
  censor_expression <- .survivalLeastDate(censor_candidates)
  bounds_cte <- paste0(
    "episode_bounds AS (SELECT eb.cohort_row_id, eb.subject_id, ",
    "eb.cohort_start_date, ", tar_start_expression,
    " AS tar_start_date, ", censor_expression, " AS censor_date FROM ",
    "episode_base eb", paste(joins, collapse = ""), ")"
  )
  risk_end_expression <- .survivalDateDiff(
    handle, "b.censor_date", "b.cohort_start_date"
  )
  risk_cte <- paste0(
    "risk_episodes AS (SELECT b.cohort_row_id, b.subject_id, ",
    "b.cohort_start_date, b.tar_start_date, b.censor_date, ",
    tar$start_offset, " AS risk_start_days, ", risk_end_expression,
    " AS risk_end_days FROM episode_bounds b WHERE b.censor_date >= ",
    "b.tar_start_date)"
  )
  ctes <- c(ctes, bounds_cte, risk_cte)
  risk_ctes <- ctes

  event_sources <- vapply(
    outcomes, .survivalEventSourceSql, character(1),
    handle = handle, washout_days = washout_days
  )
  raw_event_cte <- paste0(
    "raw_events AS (", paste(event_sources, collapse = " UNION ALL "), ")"
  )
  ctes <- c(ctes, raw_event_cte)

  if (washout_days > 0L) {
    ctes <- c(ctes, paste0(
      "lagged_events AS (SELECT r.*, LAG(r.event_date) OVER (PARTITION BY ",
      "r.cohort_row_id, r.outcome_priority ORDER BY r.event_date, ",
      "r.event_key) AS previous_event_date FROM raw_events r)"
    ))
    previous_limit <- .survivalDateAdd(
      handle, washout_days, "l.previous_event_date"
    )
    ctes <- c(ctes, paste0(
      "eligible_events AS (SELECT l.* FROM lagged_events l WHERE ",
      "l.event_date >= l.tar_start_date AND (l.previous_event_date IS NULL ",
      "OR l.event_date > ", previous_limit, "))"
    ))
  } else {
    ctes <- c(ctes, paste0(
      "eligible_events AS (SELECT r.* FROM raw_events r WHERE ",
      "r.event_date >= r.tar_start_date)"
    ))
  }
  event_prefix <- paste0("WITH ", paste(ctes, collapse = ", "))

  validation_sql <- list(
    cohort_dates = paste0(
      "WITH ", episode_cte,
      " SELECT COUNT(*) AS n_violations FROM episode_base eb WHERE ",
      "eb.subject_id IS NULL OR eb.cohort_start_date IS NULL OR ",
      "eb.cohort_end_date IS NULL OR eb.cohort_end_date < ",
      "eb.cohort_start_date"
    )
  )
  required_observation_start <- .survivalDateAdd(
    handle, tar$start_offset - washout_days, "eb.cohort_start_date"
  )
  validation_sql$observation_period_coverage <- paste0(
    "WITH ", episode_cte, ", ", op_cte,
    " SELECT COUNT(*) AS n_violations FROM episode_base eb LEFT JOIN ",
    "op_cover op ON op.cohort_row_id = eb.cohort_row_id WHERE ",
    "op.op_count <> 1 OR op.observation_period_start_date IS NULL OR ",
    "op.observation_period_end_date IS NULL OR ",
    "op.observation_period_start_date > ", required_observation_start,
    " OR op.observation_period_end_date < ", tar_start_expression
  )
  if (censoring$death) {
    validation_sql$death_rows <- paste0(
      "WITH ", episode_cte, ", ", death_cte,
      " SELECT COUNT(*) AS n_violations FROM episode_base eb INNER JOIN ",
      "death_one d ON d.person_id = eb.subject_id WHERE d.death_count <> 1 ",
      "OR d.death_date IS NULL OR d.death_date < eb.cohort_start_date"
    )
  }
  validation_sql$risk_entry <- paste0(
    "WITH ", paste(risk_ctes[-length(risk_ctes)], collapse = ", "),
    " SELECT COUNT(*) AS n_violations FROM episode_bounds b WHERE ",
    "b.censor_date < b.tar_start_date"
  )
  for (outcome in outcomes) {
    checks <- .survivalEventValidationSql(handle, cohort_table, outcome)
    names(checks) <- paste0("outcome_", outcome$priority, "_", names(checks))
    validation_sql <- c(validation_sql, checks)
  }
  if (tie_policy == "error" && format != "survival") {
    validation_sql$event_ties <- paste0(
      event_prefix,
      " SELECT COUNT(*) AS n_violations FROM (SELECT e.cohort_row_id, ",
      "e.event_date FROM eligible_events e GROUP BY e.cohort_row_id, ",
      "e.event_date HAVING COUNT(*) > 1) tied_events"
    )
  }

  selected_ctes <- character(0)
  selected_source <- "eligible_events"
  if (format != "survival" && tie_policy %in% c("priority", "error")) {
    selected_ctes <- c(selected_ctes, paste0(
      "tie_ranked AS (SELECT e.*, ROW_NUMBER() OVER (PARTITION BY ",
      "e.cohort_row_id, e.event_date ORDER BY e.outcome_priority, ",
      "e.event_key, e.outcome_name) AS dsomop_tie_rank FROM eligible_events e)"
    ), paste0(
      "events_selected AS (SELECT t.* FROM tie_ranked t WHERE ",
      "t.dsomop_tie_rank = 1)"
    ))
    selected_source <- "events_selected"
  }

  output_sql <- NULL
  components <- list()
  columns <- NULL
  if (identical(format, "survival")) {
    survival_order <- if (identical(event_order, "last")) "DESC" else "ASC"
    selected_ctes <- c(selected_ctes, paste0(
      "first_events AS (SELECT e.*, ROW_NUMBER() OVER (PARTITION BY ",
      "e.cohort_row_id, e.outcome_priority ORDER BY e.event_date ",
      survival_order, ", ",
      "e.event_key) AS dsomop_event_rank FROM eligible_events e)"
    ))
    selects <- vapply(outcomes, function(outcome) {
      event_day <- .survivalDateDiff(
        handle, "f.event_date", "e.cohort_start_date"
      )
      followup <- .survivalDateDiff(
        handle, "f.event_date", "e.tar_start_date"
      )
      from <- paste0(
        " FROM risk_episodes e LEFT JOIN first_events f ON ",
        "f.cohort_row_id = e.cohort_row_id AND f.outcome_priority = ",
        outcome$priority, " AND f.dsomop_event_rank = 1"
      )
      if (legacy) {
        paste0(
          "SELECT e.cohort_row_id AS row_id, e.cohort_row_id, ",
          "e.subject_id AS person_id, CASE WHEN f.event_date IS NULL THEN 0 ",
          "ELSE 1 END AS event, CASE WHEN f.event_date IS NULL THEN ",
          .survivalDateDiff(handle, "e.censor_date", "e.tar_start_date"),
          " ELSE ", followup, " END AS time_to_event_days", from
        )
      } else {
        paste0(
          "SELECT e.cohort_row_id AS row_id, e.cohort_row_id, ",
          "e.subject_id AS person_id, ",
          .quoteLiteral(outcome$name, handle), " AS outcome_name, ",
          "CASE WHEN f.event_date IS NULL THEN 0 ELSE 1 END AS event, ",
          "e.risk_start_days AS entry_days_from_index, CASE WHEN ",
          "f.event_date IS NULL THEN e.risk_end_days ELSE ", event_day,
          " END AS exit_days_from_index, CASE WHEN f.event_date IS NULL THEN ",
          .survivalDateDiff(handle, "e.censor_date", "e.tar_start_date"),
          " ELSE ", followup, " END AS follow_up_days", from
        )
      }
    }, character(1))
    output_sql <- paste0(
      event_prefix, ", ", paste(selected_ctes, collapse = ", "), " ",
      paste(selects, collapse = " UNION ALL "),
      if (legacy) " ORDER BY 2" else " ORDER BY 2, 4"
    )
    columns <- if (legacy) {
      c("row_id", "cohort_row_id", "person_id", "event",
        "time_to_event_days")
    } else {
      c("row_id", "cohort_row_id", "person_id", "outcome_name", "event",
        "entry_days_from_index", "exit_days_from_index", "follow_up_days")
    }
  } else if (identical(format, "competing_risk")) {
    selected_ctes <- c(selected_ctes, paste0(
      "first_event AS (SELECT e.*, ROW_NUMBER() OVER (PARTITION BY ",
      "e.cohort_row_id ORDER BY e.event_date, e.outcome_priority, ",
      "e.event_key, e.outcome_name) AS dsomop_event_rank FROM ",
      selected_source, " e)"
    ))
    event_day <- .survivalDateDiff(
      handle, "f.event_date", "e.cohort_start_date"
    )
    followup <- .survivalDateDiff(handle, "f.event_date", "e.tar_start_date")
    output_sql <- paste0(
      event_prefix, ", ", paste(selected_ctes, collapse = ", "),
      " SELECT e.cohort_row_id AS row_id, e.cohort_row_id, ",
      "e.subject_id AS person_id, f.outcome_name, CASE WHEN f.event_date IS NULL THEN 0 ELSE 1 END ",
      "AS event, e.risk_start_days AS entry_days_from_index, CASE WHEN ",
      "f.event_date IS NULL THEN e.risk_end_days ELSE ", event_day,
      " END AS exit_days_from_index, CASE WHEN f.event_date IS NULL THEN ",
      .survivalDateDiff(handle, "e.censor_date", "e.tar_start_date"),
      " ELSE ", followup, " END AS follow_up_days FROM risk_episodes e ",
      "LEFT JOIN first_event f ON f.cohort_row_id = e.cohort_row_id AND ",
      "f.dsomop_event_rank = 1 ORDER BY e.cohort_row_id"
    )
    columns <- c(
      "row_id", "cohort_row_id", "person_id", "outcome_name", "event",
      "entry_days_from_index", "exit_days_from_index", "follow_up_days"
    )
  } else {
    if (event_order == "first") {
      selected_ctes <- c(selected_ctes, paste0(
        "order_limited AS (SELECT e.*, ROW_NUMBER() OVER (PARTITION BY ",
        "e.cohort_row_id ORDER BY e.event_date, e.outcome_priority, ",
        "e.event_key, e.outcome_name) AS dsomop_order_rank FROM ",
        selected_source, " e)"
      ), paste0(
        "events_ordered AS (SELECT e.* FROM order_limited e WHERE ",
        "e.dsomop_order_rank = 1)"
      ))
    } else {
      selected_ctes <- c(selected_ctes, paste0(
        "events_ordered AS (SELECT e.* FROM ", selected_source, " e)"
      ))
    }
    selected_ctes <- c(selected_ctes, paste0(
      "numbered_events AS (SELECT e.*, ROW_NUMBER() OVER (PARTITION BY ",
      "e.cohort_row_id ORDER BY e.event_date, e.outcome_priority, ",
      "e.event_key, e.outcome_name) AS event_number, ROW_NUMBER() OVER (",
      "PARTITION BY e.cohort_row_id, e.outcome_priority ORDER BY ",
      "e.event_date, e.event_key) AS outcome_event_number FROM ",
      "events_ordered e)"
    ))
    event_day <- .survivalDateDiff(
      handle, "n.event_date", "n.cohort_start_date"
    )
    risk_sql <- paste0(
      "WITH ", paste(risk_ctes, collapse = ", "),
      " SELECT e.cohort_row_id AS row_id, e.cohort_row_id, e.subject_id AS person_id, ",
      "e.risk_start_days AS entry_days_from_index, e.risk_end_days AS ",
      "exit_days_from_index, ",
      .survivalDateDiff(handle, "e.censor_date", "e.tar_start_date"),
      " AS follow_up_days FROM risk_episodes e ORDER BY e.cohort_row_id"
    )
    if (identical(format, "recurrent_events")) {
      output_sql <- paste0(
        event_prefix, ", ", paste(selected_ctes, collapse = ", "),
        " SELECT n.cohort_row_id AS row_id, n.cohort_row_id, n.subject_id AS person_id, ",
        "n.outcome_name, 1 AS event, n.event_number, ",
        "n.outcome_event_number, ", event_day,
        " AS event_days_from_index, n.risk_start_days AS ",
        "entry_days_from_index, n.risk_end_days AS exit_days_from_index ",
        "FROM numbered_events n ORDER BY n.cohort_row_id, n.event_number"
      )
      columns <- c(
        "row_id", "cohort_row_id", "person_id", "outcome_name", "event", "event_number",
        "outcome_event_number", "event_days_from_index",
        "entry_days_from_index", "exit_days_from_index"
      )
      components$risk_sets <- risk_sql
    } else {
      selected_ctes <- c(selected_ctes, paste0(
        "event_intervals AS (SELECT n.*, COALESCE(LAG(", event_day,
        ") OVER (PARTITION BY n.cohort_row_id ORDER BY n.event_number), ",
        "n.risk_start_days - 1) AS interval_start_days, ", event_day,
        " AS interval_end_days FROM numbered_events n)"
      ), paste0(
        "event_summary AS (SELECT n.cohort_row_id, MAX(n.event_number) AS ",
        "last_event_number, MAX(", event_day,
        ") AS last_event_day FROM numbered_events n GROUP BY n.cohort_row_id)"
      ))
      event_rows <- paste0(
        "SELECT i.cohort_row_id AS row_id, i.cohort_row_id, i.subject_id AS person_id, i.outcome_name, ",
        "1 AS event, i.event_number AS interval_number, ",
        "i.interval_start_days, i.interval_end_days FROM event_intervals i"
      )
      tail_where <- if (event_order == "first") {
        " WHERE s.last_event_number IS NULL"
      } else {
        " WHERE s.last_event_day IS NULL OR s.last_event_day < e.risk_end_days"
      }
      tail_rows <- paste0(
        "SELECT e.cohort_row_id AS row_id, e.cohort_row_id, e.subject_id AS person_id, NULL AS ",
        "outcome_name, 0 AS event, COALESCE(s.last_event_number, 0) + 1 AS ",
        "interval_number, COALESCE(s.last_event_day, e.risk_start_days - 1) AS ",
        "interval_start_days, e.risk_end_days AS interval_end_days FROM ",
        "risk_episodes e LEFT JOIN event_summary s ON s.cohort_row_id = ",
        "e.cohort_row_id", tail_where
      )
      output_sql <- paste0(
        event_prefix, ", ", paste(selected_ctes, collapse = ", "), " ",
        event_rows, " UNION ALL ", tail_rows,
        " ORDER BY 2, 6"
      )
      columns <- c(
        "row_id", "cohort_row_id", "person_id", "outcome_name", "event",
        "interval_number", "interval_start_days", "interval_end_days"
      )
    }
  }

  components$primary <- output_sql
  structure(list(
    format = format,
    sql = output_sql,
    components = components,
    validation_sql = validation_sql,
    population_gate_sql = paste0(
      "SELECT COUNT(DISTINCT subject_id) AS n_persons FROM ", cohort_table
    ),
    columns = columns,
    semantics = list(
      grain = if (identical(format, "survival")) {
        "episode_outcome"
      } else if (identical(format, "competing_risk")) {
        "episode"
      } else {
        "episode_event"
      },
      recurrent_episodes = TRUE,
      event_order = event_order,
      tie_policy = tie_policy,
      outcome_priority = vapply(outcomes, `[[`, character(1), "name"),
      tar_start_offset = tar$start_offset,
      tar_end_offset = tar$end_offset,
      washout_days = washout_days,
      censoring = list(
        cohort_end = TRUE,
        observation_period_end = censoring$observation_period_end,
        death = censoring$death,
        administrative = !is.null(admin_expression)
      ),
      date_output = "integer_offsets_only",
      internal_person_id = TRUE,
      source_event_identifiers_output = FALSE,
      interval_convention = if (format == "counting_process") {
        paste0(
          "(start, stop] over integer day-end boundaries; the TAR start day ",
          "begins at start_offset - 1"
        )
      } else {
        NULL
      }
    ),
    dbms = dbms,
    legacy = legacy
  ), class = "dsomop_longitudinal_sql")
}

# Normalize both the historical single-outcome plan shape and the advanced
# named-outcomes shape into the SQL compiler contract.
.compilePlanSurvivalSql <- function(handle, cohort_table, output,
                                    custom_filters = NULL) {
  if (!is.list(output)) {
    stop("Survival plan output must be a list.", call. = FALSE)
  }
  legacy <- is.null(output$outcomes)
  outcomes <- if (legacy) {
    if (is.null(output$outcome)) {
      stop("Legacy survival output is missing outcome.", call. = FALSE)
    }
    list(outcome = output$outcome)
  } else {
    output$outcomes
  }
  if (!is.null(custom_filters) && length(custom_filters) > 0L) {
    outcomes <- lapply(outcomes, function(outcome) {
      outcome$filters <- if (is.null(outcome$filters)) {
        custom_filters
      } else {
        list(and = list(outcome$filters, custom_filters))
      }
      outcome
    })
  }
  censoring <- output$censoring
  if (legacy && is.null(censoring)) {
    # Old serialized plans did not carry an explicit censoring field. They must
    # still stop at the end of the observation period containing the index;
    # otherwise an outcome after a gap in observation is treated as continuous
    # follow-up. Death remains opt-in for the historical single-outcome shape.
    censoring <- list(observation_period_end = TRUE, death = FALSE)
  }
  .compileLongitudinalSurvivalSql(
    handle = handle,
    cohort_table = cohort_table,
    outcomes = outcomes,
    tar = output$tar,
    censoring = censoring,
    format = output$format %||% "survival",
    event_order = output$event_order,
    washout_days = output$washout_days %||% 0L,
    tie_policy = output$tie_policy %||% "priority",
    legacy = legacy
  )
}

# Run every data-quality/disclosure precondition before either DBI fetch or
# incremental staging. Keeping this separate prevents a caller from streaming
# the SQL while accidentally omitting its fail-closed validations.
.validateLongitudinalSurvivalSql <- function(handle, compiled) {
  if (!inherits(compiled, "dsomop_longitudinal_sql") ||
      !is.character(compiled$sql) || length(compiled$sql) != 1L) {
    stop("compiled must be a dsomop_longitudinal_sql contract.",
         call. = FALSE)
  }
  .assertMinPersons(handle = handle, sql = compiled$population_gate_sql)
  for (name in names(compiled$validation_sql)) {
    result <- .executeQuery(handle, compiled$validation_sql[[name]])
    if (nrow(result) != 1L || !"n_violations" %in% names(result) ||
        is.na(result$n_violations[1L])) {
      stop("Longitudinal validation '", name,
           "' returned an invalid result.", call. = FALSE)
    }
    if (as.numeric(result$n_violations[1L]) > 0) {
      stop("Longitudinal validation failed: ", name, ".", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Execute a compiled longitudinal SQL contract with its safety validations
#'
#' @keywords internal
.executeLongitudinalSurvivalSql <- function(handle, compiled) {
  .validateLongitudinalSurvivalSql(handle, compiled)
  if (identical(compiled$format, "recurrent_events")) {
    return(list(
      events = .executeQuery(handle, compiled$sql),
      risk_sets = .executeQuery(handle, compiled$components$risk_sets)
    ))
  }
  .executeQuery(handle, compiled$sql)
}
