# Module: Extraction Engine
# SQL generation, data extraction, and feature engineering for OMOP CDM tables.

# --- Temporal Filtering ---

.isoDate <- function(x, label) {
  if (length(x) != 1L || is.na(x)) {
    stop(label, " must be one ISO date (YYYY-MM-DD).", call. = FALSE)
  }
  value <- as.character(x)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)) {
    stop(label, " must be an ISO date (YYYY-MM-DD).", call. = FALSE)
  }
  parsed <- suppressWarnings(as.Date(value, format = "%Y-%m-%d"))
  if (is.na(parsed) || format(parsed, "%Y-%m-%d") != value) {
    stop(label, " is not a valid date.", call. = FALSE)
  }
  parsed
}

.extractionCap <- function(option, default) {
  value <- suppressWarnings(as.numeric(getOption(
    option, getOption(paste0("default.", option), default)
  )))
  if (length(value) != 1L || is.na(value) || !is.finite(value) ||
      value != floor(value) || value < 1L) {
    stop(option, " must be one positive integer.", call. = FALSE)
  }
  value
}

.filterComplexityVisit <- function(state = NULL, depth = 1L,
                                   values = 0L) {
  if (is.null(state)) {
    state <- new.env(parent = emptyenv())
    state$nodes <- 0
    state$values <- 0
    state$max_depth <- .extractionCap("dsomop.max_filter_depth", 32L)
    state$max_nodes <- .extractionCap("dsomop.max_filter_nodes", 1024L)
    state$max_values <- .extractionCap("dsomop.max_filter_values", 10000L)
  }
  depth <- suppressWarnings(as.numeric(depth))
  values <- suppressWarnings(as.numeric(values))
  if (length(depth) != 1L || !is.finite(depth) || depth < 1L ||
      depth != floor(depth) || length(values) != 1L || !is.finite(values) ||
      values < 0L || values != floor(values)) {
    stop("Invalid filter complexity accounting state.", call. = FALSE)
  }
  state$nodes <- state$nodes + 1
  state$values <- state$values + values
  if (depth > state$max_depth) {
    stop("Filter tree exceeds the server max_filter_depth cap of ",
         state$max_depth, ".", call. = FALSE)
  }
  if (state$nodes > state$max_nodes) {
    stop("Filter tree exceeds the server max_filter_nodes cap of ",
         state$max_nodes, ".", call. = FALSE)
  }
  if (state$values > state$max_values) {
    stop("Filter tree exceeds the server max_filter_values cap of ",
         state$max_values, ".", call. = FALSE)
  }
  state
}

.validateDateBounds <- function(start = NULL, end = NULL, context) {
  parsed_start <- if (!is.null(start)) .isoDate(start, paste0(context, "$start"))
  parsed_end <- if (!is.null(end)) .isoDate(end, paste0(context, "$end"))
  if (!is.null(parsed_start) && !is.null(parsed_end)) {
    if (parsed_start > parsed_end) {
      stop(context, " start must not be after end.", call. = FALSE)
    }
    settings <- .omopDisclosureSettings()
    inclusive_days <- as.numeric(parsed_end - parsed_start) + 1
    if (settings$nfilter_subset > 0 &&
        inclusive_days < settings$nfilter_date_range) {
      stop(context, " must span at least ", settings$nfilter_date_range,
           " days while disclosure filtering ",
           "is enabled.", call. = FALSE)
    }
  }
  list(start = parsed_start, end = parsed_end)
}

.temporalOffset <- function(x, field) {
  value <- suppressWarnings(as.integer(x))
  numeric_value <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || length(value) != 1L || is.na(value) ||
      length(numeric_value) != 1L || is.na(numeric_value) ||
      numeric_value != value) {
    stop("temporal$index_window$", field,
         " must be one integer day offset.", call. = FALSE)
  }
  value
}

.normalizeMinGap <- function(min_gap) {
  if (!is.list(min_gap)) min_gap <- list(days = min_gap)
  if (is.null(names(min_gap)) || any(!nzchar(names(min_gap))) ||
      anyDuplicated(names(min_gap))) {
    stop("temporal$min_gap must be one integer or a named policy.",
         call. = FALSE)
  }
  unknown <- setdiff(names(min_gap), c("days", "by", "keep"))
  if (length(unknown) > 0L) {
    stop("Unknown temporal$min_gap field(s): ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  days <- suppressWarnings(as.numeric(min_gap$days))
  integer_days <- suppressWarnings(as.integer(min_gap$days))
  if (length(days) != 1L || !is.finite(days) ||
      length(integer_days) != 1L || is.na(integer_days) ||
      days != integer_days || integer_days < 1L) {
    stop("temporal$min_gap$days must be one positive integer.",
         call. = FALSE)
  }
  by <- min_gap$by %||% "concept"
  keep <- min_gap$keep %||% "first"
  if (!is.character(by) || length(by) != 1L || is.na(by) ||
      !tolower(by) %in% c("grain", "concept")) {
    stop("temporal$min_gap$by must be grain or concept.", call. = FALSE)
  }
  if (!is.character(keep) || length(keep) != 1L || is.na(keep) ||
      !tolower(keep) %in% c("first", "last")) {
    stop("temporal$min_gap$keep must be first or last.", call. = FALSE)
  }
  list(days = integer_days, by = tolower(by), keep = tolower(keep))
}

.validateTemporalSpec <- function(temporal) {
  if (is.null(temporal)) return(invisible(TRUE))
  if (!is.list(temporal) || length(temporal) == 0L || is.null(names(temporal)) ||
      any(!nzchar(names(temporal))) || anyDuplicated(names(temporal))) {
    stop("temporal must be a non-empty named specification.", call. = FALSE)
  }
  allowed <- c("index_window", "calendar", "event_select", "min_gap")
  unknown <- setdiff(names(temporal), allowed)
  if (length(unknown) > 0L) {
    stop("Unknown temporal field(s): ", paste(unknown, collapse = ", "), ".",
         call. = FALSE)
  }
  supplied_null <- names(temporal)[vapply(temporal, is.null, logical(1))]
  if (length(supplied_null) > 0L) {
    stop("Temporal block(s) cannot be NULL when supplied: ",
         paste(supplied_null, collapse = ", "), ".", call. = FALSE)
  }
  if ("min_gap" %in% names(temporal)) .normalizeMinGap(temporal$min_gap)

  validate_bounds_block <- function(block, name) {
    if (!is.list(block) || is.null(names(block)) ||
        any(!nzchar(names(block))) || anyDuplicated(names(block))) {
      stop("temporal$", name, " must be a named list.", call. = FALSE)
    }
    unknown <- setdiff(names(block), c("start", "end"))
    if (length(unknown) > 0L) {
      stop("Unknown temporal$", name, " field(s): ",
           paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
    if (is.null(block$start) && is.null(block$end)) {
      stop("temporal$", name, " must contain start and/or end.",
           call. = FALSE)
    }
  }
  if (!is.null(temporal$index_window)) {
    validate_bounds_block(temporal$index_window, "index_window")
  }
  if (!is.null(temporal$calendar)) {
    validate_bounds_block(temporal$calendar, "calendar")
  }
  if (!is.null(temporal$event_select)) {
    es <- temporal$event_select
    if (!is.list(es) || is.null(names(es)) || any(!nzchar(names(es))) ||
        anyDuplicated(names(es))) {
      stop("temporal$event_select must be a named list.", call. = FALSE)
    }
    unknown <- setdiff(names(es), c("order", "n", "by"))
    if (length(unknown) > 0L) {
      stop("Unknown or unsupported temporal$event_select field(s): ",
           paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
    if (!is.character(es$order) || length(es$order) != 1L ||
        is.na(es$order) || !tolower(es$order) %in% c("first", "last")) {
      stop("temporal$event_select$order must be first or last.",
           call. = FALSE)
    }
    by <- es$by %||% "grain"
    if (!is.character(by) || length(by) != 1L || is.na(by) ||
        !tolower(by) %in% c("grain", "concept")) {
      stop("temporal$event_select$by must be grain or concept.",
           call. = FALSE)
    }
    n <- es$n %||% 1L
    numeric_n <- suppressWarnings(as.numeric(n))
    integer_n <- suppressWarnings(as.integer(n))
    if (length(n) != 1L || length(numeric_n) != 1L ||
        !is.finite(numeric_n) || length(integer_n) != 1L ||
        is.na(integer_n) || numeric_n != integer_n || integer_n < 1L) {
      stop("temporal$event_select$n must be one positive integer.",
           call. = FALSE)
    }
    max_n <- suppressWarnings(as.numeric(getOption("dsomop.max_event_select_n", 100L)))
    if (length(max_n) != 1L || is.na(max_n) || !is.finite(max_n) ||
        max_n != floor(max_n) || max_n < 1L) {
      stop("dsomop.max_event_select_n must be one positive integer.",
           call. = FALSE)
    }
    if (integer_n > max_n) {
      stop("temporal$event_select$n exceeds the server cap of ", max_n, ".",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

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
  if (is.null(temporal)) return(character(0))
  .validateTemporalSpec(temporal)
  where <- character(0)

  if (!is.null(temporal$min_gap)) {
    if (is.null(date_col)) {
      stop("min_gap was supplied, but the table has no usable date column.",
           call. = FALSE)
    }
    where <- c(where, paste0(alias, ".", date_col, " IS NOT NULL"))
  }

  # Index-relative window: days relative to cohort_start_date
  if (!is.null(temporal$index_window)) {
    if (is.null(date_col)) {
      stop("An index_window was supplied, but the table has no usable date ",
           "column.", call. = FALSE)
    }
    iw <- temporal$index_window
    if (!is.list(iw) || (is.null(iw$start) && is.null(iw$end))) {
      stop("temporal$index_window must contain start and/or end.",
           call. = FALSE)
    }
    iw_start <- if (!is.null(iw$start)) .temporalOffset(iw$start, "start")
    iw_end <- if (!is.null(iw$end)) .temporalOffset(iw$end, "end")
    if (!is.null(iw_start) && !is.null(iw_end) && iw_start > iw_end) {
      stop("temporal$index_window start must not be after end.",
           call. = FALSE)
    }
    if (!is.null(iw$start)) {
      where <- c(where, paste0(
        alias, ".", date_col, " >= ",
        .renderSql(handle, "DATEADD(day, @days, c.cohort_start_date)",
                   days = iw_start)
      ))
    }
    if (!is.null(iw$end)) {
      where <- c(where, paste0(
        alias, ".", date_col, " < ",
        .renderSql(handle, "DATEADD(day, @days, c.cohort_start_date)",
                   days = as.double(iw_end) + 1)
      ))
    }
  }

  # Calendar time filter
  if (!is.null(temporal$calendar)) {
    if (is.null(date_col)) {
      stop("A calendar window was supplied, but the table has no usable date ",
           "column.", call. = FALSE)
    }
    cal <- temporal$calendar
    if (!is.list(cal) || (is.null(cal$start) && is.null(cal$end))) {
      stop("temporal$calendar must contain start and/or end.", call. = FALSE)
    }
    bounds <- .validateDateBounds(cal$start, cal$end, "temporal$calendar")
    cal_start <- bounds$start
    cal_end <- bounds$end
    if (!is.null(cal$start)) {
      where <- c(where, paste0(
        alias, ".", date_col, " >= ",
        .quoteLiteral(as.character(cal_start), handle)
      ))
    }
    if (!is.null(cal$end)) {
      where <- c(where, paste0(
        alias, ".", date_col, " < ",
        .quoteLiteral(as.character(cal_end + 1L), handle)
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
#' @param tie_col Character; optional stable tie-break column
#' @return Character; possibly wrapped SQL
#' @keywords internal
.wrapEventSelect <- function(handle, sql, temporal, date_col = NULL,
                             tie_col = NULL) {
  .validateTemporalSpec(temporal)
  if (is.null(temporal$event_select)) return(sql)
  if (is.null(date_col)) {
    stop("event_select was supplied, but the table has no usable date column.",
         call. = FALSE)
  }

  es <- temporal$event_select
  order_dir <- if (identical(tolower(es$order), "last")) "DESC" else "ASC"
  n <- as.integer(es$n %||% 1L)

  partition_col <- if (!is.null(temporal$index_window)) {
    "cohort_row_id"
  } else {
    "person_id"
  }
  if (identical(tolower(es$by %||% "grain"), "concept")) {
    partition_col <- paste(
      partition_col, "dsomop_event_partition_concept", sep = ", "
    )
  }
  order_terms <- paste0(date_col, " ", order_dir)
  if (!is.null(tie_col)) {
    order_terms <- paste0(order_terms, ", ", tie_col, " ASC")
  }
  rn_expr <- paste0("ROW_NUMBER() OVER (PARTITION BY ", partition_col,
                     " ORDER BY ", order_terms, ")")

  paste0(
    "SELECT * FROM (SELECT sub.*, ", rn_expr, " AS rn ",
    "FROM (", sql, ") AS sub) AS ranked WHERE ranked.rn <= ", n
  )
}

#' Collapse temporally adjacent events into deterministic episodes
#'
#' Events are chained when the next event occurs no more than \code{days} after
#' the previous one. Collapse happens independently by person/cohort episode
#' and, by default, concept. One source row represents each chain; ties are
#' broken with the canonical OMOP event primary key.
#'
#' @param handle CDM handle.
#' @param sql Character base SQL query.
#' @param temporal Temporal specification containing \code{min_gap}.
#' @param date_col Internal event-date alias.
#' @param tie_col Internal stable primary-key alias.
#' @return Character wrapped SQL query.
#' @keywords internal
.wrapMinGap <- function(handle, sql, temporal, date_col = NULL,
                        tie_col = NULL) {
  .validateTemporalSpec(temporal)
  if (is.null(temporal$min_gap)) return(sql)
  if (is.null(date_col)) {
    stop("min_gap was supplied, but the table has no usable date column.",
         call. = FALSE)
  }
  if (is.null(tie_col)) {
    stop("min_gap requires a standard OMOP event primary key for ",
         "deterministic collapse.", call. = FALSE)
  }

  policy <- .normalizeMinGap(temporal$min_gap)
  partition <- if (!is.null(temporal$index_window)) {
    "cohort_row_id"
  } else {
    "person_id"
  }
  if (identical(policy$by, "concept")) {
    partition <- paste(partition, "dsomop_event_partition_concept", sep = ", ")
  }
  ascending <- paste0(date_col, " ASC, ", tie_col, " ASC")
  representative <- paste0(
    date_col, if (identical(policy$keep, "last")) " DESC" else " ASC",
    ", ", tie_col, " ASC"
  )
  previous_date <- paste0(
    "LAG(", date_col, ") OVER (PARTITION BY ", partition,
    " ORDER BY ", ascending, ")"
  )
  gap_limit <- .renderSql(
    handle, "DATEADD(day, @days, lagged.dsomop_gap_previous_date)",
    days = policy$days
  )

  paste0(
    "SELECT * FROM (SELECT grouped.*, ROW_NUMBER() OVER (PARTITION BY ",
    partition, ", dsomop_gap_group ORDER BY ", representative,
    ") AS dsomop_gap_row FROM (SELECT marked.*, SUM(dsomop_gap_new) OVER (",
    "PARTITION BY ", partition, " ORDER BY ", ascending,
    " ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS dsomop_gap_group ",
    "FROM (SELECT lagged.*, CASE WHEN dsomop_gap_previous_date IS NULL OR ",
    date_col, " > ", gap_limit,
    " THEN 1 ELSE 0 END AS dsomop_gap_new FROM (SELECT sub.*, ",
    previous_date, " AS dsomop_gap_previous_date FROM (", sql,
    ") AS sub) AS lagged) AS marked) AS grouped) AS collapsed ",
    "WHERE collapsed.dsomop_gap_row = 1"
  )
}

#' Resolve the standard OMOP primary key used to break same-date event ties
#'
#' OMOP clinical event tables conventionally use <table>_id as their primary
#' key. PERSON and DEATH are the two person-keyed exceptions relevant here.
#'
#' @param blueprint Schema blueprint.
#' @param table Character table name.
#' @return Character primary-key column, or NULL when the table has none.
#' @keywords internal
.eventPrimaryKeyColumn <- function(blueprint, table) {
  table <- tolower(table)
  cols <- blueprint$columns[[table]]$column_name %||% character(0)
  candidates <- c(paste0(table, "_id"),
                  if (table %in% c("person", "death")) "person_id")
  hit <- intersect(candidates, cols)
  if (length(hit) > 0L) hit[1] else NULL
}

#' Build the canonical, deduplicated cohort-episode relation
#'
#' The generated cohort_row_id is stable for one cohort snapshot and is shared
#' by event-level and temporal-covariate outputs. Exact duplicate standard
#' cohort eras are one episode. Index-event cohorts retain their source event
#' key, so distinct events with identical dates remain distinct episodes.
#'
#' @param cohort_table Character cohort table name.
#' @param handle Optional CDM handle used to inspect whether the internal
#'   index-event key is present.
#' @return Character SQL for a derived cohort table.
#' @keywords internal
.rankedCohortSql <- function(cohort_table, handle = NULL) {
  cohort_table <- .validateIdentifier(cohort_table, "cohort table")
  fields <- if (is.null(handle)) {
    character(0)
  } else {
    names(.executeQuery(
      handle, paste0("SELECT * FROM ", cohort_table, " WHERE 1 = 0")
    ))
  }
  episode_key <- if ("dsomop_episode_key" %in% fields) {
    "dsomop_episode_key"
  } else if ("index_event_id" %in% fields) {
    "index_event_id"
  } else {
    NULL
  }
  key_select <- if (!is.null(episode_key)) {
    paste0(", cohort_base.", episode_key, " AS dsomop_episode_key")
  } else {
    ""
  }
  key_source <- if (!is.null(episode_key)) paste0(", ", episode_key) else ""
  key_order <- if (!is.null(episode_key)) {
    paste0(", cohort_base.", episode_key)
  } else {
    ""
  }
  paste0(
    "(SELECT cohort_base.subject_id, cohort_base.cohort_start_date, ",
    "cohort_base.cohort_end_date", key_select,
    ", ROW_NUMBER() OVER (ORDER BY ",
    "cohort_base.subject_id, cohort_base.cohort_start_date, ",
    "cohort_base.cohort_end_date", key_order,
    ") AS cohort_row_id FROM (",
    "SELECT DISTINCT subject_id, cohort_start_date, cohort_end_date",
    key_source, " FROM ",
    cohort_table, ") AS cohort_base)"
  )
}

#' Normalize a date_handling argument to a \code{list(mode = ...)} spec
#'
#' The recipe/options layer may carry \code{date_handling} as a bare string
#' (e.g. \code{"relative_to_index"}) rather than the internal
#' \code{list(mode = ...)} form. Coerce a scalar string into that list, and map
#' the public synonym \code{"relative_to_index"} onto the internal
#' \code{"relative"} mode (days-from-index). A list is passed through unchanged
#' (with the same synonym mapping applied to its \code{mode}). NULL stays NULL so
#' callers can apply their own default.
#'
#' @param date_handling NULL, a character scalar, or a list with \code{$mode}
#' @return NULL or a normalized \code{list(mode = ...)} (extra fields preserved)
#' @keywords internal
.normalizeDateHandling <- function(date_handling) {
  if (is.null(date_handling)) return(NULL)
  if (is.character(date_handling) && length(date_handling) == 1) {
    date_handling <- list(mode = date_handling)
  }
  if (!is.list(date_handling) || is.null(names(date_handling)) ||
      any(!nzchar(names(date_handling))) || anyDuplicated(names(date_handling))) {
    stop("date_handling must be a mode string or a list with $mode.",
         call. = FALSE)
  }
  unknown <- setdiff(names(date_handling),
                     c("mode", "reference", "bin_width", "date_columns"))
  if (length(unknown) > 0L) {
    stop("Unknown date_handling field(s): ", paste(unknown, collapse = ", "),
         ".", call. = FALSE)
  }
  mode <- date_handling$mode
  if (!is.character(mode) || length(mode) != 1L || is.na(mode) ||
      !nzchar(mode)) {
    stop("date_handling$mode must be one of absolute, remove, relative, or ",
         "binned.", call. = FALSE)
  }
  mode <- tolower(mode)
  if (identical(mode, "relative_to_index")) mode <- "relative"
  if (!mode %in% c("absolute", "remove", "relative", "binned")) {
    stop("Unknown date_handling mode: '", mode, "'.", call. = FALSE)
  }
  date_handling$mode <- mode

  reference <- date_handling$reference %||% "index"
  if (!is.character(reference) || length(reference) != 1L ||
      is.na(reference) || !identical(tolower(reference), "index")) {
    stop("date_handling$reference must be 'index'.", call. = FALSE)
  }
  date_handling$reference <- "index"

  if (!is.null(date_handling$date_columns) &&
      (!is.character(date_handling$date_columns) ||
       anyNA(date_handling$date_columns))) {
    stop("date_handling$date_columns must be a character vector.",
         call. = FALSE)
  }
  if (identical(mode, "binned")) {
    bin_width <- date_handling$bin_width
    if (!is.character(bin_width) || length(bin_width) != 1L ||
        is.na(bin_width) || !tolower(bin_width) %in% c("year", "month", "week")) {
      stop("date_handling$bin_width is required for binned mode and must be ",
           "year, month, or week.",
           call. = FALSE)
    }
    date_handling$bin_width <- tolower(bin_width)
  } else if (!is.null(date_handling$bin_width)) {
    stop("date_handling$bin_width is only valid for binned mode.",
         call. = FALSE)
  }
  date_handling
}

#' Apply date handling transforms to a result data frame
#'
#' @param df Data frame
#' @param date_handling List with at least \code{$mode}. Modes:
#'   \describe{
#'     \item{\code{"remove"}}{(Default, safest) Strips all date/datetime
#'       columns entirely. No temporal information leaves the server.}
#'     \item{\code{"relative"}}{Converts dates to integer days relative to
#'       a reference date (\code{cohort_start_date} or custom). Safe if
#'       the reference date itself is not leaked to the client.}
#'     \item{\code{"binned"}}{Truncates dates to year/month/week. Reduces
#'       temporal precision while preserving broad trends.}
#'     \item{\code{"absolute"}}{Returns raw dates unchanged.
#'       \strong{Requires server authorization}:
#'       \code{dsomop.allow_absolute_dates = TRUE}. Raw dates are
#'       quasi-identifiers per OMOP Privacy Guidance.}
#'   }
#' @param index_date_col Character; column with index dates (for relative mode)
#' @return Transformed data frame
#' @keywords internal
.applyDateHandling <- function(df, date_handling, index_date_col = NULL) {
  date_handling <- .normalizeDateHandling(date_handling)
  if (is.null(date_handling)) return(df)

  mode <- date_handling$mode
  if (mode == "absolute") return(df)

  # Identify every date-like column. A caller-supplied subset is only safe when
  # raw dates have been explicitly authorized; otherwise omitted date columns
  # would leave exact dates in an ostensibly transformed result.
  date_like <- vapply(df, function(x) inherits(x, c("Date", "POSIXt")),
                      logical(1))
  all_date_columns <- unique(c(
    grep("_date$|_datetime$", names(df), value = TRUE),
    names(df)[date_like],
    intersect(index_date_col %||% character(0), names(df))
  ))
  requested <- date_handling$date_columns
  if (is.null(requested)) {
    date_columns <- all_date_columns
  } else {
    unknown <- setdiff(requested, names(df))
    if (length(unknown) > 0) {
      stop("Unknown date_handling$date_columns: ",
           paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
    not_dates <- setdiff(requested, all_date_columns)
    if (length(not_dates) > 0) {
      stop("date_handling$date_columns includes non-date column(s): ",
           paste(not_dates, collapse = ", "), ".", call. = FALSE)
    }
    date_columns <- unique(requested)
    allow_absolute <- getOption("dsomop.allow_absolute_dates",
      getOption("default.dsomop.allow_absolute_dates", FALSE))
    omitted <- setdiff(all_date_columns, date_columns)
    if (length(omitted) > 0 && !isTRUE(allow_absolute)) {
      stop("date_handling$date_columns omits date column(s) while absolute ",
           "dates are disabled: ", paste(omitted, collapse = ", "), ".",
           call. = FALSE)
    }
  }
  if (length(date_columns) == 0) return(df)

  as_date <- function(x, column) {
    timezone <- .omopDisclosureSettings()$datetime_timezone
    parsed <- tryCatch({
      if (inherits(x, "POSIXt")) as.Date(x, tz = timezone) else as.Date(x)
    }, error = function(e) NULL)
    if (is.null(parsed) || any(!is.na(x) & is.na(parsed))) {
      stop("Could not safely convert date column '", column, "'.",
           call. = FALSE)
    }
    parsed
  }

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
    if (is.null(ref_col)) {
      stop("Relative date handling requires an index date column.",
           call. = FALSE)
    }
    if (nrow(df) == 0L) {
      for (col in date_columns) df[[col]] <- integer(0)
      return(df)
    }
    ref_dates <- as_date(df[[ref_col]], ref_col)
    for (col in date_columns) {
      if (col == ref_col) next
      col_dates <- as_date(df[[col]], col)
      df[[col]] <- as.integer(col_dates - ref_dates)
    }
    # The reference itself is safe only as a relative zero, never as a date.
    if (ref_col %in% date_columns) {
      df[[ref_col]] <- 0L
    } else {
      allow_absolute <- getOption("dsomop.allow_absolute_dates",
        getOption("default.dsomop.allow_absolute_dates", FALSE))
      if (!isTRUE(allow_absolute)) {
        stop("Relative date handling cannot leave its index date untransformed.",
             call. = FALSE)
      }
    }
    return(df)
  }

  if (mode == "binned") {
    bin_width <- date_handling$bin_width %||% "month"
    if (nrow(df) == 0L) {
      for (col in date_columns) df[[col]] <- character(0)
      return(df)
    }
    for (col in date_columns) {
      col_dates <- as_date(df[[col]], col)
      df[[col]] <- switch(bin_width,
        "year"  = format(col_dates, "%Y-01-01"),
        "month" = format(col_dates, "%Y-%m-01"),
        "week"  = {
          # Truncate to start of week (Monday)
          wday <- as.integer(format(col_dates, "%u"))
          as.character(col_dates - (wday - 1L))
        }
      )
    }
    return(df)
  }
}

# --- SQL Compilation ---

#' Quote a SQL literal value safely
#'
#' @param x Scalar value to quote.
#' @param handle Optional CDM handle. Production SQL builders must supply it so
#'   the active DBI driver applies the correct escaping rules (notably for
#'   MySQL/MariaDB backslash escapes). NULL uses ANSI quoting for isolated
#'   rendering tests only.
#' @return Character; quoted SQL literal
#' @keywords internal
.quoteLiteral <- function(x, handle = NULL) {
  if (length(x) != 1L || is.na(x)) {
    stop("SQL literals must be one non-missing scalar value.", call. = FALSE)
  }
  conn <- if (is.null(handle)) DBI::ANSI() else .conn(handle)
  as.character(DBI::dbQuoteLiteral(conn, x))
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
#' @param filters List; an optional custom filter DSL tree (AND/OR of leaves,
#'   see \code{\link{.compileFilter}}). Validated fail-closed by
#'   \code{\link{.assertCustomFilterSafe}} and ANDed with the other predicates.
#' @param concept_col Character; optional override of the column the
#'   \code{concept_filter} IN-list scopes (default: the table's domain concept
#'   column). Lets a caller scope by \code{unit_concept_id},
#'   \code{*_type_concept_id}, or \code{value_as_concept_id}.
#' @param visit_filter List; optional \code{list(concept_ids = ...)} restricting
#'   events to visits of those \code{visit_concept_id} values via a join on
#'   \code{visit_occurrence_id} to \code{visit_occurrence}.
#' @param add_event_order_id Logical; include the source table's OMOP primary
#'   key under an internal alias for deterministic in-memory ordering. The alias
#'   is removed before any result is returned.
#' @return Character; compiled SQL statement
#' @keywords internal
.compileSelect <- function(handle, table, columns = NULL,
                           concept_filter = NULL, person_ids = NULL,
                           time_window = NULL, cohort_table = NULL,
                           limit = NULL, block_sensitive = TRUE,
                           temporal = NULL, add_cohort_date = FALSE,
                           filters = NULL, concept_col = NULL,
                           visit_filter = NULL,
                           add_event_order_id = FALSE) {
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

  # Concept scoping column: the domain concept by default, but a caller may
  # override it to surface/aggregate by another concept column on the same table
  # (e.g. unit_concept_id, *_type_concept_id, value_as_concept_id) for
  # unit-harmonization extraction or value-by-unit/type distributions.
  domain_concept_col <- .getDomainConceptColumn(bp, table_lower)
  if (is.null(concept_col)) {
    concept_col <- domain_concept_col
  } else {
    concept_col <- tolower(.validateIdentifier(concept_col, "concept column"))
    if (!concept_col %in% col_df$column_name) {
      stop("Concept column '", concept_col, "' not found in table '", table,
           "'.", call. = FALSE)
    }
  }
  has_concept_col <- !is.null(concept_col) && concept_col %in% col_df$column_name
  has_person_id <- "person_id" %in% col_df$column_name
  safe_non_person_tables <- c(
    "cdm_source", "metadata", "cohort_definition", "achilles_analysis"
  )
  is_safe_non_person <- identical(tbl_row$schema_category[1], "Vocabulary") ||
    table_lower %in% safe_non_person_tables
  if (!has_person_id && !is_safe_non_person) {
    stop("Row-level extraction of non-person-keyed table '", table,
         "' is not permitted. A declared person join path and a distinct-",
         "person disclosure gate are required.", call. = FALSE)
  }

  # Which column does the concept_set (concept_id list) match on? Normally the
  # concept_col, so a caller can scope the set against an alternate concept column
  # (e.g. concept_filter = 9529 with concept_col = unit_concept_id selects rows in
  # that unit). BUT when the override column is a SURFACING choice — it is also a
  # requested output column OR a custom filter independently scopes it — the
  # concept_set values are DOMAIN concepts (e.g. glucose 3004501 surfaced by
  # unit/route, with a unit/route filter): matching a domain concept on, say,
  # route_concept_id would select nothing, so the set matches on the domain
  # concept column instead. The override column is still selected/filtered. With
  # no surfacing signal (concept_col is the genuine scoping column) this is a
  # no-op, preserving the concept_col-as-scope contract.
  surfacing_override <- !is.null(domain_concept_col) &&
    !identical(concept_col, domain_concept_col) &&
    ((!is.null(columns) && tolower(concept_col) %in% tolower(columns)) ||
     (!is.null(filters) && .filterTreeReferencesColumn(filters, concept_col)))
  concept_filter_col <- if (surfacing_override) domain_concept_col else concept_col
  has_concept_filter_col <- !is.null(concept_filter_col) &&
    concept_filter_col %in% col_df$column_name

  if (!is.null(concept_filter)) {
    raw_concepts <- unlist(concept_filter, use.names = FALSE)
    max_values <- .extractionCap("dsomop.max_filter_values", 10000L)
    if (length(raw_concepts) > max_values) {
      stop("concept_filter exceeds the server max_filter_values cap of ",
           max_values, ".", call. = FALSE)
    }
    numeric_concepts <- suppressWarnings(as.numeric(raw_concepts))
    integer_concepts <- suppressWarnings(as.integer(raw_concepts))
    if (length(raw_concepts) == 0L || anyNA(numeric_concepts) ||
        any(!is.finite(numeric_concepts)) || anyNA(integer_concepts) ||
        any(numeric_concepts != integer_concepts)) {
      stop("concept_filter must contain one or more finite integer concept ",
           "IDs.", call. = FALSE)
    }
    if (!has_concept_filter_col) {
      stop("concept_filter was supplied, but table '", table,
           "' has no usable concept column.", call. = FALSE)
    }
    concept_filter <- unique(integer_concepts)
  }

  if (!is.null(person_ids) && !has_person_id) {
    stop("person_ids cannot scope table '", table,
         "' because it has no person_id column.", call. = FALSE)
  }
  if (!is.null(cohort_table) && !has_person_id) {
    stop("cohort_table cannot scope table '", table,
         "' because it has no person_id column or supported join path.",
         call. = FALSE)
  }

  # Determine columns to select
  if (is.null(columns)) {
    select_cols <- col_df$column_name
  } else {
    columns <- tolower(as.character(unlist(columns, use.names = FALSE)))
    missing_columns <- setdiff(columns, col_df$column_name)
    if (length(missing_columns) > 0) {
      stop("Column(s) not found in table '", table, "': ",
           paste(missing_columns, collapse = ", "), ".", call. = FALSE)
    }
    must_keep <- character(0)
    if (has_person_id) must_keep <- c(must_keep, "person_id")
    if (!is.null(concept_filter)) {
      # Keep the column the concept_set is matched on (domain concept column)
      # AND the override surfacing column, so both the WHERE and any per-spec /
      # aggregation step downstream find their columns.
      if (has_concept_filter_col) must_keep <- c(must_keep, concept_filter_col)
      if (has_concept_col) must_keep <- c(must_keep, concept_col)
    }
    select_cols <- unique(c(must_keep, intersect(columns, col_df$column_name)))
  }

  # Extension visibility is not identifier authorization. Unknown *_id/_key/
  # *_identifier fields have no OHDSI or typed dsOMOP semantics and are always
  # denied, including when a controller has enabled sensitive text access.
  # This gate is intentionally separate from block_sensitive so the
  # administrative PII bypass cannot expose local row/member/account keys.
  untyped_identifier_cols <- if (
    "is_untyped_identifier" %in% names(col_df)
  ) {
    col_df$column_name[col_df$is_untyped_identifier]
  } else {
    character(0)
  }
  if (!is.null(columns)) {
    requested_identifiers <- intersect(tolower(columns), untyped_identifier_cols)
    if (length(requested_identifiers) > 0L) {
      stop("Disclosive: extension column(s) '",
           paste(requested_identifiers, collapse = "', '"),
           "' are untyped identifiers and cannot be extracted from table '",
           table, "'.", call. = FALSE)
    }
  }
  select_cols <- setdiff(select_cols, untyped_identifier_cols)

  # PRIVILEGE ESCALATION GATE: block_sensitive = FALSE exposes PII columns
  # (source_value, free text, provider identifiers, geographic data).
  # This is a deliberate server-admin decision, NOT a client preference.
  # Per DataSHIELD model: disclosure controls are governed by server config.
  if (!block_sensitive) {
    allow <- getOption("dsomop.allow_sensitive_columns",
               getOption("default.dsomop.allow_sensitive_columns", FALSE))
    if (!isTRUE(allow)) {
      stop("Accessing sensitive columns is not permitted by the server. ",
           "Contact the data controller to enable dsomop.allow_sensitive_columns.",
           call. = FALSE)
    }
  }
  if (block_sensitive) {
    blocked <- col_df$column_name[col_df$is_blocked]
    # Exact birth components are quasi-identifiers. Age is exposed only through
    # the episode-aware, minimum-width age_group derivation.
    always_block <- c("year_of_birth", "month_of_birth", "day_of_birth",
                      "birth_datetime")
    blocked <- union(blocked, intersect(always_block, col_df$column_name))
    # Fail closed when a blocked column was EXPLICITLY requested (e.g. a feature
    # value_source of value_as_string / *_source_value / sig). Silently dropping
    # it would return a person-id-only frame with no signal that the requested
    # value can never be released; an explicit request must error. Implicit
    # "select all" (columns = NULL) still strips silently, as before.
    if (!is.null(columns)) {
      requested_blocked <- intersect(tolower(columns), blocked)
      if (length(requested_blocked) > 0) {
        stop("Disclosive: column(s) '",
             paste(requested_blocked, collapse = "', '"),
             "' are blocked (free-text / source values) and cannot be ",
             "extracted from table '", table, "'.", call. = FALSE)
      }
    }
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
  has_index_window <- !is.null(temporal$index_window)
  if (has_index_window && (is.null(cohort_table) || !has_person_id)) {
    stop("temporal$index_window requires a cohort table and a person-keyed ",
         "source table.", call. = FALSE)
  }
  if (add_cohort_date && (is.null(cohort_table) || !has_person_id)) {
    stop("add_cohort_date requires a cohort table and a person-keyed source ",
         "table.", call. = FALSE)
  }
  needs_cohort_join <- (has_index_window || add_cohort_date) &&
    !is.null(cohort_table) && has_person_id

  # A cohort may contain multiple eras for one person. Rank the DISTINCT cohort
  # entries once, deterministically, and carry that row id through every
  # index-relative extraction so downstream reductions never collapse eras back
  # to person_id.
  if (needs_cohort_join) {
    select_parts <- paste0(select_parts, ", c.cohort_row_id")
  }
  if (add_cohort_date && needs_cohort_join) {
    select_parts <- paste0(select_parts,
                           ", c.cohort_start_date, c.cohort_end_date")
  }

  # Temporal reduction must be able to order even when the public projection
  # omits the event date. Carry it under an internal alias and remove that alias
  # before either an in-memory result or a staged chunk is exposed.
  has_temporal_reduction <- !is.null(temporal$event_select) ||
    !is.null(temporal$min_gap)
  if (has_temporal_reduction) {
    event_date_col <- .getDateColumn(bp, table_lower)
    if (is.null(event_date_col)) {
      stop("Temporal event reduction requires a usable date column.",
           call. = FALSE)
    }
    select_parts <- paste0(select_parts, ", ", t_alias, ".", event_date_col,
                           " AS dsomop_event_order_date")
    reduce_by_concept <-
      (!is.null(temporal$event_select) &&
       identical(tolower(temporal$event_select$by %||% "grain"), "concept")) ||
      (!is.null(temporal$min_gap) &&
       identical(.normalizeMinGap(temporal$min_gap)$by, "concept"))
    if (reduce_by_concept) {
      if (!has_concept_col) {
        stop("Temporal reduction by concept requires a usable concept column.",
             call. = FALSE)
      }
      select_parts <- paste0(
        select_parts, ", ", t_alias, ".", concept_col,
        " AS dsomop_event_partition_concept"
      )
    }
  }

  # Break same-date ties with the OMOP row primary key. Both internal aliases
  # are removed after ranking and never become part of the output contract.
  event_pk_col <- if (has_temporal_reduction ||
                      isTRUE(add_event_order_id)) {
    .eventPrimaryKeyColumn(bp, table_lower)
  } else {
    NULL
  }
  if (!is.null(event_pk_col)) {
    select_parts <- paste0(select_parts, ", ", t_alias, ".", event_pk_col,
                           " AS dsomop_event_order_id")
  }

  # Use TOP for limit (OHDSI SQL convention, translated to LIMIT by .sql_translate)
  if (!is.null(limit)) {
    limit_num <- suppressWarnings(as.numeric(limit))
    if (length(limit_num) != 1L || is.na(limit_num) || !is.finite(limit_num) ||
        limit_num != floor(limit_num) || limit_num < 1L) {
      stop("limit must be one positive integer.", call. = FALSE)
    }
    max_limit <- suppressWarnings(as.numeric(
      getOption("dsomop.max_query_rows", 1000000L)
    ))
    if (length(max_limit) != 1L || is.na(max_limit) || !is.finite(max_limit) ||
        max_limit != floor(max_limit) || max_limit < 1L) {
      stop("dsomop.max_query_rows must be one positive integer.", call. = FALSE)
    }
    if (limit_num > max_limit) {
      stop("limit exceeds the server query-row cap.", call. = FALSE)
    }
    sql <- paste0("SELECT TOP ", as.integer(limit_num), " ", select_parts,
                  " FROM ", qualified_table, " AS ", t_alias)
  } else {
    sql <- paste0("SELECT ", select_parts, " FROM ", qualified_table, " AS ", t_alias)
  }

  # Build WHERE clauses
  where <- character(0)

  if (!is.null(concept_filter)) {
    where <- c(where, .sqlIdInPredicate(
      paste0(t_alias, ".", concept_filter_col), concept_filter
    ))
  }

  if (!is.null(person_ids)) {
    where <- c(where, .sqlIdInPredicate(
      paste0(t_alias, ".person_id"), person_ids
    ))
  }

  if (needs_cohort_join) {
    ranked_cohort <- .rankedCohortSql(cohort_table, handle)
    sql <- paste0(sql, " INNER JOIN ", ranked_cohort,
                  " AS c ON c.subject_id = ", t_alias, ".person_id")
  } else if (!is.null(cohort_table) && has_person_id) {
    where <- c(where, paste0(
      "EXISTS (SELECT 1 FROM ", cohort_table,
      " AS c WHERE c.subject_id = ", t_alias, ".person_id)"
    ))
  }

  # Visit-linkage filter: restrict events to visits of given visit_concept_id
  # values via the visit_occurrence_id FK (present in the join graph). Emitted as
  # an EXISTS so it never multiplies rows or exposes visit identifiers.
  if (!is.null(visit_filter)) {
    if (!"visit_occurrence_id" %in% col_df$column_name) {
      stop("visit_filter cannot be applied to table '", table,
           "' because visit_occurrence_id is unavailable.", call. = FALSE)
    }
    vo_row <- bp$tables[bp$tables$table_name == "visit_occurrence" &
                          bp$tables$present_in_db, , drop = FALSE]
    raw_visit_ids <- unlist(
      visit_filter$concept_ids %||% visit_filter$visit_concept_id,
      use.names = FALSE)
    numeric_visit_ids <- suppressWarnings(as.numeric(raw_visit_ids))
    visit_ids <- suppressWarnings(as.integer(raw_visit_ids))
    if (length(raw_visit_ids) == 0L || anyNA(numeric_visit_ids) ||
        any(!is.finite(numeric_visit_ids)) || anyNA(visit_ids) ||
        any(numeric_visit_ids != visit_ids)) {
      stop("visit_filter requires one or more finite integer visit concept ",
           "IDs.", call. = FALSE)
    }
    if (nrow(vo_row) == 0) {
      stop("visit_filter requires the visit_occurrence table.", call. = FALSE)
    }
    vo_cols <- bp$columns[["visit_occurrence"]]$column_name
    required_visit_cols <- c("visit_occurrence_id", "visit_concept_id")
    if (!all(required_visit_cols %in% vo_cols)) {
      stop("visit_filter requires visit_occurrence_id and visit_concept_id ",
           "on visit_occurrence.", call. = FALSE)
    }
    visit_ids <- unique(visit_ids)
    where <- c(where, paste0(
      "EXISTS (SELECT 1 FROM ", vo_row$qualified_name[1], " AS v",
      " WHERE v.visit_occurrence_id = ", t_alias, ".visit_occurrence_id",
      " AND v.visit_concept_id IN (",
      paste(visit_ids, collapse = ", "), "))"
    ))
  }

  # Temporal WHERE clauses
  if (!is.null(temporal)) {
    date_col_temporal <- .getDateColumn(bp, table_lower)
    temporal_where <- .compileTemporalWhere(
      handle, temporal, t_alias, date_col_temporal
    )
    where <- c(where, temporal_where)
  }

  if (!is.null(time_window)) {
    if (!is.list(time_window) || length(time_window) == 0L ||
        is.null(names(time_window)) || any(!nzchar(names(time_window))) ||
        anyDuplicated(names(time_window))) {
      stop("time_window must be a non-empty named specification.",
           call. = FALSE)
    }
    unknown_time_fields <- setdiff(
      names(time_window), c("date_column", "start_date", "end_date")
    )
    if (length(unknown_time_fields) > 0L) {
      stop("Unknown time_window field(s): ",
           paste(unknown_time_fields, collapse = ", "), ".", call. = FALSE)
    }
    if (!is.null(time_window$date_column)) {
      if (!is.character(time_window$date_column) ||
          length(time_window$date_column) != 1L ||
          is.na(time_window$date_column)) {
        stop("time_window$date_column must be one column name.",
             call. = FALSE)
      }
      date_col <- tolower(.validateIdentifier(time_window$date_column,
                                               "time_window date column"))
    } else {
      date_col <- .getDateColumn(bp, table_lower)
    }
    has_bounds <- !is.null(time_window$start_date) ||
      !is.null(time_window$end_date)
    if (!has_bounds) {
      stop("time_window must contain start_date and/or end_date.",
           call. = FALSE)
    }
    if (has_bounds && (is.null(date_col) || !date_col %in% col_df$column_name)) {
      stop("A time_window was supplied, but its date column is unavailable.",
           call. = FALSE)
    }
    if (has_bounds) {
      bounds <- .validateDateBounds(
        time_window$start_date, time_window$end_date, "time_window"
      )
      if (!is.null(time_window$start_date)) {
        where <- c(where, paste0(
          t_alias, ".", date_col, " >= ",
          .quoteLiteral(as.character(bounds$start), handle)
        ))
      }
      if (!is.null(time_window$end_date)) {
        where <- c(where, paste0(
          t_alias, ".", date_col, " < ",
          .quoteLiteral(as.character(bounds$end + 1L), handle)
        ))
      }
    }
  }

  # Custom filter DSL. Validated fail-closed FIRST (identifier/blocked columns
  # and narrow fingerprinting ops are rejected before any SQL is emitted), then
  # compiled and ANDed with the predicates above. The distinct-person gate still
  # runs on the resulting query in .extractTable / .planExecute, so a custom
  # filter can only narrow — never bypass — the suppression.
  if (!is.null(filters) && length(filters) > 0) {
    valid_cols <- .filterableColumns(bp, table_lower)
    # A date_range row filter is authored against the generic sentinels
    # "start_date"/"end_date" (the client cannot know each table's real date
    # column). Resolve them to this table's actual date column BEFORE validation,
    # otherwise the allowlist check rejects the sentinel as an unknown column.
    filters <- .resolveFilterDateColumns(filters, bp, table_lower)
    .assertCustomFilterSafe(filters, valid_cols, handle = handle,
                            table = table_lower)
    filter_sql <- .compileFilter(handle, filters, t_alias, valid_cols)
    if (!is.null(filter_sql) && nchar(filter_sql) > 0) {
      where <- c(where, filter_sql)
    } else {
      stop("Custom filter did not compile to a predicate.", call. = FALSE)
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
  .sql_translate(
    paste0("SELECT COUNT(DISTINCT person_id) AS n_persons FROM (",
           from_sql, ") AS sub"),
    handle$target_dialect
  )
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

#' Columns a custom extraction filter is allowed to reference
#'
#' The custom filter DSL (\code{\link{.compileFilter}}) can in principle target
#' ANY column, which is exactly what makes it disclosive: a leaf referencing a
#' raw identifier (\code{person_id}, \code{*_occurrence_id}, \code{provider_id})
#' could be used to probe for or leak a specific id, and a leaf on a blocked
#' source-value / free-text / quasi-identifier column reaches data the column
#' allowlist otherwise hides. This returns the SAFE subset of a table's columns:
#' every column MINUS the row-level identifiers (\code{\link{.identifierColumns}})
#' and the blocked/sensitive columns flagged in the blueprint. Filtering is
#' restricted to this set fail-closed.
#'
#' @param bp Blueprint
#' @param table_lower Character; lower-cased table name
#' @param extra Character vector; additional column names to permit (e.g.
#'   visit-join columns that live on \code{visit_occurrence}, validated by the
#'   caller against that table's own safe set).
#' @return Character vector of filterable column names
#' @keywords internal
.filterableColumns <- function(bp, table_lower, extra = character(0)) {
  col_df <- bp$columns[[table_lower]]
  if (is.null(col_df) || nrow(col_df) == 0) return(unique(extra))
  cols <- col_df$column_name
  blocked <- col_df$column_name[col_df$is_blocked]
  # Identifiers are NEVER filterable: the person/subject key is a protected
  # pseudonym on output and the row ids are dropped, so allowing a filter to
  # reference them would let a client target an individual by raw id.
  cols <- setdiff(cols, union(blocked, .identifierColumns()))
  unique(c(cols, extra))
}

#' Map a custom-filter operator to its disclosure classification family
#'
#' Bridges the \code{\link{.compileFilter}} operator vocabulary onto the filter
#' families understood by \code{\link{.classifyFilter}} so that every custom
#' filter leaf is subject to the SAME granularity policy as the cohort filters.
#' Membership/null operators map to \code{value_threshold}; exact-match and
#' client-authored ordered comparisons map to \code{custom}, which
#' \code{.classifyFilter} blocks. Dates are handled separately as validated
#' bounded ranges and numerics require a server-issued \code{value_bin}.
#' \code{value_bin} is the pre-validated, server-sanctioned binning family and
#' is always allowed.
#'
#' @param op Character; a \code{.compileFilter} operator (already lower-cased)
#' @return Character; a filter-type understood by \code{.classifyFilter}
#' @keywords internal
.filterOpClass <- function(op) {
  switch(op,
    "value_bin" = "value_bin",
    "between" =, "in" =, "not_in" =, "is_null" =,
      "not_null" = "value_threshold",
    # Exact and ordered comparisons, plus anything unrecognised, are arbitrary
    # client thresholds -> blocked by .classifyFilter (fail-closed).
    "custom"
  )
}

#' Does a custom filter tree reference a given column in any leaf?
#'
#' Walks the AND/OR/leaf grammar and returns TRUE if any leaf's \code{var}
#' equals \code{col} (case-insensitive). Used to detect when a concept_col
#' override is actually a surfacing choice (its column is independently scoped by
#' an explicit filter) rather than the concept-set's scoping column.
#'
#' @param filter List; the filter structure
#' @param col Character; column name to look for
#' @return Logical scalar
#' @keywords internal
.filterTreeReferencesColumn <- function(filter, col) {
  if (is.null(filter) || length(filter) == 0 || is.null(col)) return(FALSE)
  col <- tolower(col)
  if ("and" %in% names(filter)) {
    return(any(vapply(filter$and, .filterTreeReferencesColumn, logical(1),
                      col = col)))
  }
  if ("or" %in% names(filter)) {
    return(any(vapply(filter$or, .filterTreeReferencesColumn, logical(1),
                      col = col)))
  }
  identical(tolower(filter$var %||% ""), col)
}

#' Resolve generic date sentinels in a custom filter tree to a table's real
#' date column
#'
#' The client's \code{date_range} row filter targets the table-agnostic sentinels
#' \code{"start_date"} / \code{"end_date"} because it cannot know each OMOP
#' table's concrete date column (e.g. \code{condition_start_date},
#' \code{measurement_date}). This walks the AND/OR/leaf tree and rewrites any leaf
#' whose \code{var} is one of those sentinels to the table's actual date column
#' (from \code{\link{.getDateColumn}}). Leaves referencing real columns are left
#' untouched, so it is a no-op for every other filter type. When the table has no
#' resolvable date column the sentinel is left as-is and the downstream allowlist
#' check rejects it fail-closed.
#'
#' @param filter List; the filter structure
#' @param bp Blueprint
#' @param table_lower Character; lowercased table name
#' @return The filter structure with date sentinels resolved
#' @keywords internal
.resolveFilterDateColumns <- function(filter, bp, table_lower) {
  if (is.null(filter) || length(filter) == 0) return(filter)

  if ("and" %in% names(filter)) {
    filter$and <- lapply(filter$and, .resolveFilterDateColumns, bp = bp,
                         table_lower = table_lower)
    return(filter)
  }
  if ("or" %in% names(filter)) {
    filter$or <- lapply(filter$or, .resolveFilterDateColumns, bp = bp,
                        table_lower = table_lower)
    return(filter)
  }

  var <- tolower(filter$var %||% "")
  if (var %in% c("start_date", "end_date")) {
    date_col <- .getDateColumn(bp, table_lower)
    if (!is.null(date_col)) filter$var <- date_col
  }
  filter
}

#' Validate a numeric bin against server-issued session state
#'
#' @param handle CDM handle carrying the safe-bin cache.
#' @param table,column Source table and numeric column.
#' @param value List with finite `lower` and `upper` edges.
#' @param scope Contract returned by `.profileSafeCutpoints()`.
#' @return `TRUE` invisibly, or a generic fail-closed error.
#' @keywords internal
.assertSafeNumericBinContract <- function(handle, table, column, value, scope) {
  fail <- function() {
    stop("Disclosive: numeric bin was not issued for this resource session and ",
         "table scope; request fresh safe cutpoints first.", call. = FALSE)
  }
  if (is.null(handle) || !is.list(scope) || is.null(names(scope)) ||
      any(!nzchar(names(scope))) || anyDuplicated(names(scope))) fail()
  allowed <- c("table", "column", "concept_id", "concept_col", "n_bins")
  if (length(setdiff(names(scope), allowed)) > 0L ||
      !all(c("table", "column", "n_bins") %in% names(scope))) fail()

  table <- tolower(.validateIdentifier(table, "safe-bin table"))
  column <- tolower(.validateIdentifier(column, "safe-bin column"))
  scope_table <- tryCatch(
    tolower(.validateIdentifier(scope$table, "safe-bin scope table")),
    error = function(e) ""
  )
  scope_column <- tryCatch(
    tolower(.validateIdentifier(scope$column, "safe-bin scope column")),
    error = function(e) ""
  )
  n_bins_num <- suppressWarnings(as.numeric(scope$n_bins))
  n_bins_int <- suppressWarnings(as.integer(scope$n_bins))
  if (!identical(scope_table, table) || !identical(scope_column, column) ||
      length(n_bins_num) != 1L || !is.finite(n_bins_num) ||
      length(n_bins_int) != 1L || is.na(n_bins_int) ||
      n_bins_num != n_bins_int || n_bins_int < 2L || n_bins_int > 100L) fail()

  lower <- suppressWarnings(as.numeric(value$lower))
  upper <- suppressWarnings(as.numeric(value$upper))
  if (length(lower) != 1L || length(upper) != 1L ||
      !is.finite(lower) || !is.finite(upper) || lower >= upper) fail()

  now <- as.numeric(Sys.time())
  cache <- handle$safe_numeric_bins %||% list()
  same_nullable <- function(x, y) {
    if (is.null(x) && is.null(y)) return(TRUE)
    if (is.null(x) || is.null(y)) return(FALSE)
    identical(as.character(unlist(x, use.names = FALSE)),
              as.character(unlist(y, use.names = FALSE)))
  }
  matches <- vapply(cache, function(entry) {
    if (!is.list(entry) || is.null(entry$expires_at) ||
        !is.finite(entry$expires_at) || entry$expires_at <= now ||
        !identical(entry$table, table) || !identical(entry$column, column) ||
        !identical(as.integer(entry$n_bins), n_bins_int) ||
        !same_nullable(entry$concept_id, scope$concept_id) ||
        !same_nullable(entry$concept_col, scope$concept_col)) return(FALSE)
    edges <- suppressWarnings(as.numeric(entry$breaks))
    if (length(edges) < 2L || any(!is.finite(edges))) return(FALSE)
    near <- function(edge, target) {
      any(abs(edge - target) <= 1e-10 * pmax(1, abs(target)))
    }
    near(edges, lower) && near(edges, upper)
  }, logical(1))
  if (!any(matches)) fail()
  invisible(TRUE)
}

#' Validate a custom filter tree against the disclosure policy (fail-closed)
#'
#' Walks the same AND/OR/leaf structure as \code{\link{.compileFilter}} and, for
#' every leaf, (1) confirms the referenced column is in the table's filterable
#' allowlist (\code{\link{.filterableColumns}}) so identifier/blocked columns can
#' never be targeted, and (2) runs the leaf operator through
#' \code{\link{.validateFilter}} (via \code{\link{.filterOpClass}}) so narrow
#' fingerprinting predicates are rejected before any SQL is built. Stops on the
#' first unsafe leaf; this is the gate that keeps the custom DSL from bypassing
#' the per-patient suppression that still runs on the filtered result.
#'
#' @param filter List; the filter structure
#' @param valid_columns Character vector; filterable column allowlist
#' @param handle Optional CDM handle. Required to authenticate `value_bin`
#'   contracts at a public query boundary.
#' @param table Optional source table paired with `handle`.
#' @param .depth Internal recursion depth.
#' @param .state Internal shared complexity counter.
#' @return TRUE invisibly, or stops with a disclosure error
#' @keywords internal
.assertCustomFilterSafe <- function(filter, valid_columns, handle = NULL,
                                    table = NULL, .depth = 1L,
                                    .state = NULL) {
  if (is.null(filter) || length(filter) == 0) return(invisible(TRUE))

  leaf_values <- if (is.list(filter) && !is.null(names(filter)) &&
      !any(c("and", "or") %in% names(filter))) {
    length(unlist(filter$value, use.names = FALSE))
  } else 0L
  .state <- .filterComplexityVisit(.state, .depth, leaf_values)

  if (!is.list(filter) || is.null(names(filter)) ||
      any(!nzchar(names(filter))) || anyDuplicated(names(filter))) {
    stop("Custom filters must be named AND/OR groups or named leaves.",
         call. = FALSE)
  }
  group_keys <- intersect(names(filter), c("and", "or"))
  if (length(group_keys) > 0L) {
    if (length(group_keys) != 1L || length(names(filter)) != 1L) {
      stop("Custom filter nodes cannot mix AND/OR groups with each other or ",
           "with leaf fields.", call. = FALSE)
    }
  }

  if (identical(group_keys, "and")) {
    if (!is.list(filter$and) || length(filter$and) == 0) {
      stop("Custom filter AND group must contain at least one predicate.",
           call. = FALSE)
    }
    for (f in filter$and) {
      .assertCustomFilterSafe(
        f, valid_columns, handle = handle, table = table,
        .depth = .depth + 1L, .state = .state
      )
    }
    return(invisible(TRUE))
  }
  if (identical(group_keys, "or")) {
    if (!is.list(filter$or) || length(filter$or) == 0) {
      stop("Custom filter OR group must contain at least one predicate.",
           call. = FALSE)
    }
    for (f in filter$or) {
      .assertCustomFilterSafe(
        f, valid_columns, handle = handle, table = table,
        .depth = .depth + 1L, .state = .state
      )
    }
    return(invisible(TRUE))
  }

  allowed_leaf_fields <- c("var", "op", "value", "safe_scope")
  unknown_fields <- setdiff(names(filter), allowed_leaf_fields)
  if (length(unknown_fields) > 0L) {
    stop("Unknown custom filter leaf field(s): ",
         paste(unknown_fields, collapse = ", "), ".", call. = FALSE)
  }
  if (!all(c("var", "op") %in% names(filter))) {
    stop("Custom filter leaves require both var and op.", call. = FALSE)
  }

  var <- tolower(filter$var %||% "")
  op <- tolower(filter$op %||% "")
  if (!nzchar(var) || !nzchar(op)) {
    stop("Custom filter leaves require both var and op.", call. = FALSE)
  }
  valid_ops <- c("==", "eq", "!=", "ne", ">=", "gte", "<=", "lte",
                 ">", "gt", "<", "lt", "in", "not_in", "between",
                 "is_null", "not_null", "value_bin")
  if (!op %in% valid_ops) {
    stop("Unknown custom filter operator: '", op, "'.", call. = FALSE)
  }
  if (!op %in% c("is_null", "not_null") &&
      (!"value" %in% names(filter) || is.null(filter$value))) {
    stop("Custom filter operator '", op, "' requires a value.",
         call. = FALSE)
  }
  .validateIdentifier(var, "filter column")
  if (!var %in% valid_columns) {
    stop("Disclosive: filter on column '", var,
         "' is not permitted (identifier, blocked, or unknown column).",
         call. = FALSE)
  }

  value <- filter$value
  is_date <- grepl("_date$|_datetime$", var)
  if (is_date) {
    if (!identical(op, "between")) {
      stop("Disclosive: date filter column '", var,
           "' only permits one validated BETWEEN range; standalone date ",
           "comparisons cannot prove a safe width.", call. = FALSE)
    }
    values <- unlist(value, use.names = FALSE)
    if (length(values) != 2L) {
      stop("Date BETWEEN filters require exactly two ISO dates.",
           call. = FALSE)
    }
    bounds <- .validateDateBounds(values[1], values[2], "date filter")
    .validateFilter("date_range", list(
      start = as.character(bounds$start), end = as.character(bounds$end)
    ))
    return(invisible(TRUE))
  }

  if (op %in% c(">=", "gte", "<=", "lte", ">", "gt", "<", "lt")) {
    stop("Disclosive: client-authored ordered thresholds are not permitted; ",
         "request a server-issued value_bin for numeric columns.",
         call. = FALSE)
  }

  if (identical(op, "value_bin")) {
    lower <- suppressWarnings(as.numeric(value$lower))
    upper <- suppressWarnings(as.numeric(value$upper))
    if (length(value$lower) != 1L || length(value$upper) != 1L ||
        length(lower) != 1L || length(upper) != 1L ||
        !is.finite(lower) || !is.finite(upper) || lower >= upper) {
      stop("value_bin requires finite scalar lower/upper bounds with lower < ",
           "upper.", call. = FALSE)
    }
    if (!is.null(handle)) {
      .assertSafeNumericBinContract(
        handle, table = table, column = var, value = value,
        scope = filter$safe_scope
      )
    }
  }

  if (op %in% c("in", "not_in")) {
    safe_categories <- c(
      "domain_id", "vocabulary_id", "concept_class_id",
      "standard_concept", "invalid_reason"
    )
    is_safe_category <- grepl("_concept_id$", var) ||
      var %in% safe_categories
    values <- unlist(value, use.names = FALSE)
    if (!is_safe_category) {
      stop("Disclosive: IN/NOT IN is only permitted for concept IDs or ",
           "approved categorical columns, not '", var, "'.", call. = FALSE)
    }
    if (length(values) == 0L || anyNA(values)) {
      stop("IN/NOT IN filters require at least one non-missing value.",
           call. = FALSE)
    }
  }

  if (identical(op, "between")) {
    stop("Numeric BETWEEN filters are not permitted; use a validated ",
         "value_bin instead.", call. = FALSE)
  }
  .validateFilter(.filterOpClass(op), list())
  invisible(TRUE)
}

#' Compile a filter DSL structure into SQL WHERE fragments
#'
#' @param handle CDM handle
#' @param filter List; the filter structure
#' @param table_alias Character; table alias
#' @param valid_columns Character vector; whitelist
#' @param .depth Internal recursion depth.
#' @param .state Internal shared complexity counter.
#' @return Character; SQL WHERE fragment
#' @keywords internal
.compileFilter <- function(handle, filter, table_alias = "t",
                           valid_columns = NULL, .depth = 1L,
                           .state = NULL) {
  if (is.null(filter) || length(filter) == 0) return(NULL)

  leaf_values <- if (is.list(filter) && !is.null(names(filter)) &&
      !any(c("and", "or") %in% names(filter))) {
    length(unlist(filter$value, use.names = FALSE))
  } else 0L
  .state <- .filterComplexityVisit(.state, .depth, leaf_values)

  if ("and" %in% names(filter)) {
    parts <- vapply(filter$and, function(f) {
      .compileFilter(handle, f, table_alias, valid_columns,
                     .depth = .depth + 1L, .state = .state)
    }, character(1))
    parts <- parts[nchar(parts) > 0]
    if (length(parts) == 0) return("")
    return(paste0("(", paste(parts, collapse = " AND "), ")"))
  }

  if ("or" %in% names(filter)) {
    parts <- vapply(filter$or, function(f) {
      .compileFilter(handle, f, table_alias, valid_columns,
                     .depth = .depth + 1L, .state = .state)
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
    "==" =, "eq" = paste0(col_ref, " = ", .quoteLiteral(value, handle)),
    "!=" =, "ne" = paste0(col_ref, " != ", .quoteLiteral(value, handle)),
    ">=" =, "gte" = paste0(col_ref, " >= ", .quoteLiteral(value, handle)),
    "<=" =, "lte" = paste0(col_ref, " <= ", .quoteLiteral(value, handle)),
    ">"  =, "gt"  = paste0(col_ref, " > ", .quoteLiteral(value, handle)),
    "<"  =, "lt"  = paste0(col_ref, " < ", .quoteLiteral(value, handle)),
    "in" = {
      vals <- paste(vapply(value, .quoteLiteral, character(1), handle = handle),
                    collapse = ", ")
      paste0(col_ref, " IN (", vals, ")")
    },
    "not_in" = {
      vals <- paste(vapply(value, .quoteLiteral, character(1), handle = handle),
                    collapse = ", ")
      paste0(col_ref, " NOT IN (", vals, ")")
    },
    "between" = {
      values <- unlist(value, use.names = FALSE)
      if (grepl("_date$|_datetime$", var)) {
        start <- .isoDate(values[1], "date filter lower bound")
        end <- .isoDate(values[2], "date filter upper bound")
        if (start > end) {
          stop("Date filter lower bound must not be after its upper bound.",
               call. = FALSE)
        }
        paste0(col_ref, " >= ", .quoteLiteral(as.character(start), handle),
               " AND ", col_ref, " < ",
               .quoteLiteral(as.character(end + 1L), handle))
      } else {
        paste0(col_ref, " BETWEEN ", .quoteLiteral(values[1], handle),
               " AND ", .quoteLiteral(values[2], handle))
      }
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

#' Coerce integer64 columns to a precision-safe type
#'
#' Converts bit64::integer64 columns to plain \code{integer} when every value
#' fits int32, otherwise to \code{character}. With \code{stable = TRUE}, every
#' integer64 column becomes character regardless of the current values; bounded
#' DBI fetches use this mode so later chunks cannot change physical type when a
#' larger 64-bit identifier first appears. DataSHIELD cannot serialize
#' integer64, but it handles both integer and character. We deliberately do
#' NOT fall back to \code{double}: \code{as.numeric()} rounds values above
#' 2^53, which silently collapsed distinct 64-bit person ids onto the same
#' value — breaking cohort IN-list filters (\code{IN (NA, ...)} after a later
#' \code{as.integer}) and merging distinct identities in joins and in the
#' pseudonymous person key.
#'
#' @param df A data.frame potentially containing integer64 columns.
#' @param stable Logical; always use character for integer64 columns. Integer
#'   values in identifier-shaped columns are also character in this mode so a
#'   later BIGINT value cannot change a bounded stream's physical schema.
#' @return The data.frame with integer64 columns converted exactly.
#' @keywords internal
.coerce_integer64 <- function(df, stable = FALSE) {
  for (col in names(df)) {
    if (inherits(df[[col]], "integer64")) {
      v <- df[[col]]
      nona <- v[!is.na(v)]
      fits_int <- length(nona) == 0 ||
        (min(nona) >= -2147483647 && max(nona) <= 2147483647)
      df[[col]] <- if (!isTRUE(stable) && fits_int) {
        as.integer(v)
      } else {
        as.character(v)
      }
    } else if (isTRUE(stable) && is.integer(df[[col]]) &&
               grepl("(^id$|_(id|key|identifier)$)", tolower(col),
                     perl = TRUE)) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  df
}

#' Format identifier values as an exact SQL integer-literal list
#'
#' Replaces \code{paste(as.integer(ids), collapse = ", ")}, which returned
#' \code{NA} for any id above 2^31 (yielding \code{person_id IN (NA, ...)} and
#' empty results). Formats integer64/character/integer/double exactly, with no
#' int32 overflow and no scientific notation, and drops NAs.
#'
#' @param ids A vector of identifier values.
#' @return A single comma-separated string of literals (\code{""} if empty).
#' @keywords internal
.sqlIdList <- function(ids) {
  if (is.null(ids) || length(ids) == 0) return("")
  if (inherits(ids, "integer64") || is.character(ids)) {
    out <- as.character(ids)
  } else {
    out <- format(ids, scientific = FALSE, trim = TRUE)
    out <- sub("\\.0+$", "", out)
  }
  out <- out[!is.na(out) & nzchar(out) & out != "NA"]
  if (length(out) > 0 && any(!grepl("^[0-9]+$", out))) {
    stop("Identifier lists must contain only non-negative integer IDs.",
         call. = FALSE)
  }
  paste(out, collapse = ", ")
}

#' Build a portable chunked SQL IN predicate for identifier values
#'
#' Oracle accepts at most 1000 expressions in one IN list. Splitting larger
#' identifier vectors into parenthesized OR clauses is valid on every supported
#' dialect and avoids requiring a cross-statement temporary table.
#'
#' @param column Trusted SQL column expression assembled by dsOMOP.
#' @param ids Identifier values accepted by \code{\link{.sqlIdList}}.
#' @param max_per_clause Maximum literals per IN clause.
#' @return One SQL predicate; empty IDs compile to \code{1 = 0}.
#' @keywords internal
.sqlIdInPredicate <- function(column, ids, max_per_clause = 1000L) {
  literals <- .sqlIdList(ids)
  if (!nzchar(literals)) return("1 = 0")
  values <- strsplit(literals, ", ", fixed = TRUE)[[1L]]
  groups <- split(values, ceiling(seq_along(values) / max_per_clause))
  clauses <- vapply(groups, function(group) {
    paste0(column, " IN (", paste(group, collapse = ", "), ")")
  }, character(1))
  if (length(clauses) == 1L) clauses else {
    paste0("(", paste(clauses, collapse = " OR "), ")")
  }
}

#' Execute a SQL query and return a data frame
#'
#' @param handle CDM handle
#' @param sql Character; SQL to execute
#' @return Data frame
#' @keywords internal
.executeQuery <- function(handle, sql) {
  result <- .withDbReconnect(handle, function(conn) DBI::dbGetQuery(conn, sql))
  names(result) <- tolower(names(result))
  .coerce_integer64(result)
}

.arrowAvailable <- function() {
  requireNamespace("arrow", quietly = TRUE)
}

.renameStagingFile <- function(from, to) {
  file.rename(from, to)
}

#' Stream a SQL query result to one Parquet file in bounded chunks
#'
#' Uses DBI::dbSendQuery + dbFetch(n=chunk_size) to avoid loading the full
#' result set into R memory. Peak R memory is bounded by one transformed chunk
#' plus DBI/Arrow writer buffers and therefore still depends on row width.
#' Falls back to a CSV file if Arrow is not installed. Arrow's low-level
#' Parquet writer appends each fetched batch as a row group in one pending file,
#' which is atomically renamed after the footer is closed. This avoids a second
#' scan and never creates a duplicate of the complete result.
#'
#' @param conn DBI connection
#' @param sql Character; SQL query to execute
#' @param output_path Character; path for the output file (.parquet or .csv)
#' @param chunk_size Integer; rows per chunk (default 50000)
#' @param chunk_fn Optional function(chunk) applied to each chunk before
#'   writing. Must return a data.frame; it may remove sensitive columns. Used
#'   for per-chunk transforms like date handling, type conversion, and
#'   identifier sanitization.
#' @return Named list with file path, format, row count, column names, and
#'   stable storage/class type signatures.
#' @keywords internal
.executeQueryToParquet <- function(conn, sql, output_path, chunk_size = 50000L,
                                   chunk_fn = NULL) {
  old_umask <- Sys.umask("0077")
  on.exit(Sys.umask(old_umask), add = TRUE)
  chunk_num <- suppressWarnings(as.numeric(chunk_size))
  if (length(chunk_num) != 1L || is.na(chunk_num) || !is.finite(chunk_num) ||
      chunk_num != floor(chunk_num) || chunk_num < 1L || chunk_num > 1000000L) {
    stop("chunk_size must be one integer from 1 to 1,000,000.", call. = FALSE)
  }
  chunk_size <- as.integer(chunk_num)
  max_rows <- suppressWarnings(as.numeric(
    getOption("dsomop.max_staged_rows", 50000000L)
  ))
  max_bytes <- suppressWarnings(as.numeric(
    getOption("dsomop.max_staged_bytes", 10 * 1024^3)
  ))
  if (length(max_rows) != 1L || is.na(max_rows) || !is.finite(max_rows) ||
      max_rows != floor(max_rows) || max_rows < 1L ||
      length(max_bytes) != 1L || is.na(max_bytes) || !is.finite(max_bytes) ||
      max_bytes < 1) {
    stop("Staging row/byte caps must be positive finite server values.",
         call. = FALSE)
  }
  use_parquet <- .arrowAvailable()

  if (!is.character(output_path) || length(output_path) != 1L ||
      is.na(output_path) || !nzchar(output_path)) {
    stop("output_path must be one non-empty staging path.", call. = FALSE)
  }
  staging_dir <- dirname(output_path)
  staging_token <- basename(staging_dir)
  staging_base <- .stagingBaseDir()
  resolved_dir <- tryCatch(
    normalizePath(staging_dir, winslash = "/", mustWork = TRUE),
    error = function(e) ""
  )
  if (!grepl("^stg_[0-9a-f]{32}$", staging_token) ||
      .isSymbolicLink(staging_dir) || !dir.exists(staging_dir) ||
      !identical(resolved_dir,
                 file.path(staging_base, staging_token))) {
    stop("Streaming output must stay inside its reserved staging directory.",
         call. = FALSE)
  }
  output_basename <- basename(output_path)
  if (!grepl("^[A-Za-z_][A-Za-z0-9_.]*\\.parquet$", output_basename)) {
    stop("Streaming output must have a safe Parquet file name.",
         call. = FALSE)
  }

  if (!use_parquet) {
    output_path <- sub("\\.parquet$", ".csv", output_path)
  }
  if (file.exists(output_path) || .isSymbolicLink(output_path)) {
    stop("Staged output path already exists.", call. = FALSE)
  }
  existing_bytes <- .stagingDirectoryBytes(staging_dir)
  if (existing_bytes >= max_bytes) {
    stop("Staged output exceeds the server disk quota.", call. = FALSE)
  }

  col_names <- NULL
  column_types <- NULL
  n_rows <- 0L
  chunk_idx <- 0L
  completed <- FALSE

  # The low-level writer keeps one open Parquet file and emits one or more row
  # groups per fetched batch. The pending file is in the destination directory,
  # so final publication is an atomic same-filesystem rename.
  pending_file <- NULL
  if (use_parquet) {
    pending_file <- tempfile(
      pattern = paste0(output_basename, ".pending-"),
      tmpdir = staging_dir
    )
  }

  rs <- NULL
  parquet_writer <- NULL
  parquet_sink <- NULL
  on.exit({
    if (!is.null(rs) && DBI::dbIsValid(rs)) DBI::dbClearResult(rs)
    if (!is.null(parquet_writer)) try(parquet_writer$Close(), silent = TRUE)
    if (!is.null(parquet_sink)) try(parquet_sink$close(), silent = TRUE)
    if (!is.null(pending_file) && file.exists(pending_file)) unlink(pending_file)
    if (!completed && (file.exists(output_path) || dir.exists(output_path))) {
      unlink(output_path, recursive = TRUE)
    }
  }, add = TRUE)
  rs <- DBI::dbSendQuery(conn, sql)

  # Capture the result schema without consuming rows. A disclosure-safe query
  # may legitimately be empty; it must still produce a valid, readable staged
  # file rather than a descriptor pointing at a non-existent path.
  empty <- DBI::dbFetch(rs, n = 0L)
  names(empty) <- tolower(names(empty))
  empty <- .coerce_integer64(empty, stable = TRUE)
  if (!is.null(chunk_fn)) empty <- chunk_fn(empty)
  if (!is.data.frame(empty)) {
    stop("chunk_fn must return a data.frame.", call. = FALSE)
  }
  col_names <- names(empty)
  schema_signature <- function(x) {
    vapply(x, function(col) {
      paste(typeof(col), paste(class(col), collapse = "/"), sep = "|")
    }, character(1))
  }
  empty_column_types <- schema_signature(empty)

  open_parquet_writer <- function(example) {
    parquet_sink <<- arrow::FileOutputStream$create(pending_file)
    example_table <- arrow::Table$create(example)
    writer_properties <- arrow::ParquetWriterProperties$create(
      column_names = col_names
    )
    parquet_writer <<- arrow::ParquetFileWriter$create(
      schema = example_table$schema,
      sink = parquet_sink,
      properties = writer_properties
    )
  }

  repeat {
    chunk <- DBI::dbFetch(rs, n = chunk_size)
    if (nrow(chunk) == 0L) break

    names(chunk) <- tolower(names(chunk))
    chunk <- .coerce_integer64(chunk, stable = TRUE)

    if (!is.null(chunk_fn)) {
      chunk <- chunk_fn(chunk)
    }
    if (!is.data.frame(chunk)) {
      stop("chunk_fn must return a data.frame.", call. = FALSE)
    }
    if (n_rows + nrow(chunk) > max_rows) {
      stop("Staged output exceeds the server row quota.", call. = FALSE)
    }

    chunk_types <- schema_signature(chunk)
    if (!identical(names(chunk), col_names) ||
        (chunk_idx > 0L && !identical(chunk_types, column_types))) {
      stop("Staged chunk transformations must preserve stable names and types.",
           call. = FALSE)
    }

    chunk_idx <- chunk_idx + 1L
    if (chunk_idx == 1L) {
      # DBI drivers may report an empty BIGINT result as plain numeric and add
      # their integer64 class only when values are fetched (notably RMariaDB).
      # The first real chunk therefore defines the non-empty physical schema;
      # every later chunk is still required to match it exactly.
      column_types <- chunk_types
      if (use_parquet) open_parquet_writer(chunk)
    }

    if (use_parquet) {
      parquet_writer$WriteTable(
        arrow::Table$create(chunk),
        as.integer(nrow(chunk))
      )
    } else {
      utils::write.table(chunk, output_path,
                  sep = ",", row.names = FALSE,
                  col.names = (n_rows == 0L),
                  append = (n_rows > 0L))
    }

    n_rows <- n_rows + nrow(chunk)
    staged_files <- if (use_parquet) {
      pending_file
    } else {
      output_path
    }
    bytes <- sum(file.info(staged_files)$size, na.rm = TRUE)
    if (existing_bytes + bytes > max_bytes) {
      stop("Staged output exceeds the server disk quota.", call. = FALSE)
    }
  }

  DBI::dbClearResult(rs)

  if (chunk_idx == 0L) {
    column_types <- empty_column_types
    if (use_parquet) open_parquet_writer(empty)
    if (!use_parquet) {
      utils::write.table(empty, output_path, sep = ",", row.names = FALSE,
                  col.names = TRUE)
    }
  }

  if (use_parquet) {
    parquet_writer$Close()
    parquet_writer <- NULL
    parquet_sink$close()
    parquet_sink <- NULL
    Sys.chmod(pending_file, mode = "0600")
    if (!file.exists(pending_file) ||
        !isTRUE(.renameStagingFile(pending_file, output_path))) {
      stop("Could not atomically publish staged Parquet output.",
           call. = FALSE)
    }
    pending_file <- NULL
  }

  if (!file.exists(output_path)) {
    stop("Staged query did not create an output file.", call. = FALSE)
  }
  output_bytes <- if (dir.exists(output_path)) {
    .stagingDirectoryBytes(output_path)
  } else {
    file.info(output_path)$size
  }
  if (existing_bytes + output_bytes > max_bytes) {
    stop("Staged output exceeds the server disk quota.", call. = FALSE)
  }
  Sys.chmod(output_path, mode = if (dir.exists(output_path)) "0700" else "0600")

  fmt <- if (use_parquet) "parquet" else "csv"
  layout <- "file"
  completed <- TRUE
  list(
    file = output_path,
    format = fmt,
    layout = layout,
    parts = NULL,
    n_rows = n_rows,
    columns = col_names,
    column_types = column_types
  )
}

#' Execute a SQL statement (DDL/DML, no result set)
#'
#' @param handle CDM handle
#' @param sql Character; SQL to execute
#' @return Number of affected rows (invisible)
#' @keywords internal
.executeStatement <- function(handle, sql) {
  invisible(.withDbReconnect(handle, function(conn) DBI::dbExecute(conn, sql)))
}

#' Resolve concept sets carried by individual feature specifications
#'
#' Feature specs keep their own concept-set semantics. In particular, one spec
#' may request descendants while another spec over the same table may not. The
#' resolution therefore happens per spec, before both SQL row selection and the
#' in-memory reductions.
#'
#' @param handle CDM handle.
#' @param specs Named list of feature specifications.
#' @param table Optional source table. When supplied, output names and value
#'   columns are validated against the table blueprint before any SQL runs.
#' @return Feature specifications with concept sets resolved to integer IDs.
#' @keywords internal
.resolveFeatureSpecs <- function(handle, specs, table = NULL) {
  if (is.null(specs)) return(specs)
  if (!is.list(specs)) {
    stop("feature_specs must be a list of feature specifications.",
         call. = FALSE)
  }

  exact_ids <- function(x) {
    raw <- unlist(x, use.names = FALSE)
    if (length(raw) == 0L) return(integer(0))
    max_values <- .extractionCap("dsomop.max_filter_values", 10000L)
    if (length(raw) > max_values) {
      stop("Feature concept_set exceeds the server max_filter_values cap of ",
           max_values, ".", call. = FALSE)
    }
    numeric_ids <- suppressWarnings(as.numeric(raw))
    integer_ids <- suppressWarnings(as.integer(raw))
    if (anyNA(numeric_ids) || any(!is.finite(numeric_ids)) ||
        anyNA(integer_ids) || any(numeric_ids != integer_ids)) {
      stop("Feature concept_set values must be finite exact integers.",
           call. = FALSE)
    }
    unique(integer_ids)
  }

  resolved <- lapply(specs, function(spec) {
    if (!is.list(spec)) {
      stop("Each feature specification must be a named list.", call. = FALSE)
    }
    concept_set <- spec$concept_set
    if (is.list(concept_set) && !is.null(concept_set$concepts)) {
      concept_set$concepts <- exact_ids(concept_set$concepts)
      if (!is.null(concept_set$exclude)) {
        concept_set$exclude <- exact_ids(concept_set$exclude)
      }
      spec$concept_set <- .vocabExpandConceptSet(handle, concept_set)
      max_values <- .extractionCap("dsomop.max_filter_values", 10000L)
      if (length(spec$concept_set) > max_values) {
        stop("Expanded feature concept_set exceeds the server ",
             "max_filter_values cap of ", max_values, ".", call. = FALSE)
      }
    } else {
      spec$concept_set <- exact_ids(concept_set)
    }
    spec
  })

  if (is.null(table)) return(resolved)

  bp <- .buildBlueprint(handle)
  table_lower <- tolower(.validateIdentifier(table, "feature source table"))
  col_df <- bp$columns[[table_lower]]
  if (is.null(col_df) || nrow(col_df) == 0L) {
    stop("No columns found for feature source table '", table, "'.",
         call. = FALSE)
  }
  safe_value_columns <- .filterableColumns(bp, table_lower)
  value_types <- c(
    "mean_value", "min_value", "max_value", "first_value", "latest_value",
    "sum_value", "sd_value", "cv_value", "slope_value"
  )
  reserved_names <- unique(tolower(c(
    .identifierColumns(), .PERSON_KEY_COLS(), .EPISODE_KEY_COLS(),
    "rn", "dsomop_event_order_id", "dsomop_event_order_date",
    "dsomop_event_partition_concept", ".seq", ".present",
    "days_from_index"
  )))
  list_names <- names(resolved)
  effective_names <- character(length(resolved))

  for (i in seq_along(resolved)) {
    spec <- resolved[[i]]
    name <- spec$name
    if (!is.null(name) &&
        (!is.character(name) || length(name) != 1L || is.na(name))) {
      stop("Feature name must be NULL or one non-missing character value.",
           call. = FALSE)
    }
    if (is.null(name) || !nzchar(name)) {
      key <- if (!is.null(list_names)) list_names[[i]] else ""
      if (!is.na(key) && nzchar(key)) {
        name <- key
      } else {
        concept_tag <- if (length(spec$concept_set) > 0L) {
          paste(spec$concept_set, collapse = "_")
        } else {
          "all"
        }
        name <- .standardizeName(concept_tag)
        if (is.na(name) || !nzchar(name)) name <- paste0("feature_", i)
      }
    }
    if (tolower(name) %in% reserved_names) {
      stop("Feature name '", name, "' is reserved for an identifier or ",
           "internal linkage column.", call. = FALSE)
    }
    spec$name <- name
    effective_names[[i]] <- tolower(name)

    type <- spec$type %||% "boolean"
    if (!is.character(type) || length(type) != 1L || is.na(type) ||
        !nzchar(type)) {
      stop("Feature '", name,
           "' type must be one non-empty character value.", call. = FALSE)
    }
    supplied_value <- !is.null(spec$value_column)
    if (supplied_value || type %in% value_types) {
      value_column <- spec$value_column %||% "value_as_number"
      if (!is.character(value_column) || length(value_column) != 1L ||
          is.na(value_column) || !nzchar(value_column)) {
        stop("Feature '", name,
             "' value_column must be one non-empty column name.",
             call. = FALSE)
      }
      value_column <- tolower(.validateIdentifier(
        value_column, paste0("Feature '", name, "' value_column")
      ))
      if (!value_column %in% col_df$column_name) {
        stop("Feature '", name, "' value_column '", value_column,
             "' does not exist on table '", table, "'.", call. = FALSE)
      }
      if (!value_column %in% safe_value_columns) {
        stop("Disclosive: feature '", name, "' value_column '",
             value_column, "' is an identifier or blocked column.",
             call. = FALSE)
      }
      spec$value_column <- value_column
    }
    resolved[[i]] <- spec
  }

  if (anyDuplicated(effective_names)) {
    duplicates <- unique(effective_names[duplicated(effective_names)])
    stop("Feature output names must be unique; duplicated name(s): ",
         paste(duplicates, collapse = ", "), ".", call. = FALSE)
  }
  resolved
}

.featureSpecTypes <- function(specs) {
  if (is.null(specs) || length(specs) == 0L) return(character(0))
  vapply(specs, function(spec) {
    type <- spec$type %||% "boolean"
    if (!is.character(type) || length(type) != 1L || is.na(type) ||
        !nzchar(type)) {
      stop("Feature type must be one non-empty character value.",
           call. = FALSE)
    }
    type
  }, character(1))
}

.featureScopeFilter <- function(specs, default_concept_col) {
  if (is.null(specs) || length(specs) == 0L) return(NULL)
  concept_sets <- lapply(specs, function(spec) spec$concept_set)
  # One unscoped spec means the shared source stream must remain unscoped.
  if (any(lengths(concept_sets) == 0L)) return(NULL)

  leaves <- lapply(seq_along(specs), function(i) {
    column <- specs[[i]]$concept_col %||% default_concept_col
    if (is.null(column) || length(column) != 1L || is.na(column) ||
        !nzchar(column)) {
      stop("A scoped feature requires a usable concept_col.", call. = FALSE)
    }
    list(var = tolower(column), op = "in", value = concept_sets[[i]])
  })
  if (length(leaves) == 1L) leaves[[1]] else list(or = leaves)
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
#' @param representation Character; "long", "wide", "features", or "sparse".
#' @param feature_specs Named list; for features mode
#' @param representation_grain Character; aggregation unit for wide/features,
#'   either "person" or "episode". Episode grain requires a cohort-anchored
#'   index window and preserves one row per cohort_row_id.
#' @param block_sensitive Logical; block sensitive columns
#' @param temporal List; temporal filtering spec
#' @param date_handling List; date handling spec
#' @param filters List; optional custom filter DSL tree (see
#'   \code{\link{.compileSelect}}); validated fail-closed and ANDed in.
#' @param concept_col Character; optional concept-scoping column override
#'   passed through to \code{\link{.compileSelect}}.
#' @param visit_filter List; optional visit-linkage filter passed through to
#'   \code{\link{.compileSelect}}.
#' @details In wide mode, an explicit \code{concept_filter} is also the output
#'   column contract: every requested concept is materialized even when no
#'   qualifying row is observed. This keeps a closed concept set structurally
#'   identical across federated sites.
#' @return Data frame
#' @keywords internal
.extractTable <- function(handle, table, columns = NULL,
                          concept_filter = NULL, person_ids = NULL,
                          time_window = NULL, cohort_table = NULL,
                          translate_concepts = TRUE,
                          representation = "long",
                          feature_specs = NULL,
                          representation_grain = "person",
                          block_sensitive = TRUE,
                          temporal = NULL,
                          date_handling = NULL,
                          add_cohort_date = FALSE,
                          filters = NULL, concept_col = NULL,
                          visit_filter = NULL) {

  bp <- .buildBlueprint(handle)
  feature_person_ids <- NULL
  feature_roster <- NULL
  feature_date_col <- NULL
  feature_concept_col <- NULL
  feature_types <- character(0)
  feature_has_time_windows <- FALSE
  sparse_person_ids <- NULL
  sparse_roster <- NULL
  wide_roster <- NULL
  allowed_representations <- c("long", "wide", "features", "sparse")
  if (!is.character(representation) || length(representation) != 1L ||
      is.na(representation) || !representation %in% allowed_representations) {
    stop("representation must be long, wide, features, or sparse.",
         call. = FALSE)
  }
  if (!is.character(representation_grain) ||
      length(representation_grain) != 1L || is.na(representation_grain) ||
      !tolower(representation_grain) %in% c("person", "episode")) {
    stop("representation_grain must be 'person' or 'episode'.",
         call. = FALSE)
  }
  representation_grain <- tolower(representation_grain)
  if (identical(representation, "features")) {
    max_feature_specs <- .extractionCap("dsomop.max_feature_specs", 1000L)
    if (!is.null(feature_specs) && length(feature_specs) > max_feature_specs) {
      stop("feature_specs exceeds the server cap of ", max_feature_specs, ".",
           call. = FALSE)
    }
    feature_specs <- .resolveFeatureSpecs(handle, feature_specs, table = table)
    feature_types <- .featureSpecTypes(feature_specs)
    feature_has_time_windows <- any(vapply(feature_specs, function(spec) {
      !is.null(spec$time_window)
    }, logical(1)))

    feature_concept_col <- concept_col %||%
      .getDomainConceptColumn(bp, tolower(table))
    if (!is.null(feature_concept_col)) {
      feature_concept_col <- tolower(.validateIdentifier(
        feature_concept_col, "feature concept_col"
      ))
    }
    valid_feature_concept_cols <- intersect(
      .filterableColumns(bp, tolower(table)),
      grep("_concept_id$", bp$columns[[tolower(table)]]$column_name,
           value = TRUE)
    )
    if (!is.null(feature_concept_col) &&
        !tolower(feature_concept_col) %in% valid_feature_concept_cols) {
      stop("Feature concept_col '", feature_concept_col,
           "' is not a safe concept column on table '", table, "'.",
           call. = FALSE)
    }
    for (spec in feature_specs) {
      if (!is.null(spec$concept_col)) {
        spec_col <- tolower(.validateIdentifier(
          spec$concept_col, "feature concept_col"
        ))
        if (!spec_col %in% valid_feature_concept_cols) {
          stop("Feature concept_col '", spec_col,
               "' is not a safe concept column on table '", table, "'.",
               call. = FALSE)
        }
      }
      if (!is.null(spec$filter)) {
        .assertCustomFilterSafe(
          spec$filter, .filterableColumns(bp, tolower(table)),
          handle = handle, table = tolower(table)
        )
      }
    }
    feature_scope <- .featureScopeFilter(feature_specs, feature_concept_col)
    if (!is.null(feature_scope)) {
      filters <- if (is.null(filters) || length(filters) == 0L) {
        feature_scope
      } else {
        list(and = list(filters, feature_scope))
      }
    }

    # Explicit column projections must retain every column needed by a spec's
    # independent concept scope, value reducer, and in-memory row filter. These
    # columns are consumed by .toFeatures and never leak as additional output
    # columns.
    if (!is.null(columns) && length(feature_specs) > 0L) {
      spec_concept_cols <- vapply(feature_specs, function(spec) {
        as.character(spec$concept_col %||% feature_concept_col %||% "")[[1]]
      }, character(1))
      spec_concept_cols <- spec_concept_cols[nzchar(spec_concept_cols)]
      spec_value_cols <- vapply(feature_specs, function(spec) {
        as.character(spec$value_column %||% "")[[1]]
      }, character(1))
      spec_value_cols <- spec_value_cols[nzchar(spec_value_cols)]
      filter_columns <- function(filter) {
        if (is.null(filter) || length(filter) == 0L) return(character(0))
        if (!is.null(filter$and)) {
          return(unique(unlist(lapply(filter$and, filter_columns),
                               use.names = FALSE)))
        }
        if (!is.null(filter$or)) {
          return(unique(unlist(lapply(filter$or, filter_columns),
                               use.names = FALSE)))
        }
        tolower(as.character(filter$var %||% ""))
      }
      spec_filter_cols <- unique(unlist(lapply(feature_specs, function(spec) {
        filter_columns(spec$filter)
      }), use.names = FALSE))
      spec_filter_cols <- spec_filter_cols[nzchar(spec_filter_cols)]

      types <- .featureSpecTypes(feature_specs)
      fixed_cols <- character(0)
      if (any(types == "abnormal_high")) {
        fixed_cols <- c(fixed_cols, "value_as_number", "range_high")
      }
      if (any(types == "abnormal_low")) {
        fixed_cols <- c(fixed_cols, "value_as_number", "range_low")
      }
      if (any(types %in% c("drug_duration", "duration_sum"))) {
        fixed_cols <- c(fixed_cols, intersect(
          c("drug_exposure_start_date", "drug_exposure_end_date",
            "drug_era_start_date", "drug_era_end_date",
            "condition_era_start_date", "condition_era_end_date",
            "condition_start_date", "condition_end_date",
            "visit_start_date", "visit_end_date",
            "observation_period_start_date", "observation_period_end_date"),
          bp$columns[[tolower(table)]]$column_name
        ))
      }
      columns <- unique(c(columns, spec_concept_cols, spec_value_cols,
                          spec_filter_cols, fixed_cols))
    }

    if (identical(representation_grain, "episode")) {
      if (is.null(cohort_table)) {
        stop("Episode-grain features require a cohort table.", call. = FALSE)
      }
      feature_roster <- .executeQuery(handle, paste0(
        "SELECT c.cohort_row_id, c.subject_id AS person_id FROM ",
        .rankedCohortSql(cohort_table, handle),
        " AS c ORDER BY c.cohort_row_id"
      ))
      feature_roster$cohort_row_id <- as.integer(
        feature_roster$cohort_row_id)
      if (anyNA(feature_roster$cohort_row_id) ||
          anyDuplicated(feature_roster$cohort_row_id)) {
        stop("Episode-grain feature roster has invalid cohort_row_id values.",
             call. = FALSE)
      }
    } else {
      feature_person_ids <- if (!is.null(person_ids)) unique(person_ids) else NULL
      if (is.null(feature_person_ids) && !is.null(cohort_table)) {
        roster <- .executeQuery(handle, paste0(
          "SELECT DISTINCT subject_id AS person_id FROM ", cohort_table,
          " ORDER BY subject_id"
        ))
        feature_person_ids <- roster$person_id
      }
    }

    date_dependent <- c(
      "first_value", "latest_value", "time_since", "slope_value",
      "gap_max_days", "gap_mean_days"
    )
    if (any(feature_types %in% date_dependent) || feature_has_time_windows) {
      feature_date_col <- .getDateColumn(bp, tolower(table))
      if (!is.null(columns) && !is.null(feature_date_col)) {
        columns <- unique(c(columns, feature_date_col))
      }
    }
  }
  if (identical(representation, "sparse")) {
    if (identical(representation_grain, "episode")) {
      if (is.null(cohort_table)) {
        stop("Episode-grain sparse output requires a cohort table.",
             call. = FALSE)
      }
      sparse_roster <- .executeQuery(handle, paste0(
        "SELECT c.cohort_row_id, c.subject_id AS person_id FROM ",
        .rankedCohortSql(cohort_table, handle),
        " AS c ORDER BY c.cohort_row_id"
      ))
      sparse_roster$cohort_row_id <- as.integer(sparse_roster$cohort_row_id)
    } else {
      sparse_person_ids <- if (!is.null(person_ids)) unique(person_ids) else NULL
      if (is.null(sparse_person_ids) && !is.null(cohort_table)) {
        person_roster <- .executeQuery(handle, paste0(
          "SELECT DISTINCT subject_id AS person_id FROM ", cohort_table,
          " ORDER BY subject_id"
        ))
        sparse_person_ids <- person_roster$person_id
      }
    }
  }
  if (identical(representation, "wide")) {
    if (identical(representation_grain, "episode")) {
      if (is.null(cohort_table)) {
        stop("Episode-grain wide output requires a cohort table.",
             call. = FALSE)
      }
      wide_roster <- .executeQuery(handle, paste0(
        "SELECT c.cohort_row_id, c.subject_id AS person_id FROM ",
        .rankedCohortSql(cohort_table, handle),
        " AS c ORDER BY c.cohort_row_id"
      ))
      wide_roster$cohort_row_id <- as.integer(wide_roster$cohort_row_id)
    } else if (!is.null(person_ids)) {
      wide_roster <- data.frame(
        person_id = unique(person_ids), stringsAsFactors = FALSE
      )
    } else if (!is.null(cohort_table)) {
      wide_roster <- .executeQuery(handle, paste0(
        "SELECT DISTINCT subject_id AS person_id FROM ", cohort_table,
        " ORDER BY subject_id"
      ))
    }
  }

  # Validate the release policy before querying, including for empty results.
  date_handling <- .normalizeDateHandling(date_handling)
  if (is.null(date_handling)) {
    default_mode <- getOption("dsomop.default_date_handling", "remove")
    date_handling <- .normalizeDateHandling(default_mode)
  }
  if (identical(date_handling$mode, "absolute")) {
    allow <- getOption("dsomop.allow_absolute_dates",
               getOption("default.dsomop.allow_absolute_dates", FALSE))
    if (!isTRUE(allow)) {
      stop("Absolute date handling is not permitted by the server. ",
           "Contact the data controller to enable dsomop.allow_absolute_dates.",
           call. = FALSE)
    }
  }
  if (identical(representation, "features")) {
    has_index_window <- !is.null(temporal$index_window)
    if (has_index_window && !identical(representation_grain, "episode")) {
      stop("features with temporal$index_window require grain='episode' ",
           "so recurrent cohort entries are not collapsed by person.",
           call. = FALSE)
    }
    if (!has_index_window && identical(representation_grain, "episode")) {
      stop("grain='episode' requires temporal$index_window so every ",
           "event has an explicit episode-relative scope.", call. = FALSE)
    }
    if (feature_has_time_windows) {
      if (!has_index_window) {
        stop("Feature time_window requires temporal$index_window.",
             call. = FALSE)
      }
      .validateTemporalSpec(temporal)
      outer <- temporal$index_window
      outer_start <- if (!is.null(outer$start)) {
        .temporalOffset(outer$start, "start")
      } else {
        NULL
      }
      outer_end <- if (!is.null(outer$end)) {
        .temporalOffset(outer$end, "end")
      } else {
        NULL
      }
      normalize_feature_bound <- function(value, field, feature_name) {
        if (is.null(value)) return(NULL)
        numeric_value <- suppressWarnings(as.numeric(value))
        integer_value <- suppressWarnings(as.integer(value))
        if (length(value) != 1L || length(numeric_value) != 1L ||
            !is.finite(numeric_value) || length(integer_value) != 1L ||
            is.na(integer_value) || numeric_value != integer_value) {
          stop("Feature '", feature_name, "' time_window ", field,
               " must be one integer day offset.", call. = FALSE)
        }
        integer_value
      }
      for (i in seq_along(feature_specs)) {
        window <- feature_specs[[i]]$time_window
        if (is.null(window)) next
        if (!is.list(window) || is.null(names(window)) ||
            any(!nzchar(names(window))) || anyDuplicated(names(window)) ||
            length(setdiff(names(window), c("start", "end"))) > 0L ||
            (is.null(window$start) && is.null(window$end))) {
          stop("Feature '", feature_specs[[i]]$name,
               "' time_window must be a named start/end day window.",
               call. = FALSE)
        }
        window_start <- normalize_feature_bound(
          window$start, "start", feature_specs[[i]]$name
        )
        window_end <- normalize_feature_bound(
          window$end, "end", feature_specs[[i]]$name
        )
        if (!is.null(window_start) && !is.null(window_end) &&
            window_start > window_end) {
          stop("Feature '", feature_specs[[i]]$name,
               "' time_window start must not be after end.", call. = FALSE)
        }
        if ((!is.null(outer_start) &&
             (is.null(window_start) || window_start < outer_start)) ||
            (!is.null(outer_end) &&
             (is.null(window_end) || window_end > outer_end))) {
          stop("Feature '", feature_specs[[i]]$name,
               "' time_window must be contained in temporal$index_window.",
               call. = FALSE)
        }
        feature_specs[[i]]$time_window <- list(
          start = window_start, end = window_end
        )
      }
      # Cohort dates and the event date remain server-internal and are needed
      # to derive days_from_index even when the caller supplied a narrow
      # projection.
      add_cohort_date <- TRUE
    }
  }
  if (identical(representation, "wide")) {
    has_index_window <- !is.null(temporal$index_window)
    if (has_index_window && !identical(representation_grain, "episode")) {
      stop("wide with temporal$index_window requires grain='episode' so ",
           "recurrent cohort entries are not collapsed by person.",
           call. = FALSE)
    }
    if (!has_index_window && identical(representation_grain, "episode")) {
      stop("wide grain='episode' requires temporal$index_window.",
           call. = FALSE)
    }
  }
  if (identical(representation, "sparse")) {
    has_index_window <- !is.null(temporal$index_window)
    if (has_index_window && !identical(representation_grain, "episode")) {
      stop("sparse with temporal$index_window requires grain='episode' so ",
           "recurrent cohort entries are not collapsed by person.",
           call. = FALSE)
    }
    if (!has_index_window && identical(representation_grain, "episode")) {
      stop("sparse grain='episode' requires temporal$index_window.",
           call. = FALSE)
    }
  }

  sql <- .compileSelect(
    handle, table,
    columns = columns,
    concept_filter = concept_filter,
    person_ids = person_ids,
    time_window = time_window,
    cohort_table = cohort_table,
    block_sensitive = block_sensitive,
    temporal = temporal,
    add_cohort_date = add_cohort_date,
    filters = filters,
    concept_col = concept_col,
    visit_filter = visit_filter,
    add_event_order_id = identical(representation, "features") &&
      any(feature_types %in% c("first_value", "latest_value"))
  )

  if (!is.null(temporal$min_gap)) {
    tie_col <- if (!is.null(.eventPrimaryKeyColumn(bp, table))) {
      "dsomop_event_order_id"
    } else {
      NULL
    }
    sql <- .wrapMinGap(handle, sql, temporal,
                       "dsomop_event_order_date", tie_col = tie_col)
  }

  # Event selection is applied to the collapsed representatives.
  if (!is.null(temporal$event_select)) {
    tie_col <- if (!is.null(.eventPrimaryKeyColumn(bp, table))) {
      "dsomop_event_order_id"
    } else {
      NULL
    }
    sql <- .wrapEventSelect(handle, sql, temporal,
                            "dsomop_event_order_date",
                            tie_col = tie_col)
  }

  col_df <- bp$columns[[tolower(table)]]
  has_person_id <- "person_id" %in% col_df$column_name

  # Coverage / disclosure note: the distinct-person gate only fires for
  # person-bearing tables. Specimen and dose_era carry person_id and so are
  # extractable here under the normal gate. The cost table has NO person_id
  # (it links via cost_event_id), so this gate cannot protect raw cost rows;
  # cost is therefore intentionally NOT a person-keyed plan/extraction target
  # and must be reached only through the curated, aggregate-only query library
  # (see R/queries.R). (payer_plan_period DOES have person_id and is gated.)
  if (has_person_id) {
    count_sql <- .compilePersonCount(handle, sql)
    .assertMinPersons(handle = handle, sql = count_sql)
  }

  max_memory_rows <- suppressWarnings(as.numeric(
    getOption("dsomop.max_memory_rows", 1000000L)
  ))
  if (length(max_memory_rows) != 1L || is.na(max_memory_rows) ||
      !is.finite(max_memory_rows) || max_memory_rows != floor(max_memory_rows) ||
      max_memory_rows < 1L) {
    stop("dsomop.max_memory_rows must be one positive integer.", call. = FALSE)
  }
  row_count_sql <- paste0(
    "SELECT COUNT(*) AS n FROM (", sql, ") AS dsomop_memory_check"
  )
  row_count <- suppressWarnings(as.numeric(.executeQuery(handle, row_count_sql)$n[1]))
  if (is.na(row_count) || !is.finite(row_count)) {
    stop("Could not verify the extraction row count.", call. = FALSE)
  }
  if (row_count > max_memory_rows) {
    stop("Extraction exceeds the server in-memory row cap (", max_memory_rows,
         "); use output_mode='staged' for an event-level Parquet output or ",
         "narrow the population/windows.", call. = FALSE)
  }

  result <- .executeQuery(handle, sql)
  # ROW_NUMBER and its primary-key tie breaker are internal query machinery.
  result$rn <- NULL
  result$dsomop_event_partition_concept <- NULL
  result[grep("^dsomop_gap_", names(result), value = TRUE)] <- NULL
  if (!identical(representation, "features")) {
    result$dsomop_event_order_id <- NULL
  }

  # An empty extraction still needs the representation transform applied so the
  # caller receives a correctly-SHAPED frame. For "features" this means a
  # person-level frame carrying the feature columns (e.g. an unseeded BMI concept
  # yields a bmi_mean column of NAs), so a person_level join does not collapse to
  # zero rows when one feature sub-table happens to be empty. Long/wide/sparse
  # keep their historical empty-passthrough behavior.
  if (nrow(result) == 0) {
    result$dsomop_event_order_date <- NULL
    if (identical(representation, "features")) {
      if (feature_has_time_windows &&
          !"days_from_index" %in% names(result)) {
        result$days_from_index <- integer(0)
      }
      empty_feat <- if (identical(representation_grain, "episode")) {
        .toEpisodeFeatures(
          result, table, feature_specs, roster = feature_roster,
          date_col = feature_date_col,
          default_concept_col = feature_concept_col
        )
      } else {
        .toFeatures(
          result, table, feature_specs, person_ids = feature_person_ids,
          date_col = feature_date_col, default_concept_col = feature_concept_col
        )
      }
      if (translate_concepts && is.data.frame(empty_feat)) {
        empty_feat <- .vocabTranslateColumns(handle, empty_feat)
      }
      return(empty_feat)
    }
    if (identical(representation, "sparse")) {
      return(.toSparse(
        result, table, person_ids = sparse_person_ids,
        roster = sparse_roster, grain = representation_grain
      ))
    }
    if (identical(representation, "wide")) {
      return(.toWide(
        result, table, handle, grain = representation_grain,
        translate_concepts = translate_concepts, roster = wide_roster,
        expected_concepts = concept_filter
      ))
    }
    return(result)
  }

  # Compute days_from_index when cohort_start_date is present
  if ("cohort_start_date" %in% names(result)) {
    date_col_for_index <- .getDateColumn(bp, tolower(table))
    date_source <- if (!is.null(date_col_for_index) &&
                       date_col_for_index %in% names(result)) {
      date_col_for_index
    } else if ("dsomop_event_order_date" %in% names(result)) {
      "dsomop_event_order_date"
    } else {
      NULL
    }
    if (!is.null(date_source)) {
      result$days_from_index <- as.integer(
        as.Date(result[[date_source]]) -
        as.Date(result$cohort_start_date)
      )
    }
  }
  result$dsomop_event_order_date <- NULL

  result <- .convertTypes(result)

  # DATE PRIVACY: Default is "remove" (strip all date/datetime columns).
  # Per OMOP Privacy Guidance, date elements across the CDM may require
  # redaction or modification. The safe default removes them entirely.
  # Analysts who need temporal data should use "relative" (days from index)
  # or "binned" (year-month) modes, which preserve utility without leaking
  # exact dates that could enable longitudinal re-identification.
  # Accept a bare string (e.g. "relative_to_index") or the list form, and map
  # the public synonym onto the internal "relative" mode.
  # The "features" builder may need raw dates to compute disclosure-safe
  # aggregates — e.g. drug_duration = end_date - start_date. Its output is a
  # person-level data frame with no raw date columns, so we apply date handling
  # AFTER the transform (a no-op on the aggregated output) instead of stripping
  # the dates the builder depends on. All other representations (long/wide keep
  # raw rows; sparse returns a list) handle dates before the transform as before.
  agg_repr <- identical(representation, "features")
  if (!agg_repr) {
    result <- .applyDateHandling(result, date_handling)
    # Cohort dates are internal join references, never output columns.
    result$cohort_start_date <- NULL
    result$cohort_end_date <- NULL
  }

  # Translate concept-id VALUES to human-readable names, but ONLY for the
  # representations that surface *_concept_id as data: "long" keeps the columns
  # and "wide" uses their values as column labels. The "features" and "sparse"
  # builders instead match/encode the RAW numeric concept ids (e.g. matching a
  # spec's numeric concept_set, or computing covariateId = conceptId*1000 + k),
  # so they must see untranslated ids; readable labelling happens inside them.
  if (translate_concepts && identical(representation, "long")) {
    result <- .vocabTranslateColumns(handle, result)
  }

  result <- switch(representation,
    "long" = result,
    "wide" = .toWide(result, table, handle,
                     grain = representation_grain,
                     translate_concepts = translate_concepts,
                     roster = wide_roster,
                     expected_concepts = concept_filter),
    "features" = if (identical(representation_grain, "episode")) {
      .toEpisodeFeatures(
        result, table, feature_specs, roster = feature_roster,
        date_col = feature_date_col,
        default_concept_col = feature_concept_col
      )
    } else {
      .toFeatures(
        result, table, feature_specs, person_ids = feature_person_ids,
        date_col = feature_date_col, default_concept_col = feature_concept_col
      )
    },
    "sparse" = .toSparse(
      result, table, person_ids = sparse_person_ids,
      roster = sparse_roster, grain = representation_grain
    ),
    result
  )

  if (agg_repr) {
    result <- .applyDateHandling(result, date_handling)
  }

  # "features" keeps raw pass-through concept columns (e.g. gender_concept_id);
  # translate them so person-level frames and the factor-harmonization layer see
  # readable names. Renamed aggregate columns no longer match *_concept_id, so
  # .vocabTranslateColumns leaves them untouched.
  if (translate_concepts && identical(representation, "features") &&
      is.data.frame(result)) {
    result <- .vocabTranslateColumns(handle, result)
  }

  if (is.data.frame(result)) result$dsomop_event_order_id <- NULL

  result
}

# --- Type Conversion ---

#' Convert data types in a result data frame
#'
#' @param df Data frame
#' @return Data frame with converted types
#' @keywords internal
.convertTypes <- function(df) {
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

#' Resolve a requested raw-column spec into source columns and output aliases
#'
#' Plan column specs (\code{person_level} tables, \code{baseline}/event
#' \code{columns}) cross the DataSHIELD transport as JSON and arrive decoded
#' with \code{simplifyVector = FALSE}, i.e. as a (possibly named) list rather
#' than a character vector. This normalises any of: a character vector, a
#' list of scalars, a named list, or a named vector into two equal-length
#' character vectors — \code{source} (the real OMOP column to SELECT) and
#' \code{alias} (the name to expose in the output). Element names supply
#' aliases; unnamed elements alias to themselves. Returns \code{NULL} for an
#' empty/absent spec (meaning "server default columns").
#'
#' @param entry A column spec as received in a decoded plan.
#' @return \code{NULL}, or a list with character vectors \code{source} and
#'   \code{alias} of equal length.
#' @keywords internal
.colSpec <- function(entry) {
  if (is.null(entry)) return(NULL)
  src <- as.character(unlist(entry, use.names = FALSE))
  if (length(src) == 0) return(NULL)
  nm <- names(entry)
  if (is.null(nm)) {
    alias <- src
  } else {
    nm <- as.character(nm)
    alias <- ifelse(nzchar(nm), nm, src)
  }
  list(source = src, alias = alias)
}

#' Landed names of the concept-id columns in a column spec
#'
#' Given a \code{\link{.colSpec}} result, returns the output (\code{alias})
#' names of the columns whose \emph{source} is an OMOP \code{_concept_id}
#' column. Detection keys on the source name — the reliable signal, since an
#' alias may have renamed the \code{_concept_id} suffix away — while the value
#' returned is the name the column actually lands under. The factor
#' harmonization layer uses this to recognise concept columns even after a
#' user has renamed them, so renaming never silently opts a column out of
#' federated factor coding.
#'
#' @param spec A \code{.colSpec} result (\code{list(source=, alias=)}), or
#'   \code{NULL}.
#' @return Character vector of landed concept-column names; empty if none.
#' @keywords internal
.conceptAliases <- function(spec) {
  if (is.null(spec) || is.null(spec$source) || is.null(spec$alias)) {
    return(character(0))
  }
  unique(spec$alias[grepl("_concept_id$", spec$source)])
}

#' Rename extracted columns to their requested aliases
#'
#' Applies the \code{source -> alias} mapping from \code{\link{.colSpec}} to an
#' extracted data frame, matching source columns case-insensitively (OMOP
#' columns come back lower-cased from SQL).
#'
#' @section Security:
#' Row-level identifiers are never renamed, and no column is ever renamed
#' \emph{into} an identifier name. \code{.pseudonymizeIdentifiers} matches a
#' fixed set of identifier names (see \code{\link{.identifierColumns}}) and
#' runs after extraction — pseudonymizing the person/subject keys and dropping
#' the rest; allowing a key to be aliased away (or a benign column to
#' masquerade as one) would let a raw identifier escape pseudonymization.
#'
#' @param df A data frame returned by an extraction function.
#' @param spec A \code{.colSpec} result, or \code{NULL} (no-op).
#' @return \code{df} with columns renamed where an alias differs from source.
#' @keywords internal
.applyColumnAliases <- function(df, spec) {
  if (is.null(spec) || !is.data.frame(df) || length(names(df)) == 0) {
    return(df)
  }
  ids <- .identifierColumns()
  reviewed_ids <- .reviewedIdentifierColumns()
  cur <- names(df)
  for (i in seq_along(spec$source)) {
    s <- spec$source[i]
    a <- spec$alias[i]
    if (identical(s, a)) next
    if (tolower(s) %in% ids || tolower(a) %in% ids) next
    if (length(.untypedIdentifierColumns(
      c(s, a), reviewed = reviewed_ids, allow_concepts = TRUE
    )) > 0L) next
    # A benign value must not acquire concept-id privileges merely by being
    # renamed. Concept aliases may lose the suffix (and are separately tagged),
    # but only a concept-shaped source may land under a *_concept_id name.
    if (grepl("_concept_id$", tolower(a)) &&
        !grepl("_concept_id$", tolower(s))) next
    hit <- which(tolower(cur) == tolower(s))
    if (length(hit) == 1L) cur[hit] <- a
  }
  names(df) <- cur
  df
}

# --- Representations ---

#' Transform a long result to wide format
#'
#' Pivots on the domain concept column (from blueprint).
#'
#' @param df Data frame in long format
#' @param table Character; source table name
#' @param handle CDM handle.
#' @param grain Aggregation unit, "person" or "episode".
#' @param translate_concepts Logical; include vocabulary names in stable labels
#'   while retaining the numeric concept ID to prevent name collisions.
#' @param roster Optional complete data frame of person or cohort-episode keys.
#'   Rows without qualifying events remain in the wide output with missing
#'   values rather than disappearing.
#' @param expected_concepts Optional closed vector of concept IDs. Requested
#'   concepts are emitted as columns even when absent from \code{df}; observed
#'   concepts outside this vector are rejected.
#' @return Data frame in wide format
#' @keywords internal
.toWide <- function(df, table, handle = NULL, grain = "person",
                    translate_concepts = FALSE, roster = NULL,
                    expected_concepts = NULL) {
  bp <- .buildBlueprint(handle)
  concept_col <- NULL

  if (!is.character(grain) || length(grain) != 1L || is.na(grain) ||
      !tolower(grain) %in% c("person", "episode")) {
    stop("Wide grain must be 'person' or 'episode'.", call. = FALSE)
  }
  grain <- tolower(grain)

  # Try to find concept column from column names
  possible <- grep("_concept_id$", names(df), value = TRUE)
  # Filter out type and source concepts
  possible <- possible[!grepl("_type_concept_id$|_source_concept_id$", possible)]
  if (length(possible) > 0) concept_col <- possible[1]

  if (is.null(concept_col) || !concept_col %in% names(df)) {
    stop("Wide representation requires a domain concept column.",
         call. = FALSE)
  }
  if (!"person_id" %in% names(df)) {
    stop("Wide representation requires person_id.", call. = FALSE)
  }

  group_cols <- if (identical(grain, "episode")) {
    if (!"cohort_row_id" %in% names(df)) {
      stop("Episode-grain wide data require cohort_row_id.", call. = FALSE)
    }
    c("cohort_row_id", "person_id")
  } else {
    "person_id"
  }

  if (!is.null(roster)) {
    if (!is.data.frame(roster) || !all(group_cols %in% names(roster)) ||
        anyNA(roster[group_cols]) || anyDuplicated(roster[group_cols])) {
      stop("Wide roster must contain one unique, non-missing row per ", grain,
           ".", call. = FALSE)
    }
    roster <- roster[, group_cols, drop = FALSE]
  }

  # Identifier columns must be removed before pivoting. Once an identifier is
  # embedded in a generated name (for example
  # `concept_123.measurement_id`), the final release pass can no longer
  # recognise and drop it by its canonical OMOP column name.
  identifier_names <- tolower(.identifierColumns())
  drop_identifiers <- names(df)[
    tolower(names(df)) %in% identifier_names &
      !tolower(names(df)) %in% tolower(group_cols)
  ]
  if (length(drop_identifiers) > 0L) {
    df <- df[, setdiff(names(df), drop_identifiers), drop = FALSE]
  }

  value_cols <- setdiff(names(df), c(group_cols, concept_col))
  # A concept-only projection is still useful: materialise presence rather
  # than falling back to the original long rows (which would violate the
  # advertised representation and could retain row identifiers).
  if (length(value_cols) == 0L) {
    df$.present <- 1L
    value_cols <- ".present"
  }

  group_key <- do.call(paste, c(lapply(df[group_cols], as.character),
                                sep = "\r"))
  key <- paste(group_key, df[[concept_col]], sep = "\r")
  if (anyDuplicated(key)) {
    stop("Wide representation requires at most one row per ", grain,
         " and concept; ",
         "use event_select with a deterministic single event or an explicit ",
         "feature reduction (first/last/count/etc.).", call. = FALSE)
  }

  observed_concepts <- unique(df[[concept_col]][!is.na(df[[concept_col]])])
  if (!is.null(expected_concepts)) {
    raw_expected <- unlist(expected_concepts, use.names = FALSE)
    numeric_expected <- suppressWarnings(as.numeric(raw_expected))
    integer_expected <- suppressWarnings(as.integer(raw_expected))
    if (length(raw_expected) == 0L || anyNA(numeric_expected) ||
        any(!is.finite(numeric_expected)) || anyNA(integer_expected) ||
        any(numeric_expected != integer_expected)) {
      stop("expected_concepts must contain finite integer concept IDs.",
           call. = FALSE)
    }
    expected_concepts <- unique(integer_expected)
    observed_ids <- suppressWarnings(as.integer(as.character(
      observed_concepts
    )))
    if (anyNA(observed_ids) ||
        length(setdiff(observed_ids, expected_concepts)) > 0L) {
      stop("Wide data contain concepts outside the declared concept set.",
           call. = FALSE)
    }
    concepts <- expected_concepts
  } else {
    concepts <- observed_concepts
  }
  max_pivot_concepts <- .extractionCap(
    "dsomop.max_pivot_concepts", 1000L
  )
  if (length(concepts) > max_pivot_concepts) {
    stop("Wide representation exceeds the server concept cap of ",
         max_pivot_concepts, "; narrow the concept set.", call. = FALSE)
  }
  max_output_columns <- .extractionCap(
    "dsomop.max_output_columns", 5000L
  )
  predicted_columns <- length(group_cols) +
    as.double(length(concepts)) * length(value_cols)
  if (predicted_columns > max_output_columns) {
    stop("Wide representation would create ", predicted_columns,
         " columns, exceeding the server cap of ", max_output_columns, ".",
         call. = FALSE)
  }

  df$.seq <- stats::ave(seq_len(nrow(df)), key, FUN = seq_along)

  wide <- if (is.null(roster)) unique(df[group_cols]) else roster
  roster_key <- do.call(paste, c(lapply(wide[group_cols], as.character),
                                 sep = "\r"))
  event_key <- do.call(paste, c(lapply(df[group_cols], as.character),
                                sep = "\r"))
  if (length(setdiff(unique(event_key), roster_key)) > 0L) {
    stop("Wide events contain a ", grain, " key outside the declared roster.",
         call. = FALSE)
  }

  concept_name_map <- character(0)
  numeric_concepts <- suppressWarnings(as.numeric(as.character(concepts)))
  integer_concepts <- suppressWarnings(as.integer(as.character(concepts)))
  exact_numeric <- length(concepts) > 0L & !is.na(integer_concepts) &
    is.finite(numeric_concepts) & numeric_concepts == integer_concepts
  if (isTRUE(translate_concepts) && !is.null(handle) && any(exact_numeric)) {
    concept_rows <- tryCatch(
      .vocabLookupConcepts(handle, unique(integer_concepts[exact_numeric])),
      error = function(e) data.frame()
    )
    if (nrow(concept_rows) > 0L) {
      concept_name_map <- stats::setNames(
        as.character(concept_rows$concept_name),
        as.character(concept_rows$concept_id)
      )
    }
  }

  label_for_concept <- stats::setNames(
    vapply(concepts, function(concept) {
      fallback <- .standardizeName(as.character(concept))
      if (is.na(fallback) || !nzchar(fallback)) {
        fallback <- paste0("concept_", concept)
      }
      if (!isTRUE(translate_concepts) || is.null(handle)) return(fallback)

      numeric_id <- suppressWarnings(as.numeric(as.character(concept)))
      integer_id <- suppressWarnings(as.integer(as.character(concept)))
      if (length(numeric_id) != 1L || !is.finite(numeric_id) ||
          is.na(integer_id) || numeric_id != integer_id) {
        return(fallback)
      }
      readable_name <- unname(concept_name_map[as.character(integer_id)])
      readable <- if (length(readable_name) == 1L && !is.na(readable_name)) {
        .standardizeName(readable_name)
      } else {
        ""
      }
      if (is.na(readable) || !nzchar(readable)) readable <- "unknown"
      paste0("concept_", integer_id, "__", readable)
    }, character(1)),
    as.character(concepts)
  )
  if (anyDuplicated(unname(label_for_concept))) {
    stop("Wide concept labels are not unique; retain numeric concept IDs or ",
         "narrow the concept set.", call. = FALSE)
  }

  for (concept in concepts) {
    concept_data <- df[df[[concept_col]] == concept, , drop = FALSE]
    concept_label <- unname(label_for_concept[[as.character(concept)]])

    for (vcol in value_cols) {
      if (vcol == ".seq") next
      col_name <- paste0(concept_label, ".", vcol)
      first_occ <- concept_data[concept_data$.seq == 1, c(group_cols, vcol),
                                drop = FALSE]
      names(first_occ)[length(group_cols) + 1L] <- col_name
      wide <- merge(wide, first_occ, by = group_cols, all.x = TRUE,
                    sort = FALSE)
    }
  }

  landed_key <- do.call(paste, c(lapply(wide[group_cols], as.character),
                                 sep = "\r"))
  wide <- wide[match(roster_key, landed_key), , drop = FALSE]
  rownames(wide) <- NULL
  wide
}

#' Evaluate a custom filter tree as a row mask over an in-memory data frame
#'
#' Mirrors \code{\link{.compileFilter}}'s AND/OR/leaf grammar but evaluates it
#' against an already-extracted (disclosure-filtered) data frame in R, returning
#' a logical keep-mask. Used to scope a single feature spec's rows by its own
#' row filter (e.g. a unit/type slice) without re-querying the database. A leaf
#' that references a column not present in the frame matches NOTHING (fail-closed
#' for that leaf), and an unknown operator likewise drops the row, so a filter
#' can only ever narrow the rows feeding the aggregation.
#'
#' @param filter List; the filter structure (and/or/leaf)
#' @param df Data frame to evaluate against
#' @return Logical vector of length \code{nrow(df)}
#' @keywords internal
.evalFilterMask <- function(filter, df) {
  n <- nrow(df)
  if (is.null(filter) || length(filter) == 0) return(rep(TRUE, n))

  if ("and" %in% names(filter)) {
    mask <- rep(TRUE, n)
    for (f in filter$and) mask <- mask & .evalFilterMask(f, df)
    return(mask)
  }
  if ("or" %in% names(filter)) {
    mask <- rep(FALSE, n)
    for (f in filter$or) mask <- mask | .evalFilterMask(f, df)
    return(mask)
  }

  var <- tolower(filter$var %||% "")
  op <- tolower(filter$op %||% "")
  value <- filter$value
  if (!nzchar(var) || !var %in% names(df)) return(rep(FALSE, n))
  col <- df[[var]]

  is_date_col <- inherits(col, "Date")
  is_datetime_col <- inherits(col, c("POSIXct", "POSIXlt"))
  typed_value <- function(x, label = "filter value") {
    if (is_date_col) return(.isoDate(x, label))
    if (is_datetime_col) {
      date <- .isoDate(x, label)
      timezone <- attr(col, "tzone")
      if (is.null(timezone) || length(timezone) == 0L || !nzchar(timezone[1])) {
        timezone <- "UTC"
      }
      return(as.POSIXct(date, tz = timezone[1]))
    }
    suppressWarnings(as.numeric(x))
  }
  typed_values <- function(x, label = "filter value") {
    values <- unlist(x, use.names = FALSE)
    if (is_date_col) {
      parsed <- lapply(seq_along(values), function(i) {
        .isoDate(values[i], paste0(label, " ", i))
      })
      return(as.Date(vapply(parsed, as.character, character(1))))
    }
    if (is_datetime_col) {
      timezone <- attr(col, "tzone")
      if (is.null(timezone) || length(timezone) == 0L || !nzchar(timezone[1])) {
        timezone <- "UTC"
      }
      parsed <- lapply(seq_along(values), function(i) {
        .isoDate(values[i], paste0(label, " ", i))
      })
      return(as.POSIXct(vapply(parsed, as.character, character(1)),
                        tz = timezone[1]))
    }
    values
  }

  between_mask <- function() {
    values <- unlist(value, use.names = FALSE)
    if (length(values) != 2L) return(rep(FALSE, n))
    lower <- typed_value(values[1], "filter lower bound")
    upper <- typed_value(values[2], "filter upper bound")
    if (is_datetime_col) {
      # ISO bounds denote whole calendar days. Use an exclusive next-midnight
      # upper bound so events later on the stated end date are retained.
      timezone <- attr(col, "tzone")
      if (is.null(timezone) || length(timezone) == 0L || !nzchar(timezone[1])) {
        timezone <- "UTC"
      }
      next_midnight <- as.POSIXct(
        .isoDate(values[2], "filter upper bound") + 1L, tz = timezone[1]
      )
      return(col >= lower & col < next_midnight)
    }
    col >= lower & col <= upper
  }

  mask <- switch(op,
    "==" =, "eq" = col == if (is_date_col || is_datetime_col) {
      typed_value(value)
    } else value,
    "!=" =, "ne" = col != if (is_date_col || is_datetime_col) {
      typed_value(value)
    } else value,
    ">=" =, "gte" = col >= typed_value(value),
    "<=" =, "lte" = col <= typed_value(value),
    ">"  =, "gt"  = col > typed_value(value),
    "<"  =, "lt"  = col < typed_value(value),
    "in" = !is.na(col) & col %in% typed_values(value),
    "not_in" = !is.na(col) & !(col %in% typed_values(value)),
    "between" = between_mask(),
    "is_null" = is.na(col),
    "not_null" = !is.na(col),
    "value_bin" = col >= as.numeric(value$lower) & col < as.numeric(value$upper),
    rep(FALSE, n)
  )
  mask[is.na(mask)] <- FALSE
  mask
}

#' Compute person-level features from event data
#'
#' @param df Data frame in long format
#' @param table Character; source table name
#' @param specs Named list of feature specifications
#' @param person_ids Optional complete person roster. Persons with no matching
#'   records receive zero for occurrence features and NA for value features.
#' @param date_col Optional OMOP event date column resolved from the blueprint.
#' @param default_concept_col Optional domain concept column resolved from the
#'   blueprint; individual specs may override it with \code{concept_col}.
#' @return Data frame with one row per person
#' @keywords internal
.toFeatures <- function(df, table, specs = NULL, person_ids = NULL,
                        date_col = NULL, default_concept_col = NULL) {
  if (!"person_id" %in% names(df)) return(df)

  # Find concept column
  possible <- grep("_concept_id$", names(df), value = TRUE)
  possible <- possible[!grepl("_type_concept_id$|_source_concept_id$", possible)]
  concept_col <- if (!is.null(default_concept_col) &&
                     default_concept_col %in% names(df)) {
    default_concept_col
  } else if (length(possible) > 0) {
    possible[1]
  } else {
    NULL
  }

  persons <- if (!is.null(person_ids)) unique(person_ids) else unique(df$person_id)
  features <- data.frame(person_id = persons, stringsAsFactors = FALSE)

  # Occurrence/count columns whose absence means 0 (not missing). When this
  # frame is later left-joined onto a full person roster, the merge introduces
  # NAs for persons with no matching events; the caller uses this attribute to
  # restore them to 0. Value-based features are intentionally excluded — for
  # them an absent measurement is a genuine NA.
  zero_fill_cols <- character(0)
  zero_fill_types <- c("boolean", "count", "n_distinct",
                       "abnormal_high", "abnormal_low")

  supported_types <- c(
    "boolean", "count", "mean_value", "min_value", "max_value",
    "first_value", "latest_value", "sum_value", "time_since",
    "drug_duration", "sd_value", "cv_value", "slope_value",
    "abnormal_high", "abnormal_low", "gap_max_days", "gap_mean_days",
    "duration_sum", "n_distinct"
  )

  max_feature_specs <- .extractionCap("dsomop.max_feature_specs", 1000L)
  if (!is.null(specs) && length(specs) > max_feature_specs) {
    stop("Feature specification count exceeds the server cap of ",
         max_feature_specs, ".", call. = FALSE)
  }
  max_output_columns <- .extractionCap(
    "dsomop.max_output_columns", 5000L
  )
  if (is.null(specs) || length(specs) == 0L) {
    auto_concepts <- if (!is.null(concept_col) && concept_col %in% names(df)) {
      unique(df[[concept_col]][!is.na(df[[concept_col]])])
    } else {
      vector(mode = "logical", length = 0L)
    }
    max_auto_concepts <- .extractionCap(
      "dsomop.max_pivot_concepts", 1000L
    )
    if (length(auto_concepts) > max_auto_concepts ||
        1 + 5 * as.double(length(auto_concepts)) > max_output_columns) {
      stop("Automatic features exceed the server concept/output-column caps; ",
           "supply an explicit, narrower feature specification.",
           call. = FALSE)
    }
  } else if (1 + length(specs) > max_output_columns) {
    stop("Features exceed the server output-column cap of ",
         max_output_columns, ".", call. = FALSE)
  }

  feature_date_col <- if (!is.null(date_col) && date_col %in% names(df)) {
    date_col
  } else {
    preferred <- c(
      paste0(tolower(table), "_start_datetime"),
      paste0(tolower(table), "_start_date"),
      paste0(tolower(table), "_datetime"),
      paste0(tolower(table), "_date"),
      "condition_start_datetime", "condition_start_date",
      "drug_exposure_start_datetime", "drug_exposure_start_date",
      "procedure_datetime", "procedure_date", "device_exposure_start_datetime",
      "device_exposure_start_date", "measurement_datetime", "measurement_date",
      "observation_datetime", "observation_date", "visit_start_datetime",
      "visit_start_date", "specimen_datetime", "specimen_date",
      "note_datetime", "note_date", "death_datetime", "death_date",
      "condition_era_start_date", "drug_era_start_date", "dose_era_start_date",
      "observation_period_start_date", "payer_plan_period_start_date"
    )
    hit <- intersect(preferred, names(df))
    if (length(hit) > 0L) hit[1] else NULL
  }

  feature_pk_col <- {
    candidates <- c("dsomop_event_order_id", paste0(tolower(table), "_id"),
                    if (tolower(table) %in% c("person", "death")) "person_id")
    hit <- intersect(candidates, names(df))
    if (length(hit) > 0L) hit[1] else NULL
  }

  as_feature_date <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.POSIXct(x))
    if (!is.null(feature_date_col) && grepl("_datetime$", feature_date_col)) {
      return(suppressWarnings(as.POSIXct(x, tz = "UTC")))
    }
    suppressWarnings(as.Date(x))
  }

  select_ordered_event <- function(data, latest, feature_name) {
    if (is.null(feature_date_col) || !feature_date_col %in% names(data)) {
      stop("Feature '", feature_name,
           "' requires a usable OMOP date column for deterministic ordering.",
           call. = FALSE)
    }
    event_dates <- as_feature_date(data[[feature_date_col]])
    data <- data[!is.na(event_dates), , drop = FALSE]
    event_dates <- event_dates[!is.na(event_dates)]
    if (nrow(data) == 0L) return(data)

    date_key <- as.numeric(event_dates)
    selected_date <- if (latest) max(date_key) else min(date_key)
    tied_dates <- date_key == selected_date
    if (sum(tied_dates) > 1L) {
      if (is.null(feature_pk_col) || !feature_pk_col %in% names(data)) {
        stop("Feature '", feature_name, "' has same-date events but no OMOP ",
             "primary key is available for a deterministic tie break.",
             call. = FALSE)
      }
      tied_keys <- data[[feature_pk_col]][tied_dates]
      if (anyNA(tied_keys) || anyDuplicated(tied_keys)) {
        stop("Feature '", feature_name,
             "' has an invalid OMOP primary key among same-date events.",
             call. = FALSE)
      }
    }

    date_order <- if (latest) -date_key else date_key
    if (!is.null(feature_pk_col) && feature_pk_col %in% names(data)) {
      ord <- order(date_order, data[[feature_pk_col]], na.last = TRUE,
                   method = "radix")
    } else {
      ord <- order(date_order, na.last = TRUE, method = "radix")
    }
    data[ord[1], , drop = FALSE]
  }

  missing_feature <- function(source = NULL) {
    if (!is.null(source)) return(source[rep(NA_integer_, length(persons))])
    rep(NA_real_, length(persons))
  }

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
    if (!is.null(spec_type) && !spec_type %in% supported_types) {
      stop("Unknown or unsupported feature type: '", spec_type, "'.",
           call. = FALSE)
    }
    if (!is.null(spec_type) &&
        spec_type %in% c("first_value", "latest_value", "time_since") &&
        is.null(feature_date_col)) {
      stop("Feature '", label,
           "' requires a usable OMOP date column for deterministic ordering.",
           call. = FALSE)
    }

    # Boolean feature
    if (is.null(spec_type) || spec_type == "boolean") {
      features[[label]] <-
        as.integer(features$person_id %in% concept_data$person_id)
    }

    # Count feature
    if (is.null(spec_type) || spec_type == "count") {
      col_name <- if (identical(spec_type, "count")) label
                  else paste0(label, "_count")
      if (nrow(concept_data) > 0L) {
        count_df <- stats::aggregate(
          rep(1L, nrow(concept_data)),
          by = list(person_id = concept_data$person_id),
          FUN = length
        )
        names(count_df)[2] <- col_name
        features <- merge(features, count_df, by = "person_id", all.x = TRUE)
      } else {
        features[[col_name]] <- 0L
      }
      features[[col_name]][is.na(features[[col_name]])] <- 0L
    }

    # Number of distinct concepts after this spec's own concept/window/filter
    # scoping. An empty concept_set intentionally means all concepts in the
    # extracted table, matching the client constructor's contract.
    if (identical(spec_type, "n_distinct")) {
      distinct_col <- tolower(spec$concept_col %||% concept_col %||% "")
      if (nzchar(distinct_col) && distinct_col %in% names(concept_data) &&
          nrow(concept_data) > 0L) {
        nd_df <- stats::aggregate(
          concept_data[[distinct_col]],
          by = list(person_id = concept_data$person_id),
          FUN = function(x) length(unique(x[!is.na(x)]))
        )
        names(nd_df)[2] <- label
        features <- merge(features, nd_df, by = "person_id", all.x = TRUE)
      } else {
        features[[label]] <- 0L
      }
      features[[label]][is.na(features[[label]])] <- 0L
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
            select_ordered_event, latest = FALSE, feature_name = label))
          first_vals <- data.frame(
            person_id = first_df$person_id,
            val = first_df[[val_col]], stringsAsFactors = FALSE)
          names(first_vals)[2] <- label
          features <- merge(features, first_vals, by = "person_id", all.x = TRUE)
        }
        if (identical(spec_type, "latest_value")) {
          last_df <- do.call(rbind, lapply(
            split(num_data, num_data$person_id),
            select_ordered_event, latest = TRUE, feature_name = label))
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

    # Elapsed time since the latest matching event at or before a fixed
    # reference date. A missing reference would mean "cohort index" in the
    # client contract, which is episode-specific and therefore rejected until
    # feature grouping itself is episode-aware.
    if (identical(spec_type, "time_since")) {
      if (is.null(spec$reference_date)) {
        stop("Feature '", label, "' requires a fixed reference_date; cohort-",
             "index time_since is episode-aware and is not implemented for ",
             "person-level features.", call. = FALSE)
      }
      reference_date <- .isoDate(
        spec$reference_date, paste0("Feature '", label, "' reference_date")
      )
      unit <- spec$unit %||% "day"
      if (!is.character(unit) || length(unit) != 1L || is.na(unit) ||
          !tolower(unit) %in% c("day", "month")) {
        stop("Feature '", label, "' unit must be day or month.",
             call. = FALSE)
      }
      unit <- tolower(unit)

      event_dates <- as.Date(as_feature_date(concept_data[[feature_date_col]]))
      eligible <- !is.na(event_dates) & event_dates <= reference_date
      date_data <- data.frame(
        person_id = concept_data$person_id[eligible],
        event_date = event_dates[eligible], stringsAsFactors = FALSE
      )
      if (nrow(date_data) > 0L) {
        latest_dates <- stats::aggregate(
          date_data$event_date,
          by = list(person_id = date_data$person_id), FUN = max
        )
        if (identical(unit, "day")) {
          latest_dates$value <- as.integer(reference_date - latest_dates$x)
        } else {
          event_lt <- as.POSIXlt(latest_dates$x)
          reference_lt <- as.POSIXlt(rep(reference_date, nrow(latest_dates)))
          month_count <- (reference_lt$year - event_lt$year) * 12L +
            (reference_lt$mon - event_lt$mon)
          month_count <- month_count -
            as.integer(reference_lt$mday < event_lt$mday)
          latest_dates$value <- as.integer(month_count)
        }
        latest_dates$x <- NULL
        names(latest_dates)[2] <- label
        features <- merge(features, latest_dates, by = "person_id", all.x = TRUE)
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
              stats::coef(stats::lm(x[[val_col]] ~ x$.date_num))[2]
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

    # A declared spec always owns a stable output column, even when there are no
    # matching rows or its value column is entirely missing. Occurrence features
    # use structural zero; value/temporal features use missingness.
    if (!is.null(spec_type) && !label %in% names(features)) {
      if (spec_type %in% zero_fill_types) {
        features[[label]] <- 0L
      } else {
        source <- if (spec_type %in% c("first_value", "latest_value") &&
                      val_col %in% names(concept_data)) {
          concept_data[[val_col]]
        } else {
          NULL
        }
        features[[label]] <- missing_feature(source)
      }
    }

    features
  }

  if (!is.null(specs) && length(specs) > 0) {
    # Spec-driven path: one named column-group per spec, evaluated over the
    # UNION of the spec's concept set ("any of set" semantics). A boolean spec
    # over {a, b, c} yields a single column that is TRUE when the person has a
    # record for ANY of a, b, c; a count spec yields the total across the set.
    spec_names <- names(specs)
    for (si in seq_along(specs)) {
      spec <- specs[[si]]
      if (is.null(spec)) next

      # Resolve the output column name: an explicit spec$name wins, else the
      # list key the spec was declared under (features = list(nsaid = ...)).
      if (is.null(spec$name) || !nzchar(spec$name)) {
        key <- if (!is.null(spec_names)) spec_names[[si]] else ""
        if (!is.na(key) && nzchar(key)) spec$name <- key
      }

      # Rows matching ANY concept in this spec's set. A per-spec concept_col
      # override (e.g. scoping by unit_concept_id) wins over the table's domain
      # concept column for THIS spec only. Without a concept column (table has no
      # *_concept_id), the spec applies to every row.
      cs <- spec$concept_set
      if (is.list(cs) && !is.null(cs$concepts)) cs <- cs$concepts
      cs <- suppressWarnings(as.integer(unlist(cs, use.names = FALSE)))
      cs <- cs[!is.na(cs)]
      spec_concept_col <- if (!is.null(spec$concept_col))
        tolower(spec$concept_col) else concept_col
      if (!is.null(spec$concept_col) &&
          (length(spec_concept_col) != 1L || is.na(spec_concept_col) ||
           !nzchar(spec_concept_col) || !spec_concept_col %in% names(df))) {
        stop("Feature '", spec$name %||% spec_names[[si]] %||% si,
             "' concept_col is unavailable in the extracted table.",
             call. = FALSE)
      }
      if (!is.null(spec_concept_col) && spec_concept_col %in% names(df) &&
          length(cs) > 0) {
        spec_data <- df[df[[spec_concept_col]] %in% cs, , drop = FALSE]
      } else {
        spec_data <- df
      }

      # A direct server-side plan may attach an index-relative window to one
      # feature spec. Apply it only when the extraction actually produced the
      # required relative-day coordinate; otherwise reject rather than silently
      # broadening the feature to all time.
      if (!is.null(spec$time_window)) {
        window <- spec$time_window
        if (!is.list(window) || is.null(names(window)) ||
            any(!nzchar(names(window))) || anyDuplicated(names(window)) ||
            length(setdiff(names(window), c("start", "end"))) > 0L ||
            (is.null(window$start) && is.null(window$end))) {
          stop("Feature time_window must be a named start/end day window.",
               call. = FALSE)
        }
        normalize_bound <- function(value, name) {
          if (is.null(value)) return(NULL)
          numeric_value <- suppressWarnings(as.numeric(value))
          integer_value <- suppressWarnings(as.integer(value))
          if (length(value) != 1L || length(numeric_value) != 1L ||
              !is.finite(numeric_value) || length(integer_value) != 1L ||
              is.na(integer_value) || numeric_value != integer_value) {
            stop("Feature time_window ", name,
                 " must be one integer day offset.", call. = FALSE)
          }
          integer_value
        }
        window_start <- normalize_bound(window$start, "start")
        window_end <- normalize_bound(window$end, "end")
        if (!is.null(window_start) && !is.null(window_end) &&
            window_start > window_end) {
          stop("Feature time_window start must not be after end.",
               call. = FALSE)
        }
        if (!"days_from_index" %in% names(spec_data)) {
          stop("Feature time_window requires days_from_index; extract through ",
               "a cohort-anchored index_window.", call. = FALSE)
        }
        keep_window <- rep(TRUE, nrow(spec_data))
        if (!is.null(window_start)) {
          keep_window <- keep_window &
            spec_data$days_from_index >= window_start
        }
        if (!is.null(window_end)) {
          keep_window <- keep_window & spec_data$days_from_index <= window_end
        }
        keep_window[is.na(keep_window)] <- FALSE
        spec_data <- spec_data[keep_window, , drop = FALSE]
      }

      # Per-spec row filter (e.g. unit/type slice) scopes THIS feature's rows
      # only, so mutually-exclusive slices on one table become independent
      # columns instead of one contradictory AND. Evaluated in-memory against the
      # already-extracted (disclosure-filtered) frame.
      if (!is.null(spec$filter)) {
        feature_filter_cols <- setdiff(names(spec_data), .identifierColumns())
        .assertCustomFilterSafe(spec$filter, feature_filter_cols)
        keep <- .evalFilterMask(spec$filter, spec_data)
        spec_data <- spec_data[keep, , drop = FALSE]
      }

      concept_tag <- if (length(cs) > 0) paste(cs, collapse = "_") else "all"
      features <- .add_feature(features, spec, spec_data, concept_tag, df)
      if (!is.null(spec$type) && spec$type %in% zero_fill_types &&
          !is.null(spec$name) && nzchar(spec$name)) {
        zero_fill_cols <- c(zero_fill_cols, spec$name)
      }
    }
  } else if (!is.null(concept_col) && concept_col %in% names(df)) {
    # No specs: emit one column per distinct concept (backward compatible).
    concepts <- unique(df[[concept_col]])

    for (concept in concepts) {
      concept_str <- as.character(concept)
      concept_data <- df[df[[concept_col]] == concept, , drop = FALSE]
      features <- .add_feature(features, NULL, concept_data, concept_str, df)
    }
  } else {
    if (nrow(df) > 0L) {
      count_df <- stats::aggregate(
        rep(1, nrow(df)),
        by = list(person_id = df$person_id),
        FUN = sum
      )
      names(count_df)[2] <- "n_records"
      features <- merge(features, count_df, by = "person_id", all.x = TRUE)
    } else {
      features$n_records <- 0L
    }
    features$n_records[is.na(features$n_records)] <- 0L
  }

  if (length(persons) > 0L && nrow(features) > 0L) {
    features <- features[match(persons, features$person_id), , drop = FALSE]
    rownames(features) <- NULL
  }
  zero_fill_cols <- intersect(unique(zero_fill_cols), names(features))
  if (length(zero_fill_cols) > 0) {
    attr(features, "omop_zero_fill") <- zero_fill_cols
  }
  if (ncol(features) > max_output_columns) {
    stop("Features exceeded the server output-column cap after reduction.",
         call. = FALSE)
  }
  features
}

#' Compute episode-level features from event data
#'
#' Reuses the reviewed person-feature reducers after replacing their grouping
#' key with the stable cohort episode key. The original person identifier is
#' restored from the independently materialized cohort roster, so recurrent
#' entries remain linkable without collapsing them into one person row.
#'
#' @param df Event data containing \code{person_id} and \code{cohort_row_id}.
#' @param table Character source table name.
#' @param specs Named feature specifications.
#' @param roster Data frame with one \code{cohort_row_id}/\code{person_id} pair per
#'   cohort episode, including episodes with no qualifying event.
#' @param date_col Optional event date column.
#' @param default_concept_col Optional default concept column.
#' @return Data frame with one row per cohort episode.
#' @keywords internal
.toEpisodeFeatures <- function(df, table, specs = NULL, roster,
                               date_col = NULL,
                               default_concept_col = NULL) {
  required_roster <- c("cohort_row_id", "person_id")
  if (!is.data.frame(roster) ||
      !all(required_roster %in% names(roster)) ||
      anyNA(roster$cohort_row_id) || anyDuplicated(roster$cohort_row_id)) {
    stop("Episode feature roster must uniquely map cohort_row_id to person_id.",
         call. = FALSE)
  }
  if (!all(c("person_id", "cohort_row_id") %in% names(df))) {
    stop("Episode-grain features require person_id and cohort_row_id in events.",
         call. = FALSE)
  }
  unknown_episode <- setdiff(unique(df$cohort_row_id), roster$cohort_row_id)
  unknown_episode <- unknown_episode[!is.na(unknown_episode)]
  if (length(unknown_episode) > 0L || anyNA(df$cohort_row_id)) {
    stop("Event rows contain an invalid cohort_row_id.", call. = FALSE)
  }

  episode_df <- df
  episode_df$person_id <- episode_df$cohort_row_id
  features <- .toFeatures(
    episode_df, table, specs,
    person_ids = roster$cohort_row_id,
    date_col = date_col,
    default_concept_col = default_concept_col
  )
  zero_fill <- attr(features, "omop_zero_fill", exact = TRUE)
  names(features)[names(features) == "person_id"] <- "cohort_row_id"
  features$cohort_row_id <- as.integer(features$cohort_row_id)
  features$person_id <- roster$person_id[
    match(features$cohort_row_id, roster$cohort_row_id)
  ]
  keep <- c("cohort_row_id", "person_id",
            setdiff(names(features), c("cohort_row_id", "person_id")))
  features <- features[, keep, drop = FALSE]
  if (length(zero_fill) > 0L) {
    attr(features, "omop_zero_fill") <- zero_fill
  }
  features
}

#' Transform event data to FeatureExtraction-style sparse format
#'
#' Produces a named list with \code{covariates} (sparse triplet),
#' \code{covariateRef} (reference table), and \code{personRef} (the complete
#' row-to-person roster). This is a dsOMOP output shape, not an OHDSI
#' FeatureExtraction CovariateData object. CovariateId scheme:
#' \code{conceptId * 1000 + analysisId} where analysisId 1=binary,
#' 2=count, 3=mean, 4=min, 5=max.
#'
#' @param df Data frame in long format
#' @param table Character; source table name
#' @param person_ids Optional complete person roster for person grain. People
#'   without qualifying events remain in \code{personRef} and have implicit
#'   zero covariates.
#' @param roster Optional episode roster with \code{cohort_row_id} and
#'   \code{person_id}.
#' @param grain Aggregation unit, \code{"person"} or \code{"episode"}.
#' @return Named list with \code{covariates}, \code{covariateRef}, and
#'   \code{personRef}
#' @keywords internal
.toSparse <- function(df, table, person_ids = NULL, roster = NULL,
                      grain = "person") {
  if (!is.data.frame(df)) {
    stop("Sparse input must be a data frame.", call. = FALSE)
  }
  if (nrow(df) > 0L && !"person_id" %in% names(df)) {
    stop("Sparse event data require person_id.", call. = FALSE)
  }
  if (!is.character(grain) || length(grain) != 1L || is.na(grain) ||
      !tolower(grain) %in% c("person", "episode")) {
    stop("Sparse grain must be 'person' or 'episode'.", call. = FALSE)
  }
  grain <- tolower(grain)

  if (identical(grain, "episode")) {
    if (!is.data.frame(roster) ||
        !all(c("cohort_row_id", "person_id") %in% names(roster)) ||
        anyNA(roster$cohort_row_id) || anyNA(roster$person_id) ||
        anyDuplicated(roster$cohort_row_id)) {
      stop("Episode sparse output requires a complete unique episode roster.",
           call. = FALSE)
    }
    if (nrow(df) > 0L && !"cohort_row_id" %in% names(df)) {
      stop("Episode sparse events require cohort_row_id.", call. = FALSE)
    }
    observed_keys <- if ("cohort_row_id" %in% names(df)) {
      unique(df$cohort_row_id[!is.na(df$cohort_row_id)])
    } else integer(0)
    unknown_keys <- setdiff(as.character(observed_keys),
                            as.character(roster$cohort_row_id))
    if (length(unknown_keys) > 0L) {
      stop("Sparse events contain episodes outside the declared roster.",
           call. = FALSE)
    }
    if (nrow(df) > 0L) {
      expected_person <- roster$person_id[
        match(df$cohort_row_id, roster$cohort_row_id)
      ]
      if (anyNA(df$cohort_row_id) || anyNA(expected_person) ||
          any(as.character(df$person_id) != as.character(expected_person))) {
        stop("Sparse episode-to-person linkage is inconsistent with the roster.",
             call. = FALSE)
      }
    }
    person_ref <- data.frame(
      rowId = seq_len(nrow(roster)),
      cohort_row_id = roster$cohort_row_id,
      person_id = roster$person_id,
      stringsAsFactors = FALSE
    )
    row_map <- stats::setNames(person_ref$rowId,
                               as.character(person_ref$cohort_row_id))
    df$dsomop_sparse_row_key <- df$cohort_row_id
  } else {
    observed_persons <- if ("person_id" %in% names(df)) {
      unique(df$person_id[!is.na(df$person_id)])
    } else {
      vector(mode = if (is.null(person_ids)) "logical" else typeof(person_ids),
             length = 0L)
    }
    persons <- if (is.null(person_ids)) observed_persons else unique(person_ids)
    if (anyNA(persons)) {
      stop("Sparse person roster cannot contain missing identifiers.",
           call. = FALSE)
    }
    unknown_persons <- setdiff(as.character(observed_persons),
                               as.character(persons))
    if (length(unknown_persons) > 0L) {
      stop("Sparse events contain people outside the declared roster.",
           call. = FALSE)
    }
    person_ref <- data.frame(
      rowId = seq_along(persons),
      person_id = persons,
      stringsAsFactors = FALSE
    )
    row_map <- stats::setNames(seq_along(persons), as.character(persons))
    df$dsomop_sparse_row_key <- df$person_id
  }

  # Find concept column
  possible <- grep("_concept_id$", names(df), value = TRUE)
  possible <- possible[!grepl("_type_concept_id$|_source_concept_id$", possible)]
  concept_col <- if (length(possible) > 0) possible[1] else NULL

  covariates <- data.frame(rowId = integer(0), covariateId = numeric(0),
                           covariateValue = numeric(0),
                           stringsAsFactors = FALSE)
  covariateRef <- data.frame(covariateId = numeric(0),
                             covariateName = character(0),
                             analysisId = integer(0),
                             conceptId = integer(0),
                             stringsAsFactors = FALSE)

  if (!is.null(concept_col) && concept_col %in% names(df)) {
    concepts <- sort(unique(df[[concept_col]][!is.na(df[[concept_col]])]))
    max_covariate_concepts <- .extractionCap(
      "dsomop.max_pivot_concepts", 1000L
    )
    if (length(concepts) > max_covariate_concepts) {
      stop("Sparse representation exceeds the server concept cap of ",
           max_covariate_concepts, ".", call. = FALSE)
    }

    for (cid in concepts) {
      concept_label <- .standardizeName(as.character(cid))
      if (is.na(concept_label) || concept_label == "") {
        concept_label <- paste0("concept_", cid)
      }
      concept_data <- df[df[[concept_col]] == cid, , drop = FALSE]

      # Analysis 1: binary (ever/never)
      binary_rows <- unique(concept_data$dsomop_sparse_row_key)
      if (length(binary_rows) > 0) {
        cov_id <- as.numeric(cid) * 1000 + 1
        covariates <- rbind(covariates, data.frame(
          rowId = as.integer(row_map[as.character(binary_rows)]),
          covariateId = rep(cov_id, length(binary_rows)),
          covariateValue = rep(1, length(binary_rows)),
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
        by = list(row_key = concept_data$dsomop_sparse_row_key),
        FUN = length
      )
      if (nrow(count_agg) > 0) {
        cov_id <- as.numeric(cid) * 1000 + 2
        covariates <- rbind(covariates, data.frame(
          rowId = as.integer(row_map[as.character(count_agg$row_key)]),
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
              by = list(row_key = num_data$dsomop_sparse_row_key),
              FUN = analysis$fn
            )
            if (nrow(stat_agg) > 0) {
              cov_id <- as.numeric(cid) * 1000 + analysis$id
              covariates <- rbind(covariates, data.frame(
                rowId = as.integer(row_map[as.character(stat_agg$row_key)]),
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

  if (nrow(covariates) > 0L &&
      any(!covariates$rowId %in% person_ref$rowId)) {
    stop("Sparse covariates contain a rowId outside personRef.",
         call. = FALSE)
  }
  list(covariates = covariates, covariateRef = covariateRef,
       personRef = person_ref)
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
  index_age <- vapply(derived_specs, function(s) {
    identical(s$kind, "age") && identical(s$reference %||% "today", "index") &&
      is.null(s$reference_date)
  }, logical(1))

  if (any(index_age) && is.null(cohort_table)) {
    stop("Index-referenced age requires a cohort with an index episode.",
         call. = FALSE)
  }
  missing_fixed_date <- vapply(derived_specs, function(s) {
    (identical(s$kind, "age") &&
       identical(s$reference %||% "today", "today") &&
       is.null(s$reference_date)) ||
      (s$kind %in% c("prior_obs", "followup", "chads2", "chadsvasc") &&
         is.null(s$reference_date))
  }, logical(1))
  if (any(missing_fixed_date)) {
    bad <- vapply(derived_specs[missing_fixed_date], function(s) s$name,
                  character(1))
    stop("Derived variable(s) ", paste(bad, collapse = ", "),
         " require an explicit fixed reference_date for reproducibility.",
         call. = FALSE)
  }

  needs_person <- any(kinds %in% c("age", "sex_mf", "demo_missingness"))
  needs_obs <- any(kinds %in% c("obs_duration", "prior_obs", "followup"))
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

  op_table <- NULL
  if (needs_obs) {
    op_schema <- .resolveTableSchema(handle, "observation_period", "Clinical")
    op_table <- .qualifyTable(handle, "observation_period", op_schema)
  }

  # Filter to cohort person IDs
  where <- character(0)
  if (!is.null(person_ids) && length(person_ids) > 0) {
    ids_str <- .sqlIdList(person_ids)
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
  .assertMinPersons(handle = handle, sql = count_sql)

  df <- .executeQuery(handle, sql)
  obs_df <- NULL
  if (needs_obs) {
    obs_sql <- paste0(
      "SELECT op.person_id, op.observation_period_start_date, ",
      "op.observation_period_end_date FROM ", op_table, " op INNER JOIN ",
      person_table, " p ON p.person_id = op.person_id"
    )
    if (length(where) > 0L) {
      obs_sql <- paste0(obs_sql, " WHERE ", paste(where, collapse = " AND "))
    }
    obs_df <- .executeQuery(handle, obs_sql)
  }

  # Index-referenced values have cohort-episode grain. Join the canonical
  # episode map once, before computing any columns, so a recurrent cohort
  # yields one derived row per episode and every other person-level value is
  # replicated deterministically across those episodes.
  if (any(index_age)) {
    idx_sql <- paste0(
      "SELECT c.cohort_row_id, c.subject_id AS person_id, ",
      "c.cohort_start_date FROM ", .rankedCohortSql(cohort_table, handle),
      " AS c ORDER BY c.cohort_row_id"
    )
    idx_df <- .executeQuery(handle, idx_sql)
    idx_df$cohort_row_id <- as.integer(idx_df$cohort_row_id)
    idx_df$index_year <- as.integer(
      format(as.Date(idx_df$cohort_start_date), "%Y")
    )
    df <- merge(idx_df, df, by = "person_id", all.x = TRUE, sort = FALSE)
    df <- df[order(df$cohort_row_id), , drop = FALSE]
  }
  # A zero-row df (e.g. an empty population from an annihilating set-op) builds a
  # zero-ROW result with the full derived-column SCHEMA rather than NULL, so the
  # output surfaces as an empty data.frame instead of being dropped. The
  # column-building loop below is vectorized and produces 0-length columns
  # cleanly; comorbidity merges on an empty person set likewise return 0 rows.

  result <- data.frame(person_id = df$person_id, stringsAsFactors = FALSE)
  if ("cohort_row_id" %in% names(df)) {
    result$cohort_row_id <- df$cohort_row_id
    result$row_id <- df$cohort_row_id
    result <- result[, c("row_id", "cohort_row_id", "person_id"), drop = FALSE]
  }

  observation_value <- function(spec) {
    if (is.null(obs_df)) return(rep(NA_integer_, nrow(df)))
    policy <- tolower(spec$period_policy %||%
      if (identical(spec$kind, "obs_duration")) "total" else "containing")
    allowed <- if (identical(spec$kind, "obs_duration")) {
      c("total", "first", "last", "longest")
    } else {
      "containing"
    }
    if (!is.character(policy) || length(policy) != 1L || is.na(policy) ||
        !policy %in% allowed) {
      stop("Derived observation-period policy for '", spec$name,
           "' must be one of: ", paste(allowed, collapse = ", "), ".",
           call. = FALSE)
    }
    reference <- if (identical(policy, "containing")) {
      .isoDate(spec$reference_date,
               paste0("Derived ", spec$kind, " '", spec$name,
                      "' reference_date"))
    } else {
      NULL
    }
    groups <- split(seq_len(nrow(obs_df)), as.character(obs_df$person_id))
    vapply(as.character(df$person_id), function(person) {
      rows <- groups[[person]]
      if (is.null(rows) || length(rows) == 0L) return(NA_integer_)
      starts <- suppressWarnings(as.Date(
        obs_df$observation_period_start_date[rows]
      ))
      ends <- suppressWarnings(as.Date(
        obs_df$observation_period_end_date[rows]
      ))
      if (anyNA(starts) || anyNA(ends) || any(ends < starts)) {
        stop("Observation-period dates are invalid for derived variables.",
             call. = FALSE)
      }
      durations <- as.integer(ends - starts)
      if (identical(policy, "containing")) {
        covering <- which(starts <= reference & ends >= reference)
        if (length(covering) == 0L) return(NA_integer_)
        if (length(covering) > 1L) {
          stop("A derived observation-period reference is covered by ",
               "multiple periods.", call. = FALSE)
        }
        selected <- covering[[1L]]
        if (identical(spec$kind, "prior_obs")) {
          return(as.integer(reference - starts[[selected]]))
        }
        return(as.integer(ends[[selected]] - reference))
      }
      if (identical(policy, "total")) {
        ordered <- order(starts, ends)
        if (length(ordered) > 1L && any(
          starts[ordered[-1L]] <= ends[ordered[-length(ordered)]]
        )) {
          stop("Total observation duration requires non-overlapping periods.",
               call. = FALSE)
        }
        return(as.integer(sum(durations)))
      }
      selected <- switch(policy,
        first = order(starts, ends)[[1L]],
        last = order(starts, ends, decreasing = TRUE)[[1L]],
        longest = order(-durations, starts, ends)[[1L]]
      )
      durations[[selected]]
    }, integer(1L))
  }

  for (spec in derived_specs) {
    col_name <- spec$name

    if (spec$kind == "age") {
      if ("year_of_birth" %in% names(df)) {
        ref <- spec$reference %||% "today"
        if (!ref %in% c("today", "index")) {
          stop("Derived age reference must be 'today' or 'index'.",
               call. = FALSE)
        }
        if (!is.null(spec$reference_date)) {
          ref_date <- .isoDate(
            spec$reference_date,
            paste0("Derived age '", col_name, "' reference_date")
          )
          ref_year <- as.integer(format(ref_date, "%Y"))
        } else if (ref == "today") {
          stop("Derived age requires an explicit fixed reference_date.",
               call. = FALSE)
        } else {
          ref_year <- df$index_year
        }
        age_value <- as.integer(ref_year - df$year_of_birth)
        age_value[!is.na(age_value) & age_value < 0L] <- NA_integer_
        result[[col_name]] <- age_value
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
      result[[col_name]] <- observation_value(spec)

    } else if (spec$kind == "prior_obs") {
      result[[col_name]] <- observation_value(spec)

    } else if (spec$kind == "followup") {
      result[[col_name]] <- observation_value(spec)

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
        handle, spec$kind, unique(result$person_id),
        reference_date = spec$reference_date)
      zero_val <- if (spec$kind %in% c("hfrs", "dcsi")) 0 else 0L
      if (!is.null(score_df) && nrow(score_df) > 0) {
        result[[col_name]] <- score_df[[2]][
          match(result$person_id, score_df$person_id)
        ]
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
    stop(score_type, " score requires concept_relationship, but that table ",
         "is not present in the authorized vocabulary schema.", call. = FALSE)
  }

  # Resolve table names
  cr_table <- cr_row$qualified_name[1]
  concept_row <- bp$tables[bp$tables$table_name == "concept" &
                             bp$tables$present_in_db, , drop = FALSE]
  if (nrow(concept_row) == 0) {
    stop(score_type, " score requires the concept table.", call. = FALSE)
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
    stop(score_type, " score: no ICD-to-SNOMED mappings were found in ",
         "concept_relationship; refusing to emit misleading all-zero scores.",
         call. = FALSE)
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
.computeComorbidityScore <- function(handle, score_type, person_ids = NULL,
                                     reference_date = NULL) {
  if (is.null(person_ids) || length(person_ids) == 0) return(NULL)

  # --- Score definitions ---
  # Concept seeds and analysis IDs are traced to OHDSI FeatureExtraction
  # v3.14.0, commit 53266f0233c2ee7cae127e8669ad35b0d60406ae.
  #
  # This is a dsOMOP adapter, not an upstream-equivalent implementation:
  # FeatureExtraction uses condition_era, cohort-relative windows and
  # index-specific inclusion/exclusion rules. dsOMOP reads condition_occurrence,
  # uses its explicit reference_date where age is required, and applies the
  # local vocabulary/descendant rules below.

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
  # The positive seeds are traced to the official SQL, but dsOMOP expands every
  # seed locally and does not reproduce its selective descendant/exclusion SQL.
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

  if (score_type %in% c("charlson", "chads2", "chadsvasc")) {
    hierarchy_cache <- paste0("resolved_hierarchy_", score_type)
    expanded_categories <- handle[[hierarchy_cache]]
    if (is.null(expanded_categories)) {
      expanded_categories <- lapply(categories, function(category) {
        descendants <- .vocabGetDescendants(
          handle, category$concepts, include_self = TRUE
        )
        category$concepts <- unique(c(
          as.integer(category$concepts), as.integer(descendants$concept_id)
        ))
        category
      })
      handle[[hierarchy_cache]] <- expanded_categories
    }
    categories <- expanded_categories
  }

  if (!score_type %in% c("dcsi", "hfrs")) {
    scoring_mode <- "simple_weighted"
  }

  bp <- .buildBlueprint(handle)

  # Use one fixed dsOMOP event source on every site. Opportunistically mixing
  # condition_era where available changes score semantics across databases.
  available_tables <- "condition_occurrence"
  condition_row <- bp$tables[
    bp$tables$table_name == "condition_occurrence" & bp$tables$present_in_db,
    , drop = FALSE
  ]
  if (nrow(condition_row) != 1L) {
    stop(score_type, " score requires condition_occurrence.", call. = FALSE)
  }

  ids_str <- .sqlIdList(person_ids)

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
        person_max_tier <- stats::setNames(rep(0, length(person_ids)), person_ids)
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
      # The age component is always tied to a recipe-persisted date; silently
      # using the execution date would change a saved study definition.
      if (is.null(reference_date)) {
        stop(score_type, " requires an explicit reference_date for its age ",
             "component.", call. = FALSE)
      }
      ref_year <- as.integer(format(
        .isoDate(reference_date, paste0(score_type, " reference_date")), "%Y"
      ))
      person_df$age <- ref_year - person_df$year_of_birth
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
    "concept_ancestor_expanded"
  }
  attr(score_result, "score_meta") <- list(
    score_type = score_type,
    analysis_id = analysis_ids[[score_type]],
    adapter = "dsOMOP",
    upstream = "OHDSI/FeatureExtraction",
    upstream_release = "v3.14.0",
    upstream_commit = "53266f0233c2ee7cae127e8669ad35b0d60406ae",
    upstream_equivalent = FALSE,
    matching = matching_type,
    divergence = paste0(
      "dsOMOP condition_occurrence adapter with local vocabulary, ",
      "descendant and reference-date semantics"
    ),
    note = if (matching_type == "vocabulary_resolved") {
      "ICD codes resolved to SNOMED via concept_relationship"
    } else {
      "Referenced ancestor seeds expanded locally via concept_ancestor"
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
#' @param age_breaks Optional public age-grid coarsening negotiated by a
#'   federation.
#' @return Data frame with one row per cohort member
#' @keywords internal
.extractBaseline <- function(handle, cohort_table, columns = NULL,
                             derived = NULL, translate_concepts = TRUE,
                             age_breaks = NULL) {
  if (is.null(cohort_table)) {
    warning("Baseline output requires a cohort; returning NULL.", call. = FALSE)
    return(NULL)
  }

  bp <- .buildBlueprint(handle)

  person_schema <- .resolveTableSchema(handle, "person", "Clinical")
  person_table <- .qualifyTable(handle, "person", person_schema)

  op_schema <- .resolveTableSchema(handle, "observation_period", "Clinical")
  op_table <- .qualifyTable(handle, "observation_period", op_schema)

  derived <- tolower(derived %||% character(0))
  requested_columns <- if (is.null(columns)) {
    c("gender_concept_id", "race_concept_id")
  } else {
    tolower(columns)
  }
  birth_components <- c("year_of_birth", "month_of_birth", "day_of_birth",
                        "birth_datetime")
  requested_birth <- intersect(requested_columns, birth_components)
  if (length(requested_birth) > 0L) {
    stop("Disclosive: exact birth components are blocked; request derived ",
         "age_at_index to receive disclosure-controlled age groups.",
         call. = FALSE)
  }
  columns <- unique(c(
    requested_columns,
    if ("age_at_index" %in% derived) "year_of_birth" else character(0)
  ))

  # Apply the same explicit-column contract as .compileSelect. Baseline uses
  # bespoke join SQL, so it must not bypass the blueprint blocklist or silently
  # reduce an unknown request to person_id-only output.
  person_cols <- bp$columns[["person"]]
  if (!is.null(person_cols)) {
    avail_person <- person_cols$column_name
    missing_columns <- setdiff(columns, avail_person)
    if (length(missing_columns) > 0L) {
      stop("Baseline person column(s) not found: ",
           paste(missing_columns, collapse = ", "), ".", call. = FALSE)
    }
    blocked <- union(
      person_cols$column_name[person_cols$is_blocked],
      intersect(c("month_of_birth", "day_of_birth", "birth_datetime"),
                avail_person)
    )
    # `year_of_birth` may be added internally solely to derive an age group and
    # is removed before release. Apply the public blocklist only to columns the
    # caller actually requested; requested birth components already fail above.
    requested_blocked <- intersect(requested_columns, blocked)
    if (length(requested_blocked) > 0L) {
      stop("Disclosive: baseline person column(s) '",
           paste(requested_blocked, collapse = "', '"),
           "' are blocked.", call. = FALSE)
    }
    select_person_cols <- columns
  } else {
    stop("Person columns are unavailable for baseline extraction.",
         call. = FALSE)
  }

  person_select <- if (length(select_person_cols) > 0) {
    paste0(", ", paste(paste0("p.", select_person_cols), collapse = ", "))
  } else {
    ""
  }

  sql <- paste0(
    "SELECT c.cohort_row_id, c.subject_id AS person_id, ",
    "c.cohort_start_date, c.cohort_end_date",
    person_select, ", ",
    "op.observation_period_start_date, op.observation_period_end_date",
    " FROM ", .rankedCohortSql(cohort_table, handle), " AS c",
    " INNER JOIN ", person_table, " AS p ON p.person_id = c.subject_id",
    " LEFT JOIN ", op_table, " AS op",
    " ON op.person_id = c.subject_id",
    " AND c.cohort_start_date >= op.observation_period_start_date",
    " AND c.cohort_start_date <= op.observation_period_end_date",
    " ORDER BY c.cohort_row_id"
  )

  .assertMinPersons(handle = handle,
    sql = paste0("SELECT COUNT(DISTINCT subject_id) AS n_persons FROM ",
                 cohort_table))

  result <- .executeQuery(handle, sql)
  if (nrow(result) == 0) return(result)
  if (anyDuplicated(result$cohort_row_id)) {
    stop("Baseline extraction found multiple matching observation periods for ",
         "one cohort episode; one-row-per-episode output is ambiguous.",
         call. = FALSE)
  }

  # row_id remains the legacy name; cohort_row_id is the explicit, shared
  # episode identity used by every longitudinal output.
  result$cohort_row_id <- as.integer(result$cohort_row_id)
  result$row_id <- result$cohort_row_id

  # Compute derived fields
  if ("age_at_index" %in% derived && "year_of_birth" %in% names(result)) {
    index_year <- as.integer(format(as.Date(result$cohort_start_date), "%Y"))
    result$age_group <- .computeAgeGroups(
      result$year_of_birth, index_year, age_breaks = age_breaks,
      person_id = result$person_id
    )
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
  keep <- c("row_id", "cohort_row_id", "person_id", select_person_cols,
            derived_keep)
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
#' @param filters List; optional custom filter DSL tree narrowing which outcome
#'   events qualify (e.g. a value range). Validated fail-closed via
#'   \code{\link{.assertCustomFilterSafe}} and ANDed into the outcome-event
#'   SELECT. It only restricts events; the cohort (one row per member) and its
#'   distinct-person gate are unaffected, so this can never widen disclosure.
#' @return Data frame with row_id/cohort_row_id, person_id, event (0/1),
#'   time_to_event_days
#' @keywords internal
.extractSurvival <- function(handle, cohort_table, outcome, tar = NULL,
                              event_order = "first", filters = NULL) {
  if (is.null(cohort_table)) {
    warning("Survival output requires a cohort; returning NULL.", call. = FALSE)
    return(NULL)
  }

  if (!is.character(event_order) || length(event_order) != 1L ||
      is.na(event_order) || !tolower(event_order) %in% c("first", "last")) {
    stop("event_order must be 'first' or 'last'.", call. = FALSE)
  }
  event_order <- tolower(event_order)

  tar <- tar %||% list()
  if (!is.list(tar) ||
      (length(tar) > 0L && (is.null(names(tar)) || any(!nzchar(names(tar))) ||
                             anyDuplicated(names(tar))))) {
    stop("tar must be a uniquely named list.", call. = FALSE)
  }
  unknown_tar <- setdiff(names(tar),
                         c("start_offset", "end_offset", "censoring"))
  if (length(unknown_tar) > 0L) {
    stop("Unknown TAR field(s): ", paste(unknown_tar, collapse = ", "), ".",
         call. = FALSE)
  }
  exact_offset <- function(value, field, default = NULL) {
    if (is.null(value)) return(default)
    numeric_value <- suppressWarnings(as.numeric(value))
    integer_value <- suppressWarnings(as.integer(value))
    if (length(value) != 1L || length(numeric_value) != 1L ||
        !is.finite(numeric_value) || length(integer_value) != 1L ||
        is.na(integer_value) || numeric_value != integer_value) {
      stop("tar$", field, " must be one finite exact integer day offset.",
           call. = FALSE)
    }
    integer_value
  }
  start_offset <- exact_offset(tar$start_offset, "start_offset", 0L)
  end_offset <- exact_offset(tar$end_offset, "end_offset", NULL)
  if (!is.null(end_offset) && start_offset > end_offset) {
    stop("tar$start_offset must not be after tar$end_offset.", call. = FALSE)
  }
  censoring <- tolower(tar$censoring %||% "cohort_end")
  if (!is.character(censoring) || length(censoring) != 1L ||
      is.na(censoring) || !identical(censoring, "cohort_end")) {
    stop("tar$censoring currently supports only 'cohort_end'.", call. = FALSE)
  }
  if (!is.null(end_offset) && "censoring" %in% names(tar)) {
    stop("tar$censoring cannot be combined with an explicit end_offset.",
         call. = FALSE)
  }

  if (!is.list(outcome) || is.null(names(outcome)) ||
      any(!nzchar(names(outcome))) || anyDuplicated(names(outcome)) ||
      !setequal(names(outcome), c("table", "concept_set")) ||
      length(names(outcome)) != 2L) {
    stop("outcome must contain exactly table and concept_set.", call. = FALSE)
  }

  cohort_table <- .validateIdentifier(cohort_table, "survival cohort table")
  bp <- .buildBlueprint(handle)

  # Query 1: cohort members
  cohort_sql <- paste0(
    "SELECT c.cohort_row_id, c.subject_id AS person_id, ",
    "c.cohort_start_date, c.cohort_end_date",
    " FROM ", .rankedCohortSql(cohort_table, handle), " AS c",
    " ORDER BY c.cohort_row_id"
  )

  .assertMinPersons(handle = handle,
    sql = paste0("SELECT COUNT(DISTINCT subject_id) AS n_persons FROM ",
                 cohort_table))

  cohort_df <- .executeQuery(handle, cohort_sql)
  if (nrow(cohort_df) == 0) return(cohort_df)

  cohort_start <- suppressWarnings(as.Date(cohort_df$cohort_start_date))
  cohort_end <- suppressWarnings(as.Date(cohort_df$cohort_end_date))
  if (any(is.na(cohort_start)) || any(is.na(cohort_end)) ||
      any(cohort_end < cohort_start)) {
    stop("Survival cohort episodes require valid dates with end >= start.",
         call. = FALSE)
  }

  cohort_df$cohort_row_id <- as.integer(cohort_df$cohort_row_id)
  cohort_df$row_id <- cohort_df$cohort_row_id

  # Query 2: outcome events
  outcome_table <- tolower(.validateIdentifier(outcome$table, "outcome table"))
  outcome_concepts <- .resolveConceptSet(handle, outcome$concept_set)
  if (length(outcome_concepts) == 0L) {
    stop("Outcome concept_set resolved to no concepts.", call. = FALSE)
  }

  tbl_row <- bp$tables[bp$tables$table_name == outcome_table, , drop = FALSE]
  if (nrow(tbl_row) == 0 || !tbl_row$present_in_db[1]) {
    stop("Outcome table '", outcome$table, "' not found.", call. = FALSE)
  }

  date_col <- .getDateColumn(bp, outcome_table)
  concept_col <- .getDomainConceptColumn(bp, outcome_table)
  outcome_columns <- bp$columns[[outcome_table]]$column_name
  if (is.null(date_col) || !date_col %in% outcome_columns ||
      is.null(concept_col) || !concept_col %in% outcome_columns) {
    stop("Outcome table requires reviewed event-date and domain-concept columns.",
         call. = FALSE)
  }
  schema <- .resolveTableSchema(handle, outcome_table,
                                 tbl_row$schema_category[1])
  qualified <- .qualifyTable(handle, outcome_table, schema)

  concept_ids_str <- .sqlIdList(outcome_concepts)

  outcome_sql <- paste0(
    "SELECT t.person_id, t.", date_col, " AS outcome_date",
    " FROM ", qualified, " AS t",
    " WHERE t.", concept_col, " IN (", concept_ids_str, ")",
    " AND EXISTS (SELECT 1 FROM ", cohort_table,
    " AS c WHERE c.subject_id = t.person_id)"
  )

  # Custom filter DSL on the outcome events. Validated fail-closed (identifier/
  # blocked columns and narrow fingerprinting ops rejected) before any SQL is
  # emitted, then ANDed onto the outcome WHERE (alias t). Only narrows the
  # qualifying events; the cohort and its .assertMinPersons gate above are
  # unaffected, so the suppression can never be bypassed.
  if (!is.null(filters) && length(filters) > 0) {
    valid_cols <- .filterableColumns(bp, outcome_table)
    .assertCustomFilterSafe(filters, valid_cols, handle = handle,
                            table = outcome_table)
    filter_sql <- .compileFilter(handle, filters, "t", valid_cols)
    if (!is.null(filter_sql) && nchar(filter_sql) > 0) {
      outcome_sql <- paste0(outcome_sql, " AND ", filter_sql)
    }
  }

  outcome_df <- .executeQuery(handle, outcome_sql)

  # TAR boundaries. Inputs were validated as exact integers before any query;
  # cohort dates were validated above rather than allowing NA/negative follow-up
  # to be interpreted as ordinary censoring.
  cohort_df$tar_start <- cohort_start + start_offset
  if (!is.null(end_offset)) {
    cohort_df$tar_end <- cohort_start + end_offset
  } else {
    cohort_df$tar_end <- cohort_end
  }
  if (any(is.na(cohort_df$tar_end)) ||
      any(cohort_df$tar_end < cohort_df$tar_start)) {
    stop("Survival TAR end must not be before TAR start for any episode.",
         call. = FALSE)
  }

  # Merge and process outcomes
  if (nrow(outcome_df) > 0) {
    raw_outcome_date <- outcome_df$outcome_date
    outcome_df$outcome_date <- suppressWarnings(as.Date(raw_outcome_date))
    if (any(!is.na(raw_outcome_date) & is.na(outcome_df$outcome_date))) {
      stop("Outcome table contains an invalid event date.", call. = FALSE)
    }
    merged <- merge(cohort_df, outcome_df, by = "person_id", all.x = TRUE)
  } else {
    merged <- cohort_df
    merged$outcome_date <- as.Date(NA)
  }

  # Filter outcomes within TAR window
  merged$in_tar <- !is.na(merged$outcome_date) &
    merged$outcome_date >= merged$tar_start &
    merged$outcome_date <= merged$tar_end

  # For each cohort episode, find the first/last outcome in its own TAR.
  result_list <- lapply(split(merged, merged$cohort_row_id), function(sub) {
    tar_events <- sub[sub$in_tar, , drop = FALSE]
    if (nrow(tar_events) > 0) {
      if (event_order == "last") {
        chosen <- tar_events[which.max(tar_events$outcome_date), , drop = FALSE]
      } else {
        chosen <- tar_events[which.min(tar_events$outcome_date), , drop = FALSE]
      }
      data.frame(
        row_id = sub$row_id[1],
        cohort_row_id = sub$cohort_row_id[1],
        person_id = sub$person_id[1],
        event = 1L,
        time_to_event_days = as.integer(chosen$outcome_date[1] -
                                         sub$tar_start[1]),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        row_id = sub$row_id[1],
        cohort_row_id = sub$cohort_row_id[1],
        person_id = sub$person_id[1],
        event = 0L,
        time_to_event_days = as.integer(sub$tar_end[1] - sub$tar_start[1]),
        stringsAsFactors = FALSE
      )
    }
  })

  result <- do.call(rbind, result_list)
  result <- result[order(result$cohort_row_id), , drop = FALSE]
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
                                      cohort_definition_id,
                                      date_handling = NULL) {
  date_handling <- .normalizeDateHandling(date_handling)
  if (is.null(date_handling)) {
    date_handling <- .normalizeDateHandling(
      getOption("dsomop.default_date_handling", "remove")
    )
  }
  if (identical(date_handling$mode, "absolute")) {
    allow <- getOption("dsomop.allow_absolute_dates",
      getOption("default.dsomop.allow_absolute_dates", FALSE))
    if (!isTRUE(allow)) {
      stop("Absolute date handling is not permitted by the server.",
           call. = FALSE)
    }
  }
  if (is.null(cohort_table)) {
    warning("Cohort membership output requires a cohort; returning NULL.",
            call. = FALSE)
    return(NULL)
  }

  .assertMinPersons(
    handle = handle,
    sql = paste0("SELECT COUNT(DISTINCT subject_id) AS n_persons FROM ",
                 cohort_table)
  )

  sql <- paste0(
    "SELECT c.cohort_row_id, c.subject_id, c.cohort_start_date, ",
    "c.cohort_end_date",
    " FROM ", .rankedCohortSql(cohort_table, handle), " AS c",
    " ORDER BY c.cohort_row_id"
  )

  result <- .executeQuery(handle, sql)
  if (nrow(result) == 0) {
    result$row_id <- integer(0)
    result$cohort_definition_id <- integer(0)
    result <- result[, c("row_id", "cohort_row_id", "subject_id",
                         "cohort_definition_id", "cohort_start_date",
                         "cohort_end_date")]
    return(.applyDateHandling(result, date_handling,
                              index_date_col = "cohort_start_date"))
  }

  result$cohort_row_id <- as.integer(result$cohort_row_id)
  result$row_id <- result$cohort_row_id
  result$cohort_definition_id <- as.integer(cohort_definition_id)

  # Reorder columns: row_id, subject_id, cohort_definition_id, dates
  result <- result[, c("row_id", "cohort_row_id", "subject_id",
                       "cohort_definition_id", "cohort_start_date",
                       "cohort_end_date")]
  .applyDateHandling(result, date_handling,
                     index_date_col = "cohort_start_date")
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
#' @return Data frame with row_id, cohort_row_id, subject_id, interval_type,
#'   concept_id, start_days_from_index, end_days_from_index
#' @keywords internal
.extractIntervalsLongLegacy <- function(handle, cohort_table, tables,
                                         concept_filter = NULL,
                                         filters = NULL) {
  if (!is.character(tables) || length(tables) == 0L || anyNA(tables) ||
      any(!nzchar(tables)) || anyDuplicated(tolower(tables))) {
    stop("intervals_long tables must be a non-empty, unique character vector.",
         call. = FALSE)
  }
  if (is.null(cohort_table)) {
    warning("Intervals output requires a cohort; returning NULL.",
            call. = FALSE)
    return(NULL)
  }

  .assertMinPersons(
    handle = handle,
    sql = paste0("SELECT COUNT(DISTINCT subject_id) AS n_persons FROM ",
                 cohort_table)
  )

  bp <- .buildBlueprint(handle)
  all_intervals <- list()

  for (tbl_name in tables) {
    tbl_lower <- tolower(tbl_name)

    # A requested table is part of the output contract. In strict mode an
    # unavailable table or non-interval shape aborts instead of returning a
    # deceptively partial multi-table result.
    tbl_row <- bp$tables[bp$tables$table_name == tbl_lower, , drop = FALSE]
    if (nrow(tbl_row) == 0 || !tbl_row$present_in_db[1]) {
      message <- paste0("Intervals table '", tbl_name, "' is unavailable.")
      if (isTRUE(.omopDisclosureSettings()$query_strict)) {
        stop(message, call. = FALSE)
      }
      warning(message, call. = FALSE)
      next
    }

    # Get date pair.
    date_pair <- .getDatePair(bp, tbl_lower)
    if (is.null(date_pair)) {
      message <- paste0("Intervals table '", tbl_name,
                        "' has no start/end date pair.")
      if (isTRUE(.omopDisclosureSettings()$query_strict)) {
        stop(message, call. = FALSE)
      }
      warning(message, call. = FALSE)
      next
    }

    # Get domain concept column — use concept_role to avoid
    # returning type_concept columns (e.g. period_type_concept_id)
    col_df <- bp$columns[[tbl_lower]]
    domain_cols <- col_df$column_name[col_df$concept_role == "domain_concept"]
    concept_col <- if (length(domain_cols) > 0) domain_cols[1] else NULL

    # Apply per-table concept filter
    tbl_concepts <- concept_filter[[tbl_lower]] %||%
      concept_filter[[tbl_name]]
    if (!is.null(tbl_concepts) && is.null(concept_col)) {
      stop("Table '", tbl_name,
           "' has no domain concept column for its concept_filter.",
           call. = FALSE)
    }

    # Reuse the normal extraction compiler so custom date/value filters receive
    # the same identifier allowlist, date-sentinel resolution, and SQL escaping
    # as event-level outputs. add_cohort_date forces the required cohort join.
    select_cols <- c(date_pair$start, date_pair$end, concept_col)
    sql <- .compileSelect(
      handle, tbl_lower,
      columns = select_cols,
      concept_filter = tbl_concepts,
      cohort_table = cohort_table,
      add_cohort_date = TRUE,
      filters = filters,
      block_sensitive = TRUE,
      add_event_order_id = TRUE
    )
    .assertMinPersons(handle = handle, sql = .compilePersonCount(handle, sql))

    tbl_df <- .executeQuery(handle, sql)
    if (nrow(tbl_df) == 0) next
    if (!"dsomop_event_order_id" %in% names(tbl_df)) {
      stop("Intervals table '", tbl_name,
           "' has no standard OMOP primary key for deterministic row order.",
           call. = FALSE)
    }

    # Compute relative days
    start_dates <- as.Date(tbl_df[[date_pair$start]])
    end_dates <- as.Date(tbl_df[[date_pair$end]])
    index_dates <- as.Date(tbl_df$cohort_start_date)

    interval_df <- data.frame(
      cohort_row_id = as.integer(tbl_df$cohort_row_id),
      subject_id = tbl_df$person_id,
      .event_order_id = tbl_df$dsomop_event_order_id,
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
      row_id = integer(0), cohort_row_id = integer(0),
      subject_id = integer(0),
      interval_type = character(0), concept_id = integer(0),
      start_days_from_index = integer(0),
      end_days_from_index = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  result <- do.call(rbind, all_intervals)
  rownames(result) <- NULL
  table_order <- match(result$interval_type, tolower(tables))
  result <- result[order(table_order, result$cohort_row_id,
                         result$.event_order_id, na.last = TRUE), , drop = FALSE]
  rownames(result) <- NULL
  result$row_id <- seq_len(nrow(result))
  result$.event_order_id <- NULL

  result[, c("row_id", "cohort_row_id", "subject_id", "interval_type",
             "concept_id", "start_days_from_index", "end_days_from_index")]
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
                as.integer(window_end),
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
#' @param concept_filter Numeric vector or OHDSI concept-set specification.
#' @param bin_width Integer; bin width in days
#' @param window_start Integer; start of window (days from index)
#' @param window_end Integer; end of window (days from index)
#' @param analyses Character vector; analyses to compute
#' @param observation_roster Optional validated internal observation-period
#'   roster, used by person-period extraction to avoid querying it twice.
#' @return Named list with temporalCovariates, covariateRef, timeRef, and
#'   personRef (the cohort-episode to person mapping)
#' @keywords internal
.extractTemporalCovariates <- function(handle, cohort_table, table,
                                        concept_filter = NULL,
                                        bin_width = 30L,
                                        window_start = -365L,
                                        window_end = 0L,
                                        analyses = c("binary"),
                                        filters = NULL,
                                        observation_roster = NULL) {
  integer_setting <- function(value, name) {
    numeric_value <- suppressWarnings(as.numeric(value))
    integer_value <- suppressWarnings(as.integer(value))
    if (length(value) != 1L || length(numeric_value) != 1L ||
        !is.finite(numeric_value) || length(integer_value) != 1L ||
        is.na(integer_value) || numeric_value != integer_value) {
      stop(name, " must be one finite integer.", call. = FALSE)
    }
    integer_value
  }
  bin_width <- integer_setting(bin_width, "bin_width")
  window_start <- integer_setting(window_start, "window_start")
  window_end <- integer_setting(window_end, "window_end")
  if (bin_width <= 0L) {
    stop("bin_width must be greater than zero.", call. = FALSE)
  }
  if (window_start > window_end) {
    stop("window_start must not be after window_end.", call. = FALSE)
  }
  n_bins <- floor((as.double(window_end) - as.double(window_start)) /
                    as.double(bin_width)) + 1
  max_temporal_bins <- .extractionCap("dsomop.max_temporal_bins", 10000L)
  if (!is.finite(n_bins) || n_bins > max_temporal_bins) {
    stop("Temporal covariates would create ", n_bins,
         " bins, exceeding the server cap of ", max_temporal_bins, ".",
         call. = FALSE)
  }
  if (!is.character(analyses) || length(analyses) == 0L || anyNA(analyses) ||
      any(!analyses %in% c("binary", "count"))) {
    stop("analyses must be a non-empty subset of binary and count.",
         call. = FALSE)
  }
  analyses <- unique(analyses)

  if (is.null(cohort_table)) {
    warning("Temporal covariates output requires a cohort; returning NULL.",
            call. = FALSE)
    return(NULL)
  }

  max_covariate_concepts <- .extractionCap(
    "dsomop.max_pivot_concepts", 1000L
  )
  resolved_concepts <- .normalizeTemporalSqlConcepts(
    handle, concept_filter, max_covariate_concepts
  )

  if (is.null(observation_roster)) {
    observation_roster <- .loadTemporalObservationRoster(
      handle, cohort_table
    )
  } else if (!is.data.frame(observation_roster) ||
             !identical(
               names(observation_roster),
               c(
                 "cohort_row_id", "person_id", "observation_start_day",
                 "observation_end_day"
               )
             ) || anyNA(observation_roster) ||
             anyDuplicated(observation_roster$cohort_row_id)) {
    stop("Invalid internal observation-period roster.", call. = FALSE)
  }

  # Materialize the complete episode map independently of qualifying events.
  # This keeps rowId linkable even for cohort eras with no event in the window,
  # without returning the absolute index dates used to define those eras.
  person_ref <- data.frame(
    rowId = as.integer(observation_roster$cohort_row_id),
    # Keep the canonical identifier spelling so the recursive DataSHIELD
    # release pass pseudonymizes it; a camelCase alias would bypass that gate.
    person_id = observation_roster$person_id,
    stringsAsFactors = FALSE
  )

  # Extract events with days_from_index via .extractTable
  events <- .extractTable(
    handle,
    table = table,
    concept_filter = resolved_concepts,
    cohort_table = cohort_table,
    add_cohort_date = TRUE,
    temporal = list(
      index_window = list(start = window_start, end = window_end)
    ),
    block_sensitive = TRUE,
    filters = filters,
    translate_concepts = FALSE
  )

  # Generate time windows
  time_ref <- .generateTimeWindows(bin_width, window_start, window_end)
  analysis_map <- list(binary = 1L, count = 2L)
  declared_concepts <- if (is.null(resolved_concepts)) {
    integer(0)
  } else {
    resolved_concepts
  }
  make_covariate_ref <- function(concepts) {
    rows <- lapply(concepts, function(cid) {
      concept_label <- .standardizeName(as.character(cid))
      if (is.na(concept_label) || concept_label == "") {
        concept_label <- paste0("concept_", cid)
      }
      do.call(rbind, lapply(analyses, function(analysis_name) {
        aid <- analysis_map[[analysis_name]]
        data.frame(
          covariateId = as.numeric(cid) * 1000 + aid,
          covariateName = paste0(concept_label, "_", analysis_name),
          analysisId = aid,
          conceptId = as.integer(cid),
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

  # Empty result template
  empty_result <- list(
    temporalCovariates = data.frame(
      rowId = integer(0), timeId = integer(0),
      covariateId = numeric(0), covariateValue = numeric(0),
      stringsAsFactors = FALSE
    ),
    covariateRef = make_covariate_ref(declared_concepts),
    timeRef = time_ref,
    personRef = person_ref
  )

  if (nrow(events) == 0L) {
    .assertMinPersons(n_persons = 0L)
    return(empty_result)
  }
  if (!all(c("cohort_row_id", "person_id", "days_from_index") %in%
           names(events))) {
    stop("Temporal covariates require a stable cohort_row_id for each cohort ",
         "entry.", call. = FALSE)
  }

  observation_match <- match(
    events$cohort_row_id, observation_roster$cohort_row_id
  )
  if (anyNA(observation_match) || any(
    events$person_id != observation_roster$person_id[observation_match]
  )) {
    stop("Temporal events do not match the validated cohort episode roster.",
         call. = FALSE)
  }
  observed <-
    events$days_from_index >=
      observation_roster$observation_start_day[observation_match] &
    events$days_from_index <=
      observation_roster$observation_end_day[observation_match]
  events <- events[!is.na(observed) & observed, , drop = FALSE]
  .assertMinPersons(n_persons = length(unique(events$person_id)))
  if (nrow(events) == 0L) return(empty_result)

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

  concepts <- sort(unique(c(
    declared_concepts,
    events[[concept_col]][!is.na(events[[concept_col]])]
  )))
  if (length(concepts) > max_covariate_concepts) {
    stop("Temporal covariates exceed the server concept cap of ",
         max_covariate_concepts, ".", call. = FALSE)
  }

  covariates <- data.frame(
    rowId = integer(0), timeId = integer(0),
    covariateId = numeric(0), covariateValue = numeric(0),
    stringsAsFactors = FALSE
  )
  covariate_ref <- make_covariate_ref(concepts)

  for (cid in concepts) {
    c_events <- events[events[[concept_col]] == cid, , drop = FALSE]

    for (analysis_name in analyses) {
      aid <- analysis_map[[analysis_name]]
      if (is.null(aid)) next

      cov_id <- as.numeric(cid) * 1000 + aid

      if (analysis_name == "binary") {
        # Unique (cohort entry, time_bin) pairs. A person can contribute more
        # than one index era, and each era is a separate covariate row.
        uniq <- unique(c_events[, c("cohort_row_id", ".timeId"),
                                 drop = FALSE])
        if (nrow(uniq) > 0) {
          covariates <- rbind(covariates, data.frame(
            rowId = as.integer(uniq$cohort_row_id),
            timeId = uniq$.timeId,
            covariateId = rep(cov_id, nrow(uniq)),
            covariateValue = rep(1, nrow(uniq)),
            stringsAsFactors = FALSE
          ))
        }
      } else if (analysis_name == "count") {
        # Count events per (cohort entry, time_bin)
        count_agg <- if (nrow(c_events) > 0L) {
          stats::aggregate(
            c_events[[concept_col]],
            by = list(cohort_row_id = c_events$cohort_row_id,
                      timeId = c_events$.timeId),
            FUN = length
          )
        } else {
          data.frame()
        }
        if (nrow(count_agg) > 0) {
          covariates <- rbind(covariates, data.frame(
            rowId = as.integer(count_agg$cohort_row_id),
            timeId = count_agg$timeId,
            covariateId = rep(cov_id, nrow(count_agg)),
            covariateValue = as.numeric(count_agg$x),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }

  list(
    temporalCovariates = covariates,
    covariateRef = covariate_ref,
    timeRef = time_ref,
    personRef = person_ref
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
