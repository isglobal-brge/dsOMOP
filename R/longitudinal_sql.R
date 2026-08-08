# SQL-first longitudinal output helpers.

.longitudinalInteger <- function(value, name, default = NULL) {
  if (is.null(value)) return(default)
  number <- suppressWarnings(as.numeric(value))
  integer <- suppressWarnings(as.integer(value))
  if (length(value) != 1L || length(number) != 1L || is.na(number) ||
      !is.finite(number) || length(integer) != 1L || is.na(integer) ||
      number != integer) {
    stop(name, " must be one finite exact integer.", call. = FALSE)
  }
  integer
}

.normalizeLongitudinalWindow <- function(window, interval_match) {
  interval_match <- tolower(interval_match %||% "overlaps")
  if (!is.character(interval_match) || length(interval_match) != 1L ||
      is.na(interval_match) ||
      !interval_match %in% c("overlaps", "starts_in", "ends_in", "active_at")) {
    stop("interval_match must be overlaps, starts_in, ends_in, or active_at.",
         call. = FALSE)
  }
  if (is.null(window)) {
    if (identical(interval_match, "active_at")) {
      window <- list(at = 0L)
    } else {
      return(list(match = interval_match, relative = FALSE))
    }
  }
  if (!is.list(window) || length(window) == 0L || is.null(names(window)) ||
      any(!nzchar(names(window))) || anyDuplicated(names(window))) {
    stop("window must be a non-empty uniquely named list.", call. = FALSE)
  }
  allowed <- if (identical(interval_match, "active_at")) "at" else c("start", "end")
  unknown <- setdiff(names(window), allowed)
  if (length(unknown) > 0L) {
    stop("Unknown longitudinal window field(s): ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  if (identical(interval_match, "active_at")) {
    if (!identical(names(window), "at")) {
      stop("active_at requires window=list(at=<days from index>).",           call. = FALSE)
    }
    return(list(
      match = interval_match, relative = TRUE,
      at = .longitudinalInteger(window$at, "window$at")
    ))
  }
  if (!any(c("start", "end") %in% names(window))) {
    stop("window must contain start and/or end.", call. = FALSE)
  }
  start <- .longitudinalInteger(window$start, "window$start", NULL)
  end <- .longitudinalInteger(window$end, "window$end", NULL)
  if (!is.null(start) && !is.null(end) && start > end) {
    stop("window$start must not be after window$end.", call. = FALSE)
  }
  list(match = interval_match, relative = TRUE, start = start, end = end)
}

.normalizeLongitudinalSelection <- function(event_select = "all",
                                             select_n = 1L,
                                             select_by = "episode_source",
                                             anchor = 0L) {
  if (!is.character(event_select) || length(event_select) != 1L ||
      is.na(event_select)) {
    stop("event_select must be one selection mode.", call. = FALSE)
  }
  event_select <- tolower(event_select)
  if (!event_select %in% c("all", "first", "last", "nearest")) {
    stop("event_select must be all, first, last, or nearest.",
         call. = FALSE)
  }
  if (!is.character(select_by) || length(select_by) != 1L ||
      is.na(select_by)) {
    stop("select_by must be one grouping mode.", call. = FALSE)
  }
  select_by <- tolower(select_by)
  if (!select_by %in% c("episode_source", "episode_source_concept")) {
    stop("select_by must be episode_source or episode_source_concept.",
         call. = FALSE)
  }
  select_n <- .longitudinalInteger(select_n, "select_n")
  max_n <- .extractionCap("dsomop.max_events_per_group", 100L)
  if (select_n < 1L || select_n > max_n) {
    stop("select_n must be between 1 and the server cap of ", max_n, ".",
         call. = FALSE)
  }
  list(
    mode = event_select,
    n = select_n,
    by = select_by,
    anchor = .longitudinalInteger(anchor, "anchor")
  )
}

.longitudinalSourceFilter <- function(source_filters, table, tables) {
  if (is.null(source_filters)) return(NULL)
  if (!is.list(source_filters) || is.null(names(source_filters)) ||
      any(!nzchar(names(source_filters))) || anyDuplicated(tolower(names(source_filters)))) {
    stop("filters must be a uniquely named per-table list.", call. = FALSE)
  }
  unknown <- setdiff(tolower(names(source_filters)), tolower(tables))
  if (length(unknown) > 0L) {
    stop("filters contains unknown interval table(s): ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  index <- match(tolower(table), tolower(names(source_filters)))
  if (is.na(index)) NULL else source_filters[[index]]
}

.longitudinalNullIntegerSql <- function(handle) {
  .omopBigIntegerCastSql(handle, "NULL")
}

#' Compile a normalized, episode-grain multi-table interval stream
#'
#' Every source row is attached only to cohort episodes selected by the declared
#' interval relationship. The default is overlap with the cohort episode,
#' avoiding the accidental person-by-all-episodes multiplication of an
#' unconstrained person join. The source OMOP primary key is used solely for
#' deterministic tie-breaking and is removed by the outer projection.
#'
#' @keywords internal
.compileIntervalsLongSql <- function(handle, cohort_table, tables,
                                      concept_filter = NULL,
                                      filters = NULL,
                                      window = NULL,
                                      interval_match = "overlaps",
                                      event_select = "all",
                                      select_n = 1L,
                                      select_by = "episode_source",
                                      anchor = 0L) {
  .assertAnalyticDbmsSupport(handle, "Longitudinal interval SQL")
  if (!is.character(tables) || length(tables) == 0L || anyNA(tables) ||
      any(!nzchar(tables)) || anyDuplicated(tolower(tables))) {
    stop("intervals_long tables must be a non-empty, unique character vector.",
         call. = FALSE)
  }
  if (is.null(cohort_table)) {
    stop("intervals_long requires a cohort.", call. = FALSE)
  }
  if (!is.null(concept_filter) &&
      (!is.list(concept_filter) || is.null(names(concept_filter)) ||
       any(!nzchar(names(concept_filter))) ||
       anyDuplicated(tolower(names(concept_filter))) ||
       length(setdiff(tolower(names(concept_filter)), tolower(tables))) > 0L)) {
    stop("concept_filter must be a uniquely named per-table list.",
         call. = FALSE)
  }
  cohort_table <- .validateIdentifier(cohort_table, "interval cohort table")
  window_spec <- .normalizeLongitudinalWindow(window, interval_match)
  selection <- .normalizeLongitudinalSelection(
    event_select, select_n, select_by, anchor
  )
  bp <- .buildBlueprint(handle)
  sources <- list()

  for (table in tables) {
    table <- tolower(.validateIdentifier(table, "interval table"))
    table_row <- bp$tables[
      bp$tables$table_name == table & bp$tables$present_in_db, , drop = FALSE
    ]
    if (nrow(table_row) == 0L) {
      stop("Intervals table '", table, "' is unavailable.", call. = FALSE)
    }
    date_pair <- .getDatePair(bp, table)
    if (is.null(date_pair)) {
      stop("Intervals table '", table,
           "' has no reviewed start/end date pair.", call. = FALSE)
    }
    columns <- bp$columns[[table]]
    domain_columns <- columns$column_name[
      columns$concept_role == "domain_concept"
    ]
    concept_column <- if (length(domain_columns) > 0L) domain_columns[[1L]] else NULL
    concept_index <- match(table, tolower(names(concept_filter)))
    table_concepts <- if (is.na(concept_index)) {
      NULL
    } else {
      concept_filter[[concept_index]]
    }
    if (!is.null(table_concepts)) {
      table_concepts <- .resolveConceptSet(handle, table_concepts)
      if (length(table_concepts) == 0L) {
        stop("Concept filter for table '", table,
             "' resolves to no concepts.", call. = FALSE)
      }
    }
    if (!is.null(table_concepts) && is.null(concept_column)) {
      stop("Table '", table,
           "' has no domain concept column for its concept_filter.",
           call. = FALSE)
    }
    table_filter <- .longitudinalSourceFilter(filters, table, tables)
    selected_columns <- c(date_pair$start, date_pair$end, concept_column)
    base_sql <- .compileSelect(
      handle = handle,
      table = table,
      columns = selected_columns,
      concept_filter = table_concepts,
      cohort_table = cohort_table,
      add_cohort_date = TRUE,
      filters = table_filter,
      block_sensitive = TRUE,
      add_event_order_id = TRUE
    )
    if (is.null(.eventPrimaryKeyColumn(bp, table))) {
      stop("Intervals table '", table,
           "' lacks a reviewed primary key for deterministic ordering.",
           call. = FALSE)
    }

    start_expression <- paste0("q.", date_pair$start)
    end_expression <- paste0(
      "COALESCE(q.", date_pair$end, ", q.", date_pair$start, ")"
    )
    start_day <- .omopDateDiffDays(
      handle, start_expression, "q.cohort_start_date"
    )
    end_day <- .omopDateDiffDays(
      handle, end_expression, "q.cohort_start_date"
    )
    concept_expression <- if (is.null(concept_column)) {
      .longitudinalNullIntegerSql(handle)
    } else {
      paste0("q.", concept_column)
    }
    source <- paste0(
      "SELECT q.cohort_row_id, q.person_id AS subject_id, ",
      .quoteLiteral(table, handle), " AS interval_type, ",
      concept_expression, " AS concept_id, ",
      start_day, " AS start_days_from_index, ",
      end_day, " AS end_days_from_index, ",
      "q.dsomop_event_order_id AS dsomop_event_order_id",
      " FROM (", base_sql, ") AS q",
      " WHERE q.", date_pair$start, " IS NOT NULL",
      " AND (q.", date_pair$end, " IS NULL OR q.", date_pair$end,
      " >= q.", date_pair$start, ")"
    )

    predicates <- character(0)
    if (!isTRUE(window_spec$relative)) {
      predicates <- switch(window_spec$match,
        overlaps = c(
          paste0(start_expression, " <= q.cohort_end_date"),
          paste0(end_expression, " >= q.cohort_start_date")
        ),
        starts_in = c(
          paste0(start_expression, " >= q.cohort_start_date"),
          paste0(start_expression, " <= q.cohort_end_date")
        ),
        ends_in = c(
          paste0(end_expression, " >= q.cohort_start_date"),
          paste0(end_expression, " <= q.cohort_end_date")
        )
      )
    } else if (identical(window_spec$match, "active_at")) {
      predicates <- c(
        paste0(start_day, " <= ", window_spec$at),
        paste0(end_day, " >= ", window_spec$at)
      )
    } else {
      value <- if (identical(window_spec$match, "starts_in")) {
        start_day
      } else if (identical(window_spec$match, "ends_in")) {
        end_day
      } else {
        NULL
      }
      if (identical(window_spec$match, "overlaps")) {
        if (!is.null(window_spec$start)) {
          predicates <- c(predicates, paste0(end_day, " >= ", window_spec$start))
        }
        if (!is.null(window_spec$end)) {
          predicates <- c(predicates, paste0(start_day, " <= ", window_spec$end))
        }
      } else {
        if (!is.null(window_spec$start)) {
          predicates <- c(predicates, paste0(value, " >= ", window_spec$start))
        }
        if (!is.null(window_spec$end)) {
          predicates <- c(predicates, paste0(value, " <= ", window_spec$end))
        }
      }
    }
    if (length(predicates) > 0L) {
      source <- paste0(source, " AND ", paste(predicates, collapse = " AND "))
    }
    .assertMinPersons(
      handle = handle,
      sql = paste0(
        "SELECT COUNT(DISTINCT subject_id) AS n_persons FROM (",
        source, ") AS scoped_intervals"
      )
    )
    sources[[table]] <- source
  }

  union_sql <- paste(unname(unlist(sources)), collapse = " UNION ALL ")
  selected_sql <- union_sql
  if (!identical(selection$mode, "all")) {
    partition <- c("cohort_row_id", "interval_type")
    if (identical(selection$by, "episode_source_concept")) {
      partition <- c(partition, "concept_id")
    }
    order_sql <- switch(selection$mode,
      first = paste(
        "start_days_from_index ASC, end_days_from_index ASC,",
        "dsomop_event_order_id ASC"
      ),
      last = paste(
        "start_days_from_index DESC, end_days_from_index DESC,",
        "dsomop_event_order_id DESC"
      ),
      nearest = paste0(
        "ABS(start_days_from_index - ", selection$anchor, ") ASC, ",
        "start_days_from_index ASC, dsomop_event_order_id ASC"
      )
    )
    selected_sql <- paste0(
      "SELECT * FROM (SELECT u.*, ROW_NUMBER() OVER (PARTITION BY ",
      paste(partition, collapse = ", "), " ORDER BY ", order_sql,
      ") AS dsomop_selection_rank FROM (", union_sql,
      ") AS u) AS ranked WHERE dsomop_selection_rank <= ", selection$n
    )
  }

  final_sql <- paste0(
    "SELECT ROW_NUMBER() OVER (ORDER BY cohort_row_id, interval_type, ",
    "start_days_from_index, end_days_from_index, concept_id, ",
    "dsomop_event_order_id) AS row_id, cohort_row_id, subject_id, ",
    "interval_type, concept_id, start_days_from_index, ",
    "end_days_from_index FROM (", selected_sql, ") AS selected"
  )
  .sql_translate(final_sql, handle$target_dialect)
}

#' Execute the SQL-first interval contract in memory
#'
#' Staged plans consume the same compiled SQL through the chunked Parquet
#' writer; this in-memory wrapper exists so both physical modes have identical
#' episode matching, tie-breaking, and row ordering semantics.
#'
#' @keywords internal
.extractIntervalsLongSql <- function(handle, cohort_table, tables,
                                      concept_filter = NULL,
                                      filters = NULL,
                                      window = NULL,
                                      interval_match = "overlaps",
                                      event_select = "all",
                                      select_n = 1L,
                                      select_by = "episode_source",
                                      anchor = 0L) {
  sql <- .compileIntervalsLongSql(
    handle = handle,
    cohort_table = cohort_table,
    tables = tables,
    concept_filter = concept_filter,
    filters = filters,
    window = window,
    interval_match = interval_match,
    event_select = event_select,
    select_n = select_n,
    select_by = select_by,
    anchor = anchor
  )
  .convertTypes(.executeQuery(handle, sql))
}

.extractIntervalsLong <- function(handle, cohort_table, tables,
                                   concept_filter = NULL,
                                   filters = NULL,
                                   window = NULL,
                                   interval_match = "overlaps",
                                   event_select = "all",
                                   select_n = 1L,
                                   select_by = "episode_source",
                                   anchor = 0L) {
  .extractIntervalsLongSql(
    handle = handle,
    cohort_table = cohort_table,
    tables = tables,
    concept_filter = concept_filter,
    filters = filters,
    window = window,
    interval_match = interval_match,
    event_select = event_select,
    select_n = select_n,
    select_by = select_by,
    anchor = anchor
  )
}
