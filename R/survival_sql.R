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

.multistateCharacterVector <- function(value, name, empty = FALSE) {
  if (is.null(value) && empty) return(character(0))
  if (is.list(value) && !is.data.frame(value)) {
    if (length(value) == 0L && empty) return(character(0))
    scalar <- vapply(value, function(item) {
      is.character(item) && length(item) == 1L && !is.na(item)
    }, logical(1L))
    if (!all(scalar)) {
      stop(name, " must contain only state names.", call. = FALSE)
    }
    value <- unlist(value, use.names = FALSE)
  }
  if (!is.character(value) || (!empty && length(value) == 0L) ||
      anyNA(value) || any(!nzchar(value))) {
    stop(name, " must contain only state names.", call. = FALSE)
  }
  unname(value)
}

.multistateExactPositiveInteger <- function(value, name) {
  number <- suppressWarnings(as.numeric(value))
  integer <- suppressWarnings(as.integer(value))
  if (length(number) != 1L || is.na(number) || !is.finite(number) ||
      length(integer) != 1L || is.na(integer) || number != integer ||
      integer < 1L) {
    stop(name, " must be one positive exact integer.", call. = FALSE)
  }
  integer
}

.multistateStateStep <- function(value, n_states) {
  value <- value %||% 0.01
  number <- suppressWarnings(as.numeric(value))
  if (length(number) != 1L || is.na(number) || !is.finite(number) ||
      number <= 0 || number != round(number, 9L) ||
      number * max(0L, n_states - 1L) >= 1) {
    stop("state_step must be a positive decimal with at most nine places, and " ,
         "state_step * (number of states - 1) must be below one day.",
         call. = FALSE)
  }
  number
}

# Normalize the client-facing mstate transition matrix/adjacency forms into a
# plain, JSON-stable graph. State and transition identifiers are controller-
# owned dimensions: they never depend on which transitions happen at a site.
.normalizeMultistateSpec <- function(outcomes, transitions, initial_state,
                                     state_hierarchy = NULL,
                                     state_step = NULL) {
  initial_state <- .multistateCharacterVector(
    initial_state %||% "index", "initial_state"
  )
  if (length(initial_state) != 1L) {
    stop("initial_state must be one state name.", call. = FALSE)
  }
  outcome_names <- vapply(outcomes, `[[`, character(1L), "name")
  expected_states <- unique(c(initial_state, outcome_names))
  name_limit <- .omopDisclosureSettings()$nfilter_string
  valid_state <- grepl("^[A-Za-z][A-Za-z0-9_.-]*$", expected_states) &
    nchar(expected_states, type = "bytes") <= name_limit
  if (any(!valid_state) || anyDuplicated(tolower(expected_states))) {
    stop("Multi-state names must be portable, case-insensitively unique safe " ,
         "names that respect nfilter.string.", call. = FALSE)
  }

  canonical_edges <- NULL
  state_names <- NULL
  if (is.matrix(transitions)) {
    if (nrow(transitions) < 2L || nrow(transitions) != ncol(transitions) ||
        is.null(rownames(transitions)) || is.null(colnames(transitions)) ||
        !identical(rownames(transitions), colnames(transitions))) {
      stop("transitions matrix must be square with identical state dimnames.",
           call. = FALSE)
    }
    state_names <- rownames(transitions)
    if (any(!is.na(diag(transitions)))) {
      stop("Self transitions are not supported; the transition diagonal must be NA.",
           call. = FALSE)
    }
    positions <- which(!is.na(transitions), arr.ind = TRUE)
    values <- suppressWarnings(as.numeric(transitions[positions]))
    integers <- suppressWarnings(as.integer(values))
    if (length(values) == 0L || anyNA(values) || any(!is.finite(values)) ||
        anyNA(integers) || any(values != integers) ||
        !setequal(integers, seq_along(integers))) {
      stop("Non-NA transition matrix entries must be the unique integers 1..K.",
           call. = FALSE)
    }
    positions <- positions[order(integers), , drop = FALSE]
    canonical_edges <- lapply(seq_len(nrow(positions)), function(index) {
      list(
        from = state_names[positions[index, 1L]],
        to = state_names[positions[index, 2L]],
        trans = as.integer(index)
      )
    })
  } else if (is.list(transitions) && length(transitions) == 2L &&
             !is.null(names(transitions)) &&
             !anyDuplicated(names(transitions)) &&
             setequal(names(transitions), c("states", "edges"))) {
    state_names <- .multistateCharacterVector(
      transitions$states, "transitions$states"
    )
    edges <- transitions$edges
    if (!is.list(edges) || length(edges) == 0L) {
      stop("transitions$edges must be a non-empty list.", call. = FALSE)
    }
    canonical_edges <- lapply(seq_along(edges), function(index) {
      edge <- edges[[index]]
      if (!is.list(edge) || length(edge) != 3L || is.null(names(edge)) ||
          anyDuplicated(names(edge)) ||
          !setequal(names(edge), c("from", "to", "trans"))) {
        stop("Each transitions$edges entry must contain from, to and trans.",
             call. = FALSE)
      }
      from <- .multistateCharacterVector(
        edge$from, paste0("transitions$edges[[", index, "]]$from")
      )
      to <- .multistateCharacterVector(
        edge$to, paste0("transitions$edges[[", index, "]]$to")
      )
      if (length(from) != 1L || length(to) != 1L) {
        stop("Every transition edge must have one from and one to state.",
             call. = FALSE)
      }
      list(
        from = from,
        to = to,
        trans = .multistateExactPositiveInteger(
          edge$trans, paste0("transitions$edges[[", index, "]]$trans")
        )
      )
    })
    edge_ids <- vapply(canonical_edges, `[[`, integer(1L), "trans")
    canonical_edges <- canonical_edges[order(edge_ids)]
  } else if (is.list(transitions) && length(transitions) > 0L) {
    state_names <- names(transitions)
    if (is.null(state_names) || any(!nzchar(state_names)) ||
        anyDuplicated(state_names)) {
      stop("Adjacency transitions must be a uniquely named state list.",
           call. = FALSE)
    }
    canonical_edges <- list()
    transition_id <- 0L
    for (from_index in seq_along(transitions)) {
      targets <- transitions[[from_index]]
      if (is.null(targets) || length(targets) == 0L) next
      if (is.list(targets)) {
        targets <- unlist(targets, use.names = FALSE)
      }
      numeric_targets <- suppressWarnings(as.numeric(targets))
      integer_targets <- suppressWarnings(as.integer(targets))
      if (is.numeric(targets) ||
          (is.character(targets) && length(targets) > 0L &&
           all(grepl("^[0-9]+$", targets)))) {
        if (anyNA(numeric_targets) || anyNA(integer_targets) ||
            any(numeric_targets != integer_targets) ||
            any(integer_targets < 1L | integer_targets > length(state_names))) {
          stop("Numeric transition targets must be valid state positions.",
               call. = FALSE)
        }
        targets <- state_names[integer_targets]
      } else {
        targets <- .multistateCharacterVector(
          targets, paste0("transitions$", state_names[from_index]),
          empty = TRUE
        )
      }
      if (anyDuplicated(targets)) {
        stop("A state cannot declare the same destination twice.",
             call. = FALSE)
      }
      for (target in targets) {
        transition_id <- transition_id + 1L
        canonical_edges[[transition_id]] <- list(
          from = state_names[from_index], to = target,
          trans = transition_id
        )
      }
    }
  } else {
    stop("multi_state requires a transition matrix or adjacency list.",
         call. = FALSE)
  }

  state_names <- .multistateCharacterVector(
    state_names, "transition state names"
  )
  if (anyDuplicated(state_names) || anyDuplicated(tolower(state_names)) ||
      !setequal(state_names, expected_states)) {
    stop("Transition states must match initial_state plus the named outcomes, " ,
         "with no case-insensitive duplicates.", call. = FALSE)
  }
  if (length(canonical_edges) == 0L) {
    stop("The multi-state graph must declare at least one transition.",
         call. = FALSE)
  }
  edge_ids <- vapply(canonical_edges, `[[`, integer(1L), "trans")
  if (!identical(edge_ids, seq_along(canonical_edges))) {
    stop("Transition identifiers must be the unique contiguous integers 1..K.",
         call. = FALSE)
  }
  from <- vapply(canonical_edges, `[[`, character(1L), "from")
  to <- vapply(canonical_edges, `[[`, character(1L), "to")
  if (any(!from %in% state_names) || any(!to %in% state_names) ||
      any(from == to) || anyDuplicated(paste(from, to, sep = "\r"))) {
    stop("Transition edges must be unique, connect declared distinct states, " ,
         "and must not be self transitions.", call. = FALSE)
  }

  reachable <- initial_state
  repeat {
    next_states <- unique(c(reachable, to[from %in% reachable]))
    if (setequal(next_states, reachable)) break
    reachable <- next_states
  }
  unreachable <- setdiff(state_names, reachable)
  if (length(unreachable) > 0L) {
    stop("Every multi-state state must be graph-reachable from initial_state; " ,
         "unreachable: ", paste(unreachable, collapse = ", "), ".",
         call. = FALSE)
  }

  hierarchy <- if (is.null(state_hierarchy) || length(state_hierarchy) == 0L) {
    state_names
  } else {
    .multistateCharacterVector(state_hierarchy, "state_hierarchy")
  }
  if (anyDuplicated(hierarchy) || any(!hierarchy %in% state_names)) {
    stop("state_hierarchy must contain unique declared state names.",
         call. = FALSE)
  }
  hierarchy <- c(hierarchy, setdiff(state_names, hierarchy))
  step <- .multistateStateStep(state_step, length(state_names))
  state_id <- stats::setNames(seq_along(state_names), state_names)
  hierarchy_rank <- stats::setNames(seq_along(hierarchy), hierarchy)

  edges <- data.frame(
    trans = edge_ids,
    from = unname(as.integer(state_id[from])),
    to = unname(as.integer(state_id[to])),
    from_name = from,
    to_name = to,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  states <- data.frame(
    state_id = seq_along(state_names),
    state_name = state_names,
    hierarchy_rank = unname(as.integer(hierarchy_rank[state_names])),
    stringsAsFactors = FALSE
  )
  canonical <- list(
    states = unname(state_names),
    edges = lapply(seq_len(nrow(edges)), function(index) list(
      from = edges$from_name[index], to = edges$to_name[index],
      trans = as.integer(edges$trans[index])
    ))
  )
  list(
    initial_state = initial_state,
    initial_state_id = unname(as.integer(state_id[initial_state])),
    states = states,
    edges = edges,
    transitions = canonical,
    state_hierarchy = unname(hierarchy),
    state_step = step
  )
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
    cohort_table, " WHERE subject_id IS NOT NULL AND cohort_start_date ",
    "IS NOT NULL AND cohort_end_date IS NOT NULL AND cohort_end_date >= ",
    "cohort_start_date) cohort_base)"
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
    " <= e.censor_date WHERE t.", outcome$date_column,
    " IS NOT NULL AND t.", outcome$primary_key, " IS NOT NULL AND ",
    concept_predicate, filter_predicate
  )
}

.survivalDecimalCast <- function(handle, expression) {
  dbms <- .normalizeDBMS(handle$dbms %||% handle$target_dialect %||% "")
  # Eleven integer digits cover the validated day-offset range. Keeping the
  # declared precision at 20 prevents SQL Server's decimal multiplication
  # rules from collapsing a nine-place sequential state_step to scale six.
  type <- if (identical(dbms, "bigquery")) "NUMERIC" else "DECIMAL(20,9)"
  paste0("CAST(", expression, " AS ", type, ")")
}

.multistateSqlCase <- function(column, keys, values) {
  paste0(
    "CASE ", column, " ",
    paste0("WHEN ", as.integer(keys), " THEN ", as.integer(values),
           collapse = " "),
    " END"
  )
}

.compileMultistateStreamSql <- function(handle, ctes, outcomes, spec,
                                        tie_policy) {
  outcome_names <- vapply(outcomes, `[[`, character(1L), "name")
  outcome_priority <- vapply(outcomes, `[[`, integer(1L), "priority")
  state_rows <- spec$states[match(outcome_names, spec$states$state_name),
                            , drop = FALSE]
  state_case <- .multistateSqlCase(
    "e.outcome_priority", outcome_priority, state_rows$state_id
  )
  hierarchy_case <- .multistateSqlCase(
    "e.outcome_priority", outcome_priority, state_rows$hierarchy_rank
  )
  event_day <- .survivalDateDiff(
    handle, "e.event_date", "e.cohort_start_date"
  )
  state_ctes <- c(
    paste0(
      "multistate_ranked AS (SELECT e.*, ", state_case,
      " AS state_id, ", hierarchy_case, " AS hierarchy_rank, ",
      event_day, " AS state_day, ROW_NUMBER() OVER (PARTITION BY ",
      "e.cohort_row_id, e.outcome_priority, e.event_date ORDER BY ",
      "e.event_key) AS state_duplicate_rank FROM eligible_events e)"
    ),
    paste0(
      "multistate_unique AS (SELECT e.* FROM multistate_ranked e WHERE ",
      "e.state_duplicate_rank = 1)"
    ),
    paste0(
      "multistate_ordered AS (SELECT e.*, ROW_NUMBER() OVER (PARTITION BY ",
      "e.cohort_row_id, e.event_date ORDER BY e.hierarchy_rank, e.state_id) ",
      "AS within_day_order, COUNT(*) OVER (PARTITION BY e.cohort_row_id, ",
      "e.event_date) AS within_day_count FROM multistate_unique e)"
    )
  )
  state_time <- if (identical(tie_policy, "sequential")) {
    step <- format(
      spec$state_step, scientific = FALSE, trim = TRUE, digits = 10L,
      decimal.mark = "."
    )
    paste0(
      .survivalDecimalCast(handle, "e.state_day"), " - (",
      .survivalDecimalCast(handle,
                           "e.within_day_count - e.within_day_order"),
      " * ", .survivalDecimalCast(handle, step), ")"
    )
  } else {
    .survivalDecimalCast(handle, "e.state_day")
  }
  state_ctes <- c(state_ctes, paste0(
    "multistate_timed AS (SELECT e.*, ", state_time,
    " AS state_time FROM multistate_ordered e)"
  ))

  decimal_seed <- .survivalDecimalCast(handle, "e.risk_start_days - 1")
  decimal_end <- .survivalDecimalCast(handle, "e.risk_end_days")
  seed <- paste0(
    "SELECT e.cohort_row_id, e.subject_id AS person_id, 0 AS stream_kind, ",
    spec$initial_state_id, " AS state_id, ", decimal_seed,
    " AS state_time, e.risk_start_days - 1 AS state_day, 0 AS ",
    "within_day_order, 0 AS within_day_count, e.risk_end_days FROM ",
    "risk_episodes e"
  )
  events <- paste0(
    "SELECT e.cohort_row_id, e.subject_id AS person_id, 1 AS stream_kind, ",
    "e.state_id, e.state_time, e.state_day, e.within_day_order, ",
    "e.within_day_count, e.risk_end_days FROM multistate_timed e"
  )
  sentinel <- paste0(
    "SELECT e.cohort_row_id, e.subject_id AS person_id, 2 AS stream_kind, ",
    "NULL AS state_id, ", decimal_end,
    " AS state_time, e.risk_end_days AS state_day, 0 AS within_day_order, ",
    "0 AS within_day_count, e.risk_end_days FROM risk_episodes e"
  )
  sql <- paste0(
    "WITH ", paste(c(ctes, state_ctes), collapse = ", "),
    " SELECT s.cohort_row_id, s.person_id, s.stream_kind, s.state_id, ",
    "s.state_time, s.state_day, s.within_day_order, s.within_day_count, ",
    "s.risk_end_days FROM (", seed, " UNION ALL ", events,
    " UNION ALL ", sentinel, ") s ORDER BY s.cohort_row_id, ",
    "s.stream_kind, s.state_time, s.within_day_order, s.state_id"
  )
  list(sql = sql, state_ctes = state_ctes)
}

.multistateTransitionReference <- function(spec, tie_policy) {
  edges <- spec$edges
  outgoing <- table(factor(
    edges$from, levels = spec$states$state_id
  ))
  from_rank <- spec$states$hierarchy_rank[
    match(edges$from, spec$states$state_id)
  ]
  to_rank <- spec$states$hierarchy_rank[
    match(edges$to, spec$states$state_id)
  ]
  data.frame(
    from = as.integer(edges$from),
    to = as.integer(edges$to),
    trans = as.integer(edges$trans),
    from_name = edges$from_name,
    to_name = edges$to_name,
    from_hierarchy_rank = as.integer(from_rank),
    to_hierarchy_rank = as.integer(to_rank),
    from_is_initial = edges$from == spec$initial_state_id,
    to_is_absorbing = as.integer(outgoing[as.character(edges$to)]) == 0L,
    tie_policy = rep(tie_policy, nrow(edges)),
    state_step = rep(
      if (identical(tie_policy, "sequential")) {
        as.numeric(spec$state_step)
      } else {
        NA_real_
      },
      nrow(edges)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.multistateTransitionMatrix <- function(spec) {
  state_names <- spec$states$state_name
  matrix <- matrix(
    NA_integer_, nrow = length(state_names), ncol = length(state_names),
    dimnames = list(from = state_names, to = state_names)
  )
  matrix[cbind(spec$edges$from, spec$edges$to)] <- spec$edges$trans
  matrix
}

.emptyMultistateData <- function() {
  data.frame(
    row_id = numeric(0),
    cohort_row_id = numeric(0),
    person_id = character(0),
    from = integer(0),
    to = integer(0),
    trans = integer(0),
    Tstart = numeric(0),
    Tstop = numeric(0),
    time = numeric(0),
    status = integer(0),
    from_name = character(0),
    to_name = character(0),
    state_visit_number = integer(0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# Create one stateful transform per execution attempt. The SQL stream includes
# a seed and a censor sentinel for every cohort episode, so a chunk boundary can
# never strand the final episode. Only the current episode/state is retained.
.newMultistateStreamTransformer <- function(compiled, max_rows = Inf) {
  spec <- compiled$multistate
  edges <- spec$edges
  edge_rows <- split(seq_len(nrow(edges)), edges$from)
  state <- new.env(parent = emptyenv())
  state$active <- FALSE
  state$total_rows <- 0

  emit_visit <- function(stop_time, active_to = NA_integer_) {
    indices <- edge_rows[[as.character(state$current_state)]]
    if (is.null(indices) || length(indices) == 0L ||
        stop_time <= state$current_time) {
      return(.emptyMultistateData())
    }
    visit_edges <- edges[indices, , drop = FALSE]
    data.frame(
      row_id = rep(state$cohort_row_id, nrow(visit_edges)),
      cohort_row_id = rep(state$cohort_row_id, nrow(visit_edges)),
      person_id = rep(state$person_id, nrow(visit_edges)),
      from = as.integer(visit_edges$from),
      to = as.integer(visit_edges$to),
      trans = as.integer(visit_edges$trans),
      Tstart = rep(as.numeric(state$current_time), nrow(visit_edges)),
      Tstop = rep(as.numeric(stop_time), nrow(visit_edges)),
      time = rep(as.numeric(stop_time - state$current_time), nrow(visit_edges)),
      status = as.integer(!is.na(active_to) & visit_edges$to == active_to),
      from_name = visit_edges$from_name,
      to_name = visit_edges$to_name,
      state_visit_number = rep(as.integer(state$visit_number),
                               nrow(visit_edges)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  transform <- function(chunk) {
    if (!is.data.frame(chunk)) {
      stop("Multi-state stream chunks must be data frames.", call. = FALSE)
    }
    if (nrow(chunk) == 0L) return(.emptyMultistateData())
    required <- c(
      "cohort_row_id", "person_id", "stream_kind", "state_id",
      "state_time", "state_day", "within_day_order", "within_day_count",
      "risk_end_days"
    )
    if (!all(required %in% names(chunk))) {
      stop("Multi-state SQL returned an invalid internal stream.",
           call. = FALSE)
    }
    emitted <- list()
    add_rows <- function(rows) {
      if (nrow(rows) == 0L) return(invisible(NULL))
      state$total_rows <- state$total_rows + nrow(rows)
      if (state$total_rows > max_rows) {
        stop("Multi-state output exceeds the server in-memory row cap.",
             call. = FALSE)
      }
      emitted[[length(emitted) + 1L]] <<- rows
      invisible(NULL)
    }
    for (index in seq_len(nrow(chunk))) {
      kind <- suppressWarnings(as.integer(chunk$stream_kind[index]))
      episode <- as.character(chunk$cohort_row_id[index])
      person <- as.character(chunk$person_id[index])
      if (is.na(kind) || !kind %in% 0:2 || is.na(episode) || is.na(person)) {
        stop("Multi-state SQL returned a malformed internal stream.",
             call. = FALSE)
      }
      if (kind == 0L) {
        if (isTRUE(state$active)) {
          stop("Multi-state stream started an episode before its sentinel.",
               call. = FALSE)
        }
        state$active <- TRUE
        state$cohort_row_id <- suppressWarnings(as.numeric(episode))
        state$episode_key <- episode
        state$person_id <- person
        state$current_state <- as.integer(chunk$state_id[index])
        state$current_time <- as.numeric(chunk$state_time[index])
        state$visit_number <- 1L
        state$last_transition_day <- NA_integer_
        next
      }
      if (!isTRUE(state$active) || !identical(episode, state$episode_key) ||
          !identical(person, state$person_id)) {
        stop("Multi-state stream episode ordering is invalid.", call. = FALSE)
      }
      if (kind == 1L) {
        candidate_state <- suppressWarnings(as.integer(chunk$state_id[index]))
        candidate_time <- suppressWarnings(as.numeric(chunk$state_time[index]))
        candidate_day <- suppressWarnings(as.integer(chunk$state_day[index]))
        if (is.na(candidate_state) || is.na(candidate_time) ||
            !is.finite(candidate_time) || is.na(candidate_day)) {
          stop("Multi-state event ordering is invalid.", call. = FALSE)
        }
        if (identical(compiled$tie_policy, "priority") &&
            !is.na(state$last_transition_day) &&
            candidate_day == state$last_transition_day) {
          next
        }
        indices <- edge_rows[[as.character(state$current_state)]]
        active_edge <- if (is.null(indices)) integer(0) else
          indices[edges$to[indices] == candidate_state]
        if (length(active_edge) == 0L) next
        if (candidate_time <= state$current_time) {
          stop("Multi-state transitions must have strictly increasing analytic time.",
               call. = FALSE)
        }
        add_rows(emit_visit(candidate_time, candidate_state))
        state$current_state <- candidate_state
        state$current_time <- candidate_time
        state$last_transition_day <- candidate_day
        state$visit_number <- state$visit_number + 1L
        next
      }

      risk_end <- suppressWarnings(as.numeric(chunk$risk_end_days[index]))
      if (is.na(risk_end) || !is.finite(risk_end) ||
          risk_end < state$current_time) {
        stop("Multi-state censoring boundary is invalid.", call. = FALSE)
      }
      add_rows(emit_visit(risk_end))
      state$active <- FALSE
    }
    if (length(emitted) == 0L) return(.emptyMultistateData())
    do.call(rbind, emitted)
  }

  list(
    transform = transform,
    assert_complete = function() {
      if (isTRUE(state$active)) {
        stop("Multi-state stream ended before its censor sentinel.",
             call. = FALSE)
      }
      invisible(TRUE)
    }
  )
}

.executeMultistateMemory <- function(handle, compiled, chunk_size = NULL) {
  max_rows <- suppressWarnings(as.numeric(
    getOption("dsomop.max_memory_rows", 1000000L)
  ))
  if (length(max_rows) != 1L || is.na(max_rows) || !is.finite(max_rows) ||
      max_rows != floor(max_rows) || max_rows < 1L) {
    stop("dsomop.max_memory_rows must be one positive integer.",
         call. = FALSE)
  }
  max_fanout <- max(table(compiled$multistate$edges$from))
  default_chunk <- max(1L, floor(50000L / max_fanout))
  chunk_size <- chunk_size %||% default_chunk
  chunk_number <- suppressWarnings(as.numeric(chunk_size))
  if (length(chunk_number) != 1L || is.na(chunk_number) ||
      !is.finite(chunk_number) || chunk_number != floor(chunk_number) ||
      chunk_number < 1L || chunk_number > 1000000L) {
    stop("chunk_size must be one integer from 1 to 1,000,000.",
         call. = FALSE)
  }
  result <- .withDbReconnect(handle, function(conn) {
    machine <- .newMultistateStreamTransformer(compiled, max_rows = max_rows)
    rs <- DBI::dbSendQuery(conn, compiled$sql)
    on.exit(if (DBI::dbIsValid(rs)) DBI::dbClearResult(rs), add = TRUE)
    chunks <- list()
    repeat {
      chunk <- DBI::dbFetch(rs, n = as.integer(chunk_number))
      if (nrow(chunk) == 0L) break
      names(chunk) <- tolower(names(chunk))
      chunk <- .coerce_integer64(chunk, stable = TRUE)
      transformed <- machine$transform(chunk)
      if (nrow(transformed) > 0L) {
        chunks[[length(chunks) + 1L]] <- transformed
      }
    }
    DBI::dbClearResult(rs)
    machine$assert_complete()
    if (length(chunks) == 0L) .emptyMultistateData() else
      do.call(rbind, chunks)
  })
  attr(result, "trans") <- .multistateTransitionMatrix(compiled$multistate)
  attr(result, "dsomop_multistate") <- compiled$semantics$multi_state
  class(result) <- c("msdata", "data.frame")
  list(
    msdata = result,
    transition_ref = compiled$components$transition_ref
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
#' `format = "multi_state"` runs a bounded-memory state machine over the
#' ordered stream and returns an expanded mstate risk set plus its public
#' transition reference.
#'
#' @param handle CDM handle.
#' @param cohort_table Cohort table with subject_id/start/end columns.
#' @param outcomes Uniquely named list of table/concept_set specifications.
#' @param tar Index-relative start/end day offsets.
#' @param censoring Clinical censoring choices. Cohort end is mandatory;
#'   observation-period end and death default to enabled. admin_date is an
#'   optional controller-provided ISO date.
#' @param format survival, competing_risk, recurrent_events,
#'   counting_process, or multi_state.
#' @param event_order first, last, or all. last is retained for legacy survival
#'   plans; all is available for recurrent/counting/multi-state formats.
#' @param washout_days Minimum clean interval between raw events of the same
#'   named outcome. Events in the pre-entry lookback participate in washout.
#' @param tie_policy priority, all, or sequential. all is restricted to
#'   recurrent_events. sequential is restricted to multi_state. The historical
#'   error policy is rejected before querying because data-dependent failures
#'   form a disclosure oracle.
#' @param transitions Public mstate transition matrix, adjacency list, or
#'   canonical states/edges contract. Required by multi_state.
#' @param initial_state State occupied at time-at-risk entry.
#' @param state_hierarchy Public order for simultaneous state observations.
#' @param state_step Positive decimal separation used only for sequential
#'   same-day ordering.
#' @param legacy Emit the historical single-outcome survival columns and
#'   time-to-event semantics. Intended only for old plan shapes.
#' @return A `dsomop_longitudinal_sql` internal execution contract.
#' @keywords internal
.compileLongitudinalSurvivalSql <- function(
    handle, cohort_table, outcomes, tar = NULL, censoring = NULL,
    format = "survival", event_order = NULL, washout_days = 0L,
    tie_policy = "priority", transitions = NULL, initial_state = NULL,
    state_hierarchy = NULL, state_step = NULL, legacy = FALSE) {
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
    "survival", "competing_risk", "recurrent_events", "counting_process",
    "multi_state"
  )
  if (!format %in% allowed_formats) {
    stop("format must be survival, competing_risk, recurrent_events, or ",
         "counting_process, or multi_state.", call. = FALSE)
  }
  if (!is.logical(legacy) || length(legacy) != 1L || is.na(legacy)) {
    stop("legacy must be TRUE or FALSE.", call. = FALSE)
  }
  if (legacy && (format != "survival" || length(outcomes) != 1L)) {
    stop("legacy survival requires format='survival' and exactly one outcome.",
         call. = FALSE)
  }
  event_order <- event_order %||% if (format %in%
    c("recurrent_events", "counting_process", "multi_state")) "all" else "first"
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
  if (format == "multi_state" && event_order != "all") {
    stop("multi_state requires event_order='all'.", call. = FALSE)
  }
  if (!is.character(tie_policy) || length(tie_policy) != 1L ||
      is.na(tie_policy) ||
      !tolower(tie_policy) %in% c("priority", "error", "all", "sequential")) {
    stop("tie_policy must be priority, error, all, or sequential.",
         call. = FALSE)
  }
  tie_policy <- tolower(tie_policy)
  if (identical(tie_policy, "error")) {
    stop("tie_policy='error' is unavailable because a data-dependent query ",
         "failure creates a disclosure oracle; use deterministic priority or ",
         "the format-specific all/sequential policy.", call. = FALSE)
  }
  if (tie_policy == "all" && format != "recurrent_events") {
    stop("tie_policy='all' is supported only for recurrent_events; other ",
         "formats require one daily event chosen by priority.",
         call. = FALSE)
  }
  if (tie_policy == "sequential" && format != "multi_state") {
    stop("tie_policy='sequential' is supported only for multi_state.",
         call. = FALSE)
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
  multistate <- if (identical(format, "multi_state")) {
    if (legacy) {
      stop("multi_state requires the advanced named-outcomes contract.",
           call. = FALSE)
    }
    .normalizeMultistateSpec(
      outcomes, transitions, initial_state, state_hierarchy, state_step
    )
  } else {
    if (!is.null(transitions) || !is.null(initial_state) ||
        !is.null(state_hierarchy) || !is.null(state_step)) {
      stop("Multi-state graph fields require format='multi_state'.",
           call. = FALSE)
    }
    NULL
  }

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
  required_observation_start <- .survivalDateAdd(
    handle, tar$start_offset - washout_days, "eb.cohort_start_date"
  )
  censor_expression <- .survivalLeastDate(censor_candidates)
  eligibility <- paste0(
    "op.op_count = 1 AND op.observation_period_start_date IS NOT NULL AND ",
    "op.observation_period_end_date IS NOT NULL AND ",
    "op.observation_period_start_date <= ", required_observation_start,
    " AND op.observation_period_end_date >= ", tar_start_expression
  )
  if (censoring$death) {
    eligibility <- paste0(
      eligibility,
      " AND (d.death_count IS NULL OR (d.death_count = 1 AND ",
      "d.death_date IS NOT NULL AND d.death_date >= eb.cohort_start_date))"
    )
  }
  bounds_cte <- paste0(
    "episode_bounds AS (SELECT eb.cohort_row_id, eb.subject_id, ",
    "eb.cohort_start_date, ", tar_start_expression,
    " AS tar_start_date, ", censor_expression, " AS censor_date FROM ",
    "episode_base eb", paste(joins, collapse = ""), " WHERE ",
    eligibility, ")"
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
  ctes <- c(ctes, paste0(
    "raw_events_unranked AS (",
    paste(event_sources, collapse = " UNION ALL "), ")"
  ), paste0(
    "raw_events_ranked AS (SELECT r.*, ROW_NUMBER() OVER (PARTITION BY ",
    "r.cohort_row_id, r.outcome_priority, r.event_key ORDER BY ",
    "r.event_date, r.subject_id, r.outcome_name, r.event_key) AS ",
    "dsomop_event_key_rank FROM raw_events_unranked r)"
  ), paste0(
    "raw_events AS (SELECT r.cohort_row_id, r.subject_id, ",
    "r.cohort_start_date, r.tar_start_date, r.censor_date, ",
    "r.risk_start_days, r.risk_end_days, r.outcome_name, ",
    "r.outcome_priority, r.event_date, r.event_key FROM ",
    "raw_events_ranked r WHERE r.dsomop_event_key_rank = 1)"
  ))

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
  multistate_stream <- if (identical(format, "multi_state")) {
    .compileMultistateStreamSql(
      handle, ctes, outcomes, multistate, tie_policy
    )
  } else {
    NULL
  }

  # Private row-level data-quality conditions are eligibility rules, not
  # observable validation failures. The only data-dependent failure is the
  # generic DataShield population gate over the final eligible risk set.
  validation_sql <- list()
  selected_ctes <- character(0)
  selected_source <- "eligible_events"
  if (!format %in% c("survival", "multi_state") &&
      identical(tie_policy, "priority")) {
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
  } else if (identical(format, "multi_state")) {
    output_sql <- multistate_stream$sql
    columns <- names(.emptyMultistateData())
    components$transition_ref <- .multistateTransitionReference(
      multistate, tie_policy
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
      "WITH ", paste(risk_ctes, collapse = ", "),
      " SELECT COUNT(DISTINCT subject_id) AS n_persons FROM risk_episodes"
    ),
    columns = columns,
    semantics = list(
      grain = if (identical(format, "survival")) {
        "episode_outcome"
      } else if (identical(format, "competing_risk")) {
        "episode"
      } else if (identical(format, "counting_process")) {
        "episode_interval"
      } else if (identical(format, "multi_state")) {
        "episode_transition"
      } else {
        "episode_event"
      },
      recurrent_episodes = TRUE,
      event_order = event_order,
      tie_policy = tie_policy,
      outcome_priority = if (identical(format, "multi_state")) {
        outcome_names <- vapply(outcomes, `[[`, character(1L), "name")
        multistate$states$state_name[
          multistate$states$state_name %in% outcome_names
        ]
      } else {
        vapply(outcomes, `[[`, character(1), "name")
      },
      tar_start_offset = tar$start_offset,
      tar_end_offset = tar$end_offset,
      washout_days = washout_days,
      censoring = list(
        cohort_end = TRUE,
        observation_period_end = censoring$observation_period_end,
        death = censoring$death,
        administrative = !is.null(admin_expression)
      ),
      date_output = if (identical(format, "multi_state") &&
                         identical(tie_policy, "sequential")) {
        "relative_day_offsets_with_public_within_day_order"
      } else {
        "integer_offsets_only"
      },
      internal_person_id = TRUE,
      source_event_identifiers_output = FALSE,
      interval_convention = if (format %in%
                                  c("counting_process", "multi_state")) {
        paste0(
          "(start, stop] over integer day-end boundaries; the TAR start day ",
          "begins at start_offset - 1",
          if (identical(format, "multi_state") &&
              identical(tie_policy, "sequential")) {
            "; public state_hierarchy/state_step order simultaneous states"
          } else {
            ""
          }
        )
      } else {
        NULL
      },
      multi_state = if (is.null(multistate)) NULL else list(
        contract_version = "dsomop-multistate-v1",
        initial_state = multistate$initial_state,
        transitions = multistate$transitions,
        state_hierarchy = multistate$state_hierarchy,
        state_step = if (identical(tie_policy, "sequential")) {
          multistate$state_step
        } else {
          NULL
        },
        unreachable_event_policy = "skip_until_reachable",
        same_day_policy = tie_policy
      )
    ),
    dbms = dbms,
    legacy = legacy,
    tie_policy = tie_policy,
    multistate = multistate
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
    transitions = output$transitions,
    initial_state = output$initial_state,
    state_hierarchy = output$state_hierarchy,
    state_step = output$state_step,
    legacy = legacy
  )
}

# Run the disclosure precondition before either DBI fetch or incremental
# staging. Keeping this separate prevents a caller from streaming the SQL while
# accidentally omitting its fail-closed eligible-population gate.
.validateLongitudinalSurvivalSql <- function(handle, compiled) {
  if (!inherits(compiled, "dsomop_longitudinal_sql") ||
      !is.character(compiled$sql) || length(compiled$sql) != 1L) {
    stop("compiled must be a dsomop_longitudinal_sql contract.",
         call. = FALSE)
  }
  # The compiler's risk CTEs are already rendered for the target dialect.
  # Execute that gate once, then reuse the common fail-closed nfilter check;
  # passing it back as SQL would translate PostgreSQL/MySQL expressions twice.
  gate <- .executeQuery(handle, compiled$population_gate_sql)
  n_persons <- if (nrow(gate) == 1L && ncol(gate) == 1L) {
    suppressWarnings(as.numeric(gate[[1L]][1L]))
  } else {
    NA_real_
  }
  .assertMinPersons(n_persons = n_persons)
  invisible(TRUE)
}

#' Execute a compiled longitudinal SQL contract with its safety validations
#'
#' @keywords internal
.executeLongitudinalSurvivalSql <- function(handle, compiled,
                                            chunk_size = NULL) {
  .validateLongitudinalSurvivalSql(handle, compiled)
  if (identical(compiled$format, "recurrent_events")) {
    return(list(
      events = .executeQuery(handle, compiled$sql),
      risk_sets = .executeQuery(handle, compiled$components$risk_sets)
    ))
  }
  if (identical(compiled$format, "multi_state")) {
    return(.executeMultistateMemory(handle, compiled, chunk_size = chunk_size))
  }
  .executeQuery(handle, compiled$sql)
}
