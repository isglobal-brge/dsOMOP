# Module: Extraction Plan
# Plan construction, validation, preview, and execution for multi-table extractions.

#' Output types that require a resolved cohort table
#'
#' @return Character vector of cohort-dependent output types.
#' @keywords internal
.planCohortOutputTypes <- function() {
  c(
    "baseline", "survival", "cohort_membership", "intervals_long",
    "temporal_covariates", "person_period"
  )
}

#' Detect cohort-dependent outputs with no declared cohort-producing source
#'
#' This is a structural preflight only. Execution performs a second check after
#' scopes and populations have been resolved, because preview/validation must
#' not materialize cohorts merely to prove that a declaration produces rows.
#'
#' @param plan Extraction plan.
#' @return Character vector of output-specific validation errors.
#' @keywords internal
.planRequiredCohortErrors <- function(plan) {
  outputs <- plan$outputs %||% list()
  if (!is.list(outputs) || length(outputs) == 0L || is.null(names(outputs))) {
    return(character(0))
  }

  scope <- plan$scope
  scope_source <- if (is.list(scope) && !is.data.frame(scope)) {
    scope$tables_frames %||% scope$cohort
  } else {
    scope
  }
  has_scope <- !is.null(scope_source) && length(scope_source) > 0L
  cohort <- plan$cohort
  has_plan_cohort <- is.list(cohort) && (
    (!is.null(cohort$cohort_definition_id) &&
       identical(cohort$type, "cohort_table")) ||
      !is.null(cohort$filter_tree) || !is.null(cohort$spec)
  )
  base_declared <- has_scope || has_plan_cohort
  populations <- plan$populations %||% list()
  multi_population <- .planHasMultiPopulation(plan)

  has_population_cohort <- function(population) {
    is.list(population) && any(vapply(
      c("cohort_definition_id", "filter_tree", "index_event", "setop"),
      function(field) !is.null(population[[field]]), logical(1)
    ))
  }

  errors <- character(0)
  for (out_name in names(outputs)) {
    out <- outputs[[out_name]]
    out_type <- if (is.list(out)) out$type %||% "event_level" else ""
    if (!is.character(out_type) || length(out_type) != 1L || is.na(out_type) ||
        !tolower(out_type) %in% .planCohortOutputTypes()) {
      next
    }
    population_id <- out$population_id %||% "base"
    population_label <- if (is.character(population_id) &&
                            length(population_id) == 1L &&
                            !is.na(population_id) && nzchar(population_id)) {
      population_id
    } else {
      "<invalid>"
    }
    declared <- if (!multi_population) {
      base_declared
    } else if (!is.character(population_id) || length(population_id) != 1L ||
               is.na(population_id) || !population_id %in% names(populations)) {
      FALSE
    } else if (identical(population_id, "base")) {
      base_declared || has_population_cohort(populations[[population_id]])
    } else {
      has_population_cohort(populations[[population_id]])
    }
    if (!declared) {
      errors <- c(errors, paste0(
        "Output '", out_name, "' (type '", tolower(out_type),
        "') requires a cohort; no executable cohort source is declared for ",
        "population '", population_label, "'."
      ))
    }
  }
  errors
}

#' Validate a plan against the handle's schema
#'
#' @param handle CDM handle
#' @param plan List; the extraction plan
#' @return List with validation results
#' @keywords internal
.planValidate <- function(handle, plan) {
  bp <- .buildBlueprint(handle)
  errors <- character(0)
  warnings <- character(0)

  present_tables <- bp$tables$table_name[bp$tables$present_in_db]

  if (!is.null(plan$cohort)) {
    if (!is.null(plan$cohort$cohort_definition_id)) {
      results_tables <- .listTablesRaw(handle, .effectiveResultsSchema(handle))
      if (!"cohort" %in% results_tables) {
        errors <- c(errors,
          "Plan cohort cannot execute: results cohort table not found.")
      }
    }
  }

  validate_filter_tree <- function(tree, context) {
    tree_error <- tryCatch({
      .validateCohortFilterTree(tree)
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(tree_error)) {
      errors <<- c(errors, paste0(context, ": ", tree_error))
    }
  }
  if (!is.null(plan$cohort$filter_tree)) {
    validate_filter_tree(plan$cohort$filter_tree, "Plan cohort filter_tree")
  }
  populations <- plan$populations %||% list()
  if (is.list(populations)) {
    population_names <- names(populations)
    for (i in seq_along(populations)) {
      population <- populations[[i]]
      if (!is.list(population) || is.null(population$filter_tree)) next
      population_name <- if (!is.null(population_names) &&
                             !is.na(population_names[[i]]) &&
                             nzchar(population_names[[i]])) {
        population_names[[i]]
      } else {
        as.character(i)
      }
      validate_filter_tree(
        population$filter_tree,
        paste0("Population '", population_name, "' filter_tree")
      )
    }
  }

  outputs <- plan$outputs %||% list()
  errors <- c(errors, .planRequiredCohortErrors(plan))
  for (out_name in names(outputs)) {
    out <- outputs[[out_name]]
    out_type <- out$type %||% "event_level"

    if (out_type == "person_level" && !is.null(out$tables)) {
      for (tbl_name in names(out$tables)) {
        tbl_lower <- tolower(tbl_name)
        if (!tbl_lower %in% present_tables) {
          errors <- c(errors,
            paste0("Output '", out_name, "': table '", tbl_name, "' not found."))
          next
        }
        entry <- out$tables[[tbl_name]]
        if (is.list(entry) && !is.null(entry$features)) next
        req_cols <- tolower(.colSpec(entry)$source %||% character(0))
        avail_cols <- bp$columns[[tbl_lower]]$column_name
        missing <- setdiff(req_cols, avail_cols)
        if (length(missing) > 0) {
          warnings <- c(warnings,
            paste0("Output '", out_name, "', table '", tbl_name,
                   "': columns not found: ", paste(missing, collapse = ", ")))
        }
      }
    }

    if (out_type == "event_level") {
      tbl_name <- tolower(out$table %||% "")
      if (!tbl_name %in% present_tables) {
        errors <- c(errors,
          paste0("Output '", out_name, "': table '", tbl_name, "' not found."))
      } else {
        avail_cols <- bp$columns[[tbl_name]]$column_name
        if (!is.null(out$columns)) {
          req_cols <- tolower(out$columns)
          missing <- setdiff(req_cols, avail_cols)
          if (length(missing) > 0) {
            warnings <- c(warnings,
              paste0("Output '", out_name, "': columns not found: ",
                     paste(missing, collapse = ", ")))
          }
        }
      }
    }

    if (out_type == "baseline") {
      if (!"person" %in% present_tables) {
        errors <- c(errors,
          paste0("Output '", out_name, "': person table not found."))
      }
      if (!"observation_period" %in% present_tables) {
        warnings <- c(warnings,
          paste0("Output '", out_name,
                 "': observation_period not found; derived fields unavailable."))
      }
      if (!is.null(out$age_breaks)) {
        age_error <- tryCatch({
          .computeAgeGroups(
            2000L, 2020L, age_breaks = out$age_breaks, min_cell = 0L
          )
          NULL
        }, error = function(e) conditionMessage(e))
        if (!is.null(age_error)) {
          errors <- c(errors, paste0(
            "Output '", out_name, "': invalid age_breaks: ", age_error
          ))
        }
      }
    }

    if (out_type == "survival") {
      survival_outcomes <- out$outcomes %||% list(outcome = out$outcome)
      if (!is.list(survival_outcomes) || length(survival_outcomes) == 0L ||
          is.null(names(survival_outcomes)) ||
          any(!nzchar(names(survival_outcomes))) ||
          anyDuplicated(names(survival_outcomes))) {
        errors <- c(errors, paste0(
          "Output '", out_name, "': outcomes must be a non-empty named list."
        ))
      } else {
        for (outcome_name in names(survival_outcomes)) {
          outcome_tbl <- tolower(
            survival_outcomes[[outcome_name]]$table %||% ""
          )
          if (!nzchar(outcome_tbl) || !outcome_tbl %in% present_tables) {
            errors <- c(errors, paste0(
              "Output '", out_name, "': outcome '", outcome_name,
              "' table '", outcome_tbl, "' not found."
            ))
          }
        }
        survival_format <- out$format %||% "survival"
        if (is.character(survival_format) &&
            length(survival_format) == 1L && !is.na(survival_format) &&
            identical(tolower(survival_format), "multi_state")) {
          graph_error <- tryCatch({
            graph_outcomes <- lapply(names(survival_outcomes), function(name) {
              list(name = name)
            })
            .normalizeMultistateSpec(
              graph_outcomes,
              transitions = out$transitions,
              initial_state = out$initial_state,
              state_hierarchy = out$state_hierarchy,
              state_step = out$state_step
            )
            NULL
          }, error = conditionMessage)
          if (!is.null(graph_error)) {
            errors <- c(errors, paste0(
              "Output '", out_name, "': invalid multi-state graph: ",
              graph_error
            ))
          }
        }
      }
      censoring <- out$censoring %||% if (is.null(out$outcomes)) {
        list(observation_period_end = TRUE, death = FALSE)
      } else {
        list()
      }
      if (!identical(censoring$observation_period_end, FALSE) &&
          !"observation_period" %in% present_tables) {
        errors <- c(errors, paste0(
          "Output '", out_name,
          "': observation-period censoring requires observation_period."
        ))
      }
      if (!identical(censoring$death, FALSE) &&
          !"death" %in% present_tables) {
        errors <- c(errors, paste0(
          "Output '", out_name, "': death censoring requires death."
        ))
      }
    }

    if (out_type == "concept_dictionary") {
      if (!"concept" %in% present_tables) {
        warnings <- c(warnings,
          paste0("Output '", out_name,
                 "': concept table not found; dictionary will be empty."))
      }
    }

    if (out_type == "cohort_membership") {
      # No specific table requirements beyond cohort existing
    }

    if (out_type == "intervals_long") {
      interval_tables <- out$tables %||% character(0)
      for (itbl in interval_tables) {
        if (!tolower(itbl) %in% present_tables) {
          errors <- c(errors,
            paste0("Output '", out_name,
                   "': interval table '", itbl, "' not found."))
        }
      }
      longitudinal_error <- tryCatch({
        .normalizeLongitudinalWindow(
          out$window, out$interval_match %||% "overlaps"
        )
        .normalizeLongitudinalSelection(
          out$event_select %||% "all",
          out$select_n %||% 1L,
          out$select_by %||% "episode_source",
          out$anchor %||% 0L
        )
        if (!is.null(out$source_filters)) {
          for (itbl in interval_tables) {
            .longitudinalSourceFilter(
              out$source_filters, itbl, interval_tables
            )
          }
        }
        NULL
      }, error = function(e) conditionMessage(e))
      if (!is.null(longitudinal_error)) {
        errors <- c(errors, paste0(
          "Output '", out_name, "': ", longitudinal_error
        ))
      }
    }

    if (out_type %in% c("temporal_covariates", "person_period")) {
      tc_table <- tolower(out$table %||% "")
      if (tc_table != "" && !tc_table %in% present_tables) {
        errors <- c(errors,
          paste0("Output '", out_name,
                 "': table '", tc_table, "' not found."))
      }
      if (identical(out_type, "person_period") &&
          (!is.character(out$grain) || length(out$grain) != 1L ||
           is.na(out$grain) || !identical(tolower(out$grain), "episode") ||
           !is.character(out$time_origin) ||
           length(out$time_origin) != 1L || is.na(out$time_origin) ||
           !identical(tolower(out$time_origin), "index"))) {
        errors <- c(errors, paste0(
          "Output '", out_name,
          "': person_period requires grain='episode' and time_origin='index'."
        ))
      }
    }
  }

  list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    available_tables = present_tables
  )
}

#' Preview resolvable plan projections (safe aggregate)
#'
#' @param handle CDM handle
#' @param plan List; the extraction plan
#' @return Validation plus per-output source/projection metadata. Column names
#'   are final only when \code{columns_complete} is true. Person counts, when
#'   available, are disclosure-banded source-table counts, never row-count
#'   estimates for the executed output.
#' @keywords internal
.planPreview <- function(handle, plan) {
  bp <- .buildBlueprint(handle)
  validation <- .planValidate(handle, plan)
  cohort_errors <- validation$errors[grepl(
    "no executable cohort source|Plan cohort cannot execute",
    validation$errors
  )]
  if (length(cohort_errors) > 0L) {
    stop("Plan preview rejected: ", paste(cohort_errors, collapse = " "),
         call. = FALSE)
  }
  settings <- .omopDisclosureSettings()

  # Disclosure note (differencing defence):
  # The per-output n_persons counts below are the most-repeated, lowest-cost
  # query in the API and are therefore the primary differencing signal. The
  # nfilter_subset suppression already replaces any count < threshold with NA,
  # but suppression of small cells does NOT protect against exact-count
  # differencing: an attacker who narrows a filter and reads two EXACT
  # supra-threshold counts (e.g. a 50 -> 47 funnel delta) recovers the size of
  # the differenced subgroup without ever tripping the small-cell guard. The
  # defence is twofold: (1) BAND every surviving count down to a multiple of
  # band_width via .bandCount() so the 1-person resolution that differencing
  # exploits is destroyed, and (2) audit-log the preview call (see
  # omopPlanPreviewDS) so the data controller can detect repeated/probing
  # previews that banding alone cannot stop. min/max are never returned.
  band_width <- settings$nfilter_band

  # Preview deliberately does not materialize cohorts or execute filter trees.
  # A table-wide count is therefore meaningful only for a demonstrably
  # unrestricted output.  Scoped/filtered counts are reported as unavailable
  # instead of presenting the source-table population as if it described the
  # requested output.
  plan_is_scoped <- !is.null(plan$cohort) || !is.null(plan$scope) ||
    .planHasMultiPopulation(plan)
  preview_person_count <- function(tbl_row, col_df, restricted,
                                   unavailable_reason) {
    if (!"person_id" %in% col_df$column_name) {
      return(list(
        n_persons = NA_real_, n_persons_available = FALSE,
        n_persons_unavailable_reason = "source table is not person-keyed",
        n_persons_banded = FALSE, band_width = band_width,
        disclosive = NA
      ))
    }
    if (isTRUE(restricted)) {
      return(list(
        n_persons = NA_real_, n_persons_available = FALSE,
        n_persons_unavailable_reason = unavailable_reason,
        n_persons_banded = FALSE, band_width = band_width,
        disclosive = NA
      ))
    }

    sql <- paste0("SELECT COUNT(DISTINCT person_id) AS n FROM ",
                  tbl_row$qualified_name[1])
    n <- .executeQuery(handle, sql)$n[1]
    disclosive <- !is.na(n) && n < settings$nfilter_subset
    n_banded <- if (disclosive) NA_real_ else .bandCount(n, band_width)
    list(
      n_persons = n_banded, n_persons_available = TRUE,
      n_persons_unavailable_reason = NULL,
      n_persons_banded = !is.na(n_banded), band_width = band_width,
      disclosive = disclosive
    )
  }
  preview_feature_schema <- function(specs, table, grain = "person") {
    specs <- specs %||% list()
    keys <- c(if (identical(tolower(grain), "episode")) {
      "cohort_row_id"
    }, "person_id")
    if (length(specs) == 0L) {
      return(list(
        columns = keys,
        columns_complete = FALSE,
        columns_unavailable_reason =
          "automatic feature columns depend on concepts observed at execution"
      ))
    }
    resolved <- .resolveFeatureSpecs(handle, specs, table = table)
    list(
      columns = unique(c(keys, vapply(
        resolved, function(spec) spec$name, character(1)
      ))),
      columns_complete = TRUE,
      columns_unavailable_reason = NULL
    )
  }

  preview <- list(
    validation = validation,
    outputs = list(),
    band_width = band_width
  )

  outputs <- plan$outputs %||% list()
  for (out_name in names(outputs)) {
    out <- outputs[[out_name]]
    out_type <- out$type %||% "event_level"
    out_preview <- list(type = out_type)

    if (out_type == "person_level" && !is.null(out$tables)) {
      out_preview$tables <- list()
      for (tbl_name in names(out$tables)) {
        tbl_lower <- tolower(tbl_name)
        tbl_row <- bp$tables[bp$tables$table_name == tbl_lower & bp$tables$present_in_db, ,
                             drop = FALSE]
        if (nrow(tbl_row) == 0) next

        entry <- out$tables[[tbl_name]]
        col_df <- bp$columns[[tbl_lower]]
        entry_has_features <- is.list(entry) && !is.null(entry$features)
        if (entry_has_features) {
          source_columns <- col_df$column_name
          missing_source_columns <- character(0)
          schema_info <- preview_feature_schema(
            entry$features, table = tbl_lower, grain = "person"
          )
        } else {
          column_spec <- .colSpec(entry)
          req_cols <- tolower(column_spec$source %||% col_df$column_name)
          source_columns <- intersect(req_cols, col_df$column_name)
          missing_source_columns <- setdiff(req_cols, col_df$column_name)
          schema_info <- list(
            columns = source_columns,
            columns_complete = TRUE,
            columns_unavailable_reason = NULL
          )
        }
        entry_is_restricted <- is.list(entry) && any(c(
          "features", "filters", "concept_set", "visit", "temporal"
        ) %in% names(entry))
        output_is_restricted <- plan_is_scoped ||
          !identical(out$population_id %||% "base", "base") ||
          length(out$filters %||% list()) > 0L || entry_is_restricted
        count_info <- preview_person_count(
          tbl_row, col_df, output_is_restricted,
          "plan cohort, population, or output filters are not executed by preview"
        )
        out_preview$tables[[tbl_name]] <- c(list(
          columns = schema_info$columns,
          columns_complete = schema_info$columns_complete,
          columns_unavailable_reason = schema_info$columns_unavailable_reason,
          source_columns = source_columns,
          missing_columns = missing_source_columns
        ), count_info)
      }
    }

    if (out_type == "event_level") {
      tbl_lower <- tolower(out$table %||% "")
      tbl_row <- bp$tables[bp$tables$table_name == tbl_lower & bp$tables$present_in_db, ,
                           drop = FALSE]
      if (nrow(tbl_row) > 0) {
        col_df <- bp$columns[[tbl_lower]]
        req_cols <- tolower(out$columns %||% col_df$column_name)
        source_columns <- intersect(req_cols, col_df$column_name)
        missing_source_columns <- setdiff(req_cols, col_df$column_name)
        representation <- out$representation$format %||% "long"
        if (identical(tolower(representation), "features")) {
          schema_info <- preview_feature_schema(
            out$representation$features, table = tbl_lower,
            grain = out$representation$grain %||% "person"
          )
        } else {
          schema_info <- list(
            columns = source_columns,
            columns_complete = TRUE,
            columns_unavailable_reason = NULL
          )
        }

        output_is_restricted <- plan_is_scoped ||
          !identical(out$population_id %||% "base", "base") ||
          length(out$filters %||% list()) > 0L ||
          !is.null(out$concept_set) || !is.null(out$temporal) ||
          !is.null(out$visit_filter) ||
          length(out$representation$features %||% list()) > 0L
        count_info <- preview_person_count(
          tbl_row, col_df, output_is_restricted,
          "plan cohort, population, filters, or reductions are not executed by preview"
        )
        out_preview <- c(out_preview, list(
          table = out$table,
          columns = schema_info$columns,
          columns_complete = schema_info$columns_complete,
          columns_unavailable_reason = schema_info$columns_unavailable_reason,
          source_columns = source_columns,
          missing_columns = missing_source_columns,
          representation = representation
        ), count_info)
      }
    }

    if (out_type == "baseline") {
      out_preview$columns <- out$columns %||%
        c("gender_concept_id", "race_concept_id")
      out_preview$derived <- out$derived %||% character(0)
      out_preview$description <- "One row per cohort member with demographics"
    }

    if (out_type == "survival") {
      survival_outcomes <- out$outcomes %||% list(outcome = out$outcome)
      out_preview$outcomes <- lapply(survival_outcomes, function(outcome) {
        list(
          table = outcome$table %||% "",
          concept_set = outcome$concept_set %||% integer(0)
        )
      })
      out_preview$tar <- out$tar %||% list(start_offset = 0)
      out_preview$format <- out$format %||% "survival"
      out_preview$event_order <- out$event_order %||% "first"
      out_preview$censoring <- out$censoring %||%
        if (is.null(out$outcomes)) {
          list(cohort_end = TRUE, observation_period_end = TRUE, death = FALSE)
        } else {
          list(cohort_end = TRUE, observation_period_end = TRUE, death = TRUE)
        }
      out_preview$columns <- if (is.null(out$outcomes)) {
        c("row_id", "cohort_row_id", "person_id", "event",
          "time_to_event_days")
      } else if (identical(out_preview$format, "recurrent_events")) {
        c("row_id", "cohort_row_id", "person_id", "outcome_name", "event",
          "event_number", "outcome_event_number", "event_days_from_index",
          "entry_days_from_index", "exit_days_from_index")
      } else if (identical(out_preview$format, "counting_process")) {
        c("row_id", "cohort_row_id", "person_id", "outcome_name", "event",
          "interval_number", "interval_start_days", "interval_end_days")
      } else if (identical(out_preview$format, "multi_state")) {
        names(.emptyMultistateData())
      } else {
        c("row_id", "cohort_row_id", "person_id", "outcome_name", "event",
          "entry_days_from_index", "exit_days_from_index", "follow_up_days")
      }
      if (identical(out_preview$format, "recurrent_events")) {
        out_preview$components <- c("events", "risk_sets")
      } else if (identical(out_preview$format, "multi_state")) {
        out_preview$components <- c("msdata", "transition_ref")
        out_preview$multi_state <- list(
          initial_state = out$initial_state,
          transitions = out$transitions,
          state_hierarchy = out$state_hierarchy,
          state_step = out$state_step
        )
      }
      out_preview$columns_complete <- TRUE
      out_preview$description <- if (is.null(out$outcomes)) {
        "Historical single-outcome time-to-event contract"
      } else {
        paste0(
          "Longitudinal ", out_preview$format, " with named outcomes and ",
          "episode-specific clinical censoring"
        )
      }
    }

    if (out_type == "concept_dictionary") {
      out_preview$source_outputs <- out$source_outputs %||% "all"
      out_preview$description <- "Concept lookup table for referenced concepts"
    }

    if (out_type == "cohort_membership") {
      out_preview$description <- "Standard OHDSI cohort table format"
    }

    if (out_type == "intervals_long") {
      out_preview$tables <- out$tables %||% character(0)
      out_preview$window <- out$window %||% "cohort_episode"
      out_preview$interval_match <- out$interval_match %||% "overlaps"
      out_preview$event_select <- out$event_select %||% "all"
      out_preview$select_n <- out$select_n %||% 1L
      out_preview$select_by <- out$select_by %||% "episode_source"
      out_preview$description <- paste0(
        "Interval data from ",
        length(out$tables %||% character(0)), " tables"
      )
    }

    if (out_type %in% c("temporal_covariates", "person_period")) {
      out_preview$table <- out$table
      out_preview$bin_width <- out$bin_width %||% 30L
      out_preview$window <- list(
        start = out$window_start %||% -365L,
        end = out$window_end %||% 0L
      )
      out_preview$grain <- out$grain %||%
        if (identical(out_type, "person_period")) NA_character_ else NULL
      out_preview$time_origin <- out$time_origin %||%
        if (identical(out_type, "person_period")) NA_character_ else NULL
      out_preview$description <- paste0(
        if (identical(out_type, "person_period")) {
          "Complete episode-period panel from "
        } else {
          "Time-binned covariates from "
        }, out$table
      )
    }

    preview$outputs[[out_name]] <- out_preview
  }

  preview
}

.cohortFilterTypes <- function() {
  c("sex", "age_range", "age_group", "cohort", "has_concept",
    "not_has_concept", "concept_count", "prior_observation", "followup",
    "visit_count", "has_measurement", "missing_measurement")
}

.cohortFilterParamNames <- function(filter_type) {
  switch(tolower(filter_type),
    sex = "value",
    age_range = c("min", "max", "reference_date"),
    age_group = c("groups", "reference_date"),
    cohort = "cohort_definition_id",
    has_concept = c("concept_id", "concept_ids", "table", "concept_name",
                    "window", "min_count", "reference_date"),
    not_has_concept = c("concept_id", "concept_ids", "table", "concept_name",
                        "window", "reference_date"),
    concept_count = c("concept_id", "concept_ids", "table", "concept_name",
                      "window", "min_count", "reference_date"),
    prior_observation = c("min_days", "reference_date"),
    followup = c("min_days", "reference_date"),
    visit_count = c("min_count", "visit_concept_id", "visit_concept_ids",
                    "window", "reference_date"),
    has_measurement = c("concept_id", "concept_ids", "min_value", "max_value",
                        "safe_scope", "window", "reference_date"),
    missing_measurement = c("concept_id", "concept_ids", "window",
                            "reference_date"),
    character(0)
  )
}

.dateAddSql <- function(handle, days, expr) {
  .renderSql(handle, paste0(
    "DATEADD(day, ", as.integer(days), ", ", expr, ")"
  ))
}

# Build an " AND <alias>.<date_col> BETWEEN ..." predicate for a population
# EXISTS/NOT EXISTS subquery, scoping events to a window of day offsets relative
# to an ANCHOR date (negative = past), e.g. window=list(start=-365, end=0) is
# "in the prior year". The anchor is \code{anchor_sql} when supplied (a per-person
# SQL expression for the cohort index date, e.g. a correlated subquery against the
# cohort table), or a declared fixed reference date. Anchoring to the cohort
# index is what makes peri-index windows (washout / on-treatment / post-index)
# select the right events. A window without either anchor fails closed rather
# than changing meaning with the server wall clock. Returns "" when no window.
.windowPredicateSql <- function(handle, bp, table_name, alias, window,
                                anchor_sql = NULL, reference_date = NULL) {
  if (is.null(window)) return("")
  if (!is.list(window)) {
    stop("Population filter window must be a list with start and/or end.",
         call. = FALSE)
  }
  ws <- window$start; we <- window$end
  if (is.null(ws) && is.null(we)) {
    stop("Population filter window must contain start and/or end.",
         call. = FALSE)
  }
  date_col <- .getDateColumn(bp, table_name)
  if (is.null(date_col)) {
    stop("Population filter window cannot be applied to table '", table_name,
         "' because it has no usable date column.", call. = FALSE)
  }
  coerce_offset <- function(x, field) {
    int <- suppressWarnings(as.integer(x))
    num <- suppressWarnings(as.numeric(x))
    if (length(x) != 1L || length(int) != 1L || is.na(int) ||
        length(num) != 1L || is.na(num) || num != int) {
      stop("Population filter window ", field,
           " must be one integer day offset.", call. = FALSE)
    }
    int
  }
  ws <- if (!is.null(ws)) coerce_offset(ws, "start")
  we <- if (!is.null(we)) coerce_offset(we, "end")
  if (!is.null(ws) && !is.null(we) && ws > we) {
    stop("Population filter window start must not be after end.",
         call. = FALSE)
  }
  anchor <- if (!is.null(reference_date)) {
    fixed_date <- .isoDate(reference_date,
                           "population filter reference_date")
    .quoteLiteral(format(fixed_date, "%Y-%m-%d"))
  } else if (!is.null(anchor_sql)) {
    anchor_sql
  } else {
    stop("Population filter windows require a cohort index or an explicit ",
         "reference_date.", call. = FALSE)
  }
  parts <- character(0)
  if (!is.null(ws)) parts <- c(parts, paste0(
    " AND ", alias, ".", date_col, " >= ",
    .dateAddSql(handle, ws, anchor)))
  if (!is.null(we)) parts <- c(parts, paste0(
    " AND ", alias, ".", date_col, " <= ",
    .dateAddSql(handle, we, anchor)))
  paste(parts, collapse = "")
}

.validateCohortFilterTree <- function(node, .depth = 1L, .state = NULL) {
  if (!is.list(node) || length(node) == 0L) {
    stop("Population filter nodes must be non-empty lists.", call. = FALSE)
  }

  leaf_values <- if (!is.null(names(node)) && "type" %in% names(node)) {
    length(unlist(node$params, use.names = FALSE))
  } else 0L
  .state <- .filterComplexityVisit(.state, .depth, leaf_values)

  node_names <- names(node)
  if (!is.null(node_names) &&
      (anyNA(node_names) || any(!nzchar(node_names)) ||
       anyDuplicated(node_names))) {
    stop("Population filter nodes cannot have blank or duplicate fields.",
         call. = FALSE)
  }

  if (!is.null(node_names) && "type" %in% node_names) {
    if (!setequal(node_names, c("type", "params")) ||
        length(node_names) != 2L) {
      stop("Population filter leaves may contain only type and params and ",
           "cannot mix group fields.", call. = FALSE)
    }
    filter_type <- node$type
    if (!is.character(filter_type) || length(filter_type) != 1L ||
        is.na(filter_type) ||
        !tolower(filter_type) %in% .cohortFilterTypes()) {
      stop("Unknown population filter type.", call. = FALSE)
    }
    if (!is.list(node$params)) {
      stop("Population filter params must be a named list.", call. = FALSE)
    }
    param_names <- names(node$params)
    if (length(node$params) > 0L &&
        (is.null(param_names) || anyNA(param_names) ||
         any(!nzchar(param_names)) ||
         anyDuplicated(param_names))) {
      stop("Population filter params must be a uniquely named list.",
           call. = FALSE)
    }
    unknown <- setdiff(param_names %||% character(0),
                       .cohortFilterParamNames(filter_type))
    if (length(unknown) > 0L) {
      stop("Population filter '", tolower(filter_type),
           "' has unknown parameter(s): ", paste(unknown, collapse = ", "),
           ".", call. = FALSE)
    }
    .validateFilter(tolower(filter_type), node$params)
    return(invisible(TRUE))
  }

  group_keys <- intersect(node_names %||% character(0), c("and", "or"))
  if (length(group_keys) > 0L) {
    if (length(group_keys) != 1L || length(node_names) != 1L) {
      stop("Population filter nodes cannot mix AND/OR groups with each other ",
           "or with leaf fields.", call. = FALSE)
    }
    children <- node[[group_keys]]
    if (!is.list(children) || length(children) == 0L) {
      stop("Population filter ", toupper(group_keys),
           " group must contain at least one filter.", call. = FALSE)
    }
    for (child in children) {
      .validateCohortFilterTree(
        child, .depth = .depth + 1L, .state = .state
      )
    }
    return(invisible(TRUE))
  }

  # Preserve the legacy flat-array syntax as an implicit AND, but only for a
  # genuinely unnamed list. Named unknown fields are rejected fail-closed.
  if (is.null(node_names)) {
    for (child in node) {
      .validateCohortFilterTree(
        child, .depth = .depth + 1L, .state = .state
      )
    }
    return(invisible(TRUE))
  }

  stop("Unknown or malformed population filter specification.", call. = FALSE)
}

.isCohortFilterLeaf <- function(x) {
  if (!is.list(x) || is.null(names(x)) || !"type" %in% names(x)) return(FALSE)
  isTRUE(tryCatch({
    .validateCohortFilterTree(x)
    TRUE
  }, error = function(e) FALSE))
}

.isCohortFilterSpec <- function(x) {
  isTRUE(tryCatch({
    .validateCohortFilterTree(x)
    TRUE
  }, error = function(e) FALSE))
}

# Does a population filter need the cohort index date for its meaning?
# Recurrent cohorts require an explicit episode policy for these filters; using
# MIN(cohort_start_date) would silently turn a per-episode question into a
# first-episode question.
.cohortFilterUsesIndex <- function(node) {
  if (is.null(node) || length(node) == 0L) return(FALSE)
  if (!is.list(node)) return(TRUE)

  if (!is.null(node$type)) {
    type <- tolower(as.character(node$type)[1])
    params <- node$params %||% list()
    if (type == "age_range") return(is.null(params$reference_date))
    if (type == "age_group") return(is.null(params$reference_date))
    if (type %in% c("prior_observation", "followup")) {
      return(is.null(params$reference_date))
    }
    if (type %in% c("has_concept", "not_has_concept", "concept_count",
                    "visit_count", "has_measurement",
                    "missing_measurement")) {
      return(!is.null(params$window) && is.null(params$reference_date))
    }
    return(FALSE)
  }

  children <- if ("and" %in% names(node)) {
    node$and
  } else if ("or" %in% names(node)) {
    node$or
  } else {
    unname(node)
  }
  any(vapply(children, .cohortFilterUsesIndex, logical(1)))
}

.cohortHasMultipleEpisodes <- function(handle, cohort_table) {
  cohort_table <- .validateIdentifier(cohort_table, "index cohort table")
  sql <- paste0(
    "SELECT COUNT(*) AS n_multi FROM (",
    "SELECT d.subject_id FROM (SELECT DISTINCT subject_id, ",
    "cohort_start_date, cohort_end_date FROM ", cohort_table, ") AS d ",
    "GROUP BY d.subject_id HAVING COUNT(*) > 1) AS recurrent"
  )
  result <- .executeQuery(handle, sql)
  nrow(result) > 0L && !is.na(result$n_multi[1]) && result$n_multi[1] > 0
}

#' Build a cohort person_id set from population-level filters
#'
#' Translates flat filter specs or nested AND/OR filter trees into SQL WHERE
#' clauses on the person table and returns matching person IDs.
#'
#' @param handle CDM handle
#' @param filters List of filter specs from recipe_to_plan
#' @param index_cohort_table Character or NULL; a cohort temp table
#'   (subject_id, cohort_start_date) used to anchor windowed concept filters to
#'   each person's cohort index date. Without a cohort, windowed filters require
#'   an explicit fixed reference date.
#' @param episode_policy Character or NULL; how an index-dependent filter is
#'   evaluated when a person has recurrent cohort episodes. Supported values are
#'   \code{"any_episode"}, \code{"all_episodes"}, \code{"first_episode"}, and
#'   \code{"last_episode"}. Recurrent cohorts fail closed when this is NULL.
#' @return Integer vector of person_ids
#' @keywords internal
.buildCohortFromFilters <- function(handle, filters,
                                    index_cohort_table = NULL,
                                    episode_policy = NULL) {
  bp <- .buildBlueprint(handle)

  person_table <- bp$tables[bp$tables$table_name == "person" &
                              bp$tables$present_in_db, , drop = FALSE]
  if (nrow(person_table) == 0) return(integer(0))

  qualified_person <- person_table$qualified_name[1]
  person_cols <- bp$columns[["person"]]$column_name

  allowed_episode_policies <- c(
    "any_episode", "all_episodes", "first_episode", "last_episode"
  )
  if (!is.null(episode_policy)) {
    if (!is.character(episode_policy) || length(episode_policy) != 1L ||
        is.na(episode_policy) ||
        !tolower(episode_policy) %in% allowed_episode_policies) {
      stop("episode_policy must be one of: ",
           paste(allowed_episode_policies, collapse = ", "), ".",
           call. = FALSE)
    }
    episode_policy <- tolower(episode_policy)
  }

  # Per-person cohort index date used by windowed filters. Recurrent cohorts are
  # rejected unless their episode semantics are explicit: choosing the earliest
  # date implicitly would change the requested longitudinal estimand.
  index_anchor <- NULL
  uses_index <- .cohortFilterUsesIndex(filters)
  if (!is.null(index_cohort_table)) {
    index_cohort_table <- .validateIdentifier(
      index_cohort_table, "index cohort table"
    )
    recurrent <- uses_index &&
      .cohortHasMultipleEpisodes(handle, index_cohort_table)
    if (recurrent && is.null(episode_policy)) {
      stop("Population filters that depend on the cohort index cannot be ",
           "applied to a recurrent cohort without an explicit episode policy. ",
           "Declare any_episode, all_episodes, first_episode, or last_episode; ",
           "dsOMOP will not choose the earliest episode ",
           "silently.", call. = FALSE)
    }

    if (uses_index && !is.null(episode_policy) &&
        episode_policy %in% c("any_episode", "all_episodes")) {
      episode_where <- .compileCohortFilterWhere(
        handle, filters, bp, person_cols,
        index_anchor = "idx.cohort_start_date"
      )
      membership <- paste0(
        "EXISTS (SELECT 1 FROM ", index_cohort_table, " idx",
        " WHERE idx.subject_id = p.person_id)"
      )
      episode_match <- paste0(
        "SELECT 1 FROM ", index_cohort_table, " idx",
        " WHERE idx.subject_id = p.person_id AND (", episode_where, ")"
      )
      policy_where <- if (identical(episode_policy, "any_episode")) {
        paste0("EXISTS (", episode_match, ")")
      } else {
        # CASE treats FALSE and UNKNOWN alike, so an episode with missing data
        # cannot make an all-episodes criterion pass by SQL three-valued logic.
        paste0(
          membership,
          " AND NOT EXISTS (SELECT 1 FROM ", index_cohort_table, " idx",
          " WHERE idx.subject_id = p.person_id",
          " AND CASE WHEN (", episode_where,
          ") THEN 0 ELSE 1 END = 1)"
        )
      }
      sql <- paste0(
        "SELECT DISTINCT p.person_id FROM ", qualified_person, " p WHERE ",
        policy_where
      )
      result <- .executeQuery(handle, sql)
      return(if (nrow(result) > 0) result$person_id else integer(0))
    }

    anchor_aggregate <- if (identical(episode_policy, "last_episode")) {
      "MAX"
    } else {
      # With one episode this is the sole start date. For a recurrent cohort,
      # MIN is used only under the explicitly requested first_episode policy.
      "MIN"
    }
    index_anchor <- paste0(
      "(SELECT ", anchor_aggregate, "(idx.cohort_start_date) FROM ",
      index_cohort_table, " idx WHERE idx.subject_id = p.person_id)")
  }

  where_sql <- .compileCohortFilterWhere(handle, filters, bp, person_cols,
                                         index_anchor = index_anchor)

  sql <- paste0("SELECT DISTINCT p.person_id FROM ", qualified_person, " p")
  if (nzchar(where_sql)) {
    sql <- paste0(sql, " WHERE ", where_sql)
    if (uses_index && !is.null(index_cohort_table) &&
        !is.null(episode_policy) &&
        episode_policy %in% c("first_episode", "last_episode")) {
      sql <- paste0(
        "SELECT DISTINCT p.person_id FROM ", qualified_person, " p WHERE ",
        "EXISTS (SELECT 1 FROM ", index_cohort_table, " idx",
        " WHERE idx.subject_id = p.person_id) AND (", where_sql, ")"
      )
    }
  }

  result <- .executeQuery(handle, sql)
  if (nrow(result) > 0) result$person_id else integer(0)
}

#' Compile a cohort-filter tree to a SQL predicate
#'
#' @param handle CDM handle.
#' @param node Cohort-filter node or logical group.
#' @param bp OMOP blueprint.
#' @param person_cols Character vector of columns from the person table.
#' @param index_anchor Optional index-event anchor specification.
#' @param .depth Internal recursion depth.
#' @param .state Internal shared complexity counter.
#' @return Character SQL predicate.
#' @keywords internal
.compileCohortFilterWhere <- function(handle, node, bp, person_cols,
                                      index_anchor = NULL, .depth = 1L,
                                      .state = NULL) {
  if (is.null(node) || length(node) == 0) return("")

  leaf_values <- if (is.list(node) && !is.null(names(node)) &&
      "type" %in% names(node)) {
    length(unlist(node$params, use.names = FALSE))
  } else 0L
  .state <- .filterComplexityVisit(.state, .depth, leaf_values)

  if (!is.list(node)) {
    stop("Population filter nodes must be lists.", call. = FALSE)
  }
  node_names <- names(node)
  if (!is.null(node_names) &&
      (any(!nzchar(node_names)) || anyDuplicated(node_names))) {
    stop("Population filter nodes cannot have blank or duplicate fields.",
         call. = FALSE)
  }

  if (!is.null(node_names) && "type" %in% node_names) {
    if (!setequal(node_names, c("type", "params")) ||
        length(node_names) != 2L) {
      stop("Population filter leaves may contain only type and params and ",
           "cannot mix group fields.", call. = FALSE)
    }
    if (!is.character(node$type) || length(node$type) != 1L ||
        is.na(node$type) || !tolower(node$type) %in% .cohortFilterTypes()) {
      stop("Unknown population filter type.", call. = FALSE)
    }
    if (!is.list(node$params)) {
      stop("Population filter params must be a named list.", call. = FALSE)
    }
    return(.compileCohortFilterLeaf(handle, node, bp, person_cols,
                                    index_anchor = index_anchor))
  }

  group_keys <- intersect(node_names %||% character(0), c("and", "or"))
  if (length(group_keys) > 0L) {
    if (length(group_keys) != 1L || length(node_names) != 1L) {
      stop("Population filter nodes cannot mix AND/OR groups with each other ",
           "or with leaf fields.", call. = FALSE)
    }
  }

  if (identical(group_keys, "and")) {
    if (!is.list(node$and) || length(node$and) == 0) {
      stop("Population filter AND group must contain at least one filter.",
           call. = FALSE)
    }
    parts <- vapply(node$and, .compileCohortFilterWhere, character(1),
                    handle = handle, bp = bp, person_cols = person_cols,
                    index_anchor = index_anchor, .depth = .depth + 1L,
                    .state = .state)
    if (any(!nzchar(parts))) {
      stop("A population filter in the AND group compiled to no predicate.",
           call. = FALSE)
    }
    return(paste0("(", paste(parts, collapse = " AND "), ")"))
  }

  if (identical(group_keys, "or")) {
    if (!is.list(node$or) || length(node$or) == 0) {
      stop("Population filter OR group must contain at least one filter.",
           call. = FALSE)
    }
    parts <- vapply(node$or, .compileCohortFilterWhere, character(1),
                    handle = handle, bp = bp, person_cols = person_cols,
                    index_anchor = index_anchor, .depth = .depth + 1L,
                    .state = .state)
    if (any(!nzchar(parts))) {
      stop("A population filter in the OR group compiled to no predicate.",
           call. = FALSE)
    }
    return(paste0("(", paste(parts, collapse = " OR "), ")"))
  }

  # Legacy flat arrays are an implicit AND, but must be genuinely unnamed;
  # named unknown fields are never treated as filters and ignored.
  if (is.null(node_names)) {
    parts <- vapply(node, .compileCohortFilterWhere, character(1),
                    handle = handle, bp = bp, person_cols = person_cols,
                    index_anchor = index_anchor, .depth = .depth + 1L,
                    .state = .state)
    if (any(!nzchar(parts))) {
      stop("A population filter compiled to no predicate.", call. = FALSE)
    }
    return(paste0("(", paste(parts, collapse = " AND "), ")"))
  }

  stop("Unknown or malformed population filter specification.", call. = FALSE)
}

#' Coerce a concept_id / concept_ids filter parameter to an integer vector
#'
#' Concept cohort filters accept either a single \code{concept_id} or a
#' \code{concept_ids} set; both cross the JSON transport as scalars or lists.
#' Returns a deduplicated, NA-free integer vector.
#'
#' @param x Scalar, vector, or list of concept IDs
#' @return Integer vector
#' @keywords internal
.conceptIdList <- function(x) {
  if (is.null(x)) return(integer(0))
  raw <- unlist(x, use.names = FALSE)
  if (length(raw) == 0L) return(integer(0))
  max_values <- .extractionCap("dsomop.max_filter_values", 10000L)
  if (length(raw) > max_values) {
    stop("Concept ID list exceeds the server max_filter_values cap of ",
         max_values, ".", call. = FALSE)
  }
  numeric_ids <- suppressWarnings(as.numeric(raw))
  integer_ids <- suppressWarnings(as.integer(raw))
  if (anyNA(numeric_ids) || any(!is.finite(numeric_ids)) || anyNA(integer_ids) ||
      any(numeric_ids != integer_ids) || any(integer_ids < 0L)) {
    stop("Concept ID lists must contain only finite non-negative integers.",
         call. = FALSE)
  }
  unique(integer_ids)
}

#' Reference year for age-based person filters
#'
#' Returns the year that \code{year_of_birth} is differenced against to derive
#' age. When the filter is anchored to a cohort index date, the reference is a
#' dialect-aware SQL expression extracting the year from that per-person index
#' date, so ages are computed AT INDEX and the result is deterministic. With no
#' index anchor there is no reproducible date to use, so the caller must supply
#' an explicit reference date instead of depending on the server wall clock.
#'
#' @param handle CDM handle (for dialect resolution)
#' @param index_anchor Character SQL expression for the per-person index date,
#'   or \code{NULL}
#' @return Character; either an integer-literal year or a SQL year expression
#' @keywords internal
.ageReferenceYear <- function(handle, index_anchor = NULL) {
  if (is.null(index_anchor)) {
    stop("Age filters without a cohort index require an explicit ",
         "reference_date/year for reproducibility.", call. = FALSE)
  }
  .omopYearExpr(handle, index_anchor)
}

.compileCohortFilterLeaf <- function(handle, f, bp, person_cols,
                                     index_anchor = NULL) {
  ftype <- tolower(f$type)
  params <- f$params %||% list()
  fail <- function(message) {
    stop("Population filter '", ftype, "' cannot be compiled: ", message,
         call. = FALSE)
  }
  table_row <- function(table_name, required_columns = character(0)) {
    row <- bp$tables[bp$tables$table_name == table_name &
                       bp$tables$present_in_db, , drop = FALSE]
    if (nrow(row) == 0) fail(paste0("table '", table_name, "' is unavailable."))
    if (!is.null(bp$columns) && !is.null(bp$columns[[table_name]])) {
      available <- bp$columns[[table_name]]$column_name
      missing <- setdiff(required_columns, available)
      if (length(missing) > 0) {
        fail(paste0("table '", table_name, "' is missing column(s): ",
                    paste(missing, collapse = ", "), "."))
      }
    }
    row
  }
  integer_param <- function(value, name, default = NULL, minimum = NULL) {
    if (is.null(value)) value <- default
    numeric_value <- suppressWarnings(as.numeric(value))
    integer_value <- suppressWarnings(as.integer(value))
    if (length(value) != 1L || length(numeric_value) != 1L ||
        !is.finite(numeric_value) || length(integer_value) != 1L ||
        is.na(integer_value) || numeric_value != integer_value ||
        (!is.null(minimum) && integer_value < minimum)) {
      fail(paste0(name, " must be one finite integer",
                  if (!is.null(minimum)) paste0(" >= ", minimum) else "",
                  "."))
    }
    integer_value
  }
  numeric_param <- function(value, name) {
    numeric_value <- suppressWarnings(as.numeric(value))
    if (length(value) != 1L || length(numeric_value) != 1L ||
        !is.finite(numeric_value)) {
      fail(paste0(name, " must be one finite number."))
    }
    numeric_value
  }

  supported_params <- .cohortFilterParamNames(ftype)
  if (is.null(names(params)) && length(params) > 0L) {
    fail("params must be named.")
  }
  unknown_params <- setdiff(names(params) %||% character(0), supported_params)
  if (length(unknown_params) > 0L) {
    fail(paste0("unknown parameter(s): ",
                paste(unknown_params, collapse = ", "), "."))
  }
  if (anyDuplicated(names(params) %||% character(0))) {
    fail("params cannot contain duplicate names.")
  }

  # Pre-execution granularity gate: blocks fingerprinting filters (e.g.
  # age_range narrower than the disclosure minimum) before any SQL runs.
  .validateFilter(ftype, params)

  if (ftype == "sex") {
    if (is.null(params$value) || length(params$value) != 1L) {
      fail("value must be M/MALE or F/FEMALE.")
    }
    gender_id <- switch(toupper(params$value),
      "F" = 8532L, "FEMALE" = 8532L,
      "M" = 8507L, "MALE" = 8507L,
      NULL)
    if (is.null(gender_id)) fail("value must be M/MALE or F/FEMALE.")
    if (!"gender_concept_id" %in% person_cols) {
      fail("person.gender_concept_id is unavailable.")
    }
    return(paste0("p.gender_concept_id = ", gender_id))

  } else if (ftype == "age_range") {
    # Age is computed at the cohort index date when this filter is anchored to an
    # existing cohort (deterministic, and consistent with the rest of the system,
    # which ages year_of_birth relative to the index date). An explicit
    # reference_date (from omop_filter_age(year=)/reference_date=) overrides both,
    # keeping the filter consistent with a year-anchored age variable. With
    # neither, fail rather than silently depending on the server wall clock.
    if (!"year_of_birth" %in% person_cols) {
      fail("person.year_of_birth is unavailable.")
    }
    if (is.null(params$min) && is.null(params$max)) {
      fail("min and/or max is required.")
    }
    ref_year <- if (!is.null(params$reference_date)) {
      ref_date <- tryCatch(
        .isoDate(params$reference_date, "age_range reference_date"),
        error = function(e) fail(conditionMessage(e))
      )
      as.integer(format(ref_date, "%Y"))
    } else {
      .ageReferenceYear(handle, index_anchor)
    }
    parts <- character(0)
    if (!is.null(params$min)) {
      min_age <- integer_param(params$min, "min", minimum = 0L)
      parts <- c(parts, paste0("p.year_of_birth <= (", ref_year, " - ",
                               min_age, ")"))
    }
    if (!is.null(params$max)) {
      max_age <- integer_param(params$max, "max", minimum = 0L)
      parts <- c(parts, paste0("p.year_of_birth >= (", ref_year, " - ",
                               max_age, ")"))
      if (is.null(params$min)) {
        # A max-only filter must not admit future birth years (negative ages).
        parts <- c(parts, paste0("p.year_of_birth <= (", ref_year, ")"))
      }
    }
    if (!is.null(params$min) && !is.null(params$max) && min_age > max_age) {
      fail("min must not be greater than max.")
    }
    return(paste0("(", paste(parts, collapse = " AND "), ")"))

  } else if (ftype == "age_group") {
    if (!"year_of_birth" %in% person_cols) {
      fail("person.year_of_birth is unavailable.")
    }
    ref_year <- if (!is.null(params$reference_date)) {
      ref_date <- tryCatch(
        .isoDate(params$reference_date, "age_group reference_date"),
        error = function(e) fail(conditionMessage(e))
      )
      as.integer(format(ref_date, "%Y"))
    } else {
      tryCatch(.ageReferenceYear(handle, index_anchor),
               error = function(e) fail(conditionMessage(e)))
    }
    groups <- unlist(params$groups, use.names = FALSE)
    if (length(groups) == 0 || anyNA(groups)) {
      fail("at least one non-missing age band is required.")
    }
    band_parts <- character(0)
    for (g in groups) {
      g <- trimws(as.character(g))
      if (grepl("^[0-9]+\\+$", g)) {
        lower <- as.integer(sub("\\+$", "", g))
        band_parts <- c(band_parts, paste0(
          "(p.year_of_birth <= (", ref_year, " - ", lower, "))"))
      } else if (grepl("^[0-9]+-[0-9]+$", g)) {
        bounds <- as.integer(strsplit(g, "-", fixed = TRUE)[[1]])
        if (bounds[1] > bounds[2]) fail(paste0("invalid age band '", g, "'."))
        band_parts <- c(band_parts,
          paste0("(p.year_of_birth BETWEEN (", ref_year, " - ",
                 bounds[2], ") AND (", ref_year, " - ", bounds[1], "))"))
      } else {
        fail(paste0("invalid age band '", g, "'."))
      }
    }
    return(paste0("(", paste(band_parts, collapse = " OR "), ")"))

  } else if (ftype == "cohort") {
    cid <- integer_param(params$cohort_definition_id,
                         "cohort_definition_id", minimum = 0L)
    results_schema <- .effectiveResultsSchema(handle)
    qualified <- .qualifyTable(handle, "cohort", results_schema)
    return(paste0(
      "EXISTS (SELECT 1 FROM ", qualified, " c",
      " WHERE c.subject_id = p.person_id",
      " AND c.cohort_definition_id = ", cid, ")"
    ))

  } else if (ftype == "has_concept") {
    concept_ids <- .conceptIdList(params$concept_ids %||% params$concept_id)
    min_count <- integer_param(params$min_count, "min_count", default = 1L,
                               minimum = 1L)
    table_name <- tolower(params$table %||% "")
    if (!nzchar(table_name)) fail("table is required.")
    if (length(concept_ids) == 0) fail("concept_id(s) are required.")
    concept_col <- .getDomainConceptColumn(bp, table_name)
    if (is.null(concept_col)) fail(paste0("table '", table_name,
                                          "' has no domain concept column."))
    tbl_row <- table_row(table_name, c("person_id", concept_col))
    qualified_tbl <- tbl_row$qualified_name[1]
    concept_predicate <- .sqlIdInPredicate(
      paste0("t.", concept_col), concept_ids
    )
    win <- .windowPredicateSql(handle, bp, table_name, "t", params$window,
                               anchor_sql = index_anchor,
                               reference_date = params$reference_date)
    if (min_count <= 1L) {
      return(paste0("EXISTS (SELECT 1 FROM ", qualified_tbl, " t",
                    " WHERE t.person_id = p.person_id AND ",
                    concept_predicate, win, ")"))
    }
    return(paste0("(SELECT COUNT(*) FROM ", qualified_tbl, " t",
                  " WHERE t.person_id = p.person_id AND ",
                  concept_predicate, win,
                  ") >= ", min_count))

  } else if (ftype == "not_has_concept") {
    concept_ids <- .conceptIdList(params$concept_ids %||% params$concept_id)
    table_name <- tolower(params$table %||% "")
    if (!nzchar(table_name)) fail("table is required.")
    if (length(concept_ids) == 0) fail("concept_id(s) are required.")
    concept_col <- .getDomainConceptColumn(bp, table_name)
    if (is.null(concept_col)) fail(paste0("table '", table_name,
                                          "' has no domain concept column."))
    tbl_row <- table_row(table_name, c("person_id", concept_col))
    qualified_tbl <- tbl_row$qualified_name[1]
    concept_predicate <- .sqlIdInPredicate(
      paste0("t.", concept_col), concept_ids
    )
    win <- .windowPredicateSql(handle, bp, table_name, "t", params$window,
                               anchor_sql = index_anchor,
                               reference_date = params$reference_date)
    return(paste0("NOT EXISTS (SELECT 1 FROM ", qualified_tbl, " t",
                  " WHERE t.person_id = p.person_id AND ",
                  concept_predicate, win, ")"))

  } else if (ftype == "concept_count") {
    concept_ids <- .conceptIdList(params$concept_ids %||% params$concept_id)
    min_count <- integer_param(params$min_count, "min_count", default = 1L,
                               minimum = 1L)
    table_name <- tolower(params$table %||% "")
    if (!nzchar(table_name)) fail("table is required.")
    if (length(concept_ids) == 0) fail("concept_id(s) are required.")
    concept_col <- .getDomainConceptColumn(bp, table_name)
    if (is.null(concept_col)) fail(paste0("table '", table_name,
                                          "' has no domain concept column."))
    tbl_row <- table_row(table_name, c("person_id", concept_col))
    qualified_tbl <- tbl_row$qualified_name[1]
    concept_predicate <- .sqlIdInPredicate(
      paste0("t.", concept_col), concept_ids
    )
    win <- .windowPredicateSql(handle, bp, table_name, "t", params$window,
                               anchor_sql = index_anchor,
                               reference_date = params$reference_date)
    return(paste0("(SELECT COUNT(*) FROM ", qualified_tbl, " t",
                  " WHERE t.person_id = p.person_id AND ",
                  concept_predicate, win,
                  ") >= ", min_count))

  } else if (ftype == "prior_observation") {
    min_days <- integer_param(params$min_days, "min_days", default = 365L,
                              minimum = 0L)
    op_row <- table_row("observation_period",
      c("person_id", "observation_period_start_date"))
    op_qualified <- op_row$qualified_name[1]
    anchor <- if (!is.null(params$reference_date)) {
      fixed_date <- .isoDate(params$reference_date,
                             "prior_observation reference_date")
      .quoteLiteral(format(fixed_date, "%Y-%m-%d"))
    } else if (!is.null(index_anchor)) {
      index_anchor
    } else {
      fail("requires a cohort index or an explicit reference_date.")
    }
    cutoff <- .dateAddSql(handle, -min_days, anchor)
    return(paste0("EXISTS (SELECT 1 FROM ", op_qualified, " op",
                  " WHERE op.person_id = p.person_id",
                  " AND op.observation_period_start_date <= ", cutoff,
                  " AND op.observation_period_end_date >= ", anchor, ")"))

  } else if (ftype == "followup") {
    min_days <- integer_param(params$min_days, "min_days", default = 30L,
                              minimum = 0L)
    op_row <- table_row("observation_period",
      c("person_id", "observation_period_end_date"))
    op_qualified <- op_row$qualified_name[1]
    anchor <- if (!is.null(params$reference_date)) {
      fixed_date <- .isoDate(params$reference_date,
                             "followup reference_date")
      .quoteLiteral(format(fixed_date, "%Y-%m-%d"))
    } else if (!is.null(index_anchor)) {
      index_anchor
    } else {
      fail("requires a cohort index or an explicit reference_date.")
    }
    cutoff <- .dateAddSql(handle, min_days, anchor)
    return(paste0("EXISTS (SELECT 1 FROM ", op_qualified, " op",
                  " WHERE op.person_id = p.person_id",
                  " AND op.observation_period_start_date <= ", anchor,
                  " AND op.observation_period_end_date >= ", cutoff, ")"))

  } else if (ftype == "visit_count") {
    min_count <- integer_param(params$min_count, "min_count", default = 1L,
                               minimum = 1L)
    visit_ids <- .conceptIdList(params$visit_concept_ids %||%
                                  params$visit_concept_id)
    required <- c("person_id", if (length(visit_ids) > 0) "visit_concept_id")
    vo_row <- table_row("visit_occurrence", required)
    vo_qualified <- vo_row$qualified_name[1]
    sub_where <- paste0(" WHERE v.person_id = p.person_id")
    if (length(visit_ids) > 0) {
      sub_where <- paste0(sub_where, " AND ", .sqlIdInPredicate(
        "v.visit_concept_id", visit_ids
      ))
    }
    win <- .windowPredicateSql(handle, bp, "visit_occurrence", "v",
                               params$window, anchor_sql = index_anchor,
                               reference_date = params$reference_date)
    return(paste0("(SELECT COUNT(*) FROM ", vo_qualified, " v",
                  sub_where, win, ") >= ", min_count))

  } else if (ftype == "has_measurement") {
    concept_ids <- .conceptIdList(params$concept_ids %||% params$concept_id)
    if (length(concept_ids) == 0) fail("concept_id(s) are required.")
    required <- c("person_id", "measurement_concept_id",
      if (!is.null(params$min_value) || !is.null(params$max_value))
        "value_as_number")
    m_row <- table_row("measurement", required)
    m_qualified <- m_row$qualified_name[1]
    sub_where <- paste0(
      " WHERE m.person_id = p.person_id",
      " AND ", .sqlIdInPredicate(
        "m.measurement_concept_id", concept_ids
      ))
    has_numeric_range <- !is.null(params$min_value) ||
      !is.null(params$max_value)
    if (has_numeric_range) {
      if (is.null(params$min_value) || is.null(params$max_value)) {
        fail(paste0(
          "numeric measurement filters require both min_value and max_value ",
          "from one server-issued safe bin."
        ))
      }
      if (length(concept_ids) != 1L) {
        fail(paste0(
          "numeric measurement filters require exactly one concept_id so ",
          "the safe-bin scope is unambiguous."
        ))
      }
      min_value <- numeric_param(params$min_value, "min_value")
      max_value <- numeric_param(params$max_value, "max_value")
      if (min_value >= max_value) {
        fail("min_value must be strictly less than max_value.")
      }
      .assertSafeNumericBinContract(
        handle, table = "measurement", column = "value_as_number",
        value = list(lower = min_value, upper = max_value),
        scope = params$safe_scope
      )
      scope_concept <- suppressWarnings(as.integer(
        params$safe_scope$concept_id %||% integer(0)
      ))
      scope_concept_col <- tolower(
        params$safe_scope$concept_col %||% "measurement_concept_id"
      )
      if (length(scope_concept) != 1L || is.na(scope_concept) ||
          scope_concept != concept_ids[[1]] ||
          !identical(scope_concept_col, "measurement_concept_id")) {
        fail("safe-bin scope must match the measurement concept_id.")
      }
      sub_where <- paste0(
        sub_where,
        " AND m.value_as_number >= ", min_value,
        " AND m.value_as_number < ", max_value
      )
    }
    win <- .windowPredicateSql(handle, bp, "measurement", "m", params$window,
                               anchor_sql = index_anchor,
                               reference_date = params$reference_date)
    return(paste0("EXISTS (SELECT 1 FROM ", m_qualified, " m",
                  sub_where, win, ")"))

  } else if (ftype == "missing_measurement") {
    concept_ids <- .conceptIdList(params$concept_ids %||% params$concept_id)
    if (length(concept_ids) == 0) fail("concept_id(s) are required.")
    m_row <- table_row("measurement",
      c("person_id", "measurement_concept_id", "value_as_number"))
    m_qualified <- m_row$qualified_name[1]
    win <- .windowPredicateSql(handle, bp, "measurement", "m", params$window,
                               anchor_sql = index_anchor,
                               reference_date = params$reference_date)
    return(paste0("NOT EXISTS (SELECT 1 FROM ", m_qualified, " m",
                  " WHERE m.person_id = p.person_id AND ",
                  .sqlIdInPredicate(
                    "m.measurement_concept_id", concept_ids
                  ),
                  " AND m.value_as_number IS NOT NULL", win, ")"))
  }

  fail("unsupported filter type.")
}

#' Generate a unique staging token
#'
#' @return Character; staging token
#' @keywords internal
.generateStagingToken <- function() {
  paste0("stg_", paste0(format(openssl::rand_bytes(16L)), collapse = ""))
}

#' Test whether a path is a symbolic link
#'
#' `Sys.readlink()` returns `NA` for a non-existent path on some platforms;
#' `nzchar(NA)` is `TRUE`, so callers must handle that case explicitly.
#'
#' @param path Character path.
#' @return Logical scalar.
#' @keywords internal
.isSymbolicLink <- function(path) {
  target <- Sys.readlink(path)
  length(target) == 1L && !is.na(target) && nzchar(target)
}

#' Get the staging base directory
#'
#' @return Character; path to staging base directory
#' @keywords internal
.stagingBaseDir <- function() {
  base <- getOption("dsstaging.base_dir", file.path(tempdir(), "dsstaging"))
  if (!is.character(base) || length(base) != 1L || is.na(base) ||
      !nzchar(base)) {
    stop("dsstaging.base_dir must be one non-empty server path.",
         call. = FALSE)
  }
  if (dir.exists(base) && .isSymbolicLink(base)) {
    stop("dsstaging.base_dir must not be a symbolic link.", call. = FALSE)
  }
  created <- FALSE
  if (!dir.exists(base)) {
    old_umask <- Sys.umask("0077")
    on.exit(Sys.umask(old_umask), add = TRUE)
    if (!dir.create(base, recursive = TRUE, showWarnings = FALSE,
                    mode = "0700")) {
      stop("Could not create the staging base directory.", call. = FALSE)
    }
    created <- TRUE
  }
  if (.Platform$OS.type != "windows") {
    info <- file.info(base)
    expected_mode <- as.integer(strtoi("700", base = 8L))
    if (nrow(info) != 1L || is.na(info$isdir[[1L]]) ||
        !isTRUE(info$isdir[[1L]]) || is.na(info$uid[[1L]]) ||
        is.na(info$mode[[1L]]) ||
        !identical(as.integer(info$uid[[1L]]), .dsomopEffectiveUid()) ||
        !identical(as.integer(info$mode[[1L]]), expected_mode)) {
      stop("dsstaging.base_dir must be an owner-only directory (0700) owned ",
           "by the server R user.",
           call. = FALSE)
    }
  } else if (created) {
    Sys.chmod(base, mode = "0700")
  }
  normalizePath(base, winslash = "/", mustWork = TRUE)
}

#' Create a staging directory for a token
#'
#' @param token Character; staging token
#' @return Character; path to the staging directory
#' @keywords internal
.createStagingDir <- function(token) {
  if (!is.character(token) || length(token) != 1L || is.na(token) ||
      !grepl("^stg_[0-9a-f]{32}$", token)) {
    stop("Invalid staging token.", call. = FALSE)
  }
  staging_dir <- file.path(.stagingBaseDir(), token)
  old_umask <- Sys.umask("0077")
  on.exit(Sys.umask(old_umask), add = TRUE)
  if (file.exists(staging_dir) || dir.exists(staging_dir) ||
      !dir.create(staging_dir, recursive = FALSE, showWarnings = FALSE,
                  mode = "0700")) {
    stop("Could not create an exclusive staging directory.", call. = FALSE)
  }
  Sys.chmod(staging_dir, mode = "0700")
  staging_dir
}

.stagingDirectoryBytes <- function(staging_dir) {
  if (!dir.exists(staging_dir) || .isSymbolicLink(staging_dir)) return(0)
  paths <- list.files(staging_dir, full.names = TRUE, recursive = TRUE,
                      all.files = TRUE, no.. = TRUE)
  if (length(paths) == 0L) return(0)
  info <- file.info(paths)
  keep <- !is.na(info$size) & (is.na(info$isdir) | !info$isdir)
  sum(info$size[keep], na.rm = TRUE)
}

# Isolate the filesystem mutation so cleanup failure semantics can be exercised
# without weakening staging path/ownership validation.
.unlinkStagingDirectory <- function(path) {
  unlink(path, recursive = TRUE)
}

# Classify one tracked staging path without following or deleting it. Only a
# canonical token directory directly below the configured owner-only base can be
# live or idempotently missing; every other state is unsafe and remains tracked.
.inspectOwnedStagingPath <- function(path, base) {
  token <- if (is.character(path) && length(path) == 1L && !is.na(path)) {
    basename(path)
  } else {
    ""
  }
  canonical_path <- if (nzchar(token)) gsub("\\\\", "/", path) else ""
  expected <- if (nzchar(token)) {
    gsub("\\\\", "/", file.path(base, token))
  } else {
    ""
  }
  valid_location <- nzchar(expected) && identical(canonical_path, expected) &&
    grepl("^stg_[0-9a-f]{32}$", token)
  if (!valid_location || .isSymbolicLink(canonical_path)) {
    return(list(state = "unsafe", path = canonical_path))
  }
  if (!file.exists(canonical_path) && !dir.exists(canonical_path)) {
    return(list(state = "missing", path = canonical_path))
  }
  if (!dir.exists(canonical_path)) {
    return(list(state = "unsafe", path = canonical_path))
  }
  if (.Platform$OS.type != "windows") {
    info <- file.info(canonical_path)
    expected_mode <- as.integer(strtoi("700", base = 8L))
    valid_directory <- nrow(info) == 1L && isTRUE(info$isdir[[1L]]) &&
      !is.na(info$uid[[1L]]) && !is.na(info$mode[[1L]]) &&
      identical(as.integer(info$uid[[1L]]), .dsomopEffectiveUid()) &&
      identical(as.integer(info$mode[[1L]]), expected_mode)
    if (!valid_directory) {
      return(list(state = "unsafe", path = canonical_path))
    }
  }
  list(state = "directory", path = canonical_path)
}

#' Remove staging directories owned by a handle
#'
#' @param handle CDM handle.
#' @param paths Optional tracked paths to remove. \code{NULL} removes every path
#'   owned by the handle.
#' @return NULL, invisibly.
#' @keywords internal
.cleanupHandleStaging <- function(handle, paths = NULL) {
  tracked <- unique(handle$staging_dirs %||% character(0))
  if (length(tracked) == 0L) return(invisible(NULL))
  dirs <- if (is.null(paths)) {
    tracked
  } else {
    if (!is.character(paths) || anyNA(paths)) {
      stop("Tracked staging cleanup paths must be character values.",
           call. = FALSE)
    }
    intersect(unique(paths), tracked)
  }
  if (length(dirs) == 0L) return(invisible(NULL))

  base <- normalizePath(.stagingBaseDir(), winslash = "/", mustWork = TRUE)
  remaining <- tracked
  failures <- character(0)
  for (path in dirs) {
    inspection <- .inspectOwnedStagingPath(path, base)
    canonical_path <- inspection$path

    # A previously removed, valid owned path is an idempotent success.
    if (identical(inspection$state, "missing")) {
      remaining <- setdiff(remaining, path)
      next
    }
    if (!identical(inspection$state, "directory")) {
      failures <- c(failures, paste0(path, ": unsafe or invalid owned path"))
      next
    }

    status <- suppressWarnings(.unlinkStagingDirectory(canonical_path))
    removed <- identical(as.integer(status), 0L) &&
      !file.exists(canonical_path) && !dir.exists(canonical_path) &&
      !.isSymbolicLink(canonical_path)
    if (removed) {
      remaining <- setdiff(remaining, path)
    } else {
      failures <- c(failures, paste0(path, ": deletion was not confirmed"))
    }
  }
  handle$staging_dirs <- remaining
  if (length(failures) > 0L) {
    stop("Could not clean every owned staging directory: ",
         paste(unique(failures), collapse = "; "), call. = FALSE)
  }
  invisible(NULL)
}

#' Build a server-local staged dataset descriptor
#'
#' @param output_name Character; name of the output
#' @param file_info Named list from .executeQueryToParquet
#' @param token Character; staging token
#' @param origin Character; origin package identifier
#' @param pseudonymization Public, non-secret person-key contract.
#' @param semantic_contract Canonical staged output semantic contract.
#' @param bundle_contract Canonical output-level staged bundle contract.
#' @return Named list inheriting from \code{FlowerDatasetDescriptor} and
#'   \code{OMOPStagedDatasetDescriptor}.
#' @keywords internal
.buildStagedDescriptor <- function(output_name, file_info, token,
                                    origin = "dsOMOP",
                                    pseudonymization = NULL,
                                    semantic_contract = NULL,
                                    bundle_contract = NULL) {
  ttl_hours <- suppressWarnings(as.numeric(getOption("dsstaging.ttl_hours", 24)))
  if (length(ttl_hours) != 1L || is.na(ttl_hours) || !is.finite(ttl_hours) ||
      ttl_hours <= 0) {
    stop("dsstaging.ttl_hours must be one positive finite number.",
         call. = FALSE)
  }
  person_bearing <- any(
    tolower(file_info$columns %||% character(0)) %in% .PERSON_KEY_COLS()
  )
  if (person_bearing && is.null(pseudonymization)) {
    stop("Person-bearing staged outputs require a pseudonymization contract.",
         call. = FALSE)
  }
  if (!is.null(pseudonymization)) {
    pseudonymization <- .canonicalPseudonymizationContract(pseudonymization)
  }
  if (person_bearing && !isTRUE(pseudonymization$resource_scoped)) {
    stop("Person-bearing staged outputs require a resource-scoped ",
         "pseudonymization provider; legacy global keys are not permitted.",
         call. = FALSE)
  }
  if (is.null(semantic_contract)) {
    stop("Staged descriptor v2 requires a semantic contract.", call. = FALSE)
  }
  semantic_contract <- .validateStagedSemanticContract(semantic_contract)
  if (is.null(bundle_contract)) {
    stop("Staged descriptor v2 requires a bundle contract.", call. = FALSE)
  }
  dataset_id <- paste0("omop.plan.", output_name)
  bundle_contract <- .validateStagedBundleContract(
    bundle_contract,
    dataset_id = dataset_id,
    staged_token = token,
    semantic_contract = semantic_contract
  )
  desc <- list(
    dataset_id  = dataset_id,
    source_kind = paste0("staged_", file_info$format),
    contract_version = 2L,
    metadata    = list(
      file    = file_info$file,
      format  = file_info$format,
      layout  = file_info$layout %||% "file",
      parts   = file_info$parts %||% NULL,
      n_rows  = .bandCount(
        file_info$n_rows,
        band_width = .omopDisclosureSettings()$nfilter_band
      ),
      row_count_policy = "banded_lower_bound",
      columns = file_info$columns,
      column_types = file_info$column_types %||% NULL,
      pseudonymization = pseudonymization,
      semantic_contract = semantic_contract,
      bundle_contract = bundle_contract
    ),
    staged_token = token,
    expires_at = format(Sys.time() + ttl_hours * 3600,
                        "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    origin       = origin
  )
  # Keep dsFlower's established class first for compatibility while publishing
  # a package-neutral class for any reviewed server-side consumer.
  class(desc) <- c("FlowerDatasetDescriptor", "OMOPStagedDatasetDescriptor")
  desc
}

#' Write a staging manifest
#'
#' @param staging_dir Character; path to staging directory
#' @param descriptors Named list of descriptors
#' @keywords internal
.writeStagingManifest <- function(staging_dir, descriptors) {
  if (!is.list(descriptors) || length(descriptors) < 1L ||
      is.null(names(descriptors)) || any(!nzchar(names(descriptors))) ||
      anyDuplicated(names(descriptors))) {
    stop("A staging manifest requires uniquely named descriptors.",
         call. = FALSE)
  }
  descriptors <- lapply(descriptors, function(d) {
    invisible(omopStagedDatasetPath(d))
    list(
      dataset_id = d$dataset_id,
      source_kind = d$source_kind,
      contract_version = d$contract_version,
      metadata = within(d$metadata, {
        if (!is.null(column_types)) column_types <- as.list(column_types)
      }),
      staged_token = d$staged_token,
      expires_at = d$expires_at,
      origin = d$origin
    )
  })
  manifest <- list(
    contract_version = 2L,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    outputs = descriptors
  )
  manifest_path <- file.path(staging_dir, "manifest.json")
  old_umask <- Sys.umask("0077")
  on.exit(Sys.umask(old_umask), add = TRUE)
  writeLines(jsonlite::toJSON(
    manifest, auto_unbox = TRUE, pretty = TRUE, null = "null"
  ),
             manifest_path)
  Sys.chmod(manifest_path, mode = "0600")
  max_bytes <- suppressWarnings(as.numeric(
    getOption("dsomop.max_staged_bytes", 10 * 1024^3)
  ))
  if (length(max_bytes) != 1L || is.na(max_bytes) || !is.finite(max_bytes) ||
      max_bytes < 1 || .stagingDirectoryBytes(staging_dir) > max_bytes) {
    unlink(manifest_path)
    stop("Staged outputs exceed or have an invalid server disk quota.",
         call. = FALSE)
  }
  invisible(manifest_path)
}

#' Read and validate a v2 staging manifest
#'
#' Reconstructs descriptor classes and canonical vector fields after JSON
#' parsing, then runs the same fail-closed resolver used by downstream server
#' packages. This is the supported manifest round-trip; consumers must not
#' reconstruct descriptors from a partial JSON projection.
#'
#' @param manifest_path Absolute path returned by \code{.writeStagingManifest}.
#' @return A manifest list whose \code{outputs} are validated staged descriptors.
#' @keywords internal
.readStagingManifest <- function(manifest_path) {
  manifest_path <- .stagedScalarString(manifest_path, "manifest path")
  if (!.stagedIsAbsolutePath(manifest_path) ||
      !file.exists(manifest_path) || .isSymbolicLink(manifest_path) ||
      !utils::file_test("-f", manifest_path)) {
    stop("The staging manifest is unavailable or unsafe.", call. = FALSE)
  }
  info <- file.info(manifest_path)
  if (nrow(info) != 1L || is.na(info$size[[1L]]) || info$size[[1L]] < 1L ||
      info$size[[1L]] > 10 * 1024^2) {
    stop("The staging manifest has an invalid size.", call. = FALSE)
  }
  if (.Platform$OS.type == "unix") {
    expected_mode <- as.integer(strtoi("600", base = 8L))
    if (is.na(info$mode[[1L]]) || is.na(info$uid[[1L]]) ||
        !identical(as.integer(info$mode[[1L]]), expected_mode) ||
        !identical(as.integer(info$uid[[1L]]), .dsomopEffectiveUid()) ||
        !identical(.dsomopLinkCount(manifest_path), 1)) {
      stop("The staging manifest must be an owner-only file without hard links.",
           call. = FALSE)
    }
  }
  manifest <- tryCatch(
    jsonlite::fromJSON(manifest_path, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (!is.list(manifest) || !identical(manifest$contract_version, 2L) ||
      !is.character(manifest$created_at) || length(manifest$created_at) != 1L ||
      is.na(manifest$created_at) || !is.list(manifest$outputs) ||
      length(manifest$outputs) < 1L || is.null(names(manifest$outputs)) ||
      any(!nzchar(names(manifest$outputs))) ||
      anyDuplicated(names(manifest$outputs))) {
    stop("Invalid staging manifest v2.", call. = FALSE)
  }
  created <- suppressWarnings(as.POSIXct(
    manifest$created_at, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
  ))
  if (length(created) != 1L || is.na(created)) {
    stop("Invalid staging manifest creation timestamp.", call. = FALSE)
  }

  outputs <- lapply(manifest$outputs, function(entry) {
    if (!is.list(entry) || !is.list(entry$metadata)) {
      stop("Invalid staged descriptor in manifest.", call. = FALSE)
    }
    entry$metadata$columns <- unname(as.character(
      unlist(entry$metadata$columns, use.names = FALSE)
    ))
    if (!is.null(entry$metadata$column_types)) {
      entry$metadata$column_types <- unlist(
        entry$metadata$column_types, use.names = TRUE
      )
    }
    if (!is.null(entry$metadata$parts)) {
      entry$metadata$parts <- unname(as.character(unlist(
        entry$metadata$parts, use.names = FALSE
      )))
    }
    if (!is.null(entry$metadata$semantic_contract$age_breaks)) {
      entry$metadata$semantic_contract$age_breaks <- unname(as.integer(
        unlist(entry$metadata$semantic_contract$age_breaks, use.names = FALSE)
      ))
    }
    if (!is.null(entry$metadata$bundle_contract$semantic_contract$age_breaks)) {
      entry$metadata$bundle_contract$semantic_contract$age_breaks <-
        unname(as.integer(unlist(
          entry$metadata$bundle_contract$semantic_contract$age_breaks,
          use.names = FALSE
        )))
    }
    class(entry) <- c("FlowerDatasetDescriptor",
                      "OMOPStagedDatasetDescriptor")
    invisible(omopStagedDatasetPath(entry))
    entry
  })
  list(
    contract_version = 2L,
    created_at = manifest$created_at,
    outputs = outputs
  )
}

#' Stage a data.frame result to Parquet and return a descriptor
#'
#' For output types where SQL cannot be streamed directly (for example
#' baseline, person-level, wide, or feature outputs), this writes an
#' already-materialized data.frame to Parquet.
#'
#' @param df Data frame to stage
#' @param output_name Character; output name
#' @param staging_dir Character; path to staging directory
#' @param token Character; staging token
#' @param key Raw vector; per-resource secret used to pseudonymize person keys
#' @param pseudonymization Public, non-secret person-key contract. Required for
#'   every person-bearing staged output.
#' @param semantic_contract Canonical staged output semantic contract.
#' @param bundle_contract Canonical output-level staged bundle contract.
#' @return Staged dataset descriptor (Parquet, or CSV when Arrow is unavailable)
#' @keywords internal
.stageDataFrame <- function(df, output_name, staging_dir, token, key,
                            pseudonymization = NULL,
                            semantic_contract = NULL,
                            bundle_contract = NULL) {
  output_name <- .validateIdentifier(output_name, "staged output")
  if (is.null(semantic_contract)) {
    stop("Staged descriptor v2 requires a semantic contract.", call. = FALSE)
  }
  semantic_contract <- .validateStagedSemanticContract(semantic_contract)
  if (is.null(bundle_contract)) {
    stop("Staged descriptor v2 requires a bundle contract.", call. = FALSE)
  }
  bundle_contract <- .validateStagedBundleContract(
    bundle_contract,
    dataset_id = paste0("omop.plan.", output_name),
    staged_token = token,
    semantic_contract = semantic_contract
  )
  if (!is.character(token) || length(token) != 1L || is.na(token) ||
      !grepl("^stg_[0-9a-f]{32}$", token)) {
    stop("Invalid staging token.", call. = FALSE)
  }
  expected_dir <- normalizePath(file.path(.stagingBaseDir(), token),
                                winslash = "/", mustWork = FALSE)
  actual_dir <- normalizePath(staging_dir, winslash = "/", mustWork = FALSE)
  if (.isSymbolicLink(staging_dir) ||
      !identical(actual_dir, expected_dir) || !dir.exists(actual_dir)) {
    stop("Staging directory is unavailable or does not match its token.",
         call. = FALSE)
  }
  if (is.null(key) || length(key) == 0L) {
    stop("Staged outputs require a per-resource person key.", call. = FALSE)
  }
  if (any(tolower(names(df)) %in% .PERSON_KEY_COLS()) &&
      is.null(pseudonymization)) {
    stop("Person-bearing staged outputs require an explicit resource-scoped ",
         "pseudonymization contract.", call. = FALSE)
  }
  # The file itself is a server-side DataSHIELD object. Sanitize before the
  # first byte is written: person/subject keys become authenticated resource-
  # scoped tokens and every other OMOP row/entity identifier is removed.
  df <- .pseudonymizeIdentifiers(df, key, pseudonymization)
  max_rows <- suppressWarnings(as.numeric(
    getOption("dsomop.max_staged_rows", 50000000L)
  ))
  max_bytes <- suppressWarnings(as.numeric(
    getOption("dsomop.max_staged_bytes", 10 * 1024^3)
  ))
  if (length(max_rows) != 1L || is.na(max_rows) || !is.finite(max_rows) ||
      max_rows != floor(max_rows) || max_rows < 1L || nrow(df) > max_rows ||
      length(max_bytes) != 1L || is.na(max_bytes) || !is.finite(max_bytes) ||
      max_bytes < 1) {
    stop("Staged output exceeds or has invalid server row/byte quotas.",
         call. = FALSE)
  }
  existing_bytes <- .stagingDirectoryBytes(staging_dir)
  if (existing_bytes >= max_bytes) {
    stop("Staged output exceeds the server disk quota.", call. = FALSE)
  }

  use_parquet <- .arrowAvailable()
  ext <- if (use_parquet) "parquet" else "csv"
  output_path <- file.path(staging_dir, paste0(output_name, ".", ext))
  if (file.exists(output_path) || .isSymbolicLink(output_path)) {
    stop("Staged output path already exists.", call. = FALSE)
  }

  if (use_parquet) {
    old_umask <- Sys.umask("0077")
    on.exit(Sys.umask(old_umask), add = TRUE)
    arrow::write_parquet(df, output_path)
  } else {
    old_umask <- Sys.umask("0077")
    on.exit(Sys.umask(old_umask), add = TRUE)
    utils::write.csv(df, output_path, row.names = FALSE)
  }
  if (existing_bytes + file.info(output_path)$size > max_bytes) {
    unlink(output_path)
    stop("Staged output exceeds the server disk quota.", call. = FALSE)
  }
  Sys.chmod(output_path, mode = "0600")

  file_info <- list(
    file    = output_path,
    format  = ext,
    layout  = "file",
    parts   = NULL,
    n_rows  = nrow(df),
    columns = names(df),
    column_types = vapply(df, function(col) {
      paste(typeof(col), paste(class(col), collapse = "/"), sep = "|")
    }, character(1))
  )
  .buildStagedDescriptor(output_name, file_info, token,
                         pseudonymization = pseudonymization,
                         semantic_contract = semantic_contract,
                         bundle_contract = bundle_contract)
}

#' Does this plan declare more than the implicit unrestricted base population?
#'
#' The multi-population execution path is engaged only when there is real work
#' beyond the single base cohort: a non-base population, or a base population that
#' itself carries criteria filters or a set-op. A plan with just
#' \code{populations = list(base = list(id = "base"))} (or none) stays on the
#' unchanged single-cohort fast path.
#'
#' @param plan The extraction plan
#' @return Logical
#' @keywords internal
.planHasMultiPopulation <- function(plan) {
  pops <- plan$populations
  if (is.null(pops) || length(pops) == 0) return(FALSE)
  non_base <- setdiff(names(pops), "base")
  if (length(non_base) > 0) return(TRUE)
  base <- pops[["base"]]
  (!is.null(base$filters) && length(base$filters) > 0) ||
    !is.null(base$filter_tree) || !is.null(base$index_event) ||
    !is.null(base$setop)
}

#' Resolve the recipe-level scope into ONE folded, re-gated cohort temp table
#'
#' The scope spec on the plan (\code{plan$scope = list(cohort, combine)}) names a
#' cohort reference (cohort_definition_id / cohort temp-table name) and the fold
#' operator. \code{omop.table} SYMBOL sources cannot ride in the plan JSON, so
#' they are injected as resolved frames under \code{plan$scope$tables_frames} by
#' \code{\link{omopPlanExecuteDS}} before execution.
#'
#' A plan-level \code{plan$cohort} carrying a scalar \code{cohort_definition_id}
#' (the \code{type == "cohort_table"} form produced by
#' \code{\link[dsOMOPClient]{ds.omop.plan.cohort}} and by a base
#' \code{omop_population(cohort_definition_id = ...)}) is treated UNIFORMLY as a
#' scope source too: there is no "scalar id = base population" path. It is folded
#' in alongside any \code{plan$scope} sources so it ends up INTERSECTED into every
#' population exactly like any other scope.
#'
#' All sources are folded into a single re-gated cohort by
#' \code{\link{.omopAnalysisResolveScope}} (the same resolver the analysis catalog
#' uses; each ref is materialized + size-checked via \code{\link{.resolveCohortArg}}
#' -> \code{\link{.resolveCohortTable}}). Returns NULL when there is no scope.
#'
#' @param handle CDM handle
#' @param plan The extraction plan
#' @return Character cohort temp table name, or NULL
#' @keywords internal
.planResolveScopeCohort <- function(handle, plan) {
  scope <- plan$scope
  combine <- tolower((scope$combine %||% "union"))

  # A plan-level scalar cohort_definition_id is a scope source, not a base
  # population. Collect it so it folds in with any plan$scope sources below.
  cohort_id_src <- NULL
  if (!is.null(plan$cohort) &&
      identical(plan$cohort$type, "cohort_table") &&
      !is.null(plan$cohort$cohort_definition_id)) {
    cohort_id_src <- as.integer(plan$cohort$cohort_definition_id)
  }

  # Assemble a FLAT list of scope sources so the plan-level cohort id folds in
  # next to any plan$scope sources. The scope half may be spliced live sources
  # (a cohort literal mixed with omop.table frames, as the client's
  # .analysis_scope_expr builds them) or a JSON-only cohort ref (a saved/loaded
  # recipe scoped only to a cohort). Either is normalised to its constituent
  # sources with the SAME rule .omopAnalysisResolveScope uses: a data.frame /
  # omop.table / non-list is one source; a plain list is already a source list.
  scope_part <- if (!is.null(scope)) {
    scope$tables_frames %||% scope$cohort
  } else {
    NULL
  }
  sources <- list()
  if (!is.null(scope_part)) {
    if (is.list(scope_part) && !is.data.frame(scope_part) &&
        !.is_omop.table(scope_part)) {
      sources <- c(sources, scope_part)
    } else {
      sources <- c(sources, list(scope_part))
    }
  }
  if (!is.null(cohort_id_src)) sources <- c(sources, list(cohort_id_src))

  sources <- Filter(Negate(is.null), sources)
  if (length(sources) == 0) return(NULL)
  server_max <- as.integer(
    .omopDisclosureSettings()$max_analysis_scope_tables
  )
  .omopAnalysisScopeSourceCount(
    sources, max_sources = server_max + 1L
  )
  scope_table_count <- .omopAnalysisScopeTableCount(sources)
  if (scope_table_count > server_max) {
    stop("Plan scope exceeds the server max_analysis_scope_tables cap of ",
         server_max, ".", call. = FALSE)
  }
  .omopAnalysisResolveScope(handle, sources, combine = combine)
}

#' Materialize a population's person set as a cohort temp table
#'
#' Shared by the single-cohort path and the multi-population resolver: given a
#' vector of person ids, anchor them to \code{observation_period} (for
#' cohort_start/end_date, as baseline/survival outputs require) and create a temp
#' table of \code{subject_id, cohort_start_date, cohort_end_date}. Mirrors the
#' filter-cohort branch already used inline by \code{\link{.planExecute}}.
#'
#' An EMPTY id vector still materializes a valid, zero-row cohort table (via a
#' \code{WHERE 1=0} guard) when \code{allow_empty=TRUE}, so a criteria population
#' that legitimately resolves to nobody can still take part in a set operation
#' (intersect/union/setdiff) instead of crashing the fold. With the default
#' \code{allow_empty=FALSE} it preserves the historical contract: NULL for an
#' empty id vector (the caller keeps a bare person-id vector for event/
#' person_level outputs). Returns NULL when there is no observation_period table.
#'
#' @param handle CDM handle
#' @param bp Blueprint
#' @param person_ids Integer vector of person ids
#' @param name Character; temp table name
#' @param allow_empty Logical; when TRUE, materialize a zero-row table for an
#'   empty id vector instead of returning NULL
#' @return Character temp table name, or NULL
#' @keywords internal
.materializeCohortFromIds <- function(handle, bp, person_ids, name,
                                      allow_empty = FALSE) {
  if (length(person_ids) == 0 && !allow_empty) return(NULL)
  obs_table <- bp$tables[bp$tables$table_name == "observation_period" &
                           bp$tables$present_in_db, , drop = FALSE]
  if (nrow(obs_table) == 0) return(NULL)
  obs_qualified <- obs_table$qualified_name[1]
  where_clause <- if (length(person_ids) == 0) {
    "WHERE 1 = 0"
  } else {
    paste0("WHERE ", .sqlIdInPredicate("o.person_id", person_ids))
  }
  cohort_sql <- paste0(
    "SELECT DISTINCT o.person_id AS subject_id, ",
    "o.observation_period_start_date AS cohort_start_date, ",
    "o.observation_period_end_date AS cohort_end_date ",
    "FROM ", obs_qualified, " o ",
    where_clause
  )
  name <- .reserveTempTableName(handle, name)
  .createTempTable(handle, name, cohort_sql)
}

.normalizeIndexEventEndStrategy <- function(end_strategy) {
  if (is.null(end_strategy)) return(NULL)
  if (!is.list(end_strategy) || is.null(names(end_strategy)) ||
      any(!nzchar(names(end_strategy))) || anyDuplicated(names(end_strategy)) ||
      !identical(names(end_strategy), "DateOffset")) {
    stop("index_event end_strategy must be NULL or exactly one OHDSI ",
         "DateOffset strategy.", call. = FALSE)
  }
  offset <- end_strategy$DateOffset
  if (!is.list(offset) || is.null(names(offset)) ||
      any(!nzchar(names(offset))) || anyDuplicated(names(offset)) ||
      !setequal(names(offset), c("DateField", "Offset")) ||
      length(names(offset)) != 2L) {
    stop("index_event end_strategy$DateOffset must contain exactly ",
         "DateField and Offset.", call. = FALSE)
  }
  date_field <- offset$DateField
  if (!is.character(date_field) || length(date_field) != 1L ||
      is.na(date_field) ||
      !date_field %in% c("StartDate", "EndDate")) {
    stop("index_event end_strategy DateField must be StartDate or EndDate.",
         call. = FALSE)
  }
  value <- offset$Offset
  number <- suppressWarnings(as.numeric(value))
  integer <- suppressWarnings(as.integer(value))
  if (!is.numeric(value) || length(value) != 1L || length(number) != 1L ||
      is.na(number) || !is.finite(number) || length(integer) != 1L ||
      is.na(integer) || number != integer) {
    stop("index_event end_strategy Offset must be one finite exact integer.",
         call. = FALSE)
  }
  list(DateOffset = list(DateField = date_field, Offset = integer))
}

#' Materialize real OMOP rows as longitudinal index-event episodes
#'
#' Primary First/Last/All is applied to source events that fall inside an
#' observation period before any inclusion filter, matching OHDSI Circe. The
#' source primary key is retained internally so same-date source events remain
#' distinct while eligibility is evaluated.
#'
#' @param handle CDM handle
#' @param bp Blueprint
#' @param index_event Transport-safe index-event specification
#' @param name Temporary table name
#' @return Character temporary cohort table name
#' @keywords internal
.materializeIndexEventCohort <- function(handle, bp, index_event, name) {
  if (!is.list(index_event) || is.null(names(index_event)) ||
      any(!nzchar(names(index_event))) || anyDuplicated(names(index_event))) {
    stop("index_event must be a uniquely named list.", call. = FALSE)
  }
  unknown <- setdiff(names(index_event),
                     c("table", "concept_set", "primary_limit",
                       "end_strategy"))
  if (length(unknown) > 0L) {
    stop("Unknown index_event field(s): ", paste(unknown, collapse = ", "),
         ".", call. = FALSE)
  }
  allowed_tables <- c(
    "condition_occurrence", "drug_exposure", "measurement", "observation",
    "procedure_occurrence", "device_exposure", "visit_occurrence"
  )
  table <- index_event$table
  if (!is.character(table) || length(table) != 1L || is.na(table) ||
      !tolower(table) %in% allowed_tables) {
    stop("index_event table is outside the executable Circe table allowlist.",
         call. = FALSE)
  }
  table <- tolower(table)
  primary_limit <- index_event$primary_limit %||% "first"
  if (!is.character(primary_limit) || length(primary_limit) != 1L ||
      is.na(primary_limit) ||
      !tolower(primary_limit) %in% c("first", "last", "all")) {
    stop("index_event primary_limit must be first, last, or all.",
         call. = FALSE)
  }
  primary_limit <- tolower(primary_limit)
  end_strategy <- .normalizeIndexEventEndStrategy(index_event$end_strategy)

  row <- bp$tables[bp$tables$table_name == table & bp$tables$present_in_db,
                   , drop = FALSE]
  if (nrow(row) != 1L) {
    stop("index_event table '", table,
         "' is unavailable or ambiguous in the authorized blueprint.",
         call. = FALSE)
  }
  cols <- bp$columns[[table]]$column_name %||% character(0)
  start_col <- .getDateColumn(bp, table)
  pair <- .getDatePair(bp, table)
  end_col <- pair$end %||% start_col
  event_id_col <- .eventPrimaryKeyColumn(bp, table)
  required <- c("person_id", start_col, end_col, event_id_col)
  if (any(vapply(list(start_col, end_col, event_id_col), is.null,
                 logical(1))) || any(!required %in% cols)) {
    stop("index_event table '", table,
         "' lacks person, date, interval-end, or stable event-key columns.",
         call. = FALSE)
  }
  op_row <- bp$tables[
    bp$tables$table_name == "observation_period" & bp$tables$present_in_db,
    , drop = FALSE
  ]
  op_cols <- bp$columns[["observation_period"]]$column_name %||% character(0)
  op_required <- c(
    "observation_period_id", "person_id", "observation_period_start_date",
    "observation_period_end_date"
  )
  if (nrow(op_row) != 1L || !all(op_required %in% op_cols)) {
    stop("index_event requires one authorized observation_period table with ",
         "its person, start, end, and stable key columns.", call. = FALSE)
  }

  concept_predicate <- ""
  if (!is.null(index_event$concept_set)) {
    cs <- index_event$concept_set
    if (is.list(cs) && !is.null(cs$concepts)) {
      unknown_cs <- setdiff(names(cs),
                            c("concepts", "include_descendants",
                              "include_mapped"))
      if (length(unknown_cs) > 0L) {
        stop("Unknown index-event concept-set field(s): ",
             paste(unknown_cs, collapse = ", "), ".", call. = FALSE)
      }
      .conceptIdList(cs$concepts)
      for (flag in c("include_descendants", "include_mapped")) {
        if (!is.null(cs[[flag]]) &&
            (!is.logical(cs[[flag]]) || length(cs[[flag]]) != 1L ||
             is.na(cs[[flag]]))) {
          stop("index-event concept-set expansion flags must be TRUE/FALSE.",
               call. = FALSE)
        }
      }
    } else {
      .conceptIdList(cs)
    }
    concept_ids <- withCallingHandlers(
      .resolveConceptSet(handle, cs),
      warning = function(w) {
        stop("index-event concept expansion failed: ", conditionMessage(w),
             call. = FALSE)
      }
    )
    if (length(concept_ids) == 0L) {
      stop("index_event concept_set resolved to no concepts.", call. = FALSE)
    }
    concept_col <- .getDomainConceptColumn(bp, table)
    if (is.null(concept_col) || !concept_col %in% cols) {
      stop("index_event table '", table,
           "' has no authorized domain concept column.", call. = FALSE)
    }
    concept_predicate <- paste0(
      " AND ", .sqlIdInPredicate(paste0("t.", concept_col), concept_ids)
    )
  }

  qualified <- row$qualified_name[[1]]
  op_qualified <- op_row$qualified_name[[1]]
  op_join <- paste0(
    "op.person_id = t.person_id AND ",
    "op.observation_period_start_date <= t.", start_col, " AND ",
    "op.observation_period_end_date >= t.", start_col
  )
  # Apply the ordinary DataSHIELD population gate before any data-quality
  # diagnostic so a rare concept cannot be probed through differing validation
  # errors. Candidate events outside every observation period are not eligible
  # index events and therefore do not contribute to this gate.
  .assertMinPersons(handle = handle, sql = paste0(
    "SELECT COUNT(DISTINCT t.person_id) AS n FROM ", qualified,
    " t INNER JOIN ", op_qualified, " op ON ", op_join,
    " WHERE t.", start_col, " IS NOT NULL", concept_predicate
  ))
  coverage_sql <- paste0(
    "SELECT COUNT(*) AS n FROM (SELECT t.", event_id_col, " FROM ",
    qualified, " t LEFT JOIN ", op_qualified, " op ON ", op_join,
    " WHERE t.", start_col, " IS NOT NULL", concept_predicate,
    " GROUP BY t.person_id, t.", event_id_col,
    " HAVING COUNT(op.observation_period_id) > 1) dsomop_bad_index_op"
  )
  coverage <- .executeQuery(handle, coverage_sql)
  if (nrow(coverage) != 1L || !"n" %in% names(coverage) ||
      is.na(coverage$n[[1]]) || as.numeric(coverage$n[[1]]) > 0) {
    stop("Candidate index events cannot have overlapping covering ",
         "observation_period records.", call. = FALSE)
  }

  cohort_end <- "op.observation_period_end_date"
  if (!is.null(end_strategy)) {
    date_field <- end_strategy$DateOffset$DateField
    source_end <- if (identical(date_field, "StartDate")) {
      paste0("t.", start_col)
    } else {
      paste0("COALESCE(t.", end_col, ", t.", start_col, ")")
    }
    offset_end <- .dateAddSql(
      handle, end_strategy$DateOffset$Offset, source_end
    )
    cohort_end <- paste0(
      "CASE WHEN ", offset_end,
      " > op.observation_period_end_date THEN ",
      "op.observation_period_end_date ELSE ", offset_end, " END"
    )
  }
  event_select <- paste0(
    "t.person_id AS subject_id, t.", start_col,
    " AS cohort_start_date, ", cohort_end,
    " AS cohort_end_date, t.", event_id_col, " AS index_event_id"
  )
  where <- paste0(" WHERE t.", start_col, " IS NOT NULL", concept_predicate)
  event_from <- paste0(
    " FROM ", qualified, " t INNER JOIN ", op_qualified, " op ON ", op_join
  )
  if (identical(primary_limit, "all")) {
    selected <- paste0("SELECT ", event_select, event_from, where)
    sql <- if (is.null(end_strategy)) {
      selected
    } else {
      paste0(
        "SELECT subject_id, cohort_start_date, cohort_end_date, ",
        "index_event_id FROM (", selected, ") dsomop_offset_event ",
        "WHERE cohort_end_date >= cohort_start_date"
      )
    }
  } else {
    direction <- if (identical(primary_limit, "last")) "DESC" else "ASC"
    ranked <- paste0(
      "SELECT ", event_select, ", ROW_NUMBER() OVER (PARTITION BY t.person_id ",
      "ORDER BY t.", start_col, " ", direction, ", t.", event_id_col, " ",
      direction, ") AS dsomop_event_ordinal", event_from, where
    )
    sql <- paste0(
      "SELECT subject_id, cohort_start_date, cohort_end_date, index_event_id ",
      "FROM (", ranked, ") dsomop_ranked_event ",
      "WHERE dsomop_event_ordinal = 1",
      if (is.null(end_strategy)) "" else
        " AND cohort_end_date >= cohort_start_date"
    )
  }
  name <- .reserveTempTableName(handle, name)
  out <- .createTempTable(handle, name, sql)
  .assertMinPersons(handle = handle, sql = paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", out))
  out
}

#' Apply population eligibility predicates to each concrete index episode
#' @keywords internal
.filterIndexEventEpisodes <- function(handle, bp, cohort_table, filter_tree,
                                      name) {
  if (is.null(filter_tree)) return(cohort_table)
  person <- bp$tables[bp$tables$table_name == "person" &
                        bp$tables$present_in_db, , drop = FALSE]
  if (nrow(person) != 1L) {
    stop("Index-event filtering requires the authorized person table.",
         call. = FALSE)
  }
  person_cols <- bp$columns[["person"]]$column_name %||% character(0)
  where <- .compileCohortFilterWhere(
    handle, filter_tree, bp, person_cols,
    index_anchor = "idx.cohort_start_date"
  )
  if (!nzchar(where)) {
    stop("Index-event eligibility compiled to no predicate.", call. = FALSE)
  }
  sql <- paste0(
    "SELECT idx.subject_id, idx.cohort_start_date, idx.cohort_end_date, ",
    "idx.index_event_id FROM ", cohort_table, " idx JOIN ",
    person$qualified_name[[1]], " p ON p.person_id = idx.subject_id WHERE ",
    where
  )
  name <- .reserveTempTableName(handle, name)
  out <- .createTempTable(handle, name, sql)
  .assertMinPersons(handle = handle, sql = paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", out))
  out
}

#' Intersect a person scope while preserving every retained index episode
#' @keywords internal
.scopeIndexEventEpisodes <- function(handle, cohort_table, scope_cohort, name) {
  sql <- paste0(
    "SELECT idx.subject_id, idx.cohort_start_date, idx.cohort_end_date, ",
    "idx.index_event_id FROM ", cohort_table, " idx WHERE EXISTS (SELECT 1 FROM ",
    scope_cohort, " sc WHERE sc.subject_id = idx.subject_id)"
  )
  name <- .reserveTempTableName(handle, name)
  out <- .createTempTable(handle, name, sql)
  .assertMinPersons(handle = handle, sql = paste0(
    "SELECT COUNT(DISTINCT subject_id) AS n FROM ", out))
  out
}

#' Materialize an "all persons" cohort temp table
#'
#' Used to give an unrestricted population (e.g. the implicit base with no
#' cohort and no criteria) a concrete person set so a recipe-level scope can be
#' INTERSECTED into it. Anchors every observation_period person as a cohort row.
#' Returns NULL when there is no observation_period table.
#'
#' @param handle CDM handle
#' @param bp Blueprint
#' @param name Character; temp table name
#' @return Character temp table name, or NULL
#' @keywords internal
.materializeAllPersonsCohort <- function(handle, bp, name) {
  obs_table <- bp$tables[bp$tables$table_name == "observation_period" &
                           bp$tables$present_in_db, , drop = FALSE]
  if (nrow(obs_table) == 0) return(NULL)
  obs_qualified <- obs_table$qualified_name[1]
  cohort_sql <- paste0(
    "SELECT DISTINCT o.person_id AS subject_id, ",
    "o.observation_period_start_date AS cohort_start_date, ",
    "o.observation_period_end_date AS cohort_end_date ",
    "FROM ", obs_qualified, " o")
  name <- .reserveTempTableName(handle, name)
  .createTempTable(handle, name, cohort_sql)
}

#' Read the DISTINCT person ids from a cohort temp table
#'
#' @param handle CDM handle
#' @param cohort_table Character; cohort temp table name (subject_id column)
#' @return Integer vector of person ids
#' @keywords internal
.cohortPersonIds <- function(handle, cohort_table) {
  if (is.null(cohort_table)) return(integer(0))
  res <- .executeQuery(handle,
    paste0("SELECT DISTINCT subject_id AS person_id FROM ", cohort_table))
  if (nrow(res) > 0) res$person_id else integer(0)
}

#' Resolve a plan's populations into per-population person sets (dependency order)
#'
#' Each population in \code{plan$populations} is EITHER criteria-defined
#' (\code{filters}: a person-level cohort filter tree consumed by
#' \code{\link{.compileCohortFilterWhere}} via \code{\link{.buildCohortFromFilters}})
#' OR a set-operation over OTHER populations (\code{setop = list(op, members)},
#' folded with \code{\link{.cohortCombine}}). Set-op populations are resolved AFTER
#' their members, so a single pass over a parent-before-child ordering suffices;
#' a member that has not yet been resolved is an error (fail-closed). Every
#' produced person set is gated with \code{\link{.assertMinPersons}} — a tiny
#' intersection therefore fails closed before any output is produced.
#'
#' @param handle CDM handle
#' @param plan The extraction plan (uses \code{plan$populations} and, for the
#'   base population, \code{plan$cohort} when present)
#' @param bp Blueprint
#' @param base_cohort_table Character or NULL; a cohort temp table already
#'   materialized for the base population (e.g. from a cohort_definition_id)
#' @param base_person_ids Integer vector or NULL; person ids for the base
#'   population when no temp table was materialized
#' @param index_cohort_table Character or NULL; cohort table whose episode start
#'   dates anchor index-dependent criteria. Defaults to \code{base_cohort_table}.
#' @return Named list keyed by population id; each element is
#'   \code{list(cohort_table = <name|NULL>, person_ids = <int vector>)}
#' @keywords internal
.planResolvePopulations <- function(handle, plan, bp,
                                    base_cohort_table = NULL,
                                    base_person_ids = NULL,
                                    index_cohort_table = base_cohort_table) {
  pops <- plan$populations
  resolved <- list()

  for (pid in names(pops)) {
    pop <- pops[[pid]]

    # Population kind is declared by the client's .compile_population_spec:
    # "setop" carries setop=list(op, members); "criteria" carries an optional
    # nested AND/OR filter_tree and/or a cohort_definition_id. The base
    # population inherits whatever the plan-level cohort produced (a
    # cohort_definition_id table, a filter cohort, or unrestricted = NULL) and
    # may additionally carry its own criteria.
    is_base <- identical(pid, "base")
    kind <- tolower(pop$kind %||% "criteria")
    has_setop <- identical(kind, "setop") || !is.null(pop$setop)
    has_filter_tree <- !is.null(pop$filter_tree)
    has_own_cohort <- !is.null(pop$cohort_definition_id)
    has_index_event <- !is.null(pop$index_event)
    if (!is.null(pop$episode_policy) &&
        (!is.character(pop$episode_policy) ||
         length(pop$episode_policy) != 1L || is.na(pop$episode_policy) ||
         !tolower(pop$episode_policy) %in%
           c("any_episode", "all_episodes",
             "first_episode", "last_episode"))) {
      stop("Population '", pid, "' has an invalid episode_policy.",
           call. = FALSE)
    }

    if (has_setop) {
      if (has_index_event) {
        stop("Set-operation populations cannot declare index_event.",
             call. = FALSE)
      }
      if (!is.null(pop$episode_policy)) {
        stop("Set-operation populations cannot declare episode_policy.",
             call. = FALSE)
      }
      op <- tolower(pop$setop$op %||% "union")
      members <- unlist(pop$setop$members, use.names = FALSE)
      if (length(members) < 1) {
        stop("Population '", pid, "': set-op has no members.", call. = FALSE)
      }
      member_tables <- vapply(members, function(m) {
        r <- resolved[[m]]
        if (is.null(r)) {
          stop("Population '", pid, "': set-op member '", m,
               "' is not defined before it (declare members first).",
               call. = FALSE)
        }
        # A member with no materialized cohort table (an unrestricted base, or no
        # observation_period) cannot take part in a set operation on subject_id;
        # fail closed rather than silently widen the result.
        if (is.null(r$cohort_table)) {
          stop("Population '", pid, "': set-op member '", m,
               "' has no materialized person set.", call. = FALSE)
        }
        r$cohort_table
      }, character(1))

      combined <- member_tables[[1]]
      if (length(member_tables) > 1) {
        for (k in 2:length(member_tables)) {
          # .cohortCombine gates the running result with .assertMinPersons, so a
          # tiny intersection fails closed here, before any output is produced.
          # Each fold step needs its own temp-table name: reusing one name across
          # a >=3-member fold makes the 2nd .createTempTable collide ("table
          # already exists"). The LAST step lands at the canonical population name
          # so the cleanup loop (pop_temp_tables) drops it; intermediate steps get
          # a unique per-step name and the plan's ownership guard releases them.
          step_name <- if (k == length(member_tables)) {
            paste0("dsomop_plan_pop_", pid)
          } else {
            paste0("dsomop_plan_pop_", pid, "_fold", k)
          }
          step_name <- .reserveTempTableName(handle, step_name)
          combined <- .cohortCombine(handle, op, combined, member_tables[[k]],
            new_name = step_name)
        }
      } else {
        # Single member: re-gate so a lone-member set-op is still size-checked
        # under this population's id.
        .assertMinPersons(handle = handle, sql = paste0(
          "SELECT COUNT(DISTINCT subject_id) AS n FROM ", combined))
      }
      resolved[[pid]] <- list(
        cohort_table = combined,
        person_ids = .cohortPersonIds(handle, combined))
      next
    }

    if (has_index_event) {
      if (!is.null(pop$episode_policy)) {
        stop("Population '", pid, "': episode_policy cannot be combined with ",
             "index_event; primary_limit defines candidate-event selection.",
             call. = FALSE)
      }
      event_name <- paste0("dsomop_plan_pop_", pid, "_index")
      event_ct <- .materializeIndexEventCohort(
        handle, bp, pop$index_event, event_name
      )

      # A population-local cohort reference is a person scope. Keep the event
      # table on the left so its episode dates and recurrence survive.
      if (has_own_cohort) {
        own_ct <- .resolveCohortTable(handle, pop$cohort_definition_id)
        event_ct <- .scopeIndexEventEpisodes(
          handle, event_ct, own_ct,
          name = paste0("dsomop_plan_pop_", pid, "_own_scope")
        )
      }
      if (has_filter_tree) {
        event_ct <- .filterIndexEventEpisodes(
          handle, bp, event_ct, pop$filter_tree,
          name = paste0("dsomop_plan_pop_", pid)
        )
      }
      person_ids <- .cohortPersonIds(handle, event_ct)
      .assertMinPersons(n_persons = length(unique(person_ids)))
      resolved[[pid]] <- list(
        cohort_table = event_ct,
        person_ids = person_ids,
        preserve_index_episodes = TRUE
      )
      next
    }

    if (is_base && !has_filter_tree && !has_own_cohort) {
      # Base population with no extra criteria: reuse the plan-level cohort.
      resolved[[pid]] <- list(
        cohort_table = base_cohort_table,
        person_ids = base_person_ids %||%
          .cohortPersonIds(handle, base_cohort_table))
      next
    }

    # Criteria population. Start from the widest applicable seed, then AND each
    # narrowing source (own cohort_definition_id, filter_tree, and — for the
    # base population — the inherited plan-level cohort). Each source only
    # narrows; a population can never be wider than its seed.
    seed_ids <- NULL
    if (is_base) {
      seed_ids <- base_person_ids %||%
        .cohortPersonIds(handle, base_cohort_table)
      if (length(seed_ids) == 0 && is.null(base_cohort_table) &&
          is.null(base_person_ids)) {
        seed_ids <- NULL  # unrestricted base: no seed restriction
      }
    }

    if (has_own_cohort) {
      own_ct <- .resolveCohortTable(handle, pop$cohort_definition_id)
      own_ids <- .cohortPersonIds(handle, own_ct)
      seed_ids <- if (is.null(seed_ids)) own_ids else intersect(seed_ids, own_ids)
    }

    if (has_filter_tree) {
      # Anchor windowed concept filters in this population to the plan-level
      # cohort's index date (e.g. cohort=313217L). Without a plan cohort there is
      # nothing to anchor to, so windows fall back to the wall-clock date inside
      # .windowPredicateSql.
      filter_ids <- .buildCohortFromFilters(handle, pop$filter_tree,
        index_cohort_table = index_cohort_table,
        episode_policy = pop$episode_policy)
      seed_ids <- if (is.null(seed_ids)) filter_ids
                  else intersect(seed_ids, filter_ids)
    }

    person_ids <- seed_ids %||% integer(0)
    .assertMinPersons(n_persons = length(unique(person_ids)))
    # Materialize a cohort table even when this population resolved to nobody
    # (allow_empty), so a later set-op over it operates on a real zero-row table
    # instead of failing with "no materialized person set". An empty member makes
    # intersect -> empty, union -> the other side, setdiff -> the other side, all
    # of which are the correct algebra.
    cohort_table <- .materializeCohortFromIds(handle, bp, person_ids,
      name = paste0("dsomop_plan_pop_", pid), allow_empty = TRUE)
    resolved[[pid]] <- list(cohort_table = cohort_table,
                            person_ids = person_ids)
  }

  resolved
}

#' Intersect a unified scope cohort into every resolved population
#'
#' The recipe-level scope (a cohort ref and/or one or more \code{omop.table}
#' symbols, already folded into ONE re-gated cohort temp table by
#' \code{\link{.omopAnalysisResolveScope}}) restricts EVERY population. Each
#' population's person set is INTERSECTED with the scope on subject_id via
#' \code{\link{.cohortCombine}} (which re-gates the result fail-closed); the
#' person-id vector is then re-derived from the narrowed cohort. A NULL scope is
#' a no-op.
#'
#' @param handle CDM handle
#' @param resolved Named list from \code{\link{.planResolvePopulations}}
#' @param scope_cohort Character or NULL; the folded scope cohort temp table
#' @param bp Blueprint
#' @return The \code{resolved} list with each population narrowed to the scope
#' @keywords internal
.planScopePopulations <- function(handle, resolved, scope_cohort, bp) {
  if (is.null(scope_cohort)) return(resolved)

  for (pid in names(resolved)) {
    r <- resolved[[pid]]
    ct <- r$cohort_table
    # A population with no materialized cohort table must still be scoped:
    # materialize its person set so it can be intersected. An unrestricted
    # population (NULL table AND empty ids — e.g. the implicit base with no
    # cohort or criteria) materializes ALL persons; otherwise materialize its
    # known ids. If neither can be materialized, fail closed.
    if (is.null(ct)) {
      ct <- if (length(r$person_ids) == 0) {
        .materializeAllPersonsCohort(handle, bp,
          name = paste0("dsomop_plan_pop_", pid, "_all"))
      } else {
        .materializeCohortFromIds(handle, bp, r$person_ids,
          name = paste0("dsomop_plan_pop_", pid, "_pre"))
      }
      if (is.null(ct)) {
        stop("Scope cannot be applied to population '", pid,
             "': no materializable person set.", call. = FALSE)
      }
    }
    # Explicit index-event populations keep THEIR concrete event episodes; all
    # legacy populations keep the scope cohort's episode bounds as before.
    narrowed <- if (isTRUE(r$preserve_index_episodes)) {
      .scopeIndexEventEpisodes(
        handle, ct, scope_cohort,
        name = paste0("dsomop_plan_pop_", pid, "_scoped")
      )
    } else {
      scoped_name <- .reserveTempTableName(
        handle, paste0("dsomop_plan_pop_", pid, "_scoped")
      )
      .cohortCombine(handle, "intersect", scope_cohort, ct,
        new_name = scoped_name)
    }
    resolved[[pid]] <- list(
      cohort_table = narrowed,
      person_ids = .cohortPersonIds(handle, narrowed),
      preserve_index_episodes = isTRUE(r$preserve_index_episodes))
  }
  resolved
}

#' Execute a plan and produce server-side data frames
#'
#' Processes all outputs defined in the plan: builds a cohort (if specified),
#' then iterates over each output entry to extract, transform, and return
#' the requested data frames.
#'
#' @param handle CDM handle
#' @param plan List; the extraction plan
#' @param out_symbols Named list; output name -> R symbol mapping
#' @param output_mode Character; "memory" (default) or "staged"
#' @return Named list of data frames or staged dataset descriptors.
#' @keywords internal
.planExecute <- function(handle, plan, out_symbols, output_mode = "memory") {
  temp_tables_before <- unique(handle$temp_tables %||% character(0))
  on.exit(
    .dropTempTablesCreatedSince(handle, temp_tables_before),
    add = TRUE
  )
  outputs <- plan$outputs %||% list()
  max_plan_outputs <- .extractionCap("dsomop.max_plan_outputs", 100L)
  if (length(outputs) > max_plan_outputs) {
    stop("Plan exceeds the server max_plan_outputs cap of ",
         max_plan_outputs, ".", call. = FALSE)
  }
  bp <- .buildBlueprint(handle)

  staged <- identical(output_mode, "staged")
  person_key <- if (staged) .personKey(handle) else NULL
  pseudonymization <- if (staged) .personKeyPublicContract(handle) else NULL
  staging_dir <- NULL
  staging_token <- NULL
  staged_descriptors <- list()
  staging_committed <- FALSE

  if (!is.list(outputs) ||
      (length(outputs) > 0L &&
       (is.null(names(outputs)) || any(!nzchar(names(outputs))) ||
        anyDuplicated(names(outputs))))) {
    stop("Plan outputs must be a uniquely named list.", call. = FALSE)
  }
  for (output_name in names(outputs)) {
    .validateIdentifier(output_name, "output")
  }
  cohort_declaration_errors <- .planRequiredCohortErrors(plan)
  if (length(cohort_declaration_errors) > 0L) {
    stop(paste(cohort_declaration_errors, collapse = " "), call. = FALSE)
  }

  if (staged) {
    .validateStagedScopeDeclaration(plan)
    max_outputs <- suppressWarnings(as.numeric(
      getOption("dsomop.max_staged_outputs", 100L)
    ))
    max_dirs <- suppressWarnings(as.numeric(
      getOption("dsomop.max_staging_dirs_per_handle", 8L)
    ))
    if (length(max_outputs) != 1L || is.na(max_outputs) ||
        !is.finite(max_outputs) || max_outputs != floor(max_outputs) ||
        max_outputs < 1L || length(outputs) > max_outputs ||
        length(max_dirs) != 1L || is.na(max_dirs) || !is.finite(max_dirs) ||
        max_dirs != floor(max_dirs) || max_dirs < 1L) {
      stop("Staged output/directory caps must be positive server integers and ",
           "the plan must stay within them.", call. = FALSE)
    }
    live_dirs <- unique(handle$staging_dirs %||% character(0))
    if (length(live_dirs) > 0L) {
      base <- .stagingBaseDir()
      states <- vapply(
        live_dirs,
        function(path) .inspectOwnedStagingPath(path, base)$state,
        character(1)
      )
      not_live <- live_dirs[states != "directory"]
      if (length(not_live) > 0L) {
        # Valid paths already absent are removed idempotently. Invalid paths and
        # symlinks remain owned and abort preflight so they cannot be forgotten.
        .cleanupHandleStaging(handle, paths = not_live)
      }
      live_dirs <- unique(handle$staging_dirs %||% character(0))
    }
    if (length(live_dirs) >= max_dirs) {
      stop("This OMOP handle has reached its staged-directory cap; clean up ",
           "expired/consumed staged outputs before creating more.",
           call. = FALSE)
    }
    staging_token <- .generateStagingToken()
    staging_dir <- .createStagingDir(staging_token)
    handle$staging_dirs <- union(handle$staging_dirs %||% character(0),
                                 staging_dir)
    on.exit({
      # An error must not strand a partially written directory. A successful
      # plan commits only after every output and its manifest are complete.
      if (!staging_committed && !is.null(staging_dir)) {
        .cleanupHandleStaging(handle, paths = staging_dir)
      }
    }, add = TRUE)
  }

  cohort_table <- NULL
  cohort_person_ids <- NULL

  # Resolve scope before compiling population filters: when the scope is an
  # existing cohort, its actual cohort_start_date is the index anchor used by
  # the explicit episode policy.
  scope_cohort <- .planResolveScopeCohort(handle, plan)

  if (!is.null(plan$cohort)) {
    # NOTE: a plan$cohort of type "cohort_table" (a scalar cohort_definition_id)
    # is NOT a base population — it is resolved UNIFORMLY as a recipe-level SCOPE
    # (gated person-set -> intersected into every population) by
    # .planResolveScopeCohort below, which also supplies it as the per-person
    # INDEX anchor (its real cohort_start/end_date) so windowed population filters
    # and relative_to_index outputs keep anchoring to the cohort. Only a
    # filter_tree or an inline concept spec defines the base population here.
    if (!is.null(plan$cohort$filter_tree)) {
      # Recipe-authored population filters: a nested AND/OR cohort filter tree
      # (the sole transport from recipe_to_plan).
      filter_spec <- plan$cohort$filter_tree
      cohort_person_ids <- .buildCohortFromFilters(
        handle, filter_spec,
        index_cohort_table = scope_cohort,
        episode_policy = plan$cohort$episode_policy
      )
      # Materialize a cohort temp table so baseline/survival outputs work
      if (length(cohort_person_ids) > 0) {
        obs_table <- bp$tables[bp$tables$table_name == "observation_period" &
                                 bp$tables$present_in_db, , drop = FALSE]
        if (nrow(obs_table) > 0) {
          obs_qualified <- obs_table$qualified_name[1]
          cohort_sql <- paste0(
            "SELECT DISTINCT o.person_id AS subject_id, ",
            "o.observation_period_start_date AS cohort_start_date, ",
            "o.observation_period_end_date AS cohort_end_date ",
            "FROM ", obs_qualified, " o WHERE ",
            .sqlIdInPredicate("o.person_id", cohort_person_ids)
          )
          cohort_name <- .reserveTempTableName(handle, "dsomop_plan_cohort")
          cohort_table <- .createTempTable(handle, cohort_name, cohort_sql)
        }
      }
      .assertMinPersons(n_persons = length(unique(cohort_person_ids)))

    } else if (!is.null(plan$cohort$spec)) {
      # Inline concept-based spec from ds.omop.plan.cohort(spec = ...): use
      # existing cohortCreate.
      spec <- plan$cohort$spec
      cohort_table <- .cohortCreate(
        handle, spec, mode = "temporary",
        cohort_id = plan$cohort$cohort_definition_id)
      pid_result <- .executeQuery(handle,
        paste0("SELECT DISTINCT subject_id AS person_id FROM ",
               cohort_table))
      cohort_person_ids <- pid_result$person_id
    }
  }

  # Resolve the recipe-level scope ONCE (a folded, re-gated cohort temp table, or
  # NULL). It carries the scalar plan$cohort id too (see .planResolveScopeCohort),
  # so its cohort_start/end_date is the per-person INDEX anchor a windowed/
  # relative_to_index extraction needs. When no explicit base was built above
  # (no filter_tree / spec), adopt the scope cohort as the index anchor so
  # anchoring survives the move of the scalar cohort from "base" to "scope".
  if (is.null(cohort_table) && !is.null(scope_cohort)) {
    cohort_table <- scope_cohort
    cohort_person_ids <- .cohortPersonIds(handle, scope_cohort)
  }

  # Multi-population resolution. When the plan declares populations, resolve each
  # (criteria -> person set via the cohort filter machinery; set-op -> fold with
  # .cohortCombine) into its own gated person set, then INTERSECT the unified
  # recipe-level scope into every one of them. Each output is later produced over
  # ITS population_id's person set. When the plan declares NO populations (or only
  # the implicit base with no extra criteria), every output runs against the
  # single base cohort built above — the unchanged fast path.
  pop_sets <- NULL
  multi_pop <- !is.null(plan$populations) &&
    .planHasMultiPopulation(plan)
  if (multi_pop) {
    pop_sets <- .planResolvePopulations(handle, plan, bp,
      base_cohort_table = cohort_table,
      base_person_ids = cohort_person_ids,
      index_cohort_table = scope_cohort %||% cohort_table)
    pop_sets <- .planScopePopulations(handle, pop_sets, scope_cohort, bp)
  } else if (!is.null(scope_cohort)) {
    # Single-population fast path: intersect the scope into the base cohort and
    # re-derive ids. The scope cohort is the LEFT side of the intersect so its
    # cohort_start/end_date (the cohort's real index date) survives into the
    # result — relative_to_index outputs anchor to it. When there is no explicit
    # base (filter_tree / spec), cohort_table already IS the scope cohort (set
    # above), so the intersect is the cohort with itself: same persons, same
    # dates, re-gated.
    base_ct <- cohort_table %||% scope_cohort
    scoped_name <- .reserveTempTableName(
      handle, "dsomop_plan_cohort_scoped"
    )
    cohort_table <- .cohortCombine(handle, "intersect", scope_cohort,
      base_ct, new_name = scoped_name)
    cohort_person_ids <- .cohortPersonIds(handle, cohort_table)
  }

  # Temp tables created for populations/scope (cleaned up with the base cohort).
  pop_temp_tables <- character(0)
  if (!is.null(pop_sets)) {
    pop_temp_tables <- unique(stats::na.omit(unname(vapply(pop_sets,
      function(p) p$cohort_table %||% NA_character_, character(1)))))
  }

  # Snapshot the base cohort: the output loop reassigns cohort_table /
  # cohort_person_ids per output (to its population) in the multi-pop path, so
  # keep the single-population base values to fall back to.
  base_cohort_table <- cohort_table
  base_cohort_person_ids <- cohort_person_ids

  results <- list()
  # Landed names of concept-id columns per output, so the factor harmonization
  # layer recognises them even when a user has renamed the _concept_id suffix
  # away. Carried out of here as an attribute on the returned list (see tail).
  concept_cols_by_output <- list()
  options <- plan$options %||% list()
  translate <- options$translate_concepts %||% TRUE
  # Staged datasets are an interoperability boundary. Preserve standard OMOP
  # concept IDs so every long output can stream and downstream OHDSI tools can
  # join vocabulary tables without loading or mutating the fact dataset. Human
  # labels belong in a separate concept-reference component, not in-place in a
  # potentially massive event stream.
  if (staged) translate <- FALSE
  block_sensitive <- options$block_sensitive %||% TRUE

  # Concept expansion cache: expand each unique concept set once
  concept_cache <- new.env(parent = emptyenv())
  for (out_name_pre in names(outputs)) {
    out_pre <- outputs[[out_name_pre]]
    cs <- out_pre$filters$concept_set$ids %||% out_pre$concept_set
    if (is.list(cs) && !is.null(cs$concepts)) {
      # Expansion flags and exclusions are semantic input, not cache metadata.
      # Sets with equal roots but different expansion policies must not collide.
      key <- as.character(jsonlite::toJSON(
        cs, auto_unbox = TRUE, null = "null", digits = NA
      ))
      if (!exists(key, envir = concept_cache)) {
        expanded <- .vocabExpandConceptSet(handle, cs)
        assign(key, expanded, envir = concept_cache)
      }
    }
  }

  # Track materialized concept set temp tables for cleanup
  cleanup_plan_temps <- function(remove_staging = FALSE) {
    temp_names <- unique(c(base_cohort_table, pop_temp_tables))
    temp_names <- temp_names[!is.na(temp_names) & nzchar(temp_names)]
    # A caller-owned cohort can become the plan's base/scope.  It existed at
    # entry and must never be released by this operation.
    temp_names <- setdiff(temp_names, temp_tables_before)
    for (temp_name in temp_names) {
      try(.dropTempTable(handle, temp_name), silent = TRUE)
    }
    if (remove_staging && !is.null(staging_dir)) {
      .cleanupHandleStaging(handle, paths = staging_dir)
    }
    invisible(NULL)
  }

  # Structural validation above proves only that a cohort-producing source was
  # declared. Resolve it once, then reject the whole plan before extracting any
  # output if a cohort-dependent output's selected population did not actually
  # produce a cohort table. This check stays outside the per-output permissive
  # tryCatch so query_strict=FALSE can never turn it into warning + NULL and
  # leave a pre-existing assigned symbol looking current.
  for (out_name in names(outputs)) {
    out <- outputs[[out_name]]
    out_type <- tolower(out$type %||% "event_level")
    if (!out_type %in% .planCohortOutputTypes()) next
    population_id <- out$population_id %||% "base"
    resolved_cohort <- if (is.null(pop_sets)) {
      base_cohort_table
    } else {
      population <- pop_sets[[population_id]]
      if (is.null(population)) NULL else population$cohort_table
    }
    if (is.null(resolved_cohort)) {
      cleanup_plan_temps(remove_staging = TRUE)
      stop(
        "Output '", out_name, "' (type '", out_type,
        "') requires a cohort; population '", population_id,
        "' did not resolve to an executable cohort table.",
        call. = FALSE
      )
    }
  }

  # First pass: process all non-dictionary outputs
  for (out_name in names(outputs)) {
    out <- outputs[[out_name]]
    out_type <- out$type %||% "event_level"

    # Skip concept_dictionary for second pass
    if (out_type == "concept_dictionary") next

    # Multi-population: produce this output over ITS population's gated,
    # scope-narrowed person set. Single-population leaves the base cohort vars
    # untouched (pop_sets is NULL), preserving the fast path exactly.
    cohort_table <- base_cohort_table
    cohort_person_ids <- base_cohort_person_ids
    if (!is.null(pop_sets)) {
      pid <- out$population_id %||% "base"
      pset <- pop_sets[[pid]]
      if (is.null(pset)) {
        results[[out_name]] <- NULL
        warning("Plan output '", out_name, "' targets unknown population '",
                pid, "'.", call. = FALSE)
        next
      }
      cohort_table <- pset$cohort_table
      cohort_person_ids <- pset$person_ids
    }
    # Prefer the database-resident cohort relation whenever it exists. Sending
    # the same population again as an inline IN list is redundant, can generate
    # very large SQL, and hits backend expression limits. Keep IDs only for the
    # rare population form that has no materialized cohort table.
    sql_scope_person_ids <- if (is.null(cohort_table)) {
      cohort_person_ids
    } else {
      NULL
    }

    tryCatch({
      custom_filters <- out$filters$custom

      if (out_type == "person_level") {
        result_df <- NULL
        out_repr <- out$representation %||% "long"
        # Occurrence/count feature columns that must read 0 (not NA) for
        # persons who have no events in that table — collected from each
        # feature frame and applied after the cross-table join below.
        zero_fill_cols <- character(0)
        if (!is.null(custom_filters) && length(custom_filters) > 0 &&
            length(out$tables %||% list()) == 0) {
          stop("person_level custom filters require at least one source table.",
               call. = FALSE)
        }
        table_specs <- out$tables %||% list()
        raw_tables <- names(table_specs)[vapply(table_specs, function(entry) {
          !(is.list(entry) && !is.null(entry$features))
        }, logical(1))]
        # Only PERSON and DEATH are 0/1-row-per-person CDM tables. Any raw
        # repeatable table violates the person_level cardinality contract even
        # when it is the sole table in the output.
        repeatable_raw <- setdiff(raw_tables, c("person", "death"))
        if (length(repeatable_raw) > 0L) {
          stop("person_level raw table(s) do not guarantee one row per person: ",
               paste(repeatable_raw, collapse = ", "), ". Aggregate them as ",
               "features or request an event_level output.", call. = FALSE)
        }

        for (tbl_name in names(out$tables %||% list())) {
          entry <- out$tables[[tbl_name]]
          entry_filters <- if (is.list(entry)) entry$filters else NULL
          table_filters <- if (is.null(custom_filters) ||
                               length(custom_filters) == 0) {
            entry_filters
          } else if (is.null(entry_filters) || length(entry_filters) == 0) {
            custom_filters
          } else {
            list(and = list(entry_filters, custom_filters))
          }

          # Check if entry has feature specs (list with $features)
          if (is.list(entry) && !is.null(entry$features)) {
            tbl_df <- .extractTable(
              handle,
              table = tbl_name,
              columns = NULL,
              concept_filter = entry$concept_set,
              person_ids = sql_scope_person_ids,
              cohort_table = cohort_table,
              translate_concepts = translate,
              representation = "features",
              feature_specs = entry$features,
              block_sensitive = block_sensitive,
              filters = table_filters,
              concept_col = entry$concept_col,
              visit_filter = entry$visit
            )
            zero_fill_cols <- c(zero_fill_cols,
                                attr(tbl_df, "omop_zero_fill"))
          } else {
            # Raw column list. The plan crosses the transport as JSON, so
            # `entry` arrives as a (possibly named) list; .colSpec recovers
            # the source columns to SELECT and any aliases to expose. Named
            # entries are aliases, e.g.
            #   tables = list(person = c(sex = "gender_concept_id"))
            spec <- .colSpec(entry)
            tbl_df <- .extractTable(
              handle,
              table = tbl_name,
              columns = spec$source,
              person_ids = sql_scope_person_ids,
              cohort_table = cohort_table,
              translate_concepts = translate,
              representation = "long",
              block_sensitive = block_sensitive,
              filters = table_filters
            )
            tbl_df <- .applyColumnAliases(tbl_df, spec)
            concept_cols_by_output[[out_name]] <- c(
              concept_cols_by_output[[out_name]], .conceptAliases(spec))
          }

          if (is.null(result_df)) {
            result_df <- tbl_df
          } else if ("person_id" %in% names(tbl_df) &&
                     "person_id" %in% names(result_df)) {
            result_df <- merge(result_df, tbl_df,
                               by = "person_id", all = TRUE)
          }
        }

        # Compute derived columns (age, sex, obs_duration)
        if (!is.null(out$derived_columns) &&
            length(out$derived_columns) > 0) {
          derived_df <- .computeDerivedColumns(
            handle, out$derived_columns,
            cohort_person_ids, cohort_table)
          if (!is.null(derived_df) && !is.null(result_df)) {
            # Full outer join: derived columns are computed over the whole
            # cohort, so persons missing from an empty/sparse feature sub-table
            # (e.g. an unseeded BMI concept) must NOT be dropped — they keep
            # their demographics with NA features rather than collapsing the
            # whole person_level frame to zero rows.
            result_df <- merge(result_df, derived_df,
                               by = "person_id", all = TRUE)
          } else if (!is.null(derived_df)) {
            result_df <- derived_df
          }
        }

        # Persons absent from a feature table joined with all = TRUE above
        # arrive as NA; for occurrence/count features absence means 0.
        zf <- intersect(unique(zero_fill_cols), names(result_df))
        for (col in zf) {
          result_df[[col]][is.na(result_df[[col]])] <- 0L
        }

        results[[out_name]] <- result_df

      } else if (out_type == "event_level") {
        repr <- out$representation$format %||% "long"
        time_window <- NULL

        if (!is.null(out$filters$time_window)) {
          tw <- out$filters$time_window
          time_window <- list(
            date_column = tw$date_column,
            start_date  = tw$start_date,
            end_date    = tw$end_date
          )
        }

        concept_set <- out$filters$concept_set$ids %||% out$concept_set

        # Custom filter DSL, visit-linkage filter, and concept-scoping column
        # override carried on the output. These are forwarded to .compileSelect
        # (directly when streaming, via .extractTable otherwise); .compileSelect
        # validates the custom filter fail-closed before emitting any SQL.
        visit_filter   <- out$filters$visit %||% out$visit_filter
        concept_col    <- out$filters$concept_col %||% out$concept_col

        # Use concept cache if available, otherwise expand
        if (is.list(concept_set) && !is.null(concept_set$concepts)) {
          key <- as.character(jsonlite::toJSON(
            concept_set, auto_unbox = TRUE, null = "null", digits = NA
          ))
          if (exists(key, envir = concept_cache)) {
            concept_set <- get(key, envir = concept_cache)
          } else {
            concept_set <- .vocabExpandConceptSet(handle, concept_set)
          }
        }

        # Add cohort date when index_window is active (for days_from_index)
        add_cohort_date <- !is.null(cohort_table) &&
          !is.null(out$temporal$index_window)

        # Every staged long event stream preserves numeric OMOP concept IDs and
        # therefore writes directly to Parquet without materializing in R.
        # Features/wide/sparse need in-memory reshaping, so they fall through.
        can_stream <- staged && repr == "long"

        if (can_stream) {
          sql <- .compileSelect(
            handle, out$table,
            columns = out$columns,
            concept_filter = concept_set,
            person_ids = sql_scope_person_ids,
            time_window = time_window,
            cohort_table = cohort_table,
            block_sensitive = block_sensitive,
            temporal = out$temporal,
            add_cohort_date = add_cohort_date,
            filters = custom_filters,
            concept_col = concept_col,
            visit_filter = visit_filter
          )

          if (!is.null(out$temporal$min_gap)) {
            tie_col <- if (!is.null(
              .eventPrimaryKeyColumn(bp, tolower(out$table)))) {
              "dsomop_event_order_id"
            } else {
              NULL
            }
            sql <- .wrapMinGap(handle, sql, out$temporal,
                               "dsomop_event_order_date", tie_col = tie_col)
          }

          if (!is.null(out$temporal$event_select)) {
            tie_col <- if (!is.null(
              .eventPrimaryKeyColumn(bp, tolower(out$table)))) {
              "dsomop_event_order_id"
            } else {
              NULL
            }
            sql <- .wrapEventSelect(handle, sql, out$temporal,
                                    "dsomop_event_order_date",
                                    tie_col = tie_col)
          }

          # Disclosure check before streaming
          col_df <- bp$columns[[tolower(out$table)]]
          if ("person_id" %in% col_df$column_name) {
            count_sql <- .compilePersonCount(handle, sql)
            .assertMinPersons(handle = handle, sql = count_sql)
          }

          # Build per-chunk transform for date handling + type conversion.
          # Accept a bare string (e.g. "relative_to_index") as well as the list
          # form, mapping the public synonym onto the internal "relative" mode.
          dh <- .normalizeDateHandling(out$date_handling)
          if (is.null(dh)) {
            default_mode <- getOption("dsomop.default_date_handling", "remove")
            dh <- .normalizeDateHandling(default_mode)
          }
          if (identical(dh$mode, "absolute")) {
            allow <- getOption("dsomop.allow_absolute_dates",
                       getOption("default.dsomop.allow_absolute_dates", FALSE))
            if (!isTRUE(allow)) {
              stop("Absolute date handling is not permitted by the server.",
                   call. = FALSE)
            }
          }

          # Capture date column for days_from_index computation
          tbl_date_col <- .getDateColumn(bp, tolower(out$table))

          chunk_fn <- function(chunk) {
            # Compute days_from_index when cohort_start_date is present
            date_source <- if (!is.null(tbl_date_col) &&
                               tbl_date_col %in% names(chunk)) {
              tbl_date_col
            } else if ("dsomop_event_order_date" %in% names(chunk)) {
              "dsomop_event_order_date"
            } else {
              NULL
            }
            if ("cohort_start_date" %in% names(chunk) &&
                !is.null(date_source)) {
              chunk$days_from_index <- as.integer(
                as.Date(chunk[[date_source]]) -
                as.Date(chunk$cohort_start_date)
              )
            }
            chunk$dsomop_event_order_date <- NULL
            chunk <- .convertTypes(chunk)
            chunk <- .applyDateHandling(chunk, dh)
            chunk$cohort_start_date <- NULL
            chunk$cohort_end_date <- NULL
            chunk$rn <- NULL
            chunk$dsomop_event_order_id <- NULL
            chunk$dsomop_event_partition_concept <- NULL
            chunk[grep("^dsomop_gap_", names(chunk), value = TRUE)] <- NULL
            .pseudonymizeIdentifiers(
              chunk, person_key, pseudonymization = pseudonymization
            )
          }

          output_path <- file.path(staging_dir,
                                    paste0(out_name, ".parquet"))
          file_info <- .executeQueryToParquet(
            .conn(handle), sql, output_path, chunk_fn = chunk_fn
          )
          desc <- .buildStagedDescriptor(
            out_name, file_info, staging_token,
            pseudonymization = pseudonymization,
            semantic_contract = .stagedSemanticContract(plan, out_name),
            bundle_contract = .stagedBundleContract(
              plan, out_name, staging_token
            )
          )
          results[[out_name]] <- desc
          staged_descriptors[[out_name]] <- desc

        } else {
          result_df <- .extractTable(
            handle,
            table = out$table,
            columns = out$columns,
            concept_filter = concept_set,
            person_ids = sql_scope_person_ids,
            time_window = time_window,
            cohort_table = cohort_table,
            translate_concepts = translate,
            representation = repr,
            feature_specs = out$representation$features,
            representation_grain = out$representation$grain %||% "person",
            block_sensitive = block_sensitive,
            temporal = out$temporal,
            date_handling = out$date_handling,
            add_cohort_date = add_cohort_date,
            filters = custom_filters,
            concept_col = concept_col,
            visit_filter = visit_filter
          )

          results[[out_name]] <- result_df
        }

      } else if (out_type == "baseline") {
        if (!is.null(custom_filters) && length(custom_filters) > 0) {
          stop("baseline does not have one unambiguous event table for ",
               "filters$custom; use a filtered population or event output.",
               call. = FALSE)
        }
        # Person columns accept aliases the same way as person_level:
        # extract by source name, then rename to the requested alias.
        base_spec <- .colSpec(out$columns)
        results[[out_name]] <- .applyColumnAliases(
          .extractBaseline(
            handle,
            cohort_table = cohort_table,
            columns = base_spec$source,
            derived = out$derived,
            translate_concepts = translate,
            age_breaks = out$age_breaks
          ),
          base_spec
        )
        concept_cols_by_output[[out_name]] <- .conceptAliases(base_spec)

      } else if (out_type == "survival") {
        survival_sql <- .compilePlanSurvivalSql(
          handle,
          cohort_table = cohort_table,
          output = out,
          custom_filters = custom_filters
        )
        if (staged) {
          .validateLongitudinalSurvivalSql(handle, survival_sql)
          survival_chunk <- function(chunk) {
            chunk <- .convertTypes(chunk)
            .pseudonymizeIdentifiers(
              chunk, person_key, pseudonymization = pseudonymization
            )
          }
          stage_survival_component <- function(sql, dataset_name,
                                               component = NULL) {
            file_info <- .executeQueryToParquet(
              .conn(handle), sql,
              file.path(staging_dir, paste0(dataset_name, ".parquet")),
              chunk_fn = survival_chunk
            )
            desc <- .buildStagedDescriptor(
              dataset_name, file_info, staging_token,
              pseudonymization = pseudonymization,
              semantic_contract = .stagedSemanticContract(
                plan, out_name, component = component
              ),
              bundle_contract = .stagedBundleContract(
                plan, out_name, staging_token
              )
            )
            staged_descriptors[[dataset_name]] <<- desc
            desc
          }
          if (identical(survival_sql$format, "multi_state")) {
            machine <- .newMultistateStreamTransformer(
              survival_sql, max_rows = Inf
            )
            multistate_chunk <- function(chunk) {
              transformed <- machine$transform(chunk)
              transformed <- .convertTypes(transformed)
              .pseudonymizeIdentifiers(
                transformed, person_key,
                pseudonymization = pseudonymization
              )
            }
            max_fanout <- max(table(survival_sql$multistate$edges$from))
            stream_chunk_size <- max(1L, floor(50000L / max_fanout))
            msdata_name <- paste0(out_name, ".msdata")
            msdata_info <- .executeQueryToParquet(
              .conn(handle), survival_sql$sql,
              file.path(staging_dir, paste0(msdata_name, ".parquet")),
              chunk_size = stream_chunk_size,
              chunk_fn = multistate_chunk
            )
            machine$assert_complete()
            msdata_desc <- .buildStagedDescriptor(
              msdata_name, msdata_info, staging_token,
              pseudonymization = pseudonymization,
              semantic_contract = .stagedSemanticContract(
                plan, out_name, component = "msdata"
              ),
              bundle_contract = .stagedBundleContract(
                plan, out_name, staging_token
              )
            )
            transition_name <- paste0(out_name, ".transition_ref")
            transition_desc <- .stageDataFrame(
              survival_sql$components$transition_ref,
              transition_name, staging_dir, staging_token, person_key,
              pseudonymization = pseudonymization,
              semantic_contract = .stagedSemanticContract(
                plan, out_name, component = "transition_ref"
              ),
              bundle_contract = .stagedBundleContract(
                plan, out_name, staging_token
              )
            )
            staged_descriptors[[msdata_name]] <- msdata_desc
            staged_descriptors[[transition_name]] <- transition_desc
            results[[out_name]] <- list(
              msdata = msdata_desc,
              transition_ref = transition_desc
            )
          } else if (identical(survival_sql$format, "recurrent_events")) {
            events_name <- paste0(out_name, ".events")
            risk_name <- paste0(out_name, ".risk_sets")
            results[[out_name]] <- list(
              events = stage_survival_component(
                survival_sql$sql, events_name, "events"
              ),
              risk_sets = stage_survival_component(
                survival_sql$components$risk_sets, risk_name, "risk_sets"
              )
            )
          } else {
            desc <- stage_survival_component(survival_sql$sql, out_name)
            results[[out_name]] <- desc
          }
        } else {
          results[[out_name]] <- .executeLongitudinalSurvivalSql(
            handle, survival_sql
          )
        }

      } else if (out_type == "cohort_membership") {
        if (!is.null(custom_filters) && length(custom_filters) > 0) {
          stop("cohort_membership cannot apply event-row filters$custom; ",
               "filter the population before materializing membership.",
               call. = FALSE)
        }
        results[[out_name]] <- .extractCohortMembership(
          handle,
          cohort_table = cohort_table,
          cohort_definition_id = plan$cohort$cohort_definition_id,
          date_handling = out$date_handling
        )

      } else if (out_type == "intervals_long") {
        source_filters <- out$source_filters
        if (!is.null(custom_filters) && length(custom_filters) > 0L) {
          source_names <- tolower(names(source_filters) %||% character(0))
          source_filters <- stats::setNames(lapply(out$tables, function(table) {
            index <- match(tolower(table), source_names)
            table_filter <- if (is.na(index)) NULL else source_filters[[index]]
            if (is.null(table_filter) || length(table_filter) == 0L) {
              custom_filters
            } else {
              list(and = list(table_filter, custom_filters))
            }
          }), out$tables)
        }
        interval_sql <- .compileIntervalsLongSql(
          handle = handle,
          cohort_table = cohort_table,
          tables = out$tables,
          concept_filter = out$concept_filter,
          filters = source_filters,
          window = out$window,
          interval_match = out$interval_match %||% "overlaps",
          event_select = out$event_select %||% "all",
          select_n = out$select_n %||% 1L,
          select_by = out$select_by %||% "episode_source",
          anchor = out$anchor %||% 0L
        )
        if (staged) {
          interval_chunk <- function(chunk) {
            chunk <- .convertTypes(chunk)
            .pseudonymizeIdentifiers(
              chunk, person_key, pseudonymization = pseudonymization
            )
          }
          file_info <- .executeQueryToParquet(
            .conn(handle), interval_sql,
            file.path(staging_dir, paste0(out_name, ".parquet")),
            chunk_fn = interval_chunk
          )
          desc <- .buildStagedDescriptor(
            out_name, file_info, staging_token,
            pseudonymization = pseudonymization,
            semantic_contract = .stagedSemanticContract(plan, out_name),
            bundle_contract = .stagedBundleContract(
              plan, out_name, staging_token
            )
          )
          results[[out_name]] <- desc
          staged_descriptors[[out_name]] <- desc
        } else {
          results[[out_name]] <- .convertTypes(.executeQuery(
            handle, interval_sql
          ))
        }

      } else if (out_type == "temporal_covariates") {
        if (staged) {
          results[[out_name]] <- .compileTemporalSqlComponents(
            handle = handle,
            cohort_table = cohort_table,
            table = out$table,
            concept_filter = out$concept_set,
            bin_width = out$bin_width %||% 30L,
            window_start = out$window_start %||% -365L,
            window_end = out$window_end %||% 0L,
            analyses = out$analyses %||% c("binary"),
            filters = custom_filters,
            output_type = "temporal_covariates"
          )
        } else {
          results[[out_name]] <- .extractTemporalCovariates(
            handle,
            cohort_table = cohort_table,
            table = out$table,
            concept_filter = out$concept_set,
            bin_width = out$bin_width %||% 30L,
            window_start = out$window_start %||% -365L,
            window_end = out$window_end %||% 0L,
            analyses = out$analyses %||% c("binary"),
            filters = custom_filters
          )
        }
      } else if (out_type == "person_period") {
        if (staged) {
          results[[out_name]] <- .compileTemporalSqlComponents(
            handle = handle,
            cohort_table = cohort_table,
            table = out$table,
            concept_filter = out$concept_set,
            bin_width = out$bin_width %||% 30L,
            window_start = out$window_start %||% -365L,
            window_end = out$window_end %||% 0L,
            analyses = out$analyses %||% c("binary"),
            filters = custom_filters,
            output_type = "person_period",
            grain = out$grain,
            time_origin = out$time_origin
          )
        } else {
          results[[out_name]] <- .extractPersonPeriod(
            handle,
            cohort_table = cohort_table,
            table = out$table,
            concept_filter = out$concept_set,
            bin_width = out$bin_width %||% 30L,
            window_start = out$window_start %||% -365L,
            window_end = out$window_end %||% 0L,
            analyses = out$analyses %||% c("binary"),
            grain = out$grain,
            time_origin = out$time_origin,
            filters = custom_filters
          )
        }
      } else {
        stop("Unsupported output type '", out_type, "'.", call. = FALSE)
      }
    }, error = function(e) {
      if (isTRUE(.omopDisclosureSettings()$query_strict)) {
        cleanup_plan_temps(remove_staging = TRUE)
        stop(e)
      }
      results[[out_name]] <<- NULL
      warning("Plan output '", out_name, "' failed: ", e$message)
    })
  }

  # Second pass: process concept_dictionary outputs (need completed results)
  for (out_name in names(outputs)) {
    out <- outputs[[out_name]]
    if ((out$type %||% "event_level") != "concept_dictionary") next

    tryCatch({
      custom_filters <- out$filters$custom
      if (!is.null(custom_filters) && length(custom_filters) > 0) {
        stop("concept_dictionary is derived from other outputs and cannot ",
             "apply filters$custom directly.", call. = FALSE)
      }
      results[[out_name]] <- if (staged) {
        .buildDeclaredConceptDictionary(
          handle,
          outputs = outputs,
          source_outputs = out$source_outputs
        )
      } else {
        .buildConceptDictionary(
          handle,
          results = results,
          source_outputs = out$source_outputs
        )
      }
    }, error = function(e) {
      if (isTRUE(.omopDisclosureSettings()$query_strict)) {
        cleanup_plan_temps(remove_staging = TRUE)
        stop(e)
      }
      results[[out_name]] <<- NULL
      warning("Plan output '", out_name, "' failed: ", e$message)
    })
  }

  # Staged mode: convert remaining data.frame results to descriptors
  if (staged && !is.null(staging_dir)) {
    for (out_name in names(results)) {
      result <- results[[out_name]]
      if (is.null(result)) next

      # Already a descriptor (streamed directly to Parquet above)
      if (inherits(result, "FlowerDatasetDescriptor")) next

      # Stage data.frame results
      if (is.data.frame(result)) {
        desc <- .stageDataFrame(
          result, out_name, staging_dir, staging_token, person_key,
          pseudonymization = pseudonymization,
          semantic_contract = .stagedSemanticContract(plan, out_name),
          bundle_contract = .stagedBundleContract(
            plan, out_name, staging_token
          )
        )
        results[[out_name]] <- desc
        staged_descriptors[[out_name]] <- desc
      } else if (inherits(result, "omop_temporal_sql_components")) {
        for (validation in result$validations) {
          value <- .executeQuery(handle, validation$sql)[[1L]][[1L]]
          if (identical(validation$kind, "min_persons")) {
            .assertMinPersons(n_persons = value)
          } else if (identical(validation$kind, "max_value")) {
            numeric_value <- suppressWarnings(as.numeric(value))
            if (length(numeric_value) != 1L || is.na(numeric_value) ||
                !is.finite(numeric_value) || numeric_value > validation$max) {
              stop(validation$label, " exceed the server cap of ",
                   validation$max, ".", call. = FALSE)
            }
          } else {
            stop("Unknown SQL component validation.", call. = FALSE)
          }
        }
        component_results <- list()
        for (comp_name in names(result$components)) {
          component <- result$components[[comp_name]]
          full_name <- paste0(out_name, ".", comp_name)
          if (identical(component$kind, "sql")) {
            component_transform <- local({
              contract <- component
              function(chunk) {
                chunk <- .normalizeTemporalSqlChunk(chunk, contract)
                chunk <- .convertTypes(chunk)
                .pseudonymizeIdentifiers(
                  chunk, person_key, pseudonymization = pseudonymization
                )
              }
            })
            file_info <- .executeQueryToParquet(
              .conn(handle), component$sql,
              file.path(staging_dir, paste0(full_name, ".parquet")),
              chunk_fn = component_transform
            )
            desc <- .buildStagedDescriptor(
              full_name, file_info, staging_token,
              pseudonymization = pseudonymization,
              semantic_contract = .stagedSemanticContract(
                plan, out_name, component = comp_name
              ),
              bundle_contract = .stagedBundleContract(
                plan, out_name, staging_token
              )
            )
          } else if (identical(component$kind, "data") &&
                     is.data.frame(component$data)) {
            desc <- .stageDataFrame(
              component$data, full_name, staging_dir, staging_token,
              person_key, pseudonymization = pseudonymization,
              semantic_contract = .stagedSemanticContract(
                plan, out_name, component = comp_name
              ),
              bundle_contract = .stagedBundleContract(
                plan, out_name, staging_token
              )
            )
          } else {
            stop("Invalid SQL-backed temporal component.", call. = FALSE)
          }
          component_results[[comp_name]] <- desc
          staged_descriptors[[full_name]] <- desc
        }
        results[[out_name]] <- component_results
      } else if (is.list(result) && !is.data.frame(result)) {
        # For composite results (temporal covariates, sparse), stage each
        # data.frame component
        for (comp_name in names(result)) {
          if (is.data.frame(result[[comp_name]])) {
            full_name <- paste0(out_name, ".", comp_name)
            desc <- .stageDataFrame(
              result[[comp_name]], full_name, staging_dir, staging_token,
              person_key, pseudonymization = pseudonymization,
              semantic_contract = .stagedSemanticContract(
                plan, out_name, component = comp_name
              ),
              bundle_contract = .stagedBundleContract(
                plan, out_name, staging_token
              )
            )
            result[[comp_name]] <- desc
            staged_descriptors[[full_name]] <- desc
          }
        }
        results[[out_name]] <- result
      }
    }

    if (length(staged_descriptors) > 0) {
      .writeStagingManifest(staging_dir, staged_descriptors)
      staging_committed <- TRUE
    }
  }

  # Drop working tables only after every SQL-backed staged component has been
  # consumed. The operation-level on.exit additionally releases intermediates
  # created before cleanup_plan_temps was installed.
  cleanup_plan_temps()

  attr(results, "omop_concept_cols") <- concept_cols_by_output
  results
}

#' Infer the main date column for a table (plan helper)
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @return Character; date column name, or NULL
#' @keywords internal
.inferDateColumn <- function(handle, table) {
  bp <- .buildBlueprint(handle)
  .getDateColumn(bp, tolower(table))
}
