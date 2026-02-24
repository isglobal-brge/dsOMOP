# ==============================================================================
# dsOMOP v2 - Plan Validation, Preview, Execution
# ==============================================================================
# Uses blueprint + vocabulary for plan processing.
# ==============================================================================

#' Materialize a concept set as a temp table (for large sets)
#'
#' @param handle CDM handle
#' @param concept_ids Integer vector; concept IDs
#' @return Character; temp table name, or NULL if inline is fine
#' @keywords internal
.materializeConceptSet <- function(handle, concept_ids) {
  if (is.null(concept_ids) || length(concept_ids) <= 50) return(NULL)

  temp_name <- paste0("dsomop_cs_",
                       paste0(sample(c(0:9, letters[1:6]), 8, TRUE),
                              collapse = ""))
  ids_str <- paste(as.integer(concept_ids), collapse = " UNION ALL SELECT ")
  sql <- paste0("SELECT ", ids_str, " AS concept_id")
  .createTempTable(handle, temp_name, sql)
  temp_name
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
      results_tables <- if (!is.null(handle$results_schema)) {
        .listTablesRaw(handle, handle$results_schema)
      } else {
        present_tables
      }
      if (!"cohort" %in% results_tables) {
        warnings <- c(warnings,
          "Cohort table not found; cohort filter will be skipped.")
      }
    }
  }

  outputs <- plan$outputs %||% list()
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
        req_cols <- tolower(out$tables[[tbl_name]])
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
    }

    if (out_type == "survival") {
      outcome_tbl <- tolower(out$outcome$table %||% "")
      if (outcome_tbl != "" && !outcome_tbl %in% present_tables) {
        errors <- c(errors,
          paste0("Output '", out_name,
                 "': outcome table '", outcome_tbl, "' not found."))
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
          warnings <- c(warnings,
            paste0("Output '", out_name,
                   "': table '", itbl, "' not found; will be skipped."))
        }
      }
    }

    if (out_type == "temporal_covariates") {
      tc_table <- tolower(out$table %||% "")
      if (tc_table != "" && !tc_table %in% present_tables) {
        errors <- c(errors,
          paste0("Output '", out_name,
                 "': table '", tc_table, "' not found."))
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

#' Preview a plan (safe aggregate)
#'
#' @param handle CDM handle
#' @param plan List; the extraction plan
#' @return List with per-output preview info
#' @keywords internal
.planPreview <- function(handle, plan) {
  bp <- .buildBlueprint(handle)
  validation <- .planValidate(handle, plan)
  settings <- .omopDisclosureSettings()

  preview <- list(
    validation = validation,
    outputs = list()
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

        col_df <- bp$columns[[tbl_lower]]
        req_cols <- tolower(out$tables[[tbl_name]])
        avail_cols <- intersect(req_cols, col_df$column_name)

        if ("person_id" %in% col_df$column_name) {
          sql <- paste0("SELECT COUNT(DISTINCT person_id) AS n FROM ",
                        tbl_row$qualified_name[1])
          n <- .executeQuery(handle, sql)$n[1]
          disclosive <- !is.na(n) && n < settings$nfilter_subset
        } else {
          n <- NA_real_
          disclosive <- FALSE
        }

        out_preview$tables[[tbl_name]] <- list(
          columns = avail_cols,
          missing_columns = setdiff(req_cols, col_df$column_name),
          n_persons = if (disclosive) NA_real_ else n,
          disclosive = disclosive
        )
      }
    }

    if (out_type == "event_level") {
      tbl_lower <- tolower(out$table %||% "")
      tbl_row <- bp$tables[bp$tables$table_name == tbl_lower & bp$tables$present_in_db, ,
                           drop = FALSE]
      if (nrow(tbl_row) > 0) {
        col_df <- bp$columns[[tbl_lower]]
        req_cols <- tolower(out$columns %||% col_df$column_name)
        avail_cols <- intersect(req_cols, col_df$column_name)

        if ("person_id" %in% col_df$column_name) {
          sql <- paste0("SELECT COUNT(DISTINCT person_id) AS n FROM ",
                        tbl_row$qualified_name[1])
          n <- .executeQuery(handle, sql)$n[1]
          disclosive <- !is.na(n) && n < settings$nfilter_subset
        } else {
          n <- NA_real_
          disclosive <- FALSE
        }

        out_preview[[out_name]] <- list(
          table = out$table,
          columns = avail_cols,
          representation = out$representation$format %||% "long",
          n_persons = if (disclosive) NA_real_ else n,
          disclosive = disclosive
        )
      }
    }

    if (out_type == "baseline") {
      out_preview$columns <- out$columns %||%
        c("gender_concept_id", "year_of_birth", "race_concept_id")
      out_preview$derived <- out$derived %||% character(0)
      out_preview$description <- "One row per cohort member with demographics"
    }

    if (out_type == "survival") {
      out_preview$outcome_table <- out$outcome$table %||% ""
      out_preview$outcome_concepts <- out$outcome$concept_set %||% integer(0)
      out_preview$tar <- out$tar %||% list(start_offset = 0)
      out_preview$description <- "Time-to-event with event/censoring indicator"
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
      out_preview$description <- paste0(
        "Interval data from ",
        length(out$tables %||% character(0)), " tables"
      )
    }

    if (out_type == "temporal_covariates") {
      out_preview$table <- out$table
      out_preview$bin_width <- out$bin_width %||% 30L
      out_preview$window <- list(
        start = out$window_start %||% -365L,
        end = out$window_end %||% 0L
      )
      out_preview$description <- paste0(
        "Time-binned covariates from ", out$table
      )
    }

    preview$outputs[[out_name]] <- out_preview
  }

  preview
}

#' Execute a plan and produce server-side data frames
#'
#' @param handle CDM handle
#' @param plan List; the extraction plan
#' @param out_symbols Named list; output name -> R symbol mapping
#' @return Named list of data frames
#' @keywords internal
.planExecute <- function(handle, plan, out_symbols) {
  bp <- .buildBlueprint(handle)

  cohort_table <- NULL
  cohort_person_ids <- NULL

  if (!is.null(plan$cohort)) {
    if (!is.null(plan$cohort$type) && plan$cohort$type == "cohort_table") {
      cid <- as.integer(plan$cohort$cohort_definition_id)
      results_schema <- handle$results_schema %||% handle$cdm_schema
      qualified <- .qualifyTable(handle, "cohort", results_schema)

      cohort_sql <- paste0(
        "SELECT DISTINCT subject_id, cohort_start_date, cohort_end_date",
        " FROM ", qualified,
        " WHERE cohort_definition_id = ", cid
      )
      cohort_table <- .createTempTable(handle, "dsomop_plan_cohort", cohort_sql)

      pid_result <- .executeQuery(handle,
        paste0("SELECT DISTINCT subject_id AS person_id FROM ",
               cohort_table))
      cohort_person_ids <- pid_result$person_id

      .assertMinPersons(n_persons = length(unique(cohort_person_ids)))

    } else if (!is.null(plan$cohort$spec)) {
      cohort_table <- .cohortCreate(
        handle, plan$cohort$spec, mode = "temporary",
        cohort_id = plan$cohort$cohort_definition_id)

      pid_result <- .executeQuery(handle,
        paste0("SELECT DISTINCT subject_id AS person_id FROM ", cohort_table))
      cohort_person_ids <- pid_result$person_id
    }
  }

  results <- list()
  outputs <- plan$outputs %||% list()
  options <- plan$options %||% list()
  translate <- options$translate_concepts %||% FALSE
  block_sensitive <- options$block_sensitive %||% TRUE

  # Concept expansion cache: expand each unique concept set once
  concept_cache <- new.env(parent = emptyenv())
  for (out_name_pre in names(outputs)) {
    out_pre <- outputs[[out_name_pre]]
    cs <- out_pre$filters$concept_set$ids %||% out_pre$concept_set
    if (is.list(cs) && !is.null(cs$concepts)) {
      key <- paste(sort(cs$concepts), collapse = ",")
      if (!exists(key, envir = concept_cache)) {
        expanded <- tryCatch(
          .vocabExpandConceptSet(handle, cs),
          error = function(e) cs$concepts
        )
        assign(key, expanded, envir = concept_cache)
      }
    }
  }

  # Track materialized concept set temp tables for cleanup
  cs_temp_tables <- character(0)

  # First pass: process all non-dictionary outputs
  for (out_name in names(outputs)) {
    out <- outputs[[out_name]]
    out_type <- out$type %||% "event_level"

    # Skip concept_dictionary for second pass
    if (out_type == "concept_dictionary") next

    tryCatch({
      if (out_type == "person_level") {
        result_df <- NULL

        for (tbl_name in names(out$tables %||% list())) {
          cols <- out$tables[[tbl_name]]
          tbl_df <- .extractTable(
            handle,
            table = tbl_name,
            columns = cols,
            person_ids = cohort_person_ids,
            translate_concepts = translate,
            representation = "long",
            block_sensitive = block_sensitive
          )

          if (is.null(result_df)) {
            result_df <- tbl_df
          } else if ("person_id" %in% names(tbl_df) &&
                     "person_id" %in% names(result_df)) {
            result_df <- merge(result_df, tbl_df,
                               by = "person_id", all.x = TRUE)
          }
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

        # Use concept cache if available, otherwise expand
        if (is.list(concept_set) && !is.null(concept_set$concepts)) {
          key <- paste(sort(concept_set$concepts), collapse = ",")
          if (exists(key, envir = concept_cache)) {
            concept_set <- get(key, envir = concept_cache)
          } else {
            concept_set <- tryCatch(
              .vocabExpandConceptSet(handle, concept_set),
              error = function(e) concept_set$concepts
            )
          }
        }

        # Materialize large concept sets as temp tables
        cs_temp <- .materializeConceptSet(handle, concept_set)
        if (!is.null(cs_temp)) {
          cs_temp_tables <- c(cs_temp_tables, cs_temp)
        }

        # Add cohort date when index_window is active (for days_from_index)
        add_cohort_date <- !is.null(cohort_table) &&
          !is.null(out$temporal$index_window)

        result_df <- .extractTable(
          handle,
          table = out$table,
          columns = out$columns,
          concept_filter = concept_set,
          person_ids = cohort_person_ids,
          time_window = time_window,
          cohort_table = cohort_table,
          translate_concepts = translate,
          representation = repr,
          feature_specs = out$representation$features,
          block_sensitive = block_sensitive,
          temporal = out$temporal,
          date_handling = out$date_handling,
          add_cohort_date = add_cohort_date
        )

        results[[out_name]] <- result_df

      } else if (out_type == "baseline") {
        if (is.null(cohort_table)) {
          warning("Baseline output '", out_name,
                  "' requires a cohort; skipping.", call. = FALSE)
          results[[out_name]] <- NULL
        } else {
          results[[out_name]] <- .extractBaseline(
            handle,
            cohort_table = cohort_table,
            columns = out$columns,
            derived = out$derived,
            translate_concepts = translate
          )
        }

      } else if (out_type == "survival") {
        if (is.null(cohort_table)) {
          warning("Survival output '", out_name,
                  "' requires a cohort; skipping.", call. = FALSE)
          results[[out_name]] <- NULL
        } else {
          results[[out_name]] <- .extractSurvival(
            handle,
            cohort_table = cohort_table,
            outcome = out$outcome,
            tar = out$tar,
            event_order = out$event_order %||% "first"
          )
        }

      } else if (out_type == "cohort_membership") {
        if (is.null(cohort_table)) {
          warning("Cohort membership output '", out_name,
                  "' requires a cohort; skipping.", call. = FALSE)
          results[[out_name]] <- NULL
        } else {
          results[[out_name]] <- .extractCohortMembership(
            handle,
            cohort_table = cohort_table,
            cohort_definition_id = plan$cohort$cohort_definition_id
          )
        }

      } else if (out_type == "intervals_long") {
        if (is.null(cohort_table)) {
          warning("Intervals output '", out_name,
                  "' requires a cohort; skipping.", call. = FALSE)
          results[[out_name]] <- NULL
        } else {
          results[[out_name]] <- .extractIntervalsLong(
            handle,
            cohort_table = cohort_table,
            tables = out$tables,
            concept_filter = out$concept_filter
          )
        }

      } else if (out_type == "temporal_covariates") {
        if (is.null(cohort_table)) {
          warning("Temporal covariates output '", out_name,
                  "' requires a cohort; skipping.", call. = FALSE)
          results[[out_name]] <- NULL
        } else {
          results[[out_name]] <- .extractTemporalCovariates(
            handle,
            cohort_table = cohort_table,
            table = out$table,
            concept_filter = out$concept_set,
            bin_width = out$bin_width %||% 30L,
            window_start = out$window_start %||% -365L,
            window_end = out$window_end %||% 0L,
            analyses = out$analyses %||% c("binary")
          )
        }
      }
    }, error = function(e) {
      results[[out_name]] <<- NULL
      warning("Plan output '", out_name, "' failed: ", e$message)
    })
  }

  # Second pass: process concept_dictionary outputs (need completed results)
  for (out_name in names(outputs)) {
    out <- outputs[[out_name]]
    if ((out$type %||% "event_level") != "concept_dictionary") next

    tryCatch({
      results[[out_name]] <- .buildConceptDictionary(
        handle,
        results = results,
        source_outputs = out$source_outputs
      )
    }, error = function(e) {
      results[[out_name]] <<- NULL
      warning("Plan output '", out_name, "' failed: ", e$message)
    })
  }

  if (!is.null(cohort_table)) {
    .dropTempTable(handle, cohort_table)
  }

  # Clean up materialized concept set temp tables
  for (cs_tbl in cs_temp_tables) {
    .dropTempTable(handle, cs_tbl)
  }

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
