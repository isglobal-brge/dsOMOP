# Module: Extraction Plan
# Plan construction, validation, preview, and execution for multi-table extractions.

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

#' Build a cohort person_id set from population-level filters
#'
#' Translates filter specs (sex, age_range, age_group, has_concept) into
#' SQL WHERE clauses on the person table and returns matching person IDs.
#'
#' @param handle CDM handle
#' @param filters List of filter specs from recipe_to_plan
#' @return Integer vector of person_ids
#' @keywords internal
.buildCohortFromFilters <- function(handle, filters) {
  bp <- .buildBlueprint(handle)

  person_table <- bp$tables[bp$tables$table_name == "person" &
                              bp$tables$present_in_db, , drop = FALSE]
  if (nrow(person_table) == 0) return(integer(0))

  qualified_person <- person_table$qualified_name[1]
  person_cols <- bp$columns[["person"]]$column_name

  where_parts <- character(0)

  for (f in filters) {
    ftype <- f$type
    params <- f$params

    if (ftype == "sex") {
      gender_id <- switch(toupper(params$value),
        "F" = 8532L, "FEMALE" = 8532L,
        "M" = 8507L, "MALE" = 8507L,
        NULL)
      if (!is.null(gender_id) && "gender_concept_id" %in% person_cols) {
        where_parts <- c(where_parts,
          paste0("p.gender_concept_id = ", gender_id))
      }

    } else if (ftype == "age_range") {
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      if (!is.null(params$min) && "year_of_birth" %in% person_cols) {
        max_yob <- current_year - as.integer(params$min)
        where_parts <- c(where_parts,
          paste0("p.year_of_birth <= ", max_yob))
      }
      if (!is.null(params$max) && "year_of_birth" %in% person_cols) {
        min_yob <- current_year - as.integer(params$max)
        where_parts <- c(where_parts,
          paste0("p.year_of_birth >= ", min_yob))
      }

    } else if (ftype == "age_group") {
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      groups <- params$groups
      if (length(groups) > 0 && "year_of_birth" %in% person_cols) {
        band_parts <- character(0)
        for (g in groups) {
          parts <- strsplit(g, "-")[[1]]
          if (length(parts) == 2) {
            min_yob <- current_year - as.integer(parts[2])
            max_yob <- current_year - as.integer(parts[1])
            band_parts <- c(band_parts,
              paste0("(p.year_of_birth BETWEEN ", min_yob,
                     " AND ", max_yob, ")"))
          }
        }
        if (length(band_parts) > 0) {
          where_parts <- c(where_parts,
            paste0("(", paste(band_parts, collapse = " OR "), ")"))
        }
      }

    } else if (ftype == "has_concept") {
      concept_id <- as.integer(params$concept_id)
      table_name <- params$table
      tbl_row <- bp$tables[bp$tables$table_name == table_name &
                             bp$tables$present_in_db, , drop = FALSE]
      if (nrow(tbl_row) > 0) {
        concept_col <- .getDomainConceptColumn(bp, table_name)
        qualified_tbl <- tbl_row$qualified_name[1]
        where_parts <- c(where_parts,
          paste0("EXISTS (SELECT 1 FROM ", qualified_tbl, " t",
                 " WHERE t.person_id = p.person_id",
                 " AND t.", concept_col, " = ", concept_id, ")"))
      }

    } else if (ftype == "not_has_concept") {
      concept_id <- as.integer(params$concept_id)
      table_name <- params$table
      tbl_row <- bp$tables[bp$tables$table_name == table_name &
                             bp$tables$present_in_db, , drop = FALSE]
      if (nrow(tbl_row) > 0) {
        concept_col <- .getDomainConceptColumn(bp, table_name)
        qualified_tbl <- tbl_row$qualified_name[1]
        where_parts <- c(where_parts,
          paste0("NOT EXISTS (SELECT 1 FROM ", qualified_tbl, " t",
                 " WHERE t.person_id = p.person_id",
                 " AND t.", concept_col, " = ", concept_id, ")"))
      }

    } else if (ftype == "concept_count") {
      concept_id <- as.integer(params$concept_id)
      min_count <- as.integer(params$min_count %||% 1L)
      table_name <- params$table
      tbl_row <- bp$tables[bp$tables$table_name == table_name &
                             bp$tables$present_in_db, , drop = FALSE]
      if (nrow(tbl_row) > 0) {
        concept_col <- .getDomainConceptColumn(bp, table_name)
        qualified_tbl <- tbl_row$qualified_name[1]
        where_parts <- c(where_parts,
          paste0("(SELECT COUNT(*) FROM ", qualified_tbl, " t",
                 " WHERE t.person_id = p.person_id",
                 " AND t.", concept_col, " = ", concept_id,
                 ") >= ", min_count))
      }

    } else if (ftype == "prior_observation") {
      min_days <- as.integer(params$min_days %||% 365L)
      op_row <- bp$tables[bp$tables$table_name == "observation_period" &
                             bp$tables$present_in_db, , drop = FALSE]
      if (nrow(op_row) > 0) {
        op_qualified <- op_row$qualified_name[1]
        where_parts <- c(where_parts,
          paste0("EXISTS (SELECT 1 FROM ", op_qualified, " op",
                 " WHERE op.person_id = p.person_id",
                 " AND (CURRENT_DATE - op.observation_period_start_date) >= ",
                 min_days, ")"))
      }

    } else if (ftype == "followup") {
      min_days <- as.integer(params$min_days %||% 30L)
      op_row <- bp$tables[bp$tables$table_name == "observation_period" &
                             bp$tables$present_in_db, , drop = FALSE]
      if (nrow(op_row) > 0) {
        op_qualified <- op_row$qualified_name[1]
        where_parts <- c(where_parts,
          paste0("EXISTS (SELECT 1 FROM ", op_qualified, " op",
                 " WHERE op.person_id = p.person_id",
                 " AND (op.observation_period_end_date - CURRENT_DATE) >= ",
                 min_days, ")"))
      }

    } else if (ftype == "visit_count") {
      min_count <- as.integer(params$min_count %||% 1L)
      vo_row <- bp$tables[bp$tables$table_name == "visit_occurrence" &
                             bp$tables$present_in_db, , drop = FALSE]
      if (nrow(vo_row) > 0) {
        vo_qualified <- vo_row$qualified_name[1]
        sub_where <- paste0(" WHERE v.person_id = p.person_id")
        if (!is.null(params$visit_concept_id)) {
          sub_where <- paste0(sub_where,
            " AND v.visit_concept_id = ",
            as.integer(params$visit_concept_id))
        }
        where_parts <- c(where_parts,
          paste0("(SELECT COUNT(*) FROM ", vo_qualified, " v",
                 sub_where, ") >= ", min_count))
      }

    } else if (ftype == "has_measurement") {
      concept_id <- as.integer(params$concept_id)
      m_row <- bp$tables[bp$tables$table_name == "measurement" &
                            bp$tables$present_in_db, , drop = FALSE]
      if (nrow(m_row) > 0) {
        m_qualified <- m_row$qualified_name[1]
        sub_where <- paste0(
          " WHERE m.person_id = p.person_id",
          " AND m.measurement_concept_id = ", concept_id)
        if (!is.null(params$min_value)) {
          sub_where <- paste0(sub_where,
            " AND m.value_as_number >= ", as.numeric(params$min_value))
        }
        if (!is.null(params$max_value)) {
          sub_where <- paste0(sub_where,
            " AND m.value_as_number <= ", as.numeric(params$max_value))
        }
        where_parts <- c(where_parts,
          paste0("EXISTS (SELECT 1 FROM ", m_qualified, " m",
                 sub_where, ")"))
      }

    } else if (ftype == "missing_measurement") {
      concept_id <- as.integer(params$concept_id)
      m_row <- bp$tables[bp$tables$table_name == "measurement" &
                            bp$tables$present_in_db, , drop = FALSE]
      if (nrow(m_row) > 0) {
        m_qualified <- m_row$qualified_name[1]
        where_parts <- c(where_parts,
          paste0("NOT EXISTS (SELECT 1 FROM ", m_qualified, " m",
                 " WHERE m.person_id = p.person_id",
                 " AND m.measurement_concept_id = ", concept_id,
                 " AND m.value_as_number IS NOT NULL)"))
      }
    }
  }

  sql <- paste0("SELECT DISTINCT p.person_id FROM ", qualified_person, " p")
  if (length(where_parts) > 0) {
    sql <- paste0(sql, " WHERE ", paste(where_parts, collapse = " AND "))
  }

  result <- .executeQuery(handle, sql)
  if (nrow(result) > 0) result$person_id else integer(0)
}

#' Generate a unique staging token
#'
#' @return Character; staging token
#' @keywords internal
.generateStagingToken <- function() {
  paste0("stg_",
         format(Sys.time(), "%Y%m%d_%H%M%S"),
         "_", Sys.getpid(),
         "_", paste0(sample(c(0:9, letters[1:6]), 4, TRUE), collapse = ""))
}

#' Get the staging base directory
#'
#' @return Character; path to staging base directory
#' @keywords internal
.stagingBaseDir <- function() {
  base <- getOption("dsstaging.base_dir", file.path(tempdir(), "dsstaging"))
  if (!dir.exists(base)) {
    dir.create(base, recursive = TRUE, showWarnings = FALSE)
    Sys.chmod(base, mode = "0700")
  }
  base
}

#' Create a staging directory for a token
#'
#' @param token Character; staging token
#' @return Character; path to the staging directory
#' @keywords internal
.createStagingDir <- function(token) {
  staging_dir <- file.path(.stagingBaseDir(), token)
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  Sys.chmod(staging_dir, mode = "0700")
  staging_dir
}

#' Build a FlowerDatasetDescriptor for a staged output
#'
#' @param output_name Character; name of the output
#' @param file_info Named list from .executeQueryToParquet
#' @param token Character; staging token
#' @param origin Character; origin package identifier
#' @return Named list (FlowerDatasetDescriptor)
#' @keywords internal
.buildStagedDescriptor <- function(output_name, file_info, token,
                                    origin = "dsOMOP") {
  desc <- list(
    dataset_id  = paste0("omop.plan.", output_name),
    source_kind = "staged_parquet",
    metadata    = list(
      file    = file_info$file,
      format  = file_info$format,
      n_rows  = file_info$n_rows,
      columns = file_info$columns
    ),
    staged_token = token,
    origin       = origin
  )
  class(desc) <- "FlowerDatasetDescriptor"
  desc
}

#' Write a staging manifest
#'
#' @param staging_dir Character; path to staging directory
#' @param descriptors Named list of descriptors
#' @keywords internal
.writeStagingManifest <- function(staging_dir, descriptors) {
  manifest <- list(
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    outputs = lapply(descriptors, function(d) {
      list(
        dataset_id = d$dataset_id,
        file       = d$metadata$file,
        format     = d$metadata$format,
        n_rows     = d$metadata$n_rows
      )
    })
  )
  manifest_path <- file.path(staging_dir, "manifest.json")
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE),
             manifest_path)
  Sys.chmod(manifest_path, mode = "0600")
  invisible(manifest_path)
}

#' Stage a data.frame result to Parquet and return a descriptor
#'
#' For output types where SQL cannot be streamed directly (e.g. baseline,
#' survival), this writes an already-materialized data.frame to Parquet.
#'
#' @param df Data frame to stage
#' @param output_name Character; output name
#' @param staging_dir Character; path to staging directory
#' @param token Character; staging token
#' @return FlowerDatasetDescriptor or the original data.frame if arrow unavailable
#' @keywords internal
.stageDataFrame <- function(df, output_name, staging_dir, token) {
  use_parquet <- requireNamespace("arrow", quietly = TRUE)
  ext <- if (use_parquet) "parquet" else "csv"
  output_path <- file.path(staging_dir, paste0(output_name, ".", ext))

  # Ensure staging directory exists (may have been cleaned between calls)
  if (!dir.exists(staging_dir)) {
    dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
    Sys.chmod(staging_dir, mode = "0700")
  }

  if (use_parquet) {
    arrow::write_parquet(df, output_path)
  } else {
    utils::write.csv(df, output_path, row.names = FALSE)
  }
  Sys.chmod(output_path, mode = "0600")

  file_info <- list(
    file    = output_path,
    format  = ext,
    n_rows  = nrow(df),
    columns = names(df)
  )
  .buildStagedDescriptor(output_name, file_info, token)
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
#' @return Named list of data frames or FlowerDatasetDescriptors
#' @keywords internal
.planExecute <- function(handle, plan, out_symbols, output_mode = "memory") {
  bp <- .buildBlueprint(handle)

  staged <- identical(output_mode, "staged")
  staging_dir <- NULL
  staging_token <- NULL
  staged_descriptors <- list()

  if (staged) {
    staging_token <- .generateStagingToken()
    staging_dir <- .createStagingDir(staging_token)
    on.exit({
      # Clean up staging dir if no descriptors were produced (error path)
      if (length(staged_descriptors) == 0L && !is.null(staging_dir) &&
          dir.exists(staging_dir)) {
        unlink(staging_dir, recursive = TRUE)
      }
    }, add = TRUE)
  }

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
      spec <- plan$cohort$spec

      # Check if spec is a list of population filters (from cart_to_plan)
      is_filter_list <- is.list(spec) && length(spec) > 0 &&
        !is.null(spec[[1]]) && is.list(spec[[1]]) &&
        !is.null(spec[[1]]$type) &&
        spec[[1]]$type %in% c("sex", "age_range", "age_group",
                               "has_concept", "date_range", "value_threshold",
                               "not_has_concept", "concept_count",
                               "prior_observation", "followup",
                               "visit_count", "has_measurement",
                               "missing_measurement")

      if (is_filter_list) {
        cohort_person_ids <- .buildCohortFromFilters(handle, spec)
        # Materialize a cohort temp table so baseline/survival outputs work
        if (length(cohort_person_ids) > 0) {
          obs_table <- bp$tables[bp$tables$table_name == "observation_period" &
                                   bp$tables$present_in_db, , drop = FALSE]
          if (nrow(obs_table) > 0) {
            obs_qualified <- obs_table$qualified_name[1]
            ids_str <- paste(as.integer(cohort_person_ids), collapse = ", ")
            cohort_sql <- paste0(
              "SELECT DISTINCT o.person_id AS subject_id, ",
              "o.observation_period_start_date AS cohort_start_date, ",
              "o.observation_period_end_date AS cohort_end_date ",
              "FROM ", obs_qualified, " o ",
              "WHERE o.person_id IN (", ids_str, ")"
            )
            cohort_table <- .createTempTable(
              handle, "dsomop_plan_cohort", cohort_sql)
          }
        }
        .assertMinPersons(n_persons = length(unique(cohort_person_ids)))
      } else {
        # Single concept-based spec: use existing cohortCreate
        cohort_table <- .cohortCreate(
          handle, spec, mode = "temporary",
          cohort_id = plan$cohort$cohort_definition_id)
        pid_result <- .executeQuery(handle,
          paste0("SELECT DISTINCT subject_id AS person_id FROM ",
                 cohort_table))
        cohort_person_ids <- pid_result$person_id
      }
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
        out_repr <- out$representation %||% "long"

        for (tbl_name in names(out$tables %||% list())) {
          entry <- out$tables[[tbl_name]]

          # Check if entry has feature specs (list with $features)
          if (is.list(entry) && !is.null(entry$features)) {
            tbl_df <- .extractTable(
              handle,
              table = tbl_name,
              columns = NULL,
              concept_filter = entry$concept_set,
              person_ids = cohort_person_ids,
              translate_concepts = translate,
              representation = "features",
              feature_specs = entry$features,
              block_sensitive = block_sensitive
            )
          } else {
            # Original: entry is a character vector of column names
            cols <- if (is.character(entry)) entry else NULL
            tbl_df <- .extractTable(
              handle,
              table = tbl_name,
              columns = cols,
              person_ids = cohort_person_ids,
              translate_concepts = translate,
              representation = "long",
              block_sensitive = block_sensitive
            )
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
            result_df <- merge(result_df, derived_df,
                               by = "person_id", all.x = TRUE)
          } else if (!is.null(derived_df)) {
            result_df <- derived_df
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

        # Staged + long + no translate: stream directly to Parquet
        # This is the zero-memory path for large event-level extractions.
        # Features/wide/sparse need in-memory reshaping, so they fall through.
        can_stream <- staged && repr == "long" && !translate

        if (can_stream) {
          sql <- .compileSelect(
            handle, out$table,
            columns = out$columns,
            concept_filter = concept_set,
            person_ids = cohort_person_ids,
            time_window = time_window,
            cohort_table = cohort_table,
            block_sensitive = block_sensitive,
            temporal = out$temporal,
            add_cohort_date = add_cohort_date
          )

          if (!is.null(out$temporal$event_select)) {
            ev_date_col <- .getDateColumn(bp, tolower(out$table))
            sql <- .wrapEventSelect(handle, sql, out$temporal, ev_date_col)
          }

          # Disclosure check before streaming
          col_df <- bp$columns[[tolower(out$table)]]
          if ("person_id" %in% col_df$column_name) {
            count_sql <- .compilePersonCount(handle, sql)
            .assertMinPersons(conn = handle$conn, sql = count_sql)
          }

          # Build per-chunk transform for date handling + type conversion
          dh <- out$date_handling
          if (is.null(dh)) {
            default_mode <- getOption("dsomop.default_date_handling", "remove")
            dh <- list(mode = default_mode)
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
            if ("cohort_start_date" %in% names(chunk) &&
                !is.null(tbl_date_col) &&
                tbl_date_col %in% names(chunk)) {
              chunk$days_from_index <- as.integer(
                as.Date(chunk[[tbl_date_col]]) -
                as.Date(chunk$cohort_start_date)
              )
              chunk$cohort_start_date <- NULL
            }
            chunk <- .convertTypes(chunk)
            chunk <- .applyDateHandling(chunk, dh)
            chunk
          }

          output_path <- file.path(staging_dir,
                                    paste0(out_name, ".parquet"))
          file_info <- .executeQueryToParquet(
            handle$conn, sql, output_path, chunk_fn = chunk_fn
          )
          desc <- .buildStagedDescriptor(out_name, file_info, staging_token)
          results[[out_name]] <- desc
          staged_descriptors[[out_name]] <- desc

        } else {
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
        }

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

  # Staged mode: convert remaining data.frame results to descriptors
  if (staged && !is.null(staging_dir)) {
    for (out_name in names(results)) {
      result <- results[[out_name]]
      if (is.null(result)) next

      # Already a descriptor (streamed directly to Parquet above)
      if (inherits(result, "FlowerDatasetDescriptor")) next

      # Stage data.frame results
      if (is.data.frame(result)) {
        desc <- .stageDataFrame(result, out_name, staging_dir, staging_token)
        results[[out_name]] <- desc
        staged_descriptors[[out_name]] <- desc
      } else if (is.list(result) && !is.data.frame(result)) {
        # For composite results (temporal covariates, sparse), stage each
        # data.frame component
        for (comp_name in names(result)) {
          if (is.data.frame(result[[comp_name]])) {
            full_name <- paste0(out_name, ".", comp_name)
            desc <- .stageDataFrame(result[[comp_name]], full_name,
                                     staging_dir, staging_token)
            result[[comp_name]] <- desc
            staged_descriptors[[full_name]] <- desc
          }
        }
        results[[out_name]] <- result
      }
    }

    if (length(staged_descriptors) > 0) {
      .writeStagingManifest(staging_dir, staged_descriptors)
    }
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
