# Module: typed federated pooling contracts for the analysis catalog
#
# Pooling is a client operation, but the server is the only component that knows
# the exact semantics of every released column.  Each aggregate catalog entry
# therefore publishes one closed, inert contract.  The client must consume this
# contract literally; it must never infer keys or statistics from column names.

.omopPoolingColumn <- function(role, ...) {
  c(list(role = role), list(...))
}

.omopPoolingStableLabels <- function(columns) {
  # OHDSI concept identifiers are the cross-database identity. Human-readable
  # names are presentation metadata and must not split an otherwise identical
  # federated group when sites use different vocabulary labels.
  label_to_id <- c(
    concept_name = "concept_id",
    covariate_name = "covariate_id",
    measurement_concept_name = "measurement_concept_id",
    unit_concept_name = "unit_concept_id",
    gender_name = "gender_concept_id",
    race_name = "race_concept_id",
    ethnicity_name = "ethnicity_concept_id",
    condition_name = "condition_concept_id",
    drug_name = "drug_concept_id",
    procedure_name = "procedure_concept_id",
    observation_name = "observation_concept_id",
    visit_name = "visit_concept_id",
    visit_type_name = "visit_concept_id",
    route_name = "route_concept_id",
    specialty_name = "specialty_concept_id",
    place_of_service_name = "place_of_service_concept_id",
    cause_name = "cause_concept_id",
    comorbid_name = "comorbid_concept_id",
    concomitant_name = "concomitant_concept_id",
    unit_name = "unit_concept_id",
    measurement_name = "measurement_concept_id",
    treatment = "treatment_concept_id"
  )
  candidates <- intersect(names(label_to_id), columns)
  candidates[unname(label_to_id[candidates]) %in% columns]
}

.omopPoolingTabular <- function(columns, sum_cols = character(0),
                                ratios = list(), weighted_means = list(),
                                pooled_sds = list(), min_cols = character(0),
                                max_cols = character(0),
                                label_cols = character(0),
                                nonpoolable_cols = character(0),
                                nonpoolable_reason =
                                  "No exact cross-site sufficient statistic is released.") {
  columns <- as.character(columns)
  label_cols <- union(label_cols, .omopPoolingStableLabels(columns))
  specs <- stats::setNames(
    lapply(columns, function(x) .omopPoolingColumn("key")), columns
  )
  for (column in intersect(sum_cols, columns)) {
    specs[[column]] <- .omopPoolingColumn("sum")
  }
  for (column in intersect(names(ratios), columns)) {
    x <- ratios[[column]]
    specs[[column]] <- .omopPoolingColumn(
      "ratio", numerator = x$numerator, denominator = x$denominator,
      scale = as.numeric(x$scale %||% 1)
    )
  }
  for (column in intersect(names(weighted_means), columns)) {
    specs[[column]] <- .omopPoolingColumn(
      "weighted_mean", weight = weighted_means[[column]]$weight
    )
  }
  for (column in intersect(names(pooled_sds), columns)) {
    x <- pooled_sds[[column]]
    specs[[column]] <- .omopPoolingColumn(
      "pooled_sd", mean = x$mean, count = x$count
    )
  }
  for (column in intersect(min_cols, columns)) {
    specs[[column]] <- .omopPoolingColumn("min")
  }
  for (column in intersect(max_cols, columns)) {
    specs[[column]] <- .omopPoolingColumn("max")
  }
  for (column in intersect(label_cols, columns)) {
    specs[[column]] <- .omopPoolingColumn("label")
  }
  for (column in intersect(nonpoolable_cols, columns)) {
    specs[[column]] <- .omopPoolingColumn(
      "nonpoolable", reason = nonpoolable_reason
    )
  }
  list(version = 1L, strategy = "tabular", columns = specs)
}

.omopPoolingEffectEstimate <- function(columns, log_estimate, standard_error,
                                       strata = character(0),
                                       sum_cols = character(0),
                                       nonpoolable_cols = character(0)) {
  base <- .omopPoolingTabular(
    columns, sum_cols = sum_cols,
    nonpoolable_cols = unique(c(nonpoolable_cols, log_estimate,
                                standard_error))
  )
  list(
    version = 1L, strategy = "effect_estimate", columns = base$columns,
    log_estimate = log_estimate, standard_error = standard_error,
    transform = "exp", strata = as.character(strata)
  )
}

.omopPoolingKaplanMeier <- function(columns, strata, order, at_risk, events,
                                    survival, order_start = 1L,
                                    order_step = 1L) {
  base <- .omopPoolingTabular(
    columns, sum_cols = c(at_risk, events),
    nonpoolable_cols = survival,
    nonpoolable_reason = "Rebuilt from the pooled risk sets and event counts."
  )
  list(
    version = 1L, strategy = "kaplan_meier", columns = base$columns,
    strata = as.character(strata), order = order, at_risk = at_risk,
    events = events, survival = survival,
    order_start = as.numeric(order_start), order_step = as.numeric(order_step)
  )
}

.omopPoolingNotPoolable <- function(reason) {
  list(version = 1L, strategy = "not_poolable", reason = reason)
}

.omopAnalysisValidatePoolingContract <- function(contract, entry_name = NULL) {
  label <- if (is.null(entry_name)) "Pooling contract" else
    paste0("Pooling contract for '", entry_name, "'")
  fail <- function(...) stop(label, ": ", ..., call. = FALSE)
  if (!is.list(contract) || is.null(names(contract)) ||
      any(!nzchar(names(contract))) || anyDuplicated(names(contract))) {
    fail("must be a uniquely named list.")
  }
  if (!identical(contract$version, 1L)) fail("version must be exactly 1L.")
  strategy <- contract$strategy
  if (!is.character(strategy) || length(strategy) != 1L || is.na(strategy) ||
      !strategy %in% c("tabular", "effect_estimate", "kaplan_meier",
                       "not_poolable")) {
    fail("has an unsupported strategy.")
  }
  required <- switch(
    strategy,
    tabular = c("version", "strategy", "columns"),
    effect_estimate = c("version", "strategy", "columns", "log_estimate",
                        "standard_error", "transform", "strata"),
    kaplan_meier = c("version", "strategy", "columns", "strata", "order",
                     "at_risk", "events", "survival", "order_start",
                     "order_step"),
    not_poolable = c("version", "strategy", "reason")
  )
  if (!setequal(names(contract), required)) {
    fail("fields must be exactly: ", paste(required, collapse = ", "), ".")
  }
  if (identical(strategy, "not_poolable")) {
    if (!is.character(contract$reason) || length(contract$reason) != 1L ||
        is.na(contract$reason) || !nzchar(trimws(contract$reason))) {
      fail("not_poolable requires one non-empty reason.")
    }
    return(contract)
  }

  columns <- contract$columns
  if (!is.list(columns) || length(columns) == 0L || is.null(names(columns)) ||
      any(!nzchar(names(columns))) || anyDuplicated(names(columns))) {
    fail("columns must be a non-empty, uniquely named list.")
  }
  roles <- c("key", "label", "sum", "ratio", "weighted_mean", "pooled_sd",
             "min", "max", "nonpoolable")
  column_names <- names(columns)
  for (column in column_names) {
    spec <- columns[[column]]
    if (!is.list(spec) || is.null(names(spec)) || any(!nzchar(names(spec))) ||
        anyDuplicated(names(spec))) {
      fail("column '", column, "' must be a uniquely named list.")
    }
    role <- spec$role
    if (!is.character(role) || length(role) != 1L || is.na(role) ||
        !role %in% roles) fail("column '", column, "' has an invalid role.")
    allowed <- switch(
      role,
      ratio = c("role", "numerator", "denominator", "scale"),
      weighted_mean = c("role", "weight"),
      pooled_sd = c("role", "mean", "count"),
      nonpoolable = c("role", "reason"),
      "role"
    )
    if (!setequal(names(spec), allowed)) {
      fail("column '", column, "' fields must be exactly: ",
           paste(allowed, collapse = ", "), ".")
    }
    references <- switch(
      role,
      ratio = c(spec$numerator, spec$denominator),
      weighted_mean = spec$weight,
      pooled_sd = c(spec$mean, spec$count),
      character(0)
    )
    if (length(references) > 0L &&
        (anyNA(references) || any(!references %in% column_names))) {
      fail("column '", column, "' references an unknown column.")
    }
    if (identical(role, "ratio")) {
      if (!is.character(spec$numerator) || length(spec$numerator) == 0L ||
          anyNA(spec$numerator) || anyDuplicated(spec$numerator) ||
          !is.character(spec$denominator) || length(spec$denominator) == 0L ||
          anyNA(spec$denominator) || anyDuplicated(spec$denominator)) {
        fail("column '", column,
             "' requires non-empty numerator/denominator column vectors.")
      }
      referenced_roles <- vapply(columns[references], `[[`, character(1L),
                                 "role")
      if (any(referenced_roles != "sum")) {
        fail("column '", column, "' may reference only sum columns.")
      }
      scale <- suppressWarnings(as.numeric(spec$scale))
      if (length(scale) != 1L || is.na(scale) || !is.finite(scale) ||
          scale <= 0) fail("column '", column, "' has an invalid ratio scale.")
    }
    if (identical(role, "weighted_mean") &&
        (!is.character(spec$weight) || length(spec$weight) != 1L)) {
      fail("column '", column, "' requires one weight column.")
    }
    if (identical(role, "weighted_mean") &&
        !identical(columns[[spec$weight]]$role, "sum")) {
      fail("column '", column, "' weight must have role sum.")
    }
    if (identical(role, "pooled_sd") &&
        (!is.character(spec$mean) || length(spec$mean) != 1L ||
         !is.character(spec$count) || length(spec$count) != 1L)) {
      fail("column '", column, "' requires one mean and count column.")
    }
    if (identical(role, "pooled_sd") &&
        (!columns[[spec$mean]]$role %in% c("weighted_mean", "nonpoolable") ||
         !identical(columns[[spec$count]]$role, "sum"))) {
      fail("column '", column,
           "' must reference a mean and a sum-count column.")
    }
    if (identical(role, "nonpoolable") &&
        (!is.character(spec$reason) || length(spec$reason) != 1L ||
         is.na(spec$reason) || !nzchar(trimws(spec$reason)))) {
      fail("column '", column, "' requires a non-empty reason.")
    }
  }

  scalar_column <- function(field) {
    value <- contract[[field]]
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !value %in% column_names) fail("'", field, "' must name one column.")
  }
  if (identical(strategy, "effect_estimate")) {
    scalar_column("log_estimate")
    scalar_column("standard_error")
    if (identical(contract$log_estimate, contract$standard_error) ||
        !identical(contract$transform, "exp") ||
        any(vapply(columns[c(contract$log_estimate,
                             contract$standard_error)], `[[`, character(1L),
                   "role") != "nonpoolable")) {
      fail("effect_estimate requires distinct non-poolable estimate/SE ",
           "columns and transform 'exp'.")
    }
    strata <- contract$strata
    if (!is.character(strata) || anyNA(strata) || any(!strata %in% column_names) ||
        anyDuplicated(strata)) {
      fail("effect_estimate strata must name unique key columns.")
    }
    if (length(strata) > 0L &&
        any(vapply(columns[strata], `[[`, character(1L), "role") != "key")) {
      fail("effect_estimate strata must have role key.")
    }
    key_columns <- names(columns)[vapply(
      columns, `[[`, character(1L), "role"
    ) == "key"]
    if (!setequal(strata, key_columns)) {
      fail("effect_estimate strata must include every key column.")
    }
  }
  if (identical(strategy, "kaplan_meier")) {
    strata <- contract$strata
    if (!is.character(strata) || length(strata) == 0L || anyNA(strata) ||
        any(!strata %in% column_names) || anyDuplicated(strata)) {
      fail("strata must name one or more unique columns.")
    }
    fields <- c("order", "at_risk", "events", "survival")
    referenced <- vapply(fields, function(field) {
      scalar_column(field)
      contract[[field]]
    }, character(1L))
    if (anyDuplicated(c(strata, referenced)) ||
        !all(vapply(columns[c(strata, referenced[["order"]])], `[[`,
                    character(1L), "role") == "key") ||
        !all(vapply(columns[c(referenced[["at_risk"]],
                              referenced[["events"]])], `[[`, character(1L),
                    "role") == "sum") ||
        !identical(columns[[referenced[["survival"]]]]$role,
                   "nonpoolable")) {
      fail("Kaplan-Meier strata/order must be keys, risk/events sums, and ",
           "survival non-poolable.")
    }
    for (field in c("order_start", "order_step")) {
      value <- suppressWarnings(as.numeric(contract[[field]]))
      if (length(value) != 1L || is.na(value) || !is.finite(value) ||
          (identical(field, "order_step") && value <= 0)) {
        fail("'", field, "' must be one finite",
             if (identical(field, "order_step")) " positive" else "",
             " number.")
      }
    }
  }
  contract
}

.omopPoolingDistribution <- function(columns, count_cols,
                                     weight = "count_value") {
  mean_col <- intersect(c("avg_value", "average_value", "mean_value"), columns)
  sd_col <- intersect(c("stdev_value", "stddev_value", "sd_value"), columns)
  nonpoolable <- intersect(
    c("p10_value", "p25_value", "median_value", "p75_value", "p90_value"),
    columns
  )
  weighted <- list()
  pooled <- list()
  if (length(mean_col) == 1L && weight %in% columns) {
    weighted[[mean_col]] <- list(weight = weight)
  } else {
    nonpoolable <- c(nonpoolable, mean_col)
  }
  if (length(sd_col) == 1L && length(mean_col) == 1L && weight %in% columns) {
    pooled[[sd_col]] <- list(mean = mean_col, count = weight)
  } else {
    nonpoolable <- c(nonpoolable, sd_col)
  }
  .omopPoolingTabular(
    columns, sum_cols = count_cols, weighted_means = weighted,
    pooled_sds = pooled, nonpoolable_cols = unique(nonpoolable)
  )
}

.omopQueryPoolingContract <- function(entry) {
  columns <- entry$meta$output_fields
  if (!is.character(columns) || length(columns) == 0L || anyNA(columns) ||
      any(!nzchar(columns)) || anyDuplicated(columns)) {
    stop("QueryLibrary pooling contract has no declared output schema for '",
         entry$name, "'.", call. = FALSE)
  }
  sum_cols <- intersect(entry$disclosure$count_cols %||% character(0), columns)
  mean_cols <- grep("^(avg|mean)_", columns, ignore.case = TRUE, value = TRUE)
  sd_cols <- grep("^(sd|stddev|stdev)_", columns,
                  ignore.case = TRUE, value = TRUE)
  weighted <- list()
  pooled <- list()
  nonpoolable <- character(0)
  weight <- if ("n_values" %in% columns) {
    "n_values"
  } else if ("n_records" %in% columns) {
    "n_records"
  } else if ("n_persons" %in% columns) {
    "n_persons"
  } else NULL
  for (mean_col in mean_cols) {
    if (is.null(weight)) nonpoolable <- c(nonpoolable, mean_col) else
      weighted[[mean_col]] <- list(weight = weight)
  }
  for (sd_col in sd_cols) {
    suffix <- sub("^(sd|stddev|stdev)_", "", sd_col, ignore.case = TRUE)
    mean_col <- intersect(c(paste0("avg_", suffix), paste0("mean_", suffix)),
                          mean_cols)
    if (length(mean_col) == 1L && !is.null(weight)) {
      pooled[[sd_col]] <- list(mean = mean_col, count = weight)
    } else {
      nonpoolable <- c(nonpoolable, sd_col)
    }
  }
  .omopPoolingTabular(
    columns, sum_cols = sum_cols, weighted_means = weighted,
    pooled_sds = pooled, nonpoolable_cols = unique(nonpoolable)
  )
}

.omopAchillesPoolingContract <- function(entry) {
  aid <- as.integer(entry$meta$analysis_id)
  dist_plain <- c(103L, 105L, 203L, 403L, 603L, 703L, 803L, 1803L)
  dist_gender <- c(104L, 106L)
  dist_age_decile <- 107L
  dist_concept <- c(206L, 406L, 506L, 606L, 706L, 806L, 1806L, 2106L)
  dist_drug <- c(715L, 716L, 717L)
  stats <- c("count_value", "avg_value", "stdev_value", "p10_value",
             "p25_value", "median_value", "p75_value", "p90_value")
  if (aid %in% dist_plain) columns <- stats else
    if (aid %in% dist_gender) columns <- c("gender_concept_id", "gender_name", stats) else
    if (aid %in% dist_age_decile) columns <- c("age_decile", stats) else
    if (aid %in% dist_concept) columns <- c("concept_id", "concept_name", stats) else
    if (aid %in% dist_drug) columns <- c("covariate_id", "covariate_name", stats) else
      columns <- NULL
  if (!is.null(columns)) {
    return(.omopPoolingDistribution(columns, "count_value", "count_value"))
  }

  columns <- switch(
    as.character(aid),
    `1` =, `2000` =, `2001` =, `2002` =, `2003` = c("count_value"),
    `2` = c("gender_concept_id", "gender_name", "count_value"),
    `3` = c("year_of_birth", "count_value"),
    `4` = c("race_concept_id", "race_name", "count_value"),
    `5` = c("ethnicity_concept_id", "ethnicity_name", "count_value"),
    `10` = c("year_of_birth", "gender_concept_id", "gender_name", "count_value"),
    `12` = c("race_concept_id", "ethnicity_concept_id", "race_name",
             "ethnicity_name", "count_value"),
    `101` = c("age_band", "count_value"),
    `102` = c("age_band", "gender_name", "count_value"),
    `108` = c("length_bucket", "count_value"),
    `109` = c("calendar_year", "count_value"),
    `113` = c("num_periods", "count_value"),
    `200` =, `400` =, `500` =, `600` =, `700` =, `800` =,
    `1800` =, `2100` = c("concept_id", "concept_name", "count_value"),
    `202` =, `402` =, `602` =, `702` =, `802` =,
    `1802` = c("calendar_month", "concept_id", "concept_name", "count_value"),
    `221` = c("calendar_year", "count_value"),
    `502` = c("calendar_month", "count_value"),
    `201` =, `401` =, `505` =, `601` =, `701` =, `801` =,
    `1801` =, `2101` = c("concept_id", "concept_name", "count_value", "n_persons"),
    `220` =, `420` =, `620` =, `720` =, `820` =,
    `1820` =, `2102` = c("calendar_month", "count_value", "n_persons"),
    `1818` = c("measurement_concept_id", "measurement_concept_name",
               "unit_concept_id", "unit_concept_name", "range_bucket",
               "count_value", "n_persons"),
    NULL
  )
  if (is.null(columns)) {
    stop("No Achilles pooling schema for analysis ", aid, ".", call. = FALSE)
  }
  .omopPoolingTabular(
    columns,
    sum_cols = intersect(c("count_value", "n_persons"), columns)
  )
}

.omopOhdsiPrecomputedPoolingContract <- function(entry) {
  if (isTRUE(entry$meta$public_vocabulary_metadata)) {
    columns <- c("concept_id", "concept_name", "domain_id", "vocabulary_id",
                 "standard_concept", "is_excluded")
    return(.omopPoolingTabular(columns))
  }
  registry <- .ohdsi_tool_registry()
  tool <- registry[[entry$meta$tool_id]]
  reviewed <- if (!is.null(tool)) tool$contracts[[entry$meta$table_name]] else NULL
  if (is.null(reviewed) || !identical(reviewed$release, "public")) {
    return(.omopPoolingNotPoolable(
      "No reviewed public sufficient-statistic contract is available."
    ))
  }
  columns <- reviewed$public_columns
  counts <- intersect(reviewed$count_columns %||% character(0), columns)
  statistics <- intersect(reviewed$statistic_columns %||% character(0), columns)
  labels <- intersect(c("concept_name", "visit_context"), columns)
  site_columns <- intersect(
    c("database_id", "development_database_id", "validation_database_id"),
    columns
  )
  if (all(c("log_rr", "se_log_rr") %in% columns)) {
    strata <- setdiff(columns, c(counts, statistics, labels, site_columns))
    return(.omopPoolingEffectEstimate(
      columns, "log_rr", "se_log_rr", strata = strata, sum_cols = counts,
      nonpoolable_cols = c(statistics, labels, site_columns)
    ))
  }
  .omopPoolingTabular(
    columns, sum_cols = counts, label_cols = labels,
    nonpoolable_cols = c(statistics, site_columns)
  )
}

.omopExternalPoolingContract <- function(entry) {
  output <- entry$meta$output_contract
  columns <- names(output$columns)
  semantics <- vapply(output$columns, `[[`, character(1L), "semantic")
  sums <- names(semantics)[semantics == "count"]
  ratios <- list()
  weighted <- list()
  nonpoolable <- character(0)
  for (column in columns) {
    spec <- output$columns[[column]]
    if (identical(spec$semantic, "ratio")) {
      ratios[[column]] <- list(numerator = spec$numerator,
                               denominator = spec$denominator,
                               scale = spec$scale)
    }
    if (spec$semantic %in% c("metric", "relative_day", "duration")) {
      basis <- spec$basis %||% character(0)
      if (length(basis) == 1L) {
        weighted[[column]] <- list(weight = basis)
      } else {
        nonpoolable <- c(nonpoolable, column)
      }
    }
  }
  .omopPoolingTabular(
    columns, sum_cols = sums, ratios = ratios, weighted_means = weighted,
    nonpoolable_cols = nonpoolable
  )
}

.omopNativePoolingContract <- function(entry) {
  id <- entry$name
  tab <- function(columns, sums = character(0), ratios = list(),
                  weighted = list(), pooled_sd = list(),
                  labels = character(0),
                  nonpoolable = character(0)) {
    .omopPoolingTabular(columns, sum_cols = sums, ratios = ratios,
                        weighted_means = weighted, pooled_sds = pooled_sd,
                        label_cols = labels,
                        nonpoolable_cols = nonpoolable)
  }
  ratio <- function(numerator, denominator, scale = 1) {
    list(numerator = numerator, denominator = denominator, scale = scale)
  }

  if (id %in% c("dsomop:ohdsi.cohort_diagnostics.cohort_count",
                "dsomop:ohdsi.characterization.c_cohort_counts")) {
    return(tab(c("cohort_subjects", "cohort_entries"),
               c("cohort_subjects", "cohort_entries")))
  }
  if (id %in% c("dsomop:cohortdx.temporal_prevalence",
                "dsomop:ohdsi.cohort_diagnostics.temporal_covariate_value")) {
    columns <- c("time_window", "covariate_id", "covariate_name", "sum_value",
                 "cohort_size", "average")
    return(tab(columns, c("sum_value", "cohort_size"),
               ratios = list(average = ratio("sum_value", "cohort_size"))))
  }
  if (identical(id,
                "dsomop:ohdsi.cohort_diagnostics.temporal_covariate_value_dist")) {
    return(.omopPoolingDistribution(
      c("time_window", "covariate_id", "covariate_name", "count_value",
        "n_persons", "avg_value", "stdev_value", "p10_value", "p25_value",
        "median_value", "p75_value", "p90_value"),
      c("count_value", "n_persons"), "n_persons"
    ))
  }
  if (id %in% c("dsomop:ohdsi.cohort_diagnostics.time_series",
                "dsomop:cohortdx.time_series")) {
    return(tab(c("calendar_year", "subjects", "records", "person_days"),
               c("subjects", "records", "person_days")))
  }
  if (id %in% c("dsomop:cohortdx.included_source_concepts",
                "dsomop:ohdsi.cohort_diagnostics.included_source_concept")) {
    return(tab(c("concept_id", "concept_name", "concept_count",
                 "concept_subjects"), c("concept_count", "concept_subjects")))
  }
  if (id %in% c("dsomop:cohortdx.resolved_concepts",
                "dsomop:ohdsi.cohort_diagnostics.resolved_concepts")) {
    return(tab(c("concept_id", "concept_name", "domain_id", "vocabulary_id",
                 "standard_concept", "is_excluded")))
  }
  if (id %in% c("dsomop:cohortdx.orphan_concepts",
                "dsomop:ohdsi.cohort_diagnostics.orphan_concept")) {
    return(tab(c("concept_id", "concept_name", "n_persons", "n_records"),
               c("n_persons", "n_records")))
  }
  if (id %in% c("dsomop:char.dechallenge_rechallenge",
                "dsomop:ohdsi.characterization.c_dechallenge_rechallenge")) {
    columns <- c("num_dechallenge_attempt", "num_dechallenge_success",
                 "num_rechallenge_attempt", "num_rechallenge_success",
                 "outcome_concept_id", "pct_dechallenge_success",
                 "pct_rechallenge_success")
    return(tab(
      columns, columns[1:4], ratios = list(
        pct_dechallenge_success = ratio("num_dechallenge_success",
                                        "num_dechallenge_attempt", 100),
        pct_rechallenge_success = ratio("num_rechallenge_success",
                                        "num_rechallenge_attempt", 100)
      )
    ))
  }

  if (id %in% c("dsomop:incidence.rate")) {
    columns <- c("stratum", "persons_at_risk", "person_days", "outcomes",
                 "person_outcomes", "proportion", "rate")
    return(tab(
      columns, c("persons_at_risk", "person_days", "outcomes", "person_outcomes"),
      ratios = list(
        proportion = ratio("person_outcomes", "persons_at_risk"),
        rate = ratio("person_outcomes", "person_days")
      )
    ))
  }
  if (id %in% c("dsomop:cohortdx.index_event_breakdown")) {
    return(tab(c("concept_id", "concept_name", "concept_count", "subject_count"),
               c("concept_count", "subject_count")))
  }
  if (id %in% c("dsomop:char.time_to_event")) {
    return(tab(c("day_offset", "num_events", "persons"),
               c("num_events", "persons")))
  }
  if (id %in% c("dsomop:cohortdx.visit_context")) {
    return(tab(c("concept_id", "concept_name", "position", "subjects"),
               "subjects"))
  }
  if (id %in% c("dsomop:cohortdx.time_distribution",
                "dsomop:cm.followup_distribution",
                "dsomop:fe.comorbidity_index")) {
    return(.omopPoolingDistribution(
      c("metric", "count_value", "n_persons", "avg_value", "stdev_value",
        "p10_value", "p25_value", "median_value", "p75_value", "p90_value"),
      c("count_value", "n_persons"), "n_persons"
    ))
  }
  if (id %in% c("dsomop:fe.continuous")) {
    return(.omopPoolingDistribution(
      c("metric", "covariate_id", "covariate_name", "count_value",
        "n_persons", "avg_value", "stdev_value", "p10_value", "p25_value",
        "median_value", "p75_value", "p90_value"),
      c("count_value", "n_persons"), "n_persons"
    ))
  }
  if (id %in% c("dsomop:txpath.duration_eras")) {
    return(.omopPoolingDistribution(
      c("covariate_id", "covariate_name", "count_value", "avg_value",
        "stdev_value", "p10_value", "p25_value", "median_value", "p75_value",
        "p90_value"), "count_value", "count_value"
    ))
  }
  if (id %in% c("dsomop:fe.prevalence")) {
    columns <- c("covariate_id", "covariate_name", "sum_value", "average",
                 "cohort_size", "domain")
    return(tab(columns, c("sum_value", "cohort_size"),
               ratios = list(average = ratio("sum_value", "cohort_size"))))
  }
  if (id %in% c("dsomop:plp.covariate_summary")) {
    columns <- c("covariate_id", "covariate_name", "outcome", "sum_value",
                 "average", "group_size")
    return(tab(columns, c("sum_value", "group_size"),
               ratios = list(average = ratio("sum_value", "group_size"))))
  }
  if (id %in% c("dsomop:fe.table1")) {
    columns <- c("characteristic", "level", "unit", "sum_value", "average",
                 "count_value", "avg_value", "stdev_value", "p10_value",
                 "p25_value", "median_value", "p75_value", "p90_value")
    return(tab(columns, "sum_value", nonpoolable = setdiff(columns[5:13], "sum_value")))
  }
  if (id %in% c("dsomop:char.target_covariates")) {
    columns <- c("kind", "covariate_id", "covariate_name", "sum_value",
                 "average", "count_value", "avg_value", "stdev_value",
                 "p10_value", "p25_value", "median_value", "p75_value",
                 "p90_value")
    return(tab(columns, "sum_value", nonpoolable = columns[5:13]))
  }
  if (id %in% c("dsomop:cohortdx.cohort_overlap")) {
    return(tab(c("category", "n"), "n"))
  }
  if (id %in% c("dsomop:char.risk_factor_smd")) {
    columns <- c("covariate_id", "covariate_name", "case_sum_value",
                 "non_case_sum_value", "case_average", "non_case_average", "smd")
    return(tab(columns, c("case_sum_value", "non_case_sum_value"),
               nonpoolable = c("case_average", "non_case_average", "smd")))
  }
  if (id %in% c("dsomop:cm.covariate_balance")) {
    columns <- c("covariate_id", "covariate_name", "target_sum_value",
                 "comparator_sum_value", "target_average", "comparator_average",
                 "std_mean_diff")
    return(tab(columns, c("target_sum_value", "comparator_sum_value"),
               nonpoolable = c("target_average", "comparator_average",
                               "std_mean_diff")))
  }
  if (id %in% c("dsomop:cm.propensity_distribution")) {
    columns <- c("arm", "ps_bin", "bin_low", "bin_high", "person_count",
                 "equipoise", "auc")
    return(tab(columns, "person_count", nonpoolable = c("equipoise", "auc")))
  }

  if (id %in% c("dsomop:txpath.pathways")) {
    return(tab(
      c("row_type", "depth", "parent_path_id", "path_id",
        "treatment_concept_id", "parent_path", "path", "treatment",
        "person_count"),
      "person_count", labels = c("parent_path", "path", "treatment")
    ))
  }
  if (id %in% c("dsomop:txpath.percentage_treated")) {
    columns <- c("treatment_layer", "treatment_concept_id", "treatment",
                 "n_treated", "cohort_size", "pct_treated")
    return(tab(columns, c("n_treated", "cohort_size"),
               ratios = list(pct_treated = ratio("n_treated", "cohort_size", 100))))
  }

  if (id %in% c("dsomop:plp.attrition")) {
    return(tab(c("step_order", "step", "n_persons", "n_outcomes"),
               c("n_persons", "n_outcomes")))
  }
  if (id %in% c("dsomop:plp.performance")) {
    return(tab(c("metric", "value", "population_size"), "population_size",
               nonpoolable = "value"))
  }
  if (id %in% c("dsomop:plp.calibration")) {
    return(.omopPoolingNotPoolable(
      "Site-specific quantile bins do not define common cross-site strata."
    ))
  }
  if (id %in% c("dsomop:plp.threshold")) {
    columns <- c("threshold", "tp", "fp", "tn", "fn", "n_positive",
                 "n_negative", "sensitivity", "specificity", "ppv")
    return(tab(
      columns, c("tp", "fp", "tn", "fn", "n_positive", "n_negative"),
      ratios = list(
        sensitivity = ratio("tp", "n_positive"),
        specificity = ratio("tn", "n_negative"),
        ppv = ratio("tp", c("tp", "fp"))
      )
    ))
  }
  if (id %in% c("dsomop:plp.diagnostic")) {
    columns <- c("population_size", "n_outcomes", "n_predictors",
                 "outcome_incidence", "events_per_variable")
    return(tab(columns, c("population_size", "n_outcomes"),
               ratios = list(outcome_incidence = ratio("n_outcomes",
                                                       "population_size")),
               nonpoolable = c("n_predictors", "events_per_variable")))
  }

  if (id %in% c("dsomop:sccs.attrition")) {
    return(tab(c("step", "outcome_subjects", "outcome_events",
                 "outcome_observation_periods", "observed_days"),
               c("outcome_subjects", "outcome_events",
                 "outcome_observation_periods", "observed_days")))
  }
  if (id %in% c("dsomop:sccs.outcome_rate_per_month")) {
    columns <- c("calendar_month", "outcome_events", "observed_persons",
                 "n_persons", "outcome_rate")
    return(tab(columns, c("outcome_events", "observed_persons", "n_persons"),
               ratios = list(outcome_rate = ratio("outcome_events",
                                                  "observed_persons"))))
  }
  if (id %in% c("dsomop:sccs.count_histograms")) {
    return(tab(c("day_offset", "window", "outcome_events", "n_persons"),
               c("outcome_events", "n_persons")))
  }
  if (id %in% c("dsomop:sccs.assumption_checks")) {
    columns <- c("check", "numerator", "denominator", "n_persons", "statistic")
    return(tab(columns, c("numerator", "denominator", "n_persons"),
               ratios = list(statistic = ratio("numerator", "denominator"))))
  }
  if (id %in% c("dsomop:sccs.incidence_rate_ratio")) {
    columns <- c("model_type", "n_cases", "exposed_events", "unexposed_events",
                 "irr", "ci_lo", "ci_hi", "log_irr", "se_log_irr")
    return(.omopPoolingEffectEstimate(
      columns, "log_irr", "se_log_irr", strata = "model_type",
      sum_cols = c("n_cases", "exposed_events", "unexposed_events"),
      nonpoolable_cols = c("irr", "ci_lo", "ci_hi")
    ))
  }

  if (id %in% c("dsomop:cm.attrition",
                "dsomop:ohdsi.cohort_method.cm_attrition")) {
    return(tab(c("step_order", "step", "persons", "exposures"),
               c("persons", "exposures")))
  }
  if (id %in% c("dsomop:cm.mdrr",
                "dsomop:ohdsi.cohort_method.cm_result")) {
    columns <- c("arm", "persons", "person_days", "outcomes", "mdrr")
    return(tab(columns, c("persons", "person_days", "outcomes"),
               nonpoolable = "mdrr"))
  }
  if (id %in% c("dsomop:cm.interaction_estimate",
                "dsomop:ohdsi.cohort_method.cm_interaction_result")) {
    columns <- c("subgroup_label", "model_type", "target_persons",
                 "comparator_persons",
                 "target_outcomes", "comparator_outcomes", "target_person_days",
                 "comparator_person_days", "estimate", "ci_lo", "ci_hi",
                 "log_estimate", "se_log_estimate")
    return(.omopPoolingEffectEstimate(
      columns, "log_estimate", "se_log_estimate",
      strata = c("subgroup_label", "model_type"),
      sum_cols = columns[3:8],
      nonpoolable_cols = c("estimate", "ci_lo", "ci_hi")
    ))
  }
  if (id %in% c("dsomop:cm.diagnostics_summary",
                "dsomop:ohdsi.cohort_method.cm_diagnostics_summary")) {
    columns <- c("summary", "mdrr", "max_sdm", "attrition_fraction",
                 "shared_balance_pass", "equipoise", "ps_auc",
                 "n_persons_target", "n_persons_comparator", "n_persons_min")
    return(tab(columns, columns[8:10], nonpoolable = columns[2:7]))
  }
  if (id %in% c("dsomop:cm.kaplan_meier",
                "dsomop:ohdsi.cohort_method.cm_kaplan_meier_dist")) {
    columns <- c("arm", "time_bin", "bin_start_days", "bin_end_days", "at_risk",
                 "events", "survival_probability")
    return(.omopPoolingKaplanMeier(
      columns, strata = "arm", order = "time_bin", at_risk = "at_risk",
      events = "events", survival = "survival_probability"
    ))
  }
  if (id %in% c("dsomop:cm.effect_estimate")) {
    columns <- c("arm", "model_type", "persons", "person_days", "outcomes",
                 "estimate", "ci_lo", "ci_hi", "log_estimate",
                 "se_log_estimate")
    return(.omopPoolingEffectEstimate(
      columns, "log_estimate", "se_log_estimate",
      strata = c("arm", "model_type"),
      sum_cols = c("persons", "person_days", "outcomes"),
      nonpoolable_cols = c("estimate", "ci_lo", "ci_hi")
    ))
  }
  if (identical(id, "dsomop:demo.person_count_by_gender")) {
    return(tab(c("gender_concept_id", "gender_name", "n_persons"), "n_persons"))
  }
  NULL
}

.omopAnalysisDirectPoolingContract <- function(entry) {
  adapter <- entry$meta$adapter %||% ""
  if (identical(adapter, "query")) return(.omopQueryPoolingContract(entry))
  if (adapter %in% c("achilles", "achilles_live")) {
    return(.omopAchillesPoolingContract(entry))
  }
  if (identical(adapter, "external_pack")) {
    return(.omopExternalPoolingContract(entry))
  }
  native <- .omopNativePoolingContract(entry)
  if (!is.null(native)) return(native)
  if (identical(adapter, "ohdsi")) {
    return(.omopOhdsiPrecomputedPoolingContract(entry))
  }
  NULL
}

.omopAnalysisAttachPoolingContracts <- function(entries) {
  if (length(entries) == 0L) return(entries)
  for (id in names(entries)) {
    entry <- entries[[id]]
    if (identical(entry$mode, "assign")) {
      entry$meta$pooling_contract <- NULL
      entries[[id]] <- entry
      next
    }
    if (!is.null(entry$meta$alias_target)) next
    contract <- .omopAnalysisDirectPoolingContract(entry)
    if (is.null(contract)) {
      stop("Aggregate analysis '", id,
           "' has no typed pooling contract.", call. = FALSE)
    }
    entry$meta$pooling_contract <-
      .omopAnalysisValidatePoolingContract(contract, id)
    entries[[id]] <- entry
  }
  for (id in names(entries)) {
    entry <- entries[[id]]
    target <- entry$meta$alias_target
    if (identical(entry$mode, "assign") || is.null(target)) next
    if (!target %in% names(entries) ||
        is.null(entries[[target]]$meta$pooling_contract)) {
      stop("Pooling alias '", id, "' has no contracted target '", target,
           "'.", call. = FALSE)
    }
    entry$meta$pooling_contract <- entries[[target]]$meta$pooling_contract
    entries[[id]] <- entry
  }
  for (id in names(entries)) {
    entry <- entries[[id]]
    if (identical(entry$mode, "assign")) {
      if (!is.null(entry$meta$pooling_contract)) {
        stop("Assign analysis '", id, "' must not expose a pooling contract.",
             call. = FALSE)
      }
    } else {
      .omopAnalysisValidatePoolingContract(entry$meta$pooling_contract, id)
    }
  }
  entries
}

.omopAnalysisNormalizePoolingOutput <- function(df, entry) {
  contract <- entry$meta$pooling_contract
  if (!is.data.frame(df) || is.null(contract) ||
      identical(contract$strategy, "not_poolable")) return(df)
  expected <- names(contract$columns)
  if (nrow(df) == 0L) {
    out <- stats::setNames(rep(list(logical(0)), length(expected)), expected)
    return(as.data.frame(out, stringsAsFactors = FALSE,
                         optional = TRUE, check.names = FALSE))
  }
  if (!identical(names(df), expected)) {
    stop("Analysis '", entry$name,
         "' output does not match its closed pooling schema.", call. = FALSE)
  }
  df
}
