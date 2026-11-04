# Module: Automatic protection for the OHDSI community tools
#
# The core of dsOMOP (loading tables, recipes, filters, factors, cohorts,
# manipulation and dsOMOP's own exploration) never uses differential privacy.
# This file concerns only the QueryLibrary questions of the community tools
# that release exact means, standard deviations or fine partitions of
# person-level values: durations, per-person counts, values, costs and
# demographic distributions. Before 2.9.0 these templates were blocked. They
# are now answered by reviewed, server-owned replacements whose protection is
# chosen by the custodial dsomop.tools.mode setting (default "auto"):
#
#   "auto"      Per-person durations and counts are released as histograms of
#               one value per person over fixed public bins, and demographic
#               distributions as complete public partitions, with sticky
#               discrete-Laplace noise (epsilon-DP, sensitivity 1 person).
#               When the DP service is disabled or unavailable, the same table
#               is released under the standard DataSHIELD gate instead, so the
#               question always gets an answer. Questions without a public
#               scale (measurement values, quantities, costs and all-concept
#               summaries) use the standard person-supported summary.
#   "standard"  The original summaries (counts, means, SDs) under the standard
#               DataSHIELD gate only; no DP.
#   "dp"        Differential privacy only. Questions without a DP release are
#               refused. dsomop.dp.exclusive = TRUE (with DP enabled) selects
#               this mode.
#
# The route depends only on server settings and public request parameters,
# never on the data. A DP route never applies exact small-cell gates: the noisy
# counts of the complete public domain are released as they are.

.DSOMOP_TOOLS_MODES <- c("auto", "standard", "dp")
# The replacements build their SQL from the dialect-aware helpers only.
.DSOMOP_TOOLS_DIALECTS <- c("postgresql", "sql server", "oracle", "redshift",
                            "bigquery", "snowflake", "spark", "sqlite",
                            "duckdb", "mysql")
.DSOMOP_TOOLS_ADAPTER_VERSION <- 1L
.DSOMOP_TOOLS_DAY_BREAKS <- c(0, 30, 90, 180, 365, 730, 1825, 3650, 7300)
.DSOMOP_TOOLS_SUPPLY_BREAKS <- c(0, 7, 14, 30, 60, 90, 180, 365)
.DSOMOP_TOOLS_COUNT_BREAKS <- c(1, 2, 3, 4, 5, 10, 20, 50)
.DSOMOP_TOOLS_FIRST_BIRTH_YEAR <- 1900L
.DSOMOP_TOOLS_AGE_GROUPS <- c("0-9", "10-19", "20-29", "30-39", "40-49",
                              "50-59", "60-69", "70-79", "80-89", "90+",
                              "unknown")

# The OMOP Gender vocabulary is small and fixed. A fixed public domain keeps
# the released cells independent of which concepts a site happens to load.
# Any other gender value is counted under concept 0.
.dsomopToolsGenders <- function() {
  data.frame(
    gender_concept_id = c(8507L, 8532L, 8521L, 8551L, 8570L, 0L),
    gender_name = c("MALE", "FEMALE", "OTHER", "UNKNOWN", "AMBIGUOUS",
                    "No matching concept"),
    stringsAsFactors = FALSE
  )
}

# --- Settings ----------------------------------------------------------------

#' Effective protection mode of the community tools
#'
#' Reads the custodial \code{dsomop.tools.mode} option ("auto" by default,
#' "standard" or "dp"). \code{dsomop.dp.exclusive = TRUE} with DP enabled
#' selects "dp". The core never reads this setting.
#'
#' @return One of "auto", "standard" or "dp".
#' @keywords internal
.dsomopToolsMode <- function() {
  value <- getOption("dsomop.tools.mode",
                     getOption("default.dsomop.tools.mode", "auto"))
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop("dsomop.tools.mode must be one of auto, standard or dp.",
         call. = FALSE)
  }
  value <- tolower(trimws(value))
  if (!value %in% .DSOMOP_TOOLS_MODES) {
    stop("dsomop.tools.mode must be one of auto, standard or dp.",
         call. = FALSE)
  }
  if (.dsomopDpExclusive() && .dsomopDpEnabled()) "dp" else value
}

# Whether the DP service can release now. Any failure (disabled service,
# no persistent identity, state problems) is reported, never rerandomized:
# auto mode then uses the standard gate, strict DP mode refuses.
.dsomopToolsDpReady <- function(handle) {
  enabled <- tryCatch(.dsomopDpEnabled(), error = function(e) e)
  if (inherits(enabled, "error")) {
    message("[dsomop-tools] differential privacy unavailable: ",
            conditionMessage(enabled))
    return(list(ready = FALSE, reason = "unavailable"))
  }
  if (!isTRUE(enabled)) return(list(ready = FALSE, reason = "disabled"))
  policy <- tryCatch({
    .dsomopDpEnsureRuntime(handle)
    .dsomopDpPolicy()
  }, error = function(e) e)
  if (inherits(policy, "error")) {
    message("[dsomop-tools] differential privacy unavailable: ",
            conditionMessage(policy))
    return(list(ready = FALSE, reason = "unavailable"))
  }
  list(ready = TRUE, policy = policy)
}

# --- Reviewed questions --------------------------------------------------------

#' Reviewed replacements for the QueryLibrary distribution templates
#'
#' One record per template id. \code{kind} is "histogram" (one value per
#' person over fixed public bins; DP in auto/dp mode, the original summary in
#' standard mode), "partition" (persons counted in a complete public domain of
#' cells; DP in auto/dp mode) or "summary" (no public scale; the standard
#' person-supported summary in auto/standard mode, refused in dp mode).
#' \code{person.load} stays blocked: it is not an aggregate question.
#'
#' @return Named list of question specs.
#' @keywords internal
.dsomopToolsReviewedQueries <- function() {
  day_diff <- function(end, start) {
    force(end); force(start)
    function(handle) .omopDateDiffDays(handle, end, start)
  }
  column <- function(name) {
    force(name)
    function(handle) name
  }
  concept_summary <- function(concept_col, n_col, mean_col, sd_col,
                              extra = character(0)) {
    list(group = "concept", concept_col = concept_col,
         cols = c(concept_col, "concept_name", extra, "n_persons", n_col,
                  mean_col, sd_col),
         n_col = n_col, mean_col = mean_col, sd_col = sd_col)
  }
  list(
    "condition.duration_stats" = list(
      kind = "histogram", table = "condition_occurrence",
      concept_param = "concept_id", concept_col = "condition_concept_id",
      value = day_diff("x.condition_end_date", "x.condition_start_date"),
      where = c("x.condition_start_date IS NOT NULL",
                "x.condition_end_date IS NOT NULL"),
      reducer = "mean", valid_min = 0, breaks = .DSOMOP_TOOLS_DAY_BREAKS,
      value_label = "mean duration in days of the person's records",
      summary = concept_summary("condition_concept_id", "n_records",
                                "avg_days", "sd_days")
    ),
    "drug.duration_stats" = list(
      kind = "histogram", table = "drug_exposure",
      concept_param = "concept_id", concept_col = "drug_concept_id",
      value = day_diff("x.drug_exposure_end_date",
                       "x.drug_exposure_start_date"),
      where = c("x.drug_exposure_start_date IS NOT NULL",
                "x.drug_exposure_end_date IS NOT NULL"),
      reducer = "mean", valid_min = 0, breaks = .DSOMOP_TOOLS_DAY_BREAKS,
      value_label = "mean exposure duration in days of the person's records",
      summary = concept_summary("drug_concept_id", "n_records",
                                "avg_days", "sd_days")
    ),
    "drug.days_supply_stats" = list(
      kind = "histogram", table = "drug_exposure",
      concept_param = "concept_id", concept_col = "drug_concept_id",
      value = column("x.days_supply"),
      where = "x.days_supply IS NOT NULL",
      reducer = "mean", valid_min = 0, breaks = .DSOMOP_TOOLS_SUPPLY_BREAKS,
      value_label = "mean days supply of the person's records",
      summary = concept_summary("drug_concept_id", "n_records",
                                "avg_days_supply", "sd_days_supply")
    ),
    "drug_era.exposure_count_stats" = list(
      kind = "histogram", table = "drug_era",
      concept_param = "concept_id", concept_col = "drug_concept_id",
      value = column("x.drug_exposure_count"),
      where = "x.drug_exposure_count IS NOT NULL",
      reducer = "mean", valid_min = 1, breaks = .DSOMOP_TOOLS_COUNT_BREAKS,
      value_label = "mean number of exposures per era of the person",
      summary = concept_summary("drug_concept_id", "n_records",
                                "avg_exposure_count", "sd_exposure_count")
    ),
    "condition_era.eras_per_person_stats" = list(
      kind = "histogram", table = "condition_era",
      concept_param = "concept_id", concept_col = "condition_concept_id",
      value = NULL, where = character(0),
      reducer = "count", valid_min = 1, breaks = .DSOMOP_TOOLS_COUNT_BREAKS,
      value_label = "number of eras of the person",
      summary = list(group = "none", reducer = "person_count",
                     cols = c("n_persons", "avg_eras", "stddev_eras"),
                     n_col = NULL, mean_col = "avg_eras",
                     sd_col = "stddev_eras")
    ),
    "visit.duration_stats" = list(
      kind = "histogram", table = "visit_occurrence",
      concept_param = "visit_concept_id", concept_col = "visit_concept_id",
      all_concepts_value = 0L, label_name = "visit_name",
      value = day_diff("x.visit_end_date", "x.visit_start_date"),
      where = c("x.visit_start_date IS NOT NULL",
                "x.visit_end_date IS NOT NULL"),
      reducer = "mean", valid_min = 0, breaks = .DSOMOP_TOOLS_DAY_BREAKS,
      value_label = "mean visit duration in days of the person's visits",
      summary = list(group = "visit", concept_col = "visit_concept_id",
                     cols = c("visit_concept_id", "visit_name", "n_persons",
                              "n_records", "avg_days", "sd_days"),
                     n_col = "n_records", mean_col = "avg_days",
                     sd_col = "sd_days", order = "n_persons")
    ),
    "observation_period.length_by_gender" = list(
      kind = "histogram", table = "observation_period",
      concept_param = NULL, strata = "gender",
      value = day_diff("x.observation_period_end_date",
                       "x.observation_period_start_date"),
      where = c("x.observation_period_start_date IS NOT NULL",
                "x.observation_period_end_date IS NOT NULL"),
      reducer = "mean", valid_min = 0, breaks = .DSOMOP_TOOLS_DAY_BREAKS,
      value_label = "mean observation period length in days of the person",
      summary = list(group = "gender",
                     cols = c("gender_concept_id", "gender_name", "n_persons",
                              "avg_days"),
                     n_col = NULL, mean_col = "avg_days", sd_col = NULL,
                     order = "n_persons")
    ),
    "person.year_of_birth_distribution" = list(
      kind = "partition", partition = "year_of_birth",
      value_label = "persons by year of birth"
    ),
    "person.birth_month_distribution" = list(
      kind = "partition", partition = "month_of_birth",
      value_label = "persons with a recorded birth month, by month"
    ),
    "person.gender_by_age_decade" = list(
      kind = "partition", partition = "gender_age",
      value_label = "persons by gender and age group (years since birth year)"
    ),
    "drug.quantity_stats" = list(
      kind = "summary", table = "drug_exposure",
      concept_param = "concept_id", concept_col = "drug_concept_id",
      value = column("x.quantity"), where = "x.quantity IS NOT NULL",
      value_label = "quantity of the drug records",
      summary = concept_summary("drug_concept_id", "n_records",
                                "avg_quantity", "sd_quantity")
    ),
    "measurement.value_stats" = list(
      kind = "summary", table = "measurement",
      concept_param = "concept_id", concept_col = "measurement_concept_id",
      value = column("x.value_as_number"),
      where = "x.value_as_number IS NOT NULL",
      value_label = "numeric values of the measurement, per unit",
      summary = c(concept_summary("measurement_concept_id", "n_values",
                                  "mean_value", "sd_value",
                                  extra = c("unit_concept_id", "unit_name")),
                  list(split = "unit"))
    ),
    "drug_cost.cost_per_unit_stats" = list(
      kind = "summary", table = "drug_exposure", cost = TRUE,
      concept_param = "concept_id", concept_col = "drug_concept_id",
      value = column("(co.total_paid / x.quantity)"),
      where = c("x.quantity > 0", "co.total_paid IS NOT NULL"),
      value_label = "total paid per unit of the drug records, per currency",
      summary = c(concept_summary("drug_concept_id", "n_records",
                                  "avg_cost_per_unit", "sd_cost_per_unit",
                                  extra = c("currency_concept_id",
                                            "currency_name")),
                  list(split = "currency"))
    ),
    "drug_cost.out_of_pocket_stats" = list(
      kind = "summary", table = "drug_exposure", cost = TRUE,
      concept_param = "concept_id", concept_col = "drug_concept_id",
      value = column("co.paid_by_patient"),
      where = "co.paid_by_patient IS NOT NULL",
      value_label = "amount paid by the patient for the drug records, per currency",
      summary = c(concept_summary("drug_concept_id", "n_records",
                                  "avg_out_of_pocket", "sd_out_of_pocket",
                                  extra = c("currency_concept_id",
                                            "currency_name")),
                  list(split = "currency"))
    ),
    "condition_era.length_stats" = list(
      kind = "summary", table = "condition_era", concept_param = NULL,
      value = day_diff("x.condition_era_end_date",
                       "x.condition_era_start_date"),
      where = c("x.condition_era_start_date IS NOT NULL",
                "x.condition_era_end_date IS NOT NULL"),
      value_label = "condition era length in days",
      summary = list(group = "all_concepts", concept_col = "condition_concept_id",
                     cols = c("condition_concept_id", "concept_name",
                              "n_persons", "n_records", "avg_days",
                              "stddev_days"),
                     n_col = "n_records", mean_col = "avg_days",
                     sd_col = "stddev_days", order = "n_persons")
    ),
    "drug_era.length_stats" = list(
      kind = "summary", table = "drug_era", concept_param = NULL,
      value = day_diff("x.drug_era_end_date", "x.drug_era_start_date"),
      where = c("x.drug_concept_id <> 0", "x.drug_era_start_date IS NOT NULL",
                "x.drug_era_end_date IS NOT NULL"),
      value_label = "drug era length in days",
      summary = list(group = "all_concepts", concept_col = "drug_concept_id",
                     cols = c("drug_concept_id", "concept_name", "n_persons",
                              "n_records", "avg_days", "sd_days"),
                     n_col = "n_records", mean_col = "avg_days",
                     sd_col = "sd_days", order = "n_persons",
                     top_n = TRUE)
    ),
    "observation_period.length_stats" = list(
      kind = "summary", table = "observation_period", concept_param = NULL,
      value = day_diff("x.observation_period_end_date",
                       "x.observation_period_start_date"),
      where = c("x.observation_period_start_date IS NOT NULL",
                "x.observation_period_end_date IS NOT NULL"),
      value_label = "observation period length in days, by start year",
      summary = list(group = "start_year",
                     cols = c("start_year", "n_persons", "avg_days"),
                     n_col = NULL, mean_col = "avg_days", sd_col = NULL)
    ),
    "observation_period.length_months_stats" = list(
      kind = "summary", table = "observation_period", concept_param = NULL,
      value = function(handle) paste0(
        "(", .dsomopToolsMonthIndexSql(handle, "x.observation_period_end_date"),
        " - ",
        .dsomopToolsMonthIndexSql(handle, "x.observation_period_start_date"),
        ")"),
      where = c("x.observation_period_start_date IS NOT NULL",
                "x.observation_period_end_date IS NOT NULL"),
      value_label = "observation period length in calendar months, by start year",
      summary = list(group = "start_year",
                     cols = c("start_year", "n_persons", "avg_months",
                              "stddev_months"),
                     n_col = NULL, mean_col = "avg_months",
                     sd_col = "stddev_months")
    )
  )
}

# Calendar month index (year * 12 + month) of a date column.
.dsomopToolsMonthIndexSql <- function(handle, date_expr) {
  dialect <- tolower(handle$target_dialect %||% "")
  if (identical(dialect, "sqlite")) {
    paste0("(CAST(strftime('%Y', ", date_expr, ") AS INTEGER) * 12 + ",
           "CAST(strftime('%m', ", date_expr, ") AS INTEGER))")
  } else if (dialect %in% c("mysql", "sql server", "spark")) {
    paste0("(YEAR(", date_expr, ") * 12 + MONTH(", date_expr, "))")
  } else {
    paste0("(", .omopIntegerCastSql(handle, paste0("EXTRACT(YEAR FROM ",
                                                   date_expr, ")")),
           " * 12 + ",
           .omopIntegerCastSql(handle, paste0("EXTRACT(MONTH FROM ",
                                              date_expr, ")")),
           ")")
  }
}

# --- Output shapes -------------------------------------------------------------

# The output shape follows the mode only (never the data or DP availability), so
# the pooling contract of an entry is fixed for the session.
.dsomopToolsShape <- function(spec, mode = .dsomopToolsMode()) {
  if (identical(spec$kind, "histogram")) {
    if (identical(mode, "standard")) "summary" else "histogram"
  } else spec$kind
}

.dsomopToolsLabelCols <- function(spec) {
  if (identical(spec$strata, "gender")) {
    return(c("gender_concept_id", "gender_name"))
  }
  c(spec$concept_col, spec$label_name %||% "concept_name")
}

.dsomopToolsPartitionCols <- function(spec) {
  switch(spec$partition,
    year_of_birth = "year_of_birth",
    month_of_birth = "month_of_birth",
    gender_age = c("gender_concept_id", "gender_name", "age_group")
  )
}

.dsomopToolsOutputCols <- function(spec, shape) {
  switch(shape,
    histogram = c(.dsomopToolsLabelCols(spec), "bin", "bin_label",
                  "bin_lower", "bin_upper", "n_persons"),
    partition = c(.dsomopToolsPartitionCols(spec), "n_persons"),
    summary = spec$summary$cols
  )
}

.dsomopToolsDisclosure <- function(spec, shape) {
  if (shape %in% c("histogram", "partition")) {
    return(.omopAnalysisDisclosure(unit = "person", count_cols = "n_persons",
                                   person_id_col = "n_persons"))
  }
  s <- spec$summary
  count_cols <- c("n_persons", s$n_col)
  disclosure <- .omopAnalysisDisclosure(unit = "dist", count_cols = count_cols,
                                        person_id_col = "n_persons")
  disclosure$dist_count_col <- unique(c(s$n_col, "n_persons"))
  disclosure$summary_cols <- c(s$mean_col, s$sd_col)
  disclosure$extreme_cols <- character(0)
  disclosure
}

.dsomopToolsPoolingContract <- function(entry) {
  spec <- entry$meta$tools_spec
  shape <- entry$meta$tools_shape
  columns <- .dsomopToolsOutputCols(spec, shape)
  if (shape %in% c("histogram", "partition")) {
    return(.omopPoolingTabular(columns, sum_cols = "n_persons",
                               label_cols = setdiff(columns, "n_persons")))
  }
  s <- spec$summary
  sum_cols <- c("n_persons", s$n_col)
  weight <- s$n_col %||% "n_persons"
  weighted <- stats::setNames(list(list(weight = weight)), s$mean_col)
  pooled <- if (!is.null(s$sd_col)) {
    stats::setNames(list(list(mean = s$mean_col, count = weight)), s$sd_col)
  } else list()
  .omopPoolingTabular(columns, sum_cols = sum_cols, weighted_means = weighted,
                      pooled_sds = pooled,
                      label_cols = setdiff(columns, c(sum_cols, s$mean_col,
                                                      s$sd_col)))
}

# --- Catalog entries -------------------------------------------------------------

#' Build the catalog entry of one reviewed QueryLibrary question
#'
#' @param qid Template id.
#' @param q Parsed template.
#' @param spec Reviewed spec from \code{.dsomopToolsReviewedQueries()}.
#' @param mode Effective tools mode.
#' @return An \code{omop_analysis_entry} with adapter "tools_auto".
#' @keywords internal
.dsomopToolsAutoEntry <- function(qid, q, spec, mode) {
  shape <- .dsomopToolsShape(spec, mode)
  name <- paste0("dsomop:", qid)
  tables <- if (identical(spec$kind, "partition")) "person" else
    unique(c(spec$table, if (isTRUE(spec$cost)) "cost",
             if (identical(spec$strata, "gender")) "person"))
  .omopAnalysisEntry(
    name = name,
    description = q$description,
    domain = tolower(q$group %||% "general"),
    params = .omopAnalysisParamsFromInputs(q$inputs),
    compute = list(kind = "r", sql = NULL, fn = function(handle, ctx, params) {
      .dsomopToolsAutoCompute(handle, ctx, params)
    }),
    dependencies = list(tables = tables, packages = character(0)),
    disclosure = .dsomopToolsDisclosure(spec, shape),
    scope = .omopAnalysisScope(accepts_cohort = FALSE, accepts_tables = FALSE,
                               max_tables = 0L),
    mode = "aggregate",
    meta = list(adapter = "tools_auto", query_id = qid,
                inputs_df = q$inputs, tools_spec = spec,
                supported_dialects = .DSOMOP_TOOLS_DIALECTS,
                tools_shape = shape, tools_mode = mode,
                output_fields = .dsomopToolsOutputCols(spec, shape),
                cdm_version = q$cdm_version)
  )
}

# --- Routing -----------------------------------------------------------------------

#' Choose how one run of a reviewed question is protected
#'
#' Depends only on the custodial mode, the DP service state and the question;
#' never on the data.
#'
#' @param handle CDM handle.
#' @param entry Catalog entry (adapter "tools_auto").
#' @return List with protection ("dp", "standard_fallback" or "standard"),
#'   shape, mode and, for DP, the policy.
#' @keywords internal
.dsomopToolsRoute <- function(handle, entry) {
  spec <- entry$meta$tools_spec
  mode <- .dsomopToolsMode()
  if (!identical(mode, entry$meta$tools_mode)) {
    stop("dsomop.tools.mode changed during this R session; restart the ",
         "session after changing server configuration.", call. = FALSE)
  }
  shape <- entry$meta$tools_shape
  route <- list(mode = mode, shape = shape)
  if (identical(shape, "summary")) {
    if (identical(mode, "dp")) {
      stop("This question has no differential-privacy release, and the data ",
           "custodian has set the community tools to DP-only mode ",
           "(dsomop.tools.mode = \"dp\" or dsomop.dp.exclusive = TRUE).",
           call. = FALSE)
    }
    return(c(route, list(protection = "standard")))
  }
  if (identical(mode, "standard")) {
    return(c(route, list(protection = "standard")))
  }
  ready <- .dsomopToolsDpReady(handle)
  if (isTRUE(ready$ready)) {
    return(c(route, list(protection = "dp", policy = ready$policy)))
  }
  if (identical(mode, "dp")) {
    stop("The data custodian has set the community tools to DP-only mode, ",
         "but the differential-privacy service is not available on this ",
         "server.", call. = FALSE)
  }
  c(route, list(protection = "standard_fallback",
                fallback_reason = ready$reason))
}

# --- Compute ---------------------------------------------------------------------

.dsomopToolsConceptParam <- function(spec, params) {
  if (is.null(spec$concept_param)) return(NULL)
  value <- suppressWarnings(as.numeric(params[[spec$concept_param]]))
  if (length(value) != 1L || is.na(value) || value != floor(value) ||
      value < 0 || value > .Machine$integer.max) {
    stop("Parameter '", spec$concept_param, "' must be one non-negative ",
         "concept id.", call. = FALSE)
  }
  as.integer(value)
}

# The rows of the question's population with the filters of the template.
.dsomopToolsFromSql <- function(handle, spec, concept_id) {
  from <- paste0(" FROM ", .qualifyTable(handle, spec$table), " x")
  if (isTRUE(spec$cost)) {
    from <- paste0(" FROM ", .qualifyTable(handle, "cost"), " co",
                   " INNER JOIN ", .qualifyTable(handle, spec$table),
                   " x ON x.drug_exposure_id = co.cost_event_id")
  }
  where <- spec$where %||% character(0)
  if (isTRUE(spec$cost)) where <- c("co.cost_domain_id = 'Drug'", where)
  if (!is.null(concept_id) &&
      !identical(concept_id, spec$all_concepts_value %||% -1L)) {
    where <- c(where, paste0("x.", spec$concept_col, " = ", concept_id))
  }
  list(from = from, where = where)
}

.dsomopToolsWhere <- function(where) {
  if (length(where) == 0L) "" else
    paste0(" WHERE ", paste(where, collapse = " AND "))
}

.dsomopToolsConceptName <- function(handle, concept_id) {
  if (is.null(concept_id)) return(NA_character_)
  found <- tryCatch(.vocabLookupConcepts(handle, concept_id),
                    error = function(e) NULL)
  if (!is.data.frame(found) || nrow(found) == 0L) return(NA_character_)
  as.character(found$concept_name[[1L]])
}

# One value per person (and the person's stratum when the question has one).
.dsomopToolsPersonValues <- function(handle, spec, concept_id) {
  src <- .dsomopToolsFromSql(handle, spec, concept_id)
  if (identical(spec$reducer, "count")) {
    value_sql <- "COUNT(*)"
    where <- src$where
  } else {
    value <- spec$value(handle)
    value_sql <- paste0("AVG(CAST(", value, " AS FLOAT))")
    where <- c(src$where, paste0(value, " >= ", spec$valid_min))
  }
  stratum_select <- ""
  stratum_join <- ""
  stratum_group <- ""
  if (identical(spec$strata, "gender")) {
    stratum_join <- paste0(" INNER JOIN ", .qualifyTable(handle, "person"),
                           " p ON p.person_id = x.person_id")
    stratum_select <- ", p.gender_concept_id AS stratum"
    stratum_group <- ", p.gender_concept_id"
  }
  sql <- .sql_translate(paste0(
    "SELECT x.person_id AS person_id, ", value_sql, " AS v", stratum_select,
    src$from, stratum_join, .dsomopToolsWhere(where),
    " GROUP BY x.person_id", stratum_group),
    handle$target_dialect)
  raw <- .executeQuery(handle, sql)
  if (!is.data.frame(raw) || nrow(raw) == 0L) {
    return(data.frame(person_id = character(0), v = numeric(0),
                      stratum = integer(0), stringsAsFactors = FALSE))
  }
  names(raw) <- tolower(names(raw))
  if (anyDuplicated(as.character(raw$person_id))) {
    stop("A person-level reduction returned more than one row per person.",
         call. = FALSE)
  }
  raw$v <- suppressWarnings(as.numeric(raw$v))
  raw <- raw[!is.na(raw$v) & raw$v >= spec$valid_min, , drop = FALSE]
  if (!"stratum" %in% names(raw)) raw$stratum <- rep(NA_integer_, nrow(raw))
  raw
}

.dsomopToolsBinFrame <- function(breaks) {
  k <- length(breaks)
  format_break <- function(x) format(x, scientific = FALSE, trim = TRUE)
  upper <- c(breaks[-1L], NA_real_)
  label <- ifelse(
    is.na(upper),
    paste0("[", format_break(breaks), ", Inf)"),
    paste0("[", format_break(breaks), ", ", format_break(upper), ")")
  )
  data.frame(bin = seq_len(k), bin_label = label,
             bin_lower = as.numeric(breaks), bin_upper = as.numeric(upper),
             stringsAsFactors = FALSE)
}

# The complete public cells of a histogram question, with the exact count of
# persons in each cell.
.dsomopToolsHistogramCells <- function(handle, spec, concept_id) {
  values <- .dsomopToolsPersonValues(handle, spec, concept_id)
  bins <- .dsomopToolsBinFrame(spec$breaks)
  bin <- findInterval(values$v, spec$breaks)
  if (identical(spec$strata, "gender")) {
    genders <- .dsomopToolsGenders()
    stratum <- suppressWarnings(as.integer(values$stratum))
    stratum[is.na(stratum) | !stratum %in% genders$gender_concept_id] <- 0L
    labels <- genders[rep(seq_len(nrow(genders)), each = nrow(bins)), ,
                      drop = FALSE]
    cells <- cbind(labels, bins[rep(seq_len(nrow(bins)), nrow(genders)), ,
                                drop = FALSE])
    key <- paste(stratum, bin)
    cell_key <- paste(cells$gender_concept_id, cells$bin)
  } else {
    label_cols <- .dsomopToolsLabelCols(spec)
    concept_label <- if (!is.null(concept_id) &&
                         identical(concept_id, spec$all_concepts_value %||% -1L)) {
      "All visit types"
    } else {
      .dsomopToolsConceptName(handle, concept_id)
    }
    labels <- stats::setNames(
      data.frame(rep(concept_id, nrow(bins)), rep(concept_label, nrow(bins)),
                 stringsAsFactors = FALSE),
      label_cols
    )
    cells <- cbind(labels, bins)
    key <- as.character(bin)
    cell_key <- as.character(cells$bin)
  }
  counts <- tabulate(match(key, cell_key), nbins = nrow(cells))
  rownames(cells) <- NULL
  list(cells = cells, counts = as.numeric(counts))
}

.dsomopToolsReferenceYear <- function() {
  as.integer(format(Sys.Date(), "%Y"))
}

# The complete public cells of a person partition, with exact counts.
.dsomopToolsPartitionCells <- function(handle, spec) {
  person <- .qualifyTable(handle, "person")
  if (identical(spec$partition, "year_of_birth")) {
    reference_year <- .dsomopToolsReferenceYear()
    raw <- .executeQuery(handle, .sql_translate(paste0(
      "SELECT p.year_of_birth AS cell, COUNT(DISTINCT p.person_id) AS n",
      " FROM ", person, " p GROUP BY p.year_of_birth"),
      handle$target_dialect))
    domain <- c(seq.int(.DSOMOP_TOOLS_FIRST_BIRTH_YEAR, reference_year),
                NA_integer_)
    cells <- data.frame(year_of_birth = domain)
    cell <- suppressWarnings(as.integer(raw$cell))
    cell[!is.na(cell) & (cell < .DSOMOP_TOOLS_FIRST_BIRTH_YEAR |
                           cell > reference_year)] <- NA_integer_
    key <- ifelse(is.na(cell), "unknown", as.character(cell))
    cell_key <- ifelse(is.na(domain), "unknown", as.character(domain))
    extra <- list(reference_year = reference_year)
  } else if (identical(spec$partition, "month_of_birth")) {
    raw <- .executeQuery(handle, .sql_translate(paste0(
      "SELECT p.month_of_birth AS cell, COUNT(DISTINCT p.person_id) AS n",
      " FROM ", person, " p WHERE p.month_of_birth IS NOT NULL",
      " GROUP BY p.month_of_birth"),
      handle$target_dialect))
    domain <- c(seq_len(12L), NA_integer_)
    cells <- data.frame(month_of_birth = domain)
    cell <- suppressWarnings(as.integer(raw$cell))
    cell[!is.na(cell) & (cell < 1L | cell > 12L)] <- NA_integer_
    key <- ifelse(is.na(cell), "unknown", as.character(cell))
    cell_key <- ifelse(is.na(domain), "unknown", as.character(domain))
    extra <- list()
  } else {
    reference_year <- .dsomopToolsReferenceYear()
    raw <- .executeQuery(handle, .sql_translate(paste0(
      "SELECT p.gender_concept_id AS gender, p.year_of_birth AS yob,",
      " COUNT(DISTINCT p.person_id) AS n FROM ", person, " p",
      " GROUP BY p.gender_concept_id, p.year_of_birth"),
      handle$target_dialect))
    genders <- .dsomopToolsGenders()
    groups <- .DSOMOP_TOOLS_AGE_GROUPS
    cells <- data.frame(
      gender_concept_id = rep(genders$gender_concept_id, each = length(groups)),
      gender_name = rep(genders$gender_name, each = length(groups)),
      age_group = rep(groups, nrow(genders)),
      stringsAsFactors = FALSE
    )
    gender <- suppressWarnings(as.integer(raw$gender))
    gender[is.na(gender) | !gender %in% genders$gender_concept_id] <- 0L
    age <- reference_year - suppressWarnings(as.integer(raw$yob))
    group <- rep("unknown", length(age))
    known <- !is.na(age) & age >= 0L
    group[known] <- groups[pmin(age[known] %/% 10L, 9L) + 1L]
    key <- paste(gender, group)
    cell_key <- paste(cells$gender_concept_id, cells$age_group)
    extra <- list(reference_year = reference_year)
  }
  index <- match(key, cell_key)
  counts <- numeric(nrow(cells))
  if (length(index) > 0L) {
    sums <- tapply(suppressWarnings(as.numeric(raw$n)), index, sum)
    counts[as.integer(names(sums))] <- as.numeric(sums)
  }
  rownames(cells) <- NULL
  list(cells = cells, counts = counts, extra = extra)
}

# The standard summary: per-stratum person and record counts with the mean and
# SD of the template's value, computed from SQL sufficient statistics.
.dsomopToolsSummary <- function(handle, spec, concept_id) {
  s <- spec$summary
  src <- .dsomopToolsFromSql(handle, spec, concept_id)
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)
  group_select <- character(0)
  group_by <- character(0)
  joins <- ""
  if (s$group %in% c("concept", "all_concepts", "visit")) {
    name_col <- if (identical(s$group, "visit")) "visit_name" else
      "concept_name"
    group_select <- c(paste0("x.", s$concept_col, " AS ", s$concept_col),
                      paste0("c.concept_name AS ", name_col))
    group_by <- c(paste0("x.", s$concept_col), "c.concept_name")
    joins <- paste0(" LEFT JOIN ", concept, " c ON c.concept_id = x.",
                    s$concept_col)
  }
  if (identical(s$split, "unit")) {
    group_select <- c(group_select, "x.unit_concept_id AS unit_concept_id",
                      "u.concept_name AS unit_name")
    group_by <- c(group_by, "x.unit_concept_id", "u.concept_name")
    joins <- paste0(joins, " LEFT JOIN ", concept,
                    " u ON u.concept_id = x.unit_concept_id")
  } else if (identical(s$split, "currency")) {
    group_select <- c(group_select,
                      "co.currency_concept_id AS currency_concept_id",
                      "cur.concept_name AS currency_name")
    group_by <- c(group_by, "co.currency_concept_id", "cur.concept_name")
    joins <- paste0(joins, " LEFT JOIN ", concept,
                    " cur ON cur.concept_id = co.currency_concept_id")
  }
  if (identical(s$group, "gender")) {
    joins <- paste0(" INNER JOIN ", .qualifyTable(handle, "person"),
                    " p ON p.person_id = x.person_id LEFT JOIN ", concept,
                    " c ON c.concept_id = p.gender_concept_id")
    group_select <- c("p.gender_concept_id AS gender_concept_id",
                      "c.concept_name AS gender_name")
    group_by <- c("p.gender_concept_id", "c.concept_name")
  } else if (identical(s$group, "start_year")) {
    year <- .omopYearExpr(handle, "x.observation_period_start_date")
    group_select <- paste0(year, " AS start_year")
    group_by <- year
  }
  if (identical(s$reducer, "person_count")) {
    sql <- paste0(
      "SELECT COUNT(*) AS n_persons, COUNT(*) AS n_records,",
      " SUM(CAST(pp.v AS FLOAT)) AS s1,",
      " SUM(CAST(pp.v AS FLOAT) * CAST(pp.v AS FLOAT)) AS s2",
      " FROM (SELECT x.person_id AS person_id, COUNT(*) AS v", src$from,
      .dsomopToolsWhere(src$where), " GROUP BY x.person_id) pp")
  } else {
    value <- paste0("CAST(", spec$value(handle), " AS FLOAT)")
    select <- c(group_select,
                "COUNT(DISTINCT x.person_id) AS n_persons",
                "COUNT(*) AS n_records",
                paste0("SUM(", value, ") AS s1"),
                paste0("SUM(", value, " * ", value, ") AS s2"))
    sql <- paste0(
      "SELECT ", paste(select, collapse = ", "), src$from, joins,
      .dsomopToolsWhere(src$where),
      if (length(group_by)) paste0(" GROUP BY ", paste(group_by,
                                                      collapse = ", ")))
  }
  raw <- .executeQuery(handle, .sql_translate(sql, handle$target_dialect))
  columns <- s$cols
  if (!is.data.frame(raw) || nrow(raw) == 0L) {
    return(.dsomopToolsEmptyFrame(columns))
  }
  names(raw) <- tolower(names(raw))
  n_persons <- suppressWarnings(as.numeric(raw$n_persons))
  n <- suppressWarnings(as.numeric(raw$n_records))
  s1 <- suppressWarnings(as.numeric(raw$s1))
  s2 <- suppressWarnings(as.numeric(raw$s2))
  if (identical(s$reducer, "person_count")) n <- n_persons
  keep <- !is.na(n) & n > 0
  raw <- raw[keep, , drop = FALSE]
  n_persons <- n_persons[keep]; n <- n[keep]; s1 <- s1[keep]; s2 <- s2[keep]
  if (nrow(raw) == 0L) return(.dsomopToolsEmptyFrame(columns))
  mean_value <- s1 / n
  variance <- ifelse(n > 1, (s2 - s1 * s1 / n) / (n - 1), NA_real_)
  sd_value <- sqrt(pmax(variance, 0))
  out <- raw[, intersect(names(raw), columns), drop = FALSE]
  out$n_persons <- n_persons
  if (!is.null(s$n_col)) out[[s$n_col]] <- n
  out[[s$mean_col]] <- mean_value
  if (!is.null(s$sd_col)) out[[s$sd_col]] <- sd_value
  for (col in setdiff(columns, names(out))) out[[col]] <- NA
  out <- out[, columns, drop = FALSE]
  rownames(out) <- NULL
  out
}

.dsomopToolsEmptyFrame <- function(columns) {
  out <- stats::setNames(rep(list(logical(0)), length(columns)), columns)
  as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE,
                check.names = FALSE)
}

# Canonical public identity of a DP release: the question, its public
# parameters and cells, the reducer and the adapter version. Output names,
# ordering and the caller never enter it.
.dsomopToolsSemantic <- function(entry, spec, concept_id, cells, extra) {
  cell_keys <- vapply(seq_len(nrow(cells)), function(i) {
    paste(vapply(cells[i, , drop = FALSE], function(x) {
      if (is.na(x)) "NA" else as.character(x)
    }, character(1L)), collapse = "|")
  }, character(1L))
  list(
    statistic = paste0("tools_person_", spec$kind),
    adapter = "tools_auto",
    adapter_version = .DSOMOP_TOOLS_ADAPTER_VERSION,
    question = entry$meta$query_id,
    population = list(table = spec$table %||% "person",
                      filters = spec$where %||% character(0),
                      concept_column = spec$concept_col %||% "",
                      concept_id = if (is.null(concept_id)) "" else
                        as.character(concept_id)),
    reducer = spec$reducer %||% "partition",
    valid_min = spec$valid_min %||% "",
    breaks = spec$breaks %||% numeric(0),
    strata = spec$strata %||% spec$partition %||% "",
    reference_year = extra$reference_year %||% "",
    interval_contract = "left_closed_right_open_last_open",
    levels = cell_keys,
    unit = "distinct_person"
  )
}

.dsomopToolsDpRelease <- function(handle, entry, policy, concept_id,
                                  cells, counts, extra = list()) {
  spec <- entry$meta$tools_spec
  semantic <- .dsomopToolsSemantic(entry, spec, concept_id, cells, extra)
  release <- .dsomopDpRelease(
    policy = policy, semantic = semantic,
    bounded_snapshot = list(counts = as.numeric(counts)),
    sensitivity = list(l1 = 1, unit = "person"),
    payload_fn = function(epsilon, policy, release_context) {
      noisy <- vapply(seq_along(counts), function(i) {
        .dsomopDpNoisyInteger(counts[[i]], policy, release_context,
                              sprintf("tools-cell-%08d", i), epsilon, 1)
      }, numeric(1L))
      list(statistic = semantic$statistic, counts = as.numeric(noisy))
    }
  )
  noisy <- as.numeric(release$counts)
  if (length(noisy) != length(counts) || anyNA(noisy)) {
    stop("The DP release does not match its public cells.", call. = FALSE)
  }
  list(counts = noisy, epsilon = as.numeric(release$epsilon),
       delta = as.numeric(release$delta %||% 0),
       mechanism = as.character(release$mechanism %||% policy$mechanism))
}

#' Compute one run of a reviewed question
#'
#' @param handle CDM handle.
#' @param ctx Run-path context; \code{ctx$tools_route} and \code{ctx$entry}.
#' @param params Sanitized parameters.
#' @return Data frame of the route's shape, with attribute
#'   \code{dsomop_tools_protection} ("dp", "standard_fallback" or "standard").
#' @keywords internal
.dsomopToolsAutoCompute <- function(handle, ctx, params) {
  entry <- ctx$entry
  route <- ctx$tools_route
  if (is.null(entry) || is.null(route)) {
    stop("Reviewed community-tool questions run only through the analysis ",
         "run path.", call. = FALSE)
  }
  spec <- entry$meta$tools_spec
  concept_id <- .dsomopToolsConceptParam(spec, params)
  shape <- route$shape
  protection <- route$protection
  if (identical(shape, "summary")) {
    out <- .dsomopToolsSummary(handle, spec, concept_id)
    attr(out, "dsomop_tools_protection") <- "standard"
    return(out)
  }
  built <- if (identical(shape, "histogram")) {
    .dsomopToolsHistogramCells(handle, spec, concept_id)
  } else {
    .dsomopToolsPartitionCells(handle, spec)
  }
  cells <- built$cells
  dp <- NULL
  if (identical(protection, "dp")) {
    dp <- tryCatch(
      .dsomopToolsDpRelease(handle, entry, route$policy, concept_id, cells,
                            built$counts, built$extra %||% list()),
      error = function(e) {
        if (identical(route$mode, "dp")) stop(e)
        message("[dsomop-tools] DP release unavailable, using the standard ",
                "gate: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(dp)) protection <- "standard_fallback"
  }
  if (identical(protection, "dp")) {
    cells$n_persons <- dp$counts
    attr(cells, "dsomop_tools_protection") <- "dp"
    attr(cells, "dsomop_tools_dp") <- dp
  } else {
    cells$n_persons <- built$counts
    if (identical(shape, "partition")) {
      # Standard partitions release populated cells only, as the template did.
      cells <- cells[cells$n_persons > 0, , drop = FALSE]
      rownames(cells) <- NULL
    }
    attr(cells, "dsomop_tools_protection") <- protection
  }
  attr(cells, "dsomop_tools_extra") <- built$extra %||% list()
  cells
}

# --- Protection labels -------------------------------------------------------------

# Standard deviation of discrete-Laplace noise for a given epsilon and
# sensitivity (public).
.dsomopToolsNoiseSd <- function(epsilon, sensitivity = 1) {
  a <- exp(-epsilon / sensitivity)
  sqrt(2 * a) / (1 - a)
}

#' Attach the public protection label of an analysis result
#'
#' @param df Released data frame.
#' @param entry Catalog entry.
#' @param protection "dp", "standard_fallback" or "standard".
#' @param dp DP release metadata (DP only).
#' @param extra Public extras (e.g. reference year).
#' @return \code{df} with attribute \code{dsomop_protection}.
#' @keywords internal
.dsomopToolsLabel <- function(df, entry, protection, dp = NULL,
                              extra = list()) {
  spec <- entry$meta$tools_spec
  label <- list(
    protection = protection,
    mode = entry$meta$tools_mode %||% .dsomopToolsMode()
  )
  if (!is.null(spec)) {
    label$estimand <- spec$value_label
    if (!is.null(extra$reference_year)) {
      label$reference_year <- extra$reference_year
    }
  }
  if (identical(protection, "dp")) {
    label$mechanism <- dp$mechanism
    label$epsilon <- dp$epsilon
    label$delta <- dp$delta
    label$sensitivity <- 1
    label$adjacency <- "add_remove_person"
    label$sticky <- TRUE
    label$noise_sd <- .dsomopToolsNoiseSd(dp$epsilon, 1)
    label$note <- paste0(
      "Differential privacy: every count carries random noise (SD about ",
      format(round(label$noise_sd, 1), nsmall = 1), " persons); a repeated ",
      "request returns the same answer. Each person is counted once."
    )
  } else if (identical(protection, "standard_fallback")) {
    label$note <- paste0(
      "Standard DataSHIELD disclosure controls (the differential-privacy ",
      "service is not available on this server): small cells are removed ",
      "and counts are banded."
    )
  } else {
    label$note <- paste0(
      "Standard DataSHIELD disclosure controls: small cells are removed, ",
      "counts are banded and statistics need enough persons."
    )
  }
  attr(df, "dsomop_protection") <- label
  df
}

# --- Finalize ------------------------------------------------------------------------

#' Release one run of a reviewed question
#'
#' DP outputs are complete noisy tables of a public domain: the standard gate's
#' suppression and banding do not apply to them. Every other output (standard
#' summaries, standard partitions and the standard fallback of auto mode) goes
#' through the unified gate. A missing protection marker means the gate.
#'
#' @param handle CDM handle.
#' @param df Computed frame.
#' @param entry Catalog entry (adapter "tools_auto").
#' @param sanitized Sanitized parameters.
#' @return The released, labelled data frame.
#' @keywords internal
.dsomopToolsFinalize <- function(handle, df, entry, sanitized) {
  protection <- attr(df, "dsomop_tools_protection", exact = TRUE)
  dp <- attr(df, "dsomop_tools_dp", exact = TRUE)
  extra <- attr(df, "dsomop_tools_extra", exact = TRUE) %||% list()
  if (identical(protection, "dp") && is.list(dp)) {
    attributes(df) <- attributes(df)[c("names", "row.names", "class")]
  } else {
    if (!identical(protection, "standard_fallback")) protection <- "standard"
    df <- .omopAnalysisGate(handle, df, entry)
    df <- .dsomopToolsOrder(df, entry, sanitized)
    dp <- NULL
  }
  df <- .omopAnalysisNormalizePoolingOutput(df, entry)
  .dsomopToolsLabel(df, entry, protection, dp, extra)
}

# Standard outputs are ordered from released values and public keys only.
# Histograms and partitions keep the order of their public cells.
.dsomopToolsOrder <- function(df, entry, sanitized) {
  if (!is.data.frame(df) || nrow(df) < 2L ||
      !identical(entry$meta$tools_shape, "summary")) {
    return(df)
  }
  s <- entry$meta$tools_spec$summary
  keys <- setdiff(names(df), c("n_persons", s$n_col, s$mean_col, s$sd_col))
  if (identical(s$order, "n_persons")) {
    top_n <- if (isTRUE(s$top_n)) {
      spec <- Filter(function(p) identical(p$name, "top_n"), entry$params)
      default <- if (length(spec) == 1L) spec[[1L]]$default else NULL
      suppressWarnings(as.integer(sanitized$top_n %||% default))
    } else nrow(df)
    if (length(top_n) != 1L || is.na(top_n) || top_n < 1L) {
      stop("top_n must be a positive integer.", call. = FALSE)
    }
    return(.omopBandedTopN(df, support_cols = "n_persons", top_n = top_n,
                           key_cols = keys))
  }
  ord <- do.call(order, c(lapply(keys, function(k) df[[k]]),
                          list(na.last = TRUE, method = "radix")))
  out <- df[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}
