# Module: recipe-language capability block and plan explain (design-2.10.3
# sections 4.4, 6.15 and defect A-E7).

# The frozen operation and reducer vocabularies of section 4.4.
.RECIPE_OPERATIONS <- c(
  "concept_set", "event", "event.unit", "event.on_missing_table", "interval",
  "interval.ingredient", "interval.stockpile", "population.index",
  "population.first_eligible", "population.any_of", "population.criteria",
  "population.parent", "population.setop", "population.match",
  "population.coverage.union", "criteria.not", "criteria.age",
  "window.anchor.source", "variable.reducer", "variable.derived",
  "variable.raw", "variable.population_column", "output.wide", "output.long",
  "output.long.event_log", "output.survival", "output.survival.censor_source",
  "output.intervals", "output.temporal_covariates", "output.person_period",
  "output.person_period.partition", "output.sparse", "output.membership",
  "output.dictionary", "output.joined", "publish.roots",
  "event.episode_members", "event.cost", "reducer.supply_days",
  "reducer.covered_days.effective_supply", "status.accessor", "verb.mutate",
  "verb.filter_expr"
)
.RECIPE_REDUCERS <- c(
  "binary", "count", "n_distinct", "first_value", "last_value", "nth_value",
  "closest_value", "mean", "min", "max", "sum", "sd", "cv", "median",
  "quantile", "mode", "slope", "any", "all", "first_date", "last_date",
  "days_since", "days_until", "time_since", "duration_sum", "covered_days",
  "supply_days", "drug_duration", "gap_max", "gap_mean", "abnormal_high",
  "abnormal_low", "era_count", "era_days", "first_era_days"
)

# Language 2.0 execution needs the compilers of section 6 and the publication
# API of section 7.3 (WS-1, WS-4..WS-7); each operation family also needs its
# own compiler or endpoint. An operation is advertised only when everything
# it needs is installed, so the block stays truthful at every integration
# stage (a client sends v2 only for plans whose every operation is listed).
.RECIPE_CORE_FUNCTIONS <- c(".compileConceptSet", ".compileEvent",
                            ".compilePopulation", ".compileVariable",
                            ".compileOutput", ".planContext",
                            ".publishPrepare", ".publishCommit")
.RECIPE_OPERATION_FUNCTIONS <- list(
  interval = ".compileInterval", interval.ingredient = ".compileInterval",
  interval.stockpile = ".compileInterval",
  population.coverage.union = ".compileInterval",
  window.anchor.source = ".compileAnchor",
  population.match = ".compileMatch",
  event.episode_members = ".compileEpisodeMembers",
  event.cost = ".compileCost",
  status.accessor = "omopPlanStatusDS",
  verb.mutate = "omopMutateDS"
)

.recipeFunctionInstalled <- function(name) {
  is.function(get0(name, envir = asNamespace("dsOMOP"), inherits = FALSE))
}

#' Operations this server executes (section 4.4, minus disabled ones)
#' @keywords internal
.recipeAvailableOperations <- function() {
  if (!all(vapply(.RECIPE_CORE_FUNCTIONS, .recipeFunctionInstalled, logical(1)))) {
    return(character(0))
  }
  ok <- vapply(.RECIPE_OPERATIONS, function(op) {
    needs <- .RECIPE_OPERATION_FUNCTIONS[[op]]
    is.null(needs) || all(vapply(needs, .recipeFunctionInstalled, logical(1)))
  }, logical(1))
  setdiff(.RECIPE_OPERATIONS[ok], .irDisabledOperations())
}

#' The \code{recipe_language} capability block (design section 4.4, A-E7)
#'
#' @param handle CDM handle (for the \code{dialect} block), or NULL.
#' @return list(version, semantic_version, schema_id, operations, reducers,
#'   dialect). \code{version} is the language this server validates and
#'   explains; \code{operations} lists what it executes, so an empty list
#'   tells a client to emit v1 (or name the missing operation).
#' @keywords internal
.recipeLanguageCapability <- function(handle = NULL) {
  operations <- .recipeAvailableOperations()
  dialect <- NULL
  if (!is.null(handle)) {
    dialect <- tryCatch(.sqlDialectCapability(handle), error = function(e) NULL)
  }
  list(
    version = 2L,
    semantic_version = .IR_SEMANTIC_VERSION,
    schema_id = .IR_SCHEMA_ID,
    operations = operations,
    reducers = if ("variable.reducer" %in% operations) .RECIPE_REDUCERS else character(0),
    dialect = dialect
  )
}

# --- explain ------------------------------------------------------------------

# Tables an IR reads, with the events that read them.
.irTablesUsed <- function(ir) {
  tables <- character(0)
  for (e in ir[["events"]] %||% list()) {
    if (!is.null(e[["table"]])) tables <- c(tables, e[["table"]])
    for (t in unlist(e[["episode_members"]][["tables"]] %||% list())) tables <- c(tables, t)
    if (!is.null(e[["episode_members"]])) tables <- c(tables, "episode_event")
    if (!is.null(e[["cost"]])) tables <- c(tables, "cost")
  }
  for (v in ir[["variables"]] %||% list()) {
    if (!is.null(v[["raw"]][["table"]])) tables <- c(tables, v[["raw"]][["table"]])
  }
  .irSortNames(tables)
}

# Reducers whose result preserves an event date (relative to an anchor or a
# reference, so an exact date wherever that anchor is known): class 2.
.IR_DATE_REDUCERS <- c("first_date", "last_date", "days_since", "days_until", "time_since")
# Reducers returning one observed value of their value column: that column's
# class.
.IR_VALUE_REDUCERS <- c("first_value", "last_value", "nth_value", "closest_value",
                        "mode", "min", "max")

#' Assignment class of a reducer variable (section 7.6)
#'
#' From the reducer's result type: an exact-date-preserving reducer is class
#' 2; a reducer returning an observed value takes its value column's class
#' (a physical column of the source table; the normalised \code{value} is
#' class 3; the date columns of an event or an interval's course rows --
#' \code{start_date}, \code{end_date}, \code{end_recorded} -- are exact
#' dates, class 2); counts, flags and statistics are class 3.
#' @keywords internal
.irReducerClass <- function(ir, def, bp) {
  r <- def[["reducer"]]
  fn <- r[["fn"]] %||% ""
  if (fn %in% .IR_DATE_REDUCERS) return(2L)
  if (fn %in% .IR_VALUE_REDUCERS) {
    value <- r[["value"]]
    if (is.character(value) && value %in% c("start_date", "end_date", "end_recorded")) return(2L)
    src <- ir[["events"]][[def[["source"]][["event"]] %||% ""]]
    table <- src[["table"]]
    if (is.character(value) && !identical(value, "value") && is.character(table)) {
      return(.roleColumnClass(value, bp[["columns"]][[table]]))
    }
  }
  3L
}

# Columns each output projects, with their assignment class (section 7.6).
.irOutputColumnClasses <- function(ir, handle) {
  bp <- if (!is.null(handle)) tryCatch(.buildBlueprint(handle), error = function(e) NULL)
  out <- list()
  for (o in names(ir[["outputs"]])) {
    node <- ir[["outputs"]][[o]]
    cols <- list()
    for (v in .irOutputVariables(ir, o)) {
      def <- ir[["variables"]][[v]]
      if (!is.null(def[["raw"]])) {
        col_df <- bp[["columns"]][[def[["raw"]][["table"]]]]
        cols[[v]] <- list(source = paste0(def[["raw"]][["table"]], ".", def[["raw"]][["column"]]),
                          class = .roleColumnClass(def[["raw"]][["column"]], col_df))
      } else if (!is.null(def[["derived"]])) {
        kind <- def[["derived"]][["kind"]]
        cls <- if (kind %in% c("age", "birth_year_band")) 2L else 3L
        cols[[v]] <- list(source = paste0("derived:", kind), class = cls)
      } else if (!is.null(def[["reducer"]])) {
        cols[[v]] <- list(source = paste0("reducer:", def[["reducer"]][["fn"]]),
                          class = .irReducerClass(ir, def, bp))
      } else if (!is.null(def[["population_column"]])) {
        cols[[v]] <- list(source = paste0("population:", def[["population_column"]][["name"]]), class = 3L)
      }
    }
    if (node[["format"]] %in% c("long", "intervals")) {
      srcs <- .jsonArrayItems(node[["sources"]] %||% node[["options"]][["sources"]] %||% list())
      for (s in srcs) {
        ev <- ir[["events"]][[s[["event"]] %||% ""]]
        table <- ev[["table"]]
        if (is.null(table) || is.null(bp)) next
        col_df <- bp[["columns"]][[table]]
        if (is.null(col_df)) next
        for (cn in col_df[["column_name"]]) {
          cls <- .roleColumnClass(cn, col_df)
          if (cls < 2L) next
          cols[[paste0(s[["event"]], ".", cn)]] <- list(source = paste0(table, ".", cn), class = cls)
        }
      }
    }
    out[[o]] <- cols
  }
  out
}

# Effective defaults a reader must see (section 6.15).
.irEffectiveDefaults <- function(ir) {
  populations <- list()
  for (p in names(ir[["populations"]])) {
    pop <- ir[["populations"]][[p]]
    entry <- list(kind = pop[["kind"]])
    if (!is.null(pop[["coverage"]])) entry[["coverage"]] <- pop[["coverage"]]
    if (identical(pop[["kind"]], "index")) {
      entry[["select"]] <- pop[["index"]][["select"]]
      entry[["end"]] <- pop[["index"]][["end"]]
      entry[["restrict_to_observation"]] <- pop[["index"]][["restrict_to_observation"]]
    }
    criterion_coverage <- list()
    .irWalkCriteria(pop[["where"]], function(kind, leaf, path) {
      if (identical(kind, "observed") && !is.null(leaf[["gap"]])) {
        criterion_coverage[[path]] <<- list(policy = "union", on_ambiguous = "drop",
                                            on_missing = "drop", gap = leaf[["gap"]])
      }
    }, "")
    if (length(criterion_coverage)) entry[["criterion_coverage"]] <- criterion_coverage
    populations[[p]] <- entry
  }
  outputs <- list()
  for (o in names(ir[["outputs"]])) {
    node <- ir[["outputs"]][[o]]
    opts <- node[["options"]] %||% list()
    entry <- list(format = node[["format"]], grain = node[["grain"]])
    entry["collapse"] <- list(node[["collapse"]])
    if (!is.null(opts[["date_handling"]])) entry[["date_handling"]] <- opts[["date_handling"]]
    if (identical(node[["format"]], "survival")) {
      entry["tar"] <- list(opts[["tar"]])
      entry[["tars"]] <- opts[["tars"]]
      entry[["censor"]] <- opts[["censor"]]
      entry[["format_survival"]] <- opts[["format"]]
      entry[["event_order"]] <- opts[["event_order"]]
      entry[["tie_policy"]] <- opts[["tie_policy"]]
      entry[["cohort_end_cap"]] <- isTRUE(opts[["legacy_cohort_end_cap"]])
    }
    if (identical(node[["format"]], "person_period")) entry[["representation"]] <- opts[["representation"]]
    outputs[[o]] <- entry
  }
  reducers <- list()
  for (v in names(ir[["variables"]])) {
    r <- ir[["variables"]][[v]][["reducer"]]
    if (is.list(r)) reducers[[v]] <- list(fn = r[["fn"]], missing = r[["missing"]])
  }
  list(populations = populations, outputs = outputs, reducers = reducers)
}

#' Explain payload of a validated IR (design section 6.15)
#'
#' The normalised plan, the role records used, the column classes of the
#' projected columns, the effective defaults, the publication roots and the
#' dialect preflight result -- no counts, no SQL, no patient data.
#'
#' @param ir The IR (through steps 1 to 3 and the semantic rules).
#' @param handle CDM handle, or NULL for a site-free explanation.
#' @param v1_plan For a lowered v1 IR, the v1 plan it was lowered from: its
#'   executors' site findings (\code{.planV1SiteFindings()}) come first in
#'   \code{site_findings}, as execution checks them first.
#' @return Named list (see Details).
#' @keywords internal
.irExplain <- function(ir, handle = NULL, v1_plan = NULL) {
  report <- .irValidate(ir, NULL)
  site_findings <- character(0)
  unavailable <- character(0)
  unavailable_outputs <- character(0)
  roles <- list()
  dialect <- list(status = "not_checked")
  if (!is.null(handle)) {
    if (!is.null(v1_plan)) {
      legacy <- tryCatch(.planV1SiteFindings(v1_plan, handle),
                         error = function(e) list(errors = conditionMessage(e)))
      site_findings <- c(legacy[["declaration"]], legacy[["errors"]], legacy[["missing_tables"]])
    }
    site <- tryCatch(.irSiteErrors(ir, handle),
                     error = function(e) list(errors = conditionMessage(e), unavailable = character(0)))
    site_findings <- c(site_findings, site[["errors"]], site[["missing_tables"]])
    unavailable <- site[["unavailable"]]
    unavailable_outputs <- .irUnavailableOutputs(ir, unavailable)
    for (t in .irTablesUsed(ir)) {
      roles[[t]] <- .roleRecordSummary(.roleOf(handle, t))
    }
    dialect <- tryCatch({
      profile <- .irDialectPreflight(ir, handle)
      list(status = "ok", family = profile[["family"]],
           temporary_materialization = profile[["temporary_materialization"]],
           runtime = profile[["runtime"]])
    }, dsomop_dialect_unsupported = function(e) {
      list(status = "dialect_unsupported", output = e[["output"]],
           construct = e[["construct"]], family = e[["family"]])
    }, error = function(e) list(status = "error", message = conditionMessage(e)))
  }
  list(
    language_version = .IR_LANGUAGE_VERSION,
    source_version = attr(ir, "source_version"),
    plan = .irDocument(ir),
    semantic_digest = .planSemanticDigest(.irDocument(ir))[["sha256"]],
    operations = report[["operations"]],
    publish = ir[["publish"]],
    defaults = .irEffectiveDefaults(ir),
    roles = roles,
    column_classes = .irOutputColumnClasses(ir, handle),
    site_findings = site_findings,
    unavailable = unavailable,
    unavailable_outputs = unavailable_outputs,
    dialect = dialect,
    legacy_annex = attr(ir, "legacy_annex"),
    order = .irOrder(ir)[c("concept_sets", "events", "intervals",
                           "populations", "variables", "outputs")]
  )
}

#' Explain a plan without executing it (Aggregate)
#'
#' @description
#' Runs the plan through the single validation sequence of the 2.10.3 recipe
#' engine -- the closed envelope of its version (language 2.0 plans against
#' \code{plan-v2.json}, v1 plans against the corrected v1 validators),
#' normalisation (v1 plans are lowered to the compatibility IR), the IR
#' schema and the semantic rules -- and returns what the server would
#' compute: the normalised plan with every default expanded, the role records
#' of the tables it reads, the column classes of the columns it would
#' project, the effective defaults (coverage, TAR, censoring, reducer
#' \code{missing}, \code{collapse}, \code{representation}), the publication
#' roots and the dialect preflight result. It returns no counts, no SQL and
#' no patient data, and creates nothing. An invalid plan is refused with the
#' same typed validation error execution would raise.
#'
#' @param omop_symbol Character; the OMOP handle symbol.
#' @param plan The plan (language 2.0 or v1), as a list or encoded JSON.
#' @return Named list: \code{language_version}, \code{source_version} (1 or
#'   2), \code{plan} (the normalised plan), \code{semantic_digest},
#'   \code{operations}, \code{publish}, \code{defaults}, \code{roles},
#'   \code{column_classes}, \code{site_findings}, \code{unavailable},
#'   \code{unavailable_outputs}, \code{dialect}, \code{legacy_annex} (the
#'   advisory v1 fields kept outside the IR document, covered by no digest;
#'   amendment A-10) and \code{order}.
#' @examples
#' \dontrun{
#' omopPlanExplainDS("omop", plan)
#' }
#' @export
omopPlanExplainDS <- function(omop_symbol, plan) {
  handle <- .getHandle(omop_symbol)
  plan <- .ds_arg(plan)
  .omopAuditLog("omopPlanExplainDS", list(plan = .irJsonForm(plan)))
  json <- .irJsonForm(plan)
  .irValidateEnvelope(json)
  ir <- .irNormalize(json)
  .irValidateSchema(ir)
  .irExplain(ir, handle, v1_plan = if (!identical(attr(ir, "source_version"), 2L)) json)
}
