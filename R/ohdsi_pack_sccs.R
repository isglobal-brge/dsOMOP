# Module: Self-Controlled Case Series (SCCS) catalog entries (native live compute)
#
# Group OHDSI-B2-sccs-new. The four SCCS result tables, ported from the
# precomputed-results adapter (.ohdsiGetResults reading sccs_attrition /
# sccs_result / sccs_covariate_result / sccs_diagnostics_summary) to native,
# live-computing catalog entries that COMPUTE each DESCRIPTIVE substrate directly
# from the CDM over the SCOPED case population (cohort-wide when un-scoped,
# preserving the database-characterization semantics of the legacy adapter).
#
# Each legacy OHDSI id (dsomop:ohdsi.sccs.<table>) is KEPT as a thin alias that
# delegates to a NEW canonical native entry (dsomop:sccs.<metric>), exactly the
# B1 alias pattern (ohdsi_pack_aliases.R): the alias inherits the canonical
# entry's compute fn, disclosure spec, params and scope, so the SINGLE
# .omopAnalysisGate processes the delegated frame identically to the canonical
# id. No precomputed results table is read anywhere in this group.
#
# What is computable LIVE vs out of scope:
#   * sccs_attrition  -> dsomop:sccs.attrition: case-cohort attrition (outcome
#     subjects / events / observation periods / observed days remaining per
#     inclusion step). FULLY recomputable from counts.
#   * sccs_result     -> dsomop:sccs.outcome_rate_per_month: observed outcome
#     events + person-time per calendar month + the descriptive monthly rate.
#     This is the DESCRIPTIVE substrate, NOT the fitted Poisson incidence-rate
#     ratio (the conditional Poisson IRR + CI need the fitted model and are OUT).
#   * sccs_covariate_result -> dsomop:sccs.count_histograms: per-covariate
#     (here: exposure-window bin) exposed/unexposed event + person counts, the
#     COUNT substrate, NOT the spline relative-risk curve (the fitted spline RR
#     is OUT).
#   * sccs_diagnostics_summary -> dsomop:sccs.assumption_checks: rare-outcome,
#     time-stability and event-dependence TEST SCALARS recomputed over the
#     underlying counts (pre-exposure rate-ratio + calendar time-trend are
#     recomputable); any check needing the FITTED model is OUT.
#
# Disclosure: every entry is unit="record" with meta$adapter="ohdsi_live" (NOT
# "ohdsi") so the gate routes through its GENERIC record branch
# (analysis_catalog.R L3278-3291), gating on the distinct-person companion
# n_persons that travels IN the frame over the SAME strata (NOT the precomputed
# .ohdsiPersonGate, which expects a sibling results table). Every derived
# rate/ratio is reconciled in-fn via .omopAnalysisReconcileRatio BEFORE return,
# so a raw ratio over banded counts is never released. Concept ids are translated
# to names by default; no *_source_value / free-text is ever selected.
#
# The registrar .ohdsiPackSccsEntries(handle) returns BOTH the four canonical
# dsomop:sccs.* entries AND the four dsomop:ohdsi.sccs.* aliases keyed by id, so
# a single registrar addition wires the whole group; it takes handle for
# signature parity ONLY and performs NO DB I/O at build time (catalog build is
# cached on handle$analysis_catalog; all queries run later inside each entry's
# compute$fn).

# --- Shared SCCS substrate resolvers -----------------------------------------

#' Resolve the SCCS outcome event source over the scoped case population
#'
#' SCCS is built around an OUTCOME: the case population is persons who experience
#' the outcome at least once. The outcome is supplied either as an
#' \code{outcome_concept_id} (descendants expanded server-side, matched in the
#' domain table chosen by \code{domain_code}) via \code{\link{.omopOutcomeSource}},
#' or — when no concept is supplied — implicitly as the scoped cohort itself
#' (every scoped person is a case, the outcome being whatever defined the cohort).
#' Returns the qualified outcome table + its person/date/concept columns and the
#' concept IN-list, or NULL when no concept id was supplied (the caller then
#' treats the cohort as the case set).
#'
#' @param handle CDM handle.
#' @param params Sanitized params (reads \code{outcome_concept_id},
#'   \code{outcome_domain_code}).
#' @return The \code{\link{.omopOutcomeSource}} list, or NULL.
#' @keywords internal
.omopSccsOutcomeSource <- function(handle, params) {
  .omopOutcomeSource(handle, params$outcome_concept_id,
                     params$outcome_domain_code %||% "0")
}

#' WHERE predicate restricting outcome rows to the concept IN-list (or "")
#' @keywords internal
.omopSccsOutcomeWhere <- function(out_src, alias = "o") {
  if (is.null(out_src)) return("")
  paste0(alias, ".", out_src$concept_col, " IN (", out_src$id_list, ")")
}

# --- Canonical 1: dsomop:sccs.attrition --------------------------------------

#' \code{dsomop:sccs.attrition} entry (SCCS case-cohort attrition, live)
#'
#' Recomputes the SCCS attrition table — the population remaining after each
#' inclusion step — directly from the CDM over the scoped case population
#' (cohort-wide when un-scoped). One row per inclusion step, each carrying the
#' distinct outcome SUBJECTS remaining (the person companion) plus the record
#' substrate: outcome EVENTS, distinct OBSERVATION PERIODS, and OBSERVED DAYS of
#' person-time. Steps mirror the SCCS pipeline's standard inclusion sequence:
#' \enumerate{
#'   \item \code{all_cases}: every person in scope with >= 1 outcome event;
#'   \item \code{has_observation_period}: cases whose outcome falls inside an
#'     observation period (the SCCS person-time requirement);
#'   \item \code{sufficient_observation}: cases with >= \code{min_observed_days}
#'     of observed person-time (default 1 day).
#' }
#' @keywords internal
.omopSccsAttrition <- function() {
  name <- "dsomop:sccs.attrition"
  plot_code <- paste(
    "function(df, params) {",
    "  df$step <- factor(df$step, levels = df$step)",
    "  ggplot2::ggplot(df, ggplot2::aes(x = step, y = outcome_subjects)) +",
    "    ggplot2::geom_col() + ggplot2::coord_flip() +",
    "    ggplot2::labs(x = 'Inclusion step', y = 'Outcome subjects remaining')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    obs     <- .qualifyTable(handle, "observation_period")
    out_src <- .omopSccsOutcomeSource(handle, params)
    min_days <- max(as.integer(params$min_observed_days %||% "1"), 0L)

    # The case substrate: the outcome event table (when a concept was supplied)
    # restricted to the scoped cohort, else the scoped cohort itself (every
    # scoped person a case). Both resolve to (person_id, event_date) over which
    # the inclusion steps count distinct persons / events / obs-periods / days.
    if (!is.null(out_src)) {
      ev_tbl  <- out_src$table
      ev_pers <- out_src$person_col
      ev_date <- out_src$date_col
      ev_where <- paste0(" WHERE ", .omopSccsOutcomeWhere(out_src, "o"))
      sj <- .omopScopeJoin(ctx, "o", ev_pers)
      # Self-gate the scoped + outcome-filtered case population before any rows.
      .omopDiagAssertPersons(handle, ctx, ev_tbl, "o", ev_pers,
                             where_sql = .omopSccsOutcomeWhere(out_src, "o"))
    } else {
      # No outcome concept: the cohort IS the case set. Un-scoped + no outcome =
      # no case definition at all -> gate-safe empty frame.
      if (is.null(ctx$scoped_cohort)) return(data.frame())
      cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
      ev_tbl  <- cohort
      ev_pers <- "subject_id"
      ev_date <- "cohort_start_date"
      ev_where <- ""
      sj <- list(join = "", cohort = cohort)
      .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
        "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
        handle$target_dialect))
    }

    # A reusable case sub-select: (person_id, event_date) for every qualifying
    # outcome event in scope. The scope INNER JOIN (sj$join) restricts to the
    # cohort when scoped; "" = whole-DB (all cases) when un-scoped.
    case_sql <- paste0(
      "SELECT o.", ev_pers, " AS person_id, o.", ev_date, " AS event_date ",
      "FROM ", ev_tbl, " o", sj$join, ev_where)

    observed_days <- .omopDateDiffDays(
      handle, "op.observation_period_end_date",
      "op.observation_period_start_date")

    # Step 1: all cases (>= 1 outcome event in scope). Events come from the case
    # set directly (COUNT(*)); the observation-period metrics come from a
    # separate aggregate over the DISTINCT case persons joined to ALL their
    # observation periods (combined in R), so neither count fans out the other
    # and every value is computed honestly (no artificial zeros) — a legitimately
    # non-empty starting step is then never dropped by the gate's small-count
    # pass.
    s1_events <- .sql_translate(paste0(
      "SELECT COUNT(DISTINCT cse.person_id) AS outcome_subjects, ",
      "COUNT(*) AS outcome_events ",
      "FROM (", case_sql, ") cse"),
      handle$target_dialect)
    s1_obs <- .sql_translate(paste0(
      "SELECT COUNT(DISTINCT op.observation_period_id) AS outcome_observation_periods, ",
      "SUM(", observed_days, ") AS observed_days ",
      "FROM (SELECT DISTINCT person_id FROM (", case_sql, ") c0) cse ",
      "INNER JOIN ", obs, " op ON op.person_id = cse.person_id"),
      handle$target_dialect)

    # Step 2: cases whose outcome event falls inside an observation period, with
    # the distinct observation periods and total observed person-time those
    # periods contribute (the SCCS person-time requirement).
    s2 <- .sql_translate(paste0(
      "SELECT COUNT(DISTINCT cse.person_id) AS outcome_subjects, ",
      "COUNT(*) AS outcome_events, ",
      "COUNT(DISTINCT op.observation_period_id) AS outcome_observation_periods, ",
      "SUM(", observed_days, ") AS observed_days ",
      "FROM (", case_sql, ") cse ",
      "INNER JOIN ", obs, " op ON op.person_id = cse.person_id ",
      "AND cse.event_date >= op.observation_period_start_date ",
      "AND cse.event_date <= op.observation_period_end_date"),
      handle$target_dialect)

    # Step 3: cases with sufficient observed person-time (>= min_observed_days).
    s3 <- .sql_translate(paste0(
      "SELECT COUNT(DISTINCT cse.person_id) AS outcome_subjects, ",
      "COUNT(*) AS outcome_events, ",
      "COUNT(DISTINCT op.observation_period_id) AS outcome_observation_periods, ",
      "SUM(", observed_days, ") AS observed_days ",
      "FROM (", case_sql, ") cse ",
      "INNER JOIN ", obs, " op ON op.person_id = cse.person_id ",
      "AND cse.event_date >= op.observation_period_start_date ",
      "AND cse.event_date <= op.observation_period_end_date ",
      "WHERE (", observed_days, ") >= ", min_days),
      handle$target_dialect)

    one <- function(sql, step) {
      r <- .executeQuery(handle, sql)
      if (!is.data.frame(r) || nrow(r) == 0) return(NULL)
      r$step <- step
      r
    }
    # Step 1 combines its two scalars (events + obs metrics) into one row.
    s1_step <- {
      ev <- .executeQuery(handle, s1_events)
      ob <- .executeQuery(handle, s1_obs)
      if (!is.data.frame(ev) || nrow(ev) == 0) NULL else {
        data.frame(
          outcome_subjects            = ev$outcome_subjects[1],
          outcome_events              = ev$outcome_events[1],
          outcome_observation_periods = if (is.data.frame(ob) && nrow(ob) > 0)
            ob$outcome_observation_periods[1] else 0L,
          observed_days               = if (is.data.frame(ob) && nrow(ob) > 0)
            ob$observed_days[1] else 0L,
          step = "all_cases", stringsAsFactors = FALSE)
      }
    }
    parts <- Filter(Negate(is.null), list(
      s1_step,
      one(s2, "has_observation_period"),
      one(s3, "sufficient_observation")))
    if (length(parts) == 0) return(data.frame())
    out <- do.call(rbind, parts)
    out <- out[, c("step", "outcome_subjects", "outcome_events",
                   "outcome_observation_periods", "observed_days"),
               drop = FALSE]
    # Couple observed_days (a person-time SUM) to its person basis. The gate
    # small-cell-suppresses on the RAW outcome_subjects (>= nfilter_tab) and only
    # THEN bands it, so a case group in [nfilter_tab, nfilter_band) survives
    # suppression yet FLOORS to 0 — at which point observed_days (which banding to a
    # multiple of nfilter_band barely perturbs for a multi-thousand-day sum) would
    # be the sole surviving signal of that sub-band group's existence + aggregate
    # person-time. Mirror the gate's binary-prevalence coupling here (in-fn, the
    # sanctioned place for a derived/coupled value, exactly as the rate entries
    # reconcile their ratio): NA observed_days wherever the BANDED person basis is
    # NA-or-0, so observed_days can never outlive a suppressed/banded-away count.
    settings <- .omopDisclosureSettings()
    band_subj <- vapply(as.numeric(out$outcome_subjects), .bandCount, numeric(1),
                        band_width = settings$nfilter_band)
    band_subj[is.na(out$outcome_subjects) |
                as.numeric(out$outcome_subjects) < settings$nfilter_tab] <- NA_real_
    out$observed_days[is.na(band_subj) | band_subj == 0] <- NA_real_
    rownames(out) <- NULL
    out
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("SCCS case-cohort attrition: outcome subjects / events ",
                         "/ observation periods / observed person-days remaining ",
                         "after each inclusion step, over the scoped case ",
                         "population (cohort-wide when un-scoped)."),
    domain      = "general",
    params      = list(
      list(name = "outcome_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Outcome concept id (descendants expanded) defining cases."),
      list(name = "outcome_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4"),
           description = "Outcome domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "min_observed_days", type = "int", required = FALSE,
           default = "1",
           description = "Minimum observed person-days for the final inclusion step.")
    ),
    compute = list(kind = "r", sql = NULL, fn = fn,
                   plot = list(type = "bar", code = plot_code)),
    dependencies = list(tables = c("observation_period", "condition_occurrence"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "record",
      count_cols = c("outcome_subjects", "outcome_events",
                     "outcome_observation_periods", "observed_days"),
      person_id_col = "outcome_subjects"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "ohdsi_live", tool_id = "sccs",
                 table_name = "sccs_attrition")
  )
}

# --- Canonical 2: dsomop:sccs.outcome_rate_per_month -------------------------

#' \code{dsomop:sccs.outcome_rate_per_month} entry (SCCS descriptive substrate)
#'
#' The DESCRIPTIVE substrate of the SCCS result: per calendar month, the observed
#' outcome events, the person-time (observed days) accrued by the case
#' population, the distinct-person companion, and the descriptive monthly rate
#' (events per person-day). This is NOT the fitted conditional-Poisson incidence
#' rate ratio (the IRR + CI need the fitted model and are out of scope). The
#' month key matches \code{\link{.achillesCompanionPersonCounts}}
#' (achilles_gating.R:134) storage. The rate is reconciled from the BANDED
#' events + person-days via \code{\link{.omopAnalysisReconcileRatio}} before
#' return.
#' @keywords internal
.omopSccsOutcomeRatePerMonth <- function() {
  name <- "dsomop:sccs.outcome_rate_per_month"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = calendar_month, y = outcome_rate)) +",
    "    ggplot2::geom_line() +",
    "    ggplot2::labs(x = 'Calendar month (YYYYMM)', y = 'Outcome rate / observed person')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    obs     <- .qualifyTable(handle, "observation_period")
    out_src <- .omopSccsOutcomeSource(handle, params)

    # Case substrate (person_id, event_date), scoped or whole-DB, exactly as in
    # the attrition entry.
    if (!is.null(out_src)) {
      ev_tbl  <- out_src$table
      ev_pers <- out_src$person_col
      ev_date <- out_src$date_col
      ev_where <- paste0(" WHERE ", .omopSccsOutcomeWhere(out_src, "o"))
      sj <- .omopScopeJoin(ctx, "o", ev_pers)
      .omopDiagAssertPersons(handle, ctx, ev_tbl, "o", ev_pers,
                             where_sql = .omopSccsOutcomeWhere(out_src, "o"))
    } else {
      if (is.null(ctx$scoped_cohort)) return(data.frame())
      cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
      ev_tbl  <- cohort
      ev_pers <- "subject_id"
      ev_date <- "cohort_start_date"
      ev_where <- ""
      sj <- list(join = "", cohort = cohort)
      .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
        "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
        handle$target_dialect))
    }

    ev_month <- .omopAchillesRecordMonthKey(handle, "ev.event_date")

    case_sql <- paste0(
      "SELECT o.", ev_pers, " AS person_id, o.", ev_date, " AS event_date ",
      "FROM ", ev_tbl, " o", sj$join, ev_where)

    # Per calendar month, over OBSERVED outcome events only (each event joined to
    # an observation period that COVERS the event date — the SCCS person-time
    # requirement): the observed outcome events, the distinct case-persons
    # observed that month (the person-aligned denominator), and the
    # distinct-person companion the gate gates the whole row on. Denominator and
    # numerator are thus on the SAME per-month observed population — a descriptive
    # rate, NOT the fitted Poisson person-time split (which needs the model).
    sql <- .sql_translate(paste0(
      "SELECT ", ev_month, " AS calendar_month, ",
      "COUNT(*) AS outcome_events, ",
      "COUNT(DISTINCT ev.person_id) AS observed_persons, ",
      "COUNT(DISTINCT ev.person_id) AS n_persons ",
      "FROM (", case_sql, ") ev ",
      "INNER JOIN ", obs, " op ON op.person_id = ev.person_id ",
      "AND ev.event_date >= op.observation_period_start_date ",
      "AND ev.event_date <= op.observation_period_end_date",
      " WHERE ev.event_date IS NOT NULL",
      " GROUP BY ", ev_month,
      " ORDER BY ", ev_month),
      handle$target_dialect)

    out <- .executeQuery(handle, sql)
    if (!is.data.frame(out) || nrow(out) == 0) return(data.frame())
    out$outcome_rate <- NA_real_  # reconciled below
    out <- out[, c("calendar_month", "outcome_events", "observed_persons",
                   "n_persons", "outcome_rate"), drop = FALSE]
    # Derived rate: events / observed-persons, reconciled from BANDED counts (NA
    # where either side is suppressed). The single sanctioned exception for ratios.
    out <- .omopAnalysisReconcileRatio(out, numerator_col = "outcome_events",
                                       denominator_col = "observed_persons",
                                       ratio_col = "outcome_rate", scale = 1)
    rownames(out) <- NULL
    out
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("SCCS descriptive substrate: observed outcome events, ",
                         "the distinct case-persons observed and the descriptive ",
                         "events-per-observed-person rate per calendar month over ",
                         "the scoped case population. NOT the fitted Poisson IRR."),
    domain      = "general",
    params      = list(
      list(name = "outcome_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Outcome concept id (descendants expanded) defining cases."),
      list(name = "outcome_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4"))
    ),
    compute = list(kind = "r", sql = NULL, fn = fn,
                   plot = list(type = "line", code = plot_code)),
    dependencies = list(tables = c("observation_period", "condition_occurrence"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "record",
      count_cols = c("outcome_events", "observed_persons", "n_persons"),
      person_id_col = "n_persons"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "ohdsi_live", tool_id = "sccs",
                 table_name = "sccs_result")
  )
}

# --- Canonical 3: dsomop:sccs.count_histograms -------------------------------

#' \code{dsomop:sccs.count_histograms} entry (SCCS covariate count substrate)
#'
#' The COUNT substrate of the SCCS covariate result: per day-offset bin around
#' the exposure index, the outcome EVENTS and distinct PERSONS, split by whether
#' the bin is in the EXPOSED risk window (\code{[0, window]} days after exposure)
#' or UNEXPOSED (outside it). This is the descriptive histogram a fitted SCCS
#' spline RR is built FROM — the spline relative-risk curve itself needs the
#' fitted model and is out of scope. Requires BOTH an exposure cohort/concept and
#' an outcome cohort/concept; returns a gate-safe empty frame when either is
#' absent.
#' @keywords internal
.omopSccsCountHistograms <- function() {
  name <- "dsomop:sccs.count_histograms"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = day_offset, y = outcome_events,",
    "                                   fill = window)) +",
    "    ggplot2::geom_col() +",
    "    ggplot2::labs(x = 'Days from exposure', y = 'Outcome events',",
    "                  fill = 'Risk window')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    exp_src <- .omopOutcomeSource(handle, params$exposure_concept_id,
                                  params$exposure_domain_code %||% "1")
    out_src <- .omopOutcomeSource(handle, params$outcome_concept_id,
                                  params$outcome_domain_code %||% "0")
    # SCCS covariate histograms need BOTH an exposure and an outcome anchor.
    if (is.null(exp_src) || is.null(out_src)) return(data.frame())
    bin_width <- max(as.integer(params$bin_width %||% "30"), 1L)
    window    <- max(as.integer(params$window %||% "30"), 0L)

    # Self-gate the scoped outcome-event population before materialising.
    .omopDiagAssertPersons(handle, ctx, out_src$table, "o", out_src$person_col,
                           where_sql = .omopSccsOutcomeWhere(out_src, "o"))

    sj <- .omopScopeJoin(ctx, "o", out_src$person_col)

    # First exposure date per person (the SCCS exposure index), as a sub-select.
    first_exp <- paste0(
      "(SELECT x.", exp_src$person_col, " AS person_id, ",
      "MIN(x.", exp_src$date_col, ") AS exposure_date ",
      "FROM ", exp_src$table, " x ",
      "WHERE ", .omopSccsOutcomeWhere(exp_src, "x"),
      " GROUP BY x.", exp_src$person_col, ")")

    # Day-offset of each outcome event from that person's first exposure, binned.
    offset_expr <- .omopDateDiffDays(handle, paste0("o.", out_src$date_col),
                                     "fe.exposure_date")
    bin_expr <- paste0("(CAST(", offset_expr, " AS INTEGER) / ", bin_width,
                       ") * ", bin_width)
    # Exposed risk window: 0 <= offset <= window (post-exposure); else unexposed.
    window_expr <- paste0(
      "CASE WHEN (", offset_expr, ") >= 0 AND (", offset_expr, ") <= ", window,
      " THEN 'exposed' ELSE 'unexposed' END")

    sql <- .sql_translate(paste0(
      "SELECT ", bin_expr, " AS day_offset, ", window_expr, " AS window, ",
      "COUNT(*) AS outcome_events, ",
      "COUNT(DISTINCT o.", out_src$person_col, ") AS n_persons ",
      "FROM ", out_src$table, " o",
      " INNER JOIN ", first_exp, " fe ON fe.person_id = o.",
      out_src$person_col, sj$join,
      " WHERE ", .omopSccsOutcomeWhere(out_src, "o"),
      " GROUP BY ", bin_expr, ", ", window_expr,
      " ORDER BY day_offset"),
      handle$target_dialect)

    .executeQuery(handle, sql)
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("SCCS covariate count substrate: outcome events and ",
                         "distinct persons per day-offset bin around first ",
                         "exposure, split by exposed/unexposed risk window, over ",
                         "the scoped case population. NOT the fitted spline RR."),
    domain      = "general",
    params      = list(
      list(name = "exposure_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Exposure concept id (descendants expanded) anchoring the risk window."),
      list(name = "exposure_domain_code", type = "enum", required = FALSE,
           default = "1", choices = c("0", "1", "2", "3", "4"),
           description = "Exposure domain (default 1 drug)."),
      list(name = "outcome_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Outcome concept id (descendants expanded) defining events."),
      list(name = "outcome_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4")),
      list(name = "bin_width", type = "int", required = FALSE, default = "30",
           description = "Day-offset bin width."),
      list(name = "window", type = "int", required = FALSE, default = "30",
           description = "Exposed risk-window length in days after exposure.")
    ),
    compute = list(kind = "r", sql = NULL, fn = fn,
                   plot = list(type = "bar", code = plot_code)),
    dependencies = list(tables = c("drug_exposure", "condition_occurrence"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "record",
      count_cols = c("outcome_events", "n_persons"),
      person_id_col = "n_persons"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "ohdsi_live", tool_id = "sccs",
                 table_name = "sccs_covariate_result")
  )
}

# --- Canonical 4: dsomop:sccs.assumption_checks ------------------------------

#' \code{dsomop:sccs.assumption_checks} entry (SCCS recomputable diagnostics)
#'
#' The SCCS assumption checks that are recomputable from the underlying COUNTS
#' (the fitted-model checks — e.g. the conditional-Poisson IRR diagnostic — are
#' out of scope). One row per check, each carrying its test SCALAR plus the
#' distinct-person companion + record counts it rests on, so the gate can release
#' the scalar only behind a safe person/record basis:
#' \describe{
#'   \item{\code{rare_outcome}}{Outcome proportion = cases / persons-observed;
#'     the rare-disease assumption holds when this is small. Reconciled as a
#'     proportion of banded counts.}
#'   \item{\code{event_dependent_observation}}{Pre-exposure rate-ratio = events
#'     in the \code{pre_window} days BEFORE first exposure vs an equal-length
#'     baseline window further back; ~1 supports the no-event-dependent-
#'     observation assumption. Reconciled as a ratio of banded event counts.}
#'   \item{\code{time_trend}}{Calendar time-trend = outcome events in the LATER
#'     half of the observed span vs the EARLIER half; ~1 supports time
#'     stability. Reconciled as a ratio of banded event counts.}
#' }
#' @keywords internal
.omopSccsAssumptionChecks <- function() {
  name <- "dsomop:sccs.assumption_checks"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = check, y = statistic)) +",
    "    ggplot2::geom_col() + ggplot2::coord_flip() +",
    "    ggplot2::labs(x = 'Assumption check', y = 'Test statistic')",
    "}", sep = "\n")

  fn <- function(handle, ctx, params) {
    obs     <- .qualifyTable(handle, "observation_period")
    out_src <- .omopSccsOutcomeSource(handle, params)
    pre_window <- max(as.integer(params$pre_window %||% "30"), 1L)

    if (!is.null(out_src)) {
      ev_tbl  <- out_src$table
      ev_pers <- out_src$person_col
      ev_date <- out_src$date_col
      ev_where <- paste0(" WHERE ", .omopSccsOutcomeWhere(out_src, "o"))
      sj <- .omopScopeJoin(ctx, "o", ev_pers)
      .omopDiagAssertPersons(handle, ctx, ev_tbl, "o", ev_pers,
                             where_sql = .omopSccsOutcomeWhere(out_src, "o"))
    } else {
      if (is.null(ctx$scoped_cohort)) return(data.frame())
      cohort  <- .validateIdentifier(ctx$scoped_cohort, "cohort")
      ev_tbl  <- cohort
      ev_pers <- "subject_id"
      ev_date <- "cohort_start_date"
      ev_where <- ""
      sj <- list(join = "", cohort = cohort)
      .assertMinPersons(handle = handle, sql = .sql_translate(paste0(
        "SELECT COUNT(DISTINCT subject_id) AS n FROM ", cohort),
        handle$target_dialect))
    }

    case_sql <- paste0(
      "SELECT o.", ev_pers, " AS person_id, o.", ev_date, " AS event_date ",
      "FROM ", ev_tbl, " o", sj$join, ev_where)

    # --- Check A: rare_outcome (cases / observed persons) ---------------------
    # numerator = distinct case persons; denominator = distinct persons with an
    # observation period (the observed base population).
    a_num_sql <- .sql_translate(paste0(
      "SELECT COUNT(DISTINCT person_id) AS n FROM (", case_sql, ") cse"),
      handle$target_dialect)
    a_den_sql <- .sql_translate(paste0(
      "SELECT COUNT(DISTINCT person_id) AS n FROM ", obs),
      handle$target_dialect)
    a_num <- as.numeric(.executeQuery(handle, a_num_sql)$n[1])
    a_den <- as.numeric(.executeQuery(handle, a_den_sql)$n[1])

    # --- Check B: event_dependent_observation (pre-exposure rate ratio) -------
    # Reuse the outcome as its own pre-window anchor is not possible (no exposure
    # here), so anchor on each case's FIRST outcome and compare events in the
    # [-pre_window, 0) window before it vs the equal-length baseline window
    # [-2*pre_window, -pre_window). ~1 means events do not cluster pre-index.
    first_evt <- paste0(
      "(SELECT cse.person_id, MIN(cse.event_date) AS idx ",
      "FROM (", case_sql, ") cse GROUP BY cse.person_id)")
    off <- .omopDateDiffDays(handle, "ev.event_date", "fe.idx")
    b_pre_sql <- .sql_translate(paste0(
      "SELECT COUNT(*) AS n FROM (", case_sql, ") ev ",
      "INNER JOIN ", first_evt, " fe ON fe.person_id = ev.person_id ",
      "WHERE (", off, ") < 0 AND (", off, ") >= ", -pre_window),
      handle$target_dialect)
    b_base_sql <- .sql_translate(paste0(
      "SELECT COUNT(*) AS n FROM (", case_sql, ") ev ",
      "INNER JOIN ", first_evt, " fe ON fe.person_id = ev.person_id ",
      "WHERE (", off, ") < ", -pre_window, " AND (", off, ") >= ",
      -2L * pre_window),
      handle$target_dialect)
    b_pre  <- as.numeric(.executeQuery(handle, b_pre_sql)$n[1])
    b_base <- as.numeric(.executeQuery(handle, b_base_sql)$n[1])
    # Distinct persons behind B's numerator AND its denominator window SEPARATELY:
    # the two EVENT counts can each rest on FEWER persons than the case set (one
    # person with many clustered pre-index events), so the row's person basis is the
    # MIN of the two windows' distinct-person counts (NOT a_num, the whole case
    # set). Taking the min means the row is dropped unless BOTH the numerator and
    # the denominator each rest on >= nfilter persons, so a single-person event
    # cluster on EITHER side can never be waved through on an unrelated companion.
    b_pre_persons_sql <- .sql_translate(paste0(
      "SELECT COUNT(DISTINCT ev.person_id) AS n FROM (", case_sql, ") ev ",
      "INNER JOIN ", first_evt, " fe ON fe.person_id = ev.person_id ",
      "WHERE (", off, ") < 0 AND (", off, ") >= ", -pre_window),
      handle$target_dialect)
    b_base_persons_sql <- .sql_translate(paste0(
      "SELECT COUNT(DISTINCT ev.person_id) AS n FROM (", case_sql, ") ev ",
      "INNER JOIN ", first_evt, " fe ON fe.person_id = ev.person_id ",
      "WHERE (", off, ") < ", -pre_window, " AND (", off, ") >= ",
      -2L * pre_window),
      handle$target_dialect)
    b_persons <- min(as.numeric(.executeQuery(handle, b_pre_persons_sql)$n[1]),
                     as.numeric(.executeQuery(handle, b_base_persons_sql)$n[1]),
                     na.rm = FALSE)

    # --- Check C: time_trend (later half vs earlier half of observed span) ----
    # Split each observation period at its midpoint and count outcome events in
    # the later vs earlier half. ~1 means no calendar time trend in the outcome.
    mid <- .omopDateDiffDays(handle, "op.observation_period_end_date",
                             "op.observation_period_start_date")
    halves <- paste0(
      "SELECT cse.person_id, cse.event_date, ",
      "op.observation_period_start_date AS op_start, ",
      "(", mid, ") AS span ",
      "FROM (", case_sql, ") cse ",
      "INNER JOIN ", obs, " op ON op.person_id = cse.person_id ",
      "AND cse.event_date >= op.observation_period_start_date ",
      "AND cse.event_date <= op.observation_period_end_date")
    since_start <- .omopDateDiffDays(handle, "h.event_date", "h.op_start")
    c_late_sql <- .sql_translate(paste0(
      "SELECT COUNT(*) AS n FROM (", halves, ") h ",
      "WHERE (", since_start, ") * 2 >= h.span"),
      handle$target_dialect)
    c_early_sql <- .sql_translate(paste0(
      "SELECT COUNT(*) AS n FROM (", halves, ") h ",
      "WHERE (", since_start, ") * 2 < h.span"),
      handle$target_dialect)
    c_late  <- as.numeric(.executeQuery(handle, c_late_sql)$n[1])
    c_early <- as.numeric(.executeQuery(handle, c_early_sql)$n[1])
    # Distinct persons behind C's numerator AND denominator half SEPARATELY: as for
    # B, the half EVENT counts can each rest on fewer persons than the case set (one
    # person with many clustered late-half events), so the row's person basis is the
    # MIN of the two halves' distinct-person counts. Taking the min drops the row
    # unless BOTH halves each rest on >= nfilter persons, so a single-person event
    # cluster in EITHER half can never survive on the unrelated case-set count.
    c_late_persons_sql <- .sql_translate(paste0(
      "SELECT COUNT(DISTINCT h.person_id) AS n FROM (", halves, ") h ",
      "WHERE (", since_start, ") * 2 >= h.span"),
      handle$target_dialect)
    c_early_persons_sql <- .sql_translate(paste0(
      "SELECT COUNT(DISTINCT h.person_id) AS n FROM (", halves, ") h ",
      "WHERE (", since_start, ") * 2 < h.span"),
      handle$target_dialect)
    c_persons <- min(as.numeric(.executeQuery(handle, c_late_persons_sql)$n[1]),
                     as.numeric(.executeQuery(handle, c_early_persons_sql)$n[1]),
                     na.rm = FALSE)

    nz <- function(x) if (length(x) == 0 || is.na(x)) 0 else x
    # One row per check: the test scalar (reconciled below) plus the numerator /
    # denominator counts it rests on and the distinct-person basis n_persons that
    # the gate's generic record branch gates the whole row on. Each row carries the
    # distinct persons behind ITS OWN counts (A: case persons; B: min persons across
    # the pre/baseline windows; C: min persons across the late/early halves), so a
    # check whose numerator OR denominator event count collapses onto a handful of
    # persons is dropped on a TRUE person basis — never waved through on the
    # unrelated whole-case-set count.
    out <- data.frame(
      check       = c("rare_outcome", "event_dependent_observation",
                      "time_trend"),
      numerator   = c(nz(a_num), nz(b_pre), nz(c_late)),
      denominator = c(nz(a_den), nz(b_base), nz(c_early)),
      n_persons   = c(nz(a_num), nz(b_persons), nz(c_persons)),
      statistic   = NA_real_,
      stringsAsFactors = FALSE)
    # Each statistic is numerator/denominator reconciled from BANDED counts (NA
    # where either side is suppressed) — the sanctioned ratio exception.
    out <- .omopAnalysisReconcileRatio(out, numerator_col = "numerator",
                                       denominator_col = "denominator",
                                       ratio_col = "statistic", scale = 1)
    rownames(out) <- NULL
    out
  }

  .omopAnalysisEntry(
    name        = name,
    description = paste0("SCCS recomputable assumption checks (rare-outcome ",
                         "proportion, pre-index event-dependence rate-ratio, ",
                         "calendar time-trend) as test scalars over the scoped ",
                         "case population, each gated behind its person/record ",
                         "basis. Fitted-model checks are out of scope."),
    domain      = "general",
    params      = list(
      list(name = "outcome_concept_id", type = "concept_id", required = FALSE,
           default = NULL,
           description = "Outcome concept id (descendants expanded) defining cases."),
      list(name = "outcome_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4")),
      list(name = "pre_window", type = "int", required = FALSE, default = "30",
           description = "Pre-index window (days) for the event-dependence check.")
    ),
    compute = list(kind = "r", sql = NULL, fn = fn,
                   plot = list(type = "bar", code = plot_code)),
    dependencies = list(tables = c("observation_period", "condition_occurrence"),
                        packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit = "record",
      count_cols = c("numerator", "denominator", "n_persons"),
      person_id_col = "n_persons"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "ohdsi_live", tool_id = "sccs",
                 table_name = "sccs_diagnostics_summary")
  )
}

# --- OHDSI-id aliases onto the canonical SCCS entries ------------------------

#' Build one dsomop:ohdsi.sccs.<table> alias onto a canonical SCCS entry
#'
#' Keeps the legacy OHDSI id as a thin delegate to the canonical
#' \code{dsomop:sccs.<metric>} entry (the B1 alias pattern): inherits the
#' canonical compute fn, disclosure spec, params, dependencies and scope, and
#' carries the OHDSI labels (tool_id, table_name, alias_target) for listing only.
#' The canonical entry already computes live and gates correctly, so no compute
#' logic is duplicated here.
#'
#' @param table_name Character; the legacy SCCS result table (the \code{<table>}
#'   in the id); a LABEL only (never read).
#' @param builder Zero-arg function returning the canonical SCCS entry.
#' @return An \code{omop_analysis_entry} keyed (by the registrar) on its id.
#' @keywords internal
.ohdsiSccsAliasEntry <- function(table_name, builder) {
  canonical <- builder()
  name <- paste0("dsomop:ohdsi.sccs.", table_name)
  fn <- function(handle, ctx, params) builder()$compute$fn(handle, ctx, params)
  .omopAnalysisEntry(
    name        = name,
    description = canonical$description,
    domain      = canonical$domain,
    params      = canonical$params,
    compute     = list(kind = "r", sql = NULL, fn = fn,
                       plot = canonical$compute$plot),
    dependencies = canonical$dependencies,
    disclosure  = canonical$disclosure,
    scope = .omopAnalysisScope(
      accepts_cohort  = TRUE,
      accepts_tables  = TRUE,
      max_tables      = canonical$scope$max_tables %||% 1L,
      requires_cohort = isTRUE(canonical$scope$requires_cohort)),
    mode  = "aggregate",
    meta  = list(adapter      = canonical$meta$adapter %||% "ohdsi_live",
                 tool_id      = "sccs",
                 table_name   = table_name,
                 alias_target = canonical$name)
  )
}

#' Emit the SCCS (OHDSI-B2) catalog entries (native live compute)
#'
#' Group registrar for \code{ohdsi_pack_sccs.R}. Returns BOTH the four NEW
#' canonical \code{dsomop:sccs.*} entries AND the four legacy
#' \code{dsomop:ohdsi.sccs.*} aliases that delegate to them, each keyed by id, so
#' a single registrar addition wires the whole group. No precomputed SCCS results
#' table is read by any entry. Takes \code{handle} for signature parity with the
#' other adapters ONLY — it performs NO DB I/O at build time; all queries run
#' later inside each entry's \code{compute$fn}.
#'
#' Mappings (legacy OHDSI result table -> new canonical native entry):
#' \itemize{
#'   \item sccs.sccs_attrition -> dsomop:sccs.attrition
#'   \item sccs.sccs_result -> dsomop:sccs.outcome_rate_per_month
#'   \item sccs.sccs_covariate_result -> dsomop:sccs.count_histograms
#'   \item sccs.sccs_diagnostics_summary -> dsomop:sccs.assumption_checks
#' }
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by id.
#' @keywords internal
.ohdsiPackSccsEntries <- function(handle) {
  entries <- list(
    # New canonical native SCCS entries (live-computing substrate).
    .omopSccsAttrition(),
    .omopSccsOutcomeRatePerMonth(),
    .omopSccsCountHistograms(),
    .omopSccsAssumptionChecks(),
    # Legacy OHDSI ids kept as thin aliases onto the canonicals above.
    .ohdsiSccsAliasEntry("sccs_attrition", .omopSccsAttrition),
    .ohdsiSccsAliasEntry("sccs_result", .omopSccsOutcomeRatePerMonth),
    .ohdsiSccsAliasEntry("sccs_covariate_result", .omopSccsCountHistograms),
    .ohdsiSccsAliasEntry("sccs_diagnostics_summary", .omopSccsAssumptionChecks)
  )
  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}
