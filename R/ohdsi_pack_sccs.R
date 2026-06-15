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
#
# THE R-IN-SESSION PRINCIPLE reclaims the fitted IRR. The header above declared
# "the conditional Poisson IRR + CI" OUT as fitted-model-only, but a dsOMOP
# analysis runs in the server-side R session: it is NOT limited to one descriptive
# SQL query. It can LOAD the per-case exposed/unexposed (events, person-time)
# substrate into R and FIT the SCCS conditional Poisson there, returning ONLY a
# disclosure-safe aggregate. So group PLR-9 ADDS one canonical native that is now
# IN:
#   * dsomop:sccs.incidence_rate_ratio: the FITTED SCCS exposed-vs-unexposed
#     incidence-rate ratio (conditional Poisson, stratified per case) with 95% CI
#     and the log-IRR + SE meta-analysis sufficient statistics es_sccs synthesis
#     consumes, alongside the gated exposed/unexposed event + case-person counts it
#     rests on. It reuses the SAME per-case exposed/unexposed substrate
#     dsomop:sccs.count_histograms summarises (.omopSccsCountHistograms), pulled at
#     per-case granularity into the session and fit there; no per-case row, event
#     date or per-subject coefficient ever leaves R.
#   The fitted SPLINE relative-risk curve stays OUT: it genuinely needs the
#   per-subject smooth that no banded aggregate reconstructs.
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
# The fitted IRR (dsomop:sccs.incidence_rate_ratio) is NOT a count, so the gate
# cannot suppress it on a count basis: it follows the PLR-1 (cm.effect_estimate)
# non-frame contract — the fn applies an EXPLICIT in-fn guard BEFORE return,
# banding the exposed/unexposed event + case-person counts and setting
# irr/ci_lo/ci_hi/log_irr/se_log_irr to NA unless BOTH windows rest on a released
# (>= nfilter.tab after banding) event count AND >= nfilter.subset cases
# contribute to BOTH windows. The declared count_cols (n_cases / exposed_events /
# unexposed_events) are gated by the SINGLE .omopAnalysisGate; the per-case frame
# is materialised in-session and discarded.
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

# --- Canonical 5: dsomop:sccs.incidence_rate_ratio (FITTED, R-in-session) -----

#' Build the per-case exposed/unexposed (events, person-time) SCCS frame (in-session)
#'
#' The R-in-session DATA step for \code{\link{.omopSccsIrrFn}}. Over the scoped case
#' population it materialises the SAME per-case exposed/unexposed substrate
#' \code{\link{.omopSccsCountHistograms}} aggregates — but at PER-CASE granularity
#' (two rows per case: one exposed period, one unexposed period) so the conditional
#' Poisson can be fit with a per-case stratum. Each case is a person with >= 1
#' outcome event observed inside an observation period; the SCCS exposure index is
#' the person's FIRST exposure (\code{\link{.omopOutcomeSource}} on the exposure
#' concept, MIN date) exactly as the count-histogram substrate anchors it. Per case:
#' \itemize{
#'   \item observed person-time = the observation-period days that cover the case
#'     (clamped >= 1 so the Poisson offset stays defined);
#'   \item exposed person-time = the days of that span falling in the risk window
#'     \code{[exposure_date, exposure_date + window]} (intersected with the
#'     observation period), clamped to \code{[0, observed]};
#'   \item exposed events = outcome events whose date is in that window; unexposed
#'     events = total in-period outcome events minus exposed events; unexposed
#'     person-time = observed minus exposed.
#' }
#' The SQL aggregates GROUP BY case so it returns ONE row per case; the per-case
#' rows are pulled into R and reshaped to the two-period long frame there — NO
#' per-case row, event date or person id ever leaves the session. A case is kept
#' only when it contributes strictly positive person-time to BOTH periods (the SCCS
#' within-person contrast needs both an exposed and an unexposed window); a case
#' wholly exposed or wholly unexposed contributes no within-person information and
#' is dropped, exactly as the conditional likelihood eliminates it.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (for the scope join over the case population).
#' @param exp_src Resolved exposure \code{\link{.omopOutcomeSource}} (NULL: none).
#' @param out_src Resolved outcome \code{\link{.omopOutcomeSource}} (NULL: none).
#' @param window Integer exposed risk-window length in days after first exposure.
#' @return list(data = <long frame: case_id, exposed, events, person_time>,
#'   n_cases, exposed_events, unexposed_events).
#' @keywords internal
.omopSccsIrrCaseData <- function(handle, ctx, exp_src, out_src, window) {
  empty <- list(
    data = data.frame(case_id = integer(0), exposed = integer(0),
                      events = numeric(0), person_time = numeric(0)),
    n_cases = 0, exposed_events = 0, unexposed_events = 0)
  if (is.null(exp_src) || is.null(out_src)) return(empty)

  obs <- .qualifyTable(handle, "observation_period")
  sj  <- .omopScopeJoin(ctx, "o", out_src$person_col)

  # First exposure date per person (the SCCS exposure index), as a sub-select —
  # the SAME anchor .omopSccsCountHistograms uses.
  first_exp <- paste0(
    "(SELECT x.", exp_src$person_col, " AS person_id, ",
    "MIN(x.", exp_src$date_col, ") AS exposure_date ",
    "FROM ", exp_src$table, " x ",
    "WHERE ", .omopSccsOutcomeWhere(exp_src, "x"),
    " GROUP BY x.", exp_src$person_col, ")")

  # The exposed risk window end = exposure_date + window days.
  win_end <- paste0("DATEADD(day, ", window, ", fe.exposure_date)")
  # Observed span (days) of the observation period covering the case, clamped >= 1.
  observed_days <- .omopDateDiffDays(
    handle, "op.observation_period_end_date", "op.observation_period_start_date")
  # Exposed person-time = the overlap of [exposure_date, win_end] with the
  # observation period [op_start, op_end], in days: the lesser end minus the
  # greater start, floored at 0. Built dialect-safely from CASE bounds so the
  # interval arithmetic is the same on every backend.
  ov_lo <- paste0(
    "CASE WHEN fe.exposure_date > op.observation_period_start_date ",
    "THEN fe.exposure_date ELSE op.observation_period_start_date END")
  ov_hi <- paste0(
    "CASE WHEN (", win_end, ") < op.observation_period_end_date ",
    "THEN (", win_end, ") ELSE op.observation_period_end_date END")
  ov_days <- .omopDateDiffDays(handle, ov_hi, ov_lo)
  exposed_days_expr <- paste0("CASE WHEN (", ov_days, ") > 0 THEN (", ov_days,
                              ") ELSE 0 END")
  # Exposed-event flag per outcome row: date within [exposure_date, win_end].
  exposed_event_expr <- paste0(
    "CASE WHEN o.", out_src$date_col, " >= fe.exposure_date ",
    "AND o.", out_src$date_col, " <= (", win_end, ") THEN 1 ELSE 0 END")

  # One row per case: total in-period outcome events, exposed events, observed
  # span and exposed span. The outcome rows are joined to the covering observation
  # period (the SCCS person-time requirement) and to the person's first exposure.
  sql <- .sql_translate(paste0(
    "SELECT o.", out_src$person_col, " AS case_id, ",
    "COUNT(*) AS total_events, ",
    "SUM(", exposed_event_expr, ") AS exposed_events, ",
    "MAX(", observed_days, ") AS observed_days, ",
    "MAX(", exposed_days_expr, ") AS exposed_days ",
    "FROM ", out_src$table, " o",
    " INNER JOIN ", first_exp, " fe ON fe.person_id = o.",
    out_src$person_col,
    " INNER JOIN ", obs, " op ON op.person_id = o.", out_src$person_col,
    " AND o.", out_src$date_col, " >= op.observation_period_start_date",
    " AND o.", out_src$date_col, " <= op.observation_period_end_date",
    sj$join,
    " WHERE ", .omopSccsOutcomeWhere(out_src, "o"),
    " GROUP BY o.", out_src$person_col),
    handle$target_dialect)

  raw <- .executeQuery(handle, sql)
  if (!is.data.frame(raw) || nrow(raw) == 0) return(empty)

  total_ev <- as.numeric(raw$total_events)
  exp_ev   <- as.numeric(raw$exposed_events)
  observed <- pmax(as.numeric(raw$observed_days), 1)
  exp_pt   <- pmin(pmax(as.numeric(raw$exposed_days), 0), observed)
  unexp_pt <- observed - exp_pt
  unexp_ev <- pmax(total_ev - exp_ev, 0)

  # Keep only cases with STRICTLY positive person-time in BOTH periods: a case
  # wholly exposed (or wholly unexposed) carries no within-person contrast and is
  # eliminated by the conditional likelihood — dropping it here is the same step.
  keep <- is.finite(exp_pt) & is.finite(unexp_pt) & exp_pt > 0 & unexp_pt > 0
  case_id <- as.integer(raw$case_id)[keep]
  exp_ev <- exp_ev[keep]; unexp_ev <- unexp_ev[keep]
  exp_pt <- exp_pt[keep]; unexp_pt <- unexp_pt[keep]
  if (length(case_id) == 0) return(empty)

  # Long two-period frame: per case, an exposed row and an unexposed row.
  long <- data.frame(
    case_id     = rep(case_id, 2L),
    exposed     = c(rep(1L, length(case_id)), rep(0L, length(case_id))),
    events      = c(exp_ev, unexp_ev),
    person_time = c(exp_pt, unexp_pt),
    stringsAsFactors = FALSE)

  list(
    data             = long,
    n_cases          = length(case_id),
    exposed_events   = sum(exp_ev, na.rm = TRUE),
    unexposed_events = sum(unexp_ev, na.rm = TRUE))
}

#' Fit the SCCS conditional Poisson in the R session and extract log-IRR + SE
#'
#' The R-in-session MODEL step: given the per-case two-period
#' \code{(case_id, exposed, events, person_time)} frame from
#' \code{\link{.omopSccsIrrCaseData}}, fit the SCCS conditional Poisson and return
#' the exposed-vs-unexposed log incidence-rate ratio and its standard error (the
#' es_sccs meta-analysis sufficient statistics). Two equivalent specifications,
#' both giving a log-IRR-scale coefficient + SE on \code{exposed}:
#' \itemize{
#'   \item \code{gnm}: \code{gnm::gnm(events ~ exposed + offset(log(person_time)),
#'     family = poisson, eliminate = factor(case_id))} — the canonical SCCS
#'     conditional-Poisson fit (the per-case stratum is eliminated, not estimated).
#'     Used when \pkg{gnm} is installed (declared in Suggests).
#'   \item \code{poisson}: \code{stats::glm(events ~ exposed + factor(case_id) +
#'     offset(log(person_time)), family = poisson)} — the stats-only fallback: a
#'     fixed-effect-per-case Poisson whose \code{exposed} coefficient equals the
#'     conditional log-IRR. Always available (\pkg{stats} is a hard dependency).
#' }
#' The fit happens entirely in the session over the in-R frame; only the scalar
#' \code{log_irr} + \code{se_log_irr} leave it (never per-case coefficients, never
#' the frame). Returns \code{NA}s when the model cannot be fit (one period absent,
#' no events, fewer than two cases, non-finite SE, or fit error) — fail-closed.
#'
#' @param df Per-case two-period frame (case_id, exposed, events, person_time).
#' @return list(log_irr, se_log_irr, model_type) (NA on failure).
#' @keywords internal
.omopSccsIrrFit <- function(df) {
  na_out <- function(mt) list(log_irr = NA_real_, se_log_irr = NA_real_,
                              model_type = mt)
  use_gnm <- requireNamespace("gnm", quietly = TRUE)
  mt <- if (use_gnm) "gnm_conditional_poisson" else "stratified_poisson"
  if (!is.data.frame(df) || nrow(df) == 0) return(na_out(mt))
  # Both periods present, at least one event, and >= 2 distinct cases (a single
  # case cannot identify the within-person contrast after stratifying).
  df <- df[is.finite(df$person_time) & df$person_time > 0, , drop = FALSE]
  if (nrow(df) == 0) return(na_out(mt))
  if (length(unique(df$exposed)) < 2L) return(na_out(mt))
  if (sum(df$events, na.rm = TRUE) <= 0) return(na_out(mt))
  if (length(unique(df$case_id)) < 2L) return(na_out(mt))
  df$case_id <- factor(df$case_id)

  fit <- tryCatch({
    if (use_gnm) {
      m <- gnm::gnm(events ~ exposed, family = stats::poisson(),
                    offset = log(df$person_time), eliminate = df$case_id,
                    data = df, verbose = FALSE)
      s <- summary(m)$coefficients
      list(coef = unname(s["exposed", "Estimate"]),
           se = unname(s["exposed", "Std. Error"]))
    } else {
      m <- stats::glm(events ~ exposed + case_id, family = stats::poisson(),
                      offset = log(df$person_time), data = df)
      s <- summary(m)$coefficients
      list(coef = unname(s["exposed", "Estimate"]),
           se = unname(s["exposed", "Std. Error"]))
    }
  }, error = function(e) NULL)

  if (is.null(fit) || !is.finite(fit$coef) || !is.finite(fit$se) ||
      fit$se <= 0) {
    return(na_out(mt))
  }
  list(log_irr = as.numeric(fit$coef), se_log_irr = as.numeric(fit$se),
       model_type = mt)
}

#' Build the sccs.incidence_rate_ratio live compute fn (fitted IRR, R-in-session)
#'
#' Over the scoped case population (cohort-wide when un-scoped; self-gated
#' >= nfilter.subset persons by \code{\link{.omopDiagAssertPersons}} before any
#' pull): build the per-case two-period \code{(case_id, exposed, events,
#' person_time)} frame (\code{\link{.omopSccsIrrCaseData}}), FIT the SCCS
#' conditional Poisson in the session (\code{\link{.omopSccsIrrFit}}), and emit ONE
#' aggregate row — the fitted IRR (\code{exp(coef)}), its 95\% CI
#' (\code{exp(coef +/- 1.96 SE)}), the \code{log_irr} + \code{se_log_irr}
#' meta-analysis sufficient statistics, and the gated counts the estimate rests on
#' (\code{n_cases}, \code{exposed_events}, \code{unexposed_events}).
#'
#' DISCLOSURE — sanctioned non-frame guard (mirrors PLR-1 cm.effect_estimate). The
#' fitted IRR + CI are NOT counts, so the gate cannot suppress them on a count
#' basis. This fn applies the explicit in-fn guard BEFORE return: it bands each
#' window's event count and the case count (\code{\link{.bandCount}} at
#' nfilter_band) and sets irr / ci_lo / ci_hi / log_irr / se_log_irr to NA unless
#' BOTH windows' banded event counts are releasable (>= nfilter_tab) AND
#' >= nfilter.subset cases contribute to BOTH periods — exactly the effect-estimate
#' rule the disclosure plan mandates. The declared count_cols (n_cases /
#' exposed_events / unexposed_events) are additionally suppressed + banded by the
#' SINGLE \code{\link{.omopAnalysisGate}} (which drops the whole row if any falls
#' below threshold). No per-case row leaves R; no \code{*_source_value} is selected;
#' the exposure/outcome concept sets are descendant-expanded and name-translated.
#'
#' @return A \code{function(handle, ctx, params)} returning a one-row aggregate.
#' @keywords internal
.omopSccsIrrFn <- function() {
  function(handle, ctx, params) {
    exp_src <- .omopOutcomeSource(handle, params$exposure_concept_id,
                                  params$exposure_domain_code %||% "1")
    out_src <- .omopOutcomeSource(handle, params$outcome_concept_id,
                                  params$outcome_domain_code %||% "0")
    # The fitted IRR needs BOTH an exposure and an outcome anchor (like the count
    # histograms). Either absent -> no contrast -> gate-safe empty frame.
    if (is.null(exp_src) || is.null(out_src)) return(data.frame())
    window <- max(as.integer(params$window %||% "30"), 0L)

    # Self-gate the scoped outcome-event (case) population before materialising.
    .omopDiagAssertPersons(handle, ctx, out_src$table, "o", out_src$person_col,
                           where_sql = .omopSccsOutcomeWhere(out_src, "o"))

    # Per-case in-session data + count substrate, then FIT the conditional Poisson.
    cd <- .omopSccsIrrCaseData(handle, ctx, exp_src, out_src, window)
    if (nrow(cd$data) == 0) return(data.frame())
    fit <- .omopSccsIrrFit(cd$data)

    log_irr <- fit$log_irr
    se_log  <- fit$se_log_irr
    irr   <- if (is.finite(log_irr)) exp(log_irr) else NA_real_
    ci_lo <- if (is.finite(log_irr) && is.finite(se_log)) {
      exp(log_irr - 1.96 * se_log)
    } else NA_real_
    ci_hi <- if (is.finite(log_irr) && is.finite(se_log)) {
      exp(log_irr + 1.96 * se_log)
    } else NA_real_

    # --- Sanctioned non-frame guard (band the counts, NA the estimate) ---------
    # Effect-estimate rule: require BOTH windows to rest on a released (banded
    # >= nfilter_tab) event count AND >= nfilter.subset cases in BOTH periods.
    settings <- .omopDisclosureSettings()
    bw  <- settings$nfilter_band
    thr <- settings$nfilter_tab
    sub <- settings$nfilter_subset
    band_or_na <- function(x) {
      x <- as.numeric(x)
      v <- .bandCount(x, band_width = bw)
      if (is.na(x) || x < thr) NA_real_ else v
    }
    # A banded count is a RELEASE only when it is itself >= nfilter_tab (a raw
    # count in [nfilter_tab, nfilter_band) FLOORS to 0, which is NOT a release).
    releasable_count <- function(x) is.finite(x) && x >= thr
    exp_b   <- band_or_na(cd$exposed_events)
    unexp_b <- band_or_na(cd$unexposed_events)
    # Every kept case contributes to BOTH periods by construction, so n_cases is
    # the case basis for BOTH windows; require it >= nfilter.subset.
    estimate_releasable <- releasable_count(exp_b) && releasable_count(unexp_b) &&
      is.finite(cd$n_cases) && cd$n_cases >= sub
    if (!estimate_releasable) {
      irr <- NA_real_; ci_lo <- NA_real_; ci_hi <- NA_real_
      log_irr <- NA_real_; se_log <- NA_real_
    }

    # ONE aggregate row: the fitted IRR + CI + sufficient statistics, alongside
    # the gated counts the gate suppresses + bands (dropping the row if any falls
    # below threshold). n_cases is the distinct-case person companion.
    out <- data.frame(
      model_type       = fit$model_type,
      n_cases          = as.numeric(cd$n_cases),
      exposed_events   = as.numeric(cd$exposed_events),
      unexposed_events = as.numeric(cd$unexposed_events),
      irr              = as.numeric(irr),
      ci_lo            = as.numeric(ci_lo),
      ci_hi            = as.numeric(ci_hi),
      log_irr          = as.numeric(log_irr),
      se_log_irr       = as.numeric(se_log),
      stringsAsFactors = FALSE)
    rownames(out) <- NULL
    out
  }
}

#' \code{dsomop:sccs.incidence_rate_ratio} entry (SCCS fitted conditional-Poisson IRR)
#'
#' The FITTED SCCS exposed-vs-unexposed incidence-rate ratio, live and computed in
#' the server-side R session (the IRR the SCCS pack header originally declared OUT
#' as fitted-model-only; the R-in-session principle reclaims it). NO precomputed
#' OHDSI twin: this IS the canonical id (the read-precomputed sccs_result path that
#' once returned the fitted IRR is dead). The fitted spline relative-risk curve
#' stays out of scope (it needs the per-subject smooth). unit=record; n_cases is
#' the gated distinct-case companion; the fitted IRR is NA unless BOTH windows are
#' releasable and >= nfilter.subset cases contribute to both.
#' @keywords internal
.omopSccsIncidenceRateRatio <- function() {
  name <- "dsomop:sccs.incidence_rate_ratio"
  plot_code <- paste(
    "function(df, params) {",
    "  ggplot2::ggplot(df, ggplot2::aes(x = factor(model_type), y = irr)) +",
    "    ggplot2::geom_point() +",
    "    ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lo, ymax = ci_hi),",
    "                           width = 0.1) +",
    "    ggplot2::geom_hline(yintercept = 1, linetype = 'dashed') +",
    "    ggplot2::labs(x = NULL, y = 'SCCS incidence-rate ratio (95% CI)')",
    "}", sep = "\n")

  .omopAnalysisEntry(
    name        = name,
    description = paste0("SCCS fitted conditional-Poisson incidence-rate ratio ",
                         "(live, R-in-session): the exposed-vs-unexposed IRR over ",
                         "the scoped case population, with 95% CI and the log-IRR ",
                         "+ SE meta-analysis sufficient statistics, alongside the ",
                         "exposed/unexposed event + case counts it rests on. ",
                         "unit=record; n_cases is the gated distinct-case ",
                         "companion; the IRR is NA unless both windows are ",
                         "releasable. The fitted spline RR remains out of scope."),
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
           description = "Outcome concept id (descendants expanded) defining cases/events."),
      list(name = "outcome_domain_code", type = "enum", required = FALSE,
           default = "0", choices = c("0", "1", "2", "3", "4"),
           description = "Outcome domain (0 condition,1 drug,2 procedure,3 measurement,4 observation)."),
      list(name = "window", type = "int", required = FALSE, default = "30",
           description = "Exposed risk-window length in days after first exposure.")
    ),
    compute = list(
      kind = "r", sql = NULL, fn = .omopSccsIrrFn(),
      plot = list(type = "point", code = plot_code)
    ),
    dependencies = list(
      tables = c("observation_period", "drug_exposure", "condition_occurrence"),
      packages = character(0)),
    disclosure = .omopAnalysisDisclosure(
      unit          = "record",
      count_cols    = c("n_cases", "exposed_events", "unexposed_events"),
      person_id_col = "n_cases"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "ohdsi_live", tool_id = "sccs",
                 table_name = "sccs_result")
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
#' Group registrar for \code{ohdsi_pack_sccs.R}. Returns the FIVE canonical
#' \code{dsomop:sccs.*} entries (the four descriptive-substrate natives PLUS the
#' PLR-9 fitted IRR) AND the four legacy \code{dsomop:ohdsi.sccs.*} aliases that
#' delegate to the four substrate canonicals, each keyed by id, so a single
#' registrar addition wires the whole group. The fitted IRR
#' (\code{dsomop:sccs.incidence_rate_ratio}) has NO precomputed OHDSI twin, so it
#' is added WITHOUT an alias. No precomputed SCCS results table is read by any
#' entry. Takes \code{handle} for signature parity with the other adapters ONLY —
#' it performs NO DB I/O at build time; all queries run later inside each entry's
#' \code{compute$fn}.
#'
#' Mappings (legacy OHDSI result table -> new canonical native entry):
#' \itemize{
#'   \item sccs.sccs_attrition -> dsomop:sccs.attrition
#'   \item sccs.sccs_result -> dsomop:sccs.outcome_rate_per_month
#'   \item sccs.sccs_covariate_result -> dsomop:sccs.count_histograms
#'   \item sccs.sccs_diagnostics_summary -> dsomop:sccs.assumption_checks
#'   \item (no twin) -> dsomop:sccs.incidence_rate_ratio (fitted IRR; PLR-9)
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
    # PLR-9: the FITTED conditional-Poisson IRR reclaimed by the R-in-session
    # principle (no precomputed OHDSI twin; this IS its canonical id).
    .omopSccsIncidenceRateRatio(),
    # Legacy OHDSI ids kept as thin aliases onto the canonicals above.
    .ohdsiSccsAliasEntry("sccs_attrition", .omopSccsAttrition),
    .ohdsiSccsAliasEntry("sccs_result", .omopSccsOutcomeRatePerMonth),
    .ohdsiSccsAliasEntry("sccs_covariate_result", .omopSccsCountHistograms),
    .ohdsiSccsAliasEntry("sccs_diagnostics_summary", .omopSccsAssumptionChecks)
  )
  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}
