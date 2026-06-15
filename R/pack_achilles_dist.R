# Module: Native Achilles pack -- distribution analyses (live compute)
#
# This pack file owns the three Achilles distribution groups:
#   * ACH-F (below): the six "Number of distinct <domain> concepts per person"
#     analyses (203/403/603/703/803/1803).
#   * ACH-G (further down): the eight "Distribution of age ..." analyses
#     (206/406/606/706/806/1806/2106 by domain concept + 506 age at death by
#     gender).
#   * ACH-H (further down): the three drug-exposure field distributions by
#     drug_concept_id (715 days_supply / 716 refills / 717 quantity).
# Each was a precomputed achilles_results_dist row read back verbatim by
# .achillesGetDistributions; here every entry COMPUTES its distribution LIVE
# from the CDM over the scoped cohort (cohort-wide when un-scoped), self-gates
# the population before pulling anything into R, and returns un-gated dist
# row(s) that the SINGLE .omopAnalysisGate strips (min/max), masks (stats below
# nfilter_dist) and bands (count_value). Entry ids/names are unchanged
# (dsomop:achilles.<id>) so existing references keep resolving. The single
# group registrar .omopAchillesDistEntries(handle) returns ALL three groups'
# entries (ACH-F + ACH-G + ACH-H) for the Achilles adapter to concatenate.

#' Is a CDM table present in this database?
#'
#' Shared fail-closed guard for the live Achilles packs: a domain table absent
#' from the connected CDM (e.g. \code{device_exposure}) must make a compute fn
#' return an empty (disclosure-safe, gate-passable) frame rather than let the
#' SELECT hit the missing table and surface a raw "no such table: <name>" SQL
#' error that needlessly leaks which CDM tables are absent. Mirrors the
#' \code{.achillesCompanionPersonCounts} guard (achilles_gating.R:127-130) and
#' the inline check in \code{.omopAchillesRecordFn}.
#'
#' @param handle CDM handle.
#' @param table Character; the CDM table name.
#' @return Logical; TRUE iff the table is present in the database.
#' @keywords internal
.omopAchillesTablePresent <- function(handle, table) {
  bp <- .buildBlueprint(handle)
  table %in% bp$tables$table_name[bp$tables$present_in_db]
}

#' Is a column present on a (present) CDM table?
#'
#' Stricter companion to \code{\link{.omopAchillesTablePresent}} for the
#' drug-field distributions (715/716/717), whose summarised column
#' (\code{days_supply}/\code{refills}/\code{quantity}) a CDM may omit even when
#' \code{drug_exposure} exists. Returns FALSE when the table is absent OR the
#' column is missing, so the fn fails closed to an empty frame instead of a raw
#' "no such column: <name>" error.
#'
#' @param handle CDM handle.
#' @param table Character; the CDM table name.
#' @param column Character; the column name.
#' @return Logical; TRUE iff the table is present AND carries the column.
#' @keywords internal
.omopAchillesColumnPresent <- function(handle, table, column) {
  if (!.omopAchillesTablePresent(handle, table)) return(FALSE)
  bp <- .buildBlueprint(handle)
  cols <- bp$columns[[table]]
  is.data.frame(cols) && column %in% cols$column_name
}

#' Domain -> (table, concept column) map for the per-person distinct-concept
#' distributions.
#'
#' The metric is the per-person count of DISTINCT concepts in a domain, so only
#' the source table and its concept column are needed (no concept-name join: the
#' released metric is a count, not a concept). Kept separate from
#' \code{.omopCovariateSource} because that map has no "visit" domain and carries
#' value/date columns this group never uses.
#'
#' @keywords internal
.omopAchillesDistDomain <- function(domain) {
  switch(as.character(domain),
    "visit"       = list(table = "visit_occurrence",
                         concept_col = "visit_concept_id"),
    "condition"   = list(table = "condition_occurrence",
                         concept_col = "condition_concept_id"),
    "procedure"   = list(table = "procedure_occurrence",
                         concept_col = "procedure_concept_id"),
    "drug"        = list(table = "drug_exposure",
                         concept_col = "drug_concept_id"),
    "observation" = list(table = "observation",
                         concept_col = "observation_concept_id"),
    "measurement" = list(table = "measurement",
                         concept_col = "measurement_concept_id"),
    stop(sprintf("unknown distinct-concept domain: %s", domain)))
}

#' Build the \code{compute$fn} for one distinct-concepts-per-person distribution
#'
#' Inner query: one value per person, \code{v = COUNT(DISTINCT <concept_col>)}
#' over the domain table, restricted to the scoped cohort via
#' \code{.omopScopeJoin} (cohort-wide when un-scoped, preserving the Achilles
#' whole-DB semantics). The population is self-gated with
#' \code{.omopDiagAssertPersons} BEFORE any rows are read. The per-person values
#' are summarised in R into a single dist row exactly like
#' \code{.omopCovariateContinuous}: \code{count_value} = number of persons,
#' \code{min_value}/\code{max_value} (stripped by the gate),
#' \code{avg_value}/\code{stdev_value} and \code{p10..p90_value} (masked by the
#' gate below nfilter_dist). Returns AGGREGATE-ONLY and UN-GATED.
#'
#' @param domain Character; one of the six supported domain keys.
#' @return A \code{function(handle, ctx, params)} producing the raw dist frame.
#' @keywords internal
.omopAchillesDistFn <- function(domain) {
  force(domain)
  function(handle, ctx, params) {
    spec  <- .omopAchillesDistDomain(domain)
    # Fail closed (empty, not error) when the domain table is absent from this
    # CDM, mirroring .omopAchillesRecordFn: an empty frame is a disclosure-safe
    # no-result, whereas hitting a missing table surfaces a raw "no such table"
    # SQL error that leaks which CDM tables are absent.
    if (!.omopAchillesTablePresent(handle, spec$table)) return(data.frame())
    table <- .qualifyTable(handle, spec$table)

    # Defence-in-depth: assert the scoped (or cohort-wide) population rests on
    # >= nfilter persons BEFORE materialising per-person counts in R.
    .omopDiagAssertPersons(handle, ctx, table, "e", "person_id")

    # One value per person: how many DISTINCT domain concepts the person has.
    sj <- .omopScopeJoin(ctx, "e", "person_id")
    per_person <- paste0(
      "SELECT e.person_id AS person_id, ",
      "COUNT(DISTINCT e.", spec$concept_col, ") AS v ",
      "FROM ", table, " e", sj$join,
      " GROUP BY e.person_id")
    vsql <- .sql_translate(per_person, handle$target_dialect)

    raw <- .executeQuery(handle, vsql)
    if (!is.data.frame(raw) || nrow(raw) == 0) return(data.frame())

    v <- suppressWarnings(as.numeric(raw$v))
    v <- v[!is.na(v)]
    if (length(v) == 0) return(data.frame())

    qs <- stats::quantile(v, c(.10, .25, .5, .75, .90), names = FALSE,
                          type = 7)
    # One dist row; quantiles emitted as p10..p90 only (NEVER native 0%/100%,
    # which are min/max). The gate strips min_value/max_value, masks the stats
    # when count_value < nfilter_dist, and bands count_value.
    data.frame(
      count_value  = length(v),
      min_value    = min(v),   # stripped by the gate
      max_value    = max(v),   # stripped by the gate
      avg_value    = mean(v),
      stdev_value  = stats::sd(v),
      p10_value    = qs[1], p25_value = qs[2], median_value = qs[3],
      p75_value    = qs[4], p90_value = qs[5],
      stringsAsFactors = FALSE)
  }
}

#' Build one \code{dsomop:achilles.<id>} distinct-concepts-per-person entry
#'
#' @param analysis_id Integer Achilles analysis id (e.g. 203L).
#' @param description Character; the analysis label.
#' @param domain Character; OMOP domain key used both as the entry domain and to
#'   resolve the source table/concept column.
#' @return An \code{omop_analysis_entry}.
#' @keywords internal
.omopAchillesDistEntry <- function(analysis_id, description, domain) {
  name <- paste0("dsomop:achilles.", as.integer(analysis_id))
  spec <- .omopAchillesDistDomain(domain)
  .omopAnalysisEntry(
    name        = name,
    description = description,
    domain      = domain,
    params      = list(),
    compute     = list(kind = "r", sql = NULL,
                       fn = .omopAchillesDistFn(domain)),
    dependencies = list(tables = spec$table, packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(
      unit       = "dist",
      count_cols = "count_value",
      min_max    = TRUE
    ),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "achilles_live", analysis_id = as.integer(analysis_id))
  )
}

## ---------------------------------------------------------------------------
## Group ACH-G: distribution of AGE by concept (and age at death by gender)
## ---------------------------------------------------------------------------
#
# Live re-implementation of the eight Achilles "Distribution of age ..."
# analyses (206/406/606/706/806/1806/2106 by domain concept, + 506 age at
# death by gender). Each was a precomputed achilles_results_dist row read back
# verbatim by .achillesGetDistributions; here every entry COMPUTES its
# age distribution LIVE from the CDM. The per-stratum value is one age per
# person (year-of-event minus year_of_birth, so it is pure integer arithmetic
# that never needs a day-level birth-date), split into one dist row per
# stratum exactly like .omopCovariateContinuous. The key Achilles disclosure
# risk for these rows — releasing the oldest/youngest age (min/max, e.g.
# "maximum age = 115 years") — is handled CENTRALLY: each row declares
# unit="dist"/min_max=TRUE so the SINGLE .omopAnalysisGate strips min/max,
# masks the summary stats below nfilter_dist and bands count_value. Entry
# ids/names are unchanged (dsomop:achilles.<id>) so existing references keep
# resolving. Population is restricted to ctx$scoped_cohort (INNER JOIN on
# subject_id) or computed cohort-wide when un-scoped (whole-DB Achilles
# semantics). Concept strata are translated to names by default (LEFT JOIN
# concept); no *_source_value / free-text is ever selected.

#' Domain -> (table, concept column, event-date column) map for the age-by-concept
#' distributions.
#'
#' The metric is, per person, the age at their FIRST event of each domain
#' concept — so the source table, its domain concept column (the stratum) and
#' its event-date column (whose calendar year, minus \code{year_of_birth},
#' gives the age) are needed. Kept separate from \code{.omopAchillesDistDomain}
#' (which carries no date column and has no "device" domain) because this group
#' strata by the concept AND needs the event date.
#'
#' @param domain Character; one of the seven concept-stratified domain keys.
#' @return list(table, concept_col, date_col).
#' @keywords internal
.omopAchillesAgeDomain <- function(domain) {
  switch(as.character(domain),
    "visit"       = list(table = "visit_occurrence",
                         concept_col = "visit_concept_id",
                         date_col = "visit_start_date"),
    "condition"   = list(table = "condition_occurrence",
                         concept_col = "condition_concept_id",
                         date_col = "condition_start_date"),
    "procedure"   = list(table = "procedure_occurrence",
                         concept_col = "procedure_concept_id",
                         date_col = "procedure_date"),
    "drug"        = list(table = "drug_exposure",
                         concept_col = "drug_concept_id",
                         date_col = "drug_exposure_start_date"),
    "observation" = list(table = "observation",
                         concept_col = "observation_concept_id",
                         date_col = "observation_date"),
    "measurement" = list(table = "measurement",
                         concept_col = "measurement_concept_id",
                         date_col = "measurement_date"),
    "device"      = list(table = "device_exposure",
                         concept_col = "device_concept_id",
                         date_col = "device_exposure_start_date"),
    stop(sprintf("unknown age-by-concept domain: %s", domain)))
}

#' Summarise per-(stratum, person) ages into per-stratum dist rows
#'
#' Shared tail for every ACH-G entry: given a frame of one age value per person
#' within each stratum (columns \code{stratum_id}, \code{stratum_name},
#' \code{age}; all lower-case as \code{.executeQuery} returns them), split by
#' \code{stratum_id} and emit ONE dist row per stratum, identical in shape to
#' \code{.omopCovariateContinuous}: \code{concept_id}/\code{concept_name} label
#' the stratum, \code{count_value} = number of persons, \code{min_value}/
#' \code{max_value} (stripped by the gate), \code{avg_value}/\code{stdev_value}
#' and \code{p10..p90_value} (masked by the gate below nfilter_dist). Quantiles
#' are p10/p25/median/p75/p90 ONLY (never native 0%/100%, which are min/max).
#' Returns AGGREGATE-ONLY and UN-GATED (no person key, no pre-suppression).
#'
#' @param raw Data frame with \code{stratum_id}, \code{stratum_name},
#'   \code{age} (one row per person within each stratum).
#' @return Data frame of per-stratum dist rows, or an empty frame when no ages.
#' @keywords internal
.omopAchillesAgeSummarise <- function(raw) {
  if (!is.data.frame(raw) || nrow(raw) == 0) return(data.frame())
  raw$age <- suppressWarnings(as.numeric(raw$age))
  raw <- raw[!is.na(raw$age), , drop = FALSE]
  if (nrow(raw) == 0) return(data.frame())

  parts <- split(raw, raw$stratum_id)
  rows <- lapply(parts, function(p) {
    v  <- p$age
    qs <- stats::quantile(v, c(.10, .25, .5, .75, .90), names = FALSE,
                          type = 7)
    data.frame(
      concept_id   = p$stratum_id[1],
      concept_name = p$stratum_name[1],
      count_value  = length(v),
      min_value    = min(v),   # stripped by the gate
      max_value    = max(v),   # stripped by the gate
      avg_value    = mean(v),
      stdev_value  = stats::sd(v),
      p10_value    = qs[1], p25_value = qs[2], median_value = qs[3],
      p75_value    = qs[4], p90_value = qs[5],
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out <- out[order(-out$count_value), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Build the \code{compute$fn} for one age-by-domain-concept distribution
#'
#' Inner query: one age per person within each domain concept, where age is the
#' calendar year of the person's FIRST event of that concept minus their
#' \code{year_of_birth} (\code{.omopYearExpr(MIN(<date_col>)) - year_of_birth} —
#' integer arithmetic, no day-level birth date). The domain table is joined to
#' \code{person} for \code{year_of_birth} and restricted to the scoped cohort
#' via \code{.omopScopeJoin} (cohort-wide when un-scoped, preserving the
#' Achilles whole-DB semantics). The stratum concept id is LEFT JOINed onto
#' \code{concept} so its name is returned (translation default ON). The
#' population is self-gated with \code{.omopDiagAssertPersons} BEFORE any rows
#' are read; per-person ages are then summarised in R via
#' \code{.omopAchillesAgeSummarise}. Returns AGGREGATE-ONLY and UN-GATED.
#'
#' @param domain Character; one of the seven concept-stratified domain keys.
#' @return A \code{function(handle, ctx, params)} producing the raw dist frame.
#' @keywords internal
.omopAchillesAgeByConceptFn <- function(domain) {
  force(domain)
  function(handle, ctx, params) {
    spec    <- .omopAchillesAgeDomain(domain)
    # Fail closed (empty, not error) when the domain table is absent from this
    # CDM (e.g. device_exposure absent -> achilles.2106), mirroring
    # .omopAchillesRecordFn: an empty frame is a disclosure-safe no-result.
    if (!.omopAchillesTablePresent(handle, spec$table)) return(data.frame())
    table   <- .qualifyTable(handle, spec$table)
    person  <- .qualifyTable(handle, "person")
    concept <- .qualifyTable(handle, "concept",
                             handle$vocab_schema %||% handle$cdm_schema)

    # Defence-in-depth: assert the scoped (or cohort-wide) population over the
    # domain table rests on >= nfilter persons BEFORE materialising ages in R.
    .omopDiagAssertPersons(handle, ctx, table, "e", "person_id")

    age_expr <- paste0(.omopYearExpr(handle, paste0("MIN(e.", spec$date_col,
                                                    ")")),
                       " - p.year_of_birth")
    sj <- .omopScopeJoin(ctx, "e", "person_id")
    # One age per (concept, person): age at the person's first event of the
    # concept. GROUP BY the concept + person + year_of_birth so the MIN(date)
    # is per person within the concept.
    per_person <- paste0(
      "SELECT e.", spec$concept_col, " AS stratum_id, e.person_id AS person_id, ",
      age_expr, " AS age ",
      "FROM ", table, " e",
      " INNER JOIN ", person, " p ON p.person_id = e.person_id", sj$join,
      " WHERE e.", spec$concept_col, " IS NOT NULL",
      " GROUP BY e.", spec$concept_col, ", e.person_id, p.year_of_birth")
    vsql <- .sql_translate(paste0(
      "SELECT pp.stratum_id, cc.concept_name AS stratum_name, pp.age ",
      "FROM (", per_person, ") pp",
      " LEFT JOIN ", concept, " cc ON cc.concept_id = pp.stratum_id"),
      handle$target_dialect)

    raw <- .executeQuery(handle, vsql)
    .omopAchillesAgeSummarise(raw)
  }
}

#' Build the \code{compute$fn} for analysis 506 (age at death, by gender)
#'
#' One age per deceased person — \code{.omopYearExpr(death_date) -
#' year_of_birth} — stratified by the person's \code{gender_concept_id}
#' (translated to its name). The \code{death} table is joined to \code{person}
#' for \code{year_of_birth}/\code{gender_concept_id} and restricted to the
#' scoped cohort via \code{.omopScopeJoin} (cohort-wide when un-scoped). Death
#' is one row per person, so no MIN/GROUP BY is needed for the per-person value.
#' Self-gates the population with \code{.omopDiagAssertPersons} over the death
#' table BEFORE materialising; ages are summarised in R via
#' \code{.omopAchillesAgeSummarise}. Returns AGGREGATE-ONLY and UN-GATED.
#'
#' @return A \code{function(handle, ctx, params)} producing the raw dist frame.
#' @keywords internal
.omopAchillesAgeAtDeathFn <- function() {
  function(handle, ctx, params) {
    death   <- .qualifyTable(handle, "death")
    person  <- .qualifyTable(handle, "person")
    concept <- .qualifyTable(handle, "concept",
                             handle$vocab_schema %||% handle$cdm_schema)

    .omopDiagAssertPersons(handle, ctx, death, "e", "person_id")

    age_expr <- paste0(.omopYearExpr(handle, "e.death_date"),
                       " - p.year_of_birth")
    sj <- .omopScopeJoin(ctx, "e", "person_id")
    per_person <- paste0(
      "SELECT p.gender_concept_id AS stratum_id, e.person_id AS person_id, ",
      age_expr, " AS age ",
      "FROM ", death, " e",
      " INNER JOIN ", person, " p ON p.person_id = e.person_id", sj$join,
      " WHERE e.death_date IS NOT NULL")
    vsql <- .sql_translate(paste0(
      "SELECT pp.stratum_id, cc.concept_name AS stratum_name, pp.age ",
      "FROM (", per_person, ") pp",
      " LEFT JOIN ", concept, " cc ON cc.concept_id = pp.stratum_id"),
      handle$target_dialect)

    raw <- .executeQuery(handle, vsql)
    .omopAchillesAgeSummarise(raw)
  }
}

#' Build one \code{dsomop:achilles.<id>} age-distribution entry (ACH-G)
#'
#' Shared builder for every ACH-G entry: unit="dist", count_cols="count_value",
#' min_max=TRUE (so the gate strips min/max age and masks stats below
#' nfilter_dist), cohort-OPTIONAL (accepts_cohort/accepts_tables TRUE,
#' max_tables 1L, requires_cohort FALSE so an un-scoped run computes cohort-wide
#' preserving whole-DB Achilles semantics), kind="r", and
#' meta=list(adapter="achilles_live", analysis_id=<id>) — the analysis_id is a
#' LABEL only (never used to read a results table).
#'
#' @param analysis_id Integer Achilles analysis id (e.g. 206L).
#' @param description Character; the analysis label.
#' @param domain Character; OMOP domain key (listing + table resolution).
#' @param tables Character vector of CDM tables the fn reads (deps WARN).
#' @param fn The kind="r" compute function(handle, ctx, params).
#' @return An \code{omop_analysis_entry}.
#' @keywords internal
.omopAchillesAgeEntry <- function(analysis_id, description, domain, tables, fn) {
  .omopAnalysisEntry(
    name        = paste0("dsomop:achilles.", as.integer(analysis_id)),
    description = description,
    domain      = domain,
    params      = list(),
    compute     = list(kind = "r", sql = NULL, fn = fn),
    dependencies = list(tables = unique(c(tables, "person", "concept")),
                        packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(
      unit       = "dist",
      count_cols = "count_value",
      min_max    = TRUE
    ),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "achilles_live", analysis_id = as.integer(analysis_id))
  )
}

#' Emit the ACH-G age-distribution Achilles catalog entries (live compute)
#'
#' One \code{omop_analysis_entry} per age-distribution analysis: the seven
#' age-by-domain-concept entries (206/406/606/706/806/1806/2106) plus 506 (age
#' at death by gender). Each COMPUTES its age distribution LIVE from the CDM
#' (no \code{achilles_results_dist} read). \code{handle} is taken for signature
#' parity ONLY and is never queried at build time.
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by entry id.
#' @keywords internal
.omopAchillesAgeEntries <- function(handle) {
  by_concept <- list(
    list(id = 206L,  domain = "visit",
         description = "Distribution of age by visit_concept_id"),
    list(id = 406L,  domain = "condition",
         description = "Distribution of age by condition_concept_id"),
    list(id = 606L,  domain = "procedure",
         description = "Distribution of age by procedure_concept_id"),
    list(id = 706L,  domain = "drug",
         description = "Distribution of age by drug_concept_id"),
    list(id = 806L,  domain = "observation",
         description = "Distribution of age by observation_concept_id"),
    list(id = 1806L, domain = "measurement",
         description = "Distribution of age by measurement_concept_id"),
    list(id = 2106L, domain = "device",
         description = "Distribution of age by device_concept_id")
  )
  entries <- lapply(by_concept, function(s)
    .omopAchillesAgeEntry(
      s$id, s$description, s$domain,
      tables = .omopAchillesAgeDomain(s$domain)$table,
      fn = .omopAchillesAgeByConceptFn(s$domain)))

  # 506: age at death, stratified by gender (gender from person, not a domain
  # concept) — its own fn and "death" domain.
  entries <- c(entries, list(
    .omopAchillesAgeEntry(
      506L, "Distribution of age at death by gender", "death",
      tables = "death", fn = .omopAchillesAgeAtDeathFn())))

  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}

## ---------------------------------------------------------------------------
## Group ACH-H: distribution of drug-exposure fields by drug_concept_id
## ---------------------------------------------------------------------------
#
# Live re-implementation of the three Achilles drug-exposure field
# distributions: 715 days_supply, 716 refills, 717 quantity, each stratified by
# drug_concept_id. Each was a precomputed achilles_results_dist row read back
# verbatim by .achillesGetDistributions; here every entry COMPUTES its
# distribution LIVE from the CDM. The metric reported (avg/sd/quantiles of the
# field, by drug) matches Achilles 715/716/717, but the PER-STRATUM UNIT is
# DISTINCT PERSONS, NOT records: one value per (drug_concept_id, person) — that
# person's MEAN of the field across their records of the drug — exactly like
# .omopCovariateContinuous, so count_value is a distinct-person count. Standard
# OHDSI Achilles (OHDSI/Achilles inst/sql .../715.sql etc.) emits a per-RECORD
# distribution, but that is unsafe in this federated/DataSHIELD context: a
# per-record count_value can be large while the whole (drug_concept_id) stratum
# rests on a SINGLE person with many prescriptions, and the dist gate keys its
# nfilter_dist mask + band ONLY on count_value — so a per-record basis would
# release a single-person quantity/days_supply/refills distribution unmasked.
# Grounding count_value on DISTINCT PERSONS makes the gate's mask + band protect
# a >= nfilter_dist-PERSON distribution, satisfying the hard person-grounding
# invariant every other dsOMOP dist analysis already follows.
# The population is restricted to ctx$scoped_cohort (INNER JOIN on subject_id via
# .omopScopeJoin) or computed cohort-wide when un-scoped (whole-DB Achilles
# semantics). The drug_concept_id stratum is LEFT JOINed onto concept so its name
# is returned (translation default ON); no *_source_value / free-text is ever
# selected. The key Achilles disclosure risk here — releasing the largest
# quantity/days_supply (the min/max outlier) — is handled CENTRALLY: each row
# declares unit="dist"/min_max=TRUE so the SINGLE .omopAnalysisGate strips
# min/max, masks the summary stats below nfilter_dist and bands count_value.

#' drug_exposure field -> the numeric column whose distribution is summarised.
#'
#' Maps the three ACH-H analysis ids to their drug_exposure numeric column.
#' Centralised so the field name appears exactly once per analysis and the
#' compute fn + entry builder cannot drift apart.
#'
#' @param analysis_id Integer; one of 715L/716L/717L.
#' @return Character; the drug_exposure column name (days_supply/refills/quantity).
#' @keywords internal
.omopAchillesDrugField <- function(analysis_id) {
  switch(as.character(as.integer(analysis_id)),
    "715" = "days_supply",
    "716" = "refills",
    "717" = "quantity",
    stop(sprintf("unknown drug-field distribution analysis: %s", analysis_id)))
}

#' Summarise per-(drug_concept, person) values into per-concept dist rows
#'
#' Shared tail for every ACH-H entry: given a frame of one value per PERSON
#' within each drug concept (columns \code{covariate_id}, \code{covariate_name},
#' \code{v}; all lower-case as \code{.executeQuery} returns them), split by
#' \code{covariate_id} and emit ONE dist row per concept, identical in shape to
#' \code{.omopCovariateContinuous}: \code{covariate_id}/\code{covariate_name}
#' label the concept, \code{count_value} = number of DISTINCT PERSONS,
#' \code{min_value}/\code{max_value} (stripped by the gate),
#' \code{avg_value}/\code{stdev_value} and \code{p10..p90_value} (masked by the
#' gate below nfilter_dist). Quantiles are p10/p25/median/p75/p90 ONLY (never
#' native 0%/100%, which are min/max). Returns AGGREGATE-ONLY and UN-GATED (no
#' person key, no pre-suppression).
#'
#' @param raw Data frame with \code{covariate_id}, \code{covariate_name},
#'   \code{v} (one row per person within each concept).
#' @return Data frame of per-concept dist rows, or an empty frame when no values.
#' @keywords internal
.omopAchillesDrugFieldSummarise <- function(raw) {
  if (!is.data.frame(raw) || nrow(raw) == 0) return(data.frame())
  raw$v <- suppressWarnings(as.numeric(raw$v))
  raw <- raw[!is.na(raw$v), , drop = FALSE]
  if (nrow(raw) == 0) return(data.frame())

  parts <- split(raw, raw$covariate_id)
  rows <- lapply(parts, function(p) {
    v  <- p$v
    qs <- stats::quantile(v, c(.10, .25, .5, .75, .90), names = FALSE,
                          type = 7)
    data.frame(
      covariate_id   = p$covariate_id[1],
      covariate_name = p$covariate_name[1],
      count_value    = length(v),
      min_value      = min(v),   # stripped by the gate
      max_value      = max(v),   # stripped by the gate
      avg_value      = mean(v),
      stdev_value    = stats::sd(v),
      p10_value      = qs[1], p25_value = qs[2], median_value = qs[3],
      p75_value      = qs[4], p90_value = qs[5],
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out <- out[order(-out$count_value), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Build the \code{compute$fn} for one drug-exposure field distribution (ACH-H)
#'
#' Inner query: one value per (drug_concept_id, PERSON) — the per-person MEAN of
#' \code{<field>} (days_supply / refills / quantity) across that person's records
#' of the drug — labelled by its \code{drug_concept_id}, over
#' \code{drug_exposure} restricted to the scoped cohort via \code{.omopScopeJoin}
#' (cohort-wide when un-scoped, preserving the Achilles whole-DB semantics) and
#' filtered to \code{<field> IS NOT NULL}. The drug concept id is LEFT JOINed
#' onto \code{concept} so its name is returned (translation default ON). The
#' population is self-gated with \code{.omopDiagAssertPersons} over
#' \code{drug_exposure} BEFORE any rows are read; the per-person values are then
#' summarised into one dist row per concept via
#' \code{.omopAchillesDrugFieldSummarise} (so \code{count_value} is a
#' DISTINCT-PERSON count the gate's nfilter_dist mask + band actually protect).
#' Returns AGGREGATE-ONLY and UN-GATED.
#'
#' @param field Character; the drug_exposure numeric column to summarise.
#' @return A \code{function(handle, ctx, params)} producing the raw dist frame.
#' @keywords internal
.omopAchillesDrugFieldFn <- function(field) {
  force(field)
  function(handle, ctx, params) {
    # Fail closed (empty, not error) when drug_exposure is absent OR lacks the
    # summarised field column (a CDM may omit days_supply/refills/quantity),
    # mirroring .omopAchillesRecordFn: an empty frame is a disclosure-safe
    # no-result, whereas hitting a missing table/column surfaces a raw
    # "no such table/column" SQL error that leaks which CDM tables/fields exist.
    if (!.omopAchillesColumnPresent(handle, "drug_exposure", field)) {
      return(data.frame())
    }
    table   <- .qualifyTable(handle, "drug_exposure")
    concept <- .qualifyTable(handle, "concept",
                             handle$vocab_schema %||% handle$cdm_schema)

    # Defence-in-depth: assert the scoped (or cohort-wide) population over
    # drug_exposure rests on >= nfilter persons BEFORE materialising values in R.
    .omopDiagAssertPersons(handle, ctx, table, "e", "person_id")

    # One value per (drug_concept_id, PERSON): the per-person mean of the field
    # across that person's records of the drug, kept only where the field is
    # populated. This makes count_value a DISTINCT-PERSON count so the gate's
    # nfilter_dist mask + band genuinely protect a >= nfilter_dist-PERSON
    # distribution (a per-RECORD basis could let one person's many prescriptions
    # release a single-person quantity/days_supply/refills distribution).
    sj <- .omopScopeJoin(ctx, "e", "person_id")
    per_person <- paste0(
      "SELECT e.drug_concept_id AS covariate_id, e.person_id AS person_id, ",
      "AVG(CAST(e.", field, " AS FLOAT)) AS v ",
      "FROM ", table, " e", sj$join,
      " WHERE e.", field, " IS NOT NULL",
      " GROUP BY e.drug_concept_id, e.person_id")
    vsql <- .sql_translate(paste0(
      "SELECT pp.covariate_id, cc.concept_name AS covariate_name, pp.v ",
      "FROM (", per_person, ") pp",
      " LEFT JOIN ", concept, " cc ON cc.concept_id = pp.covariate_id"),
      handle$target_dialect)

    raw <- .executeQuery(handle, vsql)
    .omopAchillesDrugFieldSummarise(raw)
  }
}

#' Build one \code{dsomop:achilles.<id>} drug-field distribution entry (ACH-H)
#'
#' Shared builder for every ACH-H entry: unit="dist", count_cols="count_value",
#' min_max=TRUE (so the gate strips min/max and masks stats below nfilter_dist),
#' cohort-OPTIONAL (accepts_cohort/accepts_tables TRUE, max_tables 1L,
#' requires_cohort FALSE so an un-scoped run computes cohort-wide preserving
#' whole-DB Achilles semantics), kind="r", and
#' meta=list(adapter="achilles_live", analysis_id=<id>) — the analysis_id is a
#' LABEL only (never used to read a results table).
#'
#' @param analysis_id Integer Achilles analysis id (715L/716L/717L).
#' @param description Character; the analysis label.
#' @return An \code{omop_analysis_entry}.
#' @keywords internal
.omopAchillesDrugFieldEntry <- function(analysis_id, description) {
  field <- .omopAchillesDrugField(analysis_id)
  .omopAnalysisEntry(
    name        = paste0("dsomop:achilles.", as.integer(analysis_id)),
    description = description,
    domain      = "drug",
    params      = list(),
    compute     = list(kind = "r", sql = NULL,
                       fn = .omopAchillesDrugFieldFn(field)),
    dependencies = list(tables = c("drug_exposure", "concept"),
                        packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(
      unit       = "dist",
      count_cols = "count_value",
      min_max    = TRUE
    ),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "achilles_live", analysis_id = as.integer(analysis_id))
  )
}

#' Emit the ACH-H drug-field-distribution Achilles catalog entries (live compute)
#'
#' One \code{omop_analysis_entry} per drug-exposure field distribution: 715
#' (days_supply), 716 (refills), 717 (quantity), each by drug_concept_id. Each
#' COMPUTES its distribution LIVE from the CDM (no \code{achilles_results_dist}
#' read). \code{handle} is taken for signature parity ONLY and is never queried
#' at build time.
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by entry id.
#' @keywords internal
.omopAchillesDrugFieldEntries <- function(handle) {
  specs <- list(
    list(id = 715L, description = "Distribution of days_supply by drug_concept_id"),
    list(id = 716L, description = "Distribution of refills by drug_concept_id"),
    list(id = 717L, description = "Distribution of quantity by drug_concept_id")
  )
  entries <- lapply(specs, function(s)
    .omopAchillesDrugFieldEntry(s$id, s$description))
  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}

#' Native Achilles pack: per-person distributions (ACH-F), age distributions
#' (ACH-G) and drug-field distributions (ACH-H)
#'
#' Returns ALL live-computing dist entries owned by this pack file, keyed by
#' entry name (the authoritative id): the six distinct-concepts-per-person
#' distributions (ACH-F: 203/403/603/703/803/1803), the eight age distributions
#' (ACH-G: 206/406/606/706/806/1806/2106 + 506) and the three drug-field
#' distributions (ACH-H: 715/716/717). Ready to be concatenated into the
#' Achilles registrar. \code{handle} is taken for signature parity only and is
#' NOT queried at build time (all DB I/O happens later inside each
#' \code{compute$fn}).
#'
#' @param handle CDM handle (signature parity; not queried here).
#' @return Named list of \code{omop_analysis_entry} objects keyed by entry name.
#' @keywords internal
.omopAchillesDistEntries <- function(handle) {
  specs <- list(
    list(id = 203L,  domain = "visit",
         description = "Number of distinct visit concepts per person"),
    list(id = 403L,  domain = "condition",
         description = "Number of distinct condition concepts per person"),
    list(id = 603L,  domain = "procedure",
         description = "Number of distinct procedure concepts per person"),
    list(id = 703L,  domain = "drug",
         description = "Number of distinct drug concepts per person"),
    list(id = 803L,  domain = "observation",
         description = "Number of distinct observation concepts per person"),
    list(id = 1803L, domain = "measurement",
         description = "Number of distinct measurement concepts per person")
  )
  distinct_concept <- lapply(specs, function(s)
    .omopAchillesDistEntry(s$id, s$description, s$domain))
  distinct_concept <- stats::setNames(
    distinct_concept, vapply(distinct_concept, function(e) e$name, character(1)))

  c(distinct_concept, .omopAchillesAgeEntries(handle),
    .omopAchillesDrugFieldEntries(handle))
}
