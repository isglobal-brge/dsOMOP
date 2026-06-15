# Module: Native Achilles person-domain analyses (live-computing pack)
#
# Native replacements for the Achilles person-domain catalog entries
# (dsomop:achilles.1..12 + the cross-domain intersection counts 2000..2003).
# Where the legacy Achilles adapter READ a precomputed achilles_results row, each
# entry here COMPUTES its distinct-person count LIVE from the CDM person table
# (and, for the intersections, person-id membership sub-selects over the domain
# tables), restricted to ctx$scoped_cohort when scoped and cohort-wide otherwise
# (whole-DB Achilles semantics). Every entry returns a RAW aggregate-only frame
# whose person-count column (count_value) is declared so the SINGLE
# .omopAnalysisGate small-cell-suppresses + bands it; concept ids are translated
# to names by default via a LEFT JOIN onto concept; no *_source_value is ever
# selected.
#
# Entry ids are byte-for-byte the legacy ids (dsomop:achilles.<id>) so existing
# references keep resolving — only the compute changes (read-results ->
# live-compute). The registrar .omopAchillesPersonEntries(handle) returns the
# group's entries keyed by id; it takes handle for signature parity ONLY and
# performs NO DB I/O at build time (all queries run later inside compute$fn).

#' Demographic distinct-person counts over PERSON, parameterised by GROUP BY
#'
#' One shared kernel for the simple person-demographic Achilles analyses
#' (1/2/3/4/5/10/12). Counts \code{COUNT(DISTINCT person_id)} over the person
#' table, restricted to the scoped cohort via an INNER JOIN on
#' \code{subject_id = person_id} (cohort-wide when un-scoped), grouped by the
#' supplied concept columns. Each grouping concept id (gender/race/ethnicity) is
#' LEFT JOINed onto \code{concept} so its name is returned (translation default
#' ON); \code{year_of_birth} is a coarse, non-disclosive integer and is returned
#' without a concept join. Self-gates the scoped population with
#' \code{.omopDiagAssertPersons} BEFORE materialising. Returns an aggregate-only
#' frame with a \code{count_value} person-count column (un-gated; the single gate
#' suppresses + bands it).
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries ctx$scoped_cohort).
#' @param group_cols Character vector of \code{p.}-qualified person columns to
#'   GROUP BY (empty for the single-scalar analysis 1).
#' @param concept_cols Named character vector mapping a \code{p.}-qualified
#'   concept-id column to the SELECT alias for its translated name (e.g.
#'   \code{c(gender_name = "p.gender_concept_id")}); empty for analyses with no
#'   concept to translate.
#' @return Aggregate-only data.frame with the grouping cols, any concept-name
#'   cols, and \code{count_value}.
#' @keywords internal
.achillesPersonCount <- function(handle, ctx, group_cols = character(0),
                                 concept_cols = character(0)) {
  person  <- .qualifyTable(handle, "person")
  concept <- .qualifyTable(handle, "concept",
                           handle$vocab_schema %||% handle$cdm_schema)

  sj <- .omopScopeJoin(ctx, "p", "person_id")

  # Self-gate the scoped (or cohort-wide) population before pulling any rows.
  .omopDiagAssertPersons(handle, ctx, person, "p", "person_id")

  # SELECT list: grouping cols, then a translated name per concept col, then the
  # distinct-person count.
  select_parts <- character(0)
  concept_joins <- character(0)
  alias_i <- 0L
  for (gc in group_cols) {
    select_parts <- c(select_parts, gc)
  }
  for (alias_name in names(concept_cols)) {
    id_col <- concept_cols[[alias_name]]
    alias_i <- alias_i + 1L
    cj <- paste0("cj", alias_i)
    select_parts <- c(select_parts,
                      paste0(cj, ".concept_name AS ", alias_name))
    concept_joins <- c(concept_joins,
                       paste0(" LEFT JOIN ", concept, " ", cj,
                              " ON ", cj, ".concept_id = ", id_col))
  }
  select_parts <- c(select_parts,
                    "COUNT(DISTINCT p.person_id) AS count_value")

  group_by <- if (length(group_cols) > 0) {
    paste0(" GROUP BY ", paste(group_cols, collapse = ", "))
  } else ""
  order_by <- if (length(group_cols) > 0) " ORDER BY count_value DESC" else ""

  sql <- .sql_translate(paste0(
    "SELECT ", paste(select_parts, collapse = ", "),
    " FROM ", person, " p", sj$join,
    paste(concept_joins, collapse = ""),
    group_by, order_by),
    handle$target_dialect)
  .executeQuery(handle, sql)
}

#' Cross-domain distinct-person intersection count
#'
#' Single-scalar \code{COUNT(DISTINCT person_id) AS count_value} over the scoped
#' person population (cohort-wide when un-scoped) restricted to persons present in
#' EVERY supplied domain table (Achilles 2000..2003: e.g. >=1 Dx AND >=1 Rx). The
#' restriction is a chain of \code{person_id IN (SELECT person_id FROM <domain>)}
#' predicates — no concept ids, no source values. Self-gates the scoped+filtered
#' population before materialising. Returns an aggregate-only one-row frame.
#'
#' @param handle CDM handle.
#' @param ctx Run-path ctx (carries ctx$scoped_cohort).
#' @param domain_tables Character vector of CDM table names every counted person
#'   must appear in.
#' @return One-row aggregate-only data.frame with \code{count_value}.
#' @keywords internal
.achillesPersonIntersection <- function(handle, ctx, domain_tables) {
  person <- .qualifyTable(handle, "person")
  preds <- vapply(domain_tables, function(tbl) {
    qualified <- .qualifyTable(handle, tbl)
    paste0("p.person_id IN (SELECT person_id FROM ", qualified, ")")
  }, character(1))
  where_sql <- paste(preds, collapse = " AND ")

  # Self-gate the scoped + intersection-filtered population before materialising.
  .omopDiagAssertPersons(handle, ctx, person, "p", "person_id",
                         where_sql = where_sql)

  sj <- .omopScopeJoin(ctx, "p", "person_id")
  sql <- .sql_translate(paste0(
    "SELECT COUNT(DISTINCT p.person_id) AS count_value FROM ", person, " p",
    sj$join, " WHERE ", where_sql),
    handle$target_dialect)
  .executeQuery(handle, sql)
}

#' Build one native person-domain Achilles catalog entry
#'
#' Shared builder: every entry in this group is unit="person", count_cols=
#' "count_value", cohort-OPTIONAL (accepts_cohort/accepts_tables TRUE, max_tables
#' 1L, requires_cohort FALSE so an un-scoped run computes cohort-wide preserving
#' whole-DB Achilles semantics), kind="r", and carries
#' meta=list(adapter="achilles", analysis_id=<id>) — the analysis_id is a LABEL
#' only (never used to read a results table).
#'
#' @param analysis_id Integer Achilles analysis id (the stable id suffix).
#' @param description Human-readable description.
#' @param domain OMOP domain string for listing ("person").
#' @param tables Character vector of CDM tables the fn reads (for the deps WARN).
#' @param fn The kind="r" compute function(handle, ctx, params).
#' @return An \code{omop_analysis_entry}.
#' @keywords internal
.achillesPersonEntry <- function(analysis_id, description, domain, tables, fn) {
  .omopAnalysisEntry(
    name        = paste0("dsomop:achilles.", analysis_id),
    description = description,
    domain      = tolower(domain %||% "person"),
    params      = list(),
    compute     = list(kind = "r", sql = NULL, fn = fn),
    dependencies = list(tables = tables, packages = character(0)),
    disclosure  = .omopAnalysisDisclosure(unit = "person",
                                          count_cols = "count_value"),
    scope = .omopAnalysisScope(accepts_cohort = TRUE, accepts_tables = TRUE,
                               max_tables = 1L, requires_cohort = FALSE),
    mode  = "aggregate",
    meta  = list(adapter = "achilles", analysis_id = as.integer(analysis_id))
  )
}

#' Emit the native person-domain Achilles catalog entries (live-computing)
#'
#' Group registrar for \code{pack_achilles_person.R}. Returns the group's
#' \code{omop_analysis_entry} objects keyed by their stable
#' \code{dsomop:achilles.<id>} ids (1/2/3/4/5/10/12 demographic counts +
#' 2000/2001/2002/2003 cross-domain intersection counts). Takes \code{handle} for
#' signature parity with the other adapters ONLY — it performs NO DB I/O at build
#' time; all queries run later inside each entry's \code{compute$fn}.
#'
#' @param handle CDM handle (signature parity; not queried at build time).
#' @return Named list of \code{omop_analysis_entry} objects keyed by id.
#' @keywords internal
.omopAchillesPersonEntries <- function(handle) {
  entries <- list(
    # 1 — Number of persons (single scalar).
    .achillesPersonEntry(
      1L, "Number of persons.", "person", c("person"),
      function(handle, ctx, params) .achillesPersonCount(handle, ctx)),

    # 2 — Number of persons by gender.
    .achillesPersonEntry(
      2L, "Number of persons by gender.", "person", c("person", "concept"),
      function(handle, ctx, params) {
        .achillesPersonCount(handle, ctx,
          group_cols = "p.gender_concept_id",
          concept_cols = c(gender_name = "p.gender_concept_id"))
      }),

    # 3 — Number of persons by year of birth (year is coarse/non-disclosive).
    .achillesPersonEntry(
      3L, "Number of persons by year of birth.", "person", c("person"),
      function(handle, ctx, params) {
        .achillesPersonCount(handle, ctx, group_cols = "p.year_of_birth")
      }),

    # 4 — Number of persons by race.
    .achillesPersonEntry(
      4L, "Number of persons by race.", "person", c("person", "concept"),
      function(handle, ctx, params) {
        .achillesPersonCount(handle, ctx,
          group_cols = "p.race_concept_id",
          concept_cols = c(race_name = "p.race_concept_id"))
      }),

    # 5 — Number of persons by ethnicity.
    .achillesPersonEntry(
      5L, "Number of persons by ethnicity.", "person", c("person", "concept"),
      function(handle, ctx, params) {
        .achillesPersonCount(handle, ctx,
          group_cols = "p.ethnicity_concept_id",
          concept_cols = c(ethnicity_name = "p.ethnicity_concept_id"))
      }),

    # 10 — Number of persons by year of birth by gender.
    .achillesPersonEntry(
      10L, "Number of persons by year of birth and gender.", "person",
      c("person", "concept"),
      function(handle, ctx, params) {
        .achillesPersonCount(handle, ctx,
          group_cols = c("p.year_of_birth", "p.gender_concept_id"),
          concept_cols = c(gender_name = "p.gender_concept_id"))
      }),

    # 12 — Number of persons by race and ethnicity.
    .achillesPersonEntry(
      12L, "Number of persons by race and ethnicity.", "person",
      c("person", "concept"),
      function(handle, ctx, params) {
        .achillesPersonCount(handle, ctx,
          group_cols = c("p.race_concept_id", "p.ethnicity_concept_id"),
          concept_cols = c(race_name = "p.race_concept_id",
                           ethnicity_name = "p.ethnicity_concept_id"))
      }),

    # 2000 — persons with >=1 condition AND >=1 drug.
    .achillesPersonEntry(
      2000L, "Number of persons with at least 1 condition and 1 drug.",
      "general", c("person", "condition_occurrence", "drug_exposure"),
      function(handle, ctx, params) {
        .achillesPersonIntersection(handle, ctx,
          c("condition_occurrence", "drug_exposure"))
      }),

    # 2001 — persons with >=1 condition AND >=1 procedure.
    .achillesPersonEntry(
      2001L, "Number of persons with at least 1 condition and 1 procedure.",
      "general", c("person", "condition_occurrence", "procedure_occurrence"),
      function(handle, ctx, params) {
        .achillesPersonIntersection(handle, ctx,
          c("condition_occurrence", "procedure_occurrence"))
      }),

    # 2002 — persons with >=1 measurement AND >=1 condition AND >=1 drug.
    .achillesPersonEntry(
      2002L,
      "Number of persons with at least 1 measurement, 1 condition and 1 drug.",
      "general",
      c("person", "measurement", "condition_occurrence", "drug_exposure"),
      function(handle, ctx, params) {
        .achillesPersonIntersection(handle, ctx,
          c("measurement", "condition_occurrence", "drug_exposure"))
      }),

    # 2003 — persons with >=1 visit.
    .achillesPersonEntry(
      2003L, "Number of persons with at least 1 visit.", "general",
      c("person", "visit_occurrence"),
      function(handle, ctx, params) {
        .achillesPersonIntersection(handle, ctx, c("visit_occurrence"))
      })
  )

  stats::setNames(entries, vapply(entries, function(e) e$name, character(1)))
}
