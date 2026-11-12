# Module: the legacy-spec adapter (design-2.10.3 sections 5, 6 and 6.9).
#
# A v1 inline cohort spec lowers to the closed IR variant
#   populations.base = {kind: "index",
#                       index: {source: {legacy_spec: <spec>}, select: "all",
#                               restrict_to_observation: false},
#                       coverage: {on_missing: "keep"}}
# and no language 2.0 index semantics are imposed on it: the adapter runs the
# unchanged .cohortCreate() (DISTINCT (subject_id, start, COALESCE(end,
# start)) rows of the spec's table, inclusion criteria applied per row,
# value_bin as half-open bins, no observation-period coverage) and wraps its
# rows as the section 6.9 episode table, whose origin identity is the declared
# tuple (index_source = "legacy_spec", index_key = NULL) and whose
# cohort_row_id ranks (subject_id, cohort_start_date, cohort_end_date), the
# legacy tie order.

#' Translate the IR's closed legacy_spec into the .cohortCreate() spec
#' @param spec The \code{legacy_spec} object of the IR (parsed JSON form).
#' @return A named list accepted by \code{.cohortCreate()}.
#' @keywords internal
.legacySpecCohortSpec <- function(spec) {
  if (!is.list(spec) || (length(spec) > 0L && is.null(names(spec)))) {
    stop("legacy_spec must be an object.", call. = FALSE)
  }
  out <- list()
  if (!is.null(spec[["type"]])) out[["type"]] <- spec[["type"]]
  cs <- spec[["concept_set"]]
  if (!is.null(cs)) {
    out[["concept_set"]] <- if (is.list(cs) && !is.null(cs[["concepts"]])) {
      c(list(concepts = as.integer(unlist(cs[["concepts"]], use.names = FALSE))),
        if (!is.null(cs[["include_descendants"]])) list(include_descendants = isTRUE(cs[["include_descendants"]])),
        if (!is.null(cs[["include_mapped"]])) list(include_mapped = isTRUE(cs[["include_mapped"]])),
        if (!is.null(cs[["exclude"]])) list(exclude = as.integer(unlist(cs[["exclude"]], use.names = FALSE))))
    } else {
      cs
    }
  }
  if (!is.null(spec[["value_bin"]])) out[["value_bin"]] <- spec[["value_bin"]]
  if (!is.null(spec[["inclusion_criteria"]])) {
    out[["inclusion_criteria"]] <- spec[["inclusion_criteria"]]
  }
  extra <- setdiff(names(spec), c("type", "concept_set", "value_bin",
                                  "inclusion_criteria"))
  if (length(extra)) out[extra] <- spec[extra]
  out
}

#' Run a v1 inline cohort spec and wrap it as an episode roster
#'
#' @param ctx The execution context (\code{.planContext()}, WS-1), or a CDM
#'   handle. When \code{.cohortCreate()} accepts a \code{ctx} argument (the
#'   private execution path of section 7.3), the context is passed on.
#' @param spec The IR's \code{legacy_spec} object.
#' @param name Population id the roster belongs to (its namespace).
#' @return A roster \code{fragment} (\code{R/types.R}) over the materialised
#'   cohort rows: \code{cohort_row_id, subject_id, person_id,
#'   cohort_start_date, cohort_end_date, index_source, index_key,
#'   index_visit_id, set_id, is_case, sampling_weight, op_start, op_end}
#'   (NULL where the legacy spec has no value), with attribute
#'   \code{temp_table}, the materialised cohort table.
#' @keywords internal
.adaptLegacySpec <- function(ctx, spec, name = "base") {
  handle <- if ((is.environment(ctx) || is.list(ctx)) && !is.null(ctx[["handle"]])) {
    ctx[["handle"]]
  } else {
    ctx
  }
  args <- list(handle, .legacySpecCohortSpec(spec), mode = "temporary")
  if ("ctx" %in% names(formals(.cohortCreate)) && !identical(ctx, handle)) {
    args[["ctx"]] <- ctx
  }
  table <- do.call(.cohortCreate, args)
  table <- .validateIdentifier(table, "legacy spec cohort table")
  select_body <- paste0(
    "SELECT ROW_NUMBER() OVER (ORDER BY c.subject_id, c.cohort_start_date, ",
    "c.cohort_end_date) AS cohort_row_id, c.subject_id AS subject_id, ",
    "c.subject_id AS person_id, c.cohort_start_date AS cohort_start_date, ",
    "c.cohort_end_date AS cohort_end_date, 'legacy_spec' AS index_source, ",
    "CAST(NULL AS VARCHAR(64)) AS index_key, ",
    "CAST(NULL AS BIGINT) AS index_visit_id, CAST(NULL AS BIGINT) AS set_id, ",
    "CAST(NULL AS INTEGER) AS is_case, CAST(NULL AS FLOAT) AS sampling_weight, ",
    "CAST(NULL AS DATE) AS op_start, CAST(NULL AS DATE) AS op_end FROM ",
    table, " c"
  )
  col <- .fragmentColumn
  fragment <- .fragment(
    name = .v1Name(paste0("pop_", name)),
    kind = "roster", grain = "episode", select_body = select_body,
    columns = list(
      cohort_row_id = col("bigint", FALSE, "episode_key"),
      subject_id = col("bigint", FALSE, "origin_identity"),
      person_id = col("bigint", FALSE, "person_key"),
      cohort_start_date = col("date", FALSE, "origin_identity"),
      cohort_end_date = col("date", FALSE, "origin_identity"),
      index_source = col("text", FALSE, "origin_identity"),
      index_key = col("text", TRUE, "origin_identity"),
      index_visit_id = col("bigint", TRUE, "visit"),
      set_id = col("bigint", TRUE, "analytic_key"),
      is_case = col("integer", TRUE, "measure"),
      sampling_weight = col("numeric", TRUE, "measure"),
      op_start = col("date", TRUE, "start"),
      op_end = col("date", TRUE, "end")
    ),
    unique_key = "cohort_row_id",
    person_lineage = list(person_col = "person_id", via_roster = NULL),
    roster_namespace = name,
    source_identity_columns = c("index_source", "subject_id",
                                "cohort_start_date", "cohort_end_date",
                                "index_key"),
    ordering = c("subject_id", "cohort_start_date", "cohort_end_date")
  )
  attr(fragment, "temp_table") <- table
  fragment
}
