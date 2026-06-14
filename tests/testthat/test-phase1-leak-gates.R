# ==============================================================================
# Phase 1 leak-gate tests (server)
#
# Covers three fail-closed fixes:
#   (1) .resolveConceptScopeColumn is the single chokepoint for the profiling
#       `concept_col` override. A *_source_value / *_source_concept_id (blocked
#       or source_concept) or an identifier / person-key column passed as
#       concept_col must be REJECTED on every scope caller (column.stats,
#       value.counts, quantiles, histogram, safe-cutpoints, numeric-range) plus
#       drilldown; crosstab (two axes, no chokepoint) must reject a blocked
#       axis / stratifier. A legitimate domain / unit / type / value concept
#       column must still be ACCEPTED.
#   (2) The OHDSI results path fails-closed on too-few PERSONS: a result whose
#       per-row person basis is < nfilter.subset is dropped even when its
#       record/event/outcome counts are large (orthogonal to cell suppression),
#       and a count-bearing result with NO person basis is rejected in strict
#       mode; >= nfilter rows are returned.
#   (3) The DATASHIELD manifest lists omopNumericRangeDS + omopFactorLevelsDS
#       and its unique method set matches the package NAMESPACE exports.
# ==============================================================================

# --- (1) concept_col scope chokepoint -----------------------------------------

# The shared fixture's `measurement` table carries genuine concept columns
# (measurement_concept_id / unit_concept_id / *_type_concept_id /
# value_as_concept_id) and value_as_number, but NO source columns. Add a
# *_source_value (blocked) and *_source_concept_id (source_concept) so a real
# source column coexists with value_as_number and every value profiler can be
# exercised against it on ONE table.
.handle_with_measurement_source <- function() {
  handle <- create_test_handle()
  DBI::dbExecute(handle$conn,
    "ALTER TABLE measurement ADD COLUMN measurement_source_value TEXT")
  DBI::dbExecute(handle$conn,
    "ALTER TABLE measurement ADD COLUMN measurement_source_concept_id INTEGER")
  DBI::dbExecute(handle$conn,
    "UPDATE measurement SET measurement_source_value = 'SRC', measurement_source_concept_id = 0")
  handle$blueprint <- NULL
  .buildBlueprint(handle)
  handle
}

test_that("resolveConceptScopeColumn rejects leak vectors, accepts true concepts", {
  handle <- .handle_with_measurement_source()
  on.exit(cleanup_handle(handle))
  bp <- .buildBlueprint(handle)

  # Leak vectors that must NEVER be accepted as a concept_col scope override:
  #   *_source_value      -> is_blocked (the source-value filter leak)
  #   *_source_concept_id -> source_concept (and blocked)
  #   person_id           -> person-key identifier (non_concept)
  #   measurement_id      -> table identifier (non_concept)
  leak_cols <- c("measurement_source_value", "measurement_source_concept_id",
                 "person_id", "measurement_id")
  for (cc in leak_cols) {
    expect_error(
      .resolveConceptScopeColumn(bp, "measurement", cc),
      "not a valid scope column",
      info = cc
    )
  }

  # Legitimate concept columns (domain, unit, type, value) are accepted and
  # returned unchanged; NULL falls back to the domain concept column.
  expect_equal(.resolveConceptScopeColumn(bp, "measurement", "unit_concept_id"),
               "unit_concept_id")
  expect_equal(
    .resolveConceptScopeColumn(bp, "measurement", "measurement_type_concept_id"),
    "measurement_type_concept_id")
  expect_equal(
    .resolveConceptScopeColumn(bp, "measurement", "value_as_concept_id"),
    "value_as_concept_id")
  expect_equal(.resolveConceptScopeColumn(bp, "measurement", NULL),
               "measurement_concept_id")
})

test_that("every scope caller rejects a *_source_value concept_col override", {
  handle <- .handle_with_measurement_source()
  on.exit(cleanup_handle(handle))

  leak <- "measurement_source_value"
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.dist = 3,
                           nfilter.levels.max = 40,
                           nfilter.levels.density = 0.5), {
    expect_error(
      .profileColumnStats(handle, "measurement", "value_as_number",
                          concept_id = 3004410L, concept_col = leak),
      "not a valid scope column")
    expect_error(
      .profileValueCounts(handle, "measurement", "unit_concept_id",
                          concept_id = 3004410L, concept_col = leak),
      "not a valid scope column")
    expect_error(
      .profileNumericQuantiles(handle, "measurement", "value_as_number",
                               concept_id = 3004410L, concept_col = leak),
      "not a valid scope column")
    expect_error(
      .profileNumericHistogram(handle, "measurement", "value_as_number",
                               concept_id = 3004410L, concept_col = leak),
      "not a valid scope column")
    expect_error(
      .profileSafeCutpoints(handle, "measurement", "value_as_number",
                            concept_id = 3004410L, concept_col = leak),
      "not a valid scope column")
    expect_error(
      .profileNumericRange(handle, "measurement", "value_as_number",
                           concept_id = 3004410L, concept_col = leak),
      "not a valid scope column")
    # Drilldown was a separate surface that did NOT route through the chokepoint
    # before the fix; it must now reject the same way.
    expect_error(
      .profileConceptDrilldown(handle, "measurement", 3004410L,
                               concept_col = leak),
      "not a valid scope column")
  })
})

test_that("scope callers reject source_concept and identifier concept_col too", {
  handle <- .handle_with_measurement_source()
  on.exit(cleanup_handle(handle))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           nfilter.levels.max = 40,
                           nfilter.levels.density = 0.5), {
    # *_source_concept_id (source concept) -- a value column, but a forbidden one.
    expect_error(
      .profileColumnStats(handle, "measurement", "value_as_number",
                          concept_id = 3004410L,
                          concept_col = "measurement_source_concept_id"),
      "not a valid scope column")
    # person key / identifier columns are non_concept and must never scope.
    expect_error(
      .profileValueCounts(handle, "measurement", "unit_concept_id",
                          concept_id = 3004410L, concept_col = "person_id"),
      "not a valid scope column")
    expect_error(
      .profileColumnStats(handle, "measurement", "value_as_number",
                          concept_id = 3004410L, concept_col = "measurement_id"),
      "not a valid scope column")
  })
})

test_that("a legitimate concept_col scope still works end to end", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.dist = 3,
                           nfilter.levels.max = 40,
                           nfilter.levels.density = 0.5), {
    # unit-aware value stats (scope by unit_concept_id) must succeed.
    stats <- .profileColumnStats(handle, "measurement", "value_as_number",
                                 concept_id = 8554L,
                                 concept_col = "unit_concept_id")
    expect_type(stats, "list")
    expect_true(!is.na(stats$n_total))

    # value-by-type quantiles (scope by *_type_concept_id) must succeed.
    q <- .profileNumericQuantiles(handle, "measurement", "value_as_number",
                                  concept_id = 44818518L,
                                  concept_col = "measurement_type_concept_id")
    expect_s3_class(q, "data.frame")
  })
})

test_that("crosstab rejects a blocked axis / stratifier, accepts a clean one", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           nfilter.levels.max = 40,
                           nfilter.levels.density = 0.5), {
    # value_as_string is blocked: as a crosstab axis its level VALUES would be
    # emitted as raw GROUP BY output -> rejected.
    expect_error(
      .profileCrossTab(handle, "observation", "value_as_string",
                       "observation_concept_id"),
      "blocked")
    # A blocked stratifier is rejected in the same validation loop.
    expect_error(
      .profileCrossTab(handle, "observation", "observation_type_concept_id",
                       "observation_concept_id",
                       stratify_by = "value_as_string"),
      "blocked")
    # A clean axis must NOT trip the blocked-column gate. (Other disclosure
    # gates -- min levels per axis, person floor -- may still apply on this
    # fixture; we only assert it is not rejected as "blocked".)
    err <- tryCatch(
      .profileCrossTab(handle, "person", "gender_concept_id",
                       "race_concept_id"),
      error = function(e) conditionMessage(e))
    if (is.character(err)) expect_false(grepl("blocked", err))
  })
})

# --- (2) OHDSI results person gate --------------------------------------------

test_that("OHDSI person gate drops rows below nfilter persons despite large counts", {
  # incidence_summary (cohort_incidence) person basis = persons_at_risk /
  # person_outcomes. Row A: 6 persons (>= 3) -> kept. Row B: 2 persons (< 3) but
  # 50 outcomes -> the LARGE outcome count survives cell suppression, so only the
  # person gate can drop it. (Column set matches the fixture schema for append.)
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(handle$conn, "DELETE FROM incidence_summary")
  DBI::dbWriteTable(handle$conn, "incidence_summary", data.frame(
    target_cohort_definition_id = c(1L, 1L), outcome_id = c(10L, 20L),
    persons_at_risk = c(6L, 2L), person_days = c(100L, 100L),
    person_outcomes = c(4L, 50L), outcomes = c(4L, 50L),
    incidence_proportion_p100p = c(1, 1), incidence_rate_p100py = c(1, 1)
  ), append = TRUE)
  handle$blueprint <- NULL
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    result <- .ohdsiGetResults(handle, "incidence_summary")
    expect_s3_class(result, "data.frame")
    # The big-outcome / few-person row is gated out...
    expect_false(20L %in% result$outcome_id)
    # ...while the legitimate >= nfilter row is returned.
    expect_true(10L %in% result$outcome_id)
    expect_true(all(result$persons_at_risk >= 3))
  })
})

test_that("OHDSI strict mode rejects a count-bearing result with no person basis", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # incidence_rate (cohort_diagnostics) has cohort_count (a count column) but
  # NONE of the tool's person columns -> a person-less count basis.
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.query_strict = TRUE), {
    strict <- .ohdsiGetResults(handle, "incidence_rate")
    expect_s3_class(strict, "data.frame")
    expect_equal(nrow(strict), 0)  # fail-closed: emptied
  })

  # With strict mode off the same result passes through (cell-suppressed only).
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.query_strict = FALSE), {
    lax <- .ohdsiGetResults(handle, "incidence_rate")
    expect_true(nrow(lax) > 0)
  })
})

test_that("OHDSI person gate returns rows when person basis >= nfilter", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # cohort_count fixture: cohort 1 has cohort_subjects = 85 (>= 3) and must be
  # returned; cohort 2 (subjects = 2) is gated/suppressed out.
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    result <- .ohdsiGetResults(handle, "cohort_count")
    expect_true(nrow(result) > 0)
    expect_true(1L %in% result$cohort_id)
    expect_false(2L %in% result$cohort_id)
    expect_true(all(result$cohort_subjects >= 3))
  })
})

# --- (3) DATASHIELD manifest sync ---------------------------------------------

# Parse a top-level "<Header>:" block (comma/whitespace-separated method names)
# from the DATASHIELD options file, stopping at the next top-level key.
.parse_datashield_section <- function(lines, header) {
  start <- grep(paste0("^", header, ":"), lines)
  if (length(start) == 0) return(character(0))
  rest <- lines[(start + 1):length(lines)]
  endrel <- grep("^[A-Za-z].*:", rest)
  end <- if (length(endrel)) start + min(endrel) - 1L else length(lines)
  body <- lines[start:end]
  body[1] <- sub(paste0("^", header, ":"), "", body[1])
  toks <- unlist(strsplit(paste(body, collapse = " "), "[,[:space:]]+"))
  toks[nzchar(toks)]
}

# The DATASHIELD manifest and NAMESPACE live at the dsOMOP package root. From
# tests/testthat that is two levels up; guard for other working directories.
.phase1_pkg_file <- function(name) {
  cands <- c(
    testthat::test_path("..", "..", name),
    testthat::test_path("..", "..", "..", name),
    name
  )
  for (p in cands) if (file.exists(p)) return(p)
  stop(name, " not found")
}

test_that("DATASHIELD manifest lists the two previously-missing AggregateMethods", {
  lines <- readLines(.phase1_pkg_file("DATASHIELD"))
  agg <- .parse_datashield_section(lines, "AggregateMethods")
  expect_true("omopNumericRangeDS" %in% agg)
  expect_true("omopFactorLevelsDS" %in% agg)
})

test_that("DATASHIELD manifest method set matches NAMESPACE DS exports", {
  lines <- readLines(.phase1_pkg_file("DATASHIELD"))
  manifest <- unique(c(
    .parse_datashield_section(lines, "AggregateMethods"),
    .parse_datashield_section(lines, "AssignMethods")
  ))

  ns <- readLines(.phase1_pkg_file("NAMESPACE"))
  ds_exports <- sub("^export\\((.*)\\)$", "\\1",
                    grep("^export\\(omop.*DS\\)$", ns, value = TRUE))

  # No exported *DS method is missing from the manifest, and the manifest lists
  # no method that is not exported.
  expect_equal(sort(setdiff(ds_exports, manifest)), character(0))
  expect_equal(sort(setdiff(manifest, ds_exports)), character(0))
})
