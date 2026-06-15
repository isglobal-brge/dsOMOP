# ==============================================================================
# Tests for the live CohortDiagnostics concept-set ports:
#   dsomop:cohortdx.resolved_concepts  (PLR-7) + ohdsi twin resolved_concepts
#   dsomop:cohortdx.orphan_concepts    (PLR-8) + ohdsi twin orphan_concept
#
# resolved_concepts is a VOCABULARY-ONLY metadata port (live concept-set
# expansion; count-less, routed through .ohdsiPersonGate's pass-through, capped at
# nfilter.levels.max). orphan_concepts is a genuine per-patient aggregate (the CDM
# presence of candidate concept-set GAPS; record-unit, gated through the GENERIC
# record branch so n_records/n_persons are banded and small candidates dropped).
# Both replace a read-precomputed cd_* table. This file proves, on the SQLite
# fixture:
#   (a) both canonical ids + their ohdsi twins resolve with the right adapter;
#   (b) resolved_concepts expands a concept set to METADATA (no counts) and the
#       enumeration cap fires fail-closed;
#   (c) orphan_concepts RUNS and the gate BANDS the per-candidate counts;
#   (d) orphan_concepts fails closed on a sub-threshold scope; both return a
#       gate-safe empty frame with no anchor/seed.
# ==============================================================================

cdx_handle <- function(n_persons = 30) {
  h <- create_test_handle(n_persons = n_persons)
  h$person_key <- as.raw(1:16)
  .buildBlueprint(h)
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

cdx_opts <- function() {
  list(nfilter.subset = 3, nfilter.tab = 3,
       dsomop.nfilter.band = 5, nfilter.levels.max = 100)
}

# ---------------------------------------------------------------------------- #
# (a) Discovery.
# ---------------------------------------------------------------------------- #

test_that("(a) resolved/orphan canonical ids + ohdsi twins resolve live", {
  h <- cdx_handle()
  on.exit(cleanup_handle(h))

  rc <- .omopAnalysisResolve(h, "dsomop:cohortdx.resolved_concepts")
  expect_equal(rc$meta$adapter, "ohdsi")          # count-less metadata pass-through
  expect_length(rc$disclosure$count_cols, 0L)
  rc_twin <- .omopAnalysisResolve(
    h, "dsomop:ohdsi.cohort_diagnostics.resolved_concepts")
  expect_equal(rc_twin$meta$adapter, "ohdsi")

  oc <- .omopAnalysisResolve(h, "dsomop:cohortdx.orphan_concepts")
  expect_equal(oc$meta$adapter, "ohdsi_live")     # genuine per-patient aggregate
  expect_equal(oc$disclosure$unit, "record")
  expect_equal(oc$disclosure$person_id_col, "n_persons")
  expect_setequal(oc$disclosure$count_cols, c("n_records", "n_persons"))
  oc_twin <- .omopAnalysisResolve(
    h, "dsomop:ohdsi.cohort_diagnostics.orphan_concept")
  expect_equal(oc_twin$meta$adapter, "ohdsi_live")
})

# ---------------------------------------------------------------------------- #
# (b) resolved_concepts: live expansion to metadata; enumeration cap fires.
# ---------------------------------------------------------------------------- #

test_that("(b) resolved_concepts expands a concept set to vocabulary metadata", {
  h <- cdx_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(cdx_opts(), {
    # Anchor 4000001 (Respiratory disease) -> {4000001, 255573 COPD, 317009 Asthma}
    # via concept_ancestor in the fixture.
    rc <- .omopAnalysisRun(h, "dsomop:cohortdx.resolved_concepts",
                           params = list(concepts = "4000001",
                                         include_descendants = "1"))
    expect_true(all(c("concept_id", "concept_name", "domain_id", "vocabulary_id",
                      "standard_concept", "is_excluded") %in% names(rc)))
    expect_true(all(c(4000001L, 255573L, 317009L) %in% as.integer(rc$concept_id)))
    # Metadata only: EXACTLY the vocabulary columns, no count/person column.
    expect_setequal(names(rc),
                    c("concept_id", "concept_name", "domain_id", "vocabulary_id",
                      "standard_concept", "is_excluded"))
    expect_false(any(grepl("_source_value$", names(rc))))
    # The ohdsi twin id computes the same metadata.
    rc2 <- .omopAnalysisRun(h, "dsomop:ohdsi.cohort_diagnostics.resolved_concepts",
                            params = list(concepts = "4000001",
                                          include_descendants = "1"))
    expect_equal(rc, rc2)
  })
})

test_that("(b) resolved_concepts caps the enumeration at nfilter.levels.max", {
  h <- cdx_handle()
  on.exit(cleanup_handle(h))
  # A 3-concept resolution must fail closed when the cap is 1.
  withr::with_options(list(nfilter.levels.max = 1), {
    expect_error(
      .omopAnalysisRun(h, "dsomop:cohortdx.resolved_concepts",
                       params = list(concepts = "4000001",
                                     include_descendants = "1")),
      "exceeds nfilter.levels.max")
  })
})

test_that("(b) resolved_concepts returns an empty frame with no anchor", {
  h <- cdx_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(cdx_opts(), {
    rc <- .omopAnalysisRun(h, "dsomop:cohortdx.resolved_concepts", params = list())
    expect_equal(nrow(rc), 0L)
  })
})

# ---------------------------------------------------------------------------- #
# (c) orphan_concepts: runs; the gate bands the per-candidate counts.
# ---------------------------------------------------------------------------- #

test_that("(c) orphan_concepts surfaces uncovered-but-present concepts, banded", {
  h <- cdx_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(cdx_opts(), {
    # Seed 4000000 with descendants EXCLUDED from the resolved set, so its
    # descendant 201820 (Diabetes; present in the CDM for 6 persons) surfaces as a
    # candidate orphan. The gate bands 6 -> 5.
    oc <- .omopAnalysisRun(h, "dsomop:cohortdx.orphan_concepts",
                           params = list(seed_concept_id = "4000000",
                                         domain_code = "0",
                                         include_descendants = "0"))
    expect_true(nrow(oc) >= 1L)
    expect_true(all(c("concept_id", "concept_name", "n_persons", "n_records") %in%
                    names(oc)))
    expect_true(201820L %in% as.integer(oc$concept_id))
    # The gate fires: every released count is banded (floor to nfilter_band) or NA.
    expect_true(all(oc$n_persons %% 5 == 0 | is.na(oc$n_persons)))
    expect_true(all(oc$n_records %% 5 == 0 | is.na(oc$n_records)))
    # The raw 6-person count is reported as the banded 5 (never the exact 6).
    dm <- oc[as.integer(oc$concept_id) == 201820L, ]
    expect_equal(dm$n_persons, 5)
    expect_false(any(grepl("_source_value$", names(oc))))
    # The ohdsi twin id computes the same gated result.
    oc2 <- .omopAnalysisRun(h, "dsomop:ohdsi.cohort_diagnostics.orphan_concept",
                            params = list(seed_concept_id = "4000000",
                                          domain_code = "0",
                                          include_descendants = "0"))
    expect_equal(oc, oc2)
  })
})

# ---------------------------------------------------------------------------- #
# (d) Disclosure: orphan_concepts fails closed on a sub-threshold scope.
# ---------------------------------------------------------------------------- #

test_that("(d) orphan_concepts fails closed on a sub-threshold scope", {
  h <- cdx_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(cdx_opts(), {
    DBI::dbExecute(h$conn, paste0(
      "CREATE TEMP TABLE cdx_tiny AS SELECT person_id AS subject_id ",
      "FROM person WHERE person_id IN (1, 2)"))
    expect_error(
      .omopAnalysisRun(h, "dsomop:cohortdx.orphan_concepts",
                       params = list(seed_concept_id = "4000000",
                                     domain_code = "0",
                                     include_descendants = "0"),
                       scope = "cdx_tiny"),
      "insufficient individuals|disclosure threshold|blocked")
  })
})

test_that("(d) orphan_concepts returns an empty frame with no seed", {
  h <- cdx_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(cdx_opts(), {
    oc <- .omopAnalysisRun(h, "dsomop:cohortdx.orphan_concepts", params = list())
    expect_equal(nrow(oc), 0L)
  })
})
