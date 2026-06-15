# ==============================================================================
# Tests for the evidence-synthesis per-site delegate ports
# (ohdsi_pack_evidence_synthesis.R). The four cross-database es_* result-table ids
# no longer READ a precomputed table: a single site cannot compute a POOLED
# cross-site estimate, so each id now delegates LIVE to the per-site PLR port
# whose disclosure-safe log-estimate + SE the CLIENT meta-analyzes
# (dsOMOPClient::ds.omop.meta.effect_estimate). This file proves, on the SQLite
# fixture:
#   (a) the four es_* ids overlay LIVE (adapter ohdsi_live, no precomputed flag)
#       onto the correct per-site canonical (alias_target);
#   (b) es_cm_result produces the SAME per-site estimate as cm.effect_estimate;
#   (c) es_sccs_result produces the SAME per-site IRR as
#       sccs.incidence_rate_ratio;
#   (d) the per-site estimate is gated (banded counts) and carries the meta-
#       analysis sufficient statistics (log-estimate + SE) the client pools.
# ==============================================================================

es_handle <- function(n_persons = 40) {
  h <- create_test_handle(n_persons = n_persons)
  h$person_key <- as.raw(1:16)
  .buildBlueprint(h)
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

es_opts <- function() {
  list(nfilter.subset = 3, nfilter.tab = 3,
       dsomop.nfilter.dist = 10, dsomop.nfilter.band = 5)
}

# Reuse the CohortMethod two-arm setup (target outcome elevated) for es_cm_result.
es_setup_cm_arms <- function(h) {
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE es_target AS SELECT person_id AS subject_id, ",
    "'2020-01-01' AS cohort_start_date, '2023-12-31' AS cohort_end_date ",
    "FROM person WHERE person_id BETWEEN 1 AND 20"))
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE es_comparator AS SELECT person_id AS subject_id, ",
    "'2020-01-01' AS cohort_start_date, '2023-12-31' AS cohort_end_date ",
    "FROM person WHERE person_id BETWEEN 21 AND 40"))
  DBI::dbExecute(h$conn,
    "DELETE FROM condition_occurrence WHERE condition_concept_id = 4000002")
  cid <- 8400L
  add_out <- function(pid, dt) {
    cid <<- cid + 1L
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
      "condition_concept_id, condition_start_date, condition_end_date, ",
      "condition_type_concept_id) VALUES (%d, %d, 4000002, '%s', '%s', ",
      "44818518)"), cid, pid, dt, dt))
  }
  for (pid in 1:12)   add_out(pid, "2020-02-15")
  for (pid in 21:25) add_out(pid, "2020-04-15")
  invisible(NULL)
}

# SCCS exposed-window scenario for es_sccs_result (mirrors the IRR port test).
es_setup_sccs <- function(h, cases = 1:20) {
  DBI::dbExecute(h$conn, "DELETE FROM drug_exposure")
  did <- 5300L
  for (pid in cases) {
    did <- did + 1L
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO drug_exposure (drug_exposure_id, person_id, drug_concept_id, ",
      "drug_exposure_start_date, drug_exposure_end_date, drug_type_concept_id) ",
      "VALUES (%d, %d, 7777, '2021-01-01', '2021-01-31', 38000177)"), did, pid))
  }
  DBI::dbExecute(h$conn,
    "DELETE FROM condition_occurrence WHERE condition_concept_id = 4000002")
  cid <- 9300L
  addc <- function(pid, dt) {
    cid <<- cid + 1L
    DBI::dbExecute(h$conn, sprintf(paste0(
      "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
      "condition_concept_id, condition_start_date, condition_end_date, ",
      "condition_type_concept_id) VALUES (%d, %d, 4000002, '%s', '%s', ",
      "44818518)"), cid, pid, dt, dt))
  }
  for (pid in cases) { addc(pid, "2021-01-10"); addc(pid, "2021-01-20") }
  for (pid in cases) addc(pid, "2022-06-01")
  invisible(NULL)
}

# ---------------------------------------------------------------------------- #
# (a) The four es_* ids overlay LIVE onto the correct per-site canonical.
# ---------------------------------------------------------------------------- #

test_that("(a) es_* ids are live per-site delegates (no precomputed read)", {
  h <- es_handle()
  on.exit(cleanup_handle(h))
  expected <- list(
    "dsomop:ohdsi.evidence_synthesis.es_cm_result" = "dsomop:cm.effect_estimate",
    "dsomop:ohdsi.evidence_synthesis.es_sccs_result" =
      "dsomop:sccs.incidence_rate_ratio",
    "dsomop:ohdsi.evidence_synthesis.es_cm_diagnostics_summary" =
      "dsomop:cm.diagnostics_summary",
    "dsomop:ohdsi.evidence_synthesis.es_sccs_diagnostics_summary" =
      "dsomop:sccs.assumption_checks")
  for (id in names(expected)) {
    e <- .omopAnalysisResolve(h, id)
    expect_equal(e$meta$adapter, "ohdsi_live")
    expect_null(e$meta$precomputed)
    expect_equal(e$meta$alias_target, expected[[id]])
    expect_match(e$description, "PER-SITE estimate")
  }
})

# ---------------------------------------------------------------------------- #
# (b) es_cm_result == the per-site cm.effect_estimate.
# ---------------------------------------------------------------------------- #

test_that("(b) es_cm_result computes the per-site cm.effect_estimate", {
  h <- es_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(es_opts(), {
    es_setup_cm_arms(h)
    p <- list(outcome_concept_id = "4000002", tar_start_offset = "0")
    es <- .omopAnalysisRun(h, "dsomop:ohdsi.evidence_synthesis.es_cm_result",
                           params = p, scope = list("es_target", "es_comparator"))
    canon <- .omopAnalysisRun(h, "dsomop:cm.effect_estimate", params = p,
                              scope = list("es_target", "es_comparator"))
    expect_equal(es, canon)
    # Carries the meta-analysis sufficient statistics the client pools.
    expect_true(all(c("log_estimate", "se_log_estimate") %in% names(es)))
    # Counts are banded.
    expect_true(all(es$outcomes %% 5 == 0 | is.na(es$outcomes)))
  })
})

# ---------------------------------------------------------------------------- #
# (c) es_sccs_result == the per-site sccs.incidence_rate_ratio.
# ---------------------------------------------------------------------------- #

test_that("(c) es_sccs_result computes the per-site sccs IRR", {
  h <- es_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(es_opts(), {
    es_setup_sccs(h)
    p <- list(exposure_concept_id = "7777", exposure_domain_code = "1",
              outcome_concept_id = "4000002", outcome_domain_code = "0",
              window = "30")
    es <- .omopAnalysisRun(h, "dsomop:ohdsi.evidence_synthesis.es_sccs_result",
                           params = p)
    canon <- .omopAnalysisRun(h, "dsomop:sccs.incidence_rate_ratio", params = p)
    expect_equal(es, canon)
    expect_true(all(c("log_irr", "se_log_irr") %in% names(es)))
  })
})

# ---------------------------------------------------------------------------- #
# (d) Un-scoped es_cm_result returns a gate-safe empty frame (no precomputed row).
# ---------------------------------------------------------------------------- #

test_that("(d) es_cm_result un-scoped is gate-safe empty, never a precomputed row", {
  h <- es_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(es_opts(), {
    es_setup_cm_arms(h)
    d0 <- .omopAnalysisRun(h, "dsomop:ohdsi.evidence_synthesis.es_cm_result",
                           params = list(outcome_concept_id = "4000002"))
    expect_equal(nrow(d0), 0L)
  })
})
