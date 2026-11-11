# WS-0 (design-2.10.3 sections 6.4 and 7.3): typed compiler and publication records.

.ws0_roster_fragment <- function() {
  dsOMOP:::.fragment(
    name = "pop_base", kind = "roster", grain = "episode",
    select_body = "SELECT subject_id AS person_id, cohort_row_id, index_source, index_key, cohort_start_date FROM cand_base",
    columns = list(
      person_id = dsOMOP:::.fragmentColumn("bigint", FALSE, "person_key"),
      cohort_row_id = dsOMOP:::.fragmentColumn("integer", FALSE, "episode_key"),
      index_source = dsOMOP:::.fragmentColumn("text", FALSE, "origin_identity"),
      index_key = dsOMOP:::.fragmentColumn("text", TRUE, "origin_identity"),
      cohort_start_date = dsOMOP:::.fragmentColumn("date", FALSE, "start"),
      cand_key = dsOMOP:::.fragmentColumn("integer", FALSE, "private")),
    dependencies = "cand_base", unique_key = c("person_id", "cohort_row_id"),
    person_lineage = list(person_col = "person_id", via_roster = NULL),
    roster_namespace = "base",
    source_identity_columns = c("index_source", "index_key"),
    private_columns = "cand_key",
    ordering = c("person_id", "cohort_row_id"))
}

test_that("a fragment validates and rejects WITH, unknown roles and bad lineage", {
  f <- .ws0_roster_fragment()
  expect_s3_class(f, "dsomop_fragment")
  expect_error(dsOMOP:::.fragmentColumn("blob", FALSE, "measure"), "column type must be one of")
  expect_error(dsOMOP:::.fragmentColumn("text", FALSE, "secret"), "column role must be one of")
  bad <- f; bad$select_body <- "WITH x AS (SELECT 1) SELECT * FROM x"
  expect_error(dsOMOP:::.assertFragment(bad), "must not start with WITH")
  bad <- f; bad$unique_key <- "nope"
  expect_error(dsOMOP:::.assertFragment(bad), "unique_key must name existing columns")
  bad <- f; bad$person_lineage$person_col <- "cohort_row_id"
  expect_error(dsOMOP:::.assertFragment(bad), "person_key role")
  bad <- f; bad$private_columns <- character(0)
  expect_error(dsOMOP:::.assertFragment(bad), "private role must be listed")
  bad <- f; bad$grain <- "none"
  expect_error(dsOMOP:::.assertFragment(bad), "reference fragment")
  ref <- dsOMOP:::.fragment("cs_t2dm", "concept", "none", "SELECT 201826 AS concept_id",
                            columns = list(concept_id = dsOMOP:::.fragmentColumn("integer", FALSE, "concept")))
  expect_identical(ref$grain, "none")
})

test_that("publication candidates enforce one-person lineage", {
  L <- dsOMOP:::.lineage
  ok_direct <- dsOMOP:::.publicationCandidate("baseline", "baseline", "frame",
                                             L("direct", "person_id"), requested_symbol = "D_baseline")
  expect_s3_class(ok_direct, "dsomop_publication_candidate")
  ok_roster <- dsOMOP:::.publicationCandidate("tte", "itt", "staged",
    L("roster", c("person_id", "cohort_row_id"), "new_users", c("person_id", "cohort_row_id"), "person_id"),
    requested_path = "tte/itt.parquet")
  expect_identical(ok_roster$lineage$roster_id, "new_users")
  ok_ref <- dsOMOP:::.publicationCandidate("panel", "covariateRef", "frame", L("reference"),
                                           requested_symbol = "D_panel.covariateRef", grain = "none")
  expect_identical(ok_ref$grain, "none")
  expect_error(dsOMOP:::.publicationCandidate("cc", "cc", "frame", L("direct", "set_id"), requested_symbol = "D_cc"),
               "set_id is never person lineage")
  expect_error(dsOMOP:::.publicationCandidate("cc", "cc", "frame",
    L("roster", "set_id", "cc", "set_id", "person_id"), requested_symbol = "D_cc"),
    "set_id is never person lineage")
  expect_error(dsOMOP:::.publicationCandidate("cc", "cc", "frame",
    L("roster", "cohort_row_id", "cc", "cohort_row_id", "person_id"), requested_symbol = "D_cc"),
    "roster lineage keys must be")
  expect_error(dsOMOP:::.publicationCandidate("b", "b", "frame", L("direct", "person_id", roster_id = "x"),
                                              requested_symbol = "D_b"), "carries no roster mapping")
  expect_error(dsOMOP:::.publicationCandidate("b", "b", "frame", L("reference", "person_id"),
                                              requested_symbol = "D_b", grain = "none"), "carries no keys")
  expect_error(dsOMOP:::.publicationCandidate("b", "b", "frame", L("reference"), requested_symbol = "D_b"),
               "grain none")
  expect_error(dsOMOP:::.publicationCandidate("b", "b", "staged", L("direct", "person_id"),
                                              requested_path = "../escape.parquet"), "must not climb")
  expect_error(dsOMOP:::.publicationCandidate("b", "b", "frame", L("direct", "person_id"),
                                              requested_path = "x"), "exactly a symbol")
})

test_that("a compiled output fails closed on an undeclared patient component", {
  f <- .ws0_roster_fragment()
  L <- dsOMOP:::.lineage
  comp <- list(statement = "SELECT * FROM pop_base", fragment = f)
  ref_f <- dsOMOP:::.fragment("ref_x", "result", "none", "SELECT 1 AS covariate_id",
                              columns = list(covariate_id = dsOMOP:::.fragmentColumn("integer", FALSE, "measure")))
  co <- dsOMOP:::.compiledOutput("baseline",
    components = list(baseline = comp, covariateRef = list(statement = "SELECT 1", fragment = ref_f)),
    publication_candidates = list(
      dsOMOP:::.publicationCandidate("baseline", "baseline", "frame", L("direct", "person_id"), requested_symbol = "D_baseline"),
      dsOMOP:::.publicationCandidate("baseline", "covariateRef", "frame", L("reference"), requested_symbol = "D_ref", grain = "none")),
    reference_components = "covariateRef")
  expect_s3_class(co, "dsomop_compiled_output")
  expect_error(dsOMOP:::.compiledOutput("baseline", components = list(baseline = comp),
                                        publication_candidates = list()),
               "without a publication candidate")
  expect_error(dsOMOP:::.compiledOutput("baseline", components = list(baseline = comp),
    publication_candidates = list(dsOMOP:::.publicationCandidate("other", "baseline", "frame",
      L("direct", "person_id"), requested_symbol = "D"))), "does not belong here")
  expect_error(dsOMOP:::.compiledOutput("baseline", components = list(baseline = comp),
    publication_candidates = list(dsOMOP:::.publicationCandidate("baseline", "baseline", "frame",
      L("reference"), requested_symbol = "D", grain = "none"))), "not a reference component")
  expect_error(dsOMOP:::.compiledOutput("baseline",
    components = list(baseline = list(statement = "SELECT 1", frame = data.frame(a = 1), fragment = f)),
    publication_candidates = list()), "exactly one of statement or frame")
})
