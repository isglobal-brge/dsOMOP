# WS-0 (design-2.10.3 sections 6.4 and 7.3; review B4): typed compiler and publication records.

.ws0_col <- dsOMOP:::.fragmentColumn
.ws0_roster_fragment <- function() {
  dsOMOP:::.fragment(
    name = "pop_base", kind = "roster", grain = "episode",
    select_body = "SELECT subject_id AS person_id, cohort_row_id, index_source, index_key, cohort_start_date FROM cand_base",
    columns = list(person_id = .ws0_col("bigint", FALSE, "person_key"), cohort_row_id = .ws0_col("integer", FALSE, "episode_key"),
                   index_source = .ws0_col("text", FALSE, "origin_identity"), index_key = .ws0_col("text", TRUE, "origin_identity"),
                   cohort_start_date = .ws0_col("date", FALSE, "start"), cand_key = .ws0_col("integer", FALSE, "private")),
    dependencies = "cand_base", unique_key = c("person_id", "cohort_row_id"),
    person_lineage = list(person_col = "person_id", via_roster = NULL), roster_namespace = "base",
    source_identity_columns = c("index_source", "index_key"), private_columns = "cand_key",
    ordering = c("person_id", "cohort_row_id"))
}
.ws0_ref_fragment <- function(name = "ref_x") {
  dsOMOP:::.fragment(name, "result", "none", "SELECT 1 AS covariate_id",
                     columns = list(covariate_id = .ws0_col("integer", FALSE, "measure")))
}

test_that("a fragment validates and rejects WITH, unknown roles and bad lineage", {
  f <- .ws0_roster_fragment()
  expect_s3_class(f, "dsomop_fragment")
  expect_error(.ws0_col("blob", FALSE, "measure"), "column type must be one of")
  expect_error(.ws0_col("text", FALSE, "secret"), "column role must be one of")
  expect_error(.ws0_col("text", "not logical", "measure"), "nullable must be TRUE or FALSE")
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
  # nested payload mutated after construction: the class tag alone is never trusted (B4)
  bad <- f; bad$columns$person_id$type <- "blob"
  expect_error(dsOMOP:::.assertFragment(bad), "column type must be one of")
  bad <- f; bad$columns$person_id$nullable <- "not logical"
  expect_error(dsOMOP:::.assertFragment(bad), "nullable must be TRUE or FALSE")
  bad <- f; bad$columns$person_id$role <- "secret"
  expect_error(dsOMOP:::.assertFragment(bad), "column role must be one of")
  bad <- f; bad$columns$person_id$extra <- TRUE
  expect_error(dsOMOP:::.assertFragment(bad), "must be built with .fragmentColumn")
  # a person_key column without declared lineage is refused
  bad <- f; bad$kind <- "result"; bad$person_lineage <- list(person_col = NULL, via_roster = NULL)
  expect_error(dsOMOP:::.assertFragment(bad), "person_key column must declare its person lineage")
  expect_identical(.ws0_ref_fragment()$grain, "none")
  expect_error(dsOMOP:::.fragment("ref_y", "result", "none", "SELECT 1",
                                  columns = list(person_id = .ws0_col("bigint", FALSE, "person_key"))),
               "cannot carry person or episode keys")
})

test_that("publication candidates enforce one-person lineage", {
  L <- dsOMOP:::.lineage
  ok_direct <- dsOMOP:::.publicationCandidate("baseline", "baseline", "frame", L("direct", "person_id"), requested_symbol = "D_baseline")
  expect_s3_class(ok_direct, "dsomop_publication_candidate")
  ok_roster <- dsOMOP:::.publicationCandidate("tte", "itt", "staged",
    L("roster", c("person_id", "cohort_row_id"), "new_users", c("person_id", "cohort_row_id"), "person_id"), requested_path = "tte/itt.parquet")
  expect_identical(ok_roster$lineage$roster_id, "new_users")
  ok_ref <- dsOMOP:::.publicationCandidate("panel", "covariateRef", "frame", L("reference"), requested_symbol = "D_panel.covariateRef", grain = "none")
  expect_identical(ok_ref$grain, "none")
  expect_error(dsOMOP:::.publicationCandidate("cc", "cc", "frame", L("direct", "set_id"), requested_symbol = "D_cc"), "set_id is never person lineage")
  expect_error(dsOMOP:::.publicationCandidate("cc", "cc", "frame", L("roster", "set_id", "cc", "set_id", "person_id"), requested_symbol = "D_cc"), "set_id is never person lineage")
  expect_error(dsOMOP:::.publicationCandidate("cc", "cc", "frame", L("roster", "cohort_row_id", "cc", "cohort_row_id", "person_id"), requested_symbol = "D_cc"), "roster lineage keys must be")
  expect_error(dsOMOP:::.publicationCandidate("b", "b", "frame", L("direct", "person_id", roster_id = "x"), requested_symbol = "D_b"), "carries no roster mapping")
  expect_error(dsOMOP:::.publicationCandidate("b", "b", "frame", L("reference", "person_id"), requested_symbol = "D_b", grain = "none"), "carries no keys")
  expect_error(dsOMOP:::.publicationCandidate("b", "b", "frame", L("reference"), requested_symbol = "D_b"), "grain none")
  expect_error(dsOMOP:::.publicationCandidate("b", "b", "staged", L("direct", "person_id"), requested_path = "../escape.parquet"), "must not climb")
  expect_error(dsOMOP:::.publicationCandidate("b", "b", "frame", L("direct", "person_id"), requested_path = "x"), "exactly a symbol")
})

test_that("a compiled output cross-checks every candidate against its fragment (B4)", {
  f <- .ws0_roster_fragment(); L <- dsOMOP:::.lineage
  comp <- list(statement = "SELECT * FROM pop_base", fragment = f)
  ref_f <- .ws0_ref_fragment()
  cand_ok <- dsOMOP:::.publicationCandidate("baseline", "baseline", "frame", L("direct", "person_id"),
                                            private_columns = "cand_key", requested_symbol = "D_baseline")
  co <- dsOMOP:::.compiledOutput("baseline",
    components = list(baseline = comp, covariateRef = list(statement = "SELECT 1", fragment = ref_f)),
    publication_candidates = list(cand_ok,
      dsOMOP:::.publicationCandidate("baseline", "covariateRef", "frame", L("reference"), requested_symbol = "D_ref", grain = "none")),
    reference_components = "covariateRef")
  expect_s3_class(co, "dsomop_compiled_output")
  # a patient fragment declared as a reference component (no candidate) is refused
  expect_error(dsOMOP:::.compiledOutput("baseline", components = list(patient_rows = comp), publication_candidates = list(),
                                        reference_components = "patient_rows"), "carries person lineage or keys")
  # a reference-mode candidate on a patient fragment is refused
  expect_error(dsOMOP:::.compiledOutput("baseline", components = list(baseline = comp),
    publication_candidates = list(dsOMOP:::.publicationCandidate("baseline", "baseline", "frame", L("reference"), requested_symbol = "D", grain = "none"))),
    "not a reference component")
  # a direct candidate naming subject_id when the fragment only has person_id is refused
  expect_error(dsOMOP:::.compiledOutput("baseline", components = list(baseline = comp),
    publication_candidates = list(dsOMOP:::.publicationCandidate("baseline", "baseline", "frame", L("direct", "subject_id"),
                                                                  private_columns = "cand_key", requested_symbol = "D"))),
    "do not exist in the fragment")
  # the fragment's private columns must be dropped by the candidate
  expect_error(dsOMOP:::.compiledOutput("baseline", components = list(baseline = comp),
    publication_candidates = list(dsOMOP:::.publicationCandidate("baseline", "baseline", "frame", L("direct", "person_id"), requested_symbol = "D"))),
    "private columns must all be dropped")
  # grain disagreement and wrong roster namespace are refused
  expect_error(dsOMOP:::.compiledOutput("baseline", components = list(baseline = comp),
    publication_candidates = list(dsOMOP:::.publicationCandidate("baseline", "baseline", "frame", L("direct", "person_id"),
                                                                  private_columns = "cand_key", requested_symbol = "D", grain = "person"))),
    "disagrees with fragment grain")
  expect_error(dsOMOP:::.compiledOutput("baseline", components = list(baseline = comp),
    publication_candidates = list(dsOMOP:::.publicationCandidate("baseline", "baseline", "frame",
      L("roster", c("person_id", "cohort_row_id"), "other", c("person_id", "cohort_row_id"), "person_id"),
      private_columns = "cand_key", requested_symbol = "D"))), "names namespace 'other'")
  # a component without a candidate, a candidate for another output, two statements
  expect_error(dsOMOP:::.compiledOutput("baseline", components = list(baseline = comp), publication_candidates = list()),
               "without a publication candidate")
  expect_error(dsOMOP:::.compiledOutput("baseline", components = list(baseline = comp),
    publication_candidates = list(dsOMOP:::.publicationCandidate("other", "baseline", "frame", L("direct", "person_id"), requested_symbol = "D"))),
    "does not belong here")
  expect_error(dsOMOP:::.compiledOutput("baseline",
    components = list(baseline = list(statement = "SELECT 1", frame = data.frame(a = 1), fragment = f)),
    publication_candidates = list()), "exactly one of statement or frame")
  # a mutated nested column inside a component is caught through the output validator too
  bad <- co; bad$components$baseline$fragment$columns$person_id$type <- "blob"
  expect_error(dsOMOP:::.assertCompiledOutput(bad), "column type must be one of")
})

test_that("roster-mode candidates must agree with the fragment's person lineage and key roles (re-review B4)", {
  L <- dsOMOP:::.lineage
  roster_cand <- function(keys = c("person_id", "cohort_row_id")) {
    dsOMOP:::.publicationCandidate("wide", "wide", "frame", L("roster", keys, "base", keys, "person_id"), requested_symbol = "D_wide")
  }
  # (a) a result fragment whose person_id is a plain measure and that declares no person lineage
  f_measure <- dsOMOP:::.fragment("res_a", "result", "episode", "SELECT person_id, cohort_row_id, x FROM pop_base",
    columns = list(person_id = .ws0_col("bigint", FALSE, "measure"), cohort_row_id = .ws0_col("integer", FALSE, "episode_key"),
                   x = .ws0_col("numeric", TRUE, "measure")), roster_namespace = "base")
  expect_error(dsOMOP:::.compiledOutput("wide", components = list(wide = list(statement = "SELECT 1", fragment = f_measure)),
                                        publication_candidates = list(roster_cand())), "declares no person lineage")
  # (b) a fragment with direct lineage through subject_id published through person_id
  f_subject <- dsOMOP:::.fragment("res_b", "result", "episode", "SELECT subject_id, person_id, cohort_row_id FROM pop_base",
    columns = list(subject_id = .ws0_col("bigint", FALSE, "person_key"), person_id = .ws0_col("bigint", FALSE, "person_key"),
                   cohort_row_id = .ws0_col("integer", FALSE, "episode_key")),
    person_lineage = list(person_col = "subject_id", via_roster = NULL), roster_namespace = "base")
  expect_error(dsOMOP:::.compiledOutput("wide", components = list(wide = list(statement = "SELECT 1", fragment = f_subject)),
                                        publication_candidates = list(roster_cand())), "declares person lineage through 'subject_id'")
  # (c) the roster FRAGMENT name (via_roster) and the roster NAMESPACE (roster_id) are distinct: a fragment
  # mediated by the roster fragment 'pop_base' publishing through namespace 'base' is legitimate (re-review 2)
  f_pop_rowid <- dsOMOP:::.fragment("res_sparse", "result", "episode", "SELECT rowId, covariate_id FROM pop_base_sparse",
    columns = list(rowId = .ws0_col("integer", FALSE, "episode_key"), covariate_id = .ws0_col("integer", FALSE, "measure")),
    dependencies = "pop_base", person_lineage = list(person_col = NULL, via_roster = "pop_base"), roster_namespace = "base")
  expect_s3_class(dsOMOP:::.compiledOutput("sparse", components = list(sparse = list(statement = "SELECT * FROM res_sparse", fragment = f_pop_rowid)),
    publication_candidates = list(dsOMOP:::.publicationCandidate("sparse", "sparse", "frame",
      L("roster", "rowId", "base", "cohort_row_id", "person_id"), requested_symbol = "D_sparse"))), "dsomop_compiled_output")
  f_pop_keys <- dsOMOP:::.fragment("res_wide", "result", "episode", "SELECT person_id, cohort_row_id, x FROM pop_base",
    columns = list(person_id = .ws0_col("bigint", FALSE, "person_key"), cohort_row_id = .ws0_col("integer", FALSE, "episode_key"),
                   x = .ws0_col("numeric", TRUE, "measure")),
    dependencies = "pop_base", person_lineage = list(person_col = NULL, via_roster = "pop_base"), roster_namespace = "base")
  expect_s3_class(dsOMOP:::.compiledOutput("wide", components = list(wide = list(statement = "SELECT * FROM res_wide", fragment = f_pop_keys)),
                                           publication_candidates = list(roster_cand())), "dsomop_compiled_output")
  # the namespace itself must still agree
  f_other_ns <- dsOMOP:::.fragment("res_c", "result", "episode", "SELECT person_id, cohort_row_id FROM pop_other",
    columns = list(person_id = .ws0_col("bigint", FALSE, "person_key"), cohort_row_id = .ws0_col("integer", FALSE, "episode_key")),
    person_lineage = list(person_col = NULL, via_roster = "pop_other"), roster_namespace = "other")
  expect_error(dsOMOP:::.compiledOutput("wide", components = list(wide = list(statement = "SELECT 1", fragment = f_other_ns)),
                                        publication_candidates = list(roster_cand())), "names namespace 'base' but the fragment uses 'other'")
  # legitimate roster-mediated fragments: direct person column, and rowId mediated by the roster
  f_ok <- dsOMOP:::.fragment("res_d", "result", "episode", "SELECT person_id, cohort_row_id, x FROM pop_base",
    columns = list(person_id = .ws0_col("bigint", FALSE, "person_key"), cohort_row_id = .ws0_col("integer", FALSE, "episode_key"),
                   x = .ws0_col("numeric", TRUE, "measure")),
    person_lineage = list(person_col = "person_id", via_roster = NULL), roster_namespace = "base")
  expect_s3_class(dsOMOP:::.compiledOutput("wide", components = list(wide = list(statement = "SELECT 1", fragment = f_ok)),
                                           publication_candidates = list(roster_cand())), "dsomop_compiled_output")
  f_rowid <- dsOMOP:::.fragment("res_e", "result", "episode", "SELECT rowId, covariate_id FROM pop_base_sparse",
    columns = list(rowId = .ws0_col("integer", FALSE, "episode_key"), covariate_id = .ws0_col("integer", FALSE, "measure")),
    person_lineage = list(person_col = NULL, via_roster = "base"), roster_namespace = "base")
  expect_s3_class(dsOMOP:::.compiledOutput("sparse", components = list(sparse = list(statement = "SELECT 1", fragment = f_rowid)),
    publication_candidates = list(dsOMOP:::.publicationCandidate("sparse", "sparse", "frame",
      L("roster", "rowId", "base", "cohort_row_id", "person_id"), requested_symbol = "D_sparse"))), "dsomop_compiled_output")
  # a rowId-keyed fragment that is not roster-mediated is refused
  f_rowid_bad <- dsOMOP:::.fragment("res_f", "result", "episode", "SELECT rowId, covariate_id FROM pop_base_sparse",
    columns = list(rowId = .ws0_col("integer", FALSE, "episode_key"), covariate_id = .ws0_col("integer", FALSE, "measure")),
    roster_namespace = "base")
  expect_error(dsOMOP:::.compiledOutput("sparse", components = list(sparse = list(statement = "SELECT 1", fragment = f_rowid_bad)),
    publication_candidates = list(dsOMOP:::.publicationCandidate("sparse", "sparse", "frame",
      L("roster", "rowId", "base", "cohort_row_id", "person_id"), requested_symbol = "D_sparse"))), "declares no person lineage")
})
