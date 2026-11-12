# WS-0 (design-2.10.3 section 12.1; review B5/B6/B7): contract tables shipped as data.

.ws0_inst <- function(...) {
  p <- system.file(..., package = "dsOMOP")
  if (!nzchar(p)) p <- file.path("inst", ...)
  p
}

test_that("the reducer contract table enumerates exactly the schema's reducers", {
  red <- read.csv(.ws0_inst("contracts", "reducers.csv"), stringsAsFactors = FALSE)
  schema <- dsOMOP:::.jsonSchemaPackaged("plan-v2")
  fns <- unlist(schema[["$defs"]]$reducer$properties$fn$enum)
  expect_setequal(red$fn, fns)
  expect_false(anyDuplicated(red$fn) > 0)
  expect_true(all(c("fn", "input_roles", "ordering_selection", "null_missing", "empty_result", "output_type", "suffix", "notes") %in% names(red)))
  expect_true(all(nzchar(red$empty_result)))
})

test_that("the shared column corpus declares assignment class and aggregate admission per row", {
  cc <- read.csv(.ws0_inst("column_classes.csv"), stringsAsFactors = FALSE)
  expect_true(all(c("column", "table", "context", "assignment_class", "aggregate_admission", "basis") %in% names(cc)))
  expect_true(all(cc$assignment_class %in% 1:3))
  expect_true(all(cc$aggregate_admission %in% c("blocked", "admitted")))
  expect_true(all(cc$context %in% c("cdm", "extension", "derived")))
  expect_false(anyDuplicated(paste(cc$table, cc$column)) > 0)
  # complete class-2 coverage: four birth fields, extension birth-date names, exact age, bands, absolute dates
  expect_true(all(c("year_of_birth", "month_of_birth", "day_of_birth", "birth_datetime") %in% cc$column[cc$assignment_class == 2]))
  expect_true(all(c("dob", "date_of_birth", "birth_date") %in% cc$column[cc$assignment_class == 2 & cc$context == "extension"]))
  expect_true(all(c("age", "age_group", "birth_year_band") %in% cc$column[cc$assignment_class == 2 & cc$context == "derived"]))
  expect_true(any(grepl("_date$", cc$column[cc$assignment_class == 2 & cc$context == "cdm"])))
  # class 1 examples, ordinary keys and measures
  expect_true(all(c("person_source_value", "value_as_string", "sig", "note_text", "zip", "npi", "mrn", "member_key") %in% cc$column[cc$assignment_class == 1]))
  expect_true(all(c("person_id", "cohort_row_id", "set_id", "value_as_number") %in% cc$column[cc$assignment_class == 3]))
  # aggregate admission is the UNCHANGED existing policy for cdm/extension columns: the sensitive-pattern
  # predicate decides, with untyped extension identifiers denied by the blueprint rule (member_key)
  for (i in which(cc$context != "derived")) {
    blocked_today <- dsOMOP:::.detectSensitiveColumns(cc$column[i]) ||
      (cc$context[i] == "extension" && length(dsOMOP:::.untypedIdentifierColumns(cc$column[i], reviewed = dsOMOP:::.PERSON_KEY_COLS(), allow_concepts = TRUE)) > 0L)
    expect_identical(cc$aggregate_admission[i] == "blocked", blocked_today, info = paste(cc$table[i], cc$column[i]))
  }
  # per-row independent decisions (never "admitted implies class 3"): birth fields are class 2 and aggregate-blocked,
  # exact age / bands / absolute dates are class 2 and aggregate-admitted (banded by the endpoint), class 1 is blocked everywhere
  birth <- cc[cc$column %in% c("year_of_birth", "month_of_birth", "day_of_birth", "birth_datetime", "dob", "date_of_birth", "birth_date"), ]
  expect_true(all(birth$assignment_class == 2L & birth$aggregate_admission == "blocked"))
  expect_true(all(cc$aggregate_admission[cc$column %in% c("age", "age_group", "birth_year_band", "condition_start_date")] == "admitted"))
  expect_true(all(cc$assignment_class[cc$column %in% c("age", "age_group", "birth_year_band", "condition_start_date")] == 2L))
  expect_true(all(cc$aggregate_admission[cc$assignment_class == 1] == "blocked"))
  expect_true(any(cc$assignment_class == 2 & cc$aggregate_admission == "admitted"))
  expect_true(any(cc$assignment_class == 2 & cc$aggregate_admission == "blocked"))
})

test_that("reviewed concepts, operator classes and adapter mappings carry verified values", {
  rc <- read.csv(.ws0_inst("ohdsi", "reviewed_concepts.csv"), stringsAsFactors = FALSE, colClasses = c(concept_code = "character"))
  expect_true(all(c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id", "concept_code",
                    "valid_start_date", "valid_end_date", "source_url", "source_snapshot") %in% names(rc)))
  expect_true(all(rc$source_snapshot == "v5.0 27-FEB-25"))
  by_id <- function(id) rc[rc$concept_id == id, ]
  ee <- read.csv(.ws0_inst("ohdsi", "episode_event_fields.csv"), stringsAsFactors = FALSE)
  expect_setequal(ee$episode_event_field_concept_id, c(1147127L, 1147094L, 1147082L, 1147115L, 1147138L, 1147165L, 1147070L, 1147049L))
  expect_false(any(c(1147332L, 1147210L, 1147869L) %in% ee$episode_event_field_concept_id))
  for (i in seq_len(nrow(ee))) {
    r <- by_id(ee$episode_event_field_concept_id[i])
    expect_identical(nrow(r), 1L, info = ee$episode_event_field_concept_id[i])
    expect_identical(r$concept_name, paste0(ee$table_name[i], ".", ee$key_column[i]))
    expect_identical(r$vocabulary_id, "CDM"); expect_identical(r$concept_class_id, "Field")
    expect_identical(ee$concept_name[i], r$concept_name); expect_identical(ee$concept_code[i], r$concept_code)
    expect_true(startsWith(ee$verification[i], "verified"))
  }
  expect_identical(by_id(1147070L)$concept_code, "CDM126"); expect_identical(by_id(1147049L)$concept_code, "CDM105")
  expect_identical(by_id(1147332L)$concept_class_id, "Table")                       # the rejected id is a table concept
  expect_identical(by_id(1147210L)$concept_name, "provider.specialty_source_value")  # the rejected id is another field
  fields <- read.csv(.ws0_inst("ohdsi", "OMOP_CDMv5.4_Field_Level.csv"), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(ee))) {
    expect_true(any(fields$cdmTableName == ee$table_name[i] & fields$cdmFieldName == ee$key_column[i]))
    expect_true(any(fields$cdmTableName == ee$table_name[i] & fields$cdmFieldName == "person_id"))
  }
  ops <- read.csv(.ws0_inst("ohdsi", "operator_classes.csv"), stringsAsFactors = FALSE, colClasses = c(concept_code = "character"))
  expect_identical(ops$concept_id[ops$class == "equality"], 4172703L)
  expect_setequal(ops$concept_id[ops$class == "inequality"], c(4171756L, 4172704L, 4171754L, 4171755L))
  for (i in seq_len(nrow(ops))) {
    r <- by_id(ops$concept_id[i]); expect_identical(r$concept_name, ops$operator[i]); expect_identical(r$concept_code, ops$concept_code[i])
  }
  # currencies: USD and EUR by code, not by assumption
  expect_identical(by_id(44818668L)$concept_code, "USD"); expect_identical(by_id(44818568L)$concept_code, "EUR")
  expect_identical(by_id(44818570L)$concept_code, "FKP")
  cd <- read.csv(.ws0_inst("ohdsi", "cost_domains.csv"), stringsAsFactors = FALSE)
  expected <- c(condition_occurrence = "Condition", drug_exposure = "Drug", procedure_occurrence = "Procedure",
                device_exposure = "Device", measurement = "Measurement", observation = "Observation",
                visit_occurrence = "Visit", specimen = "Specimen", visit_detail = "Visit Detail")
  expect_setequal(cd$table_name, names(expected))
  for (t in names(expected)) expect_identical(cd$cost_domain_id[cd$table_name == t], expected[[t]], info = t)
  expect_identical(cd$evidence_kind[cd$table_name == "visit_detail"], "ETL convention (not a standard DOMAIN row)")
  expect_true(grepl("LambdaBuilder", cd$basis[cd$table_name == "visit_detail"]))
  expect_true(all(cd$evidence_kind[cd$table_name != "visit_detail"] == "standard DOMAIN row"))
  for (i in seq_len(nrow(cd))) expect_true(any(fields$cdmTableName == cd$table_name[i] & fields$cdmFieldName == cd$key_column[i]))
  expect_true(all(c("cost_event_id", "cost_domain_id", "currency_concept_id") %in% fields$cdmFieldName[fields$cdmTableName == "cost"]))
  # the discriminator keeps a colliding visit-occurrence / visit-detail key apart
  expect_false(identical(cd$cost_domain_id[cd$table_name == "visit_occurrence"], cd$cost_domain_id[cd$table_name == "visit_detail"]))
  prov <- jsonlite::fromJSON(.ws0_inst("contracts", "ohdsi-webapi-concepts.json"), simplifyVector = FALSE)
  expect_identical(prov$snapshot_version, "v5.0 27-FEB-25")
  expect_false("Visit Detail" %in% unlist(prov$domains)); expect_true("Visit" %in% unlist(prov$domains))
})

test_that("the boundary registry skeleton lists every assign method", {
  desc <- read.dcf(.ws0_inst("DESCRIPTION"))
  if (!"AssignMethods" %in% colnames(desc)) desc <- read.dcf(file.path("..", "..", "DESCRIPTION"))
  skip_if(!"AssignMethods" %in% colnames(desc), "DESCRIPTION without AssignMethods")
  assign_methods <- trimws(strsplit(desc[1, "AssignMethods"], ",")[[1]])
  reg <- read.csv(.ws0_inst("boundaries.csv"), stringsAsFactors = FALSE, check.names = FALSE)
  expect_true(all(assign_methods %in% reg[["function"]]))
  expect_true(all(reg$kind %in% c("assign", "aggregate", "return", "staged")))
})

test_that("the v1 corpus classes are reviewed per plan instance and keyed by stable identifiers (CR-7)", {
  cls <- read.csv(testthat::test_path("fixtures", "v1-corpus-classes.csv"), stringsAsFactors = FALSE)
  expect_true(all(c("plan_id", "package", "file", "test", "baseline_line", "class", "review", "prior_behaviour", "intended_behaviour", "rationale") %in% names(cls)))
  expect_true(all(cls$class %in% c("safe", "corrected", "newly_permitted", "newly_refused_structural", "newly_refused_publication",
                                   "publication_refused_unchanged", "unchanged_invalid", "non_plan")))
  expect_true(all(cls$review == "reviewed"))
  expect_false(anyDuplicated(cls$plan_id) > 0)
  # stable keys: package:file:<test_that description>[@k][#sub-row]; baseline_line is informational (pinned 2.10.2 commits,
  # tools/ws0/corpus_baseline.json): editing a live test never changes this fixture, a reclassification edits corpus_decisions.csv
  expect_true(all(grepl("^dsOMOP(Client)?:test-[A-Za-z0-9-]+\\.R:.+", cls$plan_id)))
  expect_true(all(startsWith(cls$plan_id, paste0(cls$package, ":", cls$file, ":", cls$test))))
  expect_true(all(cls$baseline_line == as.integer(cls$baseline_line) & cls$baseline_line >= 1L))
  expect_gte(nrow(cls), 1300L)
  expect_gte(sum(cls$class != "non_plan"), 580L)
  expect_true(all(nzchar(cls$rationale)))
  get <- function(file, test, pkg = "dsOMOP") cls[cls$package == pkg & cls$file == file & cls$test == test, ]
  # the boundary examples of the review
  expect_true(all(get("test-recipe-reach.R", "reach C: blocked value_source is rejected at compile")$class == "unchanged_invalid"))
  expect_true(all(get("test-manipulate.R", "omopMergeDS fails closed when the join yields too few persons")$class == "publication_refused_unchanged"))
  expect_true(all(get("test-manipulate.R", "omopBindRowsDS of a table with ITSELF is blocked (doubling bypass)")$class == "publication_refused_unchanged"))
  expect_true(all(get("test-manipulate.R", "omopFilterDS rejects arbitrary numeric thresholds")$class == "newly_permitted"))
  expect_setequal(get("test-cohort.R", "identical cohort inputs preserve set semantics and lifecycle")$class, c("safe", "publication_refused_unchanged"))
  for (nm in c("set-op result below nfilter fails closed (empty intersection)", "set-op result below nfilter fails closed (tiny non-zero intersection)",
               "a too-small criteria population fails closed before any output")) {
    expect_true("newly_permitted" %in% get("test-recipe-setops.R", nm)$class, info = nm)
  }
  oc72 <- get("test-output-contracts.R", "baseline rejects blocked and unknown person columns"); expect_true(all(c("newly_permitted", "unchanged_invalid") %in% oc72$class))
  oc99 <- get("test-output-contracts.R", "baseline accepts only a public common age-grid coarsening"); expect_true(all(c("safe", "newly_permitted") %in% oc99$class))
  expect_true(all(get("test-filters.R", "a client-authored ordered threshold is rejected")$class == "newly_permitted"))
  # re-review B7: helper-built plans are inventoried and classified; mixed blocks are split
  ie190 <- get("test-plan-index-event.R", "Primary All preserves distinct same-date source events downstream")
  expect_identical(ie190$class, "safe"); expect_match(ie190$rationale, ".ie_resolve")
  so213 <- get("test-recipe-setops.R", "set-op union/intersect/setdiff produce the expected person sets")
  expect_setequal(sub("^.*#", "", so213$plan_id), c("union", "intersect", "setdiff")); expect_true(all(so213$class == "safe"))
  expect_identical(get("test-recipe-circe.R", "recipe_export_circe errors for an unknown population_id", "dsOMOPClient")$class, "unchanged_invalid")
  sl298 <- get("test-survival-longitudinal.R", "multi-state recipes round-trip and preview both components", "dsOMOPClient")
  expect_setequal(sub("^.*#", "", sl298$plan_id), c("1", "2")); expect_setequal(sl298$class, c("safe", "unchanged_invalid"))
  # second re-review: refusals are never certified as successful plans
  expect_identical(get("test-plan-dsl.R", "YAML plan loading never evaluates !expr tags", "dsOMOPClient")$class, "unchanged_invalid")
  expect_identical(get("test-recipe-lint.R", "no_compile: output targets an undeclared population", "dsOMOPClient")$class, "unchanged_invalid")
  co303 <- get("test-cohort.R", "cohort criteria fail closed instead of becoming unfiltered")
  expect_setequal(sub("^.*#", "", co303$plan_id), c("1", "2")); expect_setequal(co303$class, c("unchanged_invalid", "newly_permitted"))
  expect_identical(get("test-analysis-ports-1bc.R", "(d) a two-population port fails closed when either arm < nfilter")$class, "publication_refused_unchanged")
  # third re-review: a block whose only error expectations are expect_error(..., NA) asserts successes, never a refusal
  expect_identical(get("test-analysis-ports.R", "TAR diagnostics use synthesized cohort ends for point events")$class, "safe")
  cw222 <- get("test-extraction.R", "calendar windows reject injection, invalid dates, and reversed ranges")
  expect_setequal(sub("^.*#", "", cw222$plan_id), c("1", "2")); expect_setequal(cw222$class, c("unchanged_invalid", "newly_permitted"))
  expect_identical(get("test-recipe-circe.R", "numeric measurement bounds fail closed on Circe export", "dsOMOPClient")$class, "newly_permitted")
  # a block that builds no plan is only ever non_plan, and every non_plan row says so
  expect_true(all(grepl("^no plan|^gate:", cls$rationale[cls$class == "non_plan"])))
  expect_true(all(get("test-plan-dsl.R", "omop.date_handling builds correct spec", "dsOMOPClient")$class == "non_plan"))
  # the gate-helper blocks the design names carry their own dispositions
  expect_true(all(get("test-disclosure.R", "age_group filter enforces the 5-year minimum band width")$class == "newly_permitted"))
  expect_setequal(get("test-disclosure.R", "age grid is public, configurable, and only permits coarsening")$class, c("safe", "unchanged_invalid"))
  expect_true(all(get("test-phase1-leak-gates.R", "resolveConceptScopeColumn rejects leak vectors, accepts true concepts")$class == "unchanged_invalid"))
  expect_true("newly_permitted" %in% get("test-filters.R", "an exact-match (==) filter is blocked as fingerprinting")$class)
  expect_true("newly_permitted" %in% get("test-extraction.R", "date widths are inclusive and use the server policy")$class)
  # inventory completeness: every plan-building suite of both packages is covered
  for (f in c("test-phase-e-ergonomics.R", "test-staged-harmonization-regression.R", "test-factor-harmonization-client.R", "test-dp-prepare.R")) {
    expect_true(f %in% cls$file[cls$package == "dsOMOPClient"], info = f)
  }
  for (f in c("test-factor-harmonization.R", "test-privacy-dp.R", "test-staging-contract.R", "test-phase1-leak-gates.R", "test-disclosure.R", "test-phase-b.R")) {
    expect_true(f %in% cls$file[cls$package == "dsOMOP"], info = f)
  }
  # the baseline anchoring itself (pinned commits, strict join) is verified by tools/ws0/review_corpus_classes.py --check,
  # which tools/ws0/check_contracts.py runs; this suite only reads the frozen fixture
})
