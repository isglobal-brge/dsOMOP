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
  rc <- read.csv(.ws0_inst("ohdsi", "reviewed_concepts.csv"), stringsAsFactors = FALSE)
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
  ops <- read.csv(.ws0_inst("ohdsi", "operator_classes.csv"), stringsAsFactors = FALSE)
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
  prov <- jsonlite::fromJSON(.ws0_inst("ohdsi", "provenance", "ohdsi-webapi-concepts.json"), simplifyVector = FALSE)
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

test_that("the v1 corpus classes are reviewed per plan instance", {
  cls <- read.csv(testthat::test_path("fixtures", "v1-corpus-classes.csv"), stringsAsFactors = FALSE)
  expect_true(all(c("plan_id", "package", "file", "line", "test", "class", "review", "prior_behaviour", "intended_behaviour", "rationale") %in% names(cls)))
  expect_true(all(cls$class %in% c("safe", "corrected", "newly_permitted", "newly_refused_structural", "newly_refused_publication",
                                   "publication_refused_unchanged", "unchanged_invalid", "non_plan")))
  expect_true(all(cls$review == "reviewed"))
  expect_false(anyDuplicated(cls$plan_id) > 0)
  expect_gte(nrow(cls), 1300L)
  expect_gte(sum(cls$class != "non_plan"), 580L)
  expect_true(all(nzchar(cls$rationale)))
  # the boundary examples of the review
  get <- function(file, line, pkg = "dsOMOP") cls[cls$package == pkg & cls$file == file & cls$line == line, ]
  expect_true(all(get("test-recipe-reach.R", 257)$class == "unchanged_invalid"))
  expect_true(all(get("test-manipulate.R", 216)$class == "publication_refused_unchanged"))
  expect_true(all(get("test-manipulate.R", 229)$class == "publication_refused_unchanged"))
  expect_true(all(get("test-manipulate.R", 208)$class == "newly_permitted"))
  expect_true(all(get("test-cohort.R", 512)$class == "publication_refused_unchanged"))
  for (ln in c(247, 268, 672)) expect_true("newly_permitted" %in% get("test-recipe-setops.R", ln)$class, info = ln)
  oc72 <- get("test-output-contracts.R", 72); expect_true(all(c("newly_permitted", "unchanged_invalid") %in% oc72$class))
  oc99 <- get("test-output-contracts.R", 99); expect_true(all(c("safe", "newly_permitted") %in% oc99$class))
  expect_true(all(get("test-plan-dsl.R", 884, "dsOMOPClient")$class == "unchanged_invalid"))
  expect_true(all(get("test-filters.R", 18)$class == "newly_permitted"))
  # inventory completeness: every plan-building suite of both packages is covered
  for (f in c("test-phase-e-ergonomics.R", "test-staged-harmonization-regression.R", "test-factor-harmonization-client.R", "test-dp-prepare.R")) {
    expect_true(f %in% cls$file[cls$package == "dsOMOPClient"], info = f)
  }
  for (f in c("test-factor-harmonization.R", "test-privacy-dp.R", "test-staging-contract.R", "test-phase1-leak-gates.R", "test-disclosure.R")) {
    expect_true(f %in% cls$file[cls$package == "dsOMOP"], info = f)
  }
  srv <- cls[cls$package == "dsOMOP", ]
  for (f in unique(srv$file)) {
    path <- testthat::test_path(f)
    skip_if(!file.exists(path), paste("suite file not found:", f))
    lines <- readLines(path, warn = FALSE)
    for (ln in unique(srv$line[srv$file == f])) expect_true(grepl("^\\s*test_that\\(", lines[ln]), info = paste(f, ln))
  }
})
