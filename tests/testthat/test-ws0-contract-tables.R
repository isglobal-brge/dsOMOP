# WS-0 (design-2.10.3 section 12.1): contract tables shipped as data.

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
  expect_true(all(c("fn", "input_roles", "ordering_selection", "null_missing",
                    "empty_result", "output_type", "suffix", "notes") %in% names(red)))
  expect_true(all(nzchar(red$empty_result)))
})

test_that("column classes separate assignment from aggregate admission", {
  cc <- read.csv(.ws0_inst("contracts", "column_classes.csv"), stringsAsFactors = FALSE)
  expect_true(all(cc$assignment_class %in% 1:3))
  expect_true(all(cc$aggregate_admission %in% c("blocked", "admitted")))
  for (i in seq_len(nrow(cc))) {
    expect_true(grepl(cc$pattern[i], cc$example_column[i], perl = TRUE), info = cc$pattern[i])
    # aggregate admission is the unchanged .detectSensitiveColumns() verdict
    blocked_today <- dsOMOP:::.detectSensitiveColumns(cc$example_column[i])
    expect_identical(cc$aggregate_admission[i] == "blocked", blocked_today, info = cc$example_column[i])
  }
  # birth components are class 2 (permitted on assignment) while still aggregate-blocked
  birth <- cc[cc$example_column %in% c("year_of_birth", "birth_datetime"), ]
  expect_true(all(birth$assignment_class == 2L))
  expect_true(all(birth$aggregate_admission == "blocked"))
  expect_true(all(cc$assignment_class[cc$aggregate_admission == "admitted"] == 3L))
})

test_that("operator classes and adapter mappings are consistent with the vendored metadata", {
  ops <- read.csv(.ws0_inst("ohdsi", "operator_classes.csv"), stringsAsFactors = FALSE)
  expect_identical(ops$concept_id[ops$class == "equality"], 4172703L)
  expect_setequal(ops$concept_id[ops$class == "inequality"], c(4171756L, 4172704L, 4171754L, 4171755L))
  fields <- read.csv(.ws0_inst("ohdsi", "OMOP_CDMv5.4_Field_Level.csv"), stringsAsFactors = FALSE)
  ee <- read.csv(.ws0_inst("ohdsi", "episode_event_fields.csv"), stringsAsFactors = FALSE)
  expect_false(anyDuplicated(ee$episode_event_field_concept_id) > 0)
  for (i in seq_len(nrow(ee))) {
    expect_true(any(fields$cdmTableName == ee$table_name[i] & fields$cdmFieldName == ee$key_column[i]),
                info = paste(ee$table_name[i], ee$key_column[i]))
    expect_true(any(fields$cdmTableName == ee$table_name[i] & fields$cdmFieldName == "person_id"))
  }
  cd <- read.csv(.ws0_inst("ohdsi", "cost_domains.csv"), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(cd))) {
    expect_true(any(fields$cdmTableName == cd$table_name[i] & fields$cdmFieldName == cd$key_column[i]),
                info = cd$table_name[i])
  }
  expect_true(all(c("cost_event_id", "cost_domain_id", "currency_concept_id") %in%
                    fields$cdmFieldName[fields$cdmTableName == "cost"]))
})

test_that("the boundary registry skeleton lists every assign method", {
  desc <- read.dcf(.ws0_inst("DESCRIPTION"))
  if (!"AssignMethods" %in% colnames(desc)) desc <- read.dcf(file.path("..", "..", "DESCRIPTION"))
  skip_if(!"AssignMethods" %in% colnames(desc), "DESCRIPTION without AssignMethods")
  assign_methods <- trimws(strsplit(desc[1, "AssignMethods"], ",")[[1]])
  reg <- read.csv(.ws0_inst("boundaries.csv"), stringsAsFactors = FALSE)
  expect_true(all(assign_methods %in% reg[["function"]]))
  expect_true(all(reg$kind %in% c("assign", "aggregate", "return", "staged")))
})

test_that("the v1 corpus-class fixture is complete and points at live tests", {
  cls <- read.csv(testthat::test_path("fixtures", "v1-corpus-classes.csv"), stringsAsFactors = FALSE)
  expect_gte(nrow(cls), 800L)
  expect_true(all(cls$class %in% c("safe", "corrected", "newly_permitted",
                                   "newly_refused_structural", "newly_refused_publication")))
  expect_true(all(cls$source %in% c("design", "heuristic")))
  expect_gte(sum(cls$source == "design"), 15L)
  srv <- cls[cls$package == "dsOMOP", ]
  for (f in unique(srv$file)) {
    path <- testthat::test_path(f)
    skip_if(!file.exists(path), paste("suite file not found:", f))
    lines <- readLines(path, warn = FALSE)
    for (ln in srv$line[srv$file == f]) {
      expect_true(grepl("^\\s*test_that\\(", lines[ln]), info = paste(f, ln))
    }
  }
})
