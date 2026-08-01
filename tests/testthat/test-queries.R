# ==============================================================================
# Unit Tests: Query Template System (Templates, Classification, SDC)
# ==============================================================================

# --- Markdown Parser ----------------------------------------------------------

test_that("pinned OHDSI QueryLibrary audit manifest is internally consistent", {
  path <- system.file("queries", "upstream_querylibrary_audit.json",
                      package = "dsOMOP")
  expect_true(nzchar(path))

  audit <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_identical(audit$schema_version, 2L)
  expect_identical(
    audit$commit,
    "df8a21074b08519e581ca1afb7510468538117a4"
  )
  expect_identical(audit$query_count, 201L)
  expect_identical(audit$domain_count, 15L)
  expect_length(audit$queries, 201L)

  required <- c(
    "upstream_id", "path_id", "path", "sha256", "title", "dependencies", "tables",
    "output_fields", "output_type", "risk_signals", "portability_signals",
    "triage_class", "triage_reason", "dp_candidate", "dp_justification"
  )
  expect_true(all(vapply(audit$queries, function(query) {
    all(required %in% names(query))
  }, logical(1L))))

  ids <- vapply(audit$queries, `[[`, character(1L), "upstream_id")
  path_ids <- vapply(audit$queries, `[[`, character(1L), "path_id")
  paths <- vapply(audit$queries, `[[`, character(1L), "path")
  hashes <- vapply(audit$queries, `[[`, character(1L), "sha256")
  classes <- vapply(audit$queries, `[[`, character(1L), "triage_class")
  output_types <- vapply(audit$queries, `[[`, character(1L), "output_type")

  expect_identical(anyDuplicated(tolower(ids)), 0L)
  expect_identical(anyDuplicated(paths), 0L)
  expect_identical(paths, sort(paths, method = "radix"))
  expect_identical(path_ids, sub("\\.md$", "", basename(paths)))
  expect_identical(which(ids != path_ids), 201L)
  expect_identical(ids[[201L]], "P02")
  expect_identical(paths[[201L]], "procedure/PO2.md")
  expect_true(all(grepl("^[0-9a-f]{64}$", hashes)))
  expect_true(all(vapply(audit$queries, function(query) {
    is.list(query$dependencies) && length(query$dependencies) > 0L &&
      is.list(query$tables) && length(query$tables) > 0L &&
      is.list(query$output_fields) && length(query$output_fields) > 0L &&
      nzchar(query$title) && nzchar(query$triage_reason)
  }, logical(1L))))
  expect_true(all(vapply(audit$queries, function(query) {
    dependencies <- unlist(query$dependencies, use.names = FALSE)
    tables <- unlist(query$tables, use.names = FALSE)
    all(grepl("^@(cdm|vocab)\\.[a-z0-9_]+$", dependencies)) &&
      identical(
        tables,
        sort(unique(sub("^@(cdm|vocab)\\.", "", dependencies)),
             method = "radix")
      )
  }, logical(1L))))

  expected_classes <- c(
    vocabulary_reference_metadata = 54L,
    rewritable_patient_aggregate = 56L,
    statistical_needs_redesign = 73L,
    patient_rows_assignment_only = 13L,
    unsafe_as_written = 5L
  )
  recorded_classes <- unlist(audit$triage, use.names = TRUE)
  observed_classes <- table(factor(classes, levels = names(expected_classes)))
  expect_identical(recorded_classes, expected_classes)
  expect_identical(as.integer(observed_classes), unname(expected_classes))
  expect_identical(sum(recorded_classes), 201L)

  recorded_domains <- unlist(audit$domains, use.names = TRUE)
  observed_domains <- table(factor(dirname(paths), levels = names(recorded_domains)))
  expect_identical(sum(recorded_domains), 201L)
  expect_identical(as.integer(observed_domains), unname(recorded_domains))
  expect_identical(sum(unlist(audit$output_types, use.names = FALSE)), 201L)
  expect_setequal(names(table(output_types)), names(audit$output_types))

  manifest <- paste0(hashes, "  ", paths, "\n", collapse = "")
  observed_manifest_sha256 <- paste0(
    openssl::sha256(charToRaw(manifest))
  )
  expect_identical(
    audit$manifest_sha256,
    "07b718badf25c485a7ac12f035e6f158b28d034ea6fa176598d6f52229c9ac5f"
  )
  expect_identical(observed_manifest_sha256, audit$manifest_sha256)

  dp_candidates <- vapply(
    audit$queries, `[[`, logical(1L), "dp_candidate"
  )
  expect_identical(sum(dp_candidates), 130L)
  expect_identical(audit$dp_candidates$candidate, 130L)
  expect_identical(audit$dp_candidates$not_candidate, 71L)
  expect_true(all(vapply(audit$queries[dp_candidates], function(query) {
    is.character(query$dp_justification) &&
      length(query$dp_justification) == 1L &&
      nzchar(query$dp_justification)
  }, logical(1L))))
  expect_true(all(vapply(audit$queries[!dp_candidates], function(query) {
    is.null(query$dp_justification)
  }, logical(1L))))

  expect_false(audit$runtime_status$authorizes_execution)
  expect_identical(audit$runtime_status$queries_enabled_by_this_inventory, 0L)
  expect_match(
    audit$dp_semantics$candidate_does_not_mean,
    "currently executable"
  )
})

test_that(".ql_parse_markdown: parses complete query template", {
  md <- '---
Group: Condition
Name: Condition prevalence
ID: condition.prevalence
CDM Version: 5.3+
Mode: aggregate
Author: dsOMOP
---

## Description

Returns top conditions by person count.

## Input

| Parameter | Example | Mandatory | Notes |
|-----------|---------|-----------|-------|
| top_n     | 50      | No        | Number of results |

## Output

| Field | Description |
|-------|-------------|
| concept_id | Condition concept ID |
| n_persons | Number of persons |

## Query

```sql
SELECT co.condition_concept_id AS concept_id,
       COUNT(DISTINCT co.person_id) AS n_persons
FROM @cdm.condition_occurrence co
GROUP BY co.condition_concept_id
ORDER BY n_persons DESC
LIMIT @top_n
```

## Sensitive Fields

n_persons
'
  parsed <- dsOMOP:::.ql_parse_markdown(md)

  expect_equal(parsed$id, "condition.prevalence")
  expect_equal(parsed$group, "Condition")
  expect_equal(parsed$name, "Condition prevalence")
  expect_equal(parsed$mode, "aggregate")
  expect_equal(parsed$cdm_version, "5.3+")
  expect_true(grepl("top conditions", parsed$description))
  expect_true(grepl("SELECT", parsed$sql))
  expect_true(grepl("@cdm", parsed$sql))
  expect_true(grepl("@top_n", parsed$sql))
  expect_true("n_persons" %in% parsed$sensitive_fields)
  expect_true(is.data.frame(parsed$inputs))
  expect_true(nrow(parsed$inputs) >= 1)
  expect_true(is.data.frame(parsed$outputs))
  expect_true(nrow(parsed$outputs) >= 1)
})

test_that(".ql_parse_markdown: handles missing optional sections", {
  md <- '---
Group: General
Name: Simple count
Mode: aggregate
---

## Query

```sql
SELECT COUNT(*) AS n FROM @cdm.person
```
'
  parsed <- dsOMOP:::.ql_parse_markdown(md)

  expect_equal(parsed$group, "General")
  expect_equal(parsed$name, "Simple count")
  expect_true(!is.null(parsed$sql))
  expect_equal(parsed$description, "")
  expect_null(parsed$inputs)
  expect_null(parsed$outputs)
  expect_equal(length(parsed$sensitive_fields), 0)
})

test_that(".ql_parse_markdown: derives ID from group and name", {
  md <- '---
Group: Drug Exposure
Name: Top Drugs by Person Count
Mode: aggregate
---

## Query

```sql
SELECT drug_concept_id, COUNT(DISTINCT person_id) AS n
FROM @cdm.drug_exposure GROUP BY drug_concept_id
```
'
  parsed <- dsOMOP:::.ql_parse_markdown(md)

  # ID should be derived from group.name in snake_case
  expect_true(grepl("drug", parsed$id))
  expect_true(grepl("\\.", parsed$id))
})

test_that(".ql_parse_table: parses markdown table correctly", {
  text <- "
| Field | Description | Type |
|-------|-------------|------|
| concept_id | The concept ID | integer |
| n_persons | Person count | numeric |
"
  result <- dsOMOP:::.ql_parse_table(text)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true("field" %in% names(result))
  expect_true("description" %in% names(result))
  expect_equal(result$field[1], "concept_id")
})

test_that(".ql_parse_table: returns NULL for empty text", {
  expect_null(dsOMOP:::.ql_parse_table(NULL))
  expect_null(dsOMOP:::.ql_parse_table(""))
  expect_null(dsOMOP:::.ql_parse_table("No table here"))
})

# --- Safety Classifier -------------------------------------------------------

test_that(".ql_classify: SAFE_AGGREGATE for grouped aggregates", {
  sql <- "SELECT condition_concept_id, COUNT(DISTINCT person_id) AS n_persons
          FROM condition_occurrence GROUP BY condition_concept_id"

  result <- dsOMOP:::.ql_classify(sql, "aggregate")

  expect_equal(result$class, "SAFE_AGGREGATE")
  expect_true("n_persons" %in% result$sensitive_fields_detected)
  expect_true(result$poolable)
})

test_that(".ql_classify: SAFE_AGGREGATE for simple COUNT(*)", {
  sql <- "SELECT COUNT(*) AS total FROM person"

  result <- dsOMOP:::.ql_classify(sql, "aggregate")

  expect_equal(result$class, "SAFE_AGGREGATE")
})

test_that(".ql_classify: BLOCKED for person_id in aggregate SELECT", {
  sql <- "SELECT person_id, condition_concept_id
          FROM condition_occurrence"

  result <- dsOMOP:::.ql_classify(sql, "aggregate")

  expect_equal(result$class, "BLOCKED")
  expect_true(grepl("identifier", result$reason))
})

test_that(".ql_classify: BLOCKED for free-text columns", {
  sql <- "SELECT concept_name, note_text, COUNT(*) AS n
          FROM note GROUP BY concept_name, note_text"

  result <- dsOMOP:::.ql_classify(sql, "aggregate")

  expect_equal(result$class, "BLOCKED")
  expect_true(grepl("free-text", result$reason))
})

test_that(".ql_classify: BLOCKED for exact birth components", {
  sql <- paste(
    "SELECT year_of_birth, COUNT(DISTINCT person_id) AS n_persons",
    "FROM person GROUP BY year_of_birth"
  )
  result <- dsOMOP:::.ql_classify(sql, "aggregate")

  expect_equal(result$class, "BLOCKED")
  expect_match(result$reason, "exact birth component")
})

test_that(".ql_classify: BLOCKED for SELECT * in aggregate", {
  sql <- "SELECT * FROM condition_occurrence"

  result <- dsOMOP:::.ql_classify(sql, "aggregate")

  expect_equal(result$class, "BLOCKED")
  expect_true(grepl("SELECT \\*", result$reason))
})

test_that(".ql_classify: BLOCKED for aggregate without aggregates", {
  sql <- "SELECT condition_concept_id, condition_start_date
          FROM condition_occurrence WHERE person_id = 1"

  result <- dsOMOP:::.ql_classify(sql, "aggregate")

  expect_equal(result$class, "BLOCKED")
})

test_that(".ql_classify: unreviewed row-level assign SQL fails closed", {
  sql <- "SELECT person_id, condition_concept_id
          FROM condition_occurrence"

  result <- dsOMOP:::.ql_classify(sql, "assign")

  expect_equal(result$class, "BLOCKED")
  expect_false(result$poolable)
})

test_that(".ql_classify: person_id inside COUNT is OK for aggregate", {
  sql <- "SELECT condition_concept_id,
                 COUNT(DISTINCT person_id) AS n_persons
          FROM condition_occurrence
          GROUP BY condition_concept_id"

  result <- dsOMOP:::.ql_classify(sql, "aggregate")

  expect_equal(result$class, "SAFE_AGGREGATE")
})

test_that(".ql_classify: BLOCKED for empty SQL", {
  result <- dsOMOP:::.ql_classify("", "aggregate")
  expect_equal(result$class, "BLOCKED")

  result2 <- dsOMOP:::.ql_classify(NULL, "aggregate")
  expect_equal(result2$class, "BLOCKED")
})

test_that(".ql_classify: detects common count column names", {
  sql <- "SELECT concept_id, COUNT(*) AS count_value,
          COUNT(DISTINCT person_id) AS person_count
          FROM condition_occurrence GROUP BY concept_id"

  result <- dsOMOP:::.ql_classify(sql, "aggregate")

  expect_equal(result$class, "SAFE_AGGREGATE")
  expect_true("count_value" %in% result$sensitive_fields_detected)
  expect_true("person_count" %in% result$sensitive_fields_detected)
})

test_that(".ql_classify: BLOCKED for unbinned dates in aggregate SELECT", {
  sql <- "SELECT condition_start_date, condition_concept_id
          FROM condition_occurrence"

  result <- dsOMOP:::.ql_classify(sql, "aggregate")

  expect_equal(result$class, "BLOCKED")
  expect_true(grepl("date", result$reason, ignore.case = TRUE))
})

test_that(".ql_classify: allows dates with binning functions", {
  sql <- "SELECT EXTRACT(YEAR FROM condition_start_date) AS year,
          COUNT(DISTINCT person_id) AS n_persons
          FROM condition_occurrence
          GROUP BY EXTRACT(YEAR FROM condition_start_date)"

  result <- dsOMOP:::.ql_classify(sql, "aggregate")

  expect_equal(result$class, "SAFE_AGGREGATE")
})

# --- Allowlist Loading --------------------------------------------------------

test_that(".ql_load_allowlist: loads from package inst", {
  allowlist <- dsOMOP:::.ql_load_allowlist("dsOMOP")

  # Should load our curated allowlist
  expect_true(is.list(allowlist))
  expect_true(length(allowlist) > 0)
  expect_true("condition.prevalence_by_concept" %in% names(allowlist))

  entry <- allowlist[["condition.prevalence_by_concept"]]
  expect_equal(entry$class, "SAFE_AGGREGATE")
  expect_true(entry$poolable)
  expect_true("n_persons" %in% entry$sensitive_fields)
})

test_that(".ql_load_allowlist: errors in strict mode for missing package", {
  expect_error(
    dsOMOP:::.ql_load_allowlist("nonexistent_package_xyz"),
    "Allowlist file not found"
  )
})

test_that(".ql_load_allowlist: returns empty list in non-strict mode for missing package", {
  withr::with_options(list(dsomop.query_strict = FALSE), {
    result <- dsOMOP:::.ql_load_allowlist("nonexistent_package_xyz")
    expect_true(is.list(result))
    expect_equal(length(result), 0)
  })
})

# --- Query Template Loading ---------------------------------------------------

test_that(".ql_load_queries: loads Markdown templates", {
  queries <- dsOMOP:::.ql_load_queries("dsOMOP")

  expect_true(is.list(queries))
  expect_true(length(queries) > 0)

  # Check a known query exists
  cond_q <- queries[["condition.prevalence_by_concept"]]
  expect_false(is.null(cond_q))
  expect_equal(cond_q$group, "Condition")
  expect_true(!is.null(cond_q$sql))
  expect_true(grepl("condition_occurrence", cond_q$sql))
  expect_true("n_persons" %in% cond_q$sensitive_fields)
})

test_that(".ql_load_queries: all templates have valid SQL", {
  queries <- dsOMOP:::.ql_load_queries("dsOMOP")

  for (qid in names(queries)) {
    q <- queries[[qid]]
    expect_true(!is.null(q$sql),
      info = paste("Query", qid, "should have SQL"))
    expect_true(nchar(q$sql) > 10,
      info = paste("Query", qid, "SQL should be non-trivial"))
    expect_true(grepl("SELECT", q$sql, ignore.case = TRUE),
      info = paste("Query", qid, "should contain SELECT"))
  }
})

test_that(".ql_load_queries: all templates have required metadata", {
  queries <- dsOMOP:::.ql_load_queries("dsOMOP")

  for (qid in names(queries)) {
    q <- queries[[qid]]
    expect_true(nchar(q$id) > 0,
      info = paste("Query", qid, "should have an ID"))
    expect_true(nchar(q$name) > 0,
      info = paste("Query", qid, "should have a name"))
    expect_true(nchar(q$group) > 0,
      info = paste("Query", qid, "should have a group"))
    expect_true(q$mode %in% c("aggregate", "assign"),
      info = paste("Query", qid, "should have valid mode"))
  }
})

# --- Query List (Internal) ---------------------------------------------------

test_that(".query_list: returns data frame with expected columns", {
  # Use a mock handle (query_list only uses queries + allowlist, not handle)
  mock_handle <- list()

  result <- dsOMOP:::.query_list(mock_handle)

  expect_true(is.data.frame(result))
  expected_cols <- c("id", "group", "name", "description", "mode",
                     "class", "poolable", "cdm_version", "n_inputs")
  expect_true(all(expected_cols %in% names(result)),
    info = paste("Missing columns:",
                 paste(setdiff(expected_cols, names(result)), collapse = ", ")))
})

test_that(".query_list: filters by domain", {
  mock_handle <- list()

  all_queries <- dsOMOP:::.query_list(mock_handle)
  cond_queries <- dsOMOP:::.query_list(mock_handle, domain = "Condition")

  if (nrow(all_queries) > 0 && nrow(cond_queries) > 0) {
    expect_true(nrow(cond_queries) <= nrow(all_queries))
    expect_true(all(tolower(cond_queries$group) == "condition"))
  }
})

test_that(".query_list: excludes BLOCKED queries from listing", {
  mock_handle <- list()
  result <- dsOMOP:::.query_list(mock_handle)

  if (nrow(result) > 0) {
    expect_false(any(result$class == "BLOCKED"),
      info = "BLOCKED queries should not appear in query list")
  }
})

# --- Classifier Consistency with Allowlist ------------------------------------

test_that("allowlist classes are explicit and blocked entries stay blocked", {
  queries <- dsOMOP:::.ql_load_queries("dsOMOP")
  allowlist <- dsOMOP:::.ql_load_allowlist("dsOMOP")

  for (qid in names(allowlist)) {
    al <- allowlist[[qid]]
    q <- queries[[qid]]
    if (is.null(q)) next

    cl <- dsOMOP:::.ql_classify(q$sql, q$mode)

    expect_true(
      al$class %in% c("SAFE_AGGREGATE", "SAFE_ASSIGN", "BLOCKED"),
      info = paste("Allowlist query", qid, "has an invalid class")
    )
    if (identical(al$class, "BLOCKED")) {
      expect_equal(cl$class, "BLOCKED",
                   info = paste("Blocked query", qid,
                                "must also fail the static classifier"))
    }
  }

  blocked_birth_detail <- c(
    "person.year_of_birth_distribution", "person.birth_month_distribution",
    "person.gender_by_age_decade", "person.load"
  )
  expect_true(all(vapply(blocked_birth_detail, function(id) {
    identical(allowlist[[id]]$class, "BLOCKED")
  }, logical(1))))
})

# --- SDC Suppression ---------------------------------------------------------

test_that(".query_suppress_sensitive: drops rows with small counts", {
  withr::with_options(list(nfilter.tab = 3), {
    df <- data.frame(
      concept = c("A", "B", "C", "D"),
      n_persons = c(1, 2, 5, 10),
      n_records = c(2, 1, 8, 15),
      stringsAsFactors = FALSE
    )

    result <- dsOMOP:::.query_suppress_sensitive(
      df, c("n_persons", "n_records"), threshold = 3
    )

    # Rows A and B have counts below threshold → dropped entirely
    expect_equal(nrow(result), 2)
    expect_equal(result$concept, c("C", "D"))
    expect_equal(result$n_persons, c(5, 10))
    expect_equal(result$n_records, c(8, 15))
  })
})

test_that(".query_suppress_sensitive: handles missing columns gracefully", {
  df <- data.frame(x = 1:3, y = 4:6, stringsAsFactors = FALSE)

  result <- dsOMOP:::.query_suppress_sensitive(
    df, c("nonexistent_col"), threshold = 3
  )

  expect_equal(result, df)
})

test_that(".query_suppress_sensitive: drops NA count rows (fail-closed)", {
  df <- data.frame(
    concept = c("A", "B"),
    n_persons = c(NA_real_, 5),
    stringsAsFactors = FALSE
  )

  result <- dsOMOP:::.query_suppress_sensitive(
    df, "n_persons", threshold = 3
  )

  # Fail-closed: an NA count is treated as disclosive (an empty group is
  # 0-equivalent), so its row is dropped, leaving only the row >= threshold.
  expect_equal(nrow(result), 1L)
  expect_equal(result$n_persons[1], 5)
})

# --- SQL Injection Prevention ------------------------------------------------

test_that(".sanitizeQueryParam: accepts valid numeric values (no template)", {
  expect_equal(dsOMOP:::.sanitizeQueryParam("201820", "concept_id"), "201820")
  expect_equal(dsOMOP:::.sanitizeQueryParam("50", "top_n"), "50")
  expect_equal(dsOMOP:::.sanitizeQueryParam("2026", "reference_year"), "2026")
  expect_equal(dsOMOP:::.sanitizeQueryParam("-1", "offset"), "-1")
})

test_that(".sanitizeQueryParam: enforces declared integer type from template", {
  # Template declares concept_id with numeric example
  inputs_df <- data.frame(
    parameter = c("concept_id", "top_n"),
    example = c("201820", "50"),
    mandatory = c("Yes", "No"),
    notes = c("Concept ID", "Max rows"),
    stringsAsFactors = FALSE
  )
  # Valid integer values accepted
  expect_equal(dsOMOP:::.sanitizeQueryParam("201820", "concept_id", inputs_df), "201820")
  expect_equal(dsOMOP:::.sanitizeQueryParam("50", "top_n", inputs_df), "50")

  # SQL injection attempt with declared integer type: hard reject (not quoted)
  expect_error(
    dsOMOP:::.sanitizeQueryParam("1 OR 1=1", "concept_id", inputs_df),
    "must be integer"
  )
  expect_error(
    dsOMOP:::.sanitizeQueryParam("1 UNION SELECT person_id FROM person",
                                  "concept_id", inputs_df),
    "must be integer"
  )
  # Decimal rejected for integer param
  expect_error(
    dsOMOP:::.sanitizeQueryParam("3.14", "concept_id", inputs_df),
    "must be an integer"
  )
  # Non-numeric string rejected
  expect_error(
    dsOMOP:::.sanitizeQueryParam("diabetes", "concept_id", inputs_df),
    "must be integer"
  )
})

test_that(".sanitizeQueryParam: fail-closed for unknown params (non-numeric rejected)", {
  # Without template: non-numeric values rejected (all templates use numeric params)
  expect_error(
    dsOMOP:::.sanitizeQueryParam("1 OR 1=1", "concept_id"),
    "non-numeric"
  )
  expect_error(
    dsOMOP:::.sanitizeQueryParam("diabetes", "name"),
    "non-numeric"
  )
  expect_error(
    dsOMOP:::.sanitizeQueryParam("O'Brien", "name"),
    "non-numeric"
  )
})

test_that(".sanitizeQueryParam: rejects empty/NA values", {
  expect_error(dsOMOP:::.sanitizeQueryParam(NA, "param"), "empty or NA")
  expect_error(dsOMOP:::.sanitizeQueryParam("", "param"), "empty or NA")
})

test_that(".inferParamType: correctly infers types from template inputs", {
  inputs_df <- data.frame(
    parameter = c("concept_id", "top_n", "threshold"),
    example = c("201820", "50", "3.14"),
    stringsAsFactors = FALSE
  )
  expect_equal(dsOMOP:::.inferParamType("concept_id", inputs_df), "integer")
  expect_equal(dsOMOP:::.inferParamType("top_n", inputs_df), "integer")
  expect_equal(dsOMOP:::.inferParamType("threshold", inputs_df), "numeric")
  expect_equal(dsOMOP:::.inferParamType("unknown_param", inputs_df), "unknown")
  expect_equal(dsOMOP:::.inferParamType("concept_id", NULL), "unknown")
})

test_that("legacy aggregate query endpoint rejects client-selected assign mode", {
  handle <- create_test_handle(n_persons = 15)
  on.exit(cleanup_handle(handle), add = TRUE)
  .setHandle("query_mode_gate", handle)
  on.exit(.removeHandle("query_mode_gate"), add = TRUE)

  expect_error(
    omopQueryExecDS("query_mode_gate", "person.load", mode = "assign"),
    "aggregate-only"
  )
})

test_that("legacy aggregate query endpoint uses the unified banding gate", {
  handle <- create_test_handle(n_persons = 17)
  on.exit(cleanup_handle(handle), add = TRUE)
  .setHandle("query_band_gate", handle)
  on.exit(.removeHandle("query_band_gate"), add = TRUE)

  result <- suppressWarnings(
    withr::with_options(
      list(nfilter.tab = 3, nfilter.subset = 3, dsomop.nfilter.band = 5),
      omopQueryExecDS("query_band_gate", "person.demographic_summary")
    )
  )

  expect_true(is.data.frame(result))
  expect_true(all(result$n_persons %% 5 == 0))
})
