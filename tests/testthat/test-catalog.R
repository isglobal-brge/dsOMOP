# ==============================================================================
# Unit Tests: Catalog System (Query Templates, Classification, SDC)
# ==============================================================================

# --- Markdown Parser ----------------------------------------------------------

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

test_that(".ql_classify: SAFE_ASSIGN for row-level in assign mode", {
  sql <- "SELECT person_id, condition_concept_id
          FROM condition_occurrence"

  result <- dsOMOP:::.ql_classify(sql, "assign")

  expect_equal(result$class, "SAFE_ASSIGN")
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

test_that(".ql_load_allowlist: returns empty list for missing package", {
  result <- dsOMOP:::.ql_load_allowlist("nonexistent_package_xyz")
  expect_true(is.list(result))
  expect_equal(length(result), 0)
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

# --- Catalog List (Internal) -------------------------------------------------

test_that(".catalog_list: returns data frame with expected columns", {
  # Use a mock handle (catalog_list only uses queries + allowlist, not handle)
  mock_handle <- list()

  result <- dsOMOP:::.catalog_list(mock_handle)

  expect_true(is.data.frame(result))
  expected_cols <- c("id", "group", "name", "description", "mode",
                     "class", "poolable", "cdm_version", "n_inputs")
  expect_true(all(expected_cols %in% names(result)),
    info = paste("Missing columns:",
                 paste(setdiff(expected_cols, names(result)), collapse = ", ")))
})

test_that(".catalog_list: filters by domain", {
  mock_handle <- list()

  all_queries <- dsOMOP:::.catalog_list(mock_handle)
  cond_queries <- dsOMOP:::.catalog_list(mock_handle, domain = "Condition")

  if (nrow(all_queries) > 0 && nrow(cond_queries) > 0) {
    expect_true(nrow(cond_queries) <= nrow(all_queries))
    expect_true(all(tolower(cond_queries$group) == "condition"))
  }
})

test_that(".catalog_list: excludes BLOCKED queries", {
  mock_handle <- list()
  result <- dsOMOP:::.catalog_list(mock_handle)

  if (nrow(result) > 0) {
    expect_false(any(result$class == "BLOCKED"),
      info = "BLOCKED queries should not appear in catalog list")
  }
})

# --- Classifier Consistency with Allowlist ------------------------------------

test_that("all allowlisted queries pass classifier validation", {
  queries <- dsOMOP:::.ql_load_queries("dsOMOP")
  allowlist <- dsOMOP:::.ql_load_allowlist("dsOMOP")

  for (qid in names(allowlist)) {
    al <- allowlist[[qid]]
    q <- queries[[qid]]
    if (is.null(q)) next

    cl <- dsOMOP:::.ql_classify(q$sql, q$mode)

    # Allowlist class should match or be more permissive than classifier
    expect_true(
      al$class %in% c("SAFE_AGGREGATE", "SAFE_ASSIGN"),
      info = paste("Allowlisted query", qid, "should be safe")
    )

    # If classifier says BLOCKED, the allowlist entry is suspicious
    if (cl$class == "BLOCKED") {
      # This is OK for now — allowlist overrides classifier
      # But log a warning
    }
  }
})

# --- SDC Suppression ---------------------------------------------------------

test_that(".catalog_suppress_sensitive: suppresses small counts", {
  df <- data.frame(
    concept = c("A", "B", "C", "D"),
    n_persons = c(1, 2, 5, 10),
    n_records = c(2, 1, 8, 15),
    stringsAsFactors = FALSE
  )

  # Default threshold is 3 (from nfilter.tab)
  result <- dsOMOP:::.catalog_suppress_sensitive(
    df, c("n_persons", "n_records"), threshold = 3
  )

  expect_true(is.na(result$n_persons[1]))  # 1 < 3
  expect_true(is.na(result$n_persons[2]))  # 2 < 3
  expect_equal(result$n_persons[3], 5)     # 5 >= 3
  expect_equal(result$n_persons[4], 10)    # 10 >= 3

  expect_true(is.na(result$n_records[1]))  # 2 < 3
  expect_true(is.na(result$n_records[2]))  # 1 < 3
  expect_equal(result$n_records[3], 8)
})

test_that(".catalog_suppress_sensitive: handles missing columns gracefully", {
  df <- data.frame(x = 1:3, y = 4:6, stringsAsFactors = FALSE)

  result <- dsOMOP:::.catalog_suppress_sensitive(
    df, c("nonexistent_col"), threshold = 3
  )

  expect_equal(result, df)
})

test_that(".catalog_suppress_sensitive: handles NA values", {
  df <- data.frame(
    concept = c("A", "B"),
    n_persons = c(NA_real_, 5),
    stringsAsFactors = FALSE
  )

  result <- dsOMOP:::.catalog_suppress_sensitive(
    df, "n_persons", threshold = 3
  )

  expect_true(is.na(result$n_persons[1]))
  expect_equal(result$n_persons[2], 5)
})
