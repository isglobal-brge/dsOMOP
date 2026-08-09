pooling_contract_handle <- function() {
  handle <- create_test_handle(n_persons = 40)
  .buildBlueprint(handle)
  suppressWarnings(.omopAnalysisRegistry(handle))
  handle
}

test_that("every aggregate catalog entry has one valid typed pooling contract", {
  handle <- pooling_contract_handle()
  on.exit(cleanup_handle(handle))
  registry <- .omopAnalysisRegistry(handle)

  for (id in names(registry)) {
    entry <- registry[[id]]
    if (identical(entry$mode, "assign")) {
      expect_null(entry$meta$pooling_contract, info = id)
    } else {
      expect_silent(
        .omopAnalysisValidatePoolingContract(entry$meta$pooling_contract, id)
      )
    }
  }
})

test_that("aliases inherit the target pooling contract byte-for-byte", {
  handle <- pooling_contract_handle()
  on.exit(cleanup_handle(handle))
  registry <- .omopAnalysisRegistry(handle)
  aliases <- Filter(function(entry) !is.null(entry$meta$alias_target), registry)
  expect_gt(length(aliases), 0L)
  for (entry in aliases) {
    target <- registry[[entry$meta$alias_target]]
    expect_identical(entry$meta$pooling_contract,
                     target$meta$pooling_contract, info = entry$name)
  }
})

test_that("metadata exposes inert pooling semantics without compute internals", {
  handle <- pooling_contract_handle()
  on.exit(cleanup_handle(handle))
  meta <- .omopAnalysisGet(handle, "dsomop:incidence.rate")
  expect_identical(meta$pooling_contract$strategy, "tabular")
  expect_false(any(c("sql", "fn", "compute") %in% names(meta)))
  expect_false(any(vapply(meta, is.function, logical(1L))))
  expect_false(grepl("SELECT|FROM|JOIN", jsonlite::toJSON(meta$pooling_contract),
                     ignore.case = TRUE))
})

test_that("representative contracts encode exact sufficient statistics", {
  handle <- pooling_contract_handle()
  on.exit(cleanup_handle(handle))

  incidence <- .omopAnalysisGet(handle, "dsomop:incidence.rate")$pooling_contract
  expect_identical(names(incidence$columns),
                   c("stratum", "persons_at_risk", "person_days", "outcomes",
                     "person_outcomes", "proportion", "rate"))
  expect_identical(incidence$columns$rate$numerator, "person_outcomes")
  expect_identical(incidence$columns$rate$denominator, "person_days")

  treated <- .omopAnalysisGet(
    handle, "dsomop:txpath.percentage_treated"
  )$pooling_contract
  expect_identical(treated$columns$pct_treated$role, "ratio")
  expect_identical(treated$columns$pct_treated$scale, 100)

  km <- .omopAnalysisGet(handle, "dsomop:cm.kaplan_meier")$pooling_contract
  expect_identical(km$strategy, "kaplan_meier")
  expect_identical(km$strata, "arm")
  expect_identical(km$order_start, 1)
  expect_identical(km$order_step, 1)
  expect_identical(km$columns$survival_probability$role, "nonpoolable")

  effect <- .omopAnalysisGet(
    handle, "dsomop:cm.effect_estimate"
  )$pooling_contract
  expect_identical(effect$strategy, "effect_estimate")
  expect_identical(effect$strata, c("arm", "model_type"))
  expect_identical(effect$log_estimate, "log_estimate")
  expect_identical(effect$standard_error, "se_log_estimate")

  concepts <- .omopAnalysisGet(
    handle, "dsomop:condition.prevalence_by_concept"
  )$pooling_contract
  expect_identical(concepts$columns$concept_id$role, "key")
  expect_identical(concepts$columns$concept_name$role, "label")

  pathways <- .omopAnalysisGet(
    handle, "dsomop:txpath.pathways"
  )$pooling_contract
  expect_identical(pathways$columns$path_id$role, "key")
  expect_identical(pathways$columns$path$role, "label")

  treated <- .omopAnalysisGet(
    handle, "dsomop:txpath.percentage_treated"
  )$pooling_contract
  expect_identical(treated$columns$treatment_concept_id$role, "key")
  expect_identical(treated$columns$treatment$role, "label")
})

test_that("effect and Kaplan-Meier contracts reject unsafe column roles", {
  effect <- .omopPoolingEffectEstimate(
    c("arm", "log_effect", "se"), "log_effect", "se", strata = "arm"
  )
  effect$columns$log_effect <- .omopPoolingColumn("sum")
  expect_error(.omopAnalysisValidatePoolingContract(effect),
               "effect_estimate")

  km <- .omopPoolingKaplanMeier(
    c("arm", "time_bin", "at_risk", "events", "survival"),
    strata = "arm", order = "time_bin", at_risk = "at_risk",
    events = "events", survival = "survival"
  )
  km$columns$events <- .omopPoolingColumn("key")
  expect_error(.omopAnalysisValidatePoolingContract(km), "Kaplan-Meier")
})

test_that("every QueryLibrary DBMS subset carries its declared exact schema", {
  handle <- pooling_contract_handle()
  on.exit(cleanup_handle(handle))
  dialects <- c("postgresql", "sql server", "oracle", "redshift", "bigquery",
                "snowflake", "spark", "sqlite", "duckdb", "mysql")
  seen <- character(0)
  queries <- .ql_load_queries()
  for (dialect in dialects) {
    handle$target_dialect <- dialect
    entries <- .omopAnalysisAttachPoolingContracts(
      .omopAnalysisQueryEntries(handle)
    )
    for (entry in entries) {
      if (identical(entry$mode, "assign")) {
        expect_null(entry$meta$pooling_contract, info = entry$name)
      } else {
        expected <- as.character(queries[[entry$meta$query_id]]$outputs$field)
        expect_identical(names(entry$meta$pooling_contract$columns), expected,
                         info = paste(dialect, entry$name))
        seen <- union(seen, entry$meta$query_id)
      }
    }
  }
  expect_gt(length(seen), 40L)
})

test_that("Characterization cohort counts overlay the exact OHDSI registry id", {
  handle <- pooling_contract_handle()
  on.exit(cleanup_handle(handle))
  registry <- .omopAnalysisRegistry(handle)
  id <- "dsomop:ohdsi.characterization.c_cohort_counts"
  expect_true(id %in% names(registry))
  expect_identical(registry[[id]]$meta$adapter, "ohdsi_live")
  expect_false("dsomop:characterization.c_cohort_counts" %in% names(registry))
})

test_that("empty pooled outputs are normalized to the contracted schema", {
  entry <- .omopAnalysisEntry(
    name = "dsomop:test.empty", description = "", domain = "general",
    params = list(), compute = list(kind = "r", fn = function(...) data.frame()),
    dependencies = list(), disclosure = .omopAnalysisDisclosure(),
    scope = .omopAnalysisScope(),
    meta = list(pooling_contract = .omopPoolingTabular(
      c("group", "n"), sum_cols = "n"
    ))
  )
  out <- .omopAnalysisNormalizePoolingOutput(data.frame(), entry)
  expect_identical(names(out), c("group", "n"))
  expect_equal(nrow(out), 0L)
  expect_error(
    .omopAnalysisNormalizePoolingOutput(data.frame(wrong = 1), entry),
    "closed pooling schema"
  )
})

test_that("TreatmentPatterns uses stable concept identities for longitudinal paths", {
  rows <- data.frame(
    concept_id = c(111L, 111L, 222L),
    concept_name = c("First label", "First label", "Second label")
  )
  collapsed <- .omopTxPathCollapseConcepts(rows, max_len = 5L)
  expect_identical(collapsed$concept_id, c(111L, 222L))

  paths <- replicate(20L, collapsed, simplify = FALSE)
  gated <- .omopTxPathHierarchicalGate(paths, max_len = 5L)
  expect_true(all(c("parent_path_id", "path_id", "treatment_concept_id") %in%
                    names(gated)))
  expect_true(any(gated$path_id == "111"))
  expect_true(any(gated$path_id == "111>222"))
  expect_true(any(gated$treatment_concept_id == 222L, na.rm = TRUE))
})
