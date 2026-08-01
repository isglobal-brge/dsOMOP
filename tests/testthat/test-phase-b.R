# Phase B: subpopulation-from-table, unified cohort scoping, person/death
# exploration, numeric SD, prevalence pagination + global mode.
#
# The DISCLOSURE INVARIANT is the throughline: every scoped/materialized result
# must pass the per-patient .assertMinPersons gate on COUNT(DISTINCT person_id),
# and a cohort/table with < nfilter persons must be neither usable nor producible.

# A handle with a person key so token <-> id reversal works (Phase D).
keyed_handle <- function(n_persons = 40) {
  h <- create_test_handle(n_persons = n_persons)
  .setLegacyTestPersonKey(h, "phase-b",
                          .local_envir = parent.frame())
  h
}

# Build a token-keyed omop.table frame for a set of ORIGINAL person ids, exactly
# as .pseudonymizeIdentifiers would have stamped a plan output.
token_frame <- function(handle, ids) {
  toks <- .hashPersonKey(as.character(ids), handle$person_key)
  df <- data.frame(person_id = toks, v = seq_along(ids), stringsAsFactors = FALSE)
  attr(df, "dsomop_protected") <- "person_id"
  attr(df, "dsomop_pseudonymization") <-
    .canonicalPseudonymizationContract(.personKeyPublicContract(handle))
  class(df) <- union("omop.table", class(df))
  df
}

# --- 1. SUBPOPULATION FROM A WORKSPACE TABLE ---------------------------------

test_that("cohortFromTokenFrame reverses tokens -> ids and materializes a cohort", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    df <- token_frame(handle, 1:12)
    ct <- .cohortFromTokenFrame(handle, df)
    expect_true(is.character(ct))
    expect_true(grepl("^dsomop_cohort_fromtbl_", ct))
    # Materialized of ORIGINAL ids: exactly the 12 distinct subjects.
    n <- DBI::dbGetQuery(handle$conn,
      paste0("SELECT COUNT(DISTINCT subject_id) AS n FROM ", ct))$n
    expect_equal(n, 12)
  })
})

test_that("cohortFromTokenFrame is fail-closed below the person threshold", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    df <- token_frame(handle, 1:2)  # only 2 distinct persons
    expect_error(.cohortFromTokenFrame(handle, df), "Disclosive")
  })
})

test_that("cohortFromTokenFrame rejects non-omop.table input (admission gate)", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plain <- data.frame(person_id = as.character(1:12))
  expect_error(.cohortFromTokenFrame(handle, plain), "omop.table")
})

test_that("cohortFromTokenFrame requires the current pseudonymization contract", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  frame <- token_frame(handle, 1:12)
  missing <- frame
  attr(missing, "dsomop_pseudonymization") <- NULL
  expect_error(.cohortFromTokenFrame(handle, missing), "lacks.*contract")

  stale <- frame
  contract <- attr(stale, "dsomop_pseudonymization", exact = TRUE)
  contract$epoch <- as.integer(contract$epoch %||% 1L) + 1L
  attr(stale, "dsomop_pseudonymization") <- contract
  expect_error(.cohortFromTokenFrame(handle, stale),
               "incompatible pseudonymization contract")
  expect_length(handle$temp_tables, 0L)
})

test_that("cohortFromTokenFrame cannot overwrite an arbitrary CDM table", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    df <- token_frame(handle, 1:12)
    expect_error(
      .cohortFromTokenFrame(handle, df, new_name = "person"),
      "reserved dsomop_cohort_fromtbl_ namespace"
    )
    expect_true(DBI::dbExistsTable(handle$conn, "person"))
    expect_equal(
      DBI::dbGetQuery(handle$conn, "SELECT COUNT(*) AS n FROM person")$n,
      40
    )
  })
})

test_that("public cohort-from-table removes a materialization that fails re-gating", {
  handle <- keyed_handle(40)
  symbol <- paste0("cohort_from_table_regate_", Sys.getpid())
  .setHandle(symbol, handle)
  on.exit(.removeHandle(symbol), add = TRUE)
  .buildBlueprint(handle)
  baseline <- handle$temp_tables
  new_name <- "dsomop_cohort_fromtbl_regate_failure"

  # Three valid tokens clear the pre-write gate, but only two identify persons
  # present in the database. The post-materialization gate must fail and the
  # public operation must remove the newly created sensitive temp table.
  frame <- token_frame(handle, c(1L, 2L, 999999L))
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(
      omopCohortFromTableDS(frame, symbol, new_name = new_name),
      "Disclosive"
    )
  })

  expect_identical(handle$temp_tables, baseline)
  expect_false(DBI::dbExistsTable(handle$conn, new_name))
})

test_that("a workspace-derived cohort actually scopes a downstream explorer", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    df <- token_frame(handle, 1:12)
    ct <- .cohortFromTokenFrame(handle, df)
    scoped <- .profileConceptPrevalence(handle, "condition_occurrence",
                                        cohort_table = ct, top_n = 50)
    unscoped <- .profileConceptPrevalence(handle, "condition_occurrence",
                                          top_n = 50)
    # Scoping can only narrow: every scoped person count <= unscoped.
    expect_true(sum(scoped$n_persons) <= sum(unscoped$n_persons))
    # And the scoped population never exceeds the cohort size.
    expect_true(max(scoped$n_persons) <= 12)
  })
})

# --- 2. UNIFIED cohort scoping argument --------------------------------------

test_that(".resolveCohortTable accepts a table name and re-gates it", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    df <- token_frame(handle, 1:12)
    ct <- .cohortFromTokenFrame(handle, df)
    expect_identical(.resolveCohortTable(handle, ct), ct)
    expect_null(.resolveCohortTable(handle, NULL))
  })
})

test_that(".resolveCohortTable rejects unowned named tables", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    expect_error(.resolveCohortTable(handle, "person"),
                 "temporary cohorts created by this handle")
    expect_error(.resolveCohortTable(handle, "person; DROP TABLE person"),
                 "valid SQL identifier|must start")
  })
})

test_that(".resolveCohortTable materializes a cohort_definition_id", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # The fixture defines cohort_definition_id = 1 with 6 members.
  withr::with_options(list(nfilter.subset = 3), {
    ct <- .resolveCohortTable(handle, 1L)
    expect_true(grepl("^dsomop_cohort_def_", ct))
    n <- DBI::dbGetQuery(handle$conn,
      paste0("SELECT COUNT(DISTINCT subject_id) AS n FROM ", ct))$n
    expect_equal(n, 6)
  })
})

test_that("numeric cohort resolution preserves an owned work-name homonym", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  base <- "dsomop_cohort_def_1"
  .createTempTable(
    handle, base,
    paste(
      "SELECT 999 AS subject_id, '2000-01-01' AS cohort_start_date,",
      "'2000-01-01' AS cohort_end_date"
    )
  )

  resolved <- withr::with_options(
    list(nfilter.subset = 3),
    .resolveCohortTable(handle, 1L)
  )

  expect_false(identical(resolved, base))
  expect_true(all(vapply(c(base, resolved), function(name) {
    DBI::dbExistsTable(handle$conn, name)
  }, logical(1))))
  expect_identical(
    DBI::dbGetQuery(handle$conn, paste0(
      "SELECT subject_id FROM ", base
    ))$subject_id,
    999L
  )
})

test_that("explorers gain a unified cohort arg via .resolveCohortArg", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    df <- token_frame(handle, 1:12)
    ct <- .cohortFromTokenFrame(handle, df)
    # column.stats, value.counts, missingness all newly accept cohort_table.
    cs <- .profileColumnStats(handle, "measurement", "value_as_number",
                              cohort_table = ct)
    expect_true(!is.null(cs$n_total))
    # A low-cardinality column avoids the levels-density gate; the point here is
    # that cohort_table scoping is accepted and applied.
    vc <- .profileValueCounts(handle, "condition_occurrence",
                              "condition_type_concept_id", cohort_table = ct)
    expect_s3_class(vc, "data.frame")
    ms <- .profileMissingness(handle, "measurement", cohort_table = ct)
    expect_s3_class(ms, "data.frame")
  })
})

test_that("cohort scoping that isolates too few persons fails closed", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3), {
    # Persons 30,31 have no condition rows in the fixture's special cases but DO
    # have the baseline condition; instead drive the gate with a tiny cohort of
    # persons that exist but are below threshold for a scoped value-count.
    df <- token_frame(handle, 1:12)
    ct <- .cohortFromTokenFrame(handle, df)
    # Tighten the threshold so even the 12-person cohort is "too small": the
    # scoped value-count must then fail closed rather than leak.
    withr::with_options(list(nfilter.subset = 25), {
      expect_error(
        .profileValueCounts(handle, "condition_occurrence",
                            "condition_concept_id", cohort_table = ct),
        "Disclosive")
    })
  })
})

# --- 3. PERSON-COLUMN exploration --------------------------------------------

test_that("person domain concept defaults to gender but race/ethnicity selectable", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  bp <- .buildBlueprint(handle)

  expect_equal(.getDomainConceptColumn(bp, "person"), "gender_concept_id")

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    # gender via auto-detect prevalence
    pg <- .profileConceptPrevalence(handle, "person", top_n = 5)
    expect_true(nrow(pg) >= 1)
    # race + ethnicity via explicit column on value-counts (previously ignored)
    rc <- .profileValueCounts(handle, "person", "race_concept_id")
    expect_true("8527" %in% as.character(rc$value))
    et <- .profileValueCounts(handle, "person", "ethnicity_concept_id")
    expect_true(nrow(et) >= 1)
    # Exact birth year is a quasi-identifier; age must use protected bands.
    expect_error(.profileColumnStats(handle, "person", "year_of_birth"),
                 "blocked \\(sensitive\\)")
  })
})

# --- 4. DEATH table ----------------------------------------------------------

test_that("death auto-detects cause_concept_id (not the non-existent death_concept_id)", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  bp <- .buildBlueprint(handle)

  expect_equal(.getDomainConceptColumn(bp, "death"), "cause_concept_id")

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    pv <- .profileConceptPrevalence(handle, "death", top_n = 5)
    expect_true(nrow(pv) >= 1)
    vc <- .profileValueCounts(handle, "death", "cause_concept_id")
    expect_true(nrow(vc) >= 1)
  })
})

# --- 5. ALL-CONCEPTS pagination + GLOBAL prevalence --------------------------

test_that("prevalence pagination returns disjoint, ordered pages", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    page1 <- .profileConceptPrevalence(handle, "condition_occurrence",
                                       top_n = 2, offset = 0)
    page2 <- .profileConceptPrevalence(handle, "condition_occurrence",
                                       top_n = 2, offset = 2)
    # The two pages do not overlap (genuine offset, not a re-fetch of page 1).
    expect_length(intersect(page1$concept_id, page2$concept_id), 0)
  })
})

test_that("global prevalence ranks across all clinical tables with source_table", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    g <- .profileConceptPrevalence(handle, table = NULL, top_n = 8,
                                   global = TRUE)
    expect_true("source_table" %in% names(g))
    expect_true(length(unique(g$source_table)) > 1)  # spans >1 table
    # Every returned row clears the small-cell threshold (suppression applied).
    expect_true(all(g$n_persons >= 3))
  })
})

# --- 6. NUMERIC SUMMARY: SD ---------------------------------------------------

test_that("column.stats adds SD gated by the nfilter_dist floor", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  withr::with_options(list(nfilter.subset = 3, dsomop.nfilter.dist = 10), {
    cs <- .profileColumnStats(handle, "measurement", "value_as_number")
    expect_true(!is.null(cs$sd))
    expect_true(is.na(cs$sd) || cs$sd >= 0)
    # The canonical disclosure-safe summary still omits min/max.
    expect_null(cs$min)
    expect_null(cs$max)
  })
})

test_that("SD fails closed (NA) when non-NULL count is below nfilter_dist", {
  handle <- keyed_handle(40)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Raise the small-sample floor above the measurement count so SD must suppress
  # while mean (always safe past the person gate) is still returned.
  withr::with_options(list(nfilter.subset = 3, dsomop.nfilter.dist = 100000), {
    cs <- .profileColumnStats(handle, "measurement", "value_as_number")
    expect_true(!is.null(cs$mean))
    expect_true(is.na(cs$sd))
  })
})
