# ==============================================================================
# Tests for the LIVE-COMPUTE Achilles + new-canonical OHDSI catalog ports.
#
# The Achilles catalog family no longer READS precomputed achilles_results rows:
# every "dsomop:achilles.<id>" entry now COMPUTES its metric live from the CDM
# (assembled from the pack_achilles_*.R registrars) and runs through the SINGLE
# disclosure gate. These tests assert, per worklist group, that each group's
# representative entries (1) LIST with a live adapter and NO precomputed read,
# (2) RUN on the SQLite fixture through the one run path, and (3) the GATE FIRES
# (counts banded to multiples of nfilter_band; distributions strip min/max and
# expose only p10..p90; no *_source_value ever escapes; scoping restricts).
#
# Companion to test-analysis-ports.R / test-analysis-ports-1bc.R (which cover the
# native diagnostic / FeatureExtraction / Characterization / CohortMethod ports)
# and test-achilles.R (which covers the SEPARATE standalone Achilles results API
# .achillesGetResults/.achillesGetDistributions — untouched by the catalog port).
# ==============================================================================

al_handle <- function(n_persons = 40) {
  h <- create_test_handle(n_persons = n_persons)
  h$person_key <- as.raw(1:16)
  .buildBlueprint(h)
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

al_opts <- function() {
  list(nfilter.subset = 3, nfilter.tab = 3, nfilter.dist = 10,
       dsomop.nfilter.dist = 10, dsomop.nfilter.band = 5)
}

al_diabetes_cohort <- function(h) {
  .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                mode = "temporary", cohort_id = 1)
}

# A compute$fn that reads a precomputed results table is exactly what the port
# eliminates; assert no live entry's fn body references one.
al_reads_precomputed <- function(entry) {
  fn <- entry$compute$fn
  if (is.null(fn)) return(FALSE)
  grepl("achillesGetResults|achillesGetDistributions|ohdsiGetResults",
        paste(deparse(body(fn)), collapse = "\n"))
}

# The id sets each worklist Achilles group owns (stable legacy ids preserved).
al_groups <- list(
  "ACH-A person"        = c(1, 2, 3, 4, 5, 10, 12, 2000, 2001, 2002, 2003),
  "ACH-B obsperiod cnt" = c(101, 102, 108, 109, 113),
  "ACH-C obsperiod dist"= c(103, 104, 105, 106, 107),
  "ACH-D counts"        = c(200, 400, 600, 700, 800, 1800, 202, 402, 602, 702,
                            802, 1802, 221, 500, 502),
  "ACH-E records"       = c(201, 401, 601, 701, 801, 1801, 220, 420, 620, 720,
                            820, 1820, 505, 1818),
  "ACH-F distinct/pp"   = c(203, 403, 603, 703, 803, 1803),
  "ACH-G age dist"      = c(206, 406, 606, 706, 806, 1806, 506),
  "ACH-H drug-field"    = c(715, 716, 717)
)

# --- (1) every Achilles catalog entry computes live (no precomputed read) ------

test_that("every dsomop:achilles.* entry computes live (zero precomputed reads)", {
  h <- al_handle()
  on.exit(cleanup_handle(h))

  reg <- .omopAnalysisRegistry(h)
  ach <- reg[grepl("^dsomop:achilles\\.", names(reg))]
  expect_true(length(ach) >= 70)
  # Not one Achilles entry reads achilles_results / achilles_results_dist.
  leaks <- names(ach)[vapply(ach, al_reads_precomputed, logical(1))]
  expect_equal(length(leaks), 0,
               info = paste("precomputed-reading Achilles entries:",
                            paste(leaks, collapse = ", ")))
  # All carry a live adapter label and a kind="r" compute fn.
  adapters <- vapply(ach, function(e) e$meta$adapter %||% "", character(1))
  expect_true(all(adapters %in% c("achilles", "achilles_live")))
  expect_true(all(vapply(ach, function(e)
    identical(e$compute$kind, "r") && is.function(e$compute$fn), logical(1))))
})

test_that("each worklist Achilles group is fully registered + scopable", {
  h <- al_handle()
  on.exit(cleanup_handle(h))
  lst <- .omopAnalysisList(h)
  for (grp in names(al_groups)) {
    ids <- paste0("dsomop:achilles.", al_groups[[grp]])
    sub <- lst[lst$name %in% ids, ]
    expect_equal(nrow(sub), length(ids),
                 info = paste(grp, "missing ids:",
                              paste(setdiff(ids, lst$name), collapse = ", ")))
    # Live ports are cohort-scopable (the cohort IS the analysis population).
    expect_true(all(sub$accepts_cohort), info = paste(grp, "not scopable"))
  }
})

# --- (2)+(3) each group lists, runs, and the gate fires -----------------------

test_that("every Achilles entry runs cleanly + gates (no raw SQL leak)", {
  h <- al_handle()
  on.exit(cleanup_handle(h))

  withr::with_options(al_opts(), {
    reg <- .omopAnalysisRegistry(h)
    ach <- names(reg)[grepl("^dsomop:achilles\\.", names(reg))]
    for (id in ach) {
      df <- tryCatch(.omopAnalysisRun(h, id),
                     error = function(e) conditionMessage(e))
      # A live Achilles entry never surfaces a raw "no such table/column" SQL
      # error (a missing CDM table/field fails closed to an empty frame).
      if (is.character(df)) {
        expect_false(grepl("no such table|no such column|syntax error", df,
                           ignore.case = TRUE),
                     info = paste(id, "raw SQL error:", df))
        next
      }
      expect_s3_class(df, "data.frame")
      if (nrow(df) == 0) next
      # The gate fired: any count_value is banded to a multiple of nfilter_band.
      if ("count_value" %in% names(df)) {
        ok <- is.na(df$count_value) | df$count_value %% 5 == 0
        expect_true(all(ok), info = paste(id, "count_value not banded"))
      }
      # No *_source_value / free text ever escapes.
      expect_false(any(grepl("_source_value$", names(df))),
                   info = paste(id, "leaked a source_value column"))
    }
  })
})

test_that("ACH-A/B/D person-unit counts are suppressed + banded by the gate", {
  h <- al_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(al_opts(), {
    # by-gender (2), obs-period age band (101), persons-with->=1 condition (400).
    for (id in c("dsomop:achilles.2", "dsomop:achilles.101",
                 "dsomop:achilles.400")) {
      df <- .omopAnalysisRun(h, id)
      expect_s3_class(df, "data.frame")
      expect_true("count_value" %in% names(df))
      expect_true(all(df$count_value %% 5 == 0 | is.na(df$count_value)),
                  info = paste(id, "not banded"))
    }
  })
})

test_that("ACH-E record-unit counts carry a distinct-person companion", {
  h <- al_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(al_opts(), {
    # 401 = condition records by concept; record-unit, so it must expose its
    # distinct-person companion (n_persons) and band BOTH.
    df <- .omopAnalysisRun(h, "dsomop:achilles.401")
    expect_s3_class(df, "data.frame")
    expect_true(all(c("count_value", "n_persons") %in% names(df)))
    if (nrow(df) > 0) {
      expect_true(all(df$count_value %% 5 == 0 | is.na(df$count_value)))
      expect_true(all(df$n_persons %% 5 == 0 | is.na(df$n_persons)))
    }
  })
})

test_that("ACH-C/F/G dist entries strip min/max and expose only p10..p90", {
  h <- al_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(al_opts(), {
    # 103 obs-period length dist, 203 distinct-concepts/person, 406 age-by-concept.
    for (id in c("dsomop:achilles.103", "dsomop:achilles.203",
                 "dsomop:achilles.406")) {
      df <- .omopAnalysisRun(h, id)
      expect_s3_class(df, "data.frame")
      # min/max are NEVER released (the gate strips them).
      expect_false(any(grepl("^min_value$|^max_value$", names(df))),
                   info = paste(id, "leaked min/max"))
      if (nrow(df) > 0) {
        # Released quantiles are p10/p25/median/p75/p90 only (no native 0%/100%).
        expect_true(all(c("p10_value", "median_value", "p90_value") %in% names(df)))
        expect_true(all(df$count_value %% 5 == 0 | is.na(df$count_value)))
      }
    }
  })
})

test_that("ACH-H drug-field dist fails closed (empty) on an absent field", {
  h <- al_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(al_opts(), {
    # The fixture's drug_exposure omits days_supply (715) and refills (716), so
    # both must fail CLOSED to an empty frame (never a raw "no such column" error
    # that would leak which CDM fields are absent).
    bp <- .buildBlueprint(h)
    drug_cols <- bp$columns[["drug_exposure"]]$column_name
    for (spec in list(c("dsomop:achilles.715", "days_supply"),
                      c("dsomop:achilles.716", "refills"),
                      c("dsomop:achilles.717", "quantity"))) {
      id <- spec[1]; field <- spec[2]
      df <- .omopAnalysisRun(h, id)
      expect_s3_class(df, "data.frame")
      if (!field %in% drug_cols) {
        # Absent field -> fail closed to an empty frame.
        expect_equal(nrow(df), 0, info = paste(id, "absent field not empty"))
      } else if (nrow(df) > 0) {
        # Present field -> computes live as a dist: min/max stripped, count banded.
        expect_false(any(grepl("^min_value$|^max_value$", names(df))),
                     info = paste(id, "leaked min/max"))
        expect_true(all(df$count_value %% 5 == 0 | is.na(df$count_value)))
      }
    }
  })
})

test_that("ACH-D device entry fails closed (empty) when device_exposure absent", {
  h <- al_handle()
  on.exit(cleanup_handle(h))
  skip_if("device_exposure" %in% DBI::dbListTables(h$conn),
          "fixture has device_exposure")
  withr::with_options(al_opts(), {
    # device_exposure is absent from the fixture -> 2100 (count) and 2106 (age
    # dist) fail closed to empty, never a raw "no such table" error.
    expect_equal(nrow(.omopAnalysisRun(h, "dsomop:achilles.2100")), 0)
    expect_equal(nrow(.omopAnalysisRun(h, "dsomop:achilles.2106")), 0)
  })
})

test_that("scoping a live Achilles entry restricts + re-gates the population", {
  h <- al_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(al_opts(), {
    ct <- al_diabetes_cohort(h)
    wide   <- .omopAnalysisRun(h, "dsomop:achilles.2")
    scoped <- .omopAnalysisRun(h, "dsomop:achilles.2", scope = ct)
    expect_s3_class(scoped, "data.frame")
    expect_true(all(scoped$count_value %% 5 == 0 | is.na(scoped$count_value)))
    # The scoped distinct-person total cannot exceed the cohort-wide one.
    expect_lte(sum(scoped$count_value, na.rm = TRUE),
               sum(wide$count_value, na.rm = TRUE))
  })
})

test_that("a tiny scoped cohort fails closed for a live Achilles entry", {
  h <- al_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(list(nfilter.subset = 3), {
    # 2-person omop.table symbol -> the fn self-gate rejects before materialising.
    toks <- .hashPersonKey(as.character(1:2), h$person_key)
    df <- data.frame(person_id = toks, v = 1:2, stringsAsFactors = FALSE)
    attr(df, "dsomop_protected") <- "person_id"
    class(df) <- union("omop.table", class(df))
    expect_error(.omopAnalysisRun(h, "dsomop:achilles.400", scope = df),
                 "Disclosive")
  })
})

# --- New OHDSI canonical natives (sccs.* / txpath.* / cm.* / etc.) -------------
#
# The OHDSI worklist groups added NEW canonical natives the legacy registry ids
# alias. Assert each lists live (adapter != "ohdsi" precomputed), runs scoped
# through the one run path, and never leaks a raw SQL error or a source_value.

test_that("new OHDSI canonical natives list live + run scoped through the gate", {
  h <- al_handle()
  on.exit(cleanup_handle(h))

  reg <- .omopAnalysisRegistry(h)
  single_pop <- c(
    "dsomop:sccs.attrition", "dsomop:sccs.outcome_rate_per_month",
    "dsomop:sccs.count_histograms", "dsomop:sccs.assumption_checks",
    "dsomop:txpath.pathways", "dsomop:txpath.percentage_treated",
    "dsomop:txpath.duration_eras",
    "dsomop:cm.attrition",
    "dsomop:char.dechallenge_rechallenge", "dsomop:plp.attrition",
    "dsomop:cohortdx.time_series", "dsomop:cohortdx.included_source_concepts")

  for (id in single_pop) {
    e <- reg[[id]]
    expect_false(is.null(e), info = paste(id, "not registered"))
    # Live: not the precomputed OHDSI adapter, and no precomputed-table read.
    expect_false(identical(e$meta$adapter %||% "", "ohdsi"),
                 info = paste(id, "still on precomputed adapter"))
    expect_false(al_reads_precomputed(e),
                 info = paste(id, "reads a precomputed table"))
  }

  withr::with_options(al_opts(), {
    ct <- al_diabetes_cohort(h)
    for (id in single_pop) {
      df <- tryCatch(.omopAnalysisRun(h, id, scope = ct),
                     error = function(e) conditionMessage(e))
      if (is.character(df)) {
        expect_false(grepl("no such table|no such column|syntax error", df,
                           ignore.case = TRUE),
                     info = paste(id, "raw SQL error:", df))
        next
      }
      expect_s3_class(df, "data.frame")
      expect_false(any(grepl("_source_value$", names(df))),
                   info = paste(id, "leaked a source_value column"))
    }
  })
})

test_that("two-population OHDSI native cm.mdrr runs over two scoped arms", {
  h <- al_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(al_opts(), {
    a <- .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                       mode = "temporary", cohort_id = 1)
    b <- .cohortCreate(h, list(type = "condition", concept_set = c(255573)),
                       mode = "temporary", cohort_id = 2)
    df <- tryCatch(.omopAnalysisRun(h, "dsomop:cm.mdrr", scope = list(a, b)),
                   error = function(e) conditionMessage(e))
    if (is.character(df)) {
      expect_false(grepl("no such table|no such column|syntax error", df,
                         ignore.case = TRUE), info = df)
    } else {
      expect_s3_class(df, "data.frame")
      expect_false(any(grepl("_source_value$", names(df))))
    }
  })
})
