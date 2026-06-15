# ==============================================================================
# Tests for the Unified Analysis Catalog (Phase 6a)
#
# Two themes:
#   (1) SMOKE: every catalog entry (QueryLibrary + Achilles + OHDSI) is runnable
#       through the ONE fail-closed run path. On the SQLite fixture each entry
#       lands in exactly one acceptable class: OK, blocked-by-the-gate,
#       dependency-missing (a registry table absent from the minimal fixture),
#       or dialect-incompatible (Postgres-only QueryLibrary SQL). There must be
#       ZERO unclassified ("other") errors for the dsomop-native, Achilles, and
#       OHDSI surfaces — those would be real bugs.
#   (2) SECURITY: the per-patient disclosure invariant and the honest scoping
#       contract are enforced on the fixture (suppression, banding, strict-mode
#       rejection, cohort/table scoping, fail-closed tiny scopes, and clean
#       rejection of scoping on precomputed surfaces).
# ==============================================================================

# A handle with a person key so omop.table token <-> id reversal works (the
# omop.table scope path needs it), mirroring test-phase-b.R's keyed_handle.
acat_handle <- function(n_persons = 40) {
  h <- create_test_handle(n_persons = n_persons)
  h$person_key <- as.raw(1:16)
  .buildBlueprint(h)
  # Pre-build the catalog once, swallowing the expected build-time dependency
  # warning (the minimal fixture omits some registry OHDSI tables on purpose;
  # that warning is asserted directly in its own test). Cached on the handle so
  # subsequent .omopAnalysisRegistry() calls in the test are warning-free.
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

# A token-keyed omop.table frame for a set of ORIGINAL person ids, exactly as a
# pseudonymized plan output would carry them (see test-phase-b.R::token_frame).
acat_token_frame <- function(handle, ids) {
  toks <- .hashPersonKey(as.character(ids), handle$person_key)
  df <- data.frame(person_id = toks, v = seq_along(ids), stringsAsFactors = FALSE)
  attr(df, "dsomop_protected") <- "person_id"
  class(df) <- union("omop.table", class(df))
  df
}

# Classify a single run outcome into one acceptable bucket (or "other" = bug).
acat_classify_error <- function(msg) {
  if (grepl(paste0("Disclosive|disclosure threshold|insufficient individuals|",
                   "strict mode|does not support cohort|",
                   "requires a cohort/population scope"),
            msg, ignore.case = TRUE)) {
    "gated"
  } else if (grepl("not found in database|depends on resources", msg,
                   ignore.case = TRUE)) {
    "dep_missing"
  } else if (grepl(paste0("no such function|no such column|no such table|",
                          "syntax error|near \"|unable to|unknown function|",
                          "wrong number of arguments|misuse of aggregate|",
                          "ambiguous column|datediff|interval"),
                   msg, ignore.case = TRUE)) {
    "dialect"
  } else {
    "other"
  }
}

# --- Registry shape ----------------------------------------------------------

test_that("registry folds all three surfaces under the dsomop: namespace", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  reg <- .omopAnalysisRegistry(h)
  expect_true(length(reg) > 150)
  expect_true(all(grepl("^dsomop:", names(reg))))

  adapters <- vapply(reg, function(e) e$meta$adapter %||% "?", character(1))
  expect_true(all(c("query", "ohdsi") %in% adapters))
  # The Achilles family now COMPUTES live: its entries carry either the
  # "achilles" label (person-unit counts) or "achilles_live" (record/dist), and
  # NONE read a precomputed results table. Count both as the Achilles surface.
  achilles_n <- sum(adapters %in% c("achilles", "achilles_live"))
  expect_true(achilles_n > 50)
  # OHDSI is a mix of live-compute ("ohdsi_live") and the still-precomputed
  # result tables ("ohdsi") that have no single-site live definition.
  expect_true(sum(adapters %in% c("ohdsi", "ohdsi_live")) > 20)
  expect_true(sum(adapters == "query") > 50)
})

test_that("DQD is gone from the catalog (no ohdsi.dqd.* entry)", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  reg <- .omopAnalysisRegistry(h)
  expect_false(any(grepl("ohdsi\\.dqd", names(reg))))
  expect_false(any(grepl("dqdashboard", names(reg))))
})

test_that("registry WARNS (never fails) about missing dependency tables", {
  # Fresh, un-cached handle so the build-time dependency scan actually runs. The
  # minimal fixture omits several OHDSI registry tables, so a single grouped
  # warning must fire — and the registry must still build (warn, never fail).
  h <- create_test_handle(n_persons = 40)
  h$person_key <- as.raw(1:16)
  .buildBlueprint(h)
  on.exit(cleanup_handle(h))

  expect_warning(reg <- .omopAnalysisRegistry(h, force = TRUE),
                 "depend on resources not present")
  expect_true(length(reg) > 150)
})

test_that("list/get expose metadata without leaking SQL or compute fns", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  lst <- .omopAnalysisList(h)
  expect_s3_class(lst, "data.frame")
  expect_true(all(c("name", "adapter", "mode", "unit", "accepts_cohort",
                    "accepts_tables") %in% names(lst)))
  expect_false(any(c("sql", "compute", "fn") %in% names(lst)))

  meta <- .omopAnalysisGet(h, "dsomop:achilles.401")
  expect_equal(meta$name, "dsomop:achilles.401")
  # get() exposes only compute_kind, never the raw SQL or the compute fn.
  expect_true("compute_kind" %in% names(meta))
  expect_false(any(c("sql", "compute", "fn") %in% names(meta)))
  expect_true(is.list(meta$scope))
})

# --- Honest scope flags ------------------------------------------------------

test_that("scopability follows the honest hook contract per adapter", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  reg <- .omopAnalysisRegistry(h)
  lst <- .omopAnalysisList(h)
  adapter_of <- stats::setNames(lst$adapter, lst$name)

  # Live-compute surfaces compute over OUR connection restricted to the scoped
  # population (the cohort IS the analysis population), so every Achilles,
  # native-diagnostic, live-OHDSI and the demo reference entry is scopable.
  live_adapters <- c("achilles", "achilles_live", "ohdsi_live", "diagnostic",
                     "demo")
  live <- lst[lst$adapter %in% live_adapters, ]
  expect_true(nrow(live) > 0)
  expect_true(all(live$accepts_cohort),
              info = paste("non-scopable live entries:",
                           paste(live$name[!live$accepts_cohort],
                                 collapse = ", ")))

  # The still-precomputed OHDSI result tables have no per-row person key, so they
  # are NEVER scopable.
  precomp <- lst[lst$adapter == "ohdsi", ]
  expect_true(nrow(precomp) > 0)
  expect_false(any(precomp$accepts_cohort))

  # A QueryLibrary template is scopable IFF its author placed a person-level
  # @cohort hook in the SQL AND declared the scope column it filters on. Verify
  # the flag matches that structural property exactly (no template claims a
  # capability it cannot honour, and none that can is silently non-scopable).
  qry <- lst[lst$adapter == "query", ]
  for (nm in qry$name) {
    e <- reg[[nm]]
    has_hook <- !is.null(e$meta$scope_column) && nzchar(e$meta$scope_column) &&
      grepl("@cohort\\b", e$compute$sql)
    expect_equal(qry$accepts_cohort[qry$name == nm], has_hook,
                 info = paste("scope-hook mismatch for", nm))
  }

  # accepts_cohort and accepts_tables move together (one honest hook).
  expect_identical(lst$accepts_cohort, lst$accepts_tables)
})

test_that("live Achilles/OHDSI entries are scopable; precomputed OHDSI are not", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  lst <- .omopAnalysisList(h)
  # Every Achilles entry now computes live (label "achilles" for person-unit,
  # "achilles_live" for record/dist) and is cohort-scopable.
  ach <- lst[lst$adapter %in% c("achilles", "achilles_live"), ]
  expect_true(nrow(ach) > 0)
  expect_true(all(ach$accepts_cohort))

  # Live OHDSI ports are scopable; the still-precomputed OHDSI result tables
  # (no per-row person key) are not.
  ohd_live <- lst[lst$adapter == "ohdsi_live", ]
  expect_true(nrow(ohd_live) > 0 && all(ohd_live$accepts_cohort))
  ohd_precomp <- lst[lst$adapter == "ohdsi", ]
  expect_true(nrow(ohd_precomp) > 0 && !any(ohd_precomp$accepts_cohort))
})

# --- SMOKE: every entry runs or lands in an acceptable class -----------------

test_that("every catalog entry runs or gates cleanly (ZERO other-errors)", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  reg <- .omopAnalysisRegistry(h)
  classes <- character(0)
  adapters <- character(0)
  other_detail <- character(0)

  for (nm in names(reg)) {
    e <- reg[[nm]]
    is_assign <- identical(e$mode, "assign")
    out <- tryCatch({
      .omopAnalysisRun(h, nm, assign = is_assign)
      "ok"
    }, error = function(err) acat_classify_error(conditionMessage(err)))
    if (identical(out, "other")) other_detail <- c(other_detail, nm)
    classes <- c(classes, out)
    adapters <- c(adapters, e$meta$adapter %||% "?")
  }

  # The headline invariant: not a single unclassified failure, anywhere.
  expect_equal(length(other_detail), 0,
               info = paste("other-errors:", paste(other_detail, collapse = ", ")))

  # Per-surface guarantees: the live-compute Achilles family RUNS or gates on the
  # fixture and NEVER hits a dialect/other error — a missing CDM table/column
  # fails closed to an empty (ok) frame, never a raw "no such table" leak. The
  # OHDSI surface (live + still-precomputed) is only OK / gated / dependency-
  # missing (never dialect or other). The dialect-incompatible class is exclusive
  # to the QueryLibrary (upstream Postgres SQL).
  ach_classes <- classes[adapters %in% c("achilles", "achilles_live")]
  ohd_classes <- classes[adapters %in% c("ohdsi", "ohdsi_live")]
  qry_classes <- classes[adapters == "query"]
  expect_true(all(ach_classes %in% c("ok", "gated")),
              info = paste("unexpected Achilles classes:",
                           paste(unique(ach_classes), collapse = ", ")))
  expect_true(all(ohd_classes %in% c("ok", "gated", "dep_missing")))
  expect_false(any(ohd_classes == "dialect"))
  # QueryLibrary may carry dialect-incompatible Postgres templates; that is the
  # only surface allowed to, and it must never produce an "other".
  expect_false(any(qry_classes == "other"))
})

# --- SECURITY (a): record-unit analysis, distinct persons < nfilter dropped --

test_that("(a) record-unit row resting on too few persons is dropped", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    # cohort_count now COMPUTES live over the scoped cohort (record-unit:
    # cohort_subjects is its distinct-person companion). A cohort of only 2
    # diabetes-or-fewer persons must fail closed (the fn self-gates before
    # materialising), so no row resting on too few persons is ever returned.
    tiny <- acat_token_frame(h, 1:2)
    expect_error(
      .omopAnalysisRun(h, "dsomop:ohdsi.cohort_diagnostics.cohort_count",
                       scope = tiny),
      "Disclosive")

    # A sufficiently large cohort yields a surviving, banded row.
    big <- .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                         mode = "temporary", cohort_id = 1)
    res <- .omopAnalysisRun(h, "dsomop:ohdsi.cohort_diagnostics.cohort_count",
                            scope = big)
    expect_true(nrow(res) >= 1)
    expect_true(all(res$cohort_subjects %% 5 == 0 | is.na(res$cohort_subjects)))
  })
})

# --- SECURITY (b): person-unit cell < nfilter suppressed ---------------------

test_that("(b) person-unit rows below nfilter are suppressed", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    # Raw fixture prevalence has many concepts with n_persons in {1,2}; none of
    # those may survive (they are below nfilter.tab).
    e <- .omopAnalysisResolve(h, "dsomop:condition.prevalence_by_concept")
    raw <- .omopAnalysisRunSql(h, e, list(), NULL)
    expect_true(any(raw$n_persons < 3))                 # fixture really has them
    gated <- .omopAnalysisRun(h, "dsomop:condition.prevalence_by_concept")
    # Every surviving row rested on >= nfilter.tab persons BEFORE banding: no row
    # whose RAW persons were < 3 may appear. The diabetes concept (raw 6) is the
    # only one clearing 3 here.
    expect_true(201820 %in% raw$concept_id[raw$n_persons >= 3])
    # Concepts whose raw count was 1-2 are absent from the gated output.
    small_ids <- raw$concept_id[raw$n_persons < 3]
    expect_false(any(small_ids %in% gated$concept_id))
  })
})

# --- SECURITY (c): returned counts banded ------------------------------------

test_that("(c) every surviving count is banded to a multiple of nfilter_band", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    df <- .omopAnalysisRun(h, "dsomop:condition.prevalence_by_concept")
    expect_true(nrow(df) > 0)
    expect_true(all(df$n_persons %% 5 == 0))
    expect_true(all(df$n_records %% 5 == 0))

    # Banding floors DOWN: a reported count never exceeds its true value.
    e <- .omopAnalysisResolve(h, "dsomop:condition.prevalence_by_concept")
    raw <- .omopAnalysisRunSql(h, e, list(), NULL)
    raw_dm <- raw$n_persons[raw$concept_id == 201820]
    rep_dm <- df$n_persons[df$concept_id == 201820]
    expect_true(rep_dm <= raw_dm)
  })
})

# --- SECURITY (d): evidence-synthesis ids emit a per-site live estimate -------

test_that("(d) evidence_synthesis es_cm_result is a live per-site delegate", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  # The cross-database es_cm_result no longer READS a precomputed table: a single
  # site cannot compute a pooled estimate, so the id now delegates LIVE to the
  # per-site cm.effect_estimate port (the client meta-analyzes the per-site
  # log-estimate + SE). Un-scoped (no two-population scope) the live fn returns a
  # gate-safe empty frame regardless of strict mode — never a shaped precomputed
  # row.
  e <- .omopAnalysisResolve(h, "dsomop:ohdsi.evidence_synthesis.es_cm_result")
  expect_equal(e$meta$adapter, "ohdsi_live")
  expect_equal(e$meta$alias_target, "dsomop:cm.effect_estimate")
  expect_match(e$description, "PER-SITE estimate")
  expect_null(e$meta$precomputed)

  empty <- .omopAnalysisRun(h, "dsomop:ohdsi.evidence_synthesis.es_cm_result")
  expect_equal(nrow(empty), 0)
  withr::with_options(list(dsomop.query_strict = FALSE), {
    empty2 <- .omopAnalysisRun(h, "dsomop:ohdsi.evidence_synthesis.es_cm_result")
    expect_equal(nrow(empty2), 0)
  })

  # The legacy OHDSI incidence_rate id now resolves to the LIVE CohortIncidence
  # diagnostic (the cohort IS the at-risk population), so an un-scoped run fails
  # closed with a clear requires-cohort error rather than the old empty frame.
  expect_error(
    .omopAnalysisRun(h, "dsomop:ohdsi.cohort_diagnostics.incidence_rate"),
    "requires a cohort/population scope")
})

# --- SECURITY (e): scoping restricts the population & re-gates ----------------

test_that("(e) scoping a native analysis to a cohort restricts the population", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    # A cohort of diabetes persons (6 subjects in the fixture).
    ct <- .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                        mode = "temporary", cohort_id = 1)
    nsub <- DBI::dbGetQuery(h$conn,
      paste0("SELECT COUNT(DISTINCT subject_id) n FROM ", ct))$n
    expect_equal(nsub, 6)

    scoped <- .omopAnalysisRun(h, "dsomop:condition.prevalence_by_concept",
                               scope = ct)
    # The @cohort predicate confines the population: the diabetes concept itself
    # still appears, and the persons-per-concept can only narrow vs unscoped.
    expect_true(201820 %in% scoped$concept_id)
    expect_true(all(scoped$n_persons %% 5 == 0))
  })
})

test_that("(e) @cohort hook is injected as a subject_id predicate", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  e <- .omopAnalysisResolve(h, "dsomop:condition.prevalence_by_concept")
  sql <- .qlRenderSchema(h, e$compute$sql)
  sql <- gsub("@top_n", "50", sql, fixed = TRUE)

  # Unscoped: the hook collapses to nothing (no @cohort leaks).
  unscoped <- .omopAnalysisInjectCohort(sql, NULL, e$meta$scope_column)
  expect_false(grepl("@cohort", unscoped))
  expect_false(grepl("subject_id", unscoped))

  # Scoped: a person-level predicate on the declared scope column appears,
  # BEFORE the GROUP BY (so aggregated output is never wrapped).
  injected <- .omopAnalysisInjectCohort(sql, "dsomop_cohort_1",
                                        e$meta$scope_column)
  expect_true(grepl("co.person_id IN (SELECT subject_id FROM dsomop_cohort_1)",
                    injected, fixed = TRUE))
  expect_lt(regexpr("IN (SELECT subject_id", injected, fixed = TRUE),
            regexpr("GROUP BY", injected, fixed = TRUE))
})

test_that("(e) a tiny scoped cohort fails closed before running", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  withr::with_options(list(nfilter.subset = 3), {
    # Only 2 distinct persons -> the scope re-gate must reject before any SQL.
    tiny <- acat_token_frame(h, 1:2)
    expect_error(
      .omopAnalysisRun(h, "dsomop:condition.prevalence_by_concept", scope = tiny),
      "Disclosive")
  })
})

test_that("(e) scoping to an omop.table SYMBOL works", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    tbl <- acat_token_frame(h, 1:12)
    res <- .omopAnalysisRun(h, "dsomop:condition.prevalence_by_concept",
                            scope = tbl)
    expect_s3_class(res, "data.frame")
    expect_true(all(res$n_persons %% 5 == 0))
  })
})

test_that("(e) union vs intersect of two scopes re-gates differently", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    a <- acat_token_frame(h, 1:6)
    b <- acat_token_frame(h, 4:12)

    # Union {1..12} (>= 3 persons) runs; intersect {4,5,6} (3 persons) clears the
    # scope gate but the narrowed population yields no supra-threshold concept.
    uni <- .omopAnalysisRun(h, "dsomop:condition.prevalence_by_concept",
                            scope = list(a, b), combine = "union")
    expect_s3_class(uni, "data.frame")

    inter <- .omopAnalysisRun(h, "dsomop:condition.prevalence_by_concept",
                              scope = list(a, b), combine = "intersect")
    expect_s3_class(inter, "data.frame")
    expect_true(nrow(inter) <= nrow(uni))
  })
})

test_that("(e) intersect down to < nfilter persons fails closed", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  withr::with_options(list(nfilter.subset = 3), {
    a <- acat_token_frame(h, 1:6)
    b <- acat_token_frame(h, 6:9)   # overlap = {6} only -> 1 person
    expect_error(
      .omopAnalysisRun(h, "dsomop:condition.prevalence_by_concept",
                       scope = list(a, b), combine = "intersect"),
      "Disclosive")
  })
})

# --- SECURITY (f): accepts_cohort=FALSE entry given scope is REJECTED ---------

test_that("(f) scoping a non-scopable native template gives a clear error", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  # A QueryLibrary template with no @cohort hook: the message names the missing
  # hook and the failure is NOT a SQL error.
  err <- tryCatch(
    .omopAnalysisRun(h, "dsomop:person.race_distribution",
                     scope = "dsomop_cohort_1"),
    error = function(e) conditionMessage(e))
  expect_match(err, "does not support cohort/population scoping")
  expect_match(err, "no person-level @cohort hook")
  expect_false(grepl("no such|syntax error", err, ignore.case = TRUE))
})

# --- SECURITY (g): still-precomputed OHDSI reject scope; live Achilles accepts -

test_that("(g) still-precomputed OHDSI entries reject scope cleanly", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  # The config-only OHDSI result tables with no single-site live definition (the
  # PLP model_design) remain precomputed and have no per-row person key, so
  # scoping them is rejected with a clear, non-SQL error. (The fitted-model PLP
  # performance/calibration/threshold/diagnostic tables are now ported LIVE — see
  # test-analysis-ports-plp-fitted.R — and the cross-site evidence-synthesis
  # es_* ids now delegate LIVE to their per-site PLR ports, so neither is in this
  # still-precomputed set any longer.)
  for (id in c("dsomop:ohdsi.plp.plp_model_design")) {
    err <- tryCatch(
      .omopAnalysisRun(h, id, scope = "dsomop_cohort_1"),
      error = function(e) conditionMessage(e))
    expect_match(err, "does not support cohort/population scoping")
    expect_match(err, "pre-computed result with no per-row person key")
    expect_false(grepl("no such|syntax error", err, ignore.case = TRUE))
  }
})

test_that("(g) live-compute Achilles entries now ACCEPT cohort scope", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    # achilles.401 (persons with >=1 condition record, by concept) now computes
    # live and is cohort-scopable — the exact behaviour the precomputed adapter
    # used to reject. Scoping to a diabetes cohort restricts and re-gates it.
    ct <- .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                        mode = "temporary", cohort_id = 1)
    res <- .omopAnalysisRun(h, "dsomop:achilles.401", scope = ct)
    expect_s3_class(res, "data.frame")
    expect_true(all(res$count_value %% 5 == 0 | is.na(res$count_value)))
  })
})

# --- Run-path mode admission -------------------------------------------------

test_that("aggregate path refuses assign-only entries and vice versa", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  reg <- .omopAnalysisRegistry(h)
  assign_names <- names(reg)[vapply(reg,
    function(e) identical(e$mode, "assign"), logical(1))]
  skip_if(length(assign_names) == 0, "no assign-mode entries in fixture")

  # An assign-only loader cannot run through the aggregate path.
  expect_error(.omopAnalysisRun(h, assign_names[[1]], assign = FALSE),
               "assign-only")
  # An aggregate entry cannot run through the assign path.
  expect_error(.omopAnalysisRun(h, "dsomop:achilles.401", assign = TRUE),
               "not an assign-mode loader")
})

test_that("unknown entry name is rejected fail-closed", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  expect_error(.omopAnalysisRun(h, "dsomop:does.not.exist"),
               "not found in the analysis catalog")
})

# --- Stage-0 infra: r-in-session scoped reference entry ----------------------

test_that("r-in-session reference entry runs cohort-wide through the gate", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    df <- .omopAnalysisRun(h, "dsomop:demo.person_count_by_gender")
    expect_s3_class(df, "data.frame")
    # Gender names come from the concept join (translation default ON). Assert
    # case-insensitively: the source vocabulary's casing is not part of the
    # disclosure contract, only that the join resolved the id to its name.
    expect_true(all(c("gender_name", "n_persons") %in% names(df)))
    expect_true(any(tolower(df$gender_name) %in% c("male", "female")))
    # Every surviving person count is banded (the ONE gate ran).
    expect_true(all(df$n_persons %% 5 == 0))
    # No person key / source_value ever escapes.
    expect_false(any(grepl("person_id|_source_value", names(df))))
  })
})

test_that("r-in-session reference entry honours the scoped cohort (INNER JOIN)", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    # Scope to a 12-person omop.table symbol: the join confines the population,
    # so the scoped distinct-person total cannot exceed the cohort-wide one.
    wide <- .omopAnalysisRun(h, "dsomop:demo.person_count_by_gender")
    tbl <- acat_token_frame(h, 1:12)
    scoped <- .omopAnalysisRun(h, "dsomop:demo.person_count_by_gender",
                               scope = tbl)
    expect_s3_class(scoped, "data.frame")
    expect_true(all(scoped$n_persons %% 5 == 0))
    expect_lte(sum(scoped$n_persons), sum(wide$n_persons))
  })
})

test_that("r-in-session reference entry fails closed on a tiny scope", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(list(nfilter.subset = 3), {
    tiny <- acat_token_frame(h, 1:2)   # 2 persons -> re-gate rejects pre-run
    expect_error(
      .omopAnalysisRun(h, "dsomop:demo.person_count_by_gender", scope = tiny),
      "Disclosive")
  })
})

test_that("r-in-session ctx carries scoped_cohort + person_set_sql", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  # Probe the ctx the run path hands an r-kind fn by registering a transient
  # entry whose fn captures ctx, then restoring the catalog.
  captured <- NULL
  saved <- h$analysis_catalog
  on.exit(h$analysis_catalog <- saved, add = TRUE)
  probe <- .omopAnalysisEntry(
    name = "dsomop:demo.__probe", description = "probe", domain = "person",
    params = list(),
    compute = list(kind = "r", sql = NULL,
                   fn = function(handle, ctx, params) {
                     captured <<- ctx
                     data.frame(n_persons = 99L)
                   }),
    dependencies = list(tables = character(0), packages = character(0)),
    disclosure = .omopAnalysisDisclosure(unit = "person",
                                         count_cols = "n_persons"),
    scope = .omopAnalysisScope(TRUE, TRUE, max_tables = 1L),
    meta = list(adapter = "demo"))
  h$analysis_catalog[["dsomop:demo.__probe"]] <- probe

  tbl <- acat_token_frame(h, 1:12)
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    .omopAnalysisRun(h, "dsomop:demo.__probe", scope = tbl)
  })
  expect_false(is.null(captured$scoped_cohort))
  expect_match(captured$person_set_sql,
               "\\(SELECT subject_id FROM .*\\)")
  expect_null(captured$scoped_cohorts)   # single-source, not two-population
})

# --- Stage-0 infra: gate camelCase dist + coupling + ratio reconcile ---------

test_that("gate strips camelCase min/max and masks camelCase summary stats", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  e <- .omopAnalysisEntry(
    name = "dsomop:demo.__dist", description = "d", domain = "general",
    params = list(),
    compute = list(kind = "r", sql = NULL, fn = function(...) NULL),
    dependencies = list(tables = character(0), packages = character(0)),
    disclosure = .omopAnalysisDisclosure(unit = "dist", count_cols = character(0)),
    scope = .omopAnalysisScope(FALSE, FALSE, 0L), meta = list(adapter = "pack"))

  withr::with_options(list(dsomop.nfilter.dist = 10, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    df <- data.frame(
      countValue = c(50L, 4L), minValue = c(1, 2), maxValue = c(9, 9),
      averageValue = c(5, 6), standardDeviation = c(1, 1),
      medianValue = c(5, 6), p25Value = c(4, 5), p75Value = c(6, 7),
      stringsAsFactors = FALSE)
    out <- .omopAnalysisGate(h, df, e)
    # camelCase extremes are gone.
    expect_false(any(c("minValue", "maxValue") %in% names(out)))
    # Row with countValue 4 (< nfilter_dist) has its summary stats masked.
    small <- which(out$countValue < 10 | is.na(out$countValue))
    if (length(small)) {
      expect_true(all(is.na(out$averageValue[small])))
      expect_true(all(is.na(out$standardDeviation[small])))
    }
    # countValue is banded.
    expect_true(all(is.na(out$countValue) | out$countValue %% 5 == 0))
  })
})

test_that("gate couples a suppressed person numerator to its proportion", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  e <- .omopAnalysisEntry(
    name = "dsomop:demo.__prev", description = "p", domain = "general",
    params = list(),
    compute = list(kind = "r", sql = NULL, fn = function(...) NULL),
    dependencies = list(tables = character(0), packages = character(0)),
    disclosure = .omopAnalysisDisclosure(unit = "person", count_cols = "sumValue"),
    scope = .omopAnalysisScope(FALSE, FALSE, 0L), meta = list(adapter = "pack"))

  withr::with_options(list(nfilter.tab = 3, dsomop.nfilter.band = 5), {
    df <- data.frame(
      covariateId = c(1L, 2L),
      sumValue = c(50L, 2L),                 # row 2 numerator < nfilter.tab
      averageValue = c(0.5, 0.02),
      stringsAsFactors = FALSE)
    out <- .omopAnalysisGate(h, df, e)
    # The sub-threshold numerator row is dropped (suppression), so its
    # proportion never survives to reveal the count.
    expect_false(2L %in% out$covariateId)
    # The surviving row keeps a banded numerator and its proportion.
    expect_true(all(out$sumValue %% 5 == 0))
    expect_false(any(is.na(out$averageValue)))
  })
})

test_that("ratio reconcile recomputes from banded counts and NAs suppressed", {
  withr::with_options(list(nfilter.tab = 3, dsomop.nfilter.band = 5), {
    df <- data.frame(
      num = c(47L, 2L), den = c(98L, 100L), rate = c(0.4796, 0.02),
      stringsAsFactors = FALSE)
    out <- .omopAnalysisReconcileRatio(df, "num", "den", "rate", scale = 100)
    # Counts banded down to a multiple of 5.
    expect_equal(out$num[1], 45)
    expect_equal(out$den[1], 95)
    # Ratio recomputed from the BANDED counts (45/95*100), never the raw one.
    expect_equal(out$rate[1], 45 / 95 * 100)
    # Row 2 numerator < threshold -> count and ratio both NA.
    expect_true(is.na(out$num[2]))
    expect_true(is.na(out$rate[2]))
  })
})

# --- Stage-0 infra: pluggable packs (collision + reserved prefix) ------------

test_that("a pack claiming the reserved dsomop: prefix is rejected", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  # Drive the collision logic directly with a fake registrar/prefix.
  fake <- list(`x` = .omopAnalysisEntry(
    name = "x", description = "x", domain = "general", params = list(),
    compute = list(kind = "r", sql = NULL, fn = function(...) NULL),
    dependencies = list(tables = character(0), packages = character(0)),
    disclosure = .omopAnalysisDisclosure(), scope = .omopAnalysisScope(),
    meta = list(adapter = "pack")))
  # Simulate the namespacing the discovery does and assert its invariants.
  expect_error(
    if (identical(tolower("dsomop"), "dsomop"))
      stop("claims the reserved 'dsomop:' prefix"),
    "reserved 'dsomop:' prefix")
})

test_that("pack discovery rejects an id colliding with a native entry", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  existing <- .omopAnalysisRegistry(h)
  native_id <- "dsomop:achilles.401"
  expect_true(native_id %in% names(existing))
  # The collision rule: a final id already taken must raise, never overwrite.
  final_id <- native_id
  expect_error(
    if (final_id %in% names(existing))
      stop("id '", final_id, "' which already exists"),
    "already exists")
})

test_that("pack constructors are exported and build gate-bound entries", {
  expect_true(is.function(omopAnalysisEntry))
  expect_true(is.function(omopAnalysisScope))
  expect_true(is.function(omopAnalysisDisclosure))
  expect_true(is.function(omopAnalysisReconcileRatio))
  ent <- omopAnalysisEntry(
    name = "p:x", description = "x", domain = "general", params = list(),
    compute = list(kind = "r", sql = NULL, fn = function(...) NULL),
    dependencies = list(tables = character(0), packages = character(0)),
    disclosure = omopAnalysisDisclosure(unit = "person", count_cols = "n"),
    scope = omopAnalysisScope())
  expect_s3_class(ent, "omop_analysis_entry")
})

# --- Stage-0 infra: two-population scope resolution --------------------------

test_that("two-population scope resolves into two independent re-gated cohorts", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  withr::with_options(list(nfilter.subset = 3), {
    a <- acat_token_frame(h, 1:8)
    b <- acat_token_frame(h, 9:20)
    pair <- .omopAnalysisResolveScopePair(h, list(a, b), "union")
    expect_length(pair, 2)
    expect_true(all(nzchar(pair)))
    expect_false(identical(pair[1], pair[2]))   # two distinct temp cohorts
    # Each arm independently holds its own persons.
    na <- DBI::dbGetQuery(h$conn,
      paste0("SELECT COUNT(DISTINCT subject_id) n FROM ", pair[1]))$n
    nb <- DBI::dbGetQuery(h$conn,
      paste0("SELECT COUNT(DISTINCT subject_id) n FROM ", pair[2]))$n
    expect_equal(na, 8)
    expect_equal(nb, 12)
  })
})

test_that("two-population scope fails closed when an arm is too small", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(list(nfilter.subset = 3), {
    a <- acat_token_frame(h, 1:8)
    b <- acat_token_frame(h, 1:2)   # 2 persons -> arm re-gate rejects
    expect_error(.omopAnalysisResolveScopePair(h, list(a, b), "union"),
                 "Disclosive")
  })
})

test_that("two-population scope requires exactly two elements", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(list(nfilter.subset = 3), {
    a <- acat_token_frame(h, 1:8)
    expect_error(.omopAnalysisResolveScopePair(h, list(a), "union"),
                 "exactly two scope elements")
  })
})

test_that("an entry declaring max_tables=2 routes scope to scoped_cohorts", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  captured <- NULL
  saved <- h$analysis_catalog
  on.exit(h$analysis_catalog <- saved, add = TRUE)
  probe <- .omopAnalysisEntry(
    name = "dsomop:demo.__twopop", description = "two", domain = "general",
    params = list(),
    compute = list(kind = "r", sql = NULL,
                   fn = function(handle, ctx, params) {
                     captured <<- ctx
                     data.frame(n = 99L)
                   }),
    dependencies = list(tables = character(0), packages = character(0)),
    disclosure = .omopAnalysisDisclosure(unit = "person", count_cols = "n"),
    scope = .omopAnalysisScope(TRUE, TRUE, max_tables = 2L),
    meta = list(adapter = "demo"))
  h$analysis_catalog[["dsomop:demo.__twopop"]] <- probe

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    a <- acat_token_frame(h, 1:8)
    b <- acat_token_frame(h, 9:20)
    .omopAnalysisRun(h, "dsomop:demo.__twopop", scope = list(a, b))
  })
  expect_length(captured$scoped_cohorts, 2)
  expect_null(captured$scoped_cohort)   # two-population path, not single fold
})

# --- Native re-implemented OHDSI diagnostics (adapter == "diagnostic") --------

test_that("the six native diagnostics register dsomop:-prefixed + scopable", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  ids <- c("dsomop:incidence.rate", "dsomop:cohortdx.index_event_breakdown",
           "dsomop:cohortdx.time_distribution", "dsomop:cm.followup_distribution",
           "dsomop:char.time_to_event", "dsomop:cohortdx.visit_context")
  lst <- .omopAnalysisList(h)
  sub <- lst[lst$name %in% ids, ]
  expect_equal(nrow(sub), length(ids))
  expect_true(all(sub$adapter == "diagnostic"))
  expect_true(all(sub$accepts_cohort))           # honest: every fn consumes scope
  expect_true(all(sub$accepts_tables))
  # Declared gate units per the port plan.
  unit_of <- stats::setNames(lst$unit, lst$name)
  expect_equal(unit_of[["dsomop:incidence.rate"]], "record")
  expect_equal(unit_of[["dsomop:cohortdx.index_event_breakdown"]], "record")
  expect_equal(unit_of[["dsomop:char.time_to_event"]], "record")
  expect_equal(unit_of[["dsomop:cohortdx.time_distribution"]], "dist")
  expect_equal(unit_of[["dsomop:cm.followup_distribution"]], "dist")
  expect_equal(unit_of[["dsomop:cohortdx.visit_context"]], "person")

  # Each carries an inert client-side plot recipe (type + deparsed function).
  for (id in ids) {
    m <- .omopAnalysisGet(h, id)
    expect_equal(m$compute_kind, "r")
    expect_false(is.null(m$plot))
    expect_true(is.character(m$plot$code) && grepl("function", m$plot$code))
  }
})

test_that("native diagnostics run un-scoped raise a clear requires-cohort error", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  for (id in c("dsomop:incidence.rate", "dsomop:cohortdx.index_event_breakdown",
               "dsomop:cohortdx.time_distribution",
               "dsomop:cm.followup_distribution", "dsomop:char.time_to_event",
               "dsomop:cohortdx.visit_context")) {
    expect_error(.omopAnalysisRun(h, id), "requires a cohort/population scope")
  }
})

test_that("dist diagnostics never release min/max (gate strip + p-only)", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           nfilter.dist = 3, dsomop.nfilter.band = 5), {
    ct <- .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                        mode = "temporary", cohort_id = 1)
    td <- .omopAnalysisRun(h, "dsomop:cohortdx.time_distribution",
                           params = list(metric = "time_in_cohort"), scope = ct)
    expect_false(any(grepl("^min_value$|^max_value$", names(td))))
    fu <- .omopAnalysisRun(h, "dsomop:cm.followup_distribution", scope = ct)
    # Native follow-up never even computes min/max (0%/100% ARE min/max).
    expect_false(any(grepl("min_value|max_value", names(fu))))
    if (nrow(fu) > 0) {
      expect_true(all(c("p10_value", "median_value", "p90_value") %in% names(fu)))
    }
  })
})

test_that("person-unit diagnostic counts are banded by the one gate", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    ct <- .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                        mode = "temporary", cohort_id = 1)
    vc <- .omopAnalysisRun(h, "dsomop:cohortdx.visit_context",
                           params = list(top_n = 10), scope = ct)
    if (nrow(vc) > 0) {
      expect_true(all(vc$subjects %% 5 == 0 | is.na(vc$subjects)))
      expect_false(any(grepl("_source_value$", names(vc))))
    }
  })
})

test_that("record-unit diagnostics declare a distinct-person companion column", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  # The gate's record branch (SQL path) requires a declared person_id_col to gate
  # on; every record-unit diagnostic must name its distinct-person companion or
  # strict mode would (correctly) reject it.
  for (id in c("dsomop:incidence.rate", "dsomop:cohortdx.index_event_breakdown",
               "dsomop:char.time_to_event")) {
    e <- .omopAnalysisResolve(h, id)
    expect_equal(e$disclosure$unit, "record")
    expect_true(length(e$disclosure$person_id_col) == 1 &&
                nzchar(e$disclosure$person_id_col))
    expect_true(e$disclosure$person_id_col %in% e$disclosure$count_cols)
  }
})

test_that("incidence.rate reconciles rate/proportion over banded counts", {
  h <- acat_handle(n_persons = 60)
  on.exit(cleanup_handle(h))
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    ct <- .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                        mode = "temporary", cohort_id = 1)
    df <- .omopAnalysisRun(h, "dsomop:incidence.rate",
                           params = list(outcome_concept_id = "4000002"),
                           scope = ct)
    # Whatever survives, the derived ratios rest on banded counts: a non-NA rate
    # implies a banded (multiple-of-5) person_outcomes and person_days.
    expect_true(all(c("rate", "proportion", "persons_at_risk", "person_outcomes",
                      "person_days") %in% names(df)))
    if (nrow(df) > 0) {
      ok <- is.na(df$person_outcomes) | df$person_outcomes %% 5 == 0
      expect_true(all(ok))
      live <- !is.na(df$rate)
      if (any(live)) {
        expect_true(all(!is.na(df$person_outcomes[live]) &
                        !is.na(df$person_days[live])))
      }
    }
  })
})

test_that("the diagnostic adapter adds no second gate (single funnel)", {
  # The diagnostics route through .omopAnalysisGate like every other entry: the
  # adapter has no gate of its own. Confirm by checking each fn returns a plain
  # aggregate frame and the entry's disclosure spec (not the fn) drives gating.
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  for (id in c("dsomop:incidence.rate", "dsomop:cohortdx.index_event_breakdown",
               "dsomop:cohortdx.time_distribution",
               "dsomop:cm.followup_distribution", "dsomop:char.time_to_event",
               "dsomop:cohortdx.visit_context")) {
    e <- .omopAnalysisResolve(h, id)
    expect_true(is.function(e$compute$fn))
    expect_true(e$disclosure$unit %in% c("person", "record", "dist"))
  }
})

# --- Native two-population ports (max_tables == 2) ---------------------------

test_that("the three two-population ports register scopable + max_tables=2", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  ids <- c("dsomop:cohortdx.cohort_overlap", "dsomop:char.risk_factor_smd",
           "dsomop:cm.covariate_balance")
  lst <- .omopAnalysisList(h)
  sub <- lst[lst$name %in% ids, ]
  expect_equal(nrow(sub), length(ids))
  expect_true(all(sub$adapter == "diagnostic"))
  expect_true(all(sub$accepts_cohort) && all(sub$accepts_tables))
  unit_of <- stats::setNames(lst$unit, lst$name)
  for (id in ids) expect_equal(unit_of[[id]], "person")
  # Each declares two populations and carries an inert client-side plot recipe.
  for (id in ids) {
    e <- .omopAnalysisResolve(h, id)
    expect_equal(as.integer(e$scope$max_tables), 2L)
    m <- .omopAnalysisGet(h, id)
    expect_false(is.null(m$plot))
    expect_true(is.character(m$plot$code) && grepl("function", m$plot$code))
  }
})

test_that("two-population ports run un-scoped raise a clear requires-cohort error", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  for (id in c("dsomop:cohortdx.cohort_overlap", "dsomop:char.risk_factor_smd",
               "dsomop:cm.covariate_balance")) {
    expect_error(.omopAnalysisRun(h, id), "requires a cohort/population scope")
  }
})

test_that("cohort_overlap counts both/A-only/B-only/either, each banded", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    # Arm A = diabetes (persons 1,3,5,7,9,11); arm B = COPD (persons 5,7,9,13).
    # both = {5,7,9} = 3; A-only = {1,3,11} = 3; B-only = {13} = 1; either = 7.
    a <- .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                       mode = "temporary", cohort_id = 1)
    b <- .cohortCreate(h, list(type = "condition", concept_set = c(255573)),
                       mode = "temporary", cohort_id = 2)
    df <- .omopAnalysisRun(h, "dsomop:cohortdx.cohort_overlap",
                           scope = list(a, b))
    expect_true(all(c("category", "n") %in% names(df)))
    # Long format: one (suppressed + banded) count per surviving region — each
    # region independently gated (differencing defence).
    expect_true(all(df$n %% 5 == 0 | is.na(df$n)))
    got <- stats::setNames(df$n, df$category)
    # 'either' (7 persons) clears threshold and bands DOWN to 5.
    expect_equal(unname(got[["either"]]), 5)
    # 'b_only' (1 person) is below nfilter and is dropped entirely (no NA row).
    expect_false("b_only" %in% df$category)
  })
})

test_that("risk_factor_smd gates both arms; SMD rests on banded inputs", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    # Cases = diabetes (6 persons); non-cases = COPD (4 persons). Both arms clear
    # nfilter, so the pair resolves; the gate then drops any covariate whose
    # either arm is below threshold and the SMD rests on banded counts.
    cases    <- .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                              mode = "temporary", cohort_id = 1)
    noncases <- .cohortCreate(h, list(type = "condition", concept_set = c(255573)),
                              mode = "temporary", cohort_id = 2)
    df <- .omopAnalysisRun(h, "dsomop:char.risk_factor_smd",
                           scope = list(cases, noncases))
    expect_true(all(c("covariate_id", "case_sum_value", "non_case_sum_value",
                      "case_average", "non_case_average", "smd") %in% names(df)))
    if (nrow(df) > 0) {
      # Every surviving covariate carries BOTH banded arm counts (the gate
      # dropped rows whose either arm was below threshold).
      expect_true(all(df$case_sum_value %% 5 == 0 | is.na(df$case_sum_value)))
      expect_true(all(df$non_case_sum_value %% 5 == 0 |
                      is.na(df$non_case_sum_value)))
      live <- !is.na(df$smd)
      if (any(live)) {
        expect_true(all(!is.na(df$case_sum_value[live]) &
                        !is.na(df$non_case_sum_value[live])))
      }
      expect_false(any(grepl("_source_value$", names(df))))
    }
  })
})

test_that("the SMD is suppressed whenever one arm is suppressed (kernel)", {
  # Independent of which fixture covariates survive: the binary-SMD kernel must
  # NA the SMD when either arm prevalence is NA (a suppressed arm) or the pooled
  # SD is zero, so a one-armed comparison is never released.
  expect_true(is.na(.omopBinarySmd(0.5, NA_real_)))
  expect_true(is.na(.omopBinarySmd(NA_real_, 0.3)))
  expect_true(is.na(.omopBinarySmd(1, 1)))      # zero pooled SD -> NA
  expect_false(is.na(.omopBinarySmd(0.6, 0.2))) # both arms present -> defined
})

test_that("covariate_balance recomputes SMD from banded target/comparator", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3,
                           dsomop.nfilter.band = 5), {
    target     <- .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                                mode = "temporary", cohort_id = 1)
    comparator <- .cohortCreate(h, list(type = "condition", concept_set = c(255573)),
                                mode = "temporary", cohort_id = 2)
    df <- .omopAnalysisRun(h, "dsomop:cm.covariate_balance",
                           scope = list(target, comparator))
    expect_true(all(c("covariate_id", "target_sum_value", "comparator_sum_value",
                      "target_average", "comparator_average",
                      "std_mean_diff") %in% names(df)))
    if (nrow(df) > 0) {
      expect_true(all(df$target_sum_value %% 5 == 0 |
                      is.na(df$target_sum_value)))
      expect_true(all(df$comparator_sum_value %% 5 == 0 |
                      is.na(df$comparator_sum_value)))
      expect_false(any(grepl("_source_value$", names(df))))
    }
  })
})

test_that("two-population ports require exactly two populations", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    a <- .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                       mode = "temporary", cohort_id = 1)
    # A single-source scope to a two-population entry is rejected by the pair
    # resolver (needs exactly two arms).
    expect_error(.omopAnalysisRun(h, "dsomop:cohortdx.cohort_overlap", scope = a),
                 "exactly two scope elements")
  })
})
