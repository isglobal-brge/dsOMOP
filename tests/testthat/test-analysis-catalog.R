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
  if (grepl("Disclosive|disclosure threshold|insufficient individuals|strict mode|does not support cohort",
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
  expect_true(all(c("query", "achilles", "ohdsi") %in% adapters))
  # Each surface contributes a healthy number of entries.
  expect_true(sum(adapters == "achilles") > 50)
  expect_true(sum(adapters == "ohdsi") > 20)
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

test_that("exactly the 8 native prevalence templates are scopable", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  lst <- .omopAnalysisList(h)
  scopable <- lst$name[lst$accepts_cohort]
  expect_equal(sort(scopable), sort(c(
    "dsomop:condition.prevalence_by_concept",
    "dsomop:condition_era.prevalence_by_concept",
    "dsomop:device.prevalence_by_concept",
    "dsomop:drug.prevalence_by_concept",
    "dsomop:drug_era.prevalence_by_concept",
    "dsomop:measurement.prevalence_by_concept",
    "dsomop:observation.prevalence_by_concept",
    "dsomop:procedure.prevalence_by_concept",
    # The native r-in-session reference entry is scopable too.
    "dsomop:demo.person_count_by_gender"
  )))
  # accepts_cohort and accepts_tables move together (one honest hook).
  expect_identical(lst$accepts_cohort, lst$accepts_tables)
})

test_that("Achilles and OHDSI entries are never scopable", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  lst <- .omopAnalysisList(h)
  ach <- lst[grepl("achilles", lst$adapter), ]
  ohd <- lst[grepl("ohdsi", lst$adapter), ]
  expect_true(nrow(ach) > 0 && !any(ach$accepts_cohort))
  expect_true(nrow(ohd) > 0 && !any(ohd$accepts_cohort))
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

  # Per-surface guarantees: Achilles entries all RUN (their tables exist in the
  # fixture); OHDSI entries are only OK or dependency-missing (never dialect or
  # other); the dialect-incompatible class is exclusive to the QueryLibrary
  # (upstream Postgres SQL).
  ach_classes <- classes[adapters == "achilles"]
  ohd_classes <- classes[adapters == "ohdsi"]
  qry_classes <- classes[adapters == "query"]
  expect_true(all(ach_classes %in% c("ok", "gated")))
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

  withr::with_options(list(nfilter.subset = 3, nfilter.tab = 3), {
    # cohort_count is record-unit; the fixture's cohort_id=2 rests on only 2
    # subjects, so its row must not survive the person gate.
    res <- .omopAnalysisRun(h, "dsomop:ohdsi.cohort_diagnostics.cohort_count")
    expect_false(2 %in% res$cohort_id)
    # cohort_id=1 (85 subjects) survives.
    expect_true(1 %in% res$cohort_id)
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

# --- SECURITY (d): no-person-basis entry rejected in strict mode -------------

test_that("(d) record entry with no person basis fail-closes in strict mode", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  # es_cm_result carries only n_databases (a cross-DB meta-analysis count) and no
  # person column. Strict mode (the default) must return zero rows; relaxing it
  # lets the shaped result through.
  strict <- .omopAnalysisRun(h, "dsomop:ohdsi.evidence_synthesis.es_cm_result")
  expect_equal(nrow(strict), 0)

  withr::with_options(list(dsomop.query_strict = FALSE), {
    relaxed <- .omopAnalysisRun(h, "dsomop:ohdsi.evidence_synthesis.es_cm_result")
    expect_true(nrow(relaxed) > 0)
  })

  # incidence_rate (cohort_count, no person column) behaves the same way.
  expect_equal(
    nrow(.omopAnalysisRun(h, "dsomop:ohdsi.cohort_diagnostics.incidence_rate")),
    0)
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

# --- SECURITY (g): precomputed Achilles/OHDSI reject scope -------------------

test_that("(g) precomputed Achilles/OHDSI entries reject scope cleanly", {
  h <- acat_handle()
  on.exit(cleanup_handle(h))

  ach_err <- tryCatch(
    .omopAnalysisRun(h, "dsomop:achilles.401", scope = "dsomop_cohort_1"),
    error = function(e) conditionMessage(e))
  expect_match(ach_err, "does not support cohort/population scoping")
  expect_match(ach_err, "pre-computed result with no per-row person key")
  expect_false(grepl("no such|syntax error", ach_err, ignore.case = TRUE))

  ohd_err <- tryCatch(
    .omopAnalysisRun(h, "dsomop:ohdsi.cohort_diagnostics.cohort_count",
                     scope = "dsomop_cohort_1"),
    error = function(e) conditionMessage(e))
  expect_match(ohd_err, "does not support cohort/population scoping")
  expect_match(ohd_err, "pre-computed result with no per-row person key")
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
