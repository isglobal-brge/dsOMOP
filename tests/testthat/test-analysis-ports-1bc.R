# ==============================================================================
# Tests for the nine natively re-implemented FeatureExtraction / Characterization
# / PatientLevelPrediction / CohortMethod ports (Stage 1b single-cohort + Stage 1c
# two-population) added to the unified analysis catalog (adapter == "diagnostic").
#
# These are kind="r" catalog entries that scope via ctx$scoped_cohort (Stage 1b)
# or ctx$scoped_cohorts (Stage 1c, two populations) and funnel through the ONE
# gate (.omopAnalysisGate). For EACH of the nine this file proves, on the SQLite
# fixture:
#   (a) it appears in omopAnalysisListDS, dsomop:-prefixed, with honest
#       accepts_cohort/accepts_tables and an inert client-side plot recipe whose
#       compute$plot$code PARSES to a function(df, params);
#   (b) it RUNS scoped to a cohort (1b) or two cohorts (1c) and returns a sane
#       aggregate (or is fixture-limited -> gate-empty, with a note);
#   (c) the SINGLE gate fires: a sub-nfilter stratum/group/region is
#       dropped/suppressed, counts are banded (floor to nfilter_band), dist rows
#       release NO min/max and mask stats below nfilter_dist, the binary
#       prevalence is coupled to its suppressed numerator, the SMD/balance is
#       suppressed when a group is suppressed, and a two-population entry fails
#       closed when either group < nfilter;
#   (d) scoping a too-small cohort (or a single arm to a two-population entry)
#       fails closed.
#
# Fixture grounding (tests/fixtures/create_test_db.R, the parent-repo fixture
# create_test_handle actually sources):
#   * Diabetes (201820): persons 1,3,5,7,9,11  (6)
#   * COPD (255573):     persons 5,7,9,13       (4)
#   * Asthma (317009):   persons 2,4            (2)  -> sub-threshold
#   * MI (4000002):      persons 1,5,9          (outcome; 3 of the 6 diabetics)
#   * HbA1c (3004410):   persons 1,3,5,7,9      (5);  Body weight (3025315): 1..13
#   * gender alternates 8507/8532; race 8527/8516; year_of_birth = 1960 + 2*pid.
# ==============================================================================

# A keyed handle with the catalog pre-built (mirrors test-analysis-ports.R).
p1bc_handle <- function(n_persons = 40) {
  h <- create_test_handle(n_persons = n_persons)
  h$person_key <- as.raw(1:16)
  .buildBlueprint(h)
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

# Strong disclosure options used throughout (note the dsOMOP-namespaced option
# names: nfilter.subset / nfilter.tab / dsomop.nfilter.dist / dsomop.nfilter.band).
p1bc_opts <- function(dist = 10) {
  list(nfilter.subset = 3, nfilter.tab = 3,
       dsomop.nfilter.dist = dist, dsomop.nfilter.band = 5)
}

# The six single-cohort (Stage 1b) ports and the three two-population (Stage 1c)
# ports.
P1B_IDS <- c(
  "dsomop:fe.table1",
  "dsomop:fe.prevalence",
  "dsomop:fe.continuous",
  "dsomop:fe.comorbidity_index",
  "dsomop:char.target_covariates",
  "dsomop:plp.covariate_summary"
)
P1C_IDS <- c(
  "dsomop:cohortdx.cohort_overlap",
  "dsomop:char.risk_factor_smd",
  "dsomop:cm.covariate_balance"
)
P1BC_IDS <- c(P1B_IDS, P1C_IDS)

# Scoped diabetes cohort (6 persons) -- the canonical single-cohort scope.
p1bc_diabetes <- function(h, id = 1L) {
  .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                mode = "temporary", cohort_id = id)
}
# COPD cohort (4 persons) -- the canonical second arm.
p1bc_copd <- function(h, id = 2L) {
  .cohortCreate(h, list(type = "condition", concept_set = c(255573)),
                mode = "temporary", cohort_id = id)
}
# A synthetic all-person cohort (>= nfilter_dist) for live (un-masked) dist rows.
p1bc_allpersons <- function(h, name = "p1bc_allp") {
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE ", name, " AS SELECT person_id AS subject_id, ",
    "'2020-01-01' AS cohort_start_date, '2024-12-31' AS cohort_end_date ",
    "FROM person"))
  name
}

# ---------------------------------------------------------------------------- #
# (a) Discovery: in omopAnalysisListDS, dsomop:-prefixed, honest scope, plot.
# ---------------------------------------------------------------------------- #

test_that("(a) all nine ports appear in omopAnalysisListDS dsomop:-prefixed", {
  h <- p1bc_handle()
  on.exit({
    suppressWarnings(rm(list = "handle_p1bc", envir = dsOMOP:::.dsomop_env))
    cleanup_handle(h)
  })
  .setHandle("p1bc", h)

  lst <- omopAnalysisListDS("p1bc")               # the real DS entrypoint
  sub <- lst[lst$name %in% P1BC_IDS, ]
  expect_equal(nrow(sub), length(P1BC_IDS))
  expect_true(all(startsWith(sub$name, "dsomop:")))
  expect_true(all(sub$adapter == "diagnostic"))
  # accepts_cohort/accepts_tables are honest (both TRUE, and move together).
  expect_true(all(sub$accepts_cohort))
  expect_true(all(sub$accepts_tables))
  expect_identical(sub$accepts_cohort, sub$accepts_tables)

  # Declared gate units match the implementation contract.
  unit_of <- stats::setNames(lst$unit, lst$name)
  expect_equal(unit_of[["dsomop:fe.table1"]], "person")
  expect_equal(unit_of[["dsomop:fe.prevalence"]], "person")
  expect_equal(unit_of[["dsomop:fe.continuous"]], "dist")
  expect_equal(unit_of[["dsomop:fe.comorbidity_index"]], "dist")
  expect_equal(unit_of[["dsomop:char.target_covariates"]], "person")
  expect_equal(unit_of[["dsomop:plp.covariate_summary"]], "person")
  for (id in P1C_IDS) expect_equal(unit_of[[id]], "person")
})

test_that("(a) the two-population ports declare exactly two populations", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  for (id in P1C_IDS) {
    e <- .omopAnalysisResolve(h, id)
    expect_equal(as.integer(e$scope$max_tables), 2L)
  }
  # The single-cohort ports declare one.
  for (id in P1B_IDS) {
    e <- .omopAnalysisResolve(h, id)
    expect_equal(as.integer(e$scope$max_tables), 1L)
  }
})

test_that("(a) each compute$plot recipe parses to a function(df, params)", {
  h <- p1bc_handle()
  on.exit({
    suppressWarnings(rm(list = "handle_p1bc", envir = dsOMOP:::.dsomop_env))
    cleanup_handle(h)
  })
  .setHandle("p1bc", h)
  for (id in P1BC_IDS) {
    e <- .omopAnalysisResolve(h, id)
    expect_true(is.character(e$compute$plot$code))
    fn <- eval(parse(text = e$compute$plot$code))
    expect_true(is.function(fn))
    expect_named(formals(fn), c("df", "params"))
    # The client-safe DS metadata view also surfaces the (inert) recipe.
    m <- omopAnalysisGetDS("p1bc", id)
    expect_equal(m$compute_kind, "r")
    expect_true(is.character(m$plot$code) && grepl("function", m$plot$code))
  }
})

# ---------------------------------------------------------------------------- #
# (b) Runs scoped to a cohort (1b) / two cohorts (1c), returns a sane aggregate.
# ---------------------------------------------------------------------------- #

test_that("(b) fe.table1 runs on the diabetes cohort (mixed person/dist rows)", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(), {
    ct <- p1bc_diabetes(h)
    df <- .omopAnalysisRun(h, "dsomop:fe.table1", scope = ct)
    expect_true(nrow(df) >= 1L)
    expect_true(all(c("characteristic", "level", "unit", "sum_value") %in%
                    names(df)))
    expect_true(all(df$unit %in% c("person", "dist")))
    # All gender persons are in one band; every released person count is banded.
    expect_true(all(df$sum_value %% 5 == 0 | is.na(df$sum_value)))
    expect_false(any(grepl("_source_value$", names(df))))
    # No raw min/max ever leaves the dist rows.
    expect_false(any(grepl("^min_value$|^max_value$", names(df))))
  })
})

test_that("(b) fe.prevalence runs for condition + drug domains", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(), {
    ct <- p1bc_diabetes(h)
    cond <- .omopAnalysisRun(h, "dsomop:fe.prevalence",
                             params = list(domain_code = "0"), scope = ct)
    expect_true(all(c("covariate_id", "covariate_name", "sum_value",
                      "average") %in% names(cond)))
    # Diabetes is universal in the diabetes cohort (6/6 -> banded 5, average 1).
    diab <- cond[cond$covariate_id == 201820, ]
    expect_equal(nrow(diab), 1L)
    expect_equal(diab$sum_value, 5)
    expect_equal(diab$average, 1)
    expect_false(any(grepl("_source_value$", names(cond))))

    drug <- .omopAnalysisRun(h, "dsomop:fe.prevalence",
                             params = list(domain_code = "1"), scope = ct)
    expect_true(all(drug$sum_value %% 5 == 0 | is.na(drug$sum_value)))
  })
})

test_that("(b) fe.continuous returns un-masked stats for a >= nfilter_dist cohort", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(dist = 10), {
    allp <- p1bc_allpersons(h)
    # Body weight (3025315) is recorded for persons 1..13 -> 13 banded to 10
    # (>= nfilter_dist) so its summary stats are released (NOT masked).
    bw <- .omopAnalysisRun(h, "dsomop:fe.continuous",
                           params = list(metric = "measurement_value",
                                         domain_code = "3"), scope = allp)
    expect_true(nrow(bw) >= 1L)
    wrow <- bw[bw$covariate_id == 3025315, ]
    expect_equal(nrow(wrow), 1L)
    expect_equal(wrow$count_value, 10)            # 13 banded down to 10
    expect_false(is.na(wrow$median_value))        # >= nfilter_dist -> released
    expect_true(all(c("p10_value", "p25_value", "p75_value", "p90_value") %in%
                    names(bw)))
    # Min/max are NEVER released by a dist entry.
    expect_false(any(grepl("^min_value$|^max_value$", names(bw))))

    # The age metric path also runs and bands its count.
    age <- .omopAnalysisRun(h, "dsomop:fe.continuous",
                            params = list(metric = "age"), scope = allp)
    expect_equal(nrow(age), 1L)
    expect_equal(age$metric, "age")
    expect_true(age$count_value %% 5 == 0)
  })
})

test_that("(b) fe.comorbidity_index returns a clamped score distribution", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(dist = 10), {
    allp <- p1bc_allpersons(h)
    df <- .omopAnalysisRun(h, "dsomop:fe.comorbidity_index",
                           params = list(index_type = "charlson"), scope = allp)
    expect_equal(nrow(df), 1L)
    expect_equal(df$metric, "charlson_index")
    expect_true(df$count_value %% 5 == 0)
    expect_false(any(grepl("^min_value$|^max_value$", names(df))))
    # A second index_type (dcsi) also resolves and runs.
    dcsi <- .omopAnalysisRun(h, "dsomop:fe.comorbidity_index",
                             params = list(index_type = "dcsi"), scope = allp)
    expect_equal(nrow(dcsi), 1L)
    expect_equal(dcsi$metric, "dcsi_index")
  })
})

test_that("(b) char.target_covariates returns binary + continuous covariates", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(), {
    ct <- p1bc_diabetes(h)
    df <- .omopAnalysisRun(h, "dsomop:char.target_covariates", scope = ct)
    expect_true(nrow(df) >= 1L)
    expect_true(all(c("kind", "covariate_id", "covariate_name", "sum_value") %in%
                    names(df)))
    expect_true(all(df$kind %in% c("binary", "continuous")))
    # sum_value is the UNIFIED person-basis column across both kinds and is
    # always banded.
    expect_true(all(df$sum_value %% 5 == 0 | is.na(df$sum_value)))
    expect_false(any(grepl("_source_value$", names(df))))
  })
})

test_that("(b) plp.covariate_summary splits prevalence by an outcome concept set", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(), {
    ct <- p1bc_diabetes(h)
    df <- .omopAnalysisRun(h, "dsomop:plp.covariate_summary",
                           params = list(outcome_concept_id = "4000002",
                                         domain_code = "0"), scope = ct)
    expect_true(all(c("covariate_id", "covariate_name", "outcome", "sum_value",
                      "average") %in% names(df)))
    # The split is by outcome status (0/1); each arm's count is banded.
    expect_true(all(df$outcome %in% c(0, 1)))
    expect_true(all(df$sum_value %% 5 == 0 | is.na(df$sum_value)))
    # No outcome concept -> no split -> gate-safe empty frame (not an error).
    none <- .omopAnalysisRun(h, "dsomop:plp.covariate_summary", scope = ct)
    expect_equal(nrow(none), 0L)
  })
})

test_that("(b) cohort_overlap counts both / A-only / B-only / either", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(), {
    # diabetes = {1,3,5,7,9,11} (6); COPD = {5,7,9,13} (4).
    # both = {5,7,9} = 3; a_only = {1,3,11} = 3; b_only = {13} = 1; either = 7.
    a <- p1bc_diabetes(h, id = 1L)
    b <- p1bc_copd(h, id = 2L)
    df <- .omopAnalysisRun(h, "dsomop:cohortdx.cohort_overlap",
                           scope = list(a, b))
    expect_true(all(c("category", "n") %in% names(df)))
    # Long format: each surviving region is suppressed + banded independently.
    expect_true(all(df$n %% 5 == 0 | is.na(df$n)))
    got <- stats::setNames(df$n, df$category)
    expect_equal(unname(got[["either"]]), 5)        # 7 persons -> banded to 5
    expect_false("b_only" %in% df$category)         # 1 person -> dropped
  })
})

test_that("(b) the SMD ports are fixture-limited on small cohorts (gate-empty)", {
  # On the bare fixture every per-covariate arm count over a ~6-person cohort
  # bands below threshold, so the gate drops every covariate (no two-armed
  # survivor). This is CORRECT gating, not a failure; a live SMD over arms with a
  # >= 5-person shared covariate is asserted in the (c) tests below.
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(), {
    a <- p1bc_diabetes(h, id = 1L)
    b <- p1bc_copd(h, id = 2L)
    smd <- .omopAnalysisRun(h, "dsomop:char.risk_factor_smd", scope = list(a, b))
    expect_equal(nrow(smd), 0L)
    bal <- .omopAnalysisRun(h, "dsomop:cm.covariate_balance", scope = list(a, b))
    expect_equal(nrow(bal), 0L)
  })
})

# ---------------------------------------------------------------------------- #
# (c) The SINGLE gate fires.
# ---------------------------------------------------------------------------- #

test_that("(c) prevalence couples a banded-away numerator to a masked proportion", {
  # COPD (255573) is present for only 3 of the 6 diabetics ({5,7,9}); a raw
  # numerator of 3 survives small-cell suppression (>= nfilter_tab) but BANDS to
  # 0. Its proportion (3/6 = 0.5) would re-derive the raw count, so the gate's
  # binary-prevalence coupling must NA the proportion wherever the banded
  # numerator is 0 (or NA). Diabetes (6/6 -> banded 5, average 1) is unaffected.
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(), {
    ct <- p1bc_diabetes(h)
    df <- .omopAnalysisRun(h, "dsomop:fe.prevalence",
                           params = list(domain_code = "0"), scope = ct)
    leak <- df[(is.na(df$sum_value) | df$sum_value == 0) & !is.na(df$average), ]
    expect_equal(nrow(leak), 0L)                   # no surviving proportion leak
    diab <- df[df$covariate_id == 201820, ]        # genuine supra-band survivor
    expect_equal(diab$sum_value, 5)
    expect_equal(diab$average, 1)
  })
})

test_that("(c) dist entries mask stats below nfilter_dist and never emit min/max", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(dist = 10), {
    ct <- p1bc_diabetes(h)   # 6 persons -> banded count 5 < nfilter_dist (10)
    cont <- .omopAnalysisRun(h, "dsomop:fe.continuous",
                             params = list(metric = "age"), scope = ct)
    expect_false(any(grepl("^min_value$|^max_value$", names(cont))))
    if (nrow(cont) > 0) {
      expect_true(cont$count_value %% 5 == 0)
      expect_true(is.na(cont$median_value))        # 5 < 10 -> masked
      expect_true(is.na(cont$avg_value))
      expect_true(is.na(cont$p90_value))
    }
    como <- .omopAnalysisRun(h, "dsomop:fe.comorbidity_index",
                             params = list(index_type = "charlson"), scope = ct)
    expect_false(any(grepl("^min_value$|^max_value$", names(como))))
    if (nrow(como) > 0) expect_true(is.na(como$median_value))
  })
})

test_that("(c) char.target_covariates strips dist controls onto the person basis", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(dist = 10), {
    ct <- p1bc_diabetes(h)
    df <- .omopAnalysisRun(h, "dsomop:char.target_covariates", scope = ct)
    # No min/max ever; continuous rows over the 6-person cohort have their stats
    # masked (count banded to 5 < nfilter_dist), yet still carry a non-NA banded
    # sum_value so they are gate-consistent under the person branch.
    expect_false(any(grepl("^min_value$|^max_value$", names(df))))
    cont <- df[df$kind == "continuous", ]
    if (nrow(cont) > 0) {
      expect_true(all(cont$sum_value %% 5 == 0 | is.na(cont$sum_value)))
      expect_true(all(is.na(cont$median_value)))   # masked (< nfilter_dist)
    }
  })
})

test_that("(c) cohort_overlap bands each region down and drops the tiny region", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(), {
    a <- p1bc_diabetes(h, id = 1L)
    b <- p1bc_copd(h, id = 2L)
    df <- .omopAnalysisRun(h, "dsomop:cohortdx.cohort_overlap",
                           scope = list(a, b))
    # Every released region is a multiple of nfilter_band; the 1-person b_only is
    # dropped (no NA row), so differencing released regions cannot recover it.
    expect_true(all(df$n %% 5 == 0))
    expect_false("b_only" %in% df$category)
  })
})

test_that("(c) risk_factor_smd: live SMD rests on banded counts, both arms gated", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(), {
    # Two synthetic arms with a shared covariate big enough to survive in BOTH:
    # arm A = persons 1..20, arm B = 21..40. Insert diabetes for >= 5 persons in
    # each arm (a supra-band shared covariate) and COPD for only 2 persons in arm
    # B (a sub-threshold arm -> that covariate must be dropped).
    DBI::dbExecute(h$conn, paste0(
      "CREATE TEMP TABLE p1bc_smdA AS SELECT person_id AS subject_id, ",
      "'2020-01-01' AS cohort_start_date, '2024-12-31' AS cohort_end_date ",
      "FROM person WHERE person_id BETWEEN 1 AND 20"))
    DBI::dbExecute(h$conn, paste0(
      "CREATE TEMP TABLE p1bc_smdB AS SELECT person_id AS subject_id, ",
      "'2020-01-01' AS cohort_start_date, '2024-12-31' AS cohort_end_date ",
      "FROM person WHERE person_id BETWEEN 21 AND 40"))
    cid <- 6000L
    for (pid in c(1:8, 21:27)) {       # diabetes in both arms (>= 5 each)
      cid <- cid + 1L
      DBI::dbExecute(h$conn, sprintf(paste0(
        "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
        "condition_concept_id, condition_start_date, condition_end_date, ",
        "condition_type_concept_id) VALUES (%d, %d, 201820, '2021-01-01', ",
        "'2021-01-10', 44818518)"), cid, pid))
    }
    for (pid in c(21, 22)) {           # COPD only in arm B, sub-threshold
      cid <- cid + 1L
      DBI::dbExecute(h$conn, sprintf(paste0(
        "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
        "condition_concept_id, condition_start_date, condition_end_date, ",
        "condition_type_concept_id) VALUES (%d, %d, 255573, '2021-02-01', ",
        "'2021-02-10', 44818518)"), cid, pid))
    }
    df <- .omopAnalysisRun(h, "dsomop:char.risk_factor_smd",
                           scope = list("p1bc_smdA", "p1bc_smdB"))
    expect_true(nrow(df) >= 1L)
    expect_true(all(c("case_sum_value", "non_case_sum_value", "case_average",
                      "non_case_average", "smd") %in% names(df)))
    # COPD (arm-B count 2) is dropped; diabetes survives in both arms.
    expect_true(201820 %in% df$covariate_id)
    expect_false(255573 %in% df$covariate_id)
    # Both arm counts banded; SMD only where BOTH arms survived.
    expect_true(all(df$case_sum_value %% 5 == 0))
    expect_true(all(df$non_case_sum_value %% 5 == 0))
    live <- df[!is.na(df$smd), ]
    expect_true(all(!is.na(live$case_sum_value) & !is.na(live$non_case_sum_value)))
    # SMD recomputed from banded prevalences (case 10/20=.5 vs non-case 5/20=.25).
    drow <- df[df$covariate_id == 201820, ]
    expect_equal(drow$case_average, drow$case_sum_value / 20)
    expect_equal(drow$non_case_average, drow$non_case_sum_value / 20)
    expect_equal(drow$smd, .omopBinarySmd(drow$case_average, drow$non_case_average))
  })
})

test_that("(c) cm.covariate_balance recomputes SMD from banded target/comparator", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(), {
    DBI::dbExecute(h$conn, paste0(
      "CREATE TEMP TABLE p1bc_balT AS SELECT person_id AS subject_id, ",
      "'2020-01-01' AS cohort_start_date, '2024-12-31' AS cohort_end_date ",
      "FROM person WHERE person_id BETWEEN 1 AND 20"))
    DBI::dbExecute(h$conn, paste0(
      "CREATE TEMP TABLE p1bc_balC AS SELECT person_id AS subject_id, ",
      "'2020-01-01' AS cohort_start_date, '2024-12-31' AS cohort_end_date ",
      "FROM person WHERE person_id BETWEEN 21 AND 40"))
    cid <- 7000L
    for (pid in c(1:8, 21:27)) {
      cid <- cid + 1L
      DBI::dbExecute(h$conn, sprintf(paste0(
        "INSERT INTO condition_occurrence (condition_occurrence_id, person_id, ",
        "condition_concept_id, condition_start_date, condition_end_date, ",
        "condition_type_concept_id) VALUES (%d, %d, 201820, '2021-01-01', ",
        "'2021-01-10', 44818518)"), cid, pid))
    }
    df <- .omopAnalysisRun(h, "dsomop:cm.covariate_balance",
                           scope = list("p1bc_balT", "p1bc_balC"))
    expect_true(nrow(df) >= 1L)
    expect_true(all(c("target_sum_value", "comparator_sum_value",
                      "target_average", "comparator_average",
                      "std_mean_diff") %in% names(df)))
    expect_true(all(df$target_sum_value %% 5 == 0))
    expect_true(all(df$comparator_sum_value %% 5 == 0))
    drow <- df[df$covariate_id == 201820, ]
    expect_equal(drow$std_mean_diff,
                 .omopBinarySmd(drow$target_average, drow$comparator_average))
    expect_false(any(grepl("_source_value$", names(df))))
  })
})

test_that("(c) the binary-SMD kernel suppresses the SMD when an arm is suppressed", {
  # Defence-in-depth on the shared kernel: the SMD must be NA when either arm
  # prevalence is NA (a suppressed arm) or the pooled SD is 0, so a one-armed
  # comparison is never released.
  expect_true(is.na(.omopBinarySmd(0.5, NA_real_)))
  expect_true(is.na(.omopBinarySmd(NA_real_, 0.3)))
  expect_true(is.na(.omopBinarySmd(1, 1)))         # zero pooled SD -> NA
  expect_false(is.na(.omopBinarySmd(0.6, 0.2)))    # both arms present -> defined
})

# ---------------------------------------------------------------------------- #
# (d) Too-small scope (or wrong arity) fails closed.
# ---------------------------------------------------------------------------- #

test_that("(d) a sub-threshold single-cohort scope fails closed for every 1b port", {
  # A 2-person hand-built cohort (bypassing .cohortCreate's gate) must still be
  # refused by the run path's re-gate before any port fn materialises rows.
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE p1bc_tiny AS SELECT person_id AS subject_id, ",
    "'2020-01-01' AS cohort_start_date, '2024-12-31' AS cohort_end_date ",
    "FROM person WHERE person_id IN (1, 2)"))
  withr::with_options(p1bc_opts(), {
    for (id in P1B_IDS) {
      params <- if (identical(id, "dsomop:plp.covariate_summary")) {
        list(outcome_concept_id = "4000002")
      } else list()
      expect_error(
        .omopAnalysisRun(h, id, params = params, scope = "p1bc_tiny"),
        "insufficient individuals|disclosure threshold",
        info = id)
    }
  })
})

test_that("(d) a two-population port fails closed when either arm < nfilter", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(), {
    big <- p1bc_diabetes(h, id = 1L)                # 6 persons (ok)
    DBI::dbExecute(h$conn, paste0(
      "CREATE TEMP TABLE p1bc_tinyarm AS SELECT person_id AS subject_id, ",
      "'2020-01-01' AS cohort_start_date, '2024-12-31' AS cohort_end_date ",
      "FROM person WHERE person_id IN (1, 2)"))     # 2 persons (sub-threshold)
    for (id in P1C_IDS) {
      # The pair resolver re-gates EACH arm; the tiny arm is refused fail-closed.
      expect_error(
        .omopAnalysisRun(h, id, scope = list(big, "p1bc_tinyarm")),
        "insufficient individuals|disclosure threshold",
        info = id)
    }
  })
})

test_that("(d) a two-population port rejects a single-arm scope", {
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(), {
    a <- p1bc_diabetes(h)
    for (id in P1C_IDS) {
      expect_error(.omopAnalysisRun(h, id, scope = a),
                   "exactly two scope elements", info = id)
    }
  })
})

test_that("(d) un-scoped, every port yields a gate-safe empty frame", {
  # The cohort IS the analysis population, so an un-scoped run has nothing to
  # summarise and must return an empty (never disclosive) frame, not error.
  h <- p1bc_handle()
  on.exit(cleanup_handle(h))
  withr::with_options(p1bc_opts(), {
    for (id in P1BC_IDS) {
      expect_equal(nrow(.omopAnalysisRun(h, id)), 0L, info = id)
    }
  })
})
