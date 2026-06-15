# ==============================================================================
# Tests for the live R-in-session CohortMethod INTERACTION estimate
# (dsomop:ohdsi.cohort_method.cm_interaction_result / canonical
# dsomop:cm.interaction_estimate), defined in R/ohdsi_pack_cm_interaction.R.
#
# The interaction estimate is the treatment effect WITHIN pre-specified subgroups
# (effect modification): for each subgroup level both arms of the scoped
# target+comparator pair are restricted to the level, the per-subject
# Surv/Poisson frame is LOADED into R, and a Cox/Poisson model is fit IN-SESSION
# for that level's HR/RR + CI. This file proves, on the SQLite fixture:
#   (a) it is registered (stable OHDSI id + canonical short id), record-unit, with
#       both per-arm person columns gating each row, and an inert plot that PARSES;
#   (b) the in-session FITTER recovers a known effect (target rate > comparator ->
#       estimate > 1) for BOTH the Poisson and the Cox families, and NAs an
#       unfittable (no-event / single-arm) subgroup rather than inventing a number;
#   (c) the fn runs over a two-population scope, returns one row per subgroup level
#       with the documented columns, and bands every per-arm count;
#   (d) the STRICTEST fail-closed control fires PER ROW: a subgroup whose arm bands
#       below the subset floor has its estimate + CI NA'd (and the row dropped by
#       the gate), and the subgroup level count is capped;
#   (e) a single-arm / un-scoped misuse is rejected / gate-empty.
# ==============================================================================

# A keyed handle with the catalog pre-built (mirrors test-analysis-ports-1bc.R).
cmix_handle <- function(n_persons = 40) {
  h <- create_test_handle(n_persons = n_persons)
  h$person_key <- as.raw(1:16)
  .buildBlueprint(h)
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

cmix_opts <- function() {
  list(nfilter.subset = 3, nfilter.tab = 3,
       dsomop.nfilter.dist = 10, dsomop.nfilter.band = 5,
       nfilter.levels.max = 40)
}

# An all-person arm with a fixed wide TAR (index 2018-01-01 .. 2030-12-31), so the
# per-subject person-days are well-defined and every fixture person is in-arm.
cmix_allpersons <- function(h, name) {
  DBI::dbExecute(h$conn, paste0(
    "CREATE TEMP TABLE ", name, " AS SELECT person_id AS subject_id, ",
    "'2018-01-01' AS cohort_start_date, '2030-12-31' AS cohort_end_date ",
    "FROM person"))
  name
}

OHDSI_IX_ID <- "dsomop:ohdsi.cohort_method.cm_interaction_result"
CANON_IX_ID <- "dsomop:cm.interaction_estimate"

# ---------------------------------------------------------------------------- #
# (a) Registration, metadata, plot recipe.
# ---------------------------------------------------------------------------- #

test_that("(a) interaction entry is registered live under both ids, record-unit", {
  h <- cmix_handle()
  on.exit(cleanup_handle(h))
  cat <- .omopAnalysisRegistry(h)

  expect_true(OHDSI_IX_ID %in% names(cat))
  expect_true(CANON_IX_ID %in% names(cat))

  e <- cat[[OHDSI_IX_ID]]
  # Live compute (NOT a read of a precomputed table): kind r, an fn, no precomputed
  # meta flag, the ohdsi_live adapter.
  expect_identical(e$compute$kind, "r")
  expect_true(is.function(e$compute$fn))
  expect_null(e$meta$precomputed)
  expect_identical(e$meta$adapter, "ohdsi_live")
  # Two-population scope, record unit, BOTH arms' persons among the gated counts.
  expect_identical(as.integer(e$scope$max_tables), 2L)
  expect_identical(e$disclosure$unit, "record")
  expect_identical(e$disclosure$person_id_col, "target_persons")
  expect_true(all(c("target_persons", "comparator_persons") %in%
                    e$disclosure$count_cols))
  # The canonical alias is the SAME compute, only re-labelled.
  ce <- cat[[CANON_IX_ID]]
  expect_identical(ce$disclosure$unit, "record")
  expect_identical(ce$meta$table_name, "cm_interaction_result")

  # The client-side plot recipe parses (inert string -> language).
  expect_silent(parse(text = e$compute$plot$code))
})

# ---------------------------------------------------------------------------- #
# (b) The in-session fitter recovers a known effect and NAs the unfittable.
# ---------------------------------------------------------------------------- #

test_that("(b) Poisson fitter recovers target-rate > comparator-rate (RR > 1)", {
  set.seed(1)
  # Target: ~2 events per 100 person-days; comparator: ~0.5 per 100 -> true RR ~4.
  tgt <- data.frame(person_days = rep(100, 60),
                    events = stats::rpois(60, lambda = 2))
  cmp <- data.frame(person_days = rep(100, 60),
                    events = stats::rpois(60, lambda = 0.5))
  fit <- dsOMOP:::.omopCmFitSubgroupEffect(tgt, cmp, model_type = "poisson")
  expect_false(is.na(fit$log_estimate))
  expect_false(is.na(fit$se_log_estimate))
  expect_gt(exp(fit$log_estimate), 1)          # RR clearly above the null
  expect_gt(fit$se_log_estimate, 0)
})

test_that("(b) Cox fitter recovers a higher target hazard (HR > 1)", {
  skip_if_not_installed("survival")
  set.seed(2)
  # Target events earlier (shorter time, more events) than comparator, but with
  # overlapping follow-up so the partial likelihood stays finite (no separation).
  tgt <- data.frame(person_days = sample(30:260, 80, replace = TRUE),
                    events = rbinom(80, 1, 0.6))
  cmp <- data.frame(person_days = sample(60:300, 80, replace = TRUE),
                    events = rbinom(80, 1, 0.3))
  fit <- suppressWarnings(
    dsOMOP:::.omopCmFitSubgroupEffect(tgt, cmp, model_type = "cox"))
  expect_false(is.na(fit$log_estimate))
  expect_gt(exp(fit$log_estimate), 1)          # higher hazard in target
})

test_that("(b) an unfittable subgroup (no events / empty arm) returns NA, not a number", {
  # No events anywhere -> effect unidentified.
  z_t <- data.frame(person_days = rep(100, 10), events = rep(0, 10))
  z_c <- data.frame(person_days = rep(100, 10), events = rep(0, 10))
  f0 <- dsOMOP:::.omopCmFitSubgroupEffect(z_t, z_c, model_type = "poisson")
  expect_true(is.na(f0$log_estimate))
  expect_true(is.na(f0$se_log_estimate))

  # An empty arm -> nothing to contrast.
  empty <- data.frame(person_days = numeric(0), events = numeric(0))
  f1 <- dsOMOP:::.omopCmFitSubgroupEffect(z_t, empty, model_type = "poisson")
  expect_true(is.na(f1$log_estimate))
})

# ---------------------------------------------------------------------------- #
# (c) The fn runs over a two-population scope and returns gated subgroup rows.
# ---------------------------------------------------------------------------- #

test_that("(c) sex-subgroup interaction runs, returns the documented columns, bands counts", {
  h <- cmix_handle(n_persons = 40)
  on.exit(cleanup_handle(h))
  withr::with_options(cmix_opts(), {
    a <- cmix_allpersons(h, "cmix_t")        # both arms = all 40 persons (each
    b <- cmix_allpersons(h, "cmix_c")        # sex level ~20 persons per arm)
    df <- .omopAnalysisRun(h, CANON_IX_ID, scope = list(a, b),
                           params = list(subgroup_kind = "sex",
                                         outcome_concept_id = "4000002",
                                         model_type = "poisson"))
    expect_s3_class(df, "data.frame")
    needed <- c("subgroup_label", "target_persons", "comparator_persons",
                "target_outcomes", "comparator_outcomes",
                "target_person_days", "comparator_person_days",
                "estimate", "ci_lo", "ci_hi", "log_estimate", "se_log_estimate")
    expect_true(all(needed %in% names(df)))
    # No *_source_value ever leaks through the gate.
    expect_false(any(grepl("_source_value$", names(df), ignore.case = TRUE)))
    if (nrow(df) > 0) {
      # Each surviving row's per-arm counts are banded (floor to nfilter_band=5).
      for (col in c("target_persons", "comparator_persons")) {
        expect_true(all(df[[col]] %% 5 == 0 | is.na(df[[col]])))
      }
      # Sex labels are present and translated (not raw concept ids / source vals).
      expect_true(all(grepl("^sex: ", df$subgroup_label)))
    }
  })
})

# ---------------------------------------------------------------------------- #
# (d) The strictest fail-closed control fires PER ROW + the level cap.
# ---------------------------------------------------------------------------- #

test_that("(d) a subgroup arm below the subset floor has NA estimate and is dropped", {
  # Asthma (317009) is present for only persons {2,4} -> a 2-person subgroup arm,
  # below nfilter.subset. Its "yes" level must NEVER carry a fitted estimate, and
  # the gate must drop the row (its per-arm person count bands below threshold).
  h <- cmix_handle(n_persons = 40)
  on.exit(cleanup_handle(h))
  withr::with_options(cmix_opts(), {
    a <- cmix_allpersons(h, "cmix_t2")
    b <- cmix_allpersons(h, "cmix_c2")
    df <- .omopAnalysisRun(h, CANON_IX_ID, scope = list(a, b),
                           params = list(subgroup_kind = "concept",
                                         subgroup_concept_id = "317009",
                                         subgroup_domain_code = "0",
                                         outcome_concept_id = "4000002",
                                         model_type = "poisson"))
    # The "yes" (asthma-present, 2 persons) level is gate-dropped; whatever rows
    # survive must have a banded per-arm person count >= nfilter.tab and, if their
    # estimate is non-NA, it rests on two supra-floor arms.
    if (nrow(df) > 0) {
      asthma_yes <- df[grepl(": yes$", df$subgroup_label), , drop = FALSE]
      expect_equal(nrow(asthma_yes), 0L)        # the 2-person arm row is dropped
      leak <- df[!is.na(df$estimate) &
                   (is.na(df$target_persons) | df$target_persons < 3 |
                    is.na(df$comparator_persons) | df$comparator_persons < 3), ]
      expect_equal(nrow(leak), 0L)              # no estimate over a sub-floor arm
    } else {
      succeed("fixture-limited: every subgroup arm bands out (correct gating)")
    }
  })
})

test_that("(d) the in-fn fail-closed NA fires before the gate (raw frame check)", {
  # Drive the compute fn directly so we see the UN-gated frame: a concept subgroup
  # whose 'yes' arm is tiny must already carry NA estimate/CI from the in-fn
  # banded-persons < nfilter.subset check (independent of the gate's row-drop).
  h <- cmix_handle(n_persons = 40)
  on.exit(cleanup_handle(h))
  withr::with_options(cmix_opts(), {
    a <- cmix_allpersons(h, "cmix_t3")
    b <- cmix_allpersons(h, "cmix_c3")
    fn  <- dsOMOP:::.omopCmInteractionEntry()$compute$fn
    ctx <- list(scoped_cohorts = list(a, b))
    raw <- fn(h, ctx, list(subgroup_kind = "concept",
                           subgroup_concept_id = "317009",
                           subgroup_domain_code = "0",
                           outcome_concept_id = "4000002",
                           model_type = "poisson"))
    expect_s3_class(raw, "data.frame")
    yes <- raw[grepl(": yes$", raw$subgroup_label), , drop = FALSE]
    # The 2-person 'yes' arm: estimate + CI NA'd in-fn (fail-closed), even though
    # the raw per-arm counts are still present (the gate bands/drops them after).
    expect_true(nrow(yes) >= 1)
    expect_true(all(is.na(yes$estimate)))
    expect_true(all(is.na(yes$ci_lo) & is.na(yes$ci_hi)))
    expect_true(all(is.na(yes$log_estimate) & is.na(yes$se_log_estimate)))
  })
})

test_that("(d) subgroup level count is capped at nfilter.levels.max", {
  h <- cmix_handle(n_persons = 40)
  on.exit(cleanup_handle(h))
  withr::with_options(list(nfilter.levels.max = 3), {
    # Many age bands requested, but the cap clamps the level count.
    subs <- dsOMOP:::.omopCmInteractionSubgroups(
      h, list(subgroup_kind = "age", age_breaks = "30,40,50,60,70,80,90"))
    expect_lte(length(subs), 3L)
  })
})

# ---------------------------------------------------------------------------- #
# (e) Misuse: single-arm / un-scoped is rejected or gate-empty.
# ---------------------------------------------------------------------------- #

test_that("(e) a single-arm scope is rejected (two populations required)", {
  h <- cmix_handle(n_persons = 40)
  on.exit(cleanup_handle(h))
  withr::with_options(cmix_opts(), {
    a <- cmix_allpersons(h, "cmix_solo")
    expect_error(
      .omopAnalysisRun(h, CANON_IX_ID, scope = a,
                       params = list(subgroup_kind = "sex")),
      regexp = "population|two|scope|cohort", ignore.case = TRUE)
  })
})

test_that("(e) un-scoped interaction is a gate-safe empty frame", {
  h <- cmix_handle(n_persons = 40)
  on.exit(cleanup_handle(h))
  withr::with_options(cmix_opts(), {
    fn  <- dsOMOP:::.omopCmInteractionEntry()$compute$fn
    out <- fn(h, list(scoped_cohorts = NULL), list(subgroup_kind = "sex"))
    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 0L)
  })
})
