# ==============================================================================
# REACH corpus regression: a strong subset of the 159-scenario flexibility
# corpus, executed end-to-end on a committed OMOP-CDM SQLite fixture.
#
# The full corpus (set-op algebra, peri-index temporal windows, value/unit/type
# slicing, cross-domain combination, scoping + degenerate shapes) was driven to
# 159/159 against this fixture during the "flexibility summit"; this test pins a
# representative slice of each category so the additive features stay covered:
#   - >=3-member set-op folds (unique per-step temp tables);
#   - windowed population concept filters anchored to the cohort INDEX date
#     (washout / on-treatment / post-index), incl. empty set-op members;
#   - per-spec row filters (mutually-exclusive unit/type slices -> distinct
#     columns, not one contradictory AND) on the features/wide path;
#   - concept_set matched on the DOMAIN concept column even under a concept_col
#     surfacing override; date_range row filters resolved to the real date col;
#   - relative_to_index date handling; union of per-variable windows on long;
#   - empty population -> 0-row frame (not dropped); blocked value_source ->
#     fail-closed at compile.
#
# Correctness runs with the per-person gate OFF (nfilter.subset = 0), exactly as
# the reach harness does. A final block re-enables the gate and asserts the
# fail-closed machinery still fires (tiny set-op intersection blocked), so the
# additive flexibility never weakened the disclosure barrier.
#
# The recipe DSL lives in the sibling dsOMOPClient checkout; the whole file is
# skipped when that checkout is absent (same pattern as test-recipe-setops.R).
# ==============================================================================

# Load the committed reach fixture (build_reach_handle()).
.reach_fixture_path <- local({
  tp <- tryCatch(testthat::test_path(), error = function(e) NULL)
  cand <- c(
    if (!is.null(tp)) file.path(tp, "..", "..", "..", "tests", "fixtures",
                                "create_reach_db.R"),
    file.path("tests", "fixtures", "create_reach_db.R"),
    file.path("..", "tests", "fixtures", "create_reach_db.R"),
    file.path("..", "..", "tests", "fixtures", "create_reach_db.R"))
  Find(file.exists, cand)
})

# Load the working-tree client (predates these features when installed).
.reach_client_ready <- function() {
  tp <- tryCatch(testthat::test_path(), error = function(e) ".")
  cand <- c(file.path(tp, "..", "..", "..", "dsOMOPClient"),
            file.path(tp, "..", "..", "dsOMOPClient"),
            "../dsOMOPClient", "../../dsOMOPClient")
  cp <- Find(dir.exists, cand)
  if (is.null(cp)) return(FALSE)
  tryCatch({
    suppressMessages(pkgload::load_all(cp, quiet = TRUE, export_all = FALSE))
    exists("recipe_to_plan", envir = asNamespace("dsOMOPClient"))
  }, error = function(e) FALSE)
}

# Build a fresh reach handle (in-memory SQLite, ~100 persons, blueprint built).
.reach_handle <- function() {
  source(.reach_fixture_path, local = TRUE)
  h <- build_reach_handle(100L)
  .buildBlueprint(h)
  h
}

# Compile a recipe and execute its plan on a fresh handle. Returns the result
# list (named by output). Used inside withr::with_options(nfilter.subset=...).
.reach_run <- function(recipe) {
  plan <- dsOMOPClient::recipe_to_plan(recipe)
  syms <- stats::setNames(paste0("D_", names(plan$outputs)), names(plan$outputs))
  h <- .reach_handle()
  on.exit(cleanup_handle(h))
  suppressWarnings(.planExecute(h, plan, syms))
}

# Shorthands mirroring the corpus harness.
C <- function() {
  list(
    r   = dsOMOPClient::omop_recipe,
    pop = dsOMOPClient::omop_population,
    v   = dsOMOPClient::omop_variable,
    out = dsOMOPClient::omop_output,
    grp = dsOMOPClient::omop_filter_group,
    f_hc  = dsOMOPClient::omop_filter_has_concept,
    f_nhc = dsOMOPClient::omop_filter_not_has_concept,
    f_hm  = dsOMOPClient::omop_filter_has_measurement,
    f_vco = dsOMOPClient::omop_filter_value_concept,
    f_dr  = dsOMOPClient::omop_filter_date_range,
    f_sex = dsOMOPClient::omop_filter_sex,
    va_age = dsOMOPClient::omop_variable_age,
    va_sex = dsOMOPClient::omop_variable_sex)
}

# ----------------------------------------------------------------------------
# A: >=3-member set-op fold (unique per-step temp tables; no name collision)
# ----------------------------------------------------------------------------
test_that("reach A: 3-member intersect fold executes", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if(is.null(.reach_fixture_path), "reach fixture not found")
  skip_if_not(.reach_client_ready(), "working-tree dsOMOPClient not loadable")
  k <- C()
  recipe <- k$r(
    populations = list(
      k$pop("dm", filters = list(k$f_hc(201820, "condition_occurrence"))),
      k$pop("htn", filters = list(k$f_hc(320128, "condition_occurrence"))),
      k$pop("met", filters = list(k$f_hc(1503297, "drug_exposure"))),
      k$pop("all3", intersect = c("dm", "htn", "met"))),
    variables = k$va_sex(),
    outputs = k$out("i3", type = "features", population_id = "all3"))
  withr::with_options(list(nfilter.subset = 0), {
    res <- .reach_run(recipe)
    expect_true(is.data.frame(res[["i3"]]))
    expect_true(nrow(res[["i3"]]) > 0)
  })
})

# ----------------------------------------------------------------------------
# A: empty intersect -> a 0-ROW frame (not a dropped output)
# ----------------------------------------------------------------------------
test_that("reach A: empty set-op population yields a 0-row frame", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if(is.null(.reach_fixture_path), "reach fixture not found")
  skip_if_not(.reach_client_ready(), "working-tree dsOMOPClient not loadable")
  k <- C()
  recipe <- k$r(
    populations = list(
      k$pop("f", filters = list(k$f_sex("F"))),
      k$pop("m", filters = list(k$f_sex("M"))),
      k$pop("none", intersect = c("f", "m"))),
    variables = k$va_age(),
    outputs = k$out("empty_out", type = "wide", population_id = "none"))
  withr::with_options(list(nfilter.subset = 0), {
    res <- .reach_run(recipe)
    expect_true(is.data.frame(res[["empty_out"]]))
    expect_equal(nrow(res[["empty_out"]]), 0L)
  })
})

# ----------------------------------------------------------------------------
# A: windowed population concept filters anchored to the cohort index date,
#    with an empty set-op member surviving the fold (washout pattern).
# ----------------------------------------------------------------------------
test_that("reach A: index-anchored windowed set-op (washout) executes", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if(is.null(.reach_fixture_path), "reach fixture not found")
  skip_if_not(.reach_client_ready(), "working-tree dsOMOPClient not loadable")
  k <- C()
  recipe <- k$r(
    populations = list(
      k$pop("doac_early", filters = list(k$f_hc(40228152, "drug_exposure",
        window = list(start = 0, end = 29)))),
      k$pop("doac_any", filters = list(k$f_hc(40228152, "drug_exposure",
        window = list(start = 0, end = 365)))),
      k$pop("delayed", setdiff = c("doac_any", "doac_early"))),
    variables = k$va_age(), cohort = 313217L,
    outputs = k$out("washout", type = "features", population_id = "delayed"))
  withr::with_options(list(nfilter.subset = 0), {
    res <- .reach_run(recipe)
    expect_true(is.data.frame(res[["washout"]]))
    expect_true(nrow(res[["washout"]]) > 0)
  })
})

# ----------------------------------------------------------------------------
# C: per-spec unit slices -> distinct columns (not one contradictory AND)
# ----------------------------------------------------------------------------
test_that("reach C: mutually-exclusive unit slices become distinct columns", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if(is.null(.reach_fixture_path), "reach fixture not found")
  skip_if_not(.reach_client_ready(), "working-tree dsOMOPClient not loadable")
  k <- C()
  recipe <- k$r(variables = list(
    k$v(name = "hba1c_pct", table = "measurement", concept_id = 3004410,
        value_source = "value_as_number", format = "mean",
        filters = list(k$f_vco(8554, column = "unit_concept_id"))),
    k$v(name = "hba1c_mmolmol", table = "measurement", concept_id = 3004410,
        value_source = "value_as_number", format = "mean",
        filters = list(k$f_vco(8753, column = "unit_concept_id")))),
    outputs = k$out("hba1c_units", type = "wide"))
  withr::with_options(list(nfilter.subset = 0), {
    res <- .reach_run(recipe)
    df <- res[["hba1c_units"]]
    expect_true(is.data.frame(df))
    expect_true(all(c("hba1c_pct", "hba1c_mmolmol") %in% names(df)))
    # The % slice (seeded ~7.5) and the mmol/mol slice (seeded ~52) coexist:
    # at least one person carries a real (non-NA) value in EACH column.
    expect_true(any(!is.na(df[["hba1c_pct"]])))
    expect_true(any(!is.na(df[["hba1c_mmolmol"]])))
  })
})

# ----------------------------------------------------------------------------
# C: concept_set matched on the DOMAIN concept column under a concept_col
#    surfacing override (long path), so it is NOT scoped to nothing.
# ----------------------------------------------------------------------------
test_that("reach C: concept_col override does not zero the concept match", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if(is.null(.reach_fixture_path), "reach fixture not found")
  skip_if_not(.reach_client_ready(), "working-tree dsOMOPClient not loadable")
  k <- C()
  recipe <- k$r(variables = k$v(name = "gluc_stream", table = "measurement",
      concept_id = 3004501, value_source = "value_as_number", format = "raw",
      concept_col = "unit_concept_id",
      filters = list(k$f_vco(c(8840, 8753), column = "unit_concept_id"))),
    cohort = 9L,
    outputs = k$out("gluc_units_long", type = "long"))
  withr::with_options(list(nfilter.subset = 0), {
    res <- .reach_run(recipe)
    expect_true(is.data.frame(res[["gluc_units_long"]]))
    expect_true(nrow(res[["gluc_units_long"]]) > 0)
  })
})

# ----------------------------------------------------------------------------
# B: date_range row filter resolved to the real date column (long path)
# ----------------------------------------------------------------------------
test_that("reach B: date_range row filter resolves to the table date column", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if(is.null(.reach_fixture_path), "reach fixture not found")
  skip_if_not(.reach_client_ready(), "working-tree dsOMOPClient not loadable")
  k <- C()
  recipe <- k$r(
    variables = k$v(name = "resp_dx", table = "condition_occurrence",
                    concept_id = 320136, format = "count"),
    filters = k$f_dr(start = "2020-03-01", end = "2021-06-30"),
    cohort = 9L,
    outputs = k$out("covid_era_dx", type = "long"))
  withr::with_options(list(nfilter.subset = 0), {
    res <- .reach_run(recipe)
    expect_true(is.data.frame(res[["covid_era_dx"]]))
    expect_true(nrow(res[["covid_era_dx"]]) > 0)
  })
})

# ----------------------------------------------------------------------------
# B: relative_to_index date handling on a long output
# ----------------------------------------------------------------------------
test_that("reach B: relative_to_index date handling executes", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if(is.null(.reach_fixture_path), "reach fixture not found")
  skip_if_not(.reach_client_ready(), "working-tree dsOMOPClient not loadable")
  k <- C()
  recipe <- k$r(variables = k$v(name = "glucose_stream", table = "measurement",
      concept_id = 3004501, value_source = "value_as_number",
      time_window = list(start = -30, end = 365)),
    cohort = 17L,
    outputs = k$out("glucose_long", type = "long",
      options = list(date_handling = "relative_to_index",
                     temporal = list(index_window = list(start = -30, end = 365)))))
  withr::with_options(list(nfilter.subset = 0), {
    res <- .reach_run(recipe)
    expect_true(is.data.frame(res[["glucose_long"]]))
    expect_true(nrow(res[["glucose_long"]]) > 0)
  })
})

# ----------------------------------------------------------------------------
# C: a blocked free-text value_source fails CLOSED at compile time
# ----------------------------------------------------------------------------
test_that("reach C: blocked value_source is rejected at compile", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if(is.null(.reach_fixture_path), "reach fixture not found")
  skip_if_not(.reach_client_ready(), "working-tree dsOMOPClient not loadable")
  k <- C()
  recipe <- k$r(variables = list(
    k$v(name = "smoke_freetext", table = "observation", concept_id = 4275495,
        value_source = "value_as_string", format = "last_value")),
    outputs = k$out("rec_bad", type = "wide"))
  expect_error(dsOMOPClient::recipe_to_plan(recipe), "blocked|Disclos")
})

# ----------------------------------------------------------------------------
# A: a duplicate set-op member is rejected at construction
# ----------------------------------------------------------------------------
test_that("reach A: duplicate set-op member is rejected", {
  skip_if_not_installed("pkgload")
  skip_if_not(.reach_client_ready(), "working-tree dsOMOPClient not loadable")
  expect_error(
    dsOMOPClient::omop_population("dup", union = c("dm", "dm")),
    "duplicate")
})

# ----------------------------------------------------------------------------
# SAFETY: with the gate RE-ENABLED, a tiny set-op intersection fails closed.
# ----------------------------------------------------------------------------
test_that("reach SAFETY: gate fires on tiny intersection when re-enabled", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if(is.null(.reach_fixture_path), "reach fixture not found")
  skip_if_not(.reach_client_ready(), "working-tree dsOMOPClient not loadable")
  k <- C()
  recipe <- k$r(
    populations = list(
      k$pop("w", filters = list(k$f_sex("F"))),
      k$pop("dm", filters = list(k$f_hc(201820, "condition_occurrence"))),
      k$pop("mi", filters = list(k$f_hc(4329847, "condition_occurrence"))),
      k$pop("wdm", intersect = c("w", "dm")),
      k$pop("tiny", intersect = c("wdm", "mi"))),
    variables = k$va_age(),
    outputs = k$out("o", type = "wide", population_id = "tiny"))

  # Gate OFF: the tiny intersection executes (rows returned).
  withr::with_options(list(nfilter.subset = 0), {
    res0 <- .reach_run(recipe)
    expect_true(is.data.frame(res0[["o"]]))
    expect_true(nrow(res0[["o"]]) > 0)
  })

  # Gate ON (strict threshold): the SAME recipe fails closed before producing
  # the output (the population resolver gates each set-op result fail-closed).
  withr::with_options(list(nfilter.subset = 50), {
    expect_error(
      {
        plan <- dsOMOPClient::recipe_to_plan(recipe)
        syms <- stats::setNames("D_o", "o")
        h <- .reach_handle(); on.exit(cleanup_handle(h))
        .planExecute(h, plan, syms)
      },
      "Disclos|blocked|insufficient")
  })
})

# ----------------------------------------------------------------------------
# SAFETY: a narrow age_range (width < 5) is rejected while the gate is ON.
# ----------------------------------------------------------------------------
test_that("reach SAFETY: narrow age_range blocked while gate is ON", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if(is.null(.reach_fixture_path), "reach fixture not found")
  skip_if_not(.reach_client_ready(), "working-tree dsOMOPClient not loadable")
  k <- C()
  f_age <- dsOMOPClient::omop_filter_age
  recipe <- k$r(
    populations = k$pop("u", filters = list(f_age(min = 121, max = 122))),
    variables = k$va_age(),
    outputs = k$out("n", type = "wide", population_id = "u"))
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(
      {
        plan <- dsOMOPClient::recipe_to_plan(recipe)
        syms <- stats::setNames("D_n", "n")
        h <- .reach_handle(); on.exit(cleanup_handle(h))
        .planExecute(h, plan, syms)
      },
      "age_range|Disclos|fingerprint")
  })
})
