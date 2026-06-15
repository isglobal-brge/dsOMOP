# ==============================================================================
# Server-side proof for the two simplicity/cleanup changes that the client can
# only assert at the transport boundary. All run end-to-end on the committed
# OMOP-CDM SQLite fixture (create_test_handle), the same backend the analysis
# ports tests use.
#
#  (1) CLEANUP (filter_tree-only): the client now emits ONLY plan$cohort$filter_tree
#      for recipe-authored population filters (no flat `spec`). This file loads the
#      working-tree client, compiles representative recipes, and proves the
#      filter_tree-only plan still EXECUTES (single-cohort base path) and that the
#      ONE per-person gate still FAIL-CLOSES on a sub-threshold population. The
#      genuine inline-concept `spec` path (ds.omop.plan.cohort(spec=)) is exercised
#      separately so the server's split gate keeps that branch alive.
#
#  (2) SIMPLICITY (one-liners + bare names): the client one-liners add only an
#      entry-name + a human-domain->code mapping over ds.omop.analysis.run. This
#      file proves that running fe.prevalence / fe.continuous via the BARE name
#      ("prevalence") with the params the client one-liner BUILDS (via the client's
#      .analysis_domain_code) yields a gated frame IDENTICAL to the full-id
#      ds.omop.analysis.run path -- i.e. the shortcut changes nothing about the
#      gated numbers. Bare-name resolution + ambiguity + requires-cohort are also
#      pinned here (the server is the single choke point for all three).
#
# The recipe DSL + the one-liner helpers live in the sibling dsOMOPClient
# checkout; the recipe/one-liner blocks skip when that checkout is absent (same
# pattern as test-recipe-reach.R). Bare-name / requires-cohort are pure server and
# never skip.
# ==============================================================================

# Load the working-tree client (same loader as the reach corpus).
.shc_client_ready <- function() {
  tp <- tryCatch(testthat::test_path(), error = function(e) ".")
  cand <- c(file.path(tp, "..", "..", "..", "dsOMOPClient"),
            file.path(tp, "..", "..", "dsOMOPClient"),
            "../dsOMOPClient", "../../dsOMOPClient")
  cp <- Find(dir.exists, cand)
  if (is.null(cp)) return(FALSE)
  tryCatch({
    suppressMessages(pkgload::load_all(cp, quiet = TRUE, export_all = FALSE))
    exists("recipe_to_plan", envir = asNamespace("dsOMOPClient")) &&
      exists("ds.omop.prevalence", envir = asNamespace("dsOMOPClient"))
  }, error = function(e) FALSE)
}

# A keyed handle with the catalog pre-built (mirrors test-analysis-ports-1bc.R).
shc_handle <- function(n_persons = 40) {
  h <- create_test_handle(n_persons = n_persons)
  h$person_key <- as.raw(1:16)
  .buildBlueprint(h)
  suppressWarnings(.omopAnalysisRegistry(h))
  h
}

shc_opts <- function(dist = 10) {
  list(nfilter.subset = 3, nfilter.tab = 3,
       dsomop.nfilter.dist = dist, dsomop.nfilter.band = 5)
}

# Canonical diabetes scope (6 persons in the fixture) used for the equality proof.
shc_diabetes <- function(h, id = 1L) {
  .cohortCreate(h, list(type = "condition", concept_set = c(201820)),
                mode = "temporary", cohort_id = id)
}

# ----------------------------------------------------------------------------
# (1) CLEANUP: filter_tree-only recipe plans still execute end-to-end.
# ----------------------------------------------------------------------------

test_that("cleanup: a recipe population filter compiles to filter_tree-only and executes", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if_not(.shc_client_ready(), "working-tree dsOMOPClient not loadable")
  cl <- asNamespace("dsOMOPClient")

  recipe <- cl$omop_recipe(
    populations = cl$omop_population("base", filters = list(
      cl$omop_filter_has_concept(201820, "condition_occurrence"))),
    variables = cl$omop_variable_age(),
    outputs = cl$omop_output(name = "o", type = "wide"))
  plan <- cl$recipe_to_plan(recipe)

  # The plan carries ONLY filter_tree (the dual-emission flat spec is gone).
  expect_null(plan$cohort$spec)
  expect_false(is.null(plan$cohort$filter_tree))

  h <- shc_handle(); on.exit(cleanup_handle(h))
  syms <- stats::setNames(paste0("D_", names(plan$outputs)), names(plan$outputs))
  withr::with_options(list(nfilter.subset = 0), {
    res <- suppressWarnings(.planExecute(h, plan, syms))
    expect_true(is.data.frame(res[["o"]]))
    expect_true(nrow(res[["o"]]) > 0)
  })
})

test_that("cleanup: a filter_group tree compiles to filter_tree-only and executes", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if_not(.shc_client_ready(), "working-tree dsOMOPClient not loadable")
  cl <- asNamespace("dsOMOPClient")

  recipe <- cl$omop_recipe(
    populations = cl$omop_population("base", filters = list(
      cl$omop_filter_group(
        cl$omop_filter_has_concept(201820, "condition_occurrence"),
        cl$omop_filter_has_concept(255573, "condition_occurrence"),
        operator = "OR"))),
    variables = cl$omop_variable_age(),
    outputs = cl$omop_output(name = "o", type = "wide"))
  plan <- cl$recipe_to_plan(recipe)
  expect_null(plan$cohort$spec)
  expect_true("or" %in% names(plan$cohort$filter_tree))

  h <- shc_handle(); on.exit(cleanup_handle(h))
  syms <- stats::setNames(paste0("D_", names(plan$outputs)), names(plan$outputs))
  withr::with_options(list(nfilter.subset = 0), {
    res <- suppressWarnings(.planExecute(h, plan, syms))
    expect_true(is.data.frame(res[["o"]]))
    expect_true(nrow(res[["o"]]) > 0)         # diabetes(6) OR copd(4) -> >= 6
  })
})

test_that("cleanup: the gate still FAIL-CLOSES on a sub-threshold filter_tree population", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if_not(.shc_client_ready(), "working-tree dsOMOPClient not loadable")
  cl <- asNamespace("dsOMOPClient")

  # Asthma (317009) is recorded for only 2 persons in the fixture; with the gate
  # ON (nfilter.subset = 5) the filter_tree cohort must be blocked.
  recipe <- cl$omop_recipe(
    populations = cl$omop_population("base", filters = list(
      cl$omop_filter_has_concept(317009, "condition_occurrence"))),
    variables = cl$omop_variable_age(),
    outputs = cl$omop_output(name = "o", type = "wide"))
  plan <- cl$recipe_to_plan(recipe)
  expect_null(plan$cohort$spec)

  h <- shc_handle(); on.exit(cleanup_handle(h))
  syms <- stats::setNames(paste0("D_", names(plan$outputs)), names(plan$outputs))
  withr::with_options(list(nfilter.subset = 5), {
    expect_error(
      suppressWarnings(.planExecute(h, plan, syms)),
      "Disclosive|insufficient individuals|disclosure threshold")
  })
})

test_that("cleanup: the genuine inline-concept spec path still executes (kept branch)", {
  # ds.omop.plan.cohort(spec=) emits plan$cohort$spec (no filter_tree); the
  # server's reserved `else if (!is.null(plan$cohort$spec))` branch must still
  # build the cohort via .cohortCreate. This is the non-recipe API the cleanup
  # deliberately preserved. Plan shape mirrors the executable plan in test-plan.R,
  # with an inline-concept cohort added so the kept branch runs.
  h <- shc_handle(); on.exit(cleanup_handle(h))
  plan <- list(
    cohort = list(type = "spec",
                  spec = list(type = "condition", concept_set = c(201820))),
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      conditions = list(
        type = "event_level",
        table = "condition_occurrence",
        columns = NULL,
        concept_set = c(201820),
        representation = list(format = "long"),
        filters = list(concept_set = list(ids = c(201820)))
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE))
  class(plan) <- c("omop_plan", "list")
  withr::with_options(list(nfilter.subset = 0), {
    res <- suppressWarnings(.planExecute(h, plan, list(conditions = "cond_df")))
    expect_true(is.data.frame(res[["conditions"]]))
    expect_true(nrow(res[["conditions"]]) > 0)
  })
})

# ----------------------------------------------------------------------------
# (2) SIMPLICITY: the one-liner's bare name + domain mapping == the full path.
# ----------------------------------------------------------------------------

test_that("simplicity: bare-name + client domain mapping equals the full-id prevalence run", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if_not(.shc_client_ready(), "working-tree dsOMOPClient not loadable")
  cl <- asNamespace("dsOMOPClient")

  h <- shc_handle(); on.exit(cleanup_handle(h))
  withr::with_options(shc_opts(), {
    ct <- shc_diabetes(h)
    # Full-id path = exactly what ds.omop.analysis.run("dsomop:fe.prevalence")
    # sends for ds.omop.prevalence(domain = "drug").
    full <- .omopAnalysisRun(h, "dsomop:fe.prevalence",
                             params = list(domain_code = "1", top_n = 50L),
                             scope = ct)
    # Shortcut path = the BARE name + the client's own human-domain mapping.
    dc <- cl$.analysis_domain_code("drug", "0")
    expect_equal(dc, "1")
    bare <- .omopAnalysisRun(h, "prevalence",
                             params = list(domain_code = dc, top_n = 50L),
                             scope = ct)
    expect_equal(bare, full)            # identical gated frame, same numbers
  })
})

test_that("simplicity: bare-name + client mapping equals the full-id continuous run", {
  skip_if_not_installed("RSQLite"); skip_if_not_installed("pkgload")
  skip_if_not(.shc_client_ready(), "working-tree dsOMOPClient not loadable")
  cl <- asNamespace("dsOMOPClient")

  h <- shc_handle(); on.exit(cleanup_handle(h))
  withr::with_options(shc_opts(dist = 10), {
    ct <- shc_diabetes(h)
    full <- .omopAnalysisRun(h, "dsomop:fe.continuous",
                             params = list(metric = "measurement_value",
                                           domain_code = "3", top_n = 50L),
                             scope = ct)
    dc <- cl$.analysis_domain_code("measurement", "3")
    expect_equal(dc, "3")
    bare <- .omopAnalysisRun(h, "continuous",
                             params = list(metric = "measurement_value",
                                           domain_code = dc, top_n = 50L),
                             scope = ct)
    expect_equal(bare, full)
  })
})

# ----------------------------------------------------------------------------
# (2) SIMPLICITY: bare-name resolution + ambiguity + unknown (pure server).
# ----------------------------------------------------------------------------

test_that("simplicity: bare names resolve, ambiguous suffixes + unknowns fail closed", {
  h <- shc_handle(); on.exit(cleanup_handle(h))

  # Exact id, native id (drop the dsomop: prefix), and a unique suffix all land
  # on the same entry.
  expect_equal(.omopAnalysisResolve(h, "dsomop:fe.prevalence")$name,
               "dsomop:fe.prevalence")
  expect_equal(.omopAnalysisResolve(h, "fe.prevalence")$name,
               "dsomop:fe.prevalence")
  expect_equal(.omopAnalysisResolve(h, "prevalence")$name,
               "dsomop:fe.prevalence")
  expect_equal(.omopAnalysisResolve(h, "continuous")$name,
               "dsomop:fe.continuous")

  # An ambiguous suffix lists candidates rather than silently picking one
  # (prevalence_by_concept is shared by >= 2 era entries in the catalog).
  expect_error(.omopAnalysisResolve(h, "prevalence_by_concept"),
               "ambiguous")
  # Unknown name fails closed.
  expect_error(.omopAnalysisResolve(h, "no_such_analysis_xyz"),
               "not found")
})

# ----------------------------------------------------------------------------
# (2) SIMPLICITY: requires-cohort -> clear error (the one-liner's "fail-closed").
# ----------------------------------------------------------------------------

test_that("simplicity: fe.prevalence/fe.continuous un-scoped raise requires-cohort", {
  h <- shc_handle(); on.exit(cleanup_handle(h))
  withr::with_options(shc_opts(), {
    # These ARE the entries the one-liners target; run un-scoped they must fail
    # closed with the clear error (not a silent empty frame), even by bare name.
    expect_error(.omopAnalysisRun(h, "prevalence"),
                 "requires a cohort/population scope")
    expect_error(.omopAnalysisRun(h, "dsomop:fe.continuous"),
                 "requires a cohort/population scope")
  })
})
