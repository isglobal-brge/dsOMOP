# The frozen registry is an independent release-boundary contract. Walk actual
# function bodies so adding a new assign method cannot silently escape review.
.boundaryAuditSources <- function() {
  paths <- list.files(testthat::test_path("..", "..", "R"),
                      pattern = "\\.R$", full.names = TRUE)
  functions <- list()
  for (path in paths) {
    for (expr in parse(path)) {
      if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
          is.call(expr[[3L]]) &&
          identical(expr[[3L]][[1L]], as.name("function"))) {
        functions[[as.character(expr[[2L]])]] <- expr[[3L]][[3L]]
      }
    }
  }
  functions
}

.boundaryAuditReaches <- function(functions, start, boundary) {
  pending <- start
  seen <- character(0)
  while (length(pending)) {
    current <- pending[[1L]]
    pending <- pending[-1L]
    if (current %in% seen) next
    if (current %in% boundary) return(TRUE)
    seen <- c(seen, current)
    if (!is.null(functions[[current]])) {
      calls <- all.names(functions[[current]], functions = TRUE, unique = TRUE)
      pending <- union(pending, intersect(calls, names(functions)))
    }
  }
  FALSE
}

test_that("every registered assign method has an audited release boundary", {
  registry <- utils::read.csv(
    testthat::test_path("..", "..", "inst", "boundaries.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  methods <- read.dcf(testthat::test_path("..", "..", "DATASHIELD"))
  assign_methods <- trimws(strsplit(methods[1L, "AssignMethods"], ",")[[1L]])
  expect_false(anyDuplicated(registry[["function"]]) > 0L)
  expect_setequal(setdiff(assign_methods, registry[["function"]]), character(0))
  pending <- registry[["function"]] %in% c("omopPlanExplainDS")
  expect_true(all(registry$status[pending] == "skeleton"))
  expect_true(all(registry$status[!pending] == "implemented"))
  for (note in registry$notes[!pending]) {
    covering_test <- regmatches(note, regexpr("test-[A-Za-z0-9-]+\\.R", note))
    expect_length(covering_test, 1L)
    expect_true(file.exists(testthat::test_path(covering_test)))
  }
  functions <- .boundaryAuditSources()
  for (method in assign_methods) {
    row <- registry[registry[["function"]] == method, , drop = FALSE]
    expect_equal(nrow(row), 1L, info = method)
    expect_identical(row$kind, "assign", info = method)
    expect_true(method %in% names(functions), info = method)
    if (identical(row$publisher, ".publishOutputs")) {
      expect_true(.boundaryAuditReaches(
        functions, method, c(".publishOutputs", ".publishStandalone")
      ), info = paste(method, "must pass through publication"))
    } else if (grepl("creation gate", row$publisher, fixed = TRUE)) {
      expect_true(.boundaryAuditReaches(functions, method, ".assertMinPersons"),
                  info = paste(method, "retains its standalone creation gate"))
    } else {
      expect_true(method %in% c("omopInitDS", "omopCleanupDS", "omopAsFactorColumnsDS"),
                  info = paste(method, "needs an explicit non-patient exemption"))
    }
  }
  expect_true("omopStagedDatasetPath" %in% registry[["function"]])
  expect_true(.boundaryAuditReaches(functions, "omopStagedDatasetPath",
                                   ".validateStagedManifestMembership"))
})

test_that("person support gates occur only at audited boundaries", {
  functions <- .boundaryAuditSources()
  gated <- names(functions)[vapply(functions, function(body) {
    ".assertMinPersons" %in% all.names(body, functions = TRUE)
  }, logical(1L))]
  # Cohort handles remain gated for standalone callers; private calls supply
  # an authenticated context. Aggregate paths retain their own controls.
  creation <- c(".cohortCreate", ".cohortCombine", ".applyInclusionCriteria",
                ".resolveCohortTable", ".cohortFromTokenFrame")
  aggregate <- c(
    ".omopAnalysisDemoEntries", ".omopDiagAssertPersons", ".omopDiagIncidenceRate",
    ".omopDiagTimeDistribution", ".omopDiagFollowupDistribution",
    ".omopDemographicPrevalence", ".omopComorbidityDistribution",
    ".omopAgeDistribution", ".omopTimeInCohortDistribution", ".omopTwoPopCohorts",
    ".omopAnalysisResolveScope", ".getCapabilities", ".ohdsiCohortDxCohortCountFn",
    ".ohdsiCohortDxTimeSeriesFn", ".omopCmAttritionFn", ".omopCmAttritionEndpoints",
    ".omopPlpAttrition", ".omopPlpFitModel", ".omopSccsAttrition",
    ".omopSccsOutcomeRatePerMonth", ".omopSccsAssumptionChecks",
    ".assertNumericDistPersons", ".profileValueCounts", ".profileSafeCutpoints",
    ".prevalenceOneTable", ".profileConceptDrilldown", ".profileCrossTab"
  )
  publication <- c(".publishPrepare", ".publishPersons", ".publishStandalone")
  expect_setequal(setdiff(gated, c(creation, aggregate, publication)), character(0))
  for (helper in creation) {
    expect_true(".cohortPrivateContext" %in% all.names(functions[[helper]]),
                info = helper)
  }
})

test_that("private cohort intermediates are small without granting a public bypass", {
  handle <- create_test_handle(n_persons = 1L)
  on.exit(cleanup_handle(handle))
  DBI::dbExecute(handle$conn, "DELETE FROM condition_occurrence WHERE person_id <> 1")
  ctx <- .planContext(handle, list())
  on.exit(.planContextClose(ctx), add = TRUE)
  spec <- list(type = "condition", concept_set = 201820L)
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(.cohortCreate(handle, spec, cohort_id = 731L), "Disclosive")
    cohort <- .cohortCreate(handle, spec, cohort_id = 732L, ctx = ctx)
    expect_identical(.resolveCohortTable(handle, cohort, ctx = ctx), cohort)
    expect_error(.resolveCohortTable(handle, cohort), "Disclosive")
    empty <- .cohortCombine(handle, "setdiff", cohort, cohort,
                            new_name = "ws1_empty_private", ctx = ctx)
    expect_equal(nrow(.executeQuery(handle, paste0("SELECT * FROM ", empty))), 0L)
    expect_error(.cohortCombine(handle, "setdiff", cohort, cohort), "Disclosive")
    expect_error(.publishStandalone(data.frame(person_id = 1L),
                                     already_tokenized = TRUE), "Disclosive")
  })
  forged <- list(private = TRUE)
  expect_error(.cohortCreate(handle, spec, ctx = forged), "execution context")
  expect_error(.cohortCombine(handle, "union", "x", "y", ctx = forged),
               "execution context")
  expect_error(.resolveCohortTable(handle, NULL, ctx = forged), "execution context")
  expect_error(.applyInclusionCriteria(handle, "x", NULL, ctx = forged),
               "execution context")
  expect_error(.cohortFromTokenFrame(handle, NULL, ctx = forged), "execution context")
})


test_that("standalone patient releases reject incomplete lineage and keep private exclusions", {
  frame <- .testPseudonymize(data.frame(
    person_id = 1:5, group = c(rep("retained", 4), "private")
  ))
  withr::with_options(list(nfilter.subset = 3), {
    retained <- omopFilterDS(frame, "group", "==", "retained")
    expect_equal(nrow(retained), 4L)
    expect_error(omopFilterDS(frame, "group", "==", "private"),
                 "insufficient individuals")
    broken <- frame
    broken$person_id[1] <- NA_character_
    expect_error(omopSelectDS(broken, "group"), "lineage_malformed")
    expect_error(omopBindRowsDS(broken, frame), "lineage_malformed")
  })
})
