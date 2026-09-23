# Keep the public surface explicit: adding a statistical aggregate requires a
# decision about the DP-exclusive gate as well as a registration entry.
.dp_exclusive_endpoints <- c(
  "omopTableStatsDS", "omopColumnStatsDS", "omopDomainCoverageDS",
  "omopMissingnessDS", "omopValueCountsDS", "omopConceptPrevalenceDS",
  "omopCrossTabDS", "omopNumericRangeDS", "omopNumericHistogramDS",
  "omopNumericQuantilesDS", "omopDateCountsDS", "omopConceptDrilldownDS",
  "omopLocateConceptDS", "omopSafeCutpointsDS", "omopPlanPreviewDS",
  "omopCohortListDS", "omopCohortGetDefinitionDS",
  "omopAchillesStatusDS", "omopAchillesResultsDS",
  "omopAchillesDistributionDS", "omopAchillesCatalogDS", "omopOhdsiStatusDS",
  "omopOhdsiTablesDS", "omopOhdsiResultsDS", "omopOhdsiSummaryDS",
  "omopQueryExecDS", "omopAnalysisRunDS", "omopFactorLevelsDS"
)

.dp_exclusive_local_policy <- function(enabled = TRUE, exclusive = TRUE,
                                        .local_envir = parent.frame()) {
  withr::local_envvar(c(DSOMOP_DP_ENABLED = NA_character_),
                     .local_envir = .local_envir)
  withr::local_options(list(
    dsomop.dp.enabled = enabled,
    dsomop.dp.exclusive = exclusive,
    default.dsomop.dp.exclusive = NULL
  ), .local_envir = .local_envir)
  previous_runtime <- .pkg_state$dp_runtime
  withr::defer(.pkg_state$dp_runtime <- previous_runtime,
              envir = .local_envir)
  .pkg_state$dp_runtime <- NULL
}

# Poison every supplied argument. A policy rejection must happen before a
# symbol, cached output, JSON argument, cohort or data frame can be inspected.
.dp_exclusive_probe <- function(endpoint) {
  fn <- get(endpoint, envir = asNamespace("dsOMOP"))
  argument_names <- setdiff(names(formals(fn)), "...")
  arguments <- stats::setNames(
    rep(list(quote(stop("endpoint argument evaluated", call. = FALSE))),
        length(argument_names)),
    argument_names
  )
  as.call(c(list(as.name(endpoint)), arguments))
}

test_that("exclusive DP rejects every standard statistical endpoint first", {
  .dp_exclusive_local_policy()
  for (endpoint in .dp_exclusive_endpoints) {
    fn <- get(endpoint, envir = asNamespace("dsOMOP"))
    expect_error(fn(), "DP-exclusive.*omopDpReleaseDS", info = endpoint)
    expect_error(
      eval(.dp_exclusive_probe(endpoint)),
      "DP-exclusive.*omopDpReleaseDS", info = endpoint
    )
  }
})

test_that("nonexclusive DP retains every standard endpoint's argument path", {
  .dp_exclusive_local_policy(exclusive = FALSE)
  for (endpoint in .dp_exclusive_endpoints) {
    expect_error(
      eval(.dp_exclusive_probe(endpoint)),
      "endpoint argument evaluated", info = endpoint
    )
  }
})

test_that("exclusivity leaves standard endpoints open when DP is disabled", {
  .dp_exclusive_local_policy(enabled = FALSE)
  for (endpoint in .dp_exclusive_endpoints) {
    expect_error(
      eval(.dp_exclusive_probe(endpoint)),
      "endpoint argument evaluated", info = endpoint
    )
  }
})

test_that("exclusivity defaults on and supports custodial opt-outs", {
  .dp_exclusive_local_policy(exclusive = NULL)
  expect_true(.dsomopDpExclusive())
  expect_error(omopTableStatsDS(), "DP-exclusive.*omopDpReleaseDS")

  withr::local_options(list(default.dsomop.dp.exclusive = FALSE))
  expect_false(.dsomopDpExclusive())
  expect_silent(.dsomopRequireStandardStatistics())

  withr::local_options(list(dsomop.dp.exclusive = TRUE))
  expect_true(.dsomopDpExclusive())
  expect_error(omopTableStatsDS(), "DP-exclusive.*omopDpReleaseDS")

  withr::local_options(list(default.dsomop.dp.exclusive = TRUE,
                           dsomop.dp.exclusive = FALSE))
  expect_false(.dsomopDpExclusive())
  expect_silent(.dsomopRequireStandardStatistics())
})

test_that("invalid exclusivity settings fail closed before endpoint arguments", {
  .dp_exclusive_local_policy()
  for (invalid in list(NA, 2, "invalid", c(TRUE, FALSE), list(TRUE),
                       character(0))) {
    withr::with_options(list(dsomop.dp.exclusive = invalid), {
      expect_error(.dsomopDpExclusive(), "dsomop.dp.exclusive.*TRUE or FALSE")
      expect_error(
        omopTableStatsDS(stop("endpoint argument evaluated")),
        "dsomop.dp.exclusive.*TRUE or FALSE"
      )
    })
  }
})

test_that("standard profiling outputs are unchanged outside exclusive DP", {
  .dp_exclusive_local_policy(enabled = FALSE, exclusive = FALSE)
  withr::local_options(list(
    nfilter.subset = 3, nfilter.tab = 3, dsomop.nfilter.band = 5
  ))
  handle <- create_test_handle(n_persons = 15)
  symbol <- "dp_exclusive_profile_fixture"
  .setHandle(symbol, handle)
  on.exit(.removeHandle(symbol), add = TRUE)

  profile <- function() list(
    counts = omopTableStatsDS(symbol, "person"),
    column = omopColumnStatsDS(symbol, "measurement", "value_as_number"),
    coverage = omopDomainCoverageDS(symbol),
    missingness = omopMissingnessDS(symbol, "person"),
    histogram = omopValueCountsDS(symbol, "person", "gender_concept_id"),
    empty_levels = omopFactorLevelsDS(data.frame())
  )
  baseline <- profile()
  expect_equal(baseline$counts$rows, 15)
  expect_true("mean" %in% names(baseline$column))
  expect_true("missing_rate" %in% names(baseline$missingness))

  withr::with_options(list(dsomop.dp.enabled = TRUE), {
    expect_identical(profile(), baseline)
  })
  withr::with_options(list(dsomop.dp.exclusive = TRUE), {
    expect_identical(profile(), baseline)
  })
})

test_that("structural and vocabulary metadata remain available in exclusive DP", {
  .dp_exclusive_local_policy(exclusive = FALSE)
  handle <- create_test_handle(n_persons = 15)
  symbol <- "dp_exclusive_metadata_fixture"
  .setHandle(symbol, handle)
  on.exit(.removeHandle(symbol), add = TRUE)

  metadata <- function() list(
    tables = omopListTablesDS(symbol),
    columns = omopListColumnsDS(symbol, "person"),
    relationships = omopRelationshipGraphDS(symbol),
    vocabularies = omopVocabulariesDS(symbol)
  )
  baseline <- metadata()
  expect_true("person" %in% baseline$tables$table_name)
  expect_true("person_id" %in% baseline$columns$column_name)
  expect_gt(nrow(baseline$vocabularies), 0L)

  withr::local_options(list(dsomop.dp.exclusive = TRUE))
  expect_identical(metadata(), baseline)
})

test_that("exclusive capabilities retain connection metadata without a population count", {
  .dp_exclusive_local_policy(enabled = FALSE, exclusive = FALSE)
  withr::local_options(list(nfilter.subset = 3, dsomop.nfilter.band = 5))
  handle <- create_test_handle(n_persons = 15)
  symbol <- "dp_exclusive_capabilities_fixture"
  .setHandle(symbol, handle)
  on.exit(.removeHandle(symbol), add = TRUE)

  baseline <- omopGetCapabilitiesDS(symbol)
  expect_true("total_persons" %in% names(baseline))
  expect_equal(baseline$total_persons, 15)
  withr::with_options(list(dsomop.dp.enabled = TRUE), {
    expect_identical(omopGetCapabilitiesDS(symbol), baseline)
  })
  withr::with_options(list(dsomop.dp.exclusive = TRUE), {
    expect_identical(omopGetCapabilitiesDS(symbol), baseline)
  })

  withr::local_options(list(dsomop.dp.enabled = TRUE,
                           dsomop.dp.exclusive = TRUE))
  structural <- omopGetCapabilitiesDS(symbol)
  expect_false("total_persons" %in% names(structural))
  baseline$total_persons <- NULL
  expect_identical(structural, baseline)
})
