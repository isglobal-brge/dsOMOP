# Keep the public surface explicit. The core endpoints never consult DP or the
# community-tools setting; the community-tool endpoints are disabled unless the
# custodian enables them, and follow exclusive DP when a custodian opts in.
.dp_core_endpoints <- c(
  "omopTableStatsDS", "omopColumnStatsDS", "omopDomainCoverageDS",
  "omopMissingnessDS", "omopValueCountsDS", "omopConceptPrevalenceDS",
  "omopCrossTabDS", "omopNumericRangeDS", "omopNumericHistogramDS",
  "omopNumericQuantilesDS", "omopDateCountsDS", "omopConceptDrilldownDS",
  "omopLocateConceptDS", "omopSafeCutpointsDS", "omopPlanPreviewDS",
  "omopCohortListDS", "omopCohortGetDefinitionDS", "omopFactorLevelsDS"
)
.dp_exclusive_endpoints <- c(
  "omopAchillesStatusDS", "omopAchillesResultsDS",
  "omopAchillesDistributionDS", "omopAchillesCatalogDS", "omopOhdsiStatusDS",
  "omopOhdsiTablesDS", "omopOhdsiResultsDS", "omopOhdsiSummaryDS",
  "omopQueryExecDS", "omopAnalysisRunDS", "omopAnalysisRunAssignDS"
)
# The analysis catalog and the query library choose a protection route per
# question (tools_auto.R): in DP-only mode their run path refuses every
# standard statistic, before any scope or data is touched, and still answers
# the reviewed questions that have a DP release.
.dp_routed_endpoints <- c("omopQueryExecDS", "omopAnalysisRunDS")

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
  .pkg_state$dp_generations <- NULL
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

test_that("exclusive DP rejects every community-tool endpoint first", {
  .dp_exclusive_local_policy()
  expect_identical(.dsomopToolsMode(), "dp")
  for (endpoint in setdiff(.dp_exclusive_endpoints, .dp_routed_endpoints)) {
    fn <- get(endpoint, envir = asNamespace("dsOMOP"))
    expect_error(fn(), "DP-only mode", info = endpoint)
    expect_error(
      eval(.dp_exclusive_probe(endpoint)),
      "DP-only mode", info = endpoint
    )
  }
  for (endpoint in .dp_routed_endpoints) {
    expect_error(
      eval(.dp_exclusive_probe(endpoint)),
      "endpoint argument evaluated", info = endpoint
    )
  }
})

test_that("core endpoints never consult DP, whatever its settings", {
  for (settings in list(
    list(dsomop.dp.enabled = TRUE, dsomop.dp.exclusive = TRUE),
    list(dsomop.dp.enabled = TRUE, dsomop.dp.exclusive = "invalid"),
    list(dsomop.dp.enabled = "invalid", dsomop.dp.exclusive = TRUE),
    list(dsomop.tools.enabled = FALSE)
  )) {
    withr::with_options(settings, {
      for (endpoint in .dp_core_endpoints) {
        expect_error(
          eval(.dp_exclusive_probe(endpoint)),
          "endpoint argument evaluated", info = endpoint
        )
      }
    })
  }
})

test_that("community tools are enabled by default; disabling refuses first", {
  withr::local_options(list(dsomop.tools.enabled = NULL,
                            default.dsomop.tools.enabled = NULL))
  expect_true(.dsomopToolsEnabled())
  withr::local_options(list(dsomop.tools.enabled = FALSE))
  expect_false(.dsomopToolsEnabled())
  for (endpoint in .dp_exclusive_endpoints) {
    fn <- get(endpoint, envir = asNamespace("dsOMOP"))
    argument_names <- setdiff(names(formals(fn)), "...")
    arguments <- stats::setNames(
      rep(list(quote(stop("endpoint argument evaluated", call. = FALSE))),
          length(argument_names)),
      argument_names
    )
    expect_error(
      eval(as.call(c(list(as.name(endpoint)), arguments))),
      "community tools .* are disabled on this server", info = endpoint
    )
  }
  withr::local_options(list(dsomop.tools.enabled = NULL,
                            default.dsomop.tools.enabled = FALSE))
  expect_false(.dsomopToolsEnabled())
})

test_that("nonexclusive DP retains every tool endpoint's argument path", {
  .dp_exclusive_local_policy(exclusive = FALSE)
  for (endpoint in .dp_exclusive_endpoints) {
    expect_error(
      eval(.dp_exclusive_probe(endpoint)),
      "endpoint argument evaluated", info = endpoint
    )
  }
})

test_that("exclusivity leaves tool endpoints open when DP is disabled", {
  .dp_exclusive_local_policy(enabled = FALSE)
  for (endpoint in .dp_exclusive_endpoints) {
    expect_error(
      eval(.dp_exclusive_probe(endpoint)),
      "endpoint argument evaluated", info = endpoint
    )
  }
})

test_that("exclusivity defaults off and supports custodial opt-ins", {
  .dp_exclusive_local_policy(exclusive = NULL)
  expect_false(.dsomopDpExclusive())
  expect_silent(.dsomopRequireStandardStatistics())

  withr::local_options(list(default.dsomop.dp.exclusive = TRUE))
  expect_true(.dsomopDpExclusive())
  expect_error(omopAchillesResultsDS(), "DP-only mode")

  withr::local_options(list(dsomop.dp.exclusive = FALSE))
  expect_false(.dsomopDpExclusive())
  expect_silent(.dsomopRequireStandardStatistics())

  withr::local_options(list(default.dsomop.dp.exclusive = FALSE,
                           dsomop.dp.exclusive = TRUE))
  expect_true(.dsomopDpExclusive())
  expect_error(omopAchillesResultsDS(), "DP-only mode")
})

test_that("invalid exclusivity settings fail closed before tool arguments", {
  .dp_exclusive_local_policy()
  for (invalid in list(NA, 2, "invalid", c(TRUE, FALSE), list(TRUE),
                       character(0))) {
    withr::with_options(list(dsomop.dp.exclusive = invalid), {
      expect_error(.dsomopDpExclusive(), "dsomop.dp.exclusive.*TRUE or FALSE")
      expect_error(
        omopAchillesResultsDS(stop("endpoint argument evaluated")),
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

test_that("capabilities keep the banded population in every DP mode", {
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
  expect_identical(omopGetCapabilitiesDS(symbol), baseline)
})
