# Replay the zero protected person-time observed in real-Opal run 43464880783.
# Fixed sampler outcomes exercise availability without depending on a random root.
test_that("unavailable protected MDRR has a stable reason on every route and alias", {
  .release_state()
  h <- create_test_handle(n_persons = 200)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = 1:200, person_id = 1:200,
    observation_period_start_date = "2015-01-01",
    observation_period_end_date = "2024-12-31",
    period_type_concept_id = 44814724L), append = TRUE)
  .release_dated_cohort(h, "mdrr_t", 1:100, start = "2020-01-01",
                        end = "2023-12-31")
  .release_dated_cohort(h, "mdrr_c", 101:200, start = "2020-01-01",
                        end = "2023-12-31")
  for (person in seq(1, 200, 4)) {
    .release_add_condition(h, person, 4000002L, "2020-06-01")
  }
  expect_true(suppressMessages(.dsomopToolsDpReady(h))$ready)
  fault <- "T"
  local_mocked_bindings(.dsomopDpDiscreteLaplace = function(key, context, ...) {
    .dsomopBigInteger(if (identical(context$family, fault)) -1000000L else 0L)
  })
  params <- list(outcome_concept_id = "4000002", tar_start_offset = "0",
                 tar_end_offset = "365", tar_anchor_start = "start",
                 tar_anchor_end = "start")
  run <- function(id) .release_run(h, id, params, list("mdrr_t", "mdrr_c"))
  ids <- c("dsomop:cm.mdrr", "dsomop:ohdsi.cohort_method.cm_result",
           "dsomop:cm.diagnostics_summary",
           "dsomop:ohdsi.cohort_method.cm_diagnostics_summary")
  reason <- NULL
  for (mode in c("auto", "standard")) {
    withr::local_options(list(dsomop.tools.mode = mode))
    suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
    for (id in ids) {
      out <- run(id)
      expect_gt(nrow(out), 0L)
      expect_true(all(is.na(out$mdrr)))
      label <- attr(out, "dsomop_protection")
      expect_match(label$unavailable_reason, "protected.*MDRR.*unavailable")
      if (is.null(reason)) reason <- label$unavailable_reason
      expect_identical(label$unavailable_reason, reason)
      expect_identical(run(id), out)
      # R serialization is the attribute-bearing transport used by DSI.
      expect_identical(attr(unserialize(serialize(out, NULL)),
                            "dsomop_protection")$unavailable_reason, reason)
    }
  }
  withr::local_options(list(dsomop.tools.mode = "auto"))
  suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
  # A noisy person total below the display floor has the same public reason.
  fault <- "N"
  low <- run(ids[[1L]])
  expect_true(all(is.na(low$mdrr)))
  expect_identical(attr(low, "dsomop_protection")$unavailable_reason, reason)
  fault <- "none"
  finite <- run(ids[[1L]])
  expect_true(all(is.finite(finite$mdrr)))
  expect_null(attr(finite, "dsomop_protection")$unavailable_reason)
})
