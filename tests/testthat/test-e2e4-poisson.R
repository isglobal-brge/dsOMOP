# Replay the missing explanation in real-Opal run 43483695412 without
# depending on a particular noise root. Only the sampler outcome is controlled.
test_that("unavailable protected Poisson rates explain protected components", {
  .release_state()
  h <- create_test_handle(n_persons = 200)
  on.exit(cleanup_handle(h), add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = 1:200, person_id = 1:200,
    observation_period_start_date = "2015-01-01",
    observation_period_end_date = "2024-12-31",
    period_type_concept_id = 44814724L), append = TRUE)
  .release_dated_cohort(h, "rr_t", 1:100, start = "2020-01-01",
                        end = "2023-12-31")
  .release_dated_cohort(h, "rr_c", 101:200, start = "2020-01-01",
                        end = "2023-12-31")
  for (person in seq(1, 200, 4)) {
    .release_add_condition(h, person, 4000002L, "2020-06-01")
  }
  expect_true(suppressMessages(.dsomopToolsDpReady(h))$ready)
  fault <- "T"
  local_mocked_bindings(.dsomopDpDiscreteLaplace = function(key, context, ...) {
    hit <- identical(context$family, fault) &&
      (fault != "status" || grepl("[|]status=event$", context$coordinate))
    .dsomopBigInteger(if (hit) -1000000L else 0L)
  })
  params <- list(outcome_concept_id = "4000002", model_type = "poisson",
                 tar_start_offset = "0", tar_end_offset = "365",
                 tar_anchor_start = "start", tar_anchor_end = "start")
  run <- function() .release_run(h, "dsomop:cm.effect_estimate", params,
                                 list("rr_t", "rr_c"))
  reason <- NULL
  for (mode in c("auto", "standard")) {
    withr::local_options(list(dsomop.tools.mode = mode))
    suppressWarnings(.omopAnalysisRegistry(h, force = TRUE))
    for (component in c("T", "status")) {
      fault <- component
      out <- run()
      expect_equal(nrow(out), 2L)
      expect_true(all(is.na(out$estimate)))
      expect_true(all(out$model_type == "poisson"))
      label <- attr(out, "dsomop_protection")
      expect_match(label$unavailable_reason, "protected Poisson rate ratio is unavailable")
      if (is.null(reason)) reason <- label$unavailable_reason
      expect_identical(label$unavailable_reason, reason)
      expect_identical(run(), out)
      expect_identical(attr(unserialize(serialize(out, NULL)),
                            "dsomop_protection")$unavailable_reason, reason)
    }
    fault <- "none"
    finite <- run()
    expect_true(all(is.finite(finite$estimate)))
    expect_null(attr(finite, "dsomop_protection")$unavailable_reason)
  }
})
