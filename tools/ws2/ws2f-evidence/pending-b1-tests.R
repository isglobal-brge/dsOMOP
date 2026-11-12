test_that("WS-2f B1 the released root identifier is semantic on every membership output", {
  for (other in c(FALSE, TRUE)) {
    irs <- lapply(1:2, function(id) .irPrepare(.ws2f_membership_plan(id, other)))
    for (id in 1:2) {
      ir <- irs[[id]]
      expect_identical(ir$outputs$members$options$legacy_cohort_definition_id, id)
      expect_identical(.irExplain(ir)$plan$outputs$members$options$legacy_cohort_definition_id, id)
      expect_identical(.irExplain(ir)$semantic_digest, .planSemanticDigest(ir)$sha256)
      sliced <- .irOutputClosure(ir, "members")
      expect_identical(sliced$outputs$members$options$legacy_cohort_definition_id, id)
      if (other) {
        expect_identical(names(sliced$populations), "women")
        expect_null(sliced$populations$women$cohort_definition_id)
      }
    }
    expect_false(identical(.irDocument(irs[[1L]]), .irDocument(irs[[2L]])))
    expect_false(identical(.planSemanticDigest(irs[[1L]]), .planSemanticDigest(irs[[2L]])))
    expect_false(identical(.planSemanticDigest(.irOutputClosure(irs[[1L]], "members")),
                           .planSemanticDigest(.irOutputClosure(irs[[2L]], "members"))))
  }
})

test_that("WS-2f B1 membership IDs preserve historical conversion and absence", {
  one <- .irPrepare(.ws2f_membership_plan(1L))
  for (id in list("01", "1.9", 1.9, list(1L))) {
    ir <- .irPrepare(.ws2f_membership_plan(id))
    expect_identical(ir$outputs$members$options$legacy_cohort_definition_id, 1L)
    expect_identical(.planSemanticDigest(ir), .planSemanticDigest(one))
  }
  zero <- .irPrepare(.ws2f_membership_plan(0L))
  expect_identical(zero$outputs$members$options$legacy_cohort_definition_id, 0L)
  absent <- .ws2f_membership_plan()
  absent$cohort$cohort_definition_id <- NULL
  missing <- .irPrepare(absent)
  expect_false("legacy_cohort_definition_id" %in% names(missing$outputs$members$options))
  absent$cohort["cohort_definition_id"] <- list(NULL)
  expect_identical(.planSemanticDigest(.irPrepare(absent)), .planSemanticDigest(missing))
  expect_false(identical(.planSemanticDigest(missing), .planSemanticDigest(zero)))
  results_zero <- .ws2f_membership_plan(0L)
  results_zero$cohort <- list(type = "cohort_table", cohort_definition_id = 0L)
  expect_error(.irPrepare(results_zero), "0 is refused", class = "dsomop_plan_rejection")
  # A-16's independent inline-spec field remains intact, and the membership
  # value is also explicit for inline-spec and results-cohort dispatches.
  for (cohort in list(list(type = "spec", spec = list(type = "condition", concept_set = 201820L),
                           cohort_definition_id = 2L),
                     list(type = "cohort_table", cohort_definition_id = 2L))) {
    plan <- .ws2f_membership_plan()
    plan$cohort <- cohort
    ir <- .irPrepare(plan)
    expect_identical(ir$outputs$members$options$legacy_cohort_definition_id, 2L)
    if (!is.null(cohort$spec)) {
      expect_identical(ir$populations$base$index$source$legacy_spec$cohort_definition_id, 2L)
    }
  }
})

test_that("WS-2f B1 membership compatibility option is closed and IR-only", {
  native <- list(version = 2L,
    meta = list(language_version = "2.0", semantic_version = "2.0.0", generated_by = "dsOMOPClient"),
    populations = list(p = list(kind = "criteria")),
    outputs = list(members = list(rows = "p", format = "membership")))
  expect_s3_class(.irPrepare(native), "dsomop_ir")
  native$outputs$members$options <- list(legacy_cohort_definition_id = 1L)
  expect_error(.irPrepare(native), "legacy_cohort_definition_id", class = "dsomop_plan_rejection")
  document <- .irDocument(.irPrepare(.ws2f_membership_plan()))
  for (id in list(NULL, -1L, 1.5, "1", TRUE, list(1L), list(value = 1L))) {
    forged <- document
    forged$outputs$members$options["legacy_cohort_definition_id"] <- list(id)
    expect_false(.jsonSchemaValidate(.irSchema("ir-v2"), forged)$valid)
  }
  document$outputs$members$format <- "wide"
  expect_false(.jsonSchemaValidate(.irSchema("ir-v2"), document)$valid)
})
