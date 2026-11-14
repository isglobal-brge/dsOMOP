source(testthat::test_path("..", "..", "tools", "ws2", "ws2f-membership-results.R"), local = TRUE)

test_that("WS-2f B1 complete membership results retain the pristine baseline", {
  baseline <- readRDS(testthat::test_path("fixtures", "ws2f-membership-baseline.rds"))
  current <- .ws2f_membership_results()
  expect_identical(names(current), names(baseline))
  for (name in names(baseline)) expect_identical(current[[name]], baseline[[name]], info = name)
})


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
  for (id in list(1.5, "1", TRUE, list(1L), list(value = 1L), -2147483648, 2147483648)) {
    forged <- document
    forged$outputs$members$options["legacy_cohort_definition_id"] <- list(id)
    expect_false(.jsonSchemaValidate(.irSchema("ir-v2"), forged)$valid)
  }
  document$outputs$members$format <- "wide"
  expect_false(.jsonSchemaValidate(.irSchema("ir-v2"), document)$valid)
})

test_that("A-26 signed, null and absent labels survive every semantic projection", {
  for (other in c(FALSE, TRUE)) {
    inputs <- list(1L, 0L, -2L, "foo", NULL)
    irs <- lapply(inputs, function(id) .irPrepare(.ws2f_membership_plan(id, other)))
    expect_length(unique(vapply(irs, function(ir) .planSemanticDigest(ir)$sha256, "")), 5L)
    for (i in seq_along(irs)) {
      ir <- irs[[i]]
      for (doc in list(.irDocument(ir), .irExplain(ir)$plan, .irOutputClosure(ir, "members"),
                       .planNormalizeV2(.irDocument(ir), .irSchema("ir-v2")))) {
        opts <- doc$outputs$members$options
        expect_identical("legacy_cohort_definition_id" %in% names(opts), i != 5L)
        expect_identical(opts$legacy_cohort_definition_id, list(1L, 0L, -2L, NULL, NULL)[[i]])
      }
      expect_identical(.v1MembershipLabel(ir$outputs$members$options),
                       list(1L, 0L, -2L, NA_integer_, NULL)[[i]])
    }
    for (pair in list(list(-2L, -2.9), list(2L, "02"), list(2L, 2.9),
                     list(1L, TRUE), list("foo", 2147483648))) {
      expect_identical(.irDocument(.irPrepare(.ws2f_membership_plan(pair[[1]], other))),
                       .irDocument(.irPrepare(.ws2f_membership_plan(pair[[2]], other))))
    }
  }
  native <- list(version = 2L,
    meta = list(language_version = "2.0", semantic_version = "2.0.0", generated_by = "dsOMOPClient"),
    populations = list(p = list(kind = "criteria")),
    outputs = list(members = list(rows = "p", format = "membership", options = list())))
  for (id in list(NULL, 0L, -2L, 2147483647L, -2147483647L)) {
    native$outputs$members$options["legacy_cohort_definition_id"] <- list(id)
    expect_error(.irPrepare(native), "legacy_cohort_definition_id", class = "dsomop_plan_rejection")
  }
  doc <- .irDocument(.irPrepare(.ws2f_membership_plan("foo")))
  for (id in list(NULL, 0L, -2L, 2147483647L, -2147483647L)) {
    doc$outputs$members$options["legacy_cohort_definition_id"] <- list(id)
    expect_true(.jsonSchemaValidate(.irSchema("ir-v2"), doc)$valid)
  }
})

# Independent CR-27 baseline probe. Unmodified server 30b7905 and fixture.
cr27_plan <- function(id, different = FALSE) {
  p <- list(cohort=list(type="spec",cohort_definition_id=id,
    filter_tree=list(type="sex",params=list(value="M"))),
    outputs=list(members=list(type="cohort_membership")))
  if (different) {
    p$populations <- list(women=list(kind="criteria",filter_tree=list(type="sex",params=list(value="F"))))
    p$outputs$members$population_id <- "women"
  }
  p
}
cr27_run <- function(p) {
  h <- create_test_handle(); on.exit(cleanup_handle(h))
  v <- .planValidate(h,p)
  warnings <- character()
  r <- withCallingHandlers(tryCatch(.planExecute(h,p,list(members="D_members")),error=identity),
    warning=function(w) { warnings <<- c(warnings,conditionMessage(w)); invokeRestart("muffleWarning") })
  list(validation=v[c("valid","errors","warnings")], warnings=warnings,
       result=if(inherits(r,"error")) list(error=conditionMessage(r),class=class(r)) else r)
}

test_that("A-26 complete results and diagnostics equal the independent pristine baseline", {
  withr::local_options(list(nfilter.subset = 3, nfilter.tab = 1))
  baseline <- dget(testthat::test_path("fixtures", "a26-membership-baseline.R"))
  cases <- list(one=1L, two=2L, zero=0L, text="02", fraction=2.9, logical=TRUE,
    negative=-2L, negative_fraction=-2.9, bad_text="foo", overflow=2147483648, null=NULL)
  for (nm in names(cases)) {
    expect_identical(cr27_run(cr27_plan(cases[[nm]])), baseline[[nm]], info = nm)
  }
  p <- cr27_plan(NULL); p$cohort$cohort_definition_id <- NULL
  expect_identical(cr27_run(p), baseline$omitted)
  for (id in 1:2) {
    expect_identical(cr27_run(cr27_plan(id, TRUE)), baseline[[paste0("different_population", id)]])
  }
})
