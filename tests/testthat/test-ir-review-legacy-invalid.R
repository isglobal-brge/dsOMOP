# Raw v1 checks for inputs independently refused by the 2.10.2 executors.
.legacy_invalid_rejection <- function(plan) {
  tryCatch(.irPrepare(plan), dsomop_plan_rejection = identity)
}

.legacy_invalid_temporal <- function(type, start = -365L, end = 0L) {
  out <- list(type = type, table = "condition_occurrence",
              concept_set = list(201820L), bin_width = 30L,
              window_start = start, window_end = end)
  if (identical(type, "person_period")) {
    out$grain <- "episode"
    out$time_origin <- "index"
  }
  list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
       outputs = list(x = out))
}

test_that("legacy invalid windows and concept restrictions keep the pinned baseline refusals", {
  fixture <- jsonlite::fromJSON(testthat::test_path("fixtures", "ir-v1-compat.json"),
                                simplifyVector = FALSE)
  ids <- c("A.event_level.concept_set.item.empty_array",
           "A.event_level.concept_set.scalar.empty_array",
           "A.event_level.concept_set.concepts.item.empty_array",
           "A.event_level.filters.concept_set.ids.item.empty_array",
           "A.temporal_covariates.concept_set.item.empty_array",
           "A.temporal_covariates.window_start.boolean",
           "A.person_period.window_start.boolean")
  cases <- Filter(function(case) case$id %in% ids, fixture$cases)
  expect_length(cases, length(ids))
  for (case in cases) {
    expect_identical(case$baseline$status, "error", info = case$id)
    template <- fixture$templates[[case$template]]$template
    plan <- jsonlite::fromJSON(gsub("@V@", case$value, template, fixed = TRUE),
                              simplifyVector = FALSE)
    error <- .legacy_invalid_rejection(plan)
    expect_s3_class(error, "dsomop_plan_rejection")
    expect_identical(error$code, "validation_error", info = case$id)
    expect_match(paste(error$errors, collapse = "\n"), case$baseline$error$message,
                 fixed = TRUE, info = case$id)
  }
})

test_that("legacy temporal window ordering uses historical coercions and defaults", {
  for (type in c("temporal_covariates", "person_period")) {
    for (start in list(TRUE, "1", list(TRUE))) {
      error <- .legacy_invalid_rejection(.legacy_invalid_temporal(type, start, 0L))
      expect_s3_class(error, "dsomop_plan_rejection")
      expect_match(paste(error$errors, collapse = "\n"), "/outputs/x/window_start", fixed = TRUE)
    }
    # An omitted end still means zero. Equal offsets, negative numeric strings
    # and the FALSE/TRUE integer conversions remain accepted.
    expect_s3_class(.legacy_invalid_rejection(.legacy_invalid_temporal(type, TRUE, NULL)),
                    "dsomop_plan_rejection")
    for (offsets in list(list(TRUE, TRUE), list(FALSE, TRUE), list("-30", "0"))) {
      plan <- .legacy_invalid_temporal(type, offsets[[1L]], offsets[[2L]])
      integer_plan <- .legacy_invalid_temporal(type, as.integer(offsets[[1L]]),
                                               as.integer(offsets[[2L]]))
      expect_identical(.planSemanticDigest(.irPrepare(plan)),
                       .planSemanticDigest(.irPrepare(integer_plan)))
    }
    expect_s3_class(.irPrepare(.legacy_invalid_temporal(type, NULL, NULL)), "dsomop_ir")
  }
})

test_that("legacy output concept emptiness follows precedence and preserves feature meanings", {
  base <- list(outputs = list(e = list(type = "event_level", table = "condition_occurrence",
                                       columns = list("condition_concept_id"))))
  for (empty in list(list(), list(list()),
                     list(concepts = list(), include_descendants = TRUE))) {
    plan <- base
    plan$outputs$e$concept_set <- empty
    expect_s3_class(.legacy_invalid_rejection(plan), "dsomop_plan_rejection")
    # The effective filter wins, so an ignored empty output-level set is valid.
    plan$outputs$e$filters <- list(concept_set = list(ids = list("201820")))
    expect_s3_class(.irPrepare(plan), "dsomop_ir")
    plan$outputs$e$concept_set <- list(201820L)
    plan$outputs$e$filters$concept_set$ids <- empty
    error <- .legacy_invalid_rejection(plan)
    expect_s3_class(error, "dsomop_plan_rejection")
    expect_match(paste(error$errors, collapse = "\n"), "/outputs/e/filters/concept_set/ids", fixed = TRUE)
  }
  # An absent output restriction and nested empties alongside actual IDs keep
  # their historical meanings. Inner feature specs are a separate contract.
  expect_s3_class(.irPrepare(base), "dsomop_ir")
  plan <- base
  plan$outputs$e$concept_set <- list(list(), "201820")
  expect_s3_class(.irPrepare(plan), "dsomop_ir")
  for (concepts in list(list(), list(concepts = list()))) {
    plan <- base
    plan$outputs$e$columns <- NULL
    plan$outputs$e$representation <- list(format = "features", features = list(
      list(name = "n", type = "count", concept_set = concepts)))
    expect_s3_class(.irPrepare(plan), "dsomop_ir")
  }
  for (type in c("temporal_covariates", "person_period")) {
    for (empty in list(list(), list(list()), list(concepts = list()))) {
      plan <- .legacy_invalid_temporal(type)
      plan$outputs$x$concept_set <- empty
      expect_s3_class(.legacy_invalid_rejection(plan), "dsomop_plan_rejection")
    }
    plan <- .legacy_invalid_temporal(type)
    plan$outputs$x$concept_set <- NULL
    expect_s3_class(.irPrepare(plan), "dsomop_ir")
    plan$outputs$x$concept_set <- list(list(), "201820")
    expect_s3_class(.irPrepare(plan), "dsomop_ir")
  }
})
