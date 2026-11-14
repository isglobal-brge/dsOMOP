# WS-2f B1: full historical membership records, shared by pristine-baseline
# capture and current regression tests. No validator/executor is replaced.
.ws2f_membership_plan <- function(id = 1L, other_population = FALSE) {
  plan <- list(cohort = list(type = "spec", cohort_definition_id = id,
    filter_tree = list(type = "sex", params = list(value = "M"))),
    outputs = list(members = list(type = "cohort_membership")))
  if (other_population) {
    plan$populations <- list(women = list(id = "women", kind = "criteria",
      filter_tree = list(type = "sex", params = list(value = "F"))))
    plan$outputs$members$population_id <- "women"
  }
  plan
}

.ws2f_membership_results <- function() {
  withr::local_options(list(nfilter.subset = 3, nfilter.tab = 1))
  records <- list()
  for (other in c(FALSE, TRUE)) for (id in list(1L, 2L, 0L, "01", "2.9")) {
    h <- create_test_handle()
    plan <- .ws2f_membership_plan(id, other)
    validation <- .planValidate(h, plan)
    testthat::expect_true(validation$valid)
    testthat::expect_length(validation$errors, 0L)
    testthat::expect_length(validation$warnings, 0L)
    result <- .planExecute(h, plan, list(members = "D_members"))
    testthat::expect_identical(nrow(result$members), if (other) 7L else 8L)
    testthat::expect_identical(result$members$cohort_definition_id,
                               rep(as.integer(id), nrow(result$members)))
    testthat::expect_identical(sort(result$members$subject_id),
                               if (other) seq(2L, 14L, 2L) else seq(1L, 15L, 2L))
    key <- paste(if (other) "women" else "root", typeof(id), id, sep = "_")
    records[[key]] <- list(plan = plan,
      validation = validation[c("valid", "errors", "warnings")], result = result)
    cleanup_handle(h)
  }
  # Missing ID is historically distinct from explicit zero: nonempty
  # membership cannot fill its ID column. Record the complete disposition.
  for (explicit_null in c(FALSE, TRUE)) {
    h <- create_test_handle()
    plan <- .ws2f_membership_plan()
    plan$cohort$cohort_definition_id <- NULL
    if (explicit_null) plan$cohort["cohort_definition_id"] <- list(NULL)
    validation <- .planValidate(h, plan)
    testthat::expect_true(validation$valid)
    error <- tryCatch(.planExecute(h, plan, list(members = "D_members")), error = identity)
    testthat::expect_s3_class(error, "error")
    records[[if (explicit_null) "null" else "absent"]] <- list(plan = plan,
      validation = validation[c("valid", "errors", "warnings")],
      error = list(class = class(error), message = conditionMessage(error)))
    cleanup_handle(h)
  }
  records
}
