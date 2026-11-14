# A-18: status transport of the section 11 adapter refusals. A rejected entry
# may carry a closed `reason` only with validation_error; the condition's raw
# message, field path and state never enter the status.

.reason_condition <- function(reason, code = "validation_error", field = "events/members") {
  tryCatch(.compileReject("event 'members': a member linked to an episode belongs to another person",
                          reason = reason, field = field, code = code),
           dsomop_plan_rejection = function(e) e)
}

test_that("a rejection entry carries the reason only for a vocabulary validation_error", {
  entry <- .rejectionEntry(.reason_condition("adapter_person_mismatch"))
  expect_identical(entry, list(code = "validation_error",
                               message = "Publication refused: validation_error.",
                               reason = "adapter_person_mismatch"))
  expect_identical(.rejectionEntry(.reason_condition("cost_currency_mixed"))$reason,
                   "cost_currency_mixed")
  # A-20 (j) interval bounds and A-28's closed date refusal vocabulary.
  for (reason in c("stockpile_row_cap", "reducer_row_cap", "date_spec_invalid",
                   "date_source_invalid", "date_pattern_invalid",
                   "date_policy_conflict", "date_parts_names")) {
    expect_identical(.rejectionEntry(.reason_condition(reason)),
                     list(code = "validation_error",
                          message = "Publication refused: validation_error.", reason = reason))
  }
  expect_identical(.REJECTION_REASONS, c("adapter_person_mismatch", "cost_currency_mixed",
                                         "stockpile_row_cap", "reducer_row_cap",
                                         "date_spec_invalid", "date_source_invalid",
                                         "date_pattern_invalid", "date_policy_conflict",
                                         "date_parts_names"))
  # the raw message and the field path never travel
  expect_false(any(grepl("member|events/", unlist(entry))))
  # an unknown construct, another code or a malformed reason: the field is omitted
  expect_identical(names(.rejectionEntry(.reason_condition("unit_unknown"))), c("code", "message"))
  expect_identical(.rejectionEntry(.reason_condition("adapter_person_mismatch", code = "unavailable_table")),
                   list(code = "unavailable_table", message = "Publication refused: unavailable_table."))
  e <- .reason_condition("adapter_person_mismatch")
  e$reason <- c("adapter_person_mismatch", "cost_currency_mixed")
  expect_identical(names(.rejectionEntry(e)), c("code", "message"))
  e$reason <- NA_character_
  expect_identical(names(.rejectionEntry(e)), c("code", "message"))
  e$reason <- 1L
  expect_identical(names(.rejectionEntry(e)), c("code", "message"))
  plain <- tryCatch(.publishReject("validation_error", "x"), dsomop_plan_rejection = function(e) e)
  expect_identical(.rejectionEntry(plain), list(code = "validation_error",
                                                message = "Publication refused: validation_error."))
})

test_that(".validatePlanStatus admits the closed reason only with validation_error", {
  good <- list(status = "refused", published = list(),
               rejected = list(x = list(code = "validation_error",
                                        message = .publishMessage("validation_error"),
                                        reason = "adapter_person_mismatch")),
               unavailable = character())
  expect_true(.validatePlanStatus(good))
  good$rejected$x$reason <- "cost_currency_mixed"
  expect_true(.validatePlanStatus(good))
  for (reason in c("stockpile_row_cap", "reducer_row_cap", "date_spec_invalid",
                   "date_source_invalid", "date_pattern_invalid",
                   "date_policy_conflict", "date_parts_names")) {
    ok <- good; ok$rejected$x$reason <- reason
    expect_true(.validatePlanStatus(ok))
  }
  bad <- good; bad$rejected$x$reason <- "unit_unknown"
  expect_error(.validatePlanStatus(bad), "Invalid rejection status")
  bad <- good; bad$rejected$x$reason <- 1L
  expect_error(.validatePlanStatus(bad), "Invalid rejection status")
  bad <- good; bad$rejected$x$reason <- NA_character_
  expect_error(.validatePlanStatus(bad), "Invalid rejection status")
  bad <- good
  bad$rejected$x$code <- "unavailable_table"
  bad$rejected$x$message <- .publishMessage("unavailable_table")
  expect_error(.validatePlanStatus(bad), "Invalid rejection status")
  bad <- good; bad$rejected$x$field <- "events/members"
  expect_error(.validatePlanStatus(bad), "Invalid rejection status")
  bad <- good; bad$rejected$x <- list(code = "validation_error", reason = "cost_currency_mixed",
                                      message = .publishMessage("validation_error"))
  expect_error(.validatePlanStatus(bad), "Invalid rejection status")
})

test_that("the shared handler transports the adapter reason without the condition's state", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .setHandle("publication_reason", handle)
  on.exit(.removeHandle("publication_reason"), add = TRUE)
  reason <- "adapter_person_mismatch"
  local_mocked_bindings(.planExecute = function(handle, plan, out_symbols, output_mode, ctx) {
    .compileReject("event 'members': a member linked to an episode belongs to another person",
                   reason = reason, field = "events/members")
  })
  target <- new.env(parent = emptyenv())
  # a plan that passes the validation sequence, which runs before the context
  # and before .planExecute() (WS-2: an invalid plan is refused there first)
  people <- list(type = "person_level", tables = list(person = list("gender_concept_id")))
  plan <- list(outputs = list(first = people, second = people))
  for (reason in .REJECTION_REASONS) {
    status <- .omopPlanExecuteDS("publication_reason", plan,
      list(first = "A", second = "B"), target_env = target)
    expect_identical(status$status, "refused")
    expect_identical(names(status$rejected), c("first", "second"))
    for (root in c("first", "second")) {
      expect_identical(status$rejected[[root]],
                       list(code = "validation_error",
                            message = "Publication refused: validation_error.",
                            reason = reason))
    }
    expect_length(status$published, 0L)
    expect_identical(ls(target), character(0))
    expect_true(.validatePlanStatus(status))
  }
  reason <- "unit_unknown"
  status <- .omopPlanExecuteDS("publication_reason", plan,
    list(first = "A", second = "B"), target_env = target)
  expect_identical(status$status, "refused")
  expect_identical(status$rejected$first,
                   list(code = "validation_error", message = "Publication refused: validation_error."))
  expect_identical(ls(target), character(0))
})
