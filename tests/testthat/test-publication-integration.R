test_that("public plans transport authenticated inventories and revoke after status deletion", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .setLegacyTestPersonKey(handle, "publication-integration", .local_envir = environment())
  .setHandle("publication_handle", handle)
  on.exit(.removeHandle("publication_handle"), add = TRUE)
  target <- new.env(parent = environment())
  target$D <- "user-owned"
  plan <- list(outputs = list(people = list(
    type = "event_level", table = "person",
    columns = c("person_id", "gender_concept_id"), representation = list(format = "long")
  )), options = list(translate_concepts = FALSE, block_sensitive = TRUE))
  withr::local_options(list(nfilter.subset = 3L))
  target$exec <- evalq(omopPlanExecuteDS("publication_handle", plan, c(people = "D")), target)
  status <- evalq(omopPlanStatusDS("exec"), target)
  expect_identical(names(status), c("status", "published", "rejected", "unavailable"))
  expect_identical(status$status, "ok")
  expect_null(attr(status, "execution_id"))
  expect_identical(target$D, "user-owned")
  symbol <- status$published$people$people$symbol
  expect_identical(symbol, "D.1")
  expect_true(all(grepl("^p2", get(symbol, target)$person_id)))
  forged <- status
  class(forged) <- "dsomop_plan_status"
  target$forged <- forged
  expect_error(evalq(omopPlanStatusDS("forged"), target), "server-authenticated")
  rm("exec", envir = target)
  expect_true(evalq(omopStagedRevokeDS("exec"), target))
  expect_false(exists(symbol, target, inherits = FALSE))
  expect_identical(target$D, "user-owned")
})

test_that("federation rollback removes reviewed factor recodes and preserves replacements", {
  ctx <- .publication_test_context(target = new.env(parent = environment()))
  frame <- data.frame(person_id = 1:6, gender_concept_id = rep(c(1L, 2L), each = 3L))
  .publishPrepare(ctx, .publication_test_candidate(), frame, already_tokenized = TRUE)
  .publishPrepare(ctx, .publication_test_candidate(root = "identical_copy"), frame,
                  already_tokenized = TRUE)
  ctx$target_env$exec <- .publication_test_commit(ctx)
  evalq(omopPlanStatusDS("exec"), ctx$target_env)
  before <- ctx$target_env$out
  after <- before
  after$gender_concept_id <- factor(after$gender_concept_id)
  .publishUpdateOwnedValue(before, after)
  ctx$target_env$out <- after
  expect_true(evalq(omopStagedRevokeDS("exec"), ctx$target_env))
  expect_false(exists("out", ctx$target_env, inherits = FALSE))
  expect_false(exists("identical_copy", ctx$target_env, inherits = FALSE))
  expect_false(exists("exec", ctx$target_env, inherits = FALSE))
  expect_error(evalq(omopPlanStatusDS("exec"), ctx$target_env), "unavailable")
  expect_true(evalq(omopStagedRevokeDS("exec"), ctx$target_env))

  ctx <- .publication_test_context()
  .publishPrepare(ctx, .publication_test_candidate(), frame, already_tokenized = TRUE)
  .publication_test_commit(ctx)
  ctx$target_env$out <- "independent replacement"
  .publishAbort(ctx)
  expect_identical(ctx$target_env$out, "independent replacement")
})

test_that("publication retains the keys needed for declared patient lineage", {
  ctx <- .publication_test_context()
  candidate <- .publication_test_candidate()
  candidate$private_columns <- "person_id"
  expect_error(.publishPrepare(ctx, candidate, data.frame(person_id = 1:5)),
               "lineage_malformed")
  expect_identical(ls(ctx$target_env), character(0))
})

test_that("authorized in-place publication restores the old binding on failure", {
  target <- new.env(parent = emptyenv())
  target$out <- "original"
  ctx <- .publication_test_context(target = target)
  .publishAllowReplacement(ctx, "out")
  .publishPrepare(ctx, .publication_test_candidate(), data.frame(person_id = 1:5), already_tokenized = TRUE)
  ctx$failure_hook <- function(step) if (step == "assign") stop("injected replacement failure")
  expect_error(.publication_test_commit(ctx), "injected replacement failure")
  expect_identical(target$out, "original")
})

test_that("publication closes private state even when rollback itself fails", {
  ctx <- .publication_test_context()
  .publishPrepare(ctx, .publication_test_candidate(), data.frame(person_id = 1:5), already_tokenized = TRUE)
  ctx$failure_hook <- function(step) if (step == "assign") {
    lockEnvironment(ctx$target_env, bindings = TRUE)
    stop("injected assignment failure")
  }
  error <- tryCatch(.publication_test_commit(ctx), error = identity)
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "cleanup failed")
  expect_true(ctx$closed)
  expect_false(exists(ctx$id, .planContextRegistry, inherits = FALSE))
  expect_identical(ls(ctx$staging_env), character(0))
  # A locked environment cannot be unlocked; detach it after proving that the
  # cleanup failure is surfaced so the test's deferred cleanup can complete.
  ctx$target_env <- new.env(parent = emptyenv())
  .publishAbort(ctx)
})

test_that("patient roots may use names also used for reference companions", {
  for (root in c("covariateRef", "timeRef", "transitionRef", "transition_ref")) {
    ctx <- .publication_test_context()
    ctx$return_private <- TRUE
    ctx$ir <- list(outputs = stats::setNames(list(list(type = "event_level")), root))
    status <- .publishOutputs(ctx, stats::setNames(list(data.frame(person_id = 1:5)), root),
                               stats::setNames(list(root), root))
    expect_identical(status$status, "ok")
    expect_identical(get(root, ctx$target_env), data.frame(person_id = 1:5))
  }
})

test_that("private computed inputs are not gated or published", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .setLegacyTestPersonKey(handle, "publication-integration", .local_envir = environment())
  target <- new.env(parent = emptyenv())
  plan <- list(publish = "final", outputs = list(private = list(), final = list()))
  ctx <- .planContext(handle, plan, target_env = target)
  on.exit(.planContextClose(ctx), add = TRUE)
  withr::local_options(list(nfilter.subset = 5L))
  status <- .publishOutputs(ctx, list(private = data.frame(person_id = 1L),
                                    final = data.frame(person_id = 1:5)),
                             list(private = "private", final = "final"))
  expect_identical(status$status, "ok")
  expect_identical(names(status$published), "final")
  expect_identical(ls(target), "final")
})

test_that("an explicitly empty publication root list publishes nothing", {
  ctx <- .publication_test_context()
  ctx$ir <- list(publish = list(), outputs = list(private = list()))
  status <- .publishOutputs(ctx, list(private = data.frame(person_id = 1L)),
                             list(private = "private"))
  expect_identical(status$status, "ok")
  expect_length(status$published, 0L)
  expect_length(ctx$prepared, 0L)
  expect_identical(ls(ctx$target_env), character(0))
})
