test_that("public plans transport authenticated inventories and revoke after status deletion", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  set_test_person_key(handle)
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
  ctx$target_env$exec <- .publication_test_commit(ctx)
  evalq(omopPlanStatusDS("exec"), ctx$target_env)
  before <- ctx$target_env$out
  after <- before
  after$gender_concept_id <- factor(after$gender_concept_id)
  .publishUpdateOwnedValue(before, after)
  ctx$target_env$out <- after
  expect_true(evalq(omopStagedRevokeDS("exec"), ctx$target_env))
  expect_false(exists("out", ctx$target_env, inherits = FALSE))

  ctx <- .publication_test_context()
  .publishPrepare(ctx, .publication_test_candidate(), frame, already_tokenized = TRUE)
  .publication_test_commit(ctx)
  ctx$target_env$out <- "independent replacement"
  .publishAbort(ctx)
  expect_identical(ctx$target_env$out, "independent replacement")
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
    lockBinding("out", ctx$target_env)
    stop("injected assignment failure")
  }
  expect_error(.publication_test_commit(ctx), "cleanup failed")
  expect_true(ctx$closed)
  expect_false(exists(ctx$id, .planContextRegistry, inherits = FALSE))
  expect_identical(ls(ctx$staging_env), character(0))
  unlockBinding("out", ctx$target_env)
  .publishAbort(ctx)
})

test_that("private computed inputs are not gated or published", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  set_test_person_key(handle)
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
