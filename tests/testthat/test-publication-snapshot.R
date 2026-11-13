# Publication and DP sealing are provisional until the entire read snapshot
# completes, including its final validation and rollback.

.publication_snapshot_fixture <- function(.local_envir = parent.frame()) {
  force(.local_envir)
  h <- create_test_handle(n_persons = 24)
  withr::defer(cleanup_handle(h), envir = .local_envir)
  .setHandle("publication_snapshot", h)
  withr::defer(.removeHandle("publication_snapshot"), envir = .local_envir)
  identity <- "test://resource-scoped/publication-snapshot"
  key <- .testPseudonymKey("publication-snapshot")
  resource_id <- substr(as.character(openssl::sha256(charToRaw(identity))),
                        1L, 32L)
  key_environment <- stats::setNames(paste(format(key), collapse = ""),
                                    paste0("DSOMOP_PSEUDONYM_KEY_", resource_id))
  withr::local_envvar(c(
    key_environment, DSOMOP_PSEUDONYM_PROVIDER = "scoped",
    DSOMOP_PSEUDONYM_EPOCH = "1", DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false",
    DSOMOP_PSEUDONYM_ROOT = "", DSOMOP_PSEUDONYM_KEY = ""
  ), .local_envir = .local_envir)
  h$person_key_identity <- identity
  h$person_key_id <- .personKeyId(key)
  h$person_key_provider <- "scoped"
  h$person_key_epoch <- 1L
  h$person_key_require_existing <- FALSE
  h$person_key_contract_version <- 1L
  withr::defer({
    for (id in ls(.publicationRegistry, all.names = TRUE)) {
      ctx <- get(id, .publicationRegistry, inherits = FALSE)
      if (identical(ctx$handle, h)) .publishAbort(ctx)
    }
  }, envir = .local_envir)
  h
}

.publication_snapshot_plan <- function() list(
  outputs = list(people = list(type = "event_level", table = "person",
    columns = c("person_id", "gender_concept_id"),
    representation = list(format = "long"))),
  options = list(translate_concepts = FALSE, block_sensitive = TRUE))

test_that("publication preserves the sealed person identity and established snapshot", {
  .release_state()
  h <- .publication_snapshot_fixture(.local_envir = environment())
  plan <- .publication_snapshot_plan()
  target <- new.env(parent = environment())
  ordinary <- suppressMessages(evalq(omopPlanExecuteDS(
    "publication_snapshot", plan, list(people = "ordinary")), target))
  prepared <- suppressMessages(evalq(omopDpPlanExecuteDS(
    "publication_snapshot", plan, list(people = "prepared")), target))
  expect_identical(ordinary$status, "ok")
  expect_identical(prepared$status, "ok")
  expect_null(attr(target$ordinary, "dsomop_dp_provenance", exact = TRUE))
  provenance <- .dsomopDpVerifyPersonLocal(target$prepared)
  expect_identical(provenance$read_snapshot, "serializable_transaction")
  expect_identical(provenance$release_person_namespace, "cdm_person_id_v1")
  policy <- .dsomopDpPolicy()
  ids <- .unhashPersonKey(target$prepared$person_id, .personKey(h))
  expect_identical(.dsomopDpSealedReleaseTokens(target$prepared,
    target$prepared$person_id), .dsomopReleasePersonTokens(
      .dsomopReleaseKeys(policy), policy, ids))
  expect_null(h$read_snapshot)
  expect_length(h$temp_tables, 0L)
})

for (boundary in c("validation", "rollback")) local({
  phase <- boundary
  for (prepare in c(FALSE, TRUE)) local({
    dp <- prepare
    test_that(paste("publication revokes", if (dp) "sealed" else "ordinary",
                    "rows after final snapshot", phase, "failure"), {
      .release_state()
      h <- .publication_snapshot_fixture(.local_envir = environment())
      plan <- .publication_snapshot_plan()
      target <- new.env(parent = environment())
      target$rows <- "unrelated pre-existing value"
      before_publications <- ls(.publicationRegistry, all.names = TRUE)
      before_contexts <- ls(.planContextRegistry, all.names = TRUE)
      before_temps <- h$temp_tables
      failed <- FALSE
      if (identical(phase, "validation")) {
        validate <- .dsomopSnapshotValidate
        local_mocked_bindings(.dsomopSnapshotValidate = function(ctx) {
          validate(ctx)
          failed <<- TRUE
          .dsomopSnapshotLost("injected final validation failure")
        })
      } else {
        rollback <- .dsomopSnapshotRollback
        local_mocked_bindings(.dsomopSnapshotRollback = function(ctx) {
          if (!failed) {
            failed <<- TRUE
            stop("injected final rollback failure")
          }
          rollback(ctx)
        })
      }
      expect_error(suppressMessages(evalq({
        if (dp) omopDpPlanExecuteDS("publication_snapshot", plan,
                                     list(people = "rows")) else
          omopPlanExecuteDS("publication_snapshot", plan,
                              list(people = "rows"))
      }, target)), "read snapshot was lost")
      expect_true(failed)
      expect_identical(ls(target, all.names = TRUE), "rows")
      expect_identical(target$rows, "unrelated pre-existing value")
      expect_identical(ls(.publicationRegistry, all.names = TRUE),
                       before_publications)
      expect_identical(ls(.planContextRegistry, all.names = TRUE), before_contexts)
      expect_identical(h$temp_tables, before_temps)
      expect_null(h$read_snapshot)
      expect_equal(DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM person")$n,
                   24)
    })
  })
})
