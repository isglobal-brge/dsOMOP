test_that("DP-only sessions can prepare a pinned loader then release its fixed count", {
  .release_state()
  h <- .release_derived_handle(60)
  h$person_key_identity <- "test://e2e-loader"
  h$resource_client$close <- function() invisible(NULL)
  suppressWarnings(.omopAnalysisRegistry(h))
  .setHandle("e2e_loader", h)
  withr::defer(.removeHandle("e2e_loader"))
  .dsomopDpEnsureRuntime(h)
  load <- function() suppressMessages(omopAnalysisRunAssignDS(
    "e2e_loader", "dsomop:condition_occurrence.load"))
  ordinary <- load()
  spec <- list(statistic = "count")
  before <- suppressMessages(omopDpReleaseDS(ordinary, spec))
  expect_identical(before$protection, "fixed_synopsis")
  withr::local_options(list(dsomop.dp.exclusive = TRUE))
  private <- load()
  expect_s3_class(private, "omop.table")
  after <- suppressMessages(omopDpReleaseDS(private, spec))
  expect_identical(after, before)
  expect_error(omopAnalysisRunDS("e2e_loader", "dsomop:condition_era.length_stats"),
               "fixed-synopsis|DP-only")
  # The public endpoint still refuses an aggregate masquerading as assignment.
  expect_error(omopAnalysisRunAssignDS("e2e_loader", "dsomop:achilles.3"), "assign-mode")
  withr::local_options(list(dsomop.tools.enabled = FALSE))
  expect_error(omopAnalysisRunAssignDS(stop("argument forced"), stop("name forced")), "disabled")
})

test_that("all-null GiBleed measurements refuse, numeric unregistered values retain signed H", {
  .release_state()
  h <- create_test_handle(n_persons = 60)
  withr::defer(cleanup_handle(h))
  DBI::dbExecute(h$conn, "UPDATE measurement SET value_as_number = NULL")
  expect_error(.release_run(h, "dsomop:measurement.value_stats"), "Disclosive")
  # A nonempty numeric population is required to test the fallback; no gate is bypassed.
  DBI::dbExecute(h$conn, "UPDATE measurement SET value_as_number = 1, unit_concept_id = 999001")
  value <- .release_run(h, "dsomop:measurement.value_stats")
  expect_identical(attr(value, "dsomop_protection")$protection, "realised_sticky")
  expect_true(any(vapply(attr(value, "dsomop_release")$owners, function(o)
    any(vapply(o$components, function(c) length(c$h) > 0L, logical(1))), logical(1))))
  expect_identical(value, .release_run(h, "dsomop:measurement.value_stats"))
})

test_that("fingerprint chunk indexing preserves canonical framing across chunk boundaries", {
  .release_state()
  .dsomopDpEnsureRuntime()
  policy <- .dsomopDpPolicy()
  keys <- .dsomopReleaseKeys(policy)
  n <- 20003L
  tokens <- .dsomopReleasePersonTokens(keys, policy, rev(seq_len(n)))
  contributions <- rep(c(-7, 0, 3), length.out = n)
  # Independent oracle for the pre-optimization byte contract.
  ord <- order(tokens, method = "radix")
  records <- paste0(nchar(tokens[ord], type = "bytes"), ":", tokens[ord], ":",
                    sprintf("%.0f", contributions[ord]))
  chunks <- split(records, ceiling(seq_along(records) / 10000L))
  digests <- vapply(chunks, function(x) .dsomopDpHmac(keys$fingerprint,
    paste(c("dsomop-contribution-chunk-v2", length(x), x), collapse = "\n")), "")
  expected <- .dsomopDpHmac(keys$fingerprint, .dsomopDpCanonicalJson(list(
    protocol = "dsomop-contributions-v1", owner = "owner", family = "H",
    coordinate = "cell", n = n, chunks = paste(digests, collapse = ","))))
  actual <- .dsomopReleaseFingerprint(keys, "owner", "H", "cell", tokens, contributions)
  expect_identical(actual, expected)
  expect_identical(actual, .dsomopReleaseFingerprint(keys, "owner", "H", "cell",
                                                    rev(tokens), rev(contributions)))
  expect_error(.dsomopReleaseFingerprint(keys, "owner", "H", "cell",
                                         rep(tokens[1], 2), c(1, 2)), "twice")
  owner <- .omopReleaseBirthYearOwner()
  sub <- data.frame(person_id = c("1", "1"), stratum = owner$strata$keys[1:2])
  expect_error(.dsomopReleaseCapSubstrate(sub, owner), "two strata")
  sub$stratum <- sub$stratum[1]
  expect_error(.dsomopReleaseCapSubstrate(sub, owner), "twice in one stratum")
})
