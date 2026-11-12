# WS-2 acceptance (design-2.10.3 section 12.2): every v1 plan of the 2.10.x
# suites normalises to the compatibility IR -- the golden corpus.
#
# fixtures/ir-v1-corpus.json holds every distinct plan the 2.10.2 server and
# client suites build or execute (captured with tools/ws2/), with provenance;
# fixtures/ir-v1-golden.json the expected outcome of the frozen validation
# sequence for each (tools/ws2/make_ir_golden.R): the normalised IR document,
# its legacy residue and digest, or the typed refusal.

.irc_fixture <- function(name) {
  jsonlite::fromJSON(testthat::test_path("fixtures", name), simplifyVector = FALSE)
}

test_that("every corpus plan reproduces its golden IR or refusal", {
  corpus <- .irc_fixture("ir-v1-corpus.json")
  golden <- .irc_fixture("ir-v1-golden.json")
  expect_identical(vapply(corpus, `[[`, "", "id"), vapply(golden, `[[`, "", "id"))
  expect_gte(length(corpus), 400L)
  for (i in seq_along(corpus)) {
    rec <- corpus[[i]]
    want <- golden[[i]]
    got <- tryCatch({
      ir <- .irPrepare(rec$plan, handle = NULL)
      if (is.null(ir)) list(outcome = "no_ir") else list(outcome = "ir", ir = ir)
    }, dsomop_plan_rejection = function(e) list(outcome = "refused", message = conditionMessage(e)))
    expect_identical(got$outcome, want$outcome, info = rec$id)
    if (identical(want$outcome, "ir") && identical(got$outcome, "ir")) {
      expect_identical(.jcsCanonical(.irStripPrivate(got$ir)), .jcsCanonical(want$ir), info = rec$id)
      expect_identical(.irSemanticDigest(got$ir)$sha256, want$digest, info = rec$id)
      residue <- attr(got$ir, "legacy_residue")
      expect_identical(if (is.null(residue)) "null" else .jcsCanonical(residue),
                       if (is.null(want$residue)) "null" else .jcsCanonical(want$residue), info = rec$id)
      expect_true(.jsonSchemaValidate(.irSchema("ir-v2"), want$ir)$valid, info = rec$id)
    }
    if (identical(want$outcome, "refused")) {
      expect_identical(got$message, want$message, info = rec$id)
    }
  }
})

test_that("every plan the v1 executor runs normalises (no executable plan is refused)", {
  corpus <- .irc_fixture("ir-v1-corpus.json")
  golden <- .irc_fixture("ir-v1-golden.json")
  executed <- vapply(corpus, function(r) isTRUE(r$executed), logical(1))
  outcome <- vapply(golden, `[[`, "", "outcome")
  expect_gt(sum(executed), 50L)
  expect_true(all(outcome[executed] %in% c("ir", "no_ir")))
})

test_that("the corpus plans refused by the sequence are exactly the invalid ones", {
  corpus <- .irc_fixture("ir-v1-corpus.json")
  golden <- .irc_fixture("ir-v1-golden.json")
  refused <- golden[vapply(golden, function(g) identical(g$outcome, "refused"), logical(1))]
  reasons <- vapply(refused, `[[`, "", "message")
  # an unknown row-filter field (a never-executed round-trip fixture), an
  # unknown population filter type and an unsupported output type (tests that
  # assert the refusal) and a dictionary over outputs the plan lacks
  expect_true(all(grepl(paste("unknown filter field\\(s\\) children",
                              "unknown population filter type",
                              "Unsupported output type 'not_a_real_output'",
                              "dangling output", sep = "|"), reasons)))
  expect_false(any(vapply(corpus, function(r) isTRUE(r$executed), logical(1))[
    vapply(golden, function(g) identical(g$outcome, "refused"), logical(1))]))
})
