# Targeted subset of test-ir-corpus.R: every membership golden record.
# Kept as evidence so the full corpus need not run for this label-only change.
test_that("A-26 membership corpus documents, digests and annexes retain their goldens", {
  read_fixture <- function(name) jsonlite::fromJSON(testthat::test_path("fixtures", name), simplifyVector = FALSE)
  corpus <- read_fixture("ir-v1-corpus.json")
  golden <- read_fixture("ir-v1-golden.json")
  n <- 0L
  for (i in seq_along(golden)) {
    want <- golden[[i]]
    if (!any(vapply(want$ir$outputs, function(o) identical(o$format, "membership"), logical(1)))) next
    n <- n + 1L
    wire <- .planWirePart(corpus[[i]]$plan)
    ir <- .irPrepare(wire$plan, scope_tables = wire$scope_tables)
    expect_identical(.jcsCanonical(.irDocument(ir)), .jcsCanonical(want$ir), info = want$id)
    expect_identical(.planSemanticDigest(ir)$sha256, want$digest, info = want$id)
    annex <- attr(ir, "legacy_annex")
    expect_identical(if (is.null(annex)) "null" else .jcsCanonical(annex),
                     if (is.null(want$annex)) "null" else .jcsCanonical(want$annex), info = want$id)
    expect_true(.jsonSchemaValidate(.irSchema("ir-v2"), want$ir)$valid, info = want$id)
    cat("A26_GOLDEN", want$id, "\n")
  }
  expect_identical(n, 9L)
})
