# WS-2 tooling (dsOMOP 2.10.3): write the golden IR of every corpus plan.
#
# usage: Rscript tools/ws2/make_ir_golden.R <dsOMOP source dir>
#
# Loads the package from source, runs the frozen validation sequence
# (.irPrepare, no site) on every plan of tests/testthat/fixtures/
# ir-v1-corpus.json and writes tests/testthat/fixtures/ir-v1-golden.json: per
# plan the outcome ("ir", "no_ir" for a v1 plan without outputs, or
# "refused" with the typed rejection message), and for "ir" the normalised IR
# document, its semantic digest (.planSemanticDigest) and its advisory annex.
# These are regression snapshots of the implementation, not result evidence
# (test-ir-results.R compares results with the 2.10.2 baseline). Review the
# diff of the golden file whenever the lowering changes; test-ir-corpus.R pins
# it.
args <- commandArgs(TRUE)
pkg <- if (length(args)) args[[1L]] else "."
suppressMessages(pkgload::load_all(pkg, quiet = TRUE, export_all = TRUE, helpers = FALSE))
fixtures <- file.path(pkg, "tests", "testthat", "fixtures")
corpus <- jsonlite::fromJSON(file.path(fixtures, "ir-v1-corpus.json"), simplifyVector = FALSE)
golden <- lapply(corpus, function(rec) {
  res <- tryCatch({
    # captured execution plans may carry server-injected frames: validate the
    # wire part, as .planExecute() does
    wire <- .planWirePart(rec$plan)
    ir <- .irPrepare(wire$plan, handle = NULL, scope_tables = wire$scope_tables)
    if (is.null(ir)) {
      list(outcome = "no_ir")
    } else {
      doc <- .irDocument(ir)
      list(outcome = "ir", digest = .planSemanticDigest(doc)$sha256,
           ir = doc, annex = attr(ir, "legacy_annex"))
    }
  }, dsomop_plan_rejection = function(e) list(outcome = "refused", message = conditionMessage(e)))
  c(list(id = rec$id), res)
})
json <- jsonlite::toJSON(golden, auto_unbox = TRUE, null = "null", digits = NA)
writeLines(as.character(json), file.path(fixtures, "ir-v1-golden.json"))
cat("wrote", length(golden), "golden entries\n")
print(table(vapply(golden, `[[`, "", "outcome")))
