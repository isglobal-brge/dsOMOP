# Reproduce CR-25 with the package's own SQLite helper, validator and executor.
# Usage: Rscript tools/ws2/probe_owned_cohort_reference.R <package-source> [ok|refused]
# Baseline source must be the immutable 2.10.2 pin; no validator is patched.
args <- commandArgs(TRUE)
if (!length(args)) stop("supply a package source directory")
src <- normalizePath(args[[1L]], mustWork = TRUE)
setwd(src)
suppressWarnings(suppressMessages(pkgload::load_all(src, helpers = TRUE,
                                                     export_all = TRUE, quiet = TRUE)))
local({
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  cohort <- .resolveCohortTable(handle, 1L)
  stopifnot(cohort %in% handle$temp_tables)
  plan <- list(
    populations = list(p = list(id = "p", kind = "criteria", cohort_definition_id = cohort)),
    outputs = list(demo = list(type = "baseline", columns = "gender_concept_id", population_id = "p")),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE))
  checked <- withr::with_options(list(nfilter.subset = 3, nfilter.tab = 1), {
    validation <- .planValidate(handle, plan)
    if (length(args) >= 2L && identical(args[[2L]], "ok")) {
      stopifnot(isTRUE(validation$valid), length(validation$errors) == 0L)
    }
    list(validation = validation,
         result = tryCatch(.planExecute(handle, plan, list(demo = "D_demo")),
                           error = identity))
  })
  validation <- checked$validation
  result <- checked$result
  ok <- !inherits(result, "error")
  dput(list(cohort_owned = TRUE, input = plan,
            validation = validation[c("valid", "errors", "warnings")], ok = ok,
            class = if (!ok) class(result) else NULL,
            code = if (!ok) result$code else NULL,
            message = if (!ok) conditionMessage(result) else NULL,
            result = if (ok) result else NULL))
  if (length(args) >= 2L && identical(args[[2L]], "ok")) {
    stopifnot(ok, identical(result$demo$person_id, c(1L, 3L, 5L, 7L, 9L, 11L)),
              identical(result$demo$gender_concept_id, rep(8507L, 6L)))
  } else if (length(args) >= 2L && identical(args[[2L]], "refused")) {
    stopifnot(identical(validation$valid, FALSE), length(validation$errors) > 0L,
              !ok, inherits(result, "dsomop_plan_rejection"),
              identical(result$code, "validation_error"))
  }
})
