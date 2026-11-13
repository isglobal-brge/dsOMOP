# ==============================================================================
# Language-2.0 standalone row verbs (design-2.10.3 sections 2.11, 4.6, 7.3,
# 10.3): omopMutateDS() over the frozen mutate-v1.json AST and the expr form of
# omopFilterDS() over the plan-v2 predicate grammar.
#
# Requests are data: validated against the frozen schema and type-checked
# against the frame before any row is evaluated, never evaluated as R code.
# Results are released objects and pass the standalone publication boundary
# (final distinct-person support >= nfilter.subset, constant message);
# protected keys are never read or written; DP provenance is invalidated.
# ==============================================================================

.v2v_table <- function(df) {
  dsOMOP:::.pseudonymizeIdentifiers(
    df, .testPseudonymKey(),
    pseudonymization = .testPublicPseudonymization(.testPseudonymKey())
  )
}

.v2v_frame <- function(n = 8L) {
  .v2v_table(data.frame(
    person_id = seq_len(n),
    age = c(10L, 17L, 18L, 40L, 64L, 65L, 80L, NA)[seq_len(n)],
    covered = c(4, 0, 10, 3, NA, 7, 2, 5)[seq_len(n)],
    observed = c(4, 0, 20, 6, 5, 0, 4, 10)[seq_len(n)],
    label = c("a", NA, "b", "a", "c", "b", NA, "a")[seq_len(n)],
    start = as.Date("2020-01-01") + c(0, 10, 20, 30, 40, 50, 60, 70)[seq_len(n)],
    stop = as.Date("2020-02-01") + c(0, 10, 20, 30, 40, 50, 60, 70)[seq_len(n)],
    flag = c(TRUE, FALSE, NA, TRUE, TRUE, FALSE, TRUE, FALSE)[seq_len(n)],
    stringsAsFactors = FALSE))
}

# Encode a request as the client does (B64 JSON).
.v2v_enc <- function(x) {
  json <- as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA))
  b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json)))
  paste0("B64:", gsub("=+$", "", chartr("+/", "-_", b64)))
}

.v2v_mutate <- function(x, ...) {
  assign <- lapply(list(...), function(a) list(name = a[[1L]], expr = a[[2L]]))
  omopMutateDS(x, .v2v_enc(list(version = 1L, assign = assign)))
}

.v2v_col <- function(n) list(col = n)
.v2v_lit <- function(v, as = NULL) c(list(lit = v), if (!is.null(as)) list(as = as))

test_that("every mutate-v1 probe is accepted or refused by the server exactly as frozen", {
  probes <- dsOMOP:::.jsonSchemaLoad(testthat::test_path("..", "..", "inst", "schema", "probes",
                                                         "mutate-v1-probes.json"))
  x <- .v2v_table(data.frame(person_id = 1:6, covered = c(1, 2, 3, 4, 5, 6),
                             observed = c(2, 2, 6, 4, 0, 12), age = c(5, 17, 18, 64, 65, 90),
                             label = c("x", NA, "y", "x", NA, "z"), a = 1:6, b = 6:1, c = 0,
                             stringsAsFactors = FALSE))
  withr::with_options(list(nfilter.subset = 3), {
    for (p in probes) {
      run <- tryCatch(omopMutateDS(x, .v2v_enc(p$instance)), error = function(e) e)
      if (identical(p$expect, "valid")) {
        expect_s3_class(run, "omop.table")
        new <- vapply(p$instance$assign, function(a) a$name, "")
        expect_true(all(new %in% names(run)), info = p$id)
      } else {
        expect_s3_class(run, "dsomop_plan_rejection")
        expect_match(conditionMessage(run), "invalid mutation request", info = p$id)
      }
    }
  })
})

test_that("arithmetic follows the frozen type rules: NA for division by zero and non-finite results", {
  x <- .v2v_frame()
  withr::with_options(list(nfilter.subset = 3), {
    r <- .v2v_mutate(x,
      list("pdc", list(op = "/", args = list(.v2v_col("covered"), .v2v_col("observed")))),
      list("age2", list(op = "*", args = list(.v2v_col("age"), .v2v_lit(2L, "integer")))),
      list("big", list(op = "*", args = list(.v2v_col("age"), .v2v_lit(.Machine$integer.max, "integer")))),
      list("days", list(op = "-", args = list(.v2v_col("stop"), .v2v_col("start")))),
      list("later", list(op = "+", args = list(.v2v_col("start"), .v2v_lit(7L, "integer")))))
  })
  expect_equal(r$pdc, c(1, NA, 0.5, 0.5, NA, NA, 0.5, 0.5))   # 0/0 and 7/0 are NA
  expect_type(r$age2, "integer")
  expect_equal(r$age2, c(20L, 34L, 36L, 80L, 128L, 130L, 160L, NA))
  expect_s3_class(r$big, "integer64")
  expect_equal(as.character(r$big), as.character(bit64::as.integer64(x$age) *
                                                  bit64::as.integer64(.Machine$integer.max)))
  expect_type(r$days, "integer")
  expect_equal(r$days, rep(31L, 8L))
  expect_s3_class(r$later, "Date")
  expect_equal(r$later, x$start + 7L)
  for (bad in list(
    list(op = "+", args = list(.v2v_col("label"), .v2v_lit(1))),          # text arithmetic
    list(op = "+", args = list(.v2v_col("start"), .v2v_col("stop"))),     # date + date
    list(op = "*", args = list(.v2v_col("start"), .v2v_lit(2L, "integer"))),
    list(op = "+", args = list(.v2v_col("flag"), .v2v_lit(1))),           # boolean arithmetic
    list(op = "+", args = list(.v2v_lit(NULL), .v2v_lit(NULL))))) {       # untyped
    expect_error(withr::with_options(list(nfilter.subset = 3), .v2v_mutate(x, list("y", bad))),
                 "arithmetic|no typed operand|no type", class = "dsomop_plan_rejection")
  }
})

test_that("predicates are three-valued and conditional values propagate UNKNOWN as NA", {
  x <- .v2v_frame()
  withr::with_options(list(nfilter.subset = 3), {
    r <- .v2v_mutate(x,
      list("adult", list(pred = list(var = "age", op = ">=", value = 18))),
      list("not_adult", list(pred = list(not = list(var = "age", op = ">=", value = 18)))),
      list("lab_in", list(pred = list(var = "label", op = "in", value = list("a", "b")))),
      list("grp", list(ifelse = list(test = list(var = "age", op = "<", value = 18),
                                     yes = .v2v_lit("child"), no = .v2v_lit("adult")))),
      list("cw", list(case_when = list(cases = list(
        list(when = list(var = "label", op = "==", value = "a"), then = .v2v_lit(1L, "integer")),
        list(when = list(var = "age", op = ">", value = 60), then = .v2v_lit(2L, "integer"))),
        `else` = .v2v_lit(0L, "integer")))),
      list("cw_no_else", list(case_when = list(cases = list(
        list(when = list(var = "flag", op = "==", value = TRUE), then = .v2v_lit("yes"))))))
    )
  })
  expect_equal(r$adult, c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, NA))
  expect_equal(r$not_adult, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, NA))
  expect_equal(r$lab_in, c(TRUE, NA, TRUE, TRUE, FALSE, TRUE, NA, TRUE))
  expect_equal(r$grp, c("child", "child", "adult", "adult", "adult", "adult", "adult", NA))
  # the first non-FALSE case decides: a TRUE case its value, an UNKNOWN case NA
  expect_equal(r$cw, c(1L, NA, 0L, 1L, 2L, 2L, NA, 1L))
  expect_equal(r$cw_no_else, c("yes", NA, NA, "yes", "yes", NA, "yes", NA))
})

test_that("coalesce takes one type and cut returns text labels with public breaks", {
  x <- .v2v_frame()
  withr::with_options(list(nfilter.subset = 3), {
    r <- .v2v_mutate(x,
      list("cov0", list(coalesce = list(.v2v_col("covered"), .v2v_lit(0)))),
      list("age0", list(coalesce = list(.v2v_col("age"), .v2v_lit(-1L, "integer")))),
      list("band", list(cut = list(arg = .v2v_col("age"), breaks = list(0, 18, 65, 120)))),
      list("band_r", list(cut = list(arg = .v2v_col("age"), breaks = list(0, 18, 65, 120),
                                     right = TRUE, labels = list("young", "mid", "old")))))
    expect_error(.v2v_mutate(x, list("z", list(coalesce = list(.v2v_col("label"), .v2v_lit(0))))),
                 "one type", class = "dsomop_plan_rejection")
    expect_error(.v2v_mutate(x, list("z", list(cut = list(arg = .v2v_col("label"),
                                                          breaks = list(0, 1))))),
                 "numeric argument", class = "dsomop_plan_rejection")
    expect_error(.v2v_mutate(x, list("z", list(cut = list(arg = .v2v_col("age"),
                                                          breaks = list(5, 1))))),
                 "strictly increasing", class = "dsomop_plan_rejection")
    expect_error(.v2v_mutate(x, list("z", list(cut = list(arg = .v2v_col("age"), breaks = list(0, 1, 2),
                                                          labels = list("a"))))),
                 "one per interval", class = "dsomop_plan_rejection")
  })
  expect_equal(r$cov0, c(4, 0, 10, 3, 0, 7, 2, 5))
  expect_type(r$age0, "integer")
  expect_equal(r$age0[[8L]], -1L)
  expect_equal(r$band, c("[0,18)", "[0,18)", "[18,65)", "[18,65)", "[18,65)", "[65,120)",
                         "[65,120)", NA))
  expect_equal(r$band_r, c("young", "young", "young", "mid", "mid", "mid", "old", NA))
})

test_that("assignments are sequential, may overwrite non-protected columns, and use the A-6 grammar", {
  x <- .v2v_frame()
  withr::with_options(list(nfilter.subset = 3), {
    r <- .v2v_mutate(x,
      list("Pdc", list(op = "/", args = list(.v2v_col("covered"), .v2v_col("observed")))),
      list("adherent", list(ifelse = list(test = list(var = "Pdc", op = ">=", value = 0.8),
                                          yes = .v2v_lit(1L, "integer"), no = .v2v_lit(0L, "integer")))),
      list("age", list(op = "+", args = list(.v2v_col("age"), .v2v_lit(1L, "integer")))))
    expect_error(.v2v_mutate(x, list("y", .v2v_col("nope"))), "not a column",
                 class = "dsomop_plan_rejection")
    # an expression cannot read a column assigned after it
    expect_error(.v2v_mutate(x, list("a1", .v2v_col("a2")), list("a2", .v2v_lit(1))),
                 "not a column", class = "dsomop_plan_rejection")
  })
  expect_equal(r$adherent, c(1L, NA, 0L, 0L, NA, NA, 0L, 0L))
  expect_equal(r$age, x$age + 1L)
  expect_identical(names(r)[seq_along(names(x))], names(x))   # overwritten in place
})

test_that("protected keys are never read or written and class-1 names are refused", {
  x <- .v2v_table(data.frame(person_id = 1:6, cohort_row_id = 1:6, set_id = rep(1:2, 3),
                             age = 30:35, stringsAsFactors = FALSE))
  withr::with_options(list(nfilter.subset = 3), {
    for (key in c("person_id", "cohort_row_id", "set_id")) {
      expect_error(.v2v_mutate(x, list(key, .v2v_lit(1))), "protected",
                   class = "dsomop_plan_rejection")
      expect_error(.v2v_mutate(x, list("copy", .v2v_col(key))), "protected",
                   class = "dsomop_plan_rejection")
      expect_error(.v2v_mutate(x, list("t", list(pred = list(var = key, op = "==", value = 1)))),
                   "protected", class = "dsomop_plan_rejection")
    }
    expect_error(.v2v_mutate(x, list("person_source_value", .v2v_lit("x"))), "class-1",
                 class = "dsomop_plan_rejection")
    expect_error(.v2v_mutate(x, list("patient_id", .v2v_lit(1))), "class-1|protected",
                 class = "dsomop_plan_rejection")
  })
})

test_that("a request is data: free-form code is refused before evaluation", {
  x <- .v2v_frame()
  hit <- FALSE
  testthat::local_mocked_bindings(.mutateEval = function(...) { hit <<- TRUE; stop("evaluated") },
                                  .package = "dsOMOP")
  withr::with_options(list(nfilter.subset = 3), {
    for (req in list(
      list(version = 1L, assign = list(list(name = "x", expr = list(fn = "system", code = "id")))),
      list(version = 1L, assign = list(list(name = "x", expr = list(call = "eval")))),
      list(version = 2L, assign = list(list(name = "x", expr = list(lit = 1)))),
      list(version = 1L, assign = list(list(name = "x", expr = list(lit = 1))), extra = TRUE))) {
      expect_error(omopMutateDS(x, .v2v_enc(req)), "invalid mutation request",
                   class = "dsomop_plan_rejection")
    }
    expect_error(omopMutateDS(x, "system('id')"), "JSON object", class = "dsomop_plan_rejection")
  })
  expect_false(hit)
})

test_that("mutation results pass the final-support boundary and drop DP provenance", {
  withr::with_options(list(nfilter.subset = 3), {
    msg <- "Disclosive: operation blocked - insufficient individuals to meet disclosure threshold. No further details available."
    # k - 1, k, k + 1 distinct persons; five rows of one person count once
    for (n in 2:4) {
      x <- .v2v_table(data.frame(person_id = c(seq_len(n), rep(1L, 4L)), v = 1, stringsAsFactors = FALSE))
      res <- tryCatch(.v2v_mutate(x, list("w", list(op = "*", args = list(.v2v_col("v"), .v2v_lit(2))))),
                      error = function(e) e)
      if (n < 3L) {
        expect_s3_class(res, "error")
        expect_identical(conditionMessage(res), msg)
      } else {
        expect_s3_class(res, "omop.table")
        expect_equal(res$w, rep(2, n + 4L))
      }
    }
    x <- .v2v_frame()
    attr(x, "dsomop_dp_provenance") <- list(sealed = TRUE)
    r <- .v2v_mutate(x, list("y", .v2v_lit(1)))
    expect_null(attr(r, "dsomop_dp_provenance"))
    expect_s3_class(r, "omop.table")
    expect_true("person_id" %in% attr(r, "dsomop_protected"))
    expect_identical(attr(r, "dsomop_pseudonymization"), attr(x, "dsomop_pseudonymization"))
    empty <- .v2v_table(data.frame(person_id = integer(0), v = numeric(0)))
    expect_error(.v2v_mutate(empty, list("y", .v2v_lit(1))), "insufficient individuals")
  })
})

# --- omopFilterDS(expr = ) --------------------------------------------------------------------

.v2v_filter <- function(x, pred) omopFilterDS(x, expr = .v2v_enc(pred))

test_that("omopFilterDS(expr) keeps only rows where the predicate is TRUE (three-valued)", {
  x <- .v2v_frame()
  withr::with_options(list(nfilter.subset = 3), {
    expect_equal(.v2v_filter(x, list(var = "age", op = ">=", value = 18))$age,
                 c(18L, 40L, 64L, 65L, 80L))
    # NOT over UNKNOWN stays UNKNOWN: the NA age is in neither side
    expect_equal(.v2v_filter(x, list(not = list(var = "age", op = ">=", value = 65)))$age,
                 c(10L, 17L, 18L, 40L, 64L))
    expect_equal(.v2v_filter(x, list(var = "label", op = "not_in", value = list("c")))$label,
                 c("a", "b", "a", "b", "a"))
    expect_equal(nrow(.v2v_filter(x, list(and = list(list(var = "age", op = "between",
                                                          value = list(lower = 10, upper = 64)),
                                                     list(var = "flag", op = "not_null"))))), 4L)
    expect_equal(nrow(.v2v_filter(x, list(var = "start", op = ">", value = "2020-01-31"))), 4L)
    # one person with an unknown age is a released object of support 1
    expect_error(.v2v_filter(x, list(var = "age", op = "is_null")), "insufficient individuals")
  })
})

test_that("omopFilterDS(expr) refuses invalid, untyped, protected and mixed requests", {
  x <- .v2v_frame()
  withr::with_options(list(nfilter.subset = 3), {
    expect_error(.v2v_filter(x, list(var = "age", op = "~=", value = 1)), "invalid predicate",
                 class = "dsomop_plan_rejection")
    expect_error(.v2v_filter(x, list(var = "label", op = ">", value = "a")), "text column",
                 class = "dsomop_plan_rejection")
    expect_error(.v2v_filter(x, list(var = "age", op = "==", value = "x")), "numbers",
                 class = "dsomop_plan_rejection")
    expect_error(.v2v_filter(x, list(var = "missing_col", op = "==", value = 1)), "not a column",
                 class = "dsomop_plan_rejection")
    expect_error(.v2v_filter(x, list(var = "person_id", op = "==", value = 1)),
                 "protected|invalid predicate", class = "dsomop_plan_rejection")
    expect_error(omopFilterDS(x, var = "age", op = "==", value = 18,
                              expr = .v2v_enc(list(var = "age", op = "==", value = 18))),
                 "not both")
    # the legacy form is unchanged
    expect_equal(omopFilterDS(x, "label", "==", "a")$label, c("a", "a", "a"))
  })
})

test_that("omopFilterDS(expr) results pass the final-support boundary", {
  x <- .v2v_frame()
  withr::with_options(list(nfilter.subset = 3), {
    expect_identical(
      tryCatch(.v2v_filter(x, list(var = "age", op = "<", value = 18)), error = conditionMessage),
      "Disclosive: operation blocked - insufficient individuals to meet disclosure threshold. No further details available.")
    # k persons kept from k + 1: the excluded side is never gated
    y <- .v2v_table(data.frame(person_id = 1:4, g = c(1, 1, 1, 2)))
    expect_equal(nrow(.v2v_filter(y, list(var = "g", op = "==", value = 1))), 3L)
    expect_error(.v2v_filter(y, list(var = "g", op = "==", value = 3)), "insufficient individuals")
  })
})

test_that("standalone row verbs remain assign methods in both deployment manifests", {
  verbs <- c("omopMutateDS", "omopFilterDS")
  for (file in c("DATASHIELD", "DESCRIPTION")) {
    manifest <- read.dcf(testthat::test_path("..", "..", file))
    assign_methods <- trimws(strsplit(manifest[1L, "AssignMethods"], ",")[[1L]])
    aggregate_methods <- trimws(strsplit(manifest[1L, "AggregateMethods"], ",")[[1L]])
    expect_true(all(verbs %in% assign_methods), info = file)
    expect_false(any(verbs %in% aggregate_methods), info = file)
  }
})

test_that("the capability block advertises the verbs with their own endpoints only", {
  expect_true(all(c("verb.mutate", "verb.filter_expr", "status.accessor") %in%
                    dsOMOP:::.recipeAvailableOperations()))
  expect_true("omopMutateDS" %in% getNamespaceExports("dsOMOP"))
  # the verbs do not need the plan compilers
  testthat::local_mocked_bindings(.recipeFunctionInstalled = function(name) !startsWith(name, ".compile"),
                                  .package = "dsOMOP")
  expect_setequal(dsOMOP:::.recipeAvailableOperations(),
                  c("status.accessor", "verb.mutate", "verb.filter_expr"))
  testthat::local_mocked_bindings(.recipeFunctionInstalled = function(name) name != ".omopFilterExpr",
                                  .package = "dsOMOP")
  expect_false("verb.filter_expr" %in% dsOMOP:::.recipeAvailableOperations())
})

test_that("a deployment that disables a verb operation refuses the verb", {
  x <- .v2v_frame()
  withr::local_options(list(nfilter.subset = 3,
                            dsomop.recipe_operations_disabled = c("verb.mutate", "verb.filter_expr")))
  expect_false(any(c("verb.mutate", "verb.filter_expr") %in% dsOMOP:::.recipeAvailableOperations()))
  expect_error(.v2v_mutate(x, list("y", .v2v_lit(1))), "does not offer verb.mutate",
               class = "dsomop_plan_rejection")
  expect_error(.v2v_filter(x, list(var = "age", op = ">=", value = 18)),
               "does not offer verb.filter_expr", class = "dsomop_plan_rejection")
  expect_equal(nrow(omopFilterDS(x, "label", "==", "a")), 3L)
})
