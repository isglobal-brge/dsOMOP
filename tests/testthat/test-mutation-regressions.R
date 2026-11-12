# WS-9 review B3--B5 and A-23: exercise the real publication entry point.
.mfix_frame <- function(...) {
  dsOMOP:::.pseudonymizeIdentifiers(
    data.frame(person_id = 1:5, ...), .testPseudonymKey(),
    pseudonymization = .testPublicPseudonymization(.testPseudonymKey()))
}
.mfix_col <- function(name) list(col = name)
.mfix_lit <- function(value, type = NULL) c(list(lit = value), if (!is.null(type)) list(as = type))
.mfix_req <- function(expr) list(version = 1L, assign = list(list(name = "answer", expr = expr)))
.mfix_mut <- function(x, expr) {
  req <- as.character(jsonlite::toJSON(.mfix_req(expr), auto_unbox = TRUE, null = "null", digits = NA))
  omopMutateDS(x, req)
}
.mfix_flag <- list(var = "flag", op = "==", value = 1)
.mfix_options <- list(nfilter.subset = 3, default.dsomop.dp.enabled = FALSE)

test_that("integer branches and coalesce use one lossless prototype in every order", {
  withr::local_options(.mfix_options)
  big <- bit64::as.integer64(c("9007199254740993", "200", NA, "9223372036854775807", "-9007199254740993"))
  x <- .mfix_frame(big = big, small = 1:5, flag = c(1L, 0L, 1L, 0L, NA))
  zero <- .mfix_lit(0L, "integer")
  for (reverse in c(FALSE, TRUE)) {
    yes <- if (reverse) .mfix_col("big") else zero
    no <- if (reverse) zero else .mfix_col("big")
    z <- .mfix_mut(x, list(ifelse = list(test = .mfix_flag, yes = yes, no = no)))
    expect_s3_class(z, "omop.table")
    expect_s3_class(z$answer, "integer64")
    expect_identical(as.character(z$answer), if (reverse)
      c("9007199254740993", "0", NA, "0", NA) else c("0", "200", "0", "9223372036854775807", NA))
  }
  for (null in list(.mfix_lit(NULL), .mfix_lit(NULL, "integer"))) {
    for (items in list(list(null, .mfix_col("big")), list(.mfix_col("big"), null))) {
      z <- .mfix_mut(x, list(coalesce = items))$answer
      expect_s3_class(z, "integer64")
      expect_identical(as.character(z), as.character(big))
    }
  }
  z <- .mfix_mut(x, list(coalesce = list(.mfix_col("big"), .mfix_col("small"))))$answer
  expect_identical(as.character(z), c("9007199254740993", "200", "3", "9223372036854775807", "-9007199254740993"))
  # The unselected later operand still determines the common physical type.
  z <- .mfix_mut(x, list(coalesce = list(.mfix_col("small"), .mfix_col("big"))))$answer
  expect_s3_class(z, "integer64")
  expect_identical(as.character(z), as.character(1:5))
  for (fallback in list(zero, .mfix_lit(NULL, "integer"))) {
    z <- .mfix_mut(x, list(case_when = list(cases = list(
      list(when = .mfix_flag, then = .mfix_col("big"))), `else` = fallback)))$answer
    expect_s3_class(z, "integer64")
    expect_identical(as.character(z), c("9007199254740993", if (is.null(fallback$lit)) NA else "0",
                                        NA, if (is.null(fallback$lit)) NA else "0", NA))
  }
  z <- .mfix_mut(x, list(case_when = list(cases = list(
    list(when = .mfix_flag, then = zero)), `else` = .mfix_col("big"))))$answer
  expect_identical(as.character(z), c("0", "200", "0", "9223372036854775807", NA))
  z <- .mfix_mut(x, list(case_when = list(cases = list(
    list(when = .mfix_flag, then = .mfix_col("big"))))))$answer
  expect_identical(as.character(z), c("9007199254740993", NA, NA, NA, NA))
  # Mixed semantic integer/numeric branches still unify to numeric.
  z <- .mfix_mut(x, list(ifelse = list(test = .mfix_flag,
    yes = .mfix_col("small"), no = .mfix_lit(0.5))))$answer
  expect_type(z, "double")
  expect_equal(z, c(1, 0.5, 3, 0.5, NA))
})

test_that("integer arithmetic is exact before range checks and promotes past 32 bits", {
  withr::local_options(.mfix_options)
  x <- .mfix_frame(a = bit64::as.integer64(c("9007199254740993", "9223372036854775807",
                  "-9223372036854775807", "2147483647", NA)),
                  b = bit64::as.integer64(c("9007199254740992", "9223372036854775806",
                  "-9223372036854775806", "-1", "1")), small = rep(2147483647L, 5))
  op <- function(kind, a, b) .mfix_mut(x, list(op = kind, args = list(a, b)))$answer
  expect_identical(as.character(op("-", .mfix_col("a"), .mfix_col("b"))),
                   c("1", "1", "-1", "2147483648", NA))
  expect_identical(as.character(op("+", .mfix_col("small"), .mfix_lit(1L, "integer"))),
                   rep("2147483648", 5))
  expect_identical(as.character(op("*", .mfix_col("small"), .mfix_col("small"))),
                   rep("4611686014132420609", 5))
  expect_identical(as.character(op("+", .mfix_col("a"), .mfix_lit(1L, "integer"))),
                   c("9007199254740994", NA, "-9223372036854775806", "2147483648", NA))
  expect_identical(as.character(op("-", .mfix_col("a"), .mfix_lit(1L, "integer"))),
                   c("9007199254740992", "9223372036854775806", NA, "2147483646", NA))
  expect_identical(as.character(op("*", .mfix_col("a"), .mfix_lit(2L, "integer"))),
                   c("18014398509481986", NA, NA, "4294967294", NA))
  expect_identical(op("-", .mfix_col("b"), .mfix_col("b")), c(0L, 0L, 0L, 0L, 0L))
  for (null in list(.mfix_lit(NULL), .mfix_lit(NULL, "integer"))) {
    expect_true(all(is.na(op("+", .mfix_col("a"), null))))
    expect_true(all(is.na(op("+", null, .mfix_col("a")))))
  }
  expect_true(all(is.na(op("/", .mfix_col("a"), .mfix_lit(0L, "integer")))))
  expect_true(all(is.na(op("*", .mfix_lit(1e308), .mfix_lit(1e308)))))
  expect_identical(as.character(.mfix_mut(x, .mfix_lit(2147483648, "integer"))$answer),
                   rep("2147483648", 5))
  # A later expression consumes the promoted type without reinterpreting it.
  req <- list(version = 1L, assign = list(
    list(name = "wide", expr = list(op = "+", args = list(.mfix_col("small"), .mfix_lit(1L, "integer")))),
    list(name = "back", expr = list(op = "-", args = list(.mfix_col("wide"), .mfix_col("small"))))))
  expect_identical(omopMutateDS(x, req)$back, rep(1L, 5))
})

test_that("datetime branch prototypes preserve class, instants and timezone", {
  withr::local_options(.mfix_options)
  stamp <- as.POSIXct("2020-01-01 12:00:00", tz = "America/New_York") + 1:5
  for (kind in c("POSIXct", "POSIXlt")) {
    x <- .mfix_frame(flag = c(1L, 0L, 1L, 0L, NA))
    x$stamp <- if (kind == "POSIXlt") as.POSIXlt(stamp) else stamp
    null <- .mfix_lit(NULL)
    for (items in list(list(null, .mfix_col("stamp")), list(.mfix_col("stamp"), null))) {
      z <- .mfix_mut(x, list(coalesce = items))$answer
      expect_s3_class(z, kind)
      expect_equal(z, x$stamp)
      expect_identical(attr(z, "tzone"), attr(x$stamp, "tzone"))
    }
    for (reverse in c(FALSE, TRUE)) {
      yes <- if (reverse) .mfix_col("stamp") else null
      no <- if (reverse) null else .mfix_col("stamp")
      z <- .mfix_mut(x, list(ifelse = list(test = .mfix_flag, yes = yes, no = no)))$answer
      expected <- x$stamp
      expected[c(if (reverse) c(2, 4) else c(1, 3), 5)] <- NA
      expect_s3_class(z, kind)
      expect_equal(z, expected)
      expect_identical(attr(z, "tzone"), attr(x$stamp, "tzone"))
    }
    z <- .mfix_mut(x, list(case_when = list(cases = list(
      list(when = .mfix_flag, then = .mfix_col("stamp"))))))$answer
    expected <- x$stamp
    expected[c(2, 4, 5)] <- NA
    expect_s3_class(z, kind)
    expect_equal(z, expected)
    expect_identical(attr(z, "tzone"), attr(x$stamp, "tzone"))
  }
})

test_that("date arithmetic and null branches retain date or exact day types", {
  withr::local_options(.mfix_options)
  x <- .mfix_frame(day = as.Date("2020-01-01") + 1:5,
                  offset = bit64::as.integer64(c("1", "2147483648", "3", NA, "-1")),
                  flag = c(1L, 0L, 1L, 0L, NA))
  null <- .mfix_lit(NULL)
  for (args in list(list(.mfix_col("day"), .mfix_col("offset")),
                    list(.mfix_col("offset"), .mfix_col("day")))) {
    z <- .mfix_mut(x, list(op = "+", args = args))$answer
    expect_s3_class(z, "Date")
    expect_equal(z, x$day + c(1, 2147483648, 3, NA, -1))
  }
  for (kind in c("+", "-")) {
    z <- .mfix_mut(x, list(op = kind, args = list(.mfix_col("day"), null)))$answer
    expect_s3_class(z, "Date")
    expect_true(all(is.na(z)))
  }
  z <- .mfix_mut(x, list(op = "-", args = list(null, .mfix_col("day"))))$answer
  expect_type(z, "integer")
  expect_true(all(is.na(z)))
  for (items in list(list(null, .mfix_col("day")), list(.mfix_col("day"), null))) {
    expect_equal(.mfix_mut(x, list(coalesce = items))$answer, x$day)
  }
})

test_that("publication rejects results that disagree with the static type", {
  withr::local_options(.mfix_options)
  x <- .mfix_frame(v = 1:5)
  published <- FALSE
  testthat::local_mocked_bindings(
    .mutateEval = function(...) rep(0, 5),
    .publishStandalone = function(...) { published <<- TRUE; stop("published") }, .package = "dsOMOP")
  expect_error(.mfix_mut(x, .mfix_col("v")), "inferred type 'integer'", class = "dsomop_plan_rejection")
  expect_false(published)
  testthat::local_mocked_bindings(.mutateEval = function(...) 1:2, .package = "dsOMOP")
  expect_error(.mfix_mut(x, .mfix_col("v")), "inferred type", class = "dsomop_plan_rejection")
  expect_false(published)
})

test_that("root and nested JSON member permutations preserve mutation semantics", {
  withr::local_options(.mfix_options)
  x <- .mfix_frame(v = 1:5, flag = c(1L, 0L, 1L, 0L, NA))
  reverse <- function(x) {
    if (!is.list(x)) return(x)
    x <- lapply(x, reverse)
    if (!is.null(names(x))) x <- x[rev(seq_along(x))]
    x
  }
  expressions <- list(.mfix_lit(7L, "integer"),
    list(op = "+", args = list(.mfix_col("v"), .mfix_lit(1L, "integer"))),
    list(ifelse = list(test = .mfix_flag, yes = .mfix_lit(7L, "integer"),
      no = list(op = "+", args = list(.mfix_col("v"), .mfix_lit(1L, "integer"))))),
    list(case_when = list(cases = list(list(when = .mfix_flag, then = .mfix_lit(7L, "integer"))),
                         `else` = .mfix_col("v"))),
    list(coalesce = list(.mfix_lit(NULL, "integer"), .mfix_col("v"))),
    list(cut = list(arg = .mfix_col("v"), breaks = list(0, 2, 4, 6), right = FALSE)))
  for (expr in expressions) {
    req <- reverse(.mfix_req(expr))
    expect_true(dsOMOP:::.jsonSchemaValidate(dsOMOP:::.irSchema("mutate-v1"), req)$valid)
    expect_identical(omopMutateDS(x, req)$answer, .mfix_mut(x, expr)$answer)
  }
  evaluated <- FALSE
  testthat::local_mocked_bindings(.mutateEval = function(...) { evaluated <<- TRUE; stop("evaluated") },
                                  .package = "dsOMOP")
  for (expr in list(list(col = "v", lit = 1), list(as = "integer", lit = 1, extra = TRUE),
                    list(args = list(.mfix_col("v")), op = "system"))) {
    expect_error(.mfix_mut(x, expr), "invalid mutation request", class = "dsomop_plan_rejection")
  }
  expect_false(evaluated)
})

test_that("repeated public cut labels combine adjacent intervals", {
  withr::local_options(.mfix_options)
  x <- .mfix_frame(v = 1:5)
  expr <- list(cut = list(arg = .mfix_col("v"), breaks = list(0, 2, 4, 6),
                          labels = list("lower", "lower", "upper")))
  expect_true(dsOMOP:::.jsonSchemaValidate(dsOMOP:::.irSchema("mutate-v1"), .mfix_req(expr))$valid)
  expected <- as.character(cut(1:5, c(0, 2, 4, 6), labels = c("lower", "lower", "upper"), right = FALSE))
  expect_identical(.mfix_mut(x, expr)$answer, expected)
  expect_identical(expected, c("lower", "lower", "lower", "upper", "upper"))
})

test_that("class-1 sources are refused statically even with the projection override", {
  withr::local_options(.mfix_options)
  x <- .mfix_frame(value_as_string = rep("secret", 5), flag = c(1L, 0L, 1L, 0L, NA))
  pred <- list(var = "value_as_string", op = "==", value = "secret")
  source <- .mfix_col("value_as_string")
  evaluated <- FALSE
  testthat::local_mocked_bindings(
    .mutateEval = function(...) { evaluated <<- TRUE; stop("evaluated") },
    .verbPredicateMask = function(...) { evaluated <<- TRUE; stop("evaluated") }, .package = "dsOMOP")
  for (override in c(FALSE, TRUE)) {
    withr::local_options(list(dsomop.allow_sensitive_columns = override))
    for (expr in list(source, list(pred = pred),
      list(ifelse = list(test = .mfix_flag, yes = source, no = .mfix_lit("public"))),
      list(ifelse = list(test = pred, yes = .mfix_lit("yes"), no = .mfix_lit("no"))),
      list(case_when = list(cases = list(list(when = pred, then = .mfix_lit("yes"))))),
      list(case_when = list(cases = list(list(when = .mfix_flag, then = source)))),
      list(coalesce = list(.mfix_lit(NULL), source)))) {
      expect_error(.mfix_mut(x, expr), "class-1", class = "dsomop_plan_rejection")
    }
    expect_error(omopFilterDS(x, expr = pred), "class-1", class = "dsomop_plan_rejection")
  }
  expect_false(evaluated)
})

test_that("class-2 sources remain available through final publication", {
  withr::local_options(.mfix_options)
  x <- .mfix_frame(birth_datetime = as.POSIXct("1980-01-01", tz = "UTC") + 1:5,
                  age = 30:34)
  expect_equal(.mfix_mut(x, .mfix_col("birth_datetime"))$answer, x$birth_datetime)
  expect_equal(.mfix_mut(x, .mfix_col("age"))$answer, x$age)
  expect_identical(nrow(omopFilterDS(x, expr = list(var = "age", op = ">=", value = 30))), 5L)
})

test_that("signed integer arithmetic agrees with the independent exact limb oracle", {
  # Exercise both overflow signs, cancellation and the multiplication boundary
  # without relying on floating-point values to construct expected integers.
  values <- c("0", "1", "-1", "2147483647", "2147483648", "3037000499", "3037000500",
              "9007199254740993", "9223372036854775806", "9223372036854775807",
              "-2147483648", "-3037000499", "-3037000500", "-9007199254740993",
              "-9223372036854775806", "-9223372036854775807", NA)
  pairs <- expand.grid(a = values, b = values, stringsAsFactors = FALSE)
  limit <- dsOMOP:::.dsomopBigInteger("9223372036854775807")
  for (op in c("+", "-", "*")) {
    oracle <- switch(op, "+" = dsOMOP:::.dsomopBigAdd, "-" = dsOMOP:::.dsomopBigSubtract,
                     "*" = dsOMOP:::.dsomopBigMultiply)
    expected <- vapply(seq_len(nrow(pairs)), function(i) {
      if (is.na(pairs$a[[i]]) || is.na(pairs$b[[i]])) return(NA_character_)
      value <- oracle(pairs$a[[i]], pairs$b[[i]])
      magnitude <- dsOMOP:::.dsomopBigNew(1L, value$limbs)
      if (dsOMOP:::.dsomopBigCompare(magnitude, limit) > 0L) return(NA_character_)
      dsOMOP:::.dsomopBigDecimal(value)
    }, "")
    actual <- dsOMOP:::.mutateIntegerOp(op, bit64::as.integer64(pairs$a), bit64::as.integer64(pairs$b))
    expect_identical(as.character(actual), expected, info = paste("exact", op))
  }
})

test_that("integer64 cut preserves exact ordering at public interval boundaries", {
  withr::local_options(.mfix_options)
  x <- .mfix_frame(big = bit64::as.integer64(c("9007199254740991", "9007199254740992",
                                             "9007199254740993", "9007199254740994", "9007199254740995")))
  for (right in c(FALSE, TRUE)) {
    # Literal JSON keeps these distinct public doubles separate even on
    # jsonlite versions whose digits=NA encoder uses only 15 significant digits.
    request <- paste0('{"version":1,"assign":[{"name":"answer","expr":{"cut":',
      '{"arg":{"col":"big"},"breaks":[9007199254740990,9007199254740992,',
      '9007199254740994,9007199254740996],"labels":["a","b","c"],"right":',
      if (right) "true" else "false", '}}}]}')
    z <- omopMutateDS(x, request)$answer
    expect_identical(z, if (right) c("a", "a", "b", "b", "c") else c("a", "b", "b", "c", "c"))
  }
  for (right in c(FALSE, TRUE)) {
    values <- bit64::as.integer64(c(-2, -1, 0, 1, 2, NA))
    breaks <- c(-1e20, -1.5, -0.5, 0.5, 1.5, 1e20)
    expect_identical(dsOMOP:::.mutateCut(values, breaks, NULL, right),
                     as.character(cut(c(-2, -1, 0, 1, 2, NA), breaks, right = right)))
  }
})

test_that("nested results are type-checked before a parent can coerce them", {
  withr::local_options(.mfix_options)
  x <- .mfix_frame(v = 1:5)
  evaluate_node <- dsOMOP:::.mutateEvalNode
  published <- FALSE
  testthat::local_mocked_bindings(
    .mutateEvalNode = function(expr, df, types) {
      if (identical(dsOMOP:::.mutateVariant(expr), "col")) return(as.numeric(df[[expr$col]]))
      evaluate_node(expr, df, types)
    },
    .publishStandalone = function(...) { published <<- TRUE; stop("published") }, .package = "dsOMOP")
  expect_error(.mfix_mut(x, list(coalesce = list(.mfix_col("v"), .mfix_lit(NULL, "integer")))),
               "inferred type 'integer'", class = "dsomop_plan_rejection")
  expect_false(published)
})
