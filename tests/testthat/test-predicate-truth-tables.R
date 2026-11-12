# WS-4: three-valued predicate truth tables, SQL vs the R mask (design-2.10.3
# section 6.6). The same rows and the same predicates are evaluated by the
# language-2.0 SQL compiler, by the v1 SQL compiler (.compileFilter) and by
# the NA-preserving R mirror (.evalFilterMask), on every available backend;
# the three must agree row by row, UNKNOWN included.

.ws4_tt_backends <- .ws4_backends()
withr::defer(.ws4_cleanup(.ws4_tt_backends), testthat::teardown_env())

.ws4_tt_rows <- data.frame(
  id = 1:9,
  x = c(1, 5, 10, NA, 5, NA, 7, 3, NA),
  y = c(2, NA, 10, 1, 5, NA, NA, 8, 0),
  c = c(100L, 200L, NA, 300L, 100L, NA, 200L, 999L, NA),
  ev_date = c("2020-01-01", "2020-06-15", NA, "2019-12-31", "2021-01-01", NA,
              "2020-01-01", "2020-12-31", "2020-06-30"),
  s = c("a", "b", NA, "a", "c", NA, "b", "a", NA),
  stringsAsFactors = FALSE)

.ws4_tt_catalogue <- .predicateCatalogue(list(
  x = list(type = "numeric"), y = list(type = "numeric"),
  c = list(type = "integer", concept = TRUE), ev_date = list(type = "date"),
  s = list(type = "text")), what = "the truth-table rows")

.ws4_tt_load <- function(h) {
  family <- .sqlFamily(h)
  df <- .ws4_tt_rows
  if (!identical(family, "sqlite")) df$ev_date <- as.Date(df$ev_date)
  if (DBI::dbExistsTable(h$conn, "ws4_tt")) DBI::dbRemoveTable(h$conn, "ws4_tt")
  DBI::dbWriteTable(h$conn, "ws4_tt", df, temporary = TRUE)
  invisible(TRUE)
}

# v2 predicates (a, b) and their v1 spellings where the v1 grammar has one.
.ws4_tt_cases <- function() {
  L <- function(var, op, value = NULL) {
    out <- list(var = var, op = op)
    if (!is.null(value)) out$value <- value
    out
  }
  x4 <- L("x", ">", 4); y4 <- L("y", ">", 4)
  list(
    gt = x4, ne = L("x", "!=", 5), eq = L("x", "==", 5), le = L("x", "<=", 5),
    in_c = L("c", "in", list(100, 300)), not_in_c = L("c", "not_in", list(100, 300)),
    not_in_via_not = list(not = L("c", "in", list(100))),
    not_unknown = list(not = x4),
    and_xy = list(and = list(x4, y4)), or_xy = list(or = list(x4, y4)),
    not_and = list(not = list(and = list(x4, y4))),
    demorgan_or = list(or = list(list(not = x4), list(not = y4))),
    not_or = list(not = list(or = list(x4, y4))),
    demorgan_and = list(and = list(list(not = x4), list(not = y4))),
    between_incl = L("x", "between", list(lower = 3, upper = 7, upper_inclusive = TRUE)),
    between_excl = L("x", "between", list(lower = 3, upper = 7, upper_inclusive = FALSE)),
    date_ge = L("ev_date", ">=", "2020-06-15"),
    date_between = L("ev_date", "between", list(lower = "2020-01-01", upper = "2020-12-31")),
    is_null = L("x", "is_null"), not_null = L("x", "not_null"),
    not_is_null = list(not = L("x", "is_null")),
    text_eq = L("s", "==", "a"), text_in = L("s", "in", list("a", "c")),
    text_not_in = L("s", "not_in", list("a")),
    guarded = list(and = list(L("x", "not_null"), list(not = L("x", "==", 5)))),
    double_not = list(not = list(not = L("c", "in", list(200)))),
    null_or = list(or = list(L("c", "is_null"), L("c", "==", 999))))
}

# The v1 spelling of a v2 predicate (between bounds as a two-value list).
.ws4_tt_v1 <- function(p) {
  if (!is.null(p$and)) return(list(and = lapply(p$and, .ws4_tt_v1)))
  if (!is.null(p$or)) return(list(or = lapply(p$or, .ws4_tt_v1)))
  if (!is.null(p$not)) return(list(not = .ws4_tt_v1(p$not)))
  if (identical(p$op, "between")) {
    if (isFALSE(p$value$upper_inclusive)) return(NULL)
    p$value <- list(p$value$lower, p$value$upper)
  }
  p
}

.ws4_tt_sql_truth <- function(h, pred_sql) {
  sql <- paste0("SELECT t.id, CASE WHEN (", pred_sql, ") THEN 1 WHEN NOT (", pred_sql,
                ") THEN 0 ELSE NULL END AS tv FROM ws4_tt t ORDER BY t.id")
  as.integer(.executeQuery(h, sql)$tv)
}

# The hand-written three-valued table (independent of both implementations).
.ws4_tt_expected <- list(
  gt = c(0L, 1L, 1L, NA, 1L, NA, 1L, 0L, NA),
  in_c = c(1L, 0L, NA, 1L, 1L, NA, 0L, 0L, NA),
  not_in_c = c(0L, 1L, NA, 0L, 0L, NA, 1L, 1L, NA),
  not_unknown = c(1L, 0L, 0L, NA, 0L, NA, 0L, 1L, NA),
  and_xy = c(0L, NA, 1L, 0L, 1L, NA, NA, 0L, 0L),
  or_xy = c(0L, 1L, 1L, NA, 1L, NA, 1L, 1L, NA),
  between_excl = c(0L, 1L, 0L, NA, 1L, NA, 0L, 1L, NA),
  is_null = c(0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 1L),
  null_or = c(0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 1L))

test_that("SQL and the R mask agree on every three-valued truth table, on every backend", {
  cases <- .ws4_tt_cases()
  for (b in names(.ws4_tt_backends)) {
    h <- .ws4_tt_backends[[b]]$handle
    .ws4_tt_load(h)
    df <- .executeQuery(h, "SELECT * FROM ws4_tt ORDER BY id")
    for (nm in names(cases)) {
      p <- cases[[nm]]
      v2_sql <- .ws4_tt_sql_truth(h, .predicateSql(h, p, .ws4_tt_catalogue, "t"))
      v2_r <- as.integer(.predicateMask(p, df, .ws4_tt_catalogue))
      expect_identical(v2_sql, v2_r, info = paste(b, nm, "v2 SQL vs R"))
      if (!is.null(.ws4_tt_expected[[nm]])) {
        expect_identical(v2_sql, .ws4_tt_expected[[nm]], info = paste(b, nm, "hand table"))
      }
      p1 <- .ws4_tt_v1(p)
      if (!is.null(p1)) {
        .assertCustomFilterSafe(p1, c("x", "y", "c", "ev_date", "s"))
        v1_sql <- .ws4_tt_sql_truth(h, .compileFilter(h, p1, "t", c("x", "y", "c", "ev_date", "s")))
        v1_r <- as.integer(.evalFilterMask(p1, df))
        expect_identical(v1_sql, v1_r, info = paste(b, nm, "v1 SQL vs R"))
        expect_identical(v1_sql, v2_sql, info = paste(b, nm, "v1 vs v2"))
      }
    }
    # De Morgan holds over UNKNOWN as well
    tv <- function(nm) .ws4_tt_sql_truth(h, .predicateSql(h, cases[[nm]], .ws4_tt_catalogue, "t"))
    expect_identical(tv("not_and"), tv("demorgan_or"), info = b)
    expect_identical(tv("not_or"), tv("demorgan_and"), info = b)
    expect_identical(tv("double_not"), as.integer(df$c == 200L), info = b)
  }
})

test_that("only TRUE rows survive; is_null / not_null are the only NULL-to-boolean leaves", {
  df <- .ws4_tt_rows
  keep <- function(p) df$id[.predicateMask(p, df, .ws4_tt_catalogue) %in% TRUE]
  expect_identical(keep(list(var = "c", op = "not_in", value = list(100, 300))), c(2L, 7L, 8L))
  expect_identical(keep(list(not = list(var = "x", op = ">", value = 4))), c(1L, 8L))
  expect_identical(keep(list(var = "x", op = "is_null")), c(4L, 6L, 9L))
  expect_identical(.evalFilterMask(list(var = "c", op = "in", value = list(100)), df),
                   c(TRUE, FALSE, NA, FALSE, TRUE, NA, FALSE, FALSE, NA))
})

test_that("a missing column is refused by the R mask and by both validators, never failing closed", {
  df <- .ws4_tt_rows
  expect_error(.evalFilterMask(list(var = "nope", op = "==", value = 1), df),
               "not a column of the evaluated rows")
  expect_error(.evalFilterMask(list(var = "x", op = "like", value = 1), df), "Unknown filter operator")
  expect_error(.assertCustomFilterSafe(list(var = "nope", op = "==", value = 1), c("x", "y")),
               "not permitted")
  .ws4_expect_rejection(.predicateSql("sqlite", list(var = "nope", op = "==", value = 1),
                                      .ws4_tt_catalogue, "t"),
                        pattern = "not a predicate column")
  .ws4_expect_rejection(.predicateSql("sqlite", list(var = "s", op = ">", value = "a"),
                                      .ws4_tt_catalogue, "t"),
                        pattern = "text column")
  .ws4_expect_rejection(.predicateSql("sqlite", list(var = "x", op = "between",
                                                     value = list(lower = 5, upper = 1)),
                                      .ws4_tt_catalogue, "t"),
                        pattern = "ordered")
  # text literals never carry quotes, backslashes or control characters, so
  # one quoting is safe in every family (MySQL, BigQuery, Spark read escapes)
  for (bad in c("a'b", "a\\", "a\nb")) {
    for (family in c("mysql", "bigquery", "postgresql")) {
      .ws4_expect_rejection(.predicateSql(family, list(var = "s", op = "==", value = bad),
                                          .ws4_tt_catalogue, "t"),
                            pattern = "without quotes, backslashes")
    }
  }
  expect_identical(.predicateSql("mysql", list(var = "s", op = "in", value = list("a", "b c")),
                                 .ws4_tt_catalogue, "t"), "(t.s IN ('a', 'b c'))")
})

test_that("any / all over a window follow the Kleene table; an empty window is 0 / NA", {
  expect_identical(.kleeneAny(c(TRUE, NA)), 1L)
  expect_identical(.kleeneAny(c(FALSE, NA)), NA_integer_)
  expect_identical(.kleeneAny(c(FALSE, FALSE)), 0L)
  expect_identical(.kleeneAny(logical(0)), 0L)
  expect_identical(.kleeneAll(c(TRUE, NA)), NA_integer_)
  expect_identical(.kleeneAll(c(FALSE, NA)), 0L)
  expect_identical(.kleeneAll(c(TRUE, TRUE)), 1L)
  expect_identical(.kleeneAll(logical(0)), NA_integer_)
  # rows are not pre-filtered by the predicate: the mask of every window row
  df <- .ws4_tt_rows
  m <- .predicateMask(list(var = "x", op = ">", value = 4), df, .ws4_tt_catalogue)
  expect_identical(.kleeneAny(m[df$id %in% c(1, 4)]), NA_integer_)
  expect_identical(.kleeneAll(m[df$id %in% c(2, 3, 5)]), 1L)
})

test_that("NOT is part of both v1 grammars, with native three-valued logic", {
  h <- .ws4_tt_backends$sqlite$handle
  # row grammar: NOT over a NULL unit is UNKNOWN, so the row is not kept
  sql <- .compileSelect(h, "measurement", columns = c("person_id", "unit_concept_id"),
                        filters = list(not = list(var = "unit_concept_id", op = "in", value = list(8554))))
  expect_match(sql, "(NOT (t.unit_concept_id IN (8554)))", fixed = TRUE)
  # a NOT over a bare conjunction (a date BETWEEN leaf) keeps its parentheses
  between_not <- .compileFilter(h, list(not = list(var = "measurement_date", op = "between",
                                                   value = list("2020-01-01", "2020-12-31"))), "t")
  expect_match(between_not, "^\\(NOT \\(t\\.measurement_date >= .* AND t\\.measurement_date < .*\\)\\)$")
  rows <- .executeQuery(h, sql)
  expect_false(anyNA(rows$unit_concept_id))
  expect_false(any(rows$unit_concept_id == 8554L))
  expect_error(.assertCustomFilterSafe(list(not = list()), "x"), "NOT group must contain")
  # population grammar: NOT of a leaf over a NULL year_of_birth stays UNKNOWN
  sex_not_f <- list(not = list(type = "sex", params = list(value = "F")))
  expect_silent(.validateCohortFilterTree(sex_not_f))
  where <- .compileCohortFilterWhere(h, sex_not_f, .buildBlueprint(h), "gender_concept_id")
  expect_match(where, "^\\(NOT ")
  males <- .buildCohortFromFilters(h, sex_not_f)
  genders <- .executeQuery(h, "SELECT person_id, gender_concept_id FROM person")
  expect_setequal(males, genders$person_id[genders$gender_concept_id != 8532L])
  age <- list(type = "age_range", params = list(min = 0, max = 200, reference_date = "2020-01-01"))
  in_age <- .buildCohortFromFilters(h, age)
  not_age <- .buildCohortFromFilters(h, list(not = age))
  expect_false(1009L %in% in_age)
  expect_false(1009L %in% not_age)
  expect_true(1010L %in% not_age)
  expect_error(.validateCohortFilterTree(list(not = list())), "NOT group must contain one filter")
  # the closed v1 envelope accepts NOT in both grammars and lowering keeps it
  plan <- list(
    cohort = list(filter_tree = sex_not_f),
    outputs = list(m = list(type = "event_level", table = "measurement",
                            filters = list(custom = list(not = list(var = "unit_concept_id", op = "in",
                                                                    value = list(8554)))))))
  expect_silent(.planV1Envelope(.irJsonForm(plan)))
  ir <- .irNormalize(.irJsonForm(plan))
  expect_identical(names(ir$populations$base$where), "not")
  where_ev <- ir$events[[1L]]$where
  expect_identical(names(where_ev), "not")
  expect_identical(where_ev$not$op, "in")
  bad <- plan
  bad$cohort$filter_tree <- list(not = list(type = "sex", params = list(value = "F")), and = list())
  # the closed envelope runs the executor's own filter-tree validator (WS-2 review B1)
  expect_error(.planV1Envelope(.irJsonForm(bad)), "cannot mix AND/OR/NOT groups")
})
