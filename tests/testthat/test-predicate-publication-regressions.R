# WS-9 re-review R1/R2: real harmonization, mutation and filter publication.
.pp_frame <- function(values, name) {
  x <- data.frame(person_id = seq_along(values))
  x[[name]] <- values
  .testPseudonymize(x)
}
.pp_mut <- function(x, expression) {
  omopMutateDS(x, list(version = 1L,
    assign = list(list(name = "answer", expr = expression))))$answer
}
.pp_pred <- function(name, op, value) list(var = name, op = op, value = value)
.pp_options <- list(nfilter.subset = 3, default.dsomop.dp.enabled = FALSE,
                    nfilter.levels.max = 40, nfilter.levels.density = 1, nfilter.tab = 3)

test_that("harmonized factor predicates use exact labels throughout publication", {
  withr::local_options(.pp_options)
  fixtures <- list(
    list(values = c(rep("case", 3), rep("control", 3), NA), queries = c("case", "absent")),
    list(values = c(rep(8532L, 3), rep(8507L, 3), NA_integer_), queries = c("8532", "08532")),
    list(values = c(rep("08532", 3), rep("8532", 3), NA), queries = c("08532", "8532")))
  for (fixture in fixtures) {
    source <- .pp_frame(fixture$values, "gender_concept_id")
    original <- serialize(source, NULL)
    labels <- as.character(fixture$values)
    x <- omopAsFactorColumnsDS(source,
      list(gender_concept_id = unique(labels[!is.na(labels)])))
    expect_true(is.factor(x$gender_concept_id))
    expect_identical(as.character(x$gender_concept_id), labels)
    expect_identical(serialize(source, NULL), original)
    harmonized <- serialize(x, NULL)
    for (query in fixture$queries) {
      equal <- labels == query
      predicates <- list(
        .pp_pred("gender_concept_id", "==", query),
        .pp_pred("gender_concept_id", "in", list(query)),
        .pp_pred("gender_concept_id", "!=", query),
        .pp_pred("gender_concept_id", "not_in", list(query)))
      for (i in seq_along(predicates)) {
        expected <- if (i <= 2L) equal else !equal
        pred <- predicates[[i]]
        expect_identical(.pp_mut(x, list(pred = pred)), expected)
        # Both conditional operators retain UNKNOWN, including at publication.
        for (expression in list(
          list(ifelse = list(test = pred, yes = list(lit = 1L, as = "integer"),
                             no = list(lit = 0L, as = "integer"))),
          list(case_when = list(cases = list(list(when = pred,
            then = list(lit = 1L, as = "integer"))),
            `else` = list(lit = 0L, as = "integer"))))) {
          expect_identical(.pp_mut(x, expression), as.integer(expected))
        }
        if (sum(expected %in% TRUE) >= 3L) {
          released <- omopFilterDS(x, expr = pred)
          expect_s3_class(released, "omop.table")
          expect_true(is.factor(released$gender_concept_id))
          expect_identical(released$person_id, x$person_id[expected %in% TRUE])
        } else {
          expect_error(omopFilterDS(x, expr = pred), "insufficient individuals",
                       class = "dsomop_plan_rejection")
        }
      }
    }
    expect_identical(serialize(x, NULL), harmonized)
  }
})

test_that("datetime scalar and membership predicates share local civil midnight", {
  withr::local_options(.pp_options)
  for (zone in c("America/New_York", "Asia/Tokyo")) {
    # Include the instant of UTC midnight: it must not equal local midnight.
    utc_midnight <- if (zone == "America/New_York") "2019-12-31 19:00:00" else
      "2020-01-01 09:00:00"
    stamp <- as.POSIXct(c(rep("2020-01-01 00:00:00", 3), rep(utc_midnight, 3),
      "2019-12-31 23:59:59", "2020-01-01 23:59:59", "2020-01-02 00:00:00", NA), tz = zone)
    equal <- c(rep(TRUE, 3), rep(FALSE, 6), NA)
    between <- c(rep(TRUE, 3), rep(zone == "Asia/Tokyo", 3), FALSE, TRUE, FALSE, NA)
    for (kind in c("POSIXct", "POSIXlt")) {
      x <- .pp_frame(if (kind == "POSIXlt") as.POSIXlt(stamp) else stamp, "stamp")
      original <- serialize(x, NULL)
      predicates <- list(
        .pp_pred("stamp", "==", "2020-01-01"),
        .pp_pred("stamp", "in", list("2020-01-01")),
        .pp_pred("stamp", "!=", "2020-01-01"),
        .pp_pred("stamp", "not_in", list("2020-01-01")),
        .pp_pred("stamp", "between", list(lower = "2020-01-01", upper = "2020-01-01")))
      for (i in seq_along(predicates)) {
        expected <- if (i <= 2L) equal else if (i <= 4L) !equal else between
        expect_identical(.pp_mut(x, list(pred = predicates[[i]])), expected)
        released <- omopFilterDS(x, expr = predicates[[i]])
        expect_s3_class(released, "omop.table")
        expect_s3_class(released$stamp, kind)
        expect_identical(released$person_id, x$person_id[expected %in% TRUE])
        expect_identical(released$stamp, x$stamp[expected %in% TRUE])
      }
      for (op in c("<", "<=", ">", ">=")) {
        expected <- do.call(op, list(stamp, as.POSIXct("2020-01-01 00:00:00", tz = zone)))
        expect_identical(.pp_mut(x, list(pred = .pp_pred("stamp", op, "2020-01-01"))), expected)
      }
      expect_identical(serialize(x, NULL), original)
    }
  }
})

test_that("inclusive datetime ranges follow calendar boundaries across DST", {
  withr::local_options(.pp_options)
  fixtures <- list(
    list(zone = "America/New_York", day = "2020-03-08", hours = 23),
    list(zone = "America/New_York", day = "2020-11-01", hours = 25),
    list(zone = "Europe/Berlin", day = "2020-03-29", hours = 23),
    list(zone = "Europe/Berlin", day = "2020-10-25", hours = 25))
  for (fixture in fixtures) {
    day <- fixture$day
    before <- as.character(as.Date(day) - 1L)
    after <- as.character(as.Date(day) + 1L)
    stamp <- as.POSIXct(c(paste(day, c("00:00:00", "12:00:00", "23:59:59")),
      paste(before, "23:59:59"), paste(after, c("00:00:00", "00:30:00")), NA), tz = fixture$zone)
    expect_equal(as.numeric(difftime(stamp[5], stamp[1], units = "hours")), fixture$hours)
    x <- .pp_frame(stamp, "stamp")
    inclusive <- .pp_pred("stamp", "between", list(lower = day, upper = day))
    exclusive <- .pp_pred("stamp", "between",
      list(lower = day, upper = after, upper_inclusive = FALSE))
    expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA)
    for (pred in list(inclusive, exclusive)) {
      expect_identical(.pp_mut(x, list(pred = pred)), expected)
      released <- omopFilterDS(x, expr = pred)
      expect_identical(released$person_id, x$person_id[1:3])
      expect_identical(format(released$stamp, "%Y-%m-%d", tz = fixture$zone), rep(day, 3))
    }
    empty <- .pp_pred("stamp", "between",
      list(lower = day, upper = day, upper_inclusive = FALSE))
    expect_identical(.pp_mut(x, list(pred = empty)), c(rep(FALSE, 6), NA))
    expect_error(omopFilterDS(x, expr = empty), "insufficient individuals",
                 class = "dsomop_plan_rejection")
  }
})

test_that("datetime predicates retain the UTC fallback and reject invalid dates", {
  withr::local_options(.pp_options)
  withr::local_envvar(TZ = "Pacific/Honolulu")
  for (timezone in list(NULL, character(), "")) {
    stamp <- as.POSIXct(c(rep("2020-01-01 00:00:00", 3), "2019-12-31 23:59:59", NA), tz = "UTC")
    attr(stamp, "tzone") <- timezone
    x <- .pp_frame(stamp, "stamp")
    for (pred in list(.pp_pred("stamp", "==", "2020-01-01"),
                     .pp_pred("stamp", "in", list("2020-01-01")),
                     .pp_pred("stamp", "between", list(lower = "2020-01-01", upper = "2020-01-01")))) {
      check_fallback <- function(value) {
        # Base R may warn about display-zone metadata on these fallback
        # inputs. Suppress only that warning; check the actual instants below.
        withCallingHandlers(force(value), warning = function(w) {
          if (identical(conditionMessage(w), "'tzone' attributes are inconsistent")) {
            invokeRestart("muffleWarning")
          }
        })
      }
      expect_identical(check_fallback(.pp_mut(x, list(pred = pred))), c(TRUE, TRUE, TRUE, FALSE, NA))
      expect_identical(check_fallback(omopFilterDS(x, expr = pred))$person_id, x$person_id[1:3])
    }
  }
  for (invalid in c("2020-02-30", "2020-1-1", "2020-01-01 00:00:00")) {
    for (pred in list(.pp_pred("stamp", "==", invalid), .pp_pred("stamp", "in", list(invalid)),
                     .pp_pred("stamp", "between", list(lower = "2020-01-01", upper = invalid)))) {
      expect_error(.pp_mut(x, list(pred = pred)), "ISO dates", class = "dsomop_plan_rejection")
      expect_error(omopFilterDS(x, expr = pred), "ISO dates", class = "dsomop_plan_rejection")
    }
  }
})
