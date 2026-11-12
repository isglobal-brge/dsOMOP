# WS-2 acceptance, review B10: result fidelity of the safe-class v1 corpus.
#
# fixtures/ir-v1-results.json pins the BASELINE (dsOMOP 2.10.2) canonical results
# of the safe-class v1 plans that the 2.10.2 server suite executed directly with
# .planExecute() on an unmodified default create_test_handle() fixture, in memory
# mode, identically in two independent baseline runs (captured with
# tools/ws2/capture_results.R, selected by `tools/ws2/compare_results.py pin`;
# selection rules and provenance are in the fixture). Every pin is re-executed here
# with the CURRENT code on a fresh create_test_handle() under exactly the server
# options (nfilter.* / dsomop.* / dsstaging.* / datashield.* and their default.*
# forms; others unset) and DSOMOP_* variables of the baseline call. The result must
# be identical to the baseline: output names and order, structure, column names,
# column classes and storage types, row count, row order and values (doubles to 15
# significant digits, dates ISO, factors as levels + labels, NA explicit), the
# composite/split inventories and every attribute. A refusal fails the pin. The
# test compares results only; it never inspects the IR.
#
# The only exceptions are the reviewed corrections the endorsed design makes on
# purpose (.B10_CORRECTIONS, not v1 lowering changes): the corrected column is
# compared with the design's rule, recomputed from the fixture, every baseline
# value the correction keeps must still be equal, and every other value of the
# pin is compared as above. CR-19 asks WS-0 to record them in
# v1-corpus-classes.csv.

# >>> b10-canonical: verbatim copy of the block in tools/ws2/capture_results.R
# (compare_results.py checks the copies agree)
.b10_num <- function(v, digits) {
  v <- as.vector(unclass(v))
  s <- sprintf(paste0("%.", digits, "g"), v)
  s[is.na(v)] <- NA_character_
  s[is.nan(v)] <- "NaN"
  s
}
.b10_attrs <- function(x, drop, digits) {
  a <- attributes(x)
  a <- a[setdiff(names(a), drop)]
  if (!length(a)) return(NULL)
  a <- a[order(names(a))]
  lapply(a, .b10_canon, digits = digits)
}
.b10_canon <- function(x, digits = 15L) {
  if (is.null(x)) return(list(kind = "null"))
  if (is.environment(x)) return(list(kind = "environment", class = I(class(x))))
  if (is.function(x)) return(list(kind = "function"))
  if (is.data.frame(x)) {
    rn <- if (.row_names_info(x, type = 1L) < 0L) NULL else
      I(as.character(attr(x, "row.names")))
    return(list(kind = "data.frame", class = I(class(x)),
                names = I(as.character(names(x))), nrow = nrow(x),
                row_names = rn,
                columns = lapply(seq_along(x), function(j) .b10_canon(x[[j]], digits)),
                attrs = .b10_attrs(x, c("names", "row.names", "class"), digits)))
  }
  if (is.list(x)) {
    return(list(kind = "list", class = I(class(x)),
                names = if (is.null(names(x))) NULL else I(as.character(names(x))),
                items = lapply(seq_along(x), function(i) .b10_canon(x[[i]], digits)),
                attrs = .b10_attrs(x, c("names", "class"), digits)))
  }
  if (is.atomic(x)) {
    lev <- NULL
    if (is.factor(x)) {
      lev <- I(as.character(levels(x)))
      vals <- as.character(x)
    } else if (inherits(x, "Date")) {
      vals <- format(x, "%Y-%m-%d")
    } else if (inherits(x, "POSIXt")) {
      vals <- format(as.POSIXct(x), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
    } else if (inherits(x, "integer64")) {
      vals <- if (requireNamespace("bit64", quietly = TRUE)) {
        bit64::as.character.integer64(x)
      } else paste0("integer64-bits:", .b10_num(unclass(x), 17L))
    } else if (is.double(x)) {
      vals <- .b10_num(x, digits)
    } else if (is.complex(x)) {
      vals <- paste0(.b10_num(Re(x), digits), "|", .b10_num(Im(x), digits))
      vals[is.na(x)] <- NA_character_
    } else if (is.raw(x)) {
      vals <- as.character(x)
    } else if (is.character(x)) {
      vals <- enc2utf8(as.vector(x))
    } else {
      vals <- as.vector(unclass(x))
    }
    vals <- as.vector(vals)
    attributes(vals) <- NULL
    return(list(kind = "atomic", class = I(class(x)), type = typeof(x),
                length = length(x), levels = lev, values = I(vals),
                names = if (is.null(names(x))) NULL else I(as.character(names(x))),
                attrs = .b10_attrs(x, c("names", "class", "levels"), digits)))
  }
  list(kind = "other", class = I(class(x)), type = typeof(x))
}
# <<< b10-canonical

.b10_or <- function(a, b) if (is.null(a)) b else a

# Rebuild an R object from its canonical form (exact for the 17-digit plan forms).
.b10_decode <- function(n) {
  if (is.null(n) || identical(n$kind, "null")) return(NULL)
  cls <- as.character(unlist(n$class))
  finish <- function(x) {
    if (!is.null(n$names)) {
      names(x) <- vapply(n$names, function(v) if (is.null(v)) NA_character_ else as.character(v), "")
    }
    for (a in names(n$attrs)) attr(x, a) <- .b10_decode(n$attrs[[a]])
    if (!identical(class(x), cls)) class(x) <- cls
    x
  }
  if (identical(n$kind, "atomic")) {
    v <- vapply(n$values, function(v) if (is.null(v)) NA_character_ else as.character(v), "")
    x <- if ("factor" %in% cls) {
      factor(v, levels = as.character(unlist(n$levels)))
    } else if ("Date" %in% cls) {
      as.Date(v)
    } else switch(n$type,
      logical = as.logical(v), integer = as.integer(v), double = as.numeric(v),
      character = v, stop("cannot decode a canonical ", n$type, " vector"))
    return(finish(x))
  }
  if (identical(n$kind, "list")) return(finish(lapply(n$items, .b10_decode)))
  stop("cannot decode a canonical '", n$kind, "' node")
}

.b10_roundtrip <- function(x) {
  jsonlite::fromJSON(as.character(jsonlite::toJSON(
    x, auto_unbox = TRUE, null = "null", na = "null", digits = NA, force = TRUE)),
    simplifyVector = FALSE)
}

.b10_pins <- function() {
  jsonlite::fromJSON(testthat::test_path("fixtures", "ir-v1-results.json"),
                     simplifyVector = FALSE)
}

# Exactly the baseline call's server options and DSOMOP_* variables.
.b10_options <- function(pin, pattern) {
  opts <- lapply(pin$options, .b10_decode)
  stale <- setdiff(grep(pattern, names(options()), value = TRUE), names(opts))
  c(opts, stats::setNames(vector("list", length(stale)), stale))
}
.b10_env <- function(pin) {
  env <- vapply(pin$env, as.character, "")
  stale <- setdiff(grep("^DSOMOP_", names(Sys.getenv()), value = TRUE),
                   c(names(env), "DSOMOP_RESULT_CAPTURE_DIR", "DSOMOP_RESULT_CAPTURE_LABEL",
                     "DSOMOP_RESULT_CAPTURE_PKG"))
  c(env, stats::setNames(rep(NA_character_, length(stale)), stale))
}

# F-26 (design 7.6, endorsed ledger): an assigned baseline age band is no longer
# suppressed when it holds fewer than nfilter.tab persons (2.10.2:
# .computeAgeGroups() with the per-band small-cell rule, which made every band of
# the default fixture NA); it is .bandAges() of the age at index on the server's
# grid, and the published object is gated as a whole (design 7.3).
.B10_F26 <- paste("F-26: assigned age bands without per-band suppression",
                  "(.bandAges() on the server grid; design 7.6)")
.B10_CORRECTIONS <- list(
  list(pin = "r004", output = "demo", column = "age_group", reference = .B10_F26),
  list(pin = "r021", output = "demo", column = "age_group", reference = .B10_F26)
)

# The F-26 age band of each output row: index year (the row's cohort start in
# the plan's results cohort) minus year of birth, banded by .bandAges().
.b10_f26_age_group <- function(conn, person_id, cohort_definition_id) {
  cohort <- DBI::dbGetQuery(conn, paste0(
    "SELECT subject_id, cohort_start_date FROM cohort WHERE cohort_definition_id = ",
    as.integer(cohort_definition_id)))
  person <- DBI::dbGetQuery(conn, "SELECT person_id, year_of_birth FROM person")
  stopifnot(!anyDuplicated(cohort$subject_id), all(person_id %in% cohort$subject_id))
  index_year <- as.integer(substr(as.character(cohort$cohort_start_date[match(person_id, cohort$subject_id)]), 1L, 4L))
  .bandAges(index_year - person$year_of_birth[match(person_id, person$person_id)])
}

# The baseline result with each reviewed correction applied, checked first
# against the baseline values it keeps.
.b10_corrected <- function(pin, want, conn, info) {
  for (cr in Filter(function(x) identical(x$pin, pin$id), .B10_CORRECTIONS)) {
    k <- match(cr$output, vapply(want$names, as.character, ""))
    item <- want$items[[k]]
    cols <- vapply(item$names, as.character, "")
    j <- match(cr$column, cols)
    stopifnot(!is.na(k), !is.na(j), identical(item$kind, "data.frame"))
    plan <- .b10_decode(pin$plan)
    person_id <- as.integer(vapply(item$columns[[match("person_id", cols)]]$values, as.character, ""))
    expected <- .b10_f26_age_group(conn, person_id, plan$cohort$cohort_definition_id)
    kept <- !vapply(item$columns[[j]]$values, is.null, logical(1))
    expect_identical(vapply(item$columns[[j]]$values[kept], as.character, ""), expected[kept],
                     info = paste(info, cr$reference, "keeps the unsuppressed baseline bands"))
    item$columns[[j]]$values <- lapply(expected, function(v) if (is.na(v)) NULL else v)
    want$items[[k]] <- item
  }
  want
}

test_that("the result pins are baseline captures of safe-class plans", {
  fx <- .b10_pins()
  expect_identical(fx$baseline$dsOMOP, "30b7905617dbe3de5d06699c324095635c68011f")
  expect_gte(length(fx$pins), 1L)
  classes <- utils::read.csv(testthat::test_path("fixtures", "v1-corpus-classes.csv"),
                             stringsAsFactors = FALSE, check.names = FALSE)
  block <- sub("#.*$", "", classes$plan_id)
  for (pin in fx$pins) {
    for (key in c(pin$key, vapply(pin$also, `[[`, "", "key"))) {
      cls <- unique(classes$class[block == key])
      expect_true("safe" %in% cls, info = paste(pin$id, key))
      # a mixed block's call is pinned only with a reviewed attribution to its safe row
      if (length(cls) > 1L) expect_true(is.character(pin$attribution[[key]]), info = paste(pin$id, key))
    }
    # the pinned plan object is reconstructed exactly
    expect_identical(.b10_roundtrip(.b10_canon(.b10_decode(pin$plan), 17L)), pin$plan,
                     info = paste(pin$id, "plan reconstruction"))
  }
})

test_that("safe-class v1 plans reproduce their 2.10.2 results on the default fixture", {
  fx <- .b10_pins()
  for (pin in fx$pins) {
    info <- paste0(pin$id, " (", pin$key, ")")
    handle <- create_test_handle()
    got <- withr::with_options(.b10_options(pin, fx$option_pattern),
      withr::with_envvar(.b10_env(pin), tryCatch(
        .planExecute(handle, .b10_decode(pin$plan), .b10_decode(pin$out_symbols),
                     output_mode = pin$output_mode),
        error = function(e) e)))
    want <- withr::with_options(.b10_options(pin, fx$option_pattern),
                                .b10_corrected(pin, pin$result, handle$conn, info))
    cleanup_handle(handle)
    if (inherits(got, "error")) {
      fail(paste0(info, ": refused: ", conditionMessage(got)))
      next
    }
    got <- .b10_roundtrip(.b10_canon(got, 15L))
    expect_identical(got$names, want$names, info = paste(info, "output names and order"))
    got_items <- stats::setNames(got$items, vapply(.b10_or(got$names, list()), as.character, ""))
    for (i in seq_along(want$items)) {
      nm <- as.character(want$names[[i]])
      w <- want$items[[i]]
      g <- got_items[[nm]]
      at <- paste(info, "output", nm)
      if (is.null(g)) {
        fail(paste(at, "is missing"))
        next
      }
      expect_identical(g$kind, w$kind, info = paste(at, "structure"))
      expect_identical(g$class, w$class, info = paste(at, "class"))
      if (identical(w$kind, "data.frame")) {
        by_column <- function(x, f) {
          stats::setNames(lapply(x$columns, f), vapply(x$names, as.character, ""))
        }
        expect_identical(g$names, w$names, info = paste(at, "column names"))
        expect_identical(by_column(g, function(col) col[c("class", "type", "levels")]),
                         by_column(w, function(col) col[c("class", "type", "levels")]),
                         info = paste(at, "column types"))
        expect_identical(g$nrow, w$nrow, info = paste(at, "row count"))
        expect_identical(by_column(g, function(col) col$values), by_column(w, function(col) col$values),
                         info = paste(at, "values in returned row order"))
      } else if (identical(w$kind, "list")) {
        expect_identical(g$names, w$names, info = paste(at, "component inventory"))
      }
      expect_identical(g, w, info = paste(at, "complete canonical form"))
    }
    expect_identical(got, want, info = paste(info, "complete canonical result"))
  }
})
