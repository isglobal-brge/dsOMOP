# Shared executable cases for the typed SQL helpers (R/sql_expr.R, design
# sections 6.3 and 9). The same rows and the same independent R reference
# values drive the embedded backends (SQLite, DuckDB: test-sql-expr.R) and the
# live vendor services (PostgreSQL, MySQL, MariaDB:
# test-vendor-db-integration-sql-expr.R). Every operand is a COLUMN of a
# temporary table (dynamic forms, defect A-E8), never a literal, so no backend
# can fold the expressions at plan time.

.sqlExprCaseTable <- "dsomop_sqlexpr_cases"

# One field of the dsomop-match-v1 encoding (design section 6.10), as hex.
.sqlExprMatchField <- function(tag, text = NULL) {
  bytes <- if (is.null(text)) raw(0) else charToRaw(enc2utf8(as.character(text)))
  n <- length(bytes)
  len <- as.raw(c(bitwAnd(bitwShiftR(n, 24L), 255L),
                  bitwAnd(bitwShiftR(n, 16L), 255L),
                  bitwAnd(bitwShiftR(n, 8L), 255L), bitwAnd(n, 255L)))
  c(as.raw(tag), len, bytes)
}

.sqlExprMatchEncodingHex <- function(seed, namespace, subject_id, start, end,
                                     source, key, control) {
  e <- c(.sqlExprMatchField(0x02, "dsomop-match-v1"),
         .sqlExprMatchField(0x01, format(seed, scientific = FALSE)),
         .sqlExprMatchField(0x02, namespace),
         .sqlExprMatchField(0x01, format(subject_id, scientific = FALSE)),
         .sqlExprMatchField(0x03, start),
         if (is.null(end)) .sqlExprMatchField(0x00) else
           .sqlExprMatchField(0x03, end),
         .sqlExprMatchField(0x02, source),
         if (is.null(key)) .sqlExprMatchField(0x00) else
           .sqlExprMatchField(0x02, key),
         .sqlExprMatchField(0x01, format(control, scientific = FALSE)))
  paste(as.character(e), collapse = "")
}

.sqlExprHexToRaw <- function(hex) {
  if (!nzchar(hex)) return(raw(0))
  as.raw(strtoi(substring(hex, seq(1L, nchar(hex), 2L),
                          seq(2L, nchar(hex), 2L)), 16L))
}

.sqlExprSha256 <- function(bytes) paste(as.character(openssl::sha256(bytes)),
                                        collapse = "")

# The published matching vectors (S/tests/testthat/fixtures/match-vectors.json).
.sqlExprMatchVectors <- function() {
  path <- testthat::test_path("fixtures", "match-vectors.json")
  jsonlite::fromJSON(path, simplifyVector = FALSE)$vectors
}

.sqlExprCaseRows <- function() {
  vectors <- .sqlExprMatchVectors()
  encodings <- vapply(vectors, function(x) .sqlExprMatchEncodingHex(
    x$seed, x$namespace, x$subject_id, x$cohort_start_date,
    x$cohort_end_date, x$index_source, x$index_key, x$control
  ), character(1))
  data.frame(
    id = 1:12,
    grp = rep(1:3, each = 4L),
    d1 = c("2020-01-31", "2019-12-31", "2020-02-29", "2000-03-01",
           "1970-01-01", NA, "2021-11-15", "2023-05-17",
           "1999-12-31", "2024-02-29", "2020-12-31", "2016-08-08"),
    d2 = c("2020-03-01", "2020-01-01", "2021-02-28", "1999-02-28",
           "1969-12-31", "2020-06-15", NA, "2023-05-17",
           "2000-01-01", "2024-03-31", "2021-01-01", "2016-08-07"),
    n1 = c(1L, -1L, 365L, -366L, 0L, NA, 45L, -30L, 30000L, 1L, -1L, 7L),
    k = c(0L, 1L, -1L, 13L, 4L, 2L, NA, -14L, 3L, 1L, 0L, -5L),
    y = c(2020L, 2019L, 2021L, 1900L, 2000L, NA, 2021L, 2023L, 1000L, 2024L,
          9999L, 999L),
    m = c(2L, 2L, 13L, 2L, 2L, 1L, 4L, 12L, 1L, 2L, 12L, 1L),
    dd = c(29L, 29L, 1L, 29L, 29L, 1L, 31L, 31L, 1L, 30L, 31L, 1L),
    big = c("1", "-1", "2147483648", "9007199254740993", "-9007199254740993",
            "123456789012", NA, "0", "42", "7", "4611686018427387904", "-5"),
    kd = c(42, 1e15, -3, 0, NA, 7, 123456789, 2, 1, 5, 6, 9),
    t = c("abc", "", "é", "dsomop", NA, "a'b", "x", "y", "z", "  ",
          "中", "Z"),
    hx = c(encodings, "", "616263", NA, "00", "ff", "c3a9", "0102", "dead",
           "beef"),
    stringsAsFactors = FALSE
  )
}

.sqlExprQuoteText <- function(conn, x) {
  if (is.na(x)) "NULL" else as.character(DBI::dbQuoteString(conn, enc2utf8(x)))
}

# Create and fill the case table as a session TEMPORARY table.
.sqlExprLoadCases <- function(conn, family, rows = .sqlExprCaseRows()) {
  date_type <- if (identical(family, "sqlite")) "TEXT" else "DATE"
  double_type <- switch(family, postgresql = "DOUBLE PRECISION",
                        sqlite = "REAL", "DOUBLE")
  text_type <- if (identical(family, "mysql")) {
    "VARCHAR(400) CHARACTER SET utf8mb4"
  } else {
    "VARCHAR(400)"
  }
  DBI::dbExecute(conn, paste0(
    "CREATE TEMPORARY TABLE ", .sqlExprCaseTable, " (id INTEGER, ",
    "grp INTEGER, d1 ", date_type, ", d2 ", date_type, ", n1 INTEGER, ",
    "k INTEGER, y INTEGER, m INTEGER, dd INTEGER, big BIGINT, ",
    "bigtext ", text_type, ", kd ", double_type, ", t ", text_type, ", hx ",
    text_type, ")"
  ))
  lit_int <- function(x) if (is.na(x)) "NULL" else as.character(x)
  lit_date <- function(x) if (is.na(x)) "NULL" else paste0("'", x, "'")
  lit_dbl <- function(x) if (is.na(x)) "NULL" else
    formatC(x, format = "f", digits = 1L)
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, ]
    DBI::dbExecute(conn, paste0(
      "INSERT INTO ", .sqlExprCaseTable, " VALUES (",
      paste(c(lit_int(r$id), lit_int(r$grp), lit_date(r$d1), lit_date(r$d2),
              lit_int(r$n1), lit_int(r$k), lit_int(r$y), lit_int(r$m),
              lit_int(r$dd),
              if (is.na(r$big)) "NULL" else r$big,
              .sqlExprQuoteText(conn, r$big),
              lit_dbl(r$kd), .sqlExprQuoteText(conn, r$t),
              .sqlExprQuoteText(conn, r$hx)), collapse = ", "),
      ")"
    ))
  }
  invisible(rows)
}

# --- Independent R references ---------------------------------------------------

.sqlExprDateChr <- function(x) as.character(as.Date(x))

.sqlExprFloorChr <- function(d, unit) {
  d <- as.Date(d)
  out <- rep(NA_character_, length(d))
  ok <- !is.na(d)
  y <- as.integer(format(d[ok], "%Y"))
  m <- as.integer(format(d[ok], "%m"))
  m0 <- switch(unit, month = m, quarter = ((m - 1L) %/% 3L) * 3L + 1L,
               year = rep(1L, length(m)))
  out[ok] <- sprintf("%04d-%02d-01", y, m0)
  out
}

.sqlExprAdvanceChr <- function(d, n, unit) {
  floor <- .sqlExprFloorChr(d, unit)
  per <- switch(unit, month = 1L, quarter = 3L, year = 12L)
  out <- rep(NA_character_, length(floor))
  ok <- !is.na(floor) & !is.na(n)
  y <- as.integer(substr(floor[ok], 1L, 4L))
  m <- as.integer(substr(floor[ok], 6L, 7L))
  total <- y * 12L + (m - 1L) + n[ok] * per
  out[ok] <- sprintf("%04d-%02d-01", total %/% 12L, total %% 12L + 1L)
  out
}

.sqlExprMakeDateChr <- function(y, m, d) {
  vapply(seq_along(y), function(i) {
    if (is.na(y[[i]]) || is.na(m[[i]]) || is.na(d[[i]])) return(NA_character_)
    if (y[[i]] < 1000L || y[[i]] > 9999L || m[[i]] < 1L || m[[i]] > 12L ||
        d[[i]] < 1L) return(NA_character_)
    first <- as.Date(sprintf("%04d-%02d-01", y[[i]], m[[i]]))
    days <- as.integer(seq(first, by = "1 month", length.out = 2L)[[2L]] -
                         first)
    if (d[[i]] > days) return(NA_character_)
    sprintf("%04d-%02d-%02d", y[[i]], m[[i]], d[[i]])
  }, character(1))
}

.sqlExprRowExtremum <- function(fn, ...) {
  cols <- list(...)
  vapply(seq_along(cols[[1L]]), function(i) {
    values <- vapply(cols, function(col) as.numeric(col[[i]]), numeric(1))
    values <- values[!is.na(values)]
    if (!length(values)) NA_real_ else fn(values)
  }, numeric(1))
}

.sqlExprDateExtremumChr <- function(fn, a, b) {
  num <- .sqlExprRowExtremum(fn, as.numeric(as.Date(a)), as.numeric(as.Date(b)))
  as.character(as.Date(num, origin = "1970-01-01"))
}

.sqlExprPrecedingMax <- function(rows, col) {
  out <- rep(NA_integer_, nrow(rows))
  for (g in unique(rows$grp)) {
    idx <- which(rows$grp == g)
    idx <- idx[order(rows$id[idx])]
    for (j in seq_along(idx)) {
      prev <- rows[[col]][idx[seq_len(j - 1L)]]
      prev <- prev[!is.na(prev)]
      out[[idx[[j]]]] <- if (length(prev)) max(prev) else NA_integer_
    }
  }
  out
}

.sqlExprCumulativeSum <- function(rows, col) {
  out <- rep(NA_integer_, nrow(rows))
  for (g in unique(rows$grp)) {
    idx <- which(rows$grp == g)
    idx <- idx[order(rows$id[idx])]
    for (j in seq_along(idx)) {
      upto <- rows[[col]][idx[seq_len(j)]]
      upto <- upto[!is.na(upto)]
      out[[idx[[j]]]] <- if (length(upto)) as.integer(sum(upto)) else
        NA_integer_
    }
  }
  out
}

.sqlExprRowNumberNullsLast <- function(rows, col) {
  out <- integer(nrow(rows))
  for (g in unique(rows$grp)) {
    idx <- which(rows$grp == g)
    key <- rows[[col]][idx]
    ord <- idx[order(is.na(key), key, rows$id[idx])]
    out[ord] <- seq_along(ord)
  }
  out
}

# --- The checks -------------------------------------------------------------------

# Each check: list(sql = <SELECT expression over alias c>, expected, as) where
# `as` normalises the fetched column ("date", "int", "chr"); or an ordering
# check list(order = <ORDER BY items>, expected = <ids>).
.sqlExprChecks <- function(f, rows = .sqlExprCaseRows()) {
  d1 <- rows$d1
  d2 <- rows$d2
  ref_add <- function(d, n) .sqlExprDateChr(as.Date(d) + n)
  checks <- list(
    add_dynamic = list(sql = .sqlDateAddDays(f, "c.d1", "c.n1"),
                       expected = ref_add(d1, rows$n1), as = "date"),
    add_arithmetic = list(sql = .sqlDateAddDays(f, "c.d1", "c.n1 - 1"),
                          expected = ref_add(d1, rows$n1 - 1L), as = "date"),
    add_literal = list(sql = .sqlDateAddDays(f, "c.d1", 30L),
                       expected = ref_add(d1, 30L), as = "date"),
    add_negative_literal = list(sql = .sqlDateAddDays(f, "c.d1", -366L),
                                expected = ref_add(d1, -366L), as = "date"),
    add_nested = list(
      sql = .sqlDateAddDays(f, .sqlDateAddDays(f, "c.d1", "c.n1"), "c.k"),
      expected = ref_add(as.Date(d1) + rows$n1, rows$k), as = "date"),
    add_to_literal = list(
      sql = .sqlDateAddDays(f, .sqlDateLiteral(f, "2020-02-28"), "c.n1"),
      expected = ref_add(rep("2020-02-28", nrow(rows)), rows$n1),
      as = "date"),
    diff_dynamic = list(sql = .sqlDateDiffDays(f, "c.d1", "c.d2"),
                        expected = as.integer(as.Date(d2) - as.Date(d1)),
                        as = "int"),
    diff_round_trip = list(
      sql = .sqlDateDiffDays(f, "c.d1", .sqlDateAddDays(f, "c.d1", "c.n1")),
      expected = ifelse(is.na(d1), NA_integer_, rows$n1), as = "int"),
    diff_arithmetic = list(
      sql = paste0(.sqlDateDiffDays(f, "c.d1", "c.d2"), " + 1"),
      expected = as.integer(as.Date(d2) - as.Date(d1)) + 1L, as = "int"),
    year = list(sql = .sqlYear(f, "c.d1"),
                expected = as.integer(format(as.Date(d1), "%Y")), as = "int"),
    month = list(sql = .sqlMonth(f, "c.d1"),
                 expected = as.integer(format(as.Date(d1), "%m")), as = "int"),
    month_key = list(sql = .sqlMonthKey(f, "c.d1"),
                     expected = as.integer(format(as.Date(d1), "%Y")) * 100L +
                       as.integer(format(as.Date(d1), "%m")), as = "int"),
    floor_month = list(sql = .sqlCalendarFloor(f, "c.d1", "month"),
                       expected = .sqlExprFloorChr(d1, "month"), as = "date"),
    floor_quarter = list(sql = .sqlCalendarFloor(f, "c.d1", "quarter"),
                         expected = .sqlExprFloorChr(d1, "quarter"),
                         as = "date"),
    floor_year = list(sql = .sqlCalendarFloor(f, "c.d1", "year"),
                      expected = .sqlExprFloorChr(d1, "year"), as = "date"),
    advance_month = list(sql = .sqlCalendarAdvance(f, "c.d1", "c.k", "month"),
                         expected = .sqlExprAdvanceChr(d1, rows$k, "month"),
                         as = "date"),
    advance_quarter = list(
      sql = .sqlCalendarAdvance(f, "c.d1", "c.k", "quarter"),
      expected = .sqlExprAdvanceChr(d1, rows$k, "quarter"), as = "date"),
    advance_year = list(sql = .sqlCalendarAdvance(f, "c.d1", "c.k", "year"),
                        expected = .sqlExprAdvanceChr(d1, rows$k, "year"),
                        as = "date"),
    advance_literal = list(
      sql = .sqlCalendarAdvance(f, "c.d1", -1L, "quarter"),
      expected = .sqlExprAdvanceChr(d1, rep(-1L, nrow(rows)), "quarter"),
      as = "date"),
    make_date = list(sql = .sqlMakeDate(f, "c.y", "c.m", "c.dd"),
                     expected = .sqlExprMakeDateChr(rows$y, rows$m, rows$dd),
                     as = "date"),
    make_date_literal = list(
      sql = .sqlMakeDate(f, 2024L, 2L, 29L),
      expected = rep("2024-02-29", nrow(rows)), as = "date"),
    make_date_literal_invalid = list(
      sql = .sqlMakeDate(f, 2023L, 2L, 29L),
      expected = rep(NA_character_, nrow(rows)), as = "date"),
    date_literal_predicate = list(
      sql = paste0("CASE WHEN c.d1 >= ", .sqlDateLiteral(f, "2020-01-01"),
                   " THEN 1 ELSE 0 END"),
      expected = ifelse(!is.na(d1) & d1 >= "2020-01-01", 1L, 0L), as = "int"),
    integer_literal = list(
      sql = paste0("c.n1 - ", .sqlIntegerLiteral(f, -2L)),
      expected = rows$n1 + 2L, as = "int"),
    least_dates = list(sql = .sqlLeast(f, "c.d1", "c.d2"),
                       expected = .sqlExprDateExtremumChr(min, d1, d2),
                       as = "date"),
    greatest_dates = list(sql = .sqlGreatest(f, "c.d1", "c.d2"),
                          expected = .sqlExprDateExtremumChr(max, d1, d2),
                          as = "date"),
    least_three = list(
      sql = .sqlLeast(f, "c.n1", "c.k", "c.y"),
      expected = as.integer(.sqlExprRowExtremum(min, rows$n1, rows$k, rows$y)),
      as = "int"),
    greatest_three = list(
      sql = .sqlGreatest(f, "c.n1", "c.k", 5L),
      expected = as.integer(.sqlExprRowExtremum(max, rows$n1, rows$k,
                                                rep(5L, nrow(rows)))),
      as = "int"),
    window_preceding_max = list(
      sql = .sqlWindow(f, "MAX(c.n1)", "c.grp", "c.id", frame = "preceding"),
      expected = .sqlExprPrecedingMax(rows, "n1"), as = "int"),
    window_cumulative_sum = list(
      sql = .sqlWindow(f, "SUM(c.k)", "c.grp", "c.id", frame = "cumulative"),
      expected = .sqlExprCumulativeSum(rows, "k"), as = "int"),
    window_partition_max = list(
      sql = .sqlWindow(f, "MAX(c.k)", "c.grp", frame = "partition"),
      expected = as.integer(ave(rows$k, rows$grp, FUN = function(v)
        if (all(is.na(v))) NA else max(v, na.rm = TRUE))), as = "int"),
    row_number_nulls_last = list(
      sql = .sqlRowNumber(f, "c.grp", c(.sqlNullsLast(f, "c.d1"), "c.id")),
      expected = .sqlExprRowNumberNullsLast(rows, "d1"), as = "int"),
    lossless_key_text = list(sql = .sqlLosslessKeyText(f, "c.big"),
                             expected = rows$big, as = "chr"),
    lossless_key_from_text = list(sql = .sqlLosslessKeyText(f, "c.bigtext"),
                                  expected = rows$big, as = "chr"),
    lossless_key_from_double = list(
      sql = .sqlLosslessKeyText(f, "c.kd"),
      expected = ifelse(is.na(rows$kd), NA_character_,
                        formatC(rows$kd, format = "f", digits = 0L)),
      as = "chr"),
    lossless_key_equality = list(
      sql = paste0("CASE WHEN ", .sqlLosslessKey(f, "c.big"), " = ",
                   .sqlLosslessKey(f, "c.bigtext"), " THEN 1 ELSE 0 END"),
      expected = ifelse(is.na(rows$big), 0L, 1L), as = "int"),
    date_text = list(sql = .sqlDateText(f, "c.d1"), expected = d1, as = "chr")
  )
  hash_text <- .sqlSha256Hex(f, "c.t")
  if (!is.null(hash_text)) {
    checks$sha256_text <- list(
      sql = hash_text, as = "chr",
      expected = vapply(rows$t, function(x) if (is.na(x)) NA_character_ else
        .sqlExprSha256(charToRaw(enc2utf8(x))), character(1),
        USE.NAMES = FALSE))
    checks$sha256_hex <- list(
      sql = .sqlSha256Hex(f, "c.hx", input = "hex"), as = "chr",
      expected = vapply(rows$hx, function(x) if (is.na(x)) NA_character_ else
        .sqlExprSha256(.sqlExprHexToRaw(x)), character(1),
        USE.NAMES = FALSE))
  }
  checks
}

.sqlExprOrderChecks <- function(f, rows = .sqlExprCaseRows()) {
  d1 <- as.Date(rows$d1)
  list(
    nulls_last_date = list(
      order = c(.sqlNullsLast(f, "c.d1"), "c.id"),
      expected = rows$id[order(is.na(d1), d1, rows$id)]),
    nulls_last_date_desc = list(
      order = c(.sqlNullsLast(f, "c.d1", descending = TRUE), "c.id"),
      expected = rows$id[order(is.na(d1), -as.numeric(d1), rows$id)]),
    nulls_last_number = list(
      order = c(.sqlNullsLast(f, "c.n1"), "c.id"),
      expected = rows$id[order(is.na(rows$n1), rows$n1, rows$id)])
  )
}

.sqlExprNormalise <- function(v, as) {
  switch(as,
    date = as.character(as.Date(if (is.logical(v)) as.character(v) else v)),
    int = as.integer(if (inherits(v, "integer64")) as.character(v) else v),
    chr = {
      x <- if (inherits(v, "integer64")) as.character(v) else v
      x <- as.character(x)
      x
    }
  )
}

# Run every check on one connection; returns the number of checks run.
.sqlExprRunChecks <- function(conn, family, label) {
  rows <- .sqlExprLoadCases(conn, family)
  on.exit(try(DBI::dbExecute(conn, paste0("DROP TABLE IF EXISTS ",
                                          .sqlExprCaseTable)), silent = TRUE),
          add = TRUE)
  checks <- .sqlExprChecks(family, rows)
  for (name in names(checks)) {
    check <- checks[[name]]
    sql <- paste0("SELECT c.id AS id, ", check$sql, " AS v FROM ",
                  .sqlExprCaseTable, " c ORDER BY c.id")
    .sqlAssertTypedSql(family, sql)
    got <- DBI::dbGetQuery(conn, sql)
    testthat::expect_identical(as.integer(got$id), rows$id,
                               info = paste(label, name, "row order"))
    testthat::expect_identical(.sqlExprNormalise(got$v, check$as),
                               check$expected,
                               info = paste(label, name, check$sql))
  }
  orders <- .sqlExprOrderChecks(family, rows)
  for (name in names(orders)) {
    check <- orders[[name]]
    sql <- paste0("SELECT c.id AS id FROM ", .sqlExprCaseTable,
                  " c ORDER BY ", paste(check$order, collapse = ", "))
    got <- DBI::dbGetQuery(conn, sql)
    testthat::expect_identical(as.integer(got$id), check$expected,
                               info = paste(label, name))
  }
  length(checks) + length(orders)
}

# A minimal handle around a raw connection (enough for .sqlDialectProfile).
.sqlExprHandle <- function(conn, dbms, target_dialect = dbms) {
  handle <- new.env(parent = emptyenv())
  handle$conn <- conn
  handle$dbms <- dbms
  handle$target_dialect <- target_dialect
  handle$resource_client <- NULL
  handle$temp_tables <- character(0)
  handle
}

# Print the runtime and driver actually executed (release evidence, section 9).
.sqlExprRecordRuntime <- function(profile, label, n_checks) {
  cat(sprintf("\n[sql-expr conformance] %s: family=%s engine=%s runtime=%s driver=%s checks=%d\n",
              label, profile$family, profile$engine,
              profile$runtime %||% "unknown", profile$driver %||% "unknown",
              as.integer(n_checks)))
  invisible(profile)
}
