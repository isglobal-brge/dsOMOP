# Rendered-SQL snapshots of every typed SQL helper for every dialect family
# (design-2.10.3 sections 6.3 and 9). For the SQL-contract-only families
# (SQL Server/Synapse/PDW, Oracle, Redshift, BigQuery, Snowflake,
# Spark/Databricks) these snapshots, reviewed against vendor documentation,
# are the verification; the executable families are also executed
# (test-sql-expr.R, test-vendor-db-integration-sql-expr.R).

.ws3_snapshot <- function(render) {
  for (f in .SQL_FAMILIES) {
    sql <- render(f)
    cat(sprintf("%-10s %s\n", f, if (is.null(sql)) "<unsupported: NULL>" else
      sql))
  }
}

test_that(".sqlDateAddDays renders per family", {
  expect_snapshot({
    .ws3_snapshot(function(f) .sqlDateAddDays(f, "t.start_date", 30L))
    .ws3_snapshot(function(f) .sqlDateAddDays(f, "t.start_date", -7L))
    .ws3_snapshot(function(f) .sqlDateAddDays(f, "t.start_date",
                                              "t.days_supply - 1"))
    .ws3_snapshot(function(f) .sqlDateAddDays(f, "r.end_date", "r.persistence"))
  })
})

test_that(".sqlDateDiffDays renders per family", {
  expect_snapshot({
    .ws3_snapshot(function(f) .sqlDateDiffDays(f, "t.start_date", "t.end_date"))
    .ws3_snapshot(function(f) .sqlDateDiffDays(
      f, "a.anchor_date", .sqlDateAddDays(f, "e.start_date", 1L)))
  })
})

test_that(".sqlYear, .sqlMonth and .sqlMonthKey render per family", {
  expect_snapshot({
    .ws3_snapshot(function(f) .sqlYear(f, "t.d"))
    .ws3_snapshot(function(f) .sqlMonth(f, "t.d"))
    .ws3_snapshot(function(f) .sqlMonthKey(f, "t.d"))
  })
})

test_that(".sqlCalendarFloor renders per family", {
  expect_snapshot({
    .ws3_snapshot(function(f) .sqlCalendarFloor(f, "t.d", "month"))
    .ws3_snapshot(function(f) .sqlCalendarFloor(f, "t.d", "quarter"))
    .ws3_snapshot(function(f) .sqlCalendarFloor(f, "t.d", "year"))
  })
})

test_that(".sqlCalendarAdvance renders per family", {
  expect_snapshot({
    .ws3_snapshot(function(f) .sqlCalendarAdvance(f, "t.d", 1L, "month"))
    .ws3_snapshot(function(f) .sqlCalendarAdvance(f, "t.d", "b.n", "quarter"))
    .ws3_snapshot(function(f) .sqlCalendarAdvance(f, "t.d", -2L, "year"))
  })
})

test_that(".sqlMakeDate renders per family", {
  expect_snapshot({
    .ws3_snapshot(function(f) .sqlMakeDate(f, "p.year_of_birth",
                                           "p.month_of_birth",
                                           "p.day_of_birth"))
    .ws3_snapshot(function(f) .sqlMakeDate(f, 2020L, 2L, 29L))
    .ws3_snapshot(function(f) .sqlMakeDate(f, 2019L, 2L, 29L))
  })
})

test_that(".sqlDateLiteral and .sqlIntegerLiteral render per family", {
  expect_snapshot({
    .ws3_snapshot(function(f) .sqlDateLiteral(f, "2020-01-01"))
    .ws3_snapshot(function(f) .sqlIntegerLiteral(f, 42L))
    .ws3_snapshot(function(f) .sqlIntegerLiteral(f, -3L))
    .ws3_snapshot(function(f) .sqlIntegerLiteral(f, "9007199254740993"))
  })
})

test_that(".sqlLeast and .sqlGreatest render per family", {
  expect_snapshot({
    .ws3_snapshot(function(f) .sqlLeast(f, "a.end_date", "b.op_end"))
    .ws3_snapshot(function(f) .sqlGreatest(f, "x.a", "x.b", "x.c"))
    .ws3_snapshot(function(f) .sqlGreatest(f, 0L, "x.overlap"))
    .ws3_snapshot(function(f) .sqlLeast(f, "x.a"))
  })
})

test_that(".sqlNullsLast renders per family", {
  expect_snapshot({
    .ws3_snapshot(function(f) .sqlNullsLast(f, "e.value"))
    .ws3_snapshot(function(f) .sqlNullsLast(f, "e.value", descending = TRUE))
  })
})

test_that(".sqlRowNumber and .sqlWindow render per family", {
  expect_snapshot({
    .ws3_snapshot(function(f) .sqlRowNumber(f, "r.person_id",
                                            c("e.start_date", "e.event_id")))
    .ws3_snapshot(function(f) .sqlRowNumber(
      f, character(0), c(.sqlNullsLast(f, "e.value"), "e.event_id")))
    .ws3_snapshot(function(f) .sqlWindow(
      f, paste0("MAX(", .sqlDateAddDays(f, "r.end_date", 0L), ")"),
      c("r.person_id", "r.group_id"), c("r.start_date", "r.event_id"),
      frame = "preceding"))
    .ws3_snapshot(function(f) .sqlWindow(
      f, "SUM(r.new_era)", c("r.person_id", "r.group_id"),
      c("r.start_date", "r.event_id"), frame = "cumulative"))
    .ws3_snapshot(function(f) .sqlWindow(f, "LAG(r.end_date)", "r.person_id",
                                         c("r.start_date", "r.event_id")))
  })
})

test_that(".sqlLosslessKey, .sqlLosslessKeyText and .sqlDateText render per family", {
  expect_snapshot({
    .ws3_snapshot(function(f) .sqlLosslessKey(f, "e.event_id"))
    .ws3_snapshot(function(f) .sqlLosslessKeyText(f, "e.event_id"))
    .ws3_snapshot(function(f) .sqlDateText(f, "c.cohort_start_date"))
  })
})

test_that(".sqlSha256Hex renders per family (NULL = unsupported)", {
  expect_snapshot({
    .ws3_snapshot(function(f) .sqlSha256Hex(f, "t.label"))
    .ws3_snapshot(function(f) .sqlSha256Hex(f, "t.encoding_hex", input = "hex"))
  })
})

test_that("the availability matrix renders per family", {
  expect_snapshot(print(noquote(.sqlHelperMatrix())))
})
