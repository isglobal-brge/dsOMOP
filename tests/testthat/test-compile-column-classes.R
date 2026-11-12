# WS-4: column classes on the compiled event paths (design-2.10.3 section
# 7.6), asserted side by side with aggregate admission on the shared column
# corpus (inst/column_classes.csv): class 1 is never projected or filtered by
# an event, classes 2 and 3 are ordinary event columns, and the aggregate
# classification of the same columns is unchanged.

test_that("event fragments follow the assignment class of every corpus column; aggregate admission is unchanged", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  corpus <- utils::read.csv(testthat::test_path("..", "..", "inst", "column_classes.csv"),
                            stringsAsFactors = FALSE)
  bp <- .buildBlueprint(h)
  checked <- 0L
  for (i in seq_len(nrow(corpus))) {
    column <- corpus$column[[i]]
    table <- corpus$table[[i]]
    role <- .roleOf(h, table)
    if (is.null(role) || !column %in% role$columns$column_name) next
    col_df <- bp$columns[[table]]
    info <- paste(table, column)
    # assignment class of the role record = the corpus (one predicate)
    expect_identical(role$columns$assignment_class[role$columns$column_name == column],
                     corpus$assignment_class[[i]], info = info)
    # aggregate admission keeps its own rule, independently of assignment
    aggregate_blocked <- .detectSensitiveColumns(column)
    expect_identical(if (aggregate_blocked) "blocked" else "admitted",
                     corpus$aggregate_admission[[i]], info = info)
    ctx <- .ws4_ctx(h, .ws4_plan(events = list(e = list(table = table))))
    f <- .compileEventByName(ctx, "e")
    catalogue <- .eventMeta(ctx, "e")$catalogue
    literal <- if (grepl("_date$", column)) "2020-01-01" else 1
    probe <- function() .predicateSql(h, list(var = column, op = "==", value = literal),
                                      catalogue, "n")
    if (identical(corpus$assignment_class[[i]], 1L)) {
      expect_false(column %in% names(f$columns), info = info)
      .ws4_expect_rejection(probe(), pattern = "class-1 column")
    } else if (column %in% .identifierColumns()) {
      expect_false(column %in% names(f$columns), info = info)
      .ws4_expect_rejection(probe(), pattern = "never filterable")
    } else {
      expect_match(probe(), "^\\(n\\.", info = info)
      resolved <- .predicateResolve(catalogue, column, "/")
      expect_true(resolved$column %in% names(f$columns), info = info)
    }
    checked <- checked + 1L
  }
  expect_gte(checked, 12L)
})

test_that("class-2 birth components and exact age stay assignable while aggregates keep blocking them", {
  for (column in c("year_of_birth", "month_of_birth", "day_of_birth", "birth_datetime")) {
    expect_false(.assignmentBlockedColumn(column), info = column)
    expect_true(.assignmentQuasiIdentifier(column), info = column)
    expect_true(.detectSensitiveColumns(column), info = column)
  }
  expect_true(.assignmentBlockedColumn("person_source_value"))
  expect_true(.assignmentBlockedColumn("condition_source_concept_id"))
  expect_identical(.bandAges(c(0, 17, 18, 64, 65), c(0L, 18L, 65L)),
                   c("0-17", "0-17", "18-64", "18-64", "65+"))
})

test_that("person predicates may name birth components (class 2) but never class-1 columns or keys", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  cat_p <- .predicateTableCatalogue(h, "person")
  sql <- .predicateSql(h, list(and = list(list(var = "year_of_birth", op = ">=", value = 1950),
                                          list(var = "month_of_birth", op = "in", value = list(6)))),
                       cat_p, "p")
  rows <- .executeQuery(h, paste0("SELECT p.person_id FROM person p WHERE ", sql))
  expect_gt(nrow(rows), 0L)
  .ws4_expect_rejection(.predicateSql(h, list(var = "person_source_value", op = "==", value = "x"), cat_p, "p"),
                        pattern = "class-1 column")
  .ws4_expect_rejection(.predicateSql(h, list(var = "person_id", op = "==", value = 1), cat_p, "p"),
                        pattern = "never filterable")
  .ws4_expect_rejection(.predicateTableCatalogue(h, "note"), code = "unavailable_table")
})
