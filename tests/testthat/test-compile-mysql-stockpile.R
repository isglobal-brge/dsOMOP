test_that("MySQL stockpile ordering parses only top-level clauses and preserves literals", {
  cols <- list(event_id = .fragmentColumn("bigint", FALSE, "private"),
                label = .fragmentColumn("text", TRUE, "measure"))
  f <- .fragment("ordered", "stream", "none", "SELECT 1 AS event_id, 'x' AS label", cols,
                  private_columns = "event_id")
  expect_identical(.mysqlExpandOrderAliases("CASE WHEN label = 'label' THEN 0 ELSE 1 END",
                                            c(label = "a.source_ns")),
                   "CASE WHEN (a.source_ns) = 'label' THEN 0 ELSE 1 END")
  expect_identical(.mysqlExpandOrderAliases("x + label /* label */", c(x = "label", label = "a.source_ns")),
                   "(label) + (a.source_ns) /* label */")
  expect_identical(.mysqlExpandOrderAliases("MAX(a.event_id) + max", c(max = "a.event_id")),
                   "MAX(a.event_id) + (a.event_id)")
  sql <- "SELECT a.event_id AS event_id, a.label AS label FROM source a ORDER BY a.event_id DESC, label ASC LIMIT 2"
  got <- .mysqlStockpileOrder(sql, f)
  expect_identical(got$main, sql)
  expect_identical(got$order, " ORDER BY dsomop_result.event_id DESC, dsomop_result.label ASC")
  sql <- paste0("SELECT a.label AS label FROM source a ORDER BY ",
                 "CASE WHEN label = 'label' THEN 0 ELSE 1 END DESC, a.event_id DESC LIMIT 1")
  got <- .mysqlStockpileOrder(sql, f)
  expect_match(got$main, "CASE WHEN (a.label) = 'label' THEN 0 ELSE 1 END AS dsomop_sort_1", fixed = TRUE)
  expect_identical(got$projection, "dsomop_result.label")
  expect_match(got$order, "dsomop_sort_1 DESC, dsomop_result.dsomop_sort_2 DESC", fixed = TRUE)
  nested <- "SELECT ROW_NUMBER() OVER (ORDER BY event_id) AS event_id, 'ORDER BY x' AS label FROM source"
  expect_identical(.mysqlStockpileOrder(nested, f)$order, "")
  expect_identical(.mysqlLocalSqlName("dsomop_payload_1", "DSOMOP_PAYLOAD_1"), "dsomop_payload_1_2")
  projected <- .mysqlStockpileOrder("SELECT a.event_id AS label FROM source a ORDER BY a.label", f)
  expect_match(projected$main, "a.label AS dsomop_sort_1", fixed = TRUE)
  expect_match(projected$order, "dsomop_sort_1", fixed = TRUE)
  projected <- .mysqlStockpileOrder("SELECT a.* FROM source a JOIN other b ON 1 = 1 ORDER BY b.event_id", f)
  expect_match(projected$main, "b.event_id AS dsomop_sort_1", fixed = TRUE)
})

test_that("MySQL stockpile payloads preserve exact values, ordering, empty sources and physical reads", {
  skip_if(!nzchar(Sys.getenv("DSOMOP_TEST_MYSQL_HOST")), "requires live MySQL 8.4")
  bk <- .ws5_backends(vendors = "mysql")
  on.exit(.ws4_cleanup(bk))
  h <- bk$mysql$handle
  expect_identical(.sqlDialectProfile(h, probe = TRUE)$engine, "mysql")
  first <- .reserveTempTableName(h, "ws5_json_first")
  second <- .reserveTempTableName(h, "ws5_json_second")
  on.exit({
    for (table in c(first, second)) if (table %in% h$temp_tables) .dropTempTable(h, table)
  }, add = TRUE, after = FALSE)
  .createTempTable(h, first, paste0(
    "SELECT CAST('9007199254740993' AS SIGNED) AS event_id, CAST('2020-02-29' AS DATE) AS start_date, ",
    "CONCAT('a', CHAR(39), CHAR(92), 'é', REPEAT('z', 2048)) AS label, CAST(NULL AS SIGNED) AS concept_id ",
    "UNION ALL SELECT CAST('9007199254740994' AS SIGNED), CAST(NULL AS DATE), 'b', 42"))
  .createTempTable(h, second, paste0("SELECT * FROM ", first, " WHERE 1 = 0"))
  cols <- list(event_id = .fragmentColumn("bigint", FALSE, "private"),
                start_date = .fragmentColumn("date", TRUE, "start"),
                label = .fragmentColumn("text", TRUE, "measure"),
                concept_id = .fragmentColumn("bigint", TRUE, "concept"))
  source <- function(name, table) .mysqlStockpileSource(
    .fragment(name, "interval", "none", paste0("SELECT * FROM ", table), cols,
               unique_key = "event_id", private_columns = "event_id"), table, h)
  a <- source("json_a", first)
  b <- source("json_b", first)
  empty <- source("json_empty", second)
  result_cols <- list(event_key = .fragmentColumn("text", FALSE, "measure"),
                       start_date = cols$start_date, label = cols$label, concept_id = cols$concept_id)
  f <- .fragment("json_result", "stream", "none", paste0(
    "SELECT CAST(dsomop_payload_1.event_id AS CHAR) AS event_key, b.start_date, b.label, b.concept_id ",
    "FROM json_a dsomop_payload_1 INNER JOIN json_b b ON b.event_id = dsomop_payload_1.event_id ",
    "LEFT JOIN json_empty e ON e.event_id = b.event_id"), result_cols,
    dependencies = c(a$name, b$name, empty$name))
  fragments <- list(a, b, empty, f)
  sql <- .buildStatement(fragments, f, select = "SELECT * FROM json_result ORDER BY event_key DESC", dialect = h)
  expect_identical(lengths(regmatches(sql, gregexpr(first, sql, fixed = TRUE))), 1L)
  expect_identical(lengths(regmatches(sql, gregexpr(second, sql, fixed = TRUE))), 1L)
  expect_match(sql, "dsomop_payload_1_2", fixed = TRUE)
  rows <- .executeQuery(h, sql)
  expect_identical(rows$event_key, c("9007199254740994", "9007199254740993"))
  expect_identical(as.character(rows$start_date), c(NA_character_, "2020-02-29"))
  expect_identical(rows$label, c("b", paste0("a'\\é", strrep("z", 2048))))
  expect_identical(as.numeric(rows$concept_id), c(42, NA_real_))
  # A failed aggregate or a short payload cannot silently become zero rows.
  aggregate <- "JSON_ARRAYAGG(JSON_ARRAY(event_id, start_date, label, concept_id))"
  failed_sql <- sub(aggregate, "NULL", sql, fixed = TRUE)
  expect_error(.executeQuery(h, failed_sql), "JSON|json")
  short_sql <- sub(aggregate, "JSON_ARRAY()", sql, fixed = TRUE)
  expect_error(.executeQuery(h, short_sql), "JSON|json")
  # A sort expression over an unprojected source column remains private;
  # LIMIT selects the same row and no helper column escapes the statement.
  label <- .fragment("json_label", "stream", "none",
    "SELECT a.label AS label FROM json_a a ORDER BY a.event_id DESC LIMIT 1",
    columns = list(label = cols$label), dependencies = a$name)
  rows <- .executeQuery(h, .buildStatement(fragments, label, dialect = h))
  expect_identical(names(rows), "label")
  expect_identical(rows$label, "b")
  # A volatile sort key is evaluated after DISTINCT, preserving row count.
  distinct <- .fragment("json_distinct", "stream", "none",
    "SELECT DISTINCT 1 AS one FROM json_a a ORDER BY RAND()",
    columns = list(one = .fragmentColumn("integer", FALSE, "measure")), dependencies = a$name)
  rows <- .executeQuery(h, .buildStatement(fragments, distinct, dialect = h))
  expect_identical(nrow(rows), 1L)
  expect_identical(names(rows), "one")
  # Empty payloads produce no event rows, but the one-row aggregate does not
  # erase an unrelated roster branch that deliberately keeps an uncovered row.
  kept <- .fragment("json_kept", "stream", "none",
    "SELECT 7 AS row_key, e.event_id FROM (SELECT 7 AS k) r LEFT JOIN json_empty e ON 1 = 1",
    columns = list(row_key = .fragmentColumn("integer", FALSE, "private"),
                   event_id = .fragmentColumn("bigint", TRUE, "private")),
    dependencies = empty$name, private_columns = c("row_key", "event_id"))
  rows <- .executeQuery(h, .buildStatement(fragments, kept, dialect = h))
  expect_identical(as.integer(rows$row_key), 7L)
  expect_true(is.na(rows$event_id))
})
