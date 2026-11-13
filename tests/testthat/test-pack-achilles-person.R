# G-16: the live Achilles person analyses 2/4/5/10/12 selected a joined
# concept_name without grouping it, which PostgreSQL, SQL Server, DuckDB,
# Oracle and BigQuery reject (SQLite and MySQL-family engines tolerate it).
# These tests check the SQL shape the kernel emits; the executed results on
# strict engines are in test-achilles-live-engines.R and
# test-vendor-db-integration.R.

# Split a SQL list on top-level commas.
.g16_split <- function(text) {
  chars <- strsplit(text, "", fixed = TRUE)[[1L]]
  depth <- 0L
  parts <- character(0)
  current <- character(0)
  for (ch in chars) {
    if (ch == "(") depth <- depth + 1L
    if (ch == ")") depth <- depth - 1L
    if (ch == "," && depth == 0L) {
      parts <- c(parts, trimws(paste(current, collapse = "")))
      current <- character(0)
    } else {
      current <- c(current, ch)
    }
  }
  c(parts, trimws(paste(current, collapse = "")))
}

# Run one person entry on the SQLite fixture, capturing the SQL it executes.
.g16_capture <- function(aid) {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  entry <- .omopAchillesPersonEntries(handle)[[
    paste0("dsomop:achilles.", aid)
  ]]
  captured <- character(0)
  original <- .executeQuery
  testthat::local_mocked_bindings(
    .executeQuery = function(handle, sql) {
      captured <<- c(captured, sql)
      original(handle, sql)
    },
    .package = "dsOMOP"
  )
  withr::local_options(list(nfilter.subset = 1, nfilter.tab = 1))
  out <- entry$compute$fn(handle, list(), list())
  sql <- captured[[length(captured)]]
  select <- sub("^SELECT (.*?) FROM .*$", "\\1", sql, perl = TRUE)
  group <- if (grepl(" GROUP BY ", sql, fixed = TRUE)) {
    .g16_split(sub("^.* GROUP BY (.*?)( ORDER BY .*)?$", "\\1", sql,
                   perl = TRUE))
  } else character(0)
  items <- .g16_split(select)
  expressions <- sub("\\s+AS\\s+[A-Za-z_][A-Za-z0-9_]*$", "", items,
                     perl = TRUE)
  aggregate <- grepl("^(COUNT|SUM|MIN|MAX|AVG)\\(", expressions)
  list(sql = sql, out = out, nonaggregate = expressions[!aggregate],
       group = group)
}

test_that("G-16: every selected non-aggregate expression is grouped", {
  names_expected <- list(
    "2" = "cj1.concept_name", "4" = "cj1.concept_name",
    "5" = "cj1.concept_name", "10" = "cj1.concept_name",
    "12" = c("cj1.concept_name", "cj2.concept_name")
  )
  for (aid in names(names_expected)) {
    shape <- .g16_capture(aid)
    expect_true(length(shape$nonaggregate) > 0L, info = aid)
    expect_true(all(shape$nonaggregate %in% shape$group),
                info = paste(aid, shape$sql))
    expect_true(all(names_expected[[aid]] %in% shape$group), info = aid)
    # Grouped by the exact expression, never by an output alias.
    expect_false(any(shape$group %in% c("gender_name", "race_name",
                                        "ethnicity_name")), info = aid)
    expect_true(all(grepl("^(p|cj[0-9]+)\\.", shape$group)), info = aid)
    expect_false(any(grepl("^(MIN|MAX)\\(", shape$nonaggregate)), info = aid)
  }
  # Controls: 1 is a scalar count, 3 groups only its own column.
  scalar <- .g16_capture("1")
  expect_length(scalar$nonaggregate, 0L)
  expect_length(scalar$group, 0L)
  expect_false(grepl("GROUP BY", scalar$sql, fixed = TRUE))
  by_year <- .g16_capture("3")
  expect_identical(by_year$nonaggregate, "p.year_of_birth")
  expect_identical(by_year$group, "p.year_of_birth")
})

test_that("G-16: grouped results match an independent aggregation", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  entries <- .omopAchillesPersonEntries(handle)
  withr::local_options(list(nfilter.subset = 1, nfilter.tab = 1))
  independent <- list(
    "2" = c("p.gender_concept_id", "c1.concept_name AS gender_name"),
    "4" = c("p.race_concept_id", "c1.concept_name AS race_name"),
    "5" = c("p.ethnicity_concept_id", "c1.concept_name AS ethnicity_name"),
    "10" = c("p.year_of_birth", "p.gender_concept_id",
             "c1.concept_name AS gender_name")
  )
  joins <- c("2" = "gender", "4" = "race", "5" = "ethnicity", "10" = "gender")
  canonical <- function(df) {
    df <- as.data.frame(df)
    df <- df[, sort(names(df)), drop = FALSE]
    for (col in names(df)) {
      df[[col]] <- if (grepl("_name$", col)) as.character(df[[col]]) else
        as.numeric(df[[col]])
    }
    df <- df[do.call(order, c(unname(as.list(df)),
                              list(na.last = TRUE, method = "radix"))), ,
             drop = FALSE]
    rownames(df) <- NULL
    df
  }
  for (aid in names(independent)) {
    got <- entries[[paste0("dsomop:achilles.", aid)]]$compute$fn(
      handle, list(), list()
    )
    cols <- independent[[aid]]
    grouped <- sub(" AS .*$", "", cols)
    want <- DBI::dbGetQuery(handle$conn, paste0(
      "SELECT ", paste(cols, collapse = ", "),
      ", COUNT(DISTINCT p.person_id) AS count_value FROM person p ",
      "LEFT JOIN concept c1 ON c1.concept_id = p.", joins[[aid]],
      "_concept_id GROUP BY ", paste(grouped, collapse = ", ")
    ))
    names(want) <- sub("^p\\.", "", names(want))
    expect_equal(canonical(got), canonical(want), info = aid)
  }
  got12 <- entries[["dsomop:achilles.12"]]$compute$fn(handle, list(), list())
  want12 <- DBI::dbGetQuery(handle$conn, paste0(
    "SELECT p.race_concept_id, p.ethnicity_concept_id, ",
    "r.concept_name AS race_name, e.concept_name AS ethnicity_name, ",
    "COUNT(DISTINCT p.person_id) AS count_value FROM person p ",
    "LEFT JOIN concept r ON r.concept_id = p.race_concept_id ",
    "LEFT JOIN concept e ON e.concept_id = p.ethnicity_concept_id ",
    "GROUP BY p.race_concept_id, p.ethnicity_concept_id, r.concept_name, ",
    "e.concept_name"
  ))
  expect_equal(canonical(got12), canonical(want12))
})
