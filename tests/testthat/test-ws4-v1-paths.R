# WS-4 defects on the v1 executors (design-2.10.3 section 8): F-1 (the
# table-agnostic concept_id sentinel of a row concept-set filter) and F-25
# (exact birth components on assignment paths), on the reach fixture.

test_that("F-1: a concept_id row-filter sentinel resolves to the table's domain concept and narrows", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  sentinel <- list(var = "concept_id", op = "in", value = list(201820))
  sql <- .compileSelect(h, "condition_occurrence", columns = c("person_id", "condition_concept_id"),
                        filters = sentinel)
  expect_match(sql, "t.condition_concept_id IN (201820)", fixed = TRUE)
  rows <- .executeQuery(h, sql)
  all_rows <- .executeQuery(h, "SELECT COUNT(*) AS n FROM condition_occurrence")$n
  expect_gt(nrow(rows), 0L)
  expect_lt(nrow(rows), all_rows)
  expect_true(all(rows$condition_concept_id == 201820L))
  # the same leaf over another table targets that table's domain column
  expect_match(.compileSelect(h, "drug_exposure", columns = "person_id",
                              filters = list(var = "concept_id", op = "in", value = list(1503297))),
               "t.drug_concept_id IN (1503297)", fixed = TRUE)
  # nested and negated sentinels resolve too; real columns are untouched
  tree <- .resolveFilterSentinels(
    list(and = list(list(not = sentinel), list(var = "condition_type_concept_id", op = "==", value = 1),
                    list(var = "start_date", op = "between", value = list("2020-01-01", "2020-12-31")))),
    date_col = "condition_start_date", concept_col = "condition_concept_id")
  expect_identical(tree$and[[1]]$not$var, "condition_concept_id")
  expect_identical(tree$and[[2]]$var, "condition_type_concept_id")
  expect_identical(tree$and[[3]]$var, "condition_start_date")
  # a v1 plan carrying the sentinel executes and narrows
  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    outputs = list(dx = list(type = "event_level", table = "condition_occurrence",
                             columns = c("person_id", "condition_concept_id"),
                             representation = list(format = "long"),
                             filters = list(custom = sentinel))),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE))
  withr::with_options(list(nfilter.subset = 3), {
    res <- .planExecute(h, plan, list(dx = "dx_df"))
  })
  expect_gt(nrow(res$dx), 0L)
  expect_true(all(res$dx$condition_concept_id == 201820L))
})

test_that("F-25: assigned wide outputs return requested birth components; class 1 and aggregates stay blocked", {
  h <- .ws4_reach_handle()
  on.exit(DBI::dbDisconnect(h$conn), add = TRUE)
  DBI::dbExecute(h$conn, "ALTER TABLE person ADD COLUMN birth_datetime TEXT")
  DBI::dbExecute(h$conn, paste0("UPDATE person SET birth_datetime = year_of_birth || '-06-15 08:30:00' ",
                                "WHERE year_of_birth IS NOT NULL"))
  h$blueprint <- NULL
  plan <- function(columns) list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    outputs = list(p = list(type = "person_level", tables = list(person = columns))),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE))
  cols <- c("person_id", "year_of_birth", "month_of_birth", "day_of_birth", "birth_datetime")
  withr::with_options(list(nfilter.subset = 3), {
    res <- .planExecute(h, plan(cols), list(p = "p_df"))
    expect_true(all(c("year_of_birth", "month_of_birth", "day_of_birth") %in% names(res$p)))
    expect_true(any(!is.na(res$p$year_of_birth)))
    # birth_datetime is an exact date: the unchanged absolute-date policy
    # decides it (removed under the default "remove" handling) ...
    expect_false("birth_datetime" %in% names(res$p))
  })
  # ... and the controller's absolute-date policy releases it like any date
  withr::with_options(list(nfilter.subset = 3, dsomop.default_date_handling = "absolute",
                           dsomop.allow_absolute_dates = TRUE), {
    res <- .planExecute(h, plan(cols), list(p = "p_df"))
    expect_true(all(c("year_of_birth", "month_of_birth", "day_of_birth", "birth_datetime") %in%
                      names(res$p)))
  })
  expect_error(.compileSelect(h, "person", columns = c("person_id", "person_source_value")),
               "blocked")
  # baseline (no date handling of its own) returns every requested birth
  # component, birth_datetime included
  th <- create_test_handle()
  on.exit(cleanup_handle(th), add = TRUE)
  DBI::dbExecute(th$conn, "ALTER TABLE person ADD COLUMN birth_datetime TEXT")
  DBI::dbExecute(th$conn, paste0("UPDATE person SET birth_datetime = year_of_birth || '-06-15 08:30:00' ",
                                 "WHERE year_of_birth IS NOT NULL"))
  th$blueprint <- NULL
  bplan <- list(cohort = list(type = "cohort_table", cohort_definition_id = 1L),
                outputs = list(b = list(type = "baseline",
                                        columns = c("year_of_birth", "month_of_birth", "birth_datetime"),
                                        derived = "age_at_index")),
                options = list(translate_concepts = FALSE, block_sensitive = TRUE))
  withr::with_options(list(nfilter.subset = 3), {
    res <- .planExecute(th, bplan, list(b = "b_df"))
  })
  expect_true(all(c("year_of_birth", "month_of_birth", "birth_datetime", "age_group") %in% names(res$b)))
  expect_true(any(grepl("-06-15", res$b$birth_datetime)))
  expect_identical(.ql_classify("SELECT year_of_birth, COUNT(*) AS n FROM person GROUP BY year_of_birth",
                                "aggregate")$class, "BLOCKED")
  expect_error(.profileColumnStats(h, "person", "year_of_birth"), "blocked")
})

test_that("the CDM column list is derived from the vendored OHDSI field metadata and vendored in the client", {
  path <- testthat::test_path("..", "..", "inst", "ohdsi", "cdm_columns.csv")
  shipped <- utils::read.csv(path, colClasses = "character", stringsAsFactors = FALSE)
  derived <- list()
  for (v in c("5.3", "5.4")) {
    f <- testthat::test_path("..", "..", "inst", "ohdsi", paste0("OMOP_CDMv", v, "_Field_Level.csv"))
    meta <- utils::read.csv(f, stringsAsFactors = FALSE, fileEncoding = "latin1")
    key <- paste(tolower(trimws(meta$cdmTableName)), tolower(trimws(meta$cdmFieldName)), sep = "\t")
    for (k in key) derived[[k]] <- c(derived[[k]], v)
  }
  keys <- sort(names(derived), method = "radix")
  expected <- data.frame(
    table_name = sub("\t.*$", "", keys), column_name = sub("^.*\t", "", keys),
    cdm_versions = vapply(keys, function(k) paste(sort(unique(derived[[k]])), collapse = ";"), ""),
    stringsAsFactors = FALSE, row.names = NULL)
  expect_identical(shipped, expected)
  tp <- testthat::test_path()
  client_root <- Find(dir.exists, file.path(tp, "..", "..", "..", "dsOMOPClient"))
  skip_if(is.null(client_root), "sibling dsOMOPClient source tree not reachable")
  sha <- function(f) paste(as.character(openssl::sha256(file(f))), collapse = "")
  expect_identical(sha(file.path(client_root, "inst", "ohdsi", "cdm_columns.csv")), sha(path))
})

test_that("F-26: an assigned object that qualifies keeps a one-person age band", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  cohort <- .resolveCohortTable(handle, 1L)
  ages <- .executeQuery(handle, paste0(
    "SELECT c.subject_id, c.cohort_start_date, p.year_of_birth FROM ", cohort,
    " c JOIN person p ON p.person_id = c.subject_id"))
  ages$age <- as.integer(substr(as.character(ages$cohort_start_date), 1L, 4L)) - ages$year_of_birth
  lone <- as.integer(names(which(table(ages$age) == 1L)))
  skip_if(!length(lone), "the test cohort has no unique age")
  a <- lone[[1L]]
  withr::with_options(list(nfilter.subset = 3L, nfilter.tab = 100L), {
    out <- .extractBaseline(handle, cohort, columns = "gender_concept_id",
                            derived = "age_at_index", age_breaks = c(0L, a, a + 1L),
                            translate_concepts = FALSE)
    band <- paste0(a, "-", a)
    expect_identical(sum(out$age_group == band, na.rm = TRUE), 1L)
    published <- .publishStandalone(.testPseudonymize(out), already_tokenized = TRUE)
    expect_identical(sum(published$age_group == band, na.rm = TRUE), 1L)
  })
  # aggregate age grouping keeps its own coarsening rule
  withr::with_options(list(nfilter.tab = 100L), {
    expect_error(.computeAgeGroups(2000, 2023, age_breaks = c(0, 18, 19, 40)), "coarsening")
  })
})
