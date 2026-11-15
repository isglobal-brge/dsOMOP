test_that("PhenotypeLibrary 304 matches its pinned upstream generated SQL", {
  # Both sides execute on the same private synthetic fixture. The reference is
  # the unmodified SQL bundled by upstream, not another implementation of our
  # compiler or a JSON self-round-trip. SqlRender only renders its parameters
  # and translates its dialect; no semantic edits are made to that SQL.
  skip_if_not_installed("SqlRender")
  expect_identical(as.character(utils::packageVersion("SqlRender")), "1.19.7")
  fixture <- test_path("..", "fixtures", "phenotype-library")
  manifest <- jsonlite::fromJSON(file.path(fixture, "manifest.json"), simplifyVector = FALSE)
  pin <- manifest$fixtures[[1L]]
  hash <- function(path) unclass(as.character(openssl::sha256(readBin(path, "raw", n = file.info(path)$size))))
  expect_identical(hash(file.path(fixture, "304.json")), pin$sha256)
  expect_identical(hash(file.path(fixture, "304.sql")), pin$upstream_sql$sha256)
  pg <- .ws4_pg_connect()
  skip_if(is.null(pg), "PostgreSQL is required for the pinned upstream SQL oracle")
  schema <- .ws4_namespace("audit_circe")
  DBI::dbExecute(pg, paste("CREATE SCHEMA", schema))
  source <- create_test_handle(n_persons = 40L)
  h <- source
  run <- NULL
  on.exit({
    if (!is.null(run)) .ws7_close(run)
    DBI::dbExecute(pg, paste("DROP SCHEMA", schema, "CASCADE"))
    DBI::dbDisconnect(pg)
    cleanup_handle(source)
  }, add = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM observation_period")
  DBI::dbWriteTable(h$conn, "observation_period", data.frame(
    observation_period_id = 1:40, person_id = 1:40,
    observation_period_start_date = "2020-01-01", observation_period_end_date = "2020-12-31",
    period_type_concept_id = 0L), append = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM condition_occurrence")
  events <- data.frame(condition_occurrence_id = 1:70,
    person_id = c(1:30, 1:30, 21:30),
    condition_concept_id = c(rep(377252L, 10), rep(377253L, 10), rep(377252L, 50)),
    condition_start_date = c(rep("2020-01-10", 10), rep("2020-01-12", 10),
      rep("2020-02-03", 10), rep("2020-03-01", 30), rep("2019-12-01", 10)))
  DBI::dbWriteTable(h$conn, "condition_occurrence", events, append = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM concept WHERE concept_id IN (377252, 377253)")
  DBI::dbWriteTable(h$conn, "concept", data.frame(concept_id = c(377252L, 377253L),
    concept_name = c("public root", "public descendant"), domain_id = "Condition",
    vocabulary_id = "SNOMED", concept_class_id = "Clinical Finding", standard_concept = "S",
    concept_code = c("001", "002"), valid_start_date = "1970-01-01",
    valid_end_date = "2099-12-31", invalid_reason = NA_character_), append = TRUE)
  DBI::dbExecute(h$conn, "DELETE FROM concept_ancestor WHERE ancestor_concept_id = 377252")
  DBI::dbWriteTable(h$conn, "concept_ancestor", data.frame(ancestor_concept_id = 377252L,
    descendant_concept_id = 377253L, min_levels_of_separation = 1L,
    max_levels_of_separation = 1L), append = TRUE)
  DBI::dbExecute(h$conn, paste("CREATE TABLE audit_upstream (cohort_definition_id INTEGER,",
    "subject_id INTEGER, cohort_start_date TEXT, cohort_end_date TEXT)"))
  DBI::dbExecute(h$conn, "CREATE TABLE cohort_censor_stats (cohort_definition_id INTEGER)")
  .ws4_copy_tables(source$conn, pg, schema)
  h <- .ws4_handle_on(pg, "postgresql", "postgresql", schema)
  .ws7_keys(list(h))
  .buildBlueprint(h)
  sql <- paste(readLines(file.path(fixture, "304.sql"), warn = FALSE), collapse = "\n")
  rendered <- SqlRender::render(sql, vocabulary_database_schema = schema,
    cdm_database_schema = schema, target_database_schema = schema,
    target_cohort_table = "audit_upstream", target_cohort_id = 304,
    results_database_schema = schema)
  translated <- SqlRender::translate(rendered, targetDialect = "postgresql")
  for (statement in SqlRender::splitSql(translated)) DBI::dbExecute(h$conn, statement)
  reference <- DBI::dbGetQuery(h$conn, paste("SELECT subject_id, cohort_start_date, cohort_end_date",
    "FROM", paste0(schema, ".audit_upstream"), "ORDER BY subject_id"))
  for (nm in c("cohort_start_date", "cohort_end_date")) reference[[nm]] <- as.character(reference[[nm]])
  expect_equal(reference$subject_id, 1:30)
  expect_equal(as.character(reference$cohort_start_date),
    c(rep("2020-01-10", 10), rep("2020-01-12", 10), rep("2020-02-03", 10)))
  expect_true(all(as.character(reference$cohort_end_date) == "2020-12-31"))
  pop <- dsOMOPClient::recipe_import_circe(file.path(fixture, "304.json"), id = "p")
  recipe <- dsOMOPClient::omop_recipe(populations = pop,
    outputs = dsOMOPClient::omop_output("o", rows = "p", format = "wide"))
  run <- .ws7_ctx(h, dsOMOPClient::recipe_to_plan(recipe, version = 2))
  actual <- .fetchFragment(run$cctx, .compilePopulationByName(run$cctx, "p")$roster, "subject_id")
  actual <- actual[, names(reference)]
  for (nm in c("cohort_start_date", "cohort_end_date")) actual[[nm]] <- as.character(actual[[nm]])
  expect_equal(actual, reference, ignore_attr = TRUE)
})
