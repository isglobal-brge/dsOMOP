test_that("plan validate returns structure", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      baseline = list(type = "person_level", tables = list(person = c("person_id", "gender_concept_id")))
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planPreview(handle, plan)
    expect_true(is.list(result))
  })
})

test_that("plan execute creates output data frames", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      conditions = list(
        type = "event_level",
        table = "condition_occurrence",
        columns = NULL,
        concept_set = c(201820),
        representation = list(format = "long"),
        filters = list(concept_set = list(ids = c(201820)))
      )
    ),
    options = list(
      translate_concepts = FALSE,
      block_sensitive = TRUE,
      min_persons = NULL
    )
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan, list(conditions = "cond_df"))
    expect_true(is.list(result))
    expect_true("conditions" %in% names(result))
    expect_true(is.data.frame(result$conditions))
    expect_true(nrow(result$conditions) > 0)
    expect_true(all(result$conditions$condition_concept_id == 201820))
  })
})

test_that("plan extraction scopes person and event SQL by cohort relation only", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  original_extract <- .extractTable
  calls <- list()
  local_mocked_bindings(
    .extractTable = function(handle, table, ..., person_ids = NULL,
                             cohort_table = NULL) {
      calls[[length(calls) + 1L]] <<- list(
        table = table, person_ids = person_ids, cohort_table = cohort_table
      )
      do.call(original_extract, c(
        list(handle = handle, table = table, person_ids = person_ids,
             cohort_table = cohort_table),
        list(...)
      ))
    },
    .package = "dsOMOP"
  )
  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    outputs = list(
      people = list(
        type = "person_level",
        tables = list(person = c("person_id", "gender_concept_id"))
      ),
      events = list(
        type = "event_level", table = "condition_occurrence",
        concept_set = 201820L,
        representation = list(format = "long"), filters = list()
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )

  withr::with_options(list(nfilter.subset = 3), {
    result <- .planExecute(handle, plan,
                           list(people = "people", events = "events"))
    expect_true(nrow(result$people) > 0L)
    expect_true(nrow(result$events) > 0L)
  })
  scoped <- calls[vapply(calls, function(x) {
    x$table %in% c("person", "condition_occurrence")
  }, logical(1))]
  expect_length(scoped, 2L)
  expect_true(all(vapply(scoped, function(x) is.null(x$person_ids), logical(1))))
  expect_true(all(vapply(scoped, function(x) {
    is.character(x$cohort_table) && length(x$cohort_table) == 1L
  }, logical(1))))
})

test_that("plan work-name collision preserves the prior temp and uses a new one", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  prior <- .createTempTable(
    handle, "dsomop_plan_cohort_scoped", "SELECT 777 AS sentinel"
  )
  baseline <- handle$temp_tables
  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    outputs = list(events = list(
      type = "event_level", table = "condition_occurrence",
      concept_set = 201820L,
      representation = list(format = "long"), filters = list()
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )

  result <- withr::with_options(
    list(nfilter.subset = 3),
    .planExecute(handle, plan, list(events = "events"))
  )

  expect_true(nrow(result$events) > 0L)
  expect_identical(handle$temp_tables, baseline)
  expect_true(DBI::dbExistsTable(handle$conn, prior))
  expect_identical(
    DBI::dbGetQuery(handle$conn, paste0("SELECT sentinel FROM ", prior))$sentinel,
    777L
  )
})

test_that("internal cohort materialization never replaces an owned homonym", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  bp <- .buildBlueprint(handle)
  base <- "dsomop_plan_pop_collision"
  .createTempTable(
    handle, base,
    paste(
      "SELECT 999 AS subject_id, '2000-01-01' AS cohort_start_date,",
      "'2000-01-01' AS cohort_end_date"
    )
  )

  created <- .materializeCohortFromIds(handle, bp, 1:3, base)

  expect_false(identical(created, base))
  expect_true(all(vapply(c(base, created), function(name) {
    DBI::dbExistsTable(handle$conn, name)
  }, logical(1))))
  expect_identical(
    DBI::dbGetQuery(handle$conn, paste0(
      "SELECT subject_id FROM ", base
    ))$subject_id,
    999L
  )
  expect_setequal(
    DBI::dbGetQuery(handle$conn, paste0(
      "SELECT DISTINCT subject_id FROM ", created
    ))$subject_id,
    1:3
  )
})

read_staged_output <- function(descriptor) {
  if (identical(descriptor$metadata$format, "parquet")) {
    as.data.frame(arrow::read_parquet(descriptor$metadata$file))
  } else {
    utils::read.csv(descriptor$metadata$file, stringsAsFactors = FALSE)
  }
}

test_pseudonym_contract <- function(key) {
  .testPublicPseudonymization(key, epoch = 1L)
}

test_plan_output_contract <- function() {
  list(
    type = "event_level",
    representation = list(format = "long")
  )
}

test_plan_contract_plan <- function(output_name = "analysis",
                                    output = test_plan_output_contract()) {
  outputs <- list(output)
  names(outputs) <- output_name
  list(outputs = outputs)
}

test_plan_semantic_contract <- function(output_name = "analysis") {
  .stagedSemanticContract(
    test_plan_contract_plan(output_name), output_name
  )
}

test_plan_bundle_contract <- function(output_name, token) {
  .stagedBundleContract(
    test_plan_contract_plan(output_name), output_name, token
  )
}

test_that("staged semantics bind longitudinal query shape without irrelevant age grids", {
  first <- list(
    type = "intervals_long",
    tables = c("condition_occurrence"),
    concept_filter = list(condition_occurrence = c(10L, 20L)),
    source_filters = list(condition_occurrence = list(
      var = "condition_type_concept_id", op = "in", value = c(1L, 2L)
    )),
    window = list(start = -30L, end = 90L),
    interval_match = "overlaps",
    event_select = "nearest",
    select_n = 1L,
    select_by = "episode_source",
    anchor = 0L
  )
  changed <- first
  changed$window$end <- 91L
  first_contract <- .stagedSemanticContract(
    test_plan_contract_plan("intervals", first), "intervals"
  )
  changed_contract <- .stagedSemanticContract(
    test_plan_contract_plan("intervals", changed), "intervals"
  )

  expect_null(first_contract$age_breaks)
  expect_false(identical(
    first_contract$query_semantics_sha256,
    changed_contract$query_semantics_sha256
  ))
  expect_identical(
    .validateStagedSemanticContract(first_contract), first_contract
  )
})

set_test_person_key <- function(handle,
                                key = .testPseudonymKey("plan-resource")) {
  identity <- "test://resource-scoped/staging"
  rid <- substr(as.character(openssl::sha256(charToRaw(identity))), 1L, 32L)
  environment <- paste(format(key), collapse = "")
  names(environment) <- paste0("DSOMOP_PSEUDONYM_KEY_", rid)
  environment <- c(
    environment,
    DSOMOP_PSEUDONYM_PROVIDER = "scoped",
    DSOMOP_PSEUDONYM_EPOCH = "1",
    DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false",
    DSOMOP_PSEUDONYM_ROOT = "",
    DSOMOP_PSEUDONYM_KEY = ""
  )
  withr::local_envvar(environment, .local_envir = parent.frame())
  withr::local_options(list(
    dsomop.pseudonym_provider = NULL,
    dsomop.pseudonym_epoch = NULL,
    dsomop.pseudonym_require_existing = NULL,
    dsomop.pseudonym_root = NULL,
    dsomop.pseudonym_key = NULL
  ), .local_envir = parent.frame())
  handle$person_key <- key
  handle$person_key_identity <- identity
  handle$person_key_id <- .personKeyId(key)
  handle$person_key_provider <- "scoped"
  handle$person_key_epoch <- 1L
  handle$person_key_require_existing <- FALSE
  handle$person_key_contract_version <- 1L
  invisible(key)
}

test_that("empty streamed queries create a readable staged file with schema", {
  handle <- create_test_handle()
  set_test_person_key(handle)
  on.exit(cleanup_handle(handle), add = TRUE)
  token <- .generateStagingToken()
  staging_dir <- .createStagingDir(token)
  on.exit(unlink(staging_dir, recursive = TRUE), add = TRUE)
  out <- file.path(staging_dir, "empty.parquet")

  info <- .executeQueryToParquet(
    handle$conn,
    "SELECT person_id, gender_concept_id FROM person WHERE 1 = 0",
    out,
    chunk_fn = function(x) .pseudonymizeIdentifiers(
      x, handle$person_key, test_pseudonym_contract(handle$person_key)
    )
  )

  expect_true(file.exists(info$file))
  expect_equal(info$n_rows, 0L)
  expect_setequal(info$columns, c("person_id", "gender_concept_id"))
  landed <- if (identical(info$format, "parquet")) {
    as.data.frame(arrow::read_parquet(info$file))
  } else {
    utils::read.csv(info$file, stringsAsFactors = FALSE)
  }
  expect_equal(nrow(landed), 0L)
  expect_setequal(names(landed), info$columns)
  expect_setequal(names(info$column_types), info$columns)
})

test_that("empty staged date handling preserves the non-empty schema contract", {
  handle <- create_test_handle()
  set_test_person_key(handle)
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_empty_dates_")
  withr::local_options(list(dsstaging.base_dir = base))
  staging_dir <- .createStagingDir(.generateStagingToken())
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  transform <- function(x) {
    x <- .convertTypes(x)
    x <- .applyDateHandling(x, list(mode = "remove"))
    .pseudonymizeIdentifiers(
      x, handle$person_key, test_pseudonym_contract(handle$person_key)
    )
  }
  info <- .executeQueryToParquet(
    handle$conn,
    paste(
      "SELECT person_id, condition_start_date, condition_concept_id",
      "FROM condition_occurrence WHERE 1 = 0"
    ),
    file.path(staging_dir, "empty_dates.parquet"),
    chunk_fn = transform
  )

  expect_equal(info$n_rows, 0L)
  expect_setequal(info$columns, c("person_id", "condition_concept_id"))
  expect_false("condition_start_date" %in% info$columns)
  descriptor <- .buildStagedDescriptor(
    "empty_dates", info, basename(staging_dir),
    pseudonymization = test_pseudonym_contract(handle$person_key),
    semantic_contract = test_plan_semantic_contract(),
    bundle_contract = test_plan_bundle_contract(
      "empty_dates", basename(staging_dir)
    )
  )
  if (identical(info$format, "parquet")) {
    expect_identical(descriptor$metadata$layout, "file")
    expect_identical(
      omopStagedDatasetPath(descriptor),
      normalizePath(info$file, winslash = "/", mustWork = TRUE)
    )
  }
  expect_setequal(names(read_staged_output(descriptor)), info$columns)
})

test_that("CSV fallback streams a readable file with the same descriptor schema", {
  handle <- create_test_handle()
  set_test_person_key(handle)
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_csv_")
  withr::local_options(list(
    dsstaging.base_dir = base,
    dsomop.nfilter.band = 7
  ))
  staging_dir <- .createStagingDir(.generateStagingToken())
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(
    .arrowAvailable = function() FALSE,
    .package = "dsOMOP"
  )

  info <- .executeQueryToParquet(
    handle$conn,
    "SELECT person_id, gender_concept_id FROM person ORDER BY person_id",
    file.path(staging_dir, "fallback.parquet"),
    chunk_size = 2L,
    chunk_fn = function(x) .pseudonymizeIdentifiers(
      x, handle$person_key, test_pseudonym_contract(handle$person_key)
    )
  )
  descriptor <- .buildStagedDescriptor(
    "fallback", info, basename(staging_dir),
    pseudonymization = test_pseudonym_contract(handle$person_key),
    semantic_contract = test_plan_semantic_contract(),
    bundle_contract = test_plan_bundle_contract(
      "fallback", basename(staging_dir)
    )
  )
  landed <- read_staged_output(descriptor)

  expect_identical(info$format, "csv")
  expect_match(info$file, "\\.csv$")
  expect_equal(nrow(landed), info$n_rows)
  expect_identical(descriptor$metadata$n_rows, 14)
  expect_setequal(names(landed), info$columns)
  expect_identical(descriptor$metadata$column_types, info$column_types)

  materialized <- .stageDataFrame(
    data.frame(person_id = 1:3, value = c(1.5, 2.5, 3.5)),
    "fallback_df", staging_dir, basename(staging_dir), handle$person_key,
    pseudonymization = test_pseudonym_contract(handle$person_key),
    semantic_contract = test_plan_semantic_contract(),
    bundle_contract = test_plan_bundle_contract(
      "fallback_df", basename(staging_dir)
    )
  )
  expect_identical(materialized$metadata$format, "csv")
  expect_equal(nrow(read_staged_output(materialized)), 3L)
  expect_setequal(names(materialized$metadata$column_types),
                  materialized$metadata$columns)
})

test_that("streaming fetches bounded chunks and publishes their schema", {
  handle <- create_test_handle()
  set_test_person_key(handle)
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_chunks_")
  withr::local_options(list(dsstaging.base_dir = base))
  staging_dir <- .createStagingDir(.generateStagingToken())
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  fetched <- integer(0)

  info <- .executeQueryToParquet(
    handle$conn,
    "SELECT person_id, gender_concept_id FROM person ORDER BY person_id",
    file.path(staging_dir, "chunked.parquet"),
    chunk_size = 2L,
    chunk_fn = function(x) {
      if (nrow(x) > 0L) fetched <<- c(fetched, nrow(x))
      .pseudonymizeIdentifiers(
        x, handle$person_key, test_pseudonym_contract(handle$person_key)
      )
    }
  )

  expect_gt(length(fetched), 1L)
  expect_lte(max(fetched), 2L)
  expect_equal(sum(fetched), info$n_rows)
  expect_setequal(names(info$column_types), info$columns)
  if (identical(info$format, "parquet")) {
    expect_identical(info$layout, "file")
    expect_true(file.exists(info$file))
    expect_false(dir.exists(info$file))
    reader <- arrow::ParquetFileReader$create(info$file)
    expect_gt(reader$num_row_groups, 1L)
  }
})

test_that("streaming takes a non-empty schema from the first fetched chunk", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_driver_schema_")
  withr::local_options(list(dsstaging.base_dir = base))
  staging_dir <- .createStagingDir(.generateStagingToken())
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  # Reproduce the DBI shape used by RMariaDB for BIGINT expressions: an empty
  # fetch is numeric, while fetched values carry a more specific class/type.
  testthat::local_mocked_bindings(
    .coerce_integer64 = function(x, stable = FALSE) {
      if (nrow(x) > 0L) x$person_id <- as.integer(x$person_id)
      x
    },
    .package = "dsOMOP"
  )
  info <- .executeQueryToParquet(
    handle$conn,
    "SELECT CAST(person_id AS REAL) AS person_id FROM person ORDER BY person_id",
    file.path(staging_dir, "driver_schema.parquet"),
    chunk_size = 2L
  )

  expect_equal(
    info$n_rows,
    nrow(DBI::dbGetQuery(handle$conn, "SELECT person_id FROM person"))
  )
  expect_identical(unname(info$column_types), "integer|integer")
})

test_that("streaming keeps BIGINT physical types stable across bounded chunks", {
  skip_if_not_installed("bit64")
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_bigint_chunks_")
  withr::local_options(list(dsstaging.base_dir = base))
  staging_dir <- .createStagingDir(.generateStagingToken())
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(
    .arrowAvailable = function() FALSE,
    .package = "dsOMOP"
  )

  info <- .executeQueryToParquet(
    handle$conn,
    paste0(
      "SELECT CAST(1 AS INTEGER) AS big_id ",
      "UNION ALL SELECT CAST(3000000000 AS INTEGER) AS big_id"
    ),
    file.path(staging_dir, "bigint_chunks.parquet"),
    chunk_size = 1L
  )

  expect_equal(info$n_rows, 2L)
  expect_identical(unname(info$column_types), "character|character")
  expect_match(paste(readLines(info$file, warn = FALSE), collapse = "\n"),
               "3000000000")
})

test_that("streaming fails closed when a chunk changes column types", {
  handle <- create_test_handle()
  set_test_person_key(handle)
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_schema_")
  withr::local_options(list(dsstaging.base_dir = base))
  staging_dir <- .createStagingDir(.generateStagingToken())
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  output <- file.path(staging_dir, "unstable.parquet")
  seen <- 0L

  expect_error(.executeQueryToParquet(
    handle$conn,
    "SELECT person_id, year_of_birth FROM person ORDER BY person_id",
    output,
    chunk_size = 2L,
    chunk_fn = function(x) {
      x <- .pseudonymizeIdentifiers(
        x, handle$person_key, test_pseudonym_contract(handle$person_key)
      )
      if (nrow(x) > 0L) {
        seen <<- seen + 1L
        if (seen > 1L) x$year_of_birth <- as.character(x$year_of_birth)
      }
      x
    }
  ), "stable names and types")
  expect_false(file.exists(output))
})

test_that("streaming cannot escape or overwrite its reserved directory", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_paths_")
  withr::local_options(list(dsstaging.base_dir = base))
  staging_dir <- .createStagingDir(.generateStagingToken())
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  sql <- "SELECT person_id FROM person WHERE 1 = 0"

  expect_error(
    .executeQueryToParquet(handle$conn, sql,
      file.path(base, "escape.parquet")),
    "reserved staging directory"
  )

  existing <- file.path(staging_dir, "existing.parquet")
  writeLines("sentinel", existing)
  expect_error(
    .executeQueryToParquet(handle$conn, sql, existing),
    "already exists"
  )
  expect_identical(readLines(existing), "sentinel")

  if (.Platform$OS.type != "windows") {
    external <- tempfile("dsstaging_output_target_")
    writeLines("outside", external)
    on.exit(unlink(external), add = TRUE)
    linked_output <- file.path(staging_dir, "linked.parquet")
    if (isTRUE(file.symlink(external, linked_output))) {
      expect_error(
        .executeQueryToParquet(handle$conn, sql, linked_output),
        "already exists"
      )
      expect_identical(readLines(external), "outside")
    }
  }
})

test_that("streaming quotas remove incomplete outputs", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_quota_")
  withr::local_options(list(dsstaging.base_dir = base))
  staging_dir <- .createStagingDir(.generateStagingToken())
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  row_output <- file.path(staging_dir, "rows.parquet")
  withr::local_options(list(dsomop.max_staged_rows = 1L))
  expect_error(.executeQueryToParquet(
    handle$conn, "SELECT person_id FROM person", row_output, chunk_size = 2L
  ), "row quota")
  expect_false(file.exists(row_output))

  byte_output <- file.path(staging_dir, "bytes.parquet")
  withr::local_options(list(
    dsomop.max_staged_rows = 100L,
    dsomop.max_staged_bytes = 1
  ))
  expect_error(.executeQueryToParquet(
    handle$conn, "SELECT person_id FROM person", byte_output, chunk_size = 2L
  ), "disk quota")
  expect_false(file.exists(byte_output))
})

test_that("stream setup errors do not strand pending output", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_query_error_")
  withr::local_options(list(dsstaging.base_dir = base))
  staging_dir <- .createStagingDir(.generateStagingToken())
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  files_before <- list.files(staging_dir, all.files = TRUE, no.. = TRUE)
  output <- file.path(staging_dir, "invalid.parquet")

  expect_error(
    .executeQueryToParquet(handle$conn, "SELECT no_such_column FROM person",
                           output),
    "no such column"
  )
  files_after <- list.files(staging_dir, all.files = TRUE, no.. = TRUE)
  expect_identical(files_after, files_before)
  expect_false(file.exists(output))
})

test_that("failed Parquet publication removes pending output", {
  skip_if_not(.arrowAvailable())
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_finalize_error_")
  withr::local_options(list(dsstaging.base_dir = base))
  staging_dir <- .createStagingDir(.generateStagingToken())
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  files_before <- list.files(staging_dir, all.files = TRUE, no.. = TRUE)
  output <- file.path(staging_dir, "finalize.parquet")
  testthat::local_mocked_bindings(
    .renameStagingFile = function(from, to) FALSE,
    .package = "dsOMOP"
  )

  expect_error(.executeQueryToParquet(
    handle$conn, "SELECT person_id FROM person ORDER BY person_id",
    output, chunk_size = 2L
  ), "Could not atomically publish")
  files_after <- list.files(staging_dir, all.files = TRUE, no.. = TRUE)
  expect_identical(files_after, files_before)
  expect_false(file.exists(output))
})

test_that("the byte quota applies to the whole staged plan directory", {
  base <- tempfile("dsstaging_total_quota_")
  withr::local_options(list(dsstaging.base_dir = base))
  token <- .generateStagingToken()
  staging_dir <- .createStagingDir(token)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  key <- .testPseudonymKey("quota-resource")
  first <- .stageDataFrame(
    data.frame(person_id = 1:3, value = 1:3),
    "first", staging_dir, token, key,
    pseudonymization = test_pseudonym_contract(key),
    semantic_contract = test_plan_semantic_contract(),
    bundle_contract = test_plan_bundle_contract("first", token)
  )
  used <- .stagingDirectoryBytes(staging_dir)
  withr::local_options(list(dsomop.max_staged_bytes = used + 1))

  expect_error(.stageDataFrame(
    data.frame(person_id = 4:6, value = 4:6),
    "second", staging_dir, token, key,
    pseudonymization = test_pseudonym_contract(key),
    semantic_contract = test_plan_semantic_contract(),
    bundle_contract = test_plan_bundle_contract("second", token)
  ), "disk quota")
  expect_true(file.exists(first$metadata$file))
  expect_false(any(grepl("second", list.files(staging_dir))))
})

test_that("failed staged plans remove every partial output", {
  handle <- create_test_handle()
  set_test_person_key(handle)
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_partial_")
  withr::local_options(list(
    dsstaging.base_dir = base,
    nfilter.subset = 3,
    dsomop.query_strict = TRUE
  ))
  plan <- list(outputs = list(
    first = list(
      type = "event_level", table = "condition_occurrence",
      representation = list(format = "long"), filters = list()
    ),
    broken = list(type = "not_a_real_output")
  ))

  expect_error(
    .planExecute(handle, plan, list(), output_mode = "staged"),
    "Unsupported output type"
  )
  expect_length(list.dirs(base, recursive = FALSE), 0L)
  expect_length(handle$staging_dirs, 0L)
})

test_that("staged plan count and per-handle directory caps fail closed", {
  handle <- create_test_handle()
  set_test_person_key(handle)
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_caps_")
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  withr::local_options(list(
    dsstaging.base_dir = base,
    dsomop.max_staged_outputs = 1L
  ))
  one <- list(
    type = "event_level", table = "condition_occurrence",
    representation = list(format = "long"), filters = list()
  )
  expect_error(
    .planExecute(handle, list(outputs = list(a = one, b = one)), list(),
                 output_mode = "staged"),
    "caps"
  )
  expect_false(dir.exists(base))

  withr::local_options(list(
    dsomop.max_staged_outputs = 10L,
    dsomop.max_staging_dirs_per_handle = 1L
  ))
  tracked <- .createStagingDir(.generateStagingToken())
  handle$staging_dirs <- tracked
  expect_error(
    .planExecute(handle, list(outputs = list(a = one)), list(),
                 output_mode = "staged"),
    "directory cap"
  )
})

test_that("staged preflight rejects and retains an unsafe tracked path", {
  skip_on_os("windows")
  handle <- create_test_handle()
  set_test_person_key(handle)
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_preflight_")
  external <- tempfile("dsstaging_preflight_external_")
  withr::local_options(list(dsstaging.base_dir = base))
  dir.create(base, mode = "0700")
  Sys.chmod(base, mode = "0700")
  dir.create(external)
  marker <- file.path(external, "keep")
  writeLines("keep", marker)
  link <- file.path(base, .generateStagingToken())
  on.exit(unlink(c(link, base, external), recursive = TRUE), add = TRUE)
  if (!isTRUE(file.symlink(external, link))) skip("Cannot create symlinks")
  handle$staging_dirs <- link
  output <- list(
    type = "event_level", table = "condition_occurrence",
    representation = list(format = "long"), filters = list()
  )

  expect_error(
    .planExecute(handle, list(outputs = list(a = output)), list(),
                 output_mode = "staged"),
    "unsafe or invalid owned path"
  )
  expect_identical(handle$staging_dirs, link)
  expect_true(.isSymbolicLink(link))
  expect_true(file.exists(marker))
})

test_that("event plans do not consume temp capacity for unused concept tables", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  .buildBlueprint(handle)
  withr::local_options(list(
    dsomop.max_temp_tables_per_handle = 1L,
    nfilter.subset = 3
  ))
  existing <- .createTempTable(
    handle, "existing_temp_capacity", "SELECT 1 AS value"
  )
  concepts <- unique(c(201820L, seq_len(50L)))
  plan <- list(
    outputs = list(events = list(
      type = "event_level", table = "condition_occurrence",
      concept_set = concepts,
      representation = list(format = "long"), filters = list()
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )

  result <- .planExecute(handle, plan, list(events = "events"))
  expect_true(nrow(result$events) > 0L)
  expect_identical(handle$temp_tables, existing)
})

test_that("all output modes enforce the server plan-output cap", {
  withr::local_options(list(dsomop.max_plan_outputs = 1L))
  output <- list(type = "event_level", table = "condition_occurrence")
  expect_error(
    .planExecute(NULL, list(outputs = list(a = output, b = output)), list()),
    "max_plan_outputs"
  )
})

test_that("staging tokens are high-entropy and directories are exclusive", {
  token <- .generateStagingToken()
  expect_match(token, "^stg_[0-9a-f]{32}$")
  expect_equal(length(unique(vapply(seq_len(64), function(i) {
    .generateStagingToken()
  }, character(1)))), 64L)

  dir <- .createStagingDir(token)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  expect_true(dir.exists(dir))
  expect_error(.createStagingDir(token), "exclusive staging directory")
  expect_error(.createStagingDir("../escape"), "Invalid staging token")
})

test_that("staging rejects symlink bases and preserves private permissions", {
  skip_on_os("windows")
  target <- tempfile("dsstaging_target_")
  link <- tempfile("dsstaging_link_")
  dir.create(target, mode = "0700")
  on.exit(unlink(c(link, target), recursive = TRUE), add = TRUE)
  if (!isTRUE(file.symlink(target, link))) skip("Cannot create symlinks")
  withr::local_options(list(dsstaging.base_dir = link))
  expect_error(.stagingBaseDir(), "must not be a symbolic link")

  permissive <- tempfile("dsstaging_permissive_")
  dir.create(permissive, mode = "0770")
  Sys.chmod(permissive, mode = "0770")
  on.exit(unlink(permissive, recursive = TRUE), add = TRUE)
  withr::local_options(list(dsstaging.base_dir = permissive))
  expect_error(.stagingBaseDir(), "owner-only directory")

  base <- tempfile("dsstaging_modes_")
  withr::local_options(list(dsstaging.base_dir = base))
  token <- .generateStagingToken()
  staging_dir <- .createStagingDir(token)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  descriptor <- .stageDataFrame(
    data.frame(person_id = 1L, value = 2), "private", staging_dir,
    token, .testPseudonymKey("private-resource"),
    pseudonymization = test_pseudonym_contract(
      .testPseudonymKey("private-resource")
    ),
    semantic_contract = test_plan_semantic_contract(),
    bundle_contract = test_plan_bundle_contract("private", token)
  )
  manifest <- .writeStagingManifest(staging_dir, list(private = descriptor))
  permission_bits <- function(path) {
    bitwAnd(as.integer(file.info(path)$mode), strtoi("777", base = 8L))
  }

  expect_equal(permission_bits(base), strtoi("700", base = 8L))
  expect_equal(permission_bits(staging_dir), strtoi("700", base = 8L))
  expect_equal(permission_bits(descriptor$metadata$file),
               strtoi("600", base = 8L))
  expect_equal(permission_bits(manifest), strtoi("600", base = 8L))
  expect_setequal(names(descriptor$metadata$column_types),
                  descriptor$metadata$columns)
})

test_that("stale cleanup follows TTL but never traverses staging symlinks", {
  skip_on_os("windows")
  base <- tempfile("dsstaging_ttl_")
  withr::local_options(list(
    dsstaging.base_dir = base,
    dsstaging.ttl_hours = 1
  ))
  stale <- .createStagingDir(.generateStagingToken())
  fresh <- .createStagingDir(.generateStagingToken())
  Sys.setFileTime(stale, Sys.time() - 2 * 3600)

  external <- tempfile("dsstaging_external_")
  dir.create(external)
  marker <- file.path(external, "keep")
  writeLines("keep", marker)
  link <- file.path(base, .generateStagingToken())
  on.exit(unlink(c(link, base, external), recursive = TRUE), add = TRUE)
  if (!isTRUE(file.symlink(external, link))) skip("Cannot create symlinks")

  .cleanStaleStagingDirs()

  expect_false(dir.exists(stale))
  expect_true(dir.exists(fresh))
  expect_true(file.exists(marker))
  expect_true(.isSymbolicLink(link))
})

test_that("handle cleanup removes only its tracked staged directories", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  dir <- .createStagingDir(.generateStagingToken())
  handle$staging_dirs <- dir

  .cleanupHandleStaging(handle)
  expect_false(dir.exists(dir))
  expect_length(handle$staging_dirs, 0L)
})

test_that("failed staging deletion stays owned and propagates for retry", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  dir <- .createStagingDir(.generateStagingToken())
  writeLines("private", file.path(dir, "data.csv"))
  handle$staging_dirs <- dir

  testthat::with_mocked_bindings(
    expect_error(.cleanupHandleStaging(handle), "deletion was not confirmed"),
    .unlinkStagingDirectory = function(...) 1L,
    .package = "dsOMOP"
  )
  expect_true(dir.exists(dir))
  expect_identical(handle$staging_dirs, dir)

  expect_silent(.cleanupHandleStaging(handle))
  expect_false(dir.exists(dir))
  expect_length(handle$staging_dirs, 0L)
})

test_that("omopCleanupDS close mode removes a handle and is retry-safe", {
  symbol <- "cleanup_close_test"
  handle <- create_test_handle()
  .setHandle(symbol, handle)
  on.exit(.removeHandle(symbol), add = TRUE)

  expect_true(omopCleanupDS(symbol, close = TRUE))
  expect_error(.getHandle(symbol), "No OMOP handle")
  expect_false(.removeHandle(symbol))
})

test_that("omopCleanupDS exact mode drops only one owned temp", {
  symbol <- paste0("cleanup_exact_", Sys.getpid())
  handle <- create_test_handle()
  .setHandle(symbol, handle)
  on.exit(.removeHandle(symbol), add = TRUE)

  first <- .createTempTable(
    handle, "dsomop_cleanup_exact_one", "SELECT 1 AS value"
  )
  second <- .createTempTable(
    handle, "dsomop_cleanup_exact_two", "SELECT 2 AS value"
  )
  handle$staging_dirs <- "staging-sentinel"

  expect_true(omopCleanupDS(symbol, prefix = first, exact = TRUE))
  expect_false(DBI::dbExistsTable(handle$conn, first))
  expect_true(DBI::dbExistsTable(handle$conn, second))
  expect_identical(handle$temp_tables, second)
  expect_identical(handle$staging_dirs, "staging-sentinel")

  expect_error(
    omopCleanupDS(symbol, prefix = "dsomop_.*", exact = TRUE),
    "valid SQL identifier|[Ii]nvalid"
  )
  expect_true(DBI::dbExistsTable(handle$conn, second))
  expect_true(omopCleanupDS(symbol, prefix = second, exact = TRUE))
  handle$staging_dirs <- character(0)
})

test_that("handle close disconnects a connection without a resource client", {
  handle <- create_test_handle()

  expect_true(DBI::dbIsValid(handle$conn))
  .closeHandle(handle)
  expect_false(DBI::dbIsValid(handle$conn))
})

test_that("handle close attempts resource cleanup after a temp-drop failure", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  closed <- FALSE
  handle$resource_client <- list(
    getConnection = function() handle$conn,
    close = function() {
      closed <<- TRUE
      invisible(NULL)
    }
  )
  handle$temp_tables <- "dsomop_close_failure"
  handle$temp_connection <- handle$conn

  local_mocked_bindings(
    .dropTempTable = function(...) stop("drop failed"),
    .package = "dsOMOP"
  )
  expect_error(.closeHandle(handle), "drop failed")
  expect_true(closed)
})

test_that("removeHandle retains a handle when full cleanup cannot be proven", {
  symbol <- "cleanup_failure_test"
  handle <- create_test_handle()
  .setHandle(symbol, handle)
  on.exit({
    if (exists(paste0("handle_", symbol), envir = .dsomop_env,
               inherits = FALSE)) {
      rm(list = paste0("handle_", symbol), envir = .dsomop_env)
    }
    cleanup_handle(handle)
  }, add = TRUE)

  local_mocked_bindings(
    .closeHandle = function(...) stop("cleanup failed"),
    .package = "dsOMOP"
  )
  expect_error(.removeHandle(symbol), "cleanup failed")
  expect_identical(.getHandle(symbol), handle)
})

test_that("handle cleanup rejects and retains a tracked staging symlink", {
  skip_on_os("windows")
  base <- tempfile("dsstaging_handle_")
  withr::local_options(list(dsstaging.base_dir = base))
  dir.create(base, mode = "0700")
  Sys.chmod(base, mode = "0700")
  external <- tempfile("dsstaging_handle_external_")
  dir.create(external)
  marker <- file.path(external, "keep")
  writeLines("keep", marker)
  link <- file.path(base, .generateStagingToken())
  on.exit(unlink(c(link, base, external), recursive = TRUE), add = TRUE)
  if (!isTRUE(file.symlink(external, link))) skip("Cannot create symlinks")
  handle <- new.env(parent = emptyenv())
  handle$staging_dirs <- link

  expect_error(.cleanupHandleStaging(handle), "unsafe or invalid owned path")

  expect_true(file.exists(marker))
  expect_true(.isSymbolicLink(link))
  expect_identical(handle$staging_dirs, link)
})

test_that("staging sanitizes identifiers before writing a data frame", {
  token <- .generateStagingToken()
  staging_dir <- .createStagingDir(token)
  on.exit(unlink(staging_dir, recursive = TRUE), add = TRUE)
  key <- .testPseudonymKey("safe-resource")
  raw <- data.frame(
    person_id = c(101L, 102L),
    condition_occurrence_id = c(9001L, 9002L),
    provider_id = c(51L, 52L),
    value = c(1, 2)
  )

  descriptor <- .stageDataFrame(
    raw, "safe", staging_dir, token, key,
    pseudonymization = test_pseudonym_contract(key),
    semantic_contract = test_plan_semantic_contract(),
    bundle_contract = test_plan_bundle_contract("safe", token)
  )
  landed <- read_staged_output(descriptor)

  expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$",
                         landed$person_id)))
  expect_false(any(as.character(raw$person_id) %in% landed$person_id))
  expect_false("condition_occurrence_id" %in% names(landed))
  expect_false("provider_id" %in% names(landed))
  expect_equal(landed$value, raw$value)
  expect_setequal(descriptor$metadata$columns, names(landed))
})

test_that("streamed staged plans sanitize identifiers inside every chunk", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  set_test_person_key(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(
      conditions = list(
        type = "event_level",
        table = "condition_occurrence",
        columns = NULL,
        concept_set = c(201820),
        representation = list(format = "long"),
        filters = list(concept_set = list(ids = c(201820)))
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(
    nfilter.subset = 3,
    dsomop.default_date_handling = "remove",
    dsomop.datetime_timezone = "UTC"
  ), {
    result <- .planExecute(handle, plan, list(conditions = "cond_df"),
                           output_mode = "staged")
  })
  descriptor <- result$conditions
  on.exit(unlink(dirname(descriptor$metadata$file), recursive = TRUE), add = TRUE)
  landed <- read_staged_output(descriptor)

  expect_s3_class(descriptor, "FlowerDatasetDescriptor")
  expect_true(nrow(landed) > 0L)
  expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$",
                         landed$person_id)))
  expect_false("condition_occurrence_id" %in% names(landed))
  expect_false("visit_occurrence_id" %in% names(landed))
  expect_setequal(descriptor$metadata$columns, names(landed))
  semantics <- descriptor$metadata$semantic_contract
  expect_identical(semantics$contract_version,
                   "dsomop-staged-semantics-v2")
  expect_identical(semantics$output_type, "event_level")
  expect_identical(semantics$output_format, "long")
  expect_identical(semantics$grain, "event")
  expect_identical(semantics$date_handling,
                   list(mode = "remove", reference = "index"))
  expect_identical(semantics$harmonization_contract_version,
                   "dsomop-harmonization-v3")
  expect_identical(semantics$age_semantics,
                   "reference_year_minus_year_of_birth")
  expect_identical(semantics$datetime_timezone, "UTC")
  expect_identical(semantics$week_start, "Monday")
})

test_that("streamed event_select keeps an omitted ordering date private", {
  handle <- create_test_handle()
  set_test_person_key(handle)
  on.exit(cleanup_handle(handle), add = TRUE)
  base <- tempfile("dsstaging_event_select_")
  withr::local_options(list(
    dsstaging.base_dir = base,
    nfilter.subset = 3
  ))
  plan <- list(outputs = list(first_condition = list(
    type = "event_level",
    table = "condition_occurrence",
    columns = "condition_concept_id",
    representation = list(format = "long"),
    temporal = list(event_select = list(order = "first", n = 1L)),
    filters = list()
  )), options = list(translate_concepts = FALSE, block_sensitive = TRUE))

  result <- .planExecute(handle, plan, list(), output_mode = "staged")
  descriptor <- result$first_condition
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  landed <- read_staged_output(descriptor)

  expect_gt(nrow(landed), 0L)
  expect_lte(max(table(landed$person_id)), 1L)
  expect_false(any(c(
    "condition_start_date", "dsomop_event_order_date",
    "dsomop_event_order_id", "rn"
  ) %in% names(landed)))
  expect_setequal(descriptor$metadata$columns, names(landed))
})

test_that("plan output names cannot escape a staging directory", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  set_test_person_key(handle)
  plan <- list(outputs = stats::setNames(list(list(
    type = "event_level", table = "condition_occurrence",
    representation = list(format = "long"), filters = list()
  )), "../escape"))

  expect_error(
    .planExecute(handle, plan, list(), output_mode = "staged"),
    "Invalid output name"
  )
})

test_that("plan output symbols cannot collide with person-period components", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  handle_symbol <- paste0("person_period_collision_handle_", Sys.getpid())
  .setHandle(handle_symbol, handle)
  on.exit(.removeHandle(handle_symbol), add = TRUE)

  expect_error(
    omopPlanExecuteDS(
      handle_symbol,
      list(outputs = list()),
      c(first = "panel", second = "panel.personPeriods")
    ),
    "collide after sparse/temporal suffix expansion"
  )
})

test_that("staged temporal outputs preserve descriptors and personRef", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  DBI::dbExecute(
    handle$conn,
    "UPDATE observation_period SET observation_period_start_date = '2018-01-01'"
  )
  set_test_person_key(handle)
  handle_symbol <- paste0("staged_temporal_handle_", Sys.getpid())
  output_symbol <- paste0("staged_temporal_output_", Sys.getpid())
  .setHandle(handle_symbol, handle)
  on.exit(.removeHandle(handle_symbol), add = TRUE)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    outputs = list(tc = list(
      type = "temporal_covariates",
      table = "condition_occurrence",
      concept_set = c(201820, 255573),
      bin_width = 90L,
      window_start = -365L,
      window_end = 0L,
      analyses = c("binary", "count")
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    omopPlanExecuteDS(
      handle_symbol, plan, stats::setNames("unused", "tc"),
      output_mode = "staged"
    )
  })

  assigned_names <- paste0(
    "unused.", c("temporalCovariates", "covariateRef", "timeRef", "personRef")
  )
  expect_true(all(vapply(assigned_names, exists, logical(1),
                         envir = environment(), inherits = FALSE)))
  descriptors <- lapply(assigned_names, get, envir = environment(),
                        inherits = FALSE)
  expect_true(all(vapply(descriptors, inherits, logical(1),
                         what = "FlowerDatasetDescriptor")))
  stage_dir <- dirname(descriptors[[1]]$metadata$file)
  on.exit(unlink(stage_dir, recursive = TRUE), add = TRUE)

  person_ref <- read_staged_output(descriptors[[4]])
  expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$",
                         person_ref$person_id)))
  expect_true("rowId" %in% names(person_ref))
  semantic_components <- vapply(descriptors, function(d) {
    d$metadata$semantic_contract$component
  }, character(1))
  expect_identical(
    unname(semantic_components),
    c("temporalCovariates", "covariateRef", "timeRef", "personRef")
  )
  expect_identical(
    descriptors[[4]]$metadata$semantic_contract$output_format, "linkage"
  )
  expect_identical(
    descriptors[[4]]$metadata$semantic_contract$grain, "episode"
  )
})

test_that("plan execute passes temporal and date_handling to extraction", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      conditions = list(
        type = "event_level",
        table = "condition_occurrence",
        columns = NULL,
        concept_set = c(201820),
        representation = list(format = "long"),
        filters = list(concept_set = list(ids = c(201820))),
        temporal = list(
          calendar = list(start = "2010-01-01", end = "2030-12-31")
        ),
        date_handling = list(mode = "absolute")
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3,
                           dsomop.allow_absolute_dates = TRUE), {
    result <- .planExecute(handle, plan, list(conditions = "cond_df"))
    expect_true(is.list(result))
    expect_true("conditions" %in% names(result))
    expect_true(is.data.frame(result$conditions))
  })
})

test_that("plan execute blocks absolute dates without server authorization", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      conditions = list(
        type = "event_level",
        table = "condition_occurrence",
        columns = NULL,
        concept_set = c(201820),
        representation = list(format = "long"),
        filters = list(concept_set = list(ids = c(201820))),
        date_handling = list(mode = "absolute")
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3,
                           dsomop.allow_absolute_dates = FALSE,
                           dsomop.query_strict = TRUE), {
    expect_error(
      .planExecute(handle, plan, list(conditions = "cond_df")),
      "not permitted by the server"
    )
  })
})

test_that("cohort temp table includes dates", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  # Create a cohort table with dates in the test database
  DBI::dbExecute(handle$conn, "DROP TABLE IF EXISTS cohort")
  DBI::dbExecute(handle$conn, paste0(
    "CREATE TABLE cohort (",
    "cohort_definition_id INTEGER, ",
    "subject_id INTEGER, ",
    "cohort_start_date TEXT, ",
    "cohort_end_date TEXT)"
  ))
  # Insert rows for all test persons
  for (pid in 1:15) {
    DBI::dbExecute(handle$conn, paste0(
      "INSERT INTO cohort VALUES (1, ", pid,
      ", '2020-01-01', '2020-12-31')"
    ))
  }

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1),
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      conditions = list(
        type = "event_level",
        table = "condition_occurrence",
        columns = NULL,
        representation = list(format = "long"),
        filters = list()
      )
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    # Force blueprint rebuild to pick up cohort table
    .buildBlueprint(handle, force = TRUE)
    result <- .planExecute(handle, plan, list(conditions = "cond_df"))
    expect_true(is.list(result))
    expect_true("conditions" %in% names(result))
  })
})

# --- Differencing-defence: preview count banding + audit logging ---

test_that("preview n_persons is banded down to a multiple of 5", {
  handle <- create_test_handle(n_persons = 12)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      baseline = list(type = "person_level",
                      tables = list(person = c("person_id", "gender_concept_id")))
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    res <- .planPreview(handle, plan)
    info <- res$outputs$baseline$tables$person
    # True count is 12; banded DOWN to nearest 5 => 10 (exact count never returned)
    expect_equal(info$n_persons, 10)
    expect_true(info$n_persons %% 5 == 0)
    expect_true(info$n_persons_banded)
    expect_equal(info$band_width, 5L)
    expect_equal(res$band_width, 5L)
    expect_false(info$disclosive)
  })
})

test_that("preview n_persons is NA below nfilter_subset and not banded", {
  handle <- create_test_handle(n_persons = 2)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      baseline = list(type = "person_level",
                      tables = list(person = c("person_id")))
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    res <- .planPreview(handle, plan)
    info <- res$outputs$baseline$tables$person
    expect_true(is.na(info$n_persons))
    expect_true(info$disclosive)
    expect_false(info$n_persons_banded)
  })
})

test_that("preview never returns an exact supra-threshold count", {
  # 13 persons: exact differencing primitive would be 13; banded must be 10.
  handle <- create_test_handle(n_persons = 13)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      baseline = list(type = "person_level",
                      tables = list(person = c("person_id")))
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3), {
    res <- .planPreview(handle, plan)
    expect_equal(res$outputs$baseline$tables$person$n_persons, 10)
  })
})

test_that("event preview has a flat payload for an unrestricted output", {
  handle <- create_test_handle(n_persons = 12)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(events = list(
      type = "event_level",
      table = "condition_occurrence",
      columns = c("condition_concept_id"),
      representation = list(format = "long")
    ))
  )

  withr::with_options(list(nfilter.subset = 3), {
    info <- .planPreview(handle, plan)$outputs$events
    expect_equal(info$type, "event_level")
    expect_equal(info$table, "condition_occurrence")
    expect_equal(info$representation, "long")
    expect_true("condition_concept_id" %in% info$columns)
    expect_null(info$events)
    expect_true(info$n_persons_available)
    expect_null(info$n_persons_unavailable_reason)
  })
})

test_that("preview never substitutes table-wide counts for restricted outputs", {
  handle <- create_test_handle(n_persons = 12)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  filtered_plan <- list(
    cohort = NULL,
    outputs = list(events = list(
      type = "event_level",
      table = "condition_occurrence",
      columns = c("condition_concept_id"),
      filters = list(concept_set = list(ids = 201820L)),
      representation = list(format = "long")
    ))
  )
  scoped_plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    outputs = list(person = list(
      type = "person_level",
      tables = list(person = c("gender_concept_id"))
    ))
  )

  withr::with_options(list(nfilter.subset = 3), {
    filtered <- .planPreview(handle, filtered_plan)$outputs$events
    expect_false(filtered$n_persons_available)
    expect_true(is.na(filtered$n_persons))
    expect_false(filtered$n_persons_banded)
    expect_true(is.na(filtered$disclosive))
    expect_match(filtered$n_persons_unavailable_reason, "filters")

    scoped <- .planPreview(handle, scoped_plan)$outputs$person$tables$person
    expect_false(scoped$n_persons_available)
    expect_true(is.na(scoped$n_persons))
    expect_false(scoped$n_persons_banded)
    expect_true(is.na(scoped$disclosive))
    expect_match(scoped$n_persons_unavailable_reason, "cohort")
  })
})

test_that("person feature preview separates final and source columns", {
  handle <- create_test_handle(n_persons = 12)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(features = list(
      type = "person_level",
      tables = list(condition_occurrence = list(features = list(
        dx_count = list(type = "count", concept_set = 201820L),
        named = list(
          type = "boolean", name = "has_condition", concept_set = 255573L
        )
      )))
    ))
  )

  info <- .planPreview(handle, plan)
  table_info <- info$outputs$features$tables$condition_occurrence

  expect_identical(info$validation$warnings, character(0))
  expect_identical(
    table_info$columns,
    c("person_id", "dx_count", "has_condition")
  )
  expect_true(table_info$columns_complete)
  expect_null(table_info$columns_unavailable_reason)
  expect_true("condition_concept_id" %in% table_info$source_columns)
  expect_identical(table_info$missing_columns, character(0))
})

test_that("episode feature preview includes its linkage key", {
  handle <- create_test_handle(n_persons = 12)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = list(type = "cohort_table", cohort_definition_id = 1L),
    outputs = list(features = list(
      type = "event_level",
      table = "condition_occurrence",
      temporal = list(index_window = list(start = -30L, end = 0L)),
      representation = list(
        format = "features",
        grain = "episode",
        features = list(
          dx_count = list(type = "count", concept_set = 201820L)
        )
      )
    ))
  )

  info <- .planPreview(handle, plan)$outputs$features

  expect_identical(
    info$columns,
    c("cohort_row_id", "person_id", "dx_count")
  )
  expect_true(info$columns_complete)
  expect_null(info$columns_unavailable_reason)
  expect_true("condition_concept_id" %in% info$source_columns)
  expect_identical(info$missing_columns, character(0))
})

test_that("automatic feature preview marks its dynamic schema incomplete", {
  handle <- create_test_handle(n_persons = 12)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)

  plan <- list(
    cohort = NULL,
    outputs = list(features = list(
      type = "event_level",
      table = "condition_occurrence",
      representation = list(format = "features", features = list())
    ))
  )

  info <- .planPreview(handle, plan)$outputs$features

  expect_identical(info$columns, "person_id")
  expect_false(info$columns_complete)
  expect_match(info$columns_unavailable_reason, "observed at execution")
  expect_identical(info$missing_columns, character(0))
})

test_that(".bandCount floors to band width and preserves NA", {
  expect_equal(.bandCount(50), 50)
  expect_equal(.bandCount(47), 45)
  expect_equal(.bandCount(12), 10)
  expect_true(is.na(.bandCount(NA_real_)))
  expect_true(is.na(.bandCount(NULL)))
})

test_that("omopPlanPreviewDS fires an audit-log record", {
  handle <- create_test_handle(n_persons = 12)
  on.exit(cleanup_handle(handle))
  .buildBlueprint(handle)
  .setHandle("audit_sym", handle)            # register so .getHandle resolves it

  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(
      baseline = list(type = "person_level",
                      tables = list(person = c("person_id")))
    ),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  class(plan) <- c("omop_plan", "list")

  withr::with_options(list(nfilter.subset = 3, dsomop.audit_log = TRUE), {
    expect_message(
      omopPlanPreviewDS("audit_sym", plan),
      "\\[dsomop-audit\\].*method=omopPlanPreviewDS"
    )
  })
})
