.publication_compiled_context <- function(mode = "staged", threshold = 3L,
                                          .local_envir = parent.frame()) {
  handle <- create_test_handle()
  withr::defer(DBI::dbDisconnect(handle$conn), envir = .local_envir)
  base <- withr::local_tempdir(.local_envir = .local_envir)
  Sys.chmod(base, "0700")
  withr::local_options(list(dsstaging.base_dir = base, nfilter.subset = threshold,
                            dsomop.query_strict = TRUE, dsomop.staged_chunk_size = 2L),
                       .local_envir = .local_envir)
  identity <- paste0("test://compiled-publication/", basename(base))
  key <- .testPseudonymKey(identity)
  resource_id <- substr(as.character(openssl::sha256(charToRaw(identity))), 1L, 32L)
  environment <- paste(format(key), collapse = "")
  names(environment) <- paste0("DSOMOP_PSEUDONYM_KEY_", resource_id)
  withr::local_envvar(c(environment, DSOMOP_PSEUDONYM_PROVIDER = "scoped",
    DSOMOP_PSEUDONYM_EPOCH = "1", DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false",
    DSOMOP_PSEUDONYM_ROOT = "", DSOMOP_PSEUDONYM_KEY = ""),
    .local_envir = .local_envir)
  handle$person_key_identity <- identity
  handle$person_key_id <- .personKeyId(key)
  handle$person_key_provider <- "scoped"
  handle$person_key_epoch <- 1L
  handle$person_key_require_existing <- FALSE
  handle$person_key_contract_version <- 1L
  ir <- list(version = 2L, meta = list(language_version = "2.0",
    semantic_version = "2.0.0", generated_by = "compiled-publication-test"),
    populations = list(base = list(kind = "criteria")),
    outputs = list(out = list(rows = "base", format = "wide", grain = "person")))
  ctx <- .planContext(handle, ir, mode, new.env(parent = emptyenv()))
  withr::defer({
    .planContextClose(ctx)
    if (exists(ctx$id, .publicationRegistry, inherits = FALSE)) {
      rm(list = ctx$id, envir = .publicationRegistry)
    }
  }, envir = .local_envir)
  ctx
}

.publication_compiled_output <- function(frame = NULL, statement = NULL,
                                         mode = "staged", roster = FALSE) {
  if (is.null(frame) && is.null(statement)) statement <- paste(
    "SELECT person_id, person_id * 1.0 AS value, 'private' AS raw_source",
    "FROM person WHERE person_id <= 5 ORDER BY person_id"
  )
  columns <- if (roster) {
    list(rowId = .fragmentColumn("integer", FALSE, "episode_key"))
  } else list(person_id = .fragmentColumn("integer", FALSE, "person_key"))
  columns$value <- .fragmentColumn("numeric")
  columns$raw_source <- .fragmentColumn("text", role = "private")
  fragment <- .fragment("result", "result", "person",
    statement %||% "SELECT 1", columns,
    person_lineage = if (roster) list(person_col = NULL, via_roster = "base") else
      list(person_col = "person_id", via_roster = NULL),
    roster_namespace = if (roster) "base" else NULL,
    private_columns = "raw_source")
  lineage <- if (roster) .lineage("roster", "rowId", "base", "rowId", "person_id") else
    .lineage("direct", "person_id")
  candidate <- .publicationCandidate("out", "out", if (mode == "staged") "staged" else "frame",
    lineage, private_columns = "raw_source", grain = "person",
    requested_symbol = if (mode == "memory") "D" else NULL,
    requested_path = if (mode == "staged") "D.parquet" else NULL)
  component <- list(fragment = fragment)
  if (is.null(frame)) component$statement <- statement else component$frame <- frame
  .compiledOutput("out", list(out = component), list(candidate))
}

test_that("compiled memory frames and statements enforce the declared component schema", {
  for (source in c("frame", "statement")) {
    ctx <- .publication_compiled_context(mode = "memory")
    compiled <- .publication_compiled_output(mode = "memory",
      frame = if (source == "frame") data.frame(person_id = 1:5, value = 1:5,
                                                raw_source = "private") else NULL)
    .publishCompiled(ctx, compiled)
    expect_identical(ls(ctx$target_env), character(0))
    status <- .publishCommit(ctx)
    value <- get(status$published$out$out$symbol, ctx$target_env)
    expect_identical(names(value), c("person_id", "value"))
    expect_identical(nrow(value), 5L)
    expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$", value$person_id)))
  }
  ctx <- .publication_compiled_context(mode = "memory")
  compiled <- .publication_compiled_output(mode = "memory", frame = data.frame(
    person_id = 1:5, unexpected = 1:5, raw_source = "private"
  ))
  expect_error(.publishCompiled(ctx, compiled), "validation_error")
  expect_identical(ls(ctx$target_env), character(0))
})

test_that("compiled SQL is checked before memory or staged execution", {
  local_mocked_bindings(
    .executeQuery = function(...) stop("Unexpected SQL execution"),
    .executeQueryToParquet = function(...) stop("Unexpected SQL execution"))
  for (mode in c("memory", "staged")) {
    ctx <- .publication_compiled_context(mode = mode)
    compiled <- .publication_compiled_output(mode = mode, statement = paste(
      "SELECT person_id, DATEADD(day, person_id, '2020-01-01') AS value,",
      "'private' AS raw_source FROM person"
    ))
    expect_error(.publishCompiled(ctx, compiled), "contains an OHDSI DATEADD/")
    expect_length(ctx$prepared, 0L)
    expect_identical(ls(ctx$target_env), character(0))
  }
})

test_that("compiled staged SQL streams once and counts the actual stored chunks", {
  ctx <- .publication_compiled_context(threshold = 5L)
  compiled <- .publication_compiled_output()
  semantic_digest <- .planSemanticDigest(ctx$ir)$sha256
  stream <- .executeQueryToParquet
  calls <- 0L
  chunks <- integer(0)
  local_mocked_bindings(
    .executeQuery = function(...) stop("Full materialisation is forbidden"),
    .executeQueryToParquet = function(conn, sql, output_path, chunk_size, chunk_fn, ctx) {
      calls <<- calls + 1L
      expect_false(grepl("COUNT\\s*\\(", sql, ignore.case = TRUE))
      stream(conn, sql, output_path, chunk_size,
        chunk_fn = function(frame) {
          chunks <<- c(chunks, nrow(frame))
          chunk_fn(frame)
        }, ctx = ctx)
    })
  .publishCompiled(ctx, compiled)
  expect_identical(calls, 1L)
  expect_true(sum(chunks > 0L) >= 3L)
  expect_lte(max(chunks), 2L)
  private <- get("out/out", ctx$staging_env)
  expect_true(startsWith(private$metadata$file, ctx$private_dir))
  expect_identical(private$metadata$columns, c("person_id", "value"))
  values <- list()
  .publishReadChunks(private, function(frame) values[[length(values) + 1L]] <<- frame)
  values <- do.call(rbind, values)
  expect_identical(nrow(values), 5L)
  expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$", values$person_id)))
  expect_identical(ls(ctx$target_env), character(0))
  status <- .publishCommit(ctx)
  expect_identical(status$status, "ok")
  descriptor <- get(status$published$out$out$symbol, ctx$target_env)
  expect_identical(descriptor$contract_version, 2L)
  expect_identical(descriptor$metadata$semantic_contract$contract_version,
                   "dsomop-staged-semantics-v3")
  expect_identical(descriptor$metadata$semantic_contract$query_semantics_sha256,
                   semantic_digest)
  expect_silent(omopStagedDatasetPath(descriptor))
  expect_false(dir.exists(file.path(ctx$staging_dir, ".private")))
  expect_identical(calls, 1L)
})

test_that("compiled staged frames and origin-qualified roster streams agree with raw rosters", {
  for (source in c("frame", "statement")) {
    ctx <- .publication_compiled_context(threshold = 5L)
    roster <- data.frame(rowId = 1:5, person_id = 101:105)
    .planRegisterRoster(ctx, "base", roster)
    compiled <- .publication_compiled_output(roster = TRUE,
      statement = paste("SELECT person_id AS rowId, person_id * 1.0 AS value,",
                        "'private' AS raw_source FROM person WHERE person_id <= 5"),
      frame = if (source == "frame") data.frame(rowId = 1:5, value = 1:5,
                                                raw_source = "private") else NULL)
    .publishCompiled(ctx, compiled)
    expect_identical(get("base", ctx$rosters), roster)
    prepared <- ctx$prepared[["out/out"]]
    token_namespace <- prepared$candidate$lineage$roster_id
    expect_false(identical(token_namespace, "base"))
    expect_true(bindingIsLocked(token_namespace, ctx$rosters))
    token_roster <- get(token_namespace, ctx$rosters)
    expect_identical(token_roster$rowId, roster$rowId)
    expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$", token_roster$person_id)))
    private <- get("out/out", ctx$staging_env)
    expect_identical(private$metadata$columns, c("rowId", "value"))
    status <- .publishCommit(ctx)
    expect_identical(status$status, "ok")
    expect_silent(omopStagedDatasetPath(get(status$published$out$out$symbol, ctx$target_env)))
  }
})

test_that("CSV staged roster keys match decimal doubles from frames and SQL", {
  local_mocked_bindings(.arrowAvailable = function() FALSE)
  withr::local_options(list(scipen = 0))
  scipen <- getOption("scipen")
  for (source in c("frame", "statement")) {
    ctx <- .publication_compiled_context(threshold = 5L)
    ctx$failure_hook <- function(step) {
      if (step == "chunk") expect_identical(getOption("scipen"), scipen)
    }
    keys <- c(100000, 100001, 999999, 1000000, 1000001)
    roster <- data.frame(rowId = keys, person_id = 101:105)
    .planRegisterRoster(ctx, "base", roster)
    compiled <- .publication_compiled_output(roster = TRUE,
      statement = paste("SELECT CAST(CASE person_id",
        "WHEN 1 THEN 100000 WHEN 2 THEN 100001 WHEN 3 THEN 999999",
        "WHEN 4 THEN 1000000 ELSE 1000001 END AS REAL) AS rowId,",
        "person_id * 1.0 AS value, 'private' AS raw_source",
        "FROM person WHERE person_id <= 5 ORDER BY person_id"),
      frame = if (source == "frame") data.frame(rowId = keys, value = 1:5,
                                                raw_source = "private") else NULL)
    .publishCompiled(ctx, compiled)
    expect_identical(getOption("scipen"), scipen)
    descriptor <- get("out/out", ctx$staging_env)
    expect_identical(descriptor$metadata$format, "csv")
    rows <- NULL
    .publishReadChunks(descriptor, function(chunk) rows <<- rbind(rows, chunk))
    expect_identical(rows$rowId, c("100000", "100001", "999999", "1000000", "1000001"))
    expect_true(ctx$prepared[["out/out"]]$passed)
    status <- .publishCommit(ctx)
    expect_identical(status$status, "ok")
    expect_silent(omopStagedDatasetPath(get(status$published$out$out$symbol, ctx$target_env)))
  }
})

test_that("DuckDB BIGINT roster keys retain lineage through CSV staging", {
  skip_if_not_installed("duckdb")
  local_mocked_bindings(.arrowAvailable = function() FALSE)
  withr::local_options(list(scipen = 0))
  scipen <- getOption("scipen")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  ctx <- .publication_compiled_context(threshold = 3L)
  frame <- DBI::dbGetQuery(con, paste(
    "SELECT CAST(key AS BIGINT) AS rowId, CAST(1 AS DOUBLE) AS value,",
    "'private' AS raw_source FROM (VALUES (100000), (1000000), (3000000000)) AS t(key)",
    "ORDER BY key"))
  roster <- data.frame(rowId = frame$rowId, person_id = 101:103)
  .planRegisterRoster(ctx, "base", roster)
  compiled <- .publication_compiled_output(roster = TRUE, frame = frame)
  compiled$components$out$fragment$columns$rowId$type <- "bigint"
  .publishCompiled(ctx, compiled)
  expect_identical(getOption("scipen"), scipen)
  descriptor <- get("out/out", ctx$staging_env)
  expect_identical(descriptor$metadata$format, "csv")
  rows <- NULL
  .publishReadChunks(descriptor, function(chunk) rows <<- rbind(rows, chunk))
  expect_identical(rows$rowId, c("100000", "1000000", "3000000000"))
  expect_true(ctx$prepared[["out/out"]]$passed)
  status <- .publishCommit(ctx)
  expect_identical(status$status, "ok")
  expect_silent(omopStagedDatasetPath(get(status$published$out$out$symbol, ctx$target_env)))
})

test_that("compiled staged composite keys count their own distinct stored tokens", {
  ctx <- .publication_compiled_context(threshold = 5L)
  ctx$ir$outputs$out$grain <- "episode"
  roster <- data.frame(person_id = 1:5, cohort_row_id = 1L)
  .planRegisterRoster(ctx, "episodes", roster)
  frame <- transform(roster, value = seq_len(5L), raw_source = "private")
  fragment <- .fragment("result", "result", "episode", "SELECT 1",
    list(person_id = .fragmentColumn("integer", FALSE, "person_key"),
         cohort_row_id = .fragmentColumn("integer", FALSE, "episode_key"),
         value = .fragmentColumn("numeric"),
         raw_source = .fragmentColumn("text", role = "private")),
    person_lineage = list(person_col = "person_id", via_roster = "episodes"),
    roster_namespace = "episodes", private_columns = "raw_source")
  candidate <- .publicationCandidate("out", "out", "staged",
    .lineage("roster", c("person_id", "cohort_row_id"), "episodes",
             c("person_id", "cohort_row_id"), "person_id"),
    private_columns = "raw_source", requested_path = "D.parquet", grain = "episode")
  compiled <- .compiledOutput("out", list(out = list(frame = frame, fragment = fragment)),
                              list(candidate))
  .publishCompiled(ctx, compiled)
  expect_identical(get("episodes", ctx$rosters), roster)
  expect_true(ctx$prepared[["out/out"]]$passed)
  descriptor <- get("out/out", ctx$staging_env)
  rows <- NULL
  .publishReadChunks(descriptor, function(chunk) rows <<- rbind(rows, chunk))
  expect_identical(as.character(rows$cohort_row_id), rep("1", 5L))
  expect_identical(as.character(rows$person_id),
                   .hashPersonKey(roster$person_id, .personKey(ctx$handle)))
  expect_identical(.publishCommit(ctx)$status, "ok")

  ctx <- .publication_compiled_context(threshold = 3L)
  compiled <- .publication_compiled_output(statement = paste(
    "SELECT ((person_id - 1) % 2) + 1 AS person_id, person_id * 1.0 AS value,",
    "'private' AS raw_source FROM person WHERE person_id <= 5 ORDER BY person_id"
  ))
  .publishCompiled(ctx, compiled)
  expect_false(ctx$prepared[["out/out"]]$passed)
  status <- .publishCommit(ctx)
  expect_identical(status$status, "refused")
  expect_identical(status$rejected$out$code, "insufficient_support")
  expect_identical(ls(ctx$target_env), character(0))
  expect_false(dir.exists(ctx$staging_dir))
})

test_that("compiled outputs must belong to the selected publication root", {
  ctx <- .publication_compiled_context(mode = "memory")
  compiled <- .publication_compiled_output(mode = "memory")
  ctx$ir$outputs$public <- ctx$ir$outputs$out
  ctx$ir$publish <- list("public")
  status <- .publishOutputs(ctx, list(public = compiled), list(public = "D"))
  expect_identical(status$status, "refused")
  expect_identical(status$rejected$public$code, "validation_error")
  expect_identical(ls(ctx$target_env), character(0))
})

test_that("compiled output availability is transported independently of support", {
  ctx <- .publication_compiled_context(mode = "memory")
  compiled <- .publication_compiled_output(mode = "memory")
  compiled$status <- "unavailable"
  .publishCompiled(ctx, compiled)
  status <- .publishCommit(ctx)
  expect_identical(status$status, "ok")
  expect_identical(status$unavailable, "out")
})

test_that("late malformed rows and chunk failures cannot publish staged components", {
  for (failure in c("orphan", "chunk")) {
    ctx <- .publication_compiled_context(threshold = 3L)
    .planRegisterRoster(ctx, "base", data.frame(rowId = 1:5, person_id = 101:105))
    compiled <- .publication_compiled_output(roster = TRUE, statement = paste(
      "SELECT person_id AS rowId, person_id * 1.0 AS value, 'private' AS raw_source",
      paste0("FROM person WHERE person_id <= ", if (failure == "orphan") 6L else 5L,
             " ORDER BY person_id")
    ))
    if (failure == "chunk") {
      hits <- 0L
      ctx$failure_hook <- function(step) {
        if (step == "chunk") {
          hits <<- hits + 1L
          if (hits == 3L) stop("injected late chunk failure")
        }
      }
    }
    expect_error(.publishCompiled(ctx, compiled), if (failure == "orphan")
      "lineage_malformed" else "injected late chunk failure")
    expect_identical(ls(ctx$target_env), character(0))
    directory <- ctx$staging_dir
    .planContextClose(ctx)
    expect_false(dir.exists(directory))
  }
})

test_that("native staged semantics bind dynamic components without legacy relabelling", {
  ctx <- .publication_compiled_context()
  .publishStagingDir(ctx)
  ctx$ir$outputs$out <- list(rows = "base", format = "survival", grain = "episode",
                            options = list(format = "recurrent_events"))
  contracts <- .publicationStagedContracts(ctx, "out", "custom_tar")
  expect_identical(contracts$semantic$output_type, "survival")
  expect_identical(contracts$semantic$output_format, "recurrent_events")
  expect_identical(contracts$semantic$component, "custom_tar")
  expect_identical(contracts$semantic$query_semantics_sha256,
                   .planSemanticDigest(ctx$ir)$sha256)
  expect_silent(.validateStagedBundleContract(contracts$bundle,
    dataset_id = "omop.plan.out.custom_tar", staged_token = ctx$staging_token,
    semantic_contract = contracts$semantic))
  bad <- contracts$semantic
  bad$component <- "bad/component"
  expect_error(.publicationValidateNativeSemantic(bad), "Invalid native")
  bad <- contracts$semantic
  bad$output_format <- "unreviewed"
  expect_error(.publicationValidateNativeSemantic(bad), "Invalid native")
  ctx$ir$outputs$out <- list(rows = "base", format = "person_period",
                            options = list(representation = "dense"))
  contracts <- .publicationStagedContracts(ctx, "out")
  expect_identical(contracts$semantic$output_type, "person_period")
  expect_identical(contracts$semantic$output_format, "dense")
  expect_true("component" %in% names(contracts$semantic))
  expect_null(contracts$semantic$component)
  ctx$publication_shapes[["out/out"]] <- "person"
  primary <- .publicationStagedContracts(ctx, "out")
  companion <- .publicationStagedContracts(ctx, "out", "custom_tar")
  expect_identical(primary$bundle, companion$bundle)
  expect_identical(primary$bundle$semantic_contract$grain, "person")
  legacy <- list(outputs = list(out = list(type = "event_level",
                                           representation = list(format = "long"))))
  ctx$ir <- legacy
  contracts <- .publicationStagedContracts(ctx, "out")
  expect_identical(contracts$semantic, .stagedSemanticContract(legacy, "out"))
  expect_identical(contracts$bundle, .stagedBundleContract(legacy, "out", ctx$staging_token))
})

test_that("native staged format validation follows the packaged schema enums", {
  ctx <- .publication_compiled_context()
  contract <- .publicationStagedContracts(ctx, "out")$semantic
  schema <- .jsonSchemaPackaged("plan-v2")
  # Narrow each enum independently: a stale native-protocol enumeration would
  # continue accepting the removed values. Keep one permitted control each.
  schema[["$defs"]]$output$properties$format$enum <- list("long", "survival", "person_period")
  schema[["$defs"]]$options_survival$properties$format$enum <- list("survival")
  schema[["$defs"]]$options_person_period$properties$representation$enum <- list("sparse")
  local_mocked_bindings(.jsonSchemaPackaged = function(name) {
    expect_identical(name, "plan-v2")
    schema
  })
  expect_error(.publicationValidateNativeSemantic(contract), "Invalid native")
  contract$output_type <- contract$output_format <- "long"
  expect_silent(.publicationValidateNativeSemantic(contract))
  contract$output_type <- "survival"
  contract$output_format <- "recurrent_events"
  expect_error(.publicationValidateNativeSemantic(contract), "Invalid native")
  contract$output_format <- "survival"
  expect_silent(.publicationValidateNativeSemantic(contract))
  contract$output_type <- "person_period"
  contract$output_format <- "dense"
  expect_error(.publicationValidateNativeSemantic(contract), "Invalid native")
  contract$output_format <- "sparse"
  expect_silent(.publicationValidateNativeSemantic(contract))
})
