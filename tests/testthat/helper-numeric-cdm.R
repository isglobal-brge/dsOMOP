.numeric_enabled <- function() {
  if (!nzchar(Sys.getenv("DSOMOP_TEST_NUMERIC_CDM"))) testthat::skip("opt-in numeric CDM matrix")
  for (pkg in c("duckdb", "RPostgres", "arrow")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Numeric matrix requires ", pkg)
  }
}
.numeric_definitions <- function(env = parent.frame()) {
  sys.source(testthat::test_path("..", "fixtures", "numeric", "matrix.R"), env)
}
.numeric_backends <- function(env = parent.frame()) {
  .numeric_enabled()
  directory <- Sys.getenv("DSOMOP_TEST_NUMERIC_CDM")
  if (!file.exists(file.path(directory, "manifest.json"))) stop("Run tests/fixtures/numeric/load.R first")
  pg <- DBI::dbConnect(RPostgres::Postgres(), host = Sys.getenv("DSOMOP_TEST_POSTGRES_HOST", "omopdb"),
    port = as.integer(Sys.getenv("PGPORT", "5432")), dbname = Sys.getenv("PGDATABASE", "omop"),
    user = Sys.getenv("PGUSER", "omop"), password = Sys.getenv("PGPASSWORD", "omop"), sslmode = "disable")
  withr::defer(DBI::dbDisconnect(pg), envir = env)
  # Each matrix may create writable sidecar schemas. Parallel files cannot
  # share a DuckDB writer, and must never mutate the pinned imported fixture.
  snapshot <- withr::local_tempfile(fileext = ".duckdb", .local_envir = env)
  if (!file.copy(file.path(directory, "numeric_cdm.duckdb"), snapshot)) {
    stop("Could not create the test-local numeric DuckDB snapshot")
  }
  dk <- DBI::dbConnect(duckdb::duckdb(), dbdir = snapshot)
  withr::defer(DBI::dbDisconnect(dk, shutdown = TRUE), envir = env)
  list(postgresql = .ws4_handle_on(pg, "postgresql", "postgresql", "numeric_cdm"),
       duckdb = .ws4_handle_on(dk, "duckdb", "duckdb", "main"))
}
.numeric_run <- function(h, p, mode = "memory", chunk_check = NULL) {
  run <- .ws7_ctx(h, p, mode)
  on.exit(.ws7_close(run), add = TRUE)
  pop <- .compilePopulationByName(run$cctx, "p")
  out <- stats::setNames(as.list(paste0("N_", unlist(p$publish %||% names(p$outputs)))),
                         unlist(p$publish %||% names(p$outputs)))
  compiled <- .planCompileOutputs(run$cctx, out, populations = list(p = pop))
  # Capture raw IDs before publication, only inside this administrator test.
  raw <- lapply(compiled, function(o) lapply(o$components, function(c) c$frame))
  status <- withr::with_options(list(nfilter.subset = 5L), .publishOutputs(run$ctx, compiled, out))
  testthat::expect_identical(status$status, "ok")
  values <- lapply(status$published, function(root) lapply(root, function(c) {
    v <- get(c$symbol, run$ctx$target_env)
    if (inherits(v, "FlowerDatasetDescriptor")) {
      testthat::expect_identical(v$metadata$format,
        if (requireNamespace("arrow", quietly = TRUE)) "parquet" else "csv")
      chunks <- list(); .publishReadChunks(v, function(x) {if(!is.null(chunk_check))chunk_check(x);chunks[[length(chunks) + 1L]] <<- x})
      v <- do.call(rbind, chunks)
    }
    v
  }))
  list(values = values, raw = raw)
}
