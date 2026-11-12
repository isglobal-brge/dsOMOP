# Shared fixtures for the WS-7 tests (design-2.10.3 sections 2.7, 2.9, 6.11,
# 6.12, 7.3): variables, reducers and outputs over hand-built ROSTER DOUBLES of
# the frozen roster contract of section 6.9 (cohort_row_id, subject_id,
# cohort_start_date, cohort_end_date, index_source, index_key, index_visit_id,
# set_id, is_case, sampling_weight, op_start, op_end, private op_component_id)
# until the WS-6 populations compiler lands. A double is built like WS-6's
# rosters: candidates in a test table, the population's coverage compiled on
# them (R/compile_coverage.R), and the final roster materialised as a SESSION
# TEMPORARY table, so MySQL's one-reference rule for temporary tables is
# exercised by every statement of the reducer and output compilers.

.ws7_backends <- function() .ws5_backends()

# A scoped pseudonymisation key and staging directory for real publication of
# the compiled outputs (memory and staged), as test-publication-compiled.R.
.ws7_keys <- function(handles, .local_envir = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = .local_envir)
  Sys.chmod(base, "0700")
  identity <- paste0("test://ws7/", basename(base))
  key <- .testPseudonymKey(identity)
  resource_id <- substr(as.character(openssl::sha256(charToRaw(identity))), 1L, 32L)
  env <- paste(format(key), collapse = "")
  names(env) <- paste0("DSOMOP_PSEUDONYM_KEY_", resource_id)
  withr::local_envvar(c(env, DSOMOP_PSEUDONYM_PROVIDER = "scoped", DSOMOP_PSEUDONYM_EPOCH = "1",
                        DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false", DSOMOP_PSEUDONYM_ROOT = "",
                        DSOMOP_PSEUDONYM_KEY = ""), .local_envir = .local_envir)
  withr::local_options(list(dsstaging.base_dir = base, dsomop.staged_chunk_size = 7L),
                       .local_envir = .local_envir)
  for (h in handles) {
    h$person_key_identity <- identity
    h$person_key_id <- .personKeyId(key)
    h$person_key_provider <- "scoped"
    h$person_key_epoch <- 1L
    h$person_key_require_existing <- FALSE
    h$person_key_contract_version <- 1L
  }
  invisible(key)
}

# The plan context and its compilation context for a language-2.0 plan.
.ws7_ctx <- function(handle, plan, mode = "memory", target = new.env(parent = emptyenv())) {
  plan <- .irJsonForm(plan)
  .irValidateWire(plan)
  ir <- .irNormalize(plan)
  ctx <- .planContext(handle, ir, mode, target)
  list(ctx = ctx, cctx = .compileContext(ctx, ir), ir = ir)
}

.ws7_close <- function(run) {
  try(.compileReleaseTempTables(run$cctx), silent = TRUE)
  try(.planContextClose(run$ctx), silent = TRUE)
  if (exists(run$ctx$id, .publicationRegistry, inherits = FALSE)) {
    rm(list = run$ctx$id, envir = .publicationRegistry)
  }
  invisible(NULL)
}

.ws7_table_seq <- new.env(parent = emptyenv())

# A roster double from an episode frame: subject_id, cohort_start_date,
# cohort_end_date, index_source, index_key (+ index_visit_id, set_id,
# is_case, sampling_weight). The candidates are written as a test table, the
# population's coverage compiled on them, and the final roster -- the covered
# episodes under the policy, ends capped at op_end (event_end strategy),
# cohort_row_id ranked in the chronological identity order of section 6.9 --
# materialised as a session temporary table.
.ws7_population <- function(run, pop, frame, cap_end = TRUE) {
  cctx <- run$cctx
  handle <- cctx$handle
  h <- .compileDialect(cctx)
  conn <- handle$conn
  n <- (get0("n", envir = .ws7_table_seq, ifnotfound = 0L)) + 1L
  assign("n", n, envir = .ws7_table_seq)
  tname <- paste0("ws7c_", pop, "_", n)
  for (col in c("index_visit_id", "set_id", "is_case", "sampling_weight")) {
    if (!col %in% names(frame)) frame[[col]] <- rep(NA_real_, nrow(frame))
  }
  frame$cohort_start_date <- as.Date(frame$cohort_start_date)
  frame$cohort_end_date <- as.Date(frame$cohort_end_date)
  o <- order(frame$subject_id, frame$cohort_start_date, frame$cohort_end_date, frame$index_source,
             frame$index_key, method = "radix")
  frame <- frame[o, , drop = FALSE]
  frame$cand_key <- seq_len(nrow(frame))
  cols <- c("cand_key", "subject_id", "cohort_start_date", "cohort_end_date", "index_source", "index_key",
            "index_visit_id", "set_id", "is_case", "sampling_weight")
  frame <- frame[cols]
  schema <- handle$cdm_schema
  id <- if (is.null(schema) || identical(handle$dbms, "sqlite")) tname else DBI::Id(schema = schema, table = tname)
  types <- c(cand_key = "INTEGER", subject_id = "INTEGER", cohort_start_date = "DATE",
             cohort_end_date = "DATE", index_source = "VARCHAR(64)", index_key = "VARCHAR(255)",
             index_visit_id = "BIGINT", set_id = "BIGINT", is_case = "INTEGER", sampling_weight = "DOUBLE PRECISION")
  if (identical(handle$dbms, "sqlite")) {
    frame$cohort_start_date <- format(frame$cohort_start_date, "%Y-%m-%d")
    frame$cohort_end_date <- format(frame$cohort_end_date, "%Y-%m-%d")
    DBI::dbWriteTable(conn, id, frame, overwrite = TRUE)
  } else {
    if (handle$dbms %in% c("mysql", "mariadb")) types[["sampling_weight"]] <- "DOUBLE"
    if (identical(handle$dbms, "duckdb")) types[["sampling_weight"]] <- "DOUBLE"
    DBI::dbWriteTable(conn, id, frame, overwrite = TRUE, field.types = types)
  }
  qualified <- .qualifyTable(handle, tname, schema)
  cand <- .fragment(paste0("cand_", pop), "candidates", "episode",
                    paste0("SELECT ", .sqlLosslessKey(h, "w.cand_key"), " AS cand_key, w.subject_id AS subject_id, ",
                           "w.cohort_start_date AS cohort_start_date, w.cohort_end_date AS cohort_end_date FROM ",
                           qualified, " w"),
                    columns = list(cand_key = .fragmentColumn("bigint", FALSE, "episode_key"),
                                   subject_id = .fragmentColumn("integer", FALSE, "person_key"),
                                   cohort_start_date = .fragmentColumn("date", FALSE, "start"),
                                   cohort_end_date = .fragmentColumn("date", TRUE, "end")),
                    unique_key = "cand_key",
                    person_lineage = list(person_col = "subject_id", via_roster = NULL),
                    private_columns = "cand_key")
  attr(cand, "population") <- pop
  .fragmentRegister(cctx, cand)
  cov <- .compileCoverage(cctx, pop, cctx$ir$populations[[pop]]$coverage, cand, "candidate")
  keep <- .coverageKeepSql(cov$policy, "o")
  end <- if (cap_end) {
    paste0("CASE WHEN o.op_end IS NOT NULL AND w.cohort_end_date > o.op_end THEN o.op_end ",
           "ELSE w.cohort_end_date END")
  } else "w.cohort_end_date"
  rn <- .sqlRowNumber(h, character(0), c("w.subject_id", "w.cohort_start_date", "w.cohort_end_date",
                                         "w.index_source", "w.index_key"))
  select <- paste0("SELECT ", .sqlLosslessKey(h, rn), " AS cohort_row_id, w.subject_id AS subject_id, ",
                   "w.cohort_start_date AS cohort_start_date, ", end, " AS cohort_end_date, ",
                   "w.index_source AS index_source, w.index_key AS index_key, ",
                   .sqlLosslessKey(h, "w.index_visit_id"), " AS index_visit_id, ",
                   .sqlLosslessKey(h, "w.set_id"), " AS set_id, w.is_case AS is_case, ",
                   "w.sampling_weight AS sampling_weight, o.op_start AS op_start, o.op_end AS op_end, ",
                   "o.component_id AS op_component_id FROM ", qualified, " w INNER JOIN ", cov$coverage$name,
                   " o ON o.row_key = ", .sqlLosslessKey(h, "w.cand_key"), " WHERE ", keep)
  sql <- .buildStatement(cctx, cov$coverage, select = select, dialect = h)
  temp <- .reserveTempTableName(handle, paste0("dsomop_ws7_pop_", n))
  .createTempTable(handle, temp, sql)
  owned <- cctx[["compile_temp_tables"]] %||% character(0)
  assign("compile_temp_tables", unique(c(owned, temp)), envir = cctx)
  roster <- .fragment(paste0("pop_", pop), "roster", "episode",
                      paste0("SELECT t.cohort_row_id AS cohort_row_id, t.subject_id AS subject_id, ",
                             "t.cohort_start_date AS cohort_start_date, t.cohort_end_date AS cohort_end_date, ",
                             "t.index_source AS index_source, t.index_key AS index_key, ",
                             "t.index_visit_id AS index_visit_id, t.set_id AS set_id, t.is_case AS is_case, ",
                             "t.sampling_weight AS sampling_weight, t.op_start AS op_start, t.op_end AS op_end, ",
                             "t.op_component_id AS op_component_id FROM ", temp, " t"),
                      columns = list(cohort_row_id = .fragmentColumn("bigint", FALSE, "episode_key"),
                                     subject_id = .fragmentColumn("integer", FALSE, "person_key"),
                                     cohort_start_date = .fragmentColumn("date", TRUE, "start"),
                                     cohort_end_date = .fragmentColumn("date", TRUE, "end"),
                                     index_source = .fragmentColumn("text", FALSE, "origin_identity"),
                                     index_key = .fragmentColumn("text", TRUE, "origin_identity"),
                                     index_visit_id = .fragmentColumn("bigint", TRUE, "private"),
                                     set_id = .fragmentColumn("bigint", TRUE, "analytic_key"),
                                     is_case = .fragmentColumn("integer", TRUE, "measure"),
                                     sampling_weight = .fragmentColumn("numeric", TRUE, "measure"),
                                     op_start = .fragmentColumn("date", TRUE, "start"),
                                     op_end = .fragmentColumn("date", TRUE, "end"),
                                     op_component_id = .fragmentColumn("bigint", TRUE, "private")),
                      unique_key = "cohort_row_id",
                      person_lineage = list(person_col = "subject_id", via_roster = NULL),
                      roster_namespace = pop, private_columns = c("index_visit_id", "op_component_id"))
  .fragmentRegister(cctx, roster)
  list(roster = roster, coverage = cov, candidates = cand, temp_table = temp)
}

# Episodes of a results cohort (A-1: namespace cohort:<id>, lossless key
# <id>:<subject>:<start>:<end>).
.ws7_cohort_frame <- function(handle, cid) {
  q <- .executeQuery(handle, paste0(
    "SELECT subject_id, cohort_start_date, cohort_end_date FROM ",
    .qualifyTable(handle, "cohort", handle$cdm_schema), " WHERE cohort_definition_id = ", cid))
  q$cohort_start_date <- as.Date(substr(as.character(q$cohort_start_date), 1L, 10L))
  q$cohort_end_date <- as.Date(substr(as.character(q$cohort_end_date), 1L, 10L))
  q$index_source <- rep(paste0("cohort:", cid), nrow(q))
  q$index_key <- paste0(cid, ":", q$subject_id, ":", format(q$cohort_start_date, "%Y-%m-%d"), ":",
                        format(q$cohort_end_date, "%Y-%m-%d"))
  q
}

# Compile and publish the roots of a run (memory or staged); returns the
# status and the published values by root/component.
.ws7_publish <- function(run, populations, out = NULL, threshold = 5L) {
  ir <- run$ir
  out <- out %||% stats::setNames(as.list(vapply(names(ir$outputs), function(o) {
    ir$outputs[[o]]$result_symbol %||% paste0("D_", o)
  }, "")), names(ir$outputs))
  compiled <- .planCompileOutputs(run$cctx, out, populations = populations)
  status <- withr::with_options(list(nfilter.subset = threshold),
                                .publishOutputs(run$ctx, compiled, out))
  values <- list()
  for (root in names(status$published)) for (comp in names(status$published[[root]])) {
    v <- get(status$published[[root]][[comp]]$symbol, run$ctx$target_env)
    if (inherits(v, "FlowerDatasetDescriptor")) {
      chunks <- list()
      .publishReadChunks(v, function(x) chunks[[length(chunks) + 1L]] <<- x)
      v <- do.call(rbind, chunks)
    }
    values[[root]][[comp]] <- v
  }
  list(status = status, values = values, compiled = compiled)
}

# Expected rows of an acceptance output (recipes 01..15).
.ws7_expected <- function(recipe, output) .ws5_expected(recipe, output)
.ws7_plan <- function(recipe) .ws5_acceptance_plan(recipe)
.ws7_released <- function(recipe, output) {
  path <- testthat::test_path("..", "..", "inst", "plans", "acceptance", recipe)
  e <- jsonlite::fromJSON(file.path(path, "expected.json"), simplifyVector = FALSE)
  as.character(unlist(e$outputs[[output]]$released_columns))
}

# Compare a compiled component frame with the oracle rows on (person_id,
# index_date[, index_key]) and the comparison columns.
.ws7_compare <- function(frame, roster_frame, want, cols, info, keys = c("person_id", "index_date")) {
  at <- match(as.character(frame$cohort_row_id), as.character(roster_frame$cohort_row_id))
  got <- frame
  got$person_id <- as.numeric(roster_frame$subject_id[at])
  got$index_date <- format(as.Date(roster_frame$cohort_start_date[at]), "%Y-%m-%d")
  if ("index_key" %in% keys) got$index_key <- as.character(roster_frame$index_key[at])
  key <- function(df) do.call(paste, c(lapply(df[keys], as.character), sep = "|"))
  testthat::expect_setequal(key(got), key(want))
  got <- got[match(key(want), key(got)), , drop = FALSE]
  for (c in cols) {
    g <- got[[c]]
    w <- want[[c]]
    if (is.numeric(w) || is.numeric(g)) {
      testthat::expect_equal(as.numeric(g), as.numeric(w), tolerance = 1e-9, info = paste(info, c))
    } else {
      testthat::expect_identical(as.character(g), as.character(w), info = paste(info, c))
    }
  }
  invisible(got)
}
