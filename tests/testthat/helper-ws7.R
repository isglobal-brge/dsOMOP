# Shared fixtures for the WS-7 tests (design-2.10.3 sections 2.7, 2.9, 6.11,
# 6.12, 7.3): variables, reducers and outputs over real WS-6 populations.
# Scenario episode frames seed a scoped results-cohort fixture, then the
# production population compiler owns candidate selection, coverage, origin
# metadata, immutable roster tables and cleanup. Explicit synthetic rosters
# remain only for targeted impossible-origin and metadata-mutation probes.

.ws7_backends <- function() .ws5_backends()

# The acceptance oracle was frozen on the WS-0 reach fixture. WS-5 scenario
# rows must not enter these populations (notably its extra oncology episodes
# and matching candidates). Reuse the unextended WS-4 fixtures, adding MySQL
# with the same loader because WS-4 predates that vendor test connection.
.ws7_acceptance_backends <- function() {
  out <- .ws4_backends()
  config <- .dsomopVendorTestConfig("mysql")
  if (is.null(config) || !requireNamespace("RMariaDB", quietly = TRUE)) return(out)
  conn <- tryCatch(DBI::dbConnect(RMariaDB::MariaDB(), host = config$host, port = config$port,
                                  dbname = config$database, user = config$admin_user,
                                  password = config$admin_password), error = function(e) NULL)
  if (is.null(conn)) return(out)
  db <- .ws4_namespace("ws7_acceptance")
  ok <- tryCatch({
    .ensureMariaSession(conn, require_tls = FALSE)
    DBI::dbExecute(conn, paste0("CREATE DATABASE ", db))
    .ws4_copy_tables(out$sqlite$handle$conn, conn, db)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    try(DBI::dbExecute(conn, paste0("DROP DATABASE IF EXISTS ", db)), silent = TRUE)
    DBI::dbDisconnect(conn)
    return(out)
  }
  h <- .ws4_handle_on(conn, "mysql", "mysql", db)
  h$person_key <- out$sqlite$handle$person_key
  out$mysql <- list(handle = h, cleanup = function() {
    DBI::dbExecute(conn, paste0("DROP DATABASE IF EXISTS ", db))
    DBI::dbDisconnect(conn)
  })
  out
}

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
  on.exit({
    if (exists(run$ctx$id, .publicationRegistry, inherits = FALSE)) {
      rm(list = run$ctx$id, envir = .publicationRegistry)
    }
  }, add = TRUE)
  # A cleanup error is test evidence: do not swallow failed DROP operations.
  .planContextClose(run$ctx)
  invisible(NULL)
}

.ws7_table_seq <- new.env(parent = emptyenv())

# Temporarily replace one fixture cohort's source rows, restoring every
# original row after the caller finishes. This changes source data only:
# population compilation, coverage and row identities all remain production.
.ws7_seed_cohort <- function(handle, frame, cid = 1L) {
  schema <- handle$results_schema %||% handle$cdm_schema
  id <- if (is.null(schema) || identical(handle$dbms, "sqlite")) "cohort" else
    DBI::Id(schema = schema, table = "cohort")
  table <- .qualifyTable(handle, "cohort", schema)
  predicate <- paste0(" WHERE cohort_definition_id = ", as.integer(cid))
  original <- DBI::dbGetQuery(handle$conn, paste0("SELECT * FROM ", table, predicate))
  rows <- data.frame(cohort_definition_id = rep(as.integer(cid), nrow(frame)),
    subject_id = frame$subject_id,
    cohort_start_date = as.Date(frame$cohort_start_date),
    cohort_end_date = as.Date(frame$cohort_end_date))
  if (identical(handle$dbms, "sqlite")) {
    rows$cohort_start_date <- format(rows$cohort_start_date, "%Y-%m-%d")
    rows$cohort_end_date <- format(rows$cohort_end_date, "%Y-%m-%d")
  }
  restore <- function() {
    DBI::dbExecute(handle$conn, paste0("DELETE FROM ", table, predicate))
    if (nrow(original)) DBI::dbWriteTable(handle$conn, id, original, append = TRUE)
    invisible(NULL)
  }
  DBI::dbExecute(handle$conn, paste0("DELETE FROM ", table, predicate))
  tryCatch({
    if (nrow(rows)) DBI::dbWriteTable(handle$conn, id, rows, append = TRUE)
  }, error = function(e) {
    restore()
    stop(e)
  })
  restore
}

.ws7_population <- function(run, pop, frame) {
  if (is.null(frame)) return(.compilePopulationByName(run$cctx, pop))
  source <- run$cctx$ir$populations[[pop]]$index$source
  if (!identical(names(source), "cohort_definition_id")) {
    stop("Scenario source fixtures require a results-cohort index population.")
  }
  restore <- .ws7_seed_cohort(run$cctx$handle, frame, source$cohort_definition_id)
  on.exit(restore(), add = TRUE)
  .compilePopulationByName(run$cctx, pop)
}

# A deliberately synthetic roster for isolated association/metadata probes,
# never ordinary format or publication execution. The supplied frame has
# subject_id, cohort_start_date,
# cohort_end_date, index_source, index_key (+ index_visit_id, set_id,
# is_case, sampling_weight). The candidates are written as a test table, the
# population's coverage compiled on them, and the final roster -- the covered
# episodes under the policy, ends capped at op_end (event_end strategy),
# cohort_row_id ranked in the chronological identity order of section 6.9 --
# materialised as a session temporary table.
.ws7_population_double <- function(run, pop, frame, cap_end = FALSE) {
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
  testthat::expect_identical(anyDuplicated(as.character(roster_frame$cohort_row_id)), 0L, info = info)
  at <- match(as.character(frame$cohort_row_id), as.character(roster_frame$cohort_row_id))
  testthat::expect_false(anyNA(at), info = info)
  got <- frame
  got$person_id <- as.numeric(roster_frame$subject_id[at])
  got$index_date <- format(as.Date(roster_frame$cohort_start_date[at]), "%Y-%m-%d")
  if ("index_key" %in% keys) got$index_key <- as.character(roster_frame$index_key[at])
  key <- function(df) do.call(paste, c(lapply(df[keys], as.character), sep = "|"))
  # Set equality alone accepts duplicate output rows and can hide an invalid
  # join. Require the exact cardinality and a unique comparison key as well.
  testthat::expect_identical(nrow(got), nrow(want), info = info)
  testthat::expect_identical(anyDuplicated(key(got)), 0L, info = info)
  testthat::expect_identical(anyDuplicated(key(want)), 0L, info = info)
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

# --- the reducer scenario (persons 7001..7008, concepts 92200001..92200004) ----------
#
#   7001  lab rows 2020-01-01 (5; ranges 1..6), 01-05 (NULL, id 970002),
#         01-05 (7, id 970003), 01-10 (3; ranges 4..10), 01-20 (9; ranges 1..8);
#         dx rows 01-02 (visit 970101) x2, 01-03 (visit 970102), 01-04 (no
#         visit) and one dx2 row 01-03 (visit 970102); drug rows
#         [01-01, 01-10] and [01-05, 01-15] (overlapping)
#   7002  no rows
#   7003  lab rows 01-02 and 01-03 with NULL values
#   7004  lab rows 01-07 (1) and 01-13 (2): equidistant from the index
#   7005  lab 5 and 6 (every predicate row TRUE)
#   7006  lab 5 and NULL (TRUE and UNKNOWN)
#   7007  lab 3 and NULL (FALSE and UNKNOWN)
#   7008  no year of birth
# Every index is 2020-01-10.
.WS7 <- list(LAB = 92200001L, DX = 92200002L, DX2 = 92200003L, DRUG = 92200004L)

.ws7_extend <- function(conn) {
  W <- .WS7
  app <- function(name, df) DBI::dbWriteTable(conn, name, df, append = TRUE)
  pids <- 7001:7008
  app("person", data.frame(
    person_id = pids, gender_concept_id = c(8507L, 8532L, 8507L, 8532L, 8507L, 8532L, 8507L, 8532L),
    year_of_birth = c(1970L, 1980L, 1990L, 1960L, 1950L, 1940L, 2000L, NA),
    month_of_birth = 1L, day_of_birth = 1L, race_concept_id = 8527L,
    ethnicity_concept_id = 38003564L, location_id = pids, provider_id = pids,
    care_site_id = pids, person_source_value = paste0("WS7_", pids),
    gender_source_value = "M", stringsAsFactors = FALSE))
  app("observation_period", data.frame(
    observation_period_id = 700000L + pids, person_id = pids,
    observation_period_start_date = "2015-01-01", observation_period_end_date = "2030-12-31",
    period_type_concept_id = 44814724L, stringsAsFactors = FALSE))
  mid <- 970000L
  me <- function(pid, date, value, rl = NA_real_, rh = NA_real_) {
    mid <<- mid + 1L
    data.frame(measurement_id = mid, person_id = pid, measurement_concept_id = W$LAB,
               measurement_date = date, measurement_type_concept_id = 44818702L,
               operator_concept_id = 0L, value_as_number = value, value_as_concept_id = 0L,
               unit_concept_id = 8554L, range_low = rl, range_high = rh,
               measurement_source_value = "WS7", measurement_source_concept_id = 0L,
               unit_source_value = "%", stringsAsFactors = FALSE)
  }
  app("measurement", rbind(
    me(7001L, "2020-01-01", 5, 1, 6), me(7001L, "2020-01-05", NA), me(7001L, "2020-01-05", 7),
    me(7001L, "2020-01-10", 3, 4, 10), me(7001L, "2020-01-20", 9, 1, 8),
    me(7003L, "2020-01-02", NA), me(7003L, "2020-01-03", NA),
    me(7004L, "2020-01-07", 1), me(7004L, "2020-01-13", 2),
    me(7005L, "2020-01-08", 5), me(7005L, "2020-01-09", 6),
    me(7006L, "2020-01-08", 5), me(7006L, "2020-01-09", NA),
    me(7007L, "2020-01-08", 3), me(7007L, "2020-01-09", NA)))
  app("visit_occurrence", data.frame(
    visit_occurrence_id = c(970101L, 970102L), person_id = 7001L, visit_concept_id = 9202L,
    visit_start_date = c("2020-01-02", "2020-01-03"), visit_end_date = c("2020-01-02", "2020-01-03"),
    visit_type_concept_id = 44818518L, visit_source_value = "V", visit_source_concept_id = 0L,
    stringsAsFactors = FALSE))
  cid <- 970000L
  co <- function(pid, concept, date, visit) {
    cid <<- cid + 1L
    data.frame(condition_occurrence_id = cid, person_id = pid, condition_concept_id = concept,
               condition_start_date = date, condition_end_date = date,
               condition_type_concept_id = 44818518L, stop_reason = NA_character_,
               provider_id = pid, visit_occurrence_id = visit, condition_source_value = "WS7",
               condition_source_concept_id = 0L, stringsAsFactors = FALSE)
  }
  app("condition_occurrence", rbind(
    co(7001L, W$DX, "2020-01-02", 970101L), co(7001L, W$DX, "2020-01-02", 970101L),
    co(7001L, W$DX, "2020-01-03", 970102L), co(7001L, W$DX, "2020-01-04", NA_integer_),
    co(7001L, W$DX2, "2020-01-03", 970102L)))
  did <- 970000L
  de <- function(pid, start, end) {
    did <<- did + 1L
    data.frame(drug_exposure_id = did, person_id = pid, drug_concept_id = W$DRUG,
               drug_exposure_start_date = start, drug_exposure_end_date = end,
               drug_type_concept_id = 38000177L, stop_reason = NA_character_, refills = 0L,
               days_supply = NA_integer_, sig = NA_character_, route_concept_id = 0L,
               lot_number = NA_character_, dose_unit_concept_id = 0L,
               drug_source_value = "WS7", drug_source_concept_id = 0L, stringsAsFactors = FALSE)
  }
  app("drug_exposure", rbind(de(7001L, "2020-01-01", "2020-01-10"), de(7001L, "2020-01-05", "2020-01-15")))
  # survival scenario (index 2020-01-10, cohort end 2020-06-30; outcomes
  # a = DX, b = DX2):
  #   7101 a and b on 02-01, a on 03-01      7102 b on 01-20, a on 02-10
  #   7103 a on 2019-12-01 (prior outcome)    7104 observation ends 2020-03-31
  #   7105 periods [2015, 2020-02-29] and [2020-03-10, 2030]; a on 03-05 (in
  #        the gap) and 03-20                 7106 a on 06-30 (the cohort end)
  #   7107 a on 2020-08-01 (after the cohort end)
  sp <- 7101:7107
  app("person", data.frame(
    person_id = sp, gender_concept_id = 8507L, year_of_birth = 1970L, month_of_birth = 1L,
    day_of_birth = 1L, race_concept_id = 8527L, ethnicity_concept_id = 38003564L,
    location_id = sp, provider_id = sp, care_site_id = sp, person_source_value = paste0("WS7_", sp),
    gender_source_value = "M", stringsAsFactors = FALSE))
  app("observation_period", data.frame(
    observation_period_id = c(710101L, 710201L, 710301L, 710401L, 710501L, 710502L, 710601L, 710701L),
    person_id = c(7101L, 7102L, 7103L, 7104L, 7105L, 7105L, 7106L, 7107L),
    observation_period_start_date = c("2015-01-01", "2015-01-01", "2015-01-01", "2015-01-01", "2015-01-01",
                                      "2020-03-10", "2015-01-01", "2015-01-01"),
    observation_period_end_date = c("2030-12-31", "2030-12-31", "2030-12-31", "2020-03-31", "2020-02-29",
                                    "2030-12-31", "2030-12-31", "2030-12-31"),
    period_type_concept_id = 44814724L, stringsAsFactors = FALSE))
  app("condition_occurrence", rbind(
    co(7101L, W$DX, "2020-02-01", NA_integer_), co(7101L, W$DX2, "2020-02-01", NA_integer_),
    co(7101L, W$DX, "2020-03-01", NA_integer_),
    co(7102L, W$DX2, "2020-01-20", NA_integer_), co(7102L, W$DX, "2020-02-10", NA_integer_),
    co(7103L, W$DX, "2019-12-01", NA_integer_),
    co(7105L, W$DX, "2020-03-05", NA_integer_), co(7105L, W$DX, "2020-03-20", NA_integer_),
    co(7106L, W$DX, "2020-06-30", NA_integer_), co(7107L, W$DX, "2020-08-01", NA_integer_)))
  concepts <- data.frame(
    concept_id = c(W$LAB, W$DX, W$DX2, W$DRUG),
    concept_name = c("ws7 lab", "ws7 condition", "ws7 condition 2", "ws7 drug"),
    domain_id = c("Measurement", "Condition", "Condition", "Drug"),
    vocabulary_id = c("LOINC", "SNOMED", "SNOMED", "RxNorm"),
    concept_class_id = c("Lab Test", "Clinical Finding", "Clinical Finding", "Clinical Drug"),
    standard_concept = "S", concept_code = paste0("WS7", 1:4), valid_start_date = "1970-01-01",
    valid_end_date = "2099-12-31", invalid_reason = NA_character_, stringsAsFactors = FALSE)
  app("concept", concepts)
  app("concept_ancestor", data.frame(ancestor_concept_id = concepts$concept_id,
                                     descendant_concept_id = concepts$concept_id,
                                     min_levels_of_separation = 0L, max_levels_of_separation = 0L))
  invisible(conn)
}

# Every executable backend with the WS-5 and WS-7 scenario rows.
.ws7_scenario_backends <- function() {
  sqlite <- .ws4_reach_handle()
  .ws5_extend(sqlite$conn)
  .ws7_extend(sqlite$conn)
  sqlite$blueprint <- NULL
  sqlite$role_registry <- NULL
  out <- list(sqlite = list(handle = sqlite, cleanup = function() {
    try(DBI::dbDisconnect(sqlite$conn), silent = TRUE)
  }))
  copy <- function(name, conn, dbms, dialect, schema, drop) {
    ok <- tryCatch({ .ws4_copy_tables(sqlite$conn, conn, if (identical(dialect, "duckdb")) NULL else schema); TRUE },
                   error = function(e) FALSE)
    if (!ok) { drop(); return(NULL) }
    h <- .ws4_handle_on(conn, dbms, dialect, schema)
    h$person_key <- sqlite$person_key
    list(handle = h, cleanup = drop)
  }
  if (requireNamespace("duckdb", quietly = TRUE)) {
    dk <- tryCatch(DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:"), error = function(e) NULL)
    if (!is.null(dk)) {
      b <- copy("duckdb", dk, "duckdb", "duckdb", "main",
                function() try(DBI::dbDisconnect(dk, shutdown = TRUE), silent = TRUE))
      if (!is.null(b)) out$duckdb <- b
    }
  }
  pg <- .ws4_pg_connect()
  if (!is.null(pg)) {
    schema <- .ws4_namespace("ws7")
    made <- tryCatch({
      DBI::dbExecute(pg, "SET client_min_messages TO WARNING")
      DBI::dbExecute(pg, paste0("CREATE SCHEMA ", schema))
      TRUE
    }, error = function(e) FALSE)
    if (made) {
      b <- copy("postgresql", pg, "postgresql", "postgresql", schema, function() {
        try(DBI::dbExecute(pg, paste0("DROP SCHEMA ", schema, " CASCADE")), silent = TRUE)
        try(DBI::dbDisconnect(pg), silent = TRUE)
      })
      if (!is.null(b)) out$postgresql <- b
    }
  }
  for (dbms in c("mariadb", "mysql")) {
    config <- .dsomopVendorTestConfig(dbms)
    if (is.null(config) || !requireNamespace("RMariaDB", quietly = TRUE)) next
    my <- tryCatch(DBI::dbConnect(RMariaDB::MariaDB(), host = config$host, port = config$port,
                                  dbname = config$database, user = config$admin_user,
                                  password = config$admin_password), error = function(e) NULL)
    if (is.null(my)) next
    db <- .ws4_namespace("ws7")
    made <- tryCatch({
      .ensureMariaSession(my, require_tls = FALSE)
      DBI::dbExecute(my, paste0("CREATE DATABASE ", db))
      TRUE
    }, error = function(e) FALSE)
    if (!made) { try(DBI::dbDisconnect(my), silent = TRUE); next }
    b <- local({
      conn <- my
      name <- db
      copy(dbms, conn, dbms, "mysql", name, function() {
        try(DBI::dbExecute(conn, paste0("DROP DATABASE IF EXISTS ", name)), silent = TRUE)
        try(DBI::dbDisconnect(conn), silent = TRUE)
      })
    })
    if (!is.null(b)) out[[dbms]] <- b
  }
  out
}

# A language-2.0 plan over the scenario: events lab, dx (two concepts), drug,
# the index population `t` (rows from a double), one wide output.
.ws7_scenario_plan <- function(variables, output = list(), events = list(), populations = list()) {
  W <- .WS7
  plan <- list(
    version = 2L,
    meta = list(language_version = "2.0", semantic_version = "2.0.0", generated_by = "ws7-test"),
    events = c(list(
      lab = list(table = "measurement", concept_set = list(concepts = list(W$LAB)),
                 value = list(column = "value_as_number")),
      dx = list(table = "condition_occurrence", concept_set = list(concepts = list(W$DX, W$DX2))),
      drug = list(table = "drug_exposure", concept_set = list(concepts = list(W$DRUG)))), events),
    populations = c(list(t = list(kind = "index", index = list(source = list(cohort_definition_id = 1L),
                                                                select = "all",
                                                                end = list(strategy = "event_end")))),
                    populations),
    variables = variables,
    outputs = list(o = local({
      o <- list(rows = "t", format = "wide", grain = "episode")
      o[names(output)] <- output
      o
    })))
  plan
}

.ws7_scenario_frame <- function(pids = 7001:7008, index = "2020-01-10", end = "2020-12-31") {
  data.frame(subject_id = pids, cohort_start_date = as.Date(index), cohort_end_date = as.Date(end),
             index_source = "cohort:1", index_key = paste0("1:", pids, ":", index, ":", end),
             stringsAsFactors = FALSE)
}

# Compile output `o` of a scenario plan on one backend; the frame by person.
.ws7_scenario_run <- function(handle, plan, frame = .ws7_scenario_frame(), output = "o",
                              population = .ws7_population) {
  run <- .ws7_ctx(handle, plan)
  on.exit(.ws7_close(run), add = TRUE)
  pop <- population(run, "t", frame)
  compiled <- .planCompileOutputs(run$cctx, stats::setNames(list("D"), output),
                                  populations = list(t = pop))
  roster <- .reduceRosterFrame(run$cctx, pop$roster)
  comp <- compiled[[output]]
  main <- comp$components[[output]]$frame
  if (!is.null(main) && "cohort_row_id" %in% names(main)) {
    main$pid <- as.numeric(roster$subject_id[match(as.character(main$cohort_row_id),
                                                   as.character(roster$cohort_row_id))])
  } else if (!is.null(main) && "person_id" %in% names(main)) {
    main$pid <- as.numeric(main$person_id)
  }
  list(frame = main, compiled = comp, roster = roster)
}

.ws7_value <- function(frame, pid, col) frame[[col]][match(pid, frame$pid)]
