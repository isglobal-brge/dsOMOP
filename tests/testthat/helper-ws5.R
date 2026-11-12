# Shared fixtures for the WS-5 tests (design-2.10.3 sections 6.7, 6.8, 6.13):
# intervals/eras, anchors/windows and observation-period coverage on every
# executable backend available here (SQLite always, DuckDB when installed,
# PostgreSQL and MariaDB through the opt-in vendor variables), on the extended
# reach fixture plus the WS-5 scenario rows below (persons 3001..3205; new
# concept ids 92000001..92100002 registered with their vocabulary rows).
#
# Era scenarios (drug_exposure, drug concept ERA_DRUG unless stated):
#   3001  counterexample: records [2020-01-01,02] and [2020-01-10,11], gap 30
#   3002  nested [01-01,01-31] + [01-05,01-10] + [01-20,01-25] (running max)
#   3003  adjacent [01-01,01-10] + [01-11,01-20], then [01-22,01-25]
#   3004  one-day record 2020-03-01
#   3005  NULL end with supply 10 (2020-01-01)
#   3006  recorded [2020-01-01, 2020-01-31] (end = "start" override)
#   3007  NULL end with supply 0 and supply -5 (invalid supply, only end source)
#   3008  [01-01,01-10] + [02-20,02-25] (persistence 15 joins them)
#   3009  ingredient grouping: DRUG_A (-> ING_A) [01-01,01-10], DRUG_AB
#         (-> ING_A, ING_B) [01-05,01-20], DRUG_X (unmapped) [01-03,01-04]
#   3010  pre-window carry-in: supply 30 on 2019-12-01 and 2019-12-20
#   3011  cap binding: supply 200 on 2020-01-01, 10 on 01-02, 10 on 06-01
#   3012  supply fills: 2020-01-05 (10), 2019-12-25 (20), 2020-01-20 (0),
#         2020-01-25 (NULL)
#   3013  observation ends at the index (2019-12-31); fill 2020-01-05 (10)
#   3015  recorded ends with supply 0 ([01-01,01-05]), -5 ([02-01,02-03]) and
#         NULL ([03-01,03-02]): every row kept by its recorded end (10 days)
#   3016  NULL end and NULL supply on 2020-01-01: a one-day record
#   3020..3022  drug_era parity: ingredient-mapped fills and the matching
#         drug_era rows (same mapping, end inference, gap 30, persistence 0)
# Adapter association (episode members, concept EP_DRUG; A-20 item 6):
#   3301  episodes E1 = 3301001 [2020-01-01, 2020-02-29] and E2 = 3301002
#         [2020-01-01, 2020-03-31]; fills F1 2020-01-01 (30, linked to both),
#         F2 2020-01-15 (30, E1 only), F3 2020-02-10 (30, E2 only). Person
#         scope: recorded [01-01, 03-10], effective (stockpiled) [01-01,
#         03-30]; E1 recorded 44 / effective 60 days; E2 recorded 60 /
#         effective 60 (no carry from E1's F2 into F3).
# Coverage scenarios (observation_period; roster ws5_roster):
#   3101  two overlapping periods covering the index (ambiguous)
#   3102  periods with exactly 8 uncovered days (gap 7 apart, gap 8 merged)
#   3103  records [2020-01-01,02] and [2020-01-10,11], index 2020-01-05
#   3104  no observation period
#   3105  one period covering the index
# Anchor scenarios (condition_occurrence ANCHOR_A, measurement ANCHOR_B):
#   3201  A rows 2019-06-01..06-10, 2019-12-01..2020-02-15, 2020-03-01..03-05
#         and the tie 2020-03-01..03-20; B rows 2019-05-01, 2019-07-01
#   3202  no rows;  3203  one A row 2020-06-01 outside its observed span
#         (observation 2015-01-01 .. 2020-03-31)

.WS5 <- list(ERA_DRUG = 92000021L, ING_A = 92000001L, ING_B = 92000002L,
             DRUG_A = 92000011L, DRUG_AB = 92000012L, DRUG_X = 92000013L,
             PAR_DRUG1 = 92000014L, PAR_DRUG2 = 92000015L, EP_DRUG = 92000016L,
             ANCHOR_A = 92100001L, ANCHOR_B = 92100002L)

.ws5_extend <- function(conn) {
  W <- .WS5
  app <- function(name, df) DBI::dbWriteTable(conn, name, df, append = TRUE)
  pids <- c(3001:3013, 3015:3016, 3020:3022, 3101:3105, 3201:3203, 3301)
  app("person", data.frame(
    person_id = pids, gender_concept_id = 8507L, year_of_birth = 1970L,
    month_of_birth = 1L, day_of_birth = 1L, race_concept_id = 8527L,
    ethnicity_concept_id = 38003564L, location_id = pids, provider_id = pids,
    care_site_id = pids, person_source_value = paste0("WS5_", pids),
    gender_source_value = "M", stringsAsFactors = FALSE))
  op <- function(id, pid, s, e) data.frame(
    observation_period_id = id, person_id = pid, observation_period_start_date = s,
    observation_period_end_date = e, period_type_concept_id = 44814724L,
    stringsAsFactors = FALSE)
  plain <- c(3001:3012, 3015:3016, 3020:3022, 3105, 3201:3202, 3301)
  app("observation_period", rbind(
    op(300000L + plain, plain, "2015-01-01", "2030-12-31"),
    op(301301L, 3013L, "2015-01-01", "2019-12-31"),
    op(310101L, 3101L, "2015-01-01", "2020-06-30"), op(310102L, 3101L, "2019-06-01", "2030-12-31"),
    op(310201L, 3102L, "2015-01-01", "2019-12-23"), op(310202L, 3102L, "2020-01-01", "2030-12-31"),
    op(310301L, 3103L, "2020-01-01", "2020-01-02"), op(310302L, 3103L, "2020-01-10", "2020-01-11"),
    op(320301L, 3203L, "2015-01-01", "2020-03-31")))
  id <- 930000L
  de <- function(pid, concept, start, end = NA_character_, supply = NA_integer_) {
    id <<- id + 1L
    data.frame(drug_exposure_id = id, person_id = pid, drug_concept_id = concept,
               drug_exposure_start_date = start, drug_exposure_end_date = end,
               drug_type_concept_id = 38000177L, stop_reason = NA_character_, refills = 0L,
               days_supply = supply, sig = NA_character_, route_concept_id = 0L,
               lot_number = NA_character_, dose_unit_concept_id = 0L,
               drug_source_value = "WS5", drug_source_concept_id = 0L, stringsAsFactors = FALSE)
  }
  E <- W$ERA_DRUG
  drugs <- rbind(
    de(3001L, E, "2020-01-01", "2020-01-02"), de(3001L, E, "2020-01-10", "2020-01-11"),
    de(3002L, E, "2020-01-01", "2020-01-31"), de(3002L, E, "2020-01-05", "2020-01-10"),
    de(3002L, E, "2020-01-20", "2020-01-25"),
    de(3003L, E, "2020-01-01", "2020-01-10"), de(3003L, E, "2020-01-11", "2020-01-20"),
    de(3003L, E, "2020-01-22", "2020-01-25"),
    de(3004L, E, "2020-03-01", "2020-03-01"),
    de(3005L, E, "2020-01-01", supply = 10L),
    de(3006L, E, "2020-01-01", "2020-01-31"),
    de(3007L, E, "2020-01-01", supply = 0L), de(3007L, E, "2020-02-01", supply = -5L),
    de(3008L, E, "2020-01-01", "2020-01-10"), de(3008L, E, "2020-02-20", "2020-02-25"),
    de(3009L, W$DRUG_A, "2020-01-01", "2020-01-10"), de(3009L, W$DRUG_AB, "2020-01-05", "2020-01-20"),
    de(3009L, W$DRUG_X, "2020-01-03", "2020-01-04"),
    de(3010L, E, "2019-12-01", supply = 30L), de(3010L, E, "2019-12-20", supply = 30L),
    de(3011L, E, "2020-01-01", supply = 200L), de(3011L, E, "2020-01-02", supply = 10L),
    de(3011L, E, "2020-06-01", supply = 10L),
    de(3012L, E, "2020-01-05", supply = 10L), de(3012L, E, "2019-12-25", supply = 20L),
    de(3012L, E, "2020-01-20", supply = 0L), de(3012L, E, "2020-01-25"),
    de(3013L, E, "2020-01-05", supply = 10L),
    de(3015L, E, "2020-01-01", "2020-01-05", supply = 0L),
    de(3015L, E, "2020-02-01", "2020-02-03", supply = -5L),
    de(3015L, E, "2020-03-01", "2020-03-02"),
    de(3016L, E, "2020-01-01"),
    de(3301L, W$EP_DRUG, "2020-01-01", supply = 30L), de(3301L, W$EP_DRUG, "2020-01-15", supply = 30L),
    de(3301L, W$EP_DRUG, "2020-02-10", supply = 30L),
    # drug_era parity: fills normalised as the era definition does (recorded end,
    # else start + supply - 1, else start); ingredient A for both drugs
    de(3020L, W$PAR_DRUG1, "2020-01-01", supply = 30L), de(3020L, W$PAR_DRUG2, "2020-02-15", supply = 30L),
    de(3020L, W$PAR_DRUG1, "2020-05-01", "2020-05-10"),
    de(3021L, W$PAR_DRUG2, "2020-03-01", supply = 10L), de(3021L, W$PAR_DRUG2, "2020-04-10", supply = 5L),
    de(3022L, W$PAR_DRUG1, "2020-01-01", "2020-01-31"), de(3022L, W$PAR_DRUG1, "2020-01-10", "2020-01-15"))
  app("drug_exposure", drugs)
  ep_fills <- drugs$drug_exposure_id[drugs$person_id == 3301L]
  app("episode", data.frame(
    episode_id = c(3301001L, 3301002L), person_id = 3301L, episode_concept_id = 32531L,
    episode_start_date = "2020-01-01", episode_end_date = c("2020-02-29", "2020-03-31"),
    stringsAsFactors = FALSE))
  app("episode_event", data.frame(
    episode_id = c(3301001L, 3301002L, 3301001L, 3301002L),
    event_id = ep_fills[c(1L, 1L, 2L, 3L)], episode_event_field_concept_id = 1147094L,
    stringsAsFactors = FALSE))
  # Matched drug_era rows: ingredient A, gap 30, persistence 0.
  #   3020: [01-01,01-30] + [02-15,03-15] merge (02-15 <= 01-30 + 31); 05-01 > 03-15 + 31
  #   3021: [03-01,03-10] + [04-10,04-14] merge (04-10 <= 03-10 + 31)
  #   3022: nested -> [01-01, 01-31]
  app("drug_era", data.frame(
    drug_era_id = 930001:930004, person_id = c(3020L, 3020L, 3021L, 3022L),
    drug_concept_id = W$ING_A,
    drug_era_start_date = c("2020-01-01", "2020-05-01", "2020-03-01", "2020-01-01"),
    drug_era_end_date = c("2020-03-15", "2020-05-10", "2020-04-14", "2020-01-31"),
    drug_exposure_count = c(2L, 1L, 2L, 2L), gap_days = 0L, stringsAsFactors = FALSE))
  cid <- 930000L
  co <- function(pid, concept, start, end = start) {
    cid <<- cid + 1L
    data.frame(condition_occurrence_id = cid, person_id = pid, condition_concept_id = concept,
               condition_start_date = start, condition_end_date = end,
               condition_type_concept_id = 44818518L, stop_reason = NA_character_,
               provider_id = pid, visit_occurrence_id = NA_integer_,
               condition_source_value = "WS5", condition_source_concept_id = 0L,
               stringsAsFactors = FALSE)
  }
  A <- W$ANCHOR_A
  app("condition_occurrence", rbind(
    co(3201L, A, "2019-06-01", "2019-06-10"), co(3201L, A, "2019-12-01", "2020-02-15"),
    co(3201L, A, "2020-03-01", "2020-03-05"), co(3201L, A, "2020-03-01", "2020-03-20"),
    co(3203L, A, "2020-06-01", "2020-06-02")))
  mid <- 930000L
  me <- function(pid, concept, date) {
    mid <<- mid + 1L
    data.frame(measurement_id = mid, person_id = pid, measurement_concept_id = concept,
               measurement_date = date, measurement_type_concept_id = 44818702L,
               operator_concept_id = 0L, value_as_number = 1, value_as_concept_id = 0L,
               unit_concept_id = 8554L, range_low = NA_real_, range_high = NA_real_,
               measurement_source_value = "WS5", measurement_source_concept_id = 0L,
               unit_source_value = "u", stringsAsFactors = FALSE)
  }
  app("measurement", rbind(me(3201L, W$ANCHOR_B, "2019-05-01"), me(3201L, W$ANCHOR_B, "2019-07-01")))
  concepts <- data.frame(
    concept_id = c(W$ING_A, W$ING_B, W$DRUG_A, W$DRUG_AB, W$DRUG_X, W$ERA_DRUG, W$PAR_DRUG1,
                   W$PAR_DRUG2, W$EP_DRUG, W$ANCHOR_A, W$ANCHOR_B),
    concept_name = c("ws5 ingredient A", "ws5 ingredient B", "ws5 drug A", "ws5 drug A+B",
                     "ws5 unmapped drug", "ws5 era drug", "ws5 parity drug 1", "ws5 parity drug 2",
                     "ws5 episode drug", "ws5 anchor condition", "ws5 anchor measurement"),
    domain_id = c(rep("Drug", 9), "Condition", "Measurement"),
    vocabulary_id = c("RxNorm", "RxNorm", "RxNorm", "RxNorm", "RxNorm", "RxNorm", "RxNorm",
                      "RxNorm", "RxNorm", "SNOMED", "LOINC"),
    concept_class_id = c("Ingredient", "Ingredient", "Clinical Drug", "Clinical Drug",
                         "Clinical Drug", "Clinical Drug", "Clinical Drug", "Clinical Drug",
                         "Clinical Drug", "Clinical Finding", "Lab Test"),
    standard_concept = "S", concept_code = paste0("WS5", 1:11),
    valid_start_date = "1970-01-01", valid_end_date = "2099-12-31",
    invalid_reason = NA_character_, stringsAsFactors = FALSE)
  app("concept", concepts)
  app("concept_ancestor", rbind(
    data.frame(ancestor_concept_id = concepts$concept_id, descendant_concept_id = concepts$concept_id,
               min_levels_of_separation = 0L, max_levels_of_separation = 0L),
    data.frame(ancestor_concept_id = c(W$ING_A, W$ING_A, W$ING_B, W$ING_A, W$ING_A),
               descendant_concept_id = c(W$DRUG_A, W$DRUG_AB, W$DRUG_AB, W$PAR_DRUG1, W$PAR_DRUG2),
               min_levels_of_separation = 1L, max_levels_of_separation = 1L)))
  # A roster for coverage/anchor tests: candidates keyed by cand_key.
  DBI::dbWriteTable(conn, "ws5_roster", data.frame(
    cand_key = 1:10,
    subject_id = c(3101L, 3102L, 3103L, 3104L, 3105L, 3201L, 3202L, 3203L, 3201L, 1003L),
    cohort_start_date = c("2020-01-10", "2020-01-10", "2020-01-05", "2020-01-10", "2020-01-10",
                          "2020-01-01", "2020-01-01", "2020-01-01", "2020-03-02", "2019-12-31"),
    cohort_end_date = c("2020-12-31", "2020-12-31", "2020-12-31", "2020-12-31", "2020-12-31",
                        "2020-12-31", "2020-12-31", "2020-12-31", "2020-12-31", "2020-12-31"),
    stringsAsFactors = FALSE), overwrite = TRUE)
  # Episodes at index 2019-12-31 for the coverage-reducer tests.
  DBI::dbWriteTable(conn, "ws5_cov_roster", data.frame(
    cand_key = 1:4, subject_id = c(3012L, 3013L, 1003L, 3010L),
    cohort_start_date = "2019-12-31", cohort_end_date = "2020-12-31",
    stringsAsFactors = FALSE), overwrite = TRUE)
  invisible(conn)
}

.ws5_backend_cache <- new.env(parent = emptyenv())

#' The WS-5 fixture on every executable backend available (built once per
#' test file; callers clean up with .ws4_cleanup()). SQLite is always the
#' source; `vendors` selects the other backends (each only when available or
#' configured through its DSOMOP_TEST_<DBMS>_* variables).
.ws5_backends <- function(vendors = c("duckdb", "postgresql", "mariadb", "mysql")) {
  sqlite <- .ws4_reach_handle()
  .ws5_extend(sqlite$conn)
  sqlite$blueprint <- NULL
  sqlite$role_registry <- NULL
  out <- list(sqlite = list(handle = sqlite, cleanup = function() {
    try(DBI::dbDisconnect(sqlite$conn), silent = TRUE)
  }))
  if ("duckdb" %in% vendors && requireNamespace("duckdb", quietly = TRUE)) {
    dk <- tryCatch(DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:"), error = function(e) NULL)
    if (!is.null(dk)) {
      .ws4_copy_tables(sqlite$conn, dk)
      h <- .ws4_handle_on(dk, "duckdb", "duckdb", "main")
      h$person_key <- sqlite$person_key
      out$duckdb <- list(handle = h, cleanup = function() {
        try(DBI::dbDisconnect(dk, shutdown = TRUE), silent = TRUE)
      })
    }
  }
  pg <- if ("postgresql" %in% vendors) .ws4_pg_connect()
  if (!is.null(pg)) {
    schema <- .ws4_namespace("ws5")
    ok <- tryCatch({
      DBI::dbExecute(pg, "SET client_min_messages TO WARNING")
      DBI::dbExecute(pg, paste0("CREATE SCHEMA ", schema))
      .ws4_copy_tables(sqlite$conn, pg, schema)
      TRUE
    }, error = function(e) FALSE)
    if (ok) {
      h <- .ws4_handle_on(pg, "postgresql", "postgresql", schema)
      h$person_key <- sqlite$person_key
      out$postgresql <- list(handle = h, cleanup = function() {
        try(DBI::dbExecute(pg, paste0("DROP SCHEMA ", schema, " CASCADE")), silent = TRUE)
        try(DBI::dbDisconnect(pg), silent = TRUE)
      })
    } else {
      try(DBI::dbDisconnect(pg), silent = TRUE)
    }
  }
  for (dbms in intersect(c("mariadb", "mysql"), vendors)) {
    config <- .dsomopVendorTestConfig(dbms)
    if (is.null(config) || !requireNamespace("RMariaDB", quietly = TRUE)) next
    my <- tryCatch(DBI::dbConnect(RMariaDB::MariaDB(), host = config$host, port = config$port,
                                  dbname = config$database, user = config$admin_user,
                                  password = config$admin_password),
                   error = function(e) NULL)
    if (is.null(my)) next
    db <- .ws4_namespace("ws5")
    ok <- tryCatch({
      .ensureMariaSession(my, require_tls = FALSE)
      DBI::dbExecute(my, paste0("CREATE DATABASE ", db))
      .ws4_copy_tables(sqlite$conn, my, db)
      TRUE
    }, error = function(e) FALSE)
    if (ok) {
      h <- .ws4_handle_on(my, dbms, "mysql", db)
      h$person_key <- sqlite$person_key
      out[[dbms]] <- local({
        conn <- my
        name <- db
        list(handle = h, cleanup = function() {
          try(DBI::dbExecute(conn, paste0("DROP DATABASE IF EXISTS ", name)), silent = TRUE)
          try(DBI::dbDisconnect(conn), silent = TRUE)
        })
      })
    } else {
      try(DBI::dbExecute(my, paste0("DROP DATABASE IF EXISTS ", db)), silent = TRUE)
      try(DBI::dbDisconnect(my), silent = TRUE)
    }
  }
  out
}

# A language-2.0 plan around events, intervals and populations (schema-valid;
# the criteria population `all` and its wide output are always present). The
# populations, intervals and anchors a test compiles must be declared here:
# fragment identifiers are allocated from the IR (amendment A-17).
.ws5_plan <- function(events = list(), intervals = list(), populations = NULL,
                      variables = NULL, outputs = NULL) {
  plan <- .ws4_plan(events = events)
  if (length(intervals)) plan$intervals <- intervals
  if (!is.null(populations)) plan$populations <- c(plan$populations, populations)
  if (!is.null(variables)) plan$variables <- variables
  if (!is.null(outputs)) plan$outputs <- c(plan$outputs, outputs)
  plan
}

# A criteria population whose criteria use each anchor once (so that the
# anchors are in its candidate inventory).
.ws5_anchor_population <- function(anchors, coverage = NULL, source = "bb") {
  pop <- list(kind = "criteria", where = list(and = lapply(unname(anchors), function(a) {
    list(has = list(source = list(event = source), window = list(start = 0L, end = 0L, anchor = a)))
  })))
  if (!is.null(coverage)) pop$coverage <- coverage
  pop
}

.ws5_drug_event <- function(concepts = .WS5$ERA_DRUG, end = NULL) {
  ev <- list(table = "drug_exposure", concept_set = list(concepts = as.list(concepts)))
  if (!is.null(end)) ev$end <- end
  ev
}

# Rows of a fragment, dates as ISO text, ordered.
.ws5_rows <- function(ctx, f, order) {
  rows <- .fetchFragment(ctx, f, order)
  for (col in names(rows)) if (inherits(rows[[col]], "Date")) rows[[col]] <- format(rows[[col]], "%Y-%m-%d")
  for (col in intersect(c("person_id", "event_id", "row_key", "group_id", "era_no", "n_events",
                          "component_id", "op_count", "concept_id", "period_id", "carry",
                          "supply_days", "supply", "cand_key", "cohort_row_id", "subject_id"),
                        names(rows))) {
    rows[[col]] <- as.numeric(rows[[col]])
  }
  rows
}

# The candidates fragment over ws5_roster (or a results cohort).
.ws5_candidates <- function(ctx, cohort_definition_id = NULL, name = "cand_t") {
  h <- .compileDialect(ctx)
  handle <- ctx$handle
  body <- if (is.null(cohort_definition_id)) {
    paste0("SELECT ", .sqlLosslessKey(h, "w.cand_key"), " AS cand_key, w.subject_id AS subject_id, ",
           "w.cohort_start_date AS cohort_start_date, w.cohort_end_date AS cohort_end_date FROM ",
           .qualifyTable(handle, "ws5_roster", handle$cdm_schema), " w")
  } else {
    rn <- .sqlRowNumber(h, character(0), c("c.subject_id", "c.cohort_start_date", "c.cohort_end_date"))
    paste0("SELECT ", .sqlLosslessKey(h, rn), " AS cand_key, c.subject_id AS subject_id, ",
           "c.cohort_start_date AS cohort_start_date, c.cohort_end_date AS cohort_end_date FROM ",
           .qualifyTable(handle, "cohort", handle$cdm_schema), " c WHERE c.cohort_definition_id = ",
           .sqlIntegerLiteral(h, cohort_definition_id))
  }
  f <- .fragment(name, "candidates", "episode", body,
                 columns = list(cand_key = .fragmentColumn("bigint", FALSE, "episode_key"),
                                subject_id = .fragmentColumn("integer", FALSE, "person_key"),
                                cohort_start_date = .fragmentColumn("date", FALSE, "start"),
                                cohort_end_date = .fragmentColumn("date", TRUE, "end")),
                 unique_key = "cand_key",
                 person_lineage = list(person_col = "subject_id", via_roster = NULL),
                 private_columns = "cand_key")
  attr(f, "population") <- sub("^cand_", "", name)
  .fragmentRegister(ctx, f)
}

# Candidates with their coverage lookup attached (every candidate kept; NULL
# bounds when not uniquely covered), as the candidate stage holds them.
.ws5_covered_candidates <- function(ctx, cand, cov, name = "cand_cc") {
  body <- paste0("SELECT c.cand_key AS cand_key, c.subject_id AS subject_id, ",
                 "c.cohort_start_date AS cohort_start_date, c.cohort_end_date AS cohort_end_date, ",
                 "o.op_start AS op_start, o.op_end AS op_end, o.component_id AS op_component_id FROM ",
                 cand$name, " c INNER JOIN ", cov$coverage$name, " o ON o.row_key = c.cand_key")
  f <- .fragment(name, "candidates", "episode", body,
                 columns = list(cand_key = .fragmentColumn("bigint", FALSE, "episode_key"),
                                subject_id = .fragmentColumn("integer", FALSE, "person_key"),
                                cohort_start_date = .fragmentColumn("date", FALSE, "start"),
                                cohort_end_date = .fragmentColumn("date", TRUE, "end"),
                                op_start = .fragmentColumn("date", TRUE, "start"),
                                op_end = .fragmentColumn("date", TRUE, "end"),
                                op_component_id = .fragmentColumn("bigint", TRUE, "private")),
                 dependencies = c(cand$name, cov$coverage$name), unique_key = "cand_key",
                 person_lineage = list(person_col = "subject_id", via_roster = NULL),
                 private_columns = c("cand_key", "op_component_id"))
  attr(f, "population") <- sub("^cand_", "", name)
  .fragmentRegister(ctx, f)
}

# A final roster from candidates + their coverage (policy applied); the index
# end is capped at op_end when covered (event_end strategy).
.ws5_roster <- function(ctx, cand, cov, pop = "t") {
  h <- .compileDialect(ctx)
  keep <- .coverageKeepSql(cov$policy, "o")
  end <- paste0("CASE WHEN o.op_end IS NOT NULL AND c.cohort_end_date > o.op_end THEN o.op_end ",
                "ELSE c.cohort_end_date END")
  body <- paste0("SELECT c.cand_key AS cohort_row_id, c.subject_id AS subject_id, ",
                 "c.cohort_start_date AS cohort_start_date, ", end, " AS cohort_end_date, ",
                 "'t' AS index_source, ", .sqlLosslessKeyText(h, "c.cand_key"), " AS index_key, ",
                 "o.op_start AS op_start, o.op_end AS op_end, o.component_id AS op_component_id FROM ",
                 cand$name, " c INNER JOIN ", cov$coverage$name, " o ON o.row_key = c.cand_key WHERE ", keep)
  f <- .fragment(paste0("pop_", pop), "roster", "episode", body,
                 columns = list(cohort_row_id = .fragmentColumn("bigint", FALSE, "episode_key"),
                                subject_id = .fragmentColumn("integer", FALSE, "person_key"),
                                cohort_start_date = .fragmentColumn("date", FALSE, "start"),
                                cohort_end_date = .fragmentColumn("date", TRUE, "end"),
                                index_source = .fragmentColumn("text", FALSE, "origin_identity"),
                                index_key = .fragmentColumn("text", FALSE, "origin_identity"),
                                op_start = .fragmentColumn("date", TRUE, "start"),
                                op_end = .fragmentColumn("date", TRUE, "end"),
                                op_component_id = .fragmentColumn("bigint", TRUE, "private")),
                 dependencies = c(cand$name, cov$coverage$name), unique_key = "cohort_row_id",
                 person_lineage = list(person_col = "subject_id", via_roster = NULL),
                 roster_namespace = pop, private_columns = "op_component_id")
  .fragmentRegister(ctx, f)
}

# A candidates (phase "candidate", keyed by cand_key) or final roster (keyed
# by cohort_row_id) from literal rows: `rows` has the key, subject_id,
# cohort_start_date, cohort_end_date and optionally index_visit_id,
# index_source, index_key, op_start, op_end. The population is named
# explicitly (A-20 (a)); `index_sources` declares the rows' possible origins.
.ws5_literal_roster <- function(ctx, rows, name = "cand_t", population = "t",
                                phase = "candidate", index_sources = NULL) {
  h <- .compileDialect(ctx)
  candidate <- identical(phase, "candidate")
  key <- if (candidate) "cand_key" else "cohort_row_id"
  lit <- function(col, v) {
    if (length(v) != 1L || is.na(v)) {
      type <- switch(col, index_source = , index_key = "text",
                     cohort_start_date = , cohort_end_date = , op_start = , op_end = "date", "bigint")
      return(.compileTypedNull(h, type))
    }
    switch(col,
      subject_id = .sqlIntegerLiteral(h, as.integer(v)),
      cohort_start_date = , cohort_end_date = , op_start = , op_end = .sqlDateLiteral(h, as.character(v)),
      index_source = , index_key = .compileTextLiteral(as.character(v)),
      .sqlLosslessKey(h, format(v, scientific = FALSE)))
  }
  cols <- intersect(c(key, "subject_id", "cohort_start_date", "cohort_end_date", "index_visit_id",
                      "index_source", "index_key", "op_start", "op_end"), names(rows))
  selects <- vapply(seq_len(nrow(rows)), function(i) {
    paste0("SELECT ", paste(vapply(cols, function(cn) paste0(lit(cn, rows[[cn]][[i]]), " AS ", cn), ""),
                            collapse = ", "), .compileNoTableFrom(h))
  }, "")
  types <- list(subject_id = .fragmentColumn("integer", FALSE, "person_key"),
                cohort_start_date = .fragmentColumn("date", FALSE, "start"),
                cohort_end_date = .fragmentColumn("date", TRUE, "end"),
                index_visit_id = .fragmentColumn("bigint", TRUE, "private"),
                index_source = .fragmentColumn("text", TRUE, "origin_identity"),
                index_key = .fragmentColumn("text", TRUE, "origin_identity"),
                op_start = .fragmentColumn("date", TRUE, "start"),
                op_end = .fragmentColumn("date", TRUE, "end"))
  types[[key]] <- .fragmentColumn("bigint", FALSE, "episode_key")
  f <- .fragment(name, if (candidate) "candidates" else "roster", "episode",
                 paste(selects, collapse = " UNION ALL "), columns = types[cols], unique_key = key,
                 person_lineage = list(person_col = "subject_id", via_roster = NULL),
                 roster_namespace = if (!candidate) population,
                 private_columns = intersect(c(key, "index_visit_id", "index_source", "index_key"), cols))
  if (candidate) attr(f, "population") <- population
  if (!is.null(index_sources)) attr(f, "index_sources") <- index_sources
  .fragmentRegister(ctx, f)
}

# Release a context's compiler session tables when the calling frame exits,
# before cleanups registered earlier (the backends' connections): one deferred
# release per context, the context captured by value (A-20 (k)).
.ws5_release <- function(ctx, envir = parent.frame()) {
  force(ctx)
  withr::defer(.compileReleaseTempTables(ctx), envir = envir, priority = "first")
}

# .intervalReducerValues() with fragment names allocated by the test (the
# consumer): one bounds and one stream identifier per distinct call (A-20 (e)).
.ws5_rv <- function(ctx, fn, roster, phase = "final", ...) {
  args <- list(...)
  key <- paste(fn, roster$name, phase, args[["cov"]][["coverage"]][["name"]] %||% "",
               paste(deparse(args[setdiff(names(args), "cov")], control = NULL), collapse = ""))
  tag <- substr(paste(as.character(openssl::sha256(charToRaw(enc2utf8(key)))), collapse = ""), 1L, 12L)
  .intervalReducerValues(ctx, fn, roster, phase, ...,
                         names = list(bounds = paste0("tb_", tag), stream = paste0("ts_", tag)))
}

# Expected rows of an acceptance output as a data frame.
.ws5_expected <- function(recipe, output) {
  path <- testthat::test_path("..", "..", "inst", "plans", "acceptance", recipe)
  expected <- jsonlite::fromJSON(file.path(path, "expected.json"), simplifyVector = FALSE)
  rows <- expected$outputs[[output]]$rows
  cols <- unique(unlist(lapply(rows, names)))
  out <- lapply(cols, function(cn) unlist(lapply(rows, function(r) if (is.null(r[[cn]])) NA else r[[cn]])))
  names(out) <- cols
  as.data.frame(out, stringsAsFactors = FALSE)
}

.ws5_acceptance_plan <- function(recipe) {
  path <- testthat::test_path("..", "..", "inst", "plans", "acceptance", recipe)
  jsonlite::fromJSON(file.path(path, "plan.json"), simplifyVector = FALSE)
}

# Assert that the executable tests ran on every configured backend and record
# the runtime versions (design 9: release evidence names the runtimes).
.ws5_expect_backends <- function(backends, label) {
  b <- names(backends)
  testthat::expect_true("sqlite" %in% b)
  if (requireNamespace("duckdb", quietly = TRUE)) testthat::expect_true("duckdb" %in% b)
  if (nzchar(Sys.getenv("DSOMOP_TEST_POSTGRES_HOST"))) testthat::expect_true("postgresql" %in% b)
  if (nzchar(Sys.getenv("DSOMOP_TEST_MARIADB_HOST"))) testthat::expect_true("mariadb" %in% b)
  if (nzchar(Sys.getenv("DSOMOP_TEST_MYSQL_HOST"))) testthat::expect_true("mysql" %in% b)
  runtime <- vapply(backends, function(x) {
    p <- .sqlDialectProfile(x$handle, probe = TRUE)
    paste0(p$family, " (", p$runtime %||% "unknown runtime", ")")
  }, "")
  cat("\n[", label, " backends] ", paste(runtime, collapse = "; "), "\n", sep = "")
  invisible(b)
}
