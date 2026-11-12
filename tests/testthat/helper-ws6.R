# Shared fixtures for the WS-6 tests (design-2.10.3 sections 2.5, 2.8, 6.9,
# 6.10): populations and matching on every executable backend available here
# -- SQLite always, DuckDB when installed, PostgreSQL, MariaDB and MySQL
# through the opt-in vendor variables (DSOMOP_TEST_POSTGRES_HOST,
# DSOMOP_TEST_MARIADB_HOST, DSOMOP_TEST_MYSQL_HOST / _PORT) -- on the extended
# reach fixture, optionally extended by a test's own rows.

#' The extended reach fixture on every backend available
#'
#' @param extend NULL or function(conn) adding rows to the SQLite source
#'   before it is copied.
#' @param vendors Backends to try besides SQLite.
#' @return Named list of list(handle, cleanup).
.ws6_backends <- function(extend = NULL,
                          vendors = c("duckdb", "postgresql", "mariadb", "mysql")) {
  sqlite <- .ws4_reach_handle()
  if (!is.null(extend)) extend(sqlite$conn)
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
    schema <- .ws4_namespace("ws6")
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
    db <- .ws4_namespace("ws6")
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

# Population scenario rows (persons 6001..6019; new concept ids 93000001..9).
# Every person is male, born 1970 and observed 2010-01-01..2030-12-31 unless
# stated:
#   6001  first_eligible: IDX 2019-01-10 and 2020-01-10; ANC 2019-12-01 (only
#         the second candidate has it in [-60, 0]); OTHER 2019-12-15
#   6002  legacy order: IDX 2020-01-01 and 2020-07-01; ANC 2020-07-01 only
#   6003  two sources: Z 2020-01-01 (source z_earlier), A 2020-12-01 (a_later)
#   6004  one physical ARM event 2020-02-02 matched by two any_of arms
#   6005  ends: observed 2015-01-01..2020-03-31; IDX 2020-03-01 ending 2020-06-30
#   6006  two overlapping periods cover IDX 2020-01-15 (ambiguous)
#   6007  IDX 2020-01-15 before its only period (2021-01-01..): uncovered
#   6008  periods [2020-01-01,02] + [2020-01-10,11]; IDX 2020-01-05 in the gap
#   6009  periods [2015-01-01, 2020-01-02] + [2020-01-09, ..]; IDX 2020-01-10
#   6010  ANC rows 2020-01-01 (visit 601001) x2, 2020-01-01 (no visit),
#         2020-01-02 (visit 601002), OTHER 2020-01-03; IDX 2020-01-10
#   6011  LAB 2020-01-05 with a NULL value; IDX 2020-01-10
#   6012  NULL year_of_birth; 6013 born 2030; 6014 born 1950; IDX 2020-01-10
#   6015  IDX 2020-01-10 twice, on visits 601501 and 601502; a procedure PROC
#         on visit 601501 only
#   6016  IDX 2020-01-10 and no ANC at all (missing source anchor)
#   6017  in results cohort 93001; IDX 2020-01-10
#   6018  results cohort 93002 episodes 2020-01-01 and 2020-06-01, ANC on
#         2020-06-01 only; 6019 one episode 2020-01-01 with ANC 2020-01-01
.WS6 <- list(IDX = 93000001L, ANC = 93000002L, Z = 93000003L, A = 93000004L,
             ARM = 93000005L, ARM2 = 93000006L, LAB = 93000007L, OTHER = 93000008L,
             PROC = 93000009L)

.ws6_population_extend <- function(conn) {
  W <- .WS6
  app <- function(name, df) DBI::dbWriteTable(conn, name, df, append = TRUE)
  pids <- 6001:6019
  yob <- ifelse(pids == 6012L, NA_integer_, ifelse(pids == 6013L, 2030L, ifelse(pids == 6014L, 1950L, 1970L)))
  app("person", data.frame(
    person_id = pids, gender_concept_id = 8507L, year_of_birth = yob,
    month_of_birth = 1L, day_of_birth = 1L, race_concept_id = 8527L,
    ethnicity_concept_id = 38003564L, location_id = pids, provider_id = pids,
    care_site_id = pids, person_source_value = paste0("WS6_", pids),
    gender_source_value = "M", stringsAsFactors = FALSE))
  op <- function(id, pid, s, e) data.frame(
    observation_period_id = id, person_id = pid, observation_period_start_date = s,
    observation_period_end_date = e, period_type_concept_id = 44814724L, stringsAsFactors = FALSE)
  plain <- setdiff(pids, c(6005L, 6006L, 6007L, 6008L, 6009L))
  app("observation_period", rbind(
    op(600000L + plain, plain, "2010-01-01", "2030-12-31"),
    op(600501L, 6005L, "2015-01-01", "2020-03-31"),
    op(600601L, 6006L, "2015-01-01", "2020-06-30"), op(600602L, 6006L, "2019-01-01", "2030-12-31"),
    op(600701L, 6007L, "2021-01-01", "2030-12-31"),
    op(600801L, 6008L, "2020-01-01", "2020-01-02"), op(600802L, 6008L, "2020-01-10", "2020-01-11"),
    op(600901L, 6009L, "2015-01-01", "2020-01-02"), op(600902L, 6009L, "2020-01-09", "2030-12-31")))
  cid <- 960000L
  co <- function(pid, concept, start, end = start, visit = NA_integer_) {
    cid <<- cid + 1L
    data.frame(condition_occurrence_id = cid, person_id = pid, condition_concept_id = concept,
               condition_start_date = start, condition_end_date = end,
               condition_type_concept_id = 44818518L, stop_reason = NA_character_,
               provider_id = pid, visit_occurrence_id = visit, condition_source_value = "WS6",
               condition_source_concept_id = 0L, stringsAsFactors = FALSE)
  }
  app("condition_occurrence", rbind(
    co(6001L, W$IDX, "2019-01-10"), co(6001L, W$IDX, "2020-01-10"), co(6001L, W$ANC, "2019-12-01"),
    co(6001L, W$OTHER, "2019-12-15"),
    co(6002L, W$IDX, "2020-01-01"), co(6002L, W$IDX, "2020-07-01"), co(6002L, W$ANC, "2020-07-01"),
    co(6003L, W$Z, "2020-01-01"), co(6003L, W$A, "2020-12-01"),
    co(6004L, W$ARM, "2020-02-02"),
    co(6005L, W$IDX, "2020-03-01", end = "2020-06-30"),
    co(6006L, W$IDX, "2020-01-15"), co(6007L, W$IDX, "2020-01-15"), co(6008L, W$IDX, "2020-01-05"),
    co(6009L, W$IDX, "2020-01-10"),
    co(6010L, W$ANC, "2020-01-01", visit = 601001L), co(6010L, W$ANC, "2020-01-01", visit = 601001L),
    co(6010L, W$ANC, "2020-01-01"), co(6010L, W$ANC, "2020-01-02", visit = 601002L),
    co(6010L, W$OTHER, "2020-01-03"), co(6010L, W$IDX, "2020-01-10"),
    co(6011L, W$IDX, "2020-01-10"), co(6012L, W$IDX, "2020-01-10"), co(6013L, W$IDX, "2020-01-10"),
    co(6014L, W$IDX, "2020-01-10"),
    co(6015L, W$IDX, "2020-01-10", visit = 601501L), co(6015L, W$IDX, "2020-01-10", visit = 601502L),
    co(6016L, W$IDX, "2020-01-10"), co(6017L, W$IDX, "2020-01-10"),
    co(6018L, W$ANC, "2020-06-01"), co(6019L, W$ANC, "2020-01-01")))
  app("visit_occurrence", data.frame(
    visit_occurrence_id = c(601001L, 601002L, 601501L, 601502L),
    person_id = c(6010L, 6010L, 6015L, 6015L), visit_concept_id = 9202L,
    visit_start_date = c("2020-01-01", "2020-01-02", "2020-01-10", "2020-01-10"),
    visit_end_date = c("2020-01-01", "2020-01-02", "2020-01-10", "2020-01-10"),
    visit_type_concept_id = 44818518L, visit_source_value = "V", visit_source_concept_id = 0L,
    stringsAsFactors = FALSE))
  app("procedure_occurrence", data.frame(
    procedure_occurrence_id = 960001L, person_id = 6015L, procedure_concept_id = W$PROC,
    procedure_date = "2020-01-10", procedure_end_date = "2020-01-10", procedure_type_concept_id = 44818518L,
    procedure_source_value = "P", procedure_source_concept_id = 0L, visit_occurrence_id = 601501L,
    stringsAsFactors = FALSE))
  app("measurement", data.frame(
    measurement_id = 960001L, person_id = 6011L, measurement_concept_id = W$LAB,
    measurement_date = "2020-01-05", measurement_type_concept_id = 44818702L,
    operator_concept_id = 0L, value_as_number = NA_real_, value_as_concept_id = 0L,
    unit_concept_id = 8554L, range_low = NA_real_, range_high = NA_real_,
    measurement_source_value = "WS6", measurement_source_concept_id = 0L,
    unit_source_value = "u", stringsAsFactors = FALSE))
  app("cohort", data.frame(
    cohort_definition_id = c(93001L, 93002L, 93002L, 93002L), subject_id = c(6017L, 6018L, 6018L, 6019L),
    cohort_start_date = c("2020-01-10", "2020-01-01", "2020-06-01", "2020-01-01"),
    cohort_end_date = c("2020-12-31", "2020-03-31", "2020-12-31", "2020-12-31"), stringsAsFactors = FALSE))
  ids <- unlist(W, use.names = FALSE)
  app("concept", data.frame(
    concept_id = ids, concept_name = paste("ws6", names(W)),
    domain_id = ifelse(ids == W$LAB, "Measurement", ifelse(ids == W$PROC, "Procedure", "Condition")),
    vocabulary_id = "SNOMED", concept_class_id = "Clinical Finding", standard_concept = "S",
    concept_code = paste0("WS6", seq_along(ids)), valid_start_date = "1970-01-01",
    valid_end_date = "2099-12-31", invalid_reason = NA_character_, stringsAsFactors = FALSE))
  app("concept_ancestor", data.frame(ancestor_concept_id = ids, descendant_concept_id = ids,
                                     min_levels_of_separation = 0L, max_levels_of_separation = 0L))
  invisible(conn)
}

# Matching scenario rows (persons 7001..7035; outcome OUT 93100001, stratum
# STR 93100002). Birth years 1900..1925 keep their age bands apart from the
# reach persons (born 1934..1990). Everyone is observed 2010-01-01..
# 2030-12-31 unless stated.
#   cases (first OUT): 7001 M 1913 2020-03-01; 7002 F 1925 2020-03-01;
#     7003 M 1913 2020-06-01 (a future case for 7001); 7004 M 1913 2020-03-01
#     (an outcome exactly at 7001's case time); 7005 M 1913 2018-01-01 (and a
#     second OUT 2021-01-01); 7006 M NULL birth year 2020-03-01; 7007 F 1900
#     2020-03-01 (a unique age band: no control)
#   controls: 7010..7024 M 1913 (STR 2019-01-01 for 7010..7012);
#     7025..7029 F 1925; 7030 M 1913 died 2020-02-01; 7031 M 1913 died
#     2020-03-01 (alive at t); 7032 M 1913 observed until 2020-02-28;
#     7033 M NULL birth year; 7034 M 1913 observed from 2019-06-01;
#     7035 M 1910 (band 22 in 2020, 21 in 2018)
#   allocation rollback (F, band 23 in 2020 and 2021): cases 8001 born 1904
#     (OUT 2020-03-01) and 8002 born 1904 (OUT 2021-03-01); controls 8010,
#     8011 born 1904 and 8012 born 1906 (band 22 in 2020, 23 in 2021)
.WS6M <- list(OUT = 93100001L, STR = 93100002L)

.ws6_match_rows <- function() {
  pids <- c(7001:7007, 7010:7035, 8001L, 8002L, 8010:8012)
  sex <- rep(8507L, length(pids))
  sex[pids %in% c(7002L, 7007L, 7025:7029, 8001L, 8002L, 8010:8012)] <- 8532L
  yob <- rep(1913L, length(pids))
  yob[pids %in% c(7002L, 7025:7029)] <- 1925L
  yob[pids == 7007L] <- 1900L
  yob[pids %in% c(7006L, 7033L)] <- NA_integer_
  yob[pids == 7035L] <- 1910L
  yob[pids %in% c(8001L, 8002L, 8010L, 8011L)] <- 1904L
  yob[pids == 8012L] <- 1906L
  start <- ifelse(pids == 7034L, "2019-06-01", "2010-01-01")
  end <- ifelse(pids == 7032L, "2020-02-28", "2030-12-31")
  out <- data.frame(person_id = c(7001L, 7002L, 7003L, 7004L, 7005L, 7005L, 7006L, 7007L, 8001L, 8002L),
                    date = c("2020-03-01", "2020-03-01", "2020-06-01", "2020-03-01", "2018-01-01",
                             "2021-01-01", "2020-03-01", "2020-03-01", "2020-03-01", "2021-03-01"),
                    stringsAsFactors = FALSE)
  str <- data.frame(person_id = 7010:7012, date = "2019-01-01", stringsAsFactors = FALSE)
  death <- data.frame(person_id = c(7030L, 7031L), date = c("2020-02-01", "2020-03-01"),
                      stringsAsFactors = FALSE)
  list(persons = data.frame(person_id = pids, gender_concept_id = sex, year_of_birth = yob,
                            start = start, end = end, stringsAsFactors = FALSE),
       out = out, str = str, death = death)
}

.ws6_match_extend <- function(conn, reverse = FALSE) {
  W <- .WS6M
  x <- .ws6_match_rows()
  ord <- function(df) if (reverse) df[rev(seq_len(nrow(df))), , drop = FALSE] else df
  app <- function(name, df) DBI::dbWriteTable(conn, name, ord(df), append = TRUE)
  p <- x$persons
  app("person", data.frame(
    person_id = p$person_id, gender_concept_id = p$gender_concept_id, year_of_birth = p$year_of_birth,
    month_of_birth = 1L, day_of_birth = 1L, race_concept_id = 8527L, ethnicity_concept_id = 38003564L,
    location_id = p$person_id, provider_id = p$person_id, care_site_id = p$person_id,
    person_source_value = paste0("WS6M_", p$person_id), gender_source_value = "U", stringsAsFactors = FALSE))
  app("observation_period", data.frame(
    observation_period_id = 700000L + p$person_id, person_id = p$person_id,
    observation_period_start_date = p$start, observation_period_end_date = p$end,
    period_type_concept_id = 44814724L, stringsAsFactors = FALSE))
  rows <- rbind(cbind(x$out, concept = W$OUT), cbind(x$str, concept = W$STR))
  app("condition_occurrence", data.frame(
    condition_occurrence_id = 970000L + seq_len(nrow(rows)), person_id = rows$person_id,
    condition_concept_id = rows$concept, condition_start_date = rows$date, condition_end_date = rows$date,
    condition_type_concept_id = 44818518L, stop_reason = NA_character_, provider_id = rows$person_id,
    visit_occurrence_id = NA_integer_, condition_source_value = "WS6M", condition_source_concept_id = 0L,
    stringsAsFactors = FALSE))
  app("death", data.frame(
    person_id = x$death$person_id, death_date = x$death$date, death_type_concept_id = 32817L,
    cause_concept_id = 0L, cause_source_value = "X", cause_source_concept_id = 0L, stringsAsFactors = FALSE))
  ids <- unlist(W, use.names = FALSE)
  DBI::dbWriteTable(conn, "concept", data.frame(
    concept_id = ids, concept_name = paste("ws6m", names(W)), domain_id = "Condition", vocabulary_id = "SNOMED",
    concept_class_id = "Clinical Finding", standard_concept = "S", concept_code = paste0("WS6M", seq_along(ids)),
    valid_start_date = "1970-01-01", valid_end_date = "2099-12-31", invalid_reason = NA_character_,
    stringsAsFactors = FALSE), append = TRUE)
  DBI::dbWriteTable(conn, "concept_ancestor", data.frame(
    ancestor_concept_id = ids, descendant_concept_id = ids, min_levels_of_separation = 0L,
    max_levels_of_separation = 0L), append = TRUE)
  invisible(conn)
}

# Rows of a population's final roster (fixture keys as the oracle writes them).
.ws6_roster <- function(ctx, pop) {
  res <- .compilePopulationByName(ctx, pop)
  r <- .fetchFragment(ctx, res$roster, "cohort_row_id")
  data.frame(cohort_row_id = as.numeric(r$cohort_row_id), person_id = as.numeric(r$subject_id),
             index_date = format(r$cohort_start_date, "%Y-%m-%d"),
             end_date = format(r$cohort_end_date, "%Y-%m-%d"),
             index_source = as.character(r$index_source), index_key = as.character(r$index_key),
             set_id = as.numeric(r$set_id), is_case = as.numeric(r$is_case),
             w = as.numeric(r$sampling_weight),
             op_start = format(r$op_start, "%Y-%m-%d"), op_end = format(r$op_end, "%Y-%m-%d"),
             stringsAsFactors = FALSE)
}

.ws6_key <- function(df, cols = c("person_id", "index_date")) do.call(paste, df[cols])

# A language-2.0 plan for population tests: events, intervals and the given
# populations, with one wide output per population (outputs make every
# population an ordinary roster; matching templates get none).
.ws6_plan <- function(events = list(), populations = list(), intervals = list(),
                      outputs = NULL, scope = NULL) {
  pops <- populations
  if (is.null(outputs)) {
    templates <- unlist(lapply(pops, function(p) if (identical(p$kind, "match")) p$match$controls))
    outputs <- list()
    for (p in setdiff(names(pops), templates)) {
      outputs[[paste0("o_", p)]] <- list(rows = p, format = "wide")
    }
  }
  plan <- list(version = 2L,
               meta = list(language_version = "2.0", semantic_version = "2.0.0", generated_by = "ws6-test"),
               events = events, populations = pops, outputs = outputs)
  if (!length(events)) plan$events <- NULL
  if (length(intervals)) plan$intervals <- intervals
  if (!is.null(scope)) plan$scope <- scope
  plan
}

.ws6_event <- function(table, concepts = NULL, ...) {
  ev <- list(table = table, ...)
  if (!is.null(concepts)) ev$concept_set <- list(concepts = as.list(concepts), include_descendants = TRUE)
  ev
}
