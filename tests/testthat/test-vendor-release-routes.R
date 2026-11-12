# dsOMOP 2.10.3 (G-01/G-02 SQL on real engines): the coalesced
# observation-period islands, the observed time-at-risk windows and the
# protected release routes execute on PostgreSQL and the MySQL family (opt-in,
# see helper-vendor-db.R) and on DuckDB, and give exactly the SQLite answers on
# the same data. Protected answers are deterministic in the noise root, the
# component and its realised bounded contributions, so equal SQL semantics give
# equal releases; any engine-specific date, window or grouping difference
# shows up as a changed answer. Fixture: helper-achilles-live.R.

# The live fixture plus first outcomes (T2DM) for comparator persons 21-34, so
# both arms have outcomes inside their windows.
.vrFrames <- function() {
  frames <- .achillesLiveFrames()
  co <- frames$cdm$condition_occurrence
  extra <- data.frame(
    condition_occurrence_id = max(co$condition_occurrence_id) + seq_len(14L),
    person_id = 21:34, condition_concept_id = 201820L,
    condition_start_date = as.Date("2020-02-15"),
    condition_end_date = as.Date("2020-02-17"),
    condition_type_concept_id = 32020L, visit_occurrence_id = NA_integer_,
    stringsAsFactors = FALSE)
  frames$cdm$condition_occurrence <- rbind(co, extra[, names(co)])
  frames
}

# Two arms of 20 persons with staggered index dates. Person 36 is indexed
# between its observation periods (no risk time); person 33 is indexed in its
# second, disjoint observation period.
.vrCohortRows <- function() {
  ids <- 1:40
  start <- as.Date("2019-01-15") + ((ids - 1L) %% 7L) * 45L
  start[ids == 36L] <- as.Date("2023-03-01")
  start[ids == 33L] <- as.Date("2023-07-01")
  end <- start + 400L + ((ids - 1L) %% 5L) * 60L
  data.frame(subject_id = ids, start = start, end = end,
             arm = ifelse(ids <= 20L, "target", "comparator"),
             stringsAsFactors = FALSE)
}

.vrDate <- function(dbms, x) {
  x <- format(as.Date(x), "%Y-%m-%d")
  if (identical(dbms, "sqlite")) paste0("'", x, "'") else {
    paste0("CAST('", x, "' AS DATE)")
  }
}

.vrCreateCohorts <- function(handle, dbms) {
  rows <- .vrCohortRows()
  make <- function(name, part) {
    sql <- paste(sprintf(paste0("SELECT %d AS subject_id, %s AS ",
                                "cohort_start_date, %s AS cohort_end_date"),
                         part$subject_id,
                         vapply(part$start, function(d) .vrDate(dbms, d),
                                character(1L)),
                         vapply(part$end, function(d) .vrDate(dbms, d),
                                character(1L))),
                 collapse = " UNION ALL ")
    .createTempTable(handle, name, sql)
  }
  list(target = make("dsomop_vr_target", rows[rows$arm == "target", ]),
       comparator = make("dsomop_vr_comparator",
                         rows[rows$arm == "comparator", ]))
}

# An embedded engine loaded with the live fixture (one schema).
.vrEngineHandle <- function(conn, dbms) {
  for (statement in .achillesLiveStatements(.vrFrames(), dbms)) {
    DBI::dbExecute(conn, statement)
  }
  handle <- new.env(parent = emptyenv())
  handle$conn <- conn
  handle$dbms <- dbms
  handle$target_dialect <- dbms
  handle$cdm_schema <- if (identical(dbms, "duckdb")) "main" else NULL
  handle$vocab_schema <- NULL
  handle$results_schema <- NULL
  handle$temp_schema <- NULL
  handle$resource_client <- NULL
  handle$config <- list()
  handle$blueprint <- NULL
  handle$temp_tables <- character(0)
  handle$temp_connection <- NULL
  handle$person_key <- .testPseudonymKey("vendor-release-routes")
  .buildBlueprint(handle)
  handle
}

# Expected coalesced islands (half-open, end + 1) of the fixture.
.vrExpectedIslands <- function() {
  op <- .vrFrames()$cdm$observation_period
  out <- lapply(split(op, op$person_id), function(p) {
    p <- p[order(p$observation_period_start_date,
                 p$observation_period_end_date), ]
    s <- as.numeric(p$observation_period_start_date)
    e <- as.numeric(p$observation_period_end_date) + 1
    lo <- s[[1L]]; hi <- e[[1L]]; islands <- list()
    for (i in seq_along(s)[-1L]) {
      if (s[[i]] <= hi) hi <- max(hi, e[[i]]) else {
        islands[[length(islands) + 1L]] <- c(lo, hi)
        lo <- s[[i]]; hi <- e[[i]]
      }
    }
    islands[[length(islands) + 1L]] <- c(lo, hi)
    data.frame(person_id = p$person_id[[1L]], n_islands = length(islands),
               days = sum(vapply(islands, function(x) x[[2L]] - x[[1L]],
                                 numeric(1L))))
  })
  out <- do.call(rbind, out)
  out[order(out$person_id), , drop = FALSE]
}

# Expected observed window of every cohort person (anchors at the index).
.vrExpectedWindows <- function(rows, start, end) {
  op <- .vrFrames()$cdm$observation_period
  res <- lapply(seq_len(nrow(rows)), function(i) {
    p <- op[op$person_id == rows$subject_id[[i]], ]
    s <- as.numeric(p$observation_period_start_date)
    e <- as.numeric(p$observation_period_end_date) + 1
    # Coalesce, then take the island containing the index.
    o <- order(s, e); s <- s[o]; e <- e[o]
    lo <- s[[1L]]; hi <- e[[1L]]; isl <- list()
    for (k in seq_along(s)[-1L]) {
      if (s[[k]] <= hi) hi <- max(hi, e[[k]]) else {
        isl[[length(isl) + 1L]] <- c(lo, hi); lo <- s[[k]]; hi <- e[[k]]
      }
    }
    isl[[length(isl) + 1L]] <- c(lo, hi)
    index <- as.numeric(rows$start[[i]])
    hit <- Filter(function(x) x[[1L]] <= index && index < x[[2L]], isl)
    if (!length(hit)) return(NULL)
    w_lo <- max(index + start, hit[[1L]][[1L]])
    w_hi <- min(index + end + 1, hit[[1L]][[2L]])
    if (w_hi <= w_lo) return(NULL)
    data.frame(subject_id = rows$subject_id[[i]], lo_offset = w_lo - index,
               days = w_hi - w_lo)
  })
  out <- do.call(rbind, res)
  out[order(out$subject_id), , drop = FALSE]
}

.vrNumericFrame <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  for (col in names(df)) {
    v <- df[[col]]
    if (inherits(v, "integer64") || is.numeric(v)) df[[col]] <- as.numeric(v)
  }
  df[do.call(order, unname(as.list(df))), , drop = FALSE]
}

.expectVrSqlHelpers <- function(handle, dbms, cohorts) {
  islands <- .vrNumericFrame(.executeQuery(handle, .sql_translate(paste0(
    "SELECT i.person_id, COUNT(*) AS n_islands, SUM(",
    .omopDateDiffDays(handle, "i.op_end", "i.op_start"), ") AS days FROM ",
    .omopObservationIslandsSql(handle), " i GROUP BY i.person_id"),
    handle$target_dialect)))
  expected <- .vrExpectedIslands()
  rownames(islands) <- rownames(expected) <- NULL
  expect_equal(islands, .vrNumericFrame(expected), info = dbms)

  rows <- .vrCohortRows()
  for (tar in list(c(0L, 365L), c(-30L, 90L), c(1L, 3650L))) {
    window <- .vrNumericFrame(.executeQuery(handle, .sql_translate(paste0(
      "SELECT w.subject_id, ",
      .omopDateDiffDays(handle, "w.tar_lo", "w.cohort_start_date"),
      " AS lo_offset, ", .omopDateDiffDays(handle, "w.tar_hi", "w.tar_lo"),
      " AS days FROM ", .omopObservedTarSql(handle, cohorts$target, list(
        start = tar[[1L]], end = tar[[2L]], anchor_start = "start",
        anchor_end = "start")), " w"), handle$target_dialect)))
    want <- .vrExpectedWindows(rows[rows$arm == "target", ], tar[[1L]],
                               tar[[2L]])
    rownames(window) <- rownames(want) <- NULL
    expect_equal(window, .vrNumericFrame(want),
                 info = paste(dbms, tar[[1L]], tar[[2L]]))
  }
}

.vrQuestions <- function(cohorts) {
  arms <- list(cohorts$target, cohorts$comparator)
  tar <- function(end) list(tar_start_offset = "0",
                            tar_end_offset = as.character(end),
                            tar_anchor_start = "start",
                            tar_anchor_end = "start")
  list(
    # Unscoped public bases of the custodial generation: fixed synopses.
    list(id = "dsomop:achilles.3"), list(id = "dsomop:achilles.10"),
    list(id = "dsomop:achilles.101"), list(id = "dsomop:achilles.103"),
    list(id = "dsomop:achilles.105"), list(id = "dsomop:achilles.107"),
    list(id = "dsomop:achilles.203"), list(id = "dsomop:achilles.406"),
    list(id = "dsomop:achilles.506"), list(id = "dsomop:achilles.715"),
    list(id = "dsomop:person.age_distribution_decade",
         params = list(reference_year = "2020")),
    list(id = "dsomop:observation_period.long_period_count",
         params = list(min_days = "365")),
    # Scoped questions: realised-contribution protected.
    list(id = "dsomop:incidence.rate", scope = cohorts$target,
         params = c(list(outcome_concept_id = "201820"), tar(365))),
    list(id = "dsomop:cm.mdrr", scope = arms,
         params = c(list(outcome_concept_id = "201820"), tar(365))),
    list(id = "dsomop:cm.effect_estimate", scope = arms,
         params = c(list(outcome_concept_id = "201820",
                         model_type = "poisson"), tar(730))),
    list(id = "dsomop:cm.interaction_estimate", scope = arms,
         params = c(list(outcome_concept_id = "201820",
                         subgroup_kind = "concept",
                         subgroup_concept_id = "320128",
                         model_type = "poisson"), tar(730))),
    list(id = "dsomop:cm.kaplan_meier", scope = arms,
         params = list(outcome_concept_id = "201820")),
    list(id = "dsomop:cm.followup_distribution", scope = cohorts$target,
         params = tar(365)),
    list(id = "dsomop:fe.continuous", scope = cohorts$target),
    list(id = "dsomop:cohortdx.time_distribution", scope = cohorts$target),
    list(id = "dsomop:char.time_to_event", scope = cohorts$target,
         params = list(outcome_concept_id = "201820")),
    # Grouped-time Cox exits/censoring and the SCCS case substrate
    # (tools_release_survival.R).
    list(id = "dsomop:cm.effect_estimate", scope = arms,
         params = c(list(outcome_concept_id = "201820",
                         model_type = "cox"), tar(730))),
    list(id = "dsomop:sccs.incidence_rate_ratio",
         params = list(exposure_concept_id = "1503297",
                       outcome_concept_id = "201820", window = "30"))
  )
}

.vrRunAll <- function(handle, cohorts) {
  lapply(.vrQuestions(cohorts), function(q) {
    out <- suppressWarnings(.omopAnalysisRun(handle, q$id, q$params %||% list(),
                                             scope = q$scope))
    protection <- attr(out, "dsomop_protection", exact = TRUE)
    read_snapshot <- protection$read_snapshot
    # Each engine reports its own transaction mechanism. Check that field
    # separately while retaining every other attribute in the parity check.
    protection$read_snapshot <- NULL
    attr(out, "dsomop_protection") <- protection
    list(id = q$id, protection = protection$protection %||% "standard",
         read_snapshot = read_snapshot, values = .vrNumericFrame(out))
  })
}

.expectVrSameAnswers <- function(reference, actual, label) {
  expect_identical(vapply(actual, `[[`, character(1L), "id"),
                   vapply(reference, `[[`, character(1L), "id"))
  expected_snapshot <- switch(label,
    duckdb = "snapshot_transaction",
    postgresql = "repeatable_read_transaction",
    mysql = "consistent_snapshot_transaction",
    mariadb = "consistent_snapshot_transaction")
  for (i in seq_along(reference)) {
    info <- paste(label, reference[[i]]$id)
    expect_identical(actual[[i]]$protection, reference[[i]]$protection,
                     info = info)
    expect_identical(actual[[i]]$read_snapshot, expected_snapshot, info = info)
    a <- actual[[i]]$values
    r <- reference[[i]]$values
    rownames(a) <- rownames(r) <- NULL
    expect_equal(a, r, tolerance = 1e-9, info = info)
  }
}

# The SQLite reference answers (computed once per test run).
.vrReference <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
    handle <- .vrEngineHandle(conn, "sqlite")
    cohorts <- .vrCreateCohorts(handle, "sqlite")
    suppressWarnings(.omopAnalysisRegistry(handle, force = TRUE))
    .expectVrSqlHelpers(handle, "sqlite", cohorts)
    cache <<- .vrRunAll(handle, cohorts)
    cache
  }
})

.expectVrEngine <- function(handle, dbms) {
  cohorts <- .vrCreateCohorts(handle, dbms)
  suppressWarnings(.omopAnalysisRegistry(handle, force = TRUE))
  .expectVrSqlHelpers(handle, dbms, cohorts)
  answers <- .vrRunAll(handle, cohorts)
  protections <- stats::setNames(
    vapply(answers, `[[`, character(1L), "protection"),
    vapply(answers, `[[`, character(1L), "id"))
  expect_true(all(protections[c("dsomop:achilles.3", "dsomop:achilles.103",
                                "dsomop:achilles.406")] == "fixed_synopsis"))
  expect_true(all(protections[c("dsomop:incidence.rate", "dsomop:cm.mdrr",
                                "dsomop:cm.effect_estimate",
                                "dsomop:fe.continuous")] ==
                    "realised_sticky"))
  .expectVrSameAnswers(.vrReference(), answers, dbms)
}

test_that("SQLite runs the observed-time helpers and every protected route", {
  .release_state()
  reference <- .vrReference()
  expect_length(reference, length(.vrQuestions(list())))
  expect_identical(vapply(reference, `[[`, character(1L), "read_snapshot"),
                   rep("serializable_transaction", length(reference)))
  # Every question returns rows on this fixture.
  for (r in reference) expect_gt(nrow(r$values), 0L, label = r$id)
})

test_that("DuckDB gives the SQLite protected answers", {
  skip_if_not_installed("duckdb")
  .release_state()
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  .expectVrEngine(.vrEngineHandle(conn, "duckdb"), "duckdb")
})

.expectVrVendor <- function(dbms) {
  config <- .dsomopVendorTestConfig(dbms)
  testthat::skip_if(is.null(config), paste0(
    "Set DSOMOP_TEST_", toupper(if (identical(dbms, "postgresql")) {
      "POSTGRES"
    } else dbms), "_HOST to run this vendor integration test."))
  .release_state(.local_envir = parent.frame())
  frames <- .vrFrames()
  fixture <- .dsomopVendorCreateFixture(config, statements = function(
      cdm, vocab, results) {
    .achillesLiveStatements(frames, dbms, list(cdm = cdm, vocab = vocab,
                                               results = results))
  })
  on.exit(fixture$cleanup(), add = TRUE)
  message("[vendor-versions] ", dbms, " server ", as.character(
    DBI::dbGetQuery(.conn(fixture$handle), "SELECT VERSION() AS v")$v[[1L]]))
  .expectVrEngine(fixture$handle, dbms)
}

test_that("PostgreSQL gives the SQLite protected answers", {
  .expectVrVendor("postgresql")
})

test_that("MySQL gives the SQLite protected answers", {
  .expectVrVendor("mysql")
})

test_that("MariaDB gives the SQLite protected answers", {
  .expectVrVendor("mariadb")
})
