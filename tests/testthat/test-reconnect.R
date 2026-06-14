# Phase 4: transparent DB auto-reconnect + disclosure fail-closed.
#
# Exercises the connection layer added in R/blueprint.R:
#   .conn() / .withDbReconnect() / .isConnectionError() / .isMissingObjectError()
# and its integration with the per-patient gate (.assertMinPersons) and the
# query path (.executeQuery / .executeStatement).
#
# Strategy: a fake resource_client backed by a FILE SQLite DB so data survives a
# DBI::dbDisconnect (an in-memory DB would vanish, masking the reconnect). The
# fake mirrors the real OMOPResourceClient$getConnection(): it revalidates with
# DBI::dbIsValid() and reconnects on demand, and can be told to throw a
# connection-class error on the next getConnection() to simulate a dead pool.

# --- Fake resource_client + handle -------------------------------------------

# Build a fake resource_client over a file-backed SQLite DB seeded with a
# `person` table of `n_persons` rows. `state` is an environment so the test can
# flip failure injection and observe reconnect counts by reference.
make_fake_rc <- function(n_persons = 15) {
  path <- tempfile(fileext = ".sqlite")
  seed <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(seed, "CREATE TABLE person (person_id INTEGER)")
  DBI::dbExecute(seed,
    paste0("INSERT INTO person (person_id) VALUES ",
           paste(sprintf("(%d)", seq_len(n_persons)), collapse = ",")))
  DBI::dbDisconnect(seed)

  state <- new.env(parent = emptyenv())
  state$path           <- path
  state$conn           <- NULL
  state$connects       <- 0L   # how many real dbConnect calls happened
  state$getcalls       <- 0L   # how many getConnection() calls happened
  state$hand_back_dead <- FALSE # return the current (dead) conn ONCE, no revalidate
  state$closed         <- FALSE

  rc <- list(
    # Mirrors OMOPResourceClient$getConnection(): revalidate + reconnect on
    # demand. `hand_back_dead` models a pooled connection that LOOKS alive but
    # has died: it is returned once without revalidation, so the next query
    # fails with a connection-class error and the retry path reconnects.
    getConnection = function() {
      state$getcalls <- state$getcalls + 1L
      if (isTRUE(state$hand_back_dead)) {
        state$hand_back_dead <- FALSE
        return(state$conn)
      }
      if (is.null(state$conn) || !DBI::dbIsValid(state$conn)) {
        state$conn <- DBI::dbConnect(RSQLite::SQLite(), state$path)
        state$connects <- state$connects + 1L
        state$closed <- FALSE
      }
      state$conn
    },
    close = function() {
      if (!is.null(state$conn) && DBI::dbIsValid(state$conn)) {
        DBI::dbDisconnect(state$conn)
      }
      state$conn <- NULL
      state$closed <- TRUE
      invisible(NULL)
    },
    getResource = function() list(url = paste0("datashield://siteX/", basename(path)),
                                  name = NULL, identity = NULL),
    getParsed   = function() list(server = "siteX")
  )
  list(rc = rc, state = state)
}

# A handle that goes through the real .conn()/.withDbReconnect() path because it
# OWNS a resource_client (unlike create_test_handle(), which sets it to NULL).
make_reconnect_handle <- function(n_persons = 15) {
  fk <- make_fake_rc(n_persons)
  handle <- new.env(parent = emptyenv())
  handle$conn            <- fk$rc$getConnection()  # initial live snapshot
  handle$dbms            <- "sqlite"
  handle$target_dialect  <- "sqlite"
  handle$cdm_schema      <- NULL
  handle$vocab_schema    <- NULL
  handle$results_schema  <- NULL
  handle$temp_schema     <- NULL
  handle$resource_client <- fk$rc
  handle$config          <- list()
  handle$blueprint       <- NULL
  handle$temp_tables     <- character(0)
  handle$person_key      <- as.raw(1:16)
  # Close the connection + delete the temp DB when the calling test finishes.
  withr::defer({
    fk$rc$close()
    unlink(fk$state$path)
  }, envir = parent.frame())
  list(handle = handle, state = fk$state, rc = fk$rc)
}

# --- (a) AUTO-RECONNECT: dead connection is transparently re-established -------

test_that("(a) .conn() revalidates and reconnects a closed connection", {
  h <- make_reconnect_handle(15)
  # Kill the underlying connection out from under the handle.
  DBI::dbDisconnect(h$state$conn)
  expect_false(DBI::dbIsValid(h$state$conn))

  conn <- dsOMOP:::.conn(h$handle)
  expect_true(DBI::dbIsValid(conn))
  # The freshly resolved connection is cached back into handle$conn.
  expect_identical(h$handle$conn, conn)
  # A query through it returns the right data.
  n <- DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM person")$n
  expect_equal(n, 15)
})

test_that("(a) .executeQuery transparently reconnects after a connection-class error", {
  h <- make_reconnect_handle(15)
  before <- h$state$connects
  # Realistic dead-pool case: the live connection has died, but the pool hands
  # it back once as if alive. The first query fails with a connection-class
  # error ("invalid or closed connection"); .withDbReconnect then reconnects.
  DBI::dbDisconnect(h$state$conn)
  h$state$hand_back_dead <- TRUE

  res <- dsOMOP:::.executeQuery(h$handle, "SELECT COUNT(*) AS n FROM person")
  expect_equal(res$n, 15)                       # correct data after reconnect
  expect_true(h$state$connects > before)        # a real reconnect happened
})

test_that("(a) .withDbReconnect retries fn ONCE and returns its value on recovery", {
  h <- make_reconnect_handle(15)
  attempts <- 0L
  out <- dsOMOP:::.withDbReconnect(h$handle, function(conn) {
    attempts <<- attempts + 1L
    if (attempts == 1L) stop("connection closed")   # connection-class error
    DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM person")$n
  })
  expect_equal(out, 15)
  expect_equal(attempts, 2L)   # exactly one retry
})

# --- (b) ONE-SHOT: a persistent connection failure propagates (no loop) --------

test_that("(b) a persistent connection failure is retried exactly once, then propagates", {
  h <- make_reconnect_handle(15)
  attempts <- 0L
  expect_error(
    dsOMOP:::.withDbReconnect(h$handle, function(conn) {
      attempts <<- attempts + 1L
      stop("server closed the connection unexpectedly")  # always fails
    }),
    "server closed the connection"
  )
  expect_equal(attempts, 2L)   # initial + one retry only -> no infinite loop
})

# --- (c) NON-connection SQL/logic errors are NOT retried -----------------------

test_that("(c) a non-connection (SQL/logic) error is not retried and propagates", {
  h <- make_reconnect_handle(15)
  attempts <- 0L
  expect_error(
    dsOMOP:::.withDbReconnect(h$handle, function(conn) {
      attempts <<- attempts + 1L
      stop("near \"SELCT\": syntax error")   # genuine SQL error
    }),
    "syntax error"
  )
  expect_equal(attempts, 1L)   # no retry for a genuine error
})

test_that("(c) .isConnectionError classifies connection vs logic errors correctly", {
  ce <- dsOMOP:::.isConnectionError
  expect_true(ce(simpleError("server closed the connection unexpectedly")))
  expect_true(ce(simpleError("Lost connection to MySQL server during query")))
  expect_true(ce(simpleError("ORA-03113: end-of-file on communication channel")))
  expect_true(ce(simpleError("could not connect to server")))
  expect_true(ce(simpleError("SQLSTATE 08006 connection failure")))
  # Must NOT treat genuine SQL/logic errors as connection errors.
  expect_false(ce(simpleError("near \"SELCT\": syntax error")))
  expect_false(ce(simpleError("no such table: person")))
  expect_false(ce(simpleError("column foo does not exist")))
  expect_false(ce(simpleError("UNIQUE constraint failed")))
})

# --- (d) TEMP-TABLE FAIL-CLOSED: dropped cohort table must STOP, not under-count

test_that("(d) a vanished temp table with tracked temp_tables FAILS CLOSED", {
  h <- make_reconnect_handle(15)
  # Pretend a cohort temp table was created earlier this session.
  h$handle$temp_tables <- "dsomop_cohort_1"
  # The reconnect "dropped" it: any query against it raises a missing-object
  # error. We must STOP with the actionable message, NOT return empty.
  expect_error(
    dsOMOP:::.withDbReconnect(h$handle, function(conn) {
      DBI::dbGetQuery(conn, "SELECT COUNT(DISTINCT subject_id) AS n FROM dsomop_cohort_1")
    }),
    "temporary cohort/working table"
  )
})

test_that("(d) fail-closed fires on the FIRST attempt too (lazy reconnect path)", {
  # Models: .conn() silently handed back a fresh connection, so the very first
  # attempt hits the already-vanished temp table. Still must fail closed.
  h <- make_reconnect_handle(15)
  h$handle$temp_tables <- "dsomop_cohort_7"
  attempts <- 0L
  expect_error(
    dsOMOP:::.withDbReconnect(h$handle, function(conn) {
      attempts <<- attempts + 1L
      stop("no such table: dsomop_cohort_7")
    }),
    "temporary cohort/working table"
  )
  # First-attempt fail-closed: never retried (a retry can't bring the table back).
  expect_equal(attempts, 1L)
})

test_that("(d) the per-patient gate FAILS CLOSED when its cohort table vanished", {
  # Critical: prove a dropped cohort table cannot slip the gate by under-counting.
  h <- make_reconnect_handle(15)
  h$handle$temp_tables <- "dsomop_cohort_1"
  withr::with_options(list(nfilter.subset = 5), {
    expect_error(
      dsOMOP:::.assertMinPersons(
        handle = h$handle,
        sql = "SELECT COUNT(DISTINCT subject_id) AS n FROM dsomop_cohort_1"),
      "temporary cohort/working table"
    )
  })
})

test_that("(d) a genuine missing CDM table with NO temp tables propagates raw", {
  # When dsOMOP is NOT tracking temp tables, a missing object is a real schema
  # problem and must surface unmasked (not as the expired-cohort message).
  h <- make_reconnect_handle(15)
  expect_length(h$handle$temp_tables, 0)
  expect_error(
    dsOMOP:::.withDbReconnect(h$handle, function(conn) {
      DBI::dbGetQuery(conn, "SELECT * FROM no_such_cdm_table")
    }),
    "no such table"
  )
})

test_that("(d) .isMissingObjectError detects vanished objects, not connection loss", {
  mo <- dsOMOP:::.isMissingObjectError
  expect_true(mo(simpleError("no such table: dsomop_cohort_1")))
  expect_true(mo(simpleError('relation "cohort" does not exist')))
  expect_true(mo(simpleError("ORA-00942: table or view does not exist")))
  expect_false(mo(simpleError("server closed the connection unexpectedly")))
})

# --- (e) STABLE KEY: tokens identical before and after a reconnect -------------

test_that("(e) person tokens are identical before and after a reconnect", {
  h <- make_reconnect_handle(15)
  ids <- c("1", "2", "9007199254740993", "42")

  tokens_before <- dsOMOP:::.hashPersonKey(ids, h$handle$person_key)

  # Force a real reconnect cycle through the helper.
  DBI::dbDisconnect(h$state$conn)
  h$state$hand_back_dead <- TRUE
  invisible(dsOMOP:::.executeQuery(h$handle, "SELECT COUNT(*) AS n FROM person"))
  expect_true(DBI::dbIsValid(dsOMOP:::.conn(h$handle)))   # reconnected

  # The key lives on the handle, not the connection: it is unchanged, so tokens
  # match exactly -> joins stay consistent across the reconnect.
  tokens_after <- dsOMOP:::.hashPersonKey(ids, h$handle$person_key)
  expect_identical(tokens_before, tokens_after)
  # And the reverse map still recovers the original ids (server-side only).
  expect_identical(
    dsOMOP:::.unhashPersonKey(tokens_after, h$handle$person_key),
    ids
  )
})

test_that("(e) a fresh handle for the same resource re-derives the same key", {
  # A brand-new handle (e.g. after a full reconnect) re-resolves the key from a
  # stable source (R option) and reproduces identical tokens for the same ids.
  withr::local_options(list(dsomop.pseudonym_key = "00112233445566778899aabbccddeeff"))
  withr::local_envvar(c(DSOMOP_PSEUDONYM_KEY = ""))
  rc <- list(
    getResource = function() list(url = "datashield://siteX/omop", name = NULL),
    getParsed   = function() list(server = "siteX")
  )
  k1 <- dsOMOP:::.resolvePersonKey(rc)
  k2 <- dsOMOP:::.resolvePersonKey(rc)
  ids <- c("1", "2", "3")
  expect_identical(
    dsOMOP:::.hashPersonKey(ids, k1),
    dsOMOP:::.hashPersonKey(ids, k2)
  )
})
