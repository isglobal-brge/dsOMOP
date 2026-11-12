# The data generation of the fixed synopses (dsOMOP 2.10.3, review R02 / §5):
# a keyed digest of the exact multiset of person identifiers, recomputed
# inside every fixed-synopsis request's own read snapshot, cached nowhere (no
# per-handle, per-session or negative cache). Ports of Astra's re-review
# generation probes (astra-scratch/rereview-root/.../test-rereview-generation.R)
# are marked. Helpers in helper-tools-release.R.

# A test CDM in a database file, so separate connections ("workers") reach
# the same data.
.gen_handle <- function(path, n_persons = 80) {
  if (!file.exists(path)) {
    conn <- create_test_omop_db(path, n_persons = n_persons)
    DBI::dbDisconnect(conn)
  }
  h <- create_test_handle(n_persons = 1)
  DBI::dbDisconnect(h$conn)
  h$conn <- DBI::dbConnect(RSQLite::SQLite(), path)
  h
}

# A new R session: the in-memory runtime is gone.
.gen_restart <- function() {
  .pkg_state$dp_status <- NULL
  .pkg_state$dp_runtime <- NULL
  .pkg_state$dp_bootstrap_in_progress <- FALSE
}

.gen_id <- function(h) {
  ready <- suppressMessages(.dsomopToolsDpReady(h))
  .dsomopDpDataGeneration(h, ready$policy)$id
}

.gen_release <- function(h) {
  h$blueprint <- NULL
  .release_run(h, "dsomop:person.gender_by_age_decade")
}

# Exported signed components by stratum.
.gen_components <- function(x) {
  owner <- attr(x, "dsomop_release")$owners[[1L]]
  stats::setNames(owner$components,
                  vapply(owner$components, `[[`, character(1L), "stratum"))
}

test_that("fixed synopses need no custodial configuration", {
  .release_state(snapshot_id = "")
  h <- create_test_handle(n_persons = 80)
  on.exit(cleanup_handle(h), add = TRUE)
  out <- .gen_release(h)
  label <- attr(out, "dsomop_protection")
  expect_identical(label$protection, "fixed_synopsis")
  expect_identical(label$data_generation, "person_membership_digest")
  expect_match(label$data_generation_residual, "Clinical-only changes")
  expect_identical(label$read_snapshot, "serializable_transaction")
  expect_identical(label$delta, 0)
  generation <- .gen_id(h)
  expect_match(generation, "^[0-9a-f]{64}$")
  status <- suppressMessages(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL))
  expect_identical(status$data_generation, "person_membership_digest")
  expect_match(status$data_generation_basis, "person_id")
  expect_match(status$snapshot_id, "^cdm_[0-9a-f]{64}$")
  # The generation value is never released.
  released <- paste(c(
    capture.output(str(unclass(out))), capture.output(str(label)),
    capture.output(str(attr(out, "dsomop_release"))),
    capture.output(str(status))), collapse = "\n")
  expect_false(grepl(generation, released, fixed = TRUE))
  # DP-only mode answers the same fixed synopsis.
  withr::local_options(list(dsomop.tools.mode = "dp"))
  dp <- .gen_release(h)
  expect_identical(attr(dp, "dsomop_protection")$protection, "fixed_synopsis")
  expect_identical(dp$n_persons, out$n_persons)
})

test_that("the membership digest is stable across stream page sizes", {
  .release_state(snapshot_id = "")
  h <- create_test_handle(n_persons = 37)
  on.exit(cleanup_handle(h), add = TRUE)
  streamed <- .dsomopDpMembershipHash(h, "stream")
  expect_identical(streamed$method, "stream")
  expect_match(streamed$digest, "^[0-9a-f]{64}$")
  expect_identical(streamed$n, "37")
  # Keyset pages: a page smaller than the table gives the same digest.
  expect_identical(
    .dsomopDpMembershipHash(h, "stream", page = 5L)[c("n", "digest")],
    streamed[c("n", "digest")])
  # Duplicated person rows count with their multiplicity (a multiset).
  DBI::dbExecute(h$conn, "CREATE TABLE person_dup AS SELECT * FROM person")
  DBI::dbExecute(h$conn, "DROP TABLE person")
  DBI::dbExecute(h$conn, paste(
    "CREATE TABLE person AS SELECT * FROM person_dup UNION ALL",
    "SELECT * FROM person_dup WHERE person_id = 7"))
  dup <- .dsomopDpMembershipHash(h, "stream")
  expect_identical(dup$n, "38")
  expect_false(identical(dup$digest, streamed$digest))
})

test_that("Astra probe: the same current data replays on an old and a recreated handle", {
  .release_state(snapshot_id = "")
  path <- tempfile(fileext = ".sqlite")
  h <- .gen_handle(path)
  on.exit(cleanup_handle(h), add = TRUE)
  original <- .gen_release(h)
  g0 <- .gen_id(h)
  # An unrelated clinical row: the person set, hence the generation and every
  # unchanged component, stay the same (review §5).
  .release_add_condition(h, 1, 201820, "2020-01-01")
  old <- .gen_release(h)
  expect_identical(.gen_id(h), g0)
  expect_identical(old$n_persons, original$n_persons)
  new <- .gen_handle(path)
  on.exit(cleanup_handle(new), add = TRUE)
  current <- .gen_release(new)
  expect_identical(.gen_id(new), g0)
  expect_identical(current$n_persons, old$n_persons)
})

test_that("Astra probe: other handles cannot redraw an unchanged request", {
  .release_state(snapshot_id = "")
  path <- tempfile(fileext = ".sqlite")
  h <- .gen_handle(path)
  on.exit(cleanup_handle(h), add = TRUE)
  .gen_release(h)
  .release_add_condition(h, 1, 201820, "2020-01-01")
  before <- .gen_release(h)
  others <- lapply(seq_len(16), function(i) .gen_handle(path))
  on.exit(lapply(others, cleanup_handle), add = TRUE)
  invisible(lapply(others, .gen_id))
  after <- .gen_release(h)
  expect_identical(after$n_persons, before$n_persons)
  # Nothing is retained in package state between requests.
  expect_null(.pkg_state$dp_generations)
  expect_null(h$read_snapshot)
})

test_that("Astra probe: a balanced person replacement is a new generation", {
  .release_state(snapshot_id = "")
  h <- create_test_handle(n_persons = 80)
  on.exit(cleanup_handle(h), add = TRUE)
  before <- .gen_id(h)
  DBI::dbExecute(h$conn, "DELETE FROM person WHERE person_id = 2")
  DBI::dbExecute(h$conn, "INSERT INTO person VALUES (-2, 8532, 1990, 1, 1, 8527, NULL)")
  .gen_restart()
  expect_false(identical(.gen_id(h), before))
  # Same count and an unchanged largest identifier, yet detected.
  expect_identical(
    DBI::dbGetQuery(h$conn, "SELECT COUNT(*) AS n FROM person")$n, 80L)
})

test_that("a person-set change redraws every fixed synopsis; equal data restore equal answers", {
  .release_state(snapshot_id = "")
  path <- tempfile(fileext = ".sqlite")
  h <- .gen_handle(path)
  on.exit(cleanup_handle(h), add = TRUE)
  before <- .gen_release(h)
  g0 <- .gen_id(h)
  b <- .gen_components(before)
  # A new woman born in 1990: the very next request (no session restart, no
  # new handle) sees the new generation.
  DBI::dbExecute(h$conn, paste(
    "INSERT INTO person VALUES (900001, 8532, 1990, 1, 1, 8527, NULL)"))
  after <- .gen_release(h)
  expect_false(identical(.gen_id(h), g0))
  a <- .gen_components(after)
  untouched <- setdiff(names(b), "gender=8532")
  contrasts <- function(x) {
    vapply(untouched[-1L], function(s) x[[s]]$n - x[[untouched[[1L]]]]$n,
           numeric(1L))
  }
  expect_false(isTRUE(all.equal(contrasts(a), contrasts(b))))
  # Removing her restores the data, the generation and every answer.
  DBI::dbExecute(h$conn, "DELETE FROM person WHERE person_id = 900001")
  expect_identical(.gen_id(h), g0)
  expect_identical(.gen_release(h)$n_persons, before$n_persons)
})

test_that("an in-place clinical edit keeps the generation; the edited components redraw", {
  .release_state(snapshot_id = "")
  path <- tempfile(fileext = ".sqlite")
  h <- .gen_handle(path)
  on.exit(cleanup_handle(h), add = TRUE)
  before <- .gen_release(h)
  g0 <- .gen_id(h)
  # The documented residual: the person set is unchanged.
  DBI::dbExecute(h$conn, paste(
    "UPDATE person SET year_of_birth = year_of_birth + 30",
    "WHERE person_id = 2"))
  after <- .gen_release(h)
  expect_identical(.gen_id(h), g0)
  gender <- DBI::dbGetQuery(h$conn, paste(
    "SELECT gender_concept_id FROM person WHERE person_id = 2"))[[1L]]
  edited <- paste0("gender=", gender)
  b <- .gen_components(before)
  a <- .gen_components(after)
  expect_false(isTRUE(all.equal(diff(a[[edited]]$h), diff(b[[edited]]$h))))
  for (s in setdiff(names(b), edited)) {
    expect_equal(diff(a[[s]]$h), diff(b[[s]]$h), tolerance = 1e-9, info = s)
  }
})

test_that("one request reads the generation once; the next request reads it again", {
  .release_state(snapshot_id = "")
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  calls <- 0L
  original <- .dsomopDpMembershipHash
  local_mocked_bindings(.dsomopDpMembershipHash = function(handle, ...) {
    calls <<- calls + 1L
    original(handle, ...)
  })
  policy <- suppressMessages(.dsomopToolsDpReady(h))$policy
  .dsomopWithReadSnapshot(h, {
    a <- .dsomopDpDataGeneration(h, policy)
    b <- .dsomopDpDataGeneration(h, policy)
  })
  expect_identical(calls, 1L)
  expect_identical(a, b)
  .gen_release(h)
  .gen_release(h)
  expect_identical(calls, 3L)
})

test_that("a failed generation read fails only that request", {
  .release_state(snapshot_id = "")
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  expected <- .gen_release(h)
  fail <- TRUE
  original <- .dsomopDpMembershipHash
  local_mocked_bindings(.dsomopDpMembershipHash = function(handle, ...) {
    if (fail) stop("simulated read failure WHERE person_id > 987654321")
    original(handle, ...)
  })
  err <- tryCatch(suppressMessages(.gen_release(h)),
                  error = function(e) conditionMessage(e))
  expect_match(err, "could not be prepared for this request")
  expect_false(grepl("simulated", err, fixed = TRUE))
  messages <- capture.output(tryCatch(.gen_release(h), error = function(e) NULL),
                              type = "message")
  expect_false(any(grepl("987654321|WHERE person_id", messages)))
  # No silent route change and no negative cache: the next request answers
  # the same fixed synopsis.
  fail <- FALSE
  again <- .gen_release(h)
  expect_identical(attr(again, "dsomop_protection")$protection,
                   "fixed_synopsis")
  expect_identical(again$n_persons, expected$n_persons)
  # DP-only mode fails the same way while the read fails.
  fail <- TRUE
  withr::local_options(list(dsomop.tools.mode = "dp"))
  err <- tryCatch(suppressMessages(.gen_release(h)),
                  error = function(e) conditionMessage(e))
  expect_match(err, "could not be prepared for this request")
})

test_that("concurrent workers on one database give equal answers", {
  skip_on_os("windows")
  .release_state(snapshot_id = "")
  path <- tempfile(fileext = ".sqlite")
  first <- .gen_handle(path)
  on.exit(cleanup_handle(first), add = TRUE)
  expected <- .gen_release(first)
  answers <- parallel::mclapply(1:4, function(i) {
    h <- .gen_handle(path)
    on.exit(DBI::dbDisconnect(h$conn))
    .gen_restart()
    .gen_release(h)$n_persons
  }, mc.cores = 2L)
  for (answer in answers) expect_identical(answer, expected$n_persons)
})

test_that("fixed noise is keyed on the generation and the contributions", {
  .release_state(snapshot_id = "")
  h <- create_test_handle(n_persons = 30)
  on.exit(cleanup_handle(h), add = TRUE)
  policy <- suppressMessages(.dsomopToolsDpReady(h))$policy
  keys <- .dsomopReleaseKeys(policy)
  component <- function(fp) list(
    owner = "owner/x", population = "population/x", family = "N",
    coordinate = "stratum=root", value = 40, allocation = 0.2,
    sensitivity = 1, fingerprint = fp)
  fp1 <- .dsomopReleaseFingerprint(keys, "owner/x", "N", "stratum=root",
                                   c("a", "b"), c(1, 1))
  fp2 <- .dsomopReleaseFingerprint(keys, "owner/x", "N", "stratum=root",
                                   c("a", "c"), c(1, 1))
  draw <- function(generation, fp, protection = "fixed_synopsis") {
    p <- policy
    p$data_generation <- generation
    .dsomopReleaseNoisyComponent(p, keys, protection, component(fp))
  }
  g1 <- strrep("1", 64L)
  g2 <- strrep("2", 64L)
  expect_identical(draw(g1, fp1), draw(g1, fp1))
  expect_false(identical(draw(g1, fp1), draw(g1, fp2)))
  expect_false(identical(draw(g1, fp1), draw(g2, fp1)))
  expect_error(draw(NULL, fp1), "lacks its data generation")
  expect_error(draw(g1, NULL), "lacks its contribution fingerprint")
  # The realised route never depends on the generation.
  expect_identical(draw(g1, fp1, "realised_sticky"),
                   draw(g2, fp1, "realised_sticky"))
})

test_that("Astra probe: the snapshot label changes no admission and no answer", {
  .release_state(snapshot_id = "label-a")
  h <- create_test_handle(n_persons = 80)
  on.exit(cleanup_handle(h), add = TRUE)
  a <- .gen_release(h)
  for (label in list("label-b", "", "not a valid label!")) {
    withr::with_options(list(dsomop.dp.snapshot_id = label), {
      expect_no_error(b <- .gen_release(h))
      expect_identical(attr(b, "dsomop_protection")$protection,
                       "fixed_synopsis")
      expect_identical(b$n_persons, a$n_persons)
    })
  }
})

test_that("Astra probe: a restored sealed table survives a label change", {
  .release_state(snapshot_id = "label-a")
  x <- .testPseudonymize(data.frame(person_id = seq_len(40),
                                    value = seq_len(40)))
  dataset <- .release_session_dataset()
  sealed <- .dsomopDpSealPersonLocal(x,
    producer = "query/measurement.load/0123456789abcdef",
    dataset_identity = dataset,
    data_generation = .release_loader_generation())
  a <- suppressMessages(omopDpReleaseDS(sealed, list(statistic = "count")))
  expect_identical(a$protection, "fixed_synopsis")
  for (label in list("label-b", "")) {
    .release_restart()
    withr::with_options(list(dsomop.dp.snapshot_id = label), {
      expect_no_error(b <- suppressMessages(
        omopDpReleaseDS(sealed, list(statistic = "count"))))
      expect_identical(b$components$signed, a$components$signed)
    })
  }
})

test_that("a pinned loader table carries its request's generation", {
  .release_state(snapshot_id = "")
  path <- tempfile(fileext = ".sqlite")
  h <- .gen_handle(path, n_persons = 120)
  on.exit(cleanup_handle(h), add = TRUE)
  h$person_key_identity <- "dsomop-generation-loader"
  .pkg_state$dp_handle <- h
  # The fixture's measurement table lacks columns the pinned loader selects.
  present <- DBI::dbGetQuery(h$conn, "PRAGMA table_info(measurement)")$name
  for (col in setdiff(c("measurement_datetime", "measurement_time",
                        "operator_concept_id", "visit_detail_id",
                        "provider_id"), present)) {
    DBI::dbExecute(h$conn, paste0("ALTER TABLE measurement ADD COLUMN ", col,
                                  if (grepl("_id$", col)) " INTEGER" else
                                    " TEXT"))
  }
  load <- function() {
    h$blueprint <- NULL
    suppressWarnings(.omopAnalysisRun(h, "dsomop:measurement.load",
                                      list(), assign = TRUE))
  }
  table <- load()
  capsule <- attr(table, "dsomop_dp_provenance")
  skip_if(is.null(capsule), "the fixture loader is not sealed in this setup")
  expect_identical(capsule$data_generation, .gen_id(h))
  expect_identical(capsule$read_snapshot, "serializable_transaction")
  count <- suppressMessages(omopDpReleaseDS(table, list(statistic = "count")))
  expect_identical(count$protection, "fixed_synopsis")
  expect_identical(count$read_snapshot, "serializable_transaction")
  # A changed live membership cannot certify old loader rows as a fixed basis.
  DBI::dbExecute(h$conn, paste(
    "INSERT INTO person VALUES (900002, 8532, 1990, 1, 1, 8527, NULL)"))
  again <- suppressMessages(omopDpReleaseDS(table, list(statistic = "count")))
  expect_identical(again$protection, "realised_sticky")
  # A fresh load of the changed database is another generation.
  fresh <- attr(load(), "dsomop_dp_provenance")
  expect_false(identical(fresh$data_generation, capsule$data_generation))
})


test_that("snapshot label options cannot select database metadata reads", {
  .release_state(snapshot_id = "")
  h <- create_test_handle(n_persons = 30)
  on.exit(cleanup_handle(h), add = TRUE)
  .pkg_state$dp_handle <- h
  # A read of metadata would abort some engines, even when caught. Policy
  # must derive its display label without touching any table.
  local_mocked_bindings(.listTablesRaw = function(...) stop("metadata probe forbidden"))
  policy <- .dsomopDpPolicy()
  for (label in list("", "display-only", "malformed label!")) {
    withr::with_options(list(dsomop.dp.snapshot_id = label), {
      current <- .dsomopDpPolicy()
      expect_identical(current$policy_hash, policy$policy_hash)
    })
  }
})
