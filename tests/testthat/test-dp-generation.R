# The automatic data generation of the fixed synopses (dsOMOP 2.10.3, after
# the review): a keyed fingerprint of every clinical table's exact row count
# and largest identifier, derived per R session and held in memory only. No
# custodial configuration is needed; dsomop.dp.snapshot_id is a deprecated
# display label. Helpers in helper-tools-release.R.

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

# A new R session: the in-memory runtime and generations are gone.
.gen_restart <- function() {
  .pkg_state$dp_status <- NULL
  .pkg_state$dp_runtime <- NULL
  .pkg_state$dp_generations <- NULL
  .pkg_state$dp_bootstrap_in_progress <- FALSE
}

.gen_id <- function(h) {
  ready <- suppressMessages(.dsomopToolsDpReady(h))
  .dsomopDpDataGeneration(h, ready$policy)
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
  expect_identical(label$data_generation, "automatic_session_fingerprint")
  expect_match(label$data_generation_residual, "in-place edit")
  expect_identical(label$delta, 0)
  generation <- .gen_id(h)
  expect_true(generation$available)
  expect_match(generation$id, "^[0-9a-f]{64}$")
  status <- suppressMessages(omopDpStatusDS())
  expect_identical(status$data_generation, "automatic_session_fingerprint")
  expect_match(status$data_generation_basis, "row count and largest")
  expect_match(status$snapshot_id, "^cdm_[0-9a-f]{64}$")
  # The generation value is never released.
  released <- paste(c(
    capture.output(str(unclass(out))), capture.output(str(label)),
    capture.output(str(attr(out, "dsomop_release"))),
    capture.output(str(status))), collapse = "\n")
  expect_false(grepl(generation$id, released, fixed = TRUE))
  # DP-only mode answers the same fixed synopsis.
  withr::local_options(list(dsomop.tools.mode = "dp"))
  dp <- .gen_release(h)
  expect_identical(attr(dp, "dsomop_protection")$protection, "fixed_synopsis")
  expect_identical(dp$n_persons, out$n_persons)
})

test_that("the generation is stable across sessions and workers, in memory", {
  state <- .release_state(snapshot_id = "")
  path <- tempfile(fileext = ".sqlite")
  a <- .gen_handle(path)
  on.exit(cleanup_handle(a), add = TRUE)
  first <- .gen_release(a)
  g1 <- .gen_id(a)$id
  # Another session on another connection (another R server worker).
  .gen_restart()
  b <- .gen_handle(path)
  on.exit(cleanup_handle(b), add = TRUE)
  expect_identical(.gen_id(b)$id, g1)
  expect_identical(.gen_release(b)$n_persons, first$n_persons)
  # Nothing is written: no store, table or cache file (the custodian injects
  # the root in these tests, so the state directory stays empty).
  expect_length(list.files(state, recursive = TRUE, all.files = TRUE), 0L)
  tables <- DBI::dbListTables(b$conn)
  expect_false(any(grepl("generation|release|dp_", tables)))
  # Another noise root is another generation.
  .gen_restart()
  withr::local_options(list(dsomop.dp.noise_root = strrep("7c", 32L)))
  expect_false(identical(.gen_id(b)$id, g1))
})

test_that("adding or removing rows redraws every fixed synopsis; equal data restore equal answers", {
  .release_state(snapshot_id = "")
  path <- tempfile(fileext = ".sqlite")
  h <- .gen_handle(path)
  on.exit(cleanup_handle(h), add = TRUE)
  before <- .gen_release(h)
  g0 <- .gen_id(h)$id
  b <- .gen_components(before)
  # A new woman born in 1990.
  DBI::dbExecute(h$conn, paste(
    "INSERT INTO person VALUES (900001, 8532, 1990, 1, 1, 8527, NULL)"))
  .gen_restart()
  after <- .gen_release(h)
  expect_false(identical(.gen_id(h)$id, g0))
  a <- .gen_components(after)
  # Every stratum redraws, also those the new person is not in: the
  # contrasts between untouched strata change too.
  untouched <- setdiff(names(b), "gender=8532")
  contrasts <- function(x) {
    vapply(untouched[-1L], function(s) x[[s]]$n - x[[untouched[[1L]]]]$n,
           numeric(1L))
  }
  expect_false(isTRUE(all.equal(contrasts(a), contrasts(b))))
  # Removing her again restores the data, the generation and every answer.
  DBI::dbExecute(h$conn, "DELETE FROM person WHERE person_id = 900001")
  .gen_restart()
  expect_identical(.gen_id(h)$id, g0)
  expect_identical(.gen_release(h)$n_persons, before$n_persons)
  # Removing an existing person, or adding an event row, is another
  # generation; a table outside the OMOP clinical tables is not.
  DBI::dbExecute(h$conn, "DELETE FROM person WHERE person_id = 3")
  .gen_restart()
  g_removed <- .gen_id(h)$id
  expect_false(identical(g_removed, g0))
  DBI::dbExecute(h$conn, paste(
    "INSERT INTO condition_occurrence (condition_occurrence_id, person_id,",
    "condition_concept_id, condition_start_date, condition_end_date,",
    "condition_type_concept_id) VALUES (990001, 1, 201820, '2020-01-01',",
    "'2020-01-01', 32020)"))
  .gen_restart()
  g_event <- .gen_id(h)$id
  expect_false(identical(g_event, g_removed))
  DBI::dbExecute(h$conn, "CREATE TABLE analyst_scratch (x INTEGER)")
  DBI::dbExecute(h$conn, "INSERT INTO analyst_scratch VALUES (1)")
  .gen_restart()
  expect_identical(.gen_id(h)$id, g_event)
})

test_that("within a session the generation is fixed and changed contributions still redraw", {
  .release_state(snapshot_id = "")
  path <- tempfile(fileext = ".sqlite")
  h <- .gen_handle(path)
  on.exit(cleanup_handle(h), add = TRUE)
  before <- .gen_release(h)
  g0 <- .gen_id(h)$id
  # A change while the session is open: the session keeps its generation
  # (derived once, in memory) ...
  DBI::dbExecute(h$conn, paste(
    "INSERT INTO person VALUES (900002, 8532, 1990, 1, 1, 8527, NULL)"))
  after <- .gen_release(h)
  expect_identical(.gen_id(h)$id, g0)
  b <- .gen_components(before)
  a <- .gen_components(after)
  # ... untouched strata keep their draws (their contrasts and their cells'
  # contrasts are unchanged under the uniform reconciliation shift), but the
  # changed stratum is redrawn: its contrast moves by more than the new
  # person, so no exact difference is released.
  untouched <- setdiff(names(b), "gender=8532")
  for (s in untouched[-1L]) {
    expect_equal(a[[s]]$n - a[[untouched[[1L]]]]$n,
                 b[[s]]$n - b[[untouched[[1L]]]]$n, tolerance = 1e-9, info = s)
    expect_equal(diff(a[[s]]$h), diff(b[[s]]$h), tolerance = 1e-9, info = s)
  }
  moved <- (a[["gender=8532"]]$n - a[[untouched[[1L]]]]$n) -
    (b[["gender=8532"]]$n - b[[untouched[[1L]]]]$n)
  expect_false(isTRUE(all.equal(moved, 1)))
})

test_that("an in-place edit keeps the generation; the edited components redraw", {
  .release_state(snapshot_id = "")
  path <- tempfile(fileext = ".sqlite")
  h <- .gen_handle(path)
  on.exit(cleanup_handle(h), add = TRUE)
  before <- .gen_release(h)
  g0 <- .gen_id(h)$id
  # The documented residual: counts and largest identifiers are unchanged.
  DBI::dbExecute(h$conn, paste(
    "UPDATE person SET year_of_birth = year_of_birth + 30",
    "WHERE person_id = 2"))
  .gen_restart()
  after <- .gen_release(h)
  expect_identical(.gen_id(h)$id, g0)
  gender <- DBI::dbGetQuery(h$conn, paste(
    "SELECT gender_concept_id FROM person WHERE person_id = 2"))[[1L]]
  edited <- paste0("gender=", gender)
  b <- .gen_components(before)
  a <- .gen_components(after)
  # The edited person's stratum histogram is redrawn (not shifted by the
  # exact move); the other strata keep their cells' draws.
  expect_false(isTRUE(all.equal(diff(a[[edited]]$h), diff(b[[edited]]$h))))
  for (s in setdiff(names(b), edited)) {
    expect_equal(diff(a[[s]]$h), diff(b[[s]]$h), tolerance = 1e-9, info = s)
  }
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
  draw <- function(generation, fp) {
    p <- policy
    p$data_generation <- generation
    .dsomopReleaseNoisyComponent(p, keys, "fixed_synopsis", component(fp))
  }
  g1 <- strrep("1", 64L)
  g2 <- strrep("2", 64L)
  expect_identical(draw(g1, fp1), draw(g1, fp1))
  expect_false(identical(draw(g1, fp1), draw(g1, fp2)))
  expect_false(identical(draw(g1, fp1), draw(g2, fp1)))
  expect_error(draw(NULL, fp1), "lacks its data generation")
  expect_error(draw(g1, NULL), "lacks its contribution fingerprint")
  # The realised route never depends on the data generation.
  realised <- function(generation) {
    p <- policy
    p$data_generation <- generation
    .dsomopReleaseNoisyComponent(p, keys, "realised_sticky", component(fp1))
  }
  expect_identical(realised(g1), realised(g2))
})

test_that("dsomop.dp.snapshot_id is deprecated: a display label only", {
  .release_state(snapshot_id = "")
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  plain <- .gen_release(h)
  g0 <- .gen_id(h)$id
  .gen_restart()
  withr::local_options(list(dsomop.dp.snapshot_id = "etl-2026-11-12"))
  expect_identical(.gen_id(h)$id, g0)
  labelled <- .gen_release(h)
  expect_identical(labelled$n_persons, plain$n_persons)
  expect_identical(
    attr(labelled, "dsomop_protection")$protection, "fixed_synopsis")
  expect_identical(suppressMessages(omopDpStatusDS())$snapshot_id,
                   "etl-2026-11-12")
})

test_that("an underivable generation falls back to realised; DP-only refuses", {
  .release_state(snapshot_id = "")
  h <- create_test_handle(n_persons = 60)
  on.exit(cleanup_handle(h), add = TRUE)
  calls <- 0L
  local_mocked_bindings(.dsomopDpGenerationSnapshot = function(handle) {
    calls <<- calls + 1L
    stop("simulated read failure")
  })
  expect_message(out <- .gen_release(h), "could not be derived")
  expect_identical(attr(out, "dsomop_protection")$protection,
                   "realised_sticky")
  # The failure holds for the session: no partial or second value.
  again <- .gen_release(h)
  expect_identical(again$n_persons, out$n_persons)
  expect_identical(calls, 1L)
  withr::local_options(list(dsomop.tools.mode = "dp"))
  err <- tryCatch(.gen_release(h), error = function(e) conditionMessage(e))
  expect_match(err, "no fixed-synopsis protection")
  expect_false(grepl("simulated", err, fixed = TRUE))
})

test_that("typed loader tables use their own source database's generation", {
  .release_state(snapshot_id = "")
  dataset <- .release_session_dataset()
  value <- .testPseudonymize(data.frame(
    person_id = rep(seq_len(40), each = 2L), value = seq_len(80)))
  seal <- function(identity) .dsomopDpSealPersonLocal(
    value, producer = "query/measurement.load/0123456789abcdef",
    dataset_identity = identity)
  own <- suppressMessages(omopDpReleaseDS(seal(dataset),
                                          list(statistic = "count")))
  expect_identical(own$protection, "fixed_synopsis")
  expect_identical(own$data_generation, "automatic_session_fingerprint")
  other <- utils::modifyList(dataset,
                             list(resource_identity = "another-database"))
  foreign <- suppressMessages(omopDpReleaseDS(seal(other),
                                              list(statistic = "count")))
  expect_identical(foreign$protection, "realised_sticky")
})
