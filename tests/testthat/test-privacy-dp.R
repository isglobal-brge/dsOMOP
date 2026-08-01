.dp_environment_names <- c(
  "DSOMOP_DP_ENABLED", "DSOMOP_DP_DOMAIN", "DSOMOP_DP_SNAPSHOT_ID",
  "DSOMOP_DP_ACCOUNTING_MODE", "DSOMOP_DP_TOTAL_EPSILON",
  "DSOMOP_DP_RELEASE_EPSILON", "DSOMOP_DP_PRIVACY_EPOCH",
  "DSOMOP_DP_REQUIRE_EXTERNAL_ANCHOR", "DSOMOP_DP_ANCHOR_PROVIDER",
  "DSOMOP_DP_NOISE_PROVIDER", "DSOMOP_DP_NOISE_REQUIRE_EXISTING",
  "DSOMOP_DP_LEDGER_PROVIDER", "DSOMOP_DP_LEDGER_REQUIRE_EXISTING",
  "DSOMOP_DP_MAX_LEVELS", "DSOMOP_DP_MAX_CONTRIBUTIONS",
  "DSOMOP_DP_NUMERIC_GRID", "DSOMOP_DP_LEDGER_PATH",
  "DSOMOP_DP_NOISE_ROOT", "DSOMOP_DP_LEDGER_ROOT"
)

.dp_local_state <- function(accounting_mode = "bounded_accounted",
                            total_epsilon = 1,
                            release_epsilon = 0.1,
                            enabled = TRUE,
                            require_external_anchor = FALSE,
                            anchor_provider = NULL,
                            .local_envir = parent.frame()) {
  state <- withr::local_tempdir(
    pattern = "dsomop-dp-", .local_envir = .local_envir
  )
  Sys.chmod(state, mode = "0700")
  state <- normalizePath(state, winslash = "/", mustWork = TRUE)
  dp_environment <- stats::setNames(
    rep.int(NA_character_, length(.dp_environment_names)),
    .dp_environment_names
  )
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state,
    DSOMOP_MARKER_DIR = NA_character_,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1",
    dp_environment
  ), .local_envir = .local_envir)
  withr::local_options(list(
    dsomop.state_dir = state,
    dsomop.dp.enabled = enabled,
    dsomop.dp.domain = "dsomop-dp-test",
    dsomop.dp.snapshot_id = "etl-2026-08-01",
    dsomop.dp.accounting_mode = accounting_mode,
    dsomop.dp.total_epsilon = total_epsilon,
    dsomop.dp.release_epsilon = release_epsilon,
    dsomop.dp.privacy_epoch = 1L,
    dsomop.dp.require_external_anchor = require_external_anchor,
    dsomop.dp.anchor_provider = anchor_provider,
    dsomop.dp.ledger_path = NULL,
    dsomop.dp.noise_provider = "file",
    dsomop.dp.noise_root = NULL,
    dsomop.dp.noise_require_existing = FALSE,
    dsomop.dp.ledger_provider = "file",
    dsomop.dp.ledger_root = NULL,
    dsomop.dp.ledger_require_existing = FALSE,
    dsomop.dp.max_levels = 1000L,
    dsomop.dp.max_contributions = 10L,
    dsomop.dp.numeric_grid = 65535L
  ), .local_envir = .local_envir)
  previous_runtime <- list(
    status = .pkg_state$dp_status,
    binding = .pkg_state$dp_bootstrap_binding,
    in_progress = .pkg_state$dp_bootstrap_in_progress,
    cache = .pkg_state$dp_ledger_cache
  )
  withr::defer({
    .pkg_state$dp_status <- previous_runtime$status
    .pkg_state$dp_bootstrap_binding <- previous_runtime$binding
    .pkg_state$dp_bootstrap_in_progress <- previous_runtime$in_progress
    .pkg_state$dp_ledger_cache <- previous_runtime$cache
  }, envir = .local_envir)
  .pkg_state$dp_status <- NULL
  .pkg_state$dp_bootstrap_binding <- NULL
  .pkg_state$dp_bootstrap_in_progress <- FALSE
  .pkg_state$dp_ledger_cache <- new.env(parent = emptyenv())
  state
}

.dp_dataset_identity <- function(resource_identity = "test-resource",
                                  cdm_schema = "main") {
  list(
    resource_identity = resource_identity,
    target_dialect = "postgresql",
    cdm_schema = cdm_schema,
    vocabulary_schema = cdm_schema,
    results_schema = cdm_schema
  )
}

.dp_restart_runtime <- function() {
  .pkg_state$dp_status <- NULL
  .pkg_state$dp_bootstrap_binding <- NULL
  .pkg_state$dp_bootstrap_in_progress <- FALSE
  .pkg_state$dp_ledger_cache <- new.env(parent = emptyenv())
  invisible(NULL)
}

.dp_seal <- function(x, producer, ..., dataset_identity =
                     .dp_dataset_identity()) {
  .dsomopDpSealPersonLocal(
    x, producer = producer, ..., dataset_identity = dataset_identity
  )
}

.dp_seal_plan <- function(x, plan, output_name, dataset_identity =
                          .dp_dataset_identity()) {
  .dsomopDpSealPlanOutput(
    x, plan, output_name, dataset_identity = dataset_identity
  )
}

.dp_capsule <- function(x) {
  .dsomopDpVerifyPersonLocal(x)
}

.dp_lineage <- function(x) {
  .dp_capsule(x)$lineage_id
}

.dp_test_table <- function(reverse_rows = FALSE) {
  value <- data.frame(
    person_id = c(1, 1, 1, 2, 2, 3, 3),
    category = c("c", "a", "b", "a", "c", "b", "b"),
    measurement = c(9, 1, 4, 2, 8, 3, 7),
    event_date = as.Date(c(
      "2020-12-31", "2020-01-01", "2020-06-01",
      "2020-02-01", "2020-08-01", "2020-03-01", "2020-09-01"
    )),
    flag = c("yes", "no", "no", "no", "yes", "no", "no"),
    stringsAsFactors = FALSE
  )
  if (reverse_rows) value <- value[rev(seq_len(nrow(value))), , drop = FALSE]
  value <- .testPseudonymize(value)
  if (.dsomopDpEnabled()) {
    value <- .dp_seal(value, producer = "test/fixture")
  }
  value
}

.dp_ledger_rows <- function(state) {
  connection <- DBI::dbConnect(
    RSQLite::SQLite(), file.path(state, "privacy", "ledger.sqlite")
  )
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  DBI::dbGetQuery(connection, "SELECT * FROM dp_releases ORDER BY release_index")
}

.dp_ledger_meta <- function(state) {
  connection <- DBI::dbConnect(
    RSQLite::SQLite(), file.path(state, "privacy", "ledger.sqlite")
  )
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  values <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT key, value FROM dp_meta",
      "WHERE key IN ('ledger_id', 'next_index', 'spent_epsilon')"
    )
  )
  stats::setNames(values$value, values$key)
}

.dp_external_anchor <- function() {
  storage <- new.env(parent = emptyenv())
  storage$value <- NULL
  function(action, anchor_id, ...) {
    if (identical(action, "capabilities")) {
      return(list(
        schema_version = 1L,
        provider_id = "test-linearizable-anchor",
        external = TRUE,
        durable = TRUE,
        linearizable_cas = TRUE
      ))
    }
    if (identical(action, "read")) return(storage$value)
    if (identical(action, "compare_and_swap")) {
      arguments <- list(...)
      swapped <- identical(storage$value, arguments$expected)
      if (swapped) storage$value <- arguments$replacement
      return(list(swapped = swapped, state = storage$value))
    }
    stop("unsupported test anchor action")
  }
}

test_that("DP canonical encoding has a stable KAT and normalizes object order", {
  expect_silent(.dsomopDpCanonicalSelfTest())

  left <- list(
    z = 1L,
    nested = list(levels = c("a", "b"), zero = -0, enabled = TRUE)
  )
  right <- list(
    nested = list(enabled = TRUE, zero = 0, levels = c("a", "b")),
    z = 1
  )
  expect_identical(.dsomopDpCanonicalJson(left),
                   .dsomopDpCanonicalJson(right))

  .dp_local_state()
  policy <- .dsomopDpPolicy()
  forward <- .dsomopDpAnalysis(
    .dp_test_table(),
    list(
      statistic = "categorical_histogram",
      variable = "category",
      levels = c("c", "a", "b"),
      reducer = "presence",
      max_contributions = 2L,
      population_id = "cohort-a"
    ),
    policy
  )
  reverse <- .dsomopDpAnalysis(
    .dp_test_table(reverse_rows = TRUE),
    list(
      max_contributions = 2L,
      reducer = "presence",
      levels = c("b", "c", "a"),
      variable = "category",
      statistic = "categorical_histogram",
      population_id = "cohort-a"
    ),
    policy
  )
  expect_identical(forward$semantic, reverse$semantic)
  expect_identical(forward$snapshot, reverse$snapshot)
  expect_identical(forward$semantic$levels, c("a", "b", "c"))
})

test_that("DP bootstrap settings can be supplied before namespace load", {
  withr::local_options(list(
    dsomop.dp.enabled = NULL,
    dsomop.dp.total_epsilon = NULL,
    dsomop.dp.anchor_provider = NULL,
    default.dsomop.dp.enabled = FALSE,
    default.dsomop.dp.total_epsilon = 1
  ))
  dp_environment <- stats::setNames(
    rep.int(NA_character_, length(.dp_environment_names)),
    .dp_environment_names
  )
  dp_environment[c(
    "DSOMOP_DP_ENABLED", "DSOMOP_DP_TOTAL_EPSILON",
    "DSOMOP_DP_ANCHOR_PROVIDER"
  )] <- c("true", "2.5", "stats::runif")
  withr::local_envvar(dp_environment)
  expect_true(.dsomopDpEnabled())
  expect_identical(.dsomopDpOption("total_epsilon"), 2.5)
  expect_identical(.dsomopDpAnchorProvider(), getExportedValue("stats", "runif"))

  withr::local_options(list(
    dsomop.dp.enabled = TRUE,
    dsomop.dp.total_epsilon = 2.5
  ))
  expect_true(.dsomopDpEnabled())
  expect_identical(.dsomopDpOption("total_epsilon"), 2.5)
  withr::with_envvar(c(DSOMOP_DP_ENABLED = NA_character_), {
    expect_true(.dsomopDpEnabled())
  })

  withr::local_options(list(dsomop.dp.enabled = FALSE))
  expect_error(.dsomopDpEnabled(), "Conflicting DP option")
})

test_that("injected DP roots reject passphrases", {
  expect_error(
    .coerceDsomopDpRoot(strrep("not-a-csprng-root-", 2L), "DP noise root"),
    "passphrases are not accepted"
  )
  expect_identical(
    length(.coerceDsomopDpRoot(strrep("a1", 32L), "DP noise root")),
    32L
  )
})

test_that("HMAC stream endpoints remain strictly inside the unit interval", {
  key <- as.raw(rep.int(1L, 32L))
  context <- list(query = "endpoint-test")

  all_zero <- testthat::with_mocked_bindings(
    .dsomopDpUniform(key, context, 1L, 1L),
    .dsomopDpHmacRaw = function(key, value) raw(32L),
    .package = "dsOMOP"
  )
  all_one <- testthat::with_mocked_bindings(
    .dsomopDpUniform(key, context, 1L, 1L),
    .dsomopDpHmacRaw = function(key, value) as.raw(rep.int(255L, 32L)),
    .package = "dsOMOP"
  )

  expect_gt(all_zero, 0)
  expect_lt(all_zero, 1)
  expect_gt(all_one, 0)
  expect_lt(all_one, 1)
  expect_lt(all_zero, all_one)
})

test_that("disabled DP creates no state and refuses releases", {
  state <- .dp_local_state(enabled = FALSE)
  status <- .dsomopDpPublicStatus(initialize = TRUE)

  expect_false(status$enabled)
  expect_false(status$ready)
  expect_false(status$formal_dp)
  expect_false(status$sticky_noise)
  expect_false(dir.exists(file.path(state, "secrets")))
  expect_false(dir.exists(file.path(state, "privacy")))
  expect_error(
    omopDpReleaseDS(.dp_test_table(), list(
      statistic = "count", population_id = "cohort-a"
    )),
    "disabled by the data custodian"
  )
  expect_false(dir.exists(file.path(state, "secrets")))
  expect_false(dir.exists(file.path(state, "privacy")))
})

test_that("bootstrap creates independent private noise, ledger and SQLite state", {
  skip_on_os("windows")
  state <- .dp_local_state()
  status <- .dsomopDpPublicStatus(initialize = TRUE)

  expect_true(status$enabled)
  expect_true(status$ready)
  expect_true(status$sticky_noise)
  expect_true(status$durable_ledger)
  expect_false(status$formal_dp)
  expect_true(status$bounded_accounting)
  expect_false(status$bounded_composition)
  expect_false(status$sampler_certified)
  expect_identical(status$privacy_guarantee,
                   "sticky_noise_not_formally_certified_dp")

  secrets <- file.path(state, "secrets")
  privacy <- file.path(state, "privacy")
  noise_path <- file.path(secrets, "dp_noise_root")
  ledger_root_path <- file.path(secrets, "dp_ledger_root")
  ledger_path <- file.path(privacy, "ledger.sqlite")
  private_files <- c(
    noise_path,
    file.path(secrets, "dp_noise_root_receipt"),
    ledger_root_path,
    file.path(secrets, "dp_ledger_root_receipt"),
    ledger_path,
    paste0(ledger_path, ".receipt")
  )

  expect_true(all(file.exists(private_files)))
  expect_true(all(vapply(c(state, secrets, privacy), function(path) {
    identical(as.integer(file.info(path)$mode), 448L) # 0700
  }, logical(1L))))
  expect_true(all(vapply(private_files, function(path) {
    identical(as.integer(file.info(path)$mode), 384L) # 0600
  }, logical(1L))))
  expect_identical(file.info(noise_path)$size, 32)
  expect_identical(file.info(ledger_root_path)$size, 32)
  expect_match(status$privacy_instance_id, "^dpi_[0-9a-f]{40}$")
  expect_match(status$noise_domain_id, "^dpn_[0-9a-f]{40}$")
  expect_false(identical(
    .dsomopValidateSecretFile(noise_path),
    .dsomopValidateSecretFile(ledger_root_path)
  ))
})

test_that("DP releases require authenticated person-local provenance", {
  .dp_local_state()
  raw <- .testPseudonymize(data.frame(
    person_id = 1:6, value = seq_len(6), stringsAsFactors = FALSE
  ))
  spec <- list(statistic = "count", population_id = "cohort-a")

  expect_error(
    omopDpReleaseDS(raw, spec),
    "person-local DP provenance capsule"
  )
  sealed <- .dp_seal(raw, producer = "test/fixture")
  expect_silent(.dsomopDpVerifyPersonLocal(sealed))
  expect_silent(omopDpReleaseDS(sealed, spec))

  copied <- sealed
  copied$value[[1L]] <- 99L
  expect_error(
    omopDpReleaseDS(copied, spec),
    "provenance MAC does not match"
  )
})

test_that("provenance v2 binds semantic lineage to a stable dataset identity", {
  .dp_local_state()
  raw <- data.frame(
    person_id = 1:8,
    value = seq_len(8),
    stringsAsFactors = FALSE
  )
  scoped <- .testPseudonymize(raw)
  first <- .dp_seal(scoped, producer = "test/semantic-root")
  reordered <- .dp_seal(
    .testPseudonymize(raw[rev(seq_len(nrow(raw))), , drop = FALSE]),
    producer = "test/semantic-root"
  )
  rekeyed <- .dp_seal(
    .testPseudonymize(raw, key = .testPseudonymKey("rotated-test-key")),
    producer = "test/semantic-root"
  )
  other_resource <- .dp_seal(
    scoped, producer = "test/semantic-root",
    dataset_identity = .dp_dataset_identity("other-resource")
  )
  other_schema <- .dp_seal(
    scoped, producer = "test/semantic-root",
    dataset_identity = .dp_dataset_identity(cdm_schema = "other_cdm")
  )

  first_capsule <- .dp_capsule(first)
  reordered_capsule <- .dp_capsule(reordered)
  rekeyed_capsule <- .dp_capsule(rekeyed)
  expect_identical(first_capsule$protocol,
                   .DSOMOP_DP_PROVENANCE_PROTOCOL)
  expect_match(first_capsule$dataset_id, "^[0-9a-f]{64}$")
  expect_match(first_capsule$lineage_id, "^[0-9a-f]{64}$")
  expect_identical(first_capsule$dataset_id,
                   reordered_capsule$dataset_id)
  expect_identical(first_capsule$lineage_id,
                   reordered_capsule$lineage_id)
  expect_false(identical(first_capsule$mac, reordered_capsule$mac))
  # Pseudonym-key rotation is not a new semantic dataset release.
  expect_identical(first_capsule$dataset_id, rekeyed_capsule$dataset_id)
  expect_identical(first_capsule$lineage_id, rekeyed_capsule$lineage_id)
  expect_false(identical(first_capsule$mac, rekeyed_capsule$mac))
  expect_false(identical(first_capsule$dataset_id,
                         .dp_capsule(other_resource)$dataset_id))
  expect_false(identical(first_capsule$lineage_id,
                         .dp_lineage(other_resource)))
  expect_false(identical(first_capsule$dataset_id,
                         .dp_capsule(other_schema)$dataset_id))
})

test_that("chunked frame authentication covers every row and its order", {
  frame <- data.frame(
    person_id = seq_len(.DSOMOP_DP_FRAME_DIGEST_CHUNK_ROWS + 5L),
    value = rep.int("constant", .DSOMOP_DP_FRAME_DIGEST_CHUNK_ROWS + 5L),
    stringsAsFactors = FALSE
  )
  original <- .dsomopDpFrameDigest(frame)
  changed <- frame
  changed$value[[nrow(changed)]] <- "changed"
  reordered <- frame[rev(seq_len(nrow(frame))), , drop = FALSE]

  expect_false(identical(.dsomopDpFrameDigest(changed), original))
  expect_false(identical(.dsomopDpFrameDigest(reordered), original))
})

test_that("audited person-local manipulations re-seal their outputs", {
  .dp_local_state()
  raw <- data.frame(
    person_id = 1:12,
    group = rep(c("A", "B"), each = 6L),
    value = seq_len(12),
    stringsAsFactors = FALSE
  )
  source <- .dp_seal(
    .testPseudonymize(raw), producer = "test/fixture"
  )

  filtered <- omopFilterDS(source, "group", "==", "A")
  expect_silent(.dsomopDpVerifyPersonLocal(filtered))
  repeated <- omopFilterDS(source, "group", "==", "A")
  singleton_in <- omopFilterDS(source, "group", "in", "A")
  other_filter <- omopFilterDS(source, "group", "==", "B")
  expect_identical(.dp_lineage(filtered), .dp_lineage(repeated))
  expect_identical(.dp_lineage(filtered), .dp_lineage(singleton_in))
  expect_false(identical(.dp_lineage(filtered), .dp_lineage(other_filter)))
  no_op_filter <- omopFilterDS(filtered, "group", "in", "A")
  expect_identical(.dp_lineage(no_op_filter), .dp_lineage(filtered))
  selected <- omopSelectDS(filtered, c("group", "value"))
  expect_silent(.dsomopDpVerifyPersonLocal(selected))
  expect_identical(.dp_lineage(selected), .dp_lineage(filtered))

  left <- .dp_seal(
    .testPseudonymize(raw[1:6, , drop = FALSE]), producer = "test/left"
  )
  right <- .dp_seal(
    .testPseudonymize(raw[7:12, , drop = FALSE]), producer = "test/right"
  )
  bound <- omopBindRowsDS(left, right)
  expect_silent(.dsomopDpVerifyPersonLocal(bound))
  reverse_bound <- omopBindRowsDS(right, left)
  duplicate_bound <- omopBindRowsDS(left, left)
  expect_identical(.dp_lineage(bound), .dp_lineage(reverse_bound))
  expect_false(identical(.dp_lineage(bound),
                         .dp_lineage(duplicate_bound)))

  joined <- omopMergeDS(source, omopSelectDS(source, "value"))
  expect_silent(.dsomopDpVerifyPersonLocal(joined))
})

test_that("row-bind reduction is invariant to operand order", {
  .dp_local_state()
  left_raw <- data.frame(
    person_id = 1:6,
    measurement = c(1e16, rep.int(0, 5L)),
    stringsAsFactors = FALSE
  )
  right_raw <- data.frame(
    person_id = c(1L, 1L, 7:10),
    measurement = c(-1e16, 1, rep.int(0, 4L)),
    stringsAsFactors = FALSE
  )
  left <- .dp_seal(
    .testPseudonymize(left_raw), producer = "test/mean-left"
  )
  right <- .dp_seal(
    .testPseudonymize(right_raw), producer = "test/mean-right"
  )
  forward <- omopBindRowsDS(left, right)
  reverse <- omopBindRowsDS(right, left)
  spec <- list(
    statistic = "bounded_mean",
    variable = "measurement",
    lower = -1e16,
    upper = 1e16,
    reducer = "mean"
  )
  policy <- .dsomopDpPolicy()

  expect_identical(.dp_lineage(forward), .dp_lineage(reverse))
  expect_identical(
    .dsomopDpAnalysis(forward, spec, policy)$snapshot,
    .dsomopDpAnalysis(reverse, spec, policy)$snapshot
  )

  low <- 1
  high <- 1 + .Machine$double.eps
  expect_identical(as.character(low), as.character(high))
  tie_left <- .dp_seal(.testPseudonymize(data.frame(
    person_id = 1:6, value = rep.int(low, 6L), order_value = 0,
    stringsAsFactors = FALSE
  )), producer = "test/tie-left")
  tie_right <- .dp_seal(.testPseudonymize(data.frame(
    person_id = 1:6, value = rep.int(high, 6L), order_value = 0,
    stringsAsFactors = FALSE
  )), producer = "test/tie-right")
  tie_forward <- omopBindRowsDS(tie_left, tie_right)
  tie_reverse <- omopBindRowsDS(tie_right, tie_left)
  first_forward <- .dsomopDpReduceOne(
    tie_forward$person_id, tie_forward$value, "first",
    tie_forward$order_value
  )
  first_reverse <- .dsomopDpReduceOne(
    tie_reverse$person_id, tie_reverse$value, "first",
    tie_reverse$order_value
  )
  last_forward <- .dsomopDpReduceOne(
    tie_forward$person_id, tie_forward$value, "last",
    tie_forward$order_value
  )
  last_reverse <- .dsomopDpReduceOne(
    tie_reverse$person_id, tie_reverse$value, "last",
    tie_reverse$order_value
  )
  expect_identical(first_forward, first_reverse)
  expect_identical(last_forward, last_reverse)
  expect_true(all(first_forward$value == low))
  expect_true(all(last_forward$value == high))

  factor_left <- .dp_seal(.testPseudonymize(data.frame(
    person_id = 1:6,
    category = factor(rep(c("a", "b"), 3L), levels = c("a", "b")),
    stringsAsFactors = FALSE
  )), producer = "test/factor-bind-left")
  factor_right <- .dp_seal(.testPseudonymize(data.frame(
    person_id = 7:12,
    category = factor(rep(c("a", "b"), 3L), levels = c("b", "a")),
    stringsAsFactors = FALSE
  )), producer = "test/factor-bind-right")
  expect_error(
    omopBindRowsDS(factor_left, factor_right),
    "incompatible column schemas"
  )
})

test_that("merge provenance preserves operand roles and dataset boundaries", {
  .dp_local_state()
  x_raw <- data.frame(
    person_id = 1:8, left_value = seq_len(8), stringsAsFactors = FALSE
  )
  y_raw <- data.frame(
    person_id = 1:6, right_value = letters[1:6], stringsAsFactors = FALSE
  )
  x <- .dp_seal(.testPseudonymize(x_raw), producer = "test/merge-left")
  y <- .dp_seal(.testPseudonymize(y_raw), producer = "test/merge-right")

  inner <- omopMergeDS(x, y, by = "person_id", type = "inner")
  left <- omopMergeDS(x, y, by = "person_id", type = "left")
  swapped <- omopMergeDS(y, x, by = "person_id", type = "inner")
  expect_silent(.dsomopDpVerifyPersonLocal(inner))
  expect_silent(.dsomopDpVerifyPersonLocal(left))
  expect_silent(.dsomopDpVerifyPersonLocal(swapped))
  expect_false(identical(.dp_lineage(inner), .dp_lineage(left)))
  expect_false(identical(.dp_lineage(inner), .dp_lineage(swapped)))

  unsealed_y <- .testPseudonymize(y_raw)
  expect_null(attr(
    omopMergeDS(x, unsealed_y, by = "person_id", type = "inner"),
    "dsomop_dp_provenance", exact = TRUE
  ))
  other_dataset_y <- .dp_seal(
    .testPseudonymize(y_raw), producer = "test/merge-right",
    dataset_identity = .dp_dataset_identity("other-resource")
  )
  expect_null(attr(
    omopMergeDS(x, other_dataset_y, by = "person_id", type = "inner"),
    "dsomop_dp_provenance", exact = TRUE
  ))

  rich_x <- .dp_seal(.testPseudonymize(data.frame(
    person_id = 1:6, a = 1:6, b = 11:16, c = 21:26,
    stringsAsFactors = FALSE
  )), producer = "test/rich-left")
  rich_y <- .dp_seal(.testPseudonymize(data.frame(
    person_id = 1:6, a = 31:36, b = 41:46, c = 51:56,
    stringsAsFactors = FALSE
  )), producer = "test/rich-right")
  first_partition <- omopMergeDS(
    omopSelectDS(rich_x, "a"),
    omopSelectDS(rich_y, c("b", "c")),
    by = "person_id", type = "inner"
  )
  second_partition <- omopMergeDS(
    omopSelectDS(rich_x, c("a", "b")),
    omopSelectDS(rich_y, "c"),
    by = "person_id", type = "inner"
  )
  expect_identical(names(first_partition), names(second_partition))
  expect_false(identical(.dp_lineage(first_partition),
                         .dp_lineage(second_partition)))

  concept_x_raw <- data.frame(
    person_id = 1:6,
    measurement_concept_id = 101:106,
    stringsAsFactors = FALSE
  )
  concept_y_raw <- data.frame(
    person_id = 1:6,
    measurement_concept_id = 201:206,
    stringsAsFactors = FALSE
  )
  attr(concept_x_raw, "omop_concept_cols") <- "measurement_concept_id"
  attr(concept_y_raw, "omop_concept_cols") <- "measurement_concept_id"
  concept_x <- .dp_seal(
    .testPseudonymize(concept_x_raw), producer = "test/concept-left"
  )
  concept_y <- .dp_seal(
    .testPseudonymize(concept_y_raw), producer = "test/concept-right"
  )
  concept_join <- omopMergeDS(concept_x, concept_y, by = "person_id")
  expect_setequal(
    attr(concept_join, "omop_concept_cols"),
    c("measurement_concept_id.x", "measurement_concept_id.y")
  )
  expect_silent(.dsomopDpVerifyPersonLocal(concept_join))

  protected_x <- concept_x
  protected_y <- concept_y
  names(protected_x)[names(protected_x) == "measurement_concept_id"] <-
    "protected_value"
  names(protected_y)[names(protected_y) == "measurement_concept_id"] <-
    "protected_value"
  attr(protected_x, "omop_concept_cols") <- NULL
  attr(protected_y, "omop_concept_cols") <- NULL
  attr(protected_x, "dsomop_protected") <- union(
    attr(protected_x, "dsomop_protected"), "protected_value"
  )
  attr(protected_y, "dsomop_protected") <- union(
    attr(protected_y, "dsomop_protected"), "protected_value"
  )
  # Re-seal after the focused fixture mutation so the rejection reaches the
  # overlap check rather than failing capsule authentication first.
  protected_x <- .dp_seal(protected_x, producer = "test/protected-left")
  protected_y <- .dp_seal(protected_y, producer = "test/protected-right")
  expect_error(
    omopMergeDS(protected_x, protected_y, by = "person_id"),
    "protected non-key columns overlap"
  )

  episode_raw_x <- data.frame(
    person_id = 1:6, cohort_row_id = 101:106,
    left_episode_value = 1:6, stringsAsFactors = FALSE
  )
  episode_raw_y <- data.frame(
    person_id = 1:6, cohort_row_id = 101:106,
    right_episode_value = 11:16, stringsAsFactors = FALSE
  )
  episode_x <- .dp_seal(
    .testPseudonymize(episode_raw_x), producer = "test/episode-left",
    episode_domain = "episode-domain-a"
  )
  episode_y <- .dp_seal(
    .testPseudonymize(episode_raw_y), producer = "test/episode-right",
    episode_domain = "episode-domain-a"
  )
  episode_join <- omopMergeDS(
    episode_x, episode_y,
    by = c("person_id", "cohort_row_id"), type = "inner"
  )
  expect_identical(.dp_capsule(episode_join)$episode_domain,
                   "episode-domain-a")
  mismatched_episode_y <- .dp_seal(
    .testPseudonymize(episode_raw_y), producer = "test/episode-right",
    episode_domain = "episode-domain-b"
  )
  withr::with_options(list(dsomop.max_memory_rows = 1L), {
    expect_error(
      omopMergeDS(
        episode_x, mismatched_episode_y,
        by = c("person_id", "cohort_row_id"), type = "inner"
      ),
      "authenticated episode domain"
    )
  })
  missing_episode_y <- .dp_seal(
    .testPseudonymize(episode_raw_y), producer = "test/episode-right"
  )
  expect_error(
    omopMergeDS(
      episode_x, missing_episode_y,
      by = c("person_id", "cohort_row_id"), type = "inner"
    ),
    "authenticated episode domain"
  )
  expect_error(
    omopMergeDS(
      episode_x, .testPseudonymize(episode_raw_y),
      by = c("person_id", "cohort_row_id"), type = "inner"
    ),
    "authenticated episode domain"
  )
  other_dataset_episode_y <- .dp_seal(
    .testPseudonymize(episode_raw_y), producer = "test/episode-right",
    episode_domain = "episode-domain-a",
    dataset_identity = .dp_dataset_identity("other-resource")
  )
  expect_error(
    omopMergeDS(
      episode_x, other_dataset_episode_y,
      by = c("person_id", "cohort_row_id"), type = "inner"
    ),
    "authenticated episode domain"
  )
  numeric_episode_y_raw <- episode_raw_y
  numeric_episode_y_raw$cohort_row_id <-
    as.numeric(numeric_episode_y_raw$cohort_row_id)
  numeric_episode_y <- .dp_seal(
    .testPseudonymize(numeric_episode_y_raw),
    producer = "test/episode-right", episode_domain = "episode-domain-a"
  )
  expect_error(
    omopMergeDS(
      episode_x, numeric_episode_y,
      by = c("person_id", "cohort_row_id"), type = "inner"
    ),
    "join-key schemas are incompatible"
  )

  legacy_x <- episode_x
  legacy_y <- episode_y
  legacy_x$row_id <- legacy_x$cohort_row_id
  legacy_y$row_id <- legacy_y$cohort_row_id
  expect_error(
    omopMergeDS(legacy_x, legacy_y, by = c("person_id", "row_id")),
    "only cohort_row_id"
  )
})

test_that("lossless factor harmonization has semantic, not level-order lineage", {
  .dp_local_state()
  raw <- data.frame(
    person_id = 1:8,
    cohort_row_id = 101:108,
    gender_concept_id = rep(c(8507L, 8532L), 4L),
    race_concept_id = rep(c(1L, 2L), 4L),
    stringsAsFactors = FALSE
  )
  attr(raw, "omop_concept_cols") <-
    c("gender_concept_id", "race_concept_id")
  source <- .dp_seal(
    .testPseudonymize(raw), producer = "test/factor-source",
    episode_domain = "factor-episode-domain"
  )
  first <- omopAsFactorColumnsDS(source, list(
    gender_concept_id = c("8507", "8532")
  ))
  reordered <- omopAsFactorColumnsDS(source, list(
    gender_concept_id = c("9999", "8532", "8507")
  ))
  two_columns <- omopAsFactorColumnsDS(source, list(
    gender_concept_id = c("8507", "8532"),
    race_concept_id = c("1", "2")
  ))

  expect_silent(.dsomopDpVerifyPersonLocal(first))
  expect_silent(.dsomopDpVerifyPersonLocal(reordered))
  expect_identical(.dp_capsule(first)$episode_domain,
                   "factor-episode-domain")
  expect_false(identical(.dp_lineage(first), .dp_lineage(reordered)))
  expect_false(identical(.dp_lineage(first), .dp_lineage(two_columns)))
  no_op <- omopAsFactorColumnsDS(first, list(
    gender_concept_id = levels(first$gender_concept_id)
  ))
  expect_identical(no_op, first)
  expect_identical(.dp_capsule(no_op), .dp_capsule(first))

  excluded <- omopAsFactorColumnsDS(source, list(
    gender_concept_id = "8507"
  ))
  expect_silent(.dsomopDpVerifyPersonLocal(excluded))
  expect_identical(excluded, source)
  expect_identical(.dp_capsule(excluded), .dp_capsule(source))
})

test_that("QueryLibrary assign provenance rejects every scoped execution", {
  .dp_local_state()
  frame <- .testPseudonymize(data.frame(
    person_id = 1:6, condition_concept_id = 101:106,
    stringsAsFactors = FALSE
  ))
  query <- .ql_load_queries()[["condition_occurrence.load"]]
  entry <- list(
    meta = list(adapter = "query", query_id = "condition_occurrence.load"),
    compute = list(sql = query$sql)
  )

  effective <- list(date_handling = "relative", limit = 100L)
  public_config <- list(
    target_dialect = "postgresql",
    cdm_schema = "main"
  )
  unscoped <- .dsomopDpSealAnalysisAssign(
    frame, entry, scope_present = FALSE,
    effective_params = effective,
    public_config = public_config,
    dataset_identity = .dp_dataset_identity()
  )
  expect_silent(.dsomopDpVerifyPersonLocal(unscoped))
  equivalent <- .dsomopDpSealAnalysisAssign(
    frame, entry, scope_present = FALSE,
    effective_params = effective[c("limit", "date_handling")],
    public_config = public_config,
    dataset_identity = .dp_dataset_identity()
  )
  changed_config <- .dsomopDpSealAnalysisAssign(
    frame, entry, scope_present = FALSE,
    effective_params = effective,
    public_config = utils::modifyList(public_config, list(cdm_schema = "alt")),
    dataset_identity = .dp_dataset_identity()
  )
  expect_identical(.dp_lineage(unscoped), .dp_lineage(equivalent))
  expect_false(identical(.dp_lineage(unscoped),
                         .dp_lineage(changed_config)))
  scoped <- .dsomopDpSealAnalysisAssign(
    frame, entry, scope_present = TRUE,
    dataset_identity = .dp_dataset_identity()
  )
  expect_null(attr(scoped, "dsomop_dp_provenance", exact = TRUE))
  drifted <- entry
  drifted$compute$sql <- paste0(entry$compute$sql, " ")
  expect_null(attr(
    .dsomopDpSealAnalysisAssign(
      frame, drifted, scope_present = FALSE,
      dataset_identity = .dp_dataset_identity()
    ),
    "dsomop_dp_provenance", exact = TRUE
  ))
})

test_that("plan provenance admits fixed longitudinal shapes only", {
  .dp_local_state()
  frame <- .testPseudonymize(data.frame(
    person_id = 1:6, concept_id = 101:106, stringsAsFactors = FALSE
  ))
  long <- list(outputs = list(events = list(
    type = "event_level", table = "measurement",
    representation = list(format = "long")
  )))
  expect_silent(.dsomopDpVerifyPersonLocal(
    .dp_seal_plan(frame, long, "events")
  ))

  auto_wide <- long
  auto_wide$outputs$events$representation$format <- "wide"
  expect_null(attr(
    .dp_seal_plan(frame, auto_wide, "events"),
    "dsomop_dp_provenance", exact = TRUE
  ))

  fixed_wide <- auto_wide
  fixed_wide$outputs$events$concept_set <- list(concepts = c(101L, 102L))
  expect_silent(.dsomopDpVerifyPersonLocal(
    .dp_seal_plan(frame, fixed_wide, "events")
  ))

  unsafe_age <- list(outputs = list(baseline = list(
    type = "baseline", derived = "age_at_index"
  )))
  expect_null(attr(
    .dp_seal_plan(frame, unsafe_age, "baseline"),
    "dsomop_dp_provenance", exact = TRUE
  ))

  forged_scope <- long
  forged_scope$scope <- list(
    tables_frames = "dsomop_cohort_fromtbl_forged"
  )
  expect_null(attr(
    .dp_seal_plan(frame, forged_scope, "events"),
    "dsomop_dp_provenance", exact = TRUE
  ))

  persistent <- long
  persistent$cohort <- list(
    type = "cohort_table", cohort_definition_id = 7L
  )
  expect_null(attr(
    .dp_seal_plan(frame, persistent, "events"),
    "dsomop_dp_provenance", exact = TRUE
  ))

  nested_persistent <- long
  nested_persistent$cohort <- list(filter_tree = list(and = list(
    list(type = "sex", params = list(value = "F")),
    list(type = "cohort", params = list(cohort_definition_id = 7L))
  )))
  expect_null(attr(
    .dp_seal_plan(frame, nested_persistent, "events"),
    "dsomop_dp_provenance", exact = TRUE
  ))

  populations <- long
  populations$populations <- list(
    base = list(kind = "criteria"),
    exposed = list(
      kind = "criteria",
      filter_tree = list(type = "has_concept", params = list(
        table = "condition_occurrence", concept_ids = 201820L
      ))
    ),
    stored = list(kind = "criteria", cohort_definition_id = 7L),
    combined = list(
      kind = "setop",
      setop = list(op = "union", members = c("exposed", "base"))
    )
  )
  populations$outputs$events$population_id <- "combined"
  expect_silent(.dsomopDpVerifyPersonLocal(
    .dp_seal_plan(frame, populations, "events")
  ))
  populations$outputs$events$population_id <- "stored"
  expect_null(attr(
    .dp_seal_plan(frame, populations, "events"),
    "dsomop_dp_provenance", exact = TRUE
  ))
})

test_that("plan lineage canonicalizes commutative filters but preserves scope order", {
  .dp_local_state()
  frame <- .testPseudonymize(data.frame(
    person_id = 1:8,
    category = rep(c("a", "b"), 4L),
    stringsAsFactors = FALSE
  ))
  parent_a <- .dp_seal(frame, producer = "test/scope-a")
  parent_b <- .dp_seal(frame, producer = "test/scope-b")
  base <- list(outputs = list(out = list(
    type = "event_level",
    table = "measurement",
    representation = list(format = "long")
  )))
  lineage <- function(plan) .dp_lineage(.dp_seal_plan(frame, plan, "out"))

  aliased <- base
  aliased$scope <- list(
    tables_frames = list(alpha = parent_a, ignored = NULL, beta = parent_b),
    combine = "union"
  )
  renamed <- base
  renamed$scope <- list(
    tables_frames = list(first = parent_a, second = parent_b),
    combine = "union"
  )
  reversed <- renamed
  reversed$scope$tables_frames <-
    list(first = parent_b, second = parent_a)
  expect_identical(lineage(aliased), lineage(renamed))
  expect_false(identical(lineage(aliased), lineage(reversed)))

  same_frame_other_parent <- .dp_seal(
    frame, producer = "test/scope-other-parent"
  )
  one_parent <- base
  one_parent$scope <- list(tables_frames = list(parent_a))
  other_parent <- base
  other_parent$scope <- list(tables_frames = list(same_frame_other_parent))
  expect_false(identical(lineage(one_parent), lineage(other_parent)))

  concept_filter <- list(
    type = "has_concept",
    params = list(table = "Measurement", concept_ids = c(20L, 10L))
  )
  sex_filter <- list(type = "sex", params = list(value = "F"))
  canonical <- base
  canonical$cohort <- list(filter_tree = list(and = list(
    concept_filter, sex_filter, concept_filter
  )))
  canonical$outputs$out$filters <- list(custom = list(
    var = "category", op = "in", value = c("b", "a", "a")
  ))
  equivalent <- canonical
  equivalent$cohort$filter_tree$and <- list(
    sex_filter,
    list(type = "has_concept", params = list(
      concept_ids = c(10L, 20L), table = "measurement"
    ))
  )
  equivalent$outputs$out$filters$custom$value <- c("a", "b")
  changed <- equivalent
  changed$outputs$out$filters$custom$value <- c("a", "c")
  expect_identical(lineage(canonical), lineage(equivalent))
  expect_false(identical(lineage(canonical), lineage(changed)))
})

test_that("longitudinal plan lineage covers execution order and custom filters", {
  .dp_local_state()
  frame <- .testPseudonymize(data.frame(
    person_id = 1:8, value = seq_len(8), stringsAsFactors = FALSE
  ))
  lineage <- function(plan) .dp_lineage(.dp_seal_plan(frame, plan, "out"))
  custom_a <- list(var = "status", op = "in", value = c("a", "b"))
  custom_b <- list(var = "status", op = "in", value = c("a", "c"))

  person <- list(outputs = list(out = list(
    type = "person_level",
    tables = list(
      person = list(columns = c("year_of_birth")),
      death = list(columns = c("death_date"))
    ),
    filters = list(custom = custom_a)
  )))
  person_reordered <- person
  person_reordered$outputs$out$tables <-
    person$outputs$out$tables[c("death", "person")]
  person_changed <- person
  person_changed$outputs$out$filters$custom <- custom_b
  expect_false(identical(lineage(person), lineage(person_reordered)))
  expect_false(identical(lineage(person), lineage(person_changed)))

  feature_plan <- list(outputs = list(out = list(
    type = "person_level",
    tables = list(measurement = list(features = list(
      first = list(kind = "count", concept_set = c(10L, 20L)),
      second = list(kind = "binary", filters = custom_a)
    )))
  )))
  feature_reordered <- feature_plan
  feature_reordered$outputs$out$tables$measurement$features <-
    feature_plan$outputs$out$tables$measurement$features[c("second", "first")]
  expect_false(identical(lineage(feature_plan),
                         lineage(feature_reordered)))

  survival <- list(outputs = list(out = list(
    type = "survival",
    outcome = list(
      table = "condition_occurrence",
      concept_set = list(concepts = c(201826L, 201820L))
    ),
    tar = list(start_offset = 0L, end_offset = 30L),
    filters = list(custom = custom_a)
  )))
  survival_changed <- survival
  survival_changed$outputs$out$filters$custom <- custom_b
  expect_false(identical(lineage(survival), lineage(survival_changed)))

  intervals <- list(outputs = list(out = list(
    type = "intervals_long",
    tables = c("condition_occurrence", "drug_exposure"),
    concept_filter = list(
      condition_occurrence = c(10L, 20L),
      drug_exposure = c(30L, 40L)
    ),
    filters = list(custom = custom_a)
  )))
  intervals_changed <- intervals
  intervals_changed$outputs$out$filters$custom <- custom_b
  expect_false(identical(lineage(intervals), lineage(intervals_changed)))
})

test_that("population lineage binds the global anchor and validates set operations", {
  .dp_local_state()
  frame <- .testPseudonymize(data.frame(
    person_id = 1:8, value = seq_len(8), stringsAsFactors = FALSE
  ))
  lineage <- function(plan) .dp_lineage(.dp_seal_plan(frame, plan, "out"))
  sex_f <- list(type = "sex", params = list(value = "F"))
  sex_m <- list(type = "sex", params = list(value = "M"))
  has_concept <- list(type = "has_concept", params = list(
    table = "condition_occurrence", concept_ids = 201820L
  ))
  output <- list(
    type = "event_level", table = "measurement",
    representation = list(format = "long")
  )

  non_base <- list(
    cohort = list(filter_tree = sex_f),
    populations = list(exposed = list(
      kind = "criteria", filter_tree = has_concept
    )),
    outputs = list(out = utils::modifyList(
      output, list(population_id = "exposed")
    ))
  )
  other_anchor <- non_base
  other_anchor$cohort$filter_tree <- sex_m
  expect_false(identical(lineage(non_base), lineage(other_anchor)))

  episode_frame <- .testPseudonymize(data.frame(
    person_id = 1:8, cohort_row_id = 101:108,
    value = seq_len(8), stringsAsFactors = FALSE
  ))
  episode_plan <- list(
    cohort = list(filter_tree = sex_f),
    outputs = list(
      first = output,
      second = utils::modifyList(output, list(table = "observation"))
    )
  )
  first_episode <- .dp_seal_plan(episode_frame, episode_plan, "first")
  second_episode <- .dp_seal_plan(episode_frame, episode_plan, "second")
  expect_false(is.null(.dp_capsule(first_episode)$episode_domain))
  expect_identical(.dp_capsule(first_episode)$episode_domain,
                   .dp_capsule(second_episode)$episode_domain)
  expect_false(identical(.dp_lineage(first_episode),
                         .dp_lineage(second_episode)))

  setop <- list(
    populations = list(
      female = list(kind = "criteria", filter_tree = sex_f),
      exposed = list(kind = "criteria", filter_tree = has_concept),
      combined = list(kind = "setop", setop = list(
        op = "union", members = c("female", "exposed")
      ))
    ),
    outputs = list(out = utils::modifyList(
      output, list(population_id = "combined")
    ))
  )
  union_reversed <- setop
  union_reversed$populations$combined$setop$members <-
    c("exposed", "female")
  expect_identical(lineage(setop), lineage(union_reversed))

  intersect_forward <- setop
  intersect_forward$populations$combined$setop$op <- "intersect"
  intersect_reverse <- intersect_forward
  intersect_reverse$populations$combined$setop$members <-
    c("exposed", "female")
  expect_false(identical(lineage(intersect_forward),
                         lineage(intersect_reverse)))

  difference <- setop
  difference$populations$combined$setop$op <- "difference"
  setdiff <- difference
  setdiff$populations$combined$setop$op <- "setdiff"
  expect_identical(lineage(difference), lineage(setdiff))

  invalid_single <- setop
  invalid_single$populations$combined$setop$members <- "female"
  invalid_duplicate <- setop
  invalid_duplicate$populations$combined$setop$members <-
    c("female", "female")
  semantic_duplicate <- setop
  semantic_duplicate$populations$alias <-
    list(kind = "criteria", filter_tree = sex_f)
  semantic_duplicate$populations <- semantic_duplicate$populations[
    c("female", "exposed", "alias", "combined")
  ]
  semantic_duplicate$populations$combined$setop$members <-
    c("female", "alias")
  for (invalid in list(invalid_single, invalid_duplicate,
                       semantic_duplicate)) {
    expect_null(attr(
      .dp_seal_plan(frame, invalid, "out"),
      "dsomop_dp_provenance", exact = TRUE
    ))
  }
})

test_that("snapshot drift fails closed without growing the ledger", {
  state <- .dp_local_state()
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")

  first <- omopDpReleaseDS(table, spec)
  before_rows <- .dp_ledger_rows(state)
  before_meta <- .dp_ledger_meta(state)

  changed <- table
  people <- unique(changed$person_id)
  changed$person_id[changed$person_id == people[[3L]]] <- people[[2L]]
  expect_error(
    omopDpReleaseDS(changed, spec),
    "provenance MAC does not match"
  )
  changed <- .dp_seal(changed, producer = "test/fixture")
  expect_identical(
    .dsomopDpVerifyPersonLocal(changed)$lineage_id,
    .dsomopDpVerifyPersonLocal(table)$lineage_id
  )
  expect_error(
    omopDpReleaseDS(changed, spec),
    "does not match its protected snapshot identity"
  )
  after_rows <- .dp_ledger_rows(state)
  after_meta <- .dp_ledger_meta(state)

  expect_identical(first$statistic, "count")
  expect_true(is.numeric(first$noisy_count))
  expect_true(isTRUE(first$sticky))
  expect_identical(nrow(before_rows), 1L)
  expect_identical(after_rows, before_rows)
  expect_identical(after_meta, before_meta)
})

test_that("advancing public snapshot adds one release without resetting state", {
  state <- .dp_local_state()
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")

  omopDpReleaseDS(table, spec)
  before_rows <- .dp_ledger_rows(state)
  before_meta <- .dp_ledger_meta(state)
  withr::local_options(list(
    dsomop.dp.snapshot_id = "etl-2026-08-02"
  ))
  .dp_restart_runtime()
  table <- .dp_seal(table, producer = "test/fixture")
  omopDpReleaseDS(table, spec)
  after_rows <- .dp_ledger_rows(state)
  after_meta <- .dp_ledger_meta(state)

  expect_identical(nrow(before_rows), 1L)
  expect_identical(nrow(after_rows), 2L)
  expect_identical(after_rows[1L, , drop = FALSE], before_rows)
  expect_identical(after_meta[["ledger_id"]], before_meta[["ledger_id"]])
  expect_identical(after_meta[["next_index"]], "2")
  expect_gt(as.numeric(after_meta[["spent_epsilon"]]),
            as.numeric(before_meta[["spent_epsilon"]]))
  expect_equal(as.numeric(after_meta[["spent_epsilon"]]),
               sum(after_rows$epsilon), tolerance = 1e-12)
})

test_that("advancing privacy epoch cannot replay an earlier release", {
  state <- .dp_local_state()
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")

  first <- omopDpReleaseDS(table, spec)
  before <- .dp_ledger_rows(state)
  # Maintenance may legitimately skip unused epoch numbers; advancing must not
  # block merely because rotations were not contiguous.
  withr::local_options(list(dsomop.dp.privacy_epoch = 7L))
  .dp_restart_runtime()
  expect_error(
    omopDpReleaseDS(table, spec),
    "privacy epoch"
  )

  current <- .dp_seal(table, producer = "test/fixture")
  second <- omopDpReleaseDS(current, spec)
  replay <- omopDpReleaseDS(current, spec)
  after <- .dp_ledger_rows(state)

  expect_identical(replay, second)
  expect_identical(first$statistic, second$statistic)
  expect_identical(nrow(before), 1L)
  expect_identical(nrow(after), 2L)
  expect_identical(as.numeric(after$privacy_epoch), c(1, 7))
  expect_identical(length(unique(after$release_id)), 2L)
})

test_that("distinct authenticated lineages create distinct releases", {
  state <- .dp_local_state()
  raw <- .testPseudonymize(data.frame(
    person_id = 1:8, value = seq_len(8), stringsAsFactors = FALSE
  ))
  first_input <- .dp_seal(raw, producer = "test/lineage-one")
  second_input <- .dp_seal(raw, producer = "test/lineage-two")
  spec <- list(statistic = "count")

  first <- omopDpReleaseDS(first_input, spec)
  first_rows <- .dp_ledger_rows(state)
  second <- omopDpReleaseDS(second_input, spec)
  second_rows <- .dp_ledger_rows(state)
  replay <- omopDpReleaseDS(first_input, spec)

  expect_false(identical(.dp_lineage(first_input),
                         .dp_lineage(second_input)))
  expect_identical(nrow(first_rows), 1L)
  expect_identical(nrow(second_rows), 2L)
  expect_identical(length(unique(second_rows$release_id)), 2L)
  expect_identical(replay, first)
  expect_identical(nrow(.dp_ledger_rows(state)), 2L)
  expect_identical(second$statistic, first$statistic)
})

test_that("population aliases replay one bounded snapshot without a charge", {
  state <- .dp_local_state()
  table <- .dp_test_table()

  first <- omopDpReleaseDS(table, list(statistic = "count"))
  first_rows <- .dp_ledger_rows(state)
  first_meta <- .dp_ledger_meta(state)
  replay <- omopDpReleaseDS(table, list(
    statistic = "count", population_id = "cohort-b"
  ))
  rows <- .dp_ledger_rows(state)
  second_meta <- .dp_ledger_meta(state)

  expect_identical(replay, first)
  expect_identical(rows, first_rows)
  expect_identical(second_meta, first_meta)
  expect_identical(nrow(rows), 1L)
})

test_that("public snapshot metadata does not leak private release identities", {
  state <- .dp_local_state()
  status <- .dsomopDpPublicStatus(initialize = TRUE)
  release <- omopDpReleaseDS(.dp_test_table(), list(
    statistic = "count", population_id = "cohort-a"
  ))
  row <- .dp_ledger_rows(state)
  encoded <- .dsomopDpCanonicalJson(release)

  expect_identical(status$snapshot_id, "etl-2026-08-01")
  expect_false(any(c(
    "seed", "noise_root", "raw_noise", "snapshot_id", "snapshot_hash",
    "data_fingerprint", "protected_fingerprint", "semantic_query_id",
    "query_id", "release_id", "noise_key_id"
  ) %in% names(release)))
  private_values <- unlist(row[c(
    "release_id", "semantic_query_id", "snapshot_id",
    "protected_fingerprint", "noise_key_id"
  )], use.names = FALSE)
  expect_false(any(vapply(
    private_values, grepl, logical(1L), x = encoded, fixed = TRUE
  )))
})

test_that("noise-root loss rotates safely while ledger-root loss fails closed", {
  skip_on_os("windows")
  state <- .dp_local_state()
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")
  first <- omopDpReleaseDS(table, spec)

  noise_path <- file.path(state, "secrets", "dp_noise_root")
  ledger_root_path <- file.path(state, "secrets", "dp_ledger_root")
  old_noise <- .dsomopValidateSecretFile(noise_path)
  withr::local_options(list(dsomop.dp.noise_require_existing = TRUE))
  expect_identical(unlink(noise_path, force = TRUE), 0L)

  replay <- omopDpReleaseDS(table, spec)
  new_noise <- .dsomopValidateSecretFile(noise_path)
  expect_identical(replay, first)
  expect_false(identical(new_noise, old_noise))
  expect_identical(nrow(.dp_ledger_rows(state)), 1L)

  expect_identical(unlink(ledger_root_path, force = TRUE), 0L)
  expect_error(
    omopDpReleaseDS(table, spec),
    "ledger authentication root is missing"
  )
})

test_that("noise recovery opens existing SQLite state with full durability", {
  skip_on_os("windows")
  state <- .dp_local_state()
  expect_true(omopDpStatusDS()$ready)
  noise_path <- file.path(state, "secrets", "dp_noise_root")
  expect_identical(unlink(noise_path, force = TRUE), 0L)
  .dp_restart_runtime()

  calls <- list()
  connect <- DBI::dbConnect
  status <- testthat::with_mocked_bindings(
    omopDpStatusDS(),
    dbConnect = function(...) {
      arguments <- list(...)
      calls[[length(calls) + 1L]] <<- arguments
      do.call(connect, arguments)
    },
    .package = "DBI"
  )

  expect_true(status$ready)
  expect_gte(length(calls), 2L)
  expect_true(all(vapply(
    calls, function(arguments) identical(arguments$synchronous, "full"),
    logical(1L)
  )))
  flags <- vapply(calls, `[[`, numeric(1L), "flags")
  expect_true(RSQLite::SQLITE_RW %in% flags)
  expect_true(RSQLite::SQLITE_RWC %in% flags)
})

test_that("required initial noise provisioning fails without state mutation", {
  skip_on_os("windows")
  state <- .dp_local_state()
  withr::local_options(list(dsomop.dp.noise_require_existing = TRUE))

  expect_error(omopDpStatusDS(), "requires an existing root")
  expect_false(dir.exists(file.path(state, "secrets")))
  expect_false(dir.exists(file.path(state, "privacy")))
})

test_that("corrupt ledger cannot authorize replacement noise material", {
  skip_on_os("windows")
  state <- .dp_local_state()
  table <- .dp_test_table()
  omopDpReleaseDS(table, list(
    statistic = "count", population_id = "cohort-a"
  ))
  ledger <- file.path(state, "privacy", "ledger.sqlite")
  connection <- DBI::dbConnect(RSQLite::SQLite(), ledger)
  DBI::dbExecute(
    connection,
    "UPDATE dp_releases SET payload = '{\"tampered\":true}' WHERE release_index = 0"
  )
  DBI::dbDisconnect(connection)
  noise_root <- file.path(state, "secrets", "dp_noise_root")
  expect_identical(unlink(noise_root, force = TRUE), 0L)
  .dp_restart_runtime()

  expect_error(omopDpStatusDS(), "does not authenticate noise-root recovery")
  expect_false(file.exists(noise_root))
})

test_that("a ledger-root receipt never authorizes replacement root creation", {
  skip_on_os("windows")
  state <- .dp_local_state()
  expect_true(omopDpStatusDS()$ready)
  ledger <- file.path(state, "privacy", "ledger.sqlite")
  root <- file.path(state, "secrets", "dp_ledger_root")
  root_receipt <- file.path(state, "secrets", "dp_ledger_root_receipt")
  expect_true(file.exists(root_receipt))
  expect_identical(
    unlink(c(ledger, paste0(ledger, ".receipt")), force = TRUE), 0L
  )
  expect_identical(unlink(root, force = TRUE), 0L)
  .dp_restart_runtime()

  expect_error(omopDpStatusDS(), "continuity receipt exists")
  expect_false(file.exists(root))
})

test_that("auto noise provider remains file-backed after injected fallback", {
  skip_on_os("windows")
  state <- .dp_local_state()
  injected <- as.raw(seq.int(0L, 31L))
  withr::local_options(list(
    dsomop.dp.noise_provider = "auto",
    dsomop.dp.noise_root = injected
  ))

  first <- omopDpStatusDS()
  expect_identical(first$noise_provider, "injected")
  noise_path <- file.path(state, "secrets", "dp_noise_root")
  expect_false(file.exists(noise_path))

  options(dsomop.dp.noise_root = NULL)
  .dp_restart_runtime()
  fallback <- omopDpStatusDS()
  expect_identical(fallback$noise_provider, "file")
  expect_true(file.exists(noise_path))
  expect_false(identical(fallback$noise_key_id, first$noise_key_id))

  options(dsomop.dp.noise_root = injected)
  .dp_restart_runtime()
  restored <- omopDpStatusDS()
  expect_identical(restored$noise_provider, "file")
  expect_identical(restored$noise_key_id, fallback$noise_key_id)
})

test_that("file rotation adopts the persisted root under the ledger lock", {
  skip_on_os("windows")
  state <- .dp_local_state()
  expect_true(omopDpStatusDS()$ready)
  stale <- .dsomopDpPolicy()
  new_root <- as.raw(seq.int(31L, 0L))
  root_path <- file.path(state, "secrets", "dp_noise_root")
  receipt_path <- file.path(state, "secrets", "dp_noise_root_receipt")
  writeBin(new_root, root_path)
  writeBin(.dsomopDpHmacRaw(
    new_root, "dsOMOP/dp/noise-root-continuity-receipt/v1"
  ), receipt_path)
  Sys.chmod(c(root_path, receipt_path), mode = "0600")

  handle <- .dsomopDpOpenLedger(stale)
  on.exit(.dsomopDpCloseLedger(handle), add = TRUE)
  expect_identical(handle$policy$noise_root$key_id,
                   .dsomopDpRootId(new_root))
  expect_false(identical(handle$policy$noise_root$key_id,
                         stale$noise_root$key_id))
})

test_that("injected noise-root rotation never requires a manual epoch", {
  skip_on_os("windows")
  state <- .dp_local_state()
  first_root <- as.raw(seq.int(0L, 31L))
  second_root <- as.raw(seq.int(32L, 63L))
  withr::local_options(list(
    dsomop.dp.noise_provider = "injected",
    dsomop.dp.noise_root = first_root
  ))
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")
  first_release <- omopDpReleaseDS(table, spec)
  first <- omopDpStatusDS()

  options(dsomop.dp.noise_root = second_root)
  .dp_restart_runtime()
  rotated <- omopDpStatusDS()
  replay <- omopDpReleaseDS(table, spec)
  meta <- .dp_ledger_meta(state)
  expect_true(rotated$ready)
  expect_identical(rotated$privacy_epoch, 1)
  expect_false(identical(rotated$noise_key_id, first$noise_key_id))
  expect_identical(replay, first_release)
  expect_identical(nrow(.dp_ledger_rows(state)), 1L)
  expect_identical(meta[["next_index"]], "1")
})

test_that("longitudinal DP primitives bound contributions and return fixed shapes", {
  .dp_local_state()
  table <- .dp_test_table()
  policy <- .dsomopDpPolicy()

  categorical_spec <- list(
    statistic = "categorical_histogram",
    variable = "category",
    levels = c("c", "a", "b"),
    reducer = "presence",
    max_contributions = 2L,
    population_id = "cohort-a"
  )
  categorical_analysis <- .dsomopDpAnalysis(
    table, categorical_spec, policy
  )
  per_person <- table(categorical_analysis$snapshot$person)
  expect_true(all(per_person <= 2L))
  expect_identical(categorical_analysis$sensitivity$l1, 2L)

  categorical <- omopDpReleaseDS(table, categorical_spec)
  expect_identical(categorical$levels, c("a", "b", "c"))
  expect_length(categorical$counts, 3L)
  expect_identical(categorical$max_contributions, 2L)

  numeric <- omopDpReleaseDS(table, list(
    statistic = "numeric_histogram",
    variable = "measurement",
    breaks = c(0, 5, 10),
    reducer = "records",
    max_contributions = 2L,
    population_id = "cohort-a"
  ))
  expect_identical(as.numeric(numeric$breaks), c(0, 5, 10))
  expect_length(numeric$counts, 2L)
  expect_identical(numeric$max_contributions, 2L)
  expect_identical(numeric$value_type, "number")

  date <- omopDpReleaseDS(table, list(
    statistic = "numeric_histogram",
    variable = "event_date",
    breaks = c("2020-01-01", "2020-07-01", "2021-01-01"),
    reducer = "records",
    max_contributions = 2L,
    order_by = "event_date",
    population_id = "cohort-a"
  ))
  expect_identical(
    as.character(date$breaks),
    c("2020-01-01", "2020-07-01", "2021-01-01")
  )
  expect_length(date$counts, 2L)
  expect_identical(date$value_type, "date")

  mean <- omopDpReleaseDS(table, list(
    statistic = "bounded_mean",
    variable = "measurement",
    lower = 0,
    upper = 10,
    reducer = "mean",
    population_id = "cohort-a"
  ))
  expect_true(all(c(
    "noisy_count", "noisy_sum_grid", "value", "lower", "upper",
    "numeric_grid", "reducer", "degraded"
  ) %in% names(mean)))
  expect_identical(mean$value_type, "number")
  if (!is.null(mean$value)) expect_true(mean$value >= 0 && mean$value <= 10)

  rate <- omopDpReleaseDS(table, list(
    statistic = "binary_rate",
    variable = "flag",
    positive = "yes",
    reducer = "any",
    denominator = "all_persons",
    population_id = "cohort-a"
  ))
  expect_true(all(c(
    "noisy_numerator", "noisy_denominator", "value", "reducer",
    "denominator", "degraded"
  ) %in% names(rate)))
  expect_identical(rate$value_type, "categorical_utf8_v1")
  expect_lte(rate$noisy_numerator, rate$noisy_denominator)
  if (!is.null(rate$value)) expect_true(rate$value >= 0 && rate$value <= 1)
})

test_that("analysts cannot control noise or select protected identifiers", {
  .dp_local_state()
  table <- .dp_test_table()
  policy <- .dsomopDpPolicy()

  for (field in c("seed", "epsilon", "nonce", "fresh_noise", "reset")) {
    spec <- list(statistic = "count", population_id = "cohort-a")
    spec[[field]] <- if (field == "epsilon") 0.5 else "attacker-controlled"
    expect_error(
      .dsomopDpAnalysis(table, spec, policy),
      "server-owned",
      info = field
    )
  }
  expect_error(
    .dsomopDpAnalysis(table, list(
      statistic = "categorical_histogram",
      variable = "person_id",
      levels = as.character(table$person_id[[1L]]),
      population_id = "cohort-a"
    ), policy),
    "Protected identifier"
  )
})

test_that("irrelevant ordering and unbounded public domains are rejected", {
  .dp_local_state()
  table <- .dp_test_table()
  policy <- .dsomopDpPolicy()

  expect_error(.dsomopDpAnalysis(table, list(
    statistic = "categorical_histogram", variable = "category",
    levels = c("a", "b", "c"), reducer = "presence",
    order_by = "event_date", population_id = "cohort-a"
  ), policy), "order_by is only valid")
  expect_error(.dsomopDpAnalysis(table, list(
    statistic = "numeric_histogram", variable = "measurement",
    breaks = c(0, 5, 10), reducer = "mean",
    order_by = "event_date", population_id = "cohort-a"
  ), policy), "order_by is only valid")
  expect_error(.dsomopDpAnalysis(table, list(
    statistic = "bounded_mean", variable = "measurement",
    lower = -.Machine$double.xmax, upper = .Machine$double.xmax,
    reducer = "mean", population_id = "cohort-a"
  ), policy), "finite positive span")
  expect_error(.dsomopDpAnalysis(table, list(
    statistic = "binary_rate", variable = "flag",
    positive = paste0("level-", seq_len(policy$max_levels + 1L)),
    reducer = "any", denominator = "all_persons",
    population_id = "cohort-a"
  ), policy), "level cap")
})

test_that("bounded allocation never exhausts and degrades data-independently", {
  .dp_local_state(
    accounting_mode = "bounded_accounted",
    total_epsilon = 1,
    release_epsilon = 0.1
  )
  policy <- .dsomopDpPolicy()
  allocations <- lapply(0:5000, .dsomopDpAllocation, policy = policy)
  epsilons <- vapply(allocations, `[[`, numeric(1L), "epsilon")

  expect_lte(sum(epsilons), policy$total_epsilon + 1e-12)
  expect_gt(sum(epsilons), 0.99 * policy$total_epsilon)
  expect_true(any(vapply(allocations, `[[`, logical(1L), "degraded")))
  far_future <- .dsomopDpAllocation(policy, 1e9)
  expect_identical(far_future, list(epsilon = 0, degraded = TRUE))

  small <- .dsomopDpAnalysis(
    .dp_test_table(),
    list(statistic = "count", population_id = "cohort-a"),
    policy
  )
  larger_raw <- data.frame(
    person_id = 100:120,
    category = "a",
    measurement = 1,
    event_date = as.Date("2020-01-01"),
    flag = "no",
    stringsAsFactors = FALSE
  )
  larger <- .dsomopDpAnalysis(
    .dp_seal(
      .testPseudonymize(larger_raw), producer = "test/fixture"
    ),
    list(statistic = "count", population_id = "cohort-a"),
    policy
  )
  context <- list(release_id = "unused-in-degraded-mode")
  expect_identical(
    small$payload_fn(0, policy, context, degraded = TRUE),
    larger$payload_fn(0, policy, context, degraded = TRUE)
  )
})

test_that("terminal bounded allocation returns safely without ledger growth", {
  state <- .dp_local_state(accounting_mode = "bounded_accounted")
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")

  release <- testthat::with_mocked_bindings(
    omopDpReleaseDS(table, spec),
    .dsomopDpAllocation = function(policy, index) {
      list(epsilon = 0, degraded = TRUE)
    },
    .package = "dsOMOP"
  )
  replay <- testthat::with_mocked_bindings(
    omopDpReleaseDS(table, spec),
    .dsomopDpAllocation = function(policy, index) {
      list(epsilon = 0, degraded = TRUE)
    },
    .package = "dsOMOP"
  )

  expect_true(release$degraded)
  expect_identical(release$epsilon, 0L)
  expect_identical(release$noisy_count, 0L)
  expect_identical(replay, release)
  expect_identical(nrow(.dp_ledger_rows(state)), 0L)
  expect_identical(.dp_ledger_meta(state)[["next_index"]], "0")
})

test_that("sticky-unbounded mode is never advertised as formal DP", {
  anchor <- .dp_external_anchor()
  .dp_local_state(
    accounting_mode = "sticky_unbounded",
    require_external_anchor = TRUE,
    anchor_provider = anchor
  )
  status <- .dsomopDpPublicStatus(initialize = TRUE)
  allocation <- .dsomopDpAllocation(.dsomopDpPolicy(), 1e9)

  expect_true(status$ready)
  expect_identical(status$rollback_protection,
                   "external_durable_linearizable_cas")
  expect_false(status$formal_dp)
  expect_false(status$bounded_accounting)
  expect_false(status$bounded_composition)
  expect_true(status$never_budget_blocked)
  expect_identical(allocation$epsilon, 0.1)
  expect_false(allocation$degraded)
})

test_that("ledger payload and accountant tampering fail closed", {
  state <- .dp_local_state()
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")
  omopDpReleaseDS(table, spec)

  path <- file.path(state, "privacy", "ledger.sqlite")
  connection <- DBI::dbConnect(RSQLite::SQLite(), path)
  original_payload <- DBI::dbGetQuery(
    connection, "SELECT payload FROM dp_releases WHERE release_index = 0"
  )$payload[[1L]]
  DBI::dbExecute(
    connection,
    "UPDATE dp_releases SET payload = '{\"statistic\":\"count\"}'"
  )
  DBI::dbDisconnect(connection)

  expect_error(
    omopDpReleaseDS(table, spec),
    "release chain is corrupt"
  )

  connection <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(
    connection,
    "UPDATE dp_releases SET payload = ? WHERE release_index = 0",
    params = list(original_payload)
  )
  DBI::dbExecute(
    connection,
    "UPDATE dp_meta SET value = 'GENESIS' WHERE key = 'chain_head'"
  )
  DBI::dbDisconnect(connection)
  expect_error(
    omopDpReleaseDS(table, spec),
    "accountant or chain head is inconsistent"
  )
})

test_that("a historical tamper is audited before a different release", {
  state <- .dp_local_state()
  table <- .dp_test_table()
  omopDpReleaseDS(table, list(
    statistic = "count", population_id = "cohort-a"
  ))

  path <- file.path(state, "privacy", "ledger.sqlite")
  connection <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(
    connection,
    "UPDATE dp_releases SET payload = '{\"statistic\":\"tampered\"}' WHERE release_index = 0"
  )
  DBI::dbDisconnect(connection)

  expect_error(
    omopDpReleaseDS(table, list(
      statistic = "categorical_histogram", variable = "category",
      levels = c("a", "b", "c"), reducer = "presence",
      max_contributions = 1L, population_id = "cohort-a"
    )),
    "release chain is corrupt"
  )
  expect_identical(nrow(.dp_ledger_rows(state)), 1L)
})

test_that("a cached ledger rejects metadata rollback", {
  state <- .dp_local_state()
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")
  omopDpReleaseDS(table, spec)

  path <- file.path(state, "privacy", "ledger.sqlite")
  connection <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(
    connection,
    "UPDATE dp_meta SET value = '0' WHERE key = 'next_index'"
  )
  DBI::dbDisconnect(connection)

  expect_error(
    omopDpReleaseDS(table, spec),
    "rolled back behind the process checkpoint"
  )
})

test_that("ledger validation scans history on cache miss but not hot status", {
  state <- .dp_local_state()
  table <- .dp_test_table()
  omopDpReleaseDS(table, list(
    statistic = "count", population_id = "cohort-a"
  ))
  omopDpReleaseDS(table, list(
    statistic = "categorical_histogram", variable = "category",
    levels = c("a", "b", "c"), reducer = "presence",
    max_contributions = 1L, population_id = "cohort-a"
  ))

  path <- file.path(state, "privacy", "ledger.sqlite")
  cache_path <- .dsomopPrivateSecretDirectory(path, .allow_test_path = TRUE)
  policy <- .dsomopDpPolicy()
  key <- .dsomopDpLedgerCacheKey(cache_path, policy)
  expect_true(exists(
    key, envir = .pkg_state$dp_ledger_cache, inherits = FALSE
  ))
  rm(list = key, envir = .pkg_state$dp_ledger_cache)

  release_fields <- .dsomopDpReleaseFields
  validated <- 0L
  expect_silent(testthat::with_mocked_bindings(
    .dsomopDpPublicStatus(initialize = TRUE),
    .dsomopDpReleaseFields = function(row) {
      validated <<- validated + 1L
      release_fields(row)
    },
    .package = "dsOMOP"
  ))
  expect_identical(validated, 2L)

  validated <- 0L
  expect_silent(testthat::with_mocked_bindings(
    .dsomopDpPublicStatus(initialize = TRUE),
    .dsomopDpReleaseFields = function(row) {
      validated <<- validated + 1L
      release_fields(row)
    },
    .package = "dsOMOP"
  ))
  expect_identical(validated, 0L)
})

test_that("root and receipt tampering fail closed", {
  skip_on_os("windows")
  state <- .dp_local_state()
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")
  omopDpReleaseDS(table, spec)

  noise_path <- file.path(state, "secrets", "dp_noise_root")
  receipt_path <- file.path(state, "secrets", "dp_noise_root_receipt")
  original_receipt <- .dsomopValidateSecretFile(receipt_path)
  writeBin(as.raw(rep.int(5L, 32L)), receipt_path)
  Sys.chmod(receipt_path, mode = "0600")
  expect_error(
    omopDpReleaseDS(table, spec),
    "continuity receipt does not match"
  )

  writeBin(original_receipt, receipt_path)
  Sys.chmod(receipt_path, mode = "0600")
  expect_silent(omopDpReleaseDS(table, spec))

  writeBin(as.raw(rep.int(7L, 32L)), noise_path)
  Sys.chmod(noise_path, mode = "0600")
  expect_error(
    omopDpReleaseDS(table, spec),
    "continuity receipt does not match"
  )
})

test_that("ledger receipt tampering fails closed", {
  skip_on_os("windows")
  state <- .dp_local_state()
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")
  omopDpReleaseDS(table, spec)

  receipt <- file.path(state, "privacy", "ledger.sqlite.receipt")
  original <- .dsomopValidateSecretFile(receipt)
  writeBin(as.raw(rep.int(13L, 32L)), receipt)
  Sys.chmod(receipt, mode = "0600")
  withr::local_options(list(dsomop.dp.privacy_epoch = 7L))
  .dp_restart_runtime()
  expect_error(
    omopDpReleaseDS(table, spec),
    "continuity receipt does not match"
  )
  connection <- DBI::dbConnect(
    RSQLite::SQLite(), file.path(state, "privacy", "ledger.sqlite")
  )
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  expect_identical(
    DBI::dbGetQuery(
      connection,
      "SELECT value FROM dp_meta WHERE key = 'privacy_epoch'"
    )$value[[1L]],
    "1"
  )
  writeBin(original, receipt)
  Sys.chmod(receipt, mode = "0600")
})

test_that("anchor incompatibility cannot commit rotation metadata", {
  skip_on_os("windows")
  anchor <- .dp_external_anchor()
  state <- .dp_local_state(
    require_external_anchor = TRUE, anchor_provider = anchor
  )
  expect_true(omopDpStatusDS()$ready)
  ledger <- file.path(state, "privacy", "ledger.sqlite")
  read_rotation_meta <- function() {
    connection <- DBI::dbConnect(RSQLite::SQLite(), ledger)
    on.exit(DBI::dbDisconnect(connection), add = TRUE)
    values <- DBI::dbGetQuery(
      connection,
      paste(
        "SELECT key, value FROM dp_meta WHERE key IN",
        "('privacy_epoch', 'current_noise_key_id', 'noise_generation')"
      )
    )
    stats::setNames(values$value, values$key)
  }
  before <- read_rotation_meta()
  anchored <- anchor("read", anchor_id = "unused")
  anchored$ledger_id <- strrep("f", 64L)
  storage <- get("storage", envir = environment(anchor), inherits = FALSE)
  storage$value <- anchored

  new_root <- as.raw(seq.int(31L, 0L))
  root_path <- file.path(state, "secrets", "dp_noise_root")
  receipt_path <- file.path(state, "secrets", "dp_noise_root_receipt")
  writeBin(new_root, root_path)
  writeBin(.dsomopDpHmacRaw(
    new_root, "dsOMOP/dp/noise-root-continuity-receipt/v1"
  ), receipt_path)
  Sys.chmod(c(root_path, receipt_path), mode = "0600")
  withr::local_options(list(dsomop.dp.privacy_epoch = 7L))
  .dp_restart_runtime()

  expect_error(omopDpStatusDS(), "ahead of or bound to a different ledger")
  expect_identical(read_rotation_meta(), before)
})

test_that("an empty ledger cannot replace state covered by a receipt", {
  skip_on_os("windows")
  state <- .dp_local_state()
  expect_true(omopDpStatusDS()$ready)
  ledger <- file.path(state, "privacy", "ledger.sqlite")
  receipt <- paste0(ledger, ".receipt")
  expect_true(file.exists(receipt))

  expect_identical(unlink(ledger, force = TRUE), 0L)
  connection <- DBI::dbConnect(RSQLite::SQLite(), ledger)
  DBI::dbDisconnect(connection)
  Sys.chmod(ledger, mode = "0600")
  .dp_restart_runtime()

  expect_error(
    omopDpStatusDS(),
    "empty DP ledger conflicts with an existing continuity receipt"
  )
  connection <- DBI::dbConnect(RSQLite::SQLite(), ledger)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)
  expect_length(DBI::dbListTables(connection), 0L)
})

test_that("a symlinked ledger fails closed before SQLite opens it", {
  skip_on_os("windows")
  state <- .dp_local_state()
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")
  omopDpReleaseDS(table, spec)

  path <- file.path(state, "privacy", "ledger.sqlite")
  target <- file.path(state, "privacy", "ledger.backup")
  expect_true(file.rename(path, target))
  expect_true(file.symlink(target, path))
  expect_error(
    omopDpReleaseDS(table, spec),
    "owner-only regular file"
  )
})

test_that("concurrent identical releases converge on one stored payload", {
  skip_on_os("windows")
  state <- .dp_local_state()
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")
  .dsomopDpPublicStatus(initialize = TRUE)

  values <- parallel::mclapply(
    1:2,
    function(unused) omopDpReleaseDS(table, spec),
    mc.cores = 2L,
    mc.preschedule = FALSE
  )

  expect_false(any(vapply(values, inherits, logical(1L), "try-error")))
  expect_identical(values[[1L]], values[[2L]])
  expect_identical(nrow(.dp_ledger_rows(state)), 1L)
})

test_that("onLoad runs the KAT but never bootstraps service secrets", {
  events <- character(0)
  old_status <- .pkg_state$dp_status
  old_resolver <- .pkg_state$resolver
  old_binding <- .pkg_state$dp_bootstrap_binding
  old_in_progress <- .pkg_state$dp_bootstrap_in_progress
  on.exit({
    .pkg_state$dp_status <- old_status
    .pkg_state$resolver <- old_resolver
    .pkg_state$dp_bootstrap_binding <- old_binding
    .pkg_state$dp_bootstrap_in_progress <- old_in_progress
  }, add = TRUE)
  withr::local_envvar(c(
    DEVTOOLS_LOAD = NA_character_,
    R_INSTALL_PKG = NA_character_,
    R_PACKAGE_DIR = NA_character_,
    `_R_CHECK_PACKAGE_NAME_` = NA_character_
  ))
  testthat::local_mocked_bindings(
    .dsomopDpCanonicalSelfTest = function() {
      events <<- c(events, "kat")
      invisible(TRUE)
    },
    .dsomopPseudonymLifecycleSettings = function() {
      events <<- c(events, "pseudonym-settings")
      list()
    },
    .dsomopDpEnabled = function() {
      stop("DP configuration must not be read during namespace load")
    },
    .cleanStaleStagingDirs = function() invisible(NULL),
    .package = "dsOMOP"
  )
  testthat::local_mocked_bindings(
    registerResourceResolver = function(resolver) invisible(NULL),
    .package = "resourcer"
  )

  expect_silent(.onLoad("/opt/R/00LOCK-dsOMOP/00new", "dsOMOP"))
  expect_identical(events, "kat")
  expect_false(.pkg_state$dp_status$ready)

  events <- character(0)
  runtime_library <- withr::local_tempdir(pattern = "dsomop-runtime-lib-")
  expect_silent(.onLoad(runtime_library, "dsOMOP"))
  expect_identical(events, c("kat", "pseudonym-settings"))
  expect_false(.pkg_state$dp_status$ready)
  expect_identical(
    .pkg_state$dp_status$bootstrap, "pending_first_service_use"
  )
  expect_null(.pkg_state$dp_bootstrap_binding)
})

test_that("image load is key-free and first service use is complete", {
  skip_on_os("windows")
  # Armadillo loads packages before applying final profile options. Simulate a
  # disabled load followed by enabled runtime configuration.
  state <- .dp_local_state(enabled = FALSE)
  previous_resolver <- .pkg_state$resolver
  on.exit(.pkg_state$resolver <- previous_resolver, add = TRUE)
  testthat::local_mocked_bindings(
    .dsomopPseudonymLifecycleSettings = function() list(),
    .cleanStaleStagingDirs = function() invisible(NULL),
    .package = "dsOMOP"
  )
  testthat::local_mocked_bindings(
    registerResourceResolver = function(resolver) invisible(NULL),
    .package = "resourcer"
  )
  artifacts <- c(
    file.path(state, "secrets", "dp_noise_root"),
    file.path(state, "secrets", "dp_ledger_root"),
    file.path(state, "privacy", "ledger.sqlite")
  )

  expect_silent(.onLoad("/opt/dsomop/image-library", "dsOMOP"))
  expect_false(any(file.exists(artifacts)))
  expect_null(.pkg_state$dp_bootstrap_binding)

  withr::local_options(list(dsomop.dp.enabled = TRUE))
  first <- omopDpStatusDS()
  expect_true(all(file.exists(artifacts)))
  expect_true(first$ready)
  expect_match(first$privacy_instance_id, "^dpi_[0-9a-f]{40}$")
  expect_match(first$noise_domain_id, "^dpn_[0-9a-f]{40}$")
  expect_match(first$ledger_key_id, "^dpl_[0-9a-f]{40}$")
  expect_match(first$ledger_id, "^[0-9a-f]{64}$")

  .dp_restart_runtime()
  restarted <- omopDpStatusDS()
  expect_identical(restarted$privacy_instance_id, first$privacy_instance_id)
  expect_identical(restarted$ledger_key_id, first$ledger_key_id)
  expect_identical(restarted$noise_key_id, first$noise_key_id)
})

test_that("concurrent first service use converges on one privacy identity", {
  skip_on_os("windows")
  state <- .dp_local_state()
  statuses <- parallel::mclapply(
    seq_len(4L), function(unused) omopDpStatusDS(),
    mc.cores = 4L, mc.preschedule = FALSE
  )

  expect_false(any(vapply(statuses, inherits, logical(1L), "try-error")))
  identities <- vapply(
    statuses, `[[`, character(1L), "privacy_instance_id"
  )
  expect_length(unique(identities), 1L)
  expect_identical(file.info(
    file.path(state, "secrets", "dp_noise_root")
  )$size, 32)
  expect_identical(file.info(
    file.path(state, "secrets", "dp_ledger_root")
  )$size, 32)

  parent <- omopDpStatusDS()
  expect_identical(parent$privacy_instance_id, identities[[1L]])
})

test_that("the central policy guard bootstraps and empty nodes differ", {
  first_state <- .dp_local_state()
  expect_false(file.exists(file.path(
    first_state, "secrets", "dp_noise_root"
  )))
  expect_silent(.dsomopDpPolicy())
  first_id <- .pkg_state$dp_status$privacy_instance_id
  expect_match(first_id, "^dpi_[0-9a-f]{40}$")
  expect_true(file.exists(file.path(
    first_state, "privacy", "ledger.sqlite"
  )))

  second_state <- .dp_local_state()
  second_id <- omopDpStatusDS()$privacy_instance_id
  expect_true(file.exists(file.path(
    second_state, "privacy", "ledger.sqlite"
  )))
  expect_false(identical(second_id, first_id))
})

test_that("a real-service bootstrap rejects late DP enablement", {
  .dp_local_state(enabled = FALSE)
  expect_silent(.dsomopDpEnsureRuntime())
  withr::local_options(list(dsomop.dp.enabled = TRUE))
  expect_error(.dsomopDpEnabled(), "changed after service bootstrap")
})
