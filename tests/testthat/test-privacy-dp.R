.dp_environment_names <- c(
  "DSOMOP_DP_ENABLED", "DSOMOP_DP_DISJOINT_PERSONS",
  "DSOMOP_DP_DOMAIN", "DSOMOP_DP_SNAPSHOT_ID",
  "DSOMOP_DP_RELEASE_EPSILON", "DSOMOP_DP_PRIVACY_EPOCH",
  "DSOMOP_DP_MAX_LEVELS", "DSOMOP_DP_MAX_CONTRIBUTIONS",
  "DSOMOP_DP_NUMERIC_GRID", "DSOMOP_DP_NOISE_ROOT"
)

.dp_local_state <- function(release_epsilon = 0.1,
                            enabled = TRUE,
                            noise_root = NULL,
                            snapshot_id = "etl-2026-08-01",
                            privacy_epoch = 1L,
                            disjoint_persons = FALSE,
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
    dsomop.dp.disjoint_persons = disjoint_persons,
    dsomop.dp.domain = "dsomop-dp-test",
    dsomop.dp.snapshot_id = snapshot_id,
    dsomop.dp.release_epsilon = release_epsilon,
    dsomop.dp.privacy_epoch = privacy_epoch,
    dsomop.dp.noise_root = noise_root,
    dsomop.dp.max_levels = 1000L,
    dsomop.dp.max_contributions = 10L,
    dsomop.dp.numeric_grid = 65535L
  ), .local_envir = .local_envir)
  previous_runtime <- list(
    status = .pkg_state$dp_status,
    runtime = .pkg_state$dp_runtime,
    in_progress = .pkg_state$dp_bootstrap_in_progress
  )
  withr::defer({
    .pkg_state$dp_status <- previous_runtime$status
    .pkg_state$dp_runtime <- previous_runtime$runtime
    .pkg_state$dp_bootstrap_in_progress <- previous_runtime$in_progress
  }, envir = .local_envir)
  .pkg_state$dp_status <- NULL
  .pkg_state$dp_runtime <- NULL
  .pkg_state$dp_bootstrap_in_progress <- FALSE
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
  .pkg_state$dp_runtime <- NULL
  .pkg_state$dp_bootstrap_in_progress <- FALSE
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

.dp_state_files <- function(state) {
  files <- list.files(
    state, recursive = TRUE, full.names = TRUE, all.files = TRUE,
    no.. = TRUE, include.dirs = FALSE
  )
  sort(substring(files, nchar(state) + 2L), method = "radix")
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
    dsomop.dp.release_epsilon = NULL,
    default.dsomop.dp.enabled = FALSE,
    default.dsomop.dp.release_epsilon = 0.1
  ))
  dp_environment <- stats::setNames(
    rep.int(NA_character_, length(.dp_environment_names)),
    .dp_environment_names
  )
  dp_environment[c(
    "DSOMOP_DP_ENABLED", "DSOMOP_DP_RELEASE_EPSILON"
  )] <- c("true", "0.25")
  withr::local_envvar(dp_environment)
  expect_true(.dsomopDpEnabled())
  expect_identical(.dsomopDpOption("release_epsilon"), 0.25)

  withr::local_options(list(
    dsomop.dp.enabled = TRUE,
    dsomop.dp.release_epsilon = 0.25
  ))
  expect_true(.dsomopDpEnabled())
  expect_identical(.dsomopDpOption("release_epsilon"), 0.25)
  withr::with_envvar(c(DSOMOP_DP_ENABLED = NA_character_), {
    expect_true(.dsomopDpEnabled())
  })

  withr::local_options(list(dsomop.dp.enabled = FALSE))
  expect_error(.dsomopDpEnabled(), "Conflicting DP option")

  withr::local_options(list(
    dsomop.dp.enabled = NULL,
    dsomop.dp.disjoint_persons = TRUE
  ))
  withr::with_envvar(c(DSOMOP_DP_DISJOINT_PERSONS = "yes"), {
    expect_true(.dsomopDpBoolean(
      .dsomopDpOption("disjoint_persons"), "disjoint_persons"
    ))
  })
  withr::with_envvar(c(DSOMOP_DP_DISJOINT_PERSONS = "false"), {
    expect_error(
      .dsomopDpOption("disjoint_persons"), "Conflicting DP option"
    )
  })
  withr::with_envvar(c(DSOMOP_DP_DISJOINT_PERSONS = "sometimes"), {
    expect_error(
      .dsomopDpOption("disjoint_persons"), "must be TRUE or FALSE"
    )
  })
})

test_that("disjoint-person attestation does not mint new sticky noise", {
  .dp_local_state(noise_root = as.raw(0:31), disjoint_persons = FALSE)
  input <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")

  first_policy <- .dsomopDpPolicy()
  first_status <- omopDpStatusDS()
  first_release <- omopDpReleaseDS(input, spec)
  expect_false(first_status$disjoint_persons)

  options(dsomop.dp.disjoint_persons = TRUE)
  expect_error(.dsomopDpPolicy(), "DP policy changed during this R session")
  .dp_restart_runtime()

  second_policy <- .dsomopDpPolicy()
  second_status <- omopDpStatusDS()
  second_release <- omopDpReleaseDS(input, spec)
  expect_true(second_status$disjoint_persons)
  expect_identical(first_policy$policy_hash, second_policy$policy_hash)
  expect_identical(first_release, second_release)
})

test_that("injected DP roots require exactly 256 bits", {
  expect_error(
    .coerceDsomopDpRoot(strrep("not-a-csprng-root-", 2L), "DP noise root"),
    "exactly 32 raw CSPRNG bytes or 64 hexadecimal characters"
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

test_that("bootstrap creates only one private persistent root", {
  skip_on_os("windows")
  state <- .dp_local_state()
  status <- .dsomopDpPublicStatus(initialize = TRUE)

  expect_true(status$enabled)
  expect_true(status$ready)
  expect_true(status$sticky_noise)
  expect_identical(status$protocol, "dsomop-dp-release-v2")
  expect_identical(status$privacy_contract,
                   "fixed_per_release_semantic_prf_v1")
  expect_identical(status$privacy_call_quota, "none")
  expect_identical(status$history_dependent, FALSE)
  expect_identical(status$disjoint_persons, FALSE)
  expect_identical(status$persistent_state, "noise_root_only")
  expect_identical(status$privacy_guarantee,
                   .DSOMOP_PRIVACY_GUARANTEE)

  secrets <- file.path(state, "secrets")
  noise_path <- file.path(secrets, "dp_noise_root")
  private_files <- c(noise_path, paste0(noise_path, ".lock"))

  expect_true(all(file.exists(private_files)))
  expect_true(all(vapply(c(state, secrets), function(path) {
    identical(as.integer(file.info(path)$mode), 448L) # 0700
  }, logical(1L))))
  expect_true(all(vapply(private_files, function(path) {
    identical(as.integer(file.info(path)$mode), 384L) # 0600
  }, logical(1L))))
  expect_identical(file.info(noise_path)$size, 32)
  expect_match(status$noise_domain_id, "^dpn_[0-9a-f]{40}$")
  expect_setequal(.dp_state_files(state), c(
    "secrets/dp_noise_root", "secrets/dp_noise_root.lock"
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

test_that("filters accumulate canonically and unsafe transforms invalidate", {
  .dp_local_state()
  raw <- data.frame(
    person_id = 1:24,
    group = rep(c("A", "B"), each = 12L),
    site = rep(c("X", "Y"), 12L),
    scope = c(rep("keep", 12L), rep(c("keep", "drop"), 6L)),
    value = seq_len(24),
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
  selected <- omopSelectDS(filtered, c("group", "site", "value"))
  reordered_selection <- omopSelectDS(filtered, c("value", "site", "group"))
  expect_silent(.dsomopDpVerifyPersonLocal(selected))
  expect_identical(.dp_lineage(selected), .dp_lineage(filtered))
  expect_identical(.dp_lineage(reordered_selection), .dp_lineage(filtered))

  tampered_state <- filtered
  state <- attr(
    tampered_state, .DSOMOP_DP_FILTER_STATE_ATTRIBUTE, exact = TRUE
  )
  state$filter_tree <- .dsomopDpNormalizeFilterTree(list(
    var = "group", op = "in", value = "B"
  ))
  attr(tampered_state, .DSOMOP_DP_FILTER_STATE_ATTRIBUTE) <- state
  expect_error(
    omopFilterDS(tampered_state, "site", "==", "X"),
    "provenance MAC|filter state does not match"
  )

  # Changing ad-hoc filters accumulate into one canonical conjunction, so
  # execution order and a row-preserving projection cannot reroll noise.
  twice_filtered <- omopFilterDS(filtered, "site", "==", "X")
  reverse_filtered <- omopFilterDS(
    omopFilterDS(source, "site", "in", "X"), "group", "in", "A"
  )
  projected_then_filtered <- omopFilterDS(selected, "site", "in", "X")
  reordered_then_filtered <- omopFilterDS(
    reordered_selection, "site", "==", "X"
  )
  expect_identical(nrow(twice_filtered), 6L)
  expect_silent(.dsomopDpVerifyPersonLocal(twice_filtered))
  expect_identical(.dp_lineage(twice_filtered), .dp_lineage(reverse_filtered))
  expect_identical(
    .dp_lineage(twice_filtered), .dp_lineage(projected_then_filtered)
  )
  expect_identical(
    .dp_lineage(twice_filtered), .dp_lineage(reordered_then_filtered)
  )
  # Canonical identity must not depend on which conjunct happens to become a
  # data-dependent no-op after the first filter.
  redundant_after_group <- omopFilterDS(filtered, "scope", "==", "keep")
  redundant_after_value <- omopFilterDS(
    omopFilterDS(source, "scope", "==", "keep"), "group", "==", "A"
  )
  expect_identical(
    .dp_lineage(redundant_after_group),
    .dp_lineage(redundant_after_value)
  )
  release_spec <- list(statistic = "count")
  expect_identical(
    omopDpReleaseDS(twice_filtered, release_spec),
    omopDpReleaseDS(reordered_then_filtered, release_spec)
  )

  left <- .dp_seal(
    .testPseudonymize(raw[1:6, , drop = FALSE]), producer = "test/left"
  )
  right <- .dp_seal(
    .testPseudonymize(raw[7:12, , drop = FALSE]), producer = "test/right"
  )
  bound <- omopBindRowsDS(left, right)
  reverse_bound <- omopBindRowsDS(right, left)
  duplicate_bound <- omopBindRowsDS(left, left)
  expect_true(all(vapply(
    list(bound, reverse_bound, duplicate_bound),
    function(value) is.null(attr(
      value, "dsomop_dp_provenance", exact = TRUE
    )), logical(1L)
  )))

  joined <- omopMergeDS(source, omopSelectDS(source, "value"))
  expect_null(attr(joined, "dsomop_dp_provenance", exact = TRUE))
  expect_null(attr(
    omopFilterDS(joined, "site", "==", "X"),
    "dsomop_dp_provenance", exact = TRUE
  ))

  bound_filtered <- omopBindRowsDS(filtered, other_filter)
  expect_null(attr(
    bound_filtered, "dsomop_dp_provenance", exact = TRUE
  ))
  expect_null(attr(
    omopFilterDS(bound_filtered, "site", "==", "X"),
    "dsomop_dp_provenance", exact = TRUE
  ))
})

test_that("negative filter aliases share semantics and sticky lineage", {
  .dp_local_state()
  raw <- data.frame(
    person_id = 1:13,
    group = c(rep("A", 6L), rep("B", 6L), NA_character_),
    stringsAsFactors = FALSE
  )
  source <- .dp_seal(
    .testPseudonymize(raw), producer = "test/negative-filter-aliases"
  )

  not_equal <- omopFilterDS(source, "group", "!=", "B")
  singleton_not_in <- omopFilterDS(source, "group", "not_in", "B")

  expect_identical(not_equal$person_id, singleton_not_in$person_id)
  expect_identical(not_equal$group, singleton_not_in$group)
  expect_false(anyNA(not_equal$group))
  expect_identical(.dp_lineage(not_equal), .dp_lineage(singleton_not_in))
})

test_that("row-bind invalidates provenance but reduction remains deterministic", {
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
  expect_null(attr(forward, "dsomop_dp_provenance", exact = TRUE))
  expect_null(attr(reverse, "dsomop_dp_provenance", exact = TRUE))
  expect_identical(
    .dsomopDpReduceOne(
      forward$person_id, forward$measurement, spec$reducer
    ),
    .dsomopDpReduceOne(
      reverse$person_id, reverse$measurement, spec$reducer
    )
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

test_that("merge validates boundaries and invalidates DP provenance", {
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
  expect_true(all(vapply(list(inner, left, swapped), function(value) {
    is.null(attr(value, "dsomop_dp_provenance", exact = TRUE))
  }, logical(1L))))

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
  expect_null(attr(
    first_partition, "dsomop_dp_provenance", exact = TRUE
  ))
  expect_null(attr(
    second_partition, "dsomop_dp_provenance", exact = TRUE
  ))

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
  expect_null(attr(
    concept_join, "dsomop_dp_provenance", exact = TRUE
  ))

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
  expect_null(attr(
    episode_join, "dsomop_dp_provenance", exact = TRUE
  ))
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

  rowid_x <- episode_x
  rowid_y <- episode_y
  rowid_x$row_id <- rowid_x$cohort_row_id
  rowid_y$row_id <- rowid_y$cohort_row_id
  expect_error(
    omopMergeDS(rowid_x, rowid_y, by = c("person_id", "row_id")),
    "only cohort_row_id"
  )
})

test_that("factor harmonization invalidates DP provenance", {
  .dp_local_state()
  raw <- data.frame(
    person_id = 1:12,
    cohort_row_id = 101:112,
    gender_concept_id = rep(c(8507L, 8532L), 6L),
    race_concept_id = rep(c(1L, 2L), 6L),
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

  expect_true(all(vapply(list(first, reordered, two_columns), function(value) {
    is.null(attr(value, "dsomop_dp_provenance", exact = TRUE))
  }, logical(1L))))
  expect_null(attr(
    omopFilterDS(first, "gender_concept_id", "==", "8507"),
    "dsomop_dp_provenance", exact = TRUE
  ))
  no_op <- omopAsFactorColumnsDS(first, list(
    gender_concept_id = levels(first$gender_concept_id)
  ))
  expect_identical(no_op, first)
  expect_null(attr(no_op, "dsomop_dp_provenance", exact = TRUE))

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

test_that("derived-only person-level age plans carry DP provenance", {
  .dp_local_state()
  plan <- list(
    outputs = list(ages = list(
      type = "person_level",
      tables = list(),
      representation = "features",
      derived_columns = list(age_at_2025 = list(
        kind = "age", name = "age_at_2025", reference = "today",
        reference_date = "2025-07-01"
      )),
      population_id = "base",
      options = list()
    )),
    populations = list(base = list(
      id = "base", label = "All Persons", kind = "criteria"
    )),
    options = list(
      translate_concepts = TRUE, block_sensitive = TRUE,
      factor_concepts = TRUE
    ),
    cohort = NULL,
    scope = NULL
  )

  expect_true(.dsomopDpPlanOutputPersonLocal(plan, "ages", list()))
  expect_silent(.dsomopDpPlanLineageSemantic(
    plan, "ages", .dsomopDpPolicy()
  ))
  expect_error(
    .dsomopDpPersonTableSequence(character()),
    "requires named source tables"
  )

  handle <- create_test_handle()
  identity <- "test://resource-scoped/derived-only-age"
  key <- .testPseudonymKey("derived-only-age")
  resource_id <- substr(
    as.character(openssl::sha256(charToRaw(identity))), 1L, 32L
  )
  pseudonym_environment <- paste(format(key), collapse = "")
  names(pseudonym_environment) <- paste0(
    "DSOMOP_PSEUDONYM_KEY_", resource_id
  )
  withr::local_envvar(c(
    pseudonym_environment,
    DSOMOP_PSEUDONYM_PROVIDER = "scoped",
    DSOMOP_PSEUDONYM_EPOCH = "1",
    DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false",
    DSOMOP_PSEUDONYM_ROOT = "",
    DSOMOP_PSEUDONYM_KEY = ""
  ))
  handle$person_key_identity <- identity
  handle$person_key_id <- .personKeyId(key)
  handle$person_key_provider <- "scoped"
  handle$person_key_epoch <- 1L
  handle$person_key_require_existing <- FALSE
  handle$person_key_contract_version <- 1L
  handle_symbol <- paste0("dp_derived_age_", Sys.getpid())
  .setHandle(handle_symbol, handle)
  on.exit(.removeHandle(handle_symbol), add = TRUE)

  assign_environment <- new.env(parent = environment())
  assign_environment$handle_symbol <- handle_symbol
  assign_environment$plan <- plan
  eval(quote(omopPlanExecuteDS(
    handle_symbol, plan, list(ages = "D_ages_diag")
  )), envir = assign_environment)
  sealed <- get("D_ages_diag", envir = assign_environment, inherits = FALSE)
  expect_silent(.dsomopDpVerifyPersonLocal(sealed))

  spec <- list(
    statistic = "numeric_histogram", variable = "age_at_2025",
    breaks = c(0, 30, 40, 50, 100), reducer = "mean",
    max_contributions = 1L
  )
  first <- omopDpReleaseDS(sealed, spec)
  second <- omopDpReleaseDS(sealed, spec)
  expect_identical(first, second)
  expect_identical(first$statistic, "numeric_histogram")

  changed <- plan
  changed$outputs$ages$derived_columns$age_at_2025$reference_date <-
    "2024-07-01"
  changed_sealed <- .dp_seal_plan(sealed, changed, "ages")
  expect_false(identical(.dp_lineage(sealed), .dp_lineage(changed_sealed)))
})

test_that("derived column lineage uses a closed canonical contract", {
  .dp_local_state()
  semantic_key <- function(specs) {
    .dsomopDpLineageKey(.dsomopDpPlanOutputSemantic(list(
      type = "person_level",
      tables = list(),
      derived_columns = specs,
      filters = list()
    )))
  }
  age <- list(
    kind = "age", name = "age_at_2025", reference = "today",
    reference_date = "2025-07-01"
  )
  reordered <- list(
    reference_date = as.Date("2025-07-01"), reference = "today",
    name = "age_at_2025", kind = "age"
  )

  expect_identical(semantic_key(list(age)), semantic_key(list(reordered)))
  expect_identical(
    semantic_key(list(public_label = age)),
    semantic_key(list(age))
  )
  fixed_index <- age
  fixed_index$reference <- "index"
  expect_identical(semantic_key(list(age)), semantic_key(list(fixed_index)))
  omitted_reference <- age[c("kind", "name", "reference_date")]
  expect_identical(
    semantic_key(list(age)), semantic_key(list(omitted_reference))
  )
  same_year <- age
  same_year$reference_date <- "2025-12-31"
  expect_identical(semantic_key(list(age)), semantic_key(list(same_year)))
  chads_start <- list(
    kind = "chads2", name = "chads2", reference_date = "2025-01-01"
  )
  chads_end <- list(
    reference_date = "2025-12-31", name = "chads2", kind = "chads2"
  )
  expect_identical(
    semantic_key(list(chads_start)), semantic_key(list(chads_end))
  )
  expect_identical(
    semantic_key(list(list(
      kind = "obs_duration", name = "observed_days"
    ))),
    semantic_key(list(list(
      name = "observed_days", period_policy = "total",
      kind = "obs_duration"
    )))
  )

  with_nonce <- age
  with_nonce$ignored_nonce <- "reroll"
  expect_error(
    semantic_key(list(with_nonce)),
    "Unknown field.*ignored_nonce"
  )
  expect_error(
    semantic_key(list(age, age)),
    "names must be unique"
  )
  reserved <- age
  reserved$name <- "person_id"
  expect_error(semantic_key(list(reserved)), "reserved for row identity")

  changed_date <- age
  changed_date$reference_date <- "2024-07-01"
  expect_false(identical(
    semantic_key(list(age)), semantic_key(list(changed_date))
  ))
})

test_that("plan lineage distinguishes persistent cohort definition ids", {
  .dp_local_state()
  policy <- .dsomopDpPolicy()
  lineage <- function(plan) {
    .dsomopDpLineageKey(
      .dsomopDpPlanLineageSemantic(plan, "out", policy)
    )
  }
  base <- list(outputs = list(out = list(
    type = "event_level",
    table = "measurement",
    representation = list(format = "long")
  )))

  cohort_7 <- base
  cohort_7$cohort <- list(
    type = "cohort_table", cohort_definition_id = 7L
  )
  cohort_8 <- cohort_7
  cohort_8$cohort$cohort_definition_id <- 8L
  expect_false(identical(lineage(cohort_7), lineage(cohort_8)))

  population_7 <- base
  population_7$outputs$out$population_id <- "stored"
  population_7$populations <- list(
    base = list(kind = "criteria"),
    stored = list(kind = "criteria", cohort_definition_id = 7L)
  )
  population_8 <- population_7
  population_8$populations$stored$cohort_definition_id <- 8L
  expect_false(identical(lineage(population_7), lineage(population_8)))
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
  nested_aliases <- list(and = list(
    list(var = "group", op = "==", value = "A"),
    list(and = list(
      list(var = "site", op = "in", value = "X"),
      list(var = "status", op = "!=", value = "bad")
    ))
  ))
  flat_aliases <- list(and = list(
    list(var = "status", op = "not_in", value = "bad"),
    list(var = "site", op = "==", value = "X"),
    list(var = "group", op = "in", value = "A")
  ))
  expect_identical(
    .dsomopDpNormalizeFilterTree(nested_aliases),
    .dsomopDpNormalizeFilterTree(flat_aliases)
  )
  nested_or <- list(or = list(
    list(var = "group", op = "eq", value = "A"),
    list(or = list(
      list(var = "site", op = "==", value = "X"),
      list(var = "group", op = "in", value = "A")
    ))
  ))
  flat_or <- list(or = list(
    list(var = "site", op = "in", value = "X"),
    list(var = "group", op = "==", value = "A")
  ))
  expect_identical(
    .dsomopDpNormalizeFilterTree(nested_or),
    .dsomopDpNormalizeFilterTree(flat_or)
  )
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

  advanced <- list(outputs = list(out = list(
    type = "survival",
    outcomes = list(
      primary = list(
        table = "condition_occurrence", concept_set = c(10L, 20L),
        filters = custom_a
      ),
      secondary = list(
        table = "drug_exposure", concept_set = c(30L, 40L)
      )
    ),
    tar = list(start_offset = 1L, end_offset = 60L),
    format = "recurrent_events",
    event_order = "all",
    washout_days = 7L,
    tie_policy = "all",
    censoring = list(
      cohort_end = TRUE, observation_period_end = TRUE, death = TRUE,
      admin_date = "2025-12-31"
    )
  )))
  change_advanced <- function(field, value) {
    changed <- advanced
    changed$outputs$out[[field]] <- value
    changed
  }
  expect_false(identical(
    lineage(advanced),
    lineage(change_advanced("washout_days", 8L))
  ))
  expect_false(identical(
    lineage(advanced),
    lineage(change_advanced("censoring", list(
      cohort_end = TRUE, observation_period_end = FALSE, death = TRUE,
      admin_date = "2025-12-31"
    )))
  ))
  endpoint_changed <- advanced
  endpoint_changed$outputs$out$outcomes$primary$filters <- custom_b
  expect_false(identical(lineage(advanced), lineage(endpoint_changed)))

  multi <- advanced
  multi$outputs$out$format <- "multi_state"
  multi$outputs$out$tie_policy <- "priority"
  multi$outputs$out$initial_state <- "index"
  multi$outputs$out$transitions <- list(
    index = c("primary", "secondary"),
    primary = "secondary",
    secondary = character(0)
  )
  multi$outputs$out$state_hierarchy <- c("primary", "secondary", "index")
  multi_equivalent <- multi
  multi_equivalent$outputs$out$transitions <- list(
    states = c("index", "primary", "secondary"),
    edges = list(
      list(from = "index", to = "primary", trans = 1L),
      list(from = "index", to = "secondary", trans = 2L),
      list(from = "primary", to = "secondary", trans = 3L)
    )
  )
  expect_identical(lineage(multi), lineage(multi_equivalent))
  multi_reordered <- multi
  multi_reordered$outputs$out$outcomes <-
    multi$outputs$out$outcomes[c("secondary", "primary")]
  expect_identical(lineage(multi), lineage(multi_reordered))
  inert_step <- multi
  inert_step$outputs$out$state_step <- 0.02
  expect_identical(lineage(multi), lineage(inert_step))
  multi_changed <- multi
  multi_changed$outputs$out$state_hierarchy <-
    c("secondary", "primary", "index")
  expect_false(identical(lineage(multi), lineage(multi_changed)))
  graph_changed <- multi
  graph_changed$outputs$out$transitions <- list(
    index = "primary", primary = "secondary", secondary = "primary"
  )
  expect_false(identical(lineage(multi), lineage(graph_changed)))

  reversible <- advanced
  reversible$outputs$out$outcomes <- list(
    well = list(table = "observation", concept_set = 10L),
    ill = list(table = "condition_occurrence", concept_set = 20L)
  )
  reversible$outputs$out$format <- "multi_state"
  reversible$outputs$out$tie_policy <- "priority"
  reversible$outputs$out$initial_state <- "well"
  reversible$outputs$out$transitions <- list(well = "ill", ill = "well")
  changed_initial_endpoint <- reversible
  changed_initial_endpoint$outputs$out$outcomes$well$concept_set <- 11L
  expect_false(identical(
    lineage(reversible), lineage(changed_initial_endpoint)
  ))
  reversed_endpoints <- reversible
  reversed_endpoints$outputs$out$outcomes <-
    reversible$outputs$out$outcomes[c("ill", "well")]
  expect_identical(lineage(reversible), lineage(reversed_endpoints))

  interval_semantics <- intervals
  interval_semantics$outputs$out$source_filters <- list(
    condition_occurrence = custom_a
  )
  interval_semantics$outputs$out$window <- list(start = -30L, end = 90L)
  interval_semantics$outputs$out$interval_match <- "overlaps"
  interval_semantics$outputs$out$event_select <- "nearest"
  interval_semantics$outputs$out$select_n <- 2L
  interval_semantics$outputs$out$select_by <- "episode_source_concept"
  interval_semantics$outputs$out[[paste0("an", "chor")]] <- 5L
  interval_window_changed <- interval_semantics
  interval_window_changed$outputs$out$window$end <- 91L
  interval_source_changed <- interval_semantics
  interval_source_changed$outputs$out$source_filters$condition_occurrence <-
    custom_b
  expect_false(identical(
    lineage(interval_semantics), lineage(interval_window_changed)
  ))
  expect_false(identical(
    lineage(interval_semantics), lineage(interval_source_changed)
  ))

  event_component <- .dp_lineage(.dsomopDpSealPlanOutput(
    frame, advanced, "out", dataset_identity = .dp_dataset_identity(),
    component = "events"
  ))
  risk_component <- .dp_lineage(.dsomopDpSealPlanOutput(
    frame, advanced, "out", dataset_identity = .dp_dataset_identity(),
    component = "risk_sets"
  ))
  expect_false(identical(event_component, risk_component))
})

test_that("population lineage binds the global cohort and validates set operations", {
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
  other_cohort <- non_base
  other_cohort$cohort$filter_tree <- sex_m
  expect_false(identical(lineage(non_base), lineage(other_cohort)))

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

test_that("protected bounded snapshots select distinct private release streams", {
  .dp_local_state(noise_root = as.raw(0:31))
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")
  policy <- .dsomopDpPolicy()
  first_analysis <- .dsomopDpAnalysis(table, spec, policy)
  first_binding <- .dsomopDpSnapshotBinding(
    policy, first_analysis$snapshot
  )
  first_id <- .dsomopDpReleaseId(
    policy, first_analysis$semantic, first_binding
  )
  first <- omopDpReleaseDS(table, spec)

  changed <- table
  people <- unique(changed$person_id)
  changed$person_id[changed$person_id == people[[3L]]] <- people[[2L]]
  expect_error(omopDpReleaseDS(changed, spec), "provenance MAC does not match")
  changed <- .dp_seal(changed, producer = "test/fixture")
  changed_analysis <- .dsomopDpAnalysis(changed, spec, policy)
  changed_binding <- .dsomopDpSnapshotBinding(
    policy, changed_analysis$snapshot
  )
  changed_id <- .dsomopDpReleaseId(
    policy, changed_analysis$semantic, changed_binding
  )
  second <- omopDpReleaseDS(changed, spec)

  expect_identical(.dp_lineage(changed), .dp_lineage(table))
  expect_false(identical(first_binding$protected_fingerprint,
                         changed_binding$protected_fingerprint))
  expect_false(identical(first_id, changed_id))
  expect_identical(omopDpReleaseDS(changed, spec), second)
  expect_true(all(vapply(list(first, second), function(value) {
    !any(c(
      "seed", "noise_root", "raw_noise", "snapshot_hash",
      "data_fingerprint", "protected_fingerprint", "semantic_query_id",
      "query_id", "release_id", "noise_key_id"
    ) %in% names(value))
  }, logical(1L))))
  encoded <- vapply(
    list(first, second), .dsomopDpCanonicalJson, character(1L)
  )
  private_values <- c(
    first_binding$protected_fingerprint,
    changed_binding$protected_fingerprint,
    first_id, changed_id
  )
  expect_false(any(vapply(private_values, function(value) {
    any(grepl(value, encoded, fixed = TRUE))
  }, logical(1L))))
})

test_that("same root semantic query and snapshot replay across runtime reset", {
  root <- as.raw(0:31)
  .dp_local_state(noise_root = root)
  table <- .dp_test_table()
  spec <- list(
    statistic = "numeric_histogram", variable = "measurement",
    breaks = c(0, 5, 10), reducer = "records", max_contributions = 2L,
    order_by = "event_date", population_id = "cohort-a"
  )

  first <- omopDpReleaseDS(table, spec)
  first_status <- omopDpStatusDS()
  .dp_restart_runtime()
  second <- omopDpReleaseDS(table, spec)
  second_status <- omopDpStatusDS()

  expect_identical(second, first)
  expect_identical(second_status$noise_key_id, first_status$noise_key_id)
  expect_identical(second_status$noise_domain_id,
                   first_status$noise_domain_id)
})

test_that("canonical aliases replay and distinct authenticated lineage is sticky", {
  .dp_local_state(noise_root = as.raw(0:31))
  table <- .dp_test_table()
  first <- omopDpReleaseDS(table, list(statistic = "count"))
  alias <- omopDpReleaseDS(table, list(
    statistic = "count", population_id = "display-only"
  ))
  raw <- .testPseudonymize(data.frame(
    person_id = 1:8, value = seq_len(8), stringsAsFactors = FALSE
  ))
  lineage_one <- .dp_seal(raw, producer = "test/lineage-one")
  lineage_two <- .dp_seal(raw, producer = "test/lineage-two")
  one <- omopDpReleaseDS(lineage_one, list(statistic = "count"))
  two <- omopDpReleaseDS(lineage_two, list(statistic = "count"))

  expect_identical(alias, first)
  expect_false(identical(.dp_lineage(lineage_one), .dp_lineage(lineage_two)))
  expect_identical(omopDpReleaseDS(lineage_one, list(statistic = "count")),
                   one)
  expect_identical(two$statistic, one$statistic)
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

  record_count_spec <- list(
    statistic = "bounded_record_count", reducer = "records",
    max_contributions = 2L, population_id = "cohort-a"
  )
  record_count_analysis <- .dsomopDpAnalysis(
    table, record_count_spec, policy
  )
  expect_identical(record_count_analysis$snapshot$count, 6)
  expect_identical(record_count_analysis$sensitivity$l1, 2L)
  record_count <- omopDpReleaseDS(table, record_count_spec)
  expect_identical(record_count$statistic, "bounded_record_count")
  expect_identical(record_count$reducer, "records")
  expect_identical(record_count$max_contributions, 2L)
  expect_identical(omopDpReleaseDS(table, record_count_spec), record_count)

  categorical_records_spec <- list(
    statistic = "categorical_histogram", variable = "category",
    levels = c("c", "a", "b"), reducer = "records",
    max_contributions = 2L, order_by = "event_date",
    population_id = "cohort-a"
  )
  categorical_records_analysis <- .dsomopDpAnalysis(
    table, categorical_records_spec, policy
  )
  expect_identical(categorical_records_analysis$snapshot$counts, c(2, 3, 1))
  expect_identical(categorical_records_analysis$sensitivity$l1, 2L)
  expect_identical(
    .dsomopDpAnalysis(
      .dp_test_table(reverse_rows = TRUE), categorical_records_spec, policy
    )$snapshot,
    categorical_records_analysis$snapshot
  )
  categorical_records <- omopDpReleaseDS(table, categorical_records_spec)
  expect_identical(categorical_records$reducer, "records")
  expect_identical(categorical_records$levels, c("a", "b", "c"))
  expect_length(categorical_records$counts, 3L)

  tied_raw <- data.frame(
    person_id = c(1L, 1L, 1L, 2L),
    category = c("c", "a", "b", "c"),
    event_date = as.Date("2020-01-01"), stringsAsFactors = FALSE
  )
  tied <- .dp_seal(
    .testPseudonymize(tied_raw), producer = "test/canonical-record-order"
  )
  tied_reverse <- .dp_seal(
    .testPseudonymize(tied_raw[4:1, , drop = FALSE]),
    producer = "test/canonical-record-order"
  )
  tied_spec <- categorical_records_spec
  tied_spec$population_id <- "tied-order"
  tied_forward <- .dsomopDpAnalysis(tied, tied_spec, policy)
  tied_backward <- .dsomopDpAnalysis(tied_reverse, tied_spec, policy)
  expect_identical(tied_forward$snapshot$counts, c(1, 1, 1))
  expect_identical(tied_forward$snapshot, tied_backward$snapshot)

  distinct_spec <- list(
    statistic = "bounded_distinct", variable = "category",
    levels = c("c", "a", "b"), reducer = "distinct",
    max_contributions = 2L, population_id = "cohort-a"
  )
  distinct_analysis <- .dsomopDpAnalysis(table, distinct_spec, policy)
  expect_identical(distinct_analysis$snapshot$count, 3)
  expect_identical(distinct_analysis$sensitivity$l1, 2L)
  expect_identical(distinct_analysis$semantic$levels, c("a", "b", "c"))
  expect_identical(
    .dsomopDpAnalysis(
      .dp_test_table(reverse_rows = TRUE), distinct_spec, policy
    )$snapshot,
    distinct_analysis$snapshot
  )
  distinct <- omopDpReleaseDS(table, distinct_spec)
  expect_identical(distinct$statistic, "bounded_distinct")
  expect_identical(distinct$domain_size, 3L)
  expect_identical(distinct$selection_order, "canonical_utf8_value_radix")
  expect_identical(omopDpReleaseDS(table, distinct_spec), distinct)
  distinct_cap_one <- distinct_spec
  distinct_cap_one$max_contributions <- 1L
  expect_identical(
    .dsomopDpAnalysis(table, distinct_cap_one, policy)$snapshot$count,
    2
  )

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
    "numeric_grid", "reducer"
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
    "denominator"
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
    statistic = "categorical_histogram", variable = "category",
    levels = c("a", "b", "c"), reducer = "records",
    max_contributions = 2L, population_id = "cohort-a"
  ), policy), "require a public order_by")
  expect_error(.dsomopDpAnalysis(table, list(
    statistic = "bounded_record_count", reducer = "presence",
    max_contributions = 2L, population_id = "cohort-a"
  ), policy), "requires reducer='records'")
  expect_error(.dsomopDpAnalysis(table, list(
    statistic = "bounded_distinct", variable = "category",
    reducer = "distinct", max_contributions = 2L,
    population_id = "cohort-a"
  ), policy), "field 'levels' is required")
  expect_error(.dsomopDpAnalysis(table, list(
    statistic = "bounded_distinct", variable = "category",
    levels = c("a", "b", "c"), reducer = "distinct",
    max_contributions = 2L, order_by = "event_date",
    population_id = "cohort-a"
  ), policy), "canonical value ordering")
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

test_that("public status and release expose the fixed v2 contract", {
  .dp_local_state(
    noise_root = as.raw(0:31), release_epsilon = 0.25,
    disjoint_persons = TRUE
  )
  status <- omopDpStatusDS()

  expect_true(status$enabled)
  expect_true(status$ready)
  expect_true(status$sticky_noise)
  expect_identical(status$protocol, "dsomop-dp-release-v2")
  expect_identical(status$privacy_contract,
                   "fixed_per_release_semantic_prf_v1")
  expect_identical(status$release_epsilon, 0.25)
  expect_identical(status$release_delta, 0)
  expect_identical(status$privacy_call_quota, "none")
  expect_identical(status$history_dependent, FALSE)
  expect_identical(status$disjoint_persons, TRUE)
  expect_identical(status$persistent_state, "noise_root_only")
  expect_setequal(status$supported_statistics, c(
    "count", "bounded_record_count", "categorical_histogram",
    "numeric_histogram", "bounded_distinct", "bounded_mean", "binary_rate"
  ))

  value <- omopDpReleaseDS(
    .dp_test_table(), list(statistic = "count", population_id = "cohort-a")
  )
  expect_setequal(names(value), c(
    "statistic", "noisy_count", "protocol", "mechanism", "adjacency",
    "epsilon", "delta", "sensitivity", "privacy_contract", "sticky", "sampler"
  ))
  expect_identical(value$protocol, "dsomop-dp-release-v2")
  expect_identical(value$privacy_contract,
                   "fixed_per_release_semantic_prf_v1")
  expect_identical(value$epsilon, 0.25)
  expect_identical(value$delta, 0L)
  expect_identical(value$sticky, TRUE)
})

test_that("more than one hundred semantic queries do not grow persistent state", {
  skip_on_os("windows")
  state <- .dp_local_state()
  expect_true(omopDpStatusDS()$ready)
  before <- .dp_state_files(state)
  expect_setequal(before, c(
    "secrets/dp_noise_root", "secrets/dp_noise_root.lock"
  ))

  raw <- .testPseudonymize(data.frame(
    person_id = 1:8, value = seq_len(8), stringsAsFactors = FALSE
  ))
  values <- lapply(seq_len(128L), function(index) {
    input <- .dp_seal(raw, producer = paste0("test/query-", index))
    omopDpReleaseDS(input, list(statistic = "count"))
  })

  expect_length(values, 128L)
  expect_true(all(vapply(
    values, function(value) identical(value$epsilon, 0.1), logical(1L)
  )))
  expect_identical(.dp_state_files(state), before)
  expect_false(any(grepl(
    "\\.sqlite(?:3)?$", .dp_state_files(state), ignore.case = TRUE
  )))
  status <- omopDpStatusDS()
  expect_identical(status$privacy_call_quota, "none")
  expect_identical(status$history_dependent, FALSE)
})

test_that("missing and privately corrupt roots regenerate automatically", {
  skip_on_os("windows")
  state <- .dp_local_state()
  first <- omopDpStatusDS()
  root_path <- file.path(state, "secrets", "dp_noise_root")

  expect_identical(unlink(root_path, force = TRUE), 0L)
  .dp_restart_runtime()
  after_missing <- omopDpStatusDS()
  expect_true(after_missing$ready)
  expect_identical(file.info(root_path)$size, 32)
  expect_false(identical(after_missing$noise_key_id, first$noise_key_id))

  writeBin(as.raw(c(1L, 2L, 3L)), root_path)
  Sys.chmod(root_path, mode = "0600")
  .dp_restart_runtime()
  after_corruption <- omopDpStatusDS()
  expect_true(after_corruption$ready)
  expect_identical(file.info(root_path)$size, 32)
  expect_false(identical(after_corruption$noise_key_id,
                         after_missing$noise_key_id))
  expect_identical(as.integer(file.info(root_path)$mode), 384L)
})

test_that("symbolic-link root substitution fails closed", {
  skip_on_os("windows")
  state <- .dp_local_state()
  expect_true(omopDpStatusDS()$ready)
  root_path <- file.path(state, "secrets", "dp_noise_root")
  target <- file.path(state, "attacker-controlled-key")
  writeBin(as.raw(0:31), target)
  Sys.chmod(target, mode = "0600")
  expect_identical(unlink(root_path, force = TRUE), 0L)
  expect_true(file.symlink(target, root_path))
  .dp_restart_runtime()

  expect_error(omopDpStatusDS(), "symbolic link")
  expect_identical(readBin(target, what = "raw", n = 32L), as.raw(0:31))
})

test_that("unsafe root permissions fail closed", {
  skip_on_os("windows")
  state <- .dp_local_state()
  expect_true(omopDpStatusDS()$ready)
  root_path <- file.path(state, "secrets", "dp_noise_root")
  Sys.chmod(root_path, mode = "0644")
  on.exit(try(Sys.chmod(root_path, mode = "0600"), silent = TRUE), add = TRUE)
  .dp_restart_runtime()

  expect_error(omopDpStatusDS(), "regular owner-only file with mode 0600")
  expect_identical(file.info(root_path)$size, 32)
})

test_that("an injected root performs no filesystem writes", {
  skip_on_os("windows")
  state <- .dp_local_state(noise_root = as.raw(0:31))
  status <- omopDpStatusDS()
  value <- omopDpReleaseDS(
    .dp_test_table(), list(statistic = "count", population_id = "cohort-a")
  )

  expect_true(status$ready)
  expect_identical(status$noise_provider, "custodial_injected")
  expect_identical(value$epsilon, status$release_epsilon)
  expect_length(.dp_state_files(state), 0L)

  .dp_restart_runtime()
  expect_identical(omopDpStatusDS()$noise_key_id, status$noise_key_id)
  expect_length(.dp_state_files(state), 0L)
})

test_that("noise-domain identity detects one root across semantic domains", {
  .dp_local_state(noise_root = as.raw(0:31))
  first <- omopDpStatusDS()

  options(dsomop.dp.domain = "dsomop-dp-other-domain")
  .dp_restart_runtime()
  second <- omopDpStatusDS()

  expect_identical(second$noise_key_id, first$noise_key_id)
  expect_identical(second$noise_domain_id, first$noise_domain_id)
})

test_that("injected root rotation is accepted after runtime reset", {
  skip_on_os("windows")
  state <- .dp_local_state(noise_root = as.raw(0:31))
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")
  first_value <- omopDpReleaseDS(table, spec)
  first_status <- omopDpStatusDS()

  options(dsomop.dp.noise_root = as.raw(32:63))
  .dp_restart_runtime()
  second_status <- omopDpStatusDS()
  expect_error(omopDpReleaseDS(table, spec), "provenance MAC")
  second_value <- omopDpReleaseDS(.dp_test_table(), spec)

  expect_true(second_status$ready)
  expect_identical(second_status$privacy_epoch, first_status$privacy_epoch)
  expect_false(identical(second_status$noise_key_id,
                         first_status$noise_key_id))
  expect_identical(second_value$statistic, first_value$statistic)
  expect_length(.dp_state_files(state), 0L)
})

test_that("concurrent identical releases are deterministic without stored rows", {
  skip_on_os("windows")
  state <- .dp_local_state()
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")
  expect_true(omopDpStatusDS()$ready)
  before <- .dp_state_files(state)

  values <- parallel::mclapply(
    1:4,
    function(unused) omopDpReleaseDS(table, spec),
    mc.cores = 2L,
    mc.preschedule = FALSE
  )

  expect_false(any(vapply(values, inherits, logical(1L), "try-error")))
  expect_true(all(vapply(
    values[-1L], identical, logical(1L), values[[1L]]
  )))
  expect_identical(.dp_state_files(state), before)
})

test_that("onLoad runs self-tests but remains key-free", {
  events <- character(0)
  old_status <- .pkg_state$dp_status
  old_runtime <- .pkg_state$dp_runtime
  old_resolver <- .pkg_state$resolver
  old_in_progress <- .pkg_state$dp_bootstrap_in_progress
  on.exit({
    .pkg_state$dp_status <- old_status
    .pkg_state$dp_runtime <- old_runtime
    .pkg_state$resolver <- old_resolver
    .pkg_state$dp_bootstrap_in_progress <- old_in_progress
  }, add = TRUE)
  withr::local_envvar(c(
    DEVTOOLS_LOAD = NA_character_,
    R_INSTALL_PKG = NA_character_,
    R_PACKAGE_DIR = NA_character_,
    '_R_CHECK_PACKAGE_NAME_' = NA_character_
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
    .pkg_state$dp_status$bootstrap, "pending_first_sticky_use"
  )
  expect_null(.pkg_state$dp_runtime)
})

test_that("image load is key-free and first service use is complete", {
  skip_on_os("windows")
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
  root_path <- file.path(state, "secrets", "dp_noise_root")

  expect_silent(.onLoad("/opt/dsomop/image-library", "dsOMOP"))
  expect_false(file.exists(root_path))
  expect_null(.pkg_state$dp_runtime)

  withr::local_options(list(dsomop.dp.enabled = TRUE))
  first <- omopDpStatusDS()
  expect_true(file.exists(root_path))
  expect_true(first$ready)
  expect_match(first$noise_domain_id, "^dpn_[0-9a-f]{40}$")

  .dp_restart_runtime()
  restarted <- omopDpStatusDS()
  expect_identical(restarted$noise_domain_id, first$noise_domain_id)
  expect_identical(restarted$noise_key_id, first$noise_key_id)
})

test_that("concurrent first service use converges on one noise identity", {
  skip_on_os("windows")
  state <- .dp_local_state()
  statuses <- parallel::mclapply(
    seq_len(4L), function(unused) omopDpStatusDS(),
    mc.cores = 2L, mc.preschedule = FALSE
  )

  expect_false(any(vapply(statuses, inherits, logical(1L), "try-error")))
  identities <- vapply(statuses, function(status) {
    status$noise_domain_id
  }, character(1L))
  expect_length(unique(identities), 1L)
  expect_identical(file.info(
    file.path(state, "secrets", "dp_noise_root")
  )$size, 32)

  parent <- omopDpStatusDS()
  expect_identical(parent$noise_domain_id, identities[[1L]])
})

test_that("the central policy guard initializes status and empty nodes differ", {
  first_state <- .dp_local_state()
  root_path <- file.path(first_state, "secrets", "dp_noise_root")
  expect_false(file.exists(root_path))
  expect_silent(.dsomopDpPolicy())
  expect_true(.pkg_state$dp_status$ready)
  first_id <- .pkg_state$dp_status$noise_domain_id
  expect_match(first_id, "^dpn_[0-9a-f]{40}$")
  expect_true(file.exists(root_path))

  second_state <- .dp_local_state()
  second_id <- omopDpStatusDS()$noise_domain_id
  expect_false(identical(second_id, first_id))
})

test_that("runtime rejects late enablement changes", {
  .dp_local_state(enabled = FALSE)
  expect_silent(.dsomopDpEnsureRuntime())
  withr::local_options(list(dsomop.dp.enabled = TRUE))
  expect_error(.dsomopDpEnabled(), "changed during this R session")
})
