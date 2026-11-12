.dp_environment_names <- c(
  "DSOMOP_DP_ENABLED", "DSOMOP_DP_DOMAIN", "DSOMOP_DP_SNAPSHOT_ID",
  "DSOMOP_DP_RELEASE_EPSILON", "DSOMOP_DP_PRIVACY_EPOCH",
  "DSOMOP_DP_MAX_LEVELS", "DSOMOP_DP_MAX_CONTRIBUTIONS",
  "DSOMOP_DP_NUMERIC_GRID", "DSOMOP_DP_NOISE_ROOT"
)

.dp_local_state <- function(release_epsilon = 0.1,
                            enabled = TRUE,
                            noise_root = NULL,
                            snapshot_id = "etl-2026-08-01",
                            privacy_epoch = 1L,
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
  # A running server: the noise root already exists.
  if (!identical(enabled, FALSE) && is.null(noise_root)) {
    suppressMessages(.dsomopDpNoiseRoot())
  }
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

.dp_test_table <- function(reverse_rows = FALSE, producer = "test/fixture",
                           dataset_identity = .dp_dataset_identity(),
                           data_generation = NULL) {
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
    value <- .dp_seal(value, producer = producer, domains = .dp_domains,
                      dataset_identity = dataset_identity,
                      data_generation = data_generation)
  }
  value
}

# Public categorical domains the fixtures' producers declare (B05).
.dp_domains <- list(category = c("a", "b", "c"), flag = c("no", "yes"))

.dp_state_files <- function(state) {
  files <- list.files(
    state, recursive = TRUE, full.names = TRUE, all.files = TRUE,
    no.. = TRUE, include.dirs = FALSE
  )
  sort(substring(files, nchar(state) + 2L), method = "radix")
}

.dp_root_files <- c("secrets/dp_noise_root", "secrets/dp_noise_root.lock")

# The internal components of one typed release (dsOMOP 2.10.3): the parsed
# public specification, every component with its exact value, and each
# release person's total bounded contribution outside the person total.
.dp_analysis <- function(x, spec, policy = .dsomopDpPolicy()) {
  common <- .dsomopDpParseSpec(x, spec, policy)
  if (identical(common$statistic, "bounded_mean")) {
    .dsomopTypedMeanScale(common)
  }
  input <- .dsomopDpValidateInput(x, policy)
  common$order_value <- .dsomopDpOrderVector(input$x, common$order_by)
  tokens <- .dsomopDpSealedReleaseTokens(x, input$token)
  capsule <- attr(x, "dsomop_dp_provenance", exact = TRUE)
  analysis <- .dsomopTypedAnalysis(input, common, policy, tokens, capsule)
  analysis$common <- common
  analysis$values <- stats::setNames(
    vapply(analysis$components, function(c) c$value, numeric(1L)),
    vapply(analysis$components, function(c) c$coordinate, character(1L)))
  cells <- Filter(function(c) !identical(c$family, "N"), analysis$components)
  token <- unlist(lapply(cells, `[[`, "tokens"), use.names = FALSE)
  weight <- unlist(lapply(cells, `[[`, "contributions"), use.names = FALSE)
  analysis$per_person <- if (length(token)) {
    tapply(weight, token, sum)
  } else numeric(0)
  analysis
}

# A registered public scale for typed bounded means of the fixtures.
.dp_mean_bounds <- list(lower = 0, upper = 100)

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
  forward_spec <- list(
    statistic = "categorical_histogram",
    variable = "category",
    levels = c("c", "a", "b"),
    reducer = "presence",
    max_contributions = 2L,
    population_id = "cohort-a"
  )
  reverse_spec <- list(
    max_contributions = 2L,
    reducer = "presence",
    levels = c("b", "c", "a"),
    variable = "category",
    statistic = "categorical_histogram",
    population_id = "cohort-a"
  )
  forward <- .dp_analysis(.dp_test_table(), forward_spec)
  reverse <- .dp_analysis(.dp_test_table(reverse_rows = TRUE), reverse_spec)
  expect_identical(forward$values, reverse$values)
  expect_identical(forward$owner, reverse$owner)
  expect_identical(forward$common$levels, c("a", "b", "c"))
  expect_identical(
    suppressMessages(omopDpReleaseDS(.dp_test_table(), forward_spec)),
    suppressMessages(omopDpReleaseDS(.dp_test_table(reverse_rows = TRUE),
                                     reverse_spec)))
})

test_that("DP bootstrap settings can be supplied before namespace load", {
  withr::local_options(list(
    dsomop.dp.enabled = NULL,
    dsomop.dp.release_epsilon = NULL,
    default.dsomop.dp.enabled = TRUE,
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

test_that("bootstrap validates the private root and stores nothing else", {
  skip_on_os("windows")
  state <- .dp_local_state()
  status <- .dsomopDpPublicStatus(initialize = TRUE)

  expect_true(status$enabled)
  expect_true(status$ready)
  expect_true(status$sticky_noise)
  expect_identical(status$protocol, "dsomop-dp-release-v3")
  expect_identical(status$privacy_contract,
                   "protected_release_fixed_basis_or_realised_v1")
  expect_identical(status$privacy_call_quota, "none")
  expect_identical(status$history_dependent, FALSE)
  expect_identical(status$persistent_state, "noise_root_only")
  expect_identical(status$release_binding,
                   "fixed_basis_or_realised_contributions_v1")
  expect_identical(status$service_capacity, "none")
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
  files <- .dp_state_files(state)
  expect_setequal(files, c("secrets/dp_noise_root",
                           "secrets/dp_noise_root.lock"))
  expect_true(all(vapply(file.path(state, files), function(path) {
    identical(as.integer(file.info(path)$mode), 384L)
  }, logical(1L))))
})

test_that("DP releases require authenticated person-local provenance", {
  .dp_local_state()
  raw <- .testPseudonymize(data.frame(
    person_id = 1:6, value = seq_len(6), stringsAsFactors = FALSE
  ))
  spec <- list(statistic = "count", population_id = "cohort-a")

  expect_error(
    omopDpReleaseDS(raw, spec),
    "carries no DP provenance"
  )
  sealed <- .dp_seal(raw, producer = "test/fixture")
  expect_silent(.dsomopDpVerifyPersonLocal(sealed))
  # The only output besides the answer is the redacted audit line.
  expect_message(omopDpReleaseDS(sealed, spec), "method=omopDpReleaseDS")

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

test_that("core filters, projections and binds keep values and never add DP provenance", {
  .dp_local_state()
  raw <- data.frame(
    person_id = 1:24,
    group = rep(c("A", "B"), each = 12L),
    site = rep(c("X", "Y"), 12L),
    scope = c(rep("keep", 12L), rep(c("keep", "drop"), 6L)),
    value = seq_len(24),
    stringsAsFactors = FALSE
  )
  source <- .testPseudonymize(raw)
  no_provenance <- function(value) {
    is.null(attr(value, "dsomop_dp_provenance", exact = TRUE))
  }

  filtered <- omopFilterDS(source, "group", "==", "A")
  singleton_in <- omopFilterDS(source, "group", "in", "A")
  expect_identical(nrow(filtered), 12L)
  expect_identical(filtered$value, singleton_in$value)
  selected <- omopSelectDS(filtered, c("group", "site", "value"))
  expect_identical(nrow(selected), 12L)

  twice_filtered <- omopFilterDS(filtered, "site", "==", "X")
  reverse_filtered <- omopFilterDS(
    omopFilterDS(source, "site", "in", "X"), "group", "in", "A"
  )
  expect_identical(nrow(twice_filtered), 6L)
  expect_identical(sort(twice_filtered$value), sort(reverse_filtered$value))

  bound <- omopBindRowsDS(
    .testPseudonymize(raw[1:6, , drop = FALSE]),
    .testPseudonymize(raw[7:12, , drop = FALSE])
  )
  expect_identical(nrow(bound), 12L)
  joined <- omopMergeDS(source, omopSelectDS(source, "value"))
  expect_identical(nrow(joined), 24L)

  expect_true(all(vapply(
    list(filtered, singleton_in, selected, twice_filtered, reverse_filtered,
         bound, joined),
    no_provenance, logical(1L)
  )))
  # The DP channel refuses core tables: they never carry DP provenance.
  expect_error(
    omopDpReleaseDS(twice_filtered, list(statistic = "count")),
    "carries no DP provenance"
  )
})
test_that("negative filter aliases share semantics", {
  .dp_local_state()
  raw <- data.frame(
    person_id = 1:13,
    group = c(rep("A", 6L), rep("B", 6L), NA_character_),
    stringsAsFactors = FALSE
  )
  source <- .testPseudonymize(raw)

  not_equal <- omopFilterDS(source, "group", "!=", "B")
  singleton_not_in <- omopFilterDS(source, "group", "not_in", "B")

  expect_identical(not_equal$person_id, singleton_not_in$person_id)
  expect_identical(not_equal$group, singleton_not_in$group)
  expect_false(anyNA(not_equal$group))
  expect_null(attr(not_equal, "dsomop_dp_provenance", exact = TRUE))
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
  # As before DP, binding factors with differently ordered levels works and
  # keeps every label.
  factor_bound <- omopBindRowsDS(factor_left, factor_right)
  expect_identical(nrow(factor_bound), 12L)
  expect_setequal(as.character(factor_bound$category), c("a", "b"))
})

test_that("merge keeps its ordinary validations and ignores DP provenance", {
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
  # Episode joins work as before DP: DP provenance, episode domains and exact
  # key storage types never restrict a merge.
  mismatched_episode_y <- .dp_seal(
    .testPseudonymize(episode_raw_y), producer = "test/episode-right",
    episode_domain = "episode-domain-b"
  )
  for (other in list(mismatched_episode_y, .testPseudonymize(episode_raw_y))) {
    joined <- omopMergeDS(
      episode_x, other, by = c("person_id", "cohort_row_id"), type = "inner"
    )
    expect_identical(nrow(joined), 6L)
  }
  numeric_episode_y_raw <- episode_raw_y
  numeric_episode_y_raw$cohort_row_id <-
    as.numeric(numeric_episode_y_raw$cohort_row_id)
  numeric_join <- omopMergeDS(
    episode_x, .testPseudonymize(numeric_episode_y_raw),
    by = c("person_id", "cohort_row_id"), type = "inner"
  )
  expect_identical(nrow(numeric_join), 6L)

  rowid_x <- .testPseudonymize(episode_raw_x)
  rowid_y <- .testPseudonymize(episode_raw_y)
  names(rowid_x)[names(rowid_x) == "cohort_row_id"] <- "row_id"
  names(rowid_y)[names(rowid_y) == "cohort_row_id"] <- "row_id"
  rowid_join <- omopMergeDS(rowid_x, rowid_y, by = c("person_id", "row_id"))
  expect_identical(nrow(rowid_join), 6L)
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

test_that("derived-only person-level age plans load without DP provenance", {
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
  loaded <- get("D_ages_diag", envir = assign_environment, inherits = FALSE)
  expect_true("age_at_2025" %in% names(loaded))
  expect_null(attr(loaded, "dsomop_dp_provenance", exact = TRUE))
  expect_error(
    omopDpReleaseDS(loaded, list(statistic = "count")),
    "carries no DP provenance"
  )

  # The DP lineage machinery itself (used by the community tools) still
  # separates different derived semantics.
  changed <- plan
  changed$outputs$ages$derived_columns$age_at_2025$reference_date <-
    "2024-07-01"
  expect_false(identical(
    .dp_lineage(.dp_seal_plan(loaded, plan, "ages")),
    .dp_lineage(.dp_seal_plan(loaded, changed, "ages"))
  ))
})
test_that("DP preparation seals plan outputs while plan loading stays DP-free", {
  .dp_local_state()
  plan <- list(
    outputs = list(ages = list(
      type = "person_level", tables = list(), representation = "features",
      derived_columns = list(age_at_2025 = list(
        kind = "age", name = "age_at_2025", reference = "today",
        reference_date = "2025-07-01"
      )),
      population_id = "base", options = list()
    )),
    populations = list(base = list(
      id = "base", label = "All Persons", kind = "criteria"
    )),
    options = list(translate_concepts = TRUE, block_sensitive = TRUE,
                   factor_concepts = TRUE),
    cohort = NULL, scope = NULL
  )
  handle <- create_test_handle()
  identity <- "test://resource-scoped/dp-prepare"
  key <- .testPseudonymKey("dp-prepare")
  resource_id <- substr(
    as.character(openssl::sha256(charToRaw(identity))), 1L, 32L
  )
  pseudonym_environment <- paste(format(key), collapse = "")
  names(pseudonym_environment) <- paste0("DSOMOP_PSEUDONYM_KEY_", resource_id)
  withr::local_envvar(c(
    pseudonym_environment, DSOMOP_PSEUDONYM_PROVIDER = "scoped",
    DSOMOP_PSEUDONYM_EPOCH = "1", DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false",
    DSOMOP_PSEUDONYM_ROOT = "", DSOMOP_PSEUDONYM_KEY = ""
  ))
  handle$person_key_identity <- identity
  handle$person_key_id <- .personKeyId(key)
  handle$person_key_provider <- "scoped"
  handle$person_key_epoch <- 1L
  handle$person_key_require_existing <- FALSE
  handle$person_key_contract_version <- 1L
  handle_symbol <- paste0("dp_prepare_", Sys.getpid())
  .setHandle(handle_symbol, handle)
  on.exit(.removeHandle(handle_symbol), add = TRUE)

  session <- new.env(parent = environment())
  session$handle_symbol <- handle_symbol
  session$plan <- plan
  eval(quote(omopPlanExecuteDS(handle_symbol, plan, list(ages = "D_ages"))),
       envir = session)
  eval(quote(omopDpPlanExecuteDS(handle_symbol, plan, list(ages = "DP_ages"))),
       envir = session)
  loaded <- get("D_ages", envir = session, inherits = FALSE)
  prepared <- get("DP_ages", envir = session, inherits = FALSE)
  expect_null(attr(loaded, "dsomop_dp_provenance", exact = TRUE))
  expect_false(is.null(attr(prepared, "dsomop_dp_provenance", exact = TRUE)))
  # The prepared rows are the loaded rows; only the seal differs.
  expect_identical(sort(names(prepared)), sort(names(loaded)))
  expect_identical(nrow(prepared), nrow(loaded))
  expect_error(omopDpReleaseDS(loaded, list(statistic = "count")),
               "carries no DP provenance")
  released <- omopDpReleaseDS(prepared, list(statistic = "count"))
  expect_identical(released$statistic, "count")
  expect_identical(omopDpReleaseDS(prepared, list(statistic = "count")),
                   released)

  # Preparing for DP is a community-tool operation that needs DP.
  withr::with_options(list(dsomop.tools.enabled = FALSE), expect_error(
    eval(quote(omopDpPlanExecuteDS(handle_symbol, plan,
                                   list(ages = "DP_off"))), envir = session),
    "community tools .* are disabled"))
  expect_error(
    eval(quote(omopDpPlanExecuteDS(handle_symbol, plan, list(ages = "DP_st"),
                                   output_mode = "staged")), envir = session),
    "in-memory tables only")
  baseline <- list(
    cohort = list(type = "spec",
                  spec = list(type = "condition", concept_set = 201820)),
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(baseline = list(
      type = "baseline", columns = "gender_concept_id",
      derived = "age_at_index"
    )),
    options = list(translate_concepts = FALSE, block_sensitive = TRUE)
  )
  session$baseline <- baseline
  expect_error(
    eval(quote(omopDpPlanExecuteDS(handle_symbol, baseline,
                                   list(baseline = "DP_base"))),
         envir = session),
    "public reference year")
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

test_that("protected fingerprints stay private and select the noise of each answer", {
  .dp_local_state(noise_root = as.raw(0:31))
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")
  first <- suppressMessages(omopDpReleaseDS(table, spec))
  expect_identical(first$protection, "realised_sticky")

  changed <- table
  people <- unique(changed$person_id)
  changed$person_id[changed$person_id == people[[3L]]] <- people[[2L]]
  expect_error(suppressMessages(omopDpReleaseDS(changed, spec)),
               "provenance MAC does not match")
  changed <- .dp_seal(changed, producer = "test/fixture")
  second <- suppressMessages(omopDpReleaseDS(changed, spec))
  expect_identical(.dp_lineage(changed), .dp_lineage(table))
  # A changed realised person set is a different contribution map: its
  # component is drawn from a different fingerprint.
  first_map <- .dp_analysis(table, spec)$components[[1L]]
  changed_map <- .dp_analysis(changed, spec)$components[[1L]]
  keys <- .dsomopReleaseKeys(.dsomopDpPolicy())
  fingerprint <- function(c) .dsomopReleaseFingerprint(
    keys, c$owner, c$family, c$coordinate, c$tokens, c$contributions)
  expect_false(identical(fingerprint(first_map), fingerprint(changed_map)))
  expect_false(identical(first$components$signed, second$components$signed))

  # Display names and row order are part of neither the question nor the
  # protected statistic.
  expect_identical(suppressMessages(omopDpReleaseDS(
    .dp_test_table(reverse_rows = TRUE),
    list(statistic = "count", population_id = "another-display-name")
  )), first)
  expect_identical(serialize(suppressMessages(omopDpReleaseDS(changed, spec)),
                             NULL, version = 3L),
                   serialize(second, NULL, version = 3L))
  expect_true(all(vapply(list(first, second), function(value) {
    !any(c(
      "seed", "noise_root", "raw_noise", "fingerprint", "contributions",
      "tokens", "person_token", "semantic_query_id", "release_id",
      "noise_key_id", "data_version"
    ) %in% c(names(value), names(value$components)))
  }, logical(1L))))
  encoded <- vapply(list(first, second), function(value) {
    paste(deparse(value, control = "digits17"), collapse = "")
  }, character(1L))
  private_values <- c(fingerprint(first_map), fingerprint(changed_map),
                      first_map$tokens, changed_map$tokens)
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
  first_status <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)
  .dp_restart_runtime()
  second <- omopDpReleaseDS(table, spec)
  second_status <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)

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
  categorical_analysis <- .dp_analysis(table, categorical_spec, policy)
  # At most k = 2 labels per person, on cells calibrated for the fixed public
  # maximum K (dsOMOP 2.10.3: the noise amplitude never follows k).
  expect_true(all(categorical_analysis$per_person <= 2))
  expect_identical(categorical_analysis$family, "W")
  expect_equal(unique(vapply(categorical_analysis$components,
                             function(c) c$sensitivity, numeric(1L))),
               policy$max_contributions)

  categorical <- omopDpReleaseDS(table, categorical_spec)
  expect_identical(categorical$levels, c("a", "b", "c"))
  expect_length(categorical$counts, 3L)
  expect_identical(categorical$max_contributions, 2L)

  record_count_spec <- list(
    statistic = "bounded_record_count", reducer = "records",
    max_contributions = 2L, population_id = "cohort-a"
  )
  record_count_analysis <- .dp_analysis(table, record_count_spec, policy)
  multiplicity <- unname(record_count_analysis$values[-1L])
  expect_identical(sum(pmin(seq_along(multiplicity), 2) * multiplicity), 6)
  expect_identical(record_count_analysis$family, "H")
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
  categorical_records_analysis <- .dp_analysis(
    table, categorical_records_spec, policy
  )
  # Cells a, b, c and Other of the complete basis.
  expect_identical(unname(categorical_records_analysis$values), c(2, 3, 1, 0))
  expect_true(all(categorical_records_analysis$per_person <= 2))
  expect_identical(
    .dp_analysis(
      .dp_test_table(reverse_rows = TRUE), categorical_records_spec, policy
    )$values,
    categorical_records_analysis$values
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
    .testPseudonymize(tied_raw), producer = "test/canonical-record-order",
    domains = .dp_domains["category"]
  )
  tied_reverse <- .dp_seal(
    .testPseudonymize(tied_raw[4:1, , drop = FALSE]),
    producer = "test/canonical-record-order",
    domains = .dp_domains["category"]
  )
  tied_spec <- categorical_records_spec
  tied_spec$population_id <- "tied-order"
  tied_forward <- .dp_analysis(tied, tied_spec, policy)
  tied_backward <- .dp_analysis(tied_reverse, tied_spec, policy)
  expect_identical(unname(tied_forward$values), c(1, 1, 1, 0))
  expect_identical(tied_forward$values, tied_backward$values)

  distinct_spec <- list(
    statistic = "bounded_distinct", variable = "category",
    levels = c("c", "a", "b"), reducer = "distinct",
    max_contributions = 2L, population_id = "cohort-a"
  )
  distinct_analysis <- .dp_analysis(table, distinct_spec, policy)
  expect_identical(unname(distinct_analysis$values), 3)
  # Fixed sensitivity K for every k (spec: no private sensitivity downshift).
  expect_equal(distinct_analysis$components[[1L]]$sensitivity,
               policy$max_contributions)
  expect_identical(distinct_analysis$common$levels, c("a", "b", "c"))
  expect_identical(
    .dp_analysis(
      .dp_test_table(reverse_rows = TRUE), distinct_spec, policy
    )$values,
    distinct_analysis$values
  )
  distinct <- omopDpReleaseDS(table, distinct_spec)
  expect_identical(distinct$statistic, "bounded_distinct")
  expect_identical(distinct$domain_size, 3L)
  expect_identical(distinct$selection_order, "canonical_utf8_value_radix")
  expect_identical(omopDpReleaseDS(table, distinct_spec), distinct)
  distinct_cap_one <- distinct_spec
  distinct_cap_one$max_contributions <- 1L
  expect_identical(
    unname(.dp_analysis(table, distinct_cap_one, policy)$values),
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
    lower = .dp_mean_bounds$lower,
    upper = .dp_mean_bounds$upper,
    reducer = "mean",
    population_id = "cohort-a"
  ))
  expect_true(all(c(
    "noisy_count", "value", "sd", "lower", "upper", "scale", "reducer"
  ) %in% names(mean)))
  expect_identical(mean$value_type, "number")
  expect_identical(mean$scale, "count_per_person")
  if (!is.na(mean$value)) expect_true(mean$value >= 0 && mean$value <= 100)

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
      .dp_analysis(table, spec, policy),
      "server-owned",
      info = field
    )
  }
  expect_error(
    .dp_analysis(table, list(
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

  expect_error(.dp_analysis(table, list(
    statistic = "categorical_histogram", variable = "category",
    levels = c("a", "b", "c"), reducer = "presence",
    order_by = "event_date", population_id = "cohort-a"
  ), policy), "order_by is only valid")
  expect_error(.dp_analysis(table, list(
    statistic = "categorical_histogram", variable = "category",
    levels = c("a", "b", "c"), reducer = "records",
    max_contributions = 2L, population_id = "cohort-a"
  ), policy), "require a public order_by")
  expect_error(.dp_analysis(table, list(
    statistic = "bounded_record_count", reducer = "presence",
    max_contributions = 2L, population_id = "cohort-a"
  ), policy), "requires reducer='records'")
  # A distinct cardinality is over the complete registered domain: its
  # levels may only restate it (B05).
  expect_error(.dp_analysis(table, list(
    statistic = "bounded_distinct", variable = "category",
    levels = c("a", "b"), reducer = "distinct", max_contributions = 2L,
    population_id = "cohort-a"
  ), policy), "complete registered public domain")
  expect_error(.dp_analysis(table, list(
    statistic = "bounded_distinct", variable = "category",
    levels = c("a", "b", "c"), reducer = "distinct",
    max_contributions = 2L, order_by = "event_date",
    population_id = "cohort-a"
  ), policy), "canonical value ordering")
  expect_error(.dp_analysis(table, list(
    statistic = "numeric_histogram", variable = "measurement",
    breaks = c(0, 5, 10), reducer = "mean",
    order_by = "event_date", population_id = "cohort-a"
  ), policy), "order_by is only valid")
  expect_error(.dp_analysis(table, list(
    statistic = "bounded_mean", variable = "measurement",
    lower = -.Machine$double.xmax, upper = .Machine$double.xmax,
    reducer = "mean", population_id = "cohort-a"
  ), policy), "finite positive span")
  expect_error(.dp_analysis(table, list(
    statistic = "binary_rate", variable = "flag",
    positive = paste0("level-", seq_len(policy$max_levels + 1L)),
    reducer = "any", denominator = "all_persons",
    population_id = "cohort-a"
  ), policy), "level cap")
})

test_that("first and last admission is unchanged by resealed private values", {
  state <- .dp_local_state(noise_root = as.raw(0:31))
  observed <- data.frame(
    person_id = c(1L, 1L, 2L), category = c("a", "b", "a"),
    measurement = c(1, 8, 3), flag = c("yes", "no", "yes"),
    event_date = as.Date("2020-01-01") + 0:2,
    stringsAsFactors = FALSE
  )
  missing <- observed
  missing$category[] <- missing$flag[] <- NA_character_
  missing$measurement[] <- NA_real_
  inputs <- lapply(list(missing = missing, observed = observed,
                        empty = observed[FALSE, , drop = FALSE]), function(raw) {
    .dp_seal(.testPseudonymize(raw), producer = "test/public-admission",
             domains = list(category = c("a", "b"), flag = c("no", "yes")))
  })
  expect_length(unique(vapply(inputs, .dp_lineage, character(1L))), 1L)

  families <- list(
    list(statistic = "categorical_histogram", variable = "category",
         levels = c("a", "b")),
    list(statistic = "numeric_histogram", variable = "measurement",
         breaks = c(0, 5, 10)),
    list(statistic = "bounded_mean", variable = "measurement",
         lower = 0, upper = 100),
    list(statistic = "binary_rate", variable = "flag", positive = "yes",
         denominator = "all_persons"),
    list(statistic = "binary_rate", variable = "flag", positive = "yes",
         denominator = "nonmissing")
  )
  specs <- unlist(lapply(families, function(spec) {
    lapply(c("first", "last"), function(reducer) {
      c(spec, list(reducer = reducer))
    })
  }), recursive = FALSE)
  required_order <- "Longitudinal first/last reducers require a public order_by column."
  for (spec in specs) {
    messages <- vapply(inputs, function(input) {
      tryCatch({
        omopDpReleaseDS(input, spec)
        NA_character_
      }, error = conditionMessage)
    }, character(1L))
    expect_identical(unname(messages), rep(required_order, length(inputs)),
                     info = paste(spec$statistic, spec$reducer,
                                  spec$denominator))
  }

  for (spec in specs) {
    spec$order_by <- "event_date"
    info <- paste(spec$statistic, spec$reducer, spec$denominator)
    answers <- lapply(inputs, function(input) omopDpReleaseDS(input, spec))
    for (name in names(inputs)) {
      # Private values never change the public shape, and repeats are exact.
      expect_identical(names(answers[[name]]), names(answers$missing),
                       info = info)
      expect_identical(
        serialize(omopDpReleaseDS(inputs[[name]], spec), NULL, version = 3L),
        serialize(answers[[name]], NULL, version = 3L), info = info
      )
    }
  }
  expect_length(.dp_state_files(state), 0L)
  empty_categorical <- .dp_analysis(
    inputs$empty,
    c(families[[1L]], list(reducer = "first", order_by = "event_date"))
  )
  # The person total and every cell of the complete basis (a, b, Other,
  # Unknown) exist and are zero for an empty population: the shape never
  # depends on private values.
  expect_identical(unname(empty_categorical$values), c(0, 0, 0, 0, 0))
})

test_that("public DP requirements reject before private admission or release", {
  .dp_local_state()
  input <- .dp_test_table()
  # This intentionally unsealed schema variant must fail its public type check,
  # without reaching the private provenance/MAC inspection below.
  input$unsupported <- rep(list(1L), nrow(input))
  specs <- list(
    bounded_record_count = list(statistic = "bounded_record_count",
                                reducer = "records"),
    categorical_histogram = list(statistic = "categorical_histogram",
                                  variable = "category", levels = c("a", "b"),
                                  reducer = "presence"),
    numeric_histogram = list(statistic = "numeric_histogram",
                              variable = "measurement", breaks = c(0, 5, 10),
                              reducer = "mean"),
    bounded_distinct = list(statistic = "bounded_distinct",
                            variable = "category", levels = c("a", "b"),
                            reducer = "distinct"),
    bounded_mean = list(statistic = "bounded_mean", variable = "measurement",
                        lower = 0, upper = 100, reducer = "mean"),
    binary_rate = list(statistic = "binary_rate", variable = "flag",
                       positive = "yes", reducer = "any")
  )
  invalid <- function(statistic, ..., error) {
    list(spec = utils::modifyList(specs[[statistic]], list(...)), error = error)
  }
  cases <- list(
    invalid("bounded_record_count", reducer = "presence",
            error = "requires reducer='records'"),
    invalid("bounded_record_count", order_by = "event_date",
            error = "does not use order_by"),
    invalid("categorical_histogram", levels = c("a", ""),
            error = "non-empty UTF-8 labels"),
    # Labels outside the registered domain are refused publicly (B05).
    invalid("categorical_histogram", levels = c("a", "zz_unregistered"),
            error = "registered public domain"),
    invalid("categorical_histogram", reducer = "mean",
            error = "Unsupported categorical longitudinal reducer"),
    invalid("categorical_histogram", order_by = "event_date",
            error = "order_by is only valid"),
    invalid("categorical_histogram", reducer = "records",
            error = "require a public order_by"),
    invalid("categorical_histogram", variable = "unsupported",
            error = "value column has an unsupported type"),
    invalid("numeric_histogram", variable = "category",
            error = "requires a numeric, Date, or UTC datetime column"),
    invalid("numeric_histogram", breaks = c(0, 10, 5),
            error = "strictly increasing"),
    invalid("numeric_histogram", reducer = "presence",
            error = "Unsupported numeric longitudinal reducer"),
    invalid("numeric_histogram", order_by = "event_date",
            error = "order_by is only valid"),
    invalid("bounded_distinct", levels = character(0),
            error = "non-empty UTF-8 labels"),
    invalid("bounded_distinct", levels = "a",
            error = "complete registered public domain"),
    invalid("bounded_distinct", reducer = "presence",
            error = "requires reducer='distinct'"),
    invalid("bounded_distinct", order_by = "event_date",
            error = "canonical value ordering"),
    invalid("bounded_mean", variable = "event_date",
            error = "requires a numeric column"),
    invalid("bounded_mean", lower = -.Machine$double.xmax,
            upper = .Machine$double.xmax, error = "finite positive span"),
    invalid("bounded_mean", reducer = "records",
            error = "Unsupported bounded-mean longitudinal reducer"),
    invalid("bounded_mean", order_by = "event_date",
            error = "order_by is only valid"),
    # Caller-chosen bounds are a public refusal (dsOMOP 2.10.3).
    invalid("bounded_mean", upper = 10, error = "public_scale_unavailable"),
    invalid("binary_rate", positive = character(0),
            error = "positive values must be non-empty"),
    invalid("binary_rate", denominator = "observed",
            error = "denominator must be all_persons or nonmissing"),
    invalid("binary_rate", reducer = "mean",
            error = "Unsupported binary-rate longitudinal reducer"),
    invalid("binary_rate", order_by = "event_date",
            error = "order_by is only valid"),
    invalid("binary_rate", reducer = "first", order_by = "unsupported",
            error = "order_by column has an unsupported type")
  )
  for (statistic in c("categorical_histogram", "numeric_histogram",
                      "bounded_mean", "binary_rate")) {
    cases[[length(cases) + 1L]] <- invalid(
      statistic, reducer = "first", error = "require a public order_by"
    )
  }
  testthat::local_mocked_bindings(
    .dsomopDpValidateInput = function(...) stop("Private input was inspected"),
    .dsomopTypedAnalysis = function(...) stop("The release was reached"),
    .package = "dsOMOP"
  )
  for (case in cases) {
    expect_error(omopDpReleaseDS(input, case$spec), case$error,
                 info = .dsomopDpCanonicalJson(case$spec))
  }
})

test_that("public status and release identify the exact sampler", {
  .dp_local_state(noise_root = as.raw(0:31), release_epsilon = 0.25)
  status <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)

  expect_true(status$enabled)
  expect_true(status$ready)
  expect_true(status$sticky_noise)
  expect_identical(status$protocol, "dsomop-dp-release-v3")
  expect_identical(status$privacy_contract,
                   "protected_release_fixed_basis_or_realised_v1")
  expect_identical(status$release_protocol, "dsomop-protected-release-v1")
  expect_identical(status$release_epsilon, 0.25)
  expect_identical(status$release_delta, 0)
  expect_identical(status$sampler, "dsomop-dp-exact-discrete-laplace-v1")
  expect_identical(status$mechanism, "dsomop-sticky-discrete-laplace-prf-v2")
  expect_identical(.dsomopDpPolicy()$schema_version, 4L)
  expect_identical(status$privacy_call_quota, "none")
  expect_false(status$history_dependent)
  expect_identical(status$persistent_state, "noise_root_only")
  expect_identical(status$release_binding,
                   "fixed_basis_or_realised_contributions_v1")
  expect_identical(status$service_capacity, "none")
  expect_false(status$history_based_admission)
  expect_setequal(status$supported_statistics, c(
    "count", "bounded_record_count", "categorical_histogram",
    "numeric_histogram", "bounded_distinct", "bounded_mean", "binary_rate"
  ))

  value <- omopDpReleaseDS(
    .dp_test_table(), list(statistic = "count", population_id = "cohort-a")
  )
  expect_setequal(names(value), c(
    "protocol", "release_contract", "statistic", "protection",
    "guarantee_scope", "noise_reference_epsilon", "mechanism_epsilon",
    "api_transcript_epsilon", "adjacency", "mechanism", "sampler",
    "sticky", "allocation", "sensitivity", "noise_sd", "privacy_call_quota",
    "history_based_admission", "source", "noisy_count", "components", "read_snapshot"
  ))
  expect_identical(value$protocol, "dsomop-protected-release-v1")
  expect_identical(value$protection, "realised_sticky")
  expect_identical(value$guarantee_scope,
                   "heuristic_realised_contribution_reuse")
  expect_identical(value$noise_reference_epsilon, 0.25)
  # A heuristic realised release claims no mechanism or transcript epsilon.
  expect_true(is.na(value$mechanism_epsilon))
  expect_true(is.na(value$api_transcript_epsilon))
  expect_equal(value$allocation$N, 0.05)
  # No pure-DP delta is claimed for a heuristic release (B14).
  expect_false("delta" %in% names(value))
  expect_identical(value$sticky, TRUE)
  expect_identical(value$sampler, status$sampler)
  expect_identical(value$mechanism, status$mechanism)
})

test_that("new public identities store nothing and have no privacy call quota", {
  skip_on_os("windows")
  state <- .dp_local_state()
  expect_true(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)$ready)
  raw <- .testPseudonymize(data.frame(
    person_id = 1:8, value = seq_len(8), stringsAsFactors = FALSE
  ))
  values <- lapply(seq_len(128L), function(index) {
    input <- .dp_seal(raw, producer = paste0("test/query-", index))
    omopDpReleaseDS(input, list(statistic = "count"))
  })
  expect_length(values, 128L)
  expect_true(all(vapply(
    values, function(value) identical(value$noise_reference_epsilon, 0.1),
    logical(1L)
  )))
  # 128 lineages selecting the same people are one canonical component
  # (dsOMOP 2.10.3, G-09): no fresh draws from lineage cosmetics.
  expect_length(unique(lapply(values, `[[`, "noisy_count")), 1L)
  expect_identical(.dp_state_files(state), .dp_root_files)
  status <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)
  expect_identical(status$privacy_call_quota, "none")
  expect_false(status$history_dependent)
})

test_that("a corrupt root fails closed and only a missing root is created", {
  skip_on_os("windows")
  state <- .dp_local_state()
  first <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)
  root_path <- file.path(state, "secrets", "dp_noise_root")
  original <- readBin(root_path, "raw", n = 32L)

  # Replacing a damaged root would silently re-draw every answer.
  writeBin(as.raw(c(1L, 2L, 3L)), root_path)
  Sys.chmod(root_path, mode = "0600")
  .dp_restart_runtime()
  expect_message(
    expect_error(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL), .DSOMOP_DP_STATE_ERROR, fixed = TRUE),
    "exactly 32 raw bytes"
  )
  expect_identical(readBin(root_path, "raw", n = 32L), as.raw(1:3))

  writeBin(original, root_path)
  Sys.chmod(root_path, mode = "0600")
  .dp_restart_runtime()
  expect_identical(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)$noise_key_id, first$noise_key_id)

  # A state directory without a root is a new service: its first use creates
  # one and tells the custodian.
  expect_identical(unlink(root_path, force = TRUE), 0L)
  .dp_restart_runtime()
  expect_message(status <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL), "created the DP noise root")
  expect_false(identical(status$noise_key_id, first$noise_key_id))
  expect_true(file.exists(root_path))
})

test_that("symbolic-link root substitution fails closed", {
  skip_on_os("windows")
  state <- .dp_local_state()
  expect_true(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)$ready)
  root_path <- file.path(state, "secrets", "dp_noise_root")
  target <- file.path(state, "attacker-controlled-key")
  writeBin(as.raw(0:31), target)
  Sys.chmod(target, mode = "0600")
  expect_identical(unlink(root_path, force = TRUE), 0L)
  expect_true(file.symlink(target, root_path))
  .dp_restart_runtime()

  expect_message(
    expect_error(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL), .DSOMOP_DP_STATE_ERROR, fixed = TRUE),
    "must not be a symbolic link"
  )
  expect_identical(readBin(target, what = "raw", n = 32L), as.raw(0:31))
})

test_that("unsafe root permissions fail closed", {
  skip_on_os("windows")
  state <- .dp_local_state()
  expect_true(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)$ready)
  root_path <- file.path(state, "secrets", "dp_noise_root")
  Sys.chmod(root_path, mode = "0644")
  on.exit(try(Sys.chmod(root_path, mode = "0600"), silent = TRUE), add = TRUE)
  .dp_restart_runtime()

  expect_message(
    expect_error(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL), .DSOMOP_DP_STATE_ERROR, fixed = TRUE),
    "mode 0600"
  )
  expect_identical(file.info(root_path)$size, 32)
})

test_that("an injected root writes no DP state and answers survive restarts", {
  skip_on_os("windows")
  state <- .dp_local_state(noise_root = as.raw(0:31))
  status <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)
  value <- omopDpReleaseDS(
    .dp_test_table(), list(statistic = "count", population_id = "cohort-a")
  )

  expect_true(status$ready)
  expect_identical(status$noise_provider, "custodial_injected")
  expect_identical(value$noise_reference_epsilon, status$release_epsilon)
  expect_length(.dp_state_files(state), 0L)

  .dp_restart_runtime()
  expect_identical(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)$noise_key_id, status$noise_key_id)
  expect_length(.dp_state_files(state), 0L)
  # The display name is not part of the question.
  expect_identical(omopDpReleaseDS(.dp_test_table(), list(statistic = "count")),
                   value)
})

test_that("noise-domain identity detects one root across semantic domains", {
  .dp_local_state(noise_root = as.raw(0:31))
  first <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)

  options(dsomop.dp.domain = "dsomop-dp-other-domain")
  .dp_restart_runtime()
  second <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)

  expect_identical(second$noise_key_id, first$noise_key_id)
  expect_identical(second$noise_domain_id, first$noise_domain_id)
})

test_that("a replaced injected root re-draws answers and the original restores them", {
  skip_on_os("windows")
  state <- .dp_local_state(noise_root = as.raw(0:31))
  table <- .dp_test_table()
  spec <- list(statistic = "count", population_id = "cohort-a")
  first_value <- omopDpReleaseDS(table, spec)
  first_status <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)

  options(dsomop.dp.noise_root = as.raw(32:63))
  .dp_restart_runtime()
  expect_false(identical(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)$noise_key_id,
                         first_status$noise_key_id))
  # Provenance is keyed by the root as well: old capsules are not admitted.
  expect_error(omopDpReleaseDS(table, spec), "provenance MAC does not match")
  expect_length(.dp_state_files(state), 0L)

  options(dsomop.dp.noise_root = as.raw(0:31))
  .dp_restart_runtime()
  expect_identical(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)$noise_key_id, first_status$noise_key_id)
  expect_identical(omopDpReleaseDS(table, spec), first_value)
})

test_that("concurrent releases agree without any coordination", {
  skip_on_os("windows")
  state <- .dp_local_state()
  table <- .dp_test_table()
  changed <- table
  people <- unique(changed$person_id)
  changed$person_id[changed$person_id == people[[3L]]] <- people[[2L]]
  changed <- .dp_seal(changed, producer = "test/fixture")
  expect_identical(.dp_lineage(table), .dp_lineage(changed))
  spec <- list(statistic = "count", population_id = "cohort-a")
  values <- parallel::mclapply(
    1:4,
    function(index) omopDpReleaseDS(if (index %% 2L) table else changed, spec),
    mc.cores = 2L,
    mc.preschedule = FALSE
  )
  expect_false(any(vapply(values, inherits, logical(1L), "try-error")))
  expect_identical(values[[3L]], values[[1L]])
  expect_identical(values[[4L]], values[[2L]])
  expect_identical(omopDpReleaseDS(table, spec), values[[1L]])
  expect_identical(omopDpReleaseDS(changed, spec), values[[2L]])
  expect_identical(.dp_state_files(state), .dp_root_files)
})

test_that("onLoad runs no self-tests, reads no configuration and never fails", {
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
      stop("a failing self-test must not break loading or installation")
    },
    .dsomopPseudonymLifecycleSettings = function() {
      events <<- c(events, "pseudonym-settings")
      stop("malformed settings must not break loading or installation")
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
  expect_identical(events, character(0))
  expect_false(.pkg_state$dp_status$ready)

  runtime_library <- withr::local_tempdir(pattern = "dsomop-runtime-lib-")
  expect_silent(.onLoad(runtime_library, "dsOMOP"))
  expect_identical(events, character(0))
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
  expect_message(first <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL), "created the DP noise root")
  expect_true(file.exists(root_path))
  expect_true(first$ready)
  expect_match(first$noise_domain_id, "^dpn_[0-9a-f]{40}$")

  .dp_restart_runtime()
  expect_silent(restarted <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL))
  expect_identical(restarted$noise_domain_id, first$noise_domain_id)
  expect_identical(restarted$noise_key_id, first$noise_key_id)
})

test_that("concurrent runtime use converges on the provisioned noise identity", {
  skip_on_os("windows")
  state <- .dp_local_state()
  statuses <- parallel::mclapply(
    seq_len(4L), function(unused) omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL),
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

  parent <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)
  expect_identical(parent$noise_domain_id, identities[[1L]])
})

test_that("the central policy guard initializes status and empty nodes differ", {
  first_state <- .dp_local_state()
  root_path <- file.path(first_state, "secrets", "dp_noise_root")
  expect_true(file.exists(root_path))
  expect_null(.pkg_state$dp_runtime)
  expect_silent(.dsomopDpPolicy())
  expect_true(.pkg_state$dp_status$ready)
  first_id <- .pkg_state$dp_status$noise_domain_id
  expect_match(first_id, "^dpn_[0-9a-f]{40}$")
  expect_true(file.exists(root_path))

  second_state <- .dp_local_state()
  second_id <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)$noise_domain_id
  expect_false(identical(second_id, first_id))
})

test_that("runtime rejects late enablement changes", {
  .dp_local_state(enabled = FALSE)
  expect_silent(.dsomopDpEnsureRuntime())
  withr::local_options(list(dsomop.dp.enabled = TRUE))
  expect_error(.dsomopDpEnabled(), "changed during this R session")
})

.dp_derived_handle <- function(database = "warehouse", source = TRUE) {
  handle <- if (source) create_test_handle() else create_test_handle_no_source()
  handle$resource_client <- list(getParsed = function() list(
    dbms = "postgresql", host = "DB.EXAMPLE", port = 5432L,
    database = database, user = "ignored", password = "ignored"
  ), getConnection = function() handle$conn)
  if (source) {
    DBI::dbExecute(handle$conn, "ALTER TABLE cdm_source ADD COLUMN cdm_release_date TEXT")
    DBI::dbExecute(handle$conn, "UPDATE cdm_source SET cdm_release_date = '2026-08-01'")
  }
  handle
}

test_that("DP defaults on with option and environment opt-outs intact", {
  .dp_local_state(enabled = NULL)
  withr::local_options(list(default.dsomop.dp.enabled = NULL))
  expect_true(.dsomopDpEnabled())
  withr::with_options(list(dsomop.dp.enabled = FALSE), {
    expect_false(.dsomopDpEnabled())
  })
  withr::with_envvar(c(DSOMOP_DP_ENABLED = "0"), {
    expect_false(.dsomopDpEnabled())
  })
})

test_that("derived resource policies separate keys and reject live changes", {
  .dp_local_state()
  withr::local_options(list(dsomop.dp.domain = "", dsomop.dp.snapshot_id = ""))
  first <- .dp_derived_handle("one")
  second <- .dp_derived_handle("two")
  on.exit({cleanup_handle(first); cleanup_handle(second)}, add = TRUE)
  expect_silent(.dsomopDpEnsureRuntime(first))
  policy <- .dsomopDpPolicy()
  expect_match(policy$domain, "^resource_[0-9a-f]{64}$")
  expect_match(policy$snapshot_id, "^cdm_[0-9a-f]{64}$")
  expect_error(.dsomopDpEnsureRuntime(second), "policy changed")
  expect_identical(.dsomopDpPolicy()$policy_hash, policy$policy_hash)
  .dp_restart_runtime()
  .dsomopDpEnsureRuntime(second)
  other <- .dsomopDpPolicy()
  expect_false(identical(policy$domain, other$domain))
  expect_false(identical(policy$keys$provenance, other$keys$provenance))
  for (key in c("person", "fingerprint", "noise")) {
    expect_false(identical(.dsomopReleaseKeys(policy)[[key]],
                           .dsomopReleaseKeys(other)[[key]]), info = key)
  }
  # A changed source label is display metadata only (review R01): no policy
  # change, no refusal.
  DBI::dbExecute(second$conn, "UPDATE cdm_source SET vocabulary_version = 'next'")
  expect_identical(.dsomopDpPolicy()$policy_hash, other$policy_hash)
})

test_that("explicit identifiers take precedence over fallback metadata", {
  .dp_local_state()
  handle <- .dp_derived_handle(source = FALSE)
  on.exit(cleanup_handle(handle), add = TRUE)
  expect_silent(.dsomopDpEnsureRuntime(handle))
  expect_identical(.dsomopDpPolicy()$domain, "dsomop-dp-test")
  expect_identical(.dsomopDpPolicy()$snapshot_id, "etl-2026-08-01")
  .dp_restart_runtime()
  withr::local_options(list(dsomop.dp.domain = "", dsomop.dp.snapshot_id = ""))
  expect_silent(.dsomopDpEnsureRuntime(handle))
  expect_true(.dsomopDpPolicy()$enabled)
})

test_that("derived identifiers respect partial overrides and conflicts", {
  .dp_local_state()
  handle <- .dp_derived_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  withr::local_options(list(dsomop.dp.snapshot_id = ""))
  .dsomopDpEnsureRuntime(handle)
  expect_identical(.dsomopDpPolicy()$domain, "dsomop-dp-test")
  expect_identical(.dsomopDpPolicy()$snapshot_id,
                   paste0("cdm_", .dsomopDpSha256(.dsomopFrame(
                     "dsomop-dp-snapshot-label-v2", "dsomop-dp-test"))))
  withr::with_envvar(c(DSOMOP_DP_DOMAIN = "conflict"), {
    expect_error(.dsomopDpPolicy(), "Conflicting DP option")
  })
  # A conflicting or malformed label is ignored (display metadata only): it
  # can never refuse the service (review R01).
  withr::with_envvar(c(DSOMOP_DP_SNAPSHOT_ID = "conflict"), {
    expect_no_error(policy <- .dsomopDpPolicy())
    expect_identical(policy$snapshot_id,
                     paste0("cdm_", .dsomopDpSha256(.dsomopFrame(
                     "dsomop-dp-snapshot-label-v2", "dsomop-dp-test"))))
  })
  withr::with_options(list(dsomop.dp.snapshot_id = "not a label!"), {
    expect_no_error(.dsomopDpPolicy())
  })
})

test_that("DSLite sessions isolate derived policies under a shared persistent root", {
  .dp_local_state()
  withr::local_options(list(dsomop.dp.domain = "", dsomop.dp.snapshot_id = ""))
  handles <- list(.dp_derived_handle("one"), .dp_derived_handle("two"))
  on.exit(lapply(handles, cleanup_handle), add = TRUE)
  old_sessions <- .pkg_state$dp_sessions
  on.exit(.pkg_state$dp_sessions <- old_sessions, add = TRUE)
  .pkg_state$dp_sessions <- NULL
  sessions <- lapply(1:2, function(i) {
    env <- new.env()
    attr(env, "name") <- sprintf("DSLiteEnv_%04d", i)
    env$handle <- handles[[i]]
    env
  })
  initialize <- function(env) evalq({
    .dsomopDpEnsureRuntime(handle)
    .dsomopDpPolicy()
  }, env)
  first <- initialize(sessions[[1]])
  second <- initialize(sessions[[2]])
  expect_false(identical(first$domain, second$domain))
  expect_identical(first$noise_root$key_id, second$noise_root$key_id)
  expect_false(identical(.dsomopReleaseKeys(first)$noise,
                         .dsomopReleaseKeys(second)$noise))
  expect_false(identical(first$keys$provenance, second$keys$provenance))
  expect_identical(initialize(sessions[[1]])$policy_hash, first$policy_hash)
})

test_that("canonical coordinates ignore credentials and normalize default ports", {
  .dp_local_state()
  handle <- .dp_derived_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  original <- .dsomopDpDerivedIdentity(handle)
  handle$resource_client$getParsed <- function() list(
    dbms = "postgresql", host = "db.example", database = "warehouse",
    password = "changed", user = "different", sslmode = "verify-full"
  )
  expect_identical(.dsomopDpDerivedIdentity(handle), original)
  handle$vocab_schema <- "different_dataset"
  expect_false(identical(.dsomopDpDerivedIdentity(handle)$domain,
                         original$domain))
  handle$vocab_schema <- NULL
  DBI::dbExecute(handle$conn, "UPDATE cdm_source SET cdm_release_date = NULL")
  expect_silent(fallback <- .dsomopDpDerivedIdentity(handle))
  expect_identical(fallback$domain, original$domain)
  expect_false(identical(fallback$snapshot_id, original$snapshot_id))
})

test_that("default-on initialization derives identity from a real SQLite resource", {
  .dp_local_state(enabled = NULL)
  withr::local_options(list(default.dsomop.dp.enabled = NULL,
                            dsomop.dp.domain = "", dsomop.dp.snapshot_id = ""))
  database <- withr::local_tempfile(fileext = ".sqlite")
  conn <- create_test_omop_db(database)
  DBI::dbExecute(conn, "ALTER TABLE cdm_source ADD COLUMN cdm_release_date TEXT")
  DBI::dbExecute(conn, "UPDATE cdm_source SET cdm_release_date = '2026-08-01'")
  DBI::dbDisconnect(conn)
  resource <- resourcer::newResource(
    name = "default_dp", url = paste0("omop+dbi:sqlite://", database),
    format = "omop"
  )
  session <- new.env()
  session$default_dp <- OMOPResourceClient$new(resource)
  evalq(omopInitDS("default_dp"), session)
  handle <- session$.dsomop_handle_default_dp
  on.exit(.closeHandle(handle), add = TRUE)
  # Connecting never initializes DP; a community tool binds it when used.
  expect_null(.pkg_state$dp_runtime)
  .dsomopDpEnsureRuntime(handle)
  expect_true(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)$enabled)
  expect_true(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)$ready)
  expect_match(.dsomopDpPolicy()$domain, "^resource_")
  expect_gt(nrow(omopQueryLibraryStickyCatalogDS()), 0L)
})

.dp_expected_snapshot <- function(identity, protocol, metadata) {
  paste0("cdm_", .dsomopDpSha256(.dsomopDpCanonicalJson(list(
    protocol = protocol, resource = sub("^resource_", "", identity$domain),
    metadata = metadata
  ))))
}

test_that("complete source rows retain single-row hashes and sort all multiple rows", {
  .dp_local_state()
  handle <- .dp_derived_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  row <- DBI::dbGetQuery(handle$conn, "SELECT * FROM cdm_source")
  fields <- c("cdm_source_name", "cdm_release_date", "cdm_version", "vocabulary_version")
  metadata <- lapply(row[fields], as.character)
  single <- .dsomopDpDerivedIdentity(handle)
  expect_identical(single$snapshot_id, .dp_expected_snapshot(
    single, "dsomop-dp-cdm-snapshot-v1", metadata))
  other <- row
  other$cdm_source_name <- "Another source"
  DBI::dbAppendTable(handle$conn, "cdm_source", other)
  rows <- list(metadata, lapply(other[fields], as.character))
  rows <- rows[order(vapply(rows, .dsomopDpCanonicalJson, character(1)), method = "radix")]
  multi <- .dsomopDpDerivedIdentity(handle)
  expect_identical(multi$snapshot_id, .dp_expected_snapshot(
    multi, "dsomop-dp-cdm-snapshot-multi-v1", rows))
  DBI::dbExecute(handle$conn, "DELETE FROM cdm_source")
  DBI::dbAppendTable(handle$conn, "cdm_source", rbind(other, row))
  expect_identical(.dsomopDpDerivedIdentity(handle), multi)
})

test_that("fallback sorts vocabulary pairs across resources and restarts", {
  .dp_local_state()
  withr::local_options(list(dsomop.dp.domain = "", dsomop.dp.snapshot_id = ""))
  handles <- list(.dp_derived_handle("one", source = FALSE),
                  .dp_derived_handle("two", source = FALSE))
  on.exit(lapply(handles, cleanup_handle), add = TRUE)
  vocabulary <- data.frame(vocabulary_id = c("Z", "None"),
                           vocabulary_version = c("2026", "v5"))
  for (handle in handles) DBI::dbWriteTable(handle$conn, "vocabulary", vocabulary, overwrite = TRUE)
  expect_silent(.dsomopDpEnsureRuntime(handles[[1]]))
  first <- .dsomopDpDerivedIdentity(handles[[1]])
  expect_identical(first$snapshot_id, .dp_expected_snapshot(first,
    "dsomop-dp-cdm-snapshot-fallback-v1", list(
      list(vocabulary_id = "None", vocabulary_version = "v5"),
      list(vocabulary_id = "Z", vocabulary_version = "2026"))))
  DBI::dbWriteTable(handles[[1]]$conn, "vocabulary", vocabulary[2:1, ], overwrite = TRUE)
  expect_identical(.dsomopDpDerivedIdentity(handles[[1]]), first)
  .dp_restart_runtime()
  expect_silent(.dsomopDpEnsureRuntime(handles[[2]]))
  expect_false(identical(.dsomopDpPolicy()$snapshot_id, first$snapshot_id))
  # The label is display metadata (dsOMOP 2.10.3, review R01): a changed
  # derived label within a session changes no policy and no admission.
  before <- .dsomopDpPolicy()
  DBI::dbExecute(handles[[2]]$conn, "UPDATE vocabulary SET vocabulary_version = 'next'")
  expect_no_error(after <- .dsomopDpPolicy())
  expect_identical(after$policy_hash, before$policy_hash)
  expect_identical(after$snapshot_id, before$snapshot_id)
})

test_that("empty incomplete and blank source metadata use resource-only fallback without vocabulary", {
  .dp_local_state()
  handle <- .dp_derived_handle()
  on.exit(cleanup_handle(handle), add = TRUE)
  DBI::dbExecute(handle$conn, "DROP TABLE IF EXISTS vocabulary")
  DBI::dbExecute(handle$conn, "UPDATE cdm_source SET cdm_release_date = ' '")
  expect_silent(identity <- .dsomopDpDerivedIdentity(handle))
  expect_identical(identity$snapshot_id, .dp_expected_snapshot(identity,
    "dsomop-dp-cdm-snapshot-resource-only-v1", NULL))
  DBI::dbExecute(handle$conn, "DELETE FROM cdm_source")
  expect_silent(expect_identical(.dsomopDpDerivedIdentity(handle), identity))
  DBI::dbExecute(handle$conn, "DROP TABLE cdm_source")
  DBI::dbWriteTable(handle$conn, "cdm_source", data.frame(cdm_source_name = "incomplete"))
  expect_silent(expect_identical(.dsomopDpDerivedIdentity(handle), identity))
  withr::local_options(list(dsomop.dp.domain = ""))
  expect_silent(.dsomopDpEnsureRuntime(handle))
  expect_identical(.dsomopDpPolicy()$snapshot_id, "etl-2026-08-01")
  handle$resource_client$getParsed <- function() list(dbms = "sqlite", database = ":memory:")
  expect_error(.dsomopDpDerivedIdentity(handle), "configure dsomop.dp.domain")
})

test_that("DP status reports configured exclusivity in enabled and disabled modes", {
  check_status <- function(enabled) {
    .dp_local_state(enabled = enabled, noise_root = as.raw(0:31))
    withr::local_options(list(
      dsomop.dp.exclusive = NULL,
      default.dsomop.dp.exclusive = NULL
    ))
    expect_identical(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)$exclusive, FALSE)

    for (exclusive in c(TRUE, FALSE)) {
      withr::with_options(list(dsomop.dp.exclusive = exclusive), {
        status <- omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)
        expect_identical(status$enabled, enabled)
        expect_identical(status$exclusive, exclusive)
        expect_identical(.pkg_state$dp_status$exclusive, exclusive)
      })
    }
  }
  check_status(TRUE)
  check_status(FALSE)
})

test_that("exclusive mode keeps only the fixed-synopsis typed releases", {
  .dp_local_state(noise_root = as.raw(0:31))
  withr::local_options(list(dsomop.dp.exclusive = FALSE))
  input <- .dp_test_table()
  specs <- list(
    list(statistic = "count"),
    list(statistic = "bounded_record_count", reducer = "records",
         max_contributions = 2L),
    list(statistic = "categorical_histogram", variable = "category",
         levels = c("a", "b", "c"), reducer = "presence",
         max_contributions = 2L),
    list(statistic = "numeric_histogram", variable = "measurement",
         breaks = c(0, 5, 10), reducer = "records",
         max_contributions = 2L),
    list(statistic = "bounded_distinct", variable = "category",
         levels = c("a", "b", "c"), reducer = "distinct",
         max_contributions = 2L),
    list(statistic = "bounded_mean", variable = "measurement",
         lower = 0, upper = 100, reducer = "mean"),
    list(statistic = "binary_rate", variable = "flag", positive = "yes",
         reducer = "any", denominator = "all_persons")
  )
  release_all <- function(x, which = seq_along(specs)) {
    lapply(specs[which], function(spec) {
      suppressMessages(omopDpReleaseDS(x, spec))
    })
  }
  standard_mode <- release_all(input)
  expect_setequal(vapply(standard_mode, `[[`, character(1L), "statistic"),
                  omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)$supported_statistics)
  expect_true(all(vapply(standard_mode, `[[`, character(1L), "protection") ==
                    "realised_sticky"))
  # Counts, capped record counts and registered-scale means of an unscoped
  # pinned loader table are fixed synopses (keyed on the person-membership
  # generation its loader request sealed with it).
  dataset <- .release_session_dataset()
  loader <- .dp_test_table(producer = "query/measurement.load/0123456789abcdef",
                           dataset_identity = dataset,
                           data_generation = .release_loader_generation())
  fixed <- release_all(loader, c(1L, 2L, 6L))
  expect_true(all(vapply(fixed, `[[`, character(1L), "protection") ==
                    "fixed_synopsis"))

  withr::with_options(list(dsomop.dp.exclusive = TRUE), {
    expect_true(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)$exclusive)
    expect_error(omopAchillesResultsDS("unused", 1L), "DP-only mode")
    # A heuristic realised release is never called DP: refused statically.
    for (spec in specs) {
      expect_error(suppressMessages(omopDpReleaseDS(input, spec)),
                   "no fixed-synopsis protection", info = spec$statistic)
    }
    expect_identical(release_all(loader, c(1L, 2L, 6L)), fixed)
    # DP input can also be prepared after the exclusive gate is activated.
    expect_identical(.dp_test_table(), input)
  })
  expect_false(omopDpStatusDS(.DSOMOP_RELEASE_PROTOCOL)$exclusive)
  expect_identical(release_all(input), standard_mode)
})

.dp_primitive_specs <- function() list(
  list(statistic = "count"),
  list(statistic = "bounded_record_count", reducer = "records",
       max_contributions = 2L),
  list(statistic = "categorical_histogram", variable = "category",
       levels = c("a", "b", "c"), reducer = "presence",
       max_contributions = 2L),
  list(statistic = "numeric_histogram", variable = "measurement",
       breaks = c(0, 5, 10), reducer = "records", max_contributions = 2L),
  list(statistic = "bounded_distinct", variable = "category",
       levels = c("a", "b", "c"), reducer = "distinct",
       max_contributions = 2L),
  list(statistic = "bounded_mean", variable = "measurement",
       lower = 0, upper = 100, reducer = "mean"),
  list(statistic = "binary_rate", variable = "flag", positive = "yes",
       reducer = "any", denominator = "all_persons")
)

.dp_primitive_input <- function(changed_persons = 0L, reverse = FALSE,
                                   producer = "test/primitive-matrix",
                                   dataset_identity = .dp_dataset_identity(),
                                   data_generation = NULL) {
  raw <- data.frame(
    person_id = 1:8, category = rep(c("a", "b"), 4L),
    measurement = 1:8, flag = rep(c("yes", "no"), 4L),
    stringsAsFactors = FALSE
  )
  raw <- rbind(raw, data.frame(
    person_id = 8L + seq_len(changed_persons),
    category = rep("c", changed_persons),
    measurement = rep(9, changed_persons),
    flag = rep("yes", changed_persons), stringsAsFactors = FALSE
  ))
  if (reverse) raw <- raw[rev(seq_len(nrow(raw))), , drop = FALSE]
  .dp_seal(.testPseudonymize(raw), producer = producer, domains = .dp_domains,
           dataset_identity = dataset_identity,
           data_generation = data_generation)
}

test_that("all seven primitives are byte-deterministic and data edits re-draw them", {
  .dp_local_state(noise_root = as.raw(0:31))
  first_input <- .dp_primitive_input()
  specs <- .dp_primitive_specs()
  first <- lapply(specs, function(spec) omopDpReleaseDS(first_input, spec))
  first_bytes <- lapply(first, serialize, connection = NULL, version = 3L)
  expect_setequal(vapply(first, `[[`, character(1L), "statistic"),
                  .DSOMOP_DP_STATISTICS)
  # Row order is not part of the protected statistic.
  reversed <- .dp_primitive_input(reverse = TRUE)
  expect_identical(lapply(specs, function(spec) {
    serialize(omopDpReleaseDS(reversed, spec), NULL, version = 3L)
  }), first_bytes)

  redrawn <- logical(0)
  for (changed_persons in c(1L, 2L, 3L, 64L)) {
    # Resealing is a trusted custodian operation. Ordinary edited or reordered
    # capsules remain invalid; no MAC check is relaxed to permit these inputs.
    input <- .dp_primitive_input(changed_persons, reverse = TRUE)
    expect_identical(.dp_lineage(input), .dp_lineage(first_input))
    expect_identical(length(unique(input$person_id)), 8L + changed_persons)
    for (index in seq_along(specs)) {
      value <- omopDpReleaseDS(input, specs[[index]])
      expect_identical(serialize(omopDpReleaseDS(input, specs[[index]]), NULL,
                                 version = 3L),
                       serialize(value, NULL, version = 3L))
      redrawn <- c(redrawn, !identical(value, first[[index]]))
    }
  }
  # A changed statistic gets fresh noise (one cell can coincide by chance).
  expect_gt(mean(redrawn), 0.5)

  .dp_restart_runtime()
  after_restart <- lapply(specs, function(spec) {
    omopDpReleaseDS(.dp_primitive_input(), spec)
  })
  expect_identical(lapply(after_restart, serialize, connection = NULL,
                          version = 3L), first_bytes)
})

test_that("data and epoch changes redraw; lineage and snapshot cosmetics do not", {
  .dp_local_state(noise_root = as.raw(0:31))
  count <- list(statistic = "count")
  release <- function(changed_persons = 0L,
                      producer = "test/primitive-matrix") {
    input <- .dp_primitive_input(changed_persons, producer = producer)
    suppressMessages(omopDpReleaseDS(input, count))$components$signed
  }
  dataset <- .release_session_dataset()
  fixed <- function() {
    input <- .dp_primitive_input(
      dataset_identity = dataset,
      producer = "query/measurement.load/0123456789abcdef",
      data_generation = .release_loader_generation())
    out <- suppressMessages(omopDpReleaseDS(input, count))
    expect_identical(out$protection, "fixed_synopsis")
    out$components$signed
  }
  first <- release()
  first_fixed <- fixed()
  # A repeat recomputes the same answer.
  expect_identical(release(), first)
  expect_identical(fixed(), first_fixed)
  # A changed realised person set is another contribution map.
  expect_false(identical(release(64L), first))
  # Another lineage selecting the same people is the same canonical component
  # (dsOMOP 2.10.3, G-09).
  expect_identical(release(producer = "test/independent-public-request"),
                   first)
  # The deprecated snapshot label keys nothing (dsOMOP 2.10.3): fixed
  # synopses use the person-membership generation sealed with the table.
  options(dsomop.dp.snapshot_id = "etl-2026-08-02")
  .dp_restart_runtime()
  expect_identical(release(), first)
  expect_identical(fixed(), first_fixed)
  # Rotation of the privacy epoch is a public custodian decision that
  # redraws every component, even with unchanged data.
  options(dsomop.dp.snapshot_id = "etl-2026-08-01",
          dsomop.dp.privacy_epoch = 2L)
  .dp_restart_runtime()
  expect_false(identical(release(), first))
  expect_false(identical(fixed(), first_fixed))
  # The original configuration recomputes the original answers.
  options(dsomop.dp.privacy_epoch = 1L)
  .dp_restart_runtime()
  expect_identical(release(), first)
  expect_identical(fixed(), first_fixed)
})

test_that("a fresh R process recomputes the canonical complete primitive response", {
  skip_on_os("windows")
  state <- .dp_local_state(noise_root = as.raw(0:31))
  specs <- .dp_primitive_specs()
  first <- lapply(specs, function(spec) {
    omopDpReleaseDS(.dp_primitive_input(), spec)
  })
  work <- withr::local_tempdir(pattern = "dsomop-seed-only-restart-")
  configuration <- options()[grep("^dsomop\\.", names(options()))]
  saveRDS(list(options = configuration,
               input = .dp_primitive_input(reverse = TRUE), specs = specs),
          file.path(work, "request.rds"))
  package_root <- normalizePath(testthat::test_path("..", ".."),
                                winslash = "/", mustWork = TRUE)
  script <- file.path(work, "worker.R")
  writeLines(c(
    "args <- commandArgs(TRUE)",
    "request <- readRDS(file.path(args[[1L]], 'request.rds'))",
    "options(request$options)",
    "if (file.exists(file.path(args[[2L]], 'R', 'privacy_state.R'))) {",
    "  suppressMessages(pkgload::load_all(args[[2L]], quiet = TRUE))",
    "} else suppressPackageStartupMessages(library(dsOMOP))",
    "result <- lapply(request$specs, function(spec) {",
    "  dsOMOP::omopDpReleaseDS(request$input, spec)",
    "})",
    "saveRDS(lapply(result, serialize, connection = NULL, version = 3L),",
    "        file.path(args[[1L]], 'result.rds'))"
  ), script)
  output <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script), shQuote(work), shQuote(package_root)),
    stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  # Nothing was stored: the new process derived the same bytes.
  expect_identical(readRDS(file.path(work, "result.rds")),
                   lapply(first, serialize, connection = NULL, version = 3L))
  expect_length(.dp_state_files(state), 0L)
})

test_that("enabling DP reuses a root created while DP was disabled", {
  state <- .dp_local_state(enabled = FALSE)
  expect_message(old_root <- .dsomopDpNoiseRoot(), "created the DP noise root")
  withr::local_options(dsomop.dp.enabled = TRUE)
  expect_silent(policy <- .dsomopDpPolicy())
  expect_identical(policy$noise_root, old_root)
  expect_true(.dsomopDpPublicStatus()$ready)
  .dp_restart_runtime()
  expect_identical(.dsomopDpPolicy()$noise_root, old_root)
  expect_identical(.dp_state_files(state), .dp_root_files)
})
