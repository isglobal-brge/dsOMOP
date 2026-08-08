test_staged_output <- function(format = "long") {
  list(
    type = "event_level",
    representation = list(format = format)
  )
}

test_staged_plan <- function(output = test_staged_output(),
                             output_name = "analysis") {
  outputs <- list(output)
  names(outputs) <- output_name
  list(outputs = outputs)
}

test_staged_semantics <- function(format = "long", component = NULL,
                                  output_name = "analysis") {
  .stagedSemanticContract(
    test_staged_plan(test_staged_output(format), output_name),
    output_name, component = component
  )
}

test_staged_bundle <- function(output_name, token, format = "long") {
  .stagedBundleContract(
    test_staged_plan(test_staged_output(format), output_name),
    output_name, token
  )
}

test_that("staged semantics bind cohort and selected population definitions", {
  digest <- function(plan) {
    .stagedSemanticContract(plan, "analysis")$query_semantics_sha256
  }

  cohort_7 <- test_staged_plan()
  cohort_7$cohort <- list(
    type = "cohort_table", cohort_definition_id = 7L
  )
  cohort_8 <- cohort_7
  cohort_8$cohort$cohort_definition_id <- 8L
  expect_false(identical(digest(cohort_7), digest(cohort_8)))

  population_7 <- test_staged_plan()
  population_7$outputs$analysis$population_id <- "selected"
  population_7$populations <- list(
    base = list(kind = "criteria"),
    selected = list(kind = "criteria", cohort_definition_id = 7L)
  )
  population_8 <- population_7
  population_8$populations$selected$cohort_definition_id <- 8L
  expect_false(identical(digest(population_7), digest(population_8)))

  filtered <- population_7
  filtered$populations$selected$cohort_definition_id <- NULL
  filtered$populations$selected$filter_tree <- list(
    type = "sex", params = list(value = "F")
  )
  filtered_other <- filtered
  filtered_other$populations$selected$filter_tree$params$value <- "M"
  expect_false(identical(digest(filtered), digest(filtered_other)))

  indexed <- population_7
  indexed$populations$selected$cohort_definition_id <- NULL
  indexed$populations$selected$index_event <- list(
    table = "condition_occurrence", concept_set = 201820L,
    primary_limit = "first"
  )
  indexed_other <- indexed
  indexed_other$populations$selected$index_event$primary_limit <- "last"
  expect_false(identical(digest(indexed), digest(indexed_other)))
})

test_that("staged scope is portable, data-independent and fail-closed", {
  frame_a <- data.frame(person_id = 1:3)
  class(frame_a) <- c("omop.table", "data.frame")
  attr(frame_a, "site_lineage") <- "site-a"
  frame_b <- data.frame(person_id = 101:103, extra = 1L)
  class(frame_b) <- c("omop.table", "data.frame")
  attr(frame_b, "site_lineage") <- "site-b"

  first <- test_staged_plan()
  first$scope <- list(
    cohort = 42L,
    tables = "eligible_people",
    combine = "intersect",
    tables_frames = list(42L, frame_a)
  )
  second <- first
  second$scope$tables_frames <- list(42L, frame_b)

  expect_silent(.validateStagedScopeDeclaration(first))
  expect_silent(.validateStagedScopeDeclaration(second))
  expect_identical(
    .stagedSemanticContract(first, "analysis")$query_semantics_sha256,
    .stagedSemanticContract(second, "analysis")$query_semantics_sha256
  )

  changed_id <- first
  changed_id$scope$cohort <- 43L
  changed_id$scope$tables_frames[[1L]] <- 43L
  expect_false(identical(
    .stagedSemanticContract(first, "analysis")$query_semantics_sha256,
    .stagedSemanticContract(changed_id, "analysis")$query_semantics_sha256
  ))
  changed_combine <- first
  changed_combine$scope$combine <- "union"
  expect_false(identical(
    .stagedSemanticContract(first, "analysis")$query_semantics_sha256,
    .stagedSemanticContract(changed_combine,
                            "analysis")$query_semantics_sha256
  ))
  changed_symbol <- first
  changed_symbol$scope$tables <- "other_eligible_people"
  expect_false(identical(
    .stagedSemanticContract(first, "analysis")$query_semantics_sha256,
    .stagedSemanticContract(changed_symbol,
                            "analysis")$query_semantics_sha256
  ))

  undeclared <- test_staged_plan()
  undeclared$scope <- list(tables_frames = frame_a, combine = "union")
  expect_error(
    .validateStagedScopeDeclaration(undeclared),
    "do not match the portable"
  )
  unresolved <- test_staged_plan()
  unresolved$scope <- list(tables = "eligible_people", combine = "union")
  expect_error(
    .validateStagedScopeDeclaration(unresolved),
    "were not resolved"
  )
})

test_that("multi-state staged contracts preserve graph and component grain", {
  output <- list(
    type = "survival",
    outcomes = list(mi = list(
      table = "condition_occurrence", concept_set = 316866L
    )),
    format = "multi_state",
    event_order = "all",
    tie_policy = "priority",
    transitions = list(index = "mi", mi = character(0)),
    initial_state = "index"
  )
  plan <- list(outputs = list(course = output))
  main <- .stagedSemanticContract(plan, "course")
  msdata <- .stagedSemanticContract(plan, "course", component = "msdata")
  reference <- .stagedSemanticContract(
    plan, "course", component = "transition_ref"
  )

  expect_identical(main$output_format, "long")
  expect_identical(main$grain, "episode_transition")
  expect_identical(msdata$output_format, "long")
  expect_identical(msdata$grain, "episode_transition")
  expect_identical(msdata$date_handling$unit, "calendar_day")
  expect_identical(reference$output_format, "reference")
  expect_identical(reference$grain, "state_transition")
  expect_silent(.validateStagedSemanticContract(msdata))
  expect_silent(.validateStagedSemanticContract(reference))

  sequential <- plan
  sequential$outputs$course$tie_policy <- "sequential"
  sequential$outputs$course$state_step <- 0.01
  sequential_msdata <- .stagedSemanticContract(
    sequential, "course", component = "msdata"
  )
  expect_identical(
    sequential_msdata$date_handling$unit,
    "calendar_day_with_public_within_day_offsets"
  )
  expect_silent(.validateStagedSemanticContract(sequential_msdata))
  invalid_unit <- sequential_msdata
  invalid_unit$date_handling$unit <- c("calendar_day", "hour")
  expect_error(.validateStagedSemanticContract(invalid_unit),
               "date_handling unit")

  changed <- plan
  changed$outputs$course$state_hierarchy <- c("mi", "index")
  expect_false(identical(
    main$query_semantics_sha256,
    .stagedSemanticContract(changed, "course")$query_semantics_sha256
  ))
})

test_that("staged descriptors publish and validate token compatibility", {
  base <- tempfile("dsomop_staging_contract_")
  withr::local_options(list(
    dsstaging.base_dir = base,
    dsstaging.ttl_hours = 24
  ))
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  token <- .generateStagingToken()
  directory <- .createStagingDir(token)
  key <- openssl::rand_bytes(32L)
  contract <- list(
    available = TRUE,
    contract_version = 1L,
    provider = "file",
    key_id = .personKeyId(key),
    epoch = 3L
  )
  descriptor <- .stageDataFrame(
    data.frame(person_id = 1:3, value = c(2, 4, 8)),
    "analysis", directory, token, key,
    pseudonymization = contract,
    semantic_contract = test_staged_semantics(),
    bundle_contract = test_staged_bundle("analysis", token)
  )

  expect_error(
    .stageDataFrame(
      data.frame(person_id = 1:3), "missing_semantics", directory, token, key,
      pseudonymization = contract
    ),
    "requires a semantic contract"
  )
  expect_false(any(grepl("missing_semantics", list.files(directory))))

  expect_error(
    .stageDataFrame(
      data.frame(person_id = 1:3), "missing_bundle", directory, token, key,
      pseudonymization = contract,
      semantic_contract = test_staged_semantics()
    ),
    "requires a bundle contract"
  )
  expect_false(any(grepl("missing_bundle", list.files(directory))))

  expect_s3_class(descriptor, "FlowerDatasetDescriptor")
  expect_true(inherits(descriptor, "OMOPStagedDatasetDescriptor"))
  expect_identical(descriptor$contract_version, 2L)
  expect_identical(
    descriptor$metadata$pseudonymization$token_protocol,
    "dsomop-person-token-v2"
  )
  expect_identical(descriptor$metadata$pseudonymization$key_id,
                   contract$key_id)
  expect_identical(descriptor$metadata$pseudonymization$epoch, 3L)
  expect_identical(
    omopStagedDatasetPath(
      descriptor, expected_key_id = contract$key_id, expected_epoch = 3L
    ),
    normalizePath(descriptor$metadata$file, winslash = "/", mustWork = TRUE)
  )
})

test_that("staged resolver rejects forged, expired, and incompatible descriptors", {
  base <- tempfile("dsomop_staging_reject_")
  withr::local_options(list(
    dsstaging.base_dir = base,
    dsstaging.ttl_hours = 24
  ))
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  token <- .generateStagingToken()
  directory <- .createStagingDir(token)
  key <- openssl::rand_bytes(32L)
  descriptor <- .stageDataFrame(
    data.frame(person_id = 1:2, value = 1:2),
    "protected", directory, token, key,
    pseudonymization = list(
      available = TRUE, contract_version = 1L, provider = "file",
      key_id = .personKeyId(key), epoch = 1L
    ),
    semantic_contract = test_staged_semantics(),
    bundle_contract = test_staged_bundle("protected", token)
  )

  expect_error(
    omopStagedDatasetPath(
      descriptor,
      expected_key_id = .personKeyId(openssl::rand_bytes(32L))
    ),
    "incompatible pseudonymization key"
  )

  expired <- descriptor
  expired$expires_at <- "2000-01-01T00:00:00.000Z"
  expect_error(omopStagedDatasetPath(expired), "expired")

  malformed_expiry <- descriptor
  malformed_expiry$expires_at <- paste0(descriptor$expires_at, "junk")
  expect_error(omopStagedDatasetPath(malformed_expiry), "Invalid.*expiry")
  malformed_expiry$expires_at <- paste0(descriptor$expires_at, "\n")
  expect_error(omopStagedDatasetPath(malformed_expiry), "Invalid.*expiry")

  unscoped <- descriptor
  unscoped$metadata$pseudonymization$resource_scoped <- FALSE
  expect_error(omopStagedDatasetPath(unscoped), "resource-scoped")

  wrong_origin <- descriptor
  wrong_origin$origin <- "otherPackage"
  expect_error(omopStagedDatasetPath(wrong_origin), "origin")

  wrong_kind <- descriptor
  wrong_kind$source_kind <- "staged_csv"
  expect_error(omopStagedDatasetPath(wrong_kind), "source_kind")

  wrong_dataset <- descriptor
  wrong_dataset$dataset_id <- "../../forged"
  expect_error(omopStagedDatasetPath(wrong_dataset), "dataset_id")

  relative_file <- descriptor
  relative_file$metadata$file <- basename(descriptor$metadata$file)
  expect_error(omopStagedDatasetPath(relative_file), "absolute")

  vector_file <- descriptor
  vector_file$metadata$file <- rep(descriptor$metadata$file, 2L)
  expect_error(omopStagedDatasetPath(vector_file), "file")

  escaped <- descriptor
  escaped$metadata$file <- file.path(base, basename(descriptor$metadata$file))
  expect_error(omopStagedDatasetPath(escaped), "unavailable|escapes")

  wrong_format <- descriptor
  wrong_format$metadata$format <- if (
    identical(descriptor$metadata$format, "parquet")) "csv" else "parquet"
  expect_error(omopStagedDatasetPath(wrong_format), "does not match")

  missing_contract <- descriptor
  missing_contract$contract_version <- 1L
  missing_contract$metadata$pseudonymization <- NULL
  expect_error(omopStagedDatasetPath(missing_contract),
               "lacks its pseudonymization contract")

  missing_semantics <- descriptor
  missing_semantics$metadata$semantic_contract <- NULL
  expect_error(omopStagedDatasetPath(missing_semantics),
               "semantic contract")

  missing_bundle <- descriptor
  missing_bundle$metadata$bundle_contract <- NULL
  expect_error(omopStagedDatasetPath(missing_bundle), "bundle contract")

  mutated_semantics <- descriptor
  mutated_semantics$metadata$semantic_contract$grain <- "individual_row"
  expect_error(omopStagedDatasetPath(mutated_semantics),
               "semantic contract")

  expected_semantics <- descriptor$metadata$semantic_contract
  expect_silent(omopStagedDatasetPath(
    descriptor, expected_semantic_contract = expected_semantics
  ))
  incompatible_semantics <- test_staged_semantics("wide")
  expect_error(
    omopStagedDatasetPath(
      descriptor, expected_semantic_contract = incompatible_semantics
    ),
    "incompatible semantic contract"
  )

  if (.Platform$OS.type == "unix") {
    Sys.chmod(descriptor$metadata$file, mode = "0640")
    expect_error(omopStagedDatasetPath(descriptor), "owner-only")
    Sys.chmod(descriptor$metadata$file, mode = "0600")

    Sys.chmod(directory, mode = "0755")
    expect_error(omopStagedDatasetPath(descriptor), "token directory")
    Sys.chmod(directory, mode = "0700")

    mkfifo <- Sys.which("mkfifo")
    if (nzchar(mkfifo)) {
      fifo <- file.path(directory, paste0("fifo.", descriptor$metadata$format))
      expect_identical(system2(mkfifo, shQuote(fifo)), 0L)
      Sys.chmod(fifo, mode = "0600")
      fifo_descriptor <- descriptor
      fifo_descriptor$metadata$file <- fifo
      expect_error(omopStagedDatasetPath(fifo_descriptor), "regular file")
    }
  }
})

test_that("component siblings share an output bundle without sharing shape", {
  base <- tempfile("dsomop_staging_bundle_")
  withr::local_options(list(dsstaging.base_dir = base))
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  token <- .generateStagingToken()
  directory <- .createStagingDir(token)
  key <- openssl::rand_bytes(32L)
  public <- list(
    available = TRUE, contract_version = 1L, provider = "file",
    key_id = .personKeyId(key), epoch = 2L
  )
  output <- list(
    type = "temporal_covariates", bin_width = 30L,
    window_start = -365L, window_end = 0L
  )
  plan <- test_staged_plan(output, "trajectory")
  bundle <- .stagedBundleContract(plan, "trajectory", token)
  person_ref <- .stageDataFrame(
    data.frame(person_id = 1:3, cohort_row_id = 11:13),
    "trajectory.personRef", directory, token, key,
    pseudonymization = public,
    semantic_contract = .stagedSemanticContract(
      plan, "trajectory", "personRef"
    ),
    bundle_contract = bundle
  )
  temporal <- .stageDataFrame(
    data.frame(
      person_id = 1:3, cohort_row_id = 11:13, time_id = 1L,
      covariate_id = 1001L, covariate_value = 1
    ),
    "trajectory.temporalCovariates", directory, token, key,
    pseudonymization = public,
    semantic_contract = .stagedSemanticContract(
      plan, "trajectory", "temporalCovariates"
    ),
    bundle_contract = bundle
  )

  expect_false(identical(
    person_ref$metadata$semantic_contract,
    temporal$metadata$semantic_contract
  ))
  expect_identical(
    person_ref$metadata$semantic_contract$query_semantics_sha256,
    temporal$metadata$semantic_contract$query_semantics_sha256
  )
  expect_identical(
    person_ref$metadata$bundle_contract,
    temporal$metadata$bundle_contract
  )
  expect_silent(omopStagedDatasetPath(
    person_ref, expected_bundle_contract = bundle
  ))
  expect_silent(omopStagedDatasetPath(
    temporal, expected_bundle_contract = bundle
  ))
  expect_error(
    omopStagedDatasetPath(
      temporal,
      expected_semantic_contract = person_ref$metadata$semantic_contract
    ),
    "incompatible semantic contract"
  )

  other_token <- .generateStagingToken()
  incompatible <- .stagedBundleContract(plan, "trajectory", other_token)
  expect_error(
    omopStagedDatasetPath(
      temporal, expected_bundle_contract = incompatible
    ),
    "incompatible bundle contract"
  )
})

test_that("staging manifest v2 round-trips complete descriptors", {
  base <- tempfile("dsomop_staging_manifest_")
  withr::local_options(list(dsstaging.base_dir = base))
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  token <- .generateStagingToken()
  directory <- .createStagingDir(token)
  key <- openssl::rand_bytes(32L)
  output <- test_staged_output()
  plan <- test_staged_plan(output, "analysis")
  descriptor <- .stageDataFrame(
    data.frame(person_id = 1:3, value = c(2, 4, 8)),
    "analysis", directory, token, key,
    pseudonymization = .testPublicPseudonymization(key),
    semantic_contract = .stagedSemanticContract(plan, "analysis"),
    bundle_contract = .stagedBundleContract(plan, "analysis", token)
  )

  path <- .writeStagingManifest(directory, list(analysis = descriptor))
  parsed <- .readStagingManifest(path)
  restored <- parsed$outputs$analysis

  expect_s3_class(restored, "FlowerDatasetDescriptor")
  expect_identical(restored$dataset_id, descriptor$dataset_id)
  expect_identical(restored$source_kind, descriptor$source_kind)
  expect_identical(restored$staged_token, descriptor$staged_token)
  expect_identical(restored$metadata$pseudonymization,
                   descriptor$metadata$pseudonymization)
  expect_identical(restored$metadata$semantic_contract,
                   descriptor$metadata$semantic_contract)
  expect_identical(restored$metadata$bundle_contract,
                   descriptor$metadata$bundle_contract)
  expect_identical(
    omopStagedDatasetPath(restored),
    normalizePath(descriptor$metadata$file, winslash = "/", mustWork = TRUE)
  )
})

test_that("different staged pseudonym roots cannot be joined as compatible", {
  base <- tempfile("dsomop_staging_keys_")
  withr::local_options(list(dsstaging.base_dir = base))
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  token <- .generateStagingToken()
  directory <- .createStagingDir(token)
  first_key <- openssl::rand_bytes(32L)
  second_key <- openssl::rand_bytes(32L)

  make_contract <- function(key) list(
    available = TRUE, contract_version = 1L, provider = "file",
    key_id = .personKeyId(key), epoch = 1L
  )
  first <- .stageDataFrame(
    data.frame(person_id = 1:2), "first", directory, token, first_key,
    pseudonymization = make_contract(first_key),
    semantic_contract = test_staged_semantics(),
    bundle_contract = test_staged_bundle("first", token)
  )
  second <- .stageDataFrame(
    data.frame(person_id = 1:2), "second", directory, token, second_key,
    pseudonymization = make_contract(second_key),
    semantic_contract = test_staged_semantics(),
    bundle_contract = test_staged_bundle("second", token)
  )
  expected <- first$metadata$pseudonymization$key_id

  expect_silent(omopStagedDatasetPath(first, expected_key_id = expected))
  expect_error(
    omopStagedDatasetPath(second, expected_key_id = expected),
    "incompatible pseudonymization key"
  )
})

test_that("person-bearing staging refuses unscoped legacy keys", {
  base <- tempfile("dsomop_staging_legacy_")
  withr::local_options(list(dsstaging.base_dir = base))
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  token <- .generateStagingToken()
  directory <- .createStagingDir(token)
  key <- openssl::rand_bytes(32L)
  frame <- data.frame(person_id = 1L, value = 2)

  expect_error(
    .stageDataFrame(
      frame, "missing", directory, token, key,
      semantic_contract = test_staged_semantics(),
      bundle_contract = test_staged_bundle("missing", token)
    ),
    "explicit resource-scoped"
  )
  expect_error(
    .stageDataFrame(
      frame, "legacy", directory, token, key,
      pseudonymization = list(
        available = TRUE, contract_version = 1L, provider = "legacy",
        key_id = .personKeyId(key), epoch = 1L
      ),
      semantic_contract = test_staged_semantics(),
      bundle_contract = test_staged_bundle("legacy", token)
    ),
    "resource-scoped pseudonymization provider"
  )
})
