test_that("runtime bootstrap creates one persistent private root", {
  skip_on_os("windows")
  state <- withr::local_tempdir(pattern = "dsomop-state-")
  Sys.chmod(state, mode = "0700")
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1",
    DSOMOP_PSEUDONYM_ROOT = "",
    DSOMOP_PSEUDONYM_KEY = ""
  ))
  withr::local_options(list(
    dsomop.pseudonym_root = NULL, dsomop.pseudonym_key = NULL))
  requested <- integer()
  deterministic_rng <- function(n) {
    requested <<- c(requested, n)
    as.raw(seq_len(n) - 1L)
  }

  first <- .ensureDsomopSecret(
    "pseudonym_root", random_bytes = deterministic_rng)
  second <- .ensureDsomopSecret(
    "pseudonym_root", random_bytes = function(n) stop("must not resample"))

  expect_identical(first, as.raw(0:31))
  expect_identical(second, first)
  expect_identical(requested, 32L)
  path <- file.path(state, "secrets", "pseudonym_root")
  expect_identical(file.info(path)$size, 32)
  expect_identical(as.integer(file.info(dirname(path))$mode), 448L) # 0700
  expect_identical(as.integer(file.info(path)$mode), 384L)         # 0600
  expect_identical(.dsomopLinkCount(path), 1)
})

test_that("secret bootstrap fails closed when durable sync cannot be proven", {
  skip_on_os("windows")
  state <- withr::local_tempdir(pattern = "dsomop-sync-fail-")
  Sys.chmod(state, mode = "0700")
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1"
  ))
  path <- file.path(state, "secrets", "pseudonym_root")

  expect_error(
    .ensureDsomopSecret(
      "pseudonym_root", random_bytes = function(n) as.raw(seq_len(n) - 1L),
      .sync = function(path) FALSE
    ),
    "durably synchronize"
  )
  expect_false(file.exists(path))

  calls <- 0L
  staged_sync <- function(path) {
    calls <<- calls + 1L
    calls == 1L
  }
  expect_error(
    .ensureDsomopSecret(
      "pseudonym_root", random_bytes = function(n) as.raw(seq_len(n) - 1L),
      .sync = staged_sync
    ),
    "durably synchronize"
  )
  # The no-clobber commit happened, but callers never receive the key until its
  # directory entry is synced successfully.
  expect_true(file.exists(path))
  expect_identical(
    .ensureDsomopSecret(
      "pseudonym_root", random_bytes = function(n) stop("must not resample"),
      .sync = function(path) TRUE
    ),
    as.raw(0:31)
  )
})

test_that("concurrent workers converge on one pseudonym root", {
  skip_on_os("windows")
  state <- withr::local_tempdir(pattern = "dsomop-concurrent-")
  Sys.chmod(state, mode = "0700")
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1",
    DSOMOP_PSEUDONYM_ROOT = "",
    DSOMOP_PSEUDONYM_KEY = ""
  ))

  values <- unlist(parallel::mclapply(
    seq_len(4L), function(unused) {
      paste(format(.ensureDsomopSecret("pseudonym_root")), collapse = "")
    }, mc.cores = 4L, mc.preschedule = FALSE), use.names = FALSE)

  expect_length(unique(values), 1L)
  path <- file.path(state, "secrets", "pseudonym_root")
  expect_identical(file.info(path)$size, 32)
  expect_identical(.dsomopLinkCount(path), 1)
})

test_that("secret commit never replaces a winning key", {
  skip_on_os("windows")
  state <- withr::local_tempdir(pattern = "dsomop-no-clobber-")
  Sys.chmod(state, mode = "0700")
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1"
  ))
  path <- .dsomopPrivateSecretDirectory(
    file.path(state, "secrets", "pseudonym_root"),
    .allow_test_path = TRUE
  )
  winner <- as.raw(0:31)
  candidate_value <- as.raw(31:0)
  writeBin(winner, path)
  Sys.chmod(path, mode = "0600")
  candidate <- tempfile(pattern = ".candidate-", tmpdir = dirname(path))
  writeBin(candidate_value, candidate)
  Sys.chmod(candidate, mode = "0600")
  on.exit(unlink(candidate, force = TRUE), add = TRUE)

  adopted <- .dsomopCommitSecretNoClobber(candidate, path)
  expect_identical(adopted, winner)
  expect_identical(.dsomopValidateSecretFile(path), winner)
  expect_true(file.exists(candidate))
  expect_false(grepl("file\\.rename", paste(deparse(body(.ensureDsomopSecret)),
                                               collapse = "\n")))

  unsupported_path <- file.path(dirname(path), "unsupported-root")
  expect_error(
    .dsomopCommitSecretNoClobber(
      candidate, unsupported_path, retry_seconds = 0,
      .link = function(from, to) FALSE
    ),
    "filesystem may not support"
  )
  expect_false(file.exists(unsupported_path))
})

test_that("secret bootstrap fails closed on unsafe files and entropy", {
  skip_on_os("windows")
  state <- withr::local_tempdir(pattern = "dsomop-negative-")
  Sys.chmod(state, mode = "0700")
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1",
    DSOMOP_PSEUDONYM_ROOT = "",
    DSOMOP_PSEUDONYM_KEY = ""
  ))
  path <- file.path(state, "secrets", "pseudonym_root")

  expect_error(
    .ensureDsomopSecret(
      "pseudonym_root", random_bytes = function(n) raw(n - 1L)),
    "entropy"
  )
  expect_false(file.exists(path))

  value <- .ensureDsomopSecret("pseudonym_root")
  linked <- file.path(dirname(path), "linked-root")
  expect_true(file.link(path, linked))
  expect_error(.dsomopValidateSecretFile(path), "hard links")
  unlink(linked)
  expect_identical(.dsomopValidateSecretFile(path), value)

  target <- file.path(dirname(path), "target-root")
  writeBin(value, target)
  Sys.chmod(target, mode = "0600")
  alias <- file.path(dirname(path), "alias-root")
  expect_true(file.symlink(target, alias))
  expect_error(.dsomopValidateSecretFile(alias), "regular owner-only")
})

test_that("temporary production state and weak injected roots are rejected", {
  state <- withr::local_tempdir(pattern = "dsomop-ephemeral-")
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = NA_character_
  ))
  expect_error(.dsomopStateRoot(), "persistent")
  expect_error(.coerceDsomopSecret("too-short"), "at least 32 bytes")
})

test_that("system and temporary roots are rejected before any mutation", {
  forbidden <- c("/", "/tmp", "/tmp/dsomop", "/var/tmp",
                 "/var/tmp/dsomop", "/dev/shm", "/dev/shm/dsomop")
  for (path in forbidden) {
    expect_error(
      .dsomopStateRoot(configured = path, environment = "", home = "",
                       .allow_test_path = FALSE),
      "persistent"
    )
  }

  state <- withr::local_tempdir(pattern = "dsomop-existing-mode-")
  Sys.chmod(state, mode = "0755")
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1"
  ))
  expect_error(
    .dsomopPrivateSecretDirectory(
      file.path(state, "secrets", "pseudonym_root"),
      .allow_test_path = TRUE
    ),
    "mode 0700"
  )
  expect_identical(as.integer(file.info(state)$mode), 493L) # still 0755
})

test_that("legacy marker directory is only a state-root alias", {
  state <- withr::local_tempdir(pattern = "dsomop-marker-alias-")
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = NA_character_,
    DSOMOP_MARKER_DIR = state,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1"
  ))
  withr::local_options(list(
    dsomop.state_dir = NULL, default.dsomop.state_dir = NULL))
  expect_identical(.dsomopStateRoot(), state)
})

test_that("an injected root is validated but never copied to state", {
  state <- withr::local_tempdir(pattern = "dsomop-injected-")
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1",
    DSOMOP_PSEUDONYM_ROOT = paste(rep("ab", 32L), collapse = ""),
    DSOMOP_PSEUDONYM_KEY = ""
  ))
  withr::local_options(list(
    dsomop.pseudonym_root = NULL, dsomop.pseudonym_key = NULL))

  expect_identical(.bootstrapDsomopSecrets(), "injected")
  expect_false(file.exists(file.path(state, "secrets", "pseudonym_root")))
})

test_that("per-resource-only injection does not require writable state", {
  rid <- paste(rep("a", 32L), collapse = "")
  scoped <- paste(rep("cd", 32L), collapse = "")
  environment <- c(scoped)
  names(environment) <- paste0("DSOMOP_PSEUDONYM_KEY_", rid)
  environment <- c(environment,
                   DSOMOP_PSEUDONYM_ROOT = "",
                   DSOMOP_PSEUDONYM_KEY = "")
  withr::local_envvar(environment)
  withr::local_options(list(
    dsomop.pseudonym_root = NULL, dsomop.pseudonym_key = NULL))

  expect_identical(.bootstrapDsomopSecrets(), "scoped_injected")
})

test_that("a legacy per-resource file remains authoritative", {
  skip_on_os("windows")
  state <- withr::local_tempdir(pattern = "dsomop-legacy-")
  Sys.chmod(state, mode = "0700")
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1",
    DSOMOP_PSEUDONYM_ROOT = "",
    DSOMOP_PSEUDONYM_KEY = ""
  ))
  withr::local_options(list(
    dsomop.pseudonym_root = NULL, dsomop.pseudonym_key = NULL))
  resource <- list(
    getResource = function() list(
      url = "datashield://siteA/legacy", name = NULL),
    getParsed = function() list(server = "datashield://siteA/legacy")
  )
  identity <- dsOMOP:::.resourceIdentity(resource)
  rid <- substr(as.character(openssl::sha256(charToRaw(identity))), 1L, 32L)
  key_dir <- file.path(state, "keys")
  dir.create(key_dir, mode = "0700")
  Sys.chmod(key_dir, mode = "0700")
  key <- as.raw(31:0)
  path <- file.path(key_dir, paste0(rid, ".key"))
  writeBin(key, path)
  Sys.chmod(path, mode = "0600")

  expect_identical(dsOMOP:::.resolvePersonKey(resource), key)
  expect_false(file.exists(file.path(state, "secrets", "pseudonym_root")))
})

test_that("pseudonymization fails without a stable resource identity", {
  resource <- list(
    getResource = function() list(url = NULL, name = NULL),
    getParsed = function() list(server = NULL)
  )
  expect_error(dsOMOP:::.resourceIdentity(resource), "stable OMOP resource")
})

test_that("configure prepares state only and contains no entropy generation", {
  configure_path <- testthat::test_path("..", "..", "configure")
  skip_if_not(
    file.exists(configure_path),
    "source-only configure script is unavailable in the installed test tree"
  )
  configure <- paste(readLines(configure_path, warn = FALSE), collapse = "\n")
  expect_false(grepl(
    "rand_bytes|/dev/urandom|openssl[[:space:]]+rand|sample\\.int|runif|rnorm",
    configure, perl = TRUE
  ))
  expect_match(configure, "first real OMOP handle use", fixed = TRUE)
  expect_false(grepl(
    "apt-get|install\\.packages|curl|wget|packagemanager|cloud\\.r-project",
    configure, ignore.case = TRUE, perl = TRUE
  ))
})

test_that("pseudonym lifecycle settings are strict and conflicts fail closed", {
  empty_env <- c(
    DSOMOP_PSEUDONYM_PROVIDER = NA_character_,
    DSOMOP_PSEUDONYM_EPOCH = NA_character_,
    DSOMOP_PSEUDONYM_REQUIRE_EXISTING = NA_character_,
    DSOMOP_ALLOW_LEGACY_GLOBAL_PSEUDONYMS = NA_character_,
    DSOMOP_PSEUDONYM_ROOT = NA_character_,
    DSOMOP_PSEUDONYM_KEY = NA_character_
  )
  empty_options <- list(
    dsomop.pseudonym_provider = NULL,
    default.dsomop.pseudonym_provider = NULL,
    dsomop.pseudonym_epoch = NULL,
    default.dsomop.pseudonym_epoch = NULL,
    dsomop.pseudonym_require_existing = NULL,
    default.dsomop.pseudonym_require_existing = NULL,
    dsomop.allow_legacy_global_pseudonyms = NULL,
    default.dsomop.allow_legacy_global_pseudonyms = NULL,
    dsomop.pseudonym_root = NULL,
    default.dsomop.pseudonym_root = NULL,
    dsomop.pseudonym_key = NULL,
    default.dsomop.pseudonym_key = NULL
  )

  withr::with_envvar(empty_env, withr::with_options(empty_options, {
    expect_identical(
      .dsomopPseudonymLifecycleSettings(),
      list(provider = "auto", epoch = 1L, require_existing = FALSE,
           allow_legacy_global = FALSE)
    )

    withr::with_envvar(c(DSOMOP_PSEUDONYM_EPOCH = "0"), {
      expect_error(.dsomopPseudonymLifecycleSettings(), "positive integer")
    })
    withr::with_envvar(c(DSOMOP_PSEUDONYM_EPOCH = "2"),
      withr::with_options(list(dsomop.pseudonym_epoch = 3L), {
        expect_error(.dsomopPseudonymLifecycleSettings(), "Conflicting")
      }))
    withr::with_envvar(c(DSOMOP_PSEUDONYM_PROVIDER = "file"),
      withr::with_options(list(dsomop.pseudonym_provider = "injected"), {
        expect_error(.dsomopPseudonymLifecycleSettings(), "Conflicting")
      }))
    withr::with_envvar(c(
      DSOMOP_ALLOW_LEGACY_GLOBAL_PSEUDONYMS = "not-a-boolean"
    ), {
      expect_error(.dsomopPseudonymLifecycleSettings(), "TRUE or FALSE")
    })

    root_a <- paste(rep("ab", 32L), collapse = "")
    root_b <- paste(rep("cd", 32L), collapse = "")
    withr::with_envvar(c(DSOMOP_PSEUDONYM_ROOT = root_a),
      withr::with_options(list(dsomop.pseudonym_root = root_b), {
        expect_error(.dsomopPseudonymSettings(), "Conflicting secret")
      }))
    withr::with_envvar(c(
      DSOMOP_PSEUDONYM_PROVIDER = "file",
      DSOMOP_PSEUDONYM_ROOT = root_a
    ), {
      expect_error(.dsomopPseudonymSettings(), "file.*conflicts")
    })
    withr::with_envvar(c(DSOMOP_PSEUDONYM_PROVIDER = "injected"), {
      expect_error(.dsomopPseudonymSettings(), "requires.*ROOT")
    })
  }))
})

test_that("onLoad is key-free and compatibility bootstrap is lazy", {
  skip_on_os("windows")
  state <- withr::local_tempdir(pattern = "dsomop-lazy-")
  Sys.chmod(state, mode = "0700")
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1",
    DSOMOP_PSEUDONYM_PROVIDER = "auto",
    DSOMOP_PSEUDONYM_EPOCH = "1",
    DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false",
    DSOMOP_PSEUDONYM_ROOT = NA_character_,
    DSOMOP_PSEUDONYM_KEY = NA_character_,
    DEVTOOLS_LOAD = NA_character_,
    R_INSTALL_PKG = NA_character_,
    R_PACKAGE_DIR = NA_character_,
    `_R_CHECK_PACKAGE_NAME_` = NA_character_
  ))
  withr::local_options(list(
    dsomop.state_dir = NULL,
    default.dsomop.state_dir = NULL,
    dsomop.pseudonym_provider = NULL,
    default.dsomop.pseudonym_provider = NULL,
    dsomop.pseudonym_epoch = NULL,
    default.dsomop.pseudonym_epoch = NULL,
    dsomop.pseudonym_require_existing = NULL,
    default.dsomop.pseudonym_require_existing = NULL,
    dsomop.pseudonym_root = NULL,
    default.dsomop.pseudonym_root = NULL,
    dsomop.pseudonym_key = NULL,
    default.dsomop.pseudonym_key = NULL
  ))
  path <- file.path(state, "secrets", "pseudonym_root")

  previous_resolver <- .pkg_state$resolver
  expect_silent(.onLoad("/opt/dsomop/runtime-library", "dsOMOP"))
  loaded_resolver <- .pkg_state$resolver
  on.exit({
    if (!is.null(loaded_resolver) &&
        !identical(loaded_resolver, previous_resolver)) {
      try(
        resourcer::unregisterResourceResolver("OMOPResourceResolver"),
        silent = TRUE
      )
      if (!is.null(previous_resolver)) {
        try(resourcer::registerResourceResolver(previous_resolver), silent = TRUE)
      }
    }
    .pkg_state$resolver <- previous_resolver
  }, add = TRUE)
  expect_false(file.exists(path))

  handle <- new.env(parent = emptyenv())
  handle$person_key_identity <- "datashield://siteA/omop"
  expect_type(.personKey(handle), "raw")
  expect_true(file.exists(path))
  expect_identical(handle$person_key_provider, "file")
  expect_identical(handle$person_key_epoch, 1L)
})

test_that("file provider require-existing never replaces a lost volume", {
  skip_on_os("windows")
  state <- withr::local_tempdir(pattern = "dsomop-require-existing-")
  Sys.chmod(state, mode = "0700")
  withr::local_envvar(c(
    DSOMOP_STATE_DIR = state,
    DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1",
    DSOMOP_PSEUDONYM_PROVIDER = "file",
    DSOMOP_PSEUDONYM_EPOCH = "7",
    DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "true",
    DSOMOP_PSEUDONYM_ROOT = NA_character_,
    DSOMOP_PSEUDONYM_KEY = NA_character_
  ))
  withr::local_options(list(
    dsomop.pseudonym_provider = NULL,
    default.dsomop.pseudonym_provider = NULL,
    dsomop.pseudonym_epoch = NULL,
    default.dsomop.pseudonym_epoch = NULL,
    dsomop.pseudonym_require_existing = NULL,
    default.dsomop.pseudonym_require_existing = NULL,
    dsomop.pseudonym_root = NULL,
    default.dsomop.pseudonym_root = NULL,
    dsomop.pseudonym_key = NULL,
    default.dsomop.pseudonym_key = NULL
  ))
  path <- file.path(state, "secrets", "pseudonym_root")

  expect_error(
    .resolvePersonKeyContract("datashield://siteA/omop"),
    "requires an existing secret"
  )
  expect_false(file.exists(path))
  expect_false(dir.exists(dirname(path)))

  Sys.setenv(DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false")
  provisioned <- .resolvePersonKeyContract("datashield://siteA/omop")
  expect_true(file.exists(path))
  expect_identical(provisioned$epoch, 7L)

  Sys.setenv(DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "true")
  restarted <- .resolvePersonKeyContract("datashield://siteA/omop")
  expect_identical(restarted$key_id, provisioned$key_id)
  expect_true(restarted$require_existing)
})

test_that("independent processes share one file identity across restart", {
  skip_on_os("windows")
  state <- withr::local_tempdir(pattern = "dsomop-processes-")
  Sys.chmod(state, mode = "0700")
  package_root <- normalizePath(testthat::test_path("..", ".."),
                                winslash = "/", mustWork = TRUE)
  script <- tempfile(pattern = "dsomop-key-worker-", fileext = ".R")
  writeLines(c(
    "args <- commandArgs(TRUE)",
    "state <- args[[1L]]",
    "package_root <- args[[2L]]",
    "require_existing <- identical(args[[3L]], 'true')",
    "Sys.setenv(DSOMOP_STATE_DIR = state,",
    "  DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = '1',",
    "  DSOMOP_PSEUDONYM_PROVIDER = 'file',",
    "  DSOMOP_PSEUDONYM_EPOCH = '11',",
    "  DSOMOP_PSEUDONYM_REQUIRE_EXISTING = if (require_existing) 'true' else 'false')",
    "Sys.unsetenv(c('DSOMOP_PSEUDONYM_ROOT', 'DSOMOP_PSEUDONYM_KEY'))",
    "options(dsomop.pseudonym_provider = NULL,",
    "  default.dsomop.pseudonym_provider = NULL,",
    "  dsomop.pseudonym_epoch = NULL,",
    "  default.dsomop.pseudonym_epoch = NULL,",
    "  dsomop.pseudonym_require_existing = NULL,",
    "  default.dsomop.pseudonym_require_existing = NULL,",
    "  dsomop.pseudonym_root = NULL, default.dsomop.pseudonym_root = NULL,",
    "  dsomop.pseudonym_key = NULL, default.dsomop.pseudonym_key = NULL)",
    "if (file.exists(file.path(package_root, 'R', 'state_secrets.R'))) {",
    "  source(file.path(package_root, 'R', 'state_secrets.R'))",
    "  source(file.path(package_root, 'R', 'blueprint.R'))",
    "} else {",
    "  suppressPackageStartupMessages(library(dsOMOP))",
    "  .resolvePersonKeyContract <- getFromNamespace('.resolvePersonKeyContract', 'dsOMOP')",
    "}",
    "contract <- .resolvePersonKeyContract('datashield://siteA/omop')",
    "cat(contract$key_id, contract$epoch, sep = '|')"
  ), script)
  on.exit(unlink(script), add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  run_worker <- function(require_existing) {
    suppressWarnings(system2(
      rscript,
      c("--vanilla", shQuote(script), shQuote(state), shQuote(package_root),
        if (require_existing) "true" else "false"),
      stdout = TRUE, stderr = TRUE
    ))
  }

  concurrent <- parallel::mclapply(
    seq_len(4L), function(unused) run_worker(FALSE),
    mc.cores = 4L, mc.preschedule = FALSE
  )
  statuses <- vapply(concurrent, function(value) {
    status <- attr(value, "status")
    if (is.null(status)) 0L else as.integer(status)
  }, integer(1))
  expect_identical(statuses, rep(0L, 4L))
  identities <- vapply(concurrent, paste, collapse = "\n", FUN.VALUE = "")
  expect_length(unique(identities), 1L)
  expect_match(identities[[1L]], "^dsomop-person-key-v1:[0-9a-f]{64}\\|11$")

  restarted <- run_worker(TRUE)
  expect_null(attr(restarted, "status"))
  expect_identical(paste(restarted, collapse = "\n"), identities[[1L]])

  key_path <- file.path(state, "secrets", "pseudonym_root")
  expect_true(unlink(key_path) == 0L)
  after_loss <- run_worker(TRUE)
  expect_true(!is.null(attr(after_loss, "status")))
  expect_match(paste(after_loss, collapse = "\n"), "requires an existing secret")
  expect_false(file.exists(key_path))
})
