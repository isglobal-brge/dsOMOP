# Differential-privacy service state.
#
# This module is deliberately independent from person pseudonymization.  The
# DP roots are independent from identity and from each other: a replaceable
# noise root drives the PRF stream, while a continuity-critical ledger root
# authenticates releases. Neither is stored in an OMOP handle or returned.

.DSOMOP_DP_PROTOCOL <- "dsomop-dp-release-v1"
.DSOMOP_DP_CANONICAL_PROTOCOL <- "dsomop-dp-canonical-json-v1"
.DSOMOP_DP_LOOKUP_PROTOCOL <- "dsomop-dp-private-release-lookup-v3"
.DSOMOP_DP_MECHANISM <- "dsomop-sticky-discrete-laplace-prf-v1"
.DSOMOP_DP_SAMPLER <- "hmac-inverse-cdf-52bit-v1"
.DSOMOP_PRIVACY_GUARANTEE <- paste0(
  "sticky_person_bounded_noise_with_authenticated_lineage_",
  "and_nominal_accounting"
)
.DSOMOP_DP_LEDGER_SCHEMA <- 2L
.DSOMOP_DP_GENESIS <- "GENESIS"
.DSOMOP_DP_MIN_USEFUL_EPSILON <- 1e-6
.DSOMOP_DP_LEDGER_AUDIT_CHUNK <- 4096L

.dsomopDpZetaMass <- function(scale, cap) {
  if (scale <= 0) return(0)
  capped_terms <- floor(sqrt(scale / cap))
  capped_terms * cap + scale * base::psigamma(capped_terms + 1, deriv = 1)
}

.dsomopDpZetaScale <- function(total, cap) {
  upper <- total + total^2 / cap
  root <- stats::uniroot(
    function(scale) .dsomopDpZetaMass(scale, cap) - total,
    interval = c(0, upper), tol = .Machine$double.eps^0.75
  )$root
  if (!is.finite(root) || root <= 0) {
    stop("The DP zeta allocator could not be normalized.", call. = FALSE)
  }
  root
}

.dsomopDpHex <- function(value) {
  if (!is.raw(value)) stop("Cannot encode non-raw DP state.", call. = FALSE)
  paste(format(value), collapse = "")
}

.dsomopDpSha256 <- function(value) {
  if (!is.raw(value)) value <- charToRaw(enc2utf8(value))
  .dsomopDpHex(as.raw(openssl::sha256(value)))
}

.dsomopDpHmacRaw <- function(key, value) {
  if (!is.raw(key) || length(key) != 32L) {
    stop("The DP root is unavailable.", call. = FALSE)
  }
  if (!is.raw(value)) value <- charToRaw(enc2utf8(value))
  as.raw(openssl::sha256(value, key = key))
}

.dsomopDpHmac <- function(key, value) {
  .dsomopDpHex(.dsomopDpHmacRaw(key, value))
}

.dsomopDpCanonicalValue <- function(value) {
  if (is.null(value)) return(NULL)
  if (inherits(value, "Date")) {
    if (anyNA(value)) {
      stop("A canonical DP query cannot contain missing dates.", call. = FALSE)
    }
    return(format(value, "%Y-%m-%d"))
  }
  if (is.object(value)) {
    stop("A canonical DP query contains an unsupported value type.",
         call. = FALSE)
  }
  if (is.list(value)) {
    fields <- names(value)
    if (!is.null(fields)) {
      if (anyNA(fields) || any(!nzchar(fields)) || anyDuplicated(fields)) {
        stop("A canonical DP query contains invalid object fields.",
             call. = FALSE)
      }
      value <- value[order(fields, method = "radix")]
    }
    return(lapply(value, .dsomopDpCanonicalValue))
  }
  if (!typeof(value) %in% c("logical", "integer", "double", "character")) {
    stop("A canonical DP query contains an unsupported value type.",
         call. = FALSE)
  }
  if (!is.null(names(value))) {
    stop("Canonical DP query vectors must be unnamed.", call. = FALSE)
  }
  if (anyNA(value) || (is.numeric(value) && any(!is.finite(value)))) {
    stop("A canonical DP query contains a missing or non-finite value.",
         call. = FALSE)
  }
  if (is.character(value)) return(enc2utf8(unname(value)))
  if (is.numeric(value)) {
    value <- unname(as.numeric(value))
    value[value == 0] <- 0
    return(value)
  }
  unname(value)
}

.dsomopDpCanonicalJson <- function(value) {
  value <- .dsomopDpCanonicalValue(value)
  as.character(jsonlite::toJSON(
    value, auto_unbox = TRUE, null = "null", na = "null", digits = 17,
    POSIXt = "ISO8601", UTC = TRUE, pretty = FALSE
  ))
}

.dsomopDpCanonicalSelfTest <- function() {
  observed <- .dsomopDpSha256(.dsomopDpCanonicalJson(list(
    protocol = .DSOMOP_DP_CANONICAL_PROTOCOL,
    method = "bounded_histogram",
    arguments = list(levels = c("a", "b"), cap = 1L, zero = -0),
    version = 1L
  )))
  expected <- "d27cf24db74bd4ceed8fa8700fa195b59a11316e092a572e75f4058ddd6e42b9"
  if (!identical(observed, expected)) {
    stop("The canonical DP encoder changed; sticky releases are disabled ",
         "until an explicit protocol migration is installed.", call. = FALSE)
  }
  sampler_observed <- .dsomopDpSha256(.dsomopDpCanonicalJson(list(
    protocol = "dsomop-dp-sampler-kat-v1",
    noise = .dsomopDpDiscreteLaplace(
      as.raw(0:31),
      list(protocol = "dsomop-sampler-kat-v1", query = "fixed",
           component = "count"),
      coordinate = 7L, epsilon = 0.25, sensitivity = 3
    )
  )))
  sampler_expected <-
    "f41d43840fc89a32939f8413d825b14d5900a13ae80e0f37b35111294e4acaf0"
  if (!identical(sampler_observed, sampler_expected)) {
    stop("The sticky-noise sampler changed; releases are disabled until an ",
         "explicit sampler protocol migration is installed.", call. = FALSE)
  }
  invisible(TRUE)
}

.dsomopDpOption <- function(name, default = NULL) {
  option <- paste0("dsomop.dp.", name)
  configured <- getOption(option, NULL)
  environment_names <- c(
    enabled = "DSOMOP_DP_ENABLED",
    domain = "DSOMOP_DP_DOMAIN",
    snapshot_id = "DSOMOP_DP_SNAPSHOT_ID",
    accounting_mode = "DSOMOP_DP_ACCOUNTING_MODE",
    total_epsilon = "DSOMOP_DP_TOTAL_EPSILON",
    release_epsilon = "DSOMOP_DP_RELEASE_EPSILON",
    privacy_epoch = "DSOMOP_DP_PRIVACY_EPOCH",
    require_external_anchor = "DSOMOP_DP_REQUIRE_EXTERNAL_ANCHOR",
    noise_provider = "DSOMOP_DP_NOISE_PROVIDER",
    noise_require_existing = "DSOMOP_DP_NOISE_REQUIRE_EXISTING",
    ledger_provider = "DSOMOP_DP_LEDGER_PROVIDER",
    ledger_require_existing = "DSOMOP_DP_LEDGER_REQUIRE_EXISTING",
    max_levels = "DSOMOP_DP_MAX_LEVELS",
    max_contributions = "DSOMOP_DP_MAX_CONTRIBUTIONS",
    numeric_grid = "DSOMOP_DP_NUMERIC_GRID"
  )
  environment_name <- unname(environment_names[name])
  if (length(environment_name) != 1L || is.na(environment_name)) {
    environment_name <- NULL
  }
  raw <- if (is.null(environment_name)) "" else
    Sys.getenv(environment_name, unset = "")
  if (!nzchar(raw)) {
    if (!is.null(configured)) return(configured)
    return(getOption(paste0("default.", option), default))
  }
  numeric_names <- c(
    "total_epsilon", "release_epsilon", "privacy_epoch", "max_levels",
    "max_contributions", "numeric_grid"
  )
  environment <- if (name %in% numeric_names) {
    suppressWarnings(as.numeric(raw))
  } else raw
  if (!is.null(configured)) {
    same <- if (name %in% c(
      "enabled", "require_external_anchor", "noise_require_existing",
      "ledger_require_existing"
    )) {
      identical(.dsomopDpBoolean(configured, name),
                .dsomopDpBoolean(environment, name))
    } else if (name %in% numeric_names) {
      is.numeric(configured) && length(configured) == 1L &&
        !is.na(configured) && identical(as.numeric(configured), environment)
    } else {
      is.character(configured) && length(configured) == 1L &&
        !is.na(configured) && identical(enc2utf8(configured), enc2utf8(raw))
    }
    if (!isTRUE(same)) {
      stop("Conflicting DP option and ", environment_name,
           " environment values are configured.", call. = FALSE)
    }
  }
  environment
}

.dsomopDpBoolean <- function(value, name) {
  .dsomopParsePseudonymBoolean(value, paste0("dsomop.dp.", name))
}

.dsomopDpNumber <- function(value, name, lower, upper = Inf,
                            integer = FALSE, lower_open = FALSE) {
  valid <- is.numeric(value) && length(value) == 1L && !is.na(value) &&
    is.finite(value) && value <= upper &&
    if (lower_open) value > lower else value >= lower
  if (valid && integer) valid <- value == floor(value)
  if (!isTRUE(valid)) {
    stop("dsomop.dp.", name, " has an invalid server-owned value.",
         call. = FALSE)
  }
  if (integer) as.integer(value) else as.numeric(value)
}

.dsomopDpString <- function(value, name, required = TRUE) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      (required && !nzchar(value))) {
    stop("dsomop.dp.", name, " must be one ",
         if (required) "non-empty " else "", "string.", call. = FALSE)
  }
  enc2utf8(value)
}

.dsomopDpEnabled <- function() {
  enabled <- .dsomopDpBoolean(.dsomopDpOption("enabled", FALSE), "enabled")
  binding <- .pkg_state$dp_bootstrap_binding
  if (is.list(binding) && !identical(binding$enabled, enabled)) {
    stop("DP enablement changed after service bootstrap; restart the service ",
         "with its final configuration.", call. = FALSE)
  }
  enabled
}

.dsomopDpAssertBootstrapPolicy <- function(policy) {
  binding <- .pkg_state$dp_bootstrap_binding
  if (!is.list(binding) || !isTRUE(binding$enabled)) return(invisible(TRUE))
  current <- list(
    enabled = TRUE,
    state_root = .dsomopStateRoot(),
    ledger_path = policy$ledger_path,
    policy_hash = policy$policy_hash,
    domain = policy$domain,
    snapshot_id = policy$snapshot_id,
    privacy_epoch = policy$privacy_epoch,
    anchor_provider = policy$anchor_provider
  )
  if (!identical(binding[names(current)], current)) {
    stop("DP policy changed after service bootstrap; restart the service with ",
         "the new snapshot, epoch, or policy.", call. = FALSE)
  }
  invisible(TRUE)
}

.dsomopDpAnchorProvider <- function() {
  configured <- .dsomopDpOption("anchor_provider", NULL)
  reference <- Sys.getenv("DSOMOP_DP_ANCHOR_PROVIDER", unset = "")
  if (!nzchar(reference)) return(configured)
  if (!is.null(configured)) {
    stop("Conflicting DP anchor providers are configured.", call. = FALSE)
  }
  if (!grepl(
    "^[A-Za-z][A-Za-z0-9.]*::[A-Za-z][A-Za-z0-9._]*$", reference
  )) {
    stop("DSOMOP_DP_ANCHOR_PROVIDER must name one exported package::function.",
         call. = FALSE)
  }
  parts <- strsplit(reference, "::", fixed = TRUE)[[1L]]
  if (!requireNamespace(parts[[1L]], quietly = TRUE)) {
    stop("The configured DP anchor-provider package is unavailable.",
         call. = FALSE)
  }
  provider <- tryCatch(
    getExportedValue(parts[[1L]], parts[[2L]]),
    error = function(e) NULL
  )
  if (!is.function(provider)) {
    stop("DSOMOP_DP_ANCHOR_PROVIDER does not resolve to an exported function.",
         call. = FALSE)
  }
  provider
}

.dsomopDpLedgerPath <- function() {
  configured <- .dsomopDpOption("ledger_path", NULL)
  environment <- Sys.getenv("DSOMOP_DP_LEDGER_PATH", unset = "")
  if (!is.null(configured) && nzchar(environment) &&
      !identical(path.expand(configured), path.expand(environment))) {
    stop("Conflicting DP ledger paths are configured.", call. = FALSE)
  }
  path <- if (nzchar(environment)) environment else configured
  if (is.null(path)) {
    path <- file.path(.dsomopStateRoot(), "privacy", "ledger.sqlite")
  }
  path <- .dsomopDpString(path, "ledger_path")
  path <- gsub("\\\\", "/", path.expand(path))
  root <- gsub("\\\\", "/", .dsomopStateRoot())
  expected_parent <- file.path(root, "privacy")
  if (!grepl("^/", path) || grepl("^//", path) ||
      grepl("(^|/)\\.{1,2}(/|$)", path) ||
      !identical(dirname(path), expected_parent) ||
      !grepl("^[A-Za-z0-9._-]+$", basename(path))) {
    stop("The DP ledger must be one canonical file directly inside the ",
         "persistent dsOMOP privacy directory.", call. = FALSE)
  }
  path
}

.dsomopDpNoiseSettings <- function() {
  provider <- tolower(.dsomopDpString(
    .dsomopDpOption("noise_provider", "auto"), "noise_provider"
  ))
  if (!provider %in% c("auto", "file", "injected")) {
    stop("dsomop.dp.noise_provider must be auto, file, or injected.",
         call. = FALSE)
  }
  env <- Sys.getenv("DSOMOP_DP_NOISE_ROOT", unset = "")
  opt <- .dsomopDpOption("noise_root", NULL)
  env_root <- if (nzchar(env)) .coerceDsomopDpRoot(env, "DP noise root") else NULL
  opt_root <- if (!is.null(opt)) {
    .coerceDsomopDpRoot(opt, "DP noise root")
  } else NULL
  if (!is.null(env_root) && !is.null(opt_root) &&
      !identical(env_root, opt_root)) {
    stop("Conflicting DP noise roots are configured.", call. = FALSE)
  }
  injected <- env_root %||% opt_root
  if (identical(provider, "file") && !is.null(injected)) {
    stop("The file DP noise provider conflicts with injected key material.",
         call. = FALSE)
  }
  if (identical(provider, "injected") && is.null(injected)) {
    stop("The injected DP noise provider requires DSOMOP_DP_NOISE_ROOT or ",
         "dsomop.dp.noise_root.", call. = FALSE)
  }
  require_existing <- .dsomopDpBoolean(
    .dsomopDpOption("noise_require_existing", FALSE),
    "noise_require_existing"
  )
  file_artifacts <- any(vapply(
    c("dp_noise_root", "dp_noise_root_receipt"), function(name) {
      path <- .dsomopSecretPath(name)
      file.exists(path) || .dsomopIsSymlink(path)
    }, logical(1L)
  ))
  list(
    provider = if (identical(provider, "auto")) {
      if (file_artifacts || is.null(injected)) "file" else "injected"
    } else provider,
    injected = injected,
    require_existing = require_existing
  )
}

.dsomopDpLedgerSettings <- function() {
  provider <- tolower(.dsomopDpString(
    .dsomopDpOption("ledger_provider", "auto"), "ledger_provider"
  ))
  if (!provider %in% c("auto", "file", "injected")) {
    stop("dsomop.dp.ledger_provider must be auto, file, or injected.",
         call. = FALSE)
  }
  env <- Sys.getenv("DSOMOP_DP_LEDGER_ROOT", unset = "")
  opt <- .dsomopDpOption("ledger_root", NULL)
  env_root <- if (nzchar(env)) {
    .coerceDsomopDpRoot(env, "DP ledger root")
  } else NULL
  opt_root <- if (!is.null(opt)) {
    .coerceDsomopDpRoot(opt, "DP ledger root")
  } else NULL
  if (!is.null(env_root) && !is.null(opt_root) &&
      !identical(env_root, opt_root)) {
    stop("Conflicting DP ledger roots are configured.", call. = FALSE)
  }
  injected <- env_root %||% opt_root
  if (provider == "file" && !is.null(injected)) {
    stop("The file DP ledger provider conflicts with injected key material.",
         call. = FALSE)
  }
  if (provider == "injected" && is.null(injected)) {
    stop("The injected DP ledger provider requires ",
         "DSOMOP_DP_LEDGER_ROOT or dsomop.dp.ledger_root.", call. = FALSE)
  }
  file_artifacts <- any(vapply(
    c("dp_ledger_root", "dp_ledger_root_receipt"), function(name) {
      path <- .dsomopSecretPath(name)
      file.exists(path) || .dsomopIsSymlink(path)
    }, logical(1L)
  ))
  list(
    provider = if (provider == "auto") {
      if (file_artifacts || is.null(injected)) "file" else "injected"
    } else provider,
    injected = injected,
    require_existing = .dsomopDpBoolean(
      .dsomopDpOption("ledger_require_existing", FALSE),
      "ledger_require_existing"
    )
  )
}

.dsomopDpRootId <- function(root) {
  paste0("dpk_", substr(.dsomopDpSha256(root), 1L, 40L))
}

.coerceDsomopDpRoot <- function(value, label) {
  if (is.raw(value) && length(value) == 32L) return(value)
  if (is.character(value) && length(value) == 1L && !is.na(value) &&
      grepl("^[0-9a-fA-F]{64}$", value)) {
    return(.coerceDsomopSecret(value, label))
  }
  stop("The ", label, " must be exactly 32 raw CSPRNG bytes or 64 ",
       "hexadecimal characters; passphrases are not accepted.",
       call. = FALSE)
}

.dsomopDpLedgerRootId <- function(root) {
  paste0("dpl_", substr(.dsomopDpSha256(root), 1L, 40L))
}

.dsomopDpLedgerArtifactsExist <- function(path = .dsomopDpLedgerPath()) {
  paths <- c(path, paste0(path, c("-journal", "-wal", "-shm", ".receipt")))
  any(vapply(paths, function(candidate) {
    if (!file.exists(candidate) && !.dsomopIsSymlink(candidate)) return(FALSE)
    info <- file.info(candidate)
    .dsomopIsSymlink(candidate) || nrow(info) != 1L || is.na(info$size[[1L]]) ||
      info$size[[1L]] > 0
  }, logical(1L)))
}

.dsomopDpNoiseRecoveryCandidate <- function(
    ledger_path, ledger_settings = .dsomopDpLedgerSettings()) {
  ledger_files <- c(ledger_path, .dsomopDpLedgerReceiptPath(ledger_path))
  if (!all(vapply(ledger_files, file.exists, logical(1L))) ||
      any(vapply(ledger_files, .dsomopIsSymlink, logical(1L)))) {
    return(FALSE)
  }
  if (identical(ledger_settings$provider, "injected")) {
    return(!is.null(ledger_settings$injected))
  }
  root_files <- vapply(
    c("dp_ledger_root", "dp_ledger_root_receipt"), .dsomopSecretPath,
    character(1L)
  )
  all(vapply(root_files, file.exists, logical(1L))) &&
    !any(vapply(root_files, .dsomopIsSymlink, logical(1L)))
}

.dsomopDpAuthenticatedLedgerEvidence <- function(policy) {
  path <- policy$ledger_path
  receipt_path <- .dsomopDpLedgerReceiptPath(path)
  if (!file.exists(path) || !file.exists(receipt_path) ||
      .dsomopIsSymlink(path) || .dsomopIsSymlink(receipt_path)) {
    return(FALSE)
  }
  value <- tryCatch({
    old_umask <- Sys.umask("0077")
    lock <- NULL
    connection <- NULL
    transaction <- FALSE
    on.exit({
      if (transaction && !is.null(connection)) {
        try(DBI::dbExecute(connection, "ROLLBACK"), silent = TRUE)
      }
      if (!is.null(connection)) {
        try(DBI::dbDisconnect(connection), silent = TRUE)
      }
      if (!is.null(lock)) try(filelock::unlock(lock), silent = TRUE)
      try(Sys.umask(old_umask), silent = TRUE)
    }, add = TRUE)
    lock_path <- paste0(path, ".lock")
    if (.dsomopIsSymlink(lock_path)) return(FALSE)
    lock <- filelock::lock(lock_path, timeout = 30000)
    if (is.null(lock)) return(FALSE)
    Sys.chmod(lock_path, mode = "0600")
    if (.dsomopIsSymlink(lock_path) || !.dsomopPrivateMode(lock_path) ||
        !identical(.dsomopLinkCount(lock_path), 1)) return(FALSE)
    if (!file.exists(path) || !file.exists(receipt_path) ||
        .dsomopIsSymlink(path) || .dsomopIsSymlink(receipt_path)) {
      return(FALSE)
    }
    .dsomopDpValidateLedgerArtifact(path)
    observed_receipt <- .dsomopValidateSecretFile(receipt_path)
    journal <- paste0(path, "-journal")
    if (file.exists(journal) || .dsomopIsSymlink(journal)) {
      .dsomopDpValidateLedgerArtifact(journal)
    }
    if (any(vapply(paste0(path, c("-wal", "-shm")), function(candidate) {
      file.exists(candidate) || .dsomopIsSymlink(candidate)
    }, logical(1L)))) return(FALSE)
    # A read-write connection is intentional here: under the ledger lock,
    # SQLite may need to recover a durable DELETE-mode hot journal left by a
    # crashed worker before continuity metadata can be read.
    connection <- DBI::dbConnect(
      RSQLite::SQLite(), path, synchronous = "full",
      flags = RSQLite::SQLITE_RW
    )
    DBI::dbExecute(connection, "PRAGMA busy_timeout = 30000")
    DBI::dbExecute(connection, "PRAGMA synchronous = FULL")
    DBI::dbExecute(connection, "PRAGMA fullfsync = ON")
    synchronous <- as.integer(DBI::dbGetQuery(
      connection, "PRAGMA synchronous"
    )[[1L]][[1L]])
    fullfsync <- as.integer(DBI::dbGetQuery(
      connection, "PRAGMA fullfsync"
    )[[1L]][[1L]])
    journal_mode <- tolower(DBI::dbGetQuery(
      connection, "PRAGMA journal_mode = DELETE"
    )[[1L]][[1L]])
    if (!identical(journal_mode, "delete") ||
        !identical(synchronous, 2L) || !identical(fullfsync, 1L)) {
      return(FALSE)
    }
    .dsomopDpValidateLedgerArtifact(path)
    tables <- DBI::dbListTables(connection)
    if (!setequal(tables, c("dp_meta", "dp_releases"))) return(FALSE)
    expected_receipt <- .dsomopDpExpectedLedgerReceipt(connection, policy)
    if (!identical(observed_receipt, expected_receipt)) return(FALSE)
    epoch <- suppressWarnings(as.numeric(
      .dsomopDpMetaGet(connection, "privacy_epoch")
    ))
    noise_key_id <- .dsomopDpMetaGet(connection, "current_noise_key_id")
    if (length(epoch) != 1L || !is.finite(epoch) || epoch < 1 ||
        epoch != floor(epoch) || policy$privacy_epoch < epoch ||
        !is.character(noise_key_id) || length(noise_key_id) != 1L ||
        !grepl("^dpk_[0-9a-f]{40}$", noise_key_id)) return(FALSE)
    validation_policy <- policy
    validation_policy$privacy_epoch <- epoch
    validation_policy$noise_root <- list(
      key_id = noise_key_id, provider = "file"
    )
    validation_policy$keys <- list(
      ledger = .dsomopDpSubkey(
        policy$ledger_root$key, policy$domain, "ledger-mac/v1"
      )
    )
    DBI::dbExecute(connection, "BEGIN IMMEDIATE")
    transaction <- TRUE
    state <- .dsomopDpValidateLedger(
      connection, validation_policy, cached = NULL,
      file_signature = .dsomopDpLedgerFileSignature(path)
    )
    capabilities <- .dsomopDpAnchorCapabilities(policy)
    if (capabilities$external) {
      anchored <- .dsomopDpAnchorState(
        .dsomopDpAnchorCall(policy, "read"), policy, allow_null = TRUE
      )
      if (is.null(anchored)) {
        if (state$next_index != 0) return(FALSE)
      } else {
        if (!identical(anchored$ledger_id, state$ledger_id) ||
            anchored$next_index > state$next_index) return(FALSE)
        local_head <- if (anchored$next_index == 0) {
          .DSOMOP_DP_GENESIS
        } else {
          row <- DBI::dbGetQuery(
            connection,
            "SELECT row_mac FROM dp_releases WHERE release_index = ?",
            params = list(anchored$next_index - 1)
          )
          if (nrow(row) != 1L) return(FALSE)
          row$row_mac[[1L]]
        }
        if (!identical(anchored$chain_head, local_head)) return(FALSE)
      }
    }
    DBI::dbExecute(connection, "ROLLBACK")
    transaction <- FALSE
    TRUE
  }, error = function(e) FALSE)
  isTRUE(value)
}

.dsomopDpFileRoot <- function(settings = .dsomopDpNoiseSettings(),
                              recovery_authorized = FALSE) {
  root_path <- .dsomopSecretPath("dp_noise_root")
  root_exists <- file.exists(root_path) || .dsomopIsSymlink(root_path)
  if (!root_exists && isTRUE(settings$require_existing) &&
      !isTRUE(recovery_authorized)) {
    stop("The configured DP noise provider requires an existing root; ",
         "refusing to generate replacement noise material.", call. = FALSE)
  }
  root_path <- .dsomopPrivateSecretDirectory(
    root_path,
    .allow_test_path = identical(
      Sys.getenv("DSOMOP_TEST_ALLOW_EPHEMERAL_STATE", unset = ""), "1")
  )
  receipt_path <- .dsomopSecretPath("dp_noise_root_receipt")
  recovery_lock_path <- paste0(root_path, ".recovery.lock")
  if (.dsomopIsSymlink(recovery_lock_path)) {
    stop("The DP noise-root recovery lock must not be a symbolic link.",
         call. = FALSE)
  }
  recovery_lock <- filelock::lock(recovery_lock_path, timeout = 30000)
  if (is.null(recovery_lock)) {
    stop("The DP noise-root recovery lock is unavailable.", call. = FALSE)
  }
  on.exit(try(filelock::unlock(recovery_lock), silent = TRUE), add = TRUE)
  Sys.chmod(recovery_lock_path, mode = "0600")
  if (.dsomopIsSymlink(recovery_lock_path) ||
      !.dsomopPrivateMode(recovery_lock_path) ||
      !identical(.dsomopLinkCount(recovery_lock_path), 1)) {
    stop("The DP noise-root recovery lock is not private.", call. = FALSE)
  }
  # Re-evaluate both artifacts under one outer lock. If the replaceable root was
  # lost, retire its old receipt before committing a new root. A crash at any
  # later point leaves either no receipt or the new matching receipt, so the
  # next bootstrap can complete recovery without an ambiguous mismatch window.
  root_exists <- file.exists(root_path) || .dsomopIsSymlink(root_path)
  receipt_exists <- file.exists(receipt_path) ||
    .dsomopIsSymlink(receipt_path)
  ledger_exists <- .dsomopDpLedgerArtifactsExist()
  # `require_existing` can require initial secret provisioning, but it must not
  # strand a deployed service after accidental noise-root loss. Authenticated
  # ledger continuity distinguishes recovery from an
  # unprovisioned first bootstrap when initial provisioning was mandatory.
  recover_deployed_root <- !root_exists && isTRUE(recovery_authorized)
  if (!root_exists && isTRUE(settings$require_existing) &&
      !recover_deployed_root) {
    stop("The configured DP noise provider requires an existing root; ",
         "refusing to generate replacement noise material.", call. = FALSE)
  }
  recovered <- !root_exists &&
    (receipt_exists || ledger_exists)
  if (!root_exists && receipt_exists) {
    .dsomopValidateSecretFile(receipt_path)
    removed <- unlink(receipt_path, force = TRUE)
    if (!identical(as.integer(removed), 0L) || file.exists(receipt_path) ||
        .dsomopIsSymlink(receipt_path)) {
      stop("The obsolete DP noise-root receipt could not be retired.",
           call. = FALSE)
    }
    .dsomopRequireSync(dirname(receipt_path))
    receipt_exists <- FALSE
  }
  root <- .ensureDsomopSecret(
    "dp_noise_root", path = root_path,
    require_existing = isTRUE(settings$require_existing) &&
      !recover_deployed_root
  )
  expected_receipt <- .dsomopDpHmacRaw(
    root, "dsOMOP/dp/noise-root-continuity-receipt/v1"
  )
  .dsomopDpEnsurePrivate32(
    receipt_path, expected_receipt,
    replace = FALSE
  )
  list(key = root, recovered = recovered)
}

.dsomopDpLedgerRoot <- function(settings = .dsomopDpLedgerSettings()) {
  if (settings$provider == "injected") {
    artifacts <- vapply(
      c("dp_ledger_root", "dp_ledger_root_receipt"),
      function(name) {
        path <- .dsomopSecretPath(name)
        file.exists(path) || .dsomopIsSymlink(path)
      }, logical(1L)
    )
    if (any(artifacts)) {
      stop("Injected DP ledger material conflicts with file-backed state.",
           call. = FALSE)
    }
    root <- settings$injected
    return(list(
      key = root, key_id = .dsomopDpLedgerRootId(root),
      provider = "injected", material_exposed = FALSE
    ))
  }
  root_path <- .dsomopSecretPath("dp_ledger_root")
  receipt_path <- .dsomopSecretPath("dp_ledger_root_receipt")
  root_exists <- file.exists(root_path) || .dsomopIsSymlink(root_path)
  receipt_exists <- file.exists(receipt_path) ||
    .dsomopIsSymlink(receipt_path)
  if (!root_exists && receipt_exists) {
    stop("The DP ledger authentication root is missing while its continuity ",
         "receipt exists; restore that continuity-critical root.",
         call. = FALSE)
  }
  if (!root_exists && .dsomopDpLedgerArtifactsExist()) {
    stop("The DP ledger authentication root is missing while release state ",
         "exists; restore that continuity-critical root.", call. = FALSE)
  }
  root <- .ensureDsomopSecret(
    "dp_ledger_root", require_existing = settings$require_existing
  )
  expected_receipt <- .dsomopDpHmacRaw(
    root, "dsOMOP/dp/ledger-root-continuity-receipt/v1"
  )
  .dsomopDpEnsurePrivate32(receipt_path, expected_receipt)
  list(
    key = root, key_id = .dsomopDpLedgerRootId(root),
    provider = settings$provider, material_exposed = FALSE
  )
}

.dsomopDpNoiseRoot <- function(settings = .dsomopDpNoiseSettings(),
                               recovery_authorized = FALSE) {
  root_state <- if (identical(settings$provider, "injected")) {
    file_artifacts <- vapply(
      c("dp_noise_root", "dp_noise_root_receipt"),
      function(name) {
        path <- .dsomopSecretPath(name)
        file.exists(path) || .dsomopIsSymlink(path)
      }, logical(1L)
    )
    if (any(file_artifacts)) {
      stop("Injected DP noise material conflicts with file-backed DP state.",
           call. = FALSE)
    }
    list(key = settings$injected, recovered = FALSE)
  } else {
    .dsomopDpFileRoot(settings, recovery_authorized = recovery_authorized)
  }
  list(
    key = root_state$key,
    key_id = .dsomopDpRootId(root_state$key),
    provider = settings$provider,
    recovered = isTRUE(root_state$recovered),
    material_exposed = FALSE
  )
}

.dsomopDpRefreshFileNoisePolicy <- function(policy) {
  if (!identical(policy$noise_root$provider, "file")) return(policy)
  root_path <- .dsomopSecretPath("dp_noise_root")
  receipt_path <- .dsomopSecretPath("dp_noise_root_receipt")
  recovery_lock_path <- paste0(root_path, ".recovery.lock")
  if (.dsomopIsSymlink(recovery_lock_path)) {
    stop("The DP noise-root recovery lock must not be a symbolic link.",
         call. = FALSE)
  }
  recovery_lock <- filelock::lock(recovery_lock_path, timeout = 30000)
  if (is.null(recovery_lock)) {
    stop("The DP noise-root recovery lock is unavailable.", call. = FALSE)
  }
  on.exit(try(filelock::unlock(recovery_lock), silent = TRUE), add = TRUE)
  Sys.chmod(recovery_lock_path, mode = "0600")
  if (.dsomopIsSymlink(recovery_lock_path) ||
      !.dsomopPrivateMode(recovery_lock_path) ||
      !identical(.dsomopLinkCount(recovery_lock_path), 1)) {
    stop("The DP noise-root recovery lock is not private.", call. = FALSE)
  }
  root <- .dsomopValidateSecretFile(root_path)
  observed_receipt <- .dsomopValidateSecretFile(receipt_path)
  expected_receipt <- .dsomopDpHmacRaw(
    root, "dsOMOP/dp/noise-root-continuity-receipt/v1"
  )
  if (!identical(observed_receipt, expected_receipt)) {
    stop("The private DP continuity receipt does not match persistent state.",
         call. = FALSE)
  }
  key_id <- .dsomopDpRootId(root)
  if (!identical(key_id, policy$noise_root$key_id)) {
    policy$noise_root$key <- root
    policy$noise_root$key_id <- key_id
    policy$noise_root$recovered <- TRUE
    policy$keys$noise <- .dsomopDpSubkey(root, policy$domain, "noise/v1")
  }
  policy
}

.dsomopDpSubkey <- function(root, domain, purpose) {
  .dsomopDpHmacRaw(root, .dsomopDpCanonicalJson(list(
    protocol = "dsomop-dp-subkey-v1", domain = domain, purpose = purpose
  )))
}

.dsomopDpAnchorCall <- function(policy, action, ...) {
  provider <- policy$anchor_provider
  if (!is.function(provider)) {
    stop("The external DP rollback anchor is unavailable.", call. = FALSE)
  }
  tryCatch(
    do.call(provider, c(list(action = action,
                             anchor_id = policy$anchor_id), list(...))),
    error = function(e) stop(
      "The external DP rollback anchor failed during '", action, "'.",
      call. = FALSE
    )
  )
}

.dsomopDpAnchorCapabilities <- function(policy) {
  if (is.null(policy$anchor_provider)) {
    return(list(
      schema_version = 1L, provider_id = "none", external = FALSE,
      durable = FALSE, linearizable_cas = FALSE
    ))
  }
  value <- .dsomopDpAnchorCall(policy, "capabilities")
  expected <- c("schema_version", "provider_id", "external", "durable",
                "linearizable_cas")
  valid <- is.list(value) && !is.null(names(value)) &&
    !anyNA(names(value)) && !anyDuplicated(names(value)) &&
    setequal(names(value), expected) &&
    identical(as.numeric(value$schema_version), 1) &&
    is.character(value$provider_id) && length(value$provider_id) == 1L &&
    !is.na(value$provider_id) && nzchar(value$provider_id) &&
    nchar(value$provider_id, type = "bytes") <= 128L &&
    identical(value$external, TRUE) && identical(value$durable, TRUE) &&
    identical(value$linearizable_cas, TRUE)
  if (!isTRUE(valid)) {
    stop("The DP rollback anchor does not attest an external, durable, ",
         "linearizable-CAS contract.", call. = FALSE)
  }
  value
}

.dsomopDpPolicy <- function(require_enabled = TRUE) {
  # Last-resort lifecycle guard: every path that can resolve DP roots passes
  # through this constructor. Service entry points initialize explicitly, but
  # this prevents a future internal DP caller from accidentally bypassing the
  # durable bootstrap contract.
  if (is.null(.pkg_state$dp_bootstrap_binding) &&
      !isTRUE(.pkg_state$dp_bootstrap_in_progress)) {
    .dsomopDpEnsureRuntime()
  }
  enabled <- .dsomopDpEnabled()
  if (isTRUE(require_enabled) && !enabled) {
    stop("Differential-privacy releases are disabled by the data custodian.",
         call. = FALSE)
  }
  if (!enabled) {
    return(list(
      enabled = FALSE, protocol = .DSOMOP_DP_PROTOCOL,
      mechanism = .DSOMOP_DP_MECHANISM
    ))
  }
  total_epsilon <- .dsomopDpNumber(
    .dsomopDpOption("total_epsilon", 1), "total_epsilon", 0, 8,
    lower_open = TRUE
  )
  release_epsilon <- .dsomopDpNumber(
    .dsomopDpOption("release_epsilon", 0.1), "release_epsilon", 1e-6, 8
  )
  if (release_epsilon > total_epsilon) {
    stop("dsomop.dp.release_epsilon cannot exceed total_epsilon.",
         call. = FALSE)
  }
  accounting_mode <- tolower(.dsomopDpString(
    .dsomopDpOption("accounting_mode", "bounded_accounted"),
    "accounting_mode"
  ))
  if (!accounting_mode %in% c("bounded_accounted", "sticky_unbounded")) {
    stop("dsomop.dp.accounting_mode must be bounded_accounted or ",
         "sticky_unbounded.", call. = FALSE)
  }
  epoch <- .dsomopDpNumber(
    .dsomopDpOption("privacy_epoch", 1), "privacy_epoch", 1,
    .Machine$integer.max, integer = TRUE
  )
  domain <- .dsomopDpString(.dsomopDpOption("domain", ""), "domain")
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._:-]{0,127}$", domain)) {
    stop("dsomop.dp.domain contains unsupported characters.", call. = FALSE)
  }
  snapshot_id <- .dsomopDpString(
    .dsomopDpOption("snapshot_id", ""), "snapshot_id"
  )
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._:@+-]{0,255}$", snapshot_id)) {
    stop("dsomop.dp.snapshot_id contains unsupported characters.",
         call. = FALSE)
  }
  require_anchor <- .dsomopDpBoolean(
    .dsomopDpOption("require_external_anchor", TRUE),
    "require_external_anchor"
  )
  anchor_provider <- .dsomopDpAnchorProvider()
  if (!is.null(anchor_provider) && !is.function(anchor_provider)) {
    stop("dsomop.dp.anchor_provider must be a provider function.",
         call. = FALSE)
  }
  if (require_anchor && is.null(anchor_provider)) {
    stop("The DP ledger is configured to require an external rollback anchor, ",
         "but dsomop.dp.anchor_provider is unavailable.", call. = FALSE)
  }
  policy <- list(
    enabled = TRUE,
    schema_version = 1L,
    protocol = .DSOMOP_DP_PROTOCOL,
    canonical_protocol = .DSOMOP_DP_CANONICAL_PROTOCOL,
    mechanism = .DSOMOP_DP_MECHANISM,
    sampler = .DSOMOP_DP_SAMPLER,
    min_useful_epsilon = .DSOMOP_DP_MIN_USEFUL_EPSILON,
    adjacency = "add_remove_person",
    domain = domain,
    snapshot_id = snapshot_id,
    total_epsilon = total_epsilon,
    total_delta = 0,
    release_epsilon = release_epsilon,
    release_delta = 0,
    accounting_mode = accounting_mode,
    allocator = if (accounting_mode == "bounded_accounted") {
      "normalized_capped_zeta2_no_block_nominal_v1"
    } else {
      "fixed_epsilon_unbounded_composition_v1"
    },
    privacy_epoch = as.numeric(epoch),
    max_levels = .dsomopDpNumber(
      .dsomopDpOption("max_levels", 1000), "max_levels", 1, 100000,
      integer = TRUE
    ),
    max_contributions = .dsomopDpNumber(
      .dsomopDpOption("max_contributions", 10), "max_contributions", 1,
      10000, integer = TRUE
    ),
    numeric_grid = .dsomopDpNumber(
      .dsomopDpOption("numeric_grid", 65535), "numeric_grid", 255,
      2^24 - 1, integer = TRUE
    ),
    ledger_path = .dsomopDpLedgerPath(),
    require_external_anchor = require_anchor,
    anchor_provider = anchor_provider
  )
  policy$allocation_scale <- if (accounting_mode == "bounded_accounted") {
    .dsomopDpZetaScale(total_epsilon, release_epsilon)
  } else {
    release_epsilon
  }
  policy$anchor_id <- paste0("dpa_", substr(.dsomopDpSha256(
    .dsomopDpCanonicalJson(list(
      protocol = "dsomop-dp-anchor-id-v1", domain = domain
    ))
  ), 1L, 40L))
  policy$policy_hash <- .dsomopDpSha256(.dsomopDpCanonicalJson(list(
    schema_version = policy$schema_version,
    protocol = policy$protocol,
    canonical_protocol = policy$canonical_protocol,
    lookup_protocol = .DSOMOP_DP_LOOKUP_PROTOCOL,
    mechanism = policy$mechanism,
    sampler = policy$sampler,
    # Keep the v1 fingerprint byte-for-byte compatible with durable ledgers
    # created before public certification flags were removed. This literal is
    # neither configurable nor exposed by the runtime API.
    sampler_certified = FALSE,
    min_useful_epsilon = policy$min_useful_epsilon,
    adjacency = policy$adjacency,
    domain = policy$domain,
    total_epsilon = policy$total_epsilon,
    total_delta = policy$total_delta,
    release_epsilon = policy$release_epsilon,
    release_delta = policy$release_delta,
    accounting_mode = policy$accounting_mode,
    allocator = policy$allocator,
    max_levels = policy$max_levels,
    max_contributions = policy$max_contributions,
    numeric_grid = policy$numeric_grid,
    require_external_anchor = policy$require_external_anchor
  )))
  .dsomopDpAssertBootstrapPolicy(policy)
  noise_settings <- .dsomopDpNoiseSettings()
  ledger_settings <- .dsomopDpLedgerSettings()
  noise_missing <- identical(noise_settings$provider, "file") &&
    !file.exists(.dsomopSecretPath("dp_noise_root")) &&
    !.dsomopIsSymlink(.dsomopSecretPath("dp_noise_root"))
  ledger_present <- .dsomopDpLedgerArtifactsExist(policy$ledger_path)
  recovery_candidate <- noise_missing && ledger_present &&
    .dsomopDpNoiseRecoveryCandidate(policy$ledger_path, ledger_settings)
  if (noise_missing && ledger_present && !recovery_candidate) {
    stop("Existing DP ledger artifacts are insufficient to authenticate ",
         "noise-root recovery; restore durable continuity state.",
         call. = FALSE)
  }
  if (noise_missing && isTRUE(noise_settings$require_existing) &&
      !recovery_candidate) {
    stop("The configured DP noise provider requires an existing root; ",
         "refusing to generate replacement noise material.", call. = FALSE)
  }
  if (!ledger_present && !is.null(policy$anchor_provider)) {
    .dsomopDpAnchorCapabilities(policy)
    anchored <- .dsomopDpAnchorState(
      .dsomopDpAnchorCall(policy, "read"), policy, allow_null = TRUE
    )
    if (!is.null(anchored)) {
      stop("An external DP anchor exists while local release state is ",
           "missing; restore the ledger before creating key material.",
           call. = FALSE)
    }
  }
  policy$ledger_root <- .dsomopDpLedgerRoot(ledger_settings)
  recovery_authorized <- recovery_candidate &&
    .dsomopDpAuthenticatedLedgerEvidence(policy)
  if (recovery_candidate && !recovery_authorized) {
    stop("The existing DP ledger does not authenticate noise-root recovery; ",
         "restore the previous root or ledger continuity state.",
         call. = FALSE)
  }
  policy$noise_root <- .dsomopDpNoiseRoot(
    noise_settings, recovery_authorized = recovery_authorized
  )
  policy$keys <- list(
    ledger = .dsomopDpSubkey(policy$ledger_root$key, domain, "ledger-mac/v1"),
    lookup = .dsomopDpSubkey(policy$ledger_root$key, domain,
                             "release-lookup/v1"),
    protected = .dsomopDpSubkey(policy$ledger_root$key, domain,
                                "protected-fingerprint/v1"),
    provenance = .dsomopDpSubkey(policy$ledger_root$key, domain,
                                "person-local-provenance/v1"),
    query = .dsomopDpSubkey(policy$ledger_root$key, domain,
                           "semantic-query-id/v1"),
    noise = .dsomopDpSubkey(policy$noise_root$key, domain, "noise/v1")
  )
  policy
}

.dsomopDpMetaGet <- function(connection, key) {
  row <- DBI::dbGetQuery(
    connection, "SELECT value FROM dp_meta WHERE key = ?", params = list(key)
  )
  if (nrow(row) == 0L) return(NULL)
  if (nrow(row) != 1L || !is.character(row$value)) {
    stop("The DP ledger metadata is corrupt.", call. = FALSE)
  }
  row$value[[1L]]
}

.dsomopDpMetaSet <- function(connection, key, value) {
  DBI::dbExecute(
    connection,
    paste0("INSERT INTO dp_meta(key, value) VALUES(?, ?) ",
           "ON CONFLICT(key) DO UPDATE SET value = excluded.value"),
    params = list(key, as.character(value))
  )
  invisible(NULL)
}

.dsomopDpLedgerReceiptPath <- function(path) paste0(path, ".receipt")

.dsomopDpExpectedLedgerReceipt <- function(connection, policy) {
  values <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT key, value FROM dp_meta WHERE key IN",
      "('ledger_id', 'policy_hash', 'ledger_key_id')"
    )
  )
  if (nrow(values) != 3L || anyDuplicated(values$key)) {
    stop("The DP ledger identity metadata is incomplete.", call. = FALSE)
  }
  meta <- stats::setNames(values$value, values$key)
  if (!grepl("^[0-9a-f]{64}$", meta[["ledger_id"]]) ||
      !identical(meta[["policy_hash"]], policy$policy_hash) ||
      !identical(meta[["ledger_key_id"]], policy$ledger_root$key_id)) {
    stop("The DP ledger identity metadata is invalid.", call. = FALSE)
  }
  ledger_key <- .dsomopDpSubkey(
    policy$ledger_root$key, policy$domain, "ledger-mac/v1"
  )
  .dsomopDpHmacRaw(
    ledger_key,
    .dsomopDpCanonicalJson(list(
      protocol = "dsomop-dp-ledger-continuity-receipt-v1",
      ledger_id = meta[["ledger_id"]],
      policy_hash = policy$policy_hash,
      ledger_key_id = policy$ledger_root$key_id
    ))
  )
}

.dsomopDpEnsurePrivate32 <- function(path, expected, replace = FALSE,
                                     .sync = .dsomopSyncFile) {
  if (!is.raw(expected) || length(expected) != 32L) {
    stop("A private DP receipt must contain 32 bytes.", call. = FALSE)
  }
  if (!is.logical(replace) || length(replace) != 1L || is.na(replace)) {
    stop("The private DP receipt replacement policy is invalid.",
         call. = FALSE)
  }
  path <- .dsomopPrivateSecretDirectory(
    path,
    .allow_test_path = identical(
      Sys.getenv("DSOMOP_TEST_ALLOW_EPHEMERAL_STATE", unset = ""), "1")
  )
  lock_path <- paste0(path, ".lock")
  if (.dsomopIsSymlink(lock_path)) {
    stop("The private DP receipt lock must not be a symbolic link.",
         call. = FALSE)
  }
  lock <- filelock::lock(lock_path, timeout = 30000)
  if (is.null(lock)) stop("The private DP receipt lock is unavailable.",
                          call. = FALSE)
  on.exit(try(filelock::unlock(lock), silent = TRUE), add = TRUE)
  Sys.chmod(lock_path, mode = "0600")
  if (.dsomopIsSymlink(lock_path) || !.dsomopPrivateMode(lock_path) ||
      !identical(.dsomopLinkCount(lock_path), 1)) {
    stop("The private DP receipt lock is not owner-only.", call. = FALSE)
  }
  if (file.exists(path) || .dsomopIsSymlink(path)) {
    observed <- .dsomopValidateSecretFile(path)
    if (identical(observed, expected)) return(invisible(path))
    if (!replace) {
      stop("The private DP continuity receipt does not match persistent state.",
           call. = FALSE)
    }
    removed <- unlink(path, force = TRUE)
    if (!identical(as.integer(removed), 0L) || file.exists(path) ||
        .dsomopIsSymlink(path)) {
      stop("The obsolete DP noise-root receipt could not be replaced.",
           call. = FALSE)
    }
    .dsomopRequireSync(dirname(path), .sync)
  }
  temporary <- tempfile(
    pattern = paste0(".", basename(path), ".", Sys.getpid(), "."),
    tmpdir = dirname(path)
  )
  on.exit(if (file.exists(temporary)) unlink(temporary, force = TRUE), add = TRUE)
  connection <- file(temporary, open = "wb")
  on.exit(try(if (isOpen(connection)) close(connection), silent = TRUE),
          add = TRUE)
  writeBin(expected, connection)
  flush(connection)
  close(connection)
  Sys.chmod(temporary, mode = "0600")
  .dsomopValidateSecretFile(temporary)
  .dsomopRequireSync(temporary, .sync)
  value <- .dsomopCommitSecretNoClobber(temporary, path)
  if (!identical(value, expected)) {
    stop("A competing DP receipt does not match persistent state.",
         call. = FALSE)
  }
  .dsomopRequireSync(dirname(path), .sync)
  invisible(path)
}

.dsomopDpRowMac <- function(policy, fields) {
  .dsomopDpHmac(policy$keys$ledger, .dsomopDpCanonicalJson(fields))
}

.dsomopDpReleaseFields <- function(row) {
  list(
    release_index = as.numeric(row$release_index[[1L]]),
    release_id = row$release_id[[1L]],
    semantic_query_id = row$semantic_query_id[[1L]],
    snapshot_id = row$snapshot_id[[1L]],
    protected_fingerprint = row$protected_fingerprint[[1L]],
    mechanism = row$mechanism[[1L]],
    epsilon = as.numeric(row$epsilon[[1L]]),
    delta = as.numeric(row$delta[[1L]]),
    sensitivity = row$sensitivity[[1L]],
    privacy_epoch = as.numeric(row$privacy_epoch[[1L]]),
    noise_key_id = row$noise_key_id[[1L]],
    payload = row$payload[[1L]],
    previous_chain = row$previous_chain[[1L]]
  )
}

.dsomopDpAllocation <- function(policy, release_index) {
  if (!is.numeric(release_index) || length(release_index) != 1L ||
      !is.finite(release_index) || release_index < 0 ||
      release_index != floor(release_index)) {
    stop("The DP release allocator received an invalid index.", call. = FALSE)
  }
  if (policy$accounting_mode == "sticky_unbounded") {
    return(list(epsilon = policy$release_epsilon, degraded = FALSE))
  }
  # The server solves for a zeta scale whose infinite capped series sums to the
  # configured total. Once finite-precision sampling would become unreliable,
  # the endpoint returns a fixed data-independent payload at epsilon zero.
  epsilon <- min(
    policy$release_epsilon,
    policy$allocation_scale / (release_index + 1)^2
  )
  if (epsilon < policy$min_useful_epsilon) {
    return(list(epsilon = 0, degraded = TRUE))
  }
  list(epsilon = epsilon, degraded = FALSE)
}

.dsomopDpValidateLedger <- function(connection, policy, cached = NULL,
                                    file_signature = NULL,
                                    apply_updates = TRUE) {
  required_meta <- c(
    "schema_version", "ledger_id", "policy_hash", "ledger_key_id",
    "current_noise_key_id", "noise_generation", "privacy_epoch",
    "next_index", "spent_epsilon", "spent_delta", "chain_head"
  )
  values <- stats::setNames(lapply(required_meta, function(key) {
    .dsomopDpMetaGet(connection, key)
  }), required_meta)
  if (any(vapply(values, is.null, logical(1L)))) {
    stop("The DP ledger metadata is incomplete.", call. = FALSE)
  }
  if (!identical(values$schema_version,
                 as.character(.DSOMOP_DP_LEDGER_SCHEMA)) ||
      !grepl("^[0-9a-f]{64}$", values$ledger_id) ||
      !identical(values$policy_hash, policy$policy_hash) ||
      !identical(values$ledger_key_id, policy$ledger_root$key_id) ||
      !grepl("^dpk_[0-9a-f]{40}$", values$current_noise_key_id)) {
    stop("The DP ledger policy, schema, or authentication-root binding changed.",
         call. = FALSE)
  }
  epoch <- suppressWarnings(as.numeric(values$privacy_epoch))
  noise_generation <- suppressWarnings(as.numeric(values$noise_generation))
  next_index <- suppressWarnings(as.numeric(values$next_index))
  spent_epsilon <- suppressWarnings(as.numeric(values$spent_epsilon))
  spent_delta <- suppressWarnings(as.numeric(values$spent_delta))
  valid_numbers <- is.finite(epoch) && epoch >= 1 && epoch == floor(epoch) &&
    is.finite(noise_generation) && noise_generation >= 1 &&
    noise_generation == floor(noise_generation) &&
    is.finite(next_index) && next_index >= 0 && next_index == floor(next_index) &&
    is.finite(spent_epsilon) && spent_epsilon >= 0 &&
    is.finite(spent_delta) && spent_delta >= 0
  if (!valid_numbers || policy$privacy_epoch < epoch) {
    stop("The DP ledger counters or privacy epoch are invalid.",
         call. = FALSE)
  }
  cache_matches <- is.list(cached) &&
    identical(cached$ledger_id, values$ledger_id) &&
    identical(cached$policy_hash, values$policy_hash) &&
    identical(cached$ledger_key_id, values$ledger_key_id) &&
    is.numeric(cached$next_index) && length(cached$next_index) == 1L &&
    is.finite(cached$next_index) && cached$next_index >= 0 &&
    cached$next_index == floor(cached$next_index) &&
    is.character(cached$chain_head) && length(cached$chain_head) == 1L &&
    is.numeric(cached$spent_epsilon) &&
    length(cached$spent_epsilon) == 1L &&
    is.finite(cached$spent_epsilon) && cached$spent_epsilon >= 0 &&
    is.numeric(cached$spent_delta) && length(cached$spent_delta) == 1L &&
    is.finite(cached$spent_delta) && cached$spent_delta >= 0 &&
    is.numeric(cached$privacy_epoch) && length(cached$privacy_epoch) == 1L &&
    is.numeric(cached$noise_generation) &&
    length(cached$noise_generation) == 1L
  if (cache_matches && next_index < cached$next_index) {
    stop("The authenticated DP ledger rolled back behind the process checkpoint.",
         call. = FALSE)
  }
  if (cache_matches &&
      (epoch < cached$privacy_epoch ||
       noise_generation < cached$noise_generation)) {
    stop("The authenticated DP ledger generation counters rolled back.",
         call. = FALSE)
  }
  if (cache_matches && !is.null(cached$file_signature) &&
      !identical(cached$file_signature, file_signature)) {
    appended <- next_index - cached$next_index
    old_counter <- cached$file_signature$change_counter %||% NA_real_
    new_counter <- file_signature$change_counter %||% NA_real_
    counter_delta <- if (is.finite(old_counter) && is.finite(new_counter)) {
      (new_counter - old_counter) %% 2^32
    } else Inf
    # One committed append transaction increments SQLite's header counter once.
    # Any other change at the same index, or extra transactions beyond the
    # append-only suffix, invalidates the checkpoint and triggers a full audit.
    if (appended == 0 || counter_delta != appended) cache_matches <- FALSE
  }
  start_index <- if (cache_matches) cached$next_index else 0
  outside <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT release_index FROM dp_releases",
      "WHERE release_index < 0 OR release_index >= ? LIMIT 1"
    ),
    params = list(next_index)
  )
  if (nrow(outside) != 0L) {
    stop("The authenticated DP release chain is incomplete.", call. = FALSE)
  }
  previous <- if (cache_matches) cached$chain_head else .DSOMOP_DP_GENESIS
  epsilon_sum <- if (cache_matches) cached$spent_epsilon else 0
  delta_sum <- if (cache_matches) cached$spent_delta else 0
  cursor <- start_index
  while (cursor < next_index) {
    upper <- min(next_index, cursor + .DSOMOP_DP_LEDGER_AUDIT_CHUNK)
    rows <- DBI::dbGetQuery(
      connection,
      paste(
        "SELECT release_id, release_index, semantic_query_id, snapshot_id,",
        "protected_fingerprint, mechanism, epsilon, delta, sensitivity,",
        "privacy_epoch, noise_key_id, payload, previous_chain, row_mac",
        "FROM dp_releases WHERE release_index >= ? AND release_index < ?",
        "ORDER BY release_index"
      ),
      params = list(cursor, upper)
    )
    if (nrow(rows) != upper - cursor) {
      stop("The authenticated DP release chain is incomplete.", call. = FALSE)
    }
    for (index in seq_len(nrow(rows))) {
      row <- rows[index, , drop = FALSE]
      fields <- .dsomopDpReleaseFields(row)
      expected_index <- cursor + index - 1L
      valid <- identical(as.numeric(fields$release_index),
                         as.numeric(expected_index)) &&
        identical(fields$previous_chain, previous) &&
        grepl("^[0-9a-f]{64}$", fields$release_id) &&
        grepl("^[0-9a-f]{64}$", fields$semantic_query_id) &&
        grepl("^[0-9a-f]{64}$", fields$snapshot_id) &&
        grepl("^[0-9a-f]{64}$", fields$protected_fingerprint) &&
        is.finite(fields$privacy_epoch) && fields$privacy_epoch >= 1 &&
        fields$privacy_epoch == floor(fields$privacy_epoch) &&
        fields$privacy_epoch <= policy$privacy_epoch &&
        is.finite(fields$epsilon) && fields$epsilon >= 0 &&
        abs(fields$epsilon -
              .dsomopDpAllocation(policy, expected_index)$epsilon) <= 1e-12 &&
        identical(fields$delta, 0) &&
        grepl("^dpk_[0-9a-f]{40}$", fields$noise_key_id) &&
        grepl("^[0-9a-f]{64}$", row$row_mac[[1L]]) &&
        identical(row$row_mac[[1L]], .dsomopDpRowMac(policy, fields))
      if (!isTRUE(valid)) {
        stop("The authenticated DP release chain is corrupt.", call. = FALSE)
      }
      previous <- row$row_mac[[1L]]
      epsilon_sum <- epsilon_sum + fields$epsilon
      delta_sum <- delta_sum + fields$delta
    }
    cursor <- upper
  }
  if (!identical(values$chain_head, previous) ||
      abs(spent_epsilon - epsilon_sum) > 1e-12 ||
      abs(spent_delta - delta_sum) > 1e-18 ||
      (policy$accounting_mode == "bounded_accounted" &&
       spent_epsilon > policy$total_epsilon + 1e-12) ||
      spent_delta > policy$total_delta + 1e-18) {
    stop("The DP ledger accountant or chain head is inconsistent.",
         call. = FALSE)
  }
  epoch_update <- policy$privacy_epoch > epoch
  if (epoch_update) {
    if (isTRUE(apply_updates)) {
      .dsomopDpMetaSet(connection, "privacy_epoch", policy$privacy_epoch)
    }
    epoch <- policy$privacy_epoch
  }
  noise_update <- !identical(
    values$current_noise_key_id, policy$noise_root$key_id
  )
  if (noise_update) {
    if (isTRUE(apply_updates)) {
      .dsomopDpMetaSet(connection, "current_noise_key_id",
                       policy$noise_root$key_id)
    }
    noise_generation <- noise_generation + 1
    if (isTRUE(apply_updates)) {
      .dsomopDpMetaSet(connection, "noise_generation", noise_generation)
    }
  }
  list(
    ledger_id = values$ledger_id,
    policy_hash = values$policy_hash,
    ledger_key_id = values$ledger_key_id,
    privacy_epoch = epoch,
    noise_generation = noise_generation,
    next_index = next_index,
    spent_epsilon = spent_epsilon,
    spent_delta = spent_delta,
    chain_head = previous,
    file_signature = file_signature,
    pending_epoch_update = epoch_update && !isTRUE(apply_updates),
    pending_noise_update = noise_update && !isTRUE(apply_updates)
  )
}

.dsomopDpLedgerFileSignature <- function(path) {
  info <- file.info(path)
  if (nrow(info) != 1L || is.na(info$size[[1L]]) || is.na(info$mtime[[1L]])) {
    stop("The DP ledger file signature is unavailable.", call. = FALSE)
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  seek(connection, where = 24L, origin = "start")
  bytes <- readBin(connection, what = "raw", n = 4L)
  if (length(bytes) != 4L) {
    stop("The SQLite ledger change counter is unavailable.", call. = FALSE)
  }
  counter <- sum(as.numeric(as.integer(bytes)) * 256^(3:0))
  list(
    size = as.numeric(info$size[[1L]]),
    mtime = as.numeric(info$mtime[[1L]]),
    change_counter = counter
  )
}

.dsomopDpLedgerCacheKey <- function(path, policy) {
  paste(path, policy$policy_hash, policy$ledger_root$key_id, sep = "\u001f")
}

.dsomopDpLedgerCacheGet <- function(path, policy) {
  key <- .dsomopDpLedgerCacheKey(path, policy)
  if (!exists(key, envir = .pkg_state$dp_ledger_cache, inherits = FALSE)) {
    return(NULL)
  }
  get(key, envir = .pkg_state$dp_ledger_cache, inherits = FALSE)
}

.dsomopDpLedgerCacheSet <- function(path, policy, state) {
  assign(.dsomopDpLedgerCacheKey(path, policy), state,
         envir = .pkg_state$dp_ledger_cache)
  invisible(state)
}

.dsomopDpCreateLedger <- function(connection, policy) {
  DBI::dbExecute(connection, paste(
    "CREATE TABLE dp_meta (",
    "key TEXT PRIMARY KEY, value TEXT NOT NULL)"
  ))
  DBI::dbExecute(connection, paste(
    "CREATE TABLE dp_releases (",
    "release_id TEXT PRIMARY KEY,",
    "release_index INTEGER NOT NULL UNIQUE,",
    "semantic_query_id TEXT NOT NULL,",
    "snapshot_id TEXT NOT NULL,",
    "protected_fingerprint TEXT NOT NULL,",
    "mechanism TEXT NOT NULL,",
    "epsilon REAL NOT NULL, delta REAL NOT NULL,",
    "sensitivity TEXT NOT NULL,",
    "privacy_epoch INTEGER NOT NULL,",
    "noise_key_id TEXT NOT NULL,",
    "payload TEXT NOT NULL,",
    "previous_chain TEXT NOT NULL, row_mac TEXT NOT NULL)"
  ))
  ledger_id <- .dsomopDpSha256(openssl::rand_bytes(32L))
  meta <- list(
    schema_version = .DSOMOP_DP_LEDGER_SCHEMA,
    ledger_id = ledger_id,
    policy_hash = policy$policy_hash,
    ledger_key_id = policy$ledger_root$key_id,
    current_noise_key_id = policy$noise_root$key_id,
    noise_generation = 1,
    privacy_epoch = policy$privacy_epoch,
    next_index = 0,
    spent_epsilon = 0,
    spent_delta = 0,
    chain_head = .DSOMOP_DP_GENESIS
  )
  for (key in names(meta)) .dsomopDpMetaSet(connection, key, meta[[key]])
  invisible(ledger_id)
}

.dsomopDpValidateLedgerArtifact <- function(path) {
  if (.dsomopIsSymlink(path) || !file.exists(path) ||
      !utils::file_test("-f", path) || !.dsomopPrivateMode(path) ||
      !identical(.dsomopLinkCount(path), 1)) {
    stop("A DP ledger artifact must be an owner-only regular file without ",
         "links.", call. = FALSE)
  }
  invisible(TRUE)
}

.dsomopDpOpenLedger <- function(policy) {
  old_umask <- Sys.umask("0077")
  on.exit(try(Sys.umask(old_umask), silent = TRUE), add = TRUE)
  path <- policy$ledger_path
  receipt_path <- .dsomopDpLedgerReceiptPath(path)
  path <- .dsomopPrivateSecretDirectory(
    path,
    .allow_test_path = identical(
      Sys.getenv("DSOMOP_TEST_ALLOW_EPHEMERAL_STATE", unset = ""), "1")
  )
  lock_path <- paste0(path, ".lock")
  if (.dsomopIsSymlink(lock_path)) {
    stop("The DP ledger lock must not be a symbolic link.", call. = FALSE)
  }
  lock <- filelock::lock(lock_path, timeout = 30000)
  if (is.null(lock)) stop("The DP ledger lock is unavailable.", call. = FALSE)
  connection <- NULL
  transaction <- FALSE
  ok <- FALSE
  on.exit(if (!ok) {
    if (transaction && !is.null(connection)) {
      try(DBI::dbExecute(connection, "ROLLBACK"), silent = TRUE)
    }
    if (!is.null(connection)) try(DBI::dbDisconnect(connection), silent = TRUE)
    try(filelock::unlock(lock), silent = TRUE)
  }, add = TRUE)
  Sys.chmod(lock_path, mode = "0600")
  if (.dsomopIsSymlink(lock_path) || !.dsomopPrivateMode(lock_path) ||
      !identical(.dsomopLinkCount(lock_path), 1)) {
    stop("The DP ledger lock is not private.", call. = FALSE)
  }
  # The ledger lock defines the rotation order. A worker that resolved the old
  # file root before another worker recovered it must adopt the currently
  # persisted root rather than rotating metadata back to stale material.
  policy <- .dsomopDpRefreshFileNoisePolicy(policy)
  # Re-evaluate existence only after acquiring the cross-process lock.
  existed <- file.exists(path) || .dsomopIsSymlink(path)
  receipt_existed <- file.exists(receipt_path) ||
    .dsomopIsSymlink(receipt_path)
  if (!existed && receipt_existed) {
    stop("The DP ledger is missing but its continuity receipt exists; restore ",
         "the durable ledger.", call. = FALSE)
  }
  if (existed) .dsomopDpValidateLedgerArtifact(path)
  journal <- paste0(path, "-journal")
  if (file.exists(journal) || .dsomopIsSymlink(journal)) {
    .dsomopDpValidateLedgerArtifact(journal)
  }
  for (forbidden in paste0(path, c("-wal", "-shm"))) {
    if (file.exists(forbidden) || .dsomopIsSymlink(forbidden)) {
      stop("Unexpected WAL/SHM state exists for the DELETE-mode DP ledger.",
           call. = FALSE)
    }
  }
  connection <- DBI::dbConnect(
    RSQLite::SQLite(), path, synchronous = "full",
    flags = RSQLite::SQLITE_RWC
  )
  Sys.chmod(path, mode = "0600")
  .dsomopDpValidateLedgerArtifact(path)
  DBI::dbExecute(connection, "PRAGMA busy_timeout = 30000")
  DBI::dbExecute(connection, "PRAGMA synchronous = FULL")
  DBI::dbExecute(connection, "PRAGMA fullfsync = ON")
  synchronous <- as.integer(DBI::dbGetQuery(
    connection, "PRAGMA synchronous"
  )[[1L]][[1L]])
  fullfsync <- as.integer(DBI::dbGetQuery(
    connection, "PRAGMA fullfsync"
  )[[1L]][[1L]])
  journal_mode <- tolower(DBI::dbGetQuery(
    connection, "PRAGMA journal_mode = DELETE"
  )[[1L]][[1L]])
  if (!identical(journal_mode, "delete") ||
      !identical(synchronous, 2L) || !identical(fullfsync, 1L)) {
    stop("The DP ledger could not establish its required SQLite durability ",
         "contract.", call. = FALSE)
  }
  DBI::dbExecute(connection, "BEGIN IMMEDIATE")
  transaction <- TRUE
  tables <- DBI::dbListTables(connection)
  fresh <- length(tables) == 0L
  if (fresh && receipt_existed) {
    stop("An empty DP ledger conflicts with an existing continuity receipt; ",
         "restore the durable ledger.", call. = FALSE)
  }
  if (fresh) {
    .dsomopDpCreateLedger(connection, policy)
  } else if (!setequal(tables, c("dp_meta", "dp_releases"))) {
    stop("The DP ledger schema is unrecognized.", call. = FALSE)
  }
  if (!fresh && receipt_existed) {
    observed_receipt <- .dsomopValidateSecretFile(receipt_path)
    expected_receipt <- .dsomopDpExpectedLedgerReceipt(connection, policy)
    if (!identical(observed_receipt, expected_receipt)) {
      stop("The private DP continuity receipt does not match persistent state.",
           call. = FALSE)
    }
  }
  if (!fresh && !receipt_existed) {
    next_index <- suppressWarnings(as.numeric(
      .dsomopDpMetaGet(connection, "next_index")
    ))
    if (length(next_index) != 1L || !is.finite(next_index) ||
        next_index < 0 || next_index != floor(next_index)) {
      stop("The DP ledger metadata is incomplete.", call. = FALSE)
    }
    if (next_index > 0) {
      stop("The non-empty DP ledger has no continuity receipt; restore its ",
           "authenticated state.", call. = FALSE)
    }
  }
  state <- .dsomopDpValidateLedger(
    connection, policy, cached = .dsomopDpLedgerCacheGet(path, policy),
    file_signature = if (fresh) NULL else .dsomopDpLedgerFileSignature(path),
    apply_updates = FALSE
  )
  .dsomopDpSyncAnchor(
    list(connection = connection, policy = policy, state = state),
    mutate = FALSE
  )
  if (isTRUE(state$pending_epoch_update)) {
    .dsomopDpMetaSet(connection, "privacy_epoch", state$privacy_epoch)
  }
  if (isTRUE(state$pending_noise_update)) {
    .dsomopDpMetaSet(connection, "current_noise_key_id",
                     policy$noise_root$key_id)
    .dsomopDpMetaSet(connection, "noise_generation",
                     state$noise_generation)
  }
  state$pending_epoch_update <- NULL
  state$pending_noise_update <- NULL
  DBI::dbExecute(connection, "COMMIT")
  transaction <- FALSE
  expected_receipt <- .dsomopDpHmacRaw(
    policy$keys$ledger,
    .dsomopDpCanonicalJson(list(
      protocol = "dsomop-dp-ledger-continuity-receipt-v1",
      ledger_id = state$ledger_id,
      policy_hash = policy$policy_hash,
      ledger_key_id = policy$ledger_root$key_id
    ))
  )
  .dsomopDpEnsurePrivate32(receipt_path, expected_receipt)
  state$file_signature <- .dsomopDpLedgerFileSignature(path)
  .dsomopDpLedgerCacheSet(path, policy, state)
  ok <- TRUE
  list(
    connection = connection, lock = lock, path = path,
    policy = policy, state = state
  )
}

.dsomopDpCloseLedger <- function(handle) {
  if (!is.list(handle)) return(invisible(NULL))
  if (!is.null(handle$lock)) {
    on.exit(try(filelock::unlock(handle$lock), silent = TRUE), add = TRUE)
  }
  if (!is.null(handle$connection)) {
    try(DBI::dbDisconnect(handle$connection), silent = TRUE)
  }
  if (!is.null(handle$path) && file.exists(handle$path)) {
    .dsomopRequireSync(handle$path)
    .dsomopRequireSync(dirname(handle$path))
    if (!is.null(handle$policy)) {
      state <- .dsomopDpLedgerCacheGet(handle$path, handle$policy)
      if (is.list(state)) {
        state$file_signature <- .dsomopDpLedgerFileSignature(handle$path)
        .dsomopDpLedgerCacheSet(handle$path, handle$policy, state)
      }
    }
  }
  invisible(NULL)
}

.dsomopDpAnchorState <- function(value, policy, allow_null = FALSE) {
  if (is.null(value) && allow_null) return(NULL)
  expected <- c(
    "schema_version", "ledger_id", "policy_hash", "next_index",
    "chain_head"
  )
  valid <- is.list(value) && !is.null(names(value)) &&
    !anyNA(names(value)) && !anyDuplicated(names(value)) &&
    setequal(names(value), expected) &&
    identical(as.numeric(value$schema_version), 1) &&
    is.character(value$ledger_id) && length(value$ledger_id) == 1L &&
    !is.na(value$ledger_id) && grepl("^[0-9a-f]{64}$", value$ledger_id) &&
    is.character(value$policy_hash) && length(value$policy_hash) == 1L &&
    !is.na(value$policy_hash) &&
    identical(value$policy_hash, policy$policy_hash) &&
    is.numeric(value$next_index) && length(value$next_index) == 1L &&
    !is.na(value$next_index) && is.finite(value$next_index) &&
    value$next_index >= 0 && value$next_index == floor(value$next_index) &&
    is.character(value$chain_head) && length(value$chain_head) == 1L &&
    ((value$next_index == 0 && identical(value$chain_head,
                                         .DSOMOP_DP_GENESIS)) ||
     (value$next_index > 0 && grepl("^[0-9a-f]{64}$", value$chain_head)))
  if (!isTRUE(valid)) {
    stop("The external DP rollback anchor returned invalid state.",
         call. = FALSE)
  }
  list(
    schema_version = 1L,
    ledger_id = value$ledger_id,
    policy_hash = value$policy_hash,
    next_index = as.numeric(value$next_index),
    chain_head = value$chain_head
  )
}

.dsomopDpAnchorEqual <- function(left, right) {
  identical(.dsomopDpCanonicalJson(left), .dsomopDpCanonicalJson(right))
}

.dsomopDpAnchorCas <- function(policy, expected, replacement) {
  result <- .dsomopDpAnchorCall(
    policy, "compare_and_swap", expected = expected,
    replacement = replacement
  )
  valid <- is.list(result) && !is.null(names(result)) &&
    !anyNA(names(result)) && !anyDuplicated(names(result)) &&
    setequal(names(result), c("swapped", "state")) &&
    is.logical(result$swapped) && length(result$swapped) == 1L &&
    !is.na(result$swapped)
  if (!valid) stop("The DP rollback anchor returned an invalid CAS result.",
                   call. = FALSE)
  state <- .dsomopDpAnchorState(result$state, policy)
  if (result$swapped && !.dsomopDpAnchorEqual(state, replacement)) {
    stop("The DP rollback anchor acknowledged the wrong state.",
         call. = FALSE)
  }
  list(swapped = result$swapped, state = state)
}

.dsomopDpSyncAnchor <- function(handle, mutate = TRUE) {
  policy <- handle$policy
  capabilities <- .dsomopDpAnchorCapabilities(policy)
  if (!capabilities$external) {
    if (policy$require_external_anchor) {
      stop("The required external DP rollback anchor is unavailable.",
           call. = FALSE)
    }
    return(capabilities)
  }
  connection <- handle$connection
  local <- handle$state
  if (!is.list(local) || is.null(local$ledger_id) ||
      is.null(local$next_index) || is.null(local$chain_head)) {
    stop("The validated DP ledger state is unavailable.", call. = FALSE)
  }
  target <- list(
    schema_version = 1L,
    ledger_id = local$ledger_id,
    policy_hash = policy$policy_hash,
    next_index = local$next_index,
    chain_head = local$chain_head
  )
  observed <- .dsomopDpAnchorState(
    .dsomopDpAnchorCall(policy, "read"), policy, allow_null = TRUE
  )
  if (is.null(observed)) {
    if (local$next_index != 0) {
      stop("The external DP rollback anchor is missing for a non-empty ",
           "ledger.", call. = FALSE)
    }
    if (!isTRUE(mutate)) return(capabilities)
    result <- .dsomopDpAnchorCas(policy, NULL, target)
    if (!result$swapped && !.dsomopDpAnchorEqual(result$state, target)) {
      stop("The external DP rollback anchor could not initialize safely.",
           call. = FALSE)
    }
    return(capabilities)
  }
  if (!identical(observed$ledger_id, local$ledger_id) ||
      observed$next_index > local$next_index) {
    stop("The external DP rollback anchor is ahead of or bound to a different ",
         "ledger; restore the durable release mapping.", call. = FALSE)
  }
  local_head_at <- function(next_index) {
    if (next_index == 0) return(.DSOMOP_DP_GENESIS)
    row <- DBI::dbGetQuery(
      connection,
      "SELECT row_mac FROM dp_releases WHERE release_index = ?",
      params = list(next_index - 1)
    )
    if (nrow(row) != 1L) stop("The local DP chain is incomplete.",
                              call. = FALSE)
    row$row_mac[[1L]]
  }
  if (!identical(observed$chain_head, local_head_at(observed$next_index))) {
    stop("The external DP rollback anchor diverges from the local chain.",
         call. = FALSE)
  }
  if (!isTRUE(mutate)) return(capabilities)
  current <- observed
  while (current$next_index < local$next_index) {
    row <- DBI::dbGetQuery(
      connection,
      "SELECT row_mac FROM dp_releases WHERE release_index = ?",
      params = list(current$next_index)
    )
    if (nrow(row) != 1L) stop("The local DP chain gap is incomplete.",
                              call. = FALSE)
    replacement <- current
    replacement$next_index <- current$next_index + 1
    replacement$chain_head <- row$row_mac[[1L]]
    result <- .dsomopDpAnchorCas(policy, current, replacement)
    if (!result$swapped && !.dsomopDpAnchorEqual(result$state, replacement)) {
      stop("The external DP rollback anchor advanced concurrently to an ",
           "incompatible state.", call. = FALSE)
    }
    current <- replacement
  }
  if (!.dsomopDpAnchorEqual(current, target)) {
    stop("The external DP rollback anchor did not reach the local chain head.",
         call. = FALSE)
  }
  capabilities
}

.dsomopDpUniform <- function(key, context, coordinate, draw) {
  digest <- .dsomopDpHmacRaw(key, .dsomopDpCanonicalJson(list(
    protocol = "dsomop-dp-hmac-stream-v1",
    context = context,
    coordinate = as.numeric(coordinate),
    draw = as.numeric(draw)
  )))
  bytes <- as.integer(digest[seq_len(7L)])
  high48 <- 0
  for (byte in bytes[seq_len(6L)]) high48 <- high48 * 256 + byte
  integer52 <- high48 * 16 + floor(bytes[[7L]] / 16)
  # Both numerator and denominator are exactly representable doubles. This
  # construction is strictly inside (0, 1), including all-zero/all-one HMACs.
  (integer52 + 1) / (2^52 + 1)
}

.dsomopDpDiscreteLaplace <- function(key, context, coordinate, epsilon,
                                     sensitivity) {
  if (!is.numeric(epsilon) || length(epsilon) != 1L || !is.finite(epsilon) ||
      epsilon <= 0 || epsilon > 8 || !is.numeric(sensitivity) ||
      length(sensitivity) != 1L || !is.finite(sensitivity) ||
      sensitivity <= 0) {
    stop("The discrete-Laplace allocation is invalid.", call. = FALSE)
  }
  log_alpha <- -epsilon / sensitivity
  geometric <- function(draw) {
    if (log_alpha < log(.Machine$double.xmin)) return(0)
    u <- .dsomopDpUniform(key, context, coordinate, draw)
    floor(log1p(-u) / log_alpha)
  }
  geometric(1L) - geometric(2L)
}

.dsomopDpNoisyInteger <- function(value, policy, release_context, component,
                                  epsilon, sensitivity, lower = 0,
                                  upper = 2^53 - 1) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
      value != floor(value) || value < 0 || value > 2^53 - 1) {
    stop("A bounded DP integer statistic is not exactly representable.",
         call. = FALSE)
  }
  context <- list(
    release = release_context,
    mechanism = .DSOMOP_DP_MECHANISM,
    sampler = policy$sampler,
    privacy_epoch = policy$privacy_epoch,
    allocation = list(epsilon = epsilon, delta = 0,
                      sensitivity = sensitivity),
    component = component
  )
  noise <- .dsomopDpDiscreteLaplace(
    policy$keys$noise, context, coordinate = 1L,
    epsilon = epsilon, sensitivity = sensitivity
  )
  min(upper, max(lower, value + noise))
}

.dsomopDpSemanticId <- function(policy, semantic) {
  .dsomopDpHmac(policy$keys$query, .dsomopDpCanonicalJson(list(
    protocol = .DSOMOP_DP_CANONICAL_PROTOCOL,
    semantic = semantic
  )))
}

.dsomopDpSnapshotBinding <- function(policy, bounded_snapshot) {
  json <- .dsomopDpCanonicalJson(list(
    protocol = "dsomop-dp-bounded-sufficient-statistic-v1",
    value = bounded_snapshot
  ))
  list(
    snapshot_id = .dsomopDpPublicSnapshotId(policy),
    protected_fingerprint = .dsomopDpHmac(policy$keys$protected, paste0(
      "dsOMOP/dp/protected-fingerprint/v1\u001f", json
    ))
  )
}

.dsomopDpPublicSnapshotId <- function(policy) {
  .dsomopDpSha256(.dsomopDpCanonicalJson(list(
    protocol = "dsomop-dp-public-snapshot-v1",
    domain = policy$domain,
    snapshot_id = policy$snapshot_id
  )))
}

.dsomopDpReleaseId <- function(policy, semantic) {
  .dsomopDpHmac(policy$keys$lookup, .dsomopDpCanonicalJson(list(
    protocol = .DSOMOP_DP_LOOKUP_PROTOCOL,
    domain = policy$domain,
    policy_hash = policy$policy_hash,
    snapshot_id = policy$snapshot_id,
    privacy_epoch = policy$privacy_epoch,
    semantic = semantic
  )))
}

.dsomopDpLedgerRelease <- function(policy, semantic, bounded_snapshot,
                                   sensitivity, payload_fn) {
  if (!is.function(payload_fn)) stop("payload_fn must be a function.",
                                     call. = FALSE)
  semantic <- list(
    protocol = .DSOMOP_DP_PROTOCOL,
    mechanism = policy$mechanism,
    accounting_mode = policy$accounting_mode,
    allocator = policy$allocator,
    adjacency = policy$adjacency,
    sensitivity = sensitivity,
    statistic = semantic
  )
  semantic_query_id <- .dsomopDpSemanticId(policy, semantic)
  snapshot <- .dsomopDpSnapshotBinding(policy, bounded_snapshot)
  release_id <- .dsomopDpReleaseId(policy, semantic)
  handle <- .dsomopDpOpenLedger(policy)
  policy <- handle$policy
  transaction <- FALSE
  on.exit({
    if (transaction) {
      try(DBI::dbExecute(handle$connection, "ROLLBACK"), silent = TRUE)
    }
    .dsomopDpCloseLedger(handle)
  }, add = TRUE)
  .dsomopDpSyncAnchor(handle)
  connection <- handle$connection
  DBI::dbExecute(connection, "BEGIN IMMEDIATE")
  transaction <- TRUE
  state <- handle$state
  row <- DBI::dbGetQuery(
    connection, paste(
      "SELECT release_id, release_index, semantic_query_id, snapshot_id,",
      "protected_fingerprint, mechanism, epsilon, delta, sensitivity,",
      "privacy_epoch, noise_key_id, payload, previous_chain, row_mac",
      "FROM dp_releases WHERE release_id = ?"
    ),
    params = list(release_id)
  )
  if (nrow(row) == 1L) {
    fields <- .dsomopDpReleaseFields(row)
    valid_row <- identical(fields$release_id, release_id) &&
      identical(fields$semantic_query_id, semantic_query_id) &&
      identical(fields$mechanism, policy$mechanism) &&
      identical(fields$sensitivity, .dsomopDpCanonicalJson(sensitivity)) &&
      is.finite(fields$release_index) && fields$release_index >= 0 &&
      fields$release_index == floor(fields$release_index) &&
      is.finite(fields$epsilon) && fields$epsilon >= 0 &&
      abs(fields$epsilon -
            .dsomopDpAllocation(policy, fields$release_index)$epsilon) <= 1e-12 &&
      identical(fields$delta, 0) &&
      grepl("^dpk_[0-9a-f]{40}$", fields$noise_key_id) &&
      identical(row$row_mac[[1L]], .dsomopDpRowMac(policy, fields))
    if (!isTRUE(valid_row)) {
      stop("The authenticated DP release chain is corrupt.", call. = FALSE)
    }
    if (!identical(fields$snapshot_id, snapshot$snapshot_id) ||
        !identical(fields$privacy_epoch, policy$privacy_epoch) ||
        !identical(fields$protected_fingerprint,
                   snapshot$protected_fingerprint)) {
      stop("The stored DP release does not match its protected snapshot identity.",
           call. = FALSE)
    }
    payload <- tryCatch(
      jsonlite::fromJSON(fields$payload, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (!is.list(payload)) stop("The stored DP payload is corrupt.",
                                call. = FALSE)
    DBI::dbExecute(connection, "COMMIT")
    transaction <- FALSE
    .dsomopDpSyncAnchor(handle)
    return(payload)
  }
  if (nrow(row) != 0L) stop("The DP release mapping is ambiguous.",
                            call. = FALSE)
  allocation <- .dsomopDpAllocation(policy, state$next_index)
  epsilon <- allocation$epsilon
  release_context <- list(
    release_id = release_id,
    semantic_query_id = semantic_query_id,
    snapshot_id = .dsomopDpPublicSnapshotId(policy),
    policy_hash = policy$policy_hash
  )
  value <- payload_fn(
    epsilon = epsilon, policy = policy,
    release_context = release_context,
    degraded = allocation$degraded
  )
  reserved <- c(
    "protocol", "mechanism", "adjacency", "epsilon", "delta",
    "sensitivity", "accounting_mode", "allocator", "sticky", "sampler"
  )
  if (!is.list(value) || is.null(names(value)) || anyNA(names(value)) ||
      anyDuplicated(names(value)) ||
      any(c("noise_root", "seed", "raw_noise") %in% names(value)) ||
      any(reserved %in% names(value))) {
    stop("The DP mechanism returned an invalid payload.", call. = FALSE)
  }
  payload_value <- c(value, list(
    protocol = policy$protocol,
    mechanism = policy$mechanism,
    adjacency = policy$adjacency,
    epsilon = epsilon,
    delta = 0,
    sensitivity = sensitivity,
    accounting_mode = policy$accounting_mode,
    allocator = policy$allocator,
    sticky = TRUE,
    sampler = policy$sampler
  ))
  payload <- .dsomopDpCanonicalJson(payload_value)
  payload_value <- jsonlite::fromJSON(payload, simplifyVector = TRUE)
  # Once the summable allocator reaches its terminal zero allocation, every
  # new semantic request is a public-shape, data-independent response. Do not
  # grow the ledger indefinitely with zero-cost rows. Existing releases were
  # looked up above and remain exact replays.
  if (isTRUE(allocation$degraded)) {
    DBI::dbExecute(connection, "COMMIT")
    transaction <- FALSE
    .dsomopDpSyncAnchor(handle)
    return(payload_value)
  }
  release_index <- state$next_index
  fields <- list(
    release_index = release_index,
    release_id = release_id,
    semantic_query_id = semantic_query_id,
    snapshot_id = snapshot$snapshot_id,
    protected_fingerprint = snapshot$protected_fingerprint,
    mechanism = policy$mechanism,
    epsilon = epsilon,
    delta = 0,
    sensitivity = .dsomopDpCanonicalJson(sensitivity),
    privacy_epoch = policy$privacy_epoch,
    noise_key_id = policy$noise_root$key_id,
    payload = payload,
    previous_chain = state$chain_head
  )
  row_mac <- .dsomopDpRowMac(policy, fields)
  DBI::dbExecute(connection, paste(
    "INSERT INTO dp_releases(",
    "release_id, release_index, semantic_query_id, snapshot_id,",
    "protected_fingerprint, mechanism, epsilon, delta, sensitivity,",
    "privacy_epoch, noise_key_id, payload, previous_chain, row_mac)",
    "VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  ), params = list(
    fields$release_id, fields$release_index, fields$semantic_query_id,
    fields$snapshot_id, fields$protected_fingerprint, fields$mechanism,
    fields$epsilon, fields$delta, fields$sensitivity,
    fields$privacy_epoch, fields$noise_key_id, fields$payload,
    fields$previous_chain, row_mac
  ))
  .dsomopDpMetaSet(connection, "next_index", release_index + 1)
  .dsomopDpMetaSet(connection, "spent_epsilon",
                   format(state$spent_epsilon + epsilon, digits = 17))
  .dsomopDpMetaSet(connection, "spent_delta", 0)
  .dsomopDpMetaSet(connection, "chain_head", row_mac)
  DBI::dbExecute(connection, "COMMIT")
  transaction <- FALSE
  handle$state <- list(
    ledger_id = state$ledger_id,
    policy_hash = state$policy_hash,
    ledger_key_id = state$ledger_key_id,
    privacy_epoch = state$privacy_epoch,
    noise_generation = state$noise_generation,
    next_index = release_index + 1,
    spent_epsilon = state$spent_epsilon + epsilon,
    spent_delta = state$spent_delta,
    chain_head = row_mac,
    file_signature = .dsomopDpLedgerFileSignature(handle$path)
  )
  .dsomopDpLedgerCacheSet(handle$path, policy, handle$state)
  .dsomopDpSyncAnchor(handle)
  payload_value
}

.dsomopDpDormantStatus <- function() {
  list(
    enabled = NA, ready = FALSE, sticky_noise = FALSE, durable_ledger = FALSE,
    bootstrap = "pending_first_service_use",
    protocol = .DSOMOP_DP_PROTOCOL,
    mechanism = .DSOMOP_DP_MECHANISM
  )
}

.dsomopDpPublicStatus <- function(initialize = TRUE) {
  if (!.dsomopDpEnabled()) {
    return(list(
      enabled = FALSE, ready = FALSE, sticky_noise = FALSE,
      durable_ledger = FALSE,
      protocol = .DSOMOP_DP_PROTOCOL,
      mechanism = .DSOMOP_DP_MECHANISM
    ))
  }
  policy <- .dsomopDpPolicy()
  status <- list(
    enabled = TRUE, ready = FALSE, sticky_noise = TRUE,
    durable_ledger = FALSE,
    protocol = policy$protocol,
    canonical_protocol = policy$canonical_protocol,
    mechanism = policy$mechanism,
    sampler = policy$sampler,
    privacy_guarantee = .DSOMOP_PRIVACY_GUARANTEE,
    adjacency = policy$adjacency,
    domain = policy$domain,
    snapshot_id = policy$snapshot_id,
    policy_hash = policy$policy_hash,
    total_epsilon = policy$total_epsilon,
    total_delta = policy$total_delta,
    release_epsilon = policy$release_epsilon,
    release_delta = policy$release_delta,
    accounting_mode = policy$accounting_mode,
    allocator = policy$allocator,
    privacy_epoch = policy$privacy_epoch,
    noise_key_id = policy$noise_root$key_id,
    noise_provider = policy$noise_root$provider,
    max_levels = policy$max_levels,
    max_contributions = policy$max_contributions,
    numeric_grid = policy$numeric_grid,
    min_useful_epsilon = policy$min_useful_epsilon
  )
  if (!initialize) return(status)
  handle <- .dsomopDpOpenLedger(policy)
  policy <- handle$policy
  on.exit(.dsomopDpCloseLedger(handle), add = TRUE)
  anchor <- .dsomopDpSyncAnchor(handle)
  state <- handle$state
  status$ready <- TRUE
  status$noise_key_id <- policy$noise_root$key_id
  status$noise_domain_id <- paste0(
    "dpn_",
    substr(.dsomopDpSha256(.dsomopDpCanonicalJson(list(
      protocol = "dsomop-dp-noise-domain-v1",
      domain = policy$domain,
      noise_key_id = policy$noise_root$key_id
    ))), 1L, 40L)
  )
  status$ledger_id <- state$ledger_id
  status$ledger_key_id <- state$ledger_key_id
  status$privacy_instance_id <- paste0(
    "dpi_",
    substr(.dsomopDpSha256(.dsomopDpCanonicalJson(list(
      protocol = "dsomop-dp-privacy-instance-v2",
      domain = policy$domain,
      ledger_key_id = state$ledger_key_id,
      noise_key_id = policy$noise_root$key_id
    ))), 1L, 40L)
  )
  status$bounded_accounting <- identical(
    policy$accounting_mode, "bounded_accounted"
  )
  status$never_budget_blocked <- TRUE
  status$durable_ledger <- TRUE
  status$rollback_protection <- if (anchor$external) {
    "external_durable_linearizable_cas"
  } else {
    "local_integrity_only_no_rollback_protection"
  }
  status$anchor_provider_id <- anchor$provider_id
  status$release_epsilon_contract <- "server_allocator_maximum"
  status$degradation_possible <- status$bounded_accounting
  next_allocation <- .dsomopDpAllocation(policy, state$next_index)
  status$next_release_epsilon <- next_allocation$epsilon
  status$next_release_degraded <- next_allocation$degraded
  status
}

.dsomopDpBootstrap <- function() {
  .dsomopDpCanonicalSelfTest()
  if (!.dsomopDpEnabled()) {
    status <- .dsomopDpPublicStatus(initialize = FALSE)
    .pkg_state$dp_bootstrap_binding <- list(enabled = FALSE)
    return(status)
  }
  status <- .dsomopDpPublicStatus(initialize = TRUE)
  policy <- .dsomopDpPolicy()
  .pkg_state$dp_bootstrap_binding <- list(
    enabled = TRUE,
    state_root = .dsomopStateRoot(),
    ledger_path = policy$ledger_path,
    policy_hash = policy$policy_hash,
    domain = policy$domain,
    snapshot_id = policy$snapshot_id,
    privacy_epoch = policy$privacy_epoch,
    anchor_provider = policy$anchor_provider,
    anchor_provider_id = status$anchor_provider_id %||% "none"
  )
  status
}

.dsomopDpEnsureRuntime <- function() {
  if (is.list(.pkg_state$dp_bootstrap_binding)) {
    # Re-read enablement so post-bootstrap drift still fails closed.
    .dsomopDpEnabled()
    return(invisible(.pkg_state$dp_status))
  }
  if (isTRUE(.pkg_state$dp_bootstrap_in_progress)) {
    stop("Recursive DP service bootstrap was detected.", call. = FALSE)
  }
  .pkg_state$dp_bootstrap_in_progress <- TRUE
  on.exit(.pkg_state$dp_bootstrap_in_progress <- FALSE, add = TRUE)
  status <- .dsomopDpBootstrap()
  .pkg_state$dp_status <- status
  invisible(status)
}
