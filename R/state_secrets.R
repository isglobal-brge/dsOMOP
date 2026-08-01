# Persistent server-owned secret state.
#
# `configure` may prepare directories, but it must never generate key material:
# package installation commonly runs as root or while building a reusable image.
# Runtime bootstrap is serialized and commits a private file atomically.

.dsomopIsSymlink <- function(path) {
  link <- Sys.readlink(path)
  length(link) != 1L || (!is.na(link) && nzchar(link))
}

.dsomopAssertNoSymlinkComponents <- function(path) {
  path <- gsub("\\\\", "/", path)
  parts <- strsplit(sub("^/", "", path), "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  current <- "/"
  for (part in parts) {
    current <- file.path(current, part)
    if (file.exists(current) || dir.exists(current)) {
      if (.dsomopIsSymlink(current)) {
        stop("The dsOMOP state path must not traverse symbolic links.",
             call. = FALSE)
      }
    } else {
      break
    }
  }
  invisible(TRUE)
}

.dsomopIsInstallOrDevelopmentLoad <- function(libname) {
  path <- normalizePath(libname, winslash = "/", mustWork = FALSE)
  install_environment <- c(
    Sys.getenv("R_INSTALL_PKG", unset = ""),
    Sys.getenv("R_PACKAGE_DIR", unset = ""),
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = "")
  )
  grepl("(^|/)00LOCK([^/]*)(/|$)", path) ||
    any(nzchar(install_environment)) ||
    nzchar(Sys.getenv("DEVTOOLS_LOAD", unset = ""))
}

.dsomopStateRoot <- function(
    configured = getOption(
      "dsomop.state_dir", getOption("default.dsomop.state_dir")),
    environment = Sys.getenv(
      "DSOMOP_STATE_DIR",
      unset = Sys.getenv("DSOMOP_MARKER_DIR", unset = "")),
    home = Sys.getenv("HOME", unset = ""),
    .allow_test_path = identical(
      Sys.getenv("DSOMOP_TEST_ALLOW_EPHEMERAL_STATE", unset = ""), "1")) {
  value <- if (!is.null(configured)) {
    configured
  } else if (nzchar(environment)) {
    environment
  } else {
    file.path(home, ".dsomop")
  }
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) {
    stop("The persistent dsOMOP state directory is unavailable.", call. = FALSE)
  }
  value <- path.expand(value)
  value <- gsub("\\\\", "/", value)
  check_value <- gsub("/+", "/", value)
  if (!grepl("^/", value) || grepl("^//", value) ||
      grepl("(^|/)\\.{1,2}(/|$)", check_value)) {
    stop("The persistent dsOMOP state directory must be an absolute canonical path.",
         call. = FALSE)
  }
  if (nchar(value) > 1L) value <- sub("/+$", "", value)
  if (!isTRUE(.allow_test_path)) {
    resolved_value <- normalizePath(check_value, winslash = "/",
                                    mustWork = FALSE)
    forbidden <- unique(c(
      c("/tmp", "/var/tmp", "/dev/shm", tempdir()),
      normalizePath(c("/tmp", "/var/tmp", "/dev/shm", tempdir()),
                    winslash = "/", mustWork = FALSE)
    ))
    below <- vapply(forbidden, function(root) {
      identical(check_value, root) ||
        startsWith(check_value, paste0(root, "/")) ||
        identical(resolved_value, root) ||
        startsWith(resolved_value, paste0(root, "/"))
    }, logical(1))
    if (identical(check_value, "/") || identical(resolved_value, "/") ||
        any(below)) {
      stop("The dsOMOP state directory must be persistent; temporary filesystems are not allowed.",
           call. = FALSE)
    }
  }
  value
}

.dsomopEffectiveUid <- function() {
  probe <- tempfile(pattern = ".dsomop-euid-")
  on.exit(unlink(probe), add = TRUE)
  if (!isTRUE(file.create(probe))) {
    stop("Could not establish the dsOMOP service process owner.", call. = FALSE)
  }
  uid <- suppressWarnings(as.integer(file.info(probe)$uid[[1]]))
  if (is.na(uid)) {
    stop("Could not establish the dsOMOP service process owner.", call. = FALSE)
  }
  uid
}

.dsomopPrivateMode <- function(path, directory = FALSE) {
  info <- file.info(path)
  if (nrow(info) != 1L || is.na(info$mode[[1]]) || is.na(info$uid[[1]])) {
    return(FALSE)
  }
  expected <- as.integer(strtoi(if (directory) "700" else "600", base = 8L))
  identical(as.integer(info$mode[[1]]), expected) &&
    identical(as.integer(info$uid[[1]]), .dsomopEffectiveUid())
}

.dsomopLinkCount <- function(path) {
  stat <- Sys.which("stat")
  if (!nzchar(stat)) {
    stop("Cannot verify the dsOMOP secret hard-link count.", call. = FALSE)
  }
  args <- if (identical(Sys.info()[["sysname"]], "Darwin")) {
    c("-f", "%l", shQuote(path))
  } else {
    c("-c", "%h", shQuote(path))
  }
  value <- suppressWarnings(tryCatch(
    system2(stat, args, stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  ))
  if (length(value) != 1L || !grepl("^[0-9]+$", value)) {
    stop("Cannot verify the dsOMOP secret hard-link count.", call. = FALSE)
  }
  as.numeric(value)
}

.dsomopPrivateSecretDirectory <- function(path, .allow_test_path = FALSE) {
  if (!identical(.Platform$OS.type, "unix")) {
    stop("File-backed dsOMOP secrets require POSIX owner-only permissions.",
         call. = FALSE)
  }
  root <- .dsomopStateRoot(.allow_test_path = .allow_test_path)
  if (!isTRUE(.allow_test_path)) {
    .dsomopAssertNoSymlinkComponents(dirname(root))
  }
  root_created <- FALSE
  if (!dir.exists(root)) {
    root_created <- isTRUE(dir.create(
      root, recursive = TRUE, showWarnings = FALSE, mode = "0700"
    ))
    # Another worker may have won between dir.exists() and dir.create().
    # Accept only the resulting directory; the owner/mode checks below still
    # fail closed if that winner created anything unsafe.
    if (!root_created && !dir.exists(root)) {
      stop("Could not create the private dsOMOP state directory.",
           call. = FALSE)
    }
  }
  if (isTRUE(.allow_test_path)) {
    if (.dsomopIsSymlink(root)) {
      stop("The dsOMOP state directory must not be a symbolic link.",
           call. = FALSE)
    }
  } else {
    .dsomopAssertNoSymlinkComponents(root)
  }
  if (root_created) {
    Sys.chmod(root, mode = "0700")
  }
  if (!.dsomopPrivateMode(root, directory = TRUE)) {
    stop("The dsOMOP state directory must be owned by the service account with mode 0700.",
         call. = FALSE)
  }

  parent <- dirname(path)
  allowed_parents <- file.path(root, c("secrets", "keys"))
  if (!identical(parent, allowed_parents[[1]]) &&
      !identical(parent, allowed_parents[[2]])) {
    stop("The dsOMOP secret path must be inside the state secrets/keys directory.",
         call. = FALSE)
  }
  parent_created <- FALSE
  if (!dir.exists(parent)) {
    parent_created <- isTRUE(dir.create(
      parent, recursive = FALSE, showWarnings = FALSE, mode = "0700"
    ))
    # Directory bootstrap is deliberately multi-process safe. Losing the
    # create race is acceptable only when the exact expected directory now
    # exists and subsequently passes the private owner/mode checks.
    if (!parent_created && !dir.exists(parent)) {
      stop("Could not create the private dsOMOP secret directory.",
           call. = FALSE)
    }
  }
  if (isTRUE(.allow_test_path)) {
    if (.dsomopIsSymlink(parent)) {
      stop("The dsOMOP secret directory must not be a symbolic link.",
           call. = FALSE)
    }
  } else {
    .dsomopAssertNoSymlinkComponents(parent)
  }
  if (parent_created) {
    Sys.chmod(parent, mode = "0700")
  }
  if (!.dsomopPrivateMode(parent, directory = TRUE)) {
    stop("The dsOMOP secret directory must be owned by the service account with mode 0700.",
         call. = FALSE)
  }
  file.path(normalizePath(parent, winslash = "/", mustWork = TRUE),
            basename(path))
}

.dsomopValidateSecretFile <- function(path) {
  if (.dsomopIsSymlink(path) || !file.exists(path) ||
      !utils::file_test("-f", path) || !.dsomopPrivateMode(path)) {
    stop("A dsOMOP secret must be a regular owner-only file with mode 0600.",
         call. = FALSE)
  }
  if (!identical(.dsomopLinkCount(path), 1)) {
    stop("A dsOMOP secret must not have hard links.", call. = FALSE)
  }
  before <- file.info(path)
  if (nrow(before) != 1L || is.na(before$size[[1]]) || before$size[[1]] != 32) {
    stop("A persisted dsOMOP secret must contain exactly 32 raw bytes.",
         call. = FALSE)
  }
  state_before <- unname(before[c("size", "mtime", "ctime")])
  value <- tryCatch(readBin(path, what = "raw", n = 33L),
                    error = function(e) raw())
  after <- file.info(path)
  state_after <- unname(after[c("size", "mtime", "ctime")])
  if (length(value) != 32L || .dsomopIsSymlink(path) ||
      !.dsomopPrivateMode(path) || !identical(.dsomopLinkCount(path), 1) ||
      !identical(state_before, state_after)) {
    stop("The persisted dsOMOP secret changed while it was being read.",
         call. = FALSE)
  }
  value
}

.dsomopSecretPath <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !identical(name, "pseudonym_root")) {
    stop("Unsupported dsOMOP secret name.", call. = FALSE)
  }
  file.path(.dsomopStateRoot(), "secrets", name)
}

.dsomopSyncFile <- function(path) {
  sync <- Sys.which("sync")
  if (!nzchar(sync)) return(invisible(FALSE))
  status <- suppressWarnings(tryCatch(
    system2(sync, c("-f", shQuote(path)), stdout = FALSE, stderr = FALSE),
    error = function(e) 1L
  ))
  invisible(identical(as.integer(status), 0L))
}

#' Require a successful durability sync for secret state
#'
#' @param path File or containing directory to synchronize.
#' @param .sync Injectable sync implementation used by focused tests.
#' @return TRUE invisibly, or an error when durability cannot be proven.
#' @keywords internal
.dsomopRequireSync <- function(path, .sync = .dsomopSyncFile) {
  if (!is.function(.sync)) {
    stop("The dsOMOP durability sync provider is invalid.", call. = FALSE)
  }
  synced <- suppressWarnings(tryCatch(
    .sync(path), error = function(e) FALSE
  ))
  if (!isTRUE(synced)) {
    stop("Could not durably synchronize dsOMOP secret state; refusing to ",
         "continue with a key whose persistence is not proven.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.dsomopCommitSecretNoClobber <- function(
    temporary, path, retry_seconds = 1, retry_interval = 0.01,
    .link = file.link) {
  if (!is.function(.link) || !is.numeric(retry_seconds) ||
      length(retry_seconds) != 1L || is.na(retry_seconds) ||
      !is.finite(retry_seconds) || retry_seconds < 0 ||
      !is.numeric(retry_interval) || length(retry_interval) != 1L ||
      is.na(retry_interval) || !is.finite(retry_interval) ||
      retry_interval <= 0) {
    stop("Invalid dsOMOP no-clobber commit settings.", call. = FALSE)
  }
  linked <- isTRUE(suppressWarnings(tryCatch(
    .link(temporary, path), error = function(e) FALSE
  )))
  if (linked) {
    removed <- suppressWarnings(unlink(temporary, force = TRUE))
    if (!identical(as.integer(removed), 0L) || file.exists(temporary)) {
      stop("Could not finalize the dsOMOP no-clobber secret commit.",
           call. = FALSE)
    }
    Sys.chmod(path, mode = "0600")
    return(.dsomopValidateSecretFile(path))
  }

  # A lock on a shared filesystem is the first serialization layer. The hard
  # link is the no-replace layer: if another host won despite advisory-lock
  # semantics, wait briefly for it to unlink its temporary name (nlink 2 -> 1)
  # and then adopt only the fully validated winning key.
  deadline <- unname(proc.time()[["elapsed"]]) + retry_seconds
  repeat {
    if (.dsomopIsSymlink(path)) {
      stop("A dsOMOP secret must not be a symbolic link.", call. = FALSE)
    }
    if (file.exists(path)) {
      links <- tryCatch(.dsomopLinkCount(path), error = function(e) NA_real_)
      if (identical(links, 1)) return(.dsomopValidateSecretFile(path))
      if (!is.na(links) && !links %in% c(1, 2)) {
        stop("The winning dsOMOP secret has an unsafe hard-link count.",
             call. = FALSE)
      }
    }
    if (unname(proc.time()[["elapsed"]]) >= deadline) break
    Sys.sleep(retry_interval)
  }
  stop("Could not atomically create the dsOMOP secret without replacing an ",
       "existing key. The filesystem may not support same-directory hard ",
       "links; use an injected or scoped pseudonymization provider.",
       call. = FALSE)
}

.ensureDsomopSecret <- function(
    name, path = .dsomopSecretPath(name), random_bytes = openssl::rand_bytes,
    require_existing = FALSE, .sync = .dsomopSyncFile,
    .allow_test_path = identical(
      Sys.getenv("DSOMOP_TEST_ALLOW_EPHEMERAL_STATE", unset = ""), "1")) {
  if (!is.function(random_bytes)) {
    stop("random_bytes must be a secure random-byte function.", call. = FALSE)
  }
  if (!is.function(.sync)) {
    stop("The dsOMOP durability sync provider is invalid.", call. = FALSE)
  }
  if (!is.logical(require_existing) || length(require_existing) != 1L ||
      is.na(require_existing)) {
    stop("require_existing must be TRUE or FALSE.", call. = FALSE)
  }
  # Production deployments can require a pre-provisioned durable key. Check
  # before creating directories or a lock file so a lost/empty volume never
  # causes a replacement identity to be generated silently.
  if (isTRUE(require_existing)) {
    if (.dsomopIsSymlink(path)) {
      stop("A dsOMOP secret must not be a symbolic link.", call. = FALSE)
    }
    if (!file.exists(path)) {
      stop("The configured pseudonymization provider requires an existing ",
           "secret; refusing to generate a replacement key.", call. = FALSE)
    }
  }
  old_umask <- Sys.umask("0077")
  on.exit(try(Sys.umask(old_umask), silent = TRUE), add = TRUE)
  path <- .dsomopPrivateSecretDirectory(path, .allow_test_path)
  lock_path <- paste0(path, ".lock")
  if (.dsomopIsSymlink(lock_path)) {
    stop("The dsOMOP secret lock must not be a symbolic link.", call. = FALSE)
  }
  lock <- filelock::lock(lock_path, timeout = 30000)
  if (is.null(lock)) {
    stop("The dsOMOP secret bootstrap lock is unavailable.", call. = FALSE)
  }
  on.exit(try(filelock::unlock(lock), silent = TRUE), add = TRUE)
  Sys.chmod(lock_path, mode = "0600")
  if (.dsomopIsSymlink(lock_path) || !.dsomopPrivateMode(lock_path) ||
      !identical(.dsomopLinkCount(lock_path), 1)) {
    stop("The dsOMOP secret bootstrap lock is not private.", call. = FALSE)
  }

  if (.dsomopIsSymlink(path)) {
    stop("A dsOMOP secret must not be a symbolic link.", call. = FALSE)
  }
  if (file.exists(path)) {
    value <- .dsomopValidateSecretFile(path)
    .dsomopRequireSync(path, .sync)
    .dsomopRequireSync(dirname(path), .sync)
    return(value)
  }
  if (isTRUE(require_existing)) {
    stop("The configured pseudonymization provider requires an existing ",
         "secret; refusing to generate a replacement key.", call. = FALSE)
  }
  key <- tryCatch(random_bytes(32L), error = function(e) raw())
  if (!is.raw(key) || length(key) != 32L) {
    stop("Secure operating-system entropy is unavailable for dsOMOP secret bootstrap.",
         call. = FALSE)
  }
  temporary <- tempfile(
    pattern = paste0(".", name, "-", Sys.getpid(), "."),
    tmpdir = dirname(path)
  )
  on.exit(if (file.exists(temporary)) unlink(temporary, force = TRUE), add = TRUE)
  connection <- file(temporary, open = "wb")
  on.exit(try(if (isOpen(connection)) close(connection), silent = TRUE),
          add = TRUE)
  writeBin(key, connection)
  flush(connection)
  close(connection)
  Sys.chmod(temporary, mode = "0600")
  .dsomopValidateSecretFile(temporary)
  .dsomopRequireSync(temporary, .sync)
  value <- .dsomopCommitSecretNoClobber(temporary, path)
  .dsomopRequireSync(dirname(path), .sync)
  value
}

.coerceDsomopSecret <- function(value, label = "pseudonymization root") {
  if (is.raw(value)) {
    if (length(value) != 32L) {
      stop("The ", label, " must contain exactly 32 raw bytes.", call. = FALSE)
    }
    return(value)
  }
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) {
    stop("The ", label, " must be one non-empty secret.", call. = FALSE)
  }
  if (grepl("^[0-9a-fA-F]+$", value)) {
    if (nchar(value) != 64L) {
      stop("A hexadecimal ", label, " must contain exactly 64 characters.",
           call. = FALSE)
    }
    pairs <- substring(value, seq.int(1L, 63L, 2L), seq.int(2L, 64L, 2L))
    return(as.raw(strtoi(pairs, base = 16L)))
  }
  if (nchar(value, type = "bytes") < 32L) {
    stop("A text ", label, " must contain at least 32 bytes.", call. = FALSE)
  }
  as.raw(openssl::sha256(charToRaw(enc2utf8(value))))
}

.deriveDsomopResourceKey <- function(root, identity) {
  if (!is.raw(root) || length(root) != 32L || !is.character(identity) ||
      length(identity) != 1L || is.na(identity) || !nzchar(identity)) {
    stop("Cannot derive a per-resource pseudonymization key.", call. = FALSE)
  }
  context <- c(
    charToRaw("dsOMOP/person-pseudonym/v3"), as.raw(0L),
    charToRaw(enc2utf8(identity))
  )
  as.raw(openssl::sha256(context, key = root))
}

.dsomopOption <- function(name) {
  value <- getOption(name, default = NULL)
  if (is.null(value)) value <- getOption(paste0("default.", name), NULL)
  value
}

.dsomopParsePseudonymEpoch <- function(value, label) {
  valid <- FALSE
  number <- NA_real_
  if (is.character(value) && length(value) == 1L && !is.na(value) &&
      grepl("^[1-9][0-9]*$", value)) {
    number <- suppressWarnings(as.numeric(value))
    valid <- is.finite(number)
  } else if (is.numeric(value) && length(value) == 1L && !is.na(value) &&
             is.finite(value)) {
    number <- as.numeric(value)
    valid <- identical(number, floor(number)) && number >= 1
  }
  if (!valid || number > .Machine$integer.max) {
    stop(label, " must be one positive integer.", call. = FALSE)
  }
  as.integer(number)
}

.dsomopParsePseudonymBoolean <- function(value, label) {
  if (is.logical(value) && length(value) == 1L && !is.na(value)) return(value)
  if (is.numeric(value) && length(value) == 1L && !is.na(value) &&
      value %in% c(0, 1)) {
    return(as.logical(value))
  }
  if (is.character(value) && length(value) == 1L && !is.na(value)) {
    normalized <- tolower(trimws(value))
    if (normalized %in% c("1", "true", "yes")) return(TRUE)
    if (normalized %in% c("0", "false", "no")) return(FALSE)
  }
  stop(label, " must be TRUE or FALSE.", call. = FALSE)
}

.dsomopPseudonymScalarSetting <- function(env_name, option_name, default,
                                           parser, label) {
  env_value <- Sys.getenv(env_name, unset = "")
  option_value <- .dsomopOption(option_name)
  has_env <- nzchar(env_value)
  has_option <- !is.null(option_value)
  parsed_env <- if (has_env) parser(env_value, env_name) else NULL
  parsed_option <- if (has_option) parser(option_value, option_name) else NULL
  if (has_env && has_option && !identical(parsed_env, parsed_option)) {
    stop("Conflicting ", label, " values are configured in ", env_name,
         " and ", option_name, ".", call. = FALSE)
  }
  if (has_env) parsed_env else if (has_option) parsed_option else default
}

.dsomopSecretSetting <- function(env_name, option_name, label) {
  env_value <- Sys.getenv(env_name, unset = "")
  option_value <- .dsomopOption(option_name)
  list(
    env_name = env_name,
    option_name = option_name,
    label = label,
    env_value = env_value,
    option_value = option_value,
    has_env = nzchar(env_value),
    has_option = !is.null(option_value),
    present = nzchar(env_value) || !is.null(option_value)
  )
}

.dsomopSecretSettingValue <- function(setting) {
  env_value <- if (isTRUE(setting$has_env)) {
    .coerceDsomopSecret(setting$env_value, setting$label)
  } else NULL
  option_value <- if (isTRUE(setting$has_option)) {
    .coerceDsomopSecret(setting$option_value, setting$label)
  } else NULL
  if (!is.null(env_value) && !is.null(option_value) &&
      !identical(env_value, option_value)) {
    stop("Conflicting secret values are configured in ", setting$env_name,
         " and ", setting$option_name, ".", call. = FALSE)
  }
  list(
    value = if (!is.null(env_value)) env_value else option_value,
    source = if (!is.null(env_value)) "environment" else "option"
  )
}

.dsomopScopedPseudonymKeys <- function() {
  environment <- Sys.getenv()
  scoped_names <- grep("^DSOMOP_PSEUDONYM_KEY_", names(environment),
                       value = TRUE)
  malformed <- scoped_names[
    !grepl("^DSOMOP_PSEUDONYM_KEY_[0-9a-f]{32}$", scoped_names)
  ]
  if (length(malformed) > 0L) {
    stop("Scoped dsOMOP pseudonym key environment names must end in the ",
         "32-character lowercase resource hash.", call. = FALSE)
  }
  scoped_values <- environment[scoped_names]
  scoped_values <- scoped_values[nzchar(scoped_values)]
  lapply(scoped_values, .coerceDsomopSecret,
         label = "per-resource pseudonymization key")
}

.dsomopPseudonymLifecycleSettings <- function() {
  parse_provider <- function(value, label) {
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !nzchar(trimws(value))) {
      stop(label, " must name one pseudonymization provider.", call. = FALSE)
    }
    value <- tolower(trimws(value))
    if (!value %in% c("auto", "file", "injected", "scoped")) {
      stop(label, " must be one of auto, file, injected, or scoped.",
           call. = FALSE)
    }
    value
  }
  provider <- .dsomopPseudonymScalarSetting(
    "DSOMOP_PSEUDONYM_PROVIDER", "dsomop.pseudonym_provider", "auto",
    parse_provider, "pseudonymization provider")
  epoch <- .dsomopPseudonymScalarSetting(
    "DSOMOP_PSEUDONYM_EPOCH", "dsomop.pseudonym_epoch", 1L,
    .dsomopParsePseudonymEpoch, "pseudonymization epoch")
  require_existing <- .dsomopPseudonymScalarSetting(
    "DSOMOP_PSEUDONYM_REQUIRE_EXISTING",
    "dsomop.pseudonym_require_existing", FALSE,
    .dsomopParsePseudonymBoolean, "pseudonymization require-existing policy")
  allow_legacy_global <- .dsomopPseudonymScalarSetting(
    "DSOMOP_ALLOW_LEGACY_GLOBAL_PSEUDONYMS",
    "dsomop.allow_legacy_global_pseudonyms", FALSE,
    .dsomopParsePseudonymBoolean, "legacy global pseudonymization opt-in")

  list(
    provider = provider,
    epoch = epoch,
    require_existing = require_existing,
    allow_legacy_global = allow_legacy_global
  )
}

.dsomopPseudonymSettings <- function() {
  lifecycle <- .dsomopPseudonymLifecycleSettings()

  root <- .dsomopSecretSetting(
    "DSOMOP_PSEUDONYM_ROOT", "dsomop.pseudonym_root",
    "pseudonymization root")
  legacy <- .dsomopSecretSetting(
    "DSOMOP_PSEUDONYM_KEY", "dsomop.pseudonym_key",
    "legacy global pseudonymization key")
  scoped <- .dsomopScopedPseudonymKeys()

  # Two representations of the same setting are allowed only when they hold
  # exactly the same secret. Different values are operationally ambiguous.
  if (root$has_env && root$has_option) .dsomopSecretSettingValue(root)
  if (legacy$has_env && legacy$has_option) .dsomopSecretSettingValue(legacy)
  if (legacy$present && !isTRUE(lifecycle$allow_legacy_global)) {
    stop("The legacy global DSOMOP_PSEUDONYM_KEY/dsomop.pseudonym_key setting ",
         "is disabled by default because it permits cross-resource linkage. ",
         "Migrate to DSOMOP_PSEUDONYM_ROOT, or set ",
         "DSOMOP_ALLOW_LEGACY_GLOBAL_PSEUDONYMS=true only as an explicit ",
         "administrator-approved compatibility exception.", call. = FALSE)
  }

  injected_sources <- root$present || legacy$present || length(scoped) > 0L
  if (identical(lifecycle$provider, "file") && injected_sources) {
    stop("The file pseudonymization provider conflicts with configured ",
         "injected or scoped key material.", call. = FALSE)
  }
  if (identical(lifecycle$provider, "injected")) {
    if (!root$present) {
      stop("The injected pseudonymization provider requires ",
           "DSOMOP_PSEUDONYM_ROOT or dsomop.pseudonym_root.", call. = FALSE)
    }
    if (legacy$present || length(scoped) > 0L) {
      stop("The injected pseudonymization provider conflicts with legacy or ",
           "scoped key material.", call. = FALSE)
    }
  }
  if (identical(lifecycle$provider, "scoped")) {
    if (length(scoped) == 0L) {
      stop("The scoped pseudonymization provider requires at least one ",
           "DSOMOP_PSEUDONYM_KEY_<resource-hash> value.", call. = FALSE)
    }
    if (root$present || legacy$present) {
      stop("The scoped pseudonymization provider conflicts with global key ",
           "material.", call. = FALSE)
    }
  }

  list(
    provider = lifecycle$provider,
    epoch = lifecycle$epoch,
    require_existing = lifecycle$require_existing,
    allow_legacy_global = lifecycle$allow_legacy_global,
    root = root,
    legacy = legacy,
    scoped = scoped
  )
}

.bootstrapDsomopSecrets <- function() {
  settings <- .dsomopPseudonymSettings()
  if (identical(settings$provider, "injected")) {
    .dsomopSecretSettingValue(settings$root)
    return(invisible("injected"))
  }
  if (identical(settings$provider, "scoped")) {
    return(invisible("scoped_injected"))
  }

  # Compatibility mode retains the historical resolution order. Explicit
  # providers above are strict and reject mixed configurations.
  if (identical(settings$provider, "auto")) {
    if (settings$root$present) {
      .dsomopSecretSettingValue(settings$root)
      return(invisible("injected"))
    }
    if (settings$legacy$present) {
      .dsomopSecretSettingValue(settings$legacy)
      return(invisible("injected"))
    }
    if (length(settings$scoped) > 0L) {
      return(invisible("scoped_injected"))
    }
  }

  .ensureDsomopSecret(
    "pseudonym_root", require_existing = settings$require_existing)
  invisible("persistent_file")
}
