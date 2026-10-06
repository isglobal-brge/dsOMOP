# Permanent first answers. This state is authoritative, never an evictable cache.
# The local UUID pin survives store loss. External pinning also detects loss of
# both the store and its local pin (or loss of the complete state root).
# MACs detect altered/deleted rows, not a rollback of an entire valid snapshot.
.DSOMOP_DP_BINDING_SCHEMA <- "dsomop-dp-first-answer-store-v1"
.DSOMOP_DP_STATE_ERROR <-
  "The DP release state is unavailable; contact the data custodian."

.dsomopDpBindingFail <- function(reason) {
  message("[dsomop-dp-state] Custodian recovery required: ", reason)
  stop(structure(list(message = .DSOMOP_DP_STATE_ERROR, call = NULL),
                 class = c("dsomop_dp_state_error", "error", "condition")))
}

.dsomopDpBindingGuard <- function(expr) {
  tryCatch(force(expr), error = function(error) {
    if (inherits(error, c("dsomop_dp_state_error", "dsomop_dp_capacity_error"))) {
      stop(error)
    }
    .dsomopDpBindingFail(conditionMessage(error))
  })
}

.dsomopDpBindingSettings <- function() {
  read_setting <- function(environment_name, option_name, default, parser) {
    environment <- Sys.getenv(environment_name, unset = "")
    explicit <- getOption(option_name, NULL)
    from_environment <- if (nzchar(environment)) parser(environment, environment_name) else NULL
    from_option <- if (!is.null(explicit)) parser(explicit, option_name) else NULL
    if (!is.null(from_environment) && !is.null(from_option) &&
        !identical(from_environment, from_option)) {
      stop("Conflicting release state settings in ", environment_name,
           " and ", option_name, ".", call. = FALSE)
    }
    if (!is.null(from_environment)) return(from_environment)
    if (!is.null(from_option)) return(from_option)
    parser(getOption(paste0("default.", option_name), default), option_name)
  }
  parse_id <- function(value, label) {
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        (nzchar(value) && !grepl(paste0(
          "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-",
          "[0-9a-f]{4}-[0-9a-f]{12}$"), value))) {
      stop(label, " must be an empty string or a canonical lowercase UUID.",
           call. = FALSE)
    }
    value
  }
  parse_bytes <- function(value, label) {
    if (is.character(value) && length(value) == 1L && !is.na(value) &&
        grepl("^[0-9]+$", value)) value <- as.numeric(value)
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < 0 || value > 2^53 - 1 ||
        value != floor(value)) {
      stop(label, " must be an exactly representable nonnegative byte count.",
           call. = FALSE)
    }
    as.numeric(value)
  }
  list(
    store_id = read_setting(
      "DSOMOP_DP_RELEASE_STORE_ID", "dsomop.dp.release_store_id", "",
      parse_id),
    capacity = read_setting(
      "DSOMOP_DP_RELEASE_STORE_BYTES", "dsomop.dp.release_store_bytes", 2^30,
      parse_bytes)
  )
}

.dsomopDpBindingEstablished <- function() {
  configured <- .dsomopOption("dsomop.dp.release_store_id")
  has_setting <- !is.null(configured) && !identical(configured, "")
  if (has_setting || nzchar(Sys.getenv("DSOMOP_DP_RELEASE_STORE_ID", ""))) {
    return(TRUE)
  }
  state_root <- .dsomopStateRoot()
  .dsomopDpBindingExists(file.path(state_root, "release-bindings")) ||
    .dsomopDpBindingExists(file.path(state_root, "release-store-id"))
}

.dsomopDpBindingExists <- function(path) {
  file.exists(path) || dir.exists(path) || .dsomopIsSymlink(path)
}

.dsomopDpBindingValidId <- function(id) {
  is.character(id) && length(id) == 1L && !is.na(id) &&
    grepl("^[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}$", id, useBytes = TRUE)
}

.dsomopDpBindingReadPin <- function(state_root) {
  path <- file.path(state_root, "release-store-id")
  if (!.dsomopDpBindingExists(path)) return(NULL)
  .dsomopDpBindingFile(path)
  before <- file.info(path)[c("size", "mtime", "ctime")]
  if (!identical(before$size, 36)) stop("The release store UUID pin is malformed.")
  bytes <- readBin(path, "raw", 37L)
  .dsomopDpBindingFile(path)
  after <- file.info(path)[c("size", "mtime", "ctime")]
  if (length(bytes) != 36L || !identical(before, after)) {
    stop("The release store UUID pin changed while being read.")
  }
  id <- rawToChar(bytes)
  if (!.dsomopDpBindingValidId(id)) stop("The release store UUID pin is malformed.")
  # Also finish durability checks after a previous process was interrupted
  # between publishing the pin and synchronizing its containing directory.
  .dsomopRequireSync(path)
  .dsomopRequireSync(state_root)
  id
}

.dsomopDpBindingWritePin <- function(state_root, store_id) {
  old_umask <- Sys.umask("0077")
  on.exit(Sys.umask(old_umask), add = TRUE)
  path <- file.path(state_root, "release-store-id")
  temporary <- tempfile(".release-store-id-", tmpdir = state_root)
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  connection <- file(temporary, "wb")
  on.exit(try(close(connection), silent = TRUE), add = TRUE)
  writeBin(charToRaw(store_id), connection)
  flush(connection)
  close(connection)
  Sys.chmod(temporary, "0600")
  .dsomopDpBindingFile(temporary)
  .dsomopRequireSync(temporary)
  if (!isTRUE(suppressWarnings(file.link(temporary, path)))) {
    stop("Could not publish the release store UUID pin without replacing existing state.")
  }
  if (unlink(temporary) != 0L) stop("Could not finalize the release store UUID pin.")
  if (!identical(.dsomopDpBindingReadPin(state_root), store_id)) {
    stop("The published release store UUID pin is invalid.")
  }
  invisible(store_id)
}

.dsomopDpBindingIdentity <- function() {
  .dsomopDpBindingGuard({
    settings <- .dsomopDpBindingSettings()
    state_root <- .dsomopStateRoot()
    if (!.dsomopDpBindingExists(file.path(state_root, "release-store-id"))) {
      # Recheck all first-use conditions under the initialization lock: another
      # worker may have published the database but not yet its final pin.
      id <- .dsomopDpBindingInitialize(automatic = TRUE)
    } else {
      .dsomopDpBindingDirectory(state_root)
      lock <- .dsomopDpBindingLock(file.path(state_root, ".release-store-init.lock"))
      on.exit(filelock::unlock(lock), add = TRUE)
      id <- .dsomopDpBindingReadPin(state_root)
      if (is.null(id)) stop("The established release store UUID pin is missing.")
      if (nzchar(settings$store_id) && !identical(settings$store_id, id)) {
        stop("The configured release store UUID differs from its local pin.")
      }
      if (nzchar(settings$store_id)) id <- settings$store_id
    }
    list(store_id = id, state_root = state_root)
  })
}

.dsomopDpBindingDirectory <- function(path, create = FALSE) {
  .dsomopAssertNoSymlinkComponents(path)
  if (!dir.exists(path) && isTRUE(create)) {
    if (!dir.create(path, recursive = FALSE, mode = "0700", showWarnings = FALSE)) {
      stop("Could not create a private release state directory.")
    }
    Sys.chmod(path, "0700")
  }
  if (!dir.exists(path) || .dsomopIsSymlink(path) ||
      !.dsomopPrivateMode(path, directory = TRUE)) {
    stop("A release state directory is missing or is not owner-only mode 0700.")
  }
  invisible(path)
}

.dsomopDpBindingFile <- function(path) {
  if (.dsomopIsSymlink(path) || !file.exists(path) ||
      !utils::file_test("-f", path) || !.dsomopPrivateMode(path) ||
      !identical(.dsomopLinkCount(path), 1)) {
    stop("A release state file is missing, linked, or not owner-only mode 0600.")
  }
  invisible(path)
}

.dsomopDpBindingCreateLock <- function(path) {
  if (!file.exists(path) && !.dsomopIsSymlink(path)) {
    old_umask <- Sys.umask("0077")
    on.exit(Sys.umask(old_umask), add = TRUE)
    if (!isTRUE(file.create(path))) stop("Could not create a release state lock.")
    Sys.chmod(path, "0600")
    .dsomopRequireSync(path)
    .dsomopRequireSync(dirname(path))
  }
  .dsomopDpBindingFile(path)
}

.dsomopDpBindingLock <- function(path) {
  # Established locks are permanent. Never recreate a missing lock while a
  # process could still hold the removed inode. Restore only with workers stopped.
  .dsomopDpBindingFile(path)
  lock <- filelock::lock(path, timeout = 30000)
  if (is.null(lock)) stop("A release state lock is unavailable.")
  tryCatch(.dsomopDpBindingFile(path), error = function(error) {
    filelock::unlock(lock)
    stop(error)
  })
  lock
}

.dsomopDpBindingContext <- function(policy) {
  identity <- .dsomopDpBindingIdentity()
  if ((!is.null(policy$binding_store_id) &&
       !identical(policy$binding_store_id, identity$store_id)) ||
      (!is.null(policy$binding_state_root) &&
       !identical(policy$binding_state_root, identity$state_root))) {
    stop("The release store configuration changed during this R session.")
  }
  root <- .dsomopDpNoiseRoot()
  if (!identical(root$key_id, policy$noise_root$key_id) ||
      !identical(root$key, policy$noise_root$key)) {
    stop("The persistent DP root differs from the active release policy.")
  }
  directory <- file.path(identity$state_root, "release-bindings")
  list(
    store_id = identity$store_id, state_root = identity$state_root,
    directory = directory, database = file.path(directory, "releases.sqlite"),
    global_lock = file.path(directory, ".store.lock"),
    locks = file.path(directory, "locks"), key_id = root$key_id,
    key = .dsomopDpHmacRaw(root$key, "dsOMOP/release-binding-MAC/v1"),
    capacity = .dsomopDpBindingSettings()$capacity
  )
}

.dsomopDpBindingDirectories <- function(context) {
  .dsomopDpBindingDirectory(context$state_root)
  .dsomopDpBindingDirectory(context$directory)
  .dsomopDpBindingDirectory(context$locks)
  .dsomopDpBindingFile(context$global_lock)
  invisible(TRUE)
}

.dsomopDpBindingPaths <- function(context) {
  .dsomopDpBindingDirectories(context)
  .dsomopDpBindingFile(context$database)
  journal <- paste0(context$database, "-journal")
  if (file.exists(journal) || .dsomopIsSymlink(journal)) {
    .dsomopDpBindingFile(journal)
  }
  for (suffix in c("-wal", "-shm")) {
    path <- paste0(context$database, suffix)
    if (file.exists(path) || .dsomopIsSymlink(path)) {
      stop("Unexpected WAL state exists for the release store.")
    }
  }
  invisible(TRUE)
}

.dsomopDpBindingConnect <- function(path, create = FALSE) {
  connection <- DBI::dbConnect(
    RSQLite::SQLite(), dbname = path,
    flags = if (create) RSQLite::SQLITE_RWC else RSQLite::SQLITE_RW)
  ready <- FALSE
  on.exit(if (!ready) DBI::dbDisconnect(connection), add = TRUE)
  DBI::dbExecute(connection, "PRAGMA busy_timeout=30000")
  mode <- DBI::dbGetQuery(connection, "PRAGMA journal_mode=DELETE")[[1L]]
  if (!identical(tolower(mode), "delete")) stop("Unsafe release store journal mode.")
  DBI::dbExecute(connection, "PRAGMA synchronous=EXTRA")
  DBI::dbExecute(connection, "PRAGMA fullfsync=ON")
  DBI::dbExecute(connection, "PRAGMA trusted_schema=OFF")
  ready <- TRUE
  connection
}

.dsomopDpBindingInventory <- function(connection) {
  DBI::dbGetQuery(connection, paste(
    "SELECT request_id, state, reservation_bytes, record_mac FROM bindings",
    "ORDER BY request_id COLLATE BINARY"))
}

.dsomopDpBindingHeadMac <- function(context, inventory) {
  .dsomopDpHmac(context$key, .dsomopDpCanonicalJson(list(
    type = "inventory", schema = .DSOMOP_DP_BINDING_SCHEMA,
    store_id = context$store_id, key_id = context$key_id,
    records = lapply(seq_len(nrow(inventory)), function(i) {
      as.list(inventory[i, , drop = FALSE])
    })
  )))
}

.dsomopDpBindingRecordMac <- function(context, request_id, state,
                                      reservation_bytes, payload) {
  metadata <- .dsomopDpCanonicalJson(list(
    type = "response", schema = .DSOMOP_DP_BINDING_SCHEMA,
    store_id = context$store_id, key_id = context$key_id,
    request_id = request_id, state = state,
    reservation_bytes = reservation_bytes
  ))
  .dsomopDpHmac(context$key, c(charToRaw(metadata), as.raw(0L), payload))
}

.dsomopDpBindingRead <- function(connection, context) {
  objects <- DBI::dbGetQuery(connection, paste(
    "SELECT type, name FROM sqlite_master",
    "WHERE name NOT LIKE 'sqlite_%' ORDER BY name"))
  if (!identical(objects$name, c("bindings", "header")) ||
      !identical(objects$type, c("table", "table")) ||
      !identical(DBI::dbListFields(connection, "header"),
                 c("singleton", "store_id", "schema", "key_id", "head_mac")) ||
      !identical(DBI::dbListFields(connection, "bindings"),
                 c("request_id", "state", "reservation_bytes", "payload", "record_mac")) ||
      !identical(DBI::dbGetQuery(connection, "PRAGMA quick_check")[[1L]], "ok")) {
    stop("The release store schema or SQLite integrity check failed.")
  }
  header <- DBI::dbGetQuery(connection, "SELECT * FROM header")
  if (nrow(header) != 1L || anyNA(header) ||
      !identical(as.integer(header$singleton), 1L) ||
      !identical(header$schema, .DSOMOP_DP_BINDING_SCHEMA) ||
      !identical(header$store_id, context$store_id) ||
      !identical(header$key_id, context$key_id)) {
    stop("The release store identity, schema, or root binding is invalid.")
  }
  inventory <- .dsomopDpBindingInventory(connection)
  if (anyNA(inventory) || anyDuplicated(inventory$request_id) ||
      any(!grepl("^[0-9a-f]{64}$", inventory$request_id)) ||
      any(!grepl("^[0-9a-f]{64}$", inventory$record_mac)) ||
      any(!inventory$state %in% c("pending", "committed")) ||
      any(!is.finite(inventory$reservation_bytes)) ||
      any(inventory$reservation_bytes < 8192) ||
      any(inventory$reservation_bytes != floor(inventory$reservation_bytes)) ||
      sum(inventory$reservation_bytes) > 2^53 - 1 ||
      !identical(header$head_mac, .dsomopDpBindingHeadMac(context, inventory))) {
    stop("The release store inventory authentication failed; a row may be missing.")
  }
  inventory
}

.dsomopDpBindingWriteHead <- function(connection, context) {
  DBI::dbExecute(connection, "UPDATE header SET head_mac = ? WHERE singleton = 1",
                 params = list(.dsomopDpBindingHeadMac(
                   context, .dsomopDpBindingInventory(connection))))
  invisible(NULL)
}

.dsomopDpBindingTransaction <- function(context, action) {
  .dsomopDpBindingGuard({
    old_umask <- Sys.umask("0077")
    lock <- NULL
    connection <- NULL
    committed <- FALSE
    on.exit({
      if (!is.null(connection)) {
        if (!committed) try(DBI::dbRollback(connection), silent = TRUE)
        try(DBI::dbDisconnect(connection), silent = TRUE)
      }
      if (!is.null(lock)) try(filelock::unlock(lock), silent = TRUE)
      Sys.umask(old_umask)
    }, add = TRUE)
    # SQLite journals are transient. Inspect them only while holding the
    # global lock, so another valid writer cannot remove one during a check.
    .dsomopDpBindingDirectories(context)
    lock <- .dsomopDpBindingLock(context$global_lock)
    .dsomopDpBindingPaths(context)
    connection <- .dsomopDpBindingConnect(context$database)
    DBI::dbExecute(connection, "BEGIN IMMEDIATE")
    inventory <- .dsomopDpBindingRead(connection, context)
    value <- action(connection, inventory)
    DBI::dbCommit(connection)
    committed <- TRUE
    .dsomopRequireSync(context$database)
    .dsomopRequireSync(context$directory)
    value
  })
}

.dsomopDpBindingReadStoreId <- function(context) {
  .dsomopDpBindingDirectories(context)
  lock <- .dsomopDpBindingLock(context$global_lock)
  on.exit(filelock::unlock(lock), add = TRUE)
  .dsomopDpBindingPaths(context)
  connection <- .dsomopDpBindingConnect(context$database)
  on.exit(DBI::dbDisconnect(connection), add = TRUE, after = FALSE)
  DBI::dbGetQuery(connection, "SELECT store_id FROM header")$store_id
}

.dsomopDpBindingCheck <- function(policy) {
  .dsomopDpBindingGuard({
    context <- .dsomopDpBindingContext(policy)
    .dsomopDpBindingTransaction(context, function(connection, inventory) TRUE)
    invisible(TRUE)
  })
}

#' Initialize the permanent DP first-answer store
#'
#' Custodian-only local setup, not a DataSHIELD aggregate or assign method.
#' First use also calls this initialization path automatically when the store,
#' configured UUID and local pin are all absent, including over pre-store upgrade
#' state. Initialization provisions a noise root on a fresh install and syncs an
#' owner-only \code{release-store-id} UUID pin in \code{DSOMOP_STATE_DIR}, outside
#' \code{release-bindings/}. Repeating initialization authenticates existing state
#' and returns its ID; it never replaces a missing or corrupt established store or
#' noise root. A configured UUID must match the local pin and authenticated header.
#'
#' We recommend retaining the returned UUID in deployment configuration outside
#' \code{DSOMOP_STATE_DIR}, using \code{DSOMOP_DP_RELEASE_STORE_ID} or
#' \code{dsomop.dp.release_store_id}. Without external pinning, deleting the whole
#' state root permits a fresh installation with a new file-backed noise root and
#' release domain; injected roots remain externally controlled. Deleting both
#' store and local pin while retaining the noise root is a residual only external
#' UUID pinning prevents. External pinning also detects whole-root loss.
#'
#' Preserve the complete store, local pin, permanent lock files, noise root, and
#' any external UUID. Stop all workers before restoration. MACs do not detect
#' restoring an older complete valid snapshot; avoiding whole-state rollback is a
#' custodial and storage assumption. Interrupted incomplete initialization requires
#' custodian recovery. There is no automatic repair or eviction.
#' @param store_id Optional lowercase UUID for a new, unconfigured store.
#'   By default a random UUID is generated. This is not a refresh control.
#' @return The store UUID, as a character scalar. It is not a secret.
#' @export
omopInitializeReleaseStore <- function(store_id = NULL) {
  .dsomopDpBindingInitialize(store_id)
}

.dsomopDpBindingInitialize <- function(store_id = NULL, automatic = FALSE) {
  .dsomopDpBindingGuard({
    settings <- .dsomopDpBindingSettings()
    state_root <- .dsomopStateRoot()
    directory <- file.path(state_root, "release-bindings")
    database <- file.path(directory, "releases.sqlite")
    pin_path <- file.path(state_root, "release-store-id")
    bootstrap_path <- file.path(state_root, ".release-store-init.lock")
    if ((.dsomopDpBindingExists(pin_path) || nzchar(settings$store_id)) &&
        !.dsomopDpBindingExists(directory)) {
      stop("The established release store is missing; initialization cannot replace it.")
    }
    root <- .dsomopDpNoiseRoot()
    if (!dir.exists(state_root)) {
      .dsomopAssertNoSymlinkComponents(state_root)
      if (!dir.create(state_root, recursive = TRUE, mode = "0700") &&
          !dir.exists(state_root)) {
        stop("Could not initialize the release state root.")
      }
      Sys.chmod(state_root, "0700")
    }
    .dsomopDpBindingDirectory(state_root)
    context_for <- function(id) list(
      store_id = id, state_root = state_root, directory = directory,
      database = database, global_lock = file.path(directory, ".store.lock"),
      locks = file.path(directory, "locks"), key_id = root$key_id,
      key = .dsomopDpHmacRaw(root$key, "dsOMOP/release-binding-MAC/v1")
    )
    validate_id <- function(id, pin = NULL) {
      if (!.dsomopDpBindingValidId(id) ||
          (!is.null(store_id) && !identical(id, store_id)) ||
          (!is.null(pin) && !identical(id, pin)) ||
          (nzchar(settings$store_id) && !identical(id, settings$store_id))) {
        stop("The release store UUID is invalid or conflicts with its pin or configuration.")
      }
      id
    }
    if (.dsomopDpBindingExists(pin_path)) {
      # A published pin also establishes this permanent lock. Never recreate an
      # inode that another worker might still hold after accidental deletion.
      .dsomopDpBindingFile(bootstrap_path)
    } else if (!.dsomopDpBindingExists(bootstrap_path)) {
      if (.dsomopDpBindingExists(directory) &&
          !.dsomopDpBindingExists(bootstrap_path)) {
        # Upgrade the original externally pinned 2.7.1 setup only after full
        # authentication under its existing permanent store lock.
        if (isTRUE(automatic) && !nzchar(settings$store_id)) {
          stop("The established release store has neither a local nor configured UUID pin.")
        }
        id <- validate_id(.dsomopDpBindingReadStoreId(context_for(NULL)))
        .dsomopDpBindingTransaction(context_for(id), function(connection, inventory) {
          .dsomopDpBindingCreateLock(bootstrap_path)
        })
      } else .dsomopDpBindingCreateLock(bootstrap_path)
    }
    bootstrap_lock <- .dsomopDpBindingLock(bootstrap_path)
    on.exit(try(filelock::unlock(bootstrap_lock), silent = TRUE), add = TRUE)
    pin <- .dsomopDpBindingReadPin(state_root)
    if (!is.null(pin) && nzchar(settings$store_id) &&
        !identical(pin, settings$store_id)) {
      stop("The configured release store UUID differs from its local pin.")
    }
    existed <- .dsomopDpBindingExists(directory)
    if (!file.exists(database) &&
        (existed || !is.null(pin) || nzchar(settings$store_id))) {
      stop("The established release store is missing; initialization cannot replace it.")
    }
    if (existed) {
      if (isTRUE(automatic) && is.null(pin) && !nzchar(settings$store_id)) {
        stop("The established release store has neither a local nor configured UUID pin.")
      }
      id <- validate_id(.dsomopDpBindingReadStoreId(context_for(NULL)), pin)
      .dsomopDpBindingTransaction(context_for(id), function(connection, inventory) TRUE)
      if (is.null(pin)) .dsomopDpBindingWritePin(state_root, id)
      return(id)
    }
    if (is.null(store_id)) {
      bytes <- openssl::rand_bytes(16L)
      bytes[[7L]] <- as.raw(bitwOr(bitwAnd(as.integer(bytes[[7L]]), 15L), 64L))
      bytes[[9L]] <- as.raw(bitwOr(bitwAnd(as.integer(bytes[[9L]]), 63L), 128L))
      hex <- .dsomopDpHex(bytes)
      store_id <- paste(substring(hex, c(1, 9, 13, 17, 21), c(8, 12, 16, 20, 32)),
                        collapse = "-")
    }
    validate_id(store_id)
    context <- context_for(store_id)
    old_umask <- Sys.umask("0077")
    on.exit(Sys.umask(old_umask), add = TRUE)
    .dsomopDpBindingDirectory(directory, create = TRUE)
    .dsomopDpBindingDirectory(context$locks, create = TRUE)
    .dsomopDpBindingCreateLock(context$global_lock)
    lock <- .dsomopDpBindingLock(context$global_lock)
    on.exit(try(filelock::unlock(lock), silent = TRUE), add = TRUE)
    temporary <- tempfile(".initialize-", tmpdir = directory)
    on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
    connection <- .dsomopDpBindingConnect(temporary, create = TRUE)
    on.exit(if (!is.null(connection)) {
      try(DBI::dbDisconnect(connection), silent = TRUE)
    }, add = TRUE)
    DBI::dbExecute(connection, paste(
      "CREATE TABLE header (singleton INTEGER PRIMARY KEY CHECK(singleton=1),",
      "store_id TEXT NOT NULL, schema TEXT NOT NULL, key_id TEXT NOT NULL,",
      "head_mac TEXT NOT NULL)"))
    DBI::dbExecute(connection, paste(
      "CREATE TABLE bindings (request_id TEXT PRIMARY KEY, state TEXT NOT NULL,",
      "reservation_bytes REAL NOT NULL, payload BLOB NOT NULL, record_mac TEXT NOT NULL)"))
    DBI::dbExecute(connection, "INSERT INTO header VALUES (1, ?, ?, ?, ?)",
                   params = list(store_id, .DSOMOP_DP_BINDING_SCHEMA, root$key_id,
                     .dsomopDpBindingHeadMac(context, .dsomopDpBindingInventory(connection))))
    DBI::dbDisconnect(connection)
    connection <- NULL
    Sys.chmod(temporary, "0600")
    .dsomopDpBindingFile(temporary)
    .dsomopRequireSync(temporary)
    if (!isTRUE(file.link(temporary, database))) {
      stop("Could not publish the release store without replacing an existing file.")
    }
    if (unlink(temporary) != 0L) stop("Could not finalize release store initialization.")
    .dsomopDpBindingFile(database)
    .dsomopRequireSync(directory)
    .dsomopRequireSync(state_root)
    .dsomopDpBindingWritePin(state_root, store_id)
    if (isTRUE(automatic)) {
      message("[dsomop-dp-state] Custodian: automatically initialized permanent ",
              "DP first-answer store ", store_id, "; local UUID pin: ", pin_path,
              ". Retain the UUID in external configuration for stronger loss detection.")
    }
    store_id
  })
}

.dsomopDpBindingRecord <- function(connection, context, request_id) {
  row <- DBI::dbGetQuery(connection,
    "SELECT * FROM bindings WHERE request_id = ?", params = list(request_id))
  if (nrow(row) != 1L || !is.raw(row$payload[[1L]]) ||
      !identical(row$record_mac, .dsomopDpBindingRecordMac(
        context, request_id, row$state, row$reservation_bytes, row$payload[[1L]])) ||
      (identical(row$state, "pending") && length(row$payload[[1L]]) != 0L) ||
      length(row$payload[[1L]]) > row$reservation_bytes - 8192) {
    stop("The release response authentication or reservation check failed.")
  }
  row
}

.dsomopDpBindingDecode <- function(bytes) {
  value <- unserialize(bytes)
  if (!identical(serialize(value, NULL, version = 3L), bytes)) {
    stop("The retained response serialization is incompatible with this runtime.")
  }
  value
}

.dsomopDpBindingRelease <- function(policy, request_id, reservation_bytes,
                                    payload_fn) {
  context <- .dsomopDpBindingGuard({
    if (!is.character(request_id) || length(request_id) != 1L ||
        is.na(request_id) || !grepl("^[0-9a-f]{64}$", request_id) ||
        !is.numeric(reservation_bytes) || length(reservation_bytes) != 1L ||
        is.na(reservation_bytes) || !is.finite(reservation_bytes) ||
        reservation_bytes < 8192 || reservation_bytes > 2^53 - 1 ||
        reservation_bytes != floor(reservation_bytes) || !is.function(payload_fn)) {
      stop("Invalid public release reservation.")
    }
    .dsomopDpBindingContext(policy)
  })
  lock_path <- file.path(context$locks, paste0(request_id, ".lock"))
  # Reserve first, then acquire R, then take short global transactions. Public
  # new identities alone consume credits. Never hold the global lock in payload_fn.
  .dsomopDpBindingTransaction(context, function(connection, inventory) {
    if (request_id %in% inventory$request_id) {
      .dsomopDpBindingFile(lock_path)
      return(invisible(NULL))
    }
    if (reservation_bytes > context$capacity - sum(inventory$reservation_bytes)) {
      stop(structure(list(
        message = "The DP release store has reached its public reservation capacity.",
        call = NULL), class = c("dsomop_dp_capacity_error", "error", "condition")))
    }
    .dsomopDpBindingCreateLock(lock_path)
    mac <- .dsomopDpBindingRecordMac(
      context, request_id, "pending", reservation_bytes, raw())
    DBI::dbExecute(connection, "INSERT INTO bindings VALUES (?, ?, ?, ?, ?)",
                   params = list(request_id, "pending", as.numeric(reservation_bytes),
                                 list(raw()), mac))
    .dsomopDpBindingWriteHead(connection, context)
    invisible(NULL)
  })
  lock <- .dsomopDpBindingGuard(.dsomopDpBindingLock(lock_path))
  on.exit(try(filelock::unlock(lock), silent = TRUE), add = TRUE)
  row <- .dsomopDpBindingTransaction(context, function(connection, inventory) {
    .dsomopDpBindingRecord(connection, context, request_id)
  })
  if (identical(row$state, "committed")) {
    return(.dsomopDpBindingGuard(.dsomopDpBindingDecode(row$payload[[1L]])))
  }
  value <- payload_fn()
  bytes <- .dsomopDpBindingGuard({
    bytes <- serialize(value, NULL, version = 3L)
    if (length(bytes) > row$reservation_bytes - 8192) {
      stop("The mechanism exceeded its public response reservation.")
    }
    bytes
  })
  .dsomopDpBindingTransaction(context, function(connection, inventory) {
    current <- .dsomopDpBindingRecord(connection, context, request_id)
    if (!identical(current$state, "pending")) {
      stop("A competing writer replaced a pending release despite its lock.")
    }
    .dsomopDpBindingFile(lock_path)
    mac <- .dsomopDpBindingRecordMac(
      context, request_id, "committed", current$reservation_bytes, bytes)
    DBI::dbExecute(connection, paste(
      "UPDATE bindings SET state = 'committed', payload = ?, record_mac = ?",
      "WHERE request_id = ?"), params = list(list(bytes), mac, request_id))
    .dsomopDpBindingWriteHead(connection, context)
    invisible(NULL)
  })
  # Fresh and replay paths decode the very same authenticated canonical bytes.
  .dsomopDpBindingGuard(.dsomopDpBindingDecode(bytes))
}
