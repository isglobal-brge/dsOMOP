# Module: Blueprint System
# CDM schema introspection, handle creation, vendored spec management.

# --- CDM Spec Loading ---

.DSOMOP_VENDORED_OHDSI_METADATA <- list(
  contract_version = 1L,
  source = "OHDSI/CommonDataModel",
  files = list(
    "OMOP_CDMv5.3_Field_Level.csv" = list(
      release = "v5.3.2",
      commit = "dd85c0d30bb3dd4bd16c5dbef7dbf9dd93075fa2",
      sha256 = "04dc596bb963d1c9827d07da885e347fb77acf7ecce298cc8fb26229e2246c0c"
    ),
    "OMOP_CDMv5.3_Table_Level.csv" = list(
      release = "v5.3.2",
      commit = "dd85c0d30bb3dd4bd16c5dbef7dbf9dd93075fa2",
      sha256 = "41a3141745b00ca6d2e4d159429d4245179c4887ee1ec096a203312cff33483f"
    ),
    "OMOP_CDMv5.4_Field_Level.csv" = list(
      release = "v5.4.2",
      commit = "aa047a3c620b5c842b4370a0c965e2aa72203b1d",
      sha256 = "94006d0fac2a3911b5665ce421468fa99af23fb51a633148e5fe6045916ad950"
    ),
    "OMOP_CDMv5.4_Table_Level.csv" = list(
      release = "v5.4.2",
      commit = "aa047a3c620b5c842b4370a0c965e2aa72203b1d",
      sha256 = "b2cc6b68dd229dec2f73fee5091e8b4bd44cdb1bdb7f6b410f05feaea31269d6"
    )
  )
)

.loadVerifiedVendoredMetadata <- function(pkg_dir) {
  fail <- function(detail) {
    stop("Vendored OHDSI metadata integrity verification failed: ", detail,
         ".", call. = FALSE)
  }
  if (!is.character(pkg_dir) || length(pkg_dir) != 1L || is.na(pkg_dir) ||
      !nzchar(pkg_dir) || !dir.exists(pkg_dir)) {
    fail("metadata directory is unavailable")
  }
  manifest_path <- file.path(pkg_dir, "UPSTREAM_METADATA.json")
  if (!file.exists(manifest_path) || !utils::file_test("-f", manifest_path)) {
    fail("manifest is unavailable")
  }
  manifest <- tryCatch(
    jsonlite::fromJSON(manifest_path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  expected <- .DSOMOP_VENDORED_OHDSI_METADATA
  if (!is.list(manifest) || is.null(names(manifest)) ||
      anyNA(names(manifest)) || anyDuplicated(names(manifest)) ||
      !setequal(names(manifest), c("contract_version", "source", "files")) ||
      !is.numeric(manifest$contract_version) ||
      length(manifest$contract_version) != 1L ||
      is.na(manifest$contract_version) ||
      !is.finite(manifest$contract_version) ||
      manifest$contract_version != expected$contract_version ||
      !identical(manifest$source, expected$source) ||
      !is.list(manifest$files) || is.null(names(manifest$files)) ||
      anyNA(names(manifest$files)) || anyDuplicated(names(manifest$files)) ||
      !setequal(names(manifest$files), names(expected$files))) {
    fail("manifest contract, source, or file set is invalid")
  }

  disk_files <- list.files(
    pkg_dir,
    pattern = "^OMOP_CDMv[0-9]+\\.[0-9]+_(Field|Table)_Level\\.csv$",
    full.names = FALSE
  )
  if (anyDuplicated(disk_files) ||
      !setequal(disk_files, names(expected$files))) {
    fail("installed file set does not match the pinned contract")
  }

  bytes <- stats::setNames(vector("list", length(expected$files)),
                           names(expected$files))
  for (name in names(expected$files)) {
    entry <- manifest$files[[name]]
    pinned <- expected$files[[name]]
    if (!is.list(entry) || is.null(names(entry)) || anyNA(names(entry)) ||
        anyDuplicated(names(entry)) ||
        !setequal(names(entry), c("release", "commit", "sha256")) ||
        !identical(entry$release, pinned$release) ||
        !identical(entry$commit, pinned$commit) ||
        !identical(entry$sha256, pinned$sha256) ||
        !grepl("^v[0-9]+\\.[0-9]+\\.[0-9]+$", entry$release) ||
        !grepl("^[0-9a-f]{40}$", entry$commit) ||
        !grepl("^[0-9a-f]{64}$", entry$sha256)) {
      fail(paste0("manifest entry is invalid for ", name))
    }
    path <- file.path(pkg_dir, name)
    info <- file.info(path)
    if (!file.exists(path) || !utils::file_test("-f", path) ||
        nrow(info) != 1L || is.na(info$size[[1L]]) || info$size[[1L]] < 1) {
      fail(paste0("pinned file is unavailable for ", name))
    }
    value <- tryCatch(
      readBin(path, what = "raw", n = info$size[[1L]]),
      error = function(e) raw()
    )
    observed <- unclass(as.character(openssl::sha256(value)))
    if (length(value) != info$size[[1L]] ||
        !identical(observed, pinned$sha256)) {
      fail(paste0("content hash does not match for ", name))
    }
    bytes[[name]] <- value
  }
  list(manifest = manifest, bytes = bytes)
}

.readVendoredCsvBytes <- function(value) {
  utils::read.csv(text = rawToChar(value), stringsAsFactors = FALSE)
}

# Normalize the current OHDSI CommonDataModel table metadata to the stable
# values historically exposed by the dsOMOP blueprint API. Official releases
# use VOCAB/RESULTS/CDM schema labels and upper-case conceptPrefix values with a
# trailing underscore (for example CONDITION_).
.normalizeCdmTableMetadata <- function(table_level) {
  schema <- trimws(as.character(table_level$schema))
  schema_key <- toupper(schema)
  schema_labels <- c(
    CDM = "CDM",
    VOCAB = "Vocabulary",
    VOCABULARY = "Vocabulary",
    RESULT = "Results",
    RESULTS = "Results"
  )
  known_schema <- !is.na(schema_key) & schema_key %in% names(schema_labels)
  schema[known_schema] <- unname(schema_labels[schema_key[known_schema]])
  table_level$schema <- schema

  prefix <- trimws(as.character(table_level$conceptPrefix))
  has_prefix <- !is.na(prefix) & nzchar(prefix)
  prefix[has_prefix] <- tolower(sub("_+$", "", prefix[has_prefix]))
  table_level$conceptPrefix <- prefix
  table_level
}

#' Load CDM spec for a given version using CommonDataModel package
#'
#' @param cdm_version Character; CDM version string (e.g. "5.4", "5.3")
#' @return List with table_level and field_level data.frames, or NULL if unsupported
#' @keywords internal
.loadCdmSpec <- function(cdm_version = NULL) {
  # Normalize version (e.g. "v5.4" -> "5.4", "5.4.0" -> "5.4")
  if (!is.null(cdm_version)) {
    cdm_version <- sub("^[vV]", "", trimws(cdm_version))
    cdm_version <- sub("\\.0$", "", cdm_version)
  }

  # Try an exact vendored specification first (no Java dependency).  The
  # vendored loader has a useful 5.4 default for an unknown/NULL version, but an
  # explicitly unsupported version must never be silently interpreted as 5.4:
  # doing so would attach the wrong OHDSI column and relationship semantics.
  vendored <- .loadVendoredSpec(cdm_version)
  if (!is.null(vendored) &&
      (is.null(cdm_version) || identical(vendored$version, cdm_version))) {
    return(vendored)
  }

  # Fall back to CommonDataModel package (may need Java)
  has_cdm_pkg <- tryCatch(
    requireNamespace("CommonDataModel", quietly = TRUE),
    warning = function(w) FALSE
  )
  if (!has_cdm_pkg) return(NULL)

  supported <- tryCatch(
    CommonDataModel::listSupportedVersions(),
    error = function(e) character(0)
  )

  # Find matching version
  version_to_load <- NULL
  if (!is.null(cdm_version) && cdm_version %in% supported) {
    version_to_load <- cdm_version
  } else if (!is.null(cdm_version) && length(supported) > 0) {
    for (sv in supported) {
      if (startsWith(cdm_version, sv)) { version_to_load <- sv; break }
    }
  }

  if (is.null(version_to_load)) return(NULL)

  pkg_csv <- system.file("csv", package = "CommonDataModel")
  tbl_file <- file.path(pkg_csv, paste0("OMOP_CDMv", version_to_load, "_Table_Level.csv"))
  fld_file <- file.path(pkg_csv, paste0("OMOP_CDMv", version_to_load, "_Field_Level.csv"))

  if (!file.exists(tbl_file) || !file.exists(fld_file)) {
    warning("CDM v", version_to_load,
            " spec files not found in CommonDataModel package.",
            call. = FALSE)
    return(NULL)
  }

  list(
    table_level = .normalizeCdmTableMetadata(
      utils::read.csv(tbl_file, stringsAsFactors = FALSE)
    ),
    field_level = utils::read.csv(fld_file, stringsAsFactors = FALSE),
    version     = version_to_load,
    source      = "CommonDataModel"
  )
}

#' Load vendored OHDSI metadata as fallback
#'
#' @param version Character; CDM version to load (e.g. "5.3", "5.4"). Defaults to "5.4".
#' @return data.frame with the CDM specification.
#' @keywords internal
.loadVendoredSpec <- function(version = NULL) {
  pkg_dir <- system.file("ohdsi", package = "dsOMOP")
  if (pkg_dir == "") {
    pkg_dir <- system.file("ohdsi", package = "dsOMOP", lib.loc = .libPaths())
  }
  verified <- .loadVerifiedVendoredMetadata(pkg_dir)
  provenance <- verified$manifest
  version <- sub("^[vV]", "", trimws(version %||% "5.4"))
  version <- sub("\\.0$", "", version)
  for (v in unique(c(version, "5.4"))) {
    tbl_file <- file.path(pkg_dir, paste0("OMOP_CDMv", v, "_Table_Level.csv"))
    fld_file <- file.path(pkg_dir, paste0("OMOP_CDMv", v, "_Field_Level.csv"))
    if (file.exists(tbl_file) && file.exists(fld_file)) {
      entries <- provenance$files[c(basename(tbl_file), basename(fld_file))]
      releases <- unique(vapply(entries, function(x) x$release %||% NA_character_,
                                character(1)))
      commits <- unique(vapply(entries, function(x) x$commit %||% NA_character_,
                               character(1)))
      return(list(
        table_level = .normalizeCdmTableMetadata(
          .readVendoredCsvBytes(verified$bytes[[basename(tbl_file)]])
        ),
        field_level = .readVendoredCsvBytes(
          verified$bytes[[basename(fld_file)]]
        ),
        version     = v,
        source      = "vendored",
        upstream_source = provenance$source %||% NA_character_,
        upstream_release = if (length(releases) == 1L) releases else NA_character_,
        upstream_commit = if (length(commits) == 1L) commits else NA_character_
      ))
    }
  }
  NULL
}

#' Heuristic concept role classification (no spec)
#'
#' @param table Character; the table name.
#' @param column Character; the column name to classify.
#' @return Character; one of "primary", "type", "source", "qualifier", or "other".
#' @keywords internal
.classifyConceptRoleHeuristic <- function(table, column) {
  if (grepl("_concept_id$", column)) {
    if (grepl("_source_concept_id$", column)) return("source_concept")
    if (grepl("_type_concept_id$", column)) return("type_concept")
    return("domain_concept")
  }
  "non_concept"
}

# --- Handle Creation ---

#' Stable identifier for a resource (for per-resource key derivation)
#'
#' Returns a string that is stable for "the same store" across reconnects and
#' DataSHIELD workspace save/load: the resource URL, else its name, else the
#' parsed host/database. Used only to derive a per-resource key file name; it is
#' never exposed and never used as a secret itself.
#' @keywords internal
.resourceIdentity <- function(resource_client) {
  res <- tryCatch(resource_client$getResource(), error = function(e) NULL)
  id <- tryCatch(res$url, error = function(e) NULL)
  if (is.null(id) || !nzchar(id)) id <- tryCatch(res$name, error = function(e) NULL)
  if (is.null(id) || !nzchar(id)) {
    id <- tryCatch(resource_client$getParsed()$server, error = function(e) NULL)
  }
  if (is.null(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    stop("A stable OMOP resource identity is required for pseudonymization.",
         call. = FALSE)
  }
  enc2utf8(as.character(id))
}

#' Resolve a persistent per-resource pseudonymization key
#'
#' Returns a raw secret key that is STABLE for a given resource across
#' reconnects and DataSHIELD workspace save/load, so a person hashes to the same
#' token every time without storing any token->id map. Resolution order:
#' \enumerate{
#'   \item Per-resource env var \code{DSOMOP_PSEUDONYM_KEY_<rid>} (exact key).
#'   \item Global env var \code{DSOMOP_PSEUDONYM_ROOT} or R option
#'         \code{dsomop.pseudonym_root}, treated as a node root from which a
#'         resource-separated key is derived with HMAC-SHA256.
#'   \item Legacy global env var \code{DSOMOP_PSEUDONYM_KEY} or R option
#'         \code{dsomop.pseudonym_key}, retained as an exact key for token
#'         compatibility only when the administrator explicitly enables
#'         \code{DSOMOP_ALLOW_LEGACY_GLOBAL_PSEUDONYMS}. It does not provide
#'         cross-resource separation.
#'   \item An existing legacy \code{0600} per-resource file at
#'         \code{<state>/keys/<rid>.key}, retained to avoid invalidating tokens.
#'   \item The runtime-created \code{0600} node root at
#'         \code{<state>/secrets/pseudonym_root}, also HMAC-separated by resource.
#' }
#' The key never leaves the server, so tokens cannot be reversed client-side.
#' @param resource_client An OMOPResourceClient instance.
#' @return Raw vector; the per-resource secret key.
#' @keywords internal
.resolvePersonKey <- function(resource_client) {
  identity <- .resourceIdentity(resource_client)
  .resolvePersonKeyIdentity(identity)
}

#' Resolve a pseudonymization key from an already validated resource identity
#'
#' Kept separate from \code{\link{.resolvePersonKey}} so session handles can store
#' only the non-secret identity and re-resolve key material when needed. Raw key
#' bytes are never cached in a serializable DataSHIELD workspace handle.
#'
#' @param identity Stable resource identity string.
#' @return Raw per-resource pseudonymization key.
#' @keywords internal
.resolvePersonKeyIdentity <- function(identity) {
  .resolvePersonKeyContract(identity)$key
}

#' Resolve key material and its non-secret lifecycle contract
#'
#' @param identity Stable resource identity string.
#' @return Internal list containing the raw key plus public provider metadata.
#' @keywords internal
.resolvePersonKeyContract <- function(identity) {
  if (!is.character(identity) || length(identity) != 1L || is.na(identity) ||
      !nzchar(identity)) {
    stop("A stable OMOP resource identity is required for pseudonymization.",
         call. = FALSE)
  }
  settings <- .dsomopPseudonymSettings()
  # Stable, filesystem-safe per-resource id (also used to scope the env var).
  rid <- substr(as.character(openssl::sha256(charToRaw(identity))), 1L, 32L)
  scoped_name <- paste0("DSOMOP_PSEUDONYM_KEY_", rid)

  finish <- function(key, provider) {
    list(
      key = key,
      provider = provider,
      key_id = .personKeyId(key),
      epoch = settings$epoch,
      require_existing = settings$require_existing,
      contract_version = 1L
    )
  }

  if (identical(settings$provider, "scoped")) {
    key <- settings$scoped[[scoped_name]]
    if (is.null(key)) {
      stop("The scoped pseudonymization provider has no key for this OMOP ",
           "resource.", call. = FALSE)
    }
    return(finish(key, "scoped"))
  }

  if (identical(settings$provider, "injected")) {
    root <- .dsomopSecretSettingValue(settings$root)$value
    return(finish(.deriveDsomopResourceKey(root, identity), "injected"))
  }

  if (identical(settings$provider, "file")) {
    root <- .ensureDsomopSecret(
      "pseudonym_root", require_existing = settings$require_existing)
    return(finish(.deriveDsomopResourceKey(root, identity), "file"))
  }

  # An explicitly scoped key is already unique to this resource.
  scoped <- settings$scoped[[scoped_name]]
  if (!is.null(scoped)) {
    return(finish(scoped, "scoped"))
  }

  # New, unambiguous root setting: derivation prevents cross-resource linkage.
  if (settings$root$present) {
    root <- .dsomopSecretSettingValue(settings$root)$value
    return(finish(.deriveDsomopResourceKey(root, identity), "injected"))
  }

  # Compatibility contract: this setting was historically the exact token key.
  # Reinterpreting it as a root would invalidate every existing p2 token.
  if (settings$legacy$present) {
    key <- .dsomopSecretSettingValue(settings$legacy)$value
    return(finish(key, "legacy"))
  }

  # Preserve a valid legacy per-resource key rather than silently changing all
  # previously issued tokens during the root-key migration.
  key_dir <- file.path(.dsomopStateRoot(), "keys")
  key_file <- file.path(key_dir, paste0(rid, ".key"))
  if (file.exists(key_file) || .dsomopIsSymlink(key_file)) {
    key_file <- .dsomopPrivateSecretDirectory(
      key_file,
      .allow_test_path = identical(
        Sys.getenv("DSOMOP_TEST_ALLOW_EPHEMERAL_STATE", unset = ""), "1"))
    return(finish(.dsomopValidateSecretFile(key_file), "legacy_file"))
  }

  root <- .ensureDsomopSecret(
    "pseudonym_root", require_existing = settings$require_existing)
  finish(.deriveDsomopResourceKey(root, identity), "file")
}

#' Non-secret identity of a resolved pseudonymization key
#'
#' Stored in a handle so changing an injected provider, losing a state volume,
#' or routing a workspace to a replica with a different root fails closed
#' instead of silently changing person tokens mid-session.
#'
#' @param key Raw pseudonymization key.
#' @return Character key identifier. It is a fingerprint, never key material.
#' @keywords internal
.personKeyId <- function(key) {
  if (!is.raw(key) || length(key) != 32L) {
    stop("Cannot identify an invalid pseudonymization key.", call. = FALSE)
  }
  paste0(
    "dsomop-person-key-v1:",
    paste(format(openssl::sha256(key)), collapse = "")
  )
}

#' Resolve the current handle's pseudonymization key without caching it
#'
#' Production handles created by \code{\link{.createHandle}} store only
#' \code{person_key_identity}. A historical handle containing raw
#' \code{person_key} bytes is accepted only under the explicit administrative
#' legacy opt-in and only when it contains exactly 32 bytes. There is no safe
#' implicit migration because the original resource identity cannot be proven;
#' absent that opt-in the handle fails closed and must be recreated.
#'
#' @param handle A CDM handle.
#' @return Raw pseudonymization key.
#' @keywords internal
.personKey <- function(handle) {
  identity <- handle$person_key_identity
  has_identity <- is.character(identity) && length(identity) == 1L &&
    !is.na(identity) && nzchar(identity)
  if (!has_identity) {
    legacy <- handle$person_key
    if (!is.null(legacy)) {
      if (!is.raw(legacy) || length(legacy) != 32L) {
        stop("A legacy handle pseudonymization key must contain exactly 32 raw ",
             "bytes; recreate the OMOP handle.", call. = FALSE)
      }
      if (!isTRUE(
        .dsomopPseudonymLifecycleSettings()$allow_legacy_global
      )) {
        stop("Raw person_key handles are disabled by default; recreate the ",
             "OMOP handle, or explicitly enable the legacy global ",
             "pseudonymization opt-in for a controlled migration.",
             call. = FALSE)
      }
      return(legacy)
    }
    stop("No pseudonymization key provider is available on this handle.",
         call. = FALSE)
  }
  contract <- .resolvePersonKeyContract(identity)
  current_id <- contract$key_id
  expected_id <- handle$person_key_id
  expected_provider <- handle$person_key_provider
  expected_epoch <- handle$person_key_epoch
  expected_require_existing <- handle$person_key_require_existing
  expected_contract_version <- handle$person_key_contract_version
  changed <- (!is.null(expected_id) &&
    (!is.character(expected_id) || length(expected_id) != 1L ||
     is.na(expected_id) || !identical(expected_id, current_id))) ||
    (!is.null(expected_provider) &&
     (!is.character(expected_provider) || length(expected_provider) != 1L ||
      is.na(expected_provider) ||
      !identical(expected_provider, contract$provider))) ||
    (!is.null(expected_epoch) &&
     (!is.numeric(expected_epoch) || length(expected_epoch) != 1L ||
      is.na(expected_epoch) || !is.finite(expected_epoch) ||
      expected_epoch != floor(expected_epoch) || expected_epoch < 1 ||
      !identical(as.integer(expected_epoch), contract$epoch))) ||
    (!is.null(expected_require_existing) &&
     (!is.logical(expected_require_existing) ||
      length(expected_require_existing) != 1L ||
      is.na(expected_require_existing) ||
      !identical(expected_require_existing, contract$require_existing))) ||
    (!is.null(expected_contract_version) &&
     (!is.numeric(expected_contract_version) ||
      length(expected_contract_version) != 1L ||
      is.na(expected_contract_version) ||
      !is.finite(expected_contract_version) ||
      expected_contract_version != floor(expected_contract_version) ||
      expected_contract_version < 1 ||
      !identical(as.integer(expected_contract_version),
                 contract$contract_version)))
  if (changed) {
    stop("The pseudonymization key contract changed after this handle was created; ",
         "refusing to emit inconsistent person tokens.", call. = FALSE)
  }
  if (is.environment(handle)) {
    if (is.null(expected_id)) handle$person_key_id <- current_id
    if (is.null(expected_provider)) {
      handle$person_key_provider <- contract$provider
    }
    if (is.null(expected_epoch)) handle$person_key_epoch <- contract$epoch
    if (is.null(expected_require_existing)) {
      handle$person_key_require_existing <- contract$require_existing
    }
    if (is.null(expected_contract_version)) {
      handle$person_key_contract_version <- contract$contract_version
    }
  }
  contract$key
}

#' Public, non-secret pseudonymization contract for capability reports
#'
#' @param handle A CDM handle.
#' @return A list that never contains raw key material or resource identity.
#' @keywords internal
.personKeyPublicContract <- function(handle) {
  legacy_global_opt_in <- isTRUE(
    .dsomopPseudonymLifecycleSettings()$allow_legacy_global
  )
  identity <- handle$person_key_identity
  if (is.character(identity) && length(identity) == 1L && !is.na(identity) &&
      nzchar(identity)) {
    invisible(.personKey(handle))
    return(list(
      available = TRUE,
      contract_version = handle$person_key_contract_version,
      provider = handle$person_key_provider,
      key_id = handle$person_key_id,
      epoch = handle$person_key_epoch,
      require_existing = handle$person_key_require_existing,
      legacy_global_opt_in = legacy_global_opt_in
    ))
  }
  legacy <- handle$person_key
  if (!is.null(legacy)) {
    legacy <- .personKey(handle)
    return(list(
      available = TRUE,
      contract_version = 0L,
      provider = "legacy_handle",
      key_id = .personKeyId(legacy),
      epoch = NULL,
      require_existing = NULL,
      legacy_global_opt_in = legacy_global_opt_in
    ))
  }
  list(
    available = FALSE,
    contract_version = NULL,
    provider = NULL,
    key_id = NULL,
    epoch = NULL,
    require_existing = NULL,
    legacy_global_opt_in = legacy_global_opt_in
  )
}

#' Create a CDM handle from a resource client
#'
#' Resolves the connection, schemas and the per-resource pseudonymization key,
#' and builds the handle environment used by all extraction/exploration ops.
#' @param resource_client An OMOPResourceClient instance. Ownership transfers to
#'   the returned handle; the client is closed if construction fails.
#' @param cdm_schema Character; override CDM schema
#' @param vocab_schema Character; override vocabulary schema
#' @param results_schema Character; override results schema
#' @param temp_schema Character; override temp schema
#' @param config Named list; additional configuration
#' @return A CDM handle (environment)
#' @keywords internal
.createHandle <- function(resource_client,
                          cdm_schema = NULL,
                          vocab_schema = NULL,
                          results_schema = NULL,
                          temp_schema = NULL,
                          config = list()) {
  # Ownership transfers to the handle constructor. If any validation or key
  # bootstrap step fails after opening the DB connection, close the resource
  # client so a failed initialization cannot strand a connection.
  tryCatch({

  conn <- resource_client$getConnection()
  parsed <- resource_client$getParsed()
  dbms <- parsed$dbms

  # Per-DBMS default namespace, used only when no CDM schema is supplied.
  default_schema <- .dbmsDefaultSchema(
    dbms,
    database = parsed$database,
    user     = tryCatch(resource_client$getResource()$identity,
                        error = function(e) NULL)
  )

  # Schema resolution (explicit override > URL > DBMS default). The vocabulary
  # schema falls back to the CDM schema, so: neither set -> both default; only
  # CDM set -> both that; only vocab set -> CDM default + vocab apart; both set
  # -> one each.
  cdm_schema     <- cdm_schema     %||% parsed$cdm_schema %||% default_schema
  vocab_schema   <- vocab_schema   %||% parsed$vocabulary_schema %||% cdm_schema
  results_schema <- results_schema %||% parsed$results_schema
  temp_schema    <- temp_schema    %||% parsed$temp_schema

  # Schema names ultimately become SQL identifiers in .qualifyTable(). They
  # may be client-supplied overrides, so validate them before storing the
  # handle. Dotted identifiers remain available for engines that use a
  # database/schema namespace; quotes, comments and statement separators do
  # not.
  validate_schema <- function(x, label) {
    if (is.null(x)) return(NULL)
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
      stop(label, " must be one non-empty schema identifier.", call. = FALSE)
    }
    .validateSchemaNamespace(dbms, x, label)
  }
  cdm_schema <- validate_schema(cdm_schema, "cdm_schema")
  vocab_schema <- validate_schema(vocab_schema, "vocab_schema")
  results_schema <- validate_schema(results_schema, "results_schema")
  temp_schema <- validate_schema(temp_schema, "temp_schema")

  if (identical(.normalizeDBMS(dbms), "sqlite")) {
    configured <- Filter(Negate(is.null), list(
      cdm_schema = cdm_schema, vocab_schema = vocab_schema,
      results_schema = results_schema, temp_schema = temp_schema
    ))
    invalid <- names(configured)[vapply(
      configured, function(x) !identical(tolower(x), "main"), logical(1)
    )]
    if (length(invalid) > 0L) {
      stop("SQLite resources open one database file and only support the main ",
           "namespace; unsupported setting(s): ",
           paste(invalid, collapse = ", "), ".", call. = FALSE)
    }
  }

  handle <- new.env(parent = emptyenv())
  handle$conn            <- conn
  handle$dbms            <- dbms
  handle$target_dialect  <- .resolve_target_dialect(dbms)
  handle$cdm_schema      <- cdm_schema
  handle$vocab_schema    <- vocab_schema
  handle$results_schema  <- results_schema
  handle$temp_schema     <- temp_schema
  handle$resource_client <- resource_client
  handle$config          <- config
  handle$blueprint       <- NULL
  handle$temp_tables     <- character(0)
  handle$temp_connection <- NULL
  handle$staging_dirs    <- character(0)
  handle$dbms_version    <- NULL
  handle$physical_table_names <- list()
  .assertAnalyticDbmsSupport(handle, "dsOMOP connection")
  # Store only the stable, non-secret key locator. Key bytes are resolved from
  # the injected provider or private state file for each operation and are never
  # embedded in a serializable DataSHIELD workspace handle.
  handle$person_key_identity <- .resourceIdentity(resource_client)
  person_key_contract <- .resolvePersonKeyContract(handle$person_key_identity)
  handle$person_key_id <- person_key_contract$key_id
  handle$person_key_provider <- person_key_contract$provider
  handle$person_key_epoch <- person_key_contract$epoch
  handle$person_key_require_existing <-
    person_key_contract$require_existing
  handle$person_key_contract_version <-
    person_key_contract$contract_version

  handle
  }, error = function(e) {
    close <- tryCatch(resource_client$close, error = function(close_error) NULL)
    cleanup_error <- if (is.function(close)) {
      tryCatch({
        close()
        NULL
      }, error = identity)
    } else {
      NULL
    }
    if (!is.null(cleanup_error)) {
      stop("OMOP handle construction failed and resource-client cleanup could ",
           "not be proven. Initialization error: ", conditionMessage(e),
           "; cleanup error: ", conditionMessage(cleanup_error), call. = FALSE)
    }
    stop(e)
  })
}

#' Enforce the server-side allowlist for client-supplied schema overrides
#'
#' Schema names carried by the resource URL are controlled by the data owner.
#' In contrast, arguments to \code{omopInitDS()} originate with the analyst and
#' must not redirect a broadly privileged connection to another tenant. An
#' administrator can opt in to exact values with the named server option
#' \code{dsomop.allowed_schema_overrides}; absent that option, every client
#' override is rejected.
#'
#' @param overrides Named list with cdm_schema, vocab_schema, results_schema and
#'   temp_schema values.
#' @return NULL, invisibly; errors on a non-allowlisted override.
#' @keywords internal
.assertAllowedSchemaOverrides <- function(overrides) {
  supplied <- !vapply(overrides, is.null, logical(1))
  if (!any(supplied)) return(invisible(NULL))

  allowlist <- getOption(
    "dsomop.allowed_schema_overrides",
    getOption("default.dsomop.allowed_schema_overrides", NULL)
  )
  if (!is.list(allowlist) || is.null(names(allowlist))) {
    stop("Client schema overrides are disabled by the data controller.",
         call. = FALSE)
  }

  for (field in names(overrides)[supplied]) {
    value <- overrides[[field]]
    allowed <- allowlist[[field]]
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !is.character(allowed) || !value %in% allowed) {
      stop("Schema override for '", field,
           "' is not allowlisted by the data controller.", call. = FALSE)
    }
  }
  invisible(NULL)
}

#' Close a CDM handle
#'
#' @param handle A CDM handle
#' @return NULL, called for side effect of closing the database connection.
#' @keywords internal
.closeHandle <- function(handle) {
  if (is.null(handle)) return(invisible(NULL))

  failures <- character(0)
  record_failure <- function(e) {
    failures <<- c(failures, conditionMessage(e))
    NULL
  }

  conn <- tryCatch(.conn(handle), error = record_failure)

  if (!is.null(conn) &&
      isTRUE(tryCatch(DBI::dbIsValid(conn), error = record_failure)) &&
      length(handle$temp_tables) > 0L) {
    for (tbl in handle$temp_tables) {
      tryCatch(.dropTempTable(handle, tbl), error = record_failure)
    }
  }

  if (length(handle$staging_dirs %||% character(0)) > 0L) {
    tryCatch(.cleanupHandleStaging(handle), error = record_failure)
  }

  if (!is.null(handle$resource_client)) {
    tryCatch(handle$resource_client$close(), error = record_failure)
  } else if (!is.null(conn) &&
             isTRUE(tryCatch(DBI::dbIsValid(conn),
                             error = record_failure))) {
    tryCatch(DBI::dbDisconnect(conn), error = record_failure)
  }
  if (length(failures) > 0L) {
    stop("Could not fully close the OMOP handle: ",
         paste(unique(failures), collapse = "; "), call. = FALSE)
  }
  invisible(NULL)
}

# --- Connection Resolution & Transparent Reconnect ---

#' Resolve the LIVE database connection for a handle
#'
#' The handle caches a connection snapshot in \code{handle$conn} taken at
#' creation time, but that snapshot can go stale when a pooled/expiring
#' resource connection is closed underneath us. When the handle owns a
#' \code{resource_client}, defer to \code{resource_client$getConnection()},
#' which revalidates with \code{DBI::dbIsValid()} and transparently reconnects
#' if needed. The freshly resolved connection is cached back into
#' \code{handle$conn} so disclosure helpers that still read the field directly
#' see the live handle. Falls back to \code{handle$conn} when there is no
#' resource_client (e.g. test handles built by \code{create_test_handle()}).
#'
#' @param handle A CDM handle.
#' @return A live DBI connection.
#' @keywords internal
.conn <- function(handle) {
  rc <- handle$resource_client
  if (!is.null(rc)) {
    conn <- rc$getConnection()
    handle$conn <- conn
    return(conn)
  }
  handle$conn
}

#' Is an error a database CONNECTION-class failure (vs a SQL/logic error)?
#'
#' Matches the messages DBI/driver layers raise when the underlying connection
#' is closed, lost, or expired — the cases a one-shot reconnect can recover.
#' Deliberately conservative: it must NOT match genuine SQL/logic errors
#' (syntax, missing column, constraint, permission), because retrying those is
#' pointless and could mask a real problem. "no such table"/"does not exist"
#' are handled separately by \code{.isMissingObjectError} and are NOT treated
#' as connection errors here.
#'
#' @param e A condition/error object.
#' @return \code{TRUE} if the error looks like a lost/closed/expired connection.
#' @keywords internal
.isConnectionError <- function(e) {
  msg <- tolower(conditionMessage(e))
  patterns <- c(
    "connection.*(closed|lost|expired|reset|terminated|not open|is closed|was closed|already closed|do not exist|does not exist)",
    "(lost|broken|closed|expired|stale|dead|invalid).*connection",
    "no connection to the server",
    "could not (connect|receive data|send data)",
    "server closed the connection",
    "terminating connection",
    "ssl connection has been closed",
    "ssl syscall error",
    "eof detected",
    "bad connection",
    "connection timed out",
    "failed to connect",
    "server has gone away",         # MySQL/MariaDB
    "mysql server has gone away",
    "lost connection to mysql",
    "ora-03114",                    # Oracle: not connected to ORACLE
    "ora-03113",                    # Oracle: end-of-file on communication channel
    "ora-03135",                    # Oracle: connection lost contact
    "ora-12537",                    # Oracle: TNS connection closed
    "08s01", "08003", "08006", "08001", "08004",  # SQLSTATE connection-exception class
    "communication link failure"    # ODBC / SQL Server
  )
  any(vapply(patterns, function(p) grepl(p, msg), logical(1)))
}

#' Is an error a "missing database object" failure (e.g. vanished temp table)?
#'
#' A reconnect DROPS all session-scoped TEMP tables, so any dsOMOP cohort/plan
#' temp table created before the reconnect disappears. A query that then runs
#' against it fails with a "no such table" / "relation does not exist" error.
#' This predicate detects that case so callers can FAIL CLOSED instead of
#' silently running against a vanished table and returning an under-populated
#' (gate-evading) result.
#'
#' @param e A condition/error object.
#' @return \code{TRUE} if the error indicates a missing table/relation.
#' @keywords internal
.isMissingObjectError <- function(e) {
  msg <- tolower(conditionMessage(e))
  patterns <- c(
    "no such table",                       # SQLite
    "relation .* does not exist",          # PostgreSQL / Redshift
    "table or view does not exist",        # Oracle (ORA-00942)
    "ora-00942",
    "doesn't exist",                       # MySQL/MariaDB
    "invalid object name",                 # SQL Server
    "object .* not found",                 # generic / Spark
    "table .* not found",
    "cannot find .* table",
    "undefined table"
  )
  any(vapply(patterns, function(p) grepl(p, msg), logical(1)))
}

#' Run a DB operation with transparent one-shot reconnect
#'
#' Executes \code{fn(conn)} against the handle's live connection. On a
#' CONNECTION-class failure (see \code{\link{.isConnectionError}}) it closes the
#' stale connection, re-resolves a fresh one via \code{\link{.conn}} (which goes
#' through \code{resource_client$getConnection()} and reconnects), and retries
#' the operation EXACTLY ONCE. A second failure — or any non-connection error —
#' propagates unchanged, so genuine SQL/logic errors are never silently retried.
#'
#' Disclosure-critical fail-closed behaviour: a reconnect drops session TEMP
#' tables, so any dsOMOP cohort/working temp table created earlier in the
#' session vanishes. If a query fails because such an object is now missing
#' (see \code{\link{.isMissingObjectError}}) WHILE this handle still has
#' registered temp tables, we STOP with a clear, actionable error telling the
#' analyst to re-run the cohort/session step. This covers BOTH paths by which
#' the loss surfaces: (a) the cached connection was already invalid, so
#' \code{.conn()} silently handed us a fresh connection and the very first
#' attempt hits a vanished table; and (b) the connection died mid-call and the
#' post-reconnect retry hits a vanished table. We never let a query proceed
#' against a vanished temp table, which would otherwise return an
#' empty/under-populated result able to slip past the per-patient gate.
#'
#' @param handle A CDM handle.
#' @param fn A function taking a single argument, the DBI connection.
#' @return The value returned by \code{fn}.
#' @keywords internal
.withDbReconnect <- function(handle, fn) {
  # Fail closed when a query hits a missing object but dsOMOP is still tracking
  # session temp tables — i.e. a reconnect dropped the cohort/working tables.
  .stopIfTempTablesLost <- function(e) {
    temps <- handle$temp_tables
    if (.isMissingObjectError(e) && length(temps) > 0) {
      # Only fail closed if the MISSING object is one of OUR tracked temp tables
      # (a reconnect dropped the cohort/working table). A missing CDM/other table
      # is a different, legitimate error and must propagate — not be masked as a
      # dropped cohort, which would hide real bugs.
      msg <- tolower(conditionMessage(e))
      hit <- any(vapply(temps, function(t)
        nzchar(t) && grepl(tolower(t), msg, fixed = TRUE), logical(1)))
      if (hit) {
        stop("Database connection was renewed, which dropped this session's ",
             "temporary cohort/working table(s). The previous result cannot be ",
             "reproduced safely. Re-run the cohort/session step (e.g. ",
             "ds.omop.cohort.*) and then retry this operation.", call. = FALSE)
      }
    }
    invisible(NULL)
  }

  conn <- .conn(handle)
  tryCatch(
    fn(conn),
    error = function(e) {
      # A vanished temp table can surface on the FIRST attempt if .conn() had to
      # hand back a freshly reconnected connection (old snapshot was invalid).
      .stopIfTempTablesLost(e)
      if (!.isConnectionError(e)) stop(e)

      # Connection looks dead: drop the stale snapshot and force a fresh
      # connection on the next .conn() call.
      if (!is.null(handle$resource_client)) {
        try(handle$resource_client$close(), silent = TRUE)
      } else if (!is.null(handle$conn)) {
        try(DBI::dbDisconnect(handle$conn), silent = TRUE)
        handle$conn <- NULL
      }

      conn2 <- .conn(handle)
      tryCatch(
        fn(conn2),
        error = function(e2) {
          # The reconnect wiped session temp tables. Fail closed rather than
          # run against a vanished cohort/session table and under-count.
          .stopIfTempTablesLost(e2)
          stop(e2)
        }
      )
    }
  )
}

# --- Blueprint Construction ---

#' Read the server-authorized CDM extension contract
#'
#' Non-standard tables and columns are invisible to the extraction blueprint
#' unless the data controller explicitly lists them here. The option is a named
#' list mapping a bare table name to one or more bare column names, for example
#' \code{list(site_event = c("person_id", "event_date", "value"),
#' measurement = "site_quality_flag")}. An entry for a standard CDM table
#' authorizes only the listed additional columns; an entry for a non-standard
#' table authorizes the table and only those columns. Wildcards are deliberately
#' unsupported so a later schema change cannot silently expand the release
#' surface. Listing a column controls visibility, not identifier semantics:
#' extension names ending in \code{_id}, \code{_key}, or \code{_identifier}
#' remain denied unless they are the person/subject key or an explicitly listed
#' OMOP-shaped \code{*_concept_id} column.
#'
#' @return Named list of lower-case table-to-column mappings.
#' @keywords internal
.allowedCdmExtensionContract <- function() {
  contract <- getOption(
    "dsomop.allowed_cdm_extensions",
    getOption("default.dsomop.allowed_cdm_extensions", list())
  )
  if (is.null(contract)) contract <- list()
  if (!is.list(contract) || (length(contract) > 0L &&
      (is.null(names(contract)) || anyNA(names(contract)) ||
       any(!nzchar(names(contract)))))) {
    stop("dsomop.allowed_cdm_extensions must be a named list mapping table ",
         "names to explicit column-name vectors.", call. = FALSE)
  }
  if (length(contract) == 0L) return(list())

  table_names <- tolower(trimws(names(contract)))
  if (anyDuplicated(table_names)) {
    stop("dsomop.allowed_cdm_extensions contains duplicate table names.",
         call. = FALSE)
  }
  valid_bare_name <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) &&
      grepl("^[A-Za-z_][A-Za-z0-9_]*$", x)
  }
  if (!all(vapply(table_names, valid_bare_name, logical(1)))) {
    stop("dsomop.allowed_cdm_extensions contains an invalid table name; ",
         "only bare SQL identifiers are permitted.", call. = FALSE)
  }

  out <- vector("list", length(contract))
  names(out) <- table_names
  for (i in seq_along(contract)) {
    cols <- contract[[i]]
    if (!is.character(cols) || length(cols) == 0L || anyNA(cols)) {
      stop("Extension contract for table '", table_names[i],
           "' must contain one or more explicit column names.",
           call. = FALSE)
    }
    cols <- tolower(trimws(cols))
    if (!all(vapply(cols, valid_bare_name, logical(1)))) {
      stop("Extension contract for table '", table_names[i],
           "' contains an invalid column name; wildcards and qualified ",
           "identifiers are not permitted.", call. = FALSE)
    }
    out[[i]] <- unique(cols)
  }
  out
}

#' Build the SchemaBlueprint for a handle
#'
#' Fuses vendored OHDSI metadata with runtime DB introspection.
#'
#' @param handle CDM handle
#' @param force Logical; rebuild even if already cached
#' @return The blueprint (also stored in handle$blueprint)
#' @keywords internal
.buildBlueprint <- function(handle, force = FALSE) {
  if (!is.null(handle$blueprint) && !force) {
    return(handle$blueprint)
  }

  # Step 1: Discover tables actually present in the DB
  db_tables_cdm <- .listTablesRaw(handle, handle$cdm_schema)

  db_tables_vocab <- character(0)
  if (!is.null(handle$vocab_schema) && handle$vocab_schema != handle$cdm_schema) {
    db_tables_vocab <- .listTablesRaw(handle, handle$vocab_schema)
  }

  # Resolve (and cache) the schema holding Achilles/results tables. Mirrors
  # vocab_schema: honors an explicit pin, else auto-detects the OHDSI "results"
  # daimon (probe results -> <cdm>_results -> cdm -> default). NULL on sqlite or
  # when no dedicated results schema exists.
  results_schema <- .resolveResultsSchema(handle)

  db_tables_results <- character(0)
  if (!is.null(results_schema) && results_schema != handle$cdm_schema) {
    db_tables_results <- .listTablesRaw(handle, results_schema)
  }

  # Step 2: Detect CDM version from cdm_source (before spec loading)
  # Only the configured CDM namespace is authoritative. A same-named table in
  # vocabulary/results must never make a missing CDM table appear present.
  cdm_info <- .detectCDMInfo(handle, db_tables_cdm)
  cdm_version <- cdm_info$cdm_version  # may be NULL

  # Step 2b: Structural version detection (fallback / cross-validation)
  # Wrapped in tryCatch so structural detection can never crash blueprint build.
  struct <- tryCatch(
    .detectCDMVersionFromStructure(handle, db_tables_cdm),
    error = function(e) NULL
  )

  if (is.null(cdm_version) && !is.null(struct)) {
    cdm_version <- struct$version
    message("CDM version ", cdm_version, " inferred from table structure",
            " (evidence: v5.4=", struct$evidence_54,
            ", v5.3=", struct$evidence_53, ")")
  } else if (!is.null(cdm_version) && !is.null(struct)) {
    # cdm_source is authoritative; structural detection is advisory only
    normalized <- sub("^[vV]", "", trimws(cdm_version))
    normalized <- sub("\\.0$", "", normalized)
    if (normalized != struct$version) {
      warning("cdm_source reports version '", cdm_version,
              "' but table structure suggests '", struct$version,
              "' (evidence: v5.4=", struct$evidence_54,
              ", v5.3=", struct$evidence_53, "). Using cdm_source version.",
              call. = FALSE)
    }
  }

  # Step 3: Load spec for detected version
  spec <- .loadCdmSpec(cdm_version)
  has_spec <- !is.null(spec)
  if (!has_spec) {
    stop("No supported OHDSI CDM specification is available for version '",
         cdm_version %||% "unknown", "'. Refusing schema introspection because ",
         "unknown tables and columns cannot be classified safely.",
         call. = FALSE)
  }

  extension_contract <- .allowedCdmExtensionContract()

  tbl_meta <- if (has_spec) spec$table_level else NULL
  fld_meta <- if (has_spec) spec$field_level else NULL

  # Build tables data.frame
  if (has_spec) {
    tables <- data.frame(
      table_name      = tolower(tbl_meta$cdmTableName),
      schema_category = tbl_meta$schema,
      concept_prefix  = tbl_meta$conceptPrefix,
      has_person_id   = logical(nrow(tbl_meta)),
      present_in_db   = logical(nrow(tbl_meta)),
      qualified_name  = character(nrow(tbl_meta)),
      stringsAsFactors = FALSE
    )

    # Determine which tables exist in the namespace selected by their OHDSI
    # schema category. A union creates false positives when separate namespaces
    # contain same-named objects.
    for (i in seq_len(nrow(tables))) {
      tbl_name <- tables$table_name[i]
      category <- tables$schema_category[i]
      category_lower <- tolower(category)
      category_tables <- if (category_lower %in% c("vocabulary", "vocab")) {
        if (is.null(handle$vocab_schema) ||
            identical(handle$vocab_schema, handle$cdm_schema)) {
          db_tables_cdm
        } else {
          db_tables_vocab
        }
      } else if (category_lower %in% c("results", "result")) {
        if (is.null(results_schema) ||
            identical(results_schema, handle$cdm_schema)) {
          db_tables_cdm
        } else {
          db_tables_results
        }
      } else {
        db_tables_cdm
      }
      tables$present_in_db[i] <- tbl_name %in% category_tables

      schema <- .resolveTableSchema(handle, tbl_name, category)
      tables$qualified_name[i] <- .qualifyTable(handle, tbl_name, schema)
    }

    # Non-standard tables are invisible by default. A data-controller-owned
    # contract may expose an exact table/column surface from the CDM schema.
    # Do not use another namespace here: a same-named object in vocabulary or
    # results must not be mistaken for an authorized CDM extension.
    extra_db <- intersect(
      setdiff(db_tables_cdm, tables$table_name),
      names(extension_contract)
    )
    if (length(extra_db) > 0) {
      extra_rows <- data.frame(
        table_name      = extra_db,
        schema_category = rep("CDM", length(extra_db)),
        concept_prefix  = rep(NA_character_, length(extra_db)),
        has_person_id   = rep(FALSE, length(extra_db)),
        present_in_db   = rep(TRUE, length(extra_db)),
        qualified_name  = vapply(extra_db, function(t) {
          .qualifyTable(handle, t, handle$cdm_schema)
        }, character(1)),
        stringsAsFactors = FALSE
      )
      tables <- rbind(tables, extra_rows)
    }
  }

  # Build columns: named list of data.frames per table
  columns <- list()
  for (tbl_name in tables$table_name[tables$present_in_db]) {
    tbl_flds <- if (has_spec) {
      fld_meta[tolower(fld_meta$cdmTableName) == tbl_name, , drop = FALSE]
    } else {
      data.frame(cdmFieldName = character(0), stringsAsFactors = FALSE)
    }
    category <- tables$schema_category[tables$table_name == tbl_name]
    schema <- .resolveTableSchema(handle, tbl_name, category)

    # Get actual DB columns
    db_cols <- .listColumnsRaw(handle, tbl_name, schema)
    concept_prefix <- tables$concept_prefix[tables$table_name == tbl_name]

    if (nrow(db_cols) == 0) next

    # Standard tables inherit exactly the columns declared by the loaded OHDSI
    # specification. Additional columns, and every column of a non-standard
    # table, require an explicit server-side contract entry. This filtering
    # happens before any heuristic type/role classification, so an unfamiliar
    # field can never become selectable merely because its name looks benign.
    standard_cols <- if (nrow(tbl_flds) > 0L) {
      tolower(tbl_flds$cdmFieldName)
    } else {
      character(0)
    }
    authorized_extension_cols <- extension_contract[[tbl_name]] %||%
      character(0)
    allowed_cols <- union(standard_cols, authorized_extension_cols)
    db_cols <- db_cols[db_cols$column_name %in% allowed_cols, , drop = FALSE]
    if (nrow(db_cols) == 0) next

    # Build column metadata by merging spec + DB
    col_df <- data.frame(
      column_name  = db_cols$column_name,
      cdm_datatype = character(nrow(db_cols)),
      db_datatype  = db_cols$data_type,
      concept_role = character(nrow(db_cols)),
      fk_domain    = character(nrow(db_cols)),
      is_date      = logical(nrow(db_cols)),
      is_extension = !db_cols$column_name %in% standard_cols,
      is_untyped_identifier = logical(nrow(db_cols)),
      is_sensitive  = logical(nrow(db_cols)),
      is_blocked   = logical(nrow(db_cols)),
      stringsAsFactors = FALSE
    )

    for (j in seq_len(nrow(col_df))) {
      col_name <- col_df$column_name[j]

      if (has_spec && nrow(tbl_flds) > 0) {
        ohdsi_row <- tbl_flds[tolower(tbl_flds$cdmFieldName) == col_name, , drop = FALSE]

        if (nrow(ohdsi_row) > 0) {
          col_df$cdm_datatype[j] <- ohdsi_row$cdmDatatype[1]
          fk_domain <- ohdsi_row$fkDomain[1]
          col_df$fk_domain[j] <- if (is.na(fk_domain)) "" else fk_domain
          is_fk <- ohdsi_row$isForeignKey[1]
          fk_table <- ohdsi_row$fkTableName[1]

          col_df$concept_role[j] <- .classifyConceptRole(
            tbl_name, col_name, concept_prefix,
            col_df$fk_domain[j],
            is_fk = (!is.na(is_fk) && toupper(is_fk) == "YES"),
            fk_table = if (is.na(fk_table)) "" else fk_table
          )
        } else {
          col_df$concept_role[j] <- .classifyConceptRoleHeuristic(tbl_name, col_name)
        }
      } else {
        # Introspection-only: use heuristic classification
        col_df$concept_role[j] <- .classifyConceptRoleHeuristic(tbl_name, col_name)
      }

      col_df$is_date[j] <- grepl("_date$|_datetime$", col_name) ||
        grepl("^date$|^datetime$", tolower(col_df$cdm_datatype[j]))

      # An explicit extension allow-list controls visibility, not semantics.
      # Therefore an unfamiliar identifier-shaped field remains denied even
      # when the controller listed it. Person keys have a dedicated
      # pseudonymization contract and *_concept_id fields have a reviewed OMOP
      # concept role; every other extension id/key/identifier is untyped.
      col_df$is_untyped_identifier[j] <-
        isTRUE(col_df$is_extension[j]) &&
        length(.untypedIdentifierColumns(
          col_name, reviewed = .PERSON_KEY_COLS(), allow_concepts = TRUE
        )) > 0L
      col_df$is_sensitive[j] <- .detectSensitiveColumns(col_name)
      col_df$is_blocked[j] <- col_df$is_sensitive[j] ||
        col_df$is_untyped_identifier[j]
    }

    columns[[tbl_name]] <- col_df

    # Update has_person_id in tables
    tables$has_person_id[tables$table_name == tbl_name] <-
      "person_id" %in% col_df$column_name
  }

  # Discover Achilles tables in results_schema (not in OHDSI spec CSVs)
  achilles_table_names <- c("achilles_analysis", "achilles_results",
                             "achilles_results_dist", "achilles_heel_results")
  effective_results_tables <- if (is.null(results_schema) ||
                                   identical(results_schema,
                                             handle$cdm_schema)) {
    db_tables_cdm
  } else {
    db_tables_results
  }
  found_achilles <- intersect(tolower(effective_results_tables),
                              achilles_table_names)
  # Avoid duplicating tables already in the tables data.frame
  new_achilles <- setdiff(found_achilles, tables$table_name)
  if (length(new_achilles) > 0) {
    achilles_schema <- .effectiveResultsSchema(handle)
    achilles_rows <- data.frame(
      table_name      = new_achilles,
      schema_category = rep("Results", length(new_achilles)),
      concept_prefix  = rep(NA_character_, length(new_achilles)),
      has_person_id   = rep(FALSE, length(new_achilles)),
      present_in_db   = rep(TRUE, length(new_achilles)),
      qualified_name  = vapply(new_achilles, function(t) {
        .qualifyTable(handle, t, achilles_schema)
      }, character(1)),
      stringsAsFactors = FALSE
    )
    tables <- rbind(tables, achilles_rows)
  }
  # Also mark already-present achilles tables as present_in_db
  existing_achilles <- intersect(found_achilles, tables$table_name)
  if (length(existing_achilles) > 0) {
    achilles_schema <- .effectiveResultsSchema(handle)
    mask <- tables$table_name %in% existing_achilles
    tables$present_in_db[mask] <- TRUE
    tables$schema_category[mask & tables$schema_category == "CDM"] <- "Results"
    tables$qualified_name[mask] <- vapply(tables$table_name[mask], function(t) {
      .qualifyTable(handle, t, achilles_schema)
    }, character(1))
  }
  handle$has_achilles <- length(found_achilles) > 0

  # Discover OHDSI result tables (CohortDiagnostics, etc.)
  registry <- .ohdsi_tool_registry()
  all_ohdsi_names <- unlist(lapply(registry, function(t) t$table_names),
                             use.names = FALSE)
  found_ohdsi <- intersect(tolower(effective_results_tables), all_ohdsi_names)
  new_ohdsi <- setdiff(found_ohdsi, tables$table_name)
  if (length(new_ohdsi) > 0) {
    ohdsi_schema <- .effectiveResultsSchema(handle)
    ohdsi_rows <- data.frame(
      table_name      = new_ohdsi,
      schema_category = rep("Results", length(new_ohdsi)),
      concept_prefix  = rep(NA_character_, length(new_ohdsi)),
      has_person_id   = rep(FALSE, length(new_ohdsi)),
      present_in_db   = rep(TRUE, length(new_ohdsi)),
      qualified_name  = vapply(new_ohdsi, function(t) {
        .qualifyTable(handle, t, ohdsi_schema)
      }, character(1)),
      stringsAsFactors = FALSE
    )
    tables <- rbind(tables, ohdsi_rows)
  }
  # Mark already-present OHDSI tables
  existing_ohdsi <- intersect(found_ohdsi, tables$table_name)
  if (length(existing_ohdsi) > 0) {
    ohdsi_schema <- .effectiveResultsSchema(handle)
    mask <- tables$table_name %in% existing_ohdsi
    tables$present_in_db[mask] <- TRUE
    tables$schema_category[mask & tables$schema_category == "CDM"] <- "Results"
    tables$qualified_name[mask] <- vapply(tables$table_name[mask], function(t) {
      .qualifyTable(handle, t, ohdsi_schema)
    }, character(1))
  }
  handle$has_ohdsi_results <- length(found_ohdsi) > 0

  # Build join graph from spec FK metadata (if available)
  if (has_spec) {
    join_graph <- .buildJoinGraph(fld_meta, tables$table_name[tables$present_in_db])
  } else {
    join_graph <- data.frame(
      from_table = character(0), from_column = character(0),
      to_table = character(0), to_column = character(0),
      stringsAsFactors = FALSE
    )
  }

  blueprint <- new.env(parent = emptyenv())
  blueprint$tables       <- tables
  blueprint$columns      <- columns
  blueprint$join_graph   <- join_graph
  blueprint$cdm_info     <- cdm_info
  blueprint$spec_version <- if (has_spec) spec$version else NULL
  blueprint$spec_source  <- if (has_spec) spec$source else "introspection_only"
  blueprint$spec_upstream <- if (has_spec) list(
    source = spec$upstream_source %||% NULL,
    release = spec$upstream_release %||% NULL,
    commit = spec$upstream_commit %||% NULL
  ) else NULL
  blueprint$extension_contract <- extension_contract

  handle$blueprint <- blueprint
  blueprint
}

# --- Concept Role Classification ---

#' Classify a concept column's role
#'
#' @param table Table name
#' @param field Field name
#' @param concept_prefix Table's concept prefix from OHDSI metadata
#' @param fk_domain FK domain from OHDSI metadata
#' @param is_fk Whether the column is a foreign key
#' @param fk_table Target table of the FK
#' @return Character: domain_concept, type_concept, source_concept, attribute_concept, or non_concept
#' @keywords internal
.classifyConceptRole <- function(table, field, concept_prefix, fk_domain,
                                  is_fk = FALSE, fk_table = "") {
  if (!grepl("_concept_id$", field)) return("non_concept")

  # Source concept IDs
  if (grepl("_source_concept_id$", field)) return("source_concept")

  # Type concepts
  if (!is.na(fk_domain) && tolower(fk_domain) == "type concept") return("type_concept")

  # Domain concept: matches the table's conceptPrefix
  if (!is.na(concept_prefix) && nchar(concept_prefix) > 0) {
    normalized_prefix <- tolower(sub("_+$", "", trimws(concept_prefix)))
    expected_col <- paste0(normalized_prefix, "_concept_id")
    if (field == expected_col) return("domain_concept")
  }

  # If it's a FK to CONCEPT table, it's an attribute concept
  if (is_fk && toupper(fk_table) == "CONCEPT") return("attribute_concept")

  # Default: if it ends in _concept_id, treat as attribute
  "attribute_concept"
}

# --- Sensitive Column Detection ---

#' Detect whether a column contains PII / sensitive data
#'
#' Checks column names against a comprehensive blocklist of potentially
#' sensitive fields per OMOP CDM Privacy Guidance. This function is the
#' single point of control for column-level sensitivity detection. It runs
#' at blueprint-build time; results are stored in \code{is_blocked} and
#' enforced by \code{.compileSelect()} in extraction and all profiling
#' endpoints.
#'
#' @section Security Rationale:
#' Sensitive columns fall into three disclosure categories:
#' \describe{
#'   \item{Direct PII}{Free-text fields from source systems
#'     (\code{*_source_value}, \code{value_as_string}, \code{sig},
#'     \code{stop_reason}) and provider/location identifiers
#'     (\code{npi}, \code{dea}, \code{provider_name}, \code{address_*},
#'     \code{zip}). These may directly identify individuals.}
#'   \item{Quasi-identifiers}{Source concept IDs
#'     (\code{*_source_concept_id}) that reveal the originating EHR
#'     coding system, enabling cross-referencing with institutional data.
#'     Geographic fields (\code{latitude}, \code{longitude}, \code{city},
#'     \code{state}, \code{county}) that narrow location.}
#'   \item{Narrative text}{Clinical notes (\code{note_text},
#'     \code{note_title}, \code{snippet}, \code{lexical_variant}) that
#'     may contain embedded PHI per HIPAA/GDPR definitions.}
#' }
#'
#' @section Bypass:
#' Bypass requires the server admin to set
#' \code{options(dsomop.allow_sensitive_columns = TRUE)}. The analyst
#' cannot override this from the client side.
#'
#' Blocked columns include:
#' \itemize{
#'   \item All \code{*_source_value} columns (free text from source systems)
#'   \item \code{value_as_string} (free-text observation/measurement values)
#'   \item \code{sig} (drug prescription signature/instructions)
#'   \item \code{stop_reason} (free-text reason for drug stop)
#'   \item \code{lot_number} (manufacturer lot, potentially identifying)
#'   \item \code{unique_device_id} (device UDI, globally unique)
#'   \item NOTE / NOTE_NLP text fields (clinical narrative text)
#'   \item Direct identifiers such as patient/MRN, SSN, email and phone fields
#'   \item Exact dates of birth and generic comment/free-text fields
#'   \item \code{*_source_concept_id} (source-system identifiers)
#'   \item LOCATION fields: address, city, zip, county, latitude, longitude
#'   \item PROVIDER fields: provider_name, npi, dea
#'   \item CARE_SITE fields: care_site_name
#' }
#'
#' @param column_name Character; column name
#' @return Logical
#' @keywords internal
.detectSensitiveColumns <- function(column_name) {
  # Metadata from an unfamiliar driver must never become safe through a failed
  # or partial name conversion.
  if (!is.character(column_name) || length(column_name) != 1L ||
      is.na(column_name) || !nzchar(trimws(column_name))) {
    return(TRUE)
  }
  column_name <- tolower(trimws(column_name))
  sensitive_patterns <- c(
    # Direct identifiers commonly found in local OMOP extensions
    "(^|_)patient_(id|identifier|key)($|_)",
    "(^|_)(mrn|medical_record_number|medical_record_id)($|_)",
    "(^|_)(ssn|social_security_number)($|_)",
    "(^|_)(email|email_address|e_mail)($|_)",
    "(^|_)(phone|phone_number|telephone|mobile|mobile_phone)($|_)",
    # Exact birth information is a quasi-identifier. Age must be derived using
    # the package's minimum-width, reference-date-aware age grouping instead.
    "(^|_)(dob|date_of_birth|birth_date|birth_datetime)($|_)",
    "^(year|month|day)_of_birth$",
    # Generic narrative fields used by non-standard tables
    "(^|_)(comment|comments|free_text|freetext|narrative|remark|remarks)($|_)",
    # Source-system values (all tables): free text from EHR
    "_source_value$",
    # Source concept IDs (may reveal source-system coding)
    "_source_concept_id$",
    # Free-text string values
    "^value_as_string$",
    "^value_source_value$",
    # Drug-specific free text
    "^sig$",
    "^stop_reason$",
    "^lot_number$",
    # Device identifiers
    "^unique_device_id$",
    # NOTE / NOTE_NLP text fields (clinical narrative)
    "^note_text$",
    "^note_title$",
    "^note_source_value$",
    "^note_nlp_source_concept_id$",
    "^snippet$",
    "^lexical_variant$",
    "^note_nlp_concept_id$",
    "(^|_)term_modifiers$",
    # LOCATION: address/geo fields (OMOP Privacy Guidance)
    "^address_1$",
    "^address_2$",
    "^city$",
    "^state$",
    "^zip$",
    "^county$",
    "^latitude$",
    "^longitude$",
    # PROVIDER: identifying fields (NPI, DEA, name)
    "^provider_name$",
    "^npi$",
    "^dea$",
    # CARE_SITE: potentially identifying
    "^care_site_name$"
  )
  any(vapply(sensitive_patterns, function(p) grepl(p, column_name), logical(1)))
}

# --- Schema Resolution ---

#' Resolve which DB schema a table should be queried from
#'
#' @param handle CDM handle
#' @param table Table name
#' @param schema_category Character; CDM, Vocabulary, or Results
#' @return Character; schema name or NULL
#' @keywords internal
.resolveTableSchema <- function(handle, table, schema_category) {
  category <- tolower(schema_category)
  if (category %in% c("vocabulary", "vocab") && !is.null(handle$vocab_schema)) {
    return(handle$vocab_schema)
  }
  if (category %in% c("results", "result")) {
    rs <- .resolveResultsSchema(handle)
    if (!is.null(rs)) return(rs)
  }
  handle$cdm_schema
}

#' Resolve the effective schema for read-only OHDSI result-table access
#'
#' An explicitly configured results schema wins. Otherwise the restricted
#' controller-side detector may select an allowlisted schema; if it finds no
#' dedicated results namespace, standard co-located result tables are read from
#' the CDM schema. Persistent writes continue to require an explicit
#' \code{handle$results_schema} and do not use this fallback.
#'
#' @param handle CDM handle.
#' @return Character schema name, or \code{NULL} on a schemaless connection.
#' @keywords internal
.effectiveResultsSchema <- function(handle) {
  .resolveResultsSchema(handle) %||% handle$cdm_schema
}

#' Resolve the schema that actually contains the Achilles / results tables
#'
#' Makes the OHDSI "results" daimon first-class, mirroring how the vocabulary
#' schema is resolved. If the site pinned \code{handle$results_schema} (via
#' \code{omopInitDS} or the resource URL), that value is honored verbatim with
#' no probing. Otherwise only the already-authorized CDM schema is probed for
#' co-located results, plus exact namespaces listed by the data controller in
#' \code{dsomop.allowed_results_schemas}. It never scans conventional global
#' schema names using an analyst-triggered connection. The resolved value (which
#' may legitimately be
#' \code{NULL}: sqlite has no schemas, or Achilles is absent everywhere) is
#' cached on the handle so the probe runs at most once per session.
#'
#' @param handle CDM handle
#' @return Character schema name, or \code{NULL}
#' @keywords internal
.resolveResultsSchema <- function(handle) {
  # A resolved value of NULL is meaningful (not "unset"), so guard with an
  # explicit done-flag rather than `%||%`, which would re-probe on NULL.
  if (isTRUE(handle$results_schema_resolved_done)) {
    return(handle$results_schema_resolved)
  }
  resolved <- .detectResultsSchema(handle)
  handle$results_schema_resolved      <- resolved
  handle$results_schema_resolved_done <- TRUE
  resolved
}

#' Probe candidate schemas for the Achilles / results tables (uncached)
#'
#' @param handle CDM handle
#' @return Character schema name, or \code{NULL}
#' @keywords internal
.detectResultsSchema <- function(handle) {
  # Explicit pin wins, no probing (no behavior change when set).
  if (!is.null(handle$results_schema)) {
    return(handle$results_schema)
  }
  # Without an explicit pin, SQLite only probes its co-located main database.
  # Attached result databases remain available when the controller supplies a
  # results_schema in the resource URL.
  if (identical(handle$target_dialect, "sqlite")) {
    return(NULL)
  }
  marker_tables <- c("achilles_results", "achilles_analysis")
  cdm <- handle$cdm_schema
  allowed <- getOption("dsomop.allowed_results_schemas",
    getOption("default.dsomop.allowed_results_schemas", character(0)))
  if (!is.character(allowed) || anyNA(allowed)) {
    stop("dsomop.allowed_results_schemas must be a character vector.",
         call. = FALSE)
  }
  if (length(allowed) > 0L) {
    allowed <- vapply(allowed, function(namespace) {
      .validateSchemaNamespace(
        handle$dbms %||% handle$target_dialect,
        namespace,
        "allowed results schema"
      )
    }, character(1L))
  }
  candidates <- unique(Filter(function(s) !is.null(s) && nzchar(s),
                              c(cdm, allowed)))
  for (schema in candidates) {
    tbls <- tryCatch(tolower(.listTablesRaw(handle, schema)),
                     error = function(e) character(0))
    if (length(intersect(marker_tables, tbls)) > 0) return(schema)
  }
  NULL
}

# Quote one already validated identifier component only when unquoted SQL would
# alter or reject it. Standard lowercase OMOP identifiers remain readable.
.quoteIdentifierPart <- function(handle, value) {
  if (grepl("^[a-z_][a-z0-9_]*$", value)) return(value)
  dialect <- tolower(handle$target_dialect %||% "")
  if (dialect %in% c("snowflake", "oracle") &&
      grepl("^[A-Z_][A-Z0-9_]*$", value)) {
    return(value)
  }
  if (dialect %in% c("mysql", "spark")) {
    return(paste0("`", gsub("`", "``", value, fixed = TRUE), "`"))
  }
  if (identical(dialect, "sql server")) {
    return(paste0("[", gsub("]", "]]", value, fixed = TRUE), "]"))
  }
  paste0('"', gsub('"', '""', value, fixed = TRUE), '"')
}

.qualifyNamespace <- function(handle, schema) {
  schema <- .validateSchemaNamespace(
    handle$dbms %||% handle$target_dialect, schema, "schema"
  )
  parts <- strsplit(schema, ".", fixed = TRUE)[[1L]]
  if (identical(tolower(handle$target_dialect %||% ""), "bigquery")) {
    return(paste0("`", paste(parts, collapse = "."), "`"))
  }
  paste(vapply(parts, .quoteIdentifierPart, character(1L), handle = handle),
        collapse = ".")
}

.metadataIdentifierValue <- function(handle, value) {
  dialect <- tolower(handle$target_dialect %||% "")
  if (dialect %in% c("snowflake", "oracle") &&
      grepl("^[A-Za-z_][A-Za-z0-9_]*$", value) &&
      identical(value, tolower(value))) {
    return(toupper(value))
  }
  value
}

# MySQL and MariaDB table names can be case-sensitive while OMOP names are
# exposed canonically in lower case. Metadata discovery records the exact
# physical spelling per database; SQL qualification resolves it here.
.physicalTableName <- function(handle, table, schema = NULL) {
  table <- .validateIdentifier(table, "table")
  if (!identical(tolower(handle$target_dialect %||% ""), "mysql")) {
    return(table)
  }
  namespace <- schema %||% handle$cdm_schema %||% "<default>"
  mapping <- (handle$physical_table_names %||% list())[[namespace]]
  physical <- unname(mapping[tolower(table)])
  if (length(physical) == 1L && !is.na(physical)) {
    return(.validateIdentifier(physical, "physical table"))
  }
  table
}

#' Build a schema-qualified table reference
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param schema Character; schema name
#' @return Character; qualified table name for SQL
#' @keywords internal
.qualifyTable <- function(handle, table, schema = NULL) {
  table <- .validateIdentifier(table, "table")
  schema <- schema %||% handle$cdm_schema
  table <- .physicalTableName(handle, table, schema)
  if (is.null(schema) || schema == "") {
    return(.quoteIdentifierPart(handle, table))
  }
  schema <- .validateSchemaNamespace(
    handle$dbms %||% handle$target_dialect, schema, "schema"
  )
  dialect <- tolower(handle$target_dialect %||% "")
  parts <- strsplit(schema, ".", fixed = TRUE)[[1L]]
  if (identical(dialect, "bigquery")) {
    return(paste0("`", paste(c(parts, table), collapse = "."), "`"))
  }
  paste0(.qualifyNamespace(handle, schema), ".",
         .quoteIdentifierPart(handle, table))
}

# --- Blueprint Query Helpers ---

#' Get the domain concept column for a table
#'
#' Uses OHDSI conceptPrefix metadata instead of heuristic suffix stripping.
#'
#' @param blueprint The schema blueprint
#' @param table Character; table name
#' @return Character; domain concept column name, or NULL
#' @keywords internal
.getDomainConceptColumn <- function(blueprint, table) {
  table <- tolower(table)
  tbl_row <- blueprint$tables[blueprint$tables$table_name == table, , drop = FALSE]
  if (nrow(tbl_row) == 0) return(NULL)

  cols <- blueprint$columns[[table]]

  prefix <- tbl_row$concept_prefix[1]
  if (!is.na(prefix) && nchar(prefix) > 0) {
    expected_col <- paste0(tolower(prefix), "_concept_id")
    # Verify it exists
    if (!is.null(cols) && expected_col %in% cols$column_name) {
      return(expected_col)
    }
  }

  # Fallback: look for the domain_concept role in columns
  if (!is.null(cols)) {
    domain_cols <- cols$column_name[cols$concept_role == "domain_concept"]
    if (length(domain_cols) > 0) return(domain_cols[1])
  }

  # Tables OHDSI gives no conceptPrefix for (person, death) have no single
  # "domain" concept, so the prefix + role paths above both yield nothing and
  # callers that auto-detect (prevalence/value-counts) would error. Provide a
  # sensible DEFAULT concept column so those tables are explorable out of the
  # box, while any other concept column on the table stays reachable via an
  # explicit `concept_col`/`column` argument:
  #   - person -> gender_concept_id (race/ethnicity selectable explicitly);
  #   - death  -> cause_concept_id (CDM 5.4 has NO death_concept_id; the old
  #     auto-detect looked for a non-existent column and failed).
  default_concept <- switch(table,
    person = "gender_concept_id",
    death  = "cause_concept_id",
    NULL
  )
  if (!is.null(default_concept) && !is.null(cols) &&
      default_concept %in% cols$column_name) {
    return(default_concept)
  }

  NULL
}

#' Get the primary date column for a table
#'
#' @param blueprint The schema blueprint
#' @param table Character; table name
#' @return Character; date column name, or NULL
#' @keywords internal
.getDateColumn <- function(blueprint, table) {
  table <- tolower(table)
  cols <- blueprint$columns[[table]]
  if (is.null(cols)) return(NULL)

  date_cols <- cols$column_name[cols$is_date]
  if (length(date_cols) == 0) return(NULL)

  # Prefer _start_date columns
  start_dates <- grep("_start_date$", date_cols, value = TRUE)
  if (length(start_dates) > 0) return(start_dates[1])

  # Then prefer _date columns (not _end_date)
  plain_dates <- grep("_date$", date_cols, value = TRUE)
  plain_dates <- setdiff(plain_dates, grep("_end_date$", plain_dates, value = TRUE))
  if (length(plain_dates) > 0) return(plain_dates[1])

  date_cols[1]
}

#' Get start/end date column pair for interval tables
#'
#' Returns a list with \code{start} and \code{end} date column names for
#' tables that have interval data (e.g. observation_period, visit_occurrence,
#' condition_occurrence, drug_exposure, drug_era, condition_era).
#' Returns NULL for single-date tables (measurement, procedure_occurrence).
#'
#' @param blueprint The schema blueprint
#' @param table Character; table name
#' @return Named list with \code{start} and \code{end}, or NULL
#' @keywords internal
.getDatePair <- function(blueprint, table) {
  table <- tolower(table)
  cols <- blueprint$columns[[table]]
  if (is.null(cols)) return(NULL)

  date_cols <- cols$column_name[cols$is_date]
  if (length(date_cols) == 0) return(NULL)

  # Find a _start_date column
  start_cols <- grep("_start_date$", date_cols, value = TRUE)
  if (length(start_cols) == 0) return(NULL)

  start_col <- start_cols[1]

  # Derive the _end_date column by substitution

  end_col <- sub("_start_date$", "_end_date", start_col)

  # Verify the end column exists
  if (!end_col %in% cols$column_name) return(NULL)

  list(start = start_col, end = end_col)
}

#' Find a join path between tables using BFS
#'
#' @param blueprint The schema blueprint
#' @param from_table Character; starting table
#' @param to_col Character; target column to reach
#' @return List with path and joins, or NULL
#' @keywords internal
.findJoinPath <- function(blueprint, from_table, to_col = "person_id") {
  edges <- blueprint$join_graph
  columns <- blueprint$columns

  from_table <- tolower(from_table)

  # Check if start table has target column
  if (!is.null(columns[[from_table]]) &&
      to_col %in% columns[[from_table]]$column_name) {
    return(list(path = from_table, joins = list()))
  }

  # BFS
  visited <- character(0)
  queue <- list(list(table = from_table, path = from_table, joins = list()))

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]

    if (current$table %in% visited) next
    visited <- c(visited, current$table)

    out_edges <- edges[edges$from_table == current$table, , drop = FALSE]

    for (i in seq_len(nrow(out_edges))) {
      next_table <- out_edges$to_table[i]
      if (next_table %in% visited) next

      new_join <- list(
        from_table  = current$table,
        from_column = out_edges$from_column[i],
        to_table    = next_table,
        to_column   = out_edges$to_column[i]
      )
      new_joins <- c(current$joins, list(new_join))
      new_path <- c(current$path, next_table)

      if (!is.null(columns[[next_table]]) &&
          to_col %in% columns[[next_table]]$column_name) {
        return(list(path = new_path, joins = new_joins))
      }

      queue <- c(queue, list(list(
        table = next_table, path = new_path, joins = new_joins
      )))
    }
  }

  NULL
}

#' Get capabilities signature for client validation
#'
#' @param handle CDM handle
#' @return Named list with schema summary
#' @keywords internal
.getCapabilities <- function(handle) {
  bp <- .buildBlueprint(handle)
  present <- bp$tables[bp$tables$present_in_db, ]

  tbl_sig <- paste(sort(present$table_name), collapse = ",")
  col_counts <- vapply(bp$columns, nrow, integer(1))
  col_sig <- paste(names(col_counts), col_counts, sep = ":", collapse = ",")

  sig_string <- paste(tbl_sig, col_sig, sep = "|")
  sig_hash <- substr(
    paste(as.character(charToRaw(sig_string)), collapse = ""),
    1, 32
  )

  achilles_table_names <- c("achilles_analysis", "achilles_results",
                              "achilles_results_dist", "achilles_heel_results")

  supported_versions <- if (tryCatch(requireNamespace("CommonDataModel", quietly = TRUE),
                                      warning = function(w) FALSE)) {
    tryCatch(CommonDataModel::listSupportedVersions(), error = function(e) character(0))
  } else {
    character(0)
  }

  # A population total is still differencing material when combined with
  # filtered releases. Apply the same admission gate and count banding as the
  # aggregate APIs; return NULL on a small population or connection failure.
  total_persons <- tryCatch({
    if ("person" %in% present$table_name) {
      person_qualified <- .qualifyTable(handle, "person")
      sql <- paste0("SELECT COUNT(*) AS n FROM ", person_qualified)
      res <- .executeQuery(handle, sql)
      n <- as.numeric(res$n[1])
      .assertMinPersons(n_persons = n)
      .bandCount(n, .omopDisclosureSettings()$nfilter_band)
    } else {
      NULL
    }
  }, error = function(e) NULL)

  # Disclosure settings (from DataSHIELD server options)
  disclosure <- tryCatch(.omopDisclosureSettings(), error = function(e) NULL)

  # Filter tables to only those recognized by CDM spec
  spec_tables <- if (!is.null(bp$spec_version)) {
    spec <- .loadCdmSpec(bp$spec_version)
    if (!is.null(spec)) tolower(spec$table_level$cdmTableName) else NULL
  } else NULL

  list(
    hash = sig_hash,
    dbms = handle$dbms,
    n_tables = nrow(present),
    total_persons = total_persons,
    tables = present$table_name,
    cdm_tables = if (!is.null(spec_tables)) {
      intersect(present$table_name, spec_tables)
    } else present$table_name,
    schema_categories = stats::setNames(present$schema_category, present$table_name),
    cdm_info = bp$cdm_info,
    spec_version = bp$spec_version,
    spec_source = bp$spec_source,
    spec_upstream = bp$spec_upstream,
    supported_versions = supported_versions,
    achilles_available = isTRUE(handle$has_achilles),
    achilles_tables = intersect(achilles_table_names,
                                 bp$tables$table_name[bp$tables$present_in_db]),
    database_support = .databaseSupportProfile(handle$dbms),
    disclosure = disclosure,
    pseudonymization = .personKeyPublicContract(handle)
  )
}

# --- Internal Helpers ---

#' Execute a metadata query with the normal reconnect contract
#' @param handle CDM handle.
#' @param sql SQL string.
#' @return Data frame returned by DBI.
#' @keywords internal
.metadataQuery <- function(handle, sql) {
  .withDbReconnect(handle, function(conn) DBI::dbGetQuery(conn, sql))
}

#' Read a DBI result column without assuming driver-specific name casing
#' @param result Data frame returned by DBI.
#' @param candidates Accepted column names.
#' @param default Value returned when no candidate exists.
#' @return A result column or default.
#' @keywords internal
.metadataResultColumn <- function(result, candidates, default = character(0)) {
  if (!is.data.frame(result) || ncol(result) == 0L) return(default)
  idx <- match(tolower(candidates), tolower(names(result)))
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0L) default else result[[idx[[1]]]]
}

#' List tables in a schema (raw DB query)
#'
#' @param handle CDM handle
#' @param schema Character; schema name
#' @return Character vector of table names (lowercase)
#' @keywords internal
.listTablesRaw <- function(handle, schema = NULL) {
  if (handle$target_dialect == "sqlite") {
    schema_to_use <- .validateIdentifier(
      schema %||% handle$cdm_schema %||% "main", "schema")
    sql <- paste0(
      "SELECT name AS table_name FROM ", schema_to_use,
      ".sqlite_master WHERE type IN ('table', 'view') ORDER BY name")
    result <- .metadataQuery(handle, sql)
    tables <- .metadataResultColumn(result, c("table_name", "name"))
  } else if (handle$target_dialect == "postgresql") {
    schema_to_use <- .validateSchemaNamespace(
      handle$dbms, schema %||% handle$cdm_schema %||% "public", "schema"
    )
    sql <- .renderSql(handle,
      "SELECT c.relname AS table_name
       FROM pg_catalog.pg_class c
       INNER JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
       WHERE n.nspname = '@schema'
         AND c.relkind IN ('r', 'p', 'v', 'm', 'f')
         AND pg_catalog.has_schema_privilege(n.oid, 'USAGE')
         AND pg_catalog.has_table_privilege(c.oid, 'SELECT')
       ORDER BY c.relname",
      schema = schema_to_use)
    result <- .metadataQuery(handle, sql)
    tables <- .metadataResultColumn(result, "table_name")
  } else if (handle$target_dialect == "oracle") {
    schema_to_use <- .metadataIdentifierValue(handle,
      .validateSchemaNamespace(handle$dbms,
        schema %||% handle$cdm_schema %||% "", "schema"))
    sql <- .renderSql(handle,
      "SELECT OBJECT_NAME AS TABLE_NAME FROM ALL_OBJECTS
       WHERE OWNER = '@schema'
         AND OBJECT_TYPE IN ('TABLE', 'VIEW', 'MATERIALIZED VIEW')
       ORDER BY OBJECT_NAME",
      schema = schema_to_use)
    result <- .metadataQuery(handle, sql)
    tables <- .metadataResultColumn(result, "table_name")
  } else if (handle$target_dialect == "bigquery") {
    schema_to_use <- .validateSchemaNamespace(handle$dbms,
      schema %||% handle$cdm_schema %||% "", "schema")
    sql <- .renderSql(handle,
      "SELECT table_name FROM `@schema.INFORMATION_SCHEMA.TABLES`
       ORDER BY table_name",
      schema = schema_to_use)
    result <- .metadataQuery(handle, sql)
    tables <- .metadataResultColumn(result, "table_name")
  } else if (handle$target_dialect == "spark") {
    schema_to_use <- .validateSchemaNamespace(handle$dbms,
      schema %||% handle$cdm_schema %||% "default", "schema")
    result <- .metadataQuery(
      handle, paste0("SHOW TABLES IN ", .qualifyNamespace(handle, schema_to_use)))
    tables <- .metadataResultColumn(
      result, c("tableName", "table_name", "tablename"))
  } else {
    namespace <- .validateSchemaNamespace(
      handle$dbms %||% handle$target_dialect,
      schema %||% handle$cdm_schema %||% "public", "schema"
    )
    parts <- strsplit(namespace, ".", fixed = TRUE)[[1L]]
    schema_to_use <- parts[[length(parts)]]
    catalog_to_use <- if (length(parts) == 2L) parts[[1L]] else NULL
    if (handle$target_dialect == "snowflake") {
      schema_to_use <- .metadataIdentifierValue(handle, schema_to_use)
      if (!is.null(catalog_to_use)) {
        catalog_to_use <- .metadataIdentifierValue(handle, catalog_to_use)
      }
    }
    catalog_predicate <- if (is.null(catalog_to_use)) "" else paste0(
      " AND TABLE_CATALOG = '", catalog_to_use, "'"
    )
    information_schema <- if (is.null(catalog_to_use)) {
      "INFORMATION_SCHEMA"
    } else {
      paste0(.quoteIdentifierPart(handle, catalog_to_use),
             ".INFORMATION_SCHEMA")
    }
    sql <- .renderSql(
      handle,
      paste0(
        "SELECT TABLE_NAME FROM ", information_schema, ".TABLES ",
        "WHERE TABLE_SCHEMA = '@schema'@catalog_predicate ",
        "ORDER BY TABLE_NAME"
      ),
      schema = schema_to_use, catalog_predicate = catalog_predicate
    )
    result <- .metadataQuery(handle, sql)
    tables <- .metadataResultColumn(result, "table_name")
  }

  physical_tables <- as.character(tables)
  canonical_tables <- tolower(physical_tables)
  if (identical(tolower(handle$target_dialect %||% ""), "mysql")) {
    duplicates <- unique(canonical_tables[duplicated(canonical_tables)])
    if (length(duplicates) > 0L) {
      stop("Ambiguous physical table names collapse to the same canonical ",
           "OMOP name: ", paste(duplicates, collapse = ", "), ".",
           call. = FALSE)
    }
    namespace <- schema %||% handle$cdm_schema %||% "<default>"
    mappings <- handle$physical_table_names %||% list()
    mappings[[namespace]] <- stats::setNames(physical_tables,
                                             canonical_tables)
    handle$physical_table_names <- mappings
  }
  canonical_tables
}

#' List columns in a table (raw DB query)
#'
#' @param handle CDM handle
#' @param table Character; table name
#' @param schema Character; schema name
#' @return Data frame with column_name, data_type, is_nullable
#' @keywords internal
.listColumnsRaw <- function(handle, table, schema = NULL) {
  table <- .validateIdentifier(table, "table")
  physical_table <- .physicalTableName(handle, table, schema)
  empty <- data.frame(column_name = character(0), data_type = character(0),
                      is_nullable = character(0), stringsAsFactors = FALSE)

  if (handle$target_dialect == "sqlite") {
    schema_to_use <- .validateIdentifier(
      schema %||% handle$cdm_schema %||% "main", "schema")
    result <- .metadataQuery(handle, paste0(
      "PRAGMA ", schema_to_use, ".table_info('", table, "')"))
    if (nrow(result) > 0) {
      data.frame(
        column_name = tolower(.metadataResultColumn(result, "name")),
        data_type   = tolower(.metadataResultColumn(result, "type")),
        is_nullable = ifelse(
          .metadataResultColumn(result, "notnull") == 0, "YES", "NO"),
        stringsAsFactors = FALSE
      )
    } else {
      empty
    }
  } else if (handle$target_dialect == "postgresql") {
    schema_to_use <- .validateSchemaNamespace(
      handle$dbms, schema %||% handle$cdm_schema %||% "public", "schema"
    )
    sql <- .renderSql(handle,
      "SELECT a.attname AS column_name,
              CASE WHEN t.typtype = 'd'
                   THEN pg_catalog.format_type(t.typbasetype, t.typtypmod)
                   ELSE pg_catalog.format_type(a.atttypid, a.atttypmod)
              END AS data_type,
              CASE WHEN a.attnotnull OR (t.typtype = 'd' AND t.typnotnull)
                   THEN 'NO' ELSE 'YES' END AS is_nullable
       FROM pg_catalog.pg_attribute a
       INNER JOIN pg_catalog.pg_class c ON c.oid = a.attrelid
       INNER JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
       INNER JOIN pg_catalog.pg_type t ON t.oid = a.atttypid
       WHERE n.nspname = '@schema'
         AND c.relname = '@table'
         AND c.relkind IN ('r', 'p', 'v', 'm', 'f')
         AND a.attnum > 0
         AND NOT a.attisdropped
         AND pg_catalog.has_schema_privilege(n.oid, 'USAGE')
         AND pg_catalog.has_table_privilege(c.oid, 'SELECT')
       ORDER BY a.attnum",
      schema = schema_to_use, table = table)
    result <- .metadataQuery(handle, sql)
    if (nrow(result) > 0L) {
      data.frame(
        column_name = tolower(.metadataResultColumn(result, "column_name")),
        data_type = tolower(.metadataResultColumn(result, "data_type")),
        is_nullable = .metadataResultColumn(result, "is_nullable"),
        stringsAsFactors = FALSE
      )
    } else {
      empty
    }
  } else if (handle$target_dialect == "oracle") {
    schema_to_use <- .metadataIdentifierValue(handle,
      .validateSchemaNamespace(handle$dbms,
        schema %||% handle$cdm_schema %||% "", "schema"))
    sql <- .renderSql(handle,
      "SELECT COLUMN_NAME, DATA_TYPE, NULLABLE AS IS_NULLABLE
       FROM ALL_TAB_COLUMNS
       WHERE OWNER = '@schema' AND TABLE_NAME = '@table'
       ORDER BY COLUMN_ID",
      schema = schema_to_use,
      table = .metadataIdentifierValue(handle, table))
    result <- .metadataQuery(handle, sql)
    if (nrow(result) > 0) {
      nullable <- .metadataResultColumn(result, "is_nullable")
      data.frame(
        column_name = tolower(.metadataResultColumn(result, "column_name")),
        data_type   = tolower(.metadataResultColumn(result, "data_type")),
        is_nullable = ifelse(nullable == "Y", "YES", "NO"),
        stringsAsFactors = FALSE
      )
    } else {
      empty
    }
  } else if (handle$target_dialect == "bigquery") {
    schema_to_use <- .validateSchemaNamespace(handle$dbms,
      schema %||% handle$cdm_schema %||% "", "schema")
    sql <- .renderSql(handle,
      "SELECT column_name, data_type, is_nullable
       FROM `@schema.INFORMATION_SCHEMA.COLUMNS`
       WHERE table_name = '@table'
       ORDER BY ordinal_position",
      schema = schema_to_use, table = table)
    result <- .metadataQuery(handle, sql)
    if (nrow(result) > 0) {
      data.frame(
        column_name = tolower(.metadataResultColumn(result, "column_name")),
        data_type   = tolower(.metadataResultColumn(result, "data_type")),
        is_nullable = .metadataResultColumn(result, "is_nullable"),
        stringsAsFactors = FALSE
      )
    } else {
      empty
    }
  } else if (handle$target_dialect == "spark") {
    schema_to_use <- .validateSchemaNamespace(handle$dbms,
      schema %||% handle$cdm_schema %||% "default", "schema")
    result <- .metadataQuery(
      handle, paste0("DESCRIBE TABLE ",
                     .qualifyTable(handle, table, schema_to_use)))
    if (nrow(result) > 0L) {
      column_name <- as.character(.metadataResultColumn(
        result, c("col_name", "column_name")))
      data_type <- as.character(.metadataResultColumn(result, "data_type"))
      keep <- nzchar(trimws(column_name)) & !startsWith(trimws(column_name), "#")
      data.frame(
        column_name = tolower(column_name[keep]),
        data_type = tolower(data_type[keep]),
        is_nullable = rep(NA_character_, sum(keep)),
        stringsAsFactors = FALSE
      )
    } else {
      empty
    }
  } else {
    namespace <- .validateSchemaNamespace(
      handle$dbms %||% handle$target_dialect,
      schema %||% handle$cdm_schema %||% "public", "schema"
    )
    parts <- strsplit(namespace, ".", fixed = TRUE)[[1L]]
    schema_to_use <- parts[[length(parts)]]
    catalog_to_use <- if (length(parts) == 2L) parts[[1L]] else NULL
    table_to_use <- physical_table
    if (handle$target_dialect == "snowflake") {
      schema_to_use <- .metadataIdentifierValue(handle, schema_to_use)
      if (!is.null(catalog_to_use)) {
        catalog_to_use <- .metadataIdentifierValue(handle, catalog_to_use)
      }
      table_to_use <- .metadataIdentifierValue(handle, table_to_use)
    }
    catalog_predicate <- if (is.null(catalog_to_use)) "" else paste0(
      " AND TABLE_CATALOG = '", catalog_to_use, "'"
    )
    information_schema <- if (is.null(catalog_to_use)) {
      "INFORMATION_SCHEMA"
    } else {
      paste0(.quoteIdentifierPart(handle, catalog_to_use),
             ".INFORMATION_SCHEMA")
    }
    sql <- .renderSql(
      handle,
      paste0(
        "SELECT COLUMN_NAME, DATA_TYPE, IS_NULLABLE ",
        "FROM ", information_schema, ".COLUMNS ",
        "WHERE TABLE_SCHEMA = '@schema' AND TABLE_NAME = '@table'",
        "@catalog_predicate ORDER BY ORDINAL_POSITION"
      ),
      schema = schema_to_use, table = table_to_use,
      catalog_predicate = catalog_predicate
    )
    result <- .metadataQuery(handle, sql)
    if (nrow(result) > 0) {
      data.frame(
        column_name = tolower(.metadataResultColumn(result, "column_name")),
        data_type   = tolower(.metadataResultColumn(result, "data_type")),
        is_nullable = .metadataResultColumn(result, "is_nullable"),
        stringsAsFactors = FALSE
      )
    } else {
      empty
    }
  }
}

#' Build join graph from OHDSI FK metadata
#'
#' @param field_level Field-level metadata data.frame
#' @param present_tables Character vector of tables present in DB
#' @return Data frame with from_table, from_column, to_table, to_column
#' @keywords internal
.buildJoinGraph <- function(field_level, present_tables) {
  edges <- data.frame(
    from_table  = character(0),
    from_column = character(0),
    to_table    = character(0),
    to_column   = character(0),
    stringsAsFactors = FALSE
  )

  # Known FK targets: column -> (table, pk)
  known_targets <- list(
    person_id             = list(table = "person",              pk = "person_id"),
    visit_occurrence_id   = list(table = "visit_occurrence",    pk = "visit_occurrence_id"),
    visit_detail_id       = list(table = "visit_detail",        pk = "visit_detail_id"),
    provider_id           = list(table = "provider",            pk = "provider_id"),
    care_site_id          = list(table = "care_site",           pk = "care_site_id"),
    location_id           = list(table = "location",            pk = "location_id"),
    observation_period_id = list(table = "observation_period",  pk = "observation_period_id"),
    episode_id            = list(table = "episode",             pk = "episode_id")
  )

  fk_rows <- field_level[
    !is.na(field_level$isForeignKey) & toupper(field_level$isForeignKey) == "YES",
    , drop = FALSE
  ]

  for (i in seq_len(nrow(fk_rows))) {
    from_tbl <- tolower(fk_rows$cdmTableName[i])
    from_col <- tolower(fk_rows$cdmFieldName[i])
    fk_table_raw <- fk_rows$fkTableName[i]

    if (is.na(fk_table_raw) || nchar(fk_table_raw) == 0) next
    if (!from_tbl %in% present_tables) next

    to_tbl <- tolower(fk_table_raw)

    # Skip concept FKs for the join graph (too many)
    if (to_tbl == "concept") next
    if (!to_tbl %in% present_tables) next

    # Use known targets for PK resolution
    if (from_col %in% names(known_targets)) {
      target <- known_targets[[from_col]]
      if (target$table == to_tbl) {
        edges <- rbind(edges, data.frame(
          from_table = from_tbl, from_column = from_col,
          to_table = to_tbl, to_column = target$pk,
          stringsAsFactors = FALSE
        ))
        next
      }
    }

    # Generic: assume PK is table_name + "_id"
    to_pk <- paste0(to_tbl, "_id")
    edges <- rbind(edges, data.frame(
      from_table = from_tbl, from_column = from_col,
      to_table = to_tbl, to_column = to_pk,
      stringsAsFactors = FALSE
    ))
  }

  if (nrow(edges) > 0) edges <- unique(edges)
  edges
}

#' Detect CDM version from table/column structure using weighted evidence scoring
#'
#' Uses multiple structural signals to infer whether the database matches
#' CDM v5.3 or v5.4. Table-level checks (episode, episode_event) are free
#' since the table list is already discovered. Column-level checks query
#' only tables that exist.
#'
#' @param handle CDM handle
#' @param db_tables Character vector of table names present in the database
#' @return List with \code{version}, \code{evidence_54}, \code{evidence_53},
#'   and \code{checks} (named list of individual check results), or NULL if
#'   inconclusive (zero evidence or tie).
#' @keywords internal
.detectCDMVersionFromStructure <- function(handle, db_tables) {
  evidence_54 <- 0
  evidence_53 <- 0
  checks <- list()


  # Check 1: episode table exists (+3 for 5.4)
  if ("episode" %in% db_tables) {
    evidence_54 <- evidence_54 + 3
    checks$episode_table <- "5.4"
  }

  # Check 2: episode_event table exists (+3 for 5.4)
  if ("episode_event" %in% db_tables) {
    evidence_54 <- evidence_54 + 3
    checks$episode_event_table <- "5.4"
  }

  # Note: this function runs BEFORE the blueprint is built, so

  # handle$blueprint is NULL. Use handle$cdm_schema directly for column queries.
  schema <- handle$cdm_schema

  # Check 3: procedure_occurrence columns (+2 for winner)
  if ("procedure_occurrence" %in% db_tables) {
    proc_cols <- .listColumnsRaw(handle, "procedure_occurrence", schema)
    if (nrow(proc_cols) > 0) {
      if ("procedure_end_date" %in% proc_cols$column_name) {
        evidence_54 <- evidence_54 + 2
        checks$procedure_end_date <- "5.4"
      } else {
        evidence_53 <- evidence_53 + 2
        checks$procedure_end_date <- "5.3"
      }
    }
  }

  # Check 4: location columns (+1 for winner)
  if ("location" %in% db_tables) {
    loc_cols <- .listColumnsRaw(handle, "location", schema)
    if (nrow(loc_cols) > 0) {
      if ("latitude" %in% loc_cols$column_name) {
        evidence_54 <- evidence_54 + 1
        checks$location_latitude <- "5.4"
      } else {
        evidence_53 <- evidence_53 + 1
        checks$location_latitude <- "5.3"
      }
    }
  }

  # Check 5: visit_detail columns (+2 for winner)
  if ("visit_detail" %in% db_tables) {
    vd_cols <- .listColumnsRaw(handle, "visit_detail", schema)
    if (nrow(vd_cols) > 0) {
      if ("parent_visit_detail_id" %in% vd_cols$column_name) {
        evidence_54 <- evidence_54 + 2
        checks$visit_detail_parent <- "5.4"
      } else if ("visit_detail_parent_id" %in% vd_cols$column_name) {
        evidence_53 <- evidence_53 + 2
        checks$visit_detail_parent <- "5.3"
      }
    }
  }

  # Decision: higher score wins; tie or zero evidence -> NULL
  if (evidence_54 == 0 && evidence_53 == 0) return(NULL)
  if (evidence_54 == evidence_53) return(NULL)

  version <- if (evidence_54 > evidence_53) "5.4" else "5.3"

  list(
    version     = version,
    evidence_54 = evidence_54,
    evidence_53 = evidence_53,
    checks      = checks
  )
}

#' Detect CDM info from cdm_source table
#'
#' @param handle CDM handle
#' @param tables Character vector of available tables
#' @return Named list or NULL
#' @keywords internal
.detectCDMInfo <- function(handle, tables) {
  if (!"cdm_source" %in% tables) return(NULL)

  tryCatch({
    qualified <- .qualifyTable(handle, "cdm_source")
    sql <- .renderSql(handle,
      "SELECT TOP 1 * FROM @qualified",
      qualified = qualified)
    result <- .coerce_integer64(
      .withDbReconnect(handle, function(conn) DBI::dbGetQuery(conn, sql)))
    if (nrow(result) == 0) return(NULL)

    names(result) <- tolower(names(result))
    list(
      cdm_version         = result$cdm_version[1] %||% NULL,
      vocabulary_version  = result$vocabulary_version[1] %||% NULL,
      source_name         = result$cdm_source_name[1] %||% NULL,
      source_abbreviation = result$cdm_source_abbreviation[1] %||% NULL
    )
  }, error = function(e) NULL)
}

# --- Temp Table Helpers ---

#' Build a temporary-object materialization statement
#'
#' @param handle A CDM handle.
#' @param name Validated temporary object name.
#' @param select_sql SELECT statement to materialize.
#' @return SQL string for the backend's session-scoped object.
#' @keywords internal
.tempCreateSql <- function(handle, name, select_sql) {
  support <- .databaseSupportProfile(handle$dbms)
  switch(support$temporary_materialization,
    session_view = paste0("CREATE TEMPORARY VIEW ", name, " AS ", select_sql),
    session_table = if (support$target_dialect == "mysql") {
      paste0("CREATE TEMPORARY TABLE ", name, " AS ", select_sql)
    } else {
      paste0("CREATE TEMP TABLE ", name, " AS ", select_sql)
    },
    stop("Session-scoped temporary materialization is not implemented safely ",
         "for DBMS '", support$dbms, "'. This backend can still use read-only ",
         "operations that do not require cross-statement temporary objects.",
         call. = FALSE)
  )
}

#' Build the matching DROP statement for an owned temporary object
#'
#' @param handle A CDM handle.
#' @param name Validated temporary object name.
#' @return SQL string.
#' @keywords internal
.tempDropSql <- function(handle, name) {
  support <- .databaseSupportProfile(handle$dbms)
  if (identical(support$temporary_materialization, "session_view")) {
    paste0("DROP VIEW IF EXISTS ", name)
  } else if (support$target_dialect == "mysql") {
    paste0("DROP TEMPORARY TABLE IF EXISTS ", name)
  } else if (identical(support$dbms, "postgresql")) {
    paste0("DROP TABLE IF EXISTS pg_temp.", name)
  } else if (support$dbms %in% c("sqlite", "duckdb")) {
    paste0("DROP TABLE IF EXISTS temp.", name)
  } else if (identical(support$temporary_materialization, "session_table")) {
    paste0("DROP TABLE IF EXISTS ", name)
  } else {
    stop("Session-scoped temporary materialization is not implemented safely ",
         "for DBMS '", support$dbms, "'; refusing to generate a DROP that ",
         "could target a persistent object.", call. = FALSE)
  }
}

#' Create a temporary table in the database
#'
#' @param handle CDM handle
#' @param name Character; temp table name
#' @param select_sql Character; SELECT statement for contents
#' @return The temp table name
#' @keywords internal
.createTempTable <- function(handle, name, select_sql) {
  name <- .validateIdentifier(name, "temporary table")
  max_tables <- .omopDisclosureSettings()$max_temp_tables_per_handle
  sql <- .tempCreateSql(handle, name, select_sql)
  conn <- .conn(handle)
  # TEMP objects are owned by one physical DB session. If the resource
  # reconnected, discard the stale registry before creating anything; never let
  # a stale name authorize a later DROP against a persistent homonym.
  if (!is.null(handle$temp_connection) &&
      !identical(handle$temp_connection, conn)) {
    handle$temp_tables <- character(0)
    handle$temp_connection <- NULL
  }
  if (name %in% (handle$temp_tables %||% character(0))) {
    stop("Temporary table name is already owned by this session.",
         call. = FALSE)
  }
  if (length(unique(handle$temp_tables %||% character(0))) >= max_tables) {
    stop("This OMOP handle has reached its server-owned temporary-table cap (",
         as.integer(max_tables), "); clean up temporary cohorts/outputs before ",
         "creating more.", call. = FALSE)
  }
  .withDbReconnect(handle, function(active_conn) DBI::dbExecute(active_conn, sql))
  active_conn <- .conn(handle)
  # The CREATE itself may have triggered .withDbReconnect(). Any names retained
  # above belonged to `conn`, not to the replacement session. Clear them before
  # registering the one object whose CREATE succeeded on `active_conn`; otherwise
  # a later cleanup could authorize DROP against a persistent homonym there.
  if (!identical(active_conn, conn)) {
    handle$temp_tables <- character(0)
    handle$temp_connection <- NULL
  }
  handle$temp_connection <- active_conn
  handle$temp_tables <- unique(c(handle$temp_tables, name))
  name
}

#' Reserve a collision-free name for an internal temporary object
#'
#' Internal plan/analysis work tables must never DROP or replace an object that
#' was already owned when the operation began. The preferred base is retained
#' when free; an owner-registry collision receives a cryptographically random
#' suffix and the caller must use the returned name.
#'
#' @param handle CDM handle.
#' @param base Preferred internal temporary-table name.
#' @return A valid name absent from the handle's current ownership registry.
#' @keywords internal
.reserveTempTableName <- function(handle, base) {
  base <- .validateIdentifier(base, "internal temporary table")
  owned <- unique(handle$temp_tables %||% character(0))
  if (!base %in% owned) return(base)

  # Keep suffixed names below PostgreSQL's 63-byte identifier limit. Internal
  # names are ASCII, so character and byte lengths coincide here.
  stem <- substr(base, 1L, 48L)
  for (attempt in seq_len(128L)) {
    suffix <- paste0(format(openssl::rand_bytes(6L)), collapse = "")
    candidate <- paste0(stem, "_", suffix)
    if (!candidate %in% owned) return(candidate)
  }
  stop("Could not reserve a collision-free internal temporary-table name.",
       call. = FALSE)
}

#' Drop a temporary table
#'
#' @param handle CDM handle
#' @param name Character; temp table name
#' @return NULL, called for side effect.
#' @keywords internal
.dropTempTable <- function(handle, name) {
  name <- .validateIdentifier(name, "temporary table")
  owned <- handle$temp_tables %||% character(0)
  if (!name %in% owned) {
    return(invisible(NULL))
  }
  conn <- .conn(handle)
  if (is.null(handle$temp_connection) ||
      !identical(handle$temp_connection, conn)) {
    # The owning DB session has gone away. Its TEMP tables are already gone;
    # executing DROP now could delete a persistent table with the same name.
    handle$temp_tables <- character(0)
    handle$temp_connection <- NULL
    return(invisible(NULL))
  }
  drop_sql <- .tempDropSql(handle, name)
  # Keep ownership until the database confirms the DROP.  Forgetting a table
  # after a failed DROP would make cleanup appear successful, prevent a retry,
  # and under-count the per-handle temporary-object cap.
  DBI::dbExecute(conn, drop_sql)
  handle$temp_tables <- setdiff(handle$temp_tables, name)
  if (length(handle$temp_tables) == 0L) handle$temp_connection <- NULL
  invisible(NULL)
}

# Drop every temporary object created after an operation-owned snapshot.
# Existing objects may be caller-provided cohort scopes and must survive.  Work
# backwards so dependent views/tables are released in reverse creation order.
.dropTempTablesCreatedSince <- function(handle, owned_before) {
  owned_before <- unique(as.character(owned_before %||% character(0)))
  current <- unique(as.character(handle$temp_tables %||% character(0)))
  to_drop <- rev(setdiff(current, owned_before))
  if (length(to_drop) == 0L) return(invisible(NULL))

  failures <- character(0)
  for (name in to_drop) {
    tryCatch(
      .dropTempTable(handle, name),
      error = function(e) {
        failures <<- c(failures, paste0(name, ": ", conditionMessage(e)))
        NULL
      }
    )
  }
  if (length(failures) > 0L) {
    stop("Could not release operation-owned temporary objects: ",
         paste(unique(failures), collapse = "; "), call. = FALSE)
  }
  invisible(NULL)
}
