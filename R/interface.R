# Module: DataSHIELD Exposed Methods
# All DataSHIELD assign/aggregate methods. Thin wrappers around internal functions.

# --- JSON transport for Opal compatibility ---

#' Deserialize a possibly-JSON argument
#'
#' When complex R objects (lists, named vectors) are passed through
#' \code{datashield.assign.expr()} or \code{datashield.aggregate()}, Opal
#' serializes them via \code{deparse()}, which generates \code{structure()} or
#' \code{c()} calls. These base R functions are not in Opal's DataSHIELD method
#' whitelist, causing \code{400 Bad Request} errors.
#'
#' The solution: the client wraps complex arguments in
#' \code{jsonlite::toJSON(auto_unbox = TRUE)}, and the server calls this helper
#' to transparently deserialize them. DSLite passes native R objects directly,
#' so this function is a no-op when the argument is already a list.
#'
#' @param x An argument that may be a JSON string or an already-parsed R object.
#' @return The deserialized R object.
#' @keywords internal
.ds_arg <- function(x) {
  if (is.character(x) && length(x) == 1) {
    if (startsWith(x, "B64:")) {
      # URL-safe base64 → standard base64
      b64 <- substring(x, 5)
      b64 <- gsub("-", "+", b64)
      b64 <- gsub("_", "/", b64)
      # Restore padding
      pad <- (4 - nchar(b64) %% 4) %% 4
      if (pad > 0) b64 <- paste0(b64, strrep("=", pad))
      json <- rawToChar(jsonlite::base64_dec(b64))
      return(jsonlite::fromJSON(json, simplifyVector = FALSE))
    }
    if (nchar(x) > 0 && substr(x, 1, 1) %in% c("{", "[")) {
      return(jsonlite::fromJSON(x, simplifyVector = FALSE))
    }
  }
  x
}

#' Resolve the unified cohort scope argument for an exploration aggregate
#'
#' The exploration aggregates expose a single \code{cohort} argument that accepts
#' the forms a caller may name a population by — a cohort temp table name
#' (character), a cohort_definition_id (numeric/integer-like string), or NULL —
#' plus the legacy \code{cohort_table} argument (an explicit temp table name).
#' This unwraps both (DataSHIELD-decoding each), gives \code{cohort} precedence,
#' and resolves the result to a materialized, size-checked cohort temp table via
#' \code{\link{.resolveCohortTable}} (which re-gates on distinct subjects, so a
#' too-small cohort can never scope a query). Returns a temp table NAME or NULL.
#'
#' @param handle CDM handle.
#' @param cohort The unified \code{cohort} argument (or NULL).
#' @param cohort_table The legacy \code{cohort_table} argument (or NULL).
#' @return Character cohort temp table name, or NULL.
#' @keywords internal
.resolveCohortArg <- function(handle, cohort = NULL, cohort_table = NULL) {
  cohort <- .ds_arg(cohort)
  cohort_table <- .ds_arg(cohort_table)
  # A list-wrapped scalar (from JSON transport) collapses to its first element.
  if (is.list(cohort)) cohort <- if (length(cohort)) cohort[[1]] else NULL
  if (is.list(cohort_table)) {
    cohort_table <- if (length(cohort_table)) cohort_table[[1]] else NULL
  }
  scope <- cohort %||% cohort_table
  .resolveCohortTable(handle, scope)
}

# --- Handle management ---

#' Retrieve a stored OMOP CDM handle
#'
#' Looks up the server-side OMOP CDM handle object associated with a given
#' resource symbol name. In DSLite (multi-server, single R process), each
#' server's session environment holds its own handle, avoiding collisions.
#' Rock sessions use the package-local registry because each R connection has
#' its own process, while DSLite handles remain in their per-server session
#' environment because multiple DSLite servers share one R process.
#'
#' @param symbol Character; the resource symbol name identifying the handle.
#' @return The OMOP CDM handle object.
#' @keywords internal
.getHandle <- function(symbol) {
  local_key <- paste0(".dsomop_handle_", symbol)

  # Search active call frames because DSLite wrappers can add different depths.
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    if (exists(local_key, envir = frames[[i]], inherits = FALSE)) {
      return(get(local_key, envir = frames[[i]], inherits = FALSE))
    }
  }

  # Fallback: process-local registry for Rock sessions and internal tests.
  key <- paste0("handle_", symbol)
  if (!exists(key, envir = .dsomop_env)) {
    stop("No OMOP handle for symbol '", symbol,
         "'. Call omopInitDS first.", call. = FALSE)
  }
  get(key, envir = .dsomop_env)
}

#' Store an OMOP CDM handle in the process-local registry
#'
#' Saves the given handle object into dsOMOP's package-local registry under a
#' key derived from the resource symbol name. Rock uses this registry because
#' each DataSHIELD connection runs in its own R process. Overwrites any existing
#' handle for the same symbol.
#'
#' @param symbol Character; the resource symbol name.
#' @param handle The OMOP CDM handle object to store.
#' @return Invisible NULL (called for side effect).
#' @keywords internal
.setHandle <- function(symbol, handle) {
  key <- paste0("handle_", symbol)
  assign(key, handle, envir = .dsomop_env)
}

#' Remove an OMOP CDM handle from the server session
#'
#' Closes the database connection associated with the handle and removes it
#' from either the calling DSLite session environment or Rock's process-local
#' registry. The latter is also used by internal tests. No-op if no handle
#' exists for the given symbol, making disconnect retries safe.
#'
#' @param symbol Character; the resource symbol name.
#' @return Invisible NULL (called for side effect).
#' @keywords internal
.removeHandle <- function(symbol) {
  local_key <- paste0(".dsomop_handle_", symbol)
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    if (exists(local_key, envir = frames[[i]], inherits = FALSE)) {
      handle <- get(local_key, envir = frames[[i]], inherits = FALSE)
      .closeHandle(handle)
      rm(list = local_key, envir = frames[[i]])
      return(invisible(TRUE))
    }
  }

  key <- paste0("handle_", symbol)
  if (exists(key, envir = .dsomop_env)) {
    handle <- get(key, envir = .dsomop_env)
    .closeHandle(handle)
    rm(list = key, envir = .dsomop_env)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

#' Person/subject key columns: pseudonymized and retained (not dropped)
#'
#' The only identifier columns kept in DataSHIELD outputs. On output their raw
#' values are replaced by a per-resource reversible token (see
#' \code{\link{.pseudonymizeIdentifiers}}), so they stay usable as join keys
#' for client-side merges and cohort set-operations while never exposing a raw
#' CDM identifier. Every other identifier column is dropped.
#' @keywords internal
.PERSON_KEY_COLS <- function() c("person_id", "subject_id")

#' Cohort-episode keys retained for longitudinal joins but never filterable
#' @keywords internal
.EPISODE_KEY_COLS <- function() {
  c("cohort_row_id", "row_id", "rowid", "rowId")
}

#' OMOP CDM row-level identifier columns
#'
#' Single source of truth shared by \code{\link{.pseudonymizeIdentifiers}}
#' (which pseudonymizes the person/subject keys and drops the rest) and
#' \code{\link{.applyColumnAliases}} (which refuses to rename them, so the
#' pseudonymize/drop step cannot be bypassed by aliasing a key to an
#' unrecognised name).
#'
#' Primary keys (person_id, *_occurrence_id) and entity foreign keys
#' (provider_id, care_site_id, location_id) that uniquely or quasi-uniquely
#' identify rows.
#' @keywords internal
.identifierColumns <- function() {
  c(
    # Person / subject identifiers (pseudonymized, retained)
    "person_id", "subject_id",
    # Clinical event / era / observation row IDs (dropped). Keep this list in
    # sync with the non-CONCEPT primary/entity keys in the vendored OHDSI CDM
    # 5.3/5.4 field specifications. Several polymorphic event links are not
    # declared as formal FKs by OHDSI, but are still row identifiers and must
    # not survive into a DataSHIELD object.
    "visit_occurrence_id", "visit_detail_id",
    "condition_occurrence_id", "drug_exposure_id",
    "procedure_occurrence_id", "measurement_id",
    "observation_id", "device_exposure_id",
    "specimen_id", "note_id", "note_nlp_id",
    "observation_period_id", "payer_plan_period_id",
    "condition_era_id", "drug_era_id", "dose_era_id",
    "episode_id", "metadata_id", "cost_id",
    # Polymorphic / parent event identifiers (not consistently marked as FKs)
    "measurement_event_id", "observation_event_id", "note_event_id",
    "cost_event_id", "event_id", "episode_parent_id",
    "fact_id_1", "fact_id_2", "specimen_source_id", "production_id",
    "preceding_visit_occurrence_id", "preceding_visit_detail_id",
    "parent_visit_detail_id", "visit_detail_parent_id",
    # Provider / location entity keys (dropped)
    "provider_id", "care_site_id", "location_id"
  )
}

#' Reviewed non-row identifier names
#'
#' Row/entity identifiers remain governed by \code{\link{.identifierColumns}}
#' and are always removed. This companion allow-list contains identifier-shaped
#' names whose semantics are fixed either by the vendored OHDSI CDM metadata or
#' by a typed dsOMOP output contract (for example \code{analysis_id} and
#' \code{covariate_id}). It exists solely so the final default-deny identifier
#' pass can distinguish reviewed analytic/reference dimensions from an unknown
#' local extension such as \code{encounter_id}.
#'
#' @return Lower-case character vector.
#' @keywords internal
.reviewedIdentifierColumns <- function() {
  cache_key <- "reviewed_identifier_columns_v1"
  if (exists(cache_key, envir = .dsomop_env, inherits = FALSE)) {
    return(get(cache_key, envir = .dsomop_env, inherits = FALSE))
  }

  official <- unique(unlist(lapply(c("5.3", "5.4"), function(version) {
    spec <- tryCatch(.loadVendoredSpec(version), error = function(e) NULL)
    if (is.null(spec) || is.null(spec$field_level) ||
        !"cdmFieldName" %in% names(spec$field_level)) {
      return(character(0))
    }
    tolower(as.character(spec$field_level$cdmFieldName))
  }), use.names = FALSE))

  # These are controller-owned analytic/reference dimensions, not source-row
  # identifiers. Keep the list explicit: an unfamiliar *_id remains denied.
  analytic <- c(
    "analysis_id", "case_id", "cohort_id", "covariate_id", "database_id",
    "dataset_id", "lineage_id", "outcome_id", "output_id", "population_id",
    "query_id", "release_id", "rule_id", "semantic_query_id", "snapshot_id",
    "time_id", "tool_id", "upstream_id"
  )
  reviewed <- unique(tolower(c(official, analytic, .EPISODE_KEY_COLS())))
  assign(cache_key, reviewed, envir = .dsomop_env)
  reviewed
}

#' Find identifier-shaped columns without reviewed semantics
#'
#' The rule is deliberately lexical and default-deny: bare \code{id} and names
#' ending in \code{_id}, \code{_key}, or \code{_identifier} are identifiers
#' unless their exact names have been reviewed. Person keys and explicitly
#' concept-shaped columns are handled by their dedicated contracts.
#'
#' @param columns Character vector of column names.
#' @param reviewed Exact names whose non-row semantics have been reviewed.
#' @param allow_concepts Whether \code{*_concept_id} names count as typed OMOP
#'   concept identifiers.
#' @return The original names classified as untyped identifiers.
#' @keywords internal
.untypedIdentifierColumns <- function(columns, reviewed = character(0),
                                      allow_concepts = TRUE) {
  if (length(columns) == 0L) return(character(0))
  columns <- as.character(columns)
  lower <- tolower(columns)
  valid <- !is.na(lower) & nzchar(trimws(lower))
  identifier_shaped <- valid & grepl(
    "(^id$|_(id|key|identifier)$)", lower, perl = TRUE
  )
  person_or_episode <- lower %in% tolower(c(
    .PERSON_KEY_COLS(), .EPISODE_KEY_COLS()
  ))
  concept <- isTRUE(allow_concepts) & grepl("_concept_id$", lower)
  typed <- lower %in% tolower(as.character(reviewed))
  columns[identifier_shaped & !person_or_episode & !concept & !typed]
}

#' Identifier columns that must not survive an assigned output
#'
#' Combines the reviewed OHDSI row/entity list with a default-deny fallback for
#' identifier-shaped names not known to the CDM or a typed dsOMOP output.
#'
#' @param columns Character vector of landed column names.
#' @return Character vector to remove.
#' @keywords internal
.outputIdentifierColumns <- function(columns) {
  columns <- as.character(columns)
  unique(c(
    columns[tolower(columns) %in% .identifierColumns()],
    .untypedIdentifierColumns(
      columns, reviewed = .reviewedIdentifierColumns(), allow_concepts = TRUE
    )
  ))
}

#' Derive encryption and authentication keys for a resource
#'
#' Derives independent SHA-256 subkeys for AES-256 encryption, its deterministic
#' IV, and HMAC authentication. The fixed IV intentionally preserves equality
#' for server-side joins; encrypt-then-MAC prevents a client from modifying a
#' token and turning the decryptor into a padding/oracle surface.
#' @param key Raw vector; the per-resource secret (\code{handle$person_key}).
#' @return List with \code{aes}, \code{iv}, and \code{mac} raw keys.
#' @keywords internal
.deriveAesParams <- function(key) {
  if (is.character(key)) key <- charToRaw(paste(key, collapse = ""))
  key <- as.raw(key)
  list(
    aes = as.raw(openssl::sha256(c(key, charToRaw("dsomop-aes-key")))),
    iv  = as.raw(openssl::sha256(c(key, charToRaw("dsomop-aes-iv"))))[1:16],
    mac = as.raw(openssl::sha256(c(key, charToRaw("dsomop-mac-key"))))
  )
}

#' Pseudonymize a person/subject key vector reversibly with a per-resource key
#'
#' Returns DETERMINISTIC, NON-NUMERIC tokens computed element-wise by
#' AES-256-CBC encrypting each id (as character) under a key + fixed IV derived
#' from the per-resource secret. The same id under the same key always yields
#' the same token, so tokens are stable across reconnects and DataSHIELD
#' workspace save/load and joinable on the key; a different resource (different
#' key) yields different tokens, so a person is not linkable across sites.
#'
#' Each version-2 token is
#' \code{paste0("p2", <hex ciphertext>, ".", <hex HMAC>)}. The leading
#' \code{"p"}
#' GUARANTEES the token is non-numeric: hex alone can be all digits, which
#' \code{as.numeric}/\code{ds.asNumeric} would parse back into a number; the
#' letter prefix forces \code{as.numeric} to \code{NA}, so the id cannot be
#' recovered or inferred client-side. The transform is reversible SERVER-SIDE
#' only (see \code{\link{.unhashPersonKey}}); the client never holds the key.
#'
#' @param ids A vector of identifier values.
#' @param key Raw vector; the per-resource secret key (\code{handle$person_key}).
#' @return Character vector of pseudonymous tokens (NA preserved).
#' @keywords internal
.hashPersonKey <- function(ids, key) {
  params <- .deriveAesParams(key)
  ids <- as.character(ids)  # exact — integer/character, never a rounded double.
  # Encrypt each DISTINCT id once, then map back onto the rows. Person/subject
  # ids repeat heavily in long-format outputs (one row per event), so a per-row
  # AES-CBC call would redo identical work; distinct + match() yields byte-for-
  # byte the same tokens with far less compute (≈ Nrows/Ndistinct speedup).
  u <- unique(ids)
  tok_u <- vapply(u, function(id) {
    if (is.na(id)) return(NA_character_)
    ct <- openssl::aes_cbc_encrypt(charToRaw(id), key = params$aes, iv = params$iv)
    tag <- openssl::sha256(
      c(charToRaw("dsomop-person-token-v2"), ct), key = params$mac
    )
    # "p" prefix forces as.numeric()/ds.asNumeric() -> NA (non-numeric token).
    paste0(
      "p2", paste(as.character(ct), collapse = ""), ".",
      paste(as.character(tag), collapse = "")
    )
  }, character(1L), USE.NAMES = FALSE)
  tok_u[match(ids, u)]
}

#' Reverse a person-key token back to the original id (SERVER-ONLY)
#'
#' Inverse of \code{\link{.hashPersonKey}}. It verifies the version-2 HMAC before
#' AES-256-CBC decryption, and rejects malformed, modified, legacy, or wrong-key
#' tokens with one generic error. This is the server-side reverse map used for
#' population-scoping joins; the client never holds either derived key.
#'
#' @param token Character vector of tokens produced by \code{.hashPersonKey}.
#' @param key Raw vector; the per-resource secret key (\code{handle$person_key}).
#' @return Character vector of the original identifier values.
#' @keywords internal
.unhashPersonKey <- function(token, key) {
  params <- .deriveAesParams(key)
  token <- as.character(token)
  invalid <- function() {
    stop("Invalid or unauthenticated person-key token.", call. = FALSE)
  }
  hex_to_raw <- function(value) {
    if (!nzchar(value) || nchar(value) %% 2L != 0L ||
        !grepl("^[0-9a-fA-F]+$", value)) {
      invalid()
    }
    as.raw(strtoi(
      substring(value, seq(1L, nchar(value), 2L),
                seq(2L, nchar(value), 2L)), 16L
    ))
  }
  vapply(token, function(tk) {
    if (is.na(tk)) return(NA_character_)
    if (!grepl("^p2[0-9a-fA-F]+\\.[0-9a-fA-F]{64}$", tk)) {
      invalid()
    }
    pieces <- strsplit(sub("^p2", "", tk), ".", fixed = TRUE)[[1]]
    ct <- hex_to_raw(pieces[1])
    supplied_tag <- hex_to_raw(pieces[2])
    expected_tag <- as.raw(openssl::sha256(
      c(charToRaw("dsomop-person-token-v2"), ct), key = params$mac
    ))
    # Compare every byte. A short-circuit equality check would turn this
    # server-only verifier into a (weak but avoidable) remote timing oracle.
    tag_mismatch <- length(supplied_tag) != length(expected_tag)
    if (!tag_mismatch) {
      tag_mismatch <- sum(bitwXor(
        as.integer(supplied_tag), as.integer(expected_tag))) != 0L
    }
    if (tag_mismatch) {
      invalid()
    }
    tryCatch(
      rawToChar(openssl::aes_cbc_decrypt(
        ct, key = params$aes, iv = params$iv
      )),
      error = function(e) invalid()
    )
  }, character(1L), USE.NAMES = FALSE)
}

#' Pseudonymize/strip row-level identifiers before DataSHIELD assignment
#'
#' Runs on every ASSIGN output before \code{base::assign()}. Person and subject
#' keys (\code{\link{.PERSON_KEY_COLS}}) are REPLACED by a per-resource reversible
#' token under their original column names (so existing analysis code and the
#' output contract are unchanged) and tagged via the \code{dsomop_protected}
#' attribute so the factor/level layer refuses to expose them. Every other
#' identifier column is DROPPED.
#'
#' @section Disclosure model:
#' This is defense-in-depth, not the sole protection. The authoritative barrier
#' is OUTPUT gating: dsBase suppresses small aggregate cells and small subsets
#' (it gates the values that leave the server, not which columns exist
#' server-side), and dsOMOP enforces \code{nfilter} on its own aggregates. A
#' pseudonymous key is therefore safe to retain — it is a high-cardinality
#' token that cannot serve as a stat/group variable (dsBase nfilter.levels.max
#' plus the \code{dsomop_protected} guard both block that) and cannot be
#' reversed. Row-level data, including this key, already lives server-side
#' exactly as it does for any \code{ds.glm} fit.
#'
#' @param x Data frame or list to sanitize. Operates recursively on lists.
#' @param key Raw vector; the per-resource secret key (\code{handle$person_key}).
#' @param pseudonymization Public, non-secret key contract returned by
#'   \code{\link{.personKeyPublicContract}}. It is required for person-bearing
#'   frames and is persisted as the \code{dsomop_pseudonymization} attribute.
#' @return Sanitized object: person/subject keys pseudonymized, other
#'   identifier columns removed.
#' @keywords internal
.pseudonymizeIdentifiers <- function(x, key, pseudonymization = NULL) {
  # Staged descriptors point to files that were sanitized before writing.
  # Preserve their S3 class and metadata when walking a composite sparse/
  # temporal result; recursively lapply()-ing the descriptor would turn it
  # into an unrecognised plain list.
  if (inherits(x, "FlowerDatasetDescriptor")) return(x)

  if (is.data.frame(x)) {
    drop <- setdiff(.outputIdentifierColumns(names(x)), .PERSON_KEY_COLS())
    if (length(drop) > 0) {
      x[drop] <- NULL
    }
    keys <- intersect(.PERSON_KEY_COLS(), names(x))
    public_contract <- NULL
    if (length(keys) > 0L) {
      if (!is.raw(key) || length(key) != 32L) {
        stop("Person-bearing outputs require exactly 32 raw bytes of ",
             "pseudonymization key material.", call. = FALSE)
      }
      if (is.null(pseudonymization)) {
        stop("Person-bearing outputs require an explicit public ",
             "pseudonymization contract.", call. = FALSE)
      }
      public_contract <- .canonicalPseudonymizationContract(pseudonymization)
      if (!identical(public_contract$key_id, .personKeyId(key))) {
        stop("The public pseudonymization contract does not identify the ",
             "supplied key; refusing to publish incompatible person tokens.",
             call. = FALSE)
      }
    }
    for (k in keys) {
      src <- x[[k]]
      tok <- .hashPersonKey(src, key)
      # Fail closed: distinct source ids MUST map to distinct tokens. A drop in
      # cardinality means a hash (or upstream precision) collision would merge
      # two real identities into one pseudonym — a correctness and disclosure
      # hazard. Abort rather than emit silently-merged rows.
      nd_src <- length(unique(src[!is.na(src)]))
      nd_tok <- length(unique(tok[!is.na(tok)]))
      if (nd_tok < nd_src) {
        stop("Person-key pseudonymization collision: ", nd_src,
             " distinct ids mapped to ", nd_tok, " tokens for column '", k,
             "'. Aborting to avoid merging identities.", call. = FALSE)
      }
      x[[k]] <- tok
    }
    episode_keys <- intersect(.EPISODE_KEY_COLS(), names(x))
    if (length(keys) > 0L || length(episode_keys) > 0L) {
      protected <- union(keys, episode_keys)
      attr(x, "dsomop_protected") <- union(attr(x, "dsomop_protected"),
                                            protected)
      # Tag every person-bearing assign output as an omop.table (additively, so
      # data.frame methods still dispatch). The client-side data-manipulation
      # verbs (omopMergeDS/omopFilterDS/omopSelectDS/omopBindRowsDS) require this
      # class so they can only ever operate on disclosure-controlled, token-keyed
      # frames produced by dsOMOP — never on arbitrary client-built data.
      if (length(keys) > 0L) {
        attr(x, "dsomop_pseudonymization") <- public_contract
        class(x) <- union("omop.table", class(x))
      }
    }
  } else if (is.list(x)) {
    x <- lapply(
      x, .pseudonymizeIdentifiers, key = key,
      pseudonymization = pseudonymization
    )
  }
  x
}

# --- Assign methods ---

#' Initialize an OMOP CDM handle (Assign)
#'
#' @description
#' Creates a server-side connection to an OMOP CDM database from a DataSHIELD
#' resource. The handle is retained within the current DataSHIELD server
#' session and used by all subsequent OMOP operations.
#'
#' @param resource_symbol Character; the resource symbol name
#' @param cdm_schema Character; override CDM schema
#' @param results_schema Character; results schema name
#' @param vocab_schema Character; vocabulary schema name
#' @param temp_schema Character; temp schema name
#' @param config Named list; additional configuration
#' @return The handle symbol (assigned server-side)
#' @examples
#' \dontrun{
#' omopInitDS("omop_resource")
#' }
#' @export
omopInitDS <- function(resource_symbol,
                       cdm_schema = NULL,
                       results_schema = NULL,
                       vocab_schema = NULL,
                       temp_schema = NULL,
                       config = list()) {
  # SECURITY: resource_symbol comes from the client and is used to look up
  # a variable in the DataSHIELD session environment. Without validation,
  # a malicious client could pass arbitrary strings (e.g., "system('rm -rf')")
  # that, if passed to eval(parse()), would execute arbitrary code.
  # We validate it as a strict R identifier, then use get() (not eval/parse).
  .validateIdentifier(resource_symbol, "resource symbol")
  .assertAllowedSchemaOverrides(list(
    cdm_schema = cdm_schema,
    vocab_schema = vocab_schema,
    results_schema = results_schema,
    temp_schema = temp_schema
  ))
  session_env <- parent.frame()
  local_key <- paste0(".dsomop_handle_", resource_symbol)
  global_key <- paste0("handle_", resource_symbol)
  if (exists(local_key, envir = session_env, inherits = FALSE) ||
      exists(global_key, envir = .dsomop_env, inherits = FALSE)) {
    stop("An OMOP handle is already active for resource symbol '",
         resource_symbol, "'. Close it with omopCleanupDS(close = TRUE) before ",
         "initializing that resource symbol again.", call. = FALSE)
  }

  resolved <- get(resource_symbol, envir = session_env, inherits = FALSE)
  session_name <- attr(session_env, "name", exact = TRUE)
  dslite_session <- is.character(session_name) &&
    length(session_name) == 1L && !is.na(session_name) &&
    grepl("^DSLiteEnv_[0-9]{4}$", session_name)
  rock_info <- if (exists(".info", envir = .GlobalEnv, inherits = FALSE)) {
    get(".info", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  rock_info_valid <- is.list(rock_info) &&
    all(c("id", "type", "cluster", "tags") %in% names(rock_info)) &&
    is.character(rock_info$id) && length(rock_info$id) == 1L &&
    !is.na(rock_info$id) && nzchar(rock_info$id) &&
    identical(rock_info$type, "rock") &&
    is.character(rock_info$cluster) && length(rock_info$cluster) == 1L &&
    !is.na(rock_info$cluster) && nzchar(rock_info$cluster) &&
    (is.character(rock_info$tags) ||
       (is.list(rock_info$tags) && length(rock_info$tags) == 0L))
  rock_runtime <- nzchar(Sys.getenv("ROCK_VERSION", unset = "")) ||
    (nzchar(Sys.getenv("ROCK_HOME", unset = "")) && rock_info_valid)
  process_local <- !dslite_session && rock_runtime
  store_handle <- function(value) {
    if (process_local) {
      .setHandle(resource_symbol, value)
    } else {
      assign(local_key, value, envir = session_env)
    }
    invisible(TRUE)
  }

  # This is the first backend-independent point at which a real dsOMOP request
  # is known to be running and DataSHIELD profile options have been applied.
  # A DP-disabled service creates no privacy key. When sticky releases are
  # enabled, initialize or validate their single persistent root now so
  # deployment faults surface before the first protected release.
  .dsomopDpEnsureRuntime()

  # DataSHIELD backends may expose a resolved ResourceClient or the raw
  # resource object. Handle both forms.
  if (inherits(resolved, "ResourceClient")) {
    resource_client <- resolved
  } else {
    resource_client <- resourcer::newResourceClient(resolved)
  }

  handle <- .createHandle(
    resource_client,
    cdm_schema = cdm_schema,
    vocab_schema = vocab_schema,
    results_schema = results_schema,
    temp_schema = temp_schema,
    config = config
  )

  committed <- FALSE
  cleanup_attempted <- FALSE
  on.exit({
    if (!committed && !cleanup_attempted) try(.closeHandle(handle), silent = TRUE)
  }, add = TRUE)

  abort_initialization <- function(e) {
    cleanup_attempted <<- TRUE
    cleanup_error <- tryCatch({
      .closeHandle(handle)
      NULL
    }, error = identity)
    if (!is.null(cleanup_error)) {
      retained <- tryCatch({
        store_handle(handle)
        TRUE
      }, error = function(store_error) FALSE)
      stop(
        "OMOP handle initialization failed and cleanup could not be proven",
        if (retained) {
          paste0(". The failed handle was retained; call omopCleanupDS('",
                 resource_symbol, "', close = TRUE) before retrying.")
        } else {
          "; the failed handle could not be retained for a cleanup retry."
        },
        " Initialization error: ", conditionMessage(e),
        "; cleanup error: ", conditionMessage(cleanup_error),
        call. = FALSE
      )
    }
    stop(e)
  }

  tryCatch(.buildBlueprint(handle), error = abort_initialization)

  # DSLite servers share one R process, so handles stay in the calling server
  # environment. Rock wrapper frames are ephemeral, while each R connection is
  # process-isolated, so retain that handle in the package-local registry.
  tryCatch(
    store_handle(handle),
    error = function(e) {
      abort_initialization(simpleError(
        "Could not store the OMOP handle in the DataSHIELD session."
      ))
    }
  )

  committed <- TRUE
  invisible(TRUE)
}

#' Execute an extraction plan (Assign)
#'
#' @description
#' Runs the extraction plan and assigns each output directly into the
#' DataSHIELD session as a named symbol. Sparse outputs are split into
#' three symbols: \code{<name>.covariates}, \code{<name>.covariateRef}, and
#' the row-to-pseudonymous-person map \code{<name>.personRef}.
#' Temporal covariates are split into four symbols, including the episode-to-
#' person map \code{<name>.personRef}.
#' Recurrent-event survival outputs are split into \code{<name>.events} and
#' \code{<name>.riskSets} so each component remains an ordinary protected table.
#' Multi-state survival outputs are split into \code{<name>.msdata} and the
#' public graph dictionary \code{<name>.transitionRef}.
#'
#' When \code{output_mode = "staged"}, outputs are written to protected,
#' server-local files and assigned as descriptors inheriting from
#' \code{FlowerDatasetDescriptor} and \code{OMOPStagedDatasetDescriptor}.
#' Long untranslated event outputs and the stateful multi-state transform stream
#' in bounded chunks; other formats requiring R-side transforms are materialized
#' before staging. Each plan-produced descriptor
#' carries a public \code{metadata$semantic_contract} snapshot of its resolved
#' output shape and the server's age/date harmonization policy. The descriptors
#' are not download URLs. Same-account readers should resolve them with
#' \code{\link{omopStagedDatasetPath}}; other services require a separately
#' reviewed server-side broker.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param plan List; the extraction plan
#' @param out Named character vector; output_name -> symbol_name mapping
#' @param output_mode Character; "memory" (default) or "staged"
#' @param scope Optional recipe-level scope: \code{omop.table} symbol(s) (which
#'   cannot ride in the plan JSON, so they are spliced in by name here) and/or a
#'   cohort reference. Folded by \code{combine} into ONE cohort that is
#'   intersected into every population (NULL = no extra scoping).
#' @param combine Character; "union" (default) or "intersect" when the scope has
#'   multiple sources.
#' @param ... Additional scalar cohort references and resolved workspace tables,
#'   independently and contiguously named \code{scope_cohort_1}, ... and
#'   \code{scope_table_1}, ... without gaps. They are combined internally with
#'   a literal \code{scope}; no generic aggregate \code{c()} or \code{list()}
#'   method is required.
#' @return Invisible TRUE (outputs are assigned into caller's environment)
#' @examples
#' \dontrun{
#' omopPlanExecuteDS("omop", plan, out = c(cohort = "my_cohort"))
#' omopPlanExecuteDS("omop", plan, out = c(features = "D"),
#'                   output_mode = "staged")
#' }
#' @export
omopPlanExecuteDS <- function(omop_symbol, plan, out,
                               output_mode = "memory",
                               scope = NULL, combine = "union", ...) {
  handle <- .getHandle(omop_symbol)
  plan <- .ds_arg(plan)
  out <- .ds_arg(out)
  output_mode <- .ds_arg(output_mode)
  if (!is.character(output_mode) || length(output_mode) != 1L ||
      is.na(output_mode) || !output_mode %in% c("memory", "staged")) {
    stop("output_mode must be 'memory' or 'staged'.", call. = FALSE)
  }
  if (!is.list(out) && !is.character(out)) {
    stop("out must be a named output-to-symbol mapping.", call. = FALSE)
  }
  if (length(out) == 0L || is.null(names(out)) || any(!nzchar(names(out))) ||
      anyDuplicated(names(out))) {
    stop("out must have unique, non-empty output names.", call. = FALSE)
  }
  symbols <- vapply(out, function(sym) {
    if (!is.character(sym) || length(sym) != 1L || is.na(sym) ||
        !grepl("^[A-Za-z][A-Za-z0-9._]*$", sym)) {
      stop("Every output symbol must be one simple non-reserved R name.",
           call. = FALSE)
    }
    sym
  }, character(1))
  if (anyDuplicated(symbols) || any(symbols == omop_symbol) ||
      any(grepl("^(\\.dsomop_|handle_)", symbols))) {
    stop("Output symbols must be unique and cannot target OMOP resources or ",
         "reserved handle names.", call. = FALSE)
  }
  expanded <- unlist(lapply(symbols, function(sym) paste0(
    sym, c("", ".covariates", ".covariateRef", ".temporalCovariates",
           ".timeRef", ".personRef", ".personPeriods", ".events",
           ".riskSets", ".msdata", ".transitionRef")
  )), use.names = FALSE)
  if (anyDuplicated(expanded)) {
    stop("Output symbols collide after sparse/temporal suffix expansion.",
         call. = FALSE)
  }
  out <- stats::setNames(as.list(symbols), names(out))

  # Recipe-level scope. omop.table SYMBOL sources are spliced in by name (DSI
  # resolves them to server-side frames) because they cannot cross in the plan
  # JSON; a cohort reference may also be present. Carry the resolved frames and
  # the cohort ref / fold operator on plan$scope, where .planResolveScopeCohort
  # folds them into ONE re-gated cohort (via .omopAnalysisResolveScope) and
  # intersects it into every population.
  scope <- .ds_arg(scope)
  scope <- .omopAnalysisScopeFromDots(scope, list(...))
  combine <- .ds_arg(combine)
  if (is.list(combine)) combine <- combine[[1]]
  if (!is.null(scope)) {
    if (is.null(plan$scope)) plan$scope <- list()
    plan$scope$tables_frames <- scope
    plan$scope$combine <- plan$scope$combine %||% combine
  }

  # Never serialize resolved workspace frames into an audit record. Preserve
  # the JSON-safe recipe definition, but replace the injected frame payload by
  # non-sensitive structural metadata.
  audit_plan <- plan
  if (is.list(audit_plan$scope) && !is.data.frame(audit_plan$scope)) {
    audit_plan$scope$tables_frames <- NULL
  }
  .omopAuditLog(
    "omopPlanExecuteDS",
    list(outputs = names(out), plan = audit_plan,
         scope_present = !is.null(scope),
         scope_table_count = .omopAnalysisScopeTableCount(scope))
  )
  outputs <- .planExecute(handle, plan, out, output_mode = output_mode)

  # Validate that requested outputs were produced
  missing <- setdiff(names(out), names(outputs))
  if (length(missing) > 0) {
    warning("Plan did not produce outputs: ",
            paste(missing, collapse = ", "), call. = FALSE)
  }

  assign_env <- parent.frame()
  concept_cols <- attr(outputs, "omop_concept_cols") %||% list()
  person_key <- .personKey(handle)
  pseudonymization <- .personKeyPublicContract(handle)

  for (nm in names(out)) {
    sym <- out[[nm]]
    result <- outputs[[nm]]
    if (is.null(result)) next

    # Staged descriptors: assign directly (no data to strip)
    if (inherits(result, "FlowerDatasetDescriptor")) {
      assign(sym, result, envir = assign_env)
      next
    }

    # Pseudonymize person/subject keys (per-resource reversible token, kept under
    # their original names so joins/set-ops and the output contract still work)
    # and drop every other row-level identifier, before data enters the
    # DataSHIELD environment. Output gating (cell/subset suppression by dsBase
    # and dsOMOP nfilter) remains the authoritative disclosure barrier.
    result <- .pseudonymizeIdentifiers(
      result, person_key, pseudonymization = pseudonymization
    )

    # Multi-state outputs carry an expanded transition risk set and a public
    # graph dictionary. Keep both as first-class tables/descriptors because
    # data-frame attributes do not survive Parquet interchange.
    if (is.list(result) && !is.data.frame(result) &&
        all(c("msdata", "transition_ref") %in% names(result))) {
      msdata <- .dsomopDpSealPlanOutput(
        result$msdata, plan, nm,
        dataset_identity = .dsomopDpDatasetIdentity(handle),
        component = "msdata"
      )
      transition_ref <- .dsomopDpSealPlanOutput(
        result$transition_ref, plan, nm,
        dataset_identity = .dsomopDpDatasetIdentity(handle),
        component = "transition_ref"
      )
      assign(paste0(sym, ".msdata"), msdata, envir = assign_env)
      assign(paste0(sym, ".transitionRef"), transition_ref,
             envir = assign_env)

    # Recurrent-event outputs have an event stream and a separate episode risk
    # set. Keep both as first-class protected tables (or staged descriptors).
    } else if (is.list(result) && !is.data.frame(result) &&
        all(c("events", "risk_sets") %in% names(result))) {
      events <- .dsomopDpSealPlanOutput(
        result$events, plan, nm,
        dataset_identity = .dsomopDpDatasetIdentity(handle),
        component = "events"
      )
      risk_sets <- .dsomopDpSealPlanOutput(
        result$risk_sets, plan, nm,
        dataset_identity = .dsomopDpDatasetIdentity(handle),
        component = "risk_sets"
      )
      assign(paste0(sym, ".events"), events, envir = assign_env)
      assign(paste0(sym, ".riskSets"), risk_sets, envir = assign_env)

    # Temporal covariates: split into four symbols, preserving the explicit
    # cohort-episode -> pseudonymous-person map needed by longitudinal models.
    } else if (is.list(result) && !is.data.frame(result) &&
        "temporalCovariates" %in% names(result)) {
      if (!is.null(result$personPeriods)) {
        assign(paste0(sym, ".personPeriods"),
               result$personPeriods, envir = assign_env)
      }
      assign(paste0(sym, ".temporalCovariates"),
             result$temporalCovariates, envir = assign_env)
      assign(paste0(sym, ".covariateRef"),
             result$covariateRef, envir = assign_env)
      assign(paste0(sym, ".timeRef"),
             result$timeRef, envir = assign_env)
      if (!is.null(result$personRef)) {
        assign(paste0(sym, ".personRef"),
               result$personRef, envir = assign_env)
      }
    # Sparse outputs: split list into data-frame symbols, including the
    # complete row-to-pseudonymous-person map.
    } else if (is.list(result) && !is.data.frame(result) &&
        all(c("covariates", "covariateRef") %in% names(result))) {
      assign(paste0(sym, ".covariates"),
             result$covariates, envir = assign_env)
      assign(paste0(sym, ".covariateRef"),
             result$covariateRef, envir = assign_env)
      if (!is.null(result$personRef)) {
        assign(paste0(sym, ".personRef"),
               result$personRef, envir = assign_env)
      }
    } else {
      # Tag the concept-id columns by their landed (possibly renamed) names so
      # the factor harmonization layer recognises them post-rename. Stamped
      # here, after identifier stripping ([ drops frame attributes), as the
      # last step before the symbol is created.
      cc <- intersect(as.character(concept_cols[[nm]]), names(result))
      if (length(cc) > 0L) {
        attr(result, "omop_concept_cols") <- cc
      }
      result <- .dsomopDpSealPlanOutput(
        result, plan, nm, dataset_identity = .dsomopDpDatasetIdentity(handle)
      )
      assign(sym, result, envir = assign_env)
    }
  }

  invisible(TRUE)
}

#' Create a cohort (Assign)
#'
#' @description
#' Creates a cohort on the server side from a cohort specification DSL object.
#' The cohort can be stored as a temporary table or persisted to the results
#' schema depending on the mode parameter.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param cohort_spec List; cohort specification DSL
#' @param mode Character; "temporary" or "persistent"
#' @param cohort_id Integer; cohort definition ID
#' @param name Reserved for future \code{cohort_definition} metadata support.
#'   Persistent mode currently writes cohort rows only.
#' @param overwrite Logical
#' @return Character; temp table name or confirmation
#' @examples
#' \dontrun{
#' result <- omopCohortCreateDS("omop", cohort_spec, mode = "temporary")
#' }
#' @export
omopCohortCreateDS <- function(omop_symbol, cohort_spec,
                               mode = "temporary",
                               cohort_id = NULL,
                               name = NULL,
                               overwrite = FALSE) {
  handle <- .getHandle(omop_symbol)
  temp_tables_before <- unique(handle$temp_tables %||% character(0))
  final_temp_table <- character(0)
  on.exit(
    .dropTempTablesCreatedSince(
      handle, unique(c(temp_tables_before, final_temp_table))
    ),
    add = TRUE
  )
  cohort_spec <- .ds_arg(cohort_spec)
  mode <- .ds_arg(mode)
  if (is.list(mode)) mode <- mode[[1]]
  if (identical(tolower(as.character(mode)), "persistent")) {
    allowed <- getOption("dsomop.allow_persistent_cohorts",
      getOption("default.dsomop.allow_persistent_cohorts", FALSE))
    if (!isTRUE(allowed)) {
      stop("Persistent cohort writes are disabled by the data controller; ",
           "use mode='temporary'.", call. = FALSE)
    }
  }
  .omopAuditLog("omopCohortCreateDS", cohort_spec)
  result <- .cohortCreate(
    handle, cohort_spec, mode, cohort_id, name, overwrite
  )
  if (identical(tolower(as.character(mode)), "temporary")) {
    if (!is.character(result) || length(result) != 1L || is.na(result) ||
        !result %in% (handle$temp_tables %||% character(0))) {
      stop("Temporary cohort creation did not produce one owned final table.",
           call. = FALSE)
    }
    # The returned value is the authoritative final table name (including the
    # `_icN` suffix produced by inclusion criteria).  Preserve only that object;
    # every base/intermediate created by this call is operation-owned cleanup.
    final_temp_table <- result
  }
  result
}

#' Combine cohorts (Assign)
#'
#' @description
#' Combines two existing server-side cohorts using a set operation (intersect,
#' union, or set difference) and stores the result as a new temporary table.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param op Character; "intersect", "union", or "setdiff"
#' @param cohort_a Character; first cohort temp table
#' @param cohort_b Character; second cohort temp table
#' @param new_name Character; result temp table name
#' @return Character; result temp table name
#' @examples
#' \dontrun{
#' combined <- omopCohortCombineDS("omop", "union", "cohort_a", "cohort_b")
#' }
#' @export
omopCohortCombineDS <- function(omop_symbol, op,
                                cohort_a, cohort_b,
                                new_name = NULL) {
  handle <- .getHandle(omop_symbol)
  temp_tables_before <- unique(handle$temp_tables %||% character(0))
  final_temp_table <- character(0)
  on.exit(
    .dropTempTablesCreatedSince(
      handle, unique(c(temp_tables_before, final_temp_table))
    ),
    add = TRUE
  )
  .omopAuditLog("omopCohortCombineDS", list(op = op, a = cohort_a, b = cohort_b))
  result <- .cohortCombine(handle, op, cohort_a, cohort_b, new_name)
  if (!is.character(result) || length(result) != 1L || is.na(result) ||
      !result %in% (handle$temp_tables %||% character(0))) {
    stop("Cohort combination did not produce one owned final table.",
         call. = FALSE)
  }
  final_temp_table <- result
  result
}

#' Build a cohort from a workspace omop.table's person tokens (Assign)
#'
#' @description
#' Turns an existing server-side, token-keyed \code{omop.table} symbol (e.g. a
#' plan output, or a merge/filter/bind result) into a reusable cohort temp table
#' that subsequent exploration aggregates and plan executions can scope by. The
#' CLIENT sends only the symbol NAME; the function reads the frame's DISTINCT
#' person tokens, reverses them to original CDM ids SERVER-SIDE with the
#' per-resource key (\code{\link{.unhashPersonKey}}), gates the distinct count
#' (fail-closed), and materializes a size-checked cohort temp table of original
#' ids (anchored on \code{observation_period} dates, as the plan path does).
#'
#' @param x A server-side \code{omop.table} data.frame (resolved from a symbol by
#'   DataSHIELD).
#' @param omop_symbol Character; the OMOP handle symbol (supplies the per-resource
#'   key and DB connection).
#' @param new_name Character; deterministic name for the cohort temp table (the
#'   client passes one so the returned handle can be reused as a \code{cohort=}
#'   scope). NULL generates a random name.
#' @return Character; the cohort temp table name (assigned to the caller's
#'   symbol). Pass it as the \code{cohort} argument of the exploration aggregates
#'   or as a plan \code{cohort_table} name.
#' @examples
#' \dontrun{
#' omopCohortFromTableDS(my_plan_output, "omop")
#' }
#' @export
omopCohortFromTableDS <- function(x, omop_symbol, new_name = NULL) {
  handle <- .getHandle(omop_symbol)
  temp_tables_before <- unique(handle$temp_tables %||% character(0))
  final_temp_table <- character(0)
  on.exit(
    .dropTempTablesCreatedSince(
      handle, unique(c(temp_tables_before, final_temp_table))
    ),
    add = TRUE
  )
  new_name <- .ds_arg(new_name)
  if (is.list(new_name)) new_name <- if (length(new_name)) new_name[[1]] else NULL
  .omopAuditLog("omopCohortFromTableDS",
                list(n_rows = if (is.data.frame(x)) nrow(x) else NA,
                     new_name = new_name))
  result <- .cohortFromTokenFrame(handle, x, new_name = new_name)
  if (!is.character(result) || length(result) != 1L || is.na(result) ||
      !result %in% (handle$temp_tables %||% character(0))) {
    stop("Cohort materialization did not produce one owned final table.",
         call. = FALSE)
  }
  final_temp_table <- result
  result
}

#' Clean up temp artifacts (Assign)
#'
#' @description
#' With \code{exact = FALSE}, drops all owned server-side temporary tables whose
#' names match the given prefix and cleans staged artifacts. With
#' \code{exact = TRUE}, drops only the named owned table and leaves staging
#' untouched. Called during teardown or when resetting analysis state.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param prefix Character; temp table prefix to clean
#' @param close Logical; when true, close and remove the resource handle after
#'   cleaning. This idempotent mode is used by client disconnect.
#' @param exact Logical; when true, \code{prefix} is treated as one exact,
#'   validated temporary-table name and only that owned object is dropped.
#'   Staged directories are not cleaned in exact mode.
#' @return Invisible TRUE
#' @examples
#' \dontrun{
#' omopCleanupDS("omop", prefix = "dsomop_")
#' }
#' @export
omopCleanupDS <- function(omop_symbol, prefix = "dsomop_", close = FALSE,
                          exact = FALSE) {
  close <- .ds_arg(close)
  if (is.list(close)) close <- if (length(close)) close[[1L]] else FALSE
  if (!is.logical(close) || length(close) != 1L || is.na(close)) {
    stop("close must be TRUE or FALSE.", call. = FALSE)
  }
  exact <- .ds_arg(exact)
  if (is.list(exact)) exact <- if (length(exact)) exact[[1L]] else FALSE
  if (!is.logical(exact) || length(exact) != 1L || is.na(exact)) {
    stop("exact must be TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(close)) {
    .removeHandle(omop_symbol)
    return(invisible(TRUE))
  }
  handle <- .getHandle(omop_symbol)
  if (isTRUE(exact)) {
    prefix <- .validateIdentifier(prefix, "temporary table")
    to_drop <- intersect(prefix, handle$temp_tables %||% character(0))
  } else {
    to_drop <- grep(paste0("^", prefix), handle$temp_tables,
                    value = TRUE)
  }
  for (tbl in to_drop) {
    .dropTempTable(handle, tbl)
  }
  if (!isTRUE(exact)) .cleanupHandleStaging(handle)
  invisible(TRUE)
}

# --- Aggregate methods ---

#' Ping / health check (Aggregate)
#'
#' @description
#' Returns basic status information indicating the dsOMOP server package is
#' loaded and responsive. Used by the client to verify connectivity before
#' issuing further commands.
#'
#' Calling it with no \code{omop_symbol} keeps only the Opal R session warm
#' (running any aggregate resets that session's inactivity timer). When an
#' \code{omop_symbol} is supplied, it ALSO issues a trivial \code{SELECT 1}
#' against that handle's database connection, keeping the server-side DB
#' connection (Rock R session -> OMOP database) warm too. This is a keepalive
#' for long-running interactive sessions, preventing BOTH connection layers from
#' timing out between commands.
#'
#' @param omop_symbol Character; optional handle symbol. When provided, the
#'   handle's DB connection is touched so it does not time out.
#' @return Named list with alive status, db_alive (logical or NA when no symbol
#'   was given), package version, and timestamp.
#' @examples
#' \dontrun{
#' omopPingDS()
#' omopPingDS("omop")
#' }
#' @export
omopPingDS <- function(omop_symbol = NULL) {
  db_alive <- NA
  if (!is.null(omop_symbol)) {
    db_alive <- tryCatch({
      handle <- .getHandle(omop_symbol)
      .executeQuery(handle, "SELECT 1 AS ping")
      TRUE
    }, error = function(e) FALSE)
  }
  list(
    alive = TRUE,
    db_alive = db_alive,
    version = as.character(utils::packageVersion("dsOMOP")),
    timestamp = Sys.time()
  )
}

#' Get capabilities snapshot (Aggregate)
#'
#' @description
#' Returns a snapshot of the server-side OMOP CDM schema, including available
#' tables, DBMS type, CDM version, a disclosure-banded population size (when
#' releasable), and a hash for cache invalidation. Used by the client to adapt
#' the UI to the server's data model.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Named list with schema summary and hash
#' @examples
#' \dontrun{
#' caps <- omopGetCapabilitiesDS("omop")
#' }
#' @export
omopGetCapabilitiesDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .getCapabilities(handle)
}

#' Report active disclosure thresholds (Aggregate)
#'
#' @description
#' Returns the disclosure-control thresholds currently in effect on this
#' server, resolved from the server's R options via the standard DataSHIELD
#' option chain (see \code{\link{.omopDisclosureSettings}}). This is a
#' \strong{read-only} introspection endpoint: it lets an analyst or admin see
#' the effective floor (most importantly \code{nfilter_subset}, the minimum
#' distinct-person count the per-patient gate enforces) without exposing any
#' way to change it. The settings can only be configured server-side; this
#' aggregate never mutates them.
#'
#' @return Named list of the active disclosure thresholds and server-gated
#'   permissions (the same structure as \code{\link{.omopDisclosureSettings}}).
#' @examples
#' \dontrun{
#' omopDisclosureSettingsDS()
#' }
#' @export
omopDisclosureSettingsDS <- function() {
  .omopDisclosureSettings()
}

#' List tables (Aggregate)
#'
#' @description
#' Returns metadata for all OMOP CDM tables present in the database, including
#' schema category, person ID availability, and concept column prefix.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with table metadata
#' @examples
#' \dontrun{
#' tables <- omopListTablesDS("omop")
#' }
#' @export
omopListTablesDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  bp <- .buildBlueprint(handle)
  present <- bp$tables[bp$tables$present_in_db, , drop = FALSE]
  present[, c("table_name", "schema_category", "has_person_id",
              "concept_prefix")]
}

#' List columns (Aggregate)
#'
#' @description
#' Returns column-level metadata for a specific OMOP CDM table, including
#' column names, data types, and whether each column is present in the database.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @return Data frame with column metadata
#' @examples
#' \dontrun{
#' cols <- omopListColumnsDS("omop", "person")
#' }
#' @export
omopListColumnsDS <- function(omop_symbol, table) {
  handle <- .getHandle(omop_symbol)
  table <- tolower(.validateIdentifier(table, "table"))
  bp <- .buildBlueprint(handle)

  if (is.null(bp$columns[[table]])) {
    stop("Table '", table, "' not found.", call. = FALSE)
  }
  bp$columns[[table]]
}

#' Get relationship graph (Aggregate)
#'
#' @description
#' Returns the join graph describing foreign-key relationships between OMOP CDM
#' tables. Used by the client to understand how tables can be linked for
#' multi-table queries.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with join relationships
#' @examples
#' \dontrun{
#' graph <- omopRelationshipGraphDS("omop")
#' }
#' @export
omopRelationshipGraphDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  bp <- .buildBlueprint(handle)
  bp$join_graph
}

#' Get table statistics (Aggregate)
#'
#' @description
#' Returns disclosure-controlled summary statistics for an OMOP CDM table,
#' such as total row count and distinct person count. Values below the
#' disclosure threshold are suppressed.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param stats Character vector; which stats to return
#' @return Named list with safe statistics
#' @examples
#' \dontrun{
#' stats <- omopTableStatsDS("omop", "condition_occurrence")
#' }
#' @export
omopTableStatsDS <- function(omop_symbol, table,
                             stats = c("rows", "persons")) {
  handle <- .getHandle(omop_symbol)
  stats <- .ds_arg(stats)
  if (is.list(stats)) stats <- unlist(stats)
  .profileTableStats(handle, table, stats)
}

#' Get column statistics (Aggregate)
#'
#' @description
#' Returns disclosure-controlled summary statistics for a single column in an
#' OMOP CDM table, including data type, completeness, distinct values, and
#' numeric summary when applicable.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param column Character; column name
#' @param concept_id Integer; optional; restrict the summary to rows of this
#'   concept, using the table's domain concept column
#' @param concept_col Character; optional; concept column to scope
#'   \code{concept_id} on instead of the domain concept (e.g.
#'   \code{unit_concept_id}, a \code{*_type_concept_id}, or
#'   \code{value_as_concept_id})
#' @param cohort Cohort scope: a cohort temp table name, a cohort_definition_id,
#'   or NULL. Takes precedence over \code{cohort_table}.
#' @param cohort_table Character; legacy cohort temp table for filtering (NULL)
#' @return Named list with column statistics
#' @examples
#' \dontrun{
#' stats <- omopColumnStatsDS("omop", "person", "year_of_birth")
#' }
#' @export
omopColumnStatsDS <- function(omop_symbol, table, column, concept_id = NULL,
                              concept_col = NULL, cohort = NULL,
                              cohort_table = NULL) {
  handle <- .getHandle(omop_symbol)
  concept_id <- .ds_arg(concept_id)
  concept_col <- .ds_arg(concept_col)
  if (!is.null(concept_id)) concept_id <- as.integer(unlist(concept_id))
  cohort_table <- .resolveCohortArg(handle, cohort, cohort_table)
  .profileColumnStats(handle, table, column, concept_id = concept_id,
                      concept_col = concept_col, cohort_table = cohort_table)
}

#' Get cross-table domain coverage (Aggregate)
#'
#' @description
#' Returns the number of distinct persons represented in each clinical domain
#' table (e.g., condition_occurrence, drug_exposure). Provides a quick overview
#' of data completeness across the CDM.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with per-table person counts
#' @examples
#' \dontrun{
#' coverage <- omopDomainCoverageDS("omop")
#' }
#' @export
omopDomainCoverageDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .profileDomainCoverage(handle)
}

#' Get missingness rates (Aggregate)
#'
#' @description
#' Computes the proportion of NULL values for each specified column (or all
#' columns) in an OMOP CDM table. Useful for data quality assessment before
#' running analyses.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param columns Character vector; columns to check
#' @param cohort Cohort scope: a cohort temp table name, a cohort_definition_id,
#'   or NULL. Takes precedence over \code{cohort_table}.
#' @param cohort_table Character; legacy cohort temp table for filtering (NULL)
#' @return Data frame with missingness rates
#' @examples
#' \dontrun{
#' missing <- omopMissingnessDS("omop", "condition_occurrence")
#' }
#' @export
omopMissingnessDS <- function(omop_symbol, table,
                              columns = NULL, cohort = NULL,
                              cohort_table = NULL) {
  handle <- .getHandle(omop_symbol)
  columns <- .ds_arg(columns)
  if (is.list(columns)) columns <- as.character(unlist(columns))
  cohort_table <- .resolveCohortArg(handle, cohort, cohort_table)
  .profileMissingness(handle, table, columns, cohort_table = cohort_table)
}

#' Get value counts (Aggregate)
#'
#' @description
#' Returns the frequency distribution of distinct values in a column, limited
#' to the top N most frequent values. Small counts are suppressed according to
#' the server's disclosure threshold.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param column Character; column name
#' @param top_n Integer; maximum number of distinct values to return
#' @param concept_id Integer; optional; restrict the summary to rows of this
#'   concept, using the table's domain concept column
#' @param concept_col Character; optional; concept column to scope
#'   \code{concept_id} on instead of the domain concept (e.g.
#'   \code{unit_concept_id}, a \code{*_type_concept_id}, or
#'   \code{value_as_concept_id})
#' @param cohort Cohort scope: a cohort temp table name, a cohort_definition_id,
#'   or NULL. Takes precedence over \code{cohort_table}.
#' @param cohort_table Character; legacy cohort temp table for filtering (NULL)
#' @return Data frame with value counts
#' @examples
#' \dontrun{
#' counts <- omopValueCountsDS("omop", "person", "gender_concept_id")
#' }
#' @export
omopValueCountsDS <- function(omop_symbol, table, column,
                              top_n = 20, concept_id = NULL,
                              concept_col = NULL, cohort = NULL,
                              cohort_table = NULL) {
  handle <- .getHandle(omop_symbol)
  concept_id <- .ds_arg(concept_id)
  concept_col <- .ds_arg(concept_col)
  if (!is.null(concept_id)) concept_id <- as.integer(unlist(concept_id))
  cohort_table <- .resolveCohortArg(handle, cohort, cohort_table)
  # Small-count suppression is mandatory for this aggregate endpoint and is not
  # client-configurable: a caller must never be able to disable disclosure control.
  .profileValueCounts(handle, table, column, top_n, suppress_small = TRUE,
                      concept_id = concept_id, concept_col = concept_col,
                      cohort_table = cohort_table)
}

#' Search concepts (Aggregate)
#'
#' @description
#' Searches the OMOP vocabulary tables for concepts matching a text pattern
#' and/or an exact concept-ID list, optionally filtered by domain, vocabulary,
#' standard status, and validity. Returns concept metadata including concept ID,
#' name, domain, vocabulary, code, validity dates, and standard status. This is
#' a reference-data (vocabulary) reader and is not disclosure-gated.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param pattern Character; search pattern (case-insensitive substring match);
#'   NULL or "" to search by \code{concept_id} only
#' @param domain Character; domain filter (e.g., "Condition", "Drug")
#' @param vocabulary Character; vocabulary filter (e.g., "SNOMED", "RxNorm")
#' @param standard_only Logical; only standard concepts (ignored when
#'   \code{standard} is supplied)
#' @param limit Integer; max results
#' @param concept_id Numeric vector; restrict to these exact concept IDs
#' @param standard Character; explicit \code{standard_concept} value (e.g. "S")
#' @param valid Logical; TRUE keeps only currently-valid concepts, FALSE only
#'   invalidated ones
#' @return Data frame with concept results
#' @examples
#' \dontrun{
#' concepts <- omopSearchConceptsDS("omop", "diabetes", domain = "Condition")
#' }
#' @export
omopSearchConceptsDS <- function(omop_symbol, pattern = NULL,
                                 domain = NULL,
                                 vocabulary = NULL,
                                 standard_only = TRUE,
                                 limit = 50,
                                 concept_id = NULL,
                                 standard = NULL,
                                 valid = NULL) {
  handle <- .getHandle(omop_symbol)
  pattern <- .ds_arg(pattern)
  concept_id <- .ds_arg(concept_id)
  if (is.list(concept_id)) concept_id <- as.integer(unlist(concept_id))
  .vocabSearchConcepts(handle, pattern, domain,
                       vocabulary, standard_only, limit,
                       concept_id = concept_id, standard = standard,
                       valid = valid)
}

#' Lookup concepts by ID (Aggregate)
#'
#' @description
#' Retrieves full concept metadata for one or more concept IDs from the OMOP
#' vocabulary tables. Returns concept name, domain, vocabulary, class, and
#' standard concept flag.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param concept_ids Numeric vector; one or more concept IDs to look up
#' @return Data frame with concept details
#' @examples
#' \dontrun{
#' details <- omopLookupConceptsDS("omop", c(201826, 4329847))
#' }
#' @export
omopLookupConceptsDS <- function(omop_symbol, concept_ids) {
  handle <- .getHandle(omop_symbol)
  concept_ids <- .ds_arg(concept_ids)
  if (is.list(concept_ids)) concept_ids <- as.integer(unlist(concept_ids))
  .vocabLookupConcepts(handle, concept_ids)
}

#' Get descendant concepts (Aggregate)
#'
#' @description
#' Traverses the OMOP concept_ancestor table to find all descendant concepts
#' of one or more ancestor concept IDs. Optionally includes the ancestor
#' concepts themselves in the result.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param ancestor_ids Numeric vector; ancestor concept IDs
#' @param include_self Logical; whether to include the ancestor IDs in results
#' @return Data frame with descendant concepts
#' @examples
#' \dontrun{
#' descendants <- omopGetDescendantsDS("omop", c(201826), include_self = TRUE)
#' }
#' @export
omopGetDescendantsDS <- function(omop_symbol, ancestor_ids,
                                 include_self = TRUE) {
  handle <- .getHandle(omop_symbol)
  ancestor_ids <- .ds_arg(ancestor_ids)
  if (is.list(ancestor_ids)) ancestor_ids <- as.integer(unlist(ancestor_ids))
  .vocabGetDescendants(handle, ancestor_ids, include_self)
}

#' Expand a concept set (Aggregate)
#'
#' @description
#' Expands a concept set specification into a flat vector of concept IDs by
#' applying inclusion/exclusion rules and descendant traversal. Mirrors the
#' ATLAS concept set expression logic.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param concept_set List; concept set specification with inclusion rules
#' @return Integer vector of expanded concept IDs
#' @examples
#' \dontrun{
#' ids <- omopExpandConceptSetDS("omop", concept_set)
#' }
#' @export
omopExpandConceptSetDS <- function(omop_symbol, concept_set) {
  handle <- .getHandle(omop_symbol)
  concept_set <- .ds_arg(concept_set)
  .vocabExpandConceptSet(handle, concept_set)
}

#' Get concept ancestors and descendants (Aggregate)
#'
#' @description
#' Returns both the ancestors and the descendants of one or more concept IDs
#' from the \code{concept_ancestor} table, in a single frame tagged with a
#' \code{direction} column. This is the hierarchy ("relationships" tree) view
#' from Athena/ATLAS. Reference-data reader; not disclosure-gated.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param concept_ids Numeric vector; concept IDs to expand the hierarchy for
#' @return Data frame with \code{direction}, concept columns, and levels of
#'   separation
#' @examples
#' \dontrun{
#' tree <- omopConceptAncestorsDS("omop", c(201826))
#' }
#' @export
omopConceptAncestorsDS <- function(omop_symbol, concept_ids) {
  handle <- .getHandle(omop_symbol)
  concept_ids <- .ds_arg(concept_ids)
  if (is.list(concept_ids)) concept_ids <- as.integer(unlist(concept_ids))
  .vocabConceptAncestors(handle, concept_ids)
}

#' Get concept synonyms (Aggregate)
#'
#' @description
#' Returns the synonyms (alternative names) for one or more concept IDs from the
#' \code{concept_synonym} table, mirroring the Athena concept "synonyms" panel.
#' Reference-data reader; not disclosure-gated.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param concept_ids Numeric vector; concept IDs to fetch synonyms for
#' @return Data frame with \code{concept_id} and \code{concept_synonym_name}
#' @examples
#' \dontrun{
#' syns <- omopConceptSynonymsDS("omop", c(201826))
#' }
#' @export
omopConceptSynonymsDS <- function(omop_symbol, concept_ids) {
  handle <- .getHandle(omop_symbol)
  concept_ids <- .ds_arg(concept_ids)
  if (is.list(concept_ids)) concept_ids <- as.integer(unlist(concept_ids))
  .vocabGetSynonyms(handle, concept_ids)
}

#' Get concept relationships (Aggregate)
#'
#' @description
#' Returns every \code{concept_relationship} edge touching the given concept IDs
#' in \strong{both} directions (not just "Maps to"), with the related concept's
#' name joined in and a \code{direction} column. An optional
#' \code{relationship_id} narrows to a single relationship type. Reference-data
#' reader; not disclosure-gated.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param concept_ids Numeric vector; concept IDs to fetch relationships for
#' @param relationship_id Character; optional single relationship_id filter
#'   (e.g. "Maps to", "Is a", "Subsumes")
#' @return Data frame with the relationship rows and related concept names
#' @examples
#' \dontrun{
#' rels <- omopConceptRelationshipsDS("omop", c(201826), "Maps to")
#' }
#' @export
omopConceptRelationshipsDS <- function(omop_symbol, concept_ids,
                                       relationship_id = NULL) {
  handle <- .getHandle(omop_symbol)
  concept_ids <- .ds_arg(concept_ids)
  if (is.list(concept_ids)) concept_ids <- as.integer(unlist(concept_ids))
  relationship_id <- .ds_arg(relationship_id)
  .vocabGetRelationships(handle, concept_ids, relationship_id)
}

#' List concepts with pagination (Aggregate)
#'
#' @description
#' Browses the \code{concept} catalog filtered by domain, vocabulary, concept
#' class, standard status, and validity, paged with OFFSET/LIMIT. Lifts the
#' 500-row cap of \code{\link{omopSearchConceptsDS}} for catalog browsing,
#' mirroring Athena's paged concept list. Returns the current page plus the
#' total matching count. Reference-data reader; not disclosure-gated.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param domain Character; filter by domain_id
#' @param vocabulary Character; filter by vocabulary_id
#' @param concept_class Character; filter by concept_class_id
#' @param standard Character; filter by standard_concept value (e.g. "S")
#' @param valid Logical; TRUE keeps only currently-valid concepts
#' @param offset Integer; rows to skip (page start)
#' @param limit Integer; page size (capped at 1000)
#' @param order Character; column to order by (default "concept_id")
#' @return List with \code{rows} (data frame), \code{total_count},
#'   \code{offset}, and \code{limit}
#' @examples
#' \dontrun{
#' page <- omopListConceptsDS("omop", vocabulary = "SNOMED", limit = 100)
#' }
#' @export
omopListConceptsDS <- function(omop_symbol, domain = NULL, vocabulary = NULL,
                               concept_class = NULL, standard = NULL,
                               valid = NULL, offset = 0L, limit = 100L,
                               order = "concept_id") {
  handle <- .getHandle(omop_symbol)
  domain <- .ds_arg(domain)
  vocabulary <- .ds_arg(vocabulary)
  concept_class <- .ds_arg(concept_class)
  standard <- .ds_arg(standard)
  order <- .ds_arg(order)
  .vocabListConcepts(handle, domain = domain, vocabulary = vocabulary,
                     concept_class = concept_class, standard = standard,
                     valid = valid, offset = offset, limit = limit,
                     order = order %||% "concept_id")
}

#' List vocabularies (Aggregate)
#'
#' @description
#' Returns the distinct vocabularies from the \code{vocabulary} table (falling
#' back to distinct \code{vocabulary_id} values on \code{concept} if that table
#' is not loaded). Reference-data reader; not disclosure-gated.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame of vocabularies
#' @examples
#' \dontrun{
#' vocabs <- omopVocabulariesDS("omop")
#' }
#' @export
omopVocabulariesDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .vocabDistinctMeta(handle, "vocabulary", "vocabulary_id")
}

#' List domains (Aggregate)
#'
#' @description
#' Returns the distinct domains from the \code{domain} table (falling back to
#' distinct \code{domain_id} values on \code{concept} if that table is not
#' loaded). Reference-data reader; not disclosure-gated.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame of domains
#' @examples
#' \dontrun{
#' domains <- omopDomainsDS("omop")
#' }
#' @export
omopDomainsDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .vocabDistinctMeta(handle, "domain", "domain_id")
}

#' List concept classes (Aggregate)
#'
#' @description
#' Returns the distinct concept classes from the \code{concept_class} table
#' (falling back to distinct \code{concept_class_id} values on \code{concept} if
#' that table is not loaded). Reference-data reader; not disclosure-gated.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame of concept classes
#' @examples
#' \dontrun{
#' classes <- omopConceptClassesDS("omop")
#' }
#' @export
omopConceptClassesDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .vocabDistinctMeta(handle, "concept_class", "concept_class_id")
}

#' Get the cdm_source row(s) (Aggregate)
#'
#' @description
#' Returns the full \code{cdm_source} table row(s), which describe the data
#' source (name, abbreviation, holder, release/version dates, etc.). Metadata
#' reader; not disclosure-gated.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame of cdm_source rows (empty if the table is absent)
#' @examples
#' \dontrun{
#' src <- omopCdmSourceDS("omop")
#' }
#' @export
omopCdmSourceDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .vocabCdmSource(handle)
}

#' Get the CDM version (Aggregate)
#'
#' @description
#' Returns the CDM version, preferring \code{cdm_source.cdm_version} and falling
#' back to the version inferred from the table structure. Metadata reader; not
#' disclosure-gated.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return List with \code{cdm_version}, \code{source}, and
#'   \code{vocabulary_version}
#' @examples
#' \dontrun{
#' ver <- omopCdmVersionDS("omop")
#' }
#' @export
omopCdmVersionDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .vocabCdmVersion(handle)
}

#' Preview a plan (Aggregate)
#'
#' @description
#' Validates and projects the resolvable parts of an extraction plan without
#' materializing cohorts or executing filters. Reported columns are source or
#' feature projections and are final only when \code{columns_complete} is true.
#' Any reported person count is a disclosure-banded source-table count for an
#' unrestricted output, not an estimated row count for the executed result.
#' Cohort-dependent outputs with no executable cohort declaration are rejected.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param plan List; the extraction plan to preview
#' @return List with validation and per-output projection metadata, including
#'   completeness/unavailability fields for columns and person counts.
#' @examples
#' \dontrun{
#' preview <- omopPlanPreviewDS("omop", plan)
#' }
#' @export
omopPlanPreviewDS <- function(omop_symbol, plan) {
  handle <- .getHandle(omop_symbol)
  plan <- .ds_arg(plan)
  # Preview is the cheapest, most-repeated data-touching op and is the primary
  # differencing signal, so the data controller must be able to see the
  # sequence of preview calls (banding alone cannot stop iterative probing).
  outputs <- plan$outputs %||% list()
  preview_tables <- unique(unlist(lapply(outputs, function(o) {
    # event_level outputs carry a scalar $table; person_level outputs carry a
    # named $tables list. Use [[ exact-match to avoid $ partial matching
    # ($table matching $tables and leaking column names into the audit detail).
    c(o[["table"]], names(o[["tables"]]))
  }), use.names = FALSE))
  .omopAuditLog("omopPlanPreviewDS",
                list(n_outputs = length(outputs), tables = preview_tables))
  .planPreview(handle, plan)
}

#' List cohort definitions (Aggregate)
#'
#' @description
#' Returns metadata for all cohort definitions available in the results schema,
#' including cohort definition IDs, names, and subject counts.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with cohort definitions
#' @examples
#' \dontrun{
#' cohorts <- omopCohortListDS("omop")
#' }
#' @export
omopCohortListDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .cohortList(handle)
}

#' Get a cohort definition (Aggregate)
#'
#' @description
#' Retrieves the full definition for a specific cohort, including the cohort
#' specification DSL, name, description, and subject count.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param cohort_definition_id Integer; the cohort definition ID to retrieve
#' @return Named list with definition details
#' @examples
#' \dontrun{
#' defn <- omopCohortGetDefinitionDS("omop", cohort_definition_id = 1L)
#' }
#' @export
omopCohortGetDefinitionDS <- function(omop_symbol,
                                      cohort_definition_id) {
  handle <- .getHandle(omop_symbol)
  .cohortGetDefinition(handle, cohort_definition_id)
}

# --- Exploration aggregate methods ---

#' Get concept prevalence (Aggregate)
#'
#' @description
#' Returns the top concepts in a table ranked by person count or record count,
#' with disclosure control applied. Concepts with counts below the server
#' threshold are suppressed.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name (ignored when \code{global = TRUE})
#' @param concept_col Character; concept column name (NULL = auto-detect)
#' @param metric Character; "persons" or "records"
#' @param top_n Integer; page size (number of top concepts to return)
#' @param cohort_table Character; cohort temp table for filtering (NULL)
#' @param window List with start/end dates for filtering (NULL)
#' @param offset Integer; concepts to skip (pagination) so prevalence is not
#'   hard-capped at the legacy top_n=500 — page through with offset = 0, 500, ...
#' @param global Logical; rank concepts across ALL clinical tables, person-gated
#'   per table and suppressed over the merged set (NULL/FALSE = single table)
#' @param cohort Cohort scope: a cohort temp table name, a cohort_definition_id,
#'   or NULL. Takes precedence over \code{cohort_table}; resolved + re-gated
#'   server-side via \code{.resolveCohortTable}.
#' @return Data frame with concept_id, concept_name, n_persons, n_records (plus
#'   source_table when \code{global = TRUE})
#' @examples
#' \dontrun{
#' prevalence <- omopConceptPrevalenceDS("omop", "condition_occurrence")
#' }
#' @export
omopConceptPrevalenceDS <- function(omop_symbol, table = NULL, concept_col = NULL,
                                     metric = "persons", top_n = 50,
                                     cohort_table = NULL, window = NULL,
                                     offset = 0L, global = FALSE,
                                     cohort = NULL) {
  handle <- .getHandle(omop_symbol)
  cohort_table <- .resolveCohortArg(handle, cohort, cohort_table)
  offset <- as.integer(.ds_arg(offset) %||% 0L)
  global <- isTRUE(.ds_arg(global))
  .profileConceptPrevalence(handle, table, concept_col, metric, top_n,
                            cohort_table, window, offset = offset,
                            global = global)
}

#' Get a disclosure-safe 2-way cross-tabulation (Aggregate)
#'
#' @description
#' Cross-tabulates two categorical columns of an OMOP table, counting distinct
#' persons (default) or records, and returns a contingency table protected by
#' primary small-cell suppression PLUS iterative complementary suppression to a
#' fixpoint. Exact margins are NEVER returned (omitted by default, or banded via
#' \code{band_margins = TRUE}). Both axes must pass the dimension gate and the
#' scoped population must pass the minimum-persons gate.
#'
#' Cross-tab is descriptive only. For true multivariable association (>= 3
#' interacting variables or continuous adjustment), route to \code{ds.glm}
#' rather than building a high-dimensional cell table. When \code{stratify_by}
#' is supplied, a NAMED LIST of INDEPENDENT protected 2-way tables is returned
#' (one per stratum level); the unstratified total is never returned and each
#' slice is independently gated and suppressed.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param row_col Character; row categorical column
#' @param col_col Character; column categorical column
#' @param count_mode Character; "persons" (distinct person_id) or "records"
#' @param row_concept_ids Integer vector; optional; restrict row axis levels
#' @param col_concept_ids Integer vector; optional; restrict column axis levels
#' @param cohort_table Character; cohort temp table to scope the population (NULL)
#' @param stratify_by Character; optional 3rd categorical column for stratified
#'   (chained 2-way) tables
#' @param band_margins Logical; when TRUE, attach banded (never exact) margins
#' @param cohort Cohort scope: a cohort temp table name, a cohort_definition_id,
#'   or NULL. Takes precedence over \code{cohort_table}.
#' @return For a plain call: a named list with the NA-masked \code{counts}
#'   matrix, axis levels, and a \code{suppressed} flag. For a stratified call:
#'   a named list of independent protected per-stratum tables.
#' @examples
#' \dontrun{
#' ct <- omopCrossTabDS("omop", "person", "gender_concept_id", "race_concept_id")
#' }
#' @export
omopCrossTabDS <- function(omop_symbol, table, row_col, col_col,
                           count_mode = "persons",
                           row_concept_ids = NULL, col_concept_ids = NULL,
                           cohort_table = NULL, stratify_by = NULL,
                           band_margins = FALSE, cohort = NULL) {
  handle <- .getHandle(omop_symbol)
  cohort_table <- .resolveCohortArg(handle, cohort, cohort_table)
  count_mode <- .ds_arg(count_mode)
  if (!is.character(count_mode) || length(count_mode) != 1L) {
    count_mode <- "persons"
  }
  row_concept_ids <- .ds_arg(row_concept_ids)
  col_concept_ids <- .ds_arg(col_concept_ids)
  if (!is.null(row_concept_ids)) {
    row_concept_ids <- as.integer(unlist(row_concept_ids))
  }
  if (!is.null(col_concept_ids)) {
    col_concept_ids <- as.integer(unlist(col_concept_ids))
  }
  stratify_by <- .ds_arg(stratify_by)
  if (!is.null(stratify_by)) stratify_by <- as.character(unlist(stratify_by))[1]
  band_margins <- isTRUE(.ds_arg(band_margins))

  # Audit-log the call sequence (cross-query differencing defence). Never logs
  # cell values — only the call shape, which the data controller reviews.
  .omopAuditLog("omopCrossTabDS",
                list(table = table, row_col = row_col, col_col = col_col,
                     count_mode = count_mode, stratify_by = stratify_by,
                     band_margins = band_margins))

  .profileCrossTab(handle, table, row_col, col_col,
                   count_mode = count_mode,
                   row_concept_ids = row_concept_ids,
                   col_concept_ids = col_concept_ids,
                   cohort_table = cohort_table,
                   stratify_by = stratify_by,
                   band_margins = band_margins)
}

#' Get numeric range (p05/p95) for two-pass histogram pooling (Aggregate)
#'
#' @description
#' Returns the 5th and 95th percentile approximations and total count for a
#' numeric column. Used as pass 1 of two-pass histogram pooling to compute
#' shared bin edges across federated sites.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param cohort_table Character; cohort temp table for filtering (NULL)
#' @param window List with start/end dates for filtering (NULL)
#' @param concept_id Integer; optional; restrict the range to rows of this
#'   concept, using the table's domain concept column
#' @param concept_col Character; optional; concept column to scope
#'   \code{concept_id} on instead of the domain concept (e.g.
#'   \code{unit_concept_id}, a \code{*_type_concept_id}, or
#'   \code{value_as_concept_id})
#' @param cohort Cohort scope: a cohort temp table name, a cohort_definition_id,
#'   or NULL. Takes precedence over \code{cohort_table}.
#' @return List with p05, p95, n_total
#' @examples
#' \dontrun{
#' range_info <- omopNumericRangeDS("omop", "measurement", "value_as_number")
#' }
#' @export
omopNumericRangeDS <- function(omop_symbol, table, value_col,
                                cohort_table = NULL, window = NULL,
                                concept_id = NULL, concept_col = NULL,
                                cohort = NULL) {
  handle <- .getHandle(omop_symbol)
  concept_id <- .ds_arg(concept_id)
  concept_col <- .ds_arg(concept_col)
  if (!is.null(concept_id)) concept_id <- as.integer(unlist(concept_id))
  cohort_table <- .resolveCohortArg(handle, cohort, cohort_table)
  .profileNumericRange(handle, table, value_col, cohort_table, window,
                       concept_id = concept_id, concept_col = concept_col)
}

#' Get numeric histogram (Aggregate)
#'
#' @description
#' Computes a disclosure-controlled histogram for a numeric column. Bins with
#' counts below the server threshold are suppressed. Supports shared bin edges
#' from two-pass pooling for consistent cross-site comparisons.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param bins Integer; number of bins
#' @param cohort_table Character; cohort temp table for filtering (NULL)
#' @param window List with start/end dates for filtering (NULL)
#' @param breaks Numeric vector; shared bin edges from two-pass pooling (NULL)
#' @param concept_id Integer; optional; restrict the histogram to rows of this
#'   concept, using the table's domain concept column
#' @param concept_col Character; optional; concept column to scope
#'   \code{concept_id} on instead of the domain concept (e.g.
#'   \code{unit_concept_id}, a \code{*_type_concept_id}, or
#'   \code{value_as_concept_id})
#' @param cohort Cohort scope: a cohort temp table name, a cohort_definition_id,
#'   or NULL. Takes precedence over \code{cohort_table}.
#' @return Data frame with bin_start, bin_end, count, suppressed
#' @examples
#' \dontrun{
#' hist_data <- omopNumericHistogramDS("omop", "measurement", "value_as_number")
#' }
#' @export
omopNumericHistogramDS <- function(omop_symbol, table, value_col,
                                    bins = 20L, cohort_table = NULL,
                                    window = NULL, breaks = NULL,
                                    concept_id = NULL, concept_col = NULL,
                                    cohort = NULL) {
  handle <- .getHandle(omop_symbol)
  breaks <- .ds_arg(breaks)
  concept_id <- .ds_arg(concept_id)
  concept_col <- .ds_arg(concept_col)
  if (!is.null(concept_id)) concept_id <- as.integer(unlist(concept_id))
  cohort_table <- .resolveCohortArg(handle, cohort, cohort_table)
  .profileNumericHistogram(handle, table, value_col, bins,
                           cohort_table, window, breaks,
                           concept_id = concept_id, concept_col = concept_col)
}

#' Get numeric quantiles (Aggregate)
#'
#' @description
#' Computes quantiles at specified probabilities using SQL-based approximation.
#' Results are rounded to the specified number of decimal places to limit
#' precision and reduce re-identification risk.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param value_col Character; numeric column name
#' @param probs Numeric vector; probabilities (e.g., c(0.05, 0.25, 0.5, 0.75, 0.95))
#' @param cohort_table Character; cohort temp table for filtering (NULL)
#' @param window List with start/end dates for filtering (NULL)
#' @param rounding Integer; decimal places for rounding
#' @param concept_id Integer; optional; restrict the summary to rows of this
#'   concept, using the table's domain concept column
#' @param concept_col Character; optional; concept column to scope
#'   \code{concept_id} on instead of the domain concept (e.g.
#'   \code{unit_concept_id}, a \code{*_type_concept_id}, or
#'   \code{value_as_concept_id})
#' @param cohort Cohort scope: a cohort temp table name, a cohort_definition_id,
#'   or NULL. Takes precedence over \code{cohort_table}.
#' @return Data frame with probability and value
#' @examples
#' \dontrun{
#' quantiles <- omopNumericQuantilesDS("omop", "measurement", "value_as_number")
#' }
#' @export
omopNumericQuantilesDS <- function(omop_symbol, table, value_col,
                                    probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                    cohort_table = NULL, window = NULL,
                                    rounding = 2L, concept_id = NULL,
                                    concept_col = NULL, cohort = NULL) {
  handle <- .getHandle(omop_symbol)
  probs <- .ds_arg(probs)
  concept_id <- .ds_arg(concept_id)
  concept_col <- .ds_arg(concept_col)
  if (!is.null(concept_id)) concept_id <- as.integer(unlist(concept_id))
  cohort_table <- .resolveCohortArg(handle, cohort, cohort_table)
  .profileNumericQuantiles(handle, table, value_col, probs,
                           cohort_table, window, rounding,
                           concept_id = concept_id, concept_col = concept_col)
}

#' Get date counts (Aggregate)
#'
#' @description
#' Counts records by time bin (year, quarter, or month) with disclosure control
#' applied. Time periods with counts below the threshold are suppressed. Useful
#' for visualizing temporal trends in clinical data.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param date_col Character; date column (NULL = auto-detect)
#' @param granularity Character; "year", "quarter", or "month"
#' @param cohort_table Character; cohort temp table for filtering (NULL)
#' @param window List with start/end dates for filtering (NULL)
#' @param cohort Cohort scope: a cohort temp table name, a cohort_definition_id,
#'   or NULL. Takes precedence over \code{cohort_table}.
#' @return Data frame with period, n_records, suppressed
#' @examples
#' \dontrun{
#' trends <- omopDateCountsDS("omop", "condition_occurrence", granularity = "year")
#' }
#' @export
omopDateCountsDS <- function(omop_symbol, table, date_col = NULL,
                              granularity = "year", cohort_table = NULL,
                              window = NULL, cohort = NULL) {
  handle <- .getHandle(omop_symbol)
  cohort_table <- .resolveCohortArg(handle, cohort, cohort_table)
  .profileDateCounts(handle, table, date_col, granularity,
                     cohort_table, window)
}

#' Get concept drilldown (Aggregate)
#'
#' @description
#' Returns a full drilldown profile for a single concept within a table,
#' including summary stats, numeric distribution, categorical values,
#' date coverage, and missingness. All count-based outputs are subject to
#' disclosure control.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param concept_id Integer; concept ID to drill into
#' @param concept_col Character; concept column name to drill into, or NULL
#'   for automatic detection based on the table's primary concept column.
#' @return Named list with drilldown results
#' @examples
#' \dontrun{
#' drilldown <- omopConceptDrilldownDS("omop", "condition_occurrence", 201826L)
#' }
#' @export
omopConceptDrilldownDS <- function(omop_symbol, table, concept_id,
                                   concept_col = NULL) {
  handle <- .getHandle(omop_symbol)
  .profileConceptDrilldown(handle, table, as.integer(concept_id),
                           concept_col = concept_col)
}

#' Locate concept across tables (Aggregate)
#'
#' @description
#' Searches all CDM tables with concept columns and returns a presence matrix
#' showing where the given concept IDs appear. Useful for understanding which
#' clinical domains contain data for a concept of interest.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param concept_ids Integer vector; concept IDs to locate
#' @return Data frame with table_name, concept_column, concept_id, n_records,
#'   n_persons
#' @examples
#' \dontrun{
#' locations <- omopLocateConceptDS("omop", c(201826L, 4329847L))
#' }
#' @export
omopLocateConceptDS <- function(omop_symbol, concept_ids) {
  handle <- .getHandle(omop_symbol)
  concept_ids <- .ds_arg(concept_ids)
  if (is.list(concept_ids)) concept_ids <- as.integer(unlist(concept_ids))
  .profileLocateConcept(handle, as.integer(concept_ids))
}

#' Get safe numeric cutpoints (Aggregate)
#'
#' @description
#' Returns a data-independent public grid configured by the data controller.
#' The complete grid is released only when every bin contains enough distinct
#' persons after a one-value-per-person reduction. Values outside the declared
#' range are winsorized only for that internal support check. Counts are banded;
#' this mechanism does not claim differential privacy.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param column Character; numeric column name
#' @param concept_id Integer or NULL; concept filter
#' @param n_bins Integer; exact number of bins in the configured public grid
#' @param concept_col Character; optional; concept column to scope
#'   \code{concept_id} on instead of the domain concept (e.g.
#'   \code{unit_concept_id}, a \code{*_type_concept_id}, or
#'   \code{value_as_concept_id})
#' @return List with public breaks, banded counts, a resource-session contract,
#'   and public clipping/grid metadata
#' @examples
#' \dontrun{
#' cuts <- omopSafeCutpointsDS("omop", "measurement", "value_as_number")
#' }
#' @export
omopSafeCutpointsDS <- function(omop_symbol, table, column,
                                 concept_id = NULL, n_bins = 10L,
                                 concept_col = NULL) {
  handle <- .getHandle(omop_symbol)
  concept_col <- .ds_arg(concept_col)
  .profileSafeCutpoints(handle, table, column, concept_id, as.integer(n_bins),
                        concept_col = concept_col)
}

# --- Achilles aggregate methods ---

#' Check Achilles availability (Aggregate)
#'
#' @description
#' Checks whether pre-computed Achilles results are available in the results
#' schema. Returns the availability status and the number of analyses found.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Named list with availability status
#' @examples
#' \dontrun{
#' status <- omopAchillesStatusDS("omop")
#' }
#' @export
omopAchillesStatusDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .achillesStatus(handle)
}

#' List Achilles analyses (Aggregate)
#'
#' @description
#' Returns the catalog of available Achilles analyses, optionally filtered by
#' clinical domain. Each entry includes analysis ID, name, and description.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param domain Character; optional domain filter (e.g., "Person", "Condition")
#' @return Data frame with analysis catalog
#' @examples
#' \dontrun{
#' analyses <- omopAchillesAnalysesDS("omop", domain = "Person")
#' }
#' @export
omopAchillesAnalysesDS <- function(omop_symbol, domain = NULL) {
  handle <- .getHandle(omop_symbol)
  .achillesListAnalyses(handle, domain)
}

#' Get Achilles count results (Aggregate)
#'
#' @description
#' Returns count-based Achilles results for the given analysis IDs with
#' server-controlled disclosure thresholds. Arbitrary stratum filtering and
#' client-supplied min_cell_count are intentionally not supported to prevent
#' probing attacks and threshold weakening.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param analysis_ids Integer vector; analysis IDs to retrieve
#' @return Data frame with analysis results
#' @examples
#' \dontrun{
#' results <- omopAchillesResultsDS("omop", c(1L, 2L, 3L))
#' }
#' @export
omopAchillesResultsDS <- function(omop_symbol, analysis_ids) {
  handle <- .getHandle(omop_symbol)
  analysis_ids <- .ds_arg(analysis_ids)
  if (is.list(analysis_ids)) analysis_ids <- as.integer(unlist(analysis_ids))
  .achillesGetResults(handle, analysis_ids)
}

#' Get Achilles distribution results (Aggregate)
#'
#' @description
#' Returns distribution statistics (average, standard deviation, median,
#' percentiles) for the given Achilles analysis IDs. Extreme values (min/max)
#' are never returned to prevent identification of outlier individuals.
#' Arbitrary stratum filtering is not supported.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param analysis_ids Integer vector; analysis IDs to retrieve
#' @return Data frame with distribution statistics (no min/max)
#' @examples
#' \dontrun{
#' dists <- omopAchillesDistributionDS("omop", c(103L, 105L))
#' }
#' @export
omopAchillesDistributionDS <- function(omop_symbol, analysis_ids) {
  handle <- .getHandle(omop_symbol)
  analysis_ids <- .ds_arg(analysis_ids)
  if (is.list(analysis_ids)) analysis_ids <- as.integer(unlist(analysis_ids))
  .achillesGetDistributions(handle, analysis_ids)
}

#' Get Achilles analysis catalog (Aggregate)
#'
#' @description
#' Returns the full catalog of available Achilles analyses, either from the
#' achilles_analysis table or dynamically discovered from the results tables.
#' Includes analysis ID, name, description, and result type for each entry.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with analysis catalog
#' @examples
#' \dontrun{
#' catalog <- omopAchillesCatalogDS("omop")
#' }
#' @export
omopAchillesCatalogDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .achillesDiscoverCatalog(handle)
}

#' Get Achilles Heel data-quality warnings (controller-only helper)
#'
#' @description
#' Achilles Heel counts records rather than distinct people and does not provide
#' enough information to enforce a person-level contribution contract. This
#' helper remains available to a data controller inside the server process, but
#' it is deliberately not exported or registered as a DataSHIELD aggregate.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with analysis_id, achilles_heel_warning, rule_id,
#'   record_count
#' @examples
#' \dontrun{
#' heel <- omopAchillesHeelDS("omop")
#' }
#' @keywords internal
omopAchillesHeelDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .achillesGetHeelResults(handle)
}

# --- OHDSI Results aggregate methods ---

#' Check OHDSI result tool availability (Aggregate)
#'
#' @description
#' Scans the database for pre-computed result tables from OHDSI tools
#' (CohortDiagnostics, CohortIncidence, Characterization, and others) and
#' returns per-tool availability status.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Named list with per-tool availability
#' @examples
#' \dontrun{
#' status <- omopOhdsiStatusDS("omop")
#' }
#' @export
omopOhdsiStatusDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  .ohdsiStatus(handle)
}

#' List discovered OHDSI result tables (Aggregate)
#'
#' @description
#' Returns a data frame of all OHDSI result tables found in the database,
#' including tool identification, qualified names, and row counts.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with table_name, tool_id, tool_name, qualified_name, n_rows
#' @examples
#' \dontrun{
#' tables <- omopOhdsiTablesDS("omop")
#' }
#' @export
omopOhdsiTablesDS <- function(omop_symbol) {
  handle <- .getHandle(omop_symbol)
  result <- .ohdsiFilterPublicInventory(.ohdsiFindResultTables(handle))
  if (nrow(result) > 0L) {
    result$n_rows <- .ohdsiBandInventoryCounts(result$n_rows)
    # Physical schema names are server internals and are unnecessary for
    # discovering the public analysis surface.
    result$qualified_name <- NULL
  }
  result
}

#' Query an OHDSI result table (Aggregate)
#'
#' @description
#' Reads only columns covered by a reviewed per-table release contract and
#' applies server-controlled person and small-cell thresholds. Sensitive or
#' uncontracted columns fail closed in strict mode. If an administrator disables
#' strict mode, the compatibility result is explicitly marked as a development
#' output and must not be treated as disclosure-safe.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table_name Character; which result table to query
#' @param columns Character vector; columns to select (NULL = all safe columns)
#' @param filters Named list; WHERE conditions
#' @param order_by Character; ORDER BY column
#' @param limit Integer; max rows (capped at 5000)
#' @param tool_id Character; optional tool identifier for registry lookup
#' @return Data frame with disclosure control applied
#' @examples
#' \dontrun{
#' results <- omopOhdsiResultsDS("omop", "cohort_count")
#' }
#' @export
omopOhdsiResultsDS <- function(omop_symbol, table_name, columns = NULL,
                                filters = NULL, order_by = NULL,
                                limit = 5000L, tool_id = NULL) {
  handle <- .getHandle(omop_symbol)
  filters <- .ds_arg(filters)
  order_by <- .ds_arg(order_by)
  # The internal consumer adds contracted disclosure-basis columns to the SQL
  # projection and removes them only after gating, so caller projection cannot
  # bypass the person gate.
  result <- .ohdsiGetResults(handle, table_name, columns = columns, filters,
                             order_by, limit, tool_id)
  result
}

#' Get the pooling contract for a physical OHDSI result table (Aggregate)
#'
#' @description
#' Resolves the exact physical result table inside the server-authorised OHDSI
#' results namespace and returns its reviewed, typed pooling contract. The
#' contract is inert metadata with a closed output schema; it contains no SQL,
#' database-qualified names, credentials, or raw result values. Unreviewed or
#' malformed physical tables fail closed.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table_name Character; physical OHDSI result-table name
#' @param tool_id Character; optional tool identifier for registry lookup
#' @return Named list containing contract version, canonical tool and table
#'   identifiers, and the typed pooling contract
#' @examples
#' \dontrun{
#' contract <- omopOhdsiResultContractDS("omop", "cohort_count")
#' }
#' @export
omopOhdsiResultContractDS <- function(omop_symbol, table_name,
                                      tool_id = NULL) {
  handle <- .getHandle(omop_symbol)
  .ohdsiResultPoolingContract(handle, table_name, tool_id)
}

#' Get OHDSI tool summary (Aggregate)
#'
#' @description
#' Returns a summary of results for a specific OHDSI tool, including
#' which tables are present and their row counts.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param tool_id Character; which tool to summarize
#' @return Named list with tool-specific summary
#' @examples
#' \dontrun{
#' summary <- omopOhdsiSummaryDS("omop", "cohort_diagnostics")
#' }
#' @export
omopOhdsiSummaryDS <- function(omop_symbol, tool_id) {
  handle <- .getHandle(omop_symbol)
  .ohdsiGetSummary(handle, tool_id)
}

# --- Query library methods ---

#' List query library templates (Aggregate)
#'
#' @description
#' Returns metadata for all available query templates that pass safety
#' classification. Queries are sourced from the curated allowlist and
#' Markdown templates in \code{inst/queries/queries/}.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param domain Character; optional domain/group filter (e.g., "Condition")
#' @param provider Character; query provider ("native" or "all")
#' @return Data frame with query ID, name, group, description, mode, class,
#'   poolable flag, CDM version, and number of input parameters
#' @examples
#' \dontrun{
#' queries <- omopQueryListDS("omop", domain = "Condition")
#' }
#' @export
omopQueryListDS <- function(omop_symbol, domain = NULL,
                               provider = "native") {
  handle <- .getHandle(omop_symbol)
  .query_list(handle, domain, provider)
}

#' Get query template details (Aggregate)
#'
#' @description
#' Returns full metadata for a specific query template, including input
#' parameters, output schema, and sensitive field annotations. Used by the
#' client to render parameter forms and validate inputs before execution.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param query_id Character; the query ID from the query library
#' @return Named list with query metadata (id, name, description, inputs,
#'   outputs, sensitive_fields, class, poolable)
#' @examples
#' \dontrun{
#' template <- omopQueryGetDS("omop", "condition_prevalence")
#' }
#' @export
omopQueryGetDS <- function(omop_symbol, query_id) {
  handle <- .getHandle(omop_symbol)
  .validateIdentifier(query_id, "query_id")
  .query_get(handle, query_id)
}

#' Execute a query template (Aggregate)
#'
#' @description
#' Executes a query template against the database with DataSHIELD-aligned
#' disclosure controls. Only queries classified as SAFE_AGGREGATE can be
#' executed in aggregate mode. Schema placeholders are automatically resolved
#' from the OMOP handle.
#'
#' Disclosure controls applied:
#' \itemize{
#'   \item Sensitive count columns suppressed below \code{nfilter.tab}
#'   \item Output rows capped at 5000 to prevent long-tail disclosure
#'   \item BLOCKED queries cannot be executed
#'   \item SAFE_ASSIGN queries cannot be executed in aggregate mode
#' }
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param query_id Character; the query ID from the query library
#' @param inputs Named list; parameter values for the query template
#' @param mode Character; retained for backwards compatibility. Only
#'   \code{"aggregate"} is accepted by this aggregate DataSHIELD method. Use the
#'   unified analysis assign path for server-side loaders.
#' @return Disclosure-controlled data frame.
#' @examples
#' \dontrun{
#' result <- omopQueryExecDS("omop", "condition_prevalence", inputs = list())
#' }
#' @export
omopQueryExecDS <- function(omop_symbol, query_id,
                               inputs = list(),
                               mode = "aggregate") {
  handle <- .getHandle(omop_symbol)
  .validateIdentifier(query_id, "query_id")
  inputs <- .ds_arg(inputs)
  mode <- match.arg(mode, c("aggregate", "assign"))
  if (!identical(mode, "aggregate")) {
    stop("omopQueryExecDS is an aggregate-only method. Assign-mode query ",
         "loaders must use omopAnalysisRunAssignDS through a DataSHIELD ",
         "assign call.", call. = FALSE)
  }
  .omopAuditLog("omopQueryExecDS", list(query_id = query_id, inputs = inputs))
  # The legacy query endpoint now delegates to the same single disclosure gate
  # as the unified analysis catalog. In particular this bands counts and couples
  # derived ratios, rather than relying on the legacy small-cell-only pass.
  .omopAnalysisRun(handle, paste0("dsomop:", query_id), inputs,
                   scope = NULL, combine = "union", assign = FALSE)
}

# --- Unified analysis catalog methods ---

#' List unified analysis catalog entries (Aggregate)
#'
#' @description
#' Returns metadata for every entry in the unified analysis catalog — the single
#' registry that folds the curated QueryLibrary SQL templates, the pre-computed
#' Achilles analyses, and the generic OHDSI result tables behind one stable,
#' pack-prefixed naming scheme (\code{"dsomop:<id>"}). Optionally filtered by
#' clinical domain. No SQL, compute functions, or other server internals are
#' returned.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param domain Character; optional domain filter (e.g., "condition", "person")
#' @return Data frame with entry name, domain, adapter, mode, disclosure unit,
#'   description, parameter summary, and scoping capability flags
#' @examples
#' \dontrun{
#' catalog <- omopAnalysisListDS("omop", domain = "condition")
#' }
#' @export
omopAnalysisListDS <- function(omop_symbol, domain = NULL) {
  handle <- .getHandle(omop_symbol)
  .omopAnalysisList(handle, domain)
}

#' Get unified analysis catalog entry metadata (Aggregate)
#'
#' @description
#' Returns full metadata for a single catalog entry: its parameter specs,
#' compute kind, disclosure spec, and scoping capabilities. Used by the client
#' to render parameter forms and decide whether scoping is supported before
#' running the analysis.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param name Character; the entry name (pack-prefixed stable id)
#' @return Named list with entry metadata (no SQL / compute function)
#' @examples
#' \dontrun{
#' meta <- omopAnalysisGetDS("omop", "dsomop:achilles.401")
#' }
#' @export
omopAnalysisGetDS <- function(omop_symbol, name) {
  handle <- .getHandle(omop_symbol)
  name <- .ds_arg(name)
  if (is.list(name)) name <- name[[1]]
  .omopAnalysisGet(handle, name)
}

#' Validate one public cohort-scope reference
#'
#' @param ref Candidate cohort definition id or temporary cohort name.
#' @return A normalized scalar reference.
#' @keywords internal
.omopAnalysisCohortScopeScalar <- function(ref) {
  if (is.numeric(ref) && length(ref) == 1L && !is.na(ref) &&
      is.finite(ref) && ref == floor(ref) && ref > 0 &&
      ref <= .Machine$integer.max) {
    return(ref)
  }
  if (!is.character(ref) || length(ref) != 1L || is.na(ref)) {
    stop("Each scope_cohort_N argument must be one positive integer cohort ",
         "definition id or one temporary cohort name.", call. = FALSE)
  }
  ref <- .validateString(ref)
  if (!nzchar(ref)) {
    stop("Each scope_cohort_N argument must be one positive integer cohort ",
         "definition id or one temporary cohort name.", call. = FALSE)
  }
  if (grepl("^[0-9]+$", ref)) {
    value <- suppressWarnings(as.numeric(ref))
    if (!is.finite(value) || value != floor(value) || value <= 0 ||
        value > .Machine$integer.max) {
      stop("Each scope_cohort_N definition id must be a positive server ",
           "integer.", call. = FALSE)
    }
    return(ref)
  }
  .validateIdentifier(ref, "cohort scope")
}

#' Normalize the literal public analysis scope
#'
#' @param scope Literal scope supplied through the formal \code{scope} argument.
#' @return Scope containing only cohort-reference scalars and \code{omop.table}
#'   frames, preserving supported nested-list structure.
#' @keywords internal
.omopAnalysisNormalizePublicScope <- function(scope) {
  if (is.null(scope) || .is_omop.table(scope)) return(scope)
  if (is.list(scope) && !is.data.frame(scope)) {
    return(lapply(scope, .omopAnalysisNormalizePublicScope))
  }
  .omopAnalysisCohortScopeScalar(scope)
}

#' Assemble resolved cohort and workspace-table scope arguments
#'
#' DataSHIELD must never expose generic aggregate constructors such as
#' \code{base::list}: they can return arbitrary server objects. Multi-table
#' analysis scope is therefore transported through the analysis endpoint's
#' \code{...} arguments instead. Cohort references use contiguous
#' \code{scope_cohort_1}, \code{scope_cohort_2}, ... scalar arguments; resolved
#' frames use contiguous \code{scope_table_1}, \code{scope_table_2}, ...
#' arguments. The endpoint combines cohort references first and frames second,
#' together with the legacy literal \code{scope} form, and enforces both total
#' source and workspace-table caps before any scope is materialised.
#'
#' @param scope Existing scope argument.
#' @param dots Evaluated \code{...} arguments as a list.
#' @return A supported analysis scope value.
#' @keywords internal
.omopAnalysisScopeFromDots <- function(scope, dots) {
  if (!is.list(dots)) {
    stop("Internal error: analysis scope dots must be a list.", call. = FALSE)
  }
  server_max <- as.integer(
    .omopDisclosureSettings()$max_analysis_scope_tables
  )
  total_max <- server_max + 1L
  .omopAnalysisScopeSourceCount(scope, max_sources = total_max)
  scope <- .omopAnalysisNormalizePublicScope(scope)
  if (length(dots) == 0L) return(scope)

  if (length(dots) > total_max) {
    stop("Analysis scope exceeds the server total source cap of ", total_max,
         ".", call. = FALSE)
  }

  dot_names <- names(dots)
  if (is.null(dot_names) || anyNA(dot_names) || any(!nzchar(dot_names))) {
    stop("Additional analysis scope sources must be strictly named ",
         "scope_cohort_1, ... or scope_table_1, ...", call. = FALSE)
  }
  if (anyDuplicated(dot_names)) {
    stop("Additional analysis scope source names must be unique.", call. = FALSE)
  }
  if (any(!grepl("^scope_(cohort|table)_[1-9][0-9]*$", dot_names))) {
    stop("Unexpected analysis argument name; additional scope sources must be ",
         "named scope_cohort_N or scope_table_N.", call. = FALSE)
  }

  ordered_family <- function(family) {
    prefix <- paste0("scope_", family, "_")
    selected <- startsWith(dot_names, prefix)
    values <- dots[selected]
    if (length(values) == 0L) return(list())
    indices <- suppressWarnings(as.integer(sub(prefix, "", names(values),
                                               fixed = TRUE)))
    if (anyNA(indices) ||
        !identical(sort(indices), seq_len(length(indices)))) {
      stop("Additional analysis scope ", family,
           " names must be contiguous from ", prefix,
           "1 with no gaps.", call. = FALSE)
    }
    unname(values[order(indices)])
  }

  cohort_dots <- ordered_family("cohort")
  table_dots <- ordered_family("table")
  if (any(!vapply(table_dots, .is_omop.table, logical(1)))) {
    stop("Each scope_table_N argument must resolve to an omop.table workspace ",
         "object.", call. = FALSE)
  }
  cohort_dots <- lapply(cohort_dots, .omopAnalysisCohortScopeScalar)

  scope_sources <- if (is.null(scope)) {
    list()
  } else if (is.list(scope) && !is.data.frame(scope) &&
             !.is_omop.table(scope)) {
    unname(scope)
  } else {
    list(scope)
  }
  scope_is_table <- vapply(scope_sources, .is_omop.table, logical(1))
  combined <- c(scope_sources[!scope_is_table], cohort_dots,
                scope_sources[scope_is_table], table_dots)
  .omopAnalysisScopeSourceCount(combined, max_sources = total_max)
  if (.omopAnalysisScopeTableCount(combined) > server_max) {
    stop("Analysis scope exceeds the server max_analysis_scope_tables cap of ",
         server_max, ".", call. = FALSE)
  }
  if (length(combined) == 1L) combined[[1L]] else unname(combined)
}

#' Run a unified analysis catalog entry (Aggregate)
#'
#' @description
#' The single fail-closed run path for catalog analyses returning data to the
#' client. Resolves the named entry, validates and sanitizes its parameters,
#' applies optional cohort/table scoping, runs the entry's compute step (SQL
#' template or wrapped Achilles/OHDSI accessor), and funnels the result through
#' the ONE unified per-patient disclosure gate. This replaces the three
#' previously divergent post-run disclosure passes with a single gate that
#' enforces the same per-patient invariant for every adapter.
#'
#' Scoping: \code{scope} accepts a cohort reference (temp-table name or
#' cohort_definition_id) and/or one or more workspace \code{omop.table} symbols.
#' Multiple sources are folded with \code{combine} (union/intersect on
#' subject_id) into one re-gated scoped cohort. SQL entries get a
#' \code{person_id IN (SELECT subject_id FROM <scope>)} predicate; pre-computed
#' Achilles/OHDSI entries reject \code{scope} (no per-row person key).
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param name Character; the entry name (pack-prefixed stable id)
#' @param params Named list; parameter values for the entry
#' @param scope Optional cohort reference and/or \code{omop.table} symbol(s) to
#'   scope the population to (NULL = no scoping)
#' @param combine Character; "union" (default) or "intersect" when scope has
#'   multiple sources
#' @param ... Additional scalar cohort references and resolved workspace tables,
#'   independently and contiguously named \code{scope_cohort_1}, ... and
#'   \code{scope_table_1}, ... without gaps. This provides safe multi-source
#'   transport without registering generic aggregate \code{c()} or \code{list()}
#'   methods.
#' @return Disclosure-controlled data frame
#' @examples
#' \dontrun{
#' df <- omopAnalysisRunDS("omop", "dsomop:condition.prevalence_by_concept",
#'                         params = list(top_n = 25))
#' }
#' @export
omopAnalysisRunDS <- function(omop_symbol, name, params = list(),
                              scope = NULL, combine = "union", ...) {
  handle <- .getHandle(omop_symbol)
  name <- .ds_arg(name)
  if (is.list(name)) name <- name[[1]]
  params <- .ds_arg(params)
  scope <- .ds_arg(scope)
  scope <- .omopAnalysisScopeFromDots(scope, list(...))
  combine <- .ds_arg(combine)
  if (is.list(combine)) combine <- combine[[1]]
  .omopAuditLog("omopAnalysisRunDS",
                list(name = name, params = params,
                     scope_present = !is.null(scope),
                     scope_table_count = .omopAnalysisScopeTableCount(scope),
                     combine = combine))
  .omopAnalysisRun(handle, name, params, scope = scope, combine = combine,
                   assign = FALSE)
}

#' Run a unified analysis catalog assign-mode loader (Assign)
#'
#' @description
#' Assign-mode counterpart to \code{\link{omopAnalysisRunDS}} for QueryLibrary
#' entries whose template \code{mode} is \code{"assign"} (server-side loaders
#' whose result stays on the server, never returned to the client). Resolves and
#' runs the entry through the same fail-closed run path; because the data is not
#' returned, the aggregate disclosure gate does not apply, but parameter
#' sanitization and scope re-gating still do.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param name Character; the entry name (pack-prefixed stable id)
#' @param params Named list; parameter values for the entry
#' @param scope Optional cohort reference and/or \code{omop.table} symbol(s)
#' @param combine Character; "union" (default) or "intersect" when scope has
#'   multiple sources
#' @param date_handling Date policy for the assigned clinical rows: remove
#'   (default), relative, binned, or server-authorized absolute.
#' @param ... Additional scalar cohort references and resolved workspace tables,
#'   independently and contiguously named \code{scope_cohort_1}, ... and
#'   \code{scope_table_1}, ... without gaps.
#' @return The server-side assignment result (data stays on the server)
#' @examples
#' \dontrun{
#' loaded <- omopAnalysisRunAssignDS("omop", "dsomop:condition.occurrence_load")
#' }
#' @export
omopAnalysisRunAssignDS <- function(omop_symbol, name, params = list(),
                                    scope = NULL, combine = "union",
                                    date_handling = NULL, ...) {
  handle <- .getHandle(omop_symbol)
  name <- .ds_arg(name)
  if (is.list(name)) name <- name[[1]]
  params <- .ds_arg(params)
  scope <- .ds_arg(scope)
  scope <- .omopAnalysisScopeFromDots(scope, list(...))
  combine <- .ds_arg(combine)
  if (is.list(combine)) combine <- combine[[1]]
  date_handling <- .ds_arg(date_handling)
  .omopAuditLog("omopAnalysisRunAssignDS",
                list(name = name, params = params,
                     scope_present = !is.null(scope),
                     scope_table_count = .omopAnalysisScopeTableCount(scope),
                     combine = combine))
  .omopAnalysisRun(handle, name, params, scope = scope, combine = combine,
                   assign = TRUE, assign_date_handling = date_handling)
}

# --- Concept-factor harmonization (cross-server coordination) ---

.omopConceptLevelAssessment <- function(df, col, person_col) {
  keep <- !is.na(df[[col]]) & !is.na(df[[person_col]])
  vals <- as.character(df[[col]][keep])
  persons <- df[[person_col]][keep]
  levels_col <- unique(vals)
  n_levels <- length(levels_col)
  if (n_levels == 0L) {
    return(list(levels = character(0), safe = FALSE))
  }
  support <- vapply(levels_col, function(level) {
    length(unique(persons[vals == level]))
  }, integer(1))
  safe <- all(support >= .omopDisclosureSettings()$nfilter_tab) && tryCatch(
    {
      .assertSafeLevels(n_levels, length(unique(persons)))
      TRUE
    },
    error = function(e) FALSE
  )
  list(levels = levels_col, safe = isTRUE(safe))
}

#' Report disclosure-safe levels of concept-id columns
#'
#' Aggregate-mode helper for the client-side concept-factor coordination layer.
#' Scans a previously extracted data frame for columns whose name ends in
#' \code{_concept_id} (both raw integer ids and translated character names keep
#' this suffix) and, for each one, reports its distinct non-missing values as
#' character levels — but only if that level set passes the server's disclosure
#' gate (\code{\link{.assertSafeLevels}}: at most \code{nfilter.levels.max}
#' distinct levels and density at or below \code{nfilter.levels.density}).
#'
#' The client collects each server's safe levels, computes their union in one
#' deterministic order, and broadcasts that shared ordering back via
#' \code{\link{omopAsFactorColumnsDS}} so the federated factor is harmonized
#' across all sites. High-cardinality clinical columns that fail the gate are
#' returned in \code{unsafe} so the client leaves them untouched (raw).
#'
#' Only the distinct category labels leave the server, never row-level data,
#' and only after passing the same disclosure threshold that governs
#' \code{ds.asFactor}/\code{ds.levels}. The result is returned natively
#' (aggregate results are not JSON-encoded on the return path).
#'
#' @param df A data frame previously assigned server-side by
#'   \code{\link{omopPlanExecuteDS}}.
#' @return A named list with three elements: \code{levels} (named list mapping
#'   each safe concept-id column to its character levels), \code{unsafe}
#'   (character vector of concept-id columns that failed the disclosure gate),
#'   and \code{nfilter_levels_max} (the server's level cap, so the client can
#'   reconcile heterogeneous caps).
#' @seealso \code{\link{omopAsFactorColumnsDS}}, \code{\link{.assertSafeLevels}}
#' @export
omopFactorLevelsDS <- function(df) {
  cap <- .omopDisclosureSettings()$nfilter_levels_max
  empty <- list(levels = list(), unsafe = character(0), nfilter_levels_max = cap)
  if (!is.data.frame(df) || !.is_omop.table(df)) {
    return(empty)
  }
  cols <- grep("_concept_id$", names(df), value = TRUE)
  # Columns whose _concept_id suffix was renamed away are tagged at extraction
  # time (omopPlanExecuteDS) so harmonization still recognises them.
  tagged <- attr(df, "omop_concept_cols")
  if (length(tagged) > 0L) {
    cols <- union(cols, intersect(as.character(tagged), names(df)))
  }
  # Never expose a protected person/subject key as a factor-level vector
  # (defense in depth; a pseudonymous key has one level per person and would
  # also exceed nfilter.levels.max).
  cols <- setdiff(cols, .PERSON_KEY_COLS())
  keys <- intersect(.PERSON_KEY_COLS(), names(df))
  if (length(keys) == 0L) {
    return(list(levels = list(), unsafe = cols,
                nfilter_levels_max = cap))
  }
  person_col <- if ("person_id" %in% keys) "person_id" else keys[[1]]
  if (length(cols) == 0L) {
    return(empty)
  }
  safe <- list()
  unsafe <- character(0)
  for (col in cols) {
    assessment <- .omopConceptLevelAssessment(df, col, person_col)
    if (length(assessment$levels) == 0L) {
      next
    }
    if (isTRUE(assessment$safe)) {
      safe[[col]] <- assessment$levels
    } else {
      unsafe <- c(unsafe, col)
    }
  }
  list(levels = safe, unsafe = unsafe, nfilter_levels_max = cap)
}

#' Recode concept-id columns to a harmonized factor
#'
#' Assign-mode counterpart to \code{\link{omopFactorLevelsDS}}. Given the union
#' of disclosure-safe levels computed across the federation by the client, this
#' rebuilds the named concept-id columns of a server-side data frame as factors
#' that share one identical level ordering across every site. Identical level
#' coding is what makes pooled \code{ds.glm} / \code{ds.glmSLMA} and
#' \code{ds.table} behave correctly on the federated factor.
#'
#' A value present on only some sites becomes an empty level on the sites that
#' lack it — valid base R, and the modelling functions handle it (pooled
#' estimation uses the global data; study-level meta-analysis yields per-study
#' \code{NA} without crashing). Columns absent from this server are silently
#' skipped, so the same broadcast spec works for every site.
#'
#' Recoding is representation-only: every original observed local level must
#' pass the same per-level person-support, density, and level-cap gate as
#' \code{\link{omopFactorLevelsDS}}, and the shared contract must cover every
#' observed value. If any requested present column fails those conditions, the
#' complete call silently returns the original frame byte-for-byte. This
#' fail-quiet, all-or-nothing behavior prevents category-presence probes while
#' guaranteeing that recoding never creates new missing values. A column with
#' no observed values may still be recoded to the public contract, and safe
#' levels present only at another site remain valid empty levels locally.
#'
#' The level cap is re-enforced here independently of the client: a column whose
#' requested level count exceeds \code{nfilter.levels.max} is rejected, so a
#' buggy or hostile client cannot coerce a disclosive factor onto the server.
#' On validation error the original data frame keeps its prior value (the
#' assignment is not applied), so the column safely remains raw.
#'
#' @param df A data frame previously assigned server-side by
#'   \code{\link{omopPlanExecuteDS}}.
#' @param spec A JSON-encoded named list mapping each concept-id column to the
#'   shared character levels to impose (decoded via \code{\link{.ds_arg}}).
#' @return The data frame with the specified concept-id columns recoded as
#'   harmonized factors; the value is re-assigned to the original symbol.
#' @seealso \code{\link{omopFactorLevelsDS}}
#' @export
omopAsFactorColumnsDS <- function(df, spec) {
  if (!is.data.frame(df) || !.is_omop.table(df)) {
    stop("omopAsFactorColumnsDS: target must be a dsOMOP omop.table.",
         call. = FALSE)
  }
  spec <- .ds_arg(spec)
  if (!is.list(spec) || length(spec) == 0L) {
    return(df)
  }
  spec_names <- names(spec)
  if (is.null(spec_names) || anyNA(spec_names) || any(!nzchar(spec_names)) ||
      anyDuplicated(spec_names)) {
    stop("omopAsFactorColumnsDS: spec must map unique column names to levels.",
         call. = FALSE)
  }
  source <- df
  cap <- .omopDisclosureSettings()$nfilter_levels_max
  allowed_cols <- grep("_concept_id$", names(df), value = TRUE)
  allowed_cols <- union(
    allowed_cols,
    intersect(as.character(attr(df, "omop_concept_cols") %||% character(0)),
              names(df))
  )
  allowed_cols <- setdiff(allowed_cols,
                          union(.PERSON_KEY_COLS(),
                                attr(df, "dsomop_protected") %||% character(0)))
  contracts <- list()
  for (col in names(spec)) {
    # A hostile client cannot coerce a protected key into a factor.
    if (col %in% .PERSON_KEY_COLS()) {
      next
    }
    if (!col %in% names(df)) {
      next
    }
    if (!col %in% allowed_cols) {
      stop("omopAsFactorColumnsDS: column '", col,
           "' is not a tagged concept column.", call. = FALSE)
    }
    levels_col <- as.character(unlist(spec[[col]], use.names = FALSE))
    levels_col <- levels_col[!is.na(levels_col) & nzchar(levels_col)]
    if (length(levels_col) == 0L) {
      next
    }
    if (length(levels_col) > cap) {
      stop(
        "omopAsFactorColumnsDS: requested levels for '", col,
        "' exceed nfilter.levels.max (", cap, ").",
        call. = FALSE
      )
    }
    if (anyDuplicated(levels_col)) {
      stop("omopAsFactorColumnsDS: levels must be unique.", call. = FALSE)
    }
    contracts[[col]] <- levels_col
  }
  if (length(contracts) == 0L) {
    return(source)
  }
  keys <- intersect(.PERSON_KEY_COLS(), names(source))
  if (length(keys) == 0L) {
    return(source)
  }
  person_col <- if ("person_id" %in% keys) "person_id" else keys[[1L]]
  contract_safe <- vapply(names(contracts), function(col) {
    assessment <- .omopConceptLevelAssessment(source, col, person_col)
    all(is.na(source[[col]])) ||
      (isTRUE(assessment$safe) &&
       all(assessment$levels %in% contracts[[col]]))
  }, logical(1L))
  if (!all(contract_safe)) {
    return(source)
  }

  applied_levels <- list()
  for (col in names(contracts)) {
    levels_col <- contracts[[col]]
    recoded <- factor(as.character(source[[col]]), levels = levels_col)
    if (!identical(recoded, df[[col]])) {
      df[[col]] <- recoded
      applied_levels[[col]] <- levels_col
    }
  }
  .dsomopDpResealConceptFactors(df, source, applied_levels)
}
