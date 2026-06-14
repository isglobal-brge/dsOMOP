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
#' Falls back to the global \code{.dsomop_env} for real DataSHIELD (Opal)
#' deployments where each server runs in its own R process.
#'
#' @param symbol Character; the resource symbol name identifying the handle.
#' @return The OMOP CDM handle object.
#' @keywords internal
.getHandle <- function(symbol) {
  local_key <- paste0(".dsomop_handle_", symbol)

  # DSLite-safe: check the calling session environment (2 frames up:
  # .getHandle <- DS_function <- DSLite session env)
  session_env <- tryCatch(parent.frame(2), error = function(e) NULL)
  if (!is.null(session_env) &&
      exists(local_key, envir = session_env, inherits = FALSE)) {
    return(get(local_key, envir = session_env, inherits = FALSE))
  }

  # Fallback: global package environment (works for Opal single-process)
  key <- paste0("handle_", symbol)
  if (!exists(key, envir = .dsomop_env)) {
    stop("No OMOP handle for symbol '", symbol,
         "'. Call omopInitDS first.", call. = FALSE)
  }
  get(key, envir = .dsomop_env)
}

#' Store an OMOP CDM handle in the session environment
#'
#' Saves the given handle object into the dsOMOP session environment under a
#' key derived from the resource symbol name. Overwrites any existing handle
#' for the same symbol.
#'
#' @param symbol Character; the resource symbol name.
#' @param handle The OMOP CDM handle object to store.
#' @return Invisible NULL (called for side effect).
#' @keywords internal
.setHandle <- function(symbol, handle) {
  key <- paste0("handle_", symbol)
  assign(key, handle, envir = .dsomop_env)
}

#' Remove an OMOP CDM handle from the session environment
#'
#' Closes the database connection associated with the handle and removes it
#' from the dsOMOP session environment. No-op if no handle exists for the
#' given symbol.
#'
#' @param symbol Character; the resource symbol name.
#' @return Invisible NULL (called for side effect).
#' @keywords internal
.removeHandle <- function(symbol) {
  key <- paste0("handle_", symbol)
  if (exists(key, envir = .dsomop_env)) {
    handle <- get(key, envir = .dsomop_env)
    .closeHandle(handle)
    rm(list = key, envir = .dsomop_env)
  }
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
    # Clinical event row IDs (dropped)
    "visit_occurrence_id", "visit_detail_id",
    "condition_occurrence_id", "drug_exposure_id",
    "procedure_occurrence_id", "measurement_id",
    "observation_id", "device_exposure_id",
    "specimen_id", "note_id",
    # Provider / location entity keys (dropped)
    "provider_id", "care_site_id", "location_id"
  )
}

#' Derive the AES key + IV for a resource from its secret key
#'
#' Splits two independent SHA-256 digests of the per-resource secret into a
#' 32-byte AES-256 key and a 16-byte IV. The IV is FIXED per resource (derived
#' deterministically from the same secret), which is what makes the token
#' transform deterministic: the same id always encrypts to the same ciphertext.
#' This is acceptable here because the plaintexts are opaque high-cardinality
#' identifiers used only as join keys, never attacker-chosen messages.
#' @param key Raw vector; the per-resource secret (\code{handle$person_key}).
#' @return List with \code{aes} (32 raw bytes) and \code{iv} (16 raw bytes).
#' @keywords internal
.deriveAesParams <- function(key) {
  if (is.character(key)) key <- charToRaw(paste(key, collapse = ""))
  key <- as.raw(key)
  list(
    aes = as.raw(openssl::sha256(c(key, charToRaw("dsomop-aes-key")))),
    iv  = as.raw(openssl::sha256(c(key, charToRaw("dsomop-aes-iv"))))[1:16]
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
#' Each token is \code{paste0("p", <hex ciphertext>)}. The leading \code{"p"}
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
  vapply(ids, function(id) {
    if (is.na(id)) return(NA_character_)
    ct <- openssl::aes_cbc_encrypt(charToRaw(id), key = params$aes, iv = params$iv)
    # "p" prefix forces as.numeric()/ds.asNumeric() -> NA (non-numeric token).
    paste0("p", paste(as.character(ct), collapse = ""))
  }, character(1L), USE.NAMES = FALSE)
}

#' Reverse a person-key token back to the original id (SERVER-ONLY)
#'
#' Inverse of \code{\link{.hashPersonKey}}: strips the \code{"p"} prefix,
#' hex-decodes the ciphertext, and AES-256-CBC decrypts it under the same
#' key + IV derived from the per-resource secret. This is the server-side reverse
#' map used for population-scoping joins (Phase B); the client never holds the
#' key, so it cannot invert a token. Vectorised; NA preserved.
#'
#' @param token Character vector of tokens produced by \code{.hashPersonKey}.
#' @param key Raw vector; the per-resource secret key (\code{handle$person_key}).
#' @return Character vector of the original identifier values.
#' @keywords internal
.unhashPersonKey <- function(token, key) {
  params <- .deriveAesParams(key)
  token <- as.character(token)
  vapply(token, function(tk) {
    if (is.na(tk)) return(NA_character_)
    hx <- sub("^p", "", tk)
    if (!nzchar(hx) || nchar(hx) %% 2L != 0L || !grepl("^[0-9a-fA-F]+$", hx)) {
      stop("Not a valid person-key token: '", tk, "'.", call. = FALSE)
    }
    raw <- as.raw(strtoi(substring(hx, seq(1L, nchar(hx), 2L),
                                   seq(2L, nchar(hx), 2L)), 16L))
    rawToChar(openssl::aes_cbc_decrypt(raw, key = params$aes, iv = params$iv))
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
#' @return Sanitized object: person/subject keys pseudonymized, other
#'   identifier columns removed.
#' @keywords internal
.pseudonymizeIdentifiers <- function(x, key) {
  if (is.data.frame(x)) {
    drop <- intersect(setdiff(.identifierColumns(), .PERSON_KEY_COLS()), names(x))
    if (length(drop) > 0) {
      x[drop] <- NULL
    }
    keys <- intersect(.PERSON_KEY_COLS(), names(x))
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
    if (length(keys) > 0) {
      attr(x, "dsomop_protected") <- union(attr(x, "dsomop_protected"), keys)
      # Tag every person-bearing assign output as an omop.table (additively, so
      # data.frame methods still dispatch). The client-side data-manipulation
      # verbs (omopMergeDS/omopFilterDS/omopSelectDS/omopBindRowsDS) require this
      # class so they can only ever operate on disclosure-controlled, token-keyed
      # frames produced by dsOMOP — never on arbitrary client-built data.
      class(x) <- union("omop.table", class(x))
    }
  } else if (is.list(x)) {
    x <- lapply(x, .pseudonymizeIdentifiers, key = key)
  }
  x
}

# --- Assign methods ---

#' Initialize an OMOP CDM handle (Assign)
#'
#' @description
#' Creates a server-side connection to an OMOP CDM database from a DataSHIELD
#' resource. The handle is stored in the dsOMOP session environment and used
#' by all subsequent OMOP operations.
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
  resolved <- get(resource_symbol, envir = parent.frame(), inherits = FALSE)

  # DSLite resolves resources to ResourceClient objects during assign.resource;
  # Opal passes raw resource objects. Handle both cases.
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

  .buildBlueprint(handle)
  .setHandle(resource_symbol, handle)

  # DSLite session isolation: in DSLite (multi-server, single R process),
  # each DataSHIELD session has its own environment. We store the handle
  # in parent.frame() (the session env) under a unique key so that
  # server_a's handle is not overwritten by server_b. Without this,
  # .dsomop_env (global) would be shared, causing cross-server data leaks.
  local_key <- paste0(".dsomop_handle_", resource_symbol)
  tryCatch(
    assign(local_key, handle, envir = parent.frame()),
    error = function(e) NULL  # silently skip if env is locked
  )

  invisible(TRUE)
}

#' Execute an extraction plan (Assign)
#'
#' @description
#' Runs the extraction plan and assigns each output directly into the
#' DataSHIELD session as a named symbol. Sparse outputs are split into
#' two symbols: \code{<name>.covariates} and \code{<name>.covariateRef}.
#' Temporal covariates are split into three symbols.
#'
#' When \code{output_mode = "staged"}, outputs are written to Parquet files
#' on disk and assigned as \code{FlowerDatasetDescriptor} objects instead of
#' data.frames. This avoids loading large datasets into R memory.
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
                               scope = NULL, combine = "union") {
  handle <- .getHandle(omop_symbol)
  plan <- .ds_arg(plan)
  out <- .ds_arg(out)
  output_mode <- .ds_arg(output_mode)
  if (!is.character(output_mode) || length(output_mode) != 1L) {
    output_mode <- "memory"
  }
  if (!output_mode %in% c("memory", "staged")) {
    output_mode <- "memory"
  }

  # Recipe-level scope. omop.table SYMBOL sources are spliced in by name (DSI
  # resolves them to server-side frames) because they cannot cross in the plan
  # JSON; a cohort reference may also be present. Carry the resolved frames and
  # the cohort ref / fold operator on plan$scope, where .planResolveScopeCohort
  # folds them into ONE re-gated cohort (via .omopAnalysisResolveScope) and
  # intersects it into every population.
  scope <- .ds_arg(scope)
  combine <- .ds_arg(combine)
  if (is.list(combine)) combine <- combine[[1]]
  if (!is.null(scope)) {
    if (is.null(plan$scope)) plan$scope <- list()
    plan$scope$tables_frames <- scope
    plan$scope$combine <- plan$scope$combine %||% combine
  }

  .omopAuditLog("omopPlanExecuteDS", list(outputs = names(out), plan = plan))
  outputs <- .planExecute(handle, plan, out, output_mode = output_mode)

  # Validate that requested outputs were produced
  missing <- setdiff(names(out), names(outputs))
  if (length(missing) > 0) {
    warning("Plan did not produce outputs: ",
            paste(missing, collapse = ", "), call. = FALSE)
  }

  assign_env <- parent.frame()
  concept_cols <- attr(outputs, "omop_concept_cols") %||% list()

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
    result <- .pseudonymizeIdentifiers(result, handle$person_key)

    # Temporal covariates: split into 3 symbols
    if (is.list(result) && !is.data.frame(result) &&
        "temporalCovariates" %in% names(result)) {
      assign(paste0(sym, ".temporalCovariates"),
             result$temporalCovariates, envir = assign_env)
      assign(paste0(sym, ".covariateRef"),
             result$covariateRef, envir = assign_env)
      assign(paste0(sym, ".timeRef"),
             result$timeRef, envir = assign_env)
    # Sparse outputs: split list into two data.frame symbols
    } else if (is.list(result) && !is.data.frame(result) &&
        all(c("covariates", "covariateRef") %in% names(result))) {
      assign(paste0(sym, ".covariates"),
             result$covariates, envir = assign_env)
      assign(paste0(sym, ".covariateRef"),
             result$covariateRef, envir = assign_env)
    } else {
      # Tag the concept-id columns by their landed (possibly renamed) names so
      # the factor harmonization layer recognises them post-rename. Stamped
      # here, after identifier stripping ([ drops frame attributes), as the
      # last step before the symbol is created.
      cc <- intersect(as.character(concept_cols[[nm]]), names(result))
      if (length(cc) > 0L) {
        attr(result, "omop_concept_cols") <- cc
      }
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
#' @param name Character; cohort name
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
  cohort_spec <- .ds_arg(cohort_spec)
  .omopAuditLog("omopCohortCreateDS", cohort_spec)
  .cohortCreate(handle, cohort_spec, mode, cohort_id, name, overwrite)
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
  .omopAuditLog("omopCohortCombineDS", list(op = op, a = cohort_a, b = cohort_b))
  .cohortCombine(handle, op, cohort_a, cohort_b, new_name)
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
  new_name <- .ds_arg(new_name)
  if (is.list(new_name)) new_name <- if (length(new_name)) new_name[[1]] else NULL
  .omopAuditLog("omopCohortFromTableDS",
                list(n_rows = if (is.data.frame(x)) nrow(x) else NA,
                     new_name = new_name))
  .cohortFromTokenFrame(handle, x, new_name = new_name)
}

#' Clean up temp artifacts (Assign)
#'
#' @description
#' Drops all server-side temporary tables whose names match the given prefix.
#' Called during session teardown or when resetting state between analyses.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param prefix Character; temp table prefix to clean
#' @return Invisible TRUE
#' @examples
#' \dontrun{
#' omopCleanupDS("omop", prefix = "dsomop_")
#' }
#' @export
omopCleanupDS <- function(omop_symbol, prefix = "dsomop_") {
  handle <- .getHandle(omop_symbol)
  to_drop <- grep(paste0("^", prefix), handle$temp_tables,
                  value = TRUE)
  for (tbl in to_drop) {
    .dropTempTable(handle, tbl)
  }
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
#' connection (Rock R session -> OMOP database) warm too. This is the keepalive
#' the Studio uses on a timer to prevent BOTH connection layers from timing out
#' while the app is open.
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
#' tables, DBMS type, CDM version, and a hash for cache invalidation. Used by
#' the client to adapt the UI to the server's data model.
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
#' Validates and previews an extraction plan without executing it. Returns
#' expected output schemas, estimated row counts, and any validation warnings
#' or errors.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param plan List; the extraction plan to preview
#' @return List with preview information including output schemas and warnings
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
#' @return Data frame with period, n_records, suppressed
#' @examples
#' \dontrun{
#' trends <- omopDateCountsDS("omop", "condition_occurrence", granularity = "year")
#' }
#' @export
omopDateCountsDS <- function(omop_symbol, table, date_col = NULL,
                              granularity = "year", cohort_table = NULL,
                              window = NULL) {
  handle <- .getHandle(omop_symbol)
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
#' Returns bin edges that are safe to use as filter thresholds. Each bin is
#' guaranteed to contain enough persons to pass the disclosure threshold,
#' preventing indirect identification via precise numeric filtering.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @param table Character; table name
#' @param column Character; numeric column name
#' @param concept_id Integer or NULL; concept filter
#' @param n_bins Integer; target number of bins (default 10)
#' @param concept_col Character; optional; concept column to scope
#'   \code{concept_id} on instead of the domain concept (e.g.
#'   \code{unit_concept_id}, a \code{*_type_concept_id}, or
#'   \code{value_as_concept_id})
#' @return List with breaks (numeric vector) and counts (integer vector)
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

#' Get Achilles Heel data-quality warnings (Aggregate)
#'
#' @description
#' Returns the Achilles Heel data-quality warnings, disclosure-controlled:
#' \code{record_count} cells below \code{nfilter.tab} are NA-masked and any
#' numeric run interpolated into the warning free-text is scrubbed. Rows (the
#' fired data-quality rules) are kept so a client can show which checks fired
#' without exposing small counts or embedded values.
#'
#' @param omop_symbol Character; the OMOP handle symbol
#' @return Data frame with analysis_id, achilles_heel_warning, rule_id,
#'   record_count
#' @examples
#' \dontrun{
#' heel <- omopAchillesHeelDS("omop")
#' }
#' @export
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
  .ohdsiFindResultTables(handle)
}

#' Query an OHDSI result table (Aggregate)
#'
#' @description
#' Reads rows from a pre-computed OHDSI result table with server-controlled
#' disclosure thresholds. Sensitive columns (SQL, JSON definitions) are
#' automatically excluded. Count columns are subject to small-cell suppression.
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
  .ohdsiGetResults(handle, table_name, columns, filters, order_by, limit,
                    tool_id)
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
#' @param mode Character; "aggregate" (default, returns data to client) or
#'   "assign" (keeps data server-side)
#' @return For aggregate mode: disclosure-controlled data frame.
#'   For assign mode: invisible TRUE.
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
  .omopAuditLog("omopQueryExecDS", list(query_id = query_id, inputs = inputs))
  .query_exec(handle, query_id, inputs, mode)
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
#' @return Disclosure-controlled data frame
#' @examples
#' \dontrun{
#' df <- omopAnalysisRunDS("omop", "dsomop:condition.prevalence_by_concept",
#'                         params = list(top_n = 25))
#' }
#' @export
omopAnalysisRunDS <- function(omop_symbol, name, params = list(),
                              scope = NULL, combine = "union") {
  handle <- .getHandle(omop_symbol)
  name <- .ds_arg(name)
  if (is.list(name)) name <- name[[1]]
  params <- .ds_arg(params)
  scope <- .ds_arg(scope)
  combine <- .ds_arg(combine)
  if (is.list(combine)) combine <- combine[[1]]
  .omopAuditLog("omopAnalysisRunDS",
                list(name = name, params = params, scope = scope,
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
#' @return The server-side assignment result (data stays on the server)
#' @examples
#' \dontrun{
#' loaded <- omopAnalysisRunAssignDS("omop", "dsomop:condition.occurrence_load")
#' }
#' @export
omopAnalysisRunAssignDS <- function(omop_symbol, name, params = list(),
                                    scope = NULL, combine = "union") {
  handle <- .getHandle(omop_symbol)
  name <- .ds_arg(name)
  if (is.list(name)) name <- name[[1]]
  params <- .ds_arg(params)
  scope <- .ds_arg(scope)
  combine <- .ds_arg(combine)
  if (is.list(combine)) combine <- combine[[1]]
  .omopAuditLog("omopAnalysisRunAssignDS",
                list(name = name, params = params, scope = scope,
                     combine = combine))
  .omopAnalysisRun(handle, name, params, scope = scope, combine = combine,
                   assign = TRUE)
}

# --- Concept-factor harmonization (cross-server coordination) ---

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
  if (!is.data.frame(df)) {
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
  if (length(cols) == 0L) {
    return(empty)
  }
  safe <- list()
  unsafe <- character(0)
  for (col in cols) {
    vals <- df[[col]]
    vals <- vals[!is.na(vals)]
    levels_col <- unique(as.character(vals))
    n_levels <- length(levels_col)
    n_total <- length(vals)
    if (n_levels == 0L) {
      next
    }
    is_safe <- tryCatch(
      {
        .assertSafeLevels(n_levels, n_total)
        TRUE
      },
      error = function(e) FALSE
    )
    if (is_safe) {
      safe[[col]] <- levels_col
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
#' The level cap is re-enforced here independently of the client: a column whose
#' requested level count exceeds \code{nfilter.levels.max} is rejected, so a
#' buggy or hostile client cannot coerce a disclosive factor onto the server.
#' On error the original data frame keeps its prior value (the assignment is not
#' applied), so the column safely remains raw.
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
  if (!is.data.frame(df)) {
    stop("omopAsFactorColumnsDS: target is not a data.frame.", call. = FALSE)
  }
  spec <- .ds_arg(spec)
  if (!is.list(spec) || length(spec) == 0L) {
    return(df)
  }
  cap <- .omopDisclosureSettings()$nfilter_levels_max
  for (col in names(spec)) {
    # A hostile client cannot coerce a protected key into a factor.
    if (col %in% .PERSON_KEY_COLS()) {
      next
    }
    if (!col %in% names(df)) {
      next
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
    df[[col]] <- factor(as.character(df[[col]]), levels = levels_col)
  }
  df
}
