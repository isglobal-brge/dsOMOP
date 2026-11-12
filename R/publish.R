# Publication is the only patient-support boundary on recipe paths. Counts,
# immutable rosters and materialisations never enter the public status object.

.publicationRegistry <- new.env(parent = emptyenv())
.planContextRegistry <- new.env(parent = emptyenv())

#' Create a private execution context
#' @keywords internal
.planContext <- function(handle, ir, output_mode = "memory", target_env = new.env(parent = emptyenv())) {
  ctx <- new.env(parent = emptyenv())
  class(ctx) <- "dsomop_plan_context"
  ctx$private <- TRUE
  ctx$closed <- FALSE
  ctx$committed <- FALSE
  ctx$id <- .generateStagingToken()
  ctx$handle <- handle
  ctx$ir <- ir
  ctx$output_mode <- output_mode
  ctx$target_env <- target_env
  ctx$staging_env <- new.env(parent = emptyenv())
  ctx$rosters <- new.env(parent = emptyenv())
  ctx$inventory <- list()
  ctx$prepared <- list()
  ctx$rejected <- list()
  ctx$unavailable <- character(0)
  ctx$owned <- list(symbols = character(0), directories = character(0), files = character(0),
                    status_bindings = list())
  ctx$rollback <- list()
  ctx$published_values <- list()
  ctx$published_variants <- list()
  ctx$commit_policy <- "server"
  ctx$seal <- NULL
  ctx$already_tokenized <- FALSE
  ctx$dialect <- if (!is.null(handle)) {
    .sqlDialectProfile(handle, probe = TRUE)
  } else NULL
  assign(ctx$id, ctx, .planContextRegistry)
  ctx
}

#' Recognise an open, server-created execution context
#' @keywords internal
.isPlanContext <- function(ctx) {
  is.environment(ctx) && inherits(ctx, "dsomop_plan_context") &&
    isTRUE(ctx$private) && !isTRUE(ctx$closed) && .isScalarString(ctx$id) &&
    exists(ctx$id, .planContextRegistry, inherits = FALSE) &&
    identical(get(ctx$id, .planContextRegistry, inherits = FALSE), ctx)
}

.publishMessage <- function(code) {
  if (identical(code, "insufficient_support")) {
    return(paste0("Disclosive: operation blocked - insufficient individuals to meet ",
                  "disclosure threshold. No further details available."))
  }
  paste0("Publication refused: ", code, ".")
}

.publishReject <- function(code, output = NULL) {
  stop(structure(list(message = .publishMessage(code), call = NULL,
                      code = code, output = output),
                 class = c("dsomop_plan_rejection", "error", "condition")))
}

# A-18: the closed vocabulary of the optional `reason` of a rejected status
# entry (section 11 adapter refusals), present only with validation_error.
.REJECTION_REASONS <- c("adapter_person_mismatch", "cost_currency_mixed")

# The bounded status entry for a dsomop_plan_rejection condition: {code,
# message} with the constant message per code, plus `reason` only when the
# code is validation_error and the condition names one construct of the closed
# vocabulary. The condition's raw message, field path and attached state never
# enter the status (A-18).
.rejectionEntry <- function(e) {
  entry <- list(code = e$code, message = .publishMessage(e$code))
  if (identical(e$code, "validation_error") && .isScalarString(e$reason) &&
      e$reason %in% .REJECTION_REASONS) {
    entry$reason <- e$reason
  }
  entry
}

#' Register an immutable roster namespace
#' @keywords internal
.planRegisterRoster <- function(ctx, roster_id, frame) {
  stopifnot(.isPlanContext(ctx), is.data.frame(frame), .isScalarString(roster_id))
  if (exists(roster_id, ctx$rosters, inherits = FALSE)) {
    if (!identical(get(roster_id, ctx$rosters, inherits = FALSE), frame)) {
      .publishReject("lineage_malformed")
    }
  } else {
    assign(roster_id, frame, ctx$rosters)
    lockBinding(roster_id, ctx$rosters)
  }
  invisible(roster_id)
}

# Length-prefixed tuples preserve delimiters and integer64 precision. Decimal
# numeric keys must also agree with their integer and CSV character forms.
.publishKeys <- function(frame, columns) {
  if (!all(columns %in% names(frame)) || anyNA(frame[columns])) {
    .publishReject("lineage_malformed")
  }
  if (nrow(frame) == 0L) return(character(0))
  values <- lapply(frame[columns], function(x) {
    if (is.numeric(x) && !inherits(x, "integer64")) {
      # Format separately so a fractional neighbour cannot add trailing zeros
      # to an integral key, and never coerce integer64 through a double.
      vapply(x, format, character(1L), scientific = FALSE, trim = TRUE, digits = 15L)
    } else as.character(x)
  })
  if (!length(values)) .publishReject("lineage_malformed")
  do.call(paste0, lapply(values, function(x) paste0(nchar(x, type = "bytes"), ":", x)))
}

#' Resolve exactly one genuine person for every materialised patient row
#' @keywords internal
.publishPersons <- function(ctx, cand, frame) {
  ln <- cand$lineage
  if (!is.data.frame(frame)) .publishReject("lineage_malformed", cand$output)
  if (ln$mode == "reference") {
    if (any(c(.PERSON_KEY_COLS(), .EPISODE_KEY_COLS(), "set_id") %in% names(frame))) {
      .publishReject("lineage_malformed", cand$output)
    }
    return(NULL)
  }
  if (ln$mode == "direct") {
    if (!ln$key_columns %in% names(frame)) .publishReject("lineage_malformed", cand$output)
    persons <- frame[[ln$key_columns]]
  } else {
    if (!exists(ln$roster_id, ctx$rosters, inherits = FALSE)) {
      .publishReject("lineage_malformed", cand$output)
    }
    roster <- get(ln$roster_id, ctx$rosters, inherits = FALSE)
    if (!ln$roster_person_col %in% names(roster) || anyNA(roster[[ln$roster_person_col]])) {
      .publishReject("lineage_malformed", cand$output)
    }
    roster_keys <- .publishKeys(roster, ln$roster_key_columns)
    if (anyDuplicated(roster_keys)) .publishReject("lineage_malformed", cand$output)
    at <- match(.publishKeys(frame, ln$key_columns), roster_keys)
    if (anyNA(at)) .publishReject("lineage_malformed", cand$output)
    persons <- roster[[ln$roster_person_col]][at]
  }
  if (anyNA(persons) || any(!nzchar(as.character(persons)))) {
    .publishReject("lineage_malformed", cand$output)
  }
  for (key in intersect(.PERSON_KEY_COLS(), names(frame))) {
    if (anyNA(frame[[key]]) || !identical(as.character(frame[[key]]), as.character(persons))) {
      .publishReject("lineage_malformed", cand$output)
    }
  }
  as.character(persons)
}

.publishFreshSymbol <- function(ctx, requested) {
  if (!.isScalarString(requested) || !grepl("^[A-Za-z][A-Za-z0-9._]*$", requested)) {
    .publishReject("validation_error")
  }
  used <- vapply(ctx$inventory, function(x) x$symbol, "")
  symbol <- requested
  if (requested %in% names(ctx$rollback) && !requested %in% used) return(requested)
  i <- 1L
  while (exists(symbol, ctx$target_env, inherits = FALSE) || symbol %in% used) {
    symbol <- paste0(requested, ".", i)
    i <- i + 1L
  }
  symbol
}

#' Prepare one declared release unit on its actual materialisation
#' @param value A frame or a private staged descriptor. If omitted, the materialisation
#'   must already be in the context staging environment under output/component.
#' @keywords internal
.publishPrepare <- function(ctx, cand, value = NULL, already_tokenized = FALSE) {
  stopifnot(.isPlanContext(ctx))
  .assertPublicationCandidate(cand)
  if (length(intersect(cand$private_columns, cand$lineage$key_columns))) {
    .publishReject("lineage_malformed", cand$output)
  }
  id <- paste(cand$output, cand$component, sep = "/")
  if (id %in% names(ctx$prepared)) stop("Component prepared more than once.", call. = FALSE)
  if (is.null(value)) value <- get(id, ctx$staging_env, inherits = FALSE)
  support <- character(0)
  rows <- 0
  threshold <- .omopDisclosureSettings()$nfilter_subset
  inspect <- function(frame) {
    rows <<- rows + nrow(frame)
    persons <- .publishPersons(ctx, cand, frame)
    # Retain at most k identities, but continue validating every subsequent row.
    if (length(support) < threshold) support <<- head(unique(c(support, persons)), threshold)
    invisible(NULL)
  }
  if (inherits(value, "FlowerDatasetDescriptor")) {
    .publishReadChunks(value, inspect)
    columns <- value$metadata$columns
  } else {
    inspect(value)
    columns <- names(value)
  }
  passed <- cand$lineage$mode == "reference" || length(support) >= threshold
  rm(support)
  .omopAuditLog("publication", list(output = cand$output, component = cand$component,
                                    passed = passed))
  unavailable <- identical(cand$status, "unavailable")
  # The support decision of a root whose sources are unavailable at this site
  # is taken at commit, after its availability (.publishResolveAvailability()).
  if (!passed && !unavailable) ctx$rejected[[cand$output]] <- list(
    code = "insufficient_support", message = .publishMessage("insufficient_support"))
  if (unavailable) ctx$unavailable <- union(ctx$unavailable, cand$output)
  requested <- cand$requested_symbol %||% tools::file_path_sans_ext(basename(cand$requested_path))
  symbol <- .publishFreshSymbol(ctx, requested)
  ctx$inventory[[id]] <- list(symbol = symbol, grain = cand$grain,
                              n_columns = as.integer(length(setdiff(columns, cand$private_columns))))
  assign(id, value, ctx$staging_env)
  ctx$prepared[[id]] <- list(candidate = cand, passed = passed,
                            already_tokenized = already_tokenized, rows = rows)
  invisible(passed)
}

#' Resolve availability before support (design 7.3; WS-1 review N11)
#'
#' A root whose sources this site cannot serve (\code{status =
#' "unavailable"}, \code{on_missing_table = "unavailable"}) is decided as a
#' family, before any support refusal: when one of its patient components
#' materialised no row, the root is withheld -- reported in
#' \code{unavailable} only, with no support refusal and no artefact (an empty
#' component of an unavailable root is the unavailability itself, never "no
#' events"). Otherwise its components are releasable patient data and keep
#' the ordinary support rule: every component at or above
#' \code{nfilter.subset} publishes the family, which stays reported in
#' \code{unavailable} (its release lacks the unavailable sources); a
#' component below it refuses the family with \code{insufficient_support}.
#' The status sets are disjoint where they would contradict each other: a
#' rejected root is never reported unavailable, a withheld root is never
#' published or rejected.
#'
#' @param ctx Execution context with every candidate prepared.
#' @return Invisibly, the withheld roots.
#' @keywords internal
.publishResolveAvailability <- function(ctx) {
  stopifnot(.isPlanContext(ctx))
  outputs <- vapply(ctx$prepared, function(p) p$candidate$output, "")
  flagged <- vapply(ctx$prepared, function(p) identical(p$candidate$status, "unavailable"), logical(1))
  withheld <- character(0)
  for (root in unique(outputs[flagged])) {
    ids <- names(ctx$prepared)[outputs == root]
    patient <- ids[vapply(ctx$prepared[ids], function(p) p$candidate$lineage$mode != "reference", logical(1))]
    if (any(vapply(ctx$prepared[patient], function(p) identical(p$rows, 0), logical(1)))) {
      # A component prepared before the unavailable component may already
      # have recorded a support refusal. Availability decides the whole
      # family; it supersedes only support, never a technical rejection.
      if (identical(ctx$rejected[[root]]$code, "insufficient_support")) {
        ctx$rejected[[root]] <- NULL
      }
      for (id in ids) {
        ctx$prepared[[id]]$withheld <- TRUE
        ctx$inventory[[id]] <- NULL
        if (exists(id, ctx$staging_env, inherits = FALSE)) rm(list = id, envir = ctx$staging_env)
      }
      ctx$unavailable <- union(ctx$unavailable, root)
      withheld <- c(withheld, root)
    } else if (!is.null(ctx$rejected[[root]]) ||
               !all(vapply(ctx$prepared[ids], function(p) isTRUE(p$passed), logical(1)))) {
      if (is.null(ctx$rejected[[root]])) {
        ctx$rejected[[root]] <- list(code = "insufficient_support",
                                     message = .publishMessage("insufficient_support"))
      }
    }
  }
  ctx$unavailable <- setdiff(ctx$unavailable, names(ctx$rejected))
  invisible(withheld)
}

#' Read a private staged materialisation in bounded chunks
#' @keywords internal
.publishReadChunks <- function(descriptor, callback) {
  path <- descriptor$metadata$file
  if (descriptor$metadata$format == "parquet") {
    reader <- arrow::ParquetFileReader$create(path)
    if (reader$num_row_groups == 0L) callback(as.data.frame(reader$ReadTable()))
    if (reader$num_row_groups > 0L) for (i in seq_len(reader$num_row_groups)) {
      callback(as.data.frame(reader$ReadRowGroup(i - 1L)))
    }
  } else {
    con <- file(path, open = "rt")
    on.exit(close(con), add = TRUE)
    header <- readLines(con, n = 1L)
    repeat {
      # Read via the CSV parser (quoted newlines are records, not line boundaries).
      chunk <- tryCatch(utils::read.csv(con, header = FALSE, col.names = descriptor$metadata$columns,
                               nrows = 50000L, colClasses = "character",
                               check.names = FALSE, stringsAsFactors = FALSE), error = function(e) {
        if (!grepl("no lines available", conditionMessage(e), fixed = TRUE)) stop(e)
        as.data.frame(stats::setNames(rep(list(character(0)), length(descriptor$metadata$columns)),
                                      descriptor$metadata$columns))
      })
      callback(chunk)
      if (nrow(chunk) < 50000L) break
    }
  }
  invisible(NULL)
}

.publishInject <- function(ctx, step) {
  if (is.function(ctx$failure_hook)) ctx$failure_hook(step)
  invisible(NULL)
}

# Ownership checks retain only a server-private fingerprint after commit, so
# deleting a large published table also releases its materialised frame.
.publishFingerprint <- function(value) {
  as.character(openssl::sha256(serialize(value, NULL, version = 3L)))
}

#' Revoke every execution-owned artefact and verify cleanup
#' @keywords internal
.publishAbort <- function(ctx) {
  failures <- character(0)
  for (symbol in rev(ctx$owned$symbols)) {
    tryCatch({
      value <- if (exists(symbol, ctx$target_env, inherits = FALSE)) {
        get(symbol, ctx$target_env, inherits = FALSE)
      } else NULL
      expected <- c(ctx$published_values[[symbol]], ctx$published_variants[[symbol]])
      if (exists(symbol, ctx$target_env, inherits = FALSE) &&
          .publishFingerprint(value) %in% expected) {
        rm(list = symbol, envir = ctx$target_env)
        if (exists(symbol, ctx$target_env, inherits = FALSE)) stop("symbol remains")
      }
    }, error = function(e) failures <<- c(failures, "symbol"))
  }
  for (symbol in names(ctx$rollback)) {
    tryCatch(assign(symbol, ctx$rollback[[symbol]], ctx$target_env),
             error = function(e) failures <<- c(failures, "replacement"))
  }
  for (binding in ctx$owned$status_bindings) {
    tryCatch({
      if (exists(binding$symbol, binding$env, inherits = FALSE) &&
          identical(get(binding$symbol, binding$env, inherits = FALSE), ctx$status)) {
        rm(list = binding$symbol, envir = binding$env)
        if (exists(binding$symbol, binding$env, inherits = FALSE)) stop("status remains")
      }
    }, error = function(e) failures <<- c(failures, "status"))
  }
  for (path in rev(unique(c(ctx$owned$files, ctx$owned$directories)))) {
    if (file.exists(path) || dir.exists(path)) unlink(path, recursive = TRUE)
    if (file.exists(path) || dir.exists(path)) failures <- c(failures, "file")
  }
  if (!is.null(ctx$handle)) ctx$handle$staging_dirs <- setdiff(
    ctx$handle$staging_dirs, ctx$owned$directories)
  ctx$committed <- FALSE
  ctx$closed <- TRUE
  if (length(failures)) stop("Publication cleanup failed.", call. = FALSE)
  if (exists(ctx$id, .publicationRegistry, inherits = FALSE)) {
    rm(list = ctx$id, envir = .publicationRegistry)
  }
  invisible(TRUE)
}

#' Close private execution state on every exit path
#' @keywords internal
.planContextClose <- function(ctx) {
  on.exit({
    if (length(ls(ctx$staging_env, all.names = TRUE))) {
      rm(list = ls(ctx$staging_env, all.names = TRUE), envir = ctx$staging_env)
    }
    ctx$closed <- TRUE
    ctx$rosters <- new.env(parent = emptyenv())
    ctx$ir <- NULL
    ctx$seal <- NULL
    ctx$rollback <- list()
    if (exists(ctx$id, .planContextRegistry, inherits = FALSE)) rm(list = ctx$id, envir = .planContextRegistry)
  }, add = TRUE)
  if (!isTRUE(ctx$committed)) .publishAbort(ctx)
  invisible(NULL)
}

#' Commit complete passing families after sanitisation and sealing
#' @keywords internal
.publishCommit <- function(ctx) {
  stopifnot(.isPlanContext(ctx))
  completed <- FALSE
  on.exit({ if (!completed) .publishAbort(ctx) }, add = TRUE)
  .publishResolveAvailability(ctx)
  strict <- isTRUE(.omopDisclosureSettings()$query_strict) || identical(ctx$commit_policy, "all_or_none")
  rejected <- names(ctx$rejected)
  technical <- vapply(ctx$rejected, function(x) x$code != "insufficient_support", logical(1))
  accepted <- if ((strict && length(rejected)) || any(technical)) character(0) else {
    names(ctx$prepared)[!vapply(ctx$prepared, function(x) {
      x$candidate$output %in% rejected || isTRUE(x$withheld)
    }, logical(1))]
  }
  ready <- list()
  published <- list()
  descriptors <- list()
  for (id in accepted) {
    pc <- ctx$prepared[[id]]$candidate
    value <- get(id, ctx$staging_env, inherits = FALSE)
    if (is.data.frame(value)) {
      value[intersect(pc$private_columns, names(value))] <- NULL
      if (!isTRUE(ctx$prepared[[id]]$already_tokenized)) {
        value <- .pseudonymizeIdentifiers(value, .personKey(ctx$handle), .personKeyPublicContract(ctx$handle))
      }
    }
    .publishInject(ctx, "tokenize")
    if (is.function(ctx$seal)) value <- ctx$seal(value, pc)
    .publishInject(ctx, "seal")
    if (identical(ctx$output_mode, "staged") && is.data.frame(value)) {
      value <- .publishStageFrame(ctx, pc, value)
    }
    ready[[id]] <- value
  }
  # All transformations finish before the first public destination is touched.
  for (id in accepted) {
    pc <- ctx$prepared[[id]]$candidate
    entry <- ctx$inventory[[id]]
    # A hook/compiler may have occupied a name since prepare; reserve again.
    if (exists(entry$symbol, ctx$target_env, inherits = FALSE) && !entry$symbol %in% names(ctx$rollback)) {
      entry$symbol <- .publishFreshSymbol(ctx, entry$symbol)
      ctx$inventory[[id]] <- entry
    }
    value <- ready[[id]]
    if (inherits(value, "FlowerDatasetDescriptor")) {
      value <- .publishMoveStaged(ctx, value)
      descriptors[[sub("^omop.plan.", "", value$dataset_id)]] <- value
      entry$n_columns <- as.integer(length(value$metadata$columns))
    } else entry$n_columns <- as.integer(ncol(value))
    ctx$owned$symbols <- c(ctx$owned$symbols, entry$symbol)
    ctx$published_values[[entry$symbol]] <- .publishFingerprint(value)
    assign(entry$symbol, value, ctx$target_env)
    .publishInject(ctx, "assign")
    published[[pc$output]][[pc$component]] <- entry
  }
  if (length(descriptors)) {
    .publishInject(ctx, "manifest")
    .writeStagingManifest(ctx$staging_dir, descriptors)
  }
  if (!is.null(ctx$private_dir)) {
    unlink(file.path(ctx$staging_dir, ".private"), recursive = TRUE)
    if (dir.exists(file.path(ctx$staging_dir, ".private"))) stop("Private staging cleanup failed.", call. = FALSE)
  }
  if (!length(descriptors) && !is.null(ctx$staging_dir)) {
    unlink(ctx$staging_dir, recursive = TRUE)
    if (dir.exists(ctx$staging_dir)) stop("Staging cleanup failed.", call. = FALSE)
    ctx$handle$staging_dirs <- setdiff(ctx$handle$staging_dirs, ctx$staging_dir)
  }
  status <- list(status = if (length(rejected)) {
    if (length(published)) "partial" else "refused"
  } else "ok", published = published, rejected = ctx$rejected,
  unavailable = ctx$unavailable)
  .validatePlanStatus(status)
  class(status) <- "dsomop_plan_status"
  attr(status, "execution_id") <- ctx$id
  ctx$status <- status
  ctx$committed <- TRUE
  ctx$closed <- TRUE
  assign(ctx$id, ctx, .publicationRegistry)
  completed <- TRUE
  status
}

#' Validate the narrow status transport (never return attached private state)
#' @keywords internal
.validatePlanStatus <- function(x) {
  codes <- c("insufficient_support", "coverage_ambiguous", "join_cardinality",
             "match_candidates_exceeded", "dialect_unsupported", "unavailable_table", "validation_error")
  valid <- is.list(x) && identical(names(x), c("status", "published", "rejected", "unavailable")) &&
    x$status %in% c("ok", "partial", "refused") && is.list(x$published) && is.list(x$rejected) &&
    is.character(x$unavailable) && !anyNA(x$unavailable)
  if (!isTRUE(valid)) stop("Invalid execution status.", call. = FALSE)
  for (root in x$published) for (entry in root) {
    if (!is.list(entry) || !identical(names(entry), c("symbol", "grain", "n_columns")) ||
        !.isScalarString(entry$symbol) || !entry$grain %in% c("person", "episode", "none") ||
        !is.numeric(entry$n_columns) || length(entry$n_columns) != 1L || is.na(entry$n_columns) ||
        entry$n_columns < 0 || entry$n_columns != floor(entry$n_columns)) {
      stop("Invalid publication inventory.", call. = FALSE)
    }
  }
  for (entry in x$rejected) {
    if (!is.list(entry) || !identical(names(entry)[1:2], c("code", "message")) ||
        !length(entry) %in% 2:3 || !.isScalarString(entry$code) ||
        !entry$code %in% codes || !identical(entry$message, .publishMessage(entry$code))) {
      stop("Invalid rejection status.", call. = FALSE)
    }
    # A-18: an optional closed `reason`, only with validation_error
    if (length(entry) == 3L && (!identical(names(entry)[3L], "reason") ||
        !identical(entry$code, "validation_error") || !.isScalarString(entry$reason) ||
        !entry$reason %in% .REJECTION_REASONS)) {
      stop("Invalid rejection status.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Read a bounded publication status (Aggregate)
#' @param exec_symbol Server execution-status symbol.
#' @return A fixed-schema publication status, without counts or private state.
#' @export
omopPlanStatusDS <- function(exec_symbol) {
  status <- .publicationStatusObject(exec_symbol, parent.frame())
  .validatePlanStatus(status)
  ctx <- get(attr(status, "execution_id"), .publicationRegistry, inherits = FALSE)
  ctx$status_symbol <- .ds_arg(exec_symbol)
  attributes(status) <- list(names = names(status))
  status
}

.publicationStatusObject <- function(exec_symbol, env, require_committed = TRUE) {
  exec_symbol <- .ds_arg(exec_symbol)
  if (!.isScalarString(exec_symbol) || !exists(exec_symbol, env, inherits = TRUE)) {
    stop("Execution status is unavailable.", call. = FALSE)
  }
  status <- get(exec_symbol, env, inherits = TRUE)
  id <- attr(status, "execution_id", exact = TRUE)
  if (!inherits(status, "dsomop_plan_status") || !.isScalarString(id) ||
      !exists(id, .publicationRegistry, inherits = FALSE) ||
      !identical(status, get(id, .publicationRegistry)$status)) {
    stop("Execution status is not server-authenticated.", call. = FALSE)
  }
  ctx <- get(id, .publicationRegistry, inherits = FALSE)
  if (require_committed && !isTRUE(ctx$committed)) {
    stop("Execution status has been revoked.", call. = FALSE)
  }
  binding_env <- env
  while (!exists(exec_symbol, binding_env, inherits = FALSE)) binding_env <- parent.env(binding_env)
  recorded <- vapply(ctx$owned$status_bindings, function(x) {
    identical(x$symbol, exec_symbol) && identical(x$env, binding_env)
  }, logical(1L))
  if (!any(recorded)) ctx$owned$status_bindings <- c(ctx$owned$status_bindings,
    list(list(symbol = exec_symbol, env = binding_env)))
  status
}

#' Revoke an execution or staged component (Aggregate)
#' @param exec_symbol A server execution-status symbol or owned staged descriptor.
#' @return TRUE after verified removal. Missing execution symbols are a no-op.
#' @export
omopStagedRevokeDS <- function(exec_symbol) {
  env <- parent.frame()
  if (is.character(exec_symbol) && length(exec_symbol) == 1L &&
      !exists(exec_symbol, env, inherits = TRUE)) {
    # The client may have deleted this site's status before another site fails.
    # The server-only association was established by the authenticated reader.
    ancestors <- list(env)
    while (!identical(ancestors[[length(ancestors)]], emptyenv())) {
      ancestors[[length(ancestors) + 1L]] <- parent.env(ancestors[[length(ancestors)]])
    }
    for (id in ls(.publicationRegistry, all.names = TRUE)) {
      ctx <- get(id, .publicationRegistry, inherits = FALSE)
      if (identical(ctx$status_symbol, exec_symbol) &&
          any(vapply(ancestors, identical, logical(1), y = ctx$target_env))) .publishAbort(ctx)
    }
    return(invisible(TRUE))
  }
  if (inherits(exec_symbol, "FlowerDatasetDescriptor")) {
    return(.publishRevokeDescriptor(exec_symbol))
  }
  status <- .publicationStatusObject(exec_symbol, env, require_committed = FALSE)
  ctx <- get(attr(status, "execution_id"), .publicationRegistry)
  .publishAbort(ctx)
  invisible(TRUE)
}

#' Apply the same release boundary to standalone returned patient frames
#' @keywords internal
.publishStandalone <- function(value, handle = NULL, caller = "standalone", already_tokenized = TRUE) {
  ctx <- .planContext(handle, list(), target_env = new.env(parent = emptyenv()))
  on.exit(.planContextClose(ctx), add = TRUE)
  key <- intersect(.PERSON_KEY_COLS(), names(value))
  if (!length(key)) .publishReject("lineage_malformed")
  pc <- .publicationCandidate("result", "result", "frame", .lineage("direct", key[[1L]]),
                              requested_symbol = "result", grain = "person")
  .publishPrepare(ctx, pc, value, already_tokenized = already_tokenized)
  if (length(ctx$rejected)) .publishReject("insufficient_support")
  .publishCommit(ctx)
  result <- get("result", ctx$target_env, inherits = FALSE)
  rm(list = ctx$id, envir = .publicationRegistry)
  result
}

#' Reserve an owner-only private staging namespace
#' @keywords internal
.publishStagingDir <- function(ctx) {
  if (!is.null(ctx$private_dir)) return(ctx$private_dir)
  ctx$staging_token <- ctx$id
  ctx$staging_dir <- .createStagingDir(ctx$staging_token)
  ctx$owned$directories <- union(ctx$owned$directories, ctx$staging_dir)
  ctx$handle$staging_dirs <- union(ctx$handle$staging_dirs, ctx$staging_dir)
  ctx$private_dir <- file.path(ctx$staging_dir, ".private", ctx$id)
  if (!dir.create(ctx$private_dir, recursive = TRUE, mode = "0700")) {
    stop("Could not create private staging namespace.", call. = FALSE)
  }
  Sys.chmod(dirname(ctx$private_dir), "0700")
  ctx$private_dir
}

.publishStageFrame <- function(ctx, cand, value) {
  directory <- .publishStagingDir(ctx)
  dataset <- if (cand$component == cand$output) cand$output else paste(cand$output, cand$component, sep = ".")
  contracts <- .publicationStagedContracts(ctx, cand$output,
    if (cand$component == cand$output) NULL else cand$component)
  .stageDataFrame(value, dataset, directory, ctx$staging_token,
                 .personKey(ctx$handle), pseudonymization = .personKeyPublicContract(ctx$handle),
                 semantic_contract = contracts$semantic,
                 bundle_contract = contracts$bundle,
                 ctx = ctx, already_tokenized = TRUE)
}

.publishMoveStaged <- function(ctx, descriptor) {
  from <- descriptor$metadata$file
  to <- file.path(ctx$staging_dir, basename(from))
  if (identical(from, to)) return(descriptor)
  if (!identical(normalizePath(dirname(from)), normalizePath(ctx$private_dir)) ||
      file.exists(to) || .isSymbolicLink(from)) {
    stop("Invalid publication staging destination.", call. = FALSE)
  }
  ctx$owned$files <- union(ctx$owned$files, c(from, to))
  .publishInject(ctx, "move")
  if (!file.rename(from, to)) stop("Could not publish staged file.", call. = FALSE)
  descriptor$metadata$file <- to
  descriptor
}

#' Convert reviewed legacy executor shapes to declared release candidates
#' @keywords internal
.publishOutputs <- function(ctx, outputs, out) {
  roots <- ctx$ir$publish %||% names(out)
  roots <- as.character(unlist(roots, use.names = FALSE))
  if (any(!roots %in% names(ctx$ir$outputs)) || anyDuplicated(roots)) {
    .publishReject("validation_error")
  }
  aliases <- c(risk_sets = "riskSets", transition_ref = "transitionRef")
  references <- c("covariateRef", "timeRef", "transitionRef", "transition_ref")
  for (root in roots) {
    value <- outputs[[root]]
    if (is.null(value)) {
      if (is.null(ctx$rejected[[root]])) .publishReject("validation_error", root)
      next
    }
    if (inherits(value, "dsomop_compiled_output")) {
      tryCatch({
        if (!identical(value$output, root)) .publishReject("validation_error", root)
        .publishCompiled(ctx, value)
      }, dsomop_plan_rejection = function(e) {
        if (!e$code %in% c("coverage_ambiguous", "join_cardinality", "match_candidates_exceeded",
                           "dialect_unsupported", "unavailable_table", "validation_error")) stop(e)
        ctx$rejected[[root]] <- .rejectionEntry(e)
      })
      next
    }
    composite <- !(is.data.frame(value) || inherits(value, "FlowerDatasetDescriptor"))
    components <- if (!composite) {
      stats::setNames(list(value), root)
    } else value
    if (!is.list(components) || !length(components) || is.null(names(components))) {
      .publishReject("lineage_malformed", root)
    }
    roster_id <- root
    if (!is.null(components$personRef)) {
      roster <- components$personRef
      if (inherits(roster, "FlowerDatasetDescriptor")) {
        chunks <- list()
        .publishReadChunks(roster, function(x) chunks[[length(chunks) + 1L]] <<- x)
        roster <- do.call(rbind, chunks)
      }
      .planRegisterRoster(ctx, roster_id, roster)
    }
    for (component in names(components)) {
      frame <- components[[component]]
      descriptor <- inherits(frame, "FlowerDatasetDescriptor")
      columns <- if (descriptor) frame$metadata$columns else names(frame)
      reference <- (composite && component %in% references) ||
        (component == root && identical(ctx$ir$outputs[[root]]$type, "concept_dictionary"))
      key <- intersect(.PERSON_KEY_COLS(), columns)
      lineage <- if (reference) .lineage("reference") else if (length(key)) {
        .lineage("direct", key[[1L]])
      } else if ("rowId" %in% columns && !is.null(components$personRef)) {
        .lineage("roster", "rowId", roster_id, "rowId", "person_id")
      } else .publishReject("lineage_malformed", root)
      suffix <- if (component %in% names(aliases)) aliases[[component]] else component
      symbol <- if (component == root) out[[root]] %||% root else paste0(out[[root]] %||% root, ".", suffix)
      pc <- .publicationCandidate(root, component, "frame", lineage,
        requested_symbol = symbol,
        grain = if (reference) "none" else if (any(c(.EPISODE_KEY_COLS(), "rowId") %in% columns)) "episode" else "person")
      if (is.data.frame(frame)) {
        cc <- intersect(attr(outputs, "omop_concept_cols")[[root]] %||% character(0), names(frame))
        if (length(cc)) attr(frame, "omop_concept_cols") <- cc
      }
      .publishPrepare(ctx, pc, frame, already_tokenized = descriptor || isTRUE(ctx$return_private))
    }
  }
  .publishCommit(ctx)
}

#' Revoke one authenticated manifest member atomically
#' @keywords internal
.publishRevokeDescriptor <- function(descriptor) {
  path <- omopStagedDatasetPath(descriptor)
  manifest_path <- file.path(dirname(path), "manifest.json")
  manifest <- .readStagingManifest(manifest_path)
  keep <- !vapply(manifest$outputs, function(x) identical(x$dataset_id, descriptor$dataset_id), logical(1))
  .writeStagingManifest(dirname(path), manifest$outputs[keep])
  unlink(path)
  if (file.exists(path)) stop("Staged revocation cleanup failed.", call. = FALSE)
  invisible(TRUE)
}

#' Check exact membership in the committed, owner-only manifest
#' @keywords internal
.validateStagedManifestMembership <- function(descriptor, directory) {
  manifest <- .readStagingManifest(file.path(directory, "manifest.json"))
  canonical <- function(x) {
    x <- unclass(x)
    if (!is.null(x$metadata$column_types)) x$metadata$column_types <- as.list(x$metadata$column_types)
    .dsomopDpCanonicalJson(x)
  }
  same <- vapply(manifest$outputs, function(x) {
    identical(x$dataset_id, descriptor$dataset_id) && identical(canonical(x), canonical(descriptor))
  }, logical(1))
  if (sum(same) != 1L) stop("Staged component is not committed or has been revoked.", call. = FALSE)
  invisible(TRUE)
}

#' Record an audited in-place factor recode without changing released attributes
#' @keywords internal
.publishUpdateOwnedValue <- function(before, after) {
  before <- .publishFingerprint(before)
  after <- .publishFingerprint(after)
  for (id in ls(.publicationRegistry, all.names = TRUE)) {
    ctx <- get(id, .publicationRegistry, inherits = FALSE)
    if (!isTRUE(ctx$committed)) next
    for (symbol in names(ctx$published_values)) {
      expected <- c(ctx$published_values[[symbol]], ctx$published_variants[[symbol]])
      if (before %in% expected && !after %in% expected) {
        # Keep earlier representations too: identical components may be
        # harmonised independently, including on DSLite sites in one process.
        ctx$published_variants[[symbol]] <- c(ctx$published_variants[[symbol]], after)
      }
    }
  }
  invisible(NULL)
}

#' Authorise one server-side in-place verb destination with a rollback copy
#' @keywords internal
.publishAllowReplacement <- function(ctx, symbol) {
  stopifnot(.isPlanContext(ctx), .isScalarString(symbol))
  if (!exists(symbol, ctx$target_env, inherits = FALSE)) stop("Replacement target is unavailable.", call. = FALSE)
  ctx$rollback[[symbol]] <- get(symbol, ctx$target_env, inherits = FALSE)
  invisible(symbol)
}
