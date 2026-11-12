# Publication is the only patient-support boundary on recipe paths. Counts,
# immutable rosters and materialisations never enter the public status object.

.publicationRegistry <- new.env(parent = emptyenv())

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
  ctx$owned <- list(symbols = character(0), directories = character(0), files = character(0))
  ctx$rollback <- list()
  ctx$commit_policy <- "server"
  ctx$seal <- NULL
  ctx$already_tokenized <- FALSE
  ctx$dialect <- if (!is.null(handle) && exists(".sqlDialectProfile", mode = "function")) {
    .sqlDialectProfile(handle, probe = TRUE)
  } else NULL
  ctx
}

#' Recognise an open, server-created execution context
#' @keywords internal
.isPlanContext <- function(ctx) {
  is.environment(ctx) && inherits(ctx, "dsomop_plan_context") &&
    isTRUE(ctx$private) && !isTRUE(ctx$closed)
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

# Collision-free tuple encoding; no delimiter or floating-point key coercion.
.publishKeys <- function(frame, columns) {
  if (!all(columns %in% names(frame)) || anyNA(frame[columns])) {
    .publishReject("lineage_malformed")
  }
  values <- lapply(frame[columns], as.character)
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
  id <- paste(cand$output, cand$component, sep = "/")
  if (id %in% names(ctx$prepared)) stop("Component prepared more than once.", call. = FALSE)
  if (is.null(value)) value <- get(id, ctx$staging_env, inherits = FALSE)
  support <- character(0)
  threshold <- .omopDisclosureSettings()$nfilter_subset
  inspect <- function(frame) {
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
  if (!passed) ctx$rejected[[cand$output]] <- list(
    code = "insufficient_support", message = .publishMessage("insufficient_support"))
  if (identical(cand$status, "unavailable")) ctx$unavailable <- union(ctx$unavailable, cand$output)
  requested <- cand$requested_symbol %||% tools::file_path_sans_ext(basename(cand$requested_path))
  symbol <- .publishFreshSymbol(ctx, requested)
  ctx$inventory[[id]] <- list(symbol = symbol, grain = cand$grain,
                              n_columns = as.integer(length(setdiff(columns, cand$private_columns))))
  assign(id, value, ctx$staging_env)
  ctx$prepared[[id]] <- list(candidate = cand, passed = passed,
                            already_tokenized = already_tokenized)
  invisible(passed)
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
      chunk <- utils::read.csv(con, header = FALSE, col.names = descriptor$metadata$columns,
                               nrows = 50000L, colClasses = "character",
                               check.names = FALSE, stringsAsFactors = FALSE)
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

#' Revoke every execution-owned artefact and verify cleanup
#' @keywords internal
.publishAbort <- function(ctx) {
  failures <- character(0)
  for (symbol in rev(ctx$owned$symbols)) {
    tryCatch({
      if (exists(symbol, ctx$target_env, inherits = FALSE)) rm(list = symbol, envir = ctx$target_env)
      if (exists(symbol, ctx$target_env, inherits = FALSE)) stop("symbol remains")
    }, error = function(e) failures <<- c(failures, "symbol"))
  }
  for (symbol in names(ctx$rollback)) {
    tryCatch(assign(symbol, ctx$rollback[[symbol]], ctx$target_env),
             error = function(e) failures <<- c(failures, "replacement"))
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
  invisible(TRUE)
}

#' Close private execution state on every exit path
#' @keywords internal
.planContextClose <- function(ctx) {
  if (!isTRUE(ctx$committed)) .publishAbort(ctx)
  if (length(ls(ctx$staging_env, all.names = TRUE))) {
    rm(list = ls(ctx$staging_env, all.names = TRUE), envir = ctx$staging_env)
  }
  ctx$closed <- TRUE
  invisible(NULL)
}

#' Commit complete passing families after sanitisation and sealing
#' @keywords internal
.publishCommit <- function(ctx) {
  stopifnot(.isPlanContext(ctx))
  completed <- FALSE
  on.exit({ if (!completed) .publishAbort(ctx) }, add = TRUE)
  strict <- isTRUE(.omopDisclosureSettings()$query_strict) || identical(ctx$commit_policy, "all_or_none")
  rejected <- names(ctx$rejected)
  technical <- vapply(ctx$rejected, function(x) x$code != "insufficient_support", logical(1))
  accepted <- if ((strict && length(rejected)) || any(technical)) character(0) else {
    names(ctx$prepared)[!vapply(ctx$prepared, function(x) x$candidate$output %in% rejected, logical(1))]
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
    if (exists(entry$symbol, ctx$target_env, inherits = FALSE)) {
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
    assign(entry$symbol, value, ctx$target_env)
    .publishInject(ctx, "assign")
    published[[pc$output]][[pc$component]] <- entry
  }
  if (length(descriptors)) {
    .publishInject(ctx, "manifest")
    .writeStagingManifest(ctx$staging_dir, descriptors)
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
    if (!is.list(entry) || !identical(names(entry), c("code", "message")) ||
        !entry$code %in% codes || !identical(entry$message, .publishMessage(entry$code))) {
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
  attributes(status) <- list(names = names(status))
  status
}

.publicationStatusObject <- function(exec_symbol, env) {
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
  status
}

#' Revoke an execution or staged component (Aggregate)
#' @param exec_symbol A server execution-status symbol or owned staged descriptor.
#' @return TRUE after verified removal. Missing execution symbols are a no-op.
#' @export
omopStagedRevokeDS <- function(exec_symbol) {
  env <- parent.frame()
  if (is.character(exec_symbol) && length(exec_symbol) == 1L &&
      !exists(exec_symbol, env, inherits = TRUE)) return(invisible(TRUE))
  if (inherits(exec_symbol, "FlowerDatasetDescriptor")) {
    return(.publishRevokeDescriptor(exec_symbol))
  }
  status <- .publicationStatusObject(exec_symbol, env)
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
  get("result", ctx$target_env, inherits = FALSE)
}
