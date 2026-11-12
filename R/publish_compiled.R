# Native compiler records enter the same publication boundary as legacy
# executors. Staged statements are fetched once in bounded chunks; raw lineage
# is checked before sanitisation, then support is counted on the stored tokens.

.publicationNativeDateHandling <- function(policy) {
  if (!is.null(policy$date_columns)) {
    policy$date_columns <- unname(unlist(policy$date_columns, use.names = FALSE))
  }
  policy <- .normalizeDateHandling(policy)
  policy <- policy[!vapply(policy, is.null, logical(1L))]
  if (!is.null(policy$date_columns)) policy$date_columns <- as.list(policy$date_columns)
  policy
}

#' Check a materialised compiler component against its declared columns
#' @keywords internal
.publicationCompiledFrame <- function(frame, fragment, output) {
  if (!is.data.frame(frame)) .publishReject("validation_error", output)
  declared <- names(fragment$columns)
  actual <- names(frame)
  if (anyDuplicated(tolower(actual)) || anyDuplicated(tolower(declared)) ||
      !setequal(tolower(actual), tolower(declared))) {
    .publishReject("validation_error", output)
  }
  # DBI's portable executor lowercases names. Restore declared analytic keys
  # such as rowId before any lineage resolution or descriptor is constructed.
  frame <- frame[match(tolower(declared), tolower(actual))]
  names(frame) <- declared
  for (name in declared) {
    column <- frame[[name]]
    type <- fragment$columns[[name]]$type
    if (!isTRUE(fragment$columns[[name]]$nullable) && anyNA(column)) {
      .publishReject("validation_error", output)
    }
    values <- column[!is.na(column)]
    valid <- if (!length(values)) TRUE else switch(type,
      integer =, bigint =
        (is.numeric(values) && all(is.finite(values) & values == floor(values))) ||
        inherits(values, "integer64") ||
        (is.character(values) && all(grepl("^-?[0-9]+$", values))),
      numeric = is.numeric(values) && all(is.finite(values)),
      date = inherits(values, "Date") ||
        (is.character(values) && all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", values))),
      text = is.character(values) || is.factor(values),
      boolean = is.logical(values) ||
        (is.numeric(values) && all(values %in% c(0, 1))),
      FALSE
    )
    if (!isTRUE(valid)) .publishReject("validation_error", output)
  }
  frame
}

#' Derive an immutable token roster for a sanitised staged component
#' @keywords internal
.publicationTokenCandidate <- function(ctx, candidate, key) {
  if (!identical(candidate$lineage$mode, "roster")) return(candidate)
  lineage <- candidate$lineage
  if (!exists(lineage$roster_id, ctx$rosters, inherits = FALSE)) {
    .publishReject("lineage_malformed", candidate$output)
  }
  roster <- get(lineage$roster_id, ctx$rosters, inherits = FALSE)
  columns <- unique(c(lineage$roster_key_columns, lineage$roster_person_col))
  if (!all(columns %in% names(roster))) .publishReject("lineage_malformed", candidate$output)
  roster <- roster[columns]
  token_columns <- unique(c(lineage$roster_person_col,
    lineage$roster_key_columns[lineage$key_columns %in% .PERSON_KEY_COLS()]))
  for (column in token_columns) roster[[column]] <- .hashPersonKey(roster[[column]], key)
  # Include the key mapping because the same raw namespace can legitimately
  # expose distinct reviewed unique keys to different output components.
  identity <- paste(c(ctx$id, lineage$roster_id, lineage$key_columns,
                      lineage$roster_key_columns, lineage$roster_person_col), collapse = "/")
  token_namespace <- paste0("publication_", substr(as.character(
    openssl::sha256(charToRaw(enc2utf8(identity)))
  ), 1L, 40L))
  .planRegisterRoster(ctx, token_namespace, roster)
  candidate$lineage$roster_id <- token_namespace
  .assertPublicationCandidate(candidate)
  candidate
}

#' Materialise and prepare every component of a frozen CompiledOutput
#' @keywords internal
.publishCompiled <- function(ctx, compiled) {
  stopifnot(.isPlanContext(ctx))
  .assertCompiledOutput(compiled)
  if (identical(compiled$status, "unavailable")) {
    ctx$unavailable <- union(ctx$unavailable, compiled$output)
  }
  for (candidate in compiled$publication_candidates) {
    id <- paste(candidate$output, candidate$component, sep = "/")
    ctx$publication_shapes[[id]] <- candidate$grain
  }
  for (candidate in compiled$publication_candidates) {
    component <- compiled$components[[candidate$component]]
    if (is.null(component$frame)) .sqlAssertTypedSql(ctx, component$statement)
    if (!identical(ctx$output_mode, "staged")) {
      frame <- component$frame
      if (is.null(frame)) frame <- .executeQuery(ctx$handle, component$statement)
      frame <- .publicationCompiledFrame(frame, component$fragment, candidate$output)
      .publishPrepare(ctx, candidate, frame)
      next
    }
    directory <- .publishStagingDir(ctx)
    key <- .personKey(ctx$handle)
    pseudonymization <- .personKeyPublicContract(ctx$handle)
    token_candidate <- .publicationTokenCandidate(ctx, candidate, key)
    transform <- function(frame) {
      frame <- .publicationCompiledFrame(frame, component$fragment, candidate$output)
      # Resolve every raw row against the declared immutable namespace before
      # tokenisation, including late chunks after sufficient support was seen.
      .publishPersons(ctx, candidate, frame)
      frame[intersect(candidate$private_columns, names(frame))] <- NULL
      frame <- .pseudonymizeIdentifiers(frame, key, pseudonymization)
      .publishInject(ctx, "chunk")
      frame
    }
    dataset <- if (candidate$component == candidate$output) candidate$output else {
      paste(candidate$output, candidate$component, sep = ".")
    }
    contracts <- .publicationStagedContracts(ctx, candidate$output,
      if (candidate$component == candidate$output) NULL else candidate$component)
    if (is.null(component$frame)) {
      info <- .withDbReconnect(ctx$handle, function(conn) {
        .executeQueryToParquet(conn, component$statement,
          file.path(directory, paste0(dataset, ".parquet")),
          chunk_size = getOption("dsomop.staged_chunk_size", 50000L),
          chunk_fn = transform, ctx = ctx)
      })
      ctx$owned$files <- union(ctx$owned$files, info$file)
      descriptor <- .buildStagedDescriptor(dataset, info, ctx$staging_token,
        pseudonymization = pseudonymization,
        semantic_contract = contracts$semantic, bundle_contract = contracts$bundle)
    } else {
      frame <- transform(component$frame)
      descriptor <- .stageDataFrame(frame, dataset, directory, ctx$staging_token,
        key, pseudonymization = pseudonymization,
        semantic_contract = contracts$semantic, bundle_contract = contracts$bundle,
        ctx = ctx, already_tokenized = TRUE)
      ctx$owned$files <- union(ctx$owned$files, descriptor$metadata$file)
    }
    # The component is now fully materialised privately. Count its own stored
    # tokens, resolving roster keys through the immutable equivalent token
    # namespace; no separate count query or raw-to-token comparison is used.
    .publishPrepare(ctx, token_candidate, descriptor, already_tokenized = TRUE)
  }
  invisible(NULL)
}

#' Native-v2 semantic contracts in the existing staged descriptor envelope
#'
#' CR-8: native semantics use an additive nested protocol. The legacy protocol
#' and descriptor envelope are unchanged. The full normalised graph digest
#' binds variable-specific semantics that cannot fit one legacy shape label.
#' @keywords internal
.publicationStagedContracts <- function(ctx, root, component = NULL) {
  if (!.dsomopDpIsV2(ctx$ir)) {
    return(list(semantic = .stagedSemanticContract(ctx$ir, root, component),
                bundle = .stagedBundleContract(ctx$ir, root, ctx$staging_token)))
  }
  ir <- ctx$ir
  if (is.list(ir$scope)) ir$scope$tables_frames <- NULL
  output <- ir$outputs[[root]]
  type <- output$format
  format <- switch(type, survival = output$options$format %||% "survival",
                   person_period = output$options$representation %||% "dense", type)
  date_handling <- if (identical(type, "dictionary")) {
    list(mode = "not_applicable")
  } else {
    .publicationNativeDateHandling(output$options$date_handling %||%
      list(mode = "relative", reference = "index"))
  }
  settings <- .omopDisclosureSettings()
  root_id <- paste(root, root, sep = "/")
  primary <- ctx$prepared[[root_id]]$candidate
  root_grain <- if (identical(type, "dictionary")) "none" else {
    ctx$publication_shapes[[root_id]] %||% primary$grain %||% output$grain %||% "episode"
  }
  base <- list(
    contract_version = "dsomop-staged-semantics-v3",
    output_type = type, output_format = format, component = NULL,
    grain = root_grain, date_handling = date_handling,
    query_semantics_sha256 = .planSemanticDigest(ir)$sha256,
    harmonization_contract_version = settings$harmonization_contract_version,
    age_semantics = settings$age_semantics, age_breaks = NULL,
    date_semantics = settings$date_semantics,
    date_granularity = settings$date_granularity,
    datetime_timezone = settings$datetime_timezone, week_start = settings$week_start
  )
  semantic <- base
  # Preserve the required explicit NULL field for the primary component.
  semantic["component"] <- list(component)
  id <- paste(root, component %||% root, sep = "/")
  candidate <- ctx$prepared[[id]]$candidate
  grain <- ctx$publication_shapes[[id]] %||% candidate$grain
  if (!is.null(grain)) semantic$grain <- grain
  # A primary component is also its bundle's shape, including a compiler-
  # resolved default grain that was omitted in the original normalised plan.
  if (is.null(component)) base$grain <- semantic$grain
  list(semantic = .publicationValidateNativeSemantic(semantic),
       bundle = list(contract_version = "dsomop-staged-bundle-v1",
                     output_id = root, staged_token = ctx$staging_token,
                     semantic_contract = .publicationValidateNativeSemantic(base)))
}

#' Validate the additive native staged semantic protocol without legacy coercion
#' @keywords internal
.publicationValidateNativeSemantic <- function(contract) {
  fail <- function() stop("Invalid native staged semantic contract.", call. = FALSE)
  fields <- c("contract_version", "output_type", "output_format", "component", "grain",
              "date_handling", "query_semantics_sha256", "harmonization_contract_version",
              "age_semantics", "age_breaks", "date_semantics", "date_granularity",
              "datetime_timezone", "week_start")
  if (!is.list(contract) || is.null(names(contract)) ||
      anyDuplicated(names(contract)) || !setequal(names(contract), fields) ||
      !identical(contract$contract_version, "dsomop-staged-semantics-v3")) fail()
  formats <- c("wide", "long", "survival", "intervals", "temporal_covariates",
               "person_period", "sparse", "membership", "dictionary", "joined")
  if (!.isScalarString(contract$output_type) || !contract$output_type %in% formats ||
      !.isScalarString(contract$output_format)) fail()
  allowed_formats <- switch(contract$output_type,
    survival = c("survival", "competing_risk", "recurrent_events", "counting_process", "multi_state"),
    person_period = c("dense", "sparse"), contract$output_type)
  if (!contract$output_format %in% allowed_formats ||
      !.isScalarString(contract$grain) || !contract$grain %in% c("person", "episode", "none") ||
      (!is.null(contract$component) && (!.isScalarString(contract$component) ||
         !grepl(.NAME_PATTERN, contract$component))) ||
      !.isScalarString(contract$query_semantics_sha256) ||
      !grepl("^[0-9a-f]{64}$", contract$query_semantics_sha256) ||
      !is.null(contract$age_breaks)) fail()
  date <- contract$date_handling
  if (!is.list(date) || is.null(names(date)) || anyDuplicated(names(date))) fail()
  if (identical(contract$output_type, "dictionary")) {
    if (!identical(date, list(mode = "not_applicable"))) fail()
  } else {
    if (!all(names(date) %in% c("mode", "reference", "bin_width", "date_columns"))) fail()
    normalized <- tryCatch(.publicationNativeDateHandling(date), error = function(e) NULL)
    if (is.null(normalized)) fail()
    if (!identical(date, normalized)) fail()
  }
  settings <- .omopDisclosureSettings()
  for (field in c("harmonization_contract_version", "age_semantics", "date_semantics",
                  "date_granularity", "datetime_timezone", "week_start")) {
    if (!.isScalarString(contract[[field]]) || !identical(contract[[field]], settings[[field]])) fail()
  }
  contract[fields]
}
