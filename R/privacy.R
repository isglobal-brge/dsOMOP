# Dedicated person-level differential-privacy aggregates.
#
# These methods intentionally do not reuse the ordinary disclosure gate.  A
# branch on an exact count before adding noise would itself disclose data.  The
# only accepted input is a server-created `omop.table`; recipes and assign
# verbs therefore remain the flexible way to select, join and format the
# longitudinal data before a bounded DP statistic is requested.

.DSOMOP_DP_STATISTICS <- c(
  "count", "bounded_record_count", "categorical_histogram",
  "numeric_histogram", "bounded_distinct", "bounded_mean", "binary_rate"
)

.dsomopDpQueryLibraryStatus <- function() {
  path <- system.file(
    "queries", "dp_redesign_registry.json", package = "dsOMOP"
  )
  if (!nzchar(path) || !file.exists(path)) {
    return(list(available = FALSE, literal_sql_authorized = FALSE))
  }
  catalog <- .omopQueryLibraryStickyCatalog()
  list(
    available = TRUE,
    upstream_commit = unique(catalog$source_commit)[[1L]],
    mapped_query_count = nrow(catalog),
    mapped_to_bounded_sticky_primitive = catalog$upstream_id,
    primitive_family = catalog$family,
    literal_sql_authorized = FALSE
  )
}

.dsomopDpSpecAtomic <- function(value, name, required = TRUE) {
  if (is.null(value)) {
    if (required) stop("DP specification field '", name, "' is required.",
                       call. = FALSE)
    return(NULL)
  }
  if (is.list(value)) {
    if (length(value) == 0L) value <- character(0) else {
      atomic <- vapply(value, function(item) {
        is.atomic(item) && length(item) == 1L && !is.object(item)
      }, logical(1L))
      if (!all(atomic)) {
        stop("DP specification field '", name, "' must be an atomic vector.",
             call. = FALSE)
      }
      value <- unlist(value, recursive = FALSE, use.names = FALSE)
    }
  }
  if (!is.atomic(value) || is.object(value) || !is.null(names(value)) ||
      anyNA(value)) {
    stop("DP specification field '", name, "' must be an unnamed, complete ",
         "atomic vector.", call. = FALSE)
  }
  value
}

.dsomopDpSpecScalar <- function(value, name, required = TRUE) {
  value <- .dsomopDpSpecAtomic(value, name, required)
  if (is.null(value)) return(NULL)
  if (length(value) != 1L) {
    stop("DP specification field '", name, "' must contain one value.",
         call. = FALSE)
  }
  value[[1L]]
}

.dsomopDpSpecInteger <- function(value, name, lower, upper) {
  value <- .dsomopDpSpecScalar(value, name)
  if (!is.numeric(value) || !is.finite(value) || value != floor(value) ||
      value < lower || value > upper) {
    stop("DP specification field '", name, "' is outside its allowed range.",
         call. = FALSE)
  }
  as.integer(value)
}

.dsomopDpSpecNumber <- function(value, name) {
  value <- .dsomopDpSpecScalar(value, name)
  if (!is.numeric(value) || !is.finite(value)) {
    stop("DP specification field '", name, "' must be finite numeric.",
         call. = FALSE)
  }
  as.numeric(value)
}

.dsomopDpColumn <- function(x, value, name, required = TRUE) {
  value <- .dsomopDpSpecScalar(value, name, required)
  if (is.null(value)) return(NULL)
  if (!is.character(value) || !nzchar(value)) {
    stop("DP specification field '", name, "' must name one column.",
         call. = FALSE)
  }
  value <- .validateIdentifier(value, paste0("DP ", name))
  if (!value %in% names(x)) {
    stop("The DP ", name, " column is unavailable in the assigned table.",
         call. = FALSE)
  }
  protected <- union(
    attr(x, "dsomop_protected", exact = TRUE) %||% character(0),
    .identifierColumns()
  )
  if (tolower(value) %in% tolower(protected)) {
    stop("Protected identifier columns cannot be DP statistic values.",
         call. = FALSE)
  }
  value
}

.dsomopDpValidateInput <- function(x, policy) {
  if (!is.data.frame(x) || !.is_omop.table(x)) {
    stop("omopDpReleaseDS requires a server-created omop.table.",
         call. = FALSE)
  }
  .assertOmopTablePseudonymization(x, caller = "omopDpReleaseDS")
  keys <- .omopPersonKeys(x)
  if (length(keys) == 0L) {
    stop("omopDpReleaseDS requires a pseudonymous person key.",
         call. = FALSE)
  }
  key <- if ("person_id" %in% keys) "person_id" else keys[[1L]]
  protected <- attr(x, "dsomop_protected", exact = TRUE)
  if (!is.character(protected) || anyNA(protected) ||
      !key %in% protected) {
    stop("omopDpReleaseDS requires the intact protected-column guard from ",
         "the server assignment path.", call. = FALSE)
  }
  provenance <- .dsomopDpVerifyPersonLocal(x, policy = policy)
  token <- x[[key]]
  keep <- !is.na(token)
  token <- enc2utf8(as.character(token[keep]))
  if (any(!nzchar(token))) {
    stop("The assigned table contains an invalid person token.",
         call. = FALSE)
  }
  list(
    x = x[keep, , drop = FALSE], token = token, person_key = key,
    lineage_id = provenance$lineage_id
  )
}

.dsomopDpValueType <- function(value, label = "selected DP value") {
  if (inherits(value, "Date")) return("date")
  if (inherits(value, c("POSIXct", "POSIXlt"))) return("datetime_utc")
  if (is.factor(value)) return("factor")
  if (is.logical(value)) return("logical")
  if (is.integer(value)) return("integer")
  if (is.numeric(value)) return("numeric")
  if (is.character(value)) return("character")
  stop("The ", label, " column has an unsupported type.", call. = FALSE)
}

.dsomopDpOrderVector <- function(x, order_by) {
  if (is.null(order_by)) return(NULL)
  value <- x[[order_by]]
  if (inherits(value, "Date")) return(as.numeric(value))
  if (inherits(value, c("POSIXct", "POSIXlt"))) {
    return(as.numeric(as.POSIXct(value, tz = "UTC")))
  }
  if (is.factor(value) || is.character(value)) {
    return(enc2utf8(as.character(value)))
  }
  if (is.logical(value)) return(as.integer(value))
  if (is.numeric(value)) return(value)
  stop("The DP order_by column has an unsupported type.", call. = FALSE)
}

.dsomopDpOrderedGroups <- function(token) {
  people <- sort(unique(token), method = "radix")
  group_index <- match(token, people)
  groups <- split(seq_along(token), group_index)
  names(groups) <- people[as.integer(names(groups))]
  groups
}

.dsomopDpPickIndex <- function(indices, values, order_value, last = FALSE) {
  if (is.null(order_value)) {
    stop("Longitudinal first/last reducers require a public order_by column.",
         call. = FALSE)
  }
  usable <- indices[!is.na(values[indices])]
  if (length(usable) == 0L) return(integer(0))
  usable <- usable[!is.na(order_value[usable])]
  if (length(usable) == 0L) return(integer(0))
  tie_value <- values[usable]
  tie <- if (inherits(tie_value, "Date")) {
    as.numeric(tie_value)
  } else if (inherits(tie_value, c("POSIXct", "POSIXlt"))) {
    as.numeric(as.POSIXct(tie_value, tz = "UTC"))
  } else if (is.factor(tie_value) || is.character(tie_value)) {
    enc2utf8(as.character(tie_value))
  } else if (is.logical(tie_value)) {
    as.integer(tie_value)
  } else if (is.numeric(tie_value)) {
    unname(tie_value)
  } else {
    stop("The DP first/last tie value has an unsupported type.",
         call. = FALSE)
  }
  ordering <- order(order_value[usable], tie, method = "radix",
                    na.last = NA)
  usable[[if (last) utils::tail(ordering, 1L) else ordering[[1L]]]]
}

.dsomopDpReduceOne <- function(token, values, reducer, order_value = NULL) {
  groups <- .dsomopDpOrderedGroups(token)
  people <- names(groups)
  reduced <- vector("list", length(groups))
  keep <- logical(length(groups))
  for (i in seq_along(groups)) {
    indices <- groups[[i]]
    usable <- values[indices]
    usable <- usable[!is.na(usable)]
    if (length(usable) == 0L) next
    result <- switch(
      reducer,
      any = usable[[1L]],
      min = min(usable),
      max = max(usable),
      mean = mean(sort(usable, method = "radix")),
      median = stats::median(sort(usable, method = "radix")),
      mode = {
        labels <- enc2utf8(as.character(usable))
        frequencies <- table(labels)
        sort(names(frequencies)[frequencies == max(frequencies)],
             method = "radix")[[1L]]
      },
      first = {
        index <- .dsomopDpPickIndex(indices, values, order_value, FALSE)
        if (length(index) == 0L) NULL else values[[index]]
      },
      last = {
        index <- .dsomopDpPickIndex(indices, values, order_value, TRUE)
        if (length(index) == 0L) NULL else values[[index]]
      },
      stop("Unsupported longitudinal DP reducer.", call. = FALSE)
    )
    if (!is.null(result) && length(result) == 1L && !is.na(result)) {
      reduced[[i]] <- result
      keep[[i]] <- TRUE
    }
  }
  list(person = people[keep], value = unlist(reduced[keep], use.names = FALSE))
}

.dsomopDpNumericValues <- function(value) {
  type <- .dsomopDpValueType(value)
  if (identical(type, "date")) {
    return(list(value = as.numeric(value), type = type))
  }
  if (identical(type, "datetime_utc")) {
    return(list(value = as.numeric(as.POSIXct(value, tz = "UTC")), type = type))
  }
  if (!type %in% c("integer", "numeric")) {
    stop("This DP statistic requires a numeric, Date, or UTC datetime column.",
         call. = FALSE)
  }
  numeric <- suppressWarnings(as.numeric(value))
  numeric[!is.finite(numeric)] <- NA_real_
  list(value = numeric, type = "number")
}

.dsomopDpBreaks <- function(value, type, policy) {
  value <- .dsomopDpSpecAtomic(value, "breaks")
  if (type == "date") {
    if (!is.character(value) ||
        any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value))) {
      stop("Date histogram breaks must be ISO YYYY-MM-DD values.",
           call. = FALSE)
    }
    parsed <- as.Date(value)
    if (anyNA(parsed) || !identical(format(parsed, "%Y-%m-%d"), value)) {
      stop("Date histogram breaks contain an invalid calendar date.",
           call. = FALSE)
    }
    numeric <- as.numeric(parsed)
    public <- value
  } else if (type == "datetime_utc") {
    if (!is.character(value) ||
        any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
                   value))) {
      stop("Datetime breaks must use canonical UTC YYYY-MM-DDTHH:MM:SSZ.",
           call. = FALSE)
    }
    parsed <- as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    if (anyNA(parsed) ||
        !identical(format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), value)) {
      stop("Datetime histogram breaks contain an invalid instant.",
           call. = FALSE)
    }
    numeric <- as.numeric(parsed)
    public <- value
  } else {
    if (!is.numeric(value) || any(!is.finite(value))) {
      stop("Numeric histogram breaks must be finite numbers.", call. = FALSE)
    }
    numeric <- as.numeric(value)
    public <- numeric
  }
  if (length(numeric) < 2L || length(numeric) - 1L > policy$max_levels ||
      any(diff(numeric) <= 0)) {
    stop("Histogram breaks must be strictly increasing and within the ",
         "server-owned level cap.", call. = FALSE)
  }
  list(numeric = numeric, public = public)
}

# Parse only public specification and column-schema requirements. This must
# finish before provenance verification, person-token filtering, or any value
# conversion/reduction, so missing private values cannot change admission.
.dsomopDpParseSpec <- function(x, spec, policy) {
  if (!is.data.frame(x) || !.is_omop.table(x)) {
    stop("omopDpReleaseDS requires a server-created omop.table.",
         call. = FALSE)
  }
  if (!is.list(spec) || is.null(names(spec)) || anyNA(names(spec)) ||
      any(!nzchar(names(spec))) || anyDuplicated(names(spec))) {
    stop("The DP specification must be one uniquely named object.",
         call. = FALSE)
  }
  forbidden <- c("seed", "nonce", "epsilon", "delta", "fresh_noise",
                 "force", "reset", "privacy_epoch", "noise_root")
  if (any(names(spec) %in% forbidden)) {
    stop("DP seeds, allocations, epochs and reroll controls are server-owned.",
         call. = FALSE)
  }
  allowed <- c(
    "statistic", "variable", "levels", "breaks", "lower", "upper",
    "reducer", "max_contributions", "positive", "denominator", "order_by",
    "population_id"
  )
  if (any(!names(spec) %in% allowed)) {
    stop("The DP specification contains an unsupported field.", call. = FALSE)
  }
  statistic <- tolower(as.character(
    .dsomopDpSpecScalar(spec$statistic, "statistic")
  ))
  if (!statistic %in% .DSOMOP_DP_STATISTICS) {
    stop("Unsupported DP statistic.", call. = FALSE)
  }
  if (!is.null(spec$population_id)) {
    population_id <- .dsomopDpSpecScalar(
      spec$population_id, "population_id"
    )
    if (!is.character(population_id) ||
        !grepl("^[A-Za-z0-9][A-Za-z0-9._:@+/-]{0,255}\\z", population_id,
               perl = TRUE)) {
      stop("DP population_id must be one valid public compatibility label.",
           call. = FALSE)
    }
  }
  variable <- if (statistic %in% c("count", "bounded_record_count")) NULL else {
    .dsomopDpColumn(x, spec$variable, "variable")
  }
  reducer <- tolower(as.character(.dsomopDpSpecScalar(
    spec$reducer %||% "any", "reducer"
  )))
  max_contributions <- .dsomopDpSpecInteger(
    spec$max_contributions %||% 1L, "max_contributions", 1,
    policy$max_contributions
  )
  order_by <- .dsomopDpColumn(
    x, spec$order_by, "order_by", required = FALSE
  )
  common <- list(
    statistic = statistic, variable = variable, reducer = reducer,
    max_contributions = max_contributions, order_by = order_by
  )
  if (!is.null(order_by)) {
    .dsomopDpValueType(x[[order_by]], label = "DP order_by")
  }
  if (!is.null(variable)) {
    common$value_type <- .dsomopDpValueType(x[[variable]])
  }
  if (statistic %in% c("numeric_histogram", "bounded_mean")) {
    if (!common$value_type %in% c("integer", "numeric", "date", "datetime_utc")) {
      stop("This DP statistic requires a numeric, Date, or UTC datetime column.",
           call. = FALSE)
    }
    if (common$value_type %in% c("integer", "numeric")) {
      common$value_type <- "number"
    }
  }
  # The registered complete public basis of a categorical variable (public
  # metadata: the capsule's declared domains, custodian options, built-ins).
  capsule <- attr(x, "dsomop_dp_provenance", exact = TRUE)
  domain_of <- function() {
    domain <- .dsomopTypedDomain(capsule, variable)
    if (length(domain) > policy$max_levels) {
      stop("The registered public domain of '", variable, "' exceeds the ",
           "server-owned level cap.", call. = FALSE)
    }
    domain
  }
  if (statistic == "categorical_histogram") {
    levels <- .dsomopDpSpecAtomic(spec$levels, "levels")
    if (length(levels) < 1L || length(levels) > policy$max_levels) {
      stop("Categorical DP levels exceed the server-owned level cap.",
           call. = FALSE)
    }
    domain <- domain_of()
    # Requested levels are a view of the complete registered domain:
    # duplicates and order are normalized, invalid labels refused publicly.
    levels <- .dsomopTypedDomainView(levels, domain, variable,
                                     "categorical DP levels")
    common$domain <- domain
    reducer <- common$reducer
    if (!reducer %in% c(
      "presence", "mode", "first", "last", "records", "any"
    )) {
      stop("Unsupported categorical longitudinal reducer.", call. = FALSE)
    }
    if (identical(reducer, "any")) reducer <- "presence"
    if (!is.null(common$order_by) &&
        !reducer %in% c("first", "last", "records")) {
      stop("Categorical order_by is only valid for first/last/records reducers.",
           call. = FALSE)
    }
    if (identical(reducer, "records") && is.null(common$order_by)) {
      stop("Categorical records reducers require a public order_by column.",
           call. = FALSE)
    }
    common$levels <- levels
    common$reducer <- reducer
    # A per-person cap only matters for multi-label and record reducers.
    if (!reducer %in% c("presence", "records")) common$max_contributions <- 1L
  } else if (statistic == "bounded_record_count") {
    if (!identical(common$reducer, "records")) {
      stop("bounded_record_count requires reducer='records'.", call. = FALSE)
    }
    if (!is.null(common$order_by)) {
      stop("bounded_record_count does not use order_by.", call. = FALSE)
    }
  } else if (statistic == "bounded_distinct") {
    if (!identical(common$reducer, "distinct")) {
      stop("bounded_distinct requires reducer='distinct'.", call. = FALSE)
    }
    if (!is.null(common$order_by)) {
      stop("bounded_distinct uses server-owned canonical value ordering, not ",
           "order_by.", call. = FALSE)
    }
    # A distinct-label cardinality is over the fixed complete domain U: its
    # levels may only restate U, never select a sub-domain (B05).
    domain <- domain_of()
    levels <- .dsomopDpSpecAtomic(spec$levels, "levels", required = FALSE)
    if (!is.null(levels)) {
      levels <- .dsomopTypedDomainView(levels, domain, variable,
                                       "bounded_distinct levels")
      if (!identical(levels, domain)) {
        stop("bounded_distinct counts distinct labels over the complete ",
             "registered public domain of '", variable, "'; its levels may ",
             "only restate that domain.", call. = FALSE)
      }
    }
    common$domain <- domain
    common$levels <- domain
  } else if (statistic == "numeric_histogram") {
    breaks <- .dsomopDpBreaks(spec$breaks, common$value_type, policy)
    reducer <- common$reducer
    if (!reducer %in% c("any", "min", "max", "mean", "median", "first",
                        "last", "records")) {
      stop("Unsupported numeric longitudinal reducer.", call. = FALSE)
    }
    if (reducer == "any") reducer <- "mean"
    if (!is.null(common$order_by) &&
        !reducer %in% c("first", "last", "records")) {
      stop("Numeric order_by is only valid for first/last/records reducers.",
           call. = FALSE)
    }
    common$breaks <- breaks
    common$reducer <- reducer
    if (!identical(reducer, "records")) common$max_contributions <- 1L
  } else if (statistic == "bounded_mean") {
    if (!identical(common$value_type, "number")) {
      stop("bounded_mean currently requires a numeric column.", call. = FALSE)
    }
    lower <- .dsomopDpSpecNumber(spec$lower, "lower")
    upper <- .dsomopDpSpecNumber(spec$upper, "upper")
    span <- upper - lower
    if (lower >= upper || !is.finite(span)) {
      stop("DP mean bounds must have one finite positive span.",
           call. = FALSE)
    }
    reducer <- common$reducer
    if (!reducer %in% c("any", "min", "max", "mean", "median", "first",
                        "last")) {
      stop("Unsupported bounded-mean longitudinal reducer.", call. = FALSE)
    }
    if (reducer == "any") reducer <- "mean"
    if (!is.null(common$order_by) && !reducer %in% c("first", "last")) {
      stop("Bounded-mean order_by is only valid for first/last reducers.",
           call. = FALSE)
    }
    common$lower <- lower
    common$upper <- upper
    common$span <- span
    common$reducer <- reducer
    common$max_contributions <- 1L
  } else if (statistic == "binary_rate") {
    positive <- .dsomopDpSpecAtomic(spec$positive, "positive")
    positive <- sort(unique(enc2utf8(as.character(positive))), method = "radix")
    if (length(positive) < 1L || length(positive) > policy$max_levels ||
        any(!nzchar(positive)) ||
        any(nchar(positive, type = "bytes") > 256L)) {
      stop("binary_rate positive values must be non-empty and within the ",
           "server-owned level cap.",
           call. = FALSE)
    }
    domain <- domain_of()
    positive <- .dsomopTypedDomainView(positive, domain, variable,
                                       "binary_rate positive values")
    common$domain <- domain
    reducer <- common$reducer
    if (!reducer %in% c("any", "all", "first", "last")) {
      stop("Unsupported binary-rate longitudinal reducer.", call. = FALSE)
    }
    if (!is.null(common$order_by) && !reducer %in% c("first", "last")) {
      stop("Binary-rate order_by is only valid for first/last reducers.",
           call. = FALSE)
    }
    denominator_contract <- .dsomopDpSpecScalar(
      spec$denominator %||% "all_persons", "denominator"
    )
    if (!is.character(denominator_contract) ||
        !denominator_contract %in% c("all_persons", "nonmissing")) {
      stop("binary_rate denominator must be all_persons or nonmissing.",
           call. = FALSE)
    }
    if (reducer %in% c("any", "all") &&
        length(domain) > .DSOMOP_TYPED_MAX_PATTERN_DOMAIN) {
      stop("public_domain_too_large: an any/all binary rate needs the ",
           "complete label-set basis of '", variable, "', available for ",
           "registered domains of at most ", .DSOMOP_TYPED_MAX_PATTERN_DOMAIN,
           " labels; use reducer first or last.", call. = FALSE)
    }
    common$positive <- positive
    common$denominator <- denominator_contract
    common$max_contributions <- 1L
  }
  if (statistic %in% c("categorical_histogram", "numeric_histogram",
                       "bounded_mean", "binary_rate") &&
      common$reducer %in% c("first", "last") && is.null(order_by)) {
    stop("Longitudinal first/last reducers require a public order_by column.",
         call. = FALSE)
  }
  common
}

# The DP service derives its public identity from the connected resource. The
# core never binds it (connecting does not touch DP); the status endpoint binds
# the session's OMOP handle when the session has exactly one and no community
# tool has bound one yet.
.dsomopDpSessionHandle <- function() {
  if (!is.null(.dsomopDpState()$dp_handle)) return(NULL)
  handles <- list()
  for (frame in rev(sys.frames())) {
    keys <- grep("^\\.dsomop_handle_", ls(frame, all.names = TRUE),
                 value = TRUE)
    for (key in keys) {
      handles[[length(handles) + 1L]] <- get(key, envir = frame,
                                             inherits = FALSE)
    }
  }
  if (length(handles) == 0L) {
    keys <- grep("^handle_", ls(.dsomop_env, all.names = TRUE), value = TRUE)
    handles <- lapply(keys, get, envir = .dsomop_env, inherits = FALSE)
  }
  handles <- unique(handles)
  if (length(handles) == 1L) handles[[1L]] else NULL
}

#' Sticky privacy-noise service status (Aggregate)
#'
#' Returns the public protected-release contract (dsOMOP 2.10.3): the
#' mechanism, the reference calibration, the guarantee scope of each route
#' (\code{fixed_synopsis}: the declared bounded vector before the exact
#' support gate; \code{realised_sticky}: heuristic realised-contribution
#' reuse), the statistics with a fixed-synopsis contract and the registered
#' public scales. Secret key material, fingerprints and person tokens are never
#' returned. The only DP state is the noise root (\code{persistent_state =
#' "noise_root_only"}); a damaged root fails closed. There is no privacy
#' accountant, budget or call quota: \code{privacy_call_quota = "none"} and
#' \code{history_based_admission = FALSE}.
#' DP serves only the OHDSI community tools, so the status requires them to be
#' enabled (\code{dsomop.tools.enabled}). It derives the service identity from
#' the session's connected OMOP resource when no explicit
#' \code{dsomop.dp.domain} is configured. The public \code{snapshot_id} is
#' display metadata only (the deprecated \code{dsomop.dp.snapshot_id} option
#' replaces it; it enters no hash, provenance check or admission); fixed
#' synopses are keyed on the person-membership generation read inside each
#' request's snapshot, described by \code{data_generation},
#' \code{data_generation_basis} and \code{data_generation_residual} (what it
#' cannot see); \code{read_snapshot} is the read consistency of this server's
#' requests. The generation itself is never returned.
#'
#' The caller declares the protected-release payload protocol it implements
#' (\code{client_contract}, sent by dsOMOPClient 2.10.3 and later). Clients of
#' dsOMOP 2.10.2 and earlier call without it; they cannot validate or pool the
#' protected components, so the call is refused with an explicit upgrade
#' message (R08) instead of a status they would reject with an opaque contract
#' error. Their per-server analysis catalogue results are unaffected.
#'
#' @param client_contract The client's protected-release payload protocol
#'   (\code{"dsomop-protected-release-v1"}), or a list of protocols. Absent
#'   for clients older than dsOMOPClient 2.10.3.
#' @return Public DP service status, including the custodial \code{exclusive}
#'   flag (default FALSE) and \code{tools_mode}, the effective protection mode
#'   of the OHDSI community tools (\code{"auto"} by default, \code{"standard"}
#'   or \code{"dp"}; \code{dsomop.dp.exclusive = TRUE} with DP enabled selects
#'   \code{"dp"}). The core (loading tables, filters, factors, dsOMOP's own
#'   exploration) never depends on either.
#' @export
omopDpStatusDS <- function(client_contract = NULL) {
  .dsomopRequireCommunityTool(dp_routes = TRUE)
  .dsomopDpRequireCurrentClient(client_contract)
  state <- .dsomopDpState()
  .dsomopDpEnsureRuntime(.dsomopDpSessionHandle())
  status <- .dsomopDpPublicStatus(initialize = TRUE)
  status$exclusive <- .dsomopDpExclusive()
  status$tools_mode <- .dsomopToolsMode()
  status$supported_statistics <- .DSOMOP_DP_STATISTICS
  status$release_protocol <- .DSOMOP_RELEASE_PROTOCOL
  status$release_contract <- .DSOMOP_RELEASE_CONTRACT_VERSION
  status$guarantee_scopes <- as.list(.DSOMOP_RELEASE_GUARANTEE)
  status$fixed_synopsis_statistics <- c("count", "bounded_record_count",
                                        "bounded_mean")
  status$public_scales <- .dsomopTypedScales()
  status$history_based_admission <- FALSE
  status$longitudinal_contract <- "deterministic_person_bounding_v1"
  status$person_local_provenance_required <- TRUE
  status$provenance_protocol <- .DSOMOP_DP_PROVENANCE_PROTOCOL
  status$ohdsi_querylibrary <- .dsomopDpQueryLibraryStatus()
  # Read consistency of this server's requests (read_snapshot.R).
  status$read_snapshot <- .dsomopReadSnapshotKind(state$dp_handle)
  state$dp_status <- status
  status
}

#' Refuse DP status calls from clients older than dsOMOPClient 2.10.3
#'
#' Clients of dsOMOP 2.10.2 and earlier do not declare the protected-release
#' payload protocol; the status they would receive describes a contract they
#' reject with an opaque field error. This refusal tells the analyst what to
#' do. It depends only on the call's public argument.
#'
#' @param client_contract The caller's declared protocol(s), possibly
#'   DataSHIELD-encoded.
#' @return Invisible TRUE for a current client.
#' @keywords internal
.dsomopDpRequireCurrentClient <- function(client_contract) {
  declared <- tryCatch(unlist(.ds_arg(client_contract), use.names = FALSE),
                       error = function(e) NULL)
  if (is.character(declared) && .DSOMOP_RELEASE_PROTOCOL %in% declared) {
    return(invisible(TRUE))
  }
  stop("This server runs dsOMOP ", utils::packageVersion("dsOMOP"), ", whose ",
       "DP releases are protected components (contract ",
       .DSOMOP_RELEASE_PROTOCOL, ") that dsOMOPClient 2.10.2 and older cannot ",
       "validate or pool. Upgrade dsOMOPClient to 2.10.3 or later to use ",
       "ds.omop.dp.status() and ds.omop.dp.release() on this server; with an ",
       "older client the analysis catalogue keeps returning per-server ",
       "results, without pooling of protected results.", call. = FALSE)
}

#' Prepare person-local tables for differential-privacy releases (Assign)
#'
#' Community-tool counterpart of \code{\link{omopPlanExecuteDS}} for the sticky
#' DP channel. It runs the same typed plan through the core plan engine, which
#' is unchanged and never uses DP, and then seals each person-local output with
#' the authenticated provenance capsule that \code{\link{omopDpReleaseDS}}
#' requires: the canonical lineage of the plan and the dataset identity of the
#' connected resource. Ordinary table loading (\code{omopPlanExecuteDS}) never
#' carries DP provenance. Filtering, merging or otherwise manipulating a
#' prepared table drops its provenance, so a release population must be defined
#' inside the plan. Memory mode only. Requires the community tools
#' (\code{dsomop.tools.enabled}) and an available DP service.
#'
#' @inheritParams omopPlanExecuteDS
#' @return Invisible TRUE (sealed outputs are assigned into the caller's
#'   environment under the requested symbols).
#' @export
omopDpPlanExecuteDS <- function(omop_symbol, plan, out,
                                output_mode = "memory",
                                scope = NULL, combine = "union", ...) {
  .dsomopRequireCommunityTool(dp_routes = TRUE)
  if (!identical(.ds_arg(output_mode), "memory")) {
    stop("DP preparation creates in-memory tables only ",
         "(output_mode = 'memory').", call. = FALSE)
  }
  handle <- .getHandle(omop_symbol)
  # The caller's environment receives the sealed outputs.
  env <- parent.frame()
  extra <- list(...)
  # The plan's reads and the sealing share one request snapshot.
  .dsomopWithReadSnapshot(handle, .dsomopDpPlanExecuteSealed(
    handle, omop_symbol, plan, out, scope, combine, extra, env))
}

# The body of omopDpPlanExecuteDS, inside its request snapshot.
.dsomopDpPlanExecuteSealed <- function(handle, omop_symbol, plan, out, scope,
                                       combine, extra, env) {
  ready <- .dsomopToolsDpReady(handle)
  if (!isTRUE(ready$ready)) {
    stop("Preparing a table for DP releases needs the differential-privacy ",
         "service, which is not available on this server.", call. = FALSE)
  }
  decoded_plan <- .ds_arg(plan)
  decoded_out <- .ds_arg(out)
  scope_value <- .omopAnalysisScopeFromDots(.ds_arg(scope), extra)
  if (!is.null(scope_value)) {
    combine_value <- .ds_arg(combine)
    if (is.list(combine_value)) combine_value <- combine_value[[1]]
    if (is.null(decoded_plan$scope)) decoded_plan$scope <- list()
    decoded_plan$scope$tables_frames <- scope_value
    decoded_plan$scope$combine <- decoded_plan$scope$combine %||% combine_value
  }
  .omopAuditLog("omopDpPlanExecuteDS", list(
    outputs = names(decoded_out), scope_present = !is.null(scope_value)
  ))

  # The core plan engine assigns its outputs into its caller's environment:
  # call it as if from that environment, then seal what it created there.
  do.call(omopPlanExecuteDS,
          c(list(omop_symbol, plan, out, "memory", scope, combine), extra),
          envir = env)

  dataset_identity <- .dsomopDpDatasetIdentity(handle)
  # The key that produced the outputs' person tokens: sealing reverses them
  # once into the table's fixed release-person map (B01).
  person_key <- .personKey(handle)
  seal <- function(symbol, output_name, component = NULL) {
    if (!exists(symbol, envir = env, inherits = FALSE)) return(FALSE)
    value <- get(symbol, envir = env, inherits = FALSE)
    if (!is.data.frame(value)) return(FALSE)
    sealed <- .dsomopDpSealPlanOutput(
      value, decoded_plan, output_name,
      dataset_identity = dataset_identity, component = component,
      person_key = person_key,
      read_snapshot = .dsomopReadSnapshotLabel(handle)
    )
    assign(symbol, sealed, envir = env)
    !is.null(attr(sealed, "dsomop_dp_provenance", exact = TRUE))
  }
  for (output_name in names(decoded_out)) {
    symbol <- as.character(decoded_out[[output_name]])[[1L]]
    sealed <- c(
      seal(symbol, output_name),
      seal(paste0(symbol, ".msdata"), output_name, "msdata"),
      seal(paste0(symbol, ".transitionRef"), output_name, "transition_ref"),
      seal(paste0(symbol, ".events"), output_name, "events"),
      seal(paste0(symbol, ".riskSets"), output_name, "risk_sets")
    )
    if (!any(sealed)) {
      spec <- decoded_plan$outputs[[output_name]]
      hint <- if (identical(tolower(spec$type %||% ""), "baseline") &&
                  "age_at_index" %in% tolower(spec$derived %||% character(0))) {
        paste0(" Its age at index is protected using other persons' ages; ",
               "derive no age_at_index and use an age at a public reference ",
               "year (omop_variable_age(year = ...)) instead.")
      } else {
        paste0(" It must be a person-local table whose population the plan ",
               "defines (not, for example, a plan scoped by a workspace ",
               "table or a persistent cohort).")
      }
      stop("Plan output '", output_name, "' cannot be prepared for DP ",
           "releases.", hint, call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Release one protected, person-bounded statistic (Aggregate)
#'
#' The typed channel of the OHDSI community tools (dsOMOP 2.10.3). It requires
#' the community tools to be enabled (\code{dsomop.tools.enabled}), checked
#' before any argument is decoded or any provenance, handle or data is read,
#' and every attempt is written to the redacted audit log (operation, public
#' statistic, protection class and a generic outcome; never contribution
#' fingerprints, person tokens or data-derived details). The answer is a
#' deterministic function of the secret noise root and either a fixed public
#' basis (counts, capped record counts and registered-scale bounded means of
#' unscoped pinned loader tables, keyed on the person-membership generation
#' sealed with the table by its loader request:
#' \code{protection = "fixed_synopsis"}) or the realised bounded
#' contributions of the people each component contains
#' (\code{protection = "realised_sticky"}, a heuristic disclosure control
#' without a joint differential-privacy guarantee). Equivalent requests, dummy
#' levels, clamp-only outer breaks, harmless cap variants and lineage
#' cosmetics replay the same components; there is no budget, quota or
#' history-dependent admission. DP-only mode accepts only fixed-synopsis
#' releases.
#'
#' @param x A server-created, pseudonymous \code{omop.table} carrying its
#'   authenticated person-local provenance capsule.
#' @param spec Typed statistic specification, usually JSON-encoded by
#'   dsOMOPClient.
#' @return The protected payload: displayed values, the signed components for
#'   pooling, allocations, sensitivities, noise scales and the guarantee scope.
#' @export
omopDpReleaseDS <- function(x, spec) {
  # Every attempt, including one the community-tool gate refuses, is written
  # to the redacted audit log (public fields only; observational, never used
  # for admission).
  audit <- list(statistic = NA_character_, protection = NA_character_,
                contract = .DSOMOP_RELEASE_CONTRACT_VERSION,
                outcome = "refused")
  on.exit(.omopAuditLog("omopDpReleaseDS", audit), add = TRUE)
  .dsomopRequireCommunityTool(dp_routes = TRUE)
  spec <- .ds_arg(spec)
  if (is.list(spec) && is.character(spec$statistic) &&
      length(spec$statistic) == 1L &&
      tolower(spec$statistic) %in% .DSOMOP_DP_STATISTICS) {
    audit$statistic <- tolower(spec$statistic)
  }
  .dsomopDpEnsureRuntime(.dsomopDpSessionHandle())
  policy <- .dsomopDpPolicy()
  payload <- .dsomopTypedRelease(x, spec, policy)
  audit$protection <- payload$protection
  audit$settings <- payload[intersect(
    c("reducer", "max_contributions", "denominator", "value_type", "scale"),
    names(payload))]
  audit$outcome <- "released"
  payload
}
