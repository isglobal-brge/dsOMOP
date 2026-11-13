# Module: Sticky privacy releases
# Client construction, federation preflight, and pooling for the dedicated
# server-owned sticky-noise API.  The client deliberately has no seed, nonce,
# epsilon, epoch, reset, or force controls.

.DP_PROTOCOL <- "dsomop-dp-release-v3"
.DP_LEGACY_PROTOCOL <- "dsomop-dp-release-v2"
.DP_PRIVACY_GUARANTEE <-
  "sticky_person_bounded_discrete_laplace_per_release_v1"
.DP_PRIVACY_CONTRACT <- "fixed_per_release_snapshot_first_answer_v1"
.DP_LEGACY_PRIVACY_CONTRACT <- "fixed_per_release_semantic_prf_v1"
.DP_RELEASE_BINDING <- "snapshot_first_answer_v1"
.DP_SERVICE_CAPACITY <- "public_identity_reservations_v1"
# Seed-only binding (dsOMOP 2.10.0 servers): answers are a deterministic
# function of the server's secret root, the question and the data; nothing but
# the root is stored.
.DP_STATELESS_PRIVACY_CONTRACT <- "fixed_per_release_seed_data_versioned_v1"
.DP_STATELESS_BINDING <- "seed_data_versioned_v1"
.DP_VERSIONED_CONTRACT_FIELDS <- c(
  "protocol", "privacy_contract", "history_dependent", "persistent_state",
  "release_binding", "service_capacity"
)

.dp_scalar_character <- function(value, name, nullable = FALSE) {
  if (is.null(value) && nullable) return(NULL)
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) {
    stop(name, " must be one non-empty character value.", call. = FALSE)
  }
  enc2utf8(value)
}

.dp_scalar_number <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value)) {
    stop(name, " must be one finite number.", call. = FALSE)
  }
  as.numeric(value)
}

.dp_choice <- function(value, choices, name) {
  value <- .dp_scalar_character(value, name)
  if (!value %in% choices) {
    stop(name, " must be one of: ", paste(choices, collapse = ", "), ".",
         call. = FALSE)
  }
  value
}

.dp_assert_unused <- function(values, statistic) {
  used <- names(values)[!vapply(values, is.null, logical(1L))]
  if (length(used) > 0L) {
    stop("`", paste(used, collapse = "`, `"), "` ",
         if (length(used) == 1L) "is" else "are",
         " not valid for statistic='", statistic, "'.", call. = FALSE)
  }
  invisible(TRUE)
}

.dp_variable <- function(variable) {
  variable <- .dp_scalar_character(variable, "variable")
  if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", variable) ||
      grepl("^\\.[0-9]", variable)) {
    stop("variable must be one bare column name.", call. = FALSE)
  }
  variable
}

.dp_public_values <- function(value, name) {
  if (is.object(value) || !is.atomic(value) || length(value) < 1L ||
      anyNA(value) || !typeof(value) %in% c("logical", "integer", "double",
                                            "character") ||
      (is.numeric(value) && any(!is.finite(value)))) {
    stop(name, " must contain non-missing logical, finite numeric, or ",
         "character values.", call. = FALSE)
  }
  value <- enc2utf8(as.character(unname(value)))
  if (any(!nzchar(value)) || any(nchar(value, type = "bytes") > 256L)) {
    stop(name, " values must be non-empty and at most 256 bytes.",
         call. = FALSE)
  }
  sort(unique(value), method = "radix")
}

.dp_order_by <- function(order_by) {
  if (is.null(order_by)) return(NULL)
  .dp_variable(order_by)
}

.dp_population_id <- function(population_id, nullable = TRUE) {
  if (is.null(population_id) && isTRUE(nullable)) return(NULL)
  population_id <- .dp_scalar_character(population_id, "population_id")
  if (nchar(population_id, type = "bytes") > 256L ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._:@+/-]*$", population_id)) {
    stop("population_id must be at most 256 characters and use only letters, ",
         "numbers, '.', '_', ':', '@', '+', '/', or '-'; it must start with ",
         "a letter or number.", call. = FALSE)
  }
  population_id
}

.dp_histogram_breaks <- function(breaks) {
  if (is.numeric(breaks)) {
    if (length(breaks) < 2L || anyNA(breaks) || any(!is.finite(breaks)) ||
        any(diff(breaks) <= 0)) {
      stop("breaks must contain at least two strictly increasing finite ",
           "numbers.", call. = FALSE)
    }
    return(unname(as.numeric(breaks)))
  }
  if (!is.character(breaks) || length(breaks) < 2L || anyNA(breaks)) {
    stop("breaks must be finite numeric values, canonical ISO dates, or ",
         "canonical UTC datetimes.", call. = FALSE)
  }
  breaks <- enc2utf8(unname(breaks))
  date_pattern <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
  datetime_pattern <- paste0(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T",
    "[0-9]{2}:[0-9]{2}:[0-9]{2}Z$"
  )
  if (all(grepl(date_pattern, breaks))) {
    parsed <- as.Date(breaks)
    valid <- !anyNA(parsed) && identical(format(parsed, "%Y-%m-%d"), breaks)
    numeric <- as.numeric(parsed)
  } else if (all(grepl(datetime_pattern, breaks))) {
    parsed <- as.POSIXct(breaks, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    valid <- !anyNA(parsed) && identical(
      format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), breaks
    )
    numeric <- as.numeric(parsed)
  } else {
    stop("Character breaks must all use canonical YYYY-MM-DD dates or ",
         "YYYY-MM-DDTHH:MM:SSZ UTC datetimes.", call. = FALSE)
  }
  if (!isTRUE(valid) || any(diff(numeric) <= 0)) {
    stop("breaks contain invalid or non-increasing dates/times.",
         call. = FALSE)
  }
  breaks
}

#' Define a server-owned sticky privacy release
#'
#' Constructs the public semantic specification for a dedicated dsOMOP
#' sticky-noise release. Noise parameters and state are deliberately
#' absent: epsilon, seeds, nonces, and privacy epochs are owned by each data
#' custodian and cannot be supplied by the analyst. Every accepted semantic
#' release uses the server's fixed per-release epsilon and deterministic sticky
#' noise. Since dsOMOP 2.10.0, each answer is a deterministic function of the
#' server's secret root, the question, the protected statistic and a private
#' automatic database version: valid repeats return the same answer, and a
#' database reload that adds rows re-draws every answer. Servers 2.7.1 to 2.9.x
#' instead retain the first complete answer per public request, snapshot and
#' privacy epoch. There is no lifetime privacy budget or call counter.
#'
#' Repeated longitudinal records are reduced or capped per person. Public
#' categorical levels are sorted canonically. Numeric histogram breaks may be
#' finite numbers, ISO dates, or canonical UTC datetimes. The \code{"first"}
#' and \code{"last"} reducers require an explicit public \code{order_by}
#' column; row order is never treated as longitudinal time.
#'
#' @param statistic One of \code{"count"}, \code{"bounded_record_count"},
#'   \code{"categorical_histogram"}, \code{"numeric_histogram"},
#'   \code{"bounded_distinct"}, \code{"bounded_mean"}, or
#'   \code{"binary_rate"}.
#' @param variable Bare column name except for \code{"count"} and
#'   \code{"bounded_record_count"}.
#' @param levels Public, fixed character domain for a categorical histogram.
#'   The server returns every requested level, including zero-count levels.
#' @param breaks Public, fixed, strictly increasing finite numbers, canonical
#'   \code{YYYY-MM-DD} dates, or \code{YYYY-MM-DDTHH:MM:SSZ} UTC datetimes.
#' @param lower,upper Public finite bounds for a bounded mean.
#' @param reducer Per-person reducer. Categorical histograms accept
#'   \code{"presence"}, \code{"mode"}, \code{"first"}, \code{"last"}, and
#'   \code{"records"};
#'   numeric histograms also accept \code{"min"}, \code{"max"},
#'   \code{"mean"}, \code{"median"}, and \code{"records"}; bounded means
#'   accept the numeric one-value reducers; binary rates accept \code{"any"},
#'   \code{"all"}, \code{"first"}, and \code{"last"}. For compatibility,
#'   categorical \code{"any"} becomes \code{"presence"}, numeric and
#'   bounded-mean \code{"any"} becomes \code{"mean"}, and the two scalar
#'   bounded primitives canonicalize it to their sole reducer.
#' @param max_contributions Positive integer person-level contribution cap.
#'   Values above one apply to bounded record counts, bounded distinct counts,
#'   categorical \code{"presence"}/\code{"records"} histograms, and numeric
#'   \code{"records"} histograms.
#' @param positive Non-empty public value vector defining positive binary-rate
#'   records. It is canonicalized to sorted, unique character labels.
#' @param order_by Optional bare column defining longitudinal order. Required
#'   for \code{"first"} and \code{"last"}.
#' @param denominator Binary-rate denominator: all persons or only persons with
#'   a non-missing value.
#' @param population_id Optional public compatibility label for the population
#'   represented by \code{x}. When omitted, \code{ds.omop.dp.release()} derives
#'   it from the bare server symbol \code{x}. This label is metadata, not sticky
#'   release identity: changing it does not request or guarantee fresh noise.
#'   Do not put secrets or personal data in this public identifier.
#' @return A strictly validated \code{omop_privacy} specification.
#' @examples
#' omop_privacy("count")
#' omop_privacy("categorical_histogram", variable = "sex",
#'              levels = c("Female", "Male", "Unknown"))
#' omop_privacy("bounded_mean", variable = "value_as_number",
#'              lower = 0, upper = 300, reducer = "mean")
#' @export
omop_privacy <- function(statistic, variable = NULL, levels = NULL,
                         breaks = NULL, lower = NULL, upper = NULL,
                         reducer = "any", max_contributions = 1L,
                         positive = NULL, order_by = NULL,
                         denominator = c("all_persons", "nonmissing"),
                         population_id = NULL) {
  denominator_supplied <- !missing(denominator)
  statistics <- c(
    "count", "bounded_record_count", "categorical_histogram",
    "numeric_histogram", "bounded_distinct", "bounded_mean", "binary_rate"
  )
  statistic <- .dp_choice(statistic, statistics, "statistic")
  reducer <- .dp_scalar_character(reducer, "reducer")
  if (!is.numeric(max_contributions) || length(max_contributions) != 1L ||
      is.na(max_contributions) || !is.finite(max_contributions) ||
      max_contributions != floor(max_contributions) ||
      max_contributions < 1 || max_contributions > 10000) {
    stop("max_contributions must be one integer from 1 to 10000.",
         call. = FALSE)
  }
  max_contributions <- as.integer(max_contributions)
  order_by <- .dp_order_by(order_by)
  population_id <- .dp_population_id(population_id)

  if (identical(statistic, "count")) {
    .dp_assert_unused(list(variable = variable, levels = levels,
                           breaks = breaks, lower = lower, upper = upper,
                           positive = positive, order_by = order_by,
                           denominator = if (denominator_supplied) {
                             denominator
                           } else NULL), statistic)
    reducer <- .dp_choice(reducer, "any", "reducer")
    if (max_contributions != 1L) {
      stop("count has one contribution per person; max_contributions must be 1.",
           call. = FALSE)
    }
    spec <- list(statistic = statistic, reducer = reducer,
                 max_contributions = 1L)
  } else if (identical(statistic, "bounded_record_count")) {
    .dp_assert_unused(list(variable = variable, levels = levels,
                           breaks = breaks, lower = lower, upper = upper,
                           positive = positive, order_by = order_by,
                           denominator = if (denominator_supplied) {
                             denominator
                           } else NULL), statistic)
    reducer <- .dp_choice(reducer, c("any", "records"), "reducer")
    if (identical(reducer, "any")) reducer <- "records"
    spec <- list(statistic = statistic, reducer = reducer,
                 max_contributions = max_contributions)
  } else if (identical(statistic, "categorical_histogram")) {
    variable <- .dp_variable(variable)
    .dp_assert_unused(list(breaks = breaks, lower = lower, upper = upper,
                           positive = positive,
                           denominator = if (denominator_supplied) {
                             denominator
                           } else NULL), statistic)
    reducer <- .dp_choice(
      reducer,
      c("any", "presence", "mode", "first", "last", "records"),
      "reducer"
    )
    if (identical(reducer, "any")) reducer <- "presence"
    if (!is.character(levels) || length(levels) < 1L || anyNA(levels) ||
        any(!nzchar(levels)) ||
        any(nchar(levels, type = "bytes") > 256L) ||
        anyDuplicated(enc2utf8(levels))) {
      stop("levels must be a non-empty vector of unique, non-missing ",
           "character values.", call. = FALSE)
    }
    levels <- sort(enc2utf8(unname(levels)), method = "radix")
    if (reducer %in% c("mode", "first", "last") &&
        max_contributions != 1L) {
      stop("mode/first/last categorical reducers have one contribution per ",
           "person; max_contributions must be 1.", call. = FALSE)
    }
    if (reducer %in% c("first", "last", "records") && is.null(order_by)) {
      stop("first/last/records reducers require order_by.", call. = FALSE)
    }
    if (!is.null(order_by) &&
        !reducer %in% c("first", "last", "records")) {
      stop("order_by is only valid for first/last/records categorical ",
           "reducers.",
           call. = FALSE)
    }
    spec <- list(statistic = statistic, variable = variable,
                 levels = levels, reducer = reducer,
                 max_contributions = max_contributions)
    if (!is.null(order_by)) spec$order_by <- order_by
  } else if (identical(statistic, "numeric_histogram")) {
    variable <- .dp_variable(variable)
    .dp_assert_unused(list(levels = levels, lower = lower, upper = upper,
                           positive = positive,
                           denominator = if (denominator_supplied) {
                             denominator
                           } else NULL), statistic)
    reducer <- .dp_choice(
      reducer, c("any", "min", "max", "mean", "median", "first", "last",
                 "records"), "reducer"
    )
    if (identical(reducer, "any")) reducer <- "mean"
    breaks <- .dp_histogram_breaks(breaks)
    if (!identical(reducer, "records") && max_contributions != 1L) {
      stop("A reduced numeric histogram has one contribution per person; ",
           "max_contributions must be 1.", call. = FALSE)
    }
    if (reducer %in% c("first", "last") && is.null(order_by)) {
      stop("first/last reducers require order_by.", call. = FALSE)
    }
    if (!is.null(order_by) &&
        !reducer %in% c("first", "last", "records")) {
      stop("order_by is only valid for first/last/records numeric reducers.",
           call. = FALSE)
    }
    spec <- list(statistic = statistic, variable = variable,
                 breaks = breaks, reducer = reducer,
                 max_contributions = max_contributions)
    if (!is.null(order_by)) spec$order_by <- order_by
  } else if (identical(statistic, "bounded_distinct")) {
    variable <- .dp_variable(variable)
    .dp_assert_unused(list(breaks = breaks, lower = lower, upper = upper,
                           positive = positive, order_by = order_by,
                           denominator = if (denominator_supplied) {
                             denominator
                           } else NULL), statistic)
    reducer <- .dp_choice(reducer, c("any", "distinct"), "reducer")
    if (identical(reducer, "any")) reducer <- "distinct"
    levels <- .dp_public_values(levels, "levels")
    spec <- list(
      statistic = statistic, variable = variable, levels = levels,
      reducer = reducer, max_contributions = max_contributions
    )
  } else if (identical(statistic, "bounded_mean")) {
    variable <- .dp_variable(variable)
    .dp_assert_unused(list(levels = levels, breaks = breaks,
                           positive = positive,
                           denominator = if (denominator_supplied) {
                             denominator
                           } else NULL), statistic)
    reducer <- .dp_choice(
      reducer, c("any", "min", "max", "mean", "median", "first", "last"),
      "reducer"
    )
    if (identical(reducer, "any")) reducer <- "mean"
    lower <- .dp_scalar_number(lower, "lower")
    upper <- .dp_scalar_number(upper, "upper")
    if (lower >= upper || !is.finite(upper - lower)) {
      stop("lower and upper must define one finite positive span.",
           call. = FALSE)
    }
    if (max_contributions != 1L) {
      stop("bounded_mean has one contribution per person; ",
           "max_contributions must be 1.", call. = FALSE)
    }
    if (reducer %in% c("first", "last") && is.null(order_by)) {
      stop("first/last reducers require order_by.", call. = FALSE)
    }
    if (!is.null(order_by) && !reducer %in% c("first", "last")) {
      stop("order_by is only valid for first/last bounded-mean reducers.",
           call. = FALSE)
    }
    spec <- list(statistic = statistic, variable = variable,
                 lower = lower, upper = upper, reducer = reducer,
                 max_contributions = 1L)
    if (!is.null(order_by)) spec$order_by <- order_by
  } else {
    variable <- .dp_variable(variable)
    .dp_assert_unused(list(levels = levels, breaks = breaks, lower = lower,
                           upper = upper), statistic)
    reducer <- .dp_choice(reducer, c("any", "all", "first", "last"),
                          "reducer")
    if (length(denominator) > 1L &&
        identical(denominator, c("all_persons", "nonmissing"))) {
      denominator <- denominator[[1L]]
    }
    denominator <- .dp_choice(
      denominator, c("all_persons", "nonmissing"), "denominator"
    )
    positive <- .dp_public_values(positive, "positive")
    if (max_contributions != 1L) {
      stop("binary_rate has one contribution per person; ",
           "max_contributions must be 1.", call. = FALSE)
    }
    if (reducer %in% c("first", "last") && is.null(order_by)) {
      stop("first/last reducers require order_by.", call. = FALSE)
    }
    if (!is.null(order_by) && !reducer %in% c("first", "last")) {
      stop("order_by is only valid for first/last binary-rate reducers.",
           call. = FALSE)
    }
    spec <- list(statistic = statistic, variable = variable,
                 positive = positive, reducer = reducer,
                 max_contributions = 1L, denominator = denominator)
    if (!is.null(order_by)) spec$order_by <- order_by
  }

  if (!is.null(population_id)) spec$population_id <- population_id

  structure(spec, class = c("omop_privacy", "list"))
}

.dp_datasources <- function(datasources) {
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!is.list(datasources) || length(datasources) < 1L ||
      is.null(names(datasources)) || anyNA(names(datasources)) ||
      any(!nzchar(names(datasources))) || anyDuplicated(names(datasources))) {
    stop("datasources must be a non-empty, uniquely named list of DataSHIELD ",
         "connections.", call. = FALSE)
  }
  datasources
}

.dp_complete_aggregate <- function(datasources, expr, context) {
  expected <- names(datasources)
  result <- .ds_safe_aggregate(datasources, expr)
  errors <- attr(result, "ds_errors") %||% list()
  missing <- setdiff(expected, names(result))
  unexpected <- setdiff(names(result), expected)
  if (length(errors) > 0L || length(missing) > 0L ||
      length(unexpected) > 0L) {
    failed <- unique(c(names(errors), missing, unexpected))
    detail <- if (length(errors) > 0L) {
      paste(names(errors), unlist(errors, use.names = FALSE), sep = ": ",
            collapse = "; ")
    } else paste(failed, collapse = ", ")
    stop(context, " requires a complete federation; no partial result was ",
         "published. Affected server(s): ", paste(failed, collapse = ", "),
         if (nzchar(detail)) paste0(" (", detail, ")") else "", ".",
         call. = FALSE)
  }
  result[expected]
}

.dp_first_answer_contract <- function(status, server) {
  expected <- if (identical(status$release_binding, .DP_STATELESS_BINDING)) {
    list(
      history_dependent = FALSE,
      privacy_call_quota = "none",
      persistent_state = "noise_root_only",
      release_binding = .DP_STATELESS_BINDING,
      service_capacity = "none",
      privacy_contract = .DP_STATELESS_PRIVACY_CONTRACT
    )
  } else {
    list(
      history_dependent = TRUE,
      privacy_call_quota = "none",
      persistent_state = "noise_root_and_release_bindings",
      release_binding = .DP_RELEASE_BINDING,
      service_capacity = .DP_SERVICE_CAPACITY
    )
  }
  missing <- setdiff(names(expected), names(status))
  if (length(missing) > 0L) {
    stop("Server '", server, "' omitted DP contract field(s): ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  for (field in names(expected)) {
    if (!identical(status[[field]], expected[[field]])) {
      stop("Server '", server, "' returned an incoherent v3 DP ",
           "contract field '", field, "'.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.dp_status_shape <- function(status, server) {
  if (!is.list(status) || is.null(names(status)) || anyNA(names(status)) ||
      anyDuplicated(names(status))) {
    stop("Server '", server, "' returned a malformed DP status.",
         call. = FALSE)
  }
  required <- c("enabled", "ready", "sticky_noise", "protocol", "mechanism")
  if (!all(required %in% names(status))) {
    stop("Server '", server, "' returned a malformed DP status.",
         call. = FALSE)
  }
  # Servers before 2.7.0 expose the standard and DP channels together.
  if (!"exclusive" %in% names(status)) status$exclusive <- FALSE
  for (field in c("enabled", "ready", "sticky_noise", "exclusive")) {
    if (!is.logical(status[[field]]) || length(status[[field]]) != 1L ||
        is.na(status[[field]])) {
      stop("Server '", server, "' returned an invalid DP status field '",
           field, "'.", call. = FALSE)
    }
  }
  for (field in c("protocol", "mechanism")) {
    if (!is.character(status[[field]]) || length(status[[field]]) != 1L ||
        is.na(status[[field]]) || !nzchar(status[[field]])) {
      stop("Server '", server, "' returned an invalid DP status field '",
           field, "'.", call. = FALSE)
    }
  }
  if (!isTRUE(status$enabled)) {
    if (isTRUE(status$ready) || isTRUE(status$sticky_noise)) {
      stop("Server '", server, "' returned an incoherent disabled DP status.",
           call. = FALSE)
    }
    return(status)
  }
  if (!"snapshot_id" %in% names(status) ||
      !is.character(status$snapshot_id) || length(status$snapshot_id) != 1L ||
      is.na(status$snapshot_id) || !nzchar(status$snapshot_id)) {
    stop("Server '", server, "' returned a malformed DP status.",
         call. = FALSE)
  }
  if (nchar(status$snapshot_id, type = "bytes") > 256L ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._:@+-]*$", status$snapshot_id)) {
    stop("Server '", server, "' returned an invalid public snapshot_id.",
         call. = FALSE)
  }
  if (identical(status$protocol, .DP_PROTOCOL)) {
    .dp_first_answer_contract(status, server)
  }
  status
}

#' Inspect sticky privacy-release services
#'
#' Queries every selected DataSHIELD server. Unlike permissive exploration
#' helpers, this function never returns a partial federation: each requested
#' node must provide a well-formed status.
#'
#' Since dsOMOP 2.10.0 the v3 service is seed-only:
#' \code{release_binding = "seed_data_versioned_v1"},
#' \code{persistent_state = "noise_root_only"},
#' \code{service_capacity = "none"} and \code{history_dependent = FALSE}. Every
#' answer is a deterministic function of the server's secret root, the
#' question, the protected statistic and a private automatic database version,
#' so it is identical after restarts and on every R server sharing the root,
#' and a reload that adds rows re-draws every answer at once. Servers 2.7.1 to
#' 2.9.x report history-dependent first-answer binding instead:
#' \code{persistent_state = "noise_root_and_release_bindings"},
#' \code{release_binding = "snapshot_first_answer_v1"}, and
#' \code{service_capacity = "public_identity_reservations_v1"}. They retain one
#' complete answer per public request, snapshot and privacy epoch until the
#' custodian rotates the snapshot or epoch. Both contracts are accepted, also
#' mixed in one federation. There is no lifetime privacy budget or privacy call quota. Epsilon, the
#' privacy epoch and the secret root remain server-owned. Legacy v2 status is
#' inspectable and printed as legacy. Release calls require v3 by default.
#' During a staged upgrade, \code{options(dsomop.dp.allow_legacy_servers = TRUE)}
#' permits v2 releases, including mixed federations, with a warning naming each
#' legacy server. The old contract has no first-answer binding: an unrotated
#' data refresh can reveal whether a released statistic changed; see
#' isglobal-brge/dsOMOP#20. The release result records every site's contract.
#' Eligible input frames must also carry the server's authenticated
#' person-local provenance capsule; a copied class or plain attribute is not
#' sufficient.
#' DP concerns only the OHDSI community tools: connecting, loading tables,
#' filters, factors and dsOMOP's own exploration never use it. Since server
#' 2.8.0, \code{exclusive} is \code{FALSE} by default and, when a custodian
#' sets it, applies only to the community tools (servers 2.7.x applied it to
#' standard statistics). A missing \code{exclusive} field on older servers is
#' treated as \code{FALSE}. Servers 2.9.0 and later also report
#' \code{tools_mode}, the community tools' protection mode (\code{"auto"} by
#' default, \code{"standard"} or \code{"dp"}).
#' Each status contains the custodian's public \code{snapshot_id}. Federated
#' sites may legitimately report different snapshot identifiers.
#' Release preflight rejects either a repeated \code{noise_domain_id} or a
#' repeated server-owned logical \code{domain}. The second check also prevents
#' one logical privacy node from being pooled twice when its connections expose
#' different noise material.
#'
#' @param datasources Named DataSHIELD connection list. \code{NULL} uses
#'   \code{DSI::datashield.connections_find()}.
#' @return An \code{omop_dp_status} named list of per-server DP status records.
#' @examples
#' \dontrun{ds.omop.dp.status()}
#' @export
ds.omop.dp.status <- function(datasources = NULL) {
  datasources <- .dp_datasources(datasources)
  statuses <- .dp_complete_aggregate(
    datasources, call("omopDpStatusDS"), "DP status preflight"
  )
  for (server in names(statuses)) {
    statuses[[server]] <- .dp_status_shape(statuses[[server]], server)
  }
  structure(statuses, class = c("omop_dp_status", "list"))
}

#' Print the release policy on each DP server
#'
#' @param x An \code{omop_dp_status} returned by
#'   \code{\link{ds.omop.dp.status}}.
#' @param ... Unused.
#' @return \code{x}, invisibly.
#' @export
print.omop_dp_status <- function(x, ...) {
  cat("OMOP differential privacy status\n")
  for (server in names(x)) {
    status <- x[[server]]
    cat("  ", server, ": enabled=", status$enabled,
        " ready=", status$ready,
        " exclusive=", status$exclusive %||% FALSE,
        if (is.character(status$tools_mode)) {
          paste0(" tools_mode=", status$tools_mode)
        } else "",
        if (identical(status$protocol, .DP_LEGACY_PROTOCOL)) {
          " contract=legacy_v2 (no first-answer binding)"
        } else if (identical(status$protocol, .DP_PROTOCOL)) {
          paste0(" binding=", status$release_binding %||% "unavailable")
        } else "", "\n", sep = "")
  }
  invisible(x)
}

.dp_status_contract <- function(statuses, privacy) {
  allow_legacy <- isTRUE(getOption("dsomop.dp.allow_legacy_servers", FALSE))
  legacy <- vapply(statuses, function(status) {
    identical(status$protocol, .DP_LEGACY_PROTOCOL) &&
      identical(status$privacy_contract, .DP_LEGACY_PRIVACY_CONTRACT)
  }, logical(1L))
  for (server in names(statuses)) {
    status <- statuses[[server]]
    if (legacy[[server]] && !allow_legacy) {
      stop("Server '", server, "' provides the legacy v2 DP contract without ",
           "first-answer binding. Upgrade to dsOMOP 2.7.1 or later before ",
           "requesting releases; old and new contracts cannot be pooled.",
           call. = FALSE)
    }
    if (!identical(status$enabled, TRUE) || !identical(status$ready, TRUE) ||
        !identical(status$sticky_noise, TRUE)) {
      stop("Server '", server, "' does not provide an enabled, ready sticky ",
           "DP service.", call. = FALSE)
    }
  }

  required_fields <- c(
    "protocol", "canonical_protocol", "mechanism", "sampler",
    "privacy_guarantee", "person_local_provenance_required",
    "provenance_protocol", "adjacency", "privacy_contract",
    "release_epsilon", "release_delta", "max_levels", "max_contributions",
    "numeric_grid", "supported_statistics", "longitudinal_contract",
    "privacy_epoch", "domain", "noise_domain_id", "history_dependent",
    "privacy_call_quota", "persistent_state", "release_binding",
    "service_capacity"
  )
  for (server in names(statuses)) {
    site_required <- if (legacy[[server]]) {
      setdiff(required_fields, c("release_binding", "service_capacity"))
    } else required_fields
    missing <- setdiff(site_required, names(statuses[[server]]))
    if (length(missing) > 0L) {
      stop("Server '", server, "' omitted DP contract field(s): ",
           paste(missing, collapse = ", "), ".", call. = FALSE)
    }
    character_fields <- c(
      "protocol", "canonical_protocol", "mechanism", "sampler",
      "privacy_guarantee", "provenance_protocol", "adjacency",
      "privacy_contract", "longitudinal_contract", "domain",
      "noise_domain_id", "privacy_call_quota", "persistent_state",
      "release_binding", "service_capacity"
    )
    if (legacy[[server]]) {
      character_fields <- setdiff(
        character_fields, c("release_binding", "service_capacity")
      )
    }
    if (any(!vapply(statuses[[server]][character_fields], function(value) {
      is.character(value) && length(value) == 1L && !is.na(value) &&
        nzchar(value)
    }, logical(1L)))) {
      stop("Server '", server, "' returned invalid textual DP contract fields.",
           call. = FALSE)
    }
    numeric_fields <- c(
      "release_epsilon", "release_delta", "max_levels",
      "max_contributions", "numeric_grid", "privacy_epoch"
    )
    if (any(!vapply(statuses[[server]][numeric_fields], function(value) {
      is.numeric(value) && length(value) == 1L && !is.na(value) &&
        is.finite(value)
    }, logical(1L)))) {
      stop("Server '", server, "' returned invalid numeric DP contract fields.",
           call. = FALSE)
    }
    boolean_fields <- c("person_local_provenance_required", "history_dependent")
    if (any(!vapply(statuses[[server]][boolean_fields], function(value) {
      is.logical(value) && length(value) == 1L && !is.na(value)
    }, logical(1L)))) {
      stop("Server '", server, "' returned invalid DP contract flags.",
           call. = FALSE)
    }
    if (statuses[[server]]$release_epsilon <= 0 ||
        statuses[[server]]$release_delta != 0 ||
        any(vapply(statuses[[server]][c("max_levels", "max_contributions",
                                       "numeric_grid")], function(value) {
          value < 1 || value != floor(value)
        }, logical(1L))) || statuses[[server]]$privacy_epoch < 1 ||
        statuses[[server]]$privacy_epoch !=
          floor(statuses[[server]]$privacy_epoch)) {
      stop("Server '", server, "' returned out-of-range DP contract fields.",
           call. = FALSE)
    }
    first_answer <- identical(statuses[[server]]$protocol, .DP_PROTOCOL) &&
      statuses[[server]]$privacy_contract %in%
        c(.DP_PRIVACY_CONTRACT, .DP_STATELESS_PRIVACY_CONTRACT)
    if (!(first_answer || legacy[[server]]) ||
        !identical(statuses[[server]]$privacy_guarantee,
                   .DP_PRIVACY_GUARANTEE) ||
        (legacy[[server]] && !allow_legacy)) {
      stop("Server '", server, "' returned an unsupported DP release contract.",
           call. = FALSE)
    }
    identifiers <- statuses[[server]]
    valid_identifiers <-
      grepl("^[A-Za-z0-9][A-Za-z0-9._:-]{0,127}$", identifiers$domain) &&
      grepl("^dpn_[0-9a-f]{40}$", identifiers$noise_domain_id)
    if (!isTRUE(valid_identifiers)) {
      stop("Server '", server, "' returned an invalid DP noise domain.",
           call. = FALSE)
    }
    statistics <- statuses[[server]]$supported_statistics
    if (!is.character(statistics) || length(statistics) < 1L ||
        anyNA(statistics) || any(!nzchar(statistics)) ||
        anyDuplicated(statistics) || !privacy$statistic %in% statistics) {
      stop("Server '", server, "' does not support the requested bounded DP ",
           "statistic.", call. = FALSE)
    }
    if (!identical(statuses[[server]]$person_local_provenance_required, TRUE)) {
      stop("Server '", server, "' returned an incoherent v3 DP ",
           "provenance contract.", call. = FALSE)
    }
    if (legacy[[server]]) {
      status <- statuses[[server]]
      if (!identical(status$history_dependent, FALSE) ||
          !identical(status$privacy_call_quota, "none") ||
          !identical(status$persistent_state, "noise_root_only") ||
          !is.null(status$release_binding) ||
          !is.null(status$service_capacity)) {
        stop("Server '", server, "' returned an incoherent legacy v2 DP ",
             "contract.", call. = FALSE)
      }
    } else {
      .dp_first_answer_contract(statuses[[server]], server)
    }
  }
  noise_domains <- vapply(
    statuses, `[[`, character(1L), "noise_domain_id"
  )
  duplicated_domains <- unique(noise_domains[duplicated(noise_domains)])
  if (length(duplicated_domains) > 0L) {
    affected <- names(noise_domains)[noise_domains %in% duplicated_domains]
    stop("Selected servers share a DP noise domain (",
         paste(affected, collapse = ", "), "); refusing a duplicate ",
         "federated release from one logical privacy node.", call. = FALSE)
  }
  logical_domains <- vapply(statuses, `[[`, character(1L), "domain")
  duplicated_logical_domains <- unique(
    logical_domains[duplicated(logical_domains)]
  )
  if (length(duplicated_logical_domains) > 0L) {
    affected <- names(logical_domains)[
      logical_domains %in% duplicated_logical_domains
    ]
    stop("Selected servers share a DP logical domain (",
         paste(affected, collapse = ", "), "); refusing to pool the same ",
         "logical privacy node under different noise material.", call. = FALSE)
  }
  common_fields <- c(
    "protocol", "canonical_protocol", "mechanism", "sampler",
    "privacy_guarantee", "person_local_provenance_required",
    "provenance_protocol", "adjacency", "privacy_contract",
    "history_dependent", "privacy_call_quota", "persistent_state",
    "release_binding", "service_capacity", "longitudinal_contract"
  )
  # Only the exact, independently validated v2/v3 tuples may differ.
  common_fields <- setdiff(common_fields, .DP_VERSIONED_CONTRACT_FIELDS)
  for (field in common_fields) {
    values <- lapply(statuses, `[[`, field)
    first <- values[[1L]]
    if (!all(vapply(values[-1L], identical, logical(1L), first))) {
      stop("DP contract field '", field, "' differs across servers.",
           call. = FALSE)
    }
  }

  reference <- statuses[[1L]]
  for (field in .DP_VERSIONED_CONTRACT_FIELDS) {
    values <- lapply(statuses, `[[`, field)
    if (!all(vapply(values[-1L], identical, logical(1L), values[[1L]]))) {
      # A mixed federation must not inherit the first site's stronger claim.
      reference[field] <- list(NULL)
    }
  }
  common_max_contributions <- min(vapply(
    statuses, `[[`, numeric(1L), "max_contributions"
  ))
  common_max_levels <- min(vapply(
    statuses, `[[`, numeric(1L), "max_levels"
  ))
  if (privacy$max_contributions > common_max_contributions) {
    stop("The privacy specification exceeds the common server ",
         "max_contributions cap of ", common_max_contributions, ".",
         call. = FALSE)
  }
  if (privacy$statistic %in% c("categorical_histogram", "bounded_distinct") &&
      length(privacy$levels) > common_max_levels) {
    stop("The privacy specification exceeds the common server max_levels cap ",
         "of ", common_max_levels, ".", call. = FALSE)
  }
  if (identical(privacy$statistic, "numeric_histogram") &&
      length(privacy$breaks) - 1L > common_max_levels) {
    stop("The privacy specification exceeds the common server max_levels cap ",
         "of ", common_max_levels, ".", call. = FALSE)
  }
  if (identical(privacy$statistic, "binary_rate") &&
      length(privacy$positive) > common_max_levels) {
    stop("The privacy specification exceeds the common server max_levels cap ",
         "of ", common_max_levels, ".", call. = FALSE)
  }
  if (identical(privacy$statistic, "bounded_mean")) {
    grids <- vapply(statuses, `[[`, numeric(1L), "numeric_grid")
    if (length(unique(grids)) != 1L) {
      stop("DP contract field 'numeric_grid' differs across servers for a ",
           "bounded mean.", call. = FALSE)
    }
  }
  reference$max_contributions <- common_max_contributions
  reference$max_levels <- common_max_levels
  reference
}

.dp_release_shape <- function(value, server, privacy, contract) {
  common <- c(
    "protocol", "mechanism", "adjacency", "epsilon", "delta",
    "privacy_contract", "sticky", "statistic", "sampler", "sensitivity"
  )
  statistic_fields <- switch(
    privacy$statistic,
    count = "noisy_count",
    bounded_record_count = c(
      "noisy_count", "reducer", "max_contributions"
    ),
    categorical_histogram = c(
      "levels", "counts", "reducer", "max_contributions", "value_type"
    ),
    numeric_histogram = c(
      "breaks", "counts", "reducer", "max_contributions", "value_type",
      "interval_contract"
    ),
    bounded_distinct = c(
      "noisy_count", "reducer", "max_contributions", "domain_size",
      "selection_order", "value_type"
    ),
    bounded_mean = c(
      "noisy_count", "noisy_sum_grid", "value", "lower", "upper",
      "numeric_grid", "reducer", "value_type"
    ),
    binary_rate = c(
      "noisy_numerator", "noisy_denominator", "value", "reducer",
      "denominator", "value_type"
    )
  )
  allowed <- c(common, statistic_fields)
  if (!is.list(value) || is.null(names(value)) || anyNA(names(value)) ||
      anyDuplicated(names(value))) {
    stop("Server '", server, "' returned a malformed DP release.",
         call. = FALSE)
  }
  if (any(c("seed", "nonce", "noise_root", "raw_noise") %in% names(value))) {
    stop("Server '", server, "' returned forbidden private noise state.",
         call. = FALSE)
  }
  if (!setequal(names(value), allowed)) {
    stop("Server '", server, "' returned a malformed DP release schema.",
         call. = FALSE)
  }
  expected <- list(
    protocol = contract$protocol, mechanism = contract$mechanism,
    adjacency = contract$adjacency,
    privacy_contract = contract$privacy_contract, sticky = TRUE,
    sampler = contract$sampler,
    statistic = privacy$statistic
  )
  for (field in names(expected)) {
    observed <- value[[field]]
    target <- expected[[field]]
    if (is.numeric(target)) {
      valid <- is.numeric(observed) && length(observed) == 1L &&
        !is.na(observed) && is.finite(observed) && identical(as.numeric(observed),
                                                               target)
    } else {
      valid <- identical(observed, target)
    }
    if (!isTRUE(valid)) {
      stop("Server '", server, "' returned a DP release whose '", field,
           "' field does not match the preflight contract.", call. = FALSE)
    }
  }
  if (!is.numeric(value$epsilon) ||
      length(value$epsilon) != 1L || is.na(value$epsilon) ||
      !is.finite(value$epsilon) ||
      !identical(as.numeric(value$epsilon),
                 as.numeric(contract$release_epsilon)) ||
      !is.numeric(value$delta) || length(value$delta) != 1L ||
      is.na(value$delta) || !is.finite(value$delta) ||
      !identical(as.numeric(value$delta),
                 as.numeric(contract$release_delta))) {
    stop("Server '", server, "' returned DP parameters that differ from its ",
         "fixed per-release contract.",
         call. = FALSE)
  }
  value
}

.dp_nonnegative_integer <- function(value, server, field) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value < 0 || value > 2^53 - 1 ||
      value != floor(value)) {
    stop("Server '", server, "' returned invalid DP field '", field, "'.",
         call. = FALSE)
  }
  as.numeric(value)
}

.dp_count_vector <- function(value, server, expected_length) {
  if (!is.numeric(value) || length(value) != expected_length || anyNA(value) ||
      any(!is.finite(value)) || any(value < 0) || any(value > 2^53 - 1) ||
      any(value != floor(value))) {
    stop("Server '", server, "' returned invalid DP histogram counts.",
         call. = FALSE)
  }
  as.numeric(value)
}

.dp_payload_equal <- function(value, expected, server, field) {
  equal <- if (is.numeric(expected)) {
    is.numeric(value) && length(value) == length(expected) && !anyNA(value) &&
      all(is.finite(value)) && identical(as.numeric(value),
                                         as.numeric(expected))
  } else {
    identical(value, expected)
  }
  if (!isTRUE(equal)) {
    stop("Server '", server, "' returned a DP payload contract drift in '",
         field, "'.", call. = FALSE)
  }
  invisible(TRUE)
}

.dp_validate_sensitivity <- function(value, server, privacy, contract) {
  expected <- switch(
    privacy$statistic,
    count = list(l1 = 1, unit = "person"),
    bounded_record_count = list(
      l1 = privacy$max_contributions, unit = "person"
    ),
    categorical_histogram = list(
      l1 = privacy$max_contributions, unit = "person"
    ),
    numeric_histogram = list(
      l1 = privacy$max_contributions, unit = "person"
    ),
    bounded_distinct = list(
      l1 = privacy$max_contributions, unit = "person"
    ),
    bounded_mean = list(
      count = 1, sum_grid = as.numeric(contract$numeric_grid),
      allocation = "sequential_half_epsilon", unit = "person"
    ),
    binary_rate = list(
      numerator = 1, denominator = 1,
      allocation = "sequential_half_epsilon", unit = "person"
    )
  )
  if (!is.list(value) || is.null(names(value)) || anyNA(names(value)) ||
      anyDuplicated(names(value)) || !setequal(names(value), names(expected))) {
    stop("Server '", server, "' returned malformed DP sensitivity metadata.",
         call. = FALSE)
  }
  for (field in names(expected)) {
    .dp_payload_equal(value[[field]], expected[[field]], server,
                      paste0("sensitivity.", field))
  }
  invisible(TRUE)
}

.dp_validate_estimate <- function(value, expected, lower, upper, server,
                                  statistic) {
  valid <- if (is.null(expected)) {
    is.null(value)
  } else {
    is.numeric(value) && length(value) == 1L && !is.na(value) &&
      is.finite(value) && value >= lower && value <= upper &&
      isTRUE(all.equal(as.numeric(value), as.numeric(expected),
                       tolerance = 1e-12))
  }
  if (!isTRUE(valid)) {
    stop("Server '", server, "' returned an incoherent ", statistic,
         " estimate.", call. = FALSE)
  }
  invisible(TRUE)
}

.dp_validate_payload <- function(value, server, privacy, contract) {
  value <- .dp_release_shape(value, server, privacy, contract)
  statistic <- privacy$statistic
  .dp_validate_sensitivity(value$sensitivity, server, privacy, contract)
  if (identical(statistic, "count")) {
    value$noisy_count <- .dp_nonnegative_integer(
      value$noisy_count, server, "noisy_count"
    )
  } else if (identical(statistic, "bounded_record_count")) {
    value$noisy_count <- .dp_nonnegative_integer(
      value$noisy_count, server, "noisy_count"
    )
    .dp_payload_equal(value$reducer, "records", server, "reducer")
    .dp_payload_equal(value$max_contributions,
                      privacy$max_contributions, server,
                      "max_contributions")
  } else if (identical(statistic, "categorical_histogram")) {
    .dp_payload_equal(value$value_type, "categorical_utf8_v1", server,
                      "value_type")
    .dp_payload_equal(value$levels, privacy$levels, server, "levels")
    .dp_payload_equal(value$reducer, privacy$reducer, server, "reducer")
    .dp_payload_equal(value$max_contributions,
                      privacy$max_contributions, server,
                      "max_contributions")
    value$counts <- .dp_count_vector(value$counts, server,
                                     length(privacy$levels))
  } else if (identical(statistic, "numeric_histogram")) {
    expected_type <- if (is.numeric(privacy$breaks)) {
      "number"
    } else if (grepl("T", privacy$breaks[[1L]], fixed = TRUE)) {
      "datetime_utc"
    } else {
      "date"
    }
    .dp_payload_equal(value$value_type, expected_type, server, "value_type")
    .dp_payload_equal(value$breaks, privacy$breaks, server, "breaks")
    .dp_payload_equal(value$reducer, privacy$reducer, server, "reducer")
    .dp_payload_equal(value$max_contributions,
                      privacy$max_contributions, server,
                      "max_contributions")
    .dp_payload_equal(
      value$interval_contract, "left_closed_right_open_last_closed",
      server, "interval_contract"
    )
    value$counts <- .dp_count_vector(value$counts, server,
                                     length(privacy$breaks) - 1L)
  } else if (identical(statistic, "bounded_distinct")) {
    value$noisy_count <- .dp_nonnegative_integer(
      value$noisy_count, server, "noisy_count"
    )
    .dp_payload_equal(value$value_type, "categorical_utf8_v1", server,
                      "value_type")
    .dp_payload_equal(value$reducer, "distinct", server, "reducer")
    .dp_payload_equal(value$max_contributions,
                      privacy$max_contributions, server,
                      "max_contributions")
    .dp_payload_equal(value$domain_size, length(privacy$levels), server,
                      "domain_size")
    .dp_payload_equal(value$selection_order,
                      "canonical_utf8_value_radix", server,
                      "selection_order")
  } else if (identical(statistic, "bounded_mean")) {
    .dp_payload_equal(value$value_type, "number", server, "value_type")
    for (field in c("noisy_count", "noisy_sum_grid")) {
      value[[field]] <- .dp_nonnegative_integer(value[[field]], server, field)
    }
    for (field in c("lower", "upper", "numeric_grid")) {
      observed <- value[[field]]
      target <- if (identical(field, "numeric_grid")) {
        as.numeric(contract$numeric_grid)
      } else privacy[[field]]
      if (!is.numeric(observed) || length(observed) != 1L || is.na(observed) ||
          !is.finite(observed) || !identical(as.numeric(observed), target)) {
        stop("Server '", server, "' returned a bounded-mean contract drift in '",
             field, "'.", call. = FALSE)
      }
    }
    .dp_payload_equal(value$reducer, privacy$reducer, server, "reducer")
    if (value$noisy_sum_grid > value$noisy_count * value$numeric_grid) {
      stop("Server '", server, "' returned infeasible bounded-mean ",
           "sufficient statistics.", call. = FALSE)
    }
    expected_value <- if (value$noisy_count > 0) {
      privacy$lower + (privacy$upper - privacy$lower) *
        value$noisy_sum_grid / (value$noisy_count * value$numeric_grid)
    } else NULL
    .dp_validate_estimate(
      value$value, expected_value, privacy$lower, privacy$upper, server,
      "bounded-mean"
    )
  } else {
    .dp_payload_equal(value$value_type, "categorical_utf8_v1", server,
                      "value_type")
    for (field in c("noisy_numerator", "noisy_denominator")) {
      value[[field]] <- .dp_nonnegative_integer(value[[field]], server, field)
    }
    .dp_payload_equal(value$reducer, privacy$reducer, server, "reducer")
    .dp_payload_equal(value$denominator, privacy$denominator, server,
                      "denominator")
    if (value$noisy_numerator > value$noisy_denominator) {
      stop("Server '", server, "' returned an infeasible binary rate.",
           call. = FALSE)
    }
    expected_value <- if (value$noisy_denominator > 0) {
      value$noisy_numerator / value$noisy_denominator
    } else NULL
    .dp_validate_estimate(
      value$value, expected_value, 0, 1, server, "binary-rate"
    )
  }
  if (!statistic %in% c("count", "bounded_record_count") &&
      (!is.character(value$value_type) || length(value$value_type) != 1L ||
       is.na(value$value_type) || !nzchar(value$value_type))) {
    stop("Server '", server, "' returned an invalid DP value type.",
         call. = FALSE)
  }
  value
}

.dp_histogram_labels <- function(breaks) {
  lower <- utils::head(breaks, -1L)
  upper <- utils::tail(breaks, -1L)
  paste0("[", lower, ", ", upper,
         c(rep.int(")", max(0L, length(lower) - 1L)), "]"))
}

.dp_format_histogram <- function(labels, counts, format, numeric_breaks = NULL) {
  if (identical(format, "raw")) {
    value <- list(statistic = if (is.null(numeric_breaks)) {
      "categorical_histogram"
    } else "numeric_histogram", counts = counts)
    if (is.null(numeric_breaks)) value$levels <- labels else {
      value$breaks <- numeric_breaks
      value$interval_contract <- "left_closed_right_open_last_closed"
    }
    return(value)
  }
  if (identical(format, "vector")) {
    return(stats::setNames(counts, labels))
  }
  if (identical(format, "wide")) {
    return(as.data.frame(
      as.list(stats::setNames(counts, labels)), check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  if (is.null(numeric_breaks)) {
    return(data.frame(level = labels, noisy_count = counts,
                      stringsAsFactors = FALSE))
  }
  data.frame(lower = utils::head(numeric_breaks, -1L),
             upper = utils::tail(numeric_breaks, -1L),
             noisy_count = counts, stringsAsFactors = FALSE)
}

.dp_pool_release <- function(per_site, privacy, contract, format = "long") {
  statistic <- privacy$statistic
  if (statistic %in% c(
    "count", "bounded_record_count", "bounded_distinct"
  )) {
    values <- vapply(per_site, `[[`, numeric(1L), "noisy_count")
    result <- list(statistic = statistic, noisy_count = sum(values))
    if (identical(statistic, "bounded_distinct")) {
      result$pooling <- "sum_of_site_local_distinct_cardinalities"
    }
    return(result)
  }
  if (identical(statistic, "categorical_histogram")) {
    counts <- Reduce(`+`, lapply(per_site, `[[`, "counts"))
    return(.dp_format_histogram(privacy$levels, counts, format))
  }
  if (identical(statistic, "numeric_histogram")) {
    counts <- Reduce(`+`, lapply(per_site, `[[`, "counts"))
    return(.dp_format_histogram(
      .dp_histogram_labels(privacy$breaks), counts, format,
      numeric_breaks = privacy$breaks
    ))
  }
  if (identical(statistic, "bounded_mean")) {
    noisy_count <- sum(vapply(per_site, `[[`, numeric(1L), "noisy_count"))
    noisy_sum <- sum(vapply(per_site, `[[`, numeric(1L), "noisy_sum_grid"))
    estimate <- if (noisy_count > 0) {
      privacy$lower + (privacy$upper - privacy$lower) *
        (noisy_sum / noisy_count) / as.numeric(contract$numeric_grid)
    } else NA_real_
    if (is.finite(estimate)) {
      estimate <- min(privacy$upper, max(privacy$lower, estimate))
    }
    return(list(statistic = statistic, estimate = estimate,
                noisy_count = noisy_count, noisy_sum_grid = noisy_sum,
                lower = privacy$lower, upper = privacy$upper,
                numeric_grid = as.numeric(contract$numeric_grid)))
  }
  numerator <- sum(vapply(per_site, `[[`, numeric(1L), "noisy_numerator"))
  denominator <- sum(vapply(per_site, `[[`, numeric(1L), "noisy_denominator"))
  estimate <- if (denominator > 0) numerator / denominator else NA_real_
  if (is.finite(estimate)) estimate <- min(1, max(0, estimate))
  list(statistic = statistic, estimate = estimate,
       noisy_numerator = numerator, noisy_denominator = denominator)
}

#' Request a sticky privacy release
#'
#' Performs a complete-federation preflight, requests the same typed release
#' from every node, verifies the returned mechanism contract, and optionally
#' pools only the noisy sufficient statistics. A failure at any site stops the
#' call without publishing another site's value. Retrying the identical
#' request returns the same complete answer. The default v3 contract requires
#' dsOMOP 2.7.1 or later.
#' For a staged upgrade only, the client option
#' \code{dsomop.dp.allow_legacy_servers} (default \code{FALSE}) can be set to
#' \code{TRUE} to permit legacy v2 servers, including mixed federations. Every
#' legacy server is named in a warning: no first-answer binding: an unrotated
#' data refresh can reveal whether a released statistic changed; see
#' isglobal-brge/dsOMOP#20. The semantics below apply to v3 sites.
#' Public request identity is server-owned and binds authenticated canonical
#' dataset/recipe lineage, the typed statistic and mechanism contract, public
#' \code{snapshot_id}, and privacy epoch. The analyst's
#' \code{population_id} compatibility label and server symbol alias do not
#' participate. Servers 2.10.0 (seed-only) derive the answer from that identity,
#' the private bounded-statistic fingerprint and a private automatic database
#' version and store nothing: a reload that adds rows re-draws every answer.
#' Servers 2.7.1 to 2.9.x (first-answer) store the first answer and return it
#' to valid later requests even after unrotated source updates, until the
#' custodian rotates the snapshot or epoch; their answers can remain stale, and
#' different requests first answered at different times need not represent one
#' coherent source snapshot.
#' For multiple sites, pooling a non-count statistic additionally requires one
#' compatible public dsOMOP harmonization contract for age grids, date
#' semantics, calendar-day granularity, UTC handling, week start, and
#' operational caps. Per-site output and pooled distinct-person counts do not
#' depend on those value semantics and therefore do not require that unrelated
#' contract.
#' Every input must have been produced by an audited person-local server path
#' and carry its authenticated content-bound provenance capsule.
#'
#' Every answer uses the fixed epsilon reported by its server and delta zero.
#' There is no lifetime privacy budget or privacy call quota. Composition
#' metadata describes only the participating sites in the current federated
#' release. Repeating a question adds no observation; no full temporal
#' transcript DP or timing, admission, or rotation guarantee is claimed.
#'
#' @param x One bare DataSHIELD symbol containing a server-side
#'   \code{omop.table}.
#' @param privacy An \code{omop_privacy} specification. If it does not contain
#'   an explicit \code{population_id}, the bare symbol \code{x} is used as its
#'   public compatibility label. This label does not control sticky identity.
#' @param datasources Named DataSHIELD connection list. \code{NULL} uses
#'   \code{DSI::datashield.connections_find()}.
#' @param pool Logical; pool the complete set of noisy site releases.
#' @param format Client-only pooled-result format: long data frame, one-row
#'   wide data frame, named vector, or raw list. Histogram releases support all
#'   four forms; other statistics retain their typed list. This argument never
#'   enters the server specification or sticky-release identity.
#' @param type Optional result view: \code{"split"}, \code{"combine"}, or
#'   \code{"both"}. When omitted, the legacy \code{pool} argument is preserved:
#'   \code{TRUE} means both views and \code{FALSE} means split only.
#' @return A \code{dsomop_result}. The \code{meta$privacy} record reports the
#'   effective population label, a named public snapshot map, fixed per-site
#'   epsilon and parallel cross-site composition for this release. Federated
#'   nodes are modeled as separate populations, so the combined epsilon and
#'   delta are the maxima of their per-site values. Multi-site results also
#'   carry \code{meta$harmonization} when non-count values were pooled.
#'   \code{meta$privacy$per_site_contract} records each server's protocol and
#'   state contract, including for \code{type = "combine"}.
#'   \code{legacy_servers} names all v2 sites and \code{mixed_contracts} reports
#'   whether servers with different DP contracts were combined (v2 and v3, or
#'   the seed-only and first-answer bindings). Shared contract fields that differ across
#'   sites are \code{NULL}; consult \code{per_site_contract} for their values.
#'   Legacy warnings are also retained in \code{meta$warnings}.
#' @examples
#' \dontrun{
#' p <- omop_privacy("count")
#' ds.omop.dp.release("analysis_table", p)
#' }
#' @export
ds.omop.dp.release <- function(x, privacy, datasources = NULL, pool = TRUE,
                               format = c("long", "wide", "vector", "raw"),
                               type = NULL) {
  pool_missing <- missing(pool)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x) ||
      !grepl("^[A-Za-z.][A-Za-z0-9._]*$", x) || grepl("^\\.[0-9]", x)) {
    stop("x must be one bare DataSHIELD symbol.", call. = FALSE)
  }
  if (!inherits(privacy, "omop_privacy") || !is.list(privacy)) {
    stop("privacy must be created by omop_privacy().", call. = FALSE)
  }
  if (!is.logical(pool) || length(pool) != 1L || is.na(pool)) {
    stop("pool must be TRUE or FALSE.", call. = FALSE)
  }
  result_type <- .resolve_result_type(
    type, pool = pool, pool_missing = pool_missing, default_type = "both"
  )
  want_combine <- .result_type_wants_combine(result_type)
  if (length(format) > 1L &&
      identical(format, c("long", "wide", "vector", "raw"))) {
    format <- format[[1L]]
  }
  format <- .dp_choice(format, c("long", "wide", "vector", "raw"),
                       "format")
  # Reconstructing also rejects forged objects with the right S3 class.
  rebuilt <- do.call(omop_privacy, unclass(privacy))
  if (!identical(unclass(rebuilt), unclass(privacy))) {
    stop("privacy is not a canonical omop_privacy specification.",
         call. = FALSE)
  }
  population_id <- .dp_population_id(privacy$population_id %||% x,
                                     nullable = FALSE)
  server_privacy <- unclass(privacy)
  server_privacy$population_id <- population_id

  datasources <- .dp_datasources(datasources)
  statuses <- ds.omop.dp.status(datasources)
  contract <- .dp_status_contract(statuses, privacy)
  harmonization <- NULL
  count_like <- privacy$statistic %in% c(
    "count", "bounded_record_count", "bounded_distinct"
  )
  if (length(datasources) > 1L && want_combine && !count_like) {
    disclosure <- .dp_complete_aggregate(
      datasources, call("omopDisclosureSettingsDS"),
      "DP harmonization preflight"
    )
    harmonization <- .federated_harmonization_contract(
      disclosure, expected_servers = names(datasources), fail = TRUE
    )
  }
  legacy_servers <- names(statuses)[vapply(statuses, function(status) {
    identical(status$protocol, .DP_LEGACY_PROTOCOL)
  }, logical(1L))]
  legacy_warnings <- vapply(legacy_servers, function(server) {
    paste0(
      "Legacy DP server '", server, "': no first-answer binding: an ",
      "unrotated data refresh can reveal whether a released statistic ",
      "changed; see isglobal-brge/dsOMOP#20."
    )
  }, character(1L), USE.NAMES = FALSE)
  for (legacy_warning in legacy_warnings) {
    warning(legacy_warning, call. = FALSE)
  }
  raw <- .dp_complete_aggregate(
    datasources,
    call("omopDpReleaseDS", as.name(x), .ds_encode(server_privacy)),
    "DP release"
  )
  for (server in names(raw)) {
    raw[[server]] <- .dp_validate_payload(
      raw[[server]], server, privacy, statuses[[server]]
    )
  }
  if (want_combine && !privacy$statistic %in% c(
    "count", "bounded_record_count"
  )) {
    value_types <- vapply(raw, `[[`, character(1L), "value_type")
    if (length(unique(value_types)) != 1L) {
      stop("DP release value_type differs across servers; no pooled or ",
           "partial result was published.", call. = FALSE)
    }
  }

  pooled <- if (want_combine) {
    .dp_pool_release(raw, privacy, contract, format = format)
  } else NULL
  epsilons <- vapply(raw, function(value) as.numeric(value$epsilon), numeric(1L))
  deltas <- vapply(raw, function(value) as.numeric(value$delta), numeric(1L))
  snapshot_ids <- vapply(statuses, `[[`, character(1L), "snapshot_id")
  warnings <- legacy_warnings
  if (want_combine && length(statuses) > 1L &&
      identical(privacy$statistic, "bounded_distinct")) {
    warnings <- c(warnings, paste0(
      "Pooled bounded_distinct is the sum of noisy site-local cardinalities, ",
      "not the cardinality of the cross-site concept union; use per-site ",
      "results when that additive estimand is not intended."
    ))
  }
  result <- dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(
      call_code = .build_code("ds.omop.dp.release", x = x,
                              privacy = "<omop_privacy>",
                              pool = if (is.null(type)) pool else NULL,
                              type = if (is.null(type)) NULL else result_type,
                              format = format),
      scope = if (want_combine) "pooled" else "per_site",
      warnings = warnings
    )
  )
  result[["meta"]]$privacy <- list(
    statistic = privacy$statistic,
    population_id = population_id,
    snapshot_id = snapshot_ids,
    format = format,
    mechanism = contract$mechanism,
    sampler = contract$sampler,
    provenance_protocol = contract$provenance_protocol,
    person_local_provenance_required =
      contract$person_local_provenance_required,
    privacy_guarantee = contract$privacy_guarantee,
    privacy_contract = contract$privacy_contract,
    adjacency = contract$adjacency,
    history_dependent = contract$history_dependent,
    privacy_call_quota = contract$privacy_call_quota,
    persistent_state = contract$persistent_state,
    release_binding = contract$release_binding,
    service_capacity = contract$service_capacity,
    per_site_contract = lapply(statuses, function(status) {
      fields <- c(.DP_VERSIONED_CONTRACT_FIELDS, "privacy_call_quota")
      stats::setNames(lapply(fields, function(field) status[[field]]), fields)
    }),
    mixed_contracts = length(unique(vapply(statuses, function(status) {
      paste(status$protocol, status$privacy_contract, sep = "/")
    }, character(1L)))) > 1L,
    legacy_servers = legacy_servers,
    privacy_epoch = vapply(
      statuses, `[[`, numeric(1L), "privacy_epoch"
    ),
    sticky = TRUE,
    composition_scope = "current_federated_release",
    composition = "parallel_across_sites",
    per_site_epsilon = epsilons,
    per_site_delta = deltas,
    conservative_epsilon = max(epsilons),
    conservative_delta = max(deltas)
  )
  result[["meta"]]$harmonization <- harmonization
  .result_type_view(
    result, result_type,
    "No complete disclosure-safe pooled DP release was available."
  )
}

#' Prepare a person-local table for sticky DP releases
#'
#' Runs a typed \code{omop_recipe} or \code{omop_plan} on every server through
#' the community-tool method \code{omopDpPlanExecuteDS} (dsOMOP 2.9.1 or later),
#' which builds the tables with the same plan engine as
#' \code{\link{ds.omop.plan.execute}} and seals them with the authenticated DP
#' provenance that \code{\link{ds.omop.dp.release}} requires. Ordinary table
#' loading never involves DP; use this function only to define the population
#' and the derived variables of a DP release (for example an age at a public
#' reference year). The analyst chooses that population; the data custodian
#' owns every privacy setting (epsilon, caps, whether DP is available).
#'
#' Prepared tables keep their concept identifiers as stored (no concept-factor
#' harmonization), and filtering, merging or otherwise manipulating them drops
#' the DP provenance: define the whole release population in the recipe.
#'
#' @param x An \code{omop_recipe} or \code{omop_plan}.
#' @param out Output symbols, as in \code{\link{recipe_execute}} and
#'   \code{\link{ds.omop.plan.execute}}.
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return The named output-to-symbol mapping, invisibly.
#' @examples
#' \dontrun{
#' recipe <- omop_recipe(
#'   variables = omop_variable_age(name = "age_at_2025", year = 2025),
#'   outputs = omop_output(name = "ages", type = "wide")
#' )
#' ds.omop.dp.prepare(recipe, out = c(ages = "DP_ages"))
#' ds.omop.dp.release("DP_ages", omop_privacy(
#'   "numeric_histogram", variable = "age_at_2025",
#'   breaks = seq(0, 120, by = 10), reducer = "mean"))
#' }
#' @export
ds.omop.dp.prepare <- function(x, out = NULL, symbol = "omop", conns = NULL) {
  if (inherits(x, "omop_recipe")) {
    plan <- recipe_to_plan(x)
    out <- .recipe_out_symbols(x, plan, out)
  } else if (inherits(x, "omop_plan")) {
    plan <- x
  } else {
    stop("x must be an omop_recipe or omop_plan.", call. = FALSE)
  }
  .plan_execute_impl(plan, out = out, symbol = symbol, conns = conns,
                     output_mode = "memory",
                     server_method = "omopDpPlanExecuteDS",
                     harmonize_factors = FALSE)
}
